module Notula.Transform where

import Notula.Prelude

import Notula.Core (Expr (..), mkLets, NameMacroDef, CallMacroDef)
import Notula.Assoc (Assoc)
import Notula.Assoc as Assoc

import Control.Monad.State (State, evalState)
import Control.Monad.State as State
import Data.Array (zip, length)


-- | AST transformation monad
type Tfm a = State { symCnt :: Int } a

runTfm :: forall a. Tfm a -> a
runTfm tfm = evalState tfm { symCnt: 0 }

genSym :: String -> Tfm String
genSym _suggestedName = do
  state <- State.get
  let result = "_" <> show state.symCnt
  State.modify_ (\s -> s { symCnt = s.symCnt + 1 })
  pure result


transform :: forall f. Functor f => Foldable f => f NameMacroDef -> f CallMacroDef -> Expr -> Expr
transform nameMacroDefs callMacroDefs =
  (runTfm <<< _) $
    fixEndoM (applyCallMacros callMacroDefs)
    >=> (applyNameMacros nameMacroDefs >>> pure)
    >=> (cleanUpBindings >>> pure)


fixEndo :: forall a. Eq a => (a -> a) -> a -> a
fixEndo f a = runIdentity $ fixEndoM (Identity <<< f) a

fixEndoM :: forall a m. Monad m => Eq a => (a -> m a) -> a -> m a
fixEndoM f a = f a >>= \a' -> if a' == a then pure a' else fixEndoM f a'

-- | `x # pipeWith [a, b, c] f` gives `x # f a # f b # f c`
pipeWith :: forall a b f. Functor f => Foldable f => f b -> (b -> (a -> a)) -> a -> a
pipeWith bs toF a0 = runIdentity (pipeWithM bs (\b a -> Identity $ toF b a) a0)

-- | `x # pipeWithM [a, b, c] f` gives `x >>= f a >>= f b >>= f c`
pipeWithM :: forall a b f m. Functor f => Foldable f => Monad m => f b -> (b -> (a -> m a)) -> a -> m a
pipeWithM bs toF a0 = bs # map toF # foldl (>=>) pure # (_ $ a0)


cleanUpBindings :: Expr -> Expr
cleanUpBindings = case _ of
  ENumber n -> ENumber n
  EString s -> EString s
  EBool b -> EBool b
  EList exprs -> EList (cleanUpBindings <$> exprs)
  ERef name -> ERef name
  ECall name args -> ECall name (cleanUpBindings <$> args)

  -- Merge nested let-bindings
  ELets binds (ELets binds' body) ->
    ELets (binds <> binds') body # cleanUpBindings

  ELets bindings body ->

    -- If this is an empty lets(), remove it and go again
    if null bindings
    then body # cleanUpBindings  -- Go again

    -- Look for a binding tht is single-use, unused, or
    -- particularly simple.
    -- If found, inline/remove it.
    -- Then go again.
    --
    -- We do this one variable at a time. The reuslt algorithm is
    -- correct but slow. Inlining multiple variables is tricky
    -- because, eg, in "let(a, 1, b, a, b)" you have to inline
    -- a into b and b into the body *at the same time*.
    else
    bindings
    # (Assoc.findMapWithKey \var val ->
        let
          totalReferences =
            (bindings # map (countRefs var) # sum)
            + (countRefs var body)
          isSimple = case val of
              ENumber _ -> true
              EBool _ -> true
              ERef _ -> true
              _ -> false
        in if totalReferences == 0
           then Just $
             ELets
               (bindings # Assoc.remove var)
               body
           else if totalReferences == 1 || isSimple
           then Just $
                   ELets
                     (bindings
                       # Assoc.remove var
                       # map (replaceRef var val)
                     )
                     (body # replaceRef var val)
            else Nothing)
    # case _ of
        Just modified -> modified # cleanUpBindings  -- Go again
        Nothing -> (

    -- If no work was done, structurally recur
    ELets (cleanUpBindings <$> bindings) (cleanUpBindings body)

    )


-- Apply name macros to an expression
applyNameMacros :: forall f. Functor f => Foldable f => f NameMacroDef -> Expr -> Expr
applyNameMacros macros = fixEndo (replaceRefs nameMap)
  where
  nameMap :: Assoc String Expr
  nameMap = macros # foldMap \{ name, body } -> Assoc.singleton name body


-- Attempt to apply a bunch of call-macros to an expression, recursively
applyCallMacros :: forall f. Functor f => Foldable f => f CallMacroDef -> Expr -> Tfm Expr
applyCallMacros macros =
  applyManyShallow macros
  >=> case _ of
    ENumber n -> pure $ ENumber n
    EString s -> pure $ EString s
    EBool b -> pure $ EBool b
    EList exprs -> EList <$> for exprs (applyCallMacros macros)
    ERef name -> pure $ ERef name
    ELets vars body -> ELets <$> for vars (applyCallMacros macros) <*> applyCallMacros macros body
    ECall name args -> ECall name <$> for args (applyCallMacros macros)

  where

  -- Attempt to apply a bunch of macros to an expression, without recurring
  applyManyShallow :: f CallMacroDef -> Expr -> Tfm Expr
  applyManyShallow macros = pipeWithM macros applyShallow


  -- Attempt to apply a single macro to the top-level of an expression
  --
  -- When applying a macro, not all macro variables are inlined. Some are
  -- instead emitted in a let().
  --
  -- For instance, when applying "F(x, y) = G(x, y)" to the
  -- expression "F(x0, y0)", if x is inlined but not y, then the
  -- result will be something like "let(_var, y0, G(x, _var))".
  --
  -- The following are the important rules regarding what is inlined:
  --   * If a variable is used in the macro body as a binding
  --     name for let(), then it will be inlined
  --     Eg. in the macro "F(x) = let(x, 2)", x will always be inlined
  --   * Otherwise, if a variable is used more than once
  --     in a macro body, then it will not be inlined
  --     Eg in the macro "F(x) = f(x, x)", x will never be inlined
  applyShallow :: CallMacroDef -> Expr -> Tfm Expr
  applyShallow macro = case _ of
    expr@(ECall name args) ->
      if name == macro.name && length args == length macro.argNames  -- Check that the macro matches
      then do

        (bindingNamesToReplace :: Assoc String String)
          /\ (varsToInline :: Assoc String Expr)
          /\ (varsToLet :: Assoc String Expr) <-
          zip macro.argNames args # foldMapM \(name /\ val) ->

            -- Variable is used as a name in let(), so inline it as a binding name
            if usedAsBindingName name macro.body
            then do
              let newName = case val of
                    ERef name -> name
                    _ -> "<error>"  -- bad
              pure $ Assoc.singleton name newName /\ mempty /\ mempty

            -- Variable is unreferenced in macro body; don't emit
            else if countRefs name macro.body == 0
            then pure mempty

            -- Variable is referenced only once; inline it
            else if countRefs name macro.body == 1
            then pure $ mempty /\ Assoc.singleton name val /\ mempty

            -- Otherwise, emit as let()
            else do
              newName <- genSym name
              pure $ mempty /\ Assoc.singleton name (ERef newName) /\ Assoc.singleton newName val

        pure $ mkLets varsToLet (macro.body # replaceRefs (varsToInline <> varsToLet) # replaceBindingNames bindingNamesToReplace)


      else pure expr
    expr -> pure expr


replaceRef :: String -> Expr -> Expr -> Expr
replaceRef var val = replaceRefs (Assoc.singleton var val)

-- Replace references to a name with an expression
replaceRefs :: Assoc String Expr -> Expr -> Expr
replaceRefs repl = case _ of
    ENumber n -> ENumber n
    EString s -> EString s
    EBool b -> EBool b
    EList xs -> EList (replaceRefs repl <$> xs)
    ERef name -> Assoc.lookup name repl # fromMaybe (ERef name)
    ELets vars body -> ELets (replaceRefs repl <$> vars) (body # replaceRefs (repl `Assoc.minus` vars))
    ECall name args -> ECall name (replaceRefs repl <$> args)

-- Replace binding names with other binding names
-- A binding name is a name used in a let() introducing a new variable
-- Eg, in "let(x, 5, y, z)" x and y are binding names (but not z)
replaceBindingNames :: Assoc String String -> Expr -> Expr
replaceBindingNames repl = case _ of
    ENumber n -> ENumber n
    EString s -> EString s
    EBool b -> EBool b
    EList xs -> EList (replaceBindingNames repl <$> xs)
    ERef name -> ERef name
    ECall name args -> ECall name (replaceBindingNames repl <$> args)
    ELets bindings body ->
      ELets
        ( bindings
          # Assoc.mapKeys (\k -> Assoc.lookup k repl # fromMaybe k)  -- Replace binding names
          # map (replaceBindingNames repl)
        )
        (replaceBindingNames repl body)

countRefs :: String -> Expr -> Int
countRefs targ = case _ of
  ENumber _ -> 0
  EString _ -> 0
  EBool _ -> 0
  EList exprs -> sum (countRefs targ <$> exprs)
  ERef name -> if name == targ then 1 else 0
  ECall _name args -> sum (countRefs targ <$> args)
  ELets bindings body ->
    sum (countRefs targ <$> bindings)
    + (if Assoc.has targ bindings then 0 else countRefs targ body)

-- Is the given name used as a binding name in let()?
usedAsBindingName :: String -> Expr -> Boolean
usedAsBindingName targ = case _ of
  ENumber _ -> false
  EString _ -> false
  EBool _ -> false
  EList exprs -> exprs # any (usedAsBindingName targ)
  ERef _ -> false
  ECall _name args -> args # any (usedAsBindingName targ)
  ELets bindings body ->
    Assoc.has targ bindings
    || (bindings # any (usedAsBindingName targ))
    || usedAsBindingName targ body
