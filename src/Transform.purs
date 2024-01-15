module Notula.Transform where

import Notula.Prelude

import Notula.Core (Expr (..), MacroDef)
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


transform :: forall f. Functor f => Foldable f => f MacroDef -> Expr -> Expr
transform macros =
  (runTfm <<< _) $
    fixEndoM (applyMacros macros)
    >=> (fixEndo mergeLets >>> pure)
    >=> (fixEndo cleanupVars >>> pure)
    >=> (fixEndo removeRedundantLets >>> pure)


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


mergeLets :: Expr -> Expr
mergeLets = case _ of
  ENumber n -> ENumber n
  EString s -> EString s
  EBool b -> EBool b
  EList exprs -> EList (mergeLets <$> exprs)
  ERef name -> ERef name
  ECall name args -> ECall name (mergeLets <$> args)

  ELets vars (ELets vars' body) -> ELets (vars <> vars') body # mergeLets
  ELets vars body -> ELets (mergeLets <$> vars) (mergeLets body)


cleanupVars :: Expr -> Expr
cleanupVars = case _ of
  ENumber n -> ENumber n
  EString s -> EString s
  EBool b -> EBool b
  EList exprs -> EList (cleanupVars <$> exprs)
  ERef name -> ERef name
  ECall name args -> ECall name (cleanupVars <$> args)
  ELets vars body ->
    let
      unusedHere var = (vars # map (countUses var) # sum) == 0
      toInline = vars # Assoc.filterWithKey (\var val ->
          -- Single-use
          (countUses var body == 1 && unusedHere var)
          -- Value is simple
          || case val of ENumber _ -> true
                         EBool _ -> true
                         ERef _ -> true
                         _ -> false
        )
      toRemove = vars # Assoc.filterKeys (\var ->
          -- Unused
          countUses var body == 0 && unusedHere var
      )
    in
      ELets
        (map cleanupVars $ vars `Assoc.minus` toInline `Assoc.minus` toRemove)
        (body # replaceRefs toInline)

  where

  countUses :: String -> Expr -> Int
  countUses targ = case _ of
    ENumber _ -> 0
    EString _ -> 0
    EBool _ -> 0
    EList exprs -> sum (countUses targ <$> exprs)
    ERef name -> if name == targ then 1 else 0
    ECall _name args -> sum (countUses targ <$> args)
    ELets vars body ->
      sum (countUses targ <$> vars)
      + (if Assoc.has targ vars then 0 else countUses targ body)


removeRedundantLets :: Expr -> Expr
removeRedundantLets = case _ of
  ENumber n -> ENumber n
  EString s -> EString s
  EBool b -> EBool b
  EList exprs -> EList (removeRedundantLets <$> exprs)
  ERef name -> ERef name
  ECall name args -> ECall name (removeRedundantLets <$> args)
  ELets vars body ->
    if null vars
    then removeRedundantLets body
    else ELets (removeRedundantLets <$> vars) (removeRedundantLets body)


-- Attempt to apply a bunch of macros to an expression, recursively
applyMacros :: forall f. Functor f => Foldable f => f MacroDef -> Expr -> Tfm Expr
applyMacros macros =
  applyMacrosShallow macros
  >=> case _ of
    ENumber n -> pure $ ENumber n
    EString s -> pure $ EString s
    EBool b -> pure $ EBool b
    EList exprs -> EList <$> for exprs (applyMacros macros)
    ERef name -> pure $ ERef name
    ELets vars body -> ELets <$> for vars (applyMacros macros) <*> applyMacros macros body
    ECall name args -> ECall name <$> for args (applyMacros macros)

  where

  -- Attempt to apply a bunch of macros to an expression, without recurring
  applyMacrosShallow :: f MacroDef -> Expr -> Tfm Expr
  applyMacrosShallow macros = pipeWithM macros applyMacroShallow


  -- Attempt to apply a single macro to the top-level of an expression
  --
  -- The result of applying macro "lhs(x) = rhs(x)" to expression "lhs(x)" is not
  -- just "rhs(x)" but rather "lets(_fr, x, rhs(_fr))" where here "_fr" stands for
  -- some fresh variable.
  --
  -- The use of lets() instead of plain replacement is so that macros which use
  -- arguments more than once won't produce formulas that are overly-large or
  -- duplicate computation.
  --
  -- Elsewhere in the codebase we deal with inlining variables when appropriate.
  applyMacroShallow :: MacroDef -> Expr -> Tfm Expr
  applyMacroShallow macro = case _ of
    expr@(ECall name args) ->
      if name == macro.name && length args == length macro.argNames
      then do
        namesArr <-
          for (zip macro.argNames args) \(given /\ val) -> do
            fresh <- genSym given
            pure { given, fresh, val }
        pure $ ELets
                 (Assoc.fromFoldable $ namesArr # map \{ fresh, val } -> fresh /\ val)
                 (macro.body # replaceRefs (Assoc.fromFoldable $ namesArr # map \{ given, fresh } -> given /\ ERef fresh))
      else pure expr
    expr -> pure expr


replaceRefs :: Assoc String Expr -> Expr -> Expr
replaceRefs repl = case _ of
    ENumber n -> ENumber n
    EString s -> EString s
    EBool b -> EBool b
    EList xs -> EList (replaceRefs repl <$> xs)
    ERef name -> Assoc.lookup name repl # fromMaybe (ERef name)
    ELets vars body -> ELets (replaceRefs repl <$> vars) (body # replaceRefs (repl `Assoc.minus` vars))
    ECall name args -> ECall name (replaceRefs repl <$> args)
