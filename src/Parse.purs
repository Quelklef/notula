module Notula.Parse where

import Notula.Prelude

import Notula.Core (Expr (..), MacroDef, mkLets)
import Notula.Assoc (Assoc)
import Notula.Assoc as Assoc

import Control.Lazy (fix)
import Partial.Unsafe (unsafePartial)
import Partial (crashWith)
import Data.String.CodeUnits (fromCharArray)
import Data.Array as Array
import Data.List as List
import Data.List (List (..))
import Data.Number as Number
import Data.Either (isRight, either)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set as Set

import StringParser.Parser (Parser, fail, ParseError, runParser)
import StringParser.Combinators (many, tryAhead, lookAhead, withError, try, option, optional)
import StringParser.CodeUnits (string, noneOf, regex, eof)


run :: forall a. Parser a -> String -> Either ParseError a
run p = runParser (deadSpace *> p <* deadSpace <* eof)

matches :: forall a. Parser a -> String -> Boolean
matches p = runParser p >>> isRight


-- Parses a block of text as a sequence of statements, then
-- inlines toplevel name definitions (eg "def A = 100") as
-- a toplevel call to lets() in each expression
--
-- (Unused variables will later get pruned out)
parseProgram :: Parser { macros :: Array MacroDef, exprs :: Array Expr }
parseProgram = do

  stmts <- parseStmts

  let macros /\ exprs /\ nameDefs = stmts # foldMap case _ of
        Stmt_MacroDef macro -> [macro] /\ mempty /\ mempty
        Stmt_Expr expr -> mempty /\ [expr] /\ mempty
        Stmt_NameDef name def -> mempty /\ mempty /\ Assoc.singleton name def

  let exprs' = exprs # map \expr -> mkLets nameDefs expr

  pure { macros, exprs: exprs' }

data Stmt
  = Stmt_MacroDef MacroDef
      -- ^ eg "def myMax(x, y) = if(x > y, x, y)"
  | Stmt_NameDef String Expr
      -- ^ eg "def myConstant = 100"
  | Stmt_Expr Expr
      -- ^ eg "10 + f(x)"

parseStmts :: Parser (Array Stmt)
parseStmts =
  (deadSpace *> many (parseStmt <* deadSpace))
  # map Array.fromFoldable

parseStmt :: Parser Stmt
parseStmt =
  ( parseDef <|> (Stmt_Expr <$> parseExpr)
  ) <* (deadSpace <* optional (string ";"))

parseDef :: Parser Stmt
parseDef = do
  try (string "def" *> deadSpace1)
  lhs <-
    Left <$> (parseDottedCallOf parseName parseName <|> parseUndottedCallOf parseName)
    <|> Right <$> parseName
  deadSpace
  _ <- string "="
  deadSpace
  rhs <- parseExpr
  pure $ case lhs of
    Left { head, args } -> Stmt_MacroDef { name: head, argNames: args, body: rhs }
    Right name -> Stmt_NameDef name rhs

parseExpr :: Parser Expr
parseExpr = fix \parseExpr' ->
  parseOperatorExprOf
    { parseOperand:
        parseIteratedDottedCallExprOf
          { parseHead:
              parseStringExpr
              <|> parseNumberExpr
              <|> parseBooleanExpr
              <|> parseListExprOf parseExpr'
              <|> parseParenthesizedExprOf parseExpr'
              <|> parseUndottedCallExprOf parseExpr'
              <|> parseRefExpr
          , parseArg: parseExpr'
          }
    }
    

parseParenthesizedExprOf :: forall a. Parser a -> Parser a
parseParenthesizedExprOf p =
  string "(" *> deadSpace *> p <* deadSpace <* string ")"


-- Skip whitespace and comments
-- Inline comments are #[ like this ]#
-- Line comments are # like this
deadSpace :: Parser Unit
deadSpace = optional deadSpace1

deadSpace1 :: Parser Unit
deadSpace1 = pure unit <* regex "(\\s|#\\[.*\\]#|#.*(\\n|$))+"


fromChars :: forall f. Foldable f => f Char -> String
fromChars = fromCharArray <<< Array.fromFoldable

-- Notion treats backslashes literally except when
-- followed by one of: ", \, n, t
parseStringExpr :: Parser Expr
parseStringExpr = do
  _ <- string "\""
  chars <- many (
            noneOf ['\\', '"']
            <|> (string "\\\\" *> pure '\\')
            <|> (string "\\\"" *> pure '"')
            <|> (string "\\n" *> pure '\n')
            <|> (string "\\t" *> pure '\t')
            <|> (string "\\" *> pure '\\')
          )
  _ <- string "\""
  pure (EString $ fromChars chars)

parseNumberExpr :: Parser Expr
parseNumberExpr = do
  digits <- regex "[0-9]+(\\.[0-9]+)?"
  case Number.fromString digits of
    Just num -> pure (ENumber num)
    Nothing -> fail "Impossible! When parsing number"

parseBooleanExpr :: Parser Expr
parseBooleanExpr =
  (string "true" *> pure (EBool true))
  <|> (string "false" *> pure (EBool false))

parseListExprOf :: Parser Expr -> Parser Expr
parseListExprOf parseElem = do
  vals <- delimitedSequenceOf "expression" "[" "]" "," parseElem
  pure (EList (Array.fromFoldable vals))

parseName :: Parser String
parseName = do
  name <- regex "[a-zA-Z_][a-zA-Z_0-9]*"
  case name of
    "def" -> fail "The name 'def' is reserved"
      -- ^ This ensures that "a + def a = b" is parsed as two statements "(a +) (def a = b)"
      --   (with one statement invalid) rather than two operators "((a + def) = b)"
    _ -> pure name

parseRefExpr :: Parser Expr
parseRefExpr = ERef <$> parseName

-- Like parseUndottedCallOf, but specialized to Expr, and
-- with special rules for let() and lets()
parseUndottedCallExprOf :: Parser Expr -> Parser Expr
parseUndottedCallExprOf p = do
  { head, args } <- parseUndottedCallOf p
  case head of
    "let" -> specialCase_let args
    "lets" -> specialCase_lets args
    _ -> pure (ECall head args)

  where

  specialCase_let args =
    case args of
      [ERef name, value, body] -> pure (ELets (Assoc.singleton name value) body)
      _ -> fail "let() must be called like let(name, value, body)"

  specialCase_lets args =
    case getLetsArgs args of
      Left error -> fail error
      Right { mapping, body } -> pure (ELets mapping body)

  getLetsArgs :: forall f. Foldable f => f Expr -> Either String { mapping :: Assoc String Expr, body :: Expr }
  getLetsArgs args = case List.fromFoldable args of
    Nil -> Left "lets() cannot be called without arguments"
    Cons body Nil -> Right { mapping: Assoc.empty, body }
    Cons (ERef varName) (Cons varDef rest) ->
      getLetsArgs rest # map \{ mapping, body } -> { body, mapping: mapping # Assoc.prepend varName varDef }
    Cons _ _ -> Left "lets() must be called with 2n+1 arguments, where each 2k+0 arg is a variable name"


-- eg HEAD.funcName1.funcName2(ARG)
--
-- If no dot-calls are present, falls through to the HEAD parser.
--
-- Dotted calls need special attention because they cause the grammar to be left-recursive
parseIteratedDottedCallExprOf :: { parseHead :: Parser Expr, parseArg :: Parser Expr } -> Parser Expr
parseIteratedDottedCallExprOf { parseHead, parseArg } = do
  head <- parseHead
  calls <- many do
    _ <- try (deadSpace *> string ".")
    funcName <- parseName
    when (funcName == "let" || funcName == "lets") do fail "Cannot use let() or lets() with a dot"
    deadSpace
    args <- parseDotCallArglistOf parseArg
    deadSpace
    pure { funcName, args }
  let addCall { funcName, args } = \expr -> ECall funcName ([expr] <> args)
  pure $ rightwardPipe head (addCall <$> calls)

  where

  rightwardPipe :: forall f a. Foldable f => a -> f (a -> a) -> a
  rightwardPipe a0 fs = foldl (\a f -> f a) a0 fs


-- eg funcName(ARG, ARG, ARG)
parseUndottedCallOf :: forall a. Parser a -> Parser { head :: String, args :: Array a }
parseUndottedCallOf parseArg = do
  funcName <- try $ parseName <* deadSpace <* lookAhead (string "(")
  args <- delimitedSequenceOf "argument" "(" ")" "," parseArg
  pure { head: funcName, args: Array.fromFoldable args }

-- eg HEAD.funcName(ARG, ARG, ARG)
--
-- Does NOT fall through if no dot is present
parseDottedCallOf :: forall a. Parser a -> Parser a -> Parser { head :: String, args :: Array a }
parseDottedCallOf parseHead parseArg = do
  firstArg <- try $ parseHead <* deadSpace <* (string ".")
  deadSpace
  funcName <- parseName
  deadSpace
  restArgs <- parseDotCallArglistOf parseArg
  pure { head: funcName, args: [firstArg] <> Array.fromFoldable restArgs }

parseDotCallArglistOf :: forall a. Parser a -> Parser (Array a)
parseDotCallArglistOf p =
  delimitedSequenceOf "argument" "(" ")" "," p
  # option []  -- Allows for "x.f" to mean "x.f()"

-- An open delimiter followed by zero or more comma-seperated elements followed by a close delimiter
-- Elements may have trailing commas
delimitedSequenceOf :: forall a. String -> String -> String -> String -> Parser a -> Parser (Array a)
delimitedSequenceOf elementIsA openDelim closeDelim comma parseElement = do
  _ <- string openDelim
  deadSpace
  elems <- many do
    elem <- parseElement `withError` ("Expected " <> elementIsA)
    deadSpace
    _ <- tryAhead (string closeDelim) <|> (string comma `withError` ("Expected '" <> closeDelim <> "' or '" <> comma <> "'"))
    deadSpace
    pure elem
  deadSpace
  _ <- string closeDelim `withError` ("Expected " <> elementIsA <> " or '" <> closeDelim <> "'")
  pure (Array.fromFoldable elems)

parseOperatorName :: Parser OpName
parseOperatorName = do
  name <- regex $ "([a-zA-Z_][a-zA-Z_0-9]*)|([`~!@#$%^&*\\-=+{}\\\\|;:<>/?]+)"
  case name of
    "def" -> fail "The name 'def' is reserved"
      -- ^ This ensures that "expr def a = b" is parsed as two statements "(expr) (def a = b)"
      --   and not as two operators "((expr def a) = b)"
    _ -> pure name


-- | Operator associativity
data Associativity = LeftAssoc | RightAssoc

derive instance Generic Associativity _
derive instance Eq Associativity
instance Ord Associativity where compare x = genericCompare x

type OpName = String

type OpInfo = { associativity :: Associativity, precedence :: Int }

getOpInfo :: OpName -> OpInfo
getOpInfo =
  case _ of

    "^" -> mkInfo RightAssoc 6
    "%" -> mkInfo LeftAssoc 5
    "*" -> mkInfo LeftAssoc 4
    "/" -> mkInfo LeftAssoc 4
    "+" -> mkInfo LeftAssoc 3
    "-" -> mkInfo LeftAssoc 3

    "==" -> mkInfo LeftAssoc 2
    "!=" -> mkInfo LeftAssoc 2
    "<=" -> mkInfo LeftAssoc 2
    ">=" -> mkInfo LeftAssoc 2
    "<" -> mkInfo LeftAssoc 2
    ">" -> mkInfo LeftAssoc 2

    "and" -> mkInfo LeftAssoc 1
    "&&" -> mkInfo LeftAssoc 1
    "or" -> mkInfo LeftAssoc 1
    "||" -> mkInfo LeftAssoc 1

    _ -> mkInfo LeftAssoc 0

  where
  mkInfo a p = { associativity: a, precedence: p }

  {- Info derived from observations like the following:

      ^ before %     | 3 ^ 2 % 2 == 1
      % before *,/   | 11 % 2 * 3 == 3
      *,/ before +,- | 1 + 1 * 0 == 1

      and before or  | false and true or false == false

      ^ right-assoc  | 2 ^ 2 ^ 3 == 256
      % left-assoc   | 19 % 7 % 3 == 2
      == no assoc    | 0 == 0 == 0 fails to parse

  -}



-- Parses an any-length chain of the operators, eg "a + b * c"
--
-- Falls through if no operators are present
parseOperatorExprOf :: { parseOperand :: Parser Expr } -> Parser Expr
parseOperatorExprOf { parseOperand } = do
  firstOperand <- parseOperand
  opsAndOperands <- many do
    opName <- try (deadSpace *> parseOperatorName)
    deadSpace
    operand <- parseOperand
    pure (opName /\ operand)
  liftEither $ buildOperatorExpr
    { seq: [Right firstOperand] <> (opsAndOperands # foldMap \(op /\ operand) -> [Left op, Right operand])
    , opInfo: getOpInfo
    , mkOpCall: \opName lhs rhs -> ECall opName [lhs, rhs]
    }

  where

  -- Like Control.Monad.Error.Class.liftEither
  liftEither :: forall a. Either String a -> Parser a
  liftEither = case _ of
    Left e -> fail e
    Right v -> pure v


-- Accepts an alternating sequence of expressions and
-- operators (eg "a + b * c") and resolves operator
-- precedence and associativity to build an expression.
--
-- If the input sequence is not alternating, or does not
-- both start and end with an expression, then
-- this function may **diverge**
--
-- If two operators of equal precedence but differing
-- associativity are used in sequence, this function
-- will return Left
buildOperatorExpr :: forall expr.
  { seq :: Array (Either OpName expr)
  , opInfo :: OpName -> OpInfo
  , mkOpCall :: String -> expr -> expr -> expr
  }
  -> Either String expr
buildOperatorExpr { seq, opInfo, mkOpCall } =
    go seq

  where

  go :: Array (Either OpName expr) -> Either String expr
  go soFar = case soFar of
    [Right result] -> pure result
    _ -> do
      let (ops :: Set String) = soFar # foldMap (either Set.singleton (const mempty))
      let (maxPrec :: Int) = ops # Set.map (opInfo >>> _.precedence) # maximum # fromJust'
      let (maxPrecOps :: Set String) = ops # Set.filter (opInfo >>> _.precedence >>> (_ == maxPrec))
      (assoc :: Associativity) <- getUnanimousAssociativity ops
      let (idx :: Int) =
            let search = case assoc of
                  LeftAssoc -> Array.findIndex
                  RightAssoc -> Array.findLastIndex
            in soFar # search (either (_ `elem` maxPrecOps) (const false)) # fromJust'
      let (op :: OpName) = Array.index soFar idx # fromJust' # fromLeft
      let (lhs :: expr) = Array.index soFar (idx - 1) # fromJust' # fromRight'
      let (rhs :: expr) = Array.index soFar (idx + 1) # fromJust' # fromRight'
      let (expr :: expr) = mkOpCall op lhs rhs
      let (soFar' :: Array (Either OpName expr)) = soFar # replaceSection (idx - 1) (idx + 1) [Right expr]
      go soFar'

  getUnanimousAssociativity :: Set String -> Either String Associativity
  getUnanimousAssociativity ops =
    let pairs = ops # Set.map (\op -> op /\ (opInfo op).associativity)
        assocs = Set.map snd pairs
    in case unSingleton assocs of
         Just assoc -> Right assoc
         Nothing -> Left $ fold
           [ "Cannot use the following equal-precedence operators together because they differ in associativity. "
           , pairs
               # Set.map (\(op /\ assoc) -> fold
                   [ op
                   , "("
                   , case assoc of
                       LeftAssoc -> "left"
                       RightAssoc -> "right"
                   , "-associative)"
                   ])
               # intercalate ", "
           ]

  --

  snd (_ /\ b) = b

  fromJust' :: forall a. Maybe a -> a
  fromJust' j = unsafePartial (fromJust j)

  fromRight' :: forall a b. Either a b -> b
  fromRight' = case _ of
    Left _ -> unsafePartial $ crashWith "fromRight': Left"
    Right v -> v

  fromLeft :: forall a b. Either a b -> a
  fromLeft = case _ of
    Left v -> v
    Right _ -> unsafePartial $ crashWith "fromLeft: Right"

  unSingleton :: forall f a. Foldable f => f a -> Maybe a
  unSingleton = Array.fromFoldable >>> case _ of
    [a] -> Just a
    _ -> Nothing

  replaceSection :: forall a. Int -> Int -> Array a -> Array a -> Array a
  replaceSection fromIdx toIdx replaceWith arr =
    Array.slice 0 fromIdx arr <> replaceWith <> Array.slice (toIdx + 1) (Array.length arr) arr


