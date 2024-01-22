module Notula.Parse where

import Notula.Prelude

import Notula.Core (Expr (..), MacroDef, mkLets)
import Notula.Assoc (Assoc)
import Notula.Assoc as Assoc

import Control.Lazy (defer)
import Partial.Unsafe (unsafePartial)
import Partial (crashWith)
import Data.Newtype (ala)
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
import StringParser.Combinators (many, many1, tryAhead, lookAhead, withError, try, option, optional)
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
  (
    parseMacroDefStmt
    <|> parseNameDefStmt
    <|> (Stmt_Expr <$> parseExpr)
  ) <* (deadSpace <* optional (string ";"))

parseNameDefStmt :: Parser Stmt
parseNameDefStmt = do
  name /\ def <- parseDefOf parseName parseExpr
  pure $ Stmt_NameDef name def

parseMacroDefStmt :: Parser Stmt
parseMacroDefStmt = do
  { head, args } /\ body <-
    parseDefOf
      (parseDottedCallOf parseName parseName <|> parseUndottedCallOf parseName)
      parseExpr
  pure $ Stmt_MacroDef { name: head, argNames: args, body }

parseDefOf :: forall a b. Parser a -> Parser b -> Parser (a /\ b)
parseDefOf parseLhs parseRhs = do
  lhs <- try (string "def" *> deadSpace1 *> parseLhs)
  deadSpace
  _ <- string "="
  deadSpace
  rhs <- parseRhs
  pure (lhs /\ rhs)

parseExpr :: Parser Expr
parseExpr =
  (defer \_ -> parseOperatorExprOf parseOperandExpr)
  <|> (defer \_ -> parseOperandExpr)

-- Expression that can be an argument of an operand
parseOperandExpr :: Parser Expr
parseOperandExpr =
  (defer \_ -> parseNestedDottedCallExprOf parseSimpleExpr parseExpr)
  <|> (defer \_ -> parseSimpleExpr)

-- Expression that can be to the left of a dot
parseSimpleExpr :: Parser Expr
parseSimpleExpr =
  (defer \_ -> parseStringExpr)
  <|> (defer \_ -> parseNumberExpr)
  <|> (defer \_ -> parseBooleanExpr)
  <|> (defer \_ -> parseListExpr)
  <|> (defer \_ -> parseParenthesizedExprOf parseExpr)
  <|> (defer \_ -> parseUndottedCallExprOf parseExpr)
  <|> (defer \_ -> parseRefExpr)

  where

  parseParenthesizedExprOf p =
    string "(" *> deadSpace *> p <* deadSpace <* string ")"


-- Skip whitespace and comments
-- Inline comments are #[ like this ]#
-- Line comments are # like this
deadSpace :: Parser Unit
deadSpace = optional deadSpace1

deadSpace1 :: Parser Unit
deadSpace1 = pure unit <* regex "(\\s|#\\[.*\\]#|#.*(\\n|$))+"


charsToString :: forall f. Foldable f => f Char -> String
charsToString = fromCharArray <<< Array.fromFoldable

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
  pure (EString $ charsToString chars)

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

parseListExpr :: Parser Expr
parseListExpr = do
  vals <- delimitedSequenceOf "expression" "[" "]" "," (defer \_ -> parseExpr)
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

-- Parses eg "x.f()" and "x.f().g()"
-- Dotted calls need special attention because they cause the grammar to be left-recursive
parseNestedDottedCallExprOf :: Parser Expr -> Parser Expr -> Parser Expr
parseNestedDottedCallExprOf headParser restParser = do
  head <- try $ headParser <* deadSpace <* lookAhead (string ".")
  calls <- many1 do
    deadSpace
    _ <- string "."
    funcName <- parseName
    when (funcName == "let" || funcName == "lets") do fail "Cannot use let() or lets() with a dot"
    deadSpace
    args <- parseDotCall restParser
    deadSpace
    pure { funcName, args }
  let bigEndo = ala Endo foldMap $ reverse calls # map (\{ funcName, args } expr -> ECall funcName ([expr] <> args))
  pure $ bigEndo head

  where
  reverse = Array.fromFoldable >>> Array.reverse

parseUndottedCallOf :: forall a. Parser a -> Parser { head :: String, args :: Array a }
parseUndottedCallOf p = do
  funcName <- try $ parseName <* deadSpace <* lookAhead (string "(")
  args <- delimitedSequenceOf "argument" "(" ")" "," p
  pure { head: funcName, args: Array.fromFoldable args }


parseDottedCallOf :: forall a. Parser a -> Parser a -> Parser { head :: String, args :: Array a }
parseDottedCallOf firstParser restParser = do
  firstArg <- try $ firstParser <* deadSpace <* (string ".")
  deadSpace
  funcName <- parseName
  deadSpace
  restArgs <- parseDotCall restParser
  pure { head: funcName, args: [firstArg] <> Array.fromFoldable restArgs }

-- Parse the ".f(a, b, c)" part of a dotted call
parseDotCall :: forall a. Parser a -> Parser (Array a)
parseDotCall p =
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
parseOperatorExprOf :: Parser Expr -> Parser Expr
parseOperatorExprOf parseTerm = do
  firstTerm /\ firstOp <- try termAndOp
  deadSpace
  restTermsAndOpsExceptFinal <- many (try termAndOp <* deadSpace) # map Array.fromFoldable
  deadSpace
  finalTerm <- parseTerm
  liftEither $ buildOperatorExpr
    { seq: [Right firstTerm, Left firstOp]
           <> (restTermsAndOpsExceptFinal # foldMap \(term /\ op) -> [Right term, Left op])
           <> [Right finalTerm]
    , opInfo: getOpInfo
    , mkOpCall: \opName lhs rhs -> ECall opName [lhs, rhs]
    }

  where

  termAndOp :: Parser (Expr /\ String)
  termAndOp = do
    term <- parseTerm
    deadSpace
    op <- parseOperatorName
    pure (term /\ op)

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


