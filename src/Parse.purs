module Notula.Parse where

import Notula.Prelude

import Notula.Core (Expr (..), MacroDef)

import Control.Lazy (defer)
import Data.Newtype (ala)
import Data.String.CodeUnits (fromCharArray)
import Data.Array as Array
import Data.List as List
import Data.List (List (..))
import Data.Number as Number
import Data.Either (isRight)
import Data.Map as Map
import Data.Semigroup.Last (Last (..))
import Data.Newtype (un)

import StringParser.Parser (Parser, fail, ParseError, runParser)
import StringParser.Combinators (many, many1, tryAhead, lookAhead, withError, try, option, sepBy1, optional)
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
        Stmt_NameDef name def -> mempty /\ mempty /\ Map.singleton name (Last def)

  let exprs' = exprs # map \expr -> ELets (un Last <$> nameDefs) expr

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
  parseMacroDefStmt
  <|> parseNameDefStmt
  <|> (Stmt_Expr <$> parseExpr)

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
      [ERef name, value, body] -> pure (ELets (Map.singleton name value) body)
      _ -> fail "let() must be called like let(name, value, body)"

  specialCase_lets args =
    case getLetsArgs args of
      Left error -> fail error
      Right { mapping, body } -> pure (ELets mapping body)

  getLetsArgs :: forall f. Foldable f => f Expr -> Either String { mapping :: Map String Expr, body :: Expr }
  getLetsArgs args = case List.fromFoldable args of
    Nil -> Left "lets() cannot be called without arguments"
    Cons body Nil -> Right { mapping: Map.empty, body }
    Cons (ERef varName) (Cons varDef rest) ->
      getLetsArgs rest # map \{ mapping, body } -> { body, mapping: mapping # Map.insert varName varDef }
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

-- Parses an any-length chain of the same operator, eg "a + b + c"
parseOperatorExprOf :: Parser Expr -> Parser Expr
parseOperatorExprOf p = do
  first /\ opName <- try do
    lhs <- p
    deadSpace
    opName <- parseOperatorName
    pure (lhs /\ opName)
  deadSpace
  rest <- p `sepBy1` (try $ deadSpace *> string opName *> deadSpace)
  let bigEndo = ala Endo foldMap $ reverse rest # map (\val acc -> ECall opName [acc, val])
  pure $ bigEndo first

  where
  reverse = Array.fromFoldable >>> Array.reverse

parseOperatorName :: Parser String
parseOperatorName = do
  name <- regex "([a-zA-Z_][a-zA-Z_0-9]*)|([-\\+\\*/&^%\\$@!~=<>]+)"
  case name of
    "def" -> fail "The name 'def' is reserved"
      -- ^ This ensures that "expr def a = b" is parsed as two statements "(expr) (def a = b)"
      --   and not as two operators "((expr def a) = b)"
    _ -> pure name

