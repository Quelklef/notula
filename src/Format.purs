module Notula.Format where

import Notula.Prelude

import Notula.Core (Expr (..))
import Notula.Parse as Parse

import Data.Array as Array
import Data.Int as Int
import Data.Set as Set
import Data.Map as Map


formatClosed :: Expr -> String
formatClosed expr =
  if needsParens expr then "(" <> format expr <> ")" else format expr

needsParens :: Expr -> Boolean
needsParens = case _ of
  ECall name _ -> treatAsOperatorName name
  _ -> false

format :: Expr -> String
format = case _ of
  ENumber num ->
    if Int.toNumber (Int.round num) == num
    then show (Int.round num)
    else show num
  EString str -> show str
  EBool bool -> show bool
  EList exprs -> "[" <> (exprs # map format # intercalate ", ") <> "]"

  ERef name -> name
  ELets mapping body ->
    let mappingAsArgs = mapping # Map.toUnfoldable # as @(Array _) # foldMap \(varName /\ varDef) -> [ERef varName, varDef]
    in format (ECall "lets" (mappingAsArgs <> [body]))

  ECall funcName args ->

    -- Plain call
    if Set.member funcName forcePlainCallNotation
    then funcName <> "(" <> (args # map format # intercalate ", ") <> ")"

    -- Operator call
    else case treatAsOperatorName funcName, args of
      true, [lhs, rhs] -> formatClosed lhs <> " " <> funcName <> " " <> formatClosed rhs

      -- Dotted call
      _, _ -> case Array.uncons args of
        Nothing -> funcName <> "()"
        Just { head, tail } ->
          formatClosed head
          <> "." <> funcName
          <> "(" <> (tail # map format # intercalate ", ") <> ")"

  where

  forcePlainCallNotation = Set.fromFoldable
    [ "let", "lets", "if", "ifs", "prop" ]

treatAsOperatorName :: String -> Boolean
treatAsOperatorName name =
  Set.member name (Set.fromFoldable [ "and", "or" ])
  || (not <<< Parse.matches Parse.parseName) name


