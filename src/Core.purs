module Notula.Core where

import Notula.Prelude


-- | Association list
type Assoc k v = Array (k /\ v)


data Expr

  = ENumber Number
      -- ^ Numeric literal
  | EString String
      -- ^ String literal
  | EBool Boolean
      -- ^ Boolean literal
  | EList (Array Expr)
      -- ^ List literal

  | ERef String
      -- ^ Variable reference
      --   Includes keywords 'current' and 'index'
  | ELets (Map String Expr) Expr
      -- ^ lets()
      --   Gets its own AST node because that makes transformations easier
      --   to write
  | ECall String (Array Expr)
      -- ^ Function or operator call, or if() or ifs()

derive instance Generic Expr _
derive instance Eq Expr
instance Show Expr where show x = genericShow x


-- A macro definition
type MacroDef = { name :: String, argNames :: Array String, body :: Expr }
