module Notula.Core where

import Notula.Prelude

import Notula.Assoc (Assoc)


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
  | ELets (Assoc String Expr) Expr
      -- ^ lets(). This gets its own AST node because it makes
      --   transformations easier to write. We use 'Assoc' instead of 'Map'
      --   because order matters in emitted lets()
  | ECall String (Array Expr)
      -- ^ Function or operator call, or if() or ifs()

derive instance Generic Expr _
derive instance Eq Expr
instance Show Expr where show x = genericShow x

mkLets :: Assoc String Expr -> Expr -> Expr
mkLets bindings =
  if null bindings then identity else ELets bindings


-- A macro definition like `f(x) = stuff`
type CallMacroDef = { name :: String, argNames :: Array String, body :: Expr }

-- A macro definition like `myThing = stuff`
type NameMacroDef = { name :: String, body :: Expr }

-- A formula definition
type FormulaDef = { mName :: Maybe String, expr :: Expr }

