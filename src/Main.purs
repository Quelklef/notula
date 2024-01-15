module Main where

import Notula.Prelude

import Mation as M
import Mation (Html', ReadWrite, DomEvent)
import Mation.Core.Refs as Ref
import Mation.Lenses (field)
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S

import Notula.Parse as Parse
import Notula.Format as Format
import Notula.Transform as Transform
import Notula.Stdlib as Stdlib

import Data.String.CodeUnits (slice)


evaluateProgram :: String -> Either String (Array String)
evaluateProgram text = do
  { macros, exprs } <-
      Parse.run Parse.parseProgram text
      # lmap (\err -> err.error <> " (at char " <> show err.pos <> ")")
  let transformed = Transform.transform macros <$> exprs
  let formatted = Format.format <$> transformed
  pure formatted


type Model =
  { code :: String
  }

initial :: Model
initial =
  { code: slice 1 (-1) """
prop("Author")
.mapNully("Author email: ".add(it.email()))
.orElse("🔴 No author")
""" <> "\n\n\n\n" <> Stdlib.stdlib
  }

renderApp :: Model -> Html' (ReadWrite Model)
renderApp model =
  let
    gap = "1rem"
  in
  E.div
  [ P.addStyles
    [ S.display "flex"
    , S.flexWrap "nowrap"
    , S.height "100vh"
    , S.gap gap
    , S.padding gap
    ]
  ]
  [ E.textarea
    [ P.addStyles
      [ S.flex "2"
      , cellSty
      ]
    , P.onKeyup \evt ref -> do
          ref # Ref.modify (field @"code" .~ getTargetValue evt)
    ]
    [ E.text model.code
    ]
  , E.div
    [ P.addStyles
      [ S.flex "1"
      , cellSty
      ]
    ]
    [ case evaluateProgram model.code of
        Left err ->
          E.div
          [ P.addStyles [ S.color "red" ] ]
          [ E.text err ]
        Right results ->
          E.text (results # intercalate "\n\n")
    ]
  ]

  where

  cellSty = fold
    [ S.fontSize "14px"
    , S.fontFamily "monospace"
    , S.whiteSpace "pre-wrap"
    , S.border "1px solid grey"
    , S.padding ".5em"
    ]

foreign import getTargetValue :: DomEvent -> String

renderToplevel :: Model -> Html' (ReadWrite Model)
renderToplevel model =
  E.div
  []
  [ E.style []
    [ E.text $ intercalate "\n"
      [ "body { margin: 0; }"
      , "* { box-sizing: border-box; }"
      ]
    ]
  , renderApp model
  ]

main :: Effect Unit
main =
  M.runApp
    { daemon: \_ -> pure unit
    , initial
    , root: M.underBody
    , render: renderToplevel
    }

