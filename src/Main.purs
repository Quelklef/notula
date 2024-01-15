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


type Model =
  { notionFormula :: String
  , notionPrelude :: String
  }

initial :: Model
initial =
  { notionFormula: slice 1 (-1) """
prop("Author")
.mapNully("Author email: ".add(it.email()))
.orElse("🔴 No author")
"""
  , notionPrelude: Stdlib.stdlib
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
  [ E.div
    [ P.addStyles
      [ S.flex "2"
      , S.display "flex"
      , S.gap gap
      , S.flexDirection "column"
      ]
    ]
    [ E.textarea
      [ P.addStyles
        [ S.flex "1"
        , cellSty
        ]
      , P.onKeyup \evt ref -> do
            ref # Ref.modify (field @"notionFormula" .~ getTargetValue evt)
      ]
      [ E.text model.notionFormula
      ]
    , E.textarea
      [ P.addStyles
        [ S.flex "3"
        , cellSty
        ]
      , P.onKeyup \evt ref -> do
            ref # Ref.modify (field @"notionPrelude" .~ getTargetValue evt)
      ]
      [ E.text model.notionPrelude
      ]
    ]
  , E.div
    [ P.addStyles
      [ S.flex "1"
        , cellSty
      , S.whiteSpace "pre-wrap !important"
      ]
    ]
    [ case Parse.run Parse.parseExpr model.notionFormula of
        Left err ->
          E.div
          [ P.addStyles [ S.color "red" ] ]
          [ E.text $ err.error <> " (at char " <> show err.pos <> " in Notion expression)"
          ]
        Right expr ->
          case Parse.run Parse.parseMacroDefs model.notionPrelude of
            Left err ->
              E.div
              [ P.addStyles [ S.color "red" ] ]
              [ E.text $ err.error <> " (at char " <> show err.pos <> " in macros)"
              ]
            Right macros ->
              let transformed = Transform.transform macros expr
              in E.text (Format.format transformed)
    ]
  ]

  where

  cellSty = fold
    [ S.fontSize "14px"
    , S.fontFamily "monospace"
    , S.whiteSpace "pre"
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

