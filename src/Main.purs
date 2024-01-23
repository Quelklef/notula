module Main where

import Notula.Prelude

import Mation as M
import Mation (Html', ReadWrite, DomEvent, ReadWriteL)
import Mation.Core.Refs as Ref
import Mation.Core.Util.Assoc (Assoc)
import Mation.Core.Util.Assoc as Assoc
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S
import Mation.Selectors as Sel
import Mation.Selectors ((#<>))
import Mation.Lenses (field)

import Notula.Parse as Parse
import Notula.Format as Format
import Notula.Transform as Transform
import Notula.Stdlib as Stdlib

import Data.String.CodeUnits (slice)
import Data.Array as Array


evaluateProgram :: String -> Either String (Array { mName :: Maybe String, expr :: String })
evaluateProgram text = do
  { macros, formulas } <-
      Parse.run Parse.parseProgram text
      # lmap (\err -> err.error <> " (at char " <> show err.pos <> ")")
  let transformed = formulas # map <<< field @"expr" %~ Transform.transform macros
  let formatted = transformed # map <<< field @"expr" %~ Format.format
  pure formatted


type Model =
  { code :: String
  , results :: Either String (Array { mName :: Maybe String, expr :: String })
  , resultsUpToDate :: Boolean
  }

setCode :: String -> Model -> Model
setCode code model =
  if model.code == code
  then model
  else model { code = code, resultsUpToDate = false }

setCodeAndEvaluate :: String -> Model -> Model
setCodeAndEvaluate code model =
  model
    { code = code
    , results = evaluateProgram code
    , resultsUpToDate = true
    }

-- Updates model.results when model.code changes
reEvaluationDaemon :: ReadWriteL Model -> Effect Unit
reEvaluationDaemon ref = do
  reEvaluateDebounced <- debounce 150.0 reEvaluate
  ref # Ref.onChange \_ -> reEvaluateDebounced
  where
  reEvaluate = do
    model <- Ref.read ref
    when (not model.resultsUpToDate) do
      let results = evaluateProgram model.code
      ref # Ref.modify ((field @"results" .~ results) >>> (field @"resultsUpToDate" .~ true))


initial :: Model
initial =
  { code: ""
  , results: Right []
  , resultsUpToDate: true
  }
  # setCodeAndEvaluate (slice 1 (-1) """
prop("Author")
.mapNully("Author email: " + it.email)
.orElse("🔴 No author")
""" <> "\n\n\n\n" <> Stdlib.stdlib)

renderApp :: Model -> Html' (ReadWrite Model)
renderApp model =
  let appWidth = "1000px"
  in
  E.div
  [ P.addStyles
    [ S.display "flex"
    , S.flexDirection "column"
    , S.height "100vh"
    , S.gap gap
    , S.padding $ gap <> " calc(max(" <> gap <> ", (100vw - " <> appWidth <> ") / 2))"
    , S.backgroundColor "rgb(230, 230, 230)"
    ]
  ]
  [ titles
  , E.div
    [ P.addStyles
      [ S.display "flex"
      , S.flexWrap "nowrap"
      , S.height "100vh"
      , S.gap gap
      ]
    ]
    [ mkCell
        { title: E.text "Input formula"
        , outerProps:
          [ P.addStyles [ S.flex "3" ]
          ]
        , innerElem: E.textarea
        , innerProps:
          [ P.onKeyup \evt ref -> do
                ref # Ref.modify (setCode (getTargetValue evt))
          , P.addStyles
            [ S.padding "1em"
            ]
          ]
        , children:
          [ E.text model.code
          ]
        }

    , E.div
      [ P.addStyles
        [ S.flex "1"
        , S.display "flex"
        , S.flexDirection "column"
        ]
      ]
      [ case model.results of
          Left err ->
            mkCell
              { title: E.text "Error"
              , outerProps: []
              , innerElem: E.div
              , innerProps: []
              , children:
                [ E.div
                  [ P.addStyles
                    [ S.padding "1em"
                    , S.color "red"
                    ]
                  ]
                  [ E.text err ]
                ]
              }
          Right results ->
            E.div
            [ P.addStyles
              [ S.display "flex"
              , S.flexDirection "column"
              , S.gap gap
              , S.maxHeight "85vh"  -- hack
              , S.overflowY "auto"
              ]
            ]
            [ results # foldMapWithIndex \resultIdx result ->
              mkCell
              { title:
                  E.div
                  []
                  [ E.div
                    []
                    [ E.text $
                        case result.mName of
                          Just name -> name
                          Nothing ->
                            if Array.length results == 1
                            then "Output formula"
                            else "Formula #" <> show (resultIdx + 1)
                    ]
                  , E.div
                    [ P.addStyles
                      [ S.fontStyle "italic"
                      , S.fontSize "0.8em"
                      , S.opacity "0.8"
                      , S.marginTop "2px"
                      ]
                    ]
                    [ E.text "Click to copy!" ]
                  ]
              , outerProps: []
              , innerElem: E.div
              , innerProps:
                  [ P.addStyles
                    [ S.maxHeight "200px"
                    , S.overflowY "auto"
                    ]
                  ]
              , children:
                  [ E.div
                    [ P.addStyles
                      [ S.cursor "pointer"
                      , S.userSelect "none"
                      ]
                    , P.onClick \_ _ -> do
                        copyToClipboard result.expr
                    ]
                    [ E.div
                      [ P.addStyles [ S.padding "1em" ]
                      ]
                      [ E.text result.expr
                      ]
                    ]
                  ]
              }
            ]
      ]
    ]
  ]

  where

  gap = "1.5rem"

  titles =
    E.div
    [ P.addStyles
      [ S.display "flex"
      , S.gap "1rem"
      , S.alignItems "baseline"
      ]
    ]
    [ E.p
      [ P.addStyles
        [ S.fontSize "1.5rem"
        , S.fontWeight "bold"
        , S.fontFamily "sans-serif"
        , S.margin "0"
        ]
      ]
      [ E.text "🧛 Notula (v0)" ]
    , E.p
      [ P.addStyles
        [ S.fontFamily "sans-serif"
        , S.fontStyle "italic"
        , S.margin "0"
        ]
      ]
      [ E.text "A formula builder for "
      , E.a
        [ P.href "https://notion.so"
        , P.target "_blank"
        , P.addStyles
          [ S.color "black"
          ]
        ]
        [ E.text "Notion" ]
      ]
    ]

  mkCell { title, outerProps, innerElem, innerProps, children } =
    let borderRadius = "4px" in
    E.div
    [ fold outerProps
    , P.addStyles
      [ S.display "flex"
      , S.flexDirection "column"
      ]
    ]
    [ E.p
      [ P.addStyles
        [ S.backgroundColor "rgb(30, 30, 30)"
        , S.color "rgb(230, 230, 230)"
        , S.fontFamily "sans-serif"
        , S.padding ".5em 1em"
        , S.margin "0"
        , S.borderRadius $ intercalate " " [borderRadius, borderRadius, "0", "0"]
        ]
      ]
      [ title
      ]
    , innerElem
      [ fold innerProps
      , P.spellcheck "false"
      , P.addStyles
        [ S.flex "1"
        , S.fontSize "14px"
        , S.fontFamily "monospace"
        , S.whiteSpace "pre-wrap"
        , S.wordBreak "break-all"
        , S.background "rgb(252, 252, 252)"
        , S.border "1px solid rgb(200, 200, 200)"
        , S.borderTop "none"
        , S.boxShadow "0 0 6px -5px rgba(0, 0, 0, 0.5)"
        , S.on (Sel.hover #<> Sel.focus)
          [ S.outline "none"
          , S.borderColor "#c06"
          ]
        , S.borderRadius $ intercalate " " ["0", "0", borderRadius, borderRadius]
        , S.rawStyle "resize" "none"
        ]
      ]
      children
    ]

foreign import getTargetValue :: DomEvent -> String
foreign import copyToClipboard :: String -> Effect Unit

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
    { daemon
    , initial
    , root: M.underBody
    , render: renderToplevel
    }

  where

  daemon :: ReadWriteL Model -> Effect Unit
  daemon ref = do

    -- Sync state with url
    qp <- readQueryParams
    qp # Assoc.lookup "text" # foldMap \text ->
      when (text /= "") do
        ref # Ref.modify (setCodeAndEvaluate text)
    ref # Ref.onChange \{ code } ->
      writeQueryParams $ Assoc.fromFoldable [ "text" /\ code ]

    -- Embed other daemons
    reEvaluationDaemon ref


foreign import readQueryParams :: Effect (Assoc String String)
foreign import writeQueryParams :: Assoc String String -> Effect Unit
foreign import debounce :: Number -> Effect Unit -> Effect (Effect Unit)
