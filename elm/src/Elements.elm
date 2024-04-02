module Elements exposing (..)

--------------------------------------------------------------------------------

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events as E
import Css exposing (Style)
import Css.Global

import Types exposing (..)

--------------------------------------------------------------------------------
-- TOOLTIPS
--------------------------------------------------------------------------------

type TooltipDir = Bottom | Right

tooltip : TooltipDir -> Html msg -> Html msg -> Html msg
tooltip dir trigger content =
  let dirAttrs = case dir of
                     Bottom -> [ Css.top (Css.pct 105), Css.left Css.zero ]
                     Right  -> [ Css.top Css.zero, Css.left (Css.pct 105) ]
  in 
    div
      [ class "tooltip"
      , css
          [ Css.display Css.inlineBlock
          , Css.position Css.relative
          , Css.hover
              [ Css.Global.descendants
                  [ Css.Global.selector ".tooltiptext"
                      [ Css.visibility Css.visible ]
                  ]
              ]
          , Css.borderBottom3 (Css.px 1) Css.dotted (Css.hex "000000")
          ]
      ]
      [ trigger
      , span
          [ class "tooltiptext"
          , css <|
              [ Css.visibility Css.hidden
              , Css.position Css.absolute
              , Css.backgroundColor <| Css.hex "000000"
              , Css.width <| Css.px 560
              , Css.color <| Css.hex "ffffff"
              , Css.fontFamilies [ "Dosis", .value Css.sansSerif ]
              , Css.zIndex (Css.int 1)
              , Css.textAlign Css.center
              , Css.padding2 (Css.px 5) (Css.px 0)
              , Css.borderRadius (Css.px 6)
              ] ++ dirAttrs
          ]
          [ content ]
      ]

--------------------------------------------------------------------------------
-- NAV BUTTONS
--------------------------------------------------------------------------------

viewNavButtons : Html Msg
viewNavButtons =
  div
  [ css navButtonsStyle ]
  [ button
    [ css navButtonStyle
    , E.onClick EditCharacter
    ]
    [ text "\u{270F}" ]
  , button
    [ css navButtonStyle
    , E.onClick GotoSelectCharacterPage
    ]
    [ text "\u{00d7}" ]
  ]

navButtonsStyle : List Style
navButtonsStyle =
  [ Css.position Css.absolute
  , Css.top Css.zero
  , Css.right Css.zero
  , Css.zIndex (Css.int 3)
  , Css.marginTop (Css.mm 1.2)
  , Css.marginRight (Css.mm 1.2)
  ]

navButtonStyle : List Style
navButtonStyle =
  [ Css.border Css.zero
  , Css.textAlign Css.center
  , Css.fontSize (Css.pt 12)
  , Css.lineHeight (Css.pt 12)
  , Css.height (Css.mm 6)
  , Css.width (Css.mm 6)
  , Css.cursor (Css.pointer)
  ]
