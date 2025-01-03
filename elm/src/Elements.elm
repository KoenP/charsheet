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

type TooltipDir = Bottom | Right | BottomLeft

tooltip : Float -> TooltipDir -> Html msg -> Html msg -> Html msg
tooltip width dir trigger content =
  let dirAttrs = case dir of
                     Bottom -> [ Css.top (Css.pct 105), Css.left Css.zero ]
                     Right  -> [ Css.top Css.zero, Css.left (Css.pct 105) ]
                     BottomLeft -> [ Css.top (Css.pct 105), Css.right (Css.pct 95) ]
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
              , Css.width <| Css.mm width
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

viewNavButtons : List (Html Msg) -> Html Msg
viewNavButtons buttons =
  div [ css navButtonsStyle ] buttons

viewNavButton : Msg -> String -> String -> Html Msg
viewNavButton msg symbol tooltipText =
  tooltip
    50
    BottomLeft
    (button
       [ css navButtonStyle
       , E.onClick msg
       ]
       [ img [ css [ Css.width (Css.px 16) ], src ("/static/icons/" ++ symbol) ] [] ])
    (text tooltipText)

viewEditCharacterButton : Html Msg
viewEditCharacterButton =
  viewNavButton GotoEditCharacter "edit.png" "Edit this character"

viewSelectCharacterButton : Html Msg
viewSelectCharacterButton =
  viewNavButton GotoSelectCharacterPage "close.png" "Return to character selection"

viewGotoSheetButton : Html Msg
viewGotoSheetButton =
  viewNavButton GotoSheet "sheet.png" "View/print character sheet"

viewGotoCardsButton : CharacterSheet -> Html Msg
viewGotoCardsButton sheet =
  viewNavButton (GotoCardsPage { showSpells = AllSpells }) "cards.png" "View/print cards"

viewGotoEquipmentButton : Html Msg
viewGotoEquipmentButton =
  viewNavButton GotoEquipmentPage "equipment.png" "Edit equipment"

-- "\u{270F}" "\u{00d7}"  "\u{1f4c4}" 


-- viewGotoSheetButton : Html Msg
-- viewGotoSheetButton 

navButtonsStyle : List Style
navButtonsStyle =
  [ Css.position Css.fixed
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
