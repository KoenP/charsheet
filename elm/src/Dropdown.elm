module Dropdown exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as E
import List
import Maybe
import Debug

import Types exposing (..)

dropdown : String -> Maybe String -> List (String, String, Msg) -> Bool -> Html Msg
dropdown id currentlySelected entries open =
  div
    [ css dropdownStyle ]
    [ button
        [ css (buttonStyle open)
        , E.onClick (ToggleDropdown id)
        ]
        [ text (Maybe.withDefault "-- select an option --" currentlySelected)]
    , div
        [ css <| (if open then display block else display none) :: contentStyle ]
        (List.map
           (\(entry, desc, msg) -> a
              [ Attr.href "#"
              , css hrefStyle
              , E.onClick msg
              , E.onMouseEnter (Debug.log "onMouseEnter" <| SetEditCharacterPageDesc (Just desc))
              , E.onMouseLeave (Debug.log "onMouseLeave" <| SetEditCharacterPageDesc Nothing)
              ]
              [ text entry ])
           entries)
    ]

-- Dropdown button.
buttonStyle : Bool -> List Style
buttonStyle open =
  [ backgroundColor (if open then hex "2989b9" else hex "3498db")
  , color (hex "ffffff")
  , padding (px 16)
  , fontSize (px 16)
  , border zero
  , cursor pointer
  , hover [ backgroundColor (hex "2989b9") ]
  ]

-- The container <div>
dropdownStyle : List Style
dropdownStyle =
  [ Css.position Css.relative
  , Css.display Css.inlineBlock
  ]

-- Dropdown content
contentStyle : List Style
contentStyle =
  [ Css.position Css.absolute
  , Css.backgroundColor (Css.hex "f1f1f1")
  , Css.minWidth (Css.px 160)
  , Css.boxShadow5 zero (px 8) zero (px 0) (rgba 0 0 0 0.2)
  , Css.zIndex (Css.int 1)
  ]

-- Links inside the dropdown
hrefStyle : List Style
hrefStyle =
  [ color (rgb 0 0 0)
  , padding2 (px 12) (px 16)
  , textDecoration none
  , display block
  , hover [ backgroundColor (hex "dddddd") ]
  ]
