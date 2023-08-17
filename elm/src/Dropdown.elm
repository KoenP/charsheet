module Dropdown exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as E
import List
import Maybe
import Debug
import Json.Decode as D

import Types exposing (..)

type alias DropdownOption =
  { entry   : String
  , desc    : String
  , enabled : Bool
  , msg     : Msg
  }

dropdown : Bool -> String -> Maybe String -> List DropdownOption -> Bool -> Html Msg
dropdown isDisabled id currentlySelected entries open =
  div
    [ css dropdownStyle
    , E.stopPropagationOn "click" (D.succeed (Null, True))
    ]
    [ button
        ( css (buttonStyle isDisabled open)
          :: if isDisabled then [] else [ E.onClick (ToggleDropdown id) ])
        [ text (Maybe.withDefault "-- select an option --" currentlySelected)]
    , div
        [ css <| (if open then display block else display none) :: contentStyle ]
        (List.map
           (\{ entry, desc, enabled, msg } -> button
              (css (hrefStyle enabled)
               :: E.onMouseEnter (SetEditCharacterPageDesc (Just desc))
               :: E.onMouseLeave (SetEditCharacterPageDesc Nothing)
               :: if enabled then [ E.onClick msg ] else [])
              -- , 
              -- ,
              [ text entry ])
           entries)
    ]

-- Dropdown button.
buttonStyle : Bool -> Bool -> List Style
buttonStyle isDisabled open =
  [ backgroundColor <| case (isDisabled, open) of
                         (True , _    ) -> rgb 150 150 150
                         (False, True ) -> hex "2989b9"
                         (False, False) -> hex "3498db"
  , color (hex "ffffff")
  , padding (px 16)
  , fontSize (px 16)
  , border zero
  , cursor pointer
  , hover <| if isDisabled then [] else [ backgroundColor (hex "2989b9") ]
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
  , Css.boxShadow5 zero (px 8) (px 8) (px 0) (rgba 0 0 0 0.2)
  , Css.zIndex (Css.int 1)
  ]

-- Links inside the dropdown
hrefStyle : Bool -> List Style
hrefStyle enabled =
  [ color (if enabled then rgb 0 0 0 else rgb 150 150 150)
  , padding2 (px 12) (px 16)
  , textDecoration none
  , display block
  , border zero
  , backgroundColor transparent
  , minWidth (px 160)
  ]
  ++ if enabled
     then [ hover [ backgroundColor (hex "dddddd") ]
          , cursor pointer
          ]
     else []
  
