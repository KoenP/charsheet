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
  , desc    : List String
  , enabled : Bool
  , msg     : Msg
  }

dropdown : Bool -> String -> Maybe String -> List DropdownOption -> Bool -> Html Msg
dropdown isDisabled id currentlySelected entries open =
  let
    filterEntries = case currentlySelected of
                      Nothing -> (\x -> x)
                      Just selected -> List.filter (\ddopt -> ddopt.entry /= selected)
  in 
    div
      [ css dropdownStyle
      , E.stopPropagationOn "click" (D.succeed (Null, True))
      ]
      [ button
          ( css (buttonStyle isDisabled (currentlySelected /= Nothing) open)
            :: if isDisabled then [] else [ E.onClick (ToggleDropdown id) ])
          [ text (Maybe.withDefault "..." currentlySelected)]
      , div
          [ css <| (if open then visibility visible else visibility hidden) :: contentStyle ]

          (List.map
             (\{ entry, desc, enabled, msg } -> button
                (css (hrefStyle enabled)
                 :: E.onMouseEnter (SetEditCharacterPageDesc (Just desc))
                 :: E.onMouseLeave (SetEditCharacterPageDesc Nothing)
                 :: if enabled then [ E.onClick msg ] else [])
                [ text entry ])
             (filterEntries entries))
      ]

-- buttonColor : Bool -> Bool -> 
buttonColor isDisabled isOptionSelected isOpen =
  case (isDisabled, isOptionSelected, isOpen ) of
    (True, _    , _    ) -> rgb 150 150 150
    (_   , True , False) -> rgb 0 180 0
    (_   , True , True ) -> rgb 0 150 0
    (_   , False, True ) -> hex "2989b9"
    (_   , False, False) -> hex "3498db"

-- Dropdown button.
buttonStyle : Bool -> Bool -> Bool -> List Style
buttonStyle isDisabled optionSelected open =
  [ backgroundColor <| buttonColor isDisabled optionSelected open
  , color (hex "ffffff")
  , padding4 (px 4) (px 8) (px 4) (px 8) -- top right bot left
  , fontSize (px 16)
  , border zero
  , cursor pointer
  , hover <| if isDisabled then [] else [ backgroundColor (buttonColor False optionSelected True) ]
  , borderRadius (px 10)
  , minWidth (px 120)
  , minHeight (px 32)
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
  [ display block
  , Css.position Css.absolute
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
  
