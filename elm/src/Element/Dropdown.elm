module Element.Dropdown exposing (..)

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
  , style   : List Style
  }

type alias Disabled = Bool
type alias OptionSelected = Bool
type alias Open = Bool
type alias DropdownStyle =
  { buttonStyle : Disabled -> OptionSelected -> Open -> List Style
  , dropdownStyle : List Style
  , contentStyle : List Style
  , hrefStyle : Bool -> List Style 
  }

defaultDropdownStyle : DropdownStyle
defaultDropdownStyle =
  { buttonStyle = buttonStyle
  , dropdownStyle = dropdownStyle
  , contentStyle = contentStyle
  , hrefStyle = hrefStyle
  }

dropdown : Bool -> String -> Maybe String -> List DropdownOption -> Bool -> Html Msg
dropdown = customStyleDropdown defaultDropdownStyle


customStyleDropdown : DropdownStyle -> Bool -> String -> Maybe String -> List DropdownOption -> Bool -> Html Msg
customStyleDropdown customStyle isDisabled id currentlySelected entries open =
  let
    filterEntries = case currentlySelected of
                      Nothing -> (\x -> x)
                      Just selected -> List.filter (\ddopt -> ddopt.entry /= selected)
  in 
    div
      [ css customStyle.dropdownStyle
      , E.stopPropagationOn "click" (D.succeed (Null, True))
      ]
      [ button
          ( css (customStyle.buttonStyle isDisabled (currentlySelected /= Nothing) open)
            :: if isDisabled then [] else [ E.onClick (ToggleDropdown id) ])
          [ text (Maybe.withDefault "..." currentlySelected)]
      , div
          [ css <| (if open then visibility visible else visibility hidden) :: customStyle.contentStyle ]

          (List.map
             (\{ entry, desc, enabled, msg, style } -> button
                (css (customStyle.hrefStyle enabled ++ style)
                 :: E.onMouseEnter (SetEditCharacterPageDesc (Just desc))
                 :: E.onMouseLeave (SetEditCharacterPageDesc Nothing)
                 :: if enabled then [ E.onClick msg ] else [])
                [ text entry ])
             (filterEntries entries))
      ]

-- buttonColor : Bool -> Bool -> String
buttonColor isDisabled isOptionSelected isOpen =
  case (isDisabled, isOptionSelected, isOpen ) of
    (True, _    , _    ) -> "var(--rp-muted)"
    (_   , True , False) -> "var(--rp-pine)"
    (_   , True , True ) -> "var(--rp-foam)"
    (_   , False, True ) -> "var(--rp-iris)"
    (_   , False, False) -> "var(--rp-rose)"

-- Dropdown button.
buttonStyle : Bool -> Bool -> Bool -> List Style
buttonStyle isDisabled optionSelected open =
  [ Css.property "background-color" (buttonColor isDisabled optionSelected open)
  , Css.property "color" "var(--rp-base)"
  , padding4 (px 0) (px 8) (px 0) (px 8) -- top right bot left
  , fontSize (px 16)
  , border zero
  , cursor pointer
  , hover <| if isDisabled then [] else [ Css.property "background-color" (buttonColor False optionSelected True) ]
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
  , Css.property "background-color" "var(--rp-surface)"
  , Css.property "color" "var(--rp-text)"
  , Css.minWidth (Css.px 160)
  , Css.property "box-shadow" "0 8px 8px 0 var(--rp-shadow)"
  , Css.zIndex (Css.int 1)
  ]

-- Links inside the dropdown
hrefStyle : Bool -> List Style
hrefStyle enabled =
  [ Css.property "color" (if enabled then "var(--rp-text)" else "var(--rp-muted)")
  --, padding2 (px 12) (px 16)
  , textDecoration none
  , display block
  , border zero
  , Css.property "background-color" "transparent"
  , minWidth (px 160)
  ]
  ++ if enabled
     then [ hover [ Css.property "background-color" "var(--rp-highlight-med)" ]
          , cursor pointer
          ]
     else []
  
