module Page.CardSelectPage exposing (..)

import Debug
import Css exposing (Style)
import Dict
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as E
import Http
import List
import Maybe
import Set exposing (Set)
import Tuple

import Decoder.CharacterSheet exposing (sheetDec)
import Element.Card exposing (colorSchemes, defaultColorScheme)
import Element.Dropdown exposing (customStyleDropdown, defaultDropdownStyle, DropdownOption, DropdownStyle)
import Request exposing (characterRequestUrl)
import Types exposing (..)
import Util exposing (..)
import Util.Diff exposing (diffTraitCategories, diffSpellcastingSections, mergeTraitAndSpellCategories)

----------------------------------------------------------------------
-- INIT
----------------------------------------------------------------------
load : CharId -> CharacterSheet -> Cmd Msg
load charId curSheet =
  Http.get
    { url = characterRequestUrl charId ["prev_level_sheet"] []
    , expect = Http.expectJson
               (mkHttpResponseMsg (GotPrevLevelSheet curSheet))
               sheetDec
    }

----------------------------------------------------------------------
-- UPDATE
----------------------------------------------------------------------
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetCardConfig newConfig ->
      ( { model | cardConfig = newConfig , focusedDropdownId = Nothing }
      , Cmd.none
      )
    SetEditCharacterPageDesc _ -> ( model , Cmd.none )
    _ ->
      let _ = Debug.log "" msg
      in errorPage model ("Page.CardSelectPage.update called with "
                           ++ Debug.toString msg)

----------------------------------------------------------------------
-- VIEW
----------------------------------------------------------------------
type Card = SpellCard Spell | TraitCard Trait
cardName : Card -> String
cardName card =
  case card of
      SpellCard spell -> spell.name
      TraitCard trait -> trait.name

cardId : Category -> Card -> String
cardId category card =
  case card of
      SpellCard spell -> category ++ "-spell-" ++ spell.name
      TraitCard trait -> category ++ "-trait-" ++ trait.name

view :  CardConfig
     -> Maybe String
     -> { curSheet : CharacterSheet , prevSheet : CharacterSheet}
     -> List (Html Msg)
view config focusedDropdownId { curSheet , prevSheet } =
  let ( notableTraits , spellcastingSections ) =
        if config.onlyShowChanges
        then ( diffTraitCategories prevSheet.notable_traits curSheet.notable_traits
             , diffSpellcastingSections prevSheet.spellcasting_sections curSheet.spellcasting_sections
             )
        else ( curSheet.notable_traits , curSheet.spellcasting_sections )
  in
    viewGlobalConfig config
    ::
    List.concatMap
      (viewCategory config focusedDropdownId)
      (mergeTraitAndSpellCategories notableTraits spellcastingSections)

viewGlobalConfig : CardConfig -> Html Msg
viewGlobalConfig config =
  div [] <|
    viewCheckbox "showTraitsCheckbox" config.showTraits 
      (SetCardConfig { config | showTraits = not config.showTraits })
      "Include features"
    ++
    viewCheckbox "showSpellsCheckbox" config.showSpells
      (SetCardConfig { config | showSpells = not config.showSpells })
      "Include spells"
    ++
    viewCheckbox "onlyShowChanges" config.onlyShowChanges
      (SetCardConfig { config | onlyShowChanges = not config.onlyShowChanges })
      "Only show changes w.r.t. previous level"

viewCategory : CardConfig -> Maybe String -> (Category , List Trait , List Spell) -> List (Html Msg)
viewCategory config focusedDropdownId (category , traits , spells) =
  let
    categoryIncluded = not <| Set.member category config.excludedCategories
    toggleCategory = if categoryIncluded then Set.insert category else Set.remove category
    categoryHeader = h2 (guardList (not categoryIncluded) [ Attr.css omittedStyle ])
                     <| viewCheckbox
                        ("show_category_" ++ category)
                        categoryIncluded
                        (SetCardConfig
                           { config
                           | excludedCategories = toggleCategory config.excludedCategories
                           })
                        ("From " ++ category ++ ":")
    mkTraitCategoryMsg mScheme =
      SetCardConfig
      { config
      | traitCategoryColorSchemes =
          case mScheme of
              Nothing     -> Dict.remove category config.traitCategoryColorSchemes
              Just scheme -> Dict.insert category scheme config.traitCategoryColorSchemes
      }

    mkSpellSectionMsg mScheme =
      SetCardConfig
      { config
      | spellcastingSectionColorSchemes =
          case mScheme of
              Nothing     -> Dict.remove category config.spellcastingSectionColorSchemes
              Just scheme -> Dict.insert category scheme config.spellcastingSectionColorSchemes
      }
  in
    if categoryIncluded
    then
      onlyIfPopulated
        categoryHeader
        (div [])
        <| guardListLazy config.showTraits
             (\() -> onlyIfPopulated
                (h4 [] (  text "Features"
                       :: viewColorSchemePicker
                          config
                          mkTraitCategoryMsg
                          (Dict.get category config.traitCategoryColorSchemes)
                          focusedDropdownId
                          (category ++ "-traits")))
                (ul [])
                (traits |> List.filter (\{desc} -> desc /= Nothing)
                        |> List.map (viewTrait config focusedDropdownId category)))
           ++
           guardListLazy config.showSpells
             (\() -> onlyIfPopulated
               (h4 [] (  text "Spells"
                      :: viewColorSchemePicker
                         config
                         mkSpellSectionMsg
                         (Dict.get category config.spellcastingSectionColorSchemes)
                         focusedDropdownId
                         (category ++ "-spells")
                      ))
               (ul [])
               (spells |> List.map (viewSpell config focusedDropdownId category)))
    else
      [ categoryHeader ]
      
viewTrait : CardConfig -> Maybe String -> Category -> Trait -> Html Msg
viewTrait config focusedDropdownId category trait =
  let included = not (Set.member (category, trait.name) config.explicitlyExcludedTraits)
      updateFn = if included then Set.insert (category, trait.name) else Set.remove (category, trait.name)
      msgs =
        { toggleCardExclusionMsg =
            SetCardConfig
            { config
            | explicitlyExcludedTraits = updateFn config.explicitlyExcludedTraits
            }
        , selectCardColor = \maybeColor ->
            SetCardConfig
            { config
            | traitColorSchemes =
                case maybeColor of
                    Nothing  -> Dict.remove (category, trait.name) config.traitColorSchemes
                    Just col -> Dict.insert (category, trait.name) col config.traitColorSchemes
            }
        }
      currentlySelectedScheme = Dict.get (category, trait.name) config.traitColorSchemes
  in viewListItem config msgs currentlySelectedScheme focusedDropdownId included category (TraitCard trait)

viewSpell : CardConfig -> Maybe String -> Category -> Spell -> Html Msg
viewSpell config focusedDropdownId category spell =
  let included = not (Set.member (category, spell.name) config.explicitlyExcludedSpells)
      updateFn = if included then Set.insert (category, spell.name) else Set.remove (category, spell.name)
      msgs = 
        { toggleCardExclusionMsg = 
            SetCardConfig { config | explicitlyExcludedSpells = updateFn config.explicitlyExcludedSpells }
        , selectCardColor = \maybeColor ->
            SetCardConfig
            { config
            | spellColorSchemes =
                case maybeColor of
                    Nothing  -> Dict.remove (category, spell.name) config.spellColorSchemes
                    Just col -> Dict.insert (category, spell.name) col config.spellColorSchemes
            }
        }
      currentlySelectedScheme = Dict.get (category, spell.name) config.spellColorSchemes
  in viewListItem config msgs currentlySelectedScheme focusedDropdownId included category (SpellCard spell)

type alias CardConfigMsgs =
  { toggleCardExclusionMsg : Msg
  , selectCardColor : Maybe ColorScheme -> Msg
  }

viewListItem :  CardConfig
             -> CardConfigMsgs
             -> Maybe ColorScheme
             -> Maybe String
             -> Bool
             -> Category
             -> Card
             -> Html Msg
viewListItem config msgs currentlySelectedScheme focusedDropdownId included category card =
  li (guardList (not included) [ Attr.css omittedStyle ]) <|
    viewCheckbox 
      (category ++ "_" ++ cardName card ++ "_checkbox")
      included
      msgs.toggleCardExclusionMsg
      (cardName card)
    ++
    viewColorSchemePicker config msgs.selectCardColor currentlySelectedScheme focusedDropdownId (cardId category card)

viewColorSchemePicker :  CardConfig
                      -> (Maybe ColorScheme -> Msg)
                      -> Maybe ColorScheme
                      -> Maybe String
                      -> String
                      -> List (Html Msg)
viewColorSchemePicker config mkSelectMsg currentlySelectedScheme focusedDropdownId dropdownId =
  let
    mkOption scheme =
      { entry = scheme.name
      , desc = []
      , enabled = True
      , msg = mkSelectMsg (Just scheme)
      , style = [Css.color scheme.fg]
      }
    deselect = { entry = "-"
               , desc = []
               , enabled = True
               , msg = mkSelectMsg Nothing
               , style = [Css.color defaultColorScheme.fg]
               }
    buttonScheme = Maybe.withDefault defaultColorScheme currentlySelectedScheme
  in [ customStyleDropdown
         { defaultDropdownStyle
         | buttonStyle = \isDisabled optionSelected isOpen ->
             [ Css.color buttonScheme.fg
             , Css.backgroundColor buttonScheme.bg
             ]
         }
         False
         dropdownId
         (Maybe.map .name currentlySelectedScheme)
         (deselect :: List.map mkOption colorSchemes)
         (Just dropdownId == focusedDropdownId)
     ]

viewColorSchemeOption : CardConfig -> Card -> ColorScheme -> Html Msg
viewColorSchemeOption config card scheme =
  option
    []
    [ div [ Attr.css [ Css.backgroundColor scheme.fg ] ] [ text <| "■" ++ scheme.name ] ]

viewCheckbox : String -> Bool -> Msg -> String -> List (Html Msg)
viewCheckbox identifier isChecked msg checkboxLabel =
  [ input [ Attr.type_ "checkbox"
          , Attr.checked isChecked
          , Attr.id identifier
          , E.onClick msg
          ] []
  , label [ Attr.for identifier ] [ text checkboxLabel ]
  ]
  
onlyIfPopulated : Html Msg -> (List (Html Msg) -> Html Msg) -> List (Html Msg)
                -> List (Html Msg)
onlyIfPopulated header wrapElements elements =
  case elements of
    [] -> []
    _  -> [ header, wrapElements elements ] 

omittedStyle : List Style
omittedStyle =
  [ Css.color (Css.hex "#aaaaaa")
  , Css.textDecoration Css.lineThrough
  ]
