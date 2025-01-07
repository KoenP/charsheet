module Page.CardSelectPage exposing (..)

import Debug
import Css exposing (Style)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as E
import Http
import List
import Maybe
import Set exposing (Set)
import Tuple

import Decoder.CharacterSheet exposing (sheetDec)
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
      ( { model | cardConfig = newConfig }
      , Cmd.none
      )
    _ ->
      let _ = Debug.log "" msg
      in errorPage model ("Page.CardSelectPage.update called with "
                           ++ Debug.toString msg)

----------------------------------------------------------------------
-- VIEW
----------------------------------------------------------------------
view :  CardConfig
     -> { curSheet : CharacterSheet , prevSheet : CharacterSheet}
     -> List (Html Msg)
view config { curSheet , prevSheet } =
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
      (viewCategory config)
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

viewCategory : CardConfig -> (Category , List Trait , List Spell) -> List (Html Msg)
viewCategory config (category , traits , spells) =
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
                        ("From " ++ category ++ ":" )
  in 
    if categoryIncluded
    then
      onlyIfPopulated
        categoryHeader
        (div [])
        <| guardListLazy config.showTraits
             (\() -> onlyIfPopulated
                (simple h4 "Features")
                (ul [])
                (traits |> List.filter (\{desc} -> desc /= Nothing)
                        |> List.map (viewTrait config category)))
           ++
           guardListLazy config.showSpells
             (\() -> onlyIfPopulated
               (simple h4 "Spells")
               (ul [])
               (spells |> List.map (viewSpell config category)))
    else
      [ categoryHeader ]
      
viewTrait : CardConfig -> Category -> Trait -> Html Msg
viewTrait config category { name } =
  let included = not (Set.member (category, name) config.explicitlyExcludedTraits)
      updateFn = if included then Set.insert (category, name) else Set.remove (category, name)
      newConfig = { config
                  | explicitlyExcludedTraits = updateFn config.explicitlyExcludedTraits
                  }
  in viewListItem newConfig included category name

-- TODO code duplication with viewTrait
viewSpell : CardConfig -> Category -> Spell -> Html Msg
viewSpell config category { name } =
  let included = not (Set.member (category, name) config.explicitlyExcludedSpells)
      updateFn = if included then Set.insert (category, name) else Set.remove (category, name)
      newConfig = { config
                  | explicitlyExcludedSpells = updateFn config.explicitlyExcludedSpells
                  }
  in viewListItem newConfig included category name

viewListItem : CardConfig -> Bool -> Category -> String -> Html Msg
viewListItem newConfig included category name =
  li (guardList (not included) [ Attr.css omittedStyle ]) <|
    viewCheckbox 
      (category ++ "_" ++ name ++ "_checkbox")
      included
      (SetCardConfig newConfig)
      name

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
