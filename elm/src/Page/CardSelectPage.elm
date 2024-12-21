module Page.CardSelectPage exposing (..)

import Debug
import Css exposing (Style)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as E
import List
import Maybe
import Set exposing (Set)
import Tuple

import Types exposing (..)
import Util exposing (..)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetCardExclusionConfig newConfig ->
      ( { model | cardExclusionConfig = newConfig }
      , Cmd.none
      )
    _ ->
      let _ = Debug.log "" msg
      in errorPage model ("Page.CardSelectPage.update called with "
                           ++ Debug.toString msg)

----------------------------------------------------------------------
-- VIEW
----------------------------------------------------------------------
view : CardExclusionConfig -> CharacterSheet -> List (Html Msg)
view config sheet =
  viewGlobalConfig config
  ::
  List.concatMap
    (viewCategory config)
    (mergeTraitAndSpellCategories sheet.notable_traits sheet.spellcasting_sections)

viewGlobalConfig : CardExclusionConfig -> Html Msg
viewGlobalConfig config =
  div [] <|
    viewCheckbox "showTraitsCheckbox" config.showTraits 
      (SetCardExclusionConfig { config | showTraits = not config.showTraits })
      "Include non-spell features"
    ++
    viewCheckbox "showSpellsCheckbox" config.showSpells
      (SetCardExclusionConfig { config | showSpells = not config.showSpells })
      "Include spells"

    

mergeTraitAndSpellCategories :  List NotableTraitCategory -> List SpellcastingSection
                             -> List (Category, List Trait, List Spell)
mergeTraitAndSpellCategories traitCategories spellcastingSections =
  mergeCategories
    (List.map (\{category, traits} -> (category, traits)) traitCategories)
    (List.map (\{origin  , spells} -> (origin  , spells)) spellcastingSections)

mergeCategories :  List (comparable, List a) -> List (comparable, List b)
                -> List (comparable, List a, List b)
mergeCategories left right =
  let go l r =
        case (l, r) of
          ((xcat,xs) :: lrem , (ycat,ys) :: rrem) ->
            case compare xcat ycat of
              EQ -> ( xcat , xs , ys ) :: go lrem rrem
              LT -> ( xcat , xs , [] ) :: go lrem r
              GT -> ( ycat , [] , ys ) :: go l    rrem
          (_  , []) ->
            List.map (\(xcat , xs) -> (xcat , xs , [])) l
          ([] , _ ) ->
            List.map (\(ycat , ys) -> (ycat , [] , ys)) r
  in go (List.sortBy Tuple.first left) (List.sortBy Tuple.first right)

viewCategory : CardExclusionConfig -> (Category , List Trait , List Spell) -> List (Html Msg)
viewCategory config (category , traits , spells) =
  let
    categoryIncluded = not <| Set.member category config.excludedCategories
    toggleCategory = if categoryIncluded then Set.insert category else Set.remove category
    categoryHeader = h2 (guardList (not categoryIncluded) [ Attr.css omittedStyle ])
                     <| viewCheckbox
                        ("show_category_" ++ category)
                        categoryIncluded
                        (SetCardExclusionConfig
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
      
viewTrait : CardExclusionConfig -> Category -> Trait -> Html Msg
viewTrait config category { name } =
  let included = not (Set.member (category, name) config.explicitlyExcludedTraits)
      updateFn = if included then Set.insert (category, name) else Set.remove (category, name)
      newConfig = { config
                  | explicitlyExcludedTraits = updateFn config.explicitlyExcludedTraits
                  }
  in viewListItem newConfig included category name

-- TODO code duplication with viewTrait
viewSpell : CardExclusionConfig -> Category -> Spell -> Html Msg
viewSpell config category { name } =
  let included = not (Set.member (category, name) config.explicitlyExcludedSpells)
      updateFn = if included then Set.insert (category, name) else Set.remove (category, name)
      newConfig = { config
                  | explicitlyExcludedSpells = updateFn config.explicitlyExcludedSpells
                  }
  in viewListItem newConfig included category name

viewListItem : CardExclusionConfig -> Bool -> Category -> String -> Html Msg
viewListItem newConfig included category name =
  li (guardList (not included) [ Attr.css omittedStyle ]) <|
    viewCheckbox 
      (category ++ "_" ++ name ++ "_checkbox")
      included
      (SetCardExclusionConfig newConfig)
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
