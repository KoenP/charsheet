module Page.CardSelectPage exposing (..)

import Debug
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
    ModifyCardExclusionConfig f ->
      ( { model | cardExclusionConfig = f model.cardExclusionConfig }
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
  List.concatMap
    (viewCategory config)
    (mergeTraitAndSpellCategories sheet.notable_traits sheet.spellcasting_sections)

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
  onlyIfPopulated
    (simple h2 ("From " ++ category ++ ":"))
    (div [])
    <| onlyIfPopulated
         (simple h4 "Features")
         (ul [])
         (traits |> List.filter (\{desc} -> desc /= Nothing)
                 |> List.map (viewTrait config category))
       ++
       onlyIfPopulated
         (simple h4 "Spells")
         (ul [])
         (spells |> List.map (viewSpell config category))
         

viewTrait : CardExclusionConfig -> Category -> Trait -> Html Msg
viewTrait { explicitlyExcludedTraits } category { name } =
  let included = not (Set.member (category, name) explicitlyExcludedTraits)
      updateFn = if included then Set.insert (category, name) else Set.remove (category, name)
      updateCardExclusionConfig config =
        { config | explicitlyExcludedTraits =
                   Debug.log "updateCardExclusionConfig" (updateFn config.explicitlyExcludedTraits) }
  in li []
     [ input [ Attr.type_ "checkbox"
             , Attr.checked included
             , E.onClick (ModifyCardExclusionConfig updateCardExclusionConfig)
             ] []
     , text name
     ]

-- TODO code duplication with viewTrait
viewSpell : CardExclusionConfig -> Category -> Spell -> Html Msg
viewSpell { explicitlyExcludedSpells } category { name } =
  let included = not (Set.member (category, name) explicitlyExcludedSpells)
      updateFn = if included then Set.insert (category, name) else Set.remove (category, name)
      updateCardExclusionConfig config =
        { config | explicitlyExcludedSpells =
                   Debug.log "updateCardExclusionConfig" (updateFn config.explicitlyExcludedSpells) }
  in li []
     [ input [ Attr.type_ "checkbox"
             , Attr.checked included
             , E.onClick (ModifyCardExclusionConfig updateCardExclusionConfig)
             ] []
     , text name
     ]
  
onlyIfPopulated : Html Msg -> (List (Html Msg) -> Html Msg) -> List (Html Msg)
                -> List (Html Msg)
onlyIfPopulated header wrapElements elements =
  case elements of
    [] -> []
    _  -> [ header, wrapElements elements ] 
