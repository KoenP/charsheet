module Page.CardsPage exposing (..)
-- TODO should be Page.Cards

import Css exposing (Style, px, mm)
import Css.Media
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as E
import Http
import List
import Platform.Cmd as Cmd
import Set exposing (Set)
import String

import Decoder.CharacterSheet exposing (sheetDec)
import Element.Card exposing (defaultColorScheme, viewSpellCard, viewNotableTraitCard)
import Request exposing (characterRequestUrl)
import Types exposing (..)
import Util exposing (simple, applyIfPresent, guardListLazy)
import Util.Diff exposing (diffTraitCategories, diffSpellcastingSections)

load : CharId -> Cmd Msg
load charId =
  Http.get
    { url = characterRequestUrl charId ["sheet"] []
    , expect = Http.expectJson
               (mkHttpResponseMsg GotCards)
               sheetDec
    }

----------------------------------------------------------------------
-- UPDATE
----------------------------------------------------------------------
update : Msg -> Model -> CharacterSheet -> (Model, Cmd Msg)
update msg model sheet = (model, Cmd.none)

----------------------------------------------------------------------
-- VIEW
----------------------------------------------------------------------
view : CardConfig -> Maybe CharacterSheet -> CharacterSheet -> List (Html Msg)
view cardConfig maybeOldSheet sheet =
  let
    ( notableTraitCategories , spellcastingSections ) =
      case (maybeOldSheet , cardConfig.onlyShowChanges) of
          ( Just oldSheet , True ) ->
            ( diffTraitCategories oldSheet.notable_traits sheet.notable_traits
            , diffSpellcastingSections oldSheet.spellcasting_sections sheet.spellcasting_sections
            )
          _                        -> ( sheet.notable_traits , sheet.spellcasting_sections )

  in 
    button [ E.onClick (GotoCardSelectPage sheet) ] [text "select"]
    ::
    (List.map (div [ Attr.css cardsStyle ])
     <| Util.chunks 8 
     <| List.concatMap
          (viewSpellcastingSection cardConfig)
          (guardListLazy cardConfig.showSpells
             <| \() -> List.filter
                       (\{origin} -> not (Set.member origin cardConfig.excludedCategories))
                       spellcastingSections)
        ++ List.concatMap
           (viewNotableTraitCategory cardConfig defaultColorScheme) -- TODO
           (excludeTraits cardConfig <|
              guardListLazy cardConfig.showTraits <|
              \() -> List.filter
                     (\{category} -> not (Set.member category cardConfig.excludedCategories))
                     notableTraitCategories)
    )

excludeTraits : CardConfig -> List NotableTraitCategory -> List NotableTraitCategory
excludeTraits config categories =
  List.map (excludeTraitsFromCategory config) categories

excludeTraitsFromCategory : CardConfig -> NotableTraitCategory -> NotableTraitCategory
excludeTraitsFromCategory config { category, traits } =
  { category = category
  , traits   = List.filter (not << isTraitExcluded config category) traits
  }

isTraitExcluded : CardConfig -> Category -> Trait -> Bool
isTraitExcluded { explicitlyExcludedTraits } category { name } =
  Set.member (category, name) explicitlyExcludedTraits

viewNotableTraitCategory : CardConfig -> ColorScheme -> NotableTraitCategory -> List (Html Msg)
viewNotableTraitCategory config ambientColorScheme { category, traits } =
  let colorScheme = Dict.get category config.traitCategoryColorSchemes
        |> Maybe.withDefault ambientColorScheme 
  in traits
       |> List.filter (\trait -> trait.desc /= Nothing)
       |> List.map (viewNotableTraitCard config colorScheme category)

viewSpellcastingSection :  CardConfig -> SpellcastingSection -> List (Html Msg)
viewSpellcastingSection cardConfig section =
  let colorScheme = Dict.get section.origin cardConfig.spellcastingSectionColorSchemes
                    |> Maybe.withDefault defaultColorScheme
  in List.map (viewSpellCard cardConfig colorScheme section.origin)
       <| List.filter (shouldIncludeSpell cardConfig section.origin)
       <| section.spells

-- TODO I'm not sure CardsPageOptions is still relevant
shouldIncludeSpell : CardConfig -> Origin -> Spell -> Bool
shouldIncludeSpell { explicitlyExcludedSpells } origin { name, prepared } =
  not <| Set.member (origin, name) explicitlyExcludedSpells
  -- case showSpells of
  --   AllSpells -> True
  --   OnlyPreparedSpells -> prepared || Set.member name preparedSpells
  --   NoSpells -> False

----------------------------------------------------------------------
-- STYLE
----------------------------------------------------------------------

cardsStyle : List Style
cardsStyle =
  [ Css.displayFlex
  , Css.flexDirection Css.row
  , Css.flexWrap Css.wrap
  , Css.property "break-inside" "avoid"
  ]
  
