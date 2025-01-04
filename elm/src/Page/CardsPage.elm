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
import Element.Card exposing (viewSpellCard, viewNotableTraitCard)
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
view : CardExclusionConfig -> Maybe CharacterSheet -> CharacterSheet -> List (Html Msg)
view exclusionConfig maybeOldSheet sheet =
  let
    ( notableTraitCategories , spellcastingSections ) =
      case (maybeOldSheet , exclusionConfig.onlyShowChanges) of
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
          (viewSpellcastingSection exclusionConfig)
          (guardListLazy exclusionConfig.showSpells
             <| \() -> List.filter
                       (\{origin} -> not (Set.member origin exclusionConfig.excludedCategories))
                       spellcastingSections)
        ++ List.concatMap
           viewNotableTraitCategory
           (excludeTraits exclusionConfig <|
              guardListLazy exclusionConfig.showTraits <|
              \() -> List.filter
                     (\{category} -> not (Set.member category exclusionConfig.excludedCategories))
                     notableTraitCategories)
    )

excludeTraits : CardExclusionConfig -> List NotableTraitCategory -> List NotableTraitCategory
excludeTraits config categories =
  List.map (excludeTraitsFromCategory config) categories

excludeTraitsFromCategory : CardExclusionConfig -> NotableTraitCategory -> NotableTraitCategory
excludeTraitsFromCategory config { category, traits } =
  { category = category
  , traits   = List.filter (not << isTraitExcluded config category) traits
  }

isTraitExcluded : CardExclusionConfig -> Category -> Trait -> Bool
isTraitExcluded { explicitlyExcludedTraits } category { name } =
  Set.member (category, name) explicitlyExcludedTraits


viewNotableTraitCategory : NotableTraitCategory -> List (Html Msg)
viewNotableTraitCategory { category, traits } =
  traits
    |> List.filter (\trait -> trait.desc /= Nothing)
    |> List.map (viewNotableTraitCard category)

viewSpellcastingSection :  CardExclusionConfig -> SpellcastingSection -> List (Html Msg)
viewSpellcastingSection exclusionConfig section =
  List.map (viewSpellCard section.origin)
    <| List.filter (shouldIncludeSpell exclusionConfig section.origin)
    <| section.spells

-- TODO I'm not sure CardsPageOptions is still relevant
shouldIncludeSpell : CardExclusionConfig -> Origin -> Spell -> Bool
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
  
