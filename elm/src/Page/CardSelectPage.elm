module Page.CardSelectPage exposing (..)

import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as E
import Dict
import List
import Maybe
import Set exposing (Set)

import Types exposing (..)
import Util exposing (..)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)

----------------------------------------------------------------------
-- VIEW
----------------------------------------------------------------------
view : CardExclusionConfig -> CharacterSheet -> List (Html Msg)
view config sheet =
  List.map (viewNotableTraitCategory config) sheet.notable_traits

viewNotableTraitCategory : CardExclusionConfig -> NotableTraitCategory -> Html Msg
viewNotableTraitCategory { explicitlyExcludedTraits } { category , traits } =
  div []
    [ simple h2 category
    , ul [] <| let excluded =  Maybe.withDefault Set.empty
                            <| Dict.get category explicitlyExcludedTraits
               in traits
                 |> List.filter (\{desc} -> desc /= Nothing)
                 |> List.map (viewTrait category excluded)
    ]

viewTrait : Category -> Set String -> Trait -> Html Msg
viewTrait category excluded { name } =
  let included = not (Set.member name excluded)
  in li []
     [ input [ Attr.type_ "checkbox"
             , Attr.checked included
             , E.onClick () -- TODO: exclude trait card
             ] []
     , text name
     ]
  
