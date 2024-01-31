module Page.PrintableCharSheet exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Maybe exposing (Maybe)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, class)
import Html.Styled.Events as E
import Css
import Css.Global
import Css.Transitions
import Http
import Debug
import Platform.Cmd as Cmd

import Elements exposing (..)
import Request exposing (requestUrl)
import Types exposing (..)
import Types.Ability exposing (..)
import Util exposing (simple, formatModifier)
import Decoder.CharacterSheet exposing (sheetDec)

load : Cmd Msg
load =
  Http.get
    { url = requestUrl "sheet" []
    , expect = Http.expectJson
               (mkHttpResponseMsg GotPrintableCharSheet)
               sheetDec
    }

--------------------------------------------------------------------------------

view : CharacterSheet -> List (Html Msg)
view sheet =
  [ div [ class "page" ]
      [ div [ class "abilities" ] (viewAbilities sheet.ability_table sheet.skill_table) ]
  ]

viewAbilities : AbilityTable -> SkillTable -> List (Html Msg)
viewAbilities abilityTable skillTable =
  [ table [] <|
      List.map
        ( tr [class "ability-row"]
            << viewAbilityRow abilityTable skillTable )
        skillsPerAbility
  ]

viewAbilityRow :  AbilityTable -> SkillTable -> ( Ability , List Skill )
               -> List (Html Msg)
viewAbilityRow abilityTable skillTable ( ability , skills ) =
  case Dict.get ability abilityTable of
      Nothing
        -> [ text "viewAbilityRow ERROR" ]
      Just { base, totalBonus, score, mod, st, stProf }
        -> [ td []
             [ div [class "ability"]
               [ div [class "ability-name"] [text ability]
               , div [class "ability-modifier"]
                 [ text (formatModifier mod)
                 , hr [css [Css.margin Css.zero]] []
                 , div [class "ability-score"] [text (String.fromInt score)]
                 ]
               ]
             ]
           , td [class "skill-td"]
             [ table [] <|
                 stRow "saving throw" (formatModifier st) stProf
                 ::
                 List.map (tr [] << List.singleton << viewSkillTableRow skillTable) skills
             ]
           ]

viewSkillTableRow : SkillTable -> Skill -> Html Msg
viewSkillTableRow table skill =
  case Dict.get skill table of
      Nothing 
        -> text "viewSkillTableRow ERROR"
      Just { score, proficient }
        -> stRow skill (formatModifier score) proficient
  

stRow : String -> String -> Bool -> Html Msg
stRow label modifier bold =
  if bold
  then tr [] [td [] [b [] [text modifier]], td [] [b [] [text label]]]
  else tr [] [td [] [text modifier], td [] [text label]]

--------------------------------------------------------------------------------

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)
