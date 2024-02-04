module Page.PrintableCharSheet exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Maybe exposing (Maybe)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, class, src)
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
      [ div [ class "abilities" ] (viewAbilities sheet.ability_table sheet.skill_table)
      , div [ class "main-body" ] (viewMainBody sheet)
      , div [ class "hit-dice-section" ] (viewHitDiceSection sheet.hit_dice)
      ]
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
                 List.map (viewSkillTableRow skillTable) skills
             ]
           ]

viewSkillTableRow : SkillTable -> Skill -> Html Msg
viewSkillTableRow table skill =
  case Dict.get skill table of
      Nothing 
        -> text "viewSkillTableRow ERROR"
      Just { score, proficient }
        -> stRow skill (formatModifier score) proficient
  

viewMainBody : CharacterSheet -> List (Html Msg)
viewMainBody sheet =
  [ div [ class "charname" ]
    [ h1 [] [ text sheet.name ]
    , div [ class "race-and-classes" ]
      [ text (raceAndClassesToString sheet.summary.class sheet.summary.race) ]
    , div [ class "charlevel" ] [ text (String.fromInt sheet.summary.level) ]
    ]
  
  , div [ class "badges" ]
    [ viewBadgeDiv "hit points" "hp" (viewHitpointsBadgeContent sheet.summary.maxhp)
    , viewBadgeDiv "armor class" "ac" (viewArmorClassContent sheet.ac_formulas)
    , viewBadgeDiv "stats" "stat-table" (viewStatTableContent sheet.summary)
    ]
  , div [ class "attacks" ]
    [ div [ class "badge-title" ] [ text "attacks" ]
    , table [] <|
        tr [] (List.map (simple th) ["Attack", "To Hit/DC", "Damage", "Range", "Notes"])
        ::
        List.map viewAttackTableRow sheet.attacks
    ]
  ]

viewAttackTableRow : Attack -> Html Msg 
viewAttackTableRow { name, range, to_hit_or_dc, damage, notes } =
  tr []
  [ td [ css [ Css.textTransform Css.capitalize ] ] [ text name ]
  , simple td range
  , simple td to_hit_or_dc
  , simple td damage
  , simple td notes
  ]

viewStatTableContent : CharacterSummary -> List (Html Msg)
viewStatTableContent { speed, initiative, prof_bon, pp } =
  [ table []
    [ tr [] [ simple th "speed" , simple td (String.fromInt speed ++ " ft") ]
    , tr [] [ simple th "initiative" , simple td (formatModifier initiative) ]
    , tr [] [ simple th "proficiency bonus" , simple td (formatModifier prof_bon) ]
    , tr [] [ simple th "passive perception" , simple td (String.fromInt pp) ]
    ]
   
  ]

viewArmorClassContent : List AcFormula -> List (Html Msg)
viewArmorClassContent acFormulas =
  [ div [ class "column" ] (List.map viewAcFormula acFormulas) ]

viewAcFormula : AcFormula -> Html Msg
viewAcFormula { name, ac, shield } =
  div [ class "ac-formula" ]
  [ div [ class "ac-formula-name" ] [ text <| "▢ " ++ name ]
  , div [ class "row" ]
    ( div [ class "labeled-flex" ]
      [ div [ class "filled-in" ] [ div [] [ text (String.fromInt ac) ] ]
      , div [] [ text "base" ]
      ]
      ::
      case shield of
        Nothing -> []
        Just shieldAc ->
          [ plus
          , div [ class "labeled-flex" ]
            [ div [ class "filled-in" ] [ text (String.fromInt shieldAc) ]
            , div [] [ text "shield" ]
            ]
          ]
    )
  ]

viewHitDiceSection : List HitDice -> List (Html Msg)
viewHitDiceSection hitDice =
  [ div [ class "badge-title" ] [ text "hit dice" ]
  , div [ class "hit-dice" ]
    <| List.concatMap viewHitDice
    <| List.sortBy .d
    <| hitDice
  ] 

viewHitDice : HitDice -> List (Html Msg) 
viewHitDice { n, d } =
  List.repeat n <|
    img [ src ("/icons/d" ++ String.fromInt d ++ ".svg") ] []

viewHitpointsBadgeContent : Int -> List (Html Msg)
viewHitpointsBadgeContent maxHp =
  [ div [ class "row" ]
    [ viewLabeledFlexTop "current" viewBlank , plus , viewLabeledFlexTop "temp" viewBlank ]
  , hr [ css [ Css.marginTop (Css.pt 2), Css.marginBottom (Css.pt 4) ] ] []
  , div [ class "row" ]
    [ viewLabeledFlexBot "max hp" (viewFilledIn maxHp)
    , plus
    , viewLabeledFlexBot "bonus max hp" viewBlank
    ]
  ]

viewBadgeDiv : String -> String -> List (Html Msg) -> Html Msg
viewBadgeDiv badgeTitle contentClass content =
  div [ class "badge" ]
    [ div [ class "badge-title" ] [ text badgeTitle ]
    , div [ class contentClass ] content
    ]

viewLabeledFlexTop : String -> Html Msg -> Html Msg    
viewLabeledFlexTop label blank = div [ class "labeled-flex"] [ div [] [ text label ] , blank ]

viewLabeledFlexBot : String -> Html Msg -> Html Msg    
viewLabeledFlexBot label blank = div [ class "labeled-flex"] [ blank , div [] [ text label ] ]

viewFilledIn : Int -> Html Msg
viewFilledIn value =
  div [ class "filled-in" ] [ text (String.fromInt value) ]

viewBlank : Html Msg
viewBlank = div [ class "blank" ] []

raceAndClassesToString : String -> String -> String
raceAndClassesToString class race = String.concat [ race, " — ", class ]

stRow : String -> String -> Bool -> Html Msg
stRow label modifier bold =
  let emphasis = if bold then [css [Css.fontWeight Css.bold]] else []
  in tr [] [td emphasis [text modifier], td emphasis [text label]]

plus : Html Msg
plus = div [] [ text <| nbsp ++ "+" ++ nbsp ]

nbsp : String
nbsp = String.fromChar (Char.fromCode 160)

simple : (List (Attribute Msg) -> List (Html Msg) -> Html Msg) -> String -> Html Msg           
simple f x = f [] [ text x ]

--------------------------------------------------------------------------------

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)
