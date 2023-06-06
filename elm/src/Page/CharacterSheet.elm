module Page.CharacterSheet exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Debug

import Request exposing (requestUrl)
import Types exposing (..)

----------------------------------------------------------------------
-- INITIALIZE
load : Cmd Msg
load =
  Http.get
    { url = requestUrl "sheet" []
    , expect = Http.expectJson
               (mkHttpResponseMsg GotCharacterSheet)
               sheetDec
    }

sheetDec : Decoder CharacterSheet
sheetDec =
  D.succeed CharacterSheet
    |> D.andMap (D.field "name" D.string)
    |> D.andMap (D.field "summary" summaryDec)
    |> D.andMap (D.field "ability_table" abilityTableDec)
    |> D.andMap (D.field "skill_table" (D.dict D.int))
  
summaryDec : Decoder CharacterSummary
summaryDec =
  D.succeed CharacterSummary
    |> D.andMap (D.field "ac" D.int)
    |> D.andMap (D.field "class" D.string)
    |> D.andMap (D.field "hd" D.string)
    |> D.andMap (D.field "initiative" D.int)
    |> D.andMap (D.field "level" D.int)
    |> D.andMap (D.field "maxhp" D.int)
    |> D.andMap (D.field "pp" D.int)
    |> D.andMap (D.field "prof_bon" D.int)
    |> D.andMap (D.field "race" D.string)
    |> D.andMap (D.field "speed" D.int)

abilityTableDec : Decoder AbilityTable
abilityTableDec =
  D.dict ( D.succeed AbilityTableEntry
             |> D.andMap (D.field "score" D.int)
             |> D.andMap (D.field "mod" D.int)
             |> D.andMap (D.field "st" D.int)
         )

----------------------------------------------------------------------
-- VIEW
view : CharacterSheet -> Html Msg
view sheet =
  div
  [ style "width" "100%"
  , style "font-family" "Liberation, sans-serif"
  ]
  [ header
      [ style "padding" "1em"
      , style "color" "black"
      , style "background-color" "lightgrey"
      , style "clear" "left"
      , style "text-align" "left"
      ]
      [ button [ style "float" "right" ] [ text "edit" ]
      , h1 [] [ text sheet.name ]
      ]
  , article [ style "padding" "1em" ]
      [ viewSummaryTable sheet.summary
      , viewAbilityTable sheet.ability_table
      , viewSkillTable sheet.skill_table
      ]
  ]

viewSummaryTable : CharacterSummary -> Html msg
viewSummaryTable sm =
  table
  (tableAttrs ++  [ style "padding" "4px", style "float" "right" ])
  [ viewSummaryTableRow "Race" sm.race
  , viewSummaryTableRow "Class" sm.class
  , viewSummaryTableRow "Level" (String.fromInt sm.level)
  , viewSummaryTableRow "Max HP" (String.fromInt sm.maxhp)
  , viewSummaryTableRow "AC" (String.fromInt sm.ac)
  , viewSummaryTableRow "Initiative" (String.fromInt sm.initiative)
  , viewSummaryTableRow "Speed" (String.fromInt sm.speed)
  , viewSummaryTableRow "HD" sm.hd
  , viewSummaryTableRow "PP" (String.fromInt sm.pp)
  , viewSummaryTableRow "Prof Bon" (String.fromInt sm.prof_bon)
  ]
  
viewSummaryTableRow : String -> String -> Html msg
viewSummaryTableRow header value =
  tr [] [ th thAttrs [ text header ]
        , td tdAttrs [ text value ]
        ]

viewAbilityTable : AbilityTable -> Html msg
viewAbilityTable abilityTable =
  captionedTable "abilities" tableAttrs <|
    tr [] [ simpleTh "", simpleTh "Score", simpleTh "Mod", simpleTh "ST" ]
      :: List.concat (List.map (viewAbilityTableRow abilityTable) abilities)

viewAbilityTableRow : AbilityTable -> Ability -> List (Html msg)
viewAbilityTableRow abilityTable abi =
  case Dict.get abi abilityTable of
    Nothing                 -> []
    Just { score, mod, st } ->
      [ tr
        []
        (simpleTh (String.toUpper abi)
           :: [ simpleTd (String.fromInt score)
              , simpleTd (formatModifier mod)
              , simpleTd (formatModifier st)
              ])
      ]

viewSkillTable : SkillTable -> Html msg
viewSkillTable skillTable =
  captionedTable "skills" tableAttrs <|
    tr [] [ simpleTh "", simpleTh "Skill", simpleTh "Score" ]
      :: List.concatMap (viewSkillTableSection skillTable) skillsPerAbility
  
viewSkillTableSection : SkillTable -> ( Ability, List Skill ) -> List (Html msg)
viewSkillTableSection skillTable ( ability, skills ) =
  case skills of
    [] ->
      [ text "viewSkillTableSection: empty list of skills" ]
    first :: rest ->
      tr []
        (td
           (rowspan (List.length skills) :: tdAttrs)
           [ b [] [text (String.toUpper ability)] ] :: viewSkillTableRowContent skillTable first)
        ::
        List.map (tr [] << viewSkillTableRowContent skillTable) rest

viewSkillTableRowContent : SkillTable -> Skill -> List (Html msg)
viewSkillTableRowContent skillTable skill =
  [ simpleTd skill
  , simpleTd (Maybe.map formatModifier (Dict.get skill skillTable)
             |> Maybe.withDefault "ERROR")
  ]
  

simpleTh : String -> Html msg
simpleTh str = th thAttrs [ text str ]

simpleTd : String -> Html msg
simpleTd str = td tdAttrs [ text str ]
  
simple :  (List (Attribute msg) -> List (Html msg) -> Html msg)
       -> String
       -> Html msg
simple f str = f [] [ text str ]

captionedTable : String -> List (Attribute msg) -> List (Html msg) -> Html msg
captionedTable captionText attrs tableRows =
  div []
    [ caption [] [ simple h4 captionText ]
    , table attrs tableRows
    ]
  
tableAttrs : List (Attribute msg)
tableAttrs =
  [ style "font-family" "Fira Code, sans-serif"
  , style "border-collapse" "collapse"
  , style "border" "1px solid lightgrey"
  ]

tdAttrs : List (Attribute msg)
tdAttrs =
  [ style "text-align" "right"
  , style "padding" "6px"
  , style "border" "1px solid lightgrey"
  ]

thAttrs : List (Attribute msg)
thAttrs =
  [ style "text-align" "right"
  , style "padding" "8px"
  , style "border" "1px solid lightgrey"
  , style "background-color" "lightgrey"
  ]

formatModifier : Int -> String
formatModifier mod =
  case compare mod 0 of
    LT -> String.fromInt mod
    EQ -> " 0"
    GT -> "+" ++ String.fromInt mod
