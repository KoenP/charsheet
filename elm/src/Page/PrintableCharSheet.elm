module Page.PrintableCharSheet exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Maybe exposing (Maybe)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, class, src, type_)
import Html.Styled.Events as E
import Css
import Css.Global
import Css.Transitions
import Http
import Debug
import Platform.Cmd as Cmd

import Request exposing (characterRequestUrl)
import Types exposing (..)
import Types.Ability exposing (..)
import Util exposing (simple, formatModifier, applyIfPresent)
import Decoder.CharacterSheet exposing (sheetDec)

load : CharId -> Cmd Msg
load charId =
  Http.get
    { url = characterRequestUrl charId ["sheet"] []
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
  , div [ class "page-break" ] []
  , div [ class "page" ]
    <| div [ class "column" ]
         (viewNotableTraits sheet.notable_traits
          ++
          viewOtherProficiencies sheet.weapons sheet.armor sheet.languages sheet.tools sheet.resistances)
       ::
       div [ class "column" ]
         (  viewSpellcastingTable
              (Util.catMaybes
                 <| List.map (\section ->
                                Maybe.map (\x -> (section.origin_shorthand,x)) section.stats)
                 <| sheet.spellcasting_sections)

         ++ viewSpellSlots sheet.spell_slots
         ++ viewPactMagic sheet.pact_magic
         )
       ::
       div [ class "column" ] (viewResources sheet.resources)
       ::
       []
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
  , div [ class "attacks attacks-positioning" ]
    [ div [ class "badge-title" ] [ text "attacks" ]
    , table [] <|
        tr [] (List.map (simple th) ["Attack", "To Hit/DC", "Damage", "Range", "Notes"])
        ::
        List.map viewAttackTableRow (List.take 5 sheet.attacks)
    ]
  ]

viewAttackTableRow : Attack -> Html Msg 
viewAttackTableRow { name, range, to_hit_or_dc, damage, notes } =
  tr []
  [ td [ css [ Css.textTransform Css.capitalize ] ] [ text name ]
  , simple td to_hit_or_dc
  , simple td damage
  , simple td range
  , simple td notes
  ]

viewStatTableContent : CharacterSummary -> List (Html Msg)
viewStatTableContent { speed, initiative, prof_bon, pp } =
  [ table []
    [ tr [] [ simple th "speed" , td [] [showSpeeds speed] ]
    , tr [] [ simple th "initiative" , simple td (formatModifier initiative) ]
    , tr [] [ simple th "proficiency bonus" , simple td (formatModifier prof_bon) ]
    , tr [] [ simple th "passive perception" , simple td (String.fromInt pp) ]
    ]
   
  ]

showSpeeds : List Speed -> Html Msg
showSpeeds speeds =
  case speeds of
    [ { speed } ] -> text <| String.fromInt speed ++ " ft"
    _ -> ul [class "multiple-speeds-list"] <| List.map showSpeed speeds 

showSpeed : Speed -> Html Msg
showSpeed { mode, speed } =
  li [] <| List.singleton <| text <|
    mode ++ ": " ++ String.fromInt speed ++ " ft"

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
  [ div [ class "badge-title" ] [ text "hd" ]
  , div [ class "hit-dice" ]
    <| List.concatMap viewHitDice
    <| List.sortBy .d
    <| hitDice
  ] 

viewHitDice : HitDice -> List (Html Msg) 
viewHitDice { n, d } =
  List.repeat n <|
    img [ src ("/static/icons/d" ++ String.fromInt d ++ ".svg") ] []

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

-- TODO merge related categories like "dwarf" and "hill dwarf"
viewNotableTraits : List NotableTraitCategory -> List (Html Msg)
viewNotableTraits categories =
  [ viewBadgeDiv "notable traits" "notable-traits"
    <| List.map (div [] << viewNotableTraitCategory)
    <| List.sortBy (negate << List.length << .traits) categories
  ]

viewNotableTraitCategory : NotableTraitCategory -> List (Html Msg)
viewNotableTraitCategory { category, traits } =
  [ h3 [] [ text category ]
  , ul []
    <| List.map (li [] << viewTrait)
    <| List.filter (not << .seminotable)
    <| traits
  ]

viewTrait : Trait -> List (Html Msg)
viewTrait { name, ref } =
  applyIfPresent ref (\refVal html -> html
                        ++ [ sub
                             [css [Css.fontSize (Css.pt 5.5), Css.fontWeight Css.bold]]
                             [text (String.concat [" ", refVal])]
                           ]
                     )
    [text name]

viewOtherProficiencies :  List String -> List String -> List String -> List Tool -> List Resistance
                       -> List (Html Msg)
viewOtherProficiencies weapons armor languages tools resistances =
  [ viewBadgeDiv "other proficiencies" "other-proficiencies"
      <| List.concatMap
         (\(category, entries)
            -> [ h3 [] [ text category ]
               , div [ class "details" ]
                 [ text <| String.concat <| List.intersperse ", " entries ]
               ])
         [ ("weapons", defaultWhenEmpty "-" weapons)
         , ("armor", defaultWhenEmpty "-" armor)
         , ("languages", defaultWhenEmpty "-" languages)
         , ("tools", defaultWhenEmpty "-" (List.map showTool tools))
         , ("resistances", defaultWhenEmpty "-" (List.map showResistance resistances))
         ]
  ]

showTool : Tool -> String
showTool { tool, expertise } = tool ++ if expertise then " (expertise)" else ""

showResistance : Resistance -> String
showResistance { damage_type, resistance } =
  String.concat [damage_type, " (", resistance, ")"]

viewSpellcastingTable : List (String, SpellcastingSectionStats) -> List (Html Msg)
viewSpellcastingTable sections =
  case sections of
      []             -> []
      [ (_, stats) ] -> [ viewBadgeDiv "spellcasting" "spellcasting"
                          (viewSingleSectionSpellcastingTable stats) ]
      _              -> [ viewBadgeDiv "spellcasting" "spellcasting"
                          (viewMultiSectionSpellcastingTable sections) ]

viewSingleSectionSpellcastingTable : SpellcastingSectionStats -> List (Html Msg)
viewSingleSectionSpellcastingTable stats =
  [ table [ class "spellcasting-table" ]
    <| List.map
       (\(field, val) -> tr [] [ simple th field, simple td val ])
       [ ("save DC", String.fromInt stats.spell_save_dc)
       , ("attack mod", Util.formatModifier stats.spell_attack_mod)
       , ("prepared", Maybe.withDefault "-"
                      <| Maybe.map String.fromInt stats.max_prepared_spells)
       , ("ability", String.toUpper stats.spellcasting_ability)
       ]
  ]

viewMultiSectionSpellcastingTable : List (String, SpellcastingSectionStats) -> List (Html Msg)
viewMultiSectionSpellcastingTable sections =
  [ table [ class "spellcasting-table" ]
    <| tr [] (simple td "" :: List.map (simple th << Tuple.first) sections)
      :: List.map
      (\(field, fn) -> tr [] (simple th field :: List.map (simple td << fn << Tuple.second) sections))
      [ ("DC", String.fromInt << .spell_save_dc)
      , ("mod", Util.formatModifier << .spell_attack_mod)
      , ("prep", Maybe.withDefault "-" << Maybe.map String.fromInt << .max_prepared_spells)
      , ("abi", String.toUpper << .spellcasting_ability)
      ]
  ]

viewSpellSlots : List Int -> List (Html Msg)
viewSpellSlots slots =
  case slots of
    [] -> []
    _  -> 
      [ div [ class "badge" ]
        [ div [ class "badge-title" ] [ text "spell slots" ]
        , div [ class "badge-content spell-slots"]
          [ table [] <| List.map2 viewSpellSlotTableRow (List.range 1 9) slots ]
        ]
      ]

viewSpellSlotTableRow : Int -> Int -> Html Msg
viewSpellSlotTableRow slotLevel count =
  tr []
  [ simple th (String.fromInt slotLevel)
  , td [] <| List.repeat count viewSlot
  ]

viewPactMagic : Maybe PactMagic -> List (Html Msg)
viewPactMagic pactMagic =
  case pactMagic of
    Nothing -> []
    Just { slot_count, slot_level } ->
      [ viewBadgeDiv "pact magic" "badge-content spell-slots"
        [ table []
          [ tr []
            [ simple th (String.fromInt slot_level)
            , td [] <| List.repeat slot_count viewSlot
            ] 
          ]
        ]
      ]

viewResources : List Resource -> List (Html Msg)
viewResources resources =
  case resources of
    [] -> []
    _  -> [ viewBadgeDiv "resources" "resources spell-slots" (List.map viewResource resources) ]

viewResource : Resource -> Html Msg
viewResource { name , number , restore } =
  div []
  [ simple h3 name
  , div [ class "resource-details" ]
    (viewResourceSlots number ++ viewResourceRestoreInfo restore)
  ]

viewResourceSlots : Int -> List (Html Msg)
viewResourceSlots num =
  if num <= 8
  then List.repeat num viewSlot
  else [ div [ class "row" ]
         [ viewSmallBlank, text (nbsp ++ "/" ++ nbsp ++ String.fromInt num) ]
       ]

viewResourceRestoreInfo : Dict String String -> List (Html Msg)
viewResourceRestoreInfo restoreDict =
  [ table []
      <| List.map viewResourceRestoreInfoLine
      <| Dict.toList restoreDict
  ]

viewResourceRestoreInfoLine : (String, String) -> Html Msg
viewResourceRestoreInfoLine (restType, restoreInfo) =
  tr [] [ simple td (restType ++ ":"), simple td restoreInfo ]
  

defaultWhenEmpty : a -> List a -> List a
defaultWhenEmpty default l =
  case l of
      _::_ -> l 
      []   -> [ default ]

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

viewSlot : Html Msg
viewSlot = input [ type_ "checkbox", class "spell-slot" ] []

viewBlank : Html Msg
viewBlank = div [ class "blank" ] []

viewSmallBlank : Html Msg
viewSmallBlank = div [ class "small-blank" ] []

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
