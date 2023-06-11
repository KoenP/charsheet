module Page.CharacterSheet exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Maybe exposing (Maybe)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events as E
import Css
import Css.Global
import Css.Transitions
import Http
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Debug
import Platform.Cmd as Cmd

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
    |> D.andMap (D.field "languages" (D.list D.string))
    |> D.andMap (D.field "weapons" (D.list D.string))
    |> D.andMap (D.field "armor" (D.list D.string))
    |> D.andMap (D.field "tools" (D.list D.string))
    |> D.andMap (D.field "notable_traits" notableTraitsDec)
    |> D.andMap (D.field "attacks" (D.list attackDec))
    |> D.andMap (D.field "spellcasting_sections" (D.list spellcastingSectionDec))
    |> D.andMap (D.field "spell_slots" (D.list D.int))
  
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
  D.dict (D.succeed AbilityTableEntry
            |> D.andMap (D.field "score" D.int)
            |> D.andMap (D.field "mod" D.int)
            |> D.andMap (D.field "st" D.int))

notableTraitsDec : Decoder (List NotableTraitCategory)
notableTraitsDec =
  D.list notableTraitDec

notableTraitDec : Decoder NotableTraitCategory
notableTraitDec =
  D.succeed NotableTraitCategory
    |> D.andMap (D.field "category" D.string)
    |> D.andMap (D.field "traits" (D.list traitDec))

traitDec : Decoder Trait
traitDec =
  D.succeed Trait
    |> D.andMap (D.field "name" D.string)
    |> D.andMap (D.field "desc" (D.nullable D.string))

attackDec : Decoder Attack
attackDec =
  D.succeed Attack
    |> D.andMap (D.field "name" D.string)
    |> D.andMap (D.field "range" D.string)
    |> D.andMap (D.field "to_hit_or_dc" D.string)
    |> D.andMap (D.field "damage" D.string)
    |> D.andMap (D.field "notes" D.string)

spellcastingSectionDec : Decoder SpellcastingSection
spellcastingSectionDec =
  D.succeed SpellcastingSection
    |> D.andMap (D.field "max_prepared_spells" D.int)
    |> D.andMap (D.field "origin" D.string)
    |> D.andMap (D.field "spell_attack_mod" D.int)
    |> D.andMap (D.field "spell_save_dc" D.int)
    |> D.andMap (D.field "spellcasting_ability" D.string)
    |> D.andMap (D.field "spellcasting_ability_mod" D.int)
    |> D.andMap (D.field "spells" (D.list spellDec))

spellDec : Decoder Spell
spellDec =
  D.succeed Spell
    |> D.andMap (D.field "casting_time" D.string)
    |> D.andMap (D.field "components" (D.list componentDec))
    |> D.andMap (D.field "concentration" D.string)
    |> D.andMap (D.field "dc" (D.nullable D.int))
    |> D.andMap (D.field "dc_abi" (D.nullable D.string))
    |> D.andMap (D.field "description" (D.list D.string))
    |> D.andMap (D.field "duration" D.string)
    |> D.andMap (D.field "level" D.int)
    |> D.andMap (D.field "name" D.string)
    |> D.andMap (D.field "prepared" preparedDec)
    |> D.andMap (D.field "range" D.string)
    |> D.andMap (D.field "resources" (D.list D.string))
    |> D.andMap (D.field "ritual" D.string)
    |> D.andMap (D.field "summary" D.string)
    |> D.andMap (D.field "to_hit" (D.nullable D.int))

preparedDec : Decoder Bool
preparedDec =
  D.oneOf
    [ exactMatchDec D.string "always" |> D.map (\_ -> True)
    , exactMatchDec D.string "maybe" |> D.map (\_ -> False)
    ]

componentDec : Decoder Component
componentDec =
  D.succeed V

exactMatchDec : Decoder a -> a -> Decoder a
exactMatchDec dec val =
  dec |>
    D.andThen (\other ->
                 case val == other of
                     True  -> D.succeed val
                     False -> D.fail "mismatch in exactMatchDec")
  
----------------------------------------------------------------------
-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetSpellPreparedness origin spell prepared ->
      ( { model | preparedSpells
                  = setSpellPreparedness origin spell prepared model.preparedSpells
        }
      , Cmd.none
      )
    SetShowOnlyPreparedSpells new ->
      ( { model | showOnlyPreparedSpells = new }
      , Cmd.none
      )
    _ ->
      ( model, Cmd.none )

----------------------------------------------------------------------
-- VIEW
view : Dict Origin (Set SpellName) -> Bool -> CharacterSheet -> Html Msg
view currentlyPreparedSpells showOnlyPreparedSpells sheet =
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
      , h2 [] [ text "Proficiencies" ]
      , viewProficiencies sheet.languages sheet.weapons sheet.armor sheet.tools
      , h2 [] [ text "Notable traits" ]
      , viewNotableTraits sheet.notable_traits
      , viewAttacks sheet.attacks
      , viewSpellcastingSections
          currentlyPreparedSpells
          showOnlyPreparedSpells
          sheet.spellcasting_sections
          sheet.spell_slots
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
  

viewProficiencies : List String -> List String -> List String -> List String -> Html msg
viewProficiencies languages weapons armor tools =
  div []
    [ ul [] [ viewProficienciesListItem "Languages: " languages
            , viewProficienciesListItem "Weapons: " weapons
            , viewProficienciesListItem "Armor: " armor
            , viewProficienciesListItem "Tools: " tools
            ]
    ]

viewProficienciesListItem : String -> List String -> Html msg
viewProficienciesListItem hdr proficiencies =
  li [] [ b [] [ text hdr ]
        , text (String.join ", " proficiencies)
        ]

viewNotableTraits : List NotableTraitCategory -> Html msg
viewNotableTraits traits =
  div [] (List.concatMap viewNotableTraitCategory traits)

viewNotableTraitCategory : NotableTraitCategory -> List (Html msg)
viewNotableTraitCategory { category, traits } =
  [ h4 [] [ text ("From " ++ category ++ ":") ]
  , ul [] (List.map (li [] << List.singleton << viewTrait) traits)
  ]
    
viewTrait : Trait -> Html msg
viewTrait { name, desc } =
  case desc of
    Nothing ->
      text name
    Just actualDesc ->
      tooltip (text name) (text actualDesc)

viewAttacks : List Attack -> Html msg
viewAttacks attacks =
  captionedTable "Attacks" tableAttrs <|
    tr [] (List.map simpleTh ["", "Range", "To Hit", "Damage", "Notes"])
    ::
    List.map
      (\{name, range, to_hit_or_dc, damage, notes} ->
         simpleRow [name, range, to_hit_or_dc, damage, notes])
      attacks

viewSpellcastingSections :  Dict Origin (Set SpellName)
                         -> Bool
                         -> List SpellcastingSection
                         -> List Int
                         -> Html Msg
viewSpellcastingSections currentlyPreparedSpells showOnlyPreparedSpells sections spellSlots =
  case sections of
    [] ->
      div [] []
    _ :: _ ->
      div [] <|
        h2 [] [text "Spellcasting"]
        :: viewSpellSlotTable spellSlots
        :: viewPactSlotTable
        :: List.map
             (\section ->
                viewSpellcastingSection showOnlyPreparedSpells section
                <| Maybe.withDefault Set.empty
                <| Dict.get section.origin currentlyPreparedSpells)
           sections
        

viewSpellSlotTable : List Int -> Html msg
viewSpellSlotTable spellSlots =
  captionedTable "Spell slots" tableAttrs
    [ tr [] <|
        simpleTh "Level" :: List.map (simpleTh << String.fromInt) (List.range 1 (List.length spellSlots))

    , tr []
        <| List.map (\n -> td tdAttrs <| List.repeat n (input [type_ "checkbox"] []))
        <| (0 :: spellSlots)
    ]

viewPactSlotTable : Html msg
viewPactSlotTable =
  text "TODO: pact slot table"

viewSpellcastingSection : Bool -> SpellcastingSection -> Set SpellName -> Html Msg
viewSpellcastingSection showOnlyPreparedSpells section currentlyPreparedSpells =
  div []
    [ h3 [] [text <| section.origin ++ " spells"]
    , ul []
        [ simple li <| "Max prepared spells: " ++ String.fromInt section.max_prepared_spells
        , simple li <| "Spell attack mod: " ++ formatModifier section.spell_attack_mod
        , simple li <| "Spell save DC: " ++ formatModifier section.spell_save_dc
        , simple li <| "Spellcasting ability: " ++ section.spellcasting_ability
        , simple li <| "Spellcasting ability mod: "
                        ++ formatModifier section.spellcasting_ability_mod
        ]
    , input [ type_ "checkbox"
            , checked showOnlyPreparedSpells
            , id "showOnlyPreparedSpellsToggle"
            , E.onClick (SetShowOnlyPreparedSpells (not showOnlyPreparedSpells))] []
    , label [for "showOnlyPreparedSpellsToggle"] [text "Show only prepared spells"]
    , table tableAttrs <|
        tr []
          (List.filter (\_ -> not showOnlyPreparedSpells) [simpleTh "Prep'd"]
           ++
           List.map simpleTh
             ["Lvl", "Spell", "CT", "Rng", "Cpts", "Dur", "Conc"
             , "To Hit/DC", "Effect (summary)", "Res"
             ])
        ::
        (List.map
           (\spell -> viewSpellTableRow showOnlyPreparedSpells section.origin spell
                      <| Set.member spell.name currentlyPreparedSpells)
           <| List.filter (\spell -> not showOnlyPreparedSpells
                                  || Set.member spell.name currentlyPreparedSpells
                                  || spell.prepared)
           <| section.spells)
    ]

viewSpellTableRow : Bool -> Origin -> Spell -> Bool -> Html Msg
viewSpellTableRow showOnlyPreparedSpells origin spell currentlyPrepared =
  tr [] <|
    List.filter (\_ -> not showOnlyPreparedSpells)
      [ td tdAttrs [viewSpellPrepared spell.prepared origin spell.name currentlyPrepared] ]
    ++
    [ simpleTd <| String.fromInt spell.level
    , td tdAttrs <| List.singleton <|
        tooltip
          (text spell.name)
          (div [] <|
             List.map
               (\paragraph -> p [] [text paragraph])
               spell.description)
    , simpleTd spell.casting_time
    , simpleTd spell.range
    , simpleTd "TODO"
    , simpleTd spell.duration
    , simpleTd spell.concentration
    , simpleTd (Maybe.map String.fromInt spell.to_hit
               |> maybeSG (Maybe.map String.fromInt spell.dc)
               |> Maybe.withDefault "")
    , simpleTd spell.summary
    , simpleTd <| String.concat <| List.intersperse "; " <| spell.resources
    ]
      
viewSpellPrepared : AlwaysPrepared -> Origin -> SpellName -> Bool -> Html Msg
viewSpellPrepared alwaysPrepared origin spell nowPrepared =
  case alwaysPrepared of
    True ->
      text "✓"
    False ->
      input
        [ type_ "checkbox"
        , E.onClick (SetSpellPreparedness origin spell (not nowPrepared))
        , checked nowPrepared
        ] []

simpleTh : String -> Html msg
simpleTh str = th thAttrs [ text str ]

simpleTd : String -> Html msg
simpleTd str = td tdAttrs [ text str ]

simpleRow : List String -> Html msg
simpleRow strs = tr [] (List.map simpleTd strs)

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
  
formatModifier : Int -> String
formatModifier mod =
  case compare mod 0 of
    LT -> String.fromInt mod
    EQ -> " 0"
    GT -> "+" ++ String.fromInt mod

--------------------------------------------------------------------------------
-- ATTRIBUTES
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

tooltipAttrs : List (Attribute msg)
tooltipAttrs =
  [ style "position" "relative"
  , style "display" "inline-block"
  , style "border-bottom" "1px dotted black"
  ]

tooltipTextAttrs =
  [ style "font-size" "12px"
  , style "font-family" "Liberation, sans-serif"
  ] ++ tooltipTooltipTextAttrs

tooltipTooltipTextAttrs : List (Attribute msg)
tooltipTooltipTextAttrs =
  [ style "visibility" "hidden"
  , style "width" "560px"
  , style "background-color" "black"
  , style "color" "#fff"
  , style "text-align" "center"
  , style "padding" "5px 0"
  , style "border-radius" "6px"
  , style "position" "absolute"
  , style "z-index" "1"
  , style "top" "0%"
  , style "left" "100%"
  ]

tooltip : Html msg -> Html msg -> Html msg
tooltip trigger content =
  div
    [ class "tooltip"
    , css
        [ Css.display Css.inlineBlock
        , Css.position Css.relative
        , Css.position Css.relative
        , Css.hover
            [ Css.Global.descendants
                [ Css.Global.selector ".tooltiptext"
                    [ Css.visibility Css.visible ]
                ]
            ]
        , Css.borderBottom3 (Css.px 1) Css.dotted (Css.hex "000000")
        ]
    ]
  [ trigger
  , span
      [ class "tooltiptext"
      , css
          [ Css.visibility Css.hidden
          , Css.position Css.absolute
          , Css.top <| Css.pct 0
          , Css.left <| Css.pct 105
          , Css.backgroundColor <| Css.hex "000000"
          , Css.width <| Css.px 560
          , Css.color <| Css.hex "ffffff"
          , Css.fontFamilies [ "Liberation", .value Css.sansSerif ]
          , Css.zIndex (Css.int 1)
          , Css.textAlign Css.center
          , Css.padding2 (Css.px 5) (Css.px 0)
          , Css.borderRadius (Css.px 6)
          ]
      ]
      [ content ]
  ]

maybeSG : Maybe a -> Maybe a -> Maybe a
maybeSG mx my =
  case mx of
    Just x ->
      Just x
    Nothing ->
      case my of
        Just y ->
          Just y
        Nothing ->
          Nothing
