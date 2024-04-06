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
import Debug
import Platform.Cmd as Cmd

import Elements exposing (..)
import Request exposing (requestUrl)
import Types exposing (..)
import Types.Ability exposing (..)
import Util exposing (simple, formatModifier)
import Decoder.CharacterSheet exposing (sheetDec)

----------------------------------------------------------------------
-- INITIALIZE
----------------------------------------------------------------------
load : Cmd Msg
load =
  Http.get
    { url = requestUrl "sheet" []
    , expect = Http.expectJson
               (mkHttpResponseMsg GotCharacterSheet)
               sheetDec
    }

----------------------------------------------------------------------
-- UPDATE
----------------------------------------------------------------------
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
----------------------------------------------------------------------
view : Dict Origin (Set SpellName) -> Bool -> CharacterSheet -> List (Html Msg)
view currentlyPreparedSpells showOnlyPreparedSpells sheet =
  [ div
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
          [ div
              [ css [ Css.float Css.right
                    , Css.displayFlex
                    , Css.flexDirection Css.column
                    ]
              ]
              [ button [ E.onClick EditCharacter ]
                  [ text "edit" ]
              , button [ E.onClick (GotoCardsPage { showSpells = AllSpells } sheet) ]
                  [ text "cards" ]
              , button [ E.onClick (GotoCardsPage { showSpells = OnlyPreparedSpells } sheet) ]
                  [ text "cards (only prepared spells)" ]
              ]
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
  ]

tooltipSize : Float
tooltipSize = 24

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
  , simpleTd (Dict.get skill skillTable
             |> Maybe.map (.score >> formatModifier) 
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
      tooltip tooltipSize Right (text name) (text actualDesc)

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
    , ul [] <|
        viewPreparedSpells section.max_prepared_spells currentlyPreparedSpells
        ++
        [ simple li <| "Spell attack mod: " ++ formatModifier section.spell_attack_mod
        , simple li <| "Spell save DC: " ++ String.fromInt section.spell_save_dc
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

viewPreparedSpells maxPreparedSpells currentlyPreparedSpells =
  case maxPreparedSpells of
    Nothing -> []
    Just maxPreparedSpells_ ->
      [ li [ css 
               [ Css.color (if Set.size currentlyPreparedSpells > maxPreparedSpells_
                            then Css.hex "ff0000"
                            else Css.hex "000000")] 
           ]
           [ text <|
               "Prepared spells: "
               ++ String.fromInt (Set.size currentlyPreparedSpells)
               ++ "/"
               ++ String.fromInt maxPreparedSpells_
           ]
      ]

viewSpellTableRow : Bool -> Origin -> Spell -> Bool -> Html Msg
viewSpellTableRow showOnlyPreparedSpells origin spell currentlyPrepared =
  tr [] <|
    List.filter (\_ -> not showOnlyPreparedSpells)
      [ td tdAttrs [viewSpellPrepared spell.prepared origin spell.name currentlyPrepared] ]
    ++
    [ simpleTd <| String.fromInt spell.level
    , td tdAttrs <| List.singleton <|
        tooltip tooltipSize Right
          (text spell.name)
          (div [] <|
             List.map
               (\paragraph -> p [] [text paragraph])
               spell.description)
    , simpleTd spell.casting_time
    , simpleTd spell.range
    , td tdAttrs <| List.map viewComponent spell.components 
    , simpleTd spell.duration
    , simpleTd (showBool spell.concentration)
    , simpleTd (Maybe.map String.fromInt spell.to_hit
               |> maybeSG (Maybe.map String.fromInt spell.dc)
               |> Maybe.withDefault "")
    , simpleTd spell.summary
    , simpleTd <| String.concat <| List.intersperse "; " <| spell.resources
    ]

showBool : Bool -> String
showBool bool =
  if bool then "True" else "False"
      
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

viewComponent : Component -> Html Msg
viewComponent component =
  case component of
    V -> text "v"
    S -> text "s"
    M desc -> tooltip tooltipSize Right (text "m") (text desc)

simpleTh : String -> Html msg
simpleTh str = th thAttrs [ text str ]

simpleTd : String -> Html msg
simpleTd str = td tdAttrs [ text str ]

simpleRow : List String -> Html msg
simpleRow strs = tr [] (List.map simpleTd strs)

captionedTable : String -> List (Attribute msg) -> List (Html msg) -> Html msg
captionedTable captionText attrs tableRows =
  div []
    [ caption [] [ simple h4 captionText ]
    , table attrs tableRows
    ]
  
--------------------------------------------------------------------------------
-- ATTRIBUTES
----------------------------------------------------------------------
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
