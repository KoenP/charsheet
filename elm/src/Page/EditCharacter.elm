module Page.EditCharacter exposing (..)

import Browser.Navigation as Nav
import Css exposing (Style)
import Debug
import Dict exposing (Dict)
import Set exposing (Set)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as E
import Http
import Maybe
import Platform.Cmd as Cmd
import Tuple
import Zipper exposing (Zipper(..))

import Elements exposing (..)
import Request exposing (requestUrl)
import Types exposing (..)
import Types.Ability exposing (..)
import Decoder.AbilityTable exposing (abilityTableDec)
import Util exposing (simple)
import Dropdown exposing (dropdown)
import Decoder.CharacterOptions exposing (gotCharacterOptionsDec)

--------------------------------------------------------------------------------
-- LOAD
--------------------------------------------------------------------------------
load : Cmd Msg
load =
  Http.get
    { url = requestUrl "edit_character_page" []
    , expect = Http.expectJson
               (mkHttpResponseMsg (\(x,y,z) -> GotCharacterOptions x y z))
               gotCharacterOptionsDec
    }

--------------------------------------------------------------------------------
-- UPDATE
--------------------------------------------------------------------------------
update : Msg -> Model -> EditCharacterPageData -> (Model, Cmd Msg)
update msg model oldData =
  case msg of
    HttpResponse (Ok (GotCharacterOptions newAbilityTable newOptions newTraitsAndBonuses)) ->
      let
        charLevel = newOptions |> Dict.keys |> List.maximum |> Maybe.withDefault 1
      in
        applyPage
          model
          ( EditCharacterPage
              { abilityTable             = newAbilityTable
              , optionsPerLevel          = newOptions
              , traitsAndBonusesPerLevel = newTraitsAndBonuses
              , charLevel                = charLevel
              , selectedLevel            = Just <| Maybe.withDefault charLevel oldData.selectedLevel
              , desc                     = Nothing
              , setAbilitiesOnNextTick   =
                  let newBaseAbilities = Dict.map (\_ v -> v.base) newAbilityTable 
                      changedAbilities = Dict.intersect newBaseAbilities oldData.setAbilitiesOnNextTick
                  in if changedAbilities == oldData.setAbilitiesOnNextTick
                     then Dict.empty
                     else oldData.setAbilitiesOnNextTick
              }
          , Cmd.none
          )

    HttpResponse (Ok ChoiceRegistered) ->
      (model, load)

    HttpResponse (Ok LeveledUp) ->
      (model, load)

    HttpResponse (Ok UpdatedBaseAbilityScores) ->
      (model, load)

    Tick _ ->
      ( model
      , if Dict.isEmpty oldData.setAbilitiesOnNextTick
        then Cmd.none
        else
          Http.get -- TODO should probably be POST
            { url = requestUrl "set_base_abilities"
                (  applyBaseAbilityChanges oldData.abilityTable oldData.setAbilitiesOnNextTick
                |> Dict.map (\_ v -> String.fromInt v)
                |> Dict.toList
                )
            , expect = Http.expectJson
                       (mkHttpResponseMsg (\_ -> UpdatedBaseAbilityScores))
                       (D.succeed ())
            }
            

          
      )

    EditCharacterLevel newLevel ->
      ( applyPageData model { oldData | selectedLevel = Just newLevel }
      , Cmd.none
      )

    GotoSheet ->
      applyPage model (Loading, Nav.pushUrl model.key "/sheet")

    GotoLevelUp ->
      ( applyPageData model { oldData | selectedLevel = Nothing, desc = Nothing }
      , Cmd.none
      ) 

    LevelUpAs class ->
      -- TODO should be POST
      (model, Http.get
         { url = requestUrl "gain_level" [("class", class)]
         , expect = Http.expectJson
                    (mkHttpResponseMsg (\_ -> LeveledUp))
                    (D.succeed ())
         }
      )

    SetBaseAbilityScore ability newScore -> 
      ( applyPageData model { oldData
                            | setAbilitiesOnNextTick =
                                Dict.insert ability newScore oldData.setAbilitiesOnNextTick
                            }
      , Cmd.none
      )

    OrSCChooseDir origin id dir ->
      let
        newOptions : Dict Level (List Options)
        newOptions =
          Dict.map
            (\_ ->
               List.map
                 (\opt ->
                    if (origin, id) /= (opt.origin, opt.id)
                    then opt
                    else case opt.spec of
                           OrSC _ left right -> { opt | spec = OrSC (Just dir) left right }
                           _                 -> opt))
            oldData.optionsPerLevel
        _ = Debug.log "" (origin, id, dir)
      in
        ( applyPageData model { oldData | optionsPerLevel = newOptions, desc = Nothing }
        , Cmd.none
        )

    SetEditCharacterPageDesc desc ->
      ( applyPageData model { oldData | desc = desc }
      , Cmd.none
      ) 

    _ ->
      let _ = Debug.log "" msg
      in errorPage model ("Page.EditCharacter.update called with "
                           ++ Debug.toString msg)
      
applyBaseAbilityChanges : AbilityTable -> Dict Ability Int -> Dict Ability Int
applyBaseAbilityChanges abilityTable setAbilitiesOnNextTick =
  Dict.union setAbilitiesOnNextTick (Dict.map (\k v -> v.base) abilityTable)

--------------------------------------------------------------------------------
-- VIEW
--------------------------------------------------------------------------------
view : Maybe String -> EditCharacterPageData -> List (Html Msg)
view focusedDropdownId data =
  [ Elements.viewNavButtons
      [ Elements.viewGotoSheetButton
      , Elements.viewSelectCharacterButton
      ]
  , viewSideNav data
  , viewMain focusedDropdownId data
  ]

tooltipSize = 80

viewSideNav : EditCharacterPageData -> Html Msg
viewSideNav { desc, optionsPerLevel, selectedLevel } =
  div [ Attr.css sideNavStyle ] <|
    case desc of
      Just (title :: paragraphs) ->
        h2 [ Attr.css descStyle ] [ text title ]
        ::
        List.map
          (p [ Attr.css (Css.fontSize (Css.px 12) :: descStyle) ] << List.singleton << text)
          paragraphs
      _ -> 
        viewLevelUpButton selectedLevel 
        ::
        ( Dict.keys optionsPerLevel
          |> List.reverse
          |> List.map (viewSideNavLevelButton selectedLevel) )

descStyle : List Style            
descStyle =
  [ Css.color (Css.hex "ffffff")
  , Css.padding2 (Css.px 0) (Css.px 10)
  ]

viewSideNavLevelButton : Maybe Level -> Level -> Html Msg
viewSideNavLevelButton selectedLevel lvl =
  button
    [ Attr.css (sideNavButtonStyle (Just lvl == selectedLevel))
    , E.onClick (EditCharacterLevel lvl)
    ]
    [ text ("Level " ++ String.fromInt lvl) ]

viewLevelUpButton : Maybe Level -> Html Msg
viewLevelUpButton selectedLevel =
  button
    [ Attr.css (sideNavButtonStyle (selectedLevel == Nothing)), E.onClick GotoLevelUp ]
    [ text "+" ]

viewMain : Maybe String -> EditCharacterPageData -> Html Msg
viewMain focusedDropdownId { abilityTable, optionsPerLevel, traitsAndBonusesPerLevel, selectedLevel, setAbilitiesOnNextTick } =
  div
    [ Attr.css mainSectionStyle ] 
    [ viewTopBar abilityTable setAbilitiesOnNextTick
    , div
        [ Attr.css
            [ Css.padding2 (Css.px 150) (Css.px 25)
            ]
        ]
        (viewMainContents focusedDropdownId optionsPerLevel traitsAndBonusesPerLevel selectedLevel)
    ]

viewTopBar : AbilityTable -> Dict Ability Int -> Html Msg
viewTopBar abilityTable setAbilitiesOnNextTick =
  div
    [ Attr.css topBarStyle ]
    [ table
        [ Attr.css
            [ Css.maxWidth (Css.px 600)
            , Css.width (Css.pct 100)
            , Css.tableLayout Css.fixed
            , Css.fontFamily Css.monospace
            ]
        ]
        [ abilityNamesTableRow
        , baseAbilityValuesTableRow abilityTable setAbilitiesOnNextTick
                        
        , abilityBonusTableRow (listFromAbilityTable .totalBonus abilityTable)
        , abilityScoreTableRow (listFromAbilityTable .score abilityTable)
        , abilityModTableRow (listFromAbilityTable .mod abilityTable)
        ]
    ]

abilityNamesTableRow : Html Msg
abilityNamesTableRow =
  tr [] <| topBarTh "" :: List.map topBarTh abilities

baseAbilityValuesTableRow : AbilityTable -> Dict Ability Int -> Html Msg
baseAbilityValuesTableRow abilityTable setAbilitiesOnNextTick =
            -- <| List.map2 Tuple.pair abilities (listFromAbilityTable .base abilityTable)
  let
    virtualBaseScoresDict =
      applyBaseAbilityChanges abilityTable setAbilitiesOnNextTick
    virtualBaseScoresList =
      abilities |> List.map (\abi ->
                               ( abi
                               , Dict.get abi virtualBaseScoresDict
                                 |> Maybe.withDefault 0
                               ))
  in 
    tr []
      <| topBarTh "base score"
         :: (virtualBaseScoresList
            |> List.map
               (\(ability, baseScore) ->
                  (td
                     [ Attr.css topBarTdCss ]
                     [ input
                         [ Attr.type_ "number"
                         , Attr.value (String.fromInt baseScore)
                         , Attr.css [ Css.maxWidth (Css.px 40) ]
                         , E.onInput (SetBaseAbilityScore ability
                                        << Maybe.withDefault 0
                                        << String.toInt)
                         ]
                         []])))

abilityBonusTableRow : List Int -> Html msg
abilityBonusTableRow bonuses =
  tr [] <| topBarTh "total bonus" :: List.map (topBarTd << Util.formatModifier) bonuses

abilityScoreTableRow : List Int -> Html msg
abilityScoreTableRow scores =
  tr [] <| topBarTh "score" :: List.map (topBarTd << String.fromInt) scores

abilityModTableRow : List Int -> Html msg
abilityModTableRow modifiers =
  tr [] <| topBarTh "mod" :: List.map (topBarTd << Util.formatModifier) modifiers

topBarTh : String -> Html msg
topBarTh val = th [] [text val]

topBarTd : String -> Html msg               
topBarTd val = td [Attr.css topBarTdCss] [text val]

topBarTdCss : List Style
topBarTdCss = [Css.textAlign Css.center]

viewMainContents :  Maybe String -> Dict Level (List Options) -> Dict Level (List Effect) -> Maybe Level
                 -> List (Html Msg)
viewMainContents focusedDropdownId opts tbs selectedLevel =
  case selectedLevel of
    Just level ->
      let
        tbsHtml =
          tbs
          |> Dict.get level |> Maybe.withDefault []
          |> categorizeEffects |> Dict.toList
          |> List.map viewEffectCategory

        optsHtml = 
          (opts
          |> Dict.get level
          |> Maybe.withDefault []
          |> Util.multiDictFromList (\{display_origin_category, origin_category_index} ->
                                       (origin_category_index, display_origin_category))
          |> Dict.map (viewOriginCategoryOptions focusedDropdownId)
          |> Dict.values)

      in
        List.filter (\_ -> not (List.isEmpty tbsHtml))
          (h1 [] [text "You gained:"] :: tbsHtml)
        ++
        List.filter (\_ -> not (List.isEmpty optsHtml)) -- not (Dict.isEmpty (Dict.get level opts)))
          (h1 [] [text "You need to make the following choices:"] :: optsHtml)
          
      
    Nothing ->
      viewLevelUpPage

-- TODO: this "works", but is not great either in terms of code
-- and presentation. I think this merits a full rework at some point.
viewEffectCategory : (String, List Effect) -> Html Msg
viewEffectCategory (category, effects) =
  let 
    showEffect : (String -> String) -> Effect -> String
    showEffect f = .effect >> defunctor >> List.map (Util.showPrologTerm >> f) >> String.concat

    viewEffects : (String -> String) -> List Effect -> List (Html Msg)
    viewEffects f
      = List.map
          (\effect ->
             case effect.desc of
                 [] -> 
                   text (showEffect f effect)
                 desc -> 
                   tooltip tooltipSize Bottom
                     (text <| showEffect f effect)
                     (div [] <| List.map (simple p) desc))

    commaSeparatedArgs f = viewEffects f >> List.intersperse (text ", ")

    showTool toolName = toolName ++ "'s tools"
    id x = x

    formatCategory : String -> List (Html Msg) -> List (Html Msg)
    formatCategory name members =
      b [] [ text (name ++ ": ") ] :: members

    content =
      case category of
        "armor" ->
          formatCategory "Armor proficiencies"
            <| commaSeparatedArgs id effects ++ [text "."]
        "language" ->
          formatCategory "Languages"
            <| commaSeparatedArgs id effects

        "resistance" ->
          formatCategory "Resistances" <| commaSeparatedArgs id effects
        "saving_throw" ->
          formatCategory "Saving throws" <| commaSeparatedArgs id effects
        "sense" ->
          formatCategory "Senses" <| commaSeparatedArgs id effects
        "tool" ->
          formatCategory "Tool proficiencies" <| commaSeparatedArgs showTool effects
        "spellcasting_focus" ->
          formatCategory "You can use a(n) " <| commaSeparatedArgs id effects ++ [text " spellcasting focus"]
        "weapon" ->
          formatCategory "Weapon proficiencies" <| commaSeparatedArgs id effects
        "skill" ->
          formatCategory "Skills" <| commaSeparatedArgs id effects
        "channel_divinity" ->
          formatCategory "Channel divinity" <| commaSeparatedArgs id effects
        "destroy_undead" ->
          formatCategory "Destroy undead" <| commaSeparatedArgs id effects
        _ ->
          formatCategory (Util.formatSnakeCase category)
            [ text <| String.concat <| List.intersperse ", "
                <| List.map (Util.showPrologTermAlt << .effect) effects ]


    -- showEffect f = .effect >> defunctor >> List.map (showPrologTerm >> f) >> String.concat
  in
    div [ Attr.css originCategoryStyle ] content
      



-- TODO move this somewhere else. Dedicated formatting module maybe?
formatEffect : PrologTerm -> String
formatEffect tm =
  case tm of
    Compound "armor" [Atomic "shield"] -> "Proficiency with shields."
    Compound "armor" [Atomic arg] -> "Proficiency with " ++ arg ++ " armor."
    _ -> Util.showPrologTerm tm

viewLevelUpPage : List (Html Msg)
viewLevelUpPage =
  [ simple h2 "Level Up"
  , simple h3 "Pick a class:"
  , select
      [ E.onInput LevelUpAs ]
      (option [Attr.disabled True, Attr.selected True] [text "-- select an option --"]
       ::
       List.map
          (\x -> option [] [text x])
          -- TODO: fetch this from the server
          ["barbarian", "bard", "cleric", "druid", "fighter", "monk", "paladin", "ranger", "rogue", "sorcerer", "warlock", "wizard"])
  ]

viewOriginCategoryOptions : Maybe String -> (Int, String) -> List Options -> Html Msg
viewOriginCategoryOptions focusedDropdownId (_, category) optionsList =
  div
    [ Attr.css originCategoryStyle ]
    (simple h2 ("From " ++ category ++ ":") :: List.map (viewOptions focusedDropdownId) optionsList)

viewOptions : Maybe String -> Options -> Html Msg
viewOptions focusedDropdownId {origin, spec, id, display_id} =
  div
    [ Attr.css optionsSectionStyle ]
    [ simple h3 display_id
    , viewSpec
        { origin            = origin
        , id                = id
        , dropdownIdSuffix  = ""
        , focusedDropdownId = focusedDropdownId
        , disabledOptions   = []
        }
        (Choice origin id << SingletonChoice) False spec
    ]

type alias ViewSpecContext =
  { origin             : String
  , id                 : String
  , dropdownIdSuffix   : String
  , focusedDropdownId  : Maybe String
  , disabledOptions    : List String
  }

viewSpec : ViewSpecContext
         -> (String -> Msg) -> Bool -> SpecAndChoice
         -> Html Msg
viewSpec ctx mkMsg isDisabled spec =
  div
    [ Attr.css [ Css.marginTop (Css.px 4) ] ]
    [ case spec of
        ListSC selected options ->
          viewListSC ctx mkMsg selected isDisabled options

        FromSC unique n subspecs ->
          viewFromSC ctx unique n subspecs

        OrSC dir left right ->
          viewOrSC ctx dir left right
    ]

viewListSC :  ViewSpecContext
           -> (String -> Msg)
           -> Maybe String -> Bool -> List (String, List String)
           -> Html Msg
viewListSC { disabledOptions, origin, id, focusedDropdownId, dropdownIdSuffix } mkMsg selected isDisabled options =
  let
    dropdownId = String.concat [origin, id, dropdownIdSuffix]
    entries =
      List.map
        (\(entry, desc) -> { entry = entry
                           , desc = entry :: desc
                           , enabled = not <| List.member entry disabledOptions
                           , msg = mkMsg entry
                           })
        options
  in
    dropdown isDisabled dropdownId selected entries (focusedDropdownId == Just dropdownId)

viewFromSC : ViewSpecContext -> Unique -> Int -> List SpecAndChoice -> Html Msg
viewFromSC ctx unique n subspecs =
  let
    {origin, id} = ctx

    choicesList : List String
    choicesList = List.concatMap extractChoicesList subspecs

    editFunctions : List (String -> Msg)
    editFunctions = choiceEditFunctions origin id choicesList

    disabledOptions = if unique then choicesList else []
    k = List.length editFunctions
  in 
    div [] <|
      List.map4
        (\i -> viewSpec
           { ctx
           | dropdownIdSuffix = ctx.dropdownIdSuffix ++ "/" ++ String.fromInt i
           , disabledOptions = disabledOptions
           })
        (List.range 1 k)
        editFunctions
        (List.repeat k False)
        subspecs
      ++
      List.map3
        (\i -> viewSpec
           { ctx | disabledOptions = disabledOptions, dropdownIdSuffix = ctx.dropdownIdSuffix ++ "/" ++ String.fromInt i}
           (\opt -> Choice origin id <| ListChoice <| choicesList ++ [opt]))
        (List.range (k+1) n)
        (List.isEmpty choicesList :: List.repeat n True)
        (List.drop k subspecs)

choiceEditFunctions : String -> String -> List String -> List (String -> Msg)
choiceEditFunctions origin id choices =
  case choices of
    [] ->
      [ Choice origin id << ListChoice << List.singleton ]
    c :: cs ->
      let
        zipper = Zipper [] c cs

        overwriteFocused : Zipper String -> (String -> Msg)
        overwriteFocused (Zipper pre _ post) =
          \x -> Zipper pre x post |> Zipper.toList |> ListChoice |> Choice origin id
      in 
        Zipper.toList (Zipper.extend overwriteFocused zipper)

viewOrSC :  ViewSpecContext
         -> Maybe Dir -> (String, SpecAndChoice) -> (String, SpecAndChoice)
         -> Html Msg
viewOrSC ctx dir (lname, lspec) (rname, rspec) =
  let
    { origin, id } = ctx
    name = origin ++ "_" ++ id
    leftId  = String.concat <| List.intersperse "_" [origin, id, lname]
    rightId = String.concat <| List.intersperse "_" [origin, id, rname]
  in 
    div []
      [ input [ Attr.type_ "radio"
              , Attr.checked (dir == Just L)
              , Attr.id leftId
              , Attr.name name
              , E.onInput (\_ -> OrSCChooseDir origin id L)
              ] []
      , label [ Attr.for leftId ] [ text lname ]
      , input [ Attr.type_ "radio"
              , Attr.id rightId
              , Attr.name name
              , Attr.checked (dir == Just R)
              , E.onInput (\_ -> OrSCChooseDir origin id R)
              ] []
      , label [ Attr.for rightId ] [ text rname ]
      , div [] <|
          case dir of
            Nothing   -> []
            Just dir_ -> [ viewSpec ctx
                             (Choice origin id << SingletonChoice)
                             False
                             (case dir_ of
                                L -> lspec
                                R -> Debug.log "" rspec)
                         ]
      ]


-- viewSpec :  List String -> String -> String
--          -> (String -> Msg) -> Bool -> SpecAndChoice
--          -> Html Msg
-- viewSpec disabledOptions origin id mkMsg isDisabled spec =

groupOptionsByOriginCategory : List Options -> Dict String (List Options)
groupOptionsByOriginCategory =
  Util.multiDictFromList .origin_category

--------------------------------------------------------------------------------
-- STYLES
--------------------------------------------------------------------------------

sideNavStyle : List Style
sideNavStyle = 
  [ Css.height (Css.pct 100)
  , Css.width (Css.px sideNavWidth)
  , Css.position Css.fixed
  , Css.zIndex (Css.int 1)
  , Css.top Css.zero
  , Css.left Css.zero
  , Css.backgroundColor (Css.hex "010101")
  , Css.overflowX Css.hidden
  ]

sideNavButtonStyle : Bool -> List Style
sideNavButtonStyle highlighted =
  [ Css.backgroundColor Css.transparent
  , Css.border Css.zero
  , Css.marginLeft (Css.px 20)
  , Css.marginTop (Css.px 15)
  , Css.padding Css.zero
  , Css.color <| Css.hex <| if highlighted then "ffffff" else "818181"
  , Css.cursor Css.pointer
  , Css.fontSize (Css.px 25)
  , Css.display Css.block
  , Css.hover [Css.color (Css.hex "ffffff")]
  -- , Css.fontFamilies [ "Dosis" ]
  ]

mainSectionStyle : List Style
mainSectionStyle =
  [ Css.marginLeft (Css.px sideNavWidth)
  -- , Css.fontFamilies [ "Dosis" ]
  -- , Css.padding2 Css.zero (Css.px 10)
  ]

originCategoryStyle : List Style
originCategoryStyle =
  [ Css.backgroundColor (Css.hex "eeeeee")
  , Css.borderRadius (Css.px 10)
  , Css.padding4 (Css.px 1) (Css.px 0) (Css.px 16) (Css.px 20) -- top right bot left
  , Css.marginTop (Css.px 8)
  -- , Css.fontFamilies [ "Dosis" ]
  ]

optionsSectionStyle : List Style
optionsSectionStyle =
  [ Css.backgroundColor (Css.hex "dddddd")
  , Css.padding4 (Css.px 1) (Css.px 0) (Css.px 16) (Css.px 20) -- top right bot left
  , Css.marginTop (Css.px 4)
  , Css.marginRight (Css.px 16)
  , Css.borderRadius (Css.px 10)
  ]

topBarStyle : List Style
topBarStyle =
  [ Css.position Css.fixed
  , Css.width (Css.pct 100)
  , Css.overflow Css.hidden
  , Css.zIndex (Css.int 2)
  , Css.backgroundColor (Css.hex "#ffffff")
  , Css.borderBottom3 (Css.px 2) (Css.solid) (Css.hex "#000000")
  , Css.padding4 (Css.px 10) (Css.px 10) (Css.px 10) (Css.px 10)
  ]

sideNavWidth : Float
sideNavWidth = 260

--------------------------------------------------------------------------------
-- DATA PROCESSING
--------------------------------------------------------------------------------
categorizeEffects : List Effect -> Dict String (List Effect)
categorizeEffects = Util.multiDictFromList categorizeEffect

categorizeEffect : Effect -> String
categorizeEffect { effect } =
  case effect of
    Compound cat _ -> if Set.member cat effectCategories then cat else "other"
    _ -> "other"

effectCategories : Set String
effectCategories = Set.fromList
                    [ "armor", "language", "resistance", "saving_throw"
                    , "sense", "tool", "spellcasting_focus", "weapon", "skill",
                      "channel_divinity", "destroy_undead"
                    ]
--------------------------------------------------------------------------------
-- UTIL
--------------------------------------------------------------------------------
applyPageData : Model -> EditCharacterPageData -> Model
applyPageData model data =
  { model | page = EditCharacterPage data }
