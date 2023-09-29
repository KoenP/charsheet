module Page.EditCharacter exposing (..)

import Browser.Navigation as Nav
import Css exposing (Style)
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as E
import Http
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Maybe
import Platform.Cmd as Cmd
import Tuple
import Zipper exposing (Zipper(..))

import Request exposing (requestUrl)
import Types exposing (..)
import Types.Ability exposing (..)
import Util exposing (simple)
import Dropdown exposing (dropdown)

--------------------------------------------------------------------------------
-- LOAD
--------------------------------------------------------------------------------
load : Cmd Msg
load =
  Http.get
    { url = requestUrl "edit_character_page" []
    , expect = Http.expectJson
               (mkHttpResponseMsg (\(x,y) -> GotCharacterOptions x y))
               gotCharacterOptionsDec
    }

gotCharacterOptionsDec : Decoder (AbilityTable, Dict Level (List Options))
gotCharacterOptionsDec =
  D.succeed (\x y -> (x,y))
    |> D.andMap (D.field "ability_table" abilityTableDec)
    |> D.andMap (D.field "options" optionsDictDec)

optionsDictDec : Decoder (Dict Level (List Options))
optionsDictDec =
  D.keyValuePairs (D.list optionsDec)
    |> D.andThen
       (\keyValPairs ->
          List.map
            (\(key, val) ->
               case D.decodeString D.int key of 
                 Ok level -> D.succeed (level, val)
                 Err err  -> D.fail (D.errorToString err))
            keyValPairs
          |> sequenceDecoders
          |> D.map Dict.fromList)

sequenceDecoders : List (Decoder a) -> Decoder (List a)
sequenceDecoders decs =
  case decs of
    []      -> D.succeed []
    d :: ds -> d |> D.andThen (\x -> sequenceDecoders ds |> D.andThen (\xs -> D.succeed (x :: xs)))

optionsDec : Decoder Options
optionsDec =
  D.succeed Options
    |> D.andMap (D.field "charlevel" D.int)
    |> D.andMap (D.field "id" D.string)
    |> D.andMap (D.field "origin" D.string)
    |> D.andMap (D.field "origin_category" D.string)
    |> D.andMap (D.field "origin_category_index" D.int)
    |> D.andMap extractSpecAndChoice

extractSpecAndChoice : Decoder SpecAndChoice
extractSpecAndChoice =
  D.field "spec" specDec |>
  D.andThen (\spec -> D.field "choice"
               (D.oneOf [ addChoiceDec spec
                        , D.null spec
                        ]))

addChoiceDec : SpecAndChoice -> Decoder SpecAndChoice
addChoiceDec spec = 
  case spec of
    ListSC _ options ->
      D.map (\choice -> ListSC (Just choice) options) <| D.string
    OrSC _ left right ->
      D.field "choicetype" (Util.matchStringDec "or") |>
      D.andThen (\_ -> D.field "side" dirDec) |>
      D.andThen (addOrChoiceDec left right)
    FromSC unique n (subspec :: _) ->
      D.map (FromSC unique n) <|
        D.lazy (\_ -> D.map
                  (\specs -> specs ++ List.repeat (n - List.length specs) subspec)
                  (D.list (addChoiceDec subspec)))
    _ -> D.fail "Page.EditCharacter.addChoiceDec: invalid match"

addOrChoiceDec :  (String, SpecAndChoice) -> (String, SpecAndChoice)
               -> Dir -> Decoder SpecAndChoice
addOrChoiceDec (lname,lspec) (rname,rspec) dir =
  let
    subspec = 
      case dir of
        L -> lspec
        R -> rspec
    choiceDec =
      D.field "choice" (addChoiceDec subspec)  
  in
    D.map
      (\newspec -> case dir of
                     L -> OrSC (Just dir) (lname,newspec) (rname,rspec)
                     R -> OrSC (Just dir) (lname,lspec) (rname,newspec))
      choiceDec

dirDec : Decoder Dir
dirDec =
  D.oneOf [ Util.matchStringDec "left"  |> Util.decSet L
          , Util.matchStringDec "right" |> Util.decSet R
          ]

specDec : Decoder SpecAndChoice
specDec =
  D.field "spectype" D.string |> 
  D.andThen
    (\spectype ->
       case spectype of
         "list"        -> listSpecDec
         "or"          -> orSpecDec
         "from"        -> fromSpecDec False
         "unique_from" -> fromSpecDec True
         _             ->
           D.fail "spectype should be 'list', 'or', 'from', or 'unique_from'")

listSpecDec : Decoder SpecAndChoice
listSpecDec =
  D.field "list" <| D.map (ListSC Nothing) <| D.list
    (D.succeed (\x y -> (x,y))
       |> D.andMap (D.field "opt" D.string)
       |> D.andMap (D.field "desc" (D.oneOf [ D.list D.string
                                            , D.string |> D.map List.singleton
                                            ]
                                   )))
    

orSpecDec : Decoder SpecAndChoice
orSpecDec =
  D.succeed (\lName lSpec rName rSpec -> OrSC Nothing (lName,lSpec) (rName,rSpec))
    |> D.andMap (D.field "leftname" D.string)
    |> D.andMap (D.field "left" (D.lazy (\_ -> specDec)))
    |> D.andMap (D.field "rightname" D.string)
    |> D.andMap (D.field "right" (D.lazy (\_ -> specDec)))

fromSpecDec : Unique -> Decoder SpecAndChoice
fromSpecDec unique =
  D.field "num" D.int
    |> D.andThen
       (\n ->
          D.map
            (FromSC unique n)
            (D.field "spec" (D.lazy (\_ -> D.map (List.repeat n) specDec))))

--------------------------------------------------------------------------------
-- UPDATE
--------------------------------------------------------------------------------
update : Msg -> Model -> EditCharacterPageData -> (Model, Cmd Msg)
update msg model oldData =
  case msg of
    HttpResponse (Ok (GotCharacterOptions newAbilityTable newOptions)) ->
      let
        charLevel = newOptions |> Dict.keys |> List.maximum |> Maybe.withDefault 1
      in
        applyPage
          model
          ( EditCharacterPage
              { abilityTable           = newAbilityTable
              , optionsPerLevel        = newOptions
              , charLevel              = charLevel
              , selectedLevel          = Just <| Maybe.withDefault charLevel oldData.selectedLevel
              , desc                   = Nothing
              , setAbilitiesOnNextTick =
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
  [ viewSideNav data, viewMain focusedDropdownId data ]
  
viewSideNav : EditCharacterPageData -> Html Msg
viewSideNav { desc, optionsPerLevel, selectedLevel } =
  div [ Attr.css sideNavStyle ] <|
    case desc of
      Just (title :: paragraphs) ->
        h2 [ Attr.css descStyle ] [ text title ]
        ::
        List.map
          (p [ Attr.css (Css.fontSize (Css.px 12) :: descStyle)] << List.singleton << text)
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
  , Css.fontFamily Css.sansSerif
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
viewMain focusedDropdownId { abilityTable, optionsPerLevel, selectedLevel, setAbilitiesOnNextTick } =
  div
    [ Attr.css
        [ Css.marginLeft (Css.px sideNavWidth)
        , Css.padding2 Css.zero (Css.px 10)
        ]
    ] 
    [ viewTopBar abilityTable setAbilitiesOnNextTick
    , div
        [ Attr.css
            [ Css.padding2 (Css.px 150) (Css.px 25)
            ]
        ]
        (viewMainContents focusedDropdownId optionsPerLevel selectedLevel)
    ]

viewTopBar : AbilityTable -> Dict Ability Int -> Html Msg
viewTopBar abilityTable setAbilitiesOnNextTick =
  div
    [ Attr.css
        [ Css.position Css.fixed
        , Css.width (Css.pct 75)
        , Css.overflow Css.hidden
        , Css.zIndex (Css.int 2)
        , Css.backgroundColor (Css.hex "#ffffff")
        , Css.borderBottom3 (Css.px 2) (Css.solid) (Css.hex "#000000")
        , Css.padding4 (Css.px 10) (Css.px 10) (Css.px 10) (Css.px 10)
        ]
    ]
    [ button [E.onClick GotoSheet] [text "View character sheet"]   
    , table
        [ Attr.css
            [ Css.maxWidth (Css.px 600)
            , Css.width (Css.pct 100)
            , Css.tableLayout Css.fixed
            , Css.fontFamily Css.monospace
            ]
        ]
        [ abilityNamesTableRow
        , baseAbilityValuesTableRow abilityTable setAbilitiesOnNextTick
                        
        , abilityBonusTableRow abilityTable
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

abilityBonusTableRow : AbilityTable -> Html msg
abilityBonusTableRow abilityTable =
  let
    baseScores = listFromAbilityTable .base abilityTable
    totalScores = listFromAbilityTable .score abilityTable
    bonuses = List.map2 (-) totalScores baseScores
  in 
  tr [] <| topBarTh "bonuses" :: List.map (topBarTd << Util.formatModifier) bonuses

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

viewMainContents : Maybe String -> Dict Level (List Options) -> Maybe Level -> List (Html Msg)
viewMainContents focusedDropdownId opts selectedLevel =
  case selectedLevel of
    Just level ->
      (opts
       |> Dict.get level
       |> Maybe.withDefault []
       |> Util.multiDictFromList (\{origin_category, origin_category_index} ->
                                    (origin_category_index, origin_category))
       |> Dict.map (viewOriginCategoryOptions focusedDropdownId)
       |> Dict.values)
    Nothing ->
      viewLevelUpPage

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
  div [] (simple h2 ("From " ++ category ++ ":") :: List.map (viewOptions focusedDropdownId) optionsList)

viewOptions : Maybe String -> Options -> Html Msg
viewOptions focusedDropdownId {origin, spec, id} =
  div
    []
    [ simple h3 id
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
  case spec of
    ListSC selected options ->
      viewListSC ctx mkMsg selected isDisabled options

    FromSC unique n subspecs ->
      viewFromSC ctx unique n subspecs

    OrSC dir left right ->
      viewOrSC ctx dir left right

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


  -- select [ E.onInput mkMsg, Attr.disabled isDisabled ] <|
  --   case selected of
  --     Nothing ->
  --       option [Attr.selected True, Attr.disabled True] [text "-- select an option --"]
  --       :: List.map (viewListSpecOption disabledOptions False) options
  --     Just selectedVal ->
  --       option [Attr.disabled True] [text "-- select an option --"]
  --       :: List.map
  --          (\(opt,desc) ->
  --             viewListSpecOption disabledOptions (opt == selectedVal) (opt,desc))
  --          options
        
-- viewListSpecOption : List String -> Bool -> (String, String) -> Html Msg
-- viewListSpecOption disabledOptions isSelected (opt, desc) =
--   option
--   [ Attr.value opt
--   , Attr.selected isSelected
--   , Attr.disabled (not isSelected && List.member opt disabledOptions)
--   , E.onMouseEnter (Debug.log "onMouseEnter" <| SetEditCharacterPageDesc (Just desc))
--   , E.onMouseLeave (Debug.log "onMouseLeave" <| SetEditCharacterPageDesc Nothing)
--   ]
--   [ text opt ]

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
                                R -> rspec)
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
  ]

sideNavWidth : Float
sideNavWidth = 260

--------------------------------------------------------------------------------
-- UTIL
--------------------------------------------------------------------------------
applyPageData : Model -> EditCharacterPageData -> Model
applyPageData model data =
  { model | page = EditCharacterPage data }
