module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Browser.Events
import Css as Css
import Css.Global as Css
import Url exposing (Url)
import Url.Parser exposing (Parser, (</>))
import Url.Parser as Parser
import Html
import Html.Attributes
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Attributes exposing (style, placeholder, type_)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Dict exposing (Dict)
import Json.Decode exposing (Decoder, field, list, string, succeed)
import Platform.Cmd exposing (none)
import List
import Maybe
import Debug exposing (log, toString)
import Time exposing (Posix)

import Page.CharacterSheet as Sheet
import Page.PrintableCharSheet as PSheet
import Page.EditCharacter as Edit
import Page.CardsPage as Cards
import Request exposing (requestUrl)
import Types exposing (..)

----------------------------------------------------------------------

-- MAIN
main =
  Browser.application
    { init = init
    , view = view
    , update = updateOrTick
    , subscriptions = subscriptions
    , onUrlRequest = LinkClicked
    , onUrlChange = UrlChanged
    }

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions { focusedDropdownId, page } =
  let
    clickoutSub =
      case focusedDropdownId of
        Just _ -> 
          Browser.Events.onClick (succeed ClickOut)
        Nothing ->
          Sub.none

    timeSub =
      case page of
        EditCharacterPage _ ->
          Time.every 500 Tick
        _ ->
          Sub.none
  in 
    Sub.batch [clickoutSub, timeSub]
    

-- MODEL
init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
  ( { url = url
    , key = key
    , preparedSpells = Dict.empty
    , showOnlyPreparedSpells = False
    , page = Loading
    , focusedDropdownId = Nothing
    , lastTick = Time.millisToPosix 0
    }
  , loadSelectCharacterPage
  -- , PSheet.load
  -- , Sheet.load
  -- , Cards.load
  -- , Edit.load
  -- , Nav.pushUrl key "/list_characters"
  )

navigate : Model -> Route -> ( Model, Cmd Msg )
navigate model route = -- ( { model | page = Loading } , Sheet.load )
  case route of
    CharacterRoute charName ->
      Debug.log ("navigate (CharacterRoute " ++ charName ++ ")")
        ( { model | page = Loading }
        , PSheet.load
        )
      
    SelectCharacterRoute ->
      Debug.log "navigate (SelectCharacterRoute)"
        ( { model | page = Loading }
        , loadSelectCharacterPage
        )
      
    SheetRoute ->
      Debug.log "navigate (SheetRoute)"
        ( { model | page = Loading }
        , PSheet.load
        )

    EditRoute ->
      Debug.log "navigate (EditRoute)"
        ( { model | page = Loading }
        , Edit.load
        )
    _ -> ( model, none )

loadSelectCharacterPage : Cmd Msg
loadSelectCharacterPage = 
  Http.get
    { url = requestUrl "list_characters" []
    , expect = Http.expectJson
               (mkHttpResponseMsg GotCharacterList)
               (field "list" (list string))
    }
  

type Route
  = SelectCharacterRoute
  | SheetRoute
  | EditRoute
  | NotFound
  | CharacterRoute String
  
routeParser : Parser (Route -> a) a
routeParser =
  Parser.oneOf
    [ Parser.map SelectCharacterRoute Parser.top
    , Parser.map SelectCharacterRoute (Parser.s "src" </> Parser.s "Main.elm")
    , Parser.map SelectCharacterRoute (Parser.s "list_characters")
    , Parser.map SheetRoute (Parser.s "sheet")
    , Parser.map EditRoute (Parser.s "edit")
    , Parser.map CharacterRoute (Parser.s "character" </> Parser.string)

      -- Parser.s "char" : Parser a a
      -- </> : Parser (String -> d) (String -> d) -> Parser (String -> d) c -> Parser (String -> d) c
      -- Parser.string : Parser (String -> d) d
    ]

urlToRoute : Url -> Route
urlToRoute url =
  Maybe.withDefault NotFound (Parser.parse routeParser url)

-- UPDATE
verbose = True

updateOrTick : Msg -> Model -> (Model, Cmd Msg)
updateOrTick msg model =
  case msg of
    Tick time -> update msg { model | lastTick = time }
    _         ->
      let _ = if verbose then Debug.log "Global update (old model)" model else model
          _ = if verbose then Debug.log "Global update (msg)" msg else msg
          _ = if verbose then Debug.log "--------" "" else ""
      in 
        update msg model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Null ->
      ( model, none )
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url))
        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      navigate model (urlToRoute url)

    EditCharacter ->
      ( { model | page = Loading }, Edit.load )

    GotoSelectCharacterPage ->
      ( { model | page = Loading }, loadSelectCharacterPage )

    GotoCardsPage options sheet ->
      ( { model | page = CardsPage options sheet }, Cmd.none)

    GotoSheet ->
      applyPage model (Loading, Nav.pushUrl model.key "/sheet")

    Choice origin id choice ->
      ( { model | focusedDropdownId = Nothing } , registerChoice origin id choice )

    SelectDropdownOption dropdownId optionId -> 
      let _ = Debug.log "" ("Selected dropdown option " ++ optionId ++ " from dropdown " ++ dropdownId)
      in ( { model | focusedDropdownId = Nothing }, none )

    ToggleDropdown dropdownId -> 
      let newFocusedDropdownId = if model.focusedDropdownId == Just dropdownId
                                 then Nothing
                                 else Just dropdownId
      in ( { model | focusedDropdownId = newFocusedDropdownId }, none )

    ClickOut ->
      ( { model | focusedDropdownId = Nothing }, none )

    _ ->
      case model.page of
        CharacterSelectionPage pageData ->
          applyPage model (updateCharacterSelectionPage msg pageData)
        CharacterSheetPage sheet ->
          Sheet.update msg model
        PrintableCharSheetPage sheet ->
          PSheet.update msg model
        EditCharacterPage data ->
          Edit.update msg model data
        CardsPage options data ->
          Cards.update msg model data
        Loading ->
          case msg of
            HttpResponse (Ok responseMsg) ->
              handleHttpResponseMsg responseMsg model
            _ ->
              let
                error = "Main.update: Expected HttpResponse, got " ++
                        Debug.toString msg
              in 
                ( { model | page = Error error }, none )
        Error error  ->
          ( { model | page = Error error }, none )

updateCharacterSelectionPage
  : Msg -> CharacterSelectionPageData -> (Page, Cmd Msg)
updateCharacterSelectionPage msg pageData =
  case msg of
    NewCharacterName name ->
      ( CharacterSelectionPage { pageData | newCharacterName = name }, none )
    SelectCharacter name ->
      ( Loading, loadCharacter name )
    CreateNewCharacter ->
      ( Loading, newCharacter pageData.newCharacterName )
    _ ->
      ( Error ("Main.updateCharacterSelectionPage: unrecognized msg "
                 ++ Debug.toString msg)
      , none
      )
  
handleHttpResponseMsg : HttpResponseMsg -> Model -> (Model, Cmd Msg)
handleHttpResponseMsg msg model =
  case msg of
      GotCharacterList chars -> 
        ( { model
            | page = CharacterSelectionPage { characters = chars, newCharacterName = "" }
          }
        , none
        )
      CharacterLoaded id ->
        ( { model | page = Loading }
        -- , Nav.pushUrl model.key ("/character/" ++ id ++ "/edit")
        , Nav.pushUrl model.key "/edit"
        )
      GotCharacterSheet sheet ->
        ( { model
            | page = CharacterSheetPage sheet
            , preparedSpells = initPreparedSpells sheet.spellcasting_sections
          }
        , none
        )
      GotPrintableCharSheet sheet ->
        ( { model | page = PrintableCharSheetPage sheet }
        , none
        )
      GotCharacterOptions abilityTable optionsPerLevel traitsAndBonusesPerLevel ->
        let charLevel = Dict.keys optionsPerLevel |> List.maximum |> Maybe.withDefault 1
        in ( { model
               | page = EditCharacterPage
                        { abilityTable = abilityTable
                        , optionsPerLevel = optionsPerLevel
                        , traitsAndBonusesPerLevel = traitsAndBonusesPerLevel 
                        , charLevel = charLevel
                        , selectedLevel = Just 1 -- Just charLevel (TODO restore)
                        , desc = Nothing
                        , setAbilitiesOnNextTick = Dict.empty
                        }
             }
           , none
           )
      ChoiceRegistered ->
        (model, Edit.load)
      _ ->
        errorPage
          model
          ("handleHttpResponseMsg called with unsupported message: "
          ++ Debug.toString msg)
               
loadCharacter : String -> Cmd Msg
loadCharacter charName =
  Http.get
    { url = requestUrl "load_character" [("name", charName)]
    , expect = Http.expectJson (mkHttpResponseMsg (\_ -> CharacterLoaded charName)) (succeed ())
    }

newCharacter : String -> Cmd Msg
newCharacter charName =
  Http.get
    { url = requestUrl "new_character" [("name", charName)]
    , expect =
        Http.expectJson
        (mkHttpResponseMsg (\_ -> CharacterLoaded charName))
        (succeed ())
    }

registerChoice : String -> String -> Choice -> Cmd Msg
registerChoice origin id choice = let _ = Debug.log "registerChoice" choice in 
  Http.get
    { url = requestUrl "choice" [ ("source", origin)
                                , ("id", id)
                                , ("choice", choiceToString choice)
                                ]
    , expect =
        Http.expectJson (mkHttpResponseMsg (\_ -> ChoiceRegistered)) (succeed ())
    }

choiceToString : Choice -> String
choiceToString choice =
  case choice of
    SingletonChoice sc ->
      sc
    ListChoice list ->
      "[" ++ String.concat (List.intersperse "," list) ++ "]"

----------------------------------------------------------------------
-- VIEW
----------------------------------------------------------------------
view : Model -> Browser.Document Msg
view model =
  { title = "Character Sheet"
  , body =
    Html.node
      "link"
      -- [ Html.Attributes.attribute "href" "https://fonts.googleapis.com/css2?family=Atkinson+Hyperlegible:ital,wght@0,400;0,700;1,400;1,700&display=swap"
      [ Html.Attributes.attribute "href" "https://fonts.googleapis.com/css2?family=Dosis:wght@400;700&display=swap"
--"https://fonts.googleapis.com/css2?family=Dosis&family=Epilogue:wght@300&family=Fira+Code&family=Quattrocento+Sans&family=Roboto:wght@300-800&display=swap"

      , Html.Attributes.attribute "rel" "stylesheet"
      ]
      []
    :: Html.node
      "link"
      [ Html.Attributes.attribute "href" "css/printable-char-sheet.css"
      , Html.Attributes.attribute "rel" "stylesheet"
      ]
    []
    :: toUnstyled globalCss
    ::
    List.map toUnstyled 
      (case model.page of
         Loading ->
           [ text "Loading..." ]
         Error msg -> 
           [ text msg ]
         CharacterSelectionPage data ->
           characterSelectionPage data model
         CharacterSheetPage data ->
           Sheet.view model.preparedSpells model.showOnlyPreparedSpells data
         PrintableCharSheetPage data ->
           PSheet.view data
         EditCharacterPage data ->
           Edit.view model.focusedDropdownId data
         CardsPage options data ->
           Cards.view options data model.preparedSpells )
  }

globalCss : Html msg
globalCss =
  Css.global
    [ Css.class "card-description"
        [ Css.descendants
            [ Css.typeSelector "p"
                [ Css.marginBottom (Css.px 2)
                , Css.marginTop Css.zero
                ]
            , Css.typeSelector "ul"
                [ Css.paddingLeft (Css.px 10)
                ]
            , Css.typeSelector "td"
                [ Css.marginBottom Css.zero
                , Css.marginTop Css.zero
                , Css.paddingTop Css.zero
                , Css.paddingBottom Css.zero
                 -- Css.lineHeight (Css.num 0)
                 
                ]
            -- , Css.selector "::marker"
            --     [ Css.fontSize (Css.px 0)
            --     -- Weird hack to make a duplicate bullet point disappear.
            --     -- I don't know why this works and visiblity hidden does not.
            --     ]
            ]
        ]
        
    ]
  

-- Character selection page
characterSelectionPage : { characters : List String, newCharacterName : String }
                       -> Model
                       -> List (Html Msg)
characterSelectionPage { characters, newCharacterName } { focusedDropdownId } =
  let
    characterList = div [] [ul [] (List.map selectCharButton characters)]
  in 
    [ h2 [] [ text "Create a new character" ]
    , input
        [ type_ "text", placeholder "New character name", onInput NewCharacterName ]
        []
    , button [ onClick CreateNewCharacter ] [ text "Create" ]
    , h2 [] [ text "... or select an existing one" ]
    , characterList
    ]

-- Shared
textSingleton : String -> List (Html msg)
textSingleton = List.singleton << text

selectCharButton : String -> Html Msg
selectCharButton char =
  li []
    [ button
        ( onClick (SelectCharacter char) :: linkButtonStyle )
        [ text char ]
    ]

linkButtonStyle : List (Attribute msg)
linkButtonStyle =
  [ style "background" "none"
  , style "border" "none"
  , style "padding" "0"
  , style "font-family" "arial, sans-serif"
  , style "text-decoration" "underline"
  , style "cursor" "pointer"
  ]
