module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Browser.Navigation as Nav
import Url
import Url exposing (Url)
import Url.Parser exposing (Parser, (</>))
import Url.Parser as Parser
import Html exposing (..)
import Html.Attributes as Attr
import Html.Attributes exposing (style, placeholder, type_)
-- import Html.Styled exposing (Html, button, div, text, ul, li, styled)
import Html.Events exposing (onClick, onInput)
-- import Css
import Http
import Json.Decode exposing (Decoder, field, list, string, succeed)
import Platform.Cmd exposing (none)
import List
import Maybe
import Debug exposing (log, toString)

requestUrl : String -> List (String, String) -> String
requestUrl req params =
  "http://localhost:8000/request/" ++ req
    ++ case params of
          [] -> ""
          ps -> String.concat
                ("?" ::
                   List.intersperse "&" (List.map (\(x,y) -> x ++ "=" ++ y) ps))

-- MAIN
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    , onUrlRequest = LinkClicked
    , onUrlChange = UrlChanged
    }
  -- Browser.element
  --   { init = init
  --   , update = update
  --   , subscriptions = \_ -> Sub.none
  --   , view = view
  --   }

-- MODEL
type alias Model =
  { url : Url.Url
  , key : Nav.Key
  , page : Page
  }
type Page
  = Loading
  | Error
  | CharacterSelectionPage CharacterSelectionPageData
  | CharacterSheetPage CharacterSheet

type alias CharacterSheet = { name : String }
type alias CharacterSelectionPageData =
  { characters : List String
  , newCharacterName : String
  }

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
  navigate { url = url, key = key, page = Loading } (urlToRoute url)

navigate : Model -> Route -> ( Model, Cmd Msg )
navigate model route =
  case route of
    SelectCharacterRoute -> ( { model | page = Loading }
                            , Http.get
                                { url = requestUrl "list_characters" []
                                , expect = Http.expectJson
                                           (mkHttpResponseMsg GotCharacterList)
                                           (field "list" (list string))
                                }
                            )
    SheetRoute -> ( { model | page = Loading }
                  , getCharacterSheet
                  )
    _ -> ( model, none )

type Route = SelectCharacterRoute | SheetRoute | NotFound
  
routeParser : Parser (Route -> a) a
routeParser =
  Parser.oneOf
    [ Parser.map SelectCharacterRoute Parser.top
    , Parser.map SelectCharacterRoute (Parser.s "src" </> Parser.s "Main.elm")
    , Parser.map SheetRoute (Parser.s "sheet")
    ]

urlToRoute : Url -> Route
urlToRoute url =
  Maybe.withDefault NotFound (Parser.parse routeParser url)

-- UPDATE
type Msg
  = HttpResponse (Result Http.Error HttpResponseMsg)
  | SelectCharacter String
  | NewCharacterName String
  | CreateNewCharacter
  | UrlChanged Url.Url
  | LinkClicked Browser.UrlRequest

type HttpResponseMsg
  = GotCharacterList (List String)
  | CharacterLoaded
  | GotCharacterSheet CharacterSheet

mkHttpResponseMsg : (a -> HttpResponseMsg) -> (Result Http.Error a -> Msg)
mkHttpResponseMsg f result =
  HttpResponse (Result.map f result)
    
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url))
        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      navigate model (urlToRoute url)

    _ ->
      case model.page of
        CharacterSelectionPage pageData ->
          applyPage model (updateCharacterSelectionPage msg pageData)
        CharacterSheetPage _ ->
          ( model, none )
        Loading ->
          case msg of
            HttpResponse (Ok responseMsg) ->
              handleHttpResponseMsg responseMsg model
            _ ->
              ( { model | page = Error }, none )
        Error ->
          ( { model | page = Error }, none )

applyPage : Model -> (Page, Cmd Msg) -> (Model, Cmd Msg)
applyPage model ( page, cmd ) =
  ( { model | page = page }, cmd)

updateCharacterSelectionPage
  : Msg -> CharacterSelectionPageData -> (Page, Cmd Msg)
updateCharacterSelectionPage msg pageData =
  case msg of
    NewCharacterName name ->
      ( CharacterSelectionPage { pageData | newCharacterName = name }, none )
    SelectCharacter name ->
      ( Loading, loadCharacter name)
    CreateNewCharacter ->
      ( Loading, newCharacter pageData.newCharacterName)
    _ ->
      ( Error, none)
  

handleHttpResponseMsg : HttpResponseMsg -> Model -> (Model, Cmd Msg)
handleHttpResponseMsg msg model =
  case msg of
      GotCharacterList chars -> 
        ( { model
            | page = CharacterSelectionPage { characters = chars, newCharacterName = "" }
          }
        , none
        )
      CharacterLoaded ->
        ( { model | page = Loading }, getCharacterSheet )
      GotCharacterSheet sheet ->
        ( { model | page = CharacterSheetPage sheet }, none )

getCharacterSheet : Cmd Msg
getCharacterSheet =
  Http.get
    { url = requestUrl "sheet" []
    , expect = Http.expectJson (mkHttpResponseMsg GotCharacterSheet) decodeSheetJson
    }

decodeSheetJson : Decoder CharacterSheet
decodeSheetJson = Json.Decode.map CharacterSheet (field "name" string)
  
               
loadCharacter : String -> Cmd Msg
loadCharacter charName =
  Http.get
    { url = requestUrl "load_character" [("name", charName)]
    , expect = Http.expectJson (mkHttpResponseMsg (\_ -> CharacterLoaded)) (succeed ())
    }

newCharacter : String -> Cmd Msg
newCharacter charName =
  Http.get
    { url = requestUrl "new_character" [("name", charName)]
    , expect = Http.expectJson (mkHttpResponseMsg (\_ -> CharacterLoaded)) (succeed ())
    }

--------------------------------------------------------------------------------
-- VIEW
--------------------------------------------------------------------------------
view : Model -> Browser.Document Msg
view model =
  { title = "Character Sheet"
  , body =
      [ text (Url.toString model.url)
      , case model.page of
          Loading ->
            text "Loading..."
          Error -> 
            text "Error"
          CharacterSelectionPage data ->
            characterSelectionPage data
          CharacterSheetPage data ->
            characterSheetPage data
      ]
  }
  

-- Character selection page
characterSelectionPage : { characters : List String, newCharacterName : String } -> Html Msg
characterSelectionPage { characters, newCharacterName } =
  let
    characterList = div [] [ul [] (List.map selectCharButton characters)]
  in 
    div [] [ h2 [] [ text "Create a new character" ]
           , input
               [ type_ "text", placeholder "New character name", onInput NewCharacterName ]
               []
           , button [ onClick CreateNewCharacter ] [ text "Create" ]
           , h2 [] [ text "... or select an existing one" ]
           , characterList ]

-- Character sheet page
characterSheetPage : CharacterSheet -> Html Msg
characterSheetPage sheet =
  div
  []
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
  , text "test"
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
