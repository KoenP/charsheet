module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (Html, Attribute, button, input, div, text, ul, li, h2)
import Html.Attributes exposing (style, placeholder, type_)
-- import Html.Styled exposing (Html, button, div, text, ul, li, styled)
import Html.Events exposing (onClick, onInput)
-- import Css
import Http
import Json.Decode exposing (Decoder, field, list, string, succeed)
import Platform.Cmd exposing (none)
import List
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
  Browser.element
    { init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }

-- MODEL
type Model
  = Loading
  | Error
  | CharacterSelectionPage { characters : List String
                           , newCharacterName : String
                           }
  | CharacterSheetPage CharacterSheet

type alias CharacterSheet = { name : String }

init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
      { url = requestUrl "list_characters" []
      , expect = Http.expectJson
                 (mkHttpResponseMsg GotCharacterList)
                 (field "list" (list string))
      }
  )
  

-- UPDATE
type Msg
  = HttpResponse (Result Http.Error HttpResponseMsg)
  | SelectCharacter String
  | NewCharacterName String
  | CreateNewCharacter

type HttpResponseMsg
  = GotCharacterList (List String)
  | CharacterLoaded CharacterSheet

mkHttpResponseMsg : (a -> HttpResponseMsg) -> (Result Http.Error a -> Msg)
mkHttpResponseMsg f result =
  HttpResponse (Result.map f result)
    
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  log (toString (msg, model))
    (case model of
       CharacterSelectionPage pageData ->
         case msg of
           NewCharacterName name ->
             (CharacterSelectionPage { pageData | newCharacterName = name }, none)
           SelectCharacter name ->
             (Loading, loadCharacter name)
           CreateNewCharacter ->
             (Loading, newCharacter pageData.newCharacterName)
           _ ->
             (Error, none)
       CharacterSheetPage _ ->
         (model, none)
       Loading ->
         case msg of
           HttpResponse (Ok responseMsg) ->
             handleHttpResponseMsg responseMsg model
           _ ->
             (Error, none)
       Error ->
         (Error, none))

handleHttpResponseMsg : HttpResponseMsg -> Model -> (Model, Cmd Msg)
handleHttpResponseMsg msg model =
  case msg of
      GotCharacterList chars -> 
        (CharacterSelectionPage { characters = chars, newCharacterName = "" }, none)
      CharacterLoaded sheet ->
        (CharacterSheetPage sheet, none)
               
loadCharacter : String -> Cmd Msg
loadCharacter charName =
  Http.get
    { url = requestUrl "load_character" [("name", charName)]
    , expect = Http.expectJson (mkHttpResponseMsg CharacterLoaded) decodeSheetJson
    }

newCharacter : String -> Cmd Msg
newCharacter charName =
  Http.get
    { url = requestUrl "new_character" [("name", charName)]
    , expect = Http.expectJson (mkHttpResponseMsg CharacterLoaded) decodeSheetJson
    }

decodeSheetJson : Decoder CharacterSheet
decodeSheetJson = Json.Decode.map CharacterSheet (field "name" string)

-- VIEW
view : Model -> Html Msg
view model =
  case model of
      Loading ->
        text "Loading..."
      Error -> 
        text "Error"
      CharacterSelectionPage data ->
        characterSelectionPage data
      CharacterSheetPage data ->
        text data.name

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
