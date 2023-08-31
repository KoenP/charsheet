module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Browser.Events
import Url exposing (Url)
import Url.Parser exposing (Parser, (</>))
import Url.Parser as Parser
import Html
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

import Page.CharacterSheet as Sheet
import Page.EditCharacter as Edit
import Request exposing (requestUrl)
import Types exposing (..)

----------------------------------------------------------------------

-- MAIN
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = LinkClicked
    , onUrlChange = UrlChanged
    }

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions { focusedDropdownId } =
  case focusedDropdownId of
    Just _ -> 
      Browser.Events.onClick (succeed ClickOut)
    Nothing ->
      Sub.none
    

-- MODEL
init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
  ( { url = url
    , key = key
    , preparedSpells = Dict.empty
    , showOnlyPreparedSpells = False
    , page = Loading
    , focusedDropdownId = Nothing
    }
  , Edit.load -- Nav.pushUrl key "/list_characters"
  )

navigate : Model -> Route -> ( Model, Cmd Msg )
navigate model route = -- ( { model | page = Loading } , Sheet.load )
  case route of
    SelectCharacterRoute ->
      ( { model | page = Loading }
      , Http.get
          { url = requestUrl "list_characters" []
          , expect = Http.expectJson
                     (mkHttpResponseMsg GotCharacterList)
              (field "list" (list string))
          }
      )
    SheetRoute ->
      Debug.log "navigate (SheetRoute)"
        ( { model | page = Loading }
        , Sheet.load
        )
    EditRoute ->
      Debug.log "navigate (EditRoute)"
        ( { model | page = Loading }
        , Edit.load
        )
    _ -> ( model, none )

type Route
  = SelectCharacterRoute
  | SheetRoute
  | EditRoute
  | NotFound
  
routeParser : Parser (Route -> a) a
routeParser =
  Parser.oneOf
    [ Parser.map SelectCharacterRoute Parser.top
    , Parser.map SelectCharacterRoute (Parser.s "src" </> Parser.s "Main.elm")
    , Parser.map SelectCharacterRoute (Parser.s "list_characters")
    , Parser.map SheetRoute (Parser.s "sheet")
    , Parser.map EditRoute (Parser.s "edit")
    ]

urlToRoute : Url -> Route
urlToRoute url =
  Maybe.withDefault NotFound (Parser.parse routeParser url)

-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let _ = Debug.log "Global update (old model)" model
      _ = Debug.log "Global update (msg)" msg
      _ = Debug.log "--------" ""
  in case msg of
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
         ( { model | page = Loading }, Nav.pushUrl model.key "/edit" )

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
           EditCharacterPage options selectedLevel _ ->
             Edit.update msg model options selectedLevel
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
      CharacterLoaded ->
        ( { model | page = Loading }, Nav.pushUrl model.key "/sheet" )
      GotCharacterSheet sheet ->
        ( { model
            | page = CharacterSheetPage sheet
            , preparedSpells = initPreparedSpells sheet.spellcasting_sections
          }
        , none
        )
      GotCharacterOptions options ->
        ( { model
            | page = EditCharacterPage options (Just 1) Nothing
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
    , expect = Http.expectJson (mkHttpResponseMsg (\_ -> CharacterLoaded)) (succeed ())
    }

newCharacter : String -> Cmd Msg
newCharacter charName =
  Http.get
    { url = requestUrl "new_character" [("name", charName)]
    , expect =
        Http.expectJson
        (mkHttpResponseMsg (\_ -> CharacterLoaded))
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
    List.map toUnstyled <|
      case model.page of
        Loading ->
          [ text "Loading..." ]
        Error msg -> 
          [ text msg ]
        CharacterSelectionPage data ->
          characterSelectionPage data model
        CharacterSheetPage data ->
          Sheet.view model.preparedSpells model.showOnlyPreparedSpells data
        EditCharacterPage options selectedLevel desc ->
          Edit.view model.focusedDropdownId options selectedLevel desc
  }
  

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
