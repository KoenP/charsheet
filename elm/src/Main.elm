module Main exposing (..)

----------------------------------------------------------------------
import Browser
import Browser.Navigation as Nav
import Browser.Events
import Css as Css
import Css.Global as Css
import Url exposing (Url)
import Url.Parser exposing (Parser, (</>))
import Url.Parser as Parser
import Html as Unstyled
import Html.Attributes
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Attributes exposing (style, placeholder, type_)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder, field, list, string, succeed)
import Json.Decode.Extra as D
import Platform.Cmd exposing (none)
import List
import Maybe
import Debug exposing (log, toString)
import Time exposing (Posix)

import Constants exposing (..)
import Page.PrintableCharSheet as PSheet
import Page.EditCharacter as Edit
import Page.CardsPage as Cards
import Page.CardSelectPage as CardSelectPage
import Page.Equipment as Equipment
import Request exposing (requestUrl)
import Types exposing (..)
----------------------------------------------------------------------

-- MAIN
main =
  Browser.element
    { init = init
    , view = view
    , update = updateOrTick
    , subscriptions = subscriptions
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
init : String -> (Model, Cmd Msg)
init charId =
  ( { preparedSpells = Dict.empty
    , showOnlyPreparedSpells = False
    , cardConfig = emptyCardConfig
    , page = Loading
    , focusedDropdownId = Nothing
    , lastTick = Time.millisToPosix 0
    , charId = CharId charId
    , sheetCache = Nothing
    , prevSheetCache = Nothing
    }
  , Edit.load (CharId charId)
  -- , Cards.load (CharId charId)
  )

parseCharId : Parser (CharId -> a) a
parseCharId = Parser.map CharId Parser.string

-- UPDATE
verbose = False

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

    GotoEditCharacter ->
      ( { model | page = Loading }, Edit.load model.charId )

    GotoSelectCharacterPage ->
      ( model, Nav.load "/select" )

    GotoCardsPage options ->
      case model.sheetCache of
        Just cachedSheet -> applyPage model (CardsPage options cachedSheet, Cmd.none)
        Nothing          -> applyPage model (Loading, Cards.load model.charId)

    GotoCardSelectPage sheet ->
      case model.prevSheetCache of
        Just cachedPrevSheet ->
          applyPage model ( CardSelectPage { curSheet  = sheet
                                           , prevSheet = cachedPrevSheet
                                           }
                          , Cmd.none
                          )
        Nothing              -> applyPage model (Loading, CardSelectPage.load model.charId sheet)

    GotoSheet ->
      case model.sheetCache of
        Just cachedSheet -> applyPage model (PrintableCharSheetPage cachedSheet, Cmd.none)
        Nothing          -> applyPage model (Loading, PSheet.load model.charId)

    GotoEquipmentPage ->
      applyPage model (Loading, Equipment.load model.charId)

    Choice origin id choice ->
      ( invalidateSheetCache { model | focusedDropdownId = Nothing }
      , registerChoice model.charId origin id choice
      )

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
        PrintableCharSheetPage sheet ->
          PSheet.update msg model
        EditCharacterPage data ->
          Edit.update msg model data
        CardsPage options data ->
          Cards.update msg model data
        CardSelectPage data ->
          CardSelectPage.update msg model
        EquipmentPage data ->
          Equipment.update msg model data
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

handleHttpResponseMsg : HttpResponseMsg -> Model -> (Model, Cmd Msg)
handleHttpResponseMsg msg model =
  case msg of
      GotCharacterList chars -> 
        ( model , none ) -- TODO delete
      NewCharacterCreated charId ->
        ( { model | page = Loading }
        , Edit.load charId
        -- , Nav.pushUrl model.key ("/character/" ++ id ++ "/edit")
        )
      GotPrintableCharSheet sheet ->
        ( { model
            | page = PrintableCharSheetPage sheet
            , sheetCache = Just sheet
          }
        , none
        )

      GotPrevLevelSheet curSheet prevLevelSheet ->
        ( { model
            | page = CardSelectPage { curSheet = curSheet , prevSheet = prevLevelSheet }
            , prevSheetCache = Just prevLevelSheet
          }
        , none
        )

      GotCards sheet ->
        ( { model
            | page = CardsPage { showSpells = AllSpells } sheet
            , sheetCache = Just sheet
          }
        , none
        )

      GotCharacterOptions { ability_table, options, traits_and_bonuses, char_level } ->
        -- TODO duplicate with EditCharacter Page?
        ( { model
            | page = EditCharacterPage
                     { abilityTable = ability_table
                     , optionsPerLevel = options
                     , traitsAndBonusesPerLevel = traits_and_bonuses 
                     , charLevel = char_level
                     , selectedLevel = char_level
                     , desc = Nothing
                     , setAbilitiesOnNextTick = Dict.empty
                     }
          }
        , none
        )
      GotEquipment (Ok equipment) ->
        ( { model | page = EquipmentPage { equipment = equipment
                                         , inputFieldVal = ""
                                         , error = Nothing
                                         } }
        , none
        )
      -- ChoiceRegistered ->
      --   (model, Edit.load)
      _ ->
        errorPage
          model
          ("handleHttpResponseMsg called with unsupported message: "
          ++ Debug.toString msg)

newCharacter : String -> Cmd Msg
newCharacter charName =
  Http.post
    { url = requestUrl ["new_character"] [("name", charName)]
    , body = Http.emptyBody
    , expect =
        Http.expectJson
        (mkHttpResponseMsg (NewCharacterCreated << CharId))
        string
    }

registerChoice : CharId -> String -> String -> Choice -> Cmd Msg
registerChoice (CharId charId) origin id choice = let _ = Debug.log "registerChoice" choice in 
  Http.post
    { url = requestUrl
        ["character", charId, "choice"]
        [ ("source", origin)
        , ("id", id)
        , ("choice", choiceToString choice)
        ]
    , body = Http.emptyBody
    , expect =
        Http.expectJson (mkHttpResponseMsg (\_ -> Update)) (succeed ())
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
view : Model -> Unstyled.Html Msg
view model =
  Unstyled.div [] <|
    List.map toUnstyled
      [ viewTabs model.page
      , div [Attr.class "below-tabs"] <|
          case model.page of
            Loading ->
              [ text "Loading..." ]
            Error msg -> 
              [ text msg ]
            PrintableCharSheetPage data ->
              PSheet.view data
            EditCharacterPage data ->
              Edit.view model.focusedDropdownId data
            CardsPage options data ->
              Cards.view model.cardConfig model.prevSheetCache data
            EquipmentPage data ->
              Equipment.view data
            CardSelectPage data ->
              CardSelectPage.view model.cardConfig model.focusedDropdownId data
      ]
      
type alias TabCfg = { name : String
                    , iconPath : String
                    , msg : Msg
                    , selected : Bool
                    }

viewTabs : Page -> Html Msg
viewTabs page =
  div
    [ Attr.class "dont-print"]
    [ div
        [ Attr.class "tab-bar" ]
        [ div []
            (List.map viewTab
               [ { name = "Edit"
                 , iconPath = "edit.png"
                 , msg = GotoEditCharacter
                 , selected = case page of
                                EditCharacterPage _ -> True
                                _ -> False }
               , { name =" Equipment" 
                 , iconPath = "equipment.png"
                 , msg = GotoEquipmentPage
                 , selected = case page of
                                EquipmentPage _ -> True
                                _ -> False
                 }
               , { name = "Sheet"
                 , iconPath = "sheet.png"
                 , msg = GotoSheet
                 , selected = case page of
                                PrintableCharSheetPage _ -> True
                                _ -> False
                 }
               , { name = "Cards"
                 , iconPath = "cards.png"
                 , msg = GotoCardsPage { showSpells = AllSpells }
                 , selected = case page of
                                CardsPage _ _ -> True
                                _ -> False
                 }
               ])
        , div []
          [ viewTab
              { name = "Back to character selection"
              , iconPath = "close.png"
              , msg = GotoSelectCharacterPage
              , selected = False
              }
          ]
        ]
    ]
    
    

viewTab : TabCfg -> Html Msg
viewTab { name, iconPath, msg, selected } =
  button
    [ onClick msg
    , Attr.css [Css.color (Css.hex <| if selected then "#ffffff" else "#cccccc")]
    , Attr.class "tab"
    ]
    [ img [ Attr.src ("/static/icons/" ++ iconPath)
          , Attr.class (if selected then "full-invert" else "partial-invert")
          ]
        []
    , text name
    ]

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
  

-- Shared
textSingleton : String -> List (Html msg)
textSingleton = List.singleton << text

linkButtonStyle : List (Attribute msg)
linkButtonStyle =
  [ style "background" "none"
  , style "border" "none"
  , style "padding" "0"
  , style "font-family" "arial, sans-serif"
  , style "text-decoration" "underline"
  , style "cursor" "pointer"
  ]
