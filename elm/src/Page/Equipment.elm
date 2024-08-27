module Page.Equipment exposing (..)

--------------------------------------------------------------------------------
import Css exposing (Style)
import Debug
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as E
import Http exposing (Expect)
import Json.Decode as D exposing (Decoder)
import List

import Elements exposing (..)
import Request exposing (characterRequestUrl)
import Types exposing (..)
import Util exposing (simple)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- LOAD
--------------------------------------------------------------------------------
load : CharId -> Cmd Msg
load charId =
  Http.get
    { url = characterRequestUrl charId ["equipment"] []
    , expect = expectGotEquipment
    }

expectGotEquipment : Expect Msg
expectGotEquipment = Http.expectJson
                     (mkHttpResponseMsg GotEquipment)
                     (D.oneOf [ D.list D.string |> D.map Ok
                              , D.string |> D.map Err
                              ])

update : Msg -> Model -> CharId -> EquipmentPageData -> (Model, Cmd Msg)
update msg model charId oldData =
  case msg of
    AddItemInput newInputFieldVal -> ( applyPageData model charId
                                         { oldData
                                           | inputFieldVal = newInputFieldVal
                                           , error = Nothing
                                         }
                               , Cmd.none
                               )
    EquipItem item -> ( model
                      , Http.post
                          { url = characterRequestUrl charId ["equip_item"] [("item", item)]
                          , body = Http.emptyBody
                          , expect = expectGotEquipment
                          }
                      )
    UnequipItem item -> ( model
                        , Http.post
                            { url = characterRequestUrl charId ["unequip_item"] [("item", item)]
                            , body = Http.emptyBody
                            , expect = expectGotEquipment
                            }
                        )

    HttpResponse (Ok (GotEquipment (Ok newEquipment))) ->
      ( applyPageData model charId
          { oldData
            | inputFieldVal = ""
            , equipment = newEquipment
            , error = Nothing
          }
      , Cmd.none
      )

    HttpResponse (Ok (GotEquipment (Err errmsg))) ->
      ( applyPageData model charId
          { oldData
            | inputFieldVal = ""
            , error = Just errmsg
          }
      , Cmd.none
      )


      
    _ -> ( { model | page = Error ("Equipment.update called with unsupported message " ++ Debug.toString msg) }
         , Cmd.none
         )

view : CharId -> EquipmentPageData -> List (Html Msg)
view charId { equipment, inputFieldVal, error } =
  [ div []
      [ viewNavButtons [ viewEditCharacterButton charId
                       , viewGotoSheetButton charId
                       , viewSelectCharacterButton
                       ]
      ]
  , table []
      (List.map
         (\item -> tr [] [ td [] [button [E.onClick (UnequipItem item)] [text "x"]]
                         , simple td item
                         ])
         equipment)
  , input [ Attr.type_ "text"
          , Attr.placeholder "Item name"
          , Attr.value inputFieldVal
          , E.onInput AddItemInput
          , onReturnPressed (EquipItem inputFieldVal)
          ] []
  , button
      [ E.onClick (EquipItem inputFieldVal)
      ]
      [ text "Add item" ]
  ]
  ++
  viewError error

onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
  E.on "keydown" (D.map tagger E.keyCode)

onReturnPressed : Msg -> Attribute Msg
onReturnPressed msg =
  onKeyDown <| \keyCode -> case keyCode of
                             13 -> msg
                             _  -> Null

viewError : Maybe String -> List (Html Msg)
viewError error =
  case error of
    Nothing -> []
    Just msg -> [ div [ Attr.css [Css.color (Css.hex "ff0000") ] ] [ text msg ] ]




--------------------------------------------------------------------------------
-- UTIL
--------------------------------------------------------------------------------

applyPageData : Model -> CharId -> EquipmentPageData -> Model
applyPageData model charId data =
  { model | page = EquipmentPage charId data }
