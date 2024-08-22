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

import Elements
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
  [ ul [] (List.map (li [] << List.singleton << text) equipment)
  , input [ Attr.type_ "text"
          , Attr.placeholder "Item name"
          , Attr.value inputFieldVal
          , E.onInput AddItemInput
          ] []
  , button [ E.onClick (EquipItem inputFieldVal) ] [ text "Add item" ]
  ]
  ++
  viewError error

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
