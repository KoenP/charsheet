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
import String

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

update : Msg -> Model -> EquipmentPageData -> (Model, Cmd Msg)
update msg model oldData =
  case msg of
    AddItemInput newInputFieldVal -> ( applyPageData model
                                         { oldData
                                           | inputFieldVal = newInputFieldVal
                                           , error = Nothing
                                         }
                               , Cmd.none
                               )
    EquipItem item -> ( invalidateAllSheetCaches model
                      , Http.post
                          { url = characterRequestUrl model.charId ["equip_item"] [("item", item)]
                          , body = Http.emptyBody
                          , expect = expectGotEquipment
                          }
                      )
    UnequipItem item -> ( invalidateAllSheetCaches model
                        , Http.post
                            { url = characterRequestUrl model.charId ["unequip_item"] [("item", item)]
                            , body = Http.emptyBody
                            , expect = expectGotEquipment
                            }
                        )

    HttpResponse (Ok (GotEquipment (Ok newEquipment))) ->
      ( applyPageData model
          { oldData
            | inputFieldVal = ""
            , equipment = newEquipment
            , error = Nothing
          }
      , Cmd.none
      )

    HttpResponse (Ok (GotEquipment (Err errmsg))) ->
      ( applyPageData model
          { oldData
            | inputFieldVal = ""
            , error = Just errmsg
          }
      , Cmd.none
      )


      
    _ -> ( { model | page = Error ("Equipment.update called with unsupported message " ++ Debug.toString msg) }
         , Cmd.none
         )

view : EquipmentPageData -> List (Html Msg)
view { equipment, inputFieldVal, error } =
  [ p [] [ text "This page is a placeholder. You can only add armor and weapons by typing the exact name. If the name of an item consists of more than one word, surround it with single quotes ('). You can add enchantment by adding for example \"+1\"." ]
  , p [] [ text "The equipment page is not intended as a full inventory; only those items that affect your character sheet (such as weapons and armor) are listed here." ]
  , p [] [ text "For example, type ",  b [] [text "'light crossbow' + 1" ]]
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
          , onReturnPressed (if String.isEmpty inputFieldVal then Null else EquipItem inputFieldVal)
          ] []
  , button
      [ E.onClick (if String.isEmpty inputFieldVal then Null else EquipItem inputFieldVal)
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

applyPageData : Model -> EquipmentPageData -> Model
applyPageData model data =
  { model | page = EquipmentPage data }
