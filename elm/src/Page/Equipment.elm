module Page.Equipment exposing (..)

--------------------------------------------------------------------------------
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as E
import Http
import List

import Decoder.Equipment exposing (gotEquipmentDec)
import Request exposing (requestUrl)
import Types exposing (..)
import Util exposing (simple)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- LOAD
--------------------------------------------------------------------------------
load : Cmd Msg
load =
  Http.get
    { url = requestUrl "equipment" []
    , expect = Http.expectJson
               (mkHttpResponseMsg GotEquipment)
               gotEquipmentDec
    }

update : Msg -> Model -> Equipment -> (Model, Cmd Msg)
update msg model _ = (model, Cmd.none)

view : Equipment -> List (Html Msg)
view { weapons } =
  [ simple h1 "Equipment"
  , simple h2 "Weapons"
  , viewWeaponsTable weapons
  ]

viewWeaponsTable : List Weapon -> Html Msg
viewWeaponsTable weapons =
  table
    [ Attr.class "attacks" ]
    ( ( tr []
          <| List.map (simple th)
             [ "", "weapon", "category", "to hit", "damage", "range", "notes" ] )
      ::
      List.map viewWeapon weapons
    )

viewWeapon : Weapon -> Html Msg
viewWeapon { weapon , category , range , to_hit , damage , notes } =
  tr []
    <| List.map (simple td)
       [ "", weapon , category , range , to_hit , damage , notes ]
