module Page.Equipment exposing (..)

--------------------------------------------------------------------------------
import Debug
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as E
import Http exposing (Expect)
import List

import Elements
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
    , expect = expectGotEquipment
    }

expectGotEquipment : Expect Msg
expectGotEquipment = Http.expectJson (mkHttpResponseMsg GotEquipment) gotEquipmentDec

update : Msg -> Model -> Equipment -> (Model, Cmd Msg)
update msg model oldEquipment =
  case msg of
      UnequipWeapon weapon ->
        ( applyPageData model
            { oldEquipment
            | weapons = List.filter (\w -> w.weapon /= weapon) oldEquipment.weapons
            }
        , Http.get
            { url = requestUrl "unequip_weapon" [ ( "weapon", weapon ) ]
            , expect = expectGotEquipment
            }
        )
      _ -> ( model, Cmd.none )
        
view : Equipment -> List (Html Msg)
view { weapons } =
  [ simple h1 "Equipment"
  , simple h2 "Weapons"
  , viewWeaponsTable weapons
  , p [] [ button [] [ text "Add weapon" ] ]
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
viewWeapon { weapon, category, to_hit, damage, range, notes, is_variant } =
  let indentIfVariant str = if is_variant then "↳ " ++ str else str
      omitIfVariant html = if is_variant then [] else [ html ]
  in 
    tr []
      <| td [] (omitIfVariant
                  <| Elements.viewNavButton
                     (UnequipWeapon weapon)
                     "close.png"
                     "Unequip weapon")
      :: List.map (simple td)
         [ indentIfVariant weapon, category, to_hit, damage, range, notes ]

--------------------------------------------------------------------------------
-- UTIL
--------------------------------------------------------------------------------

applyPageData : Model -> Equipment -> Model
applyPageData model data =
  { model | page = EquipmentPage data }
