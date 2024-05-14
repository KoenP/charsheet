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
      UnequipWeapon { base_weapon, enchantment } ->
        ( model
            -- { oldEquipment
            -- | weapons = List.filter
            --     (\w -> ( w.base_weapon, w.enchantment ) /= ( base_weapon, enchantment ))
            --     oldEquipment.weapons
            -- }
        , Http.get
            { url = requestUrl "unequip_weapon" [ ( "base_weapon", base_weapon )
                                                , ( "enchantment", String.fromInt enchantment )
                                                ]
            , expect = expectGotEquipment
            }
        )
      EquipWeapon w ->
        ( model
        , Http.get
            { url = requestUrl "equip_weapon" [ ("weapon", w) ]
            , expect = expectGotEquipment
            }
        )
      HttpResponse (Ok (GotEquipment newEquipment)) ->
        ( applyPageData model newEquipment
        , Cmd.none
        )
      _ -> ( model, Cmd.none )
        
view : Equipment -> List (Html Msg)
view { weapons, weapon_options } =
  [ Elements.viewNavButtons
      [ Elements.viewGotoSheetButton
      , Elements.viewEditCharacterButton
      , Elements.viewSelectCharacterButton
      ]
  , simple h1 "Equipment"
  , simple h2 "Weapons"
  , viewWeaponsTable weapons
  , p []
      [ select [] <|
          option [ Attr.disabled True, Attr.selected True ] [ text "-- Add a weapon --" ]
          ::
          List.map
            (\s -> option [ E.onClick (EquipWeapon s) ] [ text s ])
            weapon_options
      ]
  ]

viewWeaponsTable : List Weapon -> Html Msg
viewWeaponsTable weapons =
  table
    [ Attr.class "attacks" ]
    ( ( tr []
          <| List.map (simple th)
             [ "", "weapon", "enchantment", "category", "to hit", "damage", "range", "notes" ] )
      ::
      List.map viewWeapon weapons
    )

viewWeapon : Weapon -> Html Msg
viewWeapon { base_weapon, enchantment, category, to_hit, damage, range, notes, is_variant } =
  let indentIfVariant str = if is_variant then "↳ " ++ str else str
      omitIfVariant html = if is_variant then [] else html
      weapon = base_weapon
               ++ if enchantment > 0
                  then "+" ++ String.fromInt enchantment
                  else ""
  in 
    tr []
      [ td [] (omitIfVariant
                  [ Elements.viewNavButton
                      (UnequipWeapon { base_weapon = base_weapon
                                     , enchantment = enchantment
                                     })
                      "close.png"
                      "Unequip weapon"
                  ])
      , simple td (indentIfVariant weapon)
      , "", category, to_hit, damage, range, notes ]

--------------------------------------------------------------------------------
-- UTIL
--------------------------------------------------------------------------------

applyPageData : Model -> Equipment -> Model
applyPageData model data =
  { model | page = EquipmentPage data }
