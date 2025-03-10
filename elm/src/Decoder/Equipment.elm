module Decoder.Equipment exposing (..)

--------------------------------------------------------------------------------
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D

import Types exposing (..)
--------------------------------------------------------------------------------

equipmentDec : Decoder Equipment
equipmentDec =
  D.list itemDec

itemDec : Decoder Item
itemDec =
  D.succeed Item
    |> D.andMap (D.field "name" D.string)
    |> D.andMap (D.field "inferred" D.bool)

-- gotEquipmentDec : Decoder Equipment
-- gotEquipmentDec =
--   D.succeed Equipment
--     |> D.andMap (D.field "weapons" (D.list weaponDec))
--     |> D.andMap (D.field "weapon_options" (D.list D.string))

-- weaponDec : Decoder Weapon
-- weaponDec =
--   D.succeed Weapon
--     |> D.andMap (D.field "base_weapon" D.string)
--     |> D.andMap (D.field "enchantment" D.int)
--     |> D.andMap (D.field "category" D.string)
--     |> D.andMap (D.field "range" D.string)
--     |> D.andMap (D.field "to_hit" D.string)
--     |> D.andMap (D.field "damage" D.string)
--     |> D.andMap (D.field "notes" D.string)
