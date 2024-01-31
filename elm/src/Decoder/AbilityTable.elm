module Decoder.AbilityTable exposing (abilityTableDec, skillTableDec)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D

import Types.Ability exposing (..)

abilityTableDec : Decoder AbilityTable
abilityTableDec =
  D.dict (D.succeed AbilityTableEntry
            |> D.andMap (D.field "base" D.int)
            |> D.andMap (D.field "total_bonus" D.int)
            |> D.andMap (D.field "score" D.int)
            |> D.andMap (D.field "mod" D.int)
            |> D.andMap (D.field "st" D.int)
            |> D.andMap (D.field "stProf" D.bool))

skillTableDec : Decoder SkillTable
skillTableDec =
  D.dict (D.succeed SkillTableEntry
            |> D.andMap (D.field "score" D.int)
            |> D.andMap (D.field "proficient" D.bool))
