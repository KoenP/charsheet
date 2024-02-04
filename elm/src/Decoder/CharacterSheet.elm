module Decoder.CharacterSheet exposing (sheetDec)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D

import Types exposing (..)
import Util exposing (exactMatchDec)
import Decoder.AbilityTable exposing (abilityTableDec, skillTableDec)

sheetDec : Decoder CharacterSheet
sheetDec =
  D.succeed CharacterSheet
    |> D.andMap (D.field "name" D.string)
    |> D.andMap (D.field "summary" summaryDec)
    |> D.andMap (D.field "ac_formulas" (D.list acFormulaDec))
    |> D.andMap (D.field "hit_dice" (D.list hitDiceDec))
    |> D.andMap (D.field "ability_table" abilityTableDec)
    |> D.andMap (D.field "skill_table" skillTableDec)
    |> D.andMap (D.field "languages" (D.list D.string))
    |> D.andMap (D.field "weapons" (D.list D.string))
    |> D.andMap (D.field "armor" (D.list D.string))
    |> D.andMap (D.field "tools" (D.list D.string))
    |> D.andMap (D.field "notable_traits" notableTraitsDec)
    |> D.andMap (D.field "attacks" (D.list attackDec))
    |> D.andMap (D.field "spellcasting_sections" (D.list spellcastingSectionDec))
    |> D.andMap (D.field "spell_slots" (D.list D.int))
  
summaryDec : Decoder CharacterSummary
summaryDec =
  D.succeed CharacterSummary
    |> D.andMap (D.field "ac" D.int)
    |> D.andMap (D.field "class" D.string)
    |> D.andMap (D.field "hd" D.string)
    |> D.andMap (D.field "initiative" D.int)
    |> D.andMap (D.field "level" D.int)
    |> D.andMap (D.field "maxhp" D.int)
    |> D.andMap (D.field "pp" D.int)
    |> D.andMap (D.field "prof_bon" D.int)
    |> D.andMap (D.field "race" D.string)
    |> D.andMap (D.field "speed" D.int)

acFormulaDec : Decoder AcFormula
acFormulaDec =
  D.succeed AcFormula
    |> D.andMap (D.field "name" D.string)
    |> D.andMap (D.field "ac" D.int)
    |> D.andMap (D.field "shield" (D.nullable D.int))

hitDiceDec : Decoder HitDice
hitDiceDec =
  D.succeed HitDice
    |> D.andMap (D.field "n" D.int)
    |> D.andMap (D.field "d" D.int)

notableTraitsDec : Decoder (List NotableTraitCategory)
notableTraitsDec =
  D.list notableTraitDec

notableTraitDec : Decoder NotableTraitCategory
notableTraitDec =
  D.succeed NotableTraitCategory
    |> D.andMap (D.field "category" D.string)
    |> D.andMap (D.field "traits" (D.list traitDec))

traitDec : Decoder Trait
traitDec =
  D.succeed Trait
    |> D.andMap (D.field "name" D.string)
    |> D.andMap (D.field "desc" (D.nullable D.string))

attackDec : Decoder Attack
attackDec =
  D.succeed Attack
    |> D.andMap (D.field "name" D.string)
    |> D.andMap (D.field "range" D.string)
    |> D.andMap (D.field "to_hit_or_dc" D.string)
    |> D.andMap (D.field "damage" D.string)
    |> D.andMap (D.field "notes" D.string)

spellcastingSectionDec : Decoder SpellcastingSection
spellcastingSectionDec =
  D.succeed SpellcastingSection
    |> D.andMap (D.field "max_prepared_spells" (D.nullable D.int))
    |> D.andMap (D.field "origin" D.string)
    |> D.andMap (D.field "spell_attack_mod" D.int)
    |> D.andMap (D.field "spell_save_dc" D.int)
    |> D.andMap (D.field "spellcasting_ability" D.string)
    |> D.andMap (D.field "spellcasting_ability_mod" D.int)
    |> D.andMap (D.field "spells" (D.list spellDec))

spellDec : Decoder Spell
spellDec =
  D.succeed Spell
    |> D.andMap (D.field "aoe" (D.nullable D.string))
    |> D.andMap (D.field "casting_time" D.string)
    |> D.andMap (D.field "components" (D.list componentDec))
    |> D.andMap (D.field "concentration" Util.yesNoDec)
    |> D.andMap (D.field "dc" (D.nullable D.int))
    |> D.andMap (D.field "dc_abi" (D.nullable D.string))
    |> D.andMap (D.field "description" (D.list D.string))
    |> D.andMap (D.field "higher_level" (D.nullable D.string))
    |> D.andMap (D.field "duration" D.string)
    |> D.andMap (D.field "level" D.int)
    |> D.andMap (D.field "name" D.string)
    |> D.andMap (D.field "prepared" preparedDec)
    |> D.andMap (D.field "range" D.string)
    |> D.andMap (D.field "resources" (D.list D.string))
    |> D.andMap (D.field "ritual" Util.yesNoDec)
    |> D.andMap (D.field "school" D.string)
    |> D.andMap (D.field "shortdesc" (D.nullable (D.list D.string)))
    |> D.andMap (D.field "summary" D.string)
    |> D.andMap (D.field "to_hit" (D.nullable D.int))
    |> D.andMap (D.field "rolls" (D.nullable D.string))

preparedDec : Decoder Bool
preparedDec =
  D.oneOf
    [ exactMatchDec D.string "always" |> D.map (\_ -> True)
    , exactMatchDec D.string "maybe" |> D.map (\_ -> False)
    ]

componentDec : Decoder Component
componentDec =
  D.oneOf
    [ D.string |>
        D.andThen
          (\str -> case str of
                     "v" -> D.succeed V
                     "s" -> D.succeed S
                     _   -> D.fail "Expected either \"v\" or \"s\"")
    , D.field "args" (D.list D.string) |> D.map String.concat |> D.map M
    ]
