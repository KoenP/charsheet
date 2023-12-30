module Types.Ability exposing (..)

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Maybe exposing (Maybe)

-- TODO: should perhaps make this an enum, but not sure if it's worth
-- the hassle.
type alias Ability = String
type alias AbilityTable = Dict Ability AbilityTableEntry
type alias AbilityTableEntry = {base : Int, totalBonus : Int, score : Int, mod : Int, st : Int}
abilities : List String
abilities = ["str", "dex", "con", "wis", "int", "cha"]

listFromAbilityTable : (AbilityTableEntry -> Int) -> AbilityTable -> List Int
listFromAbilityTable extract table =
  abilities
    |> List.map
       (\abi ->
          Dict.get abi table
          |> Maybe.map extract
          |> Maybe.withDefault -9999
       )

type alias Skill = String
type alias SkillTable = Dict Skill Int

skillsPerAbility : List ( Ability , List Skill )
skillsPerAbility =
  [ ( "str", [ "athletics" ] )
  , ( "dex", [ "acrobatics"
             , "sleight of hand"
             , "stealth"
             ] ) 
  , ( "wis", [ "animal handling"
             , "insight"
             , "medicine"
             , "perception"
             , "survival"
             ] ) 
  , ( "int", [ "arcana"
             , "history"
             , "investigation"
             , "nature"
             , "religion"
             ] ) 
  , ( "cha", [ "deception"
             , "intimidation"
             , "performance"
             , "persuasion"
             ] ) 
  ]

