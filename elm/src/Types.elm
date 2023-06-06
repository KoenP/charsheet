module Types exposing (..)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Http
import Url exposing (Url)

----------------------------------------------------------------------
-- CHARACTER SHEET
type alias CharacterSheet =
  { name : String
  , summary : CharacterSummary
  , ability_table : AbilityTable
  , skill_table : SkillTable
  }

type alias CharacterSummary =
  { ac : Int
  , class : String
  , hd : String
  , initiative : Int
  , level : Int
  , maxhp : Int
  , pp : Int
  , prof_bon : Int
  , race : String
  , speed : Int
  }

-- TODO: should perhaps make this an enum, but not sure if it's worth
-- the hassle.
type alias Ability = String
type alias AbilityTable = Dict Ability AbilityTableEntry
type alias AbilityTableEntry = {score : Int, mod : Int, st : Int}
abilities : List String
abilities = ["str", "dex", "con", "wis", "int", "cha"]

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
            
----------------------------------------------------------------------
-- CHARACTER SELECTION PAGE
type alias CharacterSelectionPageData =
  { characters : List String
  , newCharacterName : String
  }

----------------------------------------------------------------------
-- MODEL
type alias Model =
  { url : Url
  , key : Nav.Key
  , page : Page
  }
type Page
  = Loading
  | Error String
  | CharacterSelectionPage CharacterSelectionPageData
  | CharacterSheetPage CharacterSheet

----------------------------------------------------------------------
-- MSG
type Msg
  = HttpResponse (Result Http.Error HttpResponseMsg)
  | SelectCharacter String
  | NewCharacterName String
  | CreateNewCharacter
  | UrlChanged Url
  | LinkClicked Browser.UrlRequest

type HttpResponseMsg
  = GotCharacterList (List String)
  | CharacterLoaded
  | GotCharacterSheet CharacterSheet

mkHttpResponseMsg : (a -> HttpResponseMsg) -> (Result Http.Error a -> Msg)
mkHttpResponseMsg f result =
  HttpResponse (Result.map f result)
    
