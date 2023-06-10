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
  , languages : List String
  , weapons : List String
  , armor : List String
  , tools : List String
  , notable_traits : List NotableTraitCategory
  , attacks : List Attack
  , spellcasting_sections : List SpellcastingSection
  , spell_slots : List Int
  }
{-
NotableTraits = [( Category, List Trait )]
Trait = [(Desc, Name)]
-}

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

type alias NotableTraitCategory = { category: String, traits: List Trait }
type alias Trait = { name: String, desc: Maybe String }

type alias Attack =
  { name : String
  , range : String
  , to_hit_or_dc : String
  , damage : String
  , notes : String
  }

type alias SpellcastingSection =
  { max_prepared_spells : Int
  , origin : String
  , spell_attack_mod : Int
  , spell_save_dc : Int
  , spellcasting_ability : String
  , spellcasting_ability_mod : Int
  , spells : List Spell
  }

type alias Spell =
  { casting_time : String
  , components : List Component
  , concentration : String
  , dc : Maybe Int
  , dc_abi : Maybe String
  , description : List String
  , duration : String
  , level : Int
  , name : String
  , prepared : Prepared
  , range : String
  , resources : List String
  , ritual : String
  , summary : String
  , to_hit : Maybe Int
  }
type Component = V | S | M String
type Prepared = Always | Maybe

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
    
