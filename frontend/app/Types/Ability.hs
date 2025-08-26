module Types.Ability where

--------------------------------------------------------------------------------
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics

import Data.Aeson
import Data.Aeson.Types

import Util (dropPrefix)
--------------------------------------------------------------------------------

type Ability = Text
type AbilityTable = Map Ability AbilityTableEntry

data AbilityTableEntry = AbilityTableEntry
  { base        :: Int
  , total_bonus :: Int
  , score       :: Int
  , mod         :: Int
  , st          :: Int
  , st_prof     :: Bool
  }
  deriving (Generic, Show, Eq)
instance FromJSON AbilityTableEntry where

abilities :: [Text]
abilities = ["str", "dex", "con", "wis", "int", "cha"]

listFromAbilityTable :: (AbilityTableEntry -> Int) -> AbilityTable -> [Int]
listFromAbilityTable extract table =
  abilities <&> \abi -> fromJust $ fmap extract $ Map.lookup abi table

type Skill = Text
type SkillTable = Map Skill SkillTableEntry
data SkillTableEntry = SkillTableEntry { score :: Int, proficient :: Bool }
  deriving (Generic, Show, Eq)
instance FromJSON SkillTableEntry where

skillsPerAbility :: [(Ability, [Skill])]
skillsPerAbility =
  [ ( "str", [ "athletics" ] )
  , ( "dex", [ "acrobatics"
             , "sleight of hand"
             , "stealth"
             ] )
  , ( "con", [] )
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
