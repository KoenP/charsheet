module Types where

--------------------------------------------------------------------------------
import Control.Monad.IO.Class
import Control.Monad.Fix
import Data.Char
import Data.Map (Map)
import GHC.Generics
import Data.Text (Text)
import Language.Javascript.JSaddle.Monad (MonadJSM)

import Data.Aeson
import Data.Aeson.Types
import Reflex.Dom

import Types.Ability
import Util (dropPrefix)
--------------------------------------------------------------------------------

type ReactiveM t m =
  ( DomBuilder t m
  , MonadHold t m
  , PostBuild t m
  , TriggerEvent t m
  , MonadFix m
  , Adjustable t m
  , NotReady t m
  )

type ReactiveIOM t m =
  ( ReactiveM t m
  , MonadIO m
  , MonadJSM (Performable m)
  , PerformEvent t m
  )

--------------------------------------------------------------------------------
-- CHARACTER SHEET
--------------------------------------------------------------------------------
newtype CharId = CharId Text

data CharacterSheet = CharacterSheet
  { cs_name :: Text
  , cs_summary :: CharacterSummary
  , cs_ac_formulas :: [AcFormula]
  , cs_hit_dice :: [HitDice]
  , cs_ability_table :: AbilityTable
  , cs_skill_table :: SkillTable
  , cs_languages :: [Text]
  , cs_weapons :: [Text]
  , cs_armor :: [Text]
  , cs_tools :: [Tool]
  , cs_resistances :: [Resistance]
  , cs_notable_traits :: [NotableTraitCategory]
  , cs_attacks :: [Attack]
  , cs_pact_magic:: Maybe PactMagic
  , cs_spellcasting_sections :: [SpellcastingSection]
  , cs_spell_slots :: [Int]
  , cs_resources :: [Resource]
  } deriving (Generic, Show)
instance FromJSON CharacterSheet where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = dropPrefix "cs_"})

data CharacterSummary = CharacterSummary
  { csm_ac :: Int
  , csm_classes :: Text
  , csm_hd :: Text
  , csm_initiative :: Int
  , csm_level :: Int
  , csm_maxhp :: Int
  , csm_pp :: Int
  , csm_prof_bon :: Int
  , csm_race :: Text
  , csm_speed :: [Speed]
  } deriving (Generic, Show)
instance FromJSON CharacterSummary where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = dropPrefix "csm_"})

data Speed = Speed { speed_mode:: Text, speed_speed :: Int } deriving (Generic, Show)
instance FromJSON Speed where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = dropPrefix "speed_"})

data Tool = Tool { tool_tool:: Text, tool_expertise :: Bool } deriving (Generic, Show)
instance FromJSON Tool where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = dropPrefix "tool_"})

data Resistance = Resistance { res_damage_type:: Text, res_resistance :: Text } deriving (Generic, Show)
instance FromJSON Resistance where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = dropPrefix "res_"})

data NotableTraitCategory = NotableTraitCategory { ntc_category:: Text
                                                 , ntc_traits:: [Trait]
                                                 } deriving (Generic, Show)
instance FromJSON NotableTraitCategory where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = dropPrefix "ntc_"})

data Trait = Trait
  { trait_name        :: Text
  , trait_desc        :: Maybe [Text]
  , trait_ref         :: Maybe Text
  , trait_seminotable :: Bool
  } deriving (Generic, Show)
instance FromJSON Trait where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = dropPrefix "trait_"})

data AcFormula = AcFormula
  { ac_name :: Text
  , ac_ac :: Int
  , ac_shield :: Maybe Int
  } deriving (Generic, Show)
instance FromJSON AcFormula where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = dropPrefix "ac_"})

data HitDice = HitDice
  { hd_n :: Int
  , hd_d :: Int
  } deriving (Generic, Show)
instance FromJSON HitDice where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = dropPrefix "hd_"})

data Attack = Attack
  { att_name :: Text
  , att_range :: Text
  , att_to_hit_or_dc :: Text
  , att_damage :: Text
  , att_notes :: Text
  } deriving (Generic, Show)
instance FromJSON Attack where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = dropPrefix "att_"})

data SpellcastingSection = SpellcastingSection
  { ss_max_prepared_spells      :: Maybe Int
  , ss_origin                   :: Origin
  , ss_origin_shorthand         :: Text
  , ss_spell_attack_mod         :: Int
  , ss_spell_save_dc            :: Int
  , ss_spellcasting_ability     :: Text
  , ss_spellcasting_ability_mod :: Int
  , ss_spells                   :: [Spell]
  } deriving (Generic, Show)
instance FromJSON SpellcastingSection where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = dropPrefix "ss_"})

type Origin = Text

data Spell = Spell
  { spell_aoe           :: Maybe Text
  , spell_casting_time  :: Text
  , spell_components    :: Text -- TODO
  , spell_concentration :: Text -- TODO -> Bool
  , spell_description   :: Text
  , spell_higher_level  :: Maybe Text
  , spell_duration      :: Text
  , spell_level         :: Int
  , spell_name          :: SpellName
  , spell_prepared      :: Text -- AlwaysPrepared
  , spell_range         :: Text
  -- , spell_resources     :: [PrologTerm] TODO
  , spell_ref           :: Maybe Text
  , spell_ritual        :: Ritual
  , spell_school        :: Text
  , spell_shortdesc     :: Maybe Text
  , spell_summary       :: Text
  , spell_to_hit        :: Maybe Int
  , spell_rolls         :: Maybe Text
  -- , spell_bonuses       :: [SpellBonus] TODO
  } deriving (Generic, Show)
instance FromJSON Spell where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = dropPrefix "spell_"})

data Component = V | S | M Text
  deriving (Generic, Show)
instance FromJSON Component where
  parseJSON = genericParseJSON (defaultOptions {constructorTagModifier = map toLower})

type AlwaysPrepared = Bool
type SpellName = Text
type Level = Int

data Ritual = Ritual | NotRitual | OnlyRitual
  deriving (Generic, Show)
instance FromJSON Ritual where
  parseJSON = genericParseJSON
    $ defaultOptions {constructorTagModifier = \case
                         "Ritual"     -> "yes"
                         "NotRitual"  -> "no"
                         "OnlyRitual" -> "only"
                         _            -> error "Ritual parseJSON"
                     }

data SpellBonus = SpellBonus
  { sbon_origin :: Text
  , sbon_bonus :: Text
  } deriving (Generic, Show)
instance FromJSON SpellBonus where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = dropPrefix "sbon_"})

data PactMagic = PactMagic
  { pm_slot_count :: Int
  , pm_slot_level :: Int
  } deriving (Generic, Show)
instance FromJSON PactMagic where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = dropPrefix "pm_"})

data Resource = Resource
  { rsc_name :: Text
  , rsc_number :: Int
  , rsc_restore :: Map Text Text
  } deriving (Generic, Show)
instance FromJSON Resource where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = dropPrefix "rsc_"})

data PrologTerm = Compound Text [PrologTerm]
                | List [PrologTerm]
                | Atomic Text
  deriving (Generic, Show)
instance FromJSON PrologTerm -- TODO this is wrong

--------------------------------------------------------------------------------
-- EDIT CHARACTER PAGE
--------------------------------------------------------------------------------
type Identifier = Text

data CharacterOptions = CharacterOptions
  --{ ability_table      :: AbilityTable
  { options            :: Map Level [Option]
  -- , traits_and_bonuses :: Map Level [Effect]
  , char_level         :: Level
  } deriving (Generic, Show, Eq)

instance FromJSON CharacterOptions where

data Option = Option
  { charlevel               :: Level
  , id                      :: Text
  , display_id              :: Text
  , origin                  :: Text
  , origin_category         :: Text
  , display_origin_category :: Text
  , origin_category_index   :: Int
  , spec                    :: Spec
  , choice                  :: Maybe Choice
  } deriving (Generic, Show, Eq)
instance FromJSON Option where

data OptionId = OptionId { oiOrigin :: Text, oiId :: Text }
  deriving Show

type Unique = Bool

data ListSpecEntry = ListSpecEntry { desc :: [Text], opt :: Text }
  deriving (Generic, Show, Eq)
instance FromJSON ListSpecEntry where

data Spec
  = ListSpec
    { list      :: [ListSpecEntry]
    }
  | OrSpec
    { leftname  :: Text
    , left      :: Spec
    , rightname :: Text
    , right     :: Spec
    }
  | FromSpec
    { unique    :: Unique
    , num       :: Maybe Int
    , subspec   :: Spec
    }
  deriving (Generic, Show, Eq)
instance FromJSON Spec where
  parseJSON = genericParseJSON $ Data.Aeson.Types.defaultOptions
    { constructorTagModifier = map toLower . reverse . drop 4 . reverse
    , sumEncoding = TaggedObject "spectype" ""
    }

data Dir = L | R
  deriving (Generic, Show, Eq)
instance FromJSON Dir where
  parseJSON = genericParseJSON $ Data.Aeson.Types.defaultOptions
    { constructorTagModifier = map toLower }

data Choice
  = OrChoice { side :: Dir, subchoice :: Choice }
  | ListChoice { subchoices :: [Choice] }
  | AtomicChoice { atomic_choice :: Text }
  deriving (Generic, Show, Eq)
instance FromJSON Choice where
  parseJSON = genericParseJSON $ Data.Aeson.Types.defaultOptions
    { constructorTagModifier = camelToSnakeCase
    }

-- TODO delete
-- getOrChoice :: Choice -> Maybe (Dir, Choice)
-- getOrChoice OrChoice{side, subchoice} = Just (side, subchoice)
-- getOrChoice _ = Nothing
--
-- getListChoice :: Choice -> Maybe [Choice]
-- getListChoice ListChoice{subchoices} = Just subchoices
-- getListChoice _ = Nothing
--
-- getAtomicChoice :: Choice -> Maybe Text
-- getAtomicChoice AtomicChoice{atomic_choice} = Just atomic_choice
-- getAtomicChoice _ = Nothing


camelToSnakeCase :: String -> String
camelToSnakeCase []     = []
camelToSnakeCase (c:cs) = toLower c
  : concatMap (\c' -> if isUpper c' then ['_', toLower c'] else [c']) cs


-- | We use distinct "choice" datatypes for receiving and submitting choices.
--   Internally, the server does not store the structure of a choice (for
--   example whether an "or" choice is left or right), but the frontend needs
--   this information to determine for example which radio button to have
--   selected. So the server retroactively determines this structure for the
--   benefit of the frontend. However it does not need to get this information
--   back, so the frontend can send an "unstructured" choice back.
data SubmitChoice = SubmitListChoice [Text] | SubmitSingletonChoice Text | RetractChoice
  deriving Show
