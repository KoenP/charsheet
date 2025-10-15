module Types where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Fix
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import GHC.Generics
import Data.Text (Text)
import Language.Javascript.JSaddle.Monad (MonadJSM)

import Data.Aeson
import Data.Aeson.Types
import Optics
import Reflex.Dom

import Types.Ability
import Util (dropPrefix)
--------------------------------------------------------------------------------

type ReactiveM t m =
  ( DomBuilder t m
  , DomBuilderSpace m ~ GhcjsDomSpace
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

neverUnless :: ReactiveM t m => Bool -> m (Event t a) -> m (Event t a)
neverUnless True  m = m
neverUnless False _ = pure never

--------------------------------------------------------------------------------
-- CHARACTER SHEET
--------------------------------------------------------------------------------
newtype CharId = CharId Text

data CharacterSheet = CharacterSheet
  { name :: Text
  , summary :: CharacterSummary
  , ac_formulas :: [AcFormula]
  , hit_dice :: [HitDice]
  , ability_table :: AbilityTable
  , skill_table :: SkillTable
  , languages :: [Text]
  , weapons :: [Text]
  , armor :: [Text]
  , tools :: [Tool]
  , resistances :: [Resistance]
  , notable_traits :: [NotableTraitCategory]
  , attacks :: [Attack]
  , pact_magic:: Maybe PactMagic
  , spellcasting_sections :: [SpellcastingSection]
  , spell_slots :: [Int]
  , resources :: [Resource]
  } deriving (Generic, Show)
instance FromJSON CharacterSheet where

data CharacterSummary = CharacterSummary
  { ac :: Int
  , classes :: Text
  , hd :: Text
  , initiative :: Int
  , level :: Int
  , maxhp :: Int
  , pp :: Int
  , prof_bon :: Int
  , race :: Text
  , speed :: [Speed]
  } deriving (Generic, Show)
instance FromJSON CharacterSummary where

data Speed = Speed { mode:: Text, speed :: Int } deriving (Generic, Show)
instance FromJSON Speed where

data Tool = Tool { tool:: Text, expertise :: Bool } deriving (Generic, Show)
instance FromJSON Tool where

data Resistance = Resistance { damage_type:: Text, resistance :: Text } deriving (Generic, Show)
instance FromJSON Resistance where

data NotableTraitCategory = NotableTraitCategory { category:: Text
                                                 , traits:: [Trait]
                                                 } deriving (Generic, Show)
instance FromJSON NotableTraitCategory where

data Trait = Trait
  { name        :: Text
  , desc        :: Maybe [Text]
  , ref         :: Maybe Text
  , seminotable :: Bool
  } deriving (Generic, Show)
instance FromJSON Trait where

data AcFormula = AcFormula
  { name :: Text
  , ac :: Int
  , shield :: Maybe Int
  } deriving (Generic, Show)
instance FromJSON AcFormula where

data HitDice = HitDice
  { n :: Int
  , d :: Int
  } deriving (Generic, Show)
instance FromJSON HitDice where

data Attack = Attack
  { name :: Text
  , range :: Text
  , to_hit_or_dc :: Text
  , damage :: Text
  , notes :: Text
  } deriving (Generic, Show)
instance FromJSON Attack where

data SpellcastingSection = SpellcastingSection
  { max_prepared_spells      :: Maybe Int
  , origin                   :: Origin
  , origin_shorthand         :: Text
  , spell_attack_mod         :: Int
  , spell_save_dc            :: Int
  , spellcasting_ability     :: Text
  , spellcasting_ability_mod :: Int
  , spells                   :: [Spell]
  } deriving (Generic, Show)
instance FromJSON SpellcastingSection where

type Origin = Text

data Spell = Spell
  { aoe           :: Maybe Text
  , casting_time  :: Text
  , components    :: [Component]
  , concentration :: Text -- TODO -> Bool
  , description   :: Text
  , higher_level  :: Maybe Text
  , duration      :: Text
  , level         :: Int
  , name          :: SpellName
  , prepared      :: Text -- AlwaysPrepared
  , range         :: Text
  , resources     :: [PrologTerm]
  , ref           :: Maybe Text
  , ritual        :: Ritual
  , school        :: Text
  , shortdesc     :: Maybe Text
  , summary       :: Text
  , to_hit        :: Maybe Int
  , rolls         :: Maybe Text
  , bonuses       :: [SpellBonus]
  } deriving (Generic, Show)
instance FromJSON Spell where

data Component = V | S | M Text
  deriving (Generic, Show)
instance FromJSON Component where
  parseJSON json = do
    prologTerm <- parseJSON @PrologTerm json
    case prologTerm of
      Atomic "v" -> return V
      Atomic "s" -> return S
      Compound "m" [Atomic material] -> return (M material)
      _ -> fail $ "not a valid spell component: " <> show prologTerm

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
  { origin :: Text
  , bonus :: Text
  } deriving (Generic, Show)
instance FromJSON SpellBonus where

data PactMagic = PactMagic
  { slot_count :: Int
  , slot_level :: Int
  } deriving (Generic, Show)
instance FromJSON PactMagic where

data Resource = Resource
  { name :: Text
  , number :: Int
  , restore :: Map Text Text
  } deriving (Generic, Show)
instance FromJSON Resource where

data PrologTerm = Compound Text [PrologTerm]
                | List [PrologTerm]
                | Atomic Text
  deriving (Generic, Show)
instance FromJSON PrologTerm where
  parseJSON (String text) = pure $ Atomic text
  parseJSON (Array list) = List <$> mapM parseJSON (Vector.toList list)
  parseJSON (Object o) = Compound <$> o .: "functor" <*> o .: "args"

--------------------------------------------------------------------------------
-- EDIT CHARACTER PAGE
--------------------------------------------------------------------------------
type Identifier = Text

data CharacterOptions = CharacterOptions
  { ability_table      :: AbilityTable
  , options            :: Map Level [Option]
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

--------------------------------------------------------------------------------
-- CARD CONFIG PAGE
--------------------------------------------------------------------------------
data CardConfig = CardConfig
  { showSpells               :: Bool
  , showTraits               :: Bool
  , onlyShowChanges          :: Bool
  , excludedCategories       :: Set Category
  , explicitlyExcludedTraits :: Set (Category, Text)
  , explicitlyExcludedSpells :: Set (Category, Text)

  , categoryColorSchemes     :: Map Category ColorScheme
  , traitColorSchemes        :: Map (Category, Text) ColorScheme
  , spellColorSchemes        :: Map (Category, Text) ColorScheme
  } deriving (Show, Generic)
instance FromJSON CardConfig
instance ToJSON CardConfig

defaultCardConfig :: CardConfig
defaultCardConfig = CardConfig
  { showSpells               = True
  , showTraits               = True
  , onlyShowChanges          = False
  , excludedCategories       = Set.empty
  , explicitlyExcludedTraits = Set.empty
  , explicitlyExcludedSpells = Set.empty

  , categoryColorSchemes     = Map.empty
  , traitColorSchemes        = Map.empty
  , spellColorSchemes        = Map.empty
  }


type Category = Text

-- | Defined as a class in CSS.
type ColorScheme = Text

maybeColorSchemeToClass :: Maybe ColorScheme -> [Text]
maybeColorSchemeToClass = map ("colorscheme-" <>) . maybeToList

categoryIncludedLens :: Category -> Lens' CardConfig Bool
categoryIncludedLens category = #excludedCategories % contains category % iso not not

traitIncludedLens :: Category -> Text -> Lens' CardConfig Bool
traitIncludedLens category trait = #explicitlyExcludedTraits % contains (category, trait) % iso not not

spellIncludedLens :: Category -> Text -> Lens' CardConfig Bool
spellIncludedLens category spell = #explicitlyExcludedSpells % contains (category, spell) % iso not not

categoryColorSchemeLens :: Category -> Lens' CardConfig (Maybe ColorScheme)
categoryColorSchemeLens category = #categoryColorSchemes % at category

traitCardColorScheme :: Category -> Text -> CardConfig -> Maybe ColorScheme
traitCardColorScheme category trait config = directScheme <|> indirectScheme
  where
    directScheme = config ^. #traitColorSchemes % at (category, trait)
    indirectScheme = config ^. #categoryColorSchemes % at category

spellCardColorScheme :: Category -> Text -> CardConfig -> Maybe ColorScheme
spellCardColorScheme category spell config = directScheme <|> indirectScheme
  where
    directScheme = config ^. #spellColorSchemes % at (category, spell)
    indirectScheme = config ^. #categoryColorSchemes % at category
