module Types where

--------------------------------------------------------------------------------
import Data.Char
import Data.Map (Map)
import GHC.Generics

import Data.Aeson
import Data.Aeson.Types
import Miso
import Miso.Effect
import Miso.String (MisoString)
import qualified Miso.String as MS

import SF (type (~>))
--------------------------------------------------------------------------------

data Action
  = NoOp
  | SendRequest -- TODO parameterize
  | GotResponse CharacterOptions -- TODO parameterize
  | Cmd Cmd
  deriving Show

data Page
  = LoadingPage
  | EditCharPage CharacterOptions
  deriving Show

data Cmd
  = Goto Page
  | SelectLevel Level
  deriving Show

data Model = Model (View Action) (Cmd ~> View Action)
instance Eq Model where
  _ == _ = False -- TODO is there a better way?

--------------------------------------------------------------------------------
-- EDIT CHARACTER PAGE
--------------------------------------------------------------------------------
type Level = Int

data CharacterOptions = CharacterOptions
  --{ ability_table      :: AbilityTable
  { options            :: Map Level [Option]
  -- , traits_and_bonuses :: Map Level [Effect]
  , char_level         :: Level
  } deriving (Generic, Show)

instance FromJSON CharacterOptions where

data Option = Option
  { charlevel               :: Level
  , id                      :: MisoString
  , display_id              :: MisoString
  , origin                  :: MisoString
  , origin_category         :: MisoString
  , display_origin_category :: MisoString
  , origin_category_index   :: Int
  , spec                    :: Spec
  , choice                  :: Maybe Choice
  } deriving (Generic, Show)
instance FromJSON Option where

type Unique = Bool

data ListSpecEntry = ListSpecEntry { desc :: MisoString, opt :: MisoString }
  deriving (Generic, Show)
instance FromJSON ListSpecEntry where

data Spec
  = ListSpec
    { list      :: [ListSpecEntry]
    }
  | OrSpec
    { leftname  :: MisoString
    , left      :: Spec
    , rightname :: MisoString
    , right     :: Spec
    }
  | FromSpec
    { unique    :: Unique
    , num       :: Maybe Int
    , subspec   :: Spec
    }
  deriving (Generic, Show)
instance FromJSON Spec where
  parseJSON = genericParseJSON $ Data.Aeson.Types.defaultOptions
    { constructorTagModifier = map toLower . reverse . drop 4 . reverse
    , sumEncoding = TaggedObject "spectype" ""
    }

data Dir = L | R
  deriving (Generic, Show)
instance FromJSON Dir

data Choice
  = OrChoice { side :: Dir, subchoice :: Choice }
  | ListChoice { subchoices :: [Choice] }
  | AtomicChoice { atomic_choice :: String }
  deriving (Generic, Show)
instance FromJSON Choice

-- type Unique = Bool
-- data SpecAndChoice
--   = ListSC
--     (Maybe String)          -- The user's choice (if relevant).
--     [(String, List String)] -- List of options, and option description (list of paragraphs).
--   | OrSC
--     (Maybe Dir)             -- The user's choice (if relevant).
--     (String, SpecAndChoice) -- Name and spec on the left side.
--     (String, SpecAndChoice) -- Name and spec on the right side.
--   | FromSC
--     Unique                  -- Whether this spec is a "from" or "unique_from" spec.
--     (Maybe Int)             -- Number of choices `n` the user gets to make,
--                             -- or `Nothing` if the user gets to make unlimited choices.
--     [SpecAndChoice]         -- `n` repetitions of the spec, each potentially with its own
--                             --   registered choice
