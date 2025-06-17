module Types where

--------------------------------------------------------------------------------
import Data.Char
import Data.Map (Map)
import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Aeson
import Data.Aeson.Types

import SF (type (~>))
--------------------------------------------------------------------------------

type Origin = Text
type Identifier = Text

-- data Action
--   = NoOp
--   | SendRequest -- TODO parameterize
--   | Cmd Cmd
--   | SendChoiceSubmission OptionId SubmitChoice
--   deriving Show
-- 
-- data Page
--   = LoadingPage
--   | EditCharPage CharacterOptions
--   deriving (Show, Eq)
-- 
-- data Cmd
--   = Goto Page
--   | SelectLevel Level
--   | DropdownCmd Text DropdownCmd
--   | ClickOut
--   | SelectOrChoiceDir Text Dir
--   | ReceivedCharacterOptions CharacterOptions
--   deriving (Show, Eq)
-- 
-- data DropdownCmd
--   = OpenDropdown
--   | SelectDropdownOption (Maybe Text)
--   deriving (Show, Eq)
-- 
-- data Model = Model (View Action) (Cmd ~> View Action)
-- instance Eq Model where
--   _ == _ = False -- TODO is there a better way?

--------------------------------------------------------------------------------
-- EDIT CHARACTER PAGE
--------------------------------------------------------------------------------
type Level = Int

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
instance FromJSON Dir

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
