module Types.Cache where

--------------------------------------------------------------------------------
import GHC.Generics
import Optics

import Types
--------------------------------------------------------------------------------

data Cache = Cache { sheet      :: Maybe CharacterSheet
                   , options    :: Maybe CharacterOptions
                   , cardConfig :: Maybe CardConfig
                   }
  deriving (Show, Generic)

invalidate :: Lens' Cache (Maybe a) -> Cache -> Cache
invalidate l = set l Nothing

emptyCache :: Cache
emptyCache = Cache Nothing Nothing Nothing
