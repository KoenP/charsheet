module Page.Sheet where

--------------------------------------------------------------------------------
import Control.Monad
import qualified Data.Text as Text
import Reflex.Dom

import Widget
import Constants (charName)
import Types
--------------------------------------------------------------------------------

load :: ReactiveIOM t m => m ()
load = void $ loadWidget () (xhrRequest "GET" url def) page
  where url = "/api/character/" <> charName <> "/sheet"

page :: ReactiveIOM t m => CharacterSheet -> m ()
page sheet = text $ Text.pack $ show sheet
