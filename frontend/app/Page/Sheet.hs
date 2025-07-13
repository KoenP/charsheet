module Page.Sheet where

--------------------------------------------------------------------------------
import Control.Monad
import Reflex.Dom

import Widget
import Constants (charName)
import Types
import Util
--------------------------------------------------------------------------------

load :: ReactiveIOM t m => m ()
load = void $ loadWidget () (xhrRequest "GET" url def) page
  where url = "/api/character/" <> charName <> "/sheet"

-- TODO: can probably use a more restricted type class as the sheet is currently
-- non-interactive.
page :: DomBuilder t m => CharacterSheet -> m ()
page sheet = do
  divcl "page" $ do
    divcl "abilities" pass
    divcl "main-body" (mainBodyWidget sheet)
    divcl "hit-dice-section" pass
  divcl "page-break" pass
  divcl "page" $ do
    divcl "column" pass
    divcl "column" pass
    divcl "column" pass

mainBodyWidget :: DomBuilder t m => CharacterSheet -> m ()
mainBodyWidget CharacterSheet{cs_name, cs_summary} =
  let CharacterSummary{csm_classes, csm_race, csm_level} = cs_summary
  in do
    divcl "charname" $ do
      el "h1" (text cs_name)
      divcl "race-and-classes" (text $ csm_race <>  " — " <> csm_classes)
      divcl "charlevel" (domShow csm_level)
