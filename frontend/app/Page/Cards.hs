module Page.Cards where

--------------------------------------------------------------------------------
import Control.Monad
import Reflex.Dom
import Data.Text (Text)
import qualified Data.Text as Text

import Types
import Types.Ability
--------------------------------------------------------------------------------

load :: ReactiveIOM t m => Maybe CharacterSheet -> m (Event t (Cache -> Cache))
load (Just sheet) = page sheet >> return never
load Nothing      = do
  (initE, _) <- loadWidget (xhrRequest "GET" ("/api/character/" <> charName <> "/sheet") def) (\sheet -> page sheet >> return never)
  return (set #sheet . Just <$> initE)

page :: DomBuilder t m => Maybe CharacterSheet -> CharacterSheet -> m ()
page maybeOldSheet sheet = do
  gotoCardSelectPageE <- button "Configure"

  let (traitCategories, spellcastingSections) getCardSections maybeOldSheet sheet


  return ()

getCardSections :: Maybe CharacterSheet -> CharacterSheet -> ([NotableTraitCategory], [SpellcastingSection])
getCardSections Nothing         sheet = (sheet ^. #notable_traits, sheet ^. #spellcasting_sections)
getCardSections (Just oldSheet) sheet =
  ( diffTraitCategories (oldSheet ^. #notable_traits) (sheet ^. #notable_traits)
  , diffSpellcastingSections (oldSheet ^. #spellcasting_sections) (sheet ^. #spellcasting_sections)
  )
  where
    diffTraitCategories = undefined
    diffSpellcastingSections = undefined

