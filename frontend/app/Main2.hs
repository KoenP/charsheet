module Main2 where

--------------------------------------------------------------------------------
import Data.Functor
import Reflex.Dom
import qualified Data.Map as Map
import Data.Text (Text)
import Optics

import qualified Page.EditChar
import qualified Page.Sheet

import Types
import Types.Cache
import Widget
--------------------------------------------------------------------------------

-- mainWidgetWithClickOut = withJSContextSingletonMono $ \jsSing -> do
--   doc <- currentDocumentUnchecked
--   body <- getBodyUnchecked doc

data Tab = EditCharTab | SheetTab deriving (Eq, Show)

type TabDef = (Tab, Text, Text)

tabPage :: ReactiveIOM t m => Tab -> (Cache -> m (Event t (Cache -> Cache)))
tabPage EditCharTab = Page.EditChar.load . view #options
tabPage SheetTab    = Page.Sheet.load . view #sheet

tabs :: [TabDef]
tabs = [(EditCharTab, "Edit", "edit.png"), (SheetTab, "Sheet", "sheet.png")]

main :: IO ()
main = mainWidget $ mdo
  cacheBh <- current . traceDyn "cache" <$> foldDyn ($) emptyCache updateCacheE
  (tabDyn, _) <- tabBarWidget
  let loadTab tab = tabPage tab =<< sample cacheBh
  updateCacheE <- elClass "div" "below-tabs" (switchHold never =<< dyn (fmap loadTab tabDyn))
  return ()

tabBarWidget :: ReactiveM t m => m (Dynamic t Tab, Event t ())
tabBarWidget = elClass "div" "dont-print tab-bar" $ liftA2 (,) tabWidgets backToCharacterSelectionWidget

tabWidgets :: ReactiveM t m => m (Dynamic t Tab)
tabWidgets = el "div" $ mdo
  selectedTabDyn <- holdDyn EditCharTab selectTabE
  selectTabE <- leftmost <$> mapM (tabWidget selectedTabDyn) tabs
  return selectedTabDyn

backToCharacterSelectionWidget :: ReactiveM t m => m (Event t ())
backToCharacterSelectionWidget = el "div" $ button "Back to character selection"

tabWidget :: ReactiveM t m => Dynamic t Tab -> TabDef -> m (Event t Tab)
tabWidget selectedTabDyn (tab, label, imgFileName) = do
  let highlightedDyn = (==tab) <$> selectedTabDyn
  let imgAttrsDyn = highlightedDyn <&> \hl ->
        Map.fromList [ ("src", "/static/icons/" <> imgFileName)
                     , ("class", if hl then "full-invert" else "partial-invert")
                     ]
  elDynAttr "img" imgAttrsDyn (pure ())
  clickE <- dynClassButton (uncondCondClasses [] ["selected"] highlightedDyn) label
  return (tab <$ clickE)
