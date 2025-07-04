module Main2 where

--------------------------------------------------------------------------------
import Data.Functor
import Reflex.Dom
import qualified Data.Map as Map
import Data.Text (Text)

import qualified Page.EditChar
import qualified Page.Sheet

import Types
import Widget
--------------------------------------------------------------------------------

-- mainWidgetWithClickOut = withJSContextSingletonMono $ \jsSing -> do
--   doc <- currentDocumentUnchecked
--   body <- getBodyUnchecked doc

data Tab = EditCharTab | SheetTab deriving Eq

type TabDef = (Tab, Text, Text)

tabs :: [TabDef]
tabs = [(EditCharTab, "Edit", "edit.png"), (SheetTab, "Sheet", "sheet.png")]

main :: IO ()
main = mainWidget $ do
  (tabDyn, _) <- tabBarWidget
  let loadTab EditCharTab = Page.EditChar.load
      loadTab SheetTab = Page.Sheet.load
  elClass "div" "below-tabs" $ dyn_ (fmap loadTab tabDyn)

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
