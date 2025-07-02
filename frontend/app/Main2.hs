module Main2 where

import Control.Applicative
import Control.Comonad
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Aeson
import Data.Functor
import Data.List
import Data.Maybe
import Reflex.Dom
import Reflex.Dom.Xhr
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.JSString (JSString)
import qualified Data.JSString as JSString
import qualified Data.Map as Map
import Language.Javascript.JSaddle.Types
import Debug.Trace hiding (traceEvent)
import Foreign.JavaScript.TH (withJSContextSingletonMono)
import JSDOM (currentDocumentUnchecked)
import GHCJS.DOM.Document (getBodyUnchecked)

import qualified Page.EditChar

import Data.Zipper (Zipper(Zipper))
import qualified Data.Zipper as Zipper
import Constants (charName)
import Types
import Util
import Widget

-- mainWidgetWithClickOut = withJSContextSingletonMono $ \jsSing -> do
--   doc <- currentDocumentUnchecked
--   body <- getBodyUnchecked doc

data Tab = EditCharTab | SheetTab deriving Eq

tabs :: [(Tab, Text)]
tabs = [(EditCharTab, "Edit"), (SheetTab, "Sheet")]

main :: IO ()
main = mainWidget $ do
  postBuildE <- getPostBuild
  let pageLoadE = "/api/character/" <> charName <> "/edit_character_page" <$ postBuildE
  receivedOptsE :: Event _ CharacterOptions <- fmap fromJust <$> getAndDecode pageLoadE
  (tabDyn, backToCharacterSelectionE) <- tabBarWidget
  widgetHold_ (text "Loading...") (Page.EditChar.page <$> receivedOptsE)


tabBarWidget :: ReactiveM t m => m (Dynamic t Tab, Event t ())
tabBarWidget = elClass "div" "dont-print tab-bar" $ liftA2 (,) tabWidgets backToCharacterSelectionWidget

tabWidgets :: ReactiveM t m => m (Dynamic t Tab)
tabWidgets = el "div" $ mdo
  selectedTabDyn <- holdDyn EditCharTab selectTabE
  selectTabE <- leftmost <$> mapM (tabWidget selectedTabDyn) tabs
  return selectedTabDyn

backToCharacterSelectionWidget :: ReactiveM t m => m (Event t ())
backToCharacterSelectionWidget = el "div" $ button "Back to character selection"

tabWidget :: ReactiveM t m => Dynamic t Tab -> (Tab, Text) -> m (Event t Tab)
tabWidget selectedTabDyn (tab, label) = do
  let highlightedDyn = (==tab) <$> selectedTabDyn
  clickE <- dynClassButton (uncondCondClasses [] ["selected"] highlightedDyn) label
  return (tab <$ clickE)
