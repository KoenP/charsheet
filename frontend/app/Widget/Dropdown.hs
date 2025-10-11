module Widget.Dropdown where

--------------------------------------------------------------------------------
import Control.Monad
import Control.Monad.Reader
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics

import Optics
import Reflex.Dom

import Types
import Util
--------------------------------------------------------------------------------

data DropdownEntry = DropdownEntry
  { label   :: Text
  , desc    :: [Text]
  , enabled :: Bool
  , classes :: [Text]
  }
  deriving Generic

-- data DropdownContext t = DropdownContext
--   { lockDyn   :: Dynamic t Bool
--   , clickOutE :: Event t ()
--   }
--   deriving Generic

class DropdownCtx ctx t | ctx -> t where
  getLockDyn   :: ctx -> Dynamic t Bool
  getClickOutE :: ctx -> Event t ()

instance DropdownCtx (Dynamic t Bool, Event t ()) t where
  getLockDyn   = fst
  getClickOutE = snd

customDropdownWidget :: forall ctx t m. (ReactiveM t m, MonadReader ctx m, DropdownCtx ctx t)
                     => [DropdownEntry]
                     -> Maybe Text
                     -> m (Dynamic t (Maybe Text), Dynamic t (Maybe [Text]))
customDropdownWidget entries selected0 = do
  lockDyn   <- asks getLockDyn
  clickOutE <- asks getClickOutE
  let
    -- The dropdown is greyed out and not clickable if it is locked.
    classAttrDyn = lockDyn <&> \locked -> ("class", if locked then "dropdown dropdown-disabled" else "dropdown dropdown-enabled")

    -- Hack to prevent the click event that opens the dropdown from immediately triggering a clickout event that closes it.
    attrDyn = Map.fromList . (: [("onclick", "event.stopPropagation();")]) <$> classAttrDyn

  elDynAttr "div" attrDyn $ mdo

    -- Keep track of which entry in the dropdown menu is selected.
    selectedDyn <- holdDyn selected0 selectE

    -- Keep track of whether the dropdown menu is open or closed.
    -- Toggle events switch the menu between the two states; close events always close it.
    openDyn <- foldDyn ($) False $ leftmost [not <$ toggleE, const False <$ closeE]

    let dropdownButtonOpenClosedDyn = ("dropdown-button-open" ? "dropdown-button-closed") <$> openDyn
        dropdownFilledInClassDyn = ("dropdown-filled-in" ? "dropdown-blank") . isJust <$> selectedDyn
        dropdownButtonClassDyn = Text.intercalate " " <$> sequenceA [dropdownButtonOpenClosedDyn, dropdownFilledInClassDyn]

    -- Render the button, and conditionally, the dropdown menu.
    (buttonEl, (selectE, hoverDyn)) <- elDynClass' "button" dropdownButtonClassDyn $ mdo
      -- Render button text.
      el "span" $ dynText $ fmap (fromMaybe "...") selectedDyn

      -- Render the menu (setting its visibility to "hidden" if it's not open).
      let divStyleDyn = openDyn <&> \open ->
            "style" |-> ("visibility: " <> if open then "visible" else "hidden")
      elDynAttr "div" (Map.insert "class" "dropdown-content" <$> divStyleDyn)
        $ fmap mconcat
        $ sequence
        $ customDropdownEntryWidget Nothing -- Add a blank entry which allows for undoing the selection.
        : map (customDropdownEntryWidget . Just) entries

    -- Clicking the button toggles the menu between its open and closed state (unless the button is locked).
    let toggleE = gate (not <$> current lockDyn) $ domEvent Click buttonEl

    -- The menu is supposed to close if either an entry was selected, or if a
    -- clickout event occurs (that's not simultaneous with a toggle event).
    let closeE = leftmost [void selectE, clickOutE `difference` toggleE]

    return (selectedDyn, hoverDyn)

customDropdownEntryWidget :: (ReactiveM t m, MonadReader ctx m, DropdownCtx ctx t)
                          => Maybe DropdownEntry -> m (Event t (Maybe Text), Dynamic t (Maybe [Text]))
customDropdownEntryWidget entry = do
  -- Create a button.
  let buttonText = fromMaybe "-- clear selection --" $ fmap (view #label) entry
      enabled = fmap (view #enabled) entry /= Just False
      entryClasses = fromMaybe [] $ entry <&> (^. #classes)
      classes = Text.intercalate " " $ entryClasses <> catMaybes [Just "dropdown-entry" , ["disabled"  | not enabled]]
  (buttonEl, _) <- elClass' "button" classes (text buttonText)

  -- When the button is clicked, fire an event carring the entry's label.
  let clickE = fmap (view #label) entry <$ domEvent Click buttonEl

  -- While hovering over the button, produce the event's description.
  hoverDyn <- holdDyn Nothing $ leftmost [ fmap (\DropdownEntry{label, desc} -> label : desc) entry <$ domEvent Mouseenter buttonEl
                                         , Nothing                                                  <$ domEvent Mouseleave buttonEl
                                         , Nothing                                                  <$ domEvent Click buttonEl
                                         ]

  -- If the entry is disabled, the click event shouldn't fire, but the hover effect should still behave as normal.
  return (if enabled then clickE else never, hoverDyn)
