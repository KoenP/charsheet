module Page.EditChar where

--------------------------------------------------------------------------------
import Prelude hiding (unzip)

import Control.Applicative
import Control.Comonad
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Bifunctor (bimap)
import Data.Functor
import Data.List hiding (unzip)
import Data.Maybe
import GHC.Generics
import Reflex.Dom hiding ((.~))
import qualified Reflex.Dom ((.~))
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Map as Map
import Language.Javascript.JSaddle.Types
import Optics

import Widget
import Widget.Dropdown
import Constants (charName)
import Types
import Types.Ability
import Types.Cache
import Util
import Data.Zipper (Zipper(Zipper))
import qualified Data.Zipper as Zipper
--------------------------------------------------------------------------------

-- | Load the "edit character" page.
--   Only sends a request to the server if no cached version of the character options is provided.
load :: forall t m. ReactiveIOM t m => Maybe CharacterOptions -> m (Event t (Cache -> Cache))
load (Just opts) = page opts
load Nothing = do
  let url = "/api/character/" <> charName <> "/edit_character_page"
  (initE, cacheE) <- loadWidget (xhrRequest "GET" url def) page
  return $ leftmost [cacheE, (set #options . Just) <$> initE]

-- | Construct the "edit character" page.
--   It consists of a side bar where the character level can be selected, and a main section.
--   The main section contains an ability score table and a list of options that can be set by the user.
--
--   If the user changes any of the character's properties, the page will send a request to the server to update the character options.
--   Upon receiving the new character options, the page will update itself.
--
--   Every time the user makes a change, the sheet and character options caches are invalidated.
--   When the server responds with new character options, the character options cache is refreshed.
page :: ReactiveIOM t m => CharacterOptions -> m (Event t (Cache -> Cache))
page charOpts0 = mdo
  let clickOutE = domEvent Click topLevel
  (topLevel, cacheE) <- elClass' "div" "edit-page" $ mdo
    -- Send any updates to the server, fire an event when the server responds with fresh data.
    -- Every time we send a request to the server, we lock the page while waiting for the response.
    receivedNewCharOptsE <- fmap fromJust <$> postAndDecode pageLoadE
    charOptsDyn <- holdDyn charOpts0 receivedNewCharOptsE
    lockDyn <- holdDyn False $ leftmost [False <$ receivedNewCharOptsE, True <$ pageLoadE]

    -- Render the side nav, which keeps track of the currently selected character level.
    selectedLevelDyn <- sideNav charOptsDyn hoverDyn

    -- Render the main section, which displays character options for the currently selected level.
    let
      selectedLevelOptsDyn = ffor2 charOptsDyn selectedLevelDyn $ \(CharacterOptions _ opts _) lvl ->
        fromJust $ lvl `Map.lookup` opts
      abilityTableDyn = fmap (view #ability_table) charOptsDyn
      runWithDropdownCtx = flip runReaderT (lockDyn, clickOutE)

    (pageLoadEE, hoverDynE) <- fmap unzip $ dyn $ fmap runWithDropdownCtx $ fmap mainSection $
      liftA3 (,,) selectedLevelDyn abilityTableDyn selectedLevelOptsDyn
    pageLoadE <- switchHold never pageLoadEE

    -- Retrieving the hover dynamic is a bit awkward. There is no flattening function for Event t (Dynamic t a),
    -- so we turn the inner Dynamic into an Event, apply an Event flattening function, and turn the resulting Event
    -- into a Dynamic.
    -- TODO maybe there is a more straightforward way.
    hoverDyn <- holdDyn Nothing =<< switchHold never (fmap updated hoverDynE)

    return $ leftmost [ set #options . Just <$> receivedNewCharOptsE
                      , const emptyCache    <$  pageLoadE
                      ]

  return cacheE


-- Requests
-- --------
postAndDecode :: ( DomBuilder t m
                 , MonadHold t m
                 , PostBuild t m
                 , MonadJSM (Performable m)
                 , MonadIO m
                 , PerformEvent t m
                 , TriggerEvent t m
                 , MonadFix m
                 , FromJSON a
                 )
              => Event t Text -> m (Event t (Maybe a))
postAndDecode url = do
  r <- performRequestAsync $ fmap (\x -> XhrRequest "POST" x def) url
  return $ fmap decodeXhrResponse r

type Req = Text

mkChoiceReq :: OptionId -> SubmitChoice -> Req
mkChoiceReq (OptionId origin id) RetractChoice = "/api/character/" <> charName <> "/retract_choice"
  <> "?source=" <> origin
  <> "&id=" <> id
mkChoiceReq (OptionId origin id) choice = "/api/character/" <> charName <> "/choice"
  <> "?source=" <> origin
  <> "&id=" <> id
  <> "&choice=" <> submitChoiceToString choice
  where
    submitChoiceToString (SubmitListChoice choices) = "[" <> Text.intercalate "," choices <> "]"
    submitChoiceToString (SubmitSingletonChoice choice) = choice

mkSetAbilitiesReq :: [(Ability, Int)] -> Req
mkSetAbilitiesReq abilities = "/api/character/" <> charName <> "/set_base_abilities?"
  <> Text.intercalate "&" [ability <> "=" <> showText score | (ability,score) <- abilities]


-- Sidenav
-- -------
sideNav :: forall m t. (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
        => Dynamic t CharacterOptions
        -> Dynamic t (Maybe [Text])
        -> m (Dynamic t Int)
sideNav charOptsDyn maybeHoverTextDyn = elClass "div" "side-nav" $ mdo
  opts0 <- sample $ current charOptsDyn

  selectLevelE <- (switchHold never =<<) $ dyn $ fmap (selectWidget selectedLevelDyn) maybeHoverTextDyn

  selectedLevelDyn <- holdDyn (opts0 ^. #char_level) selectLevelE

  return selectedLevelDyn

  where
    selectWidget :: Dynamic t Int -> Maybe [Text] -> m (Event t Int)

    selectWidget _ (Just (header : hoverText)) = do
      el "h3" (text header)
      mapM_ (el "p" . text) hoverText
      return never

    selectWidget selectedLevelDyn Nothing = (switchHold never =<<) $ dyn $ charOptsDyn
      <&> \(CharacterOptions _ optionsPerLevel curlvl) ->
            let maxlvl = maximum $ Map.keys optionsPerLevel
            in fmap leftmost $ sequence [sideNavButton selectedLevelDyn curlvl l | l <- [maxlvl,maxlvl-1..1]]

sideNavButton :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
              => Dynamic t Int -> Int -> Int -> m (Event t Int)
sideNavButton selectedLevelDyn charLevel level = do
  let styleDyn = selectedLevelDyn <&> \selectedLevel ->
        if selectedLevel == level
        then Map.singleton "style" "color: #ffffff"
        else Map.empty
  let buttonText | level > charLevel = "+"
                 | otherwise         = "Level " <> pack (show level)
  selectLevelE <- elDynAttr "div" styleDyn $ button buttonText
  return (level <$ selectLevelE)

-- Main section
-- ------------
type MainSectionIOM t m = (ReactiveIOM t m, MonadReader (Dynamic t Bool, Event t ()) m)
type MainSectionM t m = (ReactiveM t m, MonadReader (Dynamic t Bool, Event t ()) m)

mainSection :: MainSectionIOM t m
            => (Level, AbilityTable, [Option])
            -> m (Event t Req, Dynamic t (Maybe [Text]))
mainSection (level, abilityTable, options) = elClass "div" "main-section" $ do
  -- Show the ability table.
  -- Only show the base abilities as editable when character level 1 is selected.
  let baseAbilitiesEditable = level == 1
  setAbilityE <- abilityTableWidget baseAbilitiesEditable abilityTable

  -- Show all character options.
  (choiceE, hoverDyn) <- divcl "character-options" $ do
    fmap mconcat $ mapM (uncurry originCategoryWidget) originCategories

  return $ (leftmost [setAbilityE, choiceE], hoverDyn)
  where
    originCategories = map (\((_,k),v) -> (k,v))
      $ Map.assocs
      $ multiMapFromList
      $ zip (map optionsSortField options) options

    optionsSortField Option{display_origin_category, origin_category_index} =
      (origin_category_index, display_origin_category)

-- Ability table
-- -------------
abilityTableWidget :: MainSectionIOM t m => Bool -> AbilityTable -> m (Event t Req)
abilityTableWidget baseAbilitiesEditable abilityTable = divcl "ability-edit" $ el "table" $ do
  el "tr" $ el "th" blank >> mapM_ (el "th" . text) abilities

  let taggedEntries = [(abi, fromJust $ abi `Map.lookup` abilityTable) | abi <- abilities]
      entries = map snd taggedEntries

  let simpleRow header vals = el "tr" $ el "th" (text header) >> mapM_ (el "td" . text) vals

  -- Base scores row. Contains number input for each ability.
  setBaseAbilitiesE <- if baseAbilitiesEditable

    -- If the base abilities are editable, show an input element for each one.
    then el "tr" $
         do el "th" (text "Base Score")

            -- A Dynamic for each ability setter input.
            abilityValueDyns <- mapM (el "td" . baseAbilityScoreSetterWidget) taggedEntries

            -- Trigger an event 0.5 seconds after the most recent update to any one of these Dynamics.
            sampleE <- debounce 0.5 $ void $ leftmost (map updated abilityValueDyns)

            -- The value to be read when the event eventually fires.
            let collectedDyn = mkSetAbilitiesReq <$> sequence abilityValueDyns

            return $ current collectedDyn `tag` sampleE

    -- If the base abilities are not editable, just show the base score as a number (not an input).
    else simpleRow "Base Score" (map (showText . view #base) entries) >> return never

  simpleRow "Total Bonus" $ map (formatModifier . view #total_bonus ) entries
  simpleRow "Total Score" $ map (showText       . view #score       ) entries
  simpleRow "Modifier"    $ map (formatModifier . view #mod         ) entries

  return setBaseAbilitiesE


baseAbilityScoreSetterWidget :: forall t m. MainSectionIOM t m
                             => (Ability, AbilityTableEntry) -> m (Dynamic t (Ability, Int))
baseAbilityScoreSetterWidget (ability, AbilityTableEntry{base}) = mdo
  -- Each setter is a HTML number input element that should be disabled while the page is locked.
  -- As far as I can tell it's not possible to provide attributes to the input element dynamically
  -- (short of rolling our own input element), so we have to jump through some hoops to re-create
  -- the input element each time the value of the page lock changes.
  -- TODO: there might be a less roundabout way.
  -- TODO: at the moment, after the user changes the value in these setters, the
  -- value jumps back to the unchanged value when the page is locked, until the
  -- new character data is received. I should fix that at some point.
  lockDyn <- asks getLockDyn
  lock0 <- sample (current lockDyn)
  join <$> widgetHold (mkWidget lock0) (mkWidget <$> updated lockDyn)
  where
    -- Build the number input widget, parameterized on whether it is currently locked.
    mkWidget :: Bool -> m (Dynamic t (Ability, Int))
    mkWidget = fmap (fmap parse . _inputElement_value) . inputElement . config

    -- Input element configuration, depending on whether the element should be locked.
    config locked = def
      & inputElementConfig_initialValue Reflex.Dom..~ showText base
      & inputElementConfig_elementConfig.elementConfig_initialAttributes
          Reflex.Dom..~ Map.fromList ([("type", "number")] ++ [("disabled", "true") | locked])

    parse :: Text -> (Ability, Int)
    parse str = (ability, readText str)


-- Options
-- -------
originCategoryWidget :: MainSectionM t m
                     => Text -> [Option]
                     -> m (Event t Req, Dynamic t (Maybe [Text]))
originCategoryWidget cat opts = elClass "div" "origin-category" $ do
  let headerText = case cat of
        "init"     -> "Choose your background, class, and race:"
        "level up" -> "Level up:"
        _          -> "From " <> cat <> ":"
  el "h2" (text headerText)
  mconcat <$> mapM optionWidget opts

optionWidget :: MainSectionM t m => Option -> m (Event t Req, Dynamic t (Maybe [Text]))
optionWidget Option{id, display_id, origin, spec, choice}
  = elClass "div" "options-section-style"
  $ do el "h3" (text display_id)
       specWidget optionId spec choice
  where
    optionId = OptionId origin id

-- Spec and choices
-- ----------------
specWidget :: MainSectionM t m
           => OptionId -> Spec -> Maybe Choice
           -> m (Event t Req, Dynamic t (Maybe [Text]))
specWidget optionId spec choice = case spec of
  ListSpec entries -> listSpecWidget
    optionId entries (fromJust . preview #_AtomicChoice <$> choice)
  OrSpec leftname left rightname right -> orSpecWidget
    optionId leftname left rightname right
    (fromJust . preview #_OrChoice <$> choice)
  FromSpec unique num (ListSpec entries) -> fromSpecWidget
    optionId unique num entries
    (map (fromJust . preview #_AtomicChoice) $ concatMap (fromJust . preview #_ListChoice) $ maybeToList choice)
  _ -> error $ "unsupported spec: " <> show spec

listSpecWidget :: MainSectionM t m => OptionId -> [ListSpecEntry] -> Maybe Text
               -> m (Event t Req, Dynamic t (Maybe [Text]))
listSpecWidget optionId entries choice = do
  (selectE, hoverDyn) <- customDropdownWidget
      [DropdownEntry opt desc (Just opt /= choice) | ListSpecEntry desc opt <- entries]
      choice
  return (updated $ fmap inform selectE, hoverDyn)
  where
    inform (Just choice) = mkChoiceReq optionId (SubmitSingletonChoice choice)
    inform Nothing       = mkChoiceReq optionId RetractChoice

orSpecWidget :: forall t m. MainSectionM t m
             => OptionId -> Text -> Spec -> Text -> Spec -> Maybe (Dir, Choice)
             -> m (Event t Req, Dynamic t (Maybe [Text]))
orSpecWidget optionId leftname left rightname right choice = el "div" $ mdo
  let subChoice dir = [c | (dir', c) <- choice, dir == dir']

  let leftSubSpecWidget, rightSubSpecWidget :: m (Event t Req, Dynamic t (Maybe [Text]))
      leftSubSpecWidget  = specWidget optionId left (subChoice L)
      rightSubSpecWidget = specWidget optionId right (subChoice R)

  -- TODO styling
  selectedDirDyn <- holdDyn (fmap fst choice) $ leftmost [Just L <$ selectLeftE, Just R <$ selectRightE]
  let styleDyn dir = selectedDirDyn <&> \dir' -> Map.fromList
        [("style", "font-weight: bold;") | Just dir == dir']
  selectLeftE  <- dynAttrButton (styleDyn L) leftname
  selectRightE <- dynAttrButton (styleDyn R) rightname
  let selectE = leftmost [ leftSubSpecWidget  <$ selectLeftE
                         , rightSubSpecWidget <$ selectRightE
                         ]

  let subWidget0 = case fmap fst choice of
                     Nothing -> return (never, pure Nothing)
                     Just L  -> leftSubSpecWidget
                     Just R  -> rightSubSpecWidget

  el "div" $ do
    (reqE, hoverDyn) <- splitDynPure <$> widgetHold subWidget0 selectE
    return (switchDyn reqE, join hoverDyn)

fromSpecWidget :: forall m t. MainSectionM t m
               => OptionId
               -> Unique -> Maybe Int -> [ListSpecEntry]
               -> [Text]
               -> m (Event t Req, Dynamic t (Maybe [Text]))
fromSpecWidget optionId unique limit entries choices = mdo
  -- Render a prefilled dropdown widget for each choice.
  (overwriteChoiceDyns, hoverDyns1) <- unzip <$> mapM (mkDropdownWidget . Just) choices

  -- If there are fewer choices than the limit, or if there is no limit, render
  -- a dropdown widget that is not yet filled in.
  (appendChoiceDyns, hoverDyns2) <- unzip <$> mapM mkDropdownWidget [Nothing | fromMaybe True ((length choices <) <$> limit)]

  -- If there is a limit, create inert, greyed-out "dropdowns" as placeholders for the
  -- remaining choices.
  replicateM_ (fromMaybe 0 ((\num -> num - length choices - 1) <$> limit))
    $ elClass "div" "dropdown dropdown-disabled" (button "...")

  let reqE = fmap (mkChoiceReq optionId)
           $ leftmost
           $ zipWith fmap (choiceEditFunctions choices) (map updated (overwriteChoiceDyns ++ appendChoiceDyns))

  return (reqE, mconcat (hoverDyns1 <> hoverDyns2))

    where mkDropdownWidget = customDropdownWidget
            [DropdownEntry opt desc (not (opt `elem` choices))| ListSpecEntry desc opt <- entries]

choiceEditFunctions :: [Text] -> [Maybe Text -> SubmitChoice]
choiceEditFunctions choices = case choices of
  []     -> [SubmitListChoice . singleton . fromMaybe ""]
  c : cs -> Zipper.toList (extend overwriteOrDeleteFocused (Zipper [] c cs))
    <> [SubmitListChoice . ((c:cs) <>) . singleton . fromMaybe ""]

  where
    overwriteOrDeleteFocused :: Zipper Text -> (Maybe Text -> SubmitChoice)
    overwriteOrDeleteFocused (Zipper ls _ rs) newChoice =
      SubmitListChoice $ case newChoice of
                           Just x  -> Zipper.toList (Zipper ls x rs)
                           Nothing -> reverse ls <> rs
