module Page.EditChar where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Comonad
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Aeson
import Data.Functor
import Data.List
import Data.Maybe
import Reflex.Dom hiding ((.~))
import qualified Reflex.Dom ((.~))
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Map as Map
import Language.Javascript.JSaddle.Types
import Optics

import Widget
import Constants (charName)
import Types
import Types.Ability
import Types.Cache
import Util
import Data.Zipper (Zipper(Zipper))
import qualified Data.Zipper as Zipper

import Debug.Trace hiding (traceEvent, traceEventWith)
--------------------------------------------------------------------------------

load :: forall t m. ReactiveIOM t m => Maybe CharacterOptions -> m (Event t (Cache -> Cache))
load (Just opts) = trace "using cached version" $ page opts
load Nothing = do
  traceM "fetching fresh version"
  let url = "/api/character/" <> charName <> "/edit_character_page"
  loadWidget (xhrRequest "GET" url def) page
  -- let firstCacheE = fmap (\opts -> set #options (Just opts)) optionsE

page :: ReactiveIOM t m => CharacterOptions -> m (Event t (Cache -> Cache))
page charOpts0 = mdo

  let clickOutE = domEvent Click topLevel
  (topLevel, cacheE) <- elClass' "div" "edit-page" $ mdo
    let pageLoadE = mkReq <$> mainSectionE
    receivedNewCharOptsE <- fmap fromJust <$> postAndDecode pageLoadE
    charOptsDyn <- holdDyn charOpts0 receivedNewCharOptsE

    let abilityTableDyn = fmap (view #ability_table) charOptsDyn

    selectedLevelDyn <- sideNav charOptsDyn

    let selectedLevelOptsDyn = ffor2 charOptsDyn selectedLevelDyn $ \(CharacterOptions _ opts _) lvl ->
          fromJust $ lvl `Map.lookup` opts

    mainSectionE <- (switchHold never =<<) $ dyn $ fmap (mainSection clickOutE) $
      liftA3 (,,) selectedLevelDyn abilityTableDyn selectedLevelOptsDyn

    initCacheE <- getPostBuild
    return $ leftmost [ set #options . Just           <$> traceEventWith (const "set options cache") receivedNewCharOptsE
                      , const emptyCache              <$  traceEventWith (const "clear cache") mainSectionE
                      , set #options (Just charOpts0) <$  traceEventWith (const "init cache") initCacheE
                      ]

  return cacheE

-- TODO rework all of this
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

-- TODO Rework this, this sucks. Probably just construct the request when the event is fired.
mkReq :: MainSectionEvent -> Text
mkReq (MSEChoice (OptionId origin id) RetractChoice) = "/api/character/" <> charName <> "/retract_choice"
  <> "?source=" <> origin
  <> "&id=" <> id
mkReq (MSEChoice (OptionId origin id) choice) = "/api/character/" <> charName <> "/choice"
  <> "?source=" <> origin
  <> "&id=" <> id
  <> "&choice=" <> submitChoiceToString choice
  where
    submitChoiceToString (SubmitListChoice choices) = "[" <> Text.intercalate "," choices <> "]"
    submitChoiceToString (SubmitSingletonChoice choice) = choice
mkReq (MSESetBaseAbilityScores abilities) = "/api/character/" <> charName <> "/set_base_abilities?"
  <> Text.intercalate "&" [ability <> "=" <> showText score | (ability,score) <- abilities]


-- Sidenav
-- -------
sideNav :: forall m t. (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
        => Dynamic t CharacterOptions
        -> m (Dynamic t Int)
sideNav charOptsDyn = elClass "div" "side-nav" $ mdo
  opts0 <- sample $ current charOptsDyn

  selectLevelE <- (switchHold never =<<) $ dyn $ charOptsDyn
    <&> \(CharacterOptions _ optionsPerLevel curlvl) ->
          let maxlvl = maximum $ Map.keys optionsPerLevel
          in fmap leftmost $ sequence [sideNavButton selectedLevelDyn curlvl l | l <- [maxlvl,maxlvl-1..1]]

  selectedLevelDyn <- holdDyn (opts0 ^. #char_level) selectLevelE

  return selectedLevelDyn

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
data MainSectionEvent = MSEChoice OptionId SubmitChoice
                      | MSESetBaseAbilityScores [(Ability, Int)]
  deriving Show

mainSection :: ReactiveIOM t m
            => Event t ()
            -> (Level, AbilityTable, [Option])
            -> m (Event t MainSectionEvent)
mainSection clickOutE (level, abilityTable, options) = elClass "div" "main-section" $ do
  let baseAbilitiesEditable = level == 1
  setAbilityE <- abilityTableWidget baseAbilitiesEditable abilityTable
  choiceE <- divcl "character-options" $ do
    fmap leftmost $ mapM (uncurry $ originCategoryWidget clickOutE) originCategories
  return $ leftmost [setAbilityE, choiceE]
  where
    originCategories = map (\((_,k),v) -> (k,v))
      $ Map.assocs
      $ multiMapFromList
      $ zip (map optionsSortField options) options

    optionsSortField Option{display_origin_category, origin_category_index} =
      (origin_category_index, display_origin_category)

-- Ability table
-- -------------
abilityTableWidget :: ReactiveIOM t m => Bool -> AbilityTable -> m (Event t MainSectionEvent)
abilityTableWidget baseAbilitiesEditable abilityTable = divcl "ability-edit" $ el "table" $ do
  el "tr" $ el "th" blank >> mapM_ (el "th" . text) abilities

  let taggedEntries = [(abi, fromJust $ abi `Map.lookup` abilityTable) | abi <- abilities]
      entries = map snd taggedEntries

  let simpleRow header vals = el "tr" $ el "th" (text header) >> mapM_ (el "td" . text) vals

  -- Base scores row. Contains number input for each ability.
  setBaseAbilitiesE <- if baseAbilitiesEditable

    -- If the base abilities are editable, 
    then el "tr" $
         do el "th" (text "Base Score")

            -- A Dynamic for each ability setter input.
            abilityValueDyns <- mapM (el "td" . baseAbilityScoreSetterWidget) taggedEntries

            -- Trigger an event 0.5 seconds after the most recent update to one of these Dynamics.
            sampleE <- debounce 0.5 $ void $ leftmost (map updated abilityValueDyns)

            -- The value to be read when the event eventually fires.
            let collectedDyn = MSESetBaseAbilityScores <$> sequence abilityValueDyns

            return $ current collectedDyn `tag` sampleE

    -- If the base abilities are not editable, just show the base score as a number (not an input).
    else simpleRow "Base Score" (map (showText . view #base) entries) >> return never

  simpleRow "Total Bonus" $ map (formatModifier . view #total_bonus ) entries
  simpleRow "Total Score" $ map (showText       . view #score       ) entries
  simpleRow "Modifier"    $ map (formatModifier . view #mod         ) entries

  return setBaseAbilitiesE


baseAbilityScoreSetterWidget :: forall t m. ReactiveM t m
                             => (Ability, AbilityTableEntry) -> m (Dynamic t (Ability, Int))
baseAbilityScoreSetterWidget (ability, AbilityTableEntry{base}) = do
  inputE <- fmap _inputElement_value $ inputElement config
  return $ fmap (\score -> (ability, readText score)) inputE

  where
    config = def
      & inputElementConfig_initialValue Reflex.Dom..~ showText base
      & inputElementConfig_elementConfig . elementConfig_initialAttributes Reflex.Dom..~ ("type" |-> "number")


-- Options
-- -------
originCategoryWidget :: ReactiveM t m
                     => Event t () -> Text -> [Option]
                     -> m (Event t MainSectionEvent)
originCategoryWidget clickOutE cat opts = elClass "div" "origin-category" $ do
  let headerText = case cat of
        "init"     -> "Choose your background, class, and race:"
        "level up" -> "Level up:"
        _          -> "From " <> cat <> ":"
  el "h2" (text headerText)
  leftmost <$> mapM (optionWidget clickOutE) opts

optionWidget :: ReactiveM t m => Event t () -> Option -> m (Event t MainSectionEvent)
optionWidget clickOutE Option{id, display_id, origin, spec, choice}
  = elClass "div" "options-section-style"
  $ do el "h3" (text display_id)
       specWidget clickOutE optionId spec choice
  where
    optionId = OptionId origin id

-- Spec and choices
-- ----------------
specWidget :: ReactiveM t m
           => Event t () -> OptionId -> Spec -> Maybe Choice
           -> m (Event t MainSectionEvent)
specWidget clickOutE optionId spec choice = case spec of
  ListSpec entries -> listSpecWidget
    clickOutE optionId entries (fromJust . preview #_AtomicChoice <$> choice)
  OrSpec leftname left rightname right -> orSpecWidget
    clickOutE optionId leftname left rightname right
    (fromJust . preview #_OrChoice <$> choice)
  FromSpec unique num (ListSpec entries) -> fromSpecWidget
    clickOutE optionId unique num entries
    (map (fromJust . preview #_AtomicChoice) $ concatMap (fromJust . preview #_ListChoice) $ maybeToList choice)
  _ -> error $ "unsupported spec: " <> show spec

listSpecWidget :: ReactiveM t m => Event t () -> OptionId -> [ListSpecEntry] -> Maybe Text
               -> m (Event t MainSectionEvent)
listSpecWidget clickOutE optionId entries choice =
  updated . fmap inform
  <$> customDropdownWidget clickOutE [(entry ^. #opt, True)| entry <- entries] choice
  where
    inform (Just choice) = MSEChoice optionId (SubmitSingletonChoice choice)
    inform Nothing       = MSEChoice optionId RetractChoice

orSpecWidget :: forall t m. ReactiveM t m
             => Event t () -> OptionId -> Text -> Spec -> Text -> Spec -> Maybe (Dir, Choice)
             -> m (Event t MainSectionEvent)
orSpecWidget clickOutE optionId leftname left rightname right choice = el "div" $ mdo
  let subChoice dir = [c | (dir', c) <- choice, dir == dir']

  let leftSubSpecWidget, rightSubSpecWidget :: m (Event t MainSectionEvent)
      leftSubSpecWidget  = specWidget clickOutE optionId left (subChoice L)
      rightSubSpecWidget = specWidget clickOutE optionId right (subChoice R)

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
                     Nothing -> return never
                     Just L  -> leftSubSpecWidget
                     Just R  -> rightSubSpecWidget

  el "div" $ switchDyn <$> widgetHold subWidget0 selectE

fromSpecWidget :: ReactiveM t m
               => Event t () -> OptionId
               -> Unique -> Maybe Int -> [ListSpecEntry]
               -> [Text]
               -> m (Event t MainSectionEvent)
fromSpecWidget clickOutE optionId unique limit entries choices = mdo
  -- Render a prefilled dropdown widget for each choice.
  overwriteChoiceEs <- map updated <$> mapM (mkDropdownWidget . Just) choices

  -- If there are fewer choices than the limit, or if there is no limit, render
  -- a dropdown widget that is not yet filled in.
  appendChoiceEs <- map updated
    <$> mapM mkDropdownWidget [Nothing | fromMaybe True ((length choices <) <$> limit)]

  -- If there is a limit, create inert, greyed-out "dropdowns" as placeholders for the
  -- remaining choices.
  replicateM_ (fromMaybe 0 ((\num -> num - length choices - 1) <$> limit))
    $ elClass "div" "dropdown dropdown-disabled" (button "...")

  return
    $ fmap (MSEChoice optionId)
    $ leftmost
    $ zipWith fmap (choiceEditFunctions choices) (overwriteChoiceEs <> appendChoiceEs)

  where mkDropdownWidget = customDropdownWidget clickOutE
          [(o, not (o `elem` choices))| o <- map (view #opt) entries]

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


customDropdownWidget :: forall t m. ReactiveM t m
                     => Event t () -> [(Text, Bool)] -> Maybe Text
                     -> m (Dynamic t (Maybe Text))
customDropdownWidget clickOutE options selected0 = mdo
  elAttr "div" (Map.fromList [ ("class", "dropdown dropdown-enabled")
                             , ("onclick", "event.stopPropagation();")
                             ]) $ mdo
    selectedDyn <- holdDyn selected0 $ selectE
    openDyn <- foldDyn ($) False $ leftmost [not <$ toggleE, const False <$ closeE]
    let dropdownButtonClassDyn = openDyn <&> ("dropdown-button-open" ? "dropdown-button-closed")
    (buttonEl, selectE) <- elDynClass' "button" dropdownButtonClassDyn $ mdo
      dynText $ fmap (fromMaybe "...") selectedDyn

      let divStyleDyn = openDyn <&> \open ->
            "style" |-> ("visibility: " <> if open then "visible" else "hidden")
      elDynAttr "div" (Map.insert "class" "dropdown-content" <$> divStyleDyn) $
        leftmost <$>
        liftA2 (:) (customDropdownEntryWidget Nothing) (mapM (customDropdownEntryWidget . Just) options)


    let toggleE = domEvent Click buttonEl
    let closeE = leftmost [void selectE, clickOutE `difference` toggleE]

    return selectedDyn

customDropdownEntryWidget :: ReactiveM t m
                          => Maybe (Text,Bool) -> m (Event t (Maybe Text))
customDropdownEntryWidget option = do
  let buttonText = fromMaybe "-- clear selection --" $ fmap fst option
      enabled = fmap snd option /= Just False
      classes = "dropdown-entry" <> if enabled then "" else " disabled"
  (buttonEl, _) <- elClass' "button" classes (text buttonText)
  return $ if enabled
           then fmap fst option <$ domEvent Click buttonEl
           else never

