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

import Types
import Util
import Data.Zipper (Zipper(Zipper))
import qualified Data.Zipper as Zipper

foreign import javascript "getCharName"
  js_getCharName :: JSString

charName :: Text
charName = pack $ JSString.unpack js_getCharName

main :: IO ()
main = mainWidget $ do
  postBuildE <- getPostBuild
  let pageLoadE = "/api/character/" <> charName <> "/edit_character_page" <$ postBuildE
  receivedOptsE :: Event _ CharacterOptions <- fmap fromJust <$> getAndDecode pageLoadE
  widgetHold_ (text "Loading...") (editCharacterPage <$> receivedOptsE)

editCharacterPage :: ( DomBuilder t m
                     , MonadHold t m
                     , PostBuild t m
                     , MonadJSM (Performable m)
                     , MonadIO m
                     , PerformEvent t m
                     , TriggerEvent t m
                     , MonadFix m
                     )
                  => CharacterOptions -> m ()
editCharacterPage charOpts0 = mdo
  let clickOutE = domEvent Click topLevel
  (topLevel, _) <- elClass' "div" "edit-page" $ mdo
    let pageLoadE = uncurry mkChoiceReq <$> submitChoiceE
    receivedNewCharOptsE <- fmap fromJust <$> postAndDecode pageLoadE
    charOptsDyn <- holdDyn charOpts0 receivedNewCharOptsE

    selectedLevelDyn <- sideNav charOptsDyn

    let selectedLevelOptsDyn = ffor2 charOptsDyn selectedLevelDyn $ \(CharacterOptions opts _) lvl ->
          fromJust $ lvl `Map.lookup` opts

    submitChoiceE <- (switchHold never =<<) $ dyn $ fmap (mainSection clickOutE) selectedLevelOptsDyn
    return ()

  return ()

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

-- TODO Rework this, this sucks.
mkChoiceReq :: OptionId -> SubmitChoice -> Text
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

sideNav :: forall m t. (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m)
        => Dynamic t CharacterOptions
        -> m (Dynamic t Int)
sideNav charOptsDyn = elClass "div" "side-nav" $ mdo
  opts0 <- sample $ current charOptsDyn

  selectLevelE <- (switchHold never =<<) $ dyn $ charOptsDyn
    <&> \(CharacterOptions optionsPerLevel curlvl) ->
          let maxlvl = maximum $ Map.keys optionsPerLevel
          in fmap leftmost $ sequence [sideNavButton selectedLevelDyn curlvl l | l <- [maxlvl,maxlvl-1..1]]

  selectedLevelDyn <- holdDyn (char_level opts0) selectLevelE

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

class (DomBuilder t m, MonadHold t m, PostBuild t m , TriggerEvent t m, MonadFix m) => ReactiveM t m
instance (DomBuilder t m, MonadHold t m, PostBuild t m , TriggerEvent t m, MonadFix m) => ReactiveM t m

mainSection :: ReactiveM t m
            => Event t ()
            -> [Option]
            -> m (Event t (OptionId, SubmitChoice))
mainSection clickOutE options = elClass "div" "main-section"
  $ fmap leftmost
  $ mapM (uncurry $ originCategoryWidget clickOutE) originCategories
  where
    originCategories = map (\((_,k),v) -> (k,v))
      $ Map.assocs
      $ multiMapFromList
      $ zip (map optionsSortField options) options

    optionsSortField Option{display_origin_category, origin_category_index} =
      (origin_category_index, display_origin_category)

originCategoryWidget :: ReactiveM t m => Event t () -> Text -> [Option]
                     -> m (Event t (OptionId, SubmitChoice))
originCategoryWidget clickOutE cat opts = elClass "div" "origin-category" $ do
  let headerText = case cat of
        "init"     -> "Choose your background, class, and race:"
        "level up" -> "Level up:"
        _          -> "From " <> cat <> ":"
  el "h2" (text headerText)
  leftmost <$> mapM (optionWidget clickOutE) opts

optionWidget :: ReactiveM t m => Event t () -> Option -> m (Event t (OptionId, SubmitChoice))
optionWidget clickOutE Option{id, display_id, origin, spec, choice}
  = elClass "div" "options-section-style"
  $ do el "h3" (text display_id)
       case spec of
         ListSpec entries -> listSpecWidget
           clickOutE optionId entries (fmap atomic_choice choice)
         FromSpec unique num (ListSpec entries) -> fromSpecWidget
           clickOutE optionId unique num entries
             (map atomic_choice $ concatMap subchoices $ maybeToList choice)
         _ -> return never

  where
    optionId = OptionId origin id

fromSpecWidget :: ReactiveM t m
               => Event t () -> OptionId
               -> Unique -> Maybe Int -> [ListSpecEntry]
               -> [Text]
               -> m (Event t (OptionId, SubmitChoice))
fromSpecWidget clickOutE optionId unique limit entries choices = mdo
  -- Render a prefilled dropdown widget for each choice.
  overwriteChoiceEs <- map updated <$> mapM (mkDropdownWidget . Just) choices

  -- If there are fewer choices than the limit, or if there is no limit, render
  -- a dropdown widget that is not yet filled in.
  appendChoiceEs <- map updated
    <$> mapM mkDropdownWidget [Nothing | fromMaybe True ((length choices <) <$> limit)]

  -- If there is a limit, create inert, greyed-out "dropdowns" as placeholders for the
  -- remaining choices.
  replicateM (fromMaybe 0 ((\num -> num - length choices - 1) <$> limit))
    $ elClass "div" "dropdown dropdown-disabled" (button "...")

  return
    $ fmap (optionId,)
    $ leftmost
    $ zipWith fmap (choiceEditFunctions choices) (overwriteChoiceEs <> appendChoiceEs)

  where mkDropdownWidget = customDropdownWidget clickOutE (map opt entries)

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

listSpecWidget :: ReactiveM t m => Event t () -> OptionId -> [ListSpecEntry] -> Maybe Text
               -> m (Event t (OptionId, SubmitChoice))
listSpecWidget clickOutE optionId entries choice =
  updated . fmap inform <$> customDropdownWidget clickOutE (map opt entries) choice
  where
    inform (Just choice) = (optionId, SubmitSingletonChoice choice)
    inform Nothing       = (optionId, RetractChoice)


customDropdownWidget :: forall t m. ReactiveM t m
                     => Event t () -> [Text] -> Maybe Text -> m (Dynamic t (Maybe Text))
customDropdownWidget clickOutE options selected0 = mdo
  elAttr "div" (Map.fromList [ ("class", "dropdown dropdown-enabled")
                             , ("onclick", "event.stopPropagation();")
                             ]) $ mdo
    selectedDyn <- holdDyn selected0 $ traceEvent "selectE" selectE
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

customDropdownEntryWidget :: ReactiveM t m => Maybe Text -> m (Event t (Maybe Text))
customDropdownEntryWidget option = do
  (buttonEl, _) <- elClass' "button" "dropdown-entry"
                   $ text $ fromMaybe "-- clear selection --" option
  return (option <$ domEvent Click buttonEl)

{-
getAndDecode :: (MonadIO m, MonadJSM (Performable m), PerformEvent t m, TriggerEvent t m, FromJSON a)
             => Event t Text -> m (Event t (Maybe a))
-}
