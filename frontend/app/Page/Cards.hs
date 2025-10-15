module Page.Cards where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader
import Data.Bool
import qualified Data.ByteString
-- import Data.ByteString.Lazy.Internal (unpackChars)
import Data.Either
import Data.Maybe
import GHC.Generics (Generic)
import Reflex.Dom hiding ((.~))
import qualified Reflex.Dom
import Reflex.Host.Class
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Lazy (toStrict)
import Data.Tuple
import Optics
import Language.Javascript.JSaddle.Monad (MonadJSM)

import Data.Aeson

import Debug.Trace
import Constants (charName)
import Types
import Types.Ability
import Types.Cache
import Util
import Widget
import Widget.Dropdown
import Text.Markdown
--------------------------------------------------------------------------------

load :: ReactiveIOM t m => Maybe CharacterSheet -> Maybe CardConfig -> m (Event t (Cache -> Cache))
load (Just sheet) (Just config) = page Nothing sheet config
load maybeSheet   maybeConfig   = do

  -- TODO replace with performRequestsAsync
  -- TODO error handling
  let performReqs = do

        sheetReqE  :: Event t CharacterSheet <- case maybeSheet  of
          Just sheet  -> (sheet <$) <$> now
          Nothing     -> performReq $ xhrRequest "GET" ("/api/character/" <> charName <> "/sheet") def

        configReqE :: Event t CardConfig <- fmap (fromMaybe defaultCardConfig) <$> case maybeConfig of
          Just config -> (Just config <$) <$> now
          Nothing     -> performReq $ xhrRequest "GET" ("/api/character/" <> charName <> "/store/card-config") def

        combineLatest sheetReqE configReqE

  (initE, cacheE) <- loadWidget' performReqs (\(sheet, config) -> page Nothing sheet config)
  
  return $ leftmost
    [ (\(sheet, config) cache -> cache{sheet = Just sheet, cardConfig = Just config}) <$> initE
    , cacheE
    ]


-- TODO this doesn't work if both events fire at the same time.
--      I think aBh and bBh are one frame behind aE and bE
combineLatest :: ReactiveM t m => Event t a -> Event t b -> m (Event t (a, b))
combineLatest aE bE = do
  aBh <- hold Nothing (Just <$> aE)
  bBh <- hold Nothing (Just <$> bE)
  return $ leftmost [ attachWithMaybe (\mb a -> fmap (a,) mb) bBh aE
                    , attachWithMaybe (\ma b -> fmap (,b) ma) aBh bE
                    ]


type CardPageM t m = (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m)

type WithCardConfigM t m = (CardPageM t m, MonadReader CardConfig m)

type EditCardConfigM t m = (ReactiveM t m, MonadReader (CardConfigCtx t) m , EventWriter t (Endo CardConfig) m)

data CardConfigCtx t = CardConfigCtx { cardConfig :: CardConfig
                                     , clickOutE  :: Event t ()
                                     } deriving Generic
instance Reflex t => DropdownCtx (CardConfigCtx t) t where
  getLockDyn _ = constDyn False
  getClickOutE = view #clickOutE


page :: forall t m. ReactiveIOM t m => Maybe CharacterSheet -> CharacterSheet -> CardConfig -> m (Event t (Cache -> Cache))
page maybeOldSheet sheet cardConfig0 = mdo
  let clickOutE = domEvent Click topLevel
  (topLevel, cardConfigDyn) <- elClass' "div" "card-page" $ mdo

    let
      cardsWidgetDyn, configWidgetDyn :: Dynamic t (m (Event t (CardConfig -> CardConfig)))
      cardsWidgetDyn  = (never <$) . cardsWidget maybeOldSheet sheet <$> cardConfigDyn
      configWidgetDyn = cardConfigPageWidget clickOutE sheet <$> cardConfigDyn

    updateCardConfigE :: Event t (CardConfig -> CardConfig) <- switchHold never
      =<< toggleWidget "Configure" "Show cards" cardsWidgetDyn configWidgetDyn

    cardConfigDyn <- foldDyn ($) cardConfig0 updateCardConfigE

    return cardConfigDyn

  let
    postConfigUrl = "/api/character/" <> charName <> "/store/card-config"
    postConfigRequest conf = XhrRequest "POST" postConfigUrl $ def
      & xhrRequestConfig_sendData Reflex.Dom..~ Text.decodeUtf8 (Data.ByteString.toStrict $ encode conf)
      & xhrRequestConfig_responseType Reflex.Dom..~ Just XhrResponseType_Text

  -- TODO inform user if the request goes wrong.
  _ <- performRequestAsync (postConfigRequest <$> updated cardConfigDyn)

  return $ set #cardConfig . Just <$> updated cardConfigDyn

-- | Render a button that switches between two dynamic sources of widgets.
--   Returns an event 
toggleWidget :: forall t m a. ReactiveM t m => Text -> Text -> Dynamic t (m a) -> Dynamic t (m a) -> m (Event t a)
toggleWidget label0 label1 w0 w1 = mdo
  toggleE <- switchHold never =<< dyn (button . bool label0 label1 <$> toggleDyn)
  toggleDyn <- toggle False toggleE
  dyn $ bool w0 w1 =<< toggleDyn

--------------------------------------------------------------------------------
-- Card config sub-page
--------------------------------------------------------------------------------
colorSchemeDropdownEntries :: [DropdownEntry]
colorSchemeDropdownEntries = [ DropdownEntry scheme [] True ["colorscheme-" <> scheme]
                             | scheme <- ["blue", "red", "green", "yellow", "purple"]
                             ]

cardConfigPageWidget :: ReactiveM t m => Event t () -> CharacterSheet -> CardConfig
                     -> m (Event t (CardConfig -> CardConfig))
cardConfigPageWidget clickOutE sheet cardConfig = run $ divcl ["card-config-page"] $ do
  globalCardConfigWidget
  mapM_ categoryConfigWidget $ mergeTraitAndSpellCategories (sheet ^. #notable_traits) (sheet ^. #spellcasting_sections)

  where
    run = fmap (fmap appEndo . snd) . runEventWriterT . flip runReaderT (CardConfigCtx cardConfig clickOutE)

    mergeTraitAndSpellCategories :: [NotableTraitCategory] -> [SpellcastingSection]
                                 -> [(Category, [Trait], [Spell])]
    mergeTraitAndSpellCategories traitCategories spellcastingSections =
      mergeCategories [(category, traits) | NotableTraitCategory {category, traits} <- traitCategories     ]
                      [(origin  , spells) | SpellcastingSection  {origin  , spells} <- spellcastingSections]

    mergeCategories :: Ord k => [(k, [a])] -> [(k, [b])] -> [(k, [a], [b])]
    mergeCategories left right = go (sortOn fst left) (sortOn fst right)
      where
        go l                    []                   = [(xcat, xs, []) | (xcat, xs) <- l]
        go []                   r                    = [(ycat, [], ys) | (ycat, ys) <- r]
        go l@((xcat,xs) : lrem) r@((ycat,ys) : rrem) = case compare xcat ycat of
          EQ -> (xcat, xs, ys) : go lrem rrem
          LT -> (xcat, xs, []) : go lrem r
          GT -> (ycat, [], ys) : go l    rrem

globalCardConfigWidget :: forall t m. EditCardConfigM t m => m ()
globalCardConfigWidget = el "div" $ do
  checkboxWidget #showTraits "show-traits-checkbox" "Include features"
  checkboxWidget #showSpells "show-spells-checkbox" "Include spells"
  checkboxWidget #onlyShowChanges "only-show-changes" "Only show changes w.r.t. previous level"

categoryConfigWidget :: EditCardConfigM t m => (Category, [Trait], [Spell]) -> m ()
categoryConfigWidget (category, traits, spells) = asks (^. #cardConfig) >>= \config -> mkdiv config $ do
  let
    categoryIncluded = config ^. categoryIncludedLens category
    categoryHeaderClass = Text.concat ["omitted" | not categoryIncluded]

  elClass "h2" categoryHeaderClass $ do
    checkboxWidget (categoryIncludedLens category) ("show-category-" <> category <> "-checkbox") ("From " <> category <> ":")
    when categoryIncluded $ colorDropdownWidget (categoryColorSchemeLens category)

  when categoryIncluded $ do
    when (config ^. #showTraits && not (null traits)) $ do
      el "h4" (text "Features:")
      el "ul"
        $ mapM_ (\Trait{name} -> traitOrSpellConfigWidget
                    category name (traitIncludedLens category name) (#traitColorSchemes % at (category,name)))
        $ filter (^. #desc % to isJust) traits

    when (config ^. #showSpells && not (null spells)) $ do
      el "h4" (text "Spells:")
      el "ul"
        $ mapM_ (\Spell{name} -> traitOrSpellConfigWidget
                    category name (spellIncludedLens category name) (#spellColorSchemes % at (category,name)))
        $ spells


  where
    mkdiv config = divcl $ maybeColorSchemeToClass $ config ^. (categoryColorSchemeLens category)

traitOrSpellConfigWidget :: EditCardConfigM t m
                         => Text -> Text -> Lens' CardConfig Bool -> Lens' CardConfig (Maybe ColorScheme) -> m ()
traitOrSpellConfigWidget category name includedLens colorSchemeLens = asks (^. #cardConfig) >>= \config -> mkdiv config $ do
  let
    included = config ^. includedLens
    class_ = Text.concat ["omitted" | not included]
    identifier = category <> "-trait-" <> name <> "-checkbox"

  elClass "li" class_ $ do
    checkboxWidget includedLens identifier name
    when included $ colorDropdownWidget colorSchemeLens

  where
    mkdiv config = divcl $ maybeColorSchemeToClass $ config ^. colorSchemeLens

checkboxWidget :: forall k is t m. (Is k A_Getter, Is k A_Setter, EditCardConfigM t m)
                => Optic' k is CardConfig Bool -> Text -> Text -> m ()
checkboxWidget optic identifier label = do
  cardConfig0 <- asks (^. #cardConfig)
  inputEl <- inputElement $ def
    & inputElementConfig_initialChecked Reflex.Dom..~ (cardConfig0 ^. optic)
    & inputElementConfig_elementConfig.elementConfig_initialAttributes
        Reflex.Dom..~ ("type" |-> "checkbox" <> "id" |-> identifier)
  elAttr "label" ("for" |-> identifier) (text label)
  tellEvent $ Endo . set optic <$> _inputElement_checkedChange inputEl


colorDropdownWidget :: EditCardConfigM t m => Lens' CardConfig (Maybe ColorScheme) -> m ()
colorDropdownWidget lens = do
  cardConfig <- asks (^. #cardConfig)
  (selectedDyn, _) <- customDropdownWidget colorSchemeDropdownEntries (cardConfig ^. lens)
  tellEvent $ Endo . set lens <$> updated selectedDyn

--------------------------------------------------------------------------------
-- Cards sub-page
--------------------------------------------------------------------------------
cardsWidget :: CardPageM t m => Maybe CharacterSheet -> CharacterSheet -> CardConfig -> m ()
cardsWidget maybeOldSheet sheet config = flip runReaderT config $ divcl ["cards"]
  $ void $ sequence $ concat $ chunks 8
  $ concatMap notableTraitCategoryWidgets traitCategories <> concatMap spellcastingSectionCardWidgets spellcastingSections

  where
    traitCategories      | config ^. #showTraits = sheet
                                                   ^. #notable_traits
                                                   %  to (filter $ \NotableTraitCategory{category}
                                                           -> config ^. categoryIncludedLens category)
                         | otherwise             = []
    spellcastingSections | config ^. #showSpells = sheet
                                                   ^. #spellcasting_sections
                                                   %  to (filter $ \SpellcastingSection{origin}
                                                           -> config ^. categoryIncludedLens origin)
                         | otherwise             = []

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n l = case splitAt n l of (chunk, rem) -> chunk : chunks n rem

getCardSections :: Maybe CharacterSheet -> CharacterSheet -> ([NotableTraitCategory], [SpellcastingSection])
getCardSections Nothing         sheet = (sheet ^. #notable_traits, sheet ^. #spellcasting_sections)
getCardSections (Just oldSheet) sheet =
  ( diffTraitCategories (oldSheet ^. #notable_traits) (sheet ^. #notable_traits)
  , diffSpellcastingSections (oldSheet ^. #spellcasting_sections) (sheet ^. #spellcasting_sections)
  )
  where
    diffTraitCategories = undefined
    diffSpellcastingSections = undefined

notableTraitCategoryWidgets :: WithCardConfigM t m => NotableTraitCategory -> [m ()]
notableTraitCategoryWidgets NotableTraitCategory{ category, traits } = concatMap (notableTraitCardsWidget category) traits

notableTraitCardsWidget :: WithCardConfigM t m => Text -> Trait -> [m ()]
notableTraitCardsWidget category Trait{name, desc, ref, seminotable}
  = zipWith (\i page -> notableTraitCardWidget category name (mkCardTitle name i) ref page) [1..] pages
  where
    pages = fromMaybe [] desc
    mkCardTitle name i | [_] <- pages = name
                       | otherwise    = name <> " (" <> showText i <> "/" <> showText (length pages) <> ")"

notableTraitCardWidget :: WithCardConfigM t m => Text -> Text -> Text -> Maybe Text -> Text -> m ()
notableTraitCardWidget category name title ref page = do
  config <- ask
  let included = config ^. traitIncludedLens category name
      colorSchemeClass = maybeColorSchemeToClass $ traitCardColorScheme category name config

  when included $ divcl ("card" : colorSchemeClass) $ do
    divcl ["card-title-section"] $ divcl ["card-title"] $ text title
    divcl ["card-flexgrow"] blank
    traitDescriptionWidget page

traitDescriptionWidget :: CardPageM t m => Text -> m ()
traitDescriptionWidget desc =
  elAttr "div" (Map.fromList [("class", "card-description"), ("style", style)]) $ renderMarkdown desc
  where
    style = "font-size: " <> fontSize <> "px; line-height: " <> fontSize <> "px;"
    fontSize | descriptionContainsTable = "6"
             | otherwise = estimateFontSize (Text.length desc)
    descriptionContainsTable = "|---|" `Text.isInfixOf` desc

spellcastingSectionCardWidgets :: WithCardConfigM t m => SpellcastingSection -> [m ()]
spellcastingSectionCardWidgets SpellcastingSection{origin, spells} = map (spellCardWidget origin) spells

spellCardWidget :: WithCardConfigM t m => Origin -> Spell -> m ()
spellCardWidget origin spell@Spell{ name, ref, casting_time, components, duration, range, rolls, aoe
                                  , description, shortdesc, higher_level, bonuses, resources, level
                                  } = do
  config <- ask
  let included = config ^. spellIncludedLens origin name
      colorSchemeClass = maybeColorSchemeToClass $ spellCardColorScheme origin name config
  when included $ divcl ("card" : colorSchemeClass) $ do
    divcl ["card-title-section"] $ do
      divcl ["card-title"] $ text name
      divcl ["card-subtitle"] $ text (cardSubtitle spell <> fromMaybe "" (fmap (" · " <>) ref))
    divcl ["card-boxes-section"] $ do
      cardBoxWidget "action-cost-inverted" casting_time
      cardBoxWidget "components-inverted" (showComponents components)
      cardBoxWidget "rolls-inverted" (fromMaybe "-" rolls)
      cardBoxWidget "hourglass-inverted" duration
      cardBoxWidget "range-inverted" range
      cardBoxWidget "aoe-inverted" (fromMaybe "-" aoe)
    divcl ["card-flexgrow"] blank
    spellDescriptionWidget spellDescriptionText higher_level bonuses resources level

  where
    spellDescriptionText = fromMaybe description $ fmap ("(Summary:)" <>) shortdesc

spellDescriptionWidget :: CardPageM t m => Text -> Maybe Text -> [SpellBonus] -> [PrologTerm] -> Int -> m ()
spellDescriptionWidget desc higherLevel bonuses resources spellLevel =
  elAttr "div" (Map.fromList [("class", "card-description"), ("style", style)]) $ do
    -- elDynHtml' "div" (constDyn "<p><strong>Raw HTML content:</strong> This is injected!</p>")
    renderMarkdown desc

    -- text $ toStrict $ Lucid.renderText $ MMark.render $ fromRight (error "markdown parse failed") $ MMark.parse "" desc
  where
    style = "font-size: " <> fontSize <> "px; line-height: " <> fontSize <> "px;"
    fontSize | descriptionContainsTable = "6"
             | otherwise = estimateFontSize (Text.length desc + Text.length (fromMaybe "" higherLevel))
    descriptionContainsTable = "|---|" `Text.isInfixOf` desc

renderMarkdown :: forall t m. CardPageM t m => Text -> m ()
renderMarkdown = mapM_ go . errorOnLeft . parseMarkdown
  where
    go :: Block -> m ()
    go (ListBlock Numbers blocks) = el "ol" $ mapM_ (el "li" . mapM_ go) blocks
    go (ListBlock (Bullets _) blocks) = el "ul" $ mapM_ (el "li" . mapM_ go) blocks
    go (Leaf (Paragraphs paragraphs)) = mapM_ (el "p" . renderMarkdownText . concat) paragraphs
    go (Leaf (Table { headers , rows })) = el "table" $ do
      forM_ headers $ el "tr" . mapM_ (el "th" . renderMarkdownText)
      forM_ rows    $ el "tr" . mapM_ (el "td" . renderMarkdownText)
    go (Leaf (ATXHeading level heading)) = el ("h" <> showText level) $ renderMarkdownText heading
    go (Leaf ThematicBreak) = el "hr" blank

    renderMarkdownText :: MarkdownText -> m ()
    renderMarkdownText = mapM_ renderSnippet

    renderSnippet (Snippet Plain t) = text t
    renderSnippet (Snippet Bold t) = el "b" (text t)
-- mapM_ go (unCm cm)

--   where
--     cm :: Cm () Blocks
--     Just (Right cm) = parseCommonmarkWith defaultSyntaxSpec (tokenize "source" md)
-- 
-- 
--     go (Para inls) = undefined

estimateFontSize :: Int -> Text
estimateFontSize len = showText
  $ fromMaybe 14
  $ lookupLargestLeq len [(400, 12), (600,10), (900, 9), (1000, 8), (1400, 7), (1800, 6)]

type IconName = Text
cardBoxWidget :: CardPageM t m => IconName -> Text -> m ()
cardBoxWidget iconName content = divcl ["card-box"] $ icon iconName >> text content


icon :: CardPageM t m => IconName -> m ()
icon iconName = elAttr "img" (Map.fromList [ ("src", "/static/icons/" <> iconName <> ".png") ]) blank

cardSubtitle :: Spell -> Text
cardSubtitle Spell{level, school, ritual} = levelAndSchool <> case ritual of
  NotRitual  -> ""
  Ritual     -> " (ritual)"
  OnlyRitual -> " (only ritual)"
  where levelAndSchool | level == 0 = school <> " cantrip"
                       | otherwise  = Text.intercalate " " [Util.ordinal level, "level", school]

showComponents :: [Component] -> Text
showComponents = Text.intercalate ", " . map showComponent
  where
    showComponent V     = "V"
    showComponent S     = "S"
    showComponent (M _) = "M"
