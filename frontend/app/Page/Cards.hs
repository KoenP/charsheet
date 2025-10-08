module Page.Cards where

--------------------------------------------------------------------------------
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader
import Data.Bool
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
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Lazy (toStrict)
import Optics
-- import Commonmark
-- import Commonmark.Pandoc
-- import Text.Pandoc.Builder hiding (text)
import Language.Javascript.JSaddle.Monad (MonadJSM)

import Constants (charName)
import Types
import Types.Ability
import Types.Cache
import Util
import Widget
import Text.Markdown
--------------------------------------------------------------------------------

load :: ReactiveIOM t m => Maybe CharacterSheet -> m (Event t (Cache -> Cache))
load (Just sheet) = page Nothing sheet >> return never
load Nothing      = do
  (initE, _) <- loadWidget (xhrRequest "GET" ("/api/character/" <> charName <> "/sheet") def) (\sheet -> page Nothing sheet >> return never)
  return (set #sheet . Just <$> initE)

type CardPageM t m = (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m)
type WithCardConfigM t m = (CardPageM t m, MonadReader CardConfig m)

page :: forall t m. CardPageM t m => Maybe CharacterSheet -> CharacterSheet -> m ()
page maybeOldSheet sheet = mdo
  let
    cardsWidgetDyn, configWidgetDyn :: Dynamic t (m (Event t (CardConfig -> CardConfig)))
    cardsWidgetDyn  = (never <$) . cardsWidget maybeOldSheet sheet <$> cardConfigDyn
    configWidgetDyn = cardConfigPageWidget sheet <$> cardConfigDyn


  updateCardConfigE :: Event t (CardConfig -> CardConfig) <- switchHold never
    =<< toggleWidget "Configure" "Show cards" cardsWidgetDyn configWidgetDyn

  cardConfigDyn <- foldDyn ($) defaultCardConfig updateCardConfigE


  return ()

toggleWidget :: forall t m a. CardPageM t m => Text -> Text -> Dynamic t (m a) -> Dynamic t (m a) -> m (Event t a)
toggleWidget label0 label1 w0 w1 = mdo
  toggleE <- switchHold never =<< dyn (button . bool label0 label1 <$> toggleDyn)
  toggleDyn <- toggle False toggleE
  dyn $ bool w0 w1 =<< toggleDyn

--------------------------------------------------------------------------------
-- Card config sub-page
--------------------------------------------------------------------------------

type Category = Text
data ColorScheme = ColorScheme { bg :: Text, fg :: Text, name :: Text} deriving (Show, Generic)
data CardConfig = CardConfig
  { showSpells           :: Bool
  , showTraits           :: Bool
  , onlyShowChanges      :: Bool
  , excludedCategories   :: Set Category
  , categoryColorSchemes :: Map Category ColorScheme
  } deriving (Show, Generic)
defaultCardConfig :: CardConfig
defaultCardConfig = CardConfig
  { showSpells           = True
  , showTraits           = True
  , onlyShowChanges      = False
  , excludedCategories   = Set.empty
  , categoryColorSchemes = Map.empty
  }
type CardConfigE t = Event t (CardConfig -> CardConfig)

categoryIncludedLens ::  Text -> Optic' A_Lens NoIx CardConfig Bool
categoryIncludedLens category = #excludedCategories % contains category % iso not not

cardConfigPageWidget :: CardPageM t m => CharacterSheet -> CardConfig -> m (CardConfigE t)
cardConfigPageWidget sheet cardConfig = flip runReaderT cardConfig $ do
  e <- globalCardConfigWidget cardConfig
  e' <- mapM categoryConfigWidget $ mergeTraitAndSpellCategories (sheet ^. #notable_traits) (sheet ^. #spellcasting_sections)

  return (leftmost $ e : e')

  where
    mergeTraitAndSpellCategories :: [NotableTraitCategory] -> [SpellcastingSection]
                                 -> [(Category, [Trait], [Spell])]
    mergeTraitAndSpellCategories traitCategories spellcastingSections =
      mergeCategories [(category, traits) | NotableTraitCategory {category, traits} <- traitCategories]
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

globalCardConfigWidget :: forall t m. WithCardConfigM t m => CardConfig -> m (CardConfigE t)
globalCardConfigWidget cardConfig0 = el "div" $ fmap leftmost $ sequenceA
  [ checkboxWidget #showTraits "show-traits-checkbox" "Include features"
  , checkboxWidget #showSpells "show-spells-checkbox" "Include spells"
  , checkboxWidget #onlyShowChanges "only-show-changes" "Only show changes w.r.t. previous level"
  ]


categoryConfigWidget :: WithCardConfigM t m => (Category, [Trait], [Spell]) -> m (CardConfigE t)
categoryConfigWidget (category, traits, spells) = do
  CardConfig{excludedCategories} <- ask
  let
    categoryIncluded = category `Set.notMember` excludedCategories
    categoryHeaderClass = if categoryIncluded then "" else "omitted"

  elClass "h2" categoryHeaderClass $ checkboxWidget
    (categoryIncludedLens category)
    ("show-category-" <> category <> "-checkbox")
    ("From " <> category <> ":")

  -- el "div" $ el "h4" $ text "Features"
  -- return never

checkboxWidget :: (Is k A_Getter, Is k A_Setter, WithCardConfigM t m)
                => Optic' k is CardConfig Bool -> Text -> Text -> m (CardConfigE t)
checkboxWidget optic identifier label = do
  cardConfig0 <- ask
  inputEl <- inputElement $ def
    & inputElementConfig_initialChecked Reflex.Dom..~ (cardConfig0 ^. optic)
    & inputElementConfig_elementConfig.elementConfig_initialAttributes
        Reflex.Dom..~ ("type" |-> "checkbox" <> "id" |-> identifier)
  elAttr "label" ("for" |-> identifier) (text label)
  return $ (optic .~) <$> _inputElement_checkedChange inputEl


--------------------------------------------------------------------------------
-- Cards sub-page
--------------------------------------------------------------------------------


cardsWidget :: CardPageM t m => Maybe CharacterSheet -> CharacterSheet -> CardConfig -> m ()
cardsWidget maybeOldSheet sheet config = flip runReaderT config $ divcl "cards"
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
  = zipWith (\i page -> notableTraitCardWidget (mkCardTitle name i) ref page) [1..] pages
  where
    pages = fromMaybe [] desc
    mkCardTitle name i | [_] <- pages = name
                       | otherwise    = name <> " (" <> showText i <> "/" <> showText (length pages) <> ")"

notableTraitCardWidget :: WithCardConfigM t m => Text -> Maybe Text -> Text -> m ()
notableTraitCardWidget title ref page = divcl "card" $ do
  divcl "card-title-section" $ divcl "card-title" $ text title
  divcl "card-flexgrow" blank
  traitDescriptionWidget page

traitDescriptionWidget :: CardPageM t m => Text -> m ()
traitDescriptionWidget desc =
  elAttr "div" (Map.fromList [("class", "card-description"), ("style", style)]) $ do
    renderMarkdown desc
  where
    style = "font-size: " <> fontSize <> "px; line-height: " <> fontSize <> "px;"
    fontSize | descriptionContainsTable = "6"
             | otherwise = estimateFontSize (Text.length desc)
    descriptionContainsTable = "|---|" `Text.isInfixOf` desc

spellcastingSectionCardWidgets :: CardPageM t m => SpellcastingSection -> [m ()]
spellcastingSectionCardWidgets SpellcastingSection{origin, spells} = map (spellCardWidget origin) spells

spellCardWidget :: CardPageM t m => Origin -> Spell -> m ()
spellCardWidget origin spell@Spell{ name, ref, casting_time, components, duration, range, rolls, aoe
                                  , description, shortdesc, higher_level, bonuses, resources, level
                                  }
  = divcl "card" $ do
      divcl "card-title-section" $ do
        divcl "card-title" $ text name
        divcl "card-subtitle" $ text (cardSubtitle spell <> fromMaybe "" (fmap (" · " <>) ref))
      divcl "card-boxes-section" $ do
        cardBoxWidget "action-cost-inverted" casting_time
        cardBoxWidget "components-inverted" (showComponents components)
        cardBoxWidget "rolls-inverted" (fromMaybe "-" rolls)
        cardBoxWidget "hourglass-inverted" duration
        cardBoxWidget "range-inverted" range
        cardBoxWidget "aoe-inverted" (fromMaybe "-" aoe)
      divcl "card-flexgrow" blank
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
cardBoxWidget iconName content = divcl "card-box" $ icon iconName >> text content


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
