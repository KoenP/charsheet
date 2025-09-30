module Page.Cards where

--------------------------------------------------------------------------------
import Control.Monad
import Control.Monad.Fix
import Data.Either
import Data.Maybe
import Reflex.Dom
import Reflex.Host.Class
import Data.Map (Map)
import qualified Data.Map as Map
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

type CardPageM t m = (DomBuilder t m)
-- type CardPageM t m = ( DomBuilder t m, MonadJSM m, MonadJSM (Performable m), DomBuilderSpace m ~ GhcjsDomSpace
--                      , Ref m ~ Ref IO, Ref (Performable m) ~ GHC.Internal.IORef.IORef, MonadRef m, MonadRef (Performable m)
--                      , HasDocument m, TriggerEvent t m, PerformEvent t m, PostBuild t m
--                      , MonadReflexCreateTrigger t m, MonadHold t m, MonadSample t (Performable m), MonadFix m
--                      )

page :: CardPageM t m => Maybe CharacterSheet -> CharacterSheet -> m ()
page maybeOldSheet sheet = do
  gotoCardSelectPageE <- button "Configure"

  let (traitCategories, spellcastingSections) = (sheet ^. #notable_traits, sheet ^. #spellcasting_sections)

  divcl "cards"
    $ sequence $ concat $ chunks 8
    $ concatMap notableTraitCategoryWidgets traitCategories <> concatMap spellcastingSectionCardWidgets spellcastingSections

  return ()

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

notableTraitCategoryWidgets :: CardPageM t m => NotableTraitCategory -> [m ()]
notableTraitCategoryWidgets NotableTraitCategory{ category, traits } = concatMap (notableTraitCardsWidget category) traits

notableTraitCardsWidget :: CardPageM t m => Text -> Trait -> [m ()]
notableTraitCardsWidget category Trait{name, desc, ref, seminotable}
  = zipWith (\i page -> notableTraitCardWidget (mkCardTitle name i) ref page) [1..] pages
  where
    pages = fromMaybe [] desc
    mkCardTitle name i | [_] <- pages = name
                       | otherwise    = name <> " (" <> showText i <> "/" <> showText (length pages) <> ")"

notableTraitCardWidget :: CardPageM t m => Text -> Maybe Text -> Text -> m ()
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
