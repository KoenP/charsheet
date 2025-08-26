module Page.Sheet where

--------------------------------------------------------------------------------
import Control.Monad
import Data.Char (chr)
import Data.List (sortOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Optics
import Reflex.Dom

import Widget
import Constants (charName)
import Types
import Types.Ability
import Types.Cache
import Util
--------------------------------------------------------------------------------

load :: ReactiveIOM t m => Maybe CharacterSheet -> m (Event t (Cache -> Cache))
load (Just sheet) = page sheet >> return never
load Nothing      = loadWidget (xhrRequest "GET" ("/api/character/" <> charName <> "/sheet") def) $ \sheet -> do
  initCacheE <- fmap (set #sheet (Just sheet) <$) getPostBuild
  page sheet
  return initCacheE

-- TODO: can probably use a more restricted type class as the sheet is currently
-- non-interactive.
page :: DomBuilder t m => CharacterSheet -> m ()
page sheet@CharacterSheet
  { hit_dice, notable_traits, weapons, armor, languages
  , tools, resistances, spellcasting_sections, spell_slots, pact_magic
  , resources, ability_table, skill_table
  } = do
  -- First page: name, class, race, abilities, skills, attacks, hit dice, other
  -- stats.
  divcl "page" $ do
    divcl "abilities" (abilityTableWidget ability_table skill_table)
    divcl "main-body" (mainBodyWidget sheet)
    divcl "hit-dice-section" (hitDiceSectionWidget hit_dice)
  divcl "page-break" blank

  -- Second page: notable traits, non-skill proficiencies, resistances,
  -- spellcasting stats, spell slots, other finite resources.
  divcl "page" $ do
    divcl "column" $ do
      notableTraitsWidget notable_traits
      otherProficienciesWidget weapons armor languages tools resistances
    divcl "column" $ do
      spellcastingTableWidget spellcasting_sections
      spellSlotsWidget spell_slots
      pactMagicWidget pact_magic
    divcl "column" $ do
      resourcesWidget resources

-- First page
-- ----------
abilityTableWidget :: DomBuilder t m => AbilityTable -> SkillTable -> m ()
abilityTableWidget abilityTable skillTable = el "table" $ do
  mapM_ (elClass "tr" "ability-row" . abilityRowWidget) skillsPerAbility
  where
    abilityRowWidget (ability, skills) = case ability `Map.lookup` abilityTable of
      Nothing -> error $ "ability table does not contain key " <> Text.unpack ability
      Just AbilityTableEntry{base, total_bonus, score, mod, st, st_prof} -> do
        el "td" $ divcl "ability" $ do
          divcl "ability-name" (text ability)
          divcl "ability-modifier" $ do
            text (formatModifier mod)
            el "hr" blank
            divcl "ability-score" (showWidget score)

        elClass "td" "skill-td" $ el "table" $ do
          stRowWidget "saving throw" (formatModifier st) st_prof
          mapM_ skillTableRowWidget skills

    skillTableRowWidget skill = case skill `Map.lookup` skillTable of
      Nothing -> error $ Text.unpack skill <> " not in skill table"
      Just SkillTableEntry{score, proficient} ->
        stRowWidget skill (formatModifier score) proficient

stRowWidget :: DomBuilder t m => Text -> Text -> Bool -> m ()
stRowWidget label modifier bold = el "tr" $ do
  let emphasis | bold      = "style" |-> "font-weight: bold;"
               | otherwise = Map.empty
  elAttr "td" emphasis (text modifier)
  elAttr "td" emphasis (text label)

mainBodyWidget :: DomBuilder t m => CharacterSheet -> m ()
mainBodyWidget CharacterSheet{name, summary, ac_formulas, attacks} =
  let CharacterSummary{classes, race, level, maxhp} = summary
  in do
    divcl "charname" $ do
      el "h1" (text name)
      divcl "race-and-classes" (text $ race <>  " — " <> classes)
      divcl "charlevel" (domShow level)

    divcl "badges" $ do
      badgeWidget "hit points" "hp" (hitpointsBadgeContentWidget maxhp)
      badgeWidget "armor class" "ac" (armorClassContentWidget ac_formulas)
      badgeWidget "stats" "stat-table" (statTableContentWidget summary)

    divcl "attacks attacks-positioning" $ do
      divcl "badge-title" (text "attacks")
      el "table" $ do
        el "tr" $ mapM_ (el "th" . text) ["Attack", "To Hit/DC", "Damage", "Range", "Notes"]
        mapM_ (el "tr" . attackTableRowWidget) (take 5 attacks)
  where
    attackTableRowWidget Attack{name, range, to_hit_or_dc, damage, notes} =
      mapM_ (el "td" . text) [name, range, to_hit_or_dc, damage, notes]

hitDiceSectionWidget :: DomBuilder t m => [HitDice] -> m ()
hitDiceSectionWidget hitDice = do
  divcl "badge-title" (text "hd")
  divcl "hit-dice" $ mapM_ hitDiceWidget $ sortOn (view #d) $ hitDice
  where
    hitDiceWidget (HitDice n d) = replicateM n
      $ elAttr "img" ("src" |-> "/static/icons/d" <> showText d <> ".svg") blank

-- Second page
-- -----------
notableTraitsWidget :: DomBuilder t m => [NotableTraitCategory] -> m ()
notableTraitsWidget categories = do
  badgeWidget "notable traits" "notable-traits"
    $ mapM_ notableTraitCategoryWidget
    $ sortOn (negate . length . view #traits) categories

  where
    notableTraitCategoryWidget (NotableTraitCategory category traits) = el "div" $ do
      el "h3" (text category)
      el "ul" $ mapM_ traitWidget $ filter (not . view #seminotable) $ traits

    traitWidget (Trait name _ ref _) = el "li" $ do
      text name
      whenJust ref $ \refVal -> el "sub" (text (" " <> refVal))

otherProficienciesWidget :: DomBuilder t m
                         => [Text] -> [Text] -> [Text] -> [Tool] -> [Resistance] -> m ()
otherProficienciesWidget weapons armor languages tools resistances =
  badgeWidget "other proficiencies" "other-proficiencies"
  $ mapM_ profListWidget [ ("weapons", weapons)
                         , ("armor", armor)
                         , ("languages", languages)
                         , ("tools", map showTool tools)
                         , ("resistances", map showResistance resistances)
                         ]
  where
    profListWidget (category, list) = do
      el "h3" (text category)
      divcl "details" $ text (showProfList list)

    showProfList [] = "-"
    showProfList items =  Text.intercalate ", " items

    showTool (Tool tool expertise) = tool <> if expertise then " (expertise)" else ""
    showResistance (Resistance resType resRes) = Text.concat [resType, " (", resRes, ")"]

spellcastingTableWidget :: DomBuilder t m => [SpellcastingSection] -> m ()
spellcastingTableWidget [] = blank
spellcastingTableWidget sections = badgeWidget "spellcasting" "spellcasting"
  $ elClass "table" "spellcasting-table"
  $ case sections of
      [section] -> singleSectionSpellcastingTableContentWidget section
      _         -> multiSectionSpellcastingTableContentWidget sections

singleSectionSpellcastingTableContentWidget :: DomBuilder t m => SpellcastingSection -> m ()
singleSectionSpellcastingTableContentWidget section = sequence_
  $ zipWith mkRow ["save DC", "attack mod", "prepared", "ability"] spellcastingSectionRowProjections
  where
    mkRow header proj = el "tr" $ el "th" (text header) >> el "td" (text $ proj section)

multiSectionSpellcastingTableContentWidget :: DomBuilder t m => [SpellcastingSection] -> m ()
multiSectionSpellcastingTableContentWidget sections = do
  el "tr" $ el "td" blank >> mapM_ (el "th" . text . view #origin_shorthand) sections
  sequence_ $ zipWith mkRow ["DC", "mod", "prep", "abi"] spellcastingSectionRowProjections
  where
    mkRow header proj = el "tr" $ do
      el "th" (text header)
      mapM_ (el "td" . text . proj) sections

spellcastingSectionRowProjections :: [SpellcastingSection -> Text]
spellcastingSectionRowProjections
  = [ showText . view #spell_save_dc
    , formatModifier . view #spell_attack_mod
    , fromMaybe "-" . fmap showText . view #max_prepared_spells
    , Text.toUpper . view #spellcasting_ability
    ]

spellSlotsWidget :: DomBuilder t m => [Int] -> m ()
spellSlotsWidget [] = blank
spellSlotsWidget slots = badgeWidget "spell slots" "badge-content spell-slots"
  $ el "table" $ sequence_ $ zipWith mkRow [1::Int ..] slots
  where
    mkRow slotLevel count = el "tr" $ do
      el "th" (showWidget slotLevel)
      el "td" (replicateM_ count slotWidget)

pactMagicWidget :: DomBuilder t m => Maybe PactMagic -> m ()
pactMagicWidget Nothing = blank
pactMagicWidget (Just (PactMagic count level)) = badgeWidget "pact magic" "badge-content spell-slots"
  $ el "table" $ el "tr"
  $ do el "th" $ showWidget level
       el "td" $ replicateM_ count slotWidget

slotWidget :: DomBuilder t m => m ()
slotWidget = elAttr "input" attrs blank
  where attrs = Map.fromList [("type", "checkbox"), ("class", "spell-slot")]

resourcesWidget :: DomBuilder t m => [Resource] -> m ()
resourcesWidget [] = blank
resourcesWidget resources =
  badgeWidget "resources" "resources spell-slots" $ mapM_ resourceWidget resources

resourceWidget :: DomBuilder t m => Resource -> m ()
resourceWidget Resource{name, number, restore} = el "div" $ do
  el "h3" (text name)
  divcl "resource-details" (slotsWidget >> restoreInfoWidget)
  where
    slotsWidget | number <= 8 = replicateM_ number slotWidget
                | otherwise       = divcl "row" $ do
                    smallBlankWidget
                    text (nbsp <> "/" <> nbsp <> showText number)

    restoreInfoWidget = el "table" $ mapM_ restoreInfoLine (Map.assocs restore)

    restoreInfoLine (condition, restoreInfo) = el "tr" $ do
      el "td" $ text (condition <> ":")
      el "td" $ text restoreInfo

smallBlankWidget :: DomBuilder t m => m ()
smallBlankWidget = divcl "small-blank" blank

-- Badges
-- ------
hitpointsBadgeContentWidget :: DomBuilder t m => Int -> m ()
hitpointsBadgeContentWidget maxHp = do
  divcl "row" $ do
    labeledFlexTopWidget "current" (divcl "blank" blank)
    plusWidget
    labeledFlexTopWidget "temp" (divcl "blank" blank)

  el "hr" blank

  divcl "row" $ do
    labeledFlexBotWidget "max hp" $ divcl "filled-in" (showWidget maxHp)
    plusWidget
    labeledFlexBotWidget "bonus max hp" (divcl "blank" blank)

armorClassContentWidget :: DomBuilder t m => [AcFormula] -> m ()
armorClassContentWidget = divcl "column" . mapM_ acFormulaWidget
  where
    acFormulaWidget AcFormula{name, ac, shield} = divcl "ac-formula" $ do
      divcl "ac-formula-name" $ text $ "▢ " <> name
      divcl "row" $ do
        divcl "labeled-flex" $ do
          divcl "filled-in" $ el "div" $ showWidget ac
          el "div" $ text "base"
        case shield of
          Nothing -> blank
          Just shieldAc -> do
            plusWidget
            divcl "labeled-flex" $ do
              divcl "filed-in" $ showWidget shieldAc
              el "div" $ text "shield"

statTableContentWidget :: DomBuilder t m => CharacterSummary -> m ()
statTableContentWidget CharacterSummary{speed, initiative, prof_bon, pp} = el "table" $ do
  el "tr" $ el "th" (text "speed") >> el "td" speedsWidget
  el "tr" $ el "th" (text "initiative") >> el "td" (modifierWidget initiative)
  el "tr" $ el "th" (text "proficiency bonus") >> el "td" (modifierWidget prof_bon)
  el "tr" $ el "th" (text "passive perception") >> el "td" (showWidget pp)

  where
    speedsWidget = case speed of
      [Speed {speed}] -> text $ showText speed <> " ft"
      _ -> elClass "ul" "multiple-speeds-list" $ mapM_ speedWidget speed

    speedWidget Speed{mode, speed} = el "li" $ text $
      mode <> ": " <> showText speed <> " ft"

-- Auxiliary widgets for badges
-- ----------------------------
plusWidget :: DomBuilder t m => m ()
plusWidget = el "div" $ text (nbsp <> "+" <> nbsp)

nbsp :: Text
nbsp = Text.singleton (chr 160)

labeledFlexTopWidget, labeledFlexBotWidget :: DomBuilder t m => Text -> m () -> m ()
labeledFlexTopWidget label widget = divcl "labeled-flex" $ do
  el "div" (text label)
  widget
labeledFlexBotWidget label widget = divcl "labeled-flex" $ do
  widget
  el "div" (text label)

badgeWidget :: DomBuilder t m => Text -> Text -> m () -> m ()
badgeWidget title contentClass contentWidget = divcl "badge" $ do
  divcl "badge-title" (text title)
  divcl contentClass contentWidget
