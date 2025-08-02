module Page.Sheet where

--------------------------------------------------------------------------------
import Control.Monad
import Data.Char (chr)
import Data.List (sortOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Reflex.Dom

import Widget
import Constants (charName)
import Types
import Types.Ability
import Util
--------------------------------------------------------------------------------

load :: ReactiveIOM t m => m ()
load = void $ loadWidget () (xhrRequest "GET" url def) page
  where url = "/api/character/" <> charName <> "/sheet"

-- TODO: can probably use a more restricted type class as the sheet is currently
-- non-interactive.
page :: DomBuilder t m => CharacterSheet -> m ()
page sheet@CharacterSheet
  { cs_hit_dice, cs_notable_traits, cs_weapons, cs_armor, cs_languages
  , cs_tools, cs_resistances, cs_spellcasting_sections, cs_spell_slots, cs_pact_magic
  , cs_resources, cs_ability_table, cs_skill_table
  } = do
  -- First page: name, class, race, abilities, skills, attacks, hit dice, other
  -- stats.
  divcl "page" $ do
    divcl "abilities" (abilityTableWidget cs_ability_table cs_skill_table)
    divcl "main-body" (mainBodyWidget sheet)
    divcl "hit-dice-section" (hitDiceSectionWidget cs_hit_dice)
  divcl "page-break" blank

  -- Second page: notable traits, non-skill proficiencies, resistances,
  -- spellcasting stats, spell slots, other finite resources.
  divcl "page" $ do
    divcl "column" $ do
      notableTraitsWidget cs_notable_traits
      otherProficienciesWidget cs_weapons cs_armor cs_languages cs_tools cs_resistances
    divcl "column" $ do
      spellcastingTableWidget cs_spellcasting_sections
      spellSlotsWidget cs_spell_slots
      pactMagicWidget cs_pact_magic
    divcl "column" $ do
      resourcesWidget cs_resources

-- First page
-- ----------
abilityTableWidget :: DomBuilder t m => AbilityTable -> SkillTable -> m ()
abilityTableWidget abilityTable skillTable = el "table" $ do
  mapM_ (elClass "tr" "ability-row" . abilityRowWidget) skillsPerAbility
  where
    abilityRowWidget (ability, skills) = case ability `Map.lookup` abilityTable of
      Nothing -> error $ "ability table does not contain key " <> Text.unpack ability
      Just AbilityTableEntry{ate_base, ate_total_bonus, ate_score, ate_mod, ate_st, ate_st_prof} -> do
        el "td" $ divcl "ability" $ do
          divcl "ability-name" (text ability)
          divcl "ability-modifier" $ do
            text (formatModifier ate_mod)
            el "hr" blank
            divcl "ability-score" (showWidget ate_score)

        elClass "td" "skill-td" $ el "table" $ do
          stRowWidget "saving throw" (formatModifier ate_st) ate_st_prof
          mapM_ skillTableRowWidget skills

    skillTableRowWidget skill = case skill `Map.lookup` skillTable of
      Nothing -> error $ Text.unpack skill <> " not in skill table"
      Just SkillTableEntry{ste_score, ste_proficient} ->
        stRowWidget skill (formatModifier ste_score) ste_proficient

stRowWidget :: DomBuilder t m => Text -> Text -> Bool -> m ()
stRowWidget label modifier bold = el "tr" $ do
  let emphasis | bold      = "style" |-> "font-weight: bold;"
               | otherwise = Map.empty
  elAttr "td" emphasis (text modifier)
  elAttr "td" emphasis (text label)

mainBodyWidget :: DomBuilder t m => CharacterSheet -> m ()
mainBodyWidget CharacterSheet{cs_name, cs_summary, cs_ac_formulas, cs_attacks} =
  let CharacterSummary{csm_classes, csm_race, csm_level, csm_maxhp} = cs_summary
  in do
    divcl "charname" $ do
      el "h1" (text cs_name)
      divcl "race-and-classes" (text $ csm_race <>  " — " <> csm_classes)
      divcl "charlevel" (domShow csm_level)

    divcl "badges" $ do
      badgeWidget "hit points" "hp" (hitpointsBadgeContentWidget csm_maxhp)
      badgeWidget "armor class" "ac" (armorClassContentWidget cs_ac_formulas)
      badgeWidget "stats" "stat-table" (statTableContentWidget cs_summary)

    divcl "attacks attacks-positioning" $ do
      divcl "badge-title" (text "attacks")
      el "table" $ do
        el "tr" $ mapM_ (el "th" . text) ["Attack", "To Hit/DC", "Damage", "Range", "Notes"]
        mapM_ (el "tr" . attackTableRowWidget) (take 5 cs_attacks)
  where
    attackTableRowWidget Attack{att_name, att_range, att_to_hit_or_dc, att_damage, att_notes} =
      mapM_ (el "td" . text) [att_name, att_range, att_to_hit_or_dc, att_damage, att_notes]

hitDiceSectionWidget :: DomBuilder t m => [HitDice] -> m ()
hitDiceSectionWidget hitDice = do
  divcl "badge-title" (text "hd")
  divcl "hit-dice" $ mapM_ hitDiceWidget $ sortOn hd_d $ hitDice
  where
    hitDiceWidget (HitDice n d) = replicateM n
      $ elAttr "img" ("src" |-> "/static/icons/d" <> showText d <> ".svg") blank

-- Second page
-- -----------
notableTraitsWidget :: DomBuilder t m => [NotableTraitCategory] -> m ()
notableTraitsWidget categories = do
  badgeWidget "notable traits" "notable-traits"
    $ mapM_ notableTraitCategoryWidget
    $ sortOn (negate . length . ntc_traits) categories

  where
    notableTraitCategoryWidget (NotableTraitCategory category traits) = el "div" $ do
      el "h3" (text category)
      el "ul" $ mapM_ traitWidget $ filter (not . trait_seminotable) $ traits

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
  el "tr" $ el "td" blank >> mapM_ (el "th" . text . ss_origin_shorthand) sections
  sequence_ $ zipWith mkRow ["DC", "mod", "prep", "abi"] spellcastingSectionRowProjections
  where
    mkRow header proj = el "tr" $ do
      el "th" (text header)
      mapM_ (el "td" . text . proj) sections

spellcastingSectionRowProjections :: [SpellcastingSection -> Text]
spellcastingSectionRowProjections
  = [ showText . ss_spell_save_dc
    , formatModifier . ss_spell_attack_mod
    , fromMaybe "-" . fmap showText . ss_max_prepared_spells
    , Text.toUpper . ss_spellcasting_ability
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
resourceWidget Resource{rsc_name, rsc_number, rsc_restore} = el "div" $ do
  el "h3" (text rsc_name)
  divcl "resource-details" (slotsWidget >> restoreInfoWidget)
  where
    slotsWidget | rsc_number <= 8 = replicateM_ rsc_number slotWidget
                | otherwise       = divcl "row" $ do
                    smallBlankWidget
                    text (nbsp <> "/" <> nbsp <> showText rsc_number)

    restoreInfoWidget = el "table" $ mapM_ restoreInfoLine (Map.assocs rsc_restore)

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
    acFormulaWidget AcFormula{ac_name, ac_ac, ac_shield} = divcl "ac-formula" $ do
      divcl "ac-formula-name" $ text $ "▢ " <> ac_name
      divcl "row" $ do
        divcl "labeled-flex" $ do
          divcl "filled-in" $ el "div" $ showWidget ac_ac
          el "div" $ text "base"
        case ac_shield of
          Nothing -> blank
          Just shieldAc -> do
            plusWidget
            divcl "labeled-flex" $ do
              divcl "filed-in" $ showWidget shieldAc
              el "div" $ text "shield"

statTableContentWidget :: DomBuilder t m => CharacterSummary -> m ()
statTableContentWidget CharacterSummary{csm_speed, csm_initiative, csm_prof_bon, csm_pp} = el "table" $ do
  el "tr" $ el "th" (text "speed") >> el "td" speedsWidget
  el "tr" $ el "th" (text "initiative") >> el "td" (modifierWidget csm_initiative)
  el "tr" $ el "th" (text "proficiency bonus") >> el "td" (modifierWidget csm_prof_bon)
  el "tr" $ el "th" (text "passive perception") >> el "td" (showWidget csm_pp)

  where
    speedsWidget = case csm_speed of
      [Speed {speed_speed}] -> text $ showText speed_speed <> " ft"
      _ -> elClass "ul" "multiple-speeds-list" $ mapM_ speedWidget csm_speed

    speedWidget Speed{speed_mode, speed_speed} = el "li" $ text $
      speed_mode <> ": " <> showText speed_speed <> " ft"

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
