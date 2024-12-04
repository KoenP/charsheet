class_option(ranger).
hd_per_level(ranger, 1 d 10).
initial_class_base_hp(ranger, 10).
max_hp_per_level(ranger, 1 d 10).
caster(ranger, 1/2).
spellcasting_ability(ranger, wis).
choose_subclass_level(ranger:3).
class_saving_throw(ranger, str).
class_saving_throw(ranger, dex).
asi_level(ranger:L) :- default_asi_level(L).

class_skill_list(ranger, ['animal handling', athletics, insight,
                          investigation, nature, perception, stealth,
                          survival]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initial class features (don't get these multiclassing into ranger).
trait_options_source(^ranger, skill, wrap(skill),
                     2 unique_from class_skill(ranger)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 1 features (you get these when multiclassing into ranger).
traits_from_source(ranger >: 1,
                   [weapon(simple), weapon(martial),
                    armor(light), armor(medium), armor(shield)]).
trait_options_source(ranger >: 1, skill, wrap(skill),
                     class_skill(ranger)).
trait_options_source(ranger >: 1, 'favored enemy',
                     wrap(favored_enemy),
                     from_list(
                         [aberration, beast, celestial, construct,
                          dragon, elemental, fey, fiend, giant,
                          monstrosity, ooze, plant, undead
                         ])).
meta_todo('favored enemy', "select two races as favored enemies").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features from leveling up.
trait_options_source(ranger >: L, 'natural explorer',
                     wrap(natural_explorer),
                     from_list(
                         [arctic, coast, desert, forest, grassland,
                          mountain, swamp
                         ])) :-
    member(L, [1, 6, 10]).
trait_options_source(ranger >: 2, 'fighting style',
                     wrap(fighting_style),
                     from_list([archery, defense, dueling,
                                'two-weapon fighting'])).

trait_source(ranger >: 3, 'primeval awareness').
multiclass_trait_source(ranger >: 5, extra_attack(1)).
trait_source(ranger >: 8, 'land\'s stride').
trait_source(ranger >: 10, 'hide in plain sight').
trait_source(ranger >: 14, vanish).
trait_source(ranger >: 18, 'feral senses').
trait_source(ranger >: 20, 'foe slayer').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Spellcasting.

known_spell(ranger, wis, always, [slot], Ritual, Spell) :-
    class_level(ranger:L),
    selected_at_class_level(ranger:L, spell, Spell),
    spell_property(Spell, ritual, Ritual).

% Learn new spells.
options_source(ranger >: 2, spell,
               2 unique_from learnable_proper_spell(ranger)).
options_source(ranger >: L, spell, learnable_proper_spell(ranger)) :-
    between(3, 19, L),
    odd(L).

% Replace old spells.
options_source(ranger >: L, replace(spell),
               selected_at_class_level(ranger:Prev, spell)) :-
    between(3, 20, L),
    Prev is L-1.
options(ranger >: L, replacing(spell, Name),
        learnable_proper_spell(ranger)) :-
    choice_member(ranger >: L, replace(spell), Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RANGER ARCHETYPES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hunter
subclass_option(ranger, hunter).
trait_options_source(ranger(hunter) >: 3, 'hunter\'s prey',
                     wrap(hunters_prey),
                     from_list(['colossus slayer', 'giant killer',
                                'horde breaker'])).
trait_options_source(ranger(hunter) >: 7, 'defensive tactics',
                     wrap(defensive_tactics),
                     ['escape the horde', 'multiattack defense', 'steel will']).
trait_options_source(ranger(hunter) >: 11, multiattack,
                     wrap(multiattack), [volley, 'whirlwind attack']).
trait_options_source(ranger(hunter) >: 15,
                     'hunter\s superior defense',
                     wrap(hunters_superior_defense),
                     [evasion, 'stand against the tide', 'uncanny dodge']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Beast master
meta_todo(ranger('beast master'), "currently only have primal companion").
subclass_option(ranger, 'beast master').

trait_source(ranger('beast master') >: 3, 'primal companion').
trait_source(trait('primal companion'),
             beast_of_the_land(ac(AC), hp(HP), pb(ProfBon), to_hit(ToHit),
                               damage(1 d 8 + DamageBonus))) :-
    proficiency_bonus(ProfBon),
    class_level(ranger:RangerLevel),
    AC is 13 + ProfBon,
    HP is 5 + 5*RangerLevel,
    spell_attack_modifier(ranger, ToHit),
    DamageBonus is 2 + ProfBon.

trait_source(ranger('beast master') >: 7, 'exceptional training').
trait_source(ranger('beast master') >: 11, 'bestial fury').
trait_source(ranger('beast master') >: 15, 'share spells').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DESCRIPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

favored_enemy(_) ?= "Beginning at 1st level, you have significant experience studying, tracking, hunting, and even talking to a certain type of enemy.
Choose a type of favored enemy: aberrations, beasts, celestials, constructs, dragons, elementals, fey, fiends, giants, monstrosities, oozes, plants, or undead. Alternatively, you can select two races of humanoid (such as gnolls and orcs) as favored enemies.
You have advantage on Wisdom (Survival) checks to track your favored enemies, as well as on Intelligence checks to recall information about them.
When you gain this feature, you also learn one language of your choice that is spoken by your favored enemies, if they speak one at all.
You choose one additional favored enemy, as well as an associated language, at 6th and 14th level. As you gain levels, your choices should reflect the types of monsters you have encountered on your adventures.".

natural_explorer(_) ?= "You are particularly familiar with one type of natural environment and are adept at traveling and surviving in such regions. Choose one type of favored terrain: arctic, coast, desert, forest, grassland, mountain, or swamp. When you make an Intelligence or Wisdom check related to your favored terrain, your proficiency bonus is doubled if you are using a skill that you're proficient in.
While traveling for an hour or more in your favored terrain, you gain the following benefits:

    Difficult terrain doesn't slow your group's travel.
    Your group can't become lost except by magical means.
    Even when you are engaged in another activity while traveling (such as foraging, navigating, or tracking), you remain alert to danger.
    If you are traveling alone, you can move stealthily at a normal pace.
    When you forage, you find twice as much food as you normally would.
    While tracking other creatures, you also learn their exact number, their sizes, and how long ago they passed through the area.

You choose additional favored terrain types at 6th and 10th level.".

'primeval awareness' ?= "Beginning at 3rd level, you can use your action and expend one ranger spell slot to focus your awareness on the region around you. For 1 minute per level of the spell slot you expend, you can sense whether the following types of creatures are present within 1 mile of you (or within up to 6 miles if you are in your favored terrain): aberrations, celestials, dragons, elementals, fey, fiends, and undead. This feature doesn't reveal the creatures' location or number.".

'extra attack' ?= "Beginning at 5th level, you can attack twice, instead of once, whenever you take the Attack action on your turn.".

'land\'s stride' ?= "Starting at 8th level, moving through nonmagical difficult terrain costs you no extra movement. You can also pass through nonmagical plants without being slowed by them and without taking damage from them if they have thorns, spines, or a similar hazard.
In addition, you have advantage on saving throws against plants that are magically created or manipulated to impede movement, such those created by the entangle spell.".

'hide in plain sight' ?= "Starting at 10th level, you can spend 1 minute creating camouflage for yourself. You must have access to fresh mud, dirt, plants, soot, and other naturally occurring materials with which to create your camouflage.
Once you are camouflaged in this way, you can try to hide by pressing yourself up against a solid surface, such as a tree or wall, that is at least as tall and wide as you are. You gain a +10 bonus to Dexterity (Stealth) checks as long as you remain there without moving or taking actions. Once you move or take an action or a reaction, you must camouflage yourself again to gain this benefit.".

vanish ?= "Starting at 14th level, you can use the Hide action as a bonus action on your turn. Also, you can't be tracked by nonmagical means, unless you choose to leave a trail.".

'feral senses' ?= "At 18th level, you gain preternatural senses that help you fight creatures you can't see. When you attack a creature you can't see, your inability to see it doesn't impose disadvantage on your attack rolls against it. You are also aware of the location of any invisible creature within 30 feet of you, provided that the creature isn't hidden from you and you aren't blinded or deafened.".

'foe slayer' ?= "At 20th level, you become an unparalleled hunter of your enemies. Once on each of your turns, you can add your Wisdom modifier to the attack roll or the damage roll of an attack you make against one of your favored enemies. You can choose to use this feature before or after the roll, but before any effects of the roll are applied.".

hunters_prey('colossus slayer') ?= "Your tenacity can wear down the most potent foes. When you hit a creature with a weapon attack, the creature takes an extra 1d8 damage if it's below its hit point maximum. You can deal this extra damage only once per turn.".
hunters_prey('giant killer') ?= " When a Large or larger creature within 5 feet of you hits or misses you with an attack, you can use your reaction to attack that creature immediately after its attack, provided that you can see the creature.".
hunters_prey('horde breaker') ?= " Once on each of your turns when you make a weapon attack, you can make another attack with the same weapon against a different creature that is within 5 feet of the original target and within range of your weapon. ".

defensive_tactics('escape the horde') ?= "Opportunity attacks against you are made with disadvantage.".
defensive_tactics('multiattack defense') ?= "When a creature hits you with an attack, you gain a +4 bonus to AC against all subsequent attacks made by that creature for the rest of the turn.".
defensive_tactics('steel will') ?= "You have advantage on saving throws against being frightened. ".

multiattack('volley') ?= "You can use your action to make a ranged attack against any number of creatures within 10 feet of a point you can see within your weapon's range. You must have ammunition for each target, as normal, and you make a separate attack roll for each target.".
multiattack('whirlwind attack') ?= "You can use your action to make a melee attack against any number of creatures within 5 feet of you, with a separate attack roll for each target. ".

superior_hunters_defense('evasion') ?= "When you are subjected to an effect, such as a red dragon's fiery breath or a lightning bolt spell, that allows you to make a Dexterity saving throw to take only half damage, you instead take no damage if you succeed on the saving throw, and only half damage if you fail.".
superior_hunters_defense('stand against the tide') ?= "When a hostile creature misses you with a melee attack, you can use your reaction to force that creature to repeat the same attack against another creature (other than itself) of your choice.".
superior_hunters_defense('uncanny dodge') ?= "When an attacker that you can see hits you with an attack, you can use your reaction to halve the attack's damage against you.".

'primal companion' ?= "You magically summon a primal beast, which draws strength from your bond with nature. The beast is friendly to you and your companions and obeys your commands. Choose its stat block-Beast of the Land, Beast of the Sea, or Beast of the Sky-which uses your proficiency bonus (PB) in several places. You also determine the kind of animal the beast is, choosing a kind appropriate for the stat block. Whatever kind you choose, the beast bears primal markings, indicating its mystical origin.

In combat, the beast acts during your turn. It can move and use its reaction on its own, but the only action it takes is the Dodge action, unless you take a bonus action on your turn to command it to take another action. That action can be one in its stat block or some other action. You can also sacrifice one of your attacks when you take the Attack action to command the beast to take the Attack action. If
you are incapacitated, the beast can take any action of its choice, not just Dodge.

If the beast has died within the last hour, you can use your action to touch it and expend a spell slot of 1st level or higher. The beast returns to life after 1 minute with all its hit points restored. When you finish a long rest, you can summon a different primal beast. The new beast appears in an unoccupied space within 5 feet of you, and you choose its stat block and appearance. If you already have a beast from this feature, it vanishes when the new beast appears. The beast also vanishes if you die.".

(beast_of_the_land(ac(AC), hp(HP), pb(ProfBon), to_hit(ToHit), damage(1 d 8 + DamageBonus)) ?= Str) :-
    format(
        string(Str),
"Medium beast

**Armor Class**: ~w

**Hit Points**: ~w

**Speed**: 40 ft., climb 40ft.

| STR 	  | DEX 	  | CON 	  | INT 	  | WIS 	  | CHA     |
|---------|---------|---------|---------|---------|---------|
| 14 (+2) | 14 (+2) | 15 (+2) | 8 (−1) 	| 14 (+2) | 11 (+0) |

**Senses**: darkvision 60 ft., passive Perception 12

**Languages**: understands the languages you speak

**Proficiency Bonus (PB)**: +~w

**Charge**: If the beast moves at least 20 feet straight toward a target and then hits it with a maul attack on the same turn, the target takes an extra 1d6 slashing damage. If the target is a creature, it must succeed on a Strength saving throw against your spell save DC or be knocked prone.

**Primal Bond**: You can add your proficiency bonus to any ability check or saving throw that the beast makes.

**Maul**. Melee Weapon Attack: ~w to hit, reach 5 ft., one target. Hit: 1d8 + ~w slashing damage.",
        [AC,HP,ProfBon,ToHit,DamageBonus]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'primeval awareness'@=phb('92').
'hide in plain sight'@=phb('92').
vanish@=phb('92').
'feral senses'@=phb('92').
'foe slayer'@=phb('92').
'exceptional training'@=phb('93').
'bestial fury'@=phb('93').
'share spells'@=phb('93').
