class_option(rogue).
hd_per_level(rogue, 1 d 8).
initial_class_base_hp(rogue, 8).
max_hp_per_level(rogue, 1 d 8).
choose_subclass_level(rogue:3).
asi_level(rogue:L) :-
    default_asi_level(L).
class_saving_throw(rogue, dex).
class_saving_throw(rogue, int).

class_skill_list(rogue, [acrobatics, athletics, deception, insight,
                         intimidation, investigation, perception,
                         performance, persuasion,
                         'sleight of hand', stealth]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initial class features (you don't get these when multiclassing into rogue).
traits_from_source(^rogue,
                   [weapon(simple), weapon('hand crossbow'),
                    weapon(longsword), weapon(rapier),
                    weapon(shortsword)]).
trait_options_source(^rogue, skill, wrap(skill),
                     4 unique_from class_skill(rogue)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 1 features (you get these when multiclassing into rogue).
traits_from_source(rogue >: 1, [armor(light), tool('thieves\' tools')]).
trait_options_source(rogue >: 1, skill, wrap(skill), class_skill(rogue)) :-
    \+ (^rogue).

trait_options_source(rogue >: L, expertise, rogue_expertise_to_trait,
                     2 unique_from rogue_expertise_option) :-
    L = 1 ; L = 6.
rogue_expertise_option('thieves\' tools').
rogue_expertise_option(Skill) :- trait(skill(Skill)).
rogue_expertise_to_trait('thieves\' tools', expertise(tool('thieves\' tools'))).
rogue_expertise_to_trait(Skill, expertise(skill(Skill))) :- skill(Skill).

trait_source(rogue >: 1, sneak_attack(N d 6)) :-
    class_level(rogue:L),
    N is ceil(L/2).

trait_source(rogue >: 1, 'thieves\' cant').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features from leveling up.
trait_source(rogue >: 2, 'cunning action').
trait_source(rogue >: 5, 'uncanny dodge').
trait_source(rogue >: 7, evasion).
trait_source(rogue >: 11, 'reliable talent').
trait_source(rogue >: 14, sense(blindsense)).
trait_source(rogue >: 15, 'slippery mind').
trait_source(rogue >: 18, elusive).
trait_source(rogue >: 20, 'stroke of luck').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Roguish archetype: thief.
subclass_option(rogue, thief).

trait_source(rogue(thief) >: 3, 'fast hands').
trait_source(rogue(thief) >: 3, 'second-story work').
trait_source(rogue(thief) >: 9, 'supreme sneak').
trait_source(rogue(thief) >: 13, 'use magic device').
trait_source(rogue(thief) >: 17, 'thief\'s reflexes').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Roguish archetype: arcane trickster.
subclass_option(rogue, 'arcane trickster').
caster(rogue, 1/3) :- subclass(rogue('arcane trickster')).
spellcasting_ability(rogue, int) :- subclass(rogue('arcane trickster')).

% Mage hand legerdemain feature.
trait_source(rogue('arcane trickster') >: 3, 'mage hand legerdemain').
known_spell(rogue('arcane trickster'), int, always, [], no, 'mage hand') :-
    trait('mage hand legerdemain').
bonus_source(trait('mage hand legerdemain'),
             modify_spell(rogue('arcane trickster'),
                          'mage hand',
                          Goal)) :-
    BonusFeatures = ["invisible", "control with bonus action (cunning action)",
                     "can steal, stow, pick locks, disarm traps using relevant skill"],
    Goal = add_spell_effects(BonusFeatures).

% Other features.
trait_source(rogue('arcane trickster') >: 9, 'magical ambush').
trait_source(rogue('arcane trickster') >: 13, 'versatile trickster').
bonus_source(trait('versatile trickster'),
             modify_spell(rogue('arcane trickster'),
                          'mage hand',
                          add_spell_effects(['distract creature within 5 ft for advantage on attack rolls until end of turn']))).
trait_source(rogue('arcane trickster') >: 17, 'spell thief').


% Learn cantrips.
known_spell(rogue('arcane trickster'), int, always, [], no, Cantrip) :-
    choice_member(rogue('arcane trickster') >: _, cantrip, Cantrip).
options_source(rogue('arcane trickster') >: 3, cantrip,
               2 unique_from class_cantrip(wizard)).
options_source(rogue('arcane trickster') >: 10, cantrip,
               class_cantrip(wizard)).

% Learn proper spells
known_spell(rogue('arcane trickster'), int, always, [slot], no, Name) :-
    class_level(rogue:L),
    selected_at_class_level(rogue:L, spell(_), Name).

hide_known_class_spells(rogue('arcane trickster') >: _, spell(_), rogue).
hide_known_class_spells(rogue('arcane trickster') >: _, replacing(spell(_), _), rogue).

trickster_unconstrained_spell_level(L) :-
    member(L, [3, 8, 14, 20]).

options_source(rogue('arcane trickster') >: 3, spell('enchantment or illusion'),
               2 unique_from learnable_arcane_trickster_spell('enchantment or illusion')).
options_source(rogue('arcane trickster') >: L, spell(unconstrained),
               learnable_arcane_trickster_spell(unconstrained)) :-
    trickster_unconstrained_spell_level(L).
options_source(rogue('arcane trickster') >: L, spell('enchantment or illusion'),
               learnable_arcane_trickster_spell('enchantment or illusion')) :-
    member(L, [4, 7, 10, 11, 13, 16, 19]).


% Replace proper spells.
options_source(rogue('arcane trickster') >: L, replace(spell(Constraint)),
               selected_at_class_level(rogue:Prev, spell(Constraint))) :-
    between(4, 20, L),
    Prev is L-1.

options_source(rogue('arcane trickster') >: L, replacing(spell(Constraint), Name),
               learnable_arcane_trickster_spell(Constraint)) :-
    choice_member(rogue('arcane trickster') >: L, replace(spell(_)), Name),

    % We need to deal with the case that the same spell might have been picked,
    % replaced, then picked again. The right choice has to be retracted so that
    % we know the correct constraint to apply.
    Prev is L-1,
    selected_at_class_level(rogue:Prev, spell(Constraint), Name).

extend_class_spell_list(rogue, Spell) :-
    rogue('arcane trickster') >: 3,
    spell_property(Spell, classes, Classes),
    member(wizard, Classes).

learnable_arcane_trickster_spell(unconstrained, Spell) :-
    learnable_proper_spell(rogue, Spell).
learnable_arcane_trickster_spell('enchantment or illusion', Spell) :-
    learnable_proper_spell(rogue, Spell),
    spell_property(Spell, school, School),
    (School = enchantment ; School = illusion).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sneak_attack(_) ?= "Beginning at 1st level, you know how to strike subtly and exploit a foe's distraction. Once per turn, you can deal an extra 1d6 damage to one creature you hit with an attack if you have advantage on the attack roll. The attack must use a finesse or a ranged weapon. You don't need advantage on the attack roll if another enemy of the target is within 5 feet of it, that enemy isn't incapacitated, and you don't have disadvantage on the attack roll. The amount of the extra damage increases as you gain levels in this class, as shown in the Sneak Attack column of the Rogue table.".

'thieves\' cant' ?= "During your rogue training you learned thieves' cant, a secret mix of dialect, jargon, and code that allows you to hide messages in seemingly normal conversation. Only another creature that knows thieves' cant understands such messages. It takes four times longer to convey such a message than it does to speak the same idea plainly. In addition, you understand a set of secret signs and symbols used to convey short, simple messages, such as whether an area is dangerous or the territory of a thieves' guild, whether loot is nearby, or whether the people in an area are easy marks or will provide a safe house for thieves on the run.".

'cunning action' ?= "Starting at 2nd level, your quick thinking and agility allow you to move and act quickly. You can take a bonus action on each of your turns in combat. This action can be used only to take the Dash, Disengage, or Hide action.".

'uncanny dodge' ?= "Starting at 5th level, when an attacker that you can see hits you with an attack, you can use your reaction to halve the attack's damage against you.".

evasion ?= "Beginning at 7th level, you can nimbly dodge out of the way of certain area effects, such as a red dragon's fiery breath or an ice storm spell. When you are subjected to an effect that allows you to make a Dexterity saving throw to take only half damage, you instead take no damage if you succeed on the saving throw, and only half damage if you fail.".

'reliable talent' ?= "By 11th level, you have refined your chosen skills until they approach perfection. Whenever you make an ability check that lets you add your proficiency bonus, you can treat a d20 roll of 9 or lower as a 10.".

'blindsense' ?= "Starting at 14th level, if you are able to hear, you are aware of the location of any hidden or invisible creature within 10 feet of you.".

'slippery mind' ?= "By 15th level, you have acquired greater mental strength. You gain proficiency in Wisdom saving throws.".

elusive ?= "Beginning at 18th level, you are so evasive that attackers rarely gain the upper hand against you. No attack roll has advantage against you while you aren't incapacitated.".

'stroke of luck' ?= "At 20th level, you have an uncanny knack for succeeding when you need to. If your attack misses a target within range, you can turn the miss into a hit. Alternatively, if you fail an ability check, you can treat the d20 roll as a 20.Once you use this feature, you can't use it again until you finish a short or long rest.".

'fast hands' ?= "Starting at 3rd level, you can use the bonus action granted by your Cunning Action to make a Dexterity (Sleight of Hand) check, use your thieves' tools to disarm a trap or open a lock, or take the Use an Object action.".

'second-story work' ?= "When you choose this archetype at 3rd level, you gain the ability to climb faster than normal; climbing no longer costs you extra movement.
In addition, when you make a running jump, the distance you cover increases by a number of feet equal to your Dexterity modifier.".

'supreme sneak' ?= "Starting at 9th level, you have advantage on a Dexterity (Stealth) check if you move no more than half your speed on the same turn.".

'use magic device' ?= "By 13th level, you have learned enough about the workings of magic that you can improvise the use of items even when they are not intended for you. You ignore all class, race, and level requirements on the use of magic items. ".

'thief\'s reflexes' ?= "When you reach 17th level, you have become adept at laying ambushes and quickly escaping danger. You can take two turns during the first round of any combat. You take your first turn at your normal initiative and your second turn at your initiative minus 10. You can't use this feature when you are surprised.".

'mage hand legerdemain' ?= "Starting at 3rd level, when you cast Mage Hand, you can make the spectral hand invisible, and you can perform the following additional tasks with it >:  You can stow one object the hand is holding in a container worn or carried by another creature. You can retrieve an object in a container worn or carried by another creature. You can use thieves' tools to pick locks and disarm traps at range. You can perform one of these tasks without being noticed by a creature if you succeed on a Dexterity (Sleight of Hand check contested by the creature's Wisdom (Perception) check. In addition, you can use the bonus action granted by your Cunning Action to control the hand.".

'magical ambush' ?= "Starting at 9th level, if you are hidden from a creature when you cast a spell on it, the creature has disadvantage on any saving throw it makes against the spell this turn.".

'versatile trickster' ?= "At 13th level, you gain the ability to distract targets with your Mage Hand. As a bonus action on your turn, you can designate a creature within 5 feet of the spectral hand created by the spell. Doing so gives you advantage on attack rolls against that creature until the end of the turn.".

'spell thief' ?= "At 17th level, you gain the ability to magically steal the knowledge of how to cast a spell from another spellcaster. Immediately after a creature casts a spell that targets you or includes you in its area of effect, you can use your reaction to force the creature to make a saving throw with its spellcasting ability modifier. The DC equals your spell save DC. On a failed save, you negate the spell's effect against you, and you steal the knowledge of the spell if it is at least 1st level and of a level you can cast (it doesn't need to be a wizard spell). For the next 8 hours, you know the spell and can cast it using your spell slots. The creature can't cast that spell until the 8 hours have passed. Once you use this feature, you can't use it again until you finish a long rest.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sneak_attack(_)@=srd('96').
'cunning action'@=srd('96').
'reliable talent'@=srd('96').
'slippery mind'@=srd('96').
elusive@=srd('96').
'stroke of luck'@=srd('96').

'fast hands'@=srd('97').
'second-story work'@=srd('97').
'supreme sneak'@=srd('97').
'use magic device'@=srd('97').
'thief\'s reflexes'@=srd('97').

'mage hand legerdemain'@=phb('98').
'magical ambush'@=phb('98').
'versatile trickster'@=phb('98').
'spell thief'@=phb('98').
