class_option(fighter).
hd_per_level(fighter, 1 d 10).
initial_class_base_hp(fighter, 10).
max_hp_per_level(fighter, 1 d 10).
choose_subclass_level(fighter:3).
class_saving_throw(fighter, str).
class_saving_throw(fighter, con).
asi_level(fighter:L) :-
    member(L, [4,6,8,12,14,16,19]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initial class features.
trait_source(^fighter, armor(heavy)).
trait_options_source(^fighter, skill, wrap(skill),
                     2 unique_from from_list(
                         [acrobatics, 'animal handling', athletics, history,
                          insight, intimidation, perception, survival])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 1 features.
traits_from_source(fighter >: 1,
                   [armor(light), armor(medium), armor(shield),
                    weapon(simple), weapon(martial)]).

trait_options_source(fighter >: 1, 'fighting style',
                     wrap(fighting_style), fighting_style).

trait_source(fighter >: 1, second_wind(1 d 10 + L)) :-
    class_level(fighter:L).
res('second wind', 1) :-
    trait(second_wind(_)).
restore_res('short rest', 'second wind', 'full restore') :-
    trait(second_wind(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Traits from leveling up.
trait_source(fighter >: 2, 'action surge').
res('action surge', N) :-
    class_level(fighter:L),
    ordered_lookup_largest_leq([2 -> 1, 17 -> 2], L, N).
restore_res('short rest', 'action surge', 'full restore').

multiclass_trait_source(fighter >: 5, extra_attack(N)) :-
    class_level(fighter:L),
    ordered_lookup_largest_leq([5 -> 1, 11 -> 2, 20 -> 3], L, N).

trait_source(fighter >: 9, indomitable).
res(indomitable, N) :-
    class_level(fighter:L),
    ordered_lookup_largest_leq([9 -> 1, 13 -> 2, 17 -> 3], L, N).
restore_res('long rest', indomitable, 'full restore').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MARTIAL ARCHETYPES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Champion (SRD)
% --------------
subclass_option(fighter, champion).

trait_source(fighter(champion) >: 3, 'improved critical') :-
    class_level(fighter(champion):CurLevel),
    between(3, 14, CurLevel). % Overridden by superior critical. TODO this is ad-hoc, perhaps nice to have a systematic way of overriding traits.

trait_source(fighter(champion) >: 7, 'remarkable athlete').
trait_options_source(fighter(champion) >: 10, 'fighting style',
                     wrap(fighting_style), fighting_style).
trait_source(fighter(champion) >: 15, 'superior critical'). % overrides 'improved critical'
trait_source(fighter(champion) >: 18, survivor(HP)) :-
    ability_mod(con, Mod),
    HP is 5 + Mod.

% Eldritch knight
% ---------------
subclass_option(fighter, 'eldritch knight').
caster(fighter, 1/3) :- subclass(fighter('eldritch knight')).
spellcasting_ability(fighter, int) :- subclass(fighter('eldritch knight')).

% Traits.
trait_source(fighter('eldritch knight') >: 3, 'weapon bond').
trait_source(fighter('eldritch knight') >: 7, 'war magic').
trait_source(fighter('eldritch knight') >: 10, 'eldritch strike').
trait_source(fighter('eldritch knight') >: 15, 'arcane charge').
trait_source(fighter('eldritch knight') >: 18, 'improved war magic').

% Learn cantrips.
known_spell(fighter('eldritch knight'), int, always, [], no, Cantrip) :-
    choice_member(fighter('eldritch knight') >: _, cantrip, Cantrip).
options_source(fighter('eldritch knight') >: 3,
               cantrip,
               2 unique_from class_cantrip(wizard)).
options_source(fighter('eldritch knight') >: 10, cantrip, class_cantrip(wizard)).

% Learn proper spells.
known_spell(fighter('eldritch knight'), int, always, [slot], no, Name) :-
    class_level(fighter:L),
    selected_at_class_level(fighter:L, spell(_), Name).

hide_known_class_spells(fighter('eldritch knight') >: _, spell(_), fighter).
hide_known_class_spells(fighter('eldritch knight') >: _, replacing(spell(_), _), fighter).

ek_unconstrained_spell_level(L) :-
    member(L, [3, 8, 14, 20]).

options_source(fighter('eldritch knight') >: 3, spell('abjuration or evocation'),
               2 unique_from learnable_eldritch_knight_spell('abjuration or evocation')).
options_source(fighter('eldritch knight') >: L, spell(unconstrained),
               learnable_eldritch_knight_spell(unconstrained)) :-
    ek_unconstrained_spell_level(L).
options_source(fighter('eldritch knight') >: L, spell('abjuration or evocation'),
               learnable_eldritch_knight_spell('abjuration or evocation')) :-
    member(L, [4, 7, 10, 11, 13, 16, 19]).


% Replace proper spells.
options_source(fighter('eldritch knight') >: L, replace(spell(Constraint)),
               selected_at_class_level(fighter:Prev, spell(Constraint))) :-
    between(4, 20, L),
    Prev is L-1.

options_source(fighter('eldritch knight') >: L, replacing(spell(Constraint), Name),
               learnable_eldritch_knight_spell(Constraint)) :-
    choice_member(fighter('eldritch knight') >: L, replace(spell(_)), Name),

    % We need to deal with the case that the same spell might have been picked,
    % replaced, then picked again. The right choice has to be retracted so that
    % we know the correct constraint to apply.
    Prev is L-1,
    selected_at_class_level(fighter:Prev, spell(Constraint), Name).

extend_class_spell_list(fighter, Spell) :-
    fighter('eldritch knight') >: 3,
    spell_property(Spell, classes, Classes),
    member(wizard, Classes).

learnable_eldritch_knight_spell(unconstrained, Spell) :-
    learnable_proper_spell(fighter, Spell).
learnable_eldritch_knight_spell('abjuration or evocation', Spell) :-
    learnable_proper_spell(fighter, Spell),
    spell_property(Spell, school, School),
    (School = abjuration ; School = evocation).

% Battle master
% -------------
subclass_option(fighter, 'battle master').

% TODO descriptions

trait_source(fighter('battle master') >: 3, 'combat superiority'(DiceNum d DiceVal, SaveDC)) :-
    class_level(fighter:Lvl),
    ordered_lookup_largest_leq([3 -> 4, 7 -> 5, 15 -> 6], Lvl, DiceNum),
    ordered_lookup_largest_leq([3 -> 8, 10 -> 10, 18 -> 12], Lvl, DiceVal),
    proficiency_bonus(ProfBon),
    ability_mod(str, StrMod),
    ability_mod(dex, DexMod),
    SaveDC is 8 + ProfBon + max(StrMod, DexMod).
custom_format('combat superiority'(Dice, SaveDC)) -->
    ["combat superiority ("], format_dice(Dice), [", DC "], [SaveDC], [")"].
res('superiority dice'(M), N) :-
    trait('combat superiority'(N d M, _)).
restore_res('short rest', 'superiority dice'(_), 'full restore').
restore_res('long rest', 'superiority dice'(_), 'full restore').
custom_format('superiority dice'(M)) -->
    ["superiority dice (d"], [M], [")"].

trait_source(fighter('battle master') >: 3, 'student of war',
             wrap('artisan\'s tools'), artisans_tools).

% Maneuvers
replaceable_class_options(fighter('battle master'):3, maneuver,
                          3 unique_from battle_master_maneuver).
replaceable_class_options(fighter('battle master'):L, maneuver,
                          2 unique_from battle_master_maneuver) :-
    member(L, [7, 10, 15]).
replace_at_class_level(fighter('battle master'):L, maneuver, 1, battle_master_maneuver) :-
    member(L, [7, 10, 15]).
trait_source(fighter('battle master') >: L, maneuver(Maneuver)) :-
    find_choice_level(fighter('battle master'):L, maneuver, Maneuver).
lookup_option_doc(fighter('battle master') >: _, maneuver, Maneuver, Doc) :-
    (maneuver(Maneuver) ?= Doc).
battle_master_maneuver(ambush).
battle_master_maneuver('bait and switch').
battle_master_maneuver(brace).
battle_master_maneuver('commander\'s strike').
battle_master_maneuver('commanding presence').
battle_master_maneuver('disarming attack').
battle_master_maneuver('distracting strike').
battle_master_maneuver('evasive footwork').
battle_master_maneuver('feinting attack').
battle_master_maneuver('goading attack').
battle_master_maneuver('grappling strike').
battle_master_maneuver('lunging attack').
battle_master_maneuver('maneuvering attack').
battle_master_maneuver('menacing attack').
battle_master_maneuver(parry).
battle_master_maneuver('precision attack').
battle_master_maneuver('pushing attack').
battle_master_maneuver('quick toss').
battle_master_maneuver(rally).
battle_master_maneuver(riposte).
battle_master_maneuver('sweeping attack').
battle_master_maneuver('tactical assessment').
battle_master_maneuver('trip attack').

% ..
trait_source(fighter('battle master') >: 7, 'know your enemy').
trait_source(fighter('battle master') >: 10, 'improved combat superiority').
trait_source(fighter('battle master') >: 15, relentless).
restore_res('initiative roll', 'superiority dice'(_), 'if 0 left, +1').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DESCRIPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

second_wind(_) ?= "You have a limited well of stamina that you can draw on to protect yourself from harm. On your turn, you can use a bonus action to regain hit points equal to 1d10 + your fighter level. Once you use this feature, you must finish a short or long rest before you can use it again.".

'action surge' ?= "Starting at 2nd level, you can push yourself beyond your normal limits for a moment. On your turn, you can take one additional action on top of your regular action and a possible bonus action. Once you use this feature, you must finish a short or long rest before you can use it again. Starting at 17th level, you can use it twice before a rest, but only once on the same turn.".

indomitable ?= "Beginning at 9th level, you can reroll a saving throw that you fail. If you do so, you must use the new roll, and you can’t use this feature again until you finish a long rest. You can use this feature twice between long rests starting at 13th level and three times between long rests starting at 17th level.".


'improved critical' ?= "Beginning when you choose this archetype at 3rd level, your weapon attacks score a critical hit on a roll of 19 or 20.".

'remarkable athlete' ?= "Starting at 7th level, you can add half your proficiency bonus (round up) to any Strength, Dexterity, or Constitution check you make that doesn’t already use your proficiency bonus. In addition, when you make a running long jump, the distance you can cover increases by a number of feet equal to your Strength modifier.".

'additional fighting style' ?= "At 10th level, you can choose a second option from the Fighting Style class feature.".

'superior critical' ?= "Starting at 15th level, your weapon attacks score a critical hit on a roll of 18–20.".

survivor(_) ?= "At 18th level, you attain the pinnacle of resilience in battle. At the start of each of your turns, you regain hit points equal to 5 + your Constitution modifier if you have no more than half of your hit points left. You don’t gain this benefit if you have 0 hit points.".

'weapon bond' ?= " At 3rd level, you learn a ritual that creates a magical bond between yourself and one weapon. You perform the ritual over the course of 1 hour, which can be done during a short rest. The weapon must be within your reach throughout the ritual, at the conclusion of which you touch the weapon and forge the bond.

Once you have bonded a weapon to yourself, you can't be disarmed of that weapon unless you are incapacitated. If it is on the same plane of existence, you can summon that weapon as a bonus action on your turn, causing it to teleport instantly to your hand.

You can have up to two bonded weapons, but can summon only one at a time with your bonus action. If you attempt to bond with a third weapon, you must break the bond with one of the other two.".

'war magic' ?= " Beginning at 7th level, when you use your action to cast a cantrip, you can make one weapon attack as a bonus action.
Eldritch Strike

At 10th level, you learn how to make your weapon strikes undercut a creature's resistance to your spells. When you hit a creature with a weapon attack, that creature has disadvantage on the next saving throw it makes against a spell you cast before the end of your next turn.".

'arcane charge' ?= " At 15th level, you gain the ability to teleport up to 30 feet to an unoccupied space you can see when you use your Action Surge. You can teleport before or after the additional action.".

'improved war magic' ?= "Starting at 18th level, when you use your action to cast a spell, you can make one weapon attack as a bonus action.".


maneuver(ambush) ?= "When you make a Dexterity (Stealth) check or an initiative roll, you can expend one superiority die and add the die to the roll, provided you aren't incapacitated.".

maneuver('bait and switch') ?= "When you're within 5 feet of a creature on your turn, you can expend one superiority die and switch places with that creature, provided you spend at least 5 feet of movement and the creature is willing and isn't incapacitated. This movement doesn't provoke opportunity attacks.

Roll the superiority die. Until the start of your next turn, you or the other creature (your choice) gains a bonus to AC equal to the number rolled.".

maneuver(brace) ?= "When a creature you can see moves into the reach you have with the melee weapon you're wielding, you can use your reaction to expend one superiority die and make one attack against the creature, using that weapon. If the attack hits, add the superiority die to the weapon's damage roll.".

maneuver('commander\'s strike') ?= "When you take the Attack action on your turn, you can forgo one of your attacks and use a bonus action to direct one of your companions to strike. When you do so, choose a friendly creature who can see or hear you and expend one superiority die. That creature can immediately use its reaction to make one weapon attack, adding the superiority die to the attack's damage roll.".

maneuver('commanding presence') ?= "When you make a Charisma (Intimidation), a Charisma (Performance), or a Charisma (Persuasion) check, you can expend one superiority die and add the superiority die to the ability check.".

maneuver('disarming attack') ?= "When you hit a creature with a weapon attack, you can expend one superiority die to attempt to disarm the target, forcing it to drop one item of your choice that it's holding. You add the superiority die to the attack's damage roll, and the target must make a Strength saving throw. On a failed save, it drops the object you choose. The object lands at its feet.".

maneuver('distracting strike') ?= "When you hit a creature with a weapon attack, you can expend one superiority die to distract the creature, giving your allies an opening. You add the superiority die to the attack's damage roll. The next attack roll against the target by an attacker other than you has advantage if the attack is made before the start of your next turn.".

maneuver('evasive footwork') ?= "When you move, you can expend one superiority die, rolling the die and adding the number rolled to your AC until you stop moving.".

maneuver('feinting attack') ?= "You can expend one superiority die and use a bonus action on your turn to feint, choosing one creature within 5 feet of you as your target. You have advantage on your next attack roll against that creature this turn. If that attack hits, add the superiority die to the attack's damage roll.".

maneuver('goading attack') ?= "When you hit a creature with a weapon attack, you can expend one superiority die to attempt to goad the target into attacking you. You add the superiority die to the attack's damage roll, and the target must make a Wisdom saving throw. On a failed save, the target has disadvantage on all attack rolls against targets other than you until the end of your next turn.".

maneuver('grappling strike') ?= "Immediately after you hit a creature with a melee attack on your turn, you can expend one superiority die and then try to grapple the target as a bonus action (see the Player's Handbook for rules on grappling). Add the superiority die to your Strength (Athletics) check.".

maneuver('lunging attack') ?= "When you make a melee weapon attack on your turn, you can expend one superiority die to increase your reach for that attack by 5 feet. If you hit, you add the superiority die to the attack's damage roll.".

maneuver('maneuvering attack') ?= "When you hit a creature with a weapon attack, you can expend one superiority die to maneuver one of your comrades into a more advantageous position. You add the superiority die to the attack's damage roll, and you choose a friendly creature who can see or hear you. That creature can use its reaction to move up to half its speed without provoking opportunity attacks from the target of your attack.".

maneuver('menacing attack') ?= "When you hit a creature with a weapon attack, you can expend one superiority die to attempt to frighten the target. You add the superiority die to the attack's damage roll, and the target must make a Wisdom saving throw. On a failed save, it is frightened of you until the end of your next turn.".

maneuver(parry) ?= "When another creature damages you with a melee attack, you can use your reaction and expend one superiority die to reduce the damage by the number you roll on your superiority die + your Dexterity modifier.".

maneuver('precision attack') ?= "When you make a weapon attack roll against a creature, you can expend one superiority die to add it to the roll. You can use this maneuver before or after making the attack roll, but before any effects of the attack are applied.".

maneuver('pushing attack') ?= "When you hit a creature with a weapon attack, you can expend one superiority die to attempt to drive the target back. You add the superiority die to the attack's damage roll, and if the target is Large or smaller, it must make a Strength saving throw. On a failed save, you push the target up to 15 feet away from you.".

maneuver('quick toss') ?= "As a bonus action, you can expend one superiority die and make a ranged attack with a weapon that has the thrown property. You can draw the weapon as part of making this attack. If you hit, add the superiority die to the weapon's damage roll.".

maneuver(rally) ?= "On your turn, you can use a bonus action and expend one superiority die to bolster the resolve of one of your companions. When you do so, choose a friendly creature who can see or hear you. That creature gains temporary hit points equal to the superiority die roll + your Charisma modifier.".

maneuver(riposte) ?= "When a creature misses you with a melee attack, you can use your reaction and expend one superiority die to make a melee weapon attack against the creature. If you hit, you add the superiority die to the attack's damage roll.".

maneuver('sweeping attack') ?= "When you hit a creature with a melee weapon attack, you can expend one superiority die to attempt to damage another creature with the same attack. Choose another creature within 5 feet of the original target and within your reach. If the original attack roll would hit the second creature, it takes damage equal to the number you roll on your superiority die. The damage is of the same type dealt by the original attack.".

maneuver('tactical assessment') ?= "When you make an Intelligence (Investigation), an Intelligence (History), or a Wisdom (Insight) check, you can expend one superiority die and add the superiority die to the ability check.".

maneuver('trip attack') ?= "When you hit a creature with a weapon attack, you can expend one superiority die to attempt to knock the target down. You add the superiority die to the attack's damage roll, and if the target is Large or smaller, it must make a Strength saving throw. On a failed save, you knock the target prone.".

'combat superiority' ?= "When you choose this archetype at 3rd level, you learn maneuvers that are fueled by special dice called superiority dice.

**Maneuvers.** You learn three maneuvers of your choice. Many maneuvers enhance an attack in some way. You can use only one maneuver per attack. You learn two additional maneuvers of your choice at 7th, 10th, and 15th level. Each time you learn new maneuvers, you can also replace one maneuver you know with a different one.

**Superiority Dice.** You have four superiority dice, which are d8s. A superiority die is expended when you use it. You regain all of your expended superiority dice when you finish a short or long rest. You gain another superiority die at 7th level and one more at 15th level.

**Saving Throws.** Some of your maneuvers require your target to make a saving throw to resist the maneuver's effects. The saving throw DC is calculated as follows:

**Maneuver save DC** = 8 + your proficiency bonus + your Strength or Dexterity modifier (your choice)".

'know your enemy' ?= "Starting at 7th level, if you spend at least 1 minute observing or interacting with another creature outside combat, you can learn certain information about its capabilities compared to your own. The DM tells you if the creature is your equal, superior, or inferior in regard to two of the following characteristics of your choice:

- Strength score

- Dexterity score

- Constitution score

- Armor Class

- Current hit points

- Total class levels, if any

- Fighter class levels, if any".

'improved combat superiority' ?= "At 10th level, your superiority dice turn into d10s. At 18th level, they turn into d12s.".

relentless ?= "Starting at 15th level, when you roll initiative and have no superiority dice remaining, you regain 1 superiority die.".

second_wind(_)@=srd('72').
'action surge'@=srd('72').
indomitable@=srd('72').
'improved critical'@=srd('72').
'remarkable athlete'@=srd('72').
'superior critical'@=srd('73').
survivor(_)@=srd('73').
'weapon bond'@=phb('75').
'war magic'@=phb('75').
'eldritch strike'@=phb('75').
'arcane charge'@=phb('75').
'improved war magic'@=phb('75').
