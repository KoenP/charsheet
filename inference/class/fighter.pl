:- discontiguous fighting_style/1.

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
                          insight, intimidation, perception, survivial])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 1 features.
traits_from_source(fighter >: 1,
                   [armor(light), armor(medium), armor(shield),
                    weapon(simple), weapon(martial)]).

trait_options_source(fighter >: 1, 'fighting style',
                     wrap(fighting_style), fighting_style).

trait_source(fighter >: 1, second_wind(1 d 10 + L)) :-
    class_level(fighter:L).
resource('second wind', 'second wind', 1) :-
    trait(second_wind(_)).
on_rest(short, 'second wind', 'full restore') :-
    trait(second_wind(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Traits from leveling up.
trait_source(fighter >: 2, 'action surge').
resource('action surge', 'action surge', N) :-
    class_level(fighter:L),
    ordered_lookup_largest_leq([2 -> 1, 17 -> 2], L, N).
on_rest(short, 'action surge', 'full restore').

multiclass_trait_source(fighter >: 5, extra_attack(N)) :-
    class_level(fighter:L),
    ordered_lookup_largest_leq([5 -> 1, 11 -> 2, 20 -> 3], L, N).

trait_source(fighter >: 9, indomitable).
resource(indomitable, reroll, N) :-
    class_level(fighter:L),
    ordered_lookup_largest_leq([9 -> 1, 13 -> 2, 17 -> 3], L, N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MARTIAL ARCHETYPES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subclass_option(fighter, champion).
trait_source(fighter(champion) >: 3, 'improved critical').
trait_source(fighter(champion) >: 7, 'remarkable athlete').
trait_options_source(fighter(champion) >: 10, 'fighting style',
                     wrap(fighting_style), fighting_style).
trait_source(fighter(champion) >: 15, 'superior critical').
trait_source(fighter(champion) >: 18, survivor(HP)) :-
    ability_mod(con, Mod),
    HP is 5 + Mod.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DESCRIPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

second_wind(_) ?= "You have a limited well of stamina that you can draw on to protect yourself from harm. On your turn, you can use a bonus action to regain hit points equal to 1d10 + your fighter level. Once you use this feature, you must finish a short or long rest before you can use it again.".

"action surge" ?= "Starting at 2nd level, you can push yourself beyond your normal limits for a moment. On your turn, you can take one additional action on top of your regular action and a possible bonus action. Once you use this feature, you must finish a short or long rest before you can use it again. Starting at 17th level, you can use it twice before a rest, but only once on the same turn.".

indomitable ?= "Beginning at 9th level, you can reroll a saving throw that you fail. If you do so, you must use the new roll, and you can’t use this feature again until you finish a long rest. You can use this feature twice between long rests starting at 13th level and three times between long rests starting at 17th level.".

fighting_style(archery) ?= "You gain a +2 bonus to attack rolls you make with ranged weapons.".
fighting_style(dueling) ?= "When you are wielding a melee weapon in one hand and no other weapons, you gain a +2 bonus to damage rolls with that weapon.".
fighting_style('great weapon fighting') ?= "When you roll a 1 or 2 on a damage die for an attack you make with a melee weapon that you are wielding with two hands, you can reroll the die and must use the new roll, even if the new roll is a 1 or a 2. The weapon must have the two-handed or versatile property for you to gain this benefit.".
fighting_style(protection) ?= "When a creature you can see attacks a target other than you that is within 5 feet of you, you can use your reaction to impose disadvantage on the attack roll. You must be wielding a shield.".
fighting_style('two-weapon fighting') ?= "When you engage in two-weapon fighting, you can add your ability modifier.".

'improved critical' ?= "Beginning when you choose this archetype at 3rd level, your weapon attacks score a critical hit on a roll of 19 or 20.".

'remarkable athlete' ?= "Starting at 7th level, you can add half your proficiency bonus (round up) to any Strength, Dexterity, or Constitution check you make that doesn’t already use your proficiency bonus. In addition, when you make a running long jump, the distance you can cover increases by a number of feet equal to your Strength modifier.".

'additional fighting style' ?= "At 10th level, you can choose a second option from the Fighting Style class feature.".

'superior critical' ?= "Starting at 15th level, your weapon attacks score a critical hit on a roll of 18–20.".

survivor(_) ?= "At 18th level, you attain the pinnacle of resilience in battle. At the start of each of your turns, you regain hit points equal to 5 + your Constitution modifier if you have no more than half of your hit points left. You don’t gain this benefit if you have 0 hit points.".
