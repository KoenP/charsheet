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
on_rest(short, 'second wind', 'full restore') :-
    trait(second_wind(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Traits from leveling up.
trait_source(fighter >: 2, 'action surge').
res('action surge', N) :-
    class_level(fighter:L),
    ordered_lookup_largest_leq([2 -> 1, 17 -> 2], L, N).
on_rest(short, 'action surge', 'full restore').

multiclass_trait_source(fighter >: 5, extra_attack(N)) :-
    class_level(fighter:L),
    ordered_lookup_largest_leq([5 -> 1, 11 -> 2, 20 -> 3], L, N).

trait_source(fighter >: 9, indomitable).
res(indomitable, N) :-
    class_level(fighter:L),
    ordered_lookup_largest_leq([9 -> 1, 13 -> 2, 17 -> 3], L, N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MARTIAL ARCHETYPES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Champion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subclass_option(fighter, champion).

trait_source(fighter(champion) >: 3, 'improved critical') :-
    class_level(fighter(champion):CurLevel),
    between(3, 14, CurLevel). % Overridden by superior critical. TODO this is ad-hoc, perhaps nice to have a systematic way of overriding traits.

trait_source(fighter(champion) >: 3, 'improved critical').
trait_source(fighter(champion) >: 7, 'remarkable athlete').
trait_options_source(fighter(champion) >: 10, 'fighting style',
                     wrap(fighting_style), fighting_style).
trait_source(fighter(champion) >: 15, 'superior critical'). % overrides 'improved critical'
trait_source(fighter(champion) >: 18, survivor(HP)) :-
    ability_mod(con, Mod),
    HP is 5 + Mod.

% Eldritch knight
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DESCRIPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

second_wind(_) ?= "You have a limited well of stamina that you can draw on to protect yourself from harm. On your turn, you can use a bonus action to regain hit points equal to 1d10 + your fighter level. Once you use this feature, you must finish a short or long rest before you can use it again.".

"action surge" ?= "Starting at 2nd level, you can push yourself beyond your normal limits for a moment. On your turn, you can take one additional action on top of your regular action and a possible bonus action. Once you use this feature, you must finish a short or long rest before you can use it again. Starting at 17th level, you can use it twice before a rest, but only once on the same turn.".

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
