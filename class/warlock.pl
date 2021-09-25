:- discontiguous eldritch_invocation_option/1.

class_option(warlock).
hd_per_level(warlock, 1 d 8).
initial_class_base_hp(warlock, 8).
max_hp_per_level(warlock, 1 d 8).
caster(warlock, 0).
choose_subclass_level(warlock:1).
asi_level(warlock:L) :-
    default_asi_level(L).
class_saving_throw(warlock, wis).
class_saving_throw(warlock, cha).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initial class features (you don't gain these when multiclassing into warlock).
trait_options_source(initial_class(warlock), skill, wrap(skill),
                     2 unique_from from_list(
                         [arcana, deception, history, intimidation,
                          investigation, nature, religion])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features from leveling up (except pact magic, that is its own section).
traits_from_source(match_class(warlock), [armor(light), weapon(simple)]).
trait_source(match_class(warlock), spellcasting_focus(arcane)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pact magic.
trait_source(match_class(warlock), 'pact magic').

%! pact_magic_slots(?N)
%
%  The number (N) of pact magic slots for your warlock character.
pact_magic_slots(N) :-
    class_level(warlock:L),
    ordered_lookup_largest_leq([1 -> 1, 2 -> 2, 11 -> 3, 17 -> 4], L, N).

%! pact_magic_slot_level(?SlotLevel)
%
%  The level of your pact magic spell slots.
pact_magic_slot_level(N) :-
    class_level(warlock:L),
    ordered_lookup_largest_leq([1 -> 1, 3 -> 2, 5 -> 3, 7 -> 4, 9 -> 5], L, N).

resource('pact magic', pact_slot(L), N) :-
    pact_magic_slots(N),
    pact_magic_slot_level(L).
on_rest(short, pact_slot(L), full_restore) :-
    pact_magic_slot_level(L).

% Learning spells.
known_spell(warlock, cha, always, [], no, Name) :-
    class_origin_to_class(Origin, warlock),
    choice_member(Origin, cantrip, Name).
known_spell(warlock, cha, always, ['pact slot'], Ritual, Name) :-
    class_level(warlock:L),
    selected_at_class_level(warlock:L, spell, Name),
    spell_property(Name, ritual, Ritual). % TODO this might be wrong.

% Learn cantrips.
options_source(match_class(warlock), cantrip, 2 unique_from class_cantrip(warlock)).
options_source(match_class(warlock:L), cantrip, class_cantrip(warlock)) :-
    L = 4; L = 10.

% Learn proper spells.
options_source(match_class(warlock), spell,
               2 unique_from learnable_proper_spell(warlock)).
options_source(match_class(warlock:L), spell,
               learnable_proper_spell(warlock)) :-
    between(2, 9, L) ; member(L, [11,13,15,17,19]).

% Replace proper spells.
options_source(match_class(warlock:L), replace(spell),
               selected_at_class_level(warlock:Prev, spell)) :-
    between(2, 20, L),
    Prev is L-1.
options(match_class(warlock:L), replacing(spell, Name),
        learnable_proper_spell(warlock)) :-
    choice_member(match_class(warlock:L), replace(spell), Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Eldritch invocations.

replaceable_class_options(warlock:2, 'eldritch invocation',
                          2 unique_from eldritch_invocation_option).
replaceable_class_options(warlock:L, 'eldritch invocation',
                          eldritch_invocation_option) :-
    member(L, [5,7,9,12,15,18]).
replace_at_class_level(warlock:L, 'eldritch invocation', 1, eldritch_invocation_option) :-
    between(3, 20, L).

% Actually add the selected invocations as traits.
trait(match_class(warlock:Level), eldritch_invocation(Inv)) :-
    find_choice_level(warlock:Level, 'eldritch invocation', Inv).

% Eldritch invocation options and effects.
eldritch_invocation_option('agonizing blast') :-
    known_spell(warlock, 'eldritch blast').
bonus_source(trait(eldritch_invocation('agonizing blast')),
             modify_spell(_, 'eldritch blast', spell_damage_bonus(Mod))) :-
    ability_mod(cha, Mod).

eldritch_invocation_option('armor of shadows').
known_spell(warlock, cha, always, [], no, 'mage armor') :-
    trait(eldritch_invocation('armor of shadows')).

eldritch_invocation_option('ascendant step') :-
    match_class(warlock:9).
known_spell(warlock, cha, always, [], no, levitate) :-
    trait(eldritch_invocation('ascendant step')).

eldritch_invocation_option('beast speech').
known_spell(warlock, cha, always, [], no, 'speak with animals') :-
    trait(eldritch_invocation('beast speech')).

eldritch_invocation_option('beguiling influence').
traits_from_source(trait(eldritch_invocation('beguiling influence')),
                   [skill(deception), skill(persuasion)]).

eldritch_invocation_option('bewitching whispers') :-
    match_class(warlock:7).
known_spell(warlock, cha, always, ['pact slot', per_rest(long, 1)], no, compulsion) :-
    trait(eldritch_invocation('bewitching whispers')).

%todo from book of ancient secrets
