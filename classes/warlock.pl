:- [warlock/spells].

class_option(warlock).
hd_per_level(warlock, 1 d 8).
initial_class_base_hp(warlock, 8).
max_hp_per_level(warlock, 1 d 8).
class_saving_throw(warlock, wis).
class_saving_throw(warlock, cha).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pact_magic_slots(N) :-
    class_level(warlock:L),
    ordered_lookup_largest_leq([1 -> 1, 2 -> 2, 11 -> 3, 17 -> 4], L, N).
pact_magic_slot_level(N) :-
    class_level(warlock:L),
    ordered_lookup_largest_leq([1 -> 1, 3 -> 2, 5 -> 3, 7 -> 4, 9 -> 5], L, N).

class_trait(warlock:1, armor(light)).
class_trait(warlock:1, weapon(simple)).

wrap_class_trait_option(warlock:1, skill, X, skill(X)).
class_trait_options(warlock:1, skill,
                    2 from [ arcana, deception, history,
                             intimidation, investigation,
                             nature, religion ]).

% Learn warlock cantrips.
wrap_class_trait_option(warlock:_, cantrip, X, learn_spell(warlock,X)).
class_trait_options(warlock:L, cantrip, N from Cantrips) :-
    member(L, [1,4,10]),
    (L = 1 -> N = 2; L \= 1 -> N = 1),
    list_class_cantrips(warlock, Cantrips).

% Learn warlock spells.
wrap_class_trait_option(warlock:_, spell, X, learn_spell(warlock,X)).
class_trait_options(warlock:L, spell, N from Spells) :-
    member(L, [1,2,3,4,5,6,7,8,9,11,13,15,17,19]),
    (L = 1 -> N = 2; L \= 1 -> N = 1),
    list_learnable_proper_spells(warlock, Spells).

wrap_class_trait_replacement_option(warlock:_, spell,
                                    Old, learn_spell(warlock, Old),
                                    New, learn_spell(warlock, New)).
class_trait_replacement_options(warlock:Level,
                                spell,
                                1 from KnownSpells,
                                1 from NewSpells) :-
    between(2, 20, Level),
    findall(Spell, replaceable_spell_at_level(warlock:Level,Spell), KnownSpells),
    list_learnable_proper_spells(warlock, NewSpells).

replaceable_spell_at_level(Class:Level, Spell) :-
    trait(Origin, learn_spell(Class, Spell)),
    class_level_origin(Class:OriginLevel, Origin),
    between(1, 20, Level), % ground Level
    OriginLevel < Level.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Eldritch invocations.
class_trait_options(warlock:L, 'eldritch invocation', N from Invocations) :-
    member(L, [2,5,7,9,12,15,18]),
    (L = 2 -> N = 2; L \= 2 -> N = 1),
    findall(Invocation, learnable_eldritch_invocation(Invocation), Invocations).

learnable_eldritch_invocation('agonizing blast') :-
    spell_known('eldritch blast', warlock, _, _, _).
spell_single_roll_damage_bonus('eldritch blast', _, warlock, Mod) :-
    ability_mod(cha, Mod).


