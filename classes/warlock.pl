:- [warlock/spells].

:- discontiguous learnable_eldritch_invocation/1.

class_option(warlock).
hd_per_level(warlock, 1 d 8).
initial_class_base_hp(warlock, 8).
max_hp_per_level(warlock, 1 d 8).
class_saving_throw(warlock, wis).
class_saving_throw(warlock, cha).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

spellcasting_ability(warlock, cha).
spellcasting_availability(warlock, always_available).

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
    PrevLevel is Level-1,
    findall(Spell, trait_at_class_level(warlock:PrevLevel, _, learn_spell(warlock, Spell)), KnownSpells),
    list_learnable_proper_spells(warlock, NewSpells).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pact Boons.
wrap_class_trait_option(warlock:3, 'pact boon', X, pact_boon(X)).
class_trait_options(warlock:3, 'pact boon', 1 from [chain, blade, tome]).
custom_format(pact_boon(X)) --> ['pact of the '], [X].
trait_origin_level(pact_boon(_), CharLevel) :-
    reached_class_level_on_char_level(warlock:3, CharLevel).

% Pact of the chain.
spell_known('find familiar', warlock, cha, always_available, 'ritual only') :-
    trait(pact_boon(chain)).
spell_known_effect('find familiar', warlock,
                   extra_forms_from_warlock([imp, pseudodragon, quasit, sprite])) :-
    trait(pact_boon(chain)).
pact_boon(chain) ?= "You learn the find familiar spell and can cast it as a ritual. The spell doesn't count against your number of spells known.
When you cast the spell, you can choose one of the normal forms for your familiar or one of the following special forms: imp, pseudodragon, quasit, or sprite.
Additionally, when you take the Attack action, you can forgo one of your own attacks to allow your familiar to make one attack of its own with its reaction.".

% Pact of the blade.
pact_boon(blade) ?= "You can use your action to create a pact weapon in your empty hand. You can choose the form that this melee weapon takes each time you create it. You are proficient with it while you wield it. This weapon counts as magical for the purpose of overcoming resistance and immunity to nonmagical attacks and damage.
Your pact weapon disappears if it is more than 5 feet away from you for 1 minute or more. It also disappears if you use this feature again, if you dismiss the weapon (no action required), or if you die.
You can transform one magic weapon into your pact weapon by performing a special ritual while you hold the weapon. You perform the ritual over the course of 1 hour, which can be done during a short rest. You can then dismiss the weapon, shunting it into an extradimensional space, and it appears whenever you create your pact weapon thereafter. You can't affect an artifact or a sentient weapon in this way. The weapon ceases being your pact weapon if you die, if you perform the 1-hour ritual on a different weapon, or if you use a 1-hour ritual to break your bond to it. The weapon appears at your feet if it is in the extradimensional space when the bond breaks.".
% TODO add to attack table etc.

% Pact of the tome.
pact_boon(tome) ?= "Your patron gives you a grimoire called a Book of Shadows. When you gain this feature, choose three cantrips from any class's spell list (the three needn't be from the same list). While the book is on your person, you can cast those cantrips at will. They don't count against your number of cantrips known. If they don't appear on the warlock spell list, they are nonetheless warlock spells for you.
If you lose your Book of Shadows, you can perform a 1-hour ceremony to receive a replacement from your patron. This ceremony can be performed during a short or long rest, and it destroys the previous book. The book turns to ash when you die.".
wrap_trait_option(pact_boon(tome), cantrip, X, book_of_shadows_spell(X)).
trait_options(pact_boon(tome), cantrip, 3 from AllCantrips) :-
    trait(pact_boon(tome)),
    findall(Cantrip, spell(Cantrip, level, 0), AllCantrips).
spell_known(Cantrip, warlock, cha, always_available, at_will) :-
    trait(book_of_shadows_spell(Cantrip)).
spell_known_effect(Cantrip, warlock, 'from the book of shadows') :-
    trait(book_of_shadows_spell(Cantrip)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Eldritch invocations.
wrap_class_trait_option(warlock:_, 'eldritch invocation',
                        X, eldritch_invocation(X)).
class_trait_options(warlock:L, 'eldritch invocation', N from Invocations) :-
    member(L, [2,5,7,9,12,15,18]),
    (L = 2 -> N = 2; L \= 2 -> N = 1),
    findall(Invocation, learnable_eldritch_invocation(Invocation), Invocations).

learnable_eldritch_invocation('agonizing blast') :-
    spell_known('eldritch blast', warlock, _, _, _).
spell_single_roll_damage_bonus('eldritch blast', _, warlock, Mod) :-
    trait(eldritch_invocation('agonizing blast')),
    ability_mod(cha, Mod).
source('agonizing blast', phb(110)).

learnable_eldritch_invocation('armor of shadows').
spell_known('mage armor', warlock, cha, always_available, at_will) :-
    trait(eldritch_invocation('armor of shadows')).

learnable_eldritch_invocation('beast speech').
spell_known('speak with animals', warlock, cha, always_available, at_will) :-
    trait(eldritch_invocation('beast speech')).

learnable_eldritch_invocation('beguiling influence').
trait_effect(eldritch_invocation('beguiling influence'), skill(deception)).
trait_effect(eldritch_invocation('beguiling influence'), skill(persuasion)).

learnable_eldritch_invocation('bewitching whispers') :-
    matching_class_level(warlock:7).
spell_known(compulsion, warlock, cha, always_available, [pact_magic_slot, per_long_rest(1)]) :-
    trait(eldritch_invocation('bewitching whispers')).

%learnable_eldritch_invocation
