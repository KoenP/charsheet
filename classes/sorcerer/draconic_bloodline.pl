subclass_option(sorcerer, 'draconic bloodline').

dragon_ancestor_option(black, acid).
dragon_ancestor_option(blue, lightning).
dragon_ancestor_option(brass, fire).
dragon_ancestor_option(bronze, lightning).
dragon_ancestor_option(copper, acid).
dragon_ancestor_option(gold, fire).
dragon_ancestor_option(green, poison).
dragon_ancestor_option(red, fire).
dragon_ancestor_option(silver, cold).
dragon_ancestor_option(white, cold).

subclass_trait_options(sorcerer:1, 'draconic bloodline', 'dragon ancestor', 1 from Options) :-
    findall(dragon_ancestor(Option), dragon_ancestor_option(Option,_), Options).
subclass_trait(sorcerer:1, 'draconic bloodline', language(draconic)).
subclass_trait(sorcerer:1, 'draconic bloodline', 'dragon interaction').
subclass_trait(sorcerer:1, 'draconic bloodline', 'draconic resilience').
trait_effect('draconic resilience', natural_resilience(13)).
trait_effect('draconic resilience', max_hp + N) :-
    class_level(sorcerer:N).

% See spell_known_damage/3 documentation for the many concerns around
% automating elemental affinity.
subclass_trait(sorcerer:6, 'draconic bloodline', elemental_affinity(Element)) :-
    trait(dragon_ancestor(Type)),
    dragon_ancestor_option(Type, Element).
spell_single_roll_damage_bonus(Spell, Upcast, _, Mod) :-
    trait(elemental_affinity(Element)),
    ability_mod(cha, Mod),
    spell_damage_rolls(Spell, Upcast, Rolls),
    MatchingRoll =.. [Element, _],
    member(MatchingRoll, Rolls),
    !.

'dragon interaction' ?= "Whenever you make a Charisma check when interacting with dragons, your proficiency bonus is doubled if it applies to the check.".

'elemental affinity' ?= "Starting at 6th level, when you cast a spell that deals damage of the type associated with your draconic ancestry, you can add your Charisma modifier to one damage roll of that spell. At the same time, you can spend 1 sorcery point to gain resistance to that damage type for 1 hour.".
