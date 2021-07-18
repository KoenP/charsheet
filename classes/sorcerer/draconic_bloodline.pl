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

subclass_trait(sorcerer:14, 'draconic bloodline', 'dragon wings').

subclass_trait(sorcerer:18, 'draconic bloodline', 'draconic presence').

'dragon interaction' ?= "Whenever you make a Charisma check when interacting with dragons, your proficiency bonus is doubled if it applies to the check.".

'draconic resilience' ?= "As magic flows through your body, it causes physical traits of your dragon ancestors to emerge. At 1st level, your hit point maximum increases by 1 and increases by 1 again whenever you gain a level in this class.
Additionally, parts of your skin are covered by a thin sheen of dragon-like scales. When you aren't wearing armor, your AC equals 13 + your Dexterity modifier. ".

'elemental affinity' ?= "Starting at 6th level, when you cast a spell that deals damage of the type associated with your draconic ancestry, you can add your Charisma modifier to one damage roll of that spell. At the same time, you can spend 1 sorcery point to gain resistance to that damage type for 1 hour.".

'dragon wings' ?= "At 14th level, you gain the ability to sprout a pair of dragon wings from your back, gaining a flying speed equal to your current speed. You can create these wings as a bonus action on your turn. They last until you dismiss them as a bonus action on your turn.
You can't manifest your wings while wearing armor unless the armor is made to accommodate them, and clothing not made to accommodate your wings might be destroyed when you manifest them.".

'draconic presence '?= "Beginning at 18th level, you can channel the dread presence of your dragon ancestor, causing those around you to become awestruck or frightened. As an action, you can spend 5 sorcery points to draw on this power and exude an aura of awe or fear (your choice) to a distance of 60 feet. For 1 minute or until you lose your concentration (as if you were casting a concentration spell), each hostile creature that starts its turn in this aura must succeed on a Wisdom saving throw or be charmed (if you chose awe) or frightened (if you chose fear) until the aura ends. A creature that succeeds on this saving throw is immune to your aura for 24 hours.".
