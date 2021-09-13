class_option(sorcerer).
hd_per_level(sorcerer, 1 d 6).
initial_class_base_hp(sorcerer, 6).
max_hp_per_level(sorcerer, 1 d 6).
caster(sorcerer, full).
choose_subclass_level(sorcerer:1).

% TODO: asis

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initial class features (you don't gain these when multiclassing into sorcerer).
traits_from_source(initial_class(sorcerer),
                   [weapon(dagger),
                    weapon(dart),
                    weapon(sling),
                    weapon(quarterstaff),
                    weapon('light crossbow')]).

trait_options_source(initial_class(sorcerer), skill, wrap(skill),
                     2 unique_from from_list([arcana       ,
                                              deception    ,
                                              insight      ,
                                              intimidation ,
                                              persuasion   ,
                                              religion     ])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 1 features (you do gain these when multiclassing into sorcerer).
trait_source(class(sorcerer), spellcasting_focus(arcane)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sorcerer spellcasting features.
trait_source(class(sorcerer:2), 'font of magic').
trait_options_source(class(sorcerer:3), metamagic, wrap(metamagic),
                     2 unique_from from_list(['careful spell',
                                              'distant spell',
                                              'empowered spell',
                                              'extended spell',
                                              'heightened spell',
                                              'quickened spell',
                                              'subtle spell',
                                              'twinned spell'])).
trait_source(class(sorcerer:20), 'sorcerous restoration').

%  Calculate the maximum number of sorcery points your character has available.
resource(metamagic, 'sorcery point', Max) :-
    trait(metamagic),
    class_level(sorcerer:Max).
on_rest(long, 'sorcery point', full_restore) :- trait(metamagic).
on_rest(short, 'sorcery point', restore(4)) :- trait('sorcerous restoration').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Learning new sorcerer spells.

known_spell(sorcerer, cha, always, [], no, Name) :-
    class_origin_to_class(Origin, sorcerer),
    choice_member(Origin, cantrip, Name).
known_spell(sorcerer, cha, prepare, [slot], Ritual, Name) :-
    class_level(sorcerer:L),
    selected_at_class_level(sorcerer:L, spell, Name),
    spell_property(Name, ritual, Ritual). % TODO: this might be wrong

% Learn cantrips.
options_source(class(sorcerer), cantrip, 4 unique_from class_cantrip(sorcerer)).
options_source(match_class(sorcerer:L), cantrip, class_cantrip(sorcerer)) :- L=4 ; L=10.
   
% Learn proper spells.
options_source(class(sorcerer), spell,
               2 unique_from learnable_proper_spell(sorcerer)).
options_source(match_class(sorcerer:L), spell,
               learnable_proper_spell(sorcerer)) :-
    member(L, [2,3,4,5,6,7,8,9,10,11,13,15,17]).

% Replace proper spells.
options_source(match_class(sorcerer:L), replace(spell),
               selected_at_class_level(sorcerer:Prev, spell)) :-
    between(2, 20, L),
    Prev is L-1.
options(match_class(sorcerer:L), replacing(spell, Name), learnable_proper_spell(sorcerer)) :-
    choice_member(match_class(sorcerer:L), replace(spell), Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sorcerous origin: draconic bloodline.

subclass_option(sorcerer, 'draconic bloodline').

% Dragon ancestor.
dragon_ancestor_element(black, acid).
dragon_ancestor_element(blue, lightning).
dragon_ancestor_element(brass, fire).
dragon_ancestor_element(bronze, lightning).
dragon_ancestor_element(copper, acid).
dragon_ancestor_element(gold, fire).
dragon_ancestor_element(green, poison).
dragon_ancestor_element(red, fire).
dragon_ancestor_element(silver, cold).
dragon_ancestor_element(white, cold).
dragon_ancestor_element(Elem) :-
    trait(dragon_ancestor(Color)),
    dragon_ancestor_element(Color, Elem).

dragon_ancestor_option(Color) :- dragon_ancestor_element(Color, _).


trait_options_source(match_class(sorcerer('draconic bloodline')),
                     'dragon ancestor',
                     wrap(dragon_ancestor),
                     dragon_ancestor_option).

trait_source(match_class(sorcerer('draconic bloodline')), language(draconic)).

% Draconic resilience.
trait_source(match_class(sorcerer('draconic bloodline')), 'draconic resilience').
bonus_source(trait('draconic resilience'), 'max hp'+Level) :-
    class_level(sorcerer:Level).
bonus_source(trait('draconic resilience'), 'base ac'+3).

% Elemental affinity.
trait_source(match_class(sorcerer('draconic bloodline'):6),
             elemental_affinity(Element)) :-
    trait(dragon_ancestor(Color)),
    dragon_ancestor_element(Color, Element).


bonus_source(trait(elemental_affinity(Element)), modify_spell(_, Name, Goal)) :-
    dragon_ancestor_element(Element),
    known_spell(_, Name),
    spell_property(Name, damage_rolls, [on_hit:ElemDamage]),
    ElemDamage =.. [Element, _],
    Goal = modify_spell_field(damage_rolls, elemental_affinity_update_damage_rolls).

elemental_affinity_update_damage_rolls([on_hit:ElemDamage], [on_hit:NewElemDamage]) :-
    ability_mod(cha, Mod),
    ElemDamage =.. [Element, Damage],
    simplify_dice_sum(Damage+Mod, NewDamage),
    NewElemDamage =.. [Element, NewDamage].

% Dragon wings.
trait_source(match_class(sorcerer('draconic bloodline'):14), 'dragon wings').

% Draconic presence.
trait_source(match_class(sorcerer('draconic bloodline'):18), 'draconic presence').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DESCRIPTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

metamagic('careful spell') ?=
  "When you cast a spell that forces other creatures to make a saving throw, you can protect some of those creatures from the spell's full force. To do so, you spend 1 sorcery point and choose a number of those creatures up to your Charisma modifier (minimum of one creature). A chosen creature automatically succeeds on its saving throw against the spell.".
metamagic('distant spell') ?=
  "When you cast a spell that has a range of 5 feet or greater, you can spend 1 sorcery point to double the range of the spell.
When you cast a spell that has a range of touch, you can spend 1 sorcery point to make the range of the spell 30 feet.".
metamagic('empowered spell') ?=
  "When you roll damage for a spell, you can spend 1 sorcery point to reroll a number of the damage dice up to your Charisma modifier (minimum of one). You must use the new rolls.
You can use Empowered Spell even if you have already used a different Metamagic option during the casting of the spell.".
metamagic('extended spell') ?=
  "When you cast a spell that has a duration of 1 minute or longer, you can spend 1 sorcery point to double its duration, to a maximum duration of 24 hours.".
metamagic('heightened spell') ?=
  "When you cast a spell that forces a creature to make a saving throw to resist its effects, you can spend 3 sorcery points to give one target of the spell disadvantage on its first saving throw made against the spell.".
metamagic('quickened spell') ?=
  "When you cast a spell that has a casting time of 1 action, you can spend 2 sorcery points to change the casting time to 1 bonus action for this casting.".
metamagic('subtle spell') ?=
  "When you cast a spell, you can spend 1 sorcery point to cast it without any somatic or verbal components.".
metamagic('twinned spell') ?=
  "When you cast a spell that targets only one creature and doesn't have a range of self, you can spend a number of sorcery points equal to the spell's level to target a second creature in range with the same spell (1 sorcery point if the spell is a cantrip).
To be eligible, a spell must be incapable of targeting more than one creature at the spell's current level. For example, magic missile and scorching ray aren't eligible, but ray of frost and chromatic orb are. ".

'font of magic' ?= "At 2nd level, you tap into a deep wellspring of magic within yourself. This wellspring is represented by sorcery points, which allow you to create a variety of magical effects.
Sorcery Points

You have 2 sorcery points, and you gain more as you reach higher levels, as shown in the Sorcery Points column of the Sorcerer table. You can never have more sorcery points than shown on the table for your level. You regain all spent sorcery points when you finish a long rest.
Flexible Casting

You can use your sorcery points to gain additional spell slots, or sacrifice spell slots to gain additional sorcery points. You learn other ways to use your sorcery points as you reach higher levels.
Creating Spell Slots. You can transform unexpended sorcery points into one spell slot as a bonus action on your turn. The Creating Spell Slots table shows the cost of creating a spell slot of a given level. You can create spell slots no higher in level than 5th.
Any spell slot you create with this feature vanishes when you finish a long rest.
Spell Points
1st      2
2nd      3
3rd      5
4th      6
5th      7

Converting a Spell Slot to Sorcery Points. As a bonus action on your turn, you can expend one spell slot and gain a number of sorcery points equal to the slot's level.".

dragon_ancestor(_) ?= "You have a specific dragon type as your ancestor. Whenever you make a Charisma check when interacting with dragons, your proficiency bonus is doubled if it applies to the check.".

'draconic resilience' ?= "As magic flows through your body, it causes physical traits of your dragon ancestors to emerge. At 1st level, your hit point maximum increases by 1 and increases by 1 again whenever you gain a level in this class.
Additionally, parts of your skin are covered by a thin sheen of dragon-like scales. When you aren't wearing armor, your AC equals 13 + your Dexterity modifier. ".

elemental_affinity(_) ?= "Starting at 6th level, when you cast a spell that deals damage of the type associated with your draconic ancestry, you can add your Charisma modifier to one damage roll of that spell. At the same time, you can spend 1 sorcery point to gain resistance to that damage type for 1 hour.".

'dragon wings' ?= "At 14th level, you gain the ability to sprout a pair of dragon wings from your back, gaining a flying speed equal to your current speed. You can create these wings as a bonus action on your turn. They last until you dismiss them as a bonus action on your turn.
You can't manifest your wings while wearing armor unless the armor is made to accommodate them, and clothing not made to accommodate your wings might be destroyed when you manifest them.".

'draconic presence '?= "Beginning at 18th level, you can channel the dread presence of your dragon ancestor, causing those around you to become awestruck or frightened. As an action, you can spend 5 sorcery points to draw on this power and exude an aura of awe or fear (your choice) to a distance of 60 feet. For 1 minute or until you lose your concentration (as if you were casting a concentration spell), each hostile creature that starts its turn in this aura must succeed on a Wisdom saving throw or be charmed (if you chose awe) or frightened (if you chose fear) until the aura ends. A creature that succeeds on this saving throw is immune to your aura for 24 hours.".

'dragon wings' ?= "At 14th level, you gain the ability to sprout a pair of dragon wings from your back, gaining a flying speed equal to your current speed. You can create these wings as a bonus action on your turn. They last until you dismiss them as a bonus action on your turn.
You can't manifest your wings while wearing armor unless the armor is made to accommodate them, and clothing not made to accommodate your wings might be destroyed when you manifest them.".

'draconic presence' ?= "Beginning at 18th level, you can channel the dread presence of your dragon ancestor, causing those around you to become awestruck or frightened. As an action, you can spend 5 sorcery points to draw on this power and exude an aura of awe or fear (your choice) to a distance of 60 feet. For 1 minute or until you lose your concentration (as if you were casting a concentration spell), each hostile creature that starts its turn in this aura must succeed on a Wisdom saving throw or be charmed (if you chose awe) or frightened (if you chose fear) until the aura ends. A creature that succeeds on this saving throw is immune to your aura for 24 hours.".
