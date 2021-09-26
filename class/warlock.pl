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
% Features from leveling up (except pact magic and pact boons, which
% have their own sections).
traits_from_source(match_class(warlock), [armor(light), weapon(simple)]).
trait_source(match_class(warlock), spellcasting_focus(arcane)).

% Mystic arcanum.
trait_source(match_class(warlock:11), 'mystic arcanum').
options_source(match_class(warlock:WarlockLevel), 'arcanum spell',
               [Spell]>>spell_property(Spell, level, SpellLevel)) :-
    member(WarlockLevel-SpellLevel, [11-6, 13-7, 15-8, 17-9]).
known_spell(warlock, cha, always, [per_rest(long, 1)], todo, Spell) :-
    choice_member(_, 'arcanum spell', Spell).

% Eldritch master.
trait_source(match_class(warlock:20), 'eldritch master').

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
% Pact boons.
trait_options_source(match_class(warlock:3),
                     'pact boon',
                     wrap(pact_boon),
                     from_list([chain, blade, tome])).

% Pact of the chain.
known_spell(warlock, cha, always, [], only, 'find familiar') :-
    trait(pact_boon(chain)).
bonus_source(trait(pact_boon(chain)),
             modify_spell(warlock, 'find familiar', Goal)) :-
    ExtraForms = "extra warlock forms: imp, pseudodragon, quasit, sprite",
    Goal = modify_spell_field(effects, [Es1,Es2]>>append(Es1,[ExtraForms],Es2)).
% TODO: add familiar attack action.

% Pact of the blade.
% TODO

% Pact of the tome.
options_source(trait(pact_boon(tome)), 'book of shadows', 3 unique_from cantrip).
known_spell(warlock, cha, always, [], no, Cantrip) :-
    choice_member(trait(pact_boon(tome)), 'book of shadows', Cantrip).

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

eldritch_invocation_option('book of ancient secrets') :-
    trait(pact_boon(tome)).
options_source(trait(eldritch_invocation('book of ancient secrets')),
               ritual,
               2 unique_from ancient_secret_ritual).
ancient_secret_ritual(Ritual) :-
    spell_data(Ritual, Data),
    Data.level = 1,
    Data.ritual = yes. 
known_spell(warlock, cha, always, [], only, Ritual) :-
    choice_member(trait(eldritch_invocation('book of ancient secrets')),
                  ritual,
                  Ritual).
% TODO: add transcribe option

eldritch_invocation_option('chains of carceri') :-
    match_class(warlock:15),
    trait(pact_boon(chain)).
known_spell(warlock, cha, always, [], no, 'hold monster').

%AAAAAH: TODO: hold monster is ook gewoon een warlock spell, moet dus ook als normale variant leerbaar zijn
% project: known_spell Origin moet gesplitst worden in Class en Origin
%bonus_source(trait(eldritch_invocation('chains of carceri')),
%             modify_spell(warlock, 'hold monster', )
%            )

add_spell_effect('hold monster', "only on celestial, fiend, or elemental").
add_spell_effect('hold monster', "must finish long rest before same creature can be targeted again").



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Patron: the fiend.
subclass_option(warlock, fiend).
extend_class_spell_list(warlock, Spell) :-
    subclass(warlock(fiend)),
    member(Spell, ['burning hands', command, 'blindness/deafness',
                  'scorching ray', fireball, 'stinking cloud', 'fire shield',
                  'wall of fire', 'flame strike', hallow]).

trait_source(match_class(warlock:1), 'dark one\'s blessing').
trait_source(match_class(warlock:6), 'dark one\'s own luck').
resource('dark one\'s own luck', 'dark one\'s own luck', 1) :-
    trait('dark one\'s own luck').
trait_source(match_class(warlock:10), 'fiendish resilience').
trait_source(match_class(warlock:14), 'hurl through hell').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pact_boon(chain) ?= "You learn the find familiar spell and can cast it as a ritual. The spell doesn't count against your number of spells known.
When you cast the spell, you can choose one of the normal forms for your familiar or one of the following special forms: imp, pseudodragon, quasit, or sprite.
Additionally, when you take the Attack action, you can forgo one of your own attacks to allow your familiar to make one attack of its own with its reaction.".

pact_boon(blade) ?= "You can use your action to create a pact weapon in your empty hand. You can choose the form that this melee weapon takes each time you create it. You are proficient with it while you wield it. This weapon counts as magical for the purpose of overcoming resistance and immunity to nonmagical attacks and damage.
Your pact weapon disappears if it is more than 5 feet away from you for 1 minute or more. It also disappears if you use this feature again, if you dismiss the weapon (no action required), or if you die.
You can transform one magic weapon into your pact weapon by performing a special ritual while you hold the weapon. You perform the ritual over the course of 1 hour, which can be done during a short rest. You can then dismiss the weapon, shunting it into an extradimensional space, and it appears whenever you create your pact weapon thereafter. You can't affect an artifact or a sentient weapon in this way. The weapon ceases being your pact weapon if you die, if you perform the 1-hour ritual on a different weapon, or if you use a 1-hour ritual to break your bond to it. The weapon appears at your feet if it is in the extradimensional space when the bond breaks.".

pact_boon(tome) ?= "Your patron gives you a grimoire called a Book of Shadows. When you gain this feature, choose three cantrips from any class's spell list (the three needn't be from the same list). While the book is on your person, you can cast those cantrips at will. They don't count against your number of cantrips known. If they don't appear on the warlock spell list, they are nonetheless warlock spells for you.
If you lose your Book of Shadows, you can perform a 1-hour ceremony to receive a replacement from your patron. This ceremony can be performed during a short or long rest, and it destroys the previous book. The book turns to ash when you die.".

'eldritch master' ?= "At 20th level, you can draw on your inner reserve of mystical power while entreating your patron to regain expended spell slots. You can spend 1 minute entreating your patron for aid to regain all your expended spell slots from your Pact Magic feature. Once you regain spell slots with this feature, you must finish a long rest before you can do so again.".

'dark one\'s blessing' ?= "Starting at 1st level, when you reduce a hostile creature to 0 hit points, you gain temporary hit points equal to your Charisma modifier + your warlock level (minimum of 1).".

'dark one\'s own luck' ?= "Starting at 6th level, you can call on your patron to alter fate in your favor. When you make an ability check or a saving throw, you can use this feature to add a d10 to your roll. You can do so after seeing the initial roll but before any of the roll's effects occur.
Once you use this feature, you can't use it again until you finish a short or long rest. ".

'fiendish resilience' ?= "Starting at 10th level, you can choose one damage type when you finish a short or long rest. You gain resistance to that damage type until you choose a different one with this feature. Damage from magical weapons or silver weapons ignores this resistance.".

'hurl through hell' ?= "Starting at 14th level, when you hit a creature with an attack, you can use this feature to instantly transport the target through the lower planes. The creature disappears and hurtles through a nightmare landscape.
At the end of your next turn, the target returns to the space it previously occupied, or the nearest unoccupied space. If the target is not a fiend, it takes 10d10 psychic damage as it reels from its horrific experience.
Once you use this feature, you can't use it again until you finish a long rest. ".
