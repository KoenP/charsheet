:- discontiguous eldritch_invocation_option/1.

class_option(warlock).
hd_per_level(warlock, 1 d 8).
initial_class_base_hp(warlock, 8).
max_hp_per_level(warlock, 1 d 8).
caster(warlock, 0).
spellcasting_ability(warlock, cha).
choose_subclass_level(warlock:1).
asi_level(warlock:L) :-
    default_asi_level(L).
class_saving_throw(warlock, wis).
class_saving_throw(warlock, cha).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features gained for picking this class as initial class.
trait_options_source(^warlock, skill, wrap(skill),
                     2 unique_from from_list(
                         [arcana, deception, history, intimidation,
                          investigation, nature, religion])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features from leveling up (except pact magic and pact boons, which
% have their own sections).
traits_from_source(warlock >: 1, [armor(light), weapon(simple)]).
trait_source(warlock >: 1, spellcasting_focus(arcane)).

% Mystic arcanum.
trait_source(warlock >: 11, 'mystic arcanum').
options_source(warlock >: WarlockLevel, 'arcanum spell',
               [Spell]>>spell_property(Spell, level, SpellLevel)) :-
    member(WarlockLevel-SpellLevel, [11-6, 13-7, 15-8, 17-9]).
known_spell(warlock('mystic arcanum'), cha, always, [per_rest(long, 1)], no, Spell) :-
    choice_member(_, 'arcanum spell', Spell).
meta_todo('mystic arcanum', "check whether it's ritual (now defaulting to no)").

% Eldritch master.
trait_source(warlock >: 20, 'eldritch master').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pact magic.
trait_source(warlock >: 1, 'pact magic').

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

% Learning spells.
known_spell(warlock, cha, always, [], no, Name) :-
    class_origin_to_class(Origin, warlock),
    choice_member(Origin, cantrip, Name).
known_spell(warlock, cha, always, [slot], Ritual, Name) :-
    class_level(warlock:L),
    selected_at_class_level(warlock:L, spell, Name),
    spell_property(Name, ritual, Ritual). % TODO this might be wrong.
hide_known_class_spells(warlock >: _, cantrip, warlock).
hide_known_class_spells(warlock >: _, spell, warlock).

% Learn cantrips.
options_source(warlock >: 1, cantrip, 2 unique_from class_cantrip(warlock)).
options_source(warlock >: L, cantrip, class_cantrip(warlock)) :-
    L = 4; L = 10.
%hide_base_option(warlock >: _, cantrip, Cantrip) :-
%    known_spell(Origin, Cantrip),
%    Origin =.. [warlock|_].

% Learn proper spells.
options_source(warlock >: 1, spell,
               2 unique_from learnable_proper_spell(warlock)).
options_source(warlock >: L, spell,
               learnable_proper_spell(warlock)) :-
    between(2, 9, L) ; member(L, [11,13,15,17,19]).
%hide_base_option(warlock >: _, spell, Spell) :-
%    known_spell(Origin, Spell),
%    Origin =.. [warlock|_].

% Replace proper spells.
options_source(warlock >: L, replace(spell),
               selected_at_class_level(warlock:Prev, spell)) :-
    between(2, 20, L),
    Prev is L-1.
options(warlock >: L, replacing(spell, Name),
        learnable_proper_spell(warlock)) :-
    choice_member(warlock >: L, replace(spell), Name).
%hide_base_option(warlock >: _, replacing(spell,Old), Spell) :-
%    (known_spell(Origin, Spell),Origin =.. [warlock|_])
%    ; Spell = Old.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pact boons.
trait_options_source(warlock >: 3,
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

% TODO this is a workaround for a bug.
custom_format(modify_spell_field(effects, [_,_]>>append(_, ["extra warlock forms: imp, pseudodragon, quasit, sprite"], _))) --> ["extra warlock forms: imp, pseudodragon, quasit, sprite"].
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
custom_format(eldritch_invocation(Inv)) -->
    ['eldritch invocation: '], [Inv].

% Shorthands.
eldinv(Inv) :- trait(eldritch_invocation(Inv)).

:- discontiguous eldinv_deletes_spell_component/2.
delete_component_source(trait(eldritch_invocation(Inv)),
                        warlock:eldritch_invocation(Inv),
                        _,
                        Component) :-
    eldinv_deletes_spell_component(Inv, Component).

:- discontiguous eldinv_spell/3.
known_spell(warlock(eldritch_invocation(Inv)),
            cha, always, Resources, Ritual, Name) :-
    eldinv_spell(Inv, Resources, Name),
    trait(eldritch_invocation(Inv)),
    spell_property(Name, ritual, Ritual).


% Actually add the selected invocations as traits.
trait_source(warlock >: Level, eldritch_invocation(Inv)) :-
    find_choice_level(warlock:Level, 'eldritch invocation', Inv).

% Need a manual rule to lookup the doc for each invocation.
lookup_option_doc(warlock >: _, 'eldritch invocation', Inv, Doc) :-
    (eldritch_invocation(Inv) ?= Doc).

% Eldritch invocation options and effects.

eldritch_invocation_option('agonizing blast') :-
    known_spell(warlock, 'eldritch blast').
bonus_source(trait(eldritch_invocation('agonizing blast')),
             modify_spell(_, 'eldritch blast', increase_all_spell_damage_rolls(Mod))) :-
    ability_mod(cha, Mod).

eldritch_invocation_option('armor of shadows').
known_spell(warlock(eldritch_invocation('armor of shadows')), cha, always, [], no, 'mage armor') :-
    trait(eldritch_invocation('armor of shadows')).
custom_format(known_spell(warlock(eldritch_invocation('armor of shadows')), 'mage armor')) -->
    ['armor of shadows'].


eldritch_invocation_option('ascendant step') :-
    warlock >: 9.
known_spell(warlock(eldritch_invocation('ascendant step')),
            cha, always, [], no, levitate) :-
    trait(eldritch_invocation('ascendant step')).

eldritch_invocation_option('beast speech').
known_spell(warlock(eldritch_invocation('beast speech')),
            cha, always, [], no, 'speak with animals') :-
    trait(eldritch_invocation('beast speech')).

eldritch_invocation_option('beguiling influence').
traits_from_source(trait(eldritch_invocation('beguiling influence')),
                   [skill(deception), skill(persuasion)]).

eldritch_invocation_option('bewitching whispers') :-
    warlock >: 7.
known_spell(warlock(eldritch_invocation('bewitching whispers')),
            cha, always, ['pact slot', per_rest(long, 1)], no, compulsion) :-
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
known_spell(warlock(eldritch_invocation('book of ancient secrets')),
            cha, always, [], only, Ritual) :-
    choice_member(trait(eldritch_invocation('book of ancient secrets')),
                  ritual,
                  Ritual).
% TODO: add transcribe option

eldritch_invocation_option('chains of carceri') :-
    warlock >: 15,
    trait(pact_boon(chain)).
known_spell(warlock(eldritch_invocation('chains of carceri')),
            cha, always, [], no, 'hold monster') :-
    trait(eldritch_invocation('chains of carceri')).
bonus_source(trait(eldritch_invocation('chains of carceri')),
             modify_spell(warlock:eldritch_invocation('chains of carceri'),
                          'hold monster',
                          chains_of_carceri_spell_mod)).
chains_of_carceri_spell_mod(OldData, NewData) :-
    get_or_default(OldData, effects, [], OldEffects),
    append(OldEffects,
           ["only target celestial, fiend, or elemental",
            "long rest before you use this invocation on the same creature again"],
           NewEffects),
    NewData = OldData.put(components, [])
                     .put(effects, NewEffects).

eldritch_invocation_option('devil\'s sight').
trait_source(trait(eldritch_invocation('devil\'s sight')),
             sense('devil\'s sight')).

eldritch_invocation_option('dreadful word') :-
    warlock >: 7.
known_spell(warlock(eldritch_invocation('dreadful word')),
            cha, always, ['pact slot', per_rest(long,1)], no, confusion) :-
    eldinv('dreadful word').

eldritch_invocation_option('eldritch sight').
known_spell(warlock(eldritch_invocation('eldritch sight')),
            cha, always, [], no, 'detect magic') :-
    eldinv('eldritch sight').

eldritch_invocation_option('eldritch spear') :-
    known_spell(warlock, 'eldritch blast').
bonus_source(trait(eldritch_invocation('eldritch spear')),
             modify_spell(_, 'eldritch blast',
                          modify_spell_field(range, const(feet(300))))).

eldritch_invocation_option('eyes of the rune keeper').

eldritch_invocation_option('gaze of two minds').

eldritch_invocation_option(lifedrinker) :-
    warlock >: 12,
    trait(pact_boon(blade)).
% TODO: lifedrinker damage bonus

eldritch_invocation_option('mask of many faces').
known_spell(warlock(eldritch_invocation('mask of many faces')),
            cha, always, [], no, 'disguise self') :-
    eldinv('mask of many faces').

eldritch_invocation_option('master of myriad forms') :-
    warlock >: 15.
known_spell(warlock(eldritch_invocation('master of myriad forms')),
            cha, always, [], no, 'alter self') :-
    eldinv('master of myriad forms').

eldritch_invocation_option('minions of chaos') :-
    warlock >: 9.
eldinv_spell('minions of chaos', ['pact slot', per_rest(long,1)], 'conjure elemental').

eldritch_invocation_option('mire the mind') :-
    warlock >: 5.
known_spell(warlock(eldritch_invocation('mire the mind')),
            cha, always, ['pact slot', per_rest(long,1)], no, slow) :-
    eldinv('mire the mind').

eldritch_invocation_option('misty visions').
eldinv_spell('misty visions', [], 'silent image').
eldinv_deletes_spell_component('misty visions', m(_)).

eldritch_invocation_option('one with shadows').
% TODO: add an "action"

eldritch_invocation_option('otherwordly leap') :-
    warlock >: 9.
eldinv_spell('otherworldly leap', [], jump).
eldinv_deletes_spell_component('otherworldly leap', m(_)).

eldritch_invocation_option('repelling blast') :-
    known_spell(_, 'eldritch blast').
bonus_source(trait(eldritch_invocation('repelling blast')),
             modify_spell(_, 'eldritch blast', Goal)) :-
    Goal = modify_spell_field(effects, apply_repelling_blast).
apply_repelling_blast(OldEffects, NewEffects) :-
    select_subterm(spell_attack_roll(ranged):E1,
                   OldEffects,
                   spell_attack_roll(ranged):E2,
                   NewEffects),
    nonlist_to_singleton(E1, L),
    append(L, ["push up to 10 ft away"], E2).

eldritch_invocation_option('sculptor of flesh') :-
    warlock >: 7.
eldinv_spell('sculptor of flesh', ['pact slot', per_rest(long,1)], polymorph).

eldritch_invocation_option('sign of ill omen') :-
    warlock >: 5.
eldinv_spell('sign of ill omen', ['pact slot', per_rest(long,1)], 'bestow curse').

eldritch_invocation_option('thief of five fates').
eldinv_spell('thief of five fates', ['pact slot', per_rest(long,1)], bane).

eldritch_invocation_option('thirsting blade') :-
    warlock >: 5,
    trait(pact_boon(blade)).
% TODO

eldritch_invocation_option('visions of distant realms') :-
    warlock >: 15.
eldinv_spell('visions of distant realms', [], 'arcane eye').

eldritch_invocation_option('voice of the chain master') :-
    trait(pact_boon(chain)).
% TODO

eldritch_invocation_option('whispers of the grave') :-
    warlock >: 9.
eldinv_spell('whispers of the grave', [], 'speak with dead').

eldritch_invocation_option('witch sight') :-
    warlock >: 15.
trait_source(trait(eldritch_invocation('witch sight')), sense('witch sight')).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Patron: the fiend.
subclass_option(warlock, fiend).
extend_class_spell_list(warlock, Spell) :-
    subclass(warlock(fiend)),
    member(Spell, ['burning hands', command, 'blindness/deafness',
                  'scorching ray', fireball, 'stinking cloud', 'fire shield',
                  'wall of fire', 'flame strike', hallow]).

trait_source(warlock(fiend) >: 1, 'dark one\'s blessing').
trait_source(warlock(fiend) >: 6, 'dark one\'s own luck').
res('dark one\'s own luck', 1) :-
    trait('dark one\'s own luck').
trait_source(warlock(fiend) >: 10, 'fiendish resilience').
trait_source(warlock(fiend) >: 14, 'hurl through hell').


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

sense('devil\'s sight') ?= "You can see normally in darkness, both magical and nonmagical, to a distance of 120 feet.".

eldritch_invocation('eyes of the rune keeper') ?= "You can read all writing.".

eldritch_invocation('gaze of two minds') ?= "You can use your action to touch a willing humanoid and perceive through its senses until the end of your next turn. As long as the creature is on the same plane of existence as you, you can use your action on subsequent turns to maintain this connection, extending the duration until the end of your next turn. While perceiving through the other creature's senses, you benefit from any special senses possessed by that creature, and you are blinded and deafened to your own surroundings.".

(eldritch_invocation('witch sight') ?= Desc) :- (sense('witch sight') ?= Desc).
sense('witch sight') ?= "You can see the true form of any shapechanger or creature concealed by illusion or transmutation magic while the creature is within 30 feet of you and within line of sight.".
