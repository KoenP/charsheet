class_option(wizard).
hd_per_level(wizard, 1 d 6).
initial_class_base_hp(wizard, 6).
max_hp_per_level(wizard, 1 d 6).
caster(wizard, full).
spellcasting_ability(wizard, int).
max_prepared_spells(wizard, N) :-
    default_max_prepared_spells(wizard, N).
choose_subclass_level(wizard:2).
asi_level(wizard:L) :-
    default_asi_level(L).
class_saving_throw(wizard, int).
class_saving_throw(wizard, wis).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initial class features.
traits_from_source(^wizard,
                   [weapon(dagger), weapon(dart), weapon(sling),
                    weapon(quarterstaff), weapon('light crossbow')]).

trait_options_source(^wizard, skill, wrap(skill),
                     2 unique_from from_list([arcana,
                                              history,
                                              insight,
                                              investigation,
                                              medicine,
                                              religion])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
trait_source(wizard >: 1, spellbook).
trait_source(wizard >: 1, spellcasting_focus(arcane)).
trait_source(wizard >: 1, 'ritual casting'(wizard)).
trait_source(wizard >: 1, 'arcane recovery'(N)) :-
    class_level(wizard:L),
    N is ceil(L/2).
res('arcane recovery', N) :-
    trait('arcane recovery'(N)).
restore_res('long rest', 'arcane recovery', 'full restore') :-
    trait('arcane recovery'(_)).

% Spell mastery.
spell_mastery_candidate(Level, Spell) :-
    known_spell(wizard, Spell),
    spell_property(Spell, level, Level).
trait_options_source(wizard >: 18,
                     spell_mastery(L),
                     wrap(spell_mastery),
                     spell_mastery_candidate(L)) :-
    member(L, [1,2]).
bonus_source(trait(spell_mastery(Spell)),
             modify_spell(wizard, Spell, Goal)) :-
    Goal = modify_spell_field(effects,
                              append_to_list("spell mastery")).

% Signature spells.
signature_spell_candidate(Spell) :-
    known_spell(wizard, Spell),
    spell_property(Spell, level, 3).
trait_options_source(wizard >: 20,
                     'signature spell', wrap(signature_spell),
                     2 unique_from signature_spell_candidate).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Learning wizard spells
known_spell(wizard, int, always, [], no, Name) :-
    class_origin_to_class(Origin, wizard),
    choice_member(Origin, cantrip, Name).
known_spell(wizard, int, Availability, Resources, Ritual, Name) :-
    class_origin_to_class(Origin, wizard),
    member(ChoiceID, [spell, 'free spell', 'copy spells into spellbook']),
    choice_member(Origin, ChoiceID, Name),
    wizard_spell_resources(Name, Resources),
    wizard_spell_availability(Name, Availability),
    spell_property(Name, ritual, Ritual).

hide_known_class_spells(wizard >: _, cantrip, wizard).
hide_known_class_spells(wizard >: _, 'free spell', wizard).
hide_known_class_spells(wizard >: _, 'copy spells into spellbook', wizard).

wizard_spell_resources(Spell, []) :-
    trait(spell_mastery(Spell)), !.
wizard_spell_resources(Spell, [per_rest(short,1)] or [slot]) :-
    trait(signature_spell(Spell)), !.
wizard_spell_resources(_, [slot]).

wizard_spell_availability(Spell, always) :-
    trait(signature_spell(Spell)), !.
wizard_spell_availability(_, 'when prepared').

% Learn cantrips.
options_source(wizard >: 1, cantrip, 3 unique_from class_cantrip(wizard)).
options_source(wizard >: L, cantrip, class_cantrip(wizard)) :-
    L=4 ; L=10.

% Learn proper spells by leveling.
options_source(wizard >: 1, 'free spell',
               6 unique_from learnable_proper_spell(wizard)).
options_source(wizard >: L, 'free spell',
               2 unique_from learnable_proper_spell(wizard)) :-
    between(2, 20, L).

problem(selected_same_wizard_spell_more_than_once(Spell)) :-
    member(Id1, [spell, 'free spell']),
    choice_member(Origin1, Id1, Spell),
    member(Id2, [spell, 'free spell']),
    choice_member(Origin2, Id2, Spell),
    Origin1 \= Origin2.

% Copying into spellbook.
% Wizards can learn an arbitrary number of spells at each level by copying
% into their spellbook.
options_source(wizard >: L, 'copy spells into spellbook',
               unlimited unique_from learnable_proper_spell(wizard)) :-
    between(1, 20, L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Arcane tradition: school of evocation
subclass_option(wizard, evocation).

trait_source(wizard(evocation) >: 2, 'evocation savant').
trait_source(wizard(evocation) >: 2, 'sculpt spells').

trait_source(wizard(evocation) >: 6, 'potent cantrip'). % TODO
bonus_source(trait('potent cantrip'), modify_spell(_, Cantrip, Goal)) :-
    known_spell(_, Cantrip),
    spell_data(Cantrip, Data),
    Data.level = 0,
    subterm_member(saving_throw(_):damage(_,_), Data.effects),
    Goal = modify_spell_field(effects, apply_potent_cantrip).
apply_potent_cantrip(OldEffects, NewEffects) :-
    select_subterm(saving_throw(Abi):damage(Element,Dice),
                   OldEffects,
                   saving_throw(Abi):(damage(Element,Dice) else 'half damage'),
                   NewEffects).
custom_format(modify_spell_field(effects, apply_potent_cantrip)) -->
    ["If target saves, you deal half damage."].
    
trait_source(wizard(evocation) >: 10, 'empowered evocation').
bonus_source(trait('empowered evocation'),
             modify_spell(wizard, Spell, Goal)) :-
    known_spell(wizard, Spell),
    spell_data(Spell, Data),
    Data.school = evocation,
    subterm_member(damage(_,_), Data.effects), 
    ability_mod(int, Bonus),
    Goal = modify_spell_field(effects, apply_empowered_evocation(Bonus)).
apply_empowered_evocation(Bonus, OldEffects, NewEffects) :-
    \+ contains_multiple_damage_rolls(OldEffects),
    select_subterm(damage(Element, Dice), OldEffects,
                   damage(Element, NewDice), NewEffects),
    simplify_dice_sum(Dice + Bonus, NewDice).
apply_empowered_evocation(Bonus, OldEffects, NewEffects) :-
    contains_multiple_damage_rolls(OldEffects),
    atomics_to_string(["add +", Bonus, " to one damage roll"], New),
    append(OldEffects, [New], NewEffects).

custom_format(modify_spell_field(effects, apply_empowered_evocation(Bonus))) -->
    ["Add +"], [Bonus], [" damage to one damage roll."].

trait_source(wizard(evocation) >: 14, overchannel).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Arcane tradition: school of evocation
subclass_option(wizard, divination).

trait_source(wizard(divination) >: 2, 'divination savant').
trait_source(wizard(divination) >: 2, portent).
trait_source(wizard(divination) >: 6, 'expert divination').
trait_source(wizard(divination) >: 10, 'the third eye').
trait_source(wizard(divination) >: 14, 'greater portent').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'ritual casting'(wizard) ?= "You can cast a wizard spell as a ritual if that spell has the ritual tag and you have the spell in your spellbook. You don't need to have the spell prepared.".

'arcane recovery'(_) ?= "You have learned to regain some of your magical energy by studying your spellbook. Once per day when you finish a short rest, you can choose expended spell slots to recover. The spell slots can have a combined level that is equal to or less than half your wizard level (rounded up), and none of the slots can be 6th level or higher.
For example, if you're a 4th-level wizard, you can recover up to two levels worth of spell slots. You can recover either a 2nd-level spell slot or two 1st-level spell slots.".

spellbook ?= ["The spells that you add to your spellbook as you gain levels reflect the arcane research you conduct on your own, as well as intellectual breakthroughs you have had about the nature of the multiverse. You might find other spells during your adventures. You could discover a spell recorded on a scroll in an evil wizard's chest, for example, or in a dusty tome in an ancient library.

**Copying a Spell into the Book.** When you find a wizard spell of 1st level or higher, you can add it to your spellbook if it is of a spell level you can prepare and if you can spare the time to decipher and copy it. Copying that spell into your spellbook involves reproducing the basic form of the spell, then deciphering the unique system of notation used by the wizard who wrote it. You must practice the spell until you understand the sounds or gestures required, then transcribe it into your spellbook using your own notation.
For each level of the spell, the process takes 2 hours and costs 50 gp. The cost represents material components you expend as you experiment with the spell to master it, as well as the fine inks you need to record it. Once you have spent this time and money, you can prepare the spell just like your other spells.",

"**Replacing the Book.** You can copy a spell from your own spellbook into another book - for example, if you want to make a backup copy of your spellbook. This is just like copying a new spell into your spellbook, but faster and easier, since you understand your own notation and already know how to cast the spell. You need spend only 1 hour and 10 gp for each level of the copied spell.
If you lose your spellbook, you can use the same procedure to transcribe the spells that you have prepared into a new spellbook. Filling out the remainder of your spellbook requires you to find new spells to do so, as normal. For this reason, many wizards keep backup spellbooks in a safe place.

**The Book's Appearance.** Your spellbook is a unique compilation of spells, with its own decorative flourishes and margin notes. It might be a plain, functional leather volume that you received as a gift from your master, a finely bound gilt-edged tome you found in an ancient library, or even a loose collection of notes scrounged together after you lost your previous spellbook in a mishap."].

spell_mastery(_) ?= "At 18th level, you have achieved such mastery over certain spells that you can cast them at will. Choose a 1st-level wizard spell and a 2nd-level wizard spell that are in your spellbook. You can cast those spells at their lowest level without expending a spell slot when you have them prepared. If you want to cast either spell at a higher level, you must expend a spell slot as normal.
By spending 8 hours in study, you can exchange one or both of the spells you chose for different spells of the same levels.".

signature_spell(_) ?= "When you reach 20th level, you gain mastery over two powerful spells and can cast them with little effort. Choose two 3rd-level wizard spells in your spellbook as your signature spells. You always have these spells prepared, they don't count against the number of spells you have prepared, and you can cast each of them once at 3rd level without expending a spell slot. When you do so, you can't do so again until you finish a short or long rest.
If you want to cast either spell at a higher level, you must expend a spell slot as normal.".

'evocation savant' ?= "Beginning when you select this school at 2nd level, the gold and time you must spend to copy an evocation spell into your spellbook is halved.".

'sculpt spells' ?= "Beginning at 2nd level, you can create pockets of relative safety within the effects of your evocation spells. When you cast an evocation spell that affects other creatures that you can see, you can choose a number of them equal to 1 + the spell's level. The chosen creatures automatically succeed on their saving throws against the spell, and they take no damage if they would normally take half damage on a successful save.".

'potent cantrip' ?= "Starting at 6th level, your damaging cantrips affect even creatures that avoid the brunt of the effect. When a creature succeeds on a saving throw against your cantrip, the creature takes half the cantrip's damage (if any) but suffers no additional effect from the cantrip.".

'empowered evocation' ?= "Beginning at 10th level, you can add your Intelligence modifier to one damage roll of any wizard evocation spell you cast. ".

overchannel ?= "Starting at 14th level, you can increase the power of your simpler spells. When you cast a wizard spell of 1st through 5th level that deals damage, you can deal maximum damage with that spell.
The first time you do so, you suffer no adverse effect. If you use this feature again before you finish a long rest, you take 2d12 necrotic damage for each level of the spell, immediately after you cast it. Each time you use this feature again before finishing a long rest, the necrotic damage per spell level increases by 1d12. This damage ignores resistance and immunity.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

spellbook@=srd('114').
'ritual casting'(wizard)@=srd('114').
'arcane recovery'(_)@=srd('115').

'evocation savant'@=srd('117').
'sculpt spells'@=srd('117').
'potent cantrip'@=srd('117').
'empowered evocation'@=srd('117').
overchannel@=srd('118').

'divination savant'@=phb('116').
portent@=phb('116').
'expert divination'@=phb('116').
'greater portent'@=phb('117').
