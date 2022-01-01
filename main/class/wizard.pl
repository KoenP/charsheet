class_option(wizard).
hd_per_level(wizard, 1 d 6).
initial_class_base_hp(wizard, 6).
max_hp_per_level(wizard, 1 d 6).
caster(wizard, full).
choose_subclass_level(wizard:2).
asi_level(wizard:L) :-
    default_asi_level(L).
class_saving_throw(wizard, int).
class_saving_throw(wizard, wis).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initial class features.
traits_from_source(initial_class(wizard),
                   [weapon(dagger), weapon(dart), weapon(sling),
                    weapon(quarterstaff), weapon('light crossbow')]).

trait_options_source(initial_class(wizard), skill, wrap(skill),
                     2 unique_from from_list([arcana,
                                              history,
                                              insight,
                                              investigation,
                                              medicine,
                                              religion])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
trait_source(class(wizard), spellbook).
trait_source(class(wizard), spellcasting_focus(arcane)).
trait_source(class(wizard), 'ritual casting').
trait_source(class(wizard), arcane_recovery(N)) :-
    class_level(wizard:L),
    N is ceil(L/2).
on_rest(long, 'arcane recovery', full) :-
    trait('arcane recovery').

spell_mastery_candidate(Spell) :-
    known_spell(wizard, Spell),
    spell_property(Spell, level, L),
    member(L, [1,2]).
trait_options_source(class(wizard:18), 'spell mastery', wrap(spell_mastery),
                     spell_mastery_candidate).

signature_spell_candidate(Spell) :-
    known_spell(wizard, Spell),
    spell_property(Spell, level, 3).
trait_options_source(class(wizard:20), 'signature spell', wrap(signature_spell),
                     2 from signature_spell_candidate).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Learning wizard spells
known_spell(wizard, int, always, [], no, Name) :-
    class_origin_to_class(Origin, wizard),
    choice_member(Origin, cantrip, Name).
known_spell(wizard, int, 'when prepared', [slot], Ritual, Name) :-
    class_origin_to_class(Origin, wizard),
    choice_member(Origin, 'free spell', Name),
    spell_property(Name, ritual, Ritual).
known_spell(wizard, int, 'when prepared', [slot], Ritual, Name) :-
    class_origin_to_class(Origin, wizard),
    choice_member(Origin, spell, Name),
    spell_property(Name, ritual, Ritual).

% Learn cantrips.
options_source(class(wizard), cantrip, 3 unique_from class_cantrip(wizard)).
options_source(match_class(wizard:L), cantrip, class_cantrip(wizard)) :-
    L=4 ; L=10.

% Learn proper spells by leveling.
options_source(class(wizard), 'free spell',
               6 unique_from learnable_proper_spell(wizard)).
options_source(match_class(wizard:L), 'free spell',
               2 unique_from learnable_proper_spell(wizard)) :-
    between(2, 20, L).

problem(selected_same_wizard_spell_more_than_once(Spell)) :-
    member(Id1, [spell, 'free spell']),
    choice_member(Origin1, Id1, Spell),
    member(Id2, [spell, 'free spell']),
    choice_member(Origin2, Id2, Spell),
    Origin1 \= Origin2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Arcane tradition: school of evocation
subclass_option(wizard, evocation).

trait_source(match_class(wizard(evocation):2), 'evocation savant').
trait_source(match_class(wizard(evocation):2), 'sculpt spells').
trait_source(match_class(wizard(evocation):6), 'potent cantrip'). % TODO
trait_source(match_class(wizard(evocation):6), overchannel).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'arcane recovery' ?= "You have learned to regain some of your magical energy by studying your spellbook. Once per day when you finish a short rest, you can choose expended spell slots to recover. The spell slots can have a combined level that is equal to or less than half your wizard level (rounded up), and none of the slots can be 6th level or higher.
For example, if you're a 4th-level wizard, you can recover up to two levels worth of spell slots. You can recover either a 2nd-level spell slot or two 1st-level spell slots.".

spellbook ?= "The spells that you add to your spellbook as you gain levels reflect the arcane research you conduct on your own, as well as intellectual breakthroughs you have had about the nature of the multiverse. You might find other spells during your adventures. You could discover a spell recorded on a scroll in an evil wizard's chest, for example, or in a dusty tome in an ancient library.
Copying a Spell into the Book. When you find a wizard spell of 1st level or higher, you can add it to your spellbook if it is of a spell level you can prepare and if you can spare the time to decipher and copy it. Copying that spell into your spellbook involves reproducing the basic form of the spell, then deciphering the unique system of notation used by the wizard who wrote it. You must practice the spell until you understand the sounds or gestures required, then transcribe it into your spellbook using your own notation.
For each level of the spell, the process takes 2 hours and costs 50 gp. The cost represents material components you expend as you experiment with the spell to master it, as well as the fine inks you need to record it. Once you have spent this time and money, you can prepare the spell just like your other spells.
Replacing the Book. You can copy a spell from your own spellbook into another book - for example, if you want to make a backup copy of your spellbook. This is just like copying a new spell into your spellbook, but faster and easier, since you understand your own notation and already know how to cast the spell. You need spend only 1 hour and 10 gp for each level of the copied spell.
If you lose your spellbook, you can use the same procedure to transcribe the spells that you have prepared into a new spellbook. Filling out the remainder of your spellbook requires you to find new spells to do so, as normal. For this reason, many wizards keep backup spellbooks in a safe place. The Book's Appearance. Your spellbook is a unique compilation of spells, with its own decorative flourishes and margin notes. It might be a plain, functional leather volume that you received as a gift from your master, a finely bound gilt-edged tome you found in an ancient library, or even a loose collection of notes scrounged together after you lost your previous spellbook in a mishap.".

'evocation savant' ?= "Beginning when you select this school at 2nd level, the gold and time you must spend to copy an evocation spell into your spellbook is halved.".

'sculpt spells' ?= "Beginning at 2nd level, you can create pockets of relative safety within the effects of your evocation spells. When you cast an evocation spell that affects other creatures that you can see, you can choose a number of them equal to 1 + the spell's level. The chosen creatures automatically succeed on their saving throws against the spell, and they take no damage if they would normally take half damage on a successful save.".

'potent cantrip' ?= "Starting at 6th level, your damaging cantrips affect even creatures that avoid the brunt of the effect. When a creature succeeds on a saving throw against your cantrip, the creature takes half the cantrip's damage (if any) but suffers no additional effect from the cantrip.".

'empowered evocation' ?= "Beginning at 10th level, you can add your Intelligence modifier to one damage roll of any wizard evocation spell you cast. ".
