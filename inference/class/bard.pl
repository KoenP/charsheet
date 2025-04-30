class_option(bard).
hd_per_level(bard, 1 d 8).
initial_class_base_hp(bard, 8).
max_hp_per_level(bard, 1 d 8).
caster(bard, full).
spellcasting_ability(bard, cha).
choose_subclass_level(bard:3).
asi_level(bard:L) :-
    default_asi_level(L).
class_saving_throw(bard, dex).
class_saving_throw(bard, cha).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initial class features
traits_from_source(^bard, [weapon(simple),
                           weapon('hand crossbow'),
                           weapon(longsword),
                           weapon(rapier),
                           weapon(shortsword)]).
trait_options_source(^bard, 'musical instrument', wrap(tool),
                     3 unique_from musical_instrument).
trait_options_source(^bard, skill, wrap(skill), 3 unique_from skill).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 1 features
trait_source(bard >: 1, armor(light)).
trait_options_source(bard >: 1, skill, wrap(skill), skill) :-
    \+ (^bard).
trait_options_source(bard >: 1, 'musical instrument', wrap(tool),
                     musical_instrument) :-
    \+ (^bard).
trait_source(bard >: 1, 'bardic inspiration').
res('bardic inspiration', N) :-
    trait('bardic inspiration'),
    ability_mod(cha, N).
restore_res('long rest', 'bardic inspiration', 'full restore').
trait_source(bard >: 1, spellcasting_focus('musical instrument')).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features gained from leveling up.
trait_source(bard >: 2, 'jack of all trades').
trait_source(bard >: 2, song_of_rest(1 d N)) :-
    class_level(bard:L),
    ordered_lookup_largest_leq([2 -> 6, 9 -> 8, 13 -> 10, 17 -> 12], L, N).
trait_options_source(bard >: 2, expertise, deep_wrap(skill(expertise)),
                     2 unique_from proficient_at_skill).

trait_source(bard >: 5, 'font of inspiration').
restore_res('short rest', 'bardic insipiration', 'full restore') :-
    trait('font of inspiration').

trait_source(bard >: 6, countercharm).

trait_options_source(bard >: L, 'magical secrets', wrap(magical_secret),
                     2 unique_from magical_secret_spell) :-
    member(L, [10, 14, 18]).
magical_secret_spell(Spell) :-
    learnable_spell_level(bard, SpellLevel),
    spell_property(Spell, level, Level),
    Level =< SpellLevel.
magical_secret_spell(Cantrip) :-
    spell_property(Cantrip, level, 0).

trait_source(bard >: 20, 'superior inspiration').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Spellcasting.

known_spell(bard, cha, always, [], no, Spell) :-
    class_origin_to_class(Origin, bard),
    choice_member(Origin, cantrip, Spell).
known_spell(bard, cha, always, [slot], Ritual, Spell) :-
    selected_at_class_level(bard:L, spell, Spell),
    bard >: L,
    spell_property(Spell, ritual, Ritual).
known_spell(bard, cha, always, Slot, Ritual, Spell) :-
    trait(magical_secret(Spell)),
    spell_property(Spell, level, Level),
    (Level = 0 -> Slot = [] ; Slot = [slot]),
    spell_property(Spell, ritual, Ritual).

% Learn cantrips.
options_source(bard >: 1, cantrip, 2 unique_from class_cantrip(bard)).
options_source(bard >: L, cantrip, class_cantrip(bard)) :-
    member(L, [4, 10]).

% Learn proper spells.
options_source(bard >: 1, spell, 4 unique_from learnable_proper_spell(bard)).
options_source(bard >: L, spell, learnable_proper_spell(bard)) :-
    member(L, [2,3,4,5,6,7,8,9,11,13,15,17]).

% Replace proper spells.
options_source(bard >: L, replace(spell),
               selected_at_class_level(bard:Prev, spell)) :-
    between(2, 20, L),
    Prev is L-1.
options(bard >: L, replacing(spell, Name), learnable_proper_spell(sorcerer)) :-
    choice_member(bard >: L, replace(spell), Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUBCLASSES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% College of Lore
% ---------------
subclass_option(bard, lore).
trait_options_source(bard(lore) >: 3, skill, wrap(skill), 3 unique_from skill).
trait_source(bard(lore) >: 3, 'cutting words').
trait_options_source(bard(lore) >: 6, 'magical secrets', wrap(magical_secret),
                     2 unique_from magical_secret_spell).
trait_source(bard(lore) >: 14, 'peerless skill').

% College of Valor
% ----------------
% TODO untested
subclass_option(bard, valor).
traits_from_source(bard(valor) >: 3, [armor(medium),
                                      armor(shield),
                                      weapon(martial)]).
trait_source(bard(valor) >: 3, 'combat inspiration').
multiclass_trait_source(bard(valor) >: 6, extra_attack(1)).
trait_source(bard(valor) >: 14, 'battle magic').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DESCRIPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'bardic inspiration' ?= "You can inspire others through stirring words or music. To do so, you use a bonus action on your turn to choose one creature other than yourself within 60 feet of you who can hear you. That creature gains one Bardic Inspiration die, a d6.

Once within the next 10 minutes, the creature can roll the die and add the number rolled to one ability check, attack roll, or saving throw it makes. The creature can wait until after it rolls the d20 before deciding to use the Bardic Inspiration die, but must decide before the GM says whether the roll succeeds or fails. Once the Bardic Inspiration die is rolled, it is lost. A creature can have only one Bardic Inspiration die at a time.

You can use this feature a number of times equal to your Charisma modifier (a minimum of once). You regain any expended uses when you finish a long rest.

Your Bardic Inspiration die changes when you reach certain levels in this class. The die becomes a d8 at 5th level, a d10 at 10th level, and a d12 at 15th level.".

'jack of all trades' ?= "Starting at 2nd level, you can add half your proficiency bonus, rounded down, to any ability check you make that doesn’t already include your proficiency bonus.".

song_of_rest(_) ?= "Beginning at 2nd level, you can use soothing music or oration to help revitalize your wounded allies during a short rest. If you or any friendly creatures who can hear your performance regain hit points at the end of the short rest by spending one or more Hit Dice, each of those creatures regains an extra 1d6 hit points.

The extra hit points increase when you reach certain levels in this class: to 1d8 at 9th level, to 1d10 at 13th level, and to 1d12 at 17th level.".

'font of inspiration' ?= "Beginning when you reach 5th level, you regain all of your expended uses of Bardic Inspiration when you finish a short or long rest.".

'countercharm' ?= "At 6th level, you gain the ability to use musical notes or words of power to disrupt mind-influencing effects. As an action, you can start a performance that lasts until the end of your next turn. During that time, you and any friendly creatures within 30 feet of you have advantage on saving throws against being frightened or charmed. A creature must be able to hear you to gain this benefit. The performance ends early if you are incapacitated or silenced or if you voluntarily end it (no action required).".

'superior inspiration' ?= "At 20th level, when you roll initiative and have no uses of Bardic Inspiration left, you regain one use.".

'cutting words' ?= "Also at 3rd level, you learn how to use your wit to distract, confuse, and otherwise sap the confidence and competence of others. When a creature that you can see within 60 feet of you makes an attack roll, an ability check, or a damage roll, you can use your reaction to expend one of your uses of Bardic Inspiration, rolling a Bardic Inspiration die and subtracting the number rolled from the creature’s roll. You can choose to use this feature after the creature makes its roll, but before the DM determines whether the attack roll or ability check succeeds or fails, or before the creature deals its damage. The creature is immune if it can’t hear you or if it’s immune to being charmed.".

'peerless skill' ?= "Starting at 14th level, when you make an ability check, you can expend one use of Bardic Inspiration. Roll a Bardic Inspiration die and add the number rolled to your ability check. You can choose to do so after you roll the die for the ability check, but before the DM tells you whether you succeed or fail.".

'combat inspiration' ?= "Also at 3rd level, you learn to inspire others in battle. A creature that has a Bardic Inspiration die from you can roll that die and add the number rolled to a weapon damage roll it just made. Alternatively, when an attack roll is made against the creature, it can use its reaction to roll the Bardic Inspiration die and add the number rolled to its AC against that attack, after seeing the roll but before knowing whether it hits or misses.".

'battle magic' ?= "At 14th level, you have mastered the art of weaving spellcasting and weapon use into a single harmonious act. When you use your action to cast a bard spell, you can make one weapon attack as a bonus action.".

'bardic inspiration'@=srd('53-54').
'jack of all trades'@=srd('54').
song_of_rest(1 d _)@=srd('54').
'font of inspiration'@=srd('54').
countercharm@=srd('54').
'superior inspiration'@=srd('54').
'peerless skill'@=srd('55').
