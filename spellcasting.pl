:- [spells/spells].
:- [spells/spell_effects].

:- multifile 
       % spell_known(Spell, Origin, Ability, Availability, Resource)
       % documents which spells the character knows, as well as
       % from which origin they know the spell (druid, wizard, high
       % elf, ...), which ability modifier the spell uses,
       % whether it is always prepared (Availability =
       % always_available) or needs to be prepared (Availability =
       % when_prepared), and which resource it uses (at_will for spells,
       % like cantrips, which consume no resources, spell_slot for
       % regular spells which consume spell slots, and other options for
       % more specific cases).
       spell_known/5,

       spell_single_roll_damage_bonus/4.

% The table that stores all "static" information about spells (ie
% mostly independent of your character, although some things like
% cantrip scaling with your level are taken into account here already).
:- table spell/3.
spell(Name, Field, Value) :-
    spell(Name, Properties),
    Value = Properties.get(Field).
spell(Name, class, Class) :-
    spell(Name, classes, Classes),
    member(Class, Classes).

cantrip(Spell) :-
    spell(Spell, level, 0).

% True if PC has non-warlock spell slots.
spellcaster :-
    spell_slots(_, _), !.
on_long_rest('spell slots', restore) :-
    spellcaster.

% Calculate the number of available spell slots.
full_caster_spell_slot_table(1, [1,1,2,3]). % gain two spell slots on level 1, 1 on level 2, 1 on level 3
full_caster_spell_slot_table(2, [3,3,4]).
full_caster_spell_slot_table(3, [5,5,6]).
full_caster_spell_slot_table(4, [7,8,9]).
full_caster_spell_slot_table(5, [9,10,17]).
full_caster_spell_slot_table(6, [11,19]).
full_caster_spell_slot_table(7, [13,20]).
full_caster_spell_slot_table(8, [15]).
full_caster_spell_slot_table(9, [17]).

:- table spell_slots/2.
spell_slots(SpellLevel, Slots) :-
    findall(Lvl, class_spell_slot_level(_,Lvl), Levels),
    Levels = [_,_|_], % this is the multiclassing formula
    sumlist(Levels, SlotLevel),
    spell_slot_level_to_slots(SpellLevel, SlotLevel, Slots).
spell_slots(SpellLevel, Slots) :-
    \+ multiclass, % this is the single class formula
    spell_slots_single_class(SpellLevel, _, Slots).

spell_slots_single_class(SpellLevel, Class, Slots) :-
    class_level(Class:Level),
    caster(Class, Fullness),
    single_class_slot_level(Level, Fullness, SlotLevel),
    spell_slot_level_to_slots(SpellLevel, SlotLevel, Slots).

single_class_slot_level(CharLevel, full, CharLevel).
single_class_slot_level(1, 1 / 2, 0).
single_class_slot_level(CharLevel, 1 / 2, Level) :-
    between(2, 9, CharLevel),
    Level is ceil(CharLevel / 2).

spell_slot_level_to_slots(SpellLevel, SlotLevel, Slots) :-
    full_caster_spell_slot_table(SpellLevel, Gains),
    findall(X, (member(X,Gains), X=<SlotLevel), Xs),
    length(Xs, Slots),
    Slots > 0.

%multiclass_spell_slots(S)

%    gain_spell_slots(Class, SpellLevel, Gains),
%    class_level(Class:ClassLevel),
%    findall(X, (member(X,Gains),X=<ClassLevel), Xs),
%    length(Xs, Slots),
%    Slots > 0.


% The DC of your spells, parameterized on which ability you cast the
% spell with. (So note that `Abil` is _not_ the ability the target
% uses for their ST.)
spell_save_dc(Abil, DC) :-
    trait(spellcasting(Abil)),
    ability_mod(Abil, Mod),
    proficiency_bonus(Bonus),
    DC is 8 + Bonus + Mod.

% Spell attack modifier, again parameterized on the ability you cast
% the spell with.
spell_attack_modifier(Abil, Mod) :-
    trait(spellcasting(Abil)),
    ability_mod(Abil, AbilMod),
    proficiency_bonus(Bonus),
    Mod is Bonus + AbilMod.


% Summarize all our spell slot sources.
%spell_slot_source(Source) :-
%    findall(S, spell_slots(S, _, _), Sources),
%    list_to_set(Sources, SourcesSet),
%    member(Source, SourcesSet).

% The predicate spell_at_will_attack/5 can be asserted for a spell
% (usually a cantrip) that in some way lets the PC attack at will
% (directly, as with fire bolt, or indirectly, as with shillelagh).
% This code will then make sure this shows up on the list of attacks.
attack(Spell, Range, ToHit, Damage, [Note|Notes]) :-
    spell_known(Spell, Origin, Ability, always_available, at_will),
    spell(Spell, range, Range),
    spell_to_hit(Spell, Origin, ToHit),
    spell_known_damage(Spell, Origin, Ability, 0, Damage),
    atom_concat(Origin, ' cantrip', Note),
    findall(Effect, spell_other_effect(Spell,Effect), Notes).
%attack(Spell, Range, DamageType, ToHit, Damage, Notes) :-
%    spell_at_will_attack(Spell, Range, DamageType, DamageDice, Notes),
%    (spell_known(Spell, _, Ability, always_available, at_will)
%    ; Spell = Spell1:_, spell_known(Spell1, _, Ability, always_available, at_will)),
%    ability_mod(Ability, Mod),
%    proficiency_bonus(ProfBon),
%    ToHit is Mod + ProfBon,
%    spell_attack_damage(Mod, DamageDice, Damage).
spell_attack_damage(Mod, Base + mod, BaseDmg + ExtraAndMod) :-
    spell_attack_damage(Mod, Base, Dmg),
    plus_zero(Dmg, BaseDmg + ExtraDmg),
    ExtraAndMod is ExtraDmg + Mod.
spell_attack_damage(Mod, Base + Extra, BaseDmg + ExtraAndMod) :-
    number(Extra),
    spell_attack_damage(Mod, Base, Dmg),
    plus_zero(Dmg, BaseDmg + ExtraDmg),
    ExtraAndMod is ExtraDmg + Extra.
spell_attack_damage(_, X d Y, X d Y).

spell_to_hit(Spell, Source, ToHit) :-
    proficiency_bonus(ProfBon),
    spell_makes_spell_attack(Spell),
    spell_known(Spell, Source, Ability, _, _),
    ability_mod(Ability, Mod),
    ToHit is ProfBon + Mod.

spell_dc(Spell, Source, DCAbility, DC) :-
    proficiency_bonus(ProfBon),
    spell_has_dc(Spell, DCAbility),
    spell_known(Spell, Source, CastingAbility, _, _),
    ability_mod(CastingAbility, Mod),
    DC is 8 + ProfBon + Mod.

% Learnable spells are those spells you can choose from whenever
% you get to select a new spell, such as on levelup with a wizard
% or sorcerer, or when transcribing as a wizard.
% For some classes, like clerics and druids, all learnable class
% spells of level 1 or greater are immediately known.
% Which spells are learnable depends on your spell slots, or
% more precisely: a simulation of single-class spell slots, even when
% you're multiclassing.
list_learnable_proper_spells(Class, Spells) :-
    findall(Spell,
            (spell_learnable(Class,Spell), spell(Spell, level, L), L > 0),
            Spells).

:- table spell_learnable/2.
spell_learnable(Class, SpellName) :-
    class(Class),
    Class \= warlock,
    learnable_spell_level(Class, SpellLevel),
    spell(SpellName, class, Class),
    spell(SpellName, level, SpellLevel).
spell_learnable(warlock, SpellName) :-
    pact_magic_slot_level(SlotLevel),
    spell(SpellName, class, warlock),
    spell(SpellName, level, SpellLevel),
    SpellLevel =< SlotLevel.

learnable_spell_level(Class, 0) :-
    class(Class).
learnable_spell_level(Class, SpellLevel) :-
    spell_slots_single_class(SpellLevel, Class, N),
    N > 0.
    
% Known spells are those spells which can be prepared by the PC.
% Whether a spell is known or not is mostly decided by a PC's class and
% leveling up options (but other factors like race may teach spells too).
% Some examples: for druids, clerics, ... these are all their class
% spells for which they have slots; for wizards, these are the spells
% in their spell book.
% A known spell should be seen as an "instance" of a spell "template".
% Some parts need to be filled in (such as which ability modifier the PC
% uses for spell attack rolls and spell DC), others might be overridden
% by external factors (TODO: elemental affinity, archdruid, evoker...)
spell_known(Spell) :-
    spell_known(Spell, _, _, _, _).
spell_known(Spell, Class, Ability, Availability, Resource) :-
    class_level(Class:Level),
    spell_known_at_class_level(Spell, Class:Level, Ability, Availability, Resource).

% PC knows all cantrips that you explicitly selected.
spell_known_at_class_level(Spell, Class:Level, Ability, always_available, at_will) :-
    class(Class),
    trait_at_class_level(Class:Level, _, learn_spell(Class, Spell)),
    spellcasting_ability(Class, Ability),
    spell(Spell, level, 0).

% PC knows all proper spells that you explicitly selected.
spell_known_at_class_level(Spell, Class:Level, Ability, Availability, Resource) :-
    class(Class),
    trait_at_class_level(Class:Level, _, learn_spell(Class, Spell)),
    spell(Spell, level, SpellLevel),
    SpellLevel > 0,
    spellcasting_ability(Class, Ability),
    spellcasting_availability(Class, Availability),
    (Class =  warlock -> Resource = pact_magic_slot;
     Class \= warlock -> Resource = spell_slot).

% "Novel" spells are those spells which can be learned but are not
% already known.  We make no effort to keep track of which spells are
% learnable per class level, if the user builds their character
% level-by-level and checks for errors every time, all errors will be
% detected.
spell_novel_at_class_level(Class:1, Spell) :-
    !,
    spell_learnable(Class, Spell).
spell_novel_at_class_level(Class:Level, Spell) :-
    between(2, 20, Level), % ground Level
    spell_learnable(Class, Spell),
    PrevLevel is Level-1,
    \+ spell_known_at_class_level(Spell, Class:PrevLevel, _, _, _).

%spell_known(Spell, OriginClass, Ability, PrepStatus, Resource) :-
%    trait_from_class_level(OriginClass:Level, _, learn_spell(OriginClass, Spell)),
%    learn_spell_at_class_level(OriginClass:Level, ),

% The damage done by a spell you know doesn't have to be the same
% as the amount listed in the spell description; you might have other
% factors influencing it. These factors are taken into account in
% spell_known_damage/3.
% Elemental affinity states that the player can add CHA mod to one
% damage roll (it seems you can choose which one) of a spell that
% deals damage of the type associated with your draconic ancestry.
% That leaves a point of confusion. For example, say your element is
% cold, and you cast ice knife. Ice knife deals both cold and piercing
% damage. Strictly speaking, the way elemental affinity is worded,
% you can choose to add the damage to either the piercing damage roll
% or the cold damage roll. But I think many players find this
% counterintuitive and will rule that it is intended to apply to the
% cold damage roll only.
% So I'll err on the side of caution here: the automation adds the bonus
% directly to the damage roll if the spell lists only one damage roll.
% If an eligible spell has more than one damage roll, we just add a
% note in the spell table to add the damage to a roll of choice.
% TODO: how to deal with multiturn damage (spells that work like
% moonbeam); probably we should also add a note, but then the damage
% should be somehow 'marked' as multi-turn.
spell_known_damage(Spell, Origin, Ability, Upcast, FinalRolls) :-
    spell_known(Spell, Origin, Ability, _, _),
    in_upcast_range(Spell, Upcast), % ground Upcast
    spell_damage_rolls(Spell, Upcast, Rolls),
    findall(Bonus, spell_single_roll_damage_bonus(Spell,Upcast,Origin,Bonus), Bonuses),
    sumlist(Bonuses, TotalBonus),
    add_single_roll_damage_bonus(TotalBonus, Rolls, FinalRolls).
add_single_roll_damage_bonus(Bonus, [Roll], [NewRoll]) :-
    Roll =.. [Element, Damage],
    normalize_dice_formula(Damage, Dice + Constant),
    NewConstant is Constant + Bonus,
    normalize_dice_formula(NewDamage, Dice + NewConstant),
    NewRoll =.. [Element, NewDamage],
    !.
add_single_roll_damage_bonus(_, Rolls, Rolls).
spell_known_effect(Spell, Origin, Damage) :-
    spell_known_damage(Spell, Origin, _, 0, Damage).
spell_known_effect(Spell, _, Effect) :-
    spell_other_effect(Spell, Effect).
spell_known_effect(Spell, Origin, Effect) :-
    spell_damage_rolls(Spell, 0, [_,_|_]),
    findall(Bonus, spell_single_roll_damage_bonus(Spell,0,Origin,Bonus), Bonuses),
    sumlist(Bonuses, TotalBonus),
    TotalBonus \= 0,
    atomic_list_concat(['+', TotalBonus, ' to one damage roll'], Effect).

% Novel spells are spells that are both learnable and not already known.
%novel_spell(Class:Level, Spell) :-
%    spell_learnable(Class, Spell),
    
    

% Cantrips tend to get stronger at character levels 5, 11, and 17 (the
% rules don't require these levels but this configuration is common
% enough to warrant being supported explicitly).
% The cantrip_scaling/1 predicate calculates a number ranging from 1 to 4,
% 1 for characters < lvl5, 2 for characters from lvl5 to lvl10, ...
cantrip_scale(Scale) :-
    level(Level),
    ( Level < 5  -> !, Scale = 1
    ; Level < 11 -> !, Scale = 2
    ; Level < 17 -> !, Scale = 3
    ; Scale = 4
    ).

% Some custom display rules.
custom_format(when_spell_active(S)) --> ["when "], format_term(S), [" is active"].
