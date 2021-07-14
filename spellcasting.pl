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
       spell_known/5.

% The table that stores all "static" information about spells (ie
% mostly independent of your character, although some things like
% cantrip scaling with your level are taken into account here already).
:- table spell/3.
spell(Name, Field, Value) :-
    spell(Name, Properties),
    Value = Properties.get(Field).

% Calculate the number of available spell slots.
full_caster_spell_slot_table(1, [1,1,2,3]).
full_caster_spell_slot_table(2, [3,3,4]).
full_caster_spell_slot_table(3, [5,5,6]).
full_caster_spell_slot_table(4, [7,8,9]).
full_caster_spell_slot_table(5, [7,8,9]).
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
    \+ multiclass,
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
attack(Spell, Range, ToHit, Damage, [Note]) :-
    spell_known(Spell, Origin, Ability, always_available, at_will),
    spell(Spell, range, Range),
    spell_to_hit(Spell, Origin, ToHit),
    spell_known_damage(Spell, Origin, Ability, 0, Damage),
    atom_concat(Origin, ' cantrip', Note).
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

spell_dc(Spell, Source, DC) :-
    proficiency_bonus(ProfBon),
    spell_has_dc(Spell),
    spell_known(Spell, Source, Ability, _, _),
    ability_mod(Ability, Mod),
    DC is 8 + ProfBon + Mod.

% The notion of "learnable spells" is not meaningful for each class.
% For e.g. wizards and sorcerers, it's the list of spells they can
% learn new spells from. For e.g. druids and clerics this concept
% completely coincides with their known spells. Still, the list of
% known spells for these classes is derived from the defintion of
% learnable spells, as written here.
% TODO recalculate
spell_learnable(Class, SpellName) :-
    class(Class),
    spell_class(SpellName, Class),
    spell(SpellName, level, 0).
%spell_learnable(Class, SpellName) :-
%    class(Class),
%    spell(SpellName, level, SpellLevel),
%    spell_slots(Class, spell_level(SpellLevel), N),
%    N > 0.

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

% The damage done by a spell you know doesn't have to be the same
% as the amount listed in the spell description; you might have other
% factors influencing it. These factors are taken into account in
% spell_known_damage/3.
spell_known_damage(Spell, Origin, Ability, Upcast, Rolls) :-
    spell_known(Spell, Origin, Ability, _, _),
    in_upcast_range(Spell, Upcast), % ground Upcast
    spell_damage_rolls(Spell, Upcast, Rolls).

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
custom_display_rule(when_spell_active(S), Str) :-
    atomics_to_string(['when ', S, ' is active'], Str).
