:- multifile
       known_spell/6,
       spell_property/3,
       extend_class_spell_list/2.


%! known_spell(?Origin, ?Ability:atomic, ?Availability, ?Resources:list, ?Ritual:atomic, ?Name:atomic)
%
%  Known spells are those spells spells that are on your character's
%  spell list, either as spells that are always castable or spells
%  that you need to prepare. Spell learning works differently for
%  different classes. For a druid, for instance, every learnable spell
%  (see learnable_spell/2) is automatically a known spell. Sorcerers
%  get to pick new spells and replace old spells upon leveling up.
%
%  * Origin is typically a class or a race. The Origin is not
%    registered to the same level of detail as the Source of a trait/2
%    or bonus/2. We typically just note `wizard` or `'high elf'`
%    rather than choice(match_class(wizard:2), spell) or
%    choice(race('high elf'), cantrip) as the Origin.
%    Some classes can learn the same spell multiple times through
%    different means. For example, warlocks can learn 'hold monster'
%    as a regular spell, or a modified version through the eldritch
%    invocation 'chains of carceri'. In the first case, the origin is
%    simply `warlock`; in the second, it's
%    `warlock:eldritch_invocation('chains of carceri')`.
%    Origin is relevant for:
%    * Characters can learn the same spell multiple times,
%      but not more than once for the same Origin.
%    * If the spell needs to be prepared (see also Availability), then we
%      use the Origin to check which list of prepared spells it is
%      added to when prepared.
%    * Some traits add bonuses to spells based on their Origin, such
%      as the `'empowered evocation'` trait, which increases the damage
%      of wizard evocations. In this case, both the literal Origin
%      `wizard`, as well as any Origins of the form `wizard:_` count
%      as "wizard spells" .
%  * Ability is the ability used for casting the spell. For many
%    spells this is irrelevant, but it's usually tied to the
%    Origin. Because I'm not 100% sure it's _always_ tied to the
%    origin, I decided to register it as a separate argument here.
%  * Availability indicates whether the spell is always prepared
%    (`always`) or has to be explicitly prepared (`prepare`). If it
%    has to be prepared, we look at the Origin to check which list of
%    prepared spells this spell belongs to.
%  * Resources indicates which resources the spell consumes when
%    cast. Resources is always a list. Some spells don't consume any
%    resource; in those cases the Resources list is empty. The most
%    common case is spells that consume a normal spell slot; they are
%    marked with the singleton list `[slot]`. Warlock pact magic slots
%    are separate from generic spell slots, and are marked with
%    `['pact slot']`. Some spells may have additional constraints. For
%    example, the eldritch invocation `'bewitching whispers'` lets the
%    character cast the spell `compulsion` using a warlock spell slot,
%    but not more than once per long rest. In that case,
%    `Resources = ['pact slot', per_rest(long, 1)]`. TODO: I'm not
%    sure yet what the best way is to automate this bit.
%  * Ritual indicates whether the spell can be cast as a ritual
%    (`always`, `'when prepared'` or `no`), or in some cases, _only_ as
%    a ritual (`only`).
%  * Name is the name of the spell. I've put this last because I
%    anticipate it will be useful often to partially apply all the
%    other arguments.
known_spell(_,_,_,_,_,_) :- false.
meta_todo(known_spell(Origin, Name), resources_are_not_a_list(Resources)) :-
    known_spell(Origin, _, _, Resources, _, Name),
    \+ is_list(Resources).
meta_todo(known_spell(Origin, Name), invalid_field(ritual,Ritual)) :-
    known_spell(Origin, _, _, _, Ritual, Name),
    \+ member(Ritual, [always, 'when prepared', no, only]).
% TODO: check all the fields

%! known_spell_origin_class(?Origin, ?Class:atomic)
%
%  True iff Origin (as in a known_spell/6 Origin) refers to the class
%  Class.
known_spell_origin_class(Class, Class) :-
    class_option(Class).
known_spell_origin_class(Class:_, Class) :-
    class_option(Class).

%! known_spell(?Origin, ?Name)
%
%  Shorthand for known_spell/6, for when we're only interested in Origin and Name.
known_spell(Origin, Name) :-
    known_spell(Origin, _, _, _, _, Name).

%! known_spell_data(?Origin:atomic, ?Name:atomic, ?Data)
%
%  Retrieves the Data associated with the spell Name, but after
%  applying `modify_spell` bonuses with matching Origin.
known_spell_data(Origin, Name, Data) :-
    known_spell(Origin, Name),
    spell_data(Name, GenericData),
    findall(Mod,
            (bonus(modify_spell(Origin, Name, Mod));
             known_spell_mod(Origin, Name, Mod)),
            Mods),
    sequence(Mods, GenericData, Data).

%! known_spell_property(?Origin:atomic, ?Name:atomic, ?Field:atomic, ?Val)
%
%  Looks up the known_spell_data/3 associated with a given spell
%  Origin and Name, and extracts the given Field from that data to
%  yield Val. If the Field does not exist, this predicate silently fails.
known_spell_property(Origin, Name, Field, Val) :-
    known_spell_data(Origin, Name, Data),
    Val = Data.get(Field).

%! known_spell_property_or_error(?Origin:atomic, ?Name:atomic, ?Field:atomic, ?Val)
%  
%  Like known_spell_property/4, but if the Field does not exist in the
%  spell data, throw an error instead of silently failing.
known_spell_property_or_error(Origin, Name, Field, Val) :-
    known_spell_data(Origin, Name, Data),
    Val = Data.(Field).

%! spell_attack_modifier(?Class:atomic, ?Mod:int)
%
%  The PC's spell attack modifier for the given Class.
spell_attack_modifier(Class, AttackMod) :-
    spellcasting_ability(Class, Abi),
    ability_mod(Abi, AbiMod),
    proficiency_bonus(ProfBon),
    AttackMod is ProfBon + AbiMod.

spell_save_dc(Class, DC) :-
    spellcasting_ability(Class, Abi),
    ability_mod(Abi, AbiMod),
    proficiency_bonus(ProfBon),
    DC is 8 + ProfBon + AbiMod.

%! known_spell_to_hit(?Origin, ?Name:atomic, ?ToHit:int)
%
%  The to hit value of the given known spell, if it is relevant
%  (predicate will fail for a spell that does not make any attack
%  rolls; it will succeed once if a spell makes at least one attack
%  roll).
known_spell_to_hit(Origin, Name, ToHit) :-
    known_spell_property(Origin, Name, effects, Effects),
    contains_attack_roll(Effects, _),
    known_spell_origin_class(Origin, Class),
    spell_attack_modifier(Class, ToHit).

%! known_spell_saving_throw(?Origin, ?Name:atomic, ?DC:int, ?Abi:atomic)
%
%  Associates a known spell with the DC and the Ability of one of its
%  saving throws. The predicate may be true more than once for a given
%  Origin and Name, if the spell has more than one saving throw.
known_spell_saving_throw(Origin, Name, DC, Abi) :-
    known_spell_property(Origin, Name, effects, Effects),
    contains_saving_throw(Effects, saving_throw(Abi):_),
    known_spell_origin_class(Origin, Class),
    spell_save_dc(Class, DC).
%contains_saving_throw(Effects, saving_throw(Abi):Effect)
    
%! learnable_proper_spell(?Class, ?Name)
%
%  A proper spell is a spell that is not a cantrip.
%  A proper spell is learnable for a given Class if the spell is not of a
%  higher level than the highest level spell slots you have in that
%  Class, using the single class spell slot formula.
%  We use a separate but analogous calculation for warlocks using pact
%  magic slots.
learnable_proper_spell(Class, Name) :-
    class(Class),
    Class \= warlock,
    findall(L, spell_slots_single_class(L, Class, _), SlotLevels), % TODO this is inefficient
    max_member(MaxSlotLevel, SlotLevels),
    spell_data(Name, SpellData),
    (member(Class, SpellData.classes); extend_class_spell_list(Class, Name)),
    between(1, MaxSlotLevel, SpellData.level).
learnable_proper_spell(warlock, Name) :-
    pact_magic_slot_level(SlotLevel),
    spell_data(Name, Data),
    (member(warlock, Data.classes); extend_class_spell_list(warlock, Name)),
    between(1, SlotLevel, Data.level).

%! cantrip(?Name)
cantrip(Name) :- spell_property(Name, level, 0).

%! class_cantrip(?Class, ?Name)
%  
%  Name is a cantrip on the spell list for Class.
class_cantrip(Class, Name) :-
    spell_data(Name, Data),
    Data.level = 0,
    member(Class, Data.classes).

%! spell_origin(?Origin)
%
%  Origin is the origin of at least one known_spell/6.
spell_origin(Origin) :-
    findall(O, known_spell(O,_), Origins),
    list_to_set(Origins, OriginsSet),
    member(Origin, OriginsSet).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Interacting with spell data.

%! known_spell_property(?Origin:atomic, ?Name:atomic, ?Prop:atomic, ?Val)
%
%  Like spell_property/3, but taking into account any modifications to
%  the spell data made for your character (for example the damage
%  boost from `'empowered evocation'`.)
%known_spell_property(Origin, Name, Prop, Val) :-
%    known_spell(Origin, Name),
%    spell_property(Name, Prop, GenericVal),
%    findall(Mod, bonus(modify_spell_property(Origin,Name,Prop,Mod)), Mods),
%    sequence(Mods, GenericVal, Val).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Spell slots.

%! spell_slots(?SpellLevel:int, ?NSlots:int)
%
%  The number of slots (NSlots) for a given SpellLevel (between 1 and 9).
spell_slots(SpellLevel, NSlots) :-
    % Multiclass formula. Only triggered if we have more than one caster class.
    caster,
    findall(Lvl, class_spell_slot_level(_,Lvl), VirtualLevelTerms),
    VirtualLevelTerms = [_,_|_],
    sumlist(VirtualLevelTerms, VirtualLevel),
    spell_slot_level_to_slots(SpellLevel, VirtualLevel, NSlots).
spell_slots(SpellLevel, NSlots) :-
    % Single class formula.
    % If we have more than one class but only one is a caster, we still use
    % the single class formula.
    caster,
    findall(Lvl, class_spell_slot_level(_,Lvl), [_]),
    spell_slots_single_class(SpellLevel, _, NSlots).

%! spell_slots_single_class(?SpellLevel:int, ?Class, ?NSlots:int)
%
%  Spell slot formula for characters that have only a single
%  spellcasting class. Never true for `NSlots = 0`.
spell_slots_single_class(SpellLevel, Class, NSlots) :-
    class_level(Class:Level),
    caster(Class, Factor),
    single_class_virtual_full_caster_level(Level, Factor, VirtualLevel),
    spell_slot_level_to_slots(SpellLevel, VirtualLevel, NSlots),
    NSlots > 0.
single_class_virtual_full_caster_level(ClassLevel, full, ClassLevel).
single_class_virtual_full_caster_level(1, 1/2, 0).
single_class_virtual_full_caster_level(ClassLevel, 1/2, VirtualLevel) :-
    between(2, 9, ClassLevel),
    VirtualLevel is ceil(ClassLevel / 2).

%! spell_slot_level_to_slots(?SpellLevel:int, ?VirtualFullCasterLevel:int, ?NSlots:int)
%
%  Calculcates the number of slots for a given spell level and your
%  "virtual full caster" level, that is, the equivalent number of full
%  caster levels you have for the purpose of calculating spell slots.
%  For instance, if you are a level 4 ranger, you count as a level 2
%  virtual full caster. If you are a level 4 wizard, you count as a
%  level 4 virtual full caster (since you are a full caster). If you
%  are a level 4 wizard and a level 2 ranger, you count as a level 5
%  virtual full caster.
spell_slot_level_to_slots(SpellLevel, VirtualFullCasterLevel, NSlots) :-
    full_caster_spell_slot_table(SpellLevel, Gains),
    findall(X, (member(X,Gains), X=<VirtualFullCasterLevel), Xs),
    length(Xs, NSlots),
    NSlots > 0.

%! class_spell_slot_level(Class, SpellSlotLevel)
%
%  Calculcate a class' contribution to the multiclass spell slot level.
class_spell_slot_level(Class, SpellSlotLevel) :-
    class_level(Class:Level),
    caster(Class, Factor),
    Factor \= 0,
    spell_slot_factor(Factor, Level, SpellSlotLevel).
spell_slot_factor(full, Level, Level).
spell_slot_factor(1 / N, Level, SpellSlotLevel) :-
    SpellSlotLevel is floor(Level / N).

%! full_caster_spell_slot_table(?SlotLevel, ?ClassLevels)
%
%  Look up, for each SlotLevel, at which ClassLevels (of all "full
%  caster" classes, like wizard, sorcerer, druid, ...) you gain slots
%  of that level. Gaining two slots on a given class level is
%  indicated by that class level occurring twice in the list of
%  ClassLevels.
full_caster_spell_slot_table(1, [1,1,2,3]). % gain two spell slots on level 1, 1 on level 2, 1 on level 3
full_caster_spell_slot_table(2, [3,3,4]).
full_caster_spell_slot_table(3, [5,5,6]).
full_caster_spell_slot_table(4, [7,8,9]).
full_caster_spell_slot_table(5, [9,10,17]).
full_caster_spell_slot_table(6, [11,19]).
full_caster_spell_slot_table(7, [13,20]).
full_caster_spell_slot_table(8, [15]).
full_caster_spell_slot_table(9, [17]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Preparing spells.

%! default_max_prepared_spells(?Class:atomic, -N:integer)
%
%  The default formula for determining how many spells a full caster
%  can prepare for the given class.
default_max_prepared_spells(Class, N) :-
    spellcasting_ability(Class, Ability),
    ability_mod(Ability, Mod),
    class_level(Class:Level),
    N is Level + Mod.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper predicates for modifying spell data.

%! 
known_spell_mod(Origin, Name, Mod) :-
    known_spell_effect(Origin, Name, Effect),
    Mod = modify_spell_field(effects, append_to_list(Effect)).

modify_spell_field(Field, UpdateField, Old, New) :-
    OldField = Old.get(Field),
    call(UpdateField, OldField, NewField),
    New = Old.put(Field, NewField).

increase_all_spell_damage_rolls(Bonus, Old, New) :-
    get_or_default(Old, effects, [], OldEffects),
    map_matching_subterms({Bonus}/[damage(El,OldRoll),damage(El,NewRoll)]
                            >> simplify_dice_sum(OldRoll+Bonus, NewRoll),
                          OldEffects,
                          NewEffects),
    New = Old.put(effects, NewEffects).

%spell_damage_bonus(Bonus, Old, New) :-
%    get_or_default(Old, effects, [], OldEffects),
%    select_subterm(damage(Element,Dice), OldEffects,
%                   damage(Element,NewDice), NewEffects),
%    simplify_dice_sum(Dice+Bonus, NewDice),
%    New = Old.put(effects, NewEffects).
%spell_damage_bonus(Bonus, Old, New) :-
%    get_or_default(Old, effects, [], OldEffects),
%    contains_multiple_damage_rolls(OldEffects),
%    atomics_to_string(["add +", Bonus, " to one damage roll"], NewEffect),
%    append(OldEffects, [NewEffect], NewEffects),
%    New = Old.put(effects, NewEffects).
%spell_damage_bonus(_,_,_) :- writeln('wat').

contains_multiple_damage_rolls(Effects) :-
    findall(A-B, subterm_member(damage(A,B),Effects), [_,_|_]).
contains_multiple_damage_rolls(Effects) :-
    member(N*SubEffects, Effects),
    N > 1,
    subterm_member(damage(_,_), SubEffects).

add_damage(Bonus, damage(Element,Roll1), damage(Element,Roll2)) :-
    simplify_dice_sum(Roll1+Bonus, Roll2).

const(X,_,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- multifile
       drop_all_components_source/3,
       delete_component_source/4.

%! drop_all_components_source(?BonusOrigin, ?SpellOrigin, ?Spell)
%
%  True iff your character has some feature (BonusOrigin) that causes
%  Spell (for SpellOrigin) to not require components.
drop_all_components_source(_,_,_) :- false.
bonus_source(BonusOrigin, modify_spell(SpellOrigin, Spell, Goal)) :-
    drop_all_components_source(BonusOrigin, SpellOrigin, Spell),
    Goal = modify_spell_field(components, const([])).

%! delete_component_source(?BonusOrigin, ?SpellOrigin, ?Spell, ?Component)
%
%  True iff your character has some feature (BonusOrigin) that causes
%  Spell (for SpellOrigin) to not require Component as a component.
delete_component_source(_,_,_,_) :- false.
bonus_source(BonusOrigin, modify_spell(SpellOrigin, Spell, Goal)) :-
    delete_component_source(BonusOrigin, SpellOrigin, Spell, Component),
    Goal = modify_spell_field(components,
                              {Component}/[Cs1,Cs2]
                               >> delete(Cs1, Component, Cs2)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cantrip damage scaling.

% TODO: werkt niet: findall(Mod, bonus(modify_spell_property(_,'fire bolt',damage,Mod)), Mods), sequence(Mods, fire(1 d 10), X).

cantrip_scale(Scale) :-
    level(Level),
    ( Level < 5  -> !, Scale = 1
    ; Level < 11 -> !, Scale = 2
    ; Level < 17 -> !, Scale = 3
    ; Scale = 4
    ).

incr(N,M) :- M is N+1.