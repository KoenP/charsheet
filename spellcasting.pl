:- multifile
       learned_spell/6.

%! learned_spell(?Origin, ?Ability:atomic, ?Availability, ?Resources:list, ?Ritual:atomic, ?Name:atomic)
%
%  Learned spells are those spells spells that are on your character's
%  spell list, either as spells that are always castable or spells
%  that you need to prepare. Spell learning works differently for
%  different classes. For a druid, for instance, every learnable spell
%  (see learnable_spell/2) is automatically a learned spell. Sorcerers
%  get to pick new spells and replace old spells upon leveling up.
%
%  * Origin is typically a class or a race. The Origin is not
%    registered to the same level of detail as the Source of a trait/2
%    or bonus/2. We typically just note `wizard` or `'high elf'`
%    rather than choice(match_class(wizard:2), spell) or
%    choice(race('high elf'), cantrip) as the Origin.
%    Origin is relevant for:
%    * Characters can learn the same spell multiple times,
%      but not more than once for the same Origin.
%    * If the spell needs to be prepared (see also Availability), then we
%      use the Origin to check which list of prepared spells it is
%      added to when prepared.
%    * Some traits add bonuses to spells based on their Origin, such
%      as the `'empowered evocation'` trait, which increases the damage
%      of wizard evocations.
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
learned_spell(_,_,_,_,_,_) :- false.
meta_todo(learned_spell(Origin, Name), resources_are_not_a_list(Resources)) :-
    learned_spell(Origin, _, _, Resources, _, Name),
    \+ is_list(Resources).
meta_todo(learned_spell(Origin, Name), invalid_field(ritual,Ritual)) :-
    learned_spell(Origin, _, _, _, Ritual, Name),
    \+ member(Ritual, [always, 'when prepared', no, only]).
% TODO: check all the fields

%! learned_spell(?Origin, ?Name)
%
%  Shorthand for learned_spell/6, for when we're only interested in Origin and Name.
learned_spell(Origin, Name) :-
    learned_spell(Origin, _, _, _, _, Name).
    
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
    spell_property(Name, level, SpellLevel),
    between(1, MaxSlotLevel, SpellLevel).

%! class_cantrip(?Class, ?Name)
%  
%  Name is a cantrip on the spell list for Class.
class_cantrip(Class, Name) :-
    spell(Name, Data),
    Data.level = 0,
    member(Class, Data.classes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Interacting with spell data.

%! spell_property(?Name:atomic, ?Prop:atomic, ?Val)
%
%  Extract the field Prop from the properties of the spell Name (see
%  spell/2), and bind the result to Val. This uses the generic spell data.
%  For the spell data after character-specific modifications (like the
%  `'empowered spell'` trait), see learned_spell_property/4.
spell_property(Name, Prop, Val) :-
    spell(Name, Data),
    Val = Data.Prop.

%! learned_spell_property(?Origin:atomic, ?Name:atomic, ?Prop:atomic, ?Val)
%
%  Like spell_property/3, but taking into account any modifications to
%  the spell data made for your character (for example the damage
%  boost from `'empowered evocation'`.)
learned_spell_property(_, _, _, _) :- throw('TODO: implement this').

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
