% main.pl
:- multifile
       (?=)/2,
       on_rest/3,
       todo/1,
       meta_todo/2,
       problem/1,
       resource/2,
       content_source/2.

:- op(600, xfx, upto).
:- op(500, xfy, or).
:- op(400, xfy, and).
:- op(700, xfx, else).
:- op(650, xfx, from).
:- op(650, xfx, from).
:- op(650, xfx, unique_from).
:- op(650, xfx, at).
:- op(1000, xfx, ?=).
:- op(1000, xfx, ?=).
:- op(650, xfx, ft).
:- op(700, xfx, by).
:- op(100, xf, pct).
:- op(1000, xf, qq).
:- op(500, xfx, >:).
:- op(500, fx, ^).

:- multifile suppress_unarmored_ac_formula/0.

% class.pl
:- multifile
       class_option/1,
       choose_subclass_level/1,
       subclass_option/2,
       hd_per_level/2,
       initial_class_base_hp/2,
       max_hp_per_level/2,
       class_saving_throw/2,
       caster/2,
       spellcasting_ability/2,
       asi_level/1,
       max_prepared_spells/2,
       class_skill_list/2.

:- multifile
       required_predicate_for_each_class/1.

% replace.pl
:- multifile
       replaceable_class_options/3,
       replace_at_class_level/4,
       replaceable_character_options/4,
       replace_at_character_level/5.

% feat.pl
:- multifile
       feat_option/1,
       feat_option/2.

% test.pl
:- multifile test_char_level/4.
:- dynamic most_recent_test_character/1.

% class/cleric.pl
:- multifile cleric_domain_spell/2.

% class/warlock.pl
:- multifile eldritch_invocation_option/1.

% class/fighter.pl
:- multifile fighting_style/1.

% options.pl
:- multifile
       options/3,
       options_source/3,
       choice/3,
       choice_creates_category/3,
       ignore/2,
       hide_base_option/3,
       lookup_option_doc/4.
:- dynamic ignore/2.

% format.pl
:- multifile custom_format//1.

% equipment.pl
:- multifile weapon/5.
:- multifile has/1.
:- dynamic has/1.
:- dynamic attuned/1.

% fighting_style.pl
:- multifile fighting_style/1.

% multiclassing.pl
:- multifile multiclass_trait/2, multiclass_trait_source/2.

% character_file.pl
:- multifile
       name/1,
       base_ability/2,
       gain_level/3,
       choice/3.

:- dynamic name/1, base_ability/2, gain_level/3, choice/3.
       
% race.pl
:- multifile
    subrace_option/2,
    racial_speed/2,
    race_shorthand/2,
    race_option/1.

% spell_data.pl
:- dynamic spell_auto_data/2.
:- multifile spell_auto_data/2, extend_spell_data/3.
:- multifile add_spell_effect/2, known_spell_effect/3, suppress_autoderived_spell_effect/1.

% dice.pl
:- op(400, xfx, d).

% bonus.pl
:- multifile
       bonus/2,
       bonus_source/2,
       bonuses_from_source/2,
       bonus_options_source/4.

% trait.pl
:- multifile
       trait/2,
       trait_source/2,
       trait_options/4,
       trait_options_source/4,
       traits_from_source/2,
       choice_member_to_trait/3.

% :- table trait/2 as incremental.

% background.pl
:- multifile background_option/1.

% attacks.pl
:- multifile attack/5, attack_variant/5, add_weapon_note/2, suppress_unarmed/0.

% spellcasting.pl
:- multifile
       known_spell/6,
       spell_property/3,
       extend_class_spell_list/2,
       hide_known_class_spells/3,
       prepare_spell/2,
       spell_origin/1.
:- dynamic prepare_spell/2.

% :- table known_spell/6 as incremental.

%! known_spell(?Origin, ?Ability:atomic, ?Availability, ?Resources, ?Ritual:atomic, ?Name:atomic)
%
%  Known spells are those spells that are on your character's
%  spell list, either as spells that are always castable or spells
%  that you need to prepare. Spell learning works differently for
%  different classes. For a druid, for instance, every learnable spell
%  (see learnable_spell/2) is automatically a known spell. Sorcerers
%  get to pick new spells and replace old spells upon leveling up.
%
%  * Origin is typically a class or a race. The Origin is not
%    registered to the same level of detail as the Source of a trait/2
%    or bonus/2. We typically just note `wizard` or `'high elf'`
%    rather than choice(wizard >: 2, spell) or
%    choice(race('high elf'), cantrip) as the Origin.
%    Some classes can learn the same spell multiple times through
%    different means. For example, warlocks can learn 'hold monster'
%    as a regular spell, or a modified version through the eldritch
%    invocation 'chains of carceri'. In the first case, the origin is
%    simply `warlock`; in the second, it's
%    `warlock(eldritch_invocation('chains of carceri'))`.
%    Origin is relevant for:
%    * Characters can learn the same spell multiple times,
%      but not more than once for the same Origin.
%    * If the spell needs to be prepared (see also Availability), then we
%      use the Origin to check which list of prepared spells it is
%      added to when prepared.
%    * Some traits add bonuses to spells based on their Origin, such
%      as the `'empowered evocation'` trait, which increases the damage
%      of wizard evocations. In this case, both the literal Origin
%      `wizard`, as well as any Origins of the form `wizard(_)` count
%      as "wizard spells" .
%  * Ability is the ability used for casting the spell. For many
%    spells this is irrelevant, but it's usually tied to the
%    Origin. Because I'm not 100% sure it's _always_ tied to the
%    origin, I decided to register it as a separate argument here.
%  * Availability indicates whether the spell is always prepared
%    (`always`) or has to be explicitly prepared (`when prepared`). If it
%    has to be prepared, we look at the Origin to check which list of
%    prepared spells this spell belongs to.
%  * Resources indicates which resources the spell consumes when
%    cast. Resources is either a list, or an `or` functor containing
%    two "sub-resources". Some spells don't consume any
%    resource; in those cases Resources is an empty list. The most
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
%known_spell_origin_class(Class, Class) :-
%    class_option(Class).
%known_spell_origin_class(Class:_, Class) :-
%    class_option(Class).

%! known_spell(?Origin, ?Name)
%
%  Shorthand for known_spell/6, for when we're only interested in Origin and Name.
known_spell(Origin, Name) :-
    known_spell(Origin, _, _, _, _, Name).

%! known_spell(?Name)
%
%  Checks whether Name is a known spell. Only evaluates to true once.
known_spell(Name) :-
    \+ ground(Name),
    findall(N, known_spell(_,N), SpellSet),
    list_to_set(SpellSet, SpellList),
    member(Name, SpellList).
known_spell(Name) :-
    ground(Name),
    known_spell(_, Name),
    !.

%! known_spell_data(?Origin:atomic, ?Name:atomic, ?Data)
%
%  Retrieves the Data associated with the spell Name, but after
%  applying `modify_spell` bonuses with matching Origin.
known_spell_data(Origin, Name, Data) :-
    known_spell(Origin, Name),
    spell_data(Name, GenericData),
    substitute_effect_variables(Origin, GenericData, SubstData),
    findall(Mod,
            (bonus(modify_spell(Origin, Name, Mod));
             known_spell_mod(Origin, Name, Mod)),
            Mods),
    sequence(Mods, SubstData, Data).

substitute_effect_variables(Origin, Data, Data.put(effects, NewEffects)) :-
    map_matching_subterms(match_effect_variable_substitution(Origin),
                          Data.effects,
                          NewEffects).
match_effect_variable_substitution(Origin, Formula + mod, NewFormula) :-
    spellcasting_ability(Origin, Ability),
    ability_mod(Ability, Mod),
    simplify_dice_sum(Formula + Mod, NewFormula).

%! known_spell_dice_formula(?Origin:atomic, ?Spell:atomic, ?Rolls)
%
%  If the given Spell (for the given Origin) has an effect in its list of effects
%  that deals damage or heals, then Rolls is the damage/healing formula.
known_spell_dice_formula(Origin, Spell, Rolls) :-
    known_spell_property(Origin, Spell, effects, Effects),
    (unique_subterm_member(damage(_, Rolls), Effects) ; unique_subterm_member(heal(Rolls), Effects)).

%! known_spell_aoe(?Origin:atomic, ?Spell:atomic, ?Rolls)
%
%  If the given Spell (for the given Origin) has an effect in its list of effects
%  that contains the unary `in` functor, then Aoe is whatever is in that `in(_)` term.
known_spell_aoe(Origin, Spell, Aoe) :-
    known_spell_property(Origin, Spell, effects, Effects),
    unique_subterm_member(in(Aoe), Effects).

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

%! known_spell_prepared(?Origin, ?Spell)
known_spell_prepared(Origin, Spell) :-
    known_spell(Origin, _, always, _, _, Spell).
known_spell_prepared(Origin, Spell) :-
    prepare_spell(Origin, Spell),
    known_spell(Origin, _, 'when prepared', _, _, Spell).

%! known_spell_always_prepared(?Origin, ?Spell)
known_spell_always_prepared(Origin, Spell) :-
    known_spell(Origin, _, always, _, _, Spell).

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
    Origin =.. [BaseOrigin|_],
    spell_attack_modifier(BaseOrigin, ToHit).

%! known_spell_saving_throw(?Origin, ?Name:atomic, ?DC:int, ?Abi:atomic)
%
%  Associates a known spell with the DC and the Ability of one of its
%  saving throws. The predicate may be true more than once for a given
%  Origin and Name, if the spell has more than one saving throw.
known_spell_saving_throw(Origin, Name, DC, Abi) :-
    known_spell_property(Origin, Name, effects, Effects),
    contains_saving_throw(Effects, saving_throw(Abi):_),
    Origin =.. [BaseOrigin|_],
    spell_save_dc(BaseOrigin, DC).
%contains_saving_throw(Effects, saving_throw(Abi):Effect)

%max_learnable_spell_level_for_classlevel(Class:Level, MaxSpellLevel) :-
%    caster(Class, Factor),

%! learnable_proper_spell(?Class, ?Name)
%
%  A proper spell is a spell that is not a cantrip.
%  A proper spell is learnable for a given Class if the spell is not of a
%  higher level than the highest level spell slots you have in that
%  Class, using the single class spell slot formula.
%  We use a separate but analogous calculation for warlocks using pact
%  magic slots.
learnable_proper_spell(Class, Spell) :-
    learnable_spell_level(Class, MaxSpellLevel),
    on_extended_class_spell_list(Class, Spell),
    spell_property(Spell, level, SpellLevel),
    between(1, MaxSpellLevel, SpellLevel).
meta_todo(learnable_proper_spell,
          "Not sure if I like how I implemented this for e.g. arcane trickster (which use another class' spell list)").
    
learnable_spell_level(Class, SpellLevel) :-
    class(Class),
    Class \= warlock,
    findall(L, spell_slots_single_class(L, Class, _), SlotLevels), % TODO this is inefficient
    max_member(SpellLevel, SlotLevels).
learnable_spell_level(warlock, SpellLevel) :-
    pact_magic_slot_level(SpellLevel).
meta_todo(learnable_spell_level, "Inefficiency in implementation").

    

%! on_extended_class_spell_list(+Class, +Spell)
%
%  Query whether a spell is on the extended_class_spell_list/2.
on_extended_class_spell_list(Class, Spell) :-
    % If we know which spell we're looking for, don't construct the whole list.
    ground(Spell),
    !,
    class(Class),
    ( spell_property(Spell, classes, Classes), member(Class, Classes), !
    ; extend_class_spell_list(Class, Spell), !).
on_extended_class_spell_list(Class, Spell) :-
    extended_class_spell_list(Class, Spells),
    member(Spell, Spells).

%! extended_class_spell_list(?Class, ?SpellSet)
%
%  The extended spell list for a class is the list of spells available
%  to that class in general, plus the spells added to that list
%  through choices specific to the current character (usually through
%  the subclass).
extended_class_spell_list(Class, SpellSet) :-
    class(Class),
    findall(Spell,
            (spell_property(Spell,classes,Classes),member(Class,Classes)),
            BaseSpells),
    findall(Spell, extend_class_spell_list(Class,Spell), ExtensionSpells),
    append(BaseSpells, ExtensionSpells, AllSpells),
    sort(0, @<, AllSpells, SpellSet).

%! hide_known_class_spells(Origin, Id, Class)
%
%  Each clause gives rise to a set of hide_base_options/3 clauses that
%  suppresses known spells for the matching options/3 clause.
hide_known_class_spells(_,_,_) :- false.
hide_base_option(Origin, Id, Spell) :-
    hide_known_class_spells(Origin, Id, Class),
    known_spell(SpellOrigin, Spell),
    SpellOrigin =.. [Class|_].

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

base_spell_origin(BaseOrigin) :-
    findall(BO, (spell_origin(Origin), spell_origin_to_base_spell_origin(Origin, BO)), BaseOrigins),
    list_to_set(BaseOrigins, BaseOriginsSet),
    member(BaseOrigin, BaseOriginsSet).

spell_origin_to_base_spell_origin(feat(Feat), feat(Feat)).
spell_origin_to_base_spell_origin(Origin, BaseOrigin) :-
    Origin =.. [BaseOrigin | _],
    BaseOrigin \= feat.

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

%! healing_spell(?Spell)
%
%  True iff Spell has a `heal(_)` term in its effect list.
healing_spell(Spell) :-
    spell_property(Spell, effects, Effects),
    subterm_member(heal(_), Effects).

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
single_class_virtual_full_caster_level(L, 1/N, 0) :-
    L < N.
single_class_virtual_full_caster_level(ClassLevel, 1/N, VirtualLevel) :-
    %between(2, 9, ClassLevel),
    ClassLevel >= N,
    VirtualLevel is ceil(ClassLevel / N).

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
%  The default formula for determining how many spells a caster
%  can prepare for the given class.
default_max_prepared_spells(Class, N) :-
    caster(Class, Casterness),
    caster_denominator(Casterness, Denom),
    spellcasting_ability(Class, Ability),
    ability_mod(Ability, Mod),
    class_level(Class:Level),
    N is max(1, floor(Level / Denom) + Mod).

caster_denominator(full, 1).
caster_denominator(1/N, N).

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

add_spell_effects(NewEffects, Old, New) :-
    modify_spell_field(effects, [Es1,Es2]>>append(Es1,NewEffects,Es2), Old, New).

increase_all_spell_damage_rolls(Bonus, Old, New) :-
    get_or_default(Old, effects, [], OldEffects),
    map_matching_subterms({Bonus}/[damage(El,OldRoll),damage(El,NewRoll)]
                            >> simplify_dice_sum(OldRoll+Bonus, NewRoll),
                          OldEffects,
                          NewEffects),
    New = Old.put(effects, NewEffects).
custom_format(increase_all_spell_damage_rolls(Bonus)) -->
    ["all damage rolls +", Bonus].

%! increase_all_spell_healing_rolls(-Bonus, -Old, +New)
%
%  Add a flat bonus to every spell healing roll.
increase_all_spell_healing_rolls(Bonus, Old, New) :-
    get_or_default(Old, effects, [], OldEffects),
    map_matching_subterms({Bonus}/[heal(OldFormula),heal(NewFormula)]
                          >> simplify_dice_sum(OldFormula + Bonus, NewFormula),
                          OldEffects,
                          NewEffects),
    New = Old.put(effects, NewEffects).
custom_format(increase_all_spell_healing_rolls(Bonus)) -->
    ["healing rolls + "], [Bonus].

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Spell descriptions.
(Spell ?= Desc) :-
    spell_property(Spell, desc, Desc).
sheet_json_dict(_{name: Name,
                  summary: Summary,
                  ac_formulas: ACFormulas,
                  hit_dice: HitDice,
                  ability_table: AbiTable,
                  skill_table: SkillTable,
                  languages: Languages,
                  weapons: Weapons,
                  armor: Armor,
                  tools: Tools,
                  notable_traits: NotableTraits,
                  attacks: Attacks,
                  spell_slots: SpellSlots,
                  pact_magic: PactMagic,
                  spellcasting_sections: SpellcastingSections,
                  resources: Resources}) :-
    name(Name),
    summary_json_dict(Summary),
    ac_formulas_json_dict(ACFormulas),
    hit_dice_json_dict(HitDice),
    ability_table_json_dict(AbiTable),
    skill_table_json_dict(SkillTable),
    setof_or_empty(X, trait(language(X)), Languages),
    setof_or_empty(X, trait(weapon(X)), Weapons),
    setof_or_empty(X, trait(armor(X)), Armor),
    setof_or_empty(X, trait(tools(X)), Tools),
    notable_traits_json_dict(NotableTraits),
    attack_table_json_dict(Attacks),
    spell_slot_dict(SpellSlots),
    pact_magic_json_dict(PactMagic),
    findall(X, spellcasting_section_json_dict(X), SpellcastingSections),
    resources_json(Resources).

setof_or_empty(X, Spec, Xs) :-
    setof(X, Spec, Xs), !.
setof_or_empty(_, _, []).


% TODO move somewhere else
traits_and_bonuses_json(Json) :-
    level(Level),
    findall(LAtom-J,
            (between(1, Level, L),
             traits_and_bonuses_at_level_json(L, J),
             atom_number(LAtom, L)),
            Pairs),
    dict_pairs(Json, _, Pairs).

traits_and_bonuses_at_level_json(Level, Json) :-
    findall(_{origin: OriginJson, effect: EffectJson, desc: Desc},
            ( trait_from_level_reached(Level, Origin, Trait),
              term_to_json(Origin, OriginJson),
              term_to_json(Trait, EffectJson),
              (Trait ?= Desc -> true ; Desc = null)
            ),
            Json).

call_snd(Id-Goal, Id-Result) :-
    Goal =.. [Pred|Args],
    append(Args, [Result], NewArgs),
    Goal2 =.. [Pred|NewArgs],
    call(Goal2).

% Summary table.
summary_json_dict(Dict) :-
    IdGoals
    = [ class-classes_str,
        race-race_str,
        level-level,
        maxhp-max_hp,
        ac-ac,
        initiative-initiative,
        speed-display_speed, % default_on_fail(0, speed),
        hd-default_on_fail("", hit_dice_string),
        pp-passive_perception,
        prof_bon-proficiency_bonus ],
    maplist(call_snd, IdGoals, Pairs),
    dict_pairs(Dict, _, Pairs).

display_speed(Speed) :-
    \+ bonus('unarmored speed'+_),
    speed(SpeedVal),
    fmt(format_speed(SpeedVal), Speed).
display_speed(Speed) :-
    bonus('unarmored speed'+_),
    speed(SpeedVal),
    unarmored_speed_bonus(Bonus),
    fmt(format_speed(SpeedVal+Bonus), Speed).
format_speed(Ft) --> {atomic(Ft)}, [Ft], [" ft"].
format_speed(Ft1+Ft2) --> [Ft1], ["+"], [Ft2], [" ft"].


classes_str(Str) :-
    findall(ClassLevel, class_level(ClassLevel), ClassLevels),
    fmt(format_list(ClassLevels), Str).

race_str(Str) :-
    most_specific_race(Race),
    !,
    fmt(format_term(Race), Str).
race_str("").

hit_dice_string(Str) :-
    hit_dice(HD),
    fmt(format_dice_sum(HD), Str).

% AC Formulas
ac_formulas_json_dict(Jsons) :-
    findall(AcFormulaJson, ac_formula_json_dict(AcFormulaJson), Jsons).

ac_formula_json_dict(_{name: NameString, ac: AC, shield: Shield}) :-
    ac(Name, AC, Options),
    get_formula_name(Name, NameString),
    ac_formula_shield_value(Options, Shield).

ac_formula_shield_value(ACOptions, ShieldAC) :-
    member(shield(_):ShieldAC, ACOptions).
ac_formula_shield_value(ACOptions, null) :-
    \+ member(shield(_):_, ACOptions).

get_formula_name(armor(Armor), Armor) :- atom(Armor), !.
get_formula_name(armor(Armor + N), Str) :-
    !,
    atomics_to_string([Armor, " + ", N], Str).
get_formula_name(Other, Str) :-
    fmt(format_term(Other), Str).
    
% Hit dice.
hit_dice_json_dict(HitDiceJson) :-
    hit_dice(HitDice),
    hit_dice_json_dict_(HitDice, HitDiceJson).
hit_dice_json_dict_([], []).
hit_dice_json_dict_(0, []).
hit_dice_json_dict_(N d M, [_{d: M, n: N}]).
hit_dice_json_dict_(HitDice + N d M, [_{d: M, n: N} | HitDiceJson]) :-
    hit_dice_json_dict_(HitDice, HitDiceJson).


% Ability table.
ability_table_json_dict(Dict) :-
    findall(Entry, ability_table_entry(Entry), Pairs),
    dict_pairs(Dict, _, Pairs).

ability_table_entry(Abi-_{ score: Score,
                           base: Base,
                           total_bonus: TotalBonus,
                           mod: Mod,
                           st: ST,
                           stProf: STProf
                         }) :-
    ability(Abi, Score),
    base_ability(Abi, Base),
    sum_bonuses(Abi, TotalBonus),
    ability_mod(Abi, Mod),
    saving_throw(Abi, ST),
    (trait(saving_throw(Abi)) -> STProf = true ; STProf = false).

% Skill table.
skill_table_json_dict(Dict) :-
    findall(Skill-_{ score: Mod, proficient: Prof },
            ( skill(Skill,Mod),
              (trait(skill(Skill)) -> Prof = true ; Prof = false)
            ),
            Pairs),
    dict_pairs(Dict, _, Pairs).

% Notable traits.
notable_traits_json_dict(TraitDictsPerCat) :-
    notable_traits_by_category(TraitsPerCat),
    maplist(trait_category_json_dict, TraitsPerCat, TraitDictsPerCat).

trait_category_json_dict(Cat-Traits, _{category:CatStr, traits:TraitDicts}) :-
    trait_category_string(Cat, CatStr),
    maplist(trait_json_dict, Traits, TraitDicts).

trait_category_string(Compound, Atom) :-
    fully_unwrap(Compound, Atom),
    !.
trait_category_string(Cat, Str) :-
    fmt(format_term(Cat), Str).

trait_json_dict(TraitVal, _{name: Trait, desc: Desc}) :-
    %\+ member(TraitVal, [language(_), tool(_), weapon(_), armor(_), skill(_)]),
    fmt(format_trait(TraitVal), Trait),
    default_on_fail(null, ?=(TraitVal), Desc).

% Attack table.
attack_table_json_dict(List) :-
    findall(X, attack_table_json_dict_entry(X), List).
attack_table_json_dict_entry(_{name: Name,
                               range: Range,
                               to_hit_or_dc: ToHitOrDC,
                               damage: Damage,
                               notes: Notes}) :-
    attack_or_variant(NameVal, RangeVal, ToHitOrDCVal, DamageVal, NotesVal),
    fmt(format_term(NameVal), Name),
    fmt(format_range(RangeVal), Range),
    fmt(format_to_hit_or_dc(ToHitOrDCVal), ToHitOrDC),
    fmt(format_damage(DamageVal), Damage),
    fmt(format_list(NotesVal), Notes).

% Spellcasting section.
spell_slot_dict(SpellSlots) :-
    findall(N, spell_slots(_,N), SpellSlots).
    
%spell_slots_dict_entry(PactMagicStr, N) :-
%    pact_magic_slots(N),
%    pact_magic_slot_level(Level),
%    format(string(PactMagicStr), "pact magic (level ~w)", [Level]).

pact_magic_json_dict(_{slot_count: NSlots,
                       slot_level: SlotLevel}) :-
    pact_magic_slots(NSlots),
    pact_magic_slot_level(SlotLevel),
    !.
pact_magic_json_dict(null).

spellcasting_section_json_dict(
    _{origin: BaseOrigin,
      spellcasting_ability: Abi,
      spellcasting_ability_mod: AbiMod,
      spell_save_dc: DC,
      spell_attack_mod: AttackMod,
      max_prepared_spells: Prep,
      spells: Spells}) :-
    base_spell_origin(BaseOrigin),
    spellcasting_ability(BaseOrigin, Abi),
    ability_mod(Abi, AbiMod),
    %known_spell_origin_class(BaseBaseOrigin, Class),
    spell_save_dc(BaseOrigin, DC),
    spell_attack_modifier(BaseOrigin, AttackMod),
    default_on_fail(null, max_prepared_spells(BaseOrigin), Prep),
    spell_list_json_dict(BaseOrigin, Spells).

spell_list_json_dict(BaseOrigin, SpellsSorted) :-
    findall(Spell,
            spell_json_dict(BaseOrigin, Spell),
            SpellsUnsorted),
    sort(level, @=<, SpellsUnsorted, SpellsSorted).

spell_json_dict(BaseOrigin,
                _{prepared: Prepared,
                  level: Level,
                  name: Name,
                  description: Description,
                  higher_level: HigherLevel,
                  casting_time: CastingTime,
                  range: Range,
                  components: Components,
                  duration: Duration,
                  concentration: Concentration,
                  to_hit: ToHit,
                  dc: DC,
                  dc_abi: DCAbi,
                  school: School,
                  summary: Summary,
                  shortdesc: Shortdesc,
                  ritual: Ritual,
                  resources: ResourcesStrs,
                  rolls: Rolls,
                  aoe: Aoe,
                  bonuses: Bonuses
                 }) :-
    (BaseOrigin =.. [_], Origin =.. [BaseOrigin,_] ; Origin = BaseOrigin),
    known_spell(Origin, _Ability, _, ResourcesVal, Ritual, Name),
    (known_spell_always_prepared(Origin, Name) -> Prepared=always; Prepared=maybe),
    known_spell_data(Origin, Name, Data),
    Level         = Data.level,
    Description   = Data.desc,
    (Data.higher_level = no -> HigherLevel = null ; HigherLevel = Data.higher_level),
    CastingTime   = Data.casting_time,
    RangeVal      = Data.range,
    School = Data.school,
    fmt(format_range(RangeVal), Range),
    % TODO!!!
    term_to_json(Data.components, Components),
    Duration      = Data.duration,
    Concentration = Data.concentration,
    maplist(term_string, ResourcesVal, ResourcesStrs),
    display_spell_effects(Data, Summary),
    default_on_fail(null, spell_short_desc(Name), Shortdesc),
    default_on_fail(null, known_spell_to_hit(BaseOrigin:_,Name), ToHit),
    known_spell_saving_throw_or_null(Origin, Name, DC, DCAbi),
    known_spell_dice_formula_or_null(Origin, Name, Rolls),
    known_spell_aoe_or_null(Origin, Name, Aoe),
    findall(Bonus, spell_bonus_json(Origin, Name, Bonus), Bonuses).

known_spell_saving_throw_or_null(Origin, Name, DC, Abi) :-
    known_spell_saving_throw(Origin, Name, DC, Abi),
    !.
known_spell_saving_throw_or_null(_, _, null, null).

known_spell_dice_formula_or_null(Origin, Name, RollsStr) :-
    known_spell_dice_formula(Origin, Name, Rolls),
    term_string(Rolls, RollsStr),
    !.
known_spell_dice_formula_or_null(_, _, null).

known_spell_aoe_or_null(Origin, Name, AoeStr) :-
    known_spell_aoe(Origin, Name, Aoe),
    term_string(Aoe, AoeStr),
    !.
known_spell_aoe_or_null(_, _, null).

spell_bonus_json(SpellOrigin, Spell, _{ origin : OriginStr,
                                        bonus : BonusStr
                                      }) :-
    bonus(Origin, modify_spell(SpellOrigin, Spell, Bonus)),
    (Origin = trait(SubOrigin)
      -> fmt(format_term(SubOrigin), OriginStr)
      ;  fmt(format_term(Origin), OriginStr)),
    fmt(format_term(Bonus), BonusStr).

% Resources.
resources_json(List) :-
    findall(R, resource_json(R), List).
resource_json(_{feature_name: FeatureName,
                unit_name: UnitName,
                number: Num,
                short_rest: ShortRest,
                long_rest: LongRest
               }) :-
    resource(FeatureName, UnitName, Num),
    FeatureName \= 'pact magic', % this is a special case
    rest_description(short, UnitName, ShortRest),
    rest_description(long, UnitName, LongRest).

rest_description(Type, UnitName, DescStr) :-
    on_rest(Type, UnitName, Desc),
    !,
    fmt(format_term(Desc), DescStr).
rest_description(_, _, null).
custom_format(restore(N)) --> ["+"], format_number(N).
    
%! term_to_json(+List, -Json)
%
%  One-way conversion from terms to a canonical JSON representation of Prolog
%  terms.
term_to_json(List, Jsons) :-
    is_list(List),
    !,
    maplist(term_to_json, List, Jsons).
term_to_json(Atomic, String) :-
    atomic(Atomic), atom_string(Atomic, String), !.
term_to_json(Compound, _{functor: Functor, args: ArgsJson}) :-
    Compound =.. [Functor | Args],
    Args \= [],
    !,
    maplist(term_to_json, Args, ArgsJson).
sumall(X, Pred, Sum) :-
    findall(X, Pred, Xs),
    sum_list(Xs, Sum).

subterm_member(X, T) :-
    select_subterm(X, T, _, _).

unique_subterm_member(X, T) :-
    findall(Y, (Y=X, subterm_member(Y,T)), [X]).

select_subterm(X, X , Y, Y ).
select_subterm(X, T1, Y, T2) :-
    T1 =.. [Functor|Args],
    select(SubT, Args, NewSubT, NewArgs),
    select_subterm(X, SubT, Y, NewSubT),
    T2 =.. [Functor|NewArgs].

select_first_subterm(X, T1, Y, T2) :-
    select_subterm(X, T1, Y, T2),
    !.

select_all_matching_members(_, [], _, []) :- !.
select_all_matching_members(X, [X|Xs], Y, [Y|Ys]) :-
    select_all_matching_members(X, Xs, Y, Ys).
select_all_matching_members(X, [X0|Xs], Y, [X0|Ys]) :-
    X \= X0,
    select_all_matching_members(X, Xs, Y, Ys).

map_matching_subterms(Goal, X, Y) :-
    call(Goal, X, Y), !.
map_matching_subterms(Goal, T1, T2) :-
    T1 =.. [Functor|Args1],
    !,
    maplist(map_matching_subterms(Goal), Args1, Args2),
    T2 =.. [Functor|Args2].
map_matching_subterms(_, X, X).

fully_unwrap(Atom, Atom) :-
    atomic(Atom),
    !.
fully_unwrap(Compound, Kernel) :-
    Compound =.. [_, Subterm],
    fully_unwrap(Subterm, Kernel).

test(G, x) :- G =.. [g|_].

sequence([], X, X).
sequence([Pred|Preds], X, Z) :-
    call(Pred, X, Y),
    sequence(Preds, Y, Z).
    
%! ordered_lookup_largest_leq(+KVTable, +Key, -ValueFound)
% 
%  Look up the value associated with the largest key smaller than or
%  equal to the parameter key, in the provided table.
ordered_lookup_largest_leq([Key -> Value|Table], KeyToLookup, ValueFound) :-
    Key =< KeyToLookup,
    ( ordered_lookup_largest_leq(Table, KeyToLookup, ValueFound), !
    ; ValueFound = Value
    ).

get_or_default(Dict, Field, _      , Dict.get(Field)) :- !.
get_or_default(_   , _    , Default, Default).

append_to_list(X, L1, L2) :-
    append(L1, [X], L2).

nonlist_to_singleton(X, [X]) :- \+ is_list(X).
nonlist_to_singleton(L,  L ) :-    is_list(L).

% [a:1, a:2, b:1] -> _{a:[1,2], b:[1]}
%multi_assoc_list_to_dict_of_dicts(Tag, Assocs) :-
%    dict_of_dicts_helper()

default_on_fail(_, Goal, X) :-
    call(Goal, X),
    !.
default_on_fail(Default, _, Default).

zip([], Ys, Ys).
zip(Xs, [], Xs).
zip([X|Xs], [Y|Ys], [X-Y|Zs]) :- zip(Xs, Ys, Zs).

enumerate(_, [], []) :- !.
enumerate(N, [X|Xs], [N-X|NXs]) :-
    !,
    M is N+1,
    enumerate(M, Xs, NXs).

simplify_product(0*_, 0).
simplify_product(X*0, 0) :- X \= 0.
simplify_product(1*X, X).
simplify_product(X*1, X) :- X \= 1.
simplify_product(X*Y, X*Y) :- X \= 1, Y \= 1.

odd(N) :- 1 is N mod 2.

extend_multimap(InitMap, _     , []    , InitMap).
extend_multimap(InitMap, GetKey, [X|Xs], Map    ) :-
    extend_multimap(InitMap, GetKey, Xs, Map1),
    call(GetKey, X, Key),
    add_to_multimap(Key, X, Map1, Map).



%multimap_from_list(GetKey, List, Map) :-
%    multimap_from_list_(GetKey, List, _{}, Map).
%
%multimap_from_list_(_, [], Acc, Acc).
%multimap_from_list_(GetKey, [X|Xs], Acc, Out) :-
%    call(GetKey, X, Key),
%    add_to_multimap(Key, X, Acc, Acc1),
%    multimap_from_list_(GetKey, Xs, Acc1, Out).
%

add_to_multimap(Key, X, In, Out) :-
    get_dict(Key, In, Entry),
    !,
    put_dict(Key, In, [X|Entry], Out).
add_to_multimap(Key, X, In, In.put(Key, [X])).
    
predefined_predicate(prolog_clause_name/2).
predefined_predicate(thread_message_hook/3).
predefined_predicate(prolog_file_type/2).
predefined_predicate(prolog_list_goal/1).
predefined_predicate(message_hook/3).
predefined_predicate(prolog_exception_hook/4).
predefined_predicate(prolog_load_file/2).
predefined_predicate(expand_query/4).
predefined_predicate(expand_answer/2).
predefined_predicate(file_search_path/2).
predefined_predicate(resource/2).
predefined_predicate(exception/3).
predefined_predicate(portray/1).
predefined_predicate(resource/3).
predefined_predicate(goal_expansion/4).
predefined_predicate(term_expansion/4).
predefined_predicate(term_expansion/2).
predefined_predicate(library_directory/1).
predefined_predicate(message_property/2).
predefined_predicate(goal_expansion/2).
predefined_predicate('$set_optimise_load'/1).
predefined_predicate((=\=)/2).
predefined_predicate('$append'/2).
predefined_predicate(sub_string/5).
predefined_predicate('$complete_atom'/3).
predefined_predicate('$flush_predicate'/1).
predefined_predicate(get_string_code/3).
predefined_predicate(put_char/2).
predefined_predicate('$consult_file_2'/5).
predefined_predicate(string_chars/2).
predefined_predicate('$source_file_property'/3).
predefined_predicate(get_byte/1).
predefined_predicate(dwim_predicate/2).
predefined_predicate('$new_findall_bag'/0).
predefined_predicate(float_class/2).
predefined_predicate(qcompile/2).
predefined_predicate('$flushed_predicate'/1).
predefined_predicate(open_shared_object/2).
predefined_predicate('$tbl_scc_data'/2).
predefined_predicate('$set_source_file'/3).
predefined_predicate('$source_file'/2).
predefined_predicate(halt/0).
predefined_predicate('$set_autoload_level'/1).
predefined_predicate('$reverse'/2).
predefined_predicate(plus/3).
predefined_predicate('$load_history'/0).
predefined_predicate(read_string/3).
predefined_predicate(text_to_string/2).
predefined_predicate('$tbl_worklist_data'/2).
predefined_predicate('$source_file_predicates'/2).
predefined_predicate(peek_code/1).
predefined_predicate('$compilation_level'/2).
predefined_predicate('$set_no_xref'/1).
predefined_predicate(set_random/1).
predefined_predicate('$append_'/2).
predefined_predicate(prolog_cut_to/1).
predefined_predicate(between/3).
predefined_predicate(nl/1).
predefined_predicate('$clause'/4).
predefined_predicate('$tbl_wkl_table'/2).
predefined_predicate(random_property/1).
predefined_predicate('$tbl_table_status'/4).
predefined_predicate('$prolog_list_goal'/1).
predefined_predicate('$compilation_mode'/2).
predefined_predicate('$compilation_mode_store'/1).
predefined_predicate(bounded_number/3).
predefined_predicate((<)/2).
predefined_predicate('$tbl_variant_table'/6).
predefined_predicate(open_shared_object/3).
predefined_predicate(put_byte/2).
predefined_predicate(peek_char/1).
predefined_predicate('$load_msg_level'/4).
predefined_predicate((initialization)/2).
predefined_predicate((=<)/2).
predefined_predicate('$tbl_abstract_table'/6).
predefined_predicate('$member_'/3).
predefined_predicate(succ/2).
predefined_predicate('$end_consult'/1).
predefined_predicate(string_bytes/3).
predefined_predicate('$load_msg_level'/5).
predefined_predicate('$add_findall_bag'/1).
predefined_predicate(arg/3).
predefined_predicate('$directive_mode'/2).
predefined_predicate('$save_file_scoped_flags'/1).
predefined_predicate((*->)/2).
predefined_predicate(false/0).
predefined_predicate('$compilation_mode'/1).
predefined_predicate(fail/0).
predefined_predicate(not/1).
predefined_predicate(nl/0).
predefined_predicate('$resolved_source_path'/2).
predefined_predicate(put_code/2).
predefined_predicate('$fixup_reconsult'/1).
predefined_predicate(peek_byte/1).
predefined_predicate('$initialization_context'/2).
predefined_predicate('$tbl_moded_variant_table'/6).
predefined_predicate(open/4).
predefined_predicate('$directive_mode_store'/1).
predefined_predicate('$expand_goal'/2).
predefined_predicate(open_string/2).
predefined_predicate('$tbl_set_answer_completed'/1).
predefined_predicate(string_code/3).
predefined_predicate('$set_source_files'/1).
predefined_predicate('$load_id'/4).
predefined_predicate('$tbl_add_global_delays'/2).
predefined_predicate('$time_source_file'/3).
predefined_predicate(throw/1).
predefined_predicate('$store_admin_clause'/4).
predefined_predicate(float/1).
predefined_predicate('$delete'/3).
predefined_predicate('$meta_call'/3).
predefined_predicate('$open_source'/5).
predefined_predicate('$tbl_answer_update_dl'/2).
predefined_predicate('$tbl_answer'/3).
predefined_predicate('$start_consult'/2).
predefined_predicate('$urgent_exception'/3).
predefined_predicate('$tbl_is_trienode'/1).
predefined_predicate('$record_clause'/4).
predefined_predicate(assert/1).
predefined_predicate('$set_compilation_mode'/1).
predefined_predicate(($)/0).
predefined_predicate('$reverse'/3).
predefined_predicate(format_time/4).
predefined_predicate(format/1).
predefined_predicate(catch_with_backtrace/3).
predefined_predicate('$record_included'/5).
predefined_predicate(put_byte/1).
predefined_predicate(peek_byte/2).
predefined_predicate('$tbl_delay_list'/1).
predefined_predicate('$switch_toplevel_mode'/1).
predefined_predicate(asserta/1).
predefined_predicate(compiling/0).
predefined_predicate(rational/3).
predefined_predicate('$length3'/3).
predefined_predicate('$no_lco'/0).
predefined_predicate('$tbl_existing_variant_table'/5).
predefined_predicate('$set_prolog_stack'/4).
predefined_predicate('$gc_statistics'/5).
predefined_predicate(flush_output/0).
predefined_predicate('$tbl_answer_dl'/3).
predefined_predicate('$import_list'/4).
predefined_predicate('$tbl_set_delay_list'/1).
predefined_predicate('$add_findall_bag'/2).
predefined_predicate(assertz/1).
predefined_predicate('$last'/3).
predefined_predicate('$directive_mode'/1).
predefined_predicate('$remove_same_key'/3).
predefined_predicate(put_code/1).
predefined_predicate('$unload_file'/1).
predefined_predicate('$compilation_level'/1).
predefined_predicate(current_resource/2).
predefined_predicate(shift/1).
predefined_predicate('$on_signal'/4).
predefined_predicate('$pairs_keys'/2).
predefined_predicate('$tbl_scc'/1).
predefined_predicate(nb_linkarg/3).
predefined_predicate('$source_term'/7).
predefined_predicate('$set_directive_mode'/1).
predefined_predicate(open_resource/2).
predefined_predicate('$is_option'/3).
predefined_predicate('$set_verbose_load'/2).
predefined_predicate('$remove_dup_keys'/2).
predefined_predicate(put_char/1).
predefined_predicate('$tbl_answer_update_dl'/3).
predefined_predicate(atom_string/2).
predefined_predicate(peek_code/2).
predefined_predicate('$skip_list'/3).
predefined_predicate(register_iri_scheme/3).
predefined_predicate('$canonicalise_extension'/2).
predefined_predicate('$tbl_force_truth_value'/3).
predefined_predicate(predicate_property/2).
predefined_predicate(shift_for_copy/1).
predefined_predicate((=:=)/2).
predefined_predicate(current_arithmetic_function/1).
predefined_predicate(apply/2).
predefined_predicate(flush_output/1).
predefined_predicate(string_codes/2).
predefined_predicate(peek_string/3).
predefined_predicate(peek_char/2).
predefined_predicate('$find_predicate'/2).
predefined_predicate('$start_run_initialization'/2).
predefined_predicate(atom/1).
predefined_predicate('$call_at_halt'/2).
predefined_predicate('$cgc_params'/6).
predefined_predicate('$tbl_abolish_local_tables'/0).
predefined_predicate(retract/1).
predefined_predicate(get_char/2).
predefined_predicate(statistics/2).
predefined_predicate('$unmap_id'/1).
predefined_predicate(autoload/1).
predefined_predicate('$load_input'/2).
predefined_predicate('$run_initialization'/2).
predefined_predicate(atomic/1).
predefined_predicate('$start_aux'/2).
predefined_predicate('$save_history_line'/1).
predefined_predicate('$ifcompiling'/1).
predefined_predicate(setarg/3).
predefined_predicate(current_predicate/1).
predefined_predicate('$cmd_option_val'/2).
predefined_predicate('$compile_type'/1).
predefined_predicate(redefine_system_predicate/1).
predefined_predicate('$end_aux'/2).
predefined_predicate('$seek_list'/4).
predefined_predicate((==)/2).
predefined_predicate(get_byte/2).
predefined_predicate(nb_setarg/3).
predefined_predicate('$cmd_option_set'/2).
predefined_predicate('$tbl_free_component'/1).
predefined_predicate('$tbl_table_status'/2).
predefined_predicate('$setup_load'/6).
predefined_predicate('$initialization_error'/3).
predefined_predicate('$context_type'/2).
predefined_predicate('$similar_module'/2).
predefined_predicate(compile_predicates/1).
predefined_predicate(assert/2).
predefined_predicate('$enter_sandboxed'/3).
predefined_predicate('$record_clause'/3).
predefined_predicate('$file_scoped_flag'/2).
predefined_predicate('$tbl_table_discard_all'/1).
predefined_predicate(divmod/4).
predefined_predicate('$style_check'/2).
predefined_predicate(sub_atom_icasechk/3).
predefined_predicate(format_time/3).
predefined_predicate('$predefine_foreign'/1).
predefined_predicate(ground/1).
predefined_predicate('$collect_findall_bag'/2).
predefined_predicate(nth_integer_root_and_remainder/4).
predefined_predicate(close/1).
predefined_predicate(prolog_alert_signal/2).
predefined_predicate('$tbl_answer_c'/4).
predefined_predicate('$set_sandboxed_load'/2).
predefined_predicate('$load_additional_boot_files'/0).
predefined_predicate('$negate'/2).
predefined_predicate('$tbl_destroy_table'/1).
predefined_predicate('$initialization_failure'/2).
predefined_predicate('$prepare_load_stream'/3).
predefined_predicate('$close_source'/2).
predefined_predicate((->)/2).
predefined_predicate('$autoload_nesting'/1).
predefined_predicate('$at_halt'/2).
predefined_predicate(open_resource/3).
predefined_predicate(split_string/4).
predefined_predicate((initialization)/1).
predefined_predicate('$restore_load'/5).
predefined_predicate('$tbl_trienode'/1).
predefined_predicate(get_code/2).
predefined_predicate((meta_predicate)/1).
predefined_predicate(set_input/1).
predefined_predicate('$tbl_answer_dl'/4).
predefined_predicate('$gc'/0).
predefined_predicate('$get_clause_attribute'/3).
predefined_predicate(set_output/1).
predefined_predicate('$qload_stream'/5).
predefined_predicate(collation_key/2).
predefined_predicate(current_signal/3).
predefined_predicate('$run_init_goal'/1).
predefined_predicate(source_file_property/2).
predefined_predicate(deterministic/1).
predefined_predicate(current_input/1).
predefined_predicate(call/6).
predefined_predicate(exists_source/2).
predefined_predicate(clause/2).
predefined_predicate(read_string/5).
predefined_predicate('$run_at_halt'/0).
predefined_predicate('$permission_error'/3).
predefined_predicate('$exit_code'/1).
predefined_predicate('$run_init_goal'/2).
predefined_predicate('$tbl_table_complete_all'/3).
predefined_predicate('$default_predicate'/2).
predefined_predicate(current_output/1).
predefined_predicate(atomics_to_string/3).
predefined_predicate(close/2).
predefined_predicate('$import_wic'/3).
predefined_predicate(atomic_list_concat/2).
predefined_predicate(float_parts/4).
predefined_predicate(var_property/2).
predefined_predicate('$qload_file'/5).
predefined_predicate('$run_initialization'/3).
predefined_predicate(retractall/1).
predefined_predicate(trim_stacks/0).
predefined_predicate('$consult_file'/5).
predefined_predicate(at_halt/1).
predefined_predicate(get_code/1).
predefined_predicate('$set_predicate_attribute'/3).
predefined_predicate('$tbl_variant_table'/1).
predefined_predicate('$do_load_file_2'/5).
predefined_predicate(open/3).
predefined_predicate(call/7).
predefined_predicate('$map_id'/2).
predefined_predicate('$get_files_argv'/2).
predefined_predicate(assertz/2).
predefined_predicate('$save_file_scoped_flag'/1).
predefined_predicate('$compile_init_goal'/3).
predefined_predicate('$tbl_table_pi'/2).
predefined_predicate('$get_predicate_attribute'/3).
predefined_predicate('$tbl_local_variant_table'/1).
predefined_predicate(string_length/2).
predefined_predicate('$install_staged_error'/4).
predefined_predicate(atomic_list_concat/3).
predefined_predicate(prolog_stack_property/2).
predefined_predicate(term_expansion/2).
predefined_predicate(asserta/2).
predefined_predicate('$destroy_findall_bag'/0).
predefined_predicate('$restore_file_scoped_flags'/1).
predefined_predicate(get_char/1).
predefined_predicate('$initialization'/4).
predefined_predicate('$tbl_global_variant_table'/1).
predefined_predicate('$predicate_property'/2).
predefined_predicate(clause/3).
predefined_predicate('$save_history_event'/1).
predefined_predicate('$end_run_initialization'/1).
predefined_predicate('$ensure_loaded_library_sandbox'/0).
predefined_predicate('$load_wic_files'/1).
predefined_predicate('$load_msg_compat'/2).
predefined_predicate(read_term_with_history/2).
predefined_predicate('$suspend_findall_bag'/0).
predefined_predicate(copy_predicate_clauses/2).
predefined_predicate('$clause_from_source'/4).
predefined_predicate(atomics_to_string/2).
predefined_predicate(string_concat/3).
predefined_predicate(trie_property/2).
predefined_predicate(integer/1).
predefined_predicate('$run_initialization_2'/1).
predefined_predicate(cancel_halt/1).
predefined_predicate(goal_expansion/2).
predefined_predicate('$update_autoload_level'/2).
predefined_predicate(string/1).
predefined_predicate('$usage'/0).
predefined_predicate('$execute_directive_3'/1).
predefined_predicate('$wfs_call'/2).
predefined_predicate('$domain_error'/2).
predefined_predicate(callable/1).
predefined_predicate('$rule'/3).
predefined_predicate(line_count/2).
predefined_predicate(atom_prefix/2).
predefined_predicate('$tbl_propagate_start'/1).
predefined_predicate(read_term/2).
predefined_predicate(tty_get_capability/3).
predefined_predicate(skip/1).
predefined_predicate(true/0).
predefined_predicate('$exception_in_directive'/1).
predefined_predicate('$load_file_e'/3).
predefined_predicate(attvar/1).
predefined_predicate(atom_to_term/3).
predefined_predicate(character_count/2).
predefined_predicate(sig_atomic/1).
predefined_predicate(put/1).
predefined_predicate(abort/0).
predefined_predicate(instance/2).
predefined_predicate(call_residue_vars/2).
predefined_predicate(copy_term/4).
predefined_predicate(ttyflush/0).
predefined_predicate('$declare_module'/6).
predefined_predicate(fast_write/2).
predefined_predicate(style_check/1).
predefined_predicate(get/1).
predefined_predicate('$file_conditions'/2).
predefined_predicate('$register_resource_file'/1).
predefined_predicate(call/8).
predefined_predicate('$assert_load_context_module'/3).
predefined_predicate(nth_clause/3).
predefined_predicate(line_position/2).
predefined_predicate('$tbl_propagate_end'/1).
predefined_predicate('$call_no_catch'/1).
predefined_predicate(fast_term_serialized/2).
predefined_predicate(transaction/1).
predefined_predicate('$relative_to'/3).
predefined_predicate('$file_condition'/1).
predefined_predicate(predicate_option_type/2).
predefined_predicate('$do_export_list'/3).
predefined_predicate('$vm_assert'/3).
predefined_predicate('$error_count'/2).
predefined_predicate('$wrap_predicate'/5).
predefined_predicate(term_to_atom/2).
predefined_predicate('$qend'/4).
predefined_predicate(tab/2).
predefined_predicate('$trap_gdb'/0).
predefined_predicate('$idg_add_mono_dyn_dep'/3).
predefined_predicate('$one_or_member'/2).
predefined_predicate('$rule'/2).
predefined_predicate('$qlf_auto'/3).
predefined_predicate(read_term/3).
predefined_predicate(prompt/2).
predefined_predicate('$idg_add_monotonic_dep'/3).
predefined_predicate(fast_read/2).
predefined_predicate('$tbl_reeval_prepare'/2).
predefined_predicate('$extend_file'/3).
predefined_predicate('$append'/3).
predefined_predicate('$clause_term_position'/3).
predefined_predicate('$vmi_property'/2).
predefined_predicate(call_continuation/1).
predefined_predicate('$code_class'/2).
predefined_predicate(term_string/3).
predefined_predicate(byte_count/2).
predefined_predicate(read_term_from_atom/3).
predefined_predicate('$tbl_collect_mono_dep'/0).
predefined_predicate('$install_staged_file'/4).
predefined_predicate(tty_put/2).
predefined_predicate('$idg_mono_affects_eager'/3).
predefined_predicate('$can_yield'/0).
predefined_predicate('$tbl_monotonic_add_answer'/2).
predefined_predicate('$notrace'/1).
predefined_predicate('$table_option'/2).
predefined_predicate(is_list/1).
predefined_predicate('$break_pc'/3).
predefined_predicate('$add_directive_wic2'/3).
predefined_predicate(call/1).
predefined_predicate(copy_term_nat/4).
predefined_predicate('$set_table_wrappers'/1).
predefined_predicate(term_string/2).
predefined_predicate(call_with_inference_limit/3).
predefined_predicate('$do_load_file'/5).
predefined_predicate(read_clause/3).
predefined_predicate('$idg_mono_affects'/3).
predefined_predicate(tty_goto/2).
predefined_predicate(get0/1).
predefined_predicate('$tbl_reeval_wait'/2).
predefined_predicate('$put_quoted'/4).
predefined_predicate((>)/2).
predefined_predicate('$set_pi_attr'/3).
predefined_predicate('$mono_reeval_prepare'/2).
predefined_predicate(put_attr/3).
predefined_predicate(current_trie/1).
predefined_predicate(variant_sha1/2).
predefined_predicate('$already_loaded'/4).
predefined_predicate(get0/2).
predefined_predicate(thread_create/2).
predefined_predicate('$mt_do_load'/5).
predefined_predicate('$tbl_reeval_abandon'/1).
predefined_predicate('$notrace'/2).
predefined_predicate(set_flag/2).
predefined_predicate('$break_at'/3).
predefined_predicate('$mono_idg_changed'/2).
predefined_predicate('$defined_predicate'/1).
predefined_predicate(tty_size/2).
predefined_predicate(rule/2).
predefined_predicate(get/2).
predefined_predicate(sort/4).
predefined_predicate('$length'/2).
predefined_predicate(catch/3).
predefined_predicate('$existence_error'/2).
predefined_predicate(findall/3).
predefined_predicate('$pi_head'/2).
predefined_predicate('$idg_mono_empty_queue'/2).
predefined_predicate(term_hash/2).
predefined_predicate(exists_source/1).
predefined_predicate(absolute_file_name/2).
predefined_predicate('$store_admin_clause2'/4).
predefined_predicate('$ft_no_ext'/1).
predefined_predicate(predicate_option_mode/2).
predefined_predicate((>=)/2).
predefined_predicate('$join_attrs'/3).
predefined_predicate('$dwim_correct_goal'/3).
predefined_predicate('$abs_file_error'/3).
predefined_predicate(get_attr/3).
predefined_predicate(variant_hash/2).
predefined_predicate(tab/1).
predefined_predicate(call/5).
predefined_predicate(undo/1).
predefined_predicate('$pred_option'/4).
predefined_predicate('$restore_trace'/2).
predefined_predicate('$qdo_load_file2'/5).
predefined_predicate('$register_resolved_source_path'/2).
predefined_predicate('$current_break'/2).
predefined_predicate('$cache_file_found'/4).
predefined_predicate(current_key/1).
predefined_predicate(source_file/2).
predefined_predicate('$translated_source'/2).
predefined_predicate('$common_goal_type'/3).
predefined_predicate('$resolved_source_path'/3).
predefined_predicate('$must_be'/2).
predefined_predicate('$attr_options'/3).
predefined_predicate(sig_unblock/1).
predefined_predicate(transaction/2).
predefined_predicate(forall/2).
predefined_predicate(get_flag/2).
predefined_predicate('$resolve_source_path'/3).
predefined_predicate('$attr_option'/2).
predefined_predicate(qcompile/1).
predefined_predicate(del_attr/2).
predefined_predicate(open_null_stream/1).
predefined_predicate(call/4).
predefined_predicate(?= / 2).
predefined_predicate('$goal_type'/3).
predefined_predicate('$xr_member'/2).
predefined_predicate((is)/2).
predefined_predicate(tnot/1).
predefined_predicate('$in_system_dir'/1).
predefined_predicate('$mono_reeval_done'/3).
predefined_predicate(on_signal/3).
predefined_predicate(skip/2).
predefined_predicate('$qdo_load_file'/4).
predefined_predicate(transaction/3).
predefined_predicate('$file_condition'/2).
predefined_predicate('$valid_clause'/1).
predefined_predicate(import_module/2).
predefined_predicate(prolog_choice_attribute/3).
predefined_predicate('$chk_alias_file'/6).
predefined_predicate(set_prolog_flag/2).
predefined_predicate(rule/3).
predefined_predicate('$chk_file'/5).
predefined_predicate(at_end_of_stream/1).
predefined_predicate('$put_token'/2).
predefined_predicate(halt/1).
predefined_predicate('$idg_mono_affects_lazy'/5).
predefined_predicate(call/3).
predefined_predicate('$cross_module_clause'/1).
predefined_predicate(call_cleanup/2).
predefined_predicate(compound/1).
predefined_predicate('$memberchk'/3).
predefined_predicate(put/2).
predefined_predicate(erase/1).
predefined_predicate(tmp_file_stream/3).
predefined_predicate('$idg_set_current'/2).
predefined_predicate(current_op/3).
predefined_predicate(message_to_string/2).
predefined_predicate('$list_to_set'/2).
predefined_predicate(unsetenv/1).
predefined_predicate('$load_ctx_options'/2).
predefined_predicate(current_predicate/2).
predefined_predicate('$mt_load_file'/4).
predefined_predicate('$idg_changed'/1).
predefined_predicate(op/3).
predefined_predicate(fill_buffer/1).
predefined_predicate('$source_term'/8).
predefined_predicate(set_module/1).
predefined_predicate('$def_modules'/2).
predefined_predicate(once/1).
predefined_predicate('$search_path_gc_time'/1).
predefined_predicate('$undefined_procedure'/4).
predefined_predicate(create_prolog_flag/3).
predefined_predicate(call_shared_object_function/2).
predefined_predicate(at_end_of_stream/0).
predefined_predicate('$tnot_implementation'/2).
predefined_predicate(call/2).
predefined_predicate('$pack_detach'/2).
predefined_predicate(sort/2).
predefined_predicate('$load_goal'/2).
predefined_predicate('$idg_forced'/1).
predefined_predicate('$idg_reset_current'/0).
predefined_predicate('$file_error'/5).
predefined_predicate(print_message_lines/3).
predefined_predicate(compile_aux_clauses/1).
predefined_predicate('$ensure_extensions'/3).
predefined_predicate('$pack_attach'/2).
predefined_predicate('$import_from_loaded_module'/3).
predefined_predicate(prolog_frame_attribute/3).
predefined_predicate(thread_join/1).
predefined_predicate('$is_answer_trie'/2).
predefined_predicate('$open_shared_object'/3).
predefined_predicate(call_cleanup/3).
predefined_predicate(attach_packs/1).
predefined_predicate(keysort/2).
predefined_predicate(prolog_skip_level/2).
predefined_predicate(visible/1).
predefined_predicate(write_length/3).
predefined_predicate('$tbl_implementation'/2).
predefined_predicate('$number_list'/3).
predefined_predicate(import/1).
predefined_predicate(setup_call_cleanup/3).
predefined_predicate('$search_message'/1).
predefined_predicate('$load_goal_file'/2).
predefined_predicate(msort/2).
predefined_predicate('$mt_start_load'/3).
predefined_predicate('$instantiation_error'/1).
predefined_predicate(told/0).
predefined_predicate(prolog_skip_frame/1).
predefined_predicate('$idg_set_falsecount'/2).
predefined_predicate(recorded/2).
predefined_predicate('$idg_edge'/3).
predefined_predicate('$stream_properties'/2).
predefined_predicate((\=@=)/2).
predefined_predicate('$store_clause'/2).
predefined_predicate('$segments_to_list'/3).
predefined_predicate(recordz/3).
predefined_predicate(see/1).
predefined_predicate(close_shared_object/1).
predefined_predicate(dwim_match/2).
predefined_predicate('$stream_property'/2).
predefined_predicate('$tbl_is_answer_completed'/1).
predefined_predicate('$fetch_vm'/4).
predefined_predicate('$export1'/4).
predefined_predicate((=@=)/2).
predefined_predicate('$module_property'/2).
predefined_predicate(add_import_module/3).
predefined_predicate(@ / 2).
predefined_predicate('$stage_file'/2).
predefined_predicate('$mt_end_load'/1).
predefined_predicate('$head_name_arity'/3).
predefined_predicate('$tbl_reeval_prepare_top'/2).
predefined_predicate(recordz/2).
predefined_predicate(tell/1).
predefined_predicate('$alias_stream'/2).
predefined_predicate(size_abstract_term/3).
predefined_predicate('$import'/2).
predefined_predicate('$compile_aux_clauses'/2).
predefined_predicate(module/1).
predefined_predicate('$pack_attach'/1).
predefined_predicate((',')/2).
predefined_predicate('$search_path_file_cache'/3).
predefined_predicate('$qlf_part_mode'/1).
predefined_predicate(recorda/3).
predefined_predicate(seeing/1).
predefined_predicate('$streams_properties'/2).
predefined_predicate(copy_term/2).
predefined_predicate(stream_position_data/3).
predefined_predicate('$store_aux_clauses'/2).
predefined_predicate(trie_gen_compiled/2).
predefined_predicate('$clause_source'/3).
predefined_predicate(gc_file_search_cache/1).
predefined_predicate(attach_packs/2).
predefined_predicate(shell/1).
predefined_predicate(recorda/2).
predefined_predicate(seen/0).
predefined_predicate('$idg_add_edge'/1).
predefined_predicate(copy_term_nat/2).
predefined_predicate(set_prolog_stack/2).
predefined_predicate(getenv/2).
predefined_predicate(strip_module/3).
predefined_predicate(ignore/1).
predefined_predicate(delete_import_module/2).
predefined_predicate('$all_user_files'/1).
predefined_predicate('$pattr_directive'/2).
predefined_predicate(append/1).
predefined_predicate(current_module/1).
predefined_predicate('$local_op'/3).
predefined_predicate('$idg_add_dyncall'/1).
predefined_predicate(findall/4).
predefined_predicate(duplicate_term/2).
predefined_predicate(working_directory/2).
predefined_predicate('$set_source_module'/2).
predefined_predicate('$is_user_file'/1).
predefined_predicate(var/1).
predefined_predicate(prolog_interrupt/0).
predefined_predicate(set_prolog_gc_thread/1).
predefined_predicate('$idg_falsecount'/2).
predefined_predicate('$exported_op'/4).
predefined_predicate(set_stream_position/2).
predefined_predicate((dynamic)/2).
predefined_predicate('$export_op'/5).
predefined_predicate('$current_module'/2).
predefined_predicate(trie_gen_compiled/3).
predefined_predicate('$qstart'/3).
predefined_predicate('$head_module'/2).
predefined_predicate('$type_error'/2).
predefined_predicate('$idg_false_edge'/3).
predefined_predicate(telling/1).
predefined_predicate(recorded/3).
predefined_predicate(prolog_current_choice/1).
predefined_predicate(garbage_collect/0).
predefined_predicate('$idg_set_current'/1).
predefined_predicate(setenv/2).
predefined_predicate('$is_true'/1).
predefined_predicate('$term_in_file'/8).
predefined_predicate('$option'/2).
predefined_predicate(size_file/2).
predefined_predicate(call_with_depth_limit/3).
predefined_predicate(thread_wait/2).
predefined_predicate('$wrapped_predicate'/2).
predefined_predicate('$pop_input_context'/0).
predefined_predicate(zip_file_info_/3).
predefined_predicate(autoload/2).
predefined_predicate(load_files/2).
predefined_predicate(freeze/2).
predefined_predicate(zipper_open_new_file_in_zip/4).
predefined_predicate((\=)/2).
predefined_predicate('$module_class'/3).
predefined_predicate(prolog/0).
predefined_predicate(numbervars/3).
predefined_predicate('$import_as'/4).
predefined_predicate(thread_update/2).
predefined_predicate(sig_pending/1).
predefined_predicate(unload_file/1).
predefined_predicate('$make_config_dir'/1).
predefined_predicate(access_file/2).
predefined_predicate(sig_remove/2).
predefined_predicate(reexport/2).
predefined_predicate(write_term/3).
predefined_predicate('$query_loop'/0).
predefined_predicate(time_file/2).
predefined_predicate('$engine_create'/3).
predefined_predicate(numbervars/4).
predefined_predicate(message_queue_destroy/1).
predefined_predicate(unwrap_predicate/2).
predefined_predicate(zipper_open_current/3).
predefined_predicate('$foreign_registered'/2).
predefined_predicate('$raw_read'/1).
predefined_predicate('$reserved_module'/1).
predefined_predicate('$reset_dialect'/2).
predefined_predicate('$config'/0).
predefined_predicate((\+)/1).
predefined_predicate(ensure_loaded/1).
predefined_predicate(version/1).
predefined_predicate('<meta-call>'/1).
predefined_predicate('$snapshot'/1).
predefined_predicate((thread_initialization)/1).
predefined_predicate(!/0).
predefined_predicate('$expand_file_search_path'/3).
predefined_predicate(use_module/2).
predefined_predicate('$compile'/0).
predefined_predicate(make_library_index/2).
predefined_predicate(engine_next/2).
predefined_predicate(read_link/3).
predefined_predicate('$filled_array'/4).
predefined_predicate(current_transaction/1).
predefined_predicate(residual_goals/1).
predefined_predicate(is_dict/2).
predefined_predicate('$wrapped_implementation'/3).
predefined_predicate('$input_context'/1).
predefined_predicate(thread_peek_message/1).
predefined_predicate(acyclic_term/1).
predefined_predicate(expand_file_search_path/2).
predefined_predicate(writeln/1).
predefined_predicate(nonvar/1).
predefined_predicate(write_term/2).
predefined_predicate(version/0).
predefined_predicate('$expanded_term'/10).
predefined_predicate('$execute_directive'/3).
predefined_predicate(thread_setconcurrency/2).
predefined_predicate('$size_stream'/2).
predefined_predicate(set_end_of_stream/1).
predefined_predicate(thread_get_message/1).
predefined_predicate(locale_create/3).
predefined_predicate(zipper_goto/2).
predefined_predicate(cyclic_term/1).
predefined_predicate(nb_setval/2).
predefined_predicate(read/1).
predefined_predicate('$existing_dir_from_env_path'/3).
predefined_predicate(initialize/0).
predefined_predicate('$undefined_export'/2).
predefined_predicate('$push_input_context'/1).
predefined_predicate('$toplevel'/0).
predefined_predicate(reexport/1).
predefined_predicate(make_library_index/1).
predefined_predicate(write/1).
predefined_predicate('$destroy_module'/1).
predefined_predicate(break/0).
predefined_predicate(nb_link_dict/3).
predefined_predicate(engine_destroy/1).
predefined_predicate(prolog_listen/3).
predefined_predicate('$cwd'/1).
predefined_predicate(is_dict/1).
predefined_predicate('$garbage_collect'/1).
predefined_predicate('$raw_read'/2).
predefined_predicate('$term_size'/3).
predefined_predicate('$initialise'/0).
predefined_predicate((\==)/2).
predefined_predicate(writeq/1).
predefined_predicate('$current_source_module'/1).
predefined_predicate(get_dict/3).
predefined_predicate('$qlf_file'/5).
predefined_predicate('$chdir'/1).
predefined_predicate(stream_pair/3).
predefined_predicate(functor/4).
predefined_predicate('$rc_handle'/1).
predefined_predicate(number/1).
predefined_predicate(use_module/1).
predefined_predicate('$register_derived_source'/2).
predefined_predicate(export/1).
predefined_predicate('$update_library_index'/0).
predefined_predicate(snapshot/1).
predefined_predicate(b_set_dict/3).
predefined_predicate('$transaction'/2).
predefined_predicate(prolog_debug/1).
predefined_predicate(consult/1).
predefined_predicate('$qset_dialect'/1).
predefined_predicate(reload_library_index/0).
predefined_predicate(sig_block/1).
predefined_predicate(nb_set_dict/3).
predefined_predicate('$sig_unblock'/0).
predefined_predicate(with_tty_raw/1).
predefined_predicate(prolog_nodebug/1).
predefined_predicate('$import_all'/5).
predefined_predicate(thread_get_message/2).
predefined_predicate(copy_stream_data/3).
predefined_predicate('$factorize_term'/3).
predefined_predicate(current_char_conversion/2).
predefined_predicate(seek/4).
predefined_predicate(nonground/2).
predefined_predicate('$set_typein_module'/1).
predefined_predicate(copy_term/3).
predefined_predicate(autoload_path/1).
predefined_predicate(put_dict/3).
predefined_predicate(leash/1).
predefined_predicate(thread_signal/2).
predefined_predicate(thread_get_message/3).
predefined_predicate(copy_stream_data/2).
predefined_predicate(set_locale/1).
predefined_predicate(compare/3).
predefined_predicate('$export_ops'/3).
predefined_predicate('$derived_source'/3).
predefined_predicate('$ensure_slash'/2).
predefined_predicate(require/1).
predefined_predicate(write_canonical/1).
predefined_predicate(prolog_unlisten/2).
predefined_predicate(current_locale/1).
predefined_predicate(load_files/1).
predefined_predicate(current_functor/2).
predefined_predicate('$path_sep'/1).
predefined_predicate(unify_with_occurs_check/2).
predefined_predicate('$set_source_module'/1).
predefined_predicate('$export_list'/3).
predefined_predicate('$execute_query'/3).
predefined_predicate(dict_create/3).
predefined_predicate('$closure_predicate'/2).
predefined_predicate(thread_peek_message/2).
predefined_predicate('$set_source_location'/2).
predefined_predicate(char_conversion/2).
predefined_predicate(module_property/2).
predefined_predicate(subsumes_term/2).
predefined_predicate('$redefine_module'/3).
predefined_predicate('$current_typein_module'/1).
predefined_predicate('$derived_source_db'/3).
predefined_predicate('$xdg_directory'/2).
predefined_predicate(sleep/1).
predefined_predicate('$master_file'/2).
predefined_predicate('$transaction'/3).
predefined_predicate(prolog_listen/2).
predefined_predicate(dict_pairs/3).
predefined_predicate('$import_all2'/6).
predefined_predicate(engine_create/4).
predefined_predicate(locale_destroy/1).
predefined_predicate(nb_delete/1).
predefined_predicate(put_attrs/2).
predefined_predicate(abolish_monotonic_tables/0).
predefined_predicate(prolog_current_frame/1).
predefined_predicate(engine_yield/1).
predefined_predicate('[|]'/2).
predefined_predicate('$import_except_1'/3).
predefined_predicate('$meta_call'/1).
predefined_predicate('$c_current_predicate'/2).
predefined_predicate(source_location/2).
predefined_predicate(thread_send_message/3).
predefined_predicate(message_queue_create/2).
predefined_predicate(wait_for_input/3).
predefined_predicate(noprotocol/0).
predefined_predicate(is_thread/1).
predefined_predicate('$freeze'/2).
predefined_predicate(put_dict/4).
predefined_predicate(frozen/2).
predefined_predicate('$canonicalise_extensions'/2).
predefined_predicate('$import_except'/3).
predefined_predicate('$call_residue_vars_end'/0).
predefined_predicate(is_stream/1).
predefined_predicate('$merge_options'/3).
predefined_predicate(tracing/0).
predefined_predicate('$file_type_extensions'/2).
predefined_predicate('$modified_id'/3).
predefined_predicate('$is_options'/1).
predefined_predicate(engine_next_reified/2).
predefined_predicate(same_term/2).
predefined_predicate('$suspend'/3).
predefined_predicate(get_attrs/2).
predefined_predicate(abolish_shared_tables/0).
predefined_predicate(abolish_nonincremental_tables/1).
predefined_predicate('$term_id'/2).
predefined_predicate(abolish/2).
predefined_predicate('$rdef_response'/2).
predefined_predicate(read_pending_chars/3).
predefined_predicate(thread_send_message/2).
predefined_predicate(zip_unlock/1).
predefined_predicate('$attvars_after_choicepoint'/2).
predefined_predicate(protocola/1).
predefined_predicate(('.')/3).
predefined_predicate(thread_alias/1).
predefined_predicate('$spec_extension'/2).
predefined_predicate(print_message/2).
predefined_predicate('$nospy'/1).
predefined_predicate('$option'/3).
predefined_predicate(functor/3).
predefined_predicate((@>)/2).
predefined_predicate('$exported_ops'/3).
predefined_predicate('$rdef_response'/4).
predefined_predicate(mutex_create/2).
predefined_predicate(thread_detach/1).
predefined_predicate('$recover_and_rethrow'/2).
predefined_predicate((@>=)/2).
predefined_predicate(message_queue_property/2).
predefined_predicate(mutex_unlock_all/0).
predefined_predicate('$eval_when_condition'/2).
predefined_predicate('$is_named_var'/1).
predefined_predicate('$unbind_template'/1).
predefined_predicate((@<)/2).
predefined_predicate(zip_lock/1).
predefined_predicate(message_queue_set/2).
predefined_predicate(read_pending_codes/3).
predefined_predicate(thread_affinity/3).
predefined_predicate(protocol/1).
predefined_predicate('$tbl_node_answer'/2).
predefined_predicate('$qlf_out_of_date'/3).
predefined_predicate('$pi'/1).
predefined_predicate('$spy'/1).
predefined_predicate(engine_create/3).
predefined_predicate((@=<)/2).
predefined_predicate(zip_open_stream/3).
predefined_predicate(b_getval/2).
predefined_predicate('$store_clause'/4).
predefined_predicate(mutex_create/1).
predefined_predicate(var_number/2).
predefined_predicate('$import_ops'/3).
predefined_predicate(compound_name_arguments/3).
predefined_predicate('$select_option'/3).
predefined_predicate('$load_files'/3).
predefined_predicate(nb_linkval/2).
predefined_predicate(set_prolog_IO/3).
predefined_predicate(term_variables/2).
predefined_predicate(dwim_match/3).
predefined_predicate('$leash'/2).
predefined_predicate('$segments_to_atom'/2).
predefined_predicate(zip_close_/2).
predefined_predicate(protocolling/1).
predefined_predicate('$idg_mono_invalidate'/1).
predefined_predicate('$canonical_pi'/2).
predefined_predicate(flag/3).
predefined_predicate(prompt1/1).
predefined_predicate(b_setval/2).
predefined_predicate(setof/3).
predefined_predicate(thread_statistics/3).
predefined_predicate('$qq_open'/2).
predefined_predicate(prolog_load_context/2).
predefined_predicate(source_file/1).
predefined_predicate(print/1).
predefined_predicate(compound_name_arity/3).
predefined_predicate(absolute_file_name/3).
predefined_predicate(message_queue_create/1).
predefined_predicate(thread_property/2).
predefined_predicate(mutex_trylock/1).
predefined_predicate(with_output_to/2).
predefined_predicate('$remove_ops'/3).
predefined_predicate(engine_self/1).
predefined_predicate('$noload'/3).
predefined_predicate(trace/0).
predefined_predicate('$make_path'/3).
predefined_predicate(locale_property/2).
predefined_predicate(zip_clone/2).
predefined_predicate('$expand_file_search_path'/4).
predefined_predicate('$call_residue_vars_start'/0).
predefined_predicate('$prof_procedure_data'/8).
predefined_predicate(mutex_unlock/1).
predefined_predicate(length/2).
predefined_predicate(start_tabling/3).
predefined_predicate('$load_one_file'/3).
predefined_predicate(current_engine/1).
predefined_predicate(notrace/0).
predefined_predicate('$require'/1).
predefined_predicate(get_single_char/1).
predefined_predicate(nb_getval/2).
predefined_predicate('$thread_sigwait'/1).
predefined_predicate(sub_atom/5).
predefined_predicate(mutex_destroy/1).
predefined_predicate(set_system_IO/3).
predefined_predicate(thread_join/2).
predefined_predicate(del_attrs/1).
predefined_predicate(start_subsumptive_tabling/3).
predefined_predicate('$list_to_conj'/2).
predefined_predicate('$visible'/2).
predefined_predicate((=..)/2).
predefined_predicate(repeat/0).
predefined_predicate('$load_file_list'/3).
predefined_predicate(abolish/1).
predefined_predicate('$c_wrap_predicate'/5).
predefined_predicate('$lib_prefix'/1).
predefined_predicate(nb_current/2).
predefined_predicate(mutex_property/2).
predefined_predicate('$compile_term'/5).
predefined_predicate('$prof_node'/8).
predefined_predicate(mutex_lock/1).
predefined_predicate(set_stream/2).
predefined_predicate('$debuglevel'/2).
predefined_predicate(($)/1).
predefined_predicate('$fail'/0).
predefined_predicate('$load_file'/3).
predefined_predicate('$init_goal'/3).
predefined_predicate('$loading_file'/3).
predefined_predicate(trie_insert/4).
predefined_predicate(default_module/2).
predefined_predicate(not_exists/1).
predefined_predicate('$atom_hashstat'/2).
predefined_predicate(date_time_stamp/2).
predefined_predicate('$module_name'/4).
predefined_predicate('$in_library'/3).
predefined_predicate('$qlf_start_file'/1).
predefined_predicate('$load_ctx_option'/1).
predefined_predicate('$default_module'/2).
predefined_predicate(current_table/2).
predefined_predicate(format/2).
predefined_predicate('$expand_closure'/3).
predefined_predicate('$start_module'/4).
predefined_predicate('$qlf_start_sub_module'/1).
predefined_predicate(trie_gen/2).
predefined_predicate(findnsols/4).
predefined_predicate('$member'/2).
predefined_predicate('$clear_source_admin'/1).
predefined_predicate(term_variables/3).
predefined_predicate('$save_lex_state'/2).
predefined_predicate('$tbl_wkl_is_false'/1).
predefined_predicate(normalize_space/2).
predefined_predicate('$hide'/1).
predefined_predicate('$qlf_start_module'/1).
predefined_predicate(term_attvars/2).
predefined_predicate('$dwim_predicate'/2).
predefined_predicate('$tbl_wkl_negative'/1).
predefined_predicate(string_upper/2).
predefined_predicate('$iso'/1).
predefined_predicate(setlocale/3).
predefined_predicate(license/2).
predefined_predicate('$add_directive_wic'/1).
predefined_predicate(findnsols/5).
predefined_predicate('$my_file'/1).
predefined_predicate('$expansion_member'/4).
predefined_predicate('$define_predicate'/1).
predefined_predicate(trie_term/2).
predefined_predicate('$inference_limit_false'/1).
predefined_predicate('$read_clause_options'/2).
predefined_predicate(unifiable/3).
predefined_predicate('$tmp_file_stream'/4).
predefined_predicate(rational/1).
predefined_predicate('$term_multitons'/2).
predefined_predicate('$uninstantiation_error'/1).
predefined_predicate(string_lower/2).
predefined_predicate('$tbl_wkl_done'/1).
predefined_predicate('$trie_gen_node'/3).
predefined_predicate(undefined/0).
predefined_predicate('$free_variable_set'/3).
predefined_predicate(term_singletons/2).
predefined_predicate('$end_consult'/3).
predefined_predicate(stamp_date_time/3).
predefined_predicate('$open_wic'/2).
predefined_predicate(upcase_atom/2).
predefined_predicate('$start_non_module'/4).
predefined_predicate('$tbl_pop_worklist'/2).
predefined_predicate('$expand_term'/4).
predefined_predicate('$expects_dialect'/1).
predefined_predicate('$close_message'/1).
predefined_predicate(current_prolog_flag/2).
predefined_predicate(expand_goal/2).
predefined_predicate('$qlf_assert_clause'/2).
predefined_predicate(downcase_atom/2).
predefined_predicate('$trie_compile'/2).
predefined_predicate(trie_gen/3).
predefined_predicate(current_flag/1).
predefined_predicate('$skip_script_line'/2).
predefined_predicate(abolish_private_tables/0).
predefined_predicate('$thread_init'/0).
predefined_predicate(expand_term/2).
predefined_predicate('$term_attvar_variables'/2).
predefined_predicate('$tbl_wkl_make_follower'/1).
predefined_predicate('$trie_property'/2).
predefined_predicate(format/3).
predefined_predicate(writeln/2).
predefined_predicate('$depth_limit'/3).
predefined_predicate('$prof_statistics'/5).
predefined_predicate('$resolved_source_path_db'/3).
predefined_predicate(abolish_all_tables/0).
predefined_predicate('$tabled'/2).
predefined_predicate(abolish_module_tables/1).
predefined_predicate('$wrap$undefined'/0).
predefined_predicate(license/0).
predefined_predicate(code_type/2).
predefined_predicate('$qlf_load'/2).
predefined_predicate(wildcard_match/3).
predefined_predicate((thread_local)/1).
predefined_predicate('$set_debugger_write_options'/1).
predefined_predicate('$depth_limit_false'/3).
predefined_predicate(write/2).
predefined_predicate('$check_load_non_module'/2).
predefined_predicate('$repeat_and_read_error_mode'/1).
predefined_predicate('$print_message'/2).
predefined_predicate('$start_monotonic'/2).
predefined_predicate(abolish_table_subgoals/1).
predefined_predicate('$wrap$tabled_call'/1).
predefined_predicate('$set_pattr'/3).
predefined_predicate(known_licenses/0).
predefined_predicate('$qlf_end_part'/0).
predefined_predicate(char_type/2).
predefined_predicate('$compile_term'/4).
predefined_predicate('$qlf_sources'/2).
predefined_predicate((volatile)/1).
predefined_predicate('$autoload'/1).
predefined_predicate('$inference_limit'/2).
predefined_predicate(read/2).
predefined_predicate('$depth_limit_except'/3).
predefined_predicate(del_dict/4).
predefined_predicate('$restore_lex_state'/1).
predefined_predicate('$clean_history'/0).
predefined_predicate(answer_count_restraint/0).
predefined_predicate(verbose_expansion/1).
predefined_predicate('$tbl_wkl_add_suspension'/2).
predefined_predicate('$wrap$radial_restraint'/0).
predefined_predicate(open_xterm/5).
predefined_predicate((discontiguous)/1).
predefined_predicate('$loading'/1).
predefined_predicate('$find_library'/5).
predefined_predicate('$wrap_tabled'/2).
predefined_predicate('$load_context_module'/3).
predefined_predicate('$open_source'/3).
predefined_predicate('$wrap$answer_count_restraint'/0).
predefined_predicate(directory_files/2).
predefined_predicate((module_transparent)/1).
predefined_predicate('$load_ctx_options2'/2).
predefined_predicate(garbage_collect_clauses/0).
predefined_predicate('$set_encoding'/2).
predefined_predicate('$close_wic'/0).
predefined_predicate('$included'/4).
predefined_predicate('$qlf_open'/1).
predefined_predicate('$first_term'/5).
predefined_predicate('$clausable'/1).
predefined_predicate(dcg_translate_rule/4).
predefined_predicate('$inference_limit_true'/3).
predefined_predicate(write_canonical/2).
predefined_predicate(stream_property/2).
predefined_predicate(get_dict/5).
predefined_predicate(garbage_collect_atoms/0).
predefined_predicate('$tbl_wkl_answer_trie'/2).
predefined_predicate('$tbl_wkl_add_suspension'/3).
predefined_predicate(det/1).
predefined_predicate(wildcard_match/2).
predefined_predicate(print/2).
predefined_predicate('$add_dialect'/2).
predefined_predicate('$wakeup'/1).
predefined_predicate(is_most_general_term/1).
predefined_predicate(context_module/1).
predefined_predicate(radial_restraint/0).
predefined_predicate(setup_call_catcher_cleanup/4).
predefined_predicate(license/1).
predefined_predicate(non_terminal/1).
predefined_predicate(expand_file_name/2).
predefined_predicate(clause_property/2).
predefined_predicate(writeq/2).
predefined_predicate((table)/1).
predefined_predicate('$restore_load_stream'/3).
predefined_predicate('$set_dialect'/1).
predefined_predicate('$top_file'/3).
predefined_predicate('$member_rep2'/4).
predefined_predicate('$qlf_close'/0).
predefined_predicate(abolish_nonincremental_tables/0).
predefined_predicate(print_toplevel_variables/0).
predefined_predicate('$qlf_current_source'/1).
predefined_predicate(noprofile/1).
predefined_predicate('$absolute_file_name'/2).
predefined_predicate('$tbl_answer'/4).
predefined_predicate('$gc_clear'/1).
predefined_predicate('$cov_data'/3).
predefined_predicate('$is_char_code'/1).
predefined_predicate(number_chars/2).
predefined_predicate('$confirm'/1).
predefined_predicate(profiler/2).
predefined_predicate('$gc_wait'/1).
predefined_predicate('$last'/2).
predefined_predicate(trim_heap/0).
predefined_predicate('$set_pattr'/4).
predefined_predicate(start_abstract_tabling/3).
predefined_predicate(prolog_to_os_filename/2).
predefined_predicate(start_moded_tabling/5).
predefined_predicate(notrace/1).
predefined_predicate('$profile'/2).
predefined_predicate(phrase/2).
predefined_predicate(use_foreign_library/1).
predefined_predicate(exists_directory/1).
predefined_predicate('$module3'/1).
predefined_predicate('$add_encoding'/3).
predefined_predicate('$read_clause_option'/1).
predefined_predicate(current_format_predicate/2).
predefined_predicate(untable/1).
predefined_predicate(dcg_translate_rule/2).
predefined_predicate(exists_file/1).
predefined_predicate(current_atom/1).
predefined_predicate(atom_codes/2).
predefined_predicate('$inference_limit_except'/3).
predefined_predicate(format_predicate/2).
predefined_predicate((public)/1).
predefined_predicate(file_directory_name/2).
predefined_predicate(select_dict/3).
predefined_predicate(is_engine/1).
predefined_predicate('$xdg_prolog_directory'/2).
predefined_predicate(shell/2).
predefined_predicate(with_mutex/2).
predefined_predicate(atom_chars/2).
predefined_predicate(thread_create/3).
predefined_predicate('$including'/0).
predefined_predicate((multifile)/1).
predefined_predicate(file_base_name/2).
predefined_predicate(make_directory/1).
predefined_predicate(engine_fetch/1).
predefined_predicate(atomic_concat/3).
predefined_predicate(attach_packs/0).
predefined_predicate('$valid_directive'/1).
predefined_predicate(name/2).
predefined_predicate((dynamic)/1).
predefined_predicate('$prof_sibling_of'/2).
predefined_predicate('$moded_wrap_tabled'/5).
predefined_predicate(delete_directory/1).
predefined_predicate((>:<)/2).
predefined_predicate('$print_message_fail'/1).
predefined_predicate(atom_concat/3).
predefined_predicate('$btree_find_node'/5).
predefined_predicate(atom_length/2).
predefined_predicate(term_hash/4).
predefined_predicate(file_name_extension/3).
predefined_predicate('$depth_limit_true'/5).
predefined_predicate(trie_destroy/1).
predefined_predicate(rename_file/2).
predefined_predicate(delete_file/1).
predefined_predicate(mutex_statistics/0).
predefined_predicate((:<)/2).
predefined_predicate('$get_dict_ex'/3).
predefined_predicate('$is_char'/1).
predefined_predicate('$get_pid'/1).
predefined_predicate((;)/2).
predefined_predicate('$end_load_file'/2).
predefined_predicate(trie_delete/3).
predefined_predicate(reset_profiler/0).
predefined_predicate(same_file/2).
predefined_predicate(phrase/3).
predefined_predicate(trie_new/1).
predefined_predicate('$load_file'/4).
predefined_predicate(goal_expansion/4).
predefined_predicate(atom_number/2).
predefined_predicate(current_blob/2).
predefined_predicate('$check_export'/1).
predefined_predicate(tabled_call/1).
predefined_predicate(trie_lookup/3).
predefined_predicate(call_dcg/3).
predefined_predicate(is_trie/1).
predefined_predicate('$cov_stop'/0).
predefined_predicate('$inc_message_count'/1).
predefined_predicate('$is_char_list'/2).
predefined_predicate('$table_mode'/3).
predefined_predicate(thread_self/1).
predefined_predicate(trie_update/3).
predefined_predicate(is_absolute_file_name/1).
predefined_predicate(trie_insert/2).
predefined_predicate('$cov_start'/0).
predefined_predicate('$thread_local_clause_count'/3).
predefined_predicate('$undo'/1).
predefined_predicate(expand_goal/4).
predefined_predicate('$is_code_list'/2).
predefined_predicate(thread_idle/2).
predefined_predicate('$end_load_file'/1).
predefined_predicate('$select'/3).
predefined_predicate(bagof/3).
predefined_predicate((=)/2).
predefined_predicate('$prefix_module'/4).
predefined_predicate('$trie_insert_abstract'/3).
predefined_predicate(memberchk/2).
predefined_predicate('$gc_stop'/0).
predefined_predicate('$cov_reset'/0).
predefined_predicate('$current_prolog_flag'/5).
predefined_predicate(transaction_updates/1).
predefined_predicate(term_expansion/4).
predefined_predicate(engine_post/2).
predefined_predicate(thread_exit/1).
predefined_predicate('$qlf_info'/6).
predefined_predicate(reset/3).
predefined_predicate('$boot_message'/2).
predefined_predicate(expand_term/4).
predefined_predicate('$atom_completions'/2).
predefined_predicate('$set_dialect'/2).
predefined_predicate('$tbl_wkl_work'/6).
predefined_predicate('$tbl_wkl_add_answer'/4).
predefined_predicate('$mark_executable'/1).
predefined_predicate(trie_insert/3).
predefined_predicate(tmp_file/2).
predefined_predicate('$atom_references'/2).
predefined_predicate(char_code/2).
predefined_predicate('$in_reply'/2).
predefined_predicate(get_time/1).
predefined_predicate(engine_post/3).
predefined_predicate('$qlf_include'/5).
predefined_predicate(blob/2).
predefined_predicate(number_string/2).
predefined_predicate('$qlf_info'/7).
predefined_predicate(use_foreign_library/2).
predefined_predicate(number_codes/2).
predefined_predicate('$valid_term'/1).
predefined_predicate(true).
meta_todo(background, "all equipment from background").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Acolyte.
background_option(acolyte).
traits_from_source(background(acolyte),
                   [skill(insight), skill(religion),
                    'shelter of the faithful']).
trait_options_source(background(acolyte), language, wrap(language), language).
'shelter of the faithful' ?= "As an acolyte, you command the respect of those who share your faith, and you can perform the religious ceremonies of your deity. You and your adventuring companions can expect to receive free healing and care at a temple, shrine, or other established presence of your faith, though you must provide any material components needed for spells. Those who share your religion will support you (but only you) at a modest lifestyle. You might also have ties to a specific temple dedicated to your chosen deity or pantheon, and you have a residence there. This could be the temple where you used to serve, if you remain on good terms with it, or a temple where you have found a new home. While near your temple, you can call upon the priests for assistance, provided the assistance you ask for is not hazardous and you remain in good standing with your temple.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Archaeologist (NOT SRD!).
background_option(archaeologist).
traits_from_source(background(archaeologist),
                   [skill(history), skill(survival),
                    tool(cartographer), tool(navigator),
                    'historical knowledge']).
trait_options_source(background(archaeologist), language, wrap(language), language).

'historical knowledge' ?= "Can determine purpose and origin of ruin or dungeon. Can determine value of old objects.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Outlander (NOT SRD!).
background_option(outlander).
traits_from_source(background(outlander), [skill(athletics), skill(survival), wanderer]).
trait_options_source(background(outlander), language, wrap(language), language).
meta_todo(background(outlander), "plays a musical instrument").
wanderer ?= "You have an excellent memory for maps and geography, and you can always recall the general layout of terrain, settlements, and other features aorund you. In addition, you can find food and fresh water for yourself and up to five other people each day, provided that the land offers berries, small game, water, and so forth.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sage.
background_option(sage).
traits_from_source(background(sage), [skill(arcana), skill(history)]).
trait_options_source(background(sage), language, 2 unique_from language).
%! attack(?Name, ?Range, ?ToHitOrDC, ?DamageFormula, ?Notes)
%
%  Stats for an attack (typically weapon or cantrip) that is available
%  to the character.
%  For weapons (or weapon variants), this means the weapon has to be
%  in the character's posession (see has/1)
%  For cantrips, the spell has to be known (see known_spell/2).
attack(Weapon, Range, ToHit, DamageRolls, Notes) :-
    (has(Weapon) ; Weapon = unarmed),
    ( weapon_attack(Weapon, Range, ToHit, DamageRolls, Notes)
    ; weapon_variant_attack(Weapon, Range, ToHit, DamageRolls, Notes)).
attack(Cantrip, Range, to_hit(ToHit), [DamageDice], []) :-
    known_spell_to_hit(Origin, Cantrip, ToHit),
    known_spell_data(Origin, Cantrip, Data),
    Data.level = 0,
    Data.range = Range,
    unique_subterm_member(spell_attack_roll(_):DamageDice, Data.effects).
attack(Cantrip, Range, saving_throw(DC, Abi), [DamageDice], Notes) :-
    known_spell_saving_throw(Origin, Cantrip, DC, Abi),
    known_spell_data(Origin, Cantrip, Data),
    Data.level = 0,
    Data.range = Range,
    unique_subterm_member(saving_throw(_):Effect, Data.effects),
    ( (Effect = damage(_,_), Effect = DamageDice, Notes = [])
    ; (Effect = (DamageDice else Alt),
       DamageDice = damage(_,_)),
       Notes = [Str],
       format(string(Str), "on save: ~w", Alt)).

%! weapon_attack(?Weapon, ?Range, ?ToHit, ?DamageRolls, ?Notes)
%
%  Stats for an attack with a given Weapon (does not check has/1).
%  The Weapon can be a base weapon as defined by the weapon/5 predicate;
%  it can also be a compound term of the form `Weapon + Enchantment`.
weapon_attack(Weapon, Range, to_hit(ToHit), FinalDamageRolls, Notes) :-
    expand_to_sum(Weapon, BaseWeapon + Enchantment),
    weapon(BaseWeapon, _, _, _, _),
    %weapon_base_damage_rolls(BaseWeapon, BaseDamageRolls),
    weapon_attack_ability_and_modifier(BaseWeapon, _, Mod),
    weapon_proficiency_bonus(BaseWeapon, ProfBon),
    base_weapon_range(BaseWeapon, Range),
    attack_notes(BaseWeapon, Notes),
    other_bonuses_to_hit(BaseWeapon, OtherBonuses),
    ToHit is Mod + ProfBon + OtherBonuses + Enchantment,
    weapon_damage_rolls(BaseWeapon, Mod, Enchantment, FinalDamageRolls).
    %add_bonus_to_first_die(Mod + Enchantment, BaseDamageRolls, FinalDamageRolls).

%! weapon_variant_attack(?WeaponVariant, ?Range, ?ToHit, ?DamageRolls, ?Notes)
%
%  Stats for an attack with a given weapon variant (does not check has/1).
%  WeaponVariant must be exactly a term defined by the weapon_variant/4
%  predicate.
weapon_variant_attack(WeaponVariant, Range, ToHit, DamageRolls, Notes) :-
    weapon_variant(WeaponVariant, Weapon, ExtraDamageRolls, ExtraNotes),
    weapon_attack(Weapon, Range, ToHit, BaseDamageRolls, BaseNotes),
    append(BaseDamageRolls, ExtraDamageRolls, DamageRolls),
    append(BaseNotes, ExtraNotes, Notes).

weapon_damage_rolls(BaseWeapon, Mod, Enchantment, FinalRolls) :-
    weapon_base_damage_rolls(BaseWeapon, BaseRolls),
    findall(R, bonus(extra_damage_roll(BaseWeapon,R)), AdditionalRolls),
    append(BaseRolls, AdditionalRolls, AllRolls),
    add_bonus_to_first_die(Mod + Enchantment, AllRolls, FinalRolls).

weapon_base_damage_rolls(Weapon, Rolls) :-
    bonus(override_attack_damage_rolls(Weapon, Rolls)),
    !.
weapon_base_damage_rolls(Weapon, Rolls) :-
    weapon(Weapon, _, _, Rolls, _).

weapon(unarmed, unarmed, melee, [damage(bludgeoning,1)], []) :-
    \+ suppress_unarmed.

%! attack_variant(?Id, ?Range, ?ToHit, ?DamageFormula, ?Notes)
%
%  `Id = Name:_`, where `attack(Name,_,_,_,_)`.
attack_variant(Name:twohanded, Range, ToHit,
               [damage(Type,NewDmgTerm)|Terms], []) :-
    attack(Name, Range, ToHit, [damage(Type,Formula)|Terms], Notes),
    member(versatile(NewBaseDmg), Notes),
    select_first_subterm(_ d _, Formula, NewBaseDmg, NewDmgTerm).
meta_todo(versatile, "Also shows up when it's strictly worse than the onehanded variant.").
custom_format(Weapon:twohanded) --> format_term(Weapon), [" (2H)"].

%! attack_or_variant(?Id, ?Range, ?ToHit, ?DamageFormula, ?Notes, ?IsVariant:bool)
%
%  Disjunction between attack/5 and attack_variant/5, structured such that
%  attacks and their variants are "together" and tagged with a boolean indicating
%  whether the attack is a variant or not.
attack_or_variant(Id, Range, ToHit, DamageFormula, Notes, IsVariant) :-
    attack(BaseId, BaseRange, BaseToHit, BaseDamageFormula, BaseNotes),
    findall((BaseId:VarId)-VarRange-VarToHit-VarDamageFormula-VarNotes-true,
            attack_variant(BaseId:VarId, VarRange, VarToHit, VarDamageFormula, VarNotes),
            Variants),
    member(Id-Range-ToHit-DamageFormula-Notes-IsVariant,
           [BaseId-BaseRange-BaseToHit-BaseDamageFormula-BaseNotes-false | Variants]).

%! attack_or_variant(?Id, ?Range, ?ToHit, ?DamageFormula, ?Notes)
%
%  attack_or_variant/6 with the IsVariant parameter removed.
attack_or_variant(Id, Range, ToHit, DamageFormula, Notes) :-
    attack_or_variant(Id, Range, ToHit, DamageFormula, Notes, _).


%! add_bonus_to_first_die(+Bonus, +Rolls, -NewRolls)
add_bonus_to_first_die(Bonus, [damage(Type,Roll)|Rolls], [damage(Type,NewRoll)|Rolls]) :-
    EvaldBonus is Bonus,
    simplify_dice_sum(Roll+EvaldBonus, NewRoll).

attack_notes(Weapon, Notes) :-
    weapon(Weapon, _, _, _, WeaponNotes),
    findall(Note,
            (bonus(add_weapon_note(Weapon, Note)) ; not_proficient_note(Weapon, Note)),
            BonusNotes),
    append(WeaponNotes, BonusNotes, Notes).

not_proficient_note(Weapon, "not proficient") :-
    \+ weapon_proficiency(Weapon).

%! base_weapon_range(?Weapon, ?Range)
%
%  Range is either a term `feet(N)` or `feet(N) / feet(M)` where N and
%  M are numbers. In theory you could have a different unit of length
%  instead of feet.
base_weapon_range(BaseWeapon, feet(Range)) :-
    weapon(BaseWeapon, _, melee, _, Notes),
    findall(Ft, (member(reach(feet(Ft)),Notes); bonus(reach+feet(Ft))), Fts),
    sum_list(Fts, Bonus),
    Range is 5 + Bonus.
base_weapon_range(BaseWeapon, Range) :-
    weapon(BaseWeapon, _, ranged(Range), _, _).

%! weapon_attack_ability_and_modifier(?Weapon, ?Abi, ?Mod)
%
%  Determine the ability and corresponding ability modifier used to
%  make attack and damage rolls with the given weapon.
%  Automatically picks the best one.
weapon_attack_ability_and_modifier(Weapon, Abi, Mod) :-
    findall(A, weapon_ability_candidate(Weapon, A), Abis),
    highest_ability_mod_from(Abis, Abi, Mod).

%! weapon_ability_candidate(?Weapon, ?Ability)
%
%  True iff Ability can be used to calculate to hit and damage for
%  attacks with Weapon.
weapon_ability_candidate(Weapon, str) :- weapon(Weapon, _, melee, _, _).
weapon_ability_candidate(Weapon, Abi) :- bonus(use_ability(Weapon, Abi)).
weapon_ability_candidate(Weapon, dex) :-
    weapon(Weapon, _, _, _, Notes), member(finesse, Notes).
weapon_ability_candidate(Weapon, dex) :- weapon(Weapon, _, ranged(_), _, _).
    

%! weapon_proficiency_bonus(?Weapon, ?ProfBon)
%
%  Calculate the proficiency bonus with Weapon (0 if not proficient).
weapon_proficiency_bonus(Weapon, ProfBon) :-
    (weapon_proficiency(Weapon) ; Weapon = unarmed),
    !,
    proficiency_bonus(ProfBon).
weapon_proficiency_bonus(_, 0).

other_bonuses_to_hit(Weapon, TotalBonus) :-
    weapon(Weapon, _, _, _, _),
    findall(B, bonus(to_hit(Weapon) + B), Bonuses),
    sum_list(Bonuses, TotalBonus).
:- use_module(library(http/html_write)).

out :-
    warn_if_problems,
    html_write:html_set_options([doctype(html)]),
    char_sheet_html(Html),
    !,
    out_file_name(FileName, []),
    open(FileName, write, Stream),
    html_write:page(Html, Tokens, []),
    html_write:print_html(Stream, Tokens),
    close(Stream).
out_file_name --> {name(CharName)}, seq_atom(CharName), seq_atom(".html").

char_sheet_html([head(Head), body(Body)]) :-
    char_sheet_head(Head),
    char_sheet_body(Body).

char_sheet_head([title(Name), link([rel=stylesheet, href='static/css/charsheet.css'], [])]) :-
    name(Name).

char_sheet_body([Div]) :-
    name(CharName),
    body_contents(Contents),
    Div = div(class(container),
              [header(h1(CharName)), article(Contents)]).

body_contents(Contents) :-
    call_all([character_summary,
              ability_table,
              skill_table,
              proficiency_list,
              trait_list,
              attack_table,
              spellcasting_section],
              %spell_slot_table,
              %spell_preparation_table,
              %spell_table],
             Contents).
call_all(Preds, Xs) :-
    maplist([P,X]>>call(P,X), Preds, Xs).

character_summary(Div) :-
    Div = div([table( [id=summary, style='padding: 4px'] ,
                      [ tr([th("Race"), td(Race)])
                      , tr([th("Class"), td(CLsFmt)])
                      , tr([th("Level"), td(Level)])
                      , tr([th("Max HP"), td(HP)])
                      , tr([th("AC"), td(AC)])
                      , tr([th("Initiative"), td(Init)])
                      , tr([th("Speed"), td([Speed, ' ft'])])
                      , tr([th("HD"), td(HD)])
                      , tr([th("PP"), td(PP)])
                      , tr([th("Prof Bon"), td(ProfBon)])
                      ])]),
    most_specific_race(Race),
    findall(CL, class_level(CL), CLs), format_list(CLs, CLsFmt, []),
    level(Level),
    max_hp(HP),
    ac(AC),
    initiative(InitVal), format_bonus(InitVal, Init, []),
    speed(Speed),
    hit_dice(HDTerm), format_dice_sum(HDTerm, HD, []),
    passive_perception(PP),
    proficiency_bonus(ProfBonVal), format_bonus(ProfBonVal, ProfBon, []).

% Ability table.
ability_table(Table) :- 
    table('abilities', 'Abilities', Contents, Table),
    Contents = [tr([th([]), th('Score'), th('Mod'), th('ST')])|Rows],
    findall(Row, ability_table_row(_, Row), Rows).
ability_table_row(Abil, tr([th(AbilHdr), td(Score), td(Mf), td(ST)])) :-
    ability_hdr(Abil, AbilHdr),
    ability(Abil, Score),
    ability_mod(Abil, MfVal), format_bonus(MfVal, Mf, []),
    saving_throw(Abil, STVal), format_bonus(STVal, ST, []).
ability_hdr(str, 'STR').
ability_hdr(dex, 'DEX').
ability_hdr(con, 'CON').
ability_hdr(int, 'INT').
ability_hdr(wis, 'WIS').
ability_hdr(cha, 'CHA').

% Skill table.
skill_table(Table) :-
    table('skills', 'Skills', [Header|Rows], Table),
    Header = tr([th([]), th('Skill'), th('Score')]),
    findall(Row, 
            (ability(Abil), skill_table_rows_for_abil(Abil, Row)),
            AbilRows),
    flatten(AbilRows, Rows).
skill_table_rows_for_abil(Abil, [FirstRow|OtherRows]) :-
    findall(Skill, skill_ability(Skill, Abil), Skills),
    length(Skills, NumberOfSkills),
    [FirstSkill|OtherSkills] = Skills,
    skill_table_row(FirstSkill, NumberOfSkills, FirstRow),
    repl(0, NumberOfSkills, [0|Zeros]),
    maplist(skill_table_row, OtherSkills, Zeros, OtherRows).
skill_table_row(Skill, NumberOfSkills, tr(Row)) :- 
    skill_table_row_span_line(Skill, NumberOfSkills, RowSpanLine),
    skill(Skill, ScoreVal), format_bonus(ScoreVal, Score, []),
    append(RowSpanLine, [td(Skill), td(Score)], Row).
skill_table_row_span_line(_, 0, []) :- !.
skill_table_row_span_line(Skill, Rowspan, [td(rowspan=Rowspan, b(AbilHdr))]) :-
    skill_ability(Skill, Abil),
    ability_hdr(Abil, AbilHdr).

repl(X, Len, Xs) :-
    length(Xs, Len),
    maplist(=(X), Xs).

% Proficiencies, other than skill proficiencies.
proficiency_list(p([h2("Proficiencies"), div([id=proficiencies], ul(Profs))])) :-
    findall(Prof, proficiency_list_entry(Prof), Profs).
proficiency_list_entry(Entry) :-
      proficiency_category("Languages: ", language, Entry)
    ; proficiency_category("Weapons: ", weapon, Entry)
    ; proficiency_category("Armor: ", armor, Entry)
    ; proficiency_category("Tools: ", tool, Entry).
proficiency_category(CatHdr, CatFunctor, li([b(CatHdr) | Profs])) :-
    findall(T,
            (Search =.. [CatFunctor,X], trait(Search), maybe_tooltip(Search, X, T)),
            Ts),
    format_list_flat(Ts, Profs, []).

% Traits.
trait_list(p([h2("Notable traits"), div([id=traits], ul(Items))])) :-
    findall(li(Item), trait_list_entry(Item), Items).
%trait_list_entry(div(class=tooltip, [Trait, span(class=tooltiptext, Desc)])) :-
trait_list_entry(Entry) :-
    trait(TraitVal),
    \+ member(TraitVal, [language(_), tool(_), weapon(_), armor(_), skill(_)]),
    fmt(format_trait(TraitVal), Trait),
    maybe_tooltip(TraitVal, Trait, Entry).
    %fmt(format_trait(TraitVal), Trait),
    %format_trait(TraitVal, Trait),

    % ((TraitVal ?= Desc), !; source(TraitVal,Src), format_source(Src,Desc,[])).

format_trait(feat(Feat)) --> !, ['feat: '], format_term(Feat).
% format_trait(T) --> format_term(T), [' ('], summary(T), !, [')'].
format_trait(T) --> format_term(T).

% Attacks.
attack_table(Table) :-
    table('attacks', 'Attacks', [Header|Rows], Table),
    Header = tr([th([]), th('Range'), th('To Hit'), th('Damage'), th('Notes')]),
    findall(Row, attack_table_row(Row), Rows).
attack_table_row(tr([td(Name), td(Range), td(ToHitOrDC), td(DamageFmt), td(FNotes)])) :-
    attack(Name, RangeVal, ToHitOrDCVal, Damage, Notes),
    format_list(Notes, FNotes, []),
    fmt(format_range(RangeVal), Range),
    format_to_hit_or_dc(ToHitOrDCVal, ToHitOrDC, []),
    format_damage(Damage, DamageFmt, []).

format_to_hit_or_dc(to_hit(ToHit)) --> format_bonus(ToHit).
format_to_hit_or_dc(saving_throw(DC, Abi)) -->
    ["DC "], [DC], [" ("], [Abi], [")"].

% Spellcasting section.
spellcasting_section(p([])) :- \+ known_spell(_,_).
spellcasting_section(p([h2("Spellcasting"),
                        SpellSlotTable|
                        OriginSections])) :-
    spell_slot_table(SpellSlotTable),
    findall(OriginSection,
            spell_origin_section(OriginSection),
            OriginSections).

spell_slot_table(Table) :-
    table('spell_slots', 'Spell slots', [tr(Header)|Slots], Table),
    findall(Cell, spell_slot_table_header_cell(Cell), Header),
    findall(Cell, spell_slot_table_slot_cell(Cell), Slots).
spell_slot_table_header_cell(th(Cell)) :-
    pact_magic_slot_level(SlotLevel),
    format(string(Cell), "pact magic (level ~w)", [SlotLevel]).
spell_slot_table_header_cell(th(LevelStr)) :-
    spell_slots(Level, _),
    atomics_to_string(['lvl ', Level], LevelStr).
spell_slot_table_slot_cell(td(WarlockSlots)) :-
    pact_magic_slots(N),
    checkboxes(N, WarlockSlots).
spell_slot_table_slot_cell(td(Slots)) :-
    spell_slots(_, N),
    checkboxes(N, Slots).

spell_origin_section(p([h3(Origin),ul(Infos),SpellTable])) :-
    spell_origin(Origin),
    findall(li(Str),
            (spell_origin_info(Origin,Hdr,Info),
             format(string(Str), "~w: ~w", [Hdr,Info])),
            Infos),
    spell_table(Origin, SpellTable).
%spell_origin_info(_,_,_) :- false.
spell_origin_info(Origin, "Max prepared spells", Prep) :-
    max_prepared_spells(Origin, Prep).
spell_origin_info(Origin, "Spellcasting ability", AbiStr) :-
    spellcasting_ability(Origin, Abi),
    ability_mod(Abi, Mod),
    format_bonus(Mod, ModFmt, []),
    atomic_list_concat(ModFmt, ModAtom),
    format(string(AbiStr), "~w (~s)", [Abi, ModAtom]).
spell_origin_info(Origin, "Spell save DC", DC) :-
    Origin =.. [BaseOrigin|_],
    spell_save_dc(BaseOrigin, DC).
spell_origin_info(Origin, "Spell attack modifier", AttackModStr) :-
    Origin =.. [BaseOrigin|_],
    spell_attack_modifier(BaseOrigin, AttackMod),
    format_bonus_str(AttackMod, AttackModStr, []).

spell_preparation_table(Html) :-
    table('spell preparation', 'Spells to prepare', [Header|Rows], Table),
    Header = tr([th('Class'), th('Number'), th('Max Lvl')]),
    findall(Row, spell_preparation_table_row(Row), Rows),
    (Rows = [] -> Html = div([]); Rows \= [] -> Html = Table).
spell_preparation_table_row(tr([td(Class), td(Prep), td(MaxLvl)])) :-
    class(Class),
    max_prepared_spells(Class, Prep),
    findall(Level, spell_slots_single_class(Level, Class, _), Levels),
    max_list(Levels, MaxLvl).

spell_table(Origin, Table) :-
    table('spells', 'Spells', [Header|Rows], Table),
    Header = tr([th('Lvl'), th('Src'), th('Spell'), th('CT'),
                 th('Rng'), th('Cpts'), th('Dur'), th('Conc'), th('To Hit/DC'),
                 th('Effect (summary)'), th('Res')]),
    findall(Level-Row, spell_table_row(Origin, _, Level, Row), URows),
    sort(URows, LRows),
    findall(R, member(_-R, LRows), Rows).
    %spell_table_rows(Rows).

spell_table_row(Origin, Name, SpellLevel, tr(Row)) :-
    known_spell_prepared(Origin, Name),
    known_spell(Origin, Ability, _Availability, ResourcesVal, _Ritual, Name),
    known_spell_data(Origin, Name, Data),
    SpellLevel = Data.level,
    spell_origin_shorthand(Origin, OriginShorthand),
    phrase(format_range(Data.range), Range),
    phrase(format_resources(ResourcesVal), Resources),
    format_components(Data.components, Components),
    spell_to_hit_or_dc(Ability, Data, ToHitOrDC),
    display_spell_effects(Data, Effects),
    RowFields = [SpellLevel, OriginShorthand,
                 div(class=tooltip, [Name, span(class=tooltiptext, Data.desc)]),
                 Data.casting_time, Range, Components, Data.duration, Data.concentration,
                 ToHitOrDC, Effects, Resources
                ],
    maplist(wrap(td), RowFields, Row).

spell_origin_shorthand(Class, Shorthand) :-
    class_shorthand(Class, Shorthand), !.
spell_origin_shorthand(Race, Shorthand) :-
    race_shorthand(Race, Shorthand), !.
spell_origin_shorthand(Origin:Elaboration,
                       [Shorthand, div(class=tooltip, ["*", span(class=tooltiptext, ElabFmt)])]) :-
    spell_origin_shorthand(Origin, Shorthand),
    !,
    format_term(Elaboration, ElabFmt, []).
spell_origin_shorthand(Compound,
                       [Shorthand, div(class=tooltip, ["*", span(class=tooltiptext, ElabFmt)])]) :-
    Compound =.. [Origin, Elaboration],
    spell_origin_shorthand(Origin, Shorthand),
    !,
    format_term(Elaboration, ElabFmt, []).
spell_origin_shorthand(_, "").

format_spell_availability(always, "✓") :- !.
format_spell_availability('when prepared', input(type=checkbox, [])) :- !.
format_spell_availability(A, A).

spell_to_hit_or_dc(Ability, SpellData, ToHit) :-
    Effects = SpellData.get(effects),
    subterm_member(spell_attack_roll(_):_, Effects),
    !,
    proficiency_bonus(ProfBon),
    ability_mod(Ability, Mod),
    ToHitVal is ProfBon + Mod,
    format_bonus(ToHitVal, ToHit, []).
spell_to_hit_or_dc(Ability, SpellData, DC) :-
    Effects = SpellData.get(effects),
    findall(STAbi, subterm_member(saving_throw(STAbi):_, Effects), STAbis),
    STAbis = [_|_],
    !,
    proficiency_bonus(ProfBon),
    ability_mod(Ability, Mod),
    DCVal is 8 + ProfBon + Mod,
    phrase(format_dc(DCVal, STAbis), DC).
spell_to_hit_or_dc(_, _, "-").

format_dc(DC, Abis) --> ["DC "], [DC], [" ("], format_list(Abis), [")"].

format_components([], "-") :- !.
format_components(Cs, Format) :-
    maplist(format_component, Cs, Format).
    %format_list(Formats, Format, []).
format_component(m(M), span(class=tooltip, [m, span(class=tooltiptext, M)])).
format_component(C, C) :- C \= m(_).

format_resources(Rs1 or Rs2) -->
    format_resources(Rs1), [" or "], format_resources(Rs2).
format_resources([]) --> ["-"].
format_resources([R]) --> {!}, format_resource(R).
format_resources([R|Rs]) --> format_resource(R), [', '], format_resources(Rs).
format_resource(per_rest(Dur, N)) --> {!}, checkboxes(N), [' / '], [Dur], [' rest'].
format_resource(R) --> [R].

display_spell_effects(Data, Effects) :-
    fmt(format_effects(Data.get(effects)), Effects),
    !.
display_spell_effects(_, "-").



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

thtd(Header, DataRule, [tr([th(Header), td(X)])], Tail) :-
    call(DataRule, X, Tail).
    
wrapped(Goal, Rule, Result, Tail) :-
    call(Rule, X, Tail),
    call(Goal, X, Result).

pred(Pred, Result, _) :-
    call(Pred, Result).

checkboxes(N, Boxes) :-    
    repl(input(type=checkbox, []), N, Boxes).

checkboxes(N) --> {checkboxes(N, Boxes)}, seq(Boxes).

% Helper predicates.
table(Id, Caption, Contents, table(id=Id, [caption(h4(Caption))|Contents])).

tooltip(Text, Tooltip, div(class=tooltip, [Text, span(class=tooltiptext, Tooltip)])).

maybe_tooltip(Subject, Text, WithTooltip) :-
    (Subject ?= Tooltip),
    tooltip(Text, Tooltip, WithTooltip).
%maybe_tooltip(Subject, Text, WithTooltip) :-
%    source(Subject, Source),
%    phrase(format_source(Source), Tooltip),
%    tooltip(Text, Tooltip, WithTooltip).
maybe_tooltip(Subject, Text, Text) :-
    \+ (Subject ?= _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! trait(Source, Trait)
%
%  Traits are (most of) the named and idempotent properties (ie the properties
%  that don't stack) of your character.
%
%  Examples:
%  * trait(race(elf), 'fey ancestry')
%  * trait(race(elf), darkvision)
%  * trait(choice(class(_:4), abi_or_feat), feat(alert))
%  
%  Examples of things that idempotent yet are not traits:
%  * the spells your character knows (see known_spell/5)
%  * classes and subclasses of your character (see class/1 and subclass/1)
trait(Source, Trait) :-
    % Traits from trait_source/2 clauses for which we match the requirement.
    trait_source(Source, Trait),
    call(Source).
trait(choice(Source, Id), Trait) :-
    choice_member(Source, Id, Choice),
    choice_member_to_trait(Source, Id, Goal),
    call(Goal, Choice, Trait).

%! trait(Trait)
%
%  Shorthand for trait/2, when you're not interested in the source.
trait(Trait) :- trait(_, Trait).

%! trait_source(?Source, ?Trait)
%
%  Each trait_source/2 clause gives rise to a corresponding
%  trait/2 clause, *if* call(Source) is true.
trait_source(Source, Trait) :-
    traits_from_source(Source, Traits),
    member(Trait, Traits).

%! traits_from_source(?Source, ?Traits)
%
%  Equivalent to asserting a trait_source(Source, Trait) clause for
%  each member(Trait, Traits).
traits_from_source(_,_) :- false.

%! trait_options_source(?Source, ?Id, ?ToTrait, ?Spec)
%  
%  Each trait_options_source/4 clause gives rise to a corresponding
%  trait_options/4 clause, *if* call(Source) is true.
trait_options_source(_,_,_,_) :- false.

%! trait_options(?Source, ?Id, ?Spec, ?ToTrait)
%
%  Each clause gives rise to a corresponding options/3 clause,
%  as well as a corresponding choice_member_to_trait/3 clause.
trait_options(Source, Id, ToTrait, Spec) :-
    trait_options_source(Source, Id, ToTrait, Spec),
    call(Source).
options(Source, Id, Spec) :-
    trait_options(Source, Id, _, Spec).
lookup_option_doc(Source, Id, Option, Doc) :-
    trait_options(Source, Id, ToTrait, _),
    call(ToTrait, Option, Trait),
    (Trait ?= Doc).

% Don't display traits that have already been picked as options to the
% user (see inspect_options/3).
hide_base_option(Source, Id, Option) :-
    trait_options(Source, Id, ToTrait, _),
    call(ToTrait, Option, Trait),
    trait(Trait).

%! class_trait(?Class:atomic, ?Origin, ?Trait)
%
%  Query your character's traits that originate from Class.
class_trait(Class, Origin, Trait) :-
    trait(Origin, Trait),
    class_origin_to_class(Origin, Class).

%! choice_member_to_trait(Source, Id, ToTrait)
%
%  Every clause of this predicate declares that some choice/3 clause
%  should give rise to (a) corresponding trait(s).
%  If choice_member(Source, Id, Choice) is true, then this predicate
%  will make sure trait(choice(Source, Id), X) is true if call(ToTrait,
%  Choice, X) is true.
choice_member_to_trait(Source, Id, ToTrait) :-
    trait_options(Source, Id, ToTrait, _).
wrap(Functor, X, FunctorX) :- FunctorX =.. [Functor, X].
deep_wrap(NestedFunctors, X, NestedFunctorsX) :-
    NestedFunctors =.. [Functor, SubNestedFunctors],
    !,
    deep_wrap(SubNestedFunctors, X, SubX),
    NestedFunctorsX =.. [Functor, SubX].
deep_wrap(Atom, X, AtomX) :-
    Atom =.. [Atom],
    !,
    wrap(Atom, X, AtomX).

%! notable_trait(?Origin, ?Trait)
%
%  Somewhat arbitrarily, a trait is "notable" when it's not an
%  expertise, skill, language, tool, weapon, armor or shield proficiency.
notable_trait(Origin, Trait) :-
    trait(Origin, Trait),
    \+ member(Trait,
              [expertise(_), skill(_), language(_), tool(_),
               weapon(_), armor(_), shield, saving_throw(_)]).

notable_traits :-
    forall(notable_trait(_,T), writeln_quoted_term(T)).

%! notable_traits_by_category(?TraitsPerCat)
%
%  List "notable traits" (see notable_trait/2), arranged per category.
notable_traits_by_category(TraitsPerCat) :-
    findall(Cat-Trait,
            (notable_trait(Origin,Trait),
             origin_category_or_uncategorized(Cat,Origin)),
            CatTraits),
    sort(1, @=<, CatTraits, Sorted),
    group_pairs_by_key(Sorted, TraitsPerCat).
    

%! traits_by_category(?TraitsPerCat)
traits_by_category(TraitsPerCat) :-
    findall(Cat-Trait,
            (trait(Origin,Trait), origin_category_or_uncategorized(Cat,Origin)),
            CatTraits),
    sort(1, @=<, CatTraits, Sorted),
    group_pairs_by_key(Sorted, TraitsPerCat).

%! trait_from_level_reached(+Level:int, ?Origin, ?Trait)
%
%  True iff Trait was gained at the given Level automatically upon
%  reaching that level (that is, at that level no active choice was
%  required).
trait_from_level_reached(Level, Origin, Trait) :-
    trait(Origin, Trait),
    Origin \= choice(_,_),
    origin_level(Origin, Level).
%:- table bonus/2 as incremental.

%! bonus(?Source, ?Bonus)
%
%  Bonuses are properties of your character that typically stack.
%  For example, if your character is an elf,
%  bonus(race(elf), dex+2) would be true.
%  This bonus would stack with other improvements to your dexterity.
%  For most properties that don't stack, such as proficiencies or
%  class features, see trait/2.
%
%  List of valid Bonus terms (should try to keep this exhaustive):
%  - `Abi+N`, where ability(Abi) and integer(N). For example `int+1`,
%    `str+2`, ...
%  - `modify_spell(Origin, Name, Mod)` where
%    `known_spell(Origin,Name,_,_,_,_)`.
%    `Mod` is a binary goal such that `call(Mod,
%    OldData, NewData)` produces the new spell data from the old spell
%    data.
bonus(Source, Bonus) :-
    bonus_source(Source, Bonus),
    call(Source).
bonus(choice(Source, Id), Bonus) :-
    choice_member(Source, Id, Choice),
    choice_member_to_bonus(Source, Id, Goal),
    call(Goal, Choice, Bonus).

%! bonus(Bonus)
%
%  Shorthand for bonus/2, when you're not interested in the source.
bonus(Bonus) :- bonus(_, Bonus).

%! bonus_source(?Source, ?Bonus)
%
%  Each bonus_source/2 clause gives rise to a corresponding
%  bonus/2 clause, *if* call(Source) is true.
bonus_source(Source, Bonus) :-
    bonuses_from_source(Source, Bonuses),
    member(Bonus, Bonuses).

%! bonuses_from_source(?Source, ?Bonuses)
%
%  Equivalent to asserting a bonus_source(Source, Bonus) clause for
%  each member(Bonus, Bonuses).
bonuses_from_source(_,_) :- false.

%! bonus_options_source(?Source, ?Id, ?ToBonus, ?Spec)
%  
%  Each bonus_options_source/4 clause gives rise to a corresponding
%  bonus_options/4 clause, *if* call(Source) is true.
bonus_options_source(_,_,_,_) :- false.

%! bonus_options(?Source, ?Id, ?Spec, ?ToBonus)
%
%  Each clause gives rise to a corresponding options/3 clause,
%  as well as a corresponding choice_member_to_bonus/3 clause.
bonus_options(Source, Id, ToBonus, Spec) :-
    bonus_options_source(Source, Id, ToBonus, Spec),
    call(Source).
options(Source, Id, Spec) :-
    bonus_options(Source, Id, _, Spec).

%! sum_bonuses(++Stat, ?Total:int)
%
%  Sum up all the bonuses that affect Stat.
sum_bonuses(Stat, Total) :-
    ground(Stat),
    findall(Bon, bonus(Stat + Bon), Bonuses),
    sumlist(Bonuses, Total).

%! choice_member_to_bonus(Source, Id, ToBonus)
%
%  Every clause of this predicate declares that some choice/3 clause
%  should give rise to (a) corresponding bonus(es).
%  If choice_member(Source, Id, Choice) is true, then this predicate
%  will make sure bonus(choice(Source, Id), X) is true if call(ToBonus,
%  Choice, X) is true.
choice_member_to_bonus(Source, Id, ToBonus) :-
    bonus_options(Source, Id, ToBonus, _).

id(X,X).

%! bonus_from_level_reached(Level:int, ?Origin, ?Trait)
%
%  True iff Bonus was gained at the given Level automatically upon
%  reaching that level (that is, at that level no active choice was
%  required).
bonus_from_level_reached(Level, Origin, Bonus) :-
    bonus(Origin, Bonus),
    Origin \= choice(_,_),
    origin_level(Origin, Level).
add_bonus_to_first_damage_roll([damage(Type,Formula   )|Rest], Bonus,
                               [damage(Type,NewFormula)|Rest]) :-
    simplify_dice_sum(Formula + Bonus, NewFormula).
language(common).
language(dwarvish).
language(elvish).
language(giant).
language(gnomish).
language(goblin).
language(halfling).
language(orc).
language(language ).
language(abyssal ).
language(celestial).
language(draconic).
language('deep speech').
language(infernal).
language(primordial).
language(sylvan).
language(undercommon).

meta_todo(languages, "add all the languages").
% TODO: documentation and cleanup
%! roll_avg(?Roll, -Avg:int)
%
%  The average value of a die, rounded up.
roll_avg(X d Y, Avg) :- Avg is ceiling(X * (Y+1) / 2).

%! roll_max(?Roll, -Max:int)
%
%  The maximum value of a die roll.
roll_max(X d Y, Max) :- Max is X*Y.

%! simplify_dice_sum(?Sum, ?Simplified)
%
%  Simplify a sum of dice. For example, `1 d 4 + 3 + 2 d 4 + 1` is
%  simplified to `3 d 4 + 4`.
%  Makes sure the dice are ordered in descending number of eyes, with
%  the constant term at the very end.
simplify_dice_sum(Sum, Simplified) :-
    sum_to_list(Sum, List),
    add_up_dice(List, SumList),
    sort(2, @<, SumList, SumListSorted),
    list_to_sum(SumListSorted, Simplified).

%! normalize_dice_formula(?Sum, ?Normalized)
%
%  Normalized is a sum of dice equivalent to Sum, but always ending in
%  `+0`. If Sum already ends in `+0`, then Normalized = Sum,
%  otherwise, Normalized is the same Sum but with an addition `+0`
%  term.
normalize_dice_formula(Dice + N, Dice + N) :-
    number(N),
    N \= 0,
    !.
normalize_dice_formula(Dice , Dice + 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal representations for this file.
sum_to_list(X    , [DX]      ) :-
    X \= _ + _,
    normalize_dice_sum_term(X,DX).
sum_to_list(X + Y, [DY|List]) :-
    normalize_dice_sum_term(Y,DY),
    sum_to_list(X, List).

normalize_dice_sum_term(N d X, N d X) :- X \= 1.
normalize_dice_sum_term(N    , N d 1) :- number(N).
add_up_dice([0 d _ | Ds], Dice) :-
    !,
    add_up_dice(Ds, Dice).
add_up_dice([D|Ds], Dice) :-
    add_up_dice(Ds, RestDice),
    add_die_to_list(D, RestDice, Dice).
add_up_dice([], []).
add_die_to_list(N d X, OldList, NewList) :-
    append(L1, [M d X|L2], OldList),
    O is N + M,
    append(L1, [O d X|L2], NewList).
add_die_to_list(N d X, List, [N d X | List]) :-
    \+ member(_ d X, List).

list_to_sum([], 0).
list_to_sum([DX], X) :-
    normalize_dice_sum_term(X, DX).
list_to_sum([DX|Xs], Sum + X) :-
    Xs = [_|_],
    list_to_sum(Xs, Sum),
    normalize_dice_sum_term(X, DX).

strictly_worse(N d M1, N d M2) :- M1 < M2.
strictly_worse(N1 d M, N2 d M) :- N1 < N2.
%:- [resources/spells/srd].

%! spell_property(?Name:atomic, ?Field:atomic, ?Val)
%
%  Retrieve a specific Field from the spell_data/2 of the spell Name.
spell_property(Name, Field, Val) :-
    spell_data(Name, Data),
    Val = Data.get(Field).

%! spell_property_or_error(?Name:atomic, ?Prop:atomic, ?Val)
%
%  Like spell_property/3, but when the given field does not exist, it
%  throws an error instead of silently failing.
spell_property_or_error(Name, Prop, Val) :-
    spell_data(Name, Data),
    Val = Data.Prop.

%! spell_data(?Name:atomic, ?Data:dict)
%
%  Data is a dictionary with the following fields (optional fields in parens):
%  * casting_time: A string describing the time required to cast the spell.
%  * classes: List of classes (atoms) that can learn the spell.
%  * components: Subset of the set [`v`, `s`, `m(X)`], indicating
%      which components are required.
%  * desc: Textual description.
%  * duration: String describing the amount of time the effect lasts.
%  * higher_level: Atom `yes` or `no`, indicating whether the spell can be upcast.
%  * level: Spell level, as an integer.
%  * range: Description of the range of a spell. Some options are
%      `self`, `touch`, `feet(X)`, `miles(X)`.
%  * ritual: `yes` or `no`, depending on whether the spell can be cast as a ritual.
%  * school: The spell's school (as an atom).
%  * (damage_rolls): Information about the damage rolls performed by this spell.
%      The damage information is recorded as terms of the form `Cond: Type(Roll)`
%      where `Cond` is a condition that needs to be met to apply the roll, for
%      example `on_hit` (if the damage is done if a spell attack roll
%      hits), `Type` is the damage type (piercing, bludgeoning, fire,
%      ...), and `Roll` is a dice formula.
%  * (effects): Summary of effects, but not damage rolls.
spell_data(Name, Data) :-
    spell_auto_data(Name, AutoData),
    findall(Ext,
            (extend_spell_data(Name, Field, Val), Ext=add_dict_field(Field:Val)),
            Exts),
    sequence(Exts, AutoData, Data).
add_dict_field(Field:Val, Old, New) :-
    New = Old.put(Field,Val).

%! spell_effect(?Spell, ?Effect)
spell_effect(Spell, AutoEffect) :-
    autoderived_spell_effect(Spell, AutoEffect),
    \+ suppress_autoderived_spell_effect(Spell).
spell_effect(Spell, Effect) :-
    add_spell_effect(Spell, Effect).

%spell_effects(Spell, Effects) :-
%    suppress_autoderived_spell_effect(Spell),
%    findall(E, add_spell_effect(Spell, E), Effects).
%spell_effects(Spell, [AutoEffect|AddedEffects]) :-
%    spell_auto_data(Spell, _),
%    \+ suppress_autoderived_spell_effect(Spell),
%    autoderived_spell_effect(Spell, AutoEffect),
%    findall(E, add_spell_effect(Spell, E), AddedEffects).
    

%! autoderived_spell_effect(?Spell, ?Effect)
%
%  For each Spell, Effect represents a best-effort attempt to derive a
%  structured spell description from the JSON sources. It's not
%  guaranteed to be correct and can be suppressed by
%  suppress_autoderived_spell_effect/1.
autoderived_spell_effect(Spell, Effect) :-
    spell_auto_data(Spell, Data),
    spell_base_damage_formula(Spell, Damage),
    findall(Field-Value,
            (member(Field, [area_of_effect, dc, attack_type]),
             Data.get(Field) = Value,
             Value \= false),
            Aspects_),
    append(Aspects_, [damage - Damage], Aspects),
    process_spell_aspects(Aspects, Effect).
process_spell_aspects([area_of_effect - (Range ft Shape) | Aspects], in(Range ft Shape) : Effect) :-
    process_spell_aspects(Aspects, Effect).
process_spell_aspects([dc - (Abi else Success) | Aspects], saving_throw(Abi) : EffectExpr) :-
    (Success = none -> EffectExpr = Effect ; EffectExpr = (Effect else Success)),
    process_spell_aspects(Aspects, Effect).
process_spell_aspects([attack_type - Type | Aspects], spell_attack_roll(Type) : Effect) :-
    process_spell_aspects(Aspects, Effect).
process_spell_aspects([damage - Damage], Damage).

%! spell_base_damage_formula(?Spell, ?Damage)
%
%  Damage is a term `damage(Type, Formula)` that represents the damage
%  done by the Spell at the character's current level, without
%  upcasting or any character-specific bonuses (other than character
%  level).
spell_base_damage_formula(Spell, damage(Type, N d D)) :-
    spell_auto_property(Spell, damage_with_cantrip_scaling, damage(Type, _ d D)),
    cantrip_scale(N).
spell_base_damage_formula(Spell, Damage) :-
    spell_auto_property(Spell, damage_at_slot_level, Dict),
    dict_pairs(Dict, _, [_-Damage|_]).

%! spell_auto_data(?Name:atomic, ?Data:dict)
%
%  Like spell_data/2, but less complete. This is the data that is
%  autogenerated from a JSON file, whereas spell_data/2 contains
%  further manual extensions. You probably want spell_data/2.

%! contains_attack_roll(+Effects, ?Roll)
%
%  Check whether Effects contains at least one attack roll.
contains_attack_roll(Effects, spell_attack_roll(Range):Effect) :-
    subterm_member(spell_attack_roll(Range):Effect, Effects).

%! contains_saving_throw(+Effects, ?ST)
%
%  Check whether Effects contains at least one saving throw.
contains_saving_throw(Effects, saving_throw(Abi):Effect) :-
    subterm_member(saving_throw(Abi):Effect, Effects).

%! unique_effects(+Effects, ?UniqueEffects)
%
%  Effects may contain terms of the form `N * Effect`, where `N` is a
%  natural number. For each such term in Effects where `N=1`,
%  UniqueEffects contains simply Effect.
unique_effects(Effects, UniqueEffects) :-
    maplist(just_once, Effects, UniqueEffects).
just_once(_*X, X).
just_once(X, X) :- X \= _*_.
    
%! extend_spell_data(+Name:atomic, ?Field:atomic, ?Val)
extend_spell_data(Name, effects, Effects) :-
    spell_auto_data(Name, _), % ground Name
    findall(Effect, spell_effect(Name, Effect), Effects).

%! known_spell_effect(?Origin, ?Name:atomic, ?Effect)
%
%  Like add_spell_effect/2, but only gets applied to spells your
%  character knows, typically because some class-specific data is
%  needed to generate the effect summary for the spell. An example of
%  this is counterspell, where in some cases the caster needs to roll
%  an ability check with their own spellcasting ability. Filling in
%  this spellcasting ability requires knowing as which class the spell
%  has been learned.
known_spell_effect(_,_,_) :- false.

known_spell_effect(Origin, counterspell, Effect) :-
    known_spell(Origin, Ability, _, _, _, counterspell),
    atomics_to_string(
        ["target's spell fails if spell level not greater than 3, or if you pass a DC [10 + spell level] ",
         Ability,
         " check"],
        Effect).

%! add_spell_effect(?Name:atomic, ?Effect)
%
%  Add an entry to the `effects` field of the spell data.
add_spell_effect('acid splash', "up to two targets within 5 ft of eachother").

add_spell_effect(bless, "bless up to 3 creatures").
add_spell_effect(bless, "+d4 to attack rolls / saving throws").

add_spell_effect(command, "command creature to approach, drop, flee, grovel, or halt").

add_spell_effect('cure wounds', heal(1 d 8 + mod)).

add_spell_effect(darkvision, "grant 60 ft darkvision").

add_spell_effect('detect magic', "sense presence of magic within 30 ft").

suppress_autoderived_spell_effect('eldritch blast').
add_spell_effect('eldritch blast', Effect) :-
    cantrip_scale(N),
    simplify_product(N*(spell_attack_roll(ranged):damage(force, 1 d 10)), Effect),
    !.

add_spell_effect('false life', 'temp hp' + (1 d 4 + 4)).

add_spell_effect('find familiar', test).

add_spell_effect(frostbite,
                 saving_throw(con):damage(cold,N d 6)) :-
    cantrip_scale(N).

add_spell_effect(guidance, "+d4 to one ability check").

add_spell_effect('healing word', heal(1 d 4 + mod)).

bonus_source(known_spell(_,'mage armor'), ac_formula(13 + dex + shield)).

add_spell_effect('mass cure wounds', heal(3 d 8 + mod) upto "6 creatures").

add_spell_effect('misty step', "teleport 30 ft").

add_spell_effect('prayer of healing', heal(2 d 8 + mod) upto "6 creatures").

%extend_spell_data('scorching ray', damage rolls, [on_hit: fire(2 d 6)]).
suppress_autoderived_spell_effect('scorching ray').
add_spell_effect('scorching ray', 3 * ADEffect) :-
    autoderived_spell_effect('scorching ray', ADEffect).

add_spell_effect('see invisibility',
                 "see invisible creatures and objects, see through Ethereal").

attack_variant(Name:shillelagh, Range, to_hit(ToHit), [damage(bludgeoning, 1 d 8 + Mod)], [magical]) :-
    (Name = quarterstaff ; Name = club),
    attack(Name, Range, _, _, _),
    known_spell(_, shillelagh),
    ability_mod(wis, Mod),
    proficiency_bonus(ProfBon),
    ToHit is Mod + ProfBon.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SHORTENED DESCRIPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
spell_short_desc(
    'prismatic wall',
    ["Creates a multicolored, opaque wall up to 90 feet long, 30 feet high, and 1 inch thick, or a spherical wall up to 30 feet in diameter.",
     "If the wall passes through a creature's space, the spell fails. Creatures designated by the caster can pass through without harm. Creatures within 20 feet of the wall at the start of their turn must make a Constitution saving throw or be blinded for 1 minute.",
     "The wall sheds bright light for 100 feet and dim light for an additional 100 feet.",
     "Consists of seven layers, each with a different color.",
     "Red: Deals 10d6 fire damage. Nonmagical ranged attacks can't pass through.",
     "Orange: Deals 10d6 acid damage. Magical ranged attacks can't pass through. Destroyed by strong wind.",
     "Yellow: Deals 10d6 lightning damage. Can be destroyed by dealing 60 force damage.",
     "Green: Deals 10d6 poison damage. Can be destroyed by a passwall spell or similar.",
     "Blue: Deals 10d6 cold damage. Can be destroyed by dealing 25 fire damage.",
     "Indigo: Restrains creatures; requires saving throws to avoid petrification. Spells can't pass through. Destroyed by bright light (e.g., daylight spell).",
     "Violet: Blinds creatures, transports them to another plane on failed saves. Destroyed by dispel magic or similar spells.",
     "Layers can be destroyed one at a time, from red to violet, by specific means outlined for each layer. Rod of cancellation destroys the entire wall. Antimagic field has no effect on the wall."
    ]).

spell_short_desc(
    symbol,
    [ "The spell allows you to inscribe a harmful glyph on a surface or within a closed object. The glyph is nearly invisible and can be triggered by various means that you decide upon casting. If the trigger occurs, the glyph activates, filling a 60-foot-radius sphere with dim light for 10 minutes. Creatures in the sphere during activation or entering it are affected based on the chosen glyph effect:",
      "Death: Targets make a Constitution saving throw, taking 10d10 necrotic damage on a failed save, or half as much on a successful save.",
      "Discord: Targets make a Constitution saving throw. On a failed save, they bicker and argue for 1 minute, with disadvantage on attack rolls and ability checks.",
      "Fear: Targets make a Wisdom saving throw. On a failed save, they become frightened for 1 minute, dropping held items and moving away from the glyph.",
      "Hopelessness: Targets make a Charisma saving throw. On a failed save, they are overwhelmed with despair for 1 minute, unable to attack or target creatures with harmful effects.",
      "Insanity: Targets make an Intelligence saving throw. On a failed save, they are insane for 1 minute, unable to take actions, understand speech, read, and move erratically under DM control.",
      "Pain: Targets make a Constitution saving throw. On a failed save, they are incapacitated with excruciating pain for 1 minute.",
      "Sleep: Targets make a Wisdom saving throw. On a failed save, they fall unconscious for 10 minutes, waking up if damaged or shaken awake.",
      "Stunning: Targets make a Wisdom saving throw. On a failed save, they become stunned for 1 minute."
    ]).

spell_short_desc(
    teleport,
    [ "This spell instantly transports you and up to eight willing creatures or a single object to a selected destination within range on the same plane of existence.",

      "| Familiarity | Mishap | Similar Area | Off Target | On Target |\n |-------------|--------|--------------|------------|-----------|\n | Permanent Circle | - | - | - | 01-100 |\n | Associated Object | - | - | - | 01-100 |\n | Very Familiar | 01-05 | 06-13 | 14-24 | 25-100 |\n | Seen Casually | 01-33 | 34-43 | 44-53 | 54-100 |\n | Viewed Once | 01-43 | 44-53 | 54-73 | 74-100 |\n | Description | 01-43 | 44-53 | 54-73 | 74-100 |\n | False Destination | 01-50 | 51-100 | - | - |",

      "**Outcomes**",
      "- **On Target:** Successful teleportation to the desired destination.",
      "- **Off Target:** Random distance (1d10 x 1d10 % of travelled distance) away from the destination in a random direction.",
      "- **Similar Area:** Arrival in an area visually or thematically similar to the target.",
      "- **Mishap:** Unpredictable magic causes a difficult journey, dealing 3d10 force damage, and DM rerolls on the table."
    ]
).

spell_short_desc(
    imprisonment,
    [ "Target must succeed on a Wisdom saving throw or be bound by the spell. If it succeeds, it is immune to this spell if you cast it again. While affected by this spell, target doesn't need to breathe, eat, or drink, and doesn't age. Divination spells can't locate or perceive the target.",
      "Choose a form of imprisonment:",
      "Burial: Force sphere far beneath the earth. Nothing can pass through; no one can teleport / planar travel into or out of the sphere. Component: small mithral orb.",
      "Chaining: Target is restrained, and it can't move or be moved. Component: fine chain of precious metal.",
      "Hedged Prison: Tiny demiplane that is warded against teleportation and planar travel. Component: miniature representation of the prison made from jade.",
      "Minimus Containment: Target shrinks to a height of 1 inch and is imprisoned inside a gemstone or similar object. Light can pass through the gemstone normally (allowing the target to see out and other creatures to see in), but nothing else can pass through, even by means of teleportation or planar travel. The gemstone can't be cut or broken while the spell remains in effect. Component: transparent gemstone.",
      "Slumber: Target falls asleep and can't be awoken. Component: rare soporific herbs.",
      "Ending the Spell: While casting spell, you can specify a condition that ends the spell.",
      "A dispel magic spell can end the spell only if it is cast as a 9th-level spell, targeting either the prison or the special component used to create it.",
      "You can use a particular special component to create only one prison at a time. If you cast the spell again using the same component, the target of the first casting is immediately freed from its binding."
    ]
).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO copied from srd.pl
spell_auto_property(Spell, Field, Value) :-
    spell_auto_data(Spell, Data),
    Data.get(Field) = Value,
    Value \= false.

spell_data_class(Dict, Class) :-
    to_lowercase_atom(Dict.index, Class).

spell_data_higher_level([], no).
spell_data_higher_level([Desc], Desc).

spell_data_components(Data, Components) :-
    maplist(spell_data_component(Data), Data.components, Components).

spell_data_component(Data, "M", m(Data.material)) :- !.
spell_data_component(_, Component, Atom) :-
    to_lowercase_atom(Component, Atom).

to_lowercase_atom(Str, Atom) :-
    string_lower(Str, Lower),
    string_to_atom(Lower, Atom).

yesno(true, yes).
yesno(false, no).

parse_range("Self", self) :- !.
parse_range("Touch", touch) :- !.
parse_range(Str, feet(Feet)) :-
    member(Suffix, [" feet", " foot"]),
    string_concat(FeetStr, Suffix, Str),
    number_string(Feet, FeetStr),
    !.
parse_range(Str, miles(Miles)) :-
    member(Suffix, [" mile", " miles"]),
    string_concat(MilesStr, Suffix, Str),
    number_string(Miles, MilesStr),
    !.
parse_range(Str, Atom) :-
    to_lowercase_atom(Str, Atom).

wrap_list(List, List) :- is_list(List), !.
wrap_list(X, [X]) :- \+ is_list(X).

spell_data_damage_with_cantrip_scaling(Data, damage(Type, BaseRoll)) :-
    Data.get(damage) = _{ damage_at_character_level: DmgScalingDict, 
                          damage_type: DmgType },
    to_lowercase_atom(DmgType.get(name), Type),
    term_string(BaseRoll, DmgScalingDict.get('1')),
    !.
spell_data_damage_with_cantrip_scaling(_, false) :- !.

spell_data_damage_at_slot_level(Data, ParsedDict) :-
    wrap_list(Data.get(damage), DamageDicts),
    maplist(damage_at_slot_level_term, DamageDicts, Terms),
    merge_damage_dicts(Terms, ParsedDict),
    !.
spell_data_damage_at_slot_level(_, []).

damage_at_slot_level_term(_{ damage_type: TypeDict,
                             damage_at_slot_level: ScalingDict
                           },
                          ParsedDict) :-
    to_lowercase_atom(TypeDict.get(name), Type),
    dict_pairs(ScalingDict, _, Pairs),
    findall(Lvl-damage(Type, Roll),
            (member(LvlAtom-RollStr,Pairs), atom_number(LvlAtom,Lvl), term_string(Roll,RollStr,[variable_names([])])), % TODO: parse and handle "+ MOD"
            NewPairs),
    dict_pairs(ParsedDict, _, NewPairs).

merge_damage_dicts([D|Ds], Out) :-
    merge_damage_dicts(Ds, DRest),
    merge_damage_dicts(D, DRest, Out).
merge_damage_dicts([D], D).
merge_damage_dicts(D1, D2, Out) :-
    dict_pairs(D1, _, Pairs1), dict_pairs(D2, _, Pairs2),
    merge_damage_lists(Pairs1, Pairs2, NewPairs),
    dict_pairs(Out, _, NewPairs).
merge_damage_lists([L-Dmg1|R1], [L-Dmg2|R2], [L-(Dmg1+Dmg2)|R]) :-
    merge_damage_lists(R1, R2, R).
merge_damage_lists([], [], []).

% in(20 ft sphere):
spell_data_aoe(Data, Size ft Type) :-
    Data.get(area_of_effect) = _{type: TypeStr, size: Size},
    !,
    string_to_atom(TypeStr, Type).
spell_data_aoe(_, false).

spell_data_dc(Data, Abi else Succ) :-
    Data.get(dc) = DCDict,
    !,
    DCDict.get(dc_type).get(index) = AbiStr,
    string_to_atom(AbiStr, Abi),
    DCDict.get(dc_success) = SuccStr,
    string_to_atom(SuccStr, Succ).
spell_data_dc(_, false).

spell_data_attack_type(Data, Type) :-
    Data.get(attack_type) = Str,
    string_to_atom(Str, Type). 
spell_data_attack_type(_, false).
origin_category(init, init).
origin_category(class(Class), Origin) :-
    class_option(Class),
    class_origin_to_class(Origin, Class).
origin_category(race(Race), race(Race)) :-
    Race =.. [_].
origin_category(race(BaseRace), race(Race)) :-
    Race =.. [BaseRace, _].
origin_category(background(BG), background(BG)).
origin_category(feat(F), feat(F)).
origin_category(Category, trait(Trait)) :-
    trait(Origin, Trait),
    origin_category(Category, Origin).
origin_category(Category, choice(Origin, _)) :-
    origin_category(Category, Origin).

origin_category_canonical_order(init, 0) :- !.
origin_category_canonical_order(race(_), 1) :- !.
origin_category_canonical_order(background(_), 2) :- !.
origin_category_canonical_order(class(_), 3) :- !.
origin_category_canonical_order(feat(_), 4) :- !.
origin_category_canonical_order(_, 5) :- !.

meta_todo(origin_category, "probably duplication with find_origin_class in class.pl").

%! origin_category_or_uncategorized(Origin, Category)
%
%  Like origin_category/2, but instead of failing if no category can
%  be found, succeed with the category "uncategorized".
origin_category_or_uncategorized(Category, Origin) :-
    origin_category(Category, Origin),
    !.
origin_category_or_uncategorized(uncategorized, _).

%! origin_level(?Origin, ?Level)
%
%  Determine what Level your character obtained a given Origin qualification.
origin_level(init, 1) :- !.
origin_level(^_, 1) :- !.
origin_level(initial_class(_), 1) :- !.
% TODO delete
origin_level(class(C), Level) :-
    !,
    reached_classlevel_at_charlevel(C:1, Level).
origin_level(match_class(C:L), Level) :-
    % TODO delete
    !,
    reached_classlevel_at_charlevel(C:L, Level).
origin_level(match_class(C), Level) :-
    % TODO delete
    C \= _:_,
    !,
    reached_classlevel_at_charlevel(C:1, Level).
origin_level(Class >: ClassLevel, Level) :-
    !,
    origin_level(match_class(Class:ClassLevel), Level).
origin_level(race(_), 1) :- !.
origin_level(background(_), 1) :- !.
origin_level(trait(Trait), TraitOriginLevel) :-
    !,
    trait(TraitOrigin, Trait),
    origin_level(TraitOrigin, TraitOriginLevel).
origin_level(feat(Feat), FeatOriginLevel) :-
    !,
    trait(TraitOrigin, feat(Feat)),
    origin_level(TraitOrigin, FeatOriginLevel).
origin_level(choice(ChoiceOrigin, _), ChoiceOriginLevel) :-
    !,
    origin_level(ChoiceOrigin, ChoiceOriginLevel).
origin_level(_ at Level, Level) :- !.
origin_level(_, unknown) :- !.


%! origin(?Origin)
%
%  Origin is a possible origin of a trait/bonus your character may have.
origin(Origin) :- trait_source(Origin, _).
origin(Origin) :- options_source(Origin, _, _).
origin(Origin) :- bonus_source(Origin, _).
%:- use_module(library(pldoc)).
:- use_module(library(yall)).
:- use_module(library(pairs)).
:- use_module(library(pure_input)).

%:- doc_server(4000).
%:- set_prolog_flag(toplevel_print_anon, false).
:- portray_text(true).



%! meta_todo(Source, Todo)
%
%  Predicate for helping find incomplete code.
%  For instance, we have declared a class_option(Class),
%  but there is no correspoinding hd_per_level(Class, HD),
%  then we should generate a meta_todo/2.
meta_todo(_,_) :- false.

%! content_source(Content, Source).
content_source(_,_) :- false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO!!
%learnable_cantrip(wizard, S) :- learnable_spell(wizard, S).
%options(class(warlock:3), 'pact boon', from_list([tome, blade, chain])).
%options(class(wizard:_), spell, 2 unique_from learnable_spell(wizard)).
%
%choice(class(warlock:3), 'pact boon', tome).
%choice(class(wizard:1), spell, ['fire bolt', prestidigitation]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Spells.

%options_source(class(sorcerer:_), spell, learnable_spell(sorcerer)).
%options_source(class(sorcerer:N), replace_spell, spell_known_at_class_level(sorcerer:Prev)) :-
%    between(2, 20, N),
%    Prev is N-1.
%%options_source(choice())
%
%known_spell_at_class_level(Class:Level, Name) :-
%    choice_member(class(Class:Level), spell, Name).
%known_spell_at_class_level(Class:Level, Name) :-
%    between(2, 20, Level),
%    PrevLevel is Level-1,
%    known_spell_at_class_level(Class:PrevLevel, Name),
%    \+ choice(class(Class:Level), replace_spell, Name).
%
%spell_prop(Name, Prop, Val) :-
%    spell(Name, Data),
%    Val = Data.Prop.
%known_spell_prop(Name, Prop, Val) :-
%    known_spell(Name, Data, _, _, _),
%    Val = Data.Prop.
%
%learn_spell(Class, Name, SpellData, always, [slot]) :-
%    current_class_level(Class:Level),
%    known_spell_at_class_level(Class:Level, Name),
%    spell(Name, SpellData).
%    
%known_spell(Source, Name, SpellData, Availability, Resources) :-
%    learn_spell(Source, Name, GenericSpellData, Availability, Resources),
%    findall(Mod, commutative_spell_modifier(Source, Name, Mod), Mods),
%    sequence(Mods, GenericSpellData, SpellData).
%
%commutative_spell_modifier(_,_,_) :- false.
%current_class_level(_) :- false.
%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Leveling up.
level(Level) :-
    findall(L, gain_level(L, _, _), Levels),
    max_member(Level, Levels).
level(1) :-
    \+ gain_level(_,_,_).

match_level(Level) :-
    level(CurLevel),
    between(1, CurLevel, Level).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Character initialization.

% Pick your initial class.
options(init, 'initial class', class_option).
initial_class(Class) :- choice(init, 'initial class', Class), !.
problem(multiple_initial_classes(Classes)) :-
    findall(Class, initial_class(Class), Classes),
    Classes = [_,_|_].

% Pick character background.
options(init, background, background_option).
background(BG) :- choice(init, background, BG).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% To organize:
initiative(Init) :-
    ability_mod(dex, DexMod),
    sum_bonuses(init, Bonus),
    Init is DexMod + Bonus.

%! hit_dice(?HD)
hit_dice(HD) :-
    findall(CHD, hit_dice(_, CHD), CHDs),
    list_to_sum(CHDs, Sum),
    simplify_dice_sum(Sum, HD).
%! hit_dice(?Class:atomic, ?HD)
hit_dice(Class, M d X) :-
    class_level(Class:Level),
    hd_per_level(Class, N d X),
    M is N * Level.

%! speed(?Speed:int)
speed(Speed) :-
    race(Race),
    racial_speed(Race, BaseSpeed),
    findall(Bonus, bonus(speed+Bonus), Bonuses),
    sumlist([BaseSpeed|Bonuses], Speed).
unarmored_speed_bonus(TotalBonus) :-
    findall(Bonus, bonus('unarmored speed'+Bonus), Bonuses),
    sumlist([Bonuses], TotalBonus).

%! proficiency_bonus(?Bonus:int)
proficiency_bonus(Bonus) :- level(Level), calc_bonus(Level, Bonus).
calc_bonus(Level, Bonus) :- Bonus is 2 + div(Level-1, 4).

%! passive_perception(?PP:int)
passive_perception(PP) :-
    skill(perception, Perception),
    PP is 10 + Perception.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

name(_) :- false.
problem('pick a name!') :- \+ name(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Resources. TODO: need to rethink resources
% * needs new name (clashes with builtin)
% * good hard think about arcane recovery needed: it can restore a
%   finite resource in a short rest, but only once per long rest, and
%   you get to pick which slots get restored.

%! resource(?Origin, ?Name, ?Max)
%
%  Name is a finite resource your character has, such as sorcery
%  points, second wind, ...
resource(_,_,_) :- false.

%! resource(?Name, ?Max)
%
%  Shorthand for resource/3, for when you're not interested in the
%  origin of a resource.
resource(Name, Max) :- resource(_, Name, Max).

%! on_rest(?Duration, ?ResourceName, ?Goal)
%
%  Documents the effects of long and short rests on resources.
%  Duration is either `short` or `long`.
%  ResourceName is the name of a resource such that resource(_,
%  ResourceName, _) is true.
%  Goal is a trinary predicate such that call(Goal, Max, Cur, New) is true when
%  Cur is the current value of a resource before resting (TODO:
%  current values aren't implemented right now), and New is the value
%  after resting.
on_rest(_,_,_) :- false.
meta_todo(resource(Resource), 'rest for nonexistant resource') :-
    on_rest(_, Resource, _),
    \+ resource(_, Resource, _).

%! full_restore(?Max, ?Cur, ?New)
%
%  Helper predicate to fully restore a resource. Usually used as third
%  argument to on_rest/3.
full_restore(Max, _, Max).

%! restore(?Max, ?Cur, ?New)
%
%  Helper predicate to partially restore a resource. Usually used as third
%  argument to on_rest/3, by partially applying the first argument
%  (for instance, writing restore(4) to restore a resource by 4
%  points).
restore(N, Max, Cur, New) :-
    New is min(Cur+N, Max).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Shorthands.
%search(X) :-
%    atom_to_chars(X, X1),
%    (Topic ?= Desc),
%    Topic =.. []
%    atom_to_chars(Topic, Topic1),
%    append([_, X1, _], Topic1),
%    writeln(Topic),
%    writeln(Desc).

todo :-
    forall(todo(T), writeln_quoted_term(T)).

traits :-
    forall(trait(Origin, Trait), writeln_quoted_term(Origin/Trait)).

bonuses :-
    forall(bonus(Origin, Bonus), writeln_quoted_term(Origin/Bonus)).

abilities :-
    forall(ability(Abi,Score), writeln_quoted_term(Abi:Score)).

spells :-
    forall(known_spell(Origin, Name), writeln_quoted_term(Origin:Name)).

mtodo :-
    forall(meta_todo(S,T), writeln_quoted_term(S->T)).

template :-
    forall(todo(options(Origin, Id, _)),
           (inspect_options(Origin, Id, Opts),
            writeln_quoted_term(choice(Origin, Id, Opts)))).

problems :-
    forall(problem(P), writeln_quoted_term(P)).

writeln_quoted_term(T) :-
    write_term(T, [quoted(true), fullstop(true), nl(true)]).

warn_if_problems :-
    forall(problem(P), (write("WARNING: "), writeln_quoted_term(P))).

describe_spell(Spell) :-
    spell_data(Spell, Data),
    write("* "), writeln(Spell),
    writeln(""),
    write("Casting time:\t"), writeln(Data.casting_time),
    write("Duration:\t"), writeln(Data.duration),
    write("Concentration:\t"), writeln(Data.concentration),
    write("Range:\t\t"), writeln(Data.range),
    write("Components:\t"), writeln(Data.components),
    write("Ritual:\t\t"), writeln(Data.ritual),
    write("School:\t\t"), writeln(Data.school),
    writeln(""),
    writeln(Data.desc),
    writeln(""),
    writeln("").
    
describe_new_learnable_spells(Class:Level) :-
    forall((learnable_proper_spell(Class,Spell),
            spell_property(Spell,level,Level),
            \+ known_spell(Class,Spell)),
           describe_spell(Spell)).

describe_class_cantrips(Class) :-
    forall(class_cantrip(Class, Spell), describe_spell(Spell)).

qq(Pred) :-
    current_predicate(Pred/Arity),
    length(Args, Arity),
    Functor =.. [Pred|Args],
    call(Functor),
    forall(member(Arg,Args), writeln(Arg)).
ac(AC) :- ac(_, AC, _).

ac(Origin, AC, Options) :-
    ac_formula(Origin, Formula),
    sumall(B, bonus(ac + B), GlobalBonus),
    sumall(B, bonus(ac_formula(Origin) + B), SpecificBonus),
    eval_ac_formula(Formula, FormulaAC, Options),
    AC is FormulaAC + GlobalBonus + SpecificBonus.

ac_formula(armor(Armor), AC + Enchantment + shield) :-
    has(Armor),
    expand_to_sum(Armor, BaseArmor + Enchantment),
    body_armor(BaseArmor, heavy, ac(AC)).
ac_formula(armor(Armor), AC + Enchantment + min(dex,2) + shield) :-
    has(Armor),
    expand_to_sum(Armor, BaseArmor + Enchantment),
    body_armor(BaseArmor, medium, ac(AC)).
ac_formula(armor(Armor), AC + Enchantment + dex + shield) :-
    has(Armor),
    expand_to_sum(Armor, BaseArmor + Enchantment),
    body_armor(BaseArmor, light, ac(AC)).
ac_formula(Origin, Formula) :-
    bonus(Origin, ac_formula(Formula)).
ac_formula(unarmored, 10 + dex + shield) :-
    \+ suppress_unarmored_ac_formula.

%! suppress_unarmored_ac_formula
%
%  If true, we don't generate the formula `ac_formula(unarmored, 10 + dex + shield)`.
suppress_unarmored_ac_formula :- false.

%! eval_ac_formula(?Formula, ?AC:int, ?Options)
%
%  Evaluate an AC formula to an integer, and a list of options of the form `Id:Modifier`.
eval_ac_formula(A + B, AC, Options) :-
    eval_ac_formula(A, X, Opts1),
    eval_ac_formula(B, Y, Opts2),
    AC is X + Y,
    append(Opts1, Opts2, Options).
eval_ac_formula(Abi, AC, []) :-
    ability_mod(Abi, AC).
eval_ac_formula(min(F1,F2), AC, Options) :-
    eval_ac_formula(F1, AC1, Opts1),
    eval_ac_formula(F2, AC2, Opts2),
    (AC1 =< AC2 -> (AC = AC1, Options = Opts1) ; (AC = AC2, Options = Opts2)).
eval_ac_formula(Num, Num, []) :-
    number(Num).
eval_ac_formula(shield, 0, [shield(Shield):AC]) :-
    trait(armor(shield)),
    shield_ac(Shield, AC).
eval_ac_formula(shield, 0, []) :-
    \+ (trait(armor(shield)), shield_ac(_, _)).

% TODO user should select a single shield they like to not explode the
% options for calculating AC
shield_ac(shield + N, AC) :- has(shield + N), AC is 2 + N.
shield_ac(shield, 2) :- has(shield).

%! unarmored_defense_formula(?Origin, ?Formula)
%
%  The 'unarmored defense' trait has a specific multiclassing rule:
%  you can only gain this feature once. So if you're multiclass monk/barbarian,
%  you get the unarmored defense formula of whichever class you picked first.
unarmored_defense_formula(Origin, Formula) :-
    findall(OL-O-F,
            (trait(O,unarmored_defense(F)),
             origin_level(O,OL)),
            Olofs),
    sort(1, @<, Olofs, [_ - (Origin>:_) - Formula | _]).
bonus(unarmored_defense(Origin), ac_formula(Formula)) :-
    unarmored_defense_formula(Origin, Formula).

race(Race) :-
    choice(_, 'base race', Race).
race(Race) :-
    choice(race(BaseRace), subrace, Subrace),
    Race =.. [BaseRace, Subrace].

most_specific_race(Race) :-
    race(Race),
    \+ subrace_option(Race, _).

race_has_subraces(Race) :-
    subrace_option(Race, _),
    !.

options(init, 'base race', race_option).
options(race(Race), subrace, subrace_option(Race)) :-
    race(Race),
    race_has_subraces(Race).

racial_speed(_,_) :- false.
race_shorthand(_,_) :- false.

meta_todo(race(Race), 'missing racial speed') :-
    race_option(Race),
    \+ racial_speed(Race, _).

meta_todo(race(Race), 'missing race shorthand') :-
    race_option(Race),
    \+ race_shorthand(Race, _).
% Channel divinity has specific multiclassing rules.
resource('channel divinity', 'channel divinity', Uses) :-
    findall(N, bonus(channel_divinity_uses(N)), Ns),
    max_member(Uses, Ns).
on_rest(short, 'channel divinity', 'full restore').

%! multiclass_trait(?Origin, ?Trait)
%
%  A trait that can originate from multiple classes at once,
%  for which the rules define a specific way to handle overlap or conflicts.
multiclass_trait(Origin, Trait) :-
    multiclass_trait_source(Origin, Trait),
    call(Origin).

%! multiclass_trait_source(?Origin, ?Trait)
%
%  If `call(Origin)` is true, results in a `multiclass_trait(Origin, Traint)`.
multiclass_trait_source(_,_) :- false.

% Extra attack (fighter, barbarian, monk, ...).
trait(Origin, extra_attack(N)) :-
    findall(O-N, multiclass_trait(O, extra_attack(N)), Origins),
    sort(2, @>=, Origins, [Origin-N|_]).
bonus(trait(extra_attack(N)), add_weapon_note(_, Note)) :-
    trait(extra_attack(N)),
    M is N+1,
    atomics_to_string(["attack ", M, "x"], Note).
custom_format(extra_attack(1)) --> ["extra attack"].
custom_format(extra_attack(N)) --> {N \= 1}, [N], [" extra attacks"].

extra_attack(_) ?= "You can attack more than once whenever you take the Attack action on your turn.".
character_definition_predicates(
    [ name/1         ,
      base_ability/2 ,
      gain_level/3   ,
      choice/3       ,
      has/1     ]).

%! write_character_file
%
%  Writes a file containing the facts that define the current
%  character to a file. The file is placed in the `characters` folder
%  and the file name is the character's name, followed by the `.pl`
%  file extension.
write_character_file :-
    name(Name),
    charname_to_filename(Name, FileName),
    open(FileName, write, Out),
    character_definition_predicates(Preds),
    write_predicates(Out, Preds),
    close(Out).

%! write_predicates(+Stream, +List)
%
%  For each `Predicate/Arity` term in List, write all results to the
%  given output stream.
write_predicates(Out, List) :-
    forall(member(Pred,List), write_predicate(Out, Pred)).
write_predicate(Out, Pred/Arity) :-
    length(Args, Arity),
    Goal =.. [Pred|Args],
    forall(Goal,
           write_term(Out, Goal,
                      [quoted(true), fullstop(true), nl(true)])).


%! load_character_file(+CharName)
%
%  Load the facts defining the character CharName from its
%  file. CharName should only be the character name, the corresponding
%  file will be found by looking for the matching `.pl` file in the
%  `characters` folder.
load_character_file(CharName) :-
    unload_current_character,
    charname_to_filename(CharName, FileName),
    load_files(FileName, []).

%! unload_current_character
%
%  Forget everything about the current character.
%  See character_definition_predicates/1 for a list of predicates
%  which will be fully retracted.
unload_current_character :-
    character_definition_predicates(Preds),
    forall(member(Pred,Preds), retractall_pred(Pred)).

initialize_new_character(Name) :-
    unload_current_character,
    assert(name(Name)),
    forall(ability(Abi), assert(base_ability(Abi,10))),
    write_character_file.

retractall_pred(Pred/Arity) :-
    length(Args, Arity),
    Goal =.. [Pred|Args],
    retractall(Goal).

%! saved_character(CharName)
%
%  True iff CharName matches a character file in the `characters`
%  directory.
saved_character(CharName) :-
    directory_member(characters, FileName, [extensions([pl])]),
    charname_to_filename(CharName, FileName).

charname_to_filename(CharName, FileName) :-
    ground(CharName),
    !,
    atom_chars(CharName, CharNameChars),
    cn2fn(CharNameChars, FileNameChars, []),
    atom_chars(FileName, FileNameChars).
charname_to_filename(CharName, FileName) :-
    ground(FileName),
    !,
    atom_chars(FileName, FileNameChars),
    cn2fn(CharNameChars, FileNameChars, []),
    atom_chars(CharName, CharNameChars).
    %atomic_list_concat(['characters/', CharName, '.pl'], FileName).

cn2fn(CharName) --> seq_atom('characters/'),
                    seq(CharName),
                    seq_atom('.pl').

%! gain_level(?CharLevel, ?Class, ?HPMode)
%
%  Gain a level in the given Class. HPMode determines how your HP
%  increase is calculcated. Use hp_avg to gain the default "average"
%  hp for the given Class. Other options yet to be implemented.
gain_level(_,_,_) :- false.
problem(gain_level_not_contiguous(Levels)) :-
    findall(L, gain_level(L,_,_), Levels),
    max_member(Highest, Levels),
    findall(L, between(2,Highest,L), Levels2),
    Levels \= Levels2.
has(_) :- false.
attuned(_) :- false.

expand_to_sum(Item    , Item + 0) :- Item \= _+_.
expand_to_sum(Item + N, Item + N).

is_shield(shield).
is_shield(Shield + _) :- is_shield(Shield).
is_shield(ShieldF) :- ShieldF =.. [shield|_].

body_armor('studded leather', light, ac(12)).
body_armor('half plate', medium, ac(15)).
body_armor('chain mail', heavy, ac(16)).
body_armor(plate, heavy, ac(18)).
body_armor(Armor+N, Weight, ac(AC)) :-
    body_armor(Armor, Weight, ac(BaseAC)),
    AC is BaseAC + N.

weapon_proficiency(Weapon) :-
    trait(weapon(Weapon)).
weapon_proficiency(Weapon) :-
    weapon(Weapon, Category, _, _, _),
    trait(weapon(Category)).

% You are always proficient with unarmed strikes.
trait(init, weapon(unarmed)).
    
%! weapon(?Name, ?Category, ?Rangedness, ?DamageFormula, ?Notes)
weapon(club, simple, melee,
       [damage(bludgeoning, 1 d 4)], [light]).
weapon(quarterstaff, simple, melee,
       [damage(bludgeoning, 1 d 6)], [versatile(1 d 8)]).
weapon(mace, simple, melee,
       [damage(bludgeoning, 1 d 6)], []).
weapon(handaxe, simple, melee,
       [damage(slashing, 1 d 6)], [light, thrown(feet(20) / feet(60))]).

weapon(battleaxe, martial, melee,
       [damage(slashing, 1 d 8)], [versatile(1 d 10)]).
weapon(greataxe, martial, melee,
       [damage(slashing, 1 d 12)], [heavy, twohanded]).
weapon(longsword, martial, melee,
       [damage(slashing, 1 d 8)], [versatile(1 d 10)]).
weapon(rapier, martial, melee,
       [damage(piercing, 1 d 8)], [finesse]).

weapon('light crossbow', simple, ranged(feet(80) / feet(320)),
       [damage(piercing, 1 d 8)], [ammunition, loading, twohanded]).
weapon(longbow, martial, ranged(feet(150) / feet(600)),
       [damage(piercing, 1 d 8)], [ammunition, heavy, twohanded]).

weapon(javelin, simple, melee,
       [damage(piercing, 1 d 6)], [thrown(feet(30) / feet(120))]).

% Possibilities
% "Melee weapons" can always melee and sometimes be thrown.
%   -> they always use STR (also when thrown), unless they are finesse weapons, in which case you can pick STR/DEX
% "Ranged weapons" cannot be used in melee
%   -> they always use DEX, unless they have the "thrown" property, in which case they use STR
%     -> UNLESS they also have finesse, in which case you can pick STR/DEX.

% A javelin is counted as a melee weapon with thrown property -> thrown simply modifies that it can also be used at range.
% A dart is counted as a ranged weapon with thrown property -> thrown here just indicates that it uses STR and not DEX (unless it's also finesse!).

weapon_onehanded(Weapon) :-
    weapon(Weapon, _, _, _, Notes),
    \+ member(twohanded, Notes).
weapon_melee(Weapon) :-
    weapon(Weapon, _, melee, _, _).
weapon_ranged(Weapon) :-
    weapon(Weapon, _, ranged(_), _, _).

%! weapon_variant(?NamedWeapon, ?BaseWeapon, ?ExtraRolls, ?ExtraNotes)
%  Some (magic) weapons have a specific name but
%  inherit their base stats from a regular weapon.
%  The notation for an enchantment bonus works on the BaseWeapon parameter, so
%  BaseWeapon can be `longsword + 2`, for example.
%  The variant weapon can add extra damage rolls and notes, but can't change
%  any other stats. If other stats need to be changed, it's no longer considered
%  a variant.
weapon_variant(berserker_axe(BaseWeapon), BaseWeapon + 1, [], [attunement, cursed]) :-
    member(BaseWeapon, [handaxe, battleaxe, greataxe]).
custom_format(berserker_axe(BaseWeapon)) -->
    ["berserker "], [BaseWeapon].
bonus_source(attuned(berserker_axe(_)), 'max hp' + Lvl) :-
    level(Lvl).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Equipment JSON 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
equipment_json_dict(_{ weapons: Weapons,
                       weapon_options: WeaponOptions
                     }) :-
    findall(Weapon, weapon_json_dict(Weapon), Weapons),
    findall(WeaponOption, weapon(WeaponOption,_,_,_,_), WeaponOptions).

weapon_json_dict(_{ base_weapon: BaseWeapon,
                    enchantment: Enchantment,
                    category: Category,
                    range: Range,
                    to_hit: ToHit,
                    damage: Damage,
                    notes: Notes,
                    is_variant: IsVariant
                 }) :-
    attack_or_variant(WeaponVal, RangeVal, ToHitVal, DamageVal, NotesVal, IsVariant),
    destructure_weapon_or_variant(WeaponVal, BaseWeaponVal, Enchantment),
    weapon(BaseWeaponVal, Category, _, _, _),
    fmt(format_term(BaseWeaponVal), BaseWeapon),
    fmt(format_range(RangeVal), Range),
    fmt(format_to_hit_or_dc(ToHitVal), ToHit),
    fmt(format_damage(DamageVal), Damage),
    fmt(format_list(NotesVal), Notes).

destructure_weapon_or_variant(Variant : _, BaseWeapon, Enchantment) :-
    Variant \= _:_,
    destructure_weapon_or_variant(Variant, BaseWeapon, Enchantment).
destructure_weapon_or_variant(BaseWeapon + Enchantment, BaseWeapon, Enchantment) :-
    Enchantment \= 0.
destructure_weapon_or_variant(Weapon, Weapon, 0) :-
    atomic(Weapon).

%! max_hp(?HP:int)
%
%  Your character's maximum hit points.
max_hp(HP) :-
    findall(Term, (hp_term_for_level(_, Term); bonus('max hp' + Term)), Terms),
    sumlist(Terms, HP).

%! initial_class_base_hp(?HP:int)
%
%  The base hit points your character gets from its initial class.
initial_class_base_hp(HP) :-
    initial_class(Class),
    initial_class_base_hp(Class, HP).

%! hp_term_for_level(?CharLevel:int, ?Term:int)
%
%  Term is the HP gained on level CharLevel.
hp_term_for_level(1, Term) :-
    initial_class_base_hp(InitHP),
    ability_mod(con, ConMod),
    Term is InitHP + ConMod.
hp_term_for_level(CharLevel, Term) :-
    gain_level(CharLevel, ClassName, HPMode),
    max_hp_per_level(ClassName, HPDie),
    hp_die(HPMode, HPDie, ClassHP),
    ability_mod(con, ConMod),
    Term is ClassHP + ConMod.

%! hp_die(?AvgOrRolled, ?Die, ?HP:int)
%
%  If `AvgOrRolled = hp_avg`, then HP is the roll_avg/2 of Die.
%  Otherwise, `AvgOrRolled = hp_rolled(HP)`.
hp_die(hp_avg, Die, HP) :-
    roll_avg(Die, HP).
hp_die(hp_rolled(HP), _, HP).
fighting_style(archery).
bonus_source(trait(fighting_style(archery)), to_hit(Weapon) + 2) :-
    weapon(Weapon, _, ranged(_), _, _).

fighting_style(defense).
bonus_source(trait(fighting_style(defense)), ac_formula(armor(_)) + 1).
fighting_style(defense) ?= "While you are wearing armor, you gain a +1 bonus to AC.".

fighting_style(dueling).
attack_variant(Weapon:dueling, melee, ToHit, NewDamage, ["when no other weapon equipped"]) :-
    trait(fighting_style(dueling)),
    attack(Weapon, melee, ToHit, Damage, Notes),
    \+ member(twohanded, Notes),
    add_bonus_to_first_damage_roll(Damage, 2, NewDamage).

fighting_style('great weapon fighting').
bonus_source(trait(fighting_style('great weapon fighting')),
             add_weapon_note(Weapon, "may reroll 1 or 2 on a damage die")) :-
    weapon(Weapon, _, melee, _, Notes),
    intersection([twohanded, versatile], Notes, [_|_]).

fighting_style(protection).

fighting_style('two-weapon fighting').
fighting_style('two-weapon fighting') ?= "When you engage in two-weapon fighting, you can add your ability modifier to the damage of the second attack.".
fmt(Spec, Out) :-
    phrase(Spec, Phrase),
    atomics_to_string(Phrase, Out).

format_term(T) --> custom_format(T), {!}.
format_term(X d Y) --> format_dice(X d Y), {!}.
format_term(1 / 2) --> {!}, ["½"].
format_term(X / Y) --> {!}, format_term(X), ['/'], format_term(Y).
format_term(X : Y) --> {!}, format_term(X), [':'], format_term(Y).
format_term(X + Y) --> {!}, format_term(X), ['+'], format_term(Y).
format_term(X = Y) --> {!}, format_term(X), ['='], format_term(Y).
format_term(X -> Y) --> {!}, format_term(X), ['→'], format_term(Y).
format_term(X pct) --> {!}, format_term(X), ['%'].
format_term(X upto Y) --> {!}, format_term(X), [' up to '], format_term(Y).
format_term(feet(Ft)) --> format_number(Ft), {!}, [' ft'].
format_term(hours(1)) --> {!}, ['1 hour'].
format_term(hours(N)) --> {!}, format_term(N), [' hours'].
format_term(cr(X)) --> {!}, ['CR '], format_term(X).
format_term(Number) --> format_number(Number), {!}.
format_term(Atom) --> {atom(Atom), !}, format_atom(Atom).
format_term(String) --> {string(String), !}, [String].
format_term(List) --> {is_list(List), !}, format_list(List).
format_term(Compound) -->
    {Compound =.. [Ftor|Tms], Tms \= []},
    %{\+ is_list(Compound), Compound =.. [Ftor|Tms], \+ member(Ftor, ['/', ':', 'd', '+', pct, cr, upto]), Tms \= []},
    format_atom(Ftor),
    [' ('],
    format_terms(Tms),
    [')'].

format_number(Number) --> {number(Number), !, number_chars(Number, Chars)}, seq(Chars). 

format_atom(Atom) -->
    {atom_chars(Atom, Chars)},
    us_to_space(Chars).

format_terms([]) --> [].
format_terms([X]) --> format_term(X).
format_terms([X|Xs]) --> {Xs \= []}, format_term(X), [', '], format_terms(Xs).

format_list_empty_as_dash([]) --> ['-'].
format_list_empty_as_dash(L) --> {L = [_|_]}, format_list(L).

format_list([]) --> {!}, [].
format_list([X]) --> {!}, format_term(X).
format_list([X|Xs]) --> {Xs \= [], !}, format_term(X), [', '], format_list(Xs).

format_list_flat([]) --> [].
format_list_flat([X]) --> [X].
format_list_flat([X|Xs]) --> {Xs \= []}, [X], [', '], format_list_flat(Xs).

format_damage([]) --> {!}, [].
format_damage([R]) --> {!}, format_damage_roll(R).
format_damage([R|Rs]) --> {Rs \= []}, format_damage_roll(R), ',', format_damage(Rs).
format_damage_roll(damage(Type,Dice)) -->
    format_dice_sum(Dice), [' '], [Type].

format_range(feet(X)) --> {!}, [X], [" ft"].
format_range(miles(X)) --> {!}, [X], [" mi"].
format_range(Short/Long) --> {!}, format_range(Short), ['/'], format_range(Long).
format_range(X) --> [X].

format_dice_sum(Ds + K) --> {number(K), K \= 0, !}, format_dice_sum(Ds), ['+'], [K].
format_dice_sum(Ds + 0) --> {!}, format_dice_sum(Ds).
format_dice_sum(Ds + D) --> {!}, format_dice_sum(Ds), ['+'], format_dice(D).
format_dice_sum(Ds) --> format_dice(Ds).
format_dice(N d X) --> seq([N, 'd', X]).
format_dice(N) --> {number(N)}, [N].

format_bonus(N) --> {N >= 0}, ['+'], [N].
format_bonus(N) --> {N < 0}, [N].
format_bonus_str(N, Str, Tail) :-
    format_bonus(N, Fmt, Tail),
    atomic_list_concat(Fmt, Str).

format_measure(Measure) -->
    {Measure =.. [Unit, Magnitude]},
    [Magnitude],
    [" "],
    [Unit].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Format effects.
format_effects([]) --> [].
format_effects([E|Es]) -->
    format_effect(E),
    ["; "],
    format_effects(Es).

format_effect(Damage) -->
    {Damage = damage(_,_), !},
    format_damage_roll(Damage).
format_effect(spell_attack_roll(_):Effects) -->
    {is_list(Effects), !},
    ["["],
    format_effects(Effects),
    ["] on hit"].
format_effect(spell_attack_roll(_):Effect) -->
    format_effect(Effect),
    [" on hit"].
format_effect(in(Area):Effect) -->
    ["in "],
    format_area(Area),
    [": "],
    format_effect(Effect),
    {!}.
format_effect(1*Es) --> {!}, format_effect(Es).
format_effect(N*Es) -->
    {!},
    [N],
    [" times "],
    format_effect(Es).
format_effect(saving_throw(dc(Abi,DC)):(E1 else E2)) -->
    {!},
    [Abi],
    [" saving throw (DC "],
    format_number(DC),
    [") -> "],
    format_effect(E1),
    [" on fail, else "],
    format_effect(E2).
format_effect(saving_throw(Abi):(E1 else E2)) -->
    {!},
    ["saving throw ("],
    [Abi],
    [") -> "],
    format_effect(E1),
    [" on fail, else "],
    format_effect(E2).
format_effect(saving_throw(Abi):Effect) -->
    {!},
    ["saving throw ("],
    [Abi],
    [") -> "],
    format_effect(Effect),
    [" on fail"].
format_effect(damage(Type,Roll)) -->
    {!},
    format_damage_roll(damage(Type,Roll)).
format_effect(E) -->
    format_term(E).
    
format_area(N ft Shape) --> [N], [" ft "], [Shape].
format_area(N by M ft Shape) --> [N], ["×"], [M], [" ft "], [Shape].

% Replace underscores by spaces.
us_to_space([ X |Xs]) --> {X \= '_'}, [X], us_to_space(Xs).
us_to_space(['_'|Xs]) --> [' '], us_to_space(Xs).
us_to_space([]) --> [].
    
interleave([X|Xs], [Y|Ys]) --> [X], [Y], interleave(Xs, Ys).
interleave(Xs, []) --> {Xs \= []}, seq(Xs).
interleave([], Ys) --> seq(Ys).

sep(_, []) --> [].
sep(_, [X]) --> [X].
sep(Sep, [X|Xs]) --> {Xs \= []}, [X], seq(Sep), sep(Sep, Xs).

rep(_) --> [].
rep(X) --> [X], rep(X).

seq([]) --> [].
seq([E|Es]) --> [E], seq(Es).

seq_atom(Atom) --> {atom_chars(Atom, Chars)}, seq(Chars).
    
maybe(_) --> [].
maybe(X) --> [X].
race_option(tabaxi).
race_shorthand(tabaxi, tx).
racial_speed(tabaxi, 30).

meta_todo("tabaxi ability_plus_n", "forbid stacking ability plus on same attr").

bonus_options_source(race(tabaxi), 'ability + 2', id, ability_plus_n(2)).
bonus_options_source(race(tabaxi), 'ability + 1', id, ability_plus_n(1)).
hide_base_option(race(tabaxi), 'ability + 1', Abi+1) :-
    bonus(choice(race(tabaxi), 'ability + 2'), Abi+2).
hide_base_option(race(tabaxi), 'ability + 2', Abi+2) :-
    bonus(choice(race(tabaxi), 'ability + 1'), Abi+1).
problem(cant_stack_racial_asis(Abi)) :-
    choice(race(tabaxi), 'ability + 2', Abi + 2),
    choice(race(tabaxi), 'ability + 1', Abi + 1).

meta_todo(race(tabaxi), "generalize ability+1 / ability+2 option").

trait_source(race(tabaxi), sense(darkvision)).
trait_source(race(tabaxi), 'feline agility').
trait_source(race(tabaxi), 'cat\'s claws').

meta_todo("tabaxi claws", "add tabaxi claws").
suppress_unarmed :-
    trait(race(tabaxi), 'cat\'s claws').

meta_todo("other speeds", "figure out how to encode other speeds (climbing, swimming, etc)").

traits_from_source(race(tabaxi),
                   [skill(perception), skill(stealth), language(common)]).
trait_options_source(race(tabaxi), language, wrap(language), language).


'feline agility' ?= "Your reflexes and agility allow you to move with a burst of speed. When you move on your tum in combat, you can double your speed until the end of the tum. Once you use this trait, you can't use it again until you move 0 feet on one of your turns.".

'cat\'s claws' ?= "Because of your claws, you have a climbing speed of 20 feet. In addition, your claws are natural weapons, which you can use to make unarmed strikes. If you hit with them, you deal slashing damage equal to 1d4 + your Strength modifier, instead of the bludgeoning damage normal for an unarmed strike.".
race_option(gnome).
race_shorthand(gnome, gn).
racial_speed(gnome, 25).
bonus_source(race(gnome), int+2).
trait_source(race(gnome), sense(darkvision)).
trait_source(race(gnome), 'gnome cunning').
traits_from_source(race(gnome),
                   [language(common), language(gnomish)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subrace_option(gnome, 'rock gnome').
bonus_source(race(gnome('rock gnome')), con+1).
trait_source(race(gnome('rock gnome')), 'artificer\'s lore').
trait_source(race(gnome('rock gnome')), 'tinker').
trait_options_source(race(gnome('rock gnome')), tool, wrap(tool),
                     from_list([smith, brewer, mason, alchemist])).
traits_from_source(race(gnome('rock gnome')),
                   [language(common), language(gnomish)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'gnome cunning' ?= "You have advantage on all Intelligence, Wisdom, and Charisma saving throws against magic.".

'artificer\'s lore' ?=
  "Whenever you make an Intelligence (History) check related to magic items, alchemical objects, or technological devices, you can add twice your proficiency bonus, instead of any proficiency bonus you normally apply.".

'tinker' ?=
  "You have proficiency with artisan's tools (tinker's tools). Using those tools, you can spend 1 hour and 10 gp worth of materials to construct a Tiny clockwork device (AC 5, 1 hp). The device ceases to function after 24 hours (unless you spend 1 hour repairing it to keep the device functioning), or when you use your action to dismantle it; at that time, you can reclaim the materials used to create it. You can have up to three such devices active at a time.".
race_option(human).
racial_speed(human, 30).
race_shorthand(human, hu).

trait_source(race(human), language(common)).
trait_options_source(race(human), language, wrap(language), language).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subrace_option(human, standard).
bonus_source(race(human(standard)), Ability+1) :-
    ability(Ability).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subrace_option(human, variant).
bonus_options_source(race(human(variant)), asi, id, 2 from ability_plus_n(1)).
trait_options_source(race(human(variant)), skill, wrap(skill), skill).
trait_options_source(race(human(variant)), feat, wrap(feat), selectable_feat_option).
race_option(tiefling).
racial_speed(tiefling, 30).
race_shorthand(tiefling, tf).
bonus_source(race(tiefling), int+1).
bonus_source(race(tiefling), cha+2).

traits_from_source(race(tiefling), [darkvision(60),
                                    resistance(fire),
                                    trait('infernal legacy'),
                                    language(common),
                                    language(infernal)]).

known_spell(race(tiefling), cha, always, [], no, thaumaturgy) :-
    race(tiefling).
known_spell(race(tiefling), cha, always, [per_rest(1)], no, 'hellish rebuke') :-
    race(tiefling),
    match_level(3).
known_spell(race(tiefling), cha, always, [per_rest(1)], no, darkness) :-
    race(tiefling),
    match_level(5).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DESCRIPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'infernal legacy' ?= "You know the 'darkness' spell. When you reach 3rd level, you can cast the 'hellish rebuke' spell as a 2nd-level spell once per long rest. When you reach 5th level, you can also cast the 'darkness' spell once per long rest. Charisma is your spellcasting ability for these spells.".
race_option(halfling).
race_shorthand(halfling, hf).
racial_speed(halfling, 25).
bonus_source(race(halfling), dex+2).
trait_source(race(halfling), lucky).
trait_source(race(halfling), brave).
trait_source(race(halfling), 'halfling nimbleness').
traits_from_source(race(halfling), [language(common), language(halfling)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subrace_option(halfling, lightfoot).
bonus_source(race(halfling), cha+2).
trait_source(race(halfling(lightfoot)), 'naturally stealthy').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lucky ?= "When you roll a 1 on the d20 for an attack roll, ability check, or saving throw, you can reroll the die and must use the new roll.".

brave ?= "You have advantage on saving throws against being frightened.".

'halfling nimbleness' ?= "You can move through the space of any creature that is of a size larger than yours.".

'naturally stealthy' ?= "You can attempt to hide even when you are obscured only by a creature that is at least one size larger than you.".
race_option(dwarf).
race_shorthand(dwarf, dw).
racial_speed(dwarf, 25).
bonus_source(race(dwarf), con+2).
trait_source(race(dwarf), sense(darkvision)).
trait_source(race(dwarf), 'no heavy armor speed penalty').
trait_source(race(dwarf), 'dwarven resilience').
trait_source(trait('dwarven resilience'), resistance(poison)).
traits_from_source(race(dwarf),
                   [weapon(battleaxe), weapon(handaxe),
                    weapon('light hammer'), weapon(warhammer)]).
trait_options_source(race(dwarf), tool, wrap(tool),
                     from_list([smith, brewer, mason])).
trait_source(race(dwarf), stonecunning).
traits_from_source(race(dwarf), [language(common), language(dwarvish)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subrace_option(dwarf, 'hill dwarf').
bonus_source(race(dwarf('hill dwarf')), wis+1).
trait_source(race(dwarf('hill dwarf')), 'dwarven toughness').
bonus_source(trait('dwarven toughness'), 'max hp'+Lvl) :-
    level(Lvl).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'dwarven toughness' ?= "Your hit point maximum increases by 1, and it increases by 1 every time you gain a level.".

'dwarven resilience' ?=
  "Advantage on saving throws against poison and resistance against poison damage".

stonecunning ?= "Whenever you make an Intelligence (History) check related to the origin of stonework, you are considered proficient in the History skill and add double your proficiency bonus to the check, instead of your normal proficiency bonus.".
race_option('half-elf').
racial_speed('half-elf', 30).
race_shorthand(he).

bonus_source(race('half-elf'), cha+2).
bonus_options_source(race('half-elf'), asi, id, 2 from ability_plus_n(1)).
traits_from_source(race('half-elf'), ['fey ancestry',
                                      sense(darkvision),
                                      language(common),
                                      language(elvish)]).
trait_options_source(race('half-elf'), 'skill versatility', wrap(skill),
                     2 unique_from skill).
trait_options_source(race('half-elf'), language, wrap(language), language).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DESCRIPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'fey ancestry' ?= "You have advantage on saving throws against being charmed, and magic can’t put you to sleep.".
race_option(dragonborn).
race_shorthand(dragonborn, db).
racial_speed(dragonborn, 30).

traits_from_source(race(dragonborn), [language(common), language(draconic)]).

trait_options_source(race(dragonborn), 'draconic ancestry',
                     wrap(draconic_ancestry), dragon_color).

trait(trait(draconic_ancestry(Color)),
      breath_weapon(in(AoE):saving_throw(dc(Abi,DC)):(damage(Element, N d 6) else half))) :-
    trait(draconic_ancestry(Color)),
    dragon_element(Color, Element),
    breath_weapon(Color, AoE, Abi),
    breath_weapon_damage_dice(N),
    breath_weapon_dc(DC).

% TODO: not sure if we should put this in the attack table
attack('breath weapon', self, saving_throw(DC, Abi), [damage(Element, N d 6)],
       [AoEFmt, "on save: half damage", "one per long rest"]) :-
    trait(trait(draconic_ancestry(Color)), breath_weapon(_)),
    dragon_element(Color, Element),
    breath_weapon(Color, AoE, Abi),
    breath_weapon_damage_dice(N),
    breath_weapon_dc(DC),
    fmt(format_area(AoE), AoEFmt).

resource('breath weapon', 'breath weapon', 1) :-
    trait(breath_weapon(_)).
on_rest(long, 'breath weapon', 'full restore').

trait(trait(draconic_ancestry(Color)), resistance(Element)) :-
    trait(draconic_ancestry(Color)),
    dragon_element(Color, Element).

breath_weapon(black, 5 by 30 ft line, dex).
breath_weapon(blue, 5 by 30 ft line, dex).
breath_weapon(brass, 5 by 30 ft line, dex).
breath_weapon(bronze, 5 by 30 ft line, dex).
breath_weapon(copper, 5 by 30 ft line, dex).
breath_weapon(gold, 15 ft cone, dex).
breath_weapon(green, 15 ft cone, con).
breath_weapon(red, 15 ft cone, dex).
breath_weapon(silver, 15 ft cone, con).
breath_weapon(white, 15 ft cone, con).

breath_weapon_dc(DC) :-
    add_ability_mod_and_profbon(8, con, DC).
    
breath_weapon_damage_dice(N) :-
    level(Level),
    ordered_lookup_largest_leq([1 -> 2, 6 -> 3, 11 -> 4, 16 -> 5], Level, N).

custom_format(breath_weapon(Effect)) -->
    ["breath weapon ("],
    format_effect(Effect),
    [")"].
race_option(elf).
racial_speed(elf, 30).
race_shorthand(elf, el).

traits_from_source(race(elf), [sense(darkvision), 
                               'fey ancestry',
                               sense('keen senses'),
                               trance,
                               language(common),
                               language(elvish)]).
trait_source(trait(sense('keen senses')), skill(perception)).
bonus_source(race(elf), dex+2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Subrace: high elf
subrace_option(elf, 'high elf').
bonus_source(race(elf('high elf')), int+1).
traits_from_source(race(elf('high elf')), [weapon(longsword),
                                           weapon(shortsword),
                                           weapon(shortbow),
                                           weapon(longbow)]).

options_source(race(elf('high elf')), cantrip, class_cantrip(wizard)).
known_spell(race(elf('high elf')), int, always, [], no, Cantrip) :-
    choice(race(elf('high elf')), cantrip, Cantrip).

trait_options_source(race(elf('high elf')), language, wrap(language), language).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DESCRIPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'fey ancestry' ?= "You have advantage on saving throws against being charmed, and magic can’t put you to sleep.".
'keen senses' ?= "You have proficiency in the Perception skill.".
'trance' ?= "Elves don’t need to sleep. Instead, they meditate deeply, remaining semiconscious, for 4 hours a day. (The Common word for such meditation is \"trance.\") While meditating, you can dream after a fashion; such dreams are actually mental exercises that have become reflexive through years of practice. After resting in this way, you gain the same benefit that a human does from 8 hours of sleep.".
race_option('half-orc').
race_shorthand('half-orc', ho).
racial_speed('half-orc', 30).
bonus_source(race('half-orc'), str+2).
bonus_source(race('half-orc'), con+1).
trait_source(race('half-orc'), sense(darkvision)).
trait_source(race('half-orc'), menacing).
trait_source(race('half-orc'), 'savage attacks').
traits_from_source(race('half-orc'),
                   [language(common), language(orcish)]).

trait_source(race('half-orc'), 'relentless endurance').
resource('relentless endurance', 'relentless endurance', 1) :-
    trait('relentless endurance').
on_rest(long, 'relentless endurance', 'full restore').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'menacing' ?= "You gain proficiency in the Intimidation skill.".

'relentless endurance' ?=
  "When you are reduced to 0 hit points but not killed outright, you can drop to 1 hit point instead. You can't use this feature again until you finish a long rest.".

'savage attacks' ?= "When you score a critical hit with a melee weapon attack, you can roll one of the weapon's damage dice one additional time and add it to the extra damage of the critical hit.".
% :- table (options/3, hide_base_option/3) as incremental.

%! options(?Source, ?Id, ?Spec)
%  
%  Each options/3 clause indicates that a choice/3 needs to be made to
%  advance the PC. Source indicates the reason why the options are
%  available. Source and Id together uniquely identify the choice to
%  be made. Spec is a unary predicate such that Spec(Choice) is true
%  only if Choice is a valid choice.
%  A todo is generated for every options/3 clause without matching
%  choice/3 clause.
options(_,_,_) :- false.

%! todo(?Todo)
todo(options(Origin, Id, Spec)) :-
    % Generate a todo for each options clause without corresponding choice clause.
    options(Origin, Id, Spec),
    \+ (choice(Origin, Id, _); ignore(Origin, Id)).
todo(problem:P) :- problem(P).

%! ignore(?Origin, ?Id)
%
%  Indicate that you deliberately don't want to pick an option for a
%  given options/3 with the matching Origin and Id.
ignore(_,_) :- false.

%! options_source(?Source, ?Id, ?Spec)
%
%  Each options_source/3 clause gives rise to a corresponding
%  options/3 clause, *if* `call(Source)` is true.
options_source(_,_,_) :- false.
% Generate options for the options_source clauses where the requirement is matched.
options(Source, Id, Spec) :-
    options_source(Source, Id, Spec),
    call(Source).

%! choice(?Source, ?Id, ?Choice)
%
%  Each choice clause represents a choice the user made to advance the
%  PC. A choice/3 clause is merely an assertion that the user wants to
%  make a given choice, not that that choice is valid.
%  A choice is valid if
%  * there is a corresponding options/3 clause `options(Source, Id, Spec)`,
%  * the Choice matches the options clause's Spec (see choice_matches_spec/3), and
%  * there isn't another separate choice/3 clause with the same Origin and Id.
% 
%  If a choice is invalid, a problem/1 is flagged.
choice(_,_,_) :- false.
problem(choice_double_dip(Origin, Id, Choices)) :-
    % Flag a problem if we have multiple choice/3 clauses for one options/3 clause.
    options(Origin, Id, _),
    findall(Choice, choice(Origin, Id, Choice), Choices),
    length(Choices, Len),
    Len > 1.
problem(not_eligible(Origin, Id, Choice)) :-
    % Flag a problem if we have a choice/3 clause without corresponding options/3 clause.
    choice(Origin, Id, Choice),
    \+ options(Origin, Id, _).
problem(choice_does_not_match_spec(Origin, Id, Choice)) :-
    % Flag a problem if a choice clause does not match the spec of the corresponding options clause.
    choice(Origin, Id, Choice),
    \+ choice_matches_spec(Origin, Id, Choice).
choice(_,_) :- false. % Suppress a warning which I think is caused a bug in swipl.
problem(chosen_same_trait_twice(Origin1, Origin2, Id1, Id2, Trait)) :-
    % Flag a problem if the same trait is chosen twice.
    trait(choice(Origin1, Id1), Trait),
    trait(choice(Origin2, Id2), Trait),
    (Origin1 \= Origin2 ; Id1 \= Id2).

%! choice_matches_spec(?Origin, ?Id, ?Choice)
%
%  True for every possible Choice the user can make, for a given
%  options/3 clause with matching Origin and Id. So,
%  choice_matches_spec(Origin, Id, Choice) does not imply choice(Origin, Id,
%  Choice).
choice_matches_spec(Origin, Id, Choice) :-
    options(Origin, Id, Spec),
    call(Spec, Choice).

%! choice_member(?Origin, ?Id, ?Choice)
%
%  For some choice(Origin, Id, Choices) clauses, Choices is a list of
%  individual choices. choice_member(Origin, Id, Choice) retrieves the
%  individual elements if Choices is a list; if it is not, it just
%  retrieves Choices.
choice_member(Origin, Id, Choice) :-
    choice(Origin, Id, C),
    (member(Choice, C); \+ is_list(C), Choice = C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper predicates

%! from(+N:int, :Pred, ?Choices)
%  Helper predicate to support the infix `N from Pred` notation in
%  options/3 specifications.
from(1, Spec, Choice) :-
    \+ is_list(Choice),
    from_(1, Spec, [Choice]).
from(N, Spec, Choice) :-
    from_(N, Spec, Choice).
from_(N, List, Choices) :-
    is_list(List),
    !,
    is_list(Choices),
    length(Choices, N),
    subset(Choices, List).
from_(N, Pred, Choices) :-
    is_list(Choices),
    between(1, N, M), % TODO this is inefficient if M is known
    length(Choices, M),
    %M =< N,
    maplist(call(Pred), Choices).

%! unique_from(+N:int, :Pred, ?Choices)
%  Like from/3, but only true if each choice in Choices is unique.
unique_from(N, Pred, Choices) :-
    from(N, Pred, Choices),
    (is_set(Choices) ; N = 1).
         
%! from_list(?List, ?Elem)
%  Like member/2, but with the arguments flipped.
%  Useful for options/3 specifications.
from_list(L, X) :- member(X, L).

or(Goal1, Goal2, X) :-
    call(Goal1, X) ; call(Goal2, X).

%! free_choice(?Choice)
%
%  True for all Choice.
free_choice(_).

%! inspect_options(?Origin, ?Id, ?Desc)
%  Desc describes all valid choices for the options/3 clause with
%  matching Origin and Id.
%  The format of Desc is one of:
%  - `N unique_from D`; with `N` an integer and `D` a sub-escription,
%  - `N from D`; with `N` an integer and `D` a sub-escription,
%  - A list of terms.
inspect_options(Origin, Id, Desc) :-
    options(Origin, Id, Spec), % ground
    inspect_spec(Origin, Id, Spec, Desc).
inspect_spec(Origin, Id, N from Pred, N from List) :-
    inspect_spec(Origin, Id, Pred, List).
inspect_spec(Origin, Id, N unique_from Pred, N unique_from List) :-
    inspect_spec(Origin, Id, Pred, List).
inspect_spec(Origin, Id, Spec1 or Spec2, Desc1 or Desc2) :-
    inspect_spec(Origin, Id, Spec1, Desc1),
    inspect_spec(Origin, Id, Spec2, Desc2).
inspect_spec(Origin, Id, Pred, List) :-
    \+ member(Pred, [_ from _, _ unique_from _]),
    findall(X, (call(Pred, X), (\+ suppress_base_option(Origin, Id, X))), List).
suppress_base_option(Origin, Id, X) :-
    hide_base_option(Origin, Id, X), (\+ choice_member(Origin, Id, X)).

%! hide_base_option(?Source, ?Id, ?Option)
%
%  True iff Option shouldn't be displayed to the user as a valid
%  choice for the option with given Source and Id.
hide_base_option(_,_,_) :- false.

%! lookup_option_doc(-Source, -Id, -Option, +Doc)
%
%  Find the documentation associated with a given Option for a given Source and Id.
lookup_option_doc(_, _, Option, Doc) :-
    (Option ?= Doc).
    
%! option_todo(?Origin, ?Id, ?Spec)
options_todo(Origin, Id, Spec) :-
    options(Origin, Id, Spec),
    \+ choice(Origin, Id, _).

%! options_by_level_json(?Opts)
all_options_by_level_json(Out) :-
    findall(Json,
            ( options(Orig, Id, _),
              options_json(Orig, Id, Json) ),
            Jsons),
    level(Level),
    findall(LAtom-[], (between(1,Level,L), atom_number(LAtom, L)), InitDictEntries),
    dict_create(Init, _, InitDictEntries),
    extend_multimap(Init,
                    [D,LAtom]>>(dict_get_pred(D,charlevel,L), atom_number(LAtom,L)),
                    Jsons,
                    Out).

dict_get_pred(Dict, Field, Val) :-
    Dict.get(Field) = Val.

%! options_json(?Origin, ?Id, ?Json)
options_json(Origin, Id, _{origin: OriginStr,
                           origin_category: CategoryStr, display_origin_category: DisplayCategory,
                           origin_category_index: CatIdx,
                           charlevel: CharLevel, id: IdStr, display_id: DisplayId, spec: SpecJson,
                           choice: ChoiceJson}) :-
    options(Origin, Id, Spec),
    origin_category_or_uncategorized(Category, Origin), term_string(Category, CategoryStr),
    trait_category_string(Category, DisplayCategory),
    origin_category_canonical_order(Category, CatIdx),
    origin_level(Origin, CharLevel),
    spec_to_json(Origin, Id, Spec, SpecJson),
    choice_json(Origin, Id, Spec, ChoiceJson),
    term_string(Origin, OriginStr),
    term_string(Id, IdStr),
    fmt(format_term(Id), DisplayId).

%! resolve_not_eligible
%
%  If there are any choice/3 facts for which the PC is not eligible,
%  retract them.
resolve_not_eligible :-
    forall(problem(not_eligible(A,B,C)),
           (format(user_output, "Retracted ~w!~n", [choice(A,B,C)]),
            flush_output(user_output),
            retractall(choice(A,B,C)))).

% Case: `from` or `unique_from` spec.
spec_to_json(Origin, Id, Spec,
             _{spectype: Functor,
               num: N,
               spec: SubSpec1}) :-
    Spec =.. [Functor, N, SubSpec],
    (Functor = from ; Functor = unique_from),
    !,
    spec_to_json(Origin, Id, SubSpec, SubSpec1).
% Case: `or` spec.
spec_to_json(Origin, Id, Spec1 or Spec2,
             _{spectype: or,
               left: SubSpec1,
               right: SubSpec2,
               leftname: LeftName,
               rightname: RightName}) :-
    !,
    spec_to_json(Origin, Id, Spec1, SubSpec1),
    spec_to_json(Origin, Id, Spec2, SubSpec2),
    term_string(Spec1, LeftName),
    term_string(Spec2, RightName).
% Case: any other predicate.
spec_to_json(Origin, Id, Spec,
             _{spectype: list, list: List}) :-
    findall(_{opt: XStr, desc: Desc},
            (call(Spec, X),
             (\+ suppress_base_option(Origin, Id, X)),
             term_string(X, XStr),
             default_on_fail("", lookup_option_doc(Origin, Id, X), Desc)),
             %fmt(format_term(X), XStr)),
            List).

choice_json(Origin, Id, Spec, Json) :-
    choice(Origin, Id, Choice),
    options(Origin, Id, Spec),
    choice_to_json(Choice, Spec, Json).
choice_json(Origin, Id, _, null) :-
    \+ choice(Origin, Id, _).
choice_to_json(X, Left or _, _{choicetype: or, side: left, choice: Json}) :-
    ground(Left),
    call(Left, X),
    !,
    choice_to_json(X, Left, Json).
choice_to_json(X, _ or Right, _{choicetype: or, side: right, choice: Json}) :-
    ground(Right),
    call(Right, X),
    !,
    choice_to_json(X, Right, Json).
choice_to_json(List, Pred, JsonList) :-
    is_list(List),
    !,
    maplist([X,Y]>>choice_to_json(X,Pred,Y), List, JsonList).
choice_to_json(X, _, XStr) :-
    term_string(X, XStr).
    
desc_to_dict_pairs(Desc, [spectype-"list", num-N, options-List]) :-
    ((Desc = [From, N, List], (From = from ; From = unique_from)))
    ;
    (is_list(Desc), List=Desc, N=1).

fill_in_all_options :-
    fill_in_option(Origin, Id, Choice),
    writeln(Origin : Id : Choice),
    fill_in_all_options.

fill_in_option(Origin, Id, Choice) :-
    todo(options(Origin, Id, Spec)),
    call(Spec, Choice), !,
    assert(choice(Origin, Id, Choice)).
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OPTION TREE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO work on this some time.

% option_tree([ init : 'initial class' : ClassOpts, 
%               init : 'base race'     : RaceOpts,
%               init : background      : BackgroundOpts
%             ]) :-
%     class_option_tree(ClassOpts),
%     race_option_tree(RaceOpts),
%     background_option_tree(BackgroundOpts).
% 
% class_option_tree() :-
%     
% race_option_tree([]).
% background_option_tree([]).
% 
% %! choice_creates_category(Origin, Id, )
% choice_creates_category(_, _, _) :- false.
:- dynamic spell_auto_data/2.
:- multifile spell_auto_data/2.

spell_auto_data('acid arrow', properties{area_of_effect:false, attack_type:ranged, casting_time:"1 action", classes:[wizard], components:[v, s, m("Powdered rhubarb leaf and an adder's stomach.")], concentration:no, damage_at_slot_level:_{2:damage(acid, 4 d 4), 3:damage(acid, 5 d 4), 4:damage(acid, 6 d 4), 5:damage(acid, 7 d 4), 6:damage(acid, 8 d 4), 7:damage(acid, 9 d 4), 8:damage(acid, 10 d 4), 9:damage(acid, 11 d 4)}, damage_with_cantrip_scaling:false, dc:false, desc:["A shimmering green arrow streaks toward a target within range and bursts in a spray of acid. Make a ranged spell attack against the target. On a hit, the target takes 4 d 4 acid damage immediately and 2 d 4 acid damage at the end of its next turn. On a miss, the arrow splashes the target with acid for half as much of the initial damage and no damage at the end of its next turn."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 3rd level or higher, the damage (both initial and later) increases by 1 d 4 for each slot level above 2nd.", level:2, material:"Powdered rhubarb leaf and an adder's stomach.", range:feet(90), ritual:no, school:evocation}).
spell_auto_data('acid splash', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:damage(acid, 1 d 6), dc:dex else none, desc:["You hurl a bubble of acid. Choose one creature within range, or choose two creatures within range that are within 5 feet of each other. A target must succeed on a dexterity saving throw or take 1 d 6 acid damage.", "This spell's damage increases by 1 d 6 when you reach 5th level (2 d 6), 11th level (3 d 6), and 17th level (4 d 6)."], duration:"Instantaneous", higher_level:no, level:0, material:false, range:feet(60), ritual:no, school:conjuration}).
spell_auto_data(aid, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric, paladin], components:[v, s, m("A tiny strip of white cloth.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Your spell bolsters your allies with toughness and resolve. Choose up to three creatures within range. Each target's hit point maximum and current hit points increase by 5 for the duration."], duration:"8 hours", higher_level:"When you cast this spell using a spell slot of 3rd level or higher, a target's hit points increase by an additional 5 for each slot level above 2nd.", level:2, material:"A tiny strip of white cloth.", range:feet(30), ritual:no, school:abjuration}).
spell_auto_data(alarm, properties{area_of_effect:20 ft cube, attack_type:false, casting_time:"1 minute", classes:[ranger, wizard], components:[v, s, m("A tiny bell and a piece of fine silver wire.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You set an alarm against unwanted intrusion. Choose a door, a window, or an area within range that is no larger than a 20-foot cube. Until the spell ends, an alarm alerts you whenever a Tiny or larger creature touches or enters the warded area. When you cast the spell, you can designate creatures that won't set off the alarm. You also choose whether the alarm is mental or audible.", "A mental alarm alerts you with a ping in your mind if you are within 1 mile of the warded area. This ping awakens you if you are sleeping.", "An audible alarm produces the sound of a hand bell for 10 seconds within 60 feet."], duration:"8 hours", higher_level:no, level:1, material:"A tiny bell and a piece of fine silver wire.", range:feet(30), ritual:yes, school:abjuration}).
spell_auto_data('alter self', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You assume a different form. When you cast the spell, choose one of the following options, the effects of which last for the duration of the spell. While the spell lasts, you can end one option as an action to gain the benefits of a different one.", "**Aquatic Adaptation.** You adapt your body to an aquatic environment, sprouting gills and growing webbing between your fingers. You can breathe underwater and gain a swimming speed equal to your walking speed.", "**Change Appearance.** You transform your appearance. You decide what you look like, including your height, weight, facial features, sound of your voice, hair length, coloration, and distinguishing characteristics, if any. You can make yourself appear as a member of another race, though none of your statistics change. You also can't appear as a creature of a different size than you, and your basic shape stays the same; if you're bipedal, you can't use this spell to become quadrupedal, for instance. At any time for the duration of the spell, you can use your action to change your appearance in this way again.", "**Natural Weapons.** You grow claws, fangs, spines, horns, or a different natural weapon of your choice. Your unarmed strikes deal 1 d 6 bludgeoning, piercing, or slashing damage, as appropriate to the natural weapon you chose, and you are proficient with your unarmed strikes. Finally, the natural weapon is magic and you have a +1 bonus to the attack and damage rolls you make using it."], duration:"Up to 1 hour", higher_level:no, level:2, material:false, range:self, ritual:no, school:transmutation}).
spell_auto_data('animal friendship', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, druid, ranger], components:[v, s, m("A morsel of food.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else none, desc:["This spell lets you convince a beast that you mean it no harm. Choose a beast that you can see within range. It must see and hear you. If the beast's Intelligence is 4 or higher, the spell fails. Otherwise, the beast must succeed on a wisdom saving throw or be charmed by you for the spell's duration. If you or one of your companions harms the target, the spells ends."], duration:"24 hours", higher_level:no, level:1, material:"A morsel of food.", range:feet(30), ritual:no, school:enchantment}).
spell_auto_data('animal messenger', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, druid, ranger], components:[v, s, m("A morsel of food.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["By means of this spell, you use an animal to deliver a message. Choose a Tiny beast you can see within range, such as a squirrel, a blue jay, or a bat. You specify a location, which you must have visited, and a recipient who matches a general description, such as \"a man or woman dressed in the uniform of the town guard\" or \"a red-haired dwarf wearing a pointed hat.\" You also speak a message of up to twenty-five words. The target beast travels for the duration of the spell toward the specified location, covering about 50 miles per 24 hours for a flying messenger, or 25 miles for other animals.", "When the messenger arrives, it delivers your message to the creature that you described, replicating the sound of your voice. The messenger speaks only to a creature matching the description you gave. If the messenger doesn't reach its destination before the spell ends, the message is lost, and the beast makes its way back to where you cast this spell."], duration:"24 hours", higher_level:"If you cast this spell using a spell slot of 3nd level or higher, the duration of the spell increases by 48 hours for each slot level above 2nd.", level:2, material:"A morsel of food.", range:feet(30), ritual:yes, school:enchantment}).
spell_auto_data('animal shapes', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[druid], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Your magic turns others into beasts. Choose any number of willing creatures that you can see within range. You transform each target into the form of a Large or smaller beast with a challenge rating of 4 or lower. On subsequent turns, you can use your action to transform affected creatures into new forms.", "The transformation lasts for the duration for each target, or until the target drops to 0 hit points or dies. You can choose a different form for each target. A target's game statistics are replaced by the statistics of the chosen beast, though the target retains its alignment and Intelligence, Wisdom, and Charisma scores. The target assumes the hit points of its new form, and when it reverts to its normal form, it returns to the number of hit points it had before it transformed. If it reverts as a result of dropping to 0 hit points, any excess damage carries over to its normal form. As long as the excess damage doesn't reduce the creature's normal form to 0 hit points, it isn't knocked unconscious. The creature is limited in the actions it can perform by the nature of its new form, and it can't speak or cast spells.", "The target's gear melds into the new form. The target can't activate, wield, or otherwise benefit from any of its equipment."], duration:"Up to 24 hours", higher_level:no, level:8, material:false, range:feet(30), ritual:no, school:transmutation}).
spell_auto_data('animate dead', properties{area_of_effect:false, attack_type:false, casting_time:"1 minute", classes:[cleric, wizard], components:[v, s, m("A drop of blood, a piece of flesh, and a pinch of bone dust.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["This spell creates an undead servant. Choose a pile of bones or a corpse of a Medium or Small humanoid within range. Your spell imbues the target with a foul mimicry of life, raising it as an undead creature. The target becomes a skeleton if you chose bones or a zombie if you chose a corpse (the DM has the creature's game statistics).", "On each of your turns, you can use a bonus action to mentally command any creature you made with this spell if the creature is within 60 feet of you (if you control multiple creatures, you can command any or all of them at the same time, issuing the same command to each one). You decide what action the creature will take and where it will move during its next turn, or you can issue a general command, such as to guard a particular chamber or corridor. If you issue no commands, the creature only defends itself against hostile creatures. Once given an order, the creature continues to follow it until its task is complete.", "The creature is under your control for 24 hours, after which it stops obeying any command you've given it. To maintain control of the creature for another 24 hours, you must cast this spell on the creature again before the current 24-hour period ends. This use of the spell reasserts your control over up to four creatures you have animated with this spell, rather than animating a new one."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 4th level or higher, you animate or reassert control over two additional undead creatures for each slot level above 3rd. Each of the creatures must come from a different corpse or pile of bones.", level:3, material:"A drop of blood, a piece of flesh, and a pinch of bone dust.", range:feet(10), ritual:no, school:necromancy}).
spell_auto_data('animate objects', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, wizard], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Objects come to life at your command. Choose up to ten nonmagical objects within range that are not being worn or carried. Medium targets count as two objects, Large targets count as four objects, Huge targets count as eight objects. You can't animate any object larger than Huge. Each target animates and becomes a creature under your control until the spell ends or until reduced to 0 hit points.", "As a bonus action, you can mentally command any creature you made with this spell if the creature is within 500 feet of you (if you control multiple creatures, you can command any or all of them at the same time, issuing the same command to each one). You decide what action the creature will take and where it will move during its next turn, or you can issue a general command, such as to guard a particular chamber or corridor. If you issue no commands, the creature only defends itself against hostile creatures. Once given an order, the creature continues to follow it until its task is complete.", "Animated Object Statistics", "", "| Size | HP | AC | Attack | Str | Dex |\n |---|---|---|---|---|---|\n | Tiny | 20 | 18 | +8 to hit, 1 d 4 + 4 damage | 4 | 18 |\n | Small | 25 | 16 | +6 to hit, 1 d 8 + 2 damage | 6 | 14 |\n | Medium | 40 | 13 | +5 to hit, 2 d 6 + 1 damage | 10 | 12 |\n | Large | 50 | 10 | +6 to hit, 2 d 10 + 2 damage | 14 | 10 |\n | Huge | 80 | 10 | +8 to hit, 2 d 12 + 4 damage | 18 | 6 |", "An animated object is a construct with AC, hit points, attacks, Strength, and Dexterity determined by its size. Its Constitution is 10 and its Intelligence and Wisdom are 3, and its Charisma is 1. Its speed is 30 feet; if the object lacks legs or other appendages it can use for locomotion, it instead has a flying speed of 30 feet and can hover. If the object is securely attached to a surface or a larger object, such as a chain bolted to a wall, its speed is 0. It has blindsight with a radius of 30 feet and is blind beyond that distance. When the animated object drops to 0 hit points, it reverts to its original object form, and any remaining damage carries over to its original object form.", "If you command an object to attack, it can make a single melee attack against a creature within 5 feet of it. It makes a slam attack with an attack bonus and bludgeoning damage determined by its size. The DM might rule that a specific object inflicts slashing or piercing damage based on its form."], duration:"Up to 1 minute", higher_level:"If you cast this spell using a spell slot of 6th level or higher, you can animate two additional objects for each slot level above 5th.", level:5, material:false, range:feet(120), ritual:no, school:transmutation}).
spell_auto_data('antilife shell', properties{area_of_effect:10 ft sphere, attack_type:false, casting_time:"1 action", classes:[druid], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["A shimmering barrier extends out from you in a 10-foot radius and moves with you, remaining centered on you and hedging out creatures other than undead and constructs. The barrier lasts for the duration.", "The barrier prevents an affected creature from passing or reaching through. An affected creature can cast spells or make attacks with ranged or reach weapons through the barrier.", "If you move so that an affected creature is forced to pass through the barrier, the spell ends."], duration:"Up to 1 hour", higher_level:no, level:5, material:false, range:self, ritual:no, school:abjuration}).
spell_auto_data('antimagic field', properties{area_of_effect:10 ft sphere, attack_type:false, casting_time:"1 action", classes:[cleric, wizard], components:[v, s, m("A pinch of powdered iron or iron filings.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["A 10-foot-radius invisible sphere of antimagic surrounds you. This area is divorced from the magical energy that suffuses the multiverse. Within the sphere, spells can't be cast, summoned creatures disappear, and even magic items become mundane. Until the spell ends, the sphere moves with you, centered on you.", "Spells and other magical effects, except those created by an artifact or a deity, are suppressed in the sphere and can't protrude into it. A slot expended to cast a suppressed spell is consumed. While an effect is suppressed, it doesn't function, but the time it spends suppressed counts against its duration.", "**Targeted Effects.** Spells and other magical effects, such as magic missile and charm person, that target a creature or an object in the sphere have no effect on that target.", "**Areas of Magic.** The area of another spell or magical effect, such as fireball, can't extend into the sphere. If the sphere overlaps an area of magic, the part of the area that is covered by the sphere is suppressed. For example, the flames created by a wall of fire are suppressed within the sphere, creating a gap in the wall if the overlap is large enough.", "**Spells.** Any active spell or other magical effect on a creature or an object in the sphere is suppressed while the creature or object is in it.", "**Magic Items.** The properties and powers of magic items are suppressed in the sphere. For example, a +1 longsword in the sphere functions as a nonmagical longsword.", "A magic weapon's properties and powers are suppressed if it is used against a target in the sphere or wielded by an attacker in the sphere. If a magic weapon or a piece of magic ammunition fully leaves the sphere (for example, if you fire a magic arrow or throw a magic spear at a target outside the sphere), the magic of the item ceases to be suppressed as soon as it exits.", "**Magical Travel.** Teleportation and planar travel fail to work in the sphere, whether the sphere is the destination or the departure point for such magical travel. A portal to another location, world, or plane of existence, as well as an opening to an extradimensional space such as that created by the rope trick spell, temporarily closes while in the sphere.", "**Creatures and Objects.** A creature or object summoned or created by magic temporarily winks out of existence in the sphere. Such a creature instantly reappears once the space the creature occupied is no longer within the sphere.", "**Dispel Magic.** Spells and magical effects such as dispel magic have no effect on the sphere. Likewise, the spheres created by different antimagic field spells don't nullify each other."], duration:"Up to 1 hour", higher_level:no, level:8, material:"A pinch of powdered iron or iron filings.", range:self, ritual:no, school:abjuration}).
spell_auto_data('antipathy/sympathy', properties{area_of_effect:200 ft cube, attack_type:false, casting_time:"1 hour", classes:[druid, wizard], components:[v, s, m("Either a lump of alum soaked in vinegar for the antipathy effect or a drop of honey for the sympathy effect.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["This spell attracts or repels creatures of your choice. You target something within range, either a Huge or smaller object or creature or an area that is no larger than a 200-foot cube. Then specify a kind of intelligent creature, such as red dragons, goblins, or vampires. You invest the target with an aura that either attracts or repels the specified creatures for the duration. Choose antipathy or sympathy as the aura's effect.", "**Antipathy.** The enchantment causes creatures of the kind you designated to feel an intense urge to leave the area and avoid the target. When such a creature can see the target or comes within 60 feet of it, the creature must succeed on a wisdom saving throw or become frightened. The creature remains frightened while it can see the target or is within 60 feet of it. While frightened by the target, the creature must use its movement to move to the nearest safe spot from which it can't see the target. If the creature moves more than 60 feet from the target and can't see it, the creature is no longer frightened, but the creature becomes frightened again if it regains sight of the target or moves within 60 feet of it.", "**Sympathy.** The enchantment causes the specified creatures to feel an intense urge to approach the target while within 60 feet of it or able to see it. When such a creature can see the target or comes within 60 feet of it, the creature must succeed on a wisdom saving throw or use its movement on each of its turns to enter the area or move within reach of the target. When the creature has done so, it can't willingly move away from the target.", "If the target damages or otherwise harms an affected creature, the affected creature can make a wisdom saving throw to end the effect, as described below.", "**Ending the Effect.** If an affected creature ends its turn while not within 60 feet of the target or able to see it, the creature makes a wisdom saving throw. On a successful save, the creature is no longer affected by the target and recognizes the feeling of repugnance or attraction as magical. In addition, a creature affected by the spell is allowed another wisdom saving throw every 24 hours while the spell persists.", "A creature that successfully saves against this effect is immune to it for 1 minute, after which time it can be affected again."], duration:"10 days", higher_level:no, level:8, material:"Either a lump of alum soaked in vinegar for the antipathy effect or a drop of honey for the sympathy effect.", range:feet(60), ritual:no, school:enchantment}).
spell_auto_data('arcane eye', properties{area_of_effect:30 ft sphere, attack_type:false, casting_time:"1 action", classes:[wizard], components:[v, s, m("A bit of bat fur.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You create an invisible, magical eye within range that hovers in the air for the duration.", "You mentally receive visual information from the eye, which has normal vision and darkvision out to 30 feet. The eye can look in every direction.", "As an action, you can move the eye up to 30 feet in any direction. There is no limit to how far away from you the eye can move, but it can't enter another plane of existence. A solid barrier blocks the eye's movement, but the eye can pass through an opening as small as 1 inch in diameter."], duration:"Up to 1 hour", higher_level:no, level:4, material:"A bit of bat fur.", range:feet(30), ritual:no, school:divination}).
spell_auto_data('arcane hand', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[wizard], components:[v, s, m("An eggshell and a snakeskin glove.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You create a Large hand of shimmering, translucent force in an unoccupied space that you can see within range. The hand lasts for the spell's duration, and it moves at your command, mimicking the movements of your own hand.", "The hand is an object that has AC 20 and hit points equal to your hit point maximum. If it drops to 0 hit points, the spell ends. It has a Strength of 26 (+8) and a Dexterity of 10 (+0). The hand doesn't fill its space.", "When you cast the spell and as a bonus action on your subsequent turns, you can move the hand up to 60 feet and then cause one of the following effects with it.", "**Clenched Fist.** The hand strikes one creature or object within 5 feet of it. Make a melee spell attack for the hand using your game statistics. On a hit, the target takes 4 d 8 force damage.", "**Forceful Hand.** The hand attempts to push a creature within 5 feet of it in a direction you choose. Make a check with the hand's Strength contested by the Strength (Athletics) check of the target. If the target is Medium or smaller, you have advantage on the check. If you succeed, the hand pushes the target up to 5 feet plus a number of feet equal to five times your spellcasting ability modifier. The hand moves with the target to remain within 5 feet of it.", "**Grasping Hand.** The hand attempts to grapple a Huge or smaller creature within 5 feet of it. You use the hand's Strength score to resolve the grapple. If the target is Medium or smaller, you have advantage on the check. While the hand is grappling the target, you can use a bonus action to have the hand crush it. When you do so, the target takes bludgeoning damage equal to 2 d 6 + your spellcasting ability modifier.", "**Interposing Hand.** The hand interposes itself between you and a creature you choose until you give the hand a different command. The hand moves to stay between you and the target, providing you with half cover against the target. The target can't move through the hand's space if its Strength score is less than or equal to the hand's Strength score. If its Strength score is higher than the hand's Strength score, the target can move toward you through the hand's space, but that space is difficult terrain for the target."], duration:"Up to 1 minute", higher_level:"When you cast this spell using a spell slot of 6th level or higher, the damage from the clenched fist option increases by 2 d 8 and the damage from the grasping hand increases by 2 d 6 for each slot level above 5th.", level:5, material:"An eggshell and a snakeskin glove.", range:feet(120), ritual:no, school:evocation}).
spell_auto_data('arcane lock', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[wizard], components:[v, s, m("Gold dust worth at least 25gp, which the spell consumes.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch a closed door, window, gate, chest, or other entryway, and it becomes locked for the duration. You and the creatures you designate when you cast this spell can open the object normally. You can also set a password that, when spoken within 5 feet of the object, suppresses this spell for 1 minute. Otherwise, it is impassable until it is broken or the spell is dispelled or suppressed. Casting knock on the object suppresses arcane lock for 10 minutes.", "While affected by this spell, the object is more difficult to break or force open; the DC to break it or pick any locks on it increases by 10."], duration:"Until dispelled", higher_level:no, level:2, material:"Gold dust worth at least 25gp, which the spell consumes.", range:touch, ritual:no, school:abjuration}).
spell_auto_data('arcane sword', properties{area_of_effect:false, attack_type:melee, casting_time:"1 action", classes:[bard, wizard], components:[v, s, m("A miniature platinum sword with a grip and pommel of copper and zinc, worth 250 gp.")], concentration:yes, damage_at_slot_level:_{7:damage(force, 3 d 10)}, damage_with_cantrip_scaling:false, dc:false, desc:["You create a sword-shaped plane of force that hovers within range. It lasts for the duration.", "When the sword appears, you make a melee spell attack against a target of your choice within 5 feet of the sword. On a hit, the target takes 3 d 10 force damage. Until the spell ends, you can use a bonus action on each of your turns to move the sword up to 20 feet to a spot you can see and repeat this attack against the same target or a different one."], duration:"Up to 1 minute", higher_level:no, level:7, material:"A miniature platinum sword with a grip and pommel of copper and zinc, worth 250 gp.", range:feet(60), ritual:no, school:evocation}).
spell_auto_data('arcanist\'s magic aura', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[wizard], components:[v, s, m("A small square of silk.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You place an illusion on a creature or an object you touch so that divination spells reveal false information about it. The target can be a willing creature or an object that isn't being carried or worn by another creature.", "When you cast the spell, choose one or both of the following effects. The effect lasts for the duration. If you cast this spell on the same creature or object every day for 30 days, placing the same effect on it each time, the illusion lasts until it is dispelled.", "**False Aura.** You change the way the target appears to spells and magical effects, such as detect magic, that detect magical auras. You can make a nonmagical object appear magical, a magical object appear nonmagical, or change the object's magical aura so that it appears to belong to a specific school of magic that you choose. When you use this effect on an object, you can make the false magic apparent to any creature that handles the item.", "**Mask.** You change the way the target appears to spells and magical effects that detect creature types, such as a paladin's Divine Sense or the trigger of a symbol spell. You choose a creature type and other spells and magical effects treat the target as if it were a creature of that type or of that alignment."], duration:"24 hours", higher_level:no, level:2, material:"A small square of silk.", range:touch, ritual:no, school:illusion}).
spell_auto_data('astral projection', properties{area_of_effect:false, attack_type:false, casting_time:"1 hour", classes:[cleric, warlock, wizard], components:[v, s, m("For each creature you affect with this spell, you must provide one jacinth worth at least 1,000gp and one ornately carved bar of silver worth at least 100gp, all of which the spell consumes.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You and up to eight willing creatures within range project your astral bodies into the Astral Plane (the spell fails and the casting is wasted if you are already on that plane). The material body you leave behind is unconscious and in a state of suspended animation; it doesn't need food or air and doesn't age.", "Your astral body resembles your mortal form in almost every way, replicating your game statistics and possessions. The principal difference is the addition of a silvery cord that extends from between your shoulder blades and trails behind you, fading to invisibility after 1 foot. This cord is your tether to your material body. As long as the tether remains intact, you can find your way home. If the cord is cut--something that can happen only when an effect specifically states that it does--your soul and body are separated, killing you instantly.", "Your astral form can freely travel through the Astral Plane and can pass through portals there leading to any other plane. If you enter a new plane or return to the plane you were on when casting this spell, your body and possessions are transported along the silver cord, allowing you to re-enter your body as you enter the new plane. Your astral form is a separate incarnation. Any damage or other effects that apply to it have no effect on your physical body, nor do they persist when you return to it.", "The spell ends for you and your companions when you use your action to dismiss it. When the spell ends, the affected creature returns to its physical body, and it awakens.", "The spell might also end early for you or one of your companions. A successful dispel magic spell used against an astral or physical body ends the spell for that creature. If a creature's original body or its astral form drops to 0 hit points, the spell ends for that creature. If the spell ends and the silver cord is intact, the cord pulls the creature's astral form back to its body, ending its state of suspended animation.", "If you are returned to your body prematurely, your companions remain in their astral forms and must find their own way back to their bodies, usually by dropping to 0 hit points."], duration:"Special", higher_level:no, level:9, material:"For each creature you affect with this spell, you must provide one jacinth worth at least 1,000gp and one ornately carved bar of silver worth at least 100gp, all of which the spell consumes.", range:feet(10), ritual:no, school:necromancy}).
spell_auto_data(augury, properties{area_of_effect:false, attack_type:false, casting_time:"1 minute", classes:[cleric], components:[v, s, m("Specially marked sticks, bones, or similar tokens worth at least 25gp.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["By casting gem-inlaid sticks, rolling dragon bones, laying out ornate cards, or employing some other divining tool, you receive an omen from an otherworldly entity about the results of a specific course of action that you plan to take within the next 30 minutes. The DM chooses from the following possible omens:", "- Weal, for good results", "- Woe, for bad results", "- Weal and woe, for both good and bad results", "- Nothing, for results that aren't especially good or bad", "The spell doesn't take into account any possible circumstances that might change the outcome, such as the casting of additional spells or the loss or gain of a companion.", "If you cast the spell two or more times before completing your next long rest, there is a cumulative 25 percent chance for each casting after the first that you get a random reading. The DM makes this roll in secret."], duration:"Instantaneous", higher_level:no, level:2, material:"Specially marked sticks, bones, or similar tokens worth at least 25gp.", range:self, ritual:yes, school:divination}).
spell_auto_data(awaken, properties{area_of_effect:false, attack_type:false, casting_time:"8 hours", classes:[bard, druid], components:[v, s, m("An agate worth at least 1,000 gp, which the spell consumes.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["After spending the casting time tracing magical pathways within a precious gemstone, you touch a Huge or smaller beast or plant. The target must have either no Intelligence score or an Intelligence of 3 or less. The target gains an Intelligence of 10. The target also gains the ability to speak one language you know. If the target is a plant, it gains the ability to move its limbs, roots, vines, creepers, and so forth, and it gains senses similar to a human's. Your DM chooses statistics appropriate for the awakened plant, such as the statistics for the awakened shrub or the awakened tree.", "The awakened beast or plant is charmed by you for 30 days or until you or your companions do anything harmful to it. When the charmed condition ends, the awakened creature chooses whether to remain friendly to you, based on how you treated it while it was charmed."], duration:"Instantaneous", higher_level:no, level:5, material:"An agate worth at least 1,000 gp, which the spell consumes.", range:touch, ritual:no, school:transmutation}).
spell_auto_data(bane, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, cleric], components:[v, s, m("A drop of blood.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:cha else none, desc:["Up to three creatures of your choice that you can see within range must make charisma saving throws. Whenever a target that fails this saving throw makes an attack roll or a saving throw before the spell ends, the target must roll a d4 and subtract the number rolled from the attack roll or saving throw."], duration:"Up to 1 minute", higher_level:"When you cast this spell using a spell slot of 2nd level or higher, you can target one additional creature for each slot level above 1st.", level:1, material:"A drop of blood.", range:feet(30), ritual:no, school:enchantment}).
spell_auto_data(banishment, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric, paladin, sorcerer, warlock, wizard], components:[v, s, m("An item distasteful to the target.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:cha else none, desc:["You attempt to send one creature that you can see within range to another plane of existence. The target must succeed on a charisma saving throw or be banished.", "If the target is native to the plane of existence you're on, you banish the target to a harmless demiplane. While there, the target is incapacitated. The target remains there until the spell ends, at which point the target reappears in the space it left or in the nearest unoccupied space if that space is occupied.", "If the target is native to a different plane of existence than the one you're on, the target is banished with a faint popping noise, returning to its home plane. If the spell ends before 1 minute has passed, the target reappears in the space it left or in the nearest unoccupied space if that space is occupied. Otherwise, the target doesn't return."], duration:"Up to 1 minute", higher_level:"When you cast this spell using a spell slot of 5th level or higher, you can target one additional creature for each slot level above 4th.", level:4, material:"An item distasteful to the target.", range:feet(60), ritual:no, school:abjuration}).
spell_auto_data(barkskin, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[druid, ranger], components:[v, s, m("A handful of oak bark.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch a willing creature. Until the spell ends, the target's skin has a rough, bark-like appearance, and the target's AC can't be less than 16, regardless of what kind of armor it is wearing."], duration:"Up to 1 hour", higher_level:no, level:2, material:"A handful of oak bark.", range:touch, ritual:no, school:transmutation}).
spell_auto_data('beacon of hope', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["This spell bestows hope and vitality. Choose any number of creatures within range. For the duration, each target has advantage on wisdom saving throws and death saving throws, and regains the maximum number of hit points possible from any healing."], duration:"Up to 1 minute", higher_level:no, level:3, material:false, range:feet(30), ritual:no, school:abjuration}).
spell_auto_data('bestow curse', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, cleric, wizard], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else none, desc:["You touch a creature, and that creature must succeed on a wisdom saving throw or become cursed for the duration of the spell. When you cast this spell, choose the nature of the curse from the following options:", "- Choose one ability score. While cursed, the target has disadvantage on ability checks and saving throws made with that ability score.", "- While cursed, the target has disadvantage on attack rolls against you.", "- While cursed, the target must make a wisdom saving throw at the start of each of its turns. If it fails, it wastes its action that turn doing nothing.", "- While the target is cursed, your attacks and spells deal an extra 1 d 8 necrotic damage to the target.", "A remove curse spell ends this effect. At the DM's option, you may choose an alternative curse effect, but it should be no more powerful than those described above. The DM has final say on such a curse's effect."], duration:"Up to 1 minute", higher_level:"If you cast this spell using a spell slot of 4th level or higher, the duration is concentration, up to 10 minutes. If you use a spell slot of 5th level or higher, the duration is 8 hours. If you use a spell slot of 7th level or higher, the duration is 24 hours. If you use a 9th level spell slot, the spell lasts until it is dispelled. Using a spell slot of 5th level or higher grants a duration that doesn't require concentration.", level:3, material:false, range:touch, ritual:no, school:necromancy}).
spell_auto_data('black tentacles', properties{area_of_effect:20 ft cube, attack_type:false, casting_time:"1 action", classes:[wizard], components:[v, s, m("A piece of tentacle from a giant octopus or a giant squid")], concentration:yes, damage_at_slot_level:_{4:damage(bludgeoning, 3 d 6)}, damage_with_cantrip_scaling:false, dc:dex else none, desc:["Squirming, ebony tentacles fill a 20-foot square on ground that you can see within range. For the duration, these tentacles turn the ground in the area into difficult terrain.", "When a creature enters the affected area for the first time on a turn or starts its turn there, the creature must succeed on a Dexterity saving throw or take 3 d 6 bludgeoning damage and be restrained by the tentacles until the spell ends. A creature that starts its turn in the area and is already restrained by the tentacles takes 3 d 6 bludgeoning damage.", "A creature restrained by the tentacles can use its action to make a Strength or Dexterity check (its choice) against your spell save DC. On a success, it frees itself."], duration:"Up to 1 minute", higher_level:no, level:4, material:"A piece of tentacle from a giant octopus or a giant squid", range:feet(90), ritual:no, school:conjuration}).
spell_auto_data('blade barrier', properties{area_of_effect:100 ft line, attack_type:false, casting_time:"1 action", classes:[cleric], components:[v, s], concentration:yes, damage_at_slot_level:_{6:damage(slashing, 6 d 10)}, damage_with_cantrip_scaling:false, dc:dex else half, desc:["You create a vertical wall of whirling, razor-sharp blades made of magical energy. The wall appears within range and lasts for the duration. You can make a straight wall up to 100 feet long, 20 feet high, and 5 feet thick, or a ringed wall up to 60 feet in diameter, 20 feet high, and 5 feet thick. The wall provides three-quarters cover to creatures behind it, and its space is difficult terrain.", "When a creature enters the wall's area for the first time on a turn or starts its turn there, the creature must make a dexterity saving throw. On a failed save, the creature takes 6 d 10 slashing damage. On a successful save, the creature takes half as much damage."], duration:"Up to 10 minutes", higher_level:no, level:6, material:false, range:feet(90), ritual:no, school:evocation}).
spell_auto_data(bless, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric, paladin], components:[v, s, m("A sprinkling of holy water.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You bless up to three creatures of your choice within range. Whenever a target makes an attack roll or a saving throw before the spell ends, the target can roll a d4 and add the number rolled to the attack roll or saving throw."], duration:"Up to 1 minute", higher_level:"When you cast this spell using a spell slot of 2nd level or higher, you can target one additional creature for each slot level above 1st.", level:1, material:"A sprinkling of holy water.", range:feet(30), ritual:no, school:enchantment}).
spell_auto_data(blight, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[druid, sorcerer, warlock, wizard], components:[v, s], concentration:no, damage_at_slot_level:_{4:damage(necrotic, 8 d 8), 5:damage(necrotic, 9 d 8), 6:damage(necrotic, 10 d 8), 7:damage(necrotic, 11 d 8), 8:damage(necrotic, 12 d 8), 9:damage(necrotic, 13 d 8)}, damage_with_cantrip_scaling:false, dc:con else half, desc:["Necromantic energy washes over a creature of your choice that you can see within range, draining moisture and vitality from it. The target must make a constitution saving throw. The target takes 8 d 8 necrotic damage on a failed save, or half as much damage on a successful one. The spell has no effect on undead or constructs.", "If you target a plant creature or a magical plant, it makes the saving throw with disadvantage, and the spell deals maximum damage to it.", "If you target a nonmagical plant that isn't a creature, such as a tree or shrub, it doesn't make a saving throw; it simply withers and dies."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 5th level of higher, the damage increases by 1 d 8 for each slot level above 4th.", level:4, material:false, range:feet(30), ritual:no, school:necromancy}).
spell_auto_data('blindness/deafness', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, cleric, sorcerer, wizard], components:[v], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:con else none, desc:["You can blind or deafen a foe. Choose one creature that you can see within range to make a constitution saving throw. If it fails, the target is either blinded or deafened (your choice) for the duration. At the end of each of its turns, the target can make a constitution saving throw. On a success, the spell ends."], duration:"1 minute", higher_level:"When you cast this spell using a spell slot of 3rd level or higher, you can target one additional creature for each slot level above 2nd.", level:2, material:false, range:feet(30), ritual:no, school:necromancy}).
spell_auto_data(blink, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Roll a d20 at the end of each of your turns for the duration of the spell. On a roll of 11 or higher, you vanish from your current plane of existence and appear in the Ethereal Plane (the spell fails and the casting is wasted if you were already on that plane). At the start of your next turn, and when the spell ends if you are on the Ethereal Plane, you return to an unoccupied space of your choice that you can see within 10 feet of the space you vanished from. If no unoccupied space is available within that range, you appear in the nearest unoccupied space (chosen at random if more than one space is equally near). You can dismiss this spell as an action.", "While on the Ethereal Plane, you can see and hear the plane you originated from, which is cast in shades of gray, and you can't see anything there more than 60 feet away. You can only affect and be affected by other creatures on the Ethereal Plane. Creatures that aren't there can't perceive you or interact with you, unless they have the ability to do so."], duration:"1 minute", higher_level:no, level:3, material:false, range:self, ritual:no, school:transmutation}).
spell_auto_data(blur, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Your body becomes blurred, shifting and wavering to all who can see you. For the duration, any creature has disadvantage on attack rolls against you. An attacker is immune to this effect if it doesn't rely on sight, as with blindsight, or can see through illusions, as with truesight."], duration:"Up to 1 minute", higher_level:no, level:2, material:false, range:self, ritual:no, school:illusion}).
spell_auto_data('branding smite', properties{area_of_effect:false, attack_type:false, casting_time:"1 bonus action", classes:[paladin], components:[v], concentration:yes, damage_at_slot_level:_{2:damage(radiant, 2 d 6), 3:damage(radiant, 3 d 6), 4:damage(radiant, 4 d 6), 5:damage(radiant, 5 d 6), 6:damage(radiant, 6 d 6), 7:damage(radiant, 7 d 6), 8:damage(radiant, 8 d 6), 9:damage(radiant, 9 d 6)}, damage_with_cantrip_scaling:false, dc:false, desc:["The next time you hit a creature with a weapon attack before this spell ends, the weapon gleams with astral radiance as you strike. The attack deals an extra 2 d 6 radiant damage to the target, which becomes visible if it's invisible, and the target sheds dim light in a 5-foot radius and can't become invisible until the spell ends."], duration:"Up to 1 minute", higher_level:"When you cast this spell using a spell slot of 3rd level or higher, the extra damage increases by 1 d 6 for each slot level above 2nd.", level:2, material:false, range:self, ritual:no, school:evocation}).
spell_auto_data('burning hands', properties{area_of_effect:15 ft cone, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s], concentration:no, damage_at_slot_level:_{1:damage(fire, 3 d 6), 2:damage(fire, 4 d 6), 3:damage(fire, 5 d 6), 4:damage(fire, 6 d 6), 5:damage(fire, 7 d 6), 6:damage(fire, 8 d 6), 7:damage(fire, 9 d 6), 8:damage(fire, 10 d 6), 9:damage(fire, 11 d 6)}, damage_with_cantrip_scaling:false, dc:dex else half, desc:["As you hold your hands with thumbs touching and fingers spread, a thin sheet of flames shoots forth from your outstretched fingertips. Each creature in a 15-foot cone must make a dexterity saving throw. A creature takes 3 d 6 fire damage on a failed save, or half as much damage on a successful one.", "The fire ignites any flammable objects in the area that aren't being worn or carried."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 2nd level or higher, the damage increases by 1 d 6 for each slot level above 1st.", level:1, material:false, range:self, ritual:no, school:evocation}).
spell_auto_data('call lightning', properties{area_of_effect:5 ft sphere, attack_type:false, casting_time:"1 action", classes:[druid], components:[v, s], concentration:yes, damage_at_slot_level:_{3:damage(lightning, 3 d 10), 4:damage(lightning, 4 d 10), 5:damage(lightning, 5 d 10), 6:damage(lightning, 6 d 10), 7:damage(lightning, 7 d 10), 8:damage(lightning, 8 d 10), 9:damage(lightning, 9 d 10)}, damage_with_cantrip_scaling:false, dc:false, desc:["A storm cloud appears in the shape of a cylinder that is 10 feet tall with a 60-foot radius, centered on a point you can see 100 feet directly above you. The spell fails if you can't see a point in the air where the storm cloud could appear (for example, if you are in a room that can't accommodate the cloud).", "When you cast the spell, choose a point you can see within range. A bolt of lightning flashes down from the cloud to that point. Each creature within 5 feet of that point must make a dexterity saving throw. A creature takes 3 d 10 lightning damage on a failed save, or half as much damage on a successful one. On each of your turns until the spell ends, you can use your action to call down lightning in this way again, targeting the same point or a different one.", "If you are outdoors in stormy conditions when you cast this spell, the spell gives you control over the existing storm instead of creating a new one. Under such conditions, the spell's damage increases by 1 d 10."], duration:"Up to 10 minutes", higher_level:"When you cast this spell using a spell slot of 4th or higher level, the damage increases by 1 d 10 for each slot level above 3rd.", level:3, material:false, range:feet(120), ritual:no, school:conjuration}).
spell_auto_data('calm emotions', properties{area_of_effect:20 ft sphere, attack_type:false, casting_time:"1 action", classes:[bard, cleric], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:cha else none, desc:["You attempt to suppress strong emotions in a group of people. Each humanoid in a 20-foot-radius sphere centered on a point you choose within range must make a charisma saving throw; a creature can choose to fail this saving throw if it wishes. If a creature fails its saving throw, choose one of the following two effects. You can suppress any effect causing a target to be charmed or frightened. When this spell ends, any suppressed effect resumes, provided that its duration has not expired in the meantime.", "Alternatively, you can make a target indifferent about creatures of your choice that it is hostile toward. This indifference ends if the target is attacked or harmed by a spell or if it witnesses any of its friends being harmed. When the spell ends, the creature becomes hostile again, unless the DM rules otherwise."], duration:"Up to 1 minute", higher_level:no, level:2, material:false, range:feet(60), ritual:no, school:enchantment}).
spell_auto_data('chain lightning', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s, m("A bit of fur; a piece of amber, glass, or a crystal rod; and three silver pins.")], concentration:no, damage_at_slot_level:_{6:damage(lightning, 10 d 8)}, damage_with_cantrip_scaling:false, dc:dex else half, desc:["You create a bolt of lightning that arcs toward a target of your choice that you can see within range. Three bolts then leap from that target to as many as three other targets, each of which must be within 30 feet of the first target. A target can be a creature or an object and can be targeted by only one of the bolts.", "A target must make a dexterity saving throw. The target takes 10 d 8 lightning damage on a failed save, or half as much damage on a successful one."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 7th level or higher, one additional bolt leaps from the first target to another target for each slot level above 6th.", level:6, material:"A bit of fur; a piece of amber, glass, or a crystal rod; and three silver pins.", range:feet(150), ritual:no, school:evocation}).
spell_auto_data('charm person', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, druid, sorcerer, warlock, wizard], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else none, desc:["You attempt to charm a humanoid you can see within range. It must make a wisdom saving throw, and does so with advantage if you or your companions are fighting it. If it fails the saving throw, it is charmed by you until the spell ends or until you or your companions do anything harmful to it. The charmed creature regards you as a friendly acquaintance. When the spell ends, the creature knows it was charmed by you."], duration:"1 hour", higher_level:"When you cast this spell using a spell slot of 2nd level or higher, you can target one additional creature for each slot level above 1st. The creatures must be within 30 feet of each other when you target them.", level:1, material:false, range:feet(30), ritual:no, school:enchantment}).
spell_auto_data('chill touch', properties{area_of_effect:false, attack_type:ranged, casting_time:"1 action", classes:[sorcerer, warlock, wizard], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:damage(necrotic, 1 d 8), dc:false, desc:["You create a ghostly, skeletal hand in the space of a creature within range. Make a ranged spell attack against the creature to assail it with the chill of the grave. On a hit, the target takes 1 d 8 necrotic damage, and it can't regain hit points until the start of your next turn. Until then, the hand clings to the target.", "If you hit an undead target, it also has disadvantage on attack rolls against you until the end of your next turn.", "This spell's damage increases by 1 d 8 when you reach 5th level (2 d 8), 11th level (3 d 8), and 17th level (4 d 8)."], duration:"1 round", higher_level:no, level:0, material:false, range:feet(120), ritual:no, school:necromancy}).
spell_auto_data('circle of death', properties{area_of_effect:60 ft sphere, attack_type:false, casting_time:"1 action", classes:[sorcerer, warlock, wizard], components:[v, s, m("The powder of a crushed black pearl worth at least 500 gp.")], concentration:no, damage_at_slot_level:_{6:damage(necrotic, 8 d 6), 7:damage(necrotic, 10 d 6), 8:damage(necrotic, 12 d 6), 9:damage(necrotic, 14 d 6)}, damage_with_cantrip_scaling:false, dc:con else half, desc:["A sphere of negative energy ripples out in a 60-foot radius sphere from a point within range. Each creature in that area must make a constitution saving throw. A target takes 8 d 6 necrotic damage on a failed save, or half as much damage on a successful one."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 7th level or higher, the damage increases by 2 d 6 for each slot level above 6th.", level:6, material:"The powder of a crushed black pearl worth at least 500 gp.", range:feet(150), ritual:no, school:necromancy}).
spell_auto_data(clairvoyance, properties{area_of_effect:false, attack_type:false, casting_time:"10 minutes", classes:[bard, cleric, sorcerer, wizard], components:[v, s, m("A focus worth at least 100gp, either a jeweled horn for hearing or a glass eye for seeing.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You create an invisible sensor within range in a location familiar to you (a place you have visited or seen before) or in an obvious location that is unfamiliar to you (such as behind a door, around a corner, or in a grove of trees). The sensor remains in place for the duration, and it can't be attacked or otherwise interacted with.", "When you cast the spell, you choose seeing or hearing. You can use the chosen sense through the sensor as if you were in its space. As your action, you can switch between seeing and hearing.", "A creature that can see the sensor (such as a creature benefiting from see invisibility or truesight) sees a luminous, intangible orb about the size of your fist."], duration:"Up to 10 minutes", higher_level:no, level:3, material:"A focus worth at least 100gp, either a jeweled horn for hearing or a glass eye for seeing.", range:miles(1), ritual:no, school:divination}).
spell_auto_data(clone, properties{area_of_effect:false, attack_type:false, casting_time:"1 hour", classes:[wizard], components:[v, s, m("A diamond worth at least 1,000 gp and at least 1 cubic inch of flesh of the creature that is to be cloned, which the spell consumes, and a vessel worth at least 2,000 gp that has a sealable lid and is large enough to hold a Medium creature, such as a huge urn, coffin, mud-filled cyst in the ground, or crystal container filled with salt water.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["This spell grows an inert duplicate of a living creature as a safeguard against death. This clone forms inside a sealed vessel and grows to full size and maturity after 120 days; you can also choose to have the clone be a younger version of the same creature. It remains inert and endures indefinitely, as long as its vessel remains undisturbed.", "At any time after the clone matures, if the original creature dies, its soul transfers to the clone, provided that the soul is free and willing to return. The clone is physically identical to the original and has the same personality, memories, and abilities, but none of the original's equipment. The original creature's physical remains, if they still exist, become inert and can't thereafter be restored to life, since the creature's soul is elsewhere."], duration:"Instantaneous", higher_level:no, level:8, material:"A diamond worth at least 1,000 gp and at least 1 cubic inch of flesh of the creature that is to be cloned, which the spell consumes, and a vessel worth at least 2,000 gp that has a sealable lid and is large enough to hold a Medium creature, such as a huge urn, coffin, mud-filled cyst in the ground, or crystal container filled with salt water.", range:touch, ritual:no, school:necromancy}).
spell_auto_data(cloudkill, properties{area_of_effect:20 ft sphere, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s], concentration:yes, damage_at_slot_level:_{5:damage(poison, 5 d 8), 6:damage(poison, 6 d 8), 7:damage(poison, 7 d 8), 8:damage(poison, 8 d 8), 9:damage(poison, 9 d 8)}, damage_with_cantrip_scaling:false, dc:con else half, desc:["You create a 20-foot-radius sphere of poisonous, yellow-green fog centered on a point you choose within range. The fog spreads around corners. It lasts for the duration or until strong wind disperses the fog, ending the spell. Its area is heavily obscured.", "When a creature enters the spell's area for the first time on a turn or starts its turn there, that creature must make a constitution saving throw. The creature takes 5 d 8 poison damage on a failed save, or half as much damage on a successful one. Creatures are affected even if they hold their breath or don't need to breathe.", "The fog moves 10 feet away from you at the start of each of your turns, rolling along the surface of the ground. The vapors, being heavier than air, sink to the lowest level of the land, even pouring down openings."], duration:"Up to 10 minutes", higher_level:"When you cast this spell using a spell slot of 6th level or higher, the damage increases by 1 d 8 for each slot level above 5th.", level:5, material:false, range:feet(120), ritual:no, school:conjuration}).
spell_auto_data('color spray', properties{area_of_effect:15 ft cone, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s, m("A pinch of powder or sand that is colored red, yellow, and blue.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["A dazzling array of flashing, colored light springs from your hand. Roll 6 d 10; the total is how many hit points of creatures this spell can effect. Creatures in a 15-foot cone originating from you are affected in ascending order of their current hit points (ignoring unconscious creatures and creatures that can't see).", "Starting with the creature that has the lowest current hit points, each creature affected by this spell is blinded until the spell ends. Subtract each creature's hit points from the total before moving on to the creature with the next lowest hit points. A creature's hit points must be equal to or less than the remaining total for that creature to be affected."], duration:"1 round", higher_level:"When you cast this spell using a spell slot of 2nd level or higher, roll an additional 2 d 10 for each slot level above 1st.", level:1, material:"A pinch of powder or sand that is colored red, yellow, and blue.", range:self, ritual:no, school:illusion}).
spell_auto_data(command, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric, paladin], components:[v], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else none, desc:["You speak a one-word command to a creature you can see within range. The target must succeed on a wisdom saving throw or follow the command on its next turn. The spell has no effect if the target is undead, if it doesn't understand your language, or if your command is directly harmful to it.", "Some typical commands and their effects follow. You might issue a command other than one described here. If you do so, the DM determines how the target behaves. If the target can't follow your command, the spell ends.", "**Approach.** The target moves toward you by the shortest and most direct route, ending its turn if it moves within 5 feet of you.", "**Drop.** The target drops whatever it is holding and then ends its turn.", "**Flee.** The target spends its turn moving away from you by the fastest available means.", "**Grovel.** The target falls prone and then ends its turn.", "**Halt.** The target doesn't move and takes no actions. A flying creature stays aloft, provided that it is able to do so. If it must move to stay aloft, it flies the minimum distance needed to remain in the air."], duration:"1 round", higher_level:"When you cast this spell using a spell slot of 2nd level or higher, you can affect one additional creature for each slot level above 1st. The creatures must be within 30 feet of each other when you target them.", level:1, material:false, range:feet(60), ritual:no, school:enchantment}).
spell_auto_data(commune, properties{area_of_effect:false, attack_type:false, casting_time:"1 minute", classes:[cleric], components:[v, s, m("Incense and a vial of holy or unholy water.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You contact your deity or a divine proxy and ask up to three questions that can be answered with a yes or no. You must ask your questions before the spell ends. You receive a correct answer for each question.", "Divine beings aren't necessarily omniscient, so you might receive \"unclear\" as an answer if a question pertains to information that lies beyond the deity's knowledge. In a case where a one-word answer could be misleading or contrary to the deity's interests, the DM might offer a short phrase as an answer instead.", "If you cast the spell two or more times before finishing your next long rest, there is a cumulative 25 percent chance for each casting after the first that you get no answer. The DM makes this roll in secret."], duration:"1 minute", higher_level:no, level:5, material:"Incense and a vial of holy or unholy water.", range:self, ritual:yes, school:divination}).
spell_auto_data('commune with nature', properties{area_of_effect:false, attack_type:false, casting_time:"1 minute", classes:[druid, ranger], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You briefly become one with nature and gain knowledge of the surrounding territory. In the outdoors, the spell gives you knowledge of the land within 3 miles of you. In caves and other natural underground settings, the radius is limited to 300 feet. The spell doesn't function where nature has been replaced by construction, such as in dungeons and towns.", "You instantly gain knowledge of up to three facts of your choice about any of the following subjects as they relate to the area:", "- terrain and bodies of water", "- prevalent plants, minerals, animals, or peoples", "- powerful celestials, fey, fiends, elementals, or undead", "- influence from other planes of existence", "- buildings", "For example, you could determine the location of powerful undead in the area, the location of major sources of safe drinking water, and the location of any nearby towns."], duration:"Instantaneous", higher_level:no, level:5, material:false, range:self, ritual:yes, school:divination}).
spell_auto_data('comprehend languages', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, warlock, wizard], components:[v, s, m("A pinch of soot and salt.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["For the duration, you understand the literal meaning of any spoken language that you hear. You also understand any written language that you see, but you must be touching the surface on which the words are written. It takes about 1 minute to read one page of text.", "This spell doesn't decode secret messages in a text or a glyph, such as an arcane sigil, that isn't part of a written language."], duration:"1 hour", higher_level:no, level:1, material:"A pinch of soot and salt.", range:self, ritual:yes, school:divination}).
spell_auto_data(compulsion, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else none, desc:["Creatures of your choice that you can see within range and that can hear you must make a wisdom saving throw. A target automatically succeeds on this saving throw if it can't be charmed. On a failed save, a target is affected by this spell. Until the spell ends, you can use a bonus action on each of your turns to designate a direction that is horizontal to you. Each affected target must use as much of its movement as possible to move in that direction on its next turn. It can take any action before it moves. After moving in this way, it can make another Wisdom save to try to end the effect.", "A target isn't compelled to move into an obviously deadly hazard, such as a fire or a pit, but it will provoke opportunity attacks to move in the designated direction."], duration:"Up to 1 minute", higher_level:no, level:4, material:false, range:feet(30), ritual:no, school:enchantment}).
spell_auto_data('cone of cold', properties{area_of_effect:60 ft cone, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s, m("A small crystal or glass cone.")], concentration:no, damage_at_slot_level:_{5:damage(cold, 8 d 8), 6:damage(cold, 9 d 8), 7:damage(cold, 10 d 8), 8:damage(cold, 11 d 8), 9:damage(cold, 12 d 8)}, damage_with_cantrip_scaling:false, dc:con else half, desc:["A blast of cold air erupts from your hands. Each creature in a 60-foot cone must make a constitution saving throw. A creature takes 8 d 8 cold damage on a failed save, or half as much damage on a successful one.", "A creature killed by this spell becomes a frozen statue until it thaws."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 6th level or higher, the damage increases by 1 d 8 for each slot level above 5th.", level:5, material:"A small crystal or glass cone.", range:self, ritual:no, school:evocation}).
spell_auto_data(confusion, properties{area_of_effect:10 ft sphere, attack_type:false, casting_time:"1 action", classes:[bard, druid, sorcerer, wizard], components:[v, s, m("Three walnut shells.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else none, desc:["This spell assaults and twists creatures' minds, spawning delusions and provoking uncontrolled action. Each creature in a 10-foot-radius sphere centered on a point you choose within range must succeed on a Wisdom saving throw when you cast this spell or be affected by it.", "An affected target can't take reactions and must roll a d10 at the start of each of its turns to determine its behavior for that turn.", "| d10 | Behavior |\n |---|---|\n | 1 | The creature uses all its movement to move in a random direction. To determine the direction, roll a d8 and assign a direction to each die face. The creature doesn't take an action this turn. |\n | 2-6 | The creature doesn't move or take actions this turn. |\n | 7-8 | The creature uses its action to make a melee attack against a randomly determined creature within its reach. If there is no creature within its reach, the creature does nothing this turn. |\n | 9-10 | The creature can act and move normally. |", "At the end of each of its turns, an affected target can make a Wisdom saving throw. If it succeeds, this effect ends for that target."], duration:"Up to 1 minute", higher_level:"When you cast this spell using a spell slot of 5th level or higher, the radius of the sphere increases by 5 feet for each slot level above 4th.", level:4, material:"Three walnut shells.", range:feet(90), ritual:no, school:enchantment}).
spell_auto_data('conjure animals', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[druid, ranger], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You summon fey spirits that take the form of beasts and appear in unoccupied spaces that you can see within range. Choose one of the following options for what appears:", "- One beast of challenge rating 2 or lower", "- Two beasts of challenge rating 1 or lower", "- Four beasts of challenge rating 1/2 or lower", "- Eight beasts of challenge rating 1/4 or lower", "- Each beast is also considered fey, and it disappears when it drops to 0 hit points or when the spell ends.", "The summoned creatures are friendly to you and your companions. Roll initiative for the summoned creatures as a group, which has its own turns. They obey any verbal commands that you issue to them (no action required by you). If you don't issue any commands to them, they defend themselves from hostile creatures, but otherwise take no actions.", "The DM has the creatures' statistics."], duration:"Up to 1 hour", higher_level:"When you cast this spell using certain higher-level spell slots, you choose one of the summoning options above, and more creatures appear: twice as many with a 5th-level slot, three times as many with a 7th-level.", level:3, material:false, range:feet(60), ritual:no, school:conjuration}).
spell_auto_data('conjure celestial', properties{area_of_effect:false, attack_type:false, casting_time:"1 minute", classes:[cleric], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You summon a celestial of challenge rating 4 or lower, which appears in an unoccupied space that you can see within range. The celestial disappears when it drops to 0 hit points or when the spell ends.", "The celestial is friendly to you and your companions for the duration. Roll initiative for the celestial, which has its own turns. It obeys any verbal commands that you issue to it (no action required by you), as long as they don't violate its alignment. If you don't issue any commands to the celestial, it defends itself from hostile creatures but otherwise takes no actions.", "The DM has the celestial's statistics."], duration:"Up to 1 hour", higher_level:"When you cast this spell using a 9th-level spell slot, you summon a celestial of challenge rating 5 or lower.", level:7, material:false, range:feet(90), ritual:no, school:conjuration}).
spell_auto_data('conjure elemental', properties{area_of_effect:10 ft cube, attack_type:false, casting_time:"1 minute", classes:[druid, wizard], components:[v, s, m("Burning incense for air, soft clay for earth, sulfur and phosphorus for fire, or water and sand for water.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You call forth an elemental servant. Choose an area of air, earth, fire, or water that fills a 10-foot cube within range. An elemental of challenge rating 5 or lower appropriate to the area you chose appears in an unoccupied space within 10 feet of it. For example, a fire elemental emerges from a bonfire, and an earth elemental rises up from the ground. The elemental disappears when it drops to 0 hit points or when the spell ends.", "The elemental is friendly to you and your companions for the duration. Roll initiative for the elemental, which has its own turns. It obeys any verbal commands that you issue to it (no action required by you). If you don't issue any commands to the elemental, it defends itself from hostile creatures but otherwise takes no actions.", "If your concentration is broken, the elemental doesn't disappear. Instead, you lose control of the elemental, it becomes hostile toward you and your companions, and it might attack. An uncontrolled elemental can't be dismissed by you, and it disappears 1 hour after you summoned it.", "The DM has the elemental's statistics."], duration:"Up to 1 hour", higher_level:"When you cast this spell using a spell slot of 6th level or higher, the challenge rating increases by 1 for each slot level above 5th.", level:5, material:"Burning incense for air, soft clay for earth, sulfur and phosphorus for fire, or water and sand for water.", range:feet(90), ritual:no, school:conjuration}).
spell_auto_data('conjure fey', properties{area_of_effect:false, attack_type:false, casting_time:"1 minute", classes:[druid, warlock], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You summon a fey creature of challenge rating 6 or lower, or a fey spirit that takes the form of a beast of challenge rating 6 or lower. It appears in an unoccupied space that you can see within range. The fey creature disappears when it drops to 0 hit points or when the spell ends.", "The fey creature is friendly to you and your companions for the duration. Roll initiative for the creature, which has its own turns. It obeys any verbal commands that you issue to it (no action required by you), as long as they don't violate its alignment. If you don't issue any commands to the fey creature, it defends itself from hostile creatures but otherwise takes no actions.", "If your concentration is broken, the fey creature doesn't disappear. Instead, you lose control of the fey creature, it becomes hostile toward you and your companions, and it might attack. An uncontrolled fey creature can't be dismissed by you, and it disappears 1 hour after you summoned it.", "The DM has the fey creature's statistics."], duration:"Up to 1 hour", higher_level:"When you cast this spell using a spell slot of 7th level or higher, the challenge rating increases by 1 for each slot level above 6th.", level:6, material:false, range:feet(90), ritual:no, school:conjuration}).
spell_auto_data('conjure minor elementals', properties{area_of_effect:false, attack_type:false, casting_time:"1 minute", classes:[druid, wizard], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You summon elementals that appear in unoccupied spaces that you can see within range. You choose one the following options for what appears:", "- One elemental of challenge rating 2 or lower", "- Two elementals of challenge rating 1 or lower", "- Four elementals of challenge rating 1/2 or lower", "- Eight elementals of challenge rating 1/4 or lower.", "An elemental summoned by this spell disappears when it drops to 0 hit points or when the spell ends.", "The summoned creatures are friendly to you and your companions. Roll initiative for the summoned creatures as a group, which has its own turns. They obey any verbal commands that you issue to them (no action required by you). If you don't issue any commands to them, they defend themselves from hostile creatures, but otherwise take no actions.", "The DM has the creatures' statistics."], duration:"Up to 1 hour", higher_level:"When you cast this spell using certain higher-level spell slots, you choose one of the summoning options above, and more creatures appear: twice as many with a 6th-level slot and three times as many with an 8th-level slot.", level:4, material:false, range:feet(90), ritual:no, school:conjuration}).
spell_auto_data('conjure woodland beings', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[druid, ranger], components:[v, s, m("One holly berry per creature summoned.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You summon fey creatures that appear in unoccupied spaces that you can see within range. Choose one of the following options for what appears:", "- One fey creature of challenge rating 2 or lower", "- Two fey creatures of challenge rating 1 or lower", "- Four fey creatures of challenge rating 1/2 or lower", "- Eight fey creatures of challenge rating 1/4 or lower", "A summoned creature disappears when it drops to 0 hit points or when the spell ends.", "The summoned creatures are friendly to you and your companions. Roll initiative for the summoned creatures as a group, which have their own turns. They obey any verbal commands that you issue to them (no action required by you). If you don't issue any commands to them, they defend themselves from hostile creatures, but otherwise take no actions.", "The DM has the creatures' statistics."], duration:"Up to 1 hour", higher_level:"When you cast this spell using certain higher-level spell slots, you choose one of the summoning options above, and more creatures appear: twice as many with a 6th-level slot and three times as many with an 8th-level slot.", level:4, material:"One holly berry per creature summoned.", range:feet(60), ritual:no, school:conjuration}).
spell_auto_data('contact other plane', properties{area_of_effect:false, attack_type:false, casting_time:"1 minute", classes:[warlock, wizard], components:[v], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:int else other, desc:["You mentally contact a demigod, the spirit of a long-dead sage, or some other mysterious entity from another plane. Contacting this extraplanar intelligence can strain or even break your mind. When you cast this spell, make a DC 15 intelligence saving throw. On a failure, you take 6 d 6 psychic damage and are insane until you finish a long rest. While insane, you can't take actions, can't understand what other creatures say, can't read, and speak only in gibberish. A greater restoration spell cast on you ends this effect.", "On a successful save, you can ask the entity up to five questions. You must ask your questions before the spell ends. The DM answers each question with one word, such as \"yes,\" \"no,\" \"maybe,\" \"never,\" \"irrelevant,\" or \"unclear\" (if the entity doesn't know the answer to the question). If a one-word answer would be misleading, the DM might instead offer a short phrase as an answer."], duration:"1 minute", higher_level:no, level:5, material:false, range:self, ritual:yes, school:divination}).
spell_auto_data(contagion, properties{area_of_effect:false, attack_type:melee, casting_time:"1 action", classes:[cleric, druid], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:con else other, desc:["Your touch inflicts disease. Make a melee spell attack against a creature within your reach. On a hit, you afflict the creature with a disease of your choice from any of the ones described below.", "At the end of each of the target's turns, it must make a constitution saving throw. After failing three of these saving throws, the disease's effects last for the duration, and the creature stops making these saves. After succeeding on three of these saving throws, the creature recovers from the disease, and the spell ends.", "Since this spell induces a natural disease in its target, any effect that removes a disease or otherwise ameliorates a disease's effects apply to it.", "**Blinding Sickness.** Pain grips the creature's mind, and its eyes turn milky white. The creature has disadvantage on wisdom checks and wisdom saving throws and is blinded.", "**Filth Fever.** A raging fever sweeps through the creature's body. The creature has disadvantage on strength checks, strength saving throws, and attack rolls that use Strength.", "**Flesh Rot.** The creature's flesh decays. The creature has disadvantage on Charisma checks and vulnerability to all damage.", "**Mindfire.** The creature's mind becomes feverish. The creature has disadvantage on intelligence checks and intelligence saving throws, and the creature behaves as if under the effects of the confusion spell during combat.", "**Seizure.** The creature is overcome with shaking. The creature has disadvantage on dexterity checks, dexterity saving throws, and attack rolls that use Dexterity.", "**Slimy Doom.** The creature begins to bleed uncontrollably. The creature has disadvantage on constitution checks and constitution saving throws. In addition, whenever the creature takes damage, it is stunned until the end of its next turn."], duration:"7 days", higher_level:no, level:5, material:false, range:touch, ritual:no, school:necromancy}).
spell_auto_data(contingency, properties{area_of_effect:false, attack_type:false, casting_time:"10 minutes", classes:[wizard], components:[v, s, m("A statuette of yourself carved from ivory and decorated with gems worth at least 1,500 gp.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Choose a spell of 5th level or lower that you can cast, that has a casting time of 1 action, and that can target you. You cast that spell--called the contingent spell--as part of casting contingency, expending spell slots for both, but the contingent spell doesn't come into effect. Instead, it takes effect when a certain circumstance occurs. You describe that circumstance when you cast the two spells. For example, a contingency cast with water breathing might stipulate that water breathing comes into effect when you are engulfed in water or a similar liquid.", "The contingent spell takes effect immediately after the circumstance is met for the first time, whether or not you want it to. and then contingency ends.", "The contingent spell takes effect only on you, even if it can normally target others. You can use only one contingency spell at a time. If you cast this spell again, the effect of another contingency spell on you ends. Also, contingency ends on you if its material component is ever not on your person."], duration:"10 days", higher_level:no, level:6, material:"A statuette of yourself carved from ivory and decorated with gems worth at least 1,500 gp.", range:self, ritual:no, school:evocation}).
spell_auto_data('continual flame', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric, wizard], components:[v, s, m("Ruby dust worth 50 gp, which the spell consumes.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["A flame, equivalent in brightness to a torch, springs forth from an object that you touch. The effect looks like a regular flame, but it creates no heat and doesn't use oxygen. A continual flame can be covered or hidden but not smothered or quenched."], duration:"Until dispelled", higher_level:no, level:2, material:"Ruby dust worth 50 gp, which the spell consumes.", range:touch, ritual:no, school:evocation}).
spell_auto_data('control water', properties{area_of_effect:100 ft cube, attack_type:false, casting_time:"1 action", classes:[cleric, druid, wizard], components:[v, s, m("A drop of water and a pinch of dust.")], concentration:yes, damage_at_slot_level:_{4:damage(bludgeoning, 2 d 8)}, damage_with_cantrip_scaling:false, dc:str else half, desc:["Until the spell ends, you control any freestanding water inside an area you choose that is a cube up to 100 feet on a side. You can choose from any of the following effects when you cast this spell. As an action on your turn, you can repeat the same effect or choose a different one.", "**Flood.** You cause the water level of all standing water in the area to rise by as much as 20 feet. If the area includes a shore, the flooding water spills over onto dry land.", "If you choose an area in a large body of water, you instead create a 20-foot tall wave that travels from one side of the area to the other and then crashes down. Any Huge or smaller vehicles in the wave's path are carried with it to the other side. Any Huge or smaller vehicles struck by the wave have a 25 percent chance of capsizing.", "The water level remains elevated until the spell ends or you choose a different effect. If this effect produced a wave, the wave repeats on the start of your next turn while the flood effect lasts.", "**Part Water.** You cause water in the area to move apart and create a trench. The trench extends across the spell's area, and the separated water forms a wall to either side. The trench remains until the spell ends or you choose a different effect. The water then slowly fills in the trench over the course of the next round until the normal water level is restored.", "**Redirect Flow.** You cause flowing water in the area to move in a direction you choose, even if the water has to flow over obstacles, up walls, or in other unlikely directions. The water in the area moves as you direct it, but once it moves beyond the spell's area, it resumes its flow based on the terrain conditions. The water continues to move in the direction you chose until the spell ends or you choose a different effect.", "**Whirlpool.** This effect requires a body of water at least 50 feet square and 25 feet deep. You cause a whirlpool to form in the center of the area. The whirlpool forms a vortex that is 5 feet wide at the base, up to 50 feet wide at the top, and 25 feet tall. Any creature or object in the water and within 25 feet of the vortex is pulled 10 feet toward it. A creature can swim away from the vortex by making a Strength (Athletics) check against your spell save DC.", "When a creature enters the vortex for the first time on a turn or starts its turn there, it must make a strength saving throw. On a failed save, the creature takes 2 d 8 bludgeoning damage and is caught in the vortex until the spell ends. On a successful save, the creature takes half damage, and isn't caught in the vortex. A creature caught in the vortex can use its action to try to swim away from the vortex as described above, but has disadvantage on the Strength (Athletics) check to do so.", "The first time each turn that an object enters the vortex, the object takes 2 d 8 bludgeoning damage; this damage occurs each round it remains in the vortex."], duration:"Up to 10 minutes", higher_level:no, level:4, material:"A drop of water and a pinch of dust.", range:feet(300), ritual:no, school:transmutation}).
spell_auto_data('control weather', properties{area_of_effect:false, attack_type:false, casting_time:"10 minutes", classes:[cleric, druid, wizard], components:[v, s, m("Burning incense and bits of earth and wood mixed in water.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You take control of the weather within 5 miles of you for the duration. You must be outdoors to cast this spell. Moving to a place where you don't have a clear path to the sky ends the spell early.", "When you cast the spell, you change the current weather conditions, which are determined by the DM based on the climate and season. You can change precipitation, temperature, and wind. It takes 1 d 4 x 10 minutes for the new conditions to take effect. Once they do so, you can change the conditions again. When the spell ends, the weather gradually returns to normal.", "When you change the weather conditions, find a current condition on the following tables and change its stage by one, up or down. When changing the wind, you can change its direction.", "**Precipitation**", "| Stage | Condition |\n |---|---|\n | 1 | Clear |\n | 2 | Light clouds |\n | 3 | Overcast or ground fog |\n | 4 | Rain, hail, or snow |\n | 5 | Torrential rain, driving hail, or blizzard |", "**Temperature**", "| Stage | Condition |\n |---|---|\n | 1 | Unbearable heat |\n | 2 | Hot |\n | 3 | Warm |\n | 4 | Cool |\n | 5 | Cold |\n | 6 | Arctic cold |", "**Wind**", "| Stage | Condition |\n |---|---|\n | 1 | Calm |\n | 2 | Moderate wind |\n | 3 | Strong wind |\n | 4 | Gale |\n | 5 | Storm |"], duration:"Up to 8 hours", higher_level:no, level:8, material:"Burning incense and bits of earth and wood mixed in water.", range:self, ritual:no, school:transmutation}).
spell_auto_data(counterspell, properties{area_of_effect:false, attack_type:false, casting_time:"1 reaction", classes:[sorcerer, warlock, wizard], components:[s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You attempt to interrupt a creature in the process of casting a spell. If the creature is casting a spell of 3rd level or lower, its spell fails and has no effect. If it is casting a spell of 4th level or higher, make an ability check using your spellcasting ability. The DC equals 10 + the spell's level. On a success, the creature's spell fails and has no effect."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 4th level or higher, the interrupted spell has no effect if its level is less than or equal to the level of the spell slot you used.", level:3, material:false, range:feet(60), ritual:no, school:abjuration}).
spell_auto_data('create food and water', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric, druid, paladin], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You create 45 pounds of food and 30 gallons of water on the ground or in containers within range, enough to sustain up to fifteen humanoids or five steeds for 24 hours. The food is bland but nourishing, and spoils if uneaten after 24 hours. The water is clean and doesn't go bad."], duration:"Instantaneous", higher_level:no, level:3, material:false, range:feet(30), ritual:no, school:conjuration}).
spell_auto_data('create or destroy water', properties{area_of_effect:30 ft cube, attack_type:false, casting_time:"1 action", classes:[cleric, druid], components:[v, s, m("A drop of water if creating water, or a few grains of sand if destroying it.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You either create or destroy water.", "**Create Water.** You create up to 10 gallons of clean water within range in an open container. Alternatively, the water falls as rain in a 30-foot cube within range.", "**Destroy Water.** You destroy up to 10 gallons of water in an open container within range. Alternatively, you destroy fog in a 30-foot cube within range."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 2nd level or higher, you create or destroy 10 additional gallons of water, or the size of the cube increases by 5 feet, for each slot level above 1st.", level:1, material:"A drop of water if creating water, or a few grains of sand if destroying it.", range:feet(30), ritual:no, school:transmutation}).
spell_auto_data('create undead', properties{area_of_effect:false, attack_type:false, casting_time:"1 minute", classes:[cleric, warlock, wizard], components:[v, s, m("One clay pot filled with grave dirt, one clay pot filled with brackish water, and one 150 gp black onyx stone for each corpse.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You can cast this spell only at night. Choose up to three corpses of Medium or Small humanoids within range. Each corpse becomes a ghoul under your control. (The DM has game statistics for these creatures.)", "As a bonus action on each of your turns, you can mentally command any creature you animated with this spell if the creature is within 120 feet of you (if you control multiple creatures, you can command any or all of them at the same time, issuing the same command to each one). You decide what action the creature will take and where it will move during its next turn, or you can issue a general command, such as to guard a particular chamber or corridor. If you issue no commands, the creature only defends itself against hostile creatures. Once given an order, the creature continues to follow it until its task is complete.", "The creature is under your control for 24 hours, after which it stops obeying any command you have given it. To maintain control of the creature for another 24 hours, you must cast this spell on the creature before the current 24-hour period ends. This use of the spell reasserts your control over up to three creatures you have animated with this spell, rather than animating new ones."], duration:"Instantaneous", higher_level:"When you cast this spell using a 7th-level spell slot, you can animate or reassert control over four ghouls. When you cast this spell using an 8th-level spell slot, you can animate or reassert control over five ghouls or two ghasts or wights. When you cast this spell using a 9th-level spell slot, you can animate or reassert control over six ghouls, three ghasts or wights, or two mummies.", level:6, material:"One clay pot filled with grave dirt, one clay pot filled with brackish water, and one 150 gp black onyx stone for each corpse.", range:feet(10), ritual:no, school:necromancy}).
spell_auto_data(creation, properties{area_of_effect:5 ft cube, attack_type:false, casting_time:"1 minute", classes:[sorcerer, wizard], components:[v, s, m("A tiny piece of matter of the same type of the item you plan to create.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You pull wisps of shadow material from the Shadowfell to create a nonliving object of vegetable matter within range: soft goods, rope, wood, or something similar. You can also use this spell to create mineral objects such as stone, crystal, or metal. The object created must be no larger than a 5-foot cube, and the object must be of a form and material that you have seen before.", "The duration depends on the object's material. If the object is composed of multiple materials, use the shortest duration.", "| Material | Duration |\n |---|---|\n | Vegetable matter | 1 day |\n | Stone or crystal | 12 hours |\n | Precious metals | 1 hour |\n | Gems | 10 minutes |\n | Adamantine or mithral | 1 minute |", "Using any material created by this spell as another spell's material component causes that spell to fail."], duration:"Special", higher_level:"When you cast this spell using a spell slot of 6th level or higher, the cube increases by 5 feet for each slot level above 5th.", level:5, material:"A tiny piece of matter of the same type of the item you plan to create.", range:feet(30), ritual:no, school:illusion}).
spell_auto_data('cure wounds', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, cleric, druid, paladin, ranger], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["A creature you touch regains a number of hit points equal to 1 d 8 + your spellcasting ability modifier. This spell has no effect on undead or constructs."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 2nd level or higher, the healing increases by 1 d 8 for each slot level above 1st.", level:1, material:false, range:touch, ritual:no, school:evocation}).
spell_auto_data('dancing lights', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, wizard], components:[v, s, m("A bit of phosphorus or wychwood, or a glowworm.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You create up to four torch-sized lights within range, making them appear as torches, lanterns, or glowing orbs that hover in the air for the duration. You can also combine the four lights into one glowing vaguely humanoid form of Medium size. Whichever form you choose, each light sheds dim light in a 10-foot radius.", "As a bonus action on your turn, you can move the lights up to 60 feet to a new spot within range. A light must be within 20 feet of another light created by this spell, and a light winks out if it exceeds the spell's range."], duration:"Up to 1 minute", higher_level:no, level:0, material:"A bit of phosphorus or wychwood, or a glowworm.", range:feet(120), ritual:no, school:evocation}).
spell_auto_data(darkness, properties{area_of_effect:15 ft sphere, attack_type:false, casting_time:"1 action", classes:[sorcerer, warlock, wizard], components:[v, m("Bat fur and a drop of pitch or piece of coal.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Magical darkness spreads from a point you choose within range to fill a 15-foot-radius sphere for the duration. The darkness spreads around corners. A creature with darkvision can't see through this darkness, and nonmagical light can't illuminate it.", "If the point you choose is on an object you are holding or one that isn't being worn or carried, the darkness emanates from the object and moves with it. Completely covering the source of the darkness with an opaque object, such as a bowl or a helm, blocks the darkness.", "If any of this spell's area overlaps with an area of light created by a spell of 2nd level or lower, the spell that created the light is dispelled."], duration:"Up to 10 minutes", higher_level:no, level:2, material:"Bat fur and a drop of pitch or piece of coal.", range:feet(60), ritual:no, school:evocation}).
spell_auto_data(darkvision, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[druid, ranger, sorcerer, wizard], components:[v, s, m("Either a pinch of dried carrot or an agate.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch a willing creature to grant it the ability to see in the dark. For the duration, that creature has darkvision out to a range of 60 feet."], duration:"8 hours", higher_level:no, level:2, material:"Either a pinch of dried carrot or an agate.", range:touch, ritual:no, school:transmutation}).
spell_auto_data(daylight, properties{area_of_effect:60 ft sphere, attack_type:false, casting_time:"1 action", classes:[cleric, druid, paladin, ranger, sorcerer], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["A 60-foot-radius sphere of light spreads out from a point you choose within range. The sphere is bright light and sheds dim light for an additional 60 feet.", "If you chose a point on an object you are holding or one that isn't being worn or carried, the light shines from the object and moves with it. Completely covering the affected object with an opaque object, such as a bowl or a helm, blocks the light.", "If any of this spell's area overlaps with an area of darkness created by a spell of 3rd level or lower, the spell that created the darkness is dispelled."], duration:"1 hour", higher_level:no, level:3, material:false, range:feet(60), ritual:no, school:evocation}).
spell_auto_data('death ward', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric, paladin], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch a creature and grant it a measure of protection from death.", "The first time the target would drop to 0 hit points as a result of taking damage, the target instead drops to 1 hit point, and the spell ends.", "If the spell is still in effect when the target is subjected to an effect that would kill it instantaneously without dealing damage, that effect is instead negated against the target, and the spell ends."], duration:"8 hours", higher_level:no, level:4, material:false, range:touch, ritual:no, school:abjuration}).
spell_auto_data('delayed blast fireball', properties{area_of_effect:20 ft sphere, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s, m("A tiny ball of bat guano and sulfur.")], concentration:yes, damage_at_slot_level:_{7:damage(fire, 12 d 6), 8:damage(fire, 13 d 6), 9:damage(fire, 14 d 6)}, damage_with_cantrip_scaling:false, dc:dex else half, desc:["A beam of yellow light flashes from your pointing finger, then condenses to linger at a chosen point within range as a glowing bead for the duration. When the spell ends, either because your concentration is broken or because you decide to end it, the bead blossoms with a low roar into an explosion of flame that spreads around corners. Each creature in a 20-foot-radius sphere centered on that point must make a dexterity saving throw. A creature takes fire damage equal to the total accumulated damage on a failed save, or half as much damage on a successful one.", "The spell's base damage is 12 d 6. If at the end of your turn the bead has not yet detonated, the damage increases by 1 d 6.", "If the glowing bead is touched before the interval has expired, the creature touching it must make a dexterity saving throw. On a failed save, the spell ends immediately, causing the bead to erupt in flame. On a successful save, the creature can throw the bead up to 40 feet. When it strikes a creature or a solid object, the spell ends, and the bead explodes.", "The fire damages objects in the area and ignites flammable objects that aren't being worn or carried."], duration:"Up to 1 minute", higher_level:"When you cast this spell using a spell slot of 8th level or higher, the base damage increases by 1 d 6 for each slot level above 7th.", level:7, material:"A tiny ball of bat guano and sulfur.", range:feet(150), ritual:no, school:evocation}).
spell_auto_data(demiplane, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[warlock, wizard], components:[s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You create a shadowy door on a flat solid surface that you can see within range. The door is large enough to allow Medium creatures to pass through unhindered. When opened, the door leads to a demiplane that appears to be an empty room 30 feet in each dimension, made of wood or stone. When the spell ends, the door disappears, and any creatures or objects inside the demiplane remain trapped there, as the door also disappears from the other side.", "Each time you cast this spell, you can create a new demiplane, or have the shadowy door connect to a demiplane you created with a previous casting of this spell. Additionally, if you know the nature and contents of a demiplane created by a casting of this spell by another creature, you can have the shadowy door connect to its demiplane instead."], duration:"1 hour", higher_level:no, level:8, material:false, range:feet(60), ritual:no, school:conjuration}).
spell_auto_data('detect evil and good', properties{area_of_effect:30 ft sphere, attack_type:false, casting_time:"1 action", classes:[cleric, paladin], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["For the duration, you know if there is an aberration, celestial, elemental, fey, fiend, or undead within 30 feet of you, as well as where the creature is located. Similarly, you know if there is a place or object within 30 feet of you that has been magically consecrated or desecrated.", "The spell can penetrate most barriers, but it is blocked by 1 foot of stone, 1 inch of common metal, a thin sheet of lead, or 3 feet of wood or dirt."], duration:"Up to 10 minutes", higher_level:no, level:1, material:false, range:self, ritual:no, school:divination}).
spell_auto_data('detect magic', properties{area_of_effect:30 ft sphere, attack_type:false, casting_time:"1 action", classes:[bard, cleric, druid, paladin, ranger, sorcerer, wizard], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["For the duration, you sense the presence of magic within 30 feet of you. If you sense magic in this way, you can use your action to see a faint aura around any visible creature or object in the area that bears magic, and you learn its school of magic, if any.", "The spell can penetrate most barriers, but it is blocked by 1 foot of stone, 1 inch of common metal, a thin sheet of lead, or 3 feet of wood or dirt."], duration:"Up to 10 minutes", higher_level:no, level:1, material:false, range:self, ritual:yes, school:divination}).
spell_auto_data('detect poison and disease', properties{area_of_effect:30 ft sphere, attack_type:false, casting_time:"1 action", classes:[cleric, druid, paladin, ranger], components:[v, s, m("A yew leaf.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["For the duration, you can sense the presence and location of poisons, poisonous creatures, and diseases within 30 feet of you. You also identify the kind of poison, poisonous creature, or disease in each case.", "The spell can penetrate most barriers, but it is blocked by 1 foot of stone, 1 inch of common metal, a thin sheet of lead, or 3 feet of wood or dirt."], duration:"Up to 10 minutes", higher_level:no, level:1, material:"A yew leaf.", range:self, ritual:yes, school:divination}).
spell_auto_data('detect thoughts', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, wizard], components:[v, s, m("A copper coin.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["For the duration, you can read the thoughts of certain creatures. When you cast the spell and as your action on each turn until the spell ends, you can focus your mind on any one creature that you can see within 30 feet of you. If the creature you choose has an Intelligence of 3 or lower or doesn't speak any language, the creature is unaffected.", "You initially learn the surface thoughts of the creature - what is most on its mind in that moment. As an action, you can either shift your attention to another creature's thoughts or attempt to probe deeper into the same creature's mind. If you probe deeper, the target must make a Wisdom saving throw. If it fails, you gain insight into its reasoning (if any), its emotional state, and something that looms large in its mind (such as something it worries over, loves, or hates). If it succeeds, the spell ends. Either way, the target knows that you are probing into its mind, and unless you shift your attention to another creature's thoughts, the creature can use its action on its turn to make an Intelligence check contested by your Intelligence check; if it succeeds, the spell ends.", "Questions verbally directed at the target creature naturally shape the course of its thoughts, so this spell is particularly effective as part of an interrogation.", "You can also use this spell to detect the presence of thinking creatures you can't see. When you cast the spell or as your action during the duration, you can search for thoughts within 30 feet of you. The spell can penetrate barriers, but 2 feet of rock, 2 inches of any metal other than lead, or a thin sheet of lead blocks you. You can't detect a creature with an Intelligence of 3 or lower or one that doesn't speak any language.", "Once you detect the presence of a creature in this way, you can read its thoughts for the rest of the duration as described above, even if you can't see it, but it must still be within range."], duration:"Up to 1 minute", higher_level:no, level:2, material:"A copper coin.", range:self, ritual:no, school:divination}).
spell_auto_data('dimension door', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, warlock, wizard], components:[v], concentration:no, damage_at_slot_level:_{4:damage(force, 4 d 6)}, damage_with_cantrip_scaling:false, dc:false, desc:["You teleport yourself from your current location to any other spot within range. You arrive at exactly the spot desired. It can be a place you can see, one you can visualize, or one you can describe by stating distance and direction, such as \"200 feet straight downward\" or \"upward to the northwest at a 45-degree angle, 300 feet.\"", "You can bring along objects as long as their weight doesn't exceed what you can carry. You can also bring one willing creature of your size or smaller who is carrying gear up to its carrying capacity. The creature must be within 5 feet of you when you cast this spell.", "If you would arrive in a place already occupied by an object or a creature, you and any creature traveling with you each take 4 d 6 force damage, and the spell fails to teleport you."], duration:"Instantaneous", higher_level:no, level:4, material:false, range:feet(500), ritual:no, school:conjuration}).
spell_auto_data('disguise self', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, wizard], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You make yourself--including your clothing, armor, weapons, and other belongings on your person--look different until the spell ends or until you use your action to dismiss it. You can seem 1 foot shorter or taller and can appear thin, fat, or in between. You can't change your body type, so you must adopt a form that has the same basic arrangement of limbs. Otherwise, the extent of the illusion is up to you.", "The changes wrought by this spell fail to hold up to physical inspection. For example, if you use this spell to add a hat to your outfit, objects pass through the hat, and anyone who touches it would feel nothing or would feel your head and hair. If you use this spell to appear thinner than you are, the hand of someone who reaches out to touch you would bump into you while it was seemingly still in midair.", "To discern that you are disguised, a creature can use its action to inspect your appearance and must succeed on an Intelligence (Investigation) check against your spell save DC."], duration:"1 hour", higher_level:no, level:1, material:false, range:self, ritual:no, school:illusion}).
spell_auto_data(disintegrate, properties{area_of_effect:10 ft cube, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s, m("A lodestone and a pinch of dust.")], concentration:no, damage_at_slot_level:_{6:damage(force, 10 d 6+40)}, damage_with_cantrip_scaling:false, dc:dex else none, desc:["A thin green ray springs from your pointing finger to a target that you can see within range. The target can be a creature, an object, or a creation of magical force, such as the wall created by wall of force.", "A creature targeted by this spell must make a dexterity saving throw. On a failed save, the target takes 10 d 6 + 40 force damage. If this damage reduces the target to 0 hit points, it is disintegrated.", "A disintegrated creature and everything it is wearing and carrying, except magic items, are reduced to a pile of fine gray dust. The creature can be restored to life only by means of a true resurrection or a wish spell.", "This spell automatically disintegrates a Large or smaller nonmagical object or a creation of magical force. If the target is a Huge or larger object or creation of force, this spell disintegrates a 10-foot-cube portion of it. A magic item is unaffected by this spell."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 7th level or higher, the damage increases by 3 d 6 for each slot level above 6th.", level:6, material:"A lodestone and a pinch of dust.", range:feet(60), ritual:no, school:transmutation}).
spell_auto_data('dispel evil and good', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric, paladin], components:[v, s, m("Holy water or powdered silver and iron.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:cha else other, desc:["Shimmering energy surrounds and protects you from fey, undead, and creatures originating from beyond the Material Plane. For the duration, celestials, elementals, fey, fiends, and undead have disadvantage on attack rolls against you.", "You can end the spell early by using either of the following special functions.", "**Break Enchantment.** As your action, you touch a creature you can reach that is charmed, frightened, or possessed by a celestial, an elemental, a fey, a fiend, or an undead. The creature you touch is no longer charmed, frightened, or possessed by such creatures.", "**Dismissal.** As your action, make a melee spell attack against a celestial, an elemental, a fey, a fiend, or an undead you can reach. On a hit, you attempt to drive the creature back to its home plane. The creature must succeed on a charisma saving throw or be sent back to its home plane (if it isn't there already). If they aren't on their home plane, undead are sent to the Shadowfell, and fey are sent to the Feywild."], duration:"Up to 1 minute", higher_level:no, level:5, material:"Holy water or powdered silver and iron.", range:self, ritual:no, school:abjuration}).
spell_auto_data('dispel magic', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, cleric, druid, paladin, sorcerer, warlock, wizard], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Choose one creature, object, or magical effect within range. Any spell of 3rd level or lower on the target ends. For each spell of 4th level or higher on the target, make an ability check using your spellcasting ability. The DC equals 10 + the spell's level. On a successful check, the spell ends."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 4th level or higher, you automatically end the effects of a spell on the target if the spell's level is equal to or less than the level of the spell slot you used.", level:3, material:false, range:feet(120), ritual:no, school:abjuration}).
spell_auto_data(divination, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[druid], components:[v, s, m("Incense and a sacrificial offering appropriate to your religion, together worth at least 25gp, which the spell consumes.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Your magic and an offering put you in contact with a god or a god's servants. You ask a single question concerning a specific goal, event, or activity to occur within 7 days. The DM offers a truthful reply. The reply might be a short phrase, a cryptic rhyme, or an omen.", "The spell doesn't take into account any possible circumstances that might change the outcome, such as the casting of additional spells or the loss or gain of a companion.", "If you cast the spell two or more times before finishing your next long rest, there is a cumulative 25 percent chance for each casting after the first that you get a random reading. The DM makes this roll in secret."], duration:"Instantaneous", higher_level:no, level:4, material:"Incense and a sacrificial offering appropriate to your religion, together worth at least 25gp, which the spell consumes.", range:self, ritual:yes, school:divination}).
spell_auto_data('divine favor', properties{area_of_effect:false, attack_type:false, casting_time:"1 bonus action", classes:[paladin], components:[v, s], concentration:yes, damage_at_slot_level:_{1:damage(radiant, 1 d 4)}, damage_with_cantrip_scaling:false, dc:false, desc:["Your prayer empowers you with divine radiance. Until the spell ends, your weapon attacks deal an extra 1 d 4 radiant damage on a hit."], duration:"Up to 1 minute", higher_level:no, level:1, material:false, range:self, ritual:no, school:evocation}).
spell_auto_data('divine word', properties{area_of_effect:false, attack_type:false, casting_time:"1 bonus action", classes:[cleric], components:[v], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:cha else none, desc:["You utter a divine word, imbued with the power that shaped the world at the dawn of creation. Choose any number of creatures you can see within range. Each creature that can hear you must make a Charisma saving throw. On a failed save, a creature suffers an effect based on its current hit points:", "- 50 hit points or fewer: deafened for 1 minute", "- 40 hit points or fewer: deafened and blinded for 10 minutes", "- 30 hit points or fewer: blinded, deafened, and stunned for 1 hour", "- 20 hit points or fewer: killed instantly", "Regardless of its current hit points, a celestial, an elemental, a fey, or a fiend that fails its save is forced back to its plane of origin (if it isn't there already) and can't return to your current plane for 24 hours by any means short of a wish spell."], duration:"Instantaneous", higher_level:no, level:7, material:false, range:feet(30), ritual:no, school:evocation}).
spell_auto_data('dominate beast', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[druid, sorcerer], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else none, desc:["You attempt to beguile a creature that you can see within range. It must succeed on a wisdom saving throw or be charmed by you for the duration. If you or creatures that are friendly to you are fighting it, it has advantage on the saving throw.", "While the creature is charmed, you have a telepathic link with it as long as the two of you are on the same plane of existence. You can use this telepathic link to issue commands to the creature while you are conscious (no action required), which it does its best to obey. You can specify a simple and general course of action, such as \"Attack that creature,\" \"Run over there,\" or \"Fetch that object.\" If the creature completes the order and doesn't receive further direction from you, it defends and preserves itself to the best of its ability.", "You can use your action to take total and precise control of the target. Until the end of your next turn, the creature takes only the actions you choose, and doesn't do anything that you don't allow it to do. During this time, you can also cause the creature to use a reaction, but this requires you to use your own reaction as well. Each time the target takes damage, it makes a new wisdom saving throw against the spell. If the saving throw succeeds, the spell ends."], duration:"Up to 1 minute", higher_level:"When you cast this spell with a 9th level spell slot, the duration is concentration, up to 8 hours.", level:4, material:false, range:feet(60), ritual:no, school:enchantment}).
spell_auto_data('dominate monster', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, warlock, wizard], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else none, desc:["You attempt to beguile a creature that you can see within range. It must succeed on a wisdom saving throw or be charmed by you for the duration. If you or creatures that are friendly to you are fighting it, it has advantage on the saving throw.", "While the creature is charmed, you have a telepathic link with it as long as the two of you are on the same plane of existence. You can use this telepathic link to issue commands to the creature while you are conscious (no action required), which it does its best to obey. You can specify a simple and general course of action, such as \"Attack that creature,\" \"Run over there,\" or \"Fetch that object.\" If the creature completes the order and doesn't receive further direction from you, it defends and preserves itself to the best of its ability.", "You can use your action to take total and precise control of the target. Until the end of your next turn, the creature takes only the actions you choose, and doesn't do anything that you don't allow it to do. During this time, you can also cause the creature to use a reaction, but this requires you to use your own reaction as well.", "Each time the target takes damage, it makes a new wisdom saving throw against the spell. If the saving throw succeeds, the spell ends."], duration:"Up to 1 hour", higher_level:"When you cast this spell with a 9th-level spell slot, the duration is concentration, up to 8 hours.", level:8, material:false, range:feet(60), ritual:no, school:enchantment}).
spell_auto_data('dominate person', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, wizard], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else other, desc:["You attempt to beguile a humanoid that you can see within range. It must succeed on a wisdom saving throw or be charmed by you for the duration. If you or creatures that are friendly to you are fighting it, it has advantage on the saving throw.", "While the target is charmed, you have a telepathic link with it as long as the two of you are on the same plane of existence. You can use this telepathic link to issue commands to the creature while you are conscious (no action required), which it does its best to obey. You can specify a simple and general course of action, such as \"Attack that creature,\" \"Run over there,\" or \"Fetch that object.\" If the creature completes the order and doesn't receive further direction from you, it defends and preserves itself to the best of its ability.", "You can use your action to take total and precise control of the target. Until the end of your next turn, the creature takes only the actions you choose, and doesn't do anything that you don't allow it to do. During this time you can also cause the creature to use a reaction, but this requires you to use your own reaction as well.", "Each time the target takes damage, it makes a new wisdom saving throw against the spell. If the saving throw succeeds, the spell ends."], duration:"Up to 1 minute", higher_level:"When you cast this spell using a 6th-level spell slot, the duration is concentration, up to 10 minutes. When you use a 7th-level spell slot, the duration is concentration, up to 1 hour. When you use a spell slot of 8th level or higher, the duration is concentration, up to 8 hours.", level:5, material:false, range:feet(60), ritual:no, school:enchantment}).
spell_auto_data(dream, properties{area_of_effect:false, attack_type:false, casting_time:"1 minute", classes:[bard, warlock, wizard], components:[v, s, m("A handful of sand, a dab of ink, and a writing quill plucked from a sleeping bird.")], concentration:no, damage_at_slot_level:_{5:damage(psychic, 3 d 6)}, damage_with_cantrip_scaling:false, dc:wis else none, desc:["This spell shapes a creature's dreams. Choose a creature known to you as the target of this spell. The target must be on the same plane of existence as you. Creatures that don't sleep, such as elves, can't be contacted by this spell. You, or a willing creature you touch, enters a trance state, acting as a messenger.", "While in the trance, the messenger is aware of his or her surroundings, but can't take actions or move.", "If the target is asleep, the messenger appears in the target's dreams and can converse with the target as long as it remains asleep, through the duration of the spell. The messenger can also shape the environment of the dream, creating landscapes, objects, and other images. The messenger can emerge from the trance at any time, ending the effect of the spell early. The target recalls the dream perfectly upon waking. If the target is awake when you cast the spell, the messenger knows it, and can either end the trance (and the spell) or wait for the target to fall asleep, at which point the messenger appears in the target's dreams.", "You can make the messenger appear monstrous and terrifying to the target. If you do, the messenger can deliver a message of no more than ten words and then the target must make a wisdom saving throw. On a failed save, echoes of the phantasmal monstrosity spawn a nightmare that lasts the duration of the target's sleep and prevents the target from gaining any benefit from that rest. In addition, when the target wakes up, it takes 3 d 6 psychic damage.", "If you have a body part, lock of hair, clipping from a nail, or similar portion of the target's body, the target makes its saving throw with disadvantage."], duration:"8 hours", higher_level:no, level:5, material:"A handful of sand, a dab of ink, and a writing quill plucked from a sleeping bird.", range:special, ritual:no, school:illusion}).
spell_auto_data(druidcraft, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[druid], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Whispering to the spirits of nature, you create one of the following effects within 'range':", "- You create a tiny, harmless sensory effect that predicts what the weather will be at your location for the next 24 hours. The effect might manifest as a golden orb for clear skies, a cloud for rain, falling snowflakes for snow, and so on. This effect persists for 1 round.", "- You instantly make a flower bloom, a seed pod open, or a leaf bud bloom.", "- You create an instantaneous, harmless sensory effect, such as falling leaves, a puff of wind, the sound of a small animal, or the faint order of skunk. The effect must fit in a 5-foot cube.", "- You instantly light or snuff out a candle, a torch, or a small campfire."], duration:"Instantaneous", higher_level:no, level:0, material:false, range:feet(30), ritual:no, school:transmutation}).
spell_auto_data(earthquake, properties{area_of_effect:100 ft sphere, attack_type:false, casting_time:"1 action", classes:[cleric, druid, sorcerer], components:[v, s, m("A pinch of dirt, a piece of rock, and a lump of clay.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You create a seismic disturbance at a point on the ground that you can see within range. For the duration, an intense tremor rips through the ground in a 100-foot-radius circle centered on that point and shakes creatures and structures in contact with the ground in that area.", "The ground in the area becomes difficult terrain. Each creature on the ground that is concentrating must make a constitution saving throw. On a failed save, the creature's concentration is broken.", "When you cast this spell and at the end of each turn you spend concentrating on it, each creature on the ground in the area must make a dexterity saving throw. On a failed save, the creature is knocked prone.", "This spell can have additional effects depending on the terrain in the area, as determined by the DM.", "Fissures. Fissures open throughout the spell's area at the start of your next turn after you cast the spell. A total of 1 d 6 such fissures open in locations chosen by the DM. Each is 1 d 10 x 10 feet deep, 10 feet wide, and extends from one edge of the spell's area to the opposite side. A creature standing on a spot where a fissure opens must succeed on a dexterity saving throw or fall in. A creature that successfully saves moves with the fissure's edge as it opens.", "A fissure that opens beneath a structure causes it to automatically collapse (see below).", "Structures. The tremor deals 50 bludgeoning damage to any structure in contact with the ground in the area when you cast the spell and at the start of each of your turns until the spell ends. If a structure drops to 0 hit points, it collapses and potentially damages nearby creatures. A creature within half the distance of a structure's height must make a dexterity saving throw. On a failed save, the creature takes 5 d 6 bludgeoning damage, is knocked prone, and is buried in the rubble, requiring a DC 20 Strength (Athletics) check as an action to escape. The DM can adjust the DC higher or lower, depending on the nature of the rubble. On a successful save, the creature takes half as much damage and doesn't fall prone or become buried."], duration:"Up to 1 minute", higher_level:no, level:8, material:"A pinch of dirt, a piece of rock, and a lump of clay.", range:feet(500), ritual:no, school:evocation}).
spell_auto_data('eldritch blast', properties{area_of_effect:false, attack_type:ranged, casting_time:"1 action", classes:[warlock], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:damage(force, 1 d 10), dc:false, desc:["A beam of crackling energy streaks toward a creature within range. Make a ranged spell attack against the target. On a hit, the target takes 1 d 10 force damage. The spell creates more than one beam when you reach higher levels: two beams at 5th level, three beams at 11th level, and four beams at 17th level. You can direct the beams at the same target or at different ones. Make a separate attack roll for each beam."], duration:"Instantaneous", higher_level:no, level:0, material:false, range:feet(120), ritual:no, school:evocation}).
spell_auto_data('enhance ability', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, cleric, druid, sorcerer], components:[v, s, m("Fur or a feather from a beast.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch a creature and bestow upon it a magical enhancement. Choose one of the following effects; the target gains that effect until the spell ends.", "**Bear's Endurance.** The target has advantage on constitution checks. It also gains 2 d 6 temporary hit points, which are lost when the spell ends.", "**Bull's Strength.** The target has advantage on strength checks, and his or her carrying capacity doubles.", "**Cat's Grace.** The target has advantage on dexterity checks. It also doesn't take damage from falling 20 feet or less if it isn't incapacitated.", "**Eagle's Splendor.** The target has advantage on Charisma checks.", "**Fox's Cunning.** The target has advantage on intelligence checks.", "**Owl's Wisdom.** The target has advantage on wisdom checks."], duration:"Up to 1 hour", higher_level:"When you cast this spell using a spell slot of 3rd level or higher, you can target one additional creature for each slot level above 2nd.", level:2, material:"Fur or a feather from a beast.", range:touch, ritual:no, school:transmutation}).
spell_auto_data('enlarge/reduce', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s, m("A pinch iron powder.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:con else none, desc:["You cause a creature or an object you can see within range to grow larger or smaller for the duration. Choose either a creature or an object that is neither worn nor carried. If the target is unwilling, it can make a Constitution saving throw. On a success, the spell has no effect.", "If the target is a creature, everything it is wearing and carrying changes size with it. Any item dropped by an affected creature returns to normal size at once.", "**Enlarge.** The target's size doubles in all dimensions, and its weight is multiplied by eight. This growth increases its size by one category-from Medium to Large, for example. If there isn't enough room for the target to double its size, the creature or object attains the maximum possible size in the space available. Until the spell ends, the target also has advantage on Strength checks and Strength saving throws. The target's weapons also grow to match its new size. While these weapons are enlarged, the target's attacks with them deal 1 d 4 extra damage.", "**Reduce.** The target's size is halved in all dimensions, and its weight is reduced to one-eighth of normal. This reduction decreases its size by one category-from Medium to Small, for example. Until the spell ends, the target also has disadvantage on Strength checks and Strength saving throws. The target's weapons also shrink to match its new size. While these weapons are reduced, the target's attacks with them deal 1 d 4 less damage (this can't reduce the damage below 1)."], duration:"Up to 1 minute", higher_level:no, level:2, material:"A pinch iron powder.", range:feet(30), ritual:no, school:transmutation}).
spell_auto_data(entangle, properties{area_of_effect:20 ft cube, attack_type:false, casting_time:"1 action", classes:[druid], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:str else none, desc:["Grasping weeds and vines sprout from the ground in a 20-foot square starting form a point within range. For the duration, these plants turn the ground in the area into difficult terrain.", "A creature in the area when you cast the spell must succeed on a strength saving throw or be restrained by the entangling plants until the spell ends. A creature restrained by the plants can use its action to make a Strength check against your spell save DC. On a success, it frees itself.", "When the spell ends, the conjured plants wilt away."], duration:"Up to 1 minute", higher_level:no, level:1, material:false, range:feet(90), ritual:no, school:conjuration}).
spell_auto_data(enthrall, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, warlock], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else none, desc:["You weave a distracting string of words, causing creatures of your choice that you can see within range and that can hear you to make a wisdom saving throw. Any creature that can't be charmed succeeds on this saving throw automatically, and if you or your companions are fighting a creature, it has advantage on the save. On a failed save, the target has disadvantage on Wisdom (Perception) checks made to perceive any creature other than you until the spell ends or until the target can no longer hear you. The spell ends if you are incapacitated or can no longer speak."], duration:"1 minute", higher_level:no, level:2, material:false, range:feet(60), ritual:no, school:enchantment}).
spell_auto_data(etherealness, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, cleric, sorcerer, warlock, wizard], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You step into the border regions of the Ethereal Plane, in the area where it overlaps with your current plane. You remain in the Border Ethereal for the duration or until you use your action to dismiss the spell. During this time, you can move in any direction. If you move up or down, every foot of movement costs an extra foot. You can see and hear the plane you originated from, but everything there looks gray, and you can't see anything more than 60 feet away.", "While on the Ethereal Plane, you can only affect and be affected by other creatures on that plane. Creatures that aren't on the Ethereal Plane can't perceive you and can't interact with you, unless a special ability or magic has given them the ability to do so.", "You ignore all objects and effects that aren't on the Ethereal Plane, allowing you to move through objects you perceive on the plane you originated from.", "When the spell ends, you immediately return to the plane you originated from in the spot you currently occupy. If you occupy the same spot as a solid object or creature when this happens, you are immediately shunted to the nearest unoccupied space that you can occupy and take force damage equal to twice the number of feet you are moved.", "This spell has no effect if you cast it while you are on the Ethereal Plane or a plane that doesn't border it, such as one of the Outer Planes."], duration:"8 hours", higher_level:"When you cast this spell using a spell slot of 8th level or higher, you can target up to three willing creatures (including you) for each slot level above 7th. The creatures must be within 10 feet of you when you cast the spell.", level:7, material:false, range:self, ritual:no, school:transmutation}).
spell_auto_data('expeditious retreat', properties{area_of_effect:false, attack_type:false, casting_time:"1 bonus action", classes:[sorcerer, warlock, wizard], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["This spell allows you to move at an incredible pace. When you cast this spell, and then as a bonus action on each of your turns until the spell ends, you can take the Dash action."], duration:"Up to 10 minutes", higher_level:no, level:1, material:false, range:self, ritual:no, school:transmutation}).
spell_auto_data(eyebite, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, warlock, wizard], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else none, desc:["For the spell's duration, your eyes become an inky void imbued with dread power. One creature of your choice within 60 feet of you that you can see must succeed on a wisdom saving throw or be affected by one of the following effects of your choice for the duration. On each of your turns until the spell ends, you can use your action to target another creature but can't target a creature again if it has succeeded on a saving throw against this casting of eyebite.", "**Asleep.** The target falls unconscious. It wakes up if it takes any damage or if another creature uses its action to shake the sleeper awake.", "**Panicked.** The target is frightened of you. On each of its turns, the frightened creature must take the Dash action and move away from you by the safest and shortest available route, unless there is nowhere to move. If the target moves to a place at least 60 feet away from you where it can no longer see you, this effect ends.", "**Sickened.** The target has disadvantage on attack rolls and ability checks. At the end of each of its turns, it can make another wisdom saving throw. If it succeeds, the effect ends."], duration:"Up to 1 minute", higher_level:no, level:6, material:false, range:self, ritual:no, school:necromancy}).
spell_auto_data(fabricate, properties{area_of_effect:false, attack_type:false, casting_time:"10 minutes", classes:[wizard], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You convert raw materials into products of the same material. For example, you can fabricate a wooden bridge from a clump of trees, a rope from a patch of hemp, and clothes from flax or wool.", "Choose raw materials that you can see within range. You can fabricate a Large or smaller object (contained within a 10-foot cube, or eight connected 5-foot cubes), given a sufficient quantity of raw material. If you are working with metal, stone, or another mineral substance, however, the fabricated object can be no larger than Medium (contained within a single 5-foot cube). The quality of objects made by the spell is commensurate with the quality of the raw materials.", "Creatures or magic items can't be created or transmuted by this spell. You also can't use it to create items that ordinarily require a high degree of craftsmanship, such as jewelry, weapons, glass, or armor, unless you have proficiency with the type of artisan's tools used to craft such objects."], duration:"Instantaneous", higher_level:no, level:4, material:false, range:feet(120), ritual:no, school:transmutation}).
spell_auto_data('faerie fire', properties{area_of_effect:20 ft cube, attack_type:false, casting_time:"1 action", classes:[druid], components:[v], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:dex else none, desc:["Each object in a 20-foot cube within range is outlined in blue, green, or violet light (your choice). Any creature in the area when the spell is cast is also outlined in light if it fails a dexterity saving throw. For the duration, objects and affected creatures shed dim light in a 10-foot radius.", "Any attack roll against an affected creature or object has advantage if the attacker can see it, and the affected creature or object can't benefit from being invisible."], duration:"Up to 1 minute", higher_level:no, level:1, material:false, range:feet(60), ritual:no, school:evocation}).
spell_auto_data('faithful hound', properties{area_of_effect:false, attack_type:melee, casting_time:"1 action", classes:[wizard], components:[v, s, m("A tiny silver whistle, a piece of bone, and a thread")], concentration:no, damage_at_slot_level:_{4:damage(piercing, 4 d 8)}, damage_with_cantrip_scaling:false, dc:false, desc:["You conjure a phantom watchdog in an unoccupied space that you can see within range, where it remains for the duration, until you dismiss it as an action, or until you move more than 100 feet away from it.", "The hound is invisible to all creatures except you and can't be harmed. When a Small or larger creature comes within 30 feet of it without first speaking the password that you specify when you cast this spell, the hound starts barking loudly. The hound sees invisible creatures and can see into the Ethereal Plane. It ignores illusions.", "At the start of each of your turns, the hound attempts to bite one creature within 5 feet of it that is hostile to you. The hound's attack bonus is equal to your spellcasting ability modifier + your proficiency bonus. On a hit, it deals 4 d 8 piercing damage."], duration:"8 hours", higher_level:no, level:4, material:"A tiny silver whistle, a piece of bone, and a thread", range:feet(30), ritual:no, school:conjuration}).
spell_auto_data('false life', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s, m("A small amount of alcohol or distilled spirits.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Bolstering yourself with a necromantic facsimile of life, you gain 1 d 4 + 4 temporary hit points for the duration."], duration:"1 hour", higher_level:"When you cast this spell using a spell slot of 2nd level or higher, you gain 5 additional temporary hit points for each slot level above 1st.", level:1, material:"A small amount of alcohol or distilled spirits.", range:self, ritual:no, school:necromancy}).
spell_auto_data(fear, properties{area_of_effect:30 ft cone, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, warlock, wizard], components:[v, s, m("A white feather or the heart of a hen.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else none, desc:["You project a phantasmal image of a creature's worst fears. Each creature in a 30-foot cone must succeed on a wisdom saving throw or drop whatever it is holding and become frightened for the duration.", "While frightened by this spell, a creature must take the Dash action and move away from you by the safest available route on each of its turns, unless there is nowhere to move. If the creature ends its turn in a location where it doesn't have line of sight to you, the creature can make a wisdom saving throw. On a successful save, the spell ends for that creature."], duration:"Up to 1 minute", higher_level:no, level:3, material:"A white feather or the heart of a hen.", range:self, ritual:no, school:illusion}).
spell_auto_data('feather fall', properties{area_of_effect:false, attack_type:false, casting_time:"1 reaction", classes:[bard, sorcerer, wizard], components:[v, m("A small feather or a piece of down.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Choose up to five falling creatures within range. A falling creature's rate of descent slows to 60 feet per round until the spell ends. If the creature lands before the spell ends, it takes no falling damage and can land on its feet, and the spell ends for that creature."], duration:"1 minute", higher_level:no, level:1, material:"A small feather or a piece of down.", range:feet(60), ritual:no, school:transmutation}).
spell_auto_data(feeblemind, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, druid, warlock, wizard], components:[v, s, m("A handful of clay, crystal, glass, or mineral spheres.")], concentration:no, damage_at_slot_level:_{8:damage(psychic, 4 d 6)}, damage_with_cantrip_scaling:false, dc:int else other, desc:["You blast the mind of a creature that you can see within range, attempting to shatter its intellect and personality. The target takes 4 d 6 psychic damage and must make an intelligence saving throw.", "On a failed save, the creature's Intelligence and Charisma scores become 1. The creature can't cast spells, activate magic items, understand language, or communicate in any intelligible way. The creature can, however, identify its friends, follow them, and even protect them.", "At the end of every 30 days, the creature can repeat its saving throw against this spell. If it succeeds on its saving throw, the spell ends.", "The spell can also be ended by greater restoration, heal, or wish."], duration:"Instantaneous", higher_level:no, level:8, material:"A handful of clay, crystal, glass, or mineral spheres.", range:feet(150), ritual:no, school:enchantment}).
spell_auto_data('find familiar', properties{area_of_effect:false, attack_type:false, casting_time:"1 hour", classes:[wizard], components:[v, s, m("10gp worth of charcoal, incense, and herbs that must be consumed by fire in a brass brazier.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You gain the service of a familiar, a spirit that takes an animal form you choose: bat, cat, crab, frog (toad), hawk, lizard, octopus, owl, poisonous snake, fish (quipper), rat, raven, sea horse, spider, or weasel. Appearing in an unoccupied space within range, the familiar has the statistics of the chosen form, though it is a celestial, fey, or fiend (your choice) instead of a beast.", "Your familiar acts independently of you, but it always obeys your commands. In combat, it rolls its own initiative and acts on its own turn. A familiar can't attack, but it can take other actions as normal.", "When the familiar drops to 0 hit points, it disappears, leaving behind no physical form. It reappears after you cast this spell again.", "While your familiar is within 100 feet of you, you can communicate with it telepathically. Additionally, as an action, you can see through your familiar's eyes and hear what it hears until the start of your next turn, gaining the benefits of any special senses that the familiar has. During this time, you are deaf and blind with regard to your own senses.", "As an action, you can temporarily dismiss your familiar. It disappears into a pocket dimension where it awaits your summons. Alternatively, you can dismiss it forever. As an action while it is temporarily dismissed, you can cause it to reappear in any unoccupied space within 30 feet of you.", "You can't have more than one familiar at a time. If you cast this spell while you already have a familiar, you instead cause it to adopt a new form. Choose one of the forms from the above list. Your familiar transforms into the chosen creature.", "Finally, when you cast a spell with a range of touch, your familiar can deliver the spell as if it had cast the spell. Your familiar must be within 100 feet of you, and it must use its reaction to deliver the spell when you cast it. If the spell requires an attack roll, you use your action modifier for the roll."], duration:"Instantaneous", higher_level:no, level:1, material:"10gp worth of charcoal, incense, and herbs that must be consumed by fire in a brass brazier.", range:feet(10), ritual:yes, school:conjuration}).
spell_auto_data('find steed', properties{area_of_effect:false, attack_type:false, casting_time:"10 minutes", classes:[paladin], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You summon a spirit that assumes the form of an unusually intelligent, strong, and loyal steed, creating a long-lasting bond with it. Appearing in an unoccupied space within range, the steed takes on a form that you choose, such as a warhorse, a pony, a camel, an elk, or a mastiff. (Your DM might allow other animals to be summoned as steeds.) The steed has the statistics of the chosen form, though it is a celestial, fey, or fiend (your choice) instead of its normal type. Additionally, if your steed has an Intelligence of 5 or less, its Intelligence becomes 6, and it gains the ability to understand one language of your choice that you speak.", "Your steed serves you as a mount, both in combat and out, and you have an instinctive bond with it that allows you to fight as a seamless unit. While mounted on your steed, you can make any spell you cast that targets only you also target your steed.", "When the steed drops to 0 hit points, it disappears, leaving behind no physical form. You can also dismiss your steed at any time as an action, causing it to disappear. In either case, casting this spell again summons the same steed, restored to its hit point maximum.", "While your steed is within 1 mile of you, you can communicate with it telepathically.", "You can't have more than one steed bonded by this spell at a time. As an action, you can release the steed from its bond at any time, causing it to disappear."], duration:"Instantaneous", higher_level:no, level:2, material:false, range:feet(30), ritual:no, school:conjuration}).
spell_auto_data('find the path', properties{area_of_effect:false, attack_type:false, casting_time:"1 minute", classes:[bard, cleric], components:[v, s, m("A set of divinatory tools--such as bones, ivory sticks, cards, teeth, or carved runes--worth 100gp and an object from the location you wish to find.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["This spell allows you to find the shortest, most direct physical route to a specific fixed location that you are familiar with on the same plane of existence. If you name a destination on another plane of existence, a destination that moves (such as a mobile fortress), or a destination that isn't specific (such as \"a green dragon's lair\"), the spell fails.", "For the duration, as long as you are on the same plane of existence as the destination, you know how far it is and in what direction it lies. While you are traveling there, whenever you are presented with a choice of paths along the way, you automatically determine which path is the shortest and most direct route (but not necessarily the safest route) to the destination."], duration:"Up to 24 hours", higher_level:no, level:6, material:"A set of divinatory tools--such as bones, ivory sticks, cards, teeth, or carved runes--worth 100gp and an object from the location you wish to find.", range:self, ritual:no, school:divination}).
spell_auto_data('find traps', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric, druid, ranger], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You sense the presence of any trap within range that is within line of sight. A trap, for the purpose of this spell, includes anything that would inflict a sudden or unexpected effect you consider harmful or undesirable, which was specifically intended as such by its creator. Thus, the spell would sense an area affected by the alarm spell, a glyph of warding, or a mechanical pit trap, but it would not reveal a natural weakness in the floor, an unstable ceiling, or a hidden sinkhole.", "This spell merely reveals that a trap is present. You don't learn the location of each trap, but you do learn the general nature of the danger posed by a trap you sense."], duration:"Instantaneous", higher_level:no, level:2, material:false, range:feet(120), ritual:no, school:divination}).
spell_auto_data('finger of death', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[sorcerer, warlock, wizard], components:[v, s], concentration:no, damage_at_slot_level:_{7:damage(necrotic, 7 d 8+30)}, damage_with_cantrip_scaling:false, dc:con else half, desc:["You send negative energy coursing through a creature that you can see within range, causing it searing pain. The target must make a constitution saving throw. It takes 7 d 8 + 30 necrotic damage on a failed save, or half as much damage on a successful one.", "A humanoid killed by this spell rises at the start of your next turn as a zombie that is permanently under your command, following your verbal orders to the best of its ability."], duration:"Instantaneous", higher_level:no, level:7, material:false, range:feet(60), ritual:no, school:necromancy}).
spell_auto_data('fire bolt', properties{area_of_effect:false, attack_type:ranged, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:damage(fire, 1 d 10), dc:false, desc:["You hurl a mote of fire at a creature or object within range. Make a ranged spell attack against the target. On a hit, the target takes 1 d 10 fire damage. A flammable object hit by this spell ignites if it isn't being worn or carried.", "This spell's damage increases by 1 d 10 when you reach 5th level (2 d 10), 11th level (3 d 10), and 17th level (4 d 10)."], duration:"Instantaneous", higher_level:no, level:0, material:false, range:feet(120), ritual:no, school:evocation}).
spell_auto_data('fire shield', properties{area_of_effect:5 ft sphere, attack_type:false, casting_time:"1 action", classes:[wizard], components:[v, s, m("A little phosphorus or a firefly.")], concentration:no, damage_at_slot_level:_{4:damage(fire, 2 d 8)}, damage_with_cantrip_scaling:false, dc:false, desc:["Thin and vaporous flame surround your body for the duration of the spell, radiating a bright light bright light in a 10-foot radius and dim light for an additional 10 feet. You can end the spell using an action to make it disappear.", "The flames are around you a heat shield or cold, your choice. The heat shield gives you cold damage resistance and the cold resistance to fire damage.", "In addition, whenever a creature within 5 feet of you hits you with a melee attack, flames spring from the shield. The attacker then suffers 2 d 8 points of fire damage or cold, depending on the model."], duration:"10 minutes", higher_level:no, level:4, material:"A little phosphorus or a firefly.", range:self, ritual:no, school:evocation}).
spell_auto_data('fire storm', properties{area_of_effect:100 ft cube, attack_type:false, casting_time:"1 action", classes:[cleric, druid, sorcerer], components:[v, s], concentration:no, damage_at_slot_level:_{7:damage(fire, 7 d 10)}, damage_with_cantrip_scaling:false, dc:dex else half, desc:["A storm made up of sheets of roaring flame appears in a location you choose within range. The area of the storm consists of up to ten 10-foot cubes, which you can arrange as you wish. Each cube must have at least one face adjacent to the face of another cube. Each creature in the area must make a dexterity saving throw. It takes 7 d 10 fire damage on a failed save, or half as much damage on a successful one.", "The fire damages objects in the area and ignites flammable objects that aren't being worn or carried. If you choose, plant life in the area is unaffected by this spell."], duration:"Instantaneous", higher_level:no, level:7, material:false, range:feet(150), ritual:no, school:evocation}).
spell_auto_data(fireball, properties{area_of_effect:20 ft sphere, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s, m("A tiny ball of bat guano and sulfur.")], concentration:no, damage_at_slot_level:_{3:damage(fire, 8 d 6), 4:damage(fire, 9 d 6), 5:damage(fire, 10 d 6), 6:damage(fire, 11 d 6), 7:damage(fire, 12 d 6), 8:damage(fire, 13 d 6), 9:damage(fire, 14 d 6)}, damage_with_cantrip_scaling:false, dc:dex else half, desc:["A bright streak flashes from your pointing finger to a point you choose within range and then blossoms with a low roar into an explosion of flame. Each creature in a 20-foot-radius sphere centered on that point must make a dexterity saving throw. A target takes 8 d 6 fire damage on a failed save, or half as much damage on a successful one.", "The fire spreads around corners. It ignites flammable objects in the area that aren't being worn or carried."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 4th level or higher, the damage increases by 1 d 6 for each slot level above 3rd.", level:3, material:"A tiny ball of bat guano and sulfur.", range:feet(150), ritual:no, school:evocation}).
spell_auto_data('flame blade', properties{area_of_effect:false, attack_type:false, casting_time:"1 bonus action", classes:[druid], components:[v, s, m("Leaf of sumac.")], concentration:yes, damage_at_slot_level:_{2:damage(fire, 3 d 6), 4:damage(fire, 4 d 6), 6:damage(fire, 5 d 6), 8:damage(fire, 6 d 6)}, damage_with_cantrip_scaling:false, dc:false, desc:["You evoke a fiery blade in your free hand. The blade is similar in size and shape to a scimitar, and it lasts for the duration. If you let go of the blade, it disappears, but you can evoke the blade again as a bonus action.", "You can use your action to make a melee spell attack with the fiery blade. On a hit, the target takes 3 d 6 fire damage.", "The flaming blade sheds bright light in a 10-foot radius and dim light for an additional 10 feet."], duration:"Up to 10 minutes", higher_level:"When you cast this spell using a spell slot of 4th level or higher, the damage increases by 1 d 6 for every two slot levels above 2nd.", level:2, material:"Leaf of sumac.", range:self, ritual:no, school:evocation}).
spell_auto_data('flame strike', properties{area_of_effect:40 ft cylinder, attack_type:false, casting_time:"1 action", classes:[cleric], components:[v, s, m("Pinch of sulfur.")], concentration:no, damage_at_slot_level:_{5:damage(fire, 4 d 6)+damage(radiant, 4 d 6)}, damage_with_cantrip_scaling:false, dc:dex else half, desc:["A vertical column of divine fire roars down from the heavens in a location you specify. Each creature in a 10-foot-radius, 40-foot-high cylinder centered on a point within range must make a dexterity saving throw. A creature takes 4 d 6 fire damage and 4 d 6 radiant damage on a failed save, or half as much damage on a successful one."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 6th level or higher, the fire damage or the radiant damage (your choice) increases by 1 d 6 for each slot level above 5th.", level:5, material:"Pinch of sulfur.", range:feet(60), ritual:no, school:evocation}).
spell_auto_data('flaming sphere', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[druid, wizard], components:[v, s, m("A bit of tallow, a pinch of brimstone, and a dusting of powdered iron.")], concentration:yes, damage_at_slot_level:_{2:damage(fire, 2 d 6), 3:damage(fire, 3 d 6), 4:damage(fire, 4 d 6), 5:damage(fire, 5 d 6), 6:damage(fire, 6 d 6), 7:damage(fire, 7 d 6), 8:damage(fire, 8 d 6), 9:damage(fire, 9 d 6)}, damage_with_cantrip_scaling:false, dc:false, desc:["A 5-foot-diameter sphere of fire appears in an unoccupied space of your choice within range and lasts for the duration. Any creature that ends its turn within 5 feet of the sphere must make a dexterity saving throw. The creature takes 2 d 6 fire damage on a failed save, or half as much damage on a successful one.", "As a bonus action, you can move the sphere up to 30 feet. If you ram the sphere into a creature, that creature must make the saving throw against the sphere's damage, and the sphere stops moving this turn.", "When you move the sphere, you can direct it over barriers up to 5 feet tall and jump it across pits up to 10 feet wide. The sphere ignites flammable objects not being worn or carried, and it sheds bright light in a 20-foot radius and dim light for an additional 20 feet."], duration:"Up to 1 minute", higher_level:"When you cast this spell using a spell slot of 3rd level or higher, the damage increases by 1 d 6 for each slot level above 2nd.", level:2, material:"A bit of tallow, a pinch of brimstone, and a dusting of powdered iron.", range:feet(60), ritual:no, school:conjuration}).
spell_auto_data('flesh to stone', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[warlock, wizard], components:[v, s, m("A pinch of lime, water, and earth.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:con else none, desc:["You attempt to turn one creature that you can see within range into stone. If the target's body is made of flesh, the creature must make a constitution saving throw. On a failed save, it is restrained as its flesh begins to harden. On a successful save, the creature isn't affected.", "A creature restrained by this spell must make another constitution saving throw at the end of each of its turns. If it successfully saves against this spell three times, the spell ends. If it fails its saves three times, it is turned to stone and subjected to the petrified condition for the duration. The successes and failures don't need to be consecutive; keep track of both until the target collects three of a kind.", "If the creature is physically broken while petrified, it suffers from similar deformities if it reverts to its original state.", "If you maintain your concentration on this spell for the entire possible duration, the creature is turned to stone until the effect is removed."], duration:"Up to 1 minute", higher_level:no, level:6, material:"A pinch of lime, water, and earth.", range:feet(60), ritual:no, school:transmutation}).
spell_auto_data('floating disk', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[wizard], components:[v, s, m("A drop of mercury.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["This spell creates a circular, horizontal plane of force, 3 feet in diameter and 1 inch thick, that floats 3 feet above the ground in an unoccupied space of your choice that you can see within range. The disk remains for the duration, and can hold up to 500 pounds. If more weight is placed on it, the spell ends, and everything on the disk falls to the ground.", "The disk is immobile while you are within 20 feet of it. If you move more than 20 feet away from it, the disk follows you so that it remains within 20 feet of you. If can move across uneven terrain, up or down stairs, slopes and the like, but it can't cross an elevation change of 10 feet or more. For example, the disk can't move across a 10-foot-deep pit, nor could it leave such a pit if it was created at the bottom.", "If you move more than 100 feet away from the disk (typically because it can't move around an obstacle to follow you), the spell ends."], duration:"1 hour", higher_level:no, level:1, material:"A drop of mercury.", range:feet(30), ritual:yes, school:conjuration}).
spell_auto_data(fly, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[sorcerer, warlock, wizard], components:[v, s, m("A wing feather from any bird.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch a willing creature. The target gains a flying speed of 60 feet for the duration. When the spell ends, the target falls if it is still aloft, unless it can stop the fall."], duration:"Up to 10 minutes", higher_level:"When you cast this spell using a spell slot of 4th level or higher, you can target one additional creature for each slot level above 3rd.", level:3, material:"A wing feather from any bird.", range:touch, ritual:no, school:transmutation}).
spell_auto_data('fog cloud', properties{area_of_effect:20 ft sphere, attack_type:false, casting_time:"1 action", classes:[druid, ranger, sorcerer, wizard], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You create a 20-foot-radius sphere of fog centered on a point within range. The sphere spreads around corners, and its area is heavily obscured. It lasts for the duration or until a wind of moderate or greater speed (at least 10 miles per hour) disperses it."], duration:"Up to 1 hour", higher_level:"When you cast this spell using a spell slot of 2nd level or higher, the radius of the fog increases by 20 feet for each slot level above 1st.", level:1, material:false, range:feet(120), ritual:no, school:conjuration}).
spell_auto_data(forbiddance, properties{area_of_effect:40000 ft cube, attack_type:false, casting_time:"10 minutes", classes:[cleric], components:[v, s, m("A sprinkling of holy water, rare incense, and powdered ruby worth at least 1,000 gp.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You create a ward against magical travel that protects up to 40,000 square feet of floor space to a height of 30 feet above the floor. For the duration, creatures can't teleport into the area or use portals, such as those created by the gate spell, to enter the area. The spell proofs the area against planar travel, and therefore prevents creatures from accessing the area by way of the Astral Plane, Ethereal Plane, Feywild, Shadowfell, or the plane shift spell.", "In addition, the spell damages types of creatures that you choose when you cast it. Choose one or more of the following: celestials, elementals, fey, fiends, and undead. When a chosen creature enters the spell's area for the first time on a turn or starts its turn there, the creature takes 5 d 10 radiant or necrotic damage (your choice when you cast this spell).", "When you cast this spell, you can designate a password. A creature that speaks the password as it enters the area takes no damage from the spell.", "The spell's area can't overlap with the area of another forbiddance spell. If you cast forbiddance every day for 30 days in the same location, the spell lasts until it is dispelled, and the material components are consumed on the last casting."], duration:"24 hours", higher_level:no, level:6, material:"A sprinkling of holy water, rare incense, and powdered ruby worth at least 1,000 gp.", range:touch, ritual:yes, school:abjuration}).
spell_auto_data(forcecage, properties{area_of_effect:20 ft cube, attack_type:false, casting_time:"1 action", classes:[bard, warlock, wizard], components:[v, s, m("Ruby dust worth 1,500 gp.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["An immobile, invisible, cube-shaped prison composed of magical force springs into existence around an area you choose within range. The prison can be a cage or a solid box, as you choose.", "A prison in the shape of a cage can be up to 20 feet on a side and is made from 1/2-inch diameter bars spaced 1/2 inch apart.", "A prison in the shape of a box can be up to 10 feet on a side, creating a solid barrier that prevents any matter from passing through it and blocking any spells cast into or out from the area.", "When you cast the spell, any creature that is completely inside the cage's area is trapped. Creatures only partially within the area, or those too large to fit inside the area, are pushed away from the center of the area until they are completely outside the area.", "A creature inside the cage can't leave it by nonmagical means. If the creature tries to use teleportation or interplanar travel to leave the cage, it must first make a charisma saving throw. On a success, the creature can use that magic to exit the cage. On a failure, the creature can't exit the cage and wastes the use of the spell or effect. The cage also extends into the Ethereal Plane, blocking ethereal travel.", "This spell can't be dispelled by dispel magic."], duration:"1 hour", higher_level:no, level:7, material:"Ruby dust worth 1,500 gp.", range:feet(100), ritual:no, school:evocation}).
spell_auto_data(foresight, properties{area_of_effect:false, attack_type:false, casting_time:"1 minute", classes:[bard, druid, warlock, wizard], components:[v, s, m("A hummingbird feather.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch a willing creature and bestow a limited ability to see into the immediate future. For the duration, the target can't be surprised and has advantage on attack rolls, ability checks, and saving throws. Additionally, other creatures have disadvantage on attack rolls against the target for the duration.", "This spell immediately ends if you cast it again before its duration ends."], duration:"8 hours", higher_level:no, level:9, material:"A hummingbird feather.", range:touch, ritual:no, school:divination}).
spell_auto_data('freedom of movement', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, cleric, druid, ranger], components:[v, s, m("A leather strap, bound around the arm or a similar appendage.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch a willing creature. For the duration, the target's movement is unaffected by difficult terrain, and spells and other magical effects can neither reduce the target's speed nor cause the target to be paralyzed or restrained.", "The target can also spend 5 feet of movement to automatically escape from nonmagical restraints, such as manacles or a creature that has it grappled. Finally, being underwater imposes no penalties on the target's movement or attacks."], duration:"1 hour", higher_level:no, level:4, material:"A leather strap, bound around the arm or a similar appendage.", range:touch, ritual:no, school:abjuration}).
spell_auto_data('freezing sphere', properties{area_of_effect:60 ft sphere, attack_type:false, casting_time:"1 action", classes:[wizard], components:[v, s, m("A small crystal sphere.")], concentration:no, damage_at_slot_level:_{6:damage(cold, 10 d 6)}, damage_with_cantrip_scaling:false, dc:con else half, desc:["A frigid globe of cold energy streaks from your fingertips to a point of your choice within range, where it explodes in a 60-foot-radius sphere. Each creature within the area must make a constitution saving throw. On a failed save, a creature takes 10 d 6 cold damage. On a successful save, it takes half as much damage.", "If the globe strikes a body of water or a liquid that is principally water (not including water-based creatures), it freezes the liquid to a depth of 6 inches over an area 30 feet square. This ice lasts for 1 minute. Creatures that were swimming on the surface of frozen water are trapped in the ice. A trapped creature can use an action to make a Strength check against your spell save DC to break free.", "You can refrain from firing the globe after completing the spell, if you wish. A small globe about the size of a sling stone, cool to the touch, appears in your hand. At any time, you or a creature you give the globe to can throw the globe (to a range of 40 feet) or hurl it with a sling (to the sling's normal range). It shatters on impact, with the same effect as the normal casting of the spell. You can also set the globe down without shattering it. After 1 minute, if the globe hasn't already shattered, it explodes."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 7th level or higher, the damage increases by 1 d 6 for each slot level above 6th.", level:6, material:"A small crystal sphere.", range:feet(300), ritual:no, school:evocation}).
spell_auto_data('gaseous form', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[sorcerer, warlock, wizard], components:[v, s, m("A bit of gauze and a wisp of smoke.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You transform a willing creature you touch, along with everything it's wearing and carrying, into a misty cloud for the duration. The spell ends if the creature drops to 0 hit points. An incorporeal creature isn't affected.", "While in this form, the target's only method of movement is a flying speed of 10 feet. The target can enter and occupy the space of another creature. The target has resistance to nonmagical damage, and it has advantage on Strength, Dexterity, and constitution saving throws. The target can pass through small holes, narrow openings, and even mere cracks, though it treats liquids as though they were solid surfaces. The target can't fall and remains hovering in the air even when stunned or otherwise incapacitated.", "While in the form of a misty cloud, the target can't talk or manipulate objects, and any objects it was carrying or holding can't be dropped, used, or otherwise interacted with. The target can't attack or cast spells."], duration:"Up to 1 hour", higher_level:no, level:3, material:"A bit of gauze and a wisp of smoke.", range:touch, ritual:no, school:transmutation}).
spell_auto_data(gate, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric, sorcerer, wizard], components:[v, s, m("A diamond worth at least 5,000gp.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You conjure a portal linking an unoccupied space you can see within range to a precise location on a different plane of existence. The portal is a circular opening, which you can make 5 to 20 feet in diameter. You can orient the portal in any direction you choose. The portal lasts for the duration.", "The portal has a front and a back on each plane where it appears. Travel through the portal is possible only by moving through its front. Anything that does so is instantly transported to the other plane, appearing in the unoccupied space nearest to the portal.", "Deities and other planar rulers can prevent portals created by this spell from opening in their presence or anywhere within their domains.", "When you cast this spell, you can speak the name of a specific creature (a pseudonym, title, or nickname doesn't work). If that creature is on a plane other than the one you are on, the portal opens in the named creature's immediate vicinity and draws the creature through it to the nearest unoccupied space on your side of the portal. You gain no special power over the creature, and it is free to act as the DM deems appropriate. It might leave, attack you, or help you."], duration:"Up to 1 minute", higher_level:no, level:9, material:"A diamond worth at least 5,000gp.", range:feet(60), ritual:no, school:conjuration}).
spell_auto_data(geas, properties{area_of_effect:false, attack_type:false, casting_time:"1 minute", classes:[bard, cleric, druid, paladin, wizard], components:[v], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else none, desc:["You place a magical command on a creature that you can see within range, forcing it to carry out some service or refrain from some action or course of activity as you decide. If the creature can understand you, it must succeed on a wisdom saving throw or become charmed by you for the duration. While the creature is charmed by you, it takes 5 d 10 psychic damage each time it acts in a manner directly counter to your instructions, but no more than once each day. A creature that can't understand you is unaffected by the spell.", "You can issue any command you choose, short of an activity that would result in certain death. Should you issue a suicidal command, the spell ends.", "You can end the spell early by using an action to dismiss it. A remove curse, greater restoration, or wish spell also ends it."], duration:"30 days", higher_level:"When you cast this spell using a spell slot of 7th or 8th level, the duration is 1 year. When you cast this spell using a spell slot of 9th level, the spell lasts until it is ended by one of the spells mentioned above.", level:5, material:false, range:feet(60), ritual:no, school:enchantment}).
spell_auto_data('gentle repose', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric, wizard], components:[v, s, m("A pinch of salt and one copper piece placed on each of the corpse's eyes, which must remain there for the duration.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch a corpse or other remains. For the duration, the target is protected from decay and can't become undead.", "The spell also effectively extends the time limit on raising the target from the dead, since days spent under the influence of this spell don't count against the time limit of spells such as raise dead."], duration:"10 days", higher_level:no, level:2, material:"A pinch of salt and one copper piece placed on each of the corpse's eyes, which must remain there for the duration.", range:touch, ritual:yes, school:necromancy}).
spell_auto_data('giant insect', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[druid], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You transform up to ten centipedes, three spiders, five wasps, or one scorpion within range into giant versions of their natural forms for the duration. A centipede becomes a giant centipede, a spider becomes a giant spider, a wasp becomes a giant wasp, and a scorpion becomes a giant scorpion.", "Each creature obeys your verbal commands, and in combat, they act on your turn each round. The DM has the statistics for these creatures and resolves their actions and movement.", "A creature remains in its giant size for the duration, until it drops to 0 hit points, or until you use an action to dismiss the effect on it.", "The DM might allow you to choose different targets. For example, if you transform a bee, its giant version might have the same statistics as a giant wasp."], duration:"Up to 10 minutes", higher_level:no, level:4, material:false, range:feet(30), ritual:no, school:transmutation}).
spell_auto_data(glibness, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, warlock], components:[v], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Until the spell ends, when you make a Charisma check, you can replace the number you roll with a 15. Additionally, no matter what you say, magic that would determine if you are telling the truth indicates that you are being truthful."], duration:"1 hour", higher_level:no, level:8, material:false, range:self, ritual:no, school:transmutation}).
spell_auto_data('globe of invulnerability', properties{area_of_effect:10 ft sphere, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s, m("A glass or crystal bead that shatters when the spell ends.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["An immobile, faintly shimmering barrier springs into existence in a 10-foot radius around you and remains for the duration.", "Any spell of 5th level or lower cast from outside the barrier can't affect creatures or objects within it, even if the spell is cast using a higher level spell slot. Such a spell can target creatures and objects within the barrier, but the spell has no effect on them. Similarly, the area within the barrier is excluded from the areas affected by such spells."], duration:"Up to 1 minute", higher_level:"When you cast this spell using a spell slot of 7th level or higher, the barrier blocks spells of one level higher for each slot level above 6th.", level:6, material:"A glass or crystal bead that shatters when the spell ends.", range:self, ritual:no, school:abjuration}).
spell_auto_data('glyph of warding', properties{area_of_effect:false, attack_type:false, casting_time:"1 hour", classes:[bard, cleric, wizard], components:[v, s, m("Incense and powdered diamond worth at least 200 gp, which the spell consumes.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["When you cast this spell, you inscribe a glyph that harms other creatures, either upon a surface (such as a table or a section of floor or wall) or within an object that can be closed (such as a book, a scroll, or a treasure chest) to conceal the glyph. If you choose a surface, the glyph can cover an area of the surface no larger than 10 feet in diameter. If you choose an object, that object must remain in its place; if the object is moved more than 10 feet from where you cast this spell, the glyph is broken, and the spell ends without being triggered.", "The glyph is nearly invisible and requires a successful Intelligence (Investigation) check against your spell save DC to be found.", "You decide what triggers the glyph when you cast the spell. For glyphs inscribed on a surface, the most typical triggers include touching or standing on the glyph, removing another object covering the glyph, approaching within a certain distance of the glyph, or manipulating the object on which the glyph is inscribed. For glyphs inscribed within an object, the most common triggers include opening that object, approaching within a certain distance of the object, or seeing or reading the glyph. Once a glyph is triggered, this spell ends.", "You can further refine the trigger so the spell activates only under certain circumstances or according to physical characteristics (such as height or weight), creature kind (for example, the ward could be set to affect aberrations or drow), or alignment. You can also set conditions for creatures that don't trigger the glyph, such as those who say a certain password.", "When you inscribe the glyph, choose *explosive runes* or a *spell glyph*.", "**Explosive Runes.** When triggered, the glyph erupts with magical energy in a 20-foot-radius sphere centered on the glyph. The sphere spreads around corners. Each creature in the area must make a Dexterity saving throw. A creature takes 5 d 8 acid, cold, fire, lightning, or thunder damage on a failed saving throw (your choice when you create the glyph), or half as much damage on a successful one.", "**Spell Glyph.** You can store a prepared spell of 3rd level or lower in the glyph by casting it as part of creating the glyph. The spell must target a single creature or an area. The spell being stored has no immediate effect when cast in this way. When the glyph is triggered, the stored spell is cast. If the spell has a target, it targets the creature that triggered the glyph. If the spell affects an area, the area is centered on that creature. If the spell summons hostile creatures or creates harmful objects or traps, they appear as close as possible to the intruder and attack it. If the spell requires concentration, it lasts until the end of its full duration."], duration:"Until dispelled", higher_level:"When you cast this spell using a spell slot of 4th level or higher, the damage of an explosive runes glyph increases by 1 d 8 for each slot level above 3rd. If you create a spell glyph, you can store any spell of up to the same level as the slot you use for the glyph of warding.", level:3, material:"Incense and powdered diamond worth at least 200 gp, which the spell consumes.", range:touch, ritual:no, school:abjuration}).
spell_auto_data(goodberry, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[druid, ranger], components:[v, s, m("A sprig of mistletoe.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Up to ten berries appear in your hand and are infused with magic for the duration. A creature can use its action to eat one berry. Eating a berry restores 1 hit point, and the berry provides enough nourishment to sustain a creature for a day.", "The berries lose their potency if they have not been consumed within 24 hours of the casting of this spell."], duration:"Instantaneous", higher_level:no, level:1, material:"A sprig of mistletoe.", range:touch, ritual:no, school:transmutation}).
spell_auto_data(grease, properties{area_of_effect:10 ft cube, attack_type:false, casting_time:"1 action", classes:[wizard], components:[v, s, m("A bit of pork rind or butter.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:dex else none, desc:["Slick grease covers the ground in a 10-foot square centered on a point within range and turns it into difficult terrain for the duration.", "When the grease appears, each creature standing in its area must succeed on a dexterity saving throw or fall prone. A creature that enters the area or ends its turn there must also succeed on a dexterity saving throw or fall prone."], duration:"1 minute", higher_level:no, level:1, material:"A bit of pork rind or butter.", range:feet(60), ritual:no, school:conjuration}).
spell_auto_data('greater invisibility', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, wizard], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You or a creature you touch becomes invisible until the spell ends. Anything the target is wearing or carrying is invisible as long as it is on the target's person."], duration:"Up to 1 minute", higher_level:no, level:4, material:false, range:touch, ritual:no, school:illusion}).
spell_auto_data('greater restoration', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, cleric, druid], components:[v, s, m("Diamond dust worth at least 100gp, which the spell consumes.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You imbue a creature you touch with positive energy to undo a debilitating effect. You can reduce the target's exhaustion level by one, or end one of the following effects on the target:", "- One effect that charmed or petrified the target", "- One curse, including the target's attunement to a cursed magic item", "- Any reduction to one of the target's ability scores", "- One effect reducing the target's hit point maximum"], duration:"Instantaneous", higher_level:no, level:5, material:"Diamond dust worth at least 100gp, which the spell consumes.", range:touch, ritual:no, school:abjuration}).
spell_auto_data('guardian of faith', properties{area_of_effect:10 ft cylinder, attack_type:false, casting_time:"1 action", classes:[cleric, paladin], components:[v], concentration:no, damage_at_slot_level:_{4:damage(radiant, 20)}, damage_with_cantrip_scaling:false, dc:dex else half, desc:["A Large spectral guardian appears and hovers for the duration in an unoccupied space of your choice that you can see within range. The guardian occupies that space and is indistinct except for a gleaming sword and shield emblazoned with the symbol of your deity.", "Any creature hostile to you that moves to a space within 10 feet of the guardian for the first time on a turn must succeed on a dexterity saving throw. The creature takes 20 radiant damage on a failed save, or half as much damage on a successful one. The guardian vanishes when it has dealt a total of 60 damage."], duration:"8 hours", higher_level:no, level:4, material:false, range:feet(30), ritual:no, school:conjuration}).
spell_auto_data('guards and wards', properties{area_of_effect:2500 ft cube, attack_type:false, casting_time:"10 minutes", classes:[bard, wizard], components:[v, s, m("Burning incense, a small measure of brimstone and oil, a knotted string, a small amount of umber hulk blood, and a small silver rod worth at least 10 gp.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You create a ward that protects up to 2,500 square feet of floor space (an area 50 feet square, or one hundred 5-foot squares or twenty-five 10-foot squares). The warded area can be up to 20 feet tall, and shaped as you desire. You can ward several stories of a stronghold by dividing the area among them, as long as you can walk into each contiguous area while you are casting the spell.", "When you cast this spell, you can specify individuals that are unaffected by any or all of the effects that you choose. You can also specify a password that, when spoken aloud, makes the speaker immune to these effects.", "Guards and wards creates the following effects within the warded area.", "**Corridors.** Fog fills all the warded corridors, making them heavily obscured. In addition, at each intersection or branching passage offering a choice of direction, there is a 50 percent chance that a creature other than you will believe it is going in the opposite direction from the one it chooses.", "**Doors.** All doors in the warded area are magically locked, as if sealed by an arcane lock spell. In addition, you can cover up to ten doors with an illusion (equivalent to the illusory object function of the minor illusion spell) to make them appear as plain sections of wall.", "**Stairs.** Webs fill all stairs in the warded area from top to bottom, as the web spell. These strands regrow in 10 minutes if they are burned or torn away while guards and wards lasts.", "**Other Spell Effect.** You can place your choice of one of the following magical effects within the warded area of the stronghold.", "- Place dancing lights in four corridors. You can designate a simple program that the lights repeat as long as guards and wards lasts.", "- Place magic mouth in two locations.", "- Place stinking cloud in two locations. The vapors appear in the places you designate; they return within 10 minutes if dispersed by wind while guards and wards lasts.", "- Place a constant gust of wind in one corridor or room.", "- Place a suggestion in one location. You select an area of up to 5 feet square, and any creature that enters or passes through the area receives the suggestion mentally.", "The whole warded area radiates magic. A dispel magic cast on a specific effect, if successful, removes only that effect.", "You can create a permanently guarded and warded structure by casting this spell there every day for one year."], duration:"24 hours", higher_level:no, level:6, material:"Burning incense, a small measure of brimstone and oil, a knotted string, a small amount of umber hulk blood, and a small silver rod worth at least 10 gp.", range:touch, ritual:no, school:abjuration}).
spell_auto_data(guidance, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric, druid], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch one willing creature. Once before the spell ends, the target can roll a d4 and add the number rolled to one ability check of its choice. It can roll the die before or after making the ability check. The spell then ends."], duration:"Up to 1 minute", higher_level:no, level:0, material:false, range:touch, ritual:no, school:divination}).
spell_auto_data('guiding bolt', properties{area_of_effect:false, attack_type:ranged, casting_time:"1 action", classes:[cleric], components:[v, s], concentration:no, damage_at_slot_level:_{1:damage(radiant, 4 d 6), 2:damage(radiant, 5 d 6), 3:damage(radiant, 6 d 6), 4:damage(radiant, 7 d 6), 5:damage(radiant, 8 d 6), 6:damage(radiant, 9 d 6), 7:damage(radiant, 10 d 6), 8:damage(radiant, 11 d 6), 9:damage(radiant, 12 d 6)}, damage_with_cantrip_scaling:false, dc:false, desc:["A flash of light streaks toward a creature of your choice within range. Make a ranged spell attack against the target. On a hit, the target takes 4 d 6 radiant damage, and the next attack roll made against this target before the end of your next turn has advantage, thanks to the mystical dim light glittering on the target until then."], duration:"1 round", higher_level:"When you cast this spell using a spell slot of 2nd level or higher, the damage increases by 1 d 6 for each slot level above 1st.", level:1, material:false, range:feet(120), ritual:no, school:evocation}).
spell_auto_data('gust of wind', properties{area_of_effect:60 ft line, attack_type:false, casting_time:"1 action", classes:[druid, sorcerer, wizard], components:[v, s, m("A legume seed.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:str else none, desc:["A line of strong wind 60 feet long and 10 feet wide blasts from you in a direction you choose for the spell's duration. Each creature that starts its turn in the line must succeed on a strength saving throw or be pushed 15 feet away from you in a direction following the line.", "Any creature in the line must spend 2 feet of movement for every 1 foot it moves when moving closer to you.", "The gust disperses gas or vapor, and it extinguishes candles, torches, and similar unprotected flames in the area. It causes protected flames, such as those of lanterns, to dance wildly and has a 50 percent chance to extinguish them.", "As a bonus action on each of your turns before the spell ends, you can change the direction in which the line blasts from you."], duration:"Up to 1 minute", higher_level:no, level:2, material:"A legume seed.", range:self, ritual:no, school:evocation}).
spell_auto_data(hallow, properties{area_of_effect:60 ft sphere, attack_type:false, casting_time:"24 hours", classes:[cleric], components:[v, s, m("Herbs, oils, and incense worth at least 1,000 gp, which the spell consumes.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:cha else none, desc:["You touch a point and infuse an area around it with holy (or unholy) power. The area can have a radius up to 60 feet, and the spell fails if the radius includes an area already under the effect a hallow spell. The affected area is subject to the following effects.", "First, celestials, elementals, fey, fiends, and undead can't enter the area, nor can such creatures charm, frighten, or possess creatures within it. Any creature charmed, frightened, or possessed by such a creature is no longer charmed, frightened, or possessed upon entering the area. You can exclude one or more of those types of creatures from this effect.", "Second, you can bind an extra effect to the area. Choose the effect from the following list, or choose an effect offered by the DM. Some of these effects apply to creatures in the area; you can designate whether the effect applies to all creatures, creatures that follow a specific deity or leader, or creatures of a specific sort, such as ores or trolls. When a creature that would be affected enters the spell's area for the first time on a turn or starts its turn there, it can make a charisma saving throw. On a success, the creature ignores the extra effect until it leaves the area.", "**Courage.** Affected creatures can't be frightened while in the area.", "**Darkness.** Darkness fills the area. Normal light, as well as magical light created by spells of a lower level than the slot you used to cast this spell, can't illuminate the area.", "**Daylight.** Bright light fills the area. Magical darkness created by spells of a lower level than the slot you used to cast this spell can't extinguish the light.", "**Energy Protection.** Affected creatures in the area have resistance to one damage type of your choice, except for bludgeoning, piercing, or slashing.", "**Energy Vulnerability.** Affected creatures in the area have vulnerability to one damage type of your choice, except for bludgeoning, piercing, or slashing.", "**Everlasting Rest.** Dead bodies interred in the area can't be turned into undead.", "**Extradimensional Interference.** Affected creatures can't move or travel using teleportation or by extradimensional or interplanar means.", "**Fear.** Affected creatures are frightened while in the area.", "**Silence.** No sound can emanate from within the area, and no sound can reach into it.", "**Tongues.** Affected creatures can communicate with any other creature in the area, even if they don't share a common language."], duration:"Until dispelled", higher_level:no, level:5, material:"Herbs, oils, and incense worth at least 1,000 gp, which the spell consumes.", range:touch, ritual:no, school:evocation}).
spell_auto_data('hallucinatory terrain', properties{area_of_effect:150 ft cube, attack_type:false, casting_time:"10 minutes", classes:[bard, druid, warlock, wizard], components:[v, s, m("A stone, a twig, and a bit of green plant.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You make natural terrain in a 150-foot cube in range look, sound, and smell like some other sort of natural terrain. Thus, open fields or a road can be made to resemble a swamp, hill, crevasse, or some other difficult or impassable terrain. A pond can be made to seem like a grassy meadow, a precipice like a gentle slope, or a rock-strewn gully like a wide and smooth road. Manufactured structures, equipment, and creatures within the area aren't changed in appearance.", "The tactile characteristics of the terrain are unchanged, so creatures entering the area are likely to see through the illusion. If the difference isn't obvious by touch, a creature carefully examining the illusion can attempt an Intelligence (Investigation) check against your spell save DC to disbelieve it. A creature who discerns the illusion for what it is, sees it as a vague image superimposed on the terrain."], duration:"24 hours", higher_level:no, level:4, material:"A stone, a twig, and a bit of green plant.", range:feet(300), ritual:no, school:illusion}).
spell_auto_data(harm, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric], components:[v, s], concentration:no, damage_at_slot_level:_{6:damage(necrotic, 14 d 6)}, damage_with_cantrip_scaling:false, dc:con else half, desc:["You unleash a virulent disease on a creature that you can see within range. The target must make a constitution saving throw. On a failed save, it takes 14 d 6 necrotic damage, or half as much damage on a successful save. The damage can't reduce the target's hit points below 1. If the target fails the saving throw, its hit point maximum is reduced for 1 hour by an amount equal to the necrotic damage it took. Any effect that removes a disease allows a creature's hit point maximum to return to normal before that time passes."], duration:"Instantaneous", higher_level:no, level:6, material:false, range:feet(60), ritual:no, school:necromancy}).
spell_auto_data(haste, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s, m("A shaving of licorice root.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Choose a willing creature that you can see within range. Until the spell ends, the target's speed is doubled, it gains a +2 bonus to AC, it has advantage on dexterity saving throws, and it gains an additional action on each of its turns. That action can be used only to take the Attack (one weapon attack only), Dash, Disengage, Hide, or Use an Object action.", "When the spell ends, the target can't move or take actions until after its next turn, as a wave of lethargy sweeps over it."], duration:"Up to 1 minute", higher_level:no, level:3, material:"A shaving of licorice root.", range:feet(30), ritual:no, school:transmutation}).
spell_auto_data(heal, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric, druid], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Choose a creature that you can see within range. A surge of positive energy washes through the creature, causing it to regain 70 hit points. This spell also ends blindness, deafness, and any diseases affecting the target. This spell has no effect on constructs or undead."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 7th level or higher, the amount of healing increases by 10 for each slot level above 6th.", level:6, material:false, range:feet(60), ritual:no, school:evocation}).
spell_auto_data('healing word', properties{area_of_effect:false, attack_type:false, casting_time:"1 bonus action", classes:[bard, cleric, druid], components:[v], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["A creature of your choice that you can see within range regains hit points equal to 1 d 4 + your spellcasting ability modifier. This spell has no effect on undead or constructs."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 2nd level or higher, the healing increases by 1 d 4 for each slot level above 1st.", level:1, material:false, range:feet(60), ritual:no, school:evocation}).
spell_auto_data('heat metal', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, druid], components:[v, s, m("A piece of iron and a flame.")], concentration:yes, damage_at_slot_level:_{2:damage(fire, 2 d 8), 3:damage(fire, 3 d 8), 4:damage(fire, 4 d 8), 5:damage(fire, 5 d 8), 6:damage(fire, 6 d 8), 7:damage(fire, 7 d 8), 8:damage(fire, 8 d 8), 9:damage(fire, 9 d 8)}, damage_with_cantrip_scaling:false, dc:con else other, desc:["Choose a manufactured metal object, such as a metal weapon or a suit of heavy or medium metal armor, that you can see within range. You cause the object to glow red-hot. Any creature in physical contact with the object takes 2 d 8 fire damage when you cast the spell. Until the spell ends, you can use a bonus action on each of your subsequent turns to cause this damage again.", "If a creature is holding or wearing the object and takes the damage from it, the creature must succeed on a constitution saving throw or drop the object if it can. If it doesn't drop the object, it has disadvantage on attack rolls and ability checks until the start of your next turn."], duration:"Up to 1 minute", higher_level:"When you cast this spell using a spell slot of 3rd level or higher, the damage increases by 1 d 8 for each slot level above 2nd.", level:2, material:"A piece of iron and a flame.", range:feet(60), ritual:no, school:transmutation}).
spell_auto_data('hellish rebuke', properties{area_of_effect:false, attack_type:false, casting_time:"1 reaction", classes:[warlock], components:[v, s], concentration:no, damage_at_slot_level:_{1:damage(fire, 2 d 10)}, damage_with_cantrip_scaling:false, dc:dex else half, desc:["You point your finger, and the creature that damaged you is momentarily surrounded by hellish flames. The creature must make a dexterity saving throw. It takes 2 d 10 fire damage on a failed save, or half as much damage on a successful one."], duration:"Instantaneous", higher_level:no, level:1, material:false, range:feet(60), ritual:no, school:evocation}).
spell_auto_data('heroes\' feast', properties{area_of_effect:false, attack_type:false, casting_time:"10 minutes", classes:[cleric, druid], components:[v, s, m("A gem-encrusted bowl worth at least 1,000gp, which the spell consumes.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You bring forth a great feast, including magnificent food and drink. The feast takes 1 hour to consume and disappears at the end of that time, and the beneficial effects don't set in until this hour is over. Up to twelve other creatures can partake of the feast.", "A creature that partakes of the feast gains several benefits. The creature is cured of all diseases and poison, becomes immune to poison and being frightened, and makes all wisdom saving throws with advantage. Its hit point maximum also increases by 2 d 10, and it gains the same number of hit points. These benefits last for 24 hours."], duration:"Instantaneous", higher_level:no, level:6, material:"A gem-encrusted bowl worth at least 1,000gp, which the spell consumes.", range:feet(30), ritual:no, school:conjuration}).
spell_auto_data(heroism, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, paladin], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["A willing creature you touch is imbued with bravery. Until the spell ends, the creature is immune to being frightened and gains temporary hit points equal to your spellcasting ability modifier at the start of each of its turns. When the spell ends, the target loses any remaining temporary hit points from this spell."], duration:"Up to 1 minute", higher_level:no, level:1, material:false, range:touch, ritual:no, school:enchantment}).
spell_auto_data('hideous laughter', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, wizard], components:[v, s, m("Tiny tarts and a feather that is waved in the air.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else none, desc:["A creature of your choice that you can see within range perceives everything as hilariously funny and falls into fits of laughter if this spell affects it. The target must succeed on a wisdom saving throw or fall prone, becoming incapacitated and unable to stand up for the duration. A creature with an Intelligence score of 4 or less isn't affected.", "At the end of each of its turns, and each time it takes damage, the target can make another wisdom saving throw. The target had advantage on the saving throw if it's triggered by damage. On a success, the spell ends."], duration:"Up to 1 minute", higher_level:no, level:1, material:"Tiny tarts and a feather that is waved in the air.", range:feet(30), ritual:no, school:enchantment}).
spell_auto_data('hold monster', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, warlock, wizard], components:[v, s, m("A small piece of iron.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else none, desc:["Choose a creature you can see and reach. The target must make a saving throw of Wisdom or be paralyzed for the duration of the spell. This spell has no effect against the undead. At the end of each round, the target can make a new saving throw of Wisdom. If successful, the spell ends for the creature."], duration:"Up to 1 minute", higher_level:"When you cast this spell using a level 6 or higher location, you can target an additional creature for each level of location beyond the fifth. The creatures must be within 30 feet o f each other when you target them.", level:5, material:"A small piece of iron.", range:feet(90), ritual:no, school:enchantment}).
spell_auto_data('hold person', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, cleric, druid, sorcerer, warlock, wizard], components:[v, s, m("A small, straight piece of iron.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else none, desc:["Choose a humanoid that you can see within range. The target must succeed on a wisdom saving throw or be paralyzed for the duration. At the end of each of its turns, the target can make another wisdom saving throw. On a success, the spell ends on the target."], duration:"Up to 1 minute", higher_level:"When you cast this spell using a spell slot of 3rd level or higher, you can target one additional humanoid for each slot level above 2nd. The humanoids must be within 30 feet of each other when you target them.", level:2, material:"A small, straight piece of iron.", range:feet(60), ritual:no, school:enchantment}).
spell_auto_data('holy aura', properties{area_of_effect:30 ft sphere, attack_type:false, casting_time:"1 action", classes:[cleric], components:[v, s, m("A tiny reliquary worth at least 1,000gp containing a sacred relic, such as a scrap of cloth from a saint's robe or a piece of parchment from a religious text.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Divine light washes out from you and coalesces in a soft radiance in a 30-foot radius around you. Creatures of your choice in that radius when you cast this spell shed dim light in a 5-foot radius and have advantage on all saving throws, and other creatures have disadvantage on attack rolls against them until the spell ends. In addition, when a fiend or an undead hits an affected creature with a melee attack, the aura flashes with brilliant light. The attacker must succeed on a constitution saving throw or be blinded until the spell ends."], duration:"Up to 1 minute", higher_level:no, level:8, material:"A tiny reliquary worth at least 1,000gp containing a sacred relic, such as a scrap of cloth from a saint's robe or a piece of parchment from a religious text.", range:self, ritual:no, school:abjuration}).
spell_auto_data('hunter\'s mark', properties{area_of_effect:false, attack_type:false, casting_time:"1 bonus action", classes:[paladin, ranger], components:[v], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You choose a creature you can see within range and mystically mark it as your quarry. Until the spell ends, you deal an extra 1 d 6 damage to the target whenever you hit it with a weapon attack, and you have advantage on any Wisdom (Perception) or Wisdom (Survival) check you make to find it. If the target drops to 0 hit points before this spell ends, you can use a bonus action on a subsequent turn of yours to mark a new creature."], duration:"Up to 1 hour", higher_level:"When you cast this spell using a spell slot of 3rd or 4th level, you can maintain your concentration on the spell for up to 8 hours. When you use a spell slot of 5th level or higher, you can maintain your concentration on the spell for up to 24 hours.", level:1, material:false, range:feet(90), ritual:no, school:divination}).
spell_auto_data('hypnotic pattern', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, warlock, wizard], components:[s, m("A glowing stick of incense or a crystal vial filled with phosphorescent material.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else none, desc:["You create a twisting pattern of colors that weaves through the air inside a 30-foot cube within range. The pattern appears for a moment and vanishes. Each creature in the area who sees the pattern must make a wisdom saving throw. On a failed save, the creature becomes charmed for the duration. While charmed by this spell, the creature is incapacitated and has a speed of 0.", "The spell ends for an affected creature if it takes any damage or if someone else uses an action to shake the creature out of its stupor."], duration:"Up to 1 minute", higher_level:no, level:3, material:"A glowing stick of incense or a crystal vial filled with phosphorescent material.", range:feet(120), ritual:no, school:illusion}).
spell_auto_data('ice storm', properties{area_of_effect:20 ft cylinder, attack_type:false, casting_time:"1 action", classes:[druid, sorcerer, wizard], components:[v, s, m("A pinch of dust and a few drops of water.")], concentration:no, damage_at_slot_level:_{4:damage(bludgeoning, 2 d 8)+damage(cold, 4 d 6), 5:damage(bludgeoning, 3 d 8)+damage(cold, 4 d 6), 6:damage(bludgeoning, 4 d 8)+damage(cold, 4 d 6), 7:damage(bludgeoning, 5 d 8)+damage(cold, 4 d 6), 8:damage(bludgeoning, 6 d 8)+damage(cold, 4 d 6), 9:damage(bludgeoning, 7 d 8)+damage(cold, 4 d 6)}, damage_with_cantrip_scaling:false, dc:dex else half, desc:["A hail of rock-hard ice pounds to the ground in a 20-foot-radius, 40-foot-high cylinder centered on a point within range. Each creature in the cylinder must make a dexterity saving throw. A creature takes 2 d 8 bludgeoning damage and 4 d 6 cold damage on a failed save, or half as much damage on a successful one.", "Hailstones turn the storm's area of effect into difficult terrain until the end of your next turn."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 5th level or higher, the bludgeoning damage increases by 1 d 8 for each slot level above 4th.", level:4, material:"A pinch of dust and a few drops of water.", range:feet(300), ritual:no, school:evocation}).
spell_auto_data(identify, properties{area_of_effect:false, attack_type:false, casting_time:"1 minute", classes:[bard, wizard], components:[v, s, m("A pearl worth at least 100gp and an owl feather.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You choose one object that you must touch throughout the casting of the spell. If it is a magic item or some other magic-imbued object, you learn its properties and how to use them, whether it requires attunement to use, and how many charges it has, if any. You learn whether any spells are affecting the item and what they are. If the item was created by a spell, you learn which spell created it.", "If you instead touch a creature throughout the casting, you learn what spells, if any, are currently affecting it."], duration:"Instantaneous", higher_level:no, level:1, material:"A pearl worth at least 100gp and an owl feather.", range:touch, ritual:yes, school:divination}).
spell_auto_data('illusory script', properties{area_of_effect:false, attack_type:false, casting_time:"1 minute", classes:[bard, warlock, wizard], components:[s, m("A lead-based ink worth at least 10gp, which this spell consumes.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You write on parchment, paper, or some other suitable writing material and imbue it with a potent illusion that lasts for the duration.", "To you and any creatures you designate when you cast the spell, the writing appears normal, written in your hand, and conveys whatever meaning you intended when you wrote the text. To all others, the writing appears as if it were written in an unknown or magical script that is unintelligible. Alternatively, you can cause the writing to appear to be an entirely different message, written in a different hand and language, though the language must be one you know.", "Should the spell be dispelled, the original script and the illusion both disappear.", "A creature with truesight can read the hidden message."], duration:"10 days", higher_level:no, level:1, material:"A lead-based ink worth at least 10gp, which this spell consumes.", range:touch, ritual:yes, school:illusion}).
spell_auto_data(imprisonment, properties{area_of_effect:false, attack_type:false, casting_time:"1 minute", classes:[warlock, wizard], components:[v, s, m("A vellum depiction or a carved statuette in the likeness of the target, and a special component that varies according to the version of the spell you choose, worth at least 500gp per Hit Die of the target.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else none, desc:["You create a magical restraint to hold a creature that you can see within range. The target must succeed on a wisdom saving throw or be bound by the spell; if it succeeds, it is immune to this spell if you cast it again. While affected by this spell, the creature doesn't need to breathe, eat, or drink, and it doesn't age. Divination spells can't locate or perceive the target.", "When you cast the spell, you choose one of the following forms of imprisonment.", "**Burial.** The target is entombed far beneath the earth in a sphere of magical force that is just large enough to contain the target. Nothing can pass through the sphere, nor can any creature teleport or use planar travel to get into or out of it.", "The special component for this version of the spell is a small mithral orb.", "**Chaining.** Heavy chains, firmly rooted in the ground, hold the target in place. The target is restrained until the spell ends, and it can't move or be moved by any means until then.", "The special component for this version of the spell is a fine chain of precious metal.", "**Hedged Prison.** The spell transports the target into a tiny demiplane that is warded against teleportation and planar travel. The demiplane can be a labyrinth, a cage, a tower, or any similar confined structure or area of your choice.", "The special component for this version of the spell is a miniature representation of the prison made from jade.", "**Minimus Containment.** The target shrinks to a height of 1 inch and is imprisoned inside a gemstone or similar object. Light can pass through the gemstone normally (allowing the target to see out and other creatures to see in), but nothing else can pass through, even by means of teleportation or planar travel. The gemstone can't be cut or broken while the spell remains in effect.", "The special component for this version of the spell is a large, transparent gemstone, such as a corundum, diamond, or ruby.", "**Slumber.** The target falls asleep and can't be awoken.", "The special component for this version of the spell consists of rare soporific herbs.", "**Ending the Spell.** During the casting of the spell, in any of its versions, you can specify a condition that will cause the spell to end and release the target. The condition can be as specific or as elaborate as you choose, but the DM must agree that the condition is reasonable and has a likelihood of coming to pass. The conditions can be based on a creature's name, identity, or deity but otherwise must be based on observable actions or qualities and not based on intangibles such as level, class, or hit points.", "A dispel magic spell can end the spell only if it is cast as a 9th-level spell, targeting either the prison or the special component used to create it.", "You can use a particular special component to create only one prison at a time. If you cast the spell again using the same component, the target of the first casting is immediately freed from its binding."], duration:"Until dispelled", higher_level:no, level:9, material:"A vellum depiction or a carved statuette in the likeness of the target, and a special component that varies according to the version of the spell you choose, worth at least 500gp per Hit Die of the target.", range:feet(30), ritual:no, school:abjuration}).
spell_auto_data('incendiary cloud', properties{area_of_effect:20 ft sphere, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s], concentration:yes, damage_at_slot_level:_{8:damage(fire, 10 d 8)}, damage_with_cantrip_scaling:false, dc:dex else half, desc:["A swirling cloud of smoke shot through with white-hot embers appears in a 20-foot-radius sphere centered on a point within range. The cloud spreads around corners and is heavily obscured. It lasts for the duration or until a wind of moderate or greater speed (at least 10 miles per hour) disperses it.", "When the cloud appears, each creature in it must make a dexterity saving throw. A creature takes 10 d 8 fire damage on a failed save, or half as much damage on a successful one. A creature must also make this saving throw when it enters the spell's area for the first time on a turn or ends its turn there.", "The cloud moves 10 feet directly away from you in a direction that you choose at the start of each of your turns."], duration:"Up to 1 minute", higher_level:no, level:8, material:false, range:feet(150), ritual:no, school:conjuration}).
spell_auto_data('inflict wounds', properties{area_of_effect:false, attack_type:melee, casting_time:"1 action", classes:[cleric], components:[v, s], concentration:no, damage_at_slot_level:_{1:damage(necrotic, 3 d 10), 2:damage(necrotic, 4 d 10), 3:damage(necrotic, 5 d 10), 4:damage(necrotic, 6 d 10), 5:damage(necrotic, 7 d 10), 6:damage(necrotic, 8 d 10), 7:damage(necrotic, 9 d 10), 8:damage(necrotic, 10 d 10), 9:damage(necrotic, 11 d 10)}, damage_with_cantrip_scaling:false, dc:false, desc:["Make a melee spell attack against a creature you can reach. On a hit, the target takes 3 d 10 necrotic damage."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 2nd level or higher, the damage increases by 1 d 10 for each slot level above 1st.", level:1, material:false, range:touch, ritual:no, school:necromancy}).
spell_auto_data('insect plague', properties{area_of_effect:20 ft sphere, attack_type:false, casting_time:"1 action", classes:[cleric, druid, sorcerer], components:[v, s, m("A few grains of sugar, some kernels of grain, and a smear of fat.")], concentration:yes, damage_at_slot_level:_{5:damage(piercing, 4 d 10), 6:damage(piercing, 5 d 10), 7:damage(piercing, 6 d 10), 8:damage(piercing, 7 d 10), 9:damage(piercing, 8 d 10)}, damage_with_cantrip_scaling:false, dc:con else half, desc:["Swarming, biting locusts fill a 20-foot-radius sphere centered on a point you choose within range. The sphere spreads around corners. The sphere remains for the duration, and its area is lightly obscured. The sphere's area is difficult terrain.", "When the area appears, each creature in it must make a constitution saving throw. A creature takes 4 d 10 piercing damage on a failed save, or half as much damage on a successful one. A creature must also make this saving throw when it enters the spell's area for the first time on a turn or ends its turn there."], duration:"Up to 10 minutes", higher_level:"When you cast this spell using a spell slot of 6th level or higher, the damage increases by 1 d 10 for each slot level above 5th.", level:5, material:"A few grains of sugar, some kernels of grain, and a smear of fat.", range:feet(300), ritual:no, school:conjuration}).
spell_auto_data('instant summons', properties{area_of_effect:false, attack_type:false, casting_time:"1 minute", classes:[wizard], components:[v, s, m("A sapphire worth 1,000 gp.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch an object weighing 10 pounds or less whose longest dimension is 6 feet or less. The spell leaves an invisible mark on its surface and invisibly inscribes the name of the item on the sapphire you use as the material component. Each time you cast this spell, you must use a different sapphire.", "At any time thereafter, you can use your action to speak the item's name and crush the sapphire. The item instantly appears in your hand regardless of physical or planar distances, and the spell ends.", "If another creature is holding or carrying the item, crushing the sapphire doesn't transport the item to you, but instead you learn who the creature possessing the object is and roughly where that creature is located at that moment.", "Dispel magic or a similar effect successfully applied to the sapphire ends this spell's effect."], duration:"Until dispelled", higher_level:no, level:6, material:"A sapphire worth 1,000 gp.", range:touch, ritual:yes, school:conjuration}).
spell_auto_data(invisibility, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, warlock, wizard], components:[v, s, m("An eyelash encased in gum arabic.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["A creature you touch becomes invisible until the spell ends. Anything the target is wearing or carrying is invisible as long as it is on the target's person. The spell ends for a target that attacks or casts a spell."], duration:"Up to 1 hour", higher_level:"When you cast this spell using a spell slot of 3rd level or higher, you can target one additional creature for each slot level above 2nd.", level:2, material:"An eyelash encased in gum arabic.", range:touch, ritual:no, school:illusion}).
spell_auto_data('irresistible dance', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, wizard], components:[v], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Choose one creature that you can see within range. The target begins a comic dance in place: shuffling, tapping its feet, and capering for the duration. Creatures that can't be charmed are immune to this spell.", "A dancing creature must use all its movement to dance without leaving its space and has disadvantage on dexterity saving throws and attack rolls. While the target is affected by this spell, other creatures have advantage on attack rolls against it. As an action, a dancing creature makes a wisdom saving throw to regain control of itself. On a successful save, the spell ends."], duration:"Up to 1 minute", higher_level:no, level:6, material:false, range:feet(30), ritual:no, school:enchantment}).
spell_auto_data(jump, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[druid, ranger, sorcerer, wizard], components:[v, s, m("A grasshopper's hind leg.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch a creature. The creature's jump distance is tripled until the spell ends."], duration:"1 minute", higher_level:no, level:1, material:"A grasshopper's hind leg.", range:touch, ritual:no, school:transmutation}).
spell_auto_data(knock, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, wizard], components:[v], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Choose an object that you can see within range. The object can be a door, a box, a chest, a set of manacles, a padlock, or another object that contains a mundane or magical means that prevents access.", "A target that is held shut by a mundane lock or that is stuck or barred becomes unlocked, unstuck, or unbarred. If the object has multiple locks, only one of them is unlocked.", "If you choose a target that is held shut with arcane lock, that spell is suppressed for 10 minutes, during which time the target can be opened and shut normally.", "When you cast the spell, a loud knock, audible from as far away as 300 feet, emanates from the target object."], duration:"Instantaneous", higher_level:no, level:2, material:false, range:feet(60), ritual:no, school:transmutation}).
spell_auto_data('legend lore', properties{area_of_effect:false, attack_type:false, casting_time:"10 minutes", classes:[bard, cleric, wizard], components:[v, s, m("Incense worth at least 250 gp, which the spell consumes, and four ivory strips worth at least 50 gp each.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Name or describe a person, place, or object. The spell brings to your mind a brief summary of the significant lore about the thing you named. The lore might consist of current tales, forgotten stories, or even secret lore that has never been widely known. If the thing you named isn't of legendary importance, you gain no information. The more information you already have about the thing, the more precise and detailed the information you receive is.", "The information you learn is accurate but might be couched in figurative language. For example, if you have a mysterious magic axe on hand the spell might yield this information: \"Woe to the evildoer whose hand touches the axe, for even the haft slices the hand of the evil ones. Only a true Child of Stone, lover and beloved of Moradin, may awaken the true powers of the axe, and only with the sacred word *Rudnogg* on the lips.\""], duration:"Instantaneous", higher_level:no, level:5, material:"Incense worth at least 250 gp, which the spell consumes, and four ivory strips worth at least 50 gp each.", range:self, ritual:no, school:divination}).
spell_auto_data('lesser restoration', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, cleric, druid, paladin, ranger], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch a creature and can end either one disease or one condition afflicting it. The condition can be blinded, deafened, paralyzed, or poisoned."], duration:"Instantaneous", higher_level:no, level:2, material:false, range:touch, ritual:no, school:abjuration}).
spell_auto_data(levitate, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s, m("Either a small leather loop or a piece of golden wire bent into a cup shape with a long shank on one end.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["One creature or object of your choice that you can see within range rises vertically, up to 20 feet, and remains suspended there for the duration. The spell can levitate a target that weighs up to 500 pounds. An unwilling creature that succeeds on a constitution saving throw is unaffected.", "The target can move only by pushing or pulling against a fixed object or surface within reach (such as a wall or a ceiling), which allows it to move as if it were climbing. You can change the target's altitude by up to 20 feet in either direction on your turn. If you are the target, you can move up or down as part of your move. Otherwise, you can use your action to move the target, which must remain within the spell's range.", "When the spell ends, the target floats gently to the ground if it is still aloft."], duration:"Up to 10 minutes", higher_level:no, level:2, material:"Either a small leather loop or a piece of golden wire bent into a cup shape with a long shank on one end.", range:feet(60), ritual:no, school:transmutation}).
spell_auto_data(light, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, cleric, sorcerer, wizard], components:[v, m("A firefly or phosphorescent moss.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:dex else none, desc:["You touch one object that is no larger than 10 feet in any dimension. Until the spell ends, the object sheds bright light in a 20-foot radius and dim light for an additional 20 feet. The light can be colored as you like. Completely covering the object with something opaque blocks the light. The spell ends if you cast it again or dismiss it as an action.", "If you target an object held or worn by a hostile creature, that creature must succeed on a dexterity saving throw to avoid the spell."], duration:"1 hour", higher_level:no, level:0, material:"A firefly or phosphorescent moss.", range:touch, ritual:no, school:evocation}).
spell_auto_data('lightning bolt', properties{area_of_effect:100 ft line, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s, m("A bit of fur and a rod of amber, crystal, or glass.")], concentration:no, damage_at_slot_level:_{3:damage(lightning, 8 d 6), 4:damage(lightning, 9 d 6), 5:damage(lightning, 10 d 6), 6:damage(lightning, 11 d 6), 7:damage(lightning, 12 d 6), 8:damage(lightning, 13 d 6), 9:damage(lightning, 14 d 6)}, damage_with_cantrip_scaling:false, dc:dex else half, desc:["A stroke of lightning forming a line 100 feet long and 5 feet wide blasts out from you in a direction you choose. Each creature in the line must make a dexterity saving throw. A creature takes 8 d 6 lightning damage on a failed save, or half as much damage on a successful one.", "The lightning ignites flammable objects in the area that aren't being worn or carried."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 4th level or higher, the damage increases by 1 d 6 for each slot level above 3rd.", level:3, material:"A bit of fur and a rod of amber, crystal, or glass.", range:self, ritual:no, school:evocation}).
spell_auto_data('locate animals or plants', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, druid, ranger], components:[v, s, m("A bit of fur from a bloodhound.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Describe or name a specific kind of beast or plant. Concentrating on the voice of nature in your surroundings, you learn the direction and distance to the closest creature or plant of that kind within 5 miles, if any are present."], duration:"Instantaneous", higher_level:no, level:2, material:"A bit of fur from a bloodhound.", range:self, ritual:yes, school:divination}).
spell_auto_data('locate creature', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, cleric, druid, paladin, ranger, wizard], components:[v, s, m("A bit of fur from a bloodhound.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Describe or name a creature that is familiar to you. You sense the direction to the creature's location, as long as that creature is within 1,000 feet of you. If the creature is moving, you know the direction of its movement.", "The spell can locate a specific creature known to you, or the nearest creature of a specific kind (such as a human or a unicorn), so long as you have seen such a creature up close--within 30 feet--at least once. If the creature you described or named is in a different form, such as being under the effects of a polymorph spell, this spell doesn't locate the creature.", "This spell can't locate a creature if running water at least 10 feet wide blocks a direct path between you and the creature."], duration:"Up to 1 hour", higher_level:no, level:4, material:"A bit of fur from a bloodhound.", range:self, ritual:no, school:divination}).
spell_auto_data('locate object', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, cleric, druid, paladin, ranger, wizard], components:[v, s, m("A forked twig.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Describe or name an object that is familiar to you. You sense the direction to the object's location, as long as that object is within 1,000 feet of you. If the object is in motion, you know the direction of its movement.", "The spell can locate a specific object known to you, as long as you have seen it up close--within 30 feet--at least once. Alternatively, the spell can locate the nearest object of a particular kind, such as a certain kind of apparel, jewelry, furniture, tool, or weapon.", "This spell can't locate an object if any thickness of lead, even a thin sheet, blocks a direct path between you and the object."], duration:"Up to 10 minutes", higher_level:no, level:2, material:"A forked twig.", range:self, ritual:no, school:divination}).
spell_auto_data(longstrider, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, druid, ranger, wizard], components:[v, s, m("A pinch of dirt.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch a creature. The target's speed increases by 10 feet until the spell ends."], duration:"1 hour", higher_level:"When you cast this spell using a spell slot of 2nd level or higher, you can target one additional creature for each spell slot above 1st.", level:1, material:"A pinch of dirt.", range:touch, ritual:no, school:transmutation}).
spell_auto_data('mage armor', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s, m("A piece of cured leather.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch a willing creature who isn't wearing armor, and a protective magical force surrounds it until the spell ends. The target's base AC becomes 13 + its Dexterity modifier. The spell ends if the target dons armor or if you dismiss the spell as an action."], duration:"8 hours", higher_level:no, level:1, material:"A piece of cured leather.", range:touch, ritual:no, school:abjuration}).
spell_auto_data('mage hand', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, warlock, wizard], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["A spectral, floating hand appears at a point you choose within range. The hand lasts for the duration or until you dismiss it as an action. The hand vanishes if it is ever more than 30 feet away from you or if you cast this spell again.", "You can use your action to control the hand. You can use the hand to manipulate an object, open an unlocked door or container, stow or retrieve an item from an open container, or pour the contents out of a vial. You can move the hand up to 30 feet each time you use it.", "The hand can't attack, activate magic items, or carry more than 10 pounds."], duration:"1 minute", higher_level:no, level:0, material:false, range:feet(30), ritual:no, school:conjuration}).
spell_auto_data('magic circle', properties{area_of_effect:10 ft cylinder, attack_type:false, casting_time:"1 minute", classes:[cleric, paladin, warlock, wizard], components:[v, s, m("Holy water or powdered silver and iron worth at least 100 gp, which the spell consumes.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:cha else other, desc:["You create a 10-foot radius, 20-foot-tall cylinder of magical energy centered on a point on the ground that you can see within range. Glowing runes appear whetever the cylinder intersects with the floor or other surface.", "Choose one or more of the following types of creatures: celestials, elementals, fey, fiends, or undead. The circle affects a creature of the chosen type in the following ways:", "- The creature can't willingly enter the cylinder by nonmagical means. If the creature tries to use teleportation or interplanar travel to do so, it must first succeed on a charisma saving throw.", "- The creature has disadvantage on attack rolls against targets within the cylinder.", "- Targets within the cylinder can't be charmed, frightened, or possessed by the creature.", "When you cast this spell, you can elect to cause its magic to operate in the reverse direction, preventing a creature of the specified type from leaving the cylinder and protecting targets outside it."], duration:"1 hour", higher_level:"When you cast this spell using a spell slot of 4th level or higher, the duration increases by 1 hour for each slot level above 3rd.", level:3, material:"Holy water or powdered silver and iron worth at least 100 gp, which the spell consumes.", range:feet(10), ritual:no, school:abjuration}).
spell_auto_data('magic jar', properties{area_of_effect:false, attack_type:false, casting_time:"1 minute", classes:[wizard], components:[v, s, m("A gem, crystal, reliquary, or some other ornamental container worth at least 500 gp.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:cha else other, desc:["Your body falls into a catatonic state as your soul leaves it and enters the container you used for the spell's material component. While your soul inhabits the container, you are aware of your surroundings as if you were in the container's space. You can't move or use reactions. The only action you can take is to project your soul up to 100 feet out of the container, either returning to your living body (and ending the spell) or attempting to possess a humanoids body.", "You can attempt to possess any humanoid within 100 feet of you that you can see (creatures warded by a protection from evil and good or magic circle spell can't be possessed). The target must make a charisma saving throw. On a failure, your soul moves into the target's body, and the target's soul becomes trapped in the container. On a success, the target resists your efforts to possess it, and you can't attempt to possess it again for 24 hours.", "Once you possess a creature's body, you control it. Your game statistics are replaced by the statistics of the creature, though you retain your alignment and your Intelligence, Wisdom, and Charisma scores. You retain the benefit of your own class features. If the target has any class levels, you can't use any of its class features.", "Meanwhile, the possessed creature's soul can perceive from the container using its own senses, but it can't move or take actions at all.", "While possessing a body, you can use your action to return from the host body to the container if it is within 100 feet of you, returning the host creature's soul to its body. If the host body dies while you're in it, the creature dies, and you must make a charisma saving throw against your own spellcasting DC. On a success, you return to the container if it is within 100 feet of you. Otherwise, you die.", "If the container is destroyed or the spell ends, your soul immediately returns to your body. If your body is more than 100 feet away from you or if your body is dead when you attempt to return to it, you die. If another creature's soul is in the container when it is destroyed, the creature's soul returns to its body if the body is alive and within 100 feet. Otherwise, that creature dies.", "When the spell ends, the container is destroyed."], duration:"Until dispelled", higher_level:no, level:6, material:"A gem, crystal, reliquary, or some other ornamental container worth at least 500 gp.", range:self, ritual:no, school:necromancy}).
spell_auto_data('magic missile', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s], concentration:no, damage_at_slot_level:_{1:damage(force, 1 d 4+1), 2:damage(force, 1 d 4+1), 3:damage(force, 1 d 4+1), 4:damage(force, 1 d 4+1), 5:damage(force, 1 d 4+1), 6:damage(force, 1 d 4+1), 7:damage(force, 1 d 4+1), 8:damage(force, 1 d 4+1), 9:damage(force, 1 d 4+1)}, damage_with_cantrip_scaling:false, dc:false, desc:["You create three glowing darts of magical force. Each dart hits a creature of your choice that you can see within range. A dart deals 1 d 4 + 1 force damage to its target. The darts all strike simultaneously, and you can direct them to hit one creature or several."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 2nd level or higher, the spell creates one more dart for each slot level above 1st.", level:1, material:false, range:feet(120), ritual:no, school:evocation}).
spell_auto_data('magic mouth', properties{area_of_effect:false, attack_type:false, casting_time:"1 minute", classes:[bard, wizard], components:[v, s, m("A honeycomb and jade dust of at least 10 inches, the spell consumes.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You plant a message to an object in the range of the spell. The message is verbalized when the trigger conditions are met. Choose an object that you see, and that is not worn or carried by another creature. Then say the message, which should not exceed 25 words but listening can take up to 10 minutes. Finally, establish the circumstances that trigger the spell to deliver your message.", "When these conditions are satisfied, a magical mouth appears on the object and it articulates the message imitating your voice, the same tone used during implantation of the message. If the selected object has a mouth or something that approaches such as the mouth of a statue, the magic mouth come alive at this point, giving the illusion that the words come from the mouth of the object.", "When you cast this spell, you may decide that the spell ends when the message is delivered or it can persist and repeat the message whenever circumstances occur.", "The triggering circumstance can be as general or as detailed as you like, though it must be based on visual or audible conditions that occur within 30 feet of the object. For example, you could instruct the mouth to speak when any creature moves within 30 feet of the object or when a silver bell rings within 30 feet of it."], duration:"Until dispelled", higher_level:no, level:2, material:"A honeycomb and jade dust of at least 10 inches, the spell consumes.", range:feet(30), ritual:yes, school:illusion}).
spell_auto_data('magic weapon', properties{area_of_effect:false, attack_type:false, casting_time:"1 bonus action", classes:[paladin, wizard], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch a nonmagical weapon. Until the spell ends, that weapon becomes a magic weapon with a +1 bonus to attack rolls and damage rolls."], duration:"Up to 1 hour", higher_level:"When you cast this spell using a spell slot of 4th level or higher, the bonus increases to +2. When you use a spell slot of 6th level or higher, the bonus increases to +3.", level:2, material:false, range:touch, ritual:no, school:transmutation}).
spell_auto_data('magnificent mansion', properties{area_of_effect:5 ft cube, attack_type:false, casting_time:"1 minute", classes:[bard, wizard], components:[v, s, m("A miniature portal carved from ivory, a small piece of polished marble, and a tiny silver spoon, each item worth at least 5 gp.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You conjure an extradimensional dwelling in range that lasts for the duration. You choose where its one entrance is located. The entrance shimmers faintly and is 5 feet wide and 10 feet tall. You and any creature you designate when you cast the spell can enter the extradimensional dwelling as long as the portal remains open. You can open or close the portal if you are within 30 feet of it. While closed, the portal is invisible.", "Beyond the portal is a magnificent foyer with numerous chambers beyond. The atmosphere is clean, fresh, and warm.", "You can create any floor plan you like, but the space can't exceed 50 cubes, each cube being 10 feet on each side. The place is furnished and decorated as you choose. It contains sufficient food to serve a nine course banquet for up to 100 people. A staff of 100 near-transparent servants attends all who enter. You decide the visual appearance of these servants and their attire. They are completely obedient to your orders. Each servant can perform any task a normal human servant could perform, but they can't attack or take any action that would directly harm another creature. Thus the servants can fetch things, clean, mend, fold clothes, light fires, serve food, pour wine, and so on. The servants can go anywhere in the mansion but can't leave it. Furnishings and other objects created by this spell dissipate into smoke if removed from the mansion. When the spell ends, any creatures inside the extradimensional space are expelled into the open spaces nearest to the entrance."], duration:"24 hours", higher_level:no, level:7, material:"A miniature portal carved from ivory, a small piece of polished marble, and a tiny silver spoon, each item worth at least 5 gp.", range:feet(300), ritual:no, school:conjuration}).
spell_auto_data('major image', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, warlock, wizard], components:[v, s, m("A bit of fleece.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You create the image of an object, a creature, or some other visible phenomenon that is no larger than a 20-foot cube. The image appears at a spot that you can see within range and lasts for the duration. It seems completely real, including sounds, smells, and temperature appropriate to the thing depicted. You can't create sufficient heat or cold to cause damage, a sound loud enough to deal thunder damage or deafen a creature, or a smell that might sicken a creature (like a troglodyte's stench).", "As long as you are within range of the illusion, you can use your action to cause the image to move to any other spot within range. As the image changes location, you can alter its appearance so that its movements appear natural for the image. For example, if you create an image of a creature and move it, you can alter the image so that it appears to be walking. Similarly, you can cause the illusion to make different sounds at different times, even making it carry on a conversation, for example.", "Physical interaction with the image reveals it to be an illusion, because things can pass through it. A creature that uses its action to examine the image can determine that it is an illusion with a successful Intelligence (Investigation) check against your spell save DC. If a creature discerns the illusion for what it is, the creature can see through the image, and its other sensory qualities become faint to the creature."], duration:"Up to 10 minutes", higher_level:"When you cast this spell using a spell slot of 6th level or higher, the spell lasts until dispelled, without requiring your concentration.", level:3, material:"A bit of fleece.", range:feet(120), ritual:no, school:illusion}).
spell_auto_data('mass cure wounds', properties{area_of_effect:30 ft sphere, attack_type:false, casting_time:"1 action", classes:[bard, cleric, druid], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["A wave of healing energy washes out from a point of your choice within range. Choose up to six creatures in a 30-foot-radius sphere centered on that point. Each target regains hit points equal to 3 d 8 + your spellcasting ability modifier. This spell has no effect on undead or constructs."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 6th level or higher, the healing increases by 1 d 8 for each slot level above 5th.", level:5, material:false, range:feet(60), ritual:no, school:conjuration}).
spell_auto_data('mass heal', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["A flood of healing energy flows from you into injured creatures around you. You restore up to 700 hit points, divided as you choose among any number of creatures that you can see within range. Creatures healed by this spell are also cured of all diseases and any effect making them blinded or deafened. This spell has no effect on undead or constructs."], duration:"Instantaneous", higher_level:no, level:9, material:false, range:feet(60), ritual:no, school:conjuration}).
spell_auto_data('mass healing word', properties{area_of_effect:false, attack_type:false, casting_time:"1 bonus action", classes:[cleric], components:[v], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["As you call out words of restoration, up to six creatures of your choice that you can see within range regain hit points equal to 1 d 4 + your spellcasting ability modifier. This spell has no effect on undead or constructs."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 4th level or higher, the healing increases by 1 d 4 for each slot level above 3rd.", level:3, material:false, range:feet(60), ritual:no, school:evocation}).
spell_auto_data('mass suggestion', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, warlock, wizard], components:[v, m("A snake's tongue and either a bit of honeycomb or a drop of sweet oil.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else none, desc:["You suggest a course of activity (limited to a sentence or two) and magically influence up to twelve creatures of your choice that you can see within range and that can hear and understand you. Creatures that can't be charmed are immune to this effect. The suggestion must be worded in such a manner as to make the course of action sound reasonable. Asking the creature to stab itself, throw itself onto a spear, immolate itself, or do some other obviously harmful act automatically negates the effect of the spell.", "Each target must make a wisdom saving throw. On a failed save, it pursues the course of action you described to the best of its ability. The suggested course of action can continue for the entire duration. If the suggested activity can be completed in a shorter time, the spell ends when the subject finishes what it was asked to do.", "You can also specify conditions that will trigger a special activity during the duration. For example, you might suggest that a group of soldiers give all their money to the first beggar they meet. If the condition isn't met before the spell ends, the activity isn't performed.", "If you or any of your companions damage a creature affected by this spell, the spell ends for that creature."], duration:"24 hours", higher_level:"When you cast this spell using a 7th-level spell slot, the duration is 10 days. When you use an 8th-level spell slot, the duration is 30 days. When you use a 9th-level spell slot, the duration is a year and a day.", level:6, material:"A snake's tongue and either a bit of honeycomb or a drop of sweet oil.", range:feet(60), ritual:no, school:enchantment}).
spell_auto_data(maze, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[wizard], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You banish a creature that you can see within range into a labyrinthine demiplane. The target remains there for the duration or until it escapes the maze.", "The target can use its action to attempt to escape. When it does so, it makes a DC 20 Intelligence check. If it succeeds, it escapes, and the spell ends (a minotaur or goristro demon automatically succeeds).", "When the spell ends, the target reappears in the space it left or, if that space is occupied, in the nearest unoccupied space."], duration:"Up to 10 minutes", higher_level:no, level:8, material:false, range:feet(60), ritual:no, school:conjuration}).
spell_auto_data('meld into stone', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You step into a stone object or surface large enough to fully contain your body, melding yourself and all the equipment you carry with the stone for the duration. Using your movement, you step into the stone at a point you can touch. Nothing of your presence remains visible or otherwise detectable by nonmagical senses.", "While merged with the stone, you can't see what occurs outside it, and any Wisdom (Perception) checks you make to hear sounds outside it are made with disadvantage. You remain aware of the passage of time and can cast spells on yourself while merged in the stone. You can use your movement to leave the stone where you entered it, which ends the spell. You otherwise can't move.", "Minor physical damage to the stone doesn't harm you, but its partial destruction or a change in its shape (to the extent that you no longer fit within it) expels you and deals 6 d 6 bludgeoning damage to you. The stone's complete destruction (or transmutation into a different substance) expels you and deals 50 bludgeoning damage to you. If expelled, you fall prone in an unoccupied space closest to where you first entered."], duration:"8 hours", higher_level:no, level:3, material:false, range:touch, ritual:yes, school:transmutation}).
spell_auto_data(mending, properties{area_of_effect:false, attack_type:false, casting_time:"1 minute", classes:[cleric, bard, druid, sorcerer, wizard], components:[v, s, m("Two lodestones.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["This spell repairs a single break or tear in an object you touch, such as a broken key, a torn cloak, or a leaking wineskin. As long as the break or tear is no longer than 1 foot in any dimension, you mend it, leaving no trace of the former damage.", "This spell can physically repair a magic item or construct, but the spell can't restore magic to such an object."], duration:"Instantaneous", higher_level:no, level:0, material:"Two lodestones.", range:touch, ritual:no, school:transmutation}).
spell_auto_data(message, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, wizard], components:[v, s, m("A short piece of copper wire.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You point your finger toward a creature within range and whisper a message. The target (and only the target) hears the message and can reply in a whisper that only you can hear.", "You can cast this spell through solid objects if you are familiar with the target and know it is beyond the barrier. Magical silence, 1 foot of stone, 1 inch of common metal, a thin sheet of lead, or 3 feet of wood blocks the spell. The spell doesn't have to follow a straight line and can travel freely around corners or through openings."], duration:"1 round", higher_level:no, level:0, material:"A short piece of copper wire.", range:feet(120), ritual:no, school:transmutation}).
spell_auto_data('meteor swarm', properties{area_of_effect:40 ft sphere, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s], concentration:no, damage_at_slot_level:_{9:damage(fire, 20 d 6+20 d 6)}, damage_with_cantrip_scaling:false, dc:dex else half, desc:["Blazing orbs of fire plummet to the ground at four different points you can see within range. Each creature in a 40-foot-radius sphere centered on each point you choose must make a dexterity saving throw. The sphere spreads around corners. A creature takes 20 d 6 fire damage and 20 d 6 bludgeoning damage on a failed save, or half as much damage on a successful one. A creature in the area of more than one fiery burst is affected only once.", "The spell damages objects in the area and ignites flammable objects that aren't being worn or carried."], duration:"Instantaneous", higher_level:no, level:9, material:false, range:miles(1), ritual:no, school:evocation}).
spell_auto_data('mind blank', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, wizard], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Until the spell ends, one willing creature you touch is immune to psychic damage, any effect that would sense its emotions or read its thoughts, divination spells, and the charmed condition. The spell even foils wish spells and spells or effects of similar power used to affect the target's mind or to gain information about the target."], duration:"24 hours", higher_level:no, level:8, material:false, range:touch, ritual:no, school:abjuration}).
spell_auto_data('minor illusion', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, warlock, wizard], components:[s, m("A bit of fleece.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You create a sound or an image of an object within range that lasts for the duration. The illusion also ends if you dismiss it as an action or cast this spell again.", "If you create a sound, its volume can range from a whisper to a scream. It can be your voice, someone else's voice, a lion's roar, a beating of drums, or any other sound you choose. The sound continues unabated throughout the duration, or you can make discrete sounds at different times before the spell ends.", "If you create an image of an object--such as a chair, muddy footprints, or a small chest--it must be no larger than a 5-foot cube. The image can't create sound, light, smell, or any other sensory effect. Physical interaction with the image reveals it to be an illusion, because things can pass through it.", "If a creature uses its action to examine the sound or image, the creature can determine that it is an illusion with a successful Intelligence (Investigation) check against your spell save DC. If a creature discerns the illusion for what it is, the illusion becomes faint to the creature."], duration:"1 minute", higher_level:no, level:0, material:"A bit of fleece.", range:feet(30), ritual:no, school:illusion}).
spell_auto_data('mirage arcane', properties{area_of_effect:5280 ft cube, attack_type:false, casting_time:"10 minutes", classes:[bard, druid, wizard], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You make terrain in an area up to 1 mile square look, sound, smell, and even feel like some other sort of terrain. The terrain's general shape remains the same, however. Open fields or a road could be made to resemble a swamp, hill, crevasse, or some other difficult or impassable terrain. A pond can be made to seem like a grassy meadow, a precipice like a gentle slope, or a rock-strewn gully like a wide and smooth road.", "Similarly, you can alter the appearance of structures, or add them where none are present. The spell doesn't disguise, conceal, or add creatures.", "The illusion includes audible, visual, tactile, and olfactory elements, so it can turn clear ground into difficult terrain (or vice versa) or otherwise impede movement through the area. Any piece of the illusory terrain (such as a rock or stick) that is removed from the spell's area disappears immediately.", "Creatures with truesight can see through the illusion to the terrain's true form; however, all other elements of the illusion remain, so while the creature is aware of the illusion's presence, the creature can still physically interact with the illusion."], duration:"10 days", higher_level:no, level:7, material:false, range:sight, ritual:no, school:illusion}).
spell_auto_data('mirror image', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[sorcerer, warlock, wizard], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Three illusory duplicates of yourself appear in your space. Until the spell ends, the duplicates move with you and mimic your actions, shifting position so it's impossible to track which image is real. You can use your action to dismiss the illusory duplicates.", "Each time a creature targets you with an attack during the spell's duration, roll a d20 to determine whether the attack instead targets one of your duplicates.", "If you have three duplicates, you must roll a 6 or higher to change the attack's target to a duplicate. With two duplicates, you must roll an 8 or higher. With one duplicate, you must roll an 11 or higher.", "A duplicate's AC equals 10 + your Dexterity modifier. If an attack hits a duplicate, the duplicate is destroyed. A duplicate can be destroyed only by an attack that hits it. It ignores all other damage and effects. The spell ends when all three duplicates are destroyed.", "A creature is unaffected by this spell if it can't see, if it relies on senses other than sight, such as blindsight, or if it can perceive illusions as false, as with truesight."], duration:"1 minute", higher_level:no, level:2, material:false, range:self, ritual:no, school:illusion}).
spell_auto_data(mislead, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, wizard], components:[s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You become invisible at the same time that an illusory double of you appears where you are standing. The double lasts for the duration, but the invisibility ends if you attack or cast a spell.", "You can use your action to move your illusory double up to twice your speed and make it gesture, speak, and behave in whatever way you choose.", "You can see through its eyes and hear through its ears as if you were located where it is. On each of your turns as a bonus action, you can switch from using its senses to using your own, or back again. While you are using its senses, you are blinded and deafened in regard to your own surroundings."], duration:"Up to 1 hour", higher_level:no, level:5, material:false, range:self, ritual:no, school:illusion}).
spell_auto_data('misty step', properties{area_of_effect:false, attack_type:false, casting_time:"1 bonus action", classes:[sorcerer, warlock, wizard], components:[v], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Briefly surrounded by silvery mist, you teleport up to 30 feet to an unoccupied space that you can see."], duration:"Instantaneous", higher_level:no, level:2, material:false, range:self, ritual:no, school:conjuration}).
spell_auto_data('modify memory', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, wizard], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else none, desc:["You attempt to reshape another creature's memories. One creature that you can see must make a wisdom saving throw. If you are fighting the creature, it has advantage on the saving throw. On a failed save, the target becomes charmed by you for the duration. The charmed target is incapacitated and unaware of its surroundings, though it can still hear you. If it takes any damage or is targeted by another spell, this spell ends, and none of the target's memories are modified.", "While this charm lasts, you can affect the target's memory of an event that it experienced within the last 24 hours and that lasted no more than 10 minutes. You can permanently eliminate all memory of the event, allow the target to recall the event with perfect clarity and exacting detail, change its memory of the details of the event, or create a memory of some other event.", "You must speak to the target to describe how its memories are affected, and it must be able to understand your language for the modified memories to take root. Its mind fills in any gaps in the details of your description. If the spell ends before you have finished describing the modified memories, the creature's memory isn't altered. Otherwise, the modified memories take hold when the spell ends.", "A modified memory doesn't necessarily affect how a creature behaves, particularly if the memory contradicts the creature's natural inclinations, alignment, or beliefs. An illogical modified memory, such as implanting a memory of how much the creature enjoyed dousing itself in acid, is dismissed, perhaps as a bad dream. The DM might deem a modified memory too nonsensical to affect a creature in a significant manner.", "A remove curse or greater restoration spell cast on the target restores the creature's true memory."], duration:"Up to 1 minute", higher_level:"If you cast this spell using a spell slot of 6th level or higher, you can alter the target's memories of an event that took place up to 7 days ago (6th level), 30 days ago (7th level), 1 year ago (8th level), or any time in the creature's past (9th level).", level:5, material:false, range:feet(30), ritual:no, school:enchantment}).
spell_auto_data(moonbeam, properties{area_of_effect:5 ft cylinder, attack_type:false, casting_time:"1 action", classes:[druid], components:[v, s, m("Several seeds of any moonseed plant and a piece of opalescent feldspar.")], concentration:yes, damage_at_slot_level:_{2:damage(radiant, 2 d 10), 3:damage(radiant, 3 d 10), 4:damage(radiant, 4 d 10), 5:damage(radiant, 5 d 10), 6:damage(radiant, 6 d 10), 7:damage(radiant, 7 d 10), 8:damage(radiant, 8 d 10), 9:damage(radiant, 9 d 10)}, damage_with_cantrip_scaling:false, dc:con else half, desc:["A silvery beam of pale light shines down in a 5-foot radius, 40-foot-high cylinder centered on a point within range. Until the spell ends, dim light fills the cylinder.", "When a creature enters the spell's area for the first time on a turn or starts its turn there, it is engulfed in ghostly flames that cause searing pain, and it must make a constitution saving throw. It takes 2 d 10 radiant damage on a failed save, or half as much damage on a successful one.", "A shapechanger makes its saving throw with disadvantage. If it fails, it also instantly reverts to its original form and can't assume a different form until it leaves the spell's light.", "On each of your turns after you cast this spell, you can use an action to move the beam 60 feet in any direction."], duration:"Up to 1 minute", higher_level:"When you cast this spell using a spell slot of 3rd level or higher, the damage increases by 1dl0 for each slot level above 2nd.", level:2, material:"Several seeds of any moonseed plant and a piece of opalescent feldspar.", range:feet(120), ritual:no, school:evocation}).
spell_auto_data('move earth', properties{area_of_effect:40 ft cone, attack_type:false, casting_time:"1 action", classes:[druid, sorcerer, wizard], components:[v, s, m("An iron blade and a small bag containing a mixture of soils--clay, loam, and sand.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Choose an area of terrain no larger than 40 feet on a side within range. You can reshape dirt, sand, or clay in the area in any manner you choose for the duration. You can raise or lower the area's elevation, create or fill in a trench, erect or flatten a wall, or form a pillar. The extent of any such changes can't exceed half the area's largest dimension. So, if you affect a 40-foot square, you can create a pillar up to 20 feet high, raise or lower the square's elevation by up to 20 feet, dig a trench up to 20 feet deep, and so on. It takes 10 minutes for these changes to complete.", "At the end of every 10 minutes you spend concentrating on the spell, you can choose a new area of terrain to affect.", "Because the terrain's transformation occurs slowly, creatures in the area can't usually be trapped or injured by the ground's movement.", "This spell can't manipulate natural stone or stone construction. Rocks and structures shift to accommodate the new terrain. If the way you shape the terrain would make a structure unstable, it might collapse.", "Similarly, this spell doesn't directly affect plant growth. The moved earth carries any plants along with it."], duration:"Up to 2 hours", higher_level:no, level:6, material:"An iron blade and a small bag containing a mixture of soils--clay, loam, and sand.", range:feet(120), ritual:no, school:transmutation}).
spell_auto_data(nondetection, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, ranger, wizard], components:[v, s, m("A pinch of diamond dust worth 25 gp sprinkled over the target, which the spell consumes.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["For the duration, you hide a target that you touch from divination magic. The target can be a willing creature or a place or an object no larger than 10 feet in any dimension. The target can't be targeted by any divination magic or perceived through magical scrying sensors."], duration:"8 hours", higher_level:no, level:3, material:"A pinch of diamond dust worth 25 gp sprinkled over the target, which the spell consumes.", range:touch, ritual:no, school:abjuration}).
spell_auto_data('pass without trace', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[druid, ranger], components:[v, s, m("Ashes from a burned leaf of mistletoe and a sprig of spruce.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["A veil of shadows and silence radiates from you, masking you and your companions from detection. For the duration, each creature you choose within 30 feet of you (including you) has a +10 bonus to Dexterity (Stealth) checks and can't be tracked except by magical means. A creature that receives this bonus leaves behind no tracks or other traces of its passage."], duration:"Up to 1 hour", higher_level:no, level:2, material:"Ashes from a burned leaf of mistletoe and a sprig of spruce.", range:self, ritual:no, school:abjuration}).
spell_auto_data(passwall, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[wizard], components:[v, s, m("A pinch of sesame seeds.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["A passage appears at a point of your choice that you can see on a wooden, plaster, or stone surface (such as a wall, a ceiling, or a floor) within range, and lasts for the duration. You choose the opening's dimensions: up to 5 feet wide, 8 feet tall, and 20 feet deep. The passage creates no instability in a structure surrounding it.", "When the opening disappears, any creatures or objects still in the passage created by the spell are safely ejected to an unoccupied space nearest to the surface on which you cast the spell."], duration:"1 hour", higher_level:no, level:5, material:"A pinch of sesame seeds.", range:feet(30), ritual:no, school:transmutation}).
spell_auto_data('phantasmal killer', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[wizard], components:[v, s], concentration:yes, damage_at_slot_level:_{4:damage(psychic, 4 d 10)}, damage_with_cantrip_scaling:false, dc:wis else none, desc:["You tap into the nightmares of a creature you can see within range and create an illusory manifestation of its deepest fears, visible only to that creature. The target must make a wisdom saving throw. On a failed save, the target becomes frightened for the duration. At the start of each of the target's turns before the spell ends, the target must succeed on a wisdom saving throw or take 4 d 10 psychic damage. On a successful save, the spell ends."], duration:"Up to 1 minute", higher_level:"When you cast this spell using a spell slot of 5th level or higher, the damage increases by 1 d 10 for each slot level above 4th.", level:4, material:false, range:feet(120), ritual:no, school:illusion}).
spell_auto_data('phantom steed', properties{area_of_effect:false, attack_type:false, casting_time:"1 minute", classes:[wizard], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["A Large quasi-real, horselike creature appears on the ground in an unoccupied space of your choice within range. You decide the creature's appearance, but it is equipped with a saddle, bit, and bridle. Any of the equipment created by the spell vanishes in a puff of smoke if it is carried more than 10 feet away from the steed.", "For the duration, you or a creature you choose can ride the steed. The creature uses the statistics for a riding horse, except it has a speed of 100 feet and can travel 10 miles in an hour, or 13 miles at a fast pace. When the spell ends, the steed gradually fades, giving the rider 1 minute to dismount. The spell ends if you use an action to dismiss it or if the steed takes any damage."], duration:"1 hour", higher_level:no, level:3, material:false, range:feet(30), ritual:yes, school:illusion}).
spell_auto_data('planar ally', properties{area_of_effect:false, attack_type:false, casting_time:"10 minutes", classes:[cleric], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You beseech an otherworldly entity for aid. The being must be known to you: a god, a primordial, a demon prince, or some other being of cosmic power. That entity sends a celestial, an elemental, or a fiend loyal to it to aid you, making the creature appear in an unoccupied space within range. If you know a specific creature's name, you can speak that name when you cast this spell to request that creature, though you might get a different creature anyway (DM's choice).", "When the creature appears, it is under no compulsion to behave in any particular way. You can ask the creature to perform a service in exchange for payment, but it isn't obliged to do so. The requested task could range from simple (fly us across the chasm, or help us fight a battle) to complex (spy on our enemies, or protect us during our foray into the dungeon). You must be able to communicate with the creature to bargain for its services.", "Payment can take a variety of forms. A celestial might require a sizable donation of gold or magic items to an allied temple, while a fiend might demand a living sacrifice or a gift of treasure. Some creatures might exchange their service for a quest undertaken by you.", "As a rule of thumb, a task that can be measured in minutes requires a payment worth 100 gp per minute. A task measured in hours requires 1,000 gp per hour. And a task measured in days (up to 10 days) requires 10,000 gp per day. The DM can adjust these payments based on the circumstances under which you cast the spell. If the task is aligned with the creature's ethos, the payment might be halved or even waived. Nonhazardous tasks typically require only half the suggested payment, while especially dangerous tasks might require a greater gift. Creatures rarely accept tasks that seem suicidal.", "After the creature completes the task, or when the agreed-upon duration of service expires, the creature returns to its home plane after reporting back to you, if appropriate to the task and if possible. If you are unable to agree on a price for the creature's service, the creature immediately returns to its home plane.", "A creature enlisted to join your group counts as a member of it, receiving a full share of experience points awarded."], duration:"Instantaneous", higher_level:no, level:6, material:false, range:feet(60), ritual:no, school:conjuration}).
spell_auto_data('planar binding', properties{area_of_effect:false, attack_type:false, casting_time:"1 hour", classes:[bard, cleric, druid, wizard], components:[v, s, m("A jewel worth at least 1,000 gp, which the spell consumes.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:cha else none, desc:["With this spell, you attempt to bind a celestial, an elemental, a fey, or a fiend to your service. The creature must be within range for the entire casting of the spell. (Typically, the creature is first summoned into the center of an inverted magic circle in order to keep it trapped while this spell is cast.) At the completion of the casting, the target must make a charisma saving throw. On a failed save, it is bound to serve you for the duration. If the creature was summoned or created by another spell, that spell's duration is extended to match the duration of this spell.", "A bound creature must follow your instructions to the best of its ability. You might command the creature to accompany you on an adventure, to guard a location, or to deliver a message. The creature obeys the letter of your instructions, but if the creature is hostile to you, it strives to twist your words to achieve its own objectives. If the creature carries out your instructions completely before the spell ends, it travels to you to report this fact if you are on the same plane of existence. If you are on a different plane of existence, it returns to the place where you bound it and remains there until the spell ends."], duration:"24 hours", higher_level:"When you cast this spell using a spell slot of a higher level, the duration increases to 10 days with a 6th-level slot, to 30 days with a 7th-level slot, to 180 days with an 8th-level slot, and to a year and a day with a 9th-level spell slot.", level:5, material:"A jewel worth at least 1,000 gp, which the spell consumes.", range:feet(60), ritual:no, school:abjuration}).
spell_auto_data('plane shift', properties{area_of_effect:false, attack_type:melee, casting_time:"1 action", classes:[cleric, druid, sorcerer, warlock, wizard], components:[v, s, m("A forked, metal rod worth at least 250 gp, attuned to a particular plane of existence.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:cha else none, desc:["You and up to eight willing creatures who link hands in a circle are transported to a different plane of existence. You can specify a target destination in general terms, such as the City of Brass on the Elemental Plane of Fire or the palace of Dispater on the second level of the Nine Hells, and you appear in or near that destination. If you are trying to reach the City of Brass, for example, you might arrive in its Street of Steel, before its Gate of Ashes, or looking at the city from across the Sea of Fire, at the DM's discretion.", "Alternatively, if you know the sigil sequence of a teleportation circle on another plane of existence, this spell can take you to that circle. If the teleportation circle is too small to hold all the creatures you transported, they appear in the closest unoccupied spaces next to the circle.", "You can use this spell to banish an unwilling creature to another plane. Choose a creature within your reach and make a melee spell attack against it. On a hit, the creature must make a charisma saving throw. If the creature fails this save, it is transported to a random location on the plane of existence you specify. A creature so transported must find its own way back to your current plane of existence."], duration:"Instantaneous", higher_level:no, level:7, material:"A forked, metal rod worth at least 250 gp, attuned to a particular plane of existence.", range:touch, ritual:no, school:conjuration}).
spell_auto_data('plant growth', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, druid, ranger], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["This spell channels vitality into plants within a specific area. There are two possible uses for the spell, granting either immediate or long-term benefits.", "If you cast this spell using 1 action, choose a point within range. All normal plants in a 100-foot radius centered on that point become thick and overgrown. A creature moving through the area must spend 4 feet of movement for every 1 foot it moves.", "You can exclude one or more areas of any size within the spell's area from being affected.", "If you cast this spell over 8 hours, you enrich the land. All plants in a half-mile radius centered on a point within range become enriched for 1 year. The plants yield twice the normal amount of food when harvested."], duration:"Instantaneous", higher_level:no, level:3, material:false, range:feet(150), ritual:no, school:transmutation}).
spell_auto_data('poison spray', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[sorcerer, warlock, wizard], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:damage(poison, 1 d 12), dc:con else none, desc:["You extend your hand toward a creature you can see within range and project a puff of noxious gas from your palm. The creature must succeed on a constitution saving throw or take 1 d 12 poison damage.", "This spell's damage increases by 1 d 12 when you reach 5th level (2 d 12), 11th level (3 d 12), and 17th level (4 d 12)."], duration:"Instantaneous", higher_level:no, level:0, material:false, range:feet(10), ritual:no, school:conjuration}).
spell_auto_data(polymorph, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, druid, sorcerer, wizard], components:[v, s, m("A caterpillar cocoon.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else none, desc:["This spell transforms a creature that you can see within range into a new form. An unwilling creature must make a wisdom saving throw to avoid the effect. A shapechanger automatically succeeds on this saving throw.", "The transformation lasts for the duration, or until the target drops to 0 hit points or dies. The new form can be any beast whose challenge rating is equal to or less than the target's (or the target's level, if it doesn't have a challenge rating). The target's game statistics, including mental ability scores, are replaced by the statistics of the chosen beast. It retains its alignment and personality.", "The target assumes the hit points of its new form. When it reverts to its normal form, the creature returns to the number of hit points it had before it transformed. If it reverts as a result of dropping to 0 hit points, any excess damage carries over to its normal form. As long as the excess damage doesn't reduce the creature's normal form to 0 hit points, it isn't knocked unconscious.", "The creature is limited in the actions it can perform by the nature of its new form, and it can't speak, cast spells, or take any other action that requires hands or speech.", "The target's gear melds into the new form. The creature can't activate, use, wield, or otherwise benefit from any of its equipment."], duration:"Up to 1 hour", higher_level:no, level:4, material:"A caterpillar cocoon.", range:feet(60), ritual:no, school:transmutation}).
spell_auto_data('power word kill', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, warlock, wizard], components:[v], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You utter a word of power that can compel one creature you can see within range to die instantly. If the creature you choose has 100 hit points or fewer, it dies. Otherwise, the spell has no effect."], duration:"Instantaneous", higher_level:no, level:9, material:false, range:feet(60), ritual:no, school:enchantment}).
spell_auto_data('power word stun', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, warlock, wizard], components:[v], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You speak a word of power that can overwhelm the mind of one creature you can see within range, leaving it dumbfounded. If the target has 150 hit points or fewer, it is stunned. Otherwise, the spell has no effect.", "The stunned target must make a constitution saving throw at the end of each of its turns. On a successful save, this stunning effect ends."], duration:"Instantaneous", higher_level:no, level:8, material:false, range:feet(60), ritual:no, school:enchantment}).
spell_auto_data('prayer of healing', properties{area_of_effect:false, attack_type:false, casting_time:"10 minutes", classes:[cleric], components:[v], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Up to six creatures of your choice that you can see within range each regain hit points equal to 2 d 8 + your spellcasting ability modifier. This spell has no effect on undead or constructs."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 3rd level or higher, the healing increases by 1 d 8 for each slot level above 2nd.", level:2, material:false, range:feet(30), ritual:no, school:evocation}).
spell_auto_data(prestidigitation, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, warlock, wizard], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["This spell is a minor magical trick that novice spellcasters use for practice. You create one of the following magical effects within 'range':", "You create an instantaneous, harmless sensory effect, such as a shower of sparks, a puff of wind, faint musical notes, or an odd odor.", "You instantaneously light or snuff out a candle, a torch, or a small campfire.", "You instantaneously clean or soil an object no larger than 1 cubic foot.", "You chill, warm, or flavor up to 1 cubic foot of nonliving material for 1 hour.", "You make a color, a small mark, or a symbol appear on an object or a surface for 1 hour.", "You create a nonmagical trinket or an illusory image that can fit in your hand and that lasts until the end of your next turn.", "If you cast this spell multiple times, you can have up to three of its non-instantaneous effects active at a time, and you can dismiss such an effect as an action."], duration:"1 hour", higher_level:no, level:0, material:false, range:feet(10), ritual:no, school:transmutation}).
spell_auto_data('prismatic spray', properties{area_of_effect:60 ft cone, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:dex else other, desc:["Eight multicolored rays of light flash from your hand. Each ray is a different color and has a different power and purpose. Each creature in a 60-foot cone must make a dexterity saving throw. For each target, roll a d8 to determine which color ray affects it.", "**1. Red.** The target takes 10 d 6 fire damage on a failed save, or half as much damage on a successful one.", "**2. Orange.** The target takes 10 d 6 acid damage on a failed save, or half as much damage on a successful one.", "**3. Yellow.** The target takes 10 d 6 lightning damage on a failed save, or half as much damage on a successful one.", "**4. Green.** The target takes 10 d 6 poison damage on a failed save, or half as much damage on a successful one.", "**5. Blue.** The target takes 10 d 6 cold damage on a failed save, or half as much damage on a successful one.", "**6. Indigo.** On a failed save, the target is restrained. It must then make a constitution saving throw at the end of each of its turns. If it successfully saves three times, the spell ends. If it fails its save three times, it permanently turns to stone and is subjected to the petrified condition. The successes and failures don't need to be consecutive; keep track of both until the target collects three of a kind.", "**7. Violet.** On a failed save, the target is blinded. It must then make a wisdom saving throw at the start of your next turn. A successful save ends the blindness. If it fails that save, the creature is transported to another plane of existence of the DM's choosing and is no longer blinded. (Typically, a creature that is on a plane that isn't its home plane is banished home, while other creatures are usually cast into the Astral or Ethereal planes.)", "**8. Special.** The target is struck by two rays. Roll twice more, rerolling any 8."], duration:"Instantaneous", higher_level:no, level:7, material:false, range:self, ritual:no, school:evocation}).
spell_auto_data('prismatic wall', properties{area_of_effect:90 ft line, attack_type:false, casting_time:"1 action", classes:[wizard], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["A shimmering, multicolored plane of light forms a vertical opaque wall--up to 90 feet long, 30 feet high, and 1 inch thick--centered on a point you can see within range. Alternatively, you can shape the wall into a sphere up to 30 feet in diameter centered on a point you choose within range. The wall remains in place for the duration. If you position the wall so that it passes through a space occupied by a creature, the spell fails, and your action and the spell slot are wasted.", "The wall sheds bright light out to a range of 100 feet and dim light for an additional 100 feet. You and creatures you designate at the time you cast the spell can pass through and remain near the wall without harm. If another creature that can see the wall moves to within 20 feet of it or starts its turn there, the creature must succeed on a constitution saving throw or become blinded for 1 minute.", "The wall consists of seven layers, each with a different color. When a creature attempts to reach into or pass through the wall, it does so one layer at a time through all the wall's layers. As it passes or reaches through each layer, the creature must make a dexterity saving throw or be affected by that layer's properties as described below.", "The wall can be destroyed, also one layer at a time, in order from red to violet, by means specific to each layer. Once a layer is destroyed, it remains so for the duration of the spell. A rod of cancellation destroys a prismatic wall, but an antimagic field has no effect on it.", "**1. Red.** The creature takes 10 d 6 fire damage on a failed save, or half as much damage on a successful one. While this layer is in place, nonmagical ranged attacks can't pass through the wall. The layer can be destroyed by dealing at least 25 cold damage to it.", "**2. Orange.** The creature takes 10 d 6 acid damage on a failed save, or half as much damage on a successful one. While this layer is in place, magical ranged attacks can't pass through the wall. The layer is destroyed by a strong wind.", "**3. Yellow.** The creature takes 10 d 6 lightning damage on a failed save, or half as much damage on a successful one. This layer can be destroyed by dealing at least 60 force damage to it.", "**4. Green.** The creature takes 10 d 6 poison damage on a failed save, or half as much damage on a successful one. A passwall spell, or another spell of equal or greater level that can open a portal on a solid surface, destroys this layer.", "**5. Blue.** The creature takes 10 d 6 cold damage on a failed save, or half as much damage on a successful one. This layer can be destroyed by dealing at least 25 fire damage to it.", "**6. Indigo.** On a failed save, the creature is restrained. It must then make a constitution saving throw at the end of each of its turns. If it successfully saves three times, the spell ends. If it fails its save three times, it permanently turns to stone and is subjected to the petrified condition. The successes and failures don't need to be consecutive; keep track of both until the creature collects three of a kind.", "While this layer is in place, spells can't be cast through the wall. The layer is destroyed by bright light shed by a daylight spell or a similar spell of equal or higher level.", "**7. Violet.** On a failed save, the creature is blinded. It must then make a wisdom saving throw at the start of your next turn. A successful save ends the blindness. If it fails that save, the creature is transported to another plane of the DM's choosing and is no longer blinded. (Typically, a creature that is on a plane that isn't its home plane is banished home, while other creatures are usually cast into the Astral or Ethereal planes.) This layer is destroyed by a dispel magic spell or a similar spell of equal or higher level that can end spells and magical effects."], duration:"10 minutes", higher_level:no, level:9, material:false, range:feet(60), ritual:no, school:abjuration}).
spell_auto_data('private sanctum', properties{area_of_effect:100 ft cube, attack_type:false, casting_time:"10 minutes", classes:[wizard], components:[v, s, m("A thin sheet of lead, a piece of opaque glass, a wad of cotton or cloth, and powdered chrysolite.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You make an area within range magically secure. The area is a cube that can be as small as 5 feet to as large as 100 feet on each side. The spell lasts for the duration or until you use an action to dismiss it.", "When you cast the spell, you decide what sort of security the spell provides, choosing any or all of the following properties:", "- Sound can't pass through the barrier at the edge of the warded area.", "- The barrier of the warded area appears dark and foggy, preventing vision (including darkvision) through it.", "- Sensors created by divination spells can't appear inside the protected area or pass through the barrier at its perimeter.", "- Creatures in the area can't be targeted by divination spells.", "- Nothing can teleport into or out of the warded area.", "- Planar travel is blocked within the warded area.", "Casting this spell on the same spot every day for a year makes this effect permanent."], duration:"24 hours", higher_level:"When you cast this spell using a spell slot of 5th level or higher, you can increase the size of the cube by 100 feet for each slot level beyond 4th. Thus you could protect a cube that can be up to 200 feet on one side by using a spell slot of 5th level.", level:4, material:"A thin sheet of lead, a piece of opaque glass, a wad of cotton or cloth, and powdered chrysolite.", range:feet(120), ritual:no, school:abjuration}).
spell_auto_data('produce flame', properties{area_of_effect:false, attack_type:ranged, casting_time:"1 action", classes:[druid], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:damage(fire, 1 d 8), dc:false, desc:["A flickering flame appears in your hand. The flame remains there for the duration and harms neither you nor your equipment. The flame sheds bright light in a 10-foot radius and dim light for an additional 10 feet. The spell ends if you dismiss it as an action or if you cast it again.", "You can also attack with the flame, although doing so ends the spell. When you cast this spell, or as an action on a later turn, you can hurl the flame at a creature within 30 feet of you. Make a ranged spell attack. On a hit, the target takes 1 d 8 fire damage.", "This spell's damage increases by 1 d 8 when you reach 5th level (2 d 8), 11th level (3 d 8), and 17th level (4 d 8)."], duration:"10 minutes", higher_level:no, level:0, material:false, range:self, ritual:no, school:conjuration}).
spell_auto_data('programmed illusion', properties{area_of_effect:30 ft cube, attack_type:false, casting_time:"1 action", classes:[bard, wizard], components:[v, s, m("A bit of fleece and jade dust worth at least 25 gp.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You create an illusion of an object, a creature, or some other visible phenomenon within range that activates when a specific condition occurs. The illusion is imperceptible until then. It must be no larger than a 30-foot cube, and you decide when you cast the spell how the illusion behaves and what sounds it makes. This scripted performance can last up to 5 minutes.", "When the condition you specify occurs, the illusion springs into existence and performs in the manner you described. Once the illusion finishes performing, it disappears and remains dormant for 10 minutes. After this time, the illusion can be activated again.", "The triggering condition can be as general or as detailed as you like, though it must be based on visual or audible conditions that occur within 30 feet of the area. For example, you could create an illusion of yourself to appear and warn off others who attempt to open a trapped door, or you could set the illusion to trigger only when a creature says the correct word or phrase.", "Physical interaction with the image reveals it to be an illusion, because things can pass through it. A creature that uses its action to examine the image can determine that it is an illusion with a successful Intelligence (Investigation) check against your spell save DC. If a creature discerns the illusion for what it is, the creature can see through the image, and any noise it makes sounds hollow to the creature."], duration:"Until dispelled", higher_level:no, level:6, material:"A bit of fleece and jade dust worth at least 25 gp.", range:feet(120), ritual:no, school:illusion}).
spell_auto_data('project image', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, wizard], components:[v, s, m("A small replica of you made from materials worth at least 5 gp.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You create an illusory copy of yourself that lasts for the duration. The copy can appear at any location within range that you have seen before, regardless of intervening obstacles. The illusion looks and sounds like you but is intangible. If the illusion takes any damage, it disappears, and the spell ends.", "You can use your action to move this illusion up to twice your speed, and make it gesture, speak, and behave in whatever way you choose. It mimics your mannerisms perfectly.", "You can see through its eyes and hear through its ears as if you were in its space. On your turn as a bonus action, you can switch from using its senses to using your own, or back again. While you are using its senses, you are blinded and deafened in regard to your own surroundings.", "Physical interaction with the image reveals it to be an illusion, because things can pass through it. A creature that uses its action to examine the image can determine that it is an illusion with a successful Intelligence (Investigation) check against your spell save DC. If a creature discerns the illusion for what it is, the creature can see through the image, and any noise it makes sounds hollow to the creature."], duration:"Up to 24 hours", higher_level:no, level:7, material:"A small replica of you made from materials worth at least 5 gp.", range:miles(500), ritual:no, school:illusion}).
spell_auto_data('protection from energy', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric, druid, ranger, sorcerer, wizard], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["For the duration, the willing creature you touch has resistance to one damage type of your choice: acid, cold, fire, lightning, or thunder."], duration:"Up to 1 hour", higher_level:no, level:3, material:false, range:touch, ritual:no, school:abjuration}).
spell_auto_data('protection from evil and good', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric, paladin, warlock, wizard], components:[v, s, m("Holy water or powdered silver and iron, which the spell consumes.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Until the spell ends, one willing creature you touch is protected against certain types of creatures: aberrations, celestials, elementals, fey, fiends, and undead.", "The protection grants several benefits. Creatures of those types have disadvantage on attack rolls against the target. The target also can't be charmed, frightened, or possessed by them. If the target is already charmed, frightened, or possessed by such a creature, the target has advantage on any new saving throw against the relevant effect."], duration:"Up to 10 minutes", higher_level:no, level:1, material:"Holy water or powdered silver and iron, which the spell consumes.", range:touch, ritual:no, school:abjuration}).
spell_auto_data('protection from poison', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric, druid, paladin, ranger], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch a creature. If it is poisoned, you neutralize the poison. If more than one poison afflicts the target, you neutralize one poison that you know is present, or you neutralize one at random.", "For the duration, the target has advantage on saving throws against being poisoned, and it has resistance to poison damage."], duration:"1 hour", higher_level:no, level:2, material:false, range:touch, ritual:no, school:abjuration}).
spell_auto_data('purify food and drink', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric, druid, paladin], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["All nonmagical food and drink within a 5-foot radius sphere centered on a point of your choice within range is purified and rendered free of poison and disease."], duration:"Instantaneous", higher_level:no, level:1, material:false, range:feet(10), ritual:yes, school:transmutation}).
spell_auto_data('raise dead', properties{area_of_effect:false, attack_type:false, casting_time:"1 hour", classes:[bard, cleric, paladin], components:[v, s, m("A diamond worth at least 500gp, which the spell consumes.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You return a dead creature you touch to life, provided that it has been dead no longer than 10 days. If the creature's soul is both willing and at liberty to rejoin the body, the creature returns to life with 1 hit point.", "This spell also neutralizes any poisons and cures nonmagical diseases that affected the creature at the time it died. This spell doesn't, however, remove magical diseases, curses, or similar effects; if these aren't first removed prior to casting the spell, they take effect when the creature returns to life. The spell can't return an undead creature to life.", "This spell closes all mortal wounds, but it doesn't restore missing body parts. If the creature is lacking body parts or organs integral for its survival--its head, for instance--the spell automatically fails.", "Coming back from the dead is an ordeal. The target takes a -4 penalty to all attack rolls, saving throws, and ability checks. Every time the target finishes a long rest, the penalty is reduced by 1 until it disappears."], duration:"Instantaneous", higher_level:no, level:5, material:"A diamond worth at least 500gp, which the spell consumes.", range:touch, ritual:no, school:necromancy}).
spell_auto_data('ray of enfeeblement', properties{area_of_effect:false, attack_type:ranged, casting_time:"1 action", classes:[warlock, wizard], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:con else none, desc:["A black beam of enervating energy springs from your finger toward a creature within range. Make a ranged spell attack against the target. On a hit, the target deals only half damage with weapon attacks that use Strength until the spell ends.", "At the end of each of the target's turns, it can make a constitution saving throw against the spell. On a success, the spell ends."], duration:"Up to 1 minute", higher_level:no, level:2, material:false, range:feet(60), ritual:no, school:necromancy}).
spell_auto_data('ray of frost', properties{area_of_effect:false, attack_type:ranged, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:damage(cold, 1 d 8), dc:false, desc:["A frigid beam of blue-white light streaks toward a creature within range. Make a ranged spell attack against the target. On a hit, it takes 1 d 8 cold damage, and its speed is reduced by 10 feet until the start of your next turn.", "The spell's damage increases by 1 d 8 when you reach 5th level (2 d 8), 11th level (3 d 8), and 17th level (4 d 8)."], duration:"Instantaneous", higher_level:no, level:0, material:false, range:feet(60), ritual:no, school:evocation}).
spell_auto_data(regenerate, properties{area_of_effect:false, attack_type:false, casting_time:"1 minute", classes:[bard, cleric, druid], components:[v, s, m("A prayer wheel and holy water.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch a creature and stimulate its natural healing ability. The target regains 4 d 8 + 15 hit points. For the duration of the spell, the target regains 1 hit point at the start of each of its turns (10 hit points each minute).", "The target's severed body members (fingers, legs, tails, and so on), if any, are restored after 2 minutes. If you have the severed part and hold it to the stump, the spell instantaneously causes the limb to knit to the stump."], duration:"1 hour", higher_level:no, level:7, material:"A prayer wheel and holy water.", range:touch, ritual:no, school:transmutation}).
spell_auto_data(reincarnate, properties{area_of_effect:false, attack_type:false, casting_time:"1 hour", classes:[druid], components:[v, s, m("Rare oils and unguents worth at least 1,000 gp, which the spell consumes.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch a dead humanoid or a piece of a dead humanoid. Provided that the creature has been dead no longer than 10 days, the spell forms a new adult body for it and then calls the soul to enter that body. If the target's soul isn't free or willing to do so, the spell fails.", "The magic fashions a new body for the creature to inhabit, which likely causes the creature's race to change. The DM rolls a d 100 and consults the following table to determine what form the creature takes when restored to life, or the DM chooses a form.", "| d100 | Race |\n |---|---|\n | 01-04 | Dragonborn |\n | 05-13 | Dwarf, hill |\n | 14-21 | Dwarf, mountain |\n | 22-25 | Elf, dark |\n | 26-34 | Elf, high |\n | 35-42 | Elf, wood |\n | 43-46 | Gnome, forest |\n | 47-52 | Gnome, rock |\n | 53-56 | Half-elf |\n | 57-60 | Half-orc |\n | 61-68 | Halfling, lightfoot |\n | 69-76 | Halfling, stout |\n | 77-96 | Human |\n | 97-00 | Tiefling |", "The reincarnated creature recalls its former life and experiences. It retains the capabilities it had in its original form, except it exchanges its original race for the new one and changes its racial traits accordingly."], duration:"Instantaneous", higher_level:no, level:5, material:"Rare oils and unguents worth at least 1,000 gp, which the spell consumes.", range:touch, ritual:no, school:transmutation}).
spell_auto_data('remove curse', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric, paladin, warlock, wizard], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["At your touch, all curses affecting one creature or object end. If the object is a cursed magic item, its curse remains, but the spell breaks its owner's attunement to the object so it can be removed or discarded."], duration:"Instantaneous", higher_level:no, level:3, material:false, range:touch, ritual:no, school:abjuration}).
spell_auto_data('resilient sphere', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[wizard], components:[v, s, m("A hemispherical piece of clear crystal and a matching hemispherical piece of gum arabic.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:dex else none, desc:["A sphere of shimmering force encloses a creature or object of Large size or smaller within range. An unwilling creature must make a dexterity saving throw. On a failed save, the creature is enclosed for the duration.", "Nothing--not physical objects, energy, or other spell effects--can pass through the barrier, in or out, though a creature in the sphere can breathe there. The sphere is immune to all damage, and a creature or object inside can't be damaged by attacks or effects originating from outside, nor can a creature inside the sphere damage anything outside it.", "The sphere is weightless and just large enough to contain the creature or object inside. An enclosed creature can use its action to push against the sphere's walls and thus roll the sphere at up to half the creature's speed. Similarly, the globe can be picked up and moved by other creatures.", "A disintegrate spell targeting the globe destroys it without harming anything inside it."], duration:"Up to 1 minute", higher_level:no, level:4, material:"A hemispherical piece of clear crystal and a matching hemispherical piece of gum arabic.", range:feet(30), ritual:no, school:evocation}).
spell_auto_data(resistance, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric, druid], components:[v, s, m("A miniature cloak.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch one willing creature. Once before the spell ends, the target can roll a d4 and add the number rolled to one saving throw of its choice. It can roll the die before or after making the saving throw. The spell then ends."], duration:"Up to 1 minute", higher_level:no, level:0, material:"A miniature cloak.", range:touch, ritual:no, school:abjuration}).
spell_auto_data(resurrection, properties{area_of_effect:false, attack_type:false, casting_time:"1 hour", classes:[bard, cleric], components:[v, s, m("A diamond worth at least 1,000gp, which the spell consumes.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch a dead creature that has been dead for no more than a century, that didn't die of old age, and that isn't undead. If its soul is free and willing, the target returns to life with all its hit points.", "This spell neutralizes any poisons and cures normal diseases afflicting the creature when it died. It doesn't, however, remove magical diseases, curses, and the like; if such effects aren't removed prior to casting the spell, they afflict the target on its return to life.", "This spell closes all mortal wounds and restores any missing body parts.", "Coming back from the dead is an ordeal. The target takes a -4 penalty to all attack rolls, saving throws, and ability checks. Every time the target finishes a long rest, the penalty is reduced by 1 until it disappears.", "Casting this spell to restore life to a creature that has been dead for one year or longer taxes you greatly. Until you finish a long rest, you can't cast spells again, and you have disadvantage on all attack rolls, ability checks, and saving throws."], duration:"Instantaneous", higher_level:no, level:7, material:"A diamond worth at least 1,000gp, which the spell consumes.", range:touch, ritual:no, school:necromancy}).
spell_auto_data('reverse gravity', properties{area_of_effect:50 ft cylinder, attack_type:false, casting_time:"1 action", classes:[druid, sorcerer, wizard], components:[v, s, m("A lodestone and iron filings.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:dex else other, desc:["This spell reverses gravity in a 50-foot-radius, 100-foot high cylinder centered on a point within range. All creatures and objects that aren't somehow anchored to the ground in the area fall upward and reach the top of the area when you cast this spell. A creature can make a dexterity saving throw to grab onto a fixed object it can reach, thus avoiding the fall.", "If some solid object (such as a ceiling) is encountered in this fall, falling objects and creatures strike it just as they would during a normal downward fall. If an object or creature reaches the top of the area without striking anything, it remains there, oscillating slightly, for the duration.", "At the end of the duration, affected objects and creatures fall back down."], duration:"Up to 1 minute", higher_level:no, level:7, material:"A lodestone and iron filings.", range:feet(100), ritual:no, school:transmutation}).
spell_auto_data(revivify, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric, paladin], components:[v, s, m("Diamonds worth 300gp, which the spell consumes.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch a creature that has died within the last minute. That creature returns to life with 1 hit point. This spell can't return to life a creature that has died of old age, nor can it restore any missing body parts."], duration:"Instantaneous", higher_level:no, level:3, material:"Diamonds worth 300gp, which the spell consumes.", range:touch, ritual:no, school:conjuration}).
spell_auto_data('rope trick', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[wizard], components:[v, s, m("Powdered corn extract and a twisted loop of parchment.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch a length of rope that is up to 60 feet long. One end of the rope then rises into the air until the whole rope hangs perpendicular to the ground. At the upper end of the rope, an invisible entrance opens to an extradimensional space that lasts until the spell ends.", "The extradimensional space can be reached by climbing to the top of the rope. The space can hold as many as eight Medium or smaller creatures. The rope can be pulled into the space, making the rope disappear from view outside the space.", "Attacks and spells can't cross through the entrance into or out of the extradimensional space, but those inside can see out of it as if through a 3-foot-by-5-foot window centered on the rope.", "Anything inside the extradimensional space drops out when the spell ends."], duration:"1 hour", higher_level:no, level:2, material:"Powdered corn extract and a twisted loop of parchment.", range:touch, ritual:no, school:transmutation}).
spell_auto_data('sacred flame', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:damage(radiant, 1 d 8), dc:dex else none, desc:["Flame-like radiance descends on a creature that you can see within range. The target must succeed on a dexterity saving throw or take 1 d 8 radiant damage. The target gains no benefit from cover for this saving throw.", "The spell's damage increases by 1 d 8 when you reach 5th level (2 d 8), 11th level (3 d 8), and 17th level (4 d 8)."], duration:"Instantaneous", higher_level:no, level:0, material:false, range:feet(60), ritual:no, school:evocation}).
spell_auto_data(sanctuary, properties{area_of_effect:false, attack_type:false, casting_time:"1 bonus action", classes:[cleric], components:[v, s, m("A small silver mirror.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You ward a creature within range against attack. Until the spell ends, any creature who targets the warded creature with an attack or a harmful spell must first make a wisdom saving throw. On a failed save, the creature must choose a new target or lose the attack or spell. This spell doesn't protect the warded creature from area effects, such as the explosion of a fireball.", "If the warded creature makes an attack or casts a spell that affects an enemy creature, this spell ends."], duration:"1 minute", higher_level:no, level:1, material:"A small silver mirror.", range:feet(30), ritual:no, school:abjuration}).
spell_auto_data('scorching ray', properties{area_of_effect:false, attack_type:ranged, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s], concentration:no, damage_at_slot_level:_{2:damage(fire, 2 d 6)}, damage_with_cantrip_scaling:false, dc:false, desc:["You create three rays of fire and hurl them at targets within range. You can hurl them at one target or several.", "Make a ranged spell attack for each ray. On a hit, the target takes 2 d 6 fire damage."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 3rd level or higher, you create one additional ray for each slot level above 2nd.", level:2, material:false, range:feet(120), ritual:no, school:evocation}).
spell_auto_data(scrying, properties{area_of_effect:false, attack_type:false, casting_time:"10 minutes", classes:[bard, cleric, druid, warlock, wizard], components:[v, s, m("A focus worth at least 1,000 gp, such as a crystal ball, a silver mirror, or a font filled with holy water.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else none, desc:["You can see and hear a particular creature you choose that is on the same plane of existence as you. The target must make a wisdom saving throw, which is modified by how well you know the target and the sort of physical connection you have to it. If a target knows you're casting this spell, it can fail the saving throw voluntarily if it wants to be observed.", "| Knowledge | Save Modifier |\n |---|---|\n | Secondhand (you have heard of the target) | +5 |\n | Firsthand (you have met the target) | +0 |\n | Familiar (you know the target well) | -5 |", "| Connection | Save Modifier |\n |---|---|\n | Likeness or picture | -2 |\n | Possession or garment | -4 |\n | Body part, lock of hair, bit of nail, or the like | -10 |", "On a successful save, the target isn't affected, and you can't use this spell against it again for 24 hours.", "On a failed save, the spell creates an invisible sensor within 10 feet of the target. You can see and hear through the sensor as if you were there. The sensor moves with the target, remaining within 10 feet of it for the duration. A creature that can see invisible objects sees the sensor as a luminous orb about the size of your fist.", "Instead of targeting a creature, you can choose a location you have seen before as the target of this spell. When you do, the sensor appears at that location and doesn't move."], duration:"Up to 10 minutes", higher_level:no, level:5, material:"A focus worth at least 1,000 gp, such as a crystal ball, a silver mirror, or a font filled with holy water.", range:self, ritual:no, school:divination}).
spell_auto_data('secret chest', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[wizard], components:[v, s, m("An exquisite chest, 3 feet by 2 feet by 2 feet, constructed from rare materials worth at least 5,000 gp, and a Tiny replica made from the same materials worth at least 50 gp.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You hide a chest, and all its contents, on the Ethereal Plane. You must touch the chest and the miniature replica that serves as a material component for the spell. The chest can contain up to 12 cubic feet of nonliving material (3 feet by 2 feet by 2 feet).", "While the chest remains on the Ethereal Plane, you can use an action and touch the replica to recall the chest. It appears in an unoccupied space on the ground within 5 feet of you. You can send the chest back to the Ethereal Plane by using an action and touching both the chest and the replica.", "After 60 days, there is a cumulative 5 percent chance per day that the spell's effect ends. This effect ends if you cast this spell again, if the smaller replica chest is destroyed, or if you choose to end the spell as an action. If the spell ends and the larger chest is on the Ethereal Plane, it is irretrievably lost."], duration:"Instantaneous", higher_level:no, level:4, material:"An exquisite chest, 3 feet by 2 feet by 2 feet, constructed from rare materials worth at least 5,000 gp, and a Tiny replica made from the same materials worth at least 50 gp.", range:touch, ritual:no, school:conjuration}).
spell_auto_data('see invisibility', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, wizard], components:[v, s, m("A dash of talc and a small amount of silver powder.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["For the duration of the spell, you see invisible creatures and objects as if they were visible, and you can see through Ethereal. The ethereal objects and creatures appear ghostly translucent."], duration:"1 hour", higher_level:no, level:2, material:"A dash of talc and a small amount of silver powder.", range:self, ritual:no, school:divination}).
spell_auto_data(seeming, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, wizard], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["This spell allows you to change the appearance of any number of creatures that you can see within range. You give each target you choose a new, illusory appearance. An unwilling target can make a charisma saving throw, and if it succeeds, it is unaffected by this spell.", "The spell disguises physical appearance as well as clothing, armor, weapons, and equipment. You can make each creature seem 1 foot shorter or taller and appear thin, fat, or in between. You can't change a target's body type, so you must choose a form that has the same basic arrangement of limbs. Otherwise, the extent of the illusion is up to you. The spell lasts for the duration, unless you use your action to dismiss it sooner.", "The changes wrought by this spell fail to hold up to physical inspection. For example, if you use this spell to add a hat to a creature's outfit, objects pass through the hat, and anyone who touches it would feel nothing or would feel the creature's head and hair. If you use this spell to appear thinner than you are, the hand of someone who reaches out to touch you would bump into you while it was seemingly still in midair.", "A creature can use its action to inspect a target and make an Intelligence (Investigation) check against your spell save DC. If it succeeds, it becomes aware that the target is disguised."], duration:"8 hours", higher_level:no, level:5, material:false, range:feet(30), ritual:no, school:illusion}).
spell_auto_data(sending, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, cleric, wizard], components:[v, s, m("A short piece of fine copper wire.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You send a short message of twenty-five words or less to a creature with which you are familiar. The creature hears the message in its mind, recognizes you as the sender if it knows you, and can answer in a like manner immediately. The spell enables creatures with Intelligence scores of at least 1 to understand the meaning of your message.", "You can send the message across any distance and even to other planes of existence, but if the target is on a different plane than you, there is a 5 percent chance that the message doesn't arrive."], duration:"1 round", higher_level:no, level:3, material:"A short piece of fine copper wire.", range:unlimited, ritual:no, school:evocation}).
spell_auto_data(sequester, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[wizard], components:[v, s, m("A powder composed of diamond, emerald, ruby, and sapphire dust worth at least 5,000 gp, which the spell consumes.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["By means of this spell, a willing creature or an object can be hidden away, safe from detection for the duration. When you cast the spell and touch the target, it becomes invisible and can't be targeted by divination spells or perceived through scrying sensors created by divination spells.", "If the target is a creature, it falls into a state of suspended animation. Time ceases to flow for it, and it doesn't grow older.", "You can set a condition for the spell to end early. The condition can be anything you choose, but it must occur or be visible within 1 mile of the target. Examples include \"after 1,000 years\" or \"when the tarrasque awakens.\" This spell also ends if the target takes any damage."], duration:"Until dispelled", higher_level:no, level:7, material:"A powder composed of diamond, emerald, ruby, and sapphire dust worth at least 5,000 gp, which the spell consumes.", range:touch, ritual:no, school:transmutation}).
spell_auto_data(shapechange, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[druid, wizard], components:[v, s, m("A jade circlet worth at least 1,500 gp, which you must place on your head before you cast the spell.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You assume the form of a different creature for the duration. The new form can be of any creature with a challenge rating equal to your level or lower. The creature can't be a construct or an undead, and you must have seen the sort of creature at least once. You transform into an average example of that creature, one without any class levels or the Spellcasting trait.", "Your game statistics are replaced by the statistics of the chosen creature, though you retain your alignment and Intelligence, Wisdom, and Charisma scores. You also retain all of your skill and saving throw proficiencies, in addition to gaining those of the creature. If the creature has the same proficiency as you and the bonus listed in its statistics is higher than yours, use the creature's bonus in place of yours. You can't use any legendary actions or lair actions of the new form.", "You assume the hit points and Hit Dice of the new form. When you revert to your normal form, you return to the number of hit points you had before you transformed. If you revert as a result of dropping to 0 hit points, any excess damage carries over to your normal form. As long as the excess damage doesn't reduce your normal form to 0 hit points, you aren't knocked unconscious.", "You retain the benefit of any features from your class, race, or other source and can use them, provided that your new form is physically capable of doing so. You can't use any special senses you have (for example, darkvision) unless your new form also has that sense. You can only speak if the creature can normally speak.", "When you transform, you choose whether your equipment falls to the ground, merges into the new form, or is worn by it. Worn equipment functions as normal. The DM determines whether it is practical for the new form to wear a piece of equipment, based on the creature's shape and size. Your equipment doesn't change shape or size to match the new form, and any equipment that the new form can't wear must either fall to the ground or merge into your new form. Equipment that merges has no effect in that state.", "During this spell's duration, you can use your action to assume a different form following the same restrictions and rules for the original form, with one exception: if your new form has more hit points than your current one, your hit points remain at their current value."], duration:"Up to 1 hour", higher_level:no, level:9, material:"A jade circlet worth at least 1,500 gp, which you must place on your head before you cast the spell.", range:self, ritual:no, school:transmutation}).
spell_auto_data(shatter, properties{area_of_effect:10 ft sphere, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, warlock, wizard], components:[v, s, m("A burst of mica.")], concentration:no, damage_at_slot_level:_{2:damage(thunder, 3 d 8), 3:damage(thunder, 4 d 8), 4:damage(thunder, 5 d 8), 5:damage(thunder, 6 d 8), 6:damage(thunder, 7 d 8), 7:damage(thunder, 8 d 8), 8:damage(thunder, 9 d 8), 9:damage(thunder, 10 d 8)}, damage_with_cantrip_scaling:false, dc:con else half, desc:["A sudden loud ringing noise, painfully intense, erupts from a point of your choice within range. Each creature in a 10-foot-radius sphere centered on that point must make a Constitution saving throw. A creature takes 3 d 8 thunder damage on a failed save, or half as much damage on a successful one. A creature made of inorganic material such as stone, crystal, or metal has disadvantage on this saving throw.", "A non-magical item that is not worn or carried also suffers damage if it is in the area of the spell."], duration:"Instantaneous", higher_level:"When you cast this spell using a 3 or higher level spell slot, the damage of the spell increases by 1 d 8 for each level of higher spell slot 2.", level:2, material:"A burst of mica.", range:feet(60), ritual:no, school:evocation}).
spell_auto_data(shield, properties{area_of_effect:false, attack_type:false, casting_time:"1 reaction", classes:[sorcerer, wizard], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["An invisible barrier of magical force appears and protects you. Until the start of your next turn, you have a +5 bonus to AC, including against the triggering attack, and you take no damage from magic missile."], duration:"1 round", higher_level:no, level:1, material:false, range:self, ritual:no, school:abjuration}).
spell_auto_data('shield of faith', properties{area_of_effect:false, attack_type:false, casting_time:"1 bonus action", classes:[cleric, paladin], components:[v, s, m("A small parchment with a bit of holy text written on it.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["A shimmering field appears and surrounds a creature of your choice within range, granting it a +2 bonus to AC for the duration."], duration:"Up to 10 minutes", higher_level:no, level:1, material:"A small parchment with a bit of holy text written on it.", range:feet(60), ritual:no, school:abjuration}).
spell_auto_data(shillelagh, properties{area_of_effect:false, attack_type:false, casting_time:"1 bonus action", classes:[druid], components:[v, s, m("Mistletoe, a shamrock leaf, and a club or quarterstaff.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["The wood of a club or a quarterstaff you are holding is imbued with nature's power. For the duration, you can use your spellcasting ability instead of Strength for the attack and damage rolls of melee attacks using that weapon, and the weapon's damage die becomes a d8. The weapon also becomes magical, if it isn't already. The spell ends if you cast it again or if you let go of the weapon."], duration:"1 minute", higher_level:no, level:0, material:"Mistletoe, a shamrock leaf, and a club or quarterstaff.", range:touch, ritual:no, school:transmutation}).
spell_auto_data('shocking grasp', properties{area_of_effect:false, attack_type:melee, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:damage(lightning, 1 d 8), dc:false, desc:["Lightning springs from your hand to deliver a shock to a creature you try to touch. Make a melee spell attack against the target. You have advantage on the attack roll if the target is wearing armor made of metal. On a hit, the target takes 1 d 8 lightning damage, and it can't take reactions until the start of its next turn.", "The spell's damage increases by 1 d 8 when you reach 5th level (2 d 8), 11th level (3 d 8), and 17th level (4 d 8)."], duration:"Instantaneous", higher_level:no, level:0, material:false, range:touch, ritual:no, school:evocation}).
spell_auto_data(silence, properties{area_of_effect:20 ft sphere, attack_type:false, casting_time:"1 action", classes:[bard, cleric, ranger], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["For the duration, no sound can be created within or pass through a 20-foot-radius sphere centered on a point you choose within range. Any creature or object entirely inside the sphere is immune to thunder damage, and creatures are deafened while entirely inside it.", "Casting a spell that includes a verbal component is impossible there."], duration:"Up to 10 minutes", higher_level:no, level:2, material:false, range:feet(120), ritual:yes, school:illusion}).
spell_auto_data('silent image', properties{area_of_effect:15 ft cube, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, wizard], components:[v, s, m("A bit of fleece.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You create the image of an object, a creature, or some other visible phenomenon that is no larger than a 15-foot cube. The image appears at a spot within range and lasts for the duration. The image is purely visual; it isn't accompanied by sound, smell, or other sensory effects.", "You can use your action to cause the image to move to any spot within range. As the image changes location, you can alter its appearance so that its movements appear natural for the image. For example, if you create an image of a creature and move it, you can alter the image so that it appears to be walking.", "Physical interaction with the image reveals it to be an illusion, because things can pass through it. A creature that uses its action to examine the image can determine that it is an illusion with a successful Intelligence (Investigation) check against your spell save DC. If a creature discerns the illusion for what it is, the creature can see through the image."], duration:"Up to 10 minutes", higher_level:no, level:1, material:"A bit of fleece.", range:feet(60), ritual:no, school:illusion}).
spell_auto_data(simulacrum, properties{area_of_effect:false, attack_type:false, casting_time:"12 hours", classes:[wizard], components:[v, s, m("Snow or ice in quantities sufficient to made a life-size copy of the duplicated creature; some hair, fingernail clippings, or other piece of that creature's body placed inside the snow or ice; and powdered ruby worth 1,500 gp, sprinkled over the duplicate and consumed by the spell.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You shape an illusory duplicate of one beast or humanoid that is within range for the entire casting time of the spell. The duplicate is a creature, partially real and formed from ice or snow, and it can take actions and otherwise be affected as a normal creature. It appears to be the same as the original, but it has half the creature's hit point maximum and is formed without any equipment. Otherwise, the illusion uses all the statistics of the creature it duplicates.", "The simulacrum is friendly to you and creatures you designate. It obeys your spoken commands, moving and acting in accordance with your wishes and acting on your turn in combat. The simulacrum lacks the ability to learn or become more powerful, so it never increases its level or other abilities, nor can it regain expended spell slots.", "If the simulacrum is damaged, you can repair it in an alchemical laboratory, using rare herbs and minerals worth 100 gp per hit point it regains. The simulacrum lasts until it drops to 0 hit points, at which point it reverts to snow and melts instantly.", "If you cast this spell again, any currently active duplicates you created with this spell are instantly destroyed."], duration:"Until dispelled", higher_level:no, level:7, material:"Snow or ice in quantities sufficient to made a life-size copy of the duplicated creature; some hair, fingernail clippings, or other piece of that creature's body placed inside the snow or ice; and powdered ruby worth 1,500 gp, sprinkled over the duplicate and consumed by the spell.", range:touch, ritual:no, school:illusion}).
spell_auto_data(sleep, properties{area_of_effect:20 ft sphere, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, wizard], components:[v, s, m("A pinch of fine sand, rose petals, or a cricket.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["This spell sends creatures into a magical slumber. Roll 5 d 8; the total is how many hit points of creatures this spell can affect. Creatures within 20 feet of a point you choose within range are affected in ascending order of their current hit points (ignoring unconscious creatures).", "Starting with the creature that has the lowest current hit points, each creature affected by this spell falls unconscious until the spell ends, the sleeper takes damage, or someone uses an action to shake or slap the sleeper awake. Subtract each creature's hit points from the total before moving on to the creature with the next lowest hit points. A creature's hit points must be equal to or less than the remaining total for that creature to be affected.", "Undead and creatures immune to being charmed aren't affected by this spell."], duration:"1 minute", higher_level:"When you cast this spell using a spell slot of 2nd level or higher, roll an additional 2 d 8 for each slot level above 1st.", level:1, material:"A pinch of fine sand, rose petals, or a cricket.", range:feet(90), ritual:no, school:enchantment}).
spell_auto_data('sleet storm', properties{area_of_effect:40 ft cylinder, attack_type:false, casting_time:"1 action", classes:[druid, sorcerer, wizard], components:[v, s, m("A pinch of dust and a few drops of water.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Until the spell ends, freezing rain and sleet fall in a 20-foot-tall cylinder with a 40-foot radius centered on a point you choose within range. The area is heavily obscured, and exposed flames in the area are doused.", "The ground in the area is covered with slick ice, making it difficult terrain. When a creature enters the spell's area for the first time on a turn or starts its turn there, it must make a dexterity saving throw. On a failed save, it falls prone.", "If a creature is concentrating in the spell's area, the creature must make a successful constitution saving throw against your spell save DC or lose concentration."], duration:"Up to 1 minute", higher_level:no, level:3, material:"A pinch of dust and a few drops of water.", range:feet(150), ritual:no, school:conjuration}).
spell_auto_data(slow, properties{area_of_effect:40 ft cube, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s, m("A drop of molasses.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else none, desc:["You alter time around up to six creatures of your choice in a 40-foot cube within range. Each target must succeed on a wisdom saving throw or be affected by this spell for the duration.", "An affected target's speed is halved, it takes a -2 penalty to AC and dexterity saving throws, and it can't use reactions. On its turn, it can use either an action or a bonus action, not both. Regardless of the creature's abilities or magic items, it can't make more than one melee or ranged attack during its turn.", "If the creature attempts to cast a spell with a casting time of 1 action, roll a d20. On an 11 or higher, the spell doesn't take effect until the creature's next turn, and the creature must use its action on that turn to complete the spell. If it can't, the spell is wasted.", "A creature affected by this spell makes another wisdom saving throw at the end of its turn. On a successful save, the effect ends for it."], duration:"Up to 1 minute", higher_level:no, level:3, material:"A drop of molasses.", range:feet(120), ritual:no, school:transmutation}).
spell_auto_data('spare the dying', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch a living creature that has 0 hit points. The creature becomes stable. This spell has no effect on undead or constructs."], duration:"Instantaneous", higher_level:no, level:0, material:false, range:touch, ritual:no, school:necromancy}).
spell_auto_data('speak with animals', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, druid, ranger], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You gain the ability to comprehend and verbally communicate with beasts for the duration. The knowledge and awareness of many beasts is limited by their intelligence, but at a minimum, beasts can give you information about nearby locations and monsters, including whatever they can perceive or have perceived within the past day. You might be able to persuade a beast to perform a small favor for you, at the DM's discretion."], duration:"10 minutes", higher_level:no, level:1, material:false, range:self, ritual:yes, school:divination}).
spell_auto_data('speak with dead', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, cleric], components:[v, s, m("Burning incense.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You grant the semblance of life and intelligence to a corpse of your choice within range, allowing it to answer the questions you pose. The corpse must still have a mouth and can't be undead. The spell fails if the corpse was the target of this spell within the last 10 days.", "Until the spell ends, you can ask the corpse up to five questions. The corpse knows only what it knew in life, including the languages it knew. Answers are usually brief, cryptic, or repetitive, and the corpse is under no compulsion to offer a truthful answer if you are hostile to it or it recognizes you as an enemy. This spell doesn't return the creature's soul to its body, only its animating spirit. Thus, the corpse can't learn new information, doesn't comprehend anything that has happened since it died, and can't speculate about future events."], duration:"10 minutes", higher_level:no, level:3, material:"Burning incense.", range:feet(10), ritual:no, school:necromancy}).
spell_auto_data('speak with plants', properties{area_of_effect:30 ft sphere, attack_type:false, casting_time:"1 action", classes:[bard, druid, ranger], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You imbue plants within 30 feet of you with limited sentience and animation, giving them the ability to communicate with you and follow your simple commands. You can question plants about events in the spell's area within the past day, gaining information about creatures that have passed, weather, and other circumstances.", "You can also turn difficult terrain caused by plant growth (such as thickets and undergrowth) into ordinary terrain that lasts for the duration. Or you can turn ordinary terrain where plants are present into difficult terrain that lasts for the duration, causing vines and branches to hinder pursuers, for example.", "Plants might be able to perform other tasks on your behalf, at the DM's discretion. The spell doesn't enable plants to uproot themselves and move about, but they can freely move branches, tendrils, and stalks.", "If a plant creature is in the area, you can communicate with it as if you shared a common language, but you gain no magical ability to influence it.", "This spell can cause the plants created by the entangle spell to release a restrained creature."], duration:"10 minutes", higher_level:no, level:3, material:false, range:self, ritual:no, school:transmutation}).
spell_auto_data('spider climb', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[sorcerer, warlock, wizard], components:[v, s, m("A drop of bitumen and a spider.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Until the spell ends, one willing creature you touch gains the ability to move up, down, and across vertical surfaces and upside down along ceilings, while leaving its hands free. The target also gains a climbing speed equal to its walking speed."], duration:"Up to 1 hour", higher_level:no, level:2, material:"A drop of bitumen and a spider.", range:touch, ritual:no, school:transmutation}).
spell_auto_data('spike growth', properties{area_of_effect:20 ft cylinder, attack_type:false, casting_time:"1 action", classes:[druid, ranger], components:[v, s, m("Seven sharp thorns or seven small twigs, each sharpened to a point.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["The ground in a 20-foot radius centered on a point within range twists and sprouts hard spikes and thorns. The area becomes difficult terrain for the duration. When a creature moves into or within the area, it takes 2 d 4 piercing damage for every 5 feet it travels.", "The transformation of the ground is camouflaged to look natural. Any creature that can't see the area at the time the spell is cast can make a Wisdom (Perciption) check against your spell save DC to recognize the terrain as hazardous before entering it."], duration:"Up to 10 minutes", higher_level:no, level:2, material:"Seven sharp thorns or seven small twigs, each sharpened to a point.", range:feet(150), ritual:no, school:transmutation}).
spell_auto_data('spirit guardians', properties{area_of_effect:15 ft sphere, attack_type:false, casting_time:"1 action", classes:[cleric], components:[v, s, m("A holy symbol.")], concentration:yes, damage_at_slot_level:_{3:damage(radiant, 3 d 8), 4:damage(radiant, 4 d 8), 5:damage(radiant, 5 d 8), 6:damage(radiant, 6 d 8), 7:damage(radiant, 7 d 8), 8:damage(radiant, 8 d 8), 9:damage(radiant, 9 d 8)}, damage_with_cantrip_scaling:false, dc:wis else half, desc:["You call forth spirits to protect you. They flit around you to a distance of 15 feet for the duration. If you are good or neutral, their spectral form appears angelic or fey (your choice). If you are evil, they appear fiendish.", "When you cast this spell, you can designate any number of creatures you can see to be unaffected by it. An affected creature's speed is halved in the area, and when the creature enters the area for the first time on a turn or starts its turn there, it must make a wisdom saving throw. On a failed save, the creature takes 3 d 8 radiant damage (if you are good or neutral) or 3 d 8 necrotic damage (if you are evil). On a successful save, the creature takes half as much damage."], duration:"Up to 10 minutes", higher_level:"When you cast this spell using a spell slot of 4th level or higher, the damage increases by 1 d 8 for each slot level above 3rd.", level:3, material:"A holy symbol.", range:self, ritual:no, school:conjuration}).
spell_auto_data('spiritual weapon', properties{area_of_effect:false, attack_type:melee, casting_time:"1 bonus action", classes:[cleric], components:[v, s], concentration:no, damage_at_slot_level:_{}, damage_with_cantrip_scaling:false, dc:false, desc:["You create a floating, spectral weapon within range that lasts for the duration or until you cast this spell again. When you cast the spell, you can make a melee spell attack against a creature within 5 feet of the weapon. On a hit, the target takes force damage equal to 1 d 8 + your spellcasting ability modifier.", "As a bonus action on your turn, you can move the weapon up to 20 feet and repeat the attack against a creature within 5 feet of it.", "The weapon can take whatever form you choose. Clerics of deities who are associated with a particular weapon (as St. Cuthbert is known for his mace and Thor for his hammer) make this spell's effect resemble that weapon."], duration:"1 minute", higher_level:"When you cast this spell using a spell slot of 3rd level or higher, the damage increases by 1 d 8 for every two slot levels above the 2nd.", level:2, material:false, range:feet(60), ritual:no, school:evocation}).
spell_auto_data('stinking cloud', properties{area_of_effect:20 ft sphere, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, wizard], components:[v, s, m("A rotten egg or several skunk cabbage leaves.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:con else none, desc:["You create a 20-foot-radius sphere of yellow, nauseating gas centered on a point within range. The cloud spreads around corners, and its area is heavily obscured. The cloud lingers in the air for the duration.", "Each creature that is completely within the cloud at the start of its turn must make a constitution saving throw against poison. On a failed save, the creature spends its action that turn retching and reeling. Creatures that don't need to breathe or are immune to poison automatically succeed on this saving throw.", "A moderate wind (at least 10 miles per hour) disperses the cloud after 4 rounds. A strong wind (at least 20 miles per hour) disperses it after 1 round."], duration:"Up to 1 minute", higher_level:no, level:3, material:"A rotten egg or several skunk cabbage leaves.", range:feet(90), ritual:no, school:conjuration}).
spell_auto_data('stone shape', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric, druid, wizard], components:[v, s, m("Soft clay, to be crudely worked into the desired shape for the stone object.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch a stone object of Medium size or smaller or a section of stone no more than 5 feet in any dimension and form it into any shape that suits your purpose. So, for example, you could shape a large rock into a weapon, idol, or coffer, or make a small passage through a wall, as long as the wall is less than 5 feet thick. You could also shape a stone door or its frame to seal the door shut. The object you create can have up to two hinges and a latch, but finer mechanical detail isn't possible."], duration:"Instantaneous", higher_level:no, level:4, material:"Soft clay, to be crudely worked into the desired shape for the stone object.", range:touch, ritual:no, school:transmutation}).
spell_auto_data(stoneskin, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[druid, ranger, sorcerer, wizard], components:[v, s, m("Diamond dust worth 100 gp, which the spell consumes.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["This spell turns the flesh of a willing creature you touch as hard as stone. Until the spell ends, the target has resistance to nonmagical bludgeoning, piercing, and slashing damage."], duration:"Up to 1 hour", higher_level:no, level:4, material:"Diamond dust worth 100 gp, which the spell consumes.", range:touch, ritual:no, school:abjuration}).
spell_auto_data('storm of vengeance', properties{area_of_effect:360 ft sphere, attack_type:false, casting_time:"1 action", classes:[druid], components:[v, s], concentration:yes, damage_at_slot_level:_{9:damage(thunder, 2 d 6)}, damage_with_cantrip_scaling:false, dc:con else none, desc:["A churning storm cloud forms, centered on a point you can see and spreading to a radius of 360 feet. Lightning flashes in the area, thunder booms, and strong winds roar. Each creature under the cloud (no more than 5,000 feet beneath the cloud) when it appears must make a constitution saving throw. On a failed save, a creature takes 2 d 6 thunder damage and becomes deafened for 5 minutes.", "Each round you maintain concentration on this spell, the storm produces additional effects on your turn.", "**Round 2.** Acidic rain falls from the cloud. Each creature and object under the cloud takes 1 d 6 acid damage.", "**Round 3.** You call six bolts of lightning from the cloud to strike six creatures or objects of your choice beneath the cloud. A given creature or object can't be struck by more than one bolt. A struck creature must make a dexterity saving throw. The creature takes 10 d 6 lightning damage on a failed save, or half as much damage on a successful one.", "**Round 4.** Hailstones rain down from the cloud. Each creature under the cloud takes 2 d 6 bludgeoning damage.", "**Round 5-10.** Gusts and freezing rain assail the area under the cloud. The area becomes difficult terrain and is heavily obscured. Each creature there takes 1 d 6 cold damage. Ranged weapon attacks in the area are impossible. The wind and rain count as a severe distraction for the purposes of maintaining concentration on spells. Finally, gusts of strong wind (ranging from 20 to 50 miles per hour) automatically disperse fog, mists, and similar phenomena in the area, whether mundane or magical."], duration:"Up to 1 minute", higher_level:no, level:9, material:false, range:sight, ritual:no, school:conjuration}).
spell_auto_data(suggestion, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, warlock, wizard], components:[v, m("A snake's tongue and either a bit of honeycomb or a drop of sweet oil.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else none, desc:["You suggest a course of activity (limited to a sentence or two) and magically influence a creature you can see within range that can hear and understand you. Creatures that can't be charmed are immune to this effect. The suggestion must be worded in such a manner as to make the course of action sound reasonable. Asking the creature to stab itself, throw itself onto a spear, immolate itself, or do some other obviously harmful act ends the spell.", "The target must make a wisdom saving throw. On a failed save, it pursues the course of action you described to the best of its ability. The suggested course of action can continue for the entire duration. If the suggested activity can be completed in a shorter time, the spell ends when the subject finishes what it was asked to do.", "You can also specify conditions that will trigger a special activity during the duration. For example, you might suggest that a knight give her warhorse to the first beggar she meets. If the condition isn't met before the spell expires, the activity isn't performed.", "If you or any of your companions damage the target, the spell ends."], duration:"Up to 8 hours", higher_level:no, level:2, material:"A snake's tongue and either a bit of honeycomb or a drop of sweet oil.", range:feet(30), ritual:no, school:enchantment}).
spell_auto_data(sunbeam, properties{area_of_effect:60 ft line, attack_type:false, casting_time:"1 action", classes:[druid, sorcerer, wizard], components:[v, s, m("A magnifying glass.")], concentration:yes, damage_at_slot_level:_{6:damage(radiant, 6 d 8)}, damage_with_cantrip_scaling:false, dc:con else half, desc:["A beam of brilliant light flashes out from your hand in a 5-foot-wide, 60-foot-long line. Each creature in the line must make a constitution saving throw. On a failed save, a creature takes 6 d 8 radiant damage and is blinded until your next turn. On a successful save, it takes half as much damage and isn't blinded by this spell. Undead and oozes have disadvantage on this saving throw.", "You can create a new line of radiance as your action on any turn until the spell ends.", "For the duration, a mote of brilliant radiance shines in your hand. It sheds bright light in a 30-foot radius and dim light for an additional 30 feet. This light is sunlight."], duration:"Up to 1 minute", higher_level:no, level:6, material:"A magnifying glass.", range:self, ritual:no, school:evocation}).
spell_auto_data(sunburst, properties{area_of_effect:60 ft cylinder, attack_type:false, casting_time:"1 action", classes:[druid, sorcerer, wizard], components:[v, s, m("Fire and a piece of sunstone.")], concentration:no, damage_at_slot_level:_{8:damage(radiant, 12 d 6)}, damage_with_cantrip_scaling:false, dc:con else half, desc:["Brilliant sunlight flashes in a 60-foot radius centered on a point you choose within range. Each creature in that light must make a constitution saving throw. On a failed save, a creature takes 12 d 6 radiant damage and is blinded for 1 minute. On a successful save, it takes half as much damage and isn't blinded by this spell. Undead and oozes have disadvantage on this saving throw.", "A creature blinded by this spell makes another constitution saving throw at the end of each of its turns. On a successful save, it is no longer blinded.", "This spell dispels any darkness in its area that was created by a spell."], duration:"Instantaneous", higher_level:no, level:8, material:"Fire and a piece of sunstone.", range:feet(150), ritual:no, school:evocation}).
spell_auto_data(symbol, properties{area_of_effect:10 ft cube, attack_type:false, casting_time:"1 minute", classes:[bard, cleric, wizard], components:[v, s, m("Mercury, phosphorus, and powdered diamond and opal with a total value of at least 1,000 gp, which the spell consumes.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["When you cast this spell, you inscribe a harmful glyph either on a surface (such as a section of floor, a wall, or a table) or within an object that can be closed to conceal the glyph (such as a book, a scroll, or a treasure chest). If you choose a surface, the glyph can cover an area of the surface no larger than 10 feet in diameter. If you choose an object, that object must remain in its place; if the object is moved more than 10 feet from where you cast this spell, the glyph is broken, and the spell ends without being triggered.", "The glyph is nearly invisible, requiring an Intelligence (Investigation) check against your spell save DC to find it.", "You decide what triggers the glyph when you cast the spell. For glyphs inscribed on a surface, the most typical triggers include touching or stepping on the glyph, removing another object covering it, approaching within a certain distance of it, or manipulating the object that holds it. For glyphs inscribed within an object, the most common triggers are opening the object, approaching within a certain distance of it, or seeing or reading the glyph.", "You can further refine the trigger so the spell is activated only under certain circumstances or according to a creature's physical characteristics (such as height or weight), or physical kind (for example, the ward could be set to affect hags or shapechangers). You can also specify creatures that don't trigger the glyph, such as those who say a certain password.", "When you inscribe the glyph, choose one of the options below for its effect. Once triggered, the glyph glows, filling a 60-foot-radius sphere with dim light for 10 minutes, after which time the spell ends. Each creature in the sphere when the glyph activates is targeted by its effect, as is a creature that enters the sphere for the first time on a turn or ends its turn there.", "**Death.** Each target must make a constitution saving throw, taking 10d 10 necrotic damage on a failed save, or half as much damage on a successful save.", "**Discord.** Each target must make a constitution saving throw. On a failed save, a target bickers and argues with other creatures for 1 minute. During this time, it is incapable of meaningful communication and has disadvantage on attack rolls and ability checks.", "**Fear.** Each target must make a wisdom saving throw and becomes frightened for 1 minute on a failed save. While frightened, the target drops whatever it is holding and must move at least 30 feet away from the glyph on each of its turns, if able.", "**Hopelessness.** Each target must make a charisma saving throw. On a failed save, the target is overwhelmed with despair for 1 minute. During this time, it can't attack or target any creature with harmful abilities, spells, or other magical effects.", "**Insanity.** Each target must make an intelligence saving throw. On a failed save, the target is driven insane for 1 minute. An insane creature can't take actions, can't understand what other creatures say, can't read, and speaks only in gibberish. The DM controls its movement, which is erratic.", "**Pain.** Each target must make a constitution saving throw and becomes incapacitated with excruciating pain for 1 minute on a failed save.", "**Sleep.** Each target must make a wisdom saving throw and falls unconscious for 10 minutes on a failed save. A creature awakens if it takes damage or if someone uses an action to shake or slap it awake.", "**Stunning.** Each target must make a wisdom saving throw and becomes stunned for 1 minute on a failed save."], duration:"Until dispelled", higher_level:no, level:7, material:"Mercury, phosphorus, and powdered diamond and opal with a total value of at least 1,000 gp, which the spell consumes.", range:touch, ritual:no, school:abjuration}).
spell_auto_data(telekinesis, properties{area_of_effect:30 ft sphere, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You gain the ability to move or manipulate creatures or objects by thought. When you cast the spell, and as your action each round for the duration, you can exert your will on one creature or object that you can see within range, causing the appropriate effect below. You can affect the same target round after round, or choose a new one at any time. If you switch targets, the prior target is no longer affected by the spell.", "**Creature.** You can try to move a Huge or smaller creature. Make an ability check with your spellcasting ability contested by the creature's Strength check. If you win the contest, you move the creature up to 30 feet in any direction, including upward but not beyond the range of this spell. Until the end of your next turn, the creature is restrained in your telekinetic grip. A creature lifted upward is suspended in mid-air.", "On subsequent rounds, you can use your action to attempt to maintain your telekinetic grip on the creature by repeating the contest.", "**Object.** You can try to move an object that weighs up to 1,000 pounds. If the object isn't being worn or carried, you automatically move it up to 30 feet in any direction, but not beyond the range of this spell.", "If the object is worn or carried by a creature, you must make an ability check with your spellcasting ability contested by that creature's Strength check. If you succeed, you pull the object away from that creature and can move it up to 30 feet in any direction but not beyond the range of this spell.", "You can exert fine control on objects with your telekinetic grip, such as manipulating a simple tool, opening a door or a container, stowing or retrieving an item from an open container, or pouring the contents from a vial."], duration:"Up to 10 minutes", higher_level:no, level:5, material:false, range:feet(60), ritual:no, school:transmutation}).
spell_auto_data('telepathic bond', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[wizard], components:[v, s, m("Pieces of eggshell from two different kinds of creatures")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You forge a telepathic link among up to eight willing creatures of your choice within range, psychically linking each creature to all the others for the duration. Creatures with Intelligence scores of 2 or less aren't affected by this spell.", "Until the spell ends, the targets can communicate telepathically through the bond whether or not they have a common language. The communication is possible over any distance, though it can't extend to other planes of existence."], duration:"1 hour", higher_level:no, level:5, material:"Pieces of eggshell from two different kinds of creatures", range:feet(30), ritual:yes, school:divination}).
spell_auto_data(teleport, properties{area_of_effect:10 ft cube, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, wizard], components:[v], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["This spell instantly transports you and up to eight willing creatures of your choice that you can see within range, or a single object that you can see within range, to a destination you select. If you target an object, it must be able to fit entirely inside a 10-foot cube, and it can't be held or carried by an unwilling creature.", "The destination you choose must be known to you, and it must be on the same plane of existence as you. Your familiarity with the destination determines whether you arrive there successfully. The DM rolls d100 and consults the table.", "| Familiarity | Mishap | Similar Area | Off Target | On Target |\n |---|---|---|---|---|\n | Permanent circle | -- | -- | -- | 01-100 |\n | Associated object | -- | -- | -- | 01-100 |\n | Very familiar | 01-05 | 06-13 | 14-24 | 25-100 |\n | Seen casually | 01-33 | 34-43 | 44-53 | 54-100 |\n | Viewed once | 01-43 | 44-53 | 54-73 | 74-100 |\n | Description | 01-43 | 44-53 | 54-73 | 74-100 |\n | False destination | 01-50 | 51-100 | -- | -- |", "**Familiarity.** \"Permanent circle\" means a permanent teleportation circle whose sigil sequence you know.", "\"Associated object\" means that you possess an object taken from the desired destination within the last six months, such as a book from a wizard's library, bed linen from a royal suite, or a chunk of marble from a lich's secret tomb.", "\"Very familiar\" is a place you have been very often, a place you have carefully studied, or a place you can see when you cast the spell.", "\"Seen casually\" is someplace you have seen more than once but with which you aren't very familiar.", "\"Viewed once\" is a place you have seen once, possibly using magic.", "\"Description\" is a place whose location and appearance you know through someone else's description, perhaps from a map.", "\"False destination\" is a place that doesn't exist. Perhaps you tried to scry an enemy's sanctum but instead viewed an illusion, or you are attempting to teleport to a familiar location that no longer exists.", "**On Target.** You and your group (or the target object) appear where you want to.", "**Off Target.** You and your group (or the target object) appear a random distance away from the destination in a random direction. Distance off target is 1 d 10 x 1 d 10 percent of the distance that was to be traveled. For example, if you tried to travel 120 miles, landed off target, and rolled a 5 and 3 on the two d10s, then you would be off target by 15 percent, or 18 miles. The GM determines the direction off target randomly by rolling a d8 and designating 1 as north, 2 as northeast, 3 as east, and so on around the points of the compass.  If you were teleporting to a coastal city and wound up 18 miles out at sea, you could be in trouble.", "**Similar Area.** You and your group (or the target object) wind up in a different area that's visually or thematically similar to the target area. If you are heading for your home laboratory, for example, you might wind up in another wizard's laboratory or in an alchemical supply shop that has many of the same tools and implements as your laboratory. Generally, you appear in the closest similar place, but since the spell has no range limit, you could conceivably wind up anywhere on the plane.", "**Mishap.** The spell's unpredictable magic results in a difficult journey. Each teleporting creature (or the target object) takes 3 d 10 force damage, and the GM rerolls on the table to see where you wind up (multiple mishaps can occur, dealing damage each time)."], duration:"Instantaneous", higher_level:no, level:7, material:false, range:feet(10), ritual:no, school:conjuration}).
spell_auto_data('teleportation circle', properties{area_of_effect:10 ft sphere, attack_type:false, casting_time:"1 minute", classes:[bard, sorcerer, wizard], components:[v, m("Rare chalks and inks infused with precious gems with 50 gp, which the spell consumes.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["As you cast the spell, you draw a 10-foot-diameter circle on the ground inscribed with sigils that link your location to a permanent teleportation circle of your choice whose sigil sequence you know and that is on the same plane of existence as you. A shimmering portal opens within the circle you drew and remains open until the end of your next turn. Any creature that enters the portal instantly appears within 5 feet of the destination circle or in the nearest unoccupied space if that space is occupied.", "Many major temples, guilds, and other important places have permanent teleportation circles inscribed somewhere within their confines. Each such circle includes a unique sigil sequence--a string of magical runes arranged in a particular pattern. When you first gain the ability to cast this spell, you learn the sigil sequences for two destinations on the Material Plane, determined by the DM. You can learn additional sigil sequences during your adventures. You can commit a new sigil sequence to memory after studying it for 1 minute.", "You can create a permanent teleportation circle by casting this spell in the same location every day for one year. You need not use the circle to teleport when you cast the spell in this way."], duration:"1 round", higher_level:no, level:5, material:"Rare chalks and inks infused with precious gems with 50 gp, which the spell consumes.", range:feet(10), ritual:no, school:conjuration}).
spell_auto_data(thaumaturgy, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric], components:[v], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You manifest a minor wonder, a sign of supernatural power, within range. You create one of the following magical effects within range.", "- Your voice booms up to three times as loud as normal for 1 minute.", "- You cause flames to flicker, brighten, dim, or change color for 1 minute.", "- You cause harmless tremors in the ground for 1 minute.", "- You create an instantaneous sound that originates from a point of your choice within range, such as a rumble of thunder, the cry of a raven, or ominous whispers.", "- You instantaneously cause an unlocked door or window to fly open or slam shut.", "- You alter the appearance of your eyes for 1 minute.", "If you cast this spell multiple times, you can have up to three of its 1-minute effects active at a time, and you can dismiss such an effect as an action."], duration:"1 minute", higher_level:no, level:0, material:false, range:feet(30), ritual:no, school:transmutation}).
spell_auto_data(thunderwave, properties{area_of_effect:15 ft cube, attack_type:false, casting_time:"1 action", classes:[bard, druid, sorcerer, wizard], components:[v, s], concentration:no, damage_at_slot_level:_{1:damage(thunder, 2 d 8), 2:damage(thunder, 3 d 8), 3:damage(thunder, 4 d 8), 4:damage(thunder, 5 d 8), 5:damage(thunder, 6 d 8), 6:damage(thunder, 7 d 8), 7:damage(thunder, 8 d 8), 8:damage(thunder, 9 d 8), 9:damage(thunder, 10 d 8)}, damage_with_cantrip_scaling:false, dc:con else half, desc:["A wave of thunderous force sweeps out from you. Each creature in a 15-foot cube originating from you must make a constitution saving throw. On a failed save, a creature takes 2 d 8 thunder damage and is pushed 10 feet away from you. On a successful save, the creature takes half as much damage and isn't pushed.", "In addition, unsecured objects that are completely within the area of effect are automatically pushed 10 feet away from you by the spell's effect, and the spell emits a thunderous boom audible out to 300 feet."], duration:"Instantaneous", higher_level:"When you cast this spell using a spell slot of 2nd level or higher, the damage increases by 1 d 8 for each slot level above 1st.", level:1, material:false, range:self, ritual:no, school:evocation}).
spell_auto_data('time stop', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You briefly stop the flow of time for everyone but yourself. No time passes for other creatures, while you take 1 d 4 + 1 turns in a row, during which you can use actions and move as normal.", "This spell ends if one of the actions you use during this period, or any effects that you create during this period, affects a creature other than you or an object being worn or carried by someone other than you. In addition, the spell ends if you move to a place more than 1,000 feet from the location where you cast it."], duration:"Instantaneous", higher_level:no, level:9, material:false, range:self, ritual:no, school:transmutation}).
spell_auto_data('tiny hut', properties{area_of_effect:10 ft sphere, attack_type:false, casting_time:"1 minute", classes:[bard, wizard], components:[v, s, m("A small crystal bead.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["A 10-foot-radius immobile dome of force springs into existence around and above you and remains stationary for the duration. The spell ends if you leave its area.", "Nine creatures of Medium size or smaller can fit inside the dome with you. The spell fails if its area includes a larger creature or more than nine creatures. Creatures and objects within the dome when you cast this spell can move through it freely. All other creatures and objects are barred from passing through it. Spells and other magical effects can't extend through the dome or be cast through it. The atmosphere inside the space is comfortable and dry, regardless of the weather outside.", "Until the spell ends, you can command the interior to become dimly lit or dark. The dome is opaque from the outside, of any color you choose, but it is transparent from the inside."], duration:"8 hours", higher_level:no, level:3, material:"A small crystal bead.", range:self, ritual:yes, school:evocation}).
spell_auto_data(tongues, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, cleric, sorcerer, warlock, wizard], components:[v, m("A small clay model of a ziggurat.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["This spell grants the creature you touch the ability to understand any spoken language it hears. Moreover, when the target speaks, any creature that knows at least one language and can hear the target understands what it says."], duration:"1 hour", higher_level:no, level:3, material:"A small clay model of a ziggurat.", range:touch, ritual:no, school:divination}).
spell_auto_data('transport via plants', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[druid], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["This spell creates a magical link between a Large or larger inanimate plant within range and another plant, at any distance, on the same plane of existence. You must have seen or touched the destination plant at least once before. For the duration, any creature can step into the target plant and exit from the destination plant by using 5 feet of movement."], duration:"1 round", higher_level:no, level:6, material:false, range:feet(10), ritual:no, school:conjuration}).
spell_auto_data('tree stride', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[druid, ranger], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You gain the ability to enter a tree and move from inside it to inside another tree of the same kind within 500 feet. Both trees must be living and at least the same size as you. You must use 5 feet of movement to enter a tree. You instantly know the location of all other trees of the same kind within 500 feet and, as part of the move used to enter the tree, can either pass into one of those trees or step out of the tree you're in. You appear in a spot of your choice within 5 feet of the destination tree, using another 5 feet of movement. If you have no movement left, you appear within 5 feet of the tree you entered.", "You can use this transportation ability once per round for the duration. You must end each turn outside a tree."], duration:"Up to 1 minute", higher_level:no, level:5, material:false, range:self, ritual:no, school:conjuration}).
spell_auto_data('true polymorph', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, warlock, wizard], components:[v, s, m("A drop of mercury, a dollop of gum arabic, and a wisp of smoke.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Choose one creature or nonmagical object that you can see within range. You transform the creature into a different creature, the creature into an object, or the object into a creature (the object must be neither worn nor carried by another creature). The transformation lasts for the duration, or until the target drops to 0 hit points or dies. If you concentrate on this spell for the full duration, the transformation becomes permanent.", "Shapechangers aren't affected by this spell. An unwilling creature can make a wisdom saving throw, and if it succeeds, it isn't affected by this spell.", "**Creature into Creature.** If you turn a creature into another kind of creature, the new form can be any kind you choose whose challenge rating is equal to or less than the target's (or its level, if the target doesn't have a challenge rating). The target's game statistics, including mental ability scores, are replaced by the statistics of the new form. It retains its alignment and personality.", "The target assumes the hit points of its new form, and when it reverts to its normal form, the creature returns to the number of hit points it had before it transformed. If it reverts as a result of dropping to 0 hit points, any excess damage carries over to its normal form. As long as the excess damage doesn't reduce the creature's normal form to 0 hit points, it isn't knocked unconscious.", "The creature is limited in the actions it can perform by the nature of its new form, and it can't speak, cast spells, or take any other action that requires hands or speech unless its new form is capable of such actions.", "The target's gear melds into the new form. The creature can't activate, use, wield, or otherwise benefit from any of its equipment.", "**Object into Creature.** You can turn an object into any kind of creature, as long as the creature's size is no larger than the object's size and the creature's challenge rating is 9 or lower. The creature is friendly to you and your companions. It acts on each of your turns. You decide what action it takes and how it moves. The DM has the creature's statistics and resolves all of its actions and movement.", "If the spell becomes permanent, you no longer control the creature. It might remain friendly to you, depending on how you have treated it.", "**Creature into Object.** If you turn a creature into an object, it transforms along with whatever it is wearing and carrying into that form. The creature's statistics become those of the object, and the creature has no memory of time spent in this form, after the spell ends and it returns to its normal form."], duration:"Up to 1 hour", higher_level:no, level:9, material:"A drop of mercury, a dollop of gum arabic, and a wisp of smoke.", range:feet(30), ritual:no, school:transmutation}).
spell_auto_data('true resurrection', properties{area_of_effect:false, attack_type:false, casting_time:"1 hour", classes:[cleric, druid], components:[v, s, m("A sprinkle of holy water and diamonds worth at least 25,000gp, which the spell consumes.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You touch a creature that has been dead for no longer than 200 years and that died for any reason except old age. If the creature's soul is free and willing, the creature is restored to life with all its hit points.", "This spell closes all wounds, neutralizes any poison, cures all diseases, and lifts any curses affecting the creature when it died. The spell replaces damaged or missing organs and limbs.", "The spell can even provide a new body if the original no longer exists, in which case you must speak the creature's name. The creature then appears in an unoccupied space you choose within 10 feet of you."], duration:"Instantaneous", higher_level:no, level:9, material:"A sprinkle of holy water and diamonds worth at least 25,000gp, which the spell consumes.", range:touch, ritual:no, school:necromancy}).
spell_auto_data('true seeing', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, cleric, sorcerer, warlock, wizard], components:[v, s, m("An ointment for the eyes that costs 25gp; is made from mushroom powder, saffron, and fat; and is consumed by the spell.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["This spell gives the willing creature you touch the ability to see things as they actually are. For the duration, the creature has truesight, notices secret doors hidden by magic, and can see into the Ethereal Plane, all out to a range of 120 feet."], duration:"1 hour", higher_level:no, level:6, material:"An ointment for the eyes that costs 25gp; is made from mushroom powder, saffron, and fat; and is consumed by the spell.", range:touch, ritual:no, school:divination}).
spell_auto_data('true strike', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, sorcerer, warlock, wizard], components:[s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You extend your hand and point a finger at a target in range. Your magic grants you a brief insight into the target's defenses. On your next turn, you gain advantage on your first attack roll against the target, provided that this spell hasn't ended."], duration:"Up to 1 round", higher_level:no, level:0, material:false, range:feet(30), ritual:no, school:divination}).
spell_auto_data('unseen servant', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard, warlock, wizard], components:[v, s, m("A piece of string and a bit of wood.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["This spell creates an invisible, mindless, shapeless force that performs simple tasks at your command until the spell ends. The servant springs into existence in an unoccupied space on the ground within range. It has AC 10, 1 hit point, and a Strength of 2, and it can't attack. If it drops to 0 hit points, the spell ends.", "Once on each of your turns as a bonus action, you can mentally command the servant to move up to 15 feet and interact with an object. The servant can perform simple tasks that a human servant could do, such as fetching things, cleaning, mending, folding clothes, lighting fires, serving food, and pouring wine. Once you give the command, the servant performs the task to the best of its ability until it completes the task, then waits for your next command.", "If you command the servant to perform a task that would move it more than 60 feet away from you, the spell ends."], duration:"1 hour", higher_level:no, level:1, material:"A piece of string and a bit of wood.", range:feet(60), ritual:yes, school:conjuration}).
spell_auto_data('vampiric touch', properties{area_of_effect:false, attack_type:melee, casting_time:"1 action", classes:[warlock, wizard], components:[v, s], concentration:yes, damage_at_slot_level:_{3:damage(necrotic, 3 d 6), 4:damage(necrotic, 4 d 6), 5:damage(necrotic, 5 d 6), 6:damage(necrotic, 6 d 6), 7:damage(necrotic, 7 d 6), 8:damage(necrotic, 8 d 6), 9:damage(necrotic, 9 d 6)}, damage_with_cantrip_scaling:false, dc:false, desc:["The touch of your shadow-wreathed hand can siphon life force from others to heal your wounds. Make a melee spell attack against a creature within your reach. On a hit, the target takes 3 d 6 necrotic damage, and you regain hit points equal to half the amount of necrotic damage dealt. Until the spell ends, you can make the attack again on each of your turns as an action."], duration:"Up to 1 minute", higher_level:"When you cast this spell using a spell slot of 4th level or higher, the damage increases by 1 d 6 for each slot level above 3rd.", level:3, material:false, range:self, ritual:no, school:necromancy}).
spell_auto_data('vicious mockery', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[bard], components:[v], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:damage(psychic, 1 d 4), dc:wis else none, desc:["You unleash a string of insults laced with subtle enchantments at a creature you can see within range. If the target can hear you (though it need not understand you), it must succeed on a wisdom saving throw or take 1 d 4 psychic damage and have disadvantage on the next attack roll it makes before the end of its next turn.", "This spell's damage increases by 1 d 4 when you reach 5th level (2 d 4), 11th level (3 d 4), and 17th level (4 d 4)."], duration:"Instantaneous", higher_level:no, level:0, material:false, range:feet(60), ritual:no, school:enchantment}).
spell_auto_data('wall of fire', properties{area_of_effect:60 ft line, attack_type:false, casting_time:"1 action", classes:[druid, sorcerer, wizard], components:[v, s, m("A small piece of phosphorus.")], concentration:yes, damage_at_slot_level:_{4:damage(fire, 5 d 8)}, damage_with_cantrip_scaling:false, dc:dex else half, desc:["You create a wall of fire on a solid surface within range. You can make the wall up to 60 feet long, 20 feet high, and 1 foot thick, or a ringed wall up to 20 feet in diameter, 20 feet high, and 1 foot thick. The wall is opaque and lasts for the duration.", "When the wall appears, each creature within its area must make a Dexterity saving throw. On a failed save, a creature takes 5 d 8 fire damage, or half as much damage on a successful save.", "One side of the wall, selected by you when you cast this spell, deals 5 d 8 fire damage to each creature that ends its turn within 10 feet o f that side or inside the wall. A creature takes the same damage when it enters the wall for the first time on a turn or ends its turn there. The other side of the wall deals no damage.", "The other side of the wall deals no damage."], duration:"Up to 1 minute", higher_level:"When you cast this spell using a spell slot of 5th level or higher, the damage increases by 1 d 8 for each slot level above 4th.", level:4, material:"A small piece of phosphorus.", range:feet(120), ritual:no, school:evocation}).
spell_auto_data('wall of force', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[wizard], components:[v, s, m("A pinch of powder made by crushing a clear gemstone.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["An invisible wall of force springs into existence at a point you choose within range. The wall appears in any orientation you choose, as a horizontal or vertical barrier or at an angle. It can be free floating or resting on a solid surface. You can form it into a hemispherical dome or a sphere with a radius of up to 10 feet, or you can shape a flat surface made up of ten 10-foot-by-10-foot panels. Each panel must be contiguous with another panel. In any form, the wall is 1/4 inch thick. It lasts for the duration. If the wall cuts through a creature's space when it appears, the creature is pushed to one side of the wall (your choice which side).", "Nothing can physically pass through the wall. It is immune to all damage and can't be dispelled by dispel magic. A disintegrate spell destroys the wall instantly, however. The wall also extends into the Ethereal Plane, blocking ethereal travel through the wall."], duration:"Up to 10 minutes", higher_level:no, level:5, material:"A pinch of powder made by crushing a clear gemstone.", range:feet(120), ritual:no, school:evocation}).
spell_auto_data('wall of ice', properties{area_of_effect:10 ft sphere, attack_type:false, casting_time:"1 action", classes:[wizard], components:[v, s, m("A small piece of quartz.")], concentration:yes, damage_at_slot_level:_{6:damage(cold, 10 d 6), 7:damage(cold, 12 d 6), 8:damage(cold, 14 d 6), 9:damage(cold, 16 d 6)}, damage_with_cantrip_scaling:false, dc:dex else half, desc:["You create a wall of ice on a solid surface within range. You can form it into a hemispherical dome or a sphere with a radius of up to 10 feet, or you can shape a flat surface made up of ten 10-foot-square panels. Each panel must be contiguous with another panel. In any form, the wall is 1 foot thick and lasts for the duration.", "If the wall cuts through a creature's space when it appears, the creature within its area is pushed to one side of the wall and must make a dexterity saving throw. On a failed save, the creature takes 10 d 6 cold damage, or half as much damage on a successful save.", "The wall is an object that can be damaged and thus breached. It has AC 12 and 30 hit points per 10-foot section, and it is vulnerable to fire damage. Reducing a 10-foot section of wall to 0 hit points destroys it and leaves behind a sheet of frigid air in the space the wall occupied. A creature moving through the sheet of frigid air for the first time on a turn must make a constitution saving throw. That creature takes 5 d 6 cold damage on a failed save, or half as much damage on a successful one."], duration:"Up to 10 minutes", higher_level:"When you cast this spell using a spell slot of 7th level or higher, the damage the wall deals when it appears increases by 2 d 6, and the damage from passing through the sheet of frigid air increases by 1 d 6, for each slot level above 6th.", level:6, material:"A small piece of quartz.", range:feet(120), ritual:no, school:evocation}).
spell_auto_data('wall of stone', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[druid, sorcerer, wizard], components:[v, s, m("A small block of granite.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["A nonmagical wall of solid stone springs into existence at a point you choose within range. The wall is 6 inches thick and is composed of ten 10-foot-by-10-foot panels. Each panel must be contiguous with at least one other panel. Alternatively, you can create 10-foot-by-20-foot panels that are only 3 inches thick.", "If the wall cuts through a creature's space when it appears, the creature is pushed to one side of the wall (your choice). If a creature would be surrounded on all sides by the wall (or the wall and another solid surface), that creature can make a dexterity saving throw. On a success, it can use its reaction to move up to its speed so that it is no longer enclosed by the wall.", "The wall can have any shape you desire, though it can't occupy the same space as a creature or object. The wall doesn't need to be vertical or rest on any firm foundation. It must, however, merge with and be solidly supported by existing stone. Thus, you can use this spell to bridge a chasm or create a ramp.", "If you create a span greater than 20 feet in length, you must halve the size of each panel to create supports. You can crudely shape the wall to create crenellations, battlements, and so on.", "The wall is an object made of stone that can be damaged and thus breached. Each panel has AC 15 and 30 hit points per inch of thickness. Reducing a panel to 0 hit points destroys it and might cause connected panels to collapse at the DM's discretion.", "If you maintain your concentration on this spell for its whole duration, the wall becomes permanent and can't be dispelled. Otherwise, the wall disappears when the spell ends."], duration:"Up to 10 minutes", higher_level:no, level:5, material:"A small block of granite.", range:feet(120), ritual:no, school:evocation}).
spell_auto_data('wall of thorns', properties{area_of_effect:60 ft line, attack_type:false, casting_time:"1 action", classes:[druid], components:[v, s, m("A handful of thorns.")], concentration:yes, damage_at_slot_level:_{6:damage(piercing, 7 d 8), 7:damage(piercing, 8 d 8), 8:damage(piercing, 9 d 8), 9:damage(piercing, 10 d 8)}, damage_with_cantrip_scaling:false, dc:dex else half, desc:["You create a wall of tough, pliable, tangled brush bristling with needle-sharp thorns. The wall appears within range on a solid surface and lasts for the duration. You choose to make the wall up to 60 feet long, 10 feet high, and 5 feet thick or a circle that has a 20-foot diameter and is up to 20 feet high and 5 feet thick. The wall blocks line of sight.", "When the wall appears, each creature within its area must make a dexterity saving throw. On a failed save, a creature takes 7 d 8 piercing damage, or half as much damage on a successful save.", "A creature can move through the wall, albeit slowly and painfully. For every 1 foot a creature moves through the wall, it must spend 4 feet of movement. Furthermore, the first time a creature enters the wall on a turn or ends its turn there, the creature must make a dexterity saving throw. It takes 7 d 8 slashing damage on a failed save, or half as much damage on a successful one."], duration:"Up to 10 minutes", higher_level:"When you cast this spell using a spell slot of 7th level or higher, both types of damage increase by 1 d 8 for each slot level above 6th.", level:6, material:"A handful of thorns.", range:feet(120), ritual:no, school:conjuration}).
spell_auto_data('warding bond', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric], components:[v, s, m("A pair of platinum rings worth at least 50gp each, which you and the target must wear for the duration.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["This spell wards a willing creature you touch and creates a mystic connection between you and the target until the spell ends. While the target is within 60 feet of you, it gains a +1 bonus to AC and saving throws, and it has resistance to all damage. Also, each time it takes damage, you take the same amount of damage.", "The spell ends if you drop to 0 hit points or if you and the target become separated by more than 60 feet.", "It also ends if the spell is cast again on either of the connected creatures. You can also dismiss the spell as an action."], duration:"1 hour", higher_level:no, level:2, material:"A pair of platinum rings worth at least 50gp each, which you and the target must wear for the duration.", range:touch, ritual:no, school:abjuration}).
spell_auto_data('water breathing', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[druid, ranger, sorcerer, wizard], components:[v, s, m("A short piece of reed or straw.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["This spell gives a maximum of ten willing creatures within range and you can see, the ability to breathe underwater until the end of its term. Affected creatures also retain their normal breathing pattern."], duration:"24 hours", higher_level:no, level:3, material:"A short piece of reed or straw.", range:feet(30), ritual:yes, school:transmutation}).
spell_auto_data('water walk', properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[cleric, druid, ranger, sorcerer], components:[v, s, m("A piece of cork.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["This spell grants the ability to move across any liquid surface--such as water, acid, mud, snow, quicksand, or lava--as if it were harmless solid ground (creatures crossing molten lava can still take damage from the heat). Up to ten willing creatures you can see within range gain this ability for the duration.", "If you target a creature submerged in a liquid, the spell carries the target to the surface of the liquid at a rate of 60 feet per round."], duration:"1 hour", higher_level:no, level:3, material:"A piece of cork.", range:feet(30), ritual:yes, school:transmutation}).
spell_auto_data(web, properties{area_of_effect:20 ft cube, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v, s, m("A bit of spiderweb.")], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You conjure a mass of thick, sticky webbing at a point of your choice within range. The webs fill a 20-foot cube from that point for the duration. The webs are difficult terrain and lightly obscure their area.", "If the webs aren't anchored between two solid masses (such as walls or trees) or layered across a floor, wall, or ceiling, the conjured web collapses on itself, and the spell ends at the start of your next turn. Webs layered over a flat surface have a depth of 5 feet.", "Each creature that starts its turn in the webs or that enters them during its turn must make a dexterity saving throw. On a failed save, the creature is restrained as long as it remains in the webs or until it breaks free.", "A creature restrained by the webs can use its action to make a Strength check against your spell save DC. If it succeeds, it is no longer restrained.", "The webs are flammable. Any 5-foot cube of webs exposed to fire burns away in 1 round, dealing 2 d 4 fire damage to any creature that starts its turn in the fire."], duration:"Up to 1 hour", higher_level:no, level:2, material:"A bit of spiderweb.", range:feet(60), ritual:no, school:conjuration}).
spell_auto_data(weird, properties{area_of_effect:30 ft sphere, attack_type:false, casting_time:"1 action", classes:[wizard], components:[v, s], concentration:yes, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:wis else none, desc:["Drawing on the deepest fears of a group of creatures, you create illusory creatures in their minds, visible only to them. Each creature in a 30-foot-radius sphere centered on a point of your choice within range must make a wisdom saving throw. On a failed save, a creature becomes frightened for the duration. The illusion calls on the creature's deepest fears, manifesting its worst nightmares as an implacable threat. At the start of each of the frightened creature's turns, it must succeed on a wisdom saving throw or take 4 d 10 psychic damage. On a successful save, the spell ends for that creature."], duration:"Up to 1 minute", higher_level:no, level:9, material:false, range:feet(120), ritual:no, school:illusion}).
spell_auto_data('wind walk', properties{area_of_effect:false, attack_type:false, casting_time:"1 minute", classes:[druid], components:[v, s, m("Fire and holy water.")], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You and up to ten willing creatures you can see within range assume a gaseous form for the duration, appearing as wisps of cloud. While in this cloud form, a creature has a flying speed of 300 feet and has resistance to damage from nonmagical weapons. The only actions a creature can take in this form are the Dash action or to revert to its normal form. Reverting takes 1 minute, during which time a creature is incapacitated and can't move. Until the spell ends, a creature can revert to cloud form, which also requires the 1-minute transformation.", "If a creature is in cloud form and flying when the effect ends, the creature descends 60 feet per round for 1 minute until it lands, which it does safely. If it can't land after 1 minute, the creature falls the remaining distance."], duration:"8 hours", higher_level:no, level:6, material:"Fire and holy water.", range:feet(30), ritual:no, school:transmutation}).
spell_auto_data('wind wall', properties{area_of_effect:50 ft line, attack_type:false, casting_time:"1 action", classes:[druid, ranger], components:[v, s, m("A tiny fan and a feather of exotic origin.")], concentration:yes, damage_at_slot_level:_{3:damage(bludgeoning, 3 d 8)}, damage_with_cantrip_scaling:false, dc:str else half, desc:["A wall of strong wind rises from the ground at a point you choose within range. You can make the wall up to 50 feet long, 15 feet high, and 1 foot thick. You can shape the wall in any way you choose so long as it makes one continuous path along the ground. The wall lasts for the duration.", "When the wall appears, each creature within its area must make a strength saving throw. A creature takes 3 d 8 bludgeoning damage on a failed save, or half as much damage on a successful one.", "The strong wind keeps fog, smoke, and other gases at bay. Small or smaller flying creatures or objects can't pass through the wall. Loose, lightweight materials brought into the wall fly upward. Arrows, bolts, and other ordinary projectiles launched at targets behind the wall are deflected upward and automatically miss. (Boulders hurled by giants or siege engines, and similar projectiles, are unaffected.) Creatures in gaseous form can't pass through it."], duration:"Up to 1 minute", higher_level:no, level:3, material:"A tiny fan and a feather of exotic origin.", range:feet(120), ritual:no, school:evocation}).
spell_auto_data(wish, properties{area_of_effect:false, attack_type:false, casting_time:"1 action", classes:[sorcerer, wizard], components:[v], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["Wish is the mightiest spell a mortal creature can cast. By simply speaking aloud, you can alter the very foundations of reality in accord with your desires.", "The basic use of this spell is to duplicate any other spell of 8th level or lower. You don't need to meet any requirements in that spell, including costly components. The spell simply takes effect.", "Alternatively, you can create one of the following effects of your choice:", "- You create one object of up to 25,000 gp in value that isn't a magic item. The object can be no more than 300 feet in any dimension, and it appears in an unoccupied space you can see on the ground.", "- You allow up to twenty creatures that you can see to regain all hit points, and you end all effects on them described in the greater restoration spell.", "- You grant up to ten creatures that you can see resistance to a damage type you choose.", "- You grant up to ten creatures you can see immunity to a single spell or other magical effect for 8 hours. For instance, you could make yourself and all your companions immune to a lich's life drain attack.", "- You undo a single recent event by forcing a reroll of any roll made within the last round (including your last turn). Reality reshapes itself to accommodate the new result. For example, a wish spell could undo an opponent's successful save, a foe's critical hit, or a friend's failed save. You can force the reroll to be made with advantage or disadvantage, and you can choose whether to use the reroll or the original roll.", "You might be able to achieve something beyond the scope of the above examples. State your wish to the GM as precisely as possible. The GM has great latitude in ruling what occurs in such an instance; the greater the wish, the greater the likelihood that something goes wrong. This spell might simply fail, the effect you desire might only be partly achieved, or you might suffer some unforeseen consequence as a result of how you worded the wish. For example, wishing that a villain were dead might propel you forward in time to a period when that villain is no longer alive, effectively removing you from the game. Similarly, wishing for a legendary magic item or artifact might instantly transport you to the presence of the item's current owner.", "The stress of casting this spell to produce any effect other than duplicating another spell weakens you. After enduring that stress, each time you cast a spell until you finish a long rest, you take 1 d 10 necrotic damage per level of that spell. This damage can't be reduced or prevented in any way. In addition, your Strength drops to 3, if it isn't 3 or lower already, for 2 d 4 days. For each of those days that you spend resting and doing nothing more than light activity, your remaining recovery time decreases by 2 days. Finally, there is a 33 percent chance that you are unable to cast wish ever again if you suffer this stress."], duration:"Instantaneous", higher_level:no, level:9, material:false, range:self, ritual:no, school:conjuration}).
spell_auto_data('word of recall', properties{area_of_effect:5 ft sphere, attack_type:false, casting_time:"1 action", classes:[cleric], components:[v], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You and up to five willing creatures within 5 feet of you instantly teleport to a previously designated sanctuary. You and any creatures that teleport with you appear in the nearest unoccupied space to the spot you designated when you prepared your sanctuary (see below). If you cast this spell without first preparing a sanctuary, the spell has no effect.", "You must designate a sanctuary by casting this spell within a location, such as a temple, dedicated to or strongly linked to your deity. If you attempt to cast the spell in this manner in an area that isn't dedicated to your deity, the spell has no effect."], duration:"Instantaneous", higher_level:no, level:6, material:false, range:feet(5), ritual:no, school:conjuration}).
spell_auto_data('zone of truth', properties{area_of_effect:15 ft sphere, attack_type:false, casting_time:"1 action", classes:[bard, cleric, paladin], components:[v, s], concentration:no, damage_at_slot_level:[], damage_with_cantrip_scaling:false, dc:false, desc:["You create a magical zone that guards against deception in a 15-foot-radius sphere centered on a point of your choice within range. Until the spell ends, a creature that enters the spell's area for the first time on a turn or starts its turn there must make a Charisma saving throw. On a failed save, a creature can't speak a deliberate lie while in the radius. You know whether each creature succeeds or fails on its saving throw.", "An affected creature is aware of the spell and can thus avoid answering questions to which it would normally respond with a lie. Such a creature can remain evasive in its answers as long as it remains within the boundaries of the truth."], duration:"10 minutes", higher_level:no, level:2, material:false, range:feet(60), ritual:no, school:enchantment}).
dragon_element(black, acid).
dragon_element(blue, lightning).
dragon_element(brass, fire).
dragon_element(bronze, lightning).
dragon_element(copper, acid).
dragon_element(gold, fire).
dragon_element(green, poison).
dragon_element(red, fire).
dragon_element(silver, cold).
dragon_element(white, cold).

dragon_color(Color) :- dragon_element(Color, _).
% :- table ability/2 as incremental.

update_base_ability(Abi, NewScore) :-
    retract(base_ability(Abi, _)),
    assert(base_ability(Abi, NewScore)).

ability(str).
ability(dex).
ability(con).
ability(wis).
ability(int).
ability(cha).

highest_ability_from(List, Abi, Val) :-
    maplist([A,A-X]>>ability(A,X), List, WithScores),
    sort(2, @>=, WithScores, [Abi-Val|_]).
highest_ability_mod_from(List, Abi, Mod) :-
    highest_ability_from(List, Abi, _),
    ability_mod(Abi, Mod).

ability_max(Ability, Max) :-
    ability(Ability),
    sum_bonuses(max_ability(Ability), Bonus),
    Max is 20 + Bonus.

ability_plus_n(N, Ability+N) :-
    ability(Ability).

%:- table
%   naked_lvl1_ability/2,
%   ability_after_levelup_asis/3,
%   ability_after_feats/2.

%! ability(?Ability:atomic, ?Score:int)
%
%  Your character's ability Score for the given Ability.
ability(Ability, Score) :-
    ability_after_feats(Ability, Score).

%! ability_mod(?Ability:atomic, ?Mod:int)
%
%  Your character's ability modifier (Mod) for the given Ability.
ability_mod(Abil, Mod) :-
    ability(Abil, Val),
    mf(Val, Mod).
mf(Val, Mod) :-
    floor( (Val-10) / 2, Mod ).

%! add_ability_mod_and_profbon(Val, Abi, Sum)
%
%  Convenience predicate that adds the ability modifier for Abi and
%  the proficiency bonus to some value Val, producing Sum.
add_ability_mod_and_profbon(Val, Abi, Sum) :-
    ability_mod(Abi, Mod),
    proficiency_bonus(ProfBon),
    Sum is Val + Mod + ProfBon.

%! initial_ability(?Ability:atomic, ?Score:int)
%
%  Your character's ability Score for the given Ability, at level one,
%  not counting any bonuses apart from racial bonuses.
initial_ability(Ability, Score) :-
    base_ability(Ability, Base),
    total_racial_ability_bonus(Ability, Bonus),
    Score is Base + Bonus.

%! ability_after_feats(?Ability:atomic, ?Score:int)
%
%  Your character's ability Score for the given Ability, after
%  counting bonuses from race, ability score increases, and feats.
ability_after_feats(Ability, Score) :-
    ability_after_levelup_asis(Ability, AfterAsis),
    total_other_ability_bonus(Ability, Bonus),
    ability_max(Ability, Max),
    Score is min(AfterAsis+Bonus, Max).

%! ability_after_levelup_asis(?Ability:atomic, ?Score:int)
%
%  Your character's ability Score for the given Ability, after
%  counting racial bonuses and ability score increases, but not
%  counting any other bonuses.
ability_after_levelup_asis(Ability, Score) :-
    initial_ability(Ability, Initial),
    total_asi_ability_bonus(Ability, Bonus),
    Score is Initial + Bonus.
problem(ability_score_exceeds_maximum(Ability, Score)) :-
    % Flag a problem if the asis make you go over the max.
    % TODO: hide asi options that go over the maximum
    ability_after_levelup_asis(Ability, Score),
    ability_max(Ability, Max),
    Score > Max.

total_racial_ability_bonus(Ability, Total) :-
    ability(Ability), % ground
    findall(Bon, (bonus(Origin, Ability + Bon), origin_category(race(_), Origin)), Bonuses),
    sumlist(Bonuses, Total).
total_asi_ability_bonus(Ability, Total) :-
    ability(Ability),
    findall(Bon,
            (bonus(Origin, Ability+Bon), Origin = choice(_, 'asi or feat')),
              % Omitting the intermediate Origin variable causes a warning in swipl which,
              % I think, is a bug in swipl.
            Bonuses),
    sumlist(Bonuses, Total).
total_other_ability_bonus(Ability, Total) :-
    ability(Ability),
    findall(Bon,
            (bonus(Origin, Ability+Bon),
             \+ origin_category(race(_), Origin),
             Origin \= choice(_, 'asi or feat')),
            Bonuses),
    sumlist(Bonuses, Total).

%! saving_throw(?Ability:atomic, ?Bonus:int)
saving_throw(Ability, Bonus) :-
    ability_mod(Ability, Mod),
    proficiency_bonus(ProfBon),
    (trait(saving_throw(Ability))
     -> (!, Bonus is Mod + ProfBon)
     ;  (!, Bonus = Mod)
    ).
saving_throw(Ability, Bonus) :-
    % Make this predicate not fail if we don't have an initial class yet.
    ability_mod(Ability, Bonus),
    \+ initial_class(_).

%asi(N, Abi+N) :- ability(Abi).
%asi(N, Asis) :-
%    ground(Asis),
%    !,
%    maplist([Abi+N,N]>>ability(Abi), Asis, Ns),
%    sumlist(Ns, N).
%asi(N, Asis) :-
%    split_nat(N, Ns),
%    maplist([M,Abi+M]>>ability(Abi), Ns, Asis).
%split_nat(0, []) :- !.
%split_nat(N, [M|Ns]) :-
%    between(1, N, M),
%    N1 is N-M,
%    split_nat(N1, Ns).

problem(ability_score_missing(Abi)) :-
    ability(Abi),
    \+ ability(Abi, _).
%! skill(?Skill:atomic, ?Score:int)
%
%  Score is the bonus you get on rolls for Skill.
skill(Skill, Score) :-
    skill_ability(Skill, Ability),
    ability_mod(Ability, Mod),
    skill_proficiency_bonus(Skill, Bonus),
    Score is Mod + Bonus.

%! skill(?Skill:atomic)
skill(Skill) :- skill_ability(Skill, _).

%! skill_proficiency_bonus(?Skill:atomic, -Bonus:int)
skill_proficiency_bonus(Skill, Bonus) :-
    proficient_at_skill(Skill),
    level(Level),
    calc_bonus(Level, Bonus1),
    (trait(expertise(skill(Skill))) -> Bonus is 2*Bonus1 ; Bonus = Bonus1).
skill_proficiency_bonus(Skill, 0) :-
    \+ trait(skill(Skill)).

%! skill_ability(?Skill:atomic, ?Ability:atomic)
%
%  The Ability that covers Skill.
skill_ability(athletics         , str).
skill_ability(acrobatics        , dex).
skill_ability('sleight of hand' , dex).
skill_ability(stealth           , dex).
skill_ability(arcana            , int).
skill_ability(history           , int).
skill_ability(investigation     , int).
skill_ability(nature            , int).
skill_ability(religion          , int).
skill_ability('animal handling' , wis).
skill_ability(insight           , wis).
skill_ability(medicine          , wis).
skill_ability(perception        , wis).
skill_ability(survival          , wis).
skill_ability(deception         , cha).
skill_ability(intimidation      , cha).
skill_ability(performance       , cha).
skill_ability(persuasion        , cha).

%! proficient_at_skill(?Skill:atomic)
%
%  Checks whether character is proficient at given Skill.
%  When queried with a variable, each skill will only show up once.
proficient_at_skill(Skill) :-
    skill(Skill),
    proficient_at_skill_(Skill).
proficient_at_skill_(Skill) :-
    trait(skill(Skill)),
    !.

% The program should be able to deal with the same skill being
% selected more than once. Nevertheless, if the character has the same
% skill more than once and at least one of the times it is the result
% of a choice, we flag it as a problem.
problem(selected_same_skill_more_than_once(Skill)) :-
    Origin1 = choice(_,_),
    trait(Origin1, skill(Skill)),
    trait(Origin2, skill(Skill)),
    Origin1 \= Origin2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

skill(athletics) ?= ["Your Strength (Athletics) check covers difficult situations you encounter while climbing, jumping, or swimming. Examples include the following activities:" ,
    "You attempt to climb a sheer or slippery cliff, avoid hazards while scaling a wall, or cling to a surface while something is trying to knock you off.",
    "You try to jump an unusually long distance or pull off a stunt midjump.",
    "You struggle to swim or stay afloat in treacherous currents, storm-tossed waves, or areas of thick seaweed. Or another creature tries to push or pull you underwater or otherwise interfere with your swimming."].


skill(acrobatics) ?= ["Your Dexterity (Acrobatics) check covers your attempt to stay on your feet in a tricky situation, such as when you're trying to run across a sheet of ice, balance on a tightrope, or stay upright on a rocking ship's deck. The GM might also call for a Dexterity (Acrobatics) check to see if you can perform acrobatic stunts, including dives, rolls, somersaults, and flips."].

'sleight of hand' ?= ["Whenever you attempt an act of legerdemain or manual trickery, such as planting something on someone else or concealing an object on your person, make a Dexterity (Sleight of Hand) check. The GM might also call for a Dexterity (Sleight of Hand) check to determine whether you can lift a coin purse off another person or slip something out of another person's pocket."].

skill(stealth) ?= ["Make a Dexterity (Stealth) check when you attempt to conceal yourself from enemies, slink past guards, slip away without being noticed, or sneak up on someone without being seen or heard."].

skill(arcana) ?= ["Your Intelligence (Arcana) check measures your ability to recall lore about spells, magic items, eldritch symbols, magical traditions, the planes of existence, and the inhabitants of those planes."].

skill(history) ?= ["Your Intelligence (History) check measures your ability to recall lore about historical events, legendary people, ancient kingdoms, past disputes, recent wars, and lost civilizations."].

skill(investigation) ?= ["When you look around for clues and make deductions based on those clues, you make an Intelligence (Investigation) check. You might deduce the location of a hidden object, discern from the appearance of a wound what kind of weapon dealt it, or determine the weakest point in a tunnel that could cause it to collapse. Poring through ancient scrolls in search of a hidden fragment of knowledge might also call for an Intelligence (Investigation) check."].

skill(nature) ?= ["Your Intelligence (Nature) check measures your ability to recall lore about terrain, plants and animals, the weather, and natural cycles."].

skill(religion) ?= ["Your Intelligence (Religion) check measures your ability to recall lore about deities, rites and prayers, religious hierarchies, holy symbols, and the practices of secret cults."].

skill('animal handling') ?= ["When there is any question whether you can calm down a domesticated animal, keep a mount from getting spooked, or intuit an animal’s intentions, the GM might call for a Wisdom (Animal Handling) check. You also make a Wisdom (Animal Handling) check to control your mount when you attempt a risky maneuver."].

skill(insight) ?= ["Your Wisdom (Insight) check decides whether you can determine the true intentions of a creature, such as when searching out a lie or predicting someone’s next move. Doing so involves gleaning clues from body language, speech habits, and changes in mannerisms."].

skill(medicine) ?= ["A Wisdom (Medicine) check lets you try to stabilize a dying companion or diagnose an illness."].

skill(perception) ?= ["Your Wisdom (Perception) check lets you spot, hear, or otherwise detect the presence of something. It measures your general awareness of your surroundings and the keenness of your senses. For example, you might try to hear a conversation through a closed door, eavesdrop under an open window, or hear monsters moving stealthily in the forest. Or you might try to spot things that are obscured or easy to miss, whether they are orcs lying in ambush on a road, thugs hiding in the shadows of an alley, or candlelight under a closed secret door."].

skill(survival) ?= ["The GM might ask you to make a Wisdom (Survival) check to follow tracks, hunt wild game, guide your group through frozen wastelands, identify signs that owlbears live nearby, predict the weather, or avoid quicksand and other natural hazards."].

skill(deception) ?= [
    "Your Charisma (Deception) check determines whether you can convincingly hide the truth, either verbally or through your actions. This deception can encompass everything from misleading others through ambiguity to telling outright lies. Typical situations include trying to fast-talk a guard, con a merchant, earn money through gambling, pass yourself off in a disguise, dull someone's suspicions with false assurances, or maintain a straight face while telling a blatant lie."
].

skill(intimidation) ?= ["When you attempt to influence someone through overt threats, hostile actions, and physical violence, the GM might ask you to make a Charisma (Intimidation) check. Examples include trying to pry information out of a prisoner, convincing street thugs to back down from a confrontation, or using the edge of a broken bottle to convince a sneering vizier to reconsider a decision."].

skill(performance) ?= ["Your Charisma (Performance) check determines how well you can delight an audience with music, dance, acting, storytelling, or some other form of entertainment."].

skill(persuasion) ?= ["When you attempt to influence someone or a group of people with tact, social graces, or good nature, the GM might ask you to make a Charisma (Persuasion) check. Typically, you use persuasion when acting in good faith, to foster friendships, make cordial requests, or exhibit proper etiquette. Examples of persuading others include convincing a chamberlain to let your party see the king, negotiating peace between warring tribes, or inspiring a crowd of townsfolk."].
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
trait_source(wizard >: 1, ritual_casting(wizard)).
trait_source(wizard >: 1, arcane_recovery(N)) :-
    class_level(wizard:L),
    N is ceil(L/2).
on_rest(long, 'arcane recovery', 'full restore') :-
    trait('arcane recovery').

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
    member(ChoiceID, [spell, 'free spell']),
    choice_member(Origin, ChoiceID, Name),
    wizard_spell_resources(Name, Resources),
    wizard_spell_availability(Name, Availability),
    spell_property(Name, ritual, Ritual).

hide_known_class_spells(wizard >: _, cantrip, wizard).
hide_known_class_spells(wizard >: _, 'free spell', wizard).

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
    
trait_source(wizard(evocation) >: 10, 'empowered evocation').
bonus_source(trait('empowered evocation'),
             modify_spell(wizard, Spell, Goal)) :-
    known_spell(wizard, Spell),
    spell_data(Spell, Data),
    Data.school = evocation,
    subterm_member(damage(_,_), Data.effects), 
    Goal = modify_spell_field(effects, apply_empowered_evocation).
apply_empowered_evocation(OldEffects, NewEffects) :-
    \+ contains_multiple_damage_rolls(OldEffects),
    ability_mod(int, Bonus),
    select_subterm(damage(Element, Dice), OldEffects,
                   damage(Element, NewDice), NewEffects),
    simplify_dice_sum(Dice + Bonus, NewDice).
apply_empowered_evocation(OldEffects, NewEffects) :-
    contains_multiple_damage_rolls(OldEffects),
    ability_mod(int, Bonus),
    atomics_to_string(["add +", Bonus, " to one damage roll"], New),
    append(OldEffects, [New], NewEffects).

custom_format(modify_spell_field(effects, apply_empowered_evocation)) -->
    {ability_mod(int, Mod)}, ["+"], [Mod], [" to one damage roll"].

trait_source(wizard(evocation) >: 14, overchannel).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ritual_casting(wizard) ?= "You can cast a wizard spell as a ritual if that spell has the ritual tag and you have the spell in your spellbook. You don't need to have the spell prepared.".

arcane_recovery(_) ?= "You have learned to regain some of your magical energy by studying your spellbook. Once per day when you finish a short rest, you can choose expended spell slots to recover. The spell slots can have a combined level that is equal to or less than half your wizard level (rounded up), and none of the slots can be 6th level or higher.
For example, if you're a 4th-level wizard, you can recover up to two levels worth of spell slots. You can recover either a 2nd-level spell slot or two 1st-level spell slots.".

spellbook ?= "The spells that you add to your spellbook as you gain levels reflect the arcane research you conduct on your own, as well as intellectual breakthroughs you have had about the nature of the multiverse. You might find other spells during your adventures. You could discover a spell recorded on a scroll in an evil wizard's chest, for example, or in a dusty tome in an ancient library.
Copying a Spell into the Book. When you find a wizard spell of 1st level or higher, you can add it to your spellbook if it is of a spell level you can prepare and if you can spare the time to decipher and copy it. Copying that spell into your spellbook involves reproducing the basic form of the spell, then deciphering the unique system of notation used by the wizard who wrote it. You must practice the spell until you understand the sounds or gestures required, then transcribe it into your spellbook using your own notation.
For each level of the spell, the process takes 2 hours and costs 50 gp. The cost represents material components you expend as you experiment with the spell to master it, as well as the fine inks you need to record it. Once you have spent this time and money, you can prepare the spell just like your other spells.
Replacing the Book. You can copy a spell from your own spellbook into another book - for example, if you want to make a backup copy of your spellbook. This is just like copying a new spell into your spellbook, but faster and easier, since you understand your own notation and already know how to cast the spell. You need spend only 1 hour and 10 gp for each level of the copied spell.
If you lose your spellbook, you can use the same procedure to transcribe the spells that you have prepared into a new spellbook. Filling out the remainder of your spellbook requires you to find new spells to do so, as normal. For this reason, many wizards keep backup spellbooks in a safe place. The Book's Appearance. Your spellbook is a unique compilation of spells, with its own decorative flourishes and margin notes. It might be a plain, functional leather volume that you received as a gift from your master, a finely bound gilt-edged tome you found in an ancient library, or even a loose collection of notes scrounged together after you lost your previous spellbook in a mishap.".

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
class_option(barbarian).
hd_per_level(barbarian, 1 d 12).
initial_class_base_hp(barbarian, 12).
max_hp_per_level(barbarian, 1 d 12).
caster(barbarian, 0).
choose_subclass_level(barbarian:3).
class_saving_throw(barbarian, str).
class_saving_throw(barbarian, con).
asi_level(barbarian:L) :- default_asi_level(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initial class features (you don't get these when multiclassing into barbarian).
class_skill_list(barbarian,
                 ['animal handling', athletics, intimidation,
                   nature, perception, survival]).

traits_from_source(^barbarian, [armor(light), armor(medium)]).
trait_options_source(^barbarian, skill, wrap(skill),
                     2 unique_from class_skill(barbarian)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 1 features (you get these when multiclassing into barbarian).
traits_from_source(barbarian >: 1,
                   [armor(shield), weapon(simple), weapon(martial)]).

trait_source(barbarian >: 1, rage(damage + N)) :-
    class_level(barbarian:L),
    ordered_lookup_largest_leq([1 -> 2, 9 -> 3, 16 -> 4], L, N).
resource(rage, rage, Max) :-
    class_level(barbarian:L),
    ordered_lookup_largest_leq([1 -> 2,
                                3 -> 3,
                                6 -> 4,
                                12 -> 5,
                                17 -> 6,
                                20 -> unlimited], L, Max).
meta_todo(barbarian, "how to handle 'unlimited' rages in UI").
on_rest(long, rage, 'full restore') :- trait(rage).

trait_source(barbarian >: 1, unarmored_defense(10 + dex + con + shield)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features gained from leveling up.
trait_source(barbarian >: 2, 'reckless attack').
trait_source(barbarian >: 2, 'danger sense').
multiclass_trait_source(barbarian >: 5, extra_attack(1)).
trait_source(barbarian >: 5, 'fast movement').
bonus_source(trait('fast movement'), speed + 10).
trait_source(barbarian >: 7, 'feral instinct').
trait_source(barbarian >: 9, brutal_critical(N)) :-
    class_level(barbarian:L),
    ordered_lookup_largest_leq([9->1, 13->2, 17->3], L, N).
trait_source(barbarian >: 11, 'relentless rage').
trait_source(barbarian >: 15, 'persistent rage').
trait_source(barbarian >: 18, 'indomitable might').
trait_source(barbarian >: 20, 'primal champion').
bonus_source(trait('primal champion'), str + 4).
bonus_source(trait('primal champion'), con + 4).
bonus_source(trait('primal champion'), max_ability(str) + 4).
bonus_source(trait('primal champion'), max_ability(con) + 4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUBCLASSES                                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subclass_option(barbarian, berserker).
trait_source(barbarian(berserker) >: 3, frenzy).
trait_source(barbarian(berserker) >: 6, 'mindless rage').
trait_source(barbarian(berserker) >: 10, 'intimidating presence').
trait_source(barbarian(berserker) >: 14, retaliation).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DESCRIPTIONS                                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rage(_) ?= "In battle, you fight with primal ferocity. On your turn, you can enter a rage as a bonus action.

While raging, you gain the following benefits if you aren't wearing heavy armor:

    You have advantage on Strength checks and Strength saving throws.
    When you make a melee weapon attack using Strength, you gain a bonus to the damage roll that increases as you gain levels as a barbarian, as shown in the Rage Damage column of the Barbarian table.
    You have resistance to bludgeoning, piercing, and slashing damage.

If you are able to cast spells, you can't cast them or concentrate on them while raging.

Your rage lasts for 1 minute. It ends early if you are knocked unconscious or if your turn ends and you haven't attacked a hostile creature since your last turn or taken damage since then. You can also end your rage on your turn as a bonus action.

Once you have raged the number of times shown for your barbarian level in the Rages column of the Barbarian table, you must finish a long rest before you can rage again.".

'unarmored defense' ?= "While you are not wearing any armor, your Armor Class equals 10 + your Dexterity modifier + your Constitution modifier. You can use a shield and still gain this benefit.".

'reckless attack' ?= "Starting at 2nd level, you can throw aside all concern for defense to attack with fierce desperation. When you make your first attack on your turn, you can decide to attack recklessly. Doing so gives you advantage on melee weapon attack rolls using Strength during this turn, but attack rolls against you have advantage until your next turn.".

'danger sense' ?= "At 2nd level, you gain an uncanny sense of when things nearby aren't as they should be, giving you an edge when you dodge away from danger. You have advantage on Dexterity saving throws against effects that you can see, such as traps and spells. To gain this benefit, you can’t be blinded, deafened, or incapacitated.".

'fast movement' ?= "Starting at 5th level, your speed increases by 10 feet while you aren’t wearing heavy armor.".

'feral instinct' ?= "By 7th level, your instincts are so honed that you have advantage on initiative rolls.

Additionally, if you are surprised at the beginning of combat and aren't incapacitated, you can act normally on your first turn, but only if you enter your rage before doing anything else on that turn.".

'brutal critical' ?= "Beginning at 9th level, you can roll one additional weapon damage die when determining the extra damage for a critical hit with a melee attack. This increases to two additional dice at 13th level and three additional dice at 17th level.".

'relentless rage' ?= "Starting at 11th level, your rage can keep you fighting despite grievous wounds. If you drop to 0 hit points while you're raging and don't die outright, you can make a DC 10 Constitution saving throw. If you succeed, you drop to 1 hit point instead.

Each time you use this feature after the first, the DC increases by 5. When you finish a short or long rest, the DC resets to 10.".

'persistent rage' ?= "Beginning at 15th level, your rage is so fierce that it ends early only if you fall unconscious or if you choose to end it.".

'indomitable might' ?= "Beginning at 18th level, if your total for a Strength check is less than your Strength score, you can use that score in place of the total.".

'primal champion' ?= "At 20th level, you embody the power of the wilds. Your Strength and Constitution scores increase by 4. Your maximum for those scores is now 24.".

frenzy ?= "Starting when you choose this path at 3rd level, you can go into a frenzy when you rage. If you do so, for the duration of your rage you can make a single melee weapon attack as a bonus action on each of your turns after this one. When your rage ends, you suffer one level of exhaustion (as described in appendix A).".

'mindless rage' ?= "Beginning at 6th level, you can't be charmed or frightened while raging. If you are charmed or frightened when you enter your rage, the effect is suspended for the duration of the rage.".

'intimidating presence' ?= "Beginning at 10th level, you can use your action to frighten someone with your menacing presence. When you do so, choose one creature that you can see within 30 feet of you. If the creature can see or hear you, it must succeed on a Wisdom saving throw (DC equal to 8 + your proficiency bonus + your Charisma modifier) or be frightened of you until the end of your next turn. On subsequent turns, you can use your action to extend the duration of this effect on the frightened creature until the end of your next turn. This effect ends if the creature ends its turn out of line of sight or more than 60 feet away from you.

If the creature succeeds on its saving throw, you can't use this feature on that creature again for 24 hours. ".

retaliation ?= "Starting at 14th level, when you take damage from a creature that is within 5 feet of you, you can use your reaction to make a melee weapon attack against that creature.".
class_option(paladin).
hd_per_level(paladin, 1 d 10).
initial_class_base_hp(paladin, 10).
max_hp_per_level(paladin, 1 d 10).
caster(paladin, 1/2).
spellcasting_ability(paladin, cha).
max_prepared_spells(paladin, N) :-
    default_max_prepared_spells(paladin, N).
choose_subclass_level(paladin:3).
asi_level(paladin:L) :-
    default_asi_level(L).
class_saving_throw(paladin, wis).
class_saving_throw(paladin, cha).

class_skill_list(paladin, [athletics, insight, intimidation,
                           medicine, persuasion, religion]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initial class features (don't get these multiclassing into paladin).
trait_source(^paladin, armor(heavy)).
trait_options_source(^paladin, skill, wrap(skill),
                     2 unique_from class_skill(paladin)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 1 features (you get these when multiclassing into paladin).
traits_from_source(paladin >: 1,
                   [weapon(simple), weapon(martial),
                    armor(light), armor(medium), armor(shield)]).

% Divine sense.
trait_source(paladin >: 1, 'divine sense').
resource('divine sense', 'divine sense', N) :-
    trait('divine sense'), ability_mod(cha, Mod), N is max(0, Mod+1).
on_rest(long, 'divine sense', 'full restore').

% Lay on hands.
trait_source(paladin >: 1, 'lay on hands').
resource('lay on hands', 'hit points', HP) :-
    class_level(paladin:L), HP is 5*L.
on_rest(long, 'lay on hands', 'full restore').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features gained from leveling up.
trait_options_source(paladin >: 2, 'fighting style',
                     wrap(fighting_style),
                     from_list([defense,dueling,'great weapon fighting',
                                protection])).

trait_source(paladin >: 2, 'divine smite').
trait_source(paladin >: 3, 'divine health').

trait_source(paladin >: 3, 'channel divinity').
bonus_source(paladin >: 3, channel_divinity_uses(1)).

trait_source(paladin >: 5, 'extra attack').
trait_source(paladin >: 6, 'aura of protection').
trait_source(paladin >: 9, 'aura of courage').

trait_source(paladin >: 11, 'improved divine smite').
bonus_source(trait('improved divine smite'),
             extra_damage_roll(BaseWeapon, damage(radiant, 1 d 8))) :-
    weapon(BaseWeapon, _, melee, _, _).

trait_source(paladin >: 13, 'cleansing touch').
resource('cleansing touch', 'cleansing touch', N) :-
    trait('cleansing touch'),
    ability_mod(cha, Mod), N is max(1, Mod).
on_rest(long, 'cleansing touch', 'full restore').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Spellcasting.

trait_source(paladin >: 1, spellcasting_focus('holy symbol')).

% Paladins know all proper spells on their spell list.
known_spell(paladin, cha, 'when prepared', [slot], Ritual, Spell) :-
    learnable_proper_spell(paladin, Spell),
    spell_property(Spell, ritual, Ritual).

% Oath spells are always prepared.
known_spell(paladin(Oath), cha, always, [slot], Ritual, Spell) :-
    subclass(paladin(Oath)),
    paladin_oath_spell(Oath, Spell),
    learnable_proper_spell(paladin, Spell),
    spell_property(Spell, ritual, Ritual).

% Add oath spells to the paladin spell list.
extend_class_spell_list(paladin, Spell) :-
    subclass(paladin(Oath)),
    paladin_oath_spell(Oath, Spell).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OATHS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Oath of Devotion
subclass_option(paladin, devotion).

paladin_oath_spell(devotion, 'protection from evil and good').
paladin_oath_spell(devotion, sanctuary).
paladin_oath_spell(devotion, 'lesser restoration').
paladin_oath_spell(devotion, 'zone of truth').
paladin_oath_spell(devotion, 'beacon of hope').
paladin_oath_spell(devotion, 'dispel magic').
paladin_oath_spell(devotion, 'freedom of movement').
paladin_oath_spell(devotion, 'guardian of faith').
paladin_oath_spell(devotion, commune).
paladin_oath_spell(devotion, 'flame strike').

trait_source(paladin(devotion) >: 3, channel_divinity('sacred weapon')).
trait_source(paladin(devotion) >: 3, channel_divinity('turn the unholy')).
trait_source(paladin(devotion) >: 7, aura_of_devotion(feet(Dst))) :-
    (paladin(devotion) >: 18) -> (Dst = 30) ; (Dst = 10).
trait_source(paladin(devotion) >: 15, 'purity of spirit').
trait_source(paladin(devotion) >: 20, 'holy nimbus').
resource('holy nimbus', 'holy nimbus', 1) :-
    trait('holy nimbus').
on_rest(long, 'holy nimbus', 'full restore').
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

resource('pact magic', pact_slot(L), N) :-
    pact_magic_slots(N),
    pact_magic_slot_level(L).
on_rest(short, pact_slot(L), 'full restore') :-
    pact_magic_slot_level(L).

% Learning spells.
known_spell(warlock, cha, always, [], no, Name) :-
    class_origin_to_class(Origin, warlock),
    choice_member(Origin, cantrip, Name).
known_spell(warlock, cha, always, ['pact slot'], Ritual, Name) :-
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
trait(warlock >: Level, eldritch_invocation(Inv)) :-
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
    append(L, ["on hit, push up to 10 ft away"], E2).
custom_format(modify_spell_field(effects, apply_repelling_blast)) -->
    ["on hit, push target up to 10 ft away"].

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


eldritch_invocation_option('voice of the chain master').
% TODO

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
resource('dark one\'s own luck', 'dark one\'s own luck', 1) :-
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
class_option(rogue).
hd_per_level(rogue, 1 d 8).
initial_class_base_hp(rogue, 8).
max_hp_per_level(rogue, 1 d 8).
choose_subclass_level(rogue:3).
asi_level(rogue:L) :-
    default_asi_level(L).
class_saving_throw(rogue, dex).
class_saving_throw(rogue, int).

class_skill_list(rogue, [acrobatics, athletics, deception, insight,
                         intimidation, investigation, perception,
                         performance, persuasion,
                         'sleight of hand', stealth]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initial class features (you don't get these when multiclassing into rogue).
traits_from_source(^rogue,
                   [weapon(simple), weapon('hand crossbow'),
                    weapon(longsword), weapon(rapier),
                    weapon(shortsword)]).
trait_options_source(^rogue, skill, wrap(skill),
                     4 unique_from class_skill(rogue)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 1 features (you get these when multiclassing into rogue).
traits_from_source(rogue >: 1, [armor(light), tool('thieves\' tools')]).
trait_options_source(rogue >: 1, skill, wrap(skill), class_skill(rogue)) :-
    \+ (^rogue).

trait_options_source(rogue >: L, expertise, rogue_expertise_to_trait,
                     2 unique_from rogue_expertise_option) :-
    L = 1 ; L = 6.
rogue_expertise_option('thieves\' tools').
rogue_expertise_option(Skill) :- trait(skill(Skill)).
rogue_expertise_to_trait('thieves\' tools', expertise(tool('thieves\' tools'))).
rogue_expertise_to_trait(Skill, expertise(skill(Skill))) :- skill(Skill).

trait_source(rogue >: 1, sneak_attack(N d 6)) :-
    class_level(rogue:L),
    N is ceil(L/2).

trait_source(rogue >: 1, 'thieves\' cant').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features from leveling up.
trait_source(rogue >: 2, 'cunning action').
trait_source(rogue >: 5, 'uncanny dodge').
trait_source(rogue >: 7, evasion).
trait_source(rogue >: 11, 'reliable talent').
trait_source(rogue >: 14, sense(blindsense)).
trait_source(rogue >: 15, 'slippery mind').
trait_source(rogue >: 18, elusive).
trait_source(rogue >: 20, 'stroke of luck').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Roguish archetype: arcane trickster.
subclass_option(rogue, 'arcane trickster').
caster(rogue, 1/3) :- subclass(rogue('arcane trickster')).
spellcasting_ability(rogue, int) :- subclass(rogue('arcane trickster')).

% Mage hand legerdemain feature.
trait_source(rogue('arcane trickster') >: 3, 'mage hand legerdemain').
known_spell(rogue('arcane trickster'), int, always, [], no, 'mage hand') :-
    trait('mage hand legerdemain').
bonus_source(trait('mage hand legerdemain'),
             modify_spell(rogue('arcane trickster'),
                          'mage hand',
                          Goal)) :-
    BonusFeatures = ["invisible", "control with bonus action (cunning action)",
                     "can steal, stow, pick locks, disarm traps using relevant skill"],
    Goal = modify_spell_field(effects, [Es1,Es2]>>append(Es1,BonusFeatures,Es2)).

% Other features.
trait_source(rogue('arcane trickster') >: 9, 'magical ambush').
trait_source(rogue('arcane trickster') >: 13, 'versatile trickster').
bonus_source(trait('versatile trickster'),
             modify_spell(rogue('arcane trickster'),
                          'mage hand',
                          add_spell_effects(['distract creature within 5 ft for advantage on attack rolls until end of turn']))).
trait_source(rogue('arcane trickster') >: 17, 'spell thief').


% Learn cantrips.
known_spell(rogue('arcane trickster'), int, always, [], no, Cantrip) :-
    choice_member(rogue('arcane trickster') >: _, cantrip, Cantrip).
options_source(rogue('arcane trickster') >: 3, cantrip,
               2 unique_from class_cantrip(wizard)).
options_source(rogue('arcane trickster') >: 10, cantrip,
               class_cantrip(wizard)).

% Learn or replace unconstrained proper spells.
known_spell(rogue('arcane trickster'), int, always, [slot], no, Name) :-
    class_level(rogue:L),
    selected_at_class_level(rogue:L, 'unconstrained spell', Name).
options_source(rogue('arcane trickster') >: L, 'unconstrained spell',
               learnable_proper_spell(rogue)) :-
    member(L, [3, 8, 14, 20]).

% Learn or replace enchantment or illusion proper spells.
known_spell(rogue('arcane trickster'), int, always, [slot], no, Name) :-
    class_level(rogue:L),
    selected_at_class_level(rogue:L, 'illusion or enchantment', Name).
options_source(rogue('arcane trickster') >: 3, 'illusion or enchantment',
               2 unique_from learnable_arcane_trickster_spell).
options_source(rogue('arcane trickster') >: L, 'illusion or enchantment',
               learnable_arcane_trickster_spell) :-
    member(L, [4,7,8,10,11,13,14,16,19,20]).
learnable_arcane_trickster_spell(Spell) :-
    learnable_proper_spell(rogue, Spell),
    spell_property(Spell, school, School),
    (School = illusion ; School = enchantment).
extend_class_spell_list(rogue, Spell) :-
    rogue('arcane trickster') >: 3,
    spell_property(Spell, classes, Classes),
    member(wizard, Classes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sneak_attack(_) ?= "Beginning at 1st level, you know how to strike subtly and exploit a foe's distraction. Once per turn, you can deal an extra 1d6 damage to one creature you hit with an attack if you have advantage on the attack roll. The attack must use a finesse or a ranged weapon. You don't need advantage on the attack roll if another enemy of the target is within 5 feet of it, that enemy isn't incapacitated, and you don't have disadvantage on the attack roll. The amount of the extra damage increases as you gain levels in this class, as shown in the Sneak Attack column of the Rogue table.".

'thieves\' cant' ?= "During your rogue training you learned thieves' cant, a secret mix of dialect, jargon, and code that allows you to hide messages in seemingly normal conversation. Only another creature that knows thieves' cant understands such messages. It takes four times longer to convey such a message than it does to speak the same idea plainly. In addition, you understand a set of secret signs and symbols used to convey short, simple messages, such as whether an area is dangerous or the territory of a thieves' guild, whether loot is nearby, or whether the people in an area are easy marks or will provide a safe house for thieves on the run.".

'cunning action' ?= "Starting at 2nd level, your quick thinking and agility allow you to move and act quickly. You can take a bonus action on each of your turns in combat. This action can be used only to take the Dash, Disengage, or Hide action.".

'uncanny dodge' ?= "Starting at 5th level, when an attacker that you can see hits you with an attack, you can use your reaction to halve the attack's damage against you.".

evasion ?= "Beginning at 7th level, you can nimbly dodge out of the way of certain area effects, such as a red dragon's fiery breath or an ice storm spell. When you are subjected to an effect that allows you to make a Dexterity saving throw to take only half damage, you instead take no damage if you succeed on the saving throw, and only half damage if you fail.".

'reliable talent' ?= "By 11th level, you have refined your chosen skills until they approach perfection. Whenever you make an ability check that lets you add your proficiency bonus, you can treat a d20 roll of 9 or lower as a 10.".

'blindsense' ?= "Starting at 14th level, if you are able to hear, you are aware of the location of any hidden or invisible creature within 10 feet of you.".

'slippery mind' ?= "By 15th level, you have acquired greater mental strength. You gain proficiency in Wisdom saving throws.".

elusive ?= "Beginning at 18th level, you are so evasive that attackers rarely gain the upper hand against you. No attack roll has advantage against you while you aren't incapacitated.".

'stroke of luck' ?= "At 20th level, you have an uncanny knack for succeeding when you need to. If your attack misses a target within range, you can turn the miss into a hit. Alternatively, if you fail an ability check, you can treat the d20 roll as a 20.Once you use this feature, you can't use it again until you finish a short or long rest.".

'mage hand legerdemain' ?= "Starting at 3rd level, when you cast Mage Hand, you can make the spectral hand invisible, and you can perform the following additional tasks with it >:  You can stow one object the hand is holding in a container worn or carried by another creature. You can retrieve an object in a container worn or carried by another creature. You can use thieves' tools to pick locks and disarm traps at range. You can perform one of these tasks without being noticed by a creature if you succeed on a Dexterity (Sleight of Hand check contested by the creature's Wisdom (Perception) check. In addition, you can use the bonus action granted by your Cunning Action to control the hand.".

'magical ambush' ?= "Starting at 9th level, if you are hidden from a creature when you cast a spell on it, the creature has disadvantage on any saving throw it makes against the spell this turn.".

'versatile trickster' ?= "At 13th level, you gain the ability to distract targets with your Mage Hand. As a bonus action on your turn, you can designate a creature within 5 feet of the spectral hand created by the spell. Doing so gives you advantage on attack rolls against that creature until the end of the turn.".

'spell thief' ?= "At 17th level, you gain the ability to magically steal the knowledge of how to cast a spell from another spellcaster. Immediately after a creature casts a spell that targets you or includes you in its area of effect, you can use your reaction to force the creature to make a saving throw with its spellcasting ability modifier. The DC equals your spell save DC. On a failed save, you negate the spell's effect against you, and you steal the knowledge of the spell if it is at least 1st level and of a level you can cast (it doesn't need to be a wizard spell). For the next 8 hours, you know the spell and can cast it using your spell slots. The creature can't cast that spell until the 8 hours have passed. Once you use this feature, you can't use it again until you finish a long rest.".
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
trait_options_source(^bard, 'musical instrument', wrap(musical_instrument),
                     3 unique_from musical_instrument).
trait_options_source(^bard, skill, wrap(skill), 3 unique_from skill).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 1 features
trait_source(bard >: 1, armor(light)).
trait_options_source(bard >: 1, skill, wrap(skill), skill) :-
    \+ (^bard).
trait_options_source(bard >: 1, 'musical instrument', wrap(musical_instrument),
                     musical_instrument) :-
    \+ (^bard).
musical_instrument(lute). % TODO
meta_todo('musical instrument', "move this predicate out of bard class and list instruments").
trait_source(bard >: 1, 'bardic inspiration').
resource('bardic inspiration', 'bardic inspiration', N) :-
    trait('bardic inspiration'),
    ability_mod(cha, N).
on_rest(long, 'bardic inspiration', 'full restore').
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
on_rest(short, 'bardic insipiration', 'full restore') :-
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% College of Lore
subclass_option(bard, lore).
trait_options_source(bard(lore) >: 3, skill, wrap(skill), 3 unique_from skill).
trait_source(bard(lore) >: 3, 'curring words').
trait_options_source(bard(lore) >: 6, 'magical secrets', wrap(magical_secret),
                     2 unique_from magical_secret_spell).
trait_source(bard(lore) >: 14, 'peerless skill').

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
class_option(monk).
hd_per_level(monk, 1 d 8).
initial_class_base_hp(monk, 8).
max_hp_per_level(monk, 1 d 8).
caster(monk, 0).
choose_subclass_level(monk:3).
asi_level(monk:L) :-
    default_asi_level(L).
class_saving_throw(monk, str).
class_saving_throw(monk, dex).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initial class features
class_skill_list(monk,
                 [acrobatics, athletics, history, insight, religion, stealth]).
trait_options_source(^monk, skill, wrap(skill), 2 unique_from class_skill(monk)).
trait_options_source(^monk, 'tool or instrument', id, free_choice).
meta_todo(artisans_tool, "make an artisans_tool predicate").
meta_todo(musical_instrument, "make a musical_instrument predicate").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 1 features.
traits_from_source(monk >: 1, [weapon(simple), weapon(shortsword)]).

trait_source(monk >: 1, unarmored_defense(10 + dex + wis)).

% Martial arts.
trait_source(monk >: 1, martial_arts(1 d N)) :-
    class_level(monk:L),
    ordered_lookup_largest_leq([1 -> 4, 5 -> 6, 11 -> 8, 17 -> 10], L, N).
monk_weapon(unarmed).
monk_weapon('short sword').
monk_weapon(Weapon) :-
    weapon(Weapon, simple, _, _, Notes),
    intersection([heavy, twohanded], Notes, []).
bonus_source(trait(martial_arts(_)), use_ability(Weapon, dex)) :-
    monk_weapon(Weapon).
bonus(trait(martial_arts(1 d N)),
      override_attack_damage_rolls(Weapon,[damage(Type,1 d N)])) :-
    trait(martial_arts(1 d N)),
    monk_weapon(Weapon),
    weapon(Weapon, _, _, [damage(Type,Die)], _),
    ((Die = 1 d M, N > M) ; Die = 1).
bonus_source(trait(martial_arts(_)),
             add_weapon_note(Weapon, "unarmed strike as bonus action")) :-
    monk_weapon(Weapon).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features gained from leveling up.

% Ki features.
trait_source(monk >: 2, ki(N, dc(DC))) :-
    class_level(monk:N),
    proficiency_bonus(ProfBon),
    ability_mod(wis, WisMod),
    DC is 8 + ProfBon + WisMod.
traits_from_source(monk >: 2, [ki_feature('flurry of blows'),
                               ki_feature('patient defense'),
                               ki_feature('step of the wind')]).
resource(ki, 'ki points', N) :-
    trait(ki(N, _)).
on_rest(short, 'ki points', 'full restore').

custom_format(ki(N, dc(DC))) -->
    ["ki: "], [N], [" points, "], [DC], [" save DC"].

% Unarmored movement.
trait_source(monk >: 2, unarmored_movement(ft(Ft))) :-
    class_level(monk:L),
    Ft is 10 + 5 * floor((L-2) / 4).
bonus_source(trait(unarmored_movement(ft(Ft))), 'unarmored speed' + Ft).
trait_source(monk >: 9, unarmored_movement("vertical surfaces and liquids")).
custom_format(unarmored_movement(ft(Ft))) -->
    ["unarmored movement ("], [Ft], [" ft)"].
suppress_unarmored_ac_formula :-
    trait(monk >: _, unarmored_movement(_)).

% ...
trait_source(monk >: 3, deflect_missiles(1 d 10 + Red)) :-
    ability_mod(dex, Mod),
    class_level(monk:L),
    Red is Mod + L.

trait_source(monk >: 4, slow_fall(Red)) :-
    class_level(monk:L),
    Red is L*5.

multiclass_trait_source(monk >: 5, extra_attack(1)).

trait_source(monk >: 5, ki_feature('stunning strike')).

trait_source(monk >: 6, 'ki-empowered strikes').
bonus_source(trait('ki-empowered strikes'), add_weapon_note(unarmed, magical)).

trait_source(monk >: 7, evasion).

trait_source(monk >: 7, 'stillness of mind').

trait_source(monk >: 10, 'purity of body').

trait_source(monk >: 13, 'tongue of the sun and moon').

trait_source(monk >: 14, 'diamond soul').
trait_source(trait('diamond soul'), saving_throw(Abi)) :- ability(Abi).

trait_source(monk >: 15, 'timeless body').

trait_source(monk >: 18, ki_feature('empty body')).

trait_source(monk >: 20, 'perfect self').
meta_todo('perfect self', "encode the regain of ki points").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MONASTIC TRADITIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Way of the Open Hand
% --------------------
subclass_option(monk, 'open hand').

trait_source(monk('open hand') >: 3, 'open hand technique').

trait_source(monk('open hand') >: 6, wholeness_of_body(hp(HP))) :-
    class_level(monk:L),
    HP is L*3.
resource('wholeness of body', 'wholeness of body', 1) :-
    trait(wholeness_of_body(_)).
on_rest(long, 'wholeness of body', 'full restore').

trait_source(monk('open hand') >: 11, tranquility(dc(DC))) :-
    add_ability_mod_and_profbon(8, wis, DC).

trait_source(monk('open hand') >: 17, 'quivering palm').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DESCRIPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
meta_todo(monk, "most descriptions").

unarmored_movement(ft(_)) ?= "Starting at 2nd level, your speed increases by 10 feet while you are not wearing armor or wielding a shield. This bonus increases when you reach certain monk levels, as shown in the Monk table.".
unarmored_movement("vertical surfaces and liquids") ?= "At 9th level, you gain the ability to move along vertical surfaces and across liquids on your turn without falling during the move.".

deflect_missiles(_) ?= "Starting at 3rd level, you can use your reaction to deflect or catch the missile when you are hit by a ranged weapon attack. When you do so, the damage you take from the attack is reduced by 1d10 + your Dexterity modifier + your monk level.

If you reduce the damage to 0, you can catch the missile if it is small enough for you to hold in one hand and you have at least one hand free. If you catch a missile in this way, you can spend 1 ki point to make a ranged attack with the weapon or piece of ammunition you just caught, as part of the same reaction. You make this attack with proficiency, regardless of your weapon proficiencies, and the missile counts as a monk weapon for the attack, which has a normal range of 20 feet and a long range of 60 feet.".

ki_feature('stunning strike') ?= "Starting at 5th level, you can interfere with the flow of ki in an opponent’s body. When you hit another creature with a melee weapon attack, you can spend 1 ki point to attempt a stunning strike. The target must succeed on a Constitution saving throw or be stunned until the end of your next turn.".

martial_arts(_) ?= "At 1st level, your practice of martial arts gives you mastery of combat styles that use unarmed strikes and monk weapons, which are shortswords and any simple melee weapons that don’t have the two-handed or heavy property. You gain the following benefits while you are unarmed or wielding only monk weapons and you aren't wearing armor or wielding a shield:

- You can use Dexterity instead of Strength for the attack and damage rolls of your unarmed strikes and monk weapons.
- You can roll a d4 in place of the normal damage of your unarmed strike or monk weapon. This die changes as you gain monk levels, as shown in the Martial Arts column of the Monk table.
- When you use the Attack action with an unarmed strike or a monk weapon on your turn, you can make one unarmed strike as a bonus action. For example, if you take the Attack action and attack with a quarterstaff, you can also make an unarmed strike as a bonus action, assuming you haven't already taken a bonus action this turn.

Certain monasteries use specialized forms of the monk weapons. For example, you might use a club that is two lengths of wood connected by a short chain (called a nunchaku) or a sickle with a shorter, straighter blade (called a kama). Whatever name you use for a monk weapon, you can use the game statistics provided for the weapon.".

slow_fall(_) ?= "Beginning at 4th level, you can use your reaction when you fall to reduce any falling damage you take by an amount equal to five times your monk level.".
class_option(cleric).
hd_per_level(cleric, 1 d 8).
initial_class_base_hp(cleric, 8).
max_hp_per_level(cleric, 1 d 8).
caster(cleric, full).
spellcasting_ability(cleric, wis).
max_prepared_spells(cleric, N) :-
    default_max_prepared_spells(cleric, N).
choose_subclass_level(cleric:1).
asi_level(cleric:L) :-
    default_asi_level(L).
class_saving_throw(cleric, wis).
class_saving_throw(cleric, cha).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Spellcasting.

% Clerics get to pick cantrips.
known_spell(cleric, wis, always, [], no, Name) :-
    class_choice(cleric, cantrip, Name).
options_source(cleric >: 1, cantrip, 3 unique_from class_cantrip(cleric)).
options_source(cleric >: L, cantrip, class_cantrip(cleric)) :-
    L=4; L=10.

% Clerics know all proper spells on their spell list.
% These always need to be prepared, with the exception of domain spells.
known_spell(cleric, wis, 'when prepared', [slot], Ritual, Name) :-
    learnable_proper_spell(cleric, Name),
    \+ (subclass(Class), Class =.. [cleric, Domain], cleric_domain_spell(Domain, Name)),
    spell_property(Name, ritual, Ritual).

% Domain spells are always prepared.
known_spell(cleric(Domain), wis, always, [slot], Ritual, Name) :-
    subclass(cleric(Domain)),
    cleric_domain_spell(Domain, Name),
    learnable_proper_spell(cleric, Name),
    spell_property(Name, ritual, Ritual).

% Add domain spells to the cleric spell list.
extend_class_spell_list(cleric, Spell) :-
    subclass(Class),
    Class =.. [cleric, Domain],
    cleric_domain_spell(Domain, Spell).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initial class features.
traits_from_source(^cleric,
                   [armor(light), armor(medium), armor(shield),
                    weapon(simple)]).

trait_options_source(^cleric, skill, wrap(skill),
                     2 unique_from from_list(
                         [history,insight,medicine,persuasion,religion])).

trait_source(cleric >: 1, spellcasting_focus(divine)).
trait_source(cleric >: 1, ritual_casting(cleric)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features from leveling up.
trait_source(cleric >: 2, 'channel divinity (cleric)').
trait_source(cleric >: 2, channel_divinity('turn undead')).
bonus_source(cleric >: 2, channel_divinity_uses(N)) :-
    class_level(cleric:Lvl),
    ordered_lookup_largest_leq([2 -> 1, 6 -> 2, 18 -> 3], Lvl, N).
meta_todo(cleric, "'channel divinity' has specific multiclassing rules").

trait_source(cleric >: 5, destroy_undead(cr(CR))) :-
    class_level(cleric:L),
    ordered_lookup_largest_leq([5->1/2, 8->1, 11->2, 14->3, 17->4], L, CR).

trait_source(cleric >: 10, divine_intervention(Pct pct)) :-
    class_level(cleric:Level),
    (Level < 20 -> Pct = Level ; Pct = 100).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DOMAINS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Life domain.
subclass_option(cleric, life).

cleric_domain_spell(life, bless).
cleric_domain_spell(life, 'cure wounds').
cleric_domain_spell(life, 'lesser restoration').
cleric_domain_spell(life, 'spiritual weapon').
cleric_domain_spell(life, 'beacon of hope').
cleric_domain_spell(life, revivify).
cleric_domain_spell(life, 'death ward').
cleric_domain_spell(life, 'guardian of faith').
cleric_domain_spell(life, 'mass cure wounds').
cleric_domain_spell(life, 'raise dead').

trait_source(cleric(life) >: 1, armor(heavy)).
trait_source(cleric(life) >: 1, 'disciple of life').
bonus_source(trait('disciple of life'),
             modify_spell(_, HealingSpell, increase_all_spell_healing_rolls(Bonus))) :-
    spell_property(HealingSpell, level, Level),
    Level >= 1,
    healing_spell(HealingSpell),
    Bonus is 2 + Level.

meta_todo(trait('disciple of life'), "How to communicate the upcasting effect on the spell card?").

trait_source(cleric(life) >: 2, channel_divinity(preserve_life(Pool))) :-
    class_level(cleric:L),
    Pool is 5*L.
trait_source(cleric(life) >: 6, 'blessed healer').
bonus_source(trait('blessed healer'),
             modify_spell(_, HealingSpell, add_spell_effects([(target=other)->self_heal(HP)]))) :-
    spell_property(HealingSpell, level, Level),
    Level >= 1,
    \+ spell_property(HealingSpell, range, self),
    healing_spell(HealingSpell),
    HP is 2 + Level.
custom_format(self_heal(HP)) --> [" heal self for "], format_number(HP).

trait_source(cleric(life) >: 8, divine_strike(N d 8)) :-
    class_level(cleric:L),
    (L < 14 -> N = 1; N = 2).
trait_source(cleric(life) >: 17, 'supreme healing').
bonus_source(trait('supreme healing'),
             modify_spell(_, HealingSpell, apply_supreme_healing)) :-
    spell_property(HealingSpell, effects, Effects),
    subterm_member(heal(Formula), Effects),
    subterm_member(_ d _, Formula). % Only applies to healing spells for which you have to roll.
apply_supreme_healing(Old, Old.put(effects, NewEffects)) :-
    map_matching_subterms(heal_roll_max, Old.get(effects), NewEffects).
heal_roll_max(heal(Formula), heal(NewFormula)) :-
    map_matching_subterms(roll_max, Formula, MaxedRolls),
    simplify_dice_sum(MaxedRolls, NewFormula).

'disciple of life' ?= "Starting at 1st level, your healing spells are more effective. Whenever you use a spell of 1st level or higher to restore hit points to a creature, the creature regains additional hit points equal to 2 + the spell's level.".
'blessed healer' ?= "Beginning at 6th level, the healing spells you cast on others heal you as well. When you cast a spell of 1st level or higher that restores hit points to a creature other than you, you regain hit points equal to 2 + the spell's level.".
'divine strike' ?= "At 8th level, you gain the ability to infuse your weapon strikes with divine energy. Once on each of your turns when you hit a creature with a weapon attack, you can cause the attack to deal an extra 1d8 radiant damage to the target. When you reach 14th level, the extra damage increases to 2d8.".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Knowledge domain.
subclass_option(cleric, knowledge).

cleric_domain_spell(knowledge, command).
cleric_domain_spell(knowledge, identify).
cleric_domain_spell(knowledge, augury).
cleric_domain_spell(knowledge, suggestion).
cleric_domain_spell(knowledge, nondetection).
cleric_domain_spell(knowledge, 'speak with dead').
cleric_domain_spell(knowledge, 'arcane eye').
cleric_domain_spell(knowledge, confusion).
cleric_domain_spell(knowledge, 'legend lore').
cleric_domain_spell(knowledge, scrying).

% Blessings of Knowledge
trait_source(cleric(knowledge) >: 1, 'blessings of knowledge').
trait_options(trait('blessings of knowledge'), language, wrap(skill),
              2 unique_from language) :-
    cleric(knowledge) >: 1.
trait_options(trait('blessings of knowledge'), skill, wrap(skill),
              2 unique_from from_list([arcana,history,nature,religion])) :-
    cleric(knowledge) >: 1.
trait(trait('blessings of knowledge'), expertise(skill(Skill))) :-
    choice_member(trait('blessings of knowledge'), skill, Skill).
meta_todo(nontermination, "why can't the blessings of knowledge trait options refer to the blessings of knowledge trait without causing an infinite loop? [later note: I don't know what this is about, I can't reproduce a nontermination issue here]").
meta_todo(trait('blessings of knowledge'), "Technically it's not expertise, but mechanically I'm not sure it's worth making a distinction here. In particular if it's not expertise, I'm not sure how/whether it stacks with expertise.").

% Knowledge of the ages.
trait_source(cleric(knowledge) >: 2, 'knowledge of the ages').
trait_source(cleric(knowledge) >: 2, channel_divinity('read thoughts')).
    
meta_todo(cleric(knowledge), "Complete this subclass implementation.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DESCRIPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'channel divinity (cleric)' ?= "At 2nd level, you gain the ability to channel divine energy directly from your deity, using that energy to fuel magical effects. You start with two such effects: Turn Undead and an effect determined by your domain. Some domains grant you additional effects as you advance in levels, as noted in the domain description.
When you use your Channel Divinity, you choose which effect to create. You must then finish a short or long rest to use your Channel Divinity again. Some Channel Divinity effects require saving throws. When you use such an effect from this class, the DC equals your cleric spell save DC.
Beginning at 6th level, you can use your Channel Divinity twice between rests, and beginning at 18th level, you can use it three times between rests. When you finish a short or long rest, you regain your expended uses.".

channel_divinity('turn undead') ?= "As an action, you present your holy symbol and speak a prayer censuring the undead. Each undead that can see or hear you within 30 feet of you must make a Wisdom saving throw. If the creature fails its saving throw, it is turned for 1 minute or until it takes any damage.
A turned creature must spend its turns trying to move as far away from you as it can, and it can't willingly move to a space within 30 feet of you. It also can't take reactions. For its action, it can use only the Dash action or try to escape from an effect that prevents it from moving. If there's nowhere to move, the creature can use the Dodge action.".

channel_divinity('knowledge of the ages') ?= "Starting at 2nd level, you can use your Channel Divinity to tap into a divine well of knowledge. As an action, you choose one skill or tool. For 10 minutes, you have proficiency with the chosen skill or tool.".

channel_divinity('read thoughts') ?= "At 6th level, you can use your Channel Divinity to read a creature's thoughts. You can then use your access to the creature's mind to command it.

As an action, choose one creature that you can see within 60 feet of you. That creature must make a Wisdom saving throw. If the creature succeeds on the saving throw, you can't use this feature on it again until you finish a long rest.

If the creature fails its save, you can read its surface thoughts (those foremost in its mind, reflecting its current emotions and what it is actively thinking about) when it is within 60 feet of you. This effect lasts for 1 minute.

During that time, you can use your action to end this effect and cast the Suggestion spell on the creature without expending a spell slot. The target automatically fails its saving throw against the spell.".

destroy_undead(_) ?= "Starting at 5th level, when an undead fails its saving throw against your Turn Undead feature, the creature is instantly destroyed if its challenge rating is at or below a certain threshold, as shown in the Destroy Undead table.".

divine_intervention(_) ?= "Beginning at 10th level, you can call on your deity to intervene on your behalf when your need is great. Imploring your deity's aid requires you to use your action. Describe the assistance you seek, and roll percentile dice. If you roll a number equal to or lower than your cleric level, your deity intervenes. The GM chooses the nature of the intervention; the effect of any cleric spell or cleric domain spell would be appropriate. If your deity intervenes, you can't use this feature again for 7 days. Otherwise, you can use it again after you finish a long rest. At 20th level, your call for intervention succeeds automatically, no roll required.".
class_option(fighter).
hd_per_level(fighter, 1 d 10).
initial_class_base_hp(fighter, 10).
max_hp_per_level(fighter, 1 d 10).
choose_subclass_level(fighter:3).
class_saving_throw(fighter, str).
class_saving_throw(fighter, con).
asi_level(fighter:L) :-
    member(L, [4,6,8,12,14,16,19]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initial class features.
trait_source(^fighter, armor(heavy)).
trait_options_source(^fighter, skill, wrap(skill),
                     2 unique_from from_list(
                         [acrobatics, 'animal handling', athletics, history,
                          insight, intimidation, perception, survivial])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 1 features.
traits_from_source(fighter >: 1,
                   [armor(light), armor(medium), armor(shield),
                    weapon(simple), weapon(martial)]).

trait_options_source(fighter >: 1, 'fighting style',
                     wrap(fighting_style), fighting_style).

trait_source(fighter >: 1, second_wind(1 d 10 + L)) :-
    class_level(fighter:L).
resource('second wind', 'second wind', 1) :-
    trait(second_wind(_)).
on_rest(short, 'second wind', 'full restore') :-
    trait(second_wind(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Traits from leveling up.
trait_source(fighter >: 2, 'action surge').
resource('action surge', 'action surge', N) :-
    class_level(fighter:L),
    ordered_lookup_largest_leq([2 -> 1, 17 -> 2], L, N).
on_rest(short, 'action surge', 'full restore').

multiclass_trait_source(fighter >: 5, extra_attack(N)) :-
    class_level(fighter:L),
    ordered_lookup_largest_leq([5 -> 1, 11 -> 2, 20 -> 3], L, N).

trait_source(fighter >: 9, indomitable).
resource(indomitable, indomitable, N) :-
    class_level(fighter:L),
    ordered_lookup_largest_leq([9 -> 1, 13 -> 2, 17 -> 3], L, N).
on_rest(long, indomitable, 'full restore').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MARTIAL ARCHETYPES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subclass_option(fighter, champion).
trait_source(fighter(champion) >: 3, 'improved critical').
trait_source(fighter(champion) >: 7, 'remarkable athlete').
trait_options_source(fighter(champion) >: 10, 'fighting style',
                     wrap(fighting_style), fighting_style).
trait_source(fighter(champion) >: 15, 'superior critical').
trait_source(fighter(champion) >: 18, survivor(HP)) :-
    ability_mod(con, Mod),
    HP is 5 + Mod.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DESCRIPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

second_wind(_) ?= "You have a limited well of stamina that you can draw on to protect yourself from harm. On your turn, you can use a bonus action to regain hit points equal to 1d10 + your fighter level. Once you use this feature, you must finish a short or long rest before you can use it again.".

'action surge' ?= "Starting at 2nd level, you can push yourself beyond your normal limits for a moment. On your turn, you can take one additional action on top of your regular action and a possible bonus action. Once you use this feature, you must finish a short or long rest before you can use it again. Starting at 17th level, you can use it twice before a rest, but only once on the same turn.".

indomitable ?= "Beginning at 9th level, you can reroll a saving throw that you fail. If you do so, you must use the new roll, and you can’t use this feature again until you finish a long rest. You can use this feature twice between long rests starting at 13th level and three times between long rests starting at 17th level.".

fighting_style(archery) ?= "You gain a +2 bonus to attack rolls you make with ranged weapons.".
fighting_style(dueling) ?= "When you are wielding a melee weapon in one hand and no other weapons, you gain a +2 bonus to damage rolls with that weapon.".
fighting_style('great weapon fighting') ?= "When you roll a 1 or 2 on a damage die for an attack you make with a melee weapon that you are wielding with two hands, you can reroll the die and must use the new roll, even if the new roll is a 1 or a 2. The weapon must have the two-handed or versatile property for you to gain this benefit.".
fighting_style(protection) ?= "When a creature you can see attacks a target other than you that is within 5 feet of you, you can use your reaction to impose disadvantage on the attack roll. You must be wielding a shield.".
fighting_style('two-weapon fighting') ?= "When you engage in two-weapon fighting, you can add your ability modifier.".

'improved critical' ?= "Beginning when you choose this archetype at 3rd level, your weapon attacks score a critical hit on a roll of 19 or 20.".

'remarkable athlete' ?= "Starting at 7th level, you can add half your proficiency bonus (round up) to any Strength, Dexterity, or Constitution check you make that doesn’t already use your proficiency bonus. In addition, when you make a running long jump, the distance you can cover increases by a number of feet equal to your Strength modifier.".

'additional fighting style' ?= "At 10th level, you can choose a second option from the Fighting Style class feature.".

'superior critical' ?= "Starting at 15th level, your weapon attacks score a critical hit on a roll of 18–20.".

survivor(_) ?= "At 18th level, you attain the pinnacle of resilience in battle. At the start of each of your turns, you regain hit points equal to 5 + your Constitution modifier if you have no more than half of your hit points left. You don’t gain this benefit if you have 0 hit points.".
class_option(sorcerer).
hd_per_level(sorcerer, 1 d 6).
initial_class_base_hp(sorcerer, 6).
max_hp_per_level(sorcerer, 1 d 6).
caster(sorcerer, full).
spellcasting_ability(sorcerer, cha).
choose_subclass_level(sorcerer:1).
asi_level(sorcerer:L) :-
    default_asi_level(L).
class_saving_throw(sorcerer, con).
class_saving_throw(sorcerer, cha).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initial class features (you don't gain these when multiclassing into sorcerer).
traits_from_source(^sorcerer,
                   [weapon(dagger),
                    weapon(dart),
                    weapon(sling),
                    weapon(quarterstaff),
                    weapon('light crossbow')]).

trait_options_source(^sorcerer, skill, wrap(skill),
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
trait_source(sorcerer >: 2, 'font of magic').
trait_options_source(sorcerer >: 3, metamagic, wrap(metamagic),
                     2 unique_from metamagic_option).
trait_options_source(sorcerer >: L, metamagic, wrap(metamagic), metamagic_option) :-
    L = 10 ; L = 17.
trait_source(sorcerer >: 20, 'sorcerous restoration').

% Metamagic.
metamagic_option('careful spell').
metamagic_option('distant spell').
metamagic_option('empowered spell').
metamagic_option('extended spell').
metamagic_option('heightened spell').
metamagic_option('quickened spell').
metamagic_option('subtle spell').
metamagic_option('twinned spell').

% Calculate the maximum number of sorcery points your character has available.
resource(metamagic, 'sorcery point', Max) :-
    sorcerer >: 3,
    class_level(sorcerer:Max).
on_rest(long, 'sorcery point', 'full restore') :- sorcerer >: 3.
on_rest(short, 'sorcery point', restore(4)) :- trait('sorcerous restoration').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Learning new sorcerer spells.

known_spell(sorcerer, cha, always, [], no, Name) :-
    class_origin_to_class(Origin, sorcerer),
    choice_member(Origin, cantrip, Name).
known_spell(sorcerer, cha, always, [slot], Ritual, Name) :-
    class_level(sorcerer:L),
    selected_at_class_level(sorcerer:L, spell, Name),
    spell_property(Name, ritual, Ritual). % TODO: this might be wrong

% Learn cantrips.
options_source(sorcerer >: 1, cantrip, 4 unique_from class_cantrip(sorcerer)).
options_source(sorcerer >: L, cantrip, class_cantrip(sorcerer)) :- L=4 ; L=10.
   
% Learn proper spells.
options_source(sorcerer >: 1, spell,
               2 unique_from learnable_proper_spell(sorcerer)).
options_source(sorcerer >: L, spell,
               learnable_proper_spell(sorcerer)) :-
    member(L, [2,3,4,5,6,7,8,9,10,11,13,15,17]).

% Replace proper spells.
options_source(sorcerer >: L, replace(spell),
               selected_at_class_level(sorcerer:Prev, spell)) :-
    between(2, 20, L),
    Prev is L-1.
options(sorcerer >: L, replacing(spell, Name), learnable_proper_spell(sorcerer)) :-
    choice_member(sorcerer >: L, replace(spell), Name).

% Suppress showing duplicate spells.
hide_known_class_spells(sorcerer >: _, cantrip, sorcerer).
hide_known_class_spells(sorcerer >: _, spell, sorcerer).
hide_known_class_spells(sorcerer >: _, replacing(spell,_), sorcerer).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sorcerous origin: draconic bloodline.

subclass_option(sorcerer, 'draconic bloodline').

% Dragon ancestor.
trait_options_source(sorcerer('draconic bloodline') >: 1,
                     'dragon ancestor',
                     wrap(draconic_bloodline),
                     dragon_color).
draconic_bloodline_element(Element) :-
    trait(draconic_bloodline(Color)),
    dragon_element(Color, Element).

trait_source(sorcerer('draconic bloodline') >: 1, language(draconic)).

% Draconic resilience.
trait_source(sorcerer('draconic bloodline') >: 1, 'draconic resilience').
bonus_source(trait('draconic resilience'), 'max hp'+Level) :-
    class_level(sorcerer:Level).
bonus_source(trait('draconic resilience'), ac_formula(13 + dex + shield)).
suppress_unarmored_ac_formula :- trait('draconic resilience').

% Elemental affinity.
trait_source(sorcerer('draconic bloodline') >: 6,
             elemental_affinity(Element)) :-
    trait(draconic_bloodline(Color)),
    dragon_element(Color, Element).

bonus_source(trait(elemental_affinity(Element)),
             modify_spell(_, Name, Goal)) :-
    draconic_bloodline_element(Element),
    known_spell(Name),
    spell_property(Name, effects, Effects),
    subterm_member(damage(Element,_), Effects),
    Goal = modify_spell_field(effects, apply_elemental_affinity).

apply_elemental_affinity(OldEffects, NewEffects) :-
    \+ contains_multiple_damage_rolls(OldEffects),
    draconic_bloodline_element(Element),
    ability_mod(cha, Bonus),
    select_subterm(damage(Element,Dice), OldEffects, damage(Element,NewDice), NewEffects),
    simplify_dice_sum(Dice + Bonus, NewDice).
apply_elemental_affinity(OldEffects, NewEffects) :-
    contains_multiple_damage_rolls(OldEffects),
    ability_mod(cha, Bonus),
    atomics_to_string(["add +", Bonus, " to one damage roll"], New),
    append(OldEffects, [New], NewEffects).

custom_format(modify_spell_field(effects, apply_elemental_affinity)) -->
    {draconic_bloodline_element(Elem), ability_mod(cha, Mod)},
    ["+"], [Mod], [" to one damage roll, spend sorcery point to resist "], [Elem], [" (1h)"].

% Dragon wings.
trait_source(sorcerer('draconic bloodline') >: 14, 'dragon wings').

% Draconic presence.
trait_source(sorcerer('draconic bloodline') >: 18, 'draconic presence').

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

draconic_bloodline(_) ?= "You have a specific dragon type as your ancestor. Whenever you make a Charisma check when interacting with dragons, your proficiency bonus is doubled if it applies to the check.".

'draconic resilience' ?= "As magic flows through your body, it causes physical traits of your dragon ancestors to emerge. At 1st level, your hit point maximum increases by 1 and increases by 1 again whenever you gain a level in this class.
Additionally, parts of your skin are covered by a thin sheen of dragon-like scales. When you aren't wearing armor, your AC equals 13 + your Dexterity modifier. ".

elemental_affinity(_) ?= "Starting at 6th level, when you cast a spell that deals damage of the type associated with your draconic ancestry, you can add your Charisma modifier to one damage roll of that spell. At the same time, you can spend 1 sorcery point to gain resistance to that damage type for 1 hour.".

'dragon wings' ?= "At 14th level, you gain the ability to sprout a pair of dragon wings from your back, gaining a flying speed equal to your current speed. You can create these wings as a bonus action on your turn. They last until you dismiss them as a bonus action on your turn.
You can't manifest your wings while wearing armor unless the armor is made to accommodate them, and clothing not made to accommodate your wings might be destroyed when you manifest them.".

'draconic presence '?= "Beginning at 18th level, you can channel the dread presence of your dragon ancestor, causing those around you to become awestruck or frightened. As an action, you can spend 5 sorcery points to draw on this power and exude an aura of awe or fear (your choice) to a distance of 60 feet. For 1 minute or until you lose your concentration (as if you were casting a concentration spell), each hostile creature that starts its turn in this aura must succeed on a Wisdom saving throw or be charmed (if you chose awe) or frightened (if you chose fear) until the aura ends. A creature that succeeds on this saving throw is immune to your aura for 24 hours.".

'dragon wings' ?= "At 14th level, you gain the ability to sprout a pair of dragon wings from your back, gaining a flying speed equal to your current speed. You can create these wings as a bonus action on your turn. They last until you dismiss them as a bonus action on your turn.
You can't manifest your wings while wearing armor unless the armor is made to accommodate them, and clothing not made to accommodate your wings might be destroyed when you manifest them.".

'draconic presence' ?= "Beginning at 18th level, you can channel the dread presence of your dragon ancestor, causing those around you to become awestruck or frightened. As an action, you can spend 5 sorcery points to draw on this power and exude an aura of awe or fear (your choice) to a distance of 60 feet. For 1 minute or until you lose your concentration (as if you were casting a concentration spell), each hostile creature that starts its turn in this aura must succeed on a Wisdom saving throw or be charmed (if you chose awe) or frightened (if you chose fear) until the aura ends. A creature that succeeds on this saving throw is immune to your aura for 24 hours.".
class_option(gambler).
hd_per_level(gambler, 1 d 8).
initial_class_base_hp(gambler, 8).
max_hp_per_level(gambler, 1 d 8).
caster(gambler, 0).
%spellcasting_ability(gambler, cha).
%choose_subclass_level(gambler:1).
asi_level(gambler:L) :-
    default_asi_level(L).
class_saving_throw(gambler, dex).
class_saving_throw(gambler, cha).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features gained for picking this class as initial class.
trait_options_source(^gambler, skill, wrap(skill),
                     3 unique_from from_list(
                         [acrobatics, athletics, deception, insight,
                          investigation, perception, performance,
                          persuasion, 'sleight of hand', stealth])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features from leveling up.
traits_from_source(gambler >: 1,
                   [armor(light), weapon(simple),
                    weapon('hand crossbow'), weapon(rapier),
                    tool('deck of cards'), tool(dice)]).

trait_source(gambler >: 1, card_throwing(N)) :-
    class_level(gambler:L),
    ordered_lookup_largest_leq([1->1, 5->2, 11->3, 17->4], L, N).

attack('throw card', feet(20)/feet(40), to_hit(ToHit), [damage(piercing,1)], Notes) :-
    trait(card_throwing(NCards)),
    ability_mod(dex, DexMod),
    proficiency_bonus(ProfBon),
    ToHit is DexMod + ProfBon,
    format(string(NCardsNote), 'throw ~d cards', NCards),
    (NCards == 1 -> Notes = [] ; Notes = [NCardsNote]).

trait_source(gambler >: 1, 'deck infusion').
trait_source(gambler >: 1, 'basic deck infusion').
trait_source(gambler >: 1, 'draw hand').

meta_todo('enhanced deck infusion', 'need to implement gambler spellcasting').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

card_throwing(_) ?= "You have learned yourself to throw cards in a harmful manner. As an action you can draw and throw a card. Roll a d20 and add your Dexterity modifier and your proficiency bonus to the roll to try and hit a target. A deck of cards has a short range of 20 feet and a long range of 40 feet. On a hit with a mundane deck of cards, you deal 1 piercing damage.".

'deck infusion' ?= "Your obsession with gambling has even interested the Gods of Fate and Luck. Just for their own pleasure they have given you the power to infuse your deck of cards with magical powers.
Once per long rest you can infuse a deck of cards, making it capable of dealing magical damage instead of the 1 piercing damage. The infusion requires a deck of a number of cards depending on the type of deck, which is completely shuffled in a random order. Once infused, the cards become completely blank. Only when drawn, the original content of the card becomes clear.".

'basic deck infusion' ?= "Starting at 1st level you gain the power to infuse a basic deck of 54 cards (including jokers) with magical powers. The power of the card is dependent on its contents. The suit of the deck determines the type of damage, and the value determines the damage. Jack, Queen, King and Jokers have specific effects of their own. Kings have strong effects on the enemy, Jacks have the same effect but on the gambler. A target must be chosen before drawing a card and throwing it.
At higher levels, your Card Throwing ability grows and you are able to throw more cards at once at possibly multiple targets. At 5th level you can throw two cards. This amount increases by one at 11th level and once more at 17th level.

Hearts: Fire Damage
Diamonds: Cold Damage
Clubs: Lightning Damage
Spades: Acid Damage
Jack of Hearts: Wis Save or become Charmed
Jack of Diamonds: Con Save or become Incapacitated
Jack of Clubs: Con Save or become Paralysed
Jack of Spades Con Save or become Poisoned
Queen of Hearts: Target gets disadvantage on next attack
Queen of Diamonds: Targets movement halved next turn
Queen of Clubs: Targets reaction lost
Queen of Spades Acid splash cantrip on target
King of Hearts: Wis Save or target Charmed
King of Diamonds: Con Save or target Incapacitated
King of Clubs: Con Save or target Paralysed
King of Spades Con Save or target Poisoned
Joker (2): See Joker feature in Deck Building".

'draw hand' ?= "At 1st level as a bonus action you can spend one Gambler Point to draw a hand of three cards from one deck. You must keep these cards in your hands to be able to play the cards. You cannot draw new cards from that deck until you have used or burned all cards in the drawn hand.".
class_option(druid).
hd_per_level(druid, 1 d 8).
initial_class_base_hp(druid, 8).
max_hp_per_level(druid, 1 d 8).
caster(druid, full).
spellcasting_ability(druid, wis).
max_prepared_spells(druid, N) :-
    default_max_prepared_spells(druid, N).
choose_subclass_level(druid:2).
asi_level(druid:L) :-
    default_asi_level(L).
class_saving_throw(druid, wis).
class_saving_throw(druid, int).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initial class features.
class_skill_list(druid, [arcana, 'animal handling', insight, medicine,
                         nature, perception, religion, survival]).
traits_from_source(^druid,
                   [weapon(club), weapon(dagger), weapon(dart),
                    weapon(javelin), weapon(mace),
                    weapon(quarterstaff), weapon(scimitar),
                    weapon(sickle), weapon(sling), weapon(spear),
                    tool('herbalism kit')]).
trait_options_source(^druid, skill, wrap(skill),
                     2 unique_from class_skill(druid)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features from leveling up.
traits_from_source(druid >: 1,
                   [armor(light), armor(medium), armor(shield),
                    language(druidic)]).
trait_source(druid >: 1, 'no metal armor').

trait_source(druid >: 2, wild_shape([cr(CR),hours(Hours)|Constraints])) :-
    wild_shape_cr(CR),
    class_level(druid:Level),
    Hours is floor(Level/2),
    findall(C, (wild_shape_constraint(L,C),Level<L), Constraints).
wild_shape_cr(CR) :-
    class_level(druid:Level),
    ordered_lookup_largest_leq([2 -> 1/4, 4 -> 1/2, 8 -> 1], Level, CR).
wild_shape_constraint(4, 'no swimming speed').
wild_shape_constraint(8, 'no flying speed').
resource('wild shape', 'wild shape', 2) :-
    trait(wild_shape(_)),
    \+ trait(archdruid).
on_rest(short, 'wild shape', 'full restore').

trait_source(druid >: 18, 'timeless body').
trait_source(druid >: 18, 'beast spells').

trait_source(druid >: 20, archdruid).
bonus(trait(archdruid), modify_spell(Druidic, _, archdruid_ignore_components)) :-
    trait(archdruid),
    Druidic =.. [druid|_].
archdruid_ignore_components(Data, Data.put(components,NewComponents)) :-
    findall(m(M),
            ( member(m(M), Data.get(components)),
              sub_string(M, _, _, _, "consumes"),
              sub_string(M, _, _, _, "gp")
            ),
            NewComponents).
meta_todo(archdruid, "implement more robust way to check material cost").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Spellcasting

% Druids get to pick cantrips.
known_spell(druid, wis, always, [], no, Spell) :-
    class_choice(druid, cantrip, Spell).
options_source(druid >: 1, cantrip, 2 unique_from class_cantrip(druid)).
options_source(druid >: L, cantrip, class_cantrip(druid)) :-
    L=4; L=10.

% Druids know all proper spells on their spell list.
% These need to be prepared to be cast and cost a spell slot to cast.
known_spell(druid, wis, 'when prepared', [slot], Ritual, Spell) :-
    learnable_proper_spell(druid, Spell),
    \+ (trait(circle_spells(Circle)), druid_circle_spell(Circle, Spell)),
    spell_property(Spell, ritual, Ritual).

% Circle spells are always prepared.
known_spell(druid, wis, always, [slot], Ritual, Spell) :-
    trait(circle_spells(Circle)),
    druid_circle_spell(Circle, Spell),
    learnable_proper_spell(druid, Spell),
    spell_property(Spell, ritual, Ritual).

% Add circle spells to druid spell list.
extend_class_spell_list(druid, Spell) :-
    subclass(Class),
    Class =.. [druid, Circle],
    druid_circle_spell(Circle, Spell).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUBCLASSES (DRUIDIC CIRCLES)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subclass_option(druid, land).
custom_format(druid(land)) --> ["druid (circle of the land)"].
options_source(druid(land) >: 2, cantrip, class_cantrip(druid)).
trait_source(druid(land) >: 2, natural_recovery(Total)) :-
    class_level(druid:L),
    Total is ceiling(L / 2).
resource('natural recovery', 'spell slot total', Slots) :-
    trait(natural_recovery(Slots)).
on_rest(long, 'natural recovery', 'full restore').

trait_options_source(
    druid(land) >: 3,
    'circle spells',
    wrap(circle_spells),
    from_list([arctic,coast,desert,forest,grassland,mountain,swamp])).

trait_source(druid(land) >: 6, 'land\'s stride').

trait_source(druid(land) >: 10, 'nature\'s ward').

trait_source(druid(land) >: 14, 'nature\'s sanctuary').

druid_circle_spell(arctic, 'hold person').
druid_circle_spell(arctic, 'spike growth').
druid_circle_spell(arctic, 'sleet storm').
druid_circle_spell(arctic, slow).
druid_circle_spell(arctic, 'freedom of movement').
druid_circle_spell(arctic, 'ice storm').
druid_circle_spell(arctic, 'commune with nature').
druid_circle_spell(arctic, 'cone of cold').

druid_circle_spell(coast, 'mirror image').
druid_circle_spell(coast, 'misty step').
druid_circle_spell(coast, 'water breathing').
druid_circle_spell(coast, 'water walk').
druid_circle_spell(coast, 'control water').
druid_circle_spell(coast, 'freedom of movement').
druid_circle_spell(coast, 'conjure elemental').
druid_circle_spell(coast, scrying).

druid_circle_spell(desert, blur).
druid_circle_spell(desert, silence).
druid_circle_spell(desert, 'create food and water').
druid_circle_spell(desert, 'protection from energy').
druid_circle_spell(desert, blight).
druid_circle_spell(desert, 'hallucinatory terrain').
druid_circle_spell(desert, 'insect plague').
druid_circle_spell(desert, 'wall of stone').

druid_circle_spell(forest, barkskin).
druid_circle_spell(forest, 'spider climb').
druid_circle_spell(forest, 'call lightning').
druid_circle_spell(forest, 'plant growth').
druid_circle_spell(forest, divination).
druid_circle_spell(forest, 'freedom of movement').
druid_circle_spell(forest, 'commune with nature').
druid_circle_spell(forest, 'tree stride').

druid_circle_spell(grassland, invisibility).
druid_circle_spell(grassland, 'pass without trace').
druid_circle_spell(grassland, daylight).
druid_circle_spell(grassland, haste).
druid_circle_spell(grassland, divination).
druid_circle_spell(grassland, 'freedom of movement').
druid_circle_spell(grassland, dream).
druid_circle_spell(grassland, 'insect plague').

druid_circle_spell(mountain, 'spider climb').
druid_circle_spell(mountain, 'spike growth').
druid_circle_spell(mountain, 'lightning bolt').
druid_circle_spell(mountain, 'meld into stone').
druid_circle_spell(mountain, 'stone shape').
druid_circle_spell(mountain, stoneskin).
druid_circle_spell(mountain, passwall).
druid_circle_spell(mountain, 'wall of stone').

druid_circle_spell(swamp, 'acid arrow').
druid_circle_spell(swamp, darkness).
druid_circle_spell(swamp, 'water walk').
druid_circle_spell(swamp, 'stinking cloud').
druid_circle_spell(swamp, 'freedom of movement').
druid_circle_spell(swamp, 'locate creature').
druid_circle_spell(swamp, 'insect plague').
druid_circle_spell(swamp, scrying).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'no metal armor' ?= "druids will not wear armor or use shields made of metal".
language(druidic) ?= "You know Druidic, the secret language of druids. You can speak the language and use it to leave hidden messages. You and others who know this language automatically spot such a message. Others spot the message's presence with a successful DC 15 Wisdom (Perception) check but can't decipher it without magic.".

wild_shape(_) ?= "Starting at 2nd level, you can use your action to magically assume the shape of a beast that you have seen before. You can use this feature twice. You regain expended uses when you finish a short or long rest.
Your druid level determines the beasts you can transform into, as shown in the Beast Shapes table. At 2nd level, for example, you can transform into any beast that has a challenge rating of 1/4 or lower that doesn't have a flying or swimming speed.
Beast Shapes
Level 	Max. CR 	Limitations 	Example
2nd 	1/4 	No flying or swimming speed 	Wolf
4th 	1/2 	No flying speed 	Crocodile
8th 	1 	- 	Giant Eagle

You can stay in a beast shape for a number of hours equal to half your druid level (rounded down). You then revert to your normal form unless you expend another use of this feature. You can revert to your normal form earlier by using a bonus action on your turn. You automatically revert if you fall unconscious, drop to 0 hit points, or die.
While you are transformed, the following rules apply:

    Your game statistics are replaced by the statistics of the beast, but you retain your alignment, personality, and Intelligence, Wisdom, and Charisma scores. You also retain all of your skill and saving throw proficiencies, in addition to gaining those of the creature. If the creature has the same proficiency as you and the bonus in its stat block is higher than yours, use the creature's bonus instead of yours. If the creature has any legendary or lair actions, you can't use them.
    When you transform, you assume the beast's hit points and Hit Dice. When you revert to your normal form, you return to the number of hit points you had before you transformed. However, if you revert as a result of dropping to 0 hit points, any excess damage carries over to your normal form. For example, if you take 10 damage in animal form and have only 1 hit point left, you revert and take 9 damage. As long as the excess damage doesn't reduce your normal form to 0 hit points, you aren't knocked unconscious.
    You can't cast spells, and your ability to speak or take any action that requires hands is limited to the capabilities of your beast form. Transforming doesn't break your concentration on a spell you've already cast, however, or prevent you from taking actions that are part of a spell, such as call lightning, that you've already cast.
    You retain the benefit of any features from your class, race, or other source and can use them if the new form is physically capable of doing so. However, you can't use any of your special senses, such as darkvision, unless your new form also has that sense.
    You choose whether your equipment falls to the ground in your space, merges into your new form, or is worn by it. Worn equipment functions as normal, but the GM decides whether it is practical for the new form to wear a piece of equipment, based on the creature's shape and size. Your equipment doesn't change size or shape to match the new form, and any equipment that the new form can't wear must either fall to the ground or merge with it. Equipment that merges with the form has no effect until you leave the form.
".

'timeless body' ?= "Starting at 18th level, the primal magic that you wield causes you to age more slowly. For every 10 years that pass, your body ages only 1 year.".

'beast spells' ?= "Beginning at 18th level, you can cast many of your druid spells in any shape you assume using Wild Shape. You can perform the somatic and verbal components of a druid spell while in a beast shape, but you aren't able to provide material components.".

archdruid ?= "At 20th level, you can use your Wild Shape an unlimited number of times.
Additionally, you can ignore the verbal and somatic components of your druid spells, as well as any material components that lack a cost and aren't consumed by a spell. You gain this benefit in both your normal shape and your beast shape from Wild Shape.".

'natural recovery' ?= "Starting at 2nd level, you can regain some of your magical energy by sitting in meditation and communing with nature. During a short rest, you choose expended spell slots to recover. The spell slots can have a combined level that is equal to or less than half your druid level (rounded up), and none of the slots can be 6th level or higher. You can't use this feature again until you finish a long rest.
For example, when you are a 4th-level druid, you can recover up to two levels worth of spell slots. You can recover either a 2nd-level slot or two 1st-level slots.".

'circle spells' ?= "Your mystical connection to the land infuses you with the ability to cast certain spells. At 3rd, 5th, 7th, and 9th level you gain access to circle spells connected to the land where you became a druid. Choose that land - arctic, coast, desert, forest, grassland, mountain, or swamp - and consult the associated list of spells.
Once you gain access to a circle spell, you always have it prepared, and it doesn't count against the number of spells you can prepare each day. If you gain access to a spell that doesn’t appear on the druid spell list, the spell is nonetheless a druid spell for you.".

'land\'s stride' ?= "Starting at 6th level, moving through nonmagical difficult terrain costs you no extra movement. You can also pass through nonmagical plants without being slowed by them and without taking damage from them if they have thorns, spines, or a similar hazard.
In addition, you have advantage on saving throws against plants that are magically created or manipulated to impede movement, such those created by the entangle spell.".

'nature\'s ward' ?= "When you reach 10th level, you can't be charmed or frightened by elementals or fey, and you are immune to poison and disease.".

'nature\'s sanctuary' ?= "When you reach 14th level, creatures of the natural world sense your connection to nature and become hesitant to attack you. When a beast or plant creature attacks you, that creature must make a Wisdom saving throw against your druid spell save DC. On a failed save, the creature must choose a different target, or the attack automatically misses. On a successful save, the creature is immune to this effect for 24 hours. The creature is aware of this effect before it makes its attack against you.".
class_option(ranger).
hd_per_level(ranger, 1 d 10).
initial_class_base_hp(ranger, 10).
max_hp_per_level(ranger, 1 d 10).
caster(ranger, 1/2).
spellcasting_ability(ranger, wis).
choose_subclass_level(ranger:3).
class_saving_throw(ranger, str).
class_saving_throw(ranger, dex).
asi_level(ranger:L) :- default_asi_level(L).

class_skill_list(ranger, ['animal handling', athletics, insight,
                          investigation, nature, perception, stealth,
                          survival]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initial class features (don't get these multiclassing into ranger).
trait_options_source(^ranger, skill, wrap(skill),
                     2 unique_from class_skill(ranger)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 1 features (you get these when multiclassing into ranger).
traits_from_source(ranger >: 1,
                   [weapon(simple), weapon(martial),
                    armor(light), armor(medium), armor(shield)]).
trait_options_source(ranger >: 1, skill, wrap(skill),
                     class_skill(ranger)).
trait_options_source(ranger >: 1, 'favored enemy',
                     wrap(favored_enemy),
                     from_list(
                         [aberration, beast, celestial, construct,
                          dragon, elemental, fey, fiend, giant,
                          monstrosity, ooze, plant, undead
                         ])).
meta_todo('favored enemy', "select two races as favored enemies").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Features from leveling up.
trait_options_source(ranger >: L, 'natural explorer',
                     wrap(natural_explorer),
                     from_list(
                         [arctic, coast, desert, forest, grassland,
                          mountain, swamp
                         ])) :-
    member(L, [1, 6, 10]).
trait_options_source(ranger >: 2, 'fighting style',
                     wrap(fighting_style),
                     from_list([archery, defense, dueling,
                                'two-weapon fighting'])).

trait_source(ranger >: 3, 'primeval awareness').
multiclass_trait_source(ranger >: 5, extra_attack(1)).
trait_source(ranger >: 8, 'land\'s stride').
trait_source(ranger >: 10, 'hide in plain sight').
trait_source(ranger >: 14, vanish).
trait_source(ranger >: 18, 'feral senses').
trait_source(ranger >: 20, 'foe slayer').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Spellcasting.

known_spell(ranger, wis, always, [slot], Ritual, Spell) :-
    class_level(ranger:L),
    selected_at_class_level(ranger:L, spell, Spell),
    spell_property(Spell, ritual, Ritual).

% Learn new spells.
options_source(ranger >: 2, spell,
               2 unique_from learnable_proper_spell(ranger)).
options_source(ranger >: L, spell, learnable_proper_spell(ranger)) :-
    between(3, 19, L),
    odd(L).

% Replace old spells.
options_source(ranger >: L, replace(spell),
               selected_at_class_level(ranger:Prev, spell)) :-
    between(3, 20, L),
    Prev is L-1.
options(ranger >: L, replacing(spell, Name),
        learnable_proper_spell(ranger)) :-
    choice_member(ranger >: L, replace(spell), Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RANGER ARCHETYPES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hunter
subclass_option(ranger, hunter).
trait_options_source(ranger(hunter) >: 3, 'hunter\'s prey',
                     wrap(hunters_prey),
                     from_list(['colossus slayer', 'giant killer',
                                'horde breaker'])).
trait_options_source(ranger(hunter) >: 7, 'defensive tactics',
                     wrap(defensive_tactics),
                     ['escape the horde', 'multiattack defense', 'steel will']).
trait_options_source(ranger(hunter) >: 11, multiattack,
                     wrap(multiattack), [volley, 'whirlwind attack']).
trait_options_source(ranger(hunter) >: 15,
                     'hunter\s superior defense',
                     wrap(hunters_superior_defense),
                     [evasion, 'stand against the tide', 'uncanny dodge']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Beast master
meta_todo(ranger, "beast master").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DESCRIPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

favored_enemy(_) ?= "Beginning at 1st level, you have significant experience studying, tracking, hunting, and even talking to a certain type of enemy.
Choose a type of favored enemy: aberrations, beasts, celestials, constructs, dragons, elementals, fey, fiends, giants, monstrosities, oozes, plants, or undead. Alternatively, you can select two races of humanoid (such as gnolls and orcs) as favored enemies.
You have advantage on Wisdom (Survival) checks to track your favored enemies, as well as on Intelligence checks to recall information about them.
When you gain this feature, you also learn one language of your choice that is spoken by your favored enemies, if they speak one at all.
You choose one additional favored enemy, as well as an associated language, at 6th and 14th level. As you gain levels, your choices should reflect the types of monsters you have encountered on your adventures.".

natural_explorer(_) ?= "You are particularly familiar with one type of natural environment and are adept at traveling and surviving in such regions. Choose one type of favored terrain: arctic, coast, desert, forest, grassland, mountain, or swamp. When you make an Intelligence or Wisdom check related to your favored terrain, your proficiency bonus is doubled if you are using a skill that you're proficient in.
While traveling for an hour or more in your favored terrain, you gain the following benefits:

    Difficult terrain doesn't slow your group's travel.
    Your group can't become lost except by magical means.
    Even when you are engaged in another activity while traveling (such as foraging, navigating, or tracking), you remain alert to danger.
    If you are traveling alone, you can move stealthily at a normal pace.
    When you forage, you find twice as much food as you normally would.
    While tracking other creatures, you also learn their exact number, their sizes, and how long ago they passed through the area.

You choose additional favored terrain types at 6th and 10th level.".

'primeval awareness' ?= "Beginning at 3rd level, you can use your action and expend one ranger spell slot to focus your awareness on the region around you. For 1 minute per level of the spell slot you expend, you can sense whether the following types of creatures are present within 1 mile of you (or within up to 6 miles if you are in your favored terrain): aberrations, celestials, dragons, elementals, fey, fiends, and undead. This feature doesn't reveal the creatures' location or number.".

'extra attack' ?= "Beginning at 5th level, you can attack twice, instead of once, whenever you take the Attack action on your turn.".

'land\'s stride' ?= "Starting at 8th level, moving through nonmagical difficult terrain costs you no extra movement. You can also pass through nonmagical plants without being slowed by them and without taking damage from them if they have thorns, spines, or a similar hazard.
In addition, you have advantage on saving throws against plants that are magically created or manipulated to impede movement, such those created by the entangle spell.".

'hide in plain sight' ?= "Starting at 10th level, you can spend 1 minute creating camouflage for yourself. You must have access to fresh mud, dirt, plants, soot, and other naturally occurring materials with which to create your camouflage.
Once you are camouflaged in this way, you can try to hide by pressing yourself up against a solid surface, such as a tree or wall, that is at least as tall and wide as you are. You gain a +10 bonus to Dexterity (Stealth) checks as long as you remain there without moving or taking actions. Once you move or take an action or a reaction, you must camouflage yourself again to gain this benefit.".

vanish ?= "Starting at 14th level, you can use the Hide action as a bonus action on your turn. Also, you can't be tracked by nonmagical means, unless you choose to leave a trail.".

'feral senses' ?= "At 18th level, you gain preternatural senses that help you fight creatures you can't see. When you attack a creature you can't see, your inability to see it doesn't impose disadvantage on your attack rolls against it. You are also aware of the location of any invisible creature within 30 feet of you, provided that the creature isn't hidden from you and you aren't blinded or deafened.".

'foe slayer' ?= "At 20th level, you become an unparalleled hunter of your enemies. Once on each of your turns, you can add your Wisdom modifier to the attack roll or the damage roll of an attack you make against one of your favored enemies. You can choose to use this feature before or after the roll, but before any effects of the roll are applied.".

hunters_prey('colossus slayer') ?= "Your tenacity can wear down the most potent foes. When you hit a creature with a weapon attack, the creature takes an extra 1d8 damage if it's below its hit point maximum. You can deal this extra damage only once per turn.".
hunters_prey('giant killer') ?= " When a Large or larger creature within 5 feet of you hits or misses you with an attack, you can use your reaction to attack that creature immediately after its attack, provided that you can see the creature.".
hunters_prey('horde breaker') ?= " Once on each of your turns when you make a weapon attack, you can make another attack with the same weapon against a different creature that is within 5 feet of the original target and within range of your weapon. ".

defensive_tactics('escape the horde') ?= "Opportunity attacks against you are made with disadvantage.".
defensive_tactics('multiattack defense') ?= "When a creature hits you with an attack, you gain a +4 bonus to AC against all subsequent attacks made by that creature for the rest of the turn.".
defensive_tactics('steel will') ?= "You have advantage on saving throws against being frightened. ".

multiattack('volley') ?= "You can use your action to make a ranged attack against any number of creatures within 10 feet of a point you can see within your weapon's range. You must have ammunition for each target, as normal, and you make a separate attack roll for each target.".
multiattack('whirlwind attack') ?= "You can use your action to make a melee attack against any number of creatures within 5 feet of you, with a separate attack roll for each target. ".

superior_hunters_defense('evasion') ?= "When you are subjected to an effect, such as a red dragon's fiery breath or a lightning bolt spell, that allows you to make a Dexterity saving throw to take only half damage, you instead take no damage if you succeed on the saving throw, and only half damage if you fail.".
superior_hunters_defense('stand against the tide') ?= "When a hostile creature misses you with a melee attack, you can use your reaction to force that creature to repeat the same attack against another creature (other than itself) of your choice.".
superior_hunters_defense('uncanny dodge') ?= "When an attacker that you can see hits you with an attack, you can use your reaction to halve the attack's damage against you.".
test_char_level(
    drelf,
    1,
    [name(drelf),
     base_ability(str,10),
     base_ability(dex,13),
     base_ability(con,14),
     base_ability(int,12),
     base_ability(wis,16),
     base_ability(cha,9),

     has(quarterstaff),

     choice(init, 'base race', elf),
     choice(race(elf), subrace, 'high elf'),
     choice(race(elf('high elf')), cantrip, 'fire bolt'),
     choice(race(elf('high elf')), language, gnomish),

     choice(init, 'initial class', druid),
     choice(druid >: 1, cantrip, [shillelagh, druidcraft]),
     choice(^druid, skill, [arcana, 'animal handling'])
    ],
    [max_hp(10), % 8 (base druid) + 2 (con mod)
     ac(12), % 10 + 2 (dex mod)
     speed(30),
     ability(str,10),
     ability(dex,15), % elf bonus + 2
     ability(con,14),
     ability(int,13), % high elf bonus + 1
     ability(wis,16),
     ability(cha,9),
     
     % racial traits
     trait(race(elf), sense(darkvision)),
     trait(trait(sense('keen senses')), skill(perception)),
     trait(race(elf('high elf')), weapon(longsword)),
     trait(race(elf('high elf')), weapon(shortsword)),
     trait(race(elf('high elf')), weapon(shortbow)),
     trait(race(elf('high elf')), weapon(longbow)),
     
     \+ resource('wild shape', _, _),

     attack_variant(quarterstaff:shillelagh, feet(5), to_hit(5),
                    [damage(bludgeoning, 1 d 8 + 3)], [magical]),
     attack_variant(quarterstaff:twohanded, feet(5), to_hit(2),
                    [damage(bludgeoning, 1 d 8)], []),
     attack(quarterstaff, feet(5), to_hit(2), [damage(bludgeoning, 1 d 6)], _),

     known_spell(race(elf('high elf')), int, always, [], no, 'fire bolt'),
     known_spell(druid, wis, always, [], no, shillelagh),
     known_spell(druid, wis, always, [], no, druidcraft),

     known_spell(druid, wis, 'when prepared', [slot], no, 'animal friendship'),
     known_spell(druid, wis, 'when prepared', [slot], no, 'charm person'),
     known_spell(druid, wis, 'when prepared', [slot], no, goodberry),

     % don't know any circle spells yet
     \+ known_spell(druid, wis, always, [slot], no, barkskin),
     \+ known_spell(druid, wis, always, [slot], no, 'mirror image'),
     \+ known_spell(druid, wis, always, [slot], no, 'hold person')]).

test_char_level(
    drelf,
    2,
    [gain_level(2, druid, hp_avg),
     choice(druid >: 2,subclass,land),
     choice(druid(land) >: 2, cantrip, guidance)
    ],
    [max_hp(17), % 8 (base) + 1*5 (levelup) + 2*2 (con mod)

     resource('natural recovery', 'spell slot total', 1),
     resource('wild shape', 'wild shape', 2),
     trait(wild_shape([cr(1/4),hours(1),'no swimming speed','no flying speed'])),

     known_spell(druid, wis, always, [], no, guidance),
     % don't know any circle spells yet
     \+ known_spell(druid, wis, always, [slot], no, barkskin),
     \+ known_spell(druid, wis, always, [slot], no, 'mirror image'),
     \+ known_spell(druid, wis, always, [slot], no, 'hold person')
    ]).

test_char_level(
    drelf,
    3,
    [gain_level(3, druid, hp_avg),
     choice(druid(land) >: 3,'circle spells',forest)

    ],
    [resource('natural recovery', 'spell slot total', 2),
     resource('wild shape', 'wild shape', 2),
     trait(wild_shape([cr(1/4),hours(1),'no swimming speed','no flying speed'])),

     % know only forest circle spells
     known_spell(druid, wis, always, [slot], no, barkskin),
     \+ known_spell(druid, wis, always, [slot], no, 'mirror image'),
     \+ known_spell(druid, wis, always, [slot], no, 'hold person')
    ]).
test_char_level(
    chd,
    1,
    [name(chd),
     base_ability(str,13),
     base_ability(dex,10),
     base_ability(con,14),
     base_ability(int,12),
     base_ability(wis,16),
     base_ability(cha,9),

     choice(init, background, archaeologist),
     choice(background(archaeologist), language, elvish),

     choice(init, 'base race', dwarf),
     choice(race(dwarf), tool, smith),
     choice(race(dwarf), subrace, 'hill dwarf'),

     choice(init, 'initial class', cleric),
     choice(cleric >: 1, subclass, life),
     choice(^cleric, skill, [medicine, religion]),
     %choice(trait('blessings of knowledge'), skill, [arcana, nature]),
     %choice(trait('blessings of knowledge'), language, [giant, celestial]),
     choice(cleric >: 1, cantrip, ['sacred flame', 'spare the dying', guidance]),

     has('half plate' + 1),
     has(shield)],
    [ability(str,13),
     ability(dex,10),
     ability(con,16), % +2 from dwarf
     ability(int,12),
     ability(wis,17), % +1 from hill dwarf
     ability(cha,9),

     max_hp(12), % = 8 (cleric base hp) + 3 (con) + 1 (dwarven toughness)
     speed(25),
     ac(armor('half plate' + 1), 16, [shield(shield):2]), % = 15 (half plate) + 1 (half plate enchantment) + 2 (shield) + 0 (dex)
     trait('dwarven toughness'),
     trait(sense(darkvision)),
     trait(stonecunning),
     trait(armor(heavy)), % test a life domain feature

     findall(L-N, spell_slots(L,N), [1-2]),
     spell_save_dc(cleric, 13), % = 8 + 3 (wis) + 2 (prof bon)
     spell_attack_modifier(cleric, 5), % = 3 (wis) + 2 (prof bon)
     max_prepared_spells(cleric, 4), % = 3 (wis) + 1 (cleric level)

     attack('sacred flame', feet(60), saving_throw(13,dex), [damage(radiant,1 d 8)], []),

     known_spell(cleric, wis, always, [], no, 'sacred flame'),
     known_spell(cleric, wis, always, [], no, 'spare the dying'),
     known_spell(cleric, wis, always, [], no, guidance),

     % Domain spell are always prepared
     known_spell(cleric(life), wis, always, [slot], no, 'cure wounds'),
     known_spell(cleric(life), wis, always, [slot], no, bless),
     known_spell_property(cleric(life), 'cure wounds', effects,
                          [heal(1 d 8 + 6)]), % = 3 (wis) + 2 + 1 (spell level)

     % Clerics know all spells on their spell list; testing just a few here
     known_spell(cleric, wis, 'when prepared', [slot], no, bane),
     known_spell(cleric, wis, 'when prepared', [slot], no, sanctuary)
    ]
).

test_char_level(
    chd,
    2,
    [gain_level(2, cleric, hp_avg)],
    [max_hp(21), % = 8 (base) + 1*5 (lvlup) + 2*3 (con) + 2 (dwarven toughness)
     ac(armor('half plate' + 1), 16, [shield(shield):2]), % = 15 (half plate) + 1 (half plate enchantment) + 2 (shield) + 0 (dex)
     findall(L-N, spell_slots(L,N), [1-3]),
     resource('channel divinity', 'channel divinity', 1),
     max_prepared_spells(cleric, 5), % = 3 (wis) + 2 (cleric level)
     trait(channel_divinity('turn undead'))
    ]).

test_char_level(
    chd,
    3,
    [gain_level(3, cleric, hp_avg)],
    [max_hp(30), % = 8 (base) + 2*5 (lvlup) + 3*3 (con) + 3 (dwarven toughness)
     ac(armor('half plate' + 1), 16, [shield(shield):2]), % = 15 (half plate) + 1 (half plate enchantment) + 2 (shield) + 0 (dex)
     findall(L-N, spell_slots(L,N), [1-4, 2-2]),
     max_prepared_spells(cleric, 6), % = 3 (wis) + 3 (cleric level)

     % Test that we have access to some lvl 2 spells.
     known_spell(cleric, wis, 'when prepared', [slot], no, 'continual flame'),
     known_spell(cleric, wis, 'when prepared', [slot], no, 'hold person'),

     % Domain spells are always prepared.
     known_spell(cleric(life), wis, always, [slot], no, 'lesser restoration'),
     known_spell(cleric(life), wis, always, [slot], no, 'spiritual weapon')
    ]).

test_char_level(
    chd,
    4,
    [gain_level(4, cleric, hp_avg),
     choice(cleric >: 4, 'asi or feat', [wis, str]),
     choice(cleric >: 4, cantrip, mending),
     prepare_spell(cleric, 'cure wounds'),
     prepare_spell(cleric, 'prayer of healing')
    ],
    [max_hp(39), % = 8 (base) + 3*5 (lvlup) + 4*3 (con) + 4 (dwarven toughness)
     ac(armor('half plate' + 1), 16, [shield(shield):2]), % = 15 (half plate) + 1 (half plate enchantment) + 2 (shield) + 0 (dex)
     ability(str,14), % +1 from asi
     ability(dex,10),
     ability(con,16), % +2 from dwarf
     ability(int,12),
     ability(wis,18), % +1 from hill dwarf, + 1 from asi
     ability(cha,9),
     findall(L-N, spell_slots(L,N), [1-4, 2-3]),
     max_prepared_spells(cleric, 8), % = 4 (wis) + 4 (cleric level)
     spell_save_dc(cleric, 14), % = 8 + 4 (wis) + 2 (prof bon)
     spell_attack_modifier(cleric, 6), % = 4 (wis) + 2 (prof bon)
     attack('sacred flame', feet(60), saving_throw(14,dex), [damage(radiant,1 d 8)], []),
     known_spell(cleric, wis, always, [], no, mending),
     known_spell_property(cleric(life), 'cure wounds', effects,
                          [heal(1 d 8 + 7)]) % = 4 (wis) + 2 + 1 (spell level)
    ]).

test_char_level(
    chd,
    5,
    [gain_level(5, cleric, hp_avg)],
    [spell_save_dc(cleric, 15), % = 8 + 4 (wis) + 3 (prof bon)
     spell_attack_modifier(cleric, 7), % = 4 (wis) + 3 (prof bon)
     trait(destroy_undead(cr(1/2))),
     attack('sacred flame', feet(60), saving_throw(15,dex), [damage(radiant,2 d 8)], [])
    ]).

test_char_level(
    chd,
    6,
    [gain_level(6, cleric, hp_avg)],
    [known_spell_property(cleric(life), 'cure wounds', effects,
                          [heal(1 d 8 + 7), (target=other) -> self_heal(3)]),
     known_spell_property(cleric, 'prayer of healing', effects,
                          [heal(2 d 8 + 8) upto "6 creatures", (target=other) -> self_heal(4)])
    ]).

test_char_level(chd, L, [gain_level(L, cleric, hp_avg)], []) :-
    between(7,16,L).

test_char_level(
    chd,
    17,
    [gain_level(17, cleric, hp_avg)],
    [known_spell_property(cleric(life), 'cure wounds', effects,
                          [heal(15), % = 8 (supreme healing) + 4 (wis) + 2 + 1 (spell lvl)
                           (target=other) -> self_heal(3)]), 
     known_spell_property(cleric, 'prayer of healing', effects,
                          [heal(24) upto "6 creatures",
                           (target=other) -> self_heal(4)])]).

%test_char_level(
%    chd,
%    7,
%    [gain_level(7, cleric, hp_avg)],
%    []).
%
%test_char_level(
%    chd,
%    8,
%    [gain_level(8, cleric, hp_avg),
%     ignore(cleric >: 8, 'asi or feat')
%    ],
%    []).
%
%test_char_level(
%    chd,
%    9,
%    [gain_level(9, cleric, hp_avg)],
%    []).
%
%test_char_level(
%    chd,
%    10,
%    [gain_level(10, cleric, hp_avg),
%     ignore(cleric >: 10, cantrip)
%    ],
%    []).
    
%
%gain_level(3, cleric, hp_avg).
%
%gain_level(4, cleric, hp_avg).
%choice(cleric >: 4, 'asi or feat', [wis,str]).
%
%gain_level(5, cleric, hp_avg).
%gain_level(6, cleric, hp_avg).
%gain_level(7, cleric, hp_avg).
%
%prepare_spell(cleric, banishment).
test_char_level(
    monkbarb,
    1,
    [name(monkbarb),
     base_ability(wis,20),
     base_ability(con,18),
     base_ability(dex,16),

     base_ability(str,10),
     base_ability(int,10),
     base_ability(cha,10),

     has(shield + 1),

     choice(init, 'initial class', monk)
    ],
    [ac_formula(unarmored, 10 + dex + shield),
     ac_formula(unarmored_defense(monk), 10 + dex + wis),
     ac(unarmored, 13, []),
     ac(unarmored_defense(monk), 18, [])
    ]).

test_char_level(
    monkbarb,
    2,
    [gain_level(2, barbarian, hp_avg)],
    [ac_formula(unarmored, 10 + dex + shield),
     ac_formula(unarmored_defense(monk), 10 + dex + wis),
     %ac_formula(unarmored_defense(barbarian), 10 + dex + con + shield),
     ac(unarmored, 13, [shield(shield+1) : 3]),
     ac(unarmored_defense(monk), 18, [])
     %ac(unarmored_defense(barbarian), 20) % 10 + 3 + 4 + 3
    ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_char_level(
    barbmonk,
    1,
    [name(barbmonk),
     base_ability(wis,20),
     base_ability(con,18),
     base_ability(dex,16),

     base_ability(str,10),
     base_ability(int,10),
     base_ability(cha,10),

     has(shield + 1),

     choice(init, 'initial class', barbarian)
    ],
    []).
test_char_level(
    barbmonk,
    2,
    [gain_level(2, monk, hp_avg)],
    [ac_formula(unarmored, 10 + dex + shield),
     %ac_formula(unarmored_defense(monk), 10 + dex + wis),
     ac_formula(unarmored_defense(barbarian), 10 + dex + con + shield),
     ac(unarmored, 13, [shield(shield+1) : 3]),
     %ac(unarmored_defense(monk), 18, [])
     ac(unarmored_defense(barbarian), 17, [shield(shield+1) : 3]) % 10 + 3 + 4
    ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_char_level(
    dracsorcbarb,
    1,
    [name(dracsorcbarb),
     base_ability(wis,20),
     base_ability(con,18),
     base_ability(dex,16),

     base_ability(str,10),
     base_ability(int,10),
     base_ability(cha,10),

     has(shield + 1),

     choice(init, 'initial class', sorcerer),
     choice(sorcerer >: 1, subclass, 'draconic bloodline')
    ],
    [ac_formula(trait('draconic resilience'), 13 + dex + shield),
     ac(trait('draconic resilience'), 16, []) % no shield proficiency
    ]).
test_char_level(
    dracsorcbarb,
    2,
    [gain_level(2, barbarian, hp_avg)],
    [ac_formula(trait('draconic resilience'), 13 + dex + shield),
     ac(trait('draconic resilience'), 16, [shield(shield+1) : 3]), % with shield proficiency
     ac_formula(unarmored_defense(barbarian), 10 + dex + con + shield),
     ac(unarmored_defense(barbarian), 17, [shield(shield+1) : 3])
    ]).
test_char_level(
    testpaladin,
    1,
    [choice(init, 'initial class', paladin),
     base_ability(str, 18),
     base_ability(dex, 10),
     base_ability(con, 16),
     base_ability(wis, 12),
     base_ability(int, 8),
     base_ability(cha, 16),
     has(greataxe+1),
     name(testpaladin)],
    [\+ spell_slots(_,_),
     attack(greataxe+1, feet(5), to_hit(7), [damage(slashing, 1 d 12 + 5)], [heavy, twohanded]),
     known_spell(race(tiefling), thaumaturgy),
     \+ known_spell(paladin, _)]
).

test_char_level(
    testpaladin,
    2,
    [gain_level(2, paladin, hp_avg),
     choice(paladin >: 2, 'fighting style', 'great weapon fighting')],
    [attack(greataxe+1, feet(5), to_hit(7), [damage(slashing, 1 d 12 + 5)],
            [heavy, twohanded, "may reroll 1 or 2 on a damage die"]),
     max_prepared_spells(paladin, 4), % cha mod + half paladin level
     \+ (known_spell(paladin, Spell), spell_property(Spell, level, 0)), % don't know cantrips as paladin
     \+ known_spell(_, 'find steed'), % don't know higher level paladin spells yet
     known_spell(paladin, 'cure wounds')] % know level-appropriate paladin spells
).

test_char_level(
    testpaladin,
    3,
    [gain_level(3, paladin, hp_avg),
     choice(paladin >: 3, subclass, devotion)],
    [known_spell(paladin(devotion), cha, always, [slot], no, sanctuary),
     known_spell(paladin(devotion), cha, always, [slot], no, 'protection from evil and good'),  
     \+ known_spell(_, 'lesser restoration'),  
     known_spell(race(tiefling), 'hellish rebuke'),
     resource('channel divinity', 'channel divinity', 1),
     trait(channel_divinity('sacred weapon')),
     trait(channel_divinity('turn the unholy'))]
).

test_char_level(testpaladin, L, [gain_level(L, paladin, hp_avg)], []) :-
    between(4, 10, L).

test_char_level(
    testpaladin,
    11,
    [gain_level(11, paladin, hp_avg)],
    [attack(greataxe+1, feet(5), to_hit(9),
            [damage(slashing, 1 d 12 + 5), damage(radiant, 1 d 8)], _)]
).
test_char_level(
    monk,
    1,
    [choice(init, 'base race', 'half-orc'),
     choice(init, 'initial class', monk),
     has(club),
     has(quarterstaff),

     base_ability(str,14),
     base_ability(dex,18),
     base_ability(con,12),
     base_ability(int,10),
     base_ability(wis,16),
     base_ability(cha,8),

     name(monk)],
    [speed(30),
     attack(club, feet(5), to_hit(6), [damage(bludgeoning, 1 d 4 + 4)],
            [light, "unarmed strike as bonus action"]),
     attack(unarmed, feet(5), to_hit(6), [damage(bludgeoning, 1 d 4 + 4)],
            ["unarmed strike as bonus action"]),
     attack(quarterstaff, feet(5), to_hit(6), [damage(bludgeoning, 1 d 6 + 4)],
            [versatile(1 d 8), "unarmed strike as bonus action"]),
     attack_variant(quarterstaff:twohanded, feet(5), to_hit(6), [damage(bludgeoning, 1 d 8 + 4)],
            []),
     ac(unarmored, 14, []),
     ac(unarmored_defense(monk), 17, [])]
).

test_char_level(
    monk,
    2,
    [gain_level(2, fighter, hp_avg), % a level of fighter to get shield proficiency
     has(shield) % shouldn't show up in unarmored defense
    ],
    [attack(club, feet(5), to_hit(6), [damage(bludgeoning, 1 d 4 + 4)],
            [light, "unarmed strike as bonus action"]),
     attack(unarmed, feet(5), to_hit(6), [damage(bludgeoning, 1 d 4 + 4)],
            ["unarmed strike as bonus action"]),
     attack(quarterstaff, feet(5), to_hit(6), [damage(bludgeoning, 1 d 6 + 4)],
            [versatile(1 d 8), "unarmed strike as bonus action"]),
     attack_variant(quarterstaff:twohanded, feet(5), to_hit(6), [damage(bludgeoning, 1 d 8 + 4)],
            []),
     ac(unarmored, 14, [shield(shield):2]),
     ac(unarmored_defense(monk), 17, [])]
).

test_char_level(
    monk,
    3,
    [gain_level(3, monk, hp_avg) % a level of fighter to get shield proficiency
    ],
    [resource(ki, 'ki points', 2),
     speed(40),
     attack(club, feet(5), to_hit(6), [damage(bludgeoning, 1 d 4 + 4)],
            [light, "unarmed strike as bonus action"]),
     attack(unarmed, feet(5), to_hit(6), [damage(bludgeoning, 1 d 4 + 4)],
            ["unarmed strike as bonus action"]),
     attack(quarterstaff, feet(5), to_hit(6), [damage(bludgeoning, 1 d 6 + 4)],
            [versatile(1 d 8), "unarmed strike as bonus action"]),
     attack_variant(quarterstaff:twohanded, feet(5), to_hit(6), [damage(bludgeoning, 1 d 8 + 4)],
            []),
     ac(unarmored, 14, [shield(shield):2]),
     ac(unarmored_defense(monk), 17, [])]
).

test_char_level(
    monk,
    4,
    [gain_level(4, monk, hp_avg)],
    [resource(ki, 'ki points', 3),
     speed(40),
     attack(club, feet(5), to_hit(6), [damage(bludgeoning, 1 d 4 + 4)],
            [light, "unarmed strike as bonus action"]),
     attack(unarmed, feet(5), to_hit(6), [damage(bludgeoning, 1 d 4 + 4)],
            ["unarmed strike as bonus action"]),
     attack(quarterstaff, feet(5), to_hit(6), [damage(bludgeoning, 1 d 6 + 4)],
            [versatile(1 d 8), "unarmed strike as bonus action"]),
     attack_variant(quarterstaff:twohanded, feet(5), to_hit(6), [damage(bludgeoning, 1 d 8 + 4)],
            []),
     ac(unarmored, 14, [shield(shield):2]),
     ac(unarmored_defense(monk), 17, [])]
).

test_char_level(
    monk,
    5,
    [gain_level(5, monk, hp_avg),
     choice(monk >: 4, 'asi or feat', alert)],
    [feat(alert),
     initiative(9) % dex mod (+4) + alert feat (+5)
    ]).

test_char_level(
    monk, 6, [gain_level(6, monk, hp_avg)],
    [ gain_level(6, monk, hp_avg),
      resource(ki, 'ki points', 5),
      speed(40),
      attack(club, feet(5), to_hit(7), [damage(bludgeoning, 1 d 6 + 4)],
             [light, "unarmed strike as bonus action", "attack 2x"]),
      attack(unarmed, feet(5), to_hit(7), [damage(bludgeoning, 1 d 6 + 4)],
             ["unarmed strike as bonus action", "attack 2x"]),
      attack(quarterstaff, feet(5), to_hit(7), [damage(bludgeoning, 1 d 6 + 4)],
             [versatile(1 d 8), "unarmed strike as bonus action", "attack 2x"]),
      attack_variant(quarterstaff:twohanded, feet(5), to_hit(7),
                     [damage(bludgeoning, 1 d 8 + 4)],
                     []),
      ac(unarmored, 14, [shield(shield):2]),
      ac(unarmored_defense(monk), 17, [])
    ]).

test_char_level(monk, L, [gain_level(L, monk, hp_avg)], []) :-
    between(7,17,L).

% monk level 17
test_char_level(
    monk, 18, [gain_level(18, monk, hp_avg)],
    [ gain_level(18, monk, hp_avg),
      resource(ki, 'ki points', 17),
      speed(55),
      attack(club, feet(5), to_hit(10), [damage(bludgeoning, 1 d 10 + 4)],
             [light, "unarmed strike as bonus action", "attack 2x"]),
      attack(unarmed, feet(5), to_hit(10), [damage(bludgeoning, 1 d 10 + 4)],
             ["unarmed strike as bonus action", magical, "attack 2x"]),
      attack(quarterstaff, feet(5), to_hit(10), [damage(bludgeoning, 1 d 10 + 4)],
             [versatile(1 d 8), "unarmed strike as bonus action", "attack 2x"]),
      attack_variant(quarterstaff:twohanded, feet(5), to_hit(10),
                     [damage(bludgeoning, 1 d 8 + 4)],
                     []),
      ac(unarmored, 14, [shield(shield):2]),
      ac(unarmored_defense(monk), 17, [])
    ]).
test_char_level(
    'human ranger', 1,
    [name('human ranger'),
     base_ability(str,9),
     base_ability(dex,15),
     base_ability(con,11),
     base_ability(int,11),
     base_ability(wis,14),
     base_ability(cha,8),
     choice(init, 'base race', human),
     choice(race(human), subrace, variant),
     choice(race(human), language, 'deep speech'),
     choice(race(human(variant)), skill, acrobatics),
     choice(race(human(variant)), feat, 'fey touched'),
     choice(feat('fey touched'), asi, wis + 1),
     choice(feat('fey touched'), spell, sleep),
     choice(race(human(variant)), asi, [dex + 1, wis + 1]),
     choice(init, 'initial class', ranger)],
    [max_hp(10),
     ability(str,9),
     ability(dex,16),
     ability(con,11),
     ability(int,11),
     ability(wis,16),
     ability(cha,8),
     \+ spell_slots(_,_),
     known_spell(feat('fey touched'), sleep),
     known_spell(feat('fey touched'), 'misty step')]
).

test_char_level(
    'human ranger',
    2,
    [gain_level(2, ranger, hp_avg),
     choice(ranger >: 2, 'fighting style', archery),
     choice(ranger >: 2, spell, ['hunter\'s mark', 'cure wounds'])],
    [max_hp(16),
     findall(L-N, spell_slots(L,N), [1-2]),
     known_spell(ranger, 'hunter\'s mark'),
     known_spell(ranger, 'cure wounds')]
).
test_char_level(
    extra_attack,
    1,
    [choice(init, 'base race', 'human'),
     choice(init, 'initial class', monk),
     base_ability(str,14),
     base_ability(dex,18),
     base_ability(con,12),
     base_ability(int,10),
     base_ability(wis,16),
     base_ability(cha,8),
     name(extra_attack)],
    []).

% Get to level 5 monk, checkthat we get the extra attack feature.
test_char_level(extra_attack, L, [gain_level(L, monk, hp_avg)], []) :-
    between(2, 4, L).
test_char_level(
    extra_attack,
    5,
    [gain_level(5, monk, hp_avg)],
    [trait(monk >: 5, extra_attack(1))]).

% Add level 5 fighter multiclass, verify that the extra attacks don't stack.
test_char_level(extra_attack, L, [gain_level(L, fighter, hp_avg)], []) :-
    between(6, 9, L).
test_char_level(
    extra_attack,
    10,
    [gain_level(10, fighter, hp_avg)],
    [findall(Origin, trait(Origin, extra_attack(1)), [_]) % Make sure there's only one,
                                                          % doesn't matter which.
    ]).

% Add 6 levels of fighter to go to level 11 fighter (gain 2 extra attacks)
test_char_level(extra_attack, L, [gain_level(L, fighter, hp_avg)], []) :-
    between(11, 15, L).
test_char_level(
    extra_attack,
    16,
    [gain_level(16, fighter, hp_avg)],
    % This time we need to make sure that it's specifically the fighter trait that remains.
    [findall(Origin-N,
             trait(Origin, extra_attack(N)),
             [(fighter >: _) - 2]) 
    ]).
test_char_level(
    mmadept,
    1,
    [name(mmadept),
     base_ability(str,10),
     base_ability(dex,10),
     base_ability(con,10),
     base_ability(int,10),
     base_ability(wis,10),
     base_ability(cha,10),

     choice(init, 'base race', human),
     choice(race(human), subrace, standard),

     choice(init, 'initial class', wizard)],
    []
).

test_char_level(mmadept, L, [gain_level(L, wizard, hp_avg)], []) :-
    between(2, 3, L).

test_char_level(
    mmadept,
    4,

    [gain_level(4, wizard, hp_avg),
     choice(wizard >: 4, 'asi or feat', 'metamagic adept'),
     choice(feat('metamagic adept'), metamagic, ['careful spell', 'distant spell'])],

    [\+ options(feat('metamagic adept') at 4, replace(metamagic), _),
     selected_at_character_level(feat('metamagic adept'), 4, metamagic, 'careful spell'),
     selected_at_character_level(feat('metamagic adept'), 4, metamagic, 'distant spell'),
     trait(feat('metamagic adept'), metamagic('careful spell')),
     trait(feat('metamagic adept'), metamagic('distant spell')),
     resource(metamagic, 'adept sorcery point', 2)]
).

test_char_level(mmadept, L, [gain_level(L, wizard, hp_avg)], []) :-
    between(5, 7, L).

test_char_level(
    mmadept,
    8,

    [gain_level(8, wizard, hp_avg),
     choice(feat('metamagic adept') at 8, replace(metamagic), 'careful spell'),
     choice(feat('metamagic adept') at 8, replacing(metamagic, 'careful spell'), 'empowered spell')
    ],

    [\+ trait(feat('metamagic adept'), metamagic('careful spell')),
     trait(feat('metamagic adept'), metamagic('empowered spell')),
     trait(feat('metamagic adept'), metamagic('distant spell'))]
).

test_char_level(mmadept, L, [gain_level(L, wizard, hp_avg)], []) :-
    between(9, 11, L).

% Check that reintroducing a choice that was replaced earlier works.
test_char_level(
    mmadept,
    12,

    [gain_level(12, wizard, hp_avg),
     choice(feat('metamagic adept') at 12, replace(metamagic), 'distant spell'),
     choice(feat('metamagic adept') at 12, replacing(metamagic, 'distant spell'), 'careful spell')
    ],

    [\+ trait(feat('metamagic adept'), metamagic('distant spell')),
     trait(feat('metamagic adept'), metamagic('empowered spell')),
     trait(feat('metamagic adept'), metamagic('careful spell'))]
).

% test_char_level(?Name, ?Level, ?Facts, ?Expectations)
test_char_level(_,_,_,_) :- false.

test_character(Name) :-
    unload_current_character,
    findall(Level-Facts-Expectations,
            test_char_level(Name, Level, Facts, Expectations),
            UnsortedScenario),
    sort(1, @<, UnsortedScenario, TestScenario),
    forall(member(Level-Facts-Expectations, TestScenario),
           test_character_level(Name, Level, Facts, Expectations)).
test_character_level(Name, Level, Facts, Expectations) :-
    write("# "), write(Name), write(" lvl "), writeln(Level),
    forall(member(Fact,Facts), assert(Fact)),
    forall(member(E,Expectations), test_expectation(E)),
    findall(Problem, (problem(Problem), write('Problem: '), writeln(Problem)), []).
    %findall(Todo, (todo(Todo), write('Todo: '), writeln(Todo)), _).
test_expectation(Expectation) :-
    call(Expectation)
    -> true
    ;  (write('Failed: '), write(Expectation), false).

tc(Name) :-
    retractall(most_recent_test_character(_)),
    assert(most_recent_test_character(Name)),
    test_character(Name).

tc :-
    most_recent_test_character(Name),
    test_character(Name).

tac :-
    findall(Char, test_char_level(Char,_,_,_), UChars),
    sort(0, @<, UChars, Chars),
    forall(member(Char, Chars),
           (write("Testing "), writeln(Char), test_character(Char))).
feat_option(_,_) :- false.

feat(Feat) :- trait(feat(Feat)).
selectable_feat_option(Feat) :-
    feat_option(Feat).
selectable_feat_option(Feat) :-
    feat_option(Cond, Feat),
    call(Cond).

feat_option(alert).
feat(alert) ?= "Always on the lookout for danger, you gain the following benefits:
    - You can’t be surprised while you are conscious.
    - You gain a +5 bonus to initiative.
    - Other creatures don’t gain advantage on attack rolls against you as a result of being hidden from you.".
bonus_source(trait(feat(alert)), init + 5).

feat_option(durable).
feat(durable) ?= "Hardy and resilient, you gain the following benefits:
    Increase your Constitution score by 1, to a maximum of 20.
    When you roll a Hit Die to regain hit points, the minimum number of hit points you regain from
    the roll equals twice your Constitution modifier (minimum of 2).".
bonus_source(feat(durable), con+1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PLAYER HANDBOOK (NOT SRD)                                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
feat_option(lucky).
feat(lucky) ?= "Three times per long rest: reroll a die (own die or attack roll against you) after the roll, but before outcome is determined. Pick whichever outcome you prefer.".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TASHA'S CAULDRON OF EVERYTHING                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% FEY TOUCHED
%%%%%%%%%%%%%
feat_option('fey touched').
bonus_options_source(feat('fey touched'), asi, id,
                     from_list([int+1, wis+1, cha+1])).
%spellcasting_ability(feat('fey touched'), Abi) :-
%    choice(feat('fey touched'), asi, Abi+1).
options_source(feat('fey touched'), spell, fey_touched_spell).
known_spell(feat('fey touched'), Abi, always, [per_rest(long,1)], no, Spell) :-
    choice(feat('fey touched'), asi, Abi),
    (Spell = 'misty step' ; choice(feat('fey touched'), spell, Spell)).
fey_touched_spell(Spell) :-
    spell_data(Spell, Data),
    Data.get(level) = 1, Data.get(school) = School,
    member(School, [enchantment, divination]).
feat('fey touched') ?= "Your exposure to the Feywild's magic has changed you, granting you the following benefits:

Increase your Intelligence, Wisdom, or Charisma score by 1, to a maximum of 20.

You learn the Misty Step spell and one 1st-level spell of your choice. The 1st-level spell must be from the Divination or Enchantment school of magic. You can cast each of these spells without expending a spell slot. Once you cast either of these spells in this way, you can’t cast that spell in this way again until you finish a long rest. You can also cast these spells using spell slots you have of the appropriate level. The spells’ spellcasting ability is the ability increased by this feat.".

% METAMAGIC ADEPT
%%%%%%%%%%%%%%%%%
feat_option('metamagic adept').

% Pick two metamagic options.
options_source(feat('metamagic adept'), metamagic, 2 unique_from metamagic_option).
   
% One option can be replaced any time the character gets an ASI or feat from their class
% (except on the level this feat is picked).
replace_at_character_level(feat('metamagic adept'), CharLevel, metamagic, 1, metamagic_option) :-
    options(Class >: ClassLevel, 'asi or feat', _),
    origin_level(Class >: ClassLevel, CharLevel),
    trait(Origin, feat('metamagic adept')),
    \+ origin_level(Origin, CharLevel).

% Actually add the chosen metamagic options as traits.
trait(feat('metamagic adept'), metamagic(MetaMagic)) :-
    selected_at_current_level(feat('metamagic adept'), metamagic, MetaMagic).

% Metamagic adept grants two sorcery points which can only be used for metamagic, not for
% conversion into spell slots, so they need to be tracked as a separate resource from regular
% sorcery points.
resource(metamagic, 'adept sorcery point', 2) :-
    feat('metamagic adept').
on_rest(long, 'adept sorcery point', 'full restore').

% Documentation.
feat('metamagic adept') ?= "You’ve learned how to exert your will on your spells to alter how they function:

    - You learn two Metamagic options of your choice from the sorcerer class. You can use only one Metamagic option on a spell when you cast it, unless the option says otherwise. Whenever you reach a level that grants the Ability Score Improvement feature, you can replace one of these Metamagic options with another one from the sorcerer class.

    - You gain 2 sorcery points to spend on Metamagic (these points are added to any sorcery points you have from another source but can be used only on Metamagic). You regain all spent sorcery points when you finish a long rest.".
:- use_module(library(http/json)).

raw_data(SpellName, Data) :-
    open('inference/resources/spells/srd.json', read, In),
    json:json_read_dict(In, SpellDicts),
    member(Data, SpellDicts),
    to_lowercase_atom(Data.name, SpellName).

register_srd_spells :-
    open('inference/resources/spells/srd.json', read, In),
    json:json_read_dict(In, SpellDicts),
    maplist(register_spell, SpellDicts).

register_spell(Data) :-
    to_lowercase_atom(Data.name, Name),
    spell_data_higher_level(Data.higher_level, HigherLevel),
    string_to_atom(Data.school.index, School),
    spell_data_components(Data, Components),
    parse_range(Data.range, Range),
    yesno(Data.concentration, Concentration),
    yesno(Data.ritual, Ritual),
    maplist(spell_data_class, Data.classes, Classes),
    spell_data_damage_with_cantrip_scaling(Data, DamageCantripScaling),
    spell_data_damage_at_slot_level(Data, DamageSlotLevel),
    spell_data_aoe(Data, AOE),
    spell_data_dc(Data, DC),
    spell_data_attack_type(Data, AttackType),
    get_or_default(Data, material, false, Material),
    assert(spell_auto_data(Name,
                           properties{ level: Data.level,
                                       higher_level: HigherLevel,          
                                       school: School,
                                       components: Components,
                                       range: Range,
                                       casting_time: Data.casting_time,
                                       duration: Data.duration,
                                       concentration: Concentration,
                                       ritual: Ritual,
                                       desc: Data.desc,
                                       classes: Classes,
                                       damage_with_cantrip_scaling: DamageCantripScaling,
                                       damage_at_slot_level: DamageSlotLevel,
                                       area_of_effect: AOE,
                                       dc: DC,
                                       attack_type: AttackType,
                                       material: Material
                                     })).

spell_auto_property(Spell, Field, Value) :-
    spell_auto_data(Spell, Data),
    Data.get(Field) = Value,
    Value \= false.

spell_data_class(Dict, Class) :-
    to_lowercase_atom(Dict.index, Class).

spell_data_higher_level([], no).
spell_data_higher_level([Desc], Desc).

spell_data_components(Data, Components) :-
    maplist(spell_data_component(Data), Data.components, Components).

spell_data_component(Data, "M", m(Data.material)) :- !.
spell_data_component(_, Component, Atom) :-
    to_lowercase_atom(Component, Atom).

to_lowercase_atom(Str, Atom) :-
    string_lower(Str, Lower),
    string_to_atom(Lower, Atom).

yesno(true, yes).
yesno(false, no).

parse_range("Self", self) :- !.
parse_range("Touch", touch) :- !.
parse_range(Str, feet(Feet)) :-
    member(Suffix, [" feet", " foot"]),
    string_concat(FeetStr, Suffix, Str),
    number_string(Feet, FeetStr),
    !.
parse_range(Str, miles(Miles)) :-
    member(Suffix, [" mile", " miles"]),
    string_concat(MilesStr, Suffix, Str),
    number_string(Miles, MilesStr),
    !.
parse_range(Str, Atom) :-
    to_lowercase_atom(Str, Atom).

wrap_list(List, List) :- is_list(List), !.
wrap_list(X, [X]) :- \+ is_list(X).

spell_data_damage_with_cantrip_scaling(Data, damage(Type, BaseRoll)) :-
    Data.get(damage) = _{ damage_at_character_level: DmgScalingDict, 
                          damage_type: DmgType },
    to_lowercase_atom(DmgType.get(name), Type),
    term_string(BaseRoll, DmgScalingDict.get('1')),
    !.
spell_data_damage_with_cantrip_scaling(_, false) :- !.

spell_data_damage_at_slot_level(Data, ParsedDict) :-
    wrap_list(Data.get(damage), DamageDicts),
    maplist(damage_at_slot_level_term, DamageDicts, Terms),
    merge_damage_dicts(Terms, ParsedDict),
    !.
spell_data_damage_at_slot_level(_, []).

damage_at_slot_level_term(_{ damage_type: TypeDict,
                             damage_at_slot_level: ScalingDict
                           },
                          ParsedDict) :-
    to_lowercase_atom(TypeDict.get(name), Type),
    dict_pairs(ScalingDict, _, Pairs),
    findall(Lvl-damage(Type, Roll),
            (member(LvlAtom-RollStr,Pairs), atom_number(LvlAtom,Lvl), term_string(Roll,RollStr,[variable_names([])])), % TODO: parse and handle "+ MOD"
            NewPairs),
    dict_pairs(ParsedDict, _, NewPairs).

merge_damage_dicts([D|Ds], Out) :-
    merge_damage_dicts(Ds, DRest),
    merge_damage_dicts(D, DRest, Out).
merge_damage_dicts([D], D).
merge_damage_dicts(D1, D2, Out) :-
    dict_pairs(D1, _, Pairs1), dict_pairs(D2, _, Pairs2),
    merge_damage_lists(Pairs1, Pairs2, NewPairs),
    dict_pairs(Out, _, NewPairs).
merge_damage_lists([L-Dmg1|R1], [L-Dmg2|R2], [L-(Dmg1+Dmg2)|R]) :-
    merge_damage_lists(R1, R2, R).
merge_damage_lists([], [], []).

% in(20 ft sphere):
spell_data_aoe(Data, Size ft Type) :-
    Data.get(area_of_effect) = _{type: TypeStr, size: Size},
    !,
    string_to_atom(TypeStr, Type).
spell_data_aoe(_, false).

spell_data_dc(Data, Abi else Succ) :-
    Data.get(dc) = DCDict,
    !,
    DCDict.get(dc_type).get(index) = AbiStr,
    string_to_atom(AbiStr, Abi),
    DCDict.get(dc_success) = SuccStr,
    string_to_atom(SuccStr, Succ).
spell_data_dc(_, false).

spell_data_attack_type(Data, Type) :-
    Data.get(attack_type) = Str,
    string_to_atom(Str, Type). 
spell_data_attack_type(_, false).

:- \+ spell_auto_data(_,_) -> register_srd_spells; true.
% Standalone script to convert the json spell list from
% https://github.com/jcquinlan/dnd-spells/blob/master/spells.json
% into prolog facts.

:- use_module(library(http/json)).

read_spells(Spells) :-
    open('resources/spells.json', read, In),
    json:json_read_dict(In, SpellsJson),
    maplist(transform_dict, SpellsJson, Spells),
    close(In).

write_spells(Spells) :-
    open('spell_auto_data.pl', write, Out),
    writeln(Out, "% Auto generated, do not edit directly.\n"),
    maplist(write_spell(Out), Spells).

write_spell(Out, Spell) :-
    write_term(Out, Spell, [quoted(true), nl(true), fullstop(true)]),
    writeln(Out, "").
    
transform_dict(In, spell_auto_data(Name, properties{ level:Level,
                                                     higher_level:HigherLevel,
                                                     school:School,
                                                     components:Components,
                                                     range:Range,
                                                     casting_time:In.casting_time,
                                                     duration:In.duration,
                                                     concentration:In.concentration,
                                                     ritual:Ritual,
                                                     desc:In.desc,
                                                     classes:Classes
                                                   })) :-
    to_lowercase_atom(In.name, Name),
    parse_level(In.level, Level),
    get_or_default(In, higher_level, no, HigherLevel),
    to_lowercase_atom(In.school, School),
    get_or_default(In, material, "err: no material", Material),
    parse_components(In.components, Material, Components),
    parse_range(In.range, Range),
    string_to_atom(In.ritual, Ritual),
    parse_classes(In.class, Classes).

to_lowercase_atom(Str, Atom) :-
    string_lower(Str, Lower),
    string_to_atom(Lower, Atom).

get_or_default(Dict, Field, _, Dict.get(Field)) :-
    !.
get_or_default(Dict, Field, Default, Default) :-
    \+ (_ = Dict.get(Field)).


parse_level("Cantrip", 0).
parse_level(Str, Level) :-
    member(Suffix, ["th-level", "st-level", "nd-level", "rd-level"]),
    string_concat(N, Suffix, Str),
    !,
    number_string(Level, N).

parse_components(Str, Material, Components) :-
    string_chars(Str, Chars),
    split_on([',',' '], Chars, Comps),
    maplist(process_component(Material), Comps, Components).
process_component(Material, ['M'], m(Material)).
process_component(_, [C], C2) :-
    C \= 'M',
    to_lower(C,C1),
    char_code(C2,C1).

parse_range("Self", self) :- !.
parse_range("Touch", touch) :- !.
parse_range(Str, feet(Feet)) :-
    member(Suffix, [" feet", " foot"]),
    string_concat(FeetStr, Suffix, Str),
    number_string(Feet, FeetStr),
    !.
parse_range(Str, miles(Miles)) :-
    member(Suffix, [" mile", " miles"]),
    string_concat(MilesStr, Suffix, Str),
    number_string(Miles, MilesStr),
    !.
parse_range(Str, Atom) :-
    to_lowercase_atom(Str, Atom).

parse_classes(Str, Classes) :-
    string_chars(Str, Chars),
    split_on([',', ' '], Chars, Lists),
    maplist(atomic_list_concat, Lists, UCClasses),
    maplist(to_lowercase_atom, UCClasses, Classes).

split_on(Sep, List, [List]) :-
    \+ (append(_, Rest, List), append(Sep, _, Rest)).
split_on(Sep, List, [X|Xs]) :-
    append(Sep, AfterSep, Tail),
    append(X, Tail, List),
    !,
    split_on(Sep, AfterSep, Xs).

:- read_spells(Spells),
   write_spells(Spells).


%! class_level(?ClassLevel)
%
%  class_level(Class:Level) is true iff your character has reached
%  exactly level Level in class Class, as determined by the clauses of
%  gain_level/3.
class_level(Class:ClassLevel) :-
    class_option(Class), % ground
    findall(L, (initial_class(Class) ; gain_level(L, Class, _)), Levels),
    length(Levels, ClassLevel),
    ClassLevel > 0.

%! class(?Class)
%
%  True for each Class your character has at least one level in.
class(Class) :- class_level(Class:_).

%! subclass(?Subclass)
%
%  Subclass is a compound term, where the class name is the functor
%  and the subclass atom is the sole argument. True iff your character
%  has at least one level in that class, and has selected the
%  subclass option with a choice/3 clause.
subclass(Subclass) :-
    class(Class),
    choice(Class >: _, subclass, Sub),
    Subclass =.. [Class, Sub].
subclass_level(Subclass:Level) :-
    subclass(Subclass),
    Subclass =.. [Class, _],
    class_level(Class:Level).

%! base_class(+Class, -BaseClass)
%
%  Extract the base class from a class.
base_class(Class, BaseClass) :-
    Class =.. [BaseClass|_],
    class_option(BaseClass).

%! subclass_option(?Class, ?Subclass)
subclass_option(_,_) :- false.
options_source(Class >: ClassLevel, subclass, subclass_option(Class)) :-
    choose_subclass_level(Class:ClassLevel).

%! match_class(?X)
%
%  TODO: delete
%
%  Determine whether your character matches a class/subclass (level) requirement.
%  If, for example, your character has class_level(warlock:3) with the
%  `fiend` subclass and class_level(fighter:1), then match_class(X) is
%  true for X equal to any of:
%
%  * `warlock:3`
%  * `warlock:2`
%  * `warlock:1`
%  * `warlock(fiend):3`
%  * `warlock(fiend):2`
%  * `warlock(fiend):1`
%  * `warlock`
%  * `warlock(fiend)`
%  * `fighter:1`
%  * `fighter`
match_class(C:L1) :-
    (class_level(C:L2) ; subclass_level(C:L2)),
    between(1, L2, L1).
match_class(C) :-
    class(C) ; subclass(C).

%! >:(?Class, ?Level)
%
%  Shorthand notation for `match_class(Class:Level)`.
Class >: Level :- match_class(Class:Level).

%! ^(?Class)
%
%  Shorthand notation for `initial_class(Class)`.
^ Class :- initial_class(Class).

%! class_origin_to_class_level(?Origin, ?Level:int)
%
%  Given a class-related Origin (for a trait, or a choice, or ...),
%  determine what class level that Origin refers to.
class_origin_to_class_level(choice(Origin,_), ClassLevel) :-
    class_origin_to_class_level_(Origin, ClassLevel).
class_origin_to_class_level(Origin, ClassLevel) :-
    class_origin_to_class_level_(Origin, ClassLevel).
class_origin_to_class_level_(class(Class), Class:1).
class_origin_to_class_level_(subclass(Subclass), Class:Lvl) :-
    Subclass =.. [Class, _],
    choose_subclass_level(Class:Lvl).
class_origin_to_class_level_(initial_class(Class), Class:1).
class_origin_to_class_level_(multiclass_into(Class), Class:1).
class_origin_to_class_level_(^Class, Class:1).
class_origin_to_class_level_(ClassF >: Level, X) :-
    class_origin_to_class_level_(match_class(ClassF:Level), X).
class_origin_to_class_level_(match_class(ClassF:Level), Class:Level) :-
    (Tail = [] ; Tail = [_]),
    ClassF =.. [Class|Tail].
class_origin_to_class_level_(match_class(ClassF), Class:1) :-
    (Tail = [] ; Tail = [_]),
    ClassF =.. [Class|Tail].
%class_origin_to_class_level_(replaced_spell(Class:Level, _), Class:Level).

%! gained_level_in_class_at_charlevel(?Class, ?CharLevel)
gained_level_in_class_at_charlevel(Class, 1) :-
    initial_class(Class).
gained_level_in_class_at_charlevel(Class, CharLevel) :-
    gain_level(CharLevel, Class, _).

%! reached_classlevel_at_charlevel(?ClassLevel, ?CharLevel)
reached_classlevel_at_charlevel(Class:ClassLevel, CharLevel) :-
    base_class(Class, BaseClass),
    class_option(BaseClass),
    findall(L, gained_level_in_class_at_charlevel(BaseClass,L), Ls),
    enumerate(1, Ls, NLs),
    member(ClassLevel-CharLevel, NLs).

%! class_origin_to_class(?Origin, ?Class:atomic)
%
%  Given a class-related Origin (for a trait, or a choice, or ...),
%  determine what class that Origin refers to.
class_origin_to_class(Origin, Class) :-
    class_origin_to_class_level(Origin, Class:_).

%! find_origin_class(?Origin, ?Class)
%
%  True if Origin is somehow related to Class.
find_origin_class(Origin, Class) :-
    class_origin_to_class(Origin, Class).
find_origin_class(trait(Trait), Class) :-
    trait(TraitOrigin, Trait),
    find_origin_class(TraitOrigin, Class).

%! class_choice(?Class:atomic, ?Id, ?Choice)
%
%  A choice originating from a class-based option.
class_choice(Class, Id, Choice) :-
    choice_member(Origin, Id, Choice),
    class_origin_to_class(Origin, Class).

%! multiclass
%
%  True iff your character has more than one class.
multiclass :-
    findall(C, class(C), [_,_|_]).

%! multiclass_into(Class)
%
%  True iff your character has Class as one of its classes, but not as
%  its initial_class/1.
multiclass_into(Class) :-
    class(Class),
    \+ initial_class(Class).

%! hd_per_level(?Class, ?Dice)
%
%  Hit dice gained per level in the given Class.
%  This predicate must be defined for each class.
hd_per_level(_,_) :- false.
required_predicate_for_each_class(hd_per_level/2).

%! initial_class_base_hp(?Class, ?HP)
%
%  The base_hp/1 of a character in Class of character level 1.
%  This predicate must be defined for each class.
initial_class_base_hp(_,_) :- false.
required_predicate_for_each_class(initial_class_base_hp/2).

%! max_hp_per_level(?Class, ?Dice)
%
%  Dice roll to determine the number of hit points your character
%  gains upon leveling up in the given Class.
%  This predicate must be defined for each class.
max_hp_per_level(_,_) :- false.
required_predicate_for_each_class(max_hp_per_level/2).

%! class_saving_throw(?Class, ?Ability)
%
%  Picking Class as *initial class* makes your character proficient in
%  saving throws for Ability.
class_saving_throw(_,_) :- false.
required_predicate_for_each_class(class_saving_throw/2).
trait_source(Class >: 1, saving_throw(Abi)) :-
    ^Class,
    class_saving_throw(Class, Abi).

%! choose_subclass_level(?ClassLevel)
%
%  Determines at which ClassLevel for any given class you have to pick
%  your subclass.
choose_subclass_level(_) :- false.

%! asi_level(?ClassLevel)
%
%  Determines at which ClassLevels your character receives an ability
%  score increase.
asi_level(_) :- false.
required_predicate_for_each_class(asi_level/1).
default_asi_level(L) :-
    member(L, [4,8,12,16,19]).

options_source(Class >: Level, 'asi or feat', (2 from ability) or feat_option) :-
    asi_level(Class:Level).
trait(choice(AsiLevel,'asi or feat'), feat(Feat)) :-
    choice(AsiLevel, 'asi or feat', Feat),
    feat_option(Feat).
bonus(choice(AsiLevel,'asi or feat'), Ability+1) :-
    choice_member(AsiLevel, 'asi or feat', Ability),
    ability(Ability).

    %(Bonus = Ability + N ; member(Ability+N, Bonus)).

%! caster(?Class, ?Factor)
%
%  Indicates to what extent the given Class is a classical spell caster.
%  From PHB, wizards, bards, sorcerers, druids, and clerics are all full casters,
%  so the query caster(wizard, full) is true.
%  Rangers and paladins are half casters; we write caster(ranger, 1/2).
%  The eldritch knight subclass of the fighter class is a "one third" caster,
%  written as `caster(fighter, 1/3) :- subclass(fighter('eldritch knight'))`
%  Other classes aren't casters, but we assert this explicitly with
%  caster(Class, 0), rather than just omitting the clause altogether.
%  Note that the warlock class is indeed not a caster: warlocks get
%  the `'pact magic'` feature rather than the `spellcasting` feature,
%  which works differently.
caster(_,_) :- false.
%required_predicate_for_each_class(caster/2).

%! spellcasting_ability(?Class:atomic, ?Ability:atomic)
spellcasting_ability(Compound, Abi) :-
    Compound =.. [Class,_|_],
    spellcasting_ability(Class, Abi).

%! max_prepared_spells(?Class:atomic, ?N:int)
%
%  Number of spells you can prepare for Class. A class that doesn't
%  need to prepare spells (like sorcerer) should just not list a
%  clause for this predicate.
max_prepared_spells(_,_) :- false.

%! caster
%
%  True iff your character has at least one level in a class with the
%  spellcasting feature.
caster :-
    class(Class),
    caster(Class, Factor),
    Factor \= 0,
    !.

%! class_shorthand(?Class:atomic, ?Shorthand:atomic)
%
%  Associates each class with a two-letter shorthand.
class_shorthand(barbarian , bb).
class_shorthand(bard      , bd).
class_shorthand(cleric    , cl).
class_shorthand(druid     , dr).
class_shorthand(fighter   , fi).
class_shorthand(monk      , mo).
class_shorthand(paladin   , pa).
class_shorthand(ranger    , ra).
class_shorthand(rogue     , ro).
class_shorthand(sorcerer  , so).
class_shorthand(warlock   , wl).
class_shorthand(wizard    , wz).

%! class_skill_list(?Class, ?List)
%
%  List of skills you can typically pick from when you gain skills
%  through this class.
class_skill_list(_,_) :- false.

%! class_skill(?Class, ?Skill)
%
%  One of the skills you can typically pick when you gain skills
%  through this class.
class_skill(Class, Skill) :-
    class_skill_list(Class, List),
    member(Skill, List).
class_skill_wrapped(Class, skill(Skill)) :-
    class_skill(Class, Skill).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% meta todo's
meta_todo(class(Class), predicate_missing(Pred/Arity)) :-
    class_option(Class),
    required_predicate_for_each_class(Pred/Arity),
    missing_for_class(Class, Pred/Arity).

missing_for_class(Class, Pred/1) :-
    \+ call(Pred, Class:_).
missing_for_class(Class, Pred/Arity) :-
    Arity \= 1,
    length(Args, Arity),
    Args = [Class|_],
    Goal =.. [Pred|Args],
    \+ call(Goal).
custom_format(replacing(Id, ToReplace)) -->
    [Id], [": replace \""], [ToReplace], ["\""].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CLASSLEVEL-BOUND
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       
%! selected_at_class_level(?ClassLevel, ?Id, ?Choice)
%
%  Some choices can be replaced at a later moment.
%  For example, a sorcerer may choose to forget one spell
%  to gain another every level starting from level 2.
%  This predicate helps keep track of what is selected
%  at which class level by looking at the current and past
%  class levels.
selected_at_class_level(Class:Level, Id, Choice) :-
    class_origin_to_class_level(Origin, Class:Level),
    (choice_member(Origin, Id, Choice) ; choice_member(Origin, replacing(Id,_), Choice)).
selected_at_class_level(Class:Level, Id, Choice) :-
    class_level(Class:CurLevel),
    between(2, CurLevel, Level), % ground Level
    PrevLevel is Level-1,
    selected_at_class_level(Class:PrevLevel, Id, Choice),
    \+ (class_origin_to_class_level(Origin, Class:Level),
        choice_member(Origin, replace(Id), Choice)).

%! replaceable_class_options(?ClassLevel, ?Id, ?Goal)
replaceable_class_options(_,_,_) :- false.

%! replace_at_class_level(?ClassLevel, ?Id, ?N:integer, ?Goal)
replace_at_class_level(_,_,_,_) :- false.

% ADD NEW
options_source(Class >: L, Id, Goal) :-
    replaceable_class_options(Class:L, Id, Goal).

% WANT TO REPLACE?
options_source(Class >: L,
               replace(Id),
               N unique_from selected_at_class_level(Class:Prev, Id)) :-
    replace_at_class_level(Class:L, Id, N, _),
    Prev is L-1.

% WHAT TO REPLACE WITH?
options(Class >: L, replacing(Id, Choice), Goal) :-
    class_level(Class:CurLvl),
    replace_at_class_level(Class:CurLvl, Id, _, Goal),
    choice_member(Class >: L,
                  replace(Id),
                  Choice).

% this name is horrible
find_choice_level(Class:Level, Id, Choice) :-
    class_level(Class:CurLevel),
    selected_at_class_level(Class:CurLevel, Id, Choice),
    findall(L,
            (choice_member(Class >: L, ChoiceId, Choice),
             member(ChoiceId, [Id, replacing(Id,_)])),
            Ls),
    max_member(Level, Ls).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CHARLEVEL-BOUND
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
at(Origin, Level) :-
    call(Origin),
    level(CurLevel),
    CurLevel >= Level.

%! selected_at_current_level(?Origin, ?Id, ?Choice)
selected_at_current_level(Origin, Id, Choice) :-
    level(Level),
    selected_at_character_level(Origin, Level, Id, Choice).

%! selected_at_character_level(?Origin, ?Level, ?Id, ?Choice)
%
%  Determine if a given choice is selected at a given character level.
selected_at_character_level(Origin, Level, Id, Choice) :-
    choice_member(Origin, Id, Choice),
    origin_level(Origin, Level).
selected_at_character_level(Origin, Level, Id, Choice) :-
    choice_member(Origin at Level, replacing(Id,_), Choice).
selected_at_character_level(Origin, Level, Id, Choice) :-
    level(CurLevel),
    between(2, CurLevel, Level),
    PrevLevel is Level-1,
    selected_at_character_level(Origin, PrevLevel, Id, Choice),
    \+ (choice_member(Origin at Level, replacing(Id, Choice), _)).

%! replace_at_character_level(?Origin, ?CharLevel, ?Id, ?Num, ?Goal)
%
%  Each fact represents the opportunity for a character to replace an
%  earlier choice with a new choice. Goal defines the options for the
%  new choice.
replace_at_character_level(_,_,_,_,_) :- false.

options_source(Origin at Level,
               replace(Id),
               N unique_from selected_at_character_level(Origin, Prev, Id)) :-
    replace_at_character_level(Origin, Level, Id, N, _),
    Prev is Level-1.

options_source(Origin at Level, replacing(Id, Choice), Goal) :-
    replace_at_character_level(Origin, Level, Id, _, Goal),
    choice_member(Origin at Level, replace(Id), Choice).

