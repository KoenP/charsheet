sheet_json_dict(_{name: Name,
                  summary: Summary,
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
                  spellcasting_sections: SpellcastingSections}) :-
    name(Name),
    summary_json_dict(Summary),
    ability_table_json_dict(AbiTable),
    skill_table_json_dict(SkillTable),
    findall(X, trait(language(X)), Languages),
    findall(X, trait(weapon(X)), Weapons),
    findall(X, trait(armor(X)), Armor),
    findall(X, trait(tools(X)), Tools),
    notable_traits_json_dict(NotableTraits),
    attack_table_json_dict(Attacks),
    spell_slot_dict(SpellSlots),
    pact_magic_json_dict(PactMagic),
    findall(X, spellcasting_section_json_dict(X), SpellcastingSections).

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
        speed-default_on_fail(0, speed),
        hd-default_on_fail("", hit_dice_string),
        pp-passive_perception,
        prof_bon-proficiency_bonus ],
    maplist(call_snd, IdGoals, Pairs),
    dict_pairs(Dict, _, Pairs).

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
    fmt(format_term(Cat), CatStr),
    maplist(trait_json_dict, Traits, TraitDicts).

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
    attack(Name, RangeVal, ToHitOrDCVal, DamageVal, NotesVal),
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
                  aoe: Aoe
                 }) :-
    (Origin =.. [BaseOrigin,_] ; Origin = BaseOrigin),
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
    known_spell_aoe_or_null(Origin, Name, Aoe).

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

resources_json(R1 or R2, _{tag: or, val: Val}) :-
    maplist(resources_json, [R1,R2], Val),
    !.
resources_json(per_rest(Dur, N), _{tag: per_rest,
                                   rest_type: Dur,
                                   count: N}) :-
    !.
resources_json(List, _{tag: list, val: JsonList}) :-
    maplist(resources_json, List, JsonList),
    !.
resources_json(X, _{tag: val, val: X}).

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
