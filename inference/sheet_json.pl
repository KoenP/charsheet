sheet_json_dict(_{name: Name,
                  summary: Summary,
                  ability_table: AbiTable,
                  skill_table: SkillTable,
                  languages: Languages,
                  weapons: Weapons,
                  armor: Armor,
                  tools: Tools,
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
    attack_table_json_dict(Attacks),
    findall([Lvl,N], spell_slots(Lvl,N), SpellSlots),
    pact_magic_json_dict(PactMagic),
    findall(X, spellcasting_section_json_dict(X), SpellcastingSections).

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
        speed-speed,
        hd-hit_dice_string,
        pp-passive_perception,
        prof_bon-proficiency_bonus ],
    maplist(call_snd, IdGoals, Pairs),
    dict_pairs(Dict, _, Pairs).

classes_str(Str) :-
    findall(ClassLevel, class_level(ClassLevel), ClassLevels),
    fmt(format_list(ClassLevels), Str).

race_str(Str) :-
    most_specific_race(Race),
    fmt(format_term(Race), Str).

hit_dice_string(Str) :-
    hit_dice(HD),
    fmt(format_dice_sum(HD), Str).

% Ability table.
ability_table_json_dict(Dict) :-
    findall(Pair,
            ability_table_cell_pair(Pair),
            Pairs),
    dict_pairs(Dict, _, Pairs).

ability_table_cell_pair(Id-Val) :-
    ability(Abi),
    ability_table_cell_suffix_pred(Suffix, Pred),
    Goal =.. [Pred, Abi, Val],
    call(Goal),
    atom_concat(Abi, Suffix, Id).
ability_table_cell_suffix_pred(score, ability).
ability_table_cell_suffix_pred(mod, ability_mod_str).
ability_table_cell_suffix_pred(st, saving_throw_str).
ability_mod_str(Abi, Str) :-
    ability_mod(Abi, Mod),
    fmt(format_bonus(Mod), Str).
saving_throw_str(Abi, Str) :-
    saving_throw(Abi, Mod),
    fmt(format_bonus(Mod), Str).

% Skill table.
skill_table_json_dict(Dict) :-
    findall(Skill-ModStr,
            (skill(Skill,Mod),
             fmt(format_bonus(Mod),ModStr)),
            Pairs),
    dict_pairs(Dict, _, Pairs).

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
    _{origin: Origin,
      spellcasting_ability: Abi,
      spellcasting_ability_mod: AbiMod,
      spell_save_dc: DC,
      spell_attack_mod: AttackMod,
      max_prepared_spells: Prep,
      spells: Spells}) :-
    spell_origin(Origin),
    spellcasting_ability(Origin, Abi),
    ability_mod(Abi, AbiMod),
    known_spell_origin_class(Origin, Class),
    spell_save_dc(Class, DC),
    spell_attack_modifier(Class, AttackMod),
    default_on_fail(null, max_prepared_spells(Origin), Prep),
    spell_list_json_dict(Origin, Spells).

spell_list_json_dict(Origin, SpellsSorted) :-
    findall(Spell,
            spell_json_dict(Origin, Spell),
            SpellsUnsorted),
    sort(level, @=<, SpellsUnsorted, SpellsSorted).

spell_json_dict(Origin,
                _{availability: Availability,
                  level: Level,
                  name: Name,
                  description: Description,
                  casting_time: CastingTime,
                  range: Range,
                  components: Components,
                  duration: Duration,
                  concentration: Concentration,
                  to_hit: ToHit,
                  dc: DC,
                  dc_abi: DCAbi,
                  summary: Summary,
                  ritual: Ritual,
                  resources: Resources}) :-
    known_spell(Origin, _Ability, Availability, Resources, Ritual, Name),
    known_spell_data(Origin, Name, Data),
    Level         = Data.level,
    Description   = Data.desc,
    CastingTime   = Data.casting_time,
    RangeVal      = Data.range,
    fmt(format_measure(RangeVal), Range),
    Components    = "todo", %Data.components,
    Duration      = Data.duration,
    Concentration = Data.concentration,
    display_spell_effects(Data, Summary),
    default_on_fail(null, known_spell_to_hit(Origin,Name), ToHit),
    known_spell_saving_throw_or_null(Origin, Name, DC, DCAbi).

known_spell_saving_throw_or_null(Origin, Name, DC, Abi) :-
    known_spell_saving_throw(Origin, Name, DC, Abi),
    !.
known_spell_saving_throw_or_null(_, _, null, null).
