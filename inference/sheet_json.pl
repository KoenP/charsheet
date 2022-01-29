sheet_json_dict(_{name: Name,
                  summary: Summary,
                  ability_table: AbiTable,
                  skill_table: SkillTable,
                  languages: Languages,
                  weapons: Weapons,
                  armor: Armor,
                  tools: Tools,
                  attacks: Attacks
                 }) :-
    name(Name),
    summary_json_dict(Summary),
    ability_table_json_dict(AbiTable),
    skill_table_json_dict(SkillTable),
    findall(X, trait(language(X)), Languages),
    findall(X, trait(weapon(X)), Weapons),
    findall(X, trait(armor(X)), Armor),
    findall(X, trait(tools(X)), Tools),
    attack_table_json_dict(Attacks).

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
