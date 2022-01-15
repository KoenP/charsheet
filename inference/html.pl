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
out_file_name --> {name(CharName)}, seq_atom(CharName), ".html".

char_sheet_html([head(Head), body(Body)]) :-
    char_sheet_head(Head),
    char_sheet_body(Body).

char_sheet_head([title(Name), link([rel=stylesheet, href='css/charsheet.css'], [])]) :-
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
    fmt(format_measure(RangeVal), Range),
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
    known_spell_origin_class(Origin, Class),
    spell_save_dc(Class, DC).
spell_origin_info(Origin, "Spell attack modifier", AttackModStr) :-
    known_spell_origin_class(Origin, Class),
    spell_attack_modifier(Class, AttackMod),
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
    Header = tr([th('Prep\'d'), th('Lvl'), th('Src'), th('Spell'), th('CT'),
                 th('Rng'), th('Cpts'), th('Dur'), th('Conc'), th('To Hit/DC'),
                 th('Effect (summary)'), th('Res')]),
    findall(Row, spell_table_row(Origin, _, _, Row), Rows).
    %spell_table_rows(Rows).

spell_table_row(Origin, Name, SpellLevel, tr(Row)) :-
    known_spell(Origin, Ability, AvailabilityVal, ResourcesVal, _Ritual, Name),
    format_spell_availability(AvailabilityVal, Availability),
    known_spell_data(Origin, Name, Data),
    SpellLevel = Data.level,
    spell_origin_shorthand(Origin, OriginShorthand),
    phrase(format_range(Data.range), Range),
    phrase(format_resources(ResourcesVal), Resources),
    format_components(Data.components, Components),
    spell_to_hit_or_dc(Ability, Data, ToHitOrDC),
    display_spell_effects(Data, Effects),
    RowFields = [Availability, SpellLevel, OriginShorthand,
                 div(class=tooltip, [Name, span(class=tooltiptext, Data.desc)]),
                 Data.casting_time, Range, Components, Data.duration, Data.concentration,
                 ToHitOrDC, Effects, Resources
                ],
    maplist(wrap(td), RowFields, Row).

spell_origin_shorthand(Class, Shorthand) :-
    class_shorthand(Class, Shorthand).
spell_origin_shorthand(Race, Shorthand) :-
    race_shorthand(Race, Shorthand).
spell_origin_shorthand(Origin:Elaboration,
                       [Shorthand, div(class=tooltip, ["*", span(class=tooltiptext, ElabFmt)])]) :-
    spell_origin_shorthand(Origin, Shorthand),
    format_term(Elaboration, ElabFmt, []).

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

format_range(feet(X)) --> {!}, [X], [" ft"].
format_range(miles(X)) --> {!}, [X], [" mi"].
format_range(X) --> [X].

format_resources(Rs1 or Rs2) -->
    format_resources(Rs1), [" or "], format_resources(Rs2).
format_resources([]) --> ["-"].
format_resources([R]) --> {!}, format_resource(R).
format_resources([R|Rs]) --> format_resource(R), [', '], format_resources(Rs).
format_resource(per_rest(Dur, N)) --> {!}, checkboxes(N), [' / '], [Dur], [' rest'].
format_resource(R) --> [R].

display_spell_effects(Data, Effects) :-
    format_effects(Data.get(effects), Effects, []),
    !.
display_spell_effects(_, "-").

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

