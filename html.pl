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

char_sheet_head([title(Name), link([rel=stylesheet, href='charsheet.css'], [])]) :-
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
              spell_slot_table,
              spell_preparation_table,
              spell_table],
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
                      , tr([th("Speed"), td(Speed)])
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
    Speed = todo,
    hit_dice(HDTerm), format_dice_sum(HDTerm, HD, []),
    PP = todo,
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
proficiency_list(p([h3("Proficiencies"), div([id=proficiencies], ul(Profs))])) :-
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
trait_list(p([h3("Notable traits"), div([id=traits], ul(Items))])) :-
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

% Spellcasting section.
spell_slot_table(Table) :-
    table('spell_slots', 'Spell slots', [tr(Levels)|Rows], Table),
    findall(th(Str),
            (between(1, 9, Level), atomic_list_concat(['lvl ', Level], Str)),
            Levels),
    findall(Row, spell_slot_table_row(Row), Rows).
spell_slot_table_row(tr(Row)) :-
    findall(Cell,
            spell_slot_table_cell(Cell),
            Slots),
    \+ length(Slots, 0),
    length(Row, 9),
    append(Slots, Padding, Row),
    maplist(=(td([])), Padding).
spell_slot_table_cell(td(Contents)) :-
    between(1, 9, Level),
    spell_slots(Level, N),
    repl(input(type=checkbox, []), N, Contents).

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

spell_table(Table) :-
    table('spells', 'Spells', [Header|Rows], Table),
    Header = tr([th('Prep\'d'), th('Lvl'), th('Source'), th('Spell'), th('Cast time'),
                 th('Rng'), th('Cpts'), th('Dur'), th('To Hit/DC'),
                 th('Effect (summary)'), th('Res')]),
    findall(Row, spell_table_row(_, _, Row), Rows).
    %spell_table_rows(Rows).

spell_table_row(Name, SpellLevel, tr(Row)) :-
    known_spell(Origin, Ability, Availability, Resources, _Ritual, Name),
    known_spell_data(Origin, Name, Data),
    SpellLevel = Data.level,
    phrase(format_range(Data.range), Range),
    format_components(Data.components, Components),
    spell_to_hit_or_dc(Ability, Data, ToHitOrDC),
    RowFields = [Availability, SpellLevel, Origin,
                 div(class=tooltip, [Name, span(class=tooltiptext, Data.desc)]),
                 Data.casting_time, Range, Components, Data.duration,
                 ToHitOrDC, "todo_effects", Resources
                ],
    maplist(wrap(td), RowFields, Row).

spell_to_hit_or_dc(Ability, SpellData, ToHit) :-
    Effects = SpellData.get(effects),
    contains_attack_roll(Effects),
    proficiency_bonus(ProfBon),
    ability_mod(Ability, Mod),
    ToHitVal is ProfBon + Mod,
    format_bonus(ToHitVal, ToHit, []).
spell_to_hit_or_dc(_, SpellData, "-") :-
    \+ (Effects = SpellData.get(effects), contains_attack_roll(Effects)).

format_components(Cs, Format) :-
    maplist(format_component, Cs, Format).
    %format_list(Formats, Format, []).
format_component(m(M), span(class=tooltip, [m, span(class=tooltiptext, M)])).
format_component(C, C) :- C \= m(_).

format_range(feet(X)) --> {!}, [X], [" ft"].
format_range(miles(X)) --> {!}, [X], [" mi"].
format_range(X) --> [X].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

thtd(Header, DataRule, [tr([th(Header), td(X)])], Tail) :-
    call(DataRule, X, Tail).
    
wrapped(Goal, Rule, Result, Tail) :-
    call(Rule, X, Tail),
    call(Goal, X, Result).

pred(Pred, Result, _) :-
    call(Pred, Result).

% Helper predicates.
table(Id, Caption, Contents, table(id=Id, [caption(h3(Caption))|Contents])).

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

