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

body_contents([Summary, AbilityTable, SkillTable]) :-
    character_summary(Summary),
    ability_table(AbilityTable),
    skill_table(SkillTable).

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
    ST=todo.
    %saving_throw(Abil, STVal), format_bonus(STVal, ST, []).
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

