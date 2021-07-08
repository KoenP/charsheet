:- use_module(library(http/html_write)).

% <!DOCTYPE html>
%<html>
%<title>HTML Tutorial</title>
%<body>
%
%<h1>This is a heading</h1>
%<p>This is a paragraph.</p>
%
%</body>
%</html> 

example(Html) :-
     Html = [
         html([title("HTML Tutorial"),
               body([h1("This is a heading"),
                     p("This is a paragraph")
                    ])])].

test :-
    char_sheet_html(Html),
    open('test.html', write, Stream),
    html_write:html(Html, Tokens, []),
    html_write:print_html(Stream, Tokens),
    close(Stream).

char_sheet_html(
    [div(class('container'),
         [ header(h1(Name)),
           article(
               [ table( [style='padding: 4px']
                      , [ tr([th("Race"), td(Race)])
                        , tr([th("Class"), td(Classes)])
                        , tr([th("Level"), td(Level)])
                        , tr([th("AC"), td(AC)])
                        , tr([th("Initiative"), td(Init)])
                        , tr([th("Speed"), td(Speed)])
                        , tr([th("HD"), td(HD)])
                        , tr([th("PP"), td(PP)])
                        , tr([th("Prof Bon"), td(ProfBon)])
                        ])])])]) :-
    name(Name),
    most_specific_race(Race), %TODO
    class_levels(ClassesTerm), format_list(ClassesTerm, Classes, []),
    level(Level),
    ac(AC),
    initiative_mod(InitVal), show_bonus(InitVal, Init),
    speed(Speed),
    hit_dice(HDTerm), format_dice_sum(HDTerm, HD), %term_to_atom(HDTerm, HD),
    passive_perception(PP),
    proficiency_bonus(ProfBonVal), show_bonus(ProfBonVal, ProfBon).

show_list([X], String) :-
    term_string(X, String).
show_list([X|Xs], String) :-
    term_string(X, String1),
    show_list(Xs, String2),
    string_concat(String1, String2, String).

show_bonus(Val, ['+', Val]) :- Val >= 0.
show_bonus(Val, Val) :- Val < 0.

format_list([X]) --> lit(X).
format_list([X|Xs]) --> lit(X), [','], format_list(Xs).
lit(X,[Str|T],T) :-
    term_string(X, Str).

format_dice(N d X, [N,d,X]).
format_dice_sum(D, F) :-
    format_dice(D, F).
format_dice_sum(Ds + D, F) :-
    format_dice_sum(Ds, FDs),
    format_dice(D, FD),
    append([FDs, [" + "], FD], F).
    

%charSheetHtml([div(class('container'), 
%                   [ header(h1(Name))
%                   , article( 
%                        [ table( [style='padding: 4px']
%                               , [ tr([th("Race"), td(Race)])
%                                 , tr([th("Class"), td(Class)])
%                                 , tr([th("Level"), td(Level)])
%                                 , tr([th("AC"), td(Ac)])
%                                 , tr([th("Initiative"), td(Init)])
%                                 , tr([th("Speed"), td(Speed)])
%                                 , tr([th("HD"), td([HdN, d, HdV])])
%                                 , tr([th("PP"), td(Pp)])
%                                 , tr([th("Prof Bon"), td(ProfBon)])
%                                 ])
%                        , AbilityTable
%                        , SkillTable
%                        , SpellCastingTable
%                        ])
%                   ])
%              ]) :- 
%    name(Name),
%    race(Race),
%    baseClass(Class),
%    level(Level),
%    ac(ac(Ac)),
%    init(Init),
%    speed(ft(Speed)),
%    hd(d(HdN, HdV)),
%    passivePerception(Pp),
%    profBon(ProfBon),
%    abilityTable(AbilityTable),
%    skillTable(SkillTable),
%    spellCastingTable(SpellCastingTable).
%
%abilityTable(Table) :- 
%    table('abilities', 'Abilities', Contents, Table),
%    Contents = [tr([th([]), th('Score'), th('Modifier'), th('ST')])|Rows],
%    findall(Row, abilityTableRow(_, Row), Rows).
%abilityTableRow(Abil, tr([th(AbilHdr), td(Score), td(Mf), td(St)])) :-
%    abilityHdr(Abil, AbilHdr),
%    ability(Abil, Score),
%    abilityMf(Abil, Mf),
%    st(Abil, St).
%abilityHdr(str, 'STR').
%abilityHdr(dex, 'DEX').
%abilityHdr(con, 'CON').
%abilityHdr(int, 'INT').
%abilityHdr(wis, 'WIS').
%abilityHdr(cha, 'CHA').
%
%skillTable(Table) :- 
%    table('skills', 'Skills', [Header|Rows], Table),
%    Header = tr([th([]), th('Skill'), th('Score')]),
%    abilityList(AbilList),
%    findall(Row, 
%            (member(Abil, AbilList), skillTableRowsForAbil(Abil, Row)),
%            AbilRows),
%    flatten(AbilRows, Rows).
%skillTableRowsForAbil(Abil, [FirstRow|OtherRows]) :-
%    findall(Skill, skillAbility(Skill, Abil), Skills),
%    length(Skills, NumberOfSkills),
%    [FirstSkill|OtherSkills] = Skills,
%    skillTableRow(FirstSkill, NumberOfSkills, FirstRow),
%    repl(0, NumberOfSkills, [0|Zeros]),
%    maplist(skillTableRow, OtherSkills, Zeros, OtherRows).
%skillTableRow(Skill, NumberOfSkills, tr(Row)) :- 
%    skillTableRowSpanLine(Skill, NumberOfSkills, RowSpanLine),
%    skill(Skill, Score),
%    append(RowSpanLine, [td(Skill), td(Score)], Row).
%skillTableRowSpanLine(_, 0, []) :- !.
%skillTableRowSpanLine(Skill, Rowspan, [th(rowspan=Rowspan, AbilHdr)]) :-
%    skillAbility(Skill, Abil),
%    abilityHdr(Abil, AbilHdr).
%
%spellCastingTable(Table) :-
%    table('spellcasting', 'Spellcasting', [R1,R2,R3], Table),
%    R1 = tr([th('Ability'), td(Abil)]),
%    R2 = tr([th('Spell save DC'), td(SSDC)]),
%    R3 = tr([th('Attack Bonus'), td(Amf)]),
%    baseClass(BaseClass),
%    spellCastingAbility(BaseClass, Abil),
%    spellSaveDc(SSDC),
%    spellAttackModifier(Amf).
%
%table(Id, Caption, Contents, table(id=Id, [caption(h3(Caption))|Contents])).
%
%% Brilliant little predicate, taken from
%% http://stackoverflow.com/questions/23176840/easily-replicate-an-element-in-prolog
%repl(X, N, L) :-
%    length(L, N),
%    maplist(=(X), L).
