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


% myhtml(N d X) --> [N], [d], [X].
% myhtml(Html) --> {Html \= _ d _}, html_write:html(Html).

out :-
    html_write:html_set_options([doctype(html)]),
    char_sheet_html(Html),
    name(Name),
    atom_concat(Name, '.html', FileName),
    !,
    open(FileName, write, Stream),
    html_write:page(Html, Tokens, []),
    html_write:print_html(Stream, Tokens),
    close(Stream).

char_sheet_html([head(Head), body(Body)]) :-
    char_sheet_head(Head),
    char_sheet_body(Body).

char_sheet_head([title(Name),
                 link([rel=stylesheet, href='charsheet.css'], [])
                ]) :-
    name(Name).

char_sheet_body(
    [div(class('container'),
         [ header(h1(Name)),
           article(
               [ table( [style='padding: 4px']
                      , [ tr([th("Race"), td(Race)])
                        , tr([th("Class"), td(Classes)])
                        , tr([th("Level"), td(Level)])
                        , tr([th("Max HP"), td(HP)])
                        , tr([th("AC"), td(AC)])
                        , tr([th("Initiative"), td(Init)])
                        , tr([th("Speed"), td(Speed)])
                        , tr([th("HD"), td(HD)])
                        , tr([th("PP"), td(PP)])
                        , tr([th("Prof Bon"), td(ProfBon)])
                        ]),
                 AbilityTable,
                 SkillTable,
                 AttackTable,
                 SpellSlotTable,
                 SpellTable
               ]
           )])]) :-
    name(Name),
    most_specific_race(RaceVal), display_term(RaceVal, Race),
    class_levels(ClassesTerm), format_list(ClassesTerm, Classes, []),
    level(Level),
    max_hp(HP),
    ac(AC),
    initiative_mod(InitVal), show_bonus(InitVal, Init),
    speed(Speed),
    hit_dice(HDTerm), format_dice_sum(HDTerm, HD), %term_to_atom(HDTerm, HD),
    passive_perception(PP),
    proficiency_bonus(ProfBonVal), show_bonus(ProfBonVal, ProfBon),
    ability_table(AbilityTable),
    skill_table(SkillTable),
    attack_table(AttackTable),
    spell_slot_table(SpellSlotTable),
    spell_table(SpellTable).

% Ability table.
ability_table(Table) :- 
    table('abilities', 'Abilities', Contents, Table),
    Contents = [tr([th([]), th('Score'), th('Mod'), th('ST')])|Rows],
    findall(Row, ability_table_row(_, Row), Rows).
ability_table_row(Abil, tr([th(AbilHdr), td(Score), td(Mf), td(ST)])) :-
    ability(Abil, Score),
    ability_hdr(Abil, AbilHdr),
    ability_mod(Abil, MfVal), show_bonus(MfVal, Mf),
    saving_throw(Abil, STVal), show_bonus(STVal, ST).
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
    skill(Skill, ScoreVal), show_bonus(ScoreVal, Score),
    append(RowSpanLine, [td(Skill), td(Score)], Row).
skill_table_row_span_line(_, 0, []) :- !.
skill_table_row_span_line(Skill, Rowspan, [th(rowspan=Rowspan, AbilHdr)]) :-
    skill_ability(Skill, Abil),
    ability_hdr(Abil, AbilHdr).

% Attacks.
attack_table(Table) :-
    table('attacks', 'Attacks', [Header|Rows], Table),
    Header = tr([th([]), th('Range'), th('Type'), th('To Hit'), th('Damage'),
                 th('Notes')]),
    findall(Row, attack_table_row(Row), Rows).
attack_table_row(tr([td(Name), td(Range), td(Type), td(ToHit), td(Damage), td(FNotes)])) :-
    attack(AttackName, Range, Type, ToHitVal, DamageDice, Notes),
    format_list(Notes, FNotes, []),
    display_term(AttackName, Name),
    show_bonus(ToHitVal, ToHit),
    format_dice_sum(DamageDice, Damage).
    
% Spellcasting section.
spell_slot_table(Table) :-
    table('spell_slots', 'Spell slots', [Header|Rows], Table),
    Header = tr([th([])|Levels]),
    findall(th(Str),
            (between(1, 9, Level), atomic_list_concat(['lvl ', Level], Str)),
            Levels),
    findall(Row, spell_slot_table_row(Row), Rows).
spell_slot_table_row(tr([th(Source) | Row])) :-
    spell_slot_source(Source),
    findall(Cell,
            spell_slot_table_cell(Source, Cell),
            Slots),
    \+ length(Slots, 0),
    length(Row, 9),
    append(Slots, Padding, Row),
    maplist(=(td([])), Padding).
spell_slot_table_cell(Source, td(Contents)) :-
    between(1, 9, Level),
    spell_slots(Source, spell_level(Level), N),
    repl(input(type=checkbox, []), N, Contents).
spell_table(Table) :-
    table('spells', 'Spells', [Header|Rows], Table),
    Header = tr([th('Prepared'), th('Level'), th('Spell'), th('Casting time'),
                 th('Range'), th('To Hit/DC'), th('Resource')]),
    findall(Row,
            (between(0, 9, Lvl), spell_table_rows_for_level(Lvl, Row)),
            RowsPerLevel),
    append(RowsPerLevel, Rows).

spell_table_rows_for_level(SpellLevel, Rows) :-
    findall(Row, spell_table_row(SpellLevel, Row), Rows).
spell_table_row(SpellLevel, tr([td(Prepared),
                                td(SpellLevel),
                                td(Name),
                                td(CastingTime),
                                td(Range),
                                td(ToHitOrDC),
                                td(Resource)
                                ])) :-
    spell_known(Name, Source, _Ability, PrepVal, ResourceVal),
    spell(Name, level, SpellLevel),
    spell(Name, casting_time, CastingTime),
    spell(Name, range, Range),
    spell_to_hit_or_dc(Name, Source, ToHitOrDC),
    display_prepared(PrepVal, Prepared),
    display_resource(ResourceVal, Source, Resource).
spell_to_hit_or_dc(Name, Source, [+, ToHit]) :-
    spell_to_hit(Name, Source, ToHit), !.
spell_to_hit_or_dc(Name, Source, ['DC ', DC]) :-
    spell_dc(Name, Source, DC), !.
spell_to_hit_or_dc(_, _, "-").

display_prepared(when_prepared, input(type=checkbox,[])).
display_prepared(always_available, 'always').

display_resource(at_will, _, 'at will').
display_resource(spell_slot, Source, Source).


% Helper predicates.
table(Id, Caption, Contents, table(id=Id, [caption(h3(Caption))|Contents])).

show_list([X], String) :-
    term_string(X, String).
show_list([X|Xs], String) :-
    term_string(X, String1),
    show_list(Xs, String2),
    string_concat(String1, String2, String).

show_bonus(Val, ['+', Val]) :- Val >= 0.
show_bonus(Val, Val) :- Val < 0.

format_list([],[],[]).
format_list([X]) --> lit(X).
format_list([X|Xs]) --> {Xs \= []}, lit(X), [", "], format_list(Xs).
lit(X,[Str|T],T) :-
    display_term(X, Str).

format_dice(N d X, [N,d,X]).
format_dice_sum(D, F) :-
    format_dice(D, F).
%format_dice_sum(D, D) :-
%    D \= _ + _.
format_dice_sum(Ds + D, F) :-
    format_dice(D, FD),
    format_dice_sum(Ds, FDs),
    append([FDs, [" + "], FD], F).
format_dice_sum(Ds + K, F) :-
    number(K),
    K < 0,
    N is - K,
    format_dice_sum(Ds, FDs),
    append([FDs, [" - "], [N]], F).
format_dice_sum(Ds + K, F) :-
    number(K),
    K >= 0,
    format_dice_sum(Ds, FDs),
    append([FDs, [" + "], [K]], F).
    
display_term(T, U) :-
    custom_display_rule(T, U),
    !.
display_term(List, U) :-
    format_list(List, U, []),
    !.
display_term(Atom, S) :-
    atom(Atom),
    !,
    atomics_to_string([Atom], S).
display_term(T, S) :-
    term_string(T, S).

    
    

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
% Clever little predicate, taken from
% http://stackoverflow.com/questions/23176840/easily-replicate-an-element-in-prolog
repl(X, N, L) :-
    length(L, N),
    maplist(=(X), L).
