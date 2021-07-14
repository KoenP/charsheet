:- use_module(library(http/html_write)).

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
                 ProfList,
                 TraitList,
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
    proficiency_list(ProfList),
    trait_list(TraitList),
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

% Proficiencies, other than skill proficiencies.
proficiency_list(p([h3("Proficiencies"), ul(Profs)])) :-
    findall(Prof, proficiency_list_entry(Prof), Profs).
proficiency_list_entry(Entry) :-
      proficiency_category("Languages: ", language, Entry)
    ; proficiency_category("Weapons: ", weapon, Entry)
    ; proficiency_category("Armor: ", armor, Entry)
    ; proficiency_category("Tools: ", tool, Entry).
proficiency_category(CatHdr, CatFunctor, li([b(CatHdr) | Profs])) :-
    findall(X, (Search =.. [CatFunctor,X], trait(Search)), Xs),
    format_list(Xs, Profs, []).
%proficiency_list_entry(li([b("Languages: ") | Languages])) :-
%    findall(Lang, trait(language(Lang)), Langs),
%    format_list(Langs, Languages, []).
%proficiency_list_entry(li([b("Weapons: ") | Weapons])) :-
%    findall(Weap, trait(weapon(Weap)), Weaps),
%    format_list(Weaps, Weapons, []).
%proficiency_list_entry(li([b("Tools: ") | Tools])) :-
%    findall(Tl, trait(tool(Tl)), Tls),
%    format_list(Tls, Tools, []).

% Traits.
trait_list(p([h3("Notable traits"), ul(Items)])) :-
    findall(li(Item), trait_list_entry(Item), Items).
trait_list_entry(div(class=tooltip, [Trait, span(class=tooltiptext, Desc)])) :-
    trait(TraitVal),
    display_trait(TraitVal, Trait),
    TraitVal ?= Desc.
display_trait(CompoundTerm, Atom) :-
    CompoundTerm =.. [_, Atom].
display_trait(Atom, Atom) :-
    atom(Atom).

% Attacks.
attack_table(Table) :-
    table('attacks', 'Attacks', [Header|Rows], Table),
    Header = tr([th([]), th('Range'), th('To Hit'), th('Damage'), th('Notes')]),
    findall(Row, attack_table_row(Row), Rows).
attack_table_row(tr([td(Name), td(Range), td(ToHit), td(DamageFmt), td(FNotes)])) :-
    attack(AttackName, Range, ToHitVal, Damage, Notes),
    format_list(Notes, FNotes, []),
    display_term(AttackName, Name),
    show_bonus(ToHitVal, ToHit),
    format_damage(Damage, DamageFmt).
    %format_dice_sum(DamageDice, Damage).
format_damage(Damage, Format) :-
    maplist(format_damage_roll, Damage, Fmts),
    interleave([','], Fmts, Sum),
    append(Sum, Format).
format_damage_roll(Roll, Format) :-
    Roll =.. [Type, Dice],
    format_dice_sum(Dice, DiceFmt),
    append(DiceFmt, [' ', Type], Format).
interleave(X, [Y1,Y2|Ys], [Y1,X|Rest]) :-
    interleave(X, [Y2|Ys], Rest).
interleave(_, [Y], [Y]).
interleave(_, [], []).


% Spellcasting section.
spell_slot_table(Table) :-
    table('spell_slots', 'Spell slots and preparation', [Header|Rows], Table),
    Header = tr([th([]), th('prep') | Levels]),
    findall(th(Str),
            (between(1, 9, Level), atomic_list_concat(['lvl ', Level], Str)),
            Levels),
    findall(Row, spell_slot_table_row(Row), Rows).
spell_slot_table_row(tr([th(Source), td(Prep) | Row])) :-
    spell_slot_source(Source),
    max_prepared_spells(Source, Prep),
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
                 th('Range'), th('To Hit/DC'), th('Effect (summary)'), th('Resource')]),
    findall(Row,
            (between(0, 9, Lvl), spell_table_rows_for_level(Lvl, Row)),
            RowsPerLevel),
    append(RowsPerLevel, Rows).

spell_table_rows_for_level(SpellLevel, Rows) :-
    findall(Row, spell_table_row(SpellLevel, Row), Rows).
spell_table_row(SpellLevel, tr([td(Prepared),
                                td(SpellLevel),
                                td(div(class=tooltip, [Name, span(class=tooltiptext, Desc)])),
                                td(CastingTime),
                                td(Range),
                                td(ToHitOrDC),
                                td(Effect),
                                td(Resource)
                                ])) :-
    spell_known(Name, Source, _Ability, PrepVal, ResourceVal),
    spell(Name, level, SpellLevel),
    spell(Name, desc, Desc),
    spell(Name, casting_time, CastingTime),
    spell(Name, range, RangeVal), display_range(RangeVal, Range),
    spell_to_hit_or_dc(Name, Source, ToHitOrDC),
    display_spell_effects(Name, Source, Effect),
    display_prepared(PrepVal, Prepared),
    display_resource(ResourceVal, Source, Resource).
spell_to_hit_or_dc(Name, Source, [+, ToHit]) :-
    spell_to_hit(Name, Source, ToHit), !.
spell_to_hit_or_dc(Name, Source, ['DC ', DC]) :-
    spell_dc(Name, Source, DC), !.
spell_to_hit_or_dc(_, _, "-").
display_spell_effects(Spell, Source, Damage) :-
    spell_known_damage(Spell, Source, _, 0, DamageRolls),
    format_damage(DamageRolls, Damage).
display_spell_effects(Spell, Source, '-') :-
    \+ spell_known_damage(Spell, Source, _, 0, _).

display_range(feet(X), [X, ' ft']) :- !.
display_range(miles(X), [X, ' mi']) :- !.
display_range(X, X).

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

    
    

% Clever little predicate, taken from
% http://stackoverflow.com/questions/23176840/easily-replicate-an-element-in-prolog
repl(X, N, L) :-
    length(L, N),
    maplist(=(X), L).
