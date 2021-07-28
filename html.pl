:- use_module(library(http/html_write)).

out :-
    warn_if_problems,
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
               [ div([table( [id=summary, style='padding: 4px'] ,
                             [ tr([th("Race"), td(Race)])
                             , tr([th("Class"), td(Classes)])
                             , tr([th("Level"), td(Level)])
                             , tr([th("Max HP"), td(HP)])
                             , tr([th("AC"), td(AC)])
                             , tr([th("Initiative"), td(Init)])
                             , tr([th("Speed"), td(Speed)])
                             , tr([th("HD"), td(HD)])
                             , tr([th("PP"), td(PP)])
                             , tr([th("Prof Bon"), td(ProfBon)])
                             ])
                     , AbilityTable
                     ]),
                 SkillTable,
                 ProfList,
                 TraitList,
                 div(Custom),
                 ResourceTable,
                 AttackTable,
                 SpellSlotTable,
                 SpellPrepTable,
                 SpellTable
               ]
           )])]) :-
    name(Name),
    most_specific_race(RaceVal), format(RaceVal, Race),
    class_levels(ClassesTerm), format_list(ClassesTerm, Classes, []),
    level(Level),
    max_hp(HP),
    ac(AC),
    initiative_mod(InitVal), format_bonus(InitVal, Init, []),
    speed(Speed),
    hit_dice(HDTerm), fmt(format_dice_sum(HDTerm), HD), %term_to_atom(HDTerm, HD),
    passive_perception(PP),
    proficiency_bonus(ProfBonVal), format_bonus(ProfBonVal, ProfBon, []),
    ability_table(AbilityTable),
    skill_table(SkillTable),
    proficiency_list(ProfList),
    trait_list(TraitList),
    custom_sections(Custom),
    resource_table(ResourceTable),
    attack_table(AttackTable),
    spell_slot_table(SpellSlotTable),
    spell_preparation_table(SpellPrepTable),
    spell_table(SpellTable).

% Ability table.
ability_table(Table) :- 
    table('abilities', 'Abilities', Contents, Table),
    Contents = [tr([th([]), th('Score'), th('Mod'), th('ST')])|Rows],
    findall(Row, ability_table_row(_, Row), Rows).
ability_table_row(Abil, tr([th(AbilHdr), td(Score), td(Mf), td(ST)])) :-
    ability(Abil, Score),
    ability_hdr(Abil, AbilHdr),
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

maybe_tooltip(Subject, Text, WithTooltip) :-
    (Subject ?= Tooltip),
    tooltip(Text, Tooltip, WithTooltip).
maybe_tooltip(Subject, Text, Text) :-
    \+ (Subject ?= _).

% Traits.
trait_list(p([h3("Notable traits"), div([id=traits], ul(Items))])) :-
    findall(li(Item), trait_list_entry(Item), Items).
trait_list_entry(div(class=tooltip, [Trait, span(class=tooltiptext, Desc)])) :-
    trait(TraitVal),
    \+ member(TraitVal, [language(_), tool(_), weapon(_), armor(_)]),
    fmt(format_trait(TraitVal), Trait),
    %format_trait(TraitVal, Trait),
    TraitVal ?= Desc.

format_trait(T) --> format_term(T), [' ('], summary(T), !, [')'].
format_trait(T) --> format_term(T).

% Custom sections.
custom_sections(Sections) :-
    findall(Section, custom_section(Section), Sections).

% Resources
resource_table(Table) :-
    table('resources', 'Resources', [tr(Resources), tr(Boxes)], Table),
    findall(th(Res)-td(Boxes),
            (resource(ResVal,N), format(ResVal,Res), checkboxes(N,Boxes)),
            Pairs),

    maplist([X,Y]>>(X = (Y-_)), Pairs, Resources),
    maplist([X,Y]>>(X = (_-Y)), Pairs, Boxes).
    %maplist(\X^Y^(X = (Y-_)), Pairs, Resources),
    %maplist(\X^Y^(X = (_-Y)), Pairs, Boxes).

% Attacks.
attack_table(Table) :-
    table('attacks', 'Attacks', [Header|Rows], Table),
    Header = tr([th([]), th('Range'), th('To Hit'), th('Damage'), th('Notes')]),
    findall(Row, attack_table_row(Row), Rows).
attack_table_row(tr([td(Name), td(Range), td(ToHit), td(DamageFmt), td(FNotes)])) :-
    attack(AttackName, Range, ToHitVal, Damage, Notes),
    format_list(Notes, FNotes, []),
    format(AttackName, Name),
    format_bonus(ToHitVal, ToHit, []),
    fmt(format_damage(Damage), DamageFmt).

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
    spell_table_rows(Rows).

spell_table_rows(Rows) :-
    findall((Level,Name)-Row, spell_table_row(Name, Level, Row), LRows),
    sort(1, @=<, LRows, SortedLRows),
    maplist([_-R,R] >> true, SortedLRows, Rows).

spell_table_row(Name, SpellLevel, tr([td(Prepared),
                                      td(SpellLevel),
                                      td(Source),
                                      td(div(class=tooltip,
                                             [Name, span(class=tooltiptext, Desc)])),
                                      td(CastingTime),
                                      td(Range),
                                      td(Components),
                                      td(Duration),
                                      td(ToHitOrDC),
                                      td(Effects),
                                      td(Resource)
                                      ])) :-
    spell_known(Name, Source, _Ability, PrepVal, ResourceVal),
    spell(Name, level, SpellLevel),
    spell(Name, desc, Desc),
    spell(Name, casting_time, CastingTime),
    spell(Name, range, RangeVal), format_range(RangeVal, Range),
    spell(Name, components, ComponentsVal), format_components(ComponentsVal, Components),
    spell(Name, duration, Duration),
    spell_to_hit_or_dc(Name, Source, ToHitOrDC),
    findall(Effect, spell_known_effect(Name,Source,Effect), EffectsVal), fmt(format_terms(EffectsVal), Effects),
    format_prepared(PrepVal, Prepared),
    format_resource(ResourceVal, Resource).
spell_to_hit_or_dc(Name, Source, [+, ToHit]) :-
    spell_to_hit(Name, Source, ToHit), !.
spell_to_hit_or_dc(Name, Source, ['DC ', DC, ' ', Ability]) :-
    spell_dc(Name, Source, Ability, DC), !.
spell_to_hit_or_dc(_, _, "-").

format_range(feet(X), [X, ' ft']) :- !.
format_range(miles(X), [X, ' mi']) :- !.
format_range(X, X).

format_components(Cs, Format) :-
    maplist(format_component, Cs, Format).
    %format_list(Formats, Format, []).
format_component(m(M), span(class=tooltip, [m, span(class=tooltiptext, M)])).
format_component(C, C) :- C \= m(_).

format_prepared(when_prepared, input(type=checkbox,[])).
format_prepared(always_available, 'always').

format_resource(at_will, 'at will').
format_resource(spell_slot, slot).

tooltip(Text, Tooltip, div(class=tooltip, [Text, span(class=tooltiptext, Tooltip)])).

% Helper predicates.
table(Id, Caption, Contents, table(id=Id, [caption(h3(Caption))|Contents])).

checkboxes(N, Boxes) :-    
    repl(input(type=checkbox, []), N, Boxes).

% Clever little predicate, taken from
% http://stackoverflow.com/questions/23176840/easily-replicate-an-element-in-prolog
repl(X, N, L) :-
    length(L, N),
    maplist(=(X), L).
