:- multifile test_char_level/4.
:- multifile test_char_level/5.
:- dynamic test_char_level_up/3.
:- dynamic most_recent_test_character/1.

:- [test_characters/armor_tests].
:- [test_characters/cleric_hill_dwarf].
:- [test_characters/druid_elf].
:- [test_characters/monk].
:- [test_characters/paladin].
:- [test_characters/ranger_human].
:- [test_characters/extra_attack].
:- [test_characters/metamagic_adept].
:- [test_characters/cantrip_notes].
:- [test_characters/infusions].

% test_char_level(?Name, ?Level, ?Facts, ?Expectations)
test_char_level(_,_,_,_) :- false.

% test_char_level(?Name, ?Level, ?Facts, ?Expectations, ?ExpectedProblems)
test_char_level(Name, Level, Facts, Expectations, []) :-
    test_char_level(Name, Level, Facts, Expectations).

test_character(Name) :- test_character(Name, true).
test_character(Name, Goal) :-
    unload_char,
    snapshot(
        ( findall(Level-Facts-Expectations-ExpectedProblems,
                  test_char_level(Name, Level, Facts, Expectations, ExpectedProblems),
                  UnsortedScenario),
          sort(1, @<, UnsortedScenario, TestScenario),
          forall(member(Level-Facts-Expectations-ExpectedProblems, TestScenario),
                 test_character_level(Name, Level, Facts, Expectations, ExpectedProblems)),
          call(Goal),
          abolish_all_tables
        )).

%:-
%    unload_current_character,
%    findall(Level-Facts-Expectations,
%            test_char_level(Name, Level, Facts, Expectations),
%            UnsortedScenario),
%    sort(1, @<, UnsortedScenario, TestScenario),
%    forall(member(Level-Facts-Expectations, TestScenario),
%           test_character_level(Name, Level, Facts, Expectations)),
%    abolish_all_tables.
test_character_level(Name, Level, Facts, Expectations, ExpectedProblems) :-
    abolish_all_tables,
    write("# "), write(Name), write(" lvl "), writeln(Level),
    forall(member(Fact,Facts), assert(Fact)),
    forall(member(E,Expectations), test_expectation(E)),
    findall(Problem, (problem(Problem), write('Problem: '), writeln(Problem)), ExpectedProblems),
    !.
    %findall(Todo, (todo(Todo), write('Todo: '), writeln(Todo)), _).

test_expectation(Expectation) :-
    call(Expectation)
    -> true
    ;  (write('Failed:\n\t'), write_term(Expectation, [quoted(true), fullstop(true), nl(true)]), false).

tc(Name) :- tc(Name, true).
tc(Name, Goal) :-
    retractall(most_recent_test_character(_)),
    assert(most_recent_test_character(Name)),
    test_character(Name, Goal).

trc :- trc(true).
trc(Goal) :-
    most_recent_test_character(Name),
    test_character(Name, Goal).

tac :-
    findall(Char, test_char_level(Char,_,_,_), UChars),
    sort(0, @<, UChars, Chars),
    forall(member(Char, Chars),
           (write("Testing "), writeln(Char), test_character(Char))).

% Convenience predicates to interact with a test character definition by loading it into the shell.
ltc :-
    most_recent_test_character(Name),
    ltc(Name).
ltc(Name) :- ltc(Name, 20).
ltc(Name, Level) :-
    unload_char,
    test_char_level(Name, _, _, _), % Fail if the character does not exist.
    !,
    forall((test_char_level(Name, L, Facts, _), L =< Level, member(Fact, Facts)),
           assert(Fact)).

unload_char :-
    forall((character_definition_predicate(Pred/N),
            length(Args, N),
            Term =.. [Pred | Args]),
           retractall(Term)),
    abolish_all_tables.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

choice(level(Level), 'as class', Class) :-
    test_char_level_up(Level, Class, _).
choice(level(Level), 'max hp roll'(con(Con), avg(Avg)), Avg) :-
    test_char_level_up(Level, _, hp_avg),
    options(level(Level), 'max hp roll'(con(Con), avg(Avg)), _).


attack_with_sorted_notes(Name, Range, ToHitOrDC, DamageRolls, SortedNotes) :-
    attack(Name, Range, ToHitOrDC, DamageRolls, Notes),
    sort(Notes, SortedNotes).
