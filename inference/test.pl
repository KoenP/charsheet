:- multifile test_char_level/4.
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

% test_char_level(?Name, ?Level, ?Facts, ?Expectations)
test_char_level(_,_,_,_) :- false.

test_character(Name) :- test_character(Name, true).
test_character(Name, Goal) :-
    unload_char,
    snapshot(
        ( findall(Level-Facts-Expectations,
                  test_char_level(Name, Level, Facts, Expectations),
                  UnsortedScenario),
          sort(1, @<, UnsortedScenario, TestScenario),
          forall(member(Level-Facts-Expectations, TestScenario),
                 test_character_level(Name, Level, Facts, Expectations)),
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
test_character_level(Name, Level, Facts, Expectations) :-
    abolish_all_tables,
    write("# "), write(Name), write(" lvl "), writeln(Level),
    forall(member(Fact,Facts), assert(Fact)),
    forall(member(E,Expectations), test_expectation(E)),
    findall(Problem, (problem(Problem), write('Problem: '), writeln(Problem)), []).
    %findall(Todo, (todo(Todo), write('Todo: '), writeln(Todo)), _).
test_expectation(Expectation) :-
    call(Expectation)
    -> true
    ;  (write('Failed: '), write(Expectation), false).

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

attack_with_sorted_notes(Name, Range, ToHitOrDC, DamageRolls, SortedNotes) :-
    attack(Name, Range, ToHitOrDC, DamageRolls, Notes),
    sort(Notes, SortedNotes).
