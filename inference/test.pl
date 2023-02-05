:- multifile test_char_level/4.
:- dynamic most_recent_test_character/1.

:- [test_characters/cleric_hill_dwarf].
:- [test_characters/ranger_human].

% test_char_level(?Name, ?Level, ?Facts, ?Expectations)
test_char_level(_,_,_,_) :- false.

test_character(Name) :-
    unload_current_character,
    findall(Level-Facts-Expectations,
            test_char_level(Name, Level, Facts, Expectations),
            UnsortedScenario),
    sort(1, @<, UnsortedScenario, TestScenario),
    forall(member(_-Facts-Expectations, TestScenario),
           test_character_level(Facts, Expectations)).
test_character_level(Facts, Expectations) :-
    forall(member(Fact,Facts), assert(Fact)),
    forall(member(E,Expectations), test_expectation(E)),
    findall(Problem, (problem(Problem), write('Problem: '), writeln(Problem)), []),
    findall(Todo, (todo(Todo), write('Todo: '), writeln(Todo)), []).
test_expectation(Expectation) :-
    call(Expectation)
    -> true
    ;  (write('Failed: '), write(Expectation), false).

tc(Name) :-
    retractall(most_recent_test_character(_)),
    assert(most_recent_test_character(Name)),
    test_character(Name).

tc :-
    most_recent_test_character(Name),
    test_character(Name).

tac :-
    findall(Char, test_char_level(Char,_,_,_), UChars),
    sort(0, @<, UChars, Chars),
    forall(member(Char, Chars),
           (write("Testing "), writeln(Char), test_character(Char))).
