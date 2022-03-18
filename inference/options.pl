:- multifile
       options/3,
       options/4,
       options_source/3,
       choice/3,
       hide_option/3.

%! options(?Source, ?Id, ?Spec)
%  
%  Each options/3 clause indicates that a choice/3 needs to be made to
%  advance the PC. Source indicates the reason why the options are
%  available. Source and Id together uniquely identify the choice to
%  be made. Spec is a unary predicate such that Spec(Choice) is true
%  only if Choice is a valid choice.
%  A todo is generated for every options/3 clause without matching
%  choice/3 clause.
options(_,_,_) :- false.
todo(options(Origin, Id, Spec)) :-
    % Generate a todo for each options clause without corresponding choice clause.
    options(Origin, Id, Spec),
    \+ choice(Origin, Id, _).


%! options_source(?Source, ?Id, ?Spec)
%
%  Each options_source/3 clause gives rise to a corresponding
%  options/3 clause, *if* `call(Source)` is true.
options_source(_,_,_) :- false.
% Generate options for the options_source clauses where the requirement is matched.
options(Source, Id, Spec) :-
    options_source(Source, Id, Spec),
    call(Source).

%! choice(?Source, ?Id, ?Choice)
%
%  Each choice clause represents a choice the user made to advance the
%  PC. A choice/3 clause is merely an assertion that the user wants to
%  make a given choice, not that that choice is valid.
%  A choice is valid if
%  * there is a corresponding options/3 clause `options(Source, Id, Spec)`,
%  * the Choice matches the options clause's Spec (see choice_matches_spec/3), and
%  * there isn't another separate choice/3 clause with the same Origin and Id.
% 
%  If a choice is invalid, a problem/1 is flagged.
choice(_,_,_) :- false.
problem(choice_double_dip(Origin, Id, Choices)) :-
    % Flag a problem if we have multiple choice/3 clauses for one options/3 clause.
    options(Origin, Id, _),
    findall(Choice, choice(Origin, Id, Choice), Choices),
    length(Choices, Len),
    Len > 1.
problem(not_eligible(Origin, Id, Choice)) :-
    % Flag a problem if we have a choice/3 clause without corresponding options/3 clause.
    choice(Origin, Id, Choice),
    \+ options(Origin, Id, _).
problem(choice_does_not_match_spec(Origin, Id, Choice)) :-
    % Flag a problem if a choice clause does not match the spec of the corresponding options clause.
    choice(Origin, Id, Choice),
    \+ choice_matches_spec(Origin, Id, Choice).
choice(_,_) :- false. % Suppress a warning which I think is caused a bug in swipl.
problem(chosen_same_trait_twice(Origin1, Origin2, Id1, Id2, Trait)) :-
    % Flag a problem if the same trait is chosen twice.
    trait(choice(Origin1, Id1), Trait),
    trait(choice(Origin2, Id2), Trait),
    (Origin1 \= Origin2 ; Id1 \= Id2).

%! choice_matches_spec(?Origin, ?Id, ?Choice)
%
%  True for every possible Choice the user can make, for a given
%  options/3 clause with matching Origin and Id. So,
%  choice_matches_spec(Origin, Id, Choice) does not imply choice(Origin, Id,
%  Choice).
choice_matches_spec(Origin, Id, Choice) :-
    options(Origin, Id, Spec),
    call(Spec, Choice).

%! choice_member(?Origin, ?Id, ?Choice)
%
%  For some choice(Origin, Id, Choices) clauses, Choices is a list of
%  individual choices. choice_member(Origin, Id, Choice) retrieves the
%  individual elements if Choices is a list; if it is not, it just
%  retrieves Choices.
choice_member(Origin, Id, Choice) :-
    choice(Origin, Id, C),
    (member(Choice, C); \+ is_list(C), Choice = C).

%! selected_at_class_level(?ClassLevel, ?Id, ?Choice)
%
%  Some choices can be replaced at a later moment.
%  For example, a sorcerer may choose to forget one spell
%  to gain another every level starting from level 2.
%  This predicate helps keep track of what is selected
%  at which class level by looking at the current and past
%  class levels.
selected_at_class_level(Class:Level, Id, Choice) :-
    class_origin_to_class_level(Origin, Class:Level),
    (choice_member(Origin, Id, Choice) ; choice_member(Origin, replacing(Id,_), Choice)).
selected_at_class_level(Class:Level, Id, Choice) :-
    class_level(Class:CurLevel),
    between(2, CurLevel, Level), % ground Level
    PrevLevel is Level-1,
    selected_at_class_level(Class:PrevLevel, Id, Choice),
    \+ (class_origin_to_class_level(Origin, Class:Level),
        choice_member(Origin, replace(Id), Choice)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper predicates

%! from(+N:int, :Pred, ?Choices)
%  Helper predicate to support the infix `N from Pred` notation in
%  options/3 specifications.
from(1, Spec, Choice) :-
    \+ is_list(Choice),
    !,
    from_(1, Spec, [Choice]).
from(N, Spec, Choice) :-
    from_(N, Spec, Choice).
from_(N, List, Choices) :-
    is_list(List),
    !,
    length(Choices, N),
    subset(Choices, List).
from_(N, Pred, Choices) :-
    length(Choices, N),
    maplist(call(Pred), Choices).

%! unique_from(+N:int, :Pred, ?Choices)
%  Like from/3, but only true if each choice in Choices is unique.
unique_from(N, Pred, Choices) :-
    from(N, Pred, Choices),
    (is_set(Choices) ; N = 1).
         
%! from_list(?List, ?Elem)
%  Like member/2, but with the arguments flipped.
%  Useful for options/3 specifications.
from_list(L, X) :- member(X, L).

or(Goal1, Goal2, X) :-
    call(Goal1, X) ; call(Goal2, X).

%! inspect_options(?Origin, ?Id, ?Desc)
%  Desc describes all valid choices for the options/3 clause with
%  matching Origin and Id.
%  The format of Desc is one of:
%  - `N unique_from D`; with `N` an integer and `D` a sub-escription,
%  - `N from D`; with `N` an integer and `D` a sub-escription,
%  - A list of terms.
inspect_options(Origin, Id, Desc) :-
    options(Origin, Id, Spec), % ground
    inspect_spec(Origin, Id, Spec, Desc).
inspect_spec(Origin, Id, N from Pred, N from List) :-
    inspect_spec(Origin, Id, Pred, List).
inspect_spec(Origin, Id, N unique_from Pred, N unique_from List) :-
    inspect_spec(Origin, Id, Pred, List).
inspect_spec(Origin, Id, Pred, List) :-
    \+ member(Pred, [_ from _, _ unique_from _]),
    findall(X, (call(Pred, X), \+ hide_option(Origin, Id, X)), List).

%! hide_option(?Source, ?Id, ?Option)
%
%  True iff Option shouldn't be displayed to the user as a valid
%  choice for the option with given Source and Id.
hide_option(_,_,_) :- false.
