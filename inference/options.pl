:- multifile
       options/3,
       options_source/3,
       choice/3,
       choice_creates_category/3,
       ignore/2,
       hide_base_option/3,
       lookup_option_doc/4.
:- dynamic ignore/2.

% :- table (options/3, hide_base_option/3) as incremental.

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

%! todo(?Todo)
todo(options(Origin, Id, Spec)) :-
    % Generate a todo for each options clause without corresponding choice clause.
    options(Origin, Id, Spec),
    \+ (choice(Origin, Id, _); ignore(Origin, Id)).
todo(problem:P) :- problem(P).

%! ignore(?Origin, ?Id)
%
%  Indicate that you deliberately don't want to pick an option for a
%  given options/3 with the matching Origin and Id.
ignore(_,_) :- false.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper predicates

%! from(+N:int, :Pred, ?Choices)
%  Helper predicate to support the infix `N from Pred` notation in
%  options/3 specifications.
from(1, Spec, Choice) :-
    \+ is_list(Choice),
    from_(1, Spec, [Choice]).
from(N, Spec, Choice) :-
    from_(N, Spec, Choice).
from_(N, List, Choices) :-
    is_list(List),
    !,
    is_list(Choices),
    length(Choices, N),
    subset(Choices, List).
from_(N, Pred, Choices) :-
    is_list(Choices),
    between(1, N, M), % TODO this is inefficient if M is known
    length(Choices, M),
    %M =< N,
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

%! free_choice(?Choice)
%
%  True for all Choice.
free_choice(_).

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
inspect_spec(Origin, Id, Spec1 or Spec2, Desc1 or Desc2) :-
    inspect_spec(Origin, Id, Spec1, Desc1),
    inspect_spec(Origin, Id, Spec2, Desc2).
inspect_spec(Origin, Id, Pred, List) :-
    \+ member(Pred, [_ from _, _ unique_from _]),
    findall(X, (call(Pred, X), (\+ suppress_base_option(Origin, Id, X))), List).
suppress_base_option(Origin, Id, X) :-
    hide_base_option(Origin, Id, X), (\+ choice_member(Origin, Id, X)).

%! hide_base_option(?Source, ?Id, ?Option)
%
%  True iff Option shouldn't be displayed to the user as a valid
%  choice for the option with given Source and Id.
hide_base_option(_,_,_) :- false.

%! lookup_option_doc(-Source, -Id, -Option, +Doc)
%
%  Find the documentation associated with a given Option for a given Source and Id.
lookup_option_doc(_, _, Option, Doc) :-
    (Option ?= Doc).
    
%! option_todo(?Origin, ?Id, ?Spec)
options_todo(Origin, Id, Spec) :-
    options(Origin, Id, Spec),
    \+ choice(Origin, Id, _).

%! options_by_level_json(?Opts)
all_options_by_level_json(Out) :-
    findall(Json,
            ( options(Orig, Id, _),
              options_json(Orig, Id, Json) ),
            Jsons),
    level(Level),
    findall(LAtom-[], (between(1,Level,L), atom_number(LAtom, L)), InitDictEntries),
    dict_create(Init, _, InitDictEntries),
    extend_multimap(Init,
                    [D,LAtom]>>(dict_get_pred(D,charlevel,L), atom_number(LAtom,L)),
                    Jsons,
                    Out).

dict_get_pred(Dict, Field, Val) :-
    Dict.get(Field) = Val.

%! options_json(?Origin, ?Id, ?Json)
options_json(Origin, Id, _{origin: OriginStr,
                           origin_category: CategoryStr, display_origin_category: DisplayCategory,
                           origin_category_index: CatIdx,
                           charlevel: CharLevel, id: IdStr, display_id: DisplayId, spec: SpecJson,
                           choice: ChoiceJson}) :-
    options(Origin, Id, Spec),
    origin_category_or_uncategorized(Category, Origin), term_string(Category, CategoryStr),
    fmt(format_term(Category), DisplayCategory),
    origin_category_canonical_order(Category, CatIdx),
    origin_level(Origin, CharLevel),
    spec_to_json(Origin, Id, Spec, SpecJson),
    choice_json(Origin, Id, Spec, ChoiceJson),
    term_string(Origin, OriginStr),
    term_string(Id, IdStr),
    fmt(format_term(Id), DisplayId).

%! resolve_not_eligible
%
%  If there are any choice/3 facts for which the PC is not eligible,
%  retract them.
resolve_not_eligible :-
    forall(problem(not_eligible(A,B,C)),
           (format(user_output, "Retracted ~w!~n", [choice(A,B,C)]),
            flush_output(user_output),
            retractall(choice(A,B,C)))).

% Case: `from` or `unique_from` spec.
spec_to_json(Origin, Id, Spec,
             _{spectype: Functor,
               num: N,
               spec: SubSpec1}) :-
    Spec =.. [Functor, N, SubSpec],
    (Functor = from ; Functor = unique_from),
    !,
    spec_to_json(Origin, Id, SubSpec, SubSpec1).
% Case: `or` spec.
spec_to_json(Origin, Id, Spec1 or Spec2,
             _{spectype: or,
               left: SubSpec1,
               right: SubSpec2,
               leftname: LeftName,
               rightname: RightName}) :-
    !,
    spec_to_json(Origin, Id, Spec1, SubSpec1),
    spec_to_json(Origin, Id, Spec2, SubSpec2),
    term_string(Spec1, LeftName),
    term_string(Spec2, RightName).
% Case: any other predicate.
spec_to_json(Origin, Id, Spec,
             _{spectype: list, list: List}) :-
    findall(_{opt: XStr, desc: Desc},
            (call(Spec, X),
             (\+ suppress_base_option(Origin, Id, X)),
             term_string(X, XStr),
             default_on_fail("", lookup_option_doc(Origin, Id, X), Desc)),
             %fmt(format_term(X), XStr)),
            List).

choice_json(Origin, Id, Spec, Json) :-
    choice(Origin, Id, Choice),
    options(Origin, Id, Spec),
    choice_to_json(Choice, Spec, Json).
choice_json(Origin, Id, _, null) :-
    \+ choice(Origin, Id, _).
choice_to_json(X, Left or _, _{choicetype: or, side: left, choice: Json}) :-
    ground(Left),
    call(Left, X),
    !,
    choice_to_json(X, Left, Json).
choice_to_json(X, _ or Right, _{choicetype: or, side: right, choice: Json}) :-
    ground(Right),
    call(Right, X),
    !,
    choice_to_json(X, Right, Json).
choice_to_json(List, Pred, JsonList) :-
    is_list(List),
    !,
    maplist([X,Y]>>choice_to_json(X,Pred,Y), List, JsonList).
choice_to_json(X, _, XStr) :-
    term_string(X, XStr).
    
desc_to_dict_pairs(Desc, [spectype-"list", num-N, options-List]) :-
    ((Desc = [From, N, List], (From = from ; From = unique_from)))
    ;
    (is_list(Desc), List=Desc, N=1).

fill_in_all_options :-
    fill_in_option(Origin, Id, Choice),
    writeln(Origin : Id : Choice),
    fill_in_all_options.

fill_in_option(Origin, Id, Choice) :-
    todo(options(Origin, Id, Spec)),
    call(Spec, Choice), !,
    assert(choice(Origin, Id, Choice)).
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OPTION TREE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO work on this some time.

% option_tree([ init : 'initial class' : ClassOpts, 
%               init : 'base race'     : RaceOpts,
%               init : background      : BackgroundOpts
%             ]) :-
%     class_option_tree(ClassOpts),
%     race_option_tree(RaceOpts),
%     background_option_tree(BackgroundOpts).
% 
% class_option_tree() :-
%     
% race_option_tree([]).
% background_option_tree([]).
% 
% %! choice_creates_category(Origin, Id, )
% choice_creates_category(_, _, _) :- false.
