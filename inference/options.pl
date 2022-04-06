:- multifile
       options/3,
       options/4,
       options_source/3,
       choice/3,
       hide_base_option/3.

%! options(?CharLvl, ?Source, ?Id, ?Spec)
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
    is_list(Choices),
    length(Choices, N),
    subset(Choices, List).
from_(N, Pred, Choices) :-
    is_list(Choices),
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
    
%! option_todo(?Origin, ?Id, ?Spec)
options_todo(Origin, Id, Spec) :-
    options(Origin, Id, Spec),
    \+ choice(Origin, Id, _).

%! options_json(?Origin, ?Id, ?Json)
options_json(Origin, Id, _{origin: OriginStr, id: IdStr, spec: SpecJson}) :-
    options(Origin, Id, Spec),
    spec_to_json(Origin, Id, Spec, SpecJson),
    fmt(format_term(Origin), OriginStr),
    fmt(format_term(Id), IdStr).

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
               spec1: SubSpec1,
               spec2: SubSpec2}) :-
    !,
    spec_to_json(Origin, Id, Spec1, SubSpec1),
    spec_to_json(Origin, Id, Spec2, SubSpec2).
% Case: any other predicate.
spec_to_json(Origin, Id, Spec,
             _{spectype: list, list: List}) :-
    findall(XStr,
            (call(Spec, X),
             (\+ suppress_base_option(Origin, Id, X)),
             fmt(format_term(X), XStr)),
            List).

%options_spec_to_json(Origin, Id, Spec, Json) :-
%    inspect_spec(Origin, Id, Spec, Desc),
%    Id \= 'asi or feat',
%    desc_to_dict_pairs(Desc, Pairs),
%    choice_json(Origin, Id, ChoiceJson),
%    append([origin-Origin, id-Id, choice-ChoiceJson], Pairs, Assocs),
%    dict_pairs(Json, _, Assocs).
%options_spec_to_json(Origin, 'asi or feat', _,
%                     _{origin: Origin,
%                       id: 'asi or feat',
%                       spectype: 'asi or feat',
%                       asis: 2,
%                       feats: Feats}) :-
%    findall(Feat, selectable_feat_option(Feat), Feats).

choice_json(Origin, Id, Json) :-
    choice(Origin, Id, Choice),
    choice_to_json(Origin, Id, Choice, Json).
choice_json(Origin, Id, none) :-
    \+ choice(Origin, Id, _).
choice_to_json(_, 'asi or feat', feat(Feat), _{choicetype: feat, feat: Feat}) :- !.
choice_to_json(_, 'asi or feat', Abi+N, _{choicetype: asi, plus:N, abilities: [Abi]}) :- !.
choice_to_json(_, 'asi or feat', Asis, _{choicetype: asi, plus:N, abilities: Abis}) :-
    maplist([Abi+N,Abi,N]>>true, Asis, Abis, [N|Ns]),
    forall(member(M,Ns), N=M),
    !.
choice_to_json(_, _, Term, Json) :-
    term_to_json(Term, Json),
    !.
    
desc_to_dict_pairs(Desc, [spectype-"list", num-N, options-List]) :-
    ((Desc = [From, N, List], (From = from ; From = unique_from)))
    ;
    (is_list(Desc), List=Desc, N=1).
%# asi or feat
%  asi -> ofwel 2 attribute +1, ofwel 1 attribute +2
%  feat -> keuze uit een lijst
%
%  Top-level: radio buttons asi/feat
%    -> feat geselecteerd: dropdown lijst
%    -> asi geselecteerd hebt: tickboxes? tabel met "+" buttons
%
%[
%  {
%    origin: "rogue",
%    id: "asi or feat",
%    spec: {
%        spectype: "asi_or_feat",
%        asis: 2,
%        feats: ["alert", "durable", ...]
%    },
%    choice: {
%      choicetype: "asi",
%      plus: 1,
%      abilities: ["str", "dex"]
%    }
%    choice: { // OFWEL
%      choicetype: "feat",
%      feat: ["alert"]
%    }
%  }
%]
%
%
%# stel je mag 1 skill kiezen
%[ 
%  {
%    origin: "rogue",
%    id: "skill",
%    spec: {
%        spectype: "list",
%        num: 1,
%        options: [acrobatics, athletics, deception, ...]
%    },
%    choice: ["acrobatics"]
%  }
%]
%
%# stel je mag 4 skills kiezen
%[ 
%  {
%    origin: "rogue",
%    id: "skill",
%    spec: {
%        spectype: "list",
%        num: 4,
%        options: ["acrobatics", "athletics", "deception", ...]
%    },
%    // stel ik heb al twee skills gekozen
%    choice: ["acrobatics", "athletics"]
%  }
%]
