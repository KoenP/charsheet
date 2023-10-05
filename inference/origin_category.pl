origin_category(init, init).
origin_category(class(Class), Origin) :-
    class_option(Class),
    class_origin_to_class(Origin, Class).
origin_category(race(Race), race(Race)).
origin_category(background(BG), background(BG)).
origin_category(feat(F), feat(F)).
origin_category(Category, trait(Trait)) :-
    trait(Origin, Trait),
    origin_category(Category, Origin).
origin_category(Category, choice(Origin, _)) :-
    origin_category(Category, Origin).

origin_category_canonical_order(init, 0) :- !.
origin_category_canonical_order(race(_), 1) :- !.
origin_category_canonical_order(background(_), 2) :- !.
origin_category_canonical_order(class(_), 3) :- !.
origin_category_canonical_order(feat(_), 4) :- !.
origin_category_canonical_order(_, 5) :- !.

meta_todo(origin_category, "probably duplication with find_origin_class in class.pl").

%! origin_category_or_uncategorized(Origin, Category)
%
%  Like origin_category/2, but instead of failing if no category can
%  be found, succeed with the category "uncategorized".
origin_category_or_uncategorized(Category, Origin) :-
    origin_category(Category, Origin),
    !.
origin_category_or_uncategorized(uncategorized, _).

%! origin_level(?Origin, ?Level)
%
%  Determine what Level your character obtained a given Origin qualification.
origin_level(init, 1) :- !.
origin_level(^_, 1) :- !.
origin_level(initial_class(_), 1) :- !.
% TODO delete
origin_level(class(C), Level) :-
    !,
    reached_classlevel_at_charlevel(C:1, Level).
origin_level(match_class(C:L), Level) :-
    % TODO delete
    !,
    reached_classlevel_at_charlevel(C:L, Level).
origin_level(match_class(C), Level) :-
    % TODO delete
    C \= _:_,
    !,
    reached_classlevel_at_charlevel(C:1, Level).
origin_level(Class >: ClassLevel, Level) :-
    !,
    origin_level(match_class(Class:ClassLevel), Level).
origin_level(race(_), 1) :- !.
origin_level(background(_), 1) :- !.
origin_level(trait(Trait), TraitOriginLevel) :-
    !,
    trait(TraitOrigin, Trait),
    origin_level(TraitOrigin, TraitOriginLevel).
origin_level(choice(ChoiceOrigin, _), ChoiceOriginLevel) :-
    !,
    origin_level(ChoiceOrigin, ChoiceOriginLevel).
origin_level(_, unknown) :- !.


%! origin(?Origin)
%
%  Origin is a possible origin of a trait/bonus your character may have.
origin(Origin) :- trait_source(Origin, _).
origin(Origin) :- options_source(Origin, _, _).
origin(Origin) :- bonus_source(Origin, _).
