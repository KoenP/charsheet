


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

meta_todo(origin_category, "probably duplication with find_origin_class in class.pl").

%! origin_category_or_uncategorized(Origin, Category)
%
%  Like origin_category/2, but instead of failing if no category can
%  be found, succeed with the category "uncategorized".
origin_category_or_uncategorized(Category, Origin) :-
    origin_category(Category, Origin),
    !.
origin_category_or_uncategorized(uncategorized, _).

origin_level(init, 1) :- !.
origin_level(initial_class(_), 1) :- !.
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
origin_level(_, unknown) :- !.
