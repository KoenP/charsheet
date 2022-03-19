
origin_category(class(Class), Origin) :-
    class_option(Class),
    class_origin_to_class(Origin, Class),
    !.
origin_category(race(Race), race(Race)) :- !.
origin_category(Category, trait(Trait)) :-
    !,
    trait(Origin, Trait),
    origin_category(Category, Origin).

%! origin_category_or_uncategorized(Origin, Category)
%
%  Like origin_category/2, but instead of failing if no category can
%  be found, succeed with the category "uncategorized".
origin_category_or_uncategorized(Category, Origin) :-
    origin_category(Category, Origin),
    !.
origin_category_or_uncategorized(uncategorized, _).

