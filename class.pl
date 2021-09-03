:- multifile
       class_option/1.

% Druid example (to move)
class_option(druid).
subclass_options(druid:2, [land, moon]).

% Determine classes and class levels.
class_level(Class:ClassLevel) :-
    class_option(Class), % ground
    findall(L, (base_class(_) ; gain_level(L, Class, _)), Levels),
    length(Levels, ClassLevel),
    ClassLevel > 0.
    
class(Class) :- class_level(Class:_).

% Determine subclasses and subclass levels.
options(match_class(Class:ClassLevel), subclass, from_list(SubclassOptions)) :-
    subclass_options(Class:ClassLevel, SubclassOptions).
subclass(Subclass) :-
    subclass_options(Class:_, _),
    choice(match_class(Class:_), subclass, Sub),
    Subclass =.. [Class, Sub].
subclass_level(Subclass:Level) :-
    subclass(Subclass),
    Subclass =.. [Class, _],
    class_level(Class:Level).

% Deterimine whether the PC matches a class/subclass (level) requirement.
match_class(C:L1) :-
    (class_level(C:L2) ; subclass_level(C:L2)),
    between(1, L2, L1).
match_class(C) :-
    class(C) ; subclass(C).
