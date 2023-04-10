:- multifile
       replaceable_class_options/3,
       replace_at_class_level/3.
       

%! replaceable_class_options(?ClassLevel, ?Id, ?Goal)
replaceable_class_options(_,_,_) :- false.

%! replace_at_class_level(?ClassLevel, ?Id, ?N:integer, ?Goal)
replace_at_class_level(_,_,_) :- false.

% ADD NEW
options_source(Class >: L, Id, Goal) :-
    replaceable_class_options(Class:L, Id, Goal).

% WANT TO REPLACE?
options_source(Class >: L,
               replace(Id),
               N unique_from selected_at_class_level(Class:Prev, Id)) :-
    replace_at_class_level(Class:L, Id, N, _),
    Prev is L-1.

% WHAT TO REPLACE WITH?
options(Class >: L, replacing(Id, Choice), Goal) :-
    class_level(Class:CurLvl),
    replace_at_class_level(Class:CurLvl, Id, _, Goal),
    choice_member(Class >: L,
                  replace(Id),
                  Choice).

% this name is horrible
find_choice_level(Class:Level, Id, Choice) :-
    class_level(Class:CurLevel),
    selected_at_class_level(Class:CurLevel, Id, Choice),
    findall(L,
            (choice_member(Class >: L, ChoiceId, Choice),
             member(ChoiceId, [Id, replacing(Id,_)])),
            Ls),
    max_member(Level, Ls).
