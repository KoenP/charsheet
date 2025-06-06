:- multifile
       replaceable_class_options/3,
       replace_at_class_level/4,
       replaceable_character_options/4,
       replace_at_character_level/5,
       replaceable_autoselected_option/3.

custom_format(replacing(Id, ToReplace)) -->
    format_term(Id), [": replace \""], format_term(ToReplace), ["\""].
custom_format(replace(Atom)) -->
    {atom(Atom), !}, ["replace "], [Atom].
custom_format(replace(Compound)) -->
    {Compound =.. [Functor, Var], var(Var), !}, ["replace "], [Functor].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CLASSLEVEL-BOUND
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! selected_at_class_level(?ClassLevel, ?Id, ?Choice)
%
%  Some choices can be replaced at a later moment. For example, a sorcerer may
%  choose to forget one spell to gain another every level starting from level 2.
%  This predicate helps keep track of what is selected at which class level by
%  looking at the current and past class levels.
selected_at_class_level(Class:Level, Id, Choice) :-
    base_class(Class, BaseClass),
    class_origin_to_class_level(Origin, BaseClass:Level),
    (replaceable_choice_member(Origin, Id, Choice) ; replaceable_choice_member(Origin, replacing(Id,_), Choice)).
selected_at_class_level(Class:Level, Id, Choice) :-
    base_class(Class, BaseClass),
    class_level(BaseClass:CurLevel),
    between(2, CurLevel, Level), % ground Level
    PrevLevel is Level-1,
    selected_at_class_level(Class:PrevLevel, Id, Choice),
    \+ (class_origin_to_class_level(Origin, BaseClass:Level),
        replaceable_choice_member(Origin, replace(Id), Choice)).


%! replaceable_class_options(?ClassLevel, ?Id, ?Goal)
replaceable_class_options(_,_,_) :- false.

%! replaceable_autoselected_option(?ClassLevel, ?Id, ?Autoselected)
replaceable_autoselected_option(_,_,_) :- false.

replaceable_choice(Origin, Id, Choice) :-
    choice(Origin, Id, Choice) ; replaceable_autoselected_option(Origin, Id, Choice).
replaceable_choice_member(Origin, Id, ChoiceMember) :-
    replaceable_choice(Origin, Id, Choice),
    (is_list(Choice) -> member(ChoiceMember, Choice) ; ChoiceMember = Choice).

%! replace_at_class_level(?ClassLevel, ?Id, ?N:integer, ?Goal)
replace_at_class_level(_,_,_,_) :- false.

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
    replace_at_class_level(Class:CurLvl, Id, _, Goal),
    base_class(Class, BaseClass),
    class_level(BaseClass:CurLvl),
    replaceable_choice_member(Class >: L, replace(Id), Choice).

% this name is horrible
find_choice_level(Class:Level, Id, Choice) :-
    base_class(Class, BaseClass),
    class_level(BaseClass:CurLevel),
    selected_at_class_level(BaseClass:CurLevel, Id, Choice),
    findall(L,
            (replaceable_choice_member(Class >: L, ChoiceId, Choice),
             member(ChoiceId, [Id, replacing(Id,_)])),
            Ls),
    max_member(Level, Ls).

hide_base_option(Origin, Id, Choice) :-
    hide_replaceable_base_option(Origin, Id, Choice).
hide_replaceable_base_option(Class >: L, Id, Choice) :-
    find_choice_level(Class:ChoiceLevel, Id, Choice),
    (class_level(Class:CurLevel) ; subclass_level(Class:CurLevel)),
    between(ChoiceLevel, CurLevel, L).
hide_replaceable_base_option(Class >: L, replacing(Id, _), Choice) :-
    find_choice_level(Class:ChoiceLevel, Id, Choice),
    (class_level(Class:CurLevel) ; subclass_level(Class:CurLevel)),
    between(ChoiceLevel, CurLevel, L).
hide_replaceable_base_option(_ >: _, replacing(_, Choice), Choice).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CHARLEVEL-BOUND
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
at(Origin, Level) :-
    call(Origin),
    level(CurLevel),
    CurLevel >= Level.

%! selected_at_current_level(?Origin, ?Id, ?Choice)
selected_at_current_level(Origin, Id, Choice) :-
    level(Level),
    selected_at_character_level(Origin, Level, Id, Choice).

%! selected_at_character_level(?Origin, ?Level, ?Id, ?Choice)
%
%  Determine if a given choice is selected at a given character level.
selected_at_character_level(Origin, Level, Id, Choice) :-
    choice_member(Origin, Id, Choice),
    origin_level(Origin, Level).
selected_at_character_level(Origin, Level, Id, Choice) :-
    choice_member(Origin at Level, replacing(Id,_), Choice).
selected_at_character_level(Origin, Level, Id, Choice) :-
    level(CurLevel),
    between(2, CurLevel, Level),
    PrevLevel is Level-1,
    selected_at_character_level(Origin, PrevLevel, Id, Choice),
    \+ (choice_member(Origin at Level, replacing(Id, Choice), _)).

%! replace_at_character_level(?Origin, ?CharLevel, ?Id, ?Num, ?Goal)
%
%  Each fact represents the opportunity for a character to replace an
%  earlier choice with a new choice. Goal defines the options for the
%  new choice.
replace_at_character_level(_,_,_,_,_) :- false.

options_source(Origin at Level,
               replace(Id),
               N unique_from selected_at_character_level(Origin, Prev, Id)) :-
    replace_at_character_level(Origin, Level, Id, N, _),
    Prev is Level-1.

options_source(Origin at Level, replacing(Id, Choice), Goal) :-
    replace_at_character_level(Origin, Level, Id, _, Goal),
    choice_member(Origin at Level, replace(Id), Choice).

