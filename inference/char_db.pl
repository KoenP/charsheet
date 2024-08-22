:- module(char_db,
          [ record_name/2,
            record_base_ability/3,
            record_gain_level/4,
            withdraw_gain_level/4,
            record_choice/4,
            withdraw_choice/4,
            name/2,
            base_ability/3,
            choice/4,
            gain_level/4,
            has/2,
            list_characters/1,
            create_character/2,
            record_has/2,
            withdraw_has/2,
            show_inventory/2
          ]).

:- use_module(library(persistency)).
:- use_module(library(uuid)).

:- persistent
     name(char_id:any, name:atomic),
     base_ability(char_id:any, ability:atom, score:positive_integer),
     choice(char_id:any, origin:any, id:any, choice:any),
     gain_level(char_id:any, level:positive_integer, class:atom, hp_mode:atom),
     has(char_id:any, item:any).

:- db_attach('db.pl', []).

name(_,_) :- false.
base_ability(_,_,_) :- false.
choice(_,_,_,_) :- false.
gain_level(_,_,_,_) :- false.

current_level(CharId, CurLevel) :-
    findall(Level, gain_level(CharId, Level, _, _), Levels),
    ((max_member(CurLevel, Levels), !); CurLevel = 1).

record_name(CharId, Name) :-
    assert_name(CharId, Name).

record_base_ability(CharId, Ability, Score) :-
    ground(CharId),
    ground(Ability),
    number(Score),
    retractall_base_ability(CharId, Ability, _),
    assert_base_ability(CharId, Ability, Score).

record_gain_level(CharId, Level, Class, HpMode) :-
    ground(CharId),
    number(Level),
    retractall_gain_level(CharId, Level, _, _),
    assert_gain_level(CharId, Level, Class, HpMode).

withdraw_gain_level(CharId, Level, Class, HpMode) :-
    ground(CharId),
    number(Level),
    retract_gain_level(CharId, Level, Class, HpMode).

record_choice(CharId, Origin, Id, Choice) :-
    ground(CharId),
    ground(Origin),
    ground(Choice),
    retractall_choice(CharId, Origin, Id, _),
    assert_choice(CharId, Origin, Id, Choice).

withdraw_choice(CharId, Origin, Id, Choice) :-
    ground(CharId),
    ground(Origin),
    ground(Id),
    retract_choice(CharId, Origin, Id, Choice).

record_has(CharId, Item) :-
    ground(CharId),
    ground(Item),
    assert_has(CharId, Item).

withdraw_has(CharId, Item) :-
    ground(CharId),
    ground(Item),
    retract_has(CharId, Item).

show_inventory(CharId, Items) :-
    ground(CharId),
    findall(I, (has(CharId, Tm), term_string(Tm, I)), Items).

list_characters(List) :-
    setof(_{char_id: Id, name: Name}, name(Id, Name), List).

%! create_character(+Name, -Uuid)
create_character(Name, Uuid) :-
    uuid(Uuid),
    assert_name(Uuid, Name),
    forall(ability(Ability), assert_base_ability(Uuid, Ability, 10)).

