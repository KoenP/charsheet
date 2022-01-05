:- use_module(library(pldoc)).
:- use_module(library(yall)).
:- use_module(library(pairs)).

:- multifile
       (?=)/2,
       gain_level/3,
       on_rest/3,
       todo/1,
       meta_todo/2,
       problem/1,
       resource/2,
       name/1.
:- op(500, xfy, or).
:- op(400, xfy, and).
:- op(700, xfx, else).
:- op(650, xfx, from).
:- op(650, xfx, from).
:- op(650, xfx, unique_from).
:- op(1000, xfx, ?=).
:- op(1000, xfx, ?=).
:- op(650, xfx, ft).

:- [util].
:- [dice].
:- [format].
:- [options].
:- [replace].
:- [trait].
:- [bonus].
:- [origin_category].
:- [spell_data].
:- [spellcasting].
:- [class].
:- [race].
:- [feat].
:- [ability].
:- [skill].
:- [ac].
:- [hp].
:- [html].

%! meta_todo(Source, Todo)
%
%  Predicate for helping find incomplete code.
%  For instance, we have declared a class_option(Class),
%  but there is no correspoinding hd_per_level(Class, HD),
%  then we should generate a meta_todo/2.
meta_todo(_,_) :- false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO!!
%learnable_cantrip(wizard, S) :- learnable_spell(wizard, S).
%options(class(warlock:3), 'pact boon', from_list([tome, blade, chain])).
%options(class(wizard:_), spell, 2 unique_from learnable_spell(wizard)).
%
%choice(class(warlock:3), 'pact boon', tome).
%choice(class(wizard:1), spell, ['fire bolt', prestidigitation]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Spells.

%options_source(class(sorcerer:_), spell, learnable_spell(sorcerer)).
%options_source(class(sorcerer:N), replace_spell, spell_known_at_class_level(sorcerer:Prev)) :-
%    between(2, 20, N),
%    Prev is N-1.
%%options_source(choice())
%
%known_spell_at_class_level(Class:Level, Name) :-
%    choice_member(class(Class:Level), spell, Name).
%known_spell_at_class_level(Class:Level, Name) :-
%    between(2, 20, Level),
%    PrevLevel is Level-1,
%    known_spell_at_class_level(Class:PrevLevel, Name),
%    \+ choice(class(Class:Level), replace_spell, Name).
%
%spell_prop(Name, Prop, Val) :-
%    spell(Name, Data),
%    Val = Data.Prop.
%known_spell_prop(Name, Prop, Val) :-
%    known_spell(Name, Data, _, _, _),
%    Val = Data.Prop.
%
%learn_spell(Class, Name, SpellData, always, [slot]) :-
%    current_class_level(Class:Level),
%    known_spell_at_class_level(Class:Level, Name),
%    spell(Name, SpellData).
%    
%known_spell(Source, Name, SpellData, Availability, Resources) :-
%    learn_spell(Source, Name, GenericSpellData, Availability, Resources),
%    findall(Mod, commutative_spell_modifier(Source, Name, Mod), Mods),
%    sequence(Mods, GenericSpellData, SpellData).
%
%commutative_spell_modifier(_,_,_) :- false.
%current_class_level(_) :- false.
%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Leveling up.
level(Level) :-
    findall(L, gain_level(L, _, _), Levels),
    max_member(Level, Levels).

match_level(Level) :-
    level(CurLevel),
    between(1, CurLevel, Level).

%! gain_level(?CharLevel, ?Class, ?HPMode)
%
%  Gain a level in the given Class. HPMode determines how your HP
%  increase is calculcated. Use hp_avg to gain the default "average"
%  hp for the given Class. Other options yet to be implemented.
gain_level(_,_,_) :- false.
problem(gain_level_not_contiguous(Levels)) :-
    findall(L, gain_level(L,_,_), Levels),
    max_member(Highest, Levels),
    findall(L, between(2,Highest,L), Levels2),
    Levels \= Levels2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Character initialization.

% Pick your initial class.
options(init, 'initial class', class_option).
initial_class(Class) :- choice(init, 'initial class', Class), !.
problem(multiple_initial_classes(Classes)) :-
    findall(Class, initial_class(Class), Classes),
    Classes = [_,_|_].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% To organize:
initiative(Init) :-
    ability_mod(dex, DexMod),
    sum_bonuses(init, Bonus),
    Init is DexMod + Bonus.

%! hit_dice(?HD)
hit_dice(HD) :-
    findall(CHD, hit_dice(_, CHD), CHDs),
    list_to_sum(CHDs, Sum),
    simplify_dice_sum(Sum, HD).
%! hit_dice(?Class:atomic, ?HD)
hit_dice(Class, M d X) :-
    class_level(Class:Level),
    hd_per_level(Class, N d X),
    M is N * Level.

%! speed(?Speed:int)
speed(Speed) :-
    race(Race),
    racial_speed(Race, BaseSpeed),
    findall(Bonus, bonus(speed+Bonus), Bonuses),
    sumlist([BaseSpeed|Bonuses], Speed).

%! proficiency_bonus(?Bonus:int)
proficiency_bonus(Bonus) :- level(Level), calc_bonus(Level, Bonus).
calc_bonus(Level, Bonus) :- Bonus is 2 + div(Level-1, 4).

%! passive_perception(?PP:int)
passive_perception(PP) :-
    skill(perception, Perception),
    PP is 10 + Perception.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Todo.
todo(problem:P) :- problem(P).

name(_) :- false.
problem('pick a name!') :- \+ name(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Resources. TODO: need to rethink resources
% * needs new name (clashes with builtin)
% * good hard think about arcane recovery needed: it can restore a
%   finite resource in a short rest, but only once per long rest, and
%   you get to pick which slots get restored.

%! resource(?Origin, ?Name, ?Max)
%
%  Name is a finite resource your character has, such as sorcery
%  points, second wind, ...
resource(_,_,_).

%! resource(?Name, ?Max)
%
%  Shorthand for resource/3, for when you're not interested in the
%  origin of a resource.
resource(Name, Max) :- resource(_, Name, Max).

%! on_rest(?Duration, ?ResourceName, ?Goal)
%
%  Documents the effects of long and short rests on resources.
%  Duration is either `short` or `long`.
%  ResourceName is the name of a resource such that resource(_,
%  ResourceName, _) is true.
%  Goal is a trinary predicate such that call(Goal, Max, Cur, New) is true when
%  Cur is the current value of a resource before resting (TODO:
%  current values aren't implemented right now), and New is the value
%  after resting.
on_rest(_,_,_) :- false.
meta_todo(resource(Resource), 'rest for nonexistant resource') :-
    on_rest(_, Resource, _),
    \+ resource(_, Resource, _).

%! full_restore(?Max, ?Cur, ?New)
%
%  Helper predicate to fully restore a resource. Usually used as third
%  argument to on_rest/3.
full_restore(Max, _, Max).

%! restore(?Max, ?Cur, ?New)
%
%  Helper predicate to partially restore a resource. Usually used as third
%  argument to on_rest/3, by partially applying the first argument
%  (for instance, writing restore(4) to restore a resource by 4
%  points).
restore(N, Max, Cur, New) :-
    New is min(Cur+N, Max).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Race.
most_specific_race(Race) :-
    race(Race),
    \+ subrace(Race, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Shorthands.
%search(X) :-
%    atom_to_chars(X, X1),
%    (Topic ?= Desc),
%    Topic =.. []
%    atom_to_chars(Topic, Topic1),
%    append([_, X1, _], Topic1),
%    writeln(Topic),
%    writeln(Desc).

todo :-
    forall(todo(T), writeln_quoted_term(T)).

spells :-
    forall(known_spell(Origin, Name), writeln_quoted_term(Origin:Name)).

mtodo :-
    forall(meta_todo(S,T), writeln_quoted_term(S->T)).

problems :-
    forall(problem(P), writeln_quoted_term(P)).

writeln_quoted_term(T) :-
    write_term(T, [quoted(true), fullstop(true), nl(true)]).

warn_if_problems :-
    forall(problem(P), (write("WARNING: "), writeln_quoted_term(P))).

describe_spell(Spell) :-
    spell_data(Spell, Data),
    write("* "), writeln(Spell),
    writeln(""),
    write("Casting time:\t"), writeln(Data.casting_time),
    write("Duration:\t"), writeln(Data.duration),
    write("Concentration:\t"), writeln(Data.concentration),
    write("Range:\t\t"), writeln(Data.range),
    write("Components:\t"), writeln(Data.components),
    write("Ritual:\t\t"), writeln(Data.ritual),
    write("School:\t\t"), writeln(Data.school),
    writeln(""),
    writeln(Data.desc),
    writeln(""),
    writeln("").
    
describe_new_learnable_spells(Class:Level) :-
    forall((learnable_proper_spell(Class,Spell),
            spell_property(Spell,level,Level),
            \+ known_spell(Class,Spell)),
           describe_spell(Spell)).

describe_class_cantrips(Class) :-
    forall(class_cantrip(Class, Spell), describe_spell(Spell)).
