:- use_module(library(pldoc)).
:- use_module(library(yall)).

:- multifile
       gain_level/3,
       on_rest/3,
       todo/1,
       meta_todo/2,
       problem/1,
       resource/2,
       name/1.

:- op(650, xfx, from).
:- op(650, xfx, unique_from).
:- op(1000, xfx, ?=).
:- op(1000, xfx, ?=).

:- [dice].
:- [options].
:- [trait].
:- [bonus].
:- [spell_data].
:- [spellcasting].
:- [class].
:- [ac].
:- [html].

%! meta_todo(Source, Todo)
%
%  Predicate for helping find incomplete code.
%  For instance, we have declared a class_option(Class),
%  but there is no correspoinding hd_per_level(Class, HD),
%  then we should generate a meta_todo/2.
meta_todo(_,_) :- false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subrace(elf, 'high elf').
traits_from_source(race(elf), [darkvision, 
                               'fey ancestry',
                               'keen senses',
                               trance,
                               language(common),
                               language(elvish)]).
trait_source(trait('keen senses'), skill(perception)).
bonus_source(race(elf), dex+2).

bonus_source(race('high elf'), int+1).
traits_from_source(race('high elf'), [weapon(longsword),
                                      weapon(shortsword),
                                      weapon(shortbow),
                                      weapon(longbow)]).
%options_source(race('high elf'), cantrip, learnable_cantrip(wizard)).
%options_source(race('high elf'), language, language).
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
