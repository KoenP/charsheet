:- use_module(library(pldoc)).

:- multifile
       gain_level/3,
       todo/1,
       meta_todo/2,
       problem/1.

:- op(650, xfx, from).
:- op(650, xfx, unique_from).
:- op(1000, xfx, ?=).
:- op(1000, xfx, ?=).

:- [dice].
:- [options].
:- [trait].
:- [bonus].
:- [spells].
:- [class].

%! meta_todo(Source, Todo)
%
%  Predicate for helping find incomplete code.
%  For instance, we have declared a class_option(Class),
%  but there is no correspoinding hd_per_level(Class, HD),
%  then we should generate a meta_todo/2.
meta_todo(_,_) :- false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
learnable_spell(Class, Name) :-
    spell_prop(Name, classes, Classes),
    member(Class, Classes).

options_source(class(sorcerer:_), spell, learnable_spell(sorcerer)).
options_source(class(sorcerer:N), replace_spell, spell_known_at_class_level(sorcerer:Prev)) :-
    between(2, 20, N),
    Prev is N-1.
%options_source(choice())

known_spell_at_class_level(Class:Level, Name) :-
    choice_member(class(Class:Level), spell, Name).
known_spell_at_class_level(Class:Level, Name) :-
    between(2, 20, Level),
    PrevLevel is Level-1,
    known_spell_at_class_level(Class:PrevLevel, Name),
    \+ choice(class(Class:Level), replace_spell, Name).

spell_prop(Name, Prop, Val) :-
    spell(Name, Data),
    Val = Data.Prop.
known_spell_prop(Name, Prop, Val) :-
    known_spell(Name, Data, _, _, _),
    Val = Data.Prop.

learn_spell(Class, Name, SpellData, always, [slot]) :-
    current_class_level(Class:Level),
    known_spell_at_class_level(Class:Level, Name),
    spell(Name, SpellData).
    
known_spell(Source, Name, SpellData, Availability, Resources) :-
    learn_spell(Source, Name, GenericSpellData, Availability, Resources),
    findall(Mod, commutative_spell_modifier(Source, Name, Mod), Mods),
    sequence(Mods, GenericSpellData, SpellData).

commutative_spell_modifier(_,_,_) :- false.
current_class_level(_) :- false.

sequence([], X, X).
sequence([Pred|Preds], X, Z) :-
    call(Pred, X, Y),
    sequence(Preds, Y, Z).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Leveling up.
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
initial_class(Class) :- choice(init, 'initial class', Class).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Shorthands.
todo :-
    forall(todo(T), writeln_quoted_term(T)).

mtodo :-
    forall(meta_todo(S,T), writeln_quoted_term(S->T)).

problems :-
    forall(problem(P), writeln_quoted_term(P)).

writeln_quoted_term(T) :-
    write_term(T, [quoted(true), fullstop(true), nl(true)]).
