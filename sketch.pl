:- multifile
       gain_level/3,
       todo/1,
       problem/1,
       options/3,
       options_source/3,
       choice/3,
       trait/2,
       trait_source/2,
       traits_from_source/2,
       bonus/2,
       bonus_source/2,
       bonuses_from_source/2.

:- op(650, xfx, from).
:- op(650, xfx, unique_from).
:- op(1000, xfx, ?=).
:- op(1000, xfx, ?=).

:- [spells].
:- [class].

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
options_source(race('high elf'), cantrip, learnable_cantrip(wizard)).
options_source(race('high elf'), language, language).
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
% Traits and bonuses.
trait(Trait) :- trait(_, Trait).
trait(Source, Trait) :-
    trait_source(Source, Trait),
    call(Source).
trait_source(Source, Trait) :-
    traits_from_source(Source, Traits),
    member(Trait, Traits).

bonus(Source, Bonus) :-
    bonus_source(Source, Bonus),
    call(Source).
bonus_source(Source, Bonus) :-
    bonuses_from_source(Source, Bonuses),
    member(Bonus, Bonuses).
bonuses_from_source(_,_) :- false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Options and choices.
options(Source, Id, Spec) :-
    options_source(Source, Id, Spec),
    call(Source).
choice_member(Origin, Id, Choice) :-
    choice(Origin, Id, C),
    (member(Choice, C); \+ is_list(C), Choice = C).
valid_choice(Origin, Id, Choice) :-
    options(Origin, Id, Spec),
    call(Spec, Choice).
todo(options(Origin, Id)) :-
    options(Origin, Id, _),
    \+ choice(Origin, Id, _).
problem(invalid_choice(Origin, Id, Choice)) :-
    choice(Origin, Id, Choice),
    \+ valid_choice(Origin, Id, Choice).
problem(not_eligible(Origin, Id, Choice)) :-
    choice(Origin, Id, Choice),
    \+ options(Origin, Id, _).

% Helper predicates
from(N, Pred, Choices) :-
    length(Choices, N),
    maplist(call(Pred), Choices).
unique_from(N, Pred, Choices) :-
    from(N, Pred, Choices),
    is_set(Choices).
         
from_list(L, X) :- member(X, L).

inspect_options(Origin, Id, List) :-
    options(Origin, Id, Spec), % ground
    inspect_spec(Spec, List).
inspect_spec(N from Pred, N from List) :-
    inspect_spec(Pred, List).
inspect_spec(N unique_from Pred, N unique_from List) :-
    inspect_spec(Pred, List).
inspect_spec(Pred, List) :-
    \+ member(Pred, [_ from _, _ unique_from _]),
    findall(X, call(Pred, X), List).

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

% Pick your base class.
options(init, 'base class', class_option).
base_class(Class) :- choice(init, 'base class', Class).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Shorthands.
todo :-
    forall(todo(T), writeln_quoted_term(T)).

problems :-
    forall(problem(P), writeln_quoted_term(P)).

writeln_quoted_term(T) :-
    write_term(T, [quoted(true), fullstop(true), nl(true)]).
