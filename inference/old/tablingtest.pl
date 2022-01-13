:- module(tablingtest, [spell_list/1, level/1, t/2, tal/3, gain/3, opts/3, choice/3]).


spell_list([fwoar, kaboom, tinkle, zapzap, zoop]).

% trait
level(4).

t(O,T) :-
    level(L),
    tal(L, O, T).

% trait at level
:- table tal/3.
tal(L, O, T) :-
    gain(L, O, T).
tal(L, O, T) :-
    level(Max),
    between(2, Max, L),
    L1 is L-1,
    tal(L1, O, T).

% gains
:- table gain/3.
gain(Lvl, choice(O), T) :-
    opts(Lvl, O, Choices),
    choice(Lvl, O, T),
    member(T, Choices).
%gain(1, init, a).
%gain(2, init, b).
%gain(3, init, c).

% options
:- table opts/3.
opts(1, wiz, Spells) :-
    spell_list(Spells).
opts(Lvl, wiz, Spells) :-
    level(Max),
    between(2, Max, Lvl),
    spell_list(Candidates),
    Lvl1 is Lvl-1,
    findall(Spell, tal(Lvl1, choice(wiz), Spell), ToRemove),
    subtract(Candidates, ToRemove, Spells).

opts(2, abi, [str,dex]) :-
    t(_, kaboom).

% choices
choice(1, wiz, fwoar).
choice(2, wiz, kaboom).
choice(2, abi, str).


% tal -> gain -> opts -> findall(tal)
