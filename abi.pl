
ability_score_increase_level(Level) :- member(Level, [4,8,12,16,19]).

valid_increase_ability_score(PickLevel, Abil, Val) :-
    level(CharLevel),
    increase_ability_score(PickLevel, Abil, Val),
    between(1, CharLevel, PickLevel),
    \+ problem(increase_ability_score(PickLevel, Abil, Val), _).

% Detect unspent ability score increases.
todo(ability_score_increase_unspent(level(Level))) :-
    ability_score_increase_level(Level),
    level(CharLevel),
    Level =< CharLevel,
    \+ (increase_ability_score(Level, _, _) ; pick_feat(Level, _)).

% Detect if we tried to add an abi on an inappropriate level.
problem(increase_ability_score(Level, Abil, Val), no_abi_level(Level)) :-
    increase_ability_score(Level, Abil, Val),
    \+ ability_score_increase_level(Level).

% Detect if we tried to add a feat on an inappropriate level.
problem(pick_feat(Level, Feat), no_abi_level(Level)) :-
    pick_feat(Level, Feat),
    \+ ability_score_increase_level(Level).

% Detect if ability score increases don't sum up to 2.
problem(increase_ability_score(Level, Abil, Val), not_equal_to_2) :-
    increase_ability_score(Level, Abil, Val),
    findall(Val, increase_ability_score(Level, _, Val), Vals),
    sumlist(Vals, SumVals),
    SumVals \= 2.

% Detect if ability score increases have been picked that the PC is
% not eligible for.
problem(increase_ability_score(Level, Abil, Val), level_too_low) :-
    increase_ability_score(Level, Abil, Val),
    level(CharLevel),
    CharLevel < Level.

% Detect if the player selected both a feat and an ability score increase.
problem(increase_ability_score(Level, Abil, Val), picked_both_feat_and_abi) :-
    problem(picked_both_feat_and_abi(Level,Abil,Val,_), picked_both_feat_and_abi).
problem(pick_feat(Level,Feat), picked_both_feat_and_abi) :-
    problem(picked_both_feat_and_abi(Level,_,_,Feat), picked_both_feat_and_abi).
problem(picked_both_feat_and_abi(Level,Abil,Val,Feat), picked_both_feat_and_abi) :-
    increase_ability_score(Level, Abil, Val),
    pick_feat(Level, Feat).

% TODO: Detect if ability score increase pushes the ability over 20.
% Should figure out what kind of things can affect ability scores though.
