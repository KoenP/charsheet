todo(unsolved_problems(Problem, Error)) :-
    problem(Problem, Error).

problem(gain_level, not_contiguous(Levels)) :-
    findall(L, gain_level(L,_,_), Levels),
    max_member(Highest, Levels),
    findall(L, between(2,Highest,L), Levels2),
    Levels \= Levels2.

level(Level) :-
    findall(L, gain_level(L,_,_), Levels),
    max_member(Level, Levels).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ability score increases and feats.
levelup_abi(Level, Ability+Increment) :-
    trait(pick_trait(level(Level), abi_or_feat), Abis),
    member(Ability+Increment, Abis).

pick_trait(level(Level), abi_or_feat, [feat(Feat)]) :-
    pick_feat(Level, Feat).

pick_trait(level(Level), abi_or_feat, Abis) :-
    ability_score_increase_level(Level),
    findall(Abi, pick_abi(Level, Abi), Abis),
    Abis = [_|_].

% Table for which levels the PC gets an abi.
ability_score_increase_level(Level) :- member(Level, [4,8,12,16,19]).

% Describe the valid options for an abi or feat.
trait_options(level(Level), abi_or_feat, 1, Options) :-
    abi_or_feat_options(Level, Options),
    \+ (member(Abi, Options), bad_abi(Level, Abi)).

% Describe bad options for which we have specific error messages:
% - ability points don't add up to 2,
% - ability increase brings score above 20 (or whatever the maximum is).
trait_bad_options(_, abi_or_feat, Options, should_add_two_ability_points) :-
    Options \= [feat(_)],
    findall(Val, member(_+Val, Options), Vals),
    \+ sumlist(Vals, 2).

trait_bad_options(level(Level), abi_or_feat, Options, abi_exceeds_max_ability_score) :-
    abi_or_feat_options(Level, Options),
    member(Abi, Options),
    bad_abi(Level, Abi).

abi_or_feat_options(Level, Options) :-
    level(CharLevel),
    ability_score_increase_level(Level),
    Level =< CharLevel,
    abi_or_feat_options(Options).
abi_or_feat_options([Ability+2]) :-
    ability(Ability).
abi_or_feat_options([feat(Feat)]) :-
    feat_option(Feat).
abi_or_feat_options([Ability1+1, Ability2+1]) :-
    ability(Ability1),
    ability(Ability2),
    Ability1 \= Ability2.

% True iff this abi brings the character above the maximum ability score threshold.
bad_abi(Level, Ability+Inc) :-
    Level1 is Level - 1,
    ability_after_levelup_abis(Level1, Ability, Score),
    ability_max(Ability, Max),
    Score + Inc > Max.
