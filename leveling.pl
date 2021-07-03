todo(unsolved_problems(Problem, Error)) :-
    problem(Problem, Error).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gaining levels and level calculation.
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

% Table for which levels the PC gets an asi.
ability_score_increase_level(Level) :- member(Level, [4,8,12,16,19]).

% Identify ability score increases among accepted traits.
levelup_asi(Level, Ability+Increment) :-
    trait(choose_traits(level(Level), asi_or_feat), Ability+Increment).

% Describe the valid options for an asi or feat.
trait_options(level(Level), asi_or_feat, 1 from [2 from PlusOne, 1 from PlusTwo, 1 from Feats]) :-
    level(CharLevel),
    ability_score_increase_level(Level),
    Level =< CharLevel,
    findall(Ability+1, asi_option(Level, Ability+1), PlusOne),
    findall(Ability+2, asi_option(Level, Ability+2), PlusTwo),
    findall(feat(Feat), feat_option(Feat), Feats).
asi_option(Level, Ability+Increment) :-
    ability(Ability),
    member(Increment, [1,2]),
    Level1 is Level-1,
    ability_after_levelup_asis(Level1, Ability, Score),
    ability_max(Ability, Max),
    Score + Increment =< Max.

% Generate more specific error messages for some bad asi/feat choices.
bad_trait_choice(level(Level), asi_or_feat, Choice, ability_score_exceeds_maximum(Ability)) :-
    ability(Ability),
    findall(Incr, member(Ability+Incr, Choice), Incrs),
    Level1 is Level-1,
    ability_after_levelup_asis(Level1, Ability, Score),
    ability_max(Ability, Max),
    sumlist([Score|Incrs], Sum),
    Sum > Max.
bad_trait_choice(level(_), asi_or_feat, Choice, asis_dont_add_to_two) :-
    findall(Incr, member(_+Incr, Choice), Incrs),
    \+ sumlist(Incrs, 2).

