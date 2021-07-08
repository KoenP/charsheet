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
    max_member(Level, [1|Levels]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ability score increases and feats.

% Table for which levels the PC gets an asi.
ability_score_increase_level(Level) :- member(Level, [4,8,12,16,19]).

% Identify ability score increases among accepted traits.
:- table levelup_asi/2, asi_level/1, asi_level/2.

levelup_asi(Level, Ability+Increment) :-
    asi_level(Level),
    gain_level(Level, Class, _),
    class_level(Level, Class:ClassLevel),
    chosen_trait(class(Class:ClassLevel), asi_or_feat, Ability+Increment).
asi_level(Level) :-
    asi_level(Level, _).
asi_level(Level, Class:ClassLevel) :-
    \+ problem(gain_level, _),
    gain_level(Level, Class, _),
    class_level(Level, Class:ClassLevel),
    ability_score_increase_level(ClassLevel).

% Describe the valid options for an asi or feat.
class_trait_options(Class:Level, asi_or_feat, 1 from [2 from PlusOne, 1 from PlusTwo, 1 from Feats]) :-
    ability_score_increase_level(Level),
    matching_class_level(Class:Level),
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
bad_trait_choice(class(Class:ClassLevel), asi_or_feat, Choice, ability_score_exceeds_maximum(Ability)) :-
    ability(Ability),
    findall(Incr, member(Ability+Incr, Choice), Incrs),
    asi_level(Level, Class:ClassLevel),
    Level1 is Level-1,
    ability_after_levelup_asis(Level1, Ability, Score),
    ability_max(Ability, Max),
    sumlist([Score|Incrs], Sum),
    Sum > Max.
bad_trait_choice(class(_), asi_or_feat, Choice, asis_dont_add_to_two) :-
    \+ member(feat(_), Choice),
    findall(Incr, member(_+Incr, Choice), Incrs),
    \+ sumlist(Incrs, 2).

