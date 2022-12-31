%! skill(?Skill:atomic, ?Score:int)
%
%  Score is the bonus you get on rolls for Skill.
skill(Skill, Score) :-
    skill_ability(Skill, Ability),
    ability_mod(Ability, Mod),
    skill_proficiency_bonus(Skill, Bonus),
    Score is Mod + Bonus.

%! skill(?Skill:atomic)
skill(Skill) :- skill_ability(Skill, _).

%! skill_proficiency_bonus(?Skill:atomic, -Bonus:int)
skill_proficiency_bonus(Skill, Bonus) :-
    proficient_at_skill(Skill),
    level(Level),
    calc_bonus(Level, Bonus1),
    (trait(expertise(skill(Skill))) -> Bonus is 2*Bonus1 ; Bonus = Bonus1).
skill_proficiency_bonus(Skill, 0) :-
    \+ trait(skill(Skill)).

%! skill_ability(?Skill:atomic, ?Ability:atomic)
%
%  The Ability that covers Skill.
skill_ability(athletics         , str).
skill_ability(acrobatics        , dex).
skill_ability('sleight of hand' , dex).
skill_ability(stealth           , dex).
skill_ability(arcana            , int).
skill_ability(history           , int).
skill_ability(investigation     , int).
skill_ability(nature            , int).
skill_ability(religion          , int).
skill_ability('animal handling' , wis).
skill_ability(insight           , wis).
skill_ability(medicine          , wis).
skill_ability(perception        , wis).
skill_ability(survival          , wis).
skill_ability(deception         , cha).
skill_ability(intimidation      , cha).
skill_ability(performance       , cha).
skill_ability(persuasion        , cha).

%! proficient_at_skill(?Skill:atomic)
%
%  Checks whether character is proficient at given Skill.
%  When queried with a variable, each skill will only show up once.
proficient_at_skill(Skill) :-
    skill(Skill),
    proficient_at_skill_(Skill).
proficient_at_skill_(Skill) :-
    trait(skill(Skill)),
    !.

% The program should be able to deal with the same skill being
% selected more than once. Nevertheless, if the character has the same
% skill more than once and at least one of the times it is the result
% of a choice, we flag it as a problem.
problem(selected_same_skill_more_than_once(Skill)) :-
    Origin1 = choice(_,_),
    trait(Origin1, skill(Skill)),
    trait(Origin2, skill(Skill)),
    Origin1 \= Origin2.
