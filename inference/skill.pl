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
    trait(skill(Skill)),
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
