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

proficient(Skill) :- trait(skill(Skill)).

skill_proficiency_bonus(Skill, Bonus) :-
    proficient(Skill), !,
    level(Level),
    calc_bonus(Level, Bonus).
skill_proficiency_bonus(Skill, 0) :- \+ proficient(Skill).

skill(Skill, Score) :-
    skill_ability(Skill, Abil),
    ability_mod(Abil, Mod),
    skill_proficiency_bonus(Skill, Bonus),
    Score is Mod + Bonus.
