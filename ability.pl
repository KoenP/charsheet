ability(str).
ability(dex).
ability(con).
ability(wis).
ability(int).
ability(cha).

ability_max(Ability, Max) :-
    ability(Ability),
    sum_bonuses(max_ability(Ability), Bonus),
    Max is 20 + Bonus.
    

%:- table
%   naked_lvl1_ability/2,
%   ability_after_levelup_asis/3,
%   ability_after_feats/2.

%! ability(?Ability:atomic, ?Score:int)
%
%  Your character's ability Score for the given Ability.
ability(Ability, Score) :-
    ability_after_feats(Ability, Score).

%! ability_mod(?Ability:atomic, ?Mod:int)
%
%  Your character's ability modifier (Mod) for the given Ability.
ability_mod(Abil, Mod) :-
    ability(Abil, Val),
    mf(Val, Mod).
mf(Val, Mod) :-
    floor( (Val-10) / 2, Mod ).

%! initial_ability(?Ability:atomic, ?Score:int)
%
%  Your character's ability Score for the given Ability, at level one,
%  not counting any bonuses apart from racial bonuses.
initial_ability(Ability, Score) :-
    base_ability(Ability, Base),
    total_racial_ability_bonus(Ability, Bonus),
    Score is Base + Bonus.

%! ability_after_levelup_asis(?Ability:atomic, ?Score:int)
%
%  Your character's ability Score for the given Ability, after
%  counting racial bonuses and ability score increases, but not
%  counting any other bonuses.
ability_after_levelup_asis(Ability, Score) :-
    initial_ability(Ability, Initial),
    total_asi_ability_bonus(Ability, Bonus),
    Score is Initial + Bonus.
problem(ability_score_exceeds_maximum(Ability, Score)) :-
    % Flag a problem if the asis make you go over the max.
    % TODO: hide asi options that go over the maximum
    ability_after_levelup_asis(Ability, Score),
    ability_max(Ability, Max),
    Score > Max.

%! ability_after_feats(?Ability:atomic, ?Score:int)
%
%  Your character's ability Score for the given Ability, after
%  counting bonuses from race, ability score increases, and feats.
ability_after_feats(Ability, Score) :-
    ability_after_levelup_asis(Ability, AfterAsis),
    total_other_ability_bonus(Ability, Bonus),
    ability_max(Ability, Max),
    Score is min(AfterAsis+Bonus, Max).

total_racial_ability_bonus(Ability, Total) :-
    ability(Ability), % ground
    findall(Bon, bonus(race(_), Ability + Bon), Bonuses),
    sumlist(Bonuses, Total).

total_asi_ability_bonus(Ability, Total) :-
    ability(Ability),
    findall(Bon,
            (bonus(Origin, Ability+Bon), Origin = choice(_, 'asi or feat')),
              % Omitting the intermediate Origin variable causes a warning in swipl which,
              % I think, is a bug in swipl.
            Bonuses),
    sumlist(Bonuses, Total).

total_other_ability_bonus(Ability, Total) :-
    ability(Ability),
    findall(Bon,
            (bonus(Origin, Ability+Bon), Origin \= race(_), Origin \= choice(_, 'asi or feat')),
            Bonuses),
    sumlist(Bonuses, Total).
