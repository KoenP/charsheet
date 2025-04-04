% :- table ability/2 as incremental.

update_base_ability(Abi, NewScore) :-
    retract(base_ability(Abi, _)),
    assert(base_ability(Abi, NewScore)).

ability(str).
ability(dex).
ability(con).
ability(int).
ability(wis).
ability(cha).

highest_ability_from(List, Abi, Val) :-
    maplist([A,A-X]>>ability(A,X), List, WithScores),
    sort(2, @>=, WithScores, [Abi-Val|_]).
highest_ability_mod_from(List, Abi, Mod) :-
    highest_ability_from(List, Abi, _),
    ability_mod(Abi, Mod).

ability_max(Ability, Max) :-
    ability(Ability),
    sum_bonuses(max_ability(Ability), Bonus),
    Max is 20 + Bonus.

maxed_ability(Ability) :-
    ability(Ability, Score),
    ability_max(Ability, Max),
    Score >= Max.

ability_plus_n(N, Ability+N) :-
    ability(Ability).

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

%! add_ability_mod_and_profbon(Val, Abi, Sum)
%
%  Convenience predicate that adds the ability modifier for Abi and
%  the proficiency bonus to some value Val, producing Sum.
add_ability_mod_and_profbon(Val, Abi, Sum) :-
    ability_mod(Abi, Mod),
    proficiency_bonus(ProfBon),
    Sum is Val + Mod + ProfBon.

%! initial_ability(?Ability:atomic, ?Score:int)
%
%  Your character's ability Score for the given Ability, at level one,
%  not counting any bonuses apart from racial bonuses.
initial_ability(Ability, Score) :-
    base_ability(Ability, Base),
    total_racial_ability_bonus(Ability, Bonus),
    Score is Base + Bonus.

%! ability_after_feats(?Ability:atomic, ?Score:int)
%
%  Your character's ability Score for the given Ability, after
%  counting bonuses from race, ability score increases, and feats.
ability_after_feats(Ability, Score) :-
    ability_after_levelup_asis(Ability, AfterAsis),
    total_other_ability_bonus(Ability, Bonus),
    ability_max(Ability, Max),
    Score is min(AfterAsis+Bonus, Max).

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

total_racial_ability_bonus(Ability, Total) :-
    ability(Ability), % ground
    findall(Bon, (bonus(Origin, Ability + Bon), origin_category(race(_), Origin)), Bonuses),
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
            (bonus(Origin, Ability+Bon),
             \+ origin_category(race(_), Origin),
             Origin \= choice(_, 'asi or feat')),
            Bonuses),
    sumlist(Bonuses, Total).

%! saving_throw(?Ability:atomic, ?Bonus:int)
saving_throw(Ability, Bonus) :-
    ability_mod(Ability, Mod),
    proficiency_bonus(ProfBon),
    (trait(saving_throw(Ability))
     -> (!, Bonus is Mod + ProfBon)
     ;  (!, Bonus = Mod)
    ).
saving_throw(Ability, Bonus) :-
    % Make this predicate not fail if we don't have an initial class yet.
    ability_mod(Ability, Bonus),
    \+ initial_class(_).

%asi(N, Abi+N) :- ability(Abi).
%asi(N, Asis) :-
%    ground(Asis),
%    !,
%    maplist([Abi+N,N]>>ability(Abi), Asis, Ns),
%    sumlist(Ns, N).
%asi(N, Asis) :-
%    split_nat(N, Ns),
%    maplist([M,Abi+M]>>ability(Abi), Ns, Asis).
%split_nat(0, []) :- !.
%split_nat(N, [M|Ns]) :-
%    between(1, N, M),
%    N1 is N-M,
%    split_nat(N1, Ns).

problem(ability_score_missing(Abi)) :-
    ability(Abi),
    \+ ability(Abi, _).
