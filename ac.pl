unarmored :- false.
equipped(_) :- false.
body_armor(_,_,_) :- false.

%! ac(?AC:int)
%
%  Shorthand for ac/2.
ac(AC) :- ac(_, AC).

%! ac(?Origin, ?AC:int)
%
%  Calculate your character's armor class. It's possible that multiple
%  formulas apply, usually due to your character having multiple ways
%  to calculate their body_ac/2. Origin indicates which formula was
%  used for your body_ac/2.
ac(Origin, AC) :-
    body_ac(Origin, BodyAC),
    findall(Bonus, bonus(ac+Bonus), Bonuses),
    sumlist([BodyAC, ShieldAC | Bonuses], AC).

bonus_source(equipped(shield), ac+2).
bonus_source(equipped(shield+N), ac+Bon) :-
    Bon is 2 + N.

body_ac(Origin, AC) :-
    (unarmored_ac_formula(Origin, Formula) ; armored_ac_formula(Origin, Formula)),
    eval_ac_formula(Formula, AC).

unarmored_ac_formula(Origin, AltFormula) :-
    bonus(Origin, 'unarmored ac' = AltFormula).
unarmored_ac_formula('standard formula', 10 + dex).

armored_ac_formula(heavy_armor(Armor), AC) :-
    equipped(Armor),
    body_armor(Armor, heavy, ac(AC)).
armored_ac_formula(medium_armor(Armor), AC + min(dex,2)) :-
    equipped(Armor),
    body_armor(Armor, medium, ac(AC)).
armored_ac_formula(light_armor(Armor), AC + dex) :-
    equipped(Armor),
    body_armor(Armor, light, ac(AC)).

shield_ac_formula(shield, 2) :-
    equipped(shield).
shield_ac_formula(shield+N, 2+N) :-
    equipped(shield+N).
 
eval_ac_formula(Id, Formula, AC) :-
    findall(Bon, bonus(Id + Bon), Bonuses),
    eval_ac_formula(Formula, ACWithoutBonuses),
    sumlist([ACWithoutBonuses|Bonuses], AC).
eval_ac_formula(A + B, AC) :-
    eval_ac_formula(A, X),
    eval_ac_formula(B, Y),
    AC is X + Y.
eval_ac_formula(min(A,B), AC) :-
    eval_ac_formula(A, X),
    eval_ac_formula(B, Y),
    AC is min(X,Y).
eval_ac_formula(Abi, AC) :-
    ability_mod(Abi, AC).
eval_ac_formula(Num, Num) :-
    number(Num).
