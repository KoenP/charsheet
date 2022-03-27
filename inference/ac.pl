unarmored :- false.

%! ac(?AC:int)
%
%  Return the highest AC for your character.
ac(MaxAC) :-
    findall(AC, ac(_,AC), ACs),
    max_list(ACs, MaxAC).

%! ac(?Origin, ?AC:int)
%
%  Calculate your character's armor class. It's possible that multiple
%  formulas apply, usually due to your character having multiple ways
%  to calculate their body_ac/2. Origin indicates which formula was
%  used for your body_ac/2.
ac(Origin, AC) :-
    body_ac(Origin, BodyAC),
    findall(Bonus, bonus(ac+Bonus), Bonuses),
    sumlist([BodyAC | Bonuses], AC).

% Add shield AC to final AC.
bonus_source(equipped(shield), ac+2).
bonus(equipped(shield+N), ac+Bon) :-
    equipped(shield+N), % can't use bonus_source/2 because N might not
                        % be fully instantiated in the next line.
    Bon is 2 + N.

%! body_ac(?Origin, ?AC:int)
%
%  Your character's armor class without considering bonuses.  Multiple
%  different formulas might be applicable, the one we use is indicated
%  by Origin.
body_ac(Origin, AC) :-
    (unarmored_ac_formula(Origin, Formula) ; armored_ac_formula(Origin, Formula)),
    eval_ac_formula(Formula, AC).

%! unarmored_ac_formula(?Origin, ?Formula)
%
%  Formula is the formula used to calculate your character's unarmored AC.
%  See eval_ac_formula/3 for how a correct formula is constructed.
unarmored_ac_formula(Origin, AltFormula) :-
    bonus(Origin, 'unarmored ac' = AltFormula).
unarmored_ac_formula('standard formula', 10 + dex) :-
    \+ bonus('unarmored ac' = _).

%! armored_ac_formula(?Origin, ?Formula)
%
%  Formula is the formula used to calculate your character's AC while
%  wearing armor.  See eval_ac_formula/2 for how a correct formula is
%  constructed.
armored_ac_formula(heavy_armor(Armor), AC) :-
    equipped(Armor),
    body_armor(Armor, heavy, ac(AC)).
armored_ac_formula(medium_armor(Armor), AC + min(dex,2)) :-
    equipped(Armor),
    body_armor(Armor, medium, ac(AC)).
armored_ac_formula(light_armor(Armor), AC + dex) :-
    equipped(Armor),
    body_armor(Armor, light, ac(AC)).

%! eval_ac_formula(?Formula, ?AC:int)
%
%  Evaluate an AC formula to an integer.
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
