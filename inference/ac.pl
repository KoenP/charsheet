:- multifile suppress_unarmored_ac_formula/0.

ac(AC) :- ac(_, AC, _).

ac(Origin, AC, Options) :-
    ac_formula(Origin, Formula),
    sumall(B, bonus(ac + B), GlobalBonus),
    sumall(B, bonus(ac_formula(Origin) + B), SpecificBonus),
    eval_ac_formula(Formula, FormulaAC, Options),
    AC is FormulaAC + GlobalBonus + SpecificBonus.

ac_formula(armor(Armor), AC + Enchantment + shield) :-
    has(Armor),
    expand_to_sum(Armor, BaseArmor + Enchantment),
    body_armor(BaseArmor, heavy, ac(AC)).
ac_formula(armor(Armor), AC + Enchantment + min(dex,2) + shield) :-
    has(Armor),
    expand_to_sum(Armor, BaseArmor + Enchantment),
    body_armor(BaseArmor, medium, ac(AC)).
ac_formula(armor(Armor), AC + Enchantment + dex + shield) :-
    has(Armor),
    expand_to_sum(Armor, BaseArmor + Enchantment),
    body_armor(BaseArmor, light, ac(AC)).
ac_formula(Origin, Formula) :-
    bonus(Origin, ac_formula(Formula)).
ac_formula(unarmored, 10 + dex + shield) :-
    \+ suppress_unarmored_ac_formula.

%! eval_ac_formula(?Formula, ?AC:int, ?Options)
%
%  Evaluate an AC formula to an integer, and a list of options of the form `Id:Modifier`.
eval_ac_formula(A + B, AC, Options) :-
    eval_ac_formula(A, X, Opts1),
    eval_ac_formula(B, Y, Opts2),
    AC is X + Y,
    append(Opts1, Opts2, Options).
eval_ac_formula(Abi, AC, []) :-
    ability_mod(Abi, AC).
eval_ac_formula(min(F1,F2), AC, Options) :-
    eval_ac_formula(F1, AC1, Opts1),
    eval_ac_formula(F2, AC2, Opts2),
    (AC1 =< AC2 -> (AC = AC1, Options = Opts1) ; (AC = AC2, Options = Opts2)).
eval_ac_formula(Num, Num, []) :-
    number(Num).
eval_ac_formula(shield, 0, [shield(Shield):AC]) :-
    trait(armor(shield)),
    % Only the first shield is used to prevent explosion of options.
    once(shield_ac(Shield, AC)).
eval_ac_formula(shield, 0, []) :-
    \+ (trait(armor(shield)), shield_ac(_, _)).

shield_ac(Shield, AC) :-
    has(Shield),
    is_shield(Shield),
    calculate_shield_ac(Shield, AC).

calculate_shield_ac(Shield ~ Variant, AC) :-
    ground(Shield ~ Variant),
    calculate_shield_ac(Shield, AC).
calculate_shield_ac(shield + N, AC) :-
    AC is 2 + N.
calculate_shield_ac(shield, 2).

%! unarmored_defense_formula(?Origin, ?Formula)
%
%  The 'unarmored defense' trait has a specific multiclassing rule:
%  you can only gain this feature once. So if you're multiclass monk/barbarian,
%  you get the unarmored defense formula of whichever class you picked first.
unarmored_defense_formula(Origin, Formula) :-
    findall(OL-O-F,
            (trait(O,unarmored_defense(F)),
             origin_level(O,OL)),
            Olofs),
    sort(1, @<, Olofs, [_ - (Origin>:_) - Formula | _]).
bonus(unarmored_defense(Origin), ac_formula(Formula)) :-
    unarmored_defense_formula(Origin, Formula).

suppress_unarmored_ac_formula :- trait(unarmored_defense(_)).
