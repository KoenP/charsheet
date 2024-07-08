add_bonus_to_first_damage_roll([damage(Type,Formula   )|Rest], Bonus,
                               [damage(Type,NewFormula)|Rest]) :-
    simplify_dice_sum(Formula + Bonus, NewFormula).
