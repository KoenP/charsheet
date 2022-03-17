:- multifile attack/5.

%! attack(?Name, ?Range, ?ToHitOrDC, ?DamageFormula, ?Notes)
attack(Cantrip, Range, to_hit(ToHit), [DamageDice], []) :-
    known_spell_to_hit(Origin, Cantrip, ToHit),
    known_spell_data(Origin, Cantrip, Data),
    Data.level = 0,
    Data.range = Range,
    unique_subterm_member(spell_attack_roll(_):DamageDice, Data.effects).
attack(Cantrip, Range, saving_throw(DC, Abi), [DamageDice], Notes) :-
    known_spell_saving_throw(Origin, Cantrip, DC, Abi),
    known_spell_data(Origin, Cantrip, Data),
    Data.level = 0,
    Data.range = Range,
    unique_subterm_member(saving_throw(_):Effect, Data.effects),
    ( (Effect = damage(_,_), Effect = DamageDice, Notes = [])
    ; (Effect = (DamageDice else Alt),
       DamageDice = damage(_,_)),
       Notes = [Str],
       format(string(Str), "on save: ~w", Alt)).
