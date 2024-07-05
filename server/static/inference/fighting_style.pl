fighting_style(archery).
bonus_source(trait(fighting_style(archery)), to_hit(Weapon) + 2) :-
    weapon(Weapon, _, ranged(_), _, _).

fighting_style(defense).
bonus_source(trait(fighting_style(defense)), ac_formula(armor(_)) + 1).
fighting_style(defense) ?= "While you are wearing armor, you gain a +1 bonus to AC.".

fighting_style(dueling).
attack_variant(Weapon:dueling, melee, ToHit, NewDamage, ["when no other weapon equipped"]) :-
    trait(fighting_style(dueling)),
    attack(Weapon, melee, ToHit, Damage, Notes),
    \+ member(twohanded, Notes),
    add_bonus_to_first_damage_roll(Damage, 2, NewDamage).

fighting_style('great weapon fighting').
bonus_source(trait(fighting_style('great weapon fighting')),
             add_weapon_note(Weapon, "may reroll 1 or 2 on a damage die")) :-
    weapon(Weapon, _, melee, _, Notes),
    intersection([twohanded, versatile], Notes, [_|_]).

fighting_style(protection).

fighting_style('two-weapon fighting').
fighting_style('two-weapon fighting') ?= "When you engage in two-weapon fighting, you can add your ability modifier to the damage of the second attack.".
