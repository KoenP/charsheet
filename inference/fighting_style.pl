fighting_style(archery) ?= "You gain a +2 bonus to attack rolls you make with ranged weapons.".
bonus_source(trait(fighting_style(archery)), to_hit(Weapon) + 2) :-
    weapon_ranged(Weapon).

fighting_style(defense) ?= "While you are wearing armor, you gain a +1 bonus to AC.".
bonus_source(trait(fighting_style(defense)), ac + 1) :-
    wearing_armor.

fighting_style(dueling) ?= "When you are wielding a melee weapon in one hand and no other weapons, you gain a +2 bonus to damage rolls with that weapon.".

bonus_source(trait(fighting_style(dueling)),
             add_weapon_note(OneHandedMelee,
                             "+2 damage if no other weapon equipped")) :-
    weapon_onehanded(OneHandedMelee),
    weapon_melee(OneHandedMelee).

fighting_style('two-weapon fighting') ?= "When you engage in two-weapon fighting, you can add your ability modifier to the damage of the second attack.".
