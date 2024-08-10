:- discontiguous fighting_style/1.

% Fighting style can originate from multiple classes, so make sure to not offer the same
% style more than once.
hide_base_option(Source1, 'fighting style', FightingStyle) :-
    choice(Source2, 'fighting style', FightingStyle),
    Source2 \= Source1.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fighting_style('two-weapon fighting') ?= "When you engage in two-weapon fighting, you can add your ability modifier to the damage of the second attack.".
fighting_style(archery) ?= "You gain a +2 bonus to attack rolls you make with ranged weapons.".
fighting_style(dueling) ?= "When you are wielding a melee weapon in one hand and no other weapons, you gain a +2 bonus to damage rolls with that weapon.".
fighting_style('great weapon fighting') ?= "When you roll a 1 or 2 on a damage die for an attack you make with a melee weapon that you are wielding with two hands, you can reroll the die and must use the new roll, even if the new roll is a 1 or a 2. The weapon must have the two-handed or versatile property for you to gain this benefit.".
fighting_style(protection) ?= "When a creature you can see attacks a target other than you that is within 5 feet of you, you can use your reaction to impose disadvantage on the attack roll. You must be wielding a shield.".
fighting_style('two-weapon fighting') ?= "When you engage in two-weapon fighting, you can add your ability modifier.".
