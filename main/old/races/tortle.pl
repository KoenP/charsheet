base_race_option(tortle).
race_size(tortle, medium).
race_base_speed(tortle, 30).

racial_trait(tortle, str+2).
racial_trait(tortle, wis+1).
racial_trait(tortle, claws).
racial_trait(tortle, 'hold breath').
racial_trait(tortle, natural_armor(17)).
racial_trait(tortle, 'shell defense').
racial_trait(tortle, skill(survival)).
racial_trait(tortle, language(common)).
racial_trait(tortle, language(aquan)).

% TODO what about monks?
attack(claws, melee, Mod, [slashing(1 d 4 + StrMod)], []) :-
    trait(race(tortle), claws),
    unarmed_attack_modifier(Mod),
    ability_mod(str, StrMod).

claws ?= "Your claws are natural weapons, which you can use to make unarmed strikes. If you hit with them, you deal slashing damage equal to 1d4 + your Strength modifier, instead of the bludgeoning damage normal for an unarmed strike.".
'hold breath' ?= "You can hold your breath for up to 1 hour at a time. Tortles aren't natural swimmers, but they can remain underwater for some time before needing to come up for air.".
'shell defense' ?= "You can withdraw into your shell as an action. Until you emerge, you gain a +4 bonus to AC, and you have advantage on Strength and Constitution saving throws. While in your shell, you are prone, your speed is 0 and can't increase, you have disadvantage on Dexterity saving throws, you can't take reactions, and the only action you can take is a bonus action to emerge from your shell.".
