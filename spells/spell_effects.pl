:- discontiguous
       spell_effect/2,
       spell_effect/3,
       spell_damage_rolls/2,
       spell_damage_rolls/3,
       spell_at_will_attack/5,
       spell_makes_spell_attack/1,
       spell_has_dc/2.

%spell_effect(Spell, 0, Rolls) :-
%    spell_effect(Spell, Rolls).
%spell_effect(Spell, Rolls) :-
%    spell_damage_rolls(Spell, Rolls).
%spell_effect(Spell, Upcast, Rolls) :-
%    spell_damage_rolls(Spell, Upcast, Rolls).
spell_damage_rolls(Spell, 0, Rolls) :-
    spell_damage_rolls(Spell, Rolls).

%%%%%
spell_has_dc('acid splash', dex).
spell_damage_rolls('acid splash', [acid(Scale d 6)]) :-
    cantrip_scale(Scale).

%%%%%
%spell_effect(aid, Upcast, 'max hp' + Bonus) :-
%    Bonus is (Upcast+1) * 5.
%spell_effect(aid, Upcast, 'cur hp' + Bonus) :-
%    Bonus is (Upcast+1) * 5.

%%%%%
spell_has_dc('animal friendship', wis).
%spell_effect('animal friendship', charmed).

%%%%%
%spell_effect('armor of agathys', 'temp hp' + 5).
%spell_effect('armor of agathys', "special: damage reflection (5 cold)").

%%%%%
spell_has_dc('arms of hadar', str).
%spell_effect('arms of hadar', 'no reactions').
%spell_effect('arms of hadar', on_save('half damage')).
spell_damage_rolls('arms of hadar', Upcast, [necrotic(N d 6)]) :-
    in_upcast_range('arms of hadar', Upcast), % ground Upcast
    N is 2 + Upcast.

%%%%%
spell_has_dc(bane, cha).
%spell_effect(bane, 'attack roll' - (1 d 4)).
%spell_effect(bane, 'ST' - (1 d 4)).

%%%%%
spell_has_dc(banishment, cha).
%spell_effect(banishment, banished).
%spell_effect(banishment, special).

%%%%%
spell_has_dc('burning hands', dex).
spell_damage_rolls('burning hands', Upcast, [fire(N d 6)]) :-
    N is 3 + Upcast.

%%%%%
spell_effect('mage armor', "AC = 13 + dex mod").

%%%%%
spell_makes_spell_attack('fire bolt').
spell_damage_rolls('fire bolt', [fire(Scale d 10)]) :-
    cantrip_scale(Scale).

%%%%%
spell_makes_spell_attack('scorching ray').
spell_damage_rolls('scorching ray', Upcast, Dice) :-
    in_upcast_range('scorching ray', Upcast), % ground Upcast
    N is 3 + Upcast,
    repl(fire(2 d 6), N, Dice).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

attack(shillelagh:Weapon, melee, ToHit, [bludgeoning(1 d 8 + DamageBonus)],
       [magical, when_spell_active(shillelagh)]) :-
    spell_known(shillelagh, _, Ability, always_available, at_will),
    have(Weapon),
    plus_zero(Weapon, BaseWeapon+Enchantment),
    member(BaseWeapon, [quarterstaff, club]),
    ability_mod(Ability, AbiMod),
    weapon_proficiency_bonus(BaseWeapon, ProfBon),
    ToHit is Enchantment + ProfBon + AbiMod,
    DamageBonus is Enchantment + AbiMod.


in_upcast_range(Spell, Upcast) :-
    spell(Spell, level, Level),
    UpperBound is 9 - Level,
    between(0, UpperBound, Upcast).

%spell_has_dc('hold person').

%
%% Cantrips.
%spell('fire bolt',
%      properties(
%          school: evocation,
%          level: 0,
%          casting_time: action,
%          range: 120,
%          components: [v, s],
%          duration: instantaneous)).
%spell_at_will_attack('fire bolt', 120, fire, Scale d 10, []) :-
%    cantrip_scale(Scale).
%    
%
%spell('druidcraft',
%      properties(
%          school:transmutation,
%          level: 0,
%          casting_time: action,
%          range: 30,
%          components: [v, s],
%          duration: instantaneous,
%          info: "Whispering to the spirits of nature, you create one of the following effects within range: * You create a tiny, harmless sensory effect that predicts what the weather will be at your location for the next 24 hours. The effect might manifest as a golden orb for clear skies, a cloud for rain, falling snowflakes for snow, and so on. This effect persists for 1 round. * You instantly make a flower blossom, a seed pod open, or a leaf bud bloom. * You create an instantaneous, harmless sensory effect, such as falling leaves, a puff of wind, the sound of a small animal, or the faint odor of skunk. The effect must fit in a 5-foot cube. * You instantly light or snuff out a candle, a torch, or a small campfire."
%      )).
%
%spell('shillelagh',
%      properties(
%          school: transmutation,
%          level: 0,
%          casting_time: bonus,
%          range: touch,
%          components: [v, s, m("mistletoe, a shamrock leaf, and a club or quarterstaff")],
%          duration: minutes(1),
%          info: "The wood of a club or quarterstaff you are holding is imbued with nature's power. For the duration, you can use your spellcasting ability instead of Strength for the attack and damage rolls of melee attacks using that weapon, and the weapon's damage die becomes a d8. The weapon also becomes magical, if it isn't already. The spell ends if you cast it again or if you let go of the weapon."
%      )).
%    
%spell('guidance',
%      properties(
%          school: divination,
%          level: 0,
%          casting_time: action,
%          range: touch,
%          components: [v, s],
%          duration: concentration(minutes(1)))).
%spell('guidance') ?= "You touch one willing creature. Once before the spell ends, the target can roll a d4 and add the number rolled to one ability check of its choice. It can roll the die before or after making the ability check. The spell then ends.".
%
%% Level 1
%spell('healing word',
%      properties(
%          school: evocation,
%          level: 1,
%          casting_time: bonus,
%          range: 60,
%          components: [v],
%          duration: instantaneous)).
%
%% Level 2
%spell('moonbeam',
%      properties(
%          school: evocation,
%          level: 2,
%          casting_time: action,
%          range: 120,
%          components: [v,s,m("several seeds of any moonseed plant and a piece of opalescent feldspar")],
%          duration: concentration(minutes(1)))).
%
%spell('hold person',
%      properties(
%          school: enchantment,
%          level: 2,
%          casting_time: action,
%          range: 60,
%          components: [v,s,m("a small, straight piece of iron")],
%          duration: concentration(minutes(1)),
%          info: "Choose a humanoid that you can see within range. The target must succeed on a Wisdom saving throw or be paralyzed for the duration. At the end of each of its turns, the target can make another Wisdom saving throw. On a success, the spell ends on the target.
%
%At Higher Levels. When you cast this spell using a spell slot of 3rd level or higher, you can target one additional humanoid for each slot level above 2nd. The humanoids must be within 30 feet of each other when you target them.")).
%spell_has_dc('hold person').
