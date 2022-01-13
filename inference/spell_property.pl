:- discontiguous
       make_spell_attack/2,
       cantrip_damage_scale/2.

:- op(650, xfx, per).

make_spell_attack('fire bolt', ranged(1)).
cantrip_damage_scale('fire bolt', 1 d 10).
spell_property('fire bolt', damage, fire(1 d 10)).

make_spell_attack('scorching ray', ranged(3)).
spell_property('scorching ray', damage, fire(2 d 6) per ray).

% What do I want to register about fire bolt?
% - It makes a ranged spell attack -> display to hit
% - It initially deals 1d10 fire damage -> display damage
% - It scales by 1d10 damage on the usual cantrip damage scale.
% - It should be retrievable that the damage is of fire type -> apply damage bonuses
% - 
