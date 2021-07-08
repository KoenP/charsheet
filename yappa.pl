:- discontiguous
       race/1,
       gain_level/3,
       choose_subclass/2,
       pick_feat/2,
       pick_abi/2.

:- [charsheet].

choose_traits(_,_,_) :- false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 1
name('Yappa').

initial_class(druid).

race(tortle).

base_ability(str, 10).
base_ability(dex, 8).
base_ability(con, 17).
base_ability(int, 16).
base_ability(wis, 18).
base_ability(cha, 8).

%pick_trait(class(druid:1), druid_cantrips, shillelagh).
%pick_trait(class(druid:1), druid_cantrips, guidance).
% TODO hier zat ik

% pick_trait(_, _, _) :- false.
choose_subclass(_,_) :- false.
choose_feat(_,_) :- false.
increase_ability_score(_,_) :- false.
equipped(_) :- false.

choose_traits(class(druid:1), cantrip, [learn_spell(druid, shillelagh), learn_spell(druid, guidance)]).
choose_traits(class(druid:1), skills, [skill(perception), skill(nature)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 2
gain_level(2, druid, hp_avg).
choose_subclass(druid, land).
%choose_traits(subclass(druid:2,land), extra_cantrip, )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 3
gain_level(3, druid, hp_avg).
choose_traits(subclass(druid:3,land), land_type, [druid_land_type(arctic)]).
choose_traits(subclass(druid:3,land), circle_spell, [learn_circle_spell('hold person')]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 4
gain_level(4, druid, hp_avg).
choose_traits(class(druid:4), asi_or_feat, [feat(alert)]).
%pick_feat(4, alert).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 5
gain_level(5, druid, hp_avg).
%choose_traits(subclass(druid:5,land), circle_spell, [spell(slow)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 6
gain_level(6, druid, hp_avg).



gain_level(7, wizard, hp_avg).
%gain_level(8, wizard, hp_avg).
%gain_level(9, druid, hp_avg).
%gain_level(10, druid, hp_avg).
%choose_traits(class(druid:8), asi_or_feat, [str+2]).
