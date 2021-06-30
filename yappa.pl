:- discontiguous
       gain_level/3,
       pick_trait/3,
       pick_feat/2,
       pick_abi/2.

:- [charsheet].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 1
initial_class(druid).

race(tortle).

base_ability(str, 20).
base_ability(dex, 8).
base_ability(con, 17).
base_ability(int, 16).
base_ability(wis, 18).
base_ability(cha, 8).

pick_trait(_, _, _) :- false.
pick_feat(_,_) :- false.
pick_abi(_,_) :- false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 2
gain_level(2, druid, hp_avg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 3
gain_level(3, druid, hp_avg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 4
gain_level(4, druid, hp_avg).
% pick_trait(level(4), abi_or_feat, [abi(wis,1), abi(dex,1)]).
%pick_trait(level(4), abi_or_feat, [abi(int,1), abi(dex,1)]).
pick_feat(4, alert).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 5
gain_level(5, druid, hp_avg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 6
gain_level(6, druid, hp_avg).

pick(_,_,_) :- false.
pick(class(druid), druid_skills, [proficient(animal_handling), proficient(medicine)]).


equipped(_) :- false.

