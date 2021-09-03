:- discontiguous
       race/1,
       gain_level/3,
       choose_subclass/2,
       pick_feat/2,
       pick_abi/2.

:- [charsheet].

choose_traits(_,_,_) :- false.

have(quarterstaff).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 1
name('Yappa').

initial_class(druid).
background(hermit).

race(tortle).

base_ability(str, 8).
base_ability(dex, 8).
base_ability(con, 17).
base_ability(int, 16).
base_ability(wis, 17).
base_ability(cha, 8).

choose_subclass(_,_) :- false.
choose_feat(_,_) :- false.
increase_ability_score(_,_) :- false.
equipped(_) :- false.

choose_traits(class(druid:1), cantrip, [shillelagh, guidance]).
choose_traits(class(druid:1), skill, ['animal handling', nature]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 2
gain_level(2, druid, hp_avg).
choose_subclass(druid, moon).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 3
gain_level(3, druid, hp_avg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 4
gain_level(4, druid, hp_avg).
choose_trait(class(druid:4), asi_or_feat, feat(alert)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 5
gain_level(5, druid, hp_avg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 6
gain_level(6, druid, hp_avg).

highlight_spell('absorb elements').
highlight_spell('cure wounds').
highlight_spell('healing word').
highlight_spell('speak with animals').
highlight_spell(earthbind).
highlight_spell('locate object').
highlight_spell(moonbeam).
highlight_spell('call lightning').
highlight_spell('conjure animals').
highlight_spell('dispel magic').
