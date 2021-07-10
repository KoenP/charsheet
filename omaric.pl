:- discontiguous
       race/1,
       have/1,
       gain_level/3,
       choose_subclass/2,
       pick_feat/2,
       pick_abi/2.

:- [charsheet].

choose_traits(_,_,_) :- false.
equipped(_) :- false.
have(_) :- false.

have(scimitar).
have(quarterstaff).
have(quarterstaff+2).
have(battleaxe).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 1
name('Omaric').
initial_class(druid).
race('high elf').

base_ability(str, 8).
base_ability(dex, 14).
base_ability(con, 17).
base_ability(int, 10).
base_ability(wis, 18).
base_ability(cha, 8).

choose_traits(race('high elf'), cantrip, [learn_spell('high elf', 'fire bolt')]).
choose_traits(class(druid:1), cantrip, [learn_spell(druid, shillelagh), learn_spell(druid,guidance)]).
choose_traits(class(druid:1), skills, [skill('animal handling'), skill(nature)]).

gain_level(2, druid, hp_avg).
choose_subclass(druid, land).
choose_traits(subclass(druid:2,land), extra_cantrip, [learn_spell(druid, 'druidcraft')]).

gain_level(3, druid, hp_avg).
choose_traits(subclass(druid:3,land), land_type, [druid_land_type(arctic)]).
choose_traits(subclass(druid:3,land), circle_spell, [learn_circle_spell('hold person')]).

gain_level(4, druid, hp_avg).
gain_level(5, druid, hp_avg).
gain_level(6, wizard, hp_avg).
gain_level(7, wizard, hp_avg).
gain_level(8, wizard, hp_avg).
gain_level(9, wizard, hp_avg).

spell_known('hold person', wizard, int, when_prepared, spell_slot).
% TODO bug: suppresses circle spell!
