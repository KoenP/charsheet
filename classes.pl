:- multifile
    class_option/1,
    hd_per_level/2,
    max_hp_initial/2,
    max_hp_per_level/2,
    saving_throw/2,
    gain_spell_slots/3.

:- [classes/druid].

class_cantrips(druid, Cantrips) :- 
    findall(Cantrip, (spell(Cantrip, level, 0), spell(Cantrip, class, druid)), Cantrips).
