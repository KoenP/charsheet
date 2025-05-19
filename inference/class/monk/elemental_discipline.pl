:- discontiguous elemental_discipline_option/1, elemental_discipline_spell/3.

known_spell(monk('elemental discipline'(Discipline)), wis, always, [KiStr], no, Spell) :-
    trait('elemental discipline'(Discipline)),
    elemental_discipline_spell(Discipline, Spell, Ki),
    format(string(KiStr), "~w ki points", Ki).

elemental_discipline_option('fist of four thunders').
elemental_discipline_spell('fist of four thunders', thunderwave, 2).

elemental_discipline_option('mist stance') :- monk('four elements') >: 11.
elemental_discipline_spell('mist stance', 'gaseous form', 4).
bonus_source(trait('elemental discipline'('mist stance')),
             modify_spell(monk('elemental discipline'('mist stance')),
                          'gaseous form',
                          modify_spell_field(range, [_,self]>>true))).
custom_format(modify_spell_field(range, [_,self]>>true)) -->
    ["Can only target self."].


% modify_spell_field(effects, [Es1,Es2]>>append(Es1,NewEffects,Es2), Old, New).
