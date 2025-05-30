:- discontiguous elemental_discipline_option/1,
                 elemental_discipline_spell/3.

elemental_discipline_option('breath of winter') :- monk('four elements') >: 17.
elemental_discipline_spell('breath of winter', 'cone of cold', 6).

elemental_discipline_option('clench of the north wind') :- monk('clench of the north wind') >: 6.
elemental_discipline_spell('clench of the north wind', 'hold person', 3).

elemental_discipline_option('elemental attunement').

elemental_discipline_option('fist of four thunders').
elemental_discipline_spell('fist of four thunders', thunderwave, 2).

elemental_discipline_option('mist stance') :- monk('four elements') >: 11.
elemental_discipline_spell('mist stance', 'gaseous form', 4).
bonus_source(trait('elemental discipline'('mist stance')),
             modify_spell(monk('elemental discipline'('mist stance')),
                          'gaseous form',
                          modify_spell_field(range, [_, self] >> true))).

custom_format(modify_spell_field(range, [_,self]>>true)) -->
    ["can only target self"].
