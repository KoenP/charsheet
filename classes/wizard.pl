:- [wizard/spells].

class_option(wizard). 
hd_per_level(wizard, 1 d 6).

gain_spell_slots(wizard, spell_level(1), [1,1,2,3]).
gain_spell_slots(wizard, spell_level(2), [3,3,4]).
gain_spell_slots(wizard, spell_level(3), [5,5,6]).
gain_spell_slots(wizard, spell_level(4), [7,8,9]).
gain_spell_slots(wizard, spell_level(5), [7,8,9]).
gain_spell_slots(wizard, spell_level(6), [11,19]).
gain_spell_slots(wizard, spell_level(7), [13,20]).
gain_spell_slots(wizard, spell_level(8), [15]).
gain_spell_slots(wizard, spell_level(9), [17]).

% Calculate how many spells a wizard can prepare.
max_prepared_spells(wizard, N) :-
    ability_mod(int, IntMod),
    class_level(wizard:Level),
    N is Level + IntMod.
