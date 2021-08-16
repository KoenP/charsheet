:- [charsheet].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 1
name('Zahar').
base_ability(str, 9).
base_ability(dex, 14).
base_ability(con, 17).
base_ability(int, 13).
base_ability(wis, 10).
base_ability(cha, 17).

race(human).

initial_class(warlock).

choose_traits(class(warlock:1), spell, ['hellish rebuke', 'burning hands']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 2
gain_level(2, warlock, hp_avg).
choose_traits(class(warlock:2), spell, [sleep]).
replace_traits(class(warlock:2), spell, ['hellish rebuke'], [hex]).


gain_level(3, warlock, hp_avg).
