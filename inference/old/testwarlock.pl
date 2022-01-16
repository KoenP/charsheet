:- [charsheet].

highlight_spell(_) :- false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 1
name('testwarlock').
base_ability(str, 9).
base_ability(dex, 14).
base_ability(con, 17).
base_ability(int, 13).
base_ability(wis, 10).
base_ability(cha, 17).

race(human).

initial_class(warlock).

choose_traits(class(warlock:1), cantrip, ['eldritch blast', 'create bonfire']).
choose_traits(class(warlock:1), spell, ['hellish rebuke', 'burning hands']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 2
gain_level(2, warlock, hp_avg).
choose_traits(class(warlock:2), spell, [sleep]).
replace_traits(class(warlock:2), spell, ['hellish rebuke'], [hex]).
choose_traits(class(warlock:2), 'eldritch invocation', ['agonizing blast', 'beguiling influence']).


gain_level(3, warlock, hp_avg).
choose_traits(class(warlock:3), 'pact boon', [tome]).
choose_traits(pact_boon(tome), cantrip, ['poison spray', 'fire bolt', 'minor illusion']).

gain_level(4, warlock, hp_avg).
gain_level(5, warlock, hp_avg).
gain_level(6, warlock, hp_avg).
gain_level(7, warlock, hp_avg).
gain_level(8, warlock, hp_avg).
gain_level(9, warlock, hp_avg).

choose_traits(class(warlock:7), 'eldritch invocation', ['book of ancient secrets']).
choose_traits(eldritch_invocation('book of ancient secrets'),
              spell,
              ['detect magic', identify]).

choose_traits(class(warlock:9), 'eldritch invocation', ['ascendant step']).


%trait(eldritch_invocation('beguiling influence')).