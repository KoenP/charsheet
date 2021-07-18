:- discontiguous
       race/1,
       gain_level/3,
       choose_subclass/2,
       pick_feat/2,
       pick_abi/2.

:- [charsheet].

choose_traits(_,_,_) :- false.

equipped(_) :- false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Level 1
name('Zahar').
initial_class(sorcerer).
race(human).
base_ability(str, 9).
base_ability(dex, 14).
base_ability(con, 17).
base_ability(int, 13).
base_ability(wis, 10).
base_ability(cha, 17).

choose_subclass(sorcerer, 'draconic bloodline').
choose_traits(subclass(sorcerer:1, 'draconic bloodline'),
              'dragon ancestor',
              [dragon_ancestor(red)]).

choose_traits(class(sorcerer:1), cantrip, [ learn_spell(sorcerer, 'fire bolt'),
                                            learn_spell(sorcerer, 'acid splash'),
                                            learn_spell(sorcerer, 'minor illusion'),
                                            learn_spell(sorcerer, message) ]).
choose_traits(class(sorcerer:1), spell, [learn_sorcerer_spell('burning hands'),
                                         learn_sorcerer_spell('mage armor')]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gain_level(2, sorcerer, hp_avg).

%choose_traits(class(sorcerer:2), replace_spell, [forget_sorcerer_spell('burning hands'),
%                                                 learn_sorcerer_spell(sleep)]).
choose_traits(class(sorcerer:2), spell, [learn_sorcerer_spell('scorching ray')]).

gain_level(3, sorcerer, hp_avg).
gain_level(4, sorcerer, hp_avg).
gain_level(5, sorcerer, hp_avg).
gain_level(6, sorcerer, hp_avg).
