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
choose_traits(race(human), language, [language(elvish)]).

initial_class(sorcerer).
choose_traits(class(sorcerer:1), cantrip, [ learn_spell(sorcerer, 'fire bolt'),
                                            learn_spell(sorcerer, 'blade ward'),
                                            learn_spell(sorcerer, 'minor illusion'),
                                            learn_spell(sorcerer, message) ]).
choose_traits(class(sorcerer:1), spell, [learn_sorcerer_spell('burning hands'),
                                         learn_sorcerer_spell('magic missile')]).
choose_traits(class(sorcerer:1), skill, [skill(intimidation), skill(insight)]).

choose_subclass(sorcerer, 'draconic bloodline').
choose_traits(subclass(sorcerer:1, 'draconic bloodline'),
              'dragon ancestor',
              [dragon_ancestor(red)]).

background(noble).
choose_traits(background(noble), gaming_set, [tool('dice set')]).
choose_traits(background(noble), language, [language(celestial)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gain_level(2, sorcerer, hp_avg).
forego(class(sorcerer:2), replace_spell).

%choose_traits(class(sorcerer:2), replace_spell, [forget_sorcerer_spell('burning hands'),
%                                                 learn_sorcerer_spell(sleep)]).
choose_traits(class(sorcerer:2), spell, [learn_sorcerer_spell('scorching ray')]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gain_level(3, sorcerer, hp_avg).
forego(class(sorcerer:3), replace_spell).
choose_traits(class(sorcerer:3), metamagic, [metamagic('careful spell'), metamagic('empowered spell')]).
choose_traits(class(sorcerer:3), spell, [learn_sorcerer_spell('alter self')]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gain_level(4, sorcerer, hp_avg).
forego(class(sorcerer:4), replace_spell).
choose_traits(class(sorcerer:4), asi_or_feat, [cha+2]).
choose_traits(class(sorcerer:4), cantrip, [learn_spell(sorcerer, friends)]).
choose_traits(class(sorcerer:4), spell, [learn_sorcerer_spell('misty step')]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gain_level(5, sorcerer, hp_avg).
forego(class(sorcerer:5), replace_spell).
choose_traits(class(sorcerer:5), spell, [learn_sorcerer_spell('fireball')]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gain_level(6, sorcerer, hp_avg).
forego(class(sorcerer:6), replace_spell).
choose_traits(class(sorcerer:6), spell, [learn_sorcerer_spell('counterspell')]).


gain_level(7, druid, hp_avg).
choose_traits(class(druid:1), cantrip, [learn_spell(druid, shillelagh), learn_spell(druid, guidance)]).
have(quarterstaff).

gain_level(8, druid, hp_avg).
