:- multifile
       feat_option/1,
       feat_option/2.

feat_option(_,_) :- false.

feat(Feat) :- trait(feat(Feat)).
selectable_feat_option(Feat) :-
    feat_option(Feat).
selectable_feat_option(Feat) :-
    feat_option(Cond, Feat),
    call(Cond).

feat_option(alert).
feat(alert) ?= "Always on the lookout for danger, you gain the following benefits:
    - You can’t be surprised while you are conscious.
    - You gain a +5 bonus to initiative.
    - Other creatures don’t gain advantage on attack rolls against you as a result of being hidden from you.".
bonus_source(feat(alert), init + 5).

feat_option(durable).
feat(durable) ?= "Hardy and resilient, you gain the following benefits:
    Increase your Constitution score by 1, to a maximum of 20.
    When you roll a Hit Die to regain hit points, the minimum number of hit points you regain from
    the roll equals twice your Constitution modifier (minimum of 2).".
bonus_source(feat(durable), con+1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PLAYER HANDBOOK (NOT SRD)                                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
feat_option(lucky).
feat(lucky) ?= "Three times per long rest: reroll a die (own die or attack roll against you) after the roll, but before outcome is determined. Pick whichever outcome you prefer.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TASHA'S CAULDRON OF EVERYTHING                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
feat_option('fey touched').
options_source(feat('fey touched'), asi, from_list([int+1, wis+1, cha+1])).
options_source(feat('fey touched'), spell, fey_touched_spell).
bonus_source(choice(feat('fey touched'), asi, Abi), Abi + 1).
known_spell(feat('fey touched'), Abi, always, [per_rest(long,1)], no, Spell) :-
    choice(feat('fey touched'), asi, Abi),
    (Spell = 'misty step' ; choice(feat('fey touched'), spell, Spell)).
fey_touched_spell(Spell) :-
    spell_data(Spell, Data),
    Data.get(level) = 1, Data.get(school) = School,
    member(School, [enchantment, divination]).
