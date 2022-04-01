:- discontiguous
       feat_option/1,
       feat_option/2.

feat(Feat) :- trait(feat(Feat)).
selectable_feat_option(Feat) :-
    feat_option(Feat).
selectable_feat_option(Feat) :-
    feat_option(Cond, Feat),
    call(Cond).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% List of feats.
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

feat_option(lucky). % not SRD
feat(lucky) ?= "Three times per long rest: reroll a die (own die or attack roll against you) after the roll, but before outcome is determined. Pick whichever outcome you prefer.".
