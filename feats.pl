:- discontiguous
       feat_option/1,
       feat_trait/2.

:- multifile
       pick_feat/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Feat inferences.
trait(feat(Feat), Trait) :-
    feat(Feat),
    feat_trait(Feat, Trait).

feat(Feat) :-
    feat(_, Feat).
feat(Level, Feat) :-
    pick_trait(level(Level), abi_or_feat, Feat).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% List of feats.
feat_option(alert).
describe(feat(alert), "Always on the lookout for danger, you gain the following benefits:
    - You can’t be surprised while you are conscious.
    - You gain a +5 bonus to initiative.
    - Other creatures don’t gain advantage on attack rolls against you as a result of being hidden from you.
").
feat_trait(alert, add_initiative(5)).

feat_option(durable).
describe(feat(durable), Info) :-
    Info = "Hardy and resilient, you gain the following benefits:
    Increase your Constitution score by 1, to a maximum of 20.
    When you roll a Hit Die to regain hit points, the minimum number of hit points you regain from
    the roll equals twice your Constitution modifier (minimum of 2).".
feat_trait(durable, abi(con, 1)).
