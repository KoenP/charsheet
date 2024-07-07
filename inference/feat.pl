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
bonus_source(trait(feat(alert)), init + 5).

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

% FEY TOUCHED
%%%%%%%%%%%%%
feat_option('fey touched').
bonus_options_source(feat('fey touched'), asi, id,
                     from_list([int+1, wis+1, cha+1])).
options_source(feat('fey touched'), spell, fey_touched_spell).
known_spell(feat('fey touched'), Abi, always, [per_rest(long,1)], no, Spell) :-
    choice(feat('fey touched'), asi, Abi),
    (Spell = 'misty step' ; choice(feat('fey touched'), spell, Spell)).
fey_touched_spell(Spell) :-
    spell_data(Spell, Data),
    Data.get(level) = 1, Data.get(school) = School,
    member(School, [enchantment, divination]).
feat('fey touched') ?= "Your exposure to the Feywild's magic has changed you, granting you the following benefits:

Increase your Intelligence, Wisdom, or Charisma score by 1, to a maximum of 20.

You learn the Misty Step spell and one 1st-level spell of your choice. The 1st-level spell must be from the Divination or Enchantment school of magic. You can cast each of these spells without expending a spell slot. Once you cast either of these spells in this way, you can’t cast that spell in this way again until you finish a long rest. You can also cast these spells using spell slots you have of the appropriate level. The spells’ spellcasting ability is the ability increased by this feat.".

% METAMAGIC ADEPT
%%%%%%%%%%%%%%%%%
feat_option('metamagic adept').

% Pick two metamagic options.
options_source(feat('metamagic adept'), metamagic, 2 unique_from metamagic_option).
   
% One option can be replaced any time the character gets an ASI or feat from their class
% (except on the level this feat is picked).
replace_at_character_level(feat('metamagic adept'), CharLevel, metamagic, 1, metamagic_option) :-
    options(Class >: ClassLevel, 'asi or feat', _),
    origin_level(Class >: ClassLevel, CharLevel),
    trait(Origin, feat('metamagic adept')),
    \+ origin_level(Origin, CharLevel).

% Actually add the chosen metamagic options as traits.
trait(feat('metamagic adept'), metamagic(MetaMagic)) :-
    selected_at_current_level(feat('metamagic adept'), metamagic, MetaMagic).

% Metamagic adept grants two sorcery points which can only be used for metamagic, not for
% conversion into spell slots, so they need to be tracked as a separate resource from regular
% sorcery points.
resource(metamagic, 'adept sorcery point', 2) :-
    feat('metamagic adept').
on_rest(long, 'adept sorcery point', 'full restore').

% Documentation.
feat('metamagic adept') ?= "You’ve learned how to exert your will on your spells to alter how they function:

    - You learn two Metamagic options of your choice from the sorcerer class. You can use only one Metamagic option on a spell when you cast it, unless the option says otherwise. Whenever you reach a level that grants the Ability Score Improvement feature, you can replace one of these Metamagic options with another one from the sorcerer class.

    - You gain 2 sorcery points to spend on Metamagic (these points are added to any sorcery points you have from another source but can be used only on Metamagic). You regain all spent sorcery points when you finish a long rest.".
