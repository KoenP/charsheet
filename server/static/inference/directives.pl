% main.pl
:- multifile
       (?=)/2,
       on_rest/3,
       todo/1,
       meta_todo/2,
       problem/1,
       resource/2,
       content_source/2.

:- op(600, xfx, upto).
:- op(500, xfy, or).
:- op(400, xfy, and).
:- op(700, xfx, else).
:- op(650, xfx, from).
:- op(650, xfx, from).
:- op(650, xfx, unique_from).
:- op(650, xfx, at).
:- op(1000, xfx, ?=).
:- op(1000, xfx, ?=).
:- op(650, xfx, ft).
:- op(700, xfx, by).
:- op(100, xf, pct).
:- op(1000, xf, qq).
:- op(500, xfx, >:).
:- op(500, fx, ^).

:- multifile suppress_unarmored_ac_formula/0.

% class.pl
:- multifile
       class_option/1,
       choose_subclass_level/1,
       subclass_option/2,
       hd_per_level/2,
       initial_class_base_hp/2,
       max_hp_per_level/2,
       class_saving_throw/2,
       caster/2,
       spellcasting_ability/2,
       asi_level/1,
       max_prepared_spells/2,
       class_skill_list/2.

:- multifile
       required_predicate_for_each_class/1.

% replace.pl
:- multifile
       replaceable_class_options/3,
       replace_at_class_level/4,
       replaceable_character_options/4,
       replace_at_character_level/5.

% feat.pl
:- multifile
       feat_option/1,
       feat_option/2.

% test.pl
:- multifile test_char_level/4.
:- dynamic most_recent_test_character/1.

% class/cleric.pl
:- multifile cleric_domain_spell/2.

% class/warlock.pl
:- multifile eldritch_invocation_option/1.

% class/fighter.pl
:- multifile fighting_style/1.

% options.pl
:- multifile
       options/3,
       options_source/3,
       choice/3,
       choice_creates_category/3,
       ignore/2,
       hide_base_option/3,
       lookup_option_doc/4.
:- dynamic ignore/2.

% format.pl
:- multifile custom_format//1.

% equipment.pl
:- multifile weapon/5.
:- multifile has/1.
:- dynamic has/1.
:- dynamic attuned/1.

% fighting_style.pl
:- multifile fighting_style/1.

% multiclassing.pl
:- multifile multiclass_trait/2, multiclass_trait_source/2.

% character_file.pl
:- multifile
       name/1,
       base_ability/2,
       gain_level/3,
       choice/3.

:- dynamic name/1, base_ability/2, gain_level/3, choice/3.
       
% race.pl
:- multifile
    subrace_option/2,
    racial_speed/2,
    race_shorthand/2,
    race_option/1.

% spell_data.pl
:- dynamic spell_auto_data/2.
:- multifile spell_auto_data/2, extend_spell_data/3.
:- multifile add_spell_effect/2, known_spell_effect/3, suppress_autoderived_spell_effect/1.

% dice.pl
:- op(400, xfx, d).

% bonus.pl
:- multifile
       bonus/2,
       bonus_source/2,
       bonuses_from_source/2,
       bonus_options_source/4.

% trait.pl
:- multifile
       trait/2,
       trait_source/2,
       trait_options/4,
       trait_options_source/4,
       traits_from_source/2,
       choice_member_to_trait/3.

% :- table trait/2 as incremental.

% background.pl
:- multifile background_option/1.

% attacks.pl
:- multifile attack/5, attack_variant/5, add_weapon_note/2, suppress_unarmed/0.

% spellcasting.pl
:- multifile
       known_spell/6,
       spell_property/3,
       extend_class_spell_list/2,
       hide_known_class_spells/3,
       prepare_spell/2,
       spell_origin/1.
:- dynamic prepare_spell/2.

