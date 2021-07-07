:- [spells].

:- discontiguous
       spell/3.

spell_save_dc(Abil, DC) :-
    trait(spellcasting(Abil)),
    ability_mod(Abil, Mod),
    proficiency_bonus(Bonus),
    DC is 8 + Bonus + Mod.

spell_attack_modifier(Abil, Mod) :-
    trait(spellcasting(Abil)),
    ability_mod(Abil, AbilMod),
    proficiency_bonus(Bonus),
    Mod is Bonus + AbilMod.

spell_slots(Class, SpellLevel, Slots) :-
    gain_spell_slots(Class, SpellLevel, Gains),
    class_level(Class:ClassLevel),
    findall(X, (member(X,Gains),X=<ClassLevel), Xs),
    length(Xs, Slots),
    Slots > 0.

% The predicate spell_at_will_attack/5 can be asserted for a spell
% (usually a cantrip) that in some way lets the PC attack at will
% (directly, as with fire bolt, or indirectly, as with shillelagh).
% This code will then make sure this shows up on the list of attacks.
attack(Spell, Range, DamageType, ToHit, Damage, Notes) :-
    spell_at_will_attack(Spell, Range, DamageType, DamageDice, Notes),
    spell_known(Spell, _, Ability, always_available, at_will),
    ability_mod(Ability, Mod),
    proficiency_bonus(ProfBon),
    ToHit is Mod + ProfBon,
    spell_attack_damage(Mod, DamageDice, Damage).
spell_attack_damage(Mod, Dice + mod, Dice + Mod).
spell_attack_damage(_, X d Y, X d Y).
    

% Spells can be
% - learnable (= you have spell slots for this spell level and class)
% - known (= preparable)
% - prepared
spell_learnable(Class, SpellName) :-
    class(Class),
    spell_class(SpellName, Class),
    spell(SpellName, level, 0).
spell_learnable(Class, SpellName) :-
    class(Class),
    spell(SpellName, level, SpellLevel),
    spell_slots(Class, spell_level(SpellLevel), N),
    N > 0.
% TODO remove known spells from learnable spells without infinite looping

spell_known(Spell) :-
    spell_known(Spell, _, _, _, _).

% Some predicates to extract info from spell specifications.
term_field(Term, Field, Value) :-
    Term =.. L,
    member(Field:Value, L).

spell(Name) :-
    spell(Name, _).
spell(Name, component, Component) :-
    spell(Name, Properties),
    term_field(Properties, components, Components),
    member(Component, Components).
spell(Name, Prop, Value) :-
    spell(Name, Properties),
    term_field(Properties, Prop, Value).

% Cantrips tend to get stronger at character levels 5, 11, and 17 (the
% rules don't require these levels but this configuration is common
% enough to warrant being supported explicitly).
% The cantrip_scaling/1 predicate calculates a number ranging from 1 to 4,
% 1 for characters < lvl5, 2 for characters from lvl5 to lvl10, ...
cantrip_scale(Scale) :-
    level(Level),
    ( Level < 5  -> !, Scale = 1
    ; Level < 11 -> !, Scale = 2
    ; Level < 17 -> !, Scale = 3
    ; Scale = 4
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Describe spell.
%(spell(Spell) ?= Info) :-
%    spell(Spell, )




% TODO I'm not sure yet whether I like this.
% It's a bit of metaprogramming that adds predicates like spell_school/2, spell_range/2, ...
%register_spell_property(SpellName, Prop, Value) :-
%    spell(SpellName, Prop, Value),
%    atomic_concat('spell_', Prop, PredName),
%    Fact =.. [PredName, SpellName, Value],
%    %writeln(Fact),
%    assert(Fact).
%
%:- forall(register_spell_property(_, _, _), true).
