:- discontiguous
       spell/3,
       spell/2.

term_field(Term, Field, Value) :-
    Term =.. L,
    member(Field:Value, L).

spell(Name) :-
    spell(Name, _).
spell(Name, class, Class) :-
    spell(Name, Properties),
    term_field(Properties, classes, Classes),
    member(Class, Classes).
spell(Name, component, Component) :-
    spell(Name, Properties),
    term_field(Properties, components, Components),
    member(Component, Components).
spell(Name, Prop, Value) :-
    spell(Name, Properties),
    term_field(Properties, Prop, Value).

% Spell list.
spell(shillelagh,
      properties(
          school: transmutation,
          level: 0,
          classes: [druid],
          casting_time: bonus,
          range: touch,
          components: [v, s, m("mistletoe, a shamrock leaf, and a club or quarterstaff")],
          duration: (minutes(1)))).
describe(spell(shillelagh), "The wood of a club or quarterstaff you are holding is imbued with nature's power. For the duration, you can use your spellcasting ability instead of Strength for the attack and damage rolls of melee attacks using that weapon, and the weapon's damage die becomes a d8. The weapon also becomes magical, if it isn't already. The spell ends if you cast it again or if you let go of the weapon.").

spell(guidance,
      properties(
          school: divination,
          level: 0,
          classes: [druid, cleric], % TODO
          casting_time: action,
          range: touch,
          components: [v, s],
          duration: concentration(minutes(1)))).
describe(spell(guidance), "You touch one willing creature. Once before the spell ends, the target can roll a d4 and add the number rolled to one ability check of its choice. It can roll the die before or after making the ability check. The spell then ends.").


% TODO I'm not sure yet whether I like this.
% It's a bit of metaprogramming that adds predicates like spell_school/2, spell_range/2, ...
register_spell_property(SpellName, Prop, Value) :-
    spell(SpellName, Prop, Value),
    atomic_concat('spell_', Prop, PredName),
    Fact =.. [PredName, SpellName, Value],
    writeln(Fact),
    assert(Fact).

:- forall(register_spell_property(_, _, _), true).

