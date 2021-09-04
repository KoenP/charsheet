:- multifile
       trait/2,
       trait_source/2,
       traits_from_source/2.

%! trait(Source, Trait)
%
%  Traits are (most of) the idempotent properties (ie the properties
%  that don't stack) of your character.
%
%  Examples:
%  * trait(race(elf), 'fey ancestry')
%  * trait(race(elf), darkvision)
%  * trait(choice(class(_:4), abi_or_feat), feat(alert))
%  
%  Examples of things that idempotent yet are not traits:
%  * the spells your character knows (see known_spell/5)
%  * classes and subclasses of your character (see class/1 and subclass/1)
trait(Source, Trait) :-
    % Traits from trait_source/2 clauses for which we match the requirement.
    trait_source(Source, Trait),
    call(Source).

%! trait(Trait)
%
%  Shorthand for trait/2, when you're not interested in the source.
trait(Trait) :- trait(_, Trait).

%! trait_source(?Source, ?Trait)
%
%  Each trait_source/2 clause gives rise to a corresponding
%  trait/2 clause, *if* call(Source) is true.
trait_source(Source, Trait) :-
    traits_from_source(Source, Traits),
    member(Trait, Traits).

%! traits_from_source(?Source, ?Traits)
%
%  Equivalent to asserting a trait_source(Source, Trait) clause for
%  each member(Trait, Traits).
traits_from_source(_,_) :- false.
