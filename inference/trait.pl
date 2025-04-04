:- multifile
       trait/2,
       trait_source/2,
       trait_options/4,
       trait_options_source/4,
       traits_from_source/2,
       choice_member_to_trait/3,
       trait_from_accumulated_choices/3.

% :- table trait/2 as incremental.

%! trait(Source, Trait)
%
%  Traits are (most of) the named and idempotent properties (ie the properties
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
trait(choice(Source, Id), Trait) :-
    choice_member(Source, Id, Choice),
    choice_member_to_trait(Source, Id, Goal),
    call(Goal, Choice, Trait).

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

%! trait_options_source(?Source, ?Id, ?ToTrait, ?Spec)
%
%  Each trait_options_source/4 clause gives rise to a corresponding
%  trait_options/4 clause, *if* call(Source) is true.
trait_options_source(_,_,_,_) :- false.

%! trait_options(?Source, ?Id, ?Spec, ?ToTrait)
%
%  Each clause gives rise to a corresponding options/3 clause,
%  as well as a corresponding choice_member_to_trait/3 clause.
trait_options(Source, Id, ToTrait, Spec) :-
    trait_options_source(Source, Id, ToTrait, Spec),
    call(Source).
options(Source, Id, Spec) :-
    trait_options(Source, Id, _, Spec).
lookup_option_doc(Source, Id, Option, Doc) :-
    trait_options(Source, Id, ToTrait, _),
    call(ToTrait, Option, Trait),
    (Trait ?= Doc).

% Don't display traits that have already been picked as options to the
% user (see inspect_options/3).
hide_base_option(Source, Id, Option) :-
    trait_options(Source, Id, ToTrait, _),
    call(ToTrait, Option, Trait),
    trait(Trait).

%! class_trait(?Class:atomic, ?Origin, ?Trait)
%
%  Query your character's traits that originate from Class.
class_trait(Class, Origin, Trait) :-
    trait(Origin, Trait),
    class_origin_to_class(Origin, Class).

%! choice_member_to_trait(Source, Id, ToTrait)
%
%  Every clause of this predicate declares that some choice/3 clause
%  should give rise to (a) corresponding trait(s).
%  If choice_member(Source, Id, Choice) is true, then this predicate
%  will make sure trait(choice(Source, Id), X) is true if call(ToTrait,
%  Choice, X) is true.
choice_member_to_trait(Source, Id, ToTrait) :-
    trait_options(Source, Id, ToTrait, _).
wrap(Functor, X, FunctorX) :- FunctorX =.. [Functor, X].
deep_wrap(NestedFunctors, X, NestedFunctorsX) :-
    NestedFunctors =.. [Functor, SubNestedFunctors],
    !,
    deep_wrap(SubNestedFunctors, X, SubX),
    NestedFunctorsX =.. [Functor, SubX].
deep_wrap(Atom, X, AtomX) :-
    Atom =.. [Atom],
    !,
    wrap(Atom, X, AtomX).

%! notable_trait(?Origin, ?Trait)
%
%  Somewhat arbitrarily, a trait is "notable" when it's not an
%  expertise; skill; language; resistance/immunity; or proficiency in tool, weapon, armor or shield.
notable_trait(Origin, Trait) :-
    trait(Origin, Trait),
    \+ member(Trait,
              [expertise(_), skill(_), language(_), tool(_),
               weapon(_), armor(_), shield, saving_throw(_), resistance(_,_)]).

notable_traits :-
    forall(notable_trait(_,T), writeln_quoted_term(T)).

%! notable_traits_by_category(?TraitsPerCat)
%
%  List "notable traits" (see notable_trait/2), arranged per category.
notable_traits_by_category(TraitsPerCat) :-
    findall(Cat-Trait,
            (notable_trait(Origin,Trait),
             origin_category_or_uncategorized(Cat,Origin)),
            CatTraits),
    sort(1, @=<, CatTraits, Sorted),
    group_pairs_by_key(Sorted, TraitsPerCat).

%! traits_by_category(?TraitsPerCat)
traits_by_category(TraitsPerCat) :-
    findall(Cat-Trait,
            (trait(Origin,Trait), origin_category_or_uncategorized(Cat,Origin)),
            CatTraits),
    sort(1, @=<, CatTraits, Sorted),
    group_pairs_by_key(Sorted, TraitsPerCat).

%! trait_from_level_reached(+Level:int, ?Origin, ?Trait)
%
%  True iff Trait was gained at the given Level automatically upon
%  reaching that level (that is, at that level no active choice was
%  required).
trait_from_level_reached(Level, Origin, Trait) :-
    trait(Origin, Trait),
    Origin \= choice(_,_),
    origin_level(Origin, Level).


%! trait_from_accumulated_choices(?Origins, ?Id, ?Spec)
%
%  Some character features (for example, the ranger's "natural
%  explorer" feature) allow the user to select additional options at
%  later levels.
%  However, there is only one description for the entire feature, as
%  opposed to a description per option.
%  Consequently, we want to generate one card for the whole feature,
%  with the title containing the options chosen by the user (for
%  example, "Natural Explorer (Arctic, Grassland)").
%
%  `trait_from_accumulated_choices(Origins, Id, Spec)`
%  generates an `options(Origin, Id, Spec)` for each
%  `Origin` in `Origins` such that `call(Origin)` is true.
%  The resulting choices are then accumulated into a trait.
%  For example, if the user picked `arctic` and `mountains`
%  for the `'natural explorer'` trait, the resulting trait will be
%  `trait(ranger >: 1, [arctic, mountains])`.
trait_from_accumulated_choices(_,_,_) :- false.

% Generate options.
options_source(Origin, Id, Spec) :-
    trait_from_accumulated_choices(Origins, Id, Spec),
    member(Origin, Origins).

% Don't allow repeated choices.
hide_base_option(Origin, Id, Choice) :-
    trait_from_accumulated_choices(Origins, Id, _),
    member(Origin, Origins),
    member(ChoiceOrigin, Origins),
    choice(ChoiceOrigin, Id, Choice).

% Generate the trait if at least one choice has been made.
trait_source(FirstOrigin, Trait) :-
    trait_from_accumulated_choices(Origins, Id, _),
    Origins = [FirstOrigin | _],
    findall(Choice, (member(Origin,Origins), choice(Origin, Id, Choice)), Choices),
    Choices = [_|_],
    flatten(Choices, FlatChoices),
    Trait =.. [Id, FlatChoices].

% Custom doc lookup rule (ignoring the selected option,
% as the doc is identical for all options).
lookup_option_doc(Origin, Id, _, Doc) :-
    trait_from_accumulated_choices(Origins, Id, _),
    member(Origin, Origins),
    DocTm =.. [Id, _],
    (DocTm ?= Doc),
    !.
