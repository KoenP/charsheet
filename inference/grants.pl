
grants_trait(Source, Trait) :-
    clause(traits_from_source(Source, Traits), _),
    \+ var(Traits),
    member(Trait, Traits).
grants_trait(Source, Trait) :-
    (clause(trait_source(Source, Trait), _) ; clause(multiclass_trait_source(Source, Trait), _)),
    \+ var(Trait).

% TODO generate overview of what a class, race, or background will give a character.

%grants_bonus(Source, Bonus) :-
%    clause(bonuses_from_source(Source, Bonuses), _),
%    \+ var(Bonuses),
%    member(Bonus, Bonuses).
%grants_bonus(Source, Bonus) :-
%    clause(bonus_source(Source, Bonus), _),
%    \+ var(Bonus).

grants_trait_options(Source, Id) :-
    clause(trait_options_source(Source, Id, _, _), _),
    \+ var(Id).
    
    

    %member(Predicate, [trait_source(Source, Trait), multiclass_trait_source(Source, Trait)]),
    %clause(Predicate, Clause),
    %call((Clause ; true)),
    %\+ var(Trait).

%grants_options(Source, Id) :-
%    clause(options(Source, Id), Clause),
    
