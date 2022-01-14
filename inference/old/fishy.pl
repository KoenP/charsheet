wrap_trait_options(_,_,_,_) :- false.

fishy(wrap_trait_option(class(A),B,C,D), "use wrap_class_trait_option") :-
    wrap_trait_option(class(A), B, C, D),
    \+ wrap_class_trait_option(A, B, C, D).

fishy(wrap_class_trait_option(class(A),B,C,D), "class(X) -> X") :-
    wrap_class_trait_option(class(A),B,C,D).

fishy(wrap_trait_option(Class,B,C,D), "use wrap_class_trait_option") :-
    wrap_trait_option(Class,B,C,D),
    class_option(Class).

fishy(wrap_trait_option(Class:_,B,C,D), "use wrap_class_trait_option") :-
    wrap_trait_option(Class:_,B,C,D),
    class_option(Class).

fishy(trait_options(Origin, Name), "origin doesn't have a level") :-
    trait_options(Origin, Name, _),
    \+ trait_origin_level(Origin, _).

fishy(wrap_trait_options(A,B,C,D), "it's wrap_trait_option, not wrap_trait_options") :-
    wrap_trait_options(A,B,C,D).

fishy(override_spell_known_property(Spell, Origin, _, _), "you have an override for a spell the character doesn't know.") :-
    override_spell_known_property(Spell, Origin, _, _),
    \+ (spell_known(Spell, Origin, _, _, _)).

fishy(override_spell_known_property(Name, Origin, components, Value), "you probably don't want to override the components property, use spell_known_lose_component/3 instead.") :-
    override_spell_known_property(Name, Origin, components, Value).
fishy(override_spell_known_property(Name, Origin, component, Value), "you probably don't want to override the component property") :-
    override_spell_known_property(Name, Origin, component, Value).

fishy(override_spell_known_property(Name, Origin, Property, Value1), "two competing overrides") :- 
    override_spell_known_property(Name, Origin, Property, Value1),
    override_spell_known_property(Name, Origin, Property, Value2),
    Value1 \= Value2.
