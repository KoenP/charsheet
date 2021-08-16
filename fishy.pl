

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
