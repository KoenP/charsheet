

fishy(wrap_class_trait_option(A,B,C,D) / wrap_trait_option(class(A),B,C,D)) :-
    wrap_trait_option(class(A), B, C, D).

fishy(wrap_class_trait_option(A,B,C,D), wrap_class_trait_option(class(A),B,C,D)) :-
    wrap_class_trait_option(class(A),B,C,D).


