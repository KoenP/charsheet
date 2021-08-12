% Look up the value associated with the largest key smaller than or
% equal to the parameter key, in the provided table.
ordered_lookup_largest_leq([Key -> Value|Table], KeyToLookup, ValueFound) :-
    Key =< KeyToLookup,
    ( ordered_lookup_largest_leq(Table, KeyToLookup, ValueFound), !
    ; ValueFound = Value
    ).
