:- module(user_db,
          [ record_user/2,
            match_user/2
          ]).

:- use_module(library(persistency)).

:- persistent
     user(user_name:string, password:string).

:- db_attach('users.pl', []).

record_user(UserName, Password) :-
    ground(UserName),
    ground(Password),
    \+ user(UserName, _),
    assert_user(UserName, Password).

match_user(UserName, Password) :-
    ground(UserName),
    ground(Password),
    user(UserName, Password).
