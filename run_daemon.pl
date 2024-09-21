:- [server].

:- use_module(library(http/http_unix_daemon)).

:- initialization(http_daemon, main).

http_unix_daemon:http_server_hook(Options) :-
    http_server(http_dispatch, Options).
