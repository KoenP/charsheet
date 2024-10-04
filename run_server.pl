:- [server/server].

:- use_module(library(http/thread_httpd)).

:- initialization(run_server).

run_server :-
    http_server(http_dispatch, [port(8000)]).
