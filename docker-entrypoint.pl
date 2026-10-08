:- [server/server].

:- use_module(library(http/thread_httpd)).

:- initialization(main, main).

main :-
    ( getenv('CHARSHEET_PORT', PortAtom) -> atom_number(PortAtom, Port) ; Port = 8000 ),
    http_server(http_dispatch, [port(Port), ip('0.0.0.0')]),
    thread_get_message(_).
