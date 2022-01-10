:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).

:- http_handler(root(.), remote_query(Method), [method(Method)]).

run_server :-
    http_server(http_dispatch, [port(8000)]).

%:- run_server.

script("let query = function(q) {fetch(\"/?\" + new URLSearchParams({query: q}), {method:\"post\"}).then(function(response) {response.text().then(function(text) {console.log(text);});});};").

remote_query(get, _Request) :-
    script(Script),
    reply_html_page(title("Query me!"),
                    [h1("Query me!"),
                     p("Ga naar de console en typ bijvoorbeeld"),
                     p(i("query(\"ac(X)\")")),
                     p("of"),
                     p(i("query(\"ability_mod(int,X)\")")),
                     p("in. Momenteel werkt het enkel als er precies 1 variabele in zit en die variabele heet \"X\"."),
                     script(type="text/javascript", [Script])]).
remote_query(post, Request) :-
    http_parameters(Request, [query(QueryString,[])]),
    format('Content-type: text/plain~n~n'),
    read_term_from_atom(QueryString, Query, [variables([X])]),
    call(Query),
    format(X).
