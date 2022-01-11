:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_header)).

:- http_handler(root(.), remote_query(Method), [method(Method)]).

run_server :-
    http_server(http_dispatch, [port(8000)]).

%:- run_server.

script("let query = function(q) {fetch(\"/?\" + new URLSearchParams({query: q}), {method:\"post\"}).then(function(response) {response.text().then(function(text) {console.log(text);});});};").
script_todo("let todo = function() {fetch(\"/?\" + new URLSearchParams({todo: \"foo\"}), {method:\"post\"}).then(function(response) {response.text().then(function(text) {console.log(text);});});};").

remote_query(get, _Request) :-
    script(Script),
    script_todo(ScriptTodo),
    reply_html_page(title("Query me!"),
                    [h1("Query me!"),
                     p("Ga naar de console en typ bijvoorbeeld"),
                     p(i("query(\"ac(X)\")")),
                     p("of"),
                     p(i("query(\"ability_mod(int,X)\")")),
                     p("in. Momenteel werkt het enkel als er precies 1 variabele in zit en die variabele heet \"X\"."),
                     script(type="text/javascript", [Script]),
                     script(type="text/javascript", [ScriptTodo])]).
remote_query(post, Request) :-
    http_parameters(Request, [query(QueryString,[optional(true)]), todo(TodoString,[optional(true)])]),
    handle_request_param(QueryString, TodoString).
handle_request_param(QueryString, _) :-
    ground(QueryString),
    format('Content-type: text/plain~n~n'),
    read_term_from_atom(QueryString, Query, [variables([X])]),
    call(Query),
    format(X).
handle_request_param(_, TodoString) :-
    ground(TodoString),
    findall(Entry, todo_entry_jsondict(Entry), Entries),
    format("Access-Control-Allow-Origin: *~n"),
    reply_json_dict(Entries).

todo_entry_jsondict(_{origin:OriginStr, id:IdStr, spec:SpecDict}) :-
    todo(options(Origin, Id, Spec)),
    term_string(Origin, OriginStr),
    term_string(Id, IdStr),
    inspect_spec(Spec, ISpec),
    spec_to_jsondict(ISpec, SpecDict).

spec_to_jsondict(N unique_from Spec, _{unique_from: _{number:N, spec:SpecDict}}) :-
    spec_to_jsondict(Spec, SpecDict).
spec_to_jsondict(N from Spec, _{from: _{number:N, spec:SpecDict}}) :-
    spec_to_jsondict(Spec, SpecDict).
spec_to_jsondict(List, List) :-
    is_list(List).

