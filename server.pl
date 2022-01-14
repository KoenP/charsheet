:- [inference/main].
:- [characters/mrdragon].

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_files)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_error)).
:- use_module(library(sgml)).

user:file_search_path(html, 'static').
user:file_search_path(css, 'static/css').
user:file_search_path(js, 'static/js').

:- http_handler(root(.), http_reply_file('static/index.html',[]), [priority=1]).
:- http_handler(root(.), serve_files_in_directory(html), [prefix]).
:- http_handler(css(.), serve_files_in_directory(css), [prefix]).
:- http_handler(js(.), serve_files_in_directory(js), [prefix]).
:- http_handler(root(request), remote_query, [method=post]).

:- initialization(http_server(http_dispatch, [port(8000)])).

remote_query(Request) :-
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

