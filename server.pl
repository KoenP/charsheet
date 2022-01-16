:- [inference/main].
%:- [characters/mrdragon].

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
:- http_handler(root(sheet), send_char_sheet, [priority=1]).
:- http_handler(root(.), serve_files_in_directory(html), [prefix]).
:- http_handler(css(.), serve_files_in_directory(css), [prefix]).
:- http_handler(js(.), serve_files_in_directory(js), [prefix]).
:- http_handler(root(request), remote_query, [method=post, prefix]).

:- initialization(run_server).

%! run_server
%
%  Run an HTTP server on port 8000. 
%
%  The server responds to the following GET requests:
%   - =|/|= : Serves =|static/index.html|=.
%   - =|/sheet|= : Serves a dynamically generated HTML rendering of the
%      current character sheet.
%   - Any other path: Serves the matching file path in the =|static|= folder.
%
%  The server responds to the following POST requests:
%   - =|/request/todo|=: Serve the current character's =|todo|= as JSON.
%   - =|/request/query?q=_|=: Run the query =|q|= which should contain a
%      variable =|X|=. The server responds with whatever =|X|= unified with,
%      rendered as plaintext.
%   - =|/request/list_characters|=: Replies with a JSON list of
%      characters currently saved on the server.
%   - =|/request/load_character?name=_|=: Load the given character
%      with matching =name=.
%   - =|/request/save_character|=: Save all changes made to the
%      current character.
%   - =|/request/choice?source=_&id=_&choice=_|=: Register the given
%      choice for the character. This is not committed to permanent
%      storage until =save_character= is requested.
run_server :-
    http_server(http_dispatch, [port(8000)]).

send_char_sheet(_) :-
    char_sheet_head(Head),
    char_sheet_body(Body),
    reply_html_page(Head, Body).

remote_query(Request) :-
    member(path_info(PathInfo), Request),
    files_ex:strip_trailing_slash(PathInfo, Stripped),
    writeln("Access-Control-Allow-Origin: *"),
    remote_query(Request, Stripped).
remote_query(_, '/todo') :-
    findall(Entry, todo_entry_jsondict(Entry), Entries),
    reply_json_dict(Entries).
remote_query(Request, '/query') :-
    http_parameters(Request, [q(QueryString,[])]),
    format('Content-type: text/plain~n~n'),
    read_term_from_atom(QueryString, Query, [variables([X])]),
    call(Query),
    format(X).
remote_query(_, '/list_characters') :-
    findall(Char, saved_character(Char), Chars),
    reply_json_dict(Chars).
remote_query(Request, '/load_character') :-
    http_parameters(Request, [name(Name,[])]),
    load_character_file(Name),
    reply_json_dict("success!").
remote_query(_, '/save_character') :-
    write_character_file,
    reply_json_dict("success!").
remote_query(Request, '/choice') :-
    http_parameters(Request, [source(SourceStr,[]),
                              id(IdStr,[]),
                              choice(ChoiceStr,[])]),
    term_string(Source, SourceStr),
    term_string(Id, IdStr),
    term_string(Choice, ChoiceStr),
    assert(choice(Source, Id, Choice)),
    reply_json_dict("success!").

todo_entry_jsondict(_{origin:OriginStr, id:IdStr, spec:SpecDict}) :-
    todo(options(Origin, Id, Spec)),
    quoted_term_string(Origin, OriginStr),
    quoted_term_string(Id, IdStr),
    inspect_spec(Spec, ISpec),
    spec_to_jsondict(ISpec, SpecDict).

spec_to_jsondict(N unique_from Spec, _{unique_from: _{number:N, spec:SpecDict}}) :-
    spec_to_jsondict(Spec, SpecDict).
spec_to_jsondict(N from Spec, _{from: _{number:N, spec:SpecDict}}) :-
    spec_to_jsondict(Spec, SpecDict).
spec_to_jsondict(List, StrList) :-
    maplist(quoted_term_string, List, StrList).

quoted_term_string(T, S) :-
    term_string(T, S, [quoted(true)]).
