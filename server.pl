:- [inference/main].

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

user:file_search_path(html, 'client/dist').
user:file_search_path(css, 'client/dist/css').
user:file_search_path(js, 'client/dist/js').

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
%   - =|/request/list_characters|=: Replies with a JSON object
%      containing at least a "list" field with a list of characters
%      currently saved on the server, and optionally a "current" field
%      with the name of the currently loaded character.
%   - =|/request/new_character?name=_|=: Create a new character with the
%      given name.
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
remote_query(Request, '/query') :-
    http_parameters(Request, [q(QueryString,[])]),
    format('Content-type: text/plain~n~n'),
    read_term_from_atom(QueryString, Query, [variables([X])]),
    call(Query),
    write_term(X, [quoted(true)]).
remote_query(_, '/name') :-
    name(Name),
    reply_json_dict(Name).
remote_query(_, '/list_characters') :-
    findall(Char, saved_character(Char), Chars),
    (  name(Name)
    -> !, reply_json_dict(_{list: Chars, current: Name})
    ;  reply_json_dict(_{list: Chars})
    ).
remote_query(Request, '/new_character') :-
    http_parameters(Request, [name(Name,[])]),
    initialize_new_character(Name),
    reply_json_dict("success!").
remote_query(Request, '/load_character') :-
    http_parameters(Request, [name(Name,[])]),
    load_character_file(Name),
    reply_json_dict("success!").
remote_query(_, '/save_character') :-
    write_character_file,
    reply_json_dict("success!").
remote_query(_, '/ability_table') :-
    ability_table_json_dict(Dict),
    reply_json_dict(Dict).
remote_query(Request, '/set_base_abilities') :-
    member(search(Params), Request),
    forall((member(Abi=ScoreStr,Params),
            read_term_from_atom(ScoreStr,Score,[]),
            integer(Score)),
           update_base_ability(Abi, Score)),
    reply_json_dict("success!").
remote_query(Request, '/choice') :-
    http_parameters(Request, [source(SourceStr,[]),
                              id(IdStr,[]),
                              choice(ChoiceStr,[])]),
    term_string(Source, SourceStr),
    term_string(Id, IdStr),
    term_string(Choice, ChoiceStr),
    retractall(choice(Source, Id, _)),
    assert(choice(Source, Id, Choice)),
    resolve_not_eligible,
    write_character_file,
    reply_json_dict("success!").
remote_query(_, '/sheet') :-
    sheet_json_dict(Dict),
    reply_json_dict(Dict).
remote_query(_, '/options') :-
    findall(J, options_json(_,_,J), Json),
    reply_json_dict(Json).
remote_query(Request, '/gain_level') :-
    http_parameters(Request, [class(Class,[])]),
    level(CurLevel),
    NewLevel is CurLevel + 1,
    assert(gain_level(NewLevel, Class, hp_avg)), % TODO: option to roll for HP/manually specify
    write_character_file,
    reply_json_dict("success!").
remote_query(_, '/cur_level') :-
    level(Level),
    reply_json_dict(Level).
remote_query(_, '/list_class_options') :- % TODO: filter for available class options
    findall(O, class_option(O), Options),
    reply_json_dict(Options).
    
quoted_term_string(T, S) :-
    term_string(T, S, [quoted(true)]).

term_to_json(Atomic, Atomic) :- atomic(Atomic), !.
term_to_json(List, Json) :- maplist(term_to_json, List, Json), !.
term_to_json(Compound, _{functor: Functor, args: ArgsJson}) :-
    Compound =.. [Functor|Args],
    maplist(term_to_json, Args, ArgsJson).
