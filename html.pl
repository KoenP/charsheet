:- use_module(library(http/html_write)).

out :-
    warn_if_problems,
    html_write:html_set_options([doctype(html)]),
    char_sheet_html(Html),
    !,
    out_file_name(FileName, []),
    open(FileName, write, Stream),
    html_write:page(Html, Tokens, []),
    html_write:print_html(Stream, Tokens),
    close(Stream).
out_file_name --> {name(CharName)}, seq_atom(CharName), ".html".


char_sheet_html([head(Head), body(Body)]) :-
    char_sheet_head(Head),
    char_sheet_body(Body).

char_sheet_head([title(Name), link([rel=stylesheet, href='charsheet.css'], [])]) :-
    name(Name).

char_sheet_body([Div]) :-
    name(CharName),
    body_contents(Contents),
    Div = div(class(container),
              [header(h1(CharName)), article(Contents)]).

body_contents([Summary]) :-
    character_summary(Summary).

character_summary(Div) :-
    Div = div([table( [id=summary, style='padding: 4px'] ,
                      [ tr([th("Race"), td(Race)])
                      , tr([th("Class"), td(CLsFmt)])
                      , tr([th("Level"), td(Level)])
                      , tr([th("Max HP"), td(HP)])
                      , tr([th("AC"), td(AC)])
                      , tr([th("Initiative"), td(Init)])
                      , tr([th("Speed"), td(Speed)])
                      , tr([th("HD"), td(HD)])
                      , tr([th("PP"), td(PP)])
                      , tr([th("Prof Bon"), td(ProfBon)])
                      ])]),
    most_specific_race(Race),
    findall(CL, class_level(CL), CLs), format_list(CLs, CLsFmt, []),
    level(Level),
    HP = todo,
    ac(AC),
    Init = todo,
    Speed = todo,
    HD = todo,
    PP = todo,
    ProfBon = todo.
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

thtd(Header, DataRule, [tr([th(Header), td(X)])], Tail) :-
    call(DataRule, X, Tail).
    
wrapped(Goal, Rule, Result, Tail) :-
    call(Rule, X, Tail),
    call(Goal, X, Result).

pred(Pred, Result, _) :-
    call(Pred, Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%format_term(Custom) --> custom_format(Custom), !.
format_term(Compound) -->
    {\+ is_list(Compound), Compound =.. [Ftor|Tms], \+ member(Ftor, ['/', ':', 'd', '+']), Tms \= []},
    format_atom(Ftor),
    [' ('],
    format_terms(Tms),
    [')'].
format_term(X d Y) --> format_dice(X d Y).
format_term(X / Y) --> [X], ['/'], [Y].
format_term(X : Y) --> format_term(X), [':'], format_term(Y).
format_term(X + Y) --> format_term(X), ['+'], format_term(Y).
format_term(Number) --> {number(Number), number_chars(Number, Chars)}, seq(Chars).
format_term(Atom) --> {atom(Atom)}, format_atom(Atom).
format_term(String) --> {string(String)}, [String].
format_term(List) --> format_list(List).

format_atom(Atom) -->
    {atom_chars(Atom, Chars)},
    us_to_space(Chars).

format_terms([]) --> [].
format_terms([X]) --> format_term(X).
format_terms([X|Xs]) --> {Xs \= []}, format_term(X), [', '], format_terms(Xs).

format_list([]) --> [].
format_list([X]) --> format_term(X).
format_list([X|Xs]) --> {Xs \= []}, format_term(X), [', '], format_list(Xs).

format_list_flat([]) --> [].
format_list_flat([X]) --> [X].
format_list_flat([X|Xs]) --> {Xs \= []}, [X], [', '], format_list_flat(Xs).

format_dice_sum(Ds + K) --> {number(K), K \= 0}, format_dice_sum(Ds), ['+'], [K].
format_dice_sum(Ds + 0) --> format_dice_sum(Ds).
format_dice_sum(Ds + D) --> format_dice_sum(Ds), ['+'], format_dice(D).
format_dice_sum(Ds) --> format_dice(Ds).
format_dice(N d X) --> seq([N, 'd', X]).

format_bonus(N) --> {N >= 0}, ['+'], [N].
format_bonus(N) --> {N < 0}, [N].

% Replace underscores by spaces.
us_to_space([ X |Xs]) --> {X \= '_'}, [X], us_to_space(Xs).
us_to_space(['_'|Xs]) --> [' '], us_to_space(Xs).
us_to_space([]) --> [].
    
interleave([X|Xs], [Y|Ys]) --> [X], [Y], interleave(Xs, Ys).
interleave(Xs, []) --> {Xs \= []}, seq(Xs).
interleave([], Ys) --> seq(Ys).

sep(_, []) --> [].
sep(_, [X]) --> [X].
sep(Sep, [X|Xs]) --> {Xs \= []}, [X], seq(Sep), sep(Sep, Xs).

rep(_) --> [].
rep(X) --> [X], rep(X).

seq([]) --> [].
seq([E|Es]) --> [E], seq(Es).

seq_atom(Atom) --> {atom_codes(Atom, Chars)}, seq(Chars).
    
maybe(_) --> [].
maybe(X) --> [X].
