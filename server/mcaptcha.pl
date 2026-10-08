:- module(mcaptcha, [captcha_configured/0,
                     captcha_required/0,
                     captcha_widget_link/1,
                     verify_captcha_token/1]).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).

%!  captcha_configured
%
%   Semidet: all three MCAPTCHA_* environment variables are set to non-empty
%   values.
captcha_configured :-
    getenv('MCAPTCHA_URL', Url), Url \== '',
    getenv('MCAPTCHA_SITEKEY', Sitekey), Sitekey \== '',
    getenv('MCAPTCHA_SECRET', Secret), Secret \== ''.

%!  captcha_required
%
%   Semidet: captcha solving is required. It must be configured and not
%   explicitly disabled through CHARSHEET_REQUIRE_CAPTCHA.
captcha_required :-
    captcha_configured,
    \+ captcha_disabled.

captcha_disabled :-
    getenv('CHARSHEET_REQUIRE_CAPTCHA', Val),
    member(Val, [false, 'false', '0', no, 'no']).

%!  captcha_widget_link(-Link)
%
%   Produce the widget link used by the mCaptcha glue script.
captcha_widget_link(Link) :-
    captcha_configured,
    getenv('MCAPTCHA_URL', Base0),
    normalize_mcaptcha_url(Base0, Base),
    getenv('MCAPTCHA_SITEKEY', Sitekey),
    format(string(Link), '~w/widget?sitekey=~w', [Base, Sitekey]).

normalize_mcaptcha_url(Base0, Base) :-
    (   atom_concat(Base, '/', Base0)
    ->  true
    ;   Base = Base0
    ).

%!  verify_captcha_token(+Token)
%
%   Semidet: verify a captcha token with the mCaptcha instance. Fails closed:
%   empty tokens, missing configuration, network errors, non-200 responses,
%   unparseable bodies, or a false valid field all fail.
verify_captcha_token(Token) :-
    Token \== '',
    captcha_configured,
    !,
    catch(verify_captcha_token_safe(Token), _, fail).
verify_captcha_token(_) :- fail.

verify_captcha_token_safe(Token) :-
    getenv('MCAPTCHA_URL', Base0),
    normalize_mcaptcha_url(Base0, Base),
    getenv('MCAPTCHA_SITEKEY', Sitekey),
    getenv('MCAPTCHA_SECRET', Secret),
    format(string(Url), '~w/api/v1/pow/siteverify', [Base]),
    Payload = _{secret: Secret, key: Sitekey, token: Token},
    % json_object(dict) is required: without it http_post/4 parses the reply with
    % json_read/2 and a JSON boolean arrives as the term @(true), which no
    % comparison against the atom `true` will ever match.
    http_post(Url, json(Payload), Reply, [json_object(dict), timeout(10)]),
    reply_valid(Reply).

reply_valid(Reply) :-
    is_dict(Reply),
    !,
    get_dict(valid, Reply, true).
reply_valid(Reply) :-
    Reply = json(Pairs),
    !,
    member(valid=V, Pairs),
    json_true(V).
reply_valid(_) :- fail.

% JSON true, both in the dict representation (atom true) and the term
% representation (@(true)).
json_true(V) :- V == true, !.
json_true(V) :- V =.. [F, true], F == '@'.
