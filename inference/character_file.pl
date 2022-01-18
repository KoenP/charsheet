:- multifile
       name/1,
       base_ability/2,
       gain_level/3,
       choice/3.

:- dynamic
       name/1,
       base_ability/2,
       gain_level/3,
       choice/3.

character_definition_predicates(
    [ name/1         ,
      base_ability/2 ,
      gain_level/3   ,
      choice/3       ]).

%! write_character_file
%
%  Writes a file containing the facts that define the current
%  character to a file. The file is placed in the `characters` folder
%  and the file name is the character's name, followed by the `.pl`
%  file extension.
write_character_file :-
    name(Name),
    charname_to_filename(Name, FileName),
    open(FileName, write, Out),
    character_definition_predicates(Preds),
    write_predicates(Out, Preds),
    close(Out).

%! write_predicates(+Stream, +List)
%
%  For each `Predicate/Arity` term in List, write all results to the
%  given output stream.
write_predicates(Out, List) :-
    forall(member(Pred,List), write_predicate(Out, Pred)).
write_predicate(Out, Pred/Arity) :-
    length(Args, Arity),
    Goal =.. [Pred|Args],
    forall(Goal,
           write_term(Out, Goal,
                      [quoted(true), fullstop(true), nl(true)])).


%! load_character_file(+CharName)
%
%  Load the facts defining the character CharName from its
%  file. CharName should only be the character name, the corresponding
%  file will be found by looking for the matching `.pl` file in the
%  `characters` folder.
load_character_file(CharName) :-
    unload_current_character,
    charname_to_filename(CharName, FileName),
    load_files(FileName, []).

%! unload_current_character
%
%  Forget everything about the current character.
%  See character_definition_predicates/1 for a list of predicates
%  which will be fully retracted.
unload_current_character :-
    character_definition_predicates(Preds),
    forall(member(Pred,Preds), retractall_pred(Pred)).

initialize_new_character(Name) :-
    unload_current_character,
    assert(name(Name)),
    forall(ability(Abi), assert(base_ability(Abi,10))),
    write_character_file.

retractall_pred(Pred/Arity) :-
    length(Args, Arity),
    Goal =.. [Pred|Args],
    retractall(Goal).

%! saved_character(CharName)
%
%  True iff CharName matches a character file in the `characters`
%  directory.
saved_character(CharName) :-
    directory_member(characters, FileName, [extensions([pl])]),
    charname_to_filename(CharName, FileName).

charname_to_filename(CharName, FileName) :-
    ground(CharName),
    !,
    atom_chars(CharName, CharNameChars),
    cn2fn(CharNameChars, FileNameChars, []),
    atom_chars(FileName, FileNameChars).
charname_to_filename(CharName, FileName) :-
    ground(FileName),
    !,
    atom_chars(FileName, FileNameChars),
    cn2fn(CharNameChars, FileNameChars, []),
    atom_chars(CharName, CharNameChars).
    %atomic_list_concat(['characters/', CharName, '.pl'], FileName).

cn2fn(CharName) --> seq_atom('characters/'),
                    seq(CharName),
                    seq_atom('.pl').
