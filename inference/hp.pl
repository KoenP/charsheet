
%! max_hp(?HP:int)
%
%  Your character's maximum hit points.
max_hp(HP) :-
    findall(Term, (hp_term_for_level(_, Term); bonus('max hp' + Term)), Terms),
    sumlist(Terms, HP).

%! initial_class_base_hp(?HP:int)
%
%  The base hit points your character gets from its initial class.
initial_class_base_hp(HP) :-
    initial_class(Class),
    initial_class_base_hp(Class, HP).

%! hp_term_for_level(?CharLevel:int, ?Term:int)
%
%  Term is the HP gained on level CharLevel.
hp_term_for_level(1, Term) :-
    initial_class_base_hp(InitHP),
    ability_mod(con, ConMod),
    Term is InitHP + ConMod.
hp_term_for_level(CharLevel, Term) :-
    gain_level(CharLevel, ClassName, HPMode),
    max_hp_per_level(ClassName, HPDie),
    hp_die(HPMode, HPDie, ClassHP),
    ability_mod(con, ConMod),
    Term is ClassHP + ConMod.

%! hp_die(?AvgOrRolled, ?Die, ?HP:int)
%
%  If `AvgOrRolled = hp_avg`, then HP is the roll_avg/2 of Die.
%  Otherwise, `AvgOrRolled = hp_rolled(HP)`.
hp_die(hp_avg, Die, HP) :-
    roll_avg(Die, HP).
hp_die(hp_rolled(HP), _, HP).
