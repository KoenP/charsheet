:- multifile weapon/5.
:- multifile has/1.
:- dynamic has/1.
:- dynamic weapons_equipped/1.
:- dynamic attuned/1.

weapons_equipped(_) :- false.

item_exists(Shield) :-
    is_shield(Shield).
item_exists(BodyArmor) :-
    body_armor(BodyArmor, _, _).
item_exists(Weapon) :-
    weapon_option(W, _, _, _, _),
    (Weapon = W ; Weapon = W + _).
    

has(Weapon) :-
    weapons_equipped(Weapons),
    member(Weapon, Weapons).

has_weapon(Weapon) :-
    has(Weapon),
    attack(Weapon, _, _, _, _).

weapon_index(Index, Weapon) :-
    number(Index),
    findall(W, has_weapon(W), Weapons),
    length(Prefix, Index),
    append(Prefix, [Weapon|_], Weapons).

attuned(_) :- false.

expand_to_sum(Item    , Item + 0) :- Item \= _+_.
expand_to_sum(Item + N, Item + N).

is_shield(shield).
is_shield(Shield + _) :- is_shield(Shield).
is_shield(ShieldF) :- ShieldF =.. [shield|_].

body_armor('studded leather', light, ac(12)).
body_armor('half plate', medium, ac(15)).
body_armor('chain mail', heavy, ac(16)).
body_armor(Armor+N, Weight, ac(AC)) :-
    body_armor(Armor, Weight, ac(BaseAC)),
    AC is BaseAC + N.

%! weapon(?Name, ?Category, ?Rangedness, ?DamageFormula, ?Notes)
%
%  The term "weapon" is used incorrectly here to include unarmed strikes.
weapon(club, simple, melee,
       [damage(bludgeoning, 1 d 4)], [light]).
weapon(quarterstaff, simple, melee,
       [damage(bludgeoning, 1 d 6)], [versatile(1 d 8)]).
weapon(mace, simple, melee,
       [damage(bludgeoning, 1 d 6)], []).
weapon(handaxe, simple, melee,
       [damage(slashing, 1 d 6)], [light, thrown(feet(20) / feet(60))]).

weapon(battleaxe, martial, melee,
       [damage(slashing, 1 d 8)], [versatile(1 d 10)]).
weapon(greataxe, martial, melee,
       [damage(slashing, 1 d 12)], [heavy, twohanded]).
weapon(longsword, martial, melee,
       [damage(slashing, 1 d 8)], [versatile(1 d 10)]).
weapon(rapier, martial, melee,
       [damage(piercing, 1 d 8)], [finesse]).

weapon('light crossbow', simple, ranged(feet(80) / feet(320)),
       [damage(piercing, 1 d 8)], [ammunition, loading, twohanded]).
weapon(longbow, martial, ranged(feet(150) / feet(600)),
       [damage(piercing, 1 d 8)], [ammunition, heavy, twohanded]).

weapon(javelin, simple, melee,
       [damage(piercing, 1 d 6)], [thrown(feet(30) / feet(120))]).

%! weapon_item(?Name, ?Category, ?Rangedness, ?DamageFormula, ?Notes)
%
%  Weapons that are also items (basically ignoring natural weapons).
weapon_option(Weapon, Category, Range, Damage, Notes) :-
    weapon(Weapon, Category, Range, Damage, Notes),
    Category \= natural.

% Possibilities
% "Melee weapons" can always melee and sometimes be thrown.
%   -> they always use STR (also when thrown), unless they are finesse weapons, in which case you can pick STR/DEX
% "Ranged weapons" cannot be used in melee
%   -> they always use DEX, unless they have the "thrown" property, in which case they use STR
%     -> UNLESS they also have finesse, in which case you can pick STR/DEX.

% A javelin is counted as a melee weapon with thrown property -> thrown simply modifies that it can also be used at range.
% A dart is counted as a ranged weapon with thrown property -> thrown here just indicates that it uses STR and not DEX (unless it's also finesse!).

weapon_onehanded(Weapon) :-
    weapon(Weapon, _, _, _, Notes),
    \+ member(twohanded, Notes).
weapon_melee(Weapon) :-
    weapon(Weapon, _, melee, _, _).
weapon_ranged(Weapon) :-
    weapon(Weapon, _, ranged(_), _, _).

%! weapon_variant(?NamedWeapon, ?BaseWeapon, ?ExtraRolls, ?ExtraNotes)
%  Some (magic) weapons have a specific name but
%  inherit their base stats from a regular weapon.
%  The notation for an enchantment bonus works on the BaseWeapon parameter, so
%  BaseWeapon can be `longsword + 2`, for example.
%  The variant weapon can add extra damage rolls and notes, but can't change
%  any other stats. If other stats need to be changed, it's no longer considered
%  a variant.
weapon_variant(berserker_axe(BaseWeapon), BaseWeapon + 1, [], [attunement, cursed]) :-
    member(BaseWeapon, [handaxe, battleaxe, greataxe]).
custom_format(berserker_axe(BaseWeapon)) -->
    ["berserker "], [BaseWeapon].
bonus_source(attuned(berserker_axe(_)), 'max hp' + Lvl) :-
    level(Lvl).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Equipment JSON 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
equipment_json_dict(_{ weapons: Weapons,
                       weapon_options: WeaponOptions
                     }) :-
    findall(Weapon, weapon_json_dict(Weapon), Weapons),
    findall(WeaponOption, weapon_option(WeaponOption,_,_,_,_), WeaponOptions).

weapon_json_dict(_{ base_weapon: BaseWeapon,
                    enchantment: Enchantment,
                    category: Category,
                    range: Range,
                    to_hit: ToHit,
                    damage: Damage,
                    notes: Notes
                 }) :-
    attack(WeaponVal, RangeVal, ToHitVal, DamageVal, NotesVal),
    destructure_weapon_or_variant(WeaponVal, BaseWeaponVal, Enchantment),
    weapon(BaseWeaponVal, Category, _, _, _),
    fmt(format_term(BaseWeaponVal), BaseWeapon),
    fmt(format_range(RangeVal), Range),
    fmt(format_to_hit_or_dc(ToHitVal), ToHit),
    fmt(format_damage(DamageVal), Damage),
    fmt(format_list(NotesVal), Notes).

destructure_weapon_or_variant(Variant : _, BaseWeapon, Enchantment) :-
    Variant \= _:_,
    destructure_weapon_or_variant(Variant, BaseWeapon, Enchantment).
destructure_weapon_or_variant(BaseWeapon + Enchantment, BaseWeapon, Enchantment) :-
    Enchantment \= 0.
destructure_weapon_or_variant(Weapon, Weapon, 0) :-
    atomic(Weapon).
