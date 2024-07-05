race_option(dragonborn).
race_shorthand(dragonborn, db).
racial_speed(dragonborn, 30).

traits_from_source(race(dragonborn), [language(common), language(draconic)]).

trait_options_source(race(dragonborn), 'draconic ancestry',
                     wrap(draconic_ancestry), dragon_color).

trait(trait(draconic_ancestry(Color)),
      breath_weapon(in(AoE):saving_throw(dc(Abi,DC)):(damage(Element, N d 6) else half))) :-
    trait(draconic_ancestry(Color)),
    dragon_element(Color, Element),
    breath_weapon(Color, AoE, Abi),
    breath_weapon_damage_dice(N),
    breath_weapon_dc(DC).

% TODO: not sure if we should put this in the attack table
attack('breath weapon', self, saving_throw(DC, Abi), [damage(Element, N d 6)],
       [AoEFmt, "on save: half damage", "one per long rest"]) :-
    trait(trait(draconic_ancestry(Color)), breath_weapon(_)),
    dragon_element(Color, Element),
    breath_weapon(Color, AoE, Abi),
    breath_weapon_damage_dice(N),
    breath_weapon_dc(DC),
    fmt(format_area(AoE), AoEFmt).

resource('breath weapon', 'breath weapon', 1) :-
    trait(breath_weapon(_)).
on_rest(long, 'breath weapon', 'full restore').

trait(trait(draconic_ancestry(Color)), resistance(Element)) :-
    trait(draconic_ancestry(Color)),
    dragon_element(Color, Element).

breath_weapon(black, 5 by 30 ft line, dex).
breath_weapon(blue, 5 by 30 ft line, dex).
breath_weapon(brass, 5 by 30 ft line, dex).
breath_weapon(bronze, 5 by 30 ft line, dex).
breath_weapon(copper, 5 by 30 ft line, dex).
breath_weapon(gold, 15 ft cone, dex).
breath_weapon(green, 15 ft cone, con).
breath_weapon(red, 15 ft cone, dex).
breath_weapon(silver, 15 ft cone, con).
breath_weapon(white, 15 ft cone, con).

breath_weapon_dc(DC) :-
    add_ability_mod_and_profbon(8, con, DC).
    
breath_weapon_damage_dice(N) :-
    level(Level),
    ordered_lookup_largest_leq([1 -> 2, 6 -> 3, 11 -> 4, 16 -> 5], Level, N).

custom_format(breath_weapon(Effect)) -->
    ["breath weapon ("],
    format_effect(Effect),
    [")"].
