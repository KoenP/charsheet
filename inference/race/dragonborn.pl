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
    breath_weapon_aoe(Color, AoE),
    breath_weapon_abi(Color, Abi),
    level(Level),
    ordered_lookup_largest_leq(Level, [1 -> 2, 6 -> 3, 11 -> 4, 16 -> 5], N),
    add_ability_mod_and_profbon(8, con, DC).
resource('breath weapon', 'breath weapon', 1) :-
    trait(breath_weapon(_)).
on_rest(long, 'breath weapon', full_restore).

trait(trait(draconic_ancestry(Color)), resistance(Element)) :-
    trait(draconic_ancestry(Color)),
    dragon_element(Color, Element).

