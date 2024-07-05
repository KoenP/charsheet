dragon_element(black, acid).
dragon_element(blue, lightning).
dragon_element(brass, fire).
dragon_element(bronze, lightning).
dragon_element(copper, acid).
dragon_element(gold, fire).
dragon_element(green, poison).
dragon_element(red, fire).
dragon_element(silver, cold).
dragon_element(white, cold).

dragon_color(Color) :- dragon_element(Color, _).
