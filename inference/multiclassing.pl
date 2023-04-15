% Channel divinity has specific multiclassing rules.
resource('channel divinity', 'channel divinity', Uses) :-
    findall(N, bonus(channel_divinity_uses(N)), Ns),
    max_member(Uses, Ns).
on_rest(short, 'channel divinity', full_restore).
