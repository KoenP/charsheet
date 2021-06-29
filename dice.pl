:- op(500, xfx, d).

die_avg(X d Y, Avg) :- Avg is ceiling(X * (Y+1) / 2).
