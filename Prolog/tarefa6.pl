v(v1).
v(v2).
v(v3).
v(v4).
v(v5).
v(v6).
v(v7).
a(v1,v2).
a(v2,v3).
a(v3,v4).
a(v3,v5).
a(v6,v7).

conectado(X,Y) :- a(X,Y).
conectado(X,Y) :- a(X,Z), conectado(Z,Y).