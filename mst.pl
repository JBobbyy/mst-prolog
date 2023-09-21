%%%% -*- Mode: Prolog -*-

%%%% mst.pl

:- dynamic
    graph/1.

:- dynamic
    vertex/2.

:- dynamic
    arc/4.

:- dynamic
    heap/2.

:- dynamic
    heap_entry/4.

:- dynamic
    vertex_key/3.

:- dynamic
    vertex_previous/3.


new_graph(G) :-
    nonvar(G),
    graph(G),
    !.

new_graph(G) :-
    nonvar(G),
    assert(graph(G)),
    !.


delete_graph(G) :-
    nonvar(G),
    graph(G),
    retract(graph(G)),
    retract(vertex(G, _)),
    retract(arc(G, _, _, _)).


new_vertex(G, V) :-
    vertex(G, V),
    !.

new_vertex(G, V) :-
    graph(G),
    assert(vertex(G, V)),
    !.


graph_vertices(G, Vs) :-
    graph(G),
    findall(V, vertex(G, V), Vs).


list_vertices(G) :-
    nonvar(G),
    graph(G),
    listing(vertex(G, _)).


new_arc(G, U, V, Weight) :-
    graph(G),
    vertex(G, U),
    vertex(G, V),
    arc(G, U, V, Weight),
    !.

new_arc(G, U, V, _W) :-
    arc(G, U, V, _W),
    retract(arc(G, U, V, _W)),
    retract(arc(G, V, U, _W)).

new_arc(G, U, V, Weight) :-
    graph(G),
    vertex(G, U),
    vertex(G, V),
    assert(arc(G, U, V, Weight)),
    assert(arc(G, V, U, Weight)).


graph_arcs(G, Es) :-
    graph(G),
    findall(arc(G, U, V, W), arc(G, U, V, W), Es).



vertex_neighbors(G, V, Ns) :-
    graph(G),
    vertex(G, V),
    findall(arc(G, V, N, W), arc(G, V, N, W), Ns).



adjs(G, V, Vs) :-
    graph(G),
    vertex(G, V),
    findall(vertex(G, A), arc(G, V, A, _W), Vs).


list_arcs(G) :-
    graph(G),
    listing(arc(G, _U, _V, _W)).


list_graph(G) :-
    graph(G),
    list_vertices(G),
    list_arcs(G).


read_graph(G, FileName) :-
    graph(G),
    csv_read_file(FileName, N),
    read_graph_assert(G, N).

read_graph_assert(_G, []) :- !.


read_graph_assert(G, [N | Ns]) :-
    N =.. L,
    nth0(1, L, V),
    nth0(2, L, U),
    last(L, W),
    new_arc(G, V, U, W),
    read_graph_assert(G, Ns).

list_arcs_write(G, Es) :-
    graph(G),
    findall(arc(U, V, W), arc(G, U, V, W), Es).

write_graph(G, FileName) :-
    write_graph(G, FileName, graph).


write_graph(G, FileName, Type) :-
    ==(Type, graph),
    list_arcs_write(G, Ls),
    csv_write_file(FileName, Ls).

write_graph(G, FileName, Type) :-
    ==(Type, edges),
    csv_write_file(FileName, G).






initialize_vertex_key(G) :-
    graph_vertices(G, [V | Vs]),
    assert(vertex_key(G, V, inf)),
    initialize_vertex_key(G, Vs).

initialize_vertex_key(_G, []) :-
    !.

initialize_vertex_key(G, [V | Vs]) :-
    assert(vertex_key(G, V, inf)),
    initialize_vertex_key(G, Vs).


initialize_vertex_previous(G) :-
    graph_vertices(G, [V | Vs]),
    assert(vertex_previous(G, V, nil)),
    initialize_vertex_previous(G, Vs).

initialize_vertex_previous(_G, []) :-
    !.

initialize_vertex_previous(G, [V | Vs]) :-
    assert(vertex_previous(G, V, nil)),
    initialize_vertex_previous(G, Vs).


modify_vertex_key(G, V, K) :-
    retract(vertex_key(G, V, _)),
    assert(vertex_key(G, V, K)).

modify_vertex_previous(G, V, U) :-
    retract(vertex_previous(G, V, _)),
    assert(vertex_previous(G, V, U)).


min_weight_arc(G, H) :-
    (   heap_empty(H) ->
    true
    ;   heap_extract(H, _Key, U),
        vertex_neighbors(G, U, Ns),
        min_weight_arc_extended(G, H, U, Ns),
        min_weight_arc(G, H)
    ).

min_weight_arc_extended(_G, _H, _U, []) :- !.

min_weight_arc_extended(G, H, U, [N | Ns]) :-
    N =.. L,
    nth0(3, L, V),
    nth0(4, L, W),
    (   heap_entry(H, _, K, V), W < K ->
    modify_vertex_previous(G, V, U),
        modify_vertex_key(G, V, W),
        modify_key(H, W, _OldKey, V)
    ;   true
    ),
    min_weight_arc_extended(G, H, U, Ns).


minheap_vertex_insert(_G, _H, []) :- !.

minheap_vertex_insert(G, H, [N | Ns]) :-
    vertex_key(G, N, K),
    heap_insert(H, K, N),
    minheap_vertex_insert(G, H, Ns).


mst_prim_support(G, H) :-
    new_heap(H),
    graph_vertices(G, Vs),
    minheap_vertex_insert(G, H, Vs),
    min_weight_arc(G, H).



mst_prim(G, Source) :-
    graph(G),
    initialize_vertex_key(G),
    initialize_vertex_previous(G),
    modify_vertex_key(G, Source, 0),
    mst_prim_support(G, hp).


mst_get(G, Source, PreorderTree) :-
    graph(G),
    build_list(G, Source, PreorderTree).


build_list(G, U, Ls) :-
    findall(arc(G, U, V, W), (vertex_previous(G, V, U),
			      vertex_key(G, V, W), U\=nil), Ls).




new_heap(H) :-
    heap(H, _S),
    !.

new_heap(H) :-
    assert(heap(H, 0)),
    !.


delete_heap(H) :-
    heap(H, _S),
    retract(heap(H, _S)),
    retract(heap_entry(H, _P, _K, _V)).


heap_has_size(H, S) :-
    heap(H, S).


increase_heap_size(H, S) :-
    NewSize is S+1,
    retract(heap(H, S)),
    assert(heap(H, NewSize)).

decrease_heap_size(H, S) :-
    NewSize is S-1,
    retract(heap(H, S)),
    assert(heap(H, NewSize)).


heap_empty(H) :-
    heap_has_size(H, 0).


heap_not_empty(H) :-
    heap_has_size(H, S),
    S > 0.


new_heap_entry(H, _P, K, _V) :-
    heap(H, _S),
    heap_entry(H, _P, K, _V),
    !.

new_heap_entry(H, P, K, V) :-
    heap(H, _S),
    assert(heap_entry(H, P, K, V)),
    !.


heap_head(H, K, V) :-
    heap_entry(H, 0, K, V).


heap_insert(H, K, V) :-
    nonvar(K),
    nonvar(V),
    heap_has_size(H, S),
    new_heap_entry(H, S, K, V),
    increase_heap_size(H, S),
    heap_property_insert(H, K, V).

heap_property_insert(H, K, Vs) :-
    heap_has_size(H, S),
    IndexEntry is S-1,
    odd(IndexEntry),
    Parent is div(IndexEntry, 2),
    (   heap_entry(H, Parent, Kp, Vp), K < Kp ->
    heap_exchange(H, Parent, IndexEntry, Kp, K, Vp, Vs),
        heap_property_insert_extended(H, Parent, K, Vs)
    ;   true
    ),
    !.

heap_property_insert(H, K, Vs) :-
    heap_has_size(H, S),
    IndexEntry is S-1,
    even(IndexEntry),
    Parent is div(IndexEntry, 2)-1,
    (   heap_entry(H, Parent, Kp, Vp), K < Kp ->
    heap_exchange(H, Parent, IndexEntry, Kp, K, Vp, Vs),
        heap_property_insert_extended(H, Parent, K, Vs)
    ;   true
    ).

heap_property_insert_extended(H, P, K, Vs) :-
    odd(P),
    IndexEntry is div(P, 2),
    (   heap_entry(H, IndexEntry, Kp, Vp), K < Kp ->
    heap_exchange(H, IndexEntry, P, Kp, K, Vp, Vs),
        heap_property_insert_extended(H, IndexEntry, K, Vs)
    ;   true
    ),
    !.

heap_property_insert_extended(H, P, K, Vs) :-
    even(P),
    IndexEntry is div(P, 2)-1,
    (    heap_entry(H, IndexEntry, Kp, Vp), K < Kp ->
    heap_exchange(H, IndexEntry, P, Kp, K, Vp, Vs),
         heap_property_insert_extended(H, IndexEntry, K, Vs)
    ;   true
    ).


heap_exchange(H, PosParent, PosChild, KParent, KChild, VParent,
	      VChild) :-
    retract(heap_entry(H, PosParent, KParent, VParent)),
    new_heap_entry(H, PosParent, KChild, VChild),
    retract(heap_entry(H, PosChild, KChild, VChild)),
    new_heap_entry(H, PosChild, KParent, VParent).

list_heap(H) :-
    heap(H, _S),
    listing(heap_entry(H, _P, _K, _V)).


even(N):-
	A is mod(N,2),
	A = 0.
odd(N):-
	A is mod(N,2),
	A = 1.

heap_extract(H, K, V) :-
    heap_has_size(H, S),
    IndexLastEntry is S-1,
    (   IndexLastEntry < 0 ->
    true
    ;   IndexLastEntry =:= 0 ->
    decrease_heap_size(H, S),
        heap_entry(H, 0, K, V)
    ;   heap_entry(H, 0, K, V),
        heap_entry(H, IndexLastEntry, Kmax, Vmax),
        heap_exchange(H, 0, IndexLastEntry, K, Kmax, V, Vmax),
        decrease_heap_size(H, S),
        retract(heap_entry(H, IndexLastEntry, K, V)),
        heap_property_extract(H, 0)
    ).


heap_property_extract(H, P) :-
    heap_entry(H, P, K, V),
    ChildSX is (P*2)+1,
    ChildDX is ChildSX+1,
    (   heap_entry(H, ChildSX, Ks, Vs), heap_entry(H, ChildDX, Kd, Vd)
	, K > Ks,
        Ks < Kd ->
    heap_exchange(H, P, ChildSX, K, Ks, V, Vs),
        heap_property_extract(H, ChildSX)
    ;   heap_entry(H, ChildSX, Ks, Vs), heap_entry(H, ChildDX, Kd, Vd)
	, K > Ks,
        Ks > Kd ->
    heap_exchange(H, P, ChildDX, K, Kd, V, Vd),
        heap_property_extract(H, ChildDX)
    ;   heap_entry(H, ChildSX, Ks, Vs), K > Ks ->
    heap_exchange(H, P, ChildSX, K, Ks, V, Vs),
        heap_property_extract(H, ChildSX)
    ),
    !.

heap_property_extract(H, P) :-
    heap_entry(H, P, K, V),
    ChildDX is (P*2)+2,
    (    heap_entry(H, ChildDX, Kd, Vd), K > Kd ->
    heap_exchange(H, P, ChildDX, K, Kd, V, Vd),
         heap_property_extract(H, ChildDX)
    ;   true
    ).

modify_key(H, NewKey, _OldKey, V) :-
    nonvar(NewKey),
    nonvar(V),
    heap_entry(H, P, _OldKey, V),
    retract(heap_entry(H, P, _, V)),
    assert(heap_entry(H, P, NewKey, V)),
    heap_property_insert_extended(H, P, NewKey, V).

%%%% end of file -- mst.pl
