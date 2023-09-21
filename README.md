# Graph Algorithms: Minimum Spanning Trees

The Minimum Spanning Tree is a tree, created through Prim's algorithm, that visits all nodes in a graph
through arcs whose sum of weights is the minimum possible.
To create a MST we need an undirected, connected graph and each arc must have a numerical weight. 

## Getting Started

The project has been implemented using SWI-Prolog.

Start SWI-Prolog  and choose the folder containing mst.pl.
Upload the file using the following query:

```prolog
?- consult('mst.pl').
```
At this point add a graph containing vertices and arcs to the system database.
For this step, you can read a .csv file like this:

```prolog
?- read_graph(G, FileName).
```

N.B.
The .csv file must contain a number of such arcs:

vertexA vertexB weight
vertexA vertexC weight
.
.
vertexY vertexZ weight


Or you can use the following predicates:

```prolog
?- new_graph(graphname).

?- new_vertex(graphname, vertexname).

?- new_arc(graphname, vertex, vertex, weight).
```

## Starting Prim algorithm

In order to get the MST of the corresponding graph you need to use the following predicates:

```prolog
?- mst_prim(graphname, startingvertex).
?- mst_get(graphname, startingvertex, tree).
```

where tree is the output of the algorithm.

## Built With

* [SWI-Prolog](https://www.swi-prolog.org/)
