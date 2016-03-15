(*
  mathematica-graph-utils/graph_utils.m
  Oliver Michel < oliver dot michel at colorado dot edu >
  University of Colorado Boulder
*)

(*
  set vertex weights of graph g to values in vector w
  w must be of length n for n vertices in g
*)
SetVertexWeights[g_, w_] :=
  Module[{i, gm = g},
    For[i = 1, i <= VertexCount[g], i++,
      gm = SetProperty[{gm, VertexList[g][[i]]}, VertexWeight -> w[[i]]];
    ];
    Return[gm];
  ];

(*
  set edge weights of graph g to values in vector w
  w must be of length m for m edges in g
*)
SetEdgeWeights[g_, w_] :=
  Module[{i, gm = g},
    For[i = 1, i <= EdgeCount[g], i++,
      gm = SetProperty[{gm, EdgeList[g][[i]]}, EdgeWeight -> w[[i]]];
    ];
    Return[gm];
  ];

(*
  returns a vector of length n containing vertex weights for graph g with n vertices
  unset vertex weight values return as $Failed
*)
VertexWeights[g_] :=
  Array[PropertyValue[{g, VertexList[[#]]}, VertexWeight] &, VertexCount[g]];

(*
  returns a vector of length m containing edge weights for graph g with m edges
  unset edge weight values return as $Failed
*)

EdgeWeights[g_] :=
  Array[PropertyValue[{g, EdgeList[g][[#]]}, EdgeWeight] &, EdgeCount[g]];

(*
  extracts the largest connected component from graph g
  vertex numbers may not be continuous anymore after this operation
*)

GiantComponent[g_] :=
  Subgraph[g, First[ConnectedComponents[g]]];

(*
  realigns the vertex numbering to 1..n for n vertices and keeps the edge list
  consistent with the new vertex numbering (may be used after GiantComponent[g])
*)

RealignGraph[g_] :=
  Module[{v = VertexList[g], e = EdgeList[g], i = Range[1, VertexCount[g]], r},
    r = MapIndexed[#1 -> First[i[[#2]]] &, Sort@v];
    Return[Graph[e /. r]];
  ];
