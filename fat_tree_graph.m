(*
  mathematica-graph-utils/fat_tree_graph.m
  Oliver Michel < oliver dot michel at colorado dot edu >
  University of Colorado Boulder
*)

(*
  constructs a fat-tree graph with n cores, and m leaf nodes per edge
*)
FatTreeGraph[n_, m_, p_, b_] := Module[
   {c, a, a1, a2, e, e1, s, s1, h, ca, ae, es, sh, edgeWeights, 
    vertexWeights, G},
   
   c = Partition[Range[1, n], n/2];
   
   a = Range[n + 1, 3 n];
   
   a1 = {a[[1 ;; 2 n - 1 ;; 2]], a[[2 ;; 2 n ;; 2]]};
   a2 = Partition[a, 2];
   
   e = Range[3 n + 1, 5 n];
   e1 = Partition[e, 2];
   
   s = Range[5 n + 1, 5 n + 2 n*m];
   s1 = Partition[s, m];
   
   h = Range[5 n + 2 n*m + 1, 5 n + 4 n*m];
   
   ca = #[[1]] <-> #[[2]] & /@ 
     Join[Tuples[{c[[1]], a1[[1]]}], Tuples[{c[[2]], a1[[2]]}]];
   ae = Flatten[{#[[1, 1]] <-> #[[2, 1]], #[[1, 2]] <-> #[[2, 2]], #[[
          1, 1]] <-> #[[2, 2]], #[[1, 2]] <-> #[[2, 1]]} &
      /@ MapIndexed[{#1, First[e1[[#2]]]} &, a2]];
   es = Table[(#[[1]] <-> #[[2]] & /@ 
        Tuples[{{e[[i]]}, s1[[i]]}]), {i, 2 n}] // Flatten;
   sh = Table[s[[i]] <-> h[[i]], {i, 2 n*m}];
   
   vertexWeights = Join[
     ConstantArray[{n*m*b, 0}, Length[c]],
     ConstantArray[{m*b, 0}, Length[a] + Length[e]],
     ConstantArray[{b, 0}, Length[s]],
     ConstantArray[{0, p}, Length[h]]
     ];
   
   edgeWeights = Join[
     ConstantArray[m*b, Length[ca]],
     ConstantArray[(m/2)*b, Length[ae]],
     ConstantArray[b, Length[es] + Length[sh]]
     ];
   
   G = Graph[Join[ca, ae, es, sh]];
   
   Return[{G, vertexWeights, edgeWeights}];
   ];