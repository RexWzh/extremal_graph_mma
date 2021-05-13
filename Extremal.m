(* ::Package:: *)

(* ::Section:: *)
(*Functions about extremal graphs*)


BeginPackage["Extremal`"];
Print["Loading functions about extremal graphs..."];


(* ::Subsubsection:: *)
(*Main function*)


SimpleGraphs;
(*IGSubisomorphicQ;*)
SubgraphQ;
GraphF;
GraphK3;
ExtremalGraphs;


Begin["Private`"];


(* ::Subsubsection:: *)
(*functions*)


(*Import functions*)
Needs["IGraphM`"];
SubgraphQ=IGSubisomorphicQ;


(*simple graphs data*)
SimpleGraphs[n_]:=Import["~/desktop/work_space/1 MMA/0 pkg/simplegraphs/graph"<>ToString@n<>".g6"];
(*get part of the simple graphs*)
SimpleGraphs[n_,a_,b_]:=Import["~/desktop/work_space/1 MMA/0 pkg/simplegraphs/graph"<>ToString@n<>".g6",{"GraphList",Range[a,b]}];

(*Graph Subscript[F, k] of 2k+1 vertices (k\[GreaterEqual]1) *)
GraphF[k_?Positive]:=Module[{vertices1,vertices2,edges},
vertices1=Range@k;
vertices2=Range[-1,-k,-1];
edges=(0<->#&)/@Join[vertices1,vertices2];
edges=Join[edges,Thread[TwoWayRule[Range@k,Range[-1,-k,-1]]]];
Graph@edges];

(*Disjoint union of triangules*)
GraphK3[k_?Positive]:=GraphDisjointUnion@@ConstantArray[GraphF@1,k];


(*search for graphs in "graphs"*)
ExtremalGraphs[graphs_,forbidden_,edge_:0]:=Module[{selected,maxedge},
selected=Select[graphs,EdgeCount@#>=edge&&!IGSubisomorphicQ[forbidden,#]&];
If[Length@selected==0,Return@{{},edge}];
maxedge=EdgeCount/@selected//Max;
{Select[selected,EdgeCount@#==maxedge&],maxedge}]


End[];
EndPackage[];
