(* -*- mode: math; tab-width: 3; -*- *)

BeginPackage["NewtonPolyhedron`", {"Taylor`", "DiscreteMath`Combinatorica`"}]

Unprotect[NewtonPolyhedron, Support]

Support::usage = "Support[f,x] gives the multi-indices of the monomials appearing\
	in f with nonzero coefficient."

NewtonPolyhedron::usage = "NewtonPolyhedron[f,x] gives a list {U,V}, where U\
	is a list of the vertices of the Newton polyhedron, and V is a list of vectors\
	forming the normal cone."

Begin["Private`"]

Support[f_, x_List] := Transpose[
	Flatten[Exponent[f,#,List]]& /@ x
]

NewtonPolyhedron[f_, x_List] := Module[
	{S = Union[Support[f,x]], n = Length[x], T, U = {}, V = {}, W},
	W = triangulations[S];
	Do[{T,q} = getpts[S,W[[k]]];
		If[ (q && (W[[k,2]] != 0)),
			U = Union[U,T];
			V = Append[V,W[[k,1]]]
		],
		{k,1,Length[W]}
	];
	{U,Union[V]}
]

triangperp[{v_List, w__List}] := Module[
	{u = Cross @@ ((# - v)& /@ {w}), a, b},
	If[ (b = GCD @@ u) == 0, Return[]];
	u /= b;
	a = u.v;
	If[a > 0, u *= -1; a *= -1];
	{u, a}
]

triangulations[X_/;MatrixQ[X]] := Select[
	triangperp /@ KSubsets[X, Length[X[[1]]]],
	(# =!= Null)&
]

applydual[{v_List, a_}, w_List] := v.w - a

getpts[S_List, w_] := Module[
	{T = ({#, applydual[w,#]})& /@ S, q},
	If[ q = And @@ ((#[[2]] <= 0)& /@ T),
		{#[[1]]& /@ Select[T, (#[[2]] == 0)&], q},
		{{},q}
	]
]

End[ ]

Protect[NewtonPolyhedron, Support]

EndPackage[ ]
