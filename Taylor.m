(* -*- mode: math; tab-width: 3; -*- *)

BeginPackage["Taylor`"]

Unprotect[TotalDegree, Taylor, TaylorCoeff, InitialForm, Grading, TaylorCompress]

TotalDegree::usage = "TotalDegree[expr, vars] gives the total degree\
	 of the polynomial expression expr in the variables vars.\
	 TotalDegree[expr] gives the total degree of the polynomial expr in\
	 its variables."

Taylor::usage = "Taylor[expr, vars, n] gives the Taylor polynomial\
	 of expr in the variables vars to order n."

Options[Taylor] = {Grading -> 1}

TaylorCoeff::usage = "TaylorCoeff[expr, vars, m] gives the\
	 coefficient of expr in variables vars of degree m\
	 (m is a multi-index)."

InitialForm::usage = "InitialForm[expr, vars] is the homogeneous\
	 form of expr in vars of lowest total degree."

Grading::usage = "Grading is an option for Taylor, TotalDegree,\
	 and InitialForm."

TaylorCompress::usage = "TaylorCompress[expr, vars, n, a] gives the\
	Taylor polynomial of expr in the variables vars to order n in terms\
	of unspecified coefficients with head a.  a[i,j,k,...] is the\
	coefficient of Inner[Power,vars,{j,k,...},Times] in the Taylor\
	polynomial of expr[[i]]."

Taylor::badgr = "`1` does not grade the monomials in variables `2`."

Begin["Private`"]

TotalDegree[f_, x_List] := 
	Max[Append[Sum[Exponent[f, x[[k]], List], {k,1,Length[x]}],0]]

TotalDegree[f_] := TotalDegree[f,Variables[f]]

TotalDegree[f_, x_] := Exponent[f,x,Max]

TotalDegree[f_, x_, opts__] := Module[
	{eps,grade},
	grade = Grading /. {opts} /. Options[Taylor];
	If[ (Length[grade] > 1 && Length[grade] != Length[x]),
		Message[Taylor::badgr, grade, x];
		Return[]
	];
	TotalDegree[f /. Thread[x -> (eps^grade) x], eps]
]

Taylor[f_, x_List, n_Integer] := Module[{eps},
	Expand[
		Normal[
			Series[
				(f /. Thread[x -> eps x]), 
				{eps, 0, n}
			]
		] /. eps -> 1
	]
]

Taylor[f_, x_Symbol, n_Integer] := Taylor[f,{x},n]

Taylor[f_, x_, n_Integer, opts__] := Module[
	{eps,grade},
	grade = Grading /. {opts} /. Options[Taylor];
	If[ (Length[grade] > 1 && Length[grade] != Length[x]),
		Message[Taylor::badgr, grade, x];
		Return[]
	];
	Expand[
		Normal[
			Series[
				(f /. Thread[x -> (eps^grade) x]), 
				{eps, 0, n}
			]
		] /. eps -> 1
	]
]

TaylorCoeff[expr_, {x_}, {n_}] := Coefficient[expr, x, n]

TaylorCoeff[expr_, {x_, y___}, {m_, n___}] := 
	Coefficient[TaylorCoeff[expr, {y}, {n}], x, m]

TaylorCoeff[e_, x_, n_Integer] := TaylorCoeff[e, {x}, {n}]

InitialForm[e_List, x_] := InitialForm[#,x]& /@ e

InitialForm[e_, x_] := Taylor[
	e, x, -TotalDegree[e,x,Grading -> -1]
]

InitialForm[e_, x_, opts__] := Module[
	{grade,n},
	grade = Grading /. {opts} /. Options[Taylor];
	If[ (Length[grade] > 1 && Length[grade] != Length[x]),
		Message[Taylor::badgr, grade, x];
		Return[]
	];
	n = -TotalDegree[e,x,Grading -> -grade];
	Taylor[e, x, n, Grading -> grade]
]

TaylorCompress[e_List, x_List, n_Integer, a_, opts___] := Module[
	{f, g, arules = {}, mi, m = Length[e], s, t},
	f = Taylor[e, x, n, opts];
	g = Array[0&, m];
	For[j = 0, j <= n, j++,
		mi = multiIndices[j, Length[x]];
		For[k = 1, k <= Length[mi], k++,
			t = Together[TaylorCoeff[f, x, mi[[k]]]];
			For[i = 1, i <= m, i++,
				If[
					t[[i]] =!= 0,
					s = a[i,mi[[k]]];
					g[[i]] += s Inner[Power, x, mi[[k]], Times];
					arules = Append[arules, s -> t[[i]]],
					arules = Append[arules, s -> 0]
				]
			]
		]
	];
	{g, arules}
]
		
TaylorCompress[e_, x_List, n_Integer, a_, opts___] := Module[
	{P = TaylorCompress[{e}, x, n, a, opts]},
	{P[[1,1]], P[[2]]}
]

multiIndices[order_Integer, 1] := {{order}}

multiIndices[0, n_Integer] := {Array[0&, n]}

multiIndices[order_Integer/;(order > 0), n_Integer/;(n > 1)] := Apply[
	Join,
	Table[
		Append[#,i]& /@ multiIndices[order-i, n-1],
		{i,0,order}
	]
]

End[ ]

Protect[TotalDegree, Taylor, TaylorCoeff, InitialForm, Grading, TaylorCompress]

EndPackage[ ]
