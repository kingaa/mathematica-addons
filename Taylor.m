(* -*- mode: math; tab-width: 3; -*- *)

BeginPackage["Taylor`"]

Unprotect[TotalDegree, Taylor, TaylorCoeff, InitialForm, Grading, TaylorCompress, ImplicitSolve]

TotalDegree::usage = "TotalDegree[expr, vars] gives the total degree\
	 of the polynomial expression expr in the variables vars.\
	 TotalDegree[expr] gives the total degree of the polynomial expr in\
	 its variables."

Taylor::usage = "Taylor[expr, vars, n] gives the Taylor polynomial\
	 of expr in the variables vars to order n.  Taylor[expr, vars, p, n]\
	gives the Taylor polynomial of expr in the variables vars about the\
	point p to order n."

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
	of unspecified coefficients with head a.  a[i,{j,k,...}] is the\
	coefficient of Inner[Power,vars,{j,k,...},Times] in the Taylor\
	polynomial of expr[[i]]."

Taylor::badgr = "`1` does not grade the monomials in variables `2`."

Taylor::badp  = "Incommensurate dimensions in Taylor."

ImplicitSolve::usage = "Given an expression f(x,vars), where vars is a
list of variables, such that f(0,0,...,0) = 0, ImplicitSolve[f, x,
vars, n] uses the implicit function theorem to return the n-th Taylor
polynomial of the unique solution of f == 0.  Thus Taylor[f(x,y,z)
/. ImplicitSolve[f,x,{y,z},3], {y,z}, 3] == 0."

ImplicitSolve::sing = "Singular Jacobian in ImplicitSolve.  A unique
solution is not guaranteed to exist."

ImplicitSolve::nobranch = "No branch of the solution passes through
the origin."

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

Taylor[f_, x_List, p_List, n_Integer] := Module[{eps},
	Expand[
		Normal[
			Series[
				(f /. Thread[x -> p + eps (x - p)]), 
				{eps, 0, n}
			]
		] /. eps -> 1
	]
]

Taylor[f_, x_Symbol, n_Integer] := Taylor[f,{x},n]

Taylor[f_, x_Symbol, p_, n_Integer] := Taylor[f,{x},{p},n]

Taylor[f_, x_List, n_Integer, opts__] := Module[
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

Taylor[f_, x_List, p_List, n_Integer, opts__] := Module[
	{eps, grade},
	grade = Grading /. {opts} /. Options[Taylor];
	If[ (Length[x] != Length[p]),
		Message[Taylor::badp];
		Return[]
	];
	If[ (Length[grade] > 1 && Length[grade] != Length[x]),
		Message[Taylor::badgr, grade, x];
		Return[]
	];
	Expand[
		Normal[
			Series[
				(f /. Thread[x -> p + (eps^grade) (x - p)]), 
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
				If[ t[[i]] =!= 0,
					(	s = a[i,mi[[k]]];
						g[[i]] += s Inner[Power, x, mi[[k]], Times];
						arules = Append[arules, s -> t[[i]]];
					)
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

ImplicitSolve[f_, x_, vars_List, n_Integer] := Module[
	{m = Length[vars], eqn, g, clist, xvars}, 
	g = f /. x -> 0 /. Thread[vars -> 0];
	If[
		Not[ g == 0 ],
		Message[ImplicitSolve::nobranch];
		Return[$Failed]
	];
	If[
		Coefficient[f /. Thread[vars] -> 0, x, 1] == 0,
		Message[ImplicitSolve::sing];
		Return[$Failed]
	];
	eqn = Taylor[f /. x -> x @@ vars, vars, n] /. x @@ Array[0&, m] -> 0; 
	g = Taylor[x @@ vars, vars, n] /. x @@ Array[0&, m] -> 0; 
	clist = Flatten[CoefficientList[eqn, vars]]; 
	xvars = Cases[Variables[g], Derivative[__][x][__]]; 
	g = g /. Solve[Thread[clist == 0], xvars]; 
	Return[Thread[x -> g]]
]


End[ ]

Protect[TotalDegree, Taylor, TaylorCoeff, InitialForm, Grading, TaylorCompress, ImplicitSolve]

EndPackage[ ]
