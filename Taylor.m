(* -*- mode: math; tab-width: 3; -*- *)

BeginPackage["Taylor`"]

Unprotect[TotalDegree, Taylor, TaylorCoeff, InitialForm, Grading]

TotalDegree::usage = "TotalDegree[expr, vars] gives the total degree\
	 of the polynomial expression expr in the variables vars.\
	 TotalDegree[expr] gives the total degree of the polynomial expr in\
	 its variables."

Taylor::usage = "Taylor[expr, vars, n] gives the Taylor polynomial\
	 of expr in the variables vars to order n."

TaylorCoeff::usage = "TaylorCoeff[expr, vars, m] gives the\
	 coefficient of expr in variables vars of degree m\
	 (m is a multi-index)."

InitialForm::usage = "InitialForm[expr, vars] is the homogeneous\
	 form of expr in vars of lowest total degree."

Grading::usage = "Grading is an option for Taylor, TotalDegree,\
	 and InitialForm."

Options[Taylor] = {Grading -> 1}

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

TaylorCoeff[expr_, var_List, order_List] := 
	If[ Length[var] == 0, 
		Return[expr],
		TaylorCoeff[
			Coefficient[expr,First[var],First[order]],
			Rest[var],
			Rest[order]
		]
	]

TaylorCoeff[e_, x_Symbol, n_Integer] := TaylorCoeff[e,{x},n]

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
	
End[ ]

Protect[TotalDegree, Taylor, TaylorCoeff, InitialForm, Grading]

EndPackage[ ]
