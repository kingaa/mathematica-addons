(* -*- mode: math; tab-width: 3; -*- *)

BeginPackage["BifCurve`", {"Frechet`", "Algebra`Horner`"}]

BifCurve::usage = "BifCurve[F_, x0_List, varindex_List] continues\
	the curve defined by the function F (see Funcv)."
BifCurve::smstp = "At minimum stepsize, no progress made"
FindTangent::usage = "FindTangent[F_, x0_List, varindx_List, h_]."
FindTangent::nosol = "Tangent vector could not be found"
NewtRaph::usage = "NewtRaph[F, x0, varindx] refines the initial guess x0."
NewtRaph::div = "Newton-Raphson iterations diverging"
NewtRaph::incom = "Incommensurate dimensions in NewtRaph"
Funcv::usage = "Funcv[f, vars] returns a function suitable for use\
	as the first argument in BifCurve, NewtRaph, or FindTangent."

Options[BifCurve] = {
	TryStep -> 0.01, MinStep -> 0.000001, MaxStep -> 1.0, 
	NSteps -> 1000, IncrFactor -> 1.1, DecrFactor -> 3.0, TrapFactor -> 2, 
	Window -> {{-Infinity, Infinity}}
}
Options[NewtRaph] = {MaxIter -> 20, NRTol -> 1*^-7, TrapRad -> 1000}

Begin["Private`"]

Funcv[f_List, x_List] := Module[
	{df = Transpose[Frechet[f,x]], a, b},
	a = Compile[x, #]& /@ f;
	b = Compile[x, #]& /@ df;
	Function[{val, indx}, 
		{ 
			(#[Sequence @@ val]& /@ a),
			(Transpose[#[Sequence @@ val]& /@ b[[indx]]])
		}
	]
]

NewtRaph[F_, x0_List, varindx_List, opts___] := Module[
	{x1=x0, dx, f, df, maxn, tol, traprad},
	{maxn, tol, traprad} = {MaxIter, NRTol, TrapRad} /. {opts} /. Options[NewtRaph];
	Do[
		f = F[x1, varindx];
		df = f[[2]];
		f = f[[1]];
		If[ TrueQ[Length[f] =!= Length[varindx]], Message[NewtRaph::incom]; Return[Null]];
		dx = LinearSolve[df, f];
		For[k=1, k <= Length[varindx], k++,
			x1[[varindx[[k]]]] -= dx[[k]]
		];
		If[ TrueQ[Max[Abs[f]] > traprad], Message[NewtRaph::div]; Return[ ]];
		If[ TrueQ[Max[Abs[dx]] < tol && Max[Abs[f]] < tol], Return[x1]],
		{maxn}
	]
]

FindTangent[F_, x0_List, varindx_List, h_] := Module[
	{G = F[x0, varindx], dg, dx},
	dg = Transpose[ G[[2]] ];
	dx = LinearSolve[ Transpose[Rest[dg]], - h First[dg]];
	If [ Head[dx] == LinearSolve, 
		Message[FindTangent::nosol];
		Throw[Null]
	];
	dx = Prepend[dx, h];
	Return[ reinsert[Array[0&, Length[x0]], dx, varindx] ]
]
	
firstPoint[F_, x0_List, varindx_List, h_, opts___] := Module[
	{x1, dx},
	x1 = Catch[NewtRaph[F,x0, Rest[varindx], opts]];
	If [x1 == Null, Return[ ]];
	dx = rescaleStep[h, FindTangent[F, x1, varindx, h]];
	Return[{x1, dx, varindx}]
]

nextPoint[F_, x0_List, dx_List, varindx_List, h_, opts___] := 
	nextPointAux[1, F, x0, dx, varindx, h, opts]

nextPointAux[k_Integer, F_, x0_List, dx_List, varindx_List, h_, opts___] := Module[
	{x1 = x0 + dx, x2},
	If [k > Length[varindx], 
		Return[Null]
	];
	x2 = Catch[NewtRaph[F, x1, Rest[varindx], opts]];
	If [x2 == Null, 
		Return[
			nextPointAux[
				k+1, F, x0, dx, 
				Append[Rest[varindx], First[varindx]], 
				h, opts
			]
		]
	];
	Return[ {x2, rescaleStep[h, x2 - x0], varindx} ]
]

BifCurve[P_, x0_List, varindx_List, opts___] := Module[
	{z, z1, w, htry, hmin, hmax, nsteps, incrfactor, decrfactor, traprad, k=0, window},
	{h, nsteps} = {TryStep, NSteps} /. {opts} /. Options[BifCurve];
	{hmin, hmax} = {Abs[MinStep], Abs[MaxStep]} /. {opts} /. Options[BifCurve];
	{incrfactor, decrfactor} = {IncrFactor, DecrFactor} /. {opts} /. Options[BifCurve];
	traprad = hmax * TrapFactor /. {opts} /. Options[BifCurve];
	window = Window /. {opts} /. Options[BifCurve];
	z = Catch[firstPoint[P, x0, varindx, h, opts]];
	If[ z == Null, Return[ ]];
	w = { First[z] };
	While[ TrueQ[(Abs[h] >= hmin) && (k < nsteps) && inwindow[ z[[1]], varindx, window ]],
		z1 = Catch[nextPoint[P, z[[1]], z[[2]], resort[Rest[z]], h, opts]];
		If[ TrueQ[(z1 == Null) || (norm[First[z1] - First[z]] > traprad)],
			h /= decrfactor, 
			(	z = z1; 
				k++; 
				h = Max[-hmax, Min[hmax, incrfactor h]];
				w = Append[w, First[z]]
			)
		];
	];
	If[ Abs[h] < hmin, Message[BifCurve::smstp]];
	Return[ w ]
]

nderiv[f_, {x_List, x0_List}, h_:1*^-8] := Module[
	{f0 = f /. Thread[x -> x0], x1, f1, df={}},
	For[ k = 1, k <= Length[x], k++,
		x1 = x0;
		x1[[k]] += h;
		f1 = f /. Thread[x -> x1];
		df = Append[df, 1/h (f1 - f0) //N];
	];
	Return[ Transpose[ df ] ]
]

reinsert[x1_List, _List, {}] := x1

reinsert[x1_List, {}, _List] := x1

reinsert[x1_List, {x2_, X___}, {lead_Integer, follow___Integer}] := 
	ReplacePart[ reinsert[x1, {X}, {follow}], x2, lead ]

resort[{x_List, y_List}] := Sort[y, (Abs[x[[#1]]] > Abs[x[[#2]]])&]

norm[vec_List] := Sqrt[Dot[vec,vec]]

rescaleStep[h_, vec_List] := Abs[h] vec / norm[vec]

inwindow[_List, _List, {}] := True

inwindow[_List, {}, _List] := True

inwindow[z_List, {lead_Integer, follow___Integer}, {{w1_, w2_}, W___List}] := 
	TrueQ[ (z[[lead]] >= w1) && (z[[lead]] <= w2) ] && inwindow[z, {follow}, {W}]

End[ ]
EndPackage[]
