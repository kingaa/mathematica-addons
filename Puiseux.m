(* -*- mode: math; tab-width: 3; -*- *)

BeginPackage["Puiseux`", {"FieldRat`"}]

Unprotect[NewtonPolygon, NewtonPowers, NPSolve]

NewtonPolygon::usage = "NewtonPolygon[poly, x, y]"

NewtonPowers::usage = "NewtonPowers[poly, x, y]"

NPSolve::usage = "NPSolve[poly, x, y, order]"

Begin["Private`"];

NPRels = {};
NPPars = {};

NewtonPolygon[p_, x_, y_] := Module[
	{found, L}, 
	L = Map[List[#,Exponent[Coefficient[p,x,#],y,Min]] &,Exponent[p,x,List]];
	L = Sort[L,#1[[1]] < #2[[1]] &];
	While[ Length[L] > 2, (
	    For[i=2, i <= Length[L]-1, i++,
		( found = False;
		If[(L[[i,2]]-L[[i-1,2]])*(L[[i+1,1]]-L[[i,1]])
			>= (L[[i+1,2]]-L[[i,2]])*(L[[i,1]]-L[[i-1,1]]), 
		(found = True; L = Delete[L,i])])
		 ];
	    If[ L[[Length[L],2]] == L[[Length[L]-1,2]], 
		(found = True; L = Delete[L,Length[L]])];
	    If[ !found, Return[L] ]);
	];
	If[ (Length[L] == 2) && (L[[1,2]] == L[[2,2]]),
		L = {L[[1]]} ];
	Return[L]; 
]

NewtonPowers[p_, x_, y_] := Module[
	{L = NewtonPolygon[p,x,y]},
	Table[-(L[[i+1,2]]-L[[i,2]])/(L[[i+1,1]]-L[[i,1]]),{i,1,Length[L]-1}]
]

NPSubsts[p_, x_, y_] := Module[
	{L = NewtonPowers[p,x,y]},
	Map[{x->x^Numerator[#], y->x^Denominator[#]*y} &, L]
]

NPBlowup[p_, x_, y_] := Map[Module[
	{q = p /. #},
	List[#,Expand[q / (x^Exponent[q,x,Min])]]
	]&, 
	NPSubsts[p, x, y]
]

PolySimp[f_] := FieldRat[f, NPRels, NPPars]

NPShift[p_, x_, y_, n_Integer] := Module[
	{param, sol, s, q, r = {}, i},
	sol = (p /. x->0);
	sol = FactorList[( sol / (y^Exponent[sol, y, Min]) //Expand)];
	For[i = 2, i <= Length[sol], i++,
		If[ ! NumberQ[sol[[i,1]]],
			If [Exponent[sol[[i,1]], y] == 1, 
				param = Part[Solve[sol[[i,1]]==0, y], 1, 1, 2],
				param = Unique["k"];
				NPPars = Prepend[NPPars, param];
				NPRels = GroebnerBasis[
					Append[
						NPRels,
						(sol[[i,1]] /. y->param)
					], 
					NPPars
				]
			];
			s = y -> y + param;
			q = PolySimp[(p /. s) //Expand];
			If [ Expand[q /. {x->0, y->0}] == 0,
				r = Append[r, NPS[s, NPResolution[q,x,y,n-1]]];
			]
		]
	];
	Return[ r ]
]

NPResolution[_, _, y_, 0] := NPBottom[y -> 0]

NPResolution[p_, x_, y_, n_Integer/;(n > 0)] := Module[
	{L = NPBlowup[p, x, y], e},
	If[ MemberQ[Variables[p], x],
		Table[
			NPB[L[[e,1]], NPShift[L[[e,2]], x, y, n]], 
			{e,1,Length[L]}
		],
		NPBottom[y->0]
	]
]	

UndetCoeff[p_, x_, y_, n_Integer] := Module[
	{u, s, q, sols, ss},
	s = Sum[u[k] x^k, {k,1,n}],
 	q = Normal[Series[(p /. y -> s),{x,0,n}]];
	sols = Solve[Table[Coefficient[q,x,k]==0,{k,1,n}],
		Table[u[k],{k,1,n}]];
	ss = Map[PolySimp[s /. #] &, sols];
	ss = Map[(y -> #) &, Together[ss]];
	Return[ Apply[UDC, ss] ]
]

NPBackSolve[e_, s_, y_] := Module[
	{n = Length[s], k},
	If [ n == 0, Return[ { e /. y -> 0 } ] ];
	If [ Head[s] == NPBottom,
		Return[ { e /. s[[1]] } ]
	];
	If [ Head[s] == UDC,
		Return[ Table[(e /. s[[k]]), {k,1,n}] ]
	];
	If [ Head[s] == NPS,
		Return[
			NPBackSolve[(e /. s[[1]]), s[[2]], y]
		]
	];
	If [ Head[s] == NPB,
		Return[
			Join @@ Table[
				NPBackSolve[(e /. s[[1]]), s[[k]], y],
				{k,2,n}
			]
		]
	];
	If [ Head[s] == List,
		Return[
			Join @@ Table[
				NPBackSolve[e, s[[k]], y],
				{k,1,n}
			]
		]
	];
]

NPSolve[p_, x_, y_, n_Integer] := Module[
	{q = FactorList[p], F, rels, r, s, i},
 	NPPars = {};
	NPRels = {};
	s = Table[ NPResolution[q[[i,1]], x, y, n], {i,2,Length[q]}];
	s = s /. UDC[s_] -> UDC[s, x, y, n];
	s = s /. UDC -> UndetCoeff;
	r = NPBackSolve[F[x,y], s, y];
	m = Map[Exponent[First[#], x]&, r];
	Return[
		{
			Table[
				(r[[k,2]] /. x->x^(1/m[[k]]) //Expand),
				{k,1,Length[r]}
			],
			 NPRels
		}
	] 
]

End[ ]

Protect[NewtonPolygon, NewtonPowers, NPSolve]

EndPackage[ ]
