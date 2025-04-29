(* -*- mode: wolfram; wolfram-indent: 3; -*- *)

BeginPackage["Puiseux`", {"RatSimp`"}]

Unprotect[NewtonPolygon, Puiseux, RootForm]

NewtonPolygon::usage = "NewtonPolygon[poly, x, y] gives the Newton Polygon of poly(x,y)."

Puiseux::usage = "Puiseux[poly, x, y, order] solves the polynomial equation poly(x,y) = 0 for y in terms of x by the method of Newton and Puiseux."

Options[Puiseux] = {RootForm -> True}

Begin["Private`"];

NPRels = {}
NPPars = {}

NewtonPolygon[p_, x_, y_] := Module[
   {found = False, L}, 
   L = {#, Exponent[Coefficient[p,x,#],y,Min]}& /@ Union[Exponent[p,x,List]];
   L = Sort[L, (#1[[1]] < #2[[1]])&];
   For[ j = 2, j <= Length[L], j++,
      If[ L[[j,2]] == 0, 
         found = True;
         Break[ ]
      ]
   ];
   If[found, L = Take[L,j]];
   While[ Length[L] > 2,
      For[ i = 2, i <= Length[L]-1, i++,
         found = False;
         If[ (L[[i,2]]-L[[i-1,2]])*(L[[i+1,1]]-L[[i,1]])
            >= (L[[i+1,2]]-L[[i,2]])*(L[[i,1]]-L[[i-1,1]]), 
            found = True; 
            L = Delete[L,i]
         ];
      ];
      If[ L[[Length[L],2]] == L[[Length[L]-1,2]], 
         found = True; 
         L = Delete[L,Length[L]]
      ];
      If[ !found, 
         Return[ L ]
      ]
   ];
   If[ (Length[L] == 2) && (L[[1,2]] == L[[2,2]]),
      L = {L[[1]]}
   ];
   Return[ L ]
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

NPShift[p_, x_, y_, n_Integer] := Module[
   {param, sol, m, s, q, r = {}, i},
   sol = #[[1]]& /@ Rest[FactorList[ p /. x -> 0 ]];
   For[i = 1, i <= Length[sol], i++,
      m = Exponent[sol[[i]], y];    
      If[ m == 1,
         param = Solve[sol[[i]]==0, y][[1,1,2]];
         s = y -> y + param;
         q = Expand[p /. s];
      ];
      If[ m > 1,
         param = Unique["k"];
         NPPars = Prepend[NPPars, param];
         NPRels = Append[NPRels, (sol[[i]] /. y->param)]; 
         s = y -> y + param;
         q = RatSimp[ Expand[p /. s], NPRels, NPPars ] //Expand;
      ];
      If[ TrueQ[Expand[q /. {x -> 0, y -> 0}] == 0], 
         If[ !TrueQ[ param == 0 ],
            r = Append[r, NPS[s, NPResolution[q,x,y,n-1]]]
         ]
      ]
   ];
   Return[ r ]
]

NPSolve[p_, x_, y_, n_Integer] := Module[
   {param, sol, m, s, q, r = {}, i},
   sol = #[[1]]& /@ Rest[FactorList[ p /. x -> 0 ]];
   For[i = 1, i <= Length[sol], i++,
      m = Exponent[sol[[i]], y];    
      If[ m == 1,
         param = Solve[sol[[i]]==0, y][[1,1,2]];
         s = y -> y + param;
         q = Expand[p /. s];
      ];
      If[ m > 1,
         param = Unique["k"];
         NPPars = Prepend[NPPars, param];
         NPRels = Append[NPRels, (sol[[i]] /. y->param)];
         s = y -> y + param;
         q = RatSimp[ Expand[p /. s], NPRels, NPPars ] //Expand;
      ];
      If[ TrueQ[Expand[q /. {x -> 0, y -> 0}] == 0], 
         r = Append[r, NPS[s, NPResolution[q,x,y,n]]]
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

NPBackSolve[_, {}, _] := {}

NPBackSolve[e_, s_, y_] := Module[
   {n = Length[s], k},
   If[ n == 0, Return[ { e /. y -> 0 } ] ];
   If[ Head[s] == NPBottom,
      Return[ { e /. s[[1]] } ]
   ];
   If[ Head[s] == NPS,
      Return[
         NPBackSolve[(e /. s[[1]]), s[[2]], y]
      ]
   ];
   If[ Head[s] == NPB,
      Return[
         Join @@ Table[
            NPBackSolve[(e /. s[[1]]), s[[k]], y],
            {k,2,n}
         ]
      ]
   ];
   If[ Head[s] == List,
      Return[
         Join @@ Table[
            NPBackSolve[e, s[[k]], y],
            {k,1,n}
         ]
      ]
   ];
]

Puiseux[p_, x_, y_, n_Integer, opts___] := Module[
   {q = Rest[FactorList[Expand[p / (x^Exponent[p,x,Min])]]], 
      F, r, s, i, exp},
   NPPars = {};
   NPRels = {};
   Off[Solve::svars];
   s = Table[ NPResolution[q[[i,1]], x, y, n], {i,1,Length[q]}];
   r = NPBackSolve[F[x,y], s, y];
   m = Map[Exponent[First[#], x]&, r];
   exp = Table[
            RatSimp[ Expand[r[[k,2]] /. x -> x^(1/m[[k]])], NPRels, NPPars] //Expand,
            {k,1,Length[r]}
         ];
   On[Solve::svars];
   If[ RootForm /. {opts} /. Options[Puiseux],
      Union @@ ((exp /. #)& /@ Solve[ Thread[NPRels == 0], NPPars]),
      {exp, NPRels}
   ]
]

End[ ]

Protect[NewtonPolygon, Puiseux, RootForm]

EndPackage[ ]
