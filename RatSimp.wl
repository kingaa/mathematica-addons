(* -*- mode: wolfram; wolfram-indent: 3; -*- *)

BeginPackage["RatSimp`"]

Unprotect[RatSimp]

RatSimp::usage = "RatSimp[expr, G, vars] reduces and rationalizes expr over the Groebner basis G in variables vars"

Begin["Private`"]

RatSimp[f_List, G_, vars_] := RatSimp[#,G,vars]& /@ f

RatSimp[f_, G_List, vars_List] := Module[
   {p = Together[f], n, r, a, b, H},
   n = Table[ Max[ Exponent[#,vars[[i]]]& /@ G ], {i,1,Length[vars]}];
   If[ Max[n] <= 0, Return[ Expand[p] ] ];
   H = GroebnerBasis[G, vars, MonomialOrder -> DegreeReverseLexicographic];
   b = RSPoly[vars,n-1];
   r = PolynomialReduce[
      b Denominator[p] - Numerator[p], 
      G, vars, 
      MonomialOrder -> DegreeReverseLexicographic
   ][[2]];
   a = CoefficientList[Expand[r],vars] //Flatten;
   Return[ b /. First[Solve[Thread[a==0], Variables[a]]] ]
]  

RatSimp[f_, G_, var_] := RatSimp[f,{G},{var}]

RSPoly[{x_, X__}, {n_Integer, N__Integer}] := Expand[Sum[RSPoly[{X},{N}] x^k, {k,0,n}]]

RSPoly[{x_}, {n_Integer}] := Sum[Unique["RS"] x^k, {k,0,n}]

End[ ]

Protect[RatSimp]

EndPackage[ ]
