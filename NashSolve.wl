(* -*- mode: wolfram; tab-width: 3; -*- *)

(* This is the package NashSolve for computation *)
(* of Nash equilibria of normal-form games *)
(* Author: Aaron A. King <kingaa at umich dot edu> *)

BeginPackage["NashSolve`"]

Unprotect[NashEq]

NashEq::usage = "NashEq[A,p,c] computes all Nash equilbria of the symmetric normal form game with payoff matrix A. The Nash conditions are expressed in terms of the mixture probabilities p and the payoff c. NashEq[A,B,p,q,c,d] computes the Nash equilibria of the bimatrix game (A,B), i.e., where the player in position I has payoff matrix A and the player in position II has payoff matrix B. In the second form, p and q are the mixture probabilities for players I and II, respectively, while c and d are the respective payoffs. That is, the payoff to player I is p.A.q and that to player II is q.B.p. In both forms, p and q can be supplied as a list of symbols or as a single symbol, in which case the probabilities will be, e.g., p1, p2, etc.";

Begin["Private`"]

nashCondnSym[
		  U_?SquareMatrixQ,
		  s_List,
		  p_List, c_Symbol
] := Module[
		  {n, m, X},
		  n = Range[Length[U]];
		  m = Complement[n, s];
		  X = U.p; 
		  (And @@ Thread[X[[s]] == c]) &&
		  (And @@ Thread[X[[m]] <= c]) &&
		  (Plus @@ p[[s]] == 1 ) &&
		  (And @@ Thread[p[[m]] == 0]) &&
		  (And @@ Thread[p[[s]] > 0])
	  ]

nashCondnAsym[
		  A_?MatrixQ, B_?MatrixQ,
		  s1_List, s2_List,
		  p_List, q_List,
		  c_Symbol, d_Symbol
] := Module[
		  {n1, n2, m1, m2, X1, X2},
		  n1 = Range[Length[A]];
		  n2 = Range[Length[B]]; 
		  m1 = Complement[n1, s1];
		  m2 = Complement[n2, s2];
		  X1 = A.q; 
		  X2 = B.p; 
		  (And @@ Thread[X1[[s1]] == c]) &&
		  (And @@ Thread[X1[[m1]] <= c]) &&
		  (And @@ Thread[X2[[s2]] == d]) &&
		  (And @@ Thread[X2[[m2]] <= d]) &&
		  (Plus @@ p[[s1]] == 1) &&
		  (Plus @@ q[[s2]] == 1) &&
		  (And @@ Thread[p[[m1]] == 0]) &&
		  (And @@ Thread[q[[m2]] == 0]) &&
		  (And @@ Thread[p[[s1]] > 0]) &&
		  (And @@ Thread[q[[s2]] > 0])
	  ] /; Dimensions[A] == Reverse[Dimensions[B]]

nashProbSym[
		  U_?SquareMatrixQ, s_List, p_List, c_Symbol
] := Module[
		  {pp, condn, soln, best, psol, csol},
		  pp = Take[p,Length[U]];
		  condn = nashCondnSym[U, s, pp, c]; 
		  soln = Solve[condn, Join[pp, {c}], Reals];
		  best = If[Length[soln] > 0, Simplify[Thread[U.pp == c] /. soln[[1]]], False];
		  psol = Simplify[If[Length[soln] > 0, pp /. soln[[1]], False]]; 
		  csol = Simplify[If[Length[soln] > 0, c /. soln[[1]], False]]; 
		  Association[
					 "support" -> s,
					 "bestreply" -> best,
					 "conditions" -> condn,
					 "nash" -> psol,
					 "payoff" -> csol
		  ]
	  ]

nashProbSym[
		  U_?SquareMatrixQ, s_List, p_Symbol, c_Symbol
] := Module[
		  {pp},
		  pp = Array[Symbol[ToString[p] <> ToString[#]]&, Length[U]];
		  nashProbSym[U,s,pp,c]
	  ]

nashProbAsym[
		  A_?MatrixQ, B_?MatrixQ,
		  s1_List, s2_List,
		  p_List, q_List,
		  c_Symbol, d_Symbol
] := Module[
		  {pp, qq, condn, soln, best, psol, qsol, csol},
		  pp = Take[p,Length[A]];
		  qq = Take[q,Length[B]];
		  condn = nashCondnAsym[A, B, s1, s2, pp, qq, c, d]; 
		  soln = Solve[condn, Join[pp, qq, {c, d}], Reals]; 
		  best = If[
					 Length[soln] > 0,
					 Simplify[{Thread[A.qq == c], Thread[B.pp == d]} /. soln[[1]]],
					 False
					];
		  psol = Simplify[If[Length[soln] > 0, pp /. soln[[1]], False]]; 
		  qsol = Simplify[If[Length[soln] > 0, qq /. soln[[1]], False]]; 
		  csol = Simplify[If[Length[soln] > 0, {c, d} /. soln[[1]], False]]; 
		  Association[
					 "support" -> {s1, s2},
					 "bestreply" -> best,
					 "conditions" -> condn, 
					 "nash" -> {psol, qsol},
					 "payoff" -> csol
		  ]
	  ] /; Dimensions[A] == Reverse[Dimensions[B]]

nashProbAsym[
		  A_?MatrixQ, B_?MatrixQ,
		  s1_List, s2_List,
		  p_Symbol, q_Symbol,
		  c_Symbol, d_Symbol
] := Module[
		  {pp, qq},
		  pp = Array[Symbol[ToString[p] <> ToString[#]]&,Length[A]];
		  qq = Array[Symbol[ToString[q] <> ToString[#]]&,Length[B]];
		  nashProbAsym[A,B,s1,s2,pp,qq,c,d]
	  ] /; Dimensions[A] == Reverse[Dimensions[B]]

nashProbAsym[
		  A_?MatrixQ, B_?MatrixQ,
		  s1_List, s2_List,
		  p_List, q_Symbol,
		  c_Symbol, d_Symbol
] := Module[
		  {qq},
		  qq = Array[Symbol[ToString[q] <> ToString[#]]&,Length[B]];
		  nashProbAsym[A,B,s1,s2,p,qq,c,d]
	  ] /; Dimensions[A] == Reverse[Dimensions[B]]

nashProbAsym[
		  A_?MatrixQ, B_?MatrixQ,
		  s1_List, s2_List,
		  p_Symbol, q_List,
		  c_Symbol, d_Symbol
] := Module[
		  {pp},
		  pp = Array[Symbol[ToString[p] <> ToString[#]]&,Length[A]];
		  nashProbAsym[A,B,s1,s2,pp,q,c,d]
	  ] /; Dimensions[A] == Reverse[Dimensions[B]]

nashSolve[
		  U_?SquareMatrixQ, p_, c_Symbol
] := Module[
		  {n, s},
		  n = Range[Length[U]];
		  s = Reverse[Rest[Subsets[n]]]; 
		  nashProbSym[U, #, p, c]& /@ s
	  ]

nashSolve[
		  A_?MatrixQ, B_?MatrixQ,
		  p_, q_, c_Symbol, d_Symbol
] := Module[
		  {n1, n2, s1, s2},
		  n1 = Range[Length[A]];
		  n2 = Range[Length[B]];
		  s1 = Reverse[Rest[Subsets[n1]]];
		  s2 = Reverse[Rest[Subsets[n2]]];
		  Flatten[Outer[nashProbAsym[A, B, #1, #2, p, q, c, d] &, s1, s2, 1]]
	  ] /; Dimensions[A] == Reverse[Dimensions[B]]

NashEq[
		  U_?SquareMatrixQ, p_, c_Symbol
] := Select[
		  nashSolve[U, p, c],
		  ! #1["payoff"] === False &
	  ]

NashEq[
		  A_?MatrixQ, B_?MatrixQ,
		  p_, q_, c_Symbol, d_Symbol
] := Select[
		  nashSolve[A, B, p, q, c, d],
		  ! #1["payoff"] === False &
	  ] /; Dimensions[A] == Reverse[Dimensions[B]]

End[ ]

Protect[NashEq]

EndPackage[ ]
