(* -*- mode: wolfram; tab-width: 3; -*- *)
(* This is the package BifCurve for numerical continuation of bifurcations. *)
(* Author: Aaron A. King <kingaa at umich dot edu> *)

BeginPackage["BifCurve`", {"Frechet`"}]

BifCurve::usage = "BifCurve[F_, x0_List, varindex_List] continues the curve defined by the function F (see Funcv)."
BifCurve::smstp = "At minimum stepsize, no progress made"
FindTangent::usage = "FindTangent[F_, x0_List, varindx_List, h_]."
NewtRaph::nosol = "Linear equation with no solution encountered in NewtRaph."
FindTangent::nosol = "Tangent vector could not be found."
NewtRaph::usage = "NewtRaph[F, x0, varindx] refines the initial guess x0."
NewtRaph::div = "Newton-Raphson iterations diverging."
NewtRaph::incom = "Incommensurate dimensions in NewtRaph."
Funcv::usage = "Funcv[f, vars] returns a function suitable for use as the first argument in BifCurve, NewtRaph, or FindTangent."
FirstLyapunovCoefficient::assumpfail = "Assumptions have failed in FirstLyapunovCoefficient!";
Jet::usage = "Jet[X, vars, base, parameters, order] computes the jet of the real vectorfield X in real variables vars up to the specified order at the specified base.  The argument base is a list of rules, one for each of the variables in vars; the rules may depend on the parameters.  The computations are compiled for efficiency.";
Jet::order = "Jet order is `1`, requested order is `2`.";
FirstLyapunovCoefficient::usage = "FirstLyapunovCoefficient[F, x] computes the 1st Lyapunov coefficient of the vectorfield with jet F in variables x. F is an object of the type produced by Jet (see Jet) with derivatives up to at least order 3; x is a list containing the coordinates of the point (parameters only) at which the evaluation of the first Lyapunov coefficient is desired. It is assumed that x is a Hopf point.  If the 1st Lyapunov coefficient is negative, the Hopf bifurcation is supercritical; if negative, it is subcritical.  The vanishing of the 1st Lyapunov coefficient is a necessary condition for a Bautin bifurcation.";

Options[BifCurve] = {
   TryStep -> 0.01, MinStep -> 0.000001, MaxStep -> 1.0, 
   NSteps -> 1000, IncrFactor -> 1.1, DecrFactor -> 3.0, TrapFactor -> 2, 
   Window -> {{-Infinity, Infinity}}
}

Options[NewtRaph] = {MaxIter -> 20, NRTol -> 1*^-7, TrapRad -> 1000}

Begin["Private`"]

Funcv[f_List, x_List] := Module[
   {df = Transpose[Frechet[f,x]], a, b},
   a = Compile[Evaluate[x], Evaluate[f]];
   b = Compile[Evaluate[x], #]& /@ df;
   Function[{val, indx}, 
      { 
         (a @@ val),
         Transpose[(# @@ val)& /@ b[[indx]]]
      }
   ]
]

NewtRaph[F_, x0_List, varindx_List, opts___] := Module[
   {x1=x0, dx, f, df, maxn, tol, traprad},
   {maxn, tol, traprad} = {MaxIter, NRTol, TrapRad} /. {opts} /. Options[NewtRaph];
   Off[LinearSolve::nosol];
   Do[
      f = F[x1, varindx];
      df = f[[2]];
      f = f[[1]];
      If[ TrueQ[Length[f] =!= Length[varindx]], 
         Message[NewtRaph::incom]; 
         On[LinearSolve::nosol];
         Return[Null]
      ];
      dx = LinearSolve[df, f];
      If [ Head[dx] == LinearSolve, 
         Message[NewtRaph::nosol];
         On[LinearSolve::nosol];
         Throw[Null]
      ]; 
      For[k=1, k <= Length[varindx], k++,
         x1[[varindx[[k]]]] -= dx[[k]]
      ];
      If[ TrueQ[Max[Abs[f]] > traprad], 
         Message[NewtRaph::div]; 
         On[LinearSolve::nosol];
         Return[Null]
      ];
      If[ 
         TrueQ[Max[Abs[dx]] < tol && Max[Abs[f]] < tol], 
         On[LinearSolve::nosol];
         Return[x1]
      ],
   {maxn}
   ]
]

FindTangent[F_, x0_List, varindx_List, h_] := Module[
   {G = F[x0, varindx], dg, dx},
   Off[LinearSolve::nosol];
   dg = Transpose[ G[[2]] ];
   dx = LinearSolve[ Transpose[Rest[dg]], - h First[dg]];
   If [ Head[dx] == LinearSolve, 
      Message[FindTangent::nosol];
      On[LinearSolve::nosol];
      Throw[Null]
   ];
   dx = Prepend[dx, h];
   On[LinearSolve::nosol];
   Return[ reinsert[Array[0&, Length[x0]], dx, varindx] ]
]
   
firstPoint[F_, x0_List, varindx_List, h_, opts___] := Module[
   {x1, dx},
   x1 = Catch[NewtRaph[F,x0, Rest[varindx], opts]];
   If [x1 == Null, Return[Null]];
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
   If[ z == Null, Return[Null]];
   w = { First[z] };
   While[ TrueQ[(Abs[h] >= hmin) && (k < nsteps) && inwindow[ z[[1]], varindx, window ]],
      z1 = Catch[nextPoint[P, z[[1]], z[[2]], resort[Rest[z]], h, opts]];
      If[ TrueQ[(z1 == Null) || (norm[First[z1] - First[z]] > traprad)],
         h /= decrfactor, 
         (  z = z1; 
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

Jet[X_List, x_List, base_List, par_List, n_Integer] := Module[
   {d, c, j},
   d = NestList[Frechet[#, x]&, X, n] /. base;
   For[c = {}; j = 1, j <= n+1, j++,
      c = Append[c, Compile[Evaluate[par], Evaluate[d[[j]]]]];
   ];
   Return[
      Function[
         {k},
         If[ k > n || k < 0,
            Message[Jet::order, n, k];
            $Failed,
            c[[k+1]]]
         ]
   ];
]

FirstLyapunovCoefficient[ft_, x_] := Module[
   {a, b, c, p, q, w, v, n, y, z}, 
   a = ft[1] @@ x; 
   b = ft[2] @@ x; 
   c = ft[3] @@ x; 
   n = Length[a]; 
   {w, p} = Select[
      Chop[Transpose[Eigensystem[Transpose[a]]]],
      (Re[#[[1]]] == 0 && Im[#[[1]]] < 0)&
   ][[1]];
   {v, q} = Select[
      Chop[Transpose[Eigensystem[a]]],
      (Re[#[[1]]] == 0 && Im[#[[1]]] > 0)&
   ][[1]];
   If[
      Chop[w - Conjugate[v]] != 0
         || Re[w] != 0
         || Im[w] > 0,
      Print[{w,v,p,q}];
      Message[FirstLyapunovCoefficient::assumpfail];
      Abort[]
   ];
   q = q/Sqrt[Re[Conjugate[q].q]];
   p = p/(p.Conjugate[q]);
   y = LinearSolve[a + 2 w IdentityMatrix[n], b.q.q];
   z = LinearSolve[a, b.q.Conjugate[q]];
   Return[
      (1/2)*Re[Conjugate[p].(c.q.q.Conjugate[q] - b.Conjugate[q].y - 2 b.q.z)]
   ]
]

End[ ]
EndPackage[]
