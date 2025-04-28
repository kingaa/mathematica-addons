(* -*- mode: wolfram; tab-width: 3; -*- *)
(* This is the package VFTransform for transformation of vectorfields. *)
(* Author: Aaron A. King <kingaa at umich dot edu> *)

BeginPackage["VFTransform`", {"Frechet`", "Taylor`"}]

Unprotect[VFTransform, ExponVFJet, ExponFuncJet, ExponentialSeries]

VFTransform::usage = "VFTransform[X,oldvars,f,newvars] transforms the vector field X in oldvars by the coordinate transformation f by means of direct substitution.  Thus, if Y = VFTransform[X,x,f,y], then Y = (D(f^-1).X) o f.  VFTransform[X,oldvars,f,newvars,n] gives the result to order n in newvars.  VFTransform[X,oldvars,f,newvars,t] should be used when the transformation f depends on the independent variable t.  Likewise, VFTransform[X,oldvars,f,newvars,t,n] gives the result of the time-dependent transformation f on the vector field X to order n in the vars."

ExponentialSeries::usage = "ExponentialSeries[X, vars, n], where X is a vector field in vars, is the first n+1 terms of Exp[X]."

Begin["Private`"]

VFTransform[X_List, old_List, sub_List, new_List] :=
   Expand[
      Inverse[Frechet[sub,new]] . (X /. Thread[old -> sub])
   ]

VFTransform[X_List, old_List, sub_List, new_List, order_Integer] :=
  Module[
    {dsdx, ff, eps},
    ff = Taylor[X /. Thread[old -> sub], new, order];
    dsdx = Taylor[Inverse[Frechet[sub,new]], new, order];
    Taylor[dsdx . ff, new, order]
  ]

VFTransform[X_List, old_List, sub_List, new_List, t_Symbol] :=
   Expand[
      Inverse[Frechet[sub,new]] . ((X /. Thread[old -> sub]) - D[sub,t])
   ]

VFTransform[X_List, old_List, sub_List, new_List, t_Symbol, order_Integer] := 
   Module[
      {dsdx, ff, eps},
      ff = Taylor[X /. Thread[old -> sub], new, order];
      dsdx = Taylor[Inverse[Frechet[sub,new]], new, order];
      Taylor[
         dsdx . ((X /. Thread[old -> sub]) - D[sub,t]),
         new, order
      ]
   ]

ExponVFJet[U_List, X_List, vars_List, n_Integer] :=
   Module[
      {eps, Xe, Ue, F, Y},
      Xe = Expand[X/. Thread[vars->eps vars]];
      Ue = Expand[U/. Thread[vars->eps vars]];
      F[m_,0] := F[m,0] = Coefficient[Xe,eps,m];
      Y[m_] := Y[m] = Coefficient[Ue,eps,m+2];
      F[m_,k_/;k>0] := F[m,k] = Sum[
         LieBracket[Y[i], F[m-i,k-1], vars],
         {i,0,m}
      ];
      Sum[F[m-k,k]/k!,{m,0,n},{k,0,m}]
   ]

ExponFuncJet[U_List, f_, vars_List, n_Integer] :=
   Module[
      {eps, fe, Ue, F, Y},
      fe = Expand[f /. Thread[vars -> eps vars]];
      Ue = Expand[U /. Thread[vars -> eps vars]];
      F[m_,0] := F[m,0] = Coefficient[fe,eps,m];
      Y[m_] := Y[m] = Coefficient[Ue,eps,m+2];
      F[m_,k_/;k>0] := F[m,k] =
         Sum[LieDeriv[Y[i], F[m-i,k-1], vars], {i,0,m}];
      Sum[F[m-k,k]/k!,{m,0,n},{k,0,m}]
   ]

ExponentialSeries[X_List, x_List, n_Integer] :=
   Module[
      {L},
      L[f_] := LieDeriv[X,f,x];
      Plus @@ (Table[1/k!,{k,0,n}] NestList[L,x,n])
   ]

End[ ]

Protect[VFTransform, ExponVFJet, ExponFuncJet, ExponentialSeries]

EndPackage[ ]
