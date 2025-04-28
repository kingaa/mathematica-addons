(* -*- mode: wolfram; tab-width: 3; -*- *)
(* This is the package Frechet. *)
(* Author: Aaron A. King <kingaa at umich dot edu> *)

BeginPackage["Frechet`"]

Unprotect[Frechet, LieBracket, LieDeriv]

Frechet::usage = "Frechet[exprs,vars] returns the Frechet derivative of the list of expressions exprs with respect to the variables vars.  Frechet[exprs,vars,vars] returns the second differential, Frechet[exprs,vars,vars,vars] the third, etc."

LieBracket::usage = "LieBracket[X,Y,vars] is the Lie bracket of the vector fields X and Y in the variables vars: [X,Y] = DY.X - DX.Y"

LieDeriv::usage = "LieDeriv[X,f,vars] is the Lie deriviative of the function f with respect to the vectorfield X in the variables vars."

Begin["Private`"]

Frechet[f_List, x__List] := Outer[D, f, x]

Frechet[f_, x__List] := Part[Frechet[{f}, x], 1]

LieBracket[X_List, Y_List, vars_List] := Frechet[Y,vars].X - Frechet[X,vars].Y

LieDeriv[X_List, f_, vars_List] := Frechet[f,vars].X

End[ ]

Protect[Frechet, LieBracket, LieDeriv]

EndPackage[ ]
