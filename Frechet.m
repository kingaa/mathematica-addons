(* -*- mode: math; tab-width: 3; -*- *)
(* This is the package Frechet. *)
(* Author: Aaron A. King <kingaa at umich dot edu> *)

BeginPackage["Frechet`"]

Unprotect[Frechet]

Frechet::usage = "Frechet[exprs,vars] returns the Frechet derivative of the list of expressions exprs with respect to the variables vars.  Frechet[exprs,vars,vars] returns the second differential, Frechet[exprs,vars,vars,vars] the third, etc."

Begin["Private`"]

Frechet[f_List, x__List] := Outer[D, f, x]

Frechet[f_, x__List] := Part[Frechet[{f}, x], 1]

End[ ]

Protect[Frechet]

EndPackage[ ]
