(* -*- mode: math; tab-width: 3; -*- *)

BeginPackage["Tr`"]

Unprotect[Tr]

Tr::usage = "Tr[A] is the trace of the square matrix A."

Begin["Private`"]

Tr[A_/;(MatrixQ[A] && (Length[A] == Length[First[A]]))] :=
	Sum[ A[[i,i]], {i,1,Length[A]}]

End[]

Protect[Tr]

EndPackage[]
