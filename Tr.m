(* -*- mode: math; tab-width: 3; -*- *)

BeginPackage["Tr`"]

Unprotect[Tr]

Tr::usage = "Tr[A] is the trace of the square matrix A."

Tr::nonsq = "Trace of a nonsquare matrix is undefined."

Begin["Private`"]

Tr[A_] := Sum[ A[[i,i]], {i,1,Length[A]}] /; (MatrixQ[A] && Equal @@ Dimensions[A])

Tr[A_] := (Message[Tr::nonsq]; Null)

End[]

Protect[Tr]

EndPackage[]
