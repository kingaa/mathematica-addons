(* -*- mode: math; tab-width: 3; -*- *)

BeginPackage["Tr`"]

Unprotect[Tr]

Tr::usage = "Tr[A] is the trace of the square matrix A."

Tr::matsq = "Argument `1` at position `2` is not a square matrix."

Begin["Private`"]

Tr[A_] := Sum[ A[[i,i]], {i,1,Length[A]}] /; (MatrixQ[A] && Equal @@ Dimensions[A])

Tr[A_] := (Message[Tr::matsq, A, 1]; Null)

End[]

Protect[Tr]

EndPackage[]
