(* -*- mode: math; tab-width: 3; -*- *)

BeginPackage["Hilbert`", {"LinearAlgebra`Orthogonalization`"}]

CompleteBasis::usage = "CompleteBasis[V] completes the basis V."

OrthogonalComplement::usage = "OrthogonalComplement[V] gives the
	orthogonal complement of the space spanned by V."

Image::usage = "Image[M] gives an orthogonal basis for the image
	of the linear operator with matrix M."

Begin["Private`"]

CompleteBasis[v_List /; MatrixQ[v], opts___] := Module[
	{n = Length[First[v]], w, x, ip},
	ip = InnerProduct /. {opts} /. Options[GramSchmidt];
	w = GramSchmidt[Join[v,IdentityMatrix[n]], Normalized -> False];
	Select[w, Not[TrueQ[ip[#,#] == 0]]&]
]
	
OrthogonalComplement[v_List /; MatrixQ[v]] := 
	Complement[CompleteBasis[v],v]

Image[m_List /; MatrixQ[m]] := 
	OrthogonalComplement[NullSpace[Transpose[Conjugate[m]]]]

End[]

EndPackage[]
