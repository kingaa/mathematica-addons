(* -*- mode: wolfram; tab-width: 3; -*- *)
(* This is the package VFTransform for transformation of vectorfields. *)
(* Author: Aaron A. King <kingaa at umich dot edu> *)

BeginPackage["VFTransform`", {"Frechet`", "Taylor`"}]

Unprotect[VFTransform]

VFTransform::usage = "VFTransform[X,oldvars,f,newvars] transforms the vector field X in oldvars by the coordinate transformation f by means of direct substitution.  Thus, if Y = VFTransform[X,x,f,y], then Y = (D(f^-1).X) o f.  VFTransform[X,oldvars,f,newvars,n] gives the result to order n in newvars.  VFTransform[X,oldvars,f,newvars,t] should be used when the transformation f depends on the independent variable t.  Likewise, VFTransform[X,oldvars,f,newvars,t,n] gives the result of the time-dependent transformation f on the vector field X to order n in the vars."

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

End[ ]

Protect[VFTransform]

EndPackage[ ]
