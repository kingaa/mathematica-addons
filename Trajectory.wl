(* -*- mode: math; tab-width: 3; -*- *)
(* This is the package Trajectory. *)
(* Author: Aaron A. King <king at umich dot edu> *)

BeginPackage["Trajectory`"]

Unprotect[Trajectory]

Trajectory::usage = "Trajectory[X,vars,init,{t,t0,tf}] numerically integrates the vectorfield X with state variables vars.  The symbol for time is t.  The computation yields the trajectory from t=t0, at which time vars==init, to t=tf."

Begin["Private`"]

Trajectory[X_List, vars_List, init_List, {t_Symbol, t0_, tf_}] := 
  Module[{flist, dd, vf, ic},
  flist = (#[t]) & /@ vars;
  dd = D[flist, t];
  vf = X /. Thread[vars -> flist];
  vf = Thread[dd == vf];
  ic = Thread[flist == init] /. t -> t0;
  flist /. NDSolve[Join[vf, ic], vars, {t, t0, tf}]
]

Trajectory[X_List, vars_List, {init__List}, {t_Symbol, t0_, tf_}] := 
  Trajectory[X,vars,#,{t,t0,tf}]& /@ {init}

End[ ]

Protect[Trajectory]

EndPackage[ ]
