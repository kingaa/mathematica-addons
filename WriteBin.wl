(* -*- mode: wolfram; wolfram-indent:3 -*- *)

BeginPackage["WriteBin`"]

Unprotect[WriteBin]

WriteBin::usage = "WriteBin[filename, x] flattens the list of reals x and then appends this to file filename"

WriteBinList::usage = "WriteBinList[file, x]"

Begin["Private`"]

WriteBin[file_String, x_List] := Module[
   {lk, retval},
   lk = Install["writebin"];
   retval = WriteBinList[file, Flatten[x]];
   Uninstall[lk];
   retval
]

End[]

Protect[WriteBin]

EndPackage[]
