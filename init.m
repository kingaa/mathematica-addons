SetOptions[$Output,PageWidth -> 110]
DeclarePackage["Frechet`", {"Frechet"}]
DeclarePackage["Tr`", {"Tr"}]
DeclarePackage["Taylor`", {"Taylor", "TaylorCoeff", "TotalDegree", 
	"InitialForm", "TaylorCompress"}] 
DeclarePackage["RatSimp`", {"RatSimp"}] 
DeclarePackage["Puiseux`", {"Puiseux", "NewtonPolygon"}] 
DeclarePackage["NormalForm`", {"NormalForm", "ForwardAdjointAction", 
	"BackwardAdjointAction", "FunctionNormalForm", "ForwardAction", 
	"BackwardAction", "Jordan", "VFTransform", "Complexification", 
	"Realification", "Expon", "LieBracket", "Generator"}]
DeclarePackage["Ideal`", {"MultiIndices", "Monomials", "IdealSum",
	"IdealProduct", "IdealIntersection", "Saturation", "Homogeneize", 
	"Dehomogeneize", "GeneralPolynomial", "StandardBasis",
	"TangentCone", "BlockDiagonal", "GrevLex", "GrLex", "Lex"}]
DeclarePackage["NewtonPolyhedron`", {"NewtonPolyhedron", "Support"}]
DeclarePackage["WriteBin`", {"WriteBin"}]
DeclarePackage["Dlivr`", {"Dlivr"}]
DeclarePackage["BifCurve`", {"BifCurve", "FindTangent", "NewtRaph", "Funcv"}]
DeclarePackage["Graphics`ImplicitPlot`", {"ImplicitPlot"}]
DeclarePackage["NumericalMath`Horner`", {"Horner"}]
DeclarePackage["Algebra`SymmetricPolynomials`", {"SymmetricPolynomial", 
	"SymmetricReduction"}]
DeclarePackage["LinearAlgebra`Orthogonalization`", {"Normalize", "Projection",
	"GramSchmidt"}]

If[Environment["MATHINDENTCOOKIE"] =!= $Failed,
	   $BatchInput=False;
	   If[NameQ["System`Private`$IndentSuffix"],
	     ToExpression[
	       "System`Private`$IndentSuffix = Environment[\"MATHINDENTCOOKIE\"]"];
	     Print[" ", Environment["MATHINDENTCOOKIEMSG"]]]]
