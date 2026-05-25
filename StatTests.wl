BeginPackage["AdaptivePreprocess`StatTests`"];

RunStatTests::usage =
 "RunStatTests[data,profile] invokes normality screens and correlations.";

Begin["`Private`"];

Needs["AdaptivePreprocess`Profiling`"];
Needs["AdaptivePreprocess`Utilities`"];

safeP[expr_] := Quiet @ Check[expr, Missing["NotComputed"]];

numericColumnsKeys[prof_] :=
 Keys@
  Select[
   prof["Columns"],
   #["InferredType"] === "Numeric" &];

numericCorrelationMatrix[data_Dataset, profile_] :=
 Module[{cols = numericColumnsKeys[profile], mtx},
  Which[
   cols === {}, {{1.`}},
   Length[cols] < 2, {{1.`}},
   True,
   Quiet @ Check[
    mtx =
     Lookup[#,
       cols] & /@
      AdaptivePreprocess`Utilities`toDatasetRows[data];
    mtx = Replace[mtx, _Missing -> 0., {2}];
    If[!MatrixQ[mtx, NumericQ] || Length[mtx] < 2,
     IdentityMatrix[Length[cols]],
     N[Correlation[mtx]]
    ],
    IdentityMatrix[Length[cols]]
   ]
  ]
];

columnTests[canonical_, col_, p_] /; MatchQ[p["InferredType"], "Numeric"] :=
 Module[{rows, x},
  rows =
   AdaptivePreprocess`Utilities`toDatasetRows[canonical];
  x =
   Cases[
    AdaptivePreprocess`Utilities`cellLookupRow[#,
       col] & /@ rows,
    _?NumericQ];
  If[Length[x] < 3,
   <|
    "ShapiroWilkP" -> Missing["NotComputed"],
    "JarqueBeraP" -> Missing["NotComputed"],
    "AndersonDarlP" -> Missing["NotComputed"],
    "SkewnessAbs" -> Missing["NotComputed"],
    "Kurtosis" -> Missing["NotComputed"]
    |>,
   <|
    "ShapiroWilkP" -> safeP[ShapiroWilkTest[x, "PValue"]],
    "JarqueBeraP" ->
     safeP[
      Quiet @
       Check[JarqueBeraALMTest[x, "PValue"],
        DistributionFitTest[x, NormalDistribution[], "PValue"]]],
    "AndersonDarlP" ->
     safeP[
      AndersonDarlingTest[x,
       NormalDistribution[Mean[x],
        Max[$MachineEpsilon, StandardDeviation[x]]],
       "PValue"]],
    "SkewnessAbs" -> Abs[N[Skewness[x]]],
    "Kurtosis" -> Kurtosis[x] // N
    |>
   ]
 ];

columnTests[__] := <||>;

RunStatTests[canonical_, profile_, OptionsPattern[]] :=
 Module[{tests, corrMatrix, rank},
  tests =
   Association @
    Map[col \[Function]
      col ->
       columnTests[canonical, col, profile["Columns", col]],
     Keys[profile["Columns"]]];
  corrMatrix = numericCorrelationMatrix[canonical, profile];
  rank = Quiet @ MatrixRank[corrMatrix];
  <|
   "ColumnTests" -> tests,
   "Correlations" -> corrMatrix,
   "Multicollinear" ->
    NumericQ[rank] && MatrixQ[corrMatrix] &&
     rank < Dimensions[corrMatrix][[1]]
   |>
 ];

End[];
EndPackage[];
