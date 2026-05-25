BeginPackage["AdaptivePreprocess`Clustering`"];

GenerateClusteringCandidates::usage =
 "GenerateClusteringCandidates[data, profile, opts] fits clustering models.";
chooseKBySilhouette::usage =
 "chooseKBySilhouette picks k by maximal silhouette.";

Begin["`Private`"];

Needs["AdaptivePreprocess`Utilities`"];
Needs["AdaptivePreprocess`DecisionEngine`"];

gowerDistance[x_, y_] :=
 If[Length[x] =!= Length[y],
  $Failed,
  Total[MapThread[
    Function[{a, b},
     Which[
      MissingQ[a] || MissingQ[b], 1.,
      NumericQ[a] && NumericQ[b], Abs[N[a - b]],
      SameQ[a, b], 0.,
      True, 1.]],
    {x, y}]]
 ];

selectDistance[profile_] :=
 Module[{types = Values[profile["Columns"]][[All, "InferredType"]],
   numericOnly},
  numericOnly = AllTrue[types, # === "Numeric" &];
  Which[
   numericOnly &&
    TrueQ[profile["GlobalProfile", "HighDimensional"]],
   CosineDistance,
   numericOnly,
   EuclideanDistance,
   True,
   Automatic
 ]
 ];

normalizeDist[Automatic] :=
 Function[{u, v}, Quiet @ Check[gowerDistance[u, v], EuclideanDistance[u, v]]];
normalizeDist[d_] := d;

makeNumeric[data_Dataset] :=
 Module[{rows},
  rows =
   AdaptivePreprocess`Utilities`toDatasetRows[data];
  If[rows === {},
   {},
   AdaptivePreprocess`Utilities`flattenRowNumeric /@
    rows]
 ];

chooseKBySilhouette[data_, method_, kRange_List, distFn_] :=
 Module[{scores},
  scores =
   AssociationMap[
    Function[k,
     Quiet @
      Check[
       N @
        ClusteringMeasurements[
         data -> ClusterClassify[data, k,
          Method -> method,
          DistanceFunction -> distFn],
         "Silhouette"],
       0.]],
    kRange];
  First[Keys[ReverseSort[scores]], Min[kRange]]
 ];

defaultK[n_Integer, kRange_List] :=
 Clip[Round[Sqrt[N[n]]], {Min[kRange], Max[kRange]}];

fitClustering[data_, method_String, kRange_List, dist_, opts___] :=
 Module[{numericData, labels, k, len, distFn},
  numericData = makeNumeric[data];
  len = Length[numericData];
  If[len === 0,
   Return[<|
     "Method" -> method,
     "Clusters" -> {},
     "Labels" -> {},
     "DistanceFunction" -> dist,
     "K" -> 0,
     "NumericData" -> {},
     "Data" -> {}
     |>]];
  distFn = normalizeDist[dist];
  k =
   If[MemberQ[{"DBSCAN", "Spectral"}, method],
    defaultK[len, kRange],
    Quiet @ Check[
     chooseKBySilhouette[numericData, method, kRange, distFn],
     defaultK[len, kRange]]
    ];
  labels =
   Quiet @
    Check[
     ClusteringComponents[numericData, k, 1,
      Method -> method,
      DistanceFunction -> distFn],
     ConstantArray[1, len]];
  k = Length @ Union[labels];
  <|
   "Method" -> method,
   "Clusters" -> labels,
   "Labels" -> labels,
   "DistanceFunction" -> dist,
   "K" -> k,
   "NumericData" -> numericData,
   "Data" -> numericData
   |>
 ];

Options[GenerateClusteringCandidates] = {
 "ClusteringMethods" -> Automatic,
 "ClusterRange" -> Range[2, 10]
};

GenerateClusteringCandidates[data_, profile_,
  opts : OptionsPattern[]] :=
 Module[{methods, kRange, dist, candidates, testsStub},
  testsStub =
   <|"ColumnTests" -> <||>, "Multicollinear" -> False,
    "Correlations" -> {{1}}|>;
  methods =
   Replace[
    OptionValue[GenerateClusteringCandidates, {opts},
     "ClusteringMethods"],
    Automatic :> TopClusteringMethods[profile, testsStub, 5]];
  kRange =
   OptionValue[GenerateClusteringCandidates, {opts}, "ClusterRange"];
  dist = selectDistance[profile];
  candidates =
   Table[
    Quiet @ Check[
     fitClustering[data, m, kRange, dist, opts],
     <|
      "Method" -> m,
      "Clusters" -> {},
      "Labels" -> ConstantArray[1, Length[data]],
      "DistanceFunction" -> dist,
      "K" -> 1,
      "NumericData" -> makeNumeric[data],
      "Data" -> makeNumeric[data]
      |>],
    {m, methods}];
  candidates
 ];

End[];
EndPackage[];
