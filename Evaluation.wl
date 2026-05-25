BeginPackage["AdaptivePreprocess`Evaluation`"];

EvaluateClusterings::usage =
 "EvaluateClusterings computes internal metrics for each clustering.";
SelectBestClustering::usage =
 "SelectBestClustering selects the clustering with best composite score.";

Options[EvaluateClusterings] = {"StabilityRuns" -> 20};

Begin["`Private`"];

padNumericPts[pts_List] :=
 Module[{rows, mx},
  rows =
Quiet @ Chop /@ (Flatten[List[#], 1] & /@ pts);
  If[rows === {},
   {},
   mx = Quiet@Max[Flatten[{Length /@ rows, 0}]];
   Map[
PadRight[#,
     mx,
     0.] &,
 rows]];

normalizeLbl[lbl_] :=
 Quiet @ Chop@
  Round[N[Flatten[{lbl}], MachinePrecision]];

clusterSlices[pts_List, lbl_List] :=
 (#[[All, -1]] &) /@ GatherBy[Transpose[{lbl, pts}], First];

QuietMetric[pts_List,
 lbl_,
 name_String] :=
If[Length[lbl] =!= Length[pts],
 0.,
 Module[{p,
   lb,
 slices,
 q},
 p =
padNumericPts[N[pts]];
 lb =
normalizeLbl[lbl];
 If[Length[p] === 0,
  Return[
   0.]];
 If[Length@
    Union[
     lb] < 2,
  Return[
   0.]];
 q =
Quiet @ Chop@
  Check[N @ ClusteringMeasurements[
     p -> lb,
 name,
 DistanceFunction ->
  EuclideanDistance],
   Missing[]];
 If[NumericQ[q],
  Return[q]];
 slices =
   clusterSlices[p, lb];
 If[Length[slices] < 2,
  Return[
   0.]];
 q =
Quiet @ Chop@
 Check[N@
   ClusteringMeasurements[slices,
     name,
     DistanceFunction ->
      EuclideanDistance],
   Missing[]];
 If[NumericQ[q],
 q,
  0.] ]];

pairAgreement[l1_List, l2_List] :=
 Module[{pairs =
    Subsets[
     Range[Min[Length[l1], Length[l2]]],
     {2}]},
  If[pairs === {}, 1.,
   Mean[
    Table[
     With[{
       i = pq[[1]],
       j = pq[[2]]},
      Boole[
       SameQ[
        Boole[l1[[i]] == l1[[j]]],
        Boole[l2[[i]] == l2[[j]]]
       ]]],
     {pq, pairs}]
    ]
   ]
 ];

pairAgreementMean[labelsList_List] :=
 If[Length[labelsList] < 2,
  1.,
  Mean[
   Flatten[
    Table[
     pairAgreement[labelsList[[i]], labelsList[[j]]],
     {i, Length[labelsList]},
     {j, i + 1, Length[labelsList]}]
    ]
   ]
 ];

stabilityScore[cand_, opts : OptionsPattern[]] :=
 Module[{runsRaw =
    OptionValue[EvaluateClusterings, {opts}, "StabilityRuns"],
   runs,
   data = cand["Data"], len},
  runs =
   Max[0,
    Round@
     Quiet@
      Check[
       If[NumericQ[runsRaw], N[runsRaw], 0.],
       0.]];
  len = Length[data];
  If[! ListQ[data] || data === {} || len === 0,
   Return[0.]];
  If[runs === 0,
   Return[0.]];
  pairAgreementMean[
   Table[
    SeedRandom[i + 10^6];
    Module[{subSize, idx, sample},
     subSize =
      Min[len, Max[2, Round[0.8 len]]];
     idx =
      RandomSample[Range[len], subSize];
     sample = data[[idx]];
     Quiet @
      Check[
       ClusteringComponents[sample,
        Lookup[cand, "K", 2],
        1,
        Method ->
         Lookup[
          cand,
          "Method",
          "KMeans"],
       DistanceFunction ->
         Replace[
          Lookup[
           cand,
           "DistanceFunction"],
          Automatic -> EuclideanDistance]],
       ConstantArray[1,
        Length[
         sample]]]],
    {i, runs}]]
 ];

EvaluateClusterings[candidates_List,
  opts : OptionsPattern[]] :=
 Map[
  Function[cand,
   With[{pts = cand["NumericData"], lbl = cand["Labels"]},
    Append[
     cand,
     <|
      "Silhouette" -> QuietMetric[pts, lbl, "Silhouette"],
      "DaviesBouldin" ->
       QuietMetric[pts, lbl, "DaviesBouldin"],
      "CalinskiHarabasz" ->
       QuietMetric[pts, lbl, "CalinskiHarabasz"],
      "Dunn" -> QuietMetric[pts, lbl, "Dunn"],
      "Stability" -> stabilityScore[cand, opts]
      |>]]
   ],
  candidates
 ];

SelectBestClustering[evaluated_List,
  opts : OptionsPattern[]] :=
 Module[{weights, scored},
  weights =
   <|
    "Silhouette" -> 1.0,
    "CalinskiHarabasz" -> 0.5,
    "DaviesBouldin" -> -0.5,
    "Stability" -> 1.0
    |>;
  scored =
   Map[
    Function[cand,
     Append[cand,
      "CompositeScore" ->
       Total[
        KeyValueMap[
         Function[{w, coeff},
          coeff *
           Replace[cand[w], _Missing -> 0.]],
         weights]
        ]]],
    evaluated];
  First[
   ReverseSort[scored,
    #1["CompositeScore"] < #2["CompositeScore"] &],
   <|
    "Method" -> "KMeans",
    "Labels" -> {},
    "K" -> 0,
    "NumericData" -> {},
    "DistanceFunction" -> EuclideanDistance
    |>]
 ];

End[];
EndPackage[];
