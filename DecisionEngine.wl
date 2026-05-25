BeginPackage["AdaptivePreprocess`DecisionEngine`"];

BuildDecisionPlan::usage =
 "BuildDecisionPlan[profile, tests, opts] assembles preprocessing decisions.";

TopClusteringMethods::usage =
 "TopClusteringMethods[profile, tests, n] returns the top n clustering methods.";

Begin["`Private`"];

Needs["AdaptivePreprocess`Profiling`"];

categoricalColumns[profile_] :=
 Keys@
  Select[
   profile["Columns"],
   (#["InferredType"] === "Categorical" ||
      #["InferredType"] === "Binary") &];

numericColumns[profile_] :=
 Keys@
  Select[
   profile["Columns"],
   #["InferredType"] === "Numeric" &];

missingDecision[profile_, col_, opts___] :=
 Module[{p = profile["Columns", col], r, t},
  r = p["MissingRate"];
  t = p["InferredType"];
  Which[
   TrueQ[r == 0 || r === 0.], "None",
   r > 0.6, "DropColumn",
   r > 0.3 && t == "Numeric", "AddMissingIndicator",
   r < 0.05, "DropRow",
   t == "Numeric" &&
    NumericQ[p["NormalityP"]] &&
    N[p["NormalityP"]] > 0.05, "MeanImpute",
   t == "Numeric", "MedianImpute",
   t == "Categorical" || t == "Binary", "ModeImpute",
   MatchQ[t, "Date"] || MatchQ[t, "TimeSeries"],
   "InterpolateTime",
   True, "MedianImpute"]
 ];

encodingDecision[profile_, col_, opts___] :=
 Module[{p = profile["Columns", col], ur, uc},
  uc = Replace[p["UniqueCount"], _Missing -> 10^6];
  ur = Replace[p["UniqueRate"], _Missing -> 1.];
  Which[
   uc <= 2, "FrequencyEncoding",
   ur < 0.2 || uc >= 8, "OneHot",
   True, "FrequencyEncoding"]
 ];

scalingDecision[profile_, tests_, col_, opts___] :=
 Module[{p = profile["Columns", col], t, skew, outR},
  t =
   If[
    KeyExistsQ[tests, "ColumnTests"] &&
     KeyExistsQ[tests["ColumnTests"], col],
    tests["ColumnTests", col],
    <||>];
  skew =
   Quiet @
    If[
     NumericQ[p["Skewness"]],
     Abs[p["Skewness"]],
     If[NumericQ[t["SkewnessAbs"]], t["SkewnessAbs"], 0.]];
  outR =
   Quiet @ If[NumericQ[p["OutlierRateIQR"]], p["OutlierRateIQR"], 0.];
  Which[
   skew > 2 && NumericQ[p["Min"]] && p["Min"] > 0, "LogTransform",
   skew > 1 || outR > 0.05, "RobustScaling",
   NumericQ[t["ShapiroWilkP"]] && t["ShapiroWilkP"] > 0.05,
   "Standardize",
   True, "MinMaxScale"]
 ];

outlierDecision[profile_, tests_, opts___] :=
 Module[{g = profile["GlobalProfile"]},
  Which[
   TrueQ[g["HighOutliers"]], "ClipIQR",
   True, "None"]
 ];

scoreKMeans[profile_, tests_] :=
 With[{g = profile["GlobalProfile"]},
  N @ Which[
    TrueQ[g["HighDimensional"]], 0.45,
    TrueQ[g["AllNumeric"]] && !TrueQ[g["HighOutliers"]], 0.75,
    True, 0.55]
 ];

scoreKMedoids[profile_, tests_] :=
 With[{g = profile["GlobalProfile"]},
  N @ Which[
    TrueQ[g["MixedTypes"]] || TrueQ[g["HighOutliers"]], 0.85,
    True, 0.5]
 ];

scoreAgglomerate[profile_, tests_] :=
 With[{g = profile["GlobalProfile"]},
  N @ Which[
    TrueQ[g["MixedTypes"]], 0.65,
    g["RowCount"] < 2000, 0.7,
    True, 0.45]
 ];

scoreDBSCAN[profile_, tests_] :=
 With[{g = profile["GlobalProfile"]},
  N @ Which[
    TrueQ[g["NonLinear"]], 0.88,
    TrueQ[g["AllNumeric"]] && g["ColumnCount"] === 2, 0.65,
    True, 0.35]
 ];

scoreGMM[profile_, tests_] :=
 With[{g = profile["GlobalProfile"]},
  N @ Which[
    TrueQ[g["AllNumeric"]] && !TrueQ[g["NonLinear"]], 0.72,
    True, 0.4]
 ];

scoreSpectral[profile_, tests_] :=
 With[{g = profile["GlobalProfile"]},
  N @ Which[
    TrueQ[g["NonLinear"]], 0.86,
    True, 0.32]
 ];

candidate[method_, score_] :=
 <|"Method" -> method, "Score" -> score|>;

clusteringCandidates[profile_, tests_, opts___] :=
 Module[{cands},
  cands = {
    candidate["KMeans", scoreKMeans[profile, tests]],
    candidate["KMedoids", scoreKMedoids[profile, tests]],
    candidate["Agglomerate", scoreAgglomerate[profile, tests]],
    candidate["DBSCAN", scoreDBSCAN[profile, tests]],
    candidate["GaussianMixture", scoreGMM[profile, tests]],
    candidate["Spectral", scoreSpectral[profile, tests]]};
  ReverseSort[cands, #1["Score"] < #2["Score"] &]
 ];

selectBestClusteringInPlan[cands_List] := First[cands, <|"Method" -> "KMeans"|>];

TopClusteringMethods[profile_, tests_, n_Integer] :=
 Take[clusteringCandidates[profile, tests][[All, "Method"]], UpTo[n]];

collectReasons[missing_Association, encoding_Association,
  scaling_Association, outlier_] :=
 Join[
  KeyValueMap[{#1, "Missing", #2} &, missing],
  KeyValueMap[{#1, "Encoding", #2} &, encoding],
  KeyValueMap[{#1, "Scaling", #2} &, scaling],
  {{"Global", "Outlier", outlier}}
 ];

BuildDecisionPlan[profile_, tests_, opts : OptionsPattern[]] :=
 Module[{missing, encoding, scaling, outlier, clusterCands, reasons},
  missing =
   AssociationMap[
    col \[Function] missingDecision[profile, col, opts],
    Keys[profile["Columns"]]];
  encoding =
   AssociationMap[
    col \[Function] encodingDecision[profile, col, opts],
    categoricalColumns[profile]];
  scaling =
   AssociationMap[
    col \[Function] scalingDecision[profile, tests, col, opts],
    numericColumns[profile]];
  outlier = outlierDecision[profile, tests, opts];
  clusterCands = clusteringCandidates[profile, tests, opts];
  reasons = collectReasons[missing, encoding, scaling, outlier];
  <|
   "MissingStrategy" -> missing,
   "EncodingStrategy" -> encoding,
   "ScalingStrategy" -> scaling,
   "OutlierStrategy" -> outlier,
   "ClusteringCandidates" -> clusterCands,
   "SelectedClustering" -> selectBestClusteringInPlan[clusterCands],
   "Reasons" -> reasons
   |>
 ];

End[];
EndPackage[];
