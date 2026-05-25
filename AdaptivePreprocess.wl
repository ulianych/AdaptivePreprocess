(* AdaptivePreprocess: loader + public API (diploma appendix A, chapter 3). *)

$AdaptivePreprocessRoot = DirectoryName[$InputFileName];

With[{d = $AdaptivePreprocessRoot},
 Get[FileNameJoin[{d, "Utilities.wl"}]];
 Get[FileNameJoin[{d, "Ingestion.wl"}]];
 Get[FileNameJoin[{d, "Profiling.wl"}]];
 Get[FileNameJoin[{d, "StatTests.wl"}]];
 Get[FileNameJoin[{d, "DecisionEngine.wl"}]];
 Get[FileNameJoin[{d, "Outliers.wl"}]];
 Get[FileNameJoin[{d, "Transformers.wl"}]];
 Get[FileNameJoin[{d, "Clustering.wl"}]];
 Get[FileNameJoin[{d, "Evaluation.wl"}]];
 Get[FileNameJoin[{d, "Reporting.wl"}]];
 ]

BeginPackage["AdaptivePreprocess`"];

AdaptivePreprocess::usage =
 "AdaptivePreprocess[data] performs adaptive preprocessing " <>
  "of input data and returns an association with results.";

AdaptiveCluster::usage =
 "AdaptiveCluster[data] performs adaptive preprocessing " <>
  "and selects a clustering algorithm.";

AdaptivePreprocess::highmissing =
 "Column `1` has too many missing values and may be removed.";

Begin["`Private`"];

Needs["AdaptivePreprocess`Ingestion`"];
Needs["AdaptivePreprocess`Profiling`"];
Needs["AdaptivePreprocess`StatTests`"];
Needs["AdaptivePreprocess`DecisionEngine`"];
Needs["AdaptivePreprocess`Transformers`"];
Needs["AdaptivePreprocess`Outliers`"];
Needs["AdaptivePreprocess`Clustering`"];
Needs["AdaptivePreprocess`Evaluation`"];
Needs["AdaptivePreprocess`Reporting`"];

fitClusterModel[pts_List, cand_Association] :=
 If[pts === {} || !KeyExistsQ[cand, "K"],
  Missing["NotComputed"],
  Quiet @
   Check[
    ClusterClassify[pts,
     cand["K"],
     Method -> cand["Method"],
     DistanceFunction ->
      Replace[cand["DistanceFunction"],
       Automatic -> EuclideanDistance]],
    Missing["NotComputed"]]
 ];

clusterMembers[pts_List, lbl_List] :=
 If[Length[pts] =!= Length[lbl] || pts === {},
  {},
  GatherBy[
   SortBy[Thread[{lbl, pts}], First],
   First][[All, All, 2]]];

Options[AdaptivePreprocess] = {
 "Goal" -> "Clustering",
 "DataKind" -> Automatic,
 "MissingPolicy" -> Automatic,
 "EncodingPolicy" -> Automatic,
 "ScalingPolicy" -> Automatic,
 "OutlierPolicy" -> Automatic,
 "Interpretability" -> "Medium",
 "PerformanceGoal" -> "Quality",
 "MaxRuntimeSeconds" -> 300,
 "Seed" -> 42,
 "ReturnReport" -> True,
 "ArtifactDirectory" -> "adaptive-preprocess-artifacts"
};

AdaptivePreprocess[data_,
  opts : OptionsPattern[AdaptivePreprocess]] :=
 Module[{canonical,
   profile,
   tests,
   decisionPlan,
   fittedPipeline,
   transformed,
   sd},
  sd =
   OptionValue[
    AdaptivePreprocess,
    {opts},
    "Seed"];
  SeedRandom[sd];
  canonical = CanonicalizeData[data];
  If[canonical === $Failed,
   Return[$Failed]];
  profile = ProfileData[canonical];
  tests =
   RunStatTests[canonical, profile];
  decisionPlan =
   BuildDecisionPlan[profile,
    tests];
  fittedPipeline =
   FitPreprocessingPipeline[
    canonical,
    decisionPlan];
  transformed =
   fittedPipeline["TransformFunction"][canonical];
  <|
   "ProcessedData" -> transformed,
   "Preprocessor" -> fittedPipeline,
   "Profile" -> profile,
   "Tests" -> tests,
   "DecisionPlan" -> decisionPlan,
   "Seed" -> sd
   |>];

Options[
  AdaptiveCluster] =
 Join[
  Options[AdaptivePreprocess],
  {"ClusteringMethods" -> Automatic,
   "ClusterNumber" -> Automatic,
   "ClusterRange" -> Range[2, 10],
   "ValidationMetric" ->
    "Silhouette",
   "StabilityRuns" -> 20,
   "DimensionalityReduction" -> Automatic}
 ];

AdaptiveCluster[data_,
  opts : OptionsPattern[AdaptiveCluster]] :=
 Module[{
   prepResult,
   candidates,
   evaluated,
   best,
   report,
   nd,
   lbl},
  prepResult =
   AdaptivePreprocess[data,
    Sequence @@
     FilterRules[
      {opts},
      Options[
       AdaptivePreprocess]]];
  If[! AssociationQ[prepResult],
   Return[$Failed]];
  candidates =
   GenerateClusteringCandidates[
    prepResult["ProcessedData"],
    prepResult[
     "Profile"],
    Sequence @@
     Join[
      FilterRules[
       {opts},
       Options[
        GenerateClusteringCandidates]],
      {
       "ClusterRange" ->
        OptionValue[
         AdaptiveCluster,
         {opts},
         "ClusterRange"],
       "ClusteringMethods" ->
        OptionValue[
         AdaptiveCluster,
         {opts},
         "ClusteringMethods"]
       }]];
  evaluated =
   EvaluateClusterings[
    candidates,
    Sequence @@
     Join[
      FilterRules[{opts},
       Options[EvaluateClusterings]],
      {"StabilityRuns" ->
       OptionValue[
        AdaptiveCluster,
        {opts},
        "StabilityRuns"]}
     ]
   ];
  best = SelectBestClustering[evaluated];
  nd = Lookup[best, "NumericData", {}];
  lbl =
   Lookup[best, "Labels", {}];
  report =
   GenerateAdaptiveReport[
    prepResult,
    best,
    evaluated];
  <|
   "Clusters" -> clusterMembers[nd, lbl],
   "ClusterLabels" -> lbl,
   "ClusterModel" ->
    fitClusterModel[nd, best],
   "Preprocessor" ->
    prepResult["Preprocessor"],
   "Profile" -> prepResult["Profile"],
   "Tests" ->
    prepResult["Tests"],
   "DecisionPlan" ->
    prepResult["DecisionPlan"],
   "ProcessedData" ->
    prepResult["ProcessedData"],
   "Seed" -> prepResult["Seed"],
   "Evaluation" -> evaluated,
   "SelectedMethod" -> Lookup[best, "Method", Missing[]],
   "Report" -> report
   |>];

End[];
EndPackage[];
