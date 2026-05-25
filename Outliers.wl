BeginPackage["AdaptivePreprocess`Outliers`"];

ApplyOutlier::usage =
 "ApplyOutlier[data,column,method] clips or filters numeric outliers.";
DetectAnomalies::usage =
 "DetectAnomalies[data] computes anomaly detector scores.";
DenoiseTimeSeries::usage =
 "DenoiseTimeSeries[ts] applies a denoising filter to a time series.";

Options[DetectAnomalies] = {"AnomalyThreshold" -> 0.01};

Begin["`Private`"];

Needs["AdaptivePreprocess`Utilities`"];

ApplyOutlier[data_, col_, "ClipIQR"] :=
 Module[{rows,
   x, q1, q3,
   iqr, lo,
   hi},
  rows =
   AdaptivePreprocess`Utilities`toDatasetRows[data];
  If[rows === {},
   Return[data]];
  x =
   Cases[
    AdaptivePreprocess`Utilities`cellLookupRow[#,
       col] & /@ rows,
    _?NumericQ];
  If[x === {},
   Return[data]];
  {q1, q3} =
   Quantile[x, {0.25, 0.75}];
  iqr =
   q3 - q1;
  {lo,
   hi} =
   {q1 - 1.5 iqr,
    q3 + 1.5 iqr};
  Dataset[
   Map[
    Function[row,
     Module[{v =
        AdaptivePreprocess`Utilities`cellLookupRow[row,
            col]},
      If[! NumericQ[v],
       row,
       Append[row,
        col ->
         Clip[v, {lo, hi}]]
       ]]],
    rows]]];

ApplyOutlier[data_, col_,
  "ClipMAD", k_ : 3.5] :=
 Module[{rows,
   x, med,
   mad, lo,
   hi},
  rows =
   AdaptivePreprocess`Utilities`toDatasetRows[data];
  If[rows === {},
   Return[data]];
  x =
   Cases[
    AdaptivePreprocess`Utilities`cellLookupRow[#,
       col] & /@ rows,
    _?NumericQ];
 If[x === {},
   Return[data]];
  med =
   Median[x];
  mad =
   Quiet@N[
    MedianDeviation[x]];
  If[! NumericQ[mad] ||
    mad <= 0.,
   mad = $MachineEpsilon];
  {lo,
   hi} =
   {med - k mad,
    med + k mad};
  Dataset[
   Map[
    Function[row,
     Module[{v =
        AdaptivePreprocess`Utilities`cellLookupRow[row,
            col]},
      If[! NumericQ[v],
       row,
       Append[row,
        col ->
         Clip[v, {lo, hi}]]
       ]]],
    rows]]];

DetectAnomalies[data_, opts : OptionsPattern[]] :=
 Module[{detector,
   scores,
   threshold,
   rows},
  rows =
   Which[
    Head[data] === Dataset,
    Normal[data],
    ListQ[data], data,
    True, {data}
   ];
  detector =
   Quiet @
    Check[
     AnomalyDetection[rows,
      AcceptanceThreshold -> 0.01],
     $Failed];
  If[detector === $Failed,
   Return[<|
     "Detector" ->
      Missing[
       "NotComputed"],
     "Scores" -> {},
     "Flags" -> {},
     "Indices" -> {}
     |>]];
  scores =
   Quiet@
    Check[
     detector[#, "RarerProbability"] & /@ rows,
     ConstantArray[
      0.,
      Length[rows]]];
  threshold =
   OptionValue[DetectAnomalies,
    {opts},
    "AnomalyThreshold"];
  With[{flags =
      Boole[
       # <
         threshold
        ] & /@
       scores},
   <|
    "Detector" ->
     detector,
    "Scores" -> scores,
    "Flags" -> flags,
    "Indices" ->
     Position[
       flags,
       1]
      // Flatten
    |>
   ]
 ];

autoMedianFilter[ts_TimeSeries] :=
 Module[{n =
    Length[
     ts],
   window},
  window =
   Max[
    3,
    Round[n/100]];
  MovingMedian[ts, window]];

DenoiseTimeSeries[ts_TimeSeries,
  method_ : "AutoMedian"] :=
 Switch[method,
  "MovingMedian",
  MovingMedian[ts,
   5],
  "MedianFilter",
  MedianFilter[ts,
   5],
  "Wiener",
  WienerFilter[ts],
  "Wavelet",
  InverseWaveletTransform @
    WaveletThreshold[
      DiscreteWaveletTransform[ts,
        "Haar",
        4]],
  "AutoMedian",
  autoMedianFilter[ts],
  _,
  autoMedianFilter[
   ts]];

End[];
EndPackage[];
