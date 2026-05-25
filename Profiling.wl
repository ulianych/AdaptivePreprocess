BeginPackage["AdaptivePreprocess`Profiling`"];

ProfileData::usage =
 "ProfileData[data] returns the profile of input data.";

Begin["`Private`"];

Needs["AdaptivePreprocess`Utilities`"];

outlierRateIQR[x_List] := Module[
 {q1, q3, iqr, lo, hi},
 {q1, q3} = Quantile[x, {0.25, 0.75}];
 iqr = q3 - q1;
 lo = q1 - 1.5 iqr;
 hi = q3 + 1.5 iqr;
 Count[x, _?(# < lo || # > hi &)] / Length[x] // N
];

safeShapiroP[x_List] :=
 If[
  Length[x] < 3 || Length[x] > 5000,
  Missing["NotComputed"],
  Quiet @ Check[N[ShapiroWilkTest[x, "PValue"]], Missing["NotComputed"]]
];

inferColumnType[col_List] := Module[
 {nonMissing, u, n},
 nonMissing = DeleteCases[col, _Missing];
 n = Length[col];
 u = Length[DeleteDuplicates[nonMissing]];
 Which[
  AllTrue[nonMissing, DateObjectQ],
  "Date",
  u == 2,
  "Binary",
  AllTrue[nonMissing, NumericQ] && u/n > 0.05,
  "Numeric",
  AllTrue[nonMissing, StringQ] &&
  Mean[N[StringLength /@ nonMissing]] > 20,
  "Text",
  AllTrue[nonMissing, StringQ] || u/n <= 0.05,
  "Categorical",
  True,
  "Mixed"
 ]
];

numericColumnProfile[col_List] := Module[
 {x, n},
 x = DeleteCases[col, _Missing];
 n = Length[col];
 <|
  "InferredType" -> "Numeric",
  "MissingRate" -> 1 - Length[x] / n // N,
  "Mean" -> If[x === {}, Missing[], Mean[x] // N],
  "Median" -> If[x === {}, Missing[], Median[x] // N],
  "StandardDev" -> If[x === {}, Missing[], StandardDeviation[x] // N],
  "Skewness" -> If[x === {}, Missing[], Skewness[x] // N],
  "Kurtosis" -> If[x === {}, Missing[], Quiet @ Check[Kurtosis[x] // N, Missing[]]],
  "Min" -> If[x === {}, Missing[], Min[x]],
  "Max" -> If[x === {}, Missing[], Max[x]],
  "Q1" -> If[x === {}, Missing[], Quantile[x, 0.25]],
  "Q3" -> If[x === {}, Missing[], Quantile[x, 0.75]],
  "IQR" -> If[x === {}, Missing[], InterquartileRange[x]],
  "OutlierRateIQR" -> If[x === {}, Missing[], outlierRateIQR[x]],
  "NormalityP" -> safeShapiroP[x]
 |>
];

categoricalColumnProfile[col_List] := Module[
 {x, counts, top},
 x = DeleteCases[col, _Missing];
 counts = Counts[x];
 top = Take[ReverseSort[counts], UpTo[10]];
 <|
  "InferredType" -> "Categorical",
  "MissingRate" -> If[Length[col] == 0, 1.,
   1 - Length[x] / Length[col] // N],
  "UniqueCount" -> Length[counts],
  "UniqueRate" ->
   If[Length[x] == 0, Missing[], Length[counts] / Length[x] // N],
  "TopValues" -> top,
  "Entropy" -> If[x === {}, Missing[], Entropy[2, x] // N]
 |>
];

binaryColumnProfile[col_List] :=
 Append[categoricalColumnProfile[col], "InferredType" -> "Binary"];

dateColumnProfile[col_List] := Module[
 {x},
 x = DeleteCases[col, _Missing];
 <|
  "InferredType" -> "Date",
  "MissingRate" -> If[Length[col] == 0, 1.,
   1 - Length[x] / Length[col] // N],
  "MinDate" -> If[x === {}, Missing[], Min[x]],
  "MaxDate" -> If[x === {}, Missing[], Max[x]],
  "RangeDays" ->
   If[Length[x] < 2, Missing[],
    QuantityMagnitude[
     Subtract @@ DateBounds[x]]
    ]
 |>
];

textColumnProfile[col_List] := Module[
 {x},
 x = DeleteCases[col, _Missing];
 <|
  "InferredType" -> "Text",
  "MissingRate" -> If[Length[col] == 0, 1.,
   1 - Length[x] / Length[col] // N],
  "MeanLen" ->
   If[x === {}, Missing[], Mean[N[StringLength /@ x]]],
  "MaxLen" -> If[x === {}, Missing[], Max[StringLength /@ x]]
 |>
];

mixedColumnProfile[col_List] := Module[
 {x},
 x = DeleteCases[col, _Missing];
 <|
  "InferredType" -> "Mixed",
  "MissingRate" -> If[Length[col] == 0, 1.,
   1 - Length[x] / Length[col] // N],
  "UniqueCount" -> Length[DeleteDuplicates[x]]
 |>
];

columnProfile[data_, col_] :=
 Module[{rows,
   colData,
   type},
 rows =
   AdaptivePreprocess`Utilities`toDatasetRows[data];
  colData =
   If[rows =!= {},
    Cases[
     AdaptivePreprocess`Utilities`cellLookupRow[#,
        col] & /@ rows,
     x_ /
      (! MissingQ[x] &&
       ! FailureQ[x])],
    Quiet@
     Check[
      Normal[
       data[[All, col]]],
      {}]];
  type =
   inferColumnType[colData];
  Switch[type,
   "Numeric",
   numericColumnProfile[colData],
   "Categorical",
   categoricalColumnProfile[colData],
   "Binary",
   binaryColumnProfile[colData],
   "Date",
   dateColumnProfile[colData],
   "Text",
   textColumnProfile[colData],
   _,
   mixedColumnProfile[colData]]
 ];

buildGlobalProfile[data_, columnProfiles_Association] := Module[
 {types, ncol, nrow, vals},
 ncol = Length[columnProfiles];
 nrow = Length[data];
 vals = Values[columnProfiles];
 types = Lookup[#, "InferredType", "Mixed"] & /@ vals;
 <|
  "RowCount" -> nrow,
  "ColumnCount" -> ncol,
  "AllNumeric" ->
   MatchQ[types, {Repeated["Numeric"]}],
  "MixedTypes" ->
   !MatchQ[types, {Repeated["Numeric"]}],
  "HighDimensional" -> ncol > 15,
  "HighOutliers" ->
   AnyTrue[vals,
    TrueQ[NumericQ[#["OutlierRateIQR"]] &&
      #["OutlierRateIQR"] > 0.05] &],
  "NonLinear" ->
   (MatchQ[types, {Repeated["Numeric"]}] &&
     ncol === 2 && nrow > 500)
 |>
];

ProfileData[data_, OptionsPattern[]] :=
 Module[{
   columns,
   columnProfiles,
   globalProfile,
   result},
  columns =
   Module[{rows =
      AdaptivePreprocess`Utilities`toDatasetRows[data]},
    Which[
     rows =!= {},
     Keys[
      First[
       rows]],
     ListQ[data] &&
      data =!= {} &&
       ! AssociationQ[
        First[data]],
     Range[
      Length[
       First[data]]],
     True, {}
     ]];
  columnProfiles =
   AssociationMap[
    Function[col,
     columnProfile[data, col]],
    columns];
  globalProfile =
   buildGlobalProfile[data,
    columnProfiles];
  result =
   <|
    "Meta" -> <|
     "CreatedAt" -> DateObject[],
     "Version" -> $Version,
     "Rows" -> Length[data],
     "Columns" -> Length[columns]
     |>,
    "Columns" -> columnProfiles,
    "GlobalProfile" ->
     globalProfile
    |>;
  result];

End[];
EndPackage[];
