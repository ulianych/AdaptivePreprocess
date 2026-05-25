(* ::Package:: *)

BeginPackage["AdaptivePreprocess`Transformers`"];

ApplyMissingStrategy::usage = "ApplyMissingStrategy applies a missing-data policy.";
ApplyEncoding::usage = "ApplyEncoding encodes categorical columns.";
ApplyScaling::usage = "ApplyScaling rescales numeric columns.";
FitPreprocessingPipeline::usage =
 "FitPreprocessingPipeline fits the adaptive transformation pipeline.";

Begin["`Private`"];

Needs["AdaptivePreprocess`Outliers`"];
Needs["AdaptivePreprocess`Utilities`"];

cellOk[v_] :=
 ! MissingQ[v] &&
  ! FailureQ[v];

atomCategoryQ[v_] :=
 cellOk[v] && !AssociationQ[v] &&
  !(MatchQ[v, _SparseArray]);

columnValues[data_, col_] :=
 Module[{rows, v},
  rows =
   AdaptivePreprocess`Utilities`toDatasetRows[data];
  If[rows === {}, {},
   v =
    AdaptivePreprocess`Utilities`cellLookupRow[#,
       col] & /@ rows;
   Cases[
    v,
    x_ /
     cellOk[x]]
 ]
 ];

modeFill[v_List] :=
 Quiet @ Check[
   If[v === {},
    0,
    First[Tally[v]][[1]]],
   0
 ];

imputeWith[data_, col_, fn_] :=
 Module[{values, fill},
  values =
   columnValues[data, col];
  fill =
   If[values === {},
    0,
    Quiet@
     Check[fn[values], 0]
    ];
  fill =
   Replace[fill,
    _Missing ->
     0];
  Dataset[
   Map[
    Function[row,
     Module[{v =
        AdaptivePreprocess`Utilities`cellLookupRow[row,
            col]},
      If[
       MissingQ[v] ||
        FailureQ[v],
       Append[row,
        col ->
         fill],
       row
       ]]],
    AdaptivePreprocess`Utilities`toDatasetRows[
     data]]]];

interpolateColumn[data_, col_] :=
 imputeWith[data, col,
  modeFill];

addMissingIndicator[data_, col_] :=
 Module[{rows, ind, newRows},
 rows =
  AdaptivePreprocess`Utilities`toDatasetRows[data];
 If[rows === {},
  Return[data]];
 ind =
  StringJoin[
   ToString[col],
   "_miss"];
 newRows =
  Map[
   Function[row,
    Append[row,
     ind ->
      Boole@MissingQ@
       AdaptivePreprocess`Utilities`cellLookupRow[row, col]]],
   rows];
 Dataset[newRows]];

ApplyMissingStrategy[data_,
  col_, strategy_String,
  fitState_ : <||>] :=
 Switch[strategy,
  "None", data,
  "DropRow",
  Module[{rows},
   rows =
    AdaptivePreprocess`Utilities`toDatasetRows[data];
   Dataset[
    Select[
     rows,
     !
      MissingQ[
       AdaptivePreprocess`Utilities`cellLookupRow[#,
           col]]
     ]]],
  "DropColumn",
  Module[{rows},
   rows =
    AdaptivePreprocess`Utilities`toDatasetRows[data];
   Dataset[
    KeyDrop[#,
       col] & /@
     rows]],
  "MeanImpute",
  imputeWith[data, col, Mean],
  "MedianImpute",
  imputeWith[data, col, Median],
  "ModeImpute",
  imputeWith[data, col,
   modeFill],
  "InterpolateTime",
  interpolateColumn[data,
   col],
  "AddMissingIndicator",
  addMissingIndicator[data, col],
  _, data];

ApplyEncoding[data_, col_,
  "OneHot"] :=
 Module[{rows, vals,
   cats},
  rows =
   AdaptivePreprocess`Utilities`toDatasetRows[data];
  If[rows === {},
   Return[data]];
  vals =
   AdaptivePreprocess`Utilities`cellLookupRow[#,
       col] & /@ rows;
  cats =
   Cases[
    DeleteDuplicates[vals],
    x_ /
     atomCategoryQ[x]];
  If[cats === {},
   Return[data]];
  Dataset[
   Map[
    Function[row,
     Module[{v =
        AdaptivePreprocess`Utilities`cellLookupRow[row,
            col]},
      Which[
       ! atomCategoryQ[v],
       row,
       True,
       Append[row,
        col ->
         AssociationThread[
          cats,
          Map[
           Function[c,
            Boole[
             SameQ[v, c]]
            ],
           cats]]]
       ]]],
    rows]]];

ApplyEncoding[data_, col_,
  "FrequencyEncoding"] :=
 Module[{rows, vals,
   counts, n},
  rows =
   AdaptivePreprocess`Utilities`toDatasetRows[data];
  If[rows === {},
   Return[data]];
  vals =
   AdaptivePreprocess`Utilities`cellLookupRow[#,
       col] & /@ rows;
  n =
   Length[
    vals];
  counts =
   Counts[
    Cases[
     vals,
     x_ /
      cellOk[x]]];
  Dataset[
   Map[
    Function[row,
     Module[{v =
        AdaptivePreprocess`Utilities`cellLookupRow[row,
            col]},
      Which[
       MissingQ[v] ||
        FailureQ[v],
       row,
       True,
       Append[row,
        col ->
         N[
           Replace[counts[v],
             _Missing -> 0.]/
           Max[
            1,
            n]]]
        ]]],
    rows]]];

ApplyEncoding[data_, col_,
  "HashingEncoding",
  dim_ : 128] :=
 Module[{rows},
  rows =
   AdaptivePreprocess`Utilities`toDatasetRows[data];
  If[rows === {},
   Return[data]];
  Dataset[
   Map[
    Function[row,
     Module[{v =
        AdaptivePreprocess`Utilities`cellLookupRow[row,
            col]},
      If[! cellOk[v],
        row,
        Append[row,
         col ->
           SparseArray[
            {Mod[Hash[v],
                 dim] + 1 ->
              1.},
            dim]]
       ]]],
    rows]]];

ApplyScaling[data_, col_,
  "Standardize"] :=
 Module[{rows, x,
   mu, sigma},
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
  Return[Dataset[
    rows]]];
  mu =
   Mean[x];
  sigma =
   Replace[
    StandardDeviation[x],
    0 -> $MachineEpsilon];
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
           (v - mu)/
            sigma]
        ]]],
    rows]]];

ApplyScaling[data_, col_, "RobustScaling"] :=
 Module[{rows, x, med, iqr},
  rows = AdaptivePreprocess`Utilities`toDatasetRows[data];
  If[rows === {}, Return[data]];
  x = Cases[
    AdaptivePreprocess`Utilities`cellLookupRow[#, col] & /@ rows,
    _?NumericQ];
  If[x === {}, Return[Dataset[rows]]];
  med = Median[x];
  iqr = Replace[
    InterquartileRange[x],
    0 | Indeterminate -> $MachineEpsilon];
  Dataset[
   Map[
    Function[row,
     Module[{v = AdaptivePreprocess`Utilities`cellLookupRow[row, col]},
      If[! NumericQ[v],
       row,
       Append[row, col -> (v - med)/iqr]
       ]
      ]
     ],
    rows
    ]
   ]
  ]

ApplyScaling[data_, col_, "RobustScaling"] :=
 Module[{rows, x, med, iqr},
  rows = AdaptivePreprocess`Utilities`toDatasetRows[data];
  If[rows === {}, Return[data]];
  x = Cases[
    AdaptivePreprocess`Utilities`cellLookupRow[#, col] & /@ rows,
    _?NumericQ];
  If[x === {}, Return[Dataset[rows]]];
  med = Median[x];
  iqr = Replace[
    InterquartileRange[x],
    0 | Indeterminate -> $MachineEpsilon];
  Dataset[
   Map[
    Function[row,
     Module[{v = AdaptivePreprocess`Utilities`cellLookupRow[row, col]},
      If[! NumericQ[v],
       row,
       Append[row, col -> (v - med)/iqr]
       ]
      ]
     ],
    rows
    ]
   ]
  ]

ApplyScaling[data_, col_, "RobustScaling"] :=
 Module[{rows, x, med, iqr},
  rows = AdaptivePreprocess`Utilities`toDatasetRows[data];
  If[rows === {}, Return[data]];
  x = Cases[
    AdaptivePreprocess`Utilities`cellLookupRow[#, col] & /@ rows,
    _?NumericQ];
  If[x === {}, Return[Dataset[rows]]];
  med = Median[x];
  iqr = Replace[
    InterquartileRange[x],
    0 | Indeterminate -> $MachineEpsilon];
  Dataset[
   Map[
    Function[row,
     Module[{v = AdaptivePreprocess`Utilities`cellLookupRow[row, col]},
      If[! NumericQ[v],
       row,
       Append[row, col -> (v - med)/iqr]
       ]
      ]
     ],
    rows
    ]
   ]
  ]

scalingKeysFromPlan[plan_] :=
 Keys@
  plan[
  "ScalingStrategy"];

applyOutliersAll[d_, strat_,
  cols_List] :=
 If[strat === "None" ||
   strat === None, d,
  Fold[
   Function[{dd, col},
    Quiet@
     Check[
      ApplyOutlier[dd,
       col,
       strat],
      dd]],
   d,
   cols]];

runPipeline[data0_, plan_] :=
 Module[{d = data0, cols},
  cols =
   Keys[
    plan[
     "MissingStrategy"]];
  Do[d =
      ApplyMissingStrategy[d,
       c,
       plan[
       "MissingStrategy",
        c]],
     {c, cols}
    ];
 Do[d =
     ApplyEncoding[d, k,
      plan[
       "EncodingStrategy",
       k]],
    {k,
     Keys@
      plan[
      "EncodingStrategy"]}
    ];
 Do[d =
     ApplyScaling[d, k,
      plan[
       "ScalingStrategy",
       k]],
    {k,
     Keys@
      plan[
      "ScalingStrategy"]}
    ];
  d =
    applyOutliersAll[d,
     plan[
     "OutlierStrategy"],
      scalingKeysFromPlan[
       plan]];
  d];

FitPreprocessingPipeline[
 canonical_,
 decisionPlan_, opts___] :=
 <|
  "TransformFunction" ->
   Function[{data},
    runPipeline[
     data,
     decisionPlan]],
  "Plan" -> decisionPlan
 |>;

End[];
EndPackage[];
