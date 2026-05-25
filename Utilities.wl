BeginPackage["AdaptivePreprocess`Utilities`"];

safeP;
safeMetric;
flattenRowNumeric;
toDatasetRows;
cellLookupRow;

Begin["`Private`"];

safeP[expr_] := Quiet @ Check[expr, Missing["NotComputed"]];

safeMetric[f_, args___] := Quiet @ Check[N[f[args]], 0.];

flattenRowNumeric[a_Association] :=
 Module[{kv},
  kv =
   SortBy[
    Keys[a],
    Identity];
  Flatten[
   transformVal /@
    Lookup[
     a,
     kv]]
 ];

transformVal[v_Association] :=
 N[
  Values[v] /. _Missing -> 0.];
transformVal[v_SparseArray] :=
 Normal[v] // N;
transformVal[v_?NumericQ] :=
 {N[v]};
transformVal[v_] :=
 {0.};

goodCell[v_] :=
 ! MissingQ[v] &&
  ! FailureQ[v];

toDatasetRows[data_] :=
 Quiet @
  Replace[
   Which[
    Head[data] === Dataset,
    Normal[data],
    ListQ[data] &&
     (
      data === {} ||
       AssociationQ[data[[1]]]),
    data,
    True,
    {}],
   $Failed -> {}];

cellLookupRow[row_, col_] :=
 If[! AssociationQ[row],
  Missing[],
  Quiet @ Check[Lookup[row, col, Missing[]],
    Missing[]
   ]
 ];

End[];
EndPackage[];
