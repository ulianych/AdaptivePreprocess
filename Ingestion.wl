BeginPackage["AdaptivePreprocess`Ingestion`"];

CanonicalizeData::usage =
 "CanonicalizeData[data] brings input to a canonical Dataset form.";

CanonicalizeData::unsupported =
 "File extension `1` is not supported for automatic import.";

Begin["`Private`"];

canonicalizeDataset[x_Dataset] := x;

canonicalizeTabular[x_Tabular] := Dataset @ Normal[x];

canonicalizeTimeSeries[x_TimeSeries] := Dataset @ Map[
 <|"t" -> #1, "v" -> #2|> &,
 Transpose[{x["Times"], x["Values"]}]
];

canonicalizeList[xs : {___Association}] := Dataset[xs];
canonicalizeList[xs : {___List}] := Dataset[
 AssociationThread[Range[Length[First[xs]]] -> #] & /@ xs
];

canonicalizeFile[file_String] := Module[
 {ext = ToLowerCase[FileExtension[file]]},
 Switch[ext,
  "csv" | "tsv" | "xlsx" | "json",
  SemanticImport[file, "DropMissing" -> False],
  "wxf" | "m",
  Import[file],
  _,
  Message[CanonicalizeData::unsupported, ext];
  $Failed
 ]
];

CanonicalizeData[data_, OptionsPattern[]] := Switch[
 Head[data],
 Dataset, canonicalizeDataset[data],
 Tabular, canonicalizeTabular[data],
 TimeSeries, canonicalizeTimeSeries[data],
 List, canonicalizeList[data],
 File | String,
 canonicalizeFile[data],
 _, $Failed
];

End[];
EndPackage[];
