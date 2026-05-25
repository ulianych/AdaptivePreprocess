BeginPackage["AdaptivePreprocess`Reporting`"];

GenerateAdaptiveReport::usage =
 "GenerateAdaptiveReport builds a notebook report from pipeline results.";
SaveAdaptiveArtifacts::usage =
 "SaveAdaptiveArtifacts exports run artifacts to WXF files.";
LoadAdaptiveArtifacts::usage =
 "LoadAdaptiveArtifacts imports previously saved artifacts.";
ExportReport::usage =
 "ExportReport exports a generated report notebook to HTML or PDF.";

Begin["`Private`"];

reasonFor[step_, col_, decision_] :=
 StringJoin[ToString[step], " / ", ToString[col], " -> ",
  ToString[decision]];

renderDataSummary[prep_Association] :=
 TextCell[
  StringJoin["Rows: ",
   ToString[prep["Profile", "Meta", "Rows"]],
   "  Columns: ",
   ToString[prep["Profile", "Meta", "Columns"]]]];

renderProfile[prof_] := ExpressionCell[Dataset[prof], "Output"];

renderDecisionMap[plan_Association] :=
 ExpressionCell[
  Dataset[
   Flatten[
    Join[
     decisionRow["Missing", #, plan["MissingStrategy", #]] & /@
      Keys[plan["MissingStrategy"]],
     decisionRow["Encoding", #, plan["EncodingStrategy", #]] & /@
      Keys[plan["EncodingStrategy"]],
     decisionRow["Scaling", #, plan["ScalingStrategy", #]] & /@
      Keys[plan["ScalingStrategy"]]
     ]]], "Output"];

decisionRow[step_, col_, decision_] :=
 <|
  "Step" -> step,
  "Column" -> col,
  "Decision" -> decision,
  "Reason" -> reasonFor[step, col, decision]
 |>;

renderTransformations[preproc_] :=
 ExpressionCell[Dataset[preproc["Plan"]], "Output"];

renderClustering[labels_] := ExpressionCell[labels, "Output"];

renderMetrics[eval_] := ExpressionCell[Dataset[eval], "Output"];

GenerateAdaptiveReport[prep_, best_, evaluated_, opts___] :=
 CreateDocument[
  Flatten[{
    TextCell["Adaptive Preprocessing Report", "Title"],
    TextCell["Data summary", "Section"],
    renderDataSummary[prep],
    TextCell["Data profile", "Section"],
    renderProfile[prep["Profile"]],
    TextCell["Decision map", "Section"],
    renderDecisionMap[prep["DecisionPlan"]],
    TextCell["Transformations", "Section"],
    renderTransformations[prep["Preprocessor"]],
    TextCell["Clustering", "Section"],
    renderClustering[
     Lookup[best, "Labels", {}]],
    TextCell["Quality metrics", "Section"],
    renderMetrics[eval]
    }],
  WindowTitle -> "AdaptivePreprocess Report"
 ];

runMetadata[result_Association] :=
 <|
  "Seed" -> Lookup[result, "Seed", Missing[]],
  "Version" -> $Version,
  "Date" -> DateObject[],
  "ProfileHash" -> Quiet @ Hash[result["Profile"]],
  "PipelineHash" ->
   Quiet @ Hash[Lookup[result,
     "DecisionPlan",
     <||>]]
 |>;

SaveAdaptiveArtifacts[result_, dir_String] :=
 Module[{},
  If[! DirectoryQ[dir], CreateDirectory[dir]];
  If[KeyExistsQ[result, "Profile"],
   Export[
    FileNameJoin[{dir, "profile.wxf"}],
    result["Profile"]]];
  If[KeyExistsQ[result, "Tests"],
   Export[
    FileNameJoin[{dir, "tests.wxf"}],
    result["Tests"]]];
  If[KeyExistsQ[result, "DecisionPlan"],
   Export[
    FileNameJoin[{dir, "decision-plan.wxf"}],
    result["DecisionPlan"]]];
  If[KeyExistsQ[result, "Preprocessor"],
   Export[
    FileNameJoin[{dir, "preprocessor.wxf"}],
    result["Preprocessor"]]];
  If[KeyExistsQ[result, "ProcessedData"],
   Export[
    FileNameJoin[{dir, "processed.wxf"}],
    result["ProcessedData"]]];
  Export[
   FileNameJoin[{dir, "metadata.wxf"}],
   runMetadata[result]];
 ];

LoadAdaptiveArtifacts[dir_String] :=
 <|
  "Profile" -> Import[FileNameJoin[{dir, "profile.wxf"}]],
  "Tests" -> Import[FileNameJoin[{dir, "tests.wxf"}]],
  "DecisionPlan" ->
   Import[
    FileNameJoin[{dir, "decision-plan.wxf"}]],
  "Preprocessor" ->
   Import[
    FileNameJoin[{dir, "preprocessor.wxf"}]],
  "ProcessedData" ->
   Import[FileNameJoin[{dir, "processed.wxf"}]],
  "Metadata" -> Import[FileNameJoin[{dir, "metadata.wxf"}]]
 |>;

ExportReport[notebook_NotebookObject, file_String] :=
 Switch[ToLowerCase[FileExtension[file]],
  "html", Export[file, notebook, "HTML"],
  "pdf", Export[file, notebook, "PDF"],
  _, $Failed
 ];

End[];
EndPackage[];
