#include "../../src/core/linkeda_document.h"

#include <algorithm>
#include <cassert>
#include <fstream>
#include <string>

using namespace rlispstat::core;

int main(int argc, char **argv)
{
    DataFrameModel data;
    data.group = "study";
    data.rows = 3;
    data.columns = {
        {"score", "numeric", "Score", "Edited numeric score", 2,
         {"1.25", "NA", "3.5"}, {"1.25", "", "3.50"}},
        {"arm", "factor", "Treatment arm", "Randomized arm", -1,
         {"control", "treated", ""}, {"Control", "Treated", ""},
         {"control", "treated"}},
        {"comment", "string", "Comment", "Added in LinkEDA", -1,
         {"first", "", "NA"}}
    };
    data.imputationId = "mice-1";
    data.sourceDatasetId = "study-source";
    data.imputationCount = 2;
    data.activeImputationVersion = 2;
    data.imputationDisplayMode = "all";
    data.imputationProcess = "retained-process-fixture";
    data.columns[0].imputedMissing = {false, true, false};
    data.columns[0].imputationOriginalValues = {"1.25", "NA", "3.5"};
    data.columns[0].imputationValues = {{"1.25", "2.0", "3.5"},
                                        {"1.25", "2.2", "3.5"}};
    DataColumn reversibleGender;
    reversibleGender.name = "gender_code";
    reversibleGender.type = "numeric";
    reversibleGender.values = {"0", "1", "0"};
    reversibleGender.reversibleFactorLevels = {"Female", "Male"};
    reversibleGender.storageType = "double";
    reversibleGender.numericMapping = {{"Female", "0"}, {"Male", "1"}};
    reversibleGender.reversibleCategoryType = "factor";
    data.columns.push_back(reversibleGender);

    ApplicationState typeOnlyState;
    assert(typeOnlyState.registerDataset(data));
    VariableTypeChangeEffects typeEffects;
    std::string typeMessage;
    assert(typeOnlyState.setVariableType("study", "comment", "factor",
                                         typeEffects, &typeMessage));
    assert(typeOnlyState.groupModels().empty());

    DataFrameModel binaryData;
    binaryData.group = "binary-study";
    binaryData.rows = 3;
    DataColumn binaryGender;
    binaryGender.name = "gender";
    binaryGender.type = "factor";
    binaryGender.storageType = "factor";
    binaryGender.values = {"Female", "Male", "Female"};
    binaryGender.displayValues = binaryGender.values;
    binaryGender.definedLevels = {"Female", "Male"};
    binaryData.columns = {binaryGender};
    ApplicationState binaryTypeState;
    assert(binaryTypeState.registerDataset(binaryData));
    assert(binaryTypeState.setVariableType("binary-study", "gender", "numeric",
                                           typeEffects, &typeMessage));
    const DataFrameModel *numericBinary = binaryTypeState.datasets().find("binary-study");
    assert(numericBinary);
    assert(numericBinary->columns[0].values ==
           std::vector<std::string>({"0", "1", "0"}));
    assert(binaryTypeState.setVariableType("binary-study", "gender", "factor",
                                           typeEffects, &typeMessage));
    const DataFrameModel *restoredBinary = binaryTypeState.datasets().find("binary-study");
    assert(restoredBinary);
    assert(restoredBinary->columns[0].displayValues ==
           std::vector<std::string>({"Female", "Male", "Female"}));
    assert(restoredBinary->provenance.history.size() == 2);
    assert(restoredBinary->provenance.history.back().rCode.find("category_labels") !=
           std::string::npos);
    LinkEDADataDocument binaryTypeDocument;
    std::string binaryTypeError;
    assert(CreateLinkEDADataDocument(binaryTypeState, "binary-study",
                                     binaryTypeDocument, &binaryTypeError));
    const auto binaryTypeBytes = EncodeLinkEDADataDocument(binaryTypeDocument,
                                                            &binaryTypeError);
    assert(!binaryTypeBytes.empty());
    LinkEDADataDocument decodedBinaryType;
    assert(DecodeLinkEDADataDocument(binaryTypeBytes, decodedBinaryType,
                                     &binaryTypeError));
    assert(decodedBinaryType.dataset.columns[0].displayValues ==
           std::vector<std::string>({"Female", "Male", "Female"}));
    assert(decodedBinaryType.dataset.provenance.history.size() == 2);
    assert(decodedBinaryType.dataset.provenance.history.back().rCode.find(
               "category_labels") != std::string::npos);

    ApplicationState state;
    assert(state.registerDataset(data));
    assert(state.restoreVariableRoles("study", {{"score", "dependent"},
                                                 {"arm", "independent"},
                                                 {"comment", "none"}}));
    // Default roles are dataset metadata. They must not manufacture or mutate
    // a live linear-model specification.
    assert(state.groupModels().empty());
    GroupModelState &localModel = state.groupModels()["study"];
    localModel.group = "study";
    localModel.response = "score";
    localModel.terms = {"arm"};
    assert(state.setDefaultVariableRole("study", "score", "none"));
    assert(localModel.response == "score");
    assert(localModel.terms == std::vector<std::string>({"arm"}));
    assert(state.setDefaultVariableRole("study", "score", "dependent"));
    assert(state.setPointColor("study", 2, "#2A9D8F"));
    assert(state.setSelectedRows("study", {1, 2}));
    assert(state.setSelectedColor("study", "#FF0000"));
    assert(state.setLabelColumn("study", "comment"));
    AnalysisScope scope = ExplicitAnalysisScope(
        "study", {1, 3}, AnalysisScopeSourceKind::OtherExplicitSubset,
        "Complete cases", 3);
    assert(state.setActiveAnalysisScope(scope));
    SavedSelection saved{"study", "Review", {2, 3},
                         AnalysisScopeSourceKind::DataTableRows, std::nullopt};
    assert(state.replaceSavedSelections("study", {saved}));

    WindowNote note = MakeWindowNote("data-sheet-study");
    SetWindowNoteText(note, "Check the edited score and imputed value.");
    SetWindowNoteIncludedInExport(note, true);
    assert(state.setDocumentNote("study", note));

    OutputCodeReference tableOutput;
    tableOutput.outputId = "table1-study";
    tableOutput.analysisId = "table1-analysis";
    tableOutput.outputBlockId = "table";
    tableOutput.title = "Table 1";
    tableOutput.kind = "table";
    tableOutput.provenance.analysisId = tableOutput.analysisId;
    tableOutput.provenance.title = tableOutput.title;
    tableOutput.provenance.dataVersion = DefaultDataVersionReference(
        "study", "multiple_imputation", 2, 1);
    tableOutput.provenance.scope = CaptureImmutableAnalysisScope(
        scope, tableOutput.provenance.dataVersion,
        StableRowIdsForCount("study", 3));
    tableOutput.provenance.codeOrigin = RCodeOrigin::Recorded;
    tableOutput.provenance.executedRCode =
        "table1_result <- list(display_table = data.frame(N = 3L))";
    tableOutput.provenance.outputRCode["table"] =
        "table_values <- table1_result$display_table";
    tableOutput.provenance.verificationRCode["table"] =
        "analysis_data <- base::readRDS(verification_data_path)";
    tableOutput.provenance.verificationVariables = {"score", "arm"};
    tableOutput.provenance.verificationWarnings = {"Prepared columns only."};
    state.registerOutputCodeReference(tableOutput);
    OutputCodeReference secondOutput = tableOutput;
    secondOutput.outputId = "table1-study-copy";
    secondOutput.analysisId = "table1-analysis-copy";
    secondOutput.provenance.analysisId = secondOutput.analysisId;
    secondOutput.kind = "plot";
    PublicationPlotSpec persistedEffect;
    persistedEffect.kind = "effect";
    persistedEffect.xCategoryOrder = {"1", "2"};
    persistedEffect.showConfidenceIntervals = true;
    persistedEffect.showLines = false;
    persistedEffect.showAxisTickMarks = false;
    persistedEffect.showAxisTickLabels = false;
    persistedEffect.theme = "minimal";
    persistedEffect.series = {{"all", "Estimate", {1.0, 2.0}, {2.0, 3.0},
                               {1.5, 2.4}, {2.6, 3.7}}};
    secondOutput.publication.plot = persistedEffect;
    secondOutput.publication.availableBackends = {PublicationBackend::Ggplot2};
    state.registerOutputCodeReference(secondOutput);

    DataFrameModel edited = *state.datasets().find("study");
    edited.columns[0].values[2] = "4.5";
    TransformationStep edit;
    edit.label = "Edit score";
    edit.origin = RCodeOrigin::Unavailable;
    edit.inputColumns = {"score"};
    edit.outputColumns = {"score"};
    RecordDataFrameTransformation(edited, std::move(edit));
    assert(edited.dataVersion == 2);
    assert(state.registerDataset(edited));
    const DataFrameModel *historical = state.datasetVersion(
        tableOutput.provenance.dataVersion);
    assert(historical && historical->columns[0].values[2] == "3.5");
    std::string error;
    assert(state.setPointColor("study", 1, "#E76F51"));
    assert(state.saveCurrentColorsAsScheme("study", "Review colors", &error));

    LinkEDADataDocument document;
    assert(CreateLinkEDADataDocument(state, "study", document, &error));
    document.excluded_rows = {2};
    // Both outputs refer to one immutable version, which is serialized once.
    assert(document.output_code_references.size() == 2);
    assert(document.shared_data_versions.size() == 1);
    assert(document.shared_data_versions[0].dataVersion == 1);
    const auto dataBaseline = EncodeLinkEDADataChangePayload(document, &error);
    assert(!dataBaseline.empty());
    LinkEDADataDocument analysisOnly = document;
    analysisOnly.output_code_references.clear();
    analysisOnly.shared_data_versions.clear();
    analysisOnly.row_colors.clear();
    analysisOnly.excluded_rows.clear();
    analysisOnly.saved_selections.clear();
    analysisOnly.color_schemes.clear();
    analysisOnly.analysis_scope = AllObservationsAnalysisScope("study", 3);
    analysisOnly.point_label_column.clear();
    analysisOnly.data_sheet_note.reset();
    analysisOnly.dataset.activeImputationVersion = 1;
    analysisOnly.dataset.imputationDisplayMode = "active";
    assert(EncodeLinkEDADataChangePayload(analysisOnly, &error) == dataBaseline);
    analysisOnly.dataset.columns[0].values[0] = "9.25";
    assert(EncodeLinkEDADataChangePayload(analysisOnly, &error) != dataBaseline);
    analysisOnly.dataset.columns[0].values[0] = document.dataset.columns[0].values[0];
    analysisOnly.variable_roles["arm"] = "dependent";
    assert(EncodeLinkEDADataChangePayload(analysisOnly, &error) != dataBaseline);
    LinkEDADataDocument legacyDocument = document;
    legacyDocument.version = 3;
    auto legacyBytes = EncodeLinkEDADataDocument(legacyDocument, &error);
    assert(!legacyBytes.empty());
    LinkEDADataDocument decodedLegacy;
    assert(DecodeLinkEDADataDocument(legacyBytes, decodedLegacy, &error));
    const auto legacyPlot = std::find_if(
        decodedLegacy.output_code_references.begin(),
        decodedLegacy.output_code_references.end(),
        [](const OutputCodeReference &reference) {
            return reference.outputId == "table1-study-copy";
        });
    assert(legacyPlot != decodedLegacy.output_code_references.end());
    assert(legacyPlot->publication.plot);
    assert(legacyPlot->publication.plot->showAxisTickMarks);
    assert(legacyPlot->publication.plot->showAxisTickLabels);
    assert(legacyPlot->publication.plot->theme == "bw");
    assert(decodedLegacy.dataset.columns[3].reversibleFactorLevels.empty());
    auto bytes = EncodeLinkEDADataDocument(document, &error);
    assert(!bytes.empty());
    if (argc > 1) {
        std::ofstream payload(argv[1], std::ios::binary);
        payload.write(reinterpret_cast<const char *>(bytes.data()),
                      static_cast<std::streamsize>(bytes.size()));
    }
    if (argc > 2) {
        std::ofstream script(argv[2], std::ios::binary);
        script << NativeLinkEDAWriteRScript();
    }
    if (argc > 3) {
        std::ofstream script(argv[3], std::ios::binary);
        script << NativeLinkEDAReadRScript();
    }

    LinkEDADataDocument decoded;
    assert(DecodeLinkEDADataDocument(bytes, decoded, &error));
    assert(decoded.format == "LinkEDA");
    assert(decoded.version == kLinkEDADataDocumentVersion);
    assert(decoded.dataset.imputationProcess == data.imputationProcess);
    assert(std::string(kLinkEDADataDocumentSchema) == "LinkEDADataDocument/v10");
    assert(decoded.excluded_rows == std::vector<int>({2}));
    assert(decoded.color_schemes.size() == 1);
    assert(decoded.color_schemes[0].name == "Review colors");
    assert(decoded.color_schemes[0].scopeStableRowIds.size() == 2);
    assert(decoded.dataset.columns.size() == 4);
    assert(decoded.dataset.columns[0].values[1] == "NA");
    assert(decoded.dataset.columns[1].values[2].empty());
    assert(decoded.dataset.columns[2].values[2] == "NA");
    assert(decoded.dataset.columns[1].definedLevels ==
           std::vector<std::string>({"control", "treated"}));
    assert(decoded.dataset.columns[0].imputationValues[1][1] == "2.2");
    assert(decoded.dataset.columns[3].reversibleFactorLevels ==
           std::vector<std::string>({"Female", "Male"}));
    assert(decoded.dataset.columns[3].storageType == "double");
    assert(decoded.dataset.columns[3].numericMapping.at("Female") == "0");
    assert(decoded.dataset.columns[3].reversibleCategoryType == "factor");
    DataColumn decodedReversible = decoded.dataset.columns[3];
    assert(SetDataColumnType(decodedReversible, "factor", &error));
    assert(decodedReversible.displayValues ==
           std::vector<std::string>({"Female", "Male", "Female"}));
    assert(decoded.variable_roles.at("score") == "dependent");
    assert((decoded.row_colors ==
            std::vector<std::pair<int, std::string>>({{1, "#E76F51"},
                                                       {2, "#2A9D8F"}})));
    assert(decoded.analysis_scope.originalRowIds == std::vector<int>({1, 3}));
    assert(decoded.saved_selections[0].name == "Review");
    assert(decoded.point_label_column == "comment");
    assert(decoded.data_sheet_note && decoded.data_sheet_note->plain_text == note.plain_text);
    assert(!decoded.data_sheet_note->visible);
    assert(decoded.output_code_references.size() == 2);
    assert(decoded.shared_data_versions.size() == 1);
    assert(decoded.output_code_references[0].provenance.executedRCode ==
           tableOutput.provenance.executedRCode);
    assert(decoded.output_code_references[0].provenance.outputRCode.at("table") ==
           tableOutput.provenance.outputRCode.at("table"));
    assert(decoded.output_code_references[0].provenance.verificationRCode.at("table") ==
           tableOutput.provenance.verificationRCode.at("table"));
    assert(decoded.output_code_references[0].provenance.verificationVariables ==
           tableOutput.provenance.verificationVariables);
    const auto decodedPlot = std::find_if(
        decoded.output_code_references.begin(), decoded.output_code_references.end(),
        [](const OutputCodeReference &reference) {
            return reference.outputId == "table1-study-copy";
        });
    assert(decodedPlot != decoded.output_code_references.end());
    assert(decodedPlot->publication.plot);
    assert(decodedPlot->publication.plot->xCategoryOrder ==
           std::vector<std::string>({"1", "2"}));
    assert(decodedPlot->publication.plot->showConfidenceIntervals);
    assert(!decodedPlot->publication.plot->showLines);
    assert(!decodedPlot->publication.plot->showAxisTickMarks);
    assert(!decodedPlot->publication.plot->showAxisTickLabels);
    assert(decodedPlot->publication.plot->theme == "minimal");

    // Closing the live data sheet does not invalidate an output's version.
    assert(state.eraseDataset("study"));
    historical = state.datasetVersion(tableOutput.provenance.dataVersion);
    assert(historical && historical->columns[0].values[2] == "3.5");
    assert(state.eraseOutputCodeReference("table1-study"));
    assert(state.datasetVersion(tableOutput.provenance.dataVersion));
    assert(state.eraseOutputCodeReference("table1-study-copy"));
    assert(!state.datasetVersion(tableOutput.provenance.dataVersion));

    ApplicationState restored;
    assert(ApplyLinkEDADataDocument(decoded, restored, &error));
    const DataFrameModel *restoredData = restored.datasets().find("study");
    assert(restoredData && restoredData->columns[0].values[1] == "NA");
    assert(restoredData->columns[1].type == "factor");
    assert(restoredData->columns[1].definedLevels ==
           std::vector<std::string>({"control", "treated"}));
    assert(restored.pointColors("study") == decoded.row_colors);
    assert(restored.labelColumn("study") == "comment");
    assert(restored.activeAnalysisScope("study").originalRowIds == std::vector<int>({1, 3}));
    assert((restored.excludedRows("study") == std::set<int>{2}));
    assert(restored.savedSelections("study").size() == 1);
    assert(restored.savedSelection("study", "Review")->originalRowIds ==
           std::vector<int>({2, 3}));
    assert(restored.activateSavedSelection("study", "Review"));
    assert(restored.resolveActiveAnalysisRowIds("study") ==
           std::vector<int>({3}));
    assert(restored.setExcludedRows("study", {}));
    assert(restored.resolveActiveAnalysisRowIds("study") ==
           std::vector<int>({2, 3}));
    assert(restored.colorSchemes("study").size() == 1);
    assert(restored.variableRoles("study").at("arm") == "independent");
    assert(restored.documentNote("study") && restored.documentNote("study")->has_content);
    const OutputCodeReference *restoredOutput =
        restored.outputCodeReference("table1-study");
    assert(restoredOutput);
    assert(restoredOutput->provenance.executedRCode ==
           tableOutput.provenance.executedRCode);
    assert(restoredOutput->provenance.outputRCode.at("table") ==
           tableOutput.provenance.outputRCode.at("table"));
    assert(restoredOutput->provenance.verificationRCode.at("table") ==
           tableOutput.provenance.verificationRCode.at("table"));
    const DataFrameModel *restoredHistorical = restored.datasetVersion(
        tableOutput.provenance.dataVersion);
    assert(restoredHistorical && restoredHistorical->columns[0].values[2] == "3.5");
    std::set<int> transientSelection;
    assert(restored.selectedRows("study", transientSelection) && transientSelection.empty());
    assert(restored.selectedColor("study") == "black");

    LinkEDADataDocument inconsistent = decoded;
    inconsistent.dataset.columns[0].values.pop_back();
    assert(!ApplyLinkEDADataDocument(inconsistent, restored, &error));
    assert(restored.datasets().find("study") &&
           restored.datasets().find("study")->columns[0].values.size() == 3);

    auto corrupted = bytes;
    corrupted.resize(corrupted.size() / 2);
    assert(!DecodeLinkEDADataDocument(corrupted, decoded, &error));
    LinkEDADataDocument future = document;
    future.version = 99;
    assert(!ValidateLinkEDADataDocument(future, &error));
    assert(NativeLinkEDAWriteRScript().find("saveRDS") != std::string::npos);
    assert(NativeLinkEDAReadRScript().find("readRDS") != std::string::npos);
    assert(NativeLinkEDAWriteRScript().find("version=10L") != std::string::npos);
    assert(NativeLinkEDAReadRScript().find("document$version > 10L") !=
           std::string::npos);
    return 0;
}
