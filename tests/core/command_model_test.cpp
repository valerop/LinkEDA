#include "../../src/core/command_model.h"

#include <cassert>
#include <string>
#include <vector>

using rlispstat::core::CommandCategory;
using rlispstat::core::CommandAction;
using rlispstat::core::CommandActionName;
using rlispstat::core::CommandCategoryName;
using rlispstat::core::CommandHasName;
using rlispstat::core::CommandIsDatasetCommand;
using rlispstat::core::CommandModelSpec;
using rlispstat::core::CommandRequiresPlot;
using rlispstat::core::AnalyzeCommandMenuGroups;
using rlispstat::core::ActiveDatasetCommandOptions;
using rlispstat::core::AutoRefitMenuTitle;
using rlispstat::core::DataColumnCommandOptions;
using rlispstat::core::DataPointsColorPaletteCommandOption;
using rlispstat::core::DataRefreshCommandOption;
using rlispstat::core::DataSheetImputationVersionTitle;
using rlispstat::core::DataVariableInformationCommandOption;
using rlispstat::core::DataVariableTypeCommandOptions;
using rlispstat::core::DataWindowCommandOptions;
using rlispstat::core::DefaultApplicationMenuTitles;
using rlispstat::core::DefaultDataSheetContextMenuTitles;
using rlispstat::core::DefaultPlotContextMenuTitles;
using rlispstat::core::DefaultVariableViewContextMenuTitles;
using rlispstat::core::DefaultModelContextMenuTitles;
using rlispstat::core::DecodeCommandField;
using rlispstat::core::EditCommandOptions;
using rlispstat::core::EncodeCommandField;
using rlispstat::core::EncodeCommandModelSpecTable;
using rlispstat::core::FileCommandOptionGroups;
using rlispstat::core::FileCommandOptions;
using rlispstat::core::IsDataColumnCommandName;
using rlispstat::core::IsLinkedPlotCommandName;
using rlispstat::core::IsNativeUiDispatchCommandName;
using rlispstat::core::LinkedPlotCommandOptions;
using rlispstat::core::ModelTermTypeOptions;
using rlispstat::core::ModelDiagnosticPlotOptions;
using rlispstat::core::NoRegisteredDatasetsTitle;
using rlispstat::core::ParseCommandRequest;
using rlispstat::core::ParseMenuCommand;
using rlispstat::core::PlotCommandOptions;
using rlispstat::core::TrellisPlotStartCommandOptions;
using rlispstat::core::PlotExportCommandOptionGroups;
using rlispstat::core::PredictorTypeMenuTitle;
using rlispstat::core::ProtocolSafeValue;
using rlispstat::core::RecordCommandOptionGroups;
using rlispstat::core::SelectionCommandOptions;
using rlispstat::core::TermTypeMenuTitle;
using rlispstat::core::VariableViewRoleOptions;
using rlispstat::core::VariableViewTypeOptions;

int main()
{
    std::string rawField = "a|b%c\nd\te";
    std::string encodedField = EncodeCommandField(rawField);
    assert(encodedField == "a%7Cb%25c%0Ad%09e");
    assert(DecodeCommandField(encodedField) == rawField);
    assert(DecodeCommandField("keep%ZZliteral") == "keep%ZZliteral");
    assert(ProtocolSafeValue("a|b\nc\td") == "a b c d");
    auto menuTitles = DefaultApplicationMenuTitles();
    assert(menuTitles.application == "LinkEDA");
    assert(menuTitles.quitApplication == "Quit LinkEDA");
    assert(menuTitles.quitKeyEquivalent == "q");
    assert(menuTitles.file == "File");
    assert(menuTitles.data == "Data");
    assert(menuTitles.activeDataset == "Active Dataset");
    assert(menuTitles.variableType == "Variable Type");
    assert(menuTitles.colorSelectedPoints == "Color Selected Points");
    auto plotContextTitles = DefaultPlotContextMenuTitles();
    assert(plotContextTitles.root == "LinkEDA");
    assert(plotContextTitles.variables == "Variables");
    assert(plotContextTitles.mouseMode == "Mouse mode");
    assert(plotContextTitles.selection == "Selection");
    assert(plotContextTitles.colorSelectedPoints == "Color selected points");
    assert(plotContextTitles.colorSelectedRows == "Color Selected Rows");
    assert(plotContextTitles.openColorPalette == "Open color palette");
    assert(plotContextTitles.addDataColumn == "Add Column to Data");
    assert(plotContextTitles.theme == "Theme");
    assert(plotContextTitles.analyzeThisHistogram == "Analyze This Histogram");
    assert(plotContextTitles.plot == "Plot");
    assert(plotContextTitles.exportMenu == "Export");
    auto dataSheetTitles = DefaultDataSheetContextMenuTitles();
    assert(dataSheetTitles.root == "Rows");
    assert(dataSheetTitles.colorSelectedRows == "Color Selected Rows");
    assert(dataSheetTitles.clearRowColor == "Clear Row Color");
    assert(dataSheetTitles.clearAllRowColors == "Clear All Row Colors");
    assert(dataSheetTitles.variableView == "Variable View");
    assert(dataSheetTitles.treatColumnAsText == "Treat Column as Text");
    assert(dataSheetTitles.showVariableInformation == "Show Variable Information");
    assert(dataSheetTitles.imputedDataDisplay == "Imputed Data Display");
    assert(dataSheetTitles.allImputedValuesDisplay == "All imputed values separated by |");
    assert(dataSheetTitles.originalIncompleteDataDisplay == "Original incomplete data");
    assert(DataSheetImputationVersionTitle(3) == "Imputation 3");
    assert(DataSheetImputationVersionTitle(0) == "Imputation 1");
    auto variableViewTitles = DefaultVariableViewContextMenuTitles();
    assert(variableViewTitles.root == "Variable");
    assert(variableViewTitles.role == "Role");
    assert(variableViewTitles.type == "Type");
    assert(variableViewTitles.editDescription == "Edit Description...");
    auto modelMenuTitles = DefaultModelContextMenuTitles();
    assert(modelMenuTitles.model == "Model");
    assert(modelMenuTitles.modelComparison == "Model Comparison");
    assert(modelMenuTitles.addTermAction == "Add term...");
    assert(modelMenuTitles.refitModel == "Refit Model");
    assert(modelMenuTitles.openDiagnostics == "Open Diagnostics");
    assert(modelMenuTitles.showCoefficientDetails == "Show coefficient details");
    assert(modelMenuTitles.changeType == "Change type...");
    assert(modelMenuTitles.responseVariable == "Response Variable");
    assert(modelMenuTitles.predictorTerms == "Predictor Terms");
    assert(modelMenuTitles.addRemoveTermInModel == "Add/Remove term in this model");
    assert(modelMenuTitles.removeTermFromAllModels == "Remove term from all models");
    assert(modelMenuTitles.copyRegressionTable == "Copy Regression Table");
    assert(modelMenuTitles.openDiagnosticPlot == "Open diagnostic plot");
    assert(modelMenuTitles.interpretInteraction == "Interpret interaction...");
    assert(modelMenuTitles.partialRegressionPlot == "Partial regression plot");
    assert(modelMenuTitles.noAvailableInteractions == "No available interactions");
    assert(modelMenuTitles.copyModelComparisonTable == "Copy Model Comparison Table");
    assert(modelMenuTitles.openSingleGLM == "Open this model as single GLM window");
    assert(modelMenuTitles.openAsSingleGeneralizedGLM == "Open as single generalized GLM window");
    assert(AutoRefitMenuTitle(true) == "Turn Auto-refit Off");
    assert(AutoRefitMenuTitle(false) == "Turn Auto-refit On");
    auto variableRoleOptions = VariableViewRoleOptions();
    assert(variableRoleOptions.size() == 3);
    assert(variableRoleOptions[0].title == "Dependent");
    assert(variableRoleOptions[1].value == "independent");
    assert(variableRoleOptions[2].value == "none");
    auto variableTypeOptions = VariableViewTypeOptions();
    assert(variableTypeOptions.size() == 4);
    assert(variableTypeOptions[0].value == "numeric");
    assert(variableTypeOptions[2].title == "Ordered factor");
    assert(variableTypeOptions[3].value == "character");
    auto modelTermTypeOptions = ModelTermTypeOptions();
    assert(modelTermTypeOptions.size() == 2);
    assert(modelTermTypeOptions[0].title == "Numeric");
    assert(modelTermTypeOptions[0].value == "numeric");
    assert(modelTermTypeOptions[1].title == "Factor");
    assert(modelTermTypeOptions[1].value == "factor");
    auto basicDiagnosticOptions = ModelDiagnosticPlotOptions(false);
    assert(basicDiagnosticOptions.size() == 2);
    assert(basicDiagnosticOptions[0].title == "Observed vs fitted");
    assert(basicDiagnosticOptions[0].value == "observed_fitted");
    assert(basicDiagnosticOptions[1].value == "residuals_fitted");
    auto fullDiagnosticOptions = ModelDiagnosticPlotOptions(true);
    assert(fullDiagnosticOptions.size() == 7);
    assert(fullDiagnosticOptions[2].value == "residual_histogram");
    assert(fullDiagnosticOptions[3].title == "Normal Q-Q of residuals");
    assert(fullDiagnosticOptions[6].value == "cooks_distance");
    assert(PredictorTypeMenuTitle() == "Predictor type");
    assert(TermTypeMenuTitle() == "Term type");
    auto fileOptions = FileCommandOptions();
    assert(fileOptions.size() == 5);
    assert(fileOptions[0].command == "FILE_IMPORT_DATA");
    assert(fileOptions[0].keyEquivalent == "o");
    assert(fileOptions[0].commandModifier);
    assert(!fileOptions[0].shiftModifier);
    assert(fileOptions[1].command == "FILE_OPEN_DATA_FROM_R");
    assert(fileOptions[2].command == "FILE_RETURN_DATA_TO_R");
    assert(fileOptions[3].command == "FILE_RETURN_SELECTED_ROWS_TO_R");
    assert(fileOptions[4].command == "FILE_CANCEL_DATA_RETURN");
    auto fileGroups = FileCommandOptionGroups();
    assert(fileGroups.size() == 2);
    assert(fileGroups[0].size() == 2);
    assert(fileGroups[1].size() == 3);
    auto editOptions = EditCommandOptions();
    assert(editOptions.size() == 3);
    assert(editOptions[0].command == "EDIT_COPY");
    assert(editOptions[1].keyEquivalent == "C");
    assert(editOptions[1].commandModifier);
    assert(editOptions[1].shiftModifier);
    auto recordGroups = RecordCommandOptionGroups();
    assert(recordGroups.size() == 4);
    assert(recordGroups[0][0].command == "RECORD_START");
    assert(recordGroups[1][1].value == "replay");
    assert(recordGroups[2][2].title == "Copy Internal Commands");
    assert(recordGroups[3][0].command == "RECORD_CLEAR");
    auto analyzeGroups = AnalyzeCommandMenuGroups();
    assert(analyzeGroups.size() == 4);
    assert(analyzeGroups[0].title == "Descriptives");
    assert(analyzeGroups[0].options[0].command == "ANALYZE_TABLE1");
    assert(analyzeGroups[0].options[1].command == "ANALYZE_CONTINGENCY_TABLE");
    assert(analyzeGroups[1].options[3].command == "ANALYZE_ONEWAY_ANOVA");
    assert(analyzeGroups[1].title == "Test");
    assert(analyzeGroups[2].title == "Regression");
    assert(analyzeGroups[2].options[0].keyEquivalent == "g");
    assert(analyzeGroups[2].options[0].commandModifier);
    assert(analyzeGroups[2].options[1].title == "Linear Model Trellis...");
    assert(analyzeGroups[2].options[1].command == "ANALYZE_LINEAR_MODEL_TRELLIS");
    assert(analyzeGroups[2].options[2].shiftModifier);
    assert(analyzeGroups[2].options[3].command == "ANALYZE_BINARY_REGRESSION");
    assert(analyzeGroups[2].options[4].command == "ANALYZE_BINARY_REGRESSION_COMPARISON");
    assert(analyzeGroups[3].options[1].command == "ANALYZE_GENERALIZED_MIXED_MODEL");
    assert(!analyzeGroups[3].options[0].experimental);
    assert(analyzeGroups[3].options[1].experimental);
    auto plotMenuOptions = PlotCommandOptions();
    assert(plotMenuOptions.size() == 8);
    assert(plotMenuOptions[0].command == "PLOT_NEW_LINKED_SCATTERPLOT");
    assert(plotMenuOptions[0].keyEquivalent == "n");
    assert(plotMenuOptions[0].commandModifier);
    assert(plotMenuOptions[1].keyEquivalent.empty());
    assert(plotMenuOptions[1].value == "trellis_scatterplot");
    assert(plotMenuOptions[1].title == "Trellis Plots");
    auto trellisStartOptions = TrellisPlotStartCommandOptions();
    assert(trellisStartOptions.size() == 5);
    assert(trellisStartOptions[0].command == "PLOT_NEW_TRELLIS_SCATTERPLOT|scatter");
    assert(trellisStartOptions[1].command == "PLOT_NEW_TRELLIS_SCATTERPLOT|time_series");
    assert(trellisStartOptions[4].command == "PLOT_NEW_TRELLIS_SCATTERPLOT|histogram");
    assert(plotMenuOptions[2].value == "time_series");
    assert(plotMenuOptions[4].value == "parallel_coordinates");
    assert(plotMenuOptions[7].value == "bar_chart");
    auto activeDatasetOptions = ActiveDatasetCommandOptions({"cars", "iris"});
    assert(activeDatasetOptions.size() == 2);
    assert(activeDatasetOptions[0].title == "cars");
    assert(activeDatasetOptions[0].command == "DATA_SET_ACTIVE_DATASET|cars");
    assert(activeDatasetOptions[1].value == "iris");
    assert(NoRegisteredDatasetsTitle() == "No Registered Datasets");
    auto linkedPlotOptions = LinkedPlotCommandOptions();
    assert(linkedPlotOptions.size() == 8);
    assert(linkedPlotOptions[0].command == "PLOT_NEW_LINKED_SCATTERPLOT");
    assert(linkedPlotOptions[1].title == "Trellis Plots");
    assert(linkedPlotOptions[2].title == "Time Series...");
    assert(linkedPlotOptions[3].title == "Scatterplot Matrix...");
    assert(linkedPlotOptions[4].command == "PLOT_NEW_PARALLEL_COORDINATES");
    assert(linkedPlotOptions[7].value == "bar_chart");
    auto compactSelectionOptions = SelectionCommandOptions(false);
    assert(compactSelectionOptions.size() == 2);
    assert(compactSelectionOptions[0].command == "CLEAR_SELECTION");
    assert(compactSelectionOptions[1].command == "INVERT_SELECTION");
    auto fullSelectionOptions = SelectionCommandOptions(true);
    assert(fullSelectionOptions.size() == 3);
    assert(fullSelectionOptions[1].command == "SELECT_ALL_VISIBLE");
    assert(IsLinkedPlotCommandName("PLOT_NEW_LINKED_SCATTERPLOT"));
    assert(IsLinkedPlotCommandName("PLOT_NEW_SCATTERPLOT"));
    assert(IsLinkedPlotCommandName("PLOT_NEW_TIME_SERIES"));
    assert(IsLinkedPlotCommandName("PLOT_NEW_TRELLIS_SCATTERPLOT"));
    assert(IsLinkedPlotCommandName("PLOT_NEW_LINKED_BARCHART"));
    assert(!IsLinkedPlotCommandName("ANALYZE_TABLE1"));
    auto exportGroups = PlotExportCommandOptionGroups(false);
    assert(exportGroups.size() == 2);
    assert(exportGroups[0][0].command == "COPY_PDF");
    assert(exportGroups[0][1].command == "COPY_SVG");
    assert(exportGroups[0][2].command == "COPY_PNG");
    assert(exportGroups[1][0].command == "SAVE_SVG");
    assert(exportGroups[1][1].title == "Save as PDF...");
    assert(exportGroups[1][2].command == "SAVE_PNG");
    auto selectedExportGroups = PlotExportCommandOptionGroups(true);
    assert(selectedExportGroups.size() == 3);
    assert(selectedExportGroups[2][0].command == "COPY_SELECTED");
    auto dataColumnOptions = DataColumnCommandOptions();
    assert(dataColumnOptions.size() == 2);
    assert(dataColumnOptions[0].title == "Selection: selected / not selected");
    assert(dataColumnOptions[0].command == "DATA_ADD_SELECTION_COLUMN");
    assert(dataColumnOptions[1].command == "DATA_ADD_POINT_COLOR_COLUMN");
    auto groupedDataColumnOptions = DataColumnCommandOptions("cars");
    assert(groupedDataColumnOptions[0].command == "DATA_ADD_SELECTION_COLUMN|cars");
    assert(groupedDataColumnOptions[1].command == "DATA_ADD_POINT_COLOR_COLUMN|cars");
    assert(IsDataColumnCommandName("DATA_ADD_SELECTION_COLUMN"));
    assert(IsDataColumnCommandName("DATA_ADD_POINT_COLOR_COLUMN"));
    assert(!IsDataColumnCommandName("DATA_POINTS_COLOR"));
    assert(IsNativeUiDispatchCommandName("DATA_CLOSE_DATA_SHEET"));
    assert(IsNativeUiDispatchCommandName("FILE_IMPORT_DATA"));
    assert(IsNativeUiDispatchCommandName("FILE_RETURN_DATA_TO_R"));
    assert(IsNativeUiDispatchCommandName("FILE_OPEN_DATA_FROM_R"));
    assert(IsNativeUiDispatchCommandName("FILE_RETURN_SELECTED_ROWS_TO_R"));
    assert(IsNativeUiDispatchCommandName("FILE_CANCEL_DATA_RETURN"));
    assert(IsNativeUiDispatchCommandName("DATA_REFRESH_R_DATAFRAMES"));
    assert(IsNativeUiDispatchCommandName("DATA_SHOW_ACTIVE_DATASET"));
    assert(IsNativeUiDispatchCommandName("DATA_SET_ACTIVE_DATASET"));
    assert(IsNativeUiDispatchCommandName("DATA_CHOOSE_LABEL_COLUMN"));
    assert(IsNativeUiDispatchCommandName("DATA_VARIABLE_VIEW"));
    assert(IsNativeUiDispatchCommandName("DATA_POINTS_COLOR"));
    assert(IsNativeUiDispatchCommandName("DATA_ADD_SELECTION_COLUMN"));
    assert(IsNativeUiDispatchCommandName("PLOT_NEW_LINKED_SCATTERPLOT"));
    assert(IsNativeUiDispatchCommandName("ANALYZE_TABLE1"));
    assert(IsNativeUiDispatchCommandName("ANALYZE_GENERALIZED_COMPARISON"));
    assert(IsNativeUiDispatchCommandName("OPEN_DENDROGRAM"));
    assert(!IsNativeUiDispatchCommandName("DATA_OPEN_DATA_SHEET"));
    assert(!IsNativeUiDispatchCommandName("RECORD_START"));
    auto dataWindowOptions = DataWindowCommandOptions();
    assert(dataWindowOptions.size() == 4);
    assert(dataWindowOptions[0].command == "DATA_SHOW_ACTIVE_DATASET");
    assert(dataWindowOptions[1].title == "Open Data Sheet");
    assert(dataWindowOptions[1].command == "DATA_OPEN_DATA_SHEET");
    assert(dataWindowOptions[2].title == "Variable View");
    assert(dataWindowOptions[3].command == "DATA_CHOOSE_LABEL_COLUMN");
    auto dataVariableTypeOptions = DataVariableTypeCommandOptions();
    assert(dataVariableTypeOptions.size() == 2);
    assert(dataVariableTypeOptions[0].value == "numeric");
    assert(dataVariableTypeOptions[1].title == "Treat active X variable as Factor");
    assert(DataPointsColorPaletteCommandOption().command == "DATA_POINTS_COLOR");
    assert(DataPointsColorPaletteCommandOption().title == "Open Data Points Color Palette...");
    assert(DataVariableInformationCommandOption().command == "DATA_SHOW_VARIABLE_INFORMATION");
    assert(DataRefreshCommandOption().value == "refresh_r_dataframes");

    std::string encodedModels = EncodeCommandModelSpecTable({
        CommandModelSpec{"Model|1", "y", {"x", "a:b"}},
        CommandModelSpec{"Model 2", "response\t2", {"z\nq"}}
    });
    assert(encodedModels == "Model%7C1|y|x\ta:b\nModel 2|response%092|z%0Aq");

    auto empty = ParseCommandRequest(std::vector<std::string>());
    assert(empty.name.empty());
    assert(empty.category == CommandCategory::Unknown);

    auto ping = ParseCommandRequest({"PING"});
    assert(CommandHasName(ping, "PING"));
    assert(ping.args.empty());
    assert(ping.category == CommandCategory::Protocol);
    assert(ping.action == CommandAction::Ping);
    assert(CommandCategoryName(ping.category) == "protocol");
    assert(CommandActionName(ping.action) == "ping");

    auto active = ParseCommandRequest({"SET_ACTIVE_DATASET", "cars"});
    assert(active.name == "SET_ACTIVE_DATASET");
    assert(active.args.size() == 1);
    assert(active.args[0] == "cars");
    assert(CommandIsDatasetCommand(active));
    assert(active.action == CommandAction::SetActiveDataset);
    assert(!CommandRequiresPlot(active.category));

    auto menuActive = ParseMenuCommand("DATA_SET_ACTIVE_DATASET|cars");
    assert(menuActive.action == CommandAction::DataSetActiveDataset);
    assert(menuActive.args.size() == 1);
    assert(menuActive.args[0] == "cars");

    auto nativeImport = ParseCommandRequest({"NATIVE_IMPORT_FILE", "/tmp/data.csv"});
    assert(nativeImport.category == CommandCategory::Dataset);
    assert(nativeImport.action == CommandAction::NativeImportFile);
    assert(nativeImport.args.size() == 1);

    auto plotInfo = ParseCommandRequest({"PLOT_INFO", "plot_1"});
    assert(plotInfo.category == CommandCategory::Protocol);
    assert(plotInfo.action == CommandAction::PlotInfo);

    auto groups = ParseCommandRequest({"GROUPS"});
    assert(groups.action == CommandAction::Groups);
    assert(CommandActionName(groups.action) == "groups");

    auto xvar = ParseCommandRequest({"GET_XVAR", "plot_1"});
    assert(xvar.action == CommandAction::GetXVariable);

    auto setVariableType = ParseCommandRequest({"SET_VARIABLE_TYPE", "cars", "cyl", "factor"});
    assert(setVariableType.category == CommandCategory::Dataset);
    assert(setVariableType.action == CommandAction::SetVariableType);

    auto useSavedScope = ParseMenuCommand("USE_SAVED_ANALYSIS_SCOPE|cars|High mileage cars");
    assert(useSavedScope.category == CommandCategory::Selection);
    assert(useSavedScope.action == CommandAction::UseSavedAnalysisScope);
    assert(useSavedScope.args[1] == "High mileage cars");
    auto addSavedScope = ParseMenuCommand("ADD_SAVED_ANALYSIS_SCOPE|cars|Odd cars");
    assert(addSavedScope.action == CommandAction::AddSavedAnalysisScope);
    auto subsetCurrent = ParseMenuCommand("DATA_MAKE_SUBSET_FROM_SELECTION|cars");
    assert(subsetCurrent.category == CommandCategory::Dataset);
    assert(subsetCurrent.action == CommandAction::MakeSubsetFromSelection);
    auto subsetSaved = ParseMenuCommand("DATA_MAKE_SUBSET_FROM_SAVED_SELECTION|cars|Odd cars");
    assert(subsetSaved.action == CommandAction::MakeSubsetFromSavedSelection);
    auto getSavedScopes = ParseCommandRequest({"GET_SAVED_ANALYSIS_SCOPES", "cars"});
    assert(getSavedScopes.category == CommandCategory::Protocol);
    assert(getSavedScopes.action == CommandAction::GetSavedAnalysisScopes);

    auto registerDataset = ParseCommandRequest({"REGISTER_DATASET", "cars", "DATAFRAME", "0"});
    assert(registerDataset.category == CommandCategory::Dataset);
    assert(registerDataset.action == CommandAction::RegisterDataset);
    assert(CommandActionName(registerDataset.action) == "register_dataset");

    auto registerSilent = ParseCommandRequest({"REGISTER_DATASET_SILENT", "cars", "DATAFRAME", "0"});
    assert(registerSilent.action == CommandAction::RegisterDatasetSilent);

    auto panel = ParseCommandRequest({"PANEL", "show"});
    assert(panel.category == CommandCategory::Window);
    assert(panel.action == CommandAction::Panel);

    auto panelShow = ParseCommandRequest({"PANEL_SHOW"});
    assert(panelShow.category == CommandCategory::Window);
    assert(panelShow.action == CommandAction::PanelShow);

    auto resetLayout = ParseCommandRequest({"RESET_WINDOW_LAYOUT"});
    assert(resetLayout.category == CommandCategory::Window);
    assert(resetLayout.action == CommandAction::ResetWindowLayout);

    auto modelInfo = ParseCommandRequest({"MODEL_INFO", "cars"});
    assert(modelInfo.category == CommandCategory::Model);
    assert(modelInfo.action == CommandAction::ModelInfo);

    auto modelSetY = ParseCommandRequest({"MODEL_SET_Y", "cars", "mpg"});
    assert(modelSetY.category == CommandCategory::Model);
    assert(modelSetY.action == CommandAction::ModelSetY);
    assert(modelSetY.args.size() == 2);
    assert(modelSetY.args[1] == "mpg");

    auto modelAddTerm = ParseCommandRequest({"MODEL_ADD_TERM", "cars", "wt"});
    assert(modelAddTerm.action == CommandAction::ModelAddTerm);

    auto modelDiagnostic = ParseCommandRequest({"MODEL_OPEN_DIAGNOSTIC", "cars", "residuals_fitted"});
    assert(modelDiagnostic.action == CommandAction::ModelOpenDiagnostic);
    assert(CommandActionName(modelDiagnostic.action) == "model_open_diagnostic");

    auto interactionReport = ParseCommandRequest({"MODEL_INTERACTION_REPORT", "cars", "wt%3Acyl"});
    assert(interactionReport.action == CommandAction::ModelInteractionReport);

    auto selected = ParseCommandRequest({"SET_SELECTED", "cars", "2", "1", "3"});
    assert(selected.category == CommandCategory::Selection);
    assert(selected.action == CommandAction::SetSelectedRows);
    assert(CommandActionName(selected.action) == "set_selected_rows");
    assert(CommandRequiresPlot(selected.category));

    auto selectedRows = ParseCommandRequest({"SELECTED", "cars"});
    assert(selectedRows.action == CommandAction::SelectedRows);

    auto selectAllRows = ParseCommandRequest({"SELECT_ALL", "cars"});
    assert(selectAllRows.action == CommandAction::SelectAllRows);

    auto invertRows = ParseCommandRequest({"INVERT", "cars"});
    assert(invertRows.action == CommandAction::InvertSelection);

    auto interactionMode = ParseCommandRequest({"MODE", "plot_1", "select"});
    assert(interactionMode.category == CommandCategory::Plot);
    assert(interactionMode.action == CommandAction::InteractionMode);

    auto selectionMode = ParseCommandRequest({"SELECTION_MODE", "plot_1", "add"});
    assert(selectionMode.category == CommandCategory::Selection);
    assert(selectionMode.action == CommandAction::SelectionMode);

    auto selectionOperation = ParseCommandRequest({"SELECTION_OPERATION", "plot_1", "toggle"});
    assert(selectionOperation.action == CommandAction::SelectionOperation);

    auto menu = ParseMenuCommand("CHANGE_X_VARIABLE|wt");
    assert(menu.name == "CHANGE_X_VARIABLE");
    assert(menu.args.size() == 1);
    assert(menu.args[0] == "wt");
    assert(menu.category == CommandCategory::Plot);

    auto equivalentActive = ParseMenuCommand("OPEN_EQUIVALENT_WITH_ACTIVE_SCOPE");
    assert(equivalentActive.category == CommandCategory::Analysis);
    assert(equivalentActive.action == CommandAction::OpenEquivalentWithActiveScope);
    assert(CommandActionName(equivalentActive.action) == "open_equivalent_with_active_scope");
    auto equivalentAll = ParseMenuCommand("OPEN_EQUIVALENT_WITH_ALL_ROWS");
    assert(equivalentAll.action == CommandAction::OpenEquivalentWithAllRows);

    auto barplot = ParseMenuCommand("BARPLOT_REPLACE_X|old|new");
    assert(barplot.name == "BARPLOT_REPLACE_X");
    assert(barplot.args.size() == 2);
    assert(barplot.args[0] == "old");
    assert(barplot.args[1] == "new");
    assert(barplot.category == CommandCategory::Plot);
    assert(barplot.action == CommandAction::BarplotReplaceX);

    auto setX = ParseCommandRequest({"SET_XVAR", "plot_1", "wt"});
    assert(setX.category == CommandCategory::Plot);
    assert(setX.action == CommandAction::ChangeXVariable);

    auto addLm = ParseCommandRequest({"ADD_LM", "plot_1", "color"});
    assert(addLm.action == CommandAction::AddLm);
    assert(CommandActionName(addLm.action) == "add_lm");

    auto overlays = ParseCommandRequest({"OVERLAYS", "plot_1"});
    assert(overlays.action == CommandAction::Overlays);

    auto newScatter = ParseMenuCommand("PLOT_NEW_LINKED_SCATTERPLOT");
    assert(newScatter.action == CommandAction::NewScatterplot);
    auto newTimeSeries = ParseMenuCommand("PLOT_NEW_TIME_SERIES");
    assert(newTimeSeries.action == CommandAction::NewTimeSeries);
    auto setTimeSeriesGroup = ParseMenuCommand("TIME_SERIES_SET_GROUP|cyl");
    assert(setTimeSeriesGroup.action == CommandAction::SetTimeSeriesGroup);
    assert(setTimeSeriesGroup.args == std::vector<std::string>{"cyl"});

    auto addPlot = ParseCommandRequest({"ADD_PLOT", "plot_1", "cars", "wt", "mpg", "0"});
    assert(addPlot.category == CommandCategory::Plot);
    assert(addPlot.action == CommandAction::AddPlot);
    assert(CommandActionName(addPlot.action) == "add_plot");

    auto addBoxplot = ParseCommandRequest({"ADD_BOXPLOT", "box_1", "cars"});
    assert(addBoxplot.action == CommandAction::AddBoxplot);

    auto addHistogram = ParseCommandRequest({"ADD_HISTOGRAM", "hist_1", "cars"});
    assert(addHistogram.action == CommandAction::AddHistogram);

    auto addBarplot = ParseCommandRequest({"ADD_BARPLOT", "bar_1", "cars"});
    assert(addBarplot.action == CommandAction::AddBarplot);

    auto scatterMatrixAdd = ParseMenuCommand("SCATTER_MATRIX_ADD_VARIABLE|hp");
    assert(scatterMatrixAdd.action == CommandAction::ScatterMatrixAddVariable);
    assert(scatterMatrixAdd.args.size() == 1);
    assert(scatterMatrixAdd.args[0] == "hp");

    auto scatterMatrixReplace = ParseMenuCommand("SCATTER_MATRIX_REPLACE_VARIABLE|1|qsec");
    assert(scatterMatrixReplace.action == CommandAction::ScatterMatrixReplaceVariable);
    assert(scatterMatrixReplace.args.size() == 2);
    assert(scatterMatrixReplace.args[0] == "1");
    assert(scatterMatrixReplace.args[1] == "qsec");
    assert(CommandActionName(scatterMatrixReplace.action) == "scatter_matrix_replace_variable");

    auto newBarchart = ParseMenuCommand("PLOT_NEW_LINKED_BARCHART");
    assert(newBarchart.action == CommandAction::NewBarChart);

    auto rowColors = ParseMenuCommand("BARPLOT_ROW_COLORS|bar_fill");
    assert(rowColors.action == CommandAction::BarplotRowColors);
    assert(rowColors.args.size() == 1);
    assert(rowColors.args[0] == "bar_fill");
    assert(CommandActionName(rowColors.action) == "barplot_row_colors");

    auto stroke = ParseMenuCommand("BARPLOT_SPLIT_STROKE_WIDTH|1.5");
    assert(stroke.action == CommandAction::BarplotSplitStrokeWidth);
    assert(stroke.args.size() == 1);
    assert(stroke.args[0] == "1.5");

    auto encoding = ParseMenuCommand("BARPLOT_SEGMENT_ENCODING|pattern_only");
    assert(encoding.action == CommandAction::BarplotSegmentEncoding);
    assert(encoding.args.size() == 1);
    assert(encoding.args[0] == "pattern_only");

    auto selectionDisplay = ParseMenuCommand("BARPLOT_SELECTION_DISPLAY|overlay_outline");
    assert(selectionDisplay.action == CommandAction::BarplotSelectionDisplay);
    assert(selectionDisplay.args.size() == 1);
    assert(selectionDisplay.args[0] == "overlay_outline");

    auto mode = ParseMenuCommand("BARPLOT_MODE|conditional_percent");
    assert(mode.action == CommandAction::BarplotMode);
    assert(mode.args.size() == 1);
    assert(mode.args[0] == "conditional_percent");

    auto width = ParseMenuCommand("BARPLOT_WIDTH|proportional_n");
    assert(width.action == CommandAction::BarplotWidth);
    assert(width.args.size() == 1);
    assert(width.args[0] == "proportional_n");

    auto segmentSelect = ParseMenuCommand("BARPLOT_SEGMENT_SELECT|2|1");
    assert(segmentSelect.action == CommandAction::BarplotSegmentSelect);
    assert(segmentSelect.args.size() == 2);
    assert(segmentSelect.args[0] == "2");
    assert(segmentSelect.args[1] == "1");

    auto segmentColor = ParseMenuCommand("BARPLOT_SEGMENT_SET_COLOR|2|1|orange");
    assert(segmentColor.action == CommandAction::BarplotSegmentSetColor);
    assert(segmentColor.args.size() == 3);
    assert(segmentColor.args[2] == "orange");

    auto levelPattern = ParseMenuCommand("BARPLOT_LEVEL_SET_PATTERN|level%7Cencoded|stripe");
    assert(levelPattern.action == CommandAction::BarplotLevelSetPattern);
    assert(levelPattern.args.size() == 2);
    assert(levelPattern.args[0] == "level%7Cencoded");
    assert(levelPattern.args[1] == "stripe");

    auto segmentDetails = ParseMenuCommand("BARPLOT_SEGMENT_DETAILS|0|3");
    assert(segmentDetails.action == CommandAction::BarplotSegmentDetails);
    assert(CommandActionName(segmentDetails.action) == "barplot_segment_details");

    auto contextModel = ParseMenuCommand("CONTEXT_LINEAR_MODEL_XY");
    assert(contextModel.category == CommandCategory::Analysis);
    assert(contextModel.action == CommandAction::ContextLinearModelXY);

    auto table1 = ParseMenuCommand("OPEN_TABLE1");
    assert(table1.category == CommandCategory::Analysis);
    assert(table1.action == CommandAction::OpenTable1);

    auto correlation = ParseMenuCommand("ANALYZE_CORRELATION_MATRIX");
    assert(correlation.category == CommandCategory::Analysis);
    assert(correlation.action == CommandAction::OpenCorrelationMatrix);
    assert(CommandActionName(correlation.action) == "open_correlation_matrix");

    auto corrOpen = ParseCommandRequest({"CORR_OPEN", "corr_1", "cars", "pearson", "pairwise", "TRUE", "FALSE", "TRUE", "2", "mpg", "wt"});
    assert(corrOpen.category == CommandCategory::Analysis);
    assert(corrOpen.action == CommandAction::CorrelationOpen);
    assert(CommandActionName(corrOpen.action) == "correlation_open");

    auto corrStructured = ParseCommandRequest({"CORR_OPEN_STRUCTURED", "corr_1"});
    assert(corrStructured.action == CommandAction::CorrelationOpenStructured);

    auto corrSetVariables = ParseCommandRequest({"CORR_SET_VARIABLES", "corr_1", "2", "mpg", "wt"});
    assert(corrSetVariables.action == CommandAction::CorrelationSetVariables);

    auto corrInfo = ParseCommandRequest({"CORR_INFO", "corr_1"});
    assert(corrInfo.action == CommandAction::CorrelationInfo);

    auto pcafaOpen = ParseCommandRequest({"PCAFA_OPEN", "pc_1", "cars", "pca", "listwise", "TRUE", "2", "varimax", "all", "3", "mpg", "disp", "wt"});
    assert(pcafaOpen.category == CommandCategory::Analysis);
    assert(pcafaOpen.action == CommandAction::DimensionalityOpen);
    assert(CommandActionName(pcafaOpen.action) == "dimensionality_open");

    auto pcafaSetVariables = ParseCommandRequest({"PCAFA_SET_VARIABLES", "pc_1", "2", "mpg", "wt"});
    assert(pcafaSetVariables.action == CommandAction::DimensionalitySetVariables);

    auto pcafaInfo = ParseCommandRequest({"PCAFA_INFO", "pc_1"});
    assert(pcafaInfo.action == CommandAction::DimensionalityInfo);

    auto dendroOpen = ParseCommandRequest({"DENDRO_OPEN", "den_1", "cars", "euclidean", "complete", "listwise", "2", "mpg", "wt"});
    assert(dendroOpen.action == CommandAction::DendrogramOpen);

    auto dendroSetVariables = ParseCommandRequest({"DENDRO_SET_VARIABLES", "den_1", "2", "mpg", "wt"});
    assert(dendroSetVariables.action == CommandAction::DendrogramSetVariables);

    auto dendroInfo = ParseCommandRequest({"DENDRO_INFO", "den_1"});
    assert(dendroInfo.action == CommandAction::DendrogramInfo);

    auto table1Structured = ParseCommandRequest({"TABLE1_OPEN_STRUCTURED", "tbl_1", "cars"});
    assert(table1Structured.action == CommandAction::Table1OpenStructured);

    auto table1Text = ParseCommandRequest({"TABLE1_OPEN", "tbl_1", "cars", "1", "hello"});
    assert(table1Text.action == CommandAction::Table1OpenText);

    auto compareMeans = ParseCommandRequest({"COMPARE_MEANS_OPEN", "cm_1", "cars"});
    assert(compareMeans.action == CommandAction::CompareMeansOpen);
    auto compareMeansBatch = ParseCommandRequest({"COMPARE_MEANS_BATCH_OPEN", "COMPARE_MEANS_BATCH_V1", "cm_2"});
    assert(compareMeansBatch.action == CommandAction::CompareMeansBatchOpen);
    assert(CommandActionName(compareMeans.action) == "compare_means_open");

    auto contingency = ParseMenuCommand("ANALYZE_CONTINGENCY_TABLE");
    assert(contingency.action == CommandAction::AnalyzeContingencyTable);
    assert(CommandActionName(contingency.action) == "analyze_contingency_table");

    auto boxplotOrder = ParseCommandRequest({"BOXPLOT_GROUP_ORDER", "box_1", "median_desc"});
    assert(boxplotOrder.action == CommandAction::BoxplotGroupOrder);
    assert(CommandActionName(boxplotOrder.action) == "boxplot_group_order");

    auto glm = ParseMenuCommand("ANALYZE_GLM");
    assert(glm.category == CommandCategory::Analysis);
    assert(glm.action == CommandAction::OpenGLM);

    auto modelTrellis = ParseMenuCommand("ANALYZE_LINEAR_MODEL_TRELLIS");
    assert(modelTrellis.category == CommandCategory::Analysis);
    assert(modelTrellis.action == CommandAction::OpenLinearModelTrellis);
    assert(CommandActionName(modelTrellis.action) == "open_linear_model_trellis");

    auto glmCommand = ParseCommandRequest({"GLM", "plot_1"});
    assert(glmCommand.category == CommandCategory::Model);
    assert(glmCommand.action == CommandAction::OpenGLM);

    auto regcmp = ParseMenuCommand("OPEN_REGRESSION_COMPARISON");
    assert(regcmp.action == CommandAction::OpenRegressionComparison);

    auto glz = ParseMenuCommand("ANALYZE_GENERALIZED_GLM");
    assert(glz.action == CommandAction::OpenGeneralizedGLM);

    auto glzOpen = ParseCommandRequest({"GENERALIZED_GLM_OPEN", "cars", "vs", "binomial", "logit", "1", "mpg"});
    assert(glzOpen.action == CommandAction::GeneralizedGLMOpen);

    auto glzPooled = ParseCommandRequest({"GENERALIZED_GLM_OPEN_POOLED", "glz_1"});
    assert(glzPooled.action == CommandAction::GeneralizedGLMOpenPooled);

    auto glzDiagnostic = ParseCommandRequest({"GENERALIZED_GLM_OPEN_DIAGNOSTIC", "glz_1", "residuals_fitted"});
    assert(glzDiagnostic.action == CommandAction::GeneralizedGLMOpenDiagnostic);

    auto generalizedComparison = ParseMenuCommand("OPEN_GENERALIZED_COMPARISON");
    assert(generalizedComparison.action == CommandAction::OpenGeneralizedComparison);

    auto independentT = ParseMenuCommand("ANALYZE_INDEPENDENT_T");
    assert(independentT.action == CommandAction::AnalyzeIndependentT);

    auto missingData = ParseMenuCommand("ANALYZE_MISSING_DATA_IMPUTATION");
    assert(missingData.action == CommandAction::AnalyzeMissingDataImputation);

    auto mixed = ParseMenuCommand("ANALYZE_LINEAR_MIXED_MODEL");
    assert(mixed.action == CommandAction::AnalyzeLinearMixedModel);

    auto mixedStructured = ParseCommandRequest({"MIXED_MODEL_OPEN_STRUCTURED", "mix_1", "cars"});
    assert(mixedStructured.action == CommandAction::MixedModelOpenStructured);

    auto mixedText = ParseCommandRequest({"MIXED_MODEL_OPEN_TEXT", "mix_1", "cars", "linear_mixed_model", "1", "text"});
    assert(mixedText.action == CommandAction::MixedModelOpenText);

    auto selectedColor = ParseMenuCommand("SET_SELECTED_COLOR|blue");
    assert(selectedColor.category == CommandCategory::Selection);
    assert(selectedColor.action == CommandAction::SetSelectedColor);
    assert(selectedColor.args.size() == 1);
    assert(selectedColor.args[0] == "blue");

    auto getSelectedColor = ParseCommandRequest({"GET_SELECTED_COLOR", "cars"});
    assert(getSelectedColor.action == CommandAction::GetSelectedColor);

    auto setPointColor = ParseCommandRequest({"SET_POINT_COLOR", "cars", "orange", "2", "1", "3"});
    assert(setPointColor.action == CommandAction::SetPointColor);
    assert(CommandActionName(setPointColor.action) == "set_point_color");

    auto clearRowColors = ParseCommandRequest({"CLEAR_ROW_COLORS", "cars"});
    assert(clearRowColors.action == CommandAction::ClearRowColors);

    auto labelDisplay = ParseMenuCommand("SET_LABEL_DISPLAY|selected");
    assert(labelDisplay.category == CommandCategory::Plot);
    assert(labelDisplay.action == CommandAction::SetLabelDisplay);
    assert(labelDisplay.args.size() == 1);
    assert(labelDisplay.args[0] == "selected");

    auto boxplotToggle = ParseMenuCommand("BOXPLOT_TOGGLE_POINTS");
    assert(boxplotToggle.category == CommandCategory::Plot);
    assert(boxplotToggle.action == CommandAction::BoxplotTogglePoints);

    auto boxplotAdd = ParseMenuCommand("BOXPLOT_ADD_VARIABLE|qsec");
    assert(boxplotAdd.action == CommandAction::BoxplotAddVariable);
    assert(boxplotAdd.args.size() == 1);
    assert(boxplotAdd.args[0] == "qsec");

    auto parallelCoordinates = ParseMenuCommand("PLOT_NEW_PARALLEL_COORDINATES");
    assert(parallelCoordinates.action == CommandAction::NewParallelCoordinates);
    assert(CommandActionName(parallelCoordinates.action) == "new_parallel_coordinates");

    auto boxplotReplace = ParseMenuCommand("BOXPLOT_REPLACE_VARIABLE|mpg|qsec");
    assert(boxplotReplace.action == CommandAction::BoxplotReplaceVariable);
    assert(boxplotReplace.args.size() == 2);
    assert(boxplotReplace.args[0] == "mpg");
    assert(boxplotReplace.args[1] == "qsec");

    auto boxplotSplit = ParseCommandRequest({"BOXPLOT_SPLIT_VIOLIN", "box_1", "TRUE", "two.sided", "1", "2"});
    assert(boxplotSplit.action == CommandAction::BoxplotSplitViolin);

    auto boxplotH0 = ParseCommandRequest({"BOXPLOT_H0_SIMULATION", "box_1", "TRUE", "0", "two.sided", "1000"});
    assert(boxplotH0.action == CommandAction::BoxplotH0Simulation);

    auto boxplotOptions = ParseCommandRequest({"BOXPLOT_OPTIONS", "box_1"});
    assert(boxplotOptions.action == CommandAction::BoxplotOptions);

    auto boxplotOption = ParseCommandRequest({"BOXPLOT_OPTION", "box_1", "points", "TRUE"});
    assert(boxplotOption.action == CommandAction::BoxplotOption);

    auto histogramMode = ParseMenuCommand("HIST_SET_DENSITY_MODE|colors");
    assert(histogramMode.action == CommandAction::HistogramSetDensityMode);
    assert(histogramMode.args.size() == 1);
    assert(histogramMode.args[0] == "colors");

    auto histogramBreaks = ParseCommandRequest({"HIST_BREAKS", "hist_1"});
    assert(histogramBreaks.action == CommandAction::HistogramBreaks);

    auto histogramBins = ParseCommandRequest({"HIST_SET_BINS", "hist_1", "12"});
    assert(histogramBins.action == CommandAction::HistogramSetBins);

    auto histogramRule = ParseCommandRequest({"HIST_SET_BINNING_RULE", "hist_1", "fd"});
    assert(histogramRule.action == CommandAction::HistogramSetBinningRule);

    auto histogramDensityInfo = ParseCommandRequest({"HIST_DENSITY_INFO", "hist_1"});
    assert(histogramDensityInfo.action == CommandAction::HistogramDensityInfo);

    auto histogramShowDensity = ParseCommandRequest({"HIST_SHOW_DENSITY", "hist_1", "TRUE"});
    assert(histogramShowDensity.action == CommandAction::HistogramShowDensity);

    auto biplotX = ParseMenuCommand("PCA_BIPLOT_SET_X|3");
    assert(biplotX.action == CommandAction::BiplotSetX);
    assert(biplotX.args.size() == 1);
    assert(biplotX.args[0] == "3");

    auto imputationUncertainty = ParseMenuCommand("SET_IMPUTATION_UNCERTAINTY|iqr");
    assert(imputationUncertainty.action == CommandAction::SetImputationUncertainty);

    auto changeY = ParseMenuCommand("CHANGE_Y_VARIABLE|mpg");
    assert(changeY.action == CommandAction::ChangeYVariable);
    assert(changeY.args.size() == 1);
    assert(changeY.args[0] == "mpg");

    auto modeSelect = ParseMenuCommand("SET_MODE_SELECT");
    assert(modeSelect.action == CommandAction::SetModeSelect);

    auto selectionAdd = ParseMenuCommand("SET_SELECTION_ADD");
    assert(selectionAdd.action == CommandAction::SetSelectionAdd);

    auto invert = ParseMenuCommand("INVERT_SELECTION");
    assert(invert.category == CommandCategory::Selection);
    assert(invert.action == CommandAction::InvertSelection);

    auto redraw = ParseCommandRequest({"REDRAW", "plot_1"});
    assert(redraw.category == CommandCategory::Plot);
    assert(redraw.action == CommandAction::RedrawPlot);

    auto clearGroup = ParseCommandRequest({"CLEAR", "cars"});
    assert(clearGroup.category == CommandCategory::Selection);
    assert(clearGroup.action == CommandAction::ClearSelection);

    auto closePlot = ParseCommandRequest({"CLOSE_PLOT", "plot_1"});
    assert(closePlot.category == CommandCategory::Plot);
    assert(closePlot.action == CommandAction::ClosePlot);

    auto closeAll = ParseCommandRequest({"CLOSE_ALL"});
    assert(closeAll.category == CommandCategory::Window);
    assert(closeAll.action == CommandAction::CloseAll);

    auto recordStart = ParseMenuCommand("RECORD_START");
    assert(recordStart.category == CommandCategory::Recording);
    assert(recordStart.action == CommandAction::RecordingCommand);

    auto overlay = ParseMenuCommand("TOGGLE_LM_COLOR");
    assert(overlay.action == CommandAction::ToggleLmColor);

    auto savePdf = ParseMenuCommand("SAVE_PDF");
    assert(savePdf.action == CommandAction::SavePdf);
    assert(CommandActionName(savePdf.action) == "save_pdf");

    auto palette = ParseMenuCommand("DATA_POINTS_COLOR");
    assert(palette.category == CommandCategory::Palette);
    assert(palette.action == CommandAction::OpenColorPalette);

    auto openPalette = ParseMenuCommand("OPEN_COLOR_PALETTE");
    assert(openPalette.category == CommandCategory::Palette);
    assert(openPalette.action == CommandAction::OpenColorPalette);

    auto paletteState = ParseCommandRequest({"PALETTE", "show"});
    assert(paletteState.category == CommandCategory::Palette);
    assert(paletteState.action == CommandAction::PaletteState);

    auto variablesWindow = ParseMenuCommand("OPEN_VARIABLES_WINDOW|cars");
    assert(variablesWindow.category == CommandCategory::Dataset);
    assert(variablesWindow.action == CommandAction::VariableView);
    assert(variablesWindow.args.size() == 1);
    assert(variablesWindow.args[0] == "cars");

    auto dataSheet = ParseMenuCommand("DATA_OPEN_DATA_SHEET|cars");
    assert(dataSheet.action == CommandAction::OpenDataSheet);
    assert(dataSheet.args.size() == 1);
    assert(dataSheet.args[0] == "cars");

    auto unknown = ParseMenuCommand("SOMETHING_PRIVATE|x");
    assert(unknown.name == "SOMETHING_PRIVATE");
    assert(unknown.category == CommandCategory::Unknown);
    assert(unknown.action == CommandAction::Unknown);
    assert(CommandCategoryName(unknown.category) == "unknown");

    return 0;
}
