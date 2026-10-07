#ifndef RLISPSTAT_CORE_COMMAND_MODEL_H
#define RLISPSTAT_CORE_COMMAND_MODEL_H

#include <cstddef>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

enum class CommandCategory {
    Unknown,
    Protocol,
    Dataset,
    Selection,
    Plot,
    Model,
    Analysis,
    Data,
    Recording,
    Palette,
    Window
};

enum class CommandAction {
    Unknown,
    Ping,
    NativeImportFile,
    ListPlots,
    PlotInfo,
    ExportPlot,
    CopyPlot,
    PlotTheme,
    DiagnosticInfo,
    Groups,
    GroupInfo,
    DatasetSyncStatus,
    Variables,
    GetXVariable,
    GetYVariable,
    GetAnalysisScope,
    GetSavedAnalysisScopes,
    GetExcludedRows,
    ExcludeSelectedCases,
    IncludeSelectedCases,
    IncludeAllCases,
    ToggleCaseIncluded,
    SetAnalysisScopeAll,
    SetAnalysisScopeFromSelection,
    SetAnalysisScopeFromRows,
    SaveAnalysisScopeFromSelection,
    UseSavedAnalysisScope,
    AddSavedAnalysisScope,
    TogglePlotScopeFreeze,
    OpenEquivalentWithActiveScope,
    OpenEquivalentWithAllRows,
    FileImportData,
    FileExportData,
    FileOpenDataFromR,
    FileReturnDataToR,
    FileReturnSelectedRowsToR,
    FileCancelDataReturn,
    OpenDataSheet,
    RegisterDataset,
    RegisterDatasetSilent,
    SetActiveDataset,
    DataSetActiveDataset,
    ShowActiveDataset,
    ChooseLabelColumn,
    VariableView,
    AddSelectionColumn,
    AddPointColorColumn,
    SetVariableTypeNumeric,
    SetVariableTypeFactor,
    SetVariableType,
    SetDefaultVariableRole,
    SetDataCell,
    RenameVariable,
    SetVariableDescription,
    SetVariableDecimals,
    SetLabelColumn,
    ShowVariableInformation,
    ShowRCode,
    ShowRPublicationCode,
    MakeSubsetFromSelection,
    MakeSubsetFromSavedSelection,
    MakeSubsetFromVariables,
    ModelInfo,
    ModelSetY,
    ModelAddTerm,
    ModelReplaceTerm,
    ModelRemoveTerm,
    ModelClearRole,
    ModelScope,
    ModelOpen,
    ModelOpenResidualsFitted,
    ModelOpenObservedFitted,
    ModelOpenDiagnostics,
    ModelOpenDiagnostic,
    ModelInteractionReport,
    ModelOpenInteractionPlot,
    RefreshRDataFrames,
    Panel,
    PanelShow,
    PanelHide,
    ResetWindowLayout,
    RecordingCommand,
    OpenColorPalette,
    PaletteHide,
    PaletteState,
    NewScatterplot,
    NewTrellisScatterplot,
    NewTimeSeries,
    NewScatterMatrix,
    NewParallelCoordinates,
    ScatterMatrixAddVariable,
    ScatterMatrixRemoveVariable,
    ScatterMatrixReplaceVariable,
    NewBoxplot,
    NewHistogram,
    NewBarChart,
    AddPlot,
    AddBoxplot,
    AddHistogram,
    AddBarplot,
    BarplotSetX,
    BarplotAddX,
    BarplotReplaceX,
    BarplotRemoveX,
    BarplotSplitBy,
    BarplotClearSplit,
    BarplotToggleConditionalPercent,
    BarplotRowColors,
    BarplotSetSplitStrokeWidthPrompt,
    BarplotSplitStrokeWidth,
    BarplotSegmentEncoding,
    BarplotShowPatterns,
    BarplotSelectionDisplay,
    BarplotMode,
    BarplotWidth,
    BarplotSegmentSelect,
    BarplotSegmentApplyColorToRows,
    BarplotSegmentSetColor,
    BarplotLevelSetColor,
    BarplotSegmentSetAlpha,
    BarplotLevelSetPattern,
    BarplotSegmentSetPattern,
    BarplotSegmentResetColor,
    BarplotLevelResetColor,
    BarplotSegmentDetails,
    ContextCorrelationXY,
    CorrelationOpenStructured,
    CorrelationUpdate,
    CorrelationSetPart,
    CorrelationToggleDisplay,
    CorrelationSetMissing,
    CorrelationOpen,
    CorrelationSetVariables,
    CorrelationInfo,
    ContextDescriptivesXY,
    ContextLinearModelXY,
    PlotAnalyzeModel,
    PlotAnalyzeCorrelations,
    PlotAnalyzeContingency,
    PlotAnalyzeDescriptives,
    ContextHistogramFrequency,
    ContextHistogramDescriptives,
    ContextBarchartTable,
    ContextBarchartNestedTable,
    ContextBoxplotDescriptives,
    AnalyzeContingencyTable,
    OpenCorrelationMatrix,
    DimensionalityOpen,
    DimensionalityUpdate,
    DimensionalitySetVariables,
    DimensionalitySetImputation,
    DimensionalityInfo,
    ScaleAnalysisOpen,
    ScaleAnalysisUpdate,
    ScaleAnalysisSetItems,
    ScaleAnalysisInfo,
    DendrogramOpen,
    DendrogramSetVariables,
    DendrogramSetDistance,
    DendrogramInfo,
    DendrogramUpdate,
    OpenDimensionality,
    OpenScaleAnalysis,
    OpenDendrogram,
    Table1OpenStructured,
    Table1OpenText,
    CompareMeansOpen,
    CompareMeansBatchOpen,
    CompareMeansBatchError,
    OpenTable1,
    AnalyzeOneSampleT,
    AnalyzeIndependentT,
    AnalyzePairedT,
    AnalyzeOneWayAnova,
    AnalyzeMissingDataImputation,
    OpenImputationDiagnostics,
    ShowMissingInformation,
    MissingInformationAddVariable,
    SetDiagnosticPlotProvenance,
    DataMissingDataPatterns,
    OpenGLM,
    OpenLinearModelTrellis,
    OpenRegressionComparison,
    OpenGeneralizedGLM,
    OpenPositiveContinuousModel,
    OpenProportionModel,
    OpenCountRegression,
    OpenCountRegressionComparison,
    OpenBinaryRegression,
    GeneralizedGLMOpen,
    GeneralizedGLMOpenPooled,
    GeneralizedGLMOpenDiagnostic,
    OpenGeneralizedComparison,
    OpenPositiveContinuousComparison,
    OpenProportionComparison,
    OpenBinaryRegressionComparison,
    GeneralizedComparisonUpdateError,
    GeneralizedComparisonOpenStructured,
    AnalyzeLinearMixedModel,
    AnalyzeGeneralizedMixedModel,
    MixedModelOpenStructured,
    MixedModelOpenText,
    SetLabelDisplay,
    SetSelectedColor,
    ResetSelectedColor,
    GetSelectedColor,
    SetPointColor,
    ClearRowColors,
    SelectedRows,
    SetSelectedRows,
    SelectAllRows,
    InteractionMode,
    SelectionMode,
    SelectionOperation,
    BoxplotTogglePoints,
    BoxplotToggleBox,
    BoxplotToggleWhiskers,
    BoxplotToggleViolin,
    BoxplotClearSplitViolin,
    BoxplotConfigureSplitViolin,
    BoxplotConfigureH0,
    BoxplotClearH0,
    BoxplotToggleConnectRows,
    BoxplotToggleStandardize,
    BoxplotGroupOrder,
    BoxplotAddVariable,
    BoxplotRemoveVariable,
    BoxplotReplaceVariable,
    BoxplotAddGroupingVariable,
    BoxplotReplaceGroupingVariable,
    BoxplotRemoveGroupingVariable,
    BoxplotMoveGroupingVariableEarlier,
    BoxplotMoveGroupingVariableLater,
    BoxplotSplitViolin,
    BoxplotH0Simulation,
    BoxplotOption,
    BoxplotOptions,
    BoxplotShowPoints,
    BoxplotHidePoints,
    BoxplotShowBox,
    BoxplotHideBox,
    BoxplotShowWhiskers,
    BoxplotHideWhiskers,
    ScatterToggleOverlapShading,
    ScatterToggleOverlapSize,
    HistogramToggleCounts,
    HistogramToggleTickMarks,
    HistogramToggleTickLabels,
    HistogramToggleRug,
    HistogramToggleDensity,
    HistogramBreaks,
    HistogramSetBins,
    HistogramSetBinningRule,
    HistogramDensityInfo,
    HistogramShowDensity,
    HistogramSetDensityMode,
    HistogramSetDensityBandwidth,
    HistogramSetDensityAdjust,
    BiplotSetX,
    BiplotSetY,
    SetImputationDisplay,
    SetImputationUncertainty,
    ChangeXVariable,
    ChangeYVariable,
    SetPlotColorBy,
    SaveColorScheme,
    ApplyColorScheme,
    SetPlotColorLegendVisible,
    SetPlotColorLegendPosition,
    ConditionPlotCategorical,
    ConditionPlotOrdered,
    ConditionPlotEqualWidth,
    ConditionPlotEqualCount,
    SetTrellisScatterplotX,
    SetTrellisScatterplotY,
    SetTrellisScatterplotCondition,
    SetTrellisScatterplotLayout,
    SetTrellisScatterplotOrder,
    SetTrellisPlotType,
    SetTrellisPlotTypeWithX,
    SetTrellisBoxplotGroupingVariables,
    AddTrellisBoxplotGroupingVariable,
    ReplaceTrellisBoxplotGroupingVariable,
    RemoveTrellisBoxplotGroupingVariable,
    MoveTrellisBoxplotGroupingVariableEarlier,
    MoveTrellisBoxplotGroupingVariableLater,
    AddTrellisConditionCategorical,
    AddTrellisConditionOrdered,
    AddTrellisConditionEqualWidth,
    AddTrellisConditionEqualCount,
    SetTrellisConditionFactor,
    SetTrellisConditionOrdered,
    SetTrellisConditionEqualWidth,
    SetTrellisConditionEqualCount,
    RemoveTrellisCondition,
    MoveTrellisConditionEarlier,
    MoveTrellisConditionLater,
    ConfigureTrellisConditionEqualWidth,
    ConfigureTrellisConditionEqualCount,
    ConfigureTrellisConditionBins,
    SetTrellisConditionDimensionRows,
    SetTrellisConditionDimensionColumns,
    SetTrellisConditionDimensionNested,
    SwapTrellisRowsAndColumns,
    ToggleTrellisYAxisSide,
    SetTrellisSplitVariable,
    SetTrellisScaleMode,
    SetTrellisBarMeasure,
    SetTrellisHistogramMeasure,
    SetTrellisHistogramBins,
    ToggleTrellisConnectObservations,
    SetTimeSeriesGroup,
    SetTimeSeriesIdentification,
    SetTimeSeriesLegendPosition,
    SetInteractionLegendPosition,
    ToggleRegressionConnectingLine,
    ToggleRegressionConfidenceIntervals,
    ToggleSmoothConfidenceIntervals,
    ToggleRegressionConfidenceLevel,
    SetRegressionEffectXAxis,
    SetRegressionEffectQuantity,
    SetRegressionEffectAdjustment,
    SetRegressionEffectPresentation,
    SetRegressionEffectConfidence,
    SetModeNone,
    SetModeSelect,
    SetModeBrush,
    SetModeIdentify,
    SetModeLabel,
    SetModePan,
    SetModeZoom,
    SetSelectionReplace,
    SetSelectionAdd,
    SetSelectionSubtract,
    SetSelectionToggle,
    ClearSelection,
    InvertSelection,
    SelectAllVisible,
    RedrawPlot,
    ResetZoom,
    BrushLarger,
    BrushSmaller,
    CopySelected,
    AddLm,
    ToggleLmAll,
    ToggleLmSelected,
    ToggleLmColor,
    ToggleLmBoth,
    AddLmAll,
    AddLmSelected,
    AddLmBoth,
    AddLmColor,
    AddSmooth,
    AddTrellisSmooth,
    RequestSmooth,
    SmoothInfo,
    RequestCompareMeans,
    MainRTasks,
    ModelUpdateError,
    ModelUpdate,
    ModelTrellisUpdateError,
    ModelTrellisUpdate,
    RegressionComparisonUpdateError,
    RegressionComparisonUpdate,
    RegressionComparisonOpenPooled,
    RegressionComparisonOpen,
    RegressionComparisonOpen2,
    RegressionComparisonInfo,
    RegressionComparisonCell,
    RegressionComparisonFitCell,
    RegressionComparisonVisible,
    RegressionComparisonOpenDiagnostic,
    RegressionComparisonSetResponse,
    RegressionComparisonSetScope,
    RegressionComparisonAddTerm,
    RegressionComparisonSetTerm,
    RegressionComparisonAddModel,
    RegressionComparisonSetType,
    ModelOpenPooled,
    GeneralizedGLMOpenStructured,
    PointColors,
    ToggleSmoothOverall,
    ToggleSmoothSelected,
    ToggleSmoothColor,
    Overlays,
    ClearOverlays,
    SaveSvg,
    SavePng,
    SavePdf,
    CopySvg,
    CopyEmf,
    CopyPng,
    CopyPdf,
    ClosePlot,
    CloseAll
};

struct CommandRequest {
    std::string name;
    std::vector<std::string> args;
    CommandCategory category = CommandCategory::Unknown;
    CommandAction action = CommandAction::Unknown;
};

struct CommandModelSpec {
    std::string label;
    std::string response;
    std::vector<std::string> terms;
};

struct LinkedPlotCommandOption {
    std::string title;
    std::string value;
    std::string command;
};

struct ApplicationMenuCommandOption {
    std::string title;
    std::string value;
    std::string command;
    std::string keyEquivalent;
    bool commandModifier = false;
    bool shiftModifier = false;
    bool experimental = false;
    // Optional second-level grouping inside an application-menu group.  This
    // keeps deeper Analyze hierarchies shared instead of rebuilding them in
    // each native front end.
    std::string subgroupTitle;
};

struct ApplicationMenuCommandGroup {
    // An empty title places its options directly in the parent menu.
    std::string title;
    std::vector<ApplicationMenuCommandOption> options;
};

struct ApplicationMenuTitles {
    std::string application;
    std::string quitApplication;
    std::string quitKeyEquivalent;
    std::string file;
    std::string edit;
    std::string data;
    std::string activeDataset;
    std::string record;
    std::string plot;
    std::string analyze;
    std::string variableType;
    std::string colorSelectedPoints;
};

struct PlotContextMenuTitles {
    std::string root;
    std::string variables;
    std::string changeXVariable;
    std::string changeYVariable;
    std::string mouseMode;
    std::string selection;
    std::string colorSelectedPoints;
    std::string colorSelectedRows;
    std::string openColorPalette;
    std::string addDataColumn;
    std::string labels;
    std::string view;
    std::string theme;
    std::string brush;
    std::string imputationUncertainty;
    std::string overlays;
    std::string analyzeThisPlot;
    std::string analyzeThisBarChart;
    std::string analyzeThisHistogram;
    std::string analyzeThisBoxplot;
    std::string plot;
    std::string exportMenu;
};

struct DataSheetContextMenuTitles {
    std::string root;
    std::string colorSelectedRows;
    std::string clearRowColor;
    std::string clearAllRowColors;
    std::string variableView;
    std::string treatColumnAsNumeric;
    std::string treatColumnAsFactor;
    std::string treatColumnAsText;
    std::string showVariableInformation;
    std::string imputedDataDisplay;
    std::string allImputedValuesDisplay;
    std::string originalIncompleteDataDisplay;
};

struct VariableViewContextMenuTitles {
    std::string root;
    std::string role;
    std::string type;
    std::string editDescription;
    std::string showVariableInformation;
};

struct ModelContextMenuTitles {
    std::string model;
    std::string statistic;
    std::string predictor;
    std::string coefficient;
    std::string modelFit;
    std::string modelComparison;
    std::string addTerm;
    std::string addTermAction;
    std::string addModel;
    std::string addModelFromActiveModel;
    std::string refitModel;
    std::string refitActiveModel;
    std::string autoRefitOn;
    std::string autoRefitOff;
    std::string openDiagnostics;
    std::string copyValue;
    std::string showStatisticDetails;
    std::string showCoefficientDetails;
    std::string changeVariable;
    std::string changeType;
    std::string removePredictor;
    std::string showVariableInformation;
    std::string responseVariable;
    std::string changeResponse;
    std::string clearResponse;
    std::string predictorTerms;
    std::string addTermCapitalized;
    std::string changeSelectedTerm;
    std::string removeSelectedTerm;
    std::string treatAsNumeric;
    std::string treatAsFactor;
    std::string treatSelectedTermAsNumeric;
    std::string treatSelectedTermAsFactor;
    std::string showSelectedTermInformation;
    std::string addRemoveTermInModel;
    std::string addTermToAllModels;
    std::string removeTermFromAllModels;
    std::string copyRegressionTable;
    std::string explainStatistic;
    std::string openDiagnosticPlot;
    std::string openRelevantDiagnosticPlot;
    std::string compareWithNullModel;
    std::string interpretInteraction;
    std::string interactionPlot;
    std::string partialRegressionPlot;
    std::string dropCoefficient;
    std::string noAvailableInteractions;
    std::string addPolynomialTerm;
    std::string copyModelTable;
    std::string copyModelComparisonTable;
    std::string exportModelTablePdf;
    std::string rowsUsedExcluded;
    std::string openSingleGLM;
    std::string openAsSingleGLM;
    std::string openSingleGeneralizedGLM;
    std::string openAsSingleGeneralizedGLM;
    std::string compareModels;
};

std::string EncodeCommandField(const std::string &text);
std::string DecodeCommandField(const std::string &text);
std::string ProtocolSafeValue(std::string value);
std::string EncodeCommandModelSpecTable(const std::vector<CommandModelSpec> &models);
ApplicationMenuTitles DefaultApplicationMenuTitles();
PlotContextMenuTitles DefaultPlotContextMenuTitles();
DataSheetContextMenuTitles DefaultDataSheetContextMenuTitles();
std::string DataSheetImputationVersionTitle(int version);
VariableViewContextMenuTitles DefaultVariableViewContextMenuTitles();
ModelContextMenuTitles DefaultModelContextMenuTitles();
std::vector<ApplicationMenuCommandOption> FileCommandOptions();
std::vector<std::vector<ApplicationMenuCommandOption>> FileCommandOptionGroups();
std::vector<ApplicationMenuCommandOption> EditCommandOptions();
std::vector<std::vector<ApplicationMenuCommandOption>> RecordCommandOptionGroups();
std::vector<ApplicationMenuCommandGroup> AnalyzeCommandMenuGroups();
std::vector<ApplicationMenuCommandOption> ModelingCommandOptions();
std::vector<ApplicationMenuCommandGroup> VisibleAnalyzeCommandMenuGroups(
    bool includeExperimental = false);
std::string GlobalAnalysisScopeMenuTitle();
std::string PlotAnalysisScopeMenuTitle();
std::vector<ApplicationMenuCommandOption> AnalysisScopeCommandOptions();
std::vector<ApplicationMenuCommandOption> PlotCommandOptions();
std::vector<ApplicationMenuCommandOption> TrellisPlotStartCommandOptions();
std::vector<LinkedPlotCommandOption> LinkedPlotCommandOptions();
std::vector<ApplicationMenuCommandOption> ActiveDatasetCommandOptions(const std::vector<std::string> &groups);
std::string NoRegisteredDatasetsTitle();
std::vector<LinkedPlotCommandOption> SelectionCommandOptions(bool includeSelectAllVisible);
std::vector<std::vector<LinkedPlotCommandOption>> PlotExportCommandOptionGroups(bool includeSelectedRows);
std::vector<LinkedPlotCommandOption> DataWindowCommandOptions();
std::vector<LinkedPlotCommandOption> DataColumnCommandOptions(const std::string &group = "");
std::vector<LinkedPlotCommandOption> DataVariableTypeCommandOptions();
std::vector<LinkedPlotCommandOption> VariableViewRoleOptions();
std::vector<LinkedPlotCommandOption> VariableViewTypeOptions();
std::vector<LinkedPlotCommandOption> ModelTermTypeOptions();
std::vector<LinkedPlotCommandOption> ModelDiagnosticPlotOptions(bool includeExtended);
std::string PredictorTypeMenuTitle();
std::string TermTypeMenuTitle();
std::string AutoRefitMenuTitle(bool autoRefit);
ApplicationMenuCommandOption DataPointsColorPaletteCommandOption();
ApplicationMenuCommandOption DataVariableInformationCommandOption();
ApplicationMenuCommandOption DataRefreshCommandOption();
bool IsLinkedPlotCommandName(const std::string &commandName);
bool IsDataColumnCommandName(const std::string &commandName);
bool IsNativeUiDispatchCommandName(const std::string &commandName);

CommandRequest ParseCommandRequest(const std::vector<std::string> &lines);
CommandRequest ParseMenuCommand(const std::string &command);

std::string CommandCategoryName(CommandCategory category);
std::string CommandActionName(CommandAction action);
bool CommandRequiresPlot(CommandCategory category);
bool CommandIsDatasetCommand(const CommandRequest &request);
bool CommandHasName(const CommandRequest &request, const std::string &name);

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_COMMAND_MODEL_H
