#ifndef RLISPSTAT_CORE_DATASET_MODEL_H
#define RLISPSTAT_CORE_DATASET_MODEL_H

#include <cstddef>
#include <iosfwd>
#include <map>
#include <set>
#include <string>
#include <vector>

#include "provenance_model.h"

namespace rlispstat {
namespace core {

struct DataColumn {
    std::string name;
    std::string type;
    std::string displayName;
    std::string description;
    int decimals = -1;
    std::vector<std::string> values;
    std::vector<std::string> displayValues;
    std::vector<std::string> definedLevels;
    std::vector<bool> imputedMissing;
    std::vector<std::string> imputationOriginalValues;
    std::vector<std::vector<std::string>> imputationValues;
    std::map<std::size_t, std::string> imputationOriginalSparse;
    std::vector<std::map<std::size_t, std::string>> imputationValuesSparse;
    // Exact labels retained only while a binary factor is temporarily exposed
    // as numeric 0/1. This metadata is dormant for numeric rendering and
    // analysis, and permits a lossless change back to factor.
    std::vector<std::string> reversibleFactorLevels;
    // Statistical meaning and R storage are deliberately separate. `type`
    // remains the stable persisted semantic identifier; storageType records
    // the representation used to transport the values to R.
    std::string storageType;
    std::map<std::string, std::string> numericMapping;
    std::string reversibleCategoryType;
    bool binary = false;
};

struct VariableTypeConversionSpecification {
    std::map<std::string, std::string> numericMapping;
    std::vector<std::string> categoryOrder;
    bool invertBinary = false;
    bool useOrdinalPositions = true;
};

struct DataFrameModel {
    std::string group;
    int rows = 0;
    std::vector<DataColumn> columns;
    std::string datasetType = "data_frame";
    std::string imputationId;
    std::string sourceDatasetId;
    int imputationCount = 0;
    int activeImputationVersion = 1;
    std::string imputationDisplayMode = "version";
    // Opaque compressed R serialization of the original mids and name map.
    // Native code transports it; only R validates and interprets the contents.
    std::string imputationProcess;
    // Stable identity is independent of the visible row order. Data version
    // advances for every material data or metadata mutation.
    std::vector<std::string> stableRowIds;
    std::uint64_t dataVersion = 1;
    // Runtime-only R-session identity. It is intentionally absent from documents.
    std::string syncSessionToken;
    DataProvenance provenance;
    // Only verification bundles use this frozen RDS; ordinary exports do not.
    std::string verificationPreparedRds;
};

struct VariableViewRow {
    std::string name;
    std::string type;
    std::string description;
    int decimals = -1;
};

struct NumericImputationRange {
    bool any = false;
    double min = 0.0;
    double max = 0.0;
    double center = 0.0;
};

struct ChooseLabelColumnDialogState {
    std::string statusTitle = "Label Column";
    std::string title = "Choose Label Column";
    std::string informativeText = "Labels from this column can be shown for selected points or for all points in scatterplots.";
    std::string chooseButtonTitle = "Choose";
    std::string cancelButtonTitle = "Cancel";
    std::string noneOptionTitle = "(none)";
    std::string datasetLabelPrefix = "Dataset: ";
};

struct MissingDataImputationDialogState {
    std::string title = "Multiple Imputation";
    std::string failedTitle = "Imputation Failed";
    std::string informativeText = "Runs mice on the selected dataset and creates a linked multiple-imputation data sheet with imputed cells marked.";
    std::string automaticModelNote = "General-purpose imputation model. You can choose methods and predictors here. By default, selected predictors are used across the other imputation models. This may be unsuitable for your study, cause convergence problems, or use unnecessary computing resources. Advanced model design (separate predictor sets, interactions, blocks and constraints) should be prepared in mice in R; import the original mids object.";
    std::string runButtonTitle = "Run imputation";
    std::string cancelButtonTitle = "Cancel";
    std::string noImputeVariablesStatus = "Select at least one variable with missing values to impute.";
    std::string noPredictorVariablesStatus = "Select at least one predictor variable.";
    std::string datasetUnavailableStatus = "The selected dataset is no longer available.";
    std::string datasetLabel = "Dataset";
    std::string imputationsLabel = "Imputations";
    std::string iterationsLabel = "Iterations";
    std::string seedLabel = "Seed";
    std::string seedPlaceholder = "optional";
    std::string openDataSheetTitle = "Open imputed data sheet after fitting";
    std::string deselectAllTitle = "Deselect all";
    std::string deselectPredictorsTitle = "Deselect predictors";
    std::string instructionHint = "Select variables to impute, choose a mice method, and select predictors. ID-like columns are left unchecked by default.";
    std::string methodHint = "Methods: numeric pmm/norm/cart; binary categorical variables logreg; multi-category variables polyreg/cart.";
    std::string imputeColumnTitle = "Impute";
    std::string typeMissingColumnTitle = "Type / missing";
    std::string methodColumnTitle = "Method";
    std::string predictorColumnTitle = "Predictor";
    std::string unavailableDatasetRowText = "Dataset is no longer available.";
    std::string unsupportedMethodTitle = "unsupported";
    std::string predictorUseTitle = "Use";
};

bool DataCellIsMissing(const std::string &value);
bool ParseDataCellDouble(const std::string &value, double &out);
double ParseOptionalDataCellDouble(const std::string &value);
int ParseOptionalDataCellInt(const std::string &value);
std::vector<int> ParseRowIdList(const std::string &text);
std::string CsvEscape(const std::string &value);
bool WriteDataFrameCSV(std::ostream &out,
                       const DataFrameModel &df,
                       const std::vector<int> &includeRows = {});
// Writes a user-facing CSV rather than the internal R transport format.
// Ordinary datasets contain only their data columns. Multiple-imputation
// datasets use mice-compatible long form: .imp, .id, followed by the data
// columns, including .imp = 0 for the original incomplete data.
bool WriteDataExportCSV(std::ostream &out, const DataFrameModel &df);
// Writes column semantics used when converting the user-facing CSV through R.
// Factor and ordered levels are stored explicitly so statistical-package
// exports retain their categorical coding instead of becoming plain strings.
bool WriteDataExportMetadataCSV(std::ostream &out, const DataFrameModel &df);
DataFrameModel SubsetDataFrame(const DataFrameModel &source,
                               const std::vector<int> &originalRowIds,
                               const std::string &newGroup);
DataFrameModel SubsetDataFrameColumns(const DataFrameModel &source,
                                     const std::vector<std::string> &columnNames,
                                     const std::string &newGroup);
std::string NativeImportRScript();
std::string NativeMiceImputationRScript();
std::string NativePooledAnalysisRScript();
std::string NativeImportDialogTitle();
std::string NativeImportFailedTitle();
std::string NativeImportSupportedFormatsText();
struct NativeImportFileFilter {
    std::string identifier;
    std::string title;
    std::vector<std::string> extensions;
};
std::vector<NativeImportFileFilter> NativeImportFileFilters();
std::vector<std::string> NativeImportAllowedFileExtensions();
struct NativeDataExportFileFilter {
    std::string identifier;
    std::string title;
    std::string extension;
};
std::vector<NativeDataExportFileFilter> NativeDataExportFileFilters();
std::string NativeDataExportFormatForExtension(const std::string &extension);
std::string NativeDataExportRScript();
std::string NativeDataExportTemporaryFileFailedStatus();
std::string NativeDataExportRscriptLaunchFailedStatus();
std::string NativeDataExportRscriptFailedStatus();
std::string NativeDataExportSuccessStatus(const std::string &format,
                                          bool multipleImputation);
std::string NativeImportTemporaryScriptFailedStatus();
std::string NativeImportRscriptLaunchFailedStatus();
std::string NativeImportRscriptFailedStatus();
std::string NativeImportPayloadMissingStatus();
std::string NativeImportDatasetLoadedStatus(const std::string &group);
std::string NativeImportDatasetLoadedStatus(const std::string &group,
                                            int rows,
                                            std::size_t variableCount);
std::string ImportedVariableTypeReviewWarning(const DataFrameModel &dataframe);
std::string NativeRDataPayloadTemporaryFileFailedStatus();
std::string NativeMiceTemporaryScriptFailedStatus();
std::string NativeMiceRscriptLaunchFailedStatus();
std::string NativeMiceRscriptFailedStatus();
std::string NativeMicePayloadMissingStatus();
std::string NativeMiceCreatedDatasetStatus(const std::string &outputGroup, int m);
std::string NativePooledAnalysisTemporaryScriptFailedStatus();
std::string NativePooledAnalysisRscriptLaunchFailedStatus();
std::string NativePooledAnalysisRscriptFailedStatus();
std::string NativePooledAnalysisPayloadMissingStatus();
std::string NativePooledAnalysisOpenedStatus();

std::string NormalizeVariableType(const std::string &type);
bool VariableTypeIsSupported(const std::string &type);
bool VariableTypeIsNumeric(const std::string &type);
bool VariableTypeIsCategorical(const std::string &type);
bool VariableTypeIsOrdinal(const std::string &type);
bool VariableTypeIsText(const std::string &type);
bool VariableTypeIsFactorLike(const std::string &type);
std::string VariableTypeDisplayName(const std::string &type);
std::string VariableRoleDisplayName(const std::string &role);
std::string VariableTypeEditingStatus(const std::string &variable);
std::string VariableTypeChangedStatus(const std::string &variable,
                                      const std::string &type);
std::string VariableDecimalsUnavailableStatus(const std::string &variable,
                                              const std::string &type);
std::string VariableDecimalsInvalidStatus(bool bounded);
std::string VariableDecimalsChangedStatus(const std::string &variable,
                                          int decimals);
std::string VariableDescriptionChangedStatus(const std::string &variable);
std::string VariableRenamedStatus(const std::string &oldName,
                                  const std::string &newName);
std::string VariableNotFoundStatus(const std::string &variable);
std::string VariableNotFoundInDatasetStatus(const std::string &variable,
                                            const std::string &group);
std::string VariableUnavailableStatus(const std::string &variable);
std::string DerivedDataColumnAddedStatus(const std::string &column,
                                         const std::string &group);
std::string VariableRoleChangedStatus(const std::string &variable,
                                      const std::string &roleAction);
std::string VariableViewModelRoleSummary(const std::string &dependent,
                                         std::size_t predictorCount);

bool DataColumnAllowsNumeric(const DataColumn &col, std::string *message = nullptr);
bool DataColumnHasMissing(const DataColumn &col);
bool DataColumnAllMissing(const DataColumn &col);
bool DataColumnSupportedForMice(const DataColumn &col);
bool DataColumnLooksLikeId(const DataColumn &col);
bool DataColumnLooksLikeAnalysisId(const DataColumn &col);
bool DataColumnLooksBinaryNumeric(const DataColumn &col);
bool DataColumnIsBinaryCategorical(const DataColumn &col);
std::string DataColumnStorageType(const DataColumn &col);
bool DataColumnLooksGroupingCandidate(const DataColumn &col, int rows);
int DataColumnMissingCount(const DataColumn &col);
int DataColumnObservedLevelCount(const DataColumn &col);
std::vector<std::string> DataColumnObservedLevels(const DataColumn &col);
std::vector<std::string> DataColumnFactorLevels(const DataColumn &col);
std::string DefaultMiceMethod(const DataColumn &col);
std::vector<std::string> MiceMethodOptions(const DataColumn &col);
std::string MiceVariableStatusText(const DataColumn &col, int rowCount);
std::vector<std::string> DefaultImputeVariables(const DataFrameModel &df);
std::vector<std::string> DefaultPredictorVariables(const DataFrameModel &df);
std::string MiceDatasetSummaryText(const DataFrameModel &df,
                                   int missingColumnCount,
                                   int imputeColumnCount);
std::string MiceSelectionSummaryText(std::size_t imputeCount,
                                     std::size_t predictorCount);
bool SetDataColumnType(DataColumn &col, const std::string &type,
                       std::string *message = nullptr,
                       const VariableTypeConversionSpecification *conversion = nullptr);
bool SetDataColumnDescription(DataColumn &col, const std::string &description);
bool SetDataColumnDecimals(DataColumn &col, int decimals, std::string *message = nullptr);
bool IsValidVariableName(const std::string &name, std::string *message = nullptr);
std::string SafeDataColumnSuffix(const std::string &text);
std::string SafeDatasetName(const std::string &text);
std::string SafeDatasetNameForPath(const std::string &path);
std::string UniqueDatasetName(const std::string &baseName,
                              const std::vector<std::string> &existingNames);
std::string UniqueDataColumnName(const DataFrameModel &df, const std::string &base);
bool DerivedDataColumnKindIsSupported(const std::string &kind);
bool AddDerivedDataColumn(DataFrameModel &df,
                          const std::string &source,
                          const std::string &kind,
                          const std::set<int> &selectedRows,
                          const std::map<int, std::string> &rowColors,
                          std::string *createdName = nullptr,
                          std::string *message = nullptr);
bool BuildMissingDataPatternColumn(const DataFrameModel &df,
                                   const std::vector<std::string> &variables,
                                   DataColumn &column,
                                   std::string *message = nullptr);
std::string VariableInformationText(const DataFrameModel &df,
                                    const std::string &variable);
bool RenameDataFrameColumn(DataFrameModel &df,
                           const std::string &oldName,
                           const std::string &newName,
                           std::string *message = nullptr);
bool SetDataFrameCellValue(DataFrameModel &df,
                           const std::string &variable,
                           std::size_t row,
                           const std::string &value,
                           std::string *message = nullptr);
void EnsureDataFrameProvenance(DataFrameModel &df,
                               RCodeOrigin origin = RCodeOrigin::Unavailable,
                               const std::string &originCode = {},
                               const std::string &originDescription = {});
void RecordDataFrameTransformation(DataFrameModel &df,
                                   TransformationStep step);
void RecordDataFrameMetadataChange(DataFrameModel &df,
                                   const std::string &column,
                                   const std::string &label);
std::vector<VariableViewRow> VariableViewRowsForDataFrame(const DataFrameModel &df);

DataColumn *FindDataColumnInDataFrame(DataFrameModel &df, const std::string &name);
const DataColumn *FindDataColumnInDataFrame(const DataFrameModel &df, const std::string &name);

bool DataFrameCellIsImputed(const DataFrameModel &df, const DataColumn &col, std::size_t row);
int ImputationVersionCountForCell(const DataFrameModel &df, const DataColumn &col);
std::string OriginalImputationValueForCell(const DataColumn &col, std::size_t row);
std::string DisplayValueForCell(const DataColumn &col, std::size_t row);
std::vector<std::string> DisplayValuesForColumnsAtRow(
    const std::vector<const DataColumn *> &columns,
    std::size_t row,
    const std::string &missingValue = "NA");
std::map<int, std::string> RowLabelMapForColumn(const DataColumn &col);
std::string ImputationVersionValueForCell(const DataColumn &col,
                                          std::size_t row,
                                          std::size_t versionIndex);
std::string DisplayValueForDataFrameCell(const DataFrameModel &df,
                                         const DataColumn &col,
                                         std::size_t row);
void ApplyImputationDisplayModeToStoredValues(DataFrameModel &df);
double NumericValueForDataFrameCellVersion(const DataFrameModel &df,
                                           const DataColumn &col,
                                           std::size_t row,
                                           int versionIndex);
std::vector<int> CompleteRowsForDataColumns(const DataFrameModel &df,
                                            const std::vector<const DataColumn *> &columns,
                                            int versionIndex);
std::vector<int> CompleteRowsForNumericVectors(const std::vector<std::vector<double>> &columns);
bool ScatterImputationUncertaintyModeIsValid(const std::string &mode);
std::string NormalizedScatterImputationUncertaintyMode(const std::string &mode);
std::string ScatterImputationUncertaintyDisplayName(const std::string &mode);
void NumericRangeInclude(NumericImputationRange &range, double value);
NumericImputationRange NumericImputationRangeForCell(const DataFrameModel &df,
                                                     const DataColumn &col,
                                                     std::size_t row,
                                                     const std::string &uncertaintyMode);
NumericImputationRange NumericImputationRangeForValues(
    std::vector<double> values,
    const std::string &uncertaintyMode);
bool DataFrameShowsAllImputations(const DataFrameModel &df);
std::string PlotImputationDisplayStatus(const DataFrameModel &df,
                                        bool summarizesAllImputations);
std::string PooledEffectPlotImputationStatus(const DataFrameModel &df);
std::string DataFrameStatusText(const DataFrameModel &df,
                                std::size_t selectedCount);
std::string DataFrameImputationTooltipText(const DataFrameModel &df,
                                           const DataColumn &col,
                                           std::size_t row);
std::string DataFrameWindowTitle(const std::string &group);
std::string DataSheetChooseDataColumnStatus();
std::string VariableInformationDialogTitle();
std::string NoActiveDatasetTitle();
std::string ActiveDatasetTitle();
std::string ActiveDatasetUnavailableStatus();
std::string ActiveDatasetSummaryText(const std::string &group,
                                     int rows,
                                     std::size_t variableCount,
                                     std::size_t selectedRows);
std::string ImportOrRegisterDatasetStatus();
std::string NoActiveDatasetImportOrRegisterStatus();
std::string NoActiveDatasetStatus();
std::string NoRegisteredDatasetGroupStatus();
std::string DatasetNotAvailableStatus();
std::string DatasetColumnNotFoundStatus(const std::string &column,
                                         const std::string &group);
std::string DatasetVariableNameUnchangedStatus();
std::string DatasetPointLabelsResetStatus();
std::string DatasetPointLabelColumnSetStatus(const std::string &column);
std::string DatasetTemporaryDataWriteFailedStatus();
std::string VariableViewTitle();
std::string VariableViewWindowTitle(const std::string &group);
std::string VariableViewInstructionText();
std::string VariableDescriptionDialogTitle(const std::string &variable);
std::string VariableDescriptionDialogInformationText();
std::string DatasetNotRegisteredStatus(const std::string &group);
std::string DataSheetOpenTitle();
std::string DataSheetBackendPayloadUnavailableStatus();
std::string VariableTypeTitle();
std::string NoActiveVariableForTypeStatus();
std::string VariableDecimalsInfoText();
std::string NoRowsSelectedStatus();
std::string NoCompleteCasesStatus();
std::string DatasetForPlotNotAvailableStatus();
std::string VariableForPlotNotAvailableStatus();
std::string SelectedVariableNotAvailableStatus();
std::string NoVariablesToSummarizeStatus();
std::string Table1HistogramRequiresNumericStatus();
std::string Table1BarplotRequiresValueStatus();
std::string Table1BoxplotRequiresNumericStatus();
std::string DatasetNotAvailableForRecomputeStatus();
std::string NativeBackendCannotInspectRStatus();
std::string VariableDecimalsAutoButtonTitle();
std::string ScopeAllDataTitle();
std::string ScopeSelectedRowsTitle();
std::string ScopeUnselectedRowsTitle();
std::string ScopeCompareSelectedAllTitle();
std::string ChooseLabelColumnStatusTitle();
std::string AlertOKButtonTitle();
std::string AlertApplyButtonTitle();
std::string AlertCancelButtonTitle();
std::string RFileExtension();
std::string PDFFileExtension();
std::string PNGFileExtension();
std::string ActiveDatasetWindowTitle();
std::string RefreshRDataFramesWindowTitle();
std::string HashColumnHeader();

ChooseLabelColumnDialogState BuildChooseLabelColumnDialogState();
std::string DatasetDialogLabel(const std::string &group);
std::string NoVariableOptionTitle();
std::string ChooseLabelColumnDatasetLabel(const std::string &group);
MissingDataImputationDialogState BuildMissingDataImputationDialogState();

class DatasetRegistry {
public:
    bool empty() const;
    std::size_t size() const;
    bool contains(const std::string &group) const;

    void clear();
    void registerDataset(const DataFrameModel &df);
    bool erase(const std::string &group);

    DataFrameModel *find(const std::string &group);
    const DataFrameModel *find(const std::string &group) const;
    const std::map<std::string, DataFrameModel> &datasets() const;

    bool setActiveDataset(const std::string &group);
    void rememberActiveDatasetGroup(const std::string &group);
    std::string activeDatasetGroup(const std::string &fallbackGroup = "") const;
    const DataFrameModel *activeDataset(const std::string &fallbackGroup = "") const;
    std::vector<std::string> datasetGroups() const;
    std::string uniqueDatasetName(const std::string &baseName) const;
    bool isMultipleImputation(const std::string &group) const;

    size_t rowCount(const std::string &group) const;

private:
    std::map<std::string, DataFrameModel> dataFrames_;
    std::string activeGroup_;
};

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_DATASET_MODEL_H
