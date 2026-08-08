#include "../../src/core/dataset_model.h"

#include <cassert>
#include <cmath>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <vector>

using rlispstat::core::DataColumn;
using rlispstat::core::AddDerivedDataColumn;
using rlispstat::core::ActiveDatasetSummaryText;
using rlispstat::core::ActiveDatasetTitle;
using rlispstat::core::ActiveDatasetUnavailableStatus;
using rlispstat::core::BuildChooseLabelColumnDialogState;
using rlispstat::core::ChooseLabelColumnDatasetLabel;
using rlispstat::core::ChooseLabelColumnDialogState;
using rlispstat::core::DataCellIsMissing;
using rlispstat::core::DataColumnAllowsNumeric;
using rlispstat::core::DataColumnAllMissing;
using rlispstat::core::DataColumnHasMissing;
using rlispstat::core::DataColumnLooksBinaryNumeric;
using rlispstat::core::DataColumnLooksGroupingCandidate;
using rlispstat::core::DataColumnLooksLikeId;
using rlispstat::core::DataColumnMissingCount;
using rlispstat::core::DataColumnObservedLevelCount;
using rlispstat::core::DataColumnSupportedForMice;
using rlispstat::core::DataFrameCellIsImputed;
using rlispstat::core::DataFrameModel;
using rlispstat::core::DataFrameShowsAllImputations;
using rlispstat::core::DataFrameImputationTooltipText;
using rlispstat::core::DataFrameStatusText;
using rlispstat::core::DataFrameWindowTitle;
using rlispstat::core::DataSheetChooseDataColumnStatus;
using rlispstat::core::DataSheetBackendPayloadUnavailableStatus;
using rlispstat::core::DataSheetOpenTitle;
using rlispstat::core::DatasetRegistry;
using rlispstat::core::NoActiveDatasetStatus;
using rlispstat::core::NoRegisteredDatasetGroupStatus;
using rlispstat::core::DatasetNotAvailableStatus;
using rlispstat::core::DatasetColumnNotFoundStatus;
using rlispstat::core::DatasetVariableNameUnchangedStatus;
using rlispstat::core::DatasetPointLabelsResetStatus;
using rlispstat::core::DatasetPointLabelColumnSetStatus;
using rlispstat::core::DatasetTemporaryDataWriteFailedStatus;
using rlispstat::core::DatasetDialogLabel;
using rlispstat::core::DatasetNotRegisteredStatus;
using rlispstat::core::ImportOrRegisterDatasetStatus;
using rlispstat::core::BuildMissingDataImputationDialogState;
using rlispstat::core::MissingDataImputationDialogState;
using rlispstat::core::MiceDatasetSummaryText;
using rlispstat::core::MiceSelectionSummaryText;
using rlispstat::core::MiceVariableStatusText;
using rlispstat::core::NativeImportAllowedFileExtensions;
using rlispstat::core::NativeImportDialogTitle;
using rlispstat::core::NativeImportDatasetLoadedStatus;
using rlispstat::core::NativeImportFailedTitle;
using rlispstat::core::NativeImportSupportedFormatsText;
using rlispstat::core::NativeImportPayloadMissingStatus;
using rlispstat::core::NativeImportRscriptFailedStatus;
using rlispstat::core::NativeImportRscriptLaunchFailedStatus;
using rlispstat::core::NativeImportTemporaryScriptFailedStatus;
using rlispstat::core::NativeMiceCreatedDatasetStatus;
using rlispstat::core::NativeMicePayloadMissingStatus;
using rlispstat::core::NativeMiceRscriptFailedStatus;
using rlispstat::core::NativeMiceRscriptLaunchFailedStatus;
using rlispstat::core::NativeMiceTemporaryScriptFailedStatus;
using rlispstat::core::NativePooledAnalysisOpenedStatus;
using rlispstat::core::NativePooledAnalysisPayloadMissingStatus;
using rlispstat::core::NativePooledAnalysisRscriptFailedStatus;
using rlispstat::core::NativePooledAnalysisRscriptLaunchFailedStatus;
using rlispstat::core::NativePooledAnalysisTemporaryScriptFailedStatus;
using rlispstat::core::NativeRDataPayloadTemporaryFileFailedStatus;
using rlispstat::core::NoActiveVariableForTypeStatus;
using rlispstat::core::VariableDecimalsInfoText;
using rlispstat::core::NoRowsSelectedStatus;
using rlispstat::core::NoCompleteCasesStatus;
using rlispstat::core::DatasetForPlotNotAvailableStatus;
using rlispstat::core::VariableForPlotNotAvailableStatus;
using rlispstat::core::SelectedVariableNotAvailableStatus;
using rlispstat::core::NoVariablesToSummarizeStatus;
using rlispstat::core::Table1HistogramRequiresNumericStatus;
using rlispstat::core::Table1BarplotRequiresValueStatus;
using rlispstat::core::Table1BoxplotRequiresNumericStatus;
using rlispstat::core::DatasetNotAvailableForRecomputeStatus;
using rlispstat::core::NativeBackendCannotInspectRStatus;
using rlispstat::core::VariableDecimalsAutoButtonTitle;
using rlispstat::core::ScopeAllDataTitle;
using rlispstat::core::ScopeSelectedRowsTitle;
using rlispstat::core::ScopeUnselectedRowsTitle;
using rlispstat::core::ScopeCompareSelectedAllTitle;
using rlispstat::core::AlertOKButtonTitle;
using rlispstat::core::AlertApplyButtonTitle;
using rlispstat::core::AlertCancelButtonTitle;
using rlispstat::core::RFileExtension;
using rlispstat::core::PDFFileExtension;
using rlispstat::core::PNGFileExtension;
using rlispstat::core::ActiveDatasetWindowTitle;
using rlispstat::core::RefreshRDataFramesWindowTitle;
using rlispstat::core::HashColumnHeader;
using rlispstat::core::NoActiveDatasetImportOrRegisterStatus;
using rlispstat::core::NoActiveDatasetTitle;
using rlispstat::core::NoVariableOptionTitle;
using rlispstat::core::DerivedDataColumnKindIsSupported;
using rlispstat::core::DisplayValueForCell;
using rlispstat::core::DisplayValueForDataFrameCell;
using rlispstat::core::DisplayValuesForColumnsAtRow;
using rlispstat::core::DerivedDataColumnAddedStatus;
using rlispstat::core::CompleteRowsForDataColumns;
using rlispstat::core::CompleteRowsForNumericVectors;
using rlispstat::core::CsvEscape;
using rlispstat::core::DefaultImputeVariables;
using rlispstat::core::DefaultMiceMethod;
using rlispstat::core::DefaultPredictorVariables;
using rlispstat::core::FindDataColumnInDataFrame;
using rlispstat::core::ImputationVersionCountForCell;
using rlispstat::core::ImputationVersionValueForCell;
using rlispstat::core::IsValidVariableName;
using rlispstat::core::NormalizeVariableType;
using rlispstat::core::NumericValueForDataFrameCellVersion;
using rlispstat::core::OriginalImputationValueForCell;
using rlispstat::core::ParseDataCellDouble;
using rlispstat::core::ParseOptionalDataCellDouble;
using rlispstat::core::ParseOptionalDataCellInt;
using rlispstat::core::ParseRowIdList;
using rlispstat::core::RenameDataFrameColumn;
using rlispstat::core::RowLabelMapForColumn;
using rlispstat::core::ApplyImputationDisplayModeToStoredValues;
using rlispstat::core::NormalizedScatterImputationUncertaintyMode;
using rlispstat::core::NumericImputationRange;
using rlispstat::core::NumericImputationRangeForCell;
using rlispstat::core::ScatterImputationUncertaintyDisplayName;
using rlispstat::core::ScatterImputationUncertaintyModeIsValid;
using rlispstat::core::SetDataColumnDecimals;
using rlispstat::core::SetDataColumnDescription;
using rlispstat::core::SetDataColumnType;
using rlispstat::core::SetDataFrameCellValue;
using rlispstat::core::SafeDataColumnSuffix;
using rlispstat::core::SafeDatasetName;
using rlispstat::core::SafeDatasetNameForPath;
using rlispstat::core::UniqueDataColumnName;
using rlispstat::core::UniqueDatasetName;
using rlispstat::core::SubsetDataFrame;
using rlispstat::core::VariableRoleDisplayName;
using rlispstat::core::VariableInformationDialogTitle;
using rlispstat::core::VariableInformationText;
using rlispstat::core::VariableDescriptionDialogInformationText;
using rlispstat::core::VariableDescriptionDialogTitle;
using rlispstat::core::VariableViewInstructionText;
using rlispstat::core::VariableViewTitle;
using rlispstat::core::VariableViewWindowTitle;
using rlispstat::core::VariableTypeTitle;
using rlispstat::core::VariableDecimalsChangedStatus;
using rlispstat::core::VariableDecimalsInvalidStatus;
using rlispstat::core::VariableDecimalsUnavailableStatus;
using rlispstat::core::VariableDescriptionChangedStatus;
using rlispstat::core::VariableNotFoundInDatasetStatus;
using rlispstat::core::VariableNotFoundStatus;
using rlispstat::core::VariableUnavailableStatus;
using rlispstat::core::VariableRenamedStatus;
using rlispstat::core::VariableRoleChangedStatus;
using rlispstat::core::VariableTypeDisplayName;
using rlispstat::core::VariableTypeChangedStatus;
using rlispstat::core::VariableTypeEditingStatus;
using rlispstat::core::VariableTypeIsFactorLike;
using rlispstat::core::VariableTypeIsSupported;
using rlispstat::core::VariableViewRowsForDataFrame;
using rlispstat::core::VariableViewModelRoleSummary;
using rlispstat::core::WriteDataFrameCSV;
using rlispstat::core::MiceMethodOptions;
using rlispstat::core::NativeImportRScript;
using rlispstat::core::NativeMiceImputationRScript;
using rlispstat::core::NativePooledAnalysisRScript;

static bool closeEnough(double a, double b)
{
    return std::fabs(a - b) < 1.0e-9;
}

int main()
{
    DataFrameModel cars;
    cars.group = "cars";
    cars.rows = 2;
    cars.columns.push_back(DataColumn{"mpg", "numeric", "", "", 1, {"21", "22"}, {}, {}, {}, {}, {}, {}});
    cars.columns.push_back(DataColumn{"am", "factor", "", "Transmission", -1, {"0", "1"}, {}, {}, {}, {}, {}, {}});
    DataFrameModel editableCars = cars;
    std::string editMessage;
    assert(SetDataFrameCellValue(editableCars, "mpg", 0, "25.75", &editMessage));
    assert(editableCars.columns[0].values[0] == "25.75");
    assert(DisplayValueForDataFrameCell(editableCars, editableCars.columns[0], 0) == "25.8");
    assert(!SetDataFrameCellValue(editableCars, "mpg", 0, "not a number", &editMessage));
    assert(editableCars.columns[0].values[0] == "25.75");
    assert(SetDataFrameCellValue(editableCars, "am", 1, "2", &editMessage));
    assert(editableCars.columns[1].values[1] == "2");
    assert(editableCars.columns[1].definedLevels.back() == "2");

    assert(FindDataColumnInDataFrame(cars, "mpg") != nullptr);
    assert(FindDataColumnInDataFrame(cars, "missing") == nullptr);
    assert(FindDataColumnInDataFrame(cars, "am")->description == "Transmission");

    assert(DataCellIsMissing(""));
    assert(DataCellIsMissing("NA"));
    double parsed = 0.0;
    assert(ParseDataCellDouble(" 3.25 ", parsed));
    assert(parsed == 3.25);
    assert(!ParseDataCellDouble("3abc", parsed));
    assert(closeEnough(ParseOptionalDataCellDouble("4.5"), 4.5));
    assert(!std::isfinite(ParseOptionalDataCellDouble("NA")));
    assert(ParseOptionalDataCellInt("3.6") == 4);
    assert(ParseOptionalDataCellInt("bad") == 0);
    assert((ParseRowIdList("1,2,,bad,4") == std::vector<int>{1, 2, 4}));
    assert(CsvEscape("a,b") == "\"a,b\"");
    assert(CsvEscape("a\"b") == "\"a\"\"b\"");
    std::string importScript = NativeImportRScript();
    assert(importScript.find("REGISTER_DATASET") != std::string::npos);
    assert(importScript.find("haven::read_sav") != std::string::npos);
    assert(importScript.find("utils::read.csv") != std::string::npos);
    assert(importScript.find("utils::read.delim") != std::string::npos);
    assert(importScript.find("readxl::read_excel") != std::string::npos);
    assert(importScript.find("haven::read_dta") != std::string::npos);
    assert(importScript.find("haven::read_sas") != std::string::npos);
    assert(importScript.find("haven::read_xpt") != std::string::npos);
    assert(importScript.find("readRDS") != std::string::npos);
    assert(importScript.find("DATADISPLAY") != std::string::npos);
    std::string miceScript = NativeMiceImputationRScript();
    assert(miceScript.find("mice::mice") != std::string::npos);
    assert(miceScript.find("IMPUTATION_SPARSE") != std::string::npos);
    assert(miceScript.find("Multiple imputation requires the mice package") != std::string::npos);
    std::string pooledScript = NativePooledAnalysisRScript();
    assert(pooledScript.find("ls_new_table1") != std::string::npos);
    assert(pooledScript.find(".rls_glm_pooled_native_payload") != std::string::npos);
    assert(pooledScript.find("ls_new_regression_comparison") != std::string::npos);

    std::ostringstream csv;
    assert(WriteDataFrameCSV(csv, cars, {2}));
    assert(csv.str() == "..rlispstat_row_id,mpg,am\n2,22,1\n");
    DataFrameModel carsSubset = SubsetDataFrame(cars, {2, 2, 4}, "manual cars");
    assert(carsSubset.group == "manual cars");
    assert(carsSubset.sourceDatasetId == "cars");
    assert(carsSubset.rows == 1);
    assert((carsSubset.columns[0].values == std::vector<std::string>{"22"}));
    assert((carsSubset.columns[1].values == std::vector<std::string>{"1"}));

    assert(NormalizeVariableType("Numérico") == "numeric");
    assert(NormalizeVariableType("ordered factor") == "ordered");
    assert(VariableTypeIsSupported("text"));
    assert(VariableTypeIsFactorLike("logical"));
    assert(VariableTypeDisplayName("character") == "Text");
    assert(VariableRoleDisplayName("Predictor") == "Independent");
    assert(VariableTypeEditingStatus("mpg") == "Editing type for `mpg`.");
    assert(VariableTypeChangedStatus("mpg", "numeric") == "mpg is now treated as Numeric.");
    assert(VariableDecimalsUnavailableStatus("group", "factor") ==
           "Decimals are available only for numeric variables; `group` is Factor.");
    assert(VariableDecimalsInvalidStatus(false) == "Decimals must be an integer or blank for automatic.");
    assert(VariableDecimalsInvalidStatus(true) == "Decimals must be an integer from 0 to 12, or Automatic.");
    assert(VariableDecimalsChangedStatus("mpg", -1) == "Decimals for `mpg` set to automatic.");
    assert(VariableDecimalsChangedStatus("mpg", 2) == "Decimals for `mpg` set to 2.");
    assert(VariableDescriptionChangedStatus("mpg") == "Updated description for `mpg`.");
    assert(VariableRenamedStatus("mpg", "fuel") == "Renamed `mpg` to `fuel`.");
    assert(VariableNotFoundStatus("mpg") == "Variable `mpg` was not found.");
    assert(VariableNotFoundInDatasetStatus("mpg", "cars") ==
           "Variable `mpg` was not found in dataset `cars`.");
    assert(VariableUnavailableStatus("mpg") == "Variable `mpg` is not available.");
    assert(DerivedDataColumnAddedStatus("selected_from_plot", "cars") ==
           "Added `selected_from_plot` to dataset `cars`.");
    assert(VariableRoleChangedStatus("mpg", "dependent") == "Set `mpg` as response variable.");
    assert(VariableRoleChangedStatus("wt", "predictor") == "Added `wt` as predictor.");
    assert(VariableRoleChangedStatus("wt", "remove_predictor") == "Removed predictor role from `wt`.");
    assert(VariableRoleChangedStatus("wt", "none") == "Cleared model role for `wt`.");
    assert(VariableViewModelRoleSummary("", 0) == "Model roles: Y=(none) | predictors=0");
    assert(VariableViewModelRoleSummary("mpg", 2) == "Model roles: Y=mpg | predictors=2");

    DataFrameModel miceDf;
    miceDf.group = "mice";
    miceDf.rows = 4;
    DataColumn idCol{"id", "numeric", "", "", -1, {"1", "2", "3", "4"}, {}, {}, {}, {}, {}, {}};
    DataColumn ageColForMice{"age", "numeric", "", "", -1, {"10", "NA", "13", "14"}, {}, {}, {}, {}, {}, {}};
    DataColumn groupCol{"group", "factor", "", "", -1, {"A", "B", "A", "NA"}, {}, {}, {}, {}, {}, {}};
    DataColumn orderedCol{"rating", "ordered", "", "", -1, {"low", "mid", "high", "NA"}, {}, {}, {}, {}, {}, {}};
    DataColumn allMissingCol{"empty", "numeric", "", "", -1, {"NA", "NA", "NA", "NA"}, {}, {}, {}, {}, {}, {}};
    DataColumn labelCol{"case_label", "character", "", "", -1, {"a", "b", "c", "d"}, {}, {}, {}, {}, {}, {}};
    miceDf.columns = {idCol, ageColForMice, groupCol, orderedCol, allMissingCol, labelCol};
    assert(DataColumnHasMissing(ageColForMice));
    assert(!DataColumnAllMissing(ageColForMice));
    assert(DataColumnAllMissing(allMissingCol));
    assert(DataColumnSupportedForMice(groupCol));
    assert(DataColumnLooksLikeId(idCol));
    assert(DataColumnLooksLikeId(labelCol));
    assert(!DataColumnLooksLikeId(ageColForMice));
    DataColumn binaryCol{"binary", "numeric", "", "", -1, {"0", "1", "1", "0", "NA"}, {}, {}, {}, {}, {}, {}};
    assert(DataColumnLooksBinaryNumeric(binaryCol));
    binaryCol.values = {"0", "2"};
    assert(!DataColumnLooksBinaryNumeric(binaryCol));
    assert(DataColumnLooksGroupingCandidate(groupCol, 4));
    assert(!DataColumnLooksGroupingCandidate(idCol, 4));
    assert(DataColumnMissingCount(ageColForMice) == 1);
    assert(DataColumnObservedLevelCount(groupCol) == 2);
    assert(DefaultMiceMethod(ageColForMice) == "pmm");
    assert(DefaultMiceMethod(groupCol) == "logreg");
    assert(DefaultMiceMethod(orderedCol) == "polr");
    assert((MiceMethodOptions(ageColForMice) == std::vector<std::string>{"pmm", "norm", "cart"}));
    assert((MiceMethodOptions(orderedCol) == std::vector<std::string>{"polr", "cart"}));
    assert((DefaultImputeVariables(miceDf) == std::vector<std::string>{"age", "group", "rating"}));
    assert((DefaultPredictorVariables(miceDf) == std::vector<std::string>{"age", "group", "rating"}));

    std::string message;
    DataColumn *mpg = FindDataColumnInDataFrame(cars, "mpg");
    assert(mpg != nullptr);
    assert(DataColumnAllowsNumeric(*mpg, &message));
    assert(SetDataColumnType(*mpg, "factor", &message));
    assert(mpg->type == "factor");
    assert(SetDataColumnType(*mpg, "numeric", &message));
    assert(mpg->type == "numeric");
    assert(SetDataColumnDecimals(*mpg, 2, &message));
    assert(mpg->decimals == 2);
    assert(DisplayValueForCell(*mpg, 0) == "21.00");
    mpg->displayValues = {"twenty-one", "twenty-two"};
    assert(DisplayValueForCell(*mpg, 0) == "twenty-one");
    DataColumn missingCol;
    missingCol.name = "missing";
    missingCol.values = {"NA"};
    std::vector<std::string> rowValues = DisplayValuesForColumnsAtRow({mpg, &missingCol}, 0);
    assert(rowValues.size() == 2);
    assert(rowValues[0] == "twenty-one");
    assert(rowValues[1] == "NA");
    std::vector<std::string> emptyRowValues = DisplayValuesForColumnsAtRow({}, 0, "Missing");
    assert(emptyRowValues.size() == 1);
    assert(emptyRowValues[0] == "Missing");
    DataColumn pointLabelCol;
    pointLabelCol.name = "case_label";
    pointLabelCol.values = {"Case 1", "", "NA", "Case 4"};
    pointLabelCol.displayValues = {"Display 1", "", "Display NA", "Display 4", "Extra display"};
    std::map<int, std::string> labelMap = RowLabelMapForColumn(pointLabelCol);
    assert(labelMap.size() == 4);
    assert(labelMap[1] == "Display 1");
    assert(labelMap[3] == "Display NA");
    assert(labelMap[4] == "Display 4");
    assert(labelMap[5] == "Extra display");
    mpg->displayValues.clear();
    assert(!SetDataColumnDecimals(*mpg, 13, &message));
    assert(SetDataColumnDescription(*mpg, "Miles per gallon"));
    assert(mpg->description == "Miles per gallon");

    DataColumn *am = FindDataColumnInDataFrame(cars, "am");
    assert(am != nullptr);
    am->values = {"0", "manual"};
    assert(!SetDataColumnType(*am, "numeric", &message));
    assert(!IsValidVariableName("bad|name", &message));
    assert(SafeDataColumnSuffix("mpg vs wt [cars]!") == "mpg_vs_wt_cars");
    assert(SafeDataColumnSuffix("___") == "plot");
    assert(SafeDataColumnSuffix("abcdefghijklmnopqrstuvwxyz0123456789") == "abcdefghijklmnopqrstuvwxyz012345");
    assert(SafeDatasetName(" cars data.sav ") == "cars data.sav");
    assert(SafeDatasetName("bad///name\tfile") == "bad name file");
    assert(SafeDatasetName("!!!") == "dataset");
    assert(SafeDatasetNameForPath("/tmp/cars data.sav") == "cars data");
    assert(SafeDatasetNameForPath("C:\\data\\bad///name.csv") == "name");
    assert(SafeDatasetNameForPath("/tmp/!!!.sav") == "dataset");
    assert(UniqueDatasetName("cars", {"cars", "cars 2"}) == "cars 3");
    assert(UniqueDatasetName("", {"dataset"}) == "dataset 2");
    assert(UniqueDataColumnName(cars, "am") == "am_2");
    assert(UniqueDataColumnName(cars, "new_column") == "new_column");

    assert(RenameDataFrameColumn(cars, "mpg", "fuel", &message));
    assert(FindDataColumnInDataFrame(cars, "fuel") != nullptr);
    assert(!RenameDataFrameColumn(cars, "fuel", "am", &message));
    std::vector<rlispstat::core::VariableViewRow> rows = VariableViewRowsForDataFrame(cars);
    assert(rows.size() == 2);
    assert(rows[0].name == "fuel");
    std::string info = VariableInformationText(cars, "fuel");
    assert(info.find("Variable: fuel") != std::string::npos);
    assert(info.find("Dataset: cars") != std::string::npos);
    assert(info.find("Analysis type: numeric") != std::string::npos);
    assert(info.find("Description: Miles per gallon") != std::string::npos);
    assert(info.find("Displayed decimals: 2") != std::string::npos);
    assert(info.find("Rows: 2") != std::string::npos);
    assert(info.find("Missing values: 0") != std::string::npos);
    assert(info.find("Distinct preview: 21, 22") != std::string::npos);
    assert(VariableInformationText(cars, "missing").find("was not found") != std::string::npos);

    assert(DerivedDataColumnKindIsSupported("selection"));
    assert(DerivedDataColumnKindIsSupported("color"));
    assert(!DerivedDataColumnKindIsSupported("weights"));
    std::string createdName;
    assert(AddDerivedDataColumn(cars,
                                "mpg vs wt [cars]!",
                                "selection",
                                std::set<int>{2},
                                std::map<int, std::string>(),
                                &createdName,
                                &message));
    assert(createdName == "selected_from_mpg_vs_wt_cars");
    const DataColumn *selectionCol = FindDataColumnInDataFrame(cars, createdName);
    assert(selectionCol != nullptr);
    assert(selectionCol->type == "factor");
    assert(selectionCol->values.size() == 2);
    assert(selectionCol->values[0] == "not_selected");
    assert(selectionCol->values[1] == "selected");
    assert(AddDerivedDataColumn(cars,
                                "data sheet",
                                "color",
                                std::set<int>(),
                                std::map<int, std::string>{{1, "orange"}, {2, ""}},
                                &createdName,
                                &message));
    const DataColumn *colorCol = FindDataColumnInDataFrame(cars, createdName);
    assert(colorCol != nullptr);
    assert(colorCol->name == "point_color_from_data_sheet");
    assert(colorCol->values[0] == "orange");
    assert(colorCol->values[1] == "default");
    assert(!AddDerivedDataColumn(cars,
                                 "bad",
                                 "weights",
                                 std::set<int>(),
                                 std::map<int, std::string>(),
                                 nullptr,
                                 &message));

    DataFrameModel imputed;
    imputed.group = "imp";
    imputed.rows = 3;
    imputed.datasetType = "multiple_imputation";
    imputed.imputationCount = 5;
    imputed.imputationDisplayMode = "all";
    DataColumn age;
    age.name = "age";
    age.type = "numeric";
    age.values = {"18", "NA", "23"};
    age.imputedMissing = {false, true, false};
    age.imputationOriginalSparse[1] = "NA";
    age.imputationValuesSparse.resize(2);
    age.imputationValuesSparse[0][1] = "20";
    age.imputationValuesSparse[1][1] = "21";
    imputed.columns.push_back(age);
    DataColumn score;
    score.name = "score";
    score.type = "numeric";
    score.values = {"1", "2", "NA"};
    imputed.columns.push_back(score);

    const DataColumn *ageCol = FindDataColumnInDataFrame(imputed, "age");
    const DataColumn *scoreCol = FindDataColumnInDataFrame(imputed, "score");
    assert(ageCol != nullptr);
    assert(scoreCol != nullptr);
    assert(!DataFrameCellIsImputed(imputed, *ageCol, 0));
    assert(DataFrameCellIsImputed(imputed, *ageCol, 1));
    assert(ImputationVersionCountForCell(imputed, *ageCol) == 5);
    assert(OriginalImputationValueForCell(*ageCol, 1) == "NA");
    assert(ImputationVersionValueForCell(*ageCol, 1, 0) == "20");
    assert(ImputationVersionValueForCell(*ageCol, 1, 1) == "21");
    assert(DisplayValueForDataFrameCell(imputed, *ageCol, 1) == "20 | 21 | NA | NA | NA");
    assert(DataFrameShowsAllImputations(imputed));
    assert(DataFrameWindowTitle("imp") == "Data Sheet - imp");
    assert(MiceVariableStatusText(age, imputed.rows) == "numeric, 1 missing");
    DataColumn allMissing = age;
    allMissing.values = {"NA", "NA", "NA"};
    assert(MiceVariableStatusText(allMissing, imputed.rows) == "numeric, 3 missing, all missing");
    DataColumn idColumn = score;
    idColumn.name = "id";
    idColumn.type = "character";
    idColumn.values = {"1", "2", "3"};
    assert(MiceVariableStatusText(idColumn, imputed.rows) == "character, 0 missing, id-like");
    DataColumn unsupportedColumn = score;
    unsupportedColumn.type = "date";
    unsupportedColumn.values = {"1", "1", "2"};
    assert(MiceVariableStatusText(unsupportedColumn, imputed.rows) == "date, 0 missing, unsupported");
    assert(MiceDatasetSummaryText(imputed, 2, 1) ==
           "imp: 3 rows, 2 variables, 2 variables with missing values, 1 selected for imputation.");
    assert(MiceSelectionSummaryText(2, 3) == "2 variables to impute, 3 predictors selected.");
    assert(DataSheetChooseDataColumnStatus() == "Choose a data column, not the row-number column.");
    assert(VariableInformationDialogTitle() == "Variable Information");
    assert(NoActiveDatasetTitle() == "No Active Dataset");
    assert(ActiveDatasetTitle() == "Active Dataset");
    assert(ActiveDatasetUnavailableStatus() == "The active dataset is no longer available.");
    assert(ActiveDatasetSummaryText("cars", 32, 11, 4) ==
           "Active dataset: cars\nRows: 32\nVariables: 11\nSelected rows: 4");
    assert(ImportOrRegisterDatasetStatus() == "Import or register a dataset first.");
    assert(NoActiveDatasetImportOrRegisterStatus() ==
           "No active dataset. Import or register a dataset first.");
    assert(VariableViewTitle() == "Variable View");
    assert(VariableViewWindowTitle("imp") == "Variable View - imp");
    assert(VariableViewInstructionText() ==
           "Click Name or Description to edit. Click Type, Decimals, or Role to choose an action.");
    assert(VariableDescriptionDialogTitle("age") == "Description for age");
    assert(VariableDescriptionDialogInformationText() ==
           "Edit the explanatory text used by Variable Information and the Variable View.");
    assert(DatasetNotRegisteredStatus("cars") == "Dataset `cars` is not registered.");
    assert(DataSheetOpenTitle() == "Open Data Sheet");
    assert(DataSheetBackendPayloadUnavailableStatus() ==
           "No backend data payload is available for this dataset yet. Create a new plot from R so the data can be sent to the native workbench.");
    assert(VariableTypeTitle() == "Variable Type");
    assert(NoActiveVariableForTypeStatus() ==
           "No active variable is available. Open a data sheet and right-click a column.");
    ChooseLabelColumnDialogState labelDialog = BuildChooseLabelColumnDialogState();
    assert(labelDialog.statusTitle == "Label Column");
    assert(labelDialog.title == "Choose Label Column");
    assert(labelDialog.informativeText ==
           "Labels from this column can be shown for selected points or for all points in scatterplots.");
    assert(labelDialog.chooseButtonTitle == "Choose");
    assert(labelDialog.cancelButtonTitle == "Cancel");
    assert(labelDialog.noneOptionTitle == "(none)");
    assert(DatasetDialogLabel("cars") == "Dataset: cars");
    assert(NoVariableOptionTitle() == "(none)");
    assert(ChooseLabelColumnDatasetLabel("cars") == "Dataset: cars");
    MissingDataImputationDialogState imputationDialog = BuildMissingDataImputationDialogState();
    assert(imputationDialog.title == "Multiple Imputation");
    assert(imputationDialog.failedTitle == "Imputation Failed");
    assert(imputationDialog.informativeText ==
           "Runs mice on the selected dataset and creates a linked multiple-imputation data sheet with imputed cells marked.");
    assert(imputationDialog.runButtonTitle == "Run imputation");
    assert(imputationDialog.cancelButtonTitle == "Cancel");
    assert(imputationDialog.datasetLabel == "Dataset");
    assert(imputationDialog.imputationsLabel == "Imputations");
    assert(imputationDialog.iterationsLabel == "Iterations");
    assert(imputationDialog.seedLabel == "Seed");
    assert(imputationDialog.seedPlaceholder == "optional");
    assert(imputationDialog.openDataSheetTitle == "Open imputed data sheet after fitting");
    assert(imputationDialog.deselectAllTitle == "Deselect all");
    assert(imputationDialog.deselectPredictorsTitle == "Deselect predictors");
    assert(imputationDialog.instructionHint ==
           "Select variables to impute, choose a mice method, and select predictors. ID-like columns are left unchecked by default.");
    assert(imputationDialog.methodHint ==
           "Methods: numeric pmm/norm/cart; binary factors logreg; multi-level factors polyreg/cart.");
    assert(imputationDialog.imputeColumnTitle == "Impute");
    assert(imputationDialog.typeMissingColumnTitle == "Type / missing");
    assert(imputationDialog.methodColumnTitle == "Method");
    assert(imputationDialog.predictorColumnTitle == "Predictor");
    assert(imputationDialog.unavailableDatasetRowText == "Dataset is no longer available.");
    assert(imputationDialog.unsupportedMethodTitle == "unsupported");
    assert(imputationDialog.predictorUseTitle == "Use");
    assert(imputationDialog.noImputeVariablesStatus ==
           "Select at least one variable with missing values to impute.");
    assert(imputationDialog.noPredictorVariablesStatus ==
           "Select at least one predictor variable.");
    assert(imputationDialog.datasetUnavailableStatus ==
           "The selected dataset is no longer available.");
    assert(NativeImportDialogTitle() == "Import Data");
    assert(NativeImportFailedTitle() == "Import Failed");
    assert(NativeImportSupportedFormatsText() ==
           "CSV, TSV/TXT, Excel (.xls/.xlsx), SPSS (.sav/.zsav), Stata (.dta), "
           "SAS (.sas7bdat/.xpt), and R (.rds/.rda/.RData) files.");
    std::vector<std::string> importExtensions = NativeImportAllowedFileExtensions();
    assert(importExtensions.size() == 13);
    assert(importExtensions.front() == "csv");
    assert(importExtensions.back() == "RData");
    assert(std::find(importExtensions.begin(), importExtensions.end(), "sav") != importExtensions.end());
    assert(std::find(importExtensions.begin(), importExtensions.end(), "xlsx") != importExtensions.end());
    const std::vector<rlispstat::core::NativeImportFileFilter> importFilters =
        rlispstat::core::NativeImportFileFilters();
    assert(importFilters.size() == 7);
    assert(importFilters.front().identifier == "all");
    assert(importFilters.front().extensions == importExtensions);
    assert(importFilters[1].identifier == "spss");
    assert((importFilters[1].extensions == std::vector<std::string>{"sav", "zsav"}));
    assert(importFilters.back().identifier == "r");
    assert(NativeImportTemporaryScriptFailedStatus() == "Could not create temporary import script.");
    assert(NativeImportRscriptLaunchFailedStatus() == "Could not run Rscript for data import.");
    assert(NativeImportRscriptFailedStatus() == "Rscript failed while importing the selected file.");
    assert(NativeImportPayloadMissingStatus() == "The import reader did not return a dataset payload.");
    assert(NativeImportDatasetLoadedStatus("cars") == "Imported `cars`.");
    assert(NativeImportDatasetLoadedStatus("cars", 32, 11) ==
           "Imported `cars` with 32 rows and 11 variables.");
    assert(NativeRDataPayloadTemporaryFileFailedStatus() == "Could not create temporary R data payload.");
    assert(NativeMiceTemporaryScriptFailedStatus() == "Could not create temporary imputation script.");
    assert(NativeMiceRscriptLaunchFailedStatus() == "Could not run Rscript for multiple imputation.");
    assert(NativeMiceRscriptFailedStatus() == "Rscript failed while running multiple imputation.");
    assert(NativeMicePayloadMissingStatus() == "The imputation script did not return a dataset payload.");
    assert(NativeMiceCreatedDatasetStatus("imp", 5) == "Created imputed dataset `imp` with 5 imputations.");
    assert(NativeMiceCreatedDatasetStatus("imp", 0) == "Created imputed dataset `imp` with 1 imputations.");
    assert(NativePooledAnalysisTemporaryScriptFailedStatus() == "Could not create temporary R analysis script.");
    assert(NativePooledAnalysisRscriptLaunchFailedStatus() ==
           "Could not run Rscript for the multiple-imputation analysis.");
    assert(NativePooledAnalysisRscriptFailedStatus() ==
           "Rscript failed while computing the multiple-imputation analysis.");
    assert(NativePooledAnalysisPayloadMissingStatus() ==
           "The R/mice analysis did not return a native table payload.");
    assert(NativePooledAnalysisOpenedStatus() == "Opened pooled multiple-imputation analysis.");
    assert(DataFrameStatusText(imputed, 2) ==
           "imp: 3 rows, 2 variables, 2 selected | Showing compact all-imputation preview across 5 imputations");
    assert(DataFrameImputationTooltipText(imputed, *ageCol, 1) ==
           "Originally missing. Showing a compact preview across 5 imputations.");

    DataFrameModel active = imputed;
    active.imputationDisplayMode = "version";
    active.activeImputationVersion = 2;
    assert(DisplayValueForDataFrameCell(active, *FindDataColumnInDataFrame(active, "age"), 1) == "21");
    assert(DataFrameStatusText(active, 1) ==
           "imp: 3 rows, 2 variables, 1 selected | Showing imputation: 2 of 5");
    assert(DataFrameImputationTooltipText(active, *FindDataColumnInDataFrame(active, "age"), 1) ==
           "Originally missing. Imputed value in imputation 2.");
    active.imputationDisplayMode = "original";
    assert(DisplayValueForDataFrameCell(active, *FindDataColumnInDataFrame(active, "age"), 1) == "NA");
    assert(DataFrameStatusText(active, 0) ==
           "imp: 3 rows, 2 variables, 0 selected | Showing original incomplete data");
    assert(DataFrameImputationTooltipText(active, *FindDataColumnInDataFrame(active, "age"), 1) ==
           "Originally missing. Showing original incomplete data.");
    assert(DataFrameImputationTooltipText(active, *FindDataColumnInDataFrame(active, "score"), 0).empty());
    active.imputationDisplayMode = "version";
    active.activeImputationVersion = 1;
    ApplyImputationDisplayModeToStoredValues(active);
    assert(FindDataColumnInDataFrame(active, "age")->values[1] == "20");

    assert(closeEnough(NumericValueForDataFrameCellVersion(imputed, *ageCol, 0, 0), 18.0));
    assert(closeEnough(NumericValueForDataFrameCellVersion(imputed, *ageCol, 1, 0), 20.0));
    assert(closeEnough(NumericValueForDataFrameCellVersion(imputed, *ageCol, 1, 1), 21.0));
    assert(!std::isfinite(NumericValueForDataFrameCellVersion(imputed, *scoreCol, 2, 0)));
    std::vector<int> completeDataRows = CompleteRowsForDataColumns(imputed, {ageCol, scoreCol}, 0);
    assert((completeDataRows == std::vector<int>{1, 2}));
    std::vector<int> completeVectorRows = CompleteRowsForNumericVectors({
        {1.0, NAN, 3.0, 4.0},
        {5.0, 6.0, 7.0}
    });
    assert((completeVectorRows == std::vector<int>{1, 3}));
    assert(CompleteRowsForNumericVectors({}).empty());

    assert(ScatterImputationUncertaintyModeIsValid("central80"));
    assert(ScatterImputationUncertaintyModeIsValid("iqr"));
    assert(ScatterImputationUncertaintyModeIsValid("sd"));
    assert(ScatterImputationUncertaintyModeIsValid("se"));
    assert(!ScatterImputationUncertaintyModeIsValid("range"));
    assert(NormalizedScatterImputationUncertaintyMode("unknown") == "central80");
    assert(ScatterImputationUncertaintyDisplayName("iqr") == "IQR");
    NumericImputationRange central = NumericImputationRangeForCell(imputed, *ageCol, 1, "central80");
    assert(central.any);
    assert(closeEnough(central.center, 20.5));
    assert(closeEnough(central.min, 20.1));
    assert(closeEnough(central.max, 20.9));
    NumericImputationRange iqr = NumericImputationRangeForCell(imputed, *ageCol, 1, "iqr");
    assert(iqr.any);
    assert(closeEnough(iqr.min, 20.25));
    assert(closeEnough(iqr.max, 20.75));
    NumericImputationRange se = NumericImputationRangeForCell(imputed, *ageCol, 1, "se");
    assert(se.any);
    assert(closeEnough(se.min, 20.0));
    assert(closeEnough(se.max, 21.0));
    NumericImputationRange observed = NumericImputationRangeForCell(imputed, *ageCol, 0, "central80");
    assert(!observed.any);

    DatasetRegistry registry;
    assert(registry.empty());
    registry.registerDataset(cars);
    registry.registerDataset(imputed);
    assert(registry.size() == 2);
    assert(registry.contains("cars"));
    assert(registry.uniqueDatasetName("cars") == "cars 2");
    assert(registry.activeDatasetGroup() == "cars");
    assert(registry.setActiveDataset("imp"));
    assert(registry.activeDatasetGroup() == "imp");
    assert(registry.activeDataset() != nullptr);
    assert(!registry.setActiveDataset("unknown"));
    assert(registry.erase("imp"));
    assert(registry.activeDatasetGroup() == "cars");
    DataFrameModel extra;
    extra.group = "manual";
    extra.rows = 1;
    registry.registerDataset(extra);
    registry.rememberActiveDatasetGroup("manual");
    assert(registry.contains("manual"));
    assert(registry.activeDatasetGroup() == "manual");
    registry.clear();
    assert(registry.empty());
    assert(registry.activeDataset() == nullptr);

    assert(NoActiveDatasetStatus() == "No active dataset is available.");
    assert(NoRegisteredDatasetGroupStatus() == "No registered dataset/group is available.");
    assert(DatasetNotAvailableStatus() == "The dataset is not available.");
    assert(DatasetColumnNotFoundStatus("mpg", "cars") == "Column `mpg` was not found in dataset `cars`.");
    assert(DatasetVariableNameUnchangedStatus() == "Variable name unchanged.");
    assert(DatasetPointLabelsResetStatus() == "Point labels reset.");
    assert(DatasetPointLabelColumnSetStatus("label") == "Point label column set to `label`.");
    assert(DatasetTemporaryDataWriteFailedStatus() == "Could not write temporary GLM data.");
    assert(VariableDecimalsInfoText() == "Use automatic formatting or choose a fixed number of displayed decimals.");
    assert(NoRowsSelectedStatus() == "No rows selected.");
    assert(NoCompleteCasesStatus() == "No complete cases for the selected variables and missing-data mode.");
    assert(DatasetForPlotNotAvailableStatus() == "The dataset for this plot is not available.");
    assert(VariableForPlotNotAvailableStatus() == "The variable for this plot is not available.");
    assert(SelectedVariableNotAvailableStatus() == "The selected variable is not available.");
    assert(NoVariablesToSummarizeStatus() == "This plot does not have variables that can be summarized.");
    assert(Table1HistogramRequiresNumericStatus() == "Histogram requires a numeric or ordinal variable.");
    assert(Table1BarplotRequiresValueStatus() == "Bar chart requires at least one observed value.");
    assert(Table1BoxplotRequiresNumericStatus() == "Boxplot requires a numeric or ordinal variable.");
    assert(DatasetNotAvailableForRecomputeStatus() == "Dataset is not available for native recompute.");
    assert(NativeBackendCannotInspectRStatus() == "The native backend cannot inspect R environments directly. Run ls_refresh_r_dataframes() from R, then use ls_set_active_dataset().");
    assert(VariableDecimalsAutoButtonTitle() == "Automatic");
    assert(ScopeAllDataTitle() == "All data");
    assert(ScopeSelectedRowsTitle() == "Selected rows");
    assert(ScopeUnselectedRowsTitle() == "Unselected rows");
    assert(ScopeCompareSelectedAllTitle() == "Compare selected/all");
    assert(AlertOKButtonTitle() == "OK");
    assert(AlertApplyButtonTitle() == "Apply");
    assert(AlertCancelButtonTitle() == "Cancel");
    assert(RFileExtension() == "R");
    assert(PDFFileExtension() == "PDF");
    assert(PNGFileExtension() == "PNG");
    assert(ActiveDatasetWindowTitle() == "Active Dataset");
    assert(RefreshRDataFramesWindowTitle() == "Refresh R Data Frames");
    assert(HashColumnHeader() == "#");
    return 0;
}
