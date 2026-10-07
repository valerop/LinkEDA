#ifndef RLISPSTAT_CORE_MEAN_COMPARISON_MODEL_H
#define RLISPSTAT_CORE_MEAN_COMPARISON_MODEL_H

#include "analysis_scope.h"
#include "provenance_model.h"

#include <map>
#include <optional>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

struct DataColumn;
bool MeanComparisonBinaryResponse(const DataColumn &column);
bool MeanComparisonBinaryPairCompatible(const DataColumn &first, const DataColumn &second);

enum class MeanComparisonCellFormat {
    Text,
    Integer,
    Number,
    PValue,
    ConfidenceInterval
};

enum class MeanComparisonRowKind {
    Result,
    Descriptive,
    Warning,
    GroupHeader
};

enum class MeanComparisonKind {
    OneSample,
    IndependentSamples,
    PairedSamples,
    OneWayAnova
};

enum class MeanComparisonMethod {
    OneSampleT,
    WilcoxonSignedRank,
    WelchT,
    StudentT,
    MannWhitney,
    WelchAnova,
    ClassicalAnova,
    KruskalWallis
};

enum class AlternativeHypothesis {
    TwoSided,
    Greater,
    Less
};

enum class MultipleTestingAdjustment {
    None,
    Holm,
    Bonferroni
};

struct PairedVariableSpecification {
    std::string pairId;
    std::string firstVariableId;
    std::string secondVariableId;
};

struct MeanComparisonSpecification {
    MeanComparisonKind kind = MeanComparisonKind::OneSample;
    std::vector<std::string> dependentVariableIds;
    std::optional<std::string> groupingVariableId;
    std::vector<PairedVariableSpecification> pairs;
    double testValue = 0.0;
    // One-sample analyses may use a different null value for every response.
    // testValue remains the convenient "set all" default used by the header.
    std::map<std::string, double> testValues;
    AlternativeHypothesis alternative = AlternativeHypothesis::TwoSided;
    double confidenceLevel = 0.95;
    MeanComparisonMethod method = MeanComparisonMethod::OneSampleT;
    MultipleTestingAdjustment pAdjustment = MultipleTestingAdjustment::Holm;
    std::vector<std::string> groupOrderIds;
};

struct MeanComparisonCell {
    std::optional<double> numericValue;
    std::optional<double> numericUpperValue;
    std::optional<std::string> textValue;
    bool notApplicable = false;
};

struct MeanComparisonColumn {
    std::string key;
    std::string title;
    MeanComparisonCellFormat format = MeanComparisonCellFormat::Number;
    bool visible = true;
};

struct MeanComparisonRow {
    std::string rowId;
    std::string label;
    MeanComparisonRowKind kind = MeanComparisonRowKind::Result;
    std::vector<MeanComparisonCell> cells;
    std::vector<int> originalRowIndices;
    std::string variable;
    std::string secondVariable;
    std::string group;
};

struct MeanComparisonTable {
    std::string tableId;
    std::string title;
    std::string stubTitle;
    std::vector<MeanComparisonColumn> columns;
    std::vector<MeanComparisonRow> rows;
    std::vector<std::string> notes;
    std::vector<std::string> warnings;
    std::string firstGroup;
    std::string secondGroup;
    std::string method;
    MultipleTestingAdjustment pAdjustment = MultipleTestingAdjustment::None;
    std::size_t adjustmentFamilySize = 0;
};

struct MeanComparisonState {
    std::string id;
    std::string datasetId;
    AnalysisScope dataScope;
    bool dataScopeCaptured = false;
    std::string analysisType;
    std::string title;
    std::string subtitle;
    std::string groupVariable;
    std::string method;
    std::string alternative;
    double confidenceLevel = 0.95;
    double testValue = 0.0;
    std::string firstGroup;
    std::string secondGroup;
    bool multipleImputation = false;
    std::size_t imputationCount = 0;
    std::string poolingMethod;
    MeanComparisonSpecification specification;
    std::size_t adjustmentFamilySize = 0;
    std::vector<MeanComparisonTable> tables;
    std::vector<std::string> warnings;
    AnalysisProvenance provenance;
};

struct MeanComparisonPlotMenuOption {
    std::string title;
    std::string plotKind;
};

struct MeanComparisonPairedForestRow {
    std::string label;
    std::string firstVariable;
    std::string secondVariable;
    double estimate = 0.0;
    double standardError = 0.0;
    double statistic = 0.0;
    double degreesOfFreedom = 0.0;
    double pValue = 0.0;
    double adjustedPValue = 0.0;
    double confidenceLower = 0.0;
    double confidenceUpper = 0.0;
    std::vector<int> originalRowIndices;
};

std::string MeanComparisonAdjustmentName(MultipleTestingAdjustment adjustment);
MultipleTestingAdjustment MeanComparisonAdjustmentFromName(const std::string &name);
std::string MeanComparisonGroupingVariableLine(const std::string &groupVariable);
std::string MeanComparisonSubtitleWithoutGroupingVariable(
    const std::string &subtitle,
    const std::string &groupVariable);
std::vector<MeanComparisonPlotMenuOption> MeanComparisonPlotMenuOptions(
    const std::string &analysisType,
    bool hasSelectedVariable,
    bool hasPairedVariables);
std::vector<MeanComparisonPairedForestRow> MeanComparisonPairedForestRows(
    const MeanComparisonState &state);
std::vector<int> MeanComparisonLinkedRows(const MeanComparisonRow &row);
std::vector<std::string> MeanComparisonDefaultIndependentGroupOrder(
    const std::vector<std::string> &availableLevels);
std::vector<std::string> MeanComparisonIndependentGroupOrderForReference(
    const std::vector<std::string> &availableLevels,
    const std::string &referenceLevel);
bool MeanComparisonIndependentGroupOrderIsValid(
    const std::vector<std::string> &groupOrder,
    const std::vector<std::string> &availableLevels);

bool ParseMeanComparisonBatch(const std::vector<std::string> &args,
                              MeanComparisonState &state,
                              std::string *error = nullptr);
bool MeanComparisonResponseTypeAllowed(const std::string &analysisType, const std::string &type,
    bool binaryCategorical, bool multipleImputation);
PublicationTableSpec MeanComparisonPublicationTable(const MeanComparisonTable &table);
std::string MeanComparisonCellText(const MeanComparisonCell &cell,
                                   MeanComparisonCellFormat format);
std::string MeanComparisonTableText(const MeanComparisonTable &table,
                                    bool selectedOnly = false,
                                    std::size_t selectedRow = 0);
std::string MeanComparisonTableCSV(const MeanComparisonTable &table);
std::string MeanComparisonAllTablesCSV(const MeanComparisonState &state);
double MeanComparisonPreferredStubWidth(const MeanComparisonTable &table);
std::vector<double> MeanComparisonPreferredColumnWidths(const MeanComparisonTable &table);
double MeanComparisonPreferredWidth(const MeanComparisonState &state);
double MeanComparisonPreferredHeight(const MeanComparisonState &state);
bool MeanComparisonIsDescriptiveTable(const MeanComparisonTable &table);
double MeanComparisonVisiblePreferredHeight(const MeanComparisonState &state,
                                             bool showDescriptives,
                                             bool descriptivesOnly = false);

} // namespace core
} // namespace rlispstat

#endif
