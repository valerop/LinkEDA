#ifndef RLISPSTAT_CORE_TABLE1_MODEL_H
#define RLISPSTAT_CORE_TABLE1_MODEL_H

#include "dataset_model.h"
#include "plot_geometry.h"

#include <map>
#include <set>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

using LevelKey = std::string;

struct ContingencyRowKey {
    std::vector<LevelKey> x_levels;
    bool operator<(const ContingencyRowKey &other) const { return x_levels < other.x_levels; }
    bool operator==(const ContingencyRowKey &other) const { return x_levels == other.x_levels; }
};

struct ContingencyCellKey {
    ContingencyRowKey row;
    LevelKey split_level;
    bool operator<(const ContingencyCellKey &other) const {
        if (row < other.row) return true;
        if (other.row < row) return false;
        return split_level < other.split_level;
    }
};

enum class LinkedSelectionKind {
    None,
    Row,
    Cell,
    Column
};

enum class ContingencySelectionKind {
    None,
    RowPrefix,
    LeafRow,
    SplitColumn,
    Cell
};

enum class AggregateCoverage {
    Empty,
    None,
    Partial,
    Full
};

struct ContingencySelection {
    ContingencySelectionKind kind = ContingencySelectionKind::None;
    std::size_t rowIndex = 0;
    std::size_t columnIndex = 0;
    std::vector<LevelKey> x_prefix;
};

struct Table1DisplayRow {
    int rowIndex = 0;
    std::string rowType;
    std::string variable;
    std::string level;
    std::string label;
    std::vector<std::string> stubValues;
    std::vector<std::string> values;
    std::vector<int> rowRows;
    std::vector<std::vector<int>> cellRows;
    ContingencyRowKey contingencyRowKey;
    std::string p;
    std::string test;
    std::string detail;
    // Optional machine-readable values used by CSV export.  The native view
    // keeps the formatted strings in `values`, while exports can retain the
    // full precision received from R.
    std::vector<std::string> rawValues;
    // Optional semantic components for numeric summaries.  Each entry is
    // aligned with `values` and may contain mean, sd, se, ci95, median, q1,
    // and q3.  Keeping these separate lets the native views change the
    // displayed statistics without re-running the analysis or MI pooling.
    std::vector<std::map<std::string, std::string>> statisticValues;
    // Full-precision values supplied by R. Publication code must use these,
    // never the locale-formatted strings displayed by the native table.
    std::vector<std::map<std::string, double>> rawStatisticValues;
};

struct Table1DisplayPreferences {
    bool showMean = true;
    bool showSD = true;
    bool showSE = true;
    bool showCI95 = false;
    bool showMedian = true;
    bool showQuartiles = true;
    bool showMissing = true;
    bool showOverall = true;
    bool showP = true;
    bool showTest = true;
};

struct Table1DisplayState {
    std::string id;
    std::string datasetId;
    std::string title = "Table 1. Descriptive statistics";
    std::string subtitle;
    AnalysisScope dataScope;
    bool dataScopeCaptured = false;
    std::string statusText;
    std::string groupVariable;
    std::vector<std::string> variables;
    std::map<std::string, std::string> variableTypes;
    std::vector<std::string> columns;
    std::vector<std::string> stubHeaders;
    std::vector<Table1DisplayRow> rows;
    std::vector<std::string> footnotes;
    std::vector<std::string> warnings;
    std::string groupSpanningHeader;
    int groupSpanningColumnCount = 0;
    std::string groupVariableLabel = "Group";
    std::string tableType = "table1";
    std::string sourcePlotId;
    bool linkEnabled = false;
    bool showP = false;
    bool showTest = false;
    bool nativeGenerated = false;
    bool needsRFit = false;
    std::string nestedDisplayMode = "count_percent";
    OutputCodeReference codeReference;
    // Editing control for diagnostics of one explicitly linked model.
    std::string missingnessSource;
    std::map<std::string, std::vector<int>> missingnessPatternRows;
    std::string linkedModelOutputId;
    std::vector<std::string> addVariableOptions;
};

struct Table1ReportLayout {
    double width = 720.0;
    double margin = 14.0;
    double variableWidth = 200.0;
    double pWidth = 0.0;
    double testWidth = 0.0;
    double rowHeight = 24.0;
    double stubWidth = 200.0;
    double stubArea = 200.0;
    double valueArea = 0.0;
    double columnWidth = 78.0;
    std::vector<double> valueColumnWidths;
    double headerOffset = 66.0;
    int valueColumnCount = 1;
    int stubColumnCount = 1;
    bool hasGroup = false;
    bool hasStubs = false;
    bool hasSpanningHeader = false;
};

struct Table1ContextMenuTitles {
    std::string root;
    std::string copyTable;
    std::string exportMenu;
    std::string exportCSV;
    std::string exportMarkdown;
    std::string exportPDF;
    std::string addVariable;
    std::string noAvailableVariables;
    std::string changeVariable;
    std::string showVariableInformation;
    std::string groupingVariable;
    std::string clearGroupingVariable;
};

struct Table1PlotMenuOption {
    std::string command;
    std::string title;
};

Table1ContextMenuTitles DefaultTable1ContextMenuTitles();
std::string Table1RemoveVariableTitle(const std::string &variable);
std::string Table1ReplaceVariableTitle(const std::string &variable);
std::string Table1TypeMenuTitle(const std::string &type);
std::string Table1HistogramMenuTitle(const std::string &variable);
std::string Table1BarChartMenuTitle(const std::string &variable);
std::string Table1BoxplotMenuTitle(const std::string &variable,
                                   const std::string &groupVariable);
std::string Table1VariableAnalysisType(const Table1DisplayState &state,
                                       const std::string &variable);
std::vector<Table1PlotMenuOption> Table1PlotMenuOptions(
    const std::string &variable,
    const std::string &analysisType,
    const std::string &groupVariable);
std::string Table1Subtitle(const Table1DisplayState &state,
                           bool includeUngroupedN = false);
bool Table1RowIsIndented(const Table1DisplayRow &row);
bool Table1RowIsParent(const Table1DisplayRow &row);
bool Table1RowIsSubVariable(const Table1DisplayRow &row);
bool Table1RowHasValues(const Table1DisplayRow &row);
std::string Table1DisplayLabelText(const Table1DisplayRow &row);
Table1ReportLayout BuildTable1ReportLayout(const Table1DisplayState &state,
                                           double width);
double Table1NaturalWidth(const Table1DisplayState &state);
Table1DisplayState Table1ApplyDisplayPreferences(
    const Table1DisplayState &source,
    const Table1DisplayPreferences &preferences);
double Table1PreferredHeight(const Table1DisplayState &state);
int Table1ReportRowAtPoint(const Table1DisplayState &state,
                           const Table1ReportLayout &layout,
                           const Point &point);
int Table1ReportColumnAtPoint(const Table1DisplayState &state,
                              const Table1ReportLayout &layout,
                              const Point &point);
bool Table1ReportPointIsColumnHeader(const Table1ReportLayout &layout,
                                     const Point &point);
ContingencySelection Table1ContingencySelectionAtPoint(
    const Table1DisplayState &state,
    const Table1ReportLayout &layout,
    const Point &point,
    int rowIndex);
std::vector<double> Table1NumericValuesForRows(const DataColumn &col,
                                               const std::vector<int> &rows);
std::string Table1MeanSd(const std::vector<double> &values);
double Table1Quantile(std::vector<double> values, double p);
std::string Table1MedianIqr(const std::vector<double> &values);
std::string Table1InferType(const DataColumn &col,
                            const std::map<std::string, std::string> &overrides);
std::vector<std::string> Table1Levels(const DataColumn &col,
                                      const std::string &type);
std::string Table1CountPercent(const DataColumn &col,
                               const std::vector<int> &rows,
                               const std::string &level);
Table1DisplayState Table1PendingStateForDataFrame(
    const DataFrameModel &df,
    const std::string &id,
    std::vector<std::string> variables,
    const std::string &groupVariable,
    const std::map<std::string, std::string> &types,
    const AnalysisScope *dataScope = nullptr);

Table1DisplayState Table1StateForDataFrame(
    const DataFrameModel &df,
    const std::string &id,
    std::vector<std::string> variables,
    const std::string &groupVariable,
    const std::map<std::string, std::string> &types,
    const AnalysisScope *dataScope = nullptr);

Table1DisplayState NestedContingencyTableStateForDataFrame(
    const DataFrameModel &df,
    const std::string &id,
    const std::vector<std::string> &xVariables,
    const std::string &splitVariable,
    const std::string &displayMode = "count_percent",
    const AnalysisScope *dataScope = nullptr);
// Move a row variable to the column split in one specification change. If a
// split already exists, it takes the moved variable's former row position.
bool MoveContingencyRowToColumn(std::vector<std::string> &rowVariables,
                                std::string &columnVariable,
                                const std::string &rowVariable);
std::set<int> ContingencyRowsForSelection(const Table1DisplayState &state,
                                          const ContingencySelection &selection);
std::set<int> ContingencyRowsForRowPrefix(const Table1DisplayState &state,
                                          const std::vector<LevelKey> &prefix);
std::set<int> ContingencyRowsForSplitColumn(const Table1DisplayState &state,
                                            std::size_t columnIndex);
std::set<int> ContingencyRowsForCell(const Table1DisplayState &state,
                                     std::size_t rowIndex,
                                     std::size_t columnIndex);
AggregateCoverage AggregateCoverageForRows(const std::vector<int> &sourceRows,
                                           const std::set<int> &selectedRows);
AggregateCoverage ContingencyRowCoverage(const Table1DisplayState &state,
                                         std::size_t rowIndex,
                                         const std::set<int> &selectedRows);
AggregateCoverage ContingencyCellCoverage(const Table1DisplayState &state,
                                          std::size_t rowIndex,
                                          std::size_t columnIndex,
                                          const std::set<int> &selectedRows);
std::string Table1PlainText(const Table1DisplayState &state);
std::string Table1CSVText(const Table1DisplayState &state);
std::string Table1MarkdownText(const Table1DisplayState &state);
std::string Table1ExportedStatus();
std::string Table1ExportFailedStatus();
std::string Table1ExportedPDFStatus();
std::string Table1ExportPDFFailedStatus();
std::string Table1CopiedStatus();
std::string Table1StatusFieldHint();
std::string Table1WindowTitle();
std::string Table1ExportPDFPanelTitle();
std::string Table1PlotWindowTitle();
std::string Table1ExportCSVDialogTitle();
std::string Table1ExportMarkdownDialogTitle();
std::string Table1ReportVariableHeader();
std::string Table1ReportPHeader();
std::string Table1ReportTestHeader();

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_TABLE1_MODEL_H
