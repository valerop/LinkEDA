#include "command_dispatcher.h"
#include "boxplot_model.h"
#include "barplot_model.h"
#include "dataset_protocol.h"
#include "histogram_model.h"
#include "model_terms.h"
#include "scatter_matrix_model.h"
#include "scatterplot_model.h"
#include "string_utils.h"
#include "trellis_scatterplot_model.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdlib>
#include <functional>
#include <iomanip>
#include <sstream>
#include <utility>

namespace rlispstat {
namespace core {

namespace {

bool DataColumnsAreEquivalent(const DataColumn &left, const DataColumn &right)
{
    return left.name == right.name &&
        left.type == right.type &&
        left.displayName == right.displayName &&
        left.description == right.description &&
        left.decimals == right.decimals &&
        left.values == right.values &&
        left.displayValues == right.displayValues &&
        left.definedLevels == right.definedLevels &&
        left.imputedMissing == right.imputedMissing &&
        left.imputationOriginalValues == right.imputationOriginalValues &&
        left.imputationValues == right.imputationValues &&
        left.imputationOriginalSparse == right.imputationOriginalSparse &&
        left.imputationValuesSparse == right.imputationValuesSparse &&
        left.reversibleFactorLevels == right.reversibleFactorLevels;
}

bool DataFramesAreEquivalent(const DataFrameModel &left, const DataFrameModel &right)
{
    if (left.group != right.group ||
        left.rows != right.rows ||
        left.columns.size() != right.columns.size() ||
        left.datasetType != right.datasetType ||
        left.imputationId != right.imputationId ||
        left.sourceDatasetId != right.sourceDatasetId ||
        left.imputationCount != right.imputationCount ||
        left.activeImputationVersion != right.activeImputationVersion ||
        left.imputationDisplayMode != right.imputationDisplayMode ||
        left.imputationProcess != right.imputationProcess ||
        left.syncSessionToken != right.syncSessionToken) {
        return false;
    }
    for (std::size_t column = 0; column < left.columns.size(); ++column) {
        if (!DataColumnsAreEquivalent(left.columns[column], right.columns[column])) {
            return false;
        }
    }
    return true;
}

PublicationValue PublicationText(std::string value)
{
    PublicationValue result;
    result.kind = PublicationValueKind::Text;
    result.text = std::move(value);
    return result;
}

PublicationValue PublicationNumber(double value)
{
    PublicationValue result;
    if (std::isfinite(value)) {
        result.kind = PublicationValueKind::Number;
        result.number = value;
    }
    return result;
}

PublicationValue PublicationInterval(double lower, double upper, int decimals = 3)
{
    if (!std::isfinite(lower) || !std::isfinite(upper)) return {};
    std::ostringstream value;
    value << '[' << std::fixed << std::setprecision(decimals) << lower
          << ", " << upper << ']';
    return PublicationText(value.str());
}

template <typename Row>
bool PublicationModelRowIsNested(const Row &row)
{
    return row.rowType == "reference" || row.rowType == "factor_level" ||
        (ModelTermTypeIsInteraction(row.termType) &&
         row.rowType == "coefficient" && !row.sourceTerm.empty() &&
         row.sourceTerm != row.term);
}

template <typename Row>
std::string PublicationModelRowLabel(const Row &row)
{
    std::string label = row.displayLabel.empty() ? row.term : row.displayLabel;
    if (row.rowType == "reference" && label.find("reference") == std::string::npos)
        label += " (reference)";
    if (PublicationModelRowIsNested(row) &&
        (label.empty() || !std::isspace(static_cast<unsigned char>(label.front())))) {
        label = "  " + label;
    }
    return label;
}

template <typename Row>
std::string PublicationModelTypeLabel(const Row &row)
{
    const bool intercept = row.termType == "intercept" ||
        row.term == "(Intercept)" || row.sourceTerm == "(Intercept)";
    if (intercept || PublicationModelRowIsNested(row)) return {};
    std::string label = row.termType.empty()
        ? std::string{} : ModelTermTypeDisplayName(row.termType);
    std::size_t position = 0;
    while ((position = label.find(" x ", position)) != std::string::npos) {
        label.replace(position, 3, " × ");
        position += 4;
    }
    return label;
}

template <typename Rows>
bool PublicationModelHasHierarchy(const Rows &rows)
{
    return std::any_of(rows.begin(), rows.end(), [](const auto &row) {
        return row.rowType == "reference" || row.rowType == "factor_level" ||
            row.rowType == "factor_parent" || row.rowType == "term_parent" ||
            PublicationModelRowIsNested(row);
    });
}

void AddModelHierarchyFootnote(PublicationTableSpec &table)
{
    table.footnotes.push_back(
        "Reference categories are identified explicitly. Indented rows are "
        "categories or interaction coefficients; parent rows report omnibus tests when available.");
}

std::string RCharacterValues(const std::vector<std::string> &values)
{
    if (values.empty()) return "character()";
    std::ostringstream out;
    out << "c(";
    for (std::size_t index = 0; index < values.size(); ++index) {
        if (index) out << ", ";
        out << ProvenanceRStringLiteral(values[index]);
    }
    out << ")";
    return out.str();
}

std::string RNumericValues(const std::vector<double> &values)
{
    std::ostringstream out;
    out << "c(" << std::setprecision(17);
    for (std::size_t index = 0; index < values.size(); ++index) {
        if (index) out << ", ";
        out << values[index];
    }
    out << ")";
    return out.str();
}

PublicationPlotSpec BasicPlotPublicationSpec(const PlotModel &plot)
{
    PublicationPlotSpec publication;
    publication.kind = plot.kind;
    publication.title = plot.title;
    publication.xLabel = plot.xLabel;
    publication.yLabel = plot.yLabel;
    publication.theme = plot.rExportTheme;
    publication.showConfidenceIntervals = false;

    if (plot.kind == "histogram") {
        publication.kind = plot.histogramShowDensity
            ? "histogram_density" : "histogram";
        publication.yLabel = plot.yLabel.empty() ? "Count" : plot.yLabel;
        publication.showPoints = false;
        publication.showLines = false;
        publication.showAxisTickMarks = plot.histogramShowTickMarks;
        publication.showAxisTickLabels = plot.histogramShowTickLabels;
        PublicationPlotSeries bars;
        bars.id = "histogram";
        bars.label = plot.xLabel;
        for (const HistogramBin &bin : plot.histogramBins) {
            bars.x.push_back((bin.lower + bin.upper) / 2.0);
            bars.y.push_back(static_cast<double>(bin.rows.size()));
            bars.lower.push_back(bin.lower);
            bars.upper.push_back(bin.upper);
        }
        publication.series.push_back(std::move(bars));
        if (plot.histogramShowDensity) {
            PublicationPlotSeries densitySource;
            densitySource.id = "density_source";
            densitySource.label = "Density";
            for (const HistogramPoint &point : plot.histogramPoints) {
                if (!std::isfinite(point.x)) continue;
                densitySource.x.push_back(point.x);
                densitySource.y.push_back(0.0);
            }
            publication.series.push_back(std::move(densitySource));
        }
        return publication;
    }

    if (plot.kind == "barplot") {
        publication.showPoints = false;
        publication.showLines = false;
        publication.yLabel = plot.yLabel.empty()
            ? (plot.barplotMode == "conditional_percent" ? "% within X" :
               plot.barplotMode == "overall_percent" ? "% of total" : "Count")
            : plot.yLabel;
        for (const BarplotBin &bin : plot.barplotBins)
            publication.xCategoryOrder.push_back(bin.category);

        std::vector<std::string> levels;
        for (const BarplotBin &bin : plot.barplotBins) {
            for (const BarplotSegment &segment : bin.segments) {
                if (std::find(levels.begin(), levels.end(), segment.level) == levels.end())
                    levels.push_back(segment.level);
            }
        }
        if (levels.empty()) levels.push_back("All");
        for (std::size_t levelIndex = 0; levelIndex < levels.size(); ++levelIndex) {
            PublicationPlotSeries series;
            series.id = "series_" + std::to_string(levelIndex + 1);
            series.label = levels[levelIndex];
            for (std::size_t binIndex = 0; binIndex < plot.barplotBins.size(); ++binIndex) {
                const BarplotBin &bin = plot.barplotBins[binIndex];
                const auto found = std::find_if(
                    bin.segments.begin(), bin.segments.end(),
                    [&](const BarplotSegment &segment) {
                        return segment.level == levels[levelIndex];
                    });
                double value = 0.0;
                if (found != bin.segments.end()) {
                    value = plot.barplotMode == "conditional_percent"
                        ? found->conditionalPercent
                        : (plot.barplotMode == "overall_percent"
                            ? found->overallPercent
                            : static_cast<double>(found->count));
                }
                series.x.push_back(static_cast<double>(binIndex + 1));
                series.y.push_back(value);
            }
            publication.series.push_back(std::move(series));
        }
        return publication;
    }

    if (plot.kind == "boxplot") {
        publication.kind = plot.boxplotShowViolin
            ? (plot.boxplotShowBox ? "boxplot_violin" : "violin")
            : "boxplot";
        publication.showPoints = plot.boxplotShowPoints;
        // The retained publication series contain distribution values rather
        // than source-row identities; do not invent connections between
        // unrelated cases here. The verification recipe below uses the exact
        // exported observations when row connections are requested.
        publication.showLines = false;
        publication.xCategoryOrder = plot.boxplotCategories.empty()
            ? CategoriesForPlot(plot) : plot.boxplotCategories;
        if (publication.xCategoryOrder.empty())
            publication.xCategoryOrder = plot.boxplotDefinedCategories;
        publication.xCategoryOrder.erase(std::remove_if(
            publication.xCategoryOrder.begin(), publication.xCategoryOrder.end(),
            [&](const std::string &category) {
                return std::none_of(plot.boxplotPoints.begin(), plot.boxplotPoints.end(),
                    [&](const BoxplotPoint &point) {
                        return point.category == category && std::isfinite(point.y);
                    });
            }), publication.xCategoryOrder.end());
        if (plot.boxplotConnectRows) {
            std::map<int, PublicationPlotSeries> rows;
            for (const BoxplotPoint &point : plot.boxplotPoints) {
                if (!std::isfinite(point.y)) continue;
                const auto category = std::find(
                    publication.xCategoryOrder.begin(),
                    publication.xCategoryOrder.end(), point.category);
                if (category == publication.xCategoryOrder.end()) continue;
                PublicationPlotSeries &series = rows[point.row];
                series.id = "row_" + std::to_string(point.row);
                series.label = "Row " + std::to_string(point.row);
                series.x.push_back(static_cast<double>(
                    category - publication.xCategoryOrder.begin() + 1));
                series.y.push_back(point.y);
            }
            for (auto &[row, series] : rows)
                publication.series.push_back(std::move(series));
        } else {
            for (std::size_t categoryIndex = 0;
                 categoryIndex < publication.xCategoryOrder.size(); ++categoryIndex) {
                PublicationPlotSeries series;
                series.id = "category_" + std::to_string(categoryIndex + 1);
                series.label = publication.xCategoryOrder[categoryIndex];
                for (const BoxplotPoint &point : plot.boxplotPoints) {
                    if (point.category != series.label || !std::isfinite(point.y)) continue;
                    series.x.push_back(static_cast<double>(categoryIndex + 1));
                    series.y.push_back(point.y);
                }
                publication.series.push_back(std::move(series));
            }
        }
        return publication;
    }

    publication.showPoints = true;
    publication.showLines = plot.kind == "time_series";
    if (plot.kind == "time_series" && !plot.interactionPlotLines.empty()) {
        for (std::size_t index = 0; index < plot.interactionPlotLines.size(); ++index) {
            const InteractionPlotLine &line = plot.interactionPlotLines[index];
            PublicationPlotSeries series;
            series.id = "series_" + std::to_string(index + 1);
            series.label = line.label.empty() ? "All" : line.label;
            for (const DataPoint &point : line.points) {
                series.x.push_back(point.x);
                series.y.push_back(point.y);
            }
            publication.series.push_back(std::move(series));
        }
    } else if (!plot.colorByLegendRows.empty()) {
        for (const auto &[level, rows] : plot.colorByLegendRows) {
            PublicationPlotSeries series;
            series.id = "series_" + std::to_string(publication.series.size() + 1);
            series.label = level;
            const std::set<int> rowSet(rows.begin(), rows.end());
            for (const DataPoint &point : plot.points) {
                if (!rowSet.count(point.row)) continue;
                series.x.push_back(point.x);
                series.y.push_back(point.y);
            }
            publication.series.push_back(std::move(series));
        }
    } else {
        PublicationPlotSeries series;
        series.id = "all";
        series.label = "All";
        for (const DataPoint &point : plot.points) {
            series.x.push_back(point.x);
            series.y.push_back(point.y);
        }
        publication.series.push_back(std::move(series));
    }
    return publication;
}

OutputCodeReference BasicPlotCodeReference(const PlotModel &plot,
                                           const DataFrameModel &dataframe,
                                           const AnalysisScope &scope)
{
    OutputCodeReference output;
    output.outputId = plot.id;
    output.analysisId = plot.id;
    output.outputBlockId = "plot";
    output.title = plot.title;
    output.kind = "plot";
    output.provenance.analysisId = plot.id;
    output.provenance.title = plot.title;
    output.provenance.dataVersion = dataframe.provenance.currentVersion;
    output.provenance.dataVersion.displayName = dataframe.group;
    output.provenance.scope = CaptureImmutableAnalysisScope(
        scope, output.provenance.dataVersion, dataframe.stableRowIds);
    output.provenance.codeOrigin = RCodeOrigin::Unavailable;

    std::ostringstream code;
    code << "if (!requireNamespace(\"ggplot2\", quietly = TRUE))\n"
            "  stop(\"Install package 'ggplot2' to run this check.\")\n"
         << "linkeda_plot_theme <- "
         << Ggplot2ThemeRExpression(plot.rExportTheme) << "\n";
    const bool multipleImputation = dataframe.datasetType == "multiple_imputation" ||
        dataframe.imputationCount > 1;
    if (multipleImputation) {
        const int displayedImputation = dataframe.imputationDisplayMode == "original"
            ? 0 : std::max(1, dataframe.activeImputationVersion);
        code << "mi_long <- base::readRDS(verification_data_path)\n"
                "if (!all(c(\".imp\", \".id\") %in% names(mi_long)))\n"
                "  stop(\"Expected mice-compatible long data with .imp and .id.\")\n"
                "displayed_imputation <- " << displayedImputation << "L\n"
                "analysis_data <- mi_long[mi_long$.imp == displayed_imputation, , drop = FALSE]\n"
                "analysis_data$.imp <- NULL\n"
                "analysis_data$.id <- NULL\n";
        output.provenance.verificationWarnings.push_back(
            dataframe.imputationDisplayMode == "original"
                ? "This descriptive graph represents the original incomplete data; it is not a Rubin-pooled estimate."
                : "This descriptive graph represents the active completed dataset (imputation " +
                  std::to_string(displayedImputation) + " of " +
                  std::to_string(std::max(1, dataframe.imputationCount)) +
                  "); it is not a Rubin-pooled estimate.");
        if (plot.kind == "scatter" && dataframe.imputationDisplayMode == "all")
            output.provenance.verificationWarnings.push_back(
                "LinkEDA's all-imputations scatter display adds native uncertainty glyphs; this portable ggplot2 check reproduces the active-imputation point layer.");
    } else {
        code << "analysis_data <- base::readRDS(verification_data_path)\n";
    }
    std::vector<std::string> variables;
    if (plot.kind == "trellis_scatterplot") {
        PlotModel trellisPlot = plot;
        InitializeTrellisSpecificationFromLegacy(trellisPlot);
        const TrellisPlotSpecification &specification =
            trellisPlot.trellisSpecification;
        const auto rememberVariable = [&](const std::string &variable) {
            if (!variable.empty()) variables.push_back(variable);
        };
        rememberVariable(specification.xVariableId);
        rememberVariable(specification.yVariableId);
        rememberVariable(specification.groupingVariableId);
        rememberVariable(specification.splitVariableId);
        for (const std::string &variable : specification.boxplotGroupingVariableIds)
            rememberVariable(variable);

        std::vector<std::string> conditionColumns;
        for (std::size_t index = 0;
             index < specification.conditioningVariables.size(); ++index) {
            const TrellisConditioningVariable &condition =
                specification.conditioningVariables[index];
            rememberVariable(condition.variableId);
            const std::string derived =
                ".linkeda_condition_" + std::to_string(index + 1);
            conditionColumns.push_back(derived);
            code << "condition_values <- analysis_data[["
                 << ProvenanceRStringLiteral(condition.variableId) << "]]\n";
            if (condition.kind == TrellisConditioningVariableKind::Categorical) {
                code << "analysis_data[[" << ProvenanceRStringLiteral(derived)
                     << "]] <- factor(condition_values, ordered = "
                     << (condition.orderedCategories ? "TRUE" : "FALSE") << ")\n";
                continue;
            }
            const TrellisContinuousBinningSpecification binning =
                condition.binning.value_or(TrellisContinuousBinningSpecification{});
            code << "condition_numeric <- suppressWarnings(as.numeric(condition_values))\n";
            if (binning.method == TrellisContinuousBinningMethod::CustomBreaks &&
                binning.customBreaks.size() >= 2) {
                code << "condition_breaks <- "
                     << RNumericValues(binning.customBreaks) << "\n";
            } else if (binning.method == TrellisContinuousBinningMethod::EqualCount) {
                code << "condition_breaks <- unique(stats::quantile(\n"
                        "  condition_numeric, probs = seq(0, 1, length.out = "
                     << std::max<std::size_t>(2, binning.binCount + 1)
                     << "L), na.rm = TRUE, names = FALSE\n))\n";
            } else {
                code << "condition_limits <- range(condition_numeric, na.rm = TRUE, finite = TRUE)\n"
                        "if (length(condition_limits) != 2L || diff(condition_limits) <= 0)\n"
                        "  stop(\"The conditioning variable cannot be divided into equal-width intervals.\")\n"
                        "condition_breaks <- seq(condition_limits[1L], condition_limits[2L], length.out = "
                     << std::max<std::size_t>(2, binning.binCount + 1) << "L)\n";
            }
            code << "if (length(condition_breaks) < 2L)\n"
                    "  stop(\"The conditioning variable does not provide enough distinct cut points.\")\n"
                    "analysis_data[[" << ProvenanceRStringLiteral(derived)
                 << "]] <- cut(condition_numeric, breaks = condition_breaks, include.lowest = "
                 << (binning.includeLowest ? "TRUE" : "FALSE")
                 << ", ordered_result = TRUE)\n";
        }
        if (conditionColumns.empty()) {
            code << "analysis_data$.linkeda_panel <- factor(\"All\")\n";
        } else {
            code << "trellis_condition_columns <- "
                 << RCharacterValues(conditionColumns) << "\n"
                    "analysis_data$.linkeda_panel <- interaction(\n"
                    "  analysis_data[trellis_condition_columns], drop = TRUE, sep = \" · \"\n"
                    ")\n";
        }
        if (specification.panelOrder == "ascending")
            code << "analysis_data$.linkeda_panel <- factor(analysis_data$.linkeda_panel, levels = sort(levels(analysis_data$.linkeda_panel)))\n";
        else if (specification.panelOrder == "descending")
            code << "analysis_data$.linkeda_panel <- factor(analysis_data$.linkeda_panel, levels = rev(sort(levels(analysis_data$.linkeda_panel))))\n";

        if (specification.plotType == TrellisPlotType::DataTable) {
            code << "reference_tables <- split(analysis_data, analysis_data$.linkeda_panel, drop = TRUE)\n"
                    "reference_tables\n";
        } else {
            const std::string xVariable = specification.xVariableId;
            const std::string yVariable = specification.yVariableId;
            const std::string splitVariable = specification.plotType == TrellisPlotType::TimeSeries
                ? specification.groupingVariableId : specification.splitVariableId;
            if (specification.plotType == TrellisPlotType::Boxplot) {
                const std::vector<std::string> groups =
                    specification.boxplotGroupingVariableIds.empty()
                        ? std::vector<std::string>{xVariable}
                        : specification.boxplotGroupingVariableIds;
                code << "trellis_x_variables <- " << RCharacterValues(groups) << "\n"
                        "analysis_data$.linkeda_x <- interaction(\n"
                        "  analysis_data[trellis_x_variables], drop = TRUE, sep = \" × \"\n"
                        ")\n"
                        "reference_plot <- ggplot2::ggplot(\n"
                        "  analysis_data, ggplot2::aes(x = .linkeda_x, y = .data[["
                     << ProvenanceRStringLiteral(yVariable) << "]])\n"
                        ") + ggplot2::geom_boxplot(na.rm = TRUE)\n";
            } else if (specification.plotType == TrellisPlotType::Bar) {
                code << "reference_plot <- ggplot2::ggplot(analysis_data, ggplot2::aes(\n"
                        "  x = .data[[" << ProvenanceRStringLiteral(xVariable) << "]]";
                if (!splitVariable.empty())
                    code << ", fill = .data[[" << ProvenanceRStringLiteral(splitVariable) << "]]";
                if (specification.barMeasure == "percent")
                    code << ", y = ggplot2::after_stat(100 * count / sum(count))";
                code << "\n)) + ggplot2::geom_bar(na.rm = TRUE";
                if (specification.barMeasure == "conditional_percent")
                    code << ", position = \"fill\"";
                code << ")";
                if (specification.barMeasure == "conditional_percent")
                    code << " + ggplot2::scale_y_continuous("
                            "labels = function(x) 100 * x, name = \"Percent within X\")";
                code << "\n";
            } else if (specification.plotType == TrellisPlotType::Histogram) {
                code << "reference_plot <- ggplot2::ggplot(analysis_data, ggplot2::aes(\n"
                        "  x = .data[[" << ProvenanceRStringLiteral(xVariable) << "]]";
                if (!splitVariable.empty())
                    code << ", fill = .data[[" << ProvenanceRStringLiteral(splitVariable) << "]]";
                if (specification.histogramMeasure == "density")
                    code << ", y = ggplot2::after_stat(density)";
                else if (specification.histogramMeasure == "percent")
                    code << ", y = ggplot2::after_stat(100 * count / sum(count))";
                code << "\n)) + ggplot2::geom_histogram(bins = "
                     << std::max<std::size_t>(1, specification.histogramBinCount)
                     << "L, na.rm = TRUE";
                if (!splitVariable.empty()) code << ", position = \"identity\", alpha = 0.55";
                code << ")\n";
            } else {
                code << "reference_plot <- ggplot2::ggplot(analysis_data, ggplot2::aes(\n"
                        "  x = .data[[" << ProvenanceRStringLiteral(xVariable)
                     << "]], y = .data[[" << ProvenanceRStringLiteral(yVariable) << "]]";
                if (!splitVariable.empty()) {
                    code << ", colour = .data[[" << ProvenanceRStringLiteral(splitVariable)
                         << "]], group = .data[[" << ProvenanceRStringLiteral(splitVariable) << "]]";
                } else if (specification.plotType == TrellisPlotType::TimeSeries ||
                           specification.connectObservations) {
                    code << ", group = 1";
                }
                code << "\n))\n";
                if (specification.plotType == TrellisPlotType::TimeSeries ||
                    specification.connectObservations)
                    code << "reference_plot <- reference_plot + ggplot2::geom_line(na.rm = TRUE)\n";
                code << "reference_plot <- reference_plot + ggplot2::geom_point(na.rm = TRUE)\n";
                if (specification.plotType == TrellisPlotType::Scatter) {
                    const bool showLinearIntervals =
                        trellisPlot.scatterFitConfidenceIntervalsVisible;
                    const bool showSmoothIntervals =
                        trellisPlot.scatterSmoothConfidenceIntervalsVisible;
                    const double bandAlpha = splitVariable.empty() ? 0.18 : 0.10;
                    const bool hasLm = std::any_of(
                        trellisPlot.overlays.begin(), trellisPlot.overlays.end(),
                        [](const Overlay &overlay) {
                            return overlay.visible && overlay.type == "lm";
                        });
                    const bool hasLoess = std::any_of(
                        trellisPlot.smoothCurves.begin(), trellisPlot.smoothCurves.end(),
                        [](const SmoothCurveData &curve) {
                            return curve.fitMethod == "loess";
                        });
                    if (hasLm)
                        code << "reference_plot <- reference_plot + ggplot2::geom_smooth(\n"
                                "  method = \"lm\", formula = y ~ x, se = "
                             << (showLinearIntervals ? "TRUE" : "FALSE")
                             << ", level = " << std::setprecision(17)
                             << trellisPlot.scatterFitConfidenceLevel
                             << ", alpha = " << bandAlpha << "\n)\n";
                    if (hasLoess)
                        code << "reference_plot <- reference_plot + ggplot2::geom_smooth(\n"
                                "  method = \"loess\", formula = y ~ x, se = "
                             << (showSmoothIntervals ? "TRUE" : "FALSE")
                             << ", level = " << std::setprecision(17)
                             << trellisPlot.scatterFitConfidenceLevel
                             << ", alpha = " << bandAlpha << ", span = "
                             << trellisPlot.smoothSpan << "\n)\n";
                }
            }
            const std::string facetScales = specification.scaleMode == "free_xy"
                ? "free" : (specification.scaleMode == "free_x" ? "free_x" :
                    (specification.scaleMode == "free_y" ? "free_y" : "fixed"));
            code << "reference_plot <- reference_plot + ggplot2::facet_wrap(\n"
                    "  ggplot2::vars(.linkeda_panel), scales = "
                 << ProvenanceRStringLiteral(facetScales);
            if (specification.layoutMode == "one_row") code << ", nrow = 1L";
            else if (specification.layoutMode == "one_column") code << ", ncol = 1L";
            else if (specification.layoutMode == "two_columns") code << ", ncol = 2L";
            else if (specification.layoutMode == "three_columns") code << ", ncol = 3L";
            code << "\n) + ggplot2::labs(x = "
                 << ProvenanceRStringLiteral(trellisPlot.xLabel) << ", y = "
                 << ProvenanceRStringLiteral(trellisPlot.yLabel) << ", title = "
                 << ProvenanceRStringLiteral(trellisPlot.title)
                 << ") + linkeda_plot_theme\n"
                    "print(reference_plot)\n";
        }
        output.provenance.verificationRCode["plot"] = code.str();
        std::sort(variables.begin(), variables.end());
        variables.erase(std::remove_if(variables.begin(), variables.end(),
            [](const std::string &value) { return value.empty(); }), variables.end());
        variables.erase(std::unique(variables.begin(), variables.end()), variables.end());
        output.provenance.verificationVariables = std::move(variables);
        if (specification.conditioningVariables.size() > 1)
            output.provenance.verificationWarnings.push_back(
                "The ggplot2 check uses the same conditioning variables and levels; facet_wrap may arrange nested row/column panels differently from LinkEDA.");
        return output;
    } else if (plot.kind == "scatter_matrix") {
        variables = ScatterMatrixVariablesForModel(plot);
        code << "matrix_variables <- " << RCharacterValues(variables) << "\n"
                "matrix_pairs <- expand.grid(\n"
                "  x_variable = matrix_variables, y_variable = matrix_variables,\n"
                "  stringsAsFactors = FALSE\n"
                ")\n"
                "matrix_pairs <- matrix_pairs[matrix_pairs$x_variable != matrix_pairs$y_variable, , drop = FALSE]\n"
                "matrix_data <- do.call(rbind, lapply(seq_len(nrow(matrix_pairs)), function(i) {\n"
                "  x_name <- matrix_pairs$x_variable[i]\n"
                "  y_name <- matrix_pairs$y_variable[i]\n"
                "  data.frame(x = analysis_data[[x_name]], y = analysis_data[[y_name]],\n"
                "             x_variable = x_name, y_variable = y_name)\n"
                "}))\n"
                "matrix_data$x_variable <- factor(matrix_data$x_variable, levels = matrix_variables)\n"
                "matrix_data$y_variable <- factor(matrix_data$y_variable, levels = matrix_variables)\n"
                "reference_plot <- ggplot2::ggplot(matrix_data, ggplot2::aes(x = x, y = y)) +\n"
                "  ggplot2::geom_point(na.rm = TRUE) +\n"
                "  ggplot2::facet_grid(ggplot2::vars(y_variable), ggplot2::vars(x_variable), drop = FALSE)\n";
        const bool overallLm = std::any_of(
            plot.overlays.begin(), plot.overlays.end(), [](const Overlay &overlay) {
                return overlay.visible && overlay.type == "lm" && overlay.source == "all";
            });
        if (overallLm)
            code << "reference_plot <- reference_plot + ggplot2::geom_smooth(\n"
                    "  method = \"lm\", formula = y ~ x, se = "
                 << (plot.scatterFitConfidenceIntervalsVisible ? "TRUE" : "FALSE")
                 << ", level = " << std::setprecision(17)
                 << plot.scatterFitConfidenceLevel << "\n)\n";
        const bool linkedOnlyOverlay = std::any_of(
            plot.overlays.begin(), plot.overlays.end(), [](const Overlay &overlay) {
                return overlay.visible && overlay.type == "lm" && overlay.source != "all";
            });
        if (linkedOnlyOverlay)
            output.provenance.verificationWarnings.push_back(
                "Selection- and row-colour-specific matrix fits require live linked state and are not included in the prepared data export.");
        code << "reference_plot <- reference_plot + ggplot2::labs(title = "
             << ProvenanceRStringLiteral(plot.title)
             << ") + linkeda_plot_theme\n";
    } else if (plot.kind == "histogram") {
        variables = {plot.xLabel};
        std::vector<double> breaks;
        if (!plot.histogramBins.empty()) {
            breaks.push_back(plot.histogramBins.front().lower);
            for (const HistogramBin &bin : plot.histogramBins) breaks.push_back(bin.upper);
        }
        code << "histogram_breaks <- " << RNumericValues(breaks) << "\n"
             << "x <- analysis_data[[" << ProvenanceRStringLiteral(plot.xLabel) << "]]\n"
                "x <- x[!is.na(x)]\n"
                "bin <- cut(x, breaks = histogram_breaks, include.lowest = TRUE, right = FALSE)\n"
                "bin[x == max(histogram_breaks)] <- tail(levels(bin), 1L)\n"
                "reference_counts <- as.data.frame(table(bin, useNA = \"no\"), stringsAsFactors = FALSE)\n"
                "names(reference_counts) <- c(\"bin\", \"count\")\n"
                "reference_plot <- ggplot2::ggplot(reference_counts, ggplot2::aes(x = bin, y = count)) +\n"
                "  ggplot2::geom_col() + ggplot2::labs(x = "
             << ProvenanceRStringLiteral(plot.xLabel) << ", y = \"Count\", title = "
             << ProvenanceRStringLiteral(plot.title) << ") + linkeda_plot_theme\n";
        if (plot.histogramShowDensity && plot.histogramDensityMode == "all") {
            code << "density_source <- data.frame(x = x)\n"
                    "bin_width <- stats::median(diff(histogram_breaks))\n"
                    "reference_plot <- reference_plot + ggplot2::geom_density(\n"
                    "  data = density_source,\n"
                    "  ggplot2::aes(x = x, y = ggplot2::after_stat(count * bin_width)),\n"
                    "  inherit.aes = FALSE, linewidth = 0.8, fill = NA, bw = ";
            if (plot.histogramDensityBw > 0.0)
                code << std::setprecision(17) << plot.histogramDensityBw;
            else
                code << "\"nrd0\"";
            code << ", adjust = " << std::setprecision(17)
                 << plot.histogramDensityAdjust << "\n)\n";
        } else if (plot.histogramShowDensity) {
            output.provenance.verificationWarnings.push_back(
                "The ggplot2 recipe reproduces the histogram itself; selected-case and row-colour density layers require live linked-selection state and are not included in the prepared data export.");
        }
        if (plot.histogramShowRug) {
            code << "reference_plot <- reference_plot + ggplot2::geom_rug(\n"
                    "  data = data.frame(x = x), ggplot2::aes(x = x),\n"
                    "  inherit.aes = FALSE, sides = \"b\", alpha = 0.55\n"
                    ")\n";
        }
        if (!plot.histogramShowTickMarks || !plot.histogramShowTickLabels) {
            code << "reference_plot <- reference_plot + ggplot2::theme(\n";
            if (!plot.histogramShowTickMarks)
                code << "  axis.ticks = ggplot2::element_blank()";
            if (!plot.histogramShowTickMarks && !plot.histogramShowTickLabels)
                code << ",\n";
            if (!plot.histogramShowTickLabels)
                code << "  axis.text = ggplot2::element_blank()";
            code << "\n)\n";
        }
    } else if (plot.kind == "barplot") {
        variables = plot.barplotXVariables;
        if (variables.empty() && !plot.xLabel.empty()) variables.push_back(plot.xLabel);
        if (!plot.barplotSplitVariable.empty()) variables.push_back(plot.barplotSplitVariable);
        code << "x_variables <- " << RCharacterValues(
                    plot.barplotXVariables.empty()
                        ? std::vector<std::string>{plot.xLabel}
                        : plot.barplotXVariables) << "\n"
                "analysis_data$.linkeda_x <- interaction(analysis_data[x_variables], drop = TRUE, sep = \" × \" )\n";
        if (!plot.barplotSplitVariable.empty()) {
            code << "analysis_data$.linkeda_split <- analysis_data[["
                 << ProvenanceRStringLiteral(plot.barplotSplitVariable) << "]]\n"
                    "reference_counts <- as.data.frame(table(analysis_data$.linkeda_x, analysis_data$.linkeda_split, useNA = \"ifany\"))\n"
                    "names(reference_counts) <- c(\"x\", \"series\", \"count\")\n";
        } else {
            code << "reference_counts <- as.data.frame(table(analysis_data$.linkeda_x, useNA = \"ifany\"))\n"
                    "names(reference_counts) <- c(\"x\", \"count\")\n";
        }
        if (plot.barplotMode == "conditional_percent") {
            code << "reference_counts$value <- 100 * reference_counts$count / ave(\n"
                    "  reference_counts$count, reference_counts$x, FUN = sum\n"
                    ")\n";
        } else if (plot.barplotMode == "overall_percent") {
            code << "reference_counts$value <- 100 * reference_counts$count / sum(reference_counts$count)\n";
        } else {
            code << "reference_counts$value <- reference_counts$count\n";
        }
        if (!plot.barplotSplitVariable.empty()) {
            code << "reference_plot <- ggplot2::ggplot(reference_counts, ggplot2::aes(x = x, y = value, fill = series)) +\n"
                    "  ggplot2::geom_col(position = \"stack\") +\n";
        } else {
            code << "reference_plot <- ggplot2::ggplot(reference_counts, ggplot2::aes(x = x, y = value)) +\n"
                    "  ggplot2::geom_col() +\n";
        }
        code << "  ggplot2::labs(x = " << ProvenanceRStringLiteral(plot.xLabel)
             << ", y = " << ProvenanceRStringLiteral(
                    plot.barplotMode == "conditional_percent" ? "% within X" :
                    (plot.barplotMode == "overall_percent" ? "% of total" : "Count"))
             << ", title = " << ProvenanceRStringLiteral(plot.title)
             << ") + linkeda_plot_theme\n";
        if (plot.barplotWidthMode != "equal")
            output.provenance.verificationWarnings.push_back(
                "The ggplot2 recipe reproduces bar heights and composition; LinkEDA's proportional-width layout remains a native presentation feature.");
    } else if (plot.kind == "boxplot") {
        const bool parallel = plot.boxplotGroupingVariables.empty() &&
            !plot.boxplotVariables.empty();
        if (parallel) {
            variables = plot.boxplotVariables;
            code << "value_variables <- " << RCharacterValues(plot.boxplotVariables) << "\n"
                    "plot_source <- analysis_data[value_variables]\n";
            if (plot.boxplotStandardizeVariables)
                code << "plot_source[] <- lapply(plot_source, function(x) as.numeric(scale(x)))\n";
            code << "reference_data <- utils::stack(plot_source)\n"
                    "names(reference_data) <- c(\"value\", \"group\")\n"
                    "reference_data$.row <- rep(seq_len(nrow(plot_source)), times = length(value_variables))\n";
        } else {
            variables = plot.boxplotGroupingVariables;
            if (variables.empty() && !plot.xLabel.empty()) variables.push_back(plot.xLabel);
            variables.push_back(plot.yLabel);
            code << "group_variables <- " << RCharacterValues(
                        plot.boxplotGroupingVariables.empty()
                            ? std::vector<std::string>{plot.xLabel}
                            : plot.boxplotGroupingVariables) << "\n"
                    "reference_data <- data.frame(\n"
                    "  value = analysis_data[[" << ProvenanceRStringLiteral(plot.yLabel) << "]],\n"
                    "  group = interaction(analysis_data[group_variables], drop = TRUE, sep = \" × \"),\n"
                    "  .row = seq_len(nrow(analysis_data))\n"
                    ")\n";
        }
        code << "reference_box_statistics <- lapply(\n"
                "  split(reference_data$value, reference_data$group),\n"
                "  grDevices::boxplot.stats, coef = 1.5\n"
                ")\n"
                "reference_plot <- ggplot2::ggplot(\n"
                "  reference_data, ggplot2::aes(x = group, y = value)\n"
                ")\n";
        if (plot.boxplotShowViolin)
            code << "reference_plot <- reference_plot + ggplot2::geom_violin(\n"
                    "  trim = FALSE, fill = \"grey85\", colour = \"grey35\", na.rm = TRUE\n"
                    ")\n";
        if (plot.boxplotShowBox)
            code << "reference_plot <- reference_plot + ggplot2::geom_boxplot(\n"
                    "  coef = 1.5, width = 0.35, outlier.shape = NA, fill = \"white\", na.rm = TRUE\n"
                    ")\n";
        if (plot.boxplotShowPoints)
            code << "reference_plot <- reference_plot + ggplot2::geom_jitter(\n"
                    "  width = 0.08, height = 0, alpha = 0.65, size = 1.7, na.rm = TRUE\n"
                    ")\n";
        if (parallel && plot.boxplotConnectRows)
            code << "reference_plot <- reference_plot + ggplot2::geom_line(\n"
                    "  ggplot2::aes(group = .row), alpha = 0.35, linewidth = 0.5, na.rm = TRUE\n"
                    ")\n";
        code << "reference_plot <- reference_plot + ggplot2::labs(x = "
             << ProvenanceRStringLiteral(plot.xLabel) << ", y = "
             << ProvenanceRStringLiteral(plot.yLabel) << ", title = "
             << ProvenanceRStringLiteral(plot.title) << ") + linkeda_plot_theme\n";
    } else {
        variables = {plot.xLabel, plot.yLabel};
        const bool timeSeries = plot.kind == "time_series" &&
            !plot.timeSeriesGroupVariable.empty();
        if (timeSeries) variables.push_back(plot.timeSeriesGroupVariable);
        const bool colorBy = !timeSeries && !plot.colorByVariable.empty();
        if (colorBy) variables.push_back(plot.colorByVariable);
        code << "reference_plot <- ggplot2::ggplot(analysis_data, ggplot2::aes(x = .data[["
             << ProvenanceRStringLiteral(plot.xLabel) << "]], y = .data[["
             << ProvenanceRStringLiteral(plot.yLabel) << "]]";
        if (timeSeries) {
            code << ", colour = .data[[" << ProvenanceRStringLiteral(plot.timeSeriesGroupVariable)
                 << "]], group = .data[[" << ProvenanceRStringLiteral(plot.timeSeriesGroupVariable) << "]]";
        } else if (colorBy) {
            code << ", colour = .data[[" << ProvenanceRStringLiteral(plot.colorByVariable)
                 << "]]";
        }
        code << ")) + ggplot2::geom_point(na.rm = TRUE)";
        if (timeSeries) code << " + ggplot2::geom_line(na.rm = TRUE)";
        const bool overallLm = std::any_of(
            plot.overlays.begin(), plot.overlays.end(), [](const Overlay &overlay) {
                return overlay.visible && overlay.type == "lm" && overlay.source == "all";
            });
        if (overallLm)
            code << " + ggplot2::geom_smooth(ggplot2::aes(group = 1), method = \"lm\", formula = y ~ x, se = "
                 << (plot.scatterFitConfidenceIntervalsVisible ? "TRUE" : "FALSE")
                 << ", level = " << std::setprecision(17) << plot.scatterFitConfidenceLevel
                 << ", alpha = 0.18, colour = \"black\")";
        const bool colorLm = colorBy && std::any_of(
            plot.overlays.begin(), plot.overlays.end(), [](const Overlay &overlay) {
                return overlay.visible && overlay.type == "lm" && overlay.source == "color";
            });
        if (colorLm)
            code << " + ggplot2::geom_smooth(method = \"lm\", formula = y ~ x, se = "
                 << (plot.scatterFitConfidenceIntervalsVisible ? "TRUE" : "FALSE")
                 << ", level = " << std::setprecision(17) << plot.scatterFitConfidenceLevel
                 << ", alpha = 0.10)";
        const bool overallSmooth = std::any_of(
            plot.smoothCurves.begin(), plot.smoothCurves.end(), [](const SmoothCurveData &curve) {
                return curve.ok && curve.fitMethod == "loess" &&
                    curve.scope == SmoothCurveScope::Overall;
            });
        if (overallSmooth)
            code << " + ggplot2::geom_smooth(ggplot2::aes(group = 1), method = \"loess\", formula = y ~ x, se = "
                 << (plot.scatterSmoothConfidenceIntervalsVisible ? "TRUE" : "FALSE")
                 << ", level = " << std::setprecision(17) << plot.scatterFitConfidenceLevel
                 << ", alpha = 0.18, span = " << plot.smoothSpan
                 << ", colour = \"#0072B2\")";
        const bool colorSmooth = colorBy && std::any_of(
            plot.smoothCurves.begin(), plot.smoothCurves.end(), [](const SmoothCurveData &curve) {
                return curve.ok && curve.fitMethod == "loess" &&
                    curve.scope == SmoothCurveScope::ColorGroup;
            });
        if (colorSmooth)
            code << " + ggplot2::geom_smooth(method = \"loess\", formula = y ~ x, se = "
                 << (plot.scatterSmoothConfidenceIntervalsVisible ? "TRUE" : "FALSE")
                 << ", level = " << std::setprecision(17) << plot.scatterFitConfidenceLevel
                 << ", alpha = 0.10, span = " << plot.smoothSpan << ")";
        code << " +\n  ggplot2::labs(x = " << ProvenanceRStringLiteral(plot.xLabel)
             << ", y = " << ProvenanceRStringLiteral(plot.yLabel)
             << ", title = " << ProvenanceRStringLiteral(plot.title)
             << ") + linkeda_plot_theme\n";
        const bool linkedOnlyOverlay = std::any_of(
            plot.overlays.begin(), plot.overlays.end(), [](const Overlay &overlay) {
                return overlay.visible && overlay.type == "lm" && overlay.source != "all";
            }) || std::any_of(
                plot.smoothCurves.begin(), plot.smoothCurves.end(), [](const SmoothCurveData &curve) {
                    return curve.ok && curve.scope != SmoothCurveScope::Overall;
                });
        if (linkedOnlyOverlay)
            output.provenance.verificationWarnings.push_back(
                "Selection- and row-colour-specific fitted layers require live linked state and are not included in the prepared data export.");
    }
    code << "print(reference_plot)\n";
    output.provenance.verificationRCode["plot"] = code.str();
    std::sort(variables.begin(), variables.end());
    variables.erase(std::remove_if(variables.begin(), variables.end(),
        [](const std::string &value) { return value.empty(); }), variables.end());
    variables.erase(std::unique(variables.begin(), variables.end()), variables.end());
    output.provenance.verificationVariables = std::move(variables);
    if (plot.kind == "scatter_matrix") return output;
    output.publication.plot = BasicPlotPublicationSpec(plot);
    output.publication.availableBackends = {PublicationBackend::Ggplot2};
    return output;
}

void RegisterBasicPlotCodeReference(ApplicationState &state, PlotModel &plot)
{
    if (plot.isGLMDiagnostic) {
        // A display adjustment must retain the fitted-model verification
        // recipe; diagnostic axis labels are not source dataset columns.
        const GeneralizedGLMState *source = nullptr;
        const auto found = state.generalizedGLMs().find(plot.glmModelId);
        if (found != state.generalizedGLMs().end()) source = &found->second;
        if (!source) for (const auto &comparison : state.generalizedComparisons())
            for (const auto &model : comparison.second.models)
                if (model.id == plot.glmModelId) source = &model.fit;
        if (source && plot.glmDiagnosticKind != "partial_regression")
            plot.codeReference = GeneralizedDiagnosticPlotCodeReference(plot, *source);
        if (!plot.codeReference.outputId.empty()) state.registerOutputCodeReference(plot.codeReference);
        return;
    }

    const DataFrameModel *dataframe = state.datasets().find(plot.group);
    if (!dataframe) return;
    if (!plot.dataScopeCaptured) {
        plot.dataScope = state.activeAnalysisScope(plot.group);
        plot.dataScopeCaptured = true;
    }
    plot.codeReference = BasicPlotCodeReference(plot, *dataframe, plot.dataScope);
    state.registerOutputCodeReference(plot.codeReference);
}

void RefreshRegisteredBasicPlotCodeReference(ApplicationState &state,
                                              const std::string &plotId)
{
    const auto found = state.plots().find(plotId);
    if (found == state.plots().end() || !found->second) return;
    RegisterBasicPlotCodeReference(state, *found->second);
}

std::optional<DataFrameModel> VerificationExportData(
    const ApplicationState &state,
    const AnalysisProvenance &provenance)
{
    if (!provenance.preparedDataPath.empty()) {
        DataFrameModel prepared;
        prepared.group = provenance.dataVersion.datasetId;
        prepared.verificationPreparedRds = provenance.preparedDataPath;
        return prepared;
    }
    const DataFrameModel *version = state.datasetVersion(provenance.dataVersion);
    if (!version) return std::nullopt;
    std::vector<int> rows;
    if (provenance.scope.kind != "all") {
        const std::vector<std::string> &ids = provenance.scope.requestedStableRowIds;
        for (const std::string &id : ids) {
            const auto found = std::find(version->stableRowIds.begin(),
                                         version->stableRowIds.end(), id);
            if (found != version->stableRowIds.end())
                rows.push_back(static_cast<int>(std::distance(
                    version->stableRowIds.begin(), found)) + 1);
        }
    } else {
        rows.reserve(static_cast<std::size_t>(std::max(0, version->rows)));
        for (int row = 1; row <= version->rows; ++row) rows.push_back(row);
    }
    DataFrameModel prepared = SubsetDataFrame(*version, rows, version->group);
    const std::set<std::string> required(
        provenance.verificationVariables.begin(),
        provenance.verificationVariables.end());
    if (!required.empty()) {
        prepared.columns.erase(std::remove_if(prepared.columns.begin(), prepared.columns.end(),
            [&](const DataColumn &column) {
                return !required.count(column.name);
            }), prepared.columns.end());
    }
    return prepared;
}

void CompleteAnalysisProvenance(AnalysisProvenance &provenance,
                                const DataFrameModel &dataframe,
                                const AnalysisScope &scope,
                                const std::vector<int> &rowsUsed,
                                const std::vector<int> &rowsExcluded,
                                const std::string &analysisId,
                                const std::string &title)
{
    if (provenance.executedRCode.empty()) return;
    if (provenance.analysisId.empty()) provenance.analysisId = analysisId;
    if (provenance.title.empty()) provenance.title = title;
    if (provenance.dataVersion.datasetId.empty())
        provenance.dataVersion = dataframe.provenance.currentVersion;
    provenance.dataVersion.displayName = dataframe.group;
    if (provenance.dataVersion.storageKey.empty()) {
        provenance.dataVersion.storageKey = provenance.dataVersion.datasetId + "@" +
            std::to_string(provenance.dataVersion.version);
    }
    if (!provenance.scopeRecordedInR)
        provenance.scope = CaptureImmutableAnalysisScope(
            scope, provenance.dataVersion, dataframe.stableRowIds,
            rowsUsed, rowsExcluded);
}

} // namespace

OutputCodeReference LinearModelCodeReference(const GroupModelState &state,
                                             const GLMFitSummary &fit)
{
    OutputCodeReference output;
    output.outputId = "glm:" + (state.modelId.empty() ? state.group : state.modelId);
    output.analysisId = state.provenance.analysisId;
    output.outputBlockId = "model";
    output.title = state.title.empty() ? "Linear Model" : state.title;
    output.kind = "table";
    output.provenance = state.provenance;
    PublicationTableSpec table;
    table.title = output.title;
    table.subtitle = "Response: " + state.response;
    if (!state.dataScope.sourceDescription.empty())
        table.subtitle += " — " + state.dataScope.sourceDescription;
    table.stubColumns = {0};
    table.columns = {
        {"variable", "Variable", "", -1, false},
        {"b", "b", "", 4, false},
        {"beta", "β", "", 4, false},
        {"SE", "SE", "", 4, false},
        {"t", "t", "", 3, false},
        {"p", "p", "", 3, true},
        {"partial_r", "Partial r", "", 3, false},
        {"delta_R2", "ΔR²", "", 3, false}
    };
    for (const GLMCoefficientRow &row : fit.coefficients) {
        table.rows.push_back({
            PublicationText(PublicationModelRowLabel(row)),
            PublicationNumber(row.estimate),
            PublicationNumber(row.standardizedBeta),
            PublicationNumber(row.stdError),
            PublicationNumber(row.tValue),
            PublicationNumber(row.pValue),
            PublicationNumber(row.partialR),
            PublicationNumber(row.deltaR2)
        });
    }
    if (PublicationModelHasHierarchy(fit.coefficients))
        AddModelHierarchyFootnote(table);
    std::ostringstream summary;
    summary << "Model fit: N = " << fit.n
        << "; R² = " << FormatDouble(fit.r2, 3)
        << "; adjusted R² = " << FormatDouble(fit.adjR2, 3)
        << "; residual SD = " << FormatDouble(fit.sigma, 3)
        << "; residual df = " << fit.dfResidual
        << "; " << (state.multipleImputation ? "D1/Wald" : "F")
        << " = " << FormatDouble(fit.globalF, 3)
        << "; p = " << FormatPValue(fit.globalP) << ".";
    table.footnotes.insert(table.footnotes.begin(), summary.str());
    table.footnotes.push_back("b = unstandardized coefficient; β = standardized coefficient; "
        "SE = standard error; partial r = partial correlation; ΔR² = change in R².");
    if (!state.note.empty()) table.footnotes.push_back(state.note);
    output.publication.table = std::move(table);
    output.publication.availableBackends = {
        PublicationBackend::Tinytable, PublicationBackend::Latex,
        PublicationBackend::LatexPdf
    };
    return output;
}

std::vector<PublicationTableSpec> LinearModelReportTables(const GroupModelState &state,
                                                        const GLMFitSummary &fit)
{
    PublicationTableSpec global;
    global.title = (state.title.empty() ? "Linear Model" : state.title) + " — Global fit";
    global.subtitle = "Response: " + state.response;
    if (!state.dataScope.sourceDescription.empty()) global.subtitle += " — " + state.dataScope.sourceDescription;
    global.columns = {{"r2","R²","",-1,false},{"adj_r2","Adjusted R²","",-1,false},
                      {"s","s","",3,false},{"df","df","",0,false},{"n","N","",0,false}};
    global.rows = {{PublicationText(FormatPercent(fit.r2,1)),PublicationText(FormatPercent(fit.adjR2,1)),
                    PublicationNumber(fit.sigma),PublicationNumber(fit.dfResidual),PublicationNumber(fit.n)}};
    PublicationTableSpec sources;
    sources.title = "Sources"; sources.stubColumns = {0};
    sources.columns = {{"source","Source","",-1,false},{"SS","Sum of Squares","",4,false},
        {"df","df","",0,false},{"MS","Mean Square","",4,false},
        {"stat",state.multipleImputation ? "D1/Wald" : "F","",3,false},{"p","p","",3,true}};
    sources.rows = {
        {PublicationText("Regression"),PublicationNumber(fit.ssRegression),PublicationNumber(fit.dfModel),
            PublicationNumber(fit.msRegression),PublicationNumber(fit.globalF),PublicationNumber(fit.globalP)},
        {PublicationText("Residual"),PublicationNumber(fit.ssResidual),PublicationNumber(fit.dfResidual),
            PublicationNumber(fit.msResidual),PublicationNumber(NAN),PublicationNumber(NAN)}};
    auto terms = *LinearModelCodeReference(state,fit).publication.table;
    terms.title = "Terms"; terms.subtitle.clear();
    // The usual report displays fit information above the coefficient table.
    terms.footnotes.erase(terms.footnotes.begin());
    terms.columns.insert(terms.columns.begin()+1,{"type","Type","",-1,false});
    for (std::size_t i=0;i<terms.rows.size();++i)
        terms.rows[i].insert(terms.rows[i].begin()+1,PublicationText(PublicationModelTypeLabel(fit.coefficients[i])));
    return {global,sources,terms};
}
namespace {

OutputCodeReference GeneralizedModelCodeReference(const GeneralizedGLMState &state)
{
    OutputCodeReference output;
    output.outputId = state.id;
    output.analysisId = state.provenance.analysisId;
    output.outputBlockId = "model";
    output.title = state.title.empty() ? (state.binaryRegression ? "Binary Model" :
        state.countRegression ? "Count Model" : "Generalized Linear Model") : state.title;
    output.kind = "table";
    output.provenance = state.provenance;
    PublicationTableSpec table;
    table.title = output.title;
    table.subtitle = state.dataScope.sourceDescription;
    table.stubColumns = {0};
    table.columns = {
        {"variable", "Variable", "", -1, false},
        {"type", "Type", "", -1, false},
        {"b", "b", "", 4, false},
        {"SE", "SE", "", 4, false},
        {"statistic", state.statisticName, "", 3, false},
        {"p", "p", "", 3, true}
    };
    table.columns.push_back({"confidence_interval", "95% CI", "", -1, false});
    if (GeneralizedModelHasExponentiatedEffect(state)) {
        const std::string label = GeneralizedExponentiatedEffectLabel(state);
        table.columns.push_back({"exponentiated_effect", label, "", 3, false});
        table.columns.push_back({"exponentiated_effect_ci", label + " 95% CI", "", -1, false});
    }
    for (const GeneralizedGLMRow &row : state.rows) {
        const bool parent = row.rowType == "factor_parent" || row.rowType == "term_parent";
        const GlobalTermTestRow *termTest =
            GeneralizedGlobalTermTestForPresentationRow(state, row);
        std::vector<PublicationValue> publicationRow = {
            PublicationText(PublicationModelRowLabel(row)),
            PublicationText(PublicationModelTypeLabel(row)),
            PublicationNumber(parent ? NAN : row.estimate),
            PublicationNumber(parent ? NAN : row.stdError),
            PublicationNumber(parent && termTest ? termTest->statistic : row.statistic),
            PublicationNumber(parent && termTest ? termTest->pValue : row.pValue)
        };
        publicationRow.push_back(parent
            ? PublicationText("")
            : PublicationInterval(row.ciLower, row.ciUpper));
        if (GeneralizedModelHasExponentiatedEffect(state)) {
            publicationRow.push_back(PublicationNumber(parent
                ? NAN : row.exponentiatedEstimate));
            publicationRow.push_back(parent
                ? PublicationText("")
                : PublicationInterval(row.exponentiatedLower, row.exponentiatedUpper));
        }
        table.rows.push_back(std::move(publicationRow));
    }
    if (PublicationModelHasHierarchy(state.rows))
        AddModelHierarchyFootnote(table);
    const std::string methodNote = GeneralizedGlobalTermTestMethodNote(state);
    if (!methodNote.empty()) table.footnotes.push_back(methodNote);
    const std::string hierarchyNote = GeneralizedGlobalTermTestHierarchyNote(state);
    if (!hierarchyNote.empty()) table.footnotes.push_back(hierarchyNote);
    if (state.multipleImputation)
        table.footnotes.push_back("Multiple-imputation coefficient estimates and standard errors are combined using Rubin's rules.");
    if (GeneralizedModelHasExponentiatedEffect(state)) {
        const std::string effect = GeneralizedExponentiatedEffectLabel(state);
        table.footnotes.push_back(effect == "Odds ratio"
            ? "Odds are event probability divided by non-event probability. Odds ratios are exp(b); values above 1 indicate higher odds relative to the stated reference or per one-unit increase."
            : effect + " values are exp(b); values above 1 indicate an increase relative to the stated reference or per one-unit increase.");
    }
    output.publication.table = std::move(table);
    output.publication.availableBackends = {
        PublicationBackend::Tinytable, PublicationBackend::Latex,
        PublicationBackend::LatexPdf
    };
    return output;
}

OutputCodeReference RegressionComparisonCodeReference(
    const RegressionComparisonState &state)
{
    OutputCodeReference output;
    output.outputId = state.id;
    output.analysisId = state.provenance.analysisId;
    output.outputBlockId = "comparison";
    output.title = state.title.empty() ? "Compare Linear Models" : state.title;
    output.kind = "table";
    output.provenance = state.provenance;

    PublicationTableSpec table;
    table.title = output.title;
    table.subtitle = state.dataScope.sourceDescription;
    table.stubColumns = {0};
    table.columns.push_back({"term", "Term", "", -1, false});
    for (std::size_t modelIndex = 0; modelIndex < state.models.size(); ++modelIndex) {
        const std::string key = "model_" + std::to_string(modelIndex + 1);
        const std::string label = state.models[modelIndex].label.empty()
            ? "Model " + std::to_string(modelIndex + 1)
            : state.models[modelIndex].label;
        table.columns.push_back({key, label, "", 4, false});
    }
    for (const std::string &term : state.termRows) {
        std::vector<PublicationValue> row;
        row.push_back(PublicationText(term));
        for (const RegressionComparisonModel &model : state.models) {
            double estimate = NAN;
            const auto found = std::find_if(
                model.fit.coefficients.begin(), model.fit.coefficients.end(),
                [&](const GLMCoefficientRow &coefficient) {
                    return coefficient.term == term || coefficient.displayLabel == term;
                });
            if (found != model.fit.coefficients.end()) estimate = found->estimate;
            row.push_back(PublicationNumber(estimate));
        }
        table.rows.push_back(std::move(row));
    }
    const std::vector<std::pair<std::string, std::function<double(const RegressionComparisonModel &)>>> metrics = {
        {"N", [](const RegressionComparisonModel &model) { return static_cast<double>(model.fit.n); }},
        {"R2", [](const RegressionComparisonModel &model) { return model.fit.r2; }},
        {"Adjusted R2", [](const RegressionComparisonModel &model) { return model.fit.adjR2; }},
        {"Residual s", [](const RegressionComparisonModel &model) { return model.fit.sigma; }},
        {"df residual", [](const RegressionComparisonModel &model) { return static_cast<double>(model.fit.dfResidual); }},
        {"Comparison statistic", [](const RegressionComparisonModel &model) { return model.comparisonStatistic; }},
        {"p vs previous", [](const RegressionComparisonModel &model) { return model.comparisonP; }}
    };
    for (const auto &metric : metrics) {
        std::vector<PublicationValue> row{PublicationText(metric.first)};
        for (const RegressionComparisonModel &model : state.models)
            row.push_back(PublicationNumber(metric.second(model)));
        table.rows.push_back(std::move(row));
    }
    if (!state.note.empty()) table.footnotes.push_back(state.note);
    output.publication.table = std::move(table);
    output.publication.availableBackends = {
        PublicationBackend::Tinytable, PublicationBackend::Latex,
        PublicationBackend::LatexPdf
    };
    return output;
}

OutputCodeReference GeneralizedComparisonCodeReference(
    const GeneralizedComparisonState &state)
{
    OutputCodeReference output;
    output.outputId = state.id;
    output.analysisId = state.provenance.analysisId;
    output.outputBlockId = "comparison";
    output.title = StatisticalModelComparisonTitle(
        state.modelType, state.multipleImputation);
    output.kind = "table";
    output.provenance = state.provenance;

    PublicationTableSpec table;
    table.title = output.title;
    table.subtitle = state.dataScope.sourceDescription;
    table.stubColumns = {0};
    table.columns.push_back({"term", "Term", "", -1, false});
    for (std::size_t modelIndex = 0; modelIndex < state.models.size(); ++modelIndex) {
        const std::string key = "model_" + std::to_string(modelIndex + 1);
        const std::string label = state.models[modelIndex].label.empty()
            ? "Model " + std::to_string(modelIndex + 1)
            : state.models[modelIndex].label;
        table.columns.push_back({key, label, "", 4, false});
    }
    for (const std::string &term : state.termRows) {
        std::vector<PublicationValue> row{PublicationText(term)};
        for (const GeneralizedComparisonModel &model : state.models) {
            double estimate = NAN;
            const auto found = std::find_if(
                model.fit.rows.begin(), model.fit.rows.end(),
                [&](const GeneralizedGLMRow &coefficient) {
                    return coefficient.term == term || coefficient.displayLabel == term;
                });
            if (found != model.fit.rows.end()) estimate = found->estimate;
            row.push_back(PublicationNumber(estimate));
        }
        table.rows.push_back(std::move(row));
    }
    const std::vector<std::pair<std::string, std::function<double(const GeneralizedComparisonModel &)>>> metrics = {
        {"N", [](const GeneralizedComparisonModel &model) { return static_cast<double>(model.fit.n); }},
        {"Log likelihood", [](const GeneralizedComparisonModel &model) { return model.fit.logLik; }},
        {"AIC", [](const GeneralizedComparisonModel &model) { return model.fit.aic; }},
        {"BIC", [](const GeneralizedComparisonModel &model) { return model.fit.bic; }},
        {"Residual deviance", [](const GeneralizedComparisonModel &model) { return model.fit.residualDeviance; }},
        {"df residual", [](const GeneralizedComparisonModel &model) { return static_cast<double>(model.fit.dfResidual); }},
        {"Comparison statistic", [](const GeneralizedComparisonModel &model) { return model.comparisonStatistic; }},
        {"p vs previous", [](const GeneralizedComparisonModel &model) { return model.comparisonP; }}
    };
    for (const auto &metric : metrics) {
        std::vector<PublicationValue> row{PublicationText(metric.first)};
        for (const GeneralizedComparisonModel &model : state.models)
            row.push_back(PublicationNumber(metric.second(model)));
        table.rows.push_back(std::move(row));
    }
    output.publication.table = std::move(table);
    output.publication.availableBackends = {
        PublicationBackend::Tinytable, PublicationBackend::Latex,
        PublicationBackend::LatexPdf
    };
    return output;
}

OutputCodeReference Table1CodeReference(const Table1DisplayState &state,
                                        const AnalysisProvenance &provenance)
{
    OutputCodeReference output;
    output.outputId = state.id;
    output.analysisId = provenance.analysisId;
    output.outputBlockId = "table";
    output.title = state.title;
    output.kind = "table";
    output.provenance = provenance;

    PublicationTableSpec table;
    // "Table 1" identifies this analysis in LinkEDA; it is not a fixed
    // publication number.  Keep the analysis title in the window/output
    // metadata, but leave numbering to the destination document.
    table.title = PublicationTitleWithoutTableLabel(state.title);
    table.subtitle = state.dataScope.sourceDescription;
    table.stubColumns = {0};
    table.columns.push_back({"statistic", state.tableType == "mi_contingency"
        ? "Category / statistic" : state.tableType=="missing_data_test" ? "Test" : "Variable / statistic", "", -1, false});
    for (std::size_t column = 0; column < state.columns.size(); ++column)
        table.columns.push_back({"value_" + std::to_string(column + 1),
                                 state.columns[column], "", 4, false});
    if (state.tableType == "missing_data_test" && state.showP)
        table.columns.push_back({"p", "p", "", 3, false});
    auto numeric = [](const std::map<std::string, double> &components,
                      const std::string &key) {
        auto found = components.find(key);
        return found == components.end() ? NAN : found->second;
    };
    auto addMetric = [&](const Table1DisplayRow &source,
                         const std::string &suffix,
                         const std::string &key) {
        std::vector<PublicationValue> row{PublicationText(
            suffix.empty() ? source.label : source.label + " — " + suffix)};
        for (std::size_t column = 0; column < state.columns.size(); ++column) {
            row.push_back(PublicationNumber(column < source.rawStatisticValues.size()
                ? numeric(source.rawStatisticValues[column], key) : NAN));
        }
        table.rows.push_back(std::move(row));
    };
    if (state.tableType == "nested_contingency") {
        table.columns.clear(); table.stubColumns.clear();
        for (size_t j=0;j<state.stubHeaders.size();++j) {
            table.stubColumns.push_back(j);
            table.columns.push_back({"category_"+std::to_string(j), state.stubHeaders[j], "", -1, false});
        }
        for (size_t j=0;j<state.columns.size();++j) {
            if(state.nestedDisplayMode!="percent") table.columns.push_back({"n_"+std::to_string(j), "n", state.columns[j], 0, false});
            if(state.nestedDisplayMode!="count") table.columns.push_back({"percent_"+std::to_string(j), "%", state.columns[j], 1, false});
        }
        for(const auto& row:state.rows) {
            std::vector<PublicationValue> cells;
            for(size_t j=0;j<state.stubHeaders.size();++j)
                cells.push_back(PublicationText(j<row.stubValues.size()?row.stubValues[j]:""));
            for(size_t j=0;j<state.columns.size();++j) {
                const std::map<std::string,double> empty;
                const auto& raw=j<row.rawStatisticValues.size()?row.rawStatisticValues[j]:empty;
                if(state.nestedDisplayMode!="percent") cells.push_back(PublicationNumber(numeric(raw,"n")));
                if(state.nestedDisplayMode!="count") cells.push_back(PublicationNumber(100*numeric(raw,"percent")));
            }
            table.rows.push_back(std::move(cells));
        }
    }
    for (const Table1DisplayRow &row : state.rows) {
        if (state.tableType == "nested_contingency") continue;
        if (state.tableType == "frequency") {
            std::vector<PublicationValue> cells{PublicationText(row.label)};
            for (std::size_t column=0; column<state.columns.size(); ++column)
                cells.push_back(PublicationNumber(column<row.rawStatisticValues.size()
                    ? numeric(row.rawStatisticValues[column], "value") : NAN));
            table.rows.push_back(std::move(cells));
        } else if (state.tableType == "mi_diagnostics" || state.tableType == "mi_missing_information" ||
            state.tableType == "missing_data_overview" || state.tableType == "missing_data_test") {
            std::vector<PublicationValue> cells{PublicationText(
                (Table1RowIsIndented(row) ? "  " : "") + row.label)};
            for (const auto &value : row.values) cells.push_back(PublicationText(value));
            if (state.tableType == "missing_data_test" && state.showP) cells.push_back(PublicationText(row.p));
            table.rows.push_back(std::move(cells));
        } else if (row.rowType == "numeric_mean_sd") {
            addMetric(row, "Mean", "mean");
            addMetric(row, "SD", "sd");
        } else if (row.rowType == "numeric_median_iqr" ||
                   row.rowType == "ordinal_median_iqr") {
            addMetric(row, "Median", "median");
            addMetric(row, "Q1", "q1");
            addMetric(row, "Q3", "q3");
        } else if (row.rowType.find("_level") != std::string::npos ||
                   row.rowType == "missing" ||
                   (state.tableType == "mi_contingency" && row.rowType == "nested_total")) {
            addMetric(row, state.tableType == "mi_contingency" ? "Mean count" : "n", "n");
            addMetric(row, state.tableType == "mi_contingency"
                ? (state.groupVariable.empty() ? "Mean proportion" : "Mean row proportion") : "Proportion", "percent");
        }
    }
    table.footnotes = state.footnotes;
    output.publication.table = std::move(table);
    output.publication.availableBackends = {
        PublicationBackend::Tinytable, PublicationBackend::Latex,
        PublicationBackend::LatexPdf
    };
    return output;
}

OutputCodeReference DimensionalityCodeReference(
    const DimensionalityState &state,
    const AnalysisProvenance &provenance)
{
    OutputCodeReference output;
    output.outputId = state.id;
    output.analysisId = provenance.analysisId;
    output.outputBlockId = "table";
    output.title = state.method == "factor" ? "Factor Analysis" : "Principal Components";
    output.kind = "table";
    output.provenance = provenance;

    PublicationTableSpec table;
    table.title = output.title;
    table.subtitle = state.dataScope.sourceDescription;
    table.stubColumns = {0};
    table.columns.push_back({"statistic", "Variable / statistic", "", -1, false});
    const int retained = std::max(1, state.componentCount);
    const std::string prefix = DimensionalityComponentPrefix(state.method);
    for (int component = 0; component < retained; ++component)
        table.columns.push_back({"dimension_" + std::to_string(component + 1),
                                 prefix + std::to_string(component + 1), "", 3, false});
    table.columns.push_back({"h2", "h2", "", 3, false});
    table.columns.push_back({"u2", "u2", "", 3, false});

    auto componentStatistic = [&](const std::string &label,
                                  const std::function<double(const DimensionalityFitComponent &)> &value) {
        std::vector<PublicationValue> row{PublicationText(label)};
        for (int component = 0; component < retained; ++component) {
            row.push_back(component < static_cast<int>(state.components.size())
                ? PublicationNumber(value(state.components[static_cast<std::size_t>(component)]))
                : PublicationNumber(NAN));
        }
        row.push_back(PublicationNumber(NAN));
        row.push_back(PublicationNumber(NAN));
        table.rows.push_back(std::move(row));
    };
    componentStatistic("Eigenvalue", [](const DimensionalityFitComponent &value) {
        return value.eigenvalue;
    });
    componentStatistic("Parallel-analysis reference", [](const DimensionalityFitComponent &value) {
        return value.parallelEigenvalue;
    });
    componentStatistic("Proportion of variance", [](const DimensionalityFitComponent &value) {
        return value.variance;
    });
    componentStatistic("Cumulative proportion", [](const DimensionalityFitComponent &value) {
        return value.cumulative;
    });
    for (const DimensionalityFitLoading &loading : state.loadings) {
        std::vector<PublicationValue> row{PublicationText(loading.variable)};
        for (int component = 0; component < retained; ++component) {
            row.push_back(component < static_cast<int>(loading.values.size())
                ? PublicationNumber(loading.values[static_cast<std::size_t>(component)])
                : PublicationNumber(NAN));
        }
        row.push_back(PublicationNumber(loading.communality));
        row.push_back(PublicationNumber(loading.uniqueness));
        table.rows.push_back(std::move(row));
    }
    const DimensionalityReportState report = BuildDimensionalityReportState(
        state.variables, state.components, state.loadings,
        state.rowsUsed.size(), state.rowsExcluded.size(), state.method,
        state.componentCount, state.status, 0, "", 8,
        state.missingMode, state.rotation, state.scale,
        state.multipleImputation, state.displayedImputation,
        state.imputationCount, state.calculationMethod);
    table.footnotes.push_back(report.calculationMethod);
    if (!report.calculationImputation.empty())
        table.footnotes.push_back(report.calculationImputation);
    table.footnotes.push_back("h2 = communality; u2 = uniqueness.");
    output.publication.table = std::move(table);
    output.publication.availableBackends = {
        PublicationBackend::Tinytable, PublicationBackend::Latex,
        PublicationBackend::LatexPdf
    };
    return output;
}

std::string RCharacterVectorLiteral(const std::vector<std::string> &values)
{
    std::ostringstream out;
    out << "c(";
    for (std::size_t index = 0; index < values.size(); ++index) {
        if (index) out << ", ";
        out << ProvenanceRStringLiteral(values[index]);
    }
    out << ')';
    return out.str();
}

AnalysisProvenance ReconstructedVerificationProvenance(
    const DataFrameModel &dataframe,
    const AnalysisScope &scope,
    const std::string &analysisId,
    const std::string &title,
    const std::string &outputBlock,
    const std::string &verificationCode,
    std::vector<std::string> variables,
    std::vector<std::string> warnings = {})
{
    AnalysisProvenance provenance;
    provenance.analysisId = analysisId;
    provenance.title = title;
    provenance.dataVersion = dataframe.provenance.currentVersion;
    if (provenance.dataVersion.datasetId.empty()) {
        provenance.dataVersion = DefaultDataVersionReference(
            dataframe.group, dataframe.datasetType, dataframe.imputationCount,
            dataframe.provenance.currentVersion.version);
    }
    provenance.dataVersion.displayName = dataframe.group;
    provenance.scope = CaptureImmutableAnalysisScope(
        scope, provenance.dataVersion, dataframe.stableRowIds);
    provenance.codeOrigin = RCodeOrigin::Reconstructed;
    provenance.executedRCode = verificationCode;
    provenance.verificationRCode[outputBlock] = verificationCode;
    std::sort(variables.begin(), variables.end());
    variables.erase(std::remove_if(variables.begin(), variables.end(),
        [](const std::string &value) { return value.empty(); }), variables.end());
    variables.erase(std::unique(variables.begin(), variables.end()), variables.end());
    provenance.verificationVariables = std::move(variables);
    provenance.verificationWarnings = std::move(warnings);
    return provenance;
}

double ScaleAnalysisNumber(const std::string &text)
{
    if (text.empty()) return NAN;
    char *end = nullptr;
    const double value = std::strtod(text.c_str(), &end);
    return end && end != text.c_str() && *end == '\0' ? value : NAN;
}

OutputCodeReference ScaleAnalysisCodeReference(
    const ScaleAnalysisState &state,
    const DataFrameModel &dataframe)
{
    const bool multipleImputation = state.result.imputationCount > 1 ||
        dataframe.datasetType == "multiple_imputation";
    std::vector<std::string> variables;
    std::vector<std::string> types;
    std::vector<std::string> reversed;
    std::vector<std::string> rangeNames;
    std::vector<std::string> rangeValues;
    for (const ScaleItemSpecification &item : state.specification.items) {
        variables.push_back(item.variable);
        types.push_back(item.type == ScaleItemType::Ordinal ? "ordinal" : "numeric");
        if (item.reversed) reversed.push_back(item.variable);
        if (item.hasScoringRange) {
            rangeNames.push_back(item.variable);
            std::ostringstream value;
            value << "c(" << item.scoringMinimum << ", " << item.scoringMaximum << ')';
            rangeValues.push_back(value.str());
        }
    }

    std::ostringstream ranges;
    ranges << "list(";
    for (std::size_t index = 0; index < rangeNames.size(); ++index) {
        if (index) ranges << ", ";
        ranges << RNameLiteral(rangeNames[index]) << " = " << rangeValues[index];
    }
    ranges << ')';

    const std::string requestedCorrelation = state.specification.correlationBasis.empty()
        ? "auto" : state.specification.correlationBasis;
    const int minimumValidItems = state.specification.minimumValidItems > 0
        ? state.specification.minimumValidItems
        : static_cast<int>(variables.size());
    const int retainedDimensions = state.specification.factorCount > 0
        ? state.specification.factorCount : 0;
    std::ostringstream ordinalLevels;
    ordinalLevels << "list(";
    bool firstOrdinal = true;
    for (const auto &series : state.result.plotSeries) {
        if (series.kind != "ordinal_item_levels") continue;
        if (!firstOrdinal) ordinalLevels << ", ";
        ordinalLevels << RNameLiteral(series.name) << " = " << RCharacterVectorLiteral(series.labels);
        firstOrdinal = false;
    }
    ordinalLevels << ')';
    std::ostringstream code;
    code << "# Recalculate Scale Analysis from LinkEDA's prepared data.\n"
            "if (!requireNamespace(\"psych\", quietly = TRUE))\n"
            "  stop(\"Install package 'psych' to run this check.\")\n"
            "analysis_variables <- " << RCharacterVectorLiteral(variables) << "\n"
            "item_types <- stats::setNames(" << RCharacterVectorLiteral(types)
         << ", analysis_variables)\n"
            "reverse_items <- " << RCharacterVectorLiteral(reversed) << "\n"
            "scoring_ranges <- " << ranges.str() << "\n"
            "ordinal_levels <- " << ordinalLevels.str() << "\n"
            "correlation_method <- " << ProvenanceRStringLiteral(requestedCorrelation) << "\n"
            "score_method <- " << ProvenanceRStringLiteral(state.specification.scoreMethod) << "\n"
            "minimum_valid_items <- " << minimumValidItems << "L\n"
            "compute_omega <- " << (state.specification.computeOmega ? "TRUE" : "FALSE") << "\n"
            "run_dimensionality <- " << (state.specification.showDimensionality ? "TRUE" : "FALSE") << "\n"
            "dimensionality_method <- "
         << ProvenanceRStringLiteral(state.specification.dimensionalityMethod) << "\n"
            "dimensionality_missing <- "
         << ProvenanceRStringLiteral(state.specification.dimensionalityMissingMode) << "\n"
            "standardize_dimensions <- "
         << (state.specification.dimensionalityScale ? "TRUE" : "FALSE") << "\n"
            "retained_dimensions <- " << retainedDimensions << "L\n"
            "extraction_method <- " << ProvenanceRStringLiteral(state.specification.extraction) << "\n"
            "rotation_method <- " << ProvenanceRStringLiteral(state.specification.rotation) << "\n"
            "parallel_iterations <- " << std::max(1, state.specification.parallelIterations) << "L\n"
            "prepare_scale_items <- function(data) {\n"
            "  x <- data[, analysis_variables, drop = FALSE]\n"
            "  for (variable in analysis_variables) {\n"
            "    if (item_types[[variable]] == \"ordinal\") {\n"
            "      value <- x[[variable]]\n"
            "      levels_now <- ordinal_levels[[variable]]\n"
            "      if (is.null(levels_now)) levels_now <- if (is.factor(value)) levels(value) else sort(unique(value[!is.na(value)]))\n"
            "      if (any(!is.na(value) & !as.character(value) %in% levels_now)) stop(\"Unknown ordinal category in verification data.\")\n"
            "      x[[variable]] <- as.numeric(ordered(value, levels = levels_now))\n"
            "      if (variable %in% reverse_items)\n"
            "        x[[variable]] <- length(levels_now) + 1 - x[[variable]]\n"
            "    } else {\n"
            "      value <- x[[variable]]\n"
            "      if (is.numeric(value)) x[[variable]] <- as.numeric(value) else {\n"
            "        converted <- suppressWarnings(as.numeric(as.character(value)))\n"
            "        if (any(is.na(converted) & !is.na(value))) {\n"
            "          observed <- unique(as.character(value[!is.na(value)]))\n"
            "          if (length(observed) != 2L) stop(sprintf(\"Cannot encode item '%s' as numeric.\", variable))\n"
            "          encoding_levels <- if (is.factor(value)) intersect(levels(value), observed) else sort(observed)\n"
            "          converted <- match(as.character(value), encoding_levels) - 1L\n"
            "        }\n"
            "        x[[variable]] <- converted\n"
            "      }\n"
            "      if (variable %in% reverse_items) {\n"
            "        limits <- scoring_ranges[[variable]]\n"
            "        if (is.null(limits)) stop(sprintf(\"A theoretical scoring range is required for reverse-scored numeric item '%s'.\", variable))\n"
            "        x[[variable]] <- sum(limits) - x[[variable]]\n"
            "      }\n"
            "    }\n"
            "  }\n"
            "  x\n"
            "}\n"
            "scale_correlations <- function(x) {\n"
            "  method <- correlation_method\n"
            "  if (method == \"auto\") {\n"
            "    method <- if (all(item_types == \"numeric\")) \"pearson\" else\n"
            "      if (all(item_types == \"ordinal\")) \"polychoric\" else \"mixed\"\n"
            "  }\n"
            "  matrix <- if (method == \"pearson\")\n"
            "    psych::corr.test(x, use = \"pairwise\", adjust = \"none\", ci = FALSE)$r else\n"
            "    if (method == \"polychoric\") psych::polychoric(x, smooth = FALSE)$rho else\n"
            "      psych::mixedCor(x, c = which(item_types == \"numeric\"),\n"
            "                      p = which(item_types == \"ordinal\"), smooth = FALSE)$rho\n"
            "  sample_size <- outer(seq_len(ncol(x)), seq_len(ncol(x)), Vectorize(function(i, j)\n"
            "    sum(stats::complete.cases(x[, c(i, j), drop = FALSE]))))\n"
            "  dimnames(sample_size) <- dimnames(matrix)\n"
            "  list(matrix = matrix, sample_size = sample_size, method = method)\n"
            "}\n"
            "parallel_reference <- function(r, n, method, extraction, iterations,\n"
            "                               data = NULL, covariance = FALSE, missing = \"pairwise\") {\n"
            "  old_seed <- if (exists(\".Random.seed\", envir = .GlobalEnv, inherits = FALSE))\n"
            "    get(\".Random.seed\", envir = .GlobalEnv) else NULL\n"
            "  old_options <- options(mc.cores = 1L)\n"
            "  on.exit({\n"
            "    options(old_options)\n"
            "    if (is.null(old_seed)) {\n"
            "      if (exists(\".Random.seed\", envir = .GlobalEnv, inherits = FALSE))\n"
            "        rm(\".Random.seed\", envir = .GlobalEnv)\n"
            "    } else assign(\".Random.seed\", old_seed, envir = .GlobalEnv)\n"
            "  }, add = TRUE)\n"
            "  set.seed(271828L)\n"
            "  raw_covariance <- covariance && method == \"pca\"\n"
            "  if (raw_covariance) {\n"
            "    if (is.null(data)) stop(\"Covariance PCA parallel analysis requires the source observations.\")\n"
            "    data <- as.data.frame(data)\n"
            "    data <- data[if (missing == \"listwise\") stats::complete.cases(data) else\n"
            "      rowSums(!is.na(data)) > 0L, , drop = FALSE]\n"
            "    utils::capture.output(result <- psych::fa.parallel(data, fa = \"pc\", cor = \"cov\",\n"
            "      sim = FALSE, SMC = TRUE, use = if (missing == \"listwise\") \"complete\" else \"pairwise\",\n"
            "      n.iter = iterations, quant = .95, plot = FALSE))\n"
            "  } else {\n"
            "    # Factor parallel analysis uses reduced correlations, including for covariance fits.\n"
            "    utils::capture.output(result <- psych::fa.parallel(stats::cov2cor(r), n.obs = n,\n"
            "      fm = extraction, fa = if (method == \"pca\") \"pc\" else \"fa\",\n"
            "      SMC = method == \"pca\", n.iter = iterations, quant = .95, plot = FALSE))\n"
            "  }\n"
            "  columns <- seq_len(ncol(r)) + if (method == \"pca\") 0L else ncol(r)\n"
            "  result$reference <- as.numeric(apply(result$values[, columns, drop = FALSE],\n"
            "    2L, stats::quantile, probs = .95, names = FALSE))\n"
            "  result$reference_method <- if (raw_covariance) \"column resampling (covariance units)\" else\n"
            "    if (method == \"factor\") \"normal simulations (reduced correlations)\" else \"normal simulations (correlations)\"\n"
            "  result\n"
            "}\n"
            "fit_scale <- function(data) {\n"
            "  x <- prepare_scale_items(data)\n"
            "  alpha_fit <- suppressWarnings(psych::alpha(\n"
            "    x, check.keys = FALSE, warnings = FALSE, delete = TRUE, use = \"pairwise\"))\n"
            "  valid_items <- rowSums(!is.na(x))\n"
            "  score <- if (score_method == \"sum\") rowSums(x, na.rm = TRUE) else rowMeans(x, na.rm = TRUE)\n"
            "  score[valid_items < minimum_valid_items] <- NA_real_\n"
            "  omega_total <- NA_real_\n"
            "  if (compute_omega && ncol(x) >= 3L) {\n"
            "    omega_fit <- suppressMessages(suppressWarnings(\n"
            "      psych::omega(x, plot = FALSE, warnings = FALSE, flip = FALSE)))\n"
            "    omega_total <- unname(omega_fit$omega.tot)\n"
            "  }\n"
            "  item_table <- data.frame(\n"
            "    Item = names(x), Type = unname(item_types[names(x)]),\n"
            "    Mean = as.numeric(alpha_fit$item.stats[names(x), \"mean\"]),\n"
            "    SD = as.numeric(alpha_fit$item.stats[names(x), \"sd\"]),\n"
            "    Missing_percent = 100 * colMeans(is.na(x)),\n"
            "    Item_rest_r = as.numeric(alpha_fit$item.stats[names(x), \"r.drop\"]),\n"
            "    Alpha_if_deleted = as.numeric(alpha_fit$alpha.drop[names(x), \"raw_alpha\"]),\n"
            "    check.names = FALSE)\n"
            "  summary_table <- data.frame(\n"
            "    Alpha = unname(alpha_fit$total[[\"raw_alpha\"]]),\n"
            "    Standardized_alpha = unname(alpha_fit$total[[\"std.alpha\"]]),\n"
            "    Mean_inter_item_r = unname(alpha_fit$total[[\"average_r\"]]),\n"
            "    Scale_mean = unname(alpha_fit$total[[\"mean\"]]),\n"
            "    Scale_SD = unname(alpha_fit$total[[\"sd\"]]),\n"
            "    Omega_total = omega_total, Valid_scores = sum(is.finite(score)),\n"
            "    check.names = FALSE)\n"
            "  dimensionality <- NULL\n"
            "  if (run_dimensionality) {\n"
            "    x_dimension <- if (dimensionality_missing == \"listwise\")\n"
            "      x[stats::complete.cases(x), , drop = FALSE] else x\n"
            "    correlation <- scale_correlations(x_dimension)\n"
            "    covariance_available <- correlation$method == \"pearson\" && all(item_types == \"numeric\")\n"
            "    use_covariance <- !standardize_dimensions && covariance_available\n"
            "    rho <- if (use_covariance) stats::cov(x_dimension, use = if (dimensionality_missing == \"listwise\") \"complete.obs\" else \"pairwise.complete.obs\") else correlation$matrix\n"
            "    n_observations <- if (dimensionality_missing == \"listwise\") nrow(x_dimension) else min(correlation$sample_size)\n"
            "    # PCA permits semidefinite matrices for loadings; FA also requires invertibility.\n"
            "    matrix_ok <- all(is.finite(rho)) && all(diag(rho) > 0) && isTRUE(isSymmetric(unname(rho)))\n"
            "    if (matrix_ok) {\n"
            "      correlation_check <- stats::cov2cor(rho)\n"
            "      ev <- eigen(correlation_check, symmetric = TRUE, only.values = TRUE)$values\n"
            "      matrix_ok <- min(ev) >= -max(abs(ev)) * sqrt(.Machine$double.eps)\n"
            "      if (dimensionality_method == \"factor\") matrix_ok <- matrix_ok && min(ev) > 0 &&\n"
            "        rcond(correlation_check) > sqrt(.Machine$double.eps)\n"
            "    }\n"
            "    if (!matrix_ok) {\n"
            "      dimensionality <- list(status = \"Invalid correlation matrix; no matrix correction was applied.\")\n"
            "    } else {\n"
            "      parallel <- parallel_reference(rho, n_observations, dimensionality_method,\n"
            "        extraction_method, parallel_iterations, data = x_dimension,\n"
            "        covariance = use_covariance, missing = dimensionality_missing)\n"
            "      dimensions <- if (retained_dimensions > 0L) retained_dimensions else\n"
            "        if (dimensionality_method == \"pca\") parallel$ncomp else parallel$nfact\n"
            "      if (!length(dimensions) || is.na(dimensions) || dimensions == 0L) {\n"
            "        dimensionality <- list(factors = dimensions, loadings = NULL, scores = NULL,\n"
            "          status = if (length(dimensions) && !is.na(dimensions) && dimensions == 0L)\n"
            "            \"Parallel analysis recommends 0 dimensions; no extraction was performed.\"\n"
            "          else \"Automatic dimension count unavailable; no extraction was performed.\")\n"
            "      } else {\n"
            "        dimensions <- max(1L, min(dimensions, ncol(rho) - as.integer(dimensionality_method == \"factor\")))\n"
            "        dimensionality <- if (dimensionality_method == \"pca\")\n"
            "          psych::principal(rho, nfactors = dimensions, rotate = rotation_method, n.obs = n_observations, covar = use_covariance) else\n"
            "          psych::fa(rho, nfactors = dimensions, rotate = rotation_method, fm = extraction_method, n.obs = n_observations, covar = use_covariance)\n"
            "      }\n"
            "      dimensionality$parallel <- parallel\n"
            "      if (dimensionality_method == \"factor\" && !is.null(dimensionality$loadings) &&\n"
            "          all(is.finite(dimensionality$uniquenesses)) && all(dimensionality$uniquenesses >= 0) &&\n"
            "          !identical(dimensionality$converged, FALSE)) {\n"
            "        complete <- stats::complete.cases(x_dimension)\n"
            "        dimensionality$scores <- psych::factor.scores(x_dimension[complete, , drop = FALSE],\n"
            "          f = dimensionality, method = \"Thurstone\", rho = rho, missing = FALSE)$scores\n"
            "      }\n"
            "    }\n"
            "  }\n"
            "  list(summary = summary_table, items = item_table,\n"
            "       correlations = scale_correlations(x), scores = score,\n"
            "       sum_scores = replace(rowSums(x, na.rm = TRUE), valid_items < minimum_valid_items, NA_real_),\n"
            "       mean_scores = replace(rowMeans(x, na.rm = TRUE), valid_items < minimum_valid_items, NA_real_),\n"
            "       dimensionality = dimensionality)\n"
            "}\n"
            "prepared_data <- base::readRDS(verification_data_path)\n";
    if (multipleImputation) {
        code << "if (!requireNamespace(\"mice\", quietly = TRUE))\n"
                "  stop(\"Install package 'mice' to pool the multiple-imputation checks.\")\n"
                "stopifnot(all(c(\".imp\", \".id\") %in% names(prepared_data)))\n"
                "completed_sets <- split(prepared_data[prepared_data$.imp > 0L, , drop = FALSE],\n"
                "                        prepared_data$.imp[prepared_data$.imp > 0L])\n"
                "completed_sets <- lapply(completed_sets, function(data) {\n"
                "  data$.imp <- NULL; data$.id <- NULL; rownames(data) <- NULL; data\n"
                "})\n"
                "scale_results <- lapply(completed_sets, fit_scale)\n"
                "reliability_by_imputation <- do.call(rbind, lapply(seq_along(scale_results), function(i)\n"
                "  cbind(Imputation = i, scale_results[[i]]$summary)))\n"
                "item_results_by_imputation <- lapply(scale_results, `[[`, \"items\")\n"
                "correlations_by_imputation <- lapply(scale_results, `[[`, \"correlations\")\n"
                "pool_fisher <- function(r, n) {\n"
                "  if (length(r) < 2L || length(r) != length(n) ||\n"
                "      any(!is.finite(r) | abs(r) >= 1) || any(!is.finite(n) | n <= 3))\n"
                "    return(list(qbar = NA_real_, t = NA_real_))\n"
                "  mice::pool.scalar(atanh(r), 1 / (n - 3), n = Inf, k = 1, rule = \"rubin1987\")\n"
                "}\n"
                "# Unavailable Fisher-z correlations remain NA; imputations are never dropped or clipped.\n"
                "pool_item <- function(item) {\n"
                "  rows <- lapply(item_results_by_imputation, function(table) table[table$Item == item, , drop = FALSE])\n"
                "  means <- vapply(rows, function(row) row$Mean, numeric(1L))\n"
                "  sds <- vapply(rows, function(row) row$SD, numeric(1L))\n"
                "  n <- vapply(completed_sets, function(data) sum(!is.na(data[[item]])), numeric(1L))\n"
                "  pooled_mean <- mice::pool.scalar(means, sds^2 / pmax(n, 1))\n"
                "  item_rest <- vapply(rows, function(row) row$Item_rest_r, numeric(1L))\n"
                "  pooled_rest <- pool_fisher(item_rest, n)\n"
                "  data.frame(Item = item, Mean = pooled_mean$qbar, Mean_SE = sqrt(pooled_mean$t),\n"
                "             SD = mean(sds), Item_rest_r = tanh(pooled_rest$qbar),\n"
                "             Alpha_if_deleted = mean(vapply(rows, function(row) row$Alpha_if_deleted, numeric(1L))),\n"
                "             check.names = FALSE)\n"
                "}\n"
                "pooled_item_table <- do.call(rbind, lapply(analysis_variables, pool_item))\n"
                "pooled_correlations <- NULL\n"
                "if (all(vapply(correlations_by_imputation, function(x) x$method == \"pearson\", logical(1L)))) {\n"
                "  pooled_correlations <- diag(1, length(analysis_variables))\n"
                "  dimnames(pooled_correlations) <- list(analysis_variables, analysis_variables)\n"
                "  if (length(analysis_variables) > 1L) for (i in seq_len(length(analysis_variables) - 1L))\n"
                "    for (j in seq.int(i + 1L, length(analysis_variables))) {\n"
                "      values <- vapply(correlations_by_imputation, function(x) x$matrix[i, j], numeric(1L))\n"
                "      n <- vapply(correlations_by_imputation, function(x) x$sample_size[i, j], numeric(1L))\n"
                "      pooled <- pool_fisher(values, n)\n"
                "      pooled_correlations[i, j] <- pooled_correlations[j, i] <- tanh(pooled$qbar)\n"
                "    }\n"
                "} else {\n"
                "  pooled_correlations <- Reduce(`+`, lapply(correlations_by_imputation, `[[`, \"matrix\")) /\n"
                "    length(correlations_by_imputation)\n"
                "}\n"
                "# Alpha, omega, SDs, and rotated loadings are reported by imputation; they are not Rubin-pooled.\n"
                "reliability_by_imputation\n"
                "pooled_item_table\n"
                "pooled_correlations\n"
                "lapply(scale_results, `[[`, \"dimensionality\")\n";
    } else {
        code << "scale_result <- fit_scale(prepared_data)\n"
                "scale_result$summary\n"
                "scale_result$items\n"
                "scale_result$correlations$matrix\n"
                "scale_result$dimensionality\n";
    }

    const AnalysisScope scope = state.dataScopeCaptured
        ? state.dataScope
        : AllObservationsAnalysisScope(
            state.group, static_cast<std::size_t>(std::max(0, dataframe.rows)));
    OutputCodeReference output;
    output.outputId = state.id;
    output.analysisId = state.id;
    output.outputBlockId = "table";
    output.title = ScaleAnalysisWindowTitle(multipleImputation);
    output.kind = "table";
    output.provenance = ReconstructedVerificationProvenance(
        dataframe, scope, state.id, output.title, "table", code.str(), variables,
        multipleImputation
            ? std::vector<std::string>{
                "Reliability coefficients, scale-score summaries, and dimensionality are calculated separately in every completed dataset; they are not Rubin-pooled.",
                "Pearson correlations and item-level scalar estimates use Rubin-compatible pooling in LinkEDA; the recipe exposes the per-imputation inputs for checking those calculations."}
            : std::vector<std::string>{});

    PublicationTableSpec table;
    table.title = output.title;
    table.subtitle = multipleImputation
        ? "Reliability and item analysis across completed imputations"
        : "Reliability and item analysis";
    table.stubColumns = {0};
    table.columns = {
        {"item", "Item", "", -1, false},
        {"type", "Type", "", -1, false},
        {"direction", "Scoring", "", -1, false},
        {"mean", "Mean", "", 3, false},
        {"sd", "SD", "", 3, false},
        {"missing", "Missing (%)", "", 1, false},
        {"item_rest", "Item-rest r", "", 3, false},
        {"alpha_deleted", "Alpha if deleted", "", 3, false}
    };
    for (const ScaleItemResultRow &row : state.result.items) {
        table.rows.push_back({
            PublicationText(row.variable), PublicationText(row.type),
            PublicationText(row.direction), PublicationNumber(ScaleAnalysisNumber(row.mean)),
            PublicationNumber(ScaleAnalysisNumber(row.sd)),
            PublicationNumber(ScaleAnalysisNumber(row.missingPercent)),
            PublicationNumber(ScaleAnalysisNumber(row.itemRestCorrelation)),
            PublicationNumber(ScaleAnalysisNumber(row.alphaIfDeleted))
        });
    }
    std::ostringstream reliability;
    reliability << "Alpha = " << state.result.scaleSummary.alpha
                << "; standardized alpha = " << state.result.scaleSummary.standardizedAlpha
                << "; mean inter-item r = " << state.result.scaleSummary.meanInterItemCorrelation;
    if (!state.result.scaleSummary.omegaTotal.empty())
        reliability << "; omega total = " << state.result.scaleSummary.omegaTotal;
    table.footnotes.push_back(reliability.str());
    if (multipleImputation)
        table.footnotes.push_back(
            "Alpha, omega, and dimensionality summaries are descriptive across imputations and are not Rubin-pooled. "
            "Fisher-z pooling of Pearson and item-rest correlations requires at least two imputations, "
            "with n > 3 and finite |r| < 1 in every imputation; unavailable estimates are shown as dashes.");
    output.publication.table = std::move(table);
    output.publication.availableBackends = {
        PublicationBackend::Tinytable, PublicationBackend::Latex,
        PublicationBackend::LatexPdf};
    return output;
}

OutputCodeReference CorrelationCodeReference(const CorrelationMatrixState &state,
                                             const DataFrameModel &dataframe)
{
    const AnalysisScope scope = state.dataScopeCaptured
        ? state.dataScope
        : AllObservationsAnalysisScope(state.group,
              static_cast<std::size_t>(std::max(0, dataframe.rows)));
    OutputCodeReference output;
    output.outputId = state.id;
    output.analysisId = state.id;
    output.outputBlockId = "table";
    output.title = state.title;
    output.kind = "table";
    output.provenance = state.provenance;
    PublicationTableSpec table;
    table.title = state.title;
    table.subtitle = scope.sourceDescription;
    table.stubColumns = {0};
    table.columns.push_back({"variable", "Variable", "", -1, false});
    for (const std::string &variable : state.variables)
        table.columns.push_back({variable, variable, "", 3, false});
    for (std::size_t rowIndex = 0; rowIndex < state.variables.size(); ++rowIndex) {
        std::vector<PublicationValue> row{PublicationText(state.variables[rowIndex])};
        for (std::size_t columnIndex = 0; columnIndex < state.variables.size(); ++columnIndex) {
            row.push_back(PublicationText(CorrelationDisplayedCellText(
                state, static_cast<int>(rowIndex), static_cast<int>(columnIndex))));
        }
        table.rows.push_back(std::move(row));
    }
    table.footnotes.push_back(state.missingMode == "listwise"
        ? "Pearson correlations use listwise-complete observations."
        : "Pearson correlations use pairwise-complete observations.");
    if (state.showP) table.footnotes.push_back("* p < .05; ** p < .01; *** p < .001.");
    if (state.showN) table.footnotes.push_back(state.multipleImputation
        ? "N is the rounded mean of pair counts across imputations."
        : "N is the number of complete observations for each pair.");
    if (!state.note.empty()) table.footnotes.push_back(state.note);
    output.publication.table = std::move(table);
    output.publication.availableBackends = {
        PublicationBackend::Tinytable, PublicationBackend::Latex,
        PublicationBackend::LatexPdf};
    return output;
}

OutputCodeReference DendrogramCodeReference(const DendrogramState &state,
                                            const DataFrameModel &dataframe)
{
    (void)dataframe;
    OutputCodeReference output;
    output.outputId = state.id;
    output.analysisId = state.id;
    output.outputBlockId = "plot";
    output.title = "Quick Cluster";
    output.kind = "plot";
    output.provenance = state.provenance;
    return output;
}

OutputCodeReference MeanComparisonCodeReference(const MeanComparisonState &state)
{
    OutputCodeReference output;
    output.outputId = state.id;
    output.analysisId = state.provenance.analysisId;
    output.outputBlockId = "table";
    output.title = state.title;
    output.kind = "table";
    output.provenance = state.provenance;

    PublicationTableSpec table;
    table.title = state.title;
    table.subtitle = state.subtitle;
    table.stubColumns = {0, 1};
    table.columns = {
        {"section", "Section", "", -1, false},
        {"result", "Variable / comparison", "", -1, false},
        {"statistic", "Statistics", "", -1, false}
    };
    for (const MeanComparisonTable &section : state.tables) {
        for (const MeanComparisonRow &source : section.rows) {
            std::ostringstream statistics;
            for (std::size_t column = 0;
                 column < section.columns.size() && column < source.cells.size(); ++column) {
                const std::string value = MeanComparisonCellText(
                    source.cells[column], section.columns[column].format);
                if (value.empty() || value == "\u2014") continue;
                if (statistics.tellp() > 0) statistics << "; ";
                statistics << section.columns[column].title << " = " << value;
            }
            table.rows.push_back({PublicationText(section.title),
                                  PublicationText(source.label),
                                  PublicationText(statistics.str())});
        }
        table.footnotes.insert(table.footnotes.end(),
                               section.notes.begin(), section.notes.end());
        table.footnotes.insert(table.footnotes.end(),
                               section.warnings.begin(), section.warnings.end());
    }
    output.publication.table = std::move(table);
    output.publication.availableBackends = {
        PublicationBackend::Tinytable, PublicationBackend::Latex,
        PublicationBackend::LatexPdf};
    return output;
}

bool ParseScaleAnalysisPayload(const std::vector<std::string> &args,
                               ScaleAnalysisState &state,
                               bool requireCurrentIdentity,
                               std::string &error)
{
    error.clear();
    if (args.size() < 9) {
        error = "malformed SCALE_ANALYSIS result command";
        return false;
    }
    std::size_t cursor = 0;
    const std::string id = args[cursor++];
    const std::string group = args[cursor++];
    ScaleAnalysisResult result;
    result.backend = args[cursor++];
    result.imputationCount = std::max(1, std::atoi(args[cursor++].c_str()));
    result.revision = std::atoi(args[cursor++].c_str());
    result.fingerprint = args[cursor++];
    result.summary = args[cursor++];
    const long itemCount = std::strtol(args[cursor++].c_str(), nullptr, 10);
    if (id.empty() || group.empty() || result.revision < 0 ||
        result.fingerprint.empty() || itemCount < 0 ||
        cursor + static_cast<std::size_t>(itemCount) * 11U + 1U > args.size()) {
        error = "invalid SCALE_ANALYSIS result payload";
        return false;
    }
    if (requireCurrentIdentity &&
        (state.specification.revision != result.revision ||
         state.specification.fingerprint != result.fingerprint)) {
        error = "stale Scale Analysis result rejected because its immutable specification does not match";
        return false;
    }

    ScaleAnalysisSpecification specification = state.specification;
    specification.datasetId = group;
    specification.items.clear();
    result.items.clear();
    for (long index = 0; index < itemCount; ++index) {
        ScaleItemSpecification item;
        ScaleItemResultRow row;
        row.variable = args[cursor++];
        row.type = args[cursor++];
        row.direction = args[cursor++];
        const bool hasRange = args[cursor++] == "TRUE";
        const std::string minimum = args[cursor++];
        const std::string maximum = args[cursor++];
        if (!ParseScaleItemType(row.type, item.type) ||
            (row.direction != "forward" && row.direction != "reversed")) {
            error = "invalid Scale Analysis item type or direction";
            return false;
        }
        item.variable = row.variable;
        item.reversed = row.direction == "reversed";
        item.hasScoringRange = hasRange;
        if (hasRange) {
            item.scoringMinimum = std::strtod(minimum.c_str(), nullptr);
            item.scoringMaximum = std::strtod(maximum.c_str(), nullptr);
            if (!std::isfinite(item.scoringMinimum) || !std::isfinite(item.scoringMaximum) ||
                item.scoringMinimum >= item.scoringMaximum) {
                error = "invalid Scale Analysis scoring range";
                return false;
            }
        }
        row.mean = args[cursor++];
        row.sd = args[cursor++];
        row.missingPercent = args[cursor++];
        row.itemRestCorrelation = args[cursor++];
        row.alphaIfDeleted = args[cursor++];
        specification.items.push_back(std::move(item));
        result.items.push_back(std::move(row));
    }
    result.provenance = args[cursor++];
    if (cursor < args.size() && args[cursor] == "SCALE_DETAILS_V1") {
        ++cursor;
        auto require = [&](std::size_t count) {
            return cursor + count <= args.size();
        };
        auto take = [&]() -> std::string {
            return cursor < args.size() ? args[cursor++] : std::string();
        };
        if (!require(10)) {
            error = "truncated Scale Analysis summary details";
            return false;
        }
        result.scaleSummary.numberOfItems = take();
        result.scaleSummary.nUsed = take();
        result.scaleSummary.missingPercent = take();
        result.scaleSummary.scaleMean = take();
        result.scaleSummary.scaleSd = take();
        result.scaleSummary.meanInterItemCorrelation = take();
        result.scaleSummary.alpha = take();
        result.scaleSummary.standardizedAlpha = take();
        result.scaleSummary.omegaTotal = take();
        result.scaleSummary.reliabilityStatus = take();

        if (!require(1)) { error = "truncated Scale Analysis reliability details"; return false; }
        const long reliabilityCount = std::strtol(take().c_str(), nullptr, 10);
        if (reliabilityCount < 0 || !require(static_cast<std::size_t>(reliabilityCount) * 4U)) {
            error = "invalid Scale Analysis reliability details";
            return false;
        }
        for (long index = 0; index < reliabilityCount; ++index) {
            ScaleReliabilityResultRow row;
            row.imputation = take(); row.alpha = take();
            row.standardizedAlpha = take(); row.omegaTotal = take();
            result.reliabilityByImputation.push_back(std::move(row));
        }

        if (!require(3)) { error = "truncated Scale Analysis correlation details"; return false; }
        result.correlations.basis = take();
        result.correlations.status = take();
        const long correlationCount = std::strtol(take().c_str(), nullptr, 10);
        if (correlationCount < 0) { error = "invalid Scale Analysis correlation size"; return false; }
        const auto correlationSize = static_cast<std::size_t>(correlationCount);
        if (!require(correlationSize + correlationSize * correlationSize)) {
            error = "truncated Scale Analysis correlation matrix";
            return false;
        }
        for (std::size_t index = 0; index < correlationSize; ++index)
            result.correlations.variables.push_back(take());
        for (std::size_t index = 0; index < correlationSize * correlationSize; ++index)
            result.correlations.values.push_back(take());
        if (correlationSize > 0 && require(1) &&
            args[cursor] == "SCALE_CORRELATION_DETAILS_V1") {
            take();
            const std::size_t detailCount = correlationSize * correlationSize;
            if (!require(detailCount * 2U)) {
                error = "truncated Scale Analysis correlation inferential details";
                return false;
            }
            for (std::size_t index = 0; index < detailCount; ++index)
                result.correlations.pValues.push_back(take());
            for (std::size_t index = 0; index < detailCount; ++index)
                result.correlations.sampleSizes.push_back(take());
        }

        if (!require(5)) { error = "truncated Scale Analysis dimensionality details"; return false; }
        result.dimensionality.status = take();
        result.dimensionality.suggestedFactors = take();
        result.dimensionality.factorCount = take();
        const long loadingCount = std::strtol(take().c_str(), nullptr, 10);
        const long factorCount = std::strtol(take().c_str(), nullptr, 10);
        if (loadingCount < 0 || factorCount < 0 ||
            !require(static_cast<std::size_t>(factorCount))) {
            error = "invalid Scale Analysis loading dimensions";
            return false;
        }
        for (long index = 0; index < factorCount; ++index)
            result.dimensionality.factorNames.push_back(take());
        const auto fieldsPerLoading = static_cast<std::size_t>(factorCount) + 3U;
        if (!require(static_cast<std::size_t>(loadingCount) * fieldsPerLoading)) {
            error = "truncated Scale Analysis loading table";
            return false;
        }
        for (long rowIndex = 0; rowIndex < loadingCount; ++rowIndex) {
            ScaleLoadingResultRow row;
            row.item = take();
            for (long factor = 0; factor < factorCount; ++factor)
                row.loadings.push_back(take());
            row.communality = take(); row.uniqueness = take();
            result.dimensionality.loadings.push_back(std::move(row));
        }

        if (!require(7)) { error = "truncated Scale Analysis score details"; return false; }
        result.scores.method = take(); result.scores.status = take();
        result.scores.validN = take(); result.scores.mean = take();
        result.scores.sd = take(); result.scores.minimum = take();
        result.scores.maximum = take();
        // A returned result describes the estimator actually used (for
        // example, Auto may resolve to Pearson).  It must never rewrite the
        // immutable requested specification on an update.  Doing so would
        // leave the stored fingerprint describing one specification while
        // the controls described another.  OPEN payloads created directly
        // from R have no prior native specification, so only the options that
        // can be recovered without conflating requested and resolved state
        // are initialized there.
        if (!requireCurrentIdentity) {
            specification.scoreMethod = result.scores.method.empty()
                ? specification.scoreMethod : result.scores.method;
            specification.computeOmega = !result.scaleSummary.omegaTotal.empty();
            specification.showDimensionality = result.dimensionality.status != "not_requested" &&
                !result.dimensionality.status.empty();
            specification.factorCount = std::max(
                0, std::atoi(result.dimensionality.factorCount.c_str()));
        }

        if (cursor < args.size() && args[cursor] == "SCALE_PLOTS_V1") {
            ++cursor;
            if (!require(1)) {
                error = "truncated Scale Analysis plot details";
                return false;
            }
            const long seriesCount = std::strtol(take().c_str(), nullptr, 10);
            if (seriesCount < 0) {
                error = "invalid Scale Analysis plot-series count";
                return false;
            }
            for (long seriesIndex = 0; seriesIndex < seriesCount; ++seriesIndex) {
                if (!require(3)) {
                    error = "truncated Scale Analysis plot series";
                    return false;
                }
                ScalePlotSeriesResult series;
                series.kind = take();
                series.name = take();
                const long pointCount = std::strtol(take().c_str(), nullptr, 10);
                if (series.kind.empty() || pointCount < 0 ||
                    !require(static_cast<std::size_t>(pointCount) * 2U)) {
                    error = "invalid Scale Analysis plot series";
                    return false;
                }
                for (long pointIndex = 0; pointIndex < pointCount; ++pointIndex) {
                    series.labels.push_back(take());
                    series.values.push_back(take());
                }
                result.plotSeries.push_back(std::move(series));
            }
        }
    }
    if (cursor < args.size() && args[cursor] == "SCALE_SPEC_V1") {
        if (args.size() - ++cursor < 12) {
            error = "truncated Scale Analysis specification"; return false;
        }
        if (!requireCurrentIdentity) {
            specification.correlationBasis = args[cursor];
            specification.scoreMethod = args[cursor + 1];
            specification.minimumValidItems = std::atoi(args[cursor + 2].c_str());
            specification.computeOmega = args[cursor + 3] == "TRUE";
            specification.showDimensionality = args[cursor + 4] == "TRUE";
            specification.dimensionalityMethod = args[cursor + 5];
            specification.dimensionalityMissingMode = args[cursor + 6];
            specification.dimensionalityScale = args[cursor + 7] == "TRUE";
            specification.factorCount = std::atoi(args[cursor + 8].c_str());
            specification.extraction = args[cursor + 9];
            specification.rotation = args[cursor + 10];
            specification.parallelIterations = std::atoi(args[cursor + 11].c_str());
        }
        cursor += 12;
    }
    specification.revision = result.revision;
    specification.fingerprint = result.fingerprint;
    state.id = id;
    state.group = group;
    state.specification = std::move(specification);
    state.result = std::move(result);
    state.status = state.result.summary;
    state.rFitPending = false;
    state.pendingRevision = -1;
    state.pendingFingerprint.clear();
    state.modelVersion = std::max(state.modelVersion, state.specification.revision);
    return true;
}

} // namespace

PublicationTableSpec RegressionComparisonPublicationTable(const RegressionComparisonState &state)
{
    return *RegressionComparisonCodeReference(state).publication.table;
}

PublicationTableSpec GeneralizedComparisonPublicationTable(const GeneralizedComparisonState &state)
{
    return *GeneralizedComparisonCodeReference(state).publication.table;
}

PublicationTableSpec GeneralizedModelPublicationTable(const GeneralizedGLMState &state)
{
    auto table = *GeneralizedModelCodeReference(state).publication.table;
    // Use the same coefficient hierarchy and APA renderer as the linear model.
    // Show the interval on the effect scale when an exponentiated effect exists.
    for (std::size_t j = table.columns.size(); j-- > 0;) {
        if (table.columns[j].key != "type" &&
            !(table.columns[j].key == "confidence_interval" && GeneralizedModelHasExponentiatedEffect(state))) continue;
        table.columns.erase(table.columns.begin() + j);
        for (auto &row : table.rows) row.erase(row.begin() + j);
    }
    const auto *distribution = FindDistributionSpecification(state.family);
    table.subtitle = "Response: " + state.response + "; N = " + std::to_string(state.n) +
        "; Distribution: " + (distribution ? distribution->visibleName : state.family) +
        "; Link: " + state.link + "; " + state.dataScope.sourceDescription;
    if (state.binaryRegression) table.subtitle += "; Event: " + state.responseCoding.eventLabel +
        "; Reference: " + state.responseCoding.referenceLabel;
    if (!state.offsetVariable.empty())
        table.subtitle += "; Offset: " + state.offsetVariable + " (link scale)";
    // A parent-term omnibus statistic is a different quantity from the
    // coefficient t/z values on the child rows. Mark those cells explicitly.
    auto statisticColumn = std::find_if(table.columns.begin(), table.columns.end(),
        [](const PublicationTableColumn &column) { return column.key == "statistic"; });
    if (statisticColumn != table.columns.end()) {
        const std::size_t j = static_cast<std::size_t>(statisticColumn - table.columns.begin());
        statisticColumn->decimals = -1;
        for (std::size_t i = 0; i < table.rows.size() && i < state.rows.size(); ++i) {
            const auto &cell = table.rows[i][j];
            if (cell.kind != PublicationValueKind::Number || !std::isfinite(cell.number)) continue;
            const bool global = GeneralizedGlobalTermTestForPresentationRow(state, state.rows[i]) != nullptr;
            table.rows[i][j] = PublicationText(FormatDouble(cell.number, 3) + (global ? "†" : ""));
        }
    }
    table.footnotes.clear();
    if (PublicationModelHasHierarchy(state.rows)) AddModelHierarchyFootnote(table);
    if (!GeneralizedGlobalTermTestMethodNote(state).empty()) {
        std::string note = GeneralizedGlobalTermTestPooledWaldFootnote(state);
        if (note.empty()) note = "† " + GeneralizedGlobalTermTestMethodNote(state);
        table.footnotes.push_back(note);
    }
    const std::string hierarchyNote = GeneralizedGlobalTermTestHierarchyNote(state);
    if (!hierarchyNote.empty()) table.footnotes.push_back(hierarchyNote);
    if (state.multipleImputation)
        table.footnotes.push_back("Coefficient estimates and standard errors are combined across imputations using Rubin's rules.");
    if (!state.offsetVariable.empty())
        table.footnotes.push_back("Offset " + state.offsetVariable +
            " enters the linear predictor with its coefficient fixed at 1.");
    if (GeneralizedModelHasExponentiatedEffect(state)) {
        const std::string effect = GeneralizedExponentiatedEffectLabel(state);
        table.footnotes.push_back(effect == "Odds ratio"
            ? "Odds are event probability divided by non-event probability. Odds ratios are exp(b); values above 1 indicate higher odds relative to the stated reference or per one-unit increase."
            : effect + " values are exp(b); values above 1 indicate an increase relative to the stated reference or per one-unit increase.");
    }
    return table;
}

std::vector<PublicationTableSpec> GeneralizedModelReportTables(const GeneralizedGLMState &state)
{
    PublicationTableSpec fit;
    fit.title = "Model fit";
    fit.columns = {{"n", "N", "", 0, false},
        {"null", "Null deviance", "", 3, false},
        {"residual", "Residual dev.", "", 3, false},
        {"df", "df residual", "", 0, false}};
    fit.rows = {{PublicationNumber(state.n), PublicationNumber(state.nullDeviance),
        PublicationNumber(state.residualDeviance), PublicationNumber(state.dfResidual)}};
    if (state.multipleImputation)
        fit.footnotes.push_back("Likelihood statistics are not pooled across imputations.");
    if (!state.offsetVariable.empty())
        fit.footnotes.push_back("Offset: " + state.offsetVariable + " (link scale; coefficient fixed at 1).");
    if (state.countRegression) {
        const auto parameter = CountModelParameterSummary(state);
        if (!parameter.empty()) fit.footnotes.push_back(parameter);
    }
    auto terms = *GeneralizedModelCodeReference(state).publication.table;
    terms.title = "Terms"; terms.subtitle.clear();
    const std::string omnibusMethod = GeneralizedGlobalTermTestMethodNote(state);
    if (!omnibusMethod.empty()) {
        const std::string pooledNote = GeneralizedGlobalTermTestPooledWaldFootnote(state);
        for (std::string &note : terms.footnotes)
            if (note == omnibusMethod) note = pooledNote.empty() ? "† " + omnibusMethod : pooledNote;
        auto statistic = std::find_if(terms.columns.begin(), terms.columns.end(),
            [](const PublicationTableColumn &column) { return column.key == "statistic"; });
        if (statistic != terms.columns.end()) {
            const std::size_t j = static_cast<std::size_t>(statistic - terms.columns.begin());
            statistic->decimals = -1;
            for (std::size_t i = 0; i < terms.rows.size() && i < state.rows.size(); ++i) {
                const auto &cell = terms.rows[i][j];
                if (cell.kind != PublicationValueKind::Number || !std::isfinite(cell.number)) continue;
                const bool global = GeneralizedGlobalTermTestForPresentationRow(state, state.rows[i]) != nullptr;
                terms.rows[i][j] = PublicationText(FormatDouble(cell.number, 3) + (global ? "†" : ""));
            }
        }
    }
    // One effect and interval column keeps the compact report readable.
    if (GeneralizedModelHasExponentiatedEffect(state)) {
        for (std::size_t j = terms.columns.size(); j-- > 0;) {
            if (terms.columns[j].key != "confidence_interval") continue;
            terms.columns.erase(terms.columns.begin()+j);
            for (auto &row : terms.rows) row.erase(row.begin()+j);
        }
        const std::size_t effect = terms.columns.size()-2;
        terms.columns[effect] = {"effect_interval", GeneralizedExponentiatedEffectLabel(state) + " / 95% CI", "", -1, false};
        terms.columns.pop_back();
        for (std::size_t i=0;i<terms.rows.size();++i) {
            auto &row=terms.rows[i];
            const auto &source=state.rows[i];
            const bool coefficient=source.rowType != "factor_parent" && source.rowType != "term_parent" && source.rowType != "reference";
            row[effect]=PublicationText(coefficient && std::isfinite(source.exponentiatedEstimate)
                ? FormatDouble(source.exponentiatedEstimate,3) + " " + row.back().text : "");
            row.pop_back();
        }
    }
    return {fit, terms};
}

void RefreshBasicPlotCodeReference(ApplicationState &state, PlotModel &plot)
{
    if (plot.kind != "scatter" && plot.kind != "time_series" &&
        plot.kind != "histogram" && plot.kind != "barplot" &&
        plot.kind != "boxplot" && plot.kind != "trellis_scatterplot" &&
        plot.kind != "scatter_matrix") return;
    RegisterBasicPlotCodeReference(state, plot);
}

void RefreshCorrelationCodeReference(ApplicationState &state,
                                     CorrelationMatrixState &correlation)
{
    const DataFrameModel *dataframe = state.datasets().find(correlation.group);
    if (!dataframe || correlation.id.empty() || correlation.rFitPending ||
        correlation.provenance.executedRCode.empty()) return;
    state.registerOutputCodeReference(CorrelationCodeReference(correlation, *dataframe));
}

// Include the actual cases and data version, not just the label "selected".
static std::string DimensionalityRequestIdentity(const ApplicationState &application,
                                               const DimensionalityState &state)
{
    const auto *data = application.datasets().find(state.group);
    const auto scope = application.activeAnalysisScope(state.group);
    const int imputation = data && (data->datasetType == "multiple_imputation" || data->imputationCount > 1)
        ? std::clamp(state.requestedImputation > 0 ? state.requestedImputation : data->activeImputationVersion,
                     1, std::max(1, data->imputationCount)) : 1;
    std::ostringstream out;
    out << DimensionalityFitSignature(state.method, state.missingMode, state.rotation, state.extraction,
        scope.kind == AnalysisScopeKind::AllObservations ? "all" : "selected",
        state.scale, state.componentCount, imputation, state.variables);
    out << "|version=" << (data ? data->dataVersion : 0) << "|n=" << (data ? data->rows : 0) << "|rows=";
    if (scope.kind == AnalysisScopeKind::ExplicitRowIds)
        for (int row : scope.originalRowIds) out << row << ',';
    out << "|scope=" << scope.sourceDescription.size() << ':' << scope.sourceDescription;
    return out.str();
}

static void ClearDimensionalityResult(ApplicationState &application, DimensionalityState &state)
{
    state.components.clear(); state.loadings.clear(); state.scores.clear();
    state.rowsUsed.clear(); state.rowsExcluded.clear(); state.calculationMethod.clear();
    state.provenance = AnalysisProvenance{};
    application.eraseOutputCodeReference(state.id);
}

std::optional<MainRDimensionalityTask> PrepareDimensionalityRTask(
    ApplicationState &application, DimensionalityState &state)
{
    application.captureAnalysisScope(state.group, state.dataScope, state.dataScopeCaptured, &state.scope);
    const auto *data = application.datasets().find(state.group);
    if (data) {
        state.eligibleVariables = EligibleDimensionalityVariables(*data);
        state.multipleImputation = data->datasetType == "multiple_imputation" || data->imputationCount > 1;
        state.imputationCount = state.multipleImputation ? std::max(1, data->imputationCount) : 1;
        state.displayedImputation = state.multipleImputation
            ? std::clamp(state.requestedImputation > 0 ? state.requestedImputation : data->activeImputationVersion,
                         1, state.imputationCount) : 1;
    }
    const auto identity = DimensionalityRequestIdentity(application, state);
    if (!data || state.variables.size() < 2 || !state.autoFit) {
        if (!data || state.variables.size() < 2 || state.rFitPending || state.lastRFitSignature != identity) {
            ClearDimensionalityResult(application, state);
            ++state.requestRevision; ++state.modelVersion;
            state.rFitPending = false; state.lastRFitSignature.clear();
            state.status = !data ? "The dataset is unavailable." : state.variables.size() < 2
                ? "Choose at least two numeric, ordinal, or binary variables."
                : "Auto-fit is off. Turn it on to fit the current specification.";
        }
        return std::nullopt;
    }
    // Keep terminal errors stable too: refreshing the window must not restart a failed fit.
    if (state.lastRFitSignature == identity) return std::nullopt;
    ClearDimensionalityResult(application, state);
    ++state.requestRevision; ++state.modelVersion;
    state.sourceDataVersion = data->dataVersion;
    state.lastRFitSignature = identity; state.rFitPending = true;
    state.status = state.method == "factor" ? "Fitting factor analysis in the main R session..."
                                          : "Fitting principal components in the main R session...";
    MainRDimensionalityTask task;
    task.id=state.id; task.group=state.group; task.revision=state.requestRevision;
    task.dataVersion=state.sourceDataVersion; task.displayedImputation=state.displayedImputation;
    task.method=state.method; task.missingMode=state.missingMode; task.rotation=state.rotation;
    task.extraction=state.extraction;
    task.scope=state.scope; task.scale=state.scale; task.componentCount=state.componentCount;
    task.variables=state.variables;
    if (state.dataScope.kind == AnalysisScopeKind::ExplicitRowIds) task.rows=state.dataScope.originalRowIds;
    return task;
}

MainRCorrelationTask PrepareCorrelationRTask(ApplicationState &application, CorrelationMatrixState &state)
{
    application.captureAnalysisScope(state.group, state.dataScope, state.dataScopeCaptured);
    state.cells.clear(); state.provenance = AnalysisProvenance{};
    application.eraseOutputCodeReference(state.id);
    ++state.requestRevision; ++state.modelVersion;
    const auto *data = application.datasets().find(state.group);
    state.sourceDataVersion = data ? data->dataVersion : 0;
    state.multipleImputation = data && data->datasetType == "multiple_imputation";
    state.imputationCount = state.multipleImputation ? data->imputationCount : 0;
    state.precomputed = false;
    state.rFitPending = state.variables.size() >= 2;
    state.note = state.rFitPending ? "Calculating correlations in R..." : "Add at least two numeric variables.";
    MainRCorrelationTask task;
    task.id=state.id; task.group=state.group; task.revision=state.requestRevision;
    task.dataVersion=state.sourceDataVersion; task.method=state.method;
    task.missingMode=state.missingMode; task.variables=state.variables;
    if (state.dataScopeCaptured && state.dataScope.kind == AnalysisScopeKind::ExplicitRowIds) {
        task.scope="selected"; task.rows=state.dataScope.originalRowIds;
    }
    return task;
}

MainRDendrogramTask PrepareDendrogramRTask(ApplicationState &application, DendrogramState &state)
{
    application.captureAnalysisScope(state.group, state.dataScope, state.dataScopeCaptured);
    // Keep the previous tree visible while R recalculates after a data edit.
    // It is explicitly marked as updating, and is cleared if the new fit fails.
    const bool hadTree = !state.caseRows.empty() && !state.merges.empty();
    if (state.variables.empty()) {
        state.caseRows.clear(); state.leafOrder.clear(); state.merges.clear();
    }
    state.provenance = AnalysisProvenance{};
    application.eraseOutputCodeReference(state.id);
    ++state.requestRevision;
    ++state.modelVersion;
    const auto *data = application.datasets().find(state.group);
    state.sourceDataVersion = data ? data->dataVersion : 0;
    state.displayedImputation = data ? std::max(1, data->activeImputationVersion) : 1;
    state.rFitPending = !state.variables.empty();
    state.status = state.rFitPending
        ? (hadTree ? "Recalculating clustering in R; previous tree shown until ready..."
                   : "Calculating clustering in R...")
        : DendrogramAddVariablesStatus();
    MainRDendrogramTask task;
    task.id = state.id; task.group = state.group; task.revision = state.requestRevision;
    task.dataVersion = state.sourceDataVersion; task.imputation = state.displayedImputation;
    task.distance = state.distance; task.linkage = state.linkage; task.missingMode = state.missingMode;
    task.variables = state.variables;
    if (state.dataScopeCaptured && state.dataScope.kind == AnalysisScopeKind::ExplicitRowIds) {
        task.scope = "selected"; task.rows = state.dataScope.originalRowIds;
    }
    return task;
}

void RefreshDendrogramCodeReference(ApplicationState &state,
                                    DendrogramState &dendrogram)
{
    const DataFrameModel *dataframe = state.datasets().find(dendrogram.group);
    if (!dataframe || dendrogram.id.empty() || dendrogram.variables.empty() ||
        dendrogram.rFitPending || dendrogram.provenance.executedRCode.empty()) return;
    state.registerOutputCodeReference(DendrogramCodeReference(dendrogram, *dataframe));
}

void RefreshNativeTableCodeReference(ApplicationState &state,
                                     Table1DisplayState &table)
{
    if (!table.codeReference.outputId.empty()) {
        // Derived R results reuse the source model's immutable scope.
        if(table.codeReference.outputBlockId=="pairwise") {
            for(const auto& source:state.outputCodeReferencesForDataset(table.datasetId)) {
                if(source.outputId!=table.id && source.outputBlockId!="pairwise" &&
                   source.provenance.analysisId==table.codeReference.provenance.analysisId &&
                   source.provenance.dataVersion.version==table.codeReference.provenance.dataVersion.version) {
                    table.codeReference.provenance.scope=source.provenance.scope;break;
                }
            }
        }
        state.registerOutputCodeReference(table.codeReference);
    }
    // Pending contingency tables have no calculated R result to export yet.
    // Completed tables register the immutable R reference above.

}

static Table1DisplayState BuildMissingInformationTable(const OutputCodeReference &reference,
                                                       const ApplicationState &application)
{
    const auto *output = &reference;
    Table1DisplayState table;
    table.id=output->outputId+":missing-information"; table.datasetId=output->provenance.dataVersion.datasetId;
    table.title="Missing-information diagnostics — "+output->title;table.tableType="mi_missing_information";
    const auto& columns=output->provenance.missingInformationColumns;
    const bool omnibus = std::find(columns.begin(),columns.end(),"F")!=columns.end() &&
        std::find(columns.begin(),columns.end(),"df1")!=columns.end() &&
        std::find(columns.begin(),columns.end(),"df2")!=columns.end();
    if (!columns.empty()) table.columns.assign(columns.begin()+1,columns.end());
    auto appendDiagnosticRow = [&](const std::vector<std::string>& values,
                                   const MissingInformationDisplayRow* display) {
        if (values.size()!=columns.size()) return;
        Table1DisplayRow row; row.rowIndex=static_cast<int>(table.rows.size()+1);row.rowType="text";
        row.label=values[0];row.variable=values[0];row.values.assign(values.begin()+1,values.end());
        row.rawValues=row.values;
        for (size_t i=0;i<row.values.size();++i) {
            const double value=ParseOptionalDataCellDouble(row.values[i]);
            if (!std::isfinite(value)) continue;
            row.values[i]=columns[i+1]=="p" ? FormatPValue(value)
                : (columns[i+1]=="m" || columns[i+1]=="df1") ? FormatDouble(value,0)
                : (columns[i+1]=="df" || columns[i+1]=="df1" || columns[i+1]=="df2" || columns[i+1]=="MCSE / SE (%)") ? FormatDouble(value,1)
                : FormatModelNumberOrDash(value);
        }
        if (display) {
            row.label=display->label; row.variable=display->variable;
            row.rowType=display->rowType=="factor_parent" || display->rowType=="term_parent" || display->rowType=="section_header"
                ? "categorical_parent" : display->rowType=="reference" || display->rowType=="factor_level" ||
                    display->rowType=="summary_mean" || display->rowType=="group_comparison"
                ? "categorical_level" : "text";
        }
        row.detail=omnibus ? "Overall group effect pooled across imputations. RIV describes the relative increase in variance of the joint test." :
            "FMI: fraction of uncertainty related to missing information. RIV: relative increase in variance. No good/bad thresholds are applied.";
        table.rows.push_back(std::move(row));
    };
    if (output->provenance.missingInformationDisplayRows.empty()) {
        for (const auto& values:output->provenance.missingInformationRows) appendDiagnosticRow(values,nullptr);
    } else {
        for (const auto& display:output->provenance.missingInformationDisplayRows) {
            if (display.valueRow>=0 && static_cast<size_t>(display.valueRow)<output->provenance.missingInformationRows.size())
                appendDiagnosticRow(output->provenance.missingInformationRows[display.valueRow],&display);
            else {
                std::vector<std::string> empty(columns.size(),display.rowType=="reference" ? "—" : "");
                appendDiagnosticRow(empty,&display);
            }
        }
    }
    table.footnotes={
        "Estimates and uncertainty are reported on the pooling scale (for example, log odds or Fisher z).",
        "Right-click a statistic or its column heading and choose Explain statistic for interpretation and practical guidance."};
    if (omnibus) table.footnotes[0] =
        "Rows test the overall effect of the grouping variable. These pooled tests do not provide a scalar estimate, SE, confidence interval or FMI; RIV refers to the joint test.";
    if(output->outputBlockId=="pairwise")table.footnotes.push_back(
        "Diagnostics use unadjusted scalar degrees of freedom and p-values. The original comparison table retains its multiplicity adjustment.");
    if(std::any_of(output->provenance.missingInformationDisplayRows.begin(),
                   output->provenance.missingInformationDisplayRows.end(),
                   [](const auto& row){return row.rowType=="summary_mean";}))
        table.footnotes.insert(table.footnotes.begin(),
            "Mean rows are descriptive and have no p-value. Difference rows give the unadjusted contrast p-value from the source table; omnibus tests have no single estimate or scalar FMI.");
    table.dataScope = AllObservationsAnalysisScope(table.datasetId, output->provenance.scope.sourceN);
    table.dataScopeCaptured = true;
    if (output->provenance.scope.kind != "all") {
        std::vector<int> rows;
        if (const auto *data = application.datasetVersion(output->provenance.dataVersion)) {
            for (const auto &id : output->provenance.scope.requestedStableRowIds) {
                const auto found = std::find(data->stableRowIds.begin(),data->stableRowIds.end(),id);
                if (found != data->stableRowIds.end()) rows.push_back(static_cast<int>(found-data->stableRowIds.begin())+1);
            }
        }
        table.dataScope = ExplicitAnalysisScope(table.datasetId, rows, AnalysisScopeSourceKind::OtherExplicitSubset,
            output->provenance.scope.description, output->provenance.scope.sourceN);
    }
    if (table.rows.empty()) {
        table.columns = {"Status"};
        Table1DisplayRow row; row.rowType="text"; row.label="Missing information";
        row.values={"Unavailable for the current model result."}; table.rows.push_back(row);
    }
    table.codeReference=Table1CodeReference(table, output->provenance);
    table.codeReference.outputBlockId=output->provenance.verificationRCode.count("missing_information")
        ? "missing_information" : output->outputBlockId;
    table.statusText.clear();
    // Diagnostic tables follow their source output; model editing belongs in
    // the model window. Leave edit controls unavailable on both platforms.
    return table;
}

CommandDispatcher::CommandDispatcher(CommandDispatcherServices services)
    : services_(std::move(services)),
      session_(services_.session),
      plotCoordinator_(applicationState_)
{
    applicationState_.outputCodeReferenceChanged = [this](const OutputCodeReference &output) {
        if (!linkedMissingInformationSources_.count(output.outputId) || !missingInformationRefresh_) return;
        auto table=BuildMissingInformationTable(output,applicationState_);
        applicationState_.registerOutputCodeReference(table.codeReference);
        missingInformationRefresh_(table);
    };
}

std::string CommandDispatcher::dispatchSession(SessionReadLine readLine)
{
    return session_.handle(std::move(readLine));
}

bool CommandDispatcher::sessionClosed() const
{
    return session_.isClosed();
}

const std::string &CommandDispatcher::plotTheme() const
{
    return plotTheme_;
}

ApplicationState &CommandDispatcher::applicationState()
{
    return applicationState_;
}

const ApplicationState &CommandDispatcher::applicationState() const
{
    return applicationState_;
}

PlotCoordinator &CommandDispatcher::plotCoordinator()
{
    return plotCoordinator_;
}

const PlotCoordinator &CommandDispatcher::plotCoordinator() const
{
    return plotCoordinator_;
}

void CommandDispatcher::prepareTable1Task(MainRTable1Task &task)
{
    task.variables.erase(std::remove(task.variables.begin(), task.variables.end(), task.groupVariable), task.variables.end());
    std::vector<std::string> unique;
    for (const auto &variable : task.variables)
        if (std::find(unique.begin(), unique.end(), variable) == unique.end()) unique.push_back(variable);
    task.variables = std::move(unique);
    std::sort(task.rows.begin(), task.rows.end());
    task.rows.erase(std::unique(task.rows.begin(), task.rows.end()), task.rows.end());
    if (task.scope != "selected") task.rows.clear();
    task.revision = ++table1Revision_;
    if (const auto *df = applicationState_.datasets().find(task.group)) {
        task.dataVersion = df->dataVersion;
        for (const auto &column : df->columns) {
            if (column.name != task.groupVariable && std::find(task.variables.begin(), task.variables.end(), column.name) == task.variables.end()) continue;
            const auto type = NormalizeVariableType(column.type);
            task.variableTypes[column.name] = type == "numeric" ? "numeric"
                : type == "ordered" ? "ordinal" : "categorical";
        }
    }
    const auto active = applicationState_.activeAnalysisScope(task.group);
    const bool selected = active.kind == AnalysisScopeKind::ExplicitRowIds;
    const bool follows = (task.scope == "selected") == selected &&
        (!selected || active.originalRowIds == task.rows) && active.sourceDescription == task.scopeDescription;
    table1Requests_[task.id] = {task, true, follows};
    applicationState_.eraseOutputCodeReference(task.id);
}

void CommandDispatcher::cancelTable1Task(const std::string &id)
{
    const auto found = table1Requests_.find(id);
    if (found != table1Requests_.end()) found->second.pending = false;
}

bool CommandDispatcher::currentTable1Reply(const std::string &id, const std::string &group,
    std::uint64_t revision, std::uint64_t version) const
{
    const auto found = table1Requests_.find(id);
    const auto *df = applicationState_.datasets().find(group);
    if (found == table1Requests_.end() || !found->second.pending || !df) return false;
    const auto &task = found->second.task;
    if (found->second.followsActiveScope) {
        const auto active = applicationState_.activeAnalysisScope(group);
        const bool selected = active.kind == AnalysisScopeKind::ExplicitRowIds;
        if ((task.scope == "selected") != selected || (selected && active.originalRowIds != task.rows) ||
            active.sourceDescription != task.scopeDescription) return false;
    }
    return task.group == group && task.revision == revision && task.dataVersion == version && df->dataVersion == version;
}

std::string CommandDispatcher::dispatch(const std::vector<std::string> &lines)
{
    CommandRequest request = ParseCommandRequest(lines);
    if (request.name.empty()) {
        return "ERR empty command";
    }
    if (request.name == "TABLE1_OPEN_ERROR" && request.args.size() >= 6 && request.args[2] == "REQUEST_V1") {
        const auto &a = request.args;
        const auto revision = std::strtoull(a[3].c_str(), nullptr, 10);
        const auto version = std::strtoull(a[4].c_str(), nullptr, 10);
        if (!currentTable1Reply(a[0], a[1], revision, version)) return "OK ignored stale Table 1 error";
        auto &pending = table1Requests_.at(a[0]);
        const auto &task = pending.task;
        const auto *df = applicationState_.datasets().find(task.group);
        auto scope = task.scope == "selected" ? ExplicitAnalysisScope(task.group, task.rows,
            AnalysisScopeSourceKind::OtherExplicitSubset, task.scopeDescription, df->rows)
            : AllObservationsAnalysisScope(task.group, df->rows);
        scope.sourceDescription = task.scopeDescription;
        auto state = Table1PendingStateForDataFrame(*df, task.id, task.variables, task.groupVariable, task.variableTypes, &scope);
        if (task.analysisKind == "nested_contingency") state = NestedContingencyTableStateForDataFrame(
            *df, task.id, task.variables, task.groupVariable, task.contingencyDisplayMode, &scope);
        state.needsRFit = false; state.statusText = a[5];
        if (task.analysisKind == "frequency" || task.analysisKind == "nested_contingency") state.tableType = task.analysisKind;
        state.nestedDisplayMode = task.contingencyDisplayMode;
        state.sourcePlotId = task.sourcePlotId; state.linkEnabled = task.linkEnabled;
        if (!task.title.empty()) state.title = task.title;
        pending.pending = false;
        if (services_.ui.showTable1) services_.ui.showTable1(state);
        return "OK";
    }
    if (auto reply = dispatchPortable(request)) {
        return *reply;
    }
    if (!services_.handle) {
        return "ERR unknown command";
    }
    return services_.handle(request, lines);
}

std::optional<std::string> CommandDispatcher::dispatchPortable(const CommandRequest &request)
{
    switch (request.action) {
    case CommandAction::Ping:
        return "OK";
    case CommandAction::ListPlots: {
        if (!services_.queries.listPlots) return std::nullopt;
        std::string reply = "OK";
        for (const PlotModel &plot : services_.queries.listPlots()) {
            reply += "\t" + PlotListItemText(plot);
        }
        return reply;
    }
    case CommandAction::PlotInfo: {
        if (request.args.empty()) return "ERR missing plot id";
        if (!services_.queries.plotWithSelection) return std::nullopt;
        PlotModel plot;
        std::size_t selected = 0;
        if (!services_.queries.plotWithSelection(request.args[0], plot, selected)) {
            return "ERR no active plot";
        }
        return "OK\t" + PlotInfoResponseText(plot, selected);
    }
    case CommandAction::DiagnosticInfo: {
        if (request.args.empty()) return "ERR missing plot id";
        if (!services_.queries.plot) return std::nullopt;
        PlotModel plot;
        if (!services_.queries.plot(request.args[0], plot) || !plot.isGLMDiagnostic) {
            return "ERR no diagnostic plot";
        }
        return PlotDiagnosticInfoResponseText(plot);
    }
    case CommandAction::Groups:
        if (!services_.queries.groups) return std::nullopt;
        return GroupSelectionsResponseText(services_.queries.groups());
    case CommandAction::GroupInfo: {
        if (request.args.empty()) return "ERR missing group name";
        if (!services_.queries.groupInfo) return std::nullopt;
        std::size_t selected = 0;
        std::vector<std::string> plotIds;
        if (!services_.queries.groupInfo(request.args[0], selected, plotIds)) {
            return "ERR no active plot/group";
        }
        return GroupInfoResponseText(request.args[0], selected, plotIds);
    }
    case CommandAction::DatasetSyncStatus: {
        if (request.args.empty()) return "ERR missing group name";
        const DataFrameModel *data = applicationState_.datasets().find(request.args[0]);
        if (!data) return "OK\tmissing";
        return "OK\tpresent\t" + std::to_string(data->dataVersion) + "\t" +
            std::to_string(data->rows) + "\t" + std::to_string(data->columns.size()) +
            "\t" + data->datasetType + "\t" + data->imputationId + "\t" +
            std::to_string(data->imputationCount) + "\t" +
            std::to_string(data->activeImputationVersion) + "\t" +
            data->imputationDisplayMode + "\t" + data->sourceDatasetId + "\t" +
            data->syncSessionToken + "\tEND";
    }
    case CommandAction::Variables: {
        if (request.args.empty()) return "ERR missing plot id";
        if (!services_.queries.plot) return std::nullopt;
        PlotModel plot;
        if (!services_.queries.plot(request.args[0], plot)) return "ERR no active plot";
        return PlotVariablesResponseText(plot);
    }
    case CommandAction::GetXVariable:
    case CommandAction::GetYVariable: {
        if (request.args.empty()) return "ERR missing plot id";
        if (!services_.queries.plot) return std::nullopt;
        PlotModel plot;
        if (!services_.queries.plot(request.args[0], plot)) return "ERR no active plot";
        return std::string("OK ") +
            (request.action == CommandAction::GetXVariable ? plot.xLabel : plot.yLabel);
    }
    case CommandAction::PointColors: {
        if (request.args.empty()) return "ERR missing plot id";
        if (!services_.queries.pointColors) return std::nullopt;
        std::vector<std::pair<int, std::string>> colors;
        if (!services_.queries.pointColors(request.args[0], colors)) return "ERR no active plot";
        std::string reply = "OK";
        for (const auto &color : colors) {
            reply += "\t" + std::to_string(color.first) + "|" + color.second;
        }
        return reply;
    }
    case CommandAction::ModelInfo: {
        if (request.args.empty()) return "ERR missing group name";
        if (!services_.queries.groupPlot) return std::nullopt;
        PlotModel seed;
        if (!services_.queries.groupPlot(request.args[0], seed)) {
            return "ERR no active plot/group";
        }
        GroupModelState &state = EnsureGroupModelState(
            applicationState_.groupModels(), request.args[0], &seed);
        std::ostringstream out;
        out << "OK\t" << state.group << "|" << state.response << "|" << state.scope;
        for (const std::string &term : state.terms) out << "|" << term;
        return out.str();
    }
    case CommandAction::CorrelationOpen: {
        if (request.args.size() < 8) return "ERR malformed CORR_OPEN command";
        if (!services_.queries.groupSeed || !services_.selection.refitCorrelation) {
            return std::nullopt;
        }
        CorrelationMatrixState state;
        state.id = request.args[0];
        state.group = request.args[1];
        state.multipleImputation = services_.queries.groupIsMultipleImputation &&
            services_.queries.groupIsMultipleImputation(state.group);
        state.title = state.multipleImputation ? "Pearson Correlation Matrix - Multiple Imputation"
                                                : "Pearson Correlation Matrix";
        state.method = request.args[2];
        state.missingMode = request.args[3];
        state.showP = request.args[4] == "TRUE";
        state.showPValue = request.args[5] == "TRUE";
        state.showN = request.args[6] == "TRUE";
        if (state.method != "pearson") return "ERR only Pearson correlations are supported";
        if (state.missingMode != "pairwise" && state.missingMode != "listwise") {
            return "ERR invalid missing-data mode";
        }
        if (!services_.queries.groupSeed(state.group, state.seed)) {
            return "ERR no registered dataset/group for correlation matrix";
        }
        state.hasSeed = true;
        const long count = std::strtol(request.args[7].c_str(), nullptr, 10);
        if (count < 0 || request.args.size() < static_cast<std::size_t>(8 + count)) {
            return "ERR invalid correlation variable count";
        }
        for (long index = 0; index < count; ++index) {
            const std::string &variable = request.args[static_cast<std::size_t>(8 + index)];
            if (!FindNumericVariable(state.seed, variable)) {
                return "ERR correlation variables must be numeric";
            }
            if (std::find(state.variables.begin(), state.variables.end(), variable) == state.variables.end()) {
                state.variables.push_back(variable);
            }
        }
        const auto previous=applicationState_.correlationMatrices().find(state.id);
        if (previous!=applicationState_.correlationMatrices().end())
            state.requestRevision=previous->second.requestRevision;
        applicationState_.correlationMatrices()[state.id] = state;
        return services_.selection.refitCorrelation(state.id)
            ? "OK\t" + state.id : "ERR no registered dataset/group for correlation matrix";
    }
    case CommandAction::CorrelationUpdate:
    case CommandAction::CorrelationOpenStructured: {
        const bool update = request.action == CommandAction::CorrelationUpdate;
        CorrelationMatrixState *pending = nullptr;
        auto args = request.args;
        if (update) {
            if (args.size() < 5) return "ERR malformed CORR_UPDATE command";
            auto it = applicationState_.correlationMatrices().find(args[0]);
            if (it == applicationState_.correlationMatrices().end()) return "ERR unknown correlation matrix";
            pending = &it->second;
            const auto *data = applicationState_.datasets().find(pending->group);
            if (!pending->rFitPending || args[1] != std::to_string(pending->requestRevision) ||
                args[2] != std::to_string(pending->sourceDataVersion) || !data ||
                data->dataVersion != pending->sourceDataVersion) return "OK\tignored stale correlation result";
            if (args[3] == "error") {
                pending->rFitPending=false; pending->note=args[4]; pending->cells.clear();
                pending->provenance=AnalysisProvenance{}; ++pending->modelVersion;
                applicationState_.eraseOutputCodeReference(pending->id);
                if (services_.ui.showCorrelationMatrix) services_.ui.showCorrelationMatrix(pending->id);
                return "OK\t" + pending->id;
            }
            if (args[3] != "ok") return "ERR invalid correlation response status";
            args.erase(args.begin(), args.begin()+4);
        }
        if (args.size() < 13) return "ERR malformed CORR_OPEN_STRUCTURED command";
        if (!services_.queries.groupSeed || !services_.ui.showCorrelationMatrix) {
            return std::nullopt;
        }
        CorrelationMatrixState state;
        std::size_t cursor = 0;
        state.id = args[cursor++];
        state.group = args[cursor++];
        state.title = args[cursor++];
        state.method = args[cursor++];
        state.missingMode = args[cursor++];
        state.showP = args[cursor++] == "TRUE";
        state.showPValue = args[cursor++] == "TRUE";
        state.showN = args[cursor++] == "TRUE";
        state.imputationCount = std::atoi(args[cursor++].c_str());
        state.poolingMethod = args[cursor++];
        state.note = args[cursor++];
        state.precomputed = true;
        state.multipleImputation = state.imputationCount > 0;
        if (state.method != "pearson") return "ERR only Pearson correlations are supported";
        if (state.missingMode != "pairwise" && state.missingMode != "listwise") {
            return "ERR invalid missing-data mode";
        }
        if (!services_.queries.groupSeed(state.group, state.seed)) {
            return "ERR no registered dataset/group for correlation matrix";
        }
        state.hasSeed = true;
        if (cursor >= args.size()) return "ERR malformed structured correlation variable payload";
        const long variableCount = std::strtol(args[cursor++].c_str(), nullptr, 10);
        if (variableCount < 0 || cursor + static_cast<std::size_t>(variableCount) > args.size()) {
            return "ERR invalid structured correlation variable count";
        }
        for (long index = 0; index < variableCount; ++index) {
            const std::string &variable = args[cursor++];
            if (!FindNumericVariable(state.seed, variable)) {
                return "ERR correlation variables must be numeric";
            }
            if (std::find(state.variables.begin(), state.variables.end(), variable) == state.variables.end()) {
                state.variables.push_back(variable);
            }
        }
        if (cursor >= args.size()) return "ERR malformed structured correlation cell payload";
        const long cellCount = std::strtol(args[cursor++].c_str(), nullptr, 10);
        if (cellCount < 0) return "ERR invalid structured correlation cell count";
        for (long index = 0; index < cellCount; ++index) {
            if (cursor + 8 > args.size()) return "ERR malformed structured correlation cell";
            CorrelationCellResult cell;
            cell.xVariable = args[cursor++];
            cell.yVariable = args[cursor++];
            cell.r = ParseOptionalDataCellDouble(args[cursor++]);
            cell.p = ParseOptionalDataCellDouble(args[cursor++]);
            cell.n = std::atoi(args[cursor++].c_str());
            cell.status = args[cursor++];
            cell.detail = args[cursor++];
            const long rowCount = std::strtol(args[cursor++].c_str(), nullptr, 10);
            if (rowCount < 0 || cursor + static_cast<std::size_t>(rowCount) > args.size()) {
                return "ERR invalid structured correlation row count";
            }
            for (long row = 0; row < rowCount; ++row) cell.rowsUsed.push_back(std::atoi(args[cursor++].c_str()));
            state.cells.push_back(std::move(cell));
        }
        if (cursor < args.size() && args[cursor] == "CORR_SCOPE_V1") {
            if(cursor+5 > args.size()) return "ERR malformed correlation scope";
            ++cursor;
            const auto kind=args[cursor++]; const auto description=args[cursor++];
            const int total=std::atoi(args[cursor++].c_str());
            const int count=std::atoi(args[cursor++].c_str());
            if(count<0 || cursor+count>args.size()) return "ERR invalid correlation scope rows";
            std::vector<int> rows; for(int i=0;i<count;++i) rows.push_back(std::atoi(args[cursor++].c_str()));
            state.dataScope=kind=="all" ? AllObservationsAnalysisScope(state.group,total) :
                ExplicitAnalysisScope(state.group,rows,AnalysisScopeSourceKind::OtherExplicitSubset,description,total);
            state.dataScopeCaptured=true;
        }
        if (cursor < args.size()) {
            std::string error;
            if (!ReadAnalysisProvenancePayload(args,cursor,state.provenance,&error)) return "ERR " + error;
        }
        if (cursor != args.size()) return "ERR unexpected correlation payload";
        if (pending) {
            if (state.id != pending->id || state.group != pending->group ||
                state.variables != pending->variables || state.method != pending->method ||
                state.missingMode != pending->missingMode) return "ERR mismatched correlation specification";
            state.title=pending->title;
            state.dataScope=pending->dataScope; state.dataScopeCaptured=pending->dataScopeCaptured;
            state.displayPart=pending->displayPart;
            state.showP=pending->showP; state.showPValue=pending->showPValue; state.showN=pending->showN;
            state.selectedRow=pending->selectedRow; state.selectedCol=pending->selectedCol;
            state.requestRevision=pending->requestRevision;
            state.sourceDataVersion=pending->sourceDataVersion;
            state.modelVersion=pending->modelVersion;
            if (state.provenance.executedRCode.empty()) return "ERR missing correlation provenance";
        } else {
            const auto previous=applicationState_.correlationMatrices().find(state.id);
            if (previous!=applicationState_.correlationMatrices().end()) {
                state.displayPart=previous->second.displayPart;
                state.requestRevision=previous->second.requestRevision+1;
                state.modelVersion=previous->second.modelVersion;
            }
        }
        state.precomputed=state.provenance.executedRCode.empty(); // Legacy externally supplied matrices are read-only.
        if (!state.precomputed) {
            const auto *data=applicationState_.datasets().find(state.group);
            if (!data) return "ERR missing correlation dataset";
            const auto allowed=ResolveAnalysisScopeRowIds(state.dataScope,state.dataScope.totalDatasetRows);
            std::set<int> scopeRows(allowed.begin(),allowed.end()), used;
            std::set<std::pair<std::string,std::string>> pairs;
            if (state.variables.size()>=2 && state.cells.size()!=state.variables.size()*state.variables.size())
                return "ERR incomplete correlation result";
            for (const auto &cell:state.cells) {
                if (std::find(state.variables.begin(),state.variables.end(),cell.xVariable)==state.variables.end() ||
                    std::find(state.variables.begin(),state.variables.end(),cell.yVariable)==state.variables.end() ||
                    !pairs.emplace(cell.xVariable,cell.yVariable).second || cell.n<0 ||
                    (std::isfinite(cell.r) && std::abs(cell.r)>1) ||
                    (std::isfinite(cell.p) && (cell.p<0 || cell.p>1))) return "ERR invalid correlation cell";
                for (int row:cell.rowsUsed) {
                    if (!scopeRows.count(row)) return "ERR correlation row outside requested scope";
                    used.insert(row);
                }
            }
            std::vector<int> excluded;
            for (int row:allowed) if (!used.count(row)) excluded.push_back(row);
            CompleteAnalysisProvenance(state.provenance,*data,state.dataScope,
                std::vector<int>(used.begin(),used.end()),excluded,state.id,state.title);
        }
        state.modelVersion += 1;
        applicationState_.correlationMatrices()[state.id] = state;
        RefreshCorrelationCodeReference(
            applicationState_, applicationState_.correlationMatrices()[state.id]);
        services_.ui.showCorrelationMatrix(state.id);
        return "OK\t" + state.id;
    }
    case CommandAction::CorrelationSetPart:
    case CommandAction::CorrelationToggleDisplay:
    case CommandAction::CorrelationSetMissing: {
        if (request.args.size()!=2) return "ERR invalid correlation display option";
        auto it=applicationState_.correlationMatrices().find(request.args[0]);
        if (it==applicationState_.correlationMatrices().end()) return "ERR unknown correlation matrix";
        auto &state=it->second;const auto &value=request.args[1];
        if (request.action==CommandAction::CorrelationSetMissing) {
            if (state.precomputed) return "ERR precomputed matrix cannot change missing-data policy";
            if (value!="pairwise" && value!="listwise") return "ERR invalid missing-data mode";
            if (value==state.missingMode) return "OK";
            state.missingMode=value;
            if (services_.selection.refitCorrelation) services_.selection.refitCorrelation(state.id);
            return "OK";
        }
        if (request.action==CommandAction::CorrelationSetPart) {
            if (value!="full" && value!="lower" && value!="upper") return "ERR invalid matrix triangle";
            state.displayPart=value;
            if (!CorrelationCellIsVisible(value,state.selectedRow,state.selectedCol))
                state.selectedRow=state.selectedCol=-1;
        } else {
            if (value=="show_p") state.showP=!state.showP;
            else if (value=="show_p_value") state.showPValue=!state.showPValue;
            else if (value=="show_n") state.showN=!state.showN;
            else return "ERR invalid correlation display option";
        }
        RefreshCorrelationCodeReference(applicationState_,state);
        if (services_.ui.showCorrelationMatrix) services_.ui.showCorrelationMatrix(state.id);
        return "OK";
    }
    case CommandAction::CorrelationInfo: {
        if (request.args.empty()) return "ERR missing correlation id";
        const auto it = applicationState_.correlationMatrices().find(request.args[0]);
        if (it == applicationState_.correlationMatrices().end()) {
            return "ERR unknown correlation matrix";
        }
        const CorrelationMatrixState &state = it->second;
        std::ostringstream out;
        out << "OK\tCORRELATION|" << state.id << "|" << state.group << "|" << state.method << "|"
            << state.missingMode << "|" << state.variables.size() << "|" << state.cells.size();
        for (const std::string &variable : state.variables) out << "|" << variable;
        return out.str();
    }
    case CommandAction::CorrelationSetVariables: {
        if (request.args.size() < 2) return "ERR malformed CORR_SET_VARIABLES command";
        auto it = applicationState_.correlationMatrices().find(request.args[0]);
        if (it == applicationState_.correlationMatrices().end()) return "ERR unknown correlation matrix";
        CorrelationMatrixState &state = it->second;
        if (state.precomputed) {
            return "ERR precomputed correlation matrices must be refit from R to change variables";
        }
        const long count = std::strtol(request.args[1].c_str(), nullptr, 10);
        if (count < 0 || request.args.size() < static_cast<std::size_t>(2 + count)) {
            return "ERR invalid correlation variable count";
        }
        std::vector<std::string> variables;
        for (long index = 0; index < count; ++index) {
            const std::string &variable = request.args[static_cast<std::size_t>(2 + index)];
            if (!FindNumericVariable(state.seed, variable)) {
                return "ERR correlation variables must be numeric";
            }
            if (std::find(variables.begin(), variables.end(), variable) == variables.end()) {
                variables.push_back(variable);
            }
        }
        state.variables = variables;
        state.selectedRow = -1;
        state.selectedCol = -1;
        if (!services_.selection.refitCorrelation) return std::nullopt;
        return services_.selection.refitCorrelation(state.id) ? "OK" : "ERR unknown correlation matrix";
    }
    case CommandAction::DimensionalityOpen: {
        if (request.args.size() < 7) return "ERR malformed PCAFA_OPEN command";
        if (!services_.queries.groupSeed || !services_.selection.refitDimensionality) {
            return std::nullopt;
        }
        DimensionalityState state;
        state.id = request.args[0];
        state.group = request.args[1];
        state.method = request.args[2];
        state.missingMode = request.args[3];
        state.scale = request.args[4] != "FALSE";
        state.componentCount = std::max(1, std::atoi(request.args[5].c_str()));
        std::size_t cursor = 6;
        if (cursor < request.args.size() && DimensionalityRotationIsValid(request.args[cursor])) {
            state.rotation = request.args[cursor++];
        }
        if (cursor < request.args.size() && DimensionalityScopeIsValid(request.args[cursor])) {
            state.scope = request.args[cursor++];
        }
        if (state.method != "pca" && state.method != "factor") {
            return "ERR invalid dimensionality method";
        }
        if (state.missingMode != "listwise" && state.missingMode != "pairwise") {
            return "ERR invalid dimensionality missing-data mode";
        }
        if (!services_.queries.groupSeed(state.group, state.seed)) {
            return "ERR no registered dataset/group for principal components/factor analysis";
        }
        state.hasSeed = true;
        state.eligibleVariables = NumericVariableNames(&state.seed);
        if (const DataFrameModel *dataframe = applicationState_.datasets().find(state.group)) {
            state.eligibleVariables = EligibleDimensionalityVariables(*dataframe);
            state.multipleImputation = dataframe->datasetType == "multiple_imputation" ||
                dataframe->imputationCount > 1;
            state.imputationCount = state.multipleImputation
                ? std::max(1, dataframe->imputationCount) : 1;
            state.displayedImputation = state.multipleImputation
                ? std::max(1, std::min(state.imputationCount,
                                      dataframe->activeImputationVersion)) : 1;
        }
        if (cursor >= request.args.size()) return "ERR invalid dimensionality variable count";
        const long count = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
        if (count < 0 || request.args.size() < cursor + static_cast<std::size_t>(count)) {
            return "ERR invalid dimensionality variable count";
        }
        for (long index = 0; index < count; ++index) {
            const std::string &variable = request.args[cursor + static_cast<std::size_t>(index)];
            if (std::find(state.eligibleVariables.begin(), state.eligibleVariables.end(), variable) == state.eligibleVariables.end()) {
                return "ERR dimensionality requires numeric, ordinal, or binary variables";
            }
            if (std::find(state.variables.begin(), state.variables.end(), variable) == state.variables.end()) {
                state.variables.push_back(variable);
            }
        }
        cursor += static_cast<std::size_t>(count);
        if (cursor + 1 < request.args.size() && request.args[cursor] == "EXTRACTION_V1") {
            const auto extraction = request.args[cursor + 1];
            if (extraction != "minres" && extraction != "ml" && extraction != "pa")
                return "ERR invalid dimensionality extraction";
            state.extraction = extraction;
        }
        const auto previous = applicationState_.dimensionalityModels().find(state.id);
        if (previous != applicationState_.dimensionalityModels().end())
            state.requestRevision = previous->second.requestRevision;
        applicationState_.dimensionalityModels()[state.id] = state;
        return services_.selection.refitDimensionality(state.id)
            ? "OK\t" + state.id
            : "ERR no registered dataset/group for principal components/factor analysis";
    }
    case CommandAction::DimensionalitySetImputation: {
        if (request.args.size() != 2) return "ERR malformed PCAFA_SET_IMPUTATION command";
        auto it = applicationState_.dimensionalityModels().find(request.args[0]);
        if (it == applicationState_.dimensionalityModels().end()) return "ERR unknown dimensionality model";
        auto &state = it->second;
        char *end = nullptr;
        const long imputation = std::strtol(request.args[1].c_str(), &end, 10);
        if (!state.multipleImputation || !end || *end || imputation < 1 || imputation > state.imputationCount)
            return "ERR invalid imputation";
        if (!services_.selection.refitDimensionality) return std::nullopt;
        state.requestedImputation = static_cast<int>(imputation);
        state.displayedImputation = state.requestedImputation;
        return services_.selection.refitDimensionality(state.id) ? "OK" : "ERR dimensionality refit unavailable";
    }
    case CommandAction::DimensionalityUpdate: {
        auto args = request.args;
        if (args.size() < 2) return "ERR malformed PCAFA_UPDATE command";
        const bool identified = args[1] == "REQUEST_V1";
        if (identified) {
            if (args.size() < 6) return "ERR malformed dimensionality response identity";
            auto found = applicationState_.dimensionalityModels().find(args[0]);
            if (found == applicationState_.dimensionalityModels().end()) return "ERR unknown dimensionality model";
            auto &pending = found->second;
            const auto *data = applicationState_.datasets().find(pending.group);
            if (!pending.rFitPending || !pending.autoFit || !data ||
                args[2] != std::to_string(pending.requestRevision) ||
                args[3] != std::to_string(pending.sourceDataVersion) ||
                data->dataVersion != pending.sourceDataVersion ||
                pending.lastRFitSignature != DimensionalityRequestIdentity(applicationState_, pending))
                return "OK\tignored stale dimensionality result";
            if (args[4] == "error") {
                ClearDimensionalityResult(applicationState_, pending);
                pending.rFitPending=false; pending.status=args[5]; ++pending.modelVersion;
                if (services_.ui.showDimensionality) services_.ui.showDimensionality(pending.id);
                if (services_.ui.refreshDimensionalityPlots) services_.ui.refreshDimensionalityPlots(pending.id);
                return "OK\t" + pending.id;
            }
            if (args[4] != "ok") return "ERR invalid dimensionality response status";
            args.erase(args.begin()+1, args.begin()+5);
        } else {
            auto found = applicationState_.dimensionalityModels().find(args[0]);
            if (found != applicationState_.dimensionalityModels().end() && found->second.requestRevision > 0)
                return "OK\tignored unversioned dimensionality result";
        }
        if (args.size() < 11) return "ERR malformed PCAFA_UPDATE command";
        if (!services_.ui.showDimensionality || !services_.ui.refreshDimensionalityPlots) {
            return std::nullopt;
        }
        std::size_t cursor = 0;
        const std::string id = args[cursor++];
        auto it = applicationState_.dimensionalityModels().find(id);
        if (it == applicationState_.dimensionalityModels().end()) {
            return "ERR unknown principal components/factor analysis model";
        }
        DimensionalityState updated = it->second;
        updated.group = args[cursor++];
        updated.method = args[cursor++];
        updated.missingMode = args[cursor++];
        updated.rotation = args[cursor++];
        updated.scope = args[cursor++];
        updated.scale = args[cursor++] != "FALSE";
        updated.componentCount = std::max(1, std::atoi(args[cursor++].c_str()));
        updated.status = args[cursor++];
        const long variableCount = std::strtol(args[cursor++].c_str(), nullptr, 10);
        if (variableCount < 0 || cursor + static_cast<std::size_t>(variableCount) > args.size()) {
            return "ERR invalid PCAFA_UPDATE variable payload";
        }
        updated.variables.assign(args.begin() + static_cast<std::ptrdiff_t>(cursor),
                                 args.begin() + static_cast<std::ptrdiff_t>(cursor + variableCount));
        cursor += static_cast<std::size_t>(variableCount);
        auto readInts = [&](std::vector<int> &values) {
            if (cursor >= args.size()) return false;
            const long count = std::strtol(args[cursor++].c_str(), nullptr, 10);
            if (count < 0 || cursor + static_cast<std::size_t>(count) > args.size()) return false;
            values.clear();
            for (long index = 0; index < count; ++index) values.push_back(std::atoi(args[cursor++].c_str()));
            return true;
        };
        if (!readInts(updated.rowsUsed) || !readInts(updated.rowsExcluded)) {
            return "ERR malformed PCAFA_UPDATE row payload";
        }
        if (cursor >= args.size()) return "ERR missing PCAFA_UPDATE component count";
        const long componentCount = std::strtol(args[cursor++].c_str(), nullptr, 10);
        if (componentCount < 0 || cursor + static_cast<std::size_t>(componentCount) * 5 > args.size()) {
            return "ERR malformed PCAFA_UPDATE component payload";
        }
        updated.components.clear();
        for (long index = 0; index < componentCount; ++index) {
            DimensionalityFitComponent component;
            component.index = std::atoi(args[cursor++].c_str());
            component.eigenvalue = ParseOptionalDataCellDouble(args[cursor++]);
            component.parallelEigenvalue = ParseOptionalDataCellDouble(args[cursor++]);
            component.variance = ParseOptionalDataCellDouble(args[cursor++]);
            component.cumulative = ParseOptionalDataCellDouble(args[cursor++]);
            updated.components.push_back(component);
        }
        if (cursor >= args.size()) return "ERR missing PCAFA_UPDATE loading count";
        const long loadingCount = std::strtol(args[cursor++].c_str(), nullptr, 10);
        if (loadingCount < 0) return "ERR invalid PCAFA_UPDATE loading count";
        updated.loadings.clear();
        for (long index = 0; index < loadingCount; ++index) {
            if (cursor + 4 > args.size()) return "ERR malformed PCAFA_UPDATE loading payload";
            DimensionalityFitLoading loading;
            loading.variable = args[cursor++];
            loading.communality = ParseOptionalDataCellDouble(args[cursor++]);
            loading.uniqueness = ParseOptionalDataCellDouble(args[cursor++]);
            const long valueCount = std::strtol(args[cursor++].c_str(), nullptr, 10);
            if (valueCount < 0 || cursor + static_cast<std::size_t>(valueCount) > args.size()) {
                return "ERR invalid PCAFA_UPDATE loading values";
            }
            for (long value = 0; value < valueCount; ++value) {
                loading.values.push_back(ParseOptionalDataCellDouble(args[cursor++]));
            }
            updated.loadings.push_back(std::move(loading));
        }
        if (cursor >= args.size()) return "ERR missing PCAFA_UPDATE score count";
        const long scoreCount = std::strtol(args[cursor++].c_str(), nullptr, 10);
        if (scoreCount < 0) return "ERR invalid PCAFA_UPDATE score count";
        updated.scores.clear();
        for (long index = 0; index < scoreCount; ++index) {
            if (cursor + 2 > args.size()) return "ERR malformed PCAFA_UPDATE score payload";
            DimensionalityFitScore score;
            score.row = std::atoi(args[cursor++].c_str());
            const long valueCount = std::strtol(args[cursor++].c_str(), nullptr, 10);
            if (valueCount < 0 || cursor + static_cast<std::size_t>(valueCount) > args.size()) {
                return "ERR invalid PCAFA_UPDATE score values";
            }
            for (long value = 0; value < valueCount; ++value) {
                score.values.push_back(ParseOptionalDataCellDouble(args[cursor++]));
            }
            if (!score.values.empty()) score.x = score.values[0];
            score.y = score.values.size() >= 2 ? score.values[1] : 0.0;
            updated.scores.push_back(std::move(score));
        }
        if (cursor < args.size() && args[cursor] == "IMPUTATION_V1") {
            if (cursor + 2 >= args.size()) return "ERR malformed dimensionality imputation";
            ++cursor;
            updated.displayedImputation = std::atoi(args[cursor++].c_str());
            updated.imputationCount = std::max(1, std::atoi(args[cursor++].c_str()));
            updated.multipleImputation = updated.imputationCount > 1 || updated.multipleImputation;
            if (updated.displayedImputation != it->second.displayedImputation) return "OK\t" + id;
        }
        updated.calculationMethod.clear();
        if (cursor + 1 < args.size() && args[cursor] == "CALCULATION_V1") {
            ++cursor;
            const int count = std::atoi(args[cursor++].c_str());
            if (count < 0 || count > 16 || cursor + count > args.size())
                return "ERR invalid dimensionality calculation description";
            for (int line = 0; line < count; ++line) {
                if (line) updated.calculationMethod += "\n";
                updated.calculationMethod += args[cursor++];
            }
        }
        AnalysisProvenance returnedProvenance;
        {
            std::string provenanceError;
            if (!ReadAnalysisProvenancePayload(
                    args, cursor, returnedProvenance, &provenanceError)) {
                return "ERR malformed dimensionality provenance: " + provenanceError;
            }
        }
        const DimensionalityState &current = it->second;
        const std::string incomingSignature = DimensionalityFitSignature(
            updated.method, updated.missingMode, updated.rotation, updated.extraction, updated.scope,
            updated.scale, updated.componentCount, updated.displayedImputation, updated.variables);
        const std::string currentSignature = DimensionalityFitSignature(
            current.method, current.missingMode, current.rotation, current.extraction, current.scope,
            current.scale, current.componentCount, current.displayedImputation, current.variables);
        if (!current.autoFit || (!identified && incomingSignature != currentSignature)) {
            return "OK\t" + id;
        }
        if (identified) {
            if (cursor != args.size() || updated.group != current.group || updated.variables != current.variables ||
                updated.method != current.method || updated.missingMode != current.missingMode ||
                updated.extraction != current.extraction || updated.scale != current.scale || updated.scope != current.scope ||
                returnedProvenance.dataVersion.version != current.sourceDataVersion ||
                returnedProvenance.executedRCode.empty()) return "ERR mismatched dimensionality result";
            const std::set<int> allowed(current.dataScope.originalRowIds.begin(), current.dataScope.originalRowIds.end());
            auto allowedRow = [&](int row) {
                return row > 0 && static_cast<std::size_t>(row) <= current.dataScope.totalDatasetRows &&
                    (current.dataScope.kind == AnalysisScopeKind::AllObservations || allowed.count(row));
            };
            if (!std::all_of(updated.rowsUsed.begin(), updated.rowsUsed.end(), allowedRow) ||
                !std::all_of(updated.scores.begin(), updated.scores.end(),
                    [&](const auto &score) { return allowedRow(score.row); }))
                return "ERR dimensionality result uses rows outside its scope";
        }
        updated.rFitPending = false;
        updated.lastRFitSignature = identified ? DimensionalityRequestIdentity(applicationState_, updated) : incomingSignature;
        updated.modelVersion += 1;
        if (const DataFrameModel *dataframe = applicationState_.datasets().find(updated.group);
            dataframe && !returnedProvenance.executedRCode.empty()) {
            const AnalysisScope scope = updated.dataScopeCaptured
                ? updated.dataScope : applicationState_.activeAnalysisScope(updated.group);
            CompleteAnalysisProvenance(
                returnedProvenance, *dataframe, scope,
                updated.rowsUsed, updated.rowsExcluded, updated.id,
                updated.method == "factor" ? "Factor Analysis" : "Principal Components");
            updated.provenance = returnedProvenance;
            applicationState_.registerOutputCodeReference(
                DimensionalityCodeReference(updated, returnedProvenance));
        }
        it->second = std::move(updated);
        services_.ui.showDimensionality(id);
        services_.ui.refreshDimensionalityPlots(id);
        return "OK\t" + id;
    }
    case CommandAction::DimensionalityInfo: {
        if (request.args.empty()) return "ERR missing dimensionality model id";
        const auto it = applicationState_.dimensionalityModels().find(request.args[0]);
        if (it == applicationState_.dimensionalityModels().end()) {
            return "ERR unknown principal components/factor analysis model";
        }
        const DimensionalityState &state = it->second;
        std::ostringstream out;
        out << "OK\tDIMENSIONALITY|" << state.id << "|" << state.group << "|" << state.method
            << "|" << state.missingMode << "|" << (state.scale ? "TRUE" : "FALSE")
            << "|" << state.componentCount << "|" << state.rotation << "|" << state.scope
            << "|" << state.variables.size() << "|" << state.rowsUsed.size()
            << "|" << state.loadings.size();
        for (const std::string &variable : state.variables) out << "|" << variable;
        return out.str();
    }
    case CommandAction::DimensionalitySetVariables: {
        if (request.args.size() < 2) return "ERR malformed PCAFA_SET_VARIABLES command";
        auto it = applicationState_.dimensionalityModels().find(request.args[0]);
        if (it == applicationState_.dimensionalityModels().end()) {
            return "ERR unknown principal components/factor analysis model";
        }
        DimensionalityState &state = it->second;
        const long count = std::strtol(request.args[1].c_str(), nullptr, 10);
        if (count < 0 || request.args.size() < static_cast<std::size_t>(2 + count)) {
            return "ERR invalid dimensionality variable count";
        }
        if (const auto *dataframe = applicationState_.datasets().find(state.group))
            state.eligibleVariables = EligibleDimensionalityVariables(*dataframe);
        std::vector<std::string> variables;
        for (long index = 0; index < count; ++index) {
            const std::string &variable = request.args[static_cast<std::size_t>(2 + index)];
            if (std::find(state.eligibleVariables.begin(), state.eligibleVariables.end(), variable) == state.eligibleVariables.end()) {
                return "ERR dimensionality requires numeric, ordinal, or binary variables";
            }
            if (std::find(variables.begin(), variables.end(), variable) == variables.end()) {
                variables.push_back(variable);
            }
        }
        state.variables = variables;
        state.componentCount = std::max(1, std::min(
            state.componentCount,
            static_cast<int>(std::max<std::size_t>(1, state.variables.size()))));
        if (!services_.selection.refitDimensionality) return std::nullopt;
        return services_.selection.refitDimensionality(state.id)
            ? "OK" : "ERR unknown principal components/factor analysis model";
    }
    case CommandAction::ScaleAnalysisOpen: {
        if (!services_.ui.showScaleAnalysis) return std::nullopt;
        ScaleAnalysisState state;
        std::string error;
        if (!ParseScaleAnalysisPayload(request.args, state, false, error)) {
            return "ERR " + error;
        }
        if (services_.queries.groupSeed && services_.queries.groupSeed(state.group, state.seed)) {
            state.hasSeed = true;
        }
        applicationState_.scaleAnalyses()[state.id] = std::move(state);
        ScaleAnalysisState &stored = applicationState_.scaleAnalyses()[request.args[0]];
        if (const DataFrameModel *dataframe =
                applicationState_.datasets().find(stored.group)) {
            applicationState_.registerOutputCodeReference(
                ScaleAnalysisCodeReference(stored, *dataframe));
        }
        services_.ui.showScaleAnalysis(request.args[0]);
        if (services_.ui.refreshScaleAnalysisPlots) {
            services_.ui.refreshScaleAnalysisPlots(request.args[0]);
        }
        return "OK\t" + request.args[0];
    }
    case CommandAction::ScaleAnalysisUpdate: {
        if (request.args.empty()) return "ERR missing Scale Analysis id";
        auto found = applicationState_.scaleAnalyses().find(request.args[0]);
        if (found == applicationState_.scaleAnalyses().end()) {
            return "ERR unknown Scale Analysis session";
        }
        std::string error;
        ScaleAnalysisState updated = found->second;
        if (!ParseScaleAnalysisPayload(request.args, updated, true, error)) {
            return "ERR " + error;
        }
        found->second = std::move(updated);
        if (const DataFrameModel *dataframe =
                applicationState_.datasets().find(found->second.group)) {
            applicationState_.registerOutputCodeReference(
                ScaleAnalysisCodeReference(found->second, *dataframe));
        }
        if (services_.ui.showScaleAnalysis) services_.ui.showScaleAnalysis(found->first);
        if (services_.ui.refreshScaleAnalysisPlots) {
            services_.ui.refreshScaleAnalysisPlots(found->first);
        }
        return "OK\t" + found->first;
    }
    case CommandAction::ScaleAnalysisSetItems: {
        if (request.args.size() < 2) return "ERR malformed SCALE_ANALYSIS_SET_ITEMS command";
        auto found = applicationState_.scaleAnalyses().find(request.args[0]);
        if (found == applicationState_.scaleAnalyses().end()) {
            return "ERR unknown Scale Analysis session";
        }
        ScaleAnalysisState &state = found->second;
        const DataFrameModel *dataframe = applicationState_.datasets().find(state.group);
        if (!dataframe) return "ERR Scale Analysis dataset is not registered";
        const long count = std::strtol(request.args[1].c_str(), nullptr, 10);
        std::size_t cursor = 2;
        if (count < 0 || cursor + static_cast<std::size_t>(count) * 6U != request.args.size()) {
            return "ERR invalid Scale Analysis item count";
        }
        ScaleAnalysisSpecification candidate = state.specification;
        candidate.items.clear();
        for (long index = 0; index < count; ++index) {
            ScaleItemSpecification item;
            item.variable = request.args[cursor++];
            if (!ParseScaleItemType(request.args[cursor++], item.type)) {
                return "ERR unsupported Scale Analysis item type";
            }
            item.reversed = request.args[cursor++] == "TRUE";
            item.hasScoringRange = request.args[cursor++] == "TRUE";
            const std::string minimum = request.args[cursor++];
            const std::string maximum = request.args[cursor++];
            if (item.hasScoringRange) {
                item.scoringMinimum = std::strtod(minimum.c_str(), nullptr);
                item.scoringMaximum = std::strtod(maximum.c_str(), nullptr);
            }
            candidate.items.push_back(std::move(item));
        }
        candidate.revision = state.specification.revision + 1;
        std::string error;
        if (!ValidateScaleAnalysisSpecification(*dataframe, candidate, error)) {
            return "ERR " + error;
        }
        state.specification = std::move(candidate);
        state.rFitPending = true;
        state.status = state.specification.items.size() < 2
            ? "Choose at least two numeric or ordinal items."
            : "Fitting Scale Analysis in R...";
        if (services_.ui.showScaleAnalysis) services_.ui.showScaleAnalysis(state.id);
        if (state.specification.items.size() < 2) return "OK";
        if (!services_.selection.refitScaleAnalysis) return std::nullopt;
        return services_.selection.refitScaleAnalysis(state.id)
            ? "OK" : "ERR Scale Analysis could not be queued";
    }
    case CommandAction::ScaleAnalysisInfo: {
        if (request.args.empty()) return "ERR missing Scale Analysis id";
        const auto found = applicationState_.scaleAnalyses().find(request.args[0]);
        if (found == applicationState_.scaleAnalyses().end()) {
            return "ERR unknown Scale Analysis session";
        }
        const ScaleAnalysisState &state = found->second;
        std::ostringstream out;
        out << "OK\tSCALE_ANALYSIS|" << state.id << "|" << state.group << "|"
            << state.specification.revision << "|" << state.specification.fingerprint << "|"
            << state.specification.correlationBasis << "|" << state.specification.scoreMethod
            << "|" << state.specification.items.size();
        for (const ScaleItemSpecification &item : state.specification.items) {
            out << "|" << item.variable << ":" << ScaleItemTypeToken(item.type)
                << ":" << (item.reversed ? "reversed" : "forward");
        }
        return out.str();
    }
    case CommandAction::DendrogramUpdate: {
        if (request.args.size() < 8) return "ERR malformed DENDRO_UPDATE command";
        auto it = applicationState_.dendrograms().find(request.args[0]);
        if (it == applicationState_.dendrograms().end()) return "ERR unknown dendrogram";
        auto &current = it->second;
        const auto *data = applicationState_.datasets().find(current.group);
        if (!current.rFitPending || request.args[1] != std::to_string(current.requestRevision) ||
            request.args[2] != std::to_string(current.sourceDataVersion) || !data ||
            data->dataVersion != current.sourceDataVersion ||
            std::max(1, data->activeImputationVersion) != current.displayedImputation)
            return "OK\tignored stale clustering result";
        DendrogramFitResult tree;
        std::size_t cursor = 5;
        std::string error;
        if (!ReadDendrogramRTree(request.args, cursor, tree, error)) return "ERR " + error;
        AnalysisProvenance provenance;
        const bool ok = request.args[3] == "ok";
        if (!ok && request.args[3] != "error") return "ERR invalid clustering result status";
        if (ok) {
            if (!ReadAnalysisProvenancePayload(request.args, cursor, provenance, &error) ||
                provenance.executedRCode.empty()) return "ERR missing clustering provenance: " + error;
            const auto allowed = ResolveAnalysisScopeRowIds(current.dataScope, current.dataScope.totalDatasetRows);
            const std::set<int> scopeRows(allowed.begin(), allowed.end());
            for (int row : tree.caseRows) if (!scopeRows.count(row)) return "ERR clustering row outside requested scope";
            std::vector<int> excluded;
            const std::set<int> used(tree.caseRows.begin(), tree.caseRows.end());
            for (int row : allowed) if (!used.count(row)) excluded.push_back(row);
            CompleteAnalysisProvenance(provenance, *data, current.dataScope, tree.caseRows, excluded,
                                       current.id, "Quick Cluster");
        } else if (!tree.caseRows.empty()) return "ERR error result contains a clustering tree";
        if (cursor != request.args.size()) return "ERR unexpected clustering payload";
        current.caseRows = std::move(tree.caseRows); current.merges = std::move(tree.merges);
        current.leafOrder = std::move(tree.leafOrder); current.provenance = std::move(provenance);
        current.rFitPending = false; current.status = request.args[4]; ++current.modelVersion;
        RefreshDendrogramCodeReference(applicationState_, current);
        if (services_.ui.showDendrogram) services_.ui.showDendrogram(current.id);
        return "OK\t" + current.id;
    }
    case CommandAction::DendrogramInfo: {
        if (request.args.empty()) return "ERR missing dendrogram id";
        const auto it = applicationState_.dendrograms().find(request.args[0]);
        if (it == applicationState_.dendrograms().end()) return "ERR unknown dendrogram";
        const DendrogramState &state = it->second;
        std::ostringstream out;
        out << "OK\tDENDROGRAM|" << state.id << "|" << state.group << "|" << state.distance
            << "|" << state.linkage << "|" << state.missingMode << "|" << state.variables.size()
            << "|" << state.caseRows.size() << "|" << state.merges.size();
        for (const std::string &variable : state.variables) out << "|" << variable;
        return out.str();
    }
    case CommandAction::DendrogramSetVariables: {
        if (request.args.size() < 2) return "ERR malformed DENDRO_SET_VARIABLES command";
        auto it = applicationState_.dendrograms().find(request.args[0]);
        if (it == applicationState_.dendrograms().end()) return "ERR unknown dendrogram";
        DendrogramState &state = it->second;
        const long count = std::strtol(request.args[1].c_str(), nullptr, 10);
        if (count < 0 || request.args.size() < static_cast<std::size_t>(2 + count)) {
            return "ERR invalid dendrogram variable count";
        }
        std::vector<std::string> variables;
        for (long index = 0; index < count; ++index) {
            const std::string &variable = request.args[static_cast<std::size_t>(2 + index)];
            if (!FindNumericVariable(state.seed, variable)) {
                return "ERR dendrogram variables must be numeric";
            }
            if (std::find(variables.begin(), variables.end(), variable) == variables.end()) {
                variables.push_back(variable);
            }
        }
        state.variables = variables;
        if (!services_.selection.refitDendrogram) return std::nullopt;
        return services_.selection.refitDendrogram(state.id) ? "OK" : "ERR unknown dendrogram";
    }
    case CommandAction::DendrogramSetDistance: {
        if (request.args.size() != 2) return "ERR malformed DENDRO_SET_DISTANCE command";
        auto it = applicationState_.dendrograms().find(request.args[0]);
        if (it == applicationState_.dendrograms().end()) return "ERR unknown dendrogram";
        if (!DendrogramDistanceIsValid(request.args[1])) return "ERR unsupported dendrogram distance";
        if (it->second.distance == request.args[1]) return "OK";
        it->second.distance = request.args[1];
        if (!services_.selection.refitDendrogram) return std::nullopt;
        return services_.selection.refitDendrogram(it->second.id) ? "OK" : "ERR unknown dendrogram";
    }
    case CommandAction::DendrogramOpen: {
        if (request.args.size() < 6) return "ERR malformed DENDRO_OPEN command";
        if (!services_.queries.groupSeed || !services_.selection.refitDendrogram) {
            return std::nullopt;
        }
        DendrogramState state;
        state.id = request.args[0];
        state.group = request.args[1];
        state.distance = request.args[2];
        state.linkage = request.args[3];
        state.missingMode = request.args[4];
        if (!DendrogramDistanceIsValid(state.distance)) return "ERR unsupported dendrogram distance";
        if (!DendrogramLinkageIsValid(state.linkage)) return "ERR invalid dendrogram linkage";
        if (state.missingMode != "pairwise" && state.missingMode != "listwise") {
            return "ERR invalid missing-data mode";
        }
        if (!services_.queries.groupSeed(state.group, state.seed)) {
            return "ERR no registered dataset/group for quick cluster";
        }
        state.hasSeed = true;
        const long count = std::strtol(request.args[5].c_str(), nullptr, 10);
        if (count < 0 || request.args.size() < static_cast<std::size_t>(6 + count)) {
            return "ERR invalid dendrogram variable count";
        }
        for (long index = 0; index < count; ++index) {
            const std::string &variable = request.args[static_cast<std::size_t>(6 + index)];
            if (!FindNumericVariable(state.seed, variable)) {
                return "ERR dendrogram variables must be numeric";
            }
            if (std::find(state.variables.begin(), state.variables.end(), variable) == state.variables.end()) {
                state.variables.push_back(variable);
            }
        }
        const auto previous = applicationState_.dendrograms().find(state.id);
        if (previous != applicationState_.dendrograms().end()) state.requestRevision = previous->second.requestRevision;
        applicationState_.dendrograms()[state.id] = state;
        return services_.selection.refitDendrogram(state.id)
            ? "OK\t" + state.id : "ERR no registered dataset/group for quick cluster";
    }
    case CommandAction::ModelSetY:
    case CommandAction::ModelAddTerm:
    case CommandAction::ModelReplaceTerm:
    case CommandAction::ModelRemoveTerm:
    case CommandAction::ModelClearRole: {
        if (request.args.empty()) return "ERR missing linear model ID";
        if (!services_.queries.groupPlot) return std::nullopt;
        const std::string modelId = request.args[0];
        auto existingModel = applicationState_.groupModels().find(modelId);
        const std::string datasetGroup = existingModel == applicationState_.groupModels().end()
            ? modelId : existingModel->second.group;
        PlotModel seed;
        if (!services_.queries.groupPlot(datasetGroup, seed)) return "ERR no active plot/group";
        GroupModelState &state = EnsureGroupModelState(
            applicationState_.groupModels(), modelId, &seed);
        const std::string variable = request.args.size() >= 2 ? request.args[1] : "";
        bool changed = false;
        if (request.action == CommandAction::ModelSetY) {
            if (variable.empty() || !FindNumericVariable(seed, variable)) {
                return "ERR dependent variable must be an available numeric variable";
            }
            const auto edit = SetModelSpecificationResponse(state, variable);
            changed = edit.ok && edit.changed;
        } else if (request.action == CommandAction::ModelAddTerm) {
            const DataFrameModel *dataframe = applicationState_.datasets().find(datasetGroup);
            const auto edit = AddModelSpecificationTerm(
                state, AvailableVariableNames(seed, dataframe), variable);
            if (!edit.ok) return "ERR " + edit.message;
            changed = edit.changed;
        } else if (request.action == CommandAction::ModelReplaceTerm) {
            if (request.args.size() < 3) return "ERR missing replacement model term";
            const std::string replacement = request.args[2];
            const DataFrameModel *dataframe = applicationState_.datasets().find(datasetGroup);
            const auto edit = ReplaceModelSpecificationTerm(
                state, AvailableVariableNames(seed, dataframe), variable, replacement);
            if (!edit.ok) return "ERR " + edit.message;
            changed = edit.changed;
        } else if (request.action == CommandAction::ModelRemoveTerm) {
            if (variable.empty()) return "ERR missing model term";
            const auto edit = RemoveModelSpecificationTerm(state, variable);
            if (!edit.ok) return "ERR " + edit.message;
            changed = edit.changed;
        } else {
            if (variable.empty()) return "ERR missing variable";
            if (state.response == variable) {
                state.response.clear();
                changed = true;
            }
            const auto edit = RemoveModelSpecificationTerm(state, variable);
            changed = changed || (edit.ok && edit.changed);
        }
        if (changed) {
            MarkGroupModelChanged(state);
            if (services_.ui.refreshModelGroup) services_.ui.refreshModelGroup(modelId);
        }
        return "OK";
    }
    case CommandAction::ModelScope: {
        return "ERR Analysis scope is global. Use the central Analysis Scope menu.";
    }
    case CommandAction::GetAnalysisScope: {
        if (request.args.empty()) return "ERR missing dataset ID";
        const DataFrameModel *dataset = applicationState_.datasets().find(request.args[0]);
        if (!dataset) return "ERR analysis scope dataset is not registered";
        const AnalysisScope scope = applicationState_.activeAnalysisScope(request.args[0]);
        const std::vector<int> rows = ResolveAnalysisScopeRowIds(
            scope, static_cast<std::size_t>(std::max(0, dataset->rows)));
        std::ostringstream out;
        out << "OK\t" << AnalysisScopeKindId(scope.kind) << "|"
            << AnalysisScopeSourceKindId(scope.sourceKind) << "|"
            << EncodeCommandField(scope.sourceDescription) << "|"
            << rows.size() << "|" << std::max(0, dataset->rows) << "|"
            << rows.size();
        for (int row : rows) out << "|" << row;
        return out.str();
    }
    case CommandAction::GetSavedAnalysisScopes: {
        if (request.args.empty()) return "ERR missing dataset ID";
        if (!applicationState_.datasets().find(request.args[0])) {
            return "ERR analysis scope dataset is not registered";
        }
        const std::vector<SavedSelection> selections =
            applicationState_.savedSelections(request.args[0]);
        std::ostringstream out;
        out << "OK\t" << selections.size();
        for (const SavedSelection &selection : selections) {
            out << "|" << EncodeCommandField(selection.name)
                << "|" << selection.originalRowIds.size();
            for (int row : selection.originalRowIds) out << "|" << row;
        }
        return out.str();
    }
    case CommandAction::GetExcludedRows: {
        if (request.args.empty() || !applicationState_.datasets().find(request.args[0]))
            return "ERR exclusion dataset is not registered";
        const auto rows = applicationState_.excludedRows(request.args[0]);
        std::ostringstream out;
        out << "OK\t" << rows.size();
        for (int row : rows) out << "|" << row;
        return out.str();
    }
    case CommandAction::ExcludeSelectedCases:
    case CommandAction::IncludeSelectedCases:
    case CommandAction::IncludeAllCases:
    case CommandAction::ToggleCaseIncluded: {
        if (request.args.empty()) return "ERR missing dataset ID";
        const std::string &datasetId = request.args[0];
        const auto *dataset = applicationState_.datasets().find(datasetId);
        if (!dataset) return "ERR exclusion dataset is not registered";
        std::set<int> next = applicationState_.excludedRows(datasetId);
        if (request.action == CommandAction::IncludeAllCases) {
            next.clear();
        } else if (request.action == CommandAction::ToggleCaseIncluded) {
            if (request.args.size() != 2) return "ERR expected one case number";
            char *end = nullptr;
            const long row = std::strtol(request.args[1].c_str(), &end, 10);
            if (!end || *end != '\0' || row < 1 || row > dataset->rows)
                return "ERR case number is outside the dataset";
            if (!next.erase(static_cast<int>(row))) next.insert(static_cast<int>(row));
        } else {
            std::set<int> selected;
            if (!applicationState_.selectedRows(datasetId, selected) || selected.empty())
                return "ERR Select one or more cases first.";
            for (int row : selected) {
                if (row < 1 || row > dataset->rows) continue;
                if (request.action == CommandAction::ExcludeSelectedCases) next.insert(row);
                else next.erase(row);
            }
        }
        if (next == applicationState_.excludedRows(datasetId)) return "OK no change";
        std::string error;
        std::set<int> previousSelection;
        applicationState_.selectedRows(datasetId, previousSelection);
        AnalysisScopeChangeEvent event;
        if (!applicationState_.setExcludedRows(datasetId, next, &event, &error))
            return "ERR " + error;
        std::set<int> currentSelection;
        applicationState_.selectedRows(datasetId, currentSelection);
        if (currentSelection != previousSelection &&
            services_.selection.selectionChanged) {
            services_.selection.selectionChanged(datasetId, currentSelection,
                applicationState_.groupSelectionVersions().at(datasetId), false);
        }
        if (services_.ui.analysisScopeChanged) services_.ui.analysisScopeChanged(event);
        return "OK " + std::to_string(std::max(0, dataset->rows) -
            static_cast<int>(next.size())) + " of " +
            std::to_string(std::max(0, dataset->rows)) + " cases included";
    }
    case CommandAction::SetAnalysisScopeAll: {
        if (request.args.empty()) return "ERR missing dataset ID";
        AnalysisScopeChangeEvent event;
        std::string error;
        if (!applicationState_.resetActiveAnalysisScopeToAllObservations(
                request.args[0], &event, &error)) {
            return "ERR " + error;
        }
        if (services_.ui.analysisScopeChanged) services_.ui.analysisScopeChanged(event);
        return "OK " + AnalysisScopeSummary(event.currentScope,
            event.currentScope.totalDatasetRows);
    }
    case CommandAction::SetAnalysisScopeFromSelection: {
        if (request.args.empty()) return "ERR missing dataset ID";
        if (request.name == "SET_ANALYSIS_SCOPE_UNSELECTED") {
            AnalysisScopeChangeEvent event; std::string error;
            if (!applicationState_.setActiveAnalysisScopeFromUnselected(request.args[0], &event, &error)) return "ERR " + error;
            if (services_.ui.analysisScopeChanged) services_.ui.analysisScopeChanged(event);
            return "OK " + AnalysisScopeSummary(event.currentScope,event.currentScope.totalDatasetRows);
        }
        const std::string description = request.args.size() > 1 && !request.args[1].empty()
            ? request.args[1] : "Current selection (live)";
        const std::optional<std::string> sourceView = request.args.size() > 2 && !request.args[2].empty()
            ? std::optional<std::string>(request.args[2]) : std::nullopt;
        AnalysisScopeChangeEvent event;
        std::string error;
        if (!applicationState_.setActiveAnalysisScopeFromSelection(
                request.args[0], AnalysisScopeSourceKind::CurrentSelection,
                description, sourceView, &event, &error)) {
            return "ERR " + error;
        }
        if (services_.ui.analysisScopeChanged) services_.ui.analysisScopeChanged(event);
        return "OK " + AnalysisScopeSummary(event.currentScope,
            event.currentScope.totalDatasetRows);
    }
    case CommandAction::SetAnalysisScopeFromRows: {
        if (request.args.size() < 4) return "ERR malformed analysis scope row request";
        const std::string &datasetId = request.args[0];
        const long count = std::strtol(request.args[1].c_str(), nullptr, 10);
        if (count < 0 || request.args.size() < static_cast<std::size_t>(4 + count)) {
            return "ERR invalid analysis scope row count";
        }
        std::vector<int> rows;
        rows.reserve(static_cast<std::size_t>(count));
        for (long i = 0; i < count; ++i) {
            rows.push_back(std::atoi(request.args[static_cast<std::size_t>(2 + i)].c_str()));
        }
        const std::size_t metadata = static_cast<std::size_t>(2 + count);
        const AnalysisScopeSourceKind sourceKind =
            AnalysisScopeSourceKindFromId(request.args[metadata]);
        const std::string description = request.args[metadata + 1];
        const std::optional<std::string> sourceView =
            request.args.size() > metadata + 2 && !request.args[metadata + 2].empty()
                ? std::optional<std::string>(request.args[metadata + 2]) : std::nullopt;
        const std::optional<std::string> sourceElement =
            request.args.size() > metadata + 3 && !request.args[metadata + 3].empty()
                ? std::optional<std::string>(request.args[metadata + 3]) : std::nullopt;
        const DataFrameModel *dataset = applicationState_.datasets().find(datasetId);
        if (!dataset) return "ERR analysis scope dataset is not registered";
        AnalysisScope scope = ExplicitAnalysisScope(
            datasetId, rows, sourceKind, description,
            static_cast<std::size_t>(std::max(0, dataset->rows)), sourceView, sourceElement);
        AnalysisScopeChangeEvent event;
        std::string error;
        if (!applicationState_.setActiveAnalysisScope(scope, &event, &error)) {
            return "ERR " + error;
        }
        if (services_.ui.analysisScopeChanged) services_.ui.analysisScopeChanged(event);
        return "OK " + AnalysisScopeSummary(event.currentScope,
            event.currentScope.totalDatasetRows);
    }
    case CommandAction::SaveAnalysisScopeFromSelection: {
        if (request.args.size() < 2) return "ERR missing saved scope name";
        const std::optional<std::string> sourceView =
            request.args.size() > 2 && !request.args[2].empty()
                ? std::optional<std::string>(request.args[2]) : std::nullopt;
        AnalysisScopeChangeEvent event;
        std::string error;
        if (!applicationState_.saveCurrentSelectionAsAnalysisScope(
                request.args[0], request.args[1],
                AnalysisScopeSourceKind::CurrentSelection, sourceView, &event, &error)) {
            return "ERR " + error;
        }
        if (services_.ui.analysisScopeChanged) services_.ui.analysisScopeChanged(event);
        const std::optional<SavedSelection> saved =
            applicationState_.savedSelection(request.args[0], request.args[1]);
        return "OK saved and applied " + request.args[1] + " (" +
            std::to_string(saved.has_value() ? saved->originalRowIds.size() : 0) + ")";
    }
    case CommandAction::UseSavedAnalysisScope: {
        if (request.args.size() < 2) return "ERR missing saved selection name";
        AnalysisScopeChangeEvent event;
        std::string error;
        if (!applicationState_.activateSavedSelection(
                request.args[0], request.args[1], &event, &error)) {
            return "ERR " + error;
        }
        if (services_.ui.analysisScopeChanged) services_.ui.analysisScopeChanged(event);
        return "OK " + AnalysisScopeSummary(event.currentScope,
            event.currentScope.totalDatasetRows);
    }
    case CommandAction::AddSavedAnalysisScope: {
        if (request.args.size() < 3) return "ERR missing combined selection name";
        AnalysisScopeChangeEvent event;
        std::string error;
        if (!applicationState_.addSavedSelectionToActiveScope(
                request.args[0], request.args[1], request.args[2], &event, &error)) {
            return "ERR " + error;
        }
        if (services_.ui.analysisScopeChanged) services_.ui.analysisScopeChanged(event);
        return "OK " + AnalysisScopeSummary(event.currentScope,
            event.currentScope.totalDatasetRows);
    }
    case CommandAction::SelectedRows: {
        if (request.args.empty()) return "ERR missing group name";
        std::set<int> rows;
        if (!applicationState_.selectedRows(request.args[0], rows)) {
            return "ERR no active plot/group";
        }
        const std::string text = CaseSetText(rows);
        return text.empty() ? "OK" : "OK " + text;
    }
    case CommandAction::SetSelectedRows: {
        if (request.args.size() < 2) return "ERR malformed SET_SELECTED command";
        const long count = std::strtol(request.args[1].c_str(), nullptr, 10);
        if (count < 0 || request.args.size() < static_cast<std::size_t>(2 + count)) {
            return "ERR invalid selected row count";
        }
        std::set<int> rows;
        const DataFrameModel *dataset = applicationState_.datasets().find(request.args[0]);
        for (long i = 0; i < count; ++i) {
            const int row = std::atoi(request.args[static_cast<std::size_t>(2 + i)].c_str());
            if (row <= 0 || (dataset && row > dataset->rows))
                return "ERR selected row is outside the dataset";
            rows.insert(row);
        }
        const auto coordination =
            plotCoordinator_.replaceSelection(request.args[0], rows);
        if (!coordination.accepted) return "ERR no active plot/group";
        if (coordination.changed && services_.selection.selectionChanged) {
            services_.selection.selectionChanged(
                coordination.event.group,
                coordination.event.selectedRows,
                coordination.event.selectionVersion,
                false);
        }
        return "OK";
    }
    case CommandAction::SelectAllRows: {
        if (request.args.empty()) return "ERR missing group name";
        std::set<int> rows;
        if (const DataFrameModel *dataset = applicationState_.datasets().find(request.args[0])) {
            for (int row = 1; row <= dataset->rows; ++row) rows.insert(row);
        } else {
            if (!services_.selection.visibleRows) return std::nullopt;
            if (!services_.selection.visibleRows(request.args[0], rows))
                return "ERR no active plot/group";
        }
        const auto coordination =
            plotCoordinator_.replaceSelection(request.args[0], rows);
        if (!coordination.accepted) return "ERR no active plot/group";
        if (coordination.changed && services_.selection.selectionChanged) {
            services_.selection.selectionChanged(
                coordination.event.group,
                coordination.event.selectedRows,
                coordination.event.selectionVersion,
                false);
        }
        return "OK";
    }
    case CommandAction::ClearSelection: {
        if (request.name != "CLEAR") return std::nullopt;
        if (request.args.empty()) return "ERR missing group name";
        const auto coordination =
            plotCoordinator_.clearSelection(request.args[0]);
        if (!coordination.accepted) return "ERR no active plot/group";
        if (coordination.changed && services_.selection.selectionChanged) {
            services_.selection.selectionChanged(
                coordination.event.group,
                coordination.event.selectedRows,
                coordination.event.selectionVersion,
                true);
        }
        return "OK";
    }
    case CommandAction::InvertSelection: {
        if (request.name != "INVERT" && request.name != "INVERT_SELECTION") {
            return std::nullopt;
        }
        if (request.args.empty()) return "ERR missing group name";
        std::set<int> visible;
        const DataFrameModel *dataset = applicationState_.datasets().find(request.args[0]);
        if (dataset) {
            for (int row = 1; row <= dataset->rows; ++row) visible.insert(row);
        } else {
            if (!services_.selection.visibleRows) return std::nullopt;
            if (!services_.selection.visibleRows(request.args[0], visible)) {
                return "ERR no active plot/group";
            }
        }
        const auto coordination =
            plotCoordinator_.invertSelection(request.args[0], visible);
        if (!coordination.accepted) return "ERR no active plot/group";
        if (coordination.changed && services_.selection.selectionChanged) {
            services_.selection.selectionChanged(
                coordination.event.group,
                coordination.event.selectedRows,
                coordination.event.selectionVersion,
                false);
        }
        return "OK";
    }
    case CommandAction::SetSelectedColor: {
        if (request.args.size() < 2) return "ERR malformed SET_SELECTED_COLOR command";
        const std::string &group = request.args[0];
        const std::string &color = request.args[1];
        if (!FindPaletteColor(color)) return "ERR unknown color";
        std::set<int> selected;
        if (!applicationState_.selectedRows(group, selected)) {
            return "ERR no active plot/group";
        }
        applicationState_.setSelectedColor(group, color);
        std::vector<int> changed;
        for (int row : selected) {
            if (applicationState_.setPointColor(group, row, color)) {
                changed.push_back(row);
            }
        }
        if (services_.selection.rowColorsChanged) {
            services_.selection.rowColorsChanged(group, changed, true, !changed.empty());
        }
        return "OK";
    }
    case CommandAction::ResetSelectedColor: {
        if (request.args.empty()) return "ERR missing group name";
        const std::string &group = request.args[0];
        std::set<int> selected;
        if (!applicationState_.selectedRows(group, selected)) {
            return "ERR no active plot/group";
        }
        applicationState_.setSelectedColor(group, "black");
        const std::vector<int> rows(selected.begin(), selected.end());
        (void)applicationState_.clearPointColors(group, rows);
        if (services_.selection.rowColorsChanged) {
            services_.selection.rowColorsChanged(group, rows, true, !rows.empty());
        }
        return "OK";
    }
    case CommandAction::GetSelectedColor: {
        if (request.args.empty()) return "ERR missing group name";
        std::set<int> selected;
        if (!applicationState_.selectedRows(request.args[0], selected)) {
            return "ERR no active plot/group";
        }
        return "OK " + applicationState_.selectedColor(request.args[0]);
    }
    case CommandAction::SetPointColor: {
        if (request.args.size() < 4) return "ERR malformed SET_POINT_COLOR command";
        const std::string &group = request.args[0];
        const std::string &color = request.args[1];
        if (!FindPaletteColor(color)) return "ERR unknown color";
        const long count = std::strtol(request.args[2].c_str(), nullptr, 10);
        if (count < 0 || request.args.size() < static_cast<std::size_t>(3 + count)) {
            return "ERR invalid row count";
        }
        std::set<int> selected;
        if (!applicationState_.selectedRows(group, selected)) {
            return "ERR no active plot/group";
        }
        std::vector<int> changed;
        for (long i = 0; i < count; ++i) {
            const int row = std::atoi(request.args[static_cast<std::size_t>(3 + i)].c_str());
            if (applicationState_.setPointColor(group, row, color)) {
                changed.push_back(row);
            }
        }
        if (services_.selection.rowColorsChanged) {
            services_.selection.rowColorsChanged(group, changed, false, !changed.empty());
        }
        return "OK";
    }
    case CommandAction::ClearRowColors: {
        if (request.args.empty()) return "ERR missing group name";
        const std::string &group = request.args[0];
        std::vector<int> rows;
        if (request.args.size() >= 2) {
            const long count = std::strtol(request.args[1].c_str(), nullptr, 10);
            if (count > 0) rows.reserve(static_cast<std::size_t>(count));
            for (long i = 0; i < count && 2 + i < static_cast<long>(request.args.size()); ++i) {
                rows.push_back(std::atoi(request.args[static_cast<std::size_t>(2 + i)].c_str()));
            }
        }
        const bool recompute = !applicationState_.pointColors(group).empty();
        std::vector<int> changed = applicationState_.clearPointColors(group, rows);
        if (services_.selection.rowColorsChanged) {
            services_.selection.rowColorsChanged(group, changed, false, recompute);
        }
        return "OK";
    }
    case CommandAction::InteractionMode: {
        if (request.args.empty()) return "ERR missing plot id";
        if (!services_.selection.plotModes) return std::nullopt;
        std::string interaction;
        std::string selection;
        if (!services_.selection.plotModes(request.args[0], interaction, selection)) {
            return "ERR no active plot";
        }
        if (request.args.size() == 1) return "OK " + interaction;
        const std::string &mode = request.args[1];
        if (mode != "none" && mode != "select" && mode != "brush" &&
            mode != "identify" && mode != "pan" && mode != "zoom" && mode != "label") {
            return "ERR invalid mode";
        }
        if (!services_.selection.setInteractionMode ||
            !services_.selection.setInteractionMode(request.args[0], mode)) {
            return "ERR no active plot";
        }
        return "OK";
    }
    case CommandAction::SelectionMode:
    case CommandAction::SelectionOperation: {
        const bool isMode = request.action == CommandAction::SelectionMode;
        if (isMode && request.args.size() < 2) {
            return "ERR malformed SELECTION_MODE command";
        }
        if (!isMode && request.args.empty()) return "ERR missing plot id";
        if (!services_.selection.plotModes) return std::nullopt;
        std::string interaction;
        std::string selection;
        if (!services_.selection.plotModes(request.args[0], interaction, selection)) {
            return "ERR no active plot";
        }
        if (!isMode && request.args.size() == 1) return "OK " + selection;
        const std::string &mode = request.args[1];
        if (mode != "replace" && mode != "add" && mode != "subtract" && mode != "toggle") {
            return std::string("ERR invalid selection ") + (isMode ? "mode" : "operation");
        }
        if (!services_.selection.setSelectionMode ||
            !services_.selection.setSelectionMode(request.args[0], mode)) {
            return "ERR no active plot";
        }
        return "OK";
    }
    case CommandAction::AddLm: {
        if (request.args.size() < 2) return "ERR malformed ADD_LM command";
        const std::string &source = request.args[1];
        if (source != "all" && source != "selected" && source != "both" && source != "color") {
            return "ERR invalid overlay data source";
        }
        if (!services_.selection.addLinearModelOverlay) return std::nullopt;
        if (!services_.selection.addLinearModelOverlay(request.args[0], source)) {
            return "ERR no active plot";
        }
        RefreshRegisteredBasicPlotCodeReference(applicationState_, request.args[0]);
        if (services_.ui.redrawPlot) services_.ui.redrawPlot(request.args[0]);
        return "OK";
    }
    case CommandAction::ToggleLmAll:
    case CommandAction::ToggleLmSelected:
    case CommandAction::ToggleLmColor:
    case CommandAction::ToggleLmBoth: {
        if (request.args.empty()) return "ERR missing plot id";
        if (!services_.queries.plot || !services_.selection.addLinearModelOverlay ||
            !services_.selection.removeLinearModelOverlay) return std::nullopt;
        PlotModel plot;
        if (!services_.queries.plot(request.args[0], plot)) return "ERR no active plot";
        std::vector<std::string> sources;
        if (request.action == CommandAction::ToggleLmAll) sources = {"all"};
        else if (request.action == CommandAction::ToggleLmSelected) sources = {"selected"};
        else if (request.action == CommandAction::ToggleLmColor) sources = {"color"};
        else sources = {"all", "selected"};
        const bool remove = std::all_of(sources.begin(), sources.end(),
            [&](const std::string &source) { return HasOverlaySource(plot, source); });
        for (const std::string &source : sources) {
            if (remove) services_.selection.removeLinearModelOverlay(request.args[0], source);
            else services_.selection.addLinearModelOverlay(request.args[0], source);
        }
        RefreshRegisteredBasicPlotCodeReference(applicationState_, request.args[0]);
        if (services_.ui.redrawPlot) services_.ui.redrawPlot(request.args[0]);
        return "OK";
    }
    case CommandAction::ClearOverlays: {
        if (request.args.empty()) return "ERR missing plot id";
        if (!services_.selection.clearOverlays) return std::nullopt;
        if (!services_.selection.clearOverlays(request.args[0])) return "ERR no active plot";
        RefreshRegisteredBasicPlotCodeReference(applicationState_, request.args[0]);
        if (services_.ui.redrawPlot) services_.ui.redrawPlot(request.args[0]);
        return "OK";
    }
    case CommandAction::AddSmooth: {
        if (request.args.size() < 3) return "ERR malformed ADD_SMOOTH command";
        const std::string &plotId = request.args[0];
        if (!services_.queries.plot || !services_.selection.replaceSmoothCurves) {
            return std::nullopt;
        }
        PlotModel plot;
        if (!services_.queries.plot(plotId, plot)) return "ERR no active plot";
        SmoothCurveScope scope = SmoothCurveScope::Overall;
        if (request.args[1] == "selected") scope = SmoothCurveScope::Selection;
        else if (request.args[1] == "color") scope = SmoothCurveScope::ColorGroup;
        std::string fitMethod = "loess";
        std::size_t cursor = 3;
        int curveCount = std::atoi(request.args[2].c_str());
        if (request.args[2] == "FIT_CURVE_V2") {
            if (request.args.size() < 5) return "ERR malformed fitted curve header";
            fitMethod = request.args[3];
            if (fitMethod != "loess" && fitMethod != "lm")
                return "ERR unsupported fitted curve method";
            curveCount = std::atoi(request.args[4].c_str());
            cursor = 5;
        }
        if (curveCount < 0) return "ERR invalid curve count";
        std::vector<SmoothCurveData> curves;
        curves.reserve(static_cast<std::size_t>(curveCount));
        for (int i = 0; i < curveCount; ++i) {
            const bool versionTwo = fitMethod != "loess" ||
                (request.args.size() >= 3 && request.args[2] == "FIT_CURVE_V2");
            if (cursor + (versionTwo ? 6 : 4) >= request.args.size())
                return "ERR malformed curve data";
            SmoothCurveData curve;
            curve.scope = scope;
            curve.fitMethod = fitMethod;
            curve.groupId = request.args[cursor++];
            curve.ok = request.args[cursor++] == "1";
            const int pointCount = std::atoi(request.args[cursor++].c_str());
            if (pointCount < 0) return "ERR invalid point count in curve";
            std::istringstream xValues(request.args[cursor++]);
            std::istringstream yValues(request.args[cursor++]);
            std::istringstream lowerValues(versionTwo ? request.args[cursor++] : "");
            std::istringstream upperValues(versionTwo ? request.args[cursor++] : "");
            curve.x.reserve(static_cast<std::size_t>(pointCount));
            curve.y.reserve(static_cast<std::size_t>(pointCount));
            double x = 0.0;
            double y = 0.0;
            for (int j = 0; j < pointCount; ++j) {
                if (!(xValues >> x) || !(yValues >> y)) break;
                curve.x.push_back(x);
                curve.y.push_back(y);
            }
            if (static_cast<int>(curve.x.size()) != pointCount ||
                static_cast<int>(curve.y.size()) != pointCount) {
                return "ERR curve point count mismatch";
            }
            if (versionTwo) {
                double lower = 0.0, upper = 0.0;
                for (int j = 0; j < pointCount; ++j) {
                    if (!(lowerValues >> lower) || !(upperValues >> upper)) break;
                    curve.confidenceLower.push_back(lower);
                    curve.confidenceUpper.push_back(upper);
                }
                if ((!curve.confidenceLower.empty() || !curve.confidenceUpper.empty()) &&
                    (static_cast<int>(curve.confidenceLower.size()) != pointCount ||
                     static_cast<int>(curve.confidenceUpper.size()) != pointCount))
                    return "ERR curve confidence point count mismatch";
            }
            curves.push_back(std::move(curve));
        }
        // A zero-curve reply means that the option is still enabled but the
        // current selection/colour groups do not contain enough observations.
        // Keep that state so a later linked-state change can request it again.
        if (curves.empty()) curves.push_back(EnabledEmptySmoothCurve(scope, fitMethod));
        if (!services_.selection.replaceSmoothCurves(plotId, scope, curves)) {
            return "ERR plot went away";
        }
        if (services_.ui.redrawPlot) services_.ui.redrawPlot(plotId);
        return "OK";
    }
    case CommandAction::AddTrellisSmooth: {
        if (request.args.size() < 4) return "ERR malformed ADD_TRELLIS_SMOOTH command";
        const std::string &plotId = request.args[0];
        const std::string &panelId = request.args[1];
        if (!services_.queries.plot || !services_.selection.replaceTrellisSmoothCurves) {
            return std::nullopt;
        }
        PlotModel plot;
        if (!services_.queries.plot(plotId, plot)) return "ERR no active plot";
        SmoothCurveScope scope = SmoothCurveScope::Overall;
        if (request.args[2] == "selected") scope = SmoothCurveScope::Selection;
        else if (request.args[2] == "color") scope = SmoothCurveScope::ColorGroup;
        else if (request.args[2] != "overall") return "ERR invalid smooth scope";
        std::string fitMethod = "loess";
        std::size_t cursor = 4;
        int curveCount = std::atoi(request.args[3].c_str());
        if (request.args[3] == "FIT_CURVE_V2") {
            if (request.args.size() < 6) return "ERR malformed trellis fitted curve header";
            fitMethod = request.args[4];
            if (fitMethod != "loess" && fitMethod != "lm")
                return "ERR unsupported trellis fitted curve method";
            curveCount = std::atoi(request.args[5].c_str());
            cursor = 6;
        }
        if (curveCount < 0) return "ERR invalid curve count";
        std::vector<SmoothCurveData> curves;
        curves.reserve(static_cast<std::size_t>(curveCount));
        for (int i = 0; i < curveCount; ++i) {
            const bool versionTwo = fitMethod != "loess" ||
                (request.args.size() >= 4 && request.args[3] == "FIT_CURVE_V2");
            if (cursor + (versionTwo ? 6 : 4) >= request.args.size())
                return "ERR malformed curve data";
            SmoothCurveData curve;
            curve.scope = scope;
            curve.fitMethod = fitMethod;
            curve.groupId = request.args[cursor++];
            curve.ok = request.args[cursor++] == "1";
            const int pointCount = std::atoi(request.args[cursor++].c_str());
            if (pointCount < 0) return "ERR invalid point count in curve";
            std::istringstream xValues(request.args[cursor++]);
            std::istringstream yValues(request.args[cursor++]);
            std::istringstream lowerValues(versionTwo ? request.args[cursor++] : "");
            std::istringstream upperValues(versionTwo ? request.args[cursor++] : "");
            curve.x.reserve(static_cast<std::size_t>(pointCount));
            curve.y.reserve(static_cast<std::size_t>(pointCount));
            double x = 0.0;
            double y = 0.0;
            for (int j = 0; j < pointCount; ++j) {
                if (!(xValues >> x) || !(yValues >> y)) break;
                curve.x.push_back(x);
                curve.y.push_back(y);
            }
            if (static_cast<int>(curve.x.size()) != pointCount ||
                static_cast<int>(curve.y.size()) != pointCount) {
                return "ERR curve point count mismatch";
            }
            if (versionTwo) {
                double lower = 0.0, upper = 0.0;
                for (int j = 0; j < pointCount; ++j) {
                    if (!(lowerValues >> lower) || !(upperValues >> upper)) break;
                    curve.confidenceLower.push_back(lower);
                    curve.confidenceUpper.push_back(upper);
                }
                if ((!curve.confidenceLower.empty() || !curve.confidenceUpper.empty()) &&
                    (static_cast<int>(curve.confidenceLower.size()) != pointCount ||
                     static_cast<int>(curve.confidenceUpper.size()) != pointCount))
                    return "ERR curve confidence point count mismatch";
            }
            curves.push_back(std::move(curve));
        }
        if (curves.empty()) curves.push_back(EnabledEmptySmoothCurve(scope, fitMethod));
        if (!services_.selection.replaceTrellisSmoothCurves(plotId, panelId, scope, curves)) {
            return "ERR plot went away";
        }
        if (services_.ui.redrawPlot) services_.ui.redrawPlot(plotId);
        return "OK";
    }
    case CommandAction::RequestSmooth: {
        if (request.args.size() < 2) return "ERR malformed REQUEST_SMOOTH command";
        SmoothCurveScope scope = SmoothCurveScope::Overall;
        if (request.args[1] == "selected") scope = SmoothCurveScope::Selection;
        else if (request.args[1] == "color") scope = SmoothCurveScope::ColorGroup;
        else if (request.args[1] != "overall") return "ERR invalid smooth scope";
        if (!services_.selection.toggleSmoothCurves) return std::nullopt;
        if (!services_.selection.toggleSmoothCurves(request.args[0], scope)) return "ERR no active plot";
        if (services_.ui.redrawPlot) services_.ui.redrawPlot(request.args[0]);
        return "OK";
    }
    case CommandAction::ToggleSmoothOverall:
    case CommandAction::ToggleSmoothSelected:
    case CommandAction::ToggleSmoothColor: {
        if (request.args.empty()) return "ERR missing plot id";
        if (!services_.selection.toggleSmoothCurves) return std::nullopt;
        SmoothCurveScope scope = SmoothCurveScope::Overall;
        if (request.action == CommandAction::ToggleSmoothSelected)
            scope = SmoothCurveScope::Selection;
        else if (request.action == CommandAction::ToggleSmoothColor)
            scope = SmoothCurveScope::ColorGroup;
        if (!services_.selection.toggleSmoothCurves(request.args[0], scope))
            return "ERR no active plot";
        if (services_.ui.redrawPlot) services_.ui.redrawPlot(request.args[0]);
        return "OK";
    }
    case CommandAction::SmoothInfo: {
        if (request.args.empty()) return "ERR missing plot id";
        if (!services_.queries.plot) return std::nullopt;
        PlotModel plot;
        if (!services_.queries.plot(request.args[0], plot)) return "ERR no active plot";
        std::ostringstream out;
        out << "OK";
        for (const SmoothCurveData &curve : plot.smoothCurves) {
            std::string scope = "overall";
            if (curve.scope == SmoothCurveScope::Selection) scope = "selected";
            else if (curve.scope == SmoothCurveScope::ColorGroup) scope = "color";
            out << "\t" << scope << "|" << (curve.ok ? "1" : "0") << "|"
                << curve.groupId << "|" << curve.x.size();
        }
        return out.str();
    }
    case CommandAction::RequestCompareMeans: {
        if (request.args.size() < 3) return "ERR malformed REQUEST_COMPARE_MEANS command";
        if (!services_.ui.queueCompareMeansTask) return std::nullopt;
        if (!applicationState_.datasets().contains(request.args[0])) {
            return "ERR dataset is not registered";
        }
        services_.ui.queueCompareMeansTask(MainRCompareMeansTask{
            request.args[0], request.args[1], request.args[2],
            request.args.size() >= 4 ? request.args[3] : "",
            request.args.size() >= 5 ? request.args[4] : ""});
        return "OK";
    }
    case CommandAction::MainRTasks:
        return "OK";
    case CommandAction::ModelUpdateError: {
        if (request.args.size() < 2) return "ERR malformed MODEL_UPDATE_ERROR command";
        if (!services_.queries.groupSeed || !services_.ui.refreshModelGroup) return std::nullopt;
        const std::string group = request.args[0];
        std::string modelId = group;
        for (std::size_t index = 2; index + 1 < request.args.size(); ++index) {
            if (request.args[index] == "LINEAR_MODEL_ID_V1") {
                modelId = request.args[index + 1];
                break;
            }
        }
        PlotModel seed;
        if (!services_.queries.groupSeed(group, seed)) {
            return "ERR no registered dataset/group for linear model";
        }
        auto existing = applicationState_.groupModels().find(modelId);
        if (modelId != group && existing == applicationState_.groupModels().end()) return "OK";
        GroupModelState &state = EnsureGroupModelState(applicationState_.groupModels(), modelId, &seed);
        if (!state.autoRefit && !state.rFitPending) return "OK\tglm:" + modelId;
        std::string resultIdentity;
        if (request.args.size() >= 4 && request.args[2] == "LINEAR_RESULT_V1") {
            resultIdentity = request.args[3];
        }
        if (!resultIdentity.empty() &&
            (!state.rFitPending || resultIdentity != state.lastRFitSignature)) {
            return "OK\tglm:" + modelId;
        }
        state.rFitPending = false;
        state.isStale = false;
        GLMFitSummary fit;
        fit.ok = false;
        fit.warning = request.args[1];
        applicationState_.linearModelFits()[modelId] = fit;
        services_.ui.refreshModelGroup(modelId);
        return "OK\tglm:" + modelId;
    }
    case CommandAction::ModelUpdate: {
        if (request.args.size() < 6) return "ERR malformed MODEL_UPDATE command";
        if (!services_.queries.groupSeed || !services_.ui.modelUpdated) return std::nullopt;
        std::size_t cursor = 0;
        const std::string group = request.args[cursor++];
        const std::string dependent = request.args[cursor++];
        const std::string scope = request.args[cursor++];
        const long termCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
        if (termCount < 0 || cursor + static_cast<std::size_t>(termCount) > request.args.size()) {
            return "ERR invalid model update term count";
        }
        std::vector<std::string> terms;
        for (long i = 0; i < termCount; ++i) terms.push_back(request.args[cursor++]);
        GLMFitSummary fit;
        if (!ReadLinearFitPayload(request.args, cursor, fit)) return "ERR malformed linear model fit payload";
        if (!ReadLinearDiagnosticsPayload(request.args, cursor, fit.diagnostics)) {
            return "ERR malformed linear model diagnostic payload";
        }
        if (!ReadLinearDesignPayload(request.args, cursor, fit)) return "ERR malformed linear model design payload";
        if (!ReadLinearMIDiagnosticsPayload(request.args, cursor, fit)) {
            return "ERR malformed linear model MI diagnostic payload";
        }
        std::string resultIdentity;
        if (cursor < request.args.size() && request.args[cursor] == "LINEAR_RESULT_V1") {
            ++cursor;
            if (cursor >= request.args.size()) {
                return "ERR missing linear model result identity";
            }
            resultIdentity = request.args[cursor++];
        }
        std::string modelId = group;
        if (cursor + 1 < request.args.size() && request.args[cursor] == "LINEAR_MODEL_ID_V1") {
            modelId = request.args[cursor + 1];
            cursor += 2;
        }
        AnalysisProvenance returnedProvenance;
        std::string provenanceError;
        if (!ReadAnalysisProvenancePayload(
                request.args, cursor, returnedProvenance, &provenanceError)) {
            return "ERR malformed linear model provenance: " + provenanceError;
        }
        PlotModel seed;
        if (!services_.queries.groupSeed(group, seed)) return "ERR no registered dataset/group for linear model";
        auto existing = applicationState_.groupModels().find(modelId);
        if (modelId != group && existing == applicationState_.groupModels().end()) return "OK";
        GroupModelState &state = EnsureGroupModelState(applicationState_.groupModels(), modelId, &seed);
        if (!state.autoRefit && !state.rFitPending) return "OK\tglm:" + modelId;
        if (const DataFrameModel *dataframe = applicationState_.datasets().find(group)) {
            SynchronizeGroupModelTermTypes(state, *dataframe);
        }
        const auto effectiveTermTypes =
            EffectiveModelSpecificationTermTypes(state);
        const std::string signature = LinearGLMFitSignature(
            dependent, terms, effectiveTermTypes, scope, state.centeredPredictors,
            state.factorReferenceLevels);
        if (!resultIdentity.empty()) {
            if (!state.rFitPending || resultIdentity != state.lastRFitSignature) {
                return "OK\tglm:" + modelId;
            }
        } else if (state.rFitPending && !state.lastRFitSignature.empty() &&
                   signature != state.lastRFitSignature) {
            return "OK\tglm:" + modelId;
        }
        // A type change can happen while R is still calculating the previous
        // model.  Do not let that old numeric/factor result overwrite the new
        // canonical specification merely because response and terms match.
        if (!LinearGLMFitMatchesTermTypes(fit, terms, effectiveTermTypes)) {
            // At this point an explicitly versioned result has already been
            // proven to belong to the current request.  Silently discarding
            // it used to leave rFitPending set forever, so the native table
            // continued displaying rows from the previous specification
            // (for example `region` after replacing it with `coastal`).
            // Finish the request as a visible semantic error.  Results from
            // genuinely older identities are still rejected above without
            // touching the current request.
            state.rFitPending = false;
            state.isStale = false;
            GLMFitSummary rejected;
            rejected.ok = false;
            rejected.warning =
                "The returned linear-model terms or types did not match the exact current model specification.";
            applicationState_.linearModelFits()[modelId] = rejected;
            services_.ui.modelUpdated(modelId, {});
            return "OK\tglm:" + modelId;
        }
        state.response = dependent;
        state.terms = terms;
        state.scope = scope;
        const bool multipleImputation = state.multipleImputation ||
            !fit.diagnosticsByImputation.empty();
        state.precomputed = multipleImputation;
        state.multipleImputation = multipleImputation;
        if (multipleImputation && !fit.diagnosticsByImputation.empty()) {
            state.imputationCount = static_cast<int>(fit.diagnosticsByImputation.size());
        } else if (!multipleImputation) {
            state.imputationCount = 0;
        }
        // Pooled MI windows carry an explicit analysis title/note that must
        // survive an ordinary specification refit.  Clearing it here made a
        // successfully refreshed MI model look like a non-MI model and also
        // hid its imputation identity from diagnostic windows.
        if (!multipleImputation) {
            if (state.modelId == state.group) state.title.clear();
            state.note.clear();
        }
        state.isStale = false;
        state.rFitPending = false;
        state.lastRFitSignature = resultIdentity.empty() ? signature : resultIdentity;
        ++state.fitVersion;
        ++state.diagnosticsVersion;
        state.diagnostics = fit.diagnostics;
        state.rowsUsed = fit.rowsUsed;
        state.rowsExcluded = fit.rowsExcluded;
        if (const DataFrameModel *dataframe = applicationState_.datasets().find(group);
            dataframe && !returnedProvenance.executedRCode.empty()) {
            if (!state.dataScopeCaptured) {
                if (scope == "all") {
                    state.dataScope = AllObservationsAnalysisScope(
                        group, static_cast<std::size_t>(dataframe->rows));
                } else {
                    std::vector<int> requested = fit.rowsUsed;
                    requested.insert(requested.end(), fit.rowsExcluded.begin(), fit.rowsExcluded.end());
                    state.dataScope = ExplicitAnalysisScope(
                        group, requested,
                        scope == "selected" ? AnalysisScopeSourceKind::CurrentSelection :
                            AnalysisScopeSourceKind::OtherExplicitSubset,
                        scope == "selected" ? "Selected observations" : "Unselected observations",
                        static_cast<std::size_t>(dataframe->rows));
                }
                state.dataScopeCaptured = true;
            }
            CompleteAnalysisProvenance(
                returnedProvenance, *dataframe, state.dataScope,
                fit.rowsUsed, fit.rowsExcluded, "glm:" + modelId,
                state.title.empty() ? "General Linear Model" : state.title);
            state.provenance = std::move(returnedProvenance);
            applicationState_.registerOutputCodeReference(
                LinearModelCodeReference(state, fit));
        }
        applicationState_.linearModelFits()[modelId] = fit;
        services_.ui.modelUpdated(modelId, fit.rowsUsed);
        return "OK\tglm:" + modelId;
    }
    case CommandAction::ModelTrellisUpdateError: {
        if (request.args.size() < 3) return "ERR malformed MODEL_TRELLIS_UPDATE_ERROR command";
        auto found = applicationState_.modelTrellises().find(request.args[0]);
        if (found == applicationState_.modelTrellises().end()) return "OK";
        const int generation = std::atoi(request.args[1].c_str());
        if (generation != found->second.specificationGeneration) return "OK";
        found->second.fitPending = false;
        found->second.status = request.args[2];
        if (services_.ui.refreshModelTrellis) services_.ui.refreshModelTrellis(request.args[0]);
        return "OK\tmodel_trellis:" + request.args[0];
    }
    case CommandAction::ModelTrellisUpdate: {
        if (request.args.size() < 3) return "ERR malformed MODEL_TRELLIS_UPDATE command";
        auto found = applicationState_.modelTrellises().find(request.args[0]);
        if (found == applicationState_.modelTrellises().end()) return "OK";
        const int generation = std::atoi(request.args[1].c_str());
        std::size_t cursor = 2;
        std::string error;
        if (!ApplyModelTrellisUpdatePayload(found->second, generation, request.args, cursor, &error)) {
            return "ERR " + error;
        }
        if (services_.ui.refreshModelTrellis) services_.ui.refreshModelTrellis(request.args[0]);
        return "OK\tmodel_trellis:" + request.args[0];
    }
    case CommandAction::RegressionComparisonUpdateError: {
        if (request.args.size() < 2) return "ERR malformed REGCMP_UPDATE_ERROR command";
        const bool versioned = request.args.size() >= 3;
        const int generation = versioned ? std::atoi(request.args[1].c_str()) : 0;
        const std::string &message = request.args[versioned ? 2 : 1];
        auto it = applicationState_.regressionComparisons().find(request.args[0]);
        if (it != applicationState_.regressionComparisons().end()) {
            if (generation > 0 &&
                (generation != it->second.rFitGeneration || !it->second.rFitPending)) {
                return "OK\t" + request.args[0];
            }
            it->second.rFitPending = false;
            for (RegressionComparisonModel &model : it->second.models) {
                model.isStale = false;
                model.fit.ok = false;
                model.fit.warning = message;
                model.fitState = RegressionComparisonFitState::Error;
            }
            if (services_.ui.refreshRegressionComparison) {
                services_.ui.refreshRegressionComparison(it->second.group);
            }
        }
        return "OK\t" + request.args[0];
    }
    case CommandAction::RegressionComparisonUpdate: {
        if (request.args.size() < 9) return "ERR malformed REGCMP_UPDATE command";
        if (!services_.queries.groupSeed || !services_.ui.regressionComparisonUpdated) return std::nullopt;
        std::size_t cursor = 0;
        RegressionComparisonState state;
        state.id = request.args[cursor++];
        state.rFitGeneration = std::atoi(request.args[cursor++].c_str());
        state.group = request.args[cursor++];
        state.response = request.args[cursor++];
        state.scope = request.args[cursor++];
        state.autoRefit = request.args[cursor++] != "FALSE";
        if (!services_.queries.groupSeed(state.group, state.seed)) {
            return "ERR no registered dataset/group for regression comparison";
        }
        state.hasSeed = true;
        const long termRows = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
        if (termRows < 0 || cursor + static_cast<std::size_t>(termRows) > request.args.size()) {
            return "ERR invalid regression comparison term-row count";
        }
        for (long i = 0; i < termRows; ++i) state.termRows.push_back(request.args[cursor++]);
        if (cursor >= request.args.size()) return "ERR missing regression comparison type count";
        const long typeCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
        if (typeCount < 0 || cursor + static_cast<std::size_t>(2 * typeCount) > request.args.size()) {
            return "ERR invalid regression comparison type payload";
        }
        for (long i = 0; i < typeCount; ++i) {
            const std::string variable = request.args[cursor++];
            const std::string type = request.args[cursor++];
            state.termTypes[variable] = type;
        }
        if (cursor >= request.args.size()) return "ERR missing regression comparison model count";
        const long modelCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
        if (modelCount < 0) return "ERR invalid regression comparison model count";
        for (long i = 0; i < modelCount; ++i) {
            if (cursor + 4 > request.args.size()) return "ERR malformed regression comparison model";
            RegressionComparisonModel model;
            model.id = request.args[cursor++];
            model.label = request.args[cursor++];
            model.response = request.args[cursor++];
            const long terms = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (terms < 0 || cursor + static_cast<std::size_t>(terms) > request.args.size()) {
                return "ERR invalid regression comparison model term count";
            }
            for (long j = 0; j < terms; ++j) model.terms.push_back(request.args[cursor++]);
            model.candidateTerms = model.terms;
            if (cursor >= request.args.size()) return "ERR missing regression comparison model type count";
            const long modelTypeCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (modelTypeCount < 0 || cursor + static_cast<std::size_t>(2 * modelTypeCount) > request.args.size()) {
                return "ERR invalid regression comparison model type payload";
            }
            for (long j = 0; j < modelTypeCount; ++j) {
                const std::string variable = request.args[cursor++];
                const std::string type = request.args[cursor++];
                model.termTypeOverrides[variable] = type;
            }
            if (cursor >= request.args.size()) return "ERR missing regression comparison centered-predictor count";
            const long centeredCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (centeredCount < 0 || cursor + static_cast<std::size_t>(centeredCount) > request.args.size()) {
                return "ERR invalid regression comparison centered-predictor payload";
            }
            for (long j = 0; j < centeredCount; ++j) model.centeredPredictors.insert(request.args[cursor++]);
            if (!ReadLinearFitPayload(request.args, cursor, model.fit)) return "ERR malformed regression comparison fit payload";
            if (!ReadLinearDiagnosticsPayload(request.args, cursor, model.fit.diagnostics)) return "ERR malformed regression comparison diagnostic payload";
            if (!ReadLinearDesignPayload(request.args, cursor, model.fit)) return "ERR malformed regression comparison design payload";
            if (!ReadLinearMIDiagnosticsPayload(request.args, cursor, model.fit)) return "ERR malformed regression comparison MI diagnostic payload";
            if (cursor + 6 > request.args.size()) return "ERR malformed regression comparison test payload";
            model.comparisonOk = request.args[cursor++] == "TRUE";
            model.comparisonDf = ParseOptionalDataCellInt(request.args[cursor++]);
            model.comparisonDf2 = ParseOptionalDataCellDouble(request.args[cursor++]);
            model.comparisonDelta = ParseOptionalDataCellDouble(request.args[cursor++]);
            model.comparisonStatistic = ParseOptionalDataCellDouble(request.args[cursor++]);
            model.comparisonP = ParseOptionalDataCellDouble(request.args[cursor++]);
            model.isStale = false;
            model.fitVersion = 1;
            model.fitState = model.fit.ok
                ? RegressionComparisonFitState::Valid
                : RegressionComparisonFitState::Error;
            state.models.push_back(std::move(model));
        }
        {
            std::string provenanceError;
            if (!ReadAnalysisProvenancePayload(
                    request.args, cursor, state.provenance, &provenanceError)) {
                return "ERR " + provenanceError;
            }
        }
        auto &comparisons = applicationState_.regressionComparisons();
        auto old = comparisons.find(state.id);
        if (old != comparisons.end()) {
            state.activeModel = old->second.activeModel;
            state.title = old->second.title;
            state.note = old->second.note;
            state.dataScope = old->second.dataScope;
            state.dataScopeCaptured = old->second.dataScopeCaptured;
            state.showInformationCriteria = old->second.showInformationCriteria;
            state.multipleImputation = old->second.multipleImputation;
            state.imputationCount = old->second.imputationCount;
            if (state.provenance.executedRCode.empty())
                state.provenance = old->second.provenance;
            // The native state is the owner of the visible insertion order.
            // R returns fitted values for that specification, but its payload
            // must not reorder rows while expanding coefficient children.
            state.termRows = old->second.termRows;
            if (state.termTypes.empty()) state.termTypes = old->second.termTypes;
            if (state.rFitGeneration > 0) {
                // A versioned result belongs to the current specification when
                // its generation is still pending.  The R payload deliberately
                // expands inherited term types (for example, a local sparse
                // override becomes `planet = factor`), so comparing that wire
                // representation byte-for-byte with the UI specification would
                // reject a valid fit.  Edits either queue a newer generation or,
                // with Auto-refit disabled, clear rFitPending; both cases still
                // reject the late result without relying on representation.
                if (state.rFitGeneration != old->second.rFitGeneration ||
                    !old->second.rFitPending) {
                    return "OK\t" + state.id;
                }
            } else {
                const std::string signature = RegressionComparisonFitSignature(state);
                if (old->second.rFitPending && !old->second.lastRFitSignature.empty() &&
                    signature != old->second.lastRFitSignature) return "OK\t" + state.id;
            }
            // A result is a fit of a specification, not a replacement for it.
            // Reject a late result if the user edited that model while the R
            // request was in flight (notably with Auto-refit disabled), and
            // retain the canonical per-model override representation.
            for (RegressionComparisonModel &model : state.models) {
                auto previous = std::find_if(old->second.models.begin(), old->second.models.end(),
                    [&](RegressionComparisonModel const &candidate) { return candidate.id == model.id; });
                if (previous == old->second.models.end()) continue;
                const auto previousTypes = RegressionComparisonModelTermTypes(old->second, *previous);
                const std::string previousResponse = previous->response.empty()
                    ? old->second.response : previous->response;
                const std::string resultResponse = model.response.empty()
                    ? state.response : model.response;
                if (state.rFitGeneration <= 0) {
                    if (resultResponse != previousResponse ||
                        model.terms != previous->terms ||
                        model.centeredPredictors != previous->centeredPredictors ||
                        model.termTypeOverrides != previousTypes) {
                        return "OK\t" + state.id;
                    }
                }
                model.termTypeOverrides = previous->termTypeOverrides;
                model.centeredPredictors = previous->centeredPredictors;
                model.factorReferenceLevels = previous->factorReferenceLevels;
                model.candidateTerms = previous->candidateTerms;
                for (const std::string &term : model.terms) {
                    if (!TermListContainsEquivalentModelTerm(model.candidateTerms, term)) {
                        model.candidateTerms.push_back(term);
                    }
                }
                model.modelVersion = previous->modelVersion;
                model.fitVersion = previous->modelVersion;
            }
        }
        InferRegressionComparisonTermTypesFromFitRows(state);
        RefreshRegressionTermRowsFromFits(state);
        PruneRegressionComparisonRowsForCurrentTypes(state);
        state.rFitPending = false;
        state.lastRFitSignature = RegressionComparisonFitSignature(state);
        if (const DataFrameModel *dataframe = applicationState_.datasets().find(state.group);
            dataframe && !state.provenance.executedRCode.empty()) {
            const AnalysisScope scope = state.dataScopeCaptured
                ? state.dataScope : applicationState_.activeAnalysisScope(state.group);
            const std::vector<int> used = state.models.empty()
                ? std::vector<int>{} : state.models.front().fit.rowsUsed;
            const std::vector<int> excluded = state.models.empty()
                ? std::vector<int>{} : state.models.front().fit.rowsExcluded;
            CompleteAnalysisProvenance(state.provenance, *dataframe, scope,
                used, excluded, state.id, "Compare Linear Models");
        }
        comparisons[state.id] = state;
        if (!state.provenance.executedRCode.empty())
            applicationState_.registerOutputCodeReference(
                RegressionComparisonCodeReference(state));
        services_.ui.regressionComparisonUpdated(state);
        return "OK\t" + state.id;
    }
    case CommandAction::RegressionComparisonOpenPooled: {
        if (request.args.size() < 10) return "ERR malformed REGCMP_OPEN_POOLED command";
        if (!services_.queries.groupSeed || !services_.ui.regressionComparisonUpdated) return std::nullopt;
        std::size_t cursor = 0;
        RegressionComparisonState state;
        state.id = request.args[cursor++]; state.group = request.args[cursor++];
        state.response = request.args[cursor++]; state.scope = request.args[cursor++];
        // A pooled result must not reset a user's Auto-refit choice. New
        // comparison windows default to Auto-refit on; subsequent R replies
        // preserve the current local setting.
        const auto existing = applicationState_.regressionComparisons().find(state.id);
        state.autoRefit = existing == applicationState_.regressionComparisons().end()
            ? true : existing->second.autoRefit;
        state.precomputed = true; state.multipleImputation = true;
        state.imputationCount = std::atoi(request.args[cursor++].c_str());
        state.title = request.args[cursor++]; state.note = request.args[cursor++];
        if (!services_.queries.groupSeed(state.group, state.seed)) return "ERR no registered dataset/group for pooled regression comparison";
        state.hasSeed = true;
        const long rows = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
        if (rows < 0 || cursor + static_cast<std::size_t>(rows) > request.args.size()) return "ERR invalid pooled comparison term count";
        for (long i = 0; i < rows; ++i) state.termRows.push_back(request.args[cursor++]);
        if (cursor >= request.args.size()) return "ERR invalid pooled comparison model count";
        const long models = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
        if (models < 0) return "ERR invalid pooled comparison model count";
        for (long i = 0; i < models; ++i) {
            if (cursor + 4 > request.args.size()) return "ERR malformed pooled comparison model";
            RegressionComparisonModel model;
            model.id = request.args[cursor++]; model.label = request.args[cursor++]; model.response = request.args[cursor++];
            const long terms = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (terms < 0 || cursor + static_cast<std::size_t>(terms) > request.args.size()) return "ERR invalid pooled comparison model term count";
            for (long j = 0; j < terms; ++j) model.terms.push_back(request.args[cursor++]);
            model.candidateTerms = model.terms;
            if (cursor >= request.args.size()) return "ERR missing pooled comparison model type count";
            const long modelTypeCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (modelTypeCount < 0 ||
                cursor + static_cast<std::size_t>(2 * modelTypeCount) > request.args.size()) {
                return "ERR invalid pooled comparison model type payload";
            }
            for (long j = 0; j < modelTypeCount; ++j) {
                const std::string variable = request.args[cursor++];
                const std::string type = request.args[cursor++];
                model.termTypeOverrides[variable] = type;
            }
            if (cursor >= request.args.size()) return "ERR missing pooled comparison centered-predictor count";
            const long centeredCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (centeredCount < 0 ||
                cursor + static_cast<std::size_t>(centeredCount) > request.args.size()) {
                return "ERR invalid pooled comparison centered-predictor payload";
            }
            for (long j = 0; j < centeredCount; ++j) {
                model.centeredPredictors.insert(request.args[cursor++]);
            }
            if (!ReadLinearFitPayload(request.args, cursor, model.fit)) return "ERR malformed pooled comparison fit payload";
            // Legacy pooled payloads placed the comparison-test boolean
            // directly after the fit. Current payloads include concrete
            // per-imputation diagnostics and design information first.
            if (cursor < request.args.size() && request.args[cursor] != "TRUE" &&
                request.args[cursor] != "FALSE") {
                if (!ReadLinearDiagnosticsPayload(request.args, cursor, model.fit.diagnostics)) return "ERR malformed pooled comparison diagnostic payload";
                if (!ReadLinearDesignPayload(request.args, cursor, model.fit)) return "ERR malformed pooled comparison design payload";
                if (!ReadLinearMIDiagnosticsPayload(request.args, cursor, model.fit)) return "ERR malformed pooled comparison MI diagnostic payload";
            }
            if (cursor + 6 > request.args.size()) return "ERR malformed pooled comparison test payload";
            model.comparisonOk = request.args[cursor++] == "TRUE"; model.comparisonDf = ParseOptionalDataCellInt(request.args[cursor++]);
            model.comparisonDf2 = ParseOptionalDataCellDouble(request.args[cursor++]); model.comparisonDelta = ParseOptionalDataCellDouble(request.args[cursor++]);
            model.comparisonStatistic = ParseOptionalDataCellDouble(request.args[cursor++]); model.comparisonP = ParseOptionalDataCellDouble(request.args[cursor++]);
            model.isStale = false; model.fitVersion = 1;
            model.fitState = model.fit.ok
                ? RegressionComparisonFitState::Valid
                : RegressionComparisonFitState::Error;
            state.models.push_back(std::move(model));
        }
        {
            std::string provenanceError;
            if (!ReadAnalysisProvenancePayload(
                    request.args, cursor, state.provenance, &provenanceError)) {
                return "ERR " + provenanceError;
            }
        }
        if (existing != applicationState_.regressionComparisons().end()) {
            state.activeModel = existing->second.activeModel;
            state.showInformationCriteria = existing->second.showInformationCriteria;
            state.dataScope = existing->second.dataScope;
            state.dataScopeCaptured = existing->second.dataScopeCaptured;
            if (state.provenance.executedRCode.empty())
                state.provenance = existing->second.provenance;
            for (RegressionComparisonModel &model : state.models) {
                const auto previous = std::find_if(
                    existing->second.models.begin(), existing->second.models.end(),
                    [&](const RegressionComparisonModel &candidate) {
                        return candidate.id == model.id;
                    });
                if (previous == existing->second.models.end()) continue;
                model.candidateTerms = previous->candidateTerms;
                for (const std::string &term : model.terms) {
                    if (!TermListContainsEquivalentModelTerm(model.candidateTerms, term)) {
                        model.candidateTerms.push_back(term);
                    }
                }
            }
        }
        RefreshRegressionTermRowsFromFits(state);
        PruneRegressionComparisonRowsForCurrentTypes(state);
        if (const DataFrameModel *dataframe = applicationState_.datasets().find(state.group);
            dataframe && !state.provenance.executedRCode.empty()) {
            const AnalysisScope scope = state.dataScopeCaptured
                ? state.dataScope : applicationState_.activeAnalysisScope(state.group);
            const std::vector<int> used = state.models.empty()
                ? std::vector<int>{} : state.models.front().fit.rowsUsed;
            const std::vector<int> excluded = state.models.empty()
                ? std::vector<int>{} : state.models.front().fit.rowsExcluded;
            CompleteAnalysisProvenance(state.provenance, *dataframe, scope,
                used, excluded, state.id, "Compare Linear Models");
        }
        applicationState_.regressionComparisons()[state.id] = state;
        if (!state.provenance.executedRCode.empty())
            applicationState_.registerOutputCodeReference(
                RegressionComparisonCodeReference(state));
        services_.ui.regressionComparisonUpdated(state);
        return "OK\t" + state.id;
    }
    case CommandAction::RegressionComparisonOpen:
    case CommandAction::RegressionComparisonOpen2: {
        const bool separateResponses = request.action == CommandAction::RegressionComparisonOpen2;
        if (request.args.size() < 6) {
            return separateResponses ? "ERR malformed REGCMP_OPEN2 command"
                                     : "ERR malformed REGCMP_OPEN command";
        }
        if (!services_.queries.groupSeed || !services_.ui.requestRegressionComparisonFit ||
            !services_.ui.showRegressionComparison) return std::nullopt;
        RegressionComparisonState state;
        state.id = request.args[0];
        state.group = request.args[1];
        state.response = request.args[2];
        state.scope = request.args[3];
        state.autoRefit = request.args[4] != "FALSE";
        if (state.scope != "all" && state.scope != "selected" && state.scope != "unselected") {
            return "ERR invalid comparison scope";
        }
        if (!services_.queries.groupSeed(state.group, state.seed)) {
            return "ERR no registered dataset/group for comparison";
        }
        state.hasSeed = true;
        if (!FindNumericVariable(state.seed, state.response)) {
            return "ERR response variable is not available";
        }
        state.termRows.push_back("(Intercept)");
        const long modelCount = std::strtol(request.args[5].c_str(), nullptr, 10);
        if (modelCount < 0) return "ERR invalid comparison model count";
        std::size_t cursor = 6;
        const std::vector<std::string> available = AvailableVariableNames(state.seed);
        for (long modelIndex = 0; modelIndex < modelCount; ++modelIndex) {
            if (cursor + (separateResponses ? 2U : 1U) >= request.args.size()) {
                return "ERR malformed comparison model payload";
            }
            RegressionComparisonModel model;
            model.id = state.id + ":model:" + std::to_string(modelIndex + 1);
            model.label = request.args[cursor++];
            model.response = separateResponses ? request.args[cursor++] : state.response;
            if (model.label.empty()) model.label = NextUntitledRegressionLabel(state);
            if (!FindNumericVariable(state.seed, model.response)) {
                return "ERR model response variable is not available";
            }
            const long termCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (termCount < 0 || cursor + static_cast<std::size_t>(termCount) > request.args.size()) {
                return "ERR invalid comparison term count";
            }
            for (long termIndex = 0; termIndex < termCount; ++termIndex) {
                const std::string &term = request.args[cursor++];
                if (!ModelTermExistsForVariables(available, term, model.response)) continue;
                for (const std::string &candidate :
                     HierarchicalTermsForModelTerm(available, model.response, term)) {
                    AddRegressionTermRow(state, candidate);
                    if (!ModelIncludesTerm(model, candidate)) model.terms.push_back(candidate);
                }
            }
            model.candidateTerms = model.terms;
            state.models.push_back(std::move(model));
        }
        if (state.models.empty()) {
            RegressionComparisonModel model;
            model.id = state.id + ":model:1";
            model.label = "Model 1";
            model.response = state.response;
            state.models.push_back(std::move(model));
        }
        auto &stored = applicationState_.regressionComparisons()[state.id] = std::move(state);
        services_.ui.requestRegressionComparisonFit(stored);
        services_.ui.showRegressionComparison(stored.id);
        return "OK\t" + stored.id;
    }
    case CommandAction::RegressionComparisonInfo: {
        if (request.args.empty()) return "ERR missing comparison id";
        auto it = applicationState_.regressionComparisons().find(request.args[0]);
        if (it == applicationState_.regressionComparisons().end()) return "ERR unknown comparison";
        const RegressionComparisonState &state = it->second;
        std::ostringstream out;
        out << "OK\tCOMPARISON|" << state.id << "|" << state.group << "|" << state.response << "|"
            << state.scope << "|" << state.models.size() << "|" << state.termRows.size() << "\tTERMS";
        for (const std::string &term : state.termRows) out << "|" << term;
        for (const RegressionComparisonModel &model : state.models) {
            out << "\tMODEL|" << model.id << "|" << model.label << "|" << model.modelVersion
                << "|" << model.fitVersion << "|" << (model.fit.ok ? "OK" : "STALE")
                << "|" << model.terms.size();
            for (const std::string &term : model.terms) out << "|" << term;
        }
        return out.str();
    }
    case CommandAction::RegressionComparisonCell: {
        if (request.args.size() < 3) return "ERR malformed REGCMP_CELL command";
        auto it = applicationState_.regressionComparisons().find(request.args[0]);
        if (it == applicationState_.regressionComparisons().end()) return "ERR unknown comparison";
        const int modelIndex = std::atoi(request.args[1].c_str()) - 1;
        if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= it->second.models.size()) {
            return "ERR invalid model index";
        }
        const RegressionComparisonModel &model = it->second.models[static_cast<std::size_t>(modelIndex)];
        return "OK\t" + RegressionCoefficientDisplay(model.fit, model.terms, request.args[2]);
    }
    case CommandAction::RegressionComparisonFitCell: {
        if (request.args.size() < 3) return "ERR malformed REGCMP_FIT_CELL command";
        auto it = applicationState_.regressionComparisons().find(request.args[0]);
        if (it == applicationState_.regressionComparisons().end()) return "ERR unknown comparison";
        const int modelIndex = std::atoi(request.args[1].c_str()) - 1;
        if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= it->second.models.size()) {
            return "ERR invalid model index";
        }
        const int fitRow = std::atoi(request.args[2].c_str());
        const RegressionComparisonModel &model = it->second.models[static_cast<std::size_t>(modelIndex)];
        return "OK\t" + RegressionComparisonFitDisplay(
            model.fit, RegressionAdjacentModelTest(it->second, modelIndex), fitRow);
    }
    case CommandAction::RegressionComparisonVisible: {
        if (request.args.empty()) return "ERR missing comparison id";
        if (!services_.ui.regressionComparisonVisible) return std::nullopt;
        bool visible = false;
        const bool registered = services_.ui.regressionComparisonVisible(request.args[0], visible);
        return std::string("OK\t") + (registered ? "REGISTERED" : "MISSING") + "|" +
            (registered && visible ? "TRUE" : "FALSE");
    }
    case CommandAction::RegressionComparisonOpenDiagnostic: {
        if (request.args.size() < 3) return "ERR malformed REGCMP_OPEN_DIAGNOSTIC command";
        if (!services_.ui.openRegressionComparisonDiagnostic) return std::nullopt;
        const int modelIndex = std::atoi(request.args[1].c_str()) - 1;
        const std::string id = services_.ui.openRegressionComparisonDiagnostic(
            request.args[0], modelIndex, request.args[2]);
        return id.empty() ? "ERR diagnostic plot could not be opened" : "OK\t" + id;
    }
    case CommandAction::RegressionComparisonSetResponse: {
        if (request.args.size() < 2) return "ERR malformed comparison update command";
        if (!services_.ui.regressionComparisonUpdated) return std::nullopt;
        auto it = applicationState_.regressionComparisons().find(request.args[0]);
        if (it == applicationState_.regressionComparisons().end()) return "ERR unknown comparison";
        RegressionComparisonState &state = it->second;
        if (!FindNumericVariable(state.seed, request.args[1])) {
            return "ERR response variable is not available";
        }
        state.response = request.args[1];
        state.termRows.erase(
            std::remove_if(state.termRows.begin(), state.termRows.end(),
                           [&](const std::string &term) {
                               return ModelTermShouldBeRemoved(term, state.response);
                           }),
            state.termRows.end());
        for (RegressionComparisonModel &model : state.models) {
            SetModelSpecificationResponse(model, state.response);
            model.candidateTerms.erase(
                std::remove_if(model.candidateTerms.begin(),
                               model.candidateTerms.end(),
                               [&](const std::string &term) {
                                   return ModelTermShouldBeRemoved(term, state.response);
                               }),
                model.candidateTerms.end());
            model.modelVersion += 1;
            model.isStale = true;
        }
        if (state.autoRefit) {
            if (!services_.ui.requestRegressionComparisonFit) return std::nullopt;
            services_.ui.requestRegressionComparisonFit(state);
        }
        services_.ui.regressionComparisonUpdated(state);
        return "OK";
    }
    case CommandAction::RegressionComparisonSetScope: {
        return "ERR Analysis scope is global. Use the central Analysis Scope menu.";
    }
    case CommandAction::RegressionComparisonAddTerm: {
        if (request.args.size() < 2) return "ERR malformed REGCMP_ADD_TERM command";
        if (!services_.ui.regressionComparisonUpdated) return std::nullopt;
        auto it = applicationState_.regressionComparisons().find(request.args[0]);
        if (it == applicationState_.regressionComparisons().end()) return "ERR unknown comparison";
        RegressionComparisonState &state = it->second;
        if (state.models.empty()) return "ERR no comparison model";
        const int modelIndex = std::max(0, std::min(
            state.activeModel, static_cast<int>(state.models.size()) - 1));
        RegressionComparisonModel &model = state.models[static_cast<std::size_t>(modelIndex)];
        const std::string response = model.response.empty() ? state.response : model.response;
        const std::vector<std::string> available = AvailableVariableNames(state.seed);
        if (!ModelTermExistsForVariables(available, request.args[1], response)) {
            return "ERR invalid comparison term";
        }
        bool changed = false;
        for (const std::string &candidate :
             HierarchicalTermsForModelTerm(available, response, request.args[1])) {
            AddRegressionTermRow(state, candidate);
            if (!ModelIncludesTerm(model, candidate)) {
                model.terms.push_back(candidate);
                changed = true;
            }
        }
        if (changed) {
            model.modelVersion += 1;
            model.isStale = true;
            if (state.autoRefit) {
                if (!services_.ui.requestRegressionComparisonFit) return std::nullopt;
                services_.ui.requestRegressionComparisonFit(state);
            }
        }
        services_.ui.regressionComparisonUpdated(state);
        return "OK";
    }
    case CommandAction::RegressionComparisonSetTerm: {
        if (request.args.size() < 4) return "ERR malformed REGCMP_SET_TERM command";
        if (!services_.ui.regressionComparisonUpdated) return std::nullopt;
        auto it = applicationState_.regressionComparisons().find(request.args[0]);
        if (it == applicationState_.regressionComparisons().end()) return "ERR unknown comparison";
        RegressionComparisonState &state = it->second;
        const int modelIndex = std::atoi(request.args[1].c_str()) - 1;
        if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size()) {
            return "ERR invalid model index";
        }
        RegressionComparisonModel &model = state.models[static_cast<std::size_t>(modelIndex)];
        const std::string response = model.response.empty() ? state.response : model.response;
        const std::vector<std::string> available = AvailableVariableNames(state.seed);
        const std::string &term = request.args[2];
        if (!ModelTermExistsForVariables(available, term, response)) return "ERR invalid comparison term";
        if (request.args[3] == "TRUE") {
            for (const std::string &candidate :
                 HierarchicalTermsForModelTerm(available, response, term)) {
                AddRegressionTermRow(state, candidate);
                if (!ModelIncludesTerm(model, candidate)) model.terms.push_back(candidate);
            }
        } else {
            RemoveModelTermCascade(model.terms, term);
        }
        model.modelVersion += 1;
        model.isStale = true;
        if (state.autoRefit) {
            if (!services_.ui.requestRegressionComparisonFit) return std::nullopt;
            services_.ui.requestRegressionComparisonFit(state);
        }
        services_.ui.regressionComparisonUpdated(state);
        return "OK";
    }
    case CommandAction::RegressionComparisonAddModel: {
        if (request.args.size() < 3) return "ERR malformed REGCMP_ADD_MODEL command";
        if (!services_.ui.requestRegressionComparisonFit || !services_.ui.regressionComparisonUpdated) {
            return std::nullopt;
        }
        auto it = applicationState_.regressionComparisons().find(request.args[0]);
        if (it == applicationState_.regressionComparisons().end()) return "ERR unknown comparison";
        RegressionComparisonState &state = it->second;
        RegressionComparisonModel model;
        model.id = NextRegressionModelId(state);
        model.label = request.args[1];
        std::size_t termCountIndex = 2;
        model.response = state.response;
        if (request.args.size() >= 4 && FindNumericVariable(state.seed, request.args[2])) {
            model.response = request.args[2];
            termCountIndex = 3;
        }
        if (model.label.empty()) model.label = NextUntitledRegressionLabel(state);
        const long termCount = std::strtol(request.args[termCountIndex].c_str(), nullptr, 10);
        if (termCount < 0 || request.args.size() < termCountIndex + 1 + static_cast<std::size_t>(termCount)) {
            return "ERR invalid comparison term count";
        }
        const std::vector<std::string> available = AvailableVariableNames(state.seed);
        for (long termIndex = 0; termIndex < termCount; ++termIndex) {
            const std::string &term = request.args[termCountIndex + 1 + static_cast<std::size_t>(termIndex)];
            if (!ModelTermExistsForVariables(available, term, model.response)) continue;
            for (const std::string &candidate :
                 HierarchicalTermsForModelTerm(available, model.response, term)) {
                AddRegressionTermRow(state, candidate);
                if (!ModelIncludesTerm(model, candidate)) model.terms.push_back(candidate);
            }
        }
        model.candidateTerms = model.terms;
        model.isStale = true;
        state.models.push_back(std::move(model));
        state.activeModel = static_cast<int>(state.models.size()) - 1;
        services_.ui.requestRegressionComparisonFit(state);
        services_.ui.regressionComparisonUpdated(state);
        return "OK\t" + state.models.back().id;
    }
    case CommandAction::RegressionComparisonSetType: {
        if (request.args.size() < 3) return "ERR malformed REGCMP_SET_TYPE command";
        if (!services_.ui.regressionComparisonUpdated) return std::nullopt;
        auto it = applicationState_.regressionComparisons().find(request.args[0]);
        if (it == applicationState_.regressionComparisons().end()) return "ERR unknown comparison";
        RegressionComparisonState &state = it->second;
        if (state.precomputed) return "ERR pooled/precomputed comparison term types must be changed from R";
        if (state.models.empty()) return "ERR comparison has no models";
        const bool modelSpecific = request.args.size() >= 4;
        const int modelIndex = modelSpecific ? std::atoi(request.args[1].c_str()) :
            std::clamp(state.activeModel, 0, static_cast<int>(state.models.size()) - 1);
        const std::string term = ModelTermBaseForDisplayRow(request.args[modelSpecific ? 2 : 1]);
        if (term.empty() || IsInteractionTerm(term)) return "ERR invalid comparison term type";
        const std::string type = request.args[modelSpecific ? 3 : 2] == "factor" ? "factor" : "numeric";
        const std::vector<std::string> available = AvailableVariableNames(state.seed);
        if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size()) {
            return "ERR invalid comparison model";
        }
        const RegressionComparisonModel &target = state.models[static_cast<std::size_t>(modelIndex)];
        const std::string response = target.response.empty() ? state.response : target.response;
        const bool exists = ModelTermExistsForVariables(available, term, response);
        if (!exists) return "ERR invalid comparison term";
        std::string message;
        const bool affected = ApplyRegressionComparisonTermType(
            state, modelIndex, term, type, &message);
        if (!affected && !message.empty()) return "ERR " + message;
        if (affected && state.autoRefit) {
            if (!services_.ui.requestRegressionComparisonFit) return std::nullopt;
            services_.ui.requestRegressionComparisonFit(state);
        }
        services_.ui.regressionComparisonUpdated(state);
        return "OK";
    }
    case CommandAction::ModelOpenPooled: {
        if (request.args.size() < 8) return "ERR malformed MODEL_OPEN_POOLED command";
        if (!services_.queries.groupSeed || !services_.ui.showLinearModel) return std::nullopt;
        std::size_t cursor = 0;
        const std::string group = request.args[cursor++];
        const std::string title = request.args[cursor++];
        const std::string dependent = request.args[cursor++];
        const std::string scope = request.args[cursor++];
        const int imputationCount = std::atoi(request.args[cursor++].c_str());
        const std::string note = request.args[cursor++];
        const long termCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
        if (termCount < 0 || cursor + static_cast<std::size_t>(termCount) > request.args.size()) {
            return "ERR invalid pooled model term count";
        }
        std::vector<std::string> terms;
        for (long termIndex = 0; termIndex < termCount; ++termIndex) terms.push_back(request.args[cursor++]);
        GLMFitSummary fit;
        if (!ReadLinearFitPayload(request.args, cursor, fit)) {
            return "ERR malformed pooled linear model fit payload";
        }
        if (cursor < request.args.size()) {
            if (!ReadLinearDiagnosticsPayload(request.args, cursor, fit.diagnostics)) {
                return "ERR malformed pooled linear model diagnostic payload";
            }
            if (!ReadLinearDesignPayload(request.args, cursor, fit)) {
                return "ERR malformed pooled linear model design payload";
            }
            if (!ReadLinearMIDiagnosticsPayload(request.args, cursor, fit)) {
                return "ERR malformed pooled linear model MI diagnostic payload";
            }
        }
        std::vector<int> fitSelectionRows;
        bool hasExactSelectionIdentity = false;
        if (cursor < request.args.size() && request.args[cursor] == "LINEAR_SCOPE_ROWS_V1") {
            ++cursor;
            if (cursor >= request.args.size()) {
                return "ERR missing pooled linear model selection row count";
            }
            const long rowCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (rowCount < 0 ||
                cursor + static_cast<std::size_t>(rowCount) > request.args.size()) {
                return "ERR malformed pooled linear model selection row payload";
            }
            for (long row = 0; row < rowCount; ++row) {
                fitSelectionRows.push_back(std::atoi(request.args[cursor++].c_str()));
            }
            hasExactSelectionIdentity = true;
        }
        AnalysisProvenance returnedProvenance;
        std::string provenanceError;
        if (!ReadAnalysisProvenancePayload(
                request.args, cursor, returnedProvenance, &provenanceError)) {
            return "ERR malformed pooled linear model provenance: " + provenanceError;
        }
        PlotModel seed;
        if (!services_.queries.groupSeed(group, seed)) {
            return "ERR no registered dataset/group for pooled linear model";
        }
        GroupModelState &state = EnsureGroupModelState(applicationState_.groupModels(), group, &seed);
        state.response = dependent;
        state.terms = std::move(terms);
        state.scope = scope;
        state.centeredPredictors.clear();
        for (const auto &center : fit.predictorCenters) {
            state.centeredPredictors.insert(center.first);
        }
        state.factorReferenceLevels.clear();
        for (const GLMFactorCoding &coding : fit.factorCodings) {
            if (!coding.variable.empty()) {
                state.termTypes[coding.variable] = "factor";
                if (!coding.referenceLevel.empty()) {
                    state.factorReferenceLevels[coding.variable] =
                        coding.referenceLevel;
                }
            }
        }
        state.precomputed = true;
        state.multipleImputation = true;
        state.imputationCount = imputationCount;
        state.title = title.empty() ? "Linear Model — Multiple Imputation" : title;
        state.note = note;
        state.isStale = false;
        state.rFitPending = false;
        state.fitVersion += 1;
        state.rowsUsed = fit.rowsUsed;
        state.rowsExcluded = fit.rowsExcluded;
        if (const DataFrameModel *dataframe = applicationState_.datasets().find(group)) {
            SynchronizeGroupModelTermTypes(state, *dataframe);
            if (!returnedProvenance.executedRCode.empty()) {
                if (scope == "all") {
                    state.dataScope = AllObservationsAnalysisScope(
                        group, static_cast<std::size_t>(dataframe->rows));
                } else {
                    std::vector<int> requested = fit.rowsUsed;
                    requested.insert(requested.end(), fit.rowsExcluded.begin(), fit.rowsExcluded.end());
                    state.dataScope = ExplicitAnalysisScope(
                        group, requested,
                        scope == "selected" ? AnalysisScopeSourceKind::CurrentSelection :
                            AnalysisScopeSourceKind::OtherExplicitSubset,
                        scope == "selected" ? "Selected observations" : "Unselected observations",
                        static_cast<std::size_t>(dataframe->rows));
                }
                state.dataScopeCaptured = true;
                CompleteAnalysisProvenance(
                    returnedProvenance, *dataframe, state.dataScope,
                    fit.rowsUsed, fit.rowsExcluded, "glm:" + group, state.title);
                state.provenance = std::move(returnedProvenance);
                applicationState_.registerOutputCodeReference(
                    LinearModelCodeReference(state, fit));
            }
        }
        if (hasExactSelectionIdentity) {
            std::string signature = LinearGLMFitSignature(
                state.response, state.terms,
                EffectiveModelSpecificationTermTypes(state), state.scope,
                state.centeredPredictors, state.factorReferenceLevels);
            state.lastRFitSignature = LinearGLMFitIdentityWithSelection(
                signature, state.scope, fitSelectionRows);
        } else {
            // Legacy payloads are adopted once by the front end.  New payloads
            // always carry exact selection identity and never use this path.
            state.lastRFitSignature.clear();
        }
        applicationState_.linearModelFits()[group] = std::move(fit);
        services_.ui.showLinearModel(group);
        return "OK\tglm:" + group;
    }
    case CommandAction::OpenGLM: {
        if (request.name != "GLM") return std::nullopt;
        if (!services_.ui.showLinearModel) return std::nullopt;
        PlotModel plot;
        if (!request.args.empty() && request.args[0] != "active") {
            if (!services_.queries.plot || !services_.queries.plot(request.args[0], plot)) {
                return "ERR no active plot";
            }
        } else if (!services_.queries.activePlot || !services_.queries.activePlot(plot)) {
            return "ERR no active plot";
        }
        const std::string modelId = CreateLinearModelInstance(
            applicationState_.groupModels(), plot.group);
        GroupModelState &state = applicationState_.groupModels().at(modelId);
        if (const DataFrameModel *dataframe = applicationState_.datasets().find(plot.group)) {
            const auto initial = ResolveInitialAnalysisSpecification(
                *dataframe, SharedAnalysisDefinition(SharedAnalysisKind::LinearModel),
                DefaultVariableRolesForAnalysisInitialization(
                    applicationState_.variableRoles(plot.group)));
            state.response = InitialAnalysisVariable(initial, "response");
            state.terms = InitialAnalysisVariables(initial, "predictors");
            state.multipleImputation = dataframe->datasetType == "multiple_imputation" ||
                dataframe->imputationCount > 1;
            state.imputationCount = state.multipleImputation ? dataframe->imputationCount : 0;
            applicationState_.captureAnalysisScope(
                plot.group, state.dataScope, state.dataScopeCaptured, &state.scope);
        }
        MarkGroupModelChanged(state);
        services_.ui.showLinearModel(modelId);
        return "OK\tglm:" + modelId;
    }
    case CommandAction::RecordingCommand:
        if (!services_.ui.dispatchRecordingCommand) return std::nullopt;
        services_.ui.dispatchRecordingCommand(request.name);
        return "OK";
    case CommandAction::AddPlot: {
        if (request.args.size() < 5) return "ERR malformed ADD_PLOT command";
        const std::string id = request.args[0];
        const std::string group = request.args[1];
        const std::string xLabel = request.args[2];
        const std::string yLabel = request.args[3];
        std::string title;
        std::size_t countIndex = 4;
        char *end = nullptr;
        long count = std::strtol(request.args[countIndex].c_str(), &end, 10);
        if (end == request.args[countIndex].c_str() || *end != '\0') {
            title = request.args[countIndex];
            countIndex = 5;
            if (request.args.size() <= countIndex) return "ERR malformed ADD_PLOT command";
            count = std::strtol(request.args[countIndex].c_str(), nullptr, 10);
        }
        if (count < 0 || count > 1000000 ||
            request.args.size() < countIndex + 1 + static_cast<std::size_t>(count)) {
            return "ERR invalid point count";
        }
        PlotModel *model = new PlotModel();
        model->id = id;
        model->group = group;
        model->xLabel = xLabel;
        model->yLabel = yLabel;
        model->title = title;
        model->points.reserve(static_cast<std::size_t>(count));
        for (long index = 0; index < count; ++index) {
            std::istringstream input(request.args[countIndex + 1 + static_cast<std::size_t>(index)]);
            DataPoint point;
            if (!(input >> point.x >> point.y >> point.row)) {
                delete model;
                return "ERR malformed point data";
            }
            model->points.push_back(point);
        }
        std::size_t cursor = countIndex + 1 + static_cast<std::size_t>(count);
        if (cursor < request.args.size() && request.args[cursor] == "TIME_SERIES") {
            ++cursor;
            if (cursor + 2 >= request.args.size()) {
                delete model; return "ERR malformed time-series metadata";
            }
            model->kind = "time_series";
            model->timeSeriesTimeType = request.args[cursor++];
            model->timeSeriesGroupVariable = request.args[cursor++];
            const long seriesCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (seriesCount < 1 || cursor + static_cast<std::size_t>(seriesCount) > request.args.size()) {
                delete model; return "ERR invalid time-series count";
            }
            std::vector<std::string> labels;
            labels.reserve(static_cast<std::size_t>(seriesCount));
            for (long seriesIndex = 0; seriesIndex < seriesCount; ++seriesIndex) {
                labels.push_back(request.args[cursor++]);
            }
            if (!model->timeSeriesGroupVariable.empty()) {
                model->timeSeriesPointGroups.reserve(model->points.size());
                for (long index = 0; index < count; ++index) {
                    std::istringstream input(request.args[countIndex + 1 + static_cast<std::size_t>(index)]);
                    double x = NAN;
                    double y = NAN;
                    int row = 0;
                    long seriesIndex = -1;
                    if (!(input >> x >> y >> row >> seriesIndex) ||
                        seriesIndex < 0 || seriesIndex >= seriesCount) {
                        delete model; return "ERR malformed time-series point";
                    }
                    model->timeSeriesPointGroups.push_back(labels[static_cast<std::size_t>(seriesIndex)]);
                }
            }
            model->interactionPlotLines = BuildTimeSeriesLines(
                model->points, model->timeSeriesPointGroups,
                labels.empty() ? model->yLabel : labels.front());
            if (cursor < request.args.size() && request.args[cursor] == "TIME_SERIES_OPTIONS") {
                ++cursor;
                if (cursor + 1 >= request.args.size()) {
                    delete model; return "ERR malformed time-series options";
                }
                const std::string identification = request.args[cursor++];
                const std::string legendPosition = request.args[cursor++];
                if (identification == "legend" || identification == "start_labels" ||
                    identification == "none") {
                    model->timeSeriesIdentification = identification;
                }
                const auto mode = model->timeSeriesIdentification;
                SetTimeSeriesLegendPosition(*model, legendPosition);
                model->timeSeriesIdentification = mode;
            }
        }
        if (cursor < request.args.size() && request.args[cursor] == "IMPUTATION_PROCESS_DIAGNOSTIC") {
            ++cursor;
            if (cursor >= request.args.size()) { delete model; return "ERR missing imputation diagnostic kind"; }
            model->glmDiagnosticKind = "imputation_" + request.args[cursor++];
            if (model->kind != "time_series" || !PlotIsImputationDiagnostic(*model)) {
                delete model; return "ERR invalid imputation diagnostic kind";
            }
        }
        if (cursor < request.args.size() && request.args[cursor] == "IMPUTATION_PLOT_VIEW") {
            ++cursor;
            if (!PlotIsImputationDiagnostic(*model) || cursor+6>request.args.size()) {
                delete model; return "ERR invalid imputation plot view";
            }
            model->imputationDiagnosticVariable=request.args[cursor++];
            model->imputationDiagnosticTotal=std::atoi(request.args[cursor++].c_str());
            model->imputationDiagnosticFirst=std::atoi(request.args[cursor++].c_str());
            model->imputationDiagnosticLast=std::atoi(request.args[cursor++].c_str());
            model->imputationDiagnosticMaxSteps=std::atoi(request.args[cursor++].c_str());
            model->imputationDiagnosticSimplified=request.args[cursor++] == "1";
            if(model->imputationDiagnosticFirst<1 || model->imputationDiagnosticLast<model->imputationDiagnosticFirst ||
               model->imputationDiagnosticLast>model->imputationDiagnosticTotal ||
               model->imputationDiagnosticLast-model->imputationDiagnosticFirst>4 || model->imputationDiagnosticMaxSteps<2) {
                delete model; return "ERR invalid imputation plot range";
            }
        }
        if (cursor < request.args.size() && request.args[cursor] == "TRELLIS_SCATTERPLOT") {
            ++cursor;
            if (cursor + 1 >= request.args.size()) {
                delete model; return "ERR malformed trellis-scatterplot metadata";
            }
            model->kind = "trellis_scatterplot";
            model->trellisConditionVariable = request.args[cursor++];
            const long panelCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (panelCount < 1 || cursor + static_cast<std::size_t>(panelCount) > request.args.size()) {
                delete model; return "ERR invalid trellis-scatterplot panel count";
            }
            for (long panelIndex = 0; panelIndex < panelCount; ++panelIndex) {
                model->trellisPanelLevels.push_back(request.args[cursor++]);
            }
            model->trellisPointPanels.reserve(model->points.size());
            for (long index = 0; index < count; ++index) {
                std::istringstream input(request.args[countIndex + 1 + static_cast<std::size_t>(index)]);
                double x = NAN;
                double y = NAN;
                int row = 0;
                long panelIndex = -1;
                if (!(input >> x >> y >> row >> panelIndex) ||
                    panelIndex < 0 || panelIndex >= panelCount) {
                    delete model; return "ERR malformed trellis-scatterplot point";
                }
                model->trellisPointPanels.push_back(
                    model->trellisPanelLevels[static_cast<std::size_t>(panelIndex)]);
            }
            if (cursor < request.args.size() && request.args[cursor] == "TRELLIS_SCATTERPLOT_OPTIONS") {
                ++cursor;
                if (cursor + 1 >= request.args.size()) {
                    delete model; return "ERR malformed trellis-scatterplot options";
                }
                const std::string layoutMode = request.args[cursor++];
                const std::string panelOrder = request.args[cursor++];
                if (layoutMode == "automatic" || layoutMode == "one_row" ||
                    layoutMode == "one_column" || layoutMode == "two_columns" ||
                    layoutMode == "three_columns") {
                    model->trellisLayoutMode = layoutMode;
                }
                if (panelOrder == "defined" || panelOrder == "ascending" ||
                    panelOrder == "descending") {
                    model->trellisPanelOrder = panelOrder;
                }
            }
            InitializeTrellisSpecificationFromLegacy(*model);
        }
        if (cursor < request.args.size() && request.args[cursor] == "VARS") {
            ++cursor;
            if (cursor >= request.args.size()) { delete model; return "ERR malformed variable payload"; }
            const long variableCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (variableCount < 0) { delete model; return "ERR invalid variable count"; }
            for (long variableIndex = 0; variableIndex < variableCount; ++variableIndex) {
                if (cursor + 1 >= request.args.size()) { delete model; return "ERR malformed variable payload"; }
                NumericVariable variable;
                variable.name = request.args[cursor++];
                const long valueCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
                if (valueCount < 0 || cursor + static_cast<std::size_t>(valueCount) > request.args.size()) {
                    delete model; return "ERR invalid variable value count";
                }
                variable.values.reserve(static_cast<std::size_t>(valueCount));
                for (long valueIndex = 0; valueIndex < valueCount; ++valueIndex) {
                    const std::string &value = request.args[cursor++];
                    variable.values.push_back(value == "NA" ? NAN : std::strtod(value.c_str(), nullptr));
                }
                model->variables.push_back(std::move(variable));
            }
        }
        if (cursor < request.args.size() && request.args[cursor] == "VARMETA") {
            ++cursor;
            if (cursor >= request.args.size()) { delete model; return "ERR malformed variable metadata"; }
            const long metadataCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (metadataCount < 0) { delete model; return "ERR invalid variable metadata count"; }
            for (long metadataIndex = 0; metadataIndex < metadataCount; ++metadataIndex) {
                if (cursor + 1 >= request.args.size()) { delete model; return "ERR malformed variable metadata"; }
                model->variableMeta.push_back({request.args[cursor], NormalizeVariableType(request.args[cursor + 1])});
                cursor += 2;
            }
        }
        if (model->variableMeta.empty()) {
            for (const NumericVariable &variable : model->variables) {
                model->variableMeta.push_back({variable.name, "numeric"});
            }
        }
        DataFrameModel dataframe;
        bool parsed = false;
        std::string error;
        if (!ParseDataFramePayload(request.args, cursor, group, dataframe, &parsed, error)) {
            delete model;
            return error;
        }
        if (parsed) {
            applicationState_.registerDataset(dataframe);
        } else if (!applicationState_.datasets().find(group) &&
                   !model->variables.empty()) {
            // Older graph payloads carried their source variables without a
            // separate DATAFRAME block. Preserve those values as the exact
            // exportable source version instead of leaving Show R Code with
            // no data reference.
            DataFrameModel embedded;
            embedded.group = group;
            for (const NumericVariable &variable : model->variables) {
                DataColumn column;
                column.name = variable.name;
                column.type = "numeric";
                const auto meta = std::find_if(
                    model->variableMeta.begin(), model->variableMeta.end(),
                    [&](const VariableMeta &entry) {
                        return entry.name == variable.name;
                    });
                if (meta != model->variableMeta.end()) column.type = meta->type;
                column.values.reserve(variable.values.size());
                for (double value : variable.values) {
                    if (!std::isfinite(value)) column.values.push_back("NA");
                    else {
                        std::ostringstream formatted;
                        formatted << std::setprecision(17) << value;
                        column.values.push_back(formatted.str());
                    }
                }
                embedded.rows = std::max(
                    embedded.rows, static_cast<int>(column.values.size()));
                embedded.columns.push_back(std::move(column));
            }
            applicationState_.registerDataset(embedded);
        }
        ComputeRanges(*model);
        model->rExportTheme = plotTheme_;
        RegisterBasicPlotCodeReference(applicationState_, *model);
        auto existing=applicationState_.plots().find(id);
        if(PlotIsImputationDiagnostic(*model) && existing!=applicationState_.plots().end() &&
           existing->second && PlotIsImputationDiagnostic(*existing->second)) {
            auto* previous=existing->second;
            model->timeSeriesIdentification=previous->timeSeriesIdentification;
            model->timeSeriesLegendPosition=previous->timeSeriesLegendPosition;
            model->interactionLegendUsesCustomPosition=previous->interactionLegendUsesCustomPosition;
            model->interactionLegendX=previous->interactionLegendX;
            model->interactionLegendY=previous->interactionLegendY;
            model->codeReference=previous->codeReference;
            *previous=std::move(*model); delete model;
            if(services_.ui.redrawPlot) services_.ui.redrawPlot(id);
            return "OK";
        }
        applicationState_.plots()[id] = model;
        applicationState_.ensureSelectionGroup(group);
        applicationState_.datasets().rememberActiveDatasetGroup(group);
        EnsureGroupModelState(applicationState_.groupModels(), group, model);
        if (services_.ui.addPlot) services_.ui.addPlot(model);
        // Aggregate diagnostic coordinates have no data sheet. Their icon resolves
        // the source from provenance when requested; do not open it over the plot.
        if (services_.ui.openDataSheet && !PlotIsImputationDiagnostic(*model))
            services_.ui.openDataSheet(group);
        return "OK";
    }
    case CommandAction::AddBoxplot: {
        if (request.args.size() < 9) return "ERR malformed ADD_BOXPLOT command";
        PlotModel *model = new PlotModel();
        model->kind = "boxplot";
        model->id = request.args[0];
        model->group = request.args[1];
        model->xLabel = request.args[2];
        model->yLabel = request.args[3];
        model->title = request.args[4];
        model->boxplotShowPoints = request.args[5] == "TRUE";
        model->boxplotShowBox = request.args[6] == "TRUE";
        model->boxplotShowWhiskers = request.args[7] == "TRUE";
        const long count = std::strtol(request.args[8].c_str(), nullptr, 10);
        if (count < 0 || request.args.size() < 9 + static_cast<std::size_t>(count)) {
            delete model; return "ERR invalid boxplot point count";
        }
        for (long index = 0; index < count; ++index) {
            const std::string &line = request.args[9 + static_cast<std::size_t>(index)];
            const std::size_t first = line.find('\t');
            const std::size_t second = first == std::string::npos ? std::string::npos : line.find('\t', first + 1);
            if (first == std::string::npos || second == std::string::npos) {
                delete model; return "ERR malformed boxplot point";
            }
            const double y = std::strtod(line.substr(0, first).c_str(), nullptr);
            const std::string category = line.substr(first + 1, second - first - 1);
            const int row = std::atoi(line.substr(second + 1).c_str());
            if (std::isfinite(y) && row > 0) {
                model->boxplotPoints.push_back({y, category.empty() ? "NA" : category, row});
            }
        }
        model->boxplotCategories = CategoriesForPlot(*model);
        model->boxplotDefinedCategories = model->boxplotCategories;
        std::size_t cursor = 9 + static_cast<std::size_t>(count);
        if (cursor < request.args.size() &&
            request.args[cursor] == "BOXPLOT_GROUPING_VARIABLES") {
            ++cursor;
            if (cursor >= request.args.size()) {
                delete model; return "ERR malformed boxplot grouping payload";
            }
            const long groupingCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (groupingCount < 0 ||
                cursor + static_cast<std::size_t>(groupingCount) > request.args.size()) {
                delete model; return "ERR invalid boxplot grouping variable count";
            }
            for (long index = 0; index < groupingCount; ++index)
                model->boxplotGroupingVariables.push_back(request.args[cursor++]);
        }
        if (cursor < request.args.size() && request.args[cursor] == "VARS") {
            ++cursor;
            if (cursor >= request.args.size()) { delete model; return "ERR malformed variable payload"; }
            const long variableCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            for (long variableIndex = 0; variableIndex < variableCount; ++variableIndex) {
                if (cursor + 1 >= request.args.size()) { delete model; return "ERR malformed variable payload"; }
                NumericVariable variable;
                variable.name = request.args[cursor++];
                const long valueCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
                if (valueCount < 0 || cursor + static_cast<std::size_t>(valueCount) > request.args.size()) {
                    delete model; return "ERR invalid variable value count";
                }
                for (long valueIndex = 0; valueIndex < valueCount; ++valueIndex) {
                    const std::string &value = request.args[cursor++];
                    variable.values.push_back(value == "NA" ? NAN : std::strtod(value.c_str(), nullptr));
                }
                model->variables.push_back(std::move(variable));
            }
        }
        if (cursor < request.args.size() && request.args[cursor] == "VARMETA") {
            ++cursor;
            if (cursor >= request.args.size()) { delete model; return "ERR malformed variable metadata"; }
            const long metadataCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            for (long metadataIndex = 0; metadataIndex < metadataCount; ++metadataIndex) {
                if (cursor + 1 >= request.args.size()) { delete model; return "ERR malformed variable metadata"; }
                model->variableMeta.push_back({request.args[cursor], NormalizeVariableType(request.args[cursor + 1])});
                cursor += 2;
            }
        }
        EnsureParallelBoxplotState(*model);
        DataFrameModel dataframe;
        bool parsed = false;
        std::string error;
        if (!ParseDataFramePayload(request.args, cursor, model->group, dataframe, &parsed, error)) {
            delete model; return error;
        }
        if (parsed) {
            applicationState_.registerDataset(dataframe);
            if (!model->boxplotGroupingVariables.empty())
                RebuildGroupedBoxplotPointsFromDataFrame(*model, dataframe);
        }
        model->rExportTheme = plotTheme_;
        RegisterBasicPlotCodeReference(applicationState_, *model);
        applicationState_.plots()[model->id] = model;
        applicationState_.ensureSelectionGroup(model->group);
        applicationState_.datasets().rememberActiveDatasetGroup(model->group);
        if (services_.ui.addPlot) services_.ui.addPlot(model);
        if (services_.ui.openDataSheet) services_.ui.openDataSheet(model->group);
        return "OK";
    }
    case CommandAction::AddHistogram: {
        if (request.args.size() < 8) return "ERR malformed ADD_HISTOGRAM command";
        PlotModel *model = new PlotModel();
        model->kind = "histogram";
        model->id = request.args[0]; model->group = request.args[1]; model->xLabel = request.args[2];
        model->title = request.args[3]; model->histogramShowCounts = request.args[4] == "TRUE";
        model->histogramShowRug = request.args[5] == "TRUE";
        const long binCount = std::strtol(request.args[6].c_str(), nullptr, 10);
        const long pointCount = std::strtol(request.args[7].c_str(), nullptr, 10);
        if (binCount < 1 || pointCount < 0 ||
            request.args.size() < 8 + static_cast<std::size_t>(binCount + pointCount)) {
            delete model; return "ERR invalid histogram payload size";
        }
        std::size_t cursor = 8;
        model->histogramBins.resize(static_cast<std::size_t>(binCount));
        for (long index = 0; index < binCount; ++index) {
            const std::string &line = request.args[cursor++];
            const std::size_t tab = line.find('\t');
            if (tab == std::string::npos) { delete model; return "ERR malformed histogram bin"; }
            model->histogramBins[static_cast<std::size_t>(index)].lower =
                std::strtod(line.substr(0, tab).c_str(), nullptr);
            model->histogramBins[static_cast<std::size_t>(index)].upper =
                std::strtod(line.substr(tab + 1).c_str(), nullptr);
        }
        for (long index = 0; index < pointCount; ++index) {
            const std::string &line = request.args[cursor++];
            const std::size_t first = line.find('\t');
            const std::size_t second = first == std::string::npos ? std::string::npos : line.find('\t', first + 1);
            if (first == std::string::npos || second == std::string::npos) {
                delete model; return "ERR malformed histogram point";
            }
            const double x = std::strtod(line.substr(0, first).c_str(), nullptr);
            const int row = std::atoi(line.substr(first + 1, second - first - 1).c_str());
            const int bin = std::atoi(line.substr(second + 1).c_str());
            if (std::isfinite(x) && row > 0 && bin >= 1 && bin <= binCount) {
                model->histogramPoints.push_back({x, row, bin});
                model->histogramBins[static_cast<std::size_t>(bin - 1)].rows.push_back(row);
            }
        }
        if (cursor < request.args.size() && request.args[cursor] == "VARS") {
            ++cursor;
            if (cursor >= request.args.size()) { delete model; return "ERR malformed variable payload"; }
            const long variableCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            for (long variableIndex = 0; variableIndex < variableCount; ++variableIndex) {
                if (cursor + 1 >= request.args.size()) { delete model; return "ERR malformed variable payload"; }
                NumericVariable variable; variable.name = request.args[cursor++];
                const long valueCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
                if (valueCount < 0 || cursor + static_cast<std::size_t>(valueCount) > request.args.size()) {
                    delete model; return "ERR invalid variable value count";
                }
                for (long valueIndex = 0; valueIndex < valueCount; ++valueIndex) {
                    const std::string &value = request.args[cursor++];
                    variable.values.push_back(value == "NA" ? NAN : std::strtod(value.c_str(), nullptr));
                }
                model->variables.push_back(std::move(variable));
            }
        }
        if (cursor < request.args.size() && request.args[cursor] == "VARMETA") {
            ++cursor;
            if (cursor >= request.args.size()) { delete model; return "ERR malformed variable metadata"; }
            const long metadataCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            for (long metadataIndex = 0; metadataIndex < metadataCount; ++metadataIndex) {
                if (cursor + 1 >= request.args.size()) { delete model; return "ERR malformed variable metadata"; }
                model->variableMeta.push_back({request.args[cursor], NormalizeVariableType(request.args[cursor + 1])});
                cursor += 2;
            }
        }
        if (model->variableMeta.empty()) {
            for (const NumericVariable &variable : model->variables) {
                model->variableMeta.push_back({variable.name, "numeric"});
            }
        }
        DataFrameModel dataframe; bool parsed = false; std::string error;
        if (!ParseDataFramePayload(request.args, cursor, model->group, dataframe, &parsed, error)) {
            delete model; return error;
        }
        if (parsed) applicationState_.registerDataset(dataframe);
        model->rExportTheme = plotTheme_;
        RegisterBasicPlotCodeReference(applicationState_, *model);
        applicationState_.plots()[model->id] = model;
        applicationState_.ensureSelectionGroup(model->group);
        applicationState_.datasets().rememberActiveDatasetGroup(model->group);
        EnsureGroupModelState(applicationState_.groupModels(), model->group, model);
        if (services_.ui.addPlot) services_.ui.addPlot(model);
        if (services_.ui.openDataSheet) services_.ui.openDataSheet(model->group);
        return "OK";
    }
    case CommandAction::AddBarplot: {
        if (request.args.size() < 10) return "ERR malformed ADD_BARPLOT command";
        PlotModel *model = new PlotModel();
        model->kind = "barplot"; model->id = request.args[0]; model->group = request.args[1];
        model->xLabel = request.args[2]; model->yLabel = request.args[3]; model->title = request.args[4];
        model->barplotXVariables = SplitBarplotXLabel(model->xLabel);
        model->barplotMode = request.args[5]; model->barplotWidthMode = request.args[6];
        model->barplotSplitVariable = request.args[7]; model->barplotRowColorDisplay = "hide";
        model->barplotShowConditionalPercent = model->barplotMode == "conditional_percent" &&
            !model->barplotSplitVariable.empty();
        model->barplotTotalN = std::atoi(request.args[8].c_str());
        const long excludedCount = std::strtol(request.args[9].c_str(), nullptr, 10);
        if (excludedCount < 0 || request.args.size() < 10 + static_cast<std::size_t>(excludedCount) + 1) {
            delete model; return "ERR invalid barplot excluded row count";
        }
        std::size_t cursor = 10;
        for (long index = 0; index < excludedCount; ++index) {
            const int row = std::atoi(request.args[cursor++].c_str());
            if (row > 0) model->barplotExcludedRows.push_back(row);
        }
        const long barCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
        if (barCount < 0) { delete model; return "ERR invalid barplot bar count"; }
        for (long barIndex = 0; barIndex < barCount; ++barIndex) {
            if (cursor >= request.args.size()) { delete model; return "ERR malformed barplot bar"; }
            const std::vector<std::string> parts = SplitTabs(request.args[cursor++]);
            if (parts.size() < 5) { delete model; return "ERR malformed barplot bar"; }
            BarplotBin bin;
            bin.category = parts[0].empty() ? "NA" : parts[0];
            bin.n = std::atoi(parts[1].c_str()); bin.percent = std::strtod(parts[2].c_str(), nullptr);
            bin.widthValue = std::strtod(parts[3].c_str(), nullptr); bin.rows = ParseRowIdList(parts[4]);
            if (cursor >= request.args.size()) { delete model; return "ERR malformed barplot segment count"; }
            const long segmentCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (segmentCount < 0 || cursor + static_cast<std::size_t>(segmentCount) > request.args.size()) {
                delete model; return "ERR invalid barplot segment count";
            }
            for (long segmentIndex = 0; segmentIndex < segmentCount; ++segmentIndex) {
                const std::vector<std::string> segmentParts = SplitTabs(request.args[cursor++]);
                if (segmentParts.size() < 9) { delete model; return "ERR malformed barplot segment"; }
                BarplotSegment segment;
                segment.level = segmentParts[0].empty() ? "NA" : segmentParts[0];
                segment.count = std::atoi(segmentParts[1].c_str());
                segment.barN = std::atoi(segmentParts[2].c_str());
                segment.totalN = std::atoi(segmentParts[3].c_str());
                segment.conditionalPercent = std::strtod(segmentParts[4].c_str(), nullptr);
                segment.overallPercent = std::strtod(segmentParts[5].c_str(), nullptr);
                segment.barPercent = std::strtod(segmentParts[6].c_str(), nullptr);
                segment.barWidthValue = std::strtod(segmentParts[7].c_str(), nullptr);
                segment.rows = ParseRowIdList(segmentParts[8]);
                bin.segments.push_back(std::move(segment));
            }
            if (bin.n <= 0) bin.n = static_cast<int>(bin.rows.size());
            if (bin.segments.empty()) {
                BarplotSegment segment;
                segment.level = "All"; segment.rows = bin.rows; segment.count = bin.n; segment.barN = bin.n;
                segment.totalN = model->barplotTotalN; segment.conditionalPercent = bin.n > 0 ? 100.0 : 0.0;
                segment.overallPercent = model->barplotTotalN > 0
                    ? 100.0 * static_cast<double>(bin.n) / static_cast<double>(model->barplotTotalN) : 0.0;
                segment.barPercent = bin.percent; segment.barWidthValue = bin.widthValue;
                bin.segments.push_back(std::move(segment));
            }
            model->barplotBins.push_back(std::move(bin));
        }
        if (cursor < request.args.size() && request.args[cursor] == "VARS") {
            ++cursor;
            if (cursor >= request.args.size()) { delete model; return "ERR malformed variable payload"; }
            const long variableCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            for (long variableIndex = 0; variableIndex < variableCount; ++variableIndex) {
                if (cursor + 1 >= request.args.size()) { delete model; return "ERR malformed variable payload"; }
                NumericVariable variable; variable.name = request.args[cursor++];
                const long valueCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
                if (valueCount < 0 || cursor + static_cast<std::size_t>(valueCount) > request.args.size()) {
                    delete model; return "ERR invalid variable value count";
                }
                for (long valueIndex = 0; valueIndex < valueCount; ++valueIndex) {
                    const std::string &value = request.args[cursor++];
                    variable.values.push_back(value == "NA" ? NAN : std::strtod(value.c_str(), nullptr));
                }
                model->variables.push_back(std::move(variable));
            }
        }
        if (cursor < request.args.size() && request.args[cursor] == "VARMETA") {
            ++cursor;
            if (cursor >= request.args.size()) { delete model; return "ERR malformed variable metadata"; }
            const long metadataCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            for (long metadataIndex = 0; metadataIndex < metadataCount; ++metadataIndex) {
                if (cursor + 1 >= request.args.size()) { delete model; return "ERR malformed variable metadata"; }
                model->variableMeta.push_back({request.args[cursor], NormalizeVariableType(request.args[cursor + 1])});
                cursor += 2;
            }
        }
        if (model->variableMeta.empty()) {
            for (const NumericVariable &variable : model->variables) model->variableMeta.push_back({variable.name, "numeric"});
        }
        DataFrameModel dataframe; bool parsed = false; std::string error;
        if (!ParseDataFramePayload(request.args, cursor, model->group, dataframe, &parsed, error)) {
            delete model; return error;
        }
        if (parsed) applicationState_.registerDataset(dataframe);
        SortBarplotBinsByXHierarchy(*model); NormalizeBarplotVisualState(*model);
        model->rExportTheme = plotTheme_;
        RegisterBasicPlotCodeReference(applicationState_, *model);
        applicationState_.plots()[model->id] = model;
        applicationState_.ensureSelectionGroup(model->group);
        applicationState_.datasets().rememberActiveDatasetGroup(model->group);
        EnsureGroupModelState(applicationState_.groupModels(), model->group, model);
        if (services_.ui.addPlot) services_.ui.addPlot(model);
        if (services_.ui.openDataSheet) services_.ui.openDataSheet(model->group);
        return "OK";
    }
    case CommandAction::CompareMeansBatchOpen: {
        if (!services_.ui.showMeanComparison) return std::nullopt;
        MeanComparisonState state;
        std::string error;
        if (!ParseMeanComparisonBatch(request.args, state, &error)) {
            return "ERR " + error;
        }
        if (const DataFrameModel *dataframe = applicationState_.datasets().find(state.datasetId)) {
            AnalysisScope scope = applicationState_.activeAnalysisScope(state.datasetId);
            state.dataScope = scope;
            state.dataScopeCaptured = true;
            std::vector<int> rowsUsed;
            for (const MeanComparisonTable &table : state.tables) {
                for (const MeanComparisonRow &row : table.rows) {
                    rowsUsed.insert(rowsUsed.end(), row.originalRowIndices.begin(),
                                    row.originalRowIndices.end());
                }
            }
            std::sort(rowsUsed.begin(), rowsUsed.end());
            rowsUsed.erase(std::unique(rowsUsed.begin(), rowsUsed.end()), rowsUsed.end());
            CompleteAnalysisProvenance(state.provenance, *dataframe, scope,
                                       rowsUsed, {}, state.id, state.title);
            if (!state.provenance.executedRCode.empty()) {
                applicationState_.registerOutputCodeReference(
                    MeanComparisonCodeReference(state));
            }
        }
        services_.ui.showMeanComparison(state);
        return "OK\t" + state.id;
    }
    case CommandAction::Table1OpenStructured: {
        if (!services_.ui.showTable1) return std::nullopt;
        if (request.args.size() < 8) return "ERR malformed TABLE1_OPEN_STRUCTURED command";
        Table1DisplayState state;
        std::size_t cursor = 0;
        state.id = request.args[cursor++];
        state.datasetId = request.args[cursor++];
        bool modern = cursor < request.args.size() && request.args[cursor] == "REQUEST_V1";
        if (modern) {
            if (cursor + 3 >= request.args.size()) return "ERR malformed Table 1 request identity";
            ++cursor;
            const auto revision = std::strtoull(request.args[cursor++].c_str(), nullptr, 10);
            const auto version = std::strtoull(request.args[cursor++].c_str(), nullptr, 10);
            if (!currentTable1Reply(state.id, state.datasetId, revision, version)) return "OK ignored stale Table 1 result";
        } else if (table1Requests_.count(state.id)) return "OK ignored unversioned Table 1 result";
        state.title = request.args[cursor++];
        state.groupVariable = request.args[cursor++];
        auto readCount = [&](long &count) {
            if (cursor >= request.args.size()) return false;
            char *end = nullptr;
            count = std::strtol(request.args[cursor++].c_str(), &end, 10);
            return end && *end == '\0' && count >= 0;
        };
        long count = 0;
        if (!readCount(count) || cursor + static_cast<std::size_t>(count) > request.args.size())
            return "ERR invalid Table 1 variable count";
        for (long i = 0; i < count; ++i) state.variables.push_back(request.args[cursor++]);
        if (!readCount(count) || cursor + static_cast<std::size_t>(count) * 2 > request.args.size())
            return "ERR invalid Table 1 type count";
        for (long i = 0; i < count; ++i) {
            const std::string variable = request.args[cursor++];
            state.variableTypes[variable] = request.args[cursor++];
        }
        if (!readCount(count) || cursor + static_cast<std::size_t>(count) > request.args.size())
            return "ERR invalid Table 1 column count";
        for (long i = 0; i < count; ++i) state.columns.push_back(request.args[cursor++]);
        if (!readCount(count)) return "ERR invalid Table 1 row count";
        for (long i = 0; i < count; ++i) {
            if (cursor + 9 > request.args.size()) return "ERR malformed Table 1 row";
            Table1DisplayRow row;
            row.rowIndex = std::atoi(request.args[cursor++].c_str());
            row.rowType = request.args[cursor++];
            row.variable = request.args[cursor++];
            row.level = request.args[cursor++];
            row.label = request.args[cursor++];
            row.p = request.args[cursor++];
            row.test = request.args[cursor++];
            row.detail = request.args[cursor++];
            long valueCount = 0;
            if (!readCount(valueCount) || cursor + static_cast<std::size_t>(valueCount) > request.args.size())
                return "ERR invalid Table 1 row value count";
            for (long value = 0; value < valueCount; ++value) row.values.push_back(request.args[cursor++]);
            if (!row.p.empty()) state.showP = true;
            if (!row.test.empty()) state.showTest = true;
            state.rows.push_back(std::move(row));
        }
        if (!readCount(count) || cursor + static_cast<std::size_t>(count) > request.args.size())
            return "ERR invalid Table 1 footnote count";
        for (long i = 0; i < count; ++i) state.footnotes.push_back(request.args[cursor++]);
        if (cursor + 3 < request.args.size()) {
            const std::string scopeKind = request.args[cursor++];
            const std::string scopeDescription = request.args[cursor++];
            const long totalRows = std::max<long>(0, std::strtol(request.args[cursor++].c_str(), nullptr, 10));
            long scopeRowCount = 0;
            if (!readCount(scopeRowCount) || cursor + static_cast<std::size_t>(scopeRowCount) > request.args.size())
                return "ERR invalid Table 1 analysis-scope row count";
            std::vector<int> scopeRows;
            for (long i = 0; i < scopeRowCount; ++i)
                scopeRows.push_back(std::atoi(request.args[cursor++].c_str()));
            state.dataScope = scopeKind == "explicit"
                ? ExplicitAnalysisScope(state.datasetId, scopeRows,
                    AnalysisScopeSourceKind::OtherExplicitSubset, scopeDescription,
                    static_cast<std::size_t>(totalRows))
                : AllObservationsAnalysisScope(state.datasetId,
                    static_cast<std::size_t>(totalRows));
            state.dataScopeCaptured = true;
        }
        if (cursor + 1 < request.args.size()) {
            state.showP = request.args[cursor++] == "TRUE";
            state.showTest = request.args[cursor++] == "TRUE";
        }
        if (cursor < request.args.size() && request.args[cursor] == "TABLE1_DISPLAY_V1") {
            ++cursor;
            long semanticRowCount = 0;
            if (!readCount(semanticRowCount)) return "ERR invalid Table 1 semantic row count";
            for (long semanticRow = 0; semanticRow < semanticRowCount; ++semanticRow) {
                long rowIndex = 0;
                long semanticColumnCount = 0;
                if (!readCount(rowIndex) || !readCount(semanticColumnCount))
                    return "ERR malformed Table 1 semantic row";
                Table1DisplayRow *target = nullptr;
                for (Table1DisplayRow &candidate : state.rows) {
                    if (candidate.rowIndex == rowIndex) { target = &candidate; break; }
                }
                std::vector<std::map<std::string, std::string>> values;
                for (long semanticColumn = 0; semanticColumn < semanticColumnCount; ++semanticColumn) {
                    long componentCount = 0;
                    if (!readCount(componentCount) || cursor + static_cast<std::size_t>(componentCount) * 2 > request.args.size())
                        return "ERR invalid Table 1 statistic component count";
                    std::map<std::string, std::string> components;
                    for (long component = 0; component < componentCount; ++component) {
                        const std::string key = request.args[cursor++];
                        components[key] = request.args[cursor++];
                    }
                    values.push_back(std::move(components));
                }
                if (target) target->statisticValues = std::move(values);
            }
        }
        if (cursor < request.args.size() && request.args[cursor] == "TABLE1_RAW_V1") {
            ++cursor;
            long semanticRowCount = 0;
            if (!readCount(semanticRowCount)) return "ERR invalid Table 1 raw row count";
            for (long semanticRow = 0; semanticRow < semanticRowCount; ++semanticRow) {
                long rowIndex = 0;
                long semanticColumnCount = 0;
                if (!readCount(rowIndex) || !readCount(semanticColumnCount))
                    return "ERR malformed Table 1 raw row";
                Table1DisplayRow *target = nullptr;
                for (Table1DisplayRow &candidate : state.rows) {
                    if (candidate.rowIndex == rowIndex) { target = &candidate; break; }
                }
                std::vector<std::map<std::string, double>> values;
                for (long semanticColumn = 0; semanticColumn < semanticColumnCount; ++semanticColumn) {
                    long componentCount = 0;
                    if (!readCount(componentCount) ||
                        cursor + static_cast<std::size_t>(componentCount) * 2 > request.args.size())
                        return "ERR invalid Table 1 raw statistic component count";
                    std::map<std::string, double> components;
                    for (long component = 0; component < componentCount; ++component) {
                        const std::string key = request.args[cursor++];
                        components[key] = ParseOptionalDataCellDouble(request.args[cursor++]);
                    }
                    values.push_back(std::move(components));
                }
                if (target) target->rawStatisticValues = std::move(values);
            }
        }
        if (cursor < request.args.size() && request.args[cursor] == "TABLE_KIND_V1") {
            ++cursor;
            if (cursor >= request.args.size()) return "ERR missing structured table kind";
            state.tableType = request.args[cursor++];
            if (state.tableType != "nested_contingency" && state.tableType != "mi_contingency" && state.tableType != "mi_diagnostics" && state.tableType != "missing_data_overview" && state.tableType != "missing_data_test" && state.tableType != "missingness_descriptives" && state.tableType != "frequency") return "ERR unsupported structured table kind";
            if(state.tableType=="missing_data_test") {
                state.stubHeaders={"Test"};
                for(auto& row:state.rows)row.stubValues={row.label};
                state.statusText="Right-click the table for export options.";
            }
            if(state.tableType=="missing_data_overview") {
                state.stubHeaders={"Pattern"};
                for(auto& row:state.rows)row.stubValues={row.label};
                state.statusText="Right-click a table for export options.";
            }
            if (state.tableType == "mi_contingency") {
            state.statusText = "Descriptive averages across imputations. Click variable headers or right-click to edit the table.";
            std::string stub;
            for (const auto &variable : state.variables) {
                if (!stub.empty()) stub += " / ";
                stub += variable;
            }
            state.stubHeaders = {stub};
            for (auto &row : state.rows) row.stubValues = {row.label};
            state.groupSpanningHeader = state.groupVariable;
            state.groupSpanningColumnCount = static_cast<int>(state.columns.size()) - 1;
            }
        }
        if (cursor < request.args.size() && request.args[cursor] == "MI_CONTINGENCY_LAYOUT_V1") {
            ++cursor;
            if(cursor>=request.args.size()) return "ERR missing contingency display mode";
            state.nestedDisplayMode=request.args[cursor++];
            if(state.nestedDisplayMode!="count_percent" && state.nestedDisplayMode!="count" && state.nestedDisplayMode!="percent")
                return "ERR invalid contingency display mode";
            long count=0;
            if(!readCount(count) || cursor+static_cast<size_t>(count)>request.args.size())
                return "ERR malformed contingency headers";
            state.stubHeaders.assign(request.args.begin()+cursor,request.args.begin()+cursor+count);cursor+=count;
            if(!readCount(count) || static_cast<size_t>(count)!=state.rows.size())
                return "ERR malformed contingency layout rows";
            for(auto& row:state.rows) {
                long columns=0;
                if(!readCount(columns) || static_cast<size_t>(columns)!=state.stubHeaders.size() ||
                   cursor+static_cast<size_t>(columns)>request.args.size()) return "ERR malformed contingency row labels";
                row.stubValues.assign(request.args.begin()+cursor,request.args.begin()+cursor+columns);cursor+=columns;
                if(row.rowType!="nested_total" && !state.variables.empty()) row.variable=state.variables.back();
            }
        }
        if (cursor < request.args.size() && request.args[cursor] == "CONTINGENCY_ROWS_V1") {
            ++cursor;
            long rowCount = 0;
            if (!readCount(rowCount) || static_cast<size_t>(rowCount) != state.rows.size())
                return "ERR malformed contingency row identities";
            auto readRows = [&](std::vector<int>& rows) {
                long n = 0;
                if (!readCount(n) || cursor + static_cast<size_t>(n) > request.args.size()) return false;
                for (long i=0; i<n; ++i) {
                    char* end = nullptr;
                    const long id = std::strtol(request.args[cursor++].c_str(), &end, 10);
                    if (!end || *end || id <= 0 || id > state.dataScope.totalDatasetRows) return false;
                    if (state.dataScope.kind == AnalysisScopeKind::ExplicitRowIds &&
                        std::find(state.dataScope.originalRowIds.begin(), state.dataScope.originalRowIds.end(), id) == state.dataScope.originalRowIds.end()) return false;
                    rows.push_back(static_cast<int>(id));
                }
                return true;
            };
            for (auto& row : state.rows) {
                long n = 0;
                if (!readCount(n) || cursor + static_cast<size_t>(n) > request.args.size()) return "ERR invalid contingency key";
                if (static_cast<size_t>(n) != (row.rowType == "nested_total" ? 0 : state.variables.size())) return "ERR invalid contingency key length";
                row.contingencyRowKey.x_levels.assign(request.args.begin()+cursor, request.args.begin()+cursor+n); cursor+=n;
                if (!readRows(row.rowRows) || !readCount(n) || static_cast<size_t>(n) != state.columns.size())
                    return "ERR invalid contingency memberships";
                row.cellRows.resize(n);
                for (auto& ids : row.cellRows) if (!readRows(ids)) return "ERR invalid contingency cell identities";
            }
            state.groupVariableLabel = "Split";
            state.groupSpanningHeader = state.groupVariable;
            state.groupSpanningColumnCount = state.groupVariable.empty() ? 0 : static_cast<int>(state.columns.size())-1;
            state.statusText = state.footnotes.empty() ? "Calculated in R." : state.footnotes.front();
        }
        if (cursor < request.args.size() && request.args[cursor] == "MISSINGNESS_LAYOUT_V1") {
            ++cursor;
            if(cursor>=request.args.size())return "ERR missing source dataset";
            state.missingnessSource=request.args[cursor++];
            state.subtitle="Dataset: "+state.missingnessSource;
            long count=0;
            if(!readCount(count) || cursor+static_cast<size_t>(count)>request.args.size())return "ERR invalid missingness variable choices";
            state.addVariableOptions.assign(request.args.begin()+cursor,request.args.begin()+cursor+count);cursor+=count;
            if(!readCount(count) || static_cast<size_t>(count)!=state.columns.size())return "ERR invalid missingness patterns";
            for(long i=0;i<count;++i){
                if(cursor>=request.args.size())return "ERR missing pattern label";
                const auto label=request.args[cursor++];long n=0;
                if(!readCount(n) || cursor+static_cast<size_t>(n)>request.args.size())return "ERR invalid pattern row identities";
                auto& rows=state.missingnessPatternRows[label];
                for(long j=0;j<n;++j)rows.push_back(std::stoi(request.args[cursor++]));
            }
            state.stubHeaders={"Variable"};
            for(auto& row:state.rows)row.stubValues={row.label};
            state.statusText="Click a pattern to select cases. Right-click for descriptives, Little's test and Save to data.";
        }
        AnalysisProvenance provenance;
        {
            std::string provenanceError;
            if (!ReadAnalysisProvenancePayload(
                    request.args, cursor, provenance, &provenanceError)) {
                return "ERR malformed Table 1 provenance: " + provenanceError;
            }
        }
        if (modern) {
            const auto &task = table1Requests_.at(state.id).task;
            state.sourcePlotId = task.sourcePlotId; state.linkEnabled = task.linkEnabled;
            if ((task.analysisKind == "frequency" && state.tableType != "frequency") ||
                (task.analysisKind == "nested_contingency" && (state.tableType != "nested_contingency" ||
                    state.nestedDisplayMode != task.contingencyDisplayMode)) ||
                state.variables != task.variables || state.groupVariable != task.groupVariable ||
                !state.dataScopeCaptured || provenance.executedRCode.empty() ||
                provenance.dataVersion.version != task.dataVersion)
                return "ERR Table 1 result does not match its request";
            const bool selected = state.dataScope.kind == AnalysisScopeKind::ExplicitRowIds;
            if (selected != (task.scope == "selected") || (selected && state.dataScope.originalRowIds != task.rows))
                return "ERR Table 1 scope does not match its request";
            state.dataScope.sourceDescription = task.scopeDescription;
            if (!task.title.empty()) state.title = task.title;
            table1Requests_.at(state.id).pending = false;
        }
        if (state.columns.empty()) state.columns.push_back("Overall");
        state.nativeGenerated = true;
        if (const DataFrameModel *dataframe = applicationState_.datasets().find(state.datasetId);
            dataframe && !provenance.executedRCode.empty()) {
            const AnalysisScope scope = state.dataScopeCaptured
                ? state.dataScope : applicationState_.activeAnalysisScope(state.datasetId);
            CompleteAnalysisProvenance(provenance, *dataframe, scope, {}, {},
                                       state.id, state.title);
            state.codeReference = Table1CodeReference(state, provenance);
            applicationState_.registerOutputCodeReference(state.codeReference);
        }
        services_.ui.showTable1(state);
        return "OK\t" + state.id;
    }
    case CommandAction::CompareMeansBatchError: {
        if (request.args.size() < 3) return "ERR malformed COMPARE_MEANS_BATCH_ERROR command";
        if (services_.ui.showMeanComparisonError) {
            services_.ui.showMeanComparisonError(request.args[0], request.args.size() >= 3 ? request.args[2] : request.args[1]);
        }
        return "OK\t" + request.args[0];
    }
    case CommandAction::CompareMeansOpen: {
        if (request.args.size() < 14) return "ERR malformed COMPARE_MEANS_OPEN command";
        if (!services_.ui.showCompareMeansTable && !services_.ui.showCompareMeansReport) return std::nullopt;
        std::size_t cursor = 0;
        const std::string resultId = request.args[cursor++];
        const std::string datasetId = request.args[cursor++];
        const std::string analysisType = request.args[cursor++];
        const std::string title = request.args[cursor++];
        const std::string miNote = request.args[cursor++];
        const std::string statistic = request.args[cursor++];
        const std::string parameter1 = request.args[cursor++];
        const std::string parameter2 = request.args[cursor++];
        const std::string pValue = request.args[cursor++];
        const std::string difference = request.args[cursor++];
        const std::string differenceSE = request.args[cursor++];
        const std::string method = request.args[cursor++];
        const std::string directionNote = request.args[cursor++];
        std::string ciLower;
        std::string ciUpper;
        if (cursor < request.args.size() && request.args[cursor] == "COMPARE_MEANS_V2") {
            ++cursor;
            if (cursor + 1 >= request.args.size()) return "ERR malformed COMPARE_MEANS_OPEN V2 command";
            ciLower = request.args[cursor++];
            ciUpper = request.args[cursor++];
        }
        if (cursor >= request.args.size()) return "ERR malformed COMPARE_MEANS_OPEN command";

        auto readCount = [&](long &count) -> bool {
            if (cursor >= request.args.size()) return false;
            char *end = nullptr;
            count = std::strtol(request.args[cursor++].c_str(), &end, 10);
            return end && *end == '\0' && count >= 0;
        };
        auto numericValue = [](const std::string &raw, bool p = false) -> std::string {
            if (raw.empty() || raw == "NA" || raw == "NaN") return "\u2014";
            char *end = nullptr;
            const double value = std::strtod(raw.c_str(), &end);
            if (!end || *end != '\0') return raw;
            return p ? FormatPValue(value) : FormatModelNumberOrDash(value);
        };
        auto rawValue = [](const std::string &raw) -> std::string {
            return (raw == "NA" || raw == "NaN") ? "" : raw;
        };

        long specificationCount = 0;
        if (!readCount(specificationCount) || cursor + static_cast<std::size_t>(specificationCount) > request.args.size()) {
            return "ERR malformed COMPARE_MEANS_OPEN specification";
        }
        std::vector<std::pair<std::string, std::string>> specification;
        for (long index = 0; index < specificationCount; index += 2) {
            const std::string label = request.args[cursor++];
            const std::string value = index + 1 < specificationCount ? request.args[cursor++] : "";
            specification.push_back({label, value});
        }

        long descriptiveCount = 0;
        if (!readCount(descriptiveCount) || cursor + static_cast<std::size_t>(descriptiveCount) > request.args.size()) {
            return "ERR malformed COMPARE_MEANS_OPEN descriptives";
        }
        std::vector<std::string> descriptives(
            request.args.begin() + static_cast<std::ptrdiff_t>(cursor),
            request.args.begin() + static_cast<std::ptrdiff_t>(cursor + descriptiveCount));
        cursor += static_cast<std::size_t>(descriptiveCount);

        long effectSizeCount = 0;
        if (!readCount(effectSizeCount) || cursor + static_cast<std::size_t>(effectSizeCount * 2) > request.args.size()) {
            return "ERR malformed COMPARE_MEANS_OPEN effect sizes";
        }
        std::vector<std::pair<std::string, std::string>> effectSizes;
        for (long index = 0; index < effectSizeCount; ++index) {
            effectSizes.push_back({request.args[cursor], request.args[cursor + 1]});
            cursor += 2;
        }

        long postHocCount = 0;
        if (cursor < request.args.size()) {
            if (!readCount(postHocCount) || cursor + static_cast<std::size_t>(postHocCount) > request.args.size()) {
                return "ERR malformed COMPARE_MEANS_OPEN post-hoc block";
            }
        }
        std::vector<std::string> postHoc;
        for (long index = 0; index < postHocCount; ++index) postHoc.push_back(request.args[cursor++]);

        auto readRowVector = [&](std::vector<int> &rows) -> bool {
            long count = 0;
            if (!readCount(count) || cursor + static_cast<std::size_t>(count) > request.args.size()) return false;
            for (long index = 0; index < count; ++index) {
                rows.push_back(static_cast<int>(std::strtol(request.args[cursor++].c_str(), nullptr, 10)));
            }
            return true;
        };
        std::vector<int> rowsUsed;
        std::vector<int> rowsExcluded;
        std::string miStatus;
        if (cursor < request.args.size()) {
            if (!readRowVector(rowsUsed) || !readRowVector(rowsExcluded)) {
                return "ERR malformed COMPARE_MEANS_OPEN row mapping";
            }
            if (cursor < request.args.size()) miStatus = request.args[cursor++];
            if (cursor < request.args.size()) ++cursor; // result version
        }

        Table1DisplayState state;
        state.id = resultId;
        state.datasetId = datasetId;
        state.title = title;
        state.tableType = "compare_means";
        state.nativeGenerated = true;
        state.columns = {"N", "Mean", "SD", "SE", "Statistic", "df", "p",
                         "Difference", "Difference SE", "Lower 95%", "Upper 95%", "Effect size"};
        std::map<std::string, std::string> specMap;
        for (const auto &entry : specification) specMap[entry.first] = entry.second;
        auto specValue = [&](const std::string &key) -> std::string {
            auto it = specMap.find(key);
            return it == specMap.end() ? std::string() : it->second;
        };
        if (!specValue("Response:").empty()) state.subtitle = "Response: " + specValue("Response:");
        if (!specValue("Grouping variable:").empty()) state.subtitle +=
            (state.subtitle.empty() ? "" : "    ") + std::string("Group: ") + specValue("Grouping variable:");
        if (!specValue("Factor:").empty()) state.subtitle +=
            (state.subtitle.empty() ? "" : "    ") + std::string("Categorical predictor: ") + specValue("Factor:");
        if (!specValue("Variable 1:").empty()) state.subtitle =
            "Pair: " + specValue("Variable 1:") + " \u2212 " + specValue("Variable 2:");

        auto blankValues = []() { return std::vector<std::string>(12, ""); };
        auto addDescriptive = [&](const std::string &label, const std::string &n,
                                  const std::string &mean, const std::string &sd,
                                  const std::string &se) {
            Table1DisplayRow row;
            row.rowType = "numeric_level";
            row.label = label;
            row.values = blankValues();
            row.rawValues = blankValues();
            const std::string raw[] = {n, mean, sd, se};
            for (std::size_t index = 0; index < 4; ++index) {
                row.values[index] = numericValue(raw[index]);
                row.rawValues[index] = rawValue(raw[index]);
            }
            state.rows.push_back(row);
        };

        const bool grouped = analysisType == "independent_samples_t_test" || analysisType == "one_way_anova";
        if (grouped && descriptiveCount >= 10 && descriptiveCount % 5 == 0) {
            const std::size_t groups = static_cast<std::size_t>(descriptiveCount / 5 - 1);
            for (std::size_t group = 0; group < groups; ++group) {
                const std::size_t base = 5 * (group + 1);
                addDescriptive(descriptives[base], descriptives[base + 1], descriptives[base + 2],
                               descriptives[base + 3], descriptives[base + 4]);
            }
        } else {
            std::map<std::string, std::string> descMap;
            for (std::size_t index = 0; index + 1 < descriptives.size(); index += 2) {
                descMap[descriptives[index]] = descriptives[index + 1];
            }
            if (!descMap["Note"].empty()) state.footnotes.push_back(descMap["Note"]);
            if (analysisType == "paired_samples_t_test") {
                addDescriptive("Paired differences", descMap["Pairs analyzed"], descMap["Mean difference"],
                               descMap["SD of differences"], descMap["SE of differences"]);
                if (!descMap["Variable 1 mean"].empty()) addDescriptive(specValue("Variable 1:"), "", descMap["Variable 1 mean"], "", "");
                if (!descMap["Variable 2 mean"].empty()) addDescriptive(specValue("Variable 2:"), "", descMap["Variable 2 mean"], "", "");
            } else {
                addDescriptive(specValue("Response:").empty() ? "Sample" : specValue("Response:"),
                               descMap["N"], descMap["Mean"], descMap["SD"], descMap["SE"]);
            }
        }

        Table1DisplayRow testRow;
        testRow.rowType = "numeric_level";
        testRow.label = method.empty() ? "Test" : method;
        testRow.values = blankValues();
        testRow.rawValues = blankValues();
        const std::string statisticName = analysisType == "one_way_anova" ? "F" : "t";
        testRow.values[4] = statistic.empty() || statistic == "NA" ? "\u2014" : statisticName + " = " + numericValue(statistic);
        testRow.rawValues[4] = rawValue(statistic);
        if (!parameter2.empty() && parameter2 != "NA") {
            testRow.values[5] = numericValue(parameter1) + ", " + numericValue(parameter2);
            testRow.rawValues[5] = rawValue(parameter1) + "," + rawValue(parameter2);
        } else {
            testRow.values[5] = numericValue(parameter1);
            testRow.rawValues[5] = rawValue(parameter1);
        }
        testRow.values[6] = numericValue(pValue, true);
        testRow.rawValues[6] = rawValue(pValue);
        testRow.values[7] = numericValue(difference);
        testRow.rawValues[7] = rawValue(difference);
        testRow.values[8] = numericValue(differenceSE);
        testRow.rawValues[8] = rawValue(differenceSE);
        if (!ciLower.empty() || !ciUpper.empty()) {
            testRow.values[9] = numericValue(ciLower);
            testRow.rawValues[9] = rawValue(ciLower);
            testRow.values[10] = numericValue(ciUpper);
            testRow.rawValues[10] = rawValue(ciUpper);
        }
        if (!effectSizes.empty()) {
            testRow.values[11] = effectSizes.front().first + " = " + numericValue(effectSizes.front().second);
            testRow.rawValues[11] = rawValue(effectSizes.front().second);
        }
        state.rows.push_back(testRow);

        for (std::size_t index = 1; index < effectSizes.size(); ++index) {
            state.footnotes.push_back(effectSizes[index].first + ": " + numericValue(effectSizes[index].second));
        }
        if (!directionNote.empty()) state.footnotes.push_back(directionNote + ".");
        for (const auto &entry : specification) {
            if (entry.first == "Alternative:" || entry.first == "Confidence level:" ||
                entry.first == "Null hypothesis: \u03bc =" || entry.first.find("Equal variance") != std::string::npos) {
                state.footnotes.push_back(entry.first + (entry.second.empty() ? "" : " " + entry.second));
            }
        }
        if (!miNote.empty()) state.footnotes.push_back(miNote);
        if (!miStatus.empty() && miStatus != miNote) state.footnotes.push_back(miStatus);
        for (const std::string &note : postHoc) state.warnings.push_back(note);
        if (statistic.empty() || statistic == "NA" || statistic == "NaN") {
            state.warnings.push_back("The test statistic could not be estimated from the available data.");
        }
        if (analysisType != "one_way_anova" &&
            (ciLower.empty() || ciLower == "NA" || ciUpper.empty() || ciUpper == "NA")) {
            state.warnings.push_back("The confidence interval could not be estimated.");
        }
        state.statusText = "Calculated in R on " + std::to_string(rowsUsed.size()) + " rows; " +
            std::to_string(rowsExcluded.size()) + " excluded.";

        if (services_.ui.showCompareMeansTable) {
            services_.ui.showCompareMeansTable(state);
        } else {
            services_.ui.showCompareMeansReport(datasetId, title, Table1PlainText(state));
        }
        return "OK\t" + resultId;
    }
    case CommandAction::ModelOpenInteractionPlot: {
        if (request.args.size() < 2) return "ERR malformed MODEL_OPEN_INTERACTION_PLOT command";
        if (!services_.queries.groupSeed || !services_.ui.openLinearInteractionPlot) return std::nullopt;
        PlotModel seed;
        const std::string term = DecodeCommandField(request.args[1]);
        if (!services_.queries.groupSeed(request.args[0], seed)) {
            return "ERR no registered dataset/group for model";
        }
        const std::string id = services_.ui.openLinearInteractionPlot(request.args[0], term);
        return id.empty() ? "ERR could not open effect plot" : "OK\t" + id;
    }
    case CommandAction::ModelInteractionReport: {
        if (request.args.size() < 2) return "ERR malformed MODEL_INTERACTION_REPORT command";
        if (!services_.queries.groupSeed) return std::nullopt;
        const std::string group = request.args[0];
        const std::string term = DecodeCommandField(request.args[1]);
        PlotModel seed;
        if (!services_.queries.groupSeed(group, seed)) return "ERR no registered dataset/group for model";
        GroupModelState &state = EnsureGroupModelState(applicationState_.groupModels(), group, &seed);
        const DataFrameModel *dataframe = applicationState_.datasets().find(group);
        if (dataframe) {
            SynchronizeGroupModelTermTypes(state, *dataframe);
        }
        const auto effectiveTermTypes =
            EffectiveModelSpecificationTermTypes(state);
        GLMFitSummary fit;
        const auto fitted = applicationState_.linearModelFits().find(group);
        if (fitted != applicationState_.linearModelFits().end() && fitted->second.ok) {
            fit = fitted->second;
        } else {
            return "ERR linear interaction report requires a completed R fit; refit the model and retry";
        }
        const GLMInteractionReport report = BuildGLMInteractionReport(
            seed, dataframe, term, state.terms, effectiveTermTypes, fit, 0.95,
            state.centeredPredictors);
        return "OK\t" + EncodeCommandField(GLMInteractionReportText(report));
    }
    case CommandAction::ModelOpen:
    case CommandAction::ModelOpenResidualsFitted:
    case CommandAction::ModelOpenObservedFitted:
    case CommandAction::ModelOpenDiagnostics:
    case CommandAction::ModelOpenDiagnostic: {
        if (request.args.empty()) return "ERR missing group name";
        if (!services_.queries.groupPlot || !services_.ui.refreshModelGroup) return std::nullopt;
        PlotModel plot;
        if (!services_.queries.groupPlot(request.args[0], plot)) return "ERR no active plot/group";
        services_.ui.refreshModelGroup(request.args[0]);
        if (request.action == CommandAction::ModelOpen) {
            if (!services_.ui.showLinearModel) return std::nullopt;
            services_.ui.showLinearModel(request.args[0]);
            return "OK";
        }
        if (!services_.ui.openLinearModelDiagnostic) return std::nullopt;
        std::string type;
        if (request.action == CommandAction::ModelOpenObservedFitted) {
            type = "observed_fitted";
        } else if (request.action == CommandAction::ModelOpenDiagnostic) {
            if (request.args.size() < 2) return "ERR missing diagnostic type";
            type = NormalizeLinearDiagnosticKind(request.args[1]);
            if (!LinearDiagnosticKindIsImplemented(type)) {
                return "ERR diagnostic `" + request.args[1] + "` is not available for this linear model";
            }
        } else {
            type = "residuals_fitted";
        }
        const std::string id = services_.ui.openLinearModelDiagnostic(request.args[0], type);
        return id.empty() ? "ERR diagnostic plot could not be opened" : "OK\t" + id;
    }
    case CommandAction::GeneralizedGLMOpenStructured: {
        if (request.args.size() < 10) return "ERR malformed GENERALIZED_GLM_OPEN_STRUCTURED command";
        if (!services_.queries.groupSeed || !services_.ui.refreshGeneralizedGLM) return std::nullopt;
        std::size_t cursor = 0;
        GeneralizedGLMState state;
        state.id = request.args[cursor++];
        state.group = request.args[cursor++];
        state.title = request.args[cursor++];
        state.response = request.args[cursor++];
        state.family = request.args[cursor++];
        state.link = request.args[cursor++];
        state.scope = request.args[cursor++];
        state.note = request.args[cursor++];
        const long termCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
        if (termCount < 0 || cursor + static_cast<std::size_t>(termCount) > request.args.size()) {
            return "ERR invalid generalized GLM term count";
        }
        for (long index = 0; index < termCount; ++index) state.terms.push_back(request.args[cursor++]);
        if (cursor >= request.args.size()) return "ERR invalid generalized GLM type count";
        const long typeCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
        if (typeCount < 0 || cursor + static_cast<std::size_t>(2 * typeCount) > request.args.size()) {
            return "ERR invalid generalized GLM type count";
        }
        for (long index = 0; index < typeCount; ++index) {
            const std::string term = request.args[cursor++];
            const std::string type = request.args[cursor++];
            if (!term.empty() && (type == "numeric" || type == "factor")) state.termTypes[term] = type;
        }
        // Generalized-GLM term types received from R are model-local
        // interpretations.  Keep them in the canonical override layer so a
        // later metadata refresh cannot silently revert the specification.
        state.termTypeOverrides = state.termTypes;
        if (cursor < request.args.size() && request.args[cursor] == "MODEL_SPEC_V1") {
            ++cursor;
            if (cursor >= request.args.size()) {
                return "ERR invalid generalized GLM centered predictor count";
            }
            const long centeredCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (centeredCount < 0 ||
                cursor + static_cast<std::size_t>(centeredCount) > request.args.size()) {
                return "ERR invalid generalized GLM centered predictor count";
            }
            for (long index = 0; index < centeredCount; ++index) {
                const std::string predictor = request.args[cursor++];
                if (!predictor.empty()) state.centeredPredictors.insert(predictor);
            }
            if (cursor >= request.args.size()) {
                return "ERR invalid generalized GLM factor reference count";
            }
            const long referenceCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (referenceCount < 0 ||
                cursor + static_cast<std::size_t>(2 * referenceCount) > request.args.size()) {
                return "ERR invalid generalized GLM factor reference count";
            }
            for (long index = 0; index < referenceCount; ++index) {
                const std::string predictor = request.args[cursor++];
                const std::string referenceLevel = request.args[cursor++];
                if (!predictor.empty() && !referenceLevel.empty()) {
                    state.factorReferenceLevels[predictor] = referenceLevel;
                }
            }
        }
        if (cursor < request.args.size() && request.args[cursor] == "ANALYSIS_SCOPE_V1") {
            ++cursor;
            if (cursor + 5 > request.args.size()) return "ERR malformed generalized GLM analysis scope";
            const std::string scopeKind = request.args[cursor++];
            const std::string sourceKind = request.args[cursor++];
            const std::string description = request.args[cursor++];
            const long totalRows = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            const long rowCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (totalRows < 0 || rowCount < 0 ||
                cursor + static_cast<std::size_t>(rowCount) > request.args.size()) {
                return "ERR malformed generalized GLM analysis scope rows";
            }
            std::vector<int> rows;
            rows.reserve(static_cast<std::size_t>(rowCount));
            for (long index = 0; index < rowCount; ++index) {
                rows.push_back(static_cast<int>(std::strtol(request.args[cursor++].c_str(), nullptr, 10)));
            }
            state.dataScope = scopeKind == "all"
                ? AllObservationsAnalysisScope(state.group, static_cast<std::size_t>(totalRows))
                : ExplicitAnalysisScope(
                    state.group, rows, AnalysisScopeSourceKindFromId(sourceKind), description,
                    static_cast<std::size_t>(totalRows));
            std::string scopeError;
            if ((scopeKind != "all" && scopeKind != "explicit") ||
                !ValidateAnalysisScope(
                    state.dataScope, state.group, static_cast<std::size_t>(totalRows), &scopeError)) {
                return "ERR invalid generalized GLM analysis scope" +
                    (scopeError.empty() ? std::string() : ": " + scopeError);
            }
            state.dataScopeCaptured = true;
        }
        if (cursor < request.args.size() && request.args[cursor] == "BOUNDED_RESPONSE_V1") {
            ++cursor;
            if (cursor + 2 > request.args.size()) {
                return "ERR malformed generalized GLM response bounds";
            }
            char *lowerEnd = nullptr;
            char *upperEnd = nullptr;
            state.responseLower = std::strtod(request.args[cursor++].c_str(), &lowerEnd);
            state.responseUpper = std::strtod(request.args[cursor++].c_str(), &upperEnd);
            state.responseBoundsConfigured = lowerEnd && upperEnd && *lowerEnd == '\0' &&
                *upperEnd == '\0' && std::isfinite(state.responseLower) &&
                std::isfinite(state.responseUpper) && state.responseLower < state.responseUpper;
            if (!state.responseBoundsConfigured) {
                return "ERR invalid generalized GLM response bounds";
            }
        }
        if (cursor < request.args.size() && request.args[cursor] == "OFFSET_SPEC_V1") {
            ++cursor;
            if (cursor >= request.args.size()) return "ERR malformed generalized GLM offset";
            state.offsetVariable = request.args[cursor++];
        }
        if (!services_.queries.groupSeed(state.group, state.seed)) {
            return "ERR no registered dataset/group for generalized GLM";
        }
        state.hasSeed = true;
        if (!ReadGeneralizedStatePayload(request.args, cursor, state)) {
            return "ERR malformed generalized GLM fit payload";
        }
        std::uint64_t resultGeneration = 0;
        if (cursor < request.args.size() && request.args[cursor] == "GGLM_RESULT_V1") {
            ++cursor;
            if (cursor >= request.args.size()) {
                return "ERR missing generalized GLM fit generation";
            }
            char *end = nullptr;
            resultGeneration = std::strtoull(request.args[cursor++].c_str(), &end, 10);
            if (!end || end == request.args[cursor - 1].c_str() || *end != '\0') {
                return "ERR invalid generalized GLM fit generation";
            }
        }
        AnalysisProvenance returnedProvenance;
        std::string provenanceError;
        if (!ReadAnalysisProvenancePayload(
                request.args, cursor, returnedProvenance, &provenanceError)) {
            return "ERR malformed generalized GLM provenance: " + provenanceError;
        }
        state.modelVersion = 1;
        state.fitVersion = 1;
        state.diagnosticsVersion = 1;
        state.lastRFitSignature = GeneralizedGLMFitSignature(state);
        auto &models = applicationState_.generalizedGLMs();
        auto old = models.find(state.id);
        if (old != models.end()) {
            const bool versionedRequest = old->second.rFitGeneration > 0;
            if (versionedRequest &&
                (resultGeneration != old->second.rFitGeneration ||
                 !old->second.rFitPending)) {
                return "OK\t" + state.id;
            }
            const std::string signature = GeneralizedGLMFitSignature(state);
            const std::string currentSignature = GeneralizedGLMFitSignature(old->second);
            GeneralizedGLMState initialCodingCandidate = state;
            initialCodingCandidate.responseCoding = old->second.responseCoding;
            const bool acceptsInitialBinaryCoding = old->second.binaryRegression &&
                !old->second.responseCodingExplicit && state.binaryRegression &&
                GeneralizedGLMFitSignature(initialCodingCandidate) == currentSignature;
            // A result may arrive after a term was added, removed, replaced, or
            // reinterpreted.  It belongs to the specification serialized by R,
            // not necessarily to the model that is currently visible.  Compare
            // with that current canonical specification even when the pending
            // flag has already been cleared (for example by emptying the model).
            if (signature != currentSignature && !acceptsInitialBinaryCoding) {
                // A response for the current generation that does not carry
                // its exact immutable specification is a terminal error for
                // that request.  Leaving it pending would make Auto-refit
                // appear to hang forever; never launch a replacement loop.
                if (versionedRequest) {
                    old->second.rFitPending = false;
                    old->second.status =
                        "Rejected generalized-model result because its specification did not match the pending request.";
                    services_.ui.refreshGeneralizedGLM(state.id);
                }
                return "OK\t" + state.id;
            }
            state.autoRefit = old->second.autoRefit;
            state.diagnosticOptions = old->second.diagnosticOptions;
            state.modelVersion = std::max(1, old->second.modelVersion);
            state.fitVersion = state.modelVersion;
            state.diagnosticsVersion = state.modelVersion;
            state.lastRFitSignature = signature;
            state.rFitGeneration = old->second.rFitGeneration;
            if (!state.dataScopeCaptured && old->second.dataScopeCaptured) {
                state.dataScope = old->second.dataScope;
                state.dataScopeCaptured = true;
            }
        } else {
            state.rFitGeneration = resultGeneration;
        }
        // A failed fit is a result state, not a user preference.  Preserve
        // Auto-refit so changing the specification can recover immediately.
        state.rFitPending = false;
        if (!state.ok && GeneralizedFamilyRequiresResponseBounds(state.family) &&
            state.status.find("'arg' should be one of") != std::string::npos &&
            state.status.find("beta_one_inflated") == std::string::npos) {
            state.status =
                "The connected R session has an older LinkEDA namespace loaded. "
                "Restart the R session and reopen or refit this model.";
        }
        if (!state.dataScopeCaptured) {
            if (const DataFrameModel *dataframe = applicationState_.datasets().find(state.group)) {
                if (state.scope == "all") {
                    state.dataScope = AllObservationsAnalysisScope(
                        state.group, static_cast<std::size_t>(dataframe->rows));
                } else {
                    std::vector<int> requested = state.rowsUsed;
                    requested.insert(requested.end(), state.rowsExcluded.begin(), state.rowsExcluded.end());
                    state.dataScope = ExplicitAnalysisScope(
                        state.group, requested,
                        state.scope == "selected" ? AnalysisScopeSourceKind::CurrentSelection :
                            AnalysisScopeSourceKind::OtherExplicitSubset,
                        state.scope == "selected" ? "Selected observations" : "Unselected observations",
                        static_cast<std::size_t>(dataframe->rows));
                }
            } else {
                state.dataScope = applicationState_.activeAnalysisScope(state.group);
            }
            state.dataScopeCaptured = true;
        }
        if (const DataFrameModel *dataframe = applicationState_.datasets().find(state.group);
            dataframe && !returnedProvenance.executedRCode.empty()) {
            CompleteAnalysisProvenance(
                returnedProvenance, *dataframe, state.dataScope,
                state.rowsUsed, state.rowsExcluded, state.id,
                state.title.empty() ? "Generalized Linear Model" : state.title);
            state.provenance = std::move(returnedProvenance);
            applicationState_.registerOutputCodeReference(
                GeneralizedModelCodeReference(state));
        }
        models[state.id] = std::move(state);
        services_.ui.refreshGeneralizedGLM(request.args[0]);
        return "OK\t" + request.args[0];
    }
    case CommandAction::GeneralizedGLMOpen: {
        if (request.args.size() < 6) return "ERR malformed GENERALIZED_GLM_OPEN command";
        if (!services_.queries.groupSeed || !services_.queries.createGeneralizedGLMId ||
            !services_.ui.showGeneralizedGLM || !services_.ui.requestGeneralizedGLMFit) {
            return std::nullopt;
        }
        GeneralizedGLMState state;
        state.group = request.args[0];
        state.response = request.args[1];
        state.family = request.args[2];
        state.link = request.args[3];
        if (!IsValidGeneralizedFamily(state.family)) return "ERR invalid generalized linear model family";
        if (!IsValidGeneralizedLink(state.family, state.link)) return "ERR invalid generalized linear model link";
        if (!services_.queries.groupSeed(state.group, state.seed)) {
            return "ERR no registered dataset/group for generalized linear model";
        }
        state.hasSeed = true;
        if (!FindNumericVariable(state.seed, state.response)) return "ERR response variable must be numeric";
        const long termCount = std::strtol(request.args[4].c_str(), nullptr, 10);
        if (termCount < 0 || request.args.size() < static_cast<std::size_t>(5 + termCount)) {
            return "ERR invalid generalized linear model term count";
        }
        const DataFrameModel *dataframe = applicationState_.datasets().find(state.group);
        const std::vector<std::string> available = AvailableVariableNames(state.seed, dataframe);
        for (long index = 0; index < termCount; ++index) {
            const std::string &term = request.args[static_cast<std::size_t>(5 + index)];
            if (!ModelTermExistsForVariables(available, term, state.response)) continue;
            AddHierarchicalTermsToVector(available, state.response, state.terms, term);
        }
        state.id = services_.queries.createGeneralizedGLMId(state.group);
        if (state.id.empty()) return "ERR could not create generalized linear model";
        state.modelVersion = 1;
        state.status = "Fitting generalized linear model in the main R session...";
        applicationState_.generalizedGLMs()[state.id] = state;
        services_.ui.showGeneralizedGLM(state.id);
        return services_.ui.requestGeneralizedGLMFit(state.id)
            ? "OK\t" + state.id : "ERR could not fit generalized linear model";
    }
    case CommandAction::GeneralizedGLMOpenPooled: {
        if (request.args.size() < 11) return "ERR malformed GENERALIZED_GLM_OPEN_POOLED command";
        if (!services_.queries.groupSeed || !services_.ui.showGeneralizedGLM) return std::nullopt;
        std::size_t cursor = 0;
        GeneralizedGLMState state;
        state.id = request.args[cursor++];
        state.group = request.args[cursor++];
        state.title = request.args[cursor++];
        state.response = request.args[cursor++];
        state.family = request.args[cursor++];
        state.link = request.args[cursor++];
        state.scope = request.args[cursor++];
        state.precomputed = true;
        state.multipleImputation = true;
        state.autoRefit = false;
        state.imputationCount = std::atoi(request.args[cursor++].c_str());
        state.note = request.args[cursor++];
        const long termCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
        if (termCount < 0 || cursor + static_cast<std::size_t>(termCount) > request.args.size()) {
            return "ERR invalid pooled generalized GLM term count";
        }
        for (long index = 0; index < termCount; ++index) state.terms.push_back(request.args[cursor++]);
        if (cursor < request.args.size() && request.args[cursor] == "MODEL_SPEC_V1") {
            ++cursor;
            if (cursor >= request.args.size()) {
                return "ERR invalid pooled generalized GLM type count";
            }
            const long typeCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (typeCount < 0 ||
                cursor + static_cast<std::size_t>(2 * typeCount) > request.args.size()) {
                return "ERR invalid pooled generalized GLM type count";
            }
            for (long index = 0; index < typeCount; ++index) {
                const std::string term = request.args[cursor++];
                const std::string type = request.args[cursor++];
                if (!term.empty() && (type == "numeric" || type == "factor")) {
                    state.termTypes[term] = type;
                    state.termTypeOverrides[term] = type;
                }
            }
            if (cursor >= request.args.size()) {
                return "ERR invalid pooled generalized GLM centered predictor count";
            }
            const long centeredCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (centeredCount < 0 ||
                cursor + static_cast<std::size_t>(centeredCount) > request.args.size()) {
                return "ERR invalid pooled generalized GLM centered predictor count";
            }
            for (long index = 0; index < centeredCount; ++index) {
                const std::string predictor = request.args[cursor++];
                if (!predictor.empty()) state.centeredPredictors.insert(predictor);
            }
            if (cursor >= request.args.size()) {
                return "ERR invalid pooled generalized GLM factor reference count";
            }
            const long referenceCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (referenceCount < 0 ||
                cursor + static_cast<std::size_t>(2 * referenceCount) > request.args.size()) {
                return "ERR invalid pooled generalized GLM factor reference count";
            }
            for (long index = 0; index < referenceCount; ++index) {
                const std::string predictor = request.args[cursor++];
                const std::string reference = request.args[cursor++];
                if (!predictor.empty() && !reference.empty()) {
                    state.factorReferenceLevels[predictor] = reference;
                }
            }
        }
        if (cursor < request.args.size() && request.args[cursor] == "ANALYSIS_SCOPE_V1") {
            ++cursor;
            if (cursor + 5 > request.args.size()) {
                return "ERR malformed pooled generalized GLM analysis scope";
            }
            const std::string scopeKind = request.args[cursor++];
            const std::string sourceKind = request.args[cursor++];
            const std::string description = request.args[cursor++];
            const long totalRows = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            const long rowCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (totalRows < 0 || rowCount < 0 ||
                cursor + static_cast<std::size_t>(rowCount) > request.args.size()) {
                return "ERR malformed pooled generalized GLM analysis scope rows";
            }
            std::vector<int> rows;
            rows.reserve(static_cast<std::size_t>(rowCount));
            for (long index = 0; index < rowCount; ++index) {
                rows.push_back(static_cast<int>(std::strtol(request.args[cursor++].c_str(), nullptr, 10)));
            }
            state.dataScope = scopeKind == "all"
                ? AllObservationsAnalysisScope(state.group, static_cast<std::size_t>(totalRows))
                : ExplicitAnalysisScope(
                    state.group, rows, AnalysisScopeSourceKindFromId(sourceKind), description,
                    static_cast<std::size_t>(totalRows));
            std::string scopeError;
            if ((scopeKind != "all" && scopeKind != "explicit") ||
                !ValidateAnalysisScope(
                    state.dataScope, state.group, static_cast<std::size_t>(totalRows), &scopeError)) {
                return "ERR invalid pooled generalized GLM analysis scope" +
                    (scopeError.empty() ? std::string() : ": " + scopeError);
            }
            state.dataScopeCaptured = true;
        }
        if (cursor < request.args.size() && request.args[cursor] == "BOUNDED_RESPONSE_V1") {
            ++cursor;
            if (cursor + 2 > request.args.size()) {
                return "ERR malformed pooled generalized GLM response bounds";
            }
            char *lowerEnd = nullptr;
            char *upperEnd = nullptr;
            state.responseLower = std::strtod(request.args[cursor++].c_str(), &lowerEnd);
            state.responseUpper = std::strtod(request.args[cursor++].c_str(), &upperEnd);
            state.responseBoundsConfigured = lowerEnd && upperEnd && *lowerEnd == '\0' &&
                *upperEnd == '\0' && std::isfinite(state.responseLower) &&
                std::isfinite(state.responseUpper) && state.responseLower < state.responseUpper;
            if (!state.responseBoundsConfigured) {
                return "ERR invalid pooled generalized GLM response bounds";
            }
        }
        if (cursor < request.args.size() && request.args[cursor] == "OFFSET_SPEC_V1") {
            ++cursor;
            if (cursor >= request.args.size()) return "ERR malformed pooled generalized GLM offset";
            state.offsetVariable = request.args[cursor++];
        }
        if (!services_.queries.groupSeed(state.group, state.seed)) {
            return "ERR no registered dataset/group for pooled generalized GLM";
        }
        state.hasSeed = true;
        if (!ReadGeneralizedStatePayload(request.args, cursor, state)) {
            return "ERR malformed pooled generalized GLM fit payload";
        }
        std::uint64_t resultGeneration = 0;
        if (cursor < request.args.size() && request.args[cursor] == "GGLM_RESULT_V1") {
            ++cursor;
            if (cursor >= request.args.size()) {
                return "ERR missing pooled generalized GLM fit generation";
            }
            char *end = nullptr;
            resultGeneration = std::strtoull(request.args[cursor++].c_str(), &end, 10);
            if (!end || end == request.args[cursor - 1].c_str() || *end != '\0') {
                return "ERR invalid pooled generalized GLM fit generation";
            }
        }
        AnalysisProvenance returnedProvenance;
        std::string provenanceError;
        if (!ReadAnalysisProvenancePayload(
                request.args, cursor, returnedProvenance, &provenanceError)) {
            return "ERR malformed pooled generalized GLM provenance: " + provenanceError;
        }
        auto &generalizedModels = applicationState_.generalizedGLMs();
        auto previous = generalizedModels.find(state.id);
        if (previous != generalizedModels.end() && previous->second.rFitGeneration > 0) {
            if (resultGeneration != previous->second.rFitGeneration ||
                !previous->second.rFitPending) {
                return "OK\t" + state.id;
            }
            const std::string resultSignature = GeneralizedGLMFitSignature(state);
            const std::string pendingSignature = GeneralizedGLMFitSignature(previous->second);
            GeneralizedGLMState initialCodingCandidate = state;
            initialCodingCandidate.responseCoding = previous->second.responseCoding;
            const bool acceptsInitialBinaryCoding = previous->second.binaryRegression &&
                !previous->second.responseCodingExplicit && state.binaryRegression &&
                GeneralizedGLMFitSignature(initialCodingCandidate) == pendingSignature;
            if (resultSignature != pendingSignature && !acceptsInitialBinaryCoding) {
                previous->second.rFitPending = false;
                previous->second.status =
                    "Rejected pooled generalized-model result because its specification did not match the pending request.";
                services_.ui.refreshGeneralizedGLM(state.id);
                return "OK\t" + state.id;
            }
            state.rFitGeneration = previous->second.rFitGeneration;
            state.modelVersion = previous->second.modelVersion;
            state.autoRefit = previous->second.autoRefit;
            state.diagnosticOptions = previous->second.diagnosticOptions;
            state.lastRFitSignature = resultSignature;
        } else {
            state.rFitGeneration = resultGeneration;
            state.lastRFitSignature = GeneralizedGLMFitSignature(state);
        }
        state.rFitPending = false;
        // Do not silently change Auto-refit after an R failure.
        state.fitVersion = std::max(1, state.modelVersion);
        state.diagnosticsVersion = std::max(1, state.modelVersion);
        if (const DataFrameModel *dataframe = applicationState_.datasets().find(state.group);
            dataframe && !returnedProvenance.executedRCode.empty()) {
            if (!state.dataScopeCaptured) {
                if (state.scope == "all") {
                    state.dataScope = AllObservationsAnalysisScope(
                        state.group, static_cast<std::size_t>(dataframe->rows));
                } else {
                    std::vector<int> requested = state.rowsUsed;
                    requested.insert(requested.end(), state.rowsExcluded.begin(), state.rowsExcluded.end());
                    state.dataScope = ExplicitAnalysisScope(
                        state.group, requested,
                        state.scope == "selected" ? AnalysisScopeSourceKind::CurrentSelection :
                            AnalysisScopeSourceKind::OtherExplicitSubset,
                        state.scope == "selected" ? "Selected observations" : "Unselected observations",
                        static_cast<std::size_t>(dataframe->rows));
                }
                state.dataScopeCaptured = true;
            }
            CompleteAnalysisProvenance(
                returnedProvenance, *dataframe, state.dataScope,
                state.rowsUsed, state.rowsExcluded, state.id, state.title);
            state.provenance = std::move(returnedProvenance);
            applicationState_.registerOutputCodeReference(
                GeneralizedModelCodeReference(state));
        }
        generalizedModels[state.id] = state;
        services_.ui.showGeneralizedGLM(state.id);
        return "OK\t" + state.id;
    }
    case CommandAction::MixedModelOpenStructured: {
        if (request.args.size() < 11) return "ERR malformed MIXED_MODEL_OPEN_STRUCTURED command";
        if (!services_.ui.showMixedModel) return std::nullopt;
        NativeMixedModelState state;
        state.id = request.args[0];
        state.group = request.args[1];
        state.modelType = request.args[2];
        state.response = request.args[3];
        state.method = request.args[4].empty()
            ? (state.modelType == "linear_mixed_model" ? "REML" : "ML") : request.args[4];
        state.family = request.args[5].empty() ? "binomial" : request.args[5];
        state.link = request.args[6].empty() ? "logit" : request.args[6];
        std::size_t cursor = 7;
        const long fixedCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
        if (fixedCount < 0 || cursor + static_cast<std::size_t>(fixedCount) > request.args.size()) {
            return "ERR invalid mixed model fixed-effect payload";
        }
        for (long i = 0; i < fixedCount; ++i) state.fixedEffects.push_back(request.args[cursor++]);
        if (cursor >= request.args.size()) return "ERR missing mixed model random-effect count";
        const long randomCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
        if (randomCount < 0) return "ERR invalid mixed model random-effect count";
        for (long i = 0; i < randomCount; ++i) {
            if (cursor + 3 > request.args.size()) return "ERR malformed mixed model random-effect payload";
            NativeMixedRandomSpec spec;
            spec.group = request.args[cursor++];
            spec.covariance = request.args[cursor++] == "||" ? "||" : "|";
            const long terms = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (terms < 0 || cursor + static_cast<std::size_t>(terms) > request.args.size()) {
                return "ERR invalid mixed model random-slope payload";
            }
            for (long j = 0; j < terms; ++j) spec.terms.push_back(request.args[cursor++]);
            if (spec.terms.empty()) spec.terms.push_back("1");
            state.randomEffects.push_back(std::move(spec));
        }
        if (cursor >= request.args.size()) return "ERR missing mixed model report payload";
        const long lines = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
        if (lines < 0 || cursor + static_cast<std::size_t>(lines) > request.args.size()) {
            return "ERR invalid mixed model report payload";
        }
        std::ostringstream report;
        for (long i = 0; i < lines; ++i) {
            if (i) report << "\n";
            report << request.args[cursor++];
        }
        state.reportText = report.str();
        applicationState_.nativeMixedModels()[state.id] = state;
        services_.ui.showMixedModel(state);
        return "OK\t" + state.id;
    }
    case CommandAction::MixedModelOpenText: {
        if (request.args.size() < 4) return "ERR malformed MIXED_MODEL_OPEN_TEXT command";
        if (!services_.ui.showMixedModelText) return std::nullopt;
        const long count = std::strtol(request.args[3].c_str(), nullptr, 10);
        if (count < 0 || request.args.size() < static_cast<std::size_t>(4 + count)) {
            return "ERR invalid mixed model payload";
        }
        std::ostringstream text;
        for (long index = 0; index < count; ++index) {
            if (index) text << "\n";
            text << request.args[static_cast<std::size_t>(4 + index)];
        }
        services_.ui.showMixedModelText(request.args[0], request.args[1], request.args[2], text.str());
        return "OK\t" + request.args[0];
    }
    case CommandAction::GeneralizedGLMOpenDiagnostic: {
        if (request.args.size() < 2) return "ERR malformed GENERALIZED_GLM_OPEN_DIAGNOSTIC command";
        if (!services_.ui.openGeneralizedGLMDiagnostic) return std::nullopt;
        const std::string id = services_.ui.openGeneralizedGLMDiagnostic(request.args[0], request.args[1]);
        return id.empty() ? "ERR generalized linear model diagnostic could not be opened" : "OK\t" + id;
    }
    case CommandAction::GeneralizedComparisonUpdateError: {
        if (request.args.size() < 3) {
            return "ERR malformed GENERALIZED_COMPARISON_UPDATE_ERROR command";
        }
        auto found = applicationState_.generalizedComparisons().find(request.args[0]);
        if (found == applicationState_.generalizedComparisons().end()) {
            return "OK\t" + request.args[0];
        }
        const int generation = std::atoi(request.args[1].c_str());
        if (generation > 0 &&
            (generation != found->second.rFitGeneration || !found->second.rFitPending)) {
            return "OK\t" + request.args[0];
        }
        found->second.rFitPending = false;
        for (GeneralizedComparisonModel &model : found->second.models) {
            model.isStale = true;
            model.fit.ok = false;
            model.fit.status = request.args[2];
            model.fitState = RegressionComparisonFitState::Error;
        }
        RefreshGeneralizedTermRowsFromFits(found->second);
        for (GeneralizedComparisonModel &model : found->second.models) {
            model.fitState = RegressionComparisonFitState::Error;
        }
        if (services_.ui.showGeneralizedComparison) {
            services_.ui.showGeneralizedComparison(request.args[0]);
        }
        return "OK\t" + request.args[0];
    }
    case CommandAction::GeneralizedComparisonOpenStructured: {
        if (request.args.size() < 7) return "ERR malformed generalized comparison payload";
        std::size_t cursor = 0;
        GeneralizedComparisonState state;
        state.id = request.args[cursor++];
        state.group = request.args[cursor++];
        state.response = request.args[cursor++];
        state.responseCoding.eventLabel = request.args[cursor++];
        state.responseCoding.referenceLabel = request.args[cursor++];
        state.responseCoding.eventValue = state.responseCoding.eventLabel;
        state.responseCoding.referenceValue = state.responseCoding.referenceLabel;
        state.binaryComparison = !state.responseCoding.eventLabel.empty() ||
                                 !state.responseCoding.referenceLabel.empty();
        state.responseCoding.ok = state.binaryComparison;
        state.family = state.binaryComparison ? "binomial" : "";
        const bool commonRows = request.args[cursor++] == "TRUE";
        const long modelCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
        const long minimumModels = 1;
        if (state.id.empty() || state.group.empty() || modelCount < minimumModels) {
            return "ERR invalid generalized comparison payload";
        }

        auto &comparisons = applicationState_.generalizedComparisons();
        auto previous = comparisons.find(state.id);
        const bool hadPrevious = previous != comparisons.end();
        state.models.reserve(static_cast<std::size_t>(modelCount));
        for (long index = 0; index < modelCount; ++index) {
            if (cursor + 14 > request.args.size()) {
                return "ERR incomplete generalized comparison model";
            }
            GeneralizedComparisonModel model;
            model.id = request.args[cursor++];
            model.label = request.args[cursor++];
            model.response = state.response;
            std::string familyAndLink = request.args[cursor++];
            const auto separator = familyAndLink.find('|');
            model.family = separator == std::string::npos
                ? (state.binaryComparison ? "binomial" : "gaussian")
                : familyAndLink.substr(0, separator);
            model.link = separator == std::string::npos
                ? familyAndLink : familyAndLink.substr(separator + 1);
            if (state.family.empty()) state.family = model.family;
            if (state.link.empty() || index == 0) state.link = model.link;
            model.fit.id = model.id;
            model.fit.group = state.group;
            model.fit.response = state.response;
            model.fit.family = model.family;
            model.fit.link = model.link;
            model.fit.n = std::atoi(request.args[cursor++].c_str());
            model.fit.logLik = ParseOptionalDataCellDouble(request.args[cursor++]);
            model.fit.aic = ParseOptionalDataCellDouble(request.args[cursor++]);
            model.fit.bic = ParseOptionalDataCellDouble(request.args[cursor++]);
            model.fit.auc = ParseOptionalDataCellDouble(request.args[cursor++]);
            const std::string terms = request.args[cursor++];
            if (!terms.empty()) {
                std::size_t start = 0;
                while (start < terms.size()) {
                    const auto end = terms.find(" + ", start);
                    const std::string term = terms.substr(
                        start, end == std::string::npos ? std::string::npos : end - start);
                    if (!term.empty()) model.terms.push_back(term);
                    if (end == std::string::npos) break;
                    start = end + 3;
                }
            }
            model.candidateTerms = model.terms;
            model.comparisonOk = request.args[cursor++] == "TRUE";
            model.comparisonCalculatedInR = model.comparisonOk;
            model.comparisonDf = std::atoi(request.args[cursor++].c_str());
            model.comparisonStatistic = ParseOptionalDataCellDouble(request.args[cursor++]);
            model.comparisonP = ParseOptionalDataCellDouble(request.args[cursor++]);
            model.fit.status = request.args[cursor++];
            model.fit.ok = model.fit.n > 0;
            model.isStale = false;
            model.fitState = model.fit.ok
                ? RegressionComparisonFitState::Valid
                : RegressionComparisonFitState::Error;
            state.models.push_back(std::move(model));
        }

        int resultGeneration = 0;
        bool specificationIdentityV5 = false;
        if (cursor < request.args.size() &&
            (request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V2" ||
             request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V3" ||
             request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V4" ||
             request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V5" ||
             request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V6" ||
             request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V7" ||
             request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V8" ||
             request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V9")) {
            const bool semanticV3 = request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V3" ||
                                    request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V4" ||
                                    request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V5" ||
                                    request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V6" ||
                                    request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V7" ||
                                    request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V8" ||
                                    request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V9";
            const bool semanticV4 = request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V4" ||
                                    request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V5" ||
                                    request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V6" ||
                                    request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V7" ||
                                    request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V8" ||
                                    request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V9";
            const bool semanticV6 = request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V6" ||
                                    request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V7" ||
                                    request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V8";
            const bool semanticV7 = request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V7" ||
                                    request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V8";
            const bool semanticV8 = request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V8" ||
                                    request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V9";
            specificationIdentityV5 = request.args[cursor] == "GENERALIZED_COMPARISON_MODELS_V5" ||
                                      semanticV6;
            ++cursor;
            if (cursor + 2 > request.args.size()) {
                return "ERR malformed generalized comparison semantic extension";
            }
            resultGeneration = ParseOptionalDataCellInt(request.args[cursor++]);
            if (semanticV3) {
                if (cursor >= request.args.size()) {
                    return "ERR missing generalized comparison kind";
                }
                state.countComparison = request.args[cursor++] == "TRUE";
            }
            if (semanticV4) {
                if (cursor >= request.args.size()) {
                    return "ERR missing generalized comparison MI kind";
                }
                state.multipleImputation = request.args[cursor++] == "TRUE";
            }
            if (semanticV6) {
                if (cursor >= request.args.size() ||
                    !ParseStatisticalModelType(request.args[cursor++], state.modelType)) {
                    return "ERR invalid generalized comparison model type";
                }
            }
            const long semanticModelCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (semanticModelCount != modelCount) {
                return "ERR generalized comparison model count changed during fit";
            }
            for (long index = 0; index < semanticModelCount; ++index) {
                if (cursor + 6 > request.args.size()) {
                    return "ERR incomplete generalized comparison model specification";
                }
                const std::string modelId = request.args[cursor++];
                auto found = std::find_if(state.models.begin(), state.models.end(),
                    [&](GeneralizedComparisonModel const &candidate) {
                        return candidate.id == modelId;
                    });
                if (found == state.models.end()) {
                    return "ERR generalized comparison returned an unknown model";
                }
                found->response = request.args[cursor++];
                found->family = request.args[cursor++];
                found->link = request.args[cursor++];
                found->scope = request.args[cursor++];
                if (semanticV3) {
                    if (cursor + 6 > request.args.size()) {
                        return "ERR incomplete count-comparison semantic payload";
                    }
                    const bool countRegression = request.args[cursor++] == "TRUE";
                    if (countRegression != state.countComparison) {
                        return "ERR inconsistent count-comparison model kind";
                    }
                    if (!ParseCountDistribution(request.args[cursor++], found->countDistribution)) {
                        return "ERR invalid count-comparison distribution";
                    }
                    found->exposure = request.args[cursor++];
                    if (semanticV8) {
                        if (cursor >= request.args.size()) return "ERR missing comparison offset";
                        found->offsetVariable = request.args[cursor++];
                    }
                    if (semanticV7) {
                        if (cursor + 2 > request.args.size()) {
                            return "ERR incomplete bounded-count trials payload";
                        }
                        found->trialsVariable = request.args[cursor++];
                        found->trialsConstant = ParseOptionalDataCellDouble(request.args[cursor++]);
                    }
                    found->capabilities.hasLikelihood = request.args[cursor++] == "TRUE";
                    found->capabilities.hasAIC = request.args[cursor++] == "TRUE";
                    found->capabilities.supportsNestedLRComparison =
                        request.args[cursor++] == "TRUE";
                }
                if (semanticV4) {
                    if (cursor + 2 > request.args.size()) {
                        return "ERR incomplete generalized comparison MI test payload";
                    }
                    found->comparisonMethod = request.args[cursor++];
                    found->comparisonDf2 = ParseOptionalDataCellDouble(request.args[cursor++]);
                }
                if (specificationIdentityV5) {
                    if (cursor + 2 > request.args.size()) {
                        return "ERR incomplete generalized comparison specification identity";
                    }
                    found->fitSpecificationRevision =
                        ParseOptionalDataCellInt(request.args[cursor++]);
                    found->fitSpecificationFingerprint = request.args[cursor++];
                    if (found->fitSpecificationRevision < 1 ||
                        found->fitSpecificationFingerprint.empty()) {
                        return "ERR invalid generalized comparison specification identity";
                    }
                }
                found->terms.clear();
                const long termCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
                if (termCount < 0 || cursor + static_cast<std::size_t>(termCount) > request.args.size()) {
                    return "ERR invalid generalized comparison term count";
                }
                for (long i = 0; i < termCount; ++i) found->terms.push_back(request.args[cursor++]);
                if (!hadPrevious) found->candidateTerms = found->terms;
                if (cursor >= request.args.size()) return "ERR missing generalized comparison type count";
                const long typeCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
                if (typeCount < 0 || cursor + static_cast<std::size_t>(2 * typeCount) > request.args.size()) {
                    return "ERR invalid generalized comparison type payload";
                }
                found->termTypeOverrides.clear();
                for (long i = 0; i < typeCount; ++i) {
                    const std::string variable = request.args[cursor++];
                    const std::string type = request.args[cursor++];
                    found->termTypeOverrides[variable] = type;
                }
                if (cursor >= request.args.size()) return "ERR missing generalized comparison centered count";
                const long centeredCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
                if (centeredCount < 0 || cursor + static_cast<std::size_t>(centeredCount) > request.args.size()) {
                    return "ERR invalid generalized comparison centered payload";
                }
                found->centeredPredictors.clear();
                for (long i = 0; i < centeredCount; ++i) {
                    found->centeredPredictors.insert(request.args[cursor++]);
                }
                if (cursor >= request.args.size()) return "ERR missing generalized comparison reference count";
                const long referenceCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
                if (referenceCount < 0 ||
                    cursor + static_cast<std::size_t>(2 * referenceCount) > request.args.size()) {
                    return "ERR invalid generalized comparison reference payload";
                }
                found->factorReferenceLevels.clear();
                for (long i = 0; i < referenceCount; ++i) {
                    const std::string variable = request.args[cursor++];
                    const std::string reference = request.args[cursor++];
                    found->factorReferenceLevels[variable] = reference;
                }
                GeneralizedGLMState fit;
                fit.id = found->id;
                fit.group = state.group;
                fit.response = found->response;
                fit.family = found->family;
                fit.link = found->link;
                fit.modelType = state.modelType;
                fit.scope = found->scope;
                fit.countRegression = state.countComparison;
                fit.countDistribution = found->countDistribution;
                fit.exposure = found->exposure;
                fit.offsetVariable = found->offsetVariable;
                fit.trialsVariable = found->trialsVariable;
                fit.trialsConstant = found->trialsConstant;
                fit.terms = found->terms;
                fit.termTypes = found->termTypeOverrides;
                fit.termTypeOverrides = found->termTypeOverrides;
                fit.centeredPredictors = found->centeredPredictors;
                fit.factorReferenceLevels = found->factorReferenceLevels;
                if (!ReadGeneralizedStatePayload(request.args, cursor, fit)) {
                    return "ERR malformed generalized comparison fit payload";
                }
                found->responseBoundsConfigured = fit.responseBoundsConfigured;
                found->responseLower = fit.responseLower;
                found->responseUpper = fit.responseUpper;
                found->trialsVariable = fit.trialsVariable;
                found->trialsConstant = fit.trialsConstant;
                fit.modelVersion = found->fitSpecificationRevision;
                fit.fitVersion = found->fitSpecificationRevision;
                fit.lastRFitSignature = found->fitSpecificationFingerprint;
                found->fit = std::move(fit);
                found->fitState = found->fit.ok
                    ? RegressionComparisonFitState::Valid
                    : RegressionComparisonFitState::Error;
            }
        }

        {
            std::string provenanceError;
            if (!ReadAnalysisProvenancePayload(
                    request.args, cursor, state.provenance, &provenanceError)) {
                return "ERR malformed generalized comparison provenance: " +
                    provenanceError;
            }
        }

        if (hadPrevious) {
            // A result for an older generation is harmless and must not touch a
            // newer pending request. Once the generation matches, however, any
            // identity failure is terminal for that request: silently ignoring
            // it would leave the comparison window in "Fitting..." forever.
            if (previous->second.rFitGeneration > 0 &&
                (resultGeneration != previous->second.rFitGeneration ||
                 !previous->second.rFitPending)) {
                return "OK\t" + state.id;
            }
            auto rejectCurrentResult = [&](const std::string &reason) {
                previous->second.rFitPending = false;
                const std::string status =
                    "Rejected generalized-comparison result because " + reason + ".";
                for (GeneralizedComparisonModel &model : previous->second.models) {
                    model.isStale = false;
                    model.fit.ok = false;
                    model.fit.status = status;
                    model.fitState = RegressionComparisonFitState::Error;
                }
                RefreshGeneralizedTermRowsFromFits(previous->second);
                for (GeneralizedComparisonModel &model : previous->second.models) {
                    model.fitState = RegressionComparisonFitState::Error;
                }
                if (services_.ui.showGeneralizedComparison) {
                    services_.ui.showGeneralizedComparison(state.id);
                }
                return "OK\t" + state.id;
            };
            if (const DataFrameModel *dataframe =
                    applicationState_.datasets().find(previous->second.group)) {
                GeneralizedComparisonState currentDatasetIdentity = previous->second;
                SynchronizeGeneralizedComparisonDatasetIdentity(
                    currentDatasetIdentity, *dataframe);
                if (currentDatasetIdentity.datasetType != previous->second.datasetType ||
                    currentDatasetIdentity.imputationSetId != previous->second.imputationSetId ||
                    currentDatasetIdentity.sourceDatasetId != previous->second.sourceDatasetId ||
                    currentDatasetIdentity.imputationCount != previous->second.imputationCount) {
                    return rejectCurrentResult("the dataset/imputation identity changed");
                }
            }
            // Generation is the first stale-result gate.  Per-model immutable
            // identity is the second: never accept a batch that merely has the
            // latest generation but describes a different column revision.
            if (!specificationIdentityV5) {
                return rejectCurrentResult("it did not carry V5 specification identity");
            }
            const std::string returnedEvent = state.responseCoding.eventValue.empty()
                ? state.responseCoding.eventLabel : state.responseCoding.eventValue;
            const std::string returnedReference = state.responseCoding.referenceValue.empty()
                ? state.responseCoding.referenceLabel : state.responseCoding.referenceValue;
            const std::string expectedEvent = previous->second.responseCoding.eventValue.empty()
                ? previous->second.responseCoding.eventLabel
                : previous->second.responseCoding.eventValue;
            const std::string expectedReference = previous->second.responseCoding.referenceValue.empty()
                ? previous->second.responseCoding.referenceLabel
                : previous->second.responseCoding.referenceValue;
            if (state.group != previous->second.group ||
                state.modelType != previous->second.modelType ||
                state.binaryComparison != previous->second.binaryComparison ||
                state.countComparison != previous->second.countComparison ||
                state.multipleImputation != previous->second.multipleImputation ||
                (previous->second.binaryComparison &&
                 (returnedEvent != expectedEvent || returnedReference != expectedReference))) {
                return rejectCurrentResult("its comparison/event identity did not match the pending request");
            }
            for (const GeneralizedComparisonModel &returnedModel : state.models) {
                auto oldModel = std::find_if(previous->second.models.begin(), previous->second.models.end(),
                    [&](GeneralizedComparisonModel const &candidate) {
                        return candidate.id == returnedModel.id;
                    });
                if (oldModel == previous->second.models.end()) {
                    return rejectCurrentResult("it returned an unknown model column");
                }
                const std::string expectedFingerprint =
                    GeneralizedComparisonModelSpecificationFingerprint(previous->second, *oldModel);
                if (returnedModel.fitSpecificationRevision != oldModel->modelVersion ||
                    returnedModel.fitSpecificationRevision != oldModel->requestedSpecificationRevision ||
                    returnedModel.fitSpecificationFingerprint != expectedFingerprint ||
                    returnedModel.fitSpecificationFingerprint != oldModel->requestedSpecificationFingerprint) {
                    return rejectCurrentResult("a model revision/fingerprint did not match the pending request");
                }
                GeneralizedComparisonModel returnedIdentity = *oldModel;
                returnedIdentity.response = returnedModel.response;
                returnedIdentity.terms = returnedModel.terms;
                returnedIdentity.termTypes = returnedModel.termTypeOverrides;
                returnedIdentity.termTypeOverrides = returnedModel.termTypeOverrides;
                returnedIdentity.centeredPredictors = returnedModel.centeredPredictors;
                returnedIdentity.factorReferenceLevels = returnedModel.factorReferenceLevels;
                returnedIdentity.scope = returnedModel.scope;
                returnedIdentity.family = returnedModel.family;
                returnedIdentity.link = returnedModel.link;
                returnedIdentity.responseBoundsConfigured =
                    returnedModel.responseBoundsConfigured;
                returnedIdentity.responseLower = returnedModel.responseLower;
                returnedIdentity.responseUpper = returnedModel.responseUpper;
                returnedIdentity.countDistribution = returnedModel.countDistribution;
                returnedIdentity.exposure = returnedModel.exposure;
                if (GeneralizedComparisonModelSpecificationFingerprint(
                        previous->second, returnedIdentity) != expectedFingerprint) {
                    return rejectCurrentResult("returned model semantics did not match its immutable fingerprint");
                }
            }
            state.scope = previous->second.scope;
            state.modelType = previous->second.modelType;
            state.dataScope = previous->second.dataScope;
            state.dataScopeCaptured = previous->second.dataScopeCaptured;
            state.autoRefit = previous->second.autoRefit;
            state.showInformationCriteria = previous->second.showInformationCriteria;
            state.response = previous->second.response;
            state.family = previous->second.family;
            state.link = previous->second.link;
            state.binaryComparison = previous->second.binaryComparison;
            state.countComparison = previous->second.countComparison;
            state.multipleImputation = previous->second.multipleImputation;
            state.imputationCount = previous->second.imputationCount;
            state.datasetType = previous->second.datasetType;
            state.imputationSetId = previous->second.imputationSetId;
            state.sourceDatasetId = previous->second.sourceDatasetId;
            state.binaryLink = previous->second.binaryLink;
            state.responseCoding = previous->second.responseCoding;
            state.termTypes = previous->second.termTypes;
            // A returned fit updates model results, not the user's visible
            // term order.  RefreshGeneralizedTermRowsFromFits expands current
            // factor/interaction children while retaining this base order.
            state.termRows = previous->second.termRows;
            state.activeModel = std::clamp(previous->second.activeModel, 0,
                                           static_cast<int>(state.models.size()) - 1);
            state.seed = previous->second.seed;
            state.hasSeed = previous->second.hasSeed;
            if (state.provenance.executedRCode.empty())
                state.provenance = previous->second.provenance;
            state.rFitGeneration = previous->second.rFitGeneration;
            state.lastRFitSignature = previous->second.lastRFitSignature;
            for (GeneralizedComparisonModel &model : state.models) {
                auto oldModel = std::find_if(previous->second.models.begin(), previous->second.models.end(),
                    [&](GeneralizedComparisonModel const &candidate) { return candidate.id == model.id; });
                if (oldModel == previous->second.models.end()) continue;
                GeneralizedGLMState acceptedFit = std::move(model.fit);
                static_cast<ModelSpecification &>(model) =
                    static_cast<const ModelSpecification &>(*oldModel);
                model.response = oldModel->response;
                model.family = oldModel->family;
                model.link = oldModel->link;
                model.countDistribution = oldModel->countDistribution;
                model.exposure = oldModel->exposure;
                model.diagnosticOptions = oldModel->diagnosticOptions;
                model.candidateTerms = oldModel->candidateTerms;
                model.requestedSpecificationRevision = oldModel->requestedSpecificationRevision;
                model.requestedSpecificationFingerprint = oldModel->requestedSpecificationFingerprint;
                model.modelVersion = oldModel->modelVersion;
                static_cast<ModelSpecification &>(acceptedFit) =
                    EffectiveGeneralizedComparisonModelSpecification(previous->second, *oldModel);
                acceptedFit.group = previous->second.group;
                acceptedFit.modelType = previous->second.modelType;
                acceptedFit.family = model.family;
                acceptedFit.link = model.link;
                acceptedFit.binaryRegression = previous->second.binaryComparison;
                acceptedFit.binaryLink = previous->second.binaryLink;
                acceptedFit.responseCoding = previous->second.responseCoding;
                acceptedFit.countRegression = previous->second.countComparison;
                acceptedFit.countDistribution = model.countDistribution;
                acceptedFit.exposure = model.exposure;
                acceptedFit.offsetVariable = model.offsetVariable;
                acceptedFit.multipleImputation = previous->second.multipleImputation;
                acceptedFit.imputationCount = previous->second.imputationCount;
                acceptedFit.datasetType = previous->second.datasetType;
                acceptedFit.imputationSetId = previous->second.imputationSetId;
                acceptedFit.sourceDatasetId = previous->second.sourceDatasetId;
                acceptedFit.modelVersion = model.fitSpecificationRevision;
                acceptedFit.fitVersion = model.fitSpecificationRevision;
                acceptedFit.lastRFitSignature = model.fitSpecificationFingerprint;
                model.fit = std::move(acceptedFit);
                model.fit.diagnosticOptions = model.diagnosticOptions;
                model.fitVersion = model.fitSpecificationRevision;
                model.isStale = false;
                model.fitState = model.fit.ok
                    ? RegressionComparisonFitState::Valid
                    : RegressionComparisonFitState::Error;
            }
        } else {
            if (const DataFrameModel *dataframe = applicationState_.datasets().find(state.group)) {
                SynchronizeGeneralizedComparisonDatasetIdentity(state, *dataframe);
            }
            state.rFitGeneration = resultGeneration;
            for (GeneralizedComparisonModel &model : state.models) {
                model.modelVersion = 1;
                model.requestedSpecificationRevision = 1;
                model.fitVersion = 1;
                model.fitSpecificationRevision = 1;
                const std::string fingerprint =
                    GeneralizedComparisonModelSpecificationFingerprint(state, model);
                model.requestedSpecificationFingerprint = fingerprint;
                model.fitSpecificationFingerprint = fingerprint;
                model.fit.modelVersion = 1;
                model.fit.fitVersion = 1;
                model.fit.lastRFitSignature = fingerprint;
                model.fit.multipleImputation = state.multipleImputation;
                model.fit.imputationCount = state.imputationCount;
                model.fit.datasetType = state.datasetType;
                model.fit.imputationSetId = state.imputationSetId;
                model.fit.sourceDatasetId = state.sourceDatasetId;
            }
            state.lastRFitSignature = GeneralizedComparisonFitSignature(state);
        }
        if (state.multipleImputation && state.imputationCount <= 0) {
            for (const GeneralizedComparisonModel &model : state.models) {
                state.imputationCount = std::max(state.imputationCount, model.fit.imputationCount);
            }
        }
        state.rFitPending = false;
        if (!state.models.empty()) {
            state.response = state.models.front().response;
            state.family = state.models.front().family;
            state.link = state.models.front().link;
        }
        state.commonRows.clear();
        if (commonRows && !state.models.empty()) {
            state.commonRows = state.models.front().fit.rowsUsed;
        }
        RefreshGeneralizedTermRowsFromFits(state);
        if (const DataFrameModel *dataframe = applicationState_.datasets().find(state.group);
            dataframe && !state.provenance.executedRCode.empty()) {
            const AnalysisScope scope = state.dataScopeCaptured
                ? state.dataScope : applicationState_.activeAnalysisScope(state.group);
            const std::vector<int> used = state.models.empty()
                ? std::vector<int>{} : state.models.front().fit.rowsUsed;
            const std::vector<int> excluded = state.models.empty()
                ? std::vector<int>{} : state.models.front().fit.rowsExcluded;
            const std::string title = StatisticalModelComparisonTitle(
                state.modelType, state.multipleImputation);
            CompleteAnalysisProvenance(state.provenance, *dataframe, scope,
                used, excluded, state.id, title);
        }
        const std::string comparisonId = state.id;
        comparisons[comparisonId] = std::move(state);
        if (!comparisons.at(comparisonId).provenance.executedRCode.empty())
            applicationState_.registerOutputCodeReference(
                GeneralizedComparisonCodeReference(comparisons.at(comparisonId)));
        if (services_.ui.showGeneralizedComparison) {
            services_.ui.showGeneralizedComparison(comparisonId);
        }
        return "OK\t" + comparisonId;
    }
    case CommandAction::ChangeXVariable:
    case CommandAction::ChangeYVariable: {
        if (request.name != "SET_XVAR" && request.name != "SET_YVAR") return std::nullopt;
        if (request.args.size() < 2) return "ERR malformed variable change command";
        if (!services_.queries.plot || !services_.selection.setAxisVariable) return std::nullopt;
        PlotModel plot;
        if (!services_.queries.plot(request.args[0], plot)) return "ERR no active plot";
        if (!FindNumericVariable(plot, request.args[1])) {
            return "ERR variable is not available for this plot";
        }
        const bool changeX = request.action == CommandAction::ChangeXVariable;
        return services_.selection.setAxisVariable(request.args[0], changeX, request.args[1])
            ? "OK" : "ERR no active plot";
    }
    case CommandAction::ScatterMatrixAddVariable:
    case CommandAction::ScatterMatrixRemoveVariable:
    case CommandAction::ScatterMatrixReplaceVariable: {
        const std::size_t required = request.action == CommandAction::ScatterMatrixReplaceVariable ? 3 : 2;
        if (request.args.size() < required) return "ERR malformed scatter-matrix variable command";
        if (!services_.queries.plot || !services_.selection.mutateScatterMatrix) return std::nullopt;
        PlotModel plot;
        if (!services_.queries.plot(request.args[0], plot) || plot.kind != "scatter_matrix") {
            return "ERR plot is not a scatterplot matrix";
        }
        const CommandAction action = request.action;
        const std::string variable = request.args[1];
        const std::string replacement = action == CommandAction::ScatterMatrixReplaceVariable
            ? request.args[2] : "";
        const bool changed = services_.selection.mutateScatterMatrix(
            request.args[0], [action, variable, replacement](PlotModel &target) {
                const std::vector<std::string> current = ScatterMatrixVariablesForModel(target);
                const std::vector<std::string> available = NumericVariableNames(target);
                std::vector<std::string> updated = current;
                if (action == CommandAction::ScatterMatrixAddVariable) {
                    updated = ScatterMatrixVariablesAfterAdd(current, variable, available);
                } else if (action == CommandAction::ScatterMatrixRemoveVariable) {
                    updated = ScatterMatrixVariablesAfterRemove(current, variable);
                } else {
                    char *end = nullptr;
                    const long index = std::strtol(variable.c_str(), &end, 10);
                    if (!end || *end != '\0' || index < 0) return false;
                    updated = ScatterMatrixVariablesAfterReplacement(
                        current, static_cast<std::size_t>(index), replacement, available);
                }
                if (updated == current) return false;
                target.scatterMatrixVariables = std::move(updated);
                RebuildScatterMatrixPoints(target);
                ComputeRanges(target);
                return true;
            });
        if (!changed) return "ERR scatterplot matrix was not changed";
        RefreshRegisteredBasicPlotCodeReference(applicationState_, request.args[0]);
        if (services_.ui.refreshPlotTitle) services_.ui.refreshPlotTitle(request.args[0]);
        if (services_.ui.redrawPlot) services_.ui.redrawPlot(request.args[0]);
        return "OK";
    }
    case CommandAction::BoxplotTogglePoints:
    case CommandAction::BoxplotToggleBox:
    case CommandAction::BoxplotToggleWhiskers:
    case CommandAction::BoxplotToggleViolin:
    case CommandAction::BoxplotToggleConnectRows:
    case CommandAction::BoxplotToggleStandardize: {
        if (request.args.empty()) return "ERR missing plot id";
        if (!services_.queries.plot || !services_.selection.mutateBoxplot) return std::nullopt;
        PlotModel plot;
        if (!services_.queries.plot(request.args[0], plot) || plot.kind != "boxplot") {
            return "ERR plot is not a boxplot";
        }
        const CommandAction action = request.action;
        std::string message;
        const bool updated = services_.selection.mutateBoxplot(
            request.args[0],
            [action](PlotModel &target, std::string &error) {
                if (action == CommandAction::BoxplotTogglePoints) target.boxplotShowPoints = !target.boxplotShowPoints;
                else if (action == CommandAction::BoxplotToggleBox) target.boxplotShowBox = !target.boxplotShowBox;
                else if (action == CommandAction::BoxplotToggleWhiskers) target.boxplotShowWhiskers = !target.boxplotShowWhiskers;
                else if (action == CommandAction::BoxplotToggleViolin) target.boxplotShowViolin = !target.boxplotShowViolin;
                else if (action == CommandAction::BoxplotToggleConnectRows) {
                    if (!BoxplotUsesVariableAxes(target)) {
                        error = "row-connection lines require an ungrouped boxplot";
                        return false;
                    }
                    target.boxplotConnectRows = !target.boxplotConnectRows;
                } else if (action == CommandAction::BoxplotToggleStandardize) {
                    if (!BoxplotUsesVariableAxes(target)) {
                        error = "standardization requires an ungrouped boxplot";
                        return false;
                    }
                    target.boxplotStandardizeVariables = !target.boxplotStandardizeVariables;
                    if (!RebuildParallelBoxplotPoints(target, &error)) return false;
                }
                return true;
            }, message);
        if (!updated) return message.empty() ? "ERR no active plot" : "ERR " + message;
        RefreshRegisteredBasicPlotCodeReference(applicationState_, request.args[0]);
        if (services_.ui.redrawPlot) services_.ui.redrawPlot(request.args[0]);
        return "OK";
    }
    case CommandAction::BoxplotAddGroupingVariable:
    case CommandAction::BoxplotReplaceGroupingVariable:
    case CommandAction::BoxplotRemoveGroupingVariable:
    case CommandAction::BoxplotMoveGroupingVariableEarlier:
    case CommandAction::BoxplotMoveGroupingVariableLater: {
        const std::size_t required = request.action ==
            CommandAction::BoxplotReplaceGroupingVariable ? 3 : 2;
        if (request.args.size() < required) return "ERR malformed boxplot grouping command";
        if (!services_.queries.plot || !services_.selection.mutateBoxplot) return std::nullopt;
        PlotModel plot;
        if (!services_.queries.plot(request.args[0], plot) || plot.kind != "boxplot")
            return "ERR plot is not a boxplot";
        const DataFrameModel *dataframe = applicationState_.datasets().find(plot.group);
        if (!dataframe) return "ERR plot data is unavailable";
        const std::string variable = request.args[1];
        const std::string replacement = request.action ==
            CommandAction::BoxplotReplaceGroupingVariable ? request.args[2] : std::string{};
        const CommandAction action = request.action;
        std::string message;
        const bool updated = services_.selection.mutateBoxplot(
            request.args[0],
            [action, variable, replacement, dataframe](PlotModel &target, std::string &error) {
                std::vector<std::string> groups = target.boxplotGroupingVariables;
                if (groups.empty() && !target.xLabel.empty() &&
                    FindDataColumnInDataFrame(*dataframe, target.xLabel))
                    groups.push_back(target.xLabel);
                const std::string oldDefault = BoxplotDefaultTitle(
                    target.yLabel, BoxplotGroupingLabel(groups));
                auto found = std::find(groups.begin(), groups.end(), variable);
                if (action == CommandAction::BoxplotAddGroupingVariable) {
                    const DataColumn *column = FindDataColumnInDataFrame(*dataframe, variable);
                    if (!column || variable == target.yLabel || found != groups.end() ||
                        !DataColumnLooksGroupingCandidate(*column, dataframe->rows)) {
                        error = "the grouping variable is unavailable or unsuitable";
                        return false;
                    }
                    groups.push_back(variable);
                } else {
                    if (found == groups.end()) {
                        error = "the grouping variable is not in this boxplot";
                        return false;
                    }
                    const std::size_t index = static_cast<std::size_t>(found - groups.begin());
                    if (action == CommandAction::BoxplotReplaceGroupingVariable) {
                        const DataColumn *column = FindDataColumnInDataFrame(
                            *dataframe, replacement);
                        if (!column || replacement == target.yLabel ||
                            std::find(groups.begin(), groups.end(), replacement) != groups.end() ||
                            !DataColumnLooksGroupingCandidate(*column, dataframe->rows)) {
                            error = "the replacement grouping variable is unavailable or unsuitable";
                            return false;
                        }
                        groups[index] = replacement;
                    } else if (action == CommandAction::BoxplotRemoveGroupingVariable) {
                        groups.erase(found);
                    } else {
                        const bool earlier = action == CommandAction::BoxplotMoveGroupingVariableEarlier;
                        if ((earlier && index == 0) || (!earlier && index + 1 >= groups.size()))
                            return true;
                        const std::size_t other = earlier ? index - 1 : index + 1;
                        std::swap(groups[index], groups[other]);
                    }
                }
                target.boxplotGroupingVariables = groups;
                target.boxplotCategoryLevels.clear();
                if (groups.empty()) {
                    target.xLabel.clear();
                    target.boxplotVariables = {target.yLabel};
                    if (!RebuildParallelBoxplotPoints(target, &error)) return false;
                } else {
                    target.xLabel = BoxplotGroupingLabel(groups);
                    target.boxplotVariables.clear();
                    RebuildGroupedBoxplotPointsFromDataFrame(target, *dataframe);
                    if (target.boxplotPoints.empty()) {
                        error = "no finite observations are available for these groups";
                        return false;
                    }
                }
                if (target.title.empty() || target.title == oldDefault)
                    target.title = BoxplotDefaultTitle(target.yLabel, target.xLabel);
                ClearBoxplotH0Simulation(target);
                return true;
            }, message);
        if (!updated) return message.empty() ? "ERR boxplot grouping was not changed" : "ERR " + message;
        RefreshRegisteredBasicPlotCodeReference(applicationState_, request.args[0]);
        if (services_.ui.refreshPlotTitle) services_.ui.refreshPlotTitle(request.args[0]);
        if (services_.ui.redrawPlot) services_.ui.redrawPlot(request.args[0]);
        return "OK";
    }
    case CommandAction::BoxplotAddVariable:
    case CommandAction::BoxplotRemoveVariable:
    case CommandAction::BoxplotReplaceVariable: {
        const std::size_t required = request.action == CommandAction::BoxplotReplaceVariable ? 3 : 2;
        if (request.args.size() < required) return "ERR malformed boxplot variable command";
        if (!services_.queries.plot) return std::nullopt;
        PlotModel plot;
        if (!services_.queries.plot(request.args[0], plot)) return "ERR no active plot";
        if (plot.kind != "boxplot") return "ERR plot is not a boxplot";
        if (!services_.selection.mutateBoxplot) return std::nullopt;
        const CommandAction action = request.action;
        const std::string variable = request.args[1];
        const std::string replacement = request.action == CommandAction::BoxplotReplaceVariable
            ? request.args[2] : "";
        std::string message;
        const bool updated = services_.selection.mutateBoxplot(
            request.args[0],
            [action, variable, replacement](PlotModel &target, std::string &error) {
                const bool changed = action == CommandAction::BoxplotAddVariable
                    ? AddParallelBoxplotVariable(target, variable, &error)
                    : (action == CommandAction::BoxplotRemoveVariable
                        ? RemoveParallelBoxplotVariable(target, variable, &error)
                        : ReplaceParallelBoxplotVariable(target, variable, replacement, &error));
                if (!changed) return false;
                if (target.boxplotShowH0Simulation &&
                    !RebuildBoxplotH0Simulation(target, &error)) {
                    ClearBoxplotH0Simulation(target);
                }
                return true;
            },
            message);
        if (!updated) return message.empty() ? "ERR no active plot" : "ERR " + message;
        RefreshRegisteredBasicPlotCodeReference(applicationState_, request.args[0]);
        if (services_.ui.refreshPlotTitle) services_.ui.refreshPlotTitle(request.args[0]);
        if (services_.ui.redrawPlot) services_.ui.redrawPlot(request.args[0]);
        return "OK";
    }
    case CommandAction::BoxplotSplitViolin:
    case CommandAction::BoxplotH0Simulation:
    case CommandAction::BoxplotOption:
    case CommandAction::BoxplotOptions: {
        if (request.args.empty()) return "ERR missing plot id";
        if (!services_.queries.plot) return std::nullopt;
        PlotModel plot;
        if (!services_.queries.plot(request.args[0], plot)) return "ERR no active plot";
        if (plot.kind != "boxplot") return "ERR plot is not a boxplot";
        if (request.action == CommandAction::BoxplotOptions) {
            return BoxplotOptionsResponseText(plot);
        }
        if (!services_.selection.mutateBoxplot) return std::nullopt;
        std::function<bool(PlotModel &, std::string &)> mutation;
        bool refreshTitle = false;
        if (request.action == CommandAction::BoxplotSplitViolin) {
            if (request.args.size() < 5) return "ERR malformed BOXPLOT_SPLIT_VIOLIN command";
            const bool show = request.args[1] == "TRUE";
            const std::string alternative = request.args[2];
            if (alternative != "less" && alternative != "greater" && alternative != "two.sided") {
                return "ERR invalid split violin alternative";
            }
            double lower = std::strtod(request.args[3].c_str(), nullptr);
            double upper = std::strtod(request.args[4].c_str(), nullptr);
            if (show && (!std::isfinite(lower) || !std::isfinite(upper))) {
                return "ERR invalid split violin threshold";
            }
            if (alternative == "two.sided" && lower > upper) std::swap(lower, upper);
            mutation = [show, alternative, lower, upper](PlotModel &target, std::string &) {
                target.boxplotSplitViolin = show;
                if (show) {
                    target.boxplotShowViolin = true;
                    target.boxplotSplitAlternative = alternative;
                    target.boxplotSplitLower = lower;
                    target.boxplotSplitUpper = upper;
                }
                return true;
            };
        } else if (request.action == CommandAction::BoxplotH0Simulation) {
            if (request.args.size() < 5) return "ERR malformed BOXPLOT_H0_SIMULATION command";
            const bool show = request.args[1] == "TRUE";
            if (!show) {
                mutation = [](PlotModel &target, std::string &) {
                    ClearBoxplotH0Simulation(target);
                    return true;
                };
            } else {
                const double h0 = std::strtod(request.args[2].c_str(), nullptr);
                const std::string alternative = request.args[3];
                const int draws = std::atoi(request.args[4].c_str());
                if (!std::isfinite(h0)) return "ERR invalid H0 value";
                if (alternative != "less" && alternative != "greater" && alternative != "two.sided") {
                    return "ERR invalid H0 alternative";
                }
                mutation = [h0, alternative, draws](PlotModel &target, std::string &error) {
                    target.boxplotH0 = h0;
                    target.boxplotH0Alternative = alternative;
                    target.boxplotH0Draws = std::max(100, std::min(200000, draws));
                    if (RebuildBoxplotH0Simulation(target, &error)) return true;
                    ClearBoxplotH0Simulation(target);
                    return false;
                };
            }
        } else {
            if (request.args.size() < 3) return "ERR malformed BOXPLOT_OPTION command";
            const std::string option = request.args[1];
            const bool show = request.args[2] == "TRUE";
            if (option == "connect_rows" || option == "lines") {
                if (!BoxplotUsesVariableAxes(plot) && show) {
                    return "ERR row-connection lines require an ungrouped boxplot";
                }
                mutation = [show](PlotModel &target, std::string &) {
                    target.boxplotConnectRows = show;
                    return true;
                };
            } else if (option == "standardize" || option == "standardize_variables") {
                if (!BoxplotUsesVariableAxes(plot) && show) {
                    return "ERR standardization requires an ungrouped boxplot";
                }
                refreshTitle = BoxplotUsesVariableAxes(plot);
                mutation = [show](PlotModel &target, std::string &error) {
                    target.boxplotStandardizeVariables = show;
                    if (!BoxplotUsesVariableAxes(target)) return true;
                    if (!RebuildParallelBoxplotPoints(target, &error)) return false;
                    if (target.boxplotShowH0Simulation &&
                        !RebuildBoxplotH0Simulation(target, &error)) {
                        ClearBoxplotH0Simulation(target);
                    }
                    return true;
                };
            } else {
                bool PlotModel::*field = nullptr;
                if (option == "points") field = &PlotModel::boxplotShowPoints;
                else if (option == "box") field = &PlotModel::boxplotShowBox;
                else if (option == "whiskers") field = &PlotModel::boxplotShowWhiskers;
                else if (option == "violin") field = &PlotModel::boxplotShowViolin;
                else return "ERR unknown boxplot option";
                mutation = [field, show](PlotModel &target, std::string &) {
                    target.*field = show;
                    return true;
                };
            }
        }
        std::string message;
        if (!services_.selection.mutateBoxplot(request.args[0], mutation, message)) {
            return message.empty() ? "ERR no active plot" : "ERR " + message;
        }
        RefreshRegisteredBasicPlotCodeReference(applicationState_, request.args[0]);
        if (refreshTitle && services_.ui.refreshPlotTitle) services_.ui.refreshPlotTitle(request.args[0]);
        if (services_.ui.redrawPlot) services_.ui.redrawPlot(request.args[0]);
        return "OK";
    }
    case CommandAction::ScatterToggleOverlapSize:
    case CommandAction::ScatterToggleOverlapShading: {
        if (request.args.empty()) return "ERR missing plot id";
        auto found = applicationState_.plots().find(request.args[0]);
        if (found == applicationState_.plots().end() || !found->second)
            return "ERR no active plot";
        PlotModel &plot = *found->second;
        if (plot.kind != "scatter" && plot.kind != "trellis_scatterplot")
            return "ERR plot does not support overlap shading";
        if (request.action == CommandAction::ScatterToggleOverlapSize)
            plot.scatterSizeByOverlap = !plot.scatterSizeByOverlap;
        else plot.scatterShadeOverlap = !plot.scatterShadeOverlap;
        if (services_.ui.redrawPlot) services_.ui.redrawPlot(plot.id);
        return "OK";
    }
    case CommandAction::HistogramToggleCounts:
    case CommandAction::HistogramToggleTickMarks:
    case CommandAction::HistogramToggleTickLabels:
    case CommandAction::HistogramToggleRug:
    case CommandAction::HistogramToggleDensity: {
        if (request.args.empty()) return "ERR missing plot id";
        if (!services_.queries.plot || !services_.selection.mutateHistogram) return std::nullopt;
        PlotModel plot;
        if (!services_.queries.plot(request.args[0], plot) || plot.kind != "histogram") {
            return "ERR plot is not a histogram";
        }
        const CommandAction action = request.action;
        if (!services_.selection.mutateHistogram(request.args[0], [action](PlotModel &target) {
            if (action == CommandAction::HistogramToggleCounts) {
                target.histogramShowCounts = !target.histogramShowCounts;
            } else if (action == CommandAction::HistogramToggleTickMarks) {
                target.histogramShowTickMarks = !target.histogramShowTickMarks;
            } else if (action == CommandAction::HistogramToggleTickLabels) {
                target.histogramShowTickLabels = !target.histogramShowTickLabels;
            } else if (action == CommandAction::HistogramToggleRug) {
                target.histogramShowRug = !target.histogramShowRug;
            } else {
                const HistogramDensityState state = HistogramStateAfterToggleDensity(
                    target.histogramShowDensity, target.histogramDensityMode);
                target.histogramShowDensity = state.showDensity;
                target.histogramDensityMode = state.densityMode;
            }
        })) return "ERR no active plot";
        RefreshRegisteredBasicPlotCodeReference(applicationState_, request.args[0]);
        if (services_.ui.redrawPlot) services_.ui.redrawPlot(request.args[0]);
        return "OK";
    }
    case CommandAction::BarplotToggleConditionalPercent: {
        if (request.args.empty()) return "ERR missing plot id";
        if (!services_.queries.plot || !services_.selection.mutateBarplot) return std::nullopt;
        PlotModel plot;
        if (!services_.queries.plot(request.args[0], plot) || plot.kind != "barplot") {
            return "ERR plot is not a barplot";
        }
        if (!services_.selection.mutateBarplot(request.args[0], [](PlotModel &target) {
            target.barplotShowConditionalPercent = !target.barplotShowConditionalPercent;
        })) return "ERR no active plot";
        RefreshRegisteredBasicPlotCodeReference(applicationState_, request.args[0]);
        if (services_.ui.redrawPlot) services_.ui.redrawPlot(request.args[0]);
        return "OK";
    }
    case CommandAction::BarplotSetX:
    case CommandAction::BarplotAddX:
    case CommandAction::BarplotReplaceX:
    case CommandAction::BarplotRemoveX:
    case CommandAction::BarplotSplitBy:
    case CommandAction::BarplotClearSplit:
    case CommandAction::BarplotRowColors:
    case CommandAction::BarplotSplitStrokeWidth:
    case CommandAction::BarplotSegmentEncoding:
    case CommandAction::BarplotShowPatterns:
    case CommandAction::BarplotSelectionDisplay:
    case CommandAction::BarplotMode:
    case CommandAction::BarplotWidth:
    case CommandAction::BarplotSegmentSetColor:
    case CommandAction::BarplotLevelSetColor:
    case CommandAction::BarplotSegmentSetAlpha:
    case CommandAction::BarplotLevelSetPattern:
    case CommandAction::BarplotSegmentSetPattern:
    case CommandAction::BarplotSegmentResetColor:
    case CommandAction::BarplotLevelResetColor: {
        if (request.args.empty()) return "ERR missing plot id";
        if (!services_.queries.plot || !services_.selection.mutateBarplot) return std::nullopt;
        PlotModel plot;
        if (!services_.queries.plot(request.args[0], plot) || plot.kind != "barplot") {
            return "ERR plot is not a barplot";
        }
        const DataFrameModel *dataframe = applicationState_.datasets().find(plot.group);
        const CommandAction action = request.action;
        std::string error;
        PlotModel candidate = plot;
        auto requireArgument = [&](std::size_t index, const char *message) -> bool {
            if (request.args.size() > index) return true;
            error = message;
            return false;
        };
        auto segmentReference = [&](std::size_t barArg, std::size_t segmentArg)
            -> std::optional<BarplotSegmentReference> {
            if (!requireArgument(segmentArg, "missing bar segment")) return std::nullopt;
            char *barEnd = nullptr;
            char *segmentEnd = nullptr;
            const long barIndex = std::strtol(request.args[barArg].c_str(), &barEnd, 10);
            const long segmentIndex = std::strtol(request.args[segmentArg].c_str(), &segmentEnd, 10);
            if (!barEnd || *barEnd != '\0' || !segmentEnd || *segmentEnd != '\0' ||
                barIndex < 0 || segmentIndex < 0) {
                error = "invalid bar segment";
                return std::nullopt;
            }
            auto reference = BarplotCoreSegmentReferenceForIndices(
                candidate, static_cast<int>(barIndex), static_cast<int>(segmentIndex));
            if (!reference) error = "bar segment no longer exists";
            return reference;
        };

        bool rebuild = false;
        bool refreshTitle = false;
        if (action == CommandAction::BarplotSetX) {
            if (!requireArgument(1, "missing X variable")) return "ERR " + error;
            candidate.barplotXVariables = BarplotXVariablesAfterSet(request.args[1]);
            rebuild = refreshTitle = true;
        } else if (action == CommandAction::BarplotAddX) {
            if (!requireArgument(1, "missing X variable")) return "ERR " + error;
            candidate.barplotXVariables = BarplotXVariablesAfterAdd(
                candidate.barplotXVariables, candidate.xLabel, request.args[1]);
            rebuild = refreshTitle = true;
        } else if (action == CommandAction::BarplotReplaceX) {
            if (!requireArgument(2, "missing replacement X variable")) return "ERR " + error;
            candidate.barplotXVariables = BarplotXVariablesAfterReplace(
                candidate.barplotXVariables, candidate.xLabel,
                DecodeCommandField(request.args[1]), DecodeCommandField(request.args[2]));
            rebuild = refreshTitle = true;
        } else if (action == CommandAction::BarplotRemoveX) {
            if (!requireArgument(1, "missing X variable")) return "ERR " + error;
            candidate.barplotXVariables = BarplotXVariablesAfterRemove(
                candidate.barplotXVariables, candidate.xLabel,
                DecodeCommandField(request.args[1]));
            rebuild = refreshTitle = true;
        } else if (action == CommandAction::BarplotSplitBy) {
            if (!requireArgument(1, "missing split variable")) return "ERR " + error;
            const auto next = BarplotStateAfterSplitBy(
                candidate.barplotXVariables, candidate.xLabel,
                candidate.barplotSplitVariable, candidate.barplotRowColorDisplay,
                candidate.barplotShowConditionalPercent, DecodeCommandField(request.args[1]));
            candidate.barplotSplitVariable = next.splitVariable;
            candidate.barplotRowColorDisplay = next.rowColorDisplay;
            candidate.barplotShowConditionalPercent = next.showConditionalPercent;
            if (next.resetSplitVisualState) ResetBarplotSplitVisualState(candidate, true);
            rebuild = refreshTitle = true;
        } else if (action == CommandAction::BarplotClearSplit) {
            const auto next = BarplotStateAfterClearSplit(
                candidate.barplotSplitVariable, candidate.barplotRowColorDisplay,
                candidate.barplotShowConditionalPercent);
            candidate.barplotSplitVariable = next.splitVariable;
            candidate.barplotRowColorDisplay = next.rowColorDisplay;
            candidate.barplotShowConditionalPercent = next.showConditionalPercent;
            if (next.resetSplitVisualState) ResetBarplotSplitVisualState(candidate, true);
            rebuild = refreshTitle = true;
        } else if (action == CommandAction::BarplotRowColors) {
            if (!requireArgument(1, "missing row-color mode")) return "ERR " + error;
            candidate.barplotRowColorDisplay = BarplotRowColorDisplayAfterSet(
                request.args[1], !candidate.barplotSplitVariable.empty(),
                candidate.barplotRowColorDisplay);
        } else if (action == CommandAction::BarplotSplitStrokeWidth) {
            if (!requireArgument(1, "missing border width")) return "ERR " + error;
            const auto value = ParseBarplotSplitStrokeWidthCommandValue(request.args[1]);
            if (!value) return "ERR invalid bar border width";
            candidate.barplotSplitStrokeWidth = *value;
        } else if (action == CommandAction::BarplotSegmentEncoding) {
            if (!requireArgument(1, "missing segment encoding")) return "ERR " + error;
            const auto next = BarplotStateAfterSegmentEncoding(
                candidate.barplotSegmentEncodingMode, candidate.barplotShowPatterns,
                request.args[1]);
            candidate.barplotSegmentEncodingMode = next.segmentEncodingMode;
            candidate.barplotShowPatterns = next.showPatterns;
        } else if (action == CommandAction::BarplotShowPatterns) {
            if (!requireArgument(1, "missing pattern visibility")) return "ERR " + error;
            candidate.barplotShowPatterns = BarplotShowPatternsAfterCommand(
                candidate.barplotShowPatterns, request.args[1]);
        } else if (action == CommandAction::BarplotSelectionDisplay) {
            if (!requireArgument(1, "missing selection-display mode")) return "ERR " + error;
            candidate.barplotSelectionDisplay = BarplotSelectionDisplayAfterSet(
                request.args[1], candidate.barplotSelectionDisplay);
        } else if (action == CommandAction::BarplotMode) {
            if (!requireArgument(1, "missing Y-axis mode")) return "ERR " + error;
            candidate.barplotMode = BarplotModeAfterSet(request.args[1], candidate.barplotMode);
            candidate.barplotShowConditionalPercent = BarplotConditionalPercentForMode(
                candidate.barplotMode, !candidate.barplotSplitVariable.empty());
        } else if (action == CommandAction::BarplotWidth) {
            if (!requireArgument(1, "missing width mode")) return "ERR " + error;
            candidate.barplotWidthMode = BarplotWidthModeAfterSet(
                request.args[1], candidate.barplotWidthMode);
            rebuild = true;
        } else if (action == CommandAction::BarplotLevelSetColor) {
            if (!requireArgument(2, "missing level color")) return "ERR " + error;
            auto visual = BarplotVisualStateForModel(candidate);
            SetBarplotLevelColorOverride(visual, DecodeCommandField(request.args[1]), request.args[2]);
            ApplyNormalizedBarplotVisualState(candidate, visual);
        } else if (action == CommandAction::BarplotLevelSetPattern) {
            if (!requireArgument(2, "missing level pattern")) return "ERR " + error;
            auto visual = BarplotVisualStateForModel(candidate);
            SetBarplotLevelPatternOverride(visual, DecodeCommandField(request.args[1]), request.args[2]);
            ApplyNormalizedBarplotVisualState(candidate, visual);
        } else if (action == CommandAction::BarplotLevelResetColor) {
            if (!requireArgument(1, "missing level")) return "ERR " + error;
            auto visual = BarplotVisualStateForModel(candidate);
            ResetBarplotLevelColorOverride(visual, DecodeCommandField(request.args[1]));
            ApplyNormalizedBarplotVisualState(candidate, visual);
        } else {
            auto reference = segmentReference(1, 2);
            if (!reference) return "ERR " + error;
            auto visual = BarplotVisualStateForModel(candidate);
            if (action == CommandAction::BarplotSegmentSetColor) {
                if (!requireArgument(3, "missing segment color")) return "ERR " + error;
                SetBarplotSegmentColorOverride(visual, *reference, request.args[3]);
            } else if (action == CommandAction::BarplotSegmentSetAlpha) {
                if (!requireArgument(3, "missing segment opacity")) return "ERR " + error;
                const auto alpha = ParseBarplotSegmentAlphaCommandValue(request.args[3]);
                if (!alpha) return "ERR invalid segment opacity";
                SetBarplotSegmentAlphaOverride(visual, *reference, *alpha);
            } else if (action == CommandAction::BarplotSegmentSetPattern) {
                if (!requireArgument(3, "missing segment pattern")) return "ERR " + error;
                SetBarplotSegmentPatternOverride(visual, *reference, request.args[3]);
            } else if (action == CommandAction::BarplotSegmentResetColor) {
                ResetBarplotSegmentColorOverride(visual, *reference);
            }
            ApplyNormalizedBarplotVisualState(candidate, visual);
        }

        if (rebuild) {
            if (!dataframe) return "ERR dataset is not registered";
            if (!RebuildBarplotFromDataFrame(candidate, *dataframe, &error)) {
                return "ERR " + (error.empty() ? std::string("could not rebuild bar chart") : error);
            }
        }
        if (!services_.selection.mutateBarplot(request.args[0],
                [candidate](PlotModel &target) { target = candidate; })) {
            return "ERR no active plot";
        }
        RefreshRegisteredBasicPlotCodeReference(applicationState_, request.args[0]);
        if (refreshTitle && services_.ui.refreshPlotTitle)
            services_.ui.refreshPlotTitle(request.args[0]);
        if (services_.ui.redrawPlot) services_.ui.redrawPlot(request.args[0]);
        return "OK";
    }
    case CommandAction::BarplotSegmentSelect:
    case CommandAction::BarplotSegmentApplyColorToRows:
    case CommandAction::BarplotSegmentDetails: {
        if (request.args.size() < 3) return "ERR missing bar segment";
        if (!services_.queries.plot) return std::nullopt;
        PlotModel plot;
        if (!services_.queries.plot(request.args[0], plot) || plot.kind != "barplot")
            return "ERR plot is not a barplot";
        char *barEnd = nullptr;
        char *segmentEnd = nullptr;
        const long bar = std::strtol(request.args[1].c_str(), &barEnd, 10);
        const long segment = std::strtol(request.args[2].c_str(), &segmentEnd, 10);
        if (!barEnd || *barEnd != '\0' || !segmentEnd || *segmentEnd != '\0' ||
            bar < 0 || segment < 0) return "ERR invalid bar segment";
        const auto reference = BarplotCoreSegmentReferenceForIndices(
            plot, static_cast<int>(bar), static_cast<int>(segment));
        if (!reference) return "ERR bar segment no longer exists";
        const std::vector<int> rows = reference->segment.rows;
        if (request.action == CommandAction::BarplotSegmentSelect) {
            const std::set<int> replacement(rows.begin(), rows.end());
            const auto coordination = plotCoordinator_.replaceSelection(plot.group, replacement);
            if (!coordination.accepted) return "ERR no active plot/group";
            if (coordination.changed && services_.selection.selectionChanged)
                services_.selection.selectionChanged(
                    coordination.event.group, coordination.event.selectedRows,
                    coordination.event.selectionVersion, false);
            return "OK";
        }
        if (request.action == CommandAction::BarplotSegmentApplyColorToRows) {
            const std::string color = applicationState_.selectedColor(plot.group);
            for (int row : rows) applicationState_.setPointColor(plot.group, row, color);
            if (services_.selection.rowColorsChanged)
                services_.selection.rowColorsChanged(plot.group, rows, true, !rows.empty());
            return "OK";
        }
        std::map<int, std::string> rowColors;
        for (const auto &entry : applicationState_.pointColors(plot.group))
            rowColors[entry.first] = entry.second;
        std::set<int> selection;
        applicationState_.selectedRows(plot.group, selection);
        const auto levels = BarplotSplitLevels(plot);
        const auto levelIndex = BarplotLevelIndex(levels, reference->segment.level);
        const auto visual = ResolveBarplotSegmentVisual(
            plot, reference->category, reference->segment.level, levelIndex);
        return "OK\t" + BarplotSegmentTooltipText(
            plot.xLabel, plot.barplotSplitVariable, plot.barplotWidthMode,
            BarplotBinsForModel(plot)[reference->barIndex], plot.barplotTotalN,
            reference->segment, visual, rowColors, selection, BarplotPaletteOrder(),
            plot.barplotRowColorDisplay != "hide");
    }
    case CommandAction::BarplotSetSplitStrokeWidthPrompt:
        // Native front ends should present their own non-blocking prompt and
        // dispatch BARPLOT_SPLIT_STROKE_WIDTH with the chosen value.
        return "ERR native bar-border prompt required";
    case CommandAction::HistogramBreaks:
    case CommandAction::HistogramSetBins:
    case CommandAction::HistogramSetBinningRule:
    case CommandAction::HistogramDensityInfo:
    case CommandAction::HistogramShowDensity:
    case CommandAction::HistogramSetDensityMode:
    case CommandAction::HistogramSetDensityBandwidth:
    case CommandAction::HistogramSetDensityAdjust: {
        if (request.args.empty()) return "ERR missing plot id";
        if (!services_.queries.plot) return std::nullopt;
        PlotModel plot;
        if (!services_.queries.plot(request.args[0], plot)) return "ERR no active plot";
        if (plot.kind != "histogram") return "ERR plot is not a histogram";
        if (request.action == CommandAction::HistogramBreaks) {
            return HistogramBreaksResponseText(plot);
        }
        if (request.action == CommandAction::HistogramDensityInfo) {
            return HistogramDensityInfoResponseText(plot);
        }
        if (!services_.selection.mutateHistogram) return std::nullopt;

        std::function<void(PlotModel &)> mutation;
        if (request.action == CommandAction::HistogramShowDensity) {
            if (request.args.size() < 2) return "ERR missing density visibility";
            bool showDensity = request.args[1] == "TRUE";
            std::string densityMode = plot.histogramDensityMode;
            if (showDensity && densityMode == "none") {
                const HistogramDensityState state =
                    HistogramStateAfterToggleDensity(false, densityMode);
                showDensity = state.showDensity;
                densityMode = state.densityMode;
            }
            mutation = [showDensity, densityMode](PlotModel &target) {
                target.histogramShowDensity = showDensity;
                target.histogramDensityMode = densityMode;
            };
        } else if (request.action == CommandAction::HistogramSetDensityMode) {
            if (request.args.size() < 2) return "ERR invalid histogram density mode";
            const HistogramDensityState state = HistogramStateAfterSetDensityMode(
                plot.histogramShowDensity, plot.histogramDensityMode, request.args[1]);
            if (!state.changed && state.densityMode != request.args[1]) {
                return "ERR invalid histogram density mode";
            }
            mutation = [state](PlotModel &target) {
                target.histogramShowDensity = state.showDensity;
                target.histogramDensityMode = state.densityMode;
            };
        } else if (request.action == CommandAction::HistogramSetDensityBandwidth) {
            if (request.args.size() < 2) return "ERR missing histogram density bandwidth";
            const std::optional<double> bandwidth =
                ParseHistogramDensityBandwidthCommandValue(request.args[1]);
            if (!bandwidth.has_value()) return "ERR density bandwidth must be non-negative";
            mutation = [bandwidth](PlotModel &target) {
                target.histogramDensityBw = *bandwidth;
                target.histogramShowDensity = true;
            };
        } else if (request.action == CommandAction::HistogramSetDensityAdjust) {
            if (request.args.size() < 2) return "ERR missing histogram density adjust";
            const std::optional<double> adjust =
                ParseHistogramDensityAdjustCommandValue(request.args[1]);
            if (!adjust.has_value()) return "ERR density adjust must be positive";
            mutation = [adjust](PlotModel &target) {
                target.histogramDensityAdjust = *adjust;
                target.histogramShowDensity = true;
            };
        } else if (request.action == CommandAction::HistogramSetBins) {
            if (request.args.size() < 2) return "ERR missing histogram bin count";
            const int bins = std::atoi(request.args[1].c_str());
            if (bins < 1) return "ERR histogram bin count must be positive";
            mutation = [bins](PlotModel &target) { RebinHistogram(target, bins); };
        } else {
            if (request.args.size() < 2) return "ERR missing histogram binning rule";
            const std::string &rule = request.args[1];
            if (rule != "sturges" && rule != "fd" && rule != "scott" && rule != "sqrt") {
                return "ERR invalid histogram binning rule";
            }
            mutation = [rule](PlotModel &target) { RebinHistogramByRule(target, rule); };
        }
        if (!services_.selection.mutateHistogram(request.args[0], mutation)) {
            return "ERR no active plot";
        }
        RefreshRegisteredBasicPlotCodeReference(applicationState_, request.args[0]);
        if (services_.ui.redrawPlot) services_.ui.redrawPlot(request.args[0]);
        return "OK";
    }
    case CommandAction::ClosePlot: {
        if (request.args.empty()) return "ERR missing plot id";
        if (!services_.ui.closePlot) return std::nullopt;
        return services_.ui.closePlot(request.args[0]) ? "OK" : "ERR no active plot";
    }
    case CommandAction::RedrawPlot: {
        if (request.args.empty()) return "ERR missing plot id";
        if (!services_.queries.plot || !services_.ui.redrawPlot) return std::nullopt;
        PlotModel plot;
        if (!services_.queries.plot(request.args[0], plot)) return "ERR no active plot";
        services_.ui.redrawPlot(request.args[0]);
        return "OK";
    }
    case CommandAction::PanelShow:
    case CommandAction::PanelHide: {
        if (!services_.ui.setPanelVisible) return std::nullopt;
        services_.ui.setPanelVisible(request.action == CommandAction::PanelShow);
        return "OK";
    }
    case CommandAction::Panel:
    case CommandAction::PaletteState: {
        if (request.args.empty()) {
            return request.action == CommandAction::Panel
                ? "ERR missing panel state" : "ERR missing palette state";
        }
        const std::string &state = request.args[0];
        if (state != "show" && state != "hide") {
            return request.action == CommandAction::Panel
                ? "ERR invalid panel state" : "ERR invalid palette state";
        }
        const bool show = state == "show";
        const auto &service = request.action == CommandAction::Panel
            ? services_.ui.setPanelVisible : services_.ui.setPaletteVisible;
        if (!service) return std::nullopt;
        service(show);
        return "OK";
    }
    case CommandAction::SetActiveDataset:
    case CommandAction::DataSetActiveDataset: {
        if (request.args.empty()) return "ERR missing dataset name";
        const bool changed = applicationState_.datasets().activeDatasetGroup() !=
            request.args[0];
        if (!applicationState_.datasets().setActiveDataset(request.args[0])) {
            return "ERR dataset is not registered";
        }
        if (changed && services_.ui.activeDatasetChanged) {
            services_.ui.activeDatasetChanged(request.args[0]);
        }
        return "OK";
    }
    case CommandAction::OpenDataSheet: {
        if (!services_.ui.openDataSheet) return std::nullopt;
        std::string group;
        if (!request.args.empty()) {
            group = request.args[0];
        } else {
            group = applicationState_.datasets().activeDatasetGroup();
        }
        services_.ui.openDataSheet(group);
        return "OK";
    }
    case CommandAction::VariableView: {
        if (!services_.ui.showVariablesWindow) return std::nullopt;
        const std::string group = request.args.empty()
            ? applicationState_.datasets().activeDatasetGroup()
            : request.args[0];
        if (group.empty()) return "ERR missing group name";
        if (!applicationState_.datasets().find(group)) {
            return "ERR dataset is not registered";
        }
        services_.ui.showVariablesWindow(group);
        return "OK";
    }
    case CommandAction::AddSelectionColumn:
    case CommandAction::AddPointColorColumn: {
        const std::string group = request.args.empty()
            ? applicationState_.datasets().activeDatasetGroup()
            : request.args[0];
        if (group.empty()) return "ERR missing group name";
        DataFrameModel *dataset = applicationState_.datasets().find(group);
        if (!dataset) return "ERR dataset is not registered";
        if (applicationState_.datasetCalculationPending(group))
            return "ERR Wait for the current R calculation before editing this sheet.";
        std::set<int> selected;
        (void)applicationState_.selectedRows(group, selected);
        std::map<int, std::string> colors;
        for (auto const& [row, color] : applicationState_.pointColors(group))
            colors[row] = color;
        std::string created;
        std::string message;
        const std::string kind = request.action == CommandAction::AddSelectionColumn
            ? "selection" : "color";
        if (!AddDerivedDataColumn(*dataset, group, kind, selected, colors,
                                  &created, &message)) {
            return "ERR " + message;
        }
        if (services_.ui.datasetMutated) {
            services_.ui.datasetMutated({DatasetMutationKind::ColumnStructure,
                                         group, created, {}, 0});
        } else if (services_.ui.datasetRegistered) {
            services_.ui.datasetRegistered(group, false);
        }
        return "OK\t" + DerivedDataColumnAddedStatus(created, group);
    }
    case CommandAction::MakeSubsetFromSelection: {
        const std::string group = request.args.empty()
            ? applicationState_.datasets().activeDatasetGroup()
            : request.args[0];
        if (group.empty()) return "ERR missing group name";
        const DataFrameModel *dataset = applicationState_.datasets().find(group);
        if (!dataset) return "ERR dataset is not registered";
        std::set<int> selected;
        if (!applicationState_.selectedRows(group, selected) || selected.empty()) {
            return "ERR no rows are selected";
        }
        const std::string subsetGroup = applicationState_.datasets().uniqueDatasetName(
            group + "_subset");
        const std::vector<int> rows(selected.begin(), selected.end());
        const DataFrameModel subset = SubsetDataFrame(*dataset, rows, subsetGroup);
        if (!applicationState_.registerDataset(subset)) {
            return "ERR could not register the subset dataset";
        }
        if (services_.ui.datasetRegistered) services_.ui.datasetRegistered(subsetGroup, true);
        return "OK\t" + subsetGroup;
    }
    case CommandAction::MakeSubsetFromVariables: {
        if (request.args.size() < 2) return "ERR select at least one variable";
        const std::string &group = request.args[0];
        const DataFrameModel *dataset = applicationState_.datasets().find(group);
        if (!dataset) return "ERR dataset is not registered";
        const std::vector<std::string> variables(request.args.begin() + 1, request.args.end());
        const DataFrameModel subset = SubsetDataFrameColumns(*dataset, variables,
            applicationState_.datasets().uniqueDatasetName(group + "_variables"));
        if (subset.columns.size() != std::set<std::string>(variables.begin(), variables.end()).size()) {
            return "ERR one or more variables are not in the dataset";
        }
        if (!applicationState_.registerDataset(subset)) {
            return "ERR could not register the subset dataset";
        }
        if (services_.ui.datasetRegistered) services_.ui.datasetRegistered(subset.group, true);
        return "OK\t" + subset.group;
    }
    case CommandAction::ShowVariableInformation: {
        if (request.name != "VARIABLE_INFO") return std::nullopt;
        if (request.args.empty()) return "ERR missing group name";
        if (!services_.queries.variableInfo) return std::nullopt;
        std::vector<CommandVariableInfo> variables;
        if (!services_.queries.variableInfo(request.args[0], variables)) {
            return "ERR no active plot/group";
        }
        std::string reply = "OK";
        for (const CommandVariableInfo &variable : variables) {
            reply += "\t" + variable.name + "|" + variable.type + "|" + variable.role;
        }
        return reply;
    }
    case CommandAction::SetDiagnosticPlotProvenance: {
        if (request.args.empty()) return "ERR missing diagnostic plot";
        auto found = applicationState_.plots().find(request.args[0]);
        if (found == applicationState_.plots().end() || !found->second) return "ERR unknown diagnostic plot";
        std::size_t cursor=1; AnalysisProvenance provenance; std::string error;
        if (!ReadAnalysisProvenancePayload(request.args,cursor,provenance,&error)) return "ERR " + error;
        auto& plot=*found->second;
        // Imputation-process plots use synthetic row ids and deliberately keep
        // their plot group unlinked from worksheet selections.  Their window
        // scope, however, belongs to the source dataset recorded by R.  Do not
        // replace plot.group: only capture the real analytical ownership so
        // titles, notes and snapshots report the source N instead of N = 0.
        if (const DataFrameModel *source =
                applicationState_.datasets().find(provenance.dataVersion.datasetId)) {
            const AnalysisScope sourceScope =
                applicationState_.activeAnalysisScope(source->group);
            plot.dataScope = sourceScope;
            plot.dataScopeCaptured = true;
            const std::vector<int> scopeRows = ResolveAnalysisScopeRowIds(
                sourceScope, static_cast<std::size_t>(std::max(0, source->rows)));
            CompleteAnalysisProvenance(
                provenance, *source, sourceScope, scopeRows, {},
                provenance.analysisId, plot.title);
        }
        plot.codeReference.outputId=plot.id; plot.codeReference.analysisId=provenance.analysisId;
        plot.codeReference.outputBlockId="table"; plot.codeReference.kind="plot";
        plot.codeReference.title=plot.title;plot.codeReference.provenance=std::move(provenance);
        applicationState_.registerOutputCodeReference(plot.codeReference);
        if (services_.ui.redrawPlot) services_.ui.redrawPlot(plot.id);
        return "OK";
    }
    case CommandAction::MissingInformationAddVariable: {
        if(request.args.size()!=2) return "ERR missing model or variable";
        const auto* output=applicationState_.outputCodeReference(request.args[0]);
        if(!output) return "ERR the linked model is no longer available";
        const auto controls=BuildMissingInformationTable(*output,applicationState_);
        if(std::find(controls.addVariableOptions.begin(),controls.addVariableOptions.end(),request.args[1])==controls.addVariableOptions.end())
            return "ERR this variable is not available to add to the linked model";
        auto found=applicationState_.generalizedGLMs().find(request.args[0]);
        if(found!=applicationState_.generalizedGLMs().end()) {
            auto& model=found->second;
            const auto edit=AddModelSpecificationTerm(model,controls.addVariableOptions,request.args[1]);
            if(!edit.ok) return "ERR "+edit.message;
            if(edit.changed) {
                ++model.modelVersion;model.rFitPending=false;
                if(model.autoRefit && services_.ui.requestGeneralizedGLMFit)
                    services_.ui.requestGeneralizedGLMFit(model.id);
                else model.status=GeneralizedGLMPendingManualFitStatus();
                if(services_.ui.refreshGeneralizedGLM) services_.ui.refreshGeneralizedGLM(model.id);
            }
            return "OK";
        }
        for(const auto& entry:applicationState_.groupModels())
            if(request.args[0]=="glm:"+entry.first)
                return dispatch({"MODEL_ADD_TERM",entry.first,request.args[1]});
        return "ERR the linked model is no longer available";
    }
    case CommandAction::ShowMissingInformation: {
        if (request.args.empty()) return "ERR missing analytical output";
        const auto* output=applicationState_.outputCodeReference(request.args[0]);
        if (!output || output->provenance.missingInformationRows.empty()) return "ERR missing-information diagnostics are unavailable for this output";
        if (!services_.ui.showTable1) return std::nullopt;
        linkedMissingInformationSources_.insert(output->outputId);
        auto table=BuildMissingInformationTable(*output,applicationState_);
        applicationState_.registerOutputCodeReference(table.codeReference);
        services_.ui.showTable1(table); return "OK";
    }
    case CommandAction::ShowRCode: {
        if (request.args.size() < 2) return "ERR malformed SHOW_R_CODE command";
        if (!services_.ui.showRCode) return std::nullopt;
        const std::string &kind = request.args[0];
        const std::string &id = request.args[1];
        std::string title;
        std::string code;
        if (kind == "dataset" || kind == "column") {
            const DataFrameModel *dataset = applicationState_.datasets().find(id);
            if (!dataset) return "ERR dataset is not registered";
            if (kind == "column") {
                if (request.args.size() < 3) return "ERR missing column name";
                title = "Column R Code — " + request.args[2];
                code = BuildColumnProvenanceRCode(dataset->provenance, request.args[2]);
            } else {
                title = "Data R Code — " + id;
                code = BuildDataProvenanceRCode(dataset->provenance);
            }
        } else {
            const OutputCodeReference *output = applicationState_.outputCodeReference(id);
            if (!output) return "ERR R code is not available for this output";
            title = "R Code — " + (output->title.empty() ? id : output->title);
            code = BuildAnalysisVerificationRCode(output->provenance,
                                                  output->outputBlockId);
        }
        std::optional<DataFrameModel> verificationData;
        if (kind != "dataset" && kind != "column") {
            const OutputCodeReference *output = applicationState_.outputCodeReference(id);
            if (output && !output->provenance.verificationRCode.empty())
                verificationData = VerificationExportData(applicationState_, output->provenance);
        }
        services_.ui.showRCode(title, code, verificationData);
        return "OK";
    }
    case CommandAction::ShowRPublicationCode: {
        if (request.args.empty()) return "ERR missing output id";
        if (!services_.ui.showRCode) return std::nullopt;
        const OutputCodeReference *output = applicationState_.outputCodeReference(request.args[0]);
        if (!output) return "ERR publication code is not available for this output";
        PublicationBackend backend = PublicationBackend::Tinytable;
        if (request.args.size() >= 2) {
            if (request.args[1] == "ggplot2") backend = PublicationBackend::Ggplot2;
            else if (request.args[1] == "latex") backend = PublicationBackend::Latex;
            else if (request.args[1] == "latex_pdf") backend = PublicationBackend::LatexPdf;
        } else if (output->publication.plot) backend = PublicationBackend::Ggplot2;
        const std::string code = BuildPublicationRCode(*output, backend);
        if (code.empty()) return "ERR publication code is not available for this output";
        const std::string prefix = backend == PublicationBackend::Ggplot2 ? "ggplot2" :
            backend == PublicationBackend::Tinytable ? "tinytable" : "LaTeX/PDF";
        services_.ui.showRCode(prefix + " Publication Code — " +
            (output->title.empty() ? output->outputId : output->title), code,
            std::nullopt);
        return "OK";
    }
    case CommandAction::ResetWindowLayout:
        if (!services_.ui.resetWindowLayout) return std::nullopt;
        services_.ui.resetWindowLayout();
        return "OK";
    case CommandAction::ShowActiveDataset:
        if (!services_.ui.showActiveDataset) return std::nullopt;
        services_.ui.showActiveDataset();
        return "OK";
    case CommandAction::NativeImportFile: {
        if (request.args.empty()) return "ERR missing import path";
        if (!services_.ui.importDataFile) return std::nullopt;
        std::string message;
        return services_.ui.importDataFile(request.args[0], message)
            ? "OK\t" + message : "ERR " + message;
    }
    case CommandAction::SetVariableType: {
        if (request.args.size() < 3) return "ERR malformed SET_VARIABLE_TYPE command";
        VariableTypeConversionSpecification conversion;
        bool hasConversion = false;
        std::size_t option = 3;
        auto parseCount = [&](std::size_t &count) {
            if (option >= request.args.size()) return false;
            char *end = nullptr;
            const unsigned long long parsed = std::strtoull(request.args[option].c_str(), &end, 10);
            if (!end || end == request.args[option].c_str() || *end != '\0') return false;
            ++option;
            count = static_cast<std::size_t>(parsed);
            return true;
        };
        while (option < request.args.size()) {
            const std::string key = request.args[option++];
            if (key == "invert_binary") {
                if (option >= request.args.size()) return "ERR malformed invert_binary option";
                conversion.invertBinary = request.args[option++] == "1" ||
                    request.args[option - 1] == "true" || request.args[option - 1] == "TRUE";
                hasConversion = true;
            } else if (key == "ordinal_positions") {
                if (option >= request.args.size()) return "ERR malformed ordinal_positions option";
                conversion.useOrdinalPositions = !(request.args[option] == "0" ||
                    request.args[option] == "false" || request.args[option] == "FALSE");
                ++option;
                hasConversion = true;
            } else if (key == "category_order") {
                std::size_t count = 0;
                if (!parseCount(count) || option + count > request.args.size())
                    return "ERR malformed category_order option";
                conversion.categoryOrder.reserve(count);
                for (std::size_t index = 0; index < count; ++index)
                    conversion.categoryOrder.push_back(DecodeCommandField(request.args[option++]));
                hasConversion = true;
            } else if (key == "mapping") {
                std::size_t count = 0;
                if (!parseCount(count) || option + count * 2U > request.args.size())
                    return "ERR malformed numeric mapping option";
                for (std::size_t index = 0; index < count; ++index) {
                    const std::string label = DecodeCommandField(request.args[option++]);
                    const std::string value = DecodeCommandField(request.args[option++]);
                    conversion.numericMapping[label] = value;
                }
                hasConversion = true;
            } else {
                return "ERR unsupported variable type conversion option `" + key + "`";
            }
        }
        VariableTypeChangeEffects effects;
        std::string message;
        if (!applicationState_.setVariableType(request.args[0], request.args[1], request.args[2],
                                               effects, &message,
                                               hasConversion ? &conversion : nullptr)) {
            return "ERR " + message;
        }
        if (services_.ui.datasetVariableTypeChanged) {
            services_.ui.datasetVariableTypeChanged(request.args[0], request.args[1],
                                                    NormalizeVariableType(request.args[2]), effects);
        }
        // Variable type is dataset metadata, just like description and
        // decimals.  Publish the general mutation as well as the specialised
        // analysis effects so every open dataset-backed view refreshes from
        // the canonical DataColumn instead of retaining its opening snapshot.
        if (services_.ui.datasetMutated) services_.ui.datasetMutated({
            DatasetMutationKind::VariableMetadata, request.args[0], request.args[1], {}, 0 });
        return "OK\t" + message;
    }
    case CommandAction::SetDefaultVariableRole: {
        if (request.args.size() < 3)
            return "ERR malformed SET_DEFAULT_VARIABLE_ROLE command";
        std::string message;
        if (!applicationState_.setDefaultVariableRole(
                request.args[0], request.args[1], request.args[2], &message))
            return "ERR " + message;
        return "OK\t" + VariableRoleChangedStatus(request.args[1], request.args[2]);
    }
    case CommandAction::SetDataCell: {
        if (request.args.size() < 4) return "ERR malformed SET_DATA_CELL command";
        DataFrameModel *dataframe = applicationState_.datasets().find(request.args[0]);
        if (!dataframe) return "ERR dataset is not registered";
        if (applicationState_.datasetCalculationPending(request.args[0]))
            return "ERR Wait for the current R calculation before editing this sheet.";
        char *end = nullptr;
        const long row = std::strtol(request.args[2].c_str(), &end, 10);
        if (!end || *end != '\0' || row < 1 || row > dataframe->rows)
            return "ERR invalid row number";
        applicationState_.preserveCurrentDatasetVersion(request.args[0]);
        std::string message;
        if (!SetDataFrameCellValue(*dataframe, request.args[1],
                                   static_cast<std::size_t>(row - 1), request.args[3],
                                   &message)) {
            return "ERR " + message;
        }
        DatasetMutationEvent event;
        event.kind = DatasetMutationKind::CellValue;
        event.group = request.args[0];
        event.variable = request.args[1];
        event.row = static_cast<int>(row);
        event.valueChangeEffects = applicationState_.applyDatasetValueChange(
            event.group, event.variable);
        if (services_.ui.datasetMutated) services_.ui.datasetMutated(event);
        return "OK\t" + message;
    }
    case CommandAction::RenameVariable: {
        if (request.args.size() < 3) return "ERR malformed RENAME_VARIABLE command";
        DataFrameModel *dataframe = applicationState_.datasets().find(request.args[0]);
        if (!dataframe) return "ERR dataset is not registered";
        if (applicationState_.datasetCalculationPending(request.args[0]))
            return "ERR Wait for the current R calculation before editing this sheet.";
        applicationState_.preserveCurrentDatasetVersion(request.args[0]);
        std::string message;
        if (!RenameDataFrameColumn(*dataframe, request.args[1], request.args[2], &message))
            return "ERR " + message;
        DatasetMutationEvent event;
        event.kind = DatasetMutationKind::VariableRename;
        event.group = request.args[0];
        event.variable = request.args[2];
        event.previousVariable = request.args[1];
        event.valueChangeEffects = applicationState_.applyVariableRename(
            event.group, event.previousVariable, event.variable);
        if (services_.ui.datasetMutated) services_.ui.datasetMutated(event);
        return "OK\t" + VariableRenamedStatus(request.args[1], request.args[2]);
    }
    case CommandAction::SetVariableDescription: {
        if (request.args.size() < 3)
            return "ERR malformed SET_VARIABLE_DESCRIPTION command";
        DataFrameModel *dataframe = applicationState_.datasets().find(request.args[0]);
        if (!dataframe) return "ERR dataset is not registered";
        DataColumn *column = FindDataColumnInDataFrame(*dataframe, request.args[1]);
        if (!column) return "ERR " + VariableNotFoundStatus(request.args[1]);
        applicationState_.preserveCurrentDatasetVersion(request.args[0]);
        SetDataColumnDescription(*column, request.args[2]);
        RecordDataFrameMetadataChange(*dataframe, request.args[1],
                                      "Change column description");
        if (services_.ui.datasetMutated) services_.ui.datasetMutated({
            DatasetMutationKind::VariableMetadata, request.args[0], request.args[1], {}, 0 });
        return "OK\t" + VariableDescriptionChangedStatus(request.args[1]);
    }
    case CommandAction::SetVariableDecimals: {
        if (request.args.size() < 3) return "ERR malformed SET_VARIABLE_DECIMALS command";
        DataFrameModel *dataframe = applicationState_.datasets().find(request.args[0]);
        if (!dataframe) return "ERR dataset is not registered";
        DataColumn *column = FindDataColumnInDataFrame(*dataframe, request.args[1]);
        if (!column) return "ERR " + VariableNotFoundStatus(request.args[1]);
        applicationState_.preserveCurrentDatasetVersion(request.args[0]);
        char *end = nullptr;
        const long decimals = std::strtol(request.args[2].c_str(), &end, 10);
        if (!end || *end != '\0') return "ERR " + VariableDecimalsInvalidStatus(false);
        std::string message;
        if (!SetDataColumnDecimals(*column, static_cast<int>(decimals), &message))
            return "ERR " + message;
        RecordDataFrameMetadataChange(*dataframe, request.args[1],
                                      "Change displayed decimals");
        if (services_.ui.datasetMutated) services_.ui.datasetMutated({
            DatasetMutationKind::VariableMetadata, request.args[0], request.args[1], {}, 0 });
        return "OK\t" + VariableDecimalsChangedStatus(request.args[1], static_cast<int>(decimals));
    }
    case CommandAction::SetLabelColumn: {
        if (request.args.empty()) return "ERR missing group name";
        const std::string column = request.args.size() >= 2 ? request.args[1] : std::string{};
        if (!applicationState_.setLabelColumn(request.args[0], column))
            return "ERR variable was not found in the dataset";
        if (services_.ui.datasetMutated) services_.ui.datasetMutated({
            DatasetMutationKind::LabelColumn, request.args[0], column, {}, 0 });
        return "OK";
    }
    case CommandAction::RegisterDataset:
    case CommandAction::RegisterDatasetSilent: {
        if (request.args.size() < 2) return "ERR malformed REGISTER_DATASET command";
        const std::string &group = request.args[0];
        std::size_t cursor = 1;
        if (cursor < request.args.size() && request.args[cursor] == "VARS") {
            ++cursor;
            if (cursor >= request.args.size()) return "ERR malformed variable payload";
            const long variableCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            for (long i = 0; i < variableCount; ++i) {
                if (cursor + 1 >= request.args.size()) return "ERR malformed variable payload";
                ++cursor;
                const long valueCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
                if (valueCount < 0 || cursor + static_cast<std::size_t>(valueCount) > request.args.size()) {
                    return "ERR invalid variable value count";
                }
                cursor += static_cast<std::size_t>(valueCount);
            }
        }
        if (cursor < request.args.size() && request.args[cursor] == "VARMETA") {
            ++cursor;
            if (cursor >= request.args.size()) return "ERR malformed variable metadata";
            const long metadataCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (metadataCount < 0 || cursor + static_cast<std::size_t>(metadataCount) * 2 > request.args.size()) {
                return "ERR invalid variable metadata count";
            }
            cursor += static_cast<std::size_t>(metadataCount) * 2;
        }
        DataFrameModel dataframe;
        bool parsed = false;
        std::string error;
        if (!ParseDataFramePayload(request.args, cursor, group, dataframe, &parsed, error)) return error;
        const DataFrameModel *existing = parsed
            ? applicationState_.datasets().find(group)
            : nullptr;
        // A native cell edit may happen while R is fitting a model.  An older
        // result must never register its pre-edit copy over the live sheet.
        if (parsed && existing && !existing->syncSessionToken.empty() &&
            existing->syncSessionToken == dataframe.syncSessionToken &&
            dataframe.dataVersion < existing->dataVersion) {
            return "ERR dataset changed in LinkEDA while R was calculating; synchronize and refit";
        }
        const bool redundantSilentRegistration =
            request.action == CommandAction::RegisterDatasetSilent &&
            existing && DataFramesAreEquivalent(*existing, dataframe);
        if (parsed && !redundantSilentRegistration) {
            applicationState_.registerDataset(dataframe);
        }
        if (!redundantSilentRegistration && services_.ui.datasetRegistered) {
            services_.ui.datasetRegistered(group, request.action == CommandAction::RegisterDataset);
        }
        return "OK";
    }
    case CommandAction::CloseAll:
        if (!services_.ui.closeAll) return std::nullopt;
        applicationState_.clearWorkbenchModels();
        services_.ui.closeAll();
        return "OK";
    case CommandAction::Overlays: {
        if (request.args.empty()) return "ERR missing plot id";
        if (!services_.queries.plot) return std::nullopt;
        PlotModel plot;
        if (!services_.queries.plot(request.args[0], plot)) return "ERR no active plot";
        return PlotOverlaysResponseText(plot);
    }
    case CommandAction::ExportPlot: {
        if (request.args.size() < 3) return "ERR malformed EXPORT_PLOT command";
        if (!services_.ui.exportPlot) return "ERR plot view is not available for export";
        std::string format = request.args[2];
        std::transform(format.begin(), format.end(), format.begin(),
            [](unsigned char ch) { return static_cast<char>(std::toupper(ch)); });
        if (format != "PNG" && format != "PDF" && format != "SVG") return "ERR unsupported plot export format";
        return services_.ui.exportPlot(request.args[0], request.args[1], format)
            ? "OK" : "ERR plot view is not available for export";
    }
    case CommandAction::CopyPlot: {
        if (request.args.size() < 2) return "ERR malformed COPY_PLOT command";
        if (!services_.ui.copyPlot) return "ERR plot view is not available for copying";
        std::string format = request.args[1];
        std::transform(format.begin(), format.end(), format.begin(),
            [](unsigned char ch) { return static_cast<char>(std::toupper(ch)); });
        if (format != "PNG" && format != "PDF" && format != "SVG" && format != "EMF") {
            return "ERR unsupported plot copy format";
        }
        return services_.ui.copyPlot(request.args[0], format)
            ? "OK" : "ERR plot view is not available for copying";
    }
    case CommandAction::PlotTheme: {
        if (request.args.empty()) return "OK\t" + plotTheme_;
        if (!PlotThemeIsValid(request.args[0])) return "ERR unsupported plot theme";
        plotTheme_ = request.args[0];
        // R verification/publication code follows the same application-wide
        // theme as the native view.  This is presentation-only synchronization
        // and must never enqueue or repeat a statistical calculation.
        for (auto &[plotId, plot] : applicationState_.plots()) {
            if (!plot) continue;
            plot->rExportTheme = plotTheme_;
            if (plot->kind == "scatter" || plot->kind == "time_series" ||
                plot->kind == "histogram" || plot->kind == "barplot" ||
                plot->kind == "boxplot" || plot->kind == "trellis_scatterplot") {
                RegisterBasicPlotCodeReference(applicationState_, *plot);
            } else if (plot->kind == "glm_interaction") {
                SynchronizeRegressionPlotCodeReferenceDisplay(*plot);
                applicationState_.registerOutputCodeReference(plot->codeReference);
            } else if (plot->codeReference.publication.plot) {
                plot->codeReference.publication.plot->theme = plotTheme_;
                applicationState_.registerOutputCodeReference(plot->codeReference);
            }
        }
        if (services_.ui.refreshPlotTheme) services_.ui.refreshPlotTheme();
        return "OK\t" + plotTheme_;
    }
    default:
        return std::nullopt;
    }
}

} // namespace core
} // namespace rlispstat
