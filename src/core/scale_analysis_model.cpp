#include <array>
#include "scale_analysis_model.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <iomanip>
#include <set>
#include <sstream>
#include <cstdlib>

namespace rlispstat {
namespace core {

namespace {

std::string Lower(std::string value)
{
    std::transform(value.begin(), value.end(), value.begin(), [](unsigned char c) {
        return static_cast<char>(std::tolower(c));
    });
    return value;
}

const DataColumn *FindColumn(const DataFrameModel &dataframe, const std::string &name)
{
    const auto found = std::find_if(dataframe.columns.begin(), dataframe.columns.end(),
        [&](const DataColumn &column) { return column.name == name; });
    return found == dataframe.columns.end() ? nullptr : &*found;
}

bool IsScaleRole(const std::string &role)
{
    const std::string normalized = Lower(role);
    return normalized == "scale_item" || normalized == "scale item" ||
        normalized == "scale-item" || normalized == "item";
}

double ParseResultNumber(const std::string &value)
{
    if (value.empty() || value == "—" || value == "-")
        return std::numeric_limits<double>::quiet_NaN();
    char *end = nullptr;
    const double parsed = std::strtod(value.c_str(), &end);
    return end && end != value.c_str() && std::isfinite(parsed)
        ? parsed : std::numeric_limits<double>::quiet_NaN();
}

int ParseResultInteger(const std::string &value)
{
    const double parsed = ParseResultNumber(value);
    return std::isfinite(parsed) && parsed >= 0.0
        ? static_cast<int>(std::lround(parsed)) : 0;
}

} // namespace

std::string ScaleAnalysisWindowTitle(bool multipleImputation)
{
    return multipleImputation ? "Scale Analysis - Multiple Imputation" : "Scale Analysis";
}

std::string ScaleAnalysisChildWindowTitle(
    ScaleAnalysisChildKind kind,
    bool multipleImputation)
{
    std::string title;
    switch (kind) {
    case ScaleAnalysisChildKind::Reliability: title = "Scale Reliability"; break;
    case ScaleAnalysisChildKind::InterItemCorrelations: title = "Inter-item Correlations"; break;
    case ScaleAnalysisChildKind::Dimensionality: title = "Scale Dimensionality"; break;
    case ScaleAnalysisChildKind::ScaleScores: title = "Scale Scores"; break;
    case ScaleAnalysisChildKind::ItemRestPlot: title = "Item-rest Correlations"; break;
    case ScaleAnalysisChildKind::ReliabilityIfDeletedPlot: title = "Reliability if Deleted"; break;
    case ScaleAnalysisChildKind::ItemDistributionsPlot: title = "Item Distributions"; break;
    case ScaleAnalysisChildKind::ScaleScoreDistributionPlot: title = "Scale-score Distribution"; break;
    case ScaleAnalysisChildKind::CorrelationHeatmap: title = "Inter-item Correlation Heatmap"; break;
    case ScaleAnalysisChildKind::ScreePlot: title = "Scale Scree Plot"; break;
    case ScaleAnalysisChildKind::LoadingPlot: title = "Factor Loadings"; break;
    case ScaleAnalysisChildKind::Biplot: title = "Scale Factor Biplot"; break;
    }
    if (multipleImputation) title += " - Multiple Imputation";
    return title;
}

std::string ScaleItemTypeToken(ScaleItemType type)
{
    return type == ScaleItemType::Ordinal ? "ordinal" : "numeric";
}

std::string ScaleItemTypeDisplayName(ScaleItemType type)
{
    return type == ScaleItemType::Ordinal ? "Ordinal" : "Numeric";
}

std::string ScaleItemTypeMenuLabel(ScaleItemType type, const DataColumn *column)
{
    return type == ScaleItemType::Ordinal && column && DataColumnObservedLevelCount(*column) == 2U
        ? "Binary (2 categories)" : ScaleItemTypeDisplayName(type);
}

bool ParseScaleItemType(const std::string &token, ScaleItemType &type)
{
    const std::string normalized = Lower(token);
    if (normalized == "numeric" || normalized == "continuous") {
        type = ScaleItemType::Numeric;
        return true;
    }
    if (normalized == "ordinal" || normalized == "ordered") {
        type = ScaleItemType::Ordinal;
        return true;
    }
    return false;
}

bool ScaleCorrelationBasisIsValid(const std::string &basis)
{
    const std::string value = Lower(basis);
    return value == "auto" || value == "pearson" || value == "polychoric" || value == "mixed";
}

std::vector<std::pair<std::string, std::string>> ScaleAnalysisSummaryFields(const ScaleAnalysisResult &result)
{
    const auto &s = result.scaleSummary;
    return {{"Items", s.numberOfItems}, {"N used", s.nUsed},
        {"Missing in original (%)", s.missingPercent}, {"Scale mean", s.scaleMean},
        {"Scale SD", s.scaleSd}, {"Mean inter-item r", s.meanInterItemCorrelation},
        {"Alpha", s.alpha}, {"Standardized alpha", s.standardizedAlpha}, {"Omega total", s.omegaTotal}};
}

bool ScaleScoreMethodIsValid(const std::string &method)
{
    const std::string value = Lower(method);
    return value == "mean" || value == "sum";
}

bool ScaleExtractionIsValid(const std::string &method)
{
    const std::string value = Lower(method);
    return value == "minres" || value == "ml" || value == "pa";
}

bool ScaleRotationIsValid(const std::string &rotation)
{
    const std::string value = Lower(rotation);
    return value == "oblimin" || value == "varimax" || value == "quartimax" ||
        value == "promax" || value == "none";
}

bool ScaleColumnIsDichotomousCategorical(const DataColumn &column)
{
    return VariableTypeIsFactorLike(column.type) &&
        DataColumnObservedLevelCount(column) == 2U;
}

bool ScaleColumnIsEligible(const DataColumn &column)
{
    const std::string type = NormalizeVariableType(column.type);
    return (type == "numeric" && DataColumnAllowsNumeric(column)) ||
        type == "ordered" || ScaleColumnIsDichotomousCategorical(column);
}

ScaleItemType DefaultScaleItemType(const DataColumn &column)
{
    return NormalizeVariableType(column.type) == "ordered" ||
        ScaleColumnIsDichotomousCategorical(column)
        ? ScaleItemType::Ordinal : ScaleItemType::Numeric;
}

std::vector<std::string> EligibleScaleVariables(
    const DataFrameModel &dataframe,
    const std::vector<ScaleItemSpecification> &items)
{
    std::set<std::string> used;
    for (const auto &item : items) used.insert(item.variable);
    std::vector<std::string> result;
    for (const DataColumn &column : dataframe.columns) {
        if (ScaleColumnIsEligible(column) && !used.count(column.name)) result.push_back(column.name);
    }
    return result;
}

std::vector<ScaleItemSpecification> InitialScaleItems(
    const DataFrameModel &dataframe,
    const std::map<std::string, std::string> &roles,
    const ScaleAnalysisSpecification *saved)
{
    std::vector<ScaleItemSpecification> result;
    if (saved) {
        for (ScaleItemSpecification item : saved->items) {
            const DataColumn *column = FindColumn(dataframe, item.variable);
            if (!column || !ScaleColumnIsEligible(*column)) continue;
            result.push_back(std::move(item));
        }
        return result;
    }
    for (const DataColumn &column : dataframe.columns) {
        const auto role = roles.find(column.name);
        if (role == roles.end() || !IsScaleRole(role->second) || !ScaleColumnIsEligible(column)) continue;
        ScaleItemSpecification item;
        item.variable = column.name;
        item.type = DefaultScaleItemType(column);
        result.push_back(std::move(item));
    }
    // Deliberately empty when no explicit scale-item roles exist. Dataset
    // order and numeric compatibility are never used as an implicit scale.
    return result;
}

std::string ScaleAnalysisSpecificationFingerprint(
    const ScaleAnalysisSpecification &specification)
{
    std::ostringstream out;
    out << specification.datasetId << '|' << Lower(specification.correlationBasis)
        << '|' << Lower(specification.scoreMethod) << '|' << specification.minimumValidItems
        << '|' << (specification.computeOmega ? 1 : 0)
        << '|' << (specification.showDimensionality ? 1 : 0)
        << '|' << Lower(specification.dimensionalityMethod)
        << '|' << Lower(specification.dimensionalityMissingMode)
        << '|' << (specification.dimensionalityScale ? 1 : 0)
        << '|' << specification.factorCount << '|' << Lower(specification.extraction)
        << '|' << Lower(specification.rotation) << '|' << specification.parallelIterations;
    out << std::setprecision(17);
    for (const ScaleItemSpecification &item : specification.items) {
        out << "||" << item.variable << ':' << ScaleItemTypeToken(item.type)
            << ':' << (item.reversed ? 1 : 0) << ':' << (item.hasScoringRange ? 1 : 0);
        if (item.hasScoringRange) out << ':' << item.scoringMinimum << ':' << item.scoringMaximum;
    }
    return out.str();
}

bool ValidateScaleAnalysisSpecification(
    const DataFrameModel &dataframe,
    ScaleAnalysisSpecification &specification,
    std::string &error)
{
    error.clear();
    if (specification.datasetId.empty()) specification.datasetId = dataframe.group;
    if (specification.datasetId != dataframe.group) {
        error = "The scale specification belongs to a different dataset.";
        return false;
    }
    if (!ScaleCorrelationBasisIsValid(specification.correlationBasis) ||
        !ScaleScoreMethodIsValid(specification.scoreMethod) ||
        !ScaleExtractionIsValid(specification.extraction) ||
        !ScaleRotationIsValid(specification.rotation) ||
        (Lower(specification.dimensionalityMethod) != "factor" &&
         Lower(specification.dimensionalityMethod) != "pca") ||
        (Lower(specification.dimensionalityMissingMode) != "listwise" &&
         Lower(specification.dimensionalityMissingMode) != "pairwise")) {
        error = "The scale analysis contains an unsupported option.";
        return false;
    }
    std::set<std::string> variables;
    for (ScaleItemSpecification &item : specification.items) {
        const DataColumn *column = FindColumn(dataframe, item.variable);
        if (!column || !ScaleColumnIsEligible(*column)) {
            error = "Scale item `" + item.variable +
                "` is unavailable or is not continuous, ordinal, or dichotomous.";
            return false;
        }
        if (!variables.insert(item.variable).second) {
            error = "Scale item `" + item.variable + "` is duplicated.";
            return false;
        }
        if (item.type == ScaleItemType::Numeric && item.reversed && !item.hasScoringRange) {
            error = "Reverse scoring continuous item `" + item.variable +
                "` requires a theoretical scoring range.";
            return false;
        }
        if (item.hasScoringRange && (!std::isfinite(item.scoringMinimum) ||
            !std::isfinite(item.scoringMaximum) || item.scoringMinimum >= item.scoringMaximum)) {
            error = "The scoring range for `" + item.variable + "` is invalid.";
            return false;
        }
    }
    specification.minimumValidItems = std::max(0, std::min(
        specification.minimumValidItems, static_cast<int>(specification.items.size())));
    specification.factorCount = std::max(0, specification.factorCount);
    specification.parallelIterations = std::max(1, specification.parallelIterations);
    specification.fingerprint = ScaleAnalysisSpecificationFingerprint(specification);
    return true;
}

bool ScaleAnalysisResultMatchesSpecification(
    const ScaleAnalysisSpecification &specification,
    const ScaleAnalysisResult &result)
{
    return result.revision == specification.revision &&
        !specification.fingerprint.empty() && result.fingerprint == specification.fingerprint;
}

bool ScaleAnalysisHasCompletedResult(const ScaleAnalysisResult &result)
{
    return !result.fingerprint.empty() &&
        (!result.summary.empty() || !result.items.empty() ||
         !result.scaleSummary.numberOfItems.empty() ||
         !result.reliabilityByImputation.empty() ||
         !result.correlations.values.empty() ||
         !result.dimensionality.status.empty() ||
         !result.scores.status.empty() || !result.plotSeries.empty());
}

bool ScaleAnalysisResultMayBePresented(const ScaleAnalysisState &state)
{
    return ScaleAnalysisResultMatchesSpecification(state.specification, state.result) ||
        (!state.autoFit && ScaleAnalysisHasCompletedResult(state.result));
}

ScaleAnalysisDerivedIdentity BuildScaleAnalysisDerivedIdentity(
    const ScaleAnalysisState &state,
    ScaleAnalysisChildKind kind)
{
    ScaleAnalysisDerivedIdentity identity;
    identity.analysisId = state.id;
    identity.kind = kind;
    identity.sourceRevision = state.specification.revision;
    identity.sourceFingerprint = state.specification.fingerprint;
    return identity;
}

bool ScaleAnalysisDerivedIdentityMatchesState(
    const ScaleAnalysisDerivedIdentity &identity,
    const ScaleAnalysisState &state)
{
    return identity.analysisId == state.id &&
        identity.sourceRevision == state.specification.revision &&
        !identity.sourceFingerprint.empty() &&
        identity.sourceFingerprint == state.specification.fingerprint &&
        ScaleAnalysisResultMatchesSpecification(state.specification, state.result);
}

std::vector<ScaleFactorCountResultRow> ScaleFactorCountRows(
    const ScaleAnalysisResult &result)
{
    std::map<int, int> counts;
    int total = 0;
    for (const ScalePlotSeriesResult &series : result.plotSeries) {
        if (series.kind != "factor_stability") continue;
        const std::size_t n = std::min(series.labels.size(), series.values.size());
        for (std::size_t i = 0; i < n; ++i) {
            const double factorsValue = ParseResultNumber(series.labels[i]);
            const double frequencyValue = ParseResultNumber(series.values[i]);
            if (!std::isfinite(factorsValue) || factorsValue < 0.0 ||
                !std::isfinite(frequencyValue) || frequencyValue < 0.0) continue;
            const int factors = static_cast<int>(std::lround(factorsValue));
            const int frequency = static_cast<int>(std::lround(frequencyValue));
            counts[factors] += frequency;
            total += frequency;
        }
    }
    std::vector<ScaleFactorCountResultRow> rows;
    for (const auto &entry : counts) {
        ScaleFactorCountResultRow row;
        row.factorCount = entry.first;
        row.imputations = entry.second;
        row.proportion = total > 0 ? static_cast<double>(entry.second) / total : 0.0;
        rows.push_back(row);
    }
    return rows;
}

std::vector<ScaleEigenvalueResultRow> ScaleMeanEigenvalueRows(
    const ScaleAnalysisResult &result)
{
    const ScalePlotSeriesResult *observed = nullptr;
    const ScalePlotSeriesResult *reference = nullptr;
    for (const ScalePlotSeriesResult &series : result.plotSeries) {
        if (series.kind == "scree_observed" && !observed) observed = &series;
        if (series.kind == "scree_reference" && !reference) reference = &series;
    }
    if (!observed) return {};
    std::vector<ScaleEigenvalueResultRow> rows;
    for (std::size_t i = 0; i < observed->values.size(); ++i) {
        ScaleEigenvalueResultRow row;
        row.component = i < observed->labels.size()
            ? observed->labels[i] : std::to_string(i + 1U);
        row.observed = observed->values[i];
        row.reference = reference && i < reference->values.size()
            ? reference->values[i] : "";
        rows.push_back(std::move(row));
    }
    return rows;
}

ScaleDimensionalityPresentation BuildScaleDimensionalityPresentation(
    const ScaleAnalysisResult &result,
    int requestedImputation)
{
    ScaleDimensionalityPresentation presentation;
    presentation.imputation = std::max(1, requestedImputation);
    if (result.imputationCount > 0)
        presentation.imputation = std::min(presentation.imputation, result.imputationCount);

    const std::string imputationName =
        "Imputation " + std::to_string(presentation.imputation);
    const std::string imputationToken = std::to_string(presentation.imputation);
    const std::string factorPrefix = imputationToken + "|";
    bool hasMetadata = false;
    const ScalePlotSeriesResult *observed = nullptr;
    const ScalePlotSeriesResult *parallel = nullptr;
    const ScalePlotSeriesResult *variance = nullptr;
    const ScalePlotSeriesResult *cumulative = nullptr;
    const ScalePlotSeriesResult *communality = nullptr;
    const ScalePlotSeriesResult *uniqueness = nullptr;
    std::vector<const ScalePlotSeriesResult *> loadingSeries;
    std::vector<const ScalePlotSeriesResult *> scoreSeries;
    for (const ScalePlotSeriesResult &series : result.plotSeries) {
        if (series.kind == "dimensionality_metadata" && series.name == imputationToken) hasMetadata = true;
        if (series.kind == "scree_observed_imputation" &&
            series.name == imputationName) observed = &series;
        else if (series.kind == "scree_reference_imputation" &&
                 series.name == imputationName) parallel = &series;
        else if (series.kind == "component_variance_imputation" && series.name == imputationToken) variance = &series;
        else if (series.kind == "component_cumulative_imputation" && series.name == imputationToken) cumulative = &series;
        else if (series.kind == "factor_communality_imputation" &&
                 series.name == imputationToken) communality = &series;
        else if (series.kind == "factor_uniqueness_imputation" &&
                 series.name == imputationToken) uniqueness = &series;
        else if (series.kind == "factor_loading_imputation" &&
                 series.name.rfind(factorPrefix, 0) == 0) loadingSeries.push_back(&series);
        else if (series.kind == "factor_score_imputation" &&
                 series.name.rfind(factorPrefix, 0) == 0) scoreSeries.push_back(&series);
    }
    // Ordinary data and older saved analyses expose only aggregate scree
    // series.  They remain valid fallbacks, but MI never silently substitutes
    // the across-imputation mean for a requested completed dataset.
    if (!observed && result.imputationCount <= 1) {
        for (const ScalePlotSeriesResult &series : result.plotSeries) {
            if (series.kind == "scree_observed" && !observed) observed = &series;
            if (series.kind == "scree_reference" && !parallel) parallel = &series;
        }
    }
    if (observed) {
        for (std::size_t index = 0; index < observed->values.size(); ++index) {
            DimensionalityFitComponent component;
            component.index = static_cast<int>(index + 1U);
            component.eigenvalue = ParseResultNumber(observed->values[index]);
            if (parallel && index < parallel->values.size())
                component.parallelEigenvalue = ParseResultNumber(parallel->values[index]);
            component.variance = variance && index < variance->values.size()
                ? ParseResultNumber(variance->values[index]) : std::numeric_limits<double>::quiet_NaN();
            component.cumulative = cumulative && index < cumulative->values.size()
                ? ParseResultNumber(cumulative->values[index]) : std::numeric_limits<double>::quiet_NaN();
            presentation.components.push_back(std::move(component));
        }
    }

    if (!loadingSeries.empty()) {
        const auto &labels = loadingSeries.front()->labels;
        for (std::size_t rowIndex = 0; rowIndex < labels.size(); ++rowIndex) {
            DimensionalityFitLoading loading;
            loading.variable = labels[rowIndex];
            for (const ScalePlotSeriesResult *series : loadingSeries)
                loading.values.push_back(rowIndex < series->values.size()
                    ? ParseResultNumber(series->values[rowIndex])
                    : std::numeric_limits<double>::quiet_NaN());
            if (communality && rowIndex < communality->values.size())
                loading.communality = ParseResultNumber(communality->values[rowIndex]);
            if (uniqueness && rowIndex < uniqueness->values.size())
                loading.uniqueness = ParseResultNumber(uniqueness->values[rowIndex]);
            presentation.variables.push_back(loading.variable);
            presentation.loadings.push_back(std::move(loading));
        }
    } else if (presentation.imputation == 1 && !hasMetadata) {
        for (const ScaleLoadingResultRow &source : result.dimensionality.loadings) {
            DimensionalityFitLoading loading;
            loading.variable = source.item;
            for (const std::string &value : source.loadings)
                loading.values.push_back(ParseResultNumber(value));
            loading.communality = ParseResultNumber(source.communality);
            loading.uniqueness = ParseResultNumber(source.uniqueness);
            presentation.variables.push_back(loading.variable);
            presentation.loadings.push_back(std::move(loading));
        }
    }

    if (!scoreSeries.empty()) {
        const auto &labels = scoreSeries.front()->labels;
        for (std::size_t rowIndex = 0; rowIndex < labels.size(); ++rowIndex) {
            DimensionalityFitScore score;
            score.row = ParseResultInteger(labels[rowIndex]);
            for (const ScalePlotSeriesResult *series : scoreSeries)
                score.values.push_back(rowIndex < series->values.size()
                    ? ParseResultNumber(series->values[rowIndex])
                    : std::numeric_limits<double>::quiet_NaN());
            if (score.row > 0) presentation.scores.push_back(std::move(score));
        }
    }
    if (presentation.variables.empty()) presentation.variables = result.correlations.variables;
    presentation.status = result.dimensionality.status;
    for (const auto &series : result.plotSeries) {
        if (series.kind == "dimensionality_metadata" && series.name == imputationToken && series.labels.size() >= 4)
            presentation.status = series.labels[3] == "value" ? "" : series.labels[3];
    }
    if (result.imputationCount > 1) {
        presentation.status += (presentation.status.empty() ? "" : " ");
        presentation.status += "Results for Imputation " +
            std::to_string(presentation.imputation) +
            "; rotated solutions are not averaged or Rubin-pooled.";
    }
    return presentation;
}

DimensionalityScreePlotState BuildScaleDimensionalityScreePlotState(
    const ScaleAnalysisResult &result,
    int requestedImputation,
    const std::string &method)
{
    const auto presentation = BuildScaleDimensionalityPresentation(
        result, requestedImputation);
    const bool hasMetadata = std::any_of(result.plotSeries.begin(), result.plotSeries.end(),
        [&](const ScalePlotSeriesResult &s) { return s.kind == "dimensionality_metadata" &&
            s.name == std::to_string(presentation.imputation); });
    int retained = hasMetadata ? 0 : ParseResultInteger(result.dimensionality.factorCount);
    if (!presentation.loadings.empty())
        retained = static_cast<int>(presentation.loadings.front().values.size());
    return BuildDimensionalityScreePlotState(
        presentation.components, Lower(method) == "pca" ? "pca" : "factor",
        retained, "none");
}

DimensionalityBiplotPlotState BuildScaleDimensionalityBiplotPlotState(
    const ScaleAnalysisResult &result,
    int requestedImputation,
    int requestedXComponent,
    int requestedYComponent,
    const std::string &method)
{
    const auto presentation = BuildScaleDimensionalityPresentation(
        result, requestedImputation);
    DimensionalityBiplotPlotState state = BuildDimensionalityBiplotPlotState(
        presentation.components, presentation.loadings, presentation.scores,
        Lower(method) == "pca" ? "pca" : "factor",
        requestedXComponent, requestedYComponent);
    if (state.ok && result.imputationCount > 1)
        state.title += " - Imputation " + std::to_string(presentation.imputation);
    return state;
}

std::vector<PublicationTableSpec> ScaleAnalysisPublicationTables(
    const ScaleAnalysisState &state, int kind, int imputation,
    ScaleAnalysisGlobalPresentation globalPresentation,
    ScaleAnalysisDimensionalityPresentation dimensionalityPresentation)
{
    std::vector<PublicationTableSpec> tables;
    auto text = [](const std::string &value) { PublicationValue cell;
        cell.kind = PublicationValueKind::Text; cell.text = value.empty() ? "—" : value; return cell; };
    auto table = [&](const std::string &title, const std::vector<std::string> &headers) {
        PublicationTableSpec t; t.title = title; t.stubColumns = {0};
        for (std::size_t i = 0; i < headers.size(); ++i)
            t.columns.push_back({std::to_string(i), headers[i], "", -1, false});
        if (state.result.imputationCount > 1 && tables.empty() &&
            kind != static_cast<int>(ScaleAnalysisChildKind::Dimensionality))
            t.footnotes.push_back("Multiple imputation: m = " + std::to_string(state.result.imputationCount) +
                ". Summaries shown as mean (range) describe imputations; reliability and distribution summaries are not Rubin-pooled.");
        return t;
    };
    auto row = [&](PublicationTableSpec &t, const std::vector<std::string> &values) {
        std::vector<PublicationValue> cells; for (const auto &v : values) cells.push_back(text(v));
        t.rows.push_back(std::move(cells));
    };
    if ((kind < 0 && globalPresentation == ScaleAnalysisGlobalPresentation::CompactTable) ||
        kind == static_cast<int>(ScaleAnalysisChildKind::Reliability)) {
        const auto fields = ScaleAnalysisSummaryFields(state.result);
        if (kind < 0) {
            auto t = table("Scale-level results", {"Sample", "Value", "Scale", "Value", "Reliability", "Value"});
            t.stubColumns = {0, 2, 4};
            row(t, {"Items", fields[0].second, "M", fields[3].second, "Alpha", fields[6].second});
            row(t, {"N used", fields[1].second, "SD", fields[4].second, "Standardized alpha", fields[7].second});
            row(t, {"Missing (%)", fields[2].second, "Mean inter-item r", fields[5].second, "Omega total", fields[8].second});
            tables.push_back(std::move(t));
        } else {
            auto t = table("Scale reliability", {"Statistic", "Value"});
            for (std::size_t i = 5; i < fields.size(); ++i)
                row(t, {fields[i].first, fields[i].second});
            tables.push_back(std::move(t));
        }
    }
    if (kind < 0) {
        auto t = table("Item-level results", {"Item", "M", "SD", "Missing (%)", "Item-rest r", "Alpha if deleted"});
        std::vector<std::string> reverseScoredItems;
        for (const auto &r : state.result.items)
        {
            row(t, {r.variable, r.mean, r.sd, r.missingPercent, r.itemRestCorrelation, r.alphaIfDeleted});
            if (r.direction == "reversed") reverseScoredItems.push_back(r.variable);
        }
        if (globalPresentation == ScaleAnalysisGlobalPresentation::ItemTableNote) {
            const auto fields = ScaleAnalysisSummaryFields(state.result);
            t.footnotes.push_back("Scale-level results: " + fields[0].second + " items; N = " +
                fields[1].second + "; missing in original = " + fields[2].second +
                "%; M = " + fields[3].second + "; SD = " + fields[4].second +
                "; mean inter-item r = " + fields[5].second + "; alpha = " +
                fields[6].second + "; standardized alpha = " + fields[7].second +
                "; omega total = " + fields[8].second + ".");
        }
        if (!reverseScoredItems.empty()) {
            std::string note = "Reverse-scored items: ";
            for (std::size_t i = 0; i < reverseScoredItems.size(); ++i) {
                if (i) note += ", ";
                note += reverseScoredItems[i];
            }
            t.footnotes.push_back(note + ". Item statistics are based on the scored values.");
        }
        t.footnotes.push_back("Item-rest r excludes the item from the remaining-item total. Alpha if deleted is recalculated after removing the item.");
        tables.push_back(std::move(t));
    } else if (kind == static_cast<int>(ScaleAnalysisChildKind::Reliability) && !state.result.reliabilityByImputation.empty()) {
        auto t = table("Reliability by imputation", {"Imputation", "Alpha", "Standardized alpha", "Omega total"});
        for (const auto &r : state.result.reliabilityByImputation) row(t, {r.imputation, r.alpha, r.standardizedAlpha, r.omegaTotal});
        tables.push_back(std::move(t));
    } else if (kind == static_cast<int>(ScaleAnalysisChildKind::ScaleScores)) {
        const auto &s = state.result.scores;
        auto t = table("Scale scores", {"Method", "Valid N", "M", "SD", "Minimum", "Maximum"});
        row(t, {s.method, s.validN, s.mean, s.sd, s.minimum, s.maximum});
        if (state.result.imputationCount > 1) t.footnotes.push_back("N is per imputation. Other descriptives summarize stacked scores across imputations.");
        tables.push_back(std::move(t));
    } else if (kind == static_cast<int>(ScaleAnalysisChildKind::InterItemCorrelations)) {
        const auto &c = state.result.correlations;
        std::vector<std::string> labels = {"Item"}; labels.insert(labels.end(), c.variables.begin(), c.variables.end());
        auto t = table("", labels);
        for (std::size_t i = 0; i < c.variables.size(); ++i) {
            std::vector<std::string> cells = {c.variables[i]};
            for (std::size_t j = 0; j < c.variables.size(); ++j) {
                auto index = i * c.variables.size() + j;
                cells.push_back(index < c.values.size() ? c.values[index] : "");
            }
            row(t, cells);
        }
        t.footnotes.push_back("Correlation method: " + c.basis + ".");
        if (state.result.imputationCount > 1) t.footnotes.push_back(c.basis == "pearson"
            ? "Pearson correlations are pooled on Fisher's z scale using Rubin's rules."
            : "Correlations are descriptive means across imputations, not Rubin-pooled.");
        tables.push_back(std::move(t));
    } else if (kind == static_cast<int>(ScaleAnalysisChildKind::Dimensionality)) {
        const auto p = BuildScaleDimensionalityPresentation(state.result, imputation);
        const bool pca = Lower(state.specification.dimensionalityMethod) == "pca";
        const auto metadata = BuildScaleDimensionalityReportViewModel(state.result, 0, "", imputation,
            state.specification.dimensionalityMethod).report;
        const int retained = p.loadings.empty() ? 0 :
            static_cast<int>(p.loadings.front().values.size());
        auto addNotes = [&](PublicationTableSpec &t, bool includeParallel, bool includeDefinitions) {
            if (!metadata.summary.empty()) {
                std::string sample = metadata.summary;
                const auto replace = [&](const std::string &from, const std::string &to) {
                    const auto position = sample.find(from);
                    if (position != std::string::npos) sample.replace(position, from.size(), to);
                };
                replace("complete item rows", "complete cases");
                replace("incomplete rows", "incomplete cases");
                replace("Matrix N (minimum pairwise) =", "Minimum pairwise sample size =");
                replace("Matrix N =", "Analysis sample size =");
                t.footnotes.push_back(sample);
            }
            if (!metadata.calculationMethod.empty()) {
                std::string method = pca ? "Principal components analysis" : "Factor analysis";
                if (!pca) {
                    const auto extraction = Lower(state.specification.extraction);
                    method += " with " + (extraction == "minres" ? std::string("minimum-residual") :
                        extraction == "ml" ? std::string("maximum-likelihood") :
                        extraction == "pa" ? std::string("principal-axis") : extraction) + " extraction";
                }
                const auto rotation = Lower(state.specification.rotation);
                if (!rotation.empty() && rotation != "none") method += " and " + rotation + " rotation";
                const auto matrix = metadata.calculationMethod.find("covariance matrix") != std::string::npos
                    ? "covariance" : "correlation";
                const auto basis = Lower(state.result.correlations.basis);
                method += " used a " + (basis.empty() ? std::string("") : basis + " ") + matrix +
                    " matrix based on " + (Lower(state.specification.dimensionalityMissingMode) == "pairwise"
                        ? "pairwise available cases." : "complete cases.");
                t.footnotes.push_back(method);
            }
            if (includeParallel && !metadata.calculationImputation.empty()) {
                if (metadata.calculationImputation.find("Parallel analysis: psych::fa.parallel") == 0)
                    t.footnotes.push_back("The parallel-analysis reference is the 95th percentile from " +
                        std::to_string(std::max(1, state.specification.parallelIterations)) + " replicates.");
                else t.footnotes.push_back(metadata.calculationImputation);
            }
            if (includeDefinitions && retained > 0)
                t.footnotes.push_back("h² = communality; u² = uniqueness.");
            if (!p.status.empty() && p.status != metadata.calculationImputation)
                t.footnotes.push_back(p.status);
        };
        if (retained > 0) {
            const std::size_t candidateCount = std::max(static_cast<std::size_t>(retained),
                std::min(p.components.size(), static_cast<std::size_t>(8)));
            const auto display = BuildDimensionalityReportViewModel(
                p.variables, p.components, p.loadings, 0, 0,
                pca ? "pca" : "factor", retained, p.status, 0, "",
                candidateCount).report;
            const std::array<std::string, 4> statisticLabels = {
                "Observed eigenvalue", "Parallel reference", "Variance explained", "Cumulative variance"};
            auto addStatistics = [&](PublicationTableSpec &t, bool appendEmptyResultColumns) {
                for (std::size_t statistic = 0; statistic < statisticLabels.size(); ++statistic) {
                    std::vector<std::string> cells = {statisticLabels[statistic]};
                    for (std::size_t i = 0; i < candidateCount; ++i) {
                        if (i >= display.componentRows.size() ||
                            (statistic >= 2 && i >= static_cast<std::size_t>(retained))) {
                            cells.push_back("—");
                            continue;
                        }
                        const auto &component = display.componentRows[i];
                        cells.push_back(statistic == 0 ? component.eigenvalue :
                            statistic == 1 ? component.parallelEigenvalue :
                            statistic == 2 ? component.variance : component.cumulative);
                    }
                    if (appendEmptyResultColumns) cells.insert(cells.end(), {"", ""});
                    row(t, cells);
                }
            };
            const auto candidateLabels = [&]() {
                std::vector<std::string> labels = {"Statistic"};
                for (std::size_t i = 1; i <= candidateCount; ++i)
                    labels.push_back((pca ? "PC" : "F") + std::to_string(i));
                return labels;
            }();
            if (dimensionalityPresentation == ScaleAnalysisDimensionalityPresentation::SeparateTables) {
                auto summary = table("Parallel analysis and variance", candidateLabels);
                addStatistics(summary, false);
                if (p.components.size() > static_cast<std::size_t>(retained))
                    summary.footnotes.push_back("Variance explained is shown for " +
                        std::to_string(retained) + " retained dimensions; eigenvalues and parallel "
                        "references include " + std::to_string(candidateCount) + " of " +
                        std::to_string(p.components.size()) + " candidates.");
                addNotes(summary, true, false);
                tables.push_back(std::move(summary));

                std::vector<std::string> loadingLabels = {"Item"};
                for (int i = 1; i <= retained; ++i)
                    loadingLabels.push_back((pca ? "PC" : "F") + std::to_string(i));
                loadingLabels.push_back("h²"); loadingLabels.push_back("u²");
                auto loadings = table(pca ? "Component loadings" : "Factor loadings", loadingLabels);
                for (const auto &loading : display.loadingRows) {
                    std::vector<std::string> cells = {loading.variable};
                    for (std::size_t i = 0; i < static_cast<std::size_t>(retained); ++i)
                        cells.push_back(i < loading.loadings.size() ? loading.loadings[i].text : "—");
                    cells.push_back(loading.communality);
                    cells.push_back(loading.uniqueness);
                    row(loadings, cells);
                }
                addNotes(loadings, false, true);
                tables.push_back(std::move(loadings));
            } else {
                auto labels = candidateLabels;
                labels[0] = "Item / statistic";
                labels.push_back("h²"); labels.push_back("u²");
                auto t = table(pca ? "Component solution" : "Factor solution", labels);
                addStatistics(t, true);
                std::vector<std::string> heading = {"Item loadings"};
                heading.resize(candidateCount + 3);
                row(t, heading);
                for (const auto &loading : display.loadingRows) {
                    std::vector<std::string> cells = {"  " + loading.variable};
                    for (const auto &value : loading.loadings) cells.push_back(value.text);
                    cells.resize(candidateCount + 1);
                    cells.push_back(loading.communality);
                    cells.push_back(loading.uniqueness);
                    row(t, cells);
                }
                if (p.components.size() > static_cast<std::size_t>(retained))
                    t.footnotes.push_back("Loadings and variance explained are shown for " +
                        std::to_string(retained) + " retained dimensions; eigenvalues and parallel "
                        "references include " + std::to_string(candidateCount) + " of " +
                        std::to_string(p.components.size()) + " candidates.");
                addNotes(t, true, true);
                tables.push_back(std::move(t));
            }
        } else {
            auto t = table("Parallel analysis", {"Dimension", "Eigenvalue", "Parallel reference", "Variance explained", "Cumulative variance"});
            for (const auto &r : metadata.componentRows)
                row(t, {r.label, r.eigenvalue, r.parallelEigenvalue, r.variance, r.cumulative});
            t.footnotes.push_back("No retained solution or item loadings are available for the selected imputation.");
            addNotes(t, true, false);
            tables.push_back(std::move(t));
        }
    }
    return tables;
}

bool BuildScaleNativePlotModel(const ScaleAnalysisState &state, ScaleAnalysisChildKind kind,
    int imputation, PlotModel &model)
{
    if (!ScaleAnalysisResultMayBePresented(state)) return false;
    const auto oldViewport = std::array<double, 4>{model.xmin, model.xmax, model.ymin, model.ymax};
    const auto oldBounds = std::array<double, 4>{model.dataXmin, model.dataXmax, model.dataYmin, model.dataYmax};
    const bool sameImputation = model.diagnosticImputationIndex == imputation;
    const std::string oldX = model.xLabel, oldY = model.yLabel;
    model.id = state.id + ":plot:" + std::to_string(static_cast<int>(kind));
    model.group = state.group;
    model.glmDiagnosticKind = "scale_dimensionality";
    model.diagnosticImputationIndex = imputation;
    model.diagnosticImputationCount = state.result.imputationCount;
    model.title = ScaleAnalysisChildWindowTitle(kind, false);
    model.points.clear(); model.screeParallelPoints.clear(); model.biplotLoadings.clear();
    model.timeSeriesPointGroups.clear(); model.interactionPlotLines.clear();
    if (kind == ScaleAnalysisChildKind::ScreePlot) {
        const auto plot = BuildScaleDimensionalityScreePlotState(state.result, imputation,
            state.specification.dimensionalityMethod);
        if (!plot.ok) return false;
        model.kind = "pca_scree"; model.title = plot.title;
        model.xLabel = plot.xLabel; model.yLabel = plot.yLabel;
        for (const auto &p : plot.observed) model.points.push_back({p.x, p.y, p.rowId});
        for (const auto &p : plot.parallel) model.screeParallelPoints.push_back({p.x, p.y, p.rowId});
        model.xmin = plot.xmin; model.xmax = plot.xmax; model.ymin = plot.ymin; model.ymax = plot.ymax;
    } else if (kind == ScaleAnalysisChildKind::Biplot) {
        const auto plot = BuildScaleDimensionalityBiplotPlotState(state.result, imputation,
            model.biplotXComponent, model.biplotYComponent, state.specification.dimensionalityMethod);
        if (!plot.ok) return false;
        model.kind = "pca_biplot"; model.title = plot.title;
        model.xLabel = plot.xLabel; model.yLabel = plot.yLabel;
        model.biplotXComponent = plot.xComponent; model.biplotYComponent = plot.yComponent;
        for (const auto &p : plot.scores) model.points.push_back({p.x, p.y, p.rowId});
        for (const auto &p : plot.loadings) model.biplotLoadings.push_back({p.variable, p.x, p.y});
        model.xmin = plot.xmin; model.xmax = plot.xmax; model.ymin = plot.ymin; model.ymax = plot.ymax;
    } else if (kind == ScaleAnalysisChildKind::ScaleScoreDistributionPlot) {
        model.kind = "time_series"; model.glmDiagnosticKind = "scale_score_distribution";
        const std::string completedMethod = state.result.scores.method.empty()
            ? state.specification.scoreMethod : state.result.scores.method;
        model.xLabel = completedMethod == "sum" ? "Sum of items" : "Mean of items";
        model.yLabel = "Proportion"; model.timeSeriesGroupVariable = "Imputation";
        double low = std::numeric_limits<double>::infinity(), high = -low, top = 0;
        int seriesIndex = 0, row = 0;
        const int first = std::max(1, imputation);
        for (const auto &series : state.result.plotSeries) {
            if (series.kind != "score_distribution") continue;
            ++seriesIndex;
            if (seriesIndex < first || seriesIndex >= first + 5) continue;
            for (std::size_t i = 0; i < std::min(series.labels.size(), series.values.size()); ++i) {
                char *end = nullptr;
                const double x = std::strtod(series.labels[i].c_str(), &end);
                if (end == series.labels[i].c_str() || !std::isfinite(x)) continue;
                const double y = std::strtod(series.values[i].c_str(), &end);
                if (end == series.values[i].c_str() || !std::isfinite(y)) continue;
                model.points.push_back({x, y, ++row}); model.timeSeriesPointGroups.push_back(series.name);
                low = std::min(low, x); high = std::max(high, x); top = std::max(top, y);
            }
        }
        if (model.points.empty()) return false;
        model.interactionPlotLines = BuildTimeSeriesLines(model.points, model.timeSeriesPointGroups, "Scores");
        const double pad = high > low ? (high - low) * .04 : .5;
        model.xmin = low - pad; model.xmax = high + pad; model.ymin = 0; model.ymax = top > 0 ? top * 1.1 : 1;
        if (state.result.imputationCount > 1) model.title += " — Imputations " + std::to_string(first) +
            "–" + std::to_string(std::min(first + 4, state.result.imputationCount)) + " (not pooled)";
    } else return false;
    if (kind == ScaleAnalysisChildKind::ScreePlot && state.result.imputationCount > 1)
        model.title += " — Imputation " + std::to_string(imputation) + " (not pooled)";
    model.dataXmin = model.xmin; model.dataXmax = model.xmax;
    model.dataYmin = model.ymin; model.dataYmax = model.ymax;
    const auto newBounds = std::array<double, 4>{model.dataXmin, model.dataXmax, model.dataYmin, model.dataYmax};
    if (kind == ScaleAnalysisChildKind::Biplot && sameImputation && oldX == model.xLabel && oldY == model.yLabel && oldBounds == newBounds) {
        model.xmin = oldViewport[0]; model.xmax = oldViewport[1];
        model.ymin = oldViewport[2]; model.ymax = oldViewport[3];
    }
    return true;
}

std::string ScaleAnalysisPlotExplanation(ScaleAnalysisChildKind kind)
{
    if (kind == ScaleAnalysisChildKind::ItemDistributionsPlot)
        return "Each panel describes one scored item. Labels are response categories or histogram intervals; bar lengths and percentages show their relative frequencies among nonmissing responses. Reverse scoring is applied. Missing responses are excluded from the denominator. Under multiple imputation these are descriptive average proportions across completed datasets, not pooled estimates. Compare the shape and concentration within each item; scales and intervals can differ between items.";
    if (kind == ScaleAnalysisChildKind::ScaleScoreDistributionPlot)
        return "The horizontal axis is the selected sum or mean of the scored items. The vertical axis is the proportion of valid scores in each histogram interval; lines join interval midpoints (a frequency polygon, not a fitted density). R computes the scores and histogram frequencies. The minimum-valid-item rule is applied. Under multiple imputation each line describes a completed dataset, not a person or a pooled distribution. Up to five imputations are displayed at once; use Imputation to change the first displayed one.";
    if (kind == ScaleAnalysisChildKind::ScreePlot)
        return "Observed eigenvalues are compared with the simulated reference from R's psych::fa.parallel, using the selected dimensionality method. Dimensions exceeding the parallel reference are candidates for retention; also consider theory and interpretability. Under multiple imputation this shows the selected completed dataset, not a pooled solution.";
    if (kind == ScaleAnalysisChildKind::Biplot)
        return "Points are case scores on the two displayed dimensions; arrows represent item loadings. Arrow direction indicates the association with each axis. Interpret distances in the displayed projection, not as exact distances in the full item space. Under multiple imputation this shows one completed dataset, not pooled scores or loadings.";
    if (kind == ScaleAnalysisChildKind::ItemRestPlot)
        return "Each bar is the correlation between one scored item and the sum of the remaining items, calculated in R. Zero means no linear association; negative bars extend left and warrant checking scoring direction and content. Click an item to inspect or change it. Bars represent items, not individual cases.";
    if (kind == ScaleAnalysisChildKind::ReliabilityIfDeletedPlot)
        return "Each bar is Cronbach's alpha recalculated in R after deleting that item. Compare the printed value with full-scale alpha. Negative values can occur and extend left of zero; very negative values may extend beyond the plotting range, so use the printed value. Do not delete items solely to maximize alpha: consider their content. Bars identify items, not cases.";
    if (kind == ScaleAnalysisChildKind::CorrelationHeatmap)
        return "Rows and columns identify items. Each cell represents their correlation: its color indicates sign and strength. The diagonal is each item's correlation with itself. Pearson correlations under multiple imputation use Fisher-z pooling; other correlation types are descriptive means across imputations. Use the correlation table to open the corresponding pair of variables.";
    if (kind == ScaleAnalysisChildKind::LoadingPlot)
        return "Each bar is an item loading on a factor or component calculated in R. Its sign gives direction and its magnitude indicates association in the fitted solution. Under multiple imputation this displays one completed dataset. Interpret loadings together with the extraction and rotation settings.";
    return "The plot displays the corresponding R-computed scale statistics for the current items.";
}

std::vector<CorrelationCellResult> ScaleCorrelationCells(
    const ScaleAnalysisResult &result)
{
    const auto &correlations = result.correlations;
    const std::size_t count = correlations.variables.size();
    std::vector<CorrelationCellResult> cells;
    cells.reserve(count * count);
    const int n = ParseResultInteger(result.scaleSummary.nUsed);
    for (std::size_t row = 0; row < count; ++row) {
        for (std::size_t column = 0; column < count; ++column) {
            CorrelationCellResult cell;
            cell.xVariable = correlations.variables[column];
            cell.yVariable = correlations.variables[row];
            const std::size_t index = row * count + column;
            if (index < correlations.values.size())
                cell.r = ParseResultNumber(correlations.values[index]);
            if (index < correlations.pValues.size())
                cell.p = ParseResultNumber(correlations.pValues[index]);
            cell.n = index < correlations.sampleSizes.size()
                ? ParseResultInteger(correlations.sampleSizes[index]) : n;
            cell.status = row == column ? "diagonal" :
                (std::isfinite(cell.r) ? "valid" : "unavailable");
            cell.detail = "Correlation basis: " + correlations.basis;
            if (!correlations.status.empty())
                cell.detail += " · " + correlations.status;
            cells.push_back(std::move(cell));
        }
    }
    return cells;
}

CorrelationMatrixRenderPlan BuildScaleCorrelationRenderPlan(
    const ScaleAnalysisResult &result,
    int selectedRow,
    int selectedColumn,
    bool showP,
    bool showPValue,
    bool showN)
{
    CorrelationMatrixRenderPlan plan = BuildCorrelationMatrixRenderPlan(
        result.correlations.variables,
        ScaleCorrelationCells(result),
        selectedRow,
        selectedColumn,
        showP,
        showPValue,
        showN);
    // Item membership is edited in the parent Scale Analysis window.  The
    // derived matrix shares the ordinary matrix presentation and cell
    // interaction, but it must never pretend to own a mutable variable list.
    plan.addVariableLabel.clear();
    plan.addVariableRect = Rect{};
    return plan;
}

DimensionalityReportViewModel BuildScaleDimensionalityReportViewModel(
    const ScaleAnalysisResult &result,
    int focusedComponent,
    const std::string &focusedVariable,
    int requestedImputation,
    const std::string &method)
{
    const auto presentation = BuildScaleDimensionalityPresentation(
        result, requestedImputation);
    int factorCount = 0;
    // In multiple imputation the retained factor count may legitimately vary
    // by completed dataset.  Prefer the exact selected solution instead of
    // the legacy top-level value (which represents only the first valid fit).
    if (!presentation.loadings.empty())
        factorCount = static_cast<int>(presentation.loadings.front().values.size());
    if (factorCount <= 0)
        factorCount = ParseResultInteger(result.dimensionality.factorCount);
    if (factorCount <= 0 && !result.dimensionality.factorNames.empty())
        factorCount = static_cast<int>(result.dimensionality.factorNames.size());
    factorCount = std::max(1, factorCount);
    auto model = BuildDimensionalityReportViewModel(
        presentation.variables,
        presentation.components,
        presentation.loadings,
        static_cast<std::size_t>(ParseResultInteger(result.scaleSummary.nUsed)),
        0,
        Lower(method) == "pca" ? "pca" : "factor",
        factorCount,
        presentation.status,
        focusedComponent,
        focusedVariable);
    // A current R result with no extracted dimensions must not inherit a
    // placeholder F1/PC1 column or another imputation's loading count.
    const bool hasMetadata = std::any_of(result.plotSeries.begin(), result.plotSeries.end(),
        [&](const ScalePlotSeriesResult &s) { return s.kind == "dimensionality_metadata" &&
            s.name == std::to_string(presentation.imputation); });
    if (hasMetadata && presentation.loadings.empty()) {
        model.report.componentCount = 0;
        model.report.loadingHeaders.clear();
        model.report.loadingRows.clear();
        model.report.hasLoadings = false;
    }
    // Scale Analysis uses psych and transports the actual R decisions/counts.
    // Do not inherit factanal/prcomp annotations from the ordinary workflow.
    model.report.summary.clear();
    model.report.calculationMethod = "Calculated in R with psych.";
    model.report.calculationImputation.clear();
    for (const auto &series : result.plotSeries) {
        if (series.kind != "dimensionality_metadata" || series.name != std::to_string(presentation.imputation) || series.labels.size() < 3) continue;
        model.report.summary = series.labels[0];
        model.report.calculationMethod = series.labels[1];
        model.report.calculationImputation = series.labels[2];
    }
    model.layout = BuildDimensionalityReportLayout(model.report);
    model.layout.summaryRect.width = 700;
    model.renderPlan = BuildDimensionalityReportRenderPlan(model.report, model.layout);
    return model;
}

ScaleAnalysisWindowLayout BuildScaleAnalysisWindowLayout(
    std::size_t itemCount,
    std::size_t maximumVisibleItems)
{
    ScaleAnalysisWindowLayout layout;
    layout.preferredWidth = 980.0;
    const std::size_t totalRows = itemCount + 1U;
    const std::size_t maximum = std::max<std::size_t>(3U, maximumVisibleItems);
    layout.visibleItemRows = std::min(totalRows, maximum);
    layout.itemListScrolls = totalRows > maximum;
    layout.preferredHeight = std::min(760.0, 300.0 +
        layout.itemRowHeight * static_cast<double>(layout.visibleItemRows));
    return layout;
}

} // namespace core
} // namespace rlispstat
