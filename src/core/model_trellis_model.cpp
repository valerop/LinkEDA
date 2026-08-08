#include "model_trellis_model.h"

#include "model_terms.h"

#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <iomanip>
#include <sstream>

namespace rlispstat {
namespace core {
namespace {

const DataColumn *FindColumn(const DataFrameModel &data, const std::string &name)
{
    for (const DataColumn &column : data.columns) if (column.name == name) return &column;
    return nullptr;
}

std::string StableLevelId(const std::string &value)
{
    return std::to_string(value.size()) + ":" + value;
}

std::vector<std::string> Levels(const DataColumn *column)
{
    if (!column) return {};
    std::vector<std::string> values = column->definedLevels;
    for (const std::string &value : column->values) {
        if (DataCellIsMissing(value)) continue;
        if (std::find(values.begin(), values.end(), value) == values.end()) values.push_back(value);
    }
    return values;
}

bool ScopeContainsRow(const std::string &scope, int row, const std::set<int> &selection)
{
    if (scope == "selected") return selection.count(row) != 0;
    if (scope == "unselected") return selection.count(row) == 0;
    return true;
}

std::string Number(double value)
{
    if (!std::isfinite(value)) return "";
    std::ostringstream out;
    out << std::setprecision(17) << value;
    return out.str();
}

std::string WarningText(const ModelTrellisPanelResult &panel)
{
    std::ostringstream out;
    for (std::size_t i = 0; i < panel.warnings.size(); ++i) {
        if (i) out << "; ";
        out << panel.warnings[i];
    }
    return out.str();
}

const ModelTrellisCoefficientResult *DisplayedCoefficient(const ModelTrellisState &state,
                                                          const ModelTrellisPanelResult &panel)
{
    const std::string requested = state.specification.displayedCoefficientId.value_or("");
    for (const auto &row : panel.coefficients) if (!requested.empty() && row.coefficientId == requested) return &row;
    const std::string term = state.specification.displayedTermId.value_or("");
    for (const auto &row : panel.coefficients) if (row.sourceTermId == term && row.estimable) return &row;
    return nullptr;
}

const ModelTrellisTermTestResult *DisplayedTermTest(const ModelTrellisState &state,
                                                    const ModelTrellisPanelResult &panel)
{
    const std::string term = state.specification.displayedTermId.value_or("");
    for (const auto &test : panel.termTests) if (test.termId == term) return &test;
    return nullptr;
}

} // namespace

std::string ModelTrellisPanelContentId(ModelTrellisPanelContent value)
{
    switch (value) {
    case ModelTrellisPanelContent::CompactSummary: return "compact";
    case ModelTrellisPanelContent::SelectedResult: return "selected";
    case ModelTrellisPanelContent::Coefficients: return "coefficients";
    case ModelTrellisPanelContent::TermTests: return "term_tests";
    case ModelTrellisPanelContent::ModelFit: return "model_fit";
    }
    return "selected";
}

std::string ModelTrellisPAdjustmentId(ModelTrellisPAdjustment value)
{
    switch (value) {
    case ModelTrellisPAdjustment::None: return "none";
    case ModelTrellisPAdjustment::Holm: return "holm";
    case ModelTrellisPAdjustment::Bonferroni: return "bonferroni";
    }
    return "holm";
}

std::string ModelTrellisPAdjustmentLabel(ModelTrellisPAdjustment value)
{
    switch (value) {
    case ModelTrellisPAdjustment::None: return "None";
    case ModelTrellisPAdjustment::Holm: return "Holm";
    case ModelTrellisPAdjustment::Bonferroni: return "Bonferroni";
    }
    return "Holm";
}

std::string ModelTrellisFormulaText(const GeneralLinearModelSpecification &specification,
                                    const std::vector<std::string> *terms)
{
    const std::vector<std::string> &actual = terms ? *terms : specification.terms;
    std::ostringstream out;
    out << specification.response << " ~ ";
    if (specification.includeIntercept && actual.empty()) return out.str() + "1";
    if (!specification.includeIntercept) out << "0" << (actual.empty() ? "" : " + ");
    for (std::size_t i = 0; i < actual.size(); ++i) {
        if (i) out << " + ";
        out << actual[i];
    }
    return out.str();
}

std::string ModelTrellisMethodologicalNote()
{
    return "Models are fitted separately within each panel. Differences in estimates or p values do not constitute a formal test of differences between panels.";
}

std::string ModelTrellisCopyNote()
{
    return "All panels use the same model specification and are refitted together when predictors change.";
}

std::vector<std::string> ModelTrellisCategoricalVariables(
    const DataFrameModel &data,
    const ModelTrellisSpecification &specification)
{
    std::vector<std::string> out;
    for (const DataColumn &column : data.columns) {
        if (column.name == specification.baseModel.response || !VariableTypeIsFactorLike(column.type)) continue;
        if (Levels(&column).size() >= 2) out.push_back(column.name);
    }
    return out;
}

std::vector<std::string> ModelTrellisIndependentVariables(
    const DataFrameModel &data,
    const ModelTrellisSpecification &specification)
{
    std::set<std::string> unavailable{specification.baseModel.response};
    if (specification.rowConditioningVariable) {
        unavailable.insert(specification.rowConditioningVariable->variableId);
    }
    if (specification.columnConditioningVariable) {
        unavailable.insert(specification.columnConditioningVariable->variableId);
    }
    for (const std::string &term : specification.baseModel.terms) {
        if (!IsInteractionTerm(term)) unavailable.insert(term);
    }
    std::vector<std::string> out;
    for (const DataColumn &column : data.columns) {
        if (unavailable.count(column.name)) continue;
        if (VariableTypeIsFactorLike(column.type) || DataColumnAllowsNumeric(column)) {
            out.push_back(column.name);
        }
    }
    return out;
}

bool AddModelTrellisIndependentVariable(ModelTrellisSpecification &specification,
                                        const std::string &variable,
                                        const DataFrameModel &data,
                                        std::string *error)
{
    if (variable.empty() || variable == specification.baseModel.response) {
        if (error) *error = "Choose a predictor different from the response.";
        return false;
    }
    if ((specification.rowConditioningVariable &&
         specification.rowConditioningVariable->variableId == variable) ||
        (specification.columnConditioningVariable &&
         specification.columnConditioningVariable->variableId == variable)) {
        if (error) *error = "A conditioning variable cannot also be added as a predictor.";
        return false;
    }
    const DataColumn *column = FindColumn(data, variable);
    if (!column || (!VariableTypeIsFactorLike(column->type) && !DataColumnAllowsNumeric(*column))) {
        if (error) *error = "Predictors must be numeric or categorical variables.";
        return false;
    }
    if (std::find(specification.baseModel.terms.begin(),
                  specification.baseModel.terms.end(), variable) !=
        specification.baseModel.terms.end()) {
        if (error) *error = "That predictor is already in the model.";
        return false;
    }
    specification.baseModel.terms.push_back(variable);
    specification.baseModel.termTypes[variable] = VariableTypeIsFactorLike(column->type)
        ? "factor" : "numeric";
    specification.displayedTermId = variable;
    specification.displayedCoefficientId.reset();
    specification.panelContent = ModelTrellisPanelContent::SelectedResult;
    return true;
}

bool RemoveModelTrellisTerm(ModelTrellisSpecification &specification,
                            const std::string &term)
{
    auto found = std::find(specification.baseModel.terms.begin(),
                           specification.baseModel.terms.end(), term);
    if (found == specification.baseModel.terms.end()) return false;
    specification.baseModel.terms.erase(found);
    specification.baseModel.termTypes.erase(term);
    if (specification.displayedTermId && *specification.displayedTermId == term) {
        specification.displayedTermId.reset();
        specification.displayedCoefficientId.reset();
        specification.panelContent = specification.baseModel.terms.empty()
            ? ModelTrellisPanelContent::CompactSummary
            : ModelTrellisPanelContent::SelectedResult;
        if (!specification.baseModel.terms.empty()) {
            specification.displayedTermId = specification.baseModel.terms.front();
        }
    }
    return true;
}

bool SetModelTrellisConditioningVariable(ModelTrellisSpecification &specification,
                                         ModelTrellisDimension dimension,
                                         const std::optional<std::string> &variable,
                                         const DataFrameModel &data,
                                         std::string *error)
{
    if (variable) {
        if (*variable == specification.baseModel.response) {
            if (error) *error = "The response cannot be used as a conditioning variable.";
            return false;
        }
        const DataColumn *column = FindColumn(data, *variable);
        if (!column || !VariableTypeIsFactorLike(column->type) || Levels(column).size() < 2) {
            if (error) *error = "Conditioning variables must be categorical and have at least two effective levels.";
            return false;
        }
        const auto &other = dimension == ModelTrellisDimension::Rows
            ? specification.columnConditioningVariable : specification.rowConditioningVariable;
        if (other && other->variableId == *variable) {
            if (error) *error = "The same variable cannot condition both rows and columns.";
            return false;
        }
    }
    auto &slot = dimension == ModelTrellisDimension::Rows
        ? specification.rowConditioningVariable : specification.columnConditioningVariable;
    if (variable) slot = ModelTrellisConditioningVariable{*variable, dimension};
    else slot.reset();
    return true;
}

void SwapModelTrellisDimensions(ModelTrellisSpecification &specification)
{
    std::swap(specification.rowConditioningVariable, specification.columnConditioningVariable);
    if (specification.rowConditioningVariable) specification.rowConditioningVariable->dimension = ModelTrellisDimension::Rows;
    if (specification.columnConditioningVariable) specification.columnConditioningVariable->dimension = ModelTrellisDimension::Columns;
}

void ApplyModelTrellisPAdjustment(ModelTrellisState &state)
{
    for (auto &panel : state.panels) {
        for (auto &row : panel.coefficients) {
            row.adjustedPValue = state.specification.pAdjustment == ModelTrellisPAdjustment::None
                ? row.pValue : (state.specification.pAdjustment == ModelTrellisPAdjustment::Holm
                    ? row.holmPValue : row.bonferroniPValue);
        }
        for (auto &test : panel.termTests) {
            test.adjustedPValue = state.specification.pAdjustment == ModelTrellisPAdjustment::None
                ? test.pValue : (state.specification.pAdjustment == ModelTrellisPAdjustment::Holm
                    ? test.holmPValue : test.bonferroniPValue);
        }
        panel.adjustedGlobalPValue = state.specification.pAdjustment == ModelTrellisPAdjustment::None
            ? panel.modelResult.globalP : (state.specification.pAdjustment == ModelTrellisPAdjustment::Holm
                ? panel.holmGlobalPValue : panel.bonferroniGlobalPValue);
    }
}

std::vector<std::string> EffectiveModelTrellisTerms(
    const ModelTrellisSpecification &specification,
    std::vector<std::string> *omitted)
{
    std::set<std::string> conditioned;
    if (specification.rowConditioningVariable) conditioned.insert(specification.rowConditioningVariable->variableId);
    if (specification.columnConditioningVariable) conditioned.insert(specification.columnConditioningVariable->variableId);
    std::vector<std::string> kept;
    if (omitted) omitted->clear();
    for (const std::string &term : specification.baseModel.terms) {
        bool remove = false;
        for (const std::string &variable : UniqueBaseVariablesForTerm(term)) {
            if (conditioned.count(variable)) { remove = true; break; }
        }
        if (remove) { if (omitted) omitted->push_back(term); }
        else kept.push_back(term);
    }
    return kept;
}

std::vector<ModelTrellisPanelResult> BuildModelTrellisPanels(
    const ModelTrellisSpecification &specification,
    const DataFrameModel &data,
    const std::set<int> &selection)
{
    const DataColumn *rowColumn = specification.rowConditioningVariable
        ? FindColumn(data, specification.rowConditioningVariable->variableId) : nullptr;
    const DataColumn *columnColumn = specification.columnConditioningVariable
        ? FindColumn(data, specification.columnConditioningVariable->variableId) : nullptr;
    std::vector<std::string> rowLevels = rowColumn ? Levels(rowColumn) : std::vector<std::string>{""};
    std::vector<std::string> columnLevels = columnColumn ? Levels(columnColumn) : std::vector<std::string>{""};
    std::vector<std::string> omitted;
    const std::vector<std::string> effective = EffectiveModelTrellisTerms(specification, &omitted);
    std::vector<ModelTrellisPanelResult> out;
    for (const std::string &rowLevel : rowLevels) for (const std::string &columnLevel : columnLevels) {
        ModelTrellisPanelResult panel;
        if (rowColumn) {
            panel.key.rowLevelId = StableLevelId(rowLevel);
            panel.rowLevelLabel = rowLevel;
        }
        if (columnColumn) {
            panel.key.columnLevelId = StableLevelId(columnLevel);
            panel.columnLevelLabel = columnLevel;
        }
        panel.panelId = "r=" + (panel.key.rowLevelId ? *panel.key.rowLevelId : std::string("none")) +
                        "|c=" + (panel.key.columnLevelId ? *panel.key.columnLevelId : std::string("none"));
        panel.effectiveTerms = effective;
        panel.omittedTerms = omitted;
        for (int index = 0; index < data.rows; ++index) {
            if (rowColumn && (index >= (int)rowColumn->values.size() || rowColumn->values[index] != rowLevel)) continue;
            if (columnColumn && (index >= (int)columnColumn->values.size() || columnColumn->values[index] != columnLevel)) continue;
            const int rowId = index + 1;
            if (!ScopeContainsRow(specification.baseModel.scope, rowId, selection)) continue;
            panel.candidateOriginalRows.push_back(rowId);
        }
        panel.hasObservations = !panel.candidateOriginalRows.empty();
        if (!panel.hasObservations) panel.warnings.push_back("No observations");
        if (!omitted.empty()) panel.warnings.push_back("Conditioning variables are constant within panels and are omitted from the panel model formula.");
        out.push_back(std::move(panel));
    }
    return out;
}

ModelTrellisLayout BuildModelTrellisLayout(const ModelTrellisState &state)
{
    ModelTrellisLayout layout;
    std::vector<std::string> rowIds, columnIds;
    for (const auto &panel : state.panels) {
        const std::string row = panel.key.rowLevelId.value_or("none");
        const std::string col = panel.key.columnLevelId.value_or("none");
        if (std::find(rowIds.begin(), rowIds.end(), row) == rowIds.end()) rowIds.push_back(row);
        if (std::find(columnIds.begin(), columnIds.end(), col) == columnIds.end()) columnIds.push_back(col);
    }
    layout.rows = std::max<std::size_t>(1, rowIds.size());
    layout.columns = std::max<std::size_t>(1, columnIds.size());
    if (!state.specification.rowConditioningVariable && state.specification.columnConditioningVariable &&
        state.specification.arrangement == ModelTrellisPanelArrangement::OneColumn) {
        layout.rows = columnIds.size(); layout.columns = 1;
    } else if (state.specification.rowConditioningVariable && !state.specification.columnConditioningVariable &&
               state.specification.arrangement == ModelTrellisPanelArrangement::OneRow) {
        layout.rows = 1; layout.columns = rowIds.size();
    }
    for (std::size_t i = 0; i < state.panels.size(); ++i) {
        const auto &panel = state.panels[i];
        std::size_t row = std::distance(rowIds.begin(), std::find(rowIds.begin(), rowIds.end(), panel.key.rowLevelId.value_or("none")));
        std::size_t col = std::distance(columnIds.begin(), std::find(columnIds.begin(), columnIds.end(), panel.key.columnLevelId.value_or("none")));
        if (!state.specification.rowConditioningVariable && state.specification.columnConditioningVariable &&
            state.specification.arrangement == ModelTrellisPanelArrangement::OneColumn) { row = col; col = 0; }
        if (state.specification.rowConditioningVariable && !state.specification.columnConditioningVariable &&
            state.specification.arrangement == ModelTrellisPanelArrangement::OneRow) { col = row; row = 0; }
        layout.panels.push_back({panel.panelId, row, col});
    }
    return layout;
}

std::string ModelTrellisSelectedResultCSV(const ModelTrellisState &state)
{
    std::ostringstream out;
    out << "Row_Condition,Column_Condition,N,Displayed_Result,Estimate,SE,Statistic,df1,df2,p,p_adjusted,CI_Lower,CI_Upper,R2,Adjusted_R2,Estimable,Warning\n";
    for (const auto &panel : state.panels) {
        const auto *coef = DisplayedCoefficient(state, panel);
        const auto *test = DisplayedTermTest(state, panel);
        const std::string displayedTerm = state.specification.displayedTermId.value_or("");
        auto displayedType = state.specification.baseModel.termTypes.find(displayedTerm);
        const bool useTest = test && (!coef || test->df1 > 1.0 || IsInteractionTerm(displayedTerm) ||
            (displayedType != state.specification.baseModel.termTypes.end() && displayedType->second == "factor"));
        out << CsvEscape(panel.rowLevelLabel) << ',' << CsvEscape(panel.columnLevelLabel) << ',' << panel.modelResult.n << ',';
        out << CsvEscape(useTest ? test->label : (coef ? coef->label : state.specification.displayedTermId.value_or("Model fit"))) << ',';
        out << (useTest ? "" : (coef ? Number(coef->estimate) : "")) << ',';
        out << (useTest ? "" : (coef ? Number(coef->standardError) : "")) << ',';
        out << (useTest ? Number(test->statistic) : (coef ? Number(coef->statistic) : Number(panel.modelResult.globalF))) << ',';
        out << (useTest ? Number(test->df1) : (coef ? "1" : Number(panel.modelResult.dfModel))) << ',';
        out << (useTest ? Number(test->df2) : Number(panel.modelResult.dfResidual)) << ',';
        out << (useTest ? Number(test->pValue) : (coef ? Number(coef->pValue) : Number(panel.modelResult.globalP))) << ',';
        out << (useTest ? Number(test->adjustedPValue) : (coef ? Number(coef->adjustedPValue) : "")) << ',';
        out << (useTest ? "" : (coef ? Number(coef->confidenceLower) : "")) << ',';
        out << (useTest ? "" : (coef ? Number(coef->confidenceUpper) : "")) << ',';
        out << Number(panel.modelResult.r2) << ',' << Number(panel.modelResult.adjR2) << ',';
        out << (panel.estimable ? "TRUE" : "FALSE") << ',' << CsvEscape(WarningText(panel)) << "\n";
    }
    return out.str();
}

std::string ModelTrellisAllResultsCSV(const ModelTrellisState &state)
{
    std::ostringstream out;
    out << "Row_Condition,Column_Condition,Panel_ID,Result_Type,Term_ID,Term_Label,Coefficient_ID,Estimate,SE,Statistic,df1,df2,p,p_adjusted,CI_Lower,CI_Upper,N,R2,Adjusted_R2,Estimable,Warning\n";
    for (const auto &panel : state.panels) {
        for (const auto &row : panel.coefficients) {
            out << CsvEscape(panel.rowLevelLabel) << ',' << CsvEscape(panel.columnLevelLabel) << ',' << CsvEscape(panel.panelId)
                << ",coefficient," << CsvEscape(row.sourceTermId) << ',' << CsvEscape(row.label) << ',' << CsvEscape(row.coefficientId) << ','
                << Number(row.estimate) << ',' << Number(row.standardError) << ',' << Number(row.statistic) << ",1," << Number(panel.modelResult.dfResidual) << ','
                << Number(row.pValue) << ',' << Number(row.adjustedPValue) << ',' << Number(row.confidenceLower) << ',' << Number(row.confidenceUpper) << ','
                << panel.modelResult.n << ',' << Number(panel.modelResult.r2) << ',' << Number(panel.modelResult.adjR2) << ','
                << (row.estimable ? "TRUE" : "FALSE") << ',' << CsvEscape(WarningText(panel)) << "\n";
        }
        for (const auto &test : panel.termTests) {
            out << CsvEscape(panel.rowLevelLabel) << ',' << CsvEscape(panel.columnLevelLabel) << ',' << CsvEscape(panel.panelId)
                << ",term_test," << CsvEscape(test.termId) << ',' << CsvEscape(test.label) << ",,,," << Number(test.statistic) << ','
                << Number(test.df1) << ',' << Number(test.df2) << ',' << Number(test.pValue) << ',' << Number(test.adjustedPValue) << ",,,"
                << panel.modelResult.n << ',' << Number(panel.modelResult.r2) << ',' << Number(panel.modelResult.adjR2) << ','
                << (test.estimable ? "TRUE" : "FALSE") << ',' << CsvEscape(WarningText(panel)) << "\n";
        }
    }
    return out.str();
}

bool ApplyModelTrellisUpdatePayload(ModelTrellisState &state,
                                    int generation,
                                    const std::vector<std::string> &payload,
                                    std::size_t &cursor,
                                    std::string *error)
{
    if (generation != state.specificationGeneration) return true;
    auto fail = [&](const std::string &message) {
        if (error) *error = message;
        return false;
    };
    if (cursor >= payload.size()) return fail("Missing Model Trellis panel count.");
    const long panelCount = std::strtol(payload[cursor++].c_str(), nullptr, 10);
    if (panelCount < 0) return fail("Invalid Model Trellis panel count.");
    std::map<std::string, ModelTrellisPanelResult *> byId;
    for (auto &panel : state.panels) byId[panel.panelId] = &panel;
    for (long panelIndex = 0; panelIndex < panelCount; ++panelIndex) {
        if (cursor + 8 > payload.size()) return fail("Incomplete Model Trellis panel metadata.");
        const std::string panelId = payload[cursor++];
        const std::string rowLevelId = payload[cursor++];
        const std::string rowLabel = payload[cursor++];
        const std::string columnLevelId = payload[cursor++];
        const std::string columnLabel = payload[cursor++];
        const bool hasObservations = payload[cursor++] == "TRUE";
        const bool estimable = payload[cursor++] == "TRUE";
        const long warningCount = std::strtol(payload[cursor++].c_str(), nullptr, 10);
        if (warningCount < 0 || cursor + (std::size_t)warningCount > payload.size()) return fail("Invalid Model Trellis warning payload.");
        std::vector<std::string> warnings;
        for (long i = 0; i < warningCount; ++i) warnings.push_back(payload[cursor++]);
        GLMFitSummary fit;
        if (!ReadLinearFitPayload(payload, cursor, fit)) return fail("Invalid Model Trellis GLM result payload.");
        if (cursor >= payload.size()) return fail("Missing Model Trellis coefficient count.");
        const long coefficientCount = std::strtol(payload[cursor++].c_str(), nullptr, 10);
        if (coefficientCount < 0) return fail("Invalid Model Trellis coefficient count.");
        std::vector<ModelTrellisCoefficientResult> coefficients;
        for (long i = 0; i < coefficientCount; ++i) {
            if (cursor + 17 > payload.size()) return fail("Incomplete Model Trellis coefficient payload.");
            ModelTrellisCoefficientResult row;
            row.coefficientId = payload[cursor++];
            row.sourceTermId = payload[cursor++];
            row.label = payload[cursor++];
            row.rowType = payload[cursor++];
            row.termType = payload[cursor++];
            row.factorLevel = payload[cursor++];
            row.referenceLevel = payload[cursor++];
            row.estimate = ParseOptionalDataCellDouble(payload[cursor++]);
            row.standardError = ParseOptionalDataCellDouble(payload[cursor++]);
            row.statistic = ParseOptionalDataCellDouble(payload[cursor++]);
            row.pValue = ParseOptionalDataCellDouble(payload[cursor++]);
            row.adjustedPValue = ParseOptionalDataCellDouble(payload[cursor++]);
            row.holmPValue = ParseOptionalDataCellDouble(payload[cursor++]);
            row.bonferroniPValue = ParseOptionalDataCellDouble(payload[cursor++]);
            row.confidenceLower = ParseOptionalDataCellDouble(payload[cursor++]);
            row.confidenceUpper = ParseOptionalDataCellDouble(payload[cursor++]);
            row.estimable = payload[cursor++] == "TRUE";
            coefficients.push_back(std::move(row));
        }
        if (cursor >= payload.size()) return fail("Missing Model Trellis term-test count.");
        const long termCount = std::strtol(payload[cursor++].c_str(), nullptr, 10);
        if (termCount < 0) return fail("Invalid Model Trellis term-test count.");
        std::vector<ModelTrellisTermTestResult> tests;
        for (long i = 0; i < termCount; ++i) {
            if (cursor + 10 > payload.size()) return fail("Incomplete Model Trellis term-test payload.");
            ModelTrellisTermTestResult test;
            test.termId = payload[cursor++];
            test.label = payload[cursor++];
            test.statistic = ParseOptionalDataCellDouble(payload[cursor++]);
            test.df1 = ParseOptionalDataCellDouble(payload[cursor++]);
            test.df2 = ParseOptionalDataCellDouble(payload[cursor++]);
            test.pValue = ParseOptionalDataCellDouble(payload[cursor++]);
            test.adjustedPValue = ParseOptionalDataCellDouble(payload[cursor++]);
            test.holmPValue = ParseOptionalDataCellDouble(payload[cursor++]);
            test.bonferroniPValue = ParseOptionalDataCellDouble(payload[cursor++]);
            test.estimable = payload[cursor++] == "TRUE";
            tests.push_back(std::move(test));
        }
        if (cursor >= payload.size()) return fail("Missing adjusted global p value.");
        const double adjustedGlobal = ParseOptionalDataCellDouble(payload[cursor++]);
        if (cursor + 2 > payload.size()) return fail("Missing Model Trellis global adjustment payload.");
        const double holmGlobal = ParseOptionalDataCellDouble(payload[cursor++]);
        const double bonferroniGlobal = ParseOptionalDataCellDouble(payload[cursor++]);
        auto found = byId.find(panelId);
        if (found == byId.end()) continue;
        ModelTrellisPanelResult &panel = *found->second;
        panel.key.rowLevelId = rowLevelId.empty() ? std::nullopt : std::optional<std::string>(rowLevelId);
        panel.key.columnLevelId = columnLevelId.empty() ? std::nullopt : std::optional<std::string>(columnLevelId);
        panel.rowLevelLabel = rowLabel;
        panel.columnLevelLabel = columnLabel;
        panel.hasObservations = hasObservations;
        panel.estimable = estimable;
        panel.warnings = std::move(warnings);
        panel.modelResult = std::move(fit);
        panel.originalRowIndices = panel.modelResult.rowsUsed;
        panel.coefficients = std::move(coefficients);
        panel.termTests = std::move(tests);
        panel.adjustedGlobalPValue = adjustedGlobal;
        panel.holmGlobalPValue = holmGlobal;
        panel.bonferroniGlobalPValue = bonferroniGlobal;
    }
    state.fittedGeneration = generation;
    state.fitPending = false;
    state.status = "Panel models fitted in R.";
    ApplyModelTrellisPAdjustment(state);
    return true;
}

} // namespace core
} // namespace rlispstat
