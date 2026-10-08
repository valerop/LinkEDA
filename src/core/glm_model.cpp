#include "glm_model.h"
#include "string_utils.h"

#include "command_model.h"
#include "dataset_model.h"
#include "format_model.h"
#include "model_terms.h"
#include "scatterplot_model.h"
#include "statistics_model.h"

#include <algorithm>
#include <atomic>
#include <array>
#include <cmath>
#include <cctype>
#include <cstdint>
#include <cstdlib>
#include <iomanip>
#include <limits>
#include <set>
#include <sstream>
#include <utility>

namespace rlispstat {
namespace core {

namespace {

std::string VerificationRCharacterVector(const std::vector<std::string> &values)
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

bool InvertMatrix(std::vector<std::vector<double>> a, std::vector<std::vector<double>> &inv)
{
    std::size_t n = a.size();
    inv.assign(n, std::vector<double>(n, 0.0));
    for (std::size_t i = 0; i < n; ++i) {
        if (a[i].size() != n) return false;
        inv[i][i] = 1.0;
    }
    for (std::size_t col = 0; col < n; ++col) {
        std::size_t pivot = col;
        for (std::size_t row = col + 1; row < n; ++row) {
            if (std::fabs(a[row][col]) > std::fabs(a[pivot][col])) pivot = row;
        }
        if (std::fabs(a[pivot][col]) < 1e-12) return false;
        if (pivot != col) {
            std::swap(a[pivot], a[col]);
            std::swap(inv[pivot], inv[col]);
        }
        double scale = a[col][col];
        for (std::size_t j = 0; j < n; ++j) {
            a[col][j] /= scale;
            inv[col][j] /= scale;
        }
        for (std::size_t row = 0; row < n; ++row) {
            if (row == col) continue;
            double f = a[row][col];
            for (std::size_t j = 0; j < n; ++j) {
                a[row][j] -= f * a[col][j];
                inv[row][j] -= f * inv[col][j];
            }
        }
    }
    return true;
}

bool TermVectorIncludesAll(const std::vector<std::string> &superset,
                           const std::vector<std::string> &subset)
{
    for (const std::string &term : subset) {
        if (!TermListContainsEquivalentModelTerm(superset, term)) {
            return false;
        }
    }
    return true;
}

const ScenarioVariableInfo *FindScenarioVariableInfo(const ScenarioDesignInput &input,
                                                     const std::string &variable)
{
    auto it = input.variables.find(variable);
    return it == input.variables.end() ? nullptr : &it->second;
}

std::string InferredLevelSourceTerm(const std::string &term)
{
    std::string source = ModelTermWithoutLevelSuffixes(term);
    return source == term ? "" : source;
}

std::string EffectiveLinearRowSourceTerm(const GLMCoefficientRow &row,
                                         const std::string &fallback)
{
    if (!row.sourceTerm.empty() && row.sourceTerm != "NA") return row.sourceTerm;
    std::string inferred = InferredLevelSourceTerm(row.term);
    return inferred.empty() ? fallback : inferred;
}

std::string EffectiveGeneralizedRowSourceTerm(const GeneralizedGLMRow &row,
                                              const std::string &fallback)
{
    if (!row.sourceTerm.empty() && row.sourceTerm != "NA") return row.sourceTerm;
    std::string inferred = InferredLevelSourceTerm(row.term);
    return inferred.empty() ? fallback : inferred;
}

std::string ScenarioTypeForTerm(const std::string &term,
                                const ScenarioDesignInput &input)
{
    auto typeIt = input.termTypes.find(term);
    if (typeIt != input.termTypes.end() &&
        (typeIt->second == "numeric" || typeIt->second == "factor")) {
        return typeIt->second;
    }
    const std::string variable = BaseVariableForTermComponent(term);
    const ScenarioVariableInfo *info = FindScenarioVariableInfo(input, variable);
    return info && info->type == "factor" ? "factor" : "numeric";
}

} // namespace

GLMResultTableLayout GLMAnovaResultTableLayout()
{
    return {{{128.0, false}, {128.0, false}, {54.0, false},
             {104.0, false}, {78.0, false}, {64.0, false}}, false};
}

GLMResultTableLayout GLMTermsResultTableLayout()
{
    return {{{174.0, false, 360.0}, {76.0, false}, {70.0, false},
             {64.0, false}, {72.0, false}, {64.0, false},
             {64.0, false}, {76.0, false}, {72.0, false}}, false};
}

std::vector<double> ResolveGLMResultTableColumnWidths(
    const GLMResultTableLayout &layout, double availableWidth)
{
    std::vector<double> widths;
    widths.reserve(layout.columns.size());
    double minimumTotal = 0.0;
    std::size_t flexibleCount = 0;
    for (const auto &column : layout.columns) {
        const double minimum = std::max(0.0, column.minimumWidth);
        widths.push_back(minimum);
        minimumTotal += minimum;
        if (column.absorbsExtraWidth) ++flexibleCount;
    }
    if (!layout.stretchesToAvailableWidth || flexibleCount == 0 ||
        availableWidth <= minimumTotal) {
        return widths;
    }
    const double share = (availableWidth - minimumTotal) /
        static_cast<double>(flexibleCount);
    for (std::size_t index = 0; index < layout.columns.size(); ++index) {
        if (layout.columns[index].absorbsExtraWidth) widths[index] += share;
    }
    return widths;
}

bool GLMTermsTableNeedsRebuild(bool initialized,
                               const std::vector<std::string> &currentStructure,
                               const std::vector<std::string> &nextStructure)
{
    return !initialized || currentStructure != nextStructure;
}

namespace {

const std::vector<DistributionSpec> &SharedDistributionCatalogue()
{
    static const std::vector<DistributionSpec> catalogue = {
        {"gaussian", "Gaussian", ResponseDomain::Continuous,
         {"identity"}, "identity", StatisticalFitBackend::StatsLm,
         true, true, true, true, true, false, true, true, true, true},
        {"gaussian_log", "Gaussian (log link)", ResponseDomain::Continuous,
         {"log"}, "log", StatisticalFitBackend::StatsGlm,
         true, true, true, true, true, true, true, true, true, true},
        {"lognormal", "Lognormal", ResponseDomain::StrictlyPositiveContinuous,
         {"identity"}, "identity", StatisticalFitBackend::StatsLm,
         true, true, true, true, true, true, true, true, true, true},
        {"binomial", "Binomial", ResponseDomain::Binary,
         {"logit", "log", "probit", "cloglog"}, "logit", StatisticalFitBackend::StatsGlm,
         true, true, true, true, true, false, true, true, true, true},
        {"poisson", "Poisson", ResponseDomain::NonNegativeInteger,
         {"log"}, "log", StatisticalFitBackend::StatsGlm,
         true, true, true, true, true, false, true, true, true, true},
        {"negative_binomial", "Negative binomial", ResponseDomain::NonNegativeInteger,
         {"log"}, "log", StatisticalFitBackend::MassGlmNb,
         true, true, true, true, true, true, true, true, true, true},
        {"binomial_trials", "Binomial (successes/trials)", ResponseDomain::NonNegativeInteger,
         {"logit"}, "logit", StatisticalFitBackend::StatsGlm,
         true, true, true, true, true, false, true, true, true, true},
        {"beta_binomial", "Beta-Binomial", ResponseDomain::NonNegativeInteger,
         {"logit"}, "logit", StatisticalFitBackend::GlmmTMBBetaBinomial,
         true, true, true, true, true, true, true, true, true, true},
        {"hurdle_beta_binomial_ceiling", "Hurdle beta-binomial (ceiling)",
         ResponseDomain::NonNegativeInteger,
         {"logit"}, "logit", StatisticalFitBackend::GamlssCeilingHurdleBetaBinomial,
         true, true, true, true, true, true, true, true, true, true},
        {"perfect_score", "Perfect score (logistic)",
         ResponseDomain::NonNegativeInteger,
         {"logit"}, "logit", StatisticalFitBackend::StatsGlm,
         true, true, true, true, true, false, true, true, true, true},
        {"Gamma", "Gamma", ResponseDomain::StrictlyPositiveContinuous,
         {"log", "inverse", "identity"}, "log", StatisticalFitBackend::StatsGlm,
         true, true, true, true, true, true, true, true, true, true},
        {"inverse.gaussian", "Inverse Gaussian", ResponseDomain::StrictlyPositiveContinuous,
         {"log", "inverse", "identity", "1/mu^2"}, "log", StatisticalFitBackend::StatsGlm,
         true, true, true, true, true, true, true, true, true, true},
        {"beta", "Beta", ResponseDomain::OpenUnitInterval,
         {"logit"}, "logit", StatisticalFitBackend::BetaReg,
         true, true, true, true, true, true, true, true, true, true},
        {"beta_one_inflated", "One-inflated beta", ResponseDomain::OpenClosedUnitInterval,
         {"logit"}, "logit", StatisticalFitBackend::GamlssBetaOneInflated,
         true, true, true, true, true, true, true, true, true, true},
        // Quasi choices are deliberately marked as inference approaches and
        // are returned only through InferenceOptionSpecificationsForModel().
        {"quasipoisson", "Quasi-Poisson", ResponseDomain::NonNegativeInteger,
         {"log"}, "log", StatisticalFitBackend::StatsGlm,
         false, false, false, false, true, true, true, true, true, true},
        {"quasibinomial", "Quasi-binomial", ResponseDomain::Binary,
         {"logit", "probit", "cloglog"}, "logit", StatisticalFitBackend::StatsGlm,
         false, false, false, false, true, true, true, true, true, true},
        // Retained solely for legacy/programmatic compatibility.  Distance to
        // a configured maximum is a transformation plus Gamma model, not a
        // probability distribution offered by the normal model selector.
        {"gamma_distance", "Gamma distance to maximum", ResponseDomain::Any,
         {"log"}, "log", StatisticalFitBackend::StatsGlm,
         true, true, true, true, true, true, true, true, true, true}
    };
    return catalogue;
}

const std::vector<StatisticalModelTypeSpec> &SharedModelTypeCatalogue()
{
    static const std::vector<StatisticalModelTypeSpec> catalogue = {
        {StatisticalModelType::Linear, "linear", "Linear Model",
         ResponseDomain::Continuous, {"gaussian"}, {}, false},
        {StatisticalModelType::Binary, "binary", "Binary Model",
         ResponseDomain::Binary, {"binomial"}, {}, false},
        {StatisticalModelType::Count, "count", "Count Model",
         ResponseDomain::NonNegativeInteger,
         {"poisson", "negative_binomial", "binomial_trials", "beta_binomial",
          "hurdle_beta_binomial_ceiling", "perfect_score"},
         {"quasipoisson"}, false},
        {StatisticalModelType::PositiveContinuous, "positive_continuous",
         "Positive Continuous Model", ResponseDomain::Continuous,
         {"gaussian_log", "lognormal", "Gamma", "inverse.gaussian"}, {}, false},
        {StatisticalModelType::Proportion, "proportion", "Proportion Model",
         ResponseDomain::OpenClosedUnitInterval,
         {"beta", "beta_one_inflated"}, {}, false},
        {StatisticalModelType::LegacyGeneralized, "legacy_generalized",
         "Generalized Linear Model", ResponseDomain::Any,
         {"gaussian", "binomial", "poisson", "Gamma", "inverse.gaussian",
          "beta", "beta_one_inflated", "gamma_distance"},
         {"quasibinomial", "quasipoisson"}, false}
    };
    return catalogue;
}

} // namespace

std::string StatisticalModelTypeId(StatisticalModelType type)
{
    return ModelTypeSpecification(type).id;
}

std::string StatisticalModelTypeLabel(StatisticalModelType type)
{
    return ModelTypeSpecification(type).visibleName;
}

std::string StatisticalModelWindowTitle(StatisticalModelType type,
                                        bool multipleImputation)
{
    std::string title = StatisticalModelTypeLabel(type);
    if (multipleImputation) title += " \xE2\x80\x94 Multiple Imputation";
    return title;
}

std::string StatisticalModelComparisonTitle(StatisticalModelType type,
                                            bool multipleImputation)
{
    std::string title;
    switch (type) {
    case StatisticalModelType::Linear: title = "Compare Linear Models"; break;
    case StatisticalModelType::Binary: title = "Compare Binary Models"; break;
    case StatisticalModelType::Count: title = "Compare Count Models"; break;
    case StatisticalModelType::PositiveContinuous:
        title = "Compare Positive Continuous Models";
        break;
    case StatisticalModelType::Proportion:
        title = "Compare Proportion Models";
        break;
    case StatisticalModelType::LegacyGeneralized:
        title = "Compare Generalized Linear Models";
        break;
    }
    if (multipleImputation) title += " \xE2\x80\x94 Multiple Imputation";
    return title;
}

bool ParseStatisticalModelType(const std::string &value, StatisticalModelType &type)
{
    for (const auto &spec : SharedModelTypeCatalogue()) {
        if (value == spec.id || value == spec.visibleName) {
            type = spec.type;
            return true;
        }
    }
    return false;
}

const StatisticalModelTypeSpec &ModelTypeSpecification(StatisticalModelType type)
{
    for (const auto &spec : SharedModelTypeCatalogue()) {
        if (spec.type == type) return spec;
    }
    return SharedModelTypeCatalogue().back();
}

const DistributionSpec *FindDistributionSpecification(const std::string &id)
{
    for (const auto &spec : SharedDistributionCatalogue()) {
        if (spec.id == id) return &spec;
    }
    return nullptr;
}

std::vector<DistributionSpec> DistributionSpecificationsForModel(StatisticalModelType type)
{
    std::vector<DistributionSpec> result;
    for (const std::string &id : ModelTypeSpecification(type).distributionIds) {
        const DistributionSpec *spec = FindDistributionSpecification(id);
        if (spec && spec->fullProbabilityDistribution) result.push_back(*spec);
    }
    return result;
}

std::vector<DistributionSpec> InferenceOptionSpecificationsForModel(StatisticalModelType type)
{
    std::vector<DistributionSpec> result;
    for (const std::string &id : ModelTypeSpecification(type).inferenceOptionIds) {
        const DistributionSpec *spec = FindDistributionSpecification(id);
        if (spec && !spec->fullProbabilityDistribution) result.push_back(*spec);
    }
    return result;
}

std::vector<std::string> ModelStructuresForModel(StatisticalModelType type)
{
    return ModelTypeSpecification(type).modelStructureIds;
}

bool DistributionIsAvailableForModel(StatisticalModelType type, const std::string &id)
{
    const auto &model = ModelTypeSpecification(type);
    return std::find(model.distributionIds.begin(), model.distributionIds.end(), id) !=
               model.distributionIds.end() ||
           std::find(model.inferenceOptionIds.begin(), model.inferenceOptionIds.end(), id) !=
               model.inferenceOptionIds.end();
}

std::vector<std::string> LinksForModelDistribution(StatisticalModelType type,
                                                   const std::string &distributionId)
{
    if (!DistributionIsAvailableForModel(type, distributionId)) return {};
    const DistributionSpec *spec = FindDistributionSpecification(distributionId);
    return spec ? spec->supportedLinks : std::vector<std::string>{};
}

std::string DefaultLinkForModelDistribution(StatisticalModelType type,
                                            const std::string &distributionId)
{
    if (!DistributionIsAvailableForModel(type, distributionId)) return "";
    const DistributionSpec *spec = FindDistributionSpecification(distributionId);
    return spec ? spec->defaultLink : "";
}

std::string StatisticalFitBackendId(StatisticalFitBackend backend)
{
    switch (backend) {
    case StatisticalFitBackend::StatsLm: return "stats::lm";
    case StatisticalFitBackend::StatsGlm: return "stats::glm";
    case StatisticalFitBackend::MassGlmNb: return "MASS::glm.nb";
    case StatisticalFitBackend::BetaReg: return "betareg::betareg";
    case StatisticalFitBackend::GamlssBetaOneInflated: return "gamlss::gamlss";
    case StatisticalFitBackend::GlmmTMBBetaBinomial: return "glmmTMB::glmmTMB";
    case StatisticalFitBackend::GamlssCeilingHurdleBetaBinomial:
        return "gamlss::gamlss with gamlss.dist::ZABB";
    }
    return "";
}

StatisticalModelType StatisticalModelTypeForFamily(const std::string &family,
                                                   bool binaryRegression,
                                                   bool countRegression)
{
    if (binaryRegression) return StatisticalModelType::Binary;
    if (countRegression) return StatisticalModelType::Count;
    if (family == "gaussian_log" || family == "lognormal" ||
        family == "Gamma" || family == "inverse.gaussian")
        return StatisticalModelType::PositiveContinuous;
    if (family == "beta" || family == "beta_one_inflated")
        return StatisticalModelType::Proportion;
    if (family == "gaussian") return StatisticalModelType::Linear;
    if (family == "binomial") return StatisticalModelType::Binary;
    if (family == "poisson" || family == "quasipoisson" ||
        family == "negative_binomial") return StatisticalModelType::Count;
    return StatisticalModelType::LegacyGeneralized;
}

std::vector<std::string> GeneralizedLinksForFamily(const std::string &family)
{
    if (family == "gaussian") return {"identity", "log", "inverse"};
    if (family == "gaussian_log") return {"log"};
    if (family == "lognormal") return {"identity"};
    if (family == "binomial") return {"logit", "probit", "cloglog", "cauchit", "log"};
    if (family == "poisson") return {"log", "identity", "sqrt"};
    if (family == "Gamma") return {"inverse", "identity", "log"};
    if (family == "inverse.gaussian") return {"1/mu^2", "inverse", "identity", "log"};
    if (family == "quasibinomial") return {"logit", "probit", "cloglog", "cauchit", "log"};
    if (family == "quasipoisson") return {"log", "identity", "sqrt"};
    if (family == "beta" || family == "beta_one_inflated") return {"logit"};
    if (family == "gamma_distance") return {"log"};
    return {"logit"};
}

std::string DefaultGeneralizedLink(const std::string &family)
{
    if (family == "gaussian") return "identity";
    if (family == "gaussian_log") return "log";
    if (family == "lognormal") return "identity";
    if (family == "Gamma") return "inverse";
    if (family == "inverse.gaussian") return "1/mu^2";
    if (family == "poisson" || family == "quasipoisson") return "log";
    if (family == "gamma_distance") return "log";
    return "logit";
}

bool IsValidGeneralizedFamily(const std::string &family)
{
    static const std::set<std::string> families = {
        "gaussian", "gaussian_log", "lognormal", "binomial", "poisson", "Gamma", "inverse.gaussian",
        "quasibinomial", "quasipoisson", "beta", "beta_one_inflated",
        "gamma_distance"
    };
    return families.find(family) != families.end();
}

bool GeneralizedFamilyRequiresResponseBounds(const std::string &family)
{
    return family == "beta" || family == "beta_one_inflated" ||
           family == "gamma_distance";
}

bool IsValidGeneralizedLink(const std::string &family, const std::string &link)
{
    std::vector<std::string> links = GeneralizedLinksForFamily(family);
    return std::find(links.begin(), links.end(), link) != links.end();
}

std::string BinaryLinkId(BinaryLink link)
{
    if (link == BinaryLink::Log) return "log";
    if (link == BinaryLink::Probit) return "probit";
    if (link == BinaryLink::Cloglog) return "cloglog";
    return "logit";
}

std::string BinaryLinkLabel(BinaryLink link)
{
    if (link == BinaryLink::Log) return "Log";
    if (link == BinaryLink::Probit) return "Probit";
    if (link == BinaryLink::Cloglog) return "Complementary log-log";
    return "Logit";
}

bool ParseBinaryLink(const std::string &value, BinaryLink &link)
{
    if (value == "logit" || value == "Logit") {
        link = BinaryLink::Logit;
        return true;
    }
    if (value == "log" || value == "Log") {
        link = BinaryLink::Log;
        return true;
    }
    if (value == "probit" || value == "Probit") {
        link = BinaryLink::Probit;
        return true;
    }
    if (value == "cloglog" || value == "Cloglog" ||
        value == "Complementary log-log") {
        link = BinaryLink::Cloglog;
        return true;
    }
    return false;
}

std::vector<BinaryLink> SupportedBinaryLinks()
{
    return {BinaryLink::Logit, BinaryLink::Log, BinaryLink::Probit, BinaryLink::Cloglog};
}

bool GeneralizedModelHasExponentiatedEffect(const GeneralizedGLMState &state)
{
    return ((state.binaryRegression || state.family == "binomial") &&
            (state.binaryLink == BinaryLink::Logit || state.binaryLink == BinaryLink::Log)) ||
           state.countRegression || state.family == "gaussian_log" ||
           state.family == "lognormal";
}

std::string GeneralizedExponentiatedEffectLabel(const GeneralizedGLMState &state)
{
    if (state.countRegression && CountDistributionUsesTrials(state.countDistribution))
        return "Odds ratio";
    if (state.countRegression) return "Rate ratio";
    if ((state.binaryRegression || state.family == "binomial") &&
        state.binaryLink == BinaryLink::Logit)
        return "Odds ratio";
    if ((state.binaryRegression || state.family == "binomial") &&
        state.binaryLink == BinaryLink::Log)
        return "Risk ratio";
    if (state.family == "gaussian_log") return "Mean ratio";
    if (state.family == "lognormal") return "Multiplicative ratio";
    return "Exponentiated coefficient";
}

bool GeneralizedModelShowsGlobalTermTests(const GeneralizedGLMState &state)
{
    return !state.termTests.empty();
}

std::string GlobalTermTestMethodLabel(const std::string &method)
{
    if (method == "LR chi-square") return "LR χ²";
    if (method == "Wald chi-square") return "Wald χ²";
    if (method == "Rubin Wald chi-square") return "Pooled Wald χ²";
    if (method == "D1/Wald F" || method == "mice::D1") return "Pooled F";
    if (method == "F") return "F";
    return method.empty() ? "—" : method;
}

bool GeneralizedModelUsesMixedGlobalTermTestMethods(const GeneralizedGLMState &state)
{
    std::set<std::string> methods;
    for (const GlobalTermTestRow &test : state.termTests) {
        if (!test.method.empty()) methods.insert(GlobalTermTestMethodLabel(test.method));
    }
    return methods.size() > 1;
}

std::string GeneralizedGlobalTermTestStatisticHeader(const GeneralizedGLMState &state)
{
    std::set<std::string> methods;
    for (const GlobalTermTestRow &test : state.termTests) {
        if (!test.method.empty()) methods.insert(GlobalTermTestMethodLabel(test.method));
    }
    return methods.size() == 1 ? *methods.begin() : "Term test";
}

std::string GeneralizedGlobalTermTestHierarchyNote(const GeneralizedGLMState &state)
{
    for (const GlobalTermTestRow &test : state.termTests) {
        const std::vector<std::string> components = UniqueBaseVariablesForTerm(test.term);
        if (components.empty()) continue;
        for (const GlobalTermTestRow &candidate : state.termTests) {
            const std::vector<std::string> higher = UniqueBaseVariablesForTerm(candidate.term);
            if (higher.size() <= components.size()) continue;
            const bool contains = std::all_of(
                components.begin(), components.end(), [&](const std::string &component) {
                    return std::find(higher.begin(), higher.end(), component) != higher.end();
                });
            if (contains) {
                return "Lower-order terms contained in higher-order interactions are tested conditionally under the current model parameterization; interpret them together with simple effects or the effect plot.";
            }
        }
    }
    return "";
}

const GlobalTermTestRow *GeneralizedGlobalTermTestForPresentationRow(
    const GeneralizedGLMState &state,
    const GeneralizedGLMRow &row)
{
    // A global multi-parameter test belongs only on the semantic parent row.
    // Direct one-coefficient terms keep their coefficient statistic and p value.
    if (row.rowType != "factor_parent" && row.rowType != "term_parent") return nullptr;
    const std::string source = row.sourceTerm.empty() ? row.term : row.sourceTerm;
    if (source.empty() || source == "(Intercept)") return nullptr;
    for (const GlobalTermTestRow &test : state.termTests) {
        if (EquivalentModelTerm(test.term, source) &&
            (row.component.empty() || test.component.empty() ||
             test.component == row.component)) return &test;
    }
    return nullptr;
}

std::string GeneralizedGlobalTermTestMethodNote(const GeneralizedGLMState &state)
{
    std::set<std::string> methods;
    for (const GeneralizedGLMRow &row : state.rows) {
        const GlobalTermTestRow *test =
            GeneralizedGlobalTermTestForPresentationRow(state, row);
        if (test && !test->method.empty()) {
            methods.insert(GlobalTermTestMethodLabel(test->method));
        }
    }
    if (methods.empty()) return "";
    std::ostringstream note;
    note << "Global tests shown on parent term rows use ";
    bool first = true;
    for (const std::string &method : methods) {
        if (!first) note << "; ";
        note << method;
        first = false;
    }
    note << ".";
    return note.str();
}

std::string GeneralizedGlobalTermTestPooledWaldFootnote(
    const GeneralizedGLMState &state)
{
    for (const GeneralizedGLMRow &row : state.rows) {
        const GlobalTermTestRow *test =
            GeneralizedGlobalTermTestForPresentationRow(state, row);
        if (test && test->method == "Rubin Wald chi-square" &&
            std::isfinite(test->statistic))
            return "† Parent-term statistics are pooled Wald χ² tests, not t statistics.";
    }
    return "";
}

std::string CountDistributionId(CountDistribution distribution)
{
    if (distribution == CountDistribution::QuasiPoisson) return "quasipoisson";
    if (distribution == CountDistribution::NegativeBinomial) return "negative_binomial";
    if (distribution == CountDistribution::BinomialTrials) return "binomial_trials";
    if (distribution == CountDistribution::BetaBinomial) return "beta_binomial";
    if (distribution == CountDistribution::HurdleBetaBinomialCeiling)
        return "hurdle_beta_binomial_ceiling";
    if (distribution == CountDistribution::PerfectScore) return "perfect_score";
    return "poisson";
}

std::string CountDistributionLabel(CountDistribution distribution)
{
    if (distribution == CountDistribution::QuasiPoisson) return "Quasi-Poisson";
    if (distribution == CountDistribution::NegativeBinomial) return "Negative binomial";
    if (distribution == CountDistribution::BinomialTrials) return "Binomial (successes/trials)";
    if (distribution == CountDistribution::BetaBinomial) return "Beta-Binomial";
    if (distribution == CountDistribution::HurdleBetaBinomialCeiling)
        return "Hurdle beta-binomial (ceiling)";
    if (distribution == CountDistribution::PerfectScore)
        return "Perfect score (logistic)";
    return "Poisson";
}

bool ParseCountDistribution(const std::string &value,
                            CountDistribution &distribution)
{
    if (value == "poisson" || value == "Poisson") {
        distribution = CountDistribution::Poisson;
        return true;
    }
    if (value == "quasipoisson" || value == "quasi_poisson" ||
        value == "Quasi-Poisson") {
        distribution = CountDistribution::QuasiPoisson;
        return true;
    }
    if (value == "negative_binomial" || value == "negative-binomial" ||
        value == "Negative binomial" || value == "Negative Binomial") {
        distribution = CountDistribution::NegativeBinomial;
        return true;
    }
    if (value == "binomial_trials" || value == "Binomial (successes/trials)") {
        distribution = CountDistribution::BinomialTrials;
        return true;
    }
    if (value == "beta_binomial" || value == "beta-binomial" ||
        value == "Beta-Binomial") {
        distribution = CountDistribution::BetaBinomial;
        return true;
    }
    if (value == "hurdle_beta_binomial_ceiling" ||
        value == "Hurdle beta-binomial (ceiling)") {
        distribution = CountDistribution::HurdleBetaBinomialCeiling;
        return true;
    }
    if (value == "perfect_score" || value == "Perfect score" ||
        value == "Perfect score (logistic)") {
        distribution = CountDistribution::PerfectScore;
        return true;
    }
    return false;
}

bool CountDistributionUsesTrials(CountDistribution distribution)
{
    return distribution == CountDistribution::BinomialTrials ||
           distribution == CountDistribution::BetaBinomial ||
           distribution == CountDistribution::HurdleBetaBinomialCeiling ||
           distribution == CountDistribution::PerfectScore;
}

bool CountDistributionSupportsScoreDistribution(CountDistribution distribution)
{
    return distribution == CountDistribution::Poisson ||
           distribution == CountDistribution::NegativeBinomial ||
           distribution == CountDistribution::BinomialTrials ||
           distribution == CountDistribution::BetaBinomial ||
           distribution == CountDistribution::HurdleBetaBinomialCeiling ||
           distribution == CountDistribution::PerfectScore;
}

CountResponseValidation InspectCountResponse(const DataColumn &column,
                                             const std::set<int> &includedRows)
{
    CountResponseValidation result;
    if (NormalizeVariableType(column.type) != "numeric") {
        result.status = "Count Model requires a numeric response.";
        return result;
    }
    for (std::size_t index = 0; index < column.values.size(); ++index) {
        if (!includedRows.empty() &&
            includedRows.find(static_cast<int>(index + 1)) == includedRows.end()) continue;
        const std::string value = DisplayValueForCell(column, index);
        if (DataCellIsMissing(value)) {
            ++result.missing;
            continue;
        }
        char *end = nullptr;
        const double parsed = std::strtod(value.c_str(), &end);
        while (end && *end && std::isspace(static_cast<unsigned char>(*end))) ++end;
        if (!end || *end != '\0' || !std::isfinite(parsed) || parsed < 0.0 ||
            std::fabs(parsed - std::round(parsed)) > 1.0e-8) {
            result.status = "Count Model requires finite, non-negative integer response values.";
            return result;
        }
        ++result.observed;
    }
    result.ok = result.observed > 0;
    result.status = result.ok
        ? "Count response is valid."
        : "The response has no observed non-missing values in the selected scope.";
    return result;
}

CountExposureValidation InspectCountExposure(const DataColumn &column,
                                             const std::set<int> &includedRows)
{
    CountExposureValidation result;
    if (NormalizeVariableType(column.type) != "numeric") {
        result.status = "Exposure must be numeric.";
        return result;
    }
    for (std::size_t index = 0; index < column.values.size(); ++index) {
        if (!includedRows.empty() &&
            includedRows.find(static_cast<int>(index + 1)) == includedRows.end()) continue;
        const std::string value = DisplayValueForCell(column, index);
        if (DataCellIsMissing(value)) {
            ++result.missing;
            continue;
        }
        char *end = nullptr;
        const double parsed = std::strtod(value.c_str(), &end);
        while (end && *end && std::isspace(static_cast<unsigned char>(*end))) ++end;
        if (!end || *end != '\0' || !std::isfinite(parsed) || parsed <= 0.0) {
            result.status = "Exposure values must be finite and greater than zero.";
            return result;
        }
        ++result.observed;
    }
    result.ok = result.observed > 0;
    result.status = result.ok
        ? "Exposure is valid."
        : "The exposure has no observed non-missing values in the selected scope.";
    return result;
}

LinkOffsetValidation InspectLinkOffset(const DataColumn &column,
                                       const std::set<int> &includedRows)
{
    LinkOffsetValidation result;
    if (NormalizeVariableType(column.type) != "numeric") {
        result.status = "Link-scale offset must be numeric.";
        return result;
    }
    for (std::size_t index = 0; index < column.values.size(); ++index) {
        if (!includedRows.empty() &&
            includedRows.find(static_cast<int>(index + 1)) == includedRows.end()) continue;
        const std::string value = DisplayValueForCell(column, index);
        if (DataCellIsMissing(value)) {
            ++result.missing;
            continue;
        }
        char *end = nullptr;
        const double parsed = std::strtod(value.c_str(), &end);
        while (end && *end && std::isspace(static_cast<unsigned char>(*end))) ++end;
        if (!end || *end != '\0' || !std::isfinite(parsed)) {
            result.status = "Link-scale offset values must be finite. Missing values are allowed and are excluded from the fit.";
            return result;
        }
        ++result.observed;
    }
    result.ok = result.observed > 0;
    result.status = result.ok
        ? "Link-scale offset is valid."
        : "The link-scale offset has no observed non-missing values in the selected scope.";
    return result;
}

ResponseDomainValidation InspectResponseDomain(
    const DataColumn &column,
    ResponseDomain domain,
    const std::set<int> &includedRows)
{
    ResponseDomainValidation result;
    if (domain == ResponseDomain::Binary) {
        const BinaryResponseCoding coding = InspectBinaryResponse(column, includedRows);
        result.ok = coding.ok;
        result.observed = coding.referenceCount + coding.eventCount;
        result.status = coding.status;
        return result;
    }
    if (NormalizeVariableType(column.type) != "numeric") {
        result.status = "The response must be numeric for this model.";
        return result;
    }
    for (std::size_t index = 0; index < column.values.size(); ++index) {
        if (!includedRows.empty() &&
            includedRows.find(static_cast<int>(index + 1)) == includedRows.end()) continue;
        const std::string value = DisplayValueForCell(column, index);
        if (DataCellIsMissing(value)) {
            ++result.missing;
            continue;
        }
        char *end = nullptr;
        const double parsed = std::strtod(value.c_str(), &end);
        while (end && *end && std::isspace(static_cast<unsigned char>(*end))) ++end;
        if (!end || *end != '\0' || !std::isfinite(parsed)) {
            result.status = "The response must contain finite numeric values in the analysis scope.";
            return result;
        }
        bool valid = true;
        switch (domain) {
        case ResponseDomain::NonNegativeInteger:
            valid = parsed >= 0.0 && std::fabs(parsed - std::round(parsed)) <= 1.0e-8;
            break;
        case ResponseDomain::StrictlyPositiveContinuous:
            valid = parsed > 0.0;
            break;
        case ResponseDomain::OpenUnitInterval:
            valid = parsed > 0.0 && parsed < 1.0;
            break;
        case ResponseDomain::OpenClosedUnitInterval:
            valid = parsed > 0.0 && parsed <= 1.0;
            break;
        case ResponseDomain::Continuous:
        case ResponseDomain::Any:
        case ResponseDomain::Binary:
            break;
        }
        if (!valid) {
            if (domain == ResponseDomain::NonNegativeInteger)
                result.status = "Count Model requires finite, non-negative integer response values in the analysis scope.";
            else if (domain == ResponseDomain::StrictlyPositiveContinuous)
                result.status = "Positive Continuous Model requires response values greater than zero in the analysis scope.";
            else if (domain == ResponseDomain::OpenUnitInterval)
                result.status = "Beta requires response values strictly between 0 and 1 in the analysis scope.";
            else if (domain == ResponseDomain::OpenClosedUnitInterval)
                result.status = "One-inflated beta requires response values greater than 0 and no greater than 1 in the analysis scope.";
            return result;
        }
        ++result.observed;
    }
    result.ok = result.observed > 0;
    result.status = result.ok
        ? "The response is valid for the selected model."
        : "The response has no observed non-missing values in the analysis scope.";
    return result;
}

BinaryResponseCoding InspectBinaryResponse(const DataColumn &column,
                                           const std::set<int> &includedRows)
{
    BinaryResponseCoding result;
    result.numeric = NormalizeVariableType(column.type) == "numeric";
    std::vector<std::string> levels;
    std::map<std::string, int> counts;
    for (std::size_t index = 0; index < column.values.size(); ++index) {
        if (!includedRows.empty() && includedRows.find(static_cast<int>(index + 1)) == includedRows.end()) continue;
        const std::string value = DisplayValueForCell(column, index);
        if (DataCellIsMissing(value)) continue;
        if (counts.emplace(value, 0).second) levels.push_back(value);
        ++counts[value];
    }
    if (levels.size() != 2) {
        result.status = levels.empty()
            ? "The response has no observed non-missing values in the selected scope."
            : "Binary Model requires exactly two observed response values in the selected scope.";
        return result;
    }
    SortFactorLevelsLikeR(levels);
    result.referenceValue = levels.front();
    result.eventValue = levels.back();
    result.referenceLabel = result.referenceValue;
    result.eventLabel = result.eventValue;
    result.referenceCount = counts[result.referenceValue];
    result.eventCount = counts[result.eventValue];
    result.ok = result.referenceCount > 0 && result.eventCount > 0;
    result.status = result.ok ? "Binary response is valid." : "Both response categories must be observed.";
    return result;
}

void SortFactorLevelsLikeR(std::vector<std::string> &levels)
{
    if (levels.size() < 2) {
        return;
    }
    bool allNumeric = true;
    std::vector<double> numeric;
    numeric.reserve(levels.size());
    for (const std::string &level : levels) {
        char *end = nullptr;
        double value = std::strtod(level.c_str(), &end);
        while (end && *end && std::isspace(static_cast<unsigned char>(*end))) {
            ++end;
        }
        if (!end || *end != '\0' || !std::isfinite(value)) {
            allNumeric = false;
            break;
        }
        numeric.push_back(value);
    }
    if (allNumeric) {
        std::vector<std::pair<double, std::string>> paired;
        paired.reserve(levels.size());
        for (std::size_t i = 0; i < levels.size(); ++i) {
            paired.push_back({numeric[i], levels[i]});
        }
        std::sort(paired.begin(), paired.end(),
                  [](const auto &a, const auto &b) {
                      if (std::fabs(a.first - b.first) > 1.0e-12) return a.first < b.first;
                      return a.second < b.second;
                  });
        for (std::size_t i = 0; i < levels.size(); ++i) {
            levels[i] = paired[i].second;
        }
        return;
    }
    std::sort(levels.begin(), levels.end());
}

std::string GLMBaseVariableType(const PlotModel *model,
                                const std::string &variable,
                                const std::map<std::string, std::string> &termTypes)
{
    std::string type = ModelTermDisplayType(model, variable, termTypes);
    return type == "factor" ? "factor" : "numeric";
}

NumericSummary GLMNumericSummaryForVariable(
    const DataColumn *col,
    const std::vector<NumericVariable> &numericVars,
    const std::string &variable,
    const std::set<int> &rows)
{
    std::vector<double> values;
    const NumericVariable *var = nullptr;
    for (const auto &nv : numericVars) {
        if (nv.name == variable) { var = &nv; break; }
    }
    if (var) {
        values.reserve(var->values.size());
        for (size_t i = 0; i < var->values.size(); ++i) {
            double v = var->values[i];
            if (std::isfinite(v) && GLMRowIncluded(rows, (int)i + 1)) values.push_back(v);
        }
    } else if (col) {
        values.reserve(col->values.size());
        for (size_t i = 0; i < col->values.size(); ++i) {
            double parsed = NAN;
            if (GLMRowIncluded(rows, (int)i + 1) && ParseDataCellDouble(col->values[i], parsed) && std::isfinite(parsed)) {
                values.push_back(parsed);
            }
        }
    }
    return SummarizeFiniteValues(values);
}

std::string LinearGLMFitSignature(const std::string &dependent,
                                  const std::vector<std::string> &terms,
                                  const std::map<std::string, std::string> &termTypes,
                                  const std::string &scope,
                                  const std::set<std::string> &centeredPredictors,
                                  const std::map<std::string, std::string> &factorReferenceLevels)
{
    std::ostringstream out;
    out << dependent << "|" << scope;
    for (const std::string &term : terms) {
        out << "|" << term;
    }
    out << "|types";
    for (const auto &entry : termTypes) {
        out << "|" << entry.first << "=" << entry.second;
    }
    // Preserve the long-standing signature for uncentred models.  The extra
    // component is needed only when centring changes the fitted design.
    if (!centeredPredictors.empty()) {
        out << "|centered";
        for (const std::string &predictor : centeredPredictors) out << "|" << predictor;
    }
    if (!factorReferenceLevels.empty()) {
        out << "|references";
        for (const auto &entry : factorReferenceLevels)
            out << "|" << entry.first << "=" << entry.second;
    }
    return out.str();
}

std::string LinearGLMFitIdentityWithSelection(
    const std::string &baseSignature,
    const std::string &scope,
    const std::vector<int> &selectionRows)
{
    if (scope != "selected" && scope != "unselected") return baseSignature;
    std::ostringstream out;
    out << baseSignature << "|selection";
    for (int row : selectionRows) out << "|" << row;
    return out.str();
}

std::string GeneralizedGLMFitSignature(const GeneralizedGLMState &state)
{
    std::ostringstream out;
    out << state.group << "|model-type=" << StatisticalModelTypeId(state.modelType)
        << "|" << state.response << "|" << state.family << "|" << state.link
        << "|" << state.scope;
    if (GeneralizedFamilyRequiresResponseBounds(state.family)) {
        out << "|bounds=";
        if (state.responseBoundsConfigured) {
            out << state.responseLower << "," << state.responseUpper;
        } else {
            out << "unset";
        }
    }
    if (state.binaryRegression) {
        out << "|binary|event=" << state.responseCoding.eventValue
            << "|reference=" << state.responseCoding.referenceValue;
    }
    for (const std::string &term : state.terms) {
        out << "|" << term;
    }
    out << "|types";
    for (const auto &entry : EffectiveModelSpecificationTermTypes(state)) {
        out << "|" << entry.first << "=" << entry.second;
    }
    if (state.countRegression) {
        out << "|count|distribution=" << CountDistributionId(state.countDistribution)
            << "|exposure=" << state.exposure;
        if (CountDistributionUsesTrials(state.countDistribution)) {
            out << "|trials-variable=" << state.trialsVariable << "|trials=";
            if (std::isfinite(state.trialsConstant)) out << state.trialsConstant;
            else out << "unset";
        }
    }
    out << "|offset=" << state.offsetVariable;
    if (!state.centeredPredictors.empty()) {
        out << "|centered";
        for (const std::string &predictor : state.centeredPredictors) {
            out << "|" << predictor;
        }
    }
    if (!state.factorReferenceLevels.empty()) {
        out << "|references";
        for (const auto &entry : state.factorReferenceLevels) {
            out << "|" << entry.first << "=" << entry.second;
        }
    }
    if (state.dataScopeCaptured) {
        out << "|data-scope=" << AnalysisScopeKindId(state.dataScope.kind)
            << "|total-rows=" << state.dataScope.totalDatasetRows;
        if (state.dataScope.kind == AnalysisScopeKind::ExplicitRowIds) {
            out << "|row-ids";
            for (int row : state.dataScope.originalRowIds) out << "|" << row;
        }
    }
    return out.str();
}

std::string RegressionComparisonFitSignature(const RegressionComparisonState &state)
{
    std::ostringstream out;
    out << state.id << "|" << state.group << "|" << state.response << "|" << state.scope;
    if (state.multipleImputation || !state.imputationSetId.empty()) {
        out << "|dataset-kind=" << state.datasetType
            << "|imputation-set=" << state.imputationSetId
            << "|source-dataset=" << state.sourceDatasetId
            << "|imputations=" << std::max(0, state.imputationCount);
    }
    out << "|types";
    for (const auto &entry : state.termTypes) {
        out << "|" << entry.first << "=" << entry.second;
    }
    out << "|models";
    for (const RegressionComparisonModel &model : state.models) {
        out << "|" << model.id << ":" << model.label << ":"
            << (model.response.empty() ? state.response : model.response);
        for (const std::string &term : model.terms) {
            out << "+" << term;
        }
        if (!model.termTypeOverrides.empty()) {
            out << "~types";
            for (const auto &entry : model.termTypeOverrides) {
                out << "+" << entry.first << "=" << entry.second;
            }
        }
        if (!model.centeredPredictors.empty()) {
            out << "~centered";
            for (const std::string &predictor : model.centeredPredictors) {
                out << "+" << predictor;
            }
        }
        if (!model.factorReferenceLevels.empty()) {
            out << "~references";
            for (const auto &entry : model.factorReferenceLevels) {
                out << "+" << entry.first << "=" << entry.second;
            }
        }
    }
    return out.str();
}

std::string GeneralizedComparisonModelSpecificationFingerprint(
    const GeneralizedComparisonState &state,
    const GeneralizedComparisonModel &model)
{
    GeneralizedGLMState identity;
    static_cast<ModelSpecification &>(identity) =
        EffectiveGeneralizedComparisonModelSpecification(state, model);
    identity.group = state.group;
    identity.family = model.family.empty() ? state.family : model.family;
    identity.link = model.link.empty() ? state.link : model.link;
    identity.responseBoundsConfigured = model.responseBoundsConfigured;
    identity.responseLower = model.responseLower;
    identity.responseUpper = model.responseUpper;
    identity.binaryRegression = state.binaryComparison;
    identity.binaryLink = state.binaryLink;
    identity.responseCoding = state.responseCoding;
    identity.countRegression = state.countComparison;
    identity.countDistribution = model.countDistribution;
    identity.exposure = model.exposure;
    identity.offsetVariable = model.offsetVariable;
    identity.trialsVariable = model.trialsVariable;
    identity.trialsConstant = model.trialsConstant;
    std::string fingerprint = GeneralizedGLMFitSignature(identity);
    // The registered dataset id in GeneralizedGLMFitSignature identifies the
    // data source.  For MI comparisons the completed-dataset set is also part
    // of fit identity: a result fitted across a different number of
    // imputations must never be accepted merely because its model terms match.
    if (state.multipleImputation) {
        fingerprint += "|dataset-kind=" + state.datasetType +
            "|imputation-set=" + state.imputationSetId +
            "|source-dataset=" + state.sourceDatasetId +
            "|imputations=" + std::to_string(std::max(0, state.imputationCount));
    }
    return fingerprint;
}

std::string GeneralizedComparisonFitSignature(const GeneralizedComparisonState &state)
{
    std::ostringstream out;
    out << state.id << "|models=" << state.models.size();
    for (const GeneralizedComparisonModel &model : state.models) {
        out << "|" << model.id << "@" << model.modelVersion << "="
            << GeneralizedComparisonModelSpecificationFingerprint(state, model);
    }
    return out.str();
}

std::string DefaultGeneralizedFamilyForValues(const std::vector<double> &values)
{
    std::set<double> unique;
    bool finite = false;
    bool nonNegativeIntegers = true;
    for (double value : values) {
        if (!std::isfinite(value)) {
            continue;
        }
        finite = true;
        unique.insert(value);
        if (value < 0.0 || std::fabs(value - std::round(value)) > 1.0e-9) {
            nonNegativeIntegers = false;
        }
    }
    if (finite && unique.size() == 2) {
        return "binomial";
    }
    if (finite && nonNegativeIntegers) {
        return "poisson";
    }
    return "gaussian";
}

std::string GeneralizedGLMRscriptLaunchFailedStatus()
{
    return "Could not run Rscript for stats::glm().";
}

std::string GeneralizedGLMRScript()
{
    return R"RSCRIPT(
args <- commandArgs(TRUE)
csv <- args[[1]]
response <- args[[2]]
family_name <- args[[3]]
link_name <- args[[4]]
term_count <- if (length(args) > 4) suppressWarnings(as.integer(args[[5]])) else 0L
if (is.na(term_count) || term_count < 0L) term_count <- 0L
term_start <- 6L
terms <- if (term_count > 0L) args[seq.int(term_start, term_start + term_count - 1L)] else character()
term_types <- if (term_count > 0L) args[seq.int(term_start + term_count, term_start + 2L * term_count - 1L)] else character()
names(term_types) <- terms
clean <- function(x) gsub("[\t\r\n]", " ", as.character(x))
field <- function(x) {
  if (length(x) == 0 || is.null(x) || (length(x) == 1 && is.na(x))) "NA" else clean(x)
}
bt <- function(x) paste0("`", gsub("`", "``", x), "`")
term_expr <- function(x) {
  if (grepl(":", x, fixed = TRUE)) {
    return(paste(vapply(strsplit(x, ":", fixed = TRUE)[[1]], term_expr, character(1)), collapse = ":"))
  }
  if (grepl("^[[:alnum:]_. ]+$", x)) bt(x) else x
}
	dat <- read.csv(csv, check.names = FALSE, stringsAsFactors = FALSE, na.strings = "NA")
	if (!response %in% names(dat)) stop("Response variable is not available.")
	dat <- LinkEDA:::.rls_model_data_for_term_types(
	  dat, as.list(term_types), response = response
	)
	row_ids <- if ("..rlispstat_row_id" %in% names(dat)) dat[["..rlispstat_row_id"]] else seq_len(nrow(dat))
	rhs <- if (length(terms)) paste(vapply(terms, term_expr, character(1)), collapse = " + ") else "1"
	form <- stats::as.formula(paste(bt(response), "~", rhs))
family_fun <- get(family_name, envir = asNamespace("stats"))
family_obj <- family_fun(link = link_name)
fit <- stats::glm(form, data = dat, family = family_obj)
summary_fit <- summary(fit)
coef_matrix <- as.data.frame(unclass(summary_fit$coefficients), stringsAsFactors = FALSE)
if (ncol(coef_matrix) < 4) stop("Could not extract coefficient table.")
stat_label <- if (grepl("^z", names(coef_matrix)[[3]], ignore.case = TRUE)) "z" else "t"
names(coef_matrix)[1:4] <- c("estimate", "std_error", "statistic", "p_value")
critical <- stats::qnorm(0.975)
coef_matrix$ci_lower <- coef_matrix$estimate - critical * coef_matrix$std_error
coef_matrix$ci_upper <- coef_matrix$estimate + critical * coef_matrix$std_error
coef_matrix$term <- rownames(summary_fit$coefficients)
coef_matrix$partial_r <- NA_real_
coef_matrix$delta_r2 <- NA_real_
coef_matrix <- coef_matrix[, c("term", "estimate", "std_error", "statistic", "p_value", "ci_lower", "ci_upper", "partial_r", "delta_r2")]
rows <- LinkEDA:::.rls_model_coefficient_display_rows(fit, coef_matrix, dat, terms, statistic_name = stat_label)
	safe_num <- function(x) if (length(x) == 0 || is.null(x) || is.na(x) || !is.finite(x)) "NA" else sprintf("%.17g", x)
	cat("OK\n")
	cat("FIT", stats::nobs(fit), as.integer(nrow(dat) - stats::nobs(fit)),
	    safe_num(fit$null.deviance), safe_num(fit$deviance),
	    stats::df.residual(fit), safe_num(tryCatch(stats::AIC(fit), error = function(e) NA_real_)),
	    safe_num(tryCatch(stats::BIC(fit), error = function(e) NA_real_)),
	    safe_num(summary_fit$dispersion), safe_num(tryCatch(as.numeric(stats::logLik(fit)), error = function(e) NA_real_)),
	    stat_label, sep = "\t")
cat("\n")
	fit_rank <- if (length(fit$rank)) as.integer(fit$rank) else NA_integer_
	parameter_count <- length(stats::coef(fit))
	cat("META", if (isTRUE(fit$converged)) "TRUE" else "FALSE",
	    field(if (length(fit$iter)) as.integer(fit$iter) else NA_integer_),
	    if (isTRUE(fit$boundary)) "TRUE" else "FALSE",
	    field(fit_rank), field(parameter_count),
	    if (is.finite(fit_rank) && fit_rank < parameter_count) "TRUE" else "FALSE",
	    sep = "\t")
	cat("\n")
if (nrow(rows)) {
  for (i in seq_len(nrow(rows))) {
    cat("ROW", field(rows$row_type[[i]]), field(rows$term[[i]]), field(rows$display_label[[i]]),
        field(rows$source_term[[i]]), field(rows$level[[i]]), field(rows$reference_level[[i]]),
        field(rows$term_type[[i]]), safe_num(rows$estimate[[i]]), safe_num(rows$std_error[[i]]),
        field(rows$statistic_name[[i]]), safe_num(rows$statistic[[i]]), safe_num(rows$p_value[[i]]),
        safe_num(rows$ci_lower[[i]]), safe_num(rows$ci_upper[[i]]),
        sep = "\t")
    cat("\n")
	  }
	}
	mf <- stats::model.frame(fit)
	mf_rows <- suppressWarnings(as.integer(rownames(mf)))
	if (length(mf_rows) != length(stats::fitted(fit)) || any(is.na(mf_rows))) {
	  mf_rows <- seq_along(stats::fitted(fit))
	}
	diag_row_ids <- row_ids[mf_rows]
	observed <- suppressWarnings(as.numeric(stats::model.response(mf)))
	fitted_values <- unname(stats::fitted(fit))
	dev_resid <- unname(stats::residuals(fit, type = "deviance"))
	pearson_resid <- unname(stats::residuals(fit, type = "pearson"))
	working_resid <- tryCatch(unname(stats::residuals(fit, type = "working")), error = function(e) rep(NA_real_, length(fitted_values)))
	leverage <- tryCatch(unname(stats::hatvalues(fit)), error = function(e) rep(NA_real_, length(fitted_values)))
	cooks <- tryCatch(unname(stats::cooks.distance(fit)), error = function(e) rep(NA_real_, length(fitted_values)))
	for (i in seq_along(fitted_values)) {
	  cat("DIAG", as.integer(diag_row_ids[[i]]), safe_num(observed[[i]]), safe_num(fitted_values[[i]]),
	      safe_num(dev_resid[[i]]), safe_num(pearson_resid[[i]]), safe_num(working_resid[[i]]),
	      safe_num(leverage[[i]]), safe_num(cooks[[i]]), sep = "\t")
	  cat("\n")
	}
	)RSCRIPT";
}

bool ReadIntVectorPayload(const std::vector<std::string> &lines,
                          std::size_t &cursor,
                          std::vector<int> &out)
{
    if (cursor >= lines.size()) return false;
    long count = std::strtol(lines[cursor++].c_str(), nullptr, 10);
    if (count < 0 || cursor + static_cast<std::size_t>(count) > lines.size()) return false;
    out.clear();
    out.reserve(static_cast<std::size_t>(count));
    for (long i = 0; i < count; ++i) {
        out.push_back(std::atoi(lines[cursor++].c_str()));
    }
    return true;
}

bool ReadLinearCoefficientRowsPayload(const std::vector<std::string> &lines,
                                      std::size_t &cursor,
                                      std::vector<GLMCoefficientRow> &rows)
{
    if (cursor >= lines.size()) return false;
    long count = std::strtol(lines[cursor++].c_str(), nullptr, 10);
    if (count < 0) return false;
    rows.clear();
    rows.reserve(static_cast<std::size_t>(count));
    for (long i = 0; i < count; ++i) {
        if (cursor + 14 > lines.size()) return false;
        GLMCoefficientRow row;
        row.term = lines[cursor++];
        row.sourceTerm = lines[cursor++];
        row.termType = lines[cursor++];
        row.rowType = lines[cursor++];
        row.displayLabel = lines[cursor++];
        row.factorLevel = lines[cursor++];
        row.referenceLevel = lines[cursor++];
        row.estimate = ParseOptionalDataCellDouble(lines[cursor++]);
        row.standardizedBeta = ParseOptionalDataCellDouble(lines[cursor++]);
        row.stdError = ParseOptionalDataCellDouble(lines[cursor++]);
        row.tValue = ParseOptionalDataCellDouble(lines[cursor++]);
        row.pValue = ParseOptionalDataCellDouble(lines[cursor++]);
        row.partialR = ParseOptionalDataCellDouble(lines[cursor++]);
        row.deltaR2 = ParseOptionalDataCellDouble(lines[cursor++]);
        rows.push_back(row);
    }
    return true;
}

bool ReadLinearFitPayload(const std::vector<std::string> &lines,
                          std::size_t &cursor,
                          GLMFitSummary &fit)
{
    if (cursor + 18 > lines.size()) return false;
    fit.ok = lines[cursor++] == "TRUE";
    fit.n = ParseOptionalDataCellInt(lines[cursor++]);
    fit.excluded = ParseOptionalDataCellInt(lines[cursor++]);
    fit.dfModel = ParseOptionalDataCellInt(lines[cursor++]);
    fit.dfResidual = ParseOptionalDataCellInt(lines[cursor++]);
    fit.r2 = ParseOptionalDataCellDouble(lines[cursor++]);
    fit.adjR2 = ParseOptionalDataCellDouble(lines[cursor++]);
    fit.globalF = ParseOptionalDataCellDouble(lines[cursor++]);
    fit.globalP = ParseOptionalDataCellDouble(lines[cursor++]);
    fit.ssRegression = ParseOptionalDataCellDouble(lines[cursor++]);
    fit.ssResidual = ParseOptionalDataCellDouble(lines[cursor++]);
    fit.msRegression = ParseOptionalDataCellDouble(lines[cursor++]);
    fit.msResidual = ParseOptionalDataCellDouble(lines[cursor++]);
    fit.rmse = ParseOptionalDataCellDouble(lines[cursor++]);
    fit.sigma = ParseOptionalDataCellDouble(lines[cursor++]);
    fit.aic = ParseOptionalDataCellDouble(lines[cursor++]);
    fit.bic = ParseOptionalDataCellDouble(lines[cursor++]);
    fit.warning = lines[cursor++];
    if (!ReadIntVectorPayload(lines, cursor, fit.rowsUsed)) return false;
    if (!ReadIntVectorPayload(lines, cursor, fit.rowsExcluded)) return false;
    if (!ReadLinearCoefficientRowsPayload(lines, cursor, fit.coefficients)) return false;
    return true;
}

bool ReadLinearDiagnosticsPayload(const std::vector<std::string> &lines,
                                  std::size_t &cursor,
                                  std::vector<GLMDiagnosticRow> &diagnostics)
{
    if (cursor >= lines.size()) return false;
    long count = std::strtol(lines[cursor++].c_str(), nullptr, 10);
    if (count < 0) return false;
    diagnostics.clear();
    diagnostics.reserve(static_cast<std::size_t>(count));
    for (long i = 0; i < count; ++i) {
        if (cursor + 9 > lines.size()) return false;
        GLMDiagnosticRow row;
        row.row = ParseOptionalDataCellInt(lines[cursor++]);
        row.observed = ParseOptionalDataCellDouble(lines[cursor++]);
        row.fitted = ParseOptionalDataCellDouble(lines[cursor++]);
        row.residual = ParseOptionalDataCellDouble(lines[cursor++]);
        row.standardizedResidual = ParseOptionalDataCellDouble(lines[cursor++]);
        row.studentizedResidual = ParseOptionalDataCellDouble(lines[cursor++]);
        row.leverage = ParseOptionalDataCellDouble(lines[cursor++]);
        row.cooksDistance = ParseOptionalDataCellDouble(lines[cursor++]);
        row.sqrtAbsStandardizedResidual = ParseOptionalDataCellDouble(lines[cursor++]);
        diagnostics.push_back(row);
    }
    if (cursor < lines.size() && lines[cursor] == "LINEAR_QQ_V1") {
        ++cursor;
        if (cursor + diagnostics.size() > lines.size()) return false;
        for (auto &row : diagnostics) {
            const std::string encoded = lines[cursor++];
            const std::size_t first = encoded.find('\x1f');
            const std::size_t second = first == std::string::npos
                ? std::string::npos : encoded.find('\x1f', first + 1);
            if (first == std::string::npos || second == std::string::npos ||
                encoded.find('\x1f', second + 1) != std::string::npos) return false;
            row.qqRawQuantile = ParseOptionalDataCellDouble(encoded.substr(0, first));
            row.qqStandardizedQuantile = ParseOptionalDataCellDouble(
                encoded.substr(first + 1, second - first - 1));
            row.qqStudentizedQuantile = ParseOptionalDataCellDouble(encoded.substr(second + 1));
        }
    }
    return true;
}

bool ReadLinearMIDiagnosticsPayload(const std::vector<std::string> &lines,
                                    std::size_t &cursor,
                                    GLMFitSummary &fit)
{
    if (cursor >= lines.size() || lines[cursor] != "LINEAR_MI_DIAGNOSTICS_V1") {
        return true;
    }
    ++cursor;
    if (cursor >= lines.size()) return false;
    const long count = std::strtol(lines[cursor++].c_str(), nullptr, 10);
    if (count < 0) return false;
    fit.diagnosticsByImputation.clear();
    fit.diagnosticsByImputation.reserve(static_cast<std::size_t>(count));
    for (long index = 0; index < count; ++index) {
        std::vector<GLMDiagnosticRow> diagnostics;
        if (!ReadLinearDiagnosticsPayload(lines, cursor, diagnostics)) return false;
        fit.diagnosticsByImputation.push_back(std::move(diagnostics));
    }
    if (fit.diagnostics.empty()) {
        const auto first = std::find_if(
            fit.diagnosticsByImputation.begin(), fit.diagnosticsByImputation.end(),
            [](const std::vector<GLMDiagnosticRow> &rows) { return !rows.empty(); });
        if (first != fit.diagnosticsByImputation.end()) fit.diagnostics = *first;
    }
    return true;
}

bool ReadLinearDesignPayload(const std::vector<std::string> &lines,
                             std::size_t &cursor,
                             GLMFitSummary &fit)
{
    if (cursor >= lines.size()) return false;
    long count = std::strtol(lines[cursor++].c_str(), nullptr, 10);
    if (count < 0) return false;
    fit.designLabels.clear();
    fit.beta.clear();
    fit.covariance.clear();
    fit.factorCodings.clear();
    fit.predictorCenters.clear();
    if (count > 0) {
        if (cursor + static_cast<std::size_t>(count) * 2 > lines.size()) return false;
        fit.designLabels.reserve(static_cast<std::size_t>(count));
        for (long i = 0; i < count; ++i) {
            fit.designLabels.push_back(lines[cursor++]);
        }
        fit.beta.reserve(static_cast<std::size_t>(count));
        for (long i = 0; i < count; ++i) {
            fit.beta.push_back(ParseOptionalDataCellDouble(lines[cursor++]));
        }
        if (cursor + static_cast<std::size_t>(count) * static_cast<std::size_t>(count) > lines.size()) {
            return false;
        }
        fit.covariance.assign(static_cast<std::size_t>(count),
                              std::vector<double>(static_cast<std::size_t>(count), NAN));
        for (long i = 0; i < count; ++i) {
            for (long j = 0; j < count; ++j) {
                fit.covariance[static_cast<std::size_t>(i)][static_cast<std::size_t>(j)] =
                    ParseOptionalDataCellDouble(lines[cursor++]);
            }
        }
    }

    // Optional, versioned tail.  Older R packages stop after the covariance
    // matrix, so only consume this block when its marker is present.
    if (cursor < lines.size() && lines[cursor] == "FACTOR_CODINGS") {
        ++cursor;
        if (cursor >= lines.size()) return false;
        const long factorCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
        if (factorCount < 0) return false;
        fit.factorCodings.reserve(static_cast<std::size_t>(factorCount));
        for (long factorIndex = 0; factorIndex < factorCount; ++factorIndex) {
            if (cursor + 4 > lines.size()) return false;
            GLMFactorCoding coding;
            coding.variable = lines[cursor++];
            coding.referenceLevel = lines[cursor++];
            const long levelCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
            const long contrastCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
            if (levelCount < 0 || contrastCount < 0) return false;
            const std::size_t needed = static_cast<std::size_t>(levelCount) +
                static_cast<std::size_t>(contrastCount) +
                static_cast<std::size_t>(levelCount) * static_cast<std::size_t>(contrastCount);
            if (cursor + needed > lines.size()) return false;
            coding.levels.reserve(static_cast<std::size_t>(levelCount));
            for (long i = 0; i < levelCount; ++i) coding.levels.push_back(lines[cursor++]);
            coding.contrastLabels.reserve(static_cast<std::size_t>(contrastCount));
            for (long i = 0; i < contrastCount; ++i) coding.contrastLabels.push_back(lines[cursor++]);
            coding.coding.assign(static_cast<std::size_t>(levelCount),
                                 std::vector<double>(static_cast<std::size_t>(contrastCount), NAN));
            for (long i = 0; i < levelCount; ++i) {
                for (long j = 0; j < contrastCount; ++j) {
                    coding.coding[static_cast<std::size_t>(i)][static_cast<std::size_t>(j)] =
                        ParseOptionalDataCellDouble(lines[cursor++]);
                }
            }
            fit.factorCodings.push_back(std::move(coding));
        }
    }
    if (cursor < lines.size() && lines[cursor] == "PREDICTOR_CENTERS") {
        ++cursor;
        if (cursor >= lines.size()) return false;
        const long centerCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
        if (centerCount < 0 ||
            cursor + static_cast<std::size_t>(centerCount) * 2 > lines.size()) return false;
        for (long index = 0; index < centerCount; ++index) {
            const std::string variable = lines[cursor++];
            const double center = ParseOptionalDataCellDouble(lines[cursor++]);
            if (!variable.empty() && std::isfinite(center)) {
                fit.predictorCenters[variable] = center;
            }
        }
    }
    return true;
}

const GLMFactorCoding *FactorCodingForVariable(const GLMFitSummary &fit,
                                               const std::string &variable)
{
    for (const GLMFactorCoding &coding : fit.factorCodings) {
        if (coding.variable == variable ||
            BaseVariableForTermComponent(coding.variable) == variable) {
            return &coding;
        }
    }
    return nullptr;
}

bool ApplyFittedFactorCoding(const GLMFitSummary &fit,
                             const std::string &variable,
                             ScenarioVariableInfo &info)
{
    const GLMFactorCoding *coding = FactorCodingForVariable(fit, variable);
    if (!coding || coding->levels.empty() || coding->coding.size() != coding->levels.size()) {
        return false;
    }
    info.type = "factor";
    info.factorLevels = coding->levels;
    info.factorReferenceLevel = coding->referenceLevel;
    info.factorCoding.clear();
    for (std::size_t i = 0; i < coding->levels.size(); ++i) {
        info.factorCoding[coding->levels[i]] = coding->coding[i];
    }
    return true;
}

bool ReadGeneralizedRowsPayload(const std::vector<std::string> &lines,
                                std::size_t &cursor,
                                std::vector<GeneralizedGLMRow> &rows)
{
    if (cursor >= lines.size()) return false;
    bool componentAware = false;
    if (lines[cursor] == "GENERALIZED_ROWS_V2") {
        componentAware = true;
        ++cursor;
        if (cursor >= lines.size()) return false;
    }
    long count = std::strtol(lines[cursor++].c_str(), nullptr, 10);
    if (count < 0) return false;
    rows.clear();
    rows.reserve(static_cast<std::size_t>(count));
    for (long i = 0; i < count; ++i) {
        if (cursor + (componentAware ? 13 : 12) > lines.size()) return false;
        GeneralizedGLMRow row;
        row.term = lines[cursor++];
        row.sourceTerm = lines[cursor++];
        row.termType = lines[cursor++];
        row.rowType = lines[cursor++];
        row.displayLabel = lines[cursor++];
        row.factorLevel = lines[cursor++];
        row.referenceLevel = lines[cursor++];
        row.estimate = ParseOptionalDataCellDouble(lines[cursor++]);
        row.stdError = ParseOptionalDataCellDouble(lines[cursor++]);
        row.statisticName = lines[cursor++];
        row.statistic = ParseOptionalDataCellDouble(lines[cursor++]);
        row.pValue = ParseOptionalDataCellDouble(lines[cursor++]);
        if (componentAware) row.component = lines[cursor++];
        rows.push_back(row);
    }
    return true;
}

bool ReadGeneralizedFitPayload(const std::vector<std::string> &lines,
                               std::size_t &cursor,
                               GeneralizedGLMFitSummary &fit)
{
    if (cursor + 12 > lines.size()) return false;
    fit.ok = lines[cursor++] == "TRUE";
    fit.n = ParseOptionalDataCellInt(lines[cursor++]);
    fit.excluded = ParseOptionalDataCellInt(lines[cursor++]);
    fit.nullDeviance = ParseOptionalDataCellDouble(lines[cursor++]);
    fit.residualDeviance = ParseOptionalDataCellDouble(lines[cursor++]);
    fit.dfResidual = ParseOptionalDataCellInt(lines[cursor++]);
    fit.aic = ParseOptionalDataCellDouble(lines[cursor++]);
    fit.bic = ParseOptionalDataCellDouble(lines[cursor++]);
    fit.dispersion = ParseOptionalDataCellDouble(lines[cursor++]);
    fit.logLik = ParseOptionalDataCellDouble(lines[cursor++]);
    fit.statisticName = lines[cursor++];
    fit.status = lines[cursor++];
    if (!ReadIntVectorPayload(lines, cursor, fit.rowsUsed)) return false;
    if (!ReadIntVectorPayload(lines, cursor, fit.rowsExcluded)) return false;
    if (!ReadGeneralizedRowsPayload(lines, cursor, fit.rows)) return false;
    return true;
}

bool ReadGeneralizedStatePayload(const std::vector<std::string> &lines,
                                 std::size_t &cursor,
                                 GeneralizedGLMState &state)
{
    GeneralizedGLMFitSummary fit;
    if (!ReadGeneralizedFitPayload(lines, cursor, fit)) {
        return false;
    }
    state.ok = fit.ok;
    state.n = fit.n;
    state.excluded = fit.excluded;
    state.nullDeviance = fit.nullDeviance;
    state.residualDeviance = fit.residualDeviance;
    state.dfResidual = fit.dfResidual;
    state.aic = fit.aic;
    state.bic = fit.bic;
    state.dispersion = fit.dispersion;
    state.logLik = fit.logLik;
    state.statisticName = fit.statisticName;
    state.status = fit.status;
    state.rowsUsed = fit.rowsUsed;
    state.rowsExcluded = fit.rowsExcluded;
    state.rows = fit.rows;
    // Payloads written before GENERALIZED_META_V1 did not transport the
    // convergence flag.  A successful fit is the safest legacy fallback;
    // current payloads immediately replace it with stats::glm() metadata.
    state.converged = fit.ok;
    if (cursor < lines.size() && lines[cursor] == "GENERALIZED_CI_V1") {
        ++cursor;
        if (cursor >= lines.size()) return false;
        long coefficientCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
        if (coefficientCount < 0 ||
            cursor + static_cast<std::size_t>(coefficientCount) * 2 > lines.size()) {
            return false;
        }
        for (long index = 0; index < coefficientCount; ++index) {
            double ciLower = ParseOptionalDataCellDouble(lines[cursor++]);
            double ciUpper = ParseOptionalDataCellDouble(lines[cursor++]);
            if (static_cast<std::size_t>(index) < state.rows.size()) {
                state.rows[static_cast<std::size_t>(index)].ciLower = ciLower;
                state.rows[static_cast<std::size_t>(index)].ciUpper = ciUpper;
            }
        }
    }
    if (cursor < lines.size() && lines[cursor] == "GENERALIZED_EXPONENTIATED_V1") {
        ++cursor;
        if (cursor >= lines.size()) return false;
        long coefficientCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
        if (coefficientCount < 0 ||
            cursor + static_cast<std::size_t>(coefficientCount) * 3 > lines.size()) {
            return false;
        }
        for (long index = 0; index < coefficientCount; ++index) {
            const double estimate = ParseOptionalDataCellDouble(lines[cursor++]);
            const double lower = ParseOptionalDataCellDouble(lines[cursor++]);
            const double upper = ParseOptionalDataCellDouble(lines[cursor++]);
            if (static_cast<std::size_t>(index) < state.rows.size()) {
                auto &row = state.rows[static_cast<std::size_t>(index)];
                row.exponentiatedEstimate = estimate;
                row.exponentiatedLower = lower;
                row.exponentiatedUpper = upper;
            }
        }
    }
    if (cursor < lines.size() && lines[cursor] == "GENERALIZED_META_V1") {
        ++cursor;
        if (cursor + 7 > lines.size()) return false;
        state.converged = lines[cursor++] == "TRUE";
        state.iterations = ParseOptionalDataCellInt(lines[cursor++]);
        state.boundary = lines[cursor++] == "TRUE";
        state.rank = ParseOptionalDataCellInt(lines[cursor++]);
        state.parameterCount = ParseOptionalDataCellInt(lines[cursor++]);
        state.rankDeficient = lines[cursor++] == "TRUE";
        long warningCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
        if (warningCount < 0 || cursor + static_cast<std::size_t>(warningCount) > lines.size()) return false;
        state.warnings.clear();
        for (long index = 0; index < warningCount; ++index) {
            state.warnings.push_back(lines[cursor++]);
        }
    }
    if (cursor < lines.size() && (lines[cursor] == "COUNT_V1" ||
                                 lines[cursor] == "COUNT_V2" ||
                                 lines[cursor] == "COUNT_V3" ||
                                 lines[cursor] == "COUNT_V4" ||
                                 lines[cursor] == "COUNT_V5" ||
                                 lines[cursor] == "COUNT_V6" ||
                                 lines[cursor] == "COUNT_V7")) {
        const bool extendedDiagnostics = lines[cursor] == "COUNT_V2" ||
                                         lines[cursor] == "COUNT_V3" ||
                                         lines[cursor] == "COUNT_V4" ||
                                         lines[cursor] == "COUNT_V5" ||
                                         lines[cursor] == "COUNT_V6" ||
                                         lines[cursor] == "COUNT_V7";
        const bool thetaSummary = lines[cursor] == "COUNT_V3" ||
                                  lines[cursor] == "COUNT_V4" ||
                                  lines[cursor] == "COUNT_V5" ||
                                  lines[cursor] == "COUNT_V6" ||
                                  lines[cursor] == "COUNT_V7";
        const bool boundedCountTrials = lines[cursor] == "COUNT_V4" ||
                                        lines[cursor] == "COUNT_V5" ||
                                        lines[cursor] == "COUNT_V6" ||
                                        lines[cursor] == "COUNT_V7";
        const bool ceilingSummary = lines[cursor] == "COUNT_V5" ||
                                    lines[cursor] == "COUNT_V6" ||
                                    lines[cursor] == "COUNT_V7";
        const bool distributionResiduals = lines[cursor] == "COUNT_V6" ||
                                           lines[cursor] == "COUNT_V7";
        const bool hurdleStatus = lines[cursor] == "COUNT_V7";
        ++cursor;
        if (cursor + (thetaSummary ? (boundedCountTrials ? 11 : 9) : 6) +
                (ceilingSummary ? (hurdleStatus ? 13 : 7) : 0) > lines.size()) return false;
        state.countRegression = true;
        if (!ParseCountDistribution(lines[cursor++], state.countDistribution)) return false;
        state.exposure = lines[cursor++];
        if (boundedCountTrials) {
            state.trialsVariable = lines[cursor++];
            state.trialsConstant = ParseOptionalDataCellDouble(lines[cursor++]);
        }
        state.likelihoodAvailable = lines[cursor++] == "TRUE";
        state.likelihoodRatioAvailable = lines[cursor++] == "TRUE";
        state.theta = ParseOptionalDataCellDouble(lines[cursor++]);
        state.thetaDescriptiveMean = NAN;
        state.thetaDescriptiveMin = NAN;
        state.thetaDescriptiveMax = NAN;
        if (thetaSummary) {
            state.thetaDescriptiveMean = ParseOptionalDataCellDouble(lines[cursor++]);
            state.thetaDescriptiveMin = ParseOptionalDataCellDouble(lines[cursor++]);
            state.thetaDescriptiveMax = ParseOptionalDataCellDouble(lines[cursor++]);
        } else if (std::isfinite(state.theta)) {
            state.thetaDescriptiveMean = state.theta;
            state.thetaDescriptiveMin = state.theta;
            state.thetaDescriptiveMax = state.theta;
        }
        state.perfectScoreCount = NAN;
        state.nonPerfectScoreCount = NAN;
        state.observedCeilingProportion = NAN;
        state.predictedCeilingProportion = NAN;
        state.predictedCeilingCount = NAN;
        state.observedCeilingCountConstant = true;
        state.perfectScoreComponentStatus.clear();
        state.belowCeilingComponentStatus.clear();
        state.hurdleSuccessfulImputations = 0;
        state.hurdleAttemptedImputations = 0;
        state.betaBinomialDispersion = NAN;
        state.betaBinomialDispersionMin = NAN;
        state.betaBinomialDispersionMax = NAN;
        if (ceilingSummary) {
            state.perfectScoreCount = ParseOptionalDataCellDouble(lines[cursor++]);
            state.nonPerfectScoreCount = ParseOptionalDataCellDouble(lines[cursor++]);
            state.observedCeilingProportion = ParseOptionalDataCellDouble(lines[cursor++]);
            state.predictedCeilingProportion = ParseOptionalDataCellDouble(lines[cursor++]);
            if (hurdleStatus) {
                state.predictedCeilingCount = ParseOptionalDataCellDouble(lines[cursor++]);
                state.observedCeilingCountConstant = lines[cursor++] == "TRUE";
                state.perfectScoreComponentStatus = lines[cursor++];
                state.belowCeilingComponentStatus = lines[cursor++];
                state.hurdleSuccessfulImputations = ParseOptionalDataCellInt(lines[cursor++]);
                state.hurdleAttemptedImputations = ParseOptionalDataCellInt(lines[cursor++]);
            }
            state.betaBinomialDispersion = ParseOptionalDataCellDouble(lines[cursor++]);
            state.betaBinomialDispersionMin = ParseOptionalDataCellDouble(lines[cursor++]);
            state.betaBinomialDispersionMax = ParseOptionalDataCellDouble(lines[cursor++]);
        }
        fit.likelihoodAvailable = state.likelihoodAvailable;
        fit.likelihoodRatioAvailable = state.likelihoodRatioAvailable;
        fit.theta = state.theta;
        long coefficientCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
        if (coefficientCount < 0 ||
            cursor + static_cast<std::size_t>(coefficientCount) * 3 > lines.size()) return false;
        for (long index = 0; index < coefficientCount; ++index) {
            double values[3];
            for (double &value : values) value = ParseOptionalDataCellDouble(lines[cursor++]);
            if (static_cast<std::size_t>(index) < state.rows.size()) {
                GeneralizedGLMRow &row = state.rows[static_cast<std::size_t>(index)];
                row.rateRatio = values[0];
                row.rateRatioLower = values[1];
                row.rateRatioUpper = values[2];
            }
        }
        if (cursor >= lines.size()) return false;
        long diagnosticCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
        const std::size_t diagnosticWidth = distributionResiduals ? 13 :
            (extendedDiagnostics ? 11 : 9);
        if (diagnosticCount < 0 ||
            cursor + static_cast<std::size_t>(diagnosticCount) * diagnosticWidth > lines.size()) return false;
        state.diagnostics.clear();
        for (long index = 0; index < diagnosticCount; ++index) {
            GeneralizedDiagnosticRow row;
            row.row = ParseOptionalDataCellInt(lines[cursor++]);
            row.observed = ParseOptionalDataCellDouble(lines[cursor++]);
            row.fitted = ParseOptionalDataCellDouble(lines[cursor++]);
            row.linearPredictor = ParseOptionalDataCellDouble(lines[cursor++]);
            if (distributionResiduals) {
                row.dunnSmythResidual = ParseOptionalDataCellDouble(lines[cursor++]);
                row.rawResidual = ParseOptionalDataCellDouble(lines[cursor++]);
            }
            row.devianceResidual = ParseOptionalDataCellDouble(lines[cursor++]);
            row.pearsonResidual = ParseOptionalDataCellDouble(lines[cursor++]);
            row.workingResidual = ParseOptionalDataCellDouble(lines[cursor++]);
            if (extendedDiagnostics) {
                row.standardizedResidual = ParseOptionalDataCellDouble(lines[cursor++]);
                row.studentizedResidual = ParseOptionalDataCellDouble(lines[cursor++]);
            }
            row.leverage = ParseOptionalDataCellDouble(lines[cursor++]);
            row.cooksDistance = ParseOptionalDataCellDouble(lines[cursor++]);
            state.diagnostics.push_back(row);
        }
    }
    if (cursor < lines.size() && (lines[cursor] == "BINARY_V1" ||
                                 lines[cursor] == "BINARY_V2" ||
                                 lines[cursor] == "BINARY_V3" ||
                                 lines[cursor] == "BINARY_V4")) {
        bool extendedBinaryMetadata = lines[cursor] == "BINARY_V2" ||
                                      lines[cursor] == "BINARY_V3" ||
                                      lines[cursor] == "BINARY_V4";
        bool extendedDiagnostics = lines[cursor] == "BINARY_V3" ||
                                   lines[cursor] == "BINARY_V4";
        bool distributionResiduals = lines[cursor] == "BINARY_V4";
        ++cursor;
        if (cursor + 15 > lines.size()) return false;
        state.binaryRegression = true;
        ParseBinaryLink(state.link, state.binaryLink);
        state.responseCoding.ok = fit.ok;
        state.responseCoding.eventValue = lines[cursor++];
        state.responseCoding.eventLabel = state.responseCoding.eventValue;
        state.responseCoding.referenceValue = lines[cursor++];
        state.responseCoding.referenceLabel = state.responseCoding.referenceValue;
        state.responseCoding.eventCount = ParseOptionalDataCellInt(lines[cursor++]);
        state.responseCoding.referenceCount = ParseOptionalDataCellInt(lines[cursor++]);
        state.responseCodingExplicit = true;
        state.dfModel = ParseOptionalDataCellInt(lines[cursor++]);
        state.globalLR = ParseOptionalDataCellDouble(lines[cursor++]);
        state.globalP = ParseOptionalDataCellDouble(lines[cursor++]);
        state.mcfaddenR2 = ParseOptionalDataCellDouble(lines[cursor++]);
        state.coxSnellR2 = ParseOptionalDataCellDouble(lines[cursor++]);
        state.nagelkerkeR2 = ParseOptionalDataCellDouble(lines[cursor++]);
        state.auc = ParseOptionalDataCellDouble(lines[cursor++]);
        state.calibrationIntercept = ParseOptionalDataCellDouble(lines[cursor++]);
        state.calibrationSlope = ParseOptionalDataCellDouble(lines[cursor++]);
        state.converged = lines[cursor++] == "TRUE";
        if (extendedBinaryMetadata) {
            if (cursor + 5 > lines.size()) return false;
            state.iterations = ParseOptionalDataCellInt(lines[cursor++]);
            state.boundary = lines[cursor++] == "TRUE";
            state.rank = ParseOptionalDataCellInt(lines[cursor++]);
            state.parameterCount = ParseOptionalDataCellInt(lines[cursor++]);
            state.rankDeficient = lines[cursor++] == "TRUE";
        }
        long warningCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
        if (warningCount < 0 || cursor + static_cast<std::size_t>(warningCount) > lines.size()) return false;
        state.warnings.clear();
        for (long index = 0; index < warningCount; ++index) state.warnings.push_back(lines[cursor++]);

        if (cursor >= lines.size()) return false;
        long coefficientCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
        if (coefficientCount < 0 || cursor + static_cast<std::size_t>(coefficientCount) * 5 > lines.size()) return false;
        for (long index = 0; index < coefficientCount; ++index) {
            double values[5];
            for (double &value : values) {
                const std::string &wire = lines[cursor++];
                if (wire.empty() || wire == "NA" || wire == "NaN") value = NAN;
                else {
                    char *end = nullptr;
                    value = std::strtod(wire.c_str(), &end);
                    if (!end || end == wire.c_str() || *end != '\0') value = NAN;
                }
            }
            if (static_cast<std::size_t>(index) < state.rows.size()) {
                GeneralizedGLMRow &row = state.rows[static_cast<std::size_t>(index)];
                row.ciLower = values[0];
                row.ciUpper = values[1];
                row.oddsRatio = values[2];
                row.oddsRatioLower = values[3];
                row.oddsRatioUpper = values[4];
            }
        }

        if (cursor >= lines.size()) return false;
        long termTestCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
        if (termTestCount < 0 || cursor + static_cast<std::size_t>(termTestCount) * 4 > lines.size()) return false;
        state.termTests.clear();
        for (long index = 0; index < termTestCount; ++index) {
            BinaryTermTestRow row;
            row.term = lines[cursor++];
            row.df = ParseOptionalDataCellInt(lines[cursor++]);
            row.statistic = ParseOptionalDataCellDouble(lines[cursor++]);
            row.pValue = ParseOptionalDataCellDouble(lines[cursor++]);
            row.method = "LR chi-square";
            state.termTests.push_back(row);
        }

        if (cursor >= lines.size()) return false;
        long diagnosticCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
        const std::size_t diagnosticWidth = distributionResiduals ? 14 :
            (extendedDiagnostics ? 12 : 10);
        if (diagnosticCount < 0 || cursor + static_cast<std::size_t>(diagnosticCount) * diagnosticWidth > lines.size()) return false;
        state.diagnostics.clear();
        for (long index = 0; index < diagnosticCount; ++index) {
            GeneralizedDiagnosticRow row;
            row.row = ParseOptionalDataCellInt(lines[cursor++]);
            row.observedLabel = lines[cursor++];
            row.observedBinary = ParseOptionalDataCellDouble(lines[cursor++]);
            row.observed = row.observedBinary;
            row.fitted = ParseOptionalDataCellDouble(lines[cursor++]);
            row.linearPredictor = ParseOptionalDataCellDouble(lines[cursor++]);
            if (distributionResiduals) {
                row.dunnSmythResidual = ParseOptionalDataCellDouble(lines[cursor++]);
                row.rawResidual = ParseOptionalDataCellDouble(lines[cursor++]);
            }
            row.devianceResidual = ParseOptionalDataCellDouble(lines[cursor++]);
            row.pearsonResidual = ParseOptionalDataCellDouble(lines[cursor++]);
            row.workingResidual = ParseOptionalDataCellDouble(lines[cursor++]);
            if (extendedDiagnostics) {
                row.standardizedResidual = ParseOptionalDataCellDouble(lines[cursor++]);
                row.studentizedResidual = ParseOptionalDataCellDouble(lines[cursor++]);
            }
            row.leverage = ParseOptionalDataCellDouble(lines[cursor++]);
            row.cooksDistance = ParseOptionalDataCellDouble(lines[cursor++]);
            state.diagnostics.push_back(row);
        }
    }
    if (cursor < lines.size() && (lines[cursor] == "GENERALIZED_DIAGNOSTICS_V1" ||
                                 lines[cursor] == "GENERALIZED_DIAGNOSTICS_V2")) {
        const bool distributionResiduals = lines[cursor] == "GENERALIZED_DIAGNOSTICS_V2";
        ++cursor;
        if (cursor >= lines.size()) return false;
        const long diagnosticCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
        const std::size_t diagnosticWidth = distributionResiduals ? 13 : 11;
        if (diagnosticCount < 0 ||
            cursor + static_cast<std::size_t>(diagnosticCount) * diagnosticWidth > lines.size()) {
            return false;
        }
        state.diagnostics.clear();
        state.diagnostics.reserve(static_cast<std::size_t>(diagnosticCount));
        for (long index = 0; index < diagnosticCount; ++index) {
            GeneralizedDiagnosticRow row;
            row.row = ParseOptionalDataCellInt(lines[cursor++]);
            row.observed = ParseOptionalDataCellDouble(lines[cursor++]);
            row.fitted = ParseOptionalDataCellDouble(lines[cursor++]);
            row.linearPredictor = ParseOptionalDataCellDouble(lines[cursor++]);
            if (distributionResiduals) {
                row.dunnSmythResidual = ParseOptionalDataCellDouble(lines[cursor++]);
                row.rawResidual = ParseOptionalDataCellDouble(lines[cursor++]);
            }
            row.devianceResidual = ParseOptionalDataCellDouble(lines[cursor++]);
            row.pearsonResidual = ParseOptionalDataCellDouble(lines[cursor++]);
            row.workingResidual = ParseOptionalDataCellDouble(lines[cursor++]);
            row.standardizedResidual = ParseOptionalDataCellDouble(lines[cursor++]);
            row.studentizedResidual = ParseOptionalDataCellDouble(lines[cursor++]);
            row.leverage = ParseOptionalDataCellDouble(lines[cursor++]);
            row.cooksDistance = ParseOptionalDataCellDouble(lines[cursor++]);
            state.diagnostics.push_back(std::move(row));
        }
    }
    if (cursor < lines.size() && (lines[cursor] == "GLOBAL_TERM_TESTS_V1" ||
                                 lines[cursor] == "GLOBAL_TERM_TESTS_V2")) {
        const bool componentAware = lines[cursor] == "GLOBAL_TERM_TESTS_V2";
        ++cursor;
        if (cursor >= lines.size()) return false;
        const long termTestCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
        const std::size_t termTestWidth = componentAware ? 8 : 7;
        if (termTestCount < 0 ||
            cursor + static_cast<std::size_t>(termTestCount) * termTestWidth > lines.size()) {
            return false;
        }
        state.termTests.clear();
        state.termTests.reserve(static_cast<std::size_t>(termTestCount));
        for (long index = 0; index < termTestCount; ++index) {
            GlobalTermTestRow row;
            row.term = lines[cursor++];
            row.coefficientNames = lines[cursor++];
            row.method = lines[cursor++];
            row.statistic = ParseOptionalDataCellDouble(lines[cursor++]);
            row.df = ParseOptionalDataCellDouble(lines[cursor++]);
            row.df2 = ParseOptionalDataCellDouble(lines[cursor++]);
            row.pValue = ParseOptionalDataCellDouble(lines[cursor++]);
            if (componentAware) row.component = lines[cursor++];
            state.termTests.push_back(std::move(row));
        }
    }
    if (cursor < lines.size() && (lines[cursor] == "GENERALIZED_MI_DIAGNOSTICS_V1" ||
                                 lines[cursor] == "GENERALIZED_MI_DIAGNOSTICS_V2" ||
                                 lines[cursor] == "GENERALIZED_MI_DIAGNOSTICS_V3" ||
                                 lines[cursor] == "GENERALIZED_MI_DIAGNOSTICS_V4")) {
        const bool extendedDiagnostics = lines[cursor] != "GENERALIZED_MI_DIAGNOSTICS_V1";
        const bool compactDiagnostics = lines[cursor] == "GENERALIZED_MI_DIAGNOSTICS_V4";
        const bool distributionResiduals = compactDiagnostics ||
            lines[cursor] == "GENERALIZED_MI_DIAGNOSTICS_V3";
        ++cursor;
        if (cursor >= lines.size()) return false;
        const long imputationCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
        if (imputationCount < 0) return false;
        state.diagnosticsByImputation.clear();
        state.diagnosticsByImputation.reserve(static_cast<std::size_t>(imputationCount));
        for (long imputation = 0; imputation < imputationCount; ++imputation) {
            if (cursor >= lines.size()) return false;
            const long diagnosticCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
            const std::size_t diagnosticWidth = distributionResiduals ? 15 :
                (extendedDiagnostics ? 13 : 11);
            if (diagnosticCount < 0 ||
                cursor + static_cast<std::size_t>(diagnosticCount) *
                    (compactDiagnostics ? 1 : diagnosticWidth) > lines.size()) {
                return false;
            }
            std::vector<GeneralizedDiagnosticRow> diagnostics;
            diagnostics.reserve(static_cast<std::size_t>(diagnosticCount));
            for (long index = 0; index < diagnosticCount; ++index) {
                std::array<std::string, 15> fields;
                if (compactDiagnostics) {
                    const std::string &encoded = lines[cursor++];
                    std::size_t start = 0;
                    for (std::size_t field = 0; field < fields.size(); ++field) {
                        const std::size_t end = encoded.find('\x1f', start);
                        if ((field + 1 < fields.size() && end == std::string::npos) ||
                            (field + 1 == fields.size() && end != std::string::npos)) {
                            return false;
                        }
                        fields[field] = encoded.substr(start,
                            end == std::string::npos ? std::string::npos : end - start);
                        start = end == std::string::npos ? encoded.size() : end + 1;
                    }
                } else {
                    for (std::size_t field = 0; field < diagnosticWidth; ++field)
                        fields[field] = lines[cursor++];
                }
                std::size_t field = 0;
                auto next = [&]() -> const std::string & { return fields[field++]; };
                GeneralizedDiagnosticRow row;
                row.row = ParseOptionalDataCellInt(next());
                row.observedLabel = next();
                row.observed = ParseOptionalDataCellDouble(next());
                row.observedBinary = ParseOptionalDataCellDouble(next());
                row.fitted = ParseOptionalDataCellDouble(next());
                row.linearPredictor = ParseOptionalDataCellDouble(next());
                if (distributionResiduals) {
                    row.dunnSmythResidual = ParseOptionalDataCellDouble(next());
                    row.rawResidual = ParseOptionalDataCellDouble(next());
                }
                row.devianceResidual = ParseOptionalDataCellDouble(next());
                row.pearsonResidual = ParseOptionalDataCellDouble(next());
                row.workingResidual = ParseOptionalDataCellDouble(next());
                if (extendedDiagnostics) {
                    row.standardizedResidual = ParseOptionalDataCellDouble(next());
                    row.studentizedResidual = ParseOptionalDataCellDouble(next());
                }
                row.leverage = ParseOptionalDataCellDouble(next());
                row.cooksDistance = ParseOptionalDataCellDouble(next());
                if (!std::isfinite(row.observed) && std::isfinite(row.observedBinary)) {
                    row.observed = row.observedBinary;
                }
                diagnostics.push_back(std::move(row));
            }
            state.diagnosticsByImputation.push_back(std::move(diagnostics));
        }
        if (state.diagnostics.empty()) {
            const auto first = std::find_if(
                state.diagnosticsByImputation.begin(), state.diagnosticsByImputation.end(),
                [](const std::vector<GeneralizedDiagnosticRow> &rows) { return !rows.empty(); });
            if (first != state.diagnosticsByImputation.end()) state.diagnostics = *first;
        }
    }
    if (cursor < lines.size() &&
        (lines[cursor] == "BOUNDED_COUNT_DISTRIBUTION_V1" ||
         lines[cursor] == "DISCRETE_RESPONSE_DISTRIBUTION_V1")) {
        ++cursor;
        auto parseDistribution = [&](std::vector<BoundedCountDistributionPoint> &values) {
            if (cursor >= lines.size()) return false;
            const long count = std::strtol(lines[cursor++].c_str(), nullptr, 10);
            if (count < 0 ||
                cursor + static_cast<std::size_t>(count) * 3 > lines.size()) {
                return false;
            }
            values.clear();
            values.reserve(static_cast<std::size_t>(count));
            for (long index = 0; index < count; ++index) {
                BoundedCountDistributionPoint point;
                point.score = ParseOptionalDataCellDouble(lines[cursor++]);
                point.observedFrequency = ParseOptionalDataCellDouble(lines[cursor++]);
                point.predictedFrequency = ParseOptionalDataCellDouble(lines[cursor++]);
                values.push_back(point);
            }
            return true;
        };
        if (!parseDistribution(state.boundedCountDistribution) || cursor >= lines.size()) {
            return false;
        }
        const long imputationCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
        if (imputationCount < 0) return false;
        state.boundedCountDistributionsByImputation.clear();
        state.boundedCountDistributionsByImputation.reserve(
            static_cast<std::size_t>(imputationCount));
        for (long imputation = 0; imputation < imputationCount; ++imputation) {
            std::vector<BoundedCountDistributionPoint> values;
            if (!parseDistribution(values)) return false;
            state.boundedCountDistributionsByImputation.push_back(std::move(values));
        }
    }
    state.diagnosticNotesByImputation.clear();
    if (cursor < lines.size() && lines[cursor] == "GENERALIZED_DIAGNOSTIC_NOTES_V1") {
        ++cursor;
        if (cursor >= lines.size()) return false;
        const long count = std::strtol(lines[cursor++].c_str(), nullptr, 10);
        if (count < 0 || static_cast<std::size_t>(count) > lines.size()-cursor) return false;
        for (long i=0; i<count; ++i) state.diagnosticNotesByImputation.push_back(lines[cursor++]);
    }
    state.familyDiagnostics.clear();
    if (cursor < lines.size() && lines[cursor] == "GENERALIZED_FAMILY_DIAGNOSTICS_V1") {
        ++cursor;
        if (cursor >= lines.size()) return false;
        const long count = std::strtol(lines[cursor++].c_str(), nullptr, 10);
        if (count < 0 || static_cast<std::size_t>(count) > (lines.size()-cursor)/2) return false;
        for (long i=0; i<count; ++i) {
            const std::string key = lines[cursor++];
            state.familyDiagnostics[key] = ParseOptionalDataCellDouble(lines[cursor++]);
        }
    }
    if (cursor < lines.size() && lines[cursor] == "GENERALIZED_QQ_V1") {
        ++cursor;
        const std::array<std::string, 7> residualTypes = {
            "dunn_smyth", "raw", "deviance", "pearson", "working",
            "standardized", "studentized"};
        auto readQuantiles = [&](std::vector<GeneralizedDiagnosticRow> &rows) {
            if (cursor >= lines.size()) return false;
            const long count = std::strtol(lines[cursor++].c_str(), nullptr, 10);
            if (count < 0 || static_cast<std::size_t>(count) != rows.size() ||
                cursor + rows.size() > lines.size()) return false;
            for (auto &row : rows) {
                const std::string &encoded = lines[cursor++];
                std::size_t start = 0;
                for (std::size_t index = 0; index < residualTypes.size(); ++index) {
                    const std::size_t end = encoded.find('\x1f', start);
                    if ((index + 1 < residualTypes.size() && end == std::string::npos) ||
                        (index + 1 == residualTypes.size() && end != std::string::npos)) {
                        return false;
                    }
                    row.qqTheoreticalQuantiles[residualTypes[index]] =
                        ParseOptionalDataCellDouble(encoded.substr(start,
                            end == std::string::npos ? std::string::npos : end - start));
                    start = end == std::string::npos ? encoded.size() : end + 1;
                }
            }
            return true;
        };
        if (!readQuantiles(state.diagnostics) || cursor >= lines.size()) return false;
        const long count = std::strtol(lines[cursor++].c_str(), nullptr, 10);
        if (count < 0 || static_cast<std::size_t>(count) !=
            state.diagnosticsByImputation.size()) return false;
        for (auto &rows : state.diagnosticsByImputation) {
            if (!readQuantiles(rows)) return false;
        }
    }
    if (cursor < lines.size() && lines[cursor] == "MODEL_TYPE_V1") {
        ++cursor;
        if (cursor >= lines.size() ||
            !ParseStatisticalModelType(lines[cursor++], state.modelType)) return false;
    } else if (state.binaryRegression || state.countRegression) {
        // Old specialized payloads predate the explicit category marker.
        state.modelType = StatisticalModelTypeForFamily(
            state.family, state.binaryRegression, state.countRegression);
    }
    if (!IsGeneralizedResidualTypeAvailable(
            state, state.diagnosticOptions.residualType)) {
        state.diagnosticOptions.residualType =
            DefaultGeneralizedResidualType(state);
    }
    // A legacy generic model deliberately remains LegacyGeneralized even when
    // its stored family happens to be Gamma or beta. This preserves the
    // editable semantics of old .linkeda documents.
    return true;
}

const GLMCoefficientRow *CoefficientForTerm(const std::vector<GLMCoefficientRow> &rows,
                                            const std::string &term)
{
    for (const GLMCoefficientRow &row : rows) {
        if (row.term == term) {
            return &row;
        }
    }
    return nullptr;
}

const GLMCoefficientRow *CoefficientForTerm(const GLMFitSummary &fit,
                                            const std::string &term)
{
    return CoefficientForTerm(fit.coefficients, term);
}

std::vector<GLMCoefficientRow> CoefficientsForSourceTerm(const GLMFitSummary &fit,
                                                         const std::string &term)
{
    std::vector<GLMCoefficientRow> rows;
    for (const GLMCoefficientRow &row : fit.coefficients) {
        if (row.sourceTerm == term) {
            rows.push_back(row);
        }
    }
    return rows;
}

std::string RegressionRowDisplayLabel(const GLMCoefficientRow *row,
                                      const std::string &fallback)
{
    if (row && !row->displayLabel.empty() && row->displayLabel != "NA") {
        return row->displayLabel;
    }
    if (row && !row->term.empty()) {
        return row->term;
    }
    return fallback;
}

std::string RegressionRowSourceTerm(const GLMCoefficientRow *row,
                                    const std::string &fallback)
{
    if (row) {
        return EffectiveLinearRowSourceTerm(*row, fallback);
    }
    return fallback;
}

std::string RegressionComparisonFitLabel(int fitRow, bool multipleImputation)
{
    static const char *ordinaryLabels[] = {
        "N", "R\u00B2", "Adjusted R\u00B2", "s", "df residual", "F", "p",
        "AIC", "BIC", "\u0394df vs prev", "\u0394SSE vs prev", "Partial F vs prev", "p vs prev"
    };
    static const char *miLabels[] = {
        "N", "R\u00B2", "Adjusted R\u00B2", "s", "df residual", "D1/Wald", "p",
        "AIC", "BIC", "\u0394df vs prev", "\u0394SSE vs prev", "D1/Wald vs prev", "p vs prev"
    };
    if (fitRow >= 0 && fitRow < 13) {
        return multipleImputation ? miLabels[fitRow] : ordinaryLabels[fitRow];
    }
    switch (fitRow) {
        case 13: return "\u0394R\u00B2 vs prev";
        case 14: return "\u0394 adjusted R\u00B2 vs prev";
        case 15: return "\u0394s vs prev";
        case 16: return "\u0394AIC";
        case 17: return "\u0394BIC";
        default: return "";
    }
}

bool RegressionComparisonFitRowIsModelTest(int fitRow)
{
    return (fitRow >= 9 && fitRow <= 15);
}

std::vector<int> RegressionComparisonVisibleFitRows(bool showInformationCriteria)
{
    std::vector<int> rows = {0, 1, 2, 3, 4};
    if (showInformationCriteria) {
        rows.insert(rows.end(), {7, 8, 16, 17});
    }
    rows.insert(rows.end(), {13, 14, 15, 9, 11, 12});
    return rows;
}

std::vector<int> GeneralizedComparisonVisibleFitRows(bool binaryComparison,
                                                     bool showInformationCriteria)
{
    std::vector<int> rows = binaryComparison
        ? std::vector<int>{2, 3, 9}
        : std::vector<int>{2, 4, 5};
    if (showInformationCriteria) {
        const std::vector<int> informationRows = binaryComparison
            ? std::vector<int>{4, 5, 14, 15}
            : std::vector<int>{6, 7, 14, 15};
        rows.insert(rows.end(), informationRows.begin(), informationRows.end());
    }
    rows.insert(rows.end(), {10, 11, 12, 13});
    return rows;
}

std::vector<int> GeneralizedComparisonVisibleFitRows(const GeneralizedComparisonState &state)
{
    if (!state.countComparison) {
        return GeneralizedComparisonVisibleFitRows(
            state.binaryComparison, state.showInformationCriteria);
    }
    // Count comparisons expose the count-specific semantic state for every
    // model column.  Capability checks in GeneralizedComparisonFitCell keep
    // quasi-likelihood models from displaying likelihood-only statistics.
    std::vector<int> rows = {16, 1, 17, 2, 4, 5, 9, 6, 18};
    if (state.showInformationCriteria) rows.insert(rows.end(), {7, 14, 15});
    rows.insert(rows.end(), {10, 11, 12, 13});
    return rows;
}

std::string GeneralizedComparisonFitLabel(int fitRow)
{
    static const char *labels[] = {
        "Distribution", "Link", "N", "Null dev.", "Residual dev.", "df residual",
        "AIC", "BIC", "Dispersion", "logLik", "\u0394df vs prev",
        "\u0394 deviance", "\u03C7\u00B2/F vs prev", "p vs prev"
    };
    if (fitRow >= 0 && fitRow < 14) return labels[fitRow];
    if (fitRow == 14) return "\u0394AIC";
    if (fitRow == 15) return "\u0394BIC";
    if (fitRow == 16) return "Distribution";
    if (fitRow == 17) return "Exposure (log offset) / Trials";
    if (fitRow == 18) return "Distribution parameter";
    return "";
}

std::string RegressionComparisonFootnote()
{
    return "* p < .05, ** p < .01, *** p < .001. Partial F uses \u0394SSE vs previous model, not \u0394 global F.";
}

std::string GeneralizedComparisonFootnote()
{
    return "* p < .05, ** p < .01, *** p < .001. Model tests compare with previous column.";
}

std::string CountRegressionComparisonFootnote()
{
    return "* p < .05, ** p < .01, *** p < .001. Nested likelihood-ratio tests require the same distribution, exposure, and analysis rows. Likelihood, AIC, BIC, and likelihood-ratio tests are unavailable for quasi-Poisson models.";
}

std::string RegressionComparisonFitDisplay(const GLMFitSummary &fit,
                                           const NestedModelTestResult &test,
                                           int fitRow)
{
    if (!fit.ok) return "\u2014";
    switch (fitRow) {
        case 0: return std::to_string(fit.n);
        case 1: return FormatPercentOrDash(fit.r2, 1);
        case 2: return FormatPercentOrDash(fit.adjR2, 1);
        case 3: return FormatDoubleOrDash(fit.sigma, 3);
        case 4: return std::to_string(fit.dfResidual);
        case 5: return FormatDoubleOrDash(fit.globalF, 3);
        case 6: return FormatPValue(fit.globalP);
        case 7: return FormatDoubleOrDash(fit.aic, 1);
        case 8: return FormatDoubleOrDash(fit.bic, 1);
        case 9: return test.ok ? std::to_string(test.df) : "\u2014";
        case 10: return test.ok ? FormatDoubleOrDash(test.delta, 3) : "\u2014";
        case 11: return test.ok ? FormatDoubleOrDash(test.statistic, 3) : "\u2014";
        case 12: return test.ok ? FormatPValue(test.p) : "\u2014";
        default: return "";
    }
}

std::string RegressionComparisonChangeDisplay(const GLMFitSummary &fit,
                                              const GLMFitSummary *previousFit,
                                              int fitRow)
{
    if (!fit.ok || !previousFit || !previousFit->ok) return "\u2014";
    double value = NAN;
    switch (fitRow) {
        case 13: value = fit.r2 - previousFit->r2; break;
        case 14: value = fit.adjR2 - previousFit->adjR2; break;
        case 15: value = fit.sigma - previousFit->sigma; break;
        default: return "";
    }
    if (!std::isfinite(value)) return "\u2014";
    return fitRow == 15 ? FormatDoubleOrDash(value, 3) : FormatPercentOrDash(value, 1);
}

std::string GeneralizedComparisonFitDisplay(const GeneralizedComparisonFitDisplayData &fit,
                                            const NestedModelTestResult &test,
                                            int fitRow)
{
    const bool miceTest = test.statisticName.find("mice::D") != std::string::npos;
    const std::string miceMethod = test.statisticName.find("mice::D3") != std::string::npos
        ? "D3" : (test.statisticName.find("mice::D1") != std::string::npos ? "D1" : "");
    switch (fitRow) {
        case 0: return fit.family;
        case 1: return fit.link;
        case 2: return fit.ok ? std::to_string(fit.n) : "\u2014";
        case 3: return fit.ok ? FormatDoubleOrDash(fit.nullDeviance, 3) : "\u2014";
        case 4: return fit.ok ? FormatDoubleOrDash(fit.residualDeviance, 3) : "\u2014";
        case 5: return fit.ok ? std::to_string(fit.dfResidual) : "\u2014";
        case 6: return fit.ok ? FormatDoubleOrDash(fit.aic, 1) : "\u2014";
        case 7: return fit.ok ? FormatDoubleOrDash(fit.bic, 1) : "\u2014";
        case 8: return fit.ok ? FormatDoubleOrDash(fit.dispersion, 3) : "\u2014";
        case 9: return fit.ok ? FormatDoubleOrDash(fit.logLik, 3) : "\u2014";
        case 10: return test.ok
            ? (miceTest && std::isfinite(test.df2)
                ? std::to_string(test.df) + " / " + FormatDoubleOrDash(test.df2, 1)
                : std::to_string(test.df))
            : "\u2014";
        case 11: return test.ok ? FormatDoubleOrDash(test.delta, 3) : "\u2014";
        case 12: return test.ok
            ? FormatDoubleOrDash(test.statistic, 3) +
                (miceMethod.empty() ? "" : " [" + miceMethod + "]")
            : "\u2014";
        case 13: return test.ok ? FormatPValue(test.p) : "\u2014";
        default: return "";
    }
}

std::string ComparisonCopyTableText(const ComparisonCopyTable &table)
{
    std::ostringstream out;
    out << table.title << "\n";
    out << "Response:\t" << table.response << "\n\n";
    out << "Term";
    for (const std::string &label : table.modelLabels) {
        out << "\t" << label;
    }
    out << "\n";
    bool responsesDiffer = false;
    if (table.modelResponses.size() == table.modelLabels.size()) {
        for (const std::string &response : table.modelResponses) {
            if (response != table.response) {
                responsesDiffer = true;
                break;
            }
        }
    }
    if (responsesDiffer) {
        out << "Response";
        for (const std::string &response : table.modelResponses) {
            out << "\t" << response;
        }
        out << "\n";
    }
    for (std::size_t r = 0; r < table.termLabels.size(); ++r) {
        out << table.termLabels[r];
        const std::vector<std::string> *cells =
            r < table.termDisplaysByRow.size() ? &table.termDisplaysByRow[r] : nullptr;
        for (std::size_t m = 0; m < table.modelLabels.size(); ++m) {
            out << "\t";
            if (cells && m < cells->size()) {
                out << (*cells)[m];
            }
        }
        out << "\n";
    }
    out << "\n";
    for (std::size_t r = 0; r < table.fitLabels.size(); ++r) {
        out << table.fitLabels[r];
        const std::vector<std::string> *cells =
            r < table.fitDisplaysByRow.size() ? &table.fitDisplaysByRow[r] : nullptr;
        for (std::size_t m = 0; m < table.modelLabels.size(); ++m) {
            out << "\t";
            if (cells && m < cells->size()) {
                out << (*cells)[m];
            }
        }
        out << "\n";
    }
    if (!table.footnote.empty()) {
        out << "\n" << table.footnote;
    }
    return out.str();
}

std::string RegressionComparisonModelTestStatus(const std::string &currentLabel,
                                                const std::string &previousLabel,
                                                const NestedModelTestResult &test,
                                                bool multipleImputation)
{
    if (!test.ok) {
        return "";
    }
    std::string statisticName = multipleImputation ? "D1/Wald statistic" : "partial F";
    return "Status: " + currentLabel + " vs " + previousLabel +
        "; \u0394df = " + std::to_string(test.df) +
        "; \u0394SSE = " + FormatDoubleOrDash(test.delta, 3) +
        "; " + statisticName + " = " + FormatDoubleOrDash(test.statistic, 3) +
        "; p " + FormatPValue(test.p);
}

std::string RegressionComparisonNoPreviousModelStatus()
{
    return "Status: the first model has no previous model to compare against.";
}

std::string RegressionComparisonUnavailableStatus()
{
    return "Status: model comparison is available only for nested models with the same response and same rows.";
}

std::string RegressionComparisonCopiedStatus()
{
    return "Status: model comparison table copied.";
}

std::string GeneralizedComparisonCopiedStatus()
{
    return "Status: generalized model comparison table copied.";
}

std::string SelectedRowsStatus(std::size_t selected, std::size_t total)
{
    return "Selected rows: " + std::to_string(selected) + " / " + std::to_string(total);
}

std::string GLMPooledTableChangeInRStatus()
{
    return "Status: this is a pooled MI table; change the model in R and reopen the table.";
}

std::string GLMValueCopiedStatus()
{
    return "Status: value copied.";
}

std::string GLMFitStatisticCopiedStatus()
{
    return "Status: fit statistic copied.";
}

std::string GLMRegressionTableCopiedStatus()
{
    return "Status: regression table copied.";
}

std::string GLMCompareLinearModelsHintStatus()
{
    return "Status: open Regression > Compare Linear Models to compare this model with a null model.";
}

std::string GLMSelectPredictorToRemoveStatus()
{
    return "Status: select a predictor to remove.";
}

std::string GLMNoAvailablePredictorsStatus()
{
    return "Status: no available predictors.";
}

std::string GLMNoAvailableReplacementStatus()
{
    return "Status: no available replacement.";
}

std::string GLMTermActionHintStatus()
{
    return "Status: select a term, type, or statistic cell for the available contextual actions.";
}

std::string GLMFittedStatus(int rows, int excluded, const std::string &scope)
{
    return "Status: fitted on " + std::to_string(rows) + " rows, " +
        std::to_string(excluded) + " excluded, scope = " + scope;
}

std::string GLMPrecomputedTableRefitStatus(const std::string &action)
{
    return "Status: precomputed tables must be refit from R to " + action + ".";
}

std::string GLMPooledMIScopeRefitStatus()
{
    return "Status: pooled MI tables must be refit from R to change scope.";
}

std::string GLMImputedDatasetUnavailableStatus()
{
    return "Status: the imputed dataset is no longer available.";
}

std::string GLMRefittingPooledMIResponseStatus(const std::string &response)
{
    return "Status: refitting pooled MI model via R/mice with Rubin's rules; response " +
        response + "...";
}

std::string GLMRefittingPooledMITermStatus(const std::string &action,
                                           const std::string &term)
{
    return "Status: refitting pooled MI model via R/mice with Rubin's rules after " +
        action + " " + term + "...";
}

std::string GLMInteractionComponentTypeStatus(const std::string &term)
{
    return "Status: " + term + " is an interaction; change the component variable types instead.";
}

std::string GLMTermTypeChangedStatus(const std::string &term,
                                     const std::string &type)
{
    return "Status: " + term + " is " +
        (type == "factor" ? "Categorical." : "Numeric.");
}

std::string GLMModelTableCopiedStatus()
{
    return "Model table copied.";
}

std::string GLMModelTableExportedStatus()
{
    return "Model table exported as PDF.";
}

std::string GLMPDFWriteFailedStatus()
{
    return "Could not write the selected PDF file.";
}

GLMTableCellDisplay GLMRegressionTableCell(const GLMCoefficientRow &row,
                                           GLMTableColumn column)
{
    const bool sectionRow = row.rowType == "section_header";
    const bool parentRow = row.rowType == "factor_parent" ||
        row.rowType == "term_parent";
    const bool referenceRow = row.rowType == "reference";
    const bool factorLevelRow = row.rowType == "factor_level";
    const bool interceptRow = row.termType == "intercept" ||
        row.term == "(Intercept)" || row.sourceTerm == "(Intercept)";
    const bool interactionChildRow = ModelTermTypeIsInteraction(row.termType) &&
        row.rowType == "coefficient" && !row.sourceTerm.empty() &&
        row.sourceTerm != row.term;
    const bool nestedRow = referenceRow || factorLevelRow || interactionChildRow;

    const auto value = [](std::string text) {
        return GLMTableCellDisplay{GLMTableDisplayState::Value, std::move(text)};
    };
    const auto blank = []() {
        return GLMTableCellDisplay{GLMTableDisplayState::Blank, {}};
    };
    const auto notApplicable = []() {
        return GLMTableCellDisplay{GLMTableDisplayState::NotApplicable, {}};
    };
    const auto unavailable = []() {
        return GLMTableCellDisplay{GLMTableDisplayState::Unavailable, {}};
    };
    const auto number = [&](double numberValue, int digits) {
        return std::isfinite(numberValue)
            ? value(FormatDouble(numberValue, digits))
            : unavailable();
    };

    switch (column) {
        case GLMTableColumn::Variable: {
            std::string label = !row.displayLabel.empty() ? row.displayLabel :
                (!row.term.empty() ? row.term : row.sourceTerm);
            return value(std::move(label));
        }
        case GLMTableColumn::Type:
            if (sectionRow) return blank();
            if (interceptRow) return value("-");
            if (nestedRow) return blank();
            return value(ModelTermTypeDisplayName(row.termType));
        case GLMTableColumn::Estimate:
            if (sectionRow) return blank();
            if (parentRow) return blank();
            if (referenceRow) return notApplicable();
            return number(row.estimate, 4);
        case GLMTableColumn::StandardizedBeta:
            if (sectionRow) return blank();
            if (parentRow) return blank();
            if (referenceRow) return notApplicable();
            if (std::isfinite(row.standardizedBeta))
                return value(FormatDouble(row.standardizedBeta, 3));
            return interceptRow || factorLevelRow ? notApplicable() : unavailable();
        case GLMTableColumn::StandardError:
            if (sectionRow) return blank();
            if (parentRow) return blank();
            if (referenceRow) return notApplicable();
            return number(row.stdError, 4);
        case GLMTableColumn::Statistic:
            if (sectionRow) return blank();
            if (parentRow) return blank();
            if (referenceRow) return notApplicable();
            return number(row.tValue, 3);
        case GLMTableColumn::PValue:
            if (sectionRow) return blank();
            if (parentRow) return std::isfinite(row.pValue)
                ? value(FormatPValue(row.pValue))
                : unavailable();
            if (referenceRow) return notApplicable();
            return std::isfinite(row.pValue)
                ? value(FormatPValue(row.pValue))
                : unavailable();
        case GLMTableColumn::PartialR:
            if (sectionRow) return blank();
            if (parentRow || referenceRow || interceptRow)
                return notApplicable();
            return number(row.partialR, 3);
        case GLMTableColumn::DeltaRSquared:
            if (sectionRow) return blank();
            if (referenceRow) return notApplicable();
            if (std::isfinite(row.deltaR2))
                return value(FormatDouble(row.deltaR2, 3));
            return factorLevelRow || interactionChildRow || interceptRow
                ? notApplicable() : unavailable();
    }
    return unavailable();
}

std::vector<GLMTableCellDisplay> GLMRegressionTableRow(
    const GLMCoefficientRow &row)
{
    std::vector<GLMTableCellDisplay> cells;
    cells.reserve(9);
    for (int column = static_cast<int>(GLMTableColumn::Variable);
         column <= static_cast<int>(GLMTableColumn::DeltaRSquared); ++column) {
        cells.push_back(GLMRegressionTableCell(
            row, static_cast<GLMTableColumn>(column)));
    }
    return cells;
}

namespace {

bool GLMModelIdentifierCharacter(char value)
{
    const unsigned char byte = static_cast<unsigned char>(value);
    return std::isalnum(byte) || value == '_' || value == '.';
}

std::string MarkCenteredIdentifier(std::string label, const std::string &variable)
{
    if (variable.empty()) return label;
    const std::string replacement = variable + " (centered)";
    std::size_t cursor = 0;
    while ((cursor = label.find(variable, cursor)) != std::string::npos) {
        const bool leftBoundary = cursor == 0 ||
            !GLMModelIdentifierCharacter(label[cursor - 1]);
        const std::size_t right = cursor + variable.size();
        const bool rightBoundary = right == label.size() ||
            !GLMModelIdentifierCharacter(label[right]);
        const bool alreadyMarked = label.compare(right, 11, " (centered)") == 0;
        if (leftBoundary && rightBoundary && !alreadyMarked) {
            label.replace(cursor, variable.size(), replacement);
            cursor += replacement.size();
        } else {
            cursor = right;
        }
    }
    return label;
}

std::string PredictorCenterSummary(const std::map<std::string, double> &centers)
{
    std::ostringstream out;
    bool first = true;
    for (const auto &entry : centers) {
        if (!std::isfinite(entry.second)) continue;
        if (!first) out << "; ";
        first = false;
        out << entry.first << " (mean = " << FormatDouble(entry.second, 4) << ")";
    }
    return out.str();
}

} // namespace

std::string GLMRegressionCoefficientDisplayLabel(
    const GLMCoefficientRow &row,
    const std::set<std::string> &centeredPredictors)
{
    std::string label = !row.displayLabel.empty() ? row.displayLabel :
        (!row.term.empty() ? row.term : row.sourceTerm);
    const std::string source = !row.sourceTerm.empty() ? row.sourceTerm : row.term;
    const std::vector<std::string> interactionParts = SplitInteractionTerm(source);
    for (const std::string &variable : centeredPredictors) {
        if (source == variable ||
            std::find(interactionParts.begin(), interactionParts.end(), variable) !=
                interactionParts.end()) {
            label = MarkCenteredIdentifier(std::move(label), variable);
        }
    }
    return label;
}

std::vector<GLMTableCellDisplay> GLMRegressionTableRow(
    const GLMCoefficientRow &row,
    const std::set<std::string> &centeredPredictors)
{
    GLMCoefficientRow displayRow = row;
    displayRow.displayLabel = GLMRegressionCoefficientDisplayLabel(
        row, centeredPredictors);
    return GLMRegressionTableRow(displayRow);
}

std::string GLMTableDisplayStateId(GLMTableDisplayState state)
{
    switch (state) {
        case GLMTableDisplayState::Value: return "value";
        case GLMTableDisplayState::Blank: return "blank";
        case GLMTableDisplayState::NotApplicable: return "notApplicable";
        case GLMTableDisplayState::Unavailable: return "unavailable";
    }
    return "unavailable";
}

std::string GLMRegressionTableCellText(const GLMTableCellDisplay &cell)
{
    switch (cell.state) {
        case GLMTableDisplayState::Value:
            return cell.formattedValue;
        case GLMTableDisplayState::Blank:
            return {};
        case GLMTableDisplayState::NotApplicable:
        case GLMTableDisplayState::Unavailable:
            return "\u2014";
    }
    return "\u2014";
}

std::string GLMRegressionCopyTableText(const GLMFitSummary &fit,
                                       const std::string &dependent,
                                       bool usesPooledGlobalWald)
{
    std::ostringstream out;
    out << "General Linear Model\n";
    out << "Response:\t" << dependent << "\n\n";
    out << "Variable\tb\tβ\tSE\tt\tp\tPartial r\tΔR²\n";
    std::set<std::string> centeredPredictors;
    for (const auto &entry : fit.predictorCenters) centeredPredictors.insert(entry.first);
    for (const GLMCoefficientRow &coef : fit.coefficients) {
        const auto cells = GLMRegressionTableRow(coef, centeredPredictors);
        for (std::size_t column : std::array<std::size_t, 8>{0, 2, 3, 4, 5, 6, 7, 8}) {
            if (column != 0) out << '\t';
            out << GLMRegressionTableCellText(cells[column]);
        }
        out << "\n";
    }
    out << "\n";
    out << "N\t" << fit.n << "\n";
    out << "R²\t" << FormatPercentOrDash(fit.r2, 1) << "\n";
    out << "Adjusted R²\t" << FormatPercentOrDash(fit.adjR2, 1) << "\n";
    out << "s\t" << FormatDoubleOrDash(fit.sigma, 3) << "\n";
    out << "df residual\t" << fit.dfResidual << "\n";
    out << (usesPooledGlobalWald ? "D1/Wald" : "F") << "\t" << FormatDoubleOrDash(fit.globalF, 3) << "\n";
    out << "p\t" << FormatPValue(fit.globalP) << "\n";
    const std::string centers = PredictorCenterSummary(fit.predictorCenters);
    if (!centers.empty()) out << "Centered predictors\t" << centers << "\n";
    out << "\n* p < .05; ** p < .01; *** p < .001";
    return out.str();
}

std::string GLMModelDetailsText(const GLMFitSummary &fit,
                               bool usesPooledGlobalWald)
{
    std::ostringstream out;
    out << "Technical fit details\n\n";
    out << "N: " << fit.n << "\n";
    out << "Residual df: " << fit.dfResidual << "\n";
    out << (usesPooledGlobalWald ? "D1/Wald: " : "F: ")
        << FormatDoubleOrDash(fit.globalF, 3) << "\n";
    out << "p: " << FormatPValue(fit.globalP) << "\n";
    out << "AIC: " << FormatDoubleOrDash(fit.aic, 1) << "\n";
    out << "BIC: " << FormatDoubleOrDash(fit.bic, 1) << "\n";
    out << "Residual standard error: " << FormatDoubleOrDash(fit.sigma, 3);
    const std::string centers = PredictorCenterSummary(fit.predictorCenters);
    if (!centers.empty()) out << "\nCentered predictors: " << centers;
    return out.str();
}

std::string GeneralizedGLMModelDetailsText(const GeneralizedGLMState &state)
{
    std::ostringstream out;
    out << "Technical fit details\n\n";
    const std::string distribution = state.countRegression
        ? (state.countDistribution == CountDistribution::QuasiPoisson
            ? "Poisson" : CountDistributionLabel(state.countDistribution))
        : (FindDistributionSpecification(state.family)
            ? FindDistributionSpecification(state.family)->visibleName
            : state.family);
    out << "Distribution: " << distribution << "\n";
    if (state.countRegression && state.countDistribution == CountDistribution::QuasiPoisson)
        out << "Dispersion / inference: Quasi-Poisson\n";
    out << "Link: " << state.link << "\n";
    if (state.family == "lognormal") {
        out << "Response transformation: log(Y)\n";
        out << "Diagnostic scale: log-response scale\n";
        out << "Response-scale arithmetic means: exp(eta + residual variance / 2)\n";
    }
    if (GeneralizedFamilyRequiresResponseBounds(state.family) &&
        state.responseBoundsConfigured) {
        out << "Response bounds: [" << state.responseLower << ", "
            << state.responseUpper << "]\n";
        out << "Response transformation: "
            << (state.family == "gamma_distance"
                ? "upper - Y"
                : "(Y - lower) / (upper - lower)") << "\n";
    }
    out << "N: " << state.n << "\n";
    if (state.binaryRegression) {
        out << "Events: " << state.responseCoding.eventCount << " (" << state.responseCoding.eventLabel << ")\n";
        out << "References: " << state.responseCoding.referenceCount << " (" << state.responseCoding.referenceLabel << ")\n";
    }
    out << "Null deviance: " << FormatDoubleOrDash(state.nullDeviance, 3) << "\n";
    out << "Residual deviance: " << FormatDoubleOrDash(state.residualDeviance, 3) << "\n";
    out << "Residual df: " << state.dfResidual << "\n";
    out << "Log likelihood: " << FormatDoubleOrDash(state.logLik, 3) << "\n";
    out << "AIC: " << FormatDoubleOrDash(state.aic, 1) << "\n";
    out << "BIC: " << FormatDoubleOrDash(state.bic, 1) << "\n";
    if (state.countRegression) out << CountModelParameterSummary(state);
    else out << (state.family == "lognormal"
        ? "Residual variance (log scale): " : "Dispersion: ")
        << FormatDoubleOrDash(state.dispersion, 3);
    if (state.binaryRegression) {
        out << "\nMcFadden pseudo-R\u00B2: " << FormatDoubleOrDash(state.mcfaddenR2, 3)
            << "\nCox\u2013Snell pseudo-R\u00B2: " << FormatDoubleOrDash(state.coxSnellR2, 3)
            << "\nNagelkerke pseudo-R\u00B2: " << FormatDoubleOrDash(state.nagelkerkeR2, 3)
            << "\nApparent AUC: " << FormatDoubleOrDash(state.auc, 3)
            << "\nAUC was calculated on the same observations used to fit the model and does not represent external validation."
            << "\nConverged: " << (state.converged ? "yes" : "no")
            << "\nIterations: " << state.iterations
            << "\nRank: " << state.rank << "/" << state.parameterCount;
    }
    if (!state.warnings.empty()) {
        out << "\n\nWarnings";
        for (const std::string &warning : state.warnings) out << "\n- " << warning;
    }
    return out.str();
}

std::string GLMStatusMessage(const std::string &message)
{
    return "Status: " + message;
}

std::string GLMFitNoteWarningStatus(const std::string &note,
                                    const std::string &warning)
{
    return "Status: " + note + " Warning: " + warning;
}

std::string GLMInteractionReportOpenedStatus(const std::string &term)
{
    return "Status: interaction report opened for " + term + ".";
}

std::string GLMInteractionPlotOpenedStatus(const std::string &term)
{
    return "Status: effect plot opened for " + term + ".";
}

std::string ComparisonInteractionNotIncludedMessage(const std::string &term,
                                                    const std::string &modelLabel)
{
    const std::vector<std::string> parts = SplitInteractionTerm(term);
    std::ostringstream display;
    for (std::size_t index = 0; index < parts.size(); ++index) {
        if (index > 0) display << " \u00D7 ";
        display << parts[index];
    }
    const std::string interaction = display.str().empty() ? term : display.str();
    return "Interaction \u201C" + interaction +
        "\u201D is not included in the active model" +
        (modelLabel.empty() ? std::string() : " \u201C" + modelLabel + "\u201D") + ".";
}

std::string GeneralizedCoefficientDetailsStatus(const GeneralizedGLMRow &row,
                                                const std::string &defaultStatisticName)
{
    std::string source = row.sourceTerm.empty() || row.sourceTerm == "NA" ? row.term : row.sourceTerm;
    if (row.rowType == "factor_parent" || row.rowType == "term_parent") {
        std::string kind = row.rowType == "term_parent" ? "interaction term" : "categorical term";
        return "Status: " + source + " is a " + kind +
            "; omnibus p " + FormatPValue(row.pValue) +
            "; use its category rows for individual coefficients.";
    }
    if (row.rowType == "reference") {
        std::string level = row.factorLevel.empty() || row.factorLevel == "NA" ?
            row.displayLabel : row.factorLevel;
        std::string statistic = row.statisticName.empty() ? defaultStatisticName : row.statisticName;
        return "Status: " + source + ": " + level +
            " is the reference category; b = \u2014; SE = \u2014; " +
            statistic + " = \u2014; p = \u2014.";
    }
    std::string label = row.displayLabel.empty() ? row.term : row.displayLabel;
    std::string statistic = row.statisticName.empty() ? defaultStatisticName : row.statisticName;
    return "Status: " + label +
        "; b = " + FormatDoubleOrDash(row.estimate, 4) +
        "; SE = " + FormatDoubleOrDash(row.stdError, 4) +
        "; " + statistic + " = " + FormatDoubleOrDash(row.statistic, 3) +
        "; p " + FormatPValue(row.pValue);
}

std::string ComparisonTermNotIncludedStatus()
{
    return "Status: term is not included in this model.";
}

std::string GeneralizedComparisonCoefficientDetailsStatus(const GeneralizedGLMRow &row,
                                                          bool includeOddsRatio)
{
    std::string source = row.sourceTerm.empty() ? row.term : row.sourceTerm;
    if (row.rowType == "factor_parent" || row.rowType == "term_parent") {
        return "Status: " + source + " is a categorical term; use its category rows for coefficients.";
    }
    if (row.rowType == "reference") {
        return "Status: " + source + ": " + row.factorLevel +
            " is the reference category; b = \u2014; SE = \u2014; p = \u2014.";
    }
    std::string status = "Status: b = " + FormatDoubleOrDash(row.estimate, 4) +
        "; SE = " + FormatDoubleOrDash(row.stdError, 4) +
        "; " + row.statisticName + " = " + FormatDoubleOrDash(row.statistic, 3) +
        "; p " + FormatPValue(row.pValue) +
        "; 95% CI [" + FormatDoubleOrDash(row.ciLower, 4) + ", " +
        FormatDoubleOrDash(row.ciUpper, 4) + "]";
    if (includeOddsRatio) {
        status += "; OR = " + FormatDoubleOrDash(row.oddsRatio, 4) +
            "; OR 95% CI [" + FormatDoubleOrDash(row.oddsRatioLower, 4) + ", " +
            FormatDoubleOrDash(row.oddsRatioUpper, 4) + "]";
    }
    return status;
}

std::string RegressionCoefficientGroupDetailsStatus(const std::vector<GLMCoefficientRow> &rows)
{
    if (rows.empty()) {
        return ComparisonTermNotIncludedStatus();
    }
    std::ostringstream out;
    out << "Status: ";
    for (std::size_t i = 0; i < rows.size(); ++i) {
        if (i > 0) out << "; ";
        out << rows[i].term << " b = " << FormatDoubleOrDash(rows[i].estimate, 4)
            << ", p " << FormatPValue(rows[i].pValue);
    }
    return out.str();
}

std::string RegressionCoefficientStructuralDetailsStatus(const GLMCoefficientRow &row)
{
    if (row.rowType == "factor_parent") {
        return "Status: " + row.sourceTerm + " is a categorical term; omnibus p " +
            FormatPValue(row.pValue) + "; use its category rows for individual coefficients.";
    }
    if (row.rowType == "reference") {
        return "Status: " + row.sourceTerm + ": " + row.factorLevel +
            " is the reference category; b = \u2014; SE = \u2014; t = \u2014; p = \u2014.";
    }
    return "";
}

std::string RegressionComparisonFitValueStatus(const std::string &modelLabel,
                                               const std::string &fitLabel,
                                               const std::string &value)
{
    return "Status: " + modelLabel + " " + fitLabel + " = " + value;
}

std::string RegressionComparisonModelRenamedStatus()
{
    return "Status: model renamed.";
}

std::string RegressionComparisonOpenedSingleModelStatus(const std::string &modelLabel)
{
    return "Status: opened " + modelLabel + " as a single General Linear Model window.";
}

std::string GeneralizedGLMWindowStatus(const std::string &status,
                                       std::size_t selected,
                                       std::size_t total,
                                       const std::string &family,
                                       const std::string &link)
{
    return "Status: " + status + "  " + SelectedRowsStatus(selected, total) +
        "  Distribution = " + family + ", link = " + link;
}

bool GeneralizedGLMHasPendingManualFit(const GeneralizedGLMState &state)
{
    return !state.autoRefit && (!state.ok || state.fitVersion < state.modelVersion);
}

void ClearGeneralizedGLMFitResults(GeneralizedGLMState &state)
{
    state.ok = false;
    state.n = 0;
    state.excluded = 0;
    state.nullDeviance = state.residualDeviance = NAN;
    state.dfResidual = 0;
    state.aic = state.bic = state.dispersion = state.logLik = NAN;
    state.theta = state.thetaDescriptiveMean = NAN;
    state.thetaDescriptiveMin = state.thetaDescriptiveMax = NAN;
    state.perfectScoreCount = state.nonPerfectScoreCount = NAN;
    state.observedCeilingProportion = state.predictedCeilingProportion = NAN;
    state.predictedCeilingCount = NAN;
    state.observedCeilingCountConstant = true;
    state.betaBinomialDispersion = state.betaBinomialDispersionMin = NAN;
    state.betaBinomialDispersionMax = NAN;
    state.perfectScoreComponentStatus.clear();
    state.belowCeilingComponentStatus.clear();
    state.hurdleSuccessfulImputations = state.hurdleAttemptedImputations = 0;
    state.rows.clear();
    state.termTests.clear();
    state.pairwise.clear();
    state.diagnostics.clear();
    state.diagnosticsByImputation.clear();
    state.boundedCountDistribution.clear();
    state.boundedCountDistributionsByImputation.clear();
    state.diagnosticNotesByImputation.clear();
    state.warnings.clear();
    ++state.diagnosticsVersion;
}

std::string GeneralizedGLMPendingManualFitStatus()
{
    return "Auto-refit is off. Existing results belong to the last completed fit; turn it on to fit the current data and model specification.";
}

std::vector<GeneralizedGLMRow> GeneralizedGLMRowsForPresentation(
    const GeneralizedGLMState &state)
{
    if (!GeneralizedGLMHasPendingManualFit(state)) return state.rows;
    if (state.ok && !state.rows.empty()) return state.rows;

    std::vector<GeneralizedGLMRow> rows;
    rows.reserve(state.terms.size() + 1);
    GeneralizedGLMRow intercept;
    intercept.rowType = "intercept";
    intercept.term = "(Intercept)";
    intercept.displayLabel = intercept.term;
    intercept.sourceTerm = intercept.term;
    intercept.termType = "intercept";
    rows.push_back(std::move(intercept));
    for (const std::string &term : state.terms) {
        GeneralizedGLMRow row;
        row.rowType = "term_parent";
        row.term = term;
        row.sourceTerm = term;
        row.termType = ModelSpecificationTermType(state, term);
        row.displayLabel = IsInteractionTerm(term)
            ? JoinStrings(SplitInteractionTerm(term), " × ") : term;
        rows.push_back(std::move(row));
    }
    return rows;
}

std::string CountModelParameterSummary(const GeneralizedGLMState &state)
{
    std::string label;
    double value = state.dispersion;
    double minimum = std::numeric_limits<double>::quiet_NaN();
    double maximum = minimum;
    bool fixed = false;
    switch (state.countDistribution) {
    case CountDistribution::BetaBinomial: label = "Beta-binomial precision (phi)"; break;
    case CountDistribution::NegativeBinomial:
        label = "Theta";
        value = state.multipleImputation ? state.thetaDescriptiveMean : state.theta;
        if (state.multipleImputation) {
            minimum = state.thetaDescriptiveMin; maximum = state.thetaDescriptiveMax;
        }
        break;
    case CountDistribution::HurdleBetaBinomialCeiling:
        label = "Beta-binomial dispersion (sigma)"; value = state.betaBinomialDispersion;
        if (state.multipleImputation) {
            minimum = state.betaBinomialDispersionMin; maximum = state.betaBinomialDispersionMax;
        }
        break;
    case CountDistribution::BinomialTrials: label = "Pearson dispersion ratio"; break;
    case CountDistribution::QuasiPoisson: label = "Quasi-Poisson dispersion (phi)"; break;
    case CountDistribution::PerfectScore: label = "Binomial dispersion (fixed)"; fixed = true; break;
    default: label = "Poisson dispersion (fixed)"; fixed = true; break;
    }
    if (state.countDistribution == CountDistribution::BinomialTrials &&
        !std::isfinite(value)) return "";
    std::string result = label + ": " + FormatDoubleOrDash(value, 3);
    if (state.multipleImputation && !fixed) {
        result += " — mean across imputations";
        if (std::isfinite(minimum) && std::isfinite(maximum))
            result += "; range " + FormatDoubleOrDash(minimum, 3) + "–" + FormatDoubleOrDash(maximum, 3);
        result += " (not Rubin-pooled)";
    }
    return result;
}

std::vector<GeneralizedFamilyDiagnosticRow> GeneralizedFamilyDiagnosticRows(
    const GeneralizedGLMState &state)
{
    std::vector<GeneralizedFamilyDiagnosticRow> rows;
    const auto number = [&](const std::string &key) {
        auto found = state.familyDiagnostics.find(key);
        return found == state.familyDiagnostics.end() ? NAN : found->second;
    };
    const auto append = [&](const std::string &statistic, double value,
                            const std::string &qualifier = "",
                            const std::string &suffix = "") {
        if (!std::isfinite(value)) return;
        std::string display = statistic + qualifier + ": " +
            FormatDoubleOrDash(value, suffix == "×" ? 2 : 3) + suffix;
        if (state.multipleImputation) display += " — mean across imputations (not Rubin-pooled)";
        rows.push_back({statistic, display});
    };
    if (state.countRegression && state.countDistribution == CountDistribution::BetaBinomial) {
        append("Intra-trial correlation (rho)", number("beta_binomial_rho"));
        append("Variance inflation vs binomial", number("beta_binomial_variance_inflation"),
               number("beta_binomial_trials_vary") > 0.5
                   ? " (mean across fitted observations)" : "", "×");
    } else if (state.countRegression && state.countDistribution == CountDistribution::NegativeBinomial) {
        append("Variance inflation vs Poisson", number("negative_binomial_variance_inflation"),
               " at mean fitted count", "×");
    } else if (state.countRegression && state.countDistribution == CountDistribution::Poisson) {
        append("Pearson dispersion ratio", number("pearson_dispersion_ratio"));
    } else if (!state.countRegression && (state.family == "Gamma" || state.family == "gamma_distance")) {
        append("Implied coefficient of variation", number("gamma_cv"));
    }
    return rows;
}

std::string GeneralizedGLMFormattedOutputText(const GeneralizedGLMState &state,
                                              bool showModelDetails)
{
    std::ostringstream out;
    const auto displayedTermLabel = [&](const GeneralizedGLMRow &row) {
        std::string label = row.displayLabel.empty() ? row.term : row.displayLabel;
        const std::string source = row.sourceTerm.empty() ? row.term : row.sourceTerm;
        const bool nested = row.rowType == "factor_level" ||
            row.rowType == "reference" ||
            (!row.sourceTerm.empty() && row.term != row.sourceTerm);
        if (!nested && source.find(':') == std::string::npos &&
            state.centeredPredictors.count(source) != 0 &&
            label.find("(centered)") == std::string::npos) {
            label += " (centered)";
        }
        return label;
    };
    out << (state.binaryRegression ? "Binary Model\n" :
            state.countRegression ? "Count Model\n" :
            state.modelType == StatisticalModelType::PositiveContinuous
                ? "Positive Continuous Model\n" :
            state.modelType == StatisticalModelType::Proportion
                ? "Proportion Model\n" : "Generalized Linear Model\n");
    out << "Response  " << (state.response.empty() ? "—" : state.response)
        << (state.binaryRegression ? "     Distribution  Binomial     Event  " + state.responseCoding.eventLabel +
             "     Reference  " + state.responseCoding.referenceLabel :
             state.countRegression ? "     Distribution  " +
               (state.countDistribution == CountDistribution::QuasiPoisson
                   ? std::string("Poisson     Inference  Quasi-Poisson")
                   : CountDistributionLabel(state.countDistribution)) :
               "     Distribution  " + (FindDistributionSpecification(state.family)
                   ? FindDistributionSpecification(state.family)->visibleName
                   : state.family))
        << "     Link  " << state.link
        << (state.countRegression && !state.exposure.empty()
              ? "     Exposure  " + state.exposure + " (log offset)" : std::string{})
        << (!state.offsetVariable.empty()
              ? "     Link-scale offset  " + state.offsetVariable + " (used as stored)" : std::string{})
        << (state.countRegression && CountDistributionUsesTrials(state.countDistribution)
              ? "     Trials  " + (!state.trialsVariable.empty()
                    ? state.trialsVariable
                    : FormatDoubleOrDash(state.trialsConstant, 0))
              : std::string{})
        << "     Residuals  " << state.diagnosticOptions.residualType << "\n\n";
    if (GeneralizedGLMHasPendingManualFit(state) && !state.ok) {
        out << "Model fit\n" << GeneralizedGLMPendingManualFitStatus()
            << "\n\nTerms\n";
        for (const GeneralizedGLMRow &row : GeneralizedGLMRowsForPresentation(state)) {
            out << displayedTermLabel(row);
            if (row.term != "(Intercept)") {
                out << "     " << ModelTermTypeDisplayName(row.termType);
            }
            out << "\n";
        }
        return out.str();
    }
    if (GeneralizedGLMHasPendingManualFit(state)) {
        out << "Note. " << GeneralizedGLMPendingManualFitStatus() << "\n\n";
    }
    out << "Model fit\n";
    if (state.binaryRegression) {
        out << "N  " << state.n << "     Events  " << state.responseCoding.eventCount
            << "     Reference  " << state.responseCoding.referenceCount
            << "     LR chi-square  " << FormatModelNumberOrDash(state.globalLR)
            << "     df  " << state.dfModel << "     p  " << FormatPValue(state.globalP) << "\n";
        out << "Nagelkerke pseudo-R²  " << FormatModelNumberOrDash(state.nagelkerkeR2) << "\n\n";
        if (showModelDetails) {
            out << "Model details\n"
                << "Log likelihood  " << FormatModelNumberOrDash(state.logLik)
                << "     Residual deviance  " << FormatModelNumberOrDash(state.residualDeviance)
                << "     Null deviance  " << FormatModelNumberOrDash(state.nullDeviance)
                << "     df residual  " << state.dfResidual << "\n"
                << "AIC  " << FormatModelNumberOrDash(state.aic)
                << "     BIC  " << FormatModelNumberOrDash(state.bic)
                << "     McFadden R²  " << FormatModelNumberOrDash(state.mcfaddenR2)
                << "     Cox-Snell R²  " << FormatModelNumberOrDash(state.coxSnellR2) << "\n"
                << "Apparent AUC  " << FormatModelNumberOrDash(state.auc)
                << "     Converged  " << (state.converged ? "yes" : "no")
                << "     Iterations  " << state.iterations
                << "     Rank  " << state.rank << "/" << state.parameterCount << "\n"
                << "AUC was calculated on the observations used to fit the model; it is not out-of-sample validation.\n\n";
        }
        if (!state.warnings.empty()) {
            out << "Warnings\n";
            for (const std::string &warning : state.warnings) out << "- " << warning << "\n";
        }
        out << "\nTerms and coefficients\n";
        out << std::left << std::setw(30) << "Variable / category"
            << std::right << std::setw(11) << "b"
            << std::setw(11) << "SE"
            << std::setw(9) << "z"
            << std::setw(11) << "p";
        out << std::setw(30) << (GeneralizedModelHasExponentiatedEffect(state)
                ? GeneralizedExponentiatedEffectLabel(state) + " / 95% CI"
                : "95% CI") << "\n";
        out << std::string(102, '-') << "\n";
        for (const GeneralizedGLMRow &row : state.rows) {
            bool parent = row.rowType == "factor_parent" || row.rowType == "term_parent";
            bool reference = row.rowType == "reference";
            const GlobalTermTestRow *termTest =
                GeneralizedGlobalTermTestForPresentationRow(state, row);
            const std::string dash = "—";
            std::string effect = parent ? "" : dash;
            if (!parent && !reference) {
                effect = GeneralizedModelHasExponentiatedEffect(state)
                    ? FormatModelNumberOrDash(row.exponentiatedEstimate) + " [" + FormatModelNumberOrDash(row.exponentiatedLower) + ", " + FormatModelNumberOrDash(row.exponentiatedUpper) + "]"
                    : "[" + FormatModelNumberOrDash(row.ciLower) + ", " + FormatModelNumberOrDash(row.ciUpper) + "]";
            }
            std::string displayLabel = displayedTermLabel(row);
            if (reference && displayLabel.find("reference") == std::string::npos) displayLabel += " (reference)";
            out << std::left << std::setw(30) << displayLabel
                << std::right << std::setw(11) << (parent ? "" : reference ? dash : FormatModelNumberOrDash(row.estimate))
                << std::setw(11) << (parent ? "" : reference ? dash : FormatModelNumberOrDash(row.stdError))
                << std::setw(9) << (parent
                    ? (termTest ? FormatModelNumberOrDash(termTest->statistic) : dash)
                    : reference ? dash : FormatModelNumberOrDash(row.statistic))
                << std::setw(11) << (parent
                    ? (termTest ? FormatPValue(termTest->pValue) : dash)
                    : reference ? dash : FormatPValue(row.pValue));
            out << std::setw(30) << effect << "\n";
        }
        const std::string methodNote = GeneralizedGlobalTermTestMethodNote(state);
        if (!methodNote.empty()) out << "\nNote: " << methodNote << "\n";
        const std::string hierarchyNote = GeneralizedGlobalTermTestHierarchyNote(state);
        if (!hierarchyNote.empty()) out << "\nNote: " << hierarchyNote << "\n";
        return out.str();
    } else {
        out << "N  " << state.n
            << "     Null deviance  " << FormatDoubleOrDash(state.nullDeviance, 3)
            << "     Residual deviance  " << FormatDoubleOrDash(state.residualDeviance, 3)
            << "     df residual  " << state.dfResidual << "\n";
        if (state.countRegression) {
            const bool boundedCount = CountDistributionUsesTrials(
                state.countDistribution);
            if (state.likelihoodAvailable) {
                out << "AIC  " << FormatDoubleOrDash(state.aic, 1)
                    << "     BIC  " << FormatDoubleOrDash(state.bic, 1)
                    << "     Log likelihood  " << FormatDoubleOrDash(state.logLik, 3);
            } else if (state.countDistribution == CountDistribution::QuasiPoisson) {
                out << "Likelihood-based statistics unavailable for quasi-Poisson";
            } else if (state.multipleImputation) {
                out << "Likelihood-based statistics are not pooled across imputations";
            } else {
                out << "Likelihood-based statistics unavailable";
            }
            if (boundedCount) {
                out << "     " << (state.multipleImputation && !state.observedCeilingCountConstant
                    ? "Mean observed at ceiling  " : "Observed at ceiling  ")
                    << FormatDoubleOrDash(state.perfectScoreCount, 1) << " ("
                    << FormatPercentOrDash(state.observedCeilingProportion, 1) << ")"
                    << "     " << (state.multipleImputation
                        ? "Mean predicted at ceiling  " : "Predicted at ceiling  ")
                    << FormatDoubleOrDash(
                        std::isfinite(state.predictedCeilingCount)
                            ? state.predictedCeilingCount
                            : state.n * state.predictedCeilingProportion, 1) << " ("
                    << FormatPercentOrDash(state.predictedCeilingProportion, 1) << ")";
            }
            if (state.countDistribution == CountDistribution::HurdleBetaBinomialCeiling ||
                state.countDistribution == CountDistribution::PerfectScore) {
                out << "     Non-perfect scores  " << FormatDoubleOrDash(state.nonPerfectScoreCount, 0);
            }
            if (state.countDistribution == CountDistribution::HurdleBetaBinomialCeiling)
                out << "     Perfect-score component: fitted     Below-ceiling component: fitted";
            out << "\n" << CountModelParameterSummary(state);
            for (const auto &diagnostic : GeneralizedFamilyDiagnosticRows(state))
                out << "\n" << diagnostic.display;
            out << "\n";
        }
        if (!state.countRegression) {
            for (const auto &diagnostic : GeneralizedFamilyDiagnosticRows(state))
                out << diagnostic.display << "\n";
        }
        out << "\n";
    }
    out << std::left << std::setw(26) << "Variable / category"
        << std::setw(10) << "Type"
        << std::right << std::setw(12) << "b"
        << std::setw(12) << "SE"
        << std::setw(10) << state.statisticName
        << std::setw(10) << "p";
    if (GeneralizedModelHasExponentiatedEffect(state)) {
        out << std::setw(31) << GeneralizedExponentiatedEffectLabel(state) + " / 95% CI";
    } else {
        out << std::setw(22) << "95% CI";
    }
    out << "\n";
    out << std::string(102, '-') << "\n";
    for (const GeneralizedGLMRow &row : state.rows) {
        if (row.rowType == "section_header") {
            out << "\n" << displayedTermLabel(row) << "\n";
            continue;
        }
        bool parentRow = row.rowType == "factor_parent" || row.rowType == "term_parent";
        bool referenceRow = row.rowType == "reference";
        bool interactionChildRow = ModelTermTypeIsInteraction(row.termType) &&
            row.rowType == "coefficient" &&
            !row.sourceTerm.empty() &&
            row.sourceTerm != row.term;
        std::string typeLabel = (row.rowType == "reference" || row.rowType == "factor_level" || interactionChildRow)
            ? ""
            : (row.termType.empty() ? "-" : ModelTermTypeDisplayName(row.termType));
        std::string structuralValue = parentRow ? "" : "—";
        const GlobalTermTestRow *termTest =
            GeneralizedGlobalTermTestForPresentationRow(state, row);
        out << std::left << std::setw(26) << displayedTermLabel(row)
            << std::setw(10) << typeLabel
            << std::right << std::setw(12) << (parentRow || referenceRow ? structuralValue : FormatDoubleOrDash(row.estimate, 4))
            << std::setw(12) << (parentRow || referenceRow ? structuralValue : FormatDoubleOrDash(row.stdError, 4))
            << std::setw(10) << (parentRow
                ? (termTest ? FormatDoubleOrDash(termTest->statistic, 3) : "—")
                : referenceRow ? "—" : FormatDoubleOrDash(row.statistic, 3))
            << std::setw(10) << (parentRow
                ? (termTest ? FormatPValue(termTest->pValue) : "—")
                : referenceRow ? "—" : FormatPValue(row.pValue));
        if (GeneralizedModelHasExponentiatedEffect(state)) {
            out << std::setw(31) << (parentRow || referenceRow ? structuralValue :
                FormatDoubleOrDash(row.exponentiatedEstimate, 3) + " [" +
                FormatDoubleOrDash(row.exponentiatedLower, 3) + ", " +
                FormatDoubleOrDash(row.exponentiatedUpper, 3) + "]");
        } else {
            out << std::setw(22) << (parentRow || referenceRow ? structuralValue :
                "[" + FormatDoubleOrDash(row.ciLower, 3) + ", " + FormatDoubleOrDash(row.ciUpper, 3) + "]");
        }
        out << "\n";
    }
    if (state.rows.empty()) {
        out << "Add at least one predictor to inspect coefficients.\n";
    }
    const std::string methodNote = GeneralizedGlobalTermTestMethodNote(state);
    if (!methodNote.empty()) out << "\nNote: " << methodNote << "\n";
    const std::string hierarchyNote = GeneralizedGlobalTermTestHierarchyNote(state);
    if (!hierarchyNote.empty()) out << "\nNote: " << hierarchyNote << "\n";
    return out.str();
}

std::string RegressionComparisonSummaryStatus(std::size_t modelCount,
                                              std::size_t termCount,
                                              bool preferNote,
                                              const std::string &note)
{
    if (preferNote && !note.empty()) {
        return "Status: " + note;
    }
    return "Status: " + std::to_string(modelCount) + " models, " +
        std::to_string(termCount) + " terms";
}

std::string GeneralizedComparisonSummaryStatus(std::size_t modelCount,
                                               std::size_t termCount)
{
    std::size_t editableTerms = termCount > 0 ? termCount - 1 : 0;
    return "Status: " + std::to_string(modelCount) + " generalized models, " +
        std::to_string(editableTerms) + " terms";
}

std::string ComparisonNoAvailableTermsStatus()
{
    return "Status: no available terms.";
}

std::string PrecomputedComparisonChangeInRStatus(const std::string &subject)
{
    return "Status: this is a precomputed table; change the " + subject + " in R and reopen the table.";
}

std::string PooledMIComparisonChangeInRStatus()
{
    return "Status: this is a pooled MI table; change the comparison in R and reopen the table.";
}

std::string PooledMITermTypesChangeInRStatus()
{
    return "Status: this is a pooled MI table; change term types in R and reopen the table.";
}

std::string PooledMIAutoRefitDisabledStatus()
{
    return "Status: this is a pooled MI table; auto-refit is disabled.";
}

std::string PooledOrPrecomputedRefitLayerStatus()
{
    return "Status: pooled/precomputed comparisons are refit from their analysis layer.";
}

std::string PooledMIComparisonRefitStatus(const std::string &reason,
                                          bool viaMice)
{
    if (viaMice) {
        return "Status: refitting pooled MI model comparison via R/mice...";
    }
    if (reason.empty()) {
        return "Status: refitting pooled MI model comparison...";
    }
    return "Status: refitting pooled MI comparison " + reason + "...";
}

std::string RegressionComparisonRefittedStatus()
{
    return "Status: model refitted.";
}

std::string RowsUsedExcludedText(const std::vector<int> &rowsUsed,
                                 const std::vector<int> &rowsExcluded,
                                 const std::string &title)
{
    std::ostringstream out;
    if (!title.empty()) {
        out << title << "\n\n";
    }
    out << "Rows used (" << rowsUsed.size() << "): ";
    for (std::size_t i = 0; i < rowsUsed.size(); ++i) {
        if (i) out << ", ";
        out << rowsUsed[i];
    }
    out << "\n\nRows excluded (" << rowsExcluded.size() << "): ";
    for (std::size_t i = 0; i < rowsExcluded.size(); ++i) {
        if (i) out << ", ";
        out << rowsExcluded[i];
    }
    return out.str();
}

std::string RegressionCoefficientDisplay(const GLMFitSummary &fit,
                                         const std::vector<std::string> &includedTerms,
                                         const std::string &term)
{
    if (!fit.ok) {
        return "\u2014";
    }
    const GLMCoefficientRow *coef = CoefficientForTerm(fit, term);
    if (coef) {
        std::string source = RegressionRowSourceTerm(coef, term);
        if (source != "(Intercept)" &&
            !TermListContainsEquivalentModelTerm(includedTerms, source)) {
            return "\u2014";
        }
        if (coef->rowType == "reference") {
            return "\u2014";
        }
        if (coef->rowType == "factor_parent" || coef->rowType == "term_parent") {
            return "";
        }
        return FormatDouble(coef->estimate, 4) + SignificanceStars(coef->pValue);
    }
    if (term != "(Intercept)" && !TermListContainsEquivalentModelTerm(includedTerms, term)) {
        return "\u2014";
    }
    return "";
}

GeneralizedGLMRow *GeneralizedRowForTerm(std::vector<GeneralizedGLMRow> &rows,
                                         const std::string &term)
{
    for (GeneralizedGLMRow &row : rows) {
        if (row.term == term) return &row;
    }
    return nullptr;
}

const GeneralizedGLMRow *GeneralizedRowForTerm(const std::vector<GeneralizedGLMRow> &rows,
                                               const std::string &term)
{
    for (const GeneralizedGLMRow &row : rows) {
        if (row.term == term) return &row;
    }
    return nullptr;
}

namespace {

constexpr char kGeneralizedComparisonRowSeparator = '\x1e';

std::string GeneralizedComparisonPresentationTermForRow(
    const std::vector<GeneralizedGLMRow> &rows,
    std::size_t rowIndex)
{
    if (rowIndex >= rows.size()) return "";
    const GeneralizedGLMRow &row = rows[rowIndex];
    const std::size_t duplicateCount = static_cast<std::size_t>(std::count_if(
        rows.begin(), rows.end(), [&](const GeneralizedGLMRow &candidate) {
            return candidate.term == row.term;
        }));
    if (duplicateCount <= 1) return row.term;

    // The parent keeps the canonical model-term identity so edits continue to
    // target the specification.  Coefficient children receive a private,
    // deterministic presentation key; the key never becomes a display label
    // or an R coefficient name.
    if (row.rowType == "term_parent" || row.rowType == "factor_parent") {
        return row.term;
    }
    std::size_t ordinal = 0;
    for (std::size_t index = 0; index < rowIndex; ++index) {
        if (rows[index].term == row.term && rows[index].rowType == row.rowType) {
            ++ordinal;
        }
    }
    return row.term + kGeneralizedComparisonRowSeparator + row.rowType +
        kGeneralizedComparisonRowSeparator + std::to_string(ordinal);
}

} // namespace

const GeneralizedGLMRow *GeneralizedComparisonRowForPresentationTerm(
    const std::vector<GeneralizedGLMRow> &rows,
    const std::string &presentationTerm)
{
    const std::size_t firstSeparator = presentationTerm.find(
        kGeneralizedComparisonRowSeparator);
    if (firstSeparator == std::string::npos) {
        const GeneralizedGLMRow *first = nullptr;
        for (const GeneralizedGLMRow &row : rows) {
            if (row.term != presentationTerm) continue;
            if (!first) first = &row;
            if (row.rowType == "term_parent" || row.rowType == "factor_parent") {
                return &row;
            }
        }
        return first;
    }

    const std::size_t secondSeparator = presentationTerm.find(
        kGeneralizedComparisonRowSeparator, firstSeparator + 1);
    if (secondSeparator == std::string::npos) return nullptr;
    const std::string term = presentationTerm.substr(0, firstSeparator);
    const std::string rowType = presentationTerm.substr(
        firstSeparator + 1, secondSeparator - firstSeparator - 1);
    const std::string ordinalText = presentationTerm.substr(secondSeparator + 1);
    char *end = nullptr;
    const unsigned long requestedOrdinal = std::strtoul(ordinalText.c_str(), &end, 10);
    if (!end || *end != '\0') return nullptr;
    std::size_t ordinal = 0;
    for (const GeneralizedGLMRow &row : rows) {
        if (row.term != term || row.rowType != rowType) continue;
        if (ordinal == requestedOrdinal) return &row;
        ++ordinal;
    }
    return nullptr;
}

std::string GeneralizedRowDisplayLabel(const GeneralizedGLMRow *row,
                                       const std::string &fallback)
{
    if (row && !row->displayLabel.empty() && row->displayLabel != "NA") {
        return row->displayLabel;
    }
    return fallback;
}

std::string GeneralizedRowSourceTerm(const GeneralizedGLMRow *row,
                                     const std::string &fallback)
{
    if (row) {
        return EffectiveGeneralizedRowSourceTerm(*row, fallback);
    }
    return fallback;
}

std::string GeneralizedCoefficientDisplay(const std::vector<GeneralizedGLMRow> &rows,
                                          bool fitOk,
                                          const std::vector<std::string> &includedTerms,
                                          const std::string &term)
{
    if (!fitOk) {
        return "\u2014";
    }
    const GeneralizedGLMRow *row = GeneralizedComparisonRowForPresentationTerm(rows, term);
    if (row) {
        std::string source = GeneralizedRowSourceTerm(row, term);
        if (row->rowType == "reference") return "\u2014";
        if (row->rowType == "factor_parent" || row->rowType == "term_parent") return "";
        if (!TermListContainsEquivalentModelTerm(includedTerms, source) &&
            source != "(Intercept)") return "\u2014";
        return FormatDoubleOrDash(row->estimate, 3) + SignificanceStars(row->pValue);
    }
    if (term != "(Intercept)" && !TermListContainsEquivalentModelTerm(includedTerms, term)) {
        return "\u2014";
    }
    return "";
}

std::vector<double> MainEffectScenarioValues(const std::string &term,
                                             const ScenarioDesignInput &input)
{
    std::vector<double> values;
    const std::string variable = BaseVariableForTermComponent(term);
    const std::string type = ScenarioTypeForTerm(term, input);
    const ScenarioVariableInfo *info = FindScenarioVariableInfo(input, variable);

    if (type == "factor") {
        if (!info || info->factorLevels.empty()) return values;
        std::string chosen = info->factorReferenceLevel.empty()
            ? info->factorLevels.front()
            : info->factorReferenceLevel;
        auto valueIt = input.factorValues.find(variable);
        if (valueIt != input.factorValues.end()) chosen = valueIt->second;
        auto codingIt = info->factorCoding.find(chosen);
        if (codingIt != info->factorCoding.end()) {
            return codingIt->second;
        }
        values.reserve(info->factorLevels.size() > 0 ? info->factorLevels.size() - 1 : 0);
        for (std::size_t i = 1; i < info->factorLevels.size(); ++i) {
            values.push_back(chosen == info->factorLevels[i] ? 1.0 : 0.0);
        }
        return values;
    }

    auto valueIt = input.numericValues.find(variable);
    if (valueIt != input.numericValues.end()) {
        values.push_back(valueIt->second);
        return values;
    }
    values.push_back(info ? info->numericDefault : 0.0);
    return values;
}

std::vector<double> TermScenarioValues(const std::string &term,
                                       const ScenarioDesignInput &input)
{
    if (!IsInteractionTerm(term)) {
        return MainEffectScenarioValues(term, input);
    }

    std::vector<double> values(1, 1.0);
    for (const std::string &part : SplitInteractionTerm(term)) {
        std::vector<double> partValues = MainEffectScenarioValues(part, input);
        if (partValues.empty()) return std::vector<double>();
        std::vector<double> next;
        next.reserve(values.size() * partValues.size());
        for (double left : values) {
            for (double right : partValues) {
                next.push_back(left * right);
            }
        }
        values = next;
    }
    return values;
}

bool BuildScenarioDesignVector(const ScenarioDesignInput &input,
                               std::vector<double> &out)
{
    out.clear();
    out.push_back(1.0);
    for (const std::string &term : input.terms) {
        std::vector<double> values = TermScenarioValues(term, input);
        if (values.empty()) {
            out.clear();
            return false;
        }
        out.insert(out.end(), values.begin(), values.end());
    }
    if (input.expectedSize > 0 && out.size() != input.expectedSize) {
        out.clear();
        return false;
    }
    return true;
}

LinearOlsResult FitLinearOls(const LinearOlsInput &input)
{
    LinearOlsResult result;
    result.n = static_cast<int>(input.response.size());
    if (input.designMatrix.size() != input.response.size()) {
        result.warning = "Design matrix and response have different lengths.";
        return result;
    }
    if (input.rowIds.size() != input.response.size()) {
        result.warning = "Row id vector and response have different lengths.";
        return result;
    }
    if (input.designMatrix.empty() || input.designMatrix[0].empty()) {
        result.warning = "Not enough complete cases to fit the model.";
        return result;
    }

    std::size_t p = input.designMatrix[0].size();
    for (const std::vector<double> &row : input.designMatrix) {
        if (row.size() != p) {
            result.warning = "Design matrix rows have different lengths.";
            return result;
        }
    }

    result.dfModel = static_cast<int>(p) - 1;
    result.dfResidual = result.n - static_cast<int>(p);
    if (result.dfResidual <= 0) {
        result.warning = "Not enough complete cases to fit the model.";
        return result;
    }

    std::vector<std::vector<double>> xtx(p, std::vector<double>(p, 0.0));
    std::vector<double> xty(p, 0.0);
    double yMean = 0.0;
    for (double v : input.response) yMean += v;
    yMean /= static_cast<double>(input.response.size());
    for (std::size_t i = 0; i < input.designMatrix.size(); ++i) {
        for (std::size_t j = 0; j < p; ++j) {
            xty[j] += input.designMatrix[i][j] * input.response[i];
            for (std::size_t k = 0; k < p; ++k) {
                xtx[j][k] += input.designMatrix[i][j] * input.designMatrix[i][k];
            }
        }
    }

    std::vector<std::vector<double>> inv;
    if (!InvertMatrix(xtx, inv)) {
        result.warning = "Model matrix is singular; remove a redundant predictor.";
        return result;
    }

    std::vector<double> beta(p, 0.0);
    for (std::size_t j = 0; j < p; ++j) {
        for (std::size_t k = 0; k < p; ++k) beta[j] += inv[j][k] * xty[k];
    }

    double sse = 0.0;
    double sst = 0.0;
    std::vector<double> fittedValues(input.designMatrix.size(), std::numeric_limits<double>::quiet_NaN());
    std::vector<double> residualValues(input.designMatrix.size(), std::numeric_limits<double>::quiet_NaN());
    for (std::size_t i = 0; i < input.designMatrix.size(); ++i) {
        double fitted = 0.0;
        for (std::size_t j = 0; j < p; ++j) fitted += beta[j] * input.designMatrix[i][j];
        double residual = input.response[i] - fitted;
        fittedValues[i] = fitted;
        residualValues[i] = residual;
        sse += residual * residual;
        double dy = input.response[i] - yMean;
        sst += dy * dy;
    }

    double ySd = std::numeric_limits<double>::quiet_NaN();
    if (input.response.size() > 1) {
        double yVar = 0.0;
        for (double v : input.response) {
            double dy = v - yMean;
            yVar += dy * dy;
        }
        ySd = std::sqrt(yVar / static_cast<double>(input.response.size() - 1));
    }

    std::vector<double> xSd(p, std::numeric_limits<double>::quiet_NaN());
    for (std::size_t j = 1; j < p; ++j) {
        double mean = 0.0;
        for (std::size_t i = 0; i < input.designMatrix.size(); ++i) mean += input.designMatrix[i][j];
        mean /= static_cast<double>(input.designMatrix.size());
        double var = 0.0;
        for (std::size_t i = 0; i < input.designMatrix.size(); ++i) {
            double dx = input.designMatrix[i][j] - mean;
            var += dx * dx;
        }
        if (input.designMatrix.size() > 1) {
            xSd[j] = std::sqrt(var / static_cast<double>(input.designMatrix.size() - 1));
        }
    }

    result.r2 = sst > 0.0 ? std::max(0.0, 1.0 - sse / sst) : 1.0;
    result.adjR2 = 1.0 - (1.0 - result.r2) * (result.n - 1) / std::max(1, result.dfResidual);
    result.rmse = std::sqrt(sse / std::max(1, result.n));
    result.sigma = std::sqrt(sse / std::max(1, result.dfResidual));
    result.ssRegression = std::max(0.0, sst - sse);
    result.ssResidual = sse;
    result.msRegression = result.dfModel > 0 ? result.ssRegression / result.dfModel : std::numeric_limits<double>::quiet_NaN();
    result.msResidual = result.dfResidual > 0 ? result.ssResidual / result.dfResidual : std::numeric_limits<double>::quiet_NaN();
    result.globalF = (result.dfModel > 0 && result.msResidual > 0.0) ? result.msRegression / result.msResidual : std::numeric_limits<double>::quiet_NaN();
    result.globalP = FDistributionUpperTail(result.globalF, result.dfModel, result.dfResidual);
    result.aic = result.n * std::log(sse / result.n) + 2.0 * p;
    result.bic = result.n * std::log(sse / result.n) + std::log(static_cast<double>(result.n)) * p;
    result.beta = beta;
    result.covariance.assign(p, std::vector<double>(p, 0.0));
    double sigma2 = result.sigma * result.sigma;
    for (std::size_t j = 0; j < p; ++j) {
        for (std::size_t k = 0; k < p; ++k) {
            result.covariance[j][k] = sigma2 * inv[j][k];
        }
    }

    for (std::size_t i = 0; i < input.designMatrix.size(); ++i) {
        double leverage = 0.0;
        for (std::size_t j = 0; j < p; ++j) {
            for (std::size_t k = 0; k < p; ++k) {
                leverage += input.designMatrix[i][j] * inv[j][k] * input.designMatrix[i][k];
            }
        }
        double denom = result.sigma * std::sqrt(std::max(1.0e-12, 1.0 - leverage));
        double standardized = denom > 0.0 ? residualValues[i] / denom : std::numeric_limits<double>::quiet_NaN();
        double studentized = standardized;
        double cooks = (std::isfinite(standardized) && p > 0 && leverage < 1.0)
            ? (standardized * standardized * leverage) / (static_cast<double>(p) * std::max(1.0e-12, 1.0 - leverage))
            : std::numeric_limits<double>::quiet_NaN();
        LinearOlsDiagnosticRow diag;
        diag.row = input.rowIds[i];
        diag.observed = input.response[i];
        diag.fitted = fittedValues[i];
        diag.residual = residualValues[i];
        diag.standardizedResidual = standardized;
        diag.studentizedResidual = studentized;
        diag.leverage = leverage;
        diag.cooksDistance = cooks;
        diag.sqrtAbsStandardizedResidual = std::isfinite(standardized) ? std::sqrt(std::fabs(standardized)) : std::numeric_limits<double>::quiet_NaN();
        result.diagnostics.push_back(diag);
    }

    result.coefficients.resize(p);
    for (std::size_t j = 0; j < p; ++j) {
        LinearOlsCoefficient coefficient;
        coefficient.estimate = beta[j];
        if (j > 0 && std::isfinite(ySd) && ySd > 0.0 && std::isfinite(xSd[j])) {
            coefficient.standardizedBeta = coefficient.estimate * xSd[j] / ySd;
        }
        coefficient.stdError = std::sqrt(std::max(0.0, result.sigma * result.sigma * inv[j][j]));
        coefficient.tValue = coefficient.stdError > 0.0 ? coefficient.estimate / coefficient.stdError : std::numeric_limits<double>::quiet_NaN();
        coefficient.pValue = NormalTwoSidedP(coefficient.tValue);
        if (j > 0 && std::isfinite(coefficient.tValue)) {
            const double denominator = coefficient.tValue * coefficient.tValue + result.dfResidual;
            if (denominator > 0.0) {
                coefficient.partialR = coefficient.tValue / std::sqrt(denominator);
            }
        }
        result.coefficients[j] = coefficient;
    }

    result.ok = true;
    return result;
}

bool SameIntegerSet(std::vector<int> left, std::vector<int> right)
{
    std::sort(left.begin(), left.end());
    std::sort(right.begin(), right.end());
    return left == right;
}

bool TermVectorsAreNested(const std::vector<std::string> &left,
                          const std::vector<std::string> &right)
{
    return TermVectorIncludesAll(left, right) || TermVectorIncludesAll(right, left);
}

bool IsQuasiGeneralizedFamily(const std::string &family)
{
    return family.rfind("quasi", 0) == 0;
}

int ComparisonModelSerialFromId(const std::string &comparisonId,
                                const std::string &modelId)
{
    std::string prefix = comparisonId + ":model:";
    if (modelId.rfind(prefix, 0) != 0) return 0;
    return std::atoi(modelId.substr(prefix.size()).c_str());
}

int NextComparisonModelSerial(const std::string &comparisonId,
                              const std::vector<std::string> &modelIds)
{
    int next = 1;
    for (const std::string &modelId : modelIds) {
        next = std::max(next, ComparisonModelSerialFromId(comparisonId, modelId) + 1);
    }
    return next;
}

std::string NextComparisonModelId(const std::string &comparisonId,
                                  const std::vector<std::string> &modelIds)
{
    return comparisonId + ":model:" + std::to_string(NextComparisonModelSerial(comparisonId, modelIds));
}

bool ComparisonLabelExists(const std::vector<std::string> &labels,
                           const std::string &label)
{
    return std::find(labels.begin(), labels.end(), label) != labels.end();
}

std::string NextUntitledComparisonLabel(const std::vector<std::string> &labels)
{
    int next = 1;
    const std::string prefix = "Model ";
    for (const std::string &label : labels) {
        if (label.rfind(prefix, 0) == 0) {
            next = std::max(next, std::atoi(label.substr(prefix.size()).c_str()) + 1);
        }
    }
    std::string candidate;
    do {
        candidate = "Model " + std::to_string(next++);
    } while (ComparisonLabelExists(labels, candidate));
    return candidate;
}

std::string UniqueRegressionComparisonCopyLabel(const std::vector<std::string> &labels,
                                                const std::string &label)
{
    (void)label;
    return NextUntitledComparisonLabel(labels);
}

std::string UniqueGeneralizedComparisonCopyLabel(const std::vector<std::string> &labels,
                                                 const std::string &label)
{
    (void)label;
    return NextUntitledComparisonLabel(labels);
}

std::vector<std::string> RegressionComparisonTermRowsFromFits(
    const std::vector<std::string> &existingRows,
    const std::vector<std::vector<std::string>> &includedTermsByModel,
    const std::vector<std::vector<GLMCoefficientRow>> &coefficientRowsByModel)
{
    std::vector<std::pair<std::string, std::string>> displayRows;
    for (const std::vector<GLMCoefficientRow> &fitRows : coefficientRowsByModel) {
        std::string currentSource;
        for (const GLMCoefficientRow &row : fitRows) {
            std::string source = EffectiveLinearRowSourceTerm(row, row.term);
            if ((row.rowType == "factor_level" || row.rowType == "reference") &&
                source == row.term && !currentSource.empty()) {
                source = currentSource;
            }
            if (row.rowType == "factor_parent" || row.rowType == "term_parent") {
                currentSource = source.empty() ? row.term : source;
            }
            displayRows.push_back({row.term, source});
        }
    }

    std::vector<std::string> baseTerms;
    for (const std::string &term : existingRows) {
        if (term == "(Intercept)") continue;
        std::string sourceTerm = ModelTermWithoutLevelSuffixes(term);
        if (!sourceTerm.empty() && sourceTerm != term) {
            if (std::find(baseTerms.begin(), baseTerms.end(), sourceTerm) == baseTerms.end()) {
                baseTerms.push_back(sourceTerm);
            }
            continue;
        }
        bool derivedFactorRow = false;
        for (const auto &row : displayRows) {
            if (row.first == term && row.second != term) {
                derivedFactorRow = true;
                break;
            }
        }
        if (!derivedFactorRow && std::find(baseTerms.begin(), baseTerms.end(), term) == baseTerms.end()) {
            baseTerms.push_back(term);
        }
    }
    for (const std::vector<std::string> &terms : includedTermsByModel) {
        for (const std::string &term : terms) {
            if (std::find(baseTerms.begin(), baseTerms.end(), term) == baseTerms.end()) {
                baseTerms.push_back(term);
            }
        }
    }

    std::vector<std::string> rows;
    rows.push_back("(Intercept)");
    auto addRow = [&](const std::string &term) {
        if (std::find(rows.begin(), rows.end(), term) == rows.end()) rows.push_back(term);
    };
    for (const std::string &term : baseTerms) {
        bool expanded = false;
        for (const auto &row : displayRows) {
            if (row.second == term || row.first == term) {
                addRow(row.first);
                expanded = true;
            }
        }
        if (!expanded) addRow(term);
    }
    return rows;
}

std::vector<std::string> GeneralizedComparisonTermRowsFromFits(
    const std::vector<std::vector<std::string>> &includedTermsByModel,
    const std::vector<std::vector<GeneralizedGLMRow>> &rowsByModel)
{
    std::vector<std::string> baseTerms;
    for (const std::vector<std::string> &terms : includedTermsByModel) {
        for (const std::string &term : terms) {
            if (term.empty() || term == "(Intercept)") continue;
            if (std::find(baseTerms.begin(), baseTerms.end(), term) == baseTerms.end()) {
                baseTerms.push_back(term);
            }
        }
    }

    std::vector<std::string> rows;
    auto add = [&](const std::string &term) {
        if (term.empty()) return;
        if (std::find(rows.begin(), rows.end(), term) == rows.end()) rows.push_back(term);
    };
    add("(Intercept)");
    std::vector<std::pair<std::string, std::string>> displayRows;
    for (const std::vector<GeneralizedGLMRow> &fitRows : rowsByModel) {
        std::string currentSource;
        for (std::size_t rowIndex = 0; rowIndex < fitRows.size(); ++rowIndex) {
            const GeneralizedGLMRow &row = fitRows[rowIndex];
            std::string source = EffectiveGeneralizedRowSourceTerm(row, row.term);
            if ((row.rowType == "factor_level" || row.rowType == "reference") &&
                source == row.term && !currentSource.empty()) {
                source = currentSource;
            }
            if (row.rowType == "factor_parent" || row.rowType == "term_parent") {
                currentSource = source.empty() ? row.term : source;
            }
            displayRows.push_back({
                GeneralizedComparisonPresentationTermForRow(fitRows, rowIndex), source});
        }
    }
    for (const std::string &term : baseTerms) {
        bool expanded = false;
        for (const auto &row : displayRows) {
            if (row.second == term || row.first == term) {
                add(row.first);
                expanded = true;
            }
        }
        if (!expanded) add(term);
    }
    // Fitted rows are subordinate to the immutable specification.  Never
    // promote an orphan result row into a new editable/displayed model term:
    // that was the path by which an old `hp` row could survive beside fit
    // statistics for `mpg + factor(cyl)`.
    return rows;
}

NestedLinearModelSummary BuildNestedLinearModelSummary(const GLMFitSummary &fit,
                                                       const std::string &response,
                                                       const std::vector<std::string> &terms)
{
    NestedLinearModelSummary summary;
    summary.ok = fit.ok;
    summary.response = response;
    summary.terms = terms;
    summary.rowsUsed = fit.rowsUsed;
    summary.dfResidual = fit.dfResidual;
    summary.ssResidual = fit.ssResidual;
    return summary;
}

NestedGeneralizedModelSummary BuildNestedGeneralizedModelSummary(bool ok,
                                                                 const std::string &response,
                                                                 const std::string &family,
                                                                 const std::string &link,
                                                                 const std::vector<std::string> &terms,
                                                                 const std::vector<int> &rowsUsed,
                                                                 int dfResidual,
                                                                 double residualDeviance,
                                                                 double dispersion)
{
    NestedGeneralizedModelSummary summary;
    summary.ok = ok;
    summary.response = response;
    summary.family = family;
    summary.link = link;
    summary.terms = terms;
    summary.rowsUsed = rowsUsed;
    summary.dfResidual = dfResidual;
    summary.residualDeviance = residualDeviance;
    summary.dispersion = dispersion;
    return summary;
}

NestedModelTestResult LinearNestedModelTest(const NestedLinearModelSummary &left,
                                            const NestedLinearModelSummary &right)
{
    NestedModelTestResult result;
    result.statisticName = "F";
    if (!left.ok || !right.ok) return result;
    if (left.response != right.response) return result;
    if (!SameIntegerSet(left.rowsUsed, right.rowsUsed)) return result;
    if (!TermVectorsAreNested(left.terms, right.terms)) return result;

    const NestedLinearModelSummary *reduced = &left;
    const NestedLinearModelSummary *full = &right;
    if (right.dfResidual > left.dfResidual) {
        reduced = &right;
        full = &left;
    }

    int df = std::abs(reduced->dfResidual - full->dfResidual);
    if (df <= 0 || full->dfResidual <= 0) return result;
    double ssDiff = reduced->ssResidual - full->ssResidual;
    if (ssDiff < -1.0e-8 || !std::isfinite(ssDiff)) return result;
    ssDiff = std::max(0.0, ssDiff);
    double msFull = full->ssResidual / static_cast<double>(full->dfResidual);
    if (!(msFull > 0.0) || !std::isfinite(msFull)) return result;

    result.df = df;
    result.df2 = static_cast<double>(full->dfResidual);
    result.delta = ssDiff;
    result.statistic = (ssDiff / static_cast<double>(df)) / msFull;
    result.p = FDistributionUpperTail(result.statistic,
                                      static_cast<double>(df),
                                      static_cast<double>(full->dfResidual));
    result.ok = std::isfinite(result.statistic) && std::isfinite(result.p);
    return result;
}

NestedModelTestResult GeneralizedNestedModelTest(const NestedGeneralizedModelSummary &left,
                                                 const NestedGeneralizedModelSummary &right)
{
    NestedModelTestResult result;
    if (!left.ok || !right.ok) return result;
    if (left.response != right.response) return result;
    if (left.family != right.family || left.link != right.link) return result;
    if (!SameIntegerSet(left.rowsUsed, right.rowsUsed)) return result;
    if (!TermVectorsAreNested(left.terms, right.terms)) return result;

    const NestedGeneralizedModelSummary *reduced = &left;
    const NestedGeneralizedModelSummary *full = &right;
    if (right.dfResidual > left.dfResidual) {
        reduced = &right;
        full = &left;
    }

    int df = std::abs(reduced->dfResidual - full->dfResidual);
    if (df <= 0) return result;
    double devianceDiff = reduced->residualDeviance - full->residualDeviance;
    if (devianceDiff < -1.0e-8 || !std::isfinite(devianceDiff)) return result;
    devianceDiff = std::max(0.0, devianceDiff);

    result.df = df;
    result.delta = devianceDiff;
    if (IsQuasiGeneralizedFamily(full->family)) {
        result.statisticName = "F";
        if (!(full->dfResidual > 0) || !(full->dispersion > 0.0) ||
            !std::isfinite(full->dispersion)) {
            return result;
        }
        result.df2 = static_cast<double>(full->dfResidual);
        result.statistic = (devianceDiff / static_cast<double>(df)) / full->dispersion;
        result.p = FDistributionUpperTail(result.statistic,
                                          static_cast<double>(df),
                                          static_cast<double>(full->dfResidual));
    } else {
        result.statisticName = "Chi-square";
        result.statistic = devianceDiff;
        result.p = ChiSquareUpperTail(result.statistic, static_cast<double>(df));
    }
    result.ok = std::isfinite(result.statistic) && std::isfinite(result.p);
    return result;
}

std::string NormalizeLinearDiagnosticKind(const std::string &kind)
{
    if (kind == "residuals_vs_fitted") {
        return "residuals_fitted";
    }
    if (kind == "observed_vs_fitted") {
        return "observed_fitted";
    }
    if (kind == "residual_histogram" || kind == "histogram") {
        return "residual_histogram";
    }
    if (kind == "normal_qq" || kind == "qq") {
        return "normal_qq";
    }
    if (kind == "scale_location") {
        return "scale_location";
    }
    if (kind == "residuals_leverage" || kind == "residuals_vs_leverage") {
        return "residuals_leverage";
    }
    if (kind == "cooks" || kind == "cooks_distance") {
        return "cooks_distance";
    }
    return kind;
}

bool LinearDiagnosticKindIsImplemented(const std::string &kind)
{
    std::string normalized = NormalizeLinearDiagnosticKind(kind);
    return normalized == "residuals_fitted" ||
        normalized == "observed_fitted" ||
        normalized == "residual_histogram" ||
        normalized == "normal_qq" ||
        normalized == "scale_location" ||
        normalized == "residuals_leverage" ||
        normalized == "cooks_distance";
}

std::string NormalizeGeneralizedDiagnosticKind(const std::string &kind)
{
    if (kind == "observed_vs_fitted") return "observed_fitted";
    if (kind == "residuals_vs_fitted") return "residuals_fitted";
    if (kind == "residual_histogram" || kind == "histogram") return "residual_histogram";
    if (kind == "normal_qq" || kind == "qq") return "normal_qq";
    if (kind == "scale_location") return "scale_location";
    if (kind == "residuals_leverage" || kind == "residuals_vs_leverage") return "residuals_leverage";
    if (kind == "cooks" || kind == "cooks_distance") return "cooks_distance";
    if (kind == "roc" || kind == "roc_curve") return "roc_curve";
    if (kind == "calibration" || kind == "calibration_plot") return "calibration_plot";
    if (kind == "score_distribution" ||
        kind == "observed_predicted_distribution" ||
        kind == "observed_vs_predicted_score_distribution") {
        return "observed_predicted_score_distribution";
    }
    if (kind == "boundary_zero_fit" || kind == "boundary_fit" ||
        kind == "zero_fit") return "boundary_zero_fit";
    return kind;
}

double GeneralizedResidualForDiagnostic(const GeneralizedDiagnosticRow &row,
                                        const std::string &residualType)
{
    if (residualType == "dunn_smyth") return row.dunnSmythResidual;
    if (residualType == "raw") return row.rawResidual;
    if (residualType == "studentized") return row.studentizedResidual;
    if (residualType == "standardized") return row.standardizedResidual;
    if (residualType == "pearson") return row.pearsonResidual;
    if (residualType == "working") return row.workingResidual;
    return row.devianceResidual;
}

std::string GeneralizedResidualLabel(const std::string &residualType)
{
    if (residualType == "dunn_smyth") return "Dunn\xE2\x80\x93Smyth residual";
    if (residualType == "raw") return "raw residual";
    if (residualType == "studentized") return "studentized residual";
    if (residualType == "standardized") return "standardized residual";
    if (residualType == "pearson") return "Pearson residual";
    if (residualType == "working") return "working residual";
    return "deviance residual";
}

DiagnosticPlotData BuildLinearDiagnosticPlotData(const std::string &kind,
                                                 const std::vector<LinearOlsDiagnosticRow> &diagnostics,
                                                 int fitVersion,
                                                 int diagnosticsVersion)
{
    return BuildLinearDiagnosticPlotData(
        kind, "raw", diagnostics, fitVersion, diagnosticsVersion);
}

namespace {
double LinearResidualForDiagnostic(const LinearOlsDiagnosticRow &row,
                                   const std::string &residualType)
{
    if (residualType == "studentized") return row.studentizedResidual;
    if (residualType == "standardized") return row.standardizedResidual;
    return row.residual;
}

std::string LinearResidualLabel(const std::string &residualType)
{
    if (residualType == "studentized") return "studentized residual";
    if (residualType == "standardized") return "standardized residual";
    return "raw residual";
}
}

DiagnosticPlotData BuildLinearDiagnosticPlotData(const std::string &kind,
                                                 const std::string &residualType,
                                                 const std::vector<LinearOlsDiagnosticRow> &diagnostics,
                                                 int fitVersion,
                                                 int diagnosticsVersion)
{
    DiagnosticPlotData data;
    if (diagnostics.empty()) {
        data.message = "The current linear model has no diagnostic rows. Refit the model with at least one valid predictor.";
        return data;
    }
    data.kind = "scatter";
    data.diagnosticKind = NormalizeLinearDiagnosticKind(kind);
    if (!LinearDiagnosticKindIsImplemented(data.diagnosticKind)) {
        data.message = "The requested diagnostic is not available for this linear model.";
        return data;
    }
    data.title = "Residuals vs fitted";
    data.xLabel = "fitted";
    data.yLabel = "residual";
    data.fitVersion = fitVersion;
    data.diagnosticsVersion = diagnosticsVersion;
    data.residualType = residualType;

    if (data.diagnosticKind == "observed_fitted") {
        data.title = "Observed vs fitted";
        data.xLabel = "Fitted";
        data.yLabel = "Observed";
        data.showIdentityLine = true;
        for (const LinearOlsDiagnosticRow &row : diagnostics) {
            if (std::isfinite(row.fitted) && std::isfinite(row.observed)) {
                data.points.push_back(DiagnosticPlotPoint{row.fitted, row.observed, row.row});
            }
        }
    } else if (data.diagnosticKind == "residuals_fitted") {
        for (const LinearOlsDiagnosticRow &row : diagnostics) {
            const double residual = LinearResidualForDiagnostic(row, residualType);
            if (std::isfinite(row.fitted) && std::isfinite(residual)) {
                data.points.push_back(DiagnosticPlotPoint{row.fitted, residual, row.row});
            }
        }
        data.yLabel = LinearResidualLabel(residualType);
    } else if (data.diagnosticKind == "residual_histogram") {
        data.kind = "histogram";
        data.title = "Residual histogram";
        data.xLabel = LinearResidualLabel(residualType);
        data.yLabel.clear();
        for (const LinearOlsDiagnosticRow &row : diagnostics) {
            const double residual = LinearResidualForDiagnostic(row, residualType);
            if (std::isfinite(residual)) {
                data.points.push_back(DiagnosticPlotPoint{residual, 0.0, row.row});
            }
        }
    } else if (data.diagnosticKind == "normal_qq") {
        struct QQPoint { double residual; double quantile; int row; };
        std::vector<QQPoint> residuals;
        residuals.reserve(diagnostics.size());
        for (const LinearOlsDiagnosticRow &row : diagnostics) {
            const double residual = LinearResidualForDiagnostic(row, residualType);
            const double quantile = residualType == "studentized"
                ? row.qqStudentizedQuantile
                : residualType == "standardized"
                    ? row.qqStandardizedQuantile : row.qqRawQuantile;
            if (std::isfinite(residual) && std::isfinite(quantile))
                residuals.push_back({residual, quantile, row.row});
        }
        if (residuals.size() < 2) {
            data.message = "Normal Q-Q needs at least two finite residuals and R-computed theoretical quantiles. Refit this model.";
            return data;
        }
        std::sort(residuals.begin(), residuals.end(),
                  [](const auto &left, const auto &right) { return left.residual < right.residual; });
        data.title = "Normal Q-Q of residuals";
        data.xLabel = "theoretical quantile";
        data.yLabel = LinearResidualLabel(residualType);
        for (const auto &point : residuals) {
            data.points.push_back(DiagnosticPlotPoint{
                point.quantile, point.residual, point.row});
        }
    } else if (data.diagnosticKind == "scale_location") {
        data.title = "Scale-location";
        data.xLabel = "fitted";
        data.yLabel = "sqrt(|" + LinearResidualLabel(residualType) + "|)";
        for (const LinearOlsDiagnosticRow &row : diagnostics) {
            const double residual = LinearResidualForDiagnostic(row, residualType);
            const double value = std::isfinite(residual)
                ? std::sqrt(std::fabs(residual))
                : std::numeric_limits<double>::quiet_NaN();
            if (std::isfinite(row.fitted) && std::isfinite(value)) {
                data.points.push_back(DiagnosticPlotPoint{row.fitted, value, row.row});
            }
        }
    } else if (data.diagnosticKind == "residuals_leverage") {
        data.title = "Residuals vs leverage";
        data.xLabel = "leverage";
        data.yLabel = LinearResidualLabel(residualType);
        for (const LinearOlsDiagnosticRow &row : diagnostics) {
            const double residual = LinearResidualForDiagnostic(row, residualType);
            if (std::isfinite(row.leverage) && std::isfinite(residual)) {
                data.points.push_back(DiagnosticPlotPoint{row.leverage, residual, row.row});
            }
        }
    } else if (data.diagnosticKind == "cooks_distance") {
        data.title = "Cook's distance";
        data.xLabel = "case";
        data.yLabel = "Cook's distance";
        for (const LinearOlsDiagnosticRow &row : diagnostics) {
            if (std::isfinite(row.cooksDistance)) {
                data.points.push_back(DiagnosticPlotPoint{
                    static_cast<double>(row.row), row.cooksDistance, row.row});
            }
        }
    }
    data.ok = !data.points.empty();
    if (!data.ok) {
        data.message = "The requested diagnostic has no finite points for this fitted model.";
    }
    return data;
}

DiagnosticPlotData BuildBoundedCountDistributionDiagnosticPlotData(
    const std::vector<BoundedCountDistributionPoint> &distribution,
    int fitVersion,
    int diagnosticsVersion)
{
    DiagnosticPlotData data;
    data.kind = "glm_interaction";
    data.diagnosticKind = "observed_predicted_score_distribution";
    data.title = "Observed vs predicted distribution";
    data.xLabel = "Response";
    data.yLabel = "Frequency";
    data.fitVersion = fitVersion;
    data.diagnosticsVersion = diagnosticsVersion;

    InteractionPlotLine observed;
    observed.label = "Observed";
    observed.colorKey = "#0072B2";
    InteractionPlotLine predicted;
    predicted.label = "Predicted";
    predicted.colorKey = "#D55E00";
    for (const BoundedCountDistributionPoint &value : distribution) {
        if (!std::isfinite(value.score)) continue;
        if (std::isfinite(value.observedFrequency)) {
            const DataPoint point{value.score, value.observedFrequency, 0};
            observed.points.push_back(point);
            data.points.push_back(DiagnosticPlotPoint{point.x, point.y, 0});
        }
        if (std::isfinite(value.predictedFrequency)) {
            const DataPoint point{value.score, value.predictedFrequency, 0};
            predicted.points.push_back(point);
            data.points.push_back(DiagnosticPlotPoint{point.x, point.y, 0});
        }
    }
    if (!observed.points.empty()) data.lines.push_back(std::move(observed));
    if (!predicted.points.empty()) data.lines.push_back(std::move(predicted));
    data.ok = data.lines.size() == 2;
    if (!data.ok) {
        data.message = "The observed and predicted response frequencies are unavailable.";
    }
    return data;
}

DiagnosticPlotData BuildDiscreteBoundaryDiagnosticPlotData(
    const std::vector<BoundedCountDistributionPoint> &distribution,
    bool includeCeiling,
    int fitVersion,
    int diagnosticsVersion)
{
    DiagnosticPlotData data;
    data.kind = "glm_interaction";
    data.diagnosticKind = "boundary_zero_fit";
    data.title = includeCeiling ? "Floor / ceiling fit" : "Zero fit";
    data.xLabel = includeCeiling ? "Boundary response" : "Zero";
    data.yLabel = "Frequency";
    data.fitVersion = fitVersion;
    data.diagnosticsVersion = diagnosticsVersion;
    if (distribution.empty()) {
        data.message = "Boundary calibration is unavailable for this model.";
        return data;
    }
    InteractionPlotLine observed;
    observed.label = "Observed";
    observed.colorKey = "#0072B2";
    InteractionPlotLine predicted;
    predicted.label = "Predicted";
    predicted.colorKey = "#D55E00";
    auto append = [&](const BoundedCountDistributionPoint &value) {
        if (std::isfinite(value.observedFrequency)) {
            observed.points.push_back(DataPoint{
                value.score, value.observedFrequency, 0});
            data.points.push_back(DiagnosticPlotPoint{
                value.score, value.observedFrequency, 0});
        }
        if (std::isfinite(value.predictedFrequency)) {
            predicted.points.push_back(DataPoint{
                value.score, value.predictedFrequency, 0});
            data.points.push_back(DiagnosticPlotPoint{
                value.score, value.predictedFrequency, 0});
        }
    };
    append(distribution.front());
    if (includeCeiling && distribution.size() > 1) append(distribution.back());
    if (!observed.points.empty()) data.lines.push_back(std::move(observed));
    if (!predicted.points.empty()) data.lines.push_back(std::move(predicted));
    data.ok = data.lines.size() == 2;
    if (!data.ok) data.message = "Observed or predicted boundary frequencies are unavailable.";
    return data;
}

void ApplyDiagnosticIdentityRange(PlotModel &plot)
{
    if (!plot.diagnosticShowIdentityLine) return;
    const double minimum = std::min(plot.xmin, plot.ymin);
    const double maximum = std::max(plot.xmax, plot.ymax);
    if (!std::isfinite(minimum) || !std::isfinite(maximum) ||
        !(maximum > minimum)) return;
    // An observed-vs-fitted plot is only geometrically interpretable when the
    // same numeric range is used on both axes. Preserve that range for reset
    // as well as for the current viewport.
    plot.dataXmin = minimum;
    plot.dataYmin = minimum;
    plot.dataXmax = maximum;
    plot.dataYmax = maximum;
    plot.xmin = minimum;
    plot.ymin = minimum;
    plot.xmax = maximum;
    plot.ymax = maximum;
}

DiagnosticPlotData BuildGeneralizedDiagnosticPlotData(const std::string &kind,
                                                      const DiagnosticOptions &options,
                                                      const std::vector<GeneralizedDiagnosticRow> &diagnostics,
                                                      int fitVersion,
                                                      int diagnosticsVersion)
{
    DiagnosticPlotData data = BuildGeneralizedDiagnosticPlotData(
        kind, options.residualType, diagnostics, fitVersion, diagnosticsVersion);
    data.diagnosticOptionsVersion = options.version;
    data.residualType = options.residualType;
    return data;
}

DiagnosticPlotData BuildGeneralizedDiagnosticPlotData(const std::string &kind,
                                                      const std::string &residualType,
                                                      const std::vector<GeneralizedDiagnosticRow> &diagnostics,
                                                      int fitVersion,
                                                      int diagnosticsVersion)
{
    DiagnosticPlotData data;
    if (diagnostics.empty()) {
        data.message = "The current GLM has no diagnostic rows. Refit the model with at least one valid predictor.";
        return data;
    }

    data.diagnosticKind = NormalizeGeneralizedDiagnosticKind(kind);
    data.kind = "scatter";
    data.xLabel = "fitted";
    data.yLabel = "residual";
    data.title = "Residuals vs fitted";
    data.fitVersion = fitVersion;
    data.diagnosticsVersion = diagnosticsVersion;
    data.residualType = residualType;

    if (data.diagnosticKind == "observed_fitted") {
        data.title = "Observed vs fitted";
        data.xLabel = "Fitted";
        data.yLabel = "Observed";
        data.showIdentityLine = true;
        for (const GeneralizedDiagnosticRow &row : diagnostics) {
            if (std::isfinite(row.fitted) && std::isfinite(row.observed)) {
                data.points.push_back(DiagnosticPlotPoint{row.fitted, row.observed, row.row});
            }
        }
    } else if (data.diagnosticKind == "residuals_fitted") {
        for (const GeneralizedDiagnosticRow &row : diagnostics) {
            double residual = GeneralizedResidualForDiagnostic(row, residualType);
            if (std::isfinite(row.fitted) && std::isfinite(residual)) {
                data.points.push_back(DiagnosticPlotPoint{row.fitted, residual, row.row});
            }
        }
        data.yLabel = GeneralizedResidualLabel(residualType);
    } else if (data.diagnosticKind == "residual_histogram") {
        data.kind = "histogram";
        data.title = "Residual histogram";
        data.xLabel = GeneralizedResidualLabel(residualType);
        for (const GeneralizedDiagnosticRow &row : diagnostics) {
            double residual = GeneralizedResidualForDiagnostic(row, residualType);
            if (std::isfinite(residual)) {
                data.points.push_back(DiagnosticPlotPoint{residual, 0.0, row.row});
            }
        }
        if (data.points.empty()) {
            data.message = "Residual histogram is not available because residuals are not finite.";
            return data;
        }
    } else if (data.diagnosticKind == "normal_qq") {
        struct QQPoint { double residual; double quantile; int row; };
        std::vector<QQPoint> residuals;
        for (const GeneralizedDiagnosticRow &row : diagnostics) {
            double residual = GeneralizedResidualForDiagnostic(row, residualType);
            const auto quantile = row.qqTheoreticalQuantiles.find(residualType);
            if (std::isfinite(residual) &&
                quantile != row.qqTheoreticalQuantiles.end() &&
                std::isfinite(quantile->second)) {
                residuals.push_back({residual, quantile->second, row.row});
            }
        }
        if (residuals.size() < 2) {
            data.message = "Normal Q-Q needs at least two finite residuals and R-computed theoretical quantiles. Refit this model.";
            return data;
        }
        std::sort(residuals.begin(), residuals.end(),
                  [](const QQPoint &a, const QQPoint &b) {
                      return a.residual < b.residual;
                  });
        data.title = "Normal Q-Q of residuals";
        data.xLabel = "theoretical quantile";
        data.yLabel = GeneralizedResidualLabel(residualType);
        for (const auto &point : residuals) {
            data.points.push_back(DiagnosticPlotPoint{
                point.quantile, point.residual, point.row});
        }
    } else if (data.diagnosticKind == "scale_location") {
        data.title = "Scale-location";
        data.xLabel = "fitted";
        data.yLabel = "sqrt(|" + GeneralizedResidualLabel(residualType) + "|)";
        for (const GeneralizedDiagnosticRow &row : diagnostics) {
            double residual = GeneralizedResidualForDiagnostic(row, residualType);
            if (std::isfinite(row.fitted) && std::isfinite(residual)) {
                data.points.push_back(DiagnosticPlotPoint{row.fitted, std::sqrt(std::fabs(residual)), row.row});
            }
        }
    } else if (data.diagnosticKind == "residuals_leverage") {
        data.title = "Residuals vs leverage";
        data.xLabel = "leverage";
        data.yLabel = GeneralizedResidualLabel(residualType);
        for (const GeneralizedDiagnosticRow &row : diagnostics) {
            double residual = GeneralizedResidualForDiagnostic(row, residualType);
            if (std::isfinite(row.leverage) && std::isfinite(residual)) {
                data.points.push_back(DiagnosticPlotPoint{row.leverage, residual, row.row});
            }
        }
        if (data.points.empty()) {
            data.message = "Residuals vs leverage is not available for this fitted GLM.";
            return data;
        }
    } else if (data.diagnosticKind == "cooks_distance") {
        data.title = "Cook's distance";
        data.xLabel = "case";
        data.yLabel = "Cook's distance";
        for (const GeneralizedDiagnosticRow &row : diagnostics) {
            if (std::isfinite(row.cooksDistance)) {
                data.points.push_back(DiagnosticPlotPoint{static_cast<double>(row.row), row.cooksDistance, row.row});
            }
        }
        if (data.points.empty()) {
            data.message = "Cook's distance is not available for this fitted GLM.";
            return data;
        }
    } else if (data.diagnosticKind == "roc_curve") {
        std::vector<const GeneralizedDiagnosticRow *> ordered;
        int events = 0;
        int references = 0;
        for (const GeneralizedDiagnosticRow &row : diagnostics) {
            if (!std::isfinite(row.fitted) || !std::isfinite(row.observedBinary)) continue;
            ordered.push_back(&row);
            if (row.observedBinary >= 0.5) ++events; else ++references;
        }
        if (events == 0 || references == 0) {
            data.message = "ROC is not available because both response categories are required.";
            return data;
        }
        std::sort(ordered.begin(), ordered.end(), [](const auto *left, const auto *right) {
            return left->fitted > right->fitted;
        });
        data.title = "ROC curve";
        data.xLabel = "false positive rate";
        data.yLabel = "true positive rate";
        int tp = 0;
        int fp = 0;
        data.visualPoints.push_back(DiagnosticPlotPoint{0.0, 0.0, 0});
        std::size_t cursor = 0;
        while (cursor < ordered.size()) {
            const double threshold = ordered[cursor]->fitted;
            std::size_t end = cursor;
            DiagnosticROCThreshold empirical;
            empirical.threshold = threshold;
            while (end < ordered.size() && ordered[end]->fitted == threshold) {
                const GeneralizedDiagnosticRow *row = ordered[end];
                empirical.crossingRows.push_back(row->row);
                if (row->observedBinary >= 0.5) ++tp; else ++fp;
                ++end;
            }
            empirical.truePositive = tp;
            empirical.falsePositive = fp;
            empirical.trueNegative = references - fp;
            empirical.falseNegative = events - tp;
            empirical.falsePositiveRate = static_cast<double>(fp) / references;
            empirical.truePositiveRate = static_cast<double>(tp) / events;
            for (const GeneralizedDiagnosticRow *candidate : ordered) {
                const bool predictedEvent = candidate->fitted >= threshold;
                const bool observedEvent = candidate->observedBinary >= 0.5;
                if (predictedEvent && !observedEvent) empirical.falsePositiveRows.push_back(candidate->row);
                if (!predictedEvent && observedEvent) empirical.falseNegativeRows.push_back(candidate->row);
            }
            const DiagnosticPlotPoint &previous = data.visualPoints.back();
            if (empirical.falsePositiveRate != previous.x) {
                data.visualPoints.push_back(DiagnosticPlotPoint{
                    empirical.falsePositiveRate, previous.y, 0});
            }
            if (empirical.truePositiveRate != data.visualPoints.back().y) {
                data.visualPoints.push_back(DiagnosticPlotPoint{
                    empirical.falsePositiveRate, empirical.truePositiveRate, 0});
            }
            data.points.push_back(DiagnosticPlotPoint{
                empirical.falsePositiveRate,
                empirical.truePositiveRate,
                empirical.crossingRows.empty() ? 0 : empirical.crossingRows.front()
            });
            data.rocThresholds.push_back(std::move(empirical));
            cursor = end;
        }
    } else if (data.diagnosticKind == "calibration_plot") {
        data.title = "Observed vs fitted probability";
        data.xLabel = "fitted event probability";
        data.yLabel = "observed event";
        for (const GeneralizedDiagnosticRow &row : diagnostics) {
            if (std::isfinite(row.fitted) && std::isfinite(row.observedBinary)) {
                data.points.push_back(DiagnosticPlotPoint{row.fitted, row.observedBinary, row.row});
            }
        }
    } else {
        data.message = "Diagnostic `" + data.diagnosticKind + "` is not available for generalized linear models.";
        return data;
    }

    data.ok = !data.points.empty();
    if (!data.ok) {
        data.message = "The requested diagnostic has no finite points for this fitted GLM.";
    }
    return data;
}

OutputCodeReference GeneralizedDiagnosticPlotCodeReference(
    const PlotModel &plot,
    const GeneralizedGLMState &state)
{
    OutputCodeReference output;
    output.outputId = plot.id;
    output.analysisId = state.provenance.analysisId;
    output.outputBlockId = "diagnostic:" + plot.glmDiagnosticKind;
    output.title = plot.title;
    output.kind = "plot";
    output.provenance = state.provenance;

    const auto source = state.provenance.verificationRCode.find("model");
    if (source == state.provenance.verificationRCode.end() || source->second.empty()) {
        output.provenance.verificationWarnings.push_back(
            "The diagnostic has no portable model recipe to extend.");
        return output;
    }

    const std::string distribution = state.binaryRegression
        ? "binary" : state.countRegression
            ? CountDistributionId(state.countDistribution) : state.family;
    const std::string residual = plot.displayedResidualType.empty()
        ? DefaultGeneralizedResidualType(state) : plot.displayedResidualType;
    const int imputation = std::max(1, plot.diagnosticImputationIndex);
    std::ostringstream code;
    code << source->second;
    if (source->second.back() != '\n') code << '\n';
    code
        << "if (!requireNamespace(\"ggplot2\", quietly = TRUE))\n"
           "  stop(\"Install package 'ggplot2' to draw this diagnostic.\")\n"
           "diagnostic_models <- if (exists(\"reference_fits\", inherits = FALSE)) reference_fits else\n"
           "  if (exists(\"fits\", inherits = FALSE)) fits else\n"
           "  if (exists(\"reference_fit\", inherits = FALSE)) list(reference_fit) else\n"
           "  if (exists(\"fit\", inherits = FALSE)) list(fit) else\n"
           "  stop(\"The verification model was not created.\")\n"
        << "diagnostic_imputation <- " << imputation
        << "L\nstopifnot(diagnostic_imputation <= length(diagnostic_models))\n"
           "diagnostic_fit <- diagnostic_models[[diagnostic_imputation]]\n"
           "diagnostic_frame <- stats::model.frame(diagnostic_fit)\n"
           "diagnostic_response <- stats::model.response(diagnostic_frame)\n"
        << "diagnostic_distribution <- " << ProvenanceRStringLiteral(distribution) << "\n"
        << "diagnostic_kind <- " << ProvenanceRStringLiteral(plot.glmDiagnosticKind) << "\n"
        << "diagnostic_residual <- " << ProvenanceRStringLiteral(residual) << "\n"
           "fitted_mean <- as.numeric(stats::fitted(diagnostic_fit))\n"
           "if (is.matrix(diagnostic_response) && ncol(diagnostic_response) == 2L) {\n"
           "  observed <- as.numeric(diagnostic_response[, 1L])\n"
           "  trials <- as.numeric(rowSums(diagnostic_response))\n"
           "} else {\n"
           "  observed <- as.numeric(diagnostic_response)\n"
           "  trials <- rep(1, length(observed))\n"
           "}\n";

    if (state.countRegression &&
        (state.countDistribution == CountDistribution::BetaBinomial ||
         state.countDistribution == CountDistribution::HurdleBetaBinomialCeiling)) {
        code
            << "diagnostic_sources <- if (exists(\"completed_sets\", inherits = FALSE)) completed_sets else\n"
               "  if (exists(\"prepared_sets\", inherits = FALSE)) prepared_sets else list(diagnostic_frame)\n"
               "diagnostic_source <- diagnostic_sources[[min(diagnostic_imputation, length(diagnostic_sources))]]\n"
               "used_rows <- suppressWarnings(as.integer(rownames(diagnostic_frame)))\n"
               "if (length(used_rows) != length(fitted_mean) || anyNA(used_rows))\n"
               "  used_rows <- seq_along(fitted_mean)\n"
            << "observed <- as.numeric(diagnostic_source[["
            << ProvenanceRStringLiteral(state.response) << "]][used_rows])\n";
        if (!state.trialsVariable.empty()) {
            code << "trials <- as.numeric(diagnostic_source[["
                 << ProvenanceRStringLiteral(state.trialsVariable)
                 << "]][used_rows])\n";
        } else {
            code << "trials <- rep(" << std::setprecision(17) << state.trialsConstant
                 << ", length(observed))\n";
        }
    }

    code
        << "if (diagnostic_distribution == \"binary\") {\n"
           "  trials <- rep(1, length(observed)); probability <- fitted_mean\n"
           "  row_pmf <- function(k, i) stats::dbinom(k, 1, probability[i])\n"
           "  row_cdf <- function(k, i) stats::pbinom(k, 1, probability[i])\n"
           "  expected <- probability; variance <- probability * (1 - probability)\n"
           "} else if (diagnostic_distribution == \"perfect_score\") {\n"
           "  trials <- rep(1, length(observed)); probability <- fitted_mean\n"
           "  row_pmf <- function(k, i) stats::dbinom(k, 1, probability[i])\n"
           "  row_cdf <- function(k, i) stats::pbinom(k, 1, probability[i])\n"
           "  expected <- probability; variance <- probability * (1 - probability)\n"
           "} else if (diagnostic_distribution == \"poisson\") {\n"
           "  row_pmf <- function(k, i) stats::dpois(k, fitted_mean[i])\n"
           "  row_cdf <- function(k, i) stats::ppois(k, fitted_mean[i])\n"
           "  expected <- fitted_mean; variance <- fitted_mean\n"
           "} else if (diagnostic_distribution == \"negative_binomial\") {\n"
           "  theta <- diagnostic_fit$theta\n"
           "  row_pmf <- function(k, i) stats::dnbinom(k, mu = fitted_mean[i], size = theta)\n"
           "  row_cdf <- function(k, i) stats::pnbinom(k, mu = fitted_mean[i], size = theta)\n"
           "  expected <- fitted_mean; variance <- fitted_mean + fitted_mean^2 / theta\n"
           "} else if (diagnostic_distribution == \"binomial_trials\") {\n"
           "  probability <- fitted_mean\n"
           "  row_pmf <- function(k, i) stats::dbinom(k, trials[i], probability[i])\n"
           "  row_cdf <- function(k, i) stats::pbinom(k, trials[i], probability[i])\n"
           "  expected <- trials * probability\n"
           "  variance <- trials * probability * (1 - probability)\n"
           "} else if (diagnostic_distribution == \"beta_binomial\") {\n"
           "  probability <- as.numeric(stats::predict(diagnostic_fit, type = \"response\"))\n"
           "  precision <- as.numeric(stats::sigma(diagnostic_fit))\n"
           "  row_pmf <- function(k, i) gamlss.dist::dBB(k, bd=trials[i], mu=probability[i], sigma=1/precision)\n"
           "  row_cdf <- function(k, i) gamlss.dist::pBB(k, bd=trials[i], mu=probability[i], sigma=1/precision)\n"
           "  expected <- trials * probability\n"
           "  variance <- trials * probability * (1 - probability) *\n"
           "    (trials + precision) / (1 + precision)\n"
           "} else if (diagnostic_distribution == \"hurdle_beta_binomial_ceiling\") {\n"
           "  mu_failure <- as.numeric(diagnostic_fit$mu.fv)\n"
           "  sigma <- as.numeric(diagnostic_fit$sigma.fv)\n"
           "  perfect_probability <- as.numeric(diagnostic_fit$nu.fv)\n"
           "  row_pmf <- function(k, i) gamlss.dist::dZABB(trials[i]-k, mu=mu_failure[i], sigma=sigma[i], nu=perfect_probability[i], bd=trials[i])\n"
           "  row_cdf <- function(k, i) 1-gamlss.dist::pZABB(trials[i]-k-1, mu=mu_failure[i], sigma=sigma[i], nu=perfect_probability[i], bd=trials[i])\n"
           "  moments <- lapply(seq_along(observed), function(i) {\n"
           "    support <- 0:trials[i]; probability <- vapply(support, row_pmf, numeric(1L), i = i)\n"
           "    mean <- sum(support * probability); c(mean, sum((support - mean)^2 * probability))\n"
           "  })\n"
           "  expected <- vapply(moments, `[[`, numeric(1L), 1L)\n"
           "  variance <- vapply(moments, `[[`, numeric(1L), 2L)\n"
           "} else {\n"
           "  expected <- fitted_mean\n"
           "  dispersion <- summary(diagnostic_fit)$dispersion\n"
           "  variance <- pmax(.Machine$double.eps, fitted_mean * dispersion)\n"
           "  row_pmf <- row_cdf <- NULL\n"
           "}\n"
           "raw_residual <- observed - expected\n"
           "pearson_residual <- raw_residual / sqrt(variance)\n"
           "deviance_residual <- rep(NA_real_, length(observed))\n"
        << (std::find(plot.diagnosticAvailableResidualTypes.begin(),
                      plot.diagnosticAvailableResidualTypes.end(),
                      "deviance") != plot.diagnosticAvailableResidualTypes.end()
            ? "deviance_residual <- tryCatch(suppressWarnings(as.numeric(stats::residuals(\n"
              "  diagnostic_fit, type = \"deviance\"))), error = function(e) rep(NA_real_, length(observed)))\n"
            : "")
        <<
           [&]() {
            const auto recipe = state.provenance.verificationRCode.find("diagnostic_randomization");
            if (recipe != state.provenance.verificationRCode.end() && !recipe->second.empty())
                return recipe->second + "\n";
            return std::string(
                "stop('This legacy fit has no captured diagnostic seed. Refit before exporting diagnostics.')\n");
        }()
        << "dunn_smyth_residual <- rep(NA_real_, length(observed))\n"
           "if (is.function(row_pmf)) {\n"
           "  lower <- vapply(seq_along(observed), function(i) row_cdf(observed[i] - 1, i), numeric(1L))\n"
           "  upper <- vapply(seq_along(observed), function(i) row_cdf(observed[i], i), numeric(1L))\n"
        << "  u <- diagnostic_uniform\n"
           "  dunn_smyth_residual <- if (inherits(diagnostic_fit, \"glm\") && exists(\"diagnostic_statmod_residual\", inherits=FALSE))\n"
           "    diagnostic_statmod_residual(diagnostic_fit) else\n"
           "    diagnostic_cdf_residual(lower, upper)\n"
           "}\n"
           "diagnostic_values <- switch(diagnostic_residual, dunn_smyth = dunn_smyth_residual,\n"
           "  pearson = pearson_residual, raw = raw_residual, deviance = deviance_residual,\n"
           "  pearson_residual)\n"
           "linkeda_plot_theme <- " << Ggplot2ThemeRExpression(plot.rExportTheme) << "\n"
           "diagnostic_data <- data.frame(row_id = diagnostic_row_ids, observed, fitted = expected, residual = diagnostic_values)\n"
           "if (diagnostic_distribution == \"negative_binomial\" && exists(\"diagnostic_maximum\", inherits=FALSE) && is.finite(diagnostic_maximum)) {\n"
           "  above_maximum <- stats::pnbinom(diagnostic_maximum,mu=fitted_mean,size=diagnostic_fit$theta,lower.tail=FALSE)\n"
           "  print(data.frame(imputation=diagnostic_imputation,maximum=diagnostic_maximum,mean_probability=mean(above_maximum),maximum_probability=max(above_maximum)))\n"
           "  # Descriptive model probabilities, not a p value or a pooled diagnostic.\n"
           "}\n"
           "if (diagnostic_kind == \"observed_fitted\") {\n"
           "  diagnostic_plot <- ggplot2::ggplot(diagnostic_data, ggplot2::aes(fitted, observed)) +\n"
           "    ggplot2::geom_point() + ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +\n"
           "    ggplot2::coord_equal()\n"
           "} else if (diagnostic_kind == \"residual_histogram\") {\n"
           "  diagnostic_plot <- ggplot2::ggplot(diagnostic_data, ggplot2::aes(residual)) +\n"
        << "    ggplot2::geom_histogram(bins = " << std::max<std::size_t>(1, plot.histogramBins.size())
        << ", colour = \"white\")\n"
           "} else if (diagnostic_kind == \"normal_qq\") {\n"
           "  diagnostic_plot <- ggplot2::ggplot(diagnostic_data, ggplot2::aes(sample = residual)) +\n"
           "    ggplot2::stat_qq() + ggplot2::stat_qq_line()\n"
           "} else if (diagnostic_kind %in% c(\"observed_predicted_score_distribution\", \"boundary_zero_fit\")) {\n"
           "  finite_support <- diagnostic_distribution %in% c(\"binary\", \"perfect_score\",\n"
           "    \"binomial_trials\", \"beta_binomial\", \"hurdle_beta_binomial_ceiling\")\n"
           "  support_max <- if (finite_support) max(trials) else max(observed,\n"
           "    vapply(seq_along(observed), function(i) {\n"
           "      if (diagnostic_distribution == \"poisson\") stats::qpois(.9995, fitted_mean[i])\n"
           "      else stats::qnbinom(.9995, mu = fitted_mean[i], size = diagnostic_fit$theta)\n"
           "    }, numeric(1L)))\n"
           "  support <- 0:ceiling(support_max)\n"
           "  predicted_frequency <- vapply(support, function(k)\n"
           "    sum(vapply(seq_along(observed), function(i) row_pmf(k, i), numeric(1L))), numeric(1L))\n"
           "  if (!finite_support) predicted_frequency[length(support)] <-\n"
           "    sum(vapply(seq_along(observed), function(i) 1 - row_cdf(tail(support, 1L) - 1, i), numeric(1L)))\n"
           "  observed_frequency <- vapply(support, function(k) sum(observed == k), numeric(1L))\n"
           "  distribution_data <- rbind(\n"
           "    data.frame(response = support, frequency = observed_frequency, series = \"Observed\"),\n"
           "    data.frame(response = support, frequency = predicted_frequency, series = \"Predicted\"))\n"
           "  if (diagnostic_kind == \"boundary_zero_fit\") {\n"
           "    boundary <- support == 0 | (finite_support & support == max(trials))\n"
           "    distribution_data <- distribution_data[rep(boundary, 2L), , drop = FALSE]\n"
           "  }\n"
           "  diagnostic_plot <- ggplot2::ggplot(distribution_data,\n"
           "    ggplot2::aes(response, frequency, colour = series, group = series)) +\n"
           "    ggplot2::geom_line() + ggplot2::geom_point()\n"
           "} else {\n"
           "  diagnostic_plot <- ggplot2::ggplot(diagnostic_data, ggplot2::aes(fitted, residual)) +\n"
           "    ggplot2::geom_point() + ggplot2::geom_hline(yintercept = 0, linetype = 2)\n"
           "}\n"
        ;
    if (plot.glmDiagnosticKind == "residual_histogram" && plot.histogramShowDensity && plot.histogramDensityMode == "all") {
        const double width = plot.histogramBins.empty() ? 1.0 : plot.histogramBins.front().upper - plot.histogramBins.front().lower;
        code << "diagnostic_plot <- diagnostic_plot + ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(count * "
             << std::setprecision(17) << width << ")), bw = "
             << (plot.histogramDensityBw > 0 ? std::to_string(plot.histogramDensityBw) : "\"nrd0\"")
             << ", adjust = " << plot.histogramDensityAdjust << ")\n";
    }
    code
        << "diagnostic_plot <- diagnostic_plot + ggplot2::labs(title = "
        << ProvenanceRStringLiteral(plot.title) << ", x = "
        << ProvenanceRStringLiteral(plot.xLabel) << ", y = "
        << ProvenanceRStringLiteral(plot.yLabel)
        << ") + linkeda_plot_theme\nprint(diagnostic_plot)\n";

    output.provenance.verificationRCode[output.outputBlockId] = code.str();
    output.provenance.outputRCode[output.outputBlockId] =
        "# Diagnostic values are computed by the R model fit; the native layer only renders them.";
    output.publication.availableBackends = {PublicationBackend::Ggplot2};
    return output;
}

namespace {

DiagnosticPlotData BuildDiagnosticPlotDataAcrossImputationsImpl(
    const std::vector<DiagnosticPlotData> &perImputation,
    const std::set<int> *rowsWithImputedModelInputs)
{
    DiagnosticPlotData combined;
    if (perImputation.size() < 2) {
        combined.message = "Imputation uncertainty requires at least two independently fitted imputations.";
        return combined;
    }
    const DiagnosticPlotData &first = perImputation.front();
    combined = first;
    combined.points.clear();
    combined.visualPoints.clear();
    combined.rocThresholds.clear();
    combined.imputationValues.clear();
    if (!first.ok || first.kind != "scatter" ||
        first.diagnosticKind == "roc_curve" || first.diagnosticKind == "normal_qq") {
        combined.ok = false;
        combined.message =
            "Imputation uncertainty is available only for row-wise diagnostic scatterplots.";
        return combined;
    }
    for (const DiagnosticPlotData &data : perImputation) {
        if (!data.ok || data.kind != "scatter" ||
            data.diagnosticKind != first.diagnosticKind ||
            data.residualType != first.residualType) {
            combined.ok = false;
            combined.message =
                "The per-imputation diagnostic results do not describe the same diagnostic specification.";
            return combined;
        }
    }

    std::vector<std::map<int, Point>> pointsByImputation;
    pointsByImputation.reserve(perImputation.size());
    for (const DiagnosticPlotData &data : perImputation) {
        std::map<int, Point> byRow;
        for (const DiagnosticPlotPoint &point : data.points) {
            if (point.row > 0 && std::isfinite(point.x) && std::isfinite(point.y)) {
                byRow[point.row] = Point{point.x, point.y};
            }
        }
        pointsByImputation.push_back(std::move(byRow));
    }
    for (const auto &entry : pointsByImputation.front()) {
        ScatterplotPointImputationValues values;
        values.row = entry.first;
        double xSum = 0.0;
        double ySum = 0.0;
        bool complete = true;
        for (const auto &byRow : pointsByImputation) {
            const auto found = byRow.find(entry.first);
            if (found == byRow.end()) {
                complete = false;
                break;
            }
            values.values.push_back(found->second);
            xSum += found->second.x;
            ySum += found->second.y;
        }
        if (!complete) continue;
        const double count = static_cast<double>(values.values.size());
        combined.points.push_back(DiagnosticPlotPoint{
            xSum / count, ySum / count, entry.first});
        if (!rowsWithImputedModelInputs ||
            rowsWithImputedModelInputs->find(entry.first) !=
                rowsWithImputedModelInputs->end()) {
            combined.imputationValues.push_back(std::move(values));
        }
    }
    combined.ok = !combined.points.empty();
    if (combined.ok) {
        combined.title = first.title +
            (rowsWithImputedModelInputs
                ? " — Direct imputation uncertainty (m = "
                : " — Imputation uncertainty (m = ") +
            std::to_string(perImputation.size()) +
            (rowsWithImputedModelInputs
                ? ")"
                : "; red = imputed cases, black = fitted propagation)");
        combined.message.clear();
    } else {
        combined.message =
            "No observations have compatible diagnostic coordinates in every imputation.";
    }
    return combined;
}

} // namespace

DiagnosticPlotData BuildDiagnosticPlotDataAcrossImputations(
    const std::vector<DiagnosticPlotData> &perImputation)
{
    return BuildDiagnosticPlotDataAcrossImputationsImpl(perImputation, nullptr);
}

DiagnosticPlotData BuildDiagnosticPlotDataAcrossImputations(
    const std::vector<DiagnosticPlotData> &perImputation,
    const std::set<int> &rowsWithImputedModelInputs)
{
    return BuildDiagnosticPlotDataAcrossImputationsImpl(
        perImputation, &rowsWithImputedModelInputs);
}

bool DiagnosticImputationUncertaintyShouldBeDefault(
    const DataFrameModel &dataframe,
    const std::string &diagnosticKind)
{
    return DataFrameShowsAllImputations(dataframe) &&
        dataframe.imputationCount > 1 &&
        diagnosticKind != "roc_curve" &&
        diagnosticKind != "normal_qq" &&
        diagnosticKind != "residual_histogram" &&
        diagnosticKind != "histogram" &&
        diagnosticKind != "observed_predicted_score_distribution";
}

bool DiagnosticRequiresAcceptedMultipleImputationFit(
    const DataFrameModel *dataframe,
    bool modelUsesMultipleImputation)
{
    return modelUsesMultipleImputation ||
        (dataframe && dataframe->datasetType == "multiple_imputation" &&
         dataframe->imputationCount > 1);
}

std::set<int> RowsWithImputedModelInputs(
    const DataFrameModel &dataframe,
    const ModelSpecification &specification,
    const std::vector<std::string> &additionalVariables)
{
    std::set<int> rows;
    if (dataframe.datasetType != "multiple_imputation") return rows;

    std::set<std::string> variables;
    if (!specification.response.empty()) variables.insert(specification.response);
    for (const std::string &term : specification.terms) {
        for (const std::string &variable : UniqueBaseVariablesForTerm(term)) {
            if (!variable.empty()) variables.insert(variable);
        }
    }
    for (const std::string &variable : additionalVariables) {
        if (!variable.empty()) variables.insert(variable);
    }

    for (const std::string &variable : variables) {
        const DataColumn *column = FindDataColumnInDataFrame(dataframe, variable);
        if (!column) continue;
        const std::size_t rowCount = std::min(
            static_cast<std::size_t>(std::max(0, dataframe.rows)),
            column->imputedMissing.size());
        for (std::size_t row = 0; row < rowCount; ++row) {
            if (column->imputedMissing[row]) {
                rows.insert(static_cast<int>(row + 1));
            }
        }
    }
    return rows;
}

bool RegressionComparisonUsesImputedModelInputs(
    const RegressionComparisonState &state,
    const DataFrameModel &dataframe)
{
    if (!state.multipleImputation ||
        dataframe.datasetType != "multiple_imputation") return false;
    for (const RegressionComparisonModel &model : state.models) {
        const ModelSpecification specification =
            EffectiveRegressionComparisonModelSpecification(state, model);
        if (!RowsWithImputedModelInputs(dataframe, specification).empty()) {
            return true;
        }
    }
    return false;
}

std::string RegressionComparisonMultipleImputationNote(
    const RegressionComparisonState &state,
    const DataFrameModel *dataframe)
{
    if (!state.multipleImputation) return state.note;
    if (dataframe && !RegressionComparisonUsesImputedModelInputs(state, *dataframe)) {
        return "Multiple-imputation dataset, but no response or predictor value used by "
            "the compared models was imputed; Rubin's rules were not required or applied, "
            "and ordinary fits and nested F tests are identical across imputations.";
    }
    if (!state.note.empty()) return state.note;
    return "Pooled multiple-imputation General Linear Model comparison; coefficients use "
        "mice::pool() (Rubin's rules). The reported nested-model method is the "
        "actual mice::D1 or explicit mice::D3 fallback returned by R.";
}

std::vector<std::string> AvailableGeneralizedResidualTypes(const GeneralizedGLMState &state)
{
    if (state.binaryRegression) {
        return {"dunn_smyth", "pearson", "deviance", "raw"};
    }
    if (state.countRegression) {
        if (state.countDistribution == CountDistribution::BetaBinomial ||
            state.countDistribution == CountDistribution::HurdleBetaBinomialCeiling) {
            return {"dunn_smyth", "pearson", "raw"};
        }
        if (state.countDistribution == CountDistribution::QuasiPoisson) {
            return {"pearson", "deviance", "raw"};
        }
        return {"dunn_smyth", "pearson", "deviance", "raw"};
    }
    if (state.family == "binomial") {
        return {"dunn_smyth", "pearson", "deviance", "raw"};
    }
    if (state.family == "quasibinomial") return {"pearson", "deviance", "raw"};
    if (state.family == "poisson") return {"dunn_smyth", "pearson", "deviance", "raw"};
    if (state.family == "quasipoisson") return {"pearson", "deviance", "raw"};
    if (state.family == "beta_one_inflated") return {"deviance"};
    if (state.family == "beta") return {"deviance", "pearson"};
    // R returns all five vectors for the generalized families currently
    // supported by LinkEDA, including Poisson, quasi-Poisson and negative
    // binomial count models.  Keeping the declaration here prevents each
    // native frontend from maintaining a different residual list.
    return {"deviance", "pearson", "working", "standardized", "studentized"};
}

std::string DefaultGeneralizedResidualType(const GeneralizedGLMState &state)
{
    const auto available = AvailableGeneralizedResidualTypes(state);
    if (std::find(available.begin(), available.end(), "dunn_smyth") != available.end()) {
        return "dunn_smyth";
    }
    return available.empty() ? "deviance" : available.front();
}

std::string GeneralizedResidualTypeLabel(const std::string &residualType)
{
    if (residualType == "dunn_smyth") return GLMDunnSmythResidualTitle();
    if (residualType == "pearson") return GLMPearsonResidualTitle();
    if (residualType == "working") return GLMWorkingResidualTitle();
    if (residualType == "raw") return GLMRawResidualTitle();
    if (residualType == "standardized") return "Standardized residuals";
    if (residualType == "studentized") return "Studentized residuals";
    return GLMDevianceResidualTitle();
}

bool DiagnosticPlotUsesResidualChoice(const PlotModel &plot)
{
    if (!plot.isGLMDiagnostic) return false;
    const auto &kind = plot.glmDiagnosticKind;
    return kind == "residuals_fitted" || kind == "residual_histogram" ||
        kind == "normal_qq" || kind == "scale_location" ||
        kind == "residuals_leverage" || kind == "partial_regression";
}

bool DiagnosticPlotAxisUsesResidualChoice(const PlotModel &plot, bool xAxis)
{
    return DiagnosticPlotUsesResidualChoice(plot) &&
        (xAxis == (plot.glmDiagnosticKind == "residual_histogram"));
}

std::vector<std::string> DiagnosticPlotResidualChoices(const PlotModel &plot)
{
    if (!DiagnosticPlotUsesResidualChoice(plot)) return {};
    if (!plot.diagnosticAvailableResidualTypes.empty()) return plot.diagnosticAvailableResidualTypes;
    return plot.generalizedDiagnosticResiduals ? std::vector<std::string>{} : AvailableLinearResidualTypes();
}

std::vector<std::string> AvailableLinearResidualTypes()
{
    return {"raw", "standardized", "studentized"};
}

bool IsGeneralizedResidualTypeAvailable(const GeneralizedGLMState &state,
                                        const std::string &residualType)
{
    const auto available = AvailableGeneralizedResidualTypes(state);
    return std::find(available.begin(), available.end(), residualType) != available.end();
}

std::vector<double> SubtractVectors(const std::vector<double> &left,
                                    const std::vector<double> &right)
{
    std::vector<double> out;
    if (left.size() != right.size()) return out;
    out.reserve(left.size());
    for (std::size_t i = 0; i < left.size(); ++i) {
        out.push_back(left[i] - right[i]);
    }
    return out;
}

double ContrastEstimate(const std::vector<double> &beta,
                        const std::vector<double> &contrast)
{
    if (contrast.size() != beta.size()) return NAN;
    double value = 0.0;
    for (std::size_t i = 0; i < contrast.size(); ++i) {
        value += contrast[i] * beta[i];
    }
    return value;
}

double ContrastVariance(const std::vector<std::vector<double>> &covariance,
                        const std::vector<double> &contrast)
{
    if (covariance.size() != contrast.size()) return NAN;
    double value = 0.0;
    for (std::size_t i = 0; i < contrast.size(); ++i) {
        if (covariance[i].size() != contrast.size()) return NAN;
        for (std::size_t j = 0; j < contrast.size(); ++j) {
            value += contrast[i] * covariance[i][j] * contrast[j];
        }
    }
    return value;
}

LinearFunctionEstimate EstimateLinearFunction(const std::vector<double> &beta,
                                              const std::vector<std::vector<double>> &covariance,
                                              const std::vector<double> &contrast,
                                              const std::string &label,
                                              double confidenceLevel)
{
    LinearFunctionEstimate estimate;
    estimate.label = label;
    estimate.estimate = ContrastEstimate(beta, contrast);
    double variance = ContrastVariance(covariance, contrast);
    estimate.stdError = std::isfinite(variance) ? std::sqrt(std::max(0.0, variance)) : NAN;
    estimate.statistic = estimate.stdError > 0.0 ? estimate.estimate / estimate.stdError : NAN;
    estimate.pValue = NormalTwoSidedP(estimate.statistic);
    double z = NormalQuantileApprox(0.5 + confidenceLevel / 2.0);
    if (std::isfinite(z) && std::isfinite(estimate.stdError)) {
        estimate.ciLower = estimate.estimate - z * estimate.stdError;
        estimate.ciUpper = estimate.estimate + z * estimate.stdError;
    }
    return estimate;
}

std::vector<double> HolmAdjustedPValues(const std::vector<double> &pValues)
{
    std::vector<double> adjusted(pValues.size(), NAN);
    std::vector<std::pair<double, std::size_t>> finite;
    for (std::size_t i = 0; i < pValues.size(); ++i) {
        if (std::isfinite(pValues[i])) {
            finite.push_back(std::make_pair(pValues[i], i));
        }
    }
    std::sort(finite.begin(), finite.end());
    double running = 0.0;
    std::size_t m = finite.size();
    for (std::size_t rank = 0; rank < finite.size(); ++rank) {
        double value = std::min(1.0, finite[rank].first * static_cast<double>(m - rank));
        running = std::max(running, value);
        adjusted[finite[rank].second] = running;
    }
    return adjusted;
}

void ApplyHolmAdjustedPValues(std::vector<LinearFunctionEstimate> &rows)
{
    std::vector<double> pValues;
    pValues.reserve(rows.size());
    for (const LinearFunctionEstimate &row : rows) {
        pValues.push_back(row.pValue);
    }
    std::vector<double> adjusted = HolmAdjustedPValues(pValues);
    for (std::size_t i = 0; i < rows.size() && i < adjusted.size(); ++i) {
        rows[i].adjustedPValue = adjusted[i];
    }
}

std::string GeneralizedGLMFitFailedStatus()
{
    return "Could not fit generalized linear model.";
}

std::string GLMNoActiveInteractionDataStatus()
{
    return "No active model data are available.";
}

std::string GLMInteractionModelNotFittedStatus()
{
    return "The model could not be fitted.";
}

std::string GLMChooseResponseVariableStatus()
{
    return "Choose a response variable.";
}

std::string GLMInvalidFamilyStatus()
{
    return "Invalid GLM family.";
}

std::string GLMInvalidLinkForFamilyStatus()
{
    return "Invalid link for family.";
}

std::string GLMNoRowsForScopeStatus()
{
    return "No rows are available for the selected scope.";
}

std::string GLMInteractionTwoWayOnlyStatus()
{
    return "Effect plots for interactions currently support two-way interactions.";
}

std::string GLMInteractionNoFittedValuesStatus()
{
    return "The effect plot could not be built because a component has no fitted values.";
}

std::string GLMInteractionFactorNoLevelsStatus()
{
    return "The effect plot could not be built because a categorical predictor has no fitted categories.";
}

std::string GLMInteractionTypeNotAvailableStatus()
{
    return "This effect plot type is not available.";
}

std::string GLMInteractionNoFiniteValuesStatus()
{
    return "The effect plot has no finite fitted values.";
}

std::string GLMChooseDependentVariableStatus()
{
    return "Choose a dependent variable.";
}

std::string GLMDependentVariableNotAvailableStatus()
{
    return "Dependent variable is not available.";
}

std::string GLMPredictorUnavailableStatus(const std::string &term)
{
    return "Predictor `" + term + "` is not available.";
}

std::string GLMFactorPredictorUnavailableStatus(const std::string &term)
{
    return "Categorical predictor `" + term + "` is not available.";
}

std::string GLMNumericPredictorUnavailableStatus(const std::string &term)
{
    return "Numeric predictor `" + term + "` is not available.";
}

std::string GLMNoDiagnosticRowsStatus()
{
    return "The current GLM has no diagnostic rows. Refit the model with at least one valid predictor.";
}

std::string GLMNoActiveDiagnosticStatus()
{
    return "No active GLM model/plot is available.";
}

std::string GLMCurrentLinearDiagnosticUnavailableStatus(const std::string &kind)
{
    return "Diagnostic `" + kind + "` is not available for the current linear model.";
}

std::string GLMSelectedLinearDiagnosticUnavailableStatus(const std::string &kind)
{
    return "Diagnostic `" + kind + "` is not available for the selected comparison model.";
}

std::string GLMDiagnosticStateSummaryText(const std::string &group,
                                          const std::string &dependent,
                                          const std::vector<std::string> &predictors,
                                          int validPredictors,
                                          int modelVersion,
                                          int fitVersion,
                                          bool isStale,
                                          bool hasDataSeed,
                                          std::size_t usedRows,
                                          std::size_t excludedRows)
{
    std::ostringstream out;
    out << "model_id=glm:" << group
        << "\ndependent=" << dependent
        << "\npredictors=";
    if (predictors.empty()) {
        out << "(none)";
    } else {
        for (std::size_t i = 0; i < predictors.size(); ++i) {
            if (i) {
                out << ", ";
            }
            out << predictors[i];
        }
    }
    out << "\nvalid_predictor_count=" << validPredictors
        << "\nmodel_version=" << modelVersion
        << "\nfit_version=" << fitVersion
        << "\nis_stale=" << (isStale ? "TRUE" : "FALSE")
        << "\nhas_data_seed=" << (hasDataSeed ? "TRUE" : "FALSE")
        << "\nn_used_rows=" << usedRows
        << "\nn_excluded_rows=" << excludedRows;
    return out.str();
}

std::string GLMCurrentDiagnosticFitFailedStatus(const std::string &summary)
{
    return "The current GLM could not be fitted for diagnostics.\n\n" + summary;
}

std::string GLMSelectedDiagnosticFitFailedStatus(const std::string &warning)
{
    return "The selected model could not be fitted.\n\n" + warning;
}

std::string GLMCurrentGeneralizedDiagnosticFitFailedStatus(const std::string &status)
{
    return "The current generalized linear model could not be fitted.\n\n" + status;
}

std::string GLMSelectedGeneralizedDiagnosticFitFailedStatus(const std::string &status)
{
    return "The selected generalized model could not be fitted.\n\n" + status;
}

std::string GLMReadyStatus()
{
    return "Status: ready";
}

std::string GLMNoInteractionTermStatus()
{
    return "No interaction term is selected.";
}

std::string GLMRegressionComparisonNotAvailableStatus()
{
    return "The General Linear Model comparison is not available.";
}

std::string GLMChooseValidModelColumnStatus()
{
    return "Choose a valid model column.";
}

std::string GLMGeneralizedLinearModelNotAvailableStatus()
{
    return "The generalized linear model is not available.";
}

std::string GLMGeneralizedModelComparisonNotAvailableStatus()
{
    return "The generalized model comparison is not available.";
}

std::string GLMNoNumericResponseVariablesStatus()
{
    return "The active dataset has no numeric response variables.";
}

std::string GLMImputedDatasetNotAvailableStatus()
{
    return "The active imputed dataset is no longer available.";
}

std::string GLMAddPredictorBeforeTableStatus()
{
    return "Add at least one predictor before opening the pooled MI Generalized Linear Model table.";
}

std::string GLMComparisonRequiresPooledTestStatus()
{
    return "Multiple-imputation generalized model comparisons require MI-aware pooled tests from the R analysis layer. Open a pooled generalized comparison from R so this native window receives pooled model-comparison results.";
}

std::string GLMNoInteractionReportTextStatus()
{
    return "No interaction report text was produced for this term.";
}

std::string GLMWindowTitle()
{
    return "Generalized Linear Model";
}

std::string GLMGeneralizedModelComparisonTitle()
{
    return "Generalized Model Comparison";
}

std::string GLMGeneralLinearModelTitle()
{
    return "Linear Model";
}

std::string GLMAutoRefitButtonTitle()
{
    return "Auto-refit";
}

std::string GLMDiagnosticResidualHistogramTitle()
{
    return "Residual histogram";
}

std::string GLMDiagnosticResidualsFittedTitle()
{
    return "Residuals vs fitted";
}

std::string GLMDiagnosticObservedFittedTitle()
{
    return "Observed vs fitted";
}

std::string GLMDiagnosticNormalQQTitle()
{
    return "Normal Q-Q";
}

std::string GLMDevianceResidualTitle()
{
    return "Deviance";
}

std::string GLMPearsonResidualTitle()
{
    return "Pearson";
}

std::string GLMWorkingResidualTitle()
{
    return "Working";
}

std::string GLMDunnSmythResidualTitle()
{
    return "Dunn\xE2\x80\x93Smyth";
}

std::string GLMRawResidualTitle()
{
    return "Raw";
}

std::string GLMAddTermTitle()
{
    return "Add independent variable";
}

std::string GLMChangeTermTitle()
{
    return "Change term";
}

std::string GLMResponseFieldLabel()
{
    return "Response:";
}

std::string GLMFamilyFieldLabel()
{
    return "Distribution:";
}

std::string GLMLinkFieldLabel()
{
    return "Link:";
}

std::string GLMScopeFieldLabel()
{
    return "Scope:";
}

std::string GLMResidualFieldLabel()
{
    return "Residual:";
}

std::string GLMResponseVariableIsFieldLabel()
{
    return "Response variable is:";
}

std::string GLMSourceTableHeader()
{
    return "Source";
}

std::string GLMRegressionDefaultModelName()
{
    return "Regression";
}

std::string GLMInteractionPlotWindowTitle()
{
    return "Effect Plot";
}

std::string GLMDiagnosticPlotWindowTitle()
{
    return "Diagnostic Plot";
}

std::string GLMRenameModelTitle()
{
    return "Rename model";
}

std::string GLMDuplicateModelTitle()
{
    return "Duplicate model";
}

std::string GLMDeleteModelTitle()
{
    return "Delete model";
}

std::string GLMReportInteractionTermPrefix()
{
    return "Interaction term:";
}

std::string GLMReportTypePrefix()
{
    return "Type:";
}

std::string GLMReportConfidenceLevelPrefix()
{
    return "Confidence level:";
}

std::string GLMReportMultipleComparisonsPrefix()
{
    return "Multiple comparisons:";
}

std::string GLMReportNotePrefix()
{
    return "Note:";
}

std::string GLMReportEffectHeader()
{
    return "Effect";
}

std::string GLMComparisonFamilyMenuTitle()
{
    return "Distribution";
}

std::string GLMComparisonLinkMenuTitle()
{
    return "Link";
}

std::string GLMComparisonFamilyMenuItemTitle()
{
    return "Distribution...";
}

std::string GLMComparisonLinkMenuItemTitle()
{
    return "Link...";
}

std::string GLMModelFitSectionTitle()
{
    return "Model fit";
}

std::string GLMTermsSectionTitle()
{
    return "Terms";
}

std::string GLMFitNLabel()
{
    return "N";
}

std::string GLMFitNullDevianceLabel()
{
    return "Null deviance";
}

std::string GLMFitResidualDevianceLabel()
{
    return "Residual dev.";
}

std::string GLMFitDfResidualLabel()
{
    return "df residual";
}

std::string GLMFitAICLabel()
{
    return "AIC";
}

std::string GLMFitBICLabel()
{
    return "BIC";
}

std::string GLMFitDispersionLabel()
{
    return "Dispersion";
}

std::string GLMFitLogLikLabel()
{
    return "logLik";
}

std::string GLMCoefVariableHeader()
{
    return "Variable";
}

std::string GLMCoefTypeHeader()
{
    return "Type";
}

std::string GLMCoefBHeader()
{
    return "b";
}

std::string GLMCoefSEHeader()
{
    return "SE";
}

std::string GLMCoefStatisticHeader()
{
    return "z";
}

std::string GLMCoefPHeader()
{
    return "p";
}

std::string GLMAnovaSumSquaresHeader()
{
    return "Sum of Squares";
}

std::string GLMAnovaDfHeader()
{
    return "df";
}

std::string GLMAnovaMeanSquareHeader()
{
    return "Mean Square";
}

std::string GLMAnovaFRatioHeader()
{
    return "F-ratio";
}

std::string GLMAnovaPHeader()
{
    return "p";
}

std::string GLMAnovaResidualLabel()
{
    return "Residual";
}

std::string GLMRowsUsedExcludedWindowTitle()
{
    return "GLM Rows Used/Excluded";
}

std::string GLMAddModelLabel()
{
    return "+ Add model";
}

std::string GLMAddTermLabel()
{
    return "+ Add term";
}

std::string RegressionComparisonAddTermLabel()
{
    return "+ Add term";
}

std::string GLMPlusSignLabel()
{
    return "+";
}

std::string GLMSelectedRowsPlaceholder()
{
    return "Selected rows: 0 / 0";
}

std::string GLMD1WaldLabel()
{
    return "D1/Wald";
}

std::string GLMAnovaFRatioLabel()
{
    return "F-ratio";
}

std::string GLMRegressionBetaLabel()
{
    return "\u03B2";
}

std::string GLMRegressionPartialRLabel()
{
    return "Partial r";
}

std::string GLMRegressionDeltaRSquaredLabel()
{
    return "\u0394R\u00B2";
}

std::string GLMInteractionsMenuTitle()
{
    return "Interaction";
}

std::string GLMGeneralLinearTHeader()
{
    return "t";
}

std::string GLMGeneralLinearTermsHeader()
{
    return "Terms";
}

std::string GLMGeneralLinearVariableHeader()
{
    return "Variable";
}

std::string GLMGeneralLinearTypeHeader()
{
    return "Type";
}

std::string GLMGeneralLinearBetaHeader()
{
    return "\u03B2";
}

std::string GLMInterceptTermName()
{
    return "(Intercept)";
}

std::string GLMDashPlaceholder()
{
    return "-";
}

std::string GLMFitGlobalFitSectionTitle()
{
    return "Global fit\n";
}

std::string GLMFitRSquaredLabel()
{
    return "R\u00B2";
}

std::string GLMFitAdjustedRSquaredLabel()
{
    return "Adjusted R\u00B2";
}

std::string GLMFitSValueLabel()
{
    return "s";
}

std::string GLMFitDFLabel()
{
    return "df";
}

ModelStatisticExplanationContext GeneralizedModelStatisticContext(
    const GeneralizedGLMState &state, const std::string &statistic, int row)
{
    ModelStatisticExplanationContext context;
    context.statistic = statistic;
    if (statistic == "estimate") context.statistic = "b";
    else if (statistic == "stderr") context.statistic = "SE";
    else if (statistic == "stat") context.statistic = state.statisticName.empty() ? "z" : state.statisticName;
    else if (statistic == "lr") context.statistic = "LR chi-square";
    else if (statistic == "lr_df") context.statistic = "LR df";
    else if (statistic == "lr_p") context.statistic = "p LR";
    else if (statistic == "effect_ci") context.statistic = GeneralizedModelHasExponentiatedEffect(state)
        ? GeneralizedExponentiatedEffectLabel(state) + " / 95% CI" : "95% CI";
    else if (statistic == "term") context.statistic = "Variable / category";
    else if (statistic == "type") context.statistic = "Type";
    if (row >= 0 && static_cast<std::size_t>(row) < state.rows.size()) {
        const auto &coefficient = state.rows[row];
        if (GeneralizedGlobalTermTestForPresentationRow(state, coefficient)) {
            const GlobalTermTestRow *globalTest =
                GeneralizedGlobalTermTestForPresentationRow(state, coefficient);
            if (statistic == "stat" || statistic == state.statisticName)
                context.statistic = globalTest->method == "Rubin Wald chi-square"
                    ? "Pooled Wald χ²" : "Global term test";
            if (context.statistic == "p") context.statistic = "Global term p";
            context.detail = GeneralizedGlobalTermTestMethodNote(state);
        } else if (coefficient.rowType == "reference") {
            context.detail = "This reference category has a coefficient fixed to zero for coding; dashes are not failed estimates or tests.";
        } else if (coefficient.rowType == "intercept" || coefficient.term == "(Intercept)") {
            context.detail = "The intercept is the linear predictor when numeric predictors are zero and categorical predictors are at their reference categories.";
        } else if (coefficient.rowType == "section_header") {
            context.statistic = "Model component";
        }
    }
    context.analysisKind = state.countRegression ? "count model" : "generalized model";
    context.modelFamily = state.family;
    context.link = state.link;
    context.multipleImputation = state.multipleImputation;
    context.rubinRulesApplied = state.multipleImputation &&
        state.note.find("Rubin's rules were not") == std::string::npos;
    context.miMethod = context.rubinRulesApplied ? "mice::pool" : "";
    context.likelihoodAvailable = state.likelihoodAvailable;
    const auto diagnostic = [&](const std::string &key) {
        const auto found = state.familyDiagnostics.find(key);
        return found == state.familyDiagnostics.end() ? NAN : found->second;
    };
    if (statistic == "Intra-trial correlation (rho)" &&
        std::isfinite(diagnostic("beta_binomial_rho"))) {
        context.detail = "For this model rho = " +
            FormatDoubleOrDash(diagnostic("beta_binomial_rho"), 3) + ".";
    } else if (statistic == "Variance inflation vs binomial" &&
               std::isfinite(diagnostic("beta_binomial_variance_inflation"))) {
        context.detail = "Here the variance ratio is " +
            FormatDoubleOrDash(diagnostic("beta_binomial_variance_inflation"), 2) +
            " at the fitted observation trial counts.";
    } else if (statistic == "Variance inflation vs Poisson" &&
               std::isfinite(diagnostic("negative_binomial_variance_inflation"))) {
        context.detail = "Here the ratio is " +
            FormatDoubleOrDash(diagnostic("negative_binomial_variance_inflation"), 2) +
            (state.multipleImputation ?
                " as a mean of imputation-specific ratios evaluated at each fit's mean count; the mean fitted count is " :
                " at mean fitted count ") +
            FormatDoubleOrDash(diagnostic("negative_binomial_mean_fitted_count"), 3) + ".";
    } else if (statistic == "Implied coefficient of variation" &&
               std::isfinite(diagnostic("gamma_cv"))) {
        context.detail = "For this fitted Gamma model the conditional CV is " +
            FormatDoubleOrDash(diagnostic("gamma_cv"), 3) + ".";
    }
    return context;
}

std::string ExplainModelStatistic(
    const ModelStatisticExplanationContext &context)
{
    const std::string original = TrimCopy(context.statistic);
    const std::string key = LowerCopy(original);
    const std::string analysis = LowerCopy(TrimCopy(context.analysisKind));
    const std::string method = LowerCopy(TrimCopy(context.method));
    const auto contains = [&key](const std::string &text) {
        return key.find(text) != std::string::npos;
    };
    const auto analysisContains = [&analysis](const std::string &text) {
        return analysis.find(text) != std::string::npos;
    };
    const auto methodContains = [&method](const std::string &text) {
        return method.find(text) != std::string::npos;
    };

    if (analysis == "interaction interpretation") {
        const bool marginalMean = methodContains("marginal mean") ||
            methodContains("predicted mean") || methodContains("prediction");
        const bool simpleSlope = methodContains("simple slope");
        const bool interactionContrast = methodContains("interaction contrast");
        const bool transformed = methodContains("exponentiated") ||
            contains("odds ratio") || contains("ratio");
        std::string explanation;
        if (key == "estimate" || key == "emmean" || key == "response" ||
            key == "probability" || key == "rate" || transformed) {
            if (interactionContrast && transformed)
                explanation = "This is the exponentiated interaction contrast, a ratio of two contrasts on the stated link scale. It is not an ordinary single-predictor odds or rate ratio.";
            else if (interactionContrast)
                explanation = "This estimate is a difference between two simple contrasts. Its sign follows the contrast orientation stated in the report, on the stated response or link scale.";
            else if (marginalMean)
                explanation = "This is an estimated marginal mean or prediction from the fitted model for the displayed factor combination. It is not the raw group average.";
            else if (simpleSlope)
                explanation = "This is the estimated slope of one predictor at the displayed value of the interacting predictor, on the scale stated in the report.";
            else
                explanation = "This is the estimated comparison in the row's stated order and scale. Reversing the order reverses a difference or reciprocates a ratio.";
        } else if (key == "se" || contains("standard error")) {
            explanation = "SE quantifies sampling uncertainty of this section's estimate or contrast on its inferential scale. A dash means no SE is reported for this transformed quantity.";
        } else if (key == "df") {
            explanation = "df is the reference degrees of freedom for the displayed test or confidence interval; pooled multiple-imputation df need not be an integer.";
        } else if (key == "statistic" || key == "t" || key == "z" || key == "t / z") {
            explanation = "The test statistic compares the displayed estimand with its null value using its standard error. Its reference distribution and scale are those reported for this section.";
        } else if (key == "p" || contains("adjusted p")) {
            explanation = contains("adjusted")
                ? "This p value is adjusted for the family of comparisons stated in the report. It tests the corresponding contrast, not the interaction term as a whole."
                : "This p value tests the null value for this row's estimate or contrast under the fitted model. It does not measure effect size or test the interaction term as a whole.";
        } else if (contains("lower") || contains("upper") || contains("ci")) {
            explanation = "This is a confidence limit for the estimate or contrast in this section, on the scale named by its column and report notes. It is not a range of individual observations.";
        }
        if (!explanation.empty()) {
            if (!context.detail.empty()) explanation += "\n\n" + context.detail;
            if (context.multipleImputation)
                explanation += "\n\nMultiple-imputation results use " +
                    (context.miMethod.empty() ? std::string("the pooling method stated in the report") : context.miMethod) +
                    "; they do not show one selected imputation.";
            if ((key == "p" || contains("adjusted p")) && !context.adjustment.empty())
                explanation += "\n\nMultiple-comparison method: " + context.adjustment + ".";
            return explanation;
        }
    }

    if (analysis == "scale_scores") {
        std::string text = "Scale scores use the selected sum or mean of the included items after explicit reverse scoring. Rows below the minimum-valid-item requirement receive NA. ";
        if (key == "valid n") text += "Valid N counts cases with a finite score (per imputation for MI).";
        else if (key == "mean") text += "Mean is the arithmetic average of the valid scores.";
        else if (key == "sd") text += "SD is the sample standard deviation of the valid scores.";
        else if (key == "minimum" || key == "maximum") text += "Minimum and maximum are the smallest and largest valid observed scores, not the theoretical endpoints.";
        if (context.multipleImputation) text += " The descriptive score summary uses scores stacked across imputations; it is not a Rubin-pooled estimate. Valid N is reported per imputation, not as m times N. Use a single-imputation histogram to select original cases.";
        return text;
    }
    if (analysis == "scale") {
        std::string text;
        if (key == "items") text = "Number of items currently included in the scale. Longer scales can have higher alpha even without stronger item correlations.";
        else if (key == "n used") text = "Number of cases with complete values on all selected items within the analysis scope. Individual item statistics and pairwise correlations may use more cases.";
        else if (contains("missing")) text = "Percentage of missing item cells, not percentage of participants. In the summary this uses all selected items; an item row refers to that item only. Original missingness is measured before imputation.";
        else if (key == "scale mean" || key == "scale sd") text = "Mean or standard deviation of the average item score reported by psych::alpha. This descriptive summary uses available items and is independent of the sum/mean score-saving option and its minimum-valid-item rule. Interpret it in the item response units.";
        else if (key == "mean") text = "Mean of the scored item among its observed values in scope, after any selected reverse scoring. Interpret it in the item's units.";
        else if (key == "sd") text = "Standard deviation of the scored item among its observed values in scope. Larger values indicate greater response variation; zero indicates a constant item.";
        else if (key == "mean inter-item r") text = "Average correlation between pairs of scored items, from psych::alpha. Negative values call for checking item direction or content. Very high correlations can indicate redundant items. No single cutoff establishes a good scale.";
        else if (key == "alpha" || key == "standardized alpha") text = key == "alpha"
            ? "Cronbach's alpha from psych::alpha, based on item covariances. It summarizes internal consistency, not unidimensionality or validity. Interpret it with item content, item count and dimensionality. A negative value warrants checking scoring directions and item relationships."
            : "Cronbach's alpha calculated from the item correlation matrix, treating item variances equally. It can differ from raw alpha when items have different variances. It does not establish unidimensionality or validity.";
        else if (key == "omega total") text = "Omega total from psych::omega estimates the share of score variance associated with common factors under its fitted factor model. It is not omega hierarchical and does not establish one-dimensionality. A dash means it was not calculated or was unavailable; open Reliability to request it.";
        else if (key == "item-rest r") text = "Correlation of this scored item with the sum of the remaining items, from psych::alpha (r.drop). Excluding the item avoids inflating the correlation through overlap. Negative values warrant checking direction and content; low values alone are not a reason to delete an item.";
        else if (key == "alpha if deleted") text = "Cronbach's alpha recalculated after removing this item. Compare with the full-scale alpha, while considering content coverage. A higher value does not by itself justify deleting the item.";
        else text = "This quantity describes the current scale and its selected item coding.";
        if (context.multipleImputation) text += " Under multiple imputation, reliability and scale summaries are descriptive summaries across completed datasets, not Rubin-pooled estimates; ranges show between-imputation variation.";
        return text + "\n\nReference: psych package documentation (https://www.personality-project.org/r/psych/help/alpha.html; https://www.personality-project.org/r/psych/help/omega.html).";
    }

    std::string explanation;
    if (key == "intra-trial correlation (rho)") {
        explanation = "The glmmTMB beta-binomial precision phi implies correlation rho = 1 / (1 + phi) among Bernoulli trials within an observation. "
            "Larger rho means more shared variation; rho approaching zero means ordinary binomial-level dependence. "
            "It is not a correlation between predictors.";
    } else if (key == "variance inflation vs binomial") {
        explanation = "For glmmTMB's beta-binomial model, conditional variance divided by ordinary binomial variance at the same mean is "
            "1 + (n - 1) / (1 + phi), with the actual trial count n for each observation. "
            "When trial counts vary, the displayed value averages those observation-specific ratios. "
            "A ratio of 1 means binomial-level variance; larger values indicate extra-binomial variation. It is not a multicollinearity VIF or a model-selection rule.";
    } else if (key == "variance inflation vs poisson") {
        explanation = "MASS::glm.nb uses NB2 variance Var(Y | x) = mu + mu^2 / theta. "
            "Relative to Poisson variance at the same fitted mean, the ratio is 1 + mu / theta. "
            "The displayed value uses the mean fitted count, not a universal constant. A ratio of 1 means Poisson-level variance at that mean; larger values indicate extra-Poisson variation.";
    } else if (key == "implied coefficient of variation") {
        explanation = "For the stats::Gamma GLM, Var(Y | x) = phi * mu^2, with phi the estimated dispersion. "
            "The conditional coefficient of variation is sqrt(phi), a dimensionless SD-to-mean ratio. "
            "Larger values mean more relative conditional spread; this diagnostic alone does not establish model adequacy.";
    } else if (key == "gamma dispersion (phi)") {
        explanation = "For stats::Gamma, the conditional variance is phi * mu^2. Phi is the fitted scale or dispersion estimated from R's GLM summary. "
            "Larger phi means more conditional spread relative to the squared mean; sqrt(phi) is the dimensionless conditional coefficient of variation. "
            "This is distinct from a Pearson dispersion ratio for Poisson or grouped binomial data.";
    } else if (key == "theta" || contains("shape (theta)")) {
        explanation = "Theta controls extra variation in a negative-binomial model: Var(Y | x) = mu + mu^2 / theta, where mu is the fitted mean. "
            "Smaller positive theta means more overdispersion; large theta approaches Poisson variation. This is not a coefficient or a rate ratio. "
            "There is no universal cutoff: the extra variance also depends on mu.\n\n"
            "Reference: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/NegBinomial.html";
    } else if (contains("precision (phi)")) {
        explanation = "Phi is the precision of the beta distribution underlying the beta-binomial model. For n trials and success probability p, "
            "the variance is n*p*(1-p)*(phi+n)/(phi+1). Larger phi approaches binomial variation; smaller positive phi means more overdispersion. "
            "This is not a coefficient or an odds ratio.\n\nReference: https://glmmtmb.github.io/glmmTMB/reference/nbinom2.html";
    } else if (contains("dispersion (sigma)")) {
        explanation = "Sigma is the dispersion of the below-ceiling beta-binomial component in the GAMLSS parameterization. "
            "Larger sigma means more overdispersion. It is not a residual standard deviation or glmmTMB's precision phi. "
            "The component is truncated, so its conditional mean and variance also reflect truncation.";
    } else if (contains("dispersion (fixed)")) {
        explanation = "The dispersion scale is fixed at 1 rather than estimated. Poisson assumes conditional variance equal to its mean; "
            "binomial assumes n*p*(1-p). The value 1 does not establish that these assumptions fit the data.";
    } else if (contains("pearson dispersion") || contains("dispersion (phi)")) {
        explanation = "Pearson dispersion is the sum of squared Pearson residuals divided by residual degrees of freedom, when those degrees of freedom are positive. "
            "Poisson assumes conditional variance equal to the mean; ordinary grouped binomial uses n*p*(1-p). "
            "A ratio near 1 is consistent with the baseline variance, above 1 suggests extra variation, and below 1 suggests underdispersion. "
            "It is more informative for grouped binomial than for Bernoulli data and is not a formal model-selection rule. "
            "The diagnostic ratio does not automatically rescale ordinary-model standard errors; quasi-family dispersion does affect uncertainty.";
    } else if (key == "trials") {
        explanation = "Trials is the known number of opportunities per observation, or the variable supplying those numbers. "
            "The response must lie between zero and trials. This is not the number of rows or an estimated parameter.";
    } else if (contains("observed at ceiling")) {
        explanation = "Observed at ceiling counts cases whose response equals their number of trials; the percentage uses fitted cases as denominator. "
            "If imputation changes this count, the table reports its descriptive mean across imputations.";
    } else if (contains("predicted at ceiling")) {
        explanation = "Predicted at ceiling is the sum of fitted probabilities of a perfect score; the percentage is their mean probability. "
            "Fractional counts are expected. Compare with observed at ceiling to assess this aspect of fit. Under MI these are descriptive means across fits.";
    } else if (contains("logistic component") || contains("truncated beta-binomial") || key == "model component") {
        explanation = "The ceiling-hurdle model combines a logistic model for reaching the perfect score and a truncated beta-binomial model "
            "for scores below it. Each coefficient section refers to its own component, not an overall-response effect.";
    } else if (key == "likelihood statistics") {
        explanation = "Log likelihood, AIC and BIC require an ordinary likelihood. Quasi-Poisson does not define one. "
            "LinkEDA also leaves these statistics unavailable for MI when no valid pooled definition is implemented.";
    } else if (key == "type" || key == "term type") {
        explanation = "Type shows model coding: numeric uses a continuous effect; categorical uses contrasts against a reference category. "
            "An interaction allows one predictor's effect to depend on another. This is not a fitted statistic.";
    } else if (key == "term" || key == "variable / category" || key == "variable") {
        explanation = "Variable identifies the predictor or interaction. Indented category rows compare with the displayed reference category. "
            "A parent row tests the whole term; category rows test individual contrasts. With interactions, lower-order effects refer to the reference values of the other interacting predictors.";
    } else if (key == "pooled wald χ²") {
        explanation = "This pooled Wald chi-square jointly tests whether the term's estimable coefficients are zero, conditional on other model terms. "
            "The statistic uses the pooled coefficient vector and its full within- and between-imputation covariance; its reference degrees of freedom equal the number of estimable coefficients. "
            "It is not a single-coefficient t statistic. The p value tests the whole term and does not measure effect size.";
    } else if (key == "global term test" || key == "global term p") {
        explanation = "This jointly tests whether all estimable coefficients of the term are zero, conditional on the other terms. "
            "Use the stated method for its reference distribution: this is not a single-coefficient t or z test. "
            "For a categorical predictor it tests the whole term. The p value does not measure effect size.";
    } else if (key == "converged") {
        explanation = "Converged means the fitting algorithm met its numerical stopping criteria. It does not establish good fit; "
            "inspect warnings, extreme estimates and residual diagnostics too.";
    } else if (key == "fmi" || key == "lambda") {
        explanation =
            "FMI and lambda describe the share of uncertainty associated with missing information for this particular estimate. "
            "FMI includes a finite-degrees-of-freedom adjustment; lambda is the between-imputation contribution to total variance. "
            "They are not the percentage of missing cells.\n\n"
            "Practical interpretation: values closer to 0 indicate less uncertainty attributable to missing information; "
            "values closer to 1 indicate more. Around 0.5 means roughly half the information is missing for this estimate. "
            "There are no universal low/medium/high cutoffs. Focus on the largest values among the estimates you care about, "
            "and check MCSE / SE and the imputation diagnostics. More imputations reduce simulation error, but do not recover the missing information.\n\n"
            "Reference: van Buuren, Flexible Imputation of Missing Data, section 2.3. https://stefvanbuuren.name/fimd/sec-whyandwhen.html";
    } else if (key == "mcse (estimate)" || key == "mcse / se (%)") {
        explanation =
            "MCSE (estimate) approximates how much the pooled estimate would fluctuate if the imputation procedure were repeated "
            "on the same data: sqrt(B / m). It assumes independent, converged imputations. MCSE / SE (%) expresses this error "
            "relative to the pooled standard error.\n\n"
            "Rule of thumb: aim for MCSE / SE below 10% for the estimates that matter to your analysis "
            "(White, Royston and Wood, 2011). Above 10%, consider more imputations and recheck. This is a practical target, "
            "not a pass/fail test of imputation quality.\n\n"
            "For example, 25% means simulation error is about one quarter of the standard error. "
            "Roughly four times as many imputations halves MCSE if between-imputation variance stays similar. "
            "With few imputations, MCSE itself is uncertain. This diagnostic does not assess the stability of standard errors, "
            "confidence limits or p-values.\n\n"
            "Reference: https://doi.org/10.1002/sim.4067; practical guidance also in https://www.stata.com/manuals/mimiestimate.pdf";
    } else if (key == "riv") {
        explanation =
            "RIV is the relative increase in variance due to missing information, compared with within-imputation variance: "
            "(1 + 1/m) B / Ubar.\n\n"
            "Practical interpretation: 0.1 means 10% more variance; 1 means the variance has doubled. "
            "These percentages refer to variance, not standard error. Larger values indicate more uncertainty due to missing information, "
            "not necessarily a faulty imputation. There is no universal acceptable cutoff; inspect FMI and MCSE / SE alongside it.";
    } else if (key == "relative efficiency") {
        explanation =
            "Relative efficiency approximates the efficiency of using m imputations rather than infinitely many: 1 / (1 + FMI / m).\n\n"
            "Practical interpretation: closer to 1 means less efficiency lost from using a finite number of imputations; "
            "0.99 is about 99% efficiency. A value near 1 alone is not a sufficient stopping rule: "
            "also check MCSE / SE, convergence and the stability of conclusions. It does not establish that the imputation model is appropriate.";
    } else if (analysis == "mi_missing_information" && (key == "ubar" || key == "b" || key == "t")) {
        explanation =
            "Ubar is the average within-imputation variance; B is the variance of estimates between imputations; "
            "T = Ubar + (1 + 1/m) B is total variance. These are variances on the pooling scale, not regression coefficients or test statistics.\n\n"
            "Practical interpretation: compare the between-imputation contribution with Ubar through RIV or FMI. "
            "Raw variance values have no universal cutoff because they depend on units and scale. "
            "B = 0 does not prove good imputations or convergence, especially with small m.";
    } else if (analysis == "mi_missing_information" && key == "df") {
        explanation =
            "df is the degrees of freedom used for the pooled estimate's t reference distribution. "
            "It reflects missing information, the number of imputations and the applicable complete-data adjustment; "
            "it need not be an integer or equal N minus the number of coefficients. "
            "Smaller df imply heavier tails and wider confidence intervals for the same estimate and SE. "
            "There is no universal acceptable cutoff.";
    } else if (analysis == "mi_missing_information" && key == "m") {
        explanation =
            "m is the number of imputations used to pool this estimate.\n\n"
            "Practical guidance: do not assume that five imputations are enough just because relative efficiency is high. "
            "Check MCSE / SE for the estimates of interest; below 10% is a useful target for simulation error of point estimates. "
            "Increasing m reduces simulation error, but does not correct a poor imputation model. "
            "Inspect chain diagnostics and, when conclusions are borderline, assess their stability across additional imputations.";
    } else if (key.empty() || key == "model fit") {
        explanation =
            "Model fit summarizes how closely the fitted mean follows the observed "
            "response. No single number is sufficient: inspect the effect estimates, "
            "their uncertainty, the global test, residual diagnostics and the model's "
            "assumptions together.";
    } else if (key == "n" || contains("number of observations")) {
        explanation = analysis.empty() || analysisContains("regression") || analysisContains("model")
            ? "N is the number of original observations used by this fitted model after "
              "applying the analysis scope and exclusions. With multiple imputation it is "
              "the number of cases, not N multiplied by the number of imputations."
            : "N is the number of original observations contributing to this analysis after "
              "applying its scope and missing-data rule. With multiple imputation it is the "
              "number of cases, not N multiplied by the number of imputations.";
    } else if (contains("observed count and percent") ||
               contains("count and percent")) {
        explanation =
            "This cell reports a category or cell count together with its percentage. "
            "The count is the observed frequency; the percentage divides that frequency "
            "by the denominator identified by the row, column or table heading.";
    } else if (key == "count" || key == "frequency" || key == "observed" ||
               contains("observed count")) {
        explanation =
            "Count is the number of observations in this category or cell after applying "
            "the current analysis scope. It is a frequency, not a percentage or an estimate "
            "of an effect.";
    } else if (key == "missing" || contains("missing n") || contains("missing count")) {
        explanation =
            "Missing is the number of observations for which this variable has no usable "
            "value. In a multiple-imputation dataset, distinguish original missingness from "
            "the completed values supplied separately in each imputation.";
    } else if (key == "valid n" || key == "valid" || contains("non-missing")) {
        explanation =
            "Valid N is the number of observations with a usable value for the statistic "
            "after applying the current analysis scope.";
    } else if (key == "percent" || key == "%" || contains("total %")) {
        explanation =
            "Percent is this cell's count divided by the relevant total count. Check the "
            "table heading to determine whether the denominator is the whole table, a row "
            "or a column.";
    } else if (contains("valid %") || contains("valid percent")) {
        explanation =
            "Valid percent divides the category count by the number of non-missing cases, "
            "so missing observations are excluded from its denominator.";
    } else if (contains("cumulative") && !contains("variance") &&
               !analysisContains("dimensional")) {
        explanation =
            "Cumulative percent is the running sum of valid percentages through the displayed "
            "category order. It is meaningful only when that order has a substantive interpretation.";
    } else if (key == "mean" || key == "m" || key == "m₁" || key == "m₂" ||
               contains("arithmetic mean") ||
               contains("mean and sd")) {
        explanation = contains("mean and sd")
            ? "This cell reports the arithmetic mean together with the standard deviation. The mean "
              "describes location and the SD describes individual spread in the variable's original units."
            : "The mean is the arithmetic average of the usable values. It is sensitive to "
              "extreme observations, so interpret it together with dispersion and the distribution's shape.";
    } else if (key == "median" || key == "mdn" || key == "mdn₁" || key == "mdn₂" ||
               contains("median and iqr")) {
        explanation = contains("median and iqr")
            ? "This cell reports the median (50th percentile) together with the interquartile range, "
              "which describes the spread of the middle half of the usable observations."
            : "The median is the middle ordered value (the 50th percentile). It is less sensitive "
              "to extreme observations than the mean but does not describe the distribution's spread.";
    } else if (key == "mode") {
        explanation =
            "The mode is the most frequent observed value or category. More than one mode can "
            "exist when several values share the highest frequency.";
    } else if (key == "sd" || key == "sd₁" || key == "sd₂" ||
               contains("sdΔ") || contains("standard deviation")) {
        explanation =
            "The standard deviation describes the typical spread of observations around their "
            "mean in the variable's original units. It is not the standard error of the mean.";
    } else if (key == "variance") {
        explanation = analysisContains("dimensional")
            ? "Variance is the proportion of total variance represented by this component or factor. "
              "Together with interpretability, it helps determine how many dimensions to retain."
            : "Variance is the average squared spread around the mean, with the sample degrees-of-freedom "
              "correction where applicable. Its units are the square of the variable's units.";
    }
    if (explanation.empty()) {
    if (contains("se mean") || contains("standard error of the mean")) {
        explanation =
            "The standard error of the mean estimates the sampling uncertainty of the sample mean. "
            "It generally decreases as the usable sample size increases and is not a measure of individual spread.";
    } else if (key == "minimum" || key == "min") {
        explanation = "Minimum is the smallest usable observed value in the current analysis scope.";
    } else if (key == "maximum" || key == "max") {
        explanation = "Maximum is the largest usable observed value in the current analysis scope.";
    } else if (key == "range") {
        explanation =
            "Range is maximum minus minimum. It uses only the two most extreme observations and is "
            "therefore sensitive to outliers.";
    } else if (key == "q1" || contains("first quartile") || contains("25th")) {
        explanation = "Q1 is the 25th percentile: one quarter of usable observations lie at or below it.";
    } else if (key == "q3" || contains("third quartile") || contains("75th")) {
        explanation = "Q3 is the 75th percentile: three quarters of usable observations lie at or below it.";
    } else if (key == "iqr" || key.rfind("iqr", 0) == 0 ||
               contains("interquartile")) {
        explanation =
            "The interquartile range is Q3 minus Q1 and describes the spread of the middle half of "
            "the observations. It is comparatively resistant to extreme values.";
    } else if (contains("skew")) {
        explanation =
            "Skewness measures asymmetry. Positive values indicate a longer right tail and negative "
            "values a longer left tail; interpretation depends on the estimator and sample size.";
    } else if (contains("kurt")) {
        explanation =
            "Kurtosis describes tail weight and peak shape relative to the convention used by the "
            "analysis. Check whether the table reports excess kurtosis, for which a normal distribution is zero.";
    } else if (contains("expected count") || key == "expected") {
        explanation =
            "Expected count is the cell frequency predicted under the contingency table's null model, "
            "usually independence of the row and column variables. Small expected counts can invalidate "
            "a large-sample Pearson chi-square approximation.";
    } else if (contains("pearson residual") || contains("standardized residual") ||
               key == "residual") {
        explanation =
            "This contingency-table residual compares the observed cell count with its null-model expected "
            "count on a standardized scale. Its sign shows the direction and its magnitude highlights cells "
            "contributing strongly to the overall association.";
    } else if (contains("cram") || contains("cramér")) {
        explanation =
            "Cramér's V is a chi-square-based measure of association for categorical variables, scaled "
            "from zero to one. Its practical interpretation depends on table dimensions and context.";
    } else if (key == "phi" || key == "φ") {
        explanation =
            "Phi is a chi-square-based association measure most directly interpreted for a 2 × 2 table. "
            "For larger tables Cramér's V is usually the more suitable bounded summary.";
    } else if (contains("fisher")) {
        explanation =
            "Fisher's exact test evaluates categorical association using the exact conditional distribution, "
            "avoiding the large-sample expected-count approximation of Pearson's chi-square test.";
    } else if (key == "r" || contains("pearson r")) {
        explanation =
            "Pearson's r measures the strength and direction of a linear association, from -1 to 1. "
            "It is sensitive to outliers and does not establish causality.";
    } else if (contains("spearman") || key == "rho" || key == "ρ") {
        explanation =
            "Spearman's rho is a rank correlation measuring monotonic association. It can capture a "
            "consistently increasing or decreasing relationship that is not linear.";
    } else if (contains("kendall") || key == "tau" || key == "τ") {
        explanation =
            "Kendall's tau is a rank association based on concordant and discordant pairs. Its magnitude "
            "has a probability-of-ordering interpretation and its sign gives the direction.";
    } else if (contains("covariance")) {
        explanation =
            "Covariance measures joint variation in the product of the two variables' units. Its sign gives "
            "the direction, but its magnitude is scale-dependent; correlation provides a standardized version.";
    } else if (contains("mean difference") || contains("difference in means") ||
               contains("probability difference") || contains("Δm") ||
               contains("Δhl") || contains("Δp")) {
        explanation =
            contains("probability") || contains("Δp")
            ? "Probability difference is the estimated probability for the first displayed group or condition "
              "minus that for the second. Its sign follows the displayed comparison order and its scale runs "
              "from -1 to 1."
            : contains("Δhl")
            ? "The Hodges–Lehmann location shift estimates the typical paired or between-group difference on "
              "the response scale. Its sign follows the displayed subtraction order and it accompanies the "
              "corresponding rank-based analysis."
            : "Mean difference is the estimated first mean minus the second mean (or minus the stated test "
              "value). Its sign follows the displayed comparison order and its units are the response's units.";
    } else if (key == "u" || contains("mann-whitney")) {
        explanation =
            "Mann–Whitney U is based on the relative ranks of two independent groups. The accompanying "
            "p value tests equality of their rank distributions under the stated assumptions.";
    } else if (key == "w" || (key == "v" && methodContains("wilcoxon")) ||
               contains("wilcoxon")) {
        explanation =
            "The Wilcoxon statistic is computed from ranks. Its exact meaning depends on whether the test is "
            "one-sample/paired signed-rank or independent-sample rank-sum, as named in the analysis.";
    } else if ((key == "h" && methodContains("kruskal")) || contains("kruskal")) {
        explanation =
            "Kruskal–Wallis H compares rank distributions across independent groups. A significant result "
            "is an omnibus difference and does not by itself identify which groups differ.";
    } else if (contains("cohen") && contains("d")) {
        explanation =
            "Cohen's d expresses a mean difference in standard-deviation units. Its sign follows the displayed "
            "comparison order; practical importance should be judged in the study context.";
    } else if (contains("hedges") || contains("hedge")) {
        explanation =
            "Hedges' g is a standardized mean difference with a small-sample bias correction. Its sign follows "
            "the displayed group or contrast order.";
    } else if (contains("rank-biserial") || contains("rank biserial")) {
        explanation =
            "Rank-biserial correlation is an effect size for a rank comparison, scaled from -1 to 1. Its sign "
            "follows the displayed comparison order.";
    } else if (key == "r_rb" || key == "r rb") {
        explanation =
            "Rank-biserial correlation is the effect size paired with this rank comparison. It ranges from "
            "-1 to 1, and its sign follows the displayed group, variable or test-value comparison order.";
    } else if (key == "d" || key == "d_z" || key == "dz") {
        explanation = key == "d_z" || key == "dz"
            ? "Paired-samples dₛ standardizes the mean of the paired differences by their standard deviation. "
              "Its sign follows the first-variable-minus-second-variable order."
            : "Cohen's d expresses the displayed mean difference in standard-deviation units. Its sign follows "
              "the displayed comparison order; practical importance depends on the study context.";
    } else if (key == "g") {
        explanation =
            "Hedges' g is a standardized mean difference with a small-sample bias correction. Its sign follows "
            "the displayed group order.";
    } else if (key == "h") {
        explanation =
            "Cohen's h is an effect size for a difference between two proportions after an arcsine-square-root "
            "transformation. Its sign follows the displayed group order.";
    } else if (key == "ε²" || contains("epsilon")) {
        explanation =
            "Epsilon-squared estimates the proportion of rank variation associated with the grouping factor in "
            "a Kruskal–Wallis analysis. It is an omnibus effect size, not a pairwise difference.";
    } else if (contains("eta") || contains("η")) {
        explanation =
            "Eta-squared is the proportion of variation attributed to the effect under the table's sum-of-squares "
            "definition. Partial eta-squared uses effect plus error as its denominator and is not interchangeable "
            "with ordinary eta-squared.";
    } else if (contains("omega") || contains("ω")) {
        explanation =
            "Omega-squared is an effect-size estimate of the proportion of population variation attributable "
            "to the factor, with a correction intended to reduce eta-squared's upward sample bias.";
    } else if (contains("eigenvalue")) {
        explanation = contains("parallel")
            ? "The parallel-analysis eigenvalue is a reference value obtained from random data with comparable "
              "dimensions. Retaining an observed component is supported when its observed eigenvalue exceeds "
              "this reference value."
            : "An eigenvalue is the amount of total variance represented by this component or factor before any "
              "rotation. Larger values indicate dimensions accounting for more variation.";
    } else if (contains("cumulative variance") || contains("cumulative %") ||
               (key == "cumulative" && analysisContains("dimensional"))) {
        explanation =
            "Cumulative variance is the running proportion of total variance represented by this dimension and "
            "all preceding retained dimensions.";
    } else if (contains("proportion of variance") || contains("variance %") ||
               contains("% variance")) {
        explanation =
            "Proportion of variance is the share of total variance represented by this component or factor. "
            "Together with interpretability, it helps determine how many dimensions to retain.";
    } else if (contains("loading")) {
        explanation =
            "A loading describes how strongly and in which direction a variable relates to the component or "
            "factor. Its exact interpretation depends on the extraction and rotation shown by the analysis.";
    } else if (contains("communality")) {
        explanation =
            "Communality is the proportion of a variable's variance represented by the retained common factors "
            "or components under the selected dimensionality model.";
    } else if (contains("uniqueness")) {
        explanation =
            "Uniqueness is the share of a variable's variance not represented by the retained common factors. "
            "Under the usual standardized solution it is one minus communality.";
    } else if (key == "events" || key == "event count") {
        explanation =
            "Events is the number of usable observations in the response category selected "
            "as the event. In binary regression the model describes that category relative "
            "to the displayed reference category.";
    } else if (key == "event") {
        explanation =
            "Event identifies which category is counted as a success in this binary or proportion analysis. "
            "Changing it reverses the outcome coding and can change the sign or reciprocal parameterisation.";
    } else if (key == "x" && (analysisContains("sample") || methodContains("binomial"))) {
        explanation =
            "x is the observed number of events (successes) among the N usable cases for this binomial analysis.";
    } else if (key == "p̂" || key == "p̂₁" || key == "p̂₂") {
        explanation =
            "p-hat is the observed sample proportion in the displayed group or variable: the event count divided "
            "by its usable N.";
    } else if (key == "p₀") {
        explanation =
            "p₀ is the reference proportion specified by the null hypothesis for this one-sample binomial test.";
    } else if (key == "μ₀" || key == "θ₀") {
        explanation = key == "μ₀"
            ? "μ₀ is the reference mean specified by the null hypothesis for this one-sample t test."
            : "θ₀ is the reference location specified by the null hypothesis for this rank-based test.";
    } else if (key == "parameters" || key == "parameter count") {
        explanation =
            "Parameters is the rank of the fitted coefficient vector, including the intercept "
            "and all independently estimable categorical contrasts and interaction columns. A "
            "categorical predictor with k represented categories normally contributes k - 1 parameters.";
    }
    }
    if (explanation.empty()) {
    if ((contains("Δ adjusted r") || contains("delta adjusted r"))) {
        explanation =
            "Δ adjusted R² is the change in adjusted explained variance from the "
            "previous model. Because adjusted R² penalizes extra parameters, this change "
            "can be negative even when ordinary R² increases.";
    } else if (contains("adjusted r") || contains("adjusted r²")) {
        explanation =
            "Adjusted R² is the proportion of response variance explained by the "
            "linear model after penalizing the number of fitted parameters. It is most "
            "useful when comparing models fitted to the same response and cases.";
    } else if (key == "r2" || key == "r²" || contains("r-squared")) {
        explanation =
            "R² is the proportion of observed response variance explained by the "
            "linear predictions. It describes fit in this sample and is not, by itself, "
            "evidence of causality or out-of-sample accuracy.";
    } else if (contains("Δs vs") || contains("delta s vs")) {
        explanation =
            "Δs is the change in residual standard deviation from the previous linear "
            "model, in the response's original units. A negative value means the newer "
            "model has smaller typical residuals.";
    } else if (key == "s" || key == "rmse" || contains("residual standard") ||
               contains("sigma")) {
        explanation =
            "s is the residual standard deviation: the typical size of a residual in "
            "the response's original units. Smaller values indicate closer fitted values "
            "only when models use the same response and observations.";
    } else if (contains("null deviance")) {
        explanation =
            "Null deviance measures lack of fit for an intercept-only generalized linear "
            "model. Its reduction to residual deviance indicates how much the predictors "
            "improve fit on the deviance scale.";
    } else if (contains("residual dev") || key == "deviance") {
        explanation =
            "Residual deviance measures lack of fit of the current generalized linear "
            "model. It is mainly interpreted relative to the null deviance or a valid "
            "nested model; its absolute scale depends on the response family.";
    } else if (contains("Δ deviance") || contains("delta deviance")) {
        explanation =
            "Δ deviance is the change in deviance from the previous nested model. "
            "For ordinary likelihood models it underlies the likelihood-ratio comparison; "
            "a positive reduction favors the larger model.";
    } else if (key == "aic" || contains("Δaic")) {
        explanation =
            "AIC balances likelihood fit against model complexity. Lower values are "
            "preferred only among models fitted to the same response and observations; "
            "differences, rather than the absolute value, are interpreted.";
    } else if (key == "bic" || contains("Δbic")) {
        explanation =
            "BIC balances likelihood fit against complexity with a stronger sample-size "
            "penalty than AIC. Lower values are preferred only for directly comparable "
            "models fitted to the same data.";
    } else if (contains("loglik") || contains("log-likelihood") || contains("log likelihood")) {
        explanation =
            "The log-likelihood measures how compatible the observed data are with the "
            "fitted probability model. Larger values indicate better likelihood fit and "
            "support AIC, BIC and likelihood-ratio comparisons.";
    } else if (contains("nagelkerke") || contains("pseudo-r")) {
        explanation =
            "Nagelkerke pseudo-R² rescales the improvement in likelihood over an intercept-only "
            "binary model to a nominal 0-to-1 range. It is a descriptive likelihood index, not "
            "the proportion of outcome variance explained and not directly comparable with linear R².";
    } else if (key == "auc" || contains("apparent auc")) {
        explanation =
            "AUC is the probability that the fitted score ranks a randomly chosen event above "
            "a randomly chosen reference case. 'Apparent' means it was evaluated on the fitting "
            "sample and can therefore be optimistic without validation.";
    } else if (contains("dispersion")) {
        explanation =
            "Dispersion describes residual variability relative to the variance implied "
            "by the response family. Values materially above one can indicate "
            "overdispersion for families whose nominal dispersion is one.";
    } else if (contains("sum of squares") || key == "ss" || contains("Δsse")) {
        explanation = contains("Δ") || contains("delta")
            ? "ΔSSE is the change in residual sum of squares from the previous nested "
              "linear model. A reduction means the added terms explain additional variation."
            : "Sum of Squares partitions response variation into variation explained by "
              "the fitted linear model and unexplained residual variation.";
    } else if (contains("mean square") || key == "ms") {
        explanation =
            "Mean Square is a sum of squares divided by its degrees of freedom. The model "
            "mean square divided by the residual mean square forms the ordinary global F test.";
    } else if (key == "df1" || key == "df2" || key == "gl") {
        explanation = key == "df1"
            ? "df1 is the numerator degrees of freedom for the omnibus test and reflects the number of independent "
              "effect contrasts being tested."
            : key == "df2"
            ? "df2 is the denominator degrees of freedom used for the omnibus F reference distribution."
            : "gl reports the degrees of freedom of this test statistic under its null reference distribution.";
    } else if (key == "df" || contains("df residual") || contains("Δdf")) {
        explanation = (key == "df" && (analysisContains("mean") ||
                       analysisContains("sample") || analysisContains("pairwise")))
            ? "df is the degrees of freedom used by the displayed test's reference distribution. It reflects "
              "the usable sample information and, for Welch-type tests, may be non-integer."
            : contains("Δ") || contains("delta")
            ? "Δdf is the change in model degrees of freedom relative to the previous "
              "model; it is the number of independently estimable parameters added or removed."
            : "Residual degrees of freedom equal the number of usable observations minus "
              "the rank of the fitted design matrix. They determine the reference distribution "
              "for residual variance and many tests.";
    } else if ((key == "b" || key == "c") && methodContains("mcnemar")) {
        explanation = key == "b"
            ? "b is the number of discordant pairs in which only the first displayed variable is an event."
            : "c is the number of discordant pairs in which only the second displayed variable is an event.";
    } else if (key == "b" || key == "estimate" || contains("coefficient")) {
        explanation = analysisContains("pairwise") || analysisContains("mean_comparison") ||
                      analysisContains("mean comparison")
            ? "Estimate is the displayed contrast in the order named by the row, normally "
              "the first estimated marginal mean minus the second. Its sign therefore changes "
              "if the comparison order is reversed."
            : "b is the unstandardized coefficient. Holding the other terms fixed, it is the "
              "expected change in the model's linear predictor for a one-unit change in this "
              "predictor, or the contrast with the displayed reference category for a categorical predictor.";
    } else if (key == "β" || contains("standardized beta")) {
        explanation =
            "β is a standardized linear-regression coefficient, expressed in response "
            "standard deviations per predictor standard deviation while holding other terms "
            "fixed. It is not shown for categorical contrasts or models where that standardization "
            "would be misleading.";
    }
    }
    if (explanation.empty()) {
    if (key == "se" || key.rfind("se", 0) == 0 || contains("standard error")) {
        explanation =
            analysisContains("mean") || analysisContains("pair") || analysisContains("sample")
            ? "SE is the estimated sampling uncertainty of the displayed mean, proportion, or contrast. It is "
              "used to form the corresponding test statistic and confidence interval."
            : "SE is the estimated sampling uncertainty of the coefficient on the model's "
              "linear-predictor scale. It is used to form the displayed test statistic and "
              "confidence interval.";
    } else if (contains("d1")) {
        explanation =
            "D1 is mice's pooled multivariate Wald test. It combines coefficient estimates "
            "and their within- and between-imputation covariance under Rubin's rules; here it "
            "tests the displayed term or nested-model difference.";
    } else if (contains("d3")) {
        explanation =
            "D3 is mice's pooled likelihood-ratio test for nested multiply-imputed models. "
            "It is used only when the fitted family has an ordinary likelihood; it is not "
            "valid for quasi-likelihood models such as quasi-Poisson.";
    } else if (key == "test") {
        explanation =
            "Test identifies or reports the inferential statistic used for this row. Its precise null hypothesis, "
            "reference distribution and assumptions are determined by the named analysis method; interpret it "
            "together with the accompanying p value and effect estimate.";
    } else if (key == "t" || key == "z" || key == "statistic" || key == "t/z" ||
               contains("t / z") || contains("wald")) {
        explanation = analysisContains("mean") || analysisContains("pairwise")
            ? "The test statistic expresses the displayed mean, rank or marginal-mean contrast "
              "relative to its estimated sampling uncertainty. Its sign follows the displayed "
              "comparison order and its reference distribution is determined by the named test."
            : "The t/z Wald statistic compares an estimated coefficient with zero by dividing "
              "it by its standard error. Large absolute values provide stronger evidence against "
              "a zero coefficient, subject to the fitted model's assumptions.";
    } else if (key == "χ²" || key == "chi-square" || key == "chi squared") {
        explanation =
            "The chi-square statistic compares the observed result with its null-hypothesis expectation. Its "
            "degrees of freedom and exact interpretation are determined by the named contingency, proportion, "
            "rank or omnibus test.";
    } else if (contains("χ²/f") || contains("chi-square/f") || contains("chi2/f")) {
        explanation =
            "This nested-model comparison uses a likelihood-ratio χ² statistic for "
            "ordinary likelihood models and an F-type statistic where dispersion must be "
            "estimated, such as quasi-likelihood models. The displayed method is therefore "
            "determined by the fitted family.";
    } else if (key == "f" || contains("partial f")) {
        explanation = analysisContains("anova")
            ? "The ANOVA F statistic compares variation attributed to the effect with residual "
              "variation. The p value tests the omnibus null hypothesis that the corresponding "
              "population means are equal under the selected ANOVA assumptions."
            : contains("partial") || context.comparison
            ? "The partial F statistic compares two nested linear models fitted to the same "
              "observations. It tests whether the terms added in the larger model improve fit."
            : "The global F statistic tests the fitted linear model against an intercept-only "
              "model. Its null hypothesis is that all non-intercept coefficients are zero.";
    } else if (key == "p" || contains("p vs") || contains("adjusted p") ||
               contains("p adjusted") ||
               contains("p lr")) {
        explanation = contains("adjust")
            ? "The adjusted p value controls the stated multiple-comparison error criterion. "
              "It is the value to use for the corresponding family of pairwise tests."
            : "The p value is the probability, under the stated null hypothesis and model "
              "assumptions, of a test statistic at least as extreme as the observed one. It "
              "does not measure effect size or the probability that the null is true.";
    } else if (contains("lr") || contains("chi") || contains("χ")) {
        explanation =
            "The likelihood-ratio statistic compares nested likelihood models fitted to the "
            "same observations. It tests whether the reduction in deviance is larger than "
            "expected from the added parameters.";
    } else if (contains("partial r")) {
        explanation =
            "Partial r expresses the association between this predictor contrast and the "
            "response after controlling for the other model terms. Its sign indicates the "
            "direction of the association and its magnitude is bounded by one. In an ordinary linear model, "
            "r = t / sqrt(t^2 + residual df). With multiple imputation, LinkEDA calculates "
            "r in each completed-data linear model, averages atanh(r), and transforms back "
            "with tanh. It does not use the pooled t or MI-adjusted df. In an MI table, the p column tests "
            "the regression coefficient using Rubin pooling, not this correlation summary.";
    } else if (contains("Δr") || contains("delta r")) {
        explanation =
            "ΔR² is the additional proportion of response variance explained relative "
            "to the previous nested linear model fitted to the same observations.";
    } else if (contains("odds ratio") || key == "or" || contains("or /")) {
        explanation =
            "The odds ratio is exp(b) for a logit model. Values above one indicate higher "
            "event odds and values below one lower odds, holding the other predictors fixed; "
            "the confidence interval conveys its uncertainty.";
    } else if (key == "or_m" || contains("matched odds")) {
        explanation =
            "The matched odds ratio compares the two directions of discordance in paired binary data. Values "
            "above one favour events unique to the first variable; values below one favour the second.";
    } else if (contains("rate ratio") || key == "rr") {
        explanation =
            "The rate ratio is exp(b) for a count model with a log link. It is the multiplicative "
            "change in the expected count or rate, holding the other predictors fixed.";
    } else if (contains("confidence interval") || contains("95% ci") || key == "ci" ||
               contains("lower 95") || contains("upper 95") ||
               contains("ci lower") || contains("ci upper") || key == "ci low" || key == "ci high") {
        explanation =
            "The confidence interval is the range of parameter values compatible with the "
            "estimate at the stated confidence level under the fitted model. Its width reflects "
            "sampling uncertainty; it is not a range containing 95% of the observations.";
    } else {
        explanation =
            original + " is a statistic reported for this analysis. Interpret it together "
            "with the corresponding estimate, uncertainty, assumptions and relevant diagnostics; "
            "a displayed dash means it is not available for this result.";
    }
    }

    if (key == "b" || key == "estimate" || contains("coefficient")) {
        const std::string family = LowerCopy(context.modelFamily);
        const std::string link = LowerCopy(context.link);
        if (family == "binomial" && link == "logit") {
            explanation +=
                "\n\nModel-specific note: in this logit model b is a change in log odds for "
                "the selected event; exp(b) is the corresponding odds ratio.";
        } else if (family == "binomial" && link == "probit") {
            explanation +=
                "\n\nModel-specific note: in this probit model b is a change on the latent "
                "standard-normal index. It is not a log-odds coefficient or an odds ratio.";
        } else if (family == "binomial" && link == "cloglog") {
            explanation +=
                "\n\nModel-specific note: in this complementary-log-log model b is a change "
                "on the log[-log(1-p)] scale; exp(b) is not an ordinary odds ratio.";
        } else if ((family == "poisson" || family == "quasipoisson" ||
                    family == "quasi-poisson" || family == "negative_binomial" ||
                    family == "negative binomial") && link == "log") {
            explanation +=
                "\n\nModel-specific note: with this log link b is a log expected-count/rate "
                "contrast; exp(b) is the multiplicative rate ratio.";
        } else if (family == "gaussian" && (link.empty() || link == "identity")) {
            explanation +=
                "\n\nModel-specific note: with the Gaussian identity model b is expressed "
                "directly in the response variable's original units.";
        }
    }
    if (context.comparison && !contains("vs prev") &&
        key != "model fit" && key != "b" && key != "estimate") {
        explanation +=
            "\n\nComparison note: interpret this value for models fitted to the same response "
            "and analysis rows. Sequential tests refer to the immediately previous model column.";
    }

    if (context.method.empty() && !context.detail.empty()) explanation += "\n\n" + context.detail;

    if (!context.method.empty() && !method.empty()) {
        explanation += "\n\nMethod note: this value belongs to the " +
            context.method + " analysis";
        if (!context.detail.empty()) explanation += " (" + context.detail + ")";
        explanation += analysisContains("descriptive") || methodContains("descriptive")
            ? ". Its definition and missing-data handling are those stated for that analysis."
            : ". Its null hypothesis, reference distribution and assumptions are those of that named method.";
    }
    if (!context.adjustment.empty() &&
        (contains("p") || contains("comparison") || analysisContains("pairwise"))) {
        explanation += "\n\nMultiplicity note: the reported family of comparisons uses " +
            context.adjustment + ". Interpret the adjusted p value rather than the unadjusted "
            "one when making conclusions across that family.";
    }

    if (context.multipleImputation) {
        const bool informationCriterion = key == "aic" || key == "bic" ||
            contains("loglik") || contains("deviance") || key == "s" ||
            contains("dispersion") || contains("precision") || key == "theta" ||
            key == "intra-trial correlation (rho)" ||
            key == "variance inflation vs binomial" ||
            key == "variance inflation vs poisson" ||
            key == "implied coefficient of variation";
        if (!context.rubinRulesApplied) {
            explanation +=
                "\n\nMultiple-imputation note: none of the response or predictor values used "
                "by this analysis was imputed. The completed-data fits are identical, so "
                "Rubin's rules are not required for this statistic.";
        } else if (key == "pooled wald χ²") {
            explanation +=
                "\n\nMultiple-imputation note: the joint Wald chi-square uses the pooled coefficient vector and its full Rubin-combined covariance for this term. This is not a one-coefficient t test or a mice::D1 F statistic.";
        } else if (contains("d3") || context.miMethod.find("D3") != std::string::npos) {
            explanation +=
                "\n\nMultiple-imputation note: the displayed comparison was calculated by "
                "mice::D3; it is not being presented as a D1 result.";
        } else if (contains("d1") || contains("wald") ||
                   context.miMethod.find("D1") != std::string::npos) {
            explanation +=
                "\n\nMultiple-imputation note: this is the pooled mice::D1/Wald result, which "
                "uses Rubin's within- and between-imputation uncertainty.";
        } else if (key == "intra-trial correlation (rho)" ||
                   key == "variance inflation vs binomial" ||
                   key == "variance inflation vs poisson" ||
                   key == "implied coefficient of variation" ||
                   key == "pearson dispersion ratio") {
            explanation +=
                "\n\nMultiple-imputation note: this diagnostic is calculated separately from each completed-data R fit, then reported as a descriptive mean. Nonlinear transformations are performed before averaging. It is not Rubin-pooled.";
        } else if (informationCriterion) {
            explanation +=
                "\n\nMultiple-imputation note: this fit quantity is descriptive across the "
                "per-imputation models; it is not a coefficient pooled by Rubin's rules. AIC, "
                "BIC and log-likelihood are intentionally unavailable when no valid pooled "
                "definition is used.";
        } else if (key == "r2" || key == "r²" || contains("adjusted r")) {
            explanation +=
                "\n\nMultiple-imputation note: the linear-model R² summary is obtained with "
                "mice::pool.r.squared(), not by applying an ad-hoc averaging formula.";
        } else if (!analysis.empty() && !analysisContains("regression") &&
                   !analysisContains("model")) {
            explanation += "\n\nMultiple-imputation note: this analysis reports its pooling method as ";
            explanation += context.miMethod.empty() ? "the validated method shown in the result"
                                                    : context.miMethod;
            explanation += ". The interpretation is statistic-specific; LinkEDA does not substitute a "
                "single-imputation preview or label an unpooled value as a Rubin-pooled result.";
        } else {
            explanation +=
                "\n\nMultiple-imputation note: coefficient estimates and their uncertainty "
                "are pooled with mice::pool() using Rubin's rules. Diagnostics and residual "
                "fit quantities remain tied to the individual imputation fits.";
        }
    }
    if (!context.likelihoodAvailable &&
        (contains("lr") || contains("d3") || key == "aic" || key == "bic" ||
         contains("loglik"))) {
        explanation +=
            context.multipleImputation
                ? "\n\nNo pooled likelihood statistic is available here. The individual fits may still define a likelihood."
                : "\n\nAn ordinary likelihood statistic is unavailable for this fit; quasi-likelihood models do not define one.";
    }
    return explanation;
}

bool HasModelTerm(const GroupModelState &state, const std::string &term)
{
    return TermListContainsEquivalentModelTerm(state.terms, term);
}

void MarkGroupModelChanged(GroupModelState &state)
{
    // Editing an MI model invalidates its fit, but must not turn it into an
    // ordinary model.  Losing this identity made All/Selected scopes fit the
    // compact preview and caused a closed/reopened window to forget the exact
    // selected rows.  Preserve the backend ownership metadata while clearing
    // only the result identity that is no longer valid.
    const bool multipleImputation = state.multipleImputation;
    state.modelVersion += 1;
    state.frozenScopeNotice = state.autoRefit
        ? std::string{}
        : "Auto-refit is off. Displaying the last completed result; its data or model specification may differ from the current controls.";
    state.isStale = true;
    if (!multipleImputation) {
        state.precomputed = false;
        state.imputationCount = 0;
        state.title.clear();
        state.note.clear();
    } else {
        state.precomputed = true;
    }
    state.rFitPending = false;
    state.lastRFitSignature.clear();
}

ModelPredictorMetadata PredictorMetadataForModelSpecification(
    const PlotModel &model,
    const DataFrameModel *dataframe,
    const std::string &variable)
{
    ModelPredictorMetadata metadata;
    metadata.variable = variable;
    if (dataframe) {
        const DataColumn *column = FindDataColumnInDataFrame(*dataframe, variable);
        if (column) {
            metadata.storageType = VariableTypeIsFactorLike(column->type)
                ? "factor" : "numeric";
            std::set<std::string> levels;
            for (const std::string &value : column->values) {
                const std::string normalized = TrimCopy(value);
                if (normalized.empty() || normalized == "NA") continue;
                ++metadata.observedCount;
                levels.insert(normalized);
            }
            metadata.uniqueCount = levels.size();
            metadata.levels.assign(levels.begin(), levels.end());
            return metadata;
        }
    }
    const NumericVariable *numeric = FindNumericVariable(model, variable);
    if (!numeric) return metadata;
    std::vector<double> levels;
    for (double value : numeric->values) {
        if (!std::isfinite(value)) continue;
        ++metadata.observedCount;
        if (std::none_of(levels.begin(), levels.end(), [value](double existing) {
                return std::fabs(existing - value) <= 1.0e-9;
            })) {
            levels.push_back(value);
        }
    }
    std::sort(levels.begin(), levels.end());
    metadata.uniqueCount = levels.size();
    for (double level : levels) metadata.levels.push_back(FactorLevelTermValue(level));
    return metadata;
}

ModelPredictorMetadata PredictorMetadataForLinearModel(
    const PlotModel &model,
    const DataFrameModel *dataframe,
    const std::string &variable)
{
    return PredictorMetadataForModelSpecification(model, dataframe, variable);
}

bool SynchronizeModelSpecificationTermTypes(ModelSpecification &specification,
                                             const DataFrameModel &dataframe)
{
    const ModelSpecification before = specification;
    std::map<std::string, std::string> canonicalTypes;
    std::set<std::string> modelVariables;
    for (const std::string &term : specification.terms) {
        for (const std::string &variable : UniqueBaseVariablesForTerm(term)) {
            modelVariables.insert(variable);
            const DataColumn *column = FindDataColumnInDataFrame(dataframe, variable);
            if (column && VariableTypeIsFactorLike(column->type)) {
                canonicalTypes[variable] = "factor";
            } else if (column && NormalizeVariableType(column->type) == "numeric" &&
                       specification.termTypes.find(variable) != specification.termTypes.end()) {
                // Pooled/precomputed payloads may spell out numeric types.
                // Retain that harmless explicit form so merely displaying the
                // model does not make the precomputed result stale.
                canonicalTypes[variable] = "numeric";
            }
        }
    }

    // A plot can deliberately interpret a numeric storage column as a factor
    // (notably a Trellis conditioning variable). That is model semantics, not
    // dataset metadata, and must survive synchronization and later refits.
    for (auto overrideIt = specification.termTypeOverrides.begin();
         overrideIt != specification.termTypeOverrides.end(); ) {
        const bool validType = overrideIt->second == "numeric" ||
            overrideIt->second == "factor";
        const bool validVariable = modelVariables.find(overrideIt->first) !=
            modelVariables.end() &&
            FindDataColumnInDataFrame(dataframe, overrideIt->first) != nullptr;
        if (!validType || !validVariable) {
            overrideIt = specification.termTypeOverrides.erase(overrideIt);
            continue;
        }
        canonicalTypes[overrideIt->first] = overrideIt->second;
        ++overrideIt;
    }
    specification.termTypes = std::move(canonicalTypes);
    PruneModelSpecificationState(specification);
    return !EquivalentModelSpecifications(before, specification);
}

bool SynchronizeGroupModelTermTypes(GroupModelState &state,
                                    const DataFrameModel &dataframe)
{
    const ModelSpecification before = state;
    state.familyKind = ModelFamilyKind::Linear;
    SynchronizeModelSpecificationTermTypes(state, dataframe);
    return !EquivalentModelSpecifications(before, state);
}

GeneralizedGLMState CreateIndependentGeneralizedGLMSession(
    const std::string &id,
    const std::string &group,
    const PlotModel &seed,
    const DataFrameModel &dataframe,
    SharedAnalysisKind analysisKind,
    const AnalysisVariableRoles &roles,
    const AnalysisScope &dataScope,
    const std::string &scope,
    const ModelSpecification *savedSpecification)
{
    if (analysisKind != SharedAnalysisKind::GeneralizedLinearModel &&
        analysisKind != SharedAnalysisKind::PositiveContinuousModel &&
        analysisKind != SharedAnalysisKind::ProportionModel &&
        analysisKind != SharedAnalysisKind::CountRegression &&
        analysisKind != SharedAnalysisKind::BinaryRegression) {
        analysisKind = SharedAnalysisKind::GeneralizedLinearModel;
    }

    AnalysisSpecification savedVariables;
    if (savedSpecification) {
        if (!savedSpecification->response.empty())
            savedVariables["response"] = {savedSpecification->response};
        savedVariables["predictors"] = savedSpecification->terms;
    }
    const InitialAnalysisSpecification initial = ResolveInitialAnalysisSpecification(
        dataframe, SharedAnalysisDefinition(analysisKind), roles,
        savedSpecification ? &savedVariables : nullptr);

    GeneralizedGLMState state;
    if (savedSpecification) {
        // ModelSpecification owns all nested containers. This assignment is a
        // deliberate semantic deep copy, never an alias to another session.
        static_cast<ModelSpecification &>(state) = *savedSpecification;
    }
    state.familyKind = ModelFamilyKind::GeneralizedLinear;
    state.id = id;
    state.group = group;
    state.datasetType = dataframe.datasetType;
    state.imputationSetId = dataframe.imputationId;
    state.sourceDatasetId = dataframe.sourceDatasetId;
    state.multipleImputation = dataframe.datasetType == "multiple_imputation";
    state.imputationCount = state.multipleImputation
        ? std::max(1, dataframe.imputationCount) : 0;
    state.seed = seed;
    state.hasSeed = true;
    state.dataScope = dataScope;
    state.dataScopeCaptured = true;
    state.scope = scope;
    state.response = InitialAnalysisVariable(initial, "response");
    state.terms = savedSpecification
        ? savedSpecification->terms
        : InitialAnalysisVariables(initial, "predictors");
    state.terms.erase(std::remove_if(state.terms.begin(), state.terms.end(),
        [&](const std::string &term) {
            const auto variables = UniqueBaseVariablesForTerm(term);
            const bool variablesExist = !variables.empty() &&
                std::all_of(variables.begin(), variables.end(),
                    [&](const std::string &variable) {
                        return FindDataColumnInDataFrame(dataframe, variable) != nullptr;
                    });
            return !variablesExist || (!state.response.empty() &&
                ModelTermShouldBeRemoved(term, state.response));
        }), state.terms.end());

    std::set<int> validationRows;
    if (dataScope.kind == AnalysisScopeKind::ExplicitRowIds) {
        validationRows.insert(dataScope.originalRowIds.begin(), dataScope.originalRowIds.end());
    }

    state.binaryRegression = analysisKind == SharedAnalysisKind::BinaryRegression;
    state.countRegression = analysisKind == SharedAnalysisKind::CountRegression;
    state.modelType = state.binaryRegression ? StatisticalModelType::Binary
        : state.countRegression ? StatisticalModelType::Count
        : analysisKind == SharedAnalysisKind::PositiveContinuousModel
            ? StatisticalModelType::PositiveContinuous
        : analysisKind == SharedAnalysisKind::ProportionModel
            ? StatisticalModelType::Proportion
            : StatisticalModelType::LegacyGeneralized;
    if (state.binaryRegression) {
        state.title = StatisticalModelTypeLabel(state.modelType);
        state.family = "binomial";
        state.binaryLink = BinaryLink::Logit;
        state.link = BinaryLinkId(state.binaryLink);
        if (!state.response.empty()) {
            const DataColumn *column = FindDataColumnInDataFrame(dataframe, state.response);
            if (column) {
                state.responseCoding = InspectBinaryResponse(*column, validationRows);
                state.responseCodingExplicit = state.responseCoding.ok;
            }
            if (!state.responseCoding.ok) state.response.clear();
        }
    } else if (state.countRegression) {
        state.title = StatisticalModelTypeLabel(state.modelType);
        state.countDistribution = CountDistribution::Poisson;
        state.family = "poisson";
        state.link = "log";
        if (!state.response.empty()) {
            const DataColumn *column = FindDataColumnInDataFrame(dataframe, state.response);
            if (!column || !InspectCountResponse(*column, validationRows).ok) state.response.clear();
        }
    } else if (state.modelType == StatisticalModelType::PositiveContinuous) {
        state.title = StatisticalModelTypeLabel(state.modelType);
        state.family = "Gamma";
        state.link = DefaultLinkForModelDistribution(state.modelType, state.family);
        if (!state.response.empty()) {
            const DataColumn *column = FindDataColumnInDataFrame(dataframe, state.response);
            const DistributionSpec *distribution = FindDistributionSpecification(state.family);
            if (!column || !distribution || !InspectResponseDomain(
                    *column, distribution->responseDomain, validationRows).ok) {
                state.response.clear();
            }
        }
    } else if (state.modelType == StatisticalModelType::Proportion) {
        state.title = StatisticalModelTypeLabel(state.modelType);
        state.family = "beta";
        state.link = DefaultLinkForModelDistribution(state.modelType, state.family);
        state.responseBoundsConfigured = true;
        state.responseLower = 0.0;
        state.responseUpper = 1.0;
        if (!state.response.empty()) {
            const DataColumn *column = FindDataColumnInDataFrame(dataframe, state.response);
            if (!column || !InspectResponseDomain(
                    *column, ResponseDomain::OpenUnitInterval, validationRows).ok) {
                state.response.clear();
            }
        }
    } else {
        state.family = state.response.empty()
            ? "gaussian" : DefaultGeneralizedFamilyForResponse(seed, state.response);
        state.link = DefaultGeneralizedLink(state.family);
    }

    // Dataset metadata determines defaults. Only an explicitly supplied saved
    // specification may carry model-local overrides, centering or references.
    if (!savedSpecification) {
        state.termTypes.clear();
        state.termTypeOverrides.clear();
        state.centeredPredictors.clear();
        state.factorReferenceLevels.clear();
    }
    SynchronizeModelSpecificationTermTypes(state, dataframe);
    state.modelVersion = 1;
    state.status = state.response.empty()
        ? "Choose a response variable before fitting."
        : state.terms.empty() && !state.binaryRegression && !state.countRegression
            ? "Add at least one independent variable before fitting."
            : "Not fitted.";
    return state;
}

bool SynchronizeGeneralizedGLMFitSpecification(
    GeneralizedGLMState &state,
    const DataFrameModel &dataframe)
{
    state.familyKind = ModelFamilyKind::GeneralizedLinear;
    return SynchronizeModelSpecificationTermTypes(state, dataframe);
}

bool LinearGLMFitMatchesTermTypes(const GLMFitSummary &fit,
                                  const std::vector<std::string> &terms,
                                  const std::map<std::string, std::string> &termTypes)
{
    std::set<std::string> variables;
    for (const std::string &term : terms) {
        const auto bases = UniqueBaseVariablesForTerm(term);
        variables.insert(bases.begin(), bases.end());
    }
    for (const std::string &variable : variables) {
        const auto expected = termTypes.find(variable);
        const bool expectsFactor = expected != termTypes.end() && expected->second == "factor";
        bool found = false;
        bool observedFactor = false;
        for (const GLMCoefficientRow &row : fit.coefficients) {
            if (row.sourceTerm != variable && row.term != variable) continue;
            found = true;
            observedFactor = observedFactor || row.termType == "factor" ||
                row.rowType == "factor_parent" || row.rowType == "factor_level" ||
                row.rowType == "reference";
        }
        // An aliased/failed term may have no coefficient rows.  In that case
        // there is no reliable type evidence in the result to reject it on.
        if (found && observedFactor != expectsFactor) return false;
    }
    return true;
}

bool LinearGLMFitMayBePresented(const GroupModelState &state,
                                const std::string &currentSignature,
                                const GLMFitSummary *fit)
{
    return fit != nullptr && !state.isStale && !state.rFitPending &&
        !currentSignature.empty() && state.lastRFitSignature == currentSignature;
}

bool SynchronizeRegressionComparisonTermTypes(RegressionComparisonState &state,
                                              const DataFrameModel &dataframe)
{
    std::set<std::string> variables;
    for (const RegressionComparisonModel &model : state.models) {
        for (const std::string &term : model.terms) {
            const auto bases = UniqueBaseVariablesForTerm(term);
            variables.insert(bases.begin(), bases.end());
        }
    }
    for (const std::string &term : state.termRows) {
        if (term == "(Intercept)") continue;
        const auto bases = UniqueBaseVariablesForTerm(ModelTermBaseForDisplayRow(term));
        variables.insert(bases.begin(), bases.end());
    }

    std::map<std::string, std::string> canonicalTypes;
    for (const std::string &variable : variables) {
        const DataColumn *column = FindDataColumnInDataFrame(dataframe, variable);
        if (column && VariableTypeIsFactorLike(column->type)) {
            canonicalTypes[variable] = "factor";
        } else if (column && NormalizeVariableType(column->type) == "numeric" &&
                   state.termTypes.find(variable) != state.termTypes.end()) {
            // Preserve an explicit numeric spelling carried by precomputed
            // comparison results, as in the single-model GLM path.
            canonicalTypes[variable] = "numeric";
        }
    }
    const bool termTypesChanged = canonicalTypes != state.termTypes;
    if (termTypesChanged) state.termTypes = canonicalTypes;
    bool modelSpecificationsChanged = false;
    for (RegressionComparisonModel &model : state.models) {
        const ModelSpecification before = model;
        model.familyKind = ModelFamilyKind::Linear;
        model.response = RegressionComparisonEffectiveResponse(state, model);
        model.scope = state.scope;
        model.dataScope = state.dataScope;
        model.dataScopeCaptured = state.dataScopeCaptured;
        model.termTypes.clear();
        for (const std::string &term : model.terms) {
            for (const std::string &variable : UniqueBaseVariablesForTerm(term)) {
                const auto foundType = canonicalTypes.find(variable);
                if (foundType != canonicalTypes.end()) {
                    model.termTypes[variable] = foundType->second;
                }
            }
        }
        PruneModelSpecificationState(model);
        modelSpecificationsChanged =
            !EquivalentModelSpecifications(before, model) || modelSpecificationsChanged;
    }
    return termTypesChanged || modelSpecificationsChanged;
}

bool SynchronizeRegressionComparisonDatasetIdentity(
    RegressionComparisonState &state,
    const DataFrameModel &dataframe)
{
    const bool multiple = dataframe.datasetType == "multiple_imputation";
    const int count = multiple ? std::max(1, dataframe.imputationCount) : 0;
    const bool changed = state.group != dataframe.group ||
        state.datasetType != dataframe.datasetType ||
        state.imputationSetId != dataframe.imputationId ||
        state.sourceDatasetId != dataframe.sourceDatasetId ||
        state.multipleImputation != multiple || state.imputationCount != count;
    state.group = dataframe.group;
    state.datasetType = dataframe.datasetType;
    state.imputationSetId = dataframe.imputationId;
    state.sourceDatasetId = dataframe.sourceDatasetId;
    state.multipleImputation = multiple;
    state.imputationCount = count;
    return changed;
}

std::map<std::string, std::string> RegressionComparisonModelTermTypes(
    const RegressionComparisonState &state,
    const RegressionComparisonModel &model)
{
    std::map<std::string, std::string> types = state.termTypes;
    for (const auto &entry : model.termTypes) types[entry.first] = entry.second;
    for (const auto &entry : model.termTypeOverrides) {
        if (entry.second == "numeric" || entry.second == "factor") {
            types[entry.first] = entry.second;
        }
    }
    return types;
}

std::string RegressionComparisonModelTermType(const RegressionComparisonState &state,
                                              int modelIndex,
                                              const std::string &term)
{
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size()) {
        return ModelTermDisplayType(&state.seed, term, state.termTypes);
    }
    return ModelTermDisplayType(
        &state.seed, term,
        RegressionComparisonModelTermTypes(state, state.models[static_cast<std::size_t>(modelIndex)]));
}

std::string RegressionComparisonEffectiveResponse(const RegressionComparisonState &state,
                                                  const RegressionComparisonModel &model)
{
    return model.response.empty() ? state.response : model.response;
}

ModelSpecification EffectiveRegressionComparisonModelSpecification(
    const RegressionComparisonState &state,
    const RegressionComparisonModel &model)
{
    ModelSpecification specification = model;
    specification.familyKind = ModelFamilyKind::Linear;
    specification.response = RegressionComparisonEffectiveResponse(state, model);
    specification.scope = state.scope;
    specification.dataScope = state.dataScope;
    specification.dataScopeCaptured = state.dataScopeCaptured;
    const auto comparisonTypes = RegressionComparisonModelTermTypes(state, model);
    specification.termTypes.clear();
    for (const std::string &term : specification.terms) {
        for (const std::string &variable : UniqueBaseVariablesForTerm(term)) {
            const auto foundType = comparisonTypes.find(variable);
            if (foundType != comparisonTypes.end()) {
                specification.termTypes[variable] = foundType->second;
            }
        }
    }
    PruneModelSpecificationState(specification);
    return specification;
}

RegressionComparisonFitState RegressionComparisonResolvedFitState(
    const RegressionComparisonState &state,
    const RegressionComparisonModel &model)
{
    if (state.rFitPending && model.isStale) return RegressionComparisonFitState::Pending;
    if (model.isStale) return RegressionComparisonFitState::NotFitted;
    if (model.fit.ok) return RegressionComparisonFitState::Valid;
    if (!model.fit.warning.empty() || model.fitState == RegressionComparisonFitState::Error) {
        return RegressionComparisonFitState::Error;
    }
    return model.fitState;
}

GroupModelState &EnsureGroupModelState(std::map<std::string, GroupModelState> &states,
                                       const std::string &group,
                                       const PlotModel *seed)
{
    GroupModelState &state = states[group];
    if (state.modelId.empty()) state.modelId = group;
    if (state.group.empty()) {
        state.group = seed && !seed->group.empty() ? seed->group : group;
    }
    return state;
}

std::string CreateLinearModelInstance(std::map<std::string, GroupModelState> &states,
                                      const std::string &datasetGroup)
{
    static std::atomic<std::uint64_t> nextNumber{0};
    const std::string prefix = "linear_model:" + datasetGroup + ":";
    std::string id;
    do { id = prefix + std::to_string(++nextNumber); }
    while (states.find(id) != states.end());
    GroupModelState &state = states[id];
    state.modelId = id;
    state.group = datasetGroup;
    state.title = "Linear Model " + id.substr(prefix.size());
    return id;
}

std::string EncodePooledRegressionComparisonModelSpec(const RegressionComparisonState &state)
{
    std::vector<CommandModelSpec> models;
    models.reserve(state.models.size());
    for (const RegressionComparisonModel &model : state.models) {
        std::string response = model.response.empty() ? state.response : model.response;
        models.push_back(CommandModelSpec{model.label, response, model.terms});
    }
    return EncodeCommandModelSpecTable(models);
}

bool HasRegressionTerm(const RegressionComparisonState &state, const std::string &term)
{
    return TermListContainsEquivalentModelTerm(state.termRows, term);
}

bool ModelIncludesTerm(const RegressionComparisonModel &model, const std::string &term)
{
    return TermListContainsEquivalentModelTerm(model.terms, term);
}

void AddRegressionTermRow(RegressionComparisonState &state, const std::string &term)
{
    if (term.empty() || HasRegressionTerm(state, term)) {
        return;
    }
    state.termRows.push_back(term);
}

void InvalidateRegressionComparisonModel(RegressionComparisonState &state,
                                         RegressionComparisonModel &model)
{
    model.modelVersion += 1;
    model.isStale = true;
    model.fitState = RegressionComparisonFitState::NotFitted;
    model.fit = GLMFitSummary();
    model.fitVersion = 0;
    model.comparisonOk = false;
    model.comparisonDf = 0;
    model.comparisonDf2 = NAN;
    model.comparisonDelta = NAN;
    model.comparisonStatistic = NAN;
    model.comparisonP = NAN;
    state.rFitPending = false;
    state.lastRFitSignature.clear();
}

namespace {

ModelPredictorMetadata RegressionComparisonPredictorMetadata(
    const RegressionComparisonState &state,
    const RegressionComparisonModel &model,
    const std::string &term)
{
    const std::string variableName = BaseVariableForTermComponent(term);
    ModelPredictorMetadata metadata = PredictorMetadataForLinearModel(
        state.seed, nullptr, variableName);
    const auto datasetType = state.termTypes.find(metadata.variable);
    metadata.storageType = datasetType != state.termTypes.end()
        ? datasetType->second : "numeric";
    (void)model;
    return metadata;
}

} // namespace

namespace {

void AddComparisonCandidateTerm(std::vector<std::string> &candidates,
                                const std::string &term)
{
    if (term.empty() || term == "(Intercept)" ||
        TermListContainsEquivalentModelTerm(candidates, term)) return;
    candidates.push_back(term);
}

void RemoveComparisonCandidateTerm(std::vector<std::string> &candidates,
                                   const std::string &term)
{
    candidates.erase(std::remove_if(candidates.begin(), candidates.end(),
        [&](const std::string &candidate) {
            return ModelTermShouldBeRemoved(candidate, term);
        }), candidates.end());
}

std::vector<std::string> ComparisonPresentationCandidates(
    const std::vector<std::string> &candidates,
    const std::vector<std::string> &includedTerms)
{
    std::vector<std::string> result;
    for (const std::string &term : candidates) AddComparisonCandidateTerm(result, term);
    for (const std::string &term : includedTerms) AddComparisonCandidateTerm(result, term);
    return result;
}

} // namespace

bool SetRegressionComparisonActiveModel(RegressionComparisonState &state, int modelIndex)
{
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size()) return false;
    const bool changed = state.activeModel != modelIndex;
    state.activeModel = modelIndex;
    return changed;
}

bool IncludeRegressionComparisonTerm(RegressionComparisonState &state,
                                     int modelIndex,
                                     const std::string &term,
                                     const std::vector<std::string> &availableVariables)
{
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size() ||
        term.empty() || term == "(Intercept)") return false;
    RegressionComparisonModel &model = state.models[static_cast<std::size_t>(modelIndex)];
    model.response = RegressionComparisonEffectiveResponse(state, model);
    const auto result = AddModelSpecificationTerm(model, availableVariables, term);
    if (!result.ok || !result.changed) return false;
    for (const std::string &candidate : model.terms) {
        AddRegressionTermRow(state, candidate);
        if (EquivalentModelTerm(candidate, term)) {
            AddComparisonCandidateTerm(model.candidateTerms, candidate);
        }
    }
    SetRegressionComparisonActiveModel(state, modelIndex);
    InvalidateRegressionComparisonModel(state, model);
    return true;
}

bool ExcludeRegressionComparisonTerm(RegressionComparisonState &state,
                                     int modelIndex,
                                     const std::string &term)
{
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size() ||
        term.empty() || term == "(Intercept)") return false;
    RegressionComparisonModel &model = state.models[static_cast<std::size_t>(modelIndex)];
    AddComparisonCandidateTerm(model.candidateTerms, term);
    const auto result = RemoveModelSpecificationTerm(model, term);
    if (!result.ok || !result.changed) return false;
    SetRegressionComparisonActiveModel(state, modelIndex);
    InvalidateRegressionComparisonModel(state, model);
    PruneRegressionComparisonRowsForCurrentTypes(state);
    return true;
}

bool ToggleRegressionComparisonTerm(RegressionComparisonState &state,
                                    int modelIndex,
                                    const std::string &term,
                                    const std::vector<std::string> &availableVariables)
{
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size()) return false;
    return ModelIncludesTerm(state.models[static_cast<std::size_t>(modelIndex)], term)
        ? ExcludeRegressionComparisonTerm(state, modelIndex, term)
        : IncludeRegressionComparisonTerm(state, modelIndex, term, availableVariables);
}

bool IncludeRegressionComparisonTermInAllModels(
    RegressionComparisonState &state,
    const std::string &term,
    const std::vector<std::string> &availableVariables)
{
    bool changed = false;
    const int active = state.activeModel;
    for (std::size_t index = 0; index < state.models.size(); ++index) {
        changed = IncludeRegressionComparisonTerm(
            state, static_cast<int>(index), term, availableVariables) || changed;
    }
    if (!state.models.empty()) {
        state.activeModel = std::clamp(active, 0, static_cast<int>(state.models.size()) - 1);
    }
    return changed;
}

bool ExcludeRegressionComparisonTermFromAllModels(RegressionComparisonState &state,
                                                  const std::string &term)
{
    bool changed = false;
    const int active = state.activeModel;
    for (std::size_t index = 0; index < state.models.size(); ++index) {
        changed = ExcludeRegressionComparisonTerm(state, static_cast<int>(index), term) || changed;
    }
    if (!state.models.empty()) {
        state.activeModel = std::clamp(active, 0, static_cast<int>(state.models.size()) - 1);
    }
    PruneRegressionComparisonRowsForCurrentTypes(state);
    return changed;
}

bool RemoveRegressionComparisonTermCompletely(RegressionComparisonState &state,
                                              const std::string &term)
{
    if (term.empty() || term == "(Intercept)") return false;
    bool changed = ExcludeRegressionComparisonTermFromAllModels(state, term);
    for (RegressionComparisonModel &model : state.models) {
        RemoveComparisonCandidateTerm(model.candidateTerms, term);
    }
    const std::size_t before = state.termRows.size();
    state.termRows.erase(std::remove_if(state.termRows.begin(), state.termRows.end(),
        [&](const std::string &row) {
            if (row == "(Intercept)") return false;
            std::string source = RegressionComparisonSourceTerm(state, row);
            if (source.empty()) source = ModelTermBaseForDisplayRow(row);
            return ModelTermShouldBeRemoved(source, term) || ModelTermShouldBeRemoved(row, term);
        }), state.termRows.end());
    changed = changed || state.termRows.size() != before;
    for (auto it = state.termTypes.begin(); it != state.termTypes.end();) {
        if (ModelTermShouldBeRemoved(it->first, term)) it = state.termTypes.erase(it);
        else ++it;
    }
    PruneRegressionComparisonRowsForCurrentTypes(state);
    return changed;
}

bool ReplaceRegressionComparisonTerm(RegressionComparisonState &state,
                                     int modelIndex,
                                     const std::string &term,
                                     const std::string &replacement,
                                     const std::vector<std::string> &availableVariables)
{
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size()) return false;
    auto &model = state.models[static_cast<std::size_t>(modelIndex)];
    const auto result = ReplaceModelSpecificationTerm(
        model, availableVariables, term, replacement);
    if (!result.ok || !result.changed) return false;
    RemoveComparisonCandidateTerm(model.candidateTerms, term);
    for (const std::string &candidate : model.terms) {
        if (EquivalentModelTerm(candidate, replacement)) {
            AddComparisonCandidateTerm(model.candidateTerms, candidate);
        }
    }
    InvalidateRegressionComparisonModel(state, model);
    SetRegressionComparisonActiveModel(state, modelIndex);
    RefreshRegressionTermRowsFromFits(state);
    PruneRegressionComparisonRowsForCurrentTypes(state);
    return true;
}

int AddRegressionComparisonModel(RegressionComparisonState &state,
                                 int sourceModelIndex,
                                 bool emptyModel)
{
    RegressionComparisonModel model;
    int source = sourceModelIndex;
    if (!state.models.empty()) {
        if (source < 0 || static_cast<std::size_t>(source) >= state.models.size()) {
            source = std::clamp(state.activeModel, 0, static_cast<int>(state.models.size()) - 1);
        }
    } else {
        source = -1;
    }
    model.id = NextRegressionModelId(state);
    model.label = source < 0 || emptyModel
        ? NextUntitledRegressionLabel(state)
        : UniqueRegressionCopyLabel(state, state.models[static_cast<std::size_t>(source)].label);
    model.response = source < 0
        ? state.response
        : (state.models[static_cast<std::size_t>(source)].response.empty()
            ? state.response : state.models[static_cast<std::size_t>(source)].response);
    model.scope = state.scope;
    model.dataScope = state.dataScope;
    model.dataScopeCaptured = state.dataScopeCaptured;
    model.termTypes = state.termTypes;
    if (source >= 0 && !emptyModel) {
        static_cast<ModelSpecification &>(model) =
            static_cast<const ModelSpecification &>(state.models[static_cast<std::size_t>(source)]);
        model.candidateTerms = state.models[static_cast<std::size_t>(source)].candidateTerms;
    }
    model.modelVersion = 1;
    model.isStale = true;
    state.models.push_back(std::move(model));
    state.activeModel = static_cast<int>(state.models.size()) - 1;
    state.rFitPending = false;
    state.lastRFitSignature.clear();
    return state.activeModel;
}

RegressionComparisonState CreateRegressionComparisonFromSingleModel(
    const std::string &comparisonId,
    const GroupModelState &source,
    const GLMFitSummary *currentFit,
    const PlotModel *seed,
    const std::string &modelLabel)
{
    RegressionComparisonState state;
    state.id = comparisonId;
    state.group = source.group;
    state.response = source.response;
    state.scope = source.scope;
    state.dataScope = source.dataScope;
    state.dataScopeCaptured = source.dataScopeCaptured;
    state.autoRefit = true;
    state.precomputed = source.precomputed;
    state.multipleImputation = source.multipleImputation;
    state.imputationCount = source.imputationCount;
    state.title = "Compare Linear Models";
    state.note = source.note;
    state.termTypes = EffectiveModelSpecificationTermTypes(source);
    if (seed) {
        state.seed = *seed;
        state.hasSeed = true;
    }

    RegressionComparisonModel model;
    static_cast<ModelSpecification &>(model) =
        static_cast<const ModelSpecification &>(source);
    model.familyKind = ModelFamilyKind::Linear;
    model.id = comparisonId + ":model:1";
    model.label = modelLabel.empty() ? "Model 1" : modelLabel;
    model.candidateTerms = source.terms;
    model.modelVersion = source.modelVersion;

    const std::string expectedSignature = LinearGLMFitSignature(
        source.response, source.terms, state.termTypes, source.scope,
        source.centeredPredictors, source.factorReferenceLevels);
    bool reusable = currentFit && currentFit->ok && !source.isStale &&
        !source.rFitPending && source.fitVersion >= source.modelVersion &&
        source.lastRFitSignature == expectedSignature &&
        LinearGLMFitMatchesTermTypes(*currentFit, source.terms, state.termTypes);
    if (reusable) {
        for (const std::string &variable : source.centeredPredictors) {
            const auto center = currentFit->predictorCenters.find(variable);
            if (center == currentFit->predictorCenters.end() ||
                !std::isfinite(center->second)) {
                reusable = false;
                break;
            }
        }
    }
    if (reusable) {
        for (const auto &entry : currentFit->predictorCenters) {
            if (source.centeredPredictors.count(entry.first) == 0) {
                reusable = false;
                break;
            }
        }
    }
    if (reusable) {
        for (const auto &entry : source.factorReferenceLevels) {
            const auto coding = std::find_if(
                currentFit->factorCodings.begin(), currentFit->factorCodings.end(),
                [&](const GLMFactorCoding &candidate) {
                    return candidate.variable == entry.first;
                });
            if (coding == currentFit->factorCodings.end() ||
                coding->referenceLevel != entry.second) {
                reusable = false;
                break;
            }
        }
    }
    if (reusable) {
        model.fit = *currentFit;
        model.fitVersion = source.fitVersion;
        model.isStale = false;
        model.fitState = RegressionComparisonFitState::Valid;
    } else {
        model.fitVersion = 0;
        model.isStale = true;
        model.fitState = RegressionComparisonFitState::NotFitted;
    }
    state.models.push_back(std::move(model));
    state.activeModel = 0;
    RefreshRegressionTermRowsFromFits(state);
    return state;
}

bool DeleteRegressionComparisonModel(RegressionComparisonState &state, int modelIndex)
{
    if (state.models.size() <= 1 || modelIndex < 0 ||
        static_cast<std::size_t>(modelIndex) >= state.models.size()) return false;
    const int previousActiveModel = state.activeModel;
    state.models.erase(state.models.begin() + modelIndex);
    if (previousActiveModel > modelIndex) {
        state.activeModel = previousActiveModel - 1;
    } else if (previousActiveModel == modelIndex) {
        state.activeModel = std::min(modelIndex, static_cast<int>(state.models.size()) - 1);
    } else {
        state.activeModel = previousActiveModel;
    }
    state.activeModel = std::clamp(state.activeModel, 0, static_cast<int>(state.models.size()) - 1);
    RefreshRegressionTermRowsFromFits(state);
    PruneRegressionComparisonRowsForCurrentTypes(state);
    state.rFitPending = false;
    state.lastRFitSignature.clear();
    return true;
}

RegressionComparisonModel *RegressionComparisonModelById(RegressionComparisonState &state,
                                                         const std::string &modelId)
{
    auto found = std::find_if(state.models.begin(), state.models.end(),
        [&](const RegressionComparisonModel &model) { return model.id == modelId; });
    return found == state.models.end() ? nullptr : &*found;
}

const RegressionComparisonModel *RegressionComparisonModelById(
    const RegressionComparisonState &state,
    const std::string &modelId)
{
    auto found = std::find_if(state.models.begin(), state.models.end(),
        [&](const RegressionComparisonModel &model) { return model.id == modelId; });
    return found == state.models.end() ? nullptr : &*found;
}

std::vector<std::string> RegressionComparisonModelIds(const RegressionComparisonState &state)
{
    std::vector<std::string> ids;
    ids.reserve(state.models.size());
    for (const RegressionComparisonModel &model : state.models) ids.push_back(model.id);
    return ids;
}

std::vector<std::string> RegressionComparisonModelLabels(const RegressionComparisonState &state)
{
    std::vector<std::string> labels;
    labels.reserve(state.models.size());
    for (const RegressionComparisonModel &model : state.models) labels.push_back(model.label);
    return labels;
}

std::string NextRegressionModelId(const RegressionComparisonState &state)
{
    return NextComparisonModelId(state.id, RegressionComparisonModelIds(state));
}

std::string NextUntitledRegressionLabel(const RegressionComparisonState &state)
{
    return NextUntitledComparisonLabel(RegressionComparisonModelLabels(state));
}

std::string UniqueRegressionCopyLabel(const RegressionComparisonState &state, const std::string &label)
{
    return UniqueRegressionComparisonCopyLabel(RegressionComparisonModelLabels(state), label);
}

bool HasGeneralizedComparisonTerm(const GeneralizedComparisonState &state, const std::string &term)
{
    for (const GeneralizedComparisonModel &model : state.models) {
        if (ModelSpecificationIncludesTerm(model, term) ||
            TermListContainsEquivalentModelTerm(model.candidateTerms, term)) return true;
    }
    return term == "(Intercept)";
}

bool GeneralizedModelIncludesTerm(const GeneralizedComparisonModel &model, const std::string &term)
{
    return ModelSpecificationIncludesTerm(model, term);
}

void AddGeneralizedComparisonTermRow(GeneralizedComparisonState &state, const std::string &term)
{
    // Compatibility shim for platform code compiled against the old helper.
    // Term rows are no longer an independently editable registry; callers
    // must mutate a model's ModelSpecification and then derive presentation.
    (void)term;
    RefreshGeneralizedTermRowsFromFits(state);
}

namespace {

bool GeneralizedComparisonProvenanceContains(
    const std::map<std::string, std::vector<std::string>> &provenance,
    const std::string &candidate)
{
    for (const auto &entry : provenance) {
        if (TermListContainsEquivalentModelTerm(entry.second, candidate)) return true;
    }
    return false;
}

bool GeneralizedComparisonInteractionRequiresTerm(const std::string &interaction,
                                                  const std::string &candidate)
{
    if (!IsInteractionTerm(interaction) ||
        EquivalentModelTerm(interaction, candidate)) return false;
    const std::vector<std::string> interactionVariables =
        UniqueBaseVariablesForTerm(interaction);
    const std::vector<std::string> candidateVariables =
        UniqueBaseVariablesForTerm(candidate);
    if (candidateVariables.empty()) return false;
    return std::all_of(candidateVariables.begin(), candidateVariables.end(),
                       [&](const std::string &variable) {
                           return std::find(interactionVariables.begin(),
                                            interactionVariables.end(),
                                            variable) != interactionVariables.end();
                       });
}

void PruneGeneralizedComparisonInteractionProvenance(
    GeneralizedComparisonModel &model)
{
    for (auto it = model.interactionImpliedTerms.begin();
         it != model.interactionImpliedTerms.end();) {
        if (!TermListContainsEquivalentModelTerm(model.terms, it->first)) {
            it = model.interactionImpliedTerms.erase(it);
        } else {
            ++it;
        }
    }
}

} // namespace

std::map<std::string, std::string> GeneralizedComparisonModelTermTypes(
    const GeneralizedComparisonState &state,
    const GeneralizedComparisonModel &model)
{
    // `state.termTypes` is retained only as dataset-derived UI/default
    // metadata for legacy callers.  It is deliberately not part of a model
    // column's canonical semantics: otherwise editing/rebuilding one column
    // can silently change every other column that happens to fall back to the
    // shared map.  Request boundaries materialize dataset defaults into each
    // model with SynchronizeModelSpecificationTermTypes().
    (void)state;
    return EffectiveModelSpecificationTermTypes(model);
}

std::string GeneralizedComparisonModelTermType(const GeneralizedComparisonState &state,
                                               int modelIndex,
                                               const std::string &term)
{
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size()) {
        return ModelTermDisplayType(&state.seed, term, state.termTypes);
    }
    return ModelTermDisplayType(
        &state.seed, term,
        GeneralizedComparisonModelTermTypes(
            state, state.models[static_cast<std::size_t>(modelIndex)]));
}

ModelSpecification EffectiveGeneralizedComparisonModelSpecification(
    const GeneralizedComparisonState &state,
    const GeneralizedComparisonModel &model)
{
    ModelSpecification specification = model;
    specification.familyKind = ModelFamilyKind::GeneralizedLinear;
    if (specification.response.empty()) specification.response = state.response;
    // Every compared model uses the comparison's immutable execution snapshot.
    specification.scope = state.scope;
    specification.dataScope = state.dataScope;
    specification.dataScopeCaptured = state.dataScopeCaptured;
    const auto comparisonTypes = GeneralizedComparisonModelTermTypes(state, model);
    specification.termTypes.clear();
    for (const std::string &term : specification.terms) {
        for (const std::string &variable : UniqueBaseVariablesForTerm(term)) {
            const auto found = comparisonTypes.find(variable);
            if (found != comparisonTypes.end()) specification.termTypes[variable] = found->second;
        }
    }
    PruneModelSpecificationState(specification);
    return specification;
}

bool SynchronizeGeneralizedComparisonDatasetIdentity(
    GeneralizedComparisonState &state,
    const DataFrameModel &dataframe)
{
    const bool multiple = dataframe.datasetType == "multiple_imputation";
    const int count = multiple ? std::max(1, dataframe.imputationCount) : 0;
    const bool changed = state.group != dataframe.group ||
        state.datasetType != dataframe.datasetType ||
        state.imputationSetId != dataframe.imputationId ||
        state.sourceDatasetId != dataframe.sourceDatasetId ||
        state.multipleImputation != multiple || state.imputationCount != count;
    state.group = dataframe.group;
    state.datasetType = dataframe.datasetType;
    state.imputationSetId = dataframe.imputationId;
    state.sourceDatasetId = dataframe.sourceDatasetId;
    state.multipleImputation = multiple;
    state.imputationCount = count;
    return changed;
}

RegressionComparisonFitState GeneralizedComparisonResolvedFitState(
    const GeneralizedComparisonState &state,
    const GeneralizedComparisonModel &model)
{
    if (state.rFitPending && model.isStale) return RegressionComparisonFitState::Pending;
    if (model.isStale) return RegressionComparisonFitState::NotFitted;
    const std::string currentFingerprint =
        GeneralizedComparisonModelSpecificationFingerprint(state, model);
    if (model.fitVersion != model.modelVersion ||
        model.fitSpecificationRevision != model.modelVersion ||
        model.fitSpecificationFingerprint != currentFingerprint ||
        model.fit.lastRFitSignature != currentFingerprint ||
        model.fit.multipleImputation != state.multipleImputation ||
        model.fit.imputationCount != state.imputationCount ||
        model.fit.datasetType != state.datasetType ||
        model.fit.imputationSetId != state.imputationSetId ||
        model.fit.sourceDatasetId != state.sourceDatasetId) {
        return RegressionComparisonFitState::NotFitted;
    }
    if (model.fit.ok) return RegressionComparisonFitState::Valid;
    if (model.fitState == RegressionComparisonFitState::Error) {
        return RegressionComparisonFitState::Error;
    }
    return model.fitState;
}

void InvalidateGeneralizedComparisonModel(GeneralizedComparisonState &state,
                                          GeneralizedComparisonModel &model)
{
    ++model.modelVersion;
    model.isStale = true;
    model.fitState = RegressionComparisonFitState::NotFitted;
    model.fit = GeneralizedGLMState();
    model.fit.id = model.id;
    model.fit.group = state.group;
    model.fitVersion = 0;
    model.fitSpecificationRevision = 0;
    model.fitSpecificationFingerprint.clear();
    model.requestedSpecificationRevision = 0;
    model.requestedSpecificationFingerprint.clear();
    model.comparisonOk = false;
    model.comparisonCalculatedInR = false;
    model.comparisonDf = 0;
    model.comparisonDf2 = NAN;
    model.comparisonDelta = NAN;
    model.comparisonStatistic = NAN;
    model.comparisonP = NAN;
    model.comparisonMethod.clear();
    state.rFitPending = false;
    state.lastRFitSignature.clear();
}

bool SetGeneralizedComparisonActiveModel(GeneralizedComparisonState &state, int modelIndex)
{
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size()) return false;
    const bool changed = state.activeModel != modelIndex;
    state.activeModel = modelIndex;
    return changed;
}

bool IncludeGeneralizedComparisonTerm(GeneralizedComparisonState &state,
                                      int modelIndex,
                                      const std::string &term,
                                      const std::vector<std::string> &availableVariables)
{
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size() ||
        term.empty() || term == "(Intercept)") return false;
    auto &model = state.models[static_cast<std::size_t>(modelIndex)];
    if (model.response.empty()) model.response = state.response;
    const std::vector<std::string> termsBefore = model.terms;
    const auto result = AddModelSpecificationTerm(model, availableVariables, term);
    if (!result.ok || !result.changed) return false;
    for (const std::string &candidate : model.terms) {
        if (EquivalentModelTerm(candidate, term)) {
            AddComparisonCandidateTerm(model.candidateTerms, candidate);
        }
    }
    if (IsInteractionTerm(term)) {
        std::string interactionKey = term;
        for (const std::string &candidate : model.terms) {
            if (EquivalentModelTerm(candidate, term)) {
                interactionKey = candidate;
                break;
            }
        }
        std::vector<std::string> implied;
        for (const std::string &candidate : model.terms) {
            if (!EquivalentModelTerm(candidate, interactionKey) &&
                !TermListContainsEquivalentModelTerm(termsBefore, candidate)) {
                implied.push_back(candidate);
            }
        }
        for (auto it = model.interactionImpliedTerms.begin();
             it != model.interactionImpliedTerms.end();) {
            if (EquivalentModelTerm(it->first, interactionKey)) {
                it = model.interactionImpliedTerms.erase(it);
            } else {
                ++it;
            }
        }
        model.interactionImpliedTerms[interactionKey] = std::move(implied);
    }
    for (const std::string &candidate : model.terms) AddGeneralizedComparisonTermRow(state, candidate);
    SetGeneralizedComparisonActiveModel(state, modelIndex);
    InvalidateGeneralizedComparisonModel(state, model);
    RefreshGeneralizedTermRowsFromFits(state);
    return true;
}

bool ExcludeGeneralizedComparisonTerm(GeneralizedComparisonState &state,
                                      int modelIndex,
                                      const std::string &term)
{
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size() ||
        term.empty() || term == "(Intercept)") return false;
    auto &model = state.models[static_cast<std::size_t>(modelIndex)];
    AddComparisonCandidateTerm(model.candidateTerms, term);
    const auto result = RemoveModelSpecificationTerm(model, term);
    if (!result.ok || !result.changed) return false;
    std::vector<std::string> impliedByRemovedInteractions;
    for (auto it = model.interactionImpliedTerms.begin();
         it != model.interactionImpliedTerms.end();) {
        if (!TermListContainsEquivalentModelTerm(model.terms, it->first)) {
            for (const std::string &candidate : it->second) {
                if (!TermListContainsEquivalentModelTerm(
                        impliedByRemovedInteractions, candidate)) {
                    impliedByRemovedInteractions.push_back(candidate);
                }
            }
            it = model.interactionImpliedTerms.erase(it);
        } else {
            ++it;
        }
    }
    for (const std::string &candidate : impliedByRemovedInteractions) {
        if (!TermListContainsEquivalentModelTerm(model.terms, candidate) ||
            GeneralizedComparisonProvenanceContains(
                model.interactionImpliedTerms, candidate)) {
            continue;
        }
        const bool requiredByRemainingInteraction =
            std::any_of(model.terms.begin(), model.terms.end(),
                        [&](const std::string &remaining) {
                            return GeneralizedComparisonInteractionRequiresTerm(
                                remaining, candidate);
                        });
        if (!requiredByRemainingInteraction) {
            RemoveModelSpecificationTerm(model, candidate);
        }
    }
    PruneGeneralizedComparisonInteractionProvenance(model);
    SetGeneralizedComparisonActiveModel(state, modelIndex);
    InvalidateGeneralizedComparisonModel(state, model);
    RefreshGeneralizedTermRowsFromFits(state);
    return true;
}

bool ToggleGeneralizedComparisonTerm(GeneralizedComparisonState &state,
                                     int modelIndex,
                                     const std::string &term,
                                     const std::vector<std::string> &availableVariables)
{
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size()) return false;
    return GeneralizedModelIncludesTerm(state.models[static_cast<std::size_t>(modelIndex)], term)
        ? ExcludeGeneralizedComparisonTerm(state, modelIndex, term)
        : IncludeGeneralizedComparisonTerm(state, modelIndex, term, availableVariables);
}

bool IncludeGeneralizedComparisonTermInAllModels(
    GeneralizedComparisonState &state,
    const std::string &term,
    const std::vector<std::string> &availableVariables)
{
    bool changed = false;
    const int active = state.activeModel;
    for (std::size_t i = 0; i < state.models.size(); ++i) {
        changed = IncludeGeneralizedComparisonTerm(
            state, static_cast<int>(i), term, availableVariables) || changed;
    }
    if (!state.models.empty()) state.activeModel = std::clamp(active, 0, static_cast<int>(state.models.size()) - 1);
    return changed;
}

bool ExcludeGeneralizedComparisonTermFromAllModels(GeneralizedComparisonState &state,
                                                   const std::string &term)
{
    bool changed = false;
    const int active = state.activeModel;
    for (std::size_t i = 0; i < state.models.size(); ++i) {
        changed = ExcludeGeneralizedComparisonTerm(state, static_cast<int>(i), term) || changed;
    }
    if (!state.models.empty()) state.activeModel = std::clamp(active, 0, static_cast<int>(state.models.size()) - 1);
    return changed;
}

bool RemoveGeneralizedComparisonTermCompletely(GeneralizedComparisonState &state,
                                               const std::string &term)
{
    if (term.empty() || term == "(Intercept)") return false;
    bool changed = ExcludeGeneralizedComparisonTermFromAllModels(state, term);
    for (GeneralizedComparisonModel &model : state.models) {
        const std::size_t before = model.candidateTerms.size();
        RemoveComparisonCandidateTerm(model.candidateTerms, term);
        changed = changed || model.candidateTerms.size() != before;
    }
    for (auto it = state.termTypes.begin(); it != state.termTypes.end();) {
        if (ModelTermShouldBeRemoved(it->first, term)) it = state.termTypes.erase(it);
        else ++it;
    }
    RefreshGeneralizedTermRowsFromFits(state);
    return changed;
}

int AddGeneralizedComparisonModel(GeneralizedComparisonState &state,
                                  int sourceModelIndex,
                                  bool emptyModel)
{
    GeneralizedComparisonModel model;
    int source = sourceModelIndex;
    if (!state.models.empty()) {
        if (source < 0 || static_cast<std::size_t>(source) >= state.models.size()) {
            source = std::clamp(state.activeModel, 0, static_cast<int>(state.models.size()) - 1);
        }
    } else source = -1;
    model.id = NextGeneralizedComparisonModelId(state);
    model.label = source < 0 || emptyModel
        ? NextUntitledGeneralizedComparisonLabel(state)
        : UniqueGeneralizedComparisonCopyLabelForState(
            state, state.models[static_cast<std::size_t>(source)].label);
    if (source >= 0 && !emptyModel) {
        static_cast<ModelSpecification &>(model) =
            static_cast<const ModelSpecification &>(state.models[static_cast<std::size_t>(source)]);
        model.family = state.models[static_cast<std::size_t>(source)].family;
        model.link = state.models[static_cast<std::size_t>(source)].link;
        model.responseBoundsConfigured =
            state.models[static_cast<std::size_t>(source)].responseBoundsConfigured;
        model.responseLower = state.models[static_cast<std::size_t>(source)].responseLower;
        model.responseUpper = state.models[static_cast<std::size_t>(source)].responseUpper;
        model.countDistribution = state.models[static_cast<std::size_t>(source)].countDistribution;
        model.exposure = state.models[static_cast<std::size_t>(source)].exposure;
        model.offsetVariable = state.models[static_cast<std::size_t>(source)].offsetVariable;
        model.trialsVariable = state.models[static_cast<std::size_t>(source)].trialsVariable;
        model.trialsConstant = state.models[static_cast<std::size_t>(source)].trialsConstant;
        model.interactionImpliedTerms =
            state.models[static_cast<std::size_t>(source)].interactionImpliedTerms;
        model.candidateTerms =
            state.models[static_cast<std::size_t>(source)].candidateTerms;
        model.capabilities = state.models[static_cast<std::size_t>(source)].capabilities;
    } else {
        model.response = state.response;
        model.scope = state.scope;
        model.dataScope = state.dataScope;
        model.dataScopeCaptured = state.dataScopeCaptured;
        model.termTypes = state.termTypes;
        if (source >= 0) {
            const auto &sourceModel = state.models[static_cast<std::size_t>(source)];
            model.responseBoundsConfigured = sourceModel.responseBoundsConfigured;
            model.responseLower = sourceModel.responseLower;
            model.responseUpper = sourceModel.responseUpper;
        }
        if (source >= 0 && state.countComparison) {
            const auto &sourceModel = state.models[static_cast<std::size_t>(source)];
            model.family = sourceModel.family;
            model.link = sourceModel.link;
            model.countDistribution = sourceModel.countDistribution;
            model.exposure = sourceModel.exposure;
            model.offsetVariable = sourceModel.offsetVariable;
            model.trialsVariable = sourceModel.trialsVariable;
            model.trialsConstant = sourceModel.trialsConstant;
            model.capabilities = sourceModel.capabilities;
            model.scope = sourceModel.scope;
            model.dataScope = sourceModel.dataScope;
            model.dataScopeCaptured = sourceModel.dataScopeCaptured;
        } else {
            model.family = state.family;
            model.link = state.link;
            model.countDistribution = CountDistribution::Poisson;
        }
    }
    model.familyKind = ModelFamilyKind::GeneralizedLinear;
    model.modelVersion = 1;
    model.isStale = true;
    state.models.push_back(std::move(model));
    state.activeModel = static_cast<int>(state.models.size()) - 1;
    state.rFitPending = false;
    state.lastRFitSignature.clear();
    return state.activeModel;
}

GeneralizedComparisonState CreateGeneralizedComparisonFromSingleModel(
    const std::string &comparisonId,
    const GeneralizedGLMState &source,
    const std::string &modelLabel)
{
    GeneralizedComparisonState state;
    state.id = comparisonId;
    state.group = source.group;
    state.response = source.response;
    state.family = source.family;
    state.link = source.link;
    state.scope = source.scope;
    state.dataScope = source.dataScope;
    state.dataScopeCaptured = source.dataScopeCaptured;
    state.autoRefit = true;
    state.binaryComparison = source.binaryRegression;
    state.countComparison = source.countRegression;
    state.modelType = source.modelType;
    state.multipleImputation = source.multipleImputation;
    state.imputationCount = source.imputationCount;
    state.datasetType = source.datasetType;
    state.imputationSetId = source.imputationSetId;
    state.sourceDatasetId = source.sourceDatasetId;
    state.binaryLink = source.binaryLink;
    state.responseCoding = source.responseCoding;
    state.termTypes = EffectiveModelSpecificationTermTypes(source);
    state.seed = source.seed;
    state.hasSeed = source.hasSeed;

    GeneralizedComparisonModel model;
    static_cast<ModelSpecification &>(model) =
        static_cast<const ModelSpecification &>(source);
    model.familyKind = ModelFamilyKind::GeneralizedLinear;
    model.id = comparisonId + ":model:1";
    model.label = modelLabel.empty() ? "Model 1" : modelLabel;
    model.candidateTerms = source.terms;
    model.family = source.family;
    model.link = source.link;
    model.responseBoundsConfigured = source.responseBoundsConfigured;
    model.responseLower = source.responseLower;
    model.responseUpper = source.responseUpper;
    model.countDistribution = source.countDistribution;
    model.exposure = source.exposure;
    model.offsetVariable = source.offsetVariable;
    model.trialsVariable = source.trialsVariable;
    model.trialsConstant = source.trialsConstant;
    model.capabilities.hasLikelihood = source.likelihoodAvailable;
    model.capabilities.hasAIC = source.likelihoodAvailable;
    model.capabilities.supportsNestedLRComparison = source.likelihoodRatioAvailable;
    model.modelVersion = source.modelVersion;

    const std::string copiedFingerprint =
        GeneralizedComparisonModelSpecificationFingerprint(state, model);
    std::string sourceComparisonFingerprint = GeneralizedGLMFitSignature(source);
    if (source.multipleImputation) {
        sourceComparisonFingerprint += "|dataset-kind=" + source.datasetType +
            "|imputation-set=" + source.imputationSetId +
            "|source-dataset=" + source.sourceDatasetId +
            "|imputations=" + std::to_string(std::max(0, source.imputationCount));
    }
    model.requestedSpecificationRevision = model.modelVersion;
    model.requestedSpecificationFingerprint = copiedFingerprint;

    const bool fittedRowsCoverSpecification = std::all_of(
        source.terms.begin(), source.terms.end(), [&](const std::string &term) {
            return GeneralizedRowForTerm(source.rows, term) != nullptr;
        });
    const bool reusable = source.ok && !source.rFitPending &&
        source.fitVersion >= source.modelVersion &&
        source.lastRFitSignature == GeneralizedGLMFitSignature(source) &&
        copiedFingerprint == sourceComparisonFingerprint &&
        fittedRowsCoverSpecification;
    if (reusable) {
        model.fit = source;
        model.fitVersion = source.fitVersion;
        model.fitSpecificationRevision = source.fitVersion;
        model.fitSpecificationFingerprint = copiedFingerprint;
        model.fit.lastRFitSignature = copiedFingerprint;
        model.isStale = false;
        model.fitState = RegressionComparisonFitState::Valid;
        state.commonRows = source.rowsUsed;
    } else {
        model.fitVersion = 0;
        model.isStale = true;
        model.fitState = RegressionComparisonFitState::NotFitted;
    }
    state.models.push_back(std::move(model));
    state.activeModel = 0;
    RefreshGeneralizedTermRowsFromFits(state);
    return state;
}

GeneralizedGLMState CreateSingleModelFromGeneralizedComparison(
    const GeneralizedComparisonState &state,
    int modelIndex,
    const std::string &singleId)
{
    GeneralizedGLMState single;
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size()) {
        single.status = "Comparison model is not available.";
        return single;
    }

    const GeneralizedComparisonModel &model =
        state.models[static_cast<std::size_t>(modelIndex)];
    single = model.fit;
    static_cast<ModelSpecification &>(single) =
        EffectiveGeneralizedComparisonModelSpecification(state, model);
    single.id = singleId.empty()
        ? "gcomp_single:" + state.id + ":" + model.id
        : singleId;
    single.group = state.group;
    single.family = model.family.empty() ? state.family : model.family;
    single.link = model.link.empty() ? state.link : model.link;
    single.responseBoundsConfigured = model.responseBoundsConfigured;
    single.responseLower = model.responseLower;
    single.responseUpper = model.responseUpper;
    single.binaryRegression = state.binaryComparison;
    single.binaryLink = state.binaryLink;
    single.countRegression = state.countComparison;
    single.countDistribution = model.countDistribution;
    single.exposure = model.exposure;
    single.offsetVariable = model.offsetVariable;
    single.trialsVariable = model.trialsVariable;
    single.trialsConstant = model.trialsConstant;
    single.likelihoodAvailable = model.capabilities.hasLikelihood;
    single.likelihoodRatioAvailable = model.capabilities.supportsNestedLRComparison;
    single.responseCoding = state.responseCoding;
    single.responseCodingExplicit = state.binaryComparison && state.responseCoding.ok;
    single.multipleImputation = state.multipleImputation;
    single.imputationCount = state.imputationCount;
    single.datasetType = state.datasetType;
    single.imputationSetId = state.imputationSetId;
    single.sourceDatasetId = state.sourceDatasetId;
    single.autoRefit = state.autoRefit;
    single.title = state.modelType == StatisticalModelType::LegacyGeneralized
        ? (model.label.empty() ? "Generalized Linear Model" : model.label)
        : StatisticalModelTypeLabel(state.modelType);
    single.seed = state.seed;
    single.hasSeed = state.hasSeed;
    single.diagnosticOptions = model.diagnosticOptions;
    single.modelVersion = model.modelVersion;
    single.fitVersion = model.fitVersion;
    single.rFitPending = false;

    const bool fittedRowsCoverSpecification = std::all_of(
        single.terms.begin(), single.terms.end(), [&](const std::string &term) {
            return GeneralizedRowForTerm(single.rows, term) != nullptr;
        });
    const bool reusable = model.fitState == RegressionComparisonFitState::Valid &&
        !model.isStale && single.ok && single.fitVersion >= single.modelVersion &&
        fittedRowsCoverSpecification;
    if (reusable) {
        single.lastRFitSignature = GeneralizedGLMFitSignature(single);
    } else {
        single.ok = false;
        single.fitVersion = 0;
        single.lastRFitSignature.clear();
        single.status = "Not fitted.";
    }
    return single;
}

bool DeleteGeneralizedComparisonModel(GeneralizedComparisonState &state, int modelIndex)
{
    if (state.models.size() <= 1 || modelIndex < 0 ||
        static_cast<std::size_t>(modelIndex) >= state.models.size()) return false;
    const int previousActive = state.activeModel;
    state.models.erase(state.models.begin() + modelIndex);
    if (previousActive > modelIndex) state.activeModel = previousActive - 1;
    else if (previousActive == modelIndex) state.activeModel = std::min(modelIndex, static_cast<int>(state.models.size()) - 1);
    state.activeModel = std::clamp(state.activeModel, 0, static_cast<int>(state.models.size()) - 1);
    for (auto &model : state.models) {
        model.comparisonOk = false;
        model.comparisonCalculatedInR = false;
    }
    state.rFitPending = false;
    state.lastRFitSignature.clear();
    RefreshGeneralizedTermRowsFromFits(state);
    return true;
}

bool ApplyGeneralizedComparisonTermType(GeneralizedComparisonState &state,
                                        int modelIndex,
                                        const std::string &term,
                                        const std::string &type,
                                        const ModelPredictorMetadata &metadata)
{
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size()) return false;
    auto &model = state.models[static_cast<std::size_t>(modelIndex)];
    const auto result = SetModelSpecificationTermType(model, term, type, metadata);
    if (!result.ok || !result.changed) return false;
    InvalidateGeneralizedComparisonModel(state, model);
    RefreshGeneralizedTermRowsFromFits(state);
    return true;
}

bool ApplyGeneralizedComparisonReferenceLevel(GeneralizedComparisonState &state,
                                              int modelIndex,
                                              const std::string &term,
                                              const std::string &referenceLevel,
                                              const std::vector<std::string> &availableLevels)
{
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size()) return false;
    auto &model = state.models[static_cast<std::size_t>(modelIndex)];
    const auto result = SetModelSpecificationReferenceLevel(model, term, referenceLevel, availableLevels);
    if (!result.ok || !result.changed) return false;
    InvalidateGeneralizedComparisonModel(state, model);
    RefreshGeneralizedTermRowsFromFits(state);
    return true;
}

bool ToggleGeneralizedComparisonPredictorCentering(GeneralizedComparisonState &state,
                                                   int modelIndex,
                                                   const std::string &term)
{
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size()) return false;
    auto &model = state.models[static_cast<std::size_t>(modelIndex)];
    const std::string variable = BaseVariableForTermComponent(term);
    const bool centered = model.centeredPredictors.find(variable) == model.centeredPredictors.end();
    const auto result = SetModelSpecificationPredictorCentered(model, variable, centered);
    if (!result.ok || !result.changed) return false;
    InvalidateGeneralizedComparisonModel(state, model);
    RefreshGeneralizedTermRowsFromFits(state);
    return true;
}

bool ReplaceGeneralizedComparisonTerm(GeneralizedComparisonState &state,
                                      int modelIndex,
                                      const std::string &term,
                                      const std::string &replacement,
                                      const std::vector<std::string> &availableVariables)
{
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size()) return false;
    auto &model = state.models[static_cast<std::size_t>(modelIndex)];
    const auto result = ReplaceModelSpecificationTerm(
        model, availableVariables, term, replacement);
    if (!result.ok || !result.changed) return false;
    RemoveComparisonCandidateTerm(model.candidateTerms, term);
    for (const std::string &candidate : model.terms) {
        if (EquivalentModelTerm(candidate, replacement)) {
            AddComparisonCandidateTerm(model.candidateTerms, candidate);
        }
    }
    // Renaming can rewrite every component of an interaction. Treat the
    // rebuilt terms as explicit rather than retaining provenance under stale
    // term names.
    model.interactionImpliedTerms.clear();
    InvalidateGeneralizedComparisonModel(state, model);
    SetGeneralizedComparisonActiveModel(state, modelIndex);
    RefreshGeneralizedTermRowsFromFits(state);
    return true;
}

bool SetGeneralizedComparisonFamily(GeneralizedComparisonState &state,
                                    const std::string &family)
{
    if (state.binaryComparison || state.countComparison ||
        !IsValidGeneralizedFamily(family)) return false;
    if (state.modelType != StatisticalModelType::LegacyGeneralized &&
        !DistributionIsAvailableForModel(state.modelType, family)) return false;
    const std::string link = state.modelType == StatisticalModelType::LegacyGeneralized
        ? DefaultGeneralizedLink(family)
        : DefaultLinkForModelDistribution(state.modelType, family);
    if (link.empty()) return false;
    const bool changed = state.family != family || state.link != link ||
        std::any_of(state.models.begin(), state.models.end(),
                    [&](const GeneralizedComparisonModel &model) {
                        return model.family != family || model.link != link;
                    });
    if (!changed) return false;
    state.family = family;
    state.link = link;
    for (GeneralizedComparisonModel &model : state.models) {
        model.family = family;
        model.link = link;
        if (state.modelType == StatisticalModelType::Proportion &&
            GeneralizedFamilyRequiresResponseBounds(family) &&
            !model.responseBoundsConfigured) {
            model.responseBoundsConfigured = true;
            model.responseLower = 0.0;
            model.responseUpper = 1.0;
        }
        InvalidateGeneralizedComparisonModel(state, model);
    }
    return true;
}

bool SetGeneralizedComparisonLink(GeneralizedComparisonState &state,
                                  const std::string &link)
{
    const auto supportedLinks = LinksForModelDistribution(state.modelType, state.family);
    const bool validLink = state.modelType == StatisticalModelType::LegacyGeneralized
        ? IsValidGeneralizedLink(state.family, link)
        : std::find(supportedLinks.begin(), supportedLinks.end(), link) !=
              supportedLinks.end();
    if (state.countComparison || !validLink) return false;
    BinaryLink binaryLink = state.binaryLink;
    if (state.binaryComparison && !ParseBinaryLink(link, binaryLink)) return false;
    const bool changed = state.link != link ||
        (state.binaryComparison && state.binaryLink != binaryLink) ||
        std::any_of(state.models.begin(), state.models.end(),
                    [&](const GeneralizedComparisonModel &model) {
                        return model.family != state.family || model.link != link;
                    });
    if (!changed) return false;
    state.link = link;
    if (state.binaryComparison) state.binaryLink = binaryLink;
    for (GeneralizedComparisonModel &model : state.models) {
        model.family = state.family;
        model.link = link;
        InvalidateGeneralizedComparisonModel(state, model);
    }
    return true;
}

bool SetGeneralizedComparisonCountDistribution(GeneralizedComparisonState &state,
                                               int modelIndex,
                                               CountDistribution distribution)
{
    if (!state.countComparison || modelIndex < 0 ||
        static_cast<std::size_t>(modelIndex) >= state.models.size()) return false;
    auto &model = state.models[static_cast<std::size_t>(modelIndex)];
    if (model.countDistribution == distribution) return false;
    model.countDistribution = distribution;
    const bool bounded = CountDistributionUsesTrials(distribution);
    model.family = distribution == CountDistribution::QuasiPoisson
        ? "quasipoisson" : bounded ? "binomial" : "poisson";
    model.link = bounded ? "logit" : "log";
    if (bounded) model.exposure.clear();
    if (distribution == CountDistribution::HurdleBetaBinomialCeiling ||
        distribution == CountDistribution::PerfectScore)
        model.offsetVariable.clear();
    const bool quasi = distribution == CountDistribution::QuasiPoisson;
    model.capabilities.hasLikelihood = !quasi;
    model.capabilities.hasAIC = !quasi;
    model.capabilities.supportsNestedLRComparison = !quasi;
    InvalidateGeneralizedComparisonModel(state, model);
    SetGeneralizedComparisonActiveModel(state, modelIndex);
    return true;
}

bool SetGeneralizedComparisonExposure(GeneralizedComparisonState &state,
                                      int modelIndex,
                                      const std::string &exposure)
{
    if (!state.countComparison || modelIndex < 0 ||
        static_cast<std::size_t>(modelIndex) >= state.models.size()) return false;
    auto &model = state.models[static_cast<std::size_t>(modelIndex)];
    if (!exposure.empty() && exposure == model.offsetVariable) return false;
    if (model.exposure == exposure) return false;
    model.exposure = exposure;
    InvalidateGeneralizedComparisonModel(state, model);
    SetGeneralizedComparisonActiveModel(state, modelIndex);
    return true;
}

bool SetGeneralizedComparisonOffset(GeneralizedComparisonState &state,
                                    int modelIndex,
                                    const std::string &offsetVariable)
{
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size())
        return false;
    auto &model = state.models[static_cast<std::size_t>(modelIndex)];
    if (!offsetVariable.empty()) {
        const std::string &response = model.response.empty() ? state.response : model.response;
        if (offsetVariable == response || !state.hasSeed)
            return false;
        const auto numeric = NumericVariableNames(&state.seed);
        if (std::find(numeric.begin(), numeric.end(), offsetVariable) == numeric.end())
            return false;
        if (state.countComparison && offsetVariable == model.exposure)
            return false;
        if (model.family == "beta_one_inflated" ||
            (state.countComparison &&
             (model.countDistribution == CountDistribution::HurdleBetaBinomialCeiling ||
              model.countDistribution == CountDistribution::PerfectScore)))
            return false;
    }
    if (model.offsetVariable == offsetVariable) return false;
    model.offsetVariable = offsetVariable;
    InvalidateGeneralizedComparisonModel(state, model);
    SetGeneralizedComparisonActiveModel(state, modelIndex);
    return true;
}

bool SetGeneralizedComparisonTrials(GeneralizedComparisonState &state,
                                    int modelIndex,
                                    const std::string &trialsVariable,
                                    double trialsConstant)
{
    if (!state.countComparison || modelIndex < 0 ||
        static_cast<std::size_t>(modelIndex) >= state.models.size()) return false;
    auto &model = state.models[static_cast<std::size_t>(modelIndex)];
    if (!CountDistributionUsesTrials(model.countDistribution)) return false;
    if (trialsVariable.empty() &&
        (!std::isfinite(trialsConstant) || trialsConstant <= 0.0 ||
         std::floor(trialsConstant) != trialsConstant)) return false;
    if (model.trialsVariable == trialsVariable &&
        ((!std::isfinite(model.trialsConstant) && !std::isfinite(trialsConstant)) ||
         model.trialsConstant == trialsConstant)) return false;
    model.trialsVariable = trialsVariable;
    model.trialsConstant = trialsConstant;
    model.exposure.clear();
    InvalidateGeneralizedComparisonModel(state, model);
    SetGeneralizedComparisonActiveModel(state, modelIndex);
    return true;
}

bool SetGeneralizedComparisonModelScope(GeneralizedComparisonState &state,
                                        int modelIndex,
                                        const std::string &scope)
{
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size() ||
        (scope != "all" && scope != "selected" && scope != "unselected")) return false;
    auto &model = state.models[static_cast<std::size_t>(modelIndex)];
    if (model.scope == scope) return false;
    model.scope = scope;
    // Changing a semantic scope invalidates any previously captured explicit
    // rows for this model.  The R fit path resolves the selected/unselected
    // rows again from the current shared selection.
    model.dataScope = {};
    model.dataScopeCaptured = false;
    InvalidateGeneralizedComparisonModel(state, model);
    SetGeneralizedComparisonActiveModel(state, modelIndex);
    return true;
}

GeneralizedModelCapabilities GeneralizedComparisonModelCapabilities(
    const GeneralizedComparisonState &state,
    const GeneralizedComparisonModel &model)
{
    GeneralizedModelCapabilities capabilities;
    if (state.countComparison) {
        const bool quasi = model.countDistribution == CountDistribution::QuasiPoisson;
        capabilities.hasLikelihood = !quasi;
        capabilities.hasAIC = !quasi;
        capabilities.supportsNestedLRComparison = !quasi;
        if (model.fit.ok) {
            capabilities.hasLikelihood = model.capabilities.hasLikelihood;
            capabilities.hasAIC = model.capabilities.hasAIC;
            capabilities.supportsNestedLRComparison =
                model.capabilities.supportsNestedLRComparison;
        }
        return capabilities;
    }
    const std::string family = model.family.empty() ? state.family : model.family;
    const bool quasi = family.rfind("quasi", 0) == 0;
    capabilities.hasLikelihood = !quasi;
    capabilities.hasAIC = !quasi;
    capabilities.supportsNestedLRComparison = !quasi;
    return capabilities;
}

bool GeneralizedComparisonModelsSupportLikelihoodRatio(
    const GeneralizedComparisonState &state,
    const GeneralizedComparisonModel &previous,
    const GeneralizedComparisonModel &current,
    std::string *reason)
{
    auto fail = [&](const std::string &message) {
        if (reason) *reason = message;
        return false;
    };
    const auto leftCapabilities = GeneralizedComparisonModelCapabilities(state, previous);
    const auto rightCapabilities = GeneralizedComparisonModelCapabilities(state, current);
    if (!leftCapabilities.supportsNestedLRComparison ||
        !rightCapabilities.supportsNestedLRComparison) {
        return fail("Likelihood-ratio comparison is unavailable for quasi-likelihood models.");
    }
    if (previous.offsetVariable != current.offsetVariable) {
        return fail("Nested comparison requires the same offset variable.");
    }
    if (state.countComparison) {
        if (previous.countDistribution != current.countDistribution) {
            return fail("Nested likelihood-ratio comparison requires the same count distribution.");
        }
        if (previous.exposure != current.exposure) {
            return fail("Nested likelihood-ratio comparison requires the same exposure.");
        }
    } else {
        const std::string leftFamily = previous.family.empty() ? state.family : previous.family;
        const std::string rightFamily = current.family.empty() ? state.family : current.family;
        const std::string leftLink = previous.link.empty() ? state.link : previous.link;
        const std::string rightLink = current.link.empty() ? state.link : current.link;
        if (leftFamily != rightFamily || leftLink != rightLink) {
            return fail("Nested likelihood-ratio comparison requires the same distribution and link.");
        }
        if (previous.responseBoundsConfigured != current.responseBoundsConfigured ||
            (previous.responseBoundsConfigured &&
             (previous.responseLower != current.responseLower ||
              previous.responseUpper != current.responseUpper))) {
            return fail("Nested likelihood-ratio comparison requires the same response bounds.");
        }
    }
    const std::string leftResponse = previous.response.empty() ? state.response : previous.response;
    const std::string rightResponse = current.response.empty() ? state.response : current.response;
    if (leftResponse != rightResponse) return fail("Models must use the same response.");
    if (!SameIntegerSet(previous.fit.rowsUsed, current.fit.rowsUsed)) {
        return fail("Models must use the same analysis rows.");
    }
    if (!TermVectorsAreNested(previous.terms, current.terms)) {
        return fail("Model terms are not hierarchically nested.");
    }
    if (reason) reason->clear();
    return true;
}

GeneralizedComparisonModel *GeneralizedComparisonModelById(GeneralizedComparisonState &state,
                                                           const std::string &modelId)
{
    auto found = std::find_if(state.models.begin(), state.models.end(),
        [&](const GeneralizedComparisonModel &model) { return model.id == modelId; });
    return found == state.models.end() ? nullptr : &*found;
}

const GeneralizedComparisonModel *GeneralizedComparisonModelById(
    const GeneralizedComparisonState &state,
    const std::string &modelId)
{
    auto found = std::find_if(state.models.begin(), state.models.end(),
        [&](const GeneralizedComparisonModel &model) { return model.id == modelId; });
    return found == state.models.end() ? nullptr : &*found;
}

std::vector<std::string> GeneralizedComparisonModelIds(const GeneralizedComparisonState &state)
{
    std::vector<std::string> ids;
    ids.reserve(state.models.size());
    for (const GeneralizedComparisonModel &model : state.models) ids.push_back(model.id);
    return ids;
}

std::vector<std::string> GeneralizedComparisonModelLabels(const GeneralizedComparisonState &state)
{
    std::vector<std::string> labels;
    labels.reserve(state.models.size());
    for (const GeneralizedComparisonModel &model : state.models) labels.push_back(model.label);
    return labels;
}

std::string NextGeneralizedComparisonModelId(const GeneralizedComparisonState &state)
{
    return NextComparisonModelId(state.id, GeneralizedComparisonModelIds(state));
}

std::string NextUntitledGeneralizedComparisonLabel(const GeneralizedComparisonState &state)
{
    return NextUntitledComparisonLabel(GeneralizedComparisonModelLabels(state));
}

std::string UniqueGeneralizedComparisonCopyLabelForState(const GeneralizedComparisonState &state, const std::string &label)
{
    return UniqueGeneralizedComparisonCopyLabel(GeneralizedComparisonModelLabels(state), label);
}

std::vector<std::string> GLMInteractionGroupingFactors(
    const GLMInteractionReport &report,
    const std::map<std::string, std::string> &fittedTermTypes)
{
    if (report.kind == "three_way_with_numeric" && !report.ceilingHurdle) {
        std::vector<std::string> factors;
        for (const auto &variable : UniqueBaseVariablesForTerm(report.term)) {
            const auto type = fittedTermTypes.find(variable);
            if (type != fittedTermTypes.end() &&
                VariableTypeIsFactorLike(type->second))
                factors.push_back(variable);
        }
        return factors.size() >= 2 ? factors : std::vector<std::string>{};
    }
    if (report.kind != "factor_factor" &&
        report.kind != "factor_factor_factor") return {};
    if (report.ceilingHurdle) return {};
    for (const auto &section : report.sections) {
        const std::size_t expected = report.kind == "factor_factor" ? 2 : 3;
        if (section.identityHeaders.size() != expected) continue;
        const auto &headers = section.identityHeaders;
        if (std::find(headers.begin(), headers.end(), "Comparison") != headers.end()) continue;
        if (std::set<std::string>(headers.begin(), headers.end()).size() == expected)
            return headers;
    }
    return {};
}

ModelStatisticExplanationContext InteractionReportStatisticContext(
    const GLMInteractionReport &report,
    const GLMInteractionReportSection &section,
    const std::string &statistic)
{
    ModelStatisticExplanationContext context;
    context.statistic = statistic;
    context.analysisKind = "interaction interpretation";
    context.method = section.first;
    context.adjustment = report.multipleComparisons;
    context.multipleImputation = report.imputationCount > 1;
    context.miMethod = report.poolingMethod;
    context.rubinRulesApplied = context.multipleImputation && !report.poolingMethod.empty();
    const std::string sectionName = LowerCopy(section.first);
    const std::string wantedNote = sectionName.find("marginal mean") != std::string::npos
        ? "Estimated marginal means:"
        : sectionName.find("exponentiated") != std::string::npos
            ? "Transformed contrast:"
            : "Statistical contrasts:";
    for (const std::string &note : report.notes) {
        if (note.rfind(wantedNote, 0) == 0) {
            context.detail = note;
            break;
        }
    }
    if (context.detail.empty() && sectionName.find("interaction contrast") != std::string::npos) {
        for (const std::string &note : report.notes) {
            if (note.rfind("Interaction contrast estimand:", 0) == 0) {
                context.detail = note;
                break;
            }
        }
    }
    return context;
}

std::string GLMInteractionReportText(const GLMInteractionReport &report)
{
    std::ostringstream out;
    out << (report.binaryProbability ? "Effect term: " : "Interaction term: ")
        << report.term << "\n";
    if (report.binaryProbability) {
        out << "Event: " << report.eventLabel << "\n"
            << "Quantity: " << (report.quantity == "probability_difference"
                ? "Probability difference" : "Predicted probability") << "\n"
            << "Adjustment: " << (report.adjustmentMode == "reference_profile"
                ? "Reference profile" : "Average over analysis sample") << "\n"
            << "Presentation: " << (report.presentation == "percentage"
                ? (report.quantity == "probability_difference"
                    ? "Percentage points" : "Percentage")
                : (report.quantity == "probability_difference"
                    ? "Probability difference (-1 to 1)" : "Probability (0 to 1)"))
            << "\n";
    }
    if (!report.kind.empty()) out << "Type: " << report.kind << "\n";
    out << "Confidence level: " << FormatDoubleOrDash(
        report.confidenceLevel * 100.0, 0) << "%\n";
    if (!report.binaryProbability) {
        out << "Multiple comparisons: "
            << (report.multipleComparisons.empty()
                ? "Holm adjustment where applicable" : report.multipleComparisons)
            << "\n";
    }
    if (report.imputationCount > 1)
        out << "Imputations: " << report.imputationCount << "\n";
    if (!report.poolingMethod.empty())
        out << "Pooling: " << report.poolingMethod << "\n";
    if (!report.binaryProbability) {
        out << "EMM reference test: ";
        if (report.emmReferenceTestEnabled && std::isfinite(report.emmTestReference))
            out << FormatDoubleOrDash(report.emmTestReference, 4);
        else
            out << "None";
        out << "\n";
    }
    out << "\n";
    if (!report.interpretation.empty())
        out << "Interpretation: " << report.interpretation << "\n\n";
    if (!report.message.empty()) {
        out << report.message << "\n";
        return out.str();
    }
    for (const auto &section : report.sections) {
        const bool estimateOnly = section.inference ==
            GLMInteractionSectionInference::EstimateOnly;
        const bool adjusted = section.inference ==
            GLMInteractionSectionInference::AdjustedComparison;
        const std::vector<std::string> identityHeaders =
            section.identityHeaders.empty()
                ? std::vector<std::string>{"Effect"}
                : section.identityHeaders;
        out << section.first << "\n";
        for (std::size_t column = 0; column < identityHeaders.size(); ++column) {
            if (column) out << "\t";
            out << identityHeaders[column];
        }
        out << "\t" << section.estimateHeader << "\tSE";
        if (section.showDegreesOfFreedom)
            out << "\tdf";
        if (!estimateOnly)
            out << "\tStatistic\t" << (adjusted ? "Adjusted p" : "p");
        out << "\t" << section.lowerHeader << "\t" << section.upperHeader << "\n";
        if (section.second.empty()) {
            out << "(none)";
            for (std::size_t column = 1; column < identityHeaders.size(); ++column)
                out << "\t\u2014";
            out << "\t\u2014\t\u2014";
            if (section.showDegreesOfFreedom) out << "\t\u2014";
            if (!estimateOnly) out << "\t\u2014\t\u2014";
            out << "\t\u2014\t\u2014\n";
        }
        for (std::size_t rowIndex = 0; rowIndex < section.second.size(); ++rowIndex) {
            const LinearFunctionEstimate &row = section.second[rowIndex];
            const bool structured = rowIndex < section.identityRows.size() &&
                section.identityRows[rowIndex].size() == identityHeaders.size();
            if (structured) {
                for (std::size_t column = 0; column < identityHeaders.size(); ++column) {
                    if (column) out << "\t";
                    out << section.identityRows[rowIndex][column];
                }
            } else {
                out << row.label;
                for (std::size_t column = 1; column < identityHeaders.size(); ++column)
                    out << "\t\u2014";
            }
            out << "\t"
                << FormatDoubleOrDash(row.estimate, 4) << "\t"
                << FormatDoubleOrDash(row.stdError, 4);
            if (section.showDegreesOfFreedom)
                out << "\t" << FormatDoubleOrDash(row.degreesOfFreedom, 2);
            if (!estimateOnly) {
                out << "\t" << FormatDoubleOrDash(row.statistic, 3) << "\t"
                    << (adjusted && std::isfinite(row.adjustedPValue)
                        ? FormatPValue(row.adjustedPValue)
                        : FormatPValue(row.pValue));
            }
            out << "\t" << FormatDoubleOrDash(row.ciLower, 4) << "\t"
                << FormatDoubleOrDash(row.ciUpper, 4) << "\n";
        }
        out << "\n";
    }
    if (!report.notes.empty()) {
        for (const std::string &note : report.notes) out << note << "\n";
    } else {
        out << "Note: estimates are computed as linear functions of the fitted model coefficients (L beta) with standard errors from L V L'.\n";
        if (report.kind == "categorical by categorical")
            out << "Note: pairwise comparisons decompose the interaction for interpretation; the interaction itself is tested by its interaction term in the model table.\n";
    }
    return out.str();
}

bool ParseGLMInteractionReportText(const std::vector<std::string> &sourceLines,
                                   GLMInteractionReport &report)
{
    report = GLMInteractionReport{};
    std::vector<std::string> lines = sourceLines;
    const auto marker = std::find(lines.begin(), lines.end(), "ANALYSIS_PROVENANCE_V2");
    if (marker != lines.end()) {
        std::size_t cursor = static_cast<std::size_t>(marker - lines.begin());
        if (!ReadAnalysisProvenancePayload(lines, cursor, report.provenance, &report.message)) return false;
        lines.erase(marker, lines.end());
    }
    auto splitTabs = [](const std::string &line) {
        std::vector<std::string> fields;
        std::size_t begin = 0;
        for (;;) {
            const std::size_t separator = line.find('\t', begin);
            fields.push_back(line.substr(begin, separator == std::string::npos
                ? std::string::npos : separator - begin));
            if (separator == std::string::npos) break;
            begin = separator + 1;
        }
        return fields;
    };
    auto number = [](const std::string &text) {
        if (text.empty() || text == "\u2014" || text == "-")
            return std::numeric_limits<double>::quiet_NaN();
        if (text.rfind("<", 0) == 0) {
            const std::string rest = text.substr(1);
            char *end = nullptr;
            const double upper = std::strtod(rest.c_str(), &end);
            return end != rest.c_str() && std::isfinite(upper)
                ? upper / 2.0 : std::numeric_limits<double>::quiet_NaN();
        }
        char *end = nullptr;
        const double parsed = std::strtod(text.c_str(), &end);
        return end != text.c_str() && std::isfinite(parsed)
            ? parsed : std::numeric_limits<double>::quiet_NaN();
    };
    auto after = [](const std::string &line, const std::string &prefix) {
        return line.size() >= prefix.size() ? line.substr(prefix.size()) : std::string{};
    };
    for (std::size_t index = 0; index < lines.size(); ++index) {
        const std::string &line = lines[index];
        if (line.rfind("Interaction term:", 0) == 0) {
            report.term = after(line, "Interaction term: ");
            continue;
        }
        if (line.rfind("Effect term:", 0) == 0) {
            report.term = after(line, "Effect term: ");
            report.binaryProbability = true;
            report.kind = "binary probability effect";
            continue;
        }
        if (line.rfind("Event:", 0) == 0) {
            report.eventLabel = after(line, "Event: ");
            continue;
        }
        if (line.rfind("Quantity:", 0) == 0) {
            const std::string value = after(line, "Quantity: ");
            report.quantity = value.find("difference") != std::string::npos
                ? "probability_difference" : "predicted_probability";
            continue;
        }
        if (line.rfind("Adjustment:", 0) == 0) {
            const std::string value = after(line, "Adjustment: ");
            report.adjustmentMode = value.find("Reference") != std::string::npos
                ? "reference_profile" : "average_sample";
            continue;
        }
        if (line.rfind("Presentation:", 0) == 0) {
            const std::string value = after(line, "Presentation: ");
            report.presentation = value.find("0 to 1") != std::string::npos ||
                value.find("-1 to 1") != std::string::npos
                ? "probability" : "percentage";
            continue;
        }
        if (line.rfind("Type:", 0) == 0) {
            report.kind = after(line, "Type: ");
            continue;
        }
        if (line.rfind("EMM reference test:", 0) == 0) {
            const std::string value = after(line, "EMM reference test: ");
            report.emmTestReference = number(value);
            report.emmReferenceTestEnabled =
                std::isfinite(report.emmTestReference);
            continue;
        }
        if (line.rfind("Multiple comparisons:", 0) == 0) {
            report.multipleComparisons = after(line, "Multiple comparisons: ");
            continue;
        }
        if (line.rfind("Imputations:", 0) == 0) {
            const double value = number(after(line, "Imputations: "));
            if (std::isfinite(value))
                report.imputationCount = std::max(
                    1, static_cast<int>(std::llround(value)));
            continue;
        }
        if (line.rfind("Pooling:", 0) == 0) {
            report.poolingMethod = after(line, "Pooling: ");
            continue;
        }
        if (line.rfind("Interpretation:", 0) == 0) {
            report.interpretation = after(line, "Interpretation: ");
            continue;
        }
        if (line.rfind("Contrast factor A:", 0) == 0 ||
            line.rfind("Comparison factor B:", 0) == 0 ||
            line.rfind("Scale:", 0) == 0 ||
            line.rfind("Interaction contrasts:", 0) == 0 ||
            line.rfind("Interaction contrast estimand:", 0) == 0 ||
            line.rfind("Estimated marginal means:", 0) == 0 ||
            line.rfind("Statistical contrasts:", 0) == 0 ||
            line.rfind("Transformed contrast:", 0) == 0 ||
            line.rfind("Interaction contrast pooling:", 0) == 0 ||
            line.rfind("Orientation:", 0) == 0 ||
            line.rfind("Definition:", 0) == 0 ||
            line.rfind("Note:", 0) == 0) {
            report.notes.push_back(line);
            continue;
        }
        if (line.rfind("Confidence level:", 0) == 0) {
            std::string value = after(line, "Confidence level: ");
            if (!value.empty() && value.back() == '%') value.pop_back();
            const double parsed = number(value);
            if (std::isfinite(parsed)) report.confidenceLevel = parsed > 1.0
                ? parsed / 100.0 : parsed;
            continue;
        }
        if (line.empty()) continue;
        if (index + 1 >= lines.size()) continue;
        const auto candidateHeader = splitTabs(lines[index + 1]);
        const auto seHeader = std::find(candidateHeader.begin(),candidateHeader.end(),"SE");
        const auto candidateEstimate = seHeader != candidateHeader.begin() && seHeader != candidateHeader.end()
            ? seHeader-1 : candidateHeader.end();
        if (candidateEstimate == candidateHeader.end() ||
            candidateEstimate == candidateHeader.begin() ||
            std::find(candidateHeader.begin(), candidateHeader.end(), "SE") ==
                candidateHeader.end()) continue;
        GLMInteractionReportSection section;
        section.first = line;
        const auto header = splitTabs(lines[++index]);
        section.estimateHeader = *candidateEstimate;
        const std::size_t identityCount = static_cast<std::size_t>(
            std::distance(candidateHeader.begin(),candidateEstimate));
        section.identityHeaders.assign(header.begin(),
                                       header.begin() + identityCount);
        const bool hasTest = std::find(header.begin(), header.end(), "Statistic") !=
            header.end();
        section.showDegreesOfFreedom = std::find(header.begin(), header.end(), "df") !=
            header.end();
        if (header.size() >= 2) {
            section.lowerHeader = header[header.size() - 2];
            section.upperHeader = header.back();
        }
        const bool adjusted = std::find(header.begin(), header.end(), "Adjusted p") !=
            header.end();
        section.inference = !hasTest
            ? GLMInteractionSectionInference::EstimateOnly
            : adjusted
                ? GLMInteractionSectionInference::AdjustedComparison
                : (section.first.find("estimated marginal") != std::string::npos ||
                   section.first.find("Estimated marginal") != std::string::npos ||
                   section.first.find("predictions") != std::string::npos
                    ? GLMInteractionSectionInference::ReferenceTest
                    : GLMInteractionSectionInference::Test);
        while (index + 1 < lines.size() && !lines[index + 1].empty()) {
            const auto fields = splitTabs(lines[++index]);
            const std::size_t numericalCount = (hasTest ? 6U : 4U) +
                (section.showDegreesOfFreedom ? 1U : 0U);
            if (fields.size() < identityCount + numericalCount) continue;
            LinearFunctionEstimate row;
            std::vector<std::string> identities(
                fields.begin(), fields.begin() + identityCount);
            if (identityCount == 1) {
                row.label = identities.front();
            } else {
                for (std::size_t column = 0; column < identities.size(); ++column) {
                    if (!row.label.empty()) row.label += ", ";
                    row.label += section.identityHeaders[column] + " = " +
                        identities[column];
                }
            }
            std::size_t cursor = identityCount;
            row.estimate = number(fields[cursor++]);
            row.stdError = number(fields[cursor++]);
            if (section.showDegreesOfFreedom)
                row.degreesOfFreedom = number(fields[cursor++]);
            if (hasTest) {
                row.statistic = number(fields[cursor++]);
                row.pValue = number(fields[cursor++]);
                if (adjusted) row.adjustedPValue = row.pValue;
            }
            row.ciLower = number(fields[cursor++]);
            row.ciUpper = number(fields[cursor]);
            section.identityRows.push_back(std::move(identities));
            section.second.push_back(std::move(row));
        }
        report.sections.push_back(std::move(section));
    }
    report.ok = !report.sections.empty();
    if (!report.ok) report.message = "R returned no interaction-report tables.";
    return report.ok;
}

namespace {

const NumericVariable *GLMNumericVariable(const PlotModel &model, const std::string &name)
{
    return FindNumericVariable(model, name);
}

const DataColumn *GLMDataColumn(const DataFrameModel *dataframe, const std::string &name)
{
    return dataframe ? FindDataColumnInDataFrame(*dataframe, name) : nullptr;
}

bool BuildMainEffectDesignColumns(const PlotModel &model,
                                  const DataFrameModel *dataframe,
                                  const std::string &term,
                                  const std::map<std::string, std::string> &termTypes,
                                  const std::map<std::string, std::string> &factorReferenceLevels,
                                  std::vector<GLMDesignColumn> &columns,
                                  GLMFactorInfo *factorInfo,
                                  std::size_t &total,
                                  std::string &warning)
{
    const std::string variable = BaseVariableForTermComponent(term);
    if (!AvailableVariableExists(model, variable, dataframe)) {
        warning = GLMPredictorUnavailableStatus(term);
        return false;
    }
    const std::string type = ModelTermDisplayType(model, term, termTypes);
    if (type == "factor") {
        std::vector<std::string> raw;
        std::vector<std::string> declaredLevels;
        if (const DataColumn *col = GLMDataColumn(dataframe, variable)) {
            raw.reserve(col->values.size());
            for (std::size_t row = 0; row < col->values.size(); ++row)
                raw.push_back(DisplayValueForCell(*col, row));
            declaredLevels = col->definedLevels;
        } else if (const NumericVariable *xvar = GLMNumericVariable(model, variable)) {
            raw.reserve(xvar->values.size());
            for (double value : xvar->values) raw.push_back(FactorLevelTermValue(value));
        }
        if (raw.empty()) {
            warning = GLMFactorPredictorUnavailableStatus(term);
            return false;
        }
        total = std::min(total, raw.size());
        std::vector<std::string> levels;
        for (const std::string &level : declaredLevels) {
            if (DataCellIsMissing(level) ||
                std::find(raw.begin(), raw.end(), level) == raw.end() ||
                std::find(levels.begin(), levels.end(), level) != levels.end()) continue;
            levels.push_back(level);
        }
        for (const std::string &value : raw) {
            if (DataCellIsMissing(value) || std::find(levels.begin(), levels.end(), value) != levels.end()) continue;
            levels.push_back(value);
        }
        if (declaredLevels.empty()) SortFactorLevelsLikeR(levels);
        const auto requestedReference = factorReferenceLevels.find(variable);
        if (requestedReference != factorReferenceLevels.end()) {
            const auto foundReference = std::find(levels.begin(), levels.end(),
                                                  requestedReference->second);
            if (foundReference != levels.end() && foundReference != levels.begin()) {
                const std::string reference = *foundReference;
                levels.erase(foundReference);
                levels.insert(levels.begin(), reference);
            }
        }
        if (levels.size() < 2) return true;
        const std::string reference = levels.front();
        if (factorInfo) *factorInfo = GLMFactorInfo{term, levels};
        for (std::size_t levelIndex = 1; levelIndex < levels.size(); ++levelIndex) {
            GLMDesignColumn column;
            column.label = term + "=" + levels[levelIndex];
            column.sourceTerm = term;
            column.termType = "factor";
            column.factorLevel = levels[levelIndex];
            column.referenceLevel = reference;
            column.values.reserve(raw.size());
            for (const std::string &value : raw) {
                column.values.push_back(DataCellIsMissing(value) ? NAN : (value == levels[levelIndex] ? 1.0 : 0.0));
            }
            columns.push_back(column);
        }
        return true;
    }

    GLMDesignColumn column;
    std::string polynomialVariable;
    int polynomialDegree = 0;
    const bool polynomial = ParsePolynomialTerm(
        term, polynomialVariable, polynomialDegree);
    column.label = polynomial ? PolynomialTerm(variable, polynomialDegree) : variable;
    column.sourceTerm = term;
    column.termType = "numeric";
    if (const NumericVariable *xvar = GLMNumericVariable(model, variable)) {
        column.values = xvar->values;
    } else if (const DataColumn *col = GLMDataColumn(dataframe, variable)) {
        column.values.reserve(col->values.size());
        for (const std::string &value : col->values) {
            double parsed = NAN;
            column.values.push_back(ParseDataCellDouble(value, parsed) ? parsed : NAN);
        }
    }
    if (column.values.empty()) {
        warning = GLMNumericPredictorUnavailableStatus(term);
        return false;
    }
    if (polynomial) {
        for (double &value : column.values) {
            if (std::isfinite(value)) value = std::pow(value, polynomialDegree);
        }
    }
    total = std::min(total, column.values.size());
    columns.push_back(column);
    return true;
}

bool BuildInteractionDesignColumns(const PlotModel &model,
                                   const DataFrameModel *dataframe,
                                   const std::string &term,
                                   const std::map<std::string, std::string> &termTypes,
                                   const std::map<std::string, std::string> &factorReferenceLevels,
                                   std::vector<GLMDesignColumn> &columns,
                                   std::vector<GLMFactorInfo> *factorInfos,
                                   std::size_t &total,
                                   std::string &warning)
{
    const std::vector<std::string> parts = SplitInteractionTerm(term);
    if (parts.size() < 2) {
        return BuildMainEffectDesignColumns(model, dataframe, term, termTypes,
                                            factorReferenceLevels, columns, nullptr, total, warning);
    }
    std::vector<GLMDesignColumn> combined(1);
    combined.front().sourceTerm = term;
    combined.front().termType = ModelTermDisplayType(model, term, termTypes);
    combined.front().values.assign(total, 1.0);
    for (const std::string &part : parts) {
        std::vector<GLMDesignColumn> partColumns;
        GLMFactorInfo factorInfo;
        if (!BuildMainEffectDesignColumns(model, dataframe, part, termTypes,
                                          factorReferenceLevels, partColumns,
                                          &factorInfo, total, warning)) return false;
        if (factorInfos && !factorInfo.levels.empty() &&
            std::none_of(factorInfos->begin(), factorInfos->end(), [&](const GLMFactorInfo &existing) {
                return BaseVariableForTermComponent(existing.term) ==
                    BaseVariableForTermComponent(factorInfo.term);
            })) {
            factorInfos->push_back(factorInfo);
        }
        if (partColumns.empty()) return true;
        std::vector<GLMDesignColumn> next;
        for (const GLMDesignColumn &left : combined) {
            for (const GLMDesignColumn &right : partColumns) {
                GLMDesignColumn column;
                column.label = left.label.empty() ? right.label : left.label + ":" + right.label;
                column.sourceTerm = term;
                column.termType = ModelTermDisplayType(model, term, termTypes);
                column.factorLevel = !right.factorLevel.empty()
                    ? right.factorLevel : left.factorLevel;
                column.referenceLevel = !right.referenceLevel.empty()
                    ? right.referenceLevel : left.referenceLevel;
                const std::size_t n = std::min(left.values.size(), right.values.size());
                column.values.reserve(n);
                for (std::size_t i = 0; i < n; ++i) {
                    column.values.push_back(std::isfinite(left.values[i]) && std::isfinite(right.values[i]) ?
                                            left.values[i] * right.values[i] : NAN);
                }
                next.push_back(column);
            }
        }
        combined = std::move(next);
    }
    for (GLMDesignColumn &column : combined) {
        if (column.label.empty()) column.label = term;
        columns.push_back(std::move(column));
    }
    return true;
}

} // namespace

GLMFitSummary FitMultipleLinearModel(const PlotModel &model,
                                     const DataFrameModel *dataframe,
                                     const std::set<int> &selectedRows,
                                     const std::string &dependent,
                                     const std::vector<std::string> &terms,
                                     const std::map<std::string, std::string> &termTypes,
                                     const std::string &scope,
                                     const std::set<std::string> &centeredPredictors,
                                     const std::map<std::string, std::string> &factorReferenceLevels)
{
    GLMFitSummary fit;
    if (dependent.empty()) {
        fit.warning = GLMChooseDependentVariableStatus();
        return fit;
    }
    const NumericVariable *yvar = GLMNumericVariable(model, dependent);
    if (!yvar) {
        fit.warning = GLMDependentVariableNotAvailableStatus();
        return fit;
    }
    std::vector<GLMDesignColumn> design;
    std::vector<GLMFactorInfo> factorInfos;
    std::size_t total = yvar->values.size();
    const std::vector<std::string> available = AvailableVariableNames(model, dataframe);
    for (const std::string &term : terms) {
        if (!ModelTermExistsForVariables(available, term, dependent)) continue;
        if (IsInteractionTerm(term)) {
            if (!BuildInteractionDesignColumns(model, dataframe, term, termTypes,
                                               factorReferenceLevels, design, &factorInfos,
                                               total, fit.warning)) return fit;
        } else {
            GLMFactorInfo factorInfo;
            std::vector<GLMDesignColumn> termColumns;
            if (!BuildMainEffectDesignColumns(model, dataframe, term, termTypes,
                                              factorReferenceLevels, termColumns,
                                              &factorInfo, total, fit.warning)) return fit;
            if (!factorInfo.levels.empty() &&
                std::none_of(factorInfos.begin(), factorInfos.end(), [&](const GLMFactorInfo &existing) {
                    return BaseVariableForTermComponent(existing.term) ==
                        BaseVariableForTermComponent(factorInfo.term);
                })) {
                factorInfos.push_back(std::move(factorInfo));
            }
            design.insert(design.end(), termColumns.begin(), termColumns.end());
        }
    }
    const std::size_t p = design.size() + 1;
    std::vector<std::vector<double>> x;
    std::vector<double> y;
    std::vector<int> rowIds;
    std::set<int> usedRows;
    for (std::size_t i = 0; i < total; ++i) {
        const int row = (int)i + 1;
        if (scope == "selected" && selectedRows.find(row) == selectedRows.end()) continue;
        if (scope == "unselected" && selectedRows.find(row) != selectedRows.end()) continue;
        if (!std::isfinite(yvar->values[i])) continue;
        std::vector<double> xr(p, 1.0);
        bool ok = true;
        for (std::size_t j = 0; j < design.size(); ++j) {
            if (i >= design[j].values.size() || !std::isfinite(design[j].values[i])) { ok = false; break; }
            xr[j + 1] = design[j].values[i];
        }
        if (ok) { x.push_back(std::move(xr)); y.push_back(yvar->values[i]); rowIds.push_back(row); usedRows.insert(row); }
    }
    fit.n = (int)y.size();
    fit.excluded = (int)total - fit.n;
    fit.rowsUsed = rowIds;
    for (std::size_t i = 0; i < total; ++i) if (usedRows.find((int)i + 1) == usedRows.end()) fit.rowsExcluded.push_back((int)i + 1);

    // Centre source variables, then rebuild the complete design.  This keeps
    // interactions correct: (x - mean(x)):factor cannot be obtained by merely
    // subtracting a constant from the finished interaction columns.
    if (!centeredPredictors.empty() && !rowIds.empty()) {
        PlotModel centredModel = model;
        DataFrameModel centredDataframe;
        DataFrameModel *centredDataframePointer = nullptr;
        if (dataframe) {
            centredDataframe = *dataframe;
            centredDataframePointer = &centredDataframe;
        }
        bool changed = false;
        std::map<std::string, double> centers;
        for (const std::string &variable : centeredPredictors) {
            if (variable == dependent ||
                ModelTermDisplayType(model, variable, termTypes) == "factor") continue;
            std::vector<double> source;
            if (const NumericVariable *numeric = GLMNumericVariable(model, variable)) {
                source = numeric->values;
            } else if (const DataColumn *column = GLMDataColumn(dataframe, variable)) {
                source.reserve(column->values.size());
                for (const std::string &cell : column->values) {
                    double parsed = NAN;
                    source.push_back(ParseDataCellDouble(cell, parsed) ? parsed : NAN);
                }
            }
            double sum = 0.0;
            std::size_t count = 0;
            for (int row : rowIds) {
                const std::size_t index = static_cast<std::size_t>(row - 1);
                if (index < source.size() && std::isfinite(source[index])) {
                    sum += source[index];
                    ++count;
                }
            }
            if (count == 0) continue;
            const double centre = sum / static_cast<double>(count);
            centers[variable] = centre;
            for (NumericVariable &numeric : centredModel.variables) {
                if (numeric.name != variable) continue;
                for (double &value : numeric.values) if (std::isfinite(value)) value -= centre;
            }
            if (centredDataframePointer) {
                for (DataColumn &column : centredDataframePointer->columns) {
                    if (column.name != variable) continue;
                    for (std::string &cell : column.values) {
                        double parsed = NAN;
                        if (!ParseDataCellDouble(cell, parsed) || !std::isfinite(parsed)) continue;
                        std::ostringstream formatted;
                        formatted << std::setprecision(17) << parsed - centre;
                        cell = formatted.str();
                    }
                }
            }
            changed = true;
        }
        if (changed) {
            GLMFitSummary centeredFit = FitMultipleLinearModel(
                centredModel, centredDataframePointer, selectedRows, dependent,
                terms, termTypes, scope, {}, factorReferenceLevels);
            centeredFit.predictorCenters = std::move(centers);
            return centeredFit;
        }
    }
    LinearOlsResult ols = FitLinearOls(LinearOlsInput{x, y, rowIds});
    fit.n = ols.n; fit.dfModel = ols.dfModel; fit.dfResidual = ols.dfResidual;
    fit.r2 = ols.r2; fit.adjR2 = ols.adjR2; fit.globalF = ols.globalF; fit.globalP = ols.globalP;
    fit.ssRegression = ols.ssRegression; fit.ssResidual = ols.ssResidual; fit.msRegression = ols.msRegression;
    fit.msResidual = ols.msResidual; fit.rmse = ols.rmse; fit.sigma = ols.sigma; fit.aic = ols.aic; fit.bic = ols.bic;
    fit.beta = ols.beta; fit.covariance = ols.covariance; fit.warning = ols.warning;
    fit.designLabels.push_back("(Intercept)");
    for (const GLMDesignColumn &column : design) fit.designLabels.push_back(column.label);
    for (const GLMFactorInfo &factor : factorInfos) {
        if (factor.levels.empty()) continue;
        GLMFactorCoding coding;
        coding.variable = BaseVariableForTermComponent(factor.term);
        coding.levels = factor.levels;
        coding.referenceLevel = factor.levels.front();
        coding.contrastLabels.assign(factor.levels.begin() + 1, factor.levels.end());
        coding.coding.assign(factor.levels.size(),
                             std::vector<double>(coding.contrastLabels.size(), 0.0));
        for (std::size_t levelIndex = 1; levelIndex < factor.levels.size(); ++levelIndex) {
            coding.coding[levelIndex][levelIndex - 1] = 1.0;
        }
        fit.factorCodings.push_back(std::move(coding));
    }
    for (const LinearOlsDiagnosticRow &diag : ols.diagnostics) fit.diagnostics.push_back(diag);
    if (!ols.ok) return fit;

    // Term-level squared semipartial contribution.  Build every reduced
    // model from the exact complete-case design used by the full fit, so
    // missing-value handling, transformations, contrasts, scope, and row
    // identity cannot drift between the two fits.
    std::map<std::string, double> deltaR2ByTerm;
    for (const std::string &term : terms) {
        std::vector<std::size_t> keptColumns;
        bool removed = false;
        for (std::size_t column = 0; column < design.size(); ++column) {
            if (design[column].sourceTerm == term) removed = true;
            else keptColumns.push_back(column);
        }
        if (!removed) continue;
        std::vector<std::vector<double>> reducedX;
        reducedX.reserve(x.size());
        for (const std::vector<double> &fullRow : x) {
            std::vector<double> reducedRow;
            reducedRow.reserve(keptColumns.size() + 1);
            reducedRow.push_back(1.0);
            for (std::size_t column : keptColumns) reducedRow.push_back(fullRow[column + 1]);
            reducedX.push_back(std::move(reducedRow));
        }
        const LinearOlsResult reduced = FitLinearOls(LinearOlsInput{reducedX, y, rowIds});
        if (!reduced.ok || !std::isfinite(ols.r2) || !std::isfinite(reduced.r2)) continue;
        double delta = ols.r2 - reduced.r2;
        const double tolerance = 1.0e-10 * std::max(1.0, std::fabs(ols.r2));
        if (delta < -tolerance) continue;
        if (delta < 0.0) delta = 0.0;
        deltaR2ByTerm[term] = delta;
    }

    std::vector<GLMCoefficientRow> raw;
    for (std::size_t j = 0; j < p; ++j) {
        GLMCoefficientRow row;
        if (j == 0) { row.term = "(Intercept)"; row.sourceTerm = "(Intercept)"; row.termType = "intercept"; row.displayLabel = "(Intercept)"; }
        else { const GLMDesignColumn &column = design[j - 1]; row.term = column.label; row.sourceTerm = column.sourceTerm; row.termType = column.termType; row.displayLabel = column.label; row.factorLevel = column.factorLevel; row.referenceLevel = column.referenceLevel; }
        if (j < ols.coefficients.size()) { const LinearOlsCoefficient &c = ols.coefficients[j]; row.estimate = c.estimate; row.standardizedBeta = c.standardizedBeta; row.stdError = c.stdError; row.tValue = c.tValue; row.pValue = c.pValue; row.partialR = c.partialR; }
        raw.push_back(std::move(row));
    }
    auto findRaw = [&](const std::string &term) -> const GLMCoefficientRow * { for (const auto &row : raw) if (row.term == term) return &row; return nullptr; };
    auto rowsForSourceTerm = [&](const std::string &source) { std::vector<GLMCoefficientRow> rows; for (const auto &row : raw) if (row.sourceTerm == source) rows.push_back(row); return rows; };
    auto termDeltaR2 = [&](const std::string &term) {
        const auto found = deltaR2ByTerm.find(term);
        return found == deltaR2ByTerm.end() ? NAN : found->second;
    };
    if (const GLMCoefficientRow *intercept = findRaw("(Intercept)")) fit.coefficients.push_back(*intercept);
    for (const std::string &term : terms) {
        auto factor = std::find_if(factorInfos.begin(), factorInfos.end(), [&](const GLMFactorInfo &info) { return info.term == term; });
        if (factor != factorInfos.end()) {
            GLMCoefficientRow parent; parent.term = term; parent.sourceTerm = term; parent.termType = "factor"; parent.rowType = "factor_parent"; parent.displayLabel = term; parent.referenceLevel = factor->levels.front(); parent.deltaR2 = termDeltaR2(term); fit.coefficients.push_back(parent);
            for (std::size_t i = 0; i < factor->levels.size(); ++i) {
                const std::string dummy = term + "=" + factor->levels[i];
                if (i == 0) { GLMCoefficientRow ref; ref.term = dummy; ref.sourceTerm = term; ref.termType = "factor"; ref.rowType = "reference"; ref.displayLabel = "  " + factor->levels[i]; ref.factorLevel = factor->levels[i]; ref.referenceLevel = factor->levels[i]; fit.coefficients.push_back(ref); }
                else if (const GLMCoefficientRow *level = findRaw(dummy)) { GLMCoefficientRow row = *level; row.rowType = "factor_level"; row.displayLabel = "  " + factor->levels[i]; fit.coefficients.push_back(std::move(row)); }
            }
        } else if (IsInteractionTerm(term)) {
            std::vector<GLMCoefficientRow> rows = rowsForSourceTerm(term);
            if (!rows.empty()) {
                const std::vector<std::string> parts = SplitInteractionTerm(term);
                GLMCoefficientRow parent; parent.term = term; parent.sourceTerm = term;
                parent.termType = ModelTermDisplayType(model, term, termTypes);
                parent.rowType = "term_parent";
                parent.displayLabel = JoinStrings(parts, " × ");
                parent.deltaR2 = termDeltaR2(term);
                fit.coefficients.push_back(parent);
                std::string numericPart;
                std::string factorPart;
                std::vector<std::string> factorParts;
                if (parts.size() == 2) {
                    for (const std::string &part : parts) {
                        if (ModelTermDisplayType(model, part, termTypes) == "factor") {
                            factorPart = part;
                            factorParts.push_back(part);
                        }
                        else numericPart = part;
                    }
                } else {
                    for (const std::string &part : parts) {
                        if (ModelTermDisplayType(model, part, termTypes) == "factor") {
                            factorParts.push_back(part);
                        }
                    }
                }
                for (GLMCoefficientRow &row : rows) {
                    row.rowType = "coefficient";
                    row.termType = parent.termType;
                    if (!numericPart.empty() && !factorPart.empty() &&
                        !row.factorLevel.empty()) {
                        row.displayLabel = "  " + BaseVariableForTermComponent(numericPart) +
                            " × " + BaseVariableForTermComponent(factorPart) + " = " +
                            row.factorLevel;
                        if (!row.referenceLevel.empty()) {
                            row.displayLabel += " (vs " + row.referenceLevel + ")";
                        }
                    } else if (factorParts.size() >= 2 &&
                               factorParts.size() == parts.size()) {
                        std::vector<std::string> levels;
                        std::vector<std::string> references;
                        const std::vector<std::string> coefficientParts =
                            SplitInteractionTerm(row.term);
                        for (std::size_t index = 0;
                             index < factorParts.size() && index < coefficientParts.size();
                             ++index) {
                            const std::size_t equals = coefficientParts[index].find('=');
                            if (equals == std::string::npos || equals + 1 >= coefficientParts[index].size()) {
                                levels.clear();
                                break;
                            }
                            levels.push_back(coefficientParts[index].substr(equals + 1));
                            const std::string variable = BaseVariableForTermComponent(factorParts[index]);
                            const auto coding = std::find_if(
                                factorInfos.begin(), factorInfos.end(),
                                [&](const GLMFactorInfo &info) {
                                    return BaseVariableForTermComponent(info.term) == variable;
                                });
                            if (coding == factorInfos.end() || coding->levels.empty()) {
                                references.clear();
                            } else if (!references.empty() || index == 0) {
                                references.push_back(coding->levels.front());
                            }
                        }
                        if (levels.size() == factorParts.size()) {
                            row.factorLevel = JoinStrings(levels, " × ");
                            row.referenceLevel = references.size() == factorParts.size()
                                ? JoinStrings(references, " × ") : "";
                            row.displayLabel = "  " + row.factorLevel;
                            if (!row.referenceLevel.empty()) {
                                row.displayLabel += " (vs " + row.referenceLevel + ")";
                            }
                        } else {
                            row.displayLabel = "  " + row.term;
                        }
                    } else {
                        row.displayLabel = "  " + row.term;
                    }
                    fit.coefficients.push_back(std::move(row));
                }
            }
        } else if (const GLMCoefficientRow *row = findRaw(term)) {
            GLMCoefficientRow display = *row;
            auto delta = deltaR2ByTerm.find(term);
            if (delta != deltaR2ByTerm.end()) display.deltaR2 = delta->second;
            fit.coefficients.push_back(std::move(display));
        }
    }
    fit.ok = true;
    return fit;
}

GLMFitSummary FitMultipleLinearModel(const PlotModel &model,
                                     const DataFrameModel *dataframe,
                                     const std::set<int> &selectedRows,
                                     const ModelSpecification &specification)
{
    return FitMultipleLinearModel(
        model, dataframe, selectedRows, specification.response,
        specification.terms,
        EffectiveModelSpecificationTermTypes(specification), specification.scope,
        specification.centeredPredictors, specification.factorReferenceLevels);
}

GLMInteractionReport BuildGLMInteractionReport(const PlotModel &model,
                                               const DataFrameModel *dataframe,
                                               const std::string &term,
                                               const std::vector<std::string> &terms,
                                               const std::map<std::string, std::string> &termTypes,
                                               const GLMFitSummary &fit,
                                               double confidenceLevel,
                                               const std::set<std::string> &centeredPredictors,
                                               double emmTestReference)
{
    GLMInteractionReport report;
    report.term = term;
    report.multipleComparisons = "Holm adjustment where applicable";
    report.emmReferenceTestEnabled = std::isfinite(emmTestReference);
    report.emmTestReference = report.emmReferenceTestEnabled
        ? emmTestReference : NAN;
    if (!fit.ok || fit.beta.empty() || fit.covariance.empty()) {
        report.message = fit.warning.empty() ? "Fit the model before interpreting interactions." : fit.warning;
        return report;
    }
    if (!IsInteractionTerm(term)) { report.message = "This action is only available for interaction terms."; return report; }
    const std::vector<std::string> parts = SplitInteractionTerm(term);
    if (parts.size() != 2) { report.message = "Only two-way interactions can be interpreted here. Higher-order interactions are left in the model but are not expanded automatically."; return report; }
    const std::string a = BaseVariableForTermComponent(parts[0]);
    const std::string b = BaseVariableForTermComponent(parts[1]);
    if (a.empty() || b.empty() || a == b) { report.message = "This interaction does not contain two distinct variables."; return report; }
    const std::set<int> rows = GLMRowsUsedSet(fit);
    auto summary = [&](const std::string &variable) {
        return GLMNumericSummaryForVariable(GLMDataColumn(dataframe, variable), model.variables, variable, rows);
    };
    auto fittedCenter = [&](const std::string &variable) {
        const auto stored = fit.predictorCenters.find(variable);
        if (stored != fit.predictorCenters.end() && std::isfinite(stored->second)) {
            return stored->second;
        }
        const NumericSummary values = summary(variable);
        return values.ok ? values.mean : 0.0;
    };
    auto levels = [&](const std::string &variable) {
        if (const GLMFactorCoding *coding = FactorCodingForVariable(fit, variable)) {
            if (!coding->levels.empty()) return coding->levels;
        }
        std::vector<std::string> result; std::set<std::string> seen;
        if (const DataColumn *col = GLMDataColumn(dataframe, variable)) {
            for (std::size_t i = 0; i < col->values.size(); ++i) {
                const std::string value = DisplayValueForCell(*col, i);
                if (GLMRowIncluded(rows, (int)i + 1) && !DataCellIsMissing(value) &&
                    seen.insert(value).second) result.push_back(value);
            }
        } else if (const NumericVariable *var = GLMNumericVariable(model, variable)) {
            for (std::size_t i = 0; i < var->values.size(); ++i) if (GLMRowIncluded(rows, (int)i + 1) && std::isfinite(var->values[i]) && seen.insert(FactorLevelTermValue(var->values[i])).second) result.push_back(FactorLevelTermValue(var->values[i]));
        }
        return result;
    };
    auto design = [&](const std::map<std::string, double> &numeric, const std::map<std::string, std::string> &factor, std::vector<double> &out) {
        ScenarioDesignInput input; input.terms = terms; input.termTypes = termTypes;
        input.numericValues = numeric; input.factorValues = factor; input.expectedSize = fit.beta.size();
        for (auto &entry : input.numericValues) {
            if (!centeredPredictors.count(entry.first)) continue;
            entry.second -= fittedCenter(entry.first);
        }
        for (const std::string &variable : BaseVariablesFromModelTerms(AvailableVariableNames(model, dataframe), "", terms)) {
            ScenarioVariableInfo info; info.type = GLMBaseVariableType(&model, variable, termTypes);
            if (info.type == "factor") {
                if (!ApplyFittedFactorCoding(fit, variable, info)) {
                    info.factorLevels = levels(variable);
                }
            }
            else { NumericSummary values = summary(variable); info.numericDefault =
                centeredPredictors.count(variable) ? 0.0 : (values.ok ? values.mean : 0.0); }
            input.variables[variable] = std::move(info);
        }
        return BuildScenarioDesignVector(input, out);
    };
    auto estimate = [&](const std::vector<double> &contrast, const std::string &label) { return EstimateLinearFunction(fit.beta, fit.covariance, contrast, label, confidenceLevel); };
    auto applyReferenceTest = [&](LinearFunctionEstimate &row) {
        if (!report.emmReferenceTestEnabled || !std::isfinite(row.stdError) ||
            row.stdError <= 0.0) return;
        row.statistic = (row.estimate - report.emmTestReference) / row.stdError;
        row.pValue = NormalTwoSidedP(row.statistic);
    };
    auto prediction = [&](const std::map<std::string, double> &numeric, const std::map<std::string, std::string> &factor, const std::string &label) {
        std::vector<double> values; if (!design(numeric, factor, values)) { LinearFunctionEstimate missing; missing.label = label; return missing; } return estimate(values, label);
    };
    auto difference = [&](const std::map<std::string, double> &numericA, const std::map<std::string, std::string> &factorA, const std::map<std::string, double> &numericB, const std::map<std::string, std::string> &factorB, std::vector<double> &contrast) {
        std::vector<double> va, vb; if (!design(numericA, factorA, va) || !design(numericB, factorB, vb)) return false; contrast = SubtractVectors(va, vb); return !contrast.empty();
    };
    const std::string aType = GLMBaseVariableType(&model, a, termTypes);
    const std::string bType = GLMBaseVariableType(&model, b, termTypes);
    if ((aType == "numeric" && bType == "factor") || (aType == "factor" && bType == "numeric")) {
        const std::string x = aType == "numeric" ? a : b, g = aType == "factor" ? a : b;
        report.kind = "continuous by categorical";
        NumericSummary xValues = summary(x); std::vector<std::string> groupLevels = levels(g);
        if (!xValues.ok || groupLevels.empty()) { report.message = "The interaction could not be interpreted because one component has no fitted values."; return report; }
        std::vector<LinearFunctionEstimate> slopes; std::vector<std::vector<double>> contrasts;
        for (const std::string &level : groupLevels) { std::vector<double> contrast; if (difference({{x, xValues.mean + 1.0}}, {{g, level}}, {{x, xValues.mean}}, {{g, level}}, contrast)) { contrasts.push_back(contrast); slopes.push_back(estimate(contrast, "Slope of " + x + " when " + g + " = " + level)); } }
        report.sections.push_back({"Simple slopes", slopes,
            GLMInteractionSectionInference::Test});
        std::vector<LinearFunctionEstimate> comparisons;
        for (std::size_t i = 0; i < contrasts.size(); ++i) for (std::size_t j = i + 1; j < contrasts.size(); ++j) comparisons.push_back(estimate(SubtractVectors(contrasts[i], contrasts[j]), groupLevels[i] + " - " + groupLevels[j] + " slope difference"));
        ApplyHolmAdjustedPValues(comparisons); report.sections.push_back({"Pairwise slope comparisons (Holm)", comparisons,
            GLMInteractionSectionInference::AdjustedComparison});
        std::vector<std::pair<std::string, double>> values{{"M", xValues.mean}}; if (std::isfinite(xValues.sd) && xValues.sd > 0.0) values = {{"M - SD", xValues.mean - xValues.sd}, {"M", xValues.mean}, {"M + SD", xValues.mean + xValues.sd}};
        std::vector<LinearFunctionEstimate> predictions; for (const std::string &level : groupLevels) for (const auto &value : values) predictions.push_back(prediction({{x, value.second}}, {{g, level}}, g + " = " + level + ", " + x + " = " + value.first));
        for (auto &row : predictions) applyReferenceTest(row);
        report.sections.push_back({"Predicted means", predictions,
            report.emmReferenceTestEnabled
                ? GLMInteractionSectionInference::ReferenceTest
                : GLMInteractionSectionInference::EstimateOnly});
    } else if (aType == "factor" && bType == "factor") {
        report.kind = "categorical by categorical";
        const std::vector<std::string> aLevels = levels(a), bLevels = levels(b);
        if (aLevels.empty() || bLevels.empty()) { report.message = "The interaction could not be interpreted because one categorical predictor has no fitted categories."; return report; }
        report.interpretation = "Estimated marginal means for " + a +
            " at each category of " + b + ", followed by Holm-adjusted "
            "pairwise comparisons of " + a + " within each " + b +
            ". The reverse comparison direction is reported separately below.";
        std::vector<LinearFunctionEstimate> predictions;
        std::vector<std::vector<std::string>> predictionIdentities;
        for (const std::string &av : aLevels) {
            for (const std::string &bv : bLevels) {
                predictions.push_back(prediction(
                    {}, {{a, av}, {b, bv}},
                    a + " = " + av + ", " + b + " = " + bv));
                predictionIdentities.push_back({av, bv});
            }
        }
        for (auto &row : predictions) applyReferenceTest(row);
        report.sections.push_back({"Estimated marginal means", predictions,
            report.emmReferenceTestEnabled
                ? GLMInteractionSectionInference::ReferenceTest
                : GLMInteractionSectionInference::EstimateOnly});
        report.sections.back().identityHeaders = {a, b};
        report.sections.back().identityRows = std::move(predictionIdentities);
        std::vector<LinearFunctionEstimate> effectsA, effectsB;
        std::vector<std::vector<std::string>> effectsAIdentities;
        std::vector<std::vector<std::string>> effectsBIdentities;
        for (const std::string &bv : bLevels) {
            for (std::size_t i = 0; i < aLevels.size(); ++i) {
                for (std::size_t j = i + 1; j < aLevels.size(); ++j) {
                    std::vector<double> contrast;
                    if (!difference({}, {{a, aLevels[i]}, {b, bv}},
                                    {}, {{a, aLevels[j]}, {b, bv}}, contrast))
                        continue;
                    const std::string comparison = aLevels[i] + " - " + aLevels[j];
                    effectsA.push_back(estimate(
                        contrast, comparison + " within " + b + " = " + bv));
                    effectsAIdentities.push_back({bv, comparison});
                }
            }
        }
        for (const std::string &av : aLevels) {
            for (std::size_t i = 0; i < bLevels.size(); ++i) {
                for (std::size_t j = i + 1; j < bLevels.size(); ++j) {
                    std::vector<double> contrast;
                    if (!difference({}, {{a, av}, {b, bLevels[i]}},
                                    {}, {{a, av}, {b, bLevels[j]}}, contrast))
                        continue;
                    const std::string comparison = bLevels[i] + " - " + bLevels[j];
                    effectsB.push_back(estimate(
                        contrast, comparison + " within " + a + " = " + av));
                    effectsBIdentities.push_back({av, comparison});
                }
            }
        }
        ApplyHolmAdjustedPValues(effectsA);
        ApplyHolmAdjustedPValues(effectsB);
        report.sections.push_back({
            "Pairwise comparisons of " + a + " within " + b + " (Holm)",
            effectsA, GLMInteractionSectionInference::AdjustedComparison});
        report.sections.back().identityHeaders = {b, "Comparison"};
        report.sections.back().identityRows = std::move(effectsAIdentities);
        report.sections.push_back({
            "Pairwise comparisons of " + b + " within " + a + " (Holm)",
            effectsB, GLMInteractionSectionInference::AdjustedComparison});
        report.sections.back().identityHeaders = {a, "Comparison"};
        report.sections.back().identityRows = std::move(effectsBIdentities);
    } else {
        report.kind = "continuous by continuous";
        NumericSummary av = summary(a), bv = summary(b);
        if (!av.ok || !bv.ok) { report.message = "The interaction could not be interpreted because one component has no fitted values."; return report; }
        auto spread = [](const NumericSummary &v) { return std::isfinite(v.sd) && v.sd > 0.0 ? std::vector<std::pair<std::string, double>>{{"M - SD", v.mean - v.sd}, {"M", v.mean}, {"M + SD", v.mean + v.sd}} : std::vector<std::pair<std::string, double>>{{"M", v.mean}}; };
        const auto as = spread(av), bs = spread(bv); std::vector<LinearFunctionEstimate> slopesA, slopesB, predictions;
        for (const auto &value : bs) { std::vector<double> c; if (difference({{a, av.mean + 1.0}, {b, value.second}}, {}, {{a, av.mean}, {b, value.second}}, {}, c)) slopesA.push_back(estimate(c, "Slope of " + a + " when " + b + " = " + value.first)); }
        for (const auto &value : as) { std::vector<double> c; if (difference({{a, value.second}, {b, bv.mean + 1.0}}, {}, {{a, value.second}, {b, bv.mean}}, {}, c)) slopesB.push_back(estimate(c, "Slope of " + b + " when " + a + " = " + value.first)); }
        for (const auto &avalue : as) for (const auto &bvalue : bs) predictions.push_back(prediction({{a, avalue.second}, {b, bvalue.second}}, {}, a + " = " + avalue.first + ", " + b + " = " + bvalue.first));
        report.sections.push_back({"Conditional slopes of " + a, slopesA,
            GLMInteractionSectionInference::Test}); report.sections.push_back({"Conditional slopes of " + b, slopesB,
            GLMInteractionSectionInference::Test});
        for (auto &row : predictions) applyReferenceTest(row);
        report.sections.push_back({"Predicted means", predictions,
            report.emmReferenceTestEnabled
                ? GLMInteractionSectionInference::ReferenceTest
                : GLMInteractionSectionInference::EstimateOnly});
    }
    report.ok = report.message.empty();
    return report;
}

bool PopulateGLMInteractionPlotModel(PlotModel &plot,
                                     const PlotModel &seed,
                                     const DataFrameModel *dataframe,
                                     const std::string &dependent,
                                     const std::string &term,
                                     const std::vector<std::string> &terms,
                                     const std::map<std::string, std::string> &termTypes,
                                     const GLMFitSummary &fit,
                                     std::string *message,
                                     const std::set<std::string> &centeredPredictors)
{
    auto fail = [&](const std::string &text) {
        if (message) *message = text;
        return false;
    };
    if (!fit.ok || fit.beta.empty()) {
        return fail(fit.warning.empty() ? GLMInteractionModelNotFittedStatus() : fit.warning);
    }
    const std::vector<std::string> parts = SplitInteractionTerm(term);
    if (parts.size() != 2) return fail(GLMInteractionTwoWayOnlyStatus());
    const std::string a = BaseVariableForTermComponent(parts[0]);
    const std::string b = BaseVariableForTermComponent(parts[1]);
    const std::string aType = GLMBaseVariableType(&seed, a, termTypes);
    const std::string bType = GLMBaseVariableType(&seed, b, termTypes);
    std::string x = a;
    std::string moderator = b;
    std::string xType = aType;
    std::string moderatorType = bType;
    if (aType == "factor" && bType == "numeric") {
        x = b; moderator = a; xType = bType; moderatorType = aType;
    } else if (aType == "factor" && bType == "factor") {
        xType = "factor"; moderatorType = "factor";
    }

    const std::set<int> rows = GLMRowsUsedSet(fit);
    auto numericSummary = [&](const std::string &variable) {
        return GLMNumericSummaryForVariable(GLMDataColumn(dataframe, variable),
                                            seed.variables, variable, rows);
    };
    auto fittedCenter = [&](const std::string &variable) {
        const auto stored = fit.predictorCenters.find(variable);
        if (stored != fit.predictorCenters.end() && std::isfinite(stored->second)) {
            return stored->second;
        }
        const NumericSummary values = numericSummary(variable);
        return values.ok ? values.mean : 0.0;
    };
    auto factorLevels = [&](const std::string &variable) {
        if (const GLMFactorCoding *coding = FactorCodingForVariable(fit, variable)) {
            if (!coding->levels.empty()) return coding->levels;
        }
        std::vector<std::string> result;
        std::set<std::string> seen;
        if (const DataColumn *column = GLMDataColumn(dataframe, variable)) {
            for (std::size_t index = 0; index < column->values.size(); ++index) {
                const std::string value = DisplayValueForCell(*column, index);
                if (GLMRowIncluded(rows, static_cast<int>(index + 1)) &&
                    !DataCellIsMissing(value) && seen.insert(value).second) result.push_back(value);
            }
        } else if (const NumericVariable *variableData = GLMNumericVariable(seed, variable)) {
            for (std::size_t index = 0; index < variableData->values.size(); ++index) {
                if (!GLMRowIncluded(rows, static_cast<int>(index + 1)) ||
                    !std::isfinite(variableData->values[index])) continue;
                const std::string value = FactorLevelTermValue(variableData->values[index]);
                if (seen.insert(value).second) result.push_back(value);
            }
        }
        SortFactorLevelsLikeR(result);
        return result;
    };
    auto scenario = [&](const std::map<std::string, double> &numericValues,
                        const std::map<std::string, std::string> &factorValues,
                        std::vector<double> &design) {
        ScenarioDesignInput input;
        input.terms = terms;
        input.termTypes = termTypes;
        input.numericValues = numericValues;
        for (auto &entry : input.numericValues) {
            if (!centeredPredictors.count(entry.first)) continue;
            entry.second -= fittedCenter(entry.first);
        }
        input.factorValues = factorValues;
        input.expectedSize = fit.beta.size();
        for (const std::string &variable :
             BaseVariablesFromModelTerms(AvailableVariableNames(seed, dataframe), "", terms)) {
            ScenarioVariableInfo info;
            info.type = GLMBaseVariableType(&seed, variable, termTypes);
            if (info.type == "factor") {
                if (!ApplyFittedFactorCoding(fit, variable, info)) {
                    info.factorLevels = factorLevels(variable);
                }
            } else {
                const NumericSummary summary = numericSummary(variable);
                info.numericDefault = centeredPredictors.count(variable)
                    ? 0.0 : (summary.ok ? summary.mean : 0.0);
            }
            input.variables[variable] = std::move(info);
        }
        return BuildScenarioDesignVector(input, design);
    };
    auto addPrediction = [&](InteractionPlotLine &line, double xValue,
                             const std::map<std::string, double> &numericValues,
                             const std::map<std::string, std::string> &factorValues) {
        std::vector<double> design;
        if (!scenario(numericValues, factorValues, design)) return;
        const LinearFunctionEstimate estimate = EstimateLinearFunction(
            fit.beta, fit.covariance, design, "", 0.95);
        GLMAddInteractionLinePoint(&plot, line, xValue, estimate.estimate);
        if (std::isfinite(estimate.ciLower) && std::isfinite(estimate.ciUpper)) {
            line.confidenceLower.push_back({xValue, estimate.ciLower, 0});
            line.confidenceUpper.push_back({xValue, estimate.ciUpper, 0});
        }
    };
    auto numericAt = [&](const std::string &variable, int row, double &value) {
        if (row <= 0) return false;
        const std::size_t index = static_cast<std::size_t>(row - 1);
        if (const NumericVariable *numeric = GLMNumericVariable(seed, variable)) {
            if (index >= numeric->values.size()) return false;
            value = numeric->values[index];
            return std::isfinite(value);
        }
        if (const DataColumn *column = GLMDataColumn(dataframe, variable)) {
            if (index >= column->values.size()) return false;
            return ParseDataCellDouble(DisplayValueForCell(*column, index), value) && std::isfinite(value);
        }
        return false;
    };
    auto factorAt = [&](const std::string &variable, int row) {
        if (row <= 0) return std::string{};
        const std::size_t index = static_cast<std::size_t>(row - 1);
        if (const DataColumn *column = GLMDataColumn(dataframe, variable)) {
            if (index < column->values.size()) return DisplayValueForCell(*column, index);
        }
        if (const NumericVariable *numeric = GLMNumericVariable(seed, variable)) {
            if (index < numeric->values.size() && std::isfinite(numeric->values[index])) {
                return FactorLevelTermValue(numeric->values[index]);
            }
        }
        return std::string{};
    };

    plot.kind = "glm_interaction";
    plot.isDatasetSeed = false;
    plot.isGLMDiagnostic = false;
    plot.isGLMInteractionPlot = true;
    plot.glmModelId = "glm:" + seed.group;
    plot.glmInteractionTerm = term;
    plot.group = seed.group;
    plot.xLabel = x;
    plot.yLabel = "Predicted " + dependent;
    plot.title = "Effect: " + term;
    plot.points.clear();
    plot.interactionPlotLines.clear();
    plot.interactionXTicks.clear();
    plot.overlays.clear();

    const std::vector<std::string> xLevels = xType == "factor"
        ? factorLevels(x) : std::vector<std::string>{};
    const std::vector<std::string> moderatorLevels = moderatorType == "factor"
        ? factorLevels(moderator) : std::vector<std::string>{};
    for (std::size_t index = 0; index < xLevels.size(); ++index) {
        plot.interactionXTicks.push_back({static_cast<double>(index + 1), xLevels[index]});
    }

    const std::map<std::string, std::size_t> xIndex = GLMLevelIndex(xLevels);
    const std::map<std::string, std::size_t> moderatorIndex = GLMLevelIndex(moderatorLevels);
    for (int row : fit.rowsUsed) {
        double observed = NAN;
        if (!numericAt(dependent, row, observed)) continue;
        double px = NAN;
        if (xType == "factor") {
            const auto found = xIndex.find(factorAt(x, row));
            if (found == xIndex.end()) continue;
            px = static_cast<double>(found->second + 1);
            if (moderatorType == "factor") {
                const auto moderatorFound = moderatorIndex.find(factorAt(moderator, row));
                if (moderatorFound != moderatorIndex.end()) {
                    px += (static_cast<double>(moderatorFound->second) -
                           (static_cast<double>(moderatorLevels.size()) - 1.0) / 2.0) * 0.10;
                }
            }
        } else if (!numericAt(x, row, px)) {
            continue;
        }
        plot.points.push_back({px, observed, row});
    }

    if (xType == "numeric" && moderatorType == "factor") {
        const NumericSummary summary = numericSummary(x);
        if (!summary.ok || moderatorLevels.empty()) return fail(GLMInteractionNoFittedValuesStatus());
        double left = summary.min;
        double right = summary.max;
        if (!(right > left)) { left = summary.mean - 1.0; right = summary.mean + 1.0; }
        for (std::size_t levelIndex = 0; levelIndex < moderatorLevels.size(); ++levelIndex) {
            InteractionPlotLine line;
            line.label = moderator + " = " + moderatorLevels[levelIndex];
            line.colorKey = PaletteColorNameAtIndex(levelIndex);
            for (int row : fit.rowsUsed) {
                if (factorAt(moderator, row) == moderatorLevels[levelIndex]) {
                    line.caseIds.push_back(row);
                }
            }
            for (int step = 0; step <= 40; ++step) {
                const double value = left + (right - left) * static_cast<double>(step) / 40.0;
                addPrediction(line, value,
                    {{x, value}}, {{moderator, moderatorLevels[levelIndex]}});
            }
            plot.interactionPlotLines.push_back(std::move(line));
        }
    } else if (xType == "numeric" && moderatorType == "numeric") {
        const NumericSummary xSummary = numericSummary(x);
        const NumericSummary moderatorSummary = numericSummary(moderator);
        if (!xSummary.ok || !moderatorSummary.ok) return fail(GLMInteractionNoFittedValuesStatus());
        double left = xSummary.min;
        double right = xSummary.max;
        if (!(right > left)) { left = xSummary.mean - 1.0; right = xSummary.mean + 1.0; }
        std::vector<std::pair<std::string, double>> values{{"M", moderatorSummary.mean}};
        if (std::isfinite(moderatorSummary.sd) && moderatorSummary.sd > 0.0) {
            values = {{"M - SD", moderatorSummary.mean - moderatorSummary.sd},
                      {"M", moderatorSummary.mean},
                      {"M + SD", moderatorSummary.mean + moderatorSummary.sd}};
        }
        for (std::size_t valueIndex = 0; valueIndex < values.size(); ++valueIndex) {
            InteractionPlotLine line;
            line.label = moderator + " = " + values[valueIndex].first;
            line.colorKey = PaletteColorNameAtIndex(valueIndex);
            for (int row : fit.rowsUsed) {
                double observedModerator = NAN;
                if (!numericAt(moderator, row, observedModerator)) continue;
                std::size_t nearest = 0;
                double nearestDistance = std::numeric_limits<double>::infinity();
                for (std::size_t candidate = 0; candidate < values.size(); ++candidate) {
                    const double distance = std::fabs(observedModerator - values[candidate].second);
                    if (distance < nearestDistance) {
                        nearestDistance = distance;
                        nearest = candidate;
                    }
                }
                if (nearest == valueIndex) line.caseIds.push_back(row);
            }
            for (int step = 0; step <= 40; ++step) {
                const double value = left + (right - left) * static_cast<double>(step) / 40.0;
                addPrediction(line, value,
                    {{x, value}, {moderator, values[valueIndex].second}}, {});
            }
            plot.interactionPlotLines.push_back(std::move(line));
        }
    } else if (xType == "factor" && moderatorType == "factor") {
        if (xLevels.empty() || moderatorLevels.empty()) return fail(GLMInteractionFactorNoLevelsStatus());
        for (std::size_t levelIndex = 0; levelIndex < moderatorLevels.size(); ++levelIndex) {
            InteractionPlotLine line;
            line.label = moderator + " = " + moderatorLevels[levelIndex];
            line.colorKey = PaletteColorNameAtIndex(levelIndex);
            for (int row : fit.rowsUsed) {
                if (factorAt(moderator, row) == moderatorLevels[levelIndex]) {
                    line.caseIds.push_back(row);
                }
            }
            for (std::size_t categoryIndex = 0; categoryIndex < xLevels.size(); ++categoryIndex) {
                addPrediction(line, static_cast<double>(categoryIndex + 1), {},
                    {{x, xLevels[categoryIndex]},
                     {moderator, moderatorLevels[levelIndex]}});
            }
            plot.interactionPlotLines.push_back(std::move(line));
        }
    } else {
        return fail(GLMInteractionTypeNotAvailableStatus());
    }
    if (plot.points.empty() || plot.interactionPlotLines.empty()) {
        return fail(GLMInteractionNoFiniteValuesStatus());
    }
    ComputeRanges(plot);
    if (message) message->clear();
    return true;
}

namespace {

std::vector<std::string> PairwiseObservedFactorLevels(const PlotModel &model,
                                                       const DataFrameModel *dataframe,
                                                       const std::string &variable,
                                                       const std::set<int> &rows)
{
    std::vector<std::string> levels;
    std::set<std::string> seen;
    if (const DataColumn *col = GLMDataColumn(dataframe, variable)) {
        for (std::size_t i = 0; i < col->values.size(); ++i) {
            const std::string value = DisplayValueForCell(*col, i);
            if (GLMRowIncluded(rows, (int)i + 1) && !DataCellIsMissing(value) &&
                seen.insert(value).second) {
                levels.push_back(value);
            }
        }
    } else if (const NumericVariable *var = GLMNumericVariable(model, variable)) {
        for (std::size_t i = 0; i < var->values.size(); ++i) {
            if (!GLMRowIncluded(rows, (int)i + 1) || !std::isfinite(var->values[i])) continue;
            const std::string level = FactorLevelTermValue(var->values[i]);
            if (!level.empty() && seen.insert(level).second) levels.push_back(level);
        }
    }
    SortFactorLevelsLikeR(levels);
    return levels;
}

bool PairwiseRequestUsesSupportedAdjustment(const std::string &adjustment)
{
    return adjustment.empty() || adjustment == "tukey" || adjustment == "Tukey" ||
        adjustment == "games-howell" || adjustment == "classical-tukey";
}

} // namespace

GLMPairwiseComparisonEligibility EvaluateGLMPairwiseComparisonEligibility(
    const PlotModel &model,
    const DataFrameModel *dataframe,
    const std::string &dependent,
    const std::vector<std::string> &terms,
    const std::map<std::string, std::string> &termTypes,
    const GLMFitSummary &fit,
    const GLMPairwiseComparisonRequest &request)
{
    GLMPairwiseComparisonEligibility eligibility;
    eligibility.term = request.term;
    if (!PairwiseRequestUsesSupportedAdjustment(request.adjustment)) {
        eligibility.message = "The requested pairwise adjustment is not supported.";
        return eligibility;
    }
    if (request.term.empty() || request.term == "(Intercept)" || IsInteractionTerm(request.term) ||
        request.term != BaseVariableForTermComponent(request.term) ||
        request.term.find('(') != std::string::npos || request.term.find(')') != std::string::npos ||
        std::find(terms.begin(), terms.end(), request.term) == terms.end()) {
        eligibility.message = "Pairwise comparisons are available only for included simple categorical main effects.";
        return eligibility;
    }
    const std::vector<std::string> available = AvailableVariableNames(model, dataframe);
    if (!ModelTermExistsForVariables(available, request.term, dependent)) {
        eligibility.message = "The selected categorical term is no longer available in the dataset.";
        return eligibility;
    }
    if (GLMBaseVariableType(&model, request.term, termTypes) != "factor") {
        eligibility.message = "Pairwise comparisons are available only for categorical terms.";
        return eligibility;
    }
    if (!fit.ok || fit.beta.empty() || fit.covariance.empty()) {
        eligibility.message = fit.warning.empty() ? "Fit the model before requesting pairwise comparisons." : fit.warning;
        return eligibility;
    }
    eligibility.levels = PairwiseObservedFactorLevels(model, dataframe, request.term, GLMRowsUsedSet(fit));
    if (eligibility.levels.size() < 2) {
        eligibility.message = "The selected categorical term has fewer than two observed fitted categories.";
        return eligibility;
    }
    eligibility.eligible = true;
    return eligibility;
}

std::string GLMPairwiseComparisonsRScript()
{
    return R"RPAIRWISE(
args <- commandArgs(TRUE)
if (length(args) < 6L) stop("Expected CSV, response, selected term, confidence level, and model terms.", call. = FALSE)
csv <- args[[1L]]
response <- args[[2L]]
selected_term <- args[[3L]]
confidence_level <- suppressWarnings(as.numeric(args[[4L]]))
if (!is.finite(confidence_level) || confidence_level <= 0 || confidence_level >= 1) confidence_level <- 0.95
term_count <- suppressWarnings(as.integer(args[[5L]]))
if (is.na(term_count) || term_count < 1L) stop("The current model has no terms.", call. = FALSE)
term_start <- 6L
terms <- args[seq.int(term_start, term_start + term_count - 1L)]
variable_count_index <- term_start + term_count
variable_count <- suppressWarnings(as.integer(args[[variable_count_index]]))
if (is.na(variable_count) || variable_count < 1L) stop("The current model has no variables.", call. = FALSE)
variable_start <- variable_count_index + 1L
variables <- args[seq.int(variable_start, variable_start + variable_count - 1L)]
types <- args[seq.int(variable_start + variable_count, variable_start + 2L * variable_count - 1L)]
if (length(types) != length(variables)) stop("Invalid variable-type payload.", call. = FALSE)
adjustment_index <- variable_start + 2L * variable_count
requested_adjustment <- if (length(args) >= adjustment_index) args[[adjustment_index]] else "auto"
bt <- function(x) paste0("`", gsub("`", "``", x, fixed = TRUE), "`")
term_expr <- function(x) {
  if (grepl(":", x, fixed = TRUE)) return(paste(vapply(strsplit(x, ":", fixed = TRUE)[[1L]], term_expr, character(1L)), collapse = ":"))
  if (grepl("^[[:alnum:]_. ]+$", x)) bt(x) else x
}

# Keep printable level labels intact.  This R source is embedded in a C++ raw
# string, so the control-character escapes must reach R with one backslash.
# Using doubled backslashes makes TRE treat t/r/n as members of the character
# class and silently removes those letters from labels such as "Europe".
clean <- function(x) gsub("[\t\r\n]", " ", as.character(x))
safe_num <- function(x) if (length(x) != 1L || is.na(x) || !is.finite(x)) "NA" else sprintf("%.17g", x)
dat <- utils::read.csv(csv, check.names = FALSE, stringsAsFactors = FALSE, na.strings = "NA")
if (!response %in% names(dat) || !selected_term %in% names(dat)) stop("The selected term or response is no longer available in the dataset.", call. = FALSE)
for (i in seq_along(variables)) {
  if (identical(types[[i]], "factor") && variables[[i]] %in% names(dat)) dat[[variables[[i]]]] <- factor(dat[[variables[[i]]]])
}
if (!is.factor(dat[[selected_term]])) dat[[selected_term]] <- factor(dat[[selected_term]])
contrast_levels <- function(label) {
  levels <- levels(dat[[selected_term]])
  for (first in levels) {
    prefix <- paste0(first, " - ")
    if (startsWith(label, prefix)) {
      second <- substr(label, nchar(prefix) + 1L, nchar(label))
      if (second %in% levels) return(c(first, second))
    }
  }
  c(NA_character_, NA_character_)
}
if (length(terms) == 1L && identical(terms[[1L]], selected_term)) {
  one_way_formula <- stats::as.formula(paste(bt(response), "~", bt(selected_term)))
  if (identical(requested_adjustment, "classical-tukey")) {
    if (!requireNamespace("emmeans", quietly = TRUE)) stop("Tukey comparisons require the R package 'emmeans'.", call. = FALSE)
    fit <- stats::lm(one_way_formula, data = dat)
    tab <- as.data.frame(summary(emmeans::contrast(emmeans::emmeans(fit, specs = selected_term), method = "pairwise", adjust = "tukey"), infer = c(TRUE, TRUE), level = confidence_level))
    if (!nrow(tab)) stop("No estimable Tukey comparisons could be formed for the selected term.", call. = FALSE)
    cat("OK\n"); cat("METHOD\tR: stats::lm + emmeans Tukey comparisons\tTukey (emmeans)\n")
    for (i in seq_len(nrow(tab))) {
      contrast <- as.character(tab$contrast[[i]]); levels <- contrast_levels(contrast)
      cat("ROW", clean(contrast), clean(levels[[1L]]), clean(levels[[2L]]), safe_num(tab$estimate[[i]]), safe_num(tab$SE[[i]]),
          safe_num(tab$df[[i]]), safe_num(tab$t.ratio[[i]]), safe_num(tab$p.value[[i]]),
          safe_num(tab$lower.CL[[i]]), safe_num(tab$upper.CL[[i]]), sep = "\t"); cat("\n")
    }
  } else {
    if (!requireNamespace("rstatix", quietly = TRUE)) stop("Games-Howell pairwise comparisons require the R package 'rstatix'.", call. = FALSE)
    tab <- as.data.frame(rstatix::games_howell_test(dat, one_way_formula, conf.level = confidence_level, detailed = TRUE))
    if (!nrow(tab)) stop("No estimable Games-Howell comparisons could be formed for the selected term.", call. = FALSE)
    cat("OK\n")
    cat("METHOD\tR: rstatix::games_howell_test() for the one-factor model\tGames-Howell\n")
    for (i in seq_len(nrow(tab))) {
      first <- as.character(tab$group1[[i]])
      second <- as.character(tab$group2[[i]])
      cat("ROW", clean(paste(first, "-", second)), clean(first), clean(second), safe_num(tab$estimate[[i]]), safe_num(tab$se[[i]]),
          safe_num(tab$df[[i]]), safe_num(tab$statistic[[i]]), safe_num(tab$p.adj[[i]]),
          safe_num(tab$conf.low[[i]]), safe_num(tab$conf.high[[i]]), sep = "\t")
      cat("\n")
    }
  }
} else {
  if (!requireNamespace("emmeans", quietly = TRUE)) stop("Model-adjusted pairwise comparisons require the R package 'emmeans'.", call. = FALSE)
  rhs <- paste(vapply(terms, term_expr, character(1L)), collapse = " + ")
  form <- stats::as.formula(paste(bt(response), "~", rhs))
  variance_form <- stats::as.formula(paste("~ 1 |", bt(selected_term)))
  fit <- nlme::gls(form, data = dat, weights = nlme::varIdent(form = variance_form), method = "REML", na.action = stats::na.omit)
  model_data <- stats::model.frame(form, data = dat, na.action = stats::na.omit)
  if (nlevels(droplevels(model_data[[selected_term]])) < 2L) stop("The selected categorical term has fewer than two fitted categories.", call. = FALSE)
  emm <- emmeans::emmeans(fit, specs = selected_term)
  pairwise <- emmeans::contrast(emm, method = "pairwise", adjust = "tukey")
  tab <- as.data.frame(summary(pairwise, infer = c(TRUE, TRUE), level = confidence_level))
  if (!nrow(tab)) stop("No estimable pairwise comparisons could be formed for the selected term.", call. = FALSE)
  cat("OK\n")
  cat("METHOD\tR: nlme::gls with varIdent by selected categorical predictor; emmeans pairwise EMMs\tTukey (emmeans)\n")
  for (i in seq_len(nrow(tab))) {
    contrast <- as.character(tab$contrast[[i]])
    levels <- contrast_levels(contrast)
    cat("ROW", clean(contrast), clean(levels[[1L]]), clean(levels[[2L]]), safe_num(tab$estimate[[i]]), safe_num(tab$SE[[i]]),
        safe_num(tab$df[[i]]), safe_num(tab$t.ratio[[i]]), safe_num(tab$p.value[[i]]),
        safe_num(tab$lower.CL[[i]]), safe_num(tab$upper.CL[[i]]), sep = "\t")
    cat("\n")
  }
}
)RPAIRWISE";
}

std::string BinaryPairwiseComparisonsRScript()
{
    return R"RBINARYPAIRWISE(
args <- commandArgs(TRUE)
if (length(args) < 9L) stop("Invalid Binary Model pairwise request.", call. = FALSE)
csv <- args[[1L]]; response <- args[[2L]]; event <- args[[3L]]; link <- args[[4L]]; selected_term <- args[[5L]]
confidence <- suppressWarnings(as.numeric(args[[6L]])); if (!is.finite(confidence)) confidence <- 0.95
term_count <- suppressWarnings(as.integer(args[[7L]])); cursor <- 8L
terms <- if (term_count > 0L) args[seq.int(cursor, cursor + term_count - 1L)] else character(); cursor <- cursor + term_count
variable_count <- suppressWarnings(as.integer(args[[cursor]])); cursor <- cursor + 1L
variables <- if (variable_count > 0L) args[seq.int(cursor, cursor + variable_count - 1L)] else character(); cursor <- cursor + variable_count
types <- if (variable_count > 0L) args[seq.int(cursor, cursor + variable_count - 1L)] else character()
if (!requireNamespace("emmeans", quietly = TRUE)) stop("Binary pairwise comparisons require the R package 'emmeans'.", call. = FALSE)
dat <- utils::read.csv(csv, check.names = FALSE, stringsAsFactors = FALSE, na.strings = "NA")
if (!response %in% names(dat) || !selected_term %in% names(dat)) stop("Response or selected term is unavailable.", call. = FALSE)
for (i in seq_along(variables)) if (identical(types[[i]], "factor") && variables[[i]] %in% names(dat)) dat[[variables[[i]]]] <- factor(dat[[variables[[i]]]])
dat[[selected_term]] <- factor(dat[[selected_term]])
original <- as.character(dat[[response]])
if (!event %in% original) stop("The selected event is not observed in the fitted rows.", call. = FALSE)
temporary <- ".rls_binary_response"; while (temporary %in% names(dat)) temporary <- paste0(temporary, "_")
dat[[temporary]] <- as.integer(original == event)
bt <- function(x) paste0("`", gsub("`", "``", x, fixed = TRUE), "`")
term_expr <- function(x) { if (grepl(":", x, fixed = TRUE)) return(paste(vapply(strsplit(x, ":", fixed = TRUE)[[1L]], term_expr, character(1L)), collapse = ":")); bt(x) }
rhs <- if (length(terms)) paste(vapply(terms, term_expr, character(1L)), collapse = " + ") else "1"
fit <- stats::glm(stats::as.formula(paste(bt(temporary), "~", rhs)), data = dat, family = stats::binomial(link = link))
grid <- emmeans::emmeans(fit, specs = selected_term)
link_grid <- grid
scale <- if (identical(link, "logit")) "odds ratio" else "probability difference"
if (identical(link, "probit")) grid <- emmeans::regrid(grid, transform = "response")
tab <- as.data.frame(summary(emmeans::contrast(grid, method = "pairwise", adjust = "tukey"), infer = c(TRUE, TRUE), level = confidence))
if (!nrow(tab)) stop("No estimable pairwise comparisons were returned.", call. = FALSE)
clean <- function(x) gsub("[\t\r\n]", " ", as.character(x)); safe <- function(x) if (length(x) != 1L || !is.finite(x)) "NA" else sprintf("%.17g", x)
levels_for <- function(label) { lev <- levels(dat[[selected_term]]); for (first in lev) { prefix <- paste0(first, " - "); if (startsWith(label, prefix)) { second <- substr(label, nchar(prefix) + 1L, nchar(label)); if (second %in% lev) return(c(first, second)) } }; c("", "") }
cat("OK\n"); cat("METHOD", paste0("R: stats::glm binomial(", link, ") + emmeans; ", scale), "Tukey (emmeans)", sep = "\t"); cat("\n")
for (i in seq_len(nrow(tab))) {
  contrast <- as.character(tab$contrast[[i]]); lev <- levels_for(contrast)
  estimate <- tab$estimate[[i]]; lower <- tab[[intersect(c("lower.CL","asymp.LCL"),names(tab))[[1L]]]][[i]]; upper <- tab[[intersect(c("upper.CL","asymp.UCL"),names(tab))[[1L]]]][[i]]
  standard_error <- tab$SE[[i]]
  if (identical(link, "logit")) {
    estimate <- exp(estimate)
    standard_error <- abs(estimate) * standard_error
    lower <- exp(lower)
    upper <- exp(upper)
  }
  statistic <- if ("z.ratio" %in% names(tab)) tab$z.ratio[[i]] else if ("t.ratio" %in% names(tab)) tab$t.ratio[[i]] else NA_real_
  df <- if ("df" %in% names(tab)) tab$df[[i]] else NA_real_
  cat("ROW", clean(contrast), clean(lev[[1L]]), clean(lev[[2L]]), safe(estimate), safe(standard_error), safe(df), safe(statistic), safe(tab$p.value[[i]]), safe(lower), safe(upper), sep = "\t"); cat("\n")
}
# Transport descriptive response means and the original link-grid inference.
contrast <- emmeans::contrast(link_grid, method='pairwise', adjust='tukey')
details <- LinkEDA:::.rls_emmeans_standardize_table(summary(contrast,infer=c(TRUE,TRUE),level=confidence))
details <- LinkEDA:::.rls_pairwise_scale_details(details,link_grid,contrast,confidence)
details$term <- selected_term; details$adjustment <- 'tukey'
details$imputation_count <- 1L; details$pooling_method <- 'emmeans'
attr(details,'confidence_level') <- confidence
payload <- LinkEDA:::.rls_pairwise_native_payload(details)
writeLines(payload[seq.int(match('PAIRWISE_SCALES_V1',payload),length(payload))])
)RBINARYPAIRWISE";
}

bool ParseGLMPairwiseComparisonsRResult(const std::vector<std::string> &lines,
                                        GLMPairwiseComparisonResult &result)
{
    result.comparisons.clear();
    result.ok = false;
    result.message.clear();
    result.method.clear();
    result.adjustment = "tukey";
    result.provenance = {};
    result.scaleReport = {}; result.scaleHeaders.clear(); result.scaleRows.clear();
    if (lines.empty() || lines.front() != "OK") {
        result.message = "R did not return pairwise-comparison results.";
        return false;
    }
    for (std::size_t i = 1; i < lines.size(); ++i) {
        if (lines[i] == "ANALYSIS_PROVENANCE_V2") {
            std::size_t cursor=i;
            if(!ReadAnalysisProvenancePayload(lines,cursor,result.provenance,&result.message))return false;
            break;
        }
        if (lines[i] == "PAIRWISE_SCALES_V1") {
            if (++i >= lines.size()) return false;
            char *end=nullptr;const long count=std::strtol(lines[i].c_str(),&end,10);
            if (end==lines[i].c_str() || *end || count<0 || static_cast<std::size_t>(count)>lines.size()-i-1) return false;
            std::vector<std::string> report(lines.begin()+i+1,lines.begin()+i+1+count);
            if (!ParseGLMInteractionReportText(report,result.scaleReport)) return false;
            auto heading=std::find(report.begin(),report.end(),"Pairwise comparisons");
            if (heading==report.end() || ++heading==report.end()) return false;
            result.scaleHeaders=SplitTabs(*heading++);
            for(;heading!=report.end() && !heading->empty();++heading) {
                auto row=SplitTabs(*heading);
                if(row.size()!=result.scaleHeaders.size()) return false;
                result.scaleRows.push_back(std::move(row));
            }
            i+=count; continue;
        }
        const std::vector<std::string> fields = SplitTabs(lines[i]);
        if (fields.empty()) continue;
        if (fields[0] == "METHOD" && fields.size() >= 2) {
            result.method = fields[1];
            if (fields.size() >= 3) result.adjustment = fields[2];
        } else if (fields[0] == "ROW" && fields.size() >= 11) {
            LinearFunctionEstimate row;
            row.label = fields[1];
            row.factorName = result.term;
            row.firstLevel = fields[2];
            row.secondLevel = fields[3];
            row.comparisonId = result.term + "\x1f" + row.firstLevel + "\x1f" + row.secondLevel;
            row.estimate = ParseOptionalDataCellDouble(fields[4]);
            row.stdError = ParseOptionalDataCellDouble(fields[5]);
            row.degreesOfFreedom = ParseOptionalDataCellDouble(fields[6]);
            row.statistic = ParseOptionalDataCellDouble(fields[7]);
            row.pValue = ParseOptionalDataCellDouble(fields[8]);
            row.adjustedPValue = row.pValue;
            row.ciLower = ParseOptionalDataCellDouble(fields[9]);
            row.ciUpper = ParseOptionalDataCellDouble(fields[10]);
            result.comparisons.push_back(std::move(row));
        }
    }
    if (result.comparisons.empty()) {
        result.message = "R returned no estimable pairwise comparisons.";
        return false;
    }
    result.ok = true;
    return true;
}

std::string GLMPairwiseComparisonsText(const GLMPairwiseComparisonResult &result)
{
    std::ostringstream out;
    if(result.scaleReport.ok) return GLMInteractionReportText(result.scaleReport);
    out << "Pairwise comparisons: " << result.term << "\n";
    out << "Estimated marginal means calculated in R\n";
    out << "Method: " << (result.method.empty() ? "R heteroscedastic GLS model" : result.method) << "\n";
    out << "Adjustment: " << (result.adjustment.empty() ? "Tukey" : result.adjustment) << "\n";
    out << "\n";
    if (!result.ok) {
        out << (result.message.empty() ? "Pairwise comparisons are not available." : result.message) << "\n";
        return out.str();
    }
    out << "Comparison\tEstimate\tSE\tdf\tt\tp ("
        << (result.adjustment.empty() ? "Tukey" : result.adjustment) << ")\tLower 95%\tUpper 95%\n";
    for (const LinearFunctionEstimate &row : result.comparisons) {
        out << row.label << "\t" << FormatDoubleOrDash(row.estimate, 4) << "\t"
            << FormatDoubleOrDash(row.stdError, 4) << "\t"
            << FormatDoubleOrDash(row.degreesOfFreedom, 1) << "\t"
            << FormatDoubleOrDash(row.statistic, 3) << "\t"
            << (std::isfinite(row.adjustedPValue) ? FormatPValue(row.adjustedPValue) : "—") << "\t"
            << FormatDoubleOrDash(row.ciLower, 4) << "\t" << FormatDoubleOrDash(row.ciUpper, 4) << "\n";
    }
    out << "\nNote: for a one-factor model R uses Games-Howell. Otherwise, R fits a GLS model with a separate residual variance for each category of the selected categorical predictor; emmeans obtains marginal means while holding numeric covariates at their fitted-sample means and averaging over other categorical terms.\n";
    return out.str();
}

PublicationTableSpec PairwiseScalePublicationTable(const GLMPairwiseComparisonResult &result)
{
    PublicationTableSpec table; table.title="Pairwise comparisons: "+(result.term.empty()?result.scaleReport.term:result.term);
    const auto comparison=std::find(result.scaleHeaders.begin(),result.scaleHeaders.end(),"Comparison");
    table.stubColumns={comparison==result.scaleHeaders.end()?0:static_cast<std::size_t>(comparison-result.scaleHeaders.begin())};table.footnotes=result.scaleReport.notes;
    if(!result.scaleReport.multipleComparisons.empty()) table.footnotes.insert(table.footnotes.begin(),"Multiple comparisons: "+result.scaleReport.multipleComparisons);
    for(std::size_t j=0;j<result.scaleHeaders.size();++j)
        table.columns.push_back({"column_"+std::to_string(j),result.scaleHeaders[j],"",-1,false});
    for(const auto &row:result.scaleRows) {
        std::vector<PublicationValue> values;
        for(std::size_t j=0;j<row.size();++j) {
            PublicationValue v; v.kind=PublicationValueKind::Text; v.text=row[j];
            char *end=nullptr; const double number=std::strtod(row[j].c_str(),&end);
            if(j>0 && end!=row[j].c_str() && *end=='\0' && std::isfinite(number)) {
                v.kind=PublicationValueKind::Number;v.number=number;
                if(result.scaleHeaders[j]=="df" && std::fabs(number)>=1e6) {
                    std::ostringstream compact;compact<<std::scientific<<std::setprecision(2)<<number;
                    v.kind=PublicationValueKind::Text;v.text=compact.str();
                }
                table.columns[j].decimals=4;
                table.columns[j].pValue=result.scaleHeaders[j]=="Adjusted p" || result.scaleHeaders[j]=="p";
            }
            values.push_back(v);
        }
        table.rows.push_back(std::move(values));
    }
    return table;
}

std::vector<PublicationTableSpec> InteractionScalePublicationTables(const GLMInteractionReport &report)
{
    // Reuse the shared report serialization: native code transports R's values.
    std::istringstream input(GLMInteractionReportText(report));
    std::vector<PublicationTableSpec> tables;
    std::string line, title;
    GLMPairwiseComparisonResult current;
    auto flush=[&] {
        if(current.scaleHeaders.empty()) return;
        auto table=PairwiseScalePublicationTable(current);table.title=title;
        tables.push_back(std::move(table));current=GLMPairwiseComparisonResult{};
    };
    while(std::getline(input,line)) {
        if(line.empty()) {flush();continue;}
        const auto fields=SplitTabs(line);
        if(std::find(fields.begin(),fields.end(),"SE")!=fields.end()) {
            current.scaleHeaders=fields;
        } else if(!current.scaleHeaders.empty() && fields.size()==current.scaleHeaders.size()) {
            current.scaleRows.push_back(fields);
        } else if(fields.size()==1) title=line;
    }
    flush();
    if(!tables.empty()) tables.back().footnotes=report.notes;
    return tables;
}

OutputCodeReference PairwiseDiagnosticsCodeReference(
    const std::string &outputId, const GLMPairwiseComparisonResult &result)
{
    OutputCodeReference output;
    if(result.provenance.missingInformationRows.empty() &&
       !result.provenance.verificationRCode.count("pairwise_scales") &&
       result.provenance.verificationRCode.find("pairwise") == result.provenance.verificationRCode.end()) return output;
    output.outputId=outputId;output.analysisId=result.provenance.analysisId;
    output.outputBlockId=result.provenance.verificationRCode.count("pairwise_scales") ? "pairwise_scales" : "pairwise";output.kind="table";
    output.title="Pairwise comparisons — "+result.term;
    output.provenance=result.provenance;
    if(result.scaleReport.ok) {
        output.publication.table=PairwiseScalePublicationTable(result);
        output.publication.availableBackends={PublicationBackend::Tinytable,PublicationBackend::Latex,PublicationBackend::LatexPdf};
    }
    return output;
}

bool ParseRegressionInteractionPlotRResult(
    const std::vector<std::string> &lines,
    RegressionInteractionPlotResult &result)
{
    result = RegressionInteractionPlotResult();
    if (lines.empty() || lines.front() != "OK") {
        result.message = "R did not return pooled effect-plot data.";
        return false;
    }
    std::map<std::string, std::size_t> seriesByLabel;
    for (std::size_t i = 1; i < lines.size(); ++i) {
        if (lines[i] == "ANALYSIS_PROVENANCE_V2" || lines[i] == "ANALYSIS_PROVENANCE_V1") {
            if (!ReadAnalysisProvenancePayload(lines, i, result.provenance, &result.message)) return false;
            --i;
            continue;
        }
        const std::vector<std::string> fields = SplitTabs(lines[i]);
        if (fields.empty()) continue;
        if (fields[0] == "META" && fields.size() >= 6) {
            result.term = fields[1];
            result.interactionType = fields[2];
            result.focal = fields[3];
            result.moderator = fields[4];
            const double count = ParseOptionalDataCellDouble(fields[5]);
            if (std::isfinite(count) && count >= 1.0) result.imputationCount = static_cast<int>(count);
            for (std::size_t field = 6; field < fields.size(); ++field) {
                if (!fields[field].empty()) result.conditioning.push_back(fields[field]);
            }
            if (result.conditioning.empty() && !result.moderator.empty()) {
                result.conditioning.push_back(result.moderator);
            }
        } else if (fields[0] == "CONFIDENCE" && fields.size() >= 2) {
            const double level = ParseOptionalDataCellDouble(fields[1]);
            if (std::isfinite(level) && level > 0.0 && level < 1.0) {
                result.confidenceLevel = level;
            }
        } else if (fields[0] == "BINARY" && fields.size() >= 5) {
            result.binaryProbability = true;
            result.eventLabel = fields[1];
            result.quantity = fields[2];
            result.adjustmentMode = fields[3];
            result.presentation = fields[4];
        } else if (fields[0] == "BOUNDED" && fields.size() >= 4) {
            result.boundedCount = true;
            result.quantity = fields[1];
            result.trialsVariable = fields[2];
            result.trialsConstant = ParseOptionalDataCellDouble(fields[3]);
            result.ceilingHurdle = fields.size() >= 5 && fields[4] == "1";
        } else if (fields[0] == "TICK" && fields.size() >= 3) {
            const double x = ParseOptionalDataCellDouble(fields[1]);
            if (std::isfinite(x)) result.xTicks.emplace_back(x, fields[2]);
        } else if (fields[0] == "ROW" && fields.size() >= 6) {
            const double x = ParseOptionalDataCellDouble(fields[2]);
            const double y = ParseOptionalDataCellDouble(fields[3]);
            if (!std::isfinite(x) || !std::isfinite(y)) continue;
            const std::string &label = fields[1];
            auto found = seriesByLabel.find(label);
            if (found == seriesByLabel.end()) {
                const std::size_t index = result.lines.size();
                seriesByLabel[label] = index;
                InteractionPlotLine line;
                line.label = label;
                line.colorKey = PaletteColorNameAtIndex(index);
                result.lines.push_back(std::move(line));
                found = seriesByLabel.find(label);
            }
            result.lines[found->second].points.push_back({x, y, 0});
            if (fields.size() >= 6) {
                const double lower = ParseOptionalDataCellDouble(fields[4]);
                const double upper = ParseOptionalDataCellDouble(fields[5]);
                if (std::isfinite(lower) && std::isfinite(upper)) {
                    result.lines[found->second].confidenceLower.push_back({x, lower, 0});
                    result.lines[found->second].confidenceUpper.push_back({x, upper, 0});
                }
            }
        }
    }
    for (InteractionPlotLine &line : result.lines) {
        std::sort(line.points.begin(), line.points.end(), [](const DataPoint &left, const DataPoint &right) {
            return left.x < right.x;
        });
        std::sort(line.confidenceLower.begin(), line.confidenceLower.end(), [](const DataPoint &left, const DataPoint &right) {
            return left.x < right.x;
        });
        std::sort(line.confidenceUpper.begin(), line.confidenceUpper.end(), [](const DataPoint &left, const DataPoint &right) {
            return left.x < right.x;
        });
    }
    if (result.lines.empty()) {
        result.message = "R returned no finite pooled fitted values for the effect plot.";
        return false;
    }
    result.ok = true;
    return true;
}

void ApplyRegressionInteractionPresentation(
    PlotModel &plot,
    const RegressionInteractionPlotResult &result,
    const std::string &responseLabel)
{
    plot.regressionConfidenceLevel = result.confidenceLevel;
    plot.regressionBinaryProbability = result.binaryProbability;
    plot.regressionBoundedCount = result.boundedCount;
    plot.regressionCeilingHurdle = result.ceilingHurdle;
    plot.regressionTrialsVariable = result.trialsVariable;
    plot.regressionTrialsConstant = result.trialsConstant;
    plot.regressionEffectEvent = result.eventLabel;
    plot.regressionEffectQuantity = result.quantity;
    plot.regressionEffectAdjustment = result.adjustmentMode;
    plot.regressionEffectPresentation = result.presentation;
    plot.interactionFocalVariable = result.focal;
    plot.xLabel = result.focal;
    plot.presentationXLabel.clear();
    plot.presentationYLabel.clear();
    plot.title = RegressionEffectPlotTitle(result);
    plot.subtitle = result.imputationCount > 1
        ? "Pooled across " + std::to_string(result.imputationCount) + " imputations"
        : std::string();
    plot.interactionLegendTitle.clear();
    for (const std::string &variable : result.conditioning) {
        if (!plot.interactionLegendTitle.empty()) plot.interactionLegendTitle += " × ";
        plot.interactionLegendTitle += variable;
    }
    if (plot.interactionLegendTitle.empty()) plot.interactionLegendTitle = result.moderator;
    if (plot.interactionLegendTitle.empty()) plot.interactionLegendTitle = result.focal;
    plot.interactionLegendDefaultTitle = plot.interactionLegendTitle;
    if (!plot.interactionLegendTitleOverride.empty())
        plot.interactionLegendTitle = plot.interactionLegendTitleOverride;
    if (result.binaryProbability && result.quantity == "probability_difference")
        plot.xLabel += " contrast";
    if (result.conditioning.size() == 1) {
        const std::string prefix = result.conditioning.front() + " = ";
        for (InteractionPlotLine &line : plot.interactionPlotLines) {
            if (line.label.rfind(prefix, 0) == 0)
                line.label.erase(0, prefix.size());
        }
    }
    if (result.boundedCount) {
        if (result.quantity == "perfect_score_probability") {
            plot.yLabel = "Perfect score probability";
        } else if (result.quantity == "conditional_expected_score") {
            plot.yLabel = "Expected score below ceiling";
        } else if (result.quantity == "overall_expected_score") {
            plot.yLabel = "Overall expected score";
        } else {
            plot.yLabel = result.quantity == "expected_count"
                ? "Expected count" : "Predicted probability";
        }
        if (result.quantity != "perfect_score_probability" &&
            result.quantity != "predicted_probability" &&
            std::isfinite(result.trialsConstant)) {
            plot.yLabel += " (out of " +
                FormatDoubleOrDash(result.trialsConstant, 0) + ")";
        }
        return;
    }
    if (!result.binaryProbability) {
        plot.yLabel = (result.imputationCount > 1 ? "Pooled fitted " : "Fitted ") +
            responseLabel;
        return;
    }
    const bool difference = result.quantity == "probability_difference";
    const bool percentage = result.presentation == "percentage";
    const std::string event = result.eventLabel.empty() ? "event" : result.eventLabel;
    if (difference) {
        plot.yLabel = "Probability difference for " + event +
            (percentage ? " (percentage points)" : "");
    } else {
        plot.yLabel = "Predicted probability of " + event +
            (percentage ? " (%)" : "");
    }
}

void ApplyRegressionInteractionVariableLabels(
    PlotModel &plot,
    const RegressionInteractionPlotResult &result,
    const DataFrameModel &dataframe,
    const std::string &responseVariable)
{
    const auto labelFor = [&](const std::string &name) -> std::string {
        for (const DataColumn &column : dataframe.columns) {
            if (column.name != name) continue;
            if (!column.displayName.empty()) return column.displayName;
            break;
        }
        return name;
    };
    const auto replaceVariable = [](std::string &text,
                                    const std::string &raw,
                                    const std::string &label) {
        if (raw.empty() || raw == label) return;
        std::size_t offset = 0;
        while ((offset = text.find(raw, offset)) != std::string::npos) {
            const auto identifierCharacter = [](char character) {
                return std::isalnum(static_cast<unsigned char>(character)) || character == '_';
            };
            if ((offset > 0 && identifierCharacter(text[offset - 1])) ||
                (offset + raw.size() < text.size() &&
                 identifierCharacter(text[offset + raw.size()]))) {
                offset += raw.size();
                continue;
            }
            text.replace(offset, raw.size(), label);
            offset += label.size();
        }
    };
    plot.presentationXLabel = labelFor(result.focal);
    if (result.binaryProbability && result.quantity == "probability_difference")
        plot.presentationXLabel += " contrast";
    replaceVariable(plot.title, result.focal, labelFor(result.focal));
    for (const std::string &variable : result.conditioning)
        replaceVariable(plot.title, variable, labelFor(variable));
    plot.presentationYLabel = plot.yLabel;
    replaceVariable(plot.presentationYLabel, responseVariable, labelFor(responseVariable));
    {
        plot.interactionLegendDefaultTitle.clear();
        for (const std::string &variable : result.conditioning) {
            if (!plot.interactionLegendDefaultTitle.empty()) plot.interactionLegendDefaultTitle += " × ";
            plot.interactionLegendDefaultTitle += labelFor(variable);
        }
        if (plot.interactionLegendDefaultTitle.empty() && !result.moderator.empty())
            plot.interactionLegendDefaultTitle = labelFor(result.moderator);
        if (plot.interactionLegendDefaultTitle.empty())
            plot.interactionLegendDefaultTitle = labelFor(result.focal);
        plot.interactionLegendTitle = plot.interactionLegendTitleOverride.empty()
            ? plot.interactionLegendDefaultTitle : plot.interactionLegendTitleOverride;
    }
    for (InteractionPlotLine &line : plot.interactionPlotLines) {
        for (const std::string &variable : result.conditioning) {
            const std::string prefix = variable + " = ";
            if (line.label.rfind(prefix, 0) == 0)
                line.label.replace(0, variable.size(), labelFor(variable));
        }
    }
}

std::string RegressionEffectPlotTitle(
    const RegressionInteractionPlotResult &result)
{
    std::ostringstream out;
    if (result.boundedCount) {
        const std::string quantityLabel =
            result.quantity == "perfect_score_probability" ? "Perfect score probability" :
            result.quantity == "conditional_expected_score" ? "Expected score below ceiling" :
            result.quantity == "overall_expected_score" ? "Overall expected score" :
            result.quantity == "expected_count" ? "Adjusted expected count" :
            "Adjusted predicted probability";
        out << quantityLabel
            << " by " << result.focal;
        if (!result.conditioning.empty()) {
            out << ", by ";
            for (std::size_t index = 0; index < result.conditioning.size(); ++index) {
                if (index) out << " × ";
                out << result.conditioning[index];
            }
        }
    } else if (result.binaryProbability) {
        const std::string event = result.eventLabel.empty()
            ? "event" : result.eventLabel;
        if (result.quantity == "probability_difference") {
            if (result.xTicks.size() == 1 && !result.xTicks.front().second.empty())
                out << result.xTicks.front().second << " difference in P(" << event << ")";
            else
                out << "Differences in P(" << event << ") by " << result.focal;
        } else {
            out << "Predicted P(" << event << ") by " << result.focal;
        }
        if (!result.conditioning.empty()) {
            out << ", by ";
            for (std::size_t index = 0; index < result.conditioning.size(); ++index) {
                if (index) out << " × ";
                out << result.conditioning[index];
            }
        }
    } else {
        out << "Estimated marginal means: " << result.focal;
        for (const std::string &variable : result.conditioning)
            out << " × " << variable;
    }
    return out.str();
}

void ApplyRegressionInteractionAxisRange(PlotModel &plot)
{
    if (plot.regressionBoundedCount) {
        // Effect plots are comparisons of fitted values, so forcing every
        // bounded-count plot to the complete 0..Trials scale can make a real
        // adjusted difference look perfectly flat (for example, 21.9 versus
        // 22.2 successes out of 24).  ComputeRanges() has already obtained an
        // R-derived fitted-value range.  Add a modest visual margin and clamp
        // it to the response domain instead of replacing it with the domain.
        double lowerBound = 0.0;
        const bool probabilityScale =
            plot.regressionEffectQuantity == "predicted_probability" ||
            plot.regressionEffectQuantity == "perfect_score_probability";
        double upperBound = probabilityScale
            ? 1.0 : plot.regressionTrialsConstant;
        if (!std::isfinite(upperBound) || upperBound <= lowerBound) {
            upperBound = std::max(plot.ymax, lowerBound + 1.0);
        }
        double minimum = std::max(lowerBound, plot.ymin);
        double maximum = std::min(upperBound, plot.ymax);
        if (!(maximum > minimum)) {
            const double centre = std::clamp(
                std::isfinite(plot.ymin) ? plot.ymin : lowerBound,
                lowerBound, upperBound);
            const double halfSpan = std::max((upperBound - lowerBound) * 0.025, 0.05);
            minimum = std::max(lowerBound, centre - halfSpan);
            maximum = std::min(upperBound, centre + halfSpan);
        } else {
            const double padding = std::max((maximum - minimum) * 0.08,
                                            (upperBound - lowerBound) * 0.005);
            minimum = std::max(lowerBound, minimum - padding);
            maximum = std::min(upperBound, maximum + padding);
        }
        plot.ymin = minimum;
        plot.ymax = maximum;
        return;
    }
    if (!plot.regressionBinaryProbability) return;
    if (plot.regressionEffectQuantity == "probability_difference") {
        plot.ymin = std::min(plot.ymin, 0.0);
        plot.ymax = std::max(plot.ymax, 0.0);
        if (!(plot.ymax > plot.ymin)) {
            const double extent = plot.regressionEffectPresentation == "percentage"
                ? 1.0 : 0.01;
            plot.ymin = -extent;
            plot.ymax = extent;
        }
        return;
    }
    plot.ymin = 0.0;
    plot.ymax = plot.regressionEffectPresentation == "percentage" ? 100.0 : 1.0;
}

static std::string BinaryEffectVerificationRCode(
    const PlotModel &plot,
    const RegressionInteractionPlotResult &result,
    const std::string &sourceRecipe)
{
    const bool categoricalAxis = !result.xTicks.empty() ||
        result.quantity == "probability_difference";
    std::vector<double> focalValues;
    if (!categoricalAxis) {
        for (const InteractionPlotLine &line : result.lines) {
            for (const DataPoint &point : line.points) {
                if (std::isfinite(point.x) &&
                    std::find(focalValues.begin(), focalValues.end(), point.x) == focalValues.end()) {
                    focalValues.push_back(point.x);
                }
            }
        }
        std::sort(focalValues.begin(), focalValues.end());
    }
    std::ostringstream code;
    code << sourceRecipe;
    if (!sourceRecipe.empty() && sourceRecipe.back() != '\n') code << '\n';
    code
        << "if (!requireNamespace(\"marginaleffects\", quietly = TRUE))\n"
           "  stop(\"Install package 'marginaleffects' to run this check.\")\n"
           "if (!requireNamespace(\"ggplot2\", quietly = TRUE))\n"
           "  stop(\"Install package 'ggplot2' to run this check.\")\n"
        << "linkeda_plot_theme <- "
        << Ggplot2ThemeRExpression(plot.rExportTheme) << "\n"
        <<
           "grid_fits <- if (exists(\"reference_fits\", inherits = FALSE)) reference_fits else list(reference_model)\n"
        << "focal_variable <- " << ProvenanceRStringLiteral(result.focal) << "\n"
        << "conditioning_variables <- " << VerificationRCharacterVector(result.conditioning) << "\n"
        << "effect_quantity <- " << ProvenanceRStringLiteral(result.quantity) << "\n"
        << "adjustment_mode <- " << ProvenanceRStringLiteral(result.adjustmentMode) << "\n"
        << "presentation <- " << ProvenanceRStringLiteral(result.presentation) << "\n"
        << "confidence_level <- " << std::setprecision(17) << result.confidenceLevel << "\n"
        << "event_label <- " << ProvenanceRStringLiteral(result.eventLabel) << "\n"
           "first_frame <- stats::model.frame(grid_fits[[1L]])\n"
           "value_grid <- function(variable, points = 3L) {\n"
           "  value <- first_frame[[variable]]\n"
           "  if (is.factor(value)) return(levels(value))\n"
           "  observed <- unlist(lapply(grid_fits, function(fit)\n"
           "    as.numeric(stats::model.frame(fit)[[variable]])), use.names = FALSE)\n"
           "  observed <- observed[is.finite(observed)]\n"
           "  if (points > 3L) return(unique(as.numeric(stats::quantile(\n"
           "    observed, seq(0, 1, length.out = points), na.rm = TRUE, names = FALSE))))\n"
           "  centre <- mean(observed); spread <- stats::sd(observed)\n"
           "  if (!is.finite(spread) || spread <= 0) spread <- diff(range(observed)) / 2\n"
           "  unique(pmax(min(observed), pmin(max(observed), c(centre - spread, centre, centre + spread))))\n"
           "}\n"
           "scenario_values <- setNames(vector(\"list\", 1L + length(conditioning_variables)),\n"
           "                            c(focal_variable, conditioning_variables))\n";
    if (categoricalAxis && result.quantity != "probability_difference") {
        code << "scenario_values[[focal_variable]] <- levels(first_frame[[focal_variable]])\n";
    } else if (!categoricalAxis) {
        code << "scenario_values[[focal_variable]] <- c(";
        for (std::size_t index = 0; index < focalValues.size(); ++index) {
            if (index) code << ", ";
            code << std::setprecision(17) << focalValues[index];
        }
        code << ")\n";
    } else {
        code << "scenario_values[[focal_variable]] <- value_grid(focal_variable)\n";
    }
    code
        << "for (variable in conditioning_variables)\n"
           "  scenario_values[[variable]] <- value_grid(variable)\n"
           "reference_profile <- function(fit) {\n"
           "  frame <- stats::model.frame(fit)\n"
           "  response <- names(frame)[attr(stats::terms(fit), \"response\")]\n"
           "  predictors <- frame[setdiff(names(frame), response)]\n"
           "  lapply(predictors, function(value) {\n"
           "    if (is.factor(value)) levels(value)[1L] else if (is.numeric(value))\n"
           "      mean(value, na.rm = TRUE) else if (is.logical(value)) FALSE else\n"
           "      as.character(value[which(!is.na(value))[1L]])\n"
           "  })\n"
           "}\n"
           "effect_for_fit <- function(fit) {\n"
           "  values <- scenario_values\n"
           "  if (identical(effect_quantity, \"probability_difference\"))\n"
           "    values[[focal_variable]] <- NULL\n"
           "  grid_args <- list(model = fit, grid_type = if (identical(\n"
           "    adjustment_mode, \"average_sample\")) \"counterfactual\" else \"mean_or_mode\")\n"
           "  if (identical(adjustment_mode, \"reference_profile\"))\n"
           "    grid_args <- c(grid_args, reference_profile(fit))\n"
           "  grid_args[names(values)] <- values\n"
           "  prediction_data <- do.call(marginaleffects::datagrid, grid_args)\n"
           "  if (identical(effect_quantity, \"predicted_probability\")) {\n"
           "    out <- marginaleffects::avg_predictions(\n"
           "      fit, newdata = prediction_data, by = c(focal_variable, conditioning_variables),\n"
           "      type = \"response\", conf_level = confidence_level)\n"
           "  } else {\n"
           "    focal_values <- scenario_values[[focal_variable]]\n"
           "    contrast <- if (is.factor(first_frame[[focal_variable]])) \"pairwise\" else\n"
           "      c(min(focal_values), max(focal_values))\n"
           "    out <- marginaleffects::avg_comparisons(\n"
           "      fit, variables = stats::setNames(list(contrast), focal_variable),\n"
           "      newdata = prediction_data, by = conditioning_variables,\n"
           "      type = \"response\", conf_level = confidence_level)\n"
           "  }\n"
           "  as.data.frame(out)\n"
           "}\n"
           "per_imputation <- lapply(grid_fits, effect_for_fit)\n"
           "key_variables <- if (identical(effect_quantity, \"predicted_probability\"))\n"
           "  c(focal_variable, conditioning_variables) else c(\"contrast\", conditioning_variables)\n"
           "signatures <- lapply(per_imputation, function(data)\n"
           "  do.call(paste, c(data[key_variables], sep = \"\\r\")))\n"
           "if (length(signatures) > 1L && !all(vapply(signatures[-1L], identical, logical(1L), signatures[[1L]])))\n"
           "  stop(\"Effect grids differ across imputations.\")\n"
           "if (length(grid_fits) > 1L && !requireNamespace(\"mice\", quietly = TRUE))\n"
           "  stop(\"Install package 'mice' to pool multiple-imputation effects.\")\n"
           "pool_row <- function(row) {\n"
           "  estimates <- vapply(per_imputation, function(data) data$estimate[[row]], numeric(1L))\n"
           "  variances <- vapply(per_imputation, function(data) data$std.error[[row]]^2, numeric(1L))\n"
           "  if (length(estimates) == 1L) return(list(qbar = estimates, t = variances, df = Inf))\n"
           "  mice::pool.scalar(estimates, variances,\n"
           "    n = min(vapply(grid_fits, stats::nobs, numeric(1L))),\n"
           "    k = max(vapply(grid_fits, function(fit) length(stats::coef(fit)), numeric(1L))))\n"
           "}\n"
           "pooled <- lapply(seq_len(nrow(per_imputation[[1L]])), pool_row)\n"
           "reference_plot_data <- per_imputation[[1L]][key_variables]\n"
           "reference_plot_data$.estimate <- vapply(pooled, `[[`, numeric(1L), \"qbar\")\n"
           "reference_plot_data$.se <- sqrt(vapply(pooled, `[[`, numeric(1L), \"t\"))\n"
           "reference_plot_data$.df <- vapply(pooled, `[[`, numeric(1L), \"df\")\n"
           "critical <- ifelse(is.finite(reference_plot_data$.df),\n"
           "  stats::qt((1 + confidence_level) / 2, reference_plot_data$.df),\n"
           "  stats::qnorm((1 + confidence_level) / 2))\n"
           "if (identical(effect_quantity, \"predicted_probability\")) {\n"
           "  logit_se <- reference_plot_data$.se /\n"
           "    (reference_plot_data$.estimate * (1 - reference_plot_data$.estimate))\n"
           "  reference_plot_data$.lower <- stats::plogis(stats::qlogis(reference_plot_data$.estimate) - critical * logit_se)\n"
           "  reference_plot_data$.upper <- stats::plogis(stats::qlogis(reference_plot_data$.estimate) + critical * logit_se)\n"
           "} else {\n"
           "  reference_plot_data$.lower <- reference_plot_data$.estimate - critical * reference_plot_data$.se\n"
           "  reference_plot_data$.upper <- reference_plot_data$.estimate + critical * reference_plot_data$.se\n"
           "}\n"
           "display_multiplier <- if (identical(presentation, \"percentage\")) 100 else 1\n"
           "reference_plot_data[c(\".estimate\", \".lower\", \".upper\")] <-\n"
           "  reference_plot_data[c(\".estimate\", \".lower\", \".upper\")] * display_multiplier\n"
           "x_variable <- if (identical(effect_quantity, \"probability_difference\")) \"contrast\" else focal_variable\n"
           "effect_legend_labels <- function(values) {\n"
           "  if (!is.numeric(values)) return(as.character(values))\n"
           "  trim_fraction <- function(x) {\n"
           "    x <- sub(\"(\\\\.[0-9]*?)0+$\", \"\\\\1\", x)\n"
           "    x <- sub(\"\\\\.$\", \"\", x)\n"
           "    x[x %in% c(\"-0\", \"+0\")] <- \"0\"\n"
           "    x\n"
           "  }\n"
           "  finite <- is.finite(values); out <- as.character(values)\n"
           "  distinct_values <- length(unique(values[finite]))\n"
           "  for (digits in 2:6) {\n"
           "    candidate <- trim_fraction(formatC(values[finite], format = \"f\", digits = digits))\n"
           "    if (length(unique(candidate)) == distinct_values) { out[finite] <- candidate; return(out) }\n"
           "  }\n"
           "  for (digits in 4:10) {\n"
           "    candidate <- formatC(values[finite], format = \"g\", digits = digits)\n"
           "    if (length(unique(candidate)) == distinct_values) { out[finite] <- candidate; return(out) }\n"
           "  }\n"
           "  out\n"
           "}\n"
           "if (length(conditioning_variables)) {\n"
           "  series_data <- lapply(reference_plot_data[conditioning_variables], effect_legend_labels)\n"
           "  reference_plot_data$.series <- interaction(as.data.frame(series_data, check.names = FALSE), drop = TRUE, sep = \" × \" )\n"
           "  reference_plot <- ggplot2::ggplot(reference_plot_data, ggplot2::aes(\n"
           "    x = .data[[x_variable]], y = .estimate, colour = .series, group = .series))\n"
           "} else {\n"
           "  reference_plot <- ggplot2::ggplot(reference_plot_data, ggplot2::aes(\n"
           "    x = .data[[x_variable]], y = .estimate, group = 1))\n"
           "}\n"
        << "show_confidence_intervals <- " << (plot.regressionConfidenceIntervalsVisible ? "TRUE" : "FALSE") << "\n"
        << "connect_estimates <- " << (plot.regressionConnectEstimates ? "TRUE" : "FALSE") << "\n"
           "categorical_axis <- is.factor(first_frame[[focal_variable]]) ||\n"
           "  identical(effect_quantity, \"probability_difference\")\n"
           "if (show_confidence_intervals && categorical_axis)\n"
           "  reference_plot <- reference_plot + ggplot2::geom_errorbar(\n"
           "    ggplot2::aes(ymin = .lower, ymax = .upper), width = .10)\n"
           "if (show_confidence_intervals && !categorical_axis)\n"
           "  reference_plot <- reference_plot + ggplot2::geom_ribbon(\n"
           "    ggplot2::aes(ymin = .lower, ymax = .upper, fill = if (length(conditioning_variables)) .series else NULL),\n"
           "    alpha = .16, colour = NA)\n"
           "if (connect_estimates) reference_plot <- reference_plot + ggplot2::geom_line(linewidth = .8)\n"
           "reference_plot <- reference_plot + ggplot2::geom_point(size = 2.2)\n"
           "if (identical(effect_quantity, \"probability_difference\"))\n"
           "  reference_plot <- reference_plot + ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = \"grey45\")\n"
           "if (identical(effect_quantity, \"predicted_probability\"))\n"
           "  reference_plot <- reference_plot + ggplot2::coord_cartesian(ylim = if (display_multiplier == 100) c(0, 100) else c(0, 1))\n"
        << "legend_title <- " << (plot.interactionLegendTitleVisible
                ? ProvenanceRStringLiteral(plot.interactionLegendTitle) : "NULL") << "\n"
           "reference_plot <- reference_plot + ggplot2::labs(\n"
        << "  title = " << ProvenanceRStringLiteral(plot.title)
        << ", x = " << ProvenanceRStringLiteral(plot.xLabel)
        << ", y = " << ProvenanceRStringLiteral(plot.yLabel)
        << ", colour = legend_title, fill = legend_title) + linkeda_plot_theme\n"
           "print(reference_plot)\n";
    return code.str();
}

static std::string CeilingHurdleEffectVerificationRCode(
    const PlotModel &plot,
    const RegressionInteractionPlotResult &result,
    const std::string &sourceRecipe)
{
    const bool categoricalAxis = !result.xTicks.empty();
    std::vector<double> focalValues;
    if (!categoricalAxis) {
        for (const InteractionPlotLine &line : result.lines) {
            for (const DataPoint &point : line.points) {
                if (std::isfinite(point.x) &&
                    std::find(focalValues.begin(), focalValues.end(), point.x) == focalValues.end()) {
                    focalValues.push_back(point.x);
                }
            }
        }
        std::sort(focalValues.begin(), focalValues.end());
    }
    std::ostringstream code;
    code << sourceRecipe;
    if (!sourceRecipe.empty() && sourceRecipe.back() != '\n') code << '\n';
    code
        << "# Recalculate the selected response-scale effect from the public ZABB fit.\n"
           "if (!requireNamespace(\"ggplot2\", quietly = TRUE))\n"
           "  stop(\"Install package 'ggplot2' to draw this check.\")\n"
        << "linkeda_plot_theme <- " << Ggplot2ThemeRExpression(plot.rExportTheme) << "\n"
           "effect_fits <- if (exists(\"fits\", inherits = FALSE)) fits else list(fit)\n"
           "effect_data <- if (exists(\"completed_sets\", inherits = FALSE)) completed_sets else list(data)\n"
        << "focal_variable <- " << ProvenanceRStringLiteral(result.focal) << "\n"
        << "conditioning_variables <- " << VerificationRCharacterVector(result.conditioning) << "\n"
        << "effect_quantity <- " << ProvenanceRStringLiteral(result.quantity) << "\n"
        << "trials_variable <- " << ProvenanceRStringLiteral(result.trialsVariable) << "\n"
        << "trials_constant <- ";
    if (std::isfinite(result.trialsConstant)) code << std::setprecision(17) << result.trialsConstant;
    else code << "NA_real_";
    code << "\n"
        << "confidence_level <- " << std::setprecision(17) << result.confidenceLevel << "\n"
           "parts <- c(focal_variable, conditioning_variables)\n"
           "first_data <- effect_data[[1L]]\n"
           "grid_values <- lapply(parts, function(variable) {\n"
           "  value <- first_data[[variable]]\n"
           "  if (is.factor(value)) return(levels(value))\n"
           "  observed <- unlist(lapply(effect_data, function(data) as.numeric(data[[variable]])),\n"
           "                     use.names = FALSE)\n"
           "  observed <- observed[is.finite(observed)]\n"
           "  if (identical(variable, focal_variable)) return(c(";
    for (std::size_t index = 0; index < focalValues.size(); ++index) {
        if (index) code << ", ";
        code << std::setprecision(17) << focalValues[index];
    }
    code << "))\n"
           "  centre <- mean(observed); spread <- stats::sd(observed)\n"
           "  if (!is.finite(spread) || spread <= 0) spread <- diff(range(observed)) / 2\n"
           "  unique(pmax(min(observed), pmin(max(observed),\n"
           "    c(centre - spread, centre, centre + spread))))\n"
           "})\n"
           "names(grid_values) <- parts\n"
           "effect_grid <- do.call(expand.grid, c(grid_values, KEEP.OUT.ATTRS = FALSE,\n"
           "  stringsAsFactors = FALSE))\n"
           "for (variable in parts) if (is.factor(first_data[[variable]]))\n"
           "  effect_grid[[variable]] <- factor(effect_grid[[variable]],\n"
           "    levels = levels(first_data[[variable]]), ordered = is.ordered(first_data[[variable]]))\n"
           "conditional_score <- function(trials, eta_mu, eta_sigma) {\n"
           "  mu <- stats::plogis(eta_mu); sigma <- exp(eta_sigma)\n"
           "  vapply(seq_along(trials), function(index) {\n"
           "    n <- as.integer(trials[[index]])\n"
           "    p_zero <- gamlss.dist::dBB(0, mu = mu[[index]], sigma = sigma[[index]], bd = n)\n"
           "    failures <- seq_len(n)\n"
           "    n - sum(failures * gamlss.dist::dBB(failures, mu = mu[[index]],\n"
           "      sigma = sigma[[index]], bd = n)) / (1 - p_zero)\n"
           "  }, numeric(1L))\n"
           "}\n"
           "effect_for_fit <- function(model, model_data) {\n"
           "  design_terms <- stats::delete.response(stats::terms(model_formula))\n"
           "  mu_beta <- unname(model$mu.coefficients)\n"
           "  sigma_beta <- unname(model$sigma.coefficients)\n"
           "  nu_beta <- unname(model$nu.coefficients)\n"
           "  covariance_model <- model; covariance_model$call$data <- NULL\n"
           "  covariance <- as.matrix(stats::vcov(covariance_model))\n"
           "  n_mu <- length(mu_beta); n_sigma <- length(sigma_beta); n_nu <- length(nu_beta)\n"
           "  evaluate_scenario <- function(index) {\n"
           "    scenario <- model_data\n"
           "    for (variable in parts) {\n"
           "      replacement <- effect_grid[[variable]][[index]]\n"
           "      if (is.factor(model_data[[variable]])) replacement <- factor(as.character(replacement),\n"
           "        levels = levels(model_data[[variable]]), ordered = is.ordered(model_data[[variable]]))\n"
           "      scenario[[variable]] <- rep(replacement, nrow(scenario))\n"
           "    }\n"
           "    design <- stats::model.matrix(design_terms, data = scenario)\n"
           "    eta_mu <- as.numeric(design %*% mu_beta)\n"
           "    eta_nu <- as.numeric(design %*% nu_beta)\n"
           "    eta_sigma <- rep(sigma_beta[[1L]], nrow(scenario))\n"
           "    trials <- if (nzchar(trials_variable)) as.numeric(scenario[[trials_variable]])\n"
           "      else rep(trials_constant, nrow(scenario))\n"
           "    perfect <- stats::plogis(eta_nu)\n"
           "    conditional <- conditional_score(trials, eta_mu, eta_sigma)\n"
           "    estimate <- switch(effect_quantity,\n"
           "      perfect_score_probability = perfect,\n"
           "      conditional_expected_score = conditional,\n"
           "      overall_expected_score = perfect * trials + (1 - perfect) * conditional)\n"
           "    step <- 1e-5\n"
           "    derivative_mu <- (conditional_score(trials, eta_mu + step, eta_sigma) -\n"
           "      conditional_score(trials, eta_mu - step, eta_sigma)) / (2 * step)\n"
           "    derivative_sigma <- (conditional_score(trials, eta_mu, eta_sigma + step) -\n"
           "      conditional_score(trials, eta_mu, eta_sigma - step)) / (2 * step)\n"
           "    derivative_nu <- perfect * (1 - perfect)\n"
           "    weight_mu <- switch(effect_quantity, perfect_score_probability = 0,\n"
           "      conditional_expected_score = derivative_mu,\n"
           "      overall_expected_score = (1 - perfect) * derivative_mu)\n"
           "    weight_sigma <- switch(effect_quantity, perfect_score_probability = 0,\n"
           "      conditional_expected_score = derivative_sigma,\n"
           "      overall_expected_score = (1 - perfect) * derivative_sigma)\n"
           "    weight_nu <- switch(effect_quantity, perfect_score_probability = derivative_nu,\n"
           "      conditional_expected_score = 0,\n"
           "      overall_expected_score = derivative_nu * (trials - conditional))\n"
           "    gradient <- c(colMeans(design * weight_mu),\n"
           "      c(mean(weight_sigma), rep(0, max(0L, n_sigma - 1L))),\n"
           "      colMeans(design * weight_nu))\n"
           "    c(estimate = mean(estimate),\n"
           "      variance = max(0, as.numeric(crossprod(gradient, covariance %*% gradient))))\n"
           "  }\n"
           "  t(vapply(seq_len(nrow(effect_grid)), evaluate_scenario, numeric(2L)))\n"
           "}\n"
           "per_imputation <- Map(effect_for_fit, effect_fits, effect_data)\n"
           "if (length(effect_fits) > 1L && !requireNamespace(\"mice\", quietly = TRUE))\n"
           "  stop(\"Install package 'mice' to pool multiple-imputation effects.\")\n"
           "pool_row <- function(index) {\n"
           "  estimates <- vapply(per_imputation, function(value) value[index, \"estimate\"], numeric(1L))\n"
           "  variances <- vapply(per_imputation, function(value) value[index, \"variance\"], numeric(1L))\n"
           "  if (length(estimates) == 1L) return(c(estimate = estimates, se = sqrt(variances), df = Inf))\n"
           "  pooled <- mice::pool.scalar(estimates, variances, n = Inf, k = 1L, rule = \"rubin1987\")\n"
           "  c(estimate = pooled$qbar, se = sqrt(pooled$t), df = pooled$df)\n"
           "}\n"
           "pooled <- as.data.frame(t(vapply(seq_len(nrow(effect_grid)), pool_row, numeric(3L))))\n"
           "reference_plot_data <- cbind(effect_grid, pooled)\n"
           "critical <- ifelse(is.finite(reference_plot_data$df),\n"
           "  stats::qt((1 + confidence_level) / 2, reference_plot_data$df),\n"
           "  stats::qnorm((1 + confidence_level) / 2))\n"
           "reference_plot_data$.estimate <- reference_plot_data$estimate\n"
           "reference_plot_data$.lower <- reference_plot_data$estimate - critical * reference_plot_data$se\n"
           "reference_plot_data$.upper <- reference_plot_data$estimate + critical * reference_plot_data$se\n"
           "if (identical(effect_quantity, \"perfect_score_probability\")) {\n"
           "  reference_plot_data$.lower <- pmax(0, reference_plot_data$.lower)\n"
           "  reference_plot_data$.upper <- pmin(1, reference_plot_data$.upper)\n"
           "}\n"
           "if (length(conditioning_variables)) {\n"
           "  reference_plot_data$.series <- interaction(reference_plot_data[conditioning_variables],\n"
           "    drop = TRUE, sep = \" × \")\n"
           "  reference_plot <- ggplot2::ggplot(reference_plot_data, ggplot2::aes(\n"
           "    x = .data[[focal_variable]], y = .estimate, colour = .series, group = .series))\n"
           "} else reference_plot <- ggplot2::ggplot(reference_plot_data, ggplot2::aes(\n"
           "  x = .data[[focal_variable]], y = .estimate, group = 1))\n"
        << "show_confidence_intervals <- "
        << (plot.regressionConfidenceIntervalsVisible ? "TRUE" : "FALSE") << "\n"
        << "connect_estimates <- " << (plot.regressionConnectEstimates ? "TRUE" : "FALSE") << "\n";
    if (categoricalAxis) {
        code << "category_dodge <- ggplot2::position_dodge(width = .35)\n"
                "if (show_confidence_intervals) reference_plot <- reference_plot +\n"
                "  ggplot2::geom_errorbar(ggplot2::aes(ymin = .lower, ymax = .upper),\n"
                "    width = .10, position = category_dodge)\n"
                "if (connect_estimates) reference_plot <- reference_plot +\n"
                "  ggplot2::geom_line(linewidth = .8, position = category_dodge)\n"
                "reference_plot <- reference_plot + ggplot2::geom_point(size = 2.2,\n"
                "  position = category_dodge)\n";
    } else {
        code << "if (show_confidence_intervals) reference_plot <- reference_plot +\n"
                "  ggplot2::geom_ribbon(ggplot2::aes(ymin = .lower, ymax = .upper,\n"
                "    fill = if (length(conditioning_variables)) .series else NULL),\n"
                "    alpha = .16, colour = NA)\n"
                "if (connect_estimates) reference_plot <- reference_plot + ggplot2::geom_line(linewidth = .8)\n"
                "reference_plot <- reference_plot + ggplot2::geom_point(size = 2.2)\n";
    }
    code << "legend_title <- " << (plot.interactionLegendTitleVisible
                ? ProvenanceRStringLiteral(plot.interactionLegendTitle) : "NULL") << "\n"
            "reference_plot <- reference_plot + ggplot2::labs(title = "
         << ProvenanceRStringLiteral(plot.title) << ", x = "
         << ProvenanceRStringLiteral(plot.xLabel) << ", y = "
         << ProvenanceRStringLiteral(plot.yLabel)
         << ", colour = legend_title, fill = legend_title) + linkeda_plot_theme\n"
            "print(reference_plot)\n";
    return code.str();
}

OutputCodeReference RegressionInteractionReportCodeReference(
    const std::string &outputId, const GLMInteractionReport &report,
    const OutputCodeReference &sourceModel)
{
    OutputCodeReference output;
    const auto capturedRecipe = report.provenance.verificationRCode.find("table");
    if (capturedRecipe != report.provenance.verificationRCode.end() && !capturedRecipe->second.empty()) {
        output.analysisId = report.provenance.analysisId;
        output.provenance = report.provenance;
        output.outputId = outputId;
        output.outputBlockId = "interaction_report:" + report.term;
        output.title = "Effects: " + report.term;
        output.kind = "table";
        output.provenance.verificationRCode[output.outputBlockId] = capturedRecipe->second;
        output.provenance.outputRCode[output.outputBlockId] = "reference";
        return output;
    }
    const auto factors = GLMInteractionGroupingFactors(report);
    const auto recipe = sourceModel.provenance.verificationRCode.find(sourceModel.outputBlockId);
    if (factors.size() != 2 || report.binaryProbability || report.boundedCount || report.ceilingHurdle ||
        recipe == sourceModel.provenance.verificationRCode.end()) return output;
    output.analysisId = sourceModel.analysisId;
    output.provenance = sourceModel.provenance;
    output.outputId = outputId;
    output.outputBlockId = "interaction_report:" + report.term;
    output.title = "Interaction: " + report.term;
    output.kind = "table";
    std::string adjustment = "tukey";
    std::istringstream words(report.multipleComparisons);
    for (std::string word; words >> word;) {
        if (word == "tukey" || word == "holm" || word == "bonferroni" ||
            word == "sidak" || word == "none") adjustment = word;
    }
    std::ostringstream code;
    code << recipe->second << "\n"
         << "# Recalculate the selected comparison direction from the same fitted model(s).\n"
         << "comparison_factor <- " << ProvenanceRStringLiteral(factors[0]) << "\n"
         << "grouping_factor <- " << ProvenanceRStringLiteral(factors[1]) << "\n"
         << "confidence_level <- " << std::setprecision(17) << report.confidenceLevel << "\n"
         << "adjustment <- " << ProvenanceRStringLiteral(adjustment) << "\n"
         << "emm_reference <- " << (report.emmReferenceTestEnabled
             ? FormatDoubleOrDash(report.emmTestReference, 15) : "NULL") << "\n"
         << R"R(grid_fits <- if (exists("reference_fits", inherits = FALSE)) reference_fits else
  if (exists("fits", inherits = FALSE)) fits else list(reference_model)
emm_target <- if (length(grid_fits) > 1L) mice::as.mira(grid_fits) else grid_fits[[1L]]
emm_grid <- emmeans::emmeans(emm_target, specs = comparison_factor, by = grouping_factor)
print(summary(emm_grid, infer = c(TRUE, !is.null(emm_reference)),
  null = if (is.null(emm_reference)) 0 else emm_reference, level = confidence_level,
  type = if (inherits(grid_fits[[1L]], "glm")) "response" else "link"))
print(summary(emmeans::contrast(emm_grid, method = "pairwise", adjust = adjustment),
  infer = c(TRUE, TRUE), level = confidence_level))
# Differences of simple contrasts use each fitted model's full covariance matrix.
interaction_tables <- lapply(grid_fits, function(fit) {
  grid <- emmeans::emmeans(fit, specs = comparison_factor, by = grouping_factor)
  simple <- emmeans::contrast(grid, method = "pairwise", adjust = "none")
  as.data.frame(summary(emmeans::contrast(simple, method = "pairwise", by = "contrast",
    adjust = "none"), infer = c(TRUE, TRUE), level = confidence_level))
})
if (length(grid_fits) == 1L) {
  print(interaction_tables[[1L]])
} else {
  interaction_checks <- lapply(seq_len(nrow(interaction_tables[[1L]])), function(i) {
    q <- vapply(interaction_tables, function(x) x$estimate[i], numeric(1L))
    u <- vapply(interaction_tables, function(x) x$SE[i]^2, numeric(1L))
    dfs <- vapply(interaction_tables, function(x) x$df[i], numeric(1L))
    dfcom <- if (any(is.finite(dfs) & dfs > 0)) min(dfs[is.finite(dfs) & dfs > 0]) else Inf
    pooled <- mice::pool.scalar(q, u, n = dfcom + 1, k = 1)
    data.frame(interaction_tables[[1L]][i, 1:2, drop = FALSE],
      estimate = pooled$qbar, SE = sqrt(pooled$t), df = pooled$df,
      p = 2 * stats::pt(-abs(pooled$qbar / sqrt(pooled$t)), df = pooled$df))
  })
  print(do.call(rbind, interaction_checks))
}
)R";
    output.provenance.verificationRCode[output.outputBlockId] = code.str();
    output.provenance.outputRCode[output.outputBlockId] = "emm_grid";
    return output;
}

OutputCodeReference RegressionInteractionPlotCodeReference(
    const PlotModel &plot,
    const RegressionInteractionPlotResult &result,
    const OutputCodeReference &sourceModel)
{
    OutputCodeReference output;
    output.outputId = plot.id;
    output.analysisId = sourceModel.analysisId;
    output.outputBlockId = "interaction_plot:" + result.term;
    output.title = plot.title;
    output.kind = "plot";
    output.provenance = sourceModel.provenance;
    const bool simpleEffect = result.term.find(':') == std::string::npos;
    std::ostringstream block;
    if (result.ceilingHurdle) {
        block << "# Exact response-scale effect from the fitted gamlss.dist::ZABB model.\n"
              << "# Show R Code refits the public-package model, derives this quantity in R,\n"
              << "# and pools the imputation-specific estimates with mice::pool.scalar().\n"
              << "effect_quantity <- " << ProvenanceRStringLiteral(result.quantity) << "\n"
              << "confidence_level <- " << std::setprecision(17)
              << result.confidenceLevel << "\n";
    } else if (result.binaryProbability) {
        block << "# Recalculate adjusted probabilities or probability differences from the fitted model.\n"
              << "# See Show R Code for the complete marginaleffects recipe, including MI pooling.\n"
              << "effect_quantity <- " << ProvenanceRStringLiteral(result.quantity) << "\n"
              << "adjustment_mode <- " << ProvenanceRStringLiteral(result.adjustmentMode) << "\n"
              << "confidence_level <- " << std::setprecision(17) << result.confidenceLevel << "\n";
    } else {
        block << "fitted_target <- if (exists('model', inherits = FALSE)) model else mice::as.mira(fits)\n"
          << "post_estimation_grid <- emmeans::emmeans(\n"
          << "  fitted_target, specs = "
          << ProvenanceRStringLiteral(result.focal) << ", by = "
          << VerificationRCharacterVector(result.conditioning) << "\n"
          << ")\n"
          << "plot_values <- as.data.frame(summary(\n"
          << "  post_estimation_grid, infer = c(TRUE, TRUE), level = "
          << std::setprecision(17) << result.confidenceLevel << "\n"
          << (result.boundedCount ? ", type = \"response\"\n" : "")
          << "))\n";
        if (result.boundedCount && result.quantity == "expected_count" &&
            std::isfinite(result.trialsConstant)) {
            block << "scale_columns <- intersect(\n"
                  << "  c(\"response\", \"prob\", \"rate\", \"emmean\", \"SE\", \"lower.CL\", \"upper.CL\", \"asymp.LCL\", \"asymp.UCL\"),\n"
                  << "  names(plot_values)\n"
                  << ")\n"
                  << "plot_values[scale_columns] <- lapply(\n"
                  << "  plot_values[scale_columns], function(value) value * "
                  << std::setprecision(17) << result.trialsConstant << "\n"
                  << ")\n";
        }
    }
    output.provenance.outputRCode[output.outputBlockId] = block.str();

    const auto sourceRecipe =
        output.provenance.verificationRCode.find(sourceModel.outputBlockId);
    const auto sharedRecipe = result.provenance.verificationRCode.find("plot");
    const bool hasSharedRecipe = sharedRecipe != result.provenance.verificationRCode.end() &&
                                 !sharedRecipe->second.empty();
    if (hasSharedRecipe || (sourceRecipe != output.provenance.verificationRCode.end() &&
        !sourceRecipe->second.empty())) {
        if (result.ceilingHurdle) {
            output.provenance.verificationRCode[output.outputBlockId] =
                CeilingHurdleEffectVerificationRCode(plot, result, sourceRecipe->second);
        } else if (result.binaryProbability) {
            output.provenance.verificationRCode[output.outputBlockId] =
                BinaryEffectVerificationRCode(plot, result, sourceRecipe->second);
        } else {
        std::ostringstream verification;
        const bool categoricalAxis = !result.xTicks.empty();
        if (hasSharedRecipe) {
            // R captured the exact statistical recipe with the result. Native
            // code adds display instructions only; never reconstruct its model.
            output.provenance = result.provenance;
            output.analysisId = result.provenance.analysisId;
            output.provenance.outputRCode[output.outputBlockId] = "reference_plot";
            verification << sharedRecipe->second << "\n"
                << "linkeda_plot_theme <- " << Ggplot2ThemeRExpression(plot.rExportTheme) << "\n"
                << "reference_plot_data <- transform(reference, .estimate = response,\n"
                   "  .se = SE, .lower = lower.CL, .upper = upper.CL)\n";
        } else {
        verification << sourceRecipe->second;
        if (sourceRecipe->second.back() != '\n') verification << '\n';
        verification
            << "if (!requireNamespace(\"emmeans\", quietly = TRUE))\n"
               "  stop(\"Install package 'emmeans' to run this check.\")\n"
               "if (!requireNamespace(\"ggplot2\", quietly = TRUE))\n"
               "  stop(\"Install package 'ggplot2' to run this check.\")\n"
            << "linkeda_plot_theme <- "
            << Ggplot2ThemeRExpression(plot.rExportTheme) << "\n"
            << "focal_variable <- " << ProvenanceRStringLiteral(result.focal) << "\n"
            << "conditioning_variables <- "
            << VerificationRCharacterVector(result.conditioning) << "\n"
            << "confidence_level <- " << std::setprecision(17)
            << result.confidenceLevel << "\n"
               "grid_fits <- if (exists(\"reference_fits\", inherits = FALSE)) reference_fits else list(reference_model)\n"
               "numeric_grid <- function(variable, points = 3L) {\n"
               "  values <- unlist(lapply(grid_fits, function(fit) {\n"
               "    frame <- stats::model.frame(fit)\n"
               "    if (variable %in% names(frame)) as.numeric(frame[[variable]]) else numeric()\n"
               "  }), use.names = FALSE)\n"
               "  values <- values[is.finite(values)]\n"
               "  if (points == 3L) {\n"
               "    centre <- mean(values); spread <- stats::sd(values)\n"
               "    if (!is.finite(spread) || spread <= 0) spread <- diff(range(values)) / 2\n"
               "    return(unique(pmax(min(values), pmin(max(values), c(centre - spread, centre, centre + spread)))))\n"
               "  }\n"
               "  unique(as.numeric(stats::quantile(values, probs = seq(0, 1, length.out = points),\n"
               "    na.rm = TRUE, names = FALSE)))\n"
               "}\n"
               "reference_at <- list()\n";
        if (!categoricalAxis) {
            std::vector<double> focalValues;
            for (const InteractionPlotLine &line : result.lines) {
                for (const DataPoint &point : line.points) {
                    if (std::isfinite(point.x) &&
                        std::find(focalValues.begin(), focalValues.end(), point.x) ==
                            focalValues.end()) focalValues.push_back(point.x);
                }
            }
            std::sort(focalValues.begin(), focalValues.end());
            verification << "reference_at[[focal_variable]] <- c(";
            for (std::size_t index = 0; index < focalValues.size(); ++index) {
                if (index) verification << ", ";
                verification << std::setprecision(17) << focalValues[index];
            }
            verification << ")\n";
        }
        verification
            << "first_frame <- stats::model.frame(grid_fits[[1L]])\n"
               "numeric_conditioning <- conditioning_variables[vapply(\n"
               "  conditioning_variables, function(variable) is.numeric(first_frame[[variable]]), logical(1L)\n"
               ")]\n"
               "for (variable in numeric_conditioning) reference_at[[variable]] <- numeric_grid(variable, 3L)\n"
               "reference_grid_for_fit <- function(fit) {\n"
               "  as.data.frame(summary(emmeans::emmeans(\n"
               "    fit, specs = focal_variable,\n"
               "    by = if (length(conditioning_variables)) conditioning_variables else NULL,\n"
               "    at = if (length(reference_at)) reference_at else NULL\n"
               "  ), infer = c(TRUE, TRUE), level = confidence_level"
            << (result.boundedCount ? ", type = \"response\"" : "")
            << "))\n"
               "}\n"
               "if (exists(\"reference_fits\", inherits = FALSE)) {\n"
               "  reference_grids <- lapply(reference_fits, reference_grid_for_fit)\n"
            << "  key_variables <- " << VerificationRCharacterVector([&] {
                   std::vector<std::string> keys{result.focal};
                   for (const std::string &variable : result.conditioning)
                       if (!variable.empty() && std::find(keys.begin(), keys.end(), variable) == keys.end())
                           keys.push_back(variable);
                   return keys;
               }()) << "\n"
               "  signatures <- lapply(reference_grids, function(data)\n"
               "    do.call(paste, c(data[key_variables], sep = \"\\r\")))\n"
               "  if (!all(vapply(signatures[-1L], identical, logical(1L), signatures[[1L]])))\n"
               "    stop(\"The emmeans grids differ across imputations.\")\n"
               "  estimate_column <- intersect(c(\"emmean\", \"response\", \"prob\", \"rate\"),\n"
               "                               names(reference_grids[[1L]]))[[1L]]\n"
               "  pools <- lapply(seq_len(nrow(reference_grids[[1L]])), function(row) {\n"
               "    estimates <- vapply(reference_grids, function(data) data[[estimate_column]][[row]], numeric(1L))\n"
               "    variances <- vapply(reference_grids, function(data) data$SE[[row]]^2, numeric(1L))\n"
               "    mice::pool.scalar(estimates, variances,\n"
               "      n = min(vapply(reference_fits, stats::nobs, numeric(1L))),\n"
               "      k = max(vapply(reference_fits, function(fit) length(stats::coef(fit)), numeric(1L))))\n"
               "  })\n"
               "  reference_plot_data <- reference_grids[[1L]][key_variables]\n"
               "  reference_plot_data$.estimate <- vapply(pools, `[[`, numeric(1L), \"qbar\")\n"
               "  reference_plot_data$.se <- sqrt(vapply(pools, `[[`, numeric(1L), \"t\"))\n"
               "  reference_plot_data$.df <- vapply(pools, `[[`, numeric(1L), \"df\")\n"
               "  critical <- stats::qt((1 + confidence_level) / 2, reference_plot_data$.df)\n"
               "  reference_plot_data$.lower <- reference_plot_data$.estimate - critical * reference_plot_data$.se\n"
               "  reference_plot_data$.upper <- reference_plot_data$.estimate + critical * reference_plot_data$.se\n"
               "} else {\n"
               "  reference_plot_data <- reference_grid_for_fit(reference_model)\n"
               "  estimate_column <- intersect(c(\"emmean\", \"response\", \"prob\", \"rate\"),\n"
               "                               names(reference_plot_data))[[1L]]\n"
               "  lower_column <- intersect(c(\"lower.CL\", \"asymp.LCL\"), names(reference_plot_data))[[1L]]\n"
               "  upper_column <- intersect(c(\"upper.CL\", \"asymp.UCL\"), names(reference_plot_data))[[1L]]\n"
               "  reference_plot_data$.estimate <- reference_plot_data[[estimate_column]]\n"
               "  reference_plot_data$.lower <- reference_plot_data[[lower_column]]\n"
               "  reference_plot_data$.upper <- reference_plot_data[[upper_column]]\n"
               "}\n";
        if (result.boundedCount && result.quantity == "expected_count" &&
            std::isfinite(result.trialsConstant)) {
            verification
                << "trial_count <- " << std::setprecision(17)
                << result.trialsConstant << "\n"
                   "reference_plot_data[c(\".estimate\", \".se\", \".lower\", \".upper\")] <-\n"
                   "  reference_plot_data[c(\".estimate\", \".se\", \".lower\", \".upper\")] * trial_count\n";
        }
        }
        if (!result.conditioning.empty()) {
            verification
                << "series_variables <- " << VerificationRCharacterVector(result.conditioning) << "\n"
                   "effect_legend_labels <- function(values) {\n"
                   "  if (!is.numeric(values)) return(as.character(values))\n"
                   "  trim_fraction <- function(x) {\n"
                   "    x <- sub(\"(\\\\.[0-9]*?)0+$\", \"\\\\1\", x)\n"
                   "    x <- sub(\"\\\\.$\", \"\", x)\n"
                   "    x[x %in% c(\"-0\", \"+0\")] <- \"0\"\n"
                   "    x\n"
                   "  }\n"
                   "  finite <- is.finite(values); out <- as.character(values)\n"
                   "  distinct_values <- length(unique(values[finite]))\n"
                   "  for (digits in 2:6) {\n"
                   "    candidate <- trim_fraction(formatC(values[finite], format = \"f\", digits = digits))\n"
                   "    if (length(unique(candidate)) == distinct_values) { out[finite] <- candidate; return(out) }\n"
                   "  }\n"
                   "  for (digits in 4:10) {\n"
                   "    candidate <- formatC(values[finite], format = \"g\", digits = digits)\n"
                   "    if (length(unique(candidate)) == distinct_values) { out[finite] <- candidate; return(out) }\n"
                   "  }\n"
                   "  out\n"
                   "}\n"
                   "series_data <- lapply(reference_plot_data[series_variables], effect_legend_labels)\n"
                   "reference_plot_data$.series <- interaction(\n"
                   "  as.data.frame(series_data, check.names = FALSE), drop = TRUE, sep = \" × \"\n"
                   ")\n"
                << "reference_plot <- ggplot2::ggplot(reference_plot_data, ggplot2::aes(\n"
                << "  x = .data[[" << ProvenanceRStringLiteral(result.focal)
                << "]], y = .estimate, colour = .series, group = .series\n"
                   "))\n";
        } else {
            verification
                << "reference_plot <- ggplot2::ggplot(reference_plot_data, ggplot2::aes(\n"
                << "  x = .data[[" << ProvenanceRStringLiteral(result.focal)
                << "]], y = .estimate, group = 1\n"
                   "))\n";
        }
        if (categoricalAxis)
            verification << "category_dodge <- ggplot2::position_dodge(width = .35)\n";
        verification
            << "show_confidence_intervals <- "
            << (plot.regressionConfidenceIntervalsVisible ? "TRUE" : "FALSE") << "\n"
            << "connect_estimates <- "
            << (plot.regressionConnectEstimates ? "TRUE" : "FALSE") << "\n"
               "if (show_confidence_intervals) {\n";
        if (categoricalAxis) {
            verification
                << "  reference_plot <- reference_plot + ggplot2::geom_errorbar(\n"
                   "    ggplot2::aes(ymin = .lower, ymax = .upper), width = .10,\n"
                   "    position = category_dodge\n"
                   "  )\n";
        } else {
            verification
                << "  reference_plot <- reference_plot + ggplot2::geom_ribbon(\n"
                << (result.conditioning.empty()
                    ? "    ggplot2::aes(ymin = .lower, ymax = .upper),\n"
                    : "    ggplot2::aes(ymin = .lower, ymax = .upper, fill = .series),\n")
                << "    alpha = .16, colour = NA\n"
                   "  )\n";
        }
        verification
            << "}\n"
               "if (connect_estimates) {\n"
               "  reference_plot <- reference_plot + ggplot2::geom_line(linewidth = .8"
            << (categoricalAxis ? ", position = category_dodge" : "") << ")\n"
               "}\n"
            << "legend_title <- " << (plot.interactionLegendTitleVisible
                    ? ProvenanceRStringLiteral(plot.interactionLegendTitle) : "NULL") << "\n"
               "reference_plot <- reference_plot +\n  ggplot2::geom_point(size = 2.2"
            << (categoricalAxis ? ", position = category_dodge" : "") << ") +\n"
               "  ggplot2::labs(title = " << ProvenanceRStringLiteral(plot.title)
            << ", x = " << ProvenanceRStringLiteral(plot.xLabel)
            << ", y = " << ProvenanceRStringLiteral(plot.yLabel)
            << ", colour = legend_title, fill = legend_title) + linkeda_plot_theme\n"
               "print(reference_plot)\n";
        output.provenance.verificationRCode[output.outputBlockId] =
            verification.str();
        }
    } else {
        output.provenance.verificationWarnings.push_back(
            "A portable effect-plot recipe is available only when the source model retains a portable model recipe.");
    }

    PublicationPlotSpec publication;
    publication.kind = simpleEffect ? "effect" : "interaction";
    publication.title = plot.title;
    publication.xLabel = plot.xLabel;
    publication.yLabel = plot.yLabel;
    publication.legendTitle = plot.interactionLegendTitle;
    publication.legendTitleVisible = plot.interactionLegendTitleVisible;
    publication.theme = plot.rExportTheme;
    publication.showPoints = true;
    publication.showLines = plot.regressionConnectEstimates;
    publication.showConfidenceIntervals = plot.regressionConfidenceIntervalsVisible;
    publication.confidenceLevel = result.confidenceLevel;
    for (std::size_t index = 0; index < result.lines.size(); ++index) {
        const InteractionPlotLine &line = result.lines[index];
        PublicationPlotSeries series;
        series.id = "series_" + std::to_string(index + 1);
        series.label = index < plot.interactionPlotLines.size()
            ? plot.interactionPlotLines[index].label : line.label;
        for (const DataPoint &point : line.points) {
            series.x.push_back(point.x);
            series.y.push_back(point.y);
        }
        for (const DataPoint &point : line.confidenceLower) series.lower.push_back(point.y);
        for (const DataPoint &point : line.confidenceUpper) series.upper.push_back(point.y);
        publication.series.push_back(std::move(series));
    }
    for (const auto &tick : result.xTicks) publication.xCategoryOrder.push_back(tick.second);
    output.publication.plot = std::move(publication);
    output.publication.availableBackends = {PublicationBackend::Ggplot2};
    return output;
}

void SynchronizeRegressionPlotCodeReferenceDisplay(PlotModel &plot)
{
    if (plot.kind != "glm_interaction") return;
    if (plot.codeReference.publication.plot) {
        plot.codeReference.publication.plot->showLines =
            plot.regressionConnectEstimates;
        plot.codeReference.publication.plot->showConfidenceIntervals =
            plot.regressionConfidenceIntervalsVisible;
        plot.codeReference.publication.plot->theme = plot.rExportTheme;
        plot.codeReference.publication.plot->legendTitle = plot.interactionLegendTitle;
        plot.codeReference.publication.plot->legendTitleVisible =
            plot.interactionLegendTitleVisible;
    }
    auto found = plot.codeReference.provenance.verificationRCode.find(
        plot.codeReference.outputBlockId);
    if (found == plot.codeReference.provenance.verificationRCode.end()) return;
    const auto replaceLogical = [&](const std::string &name, bool value) {
        const std::string prefix = name + " <- ";
        const std::size_t start = found->second.find(prefix);
        if (start == std::string::npos) return;
        const std::size_t logical = start + prefix.size();
        if (found->second.compare(logical, 4, "TRUE") == 0) {
            found->second.replace(logical, 4, value ? "TRUE" : "FALSE");
        } else if (found->second.compare(logical, 5, "FALSE") == 0) {
            found->second.replace(logical, 5, value ? "TRUE" : "FALSE");
        }
    };
    replaceLogical("show_confidence_intervals",
                   plot.regressionConfidenceIntervalsVisible);
    replaceLogical("connect_estimates", plot.regressionConnectEstimates);
    const std::string themePrefix = "linkeda_plot_theme <- ";
    const std::size_t themeStart = found->second.find(themePrefix);
    if (themeStart != std::string::npos) {
        const std::size_t themeEnd = found->second.find('\n', themeStart);
        found->second.replace(
            themeStart,
            (themeEnd == std::string::npos ? found->second.size() : themeEnd) -
                themeStart,
            themePrefix + Ggplot2ThemeRExpression(plot.rExportTheme));
    }
    const std::string legendPrefix = "legend_title <- ";
    const std::size_t legendStart = found->second.find(legendPrefix);
    if (legendStart != std::string::npos) {
        const std::size_t legendEnd = found->second.find('\n', legendStart);
        found->second.replace(legendStart,
            (legendEnd == std::string::npos ? found->second.size() : legendEnd) - legendStart,
            legendPrefix + (plot.interactionLegendTitleVisible
                ? ProvenanceRStringLiteral(plot.interactionLegendTitle) : "NULL"));
    }
}

OutputCodeReference RegressionPartialPlotCodeReference(
    const PlotModel &plot, const RegressionPartialPlotResult &result,
    const OutputCodeReference &sourceModel)
{
    OutputCodeReference output = sourceModel;
    output.outputId = plot.id;
    output.outputBlockId = "partial";
    output.title = plot.title;
    output.kind = "plot";
    // Preserve the source's immutable scope and dataset version. The recipe
    // is captured by R alongside the exact term/residual/imputation result.
    const auto recipe = result.provenance.verificationRCode.find("partial");
    if (recipe != result.provenance.verificationRCode.end()) {
        output.provenance.verificationRCode["partial"] = recipe->second;
    }
    if (!result.provenance.executedRCode.empty()) {
        output.provenance.executedRCode = result.provenance.executedRCode;
    }
    output.publication = {};
    PublicationPlotSpec publication;
    publication.kind = "scatter";
    publication.title = plot.title;
    publication.xLabel = plot.xLabel;
    publication.yLabel = plot.yLabel;
    publication.theme = plot.rExportTheme;
    publication.showLines = false;
    PublicationPlotSeries series;
    series.id = "partial";
    series.label = result.term;
    for (const auto &point : plot.points) {
        series.x.push_back(point.x);
        series.y.push_back(point.y);
    }
    publication.series.push_back(std::move(series));
    output.publication.plot = std::move(publication);
    output.publication.availableBackends = {PublicationBackend::Ggplot2};
    return output;
}

bool ParseRegressionPartialPlotRResult(
    const std::vector<std::string> &lines,
    RegressionPartialPlotResult &result)
{
    result = RegressionPartialPlotResult();
    if (lines.empty() || lines.front() != "OK") {
        result.message = "R did not return partial-regression plot data.";
        return false;
    }
    for (std::size_t i = 1; i < lines.size(); ++i) {
        if (lines[i] == "ANALYSIS_PROVENANCE_V2" || lines[i] == "ANALYSIS_PROVENANCE_V1") {
            if (!ReadAnalysisProvenancePayload(lines, i, result.provenance, &result.message)) return false;
            --i;
            continue;
        }
        const std::vector<std::string> fields = SplitTabs(lines[i]);
        if (fields.empty()) continue;
        if (fields[0] == "META" && fields.size() >= 4) {
            result.term = fields[1];
            result.residualType = fields[2];
            if (fields.size() >= 5) result.contributionScale = fields[4];
            result.imputationCount = std::max(1, ParseOptionalDataCellInt(fields[3]));
            result.pointsByImputation.resize(
                static_cast<std::size_t>(result.imputationCount));
        } else if (fields[0] == "TERMS") {
            result.availableTerms.assign(fields.begin() + 1, fields.end());
        } else if (fields[0] == "RESIDUALS") {
            result.availableResidualTypes.assign(fields.begin() + 1, fields.end());
        } else if (fields[0] == "ROW" && fields.size() >= 5) {
            const int imputation = ParseOptionalDataCellInt(fields[1]);
            const int row = ParseOptionalDataCellInt(fields[2]);
            const double x = ParseOptionalDataCellDouble(fields[3]);
            const double y = ParseOptionalDataCellDouble(fields[4]);
            if (imputation < 1 || !std::isfinite(x) || !std::isfinite(y)) continue;
            if (result.pointsByImputation.size() < static_cast<std::size_t>(imputation)) {
                result.pointsByImputation.resize(static_cast<std::size_t>(imputation));
                result.imputationCount = imputation;
            }
            result.pointsByImputation[static_cast<std::size_t>(imputation - 1)]
                .push_back(DiagnosticPlotPoint{x, y, row});
        }
    }
    result.ok = std::any_of(
        result.pointsByImputation.begin(), result.pointsByImputation.end(),
        [](const auto &points) { return !points.empty(); });
    if (!result.ok) result.message = "R returned no finite partial-regression points.";
    return result.ok;
}

DiagnosticPlotData BuildRegressionPartialPlotData(
    const RegressionPartialPlotResult &result,
    int imputationIndex,
    bool imputationUncertainty)
{
    DiagnosticPlotData data;
    data.kind = "scatter";
    data.diagnosticKind = "partial_regression";
    data.residualType = result.residualType;
    data.title = "Partial regression plot — " + result.term;
    data.xLabel = result.term + " contribution";
    if (!result.contributionScale.empty()) data.xLabel += " (" + result.contributionScale + " scale)";
    data.yLabel = "partial residual (" + result.residualType + ")";
    if (!result.ok || result.pointsByImputation.empty()) {
        data.message = result.message.empty()
            ? "The partial regression plot is unavailable." : result.message;
        return data;
    }
    const int selected = std::clamp(
        imputationIndex, 1, static_cast<int>(result.pointsByImputation.size()));
    if (!imputationUncertainty || result.pointsByImputation.size() == 1) {
        data.points = result.pointsByImputation[static_cast<std::size_t>(selected - 1)];
    } else {
        std::vector<DiagnosticPlotData> perImputation;
        perImputation.reserve(result.pointsByImputation.size());
        for (const auto &points : result.pointsByImputation) {
            DiagnosticPlotData current = data;
            current.points = points;
            current.ok = !points.empty();
            perImputation.push_back(std::move(current));
        }
        data = BuildDiagnosticPlotDataAcrossImputations(perImputation);
        data.title = "Partial regression plot — " + result.term +
            " — Imputation uncertainty (m = " +
            std::to_string(result.pointsByImputation.size()) + ")";
    }
    data.ok = !data.points.empty();
    if (!data.ok) data.message =
        "The selected imputation has no finite partial-regression points.";
    return data;
}

void PopulateRegressionInteractionLegendRows(
    RegressionInteractionPlotResult &result,
    const DataFrameModel &dataframe)
{
    const std::size_t rowCount =
        static_cast<std::size_t>(std::max(0, dataframe.rows));
    const std::size_t activeVersion = static_cast<std::size_t>(std::max(
        0, std::min(dataframe.activeImputationVersion,
                    std::max(1, dataframe.imputationCount)) - 1));
    const auto activeValue = [&](const DataColumn &column, std::size_t row) {
        if (dataframe.imputationCount > 0 &&
            (!column.imputationValues.empty() ||
             !column.imputationValuesSparse.empty())) {
            return ImputationVersionValueForCell(column, row, activeVersion);
        }
        return DisplayValueForCell(column, row);
    };

    result.xTickCaseIds.assign(result.xTicks.size(), {});
    if (!result.xTicks.empty() && !result.focal.empty()) {
        const DataColumn *focal = FindDataColumnInDataFrame(
            dataframe, result.focal);
        if (focal) {
            for (std::size_t row = 0; row < rowCount; ++row) {
                const std::string observed = activeValue(*focal, row);
                for (std::size_t category = 0;
                     category < result.xTicks.size(); ++category) {
                    if (observed == result.xTicks[category].second) {
                        result.xTickCaseIds[category].push_back(
                            static_cast<int>(row + 1));
                        break;
                    }
                }
            }
        }
    }

    if (result.lines.empty() || result.moderator.empty()) return;
    const std::vector<std::string> conditioning = result.conditioning.empty()
        ? std::vector<std::string>{result.moderator} : result.conditioning;
    std::vector<const DataColumn *> columns;
    columns.reserve(conditioning.size());
    for (const std::string &variable : conditioning) {
        const DataColumn *column = FindDataColumnInDataFrame(dataframe, variable);
        if (!column) return;
        columns.push_back(column);
    }
    for (InteractionPlotLine &line : result.lines) line.caseIds.clear();

    std::vector<std::vector<std::string>> targets(
        result.lines.size(), std::vector<std::string>(conditioning.size()));
    for (std::size_t lineIndex = 0; lineIndex < result.lines.size(); ++lineIndex) {
        const std::string &label = result.lines[lineIndex].label;
        for (std::size_t variableIndex = 0; variableIndex < conditioning.size(); ++variableIndex) {
            const std::string prefix = conditioning[variableIndex] + " = ";
            const std::size_t begin = label.find(prefix);
            if (begin == std::string::npos) continue;
            const std::size_t valueBegin = begin + prefix.size();
            const std::size_t valueEnd = label.find(" · ", valueBegin);
            targets[lineIndex][variableIndex] = label.substr(
                valueBegin, valueEnd == std::string::npos
                    ? std::string::npos : valueEnd - valueBegin);
        }
    }

    // A linked row must belong to exactly the conditioning series shown for
    // the currently displayed imputation. Taking the union over every
    // imputation makes an originally missing factor case belong to multiple
    // categories (for example both Female and Male), so clicking either
    // legend entry activates both fitted lines. The pooled estimates still
    // use every imputation; this choice affects only linked-row identity.
    for (std::size_t row = 0; row < rowCount; ++row) {
        std::set<std::size_t> associatedLines;
        std::vector<std::string> observed(conditioning.size());
        for (std::size_t variableIndex = 0; variableIndex < columns.size(); ++variableIndex) {
            const DataColumn &column = *columns[variableIndex];
            observed[variableIndex] = activeValue(column, row);
        }
        for (std::size_t candidate = 0; candidate < result.lines.size(); ++candidate) {
            bool matches = true;
            for (std::size_t variableIndex = 0; variableIndex < columns.size(); ++variableIndex) {
                if (columns[variableIndex]->type == "numeric") {
                    const double value = ParseOptionalDataCellDouble(observed[variableIndex]);
                    const double target = ParseOptionalDataCellDouble(
                        targets[candidate][variableIndex]);
                    if (!std::isfinite(value) || !std::isfinite(target)) {
                        matches = false;
                        break;
                    }
                    double bestDistance = std::numeric_limits<double>::infinity();
                    for (std::size_t other = 0; other < result.lines.size(); ++other) {
                        const double otherTarget = ParseOptionalDataCellDouble(
                            targets[other][variableIndex]);
                        if (std::isfinite(otherTarget)) {
                            bestDistance = std::min(
                                bestDistance, std::fabs(value - otherTarget));
                        }
                    }
                    if (std::fabs(value - target) > bestDistance + 1e-12) {
                        matches = false;
                        break;
                    }
                } else if (observed[variableIndex] != targets[candidate][variableIndex]) {
                    matches = false;
                    break;
                }
            }
            if (matches) associatedLines.insert(candidate);
        }
        for (std::size_t lineIndex : associatedLines) {
            result.lines[lineIndex].caseIds.push_back(static_cast<int>(row + 1));
        }
    }
}

NestedModelTestResult RegressionAdjacentModelTest(const RegressionComparisonState &state, int modelColumn)
{
    NestedModelTestResult result;
    if (modelColumn <= 0 || (size_t)modelColumn >= state.models.size()) {
        return result;
    }
    const RegressionComparisonModel &model = state.models[(size_t)modelColumn];
    if (model.comparisonOk) {
        result.ok = true;
        result.df = model.comparisonDf;
        result.df2 = model.comparisonDf2;
        result.delta = model.comparisonDelta;
        result.statistic = model.comparisonStatistic;
        result.p = model.comparisonP;
        return result;
    }
    if (state.precomputed) {
        return result;
    }
    const RegressionComparisonModel &previous = state.models[(size_t)modelColumn - 1];
    const RegressionComparisonModel &current = state.models[(size_t)modelColumn];
    NestedLinearModelSummary left = BuildNestedLinearModelSummary(
        previous.fit, previous.response.empty() ? state.response : previous.response, previous.terms);
    NestedLinearModelSummary right = BuildNestedLinearModelSummary(
        current.fit, current.response.empty() ? state.response : current.response, current.terms);
    return LinearNestedModelTest(left, right);
}

NestedModelTestResult GeneralizedAdjacentModelTest(const GeneralizedComparisonState &state, int modelColumn)
{
    NestedModelTestResult result;
    if (modelColumn <= 0 || (size_t)modelColumn >= state.models.size()) {
        return result;
    }
    const GeneralizedComparisonModel &previous = state.models[(size_t)modelColumn - 1];
    const GeneralizedComparisonModel &current = state.models[(size_t)modelColumn];
    if (current.comparisonOk) {
        result.ok = true;
        result.df = current.comparisonDf;
        result.df2 = current.comparisonDf2;
        result.delta = current.comparisonDelta;
        result.statistic = current.comparisonStatistic;
        result.p = current.comparisonP;
        result.statisticName = current.comparisonMethod.empty()
            ? "Chi-square" : current.comparisonMethod;
        return result;
    }
    if (!GeneralizedComparisonModelsSupportLikelihoodRatio(state, previous, current)) {
        return result;
    }
    if (state.binaryComparison) return result;
    NestedGeneralizedModelSummary left = BuildNestedGeneralizedModelSummary(
        previous.fit.ok, previous.response.empty() ? state.response : previous.response,
        previous.fit.family, previous.fit.link, previous.terms, previous.fit.rowsUsed,
        previous.fit.dfResidual, previous.fit.residualDeviance, previous.fit.dispersion);
    NestedGeneralizedModelSummary right = BuildNestedGeneralizedModelSummary(
        current.fit.ok, current.response.empty() ? state.response : current.response,
        current.fit.family, current.fit.link, current.terms, current.fit.rowsUsed,
        current.fit.dfResidual, current.fit.residualDeviance, current.fit.dispersion);
    return GeneralizedNestedModelTest(left, right);
}

bool BinaryModelsCompatibleForLikelihoodRatioTest(const GeneralizedGLMState &left,
                                                   const GeneralizedGLMState &right,
                                                   std::string *reason)
{
    auto fail = [&](const std::string &message) {
        if (reason) *reason = message;
        return false;
    };
    if (!left.binaryRegression || !right.binaryRegression) return fail("Both models must be Binary Models.");
    if (!left.ok || !right.ok) return fail("Both models must be fitted successfully.");
    if (left.group != right.group || left.response != right.response) return fail("Models must use the same dataset and response.");
    if (left.responseCoding.eventValue != right.responseCoding.eventValue ||
        left.responseCoding.referenceValue != right.responseCoding.referenceValue) {
        return fail("Models must use the same event and reference categories.");
    }
    if (left.family != "binomial" || right.family != "binomial" || left.link != right.link) {
        return fail("Likelihood-ratio tests require the same binomial family and link.");
    }
    if (!SameIntegerSet(left.rowsUsed, right.rowsUsed)) return fail("Models must use the same analysis rows.");
    if (!TermVectorsAreNested(left.terms, right.terms)) return fail("Model terms are not hierarchically nested.");
    if (reason) reason->clear();
    return true;
}

void GLMAddInteractionLinePoint(PlotModel *plot, InteractionPlotLine &line, double x, double y)
{
    if (!plot || !std::isfinite(x) || !std::isfinite(y)) return;
    DataPoint p{x, y, 0};
    line.points.push_back(p);
    plot->points.push_back(p);
}

namespace {

bool GeneralizedComparisonFitSemanticsMatchSpecification(
    const GeneralizedComparisonState &state,
    const GeneralizedComparisonModel &model,
    const std::vector<GeneralizedGLMRow> &rows,
    std::string *reason)
{
    auto fail = [&](const std::string &message) {
        if (reason) *reason = message;
        return false;
    };
    const auto matchesSource = [](const GeneralizedGLMRow &row,
                                  const std::string &term) {
        return EquivalentModelTerm(
                   EffectiveGeneralizedRowSourceTerm(row, row.term), term) ||
               EquivalentModelTerm(row.term, term);
    };
    bool intercept = false;
    int coefficientRows = 0;
    for (const GeneralizedGLMRow &row : rows) {
        if (row.term == "(Intercept)" && row.rowType == "coefficient") intercept = true;
        if (row.rowType == "coefficient" || row.rowType == "factor_level") {
            ++coefficientRows;
        }
    }
    if (!intercept) return fail("the fitted semantic rows have no intercept coefficient");

    const auto types = GeneralizedComparisonModelTermTypes(state, model);
    for (const std::string &term : model.terms) {
        const std::string type = ModelTermDisplayType(&state.seed, term, types);
        bool represented = false;
        bool factorParent = false;
        bool factorReference = false;
        bool factorLevel = false;
        bool coefficientOrParent = false;
        std::string returnedReference;
        for (const GeneralizedGLMRow &row : rows) {
            if (!matchesSource(row, term)) continue;
            represented = true;
            factorParent = factorParent || row.rowType == "factor_parent";
            factorReference = factorReference || row.rowType == "reference";
            factorLevel = factorLevel || row.rowType == "factor_level";
            coefficientOrParent = coefficientOrParent ||
                row.rowType == "coefficient" || row.rowType == "term_parent";
            if (row.rowType == "reference") {
                returnedReference = !row.factorLevel.empty()
                    ? row.factorLevel : row.referenceLevel;
            }
        }
        if (!represented) return fail("the fitted semantic rows omit " + term);
        if (type == "factor") {
            if (!factorParent || !factorReference || !factorLevel) {
                return fail("the fitted categorical hierarchy is incomplete for " + term);
            }
            const std::string variable = BaseVariableForTermComponent(term);
            auto configured = model.factorReferenceLevels.find(variable);
            if (configured != model.factorReferenceLevels.end() &&
                returnedReference != configured->second) {
                return fail("the fitted reference category differs for " + term);
            }
        } else if (!coefficientOrParent) {
            return fail("the fitted coefficient structure is incomplete for " + term);
        }
    }

    if (!state.multipleImputation && !model.fit.rankDeficient &&
        model.fit.parameterCount > 0 && coefficientRows != model.fit.parameterCount) {
        return fail("the parameter count does not match the semantic coefficient rows");
    }
    if (!state.multipleImputation && model.fit.rank > 0 && model.fit.n > 0 &&
        model.fit.dfResidual >= 0 && model.fit.dfResidual != model.fit.n - model.fit.rank) {
        return fail("the residual degrees of freedom do not match the fitted rank");
    }
    if (reason) reason->clear();
    return true;
}

} // namespace

void RefreshGeneralizedTermRowsFromFits(GeneralizedComparisonState &state)
{
    std::vector<std::vector<std::string>> includedTermsByModel;
    std::vector<std::vector<GeneralizedGLMRow>> rowsByModel;
    includedTermsByModel.reserve(state.models.size() + 1);
    rowsByModel.reserve(state.models.size());

    // Preserve the insertion order already visible in every generalized
    // comparison (binary, count and other GLM families).  The first synthetic
    // term vector establishes presentation order; the per-model vectors below
    // add only genuinely new candidates.
    std::vector<std::string> existingOrder;
    for (const std::string &displayTerm : state.termRows) {
        if (displayTerm.empty() || displayTerm == "(Intercept)") continue;
        const GeneralizedGLMRow *displayRow = nullptr;
        for (const GeneralizedComparisonModel &model : state.models) {
            displayRow = GeneralizedComparisonRowForPresentationTerm(
                model.fit.rows, displayTerm);
            if (displayRow) break;
        }
        std::string source = displayRow
            ? EffectiveGeneralizedRowSourceTerm(*displayRow, displayTerm)
            : ModelTermBaseForDisplayRow(displayTerm);
        if (source.empty()) source = displayTerm;
        bool knownSource = false;
        for (const GeneralizedComparisonModel &model : state.models) {
            const std::vector<std::string> knownTerms = ComparisonPresentationCandidates(
                model.candidateTerms, model.terms);
            if (TermListContainsEquivalentModelTerm(knownTerms, source)) {
                knownSource = true;
                break;
            }
        }
        if (knownSource &&
            !TermListContainsEquivalentModelTerm(existingOrder, source)) {
            existingOrder.push_back(source);
        }
    }
    if (!existingOrder.empty()) includedTermsByModel.push_back(existingOrder);
    for (GeneralizedComparisonModel &model : state.models) {
        includedTermsByModel.push_back(ComparisonPresentationCandidates(
            model.candidateTerms, model.terms));
        const std::string currentFingerprint =
            GeneralizedComparisonModelSpecificationFingerprint(state, model);
        const bool exactFitIdentity = !model.isStale &&
            model.fitVersion == model.modelVersion &&
            model.fitSpecificationRevision == model.modelVersion &&
            model.fitSpecificationFingerprint == currentFingerprint &&
            model.fit.lastRFitSignature == currentFingerprint &&
            model.fit.multipleImputation == state.multipleImputation &&
            model.fit.imputationCount == state.imputationCount &&
            model.fit.datasetType == state.datasetType &&
            model.fit.imputationSetId == state.imputationSetId &&
            model.fit.sourceDatasetId == state.sourceDatasetId;
        if (!exactFitIdentity && !model.isStale) {
            model.isStale = true;
            model.fitState = RegressionComparisonFitState::NotFitted;
        }

        std::vector<GeneralizedGLMRow> currentRows;
        bool rejectedSemanticRow = false;
        if (exactFitIdentity) {
            std::string currentSource;
            for (const GeneralizedGLMRow &input : model.fit.rows) {
                GeneralizedGLMRow row = input;
                if (row.rowType == "section_header") {
                    currentRows.push_back(std::move(row));
                    currentSource.clear();
                    continue;
                }
                std::string source = EffectiveGeneralizedRowSourceTerm(row, row.term);
                if ((row.rowType == "factor_level" || row.rowType == "reference") &&
                    source == row.term && !currentSource.empty()) {
                    source = currentSource;
                    row.sourceTerm = source;
                }
                if (row.rowType == "factor_parent" || row.rowType == "term_parent") {
                    currentSource = source.empty() ? row.term : source;
                }
                const bool intercept = row.term == "(Intercept)" || source == "(Intercept)";
                if (intercept || ModelSpecificationIncludesTerm(model, source)) {
                    currentRows.push_back(std::move(row));
                } else {
                    rejectedSemanticRow = true;
                }
            }
            for (const std::string &term : model.terms) {
                bool represented = false;
                for (const GeneralizedGLMRow &row : currentRows) {
                    const std::string source = EffectiveGeneralizedRowSourceTerm(row, row.term);
                    if (EquivalentModelTerm(source, term) || EquivalentModelTerm(row.term, term)) {
                        represented = true;
                        break;
                    }
                }
                if (!represented) {
                    rejectedSemanticRow = true;
                    break;
                }
            }
            std::string semanticReason;
            if (!rejectedSemanticRow &&
                !GeneralizedComparisonFitSemanticsMatchSpecification(
                    state, model, currentRows, &semanticReason)) {
                rejectedSemanticRow = true;
            }
        }
        if (rejectedSemanticRow) {
            model.isStale = true;
            model.fitState = RegressionComparisonFitState::NotFitted;
            currentRows.clear();
        } else if (exactFitIdentity) {
            // Persist only source-term normalization (for older factor-level
            // payloads that omitted it).  No row outside the specification is
            // retained or promoted into presentation state.
            model.fit.rows = currentRows;
        }
        rowsByModel.push_back(std::move(currentRows));
    }
    state.termRows = GeneralizedComparisonTermRowsFromFits(includedTermsByModel, rowsByModel);
    state.presentationFingerprint = GeneralizedComparisonFitSignature(state);
}

const GeneralizedGLMRow *DisplayRowForGeneralizedComparisonTerm(const GeneralizedComparisonState &state,
                                                                const std::string &term)
{
    for (const GeneralizedComparisonModel &model : state.models) {
        if (GeneralizedComparisonResolvedFitState(state, model) !=
            RegressionComparisonFitState::Valid) continue;
        const GeneralizedGLMRow *row = GeneralizedComparisonRowForPresentationTerm(
            model.fit.rows, term);
        if (!row) continue;
        const std::string source = EffectiveGeneralizedRowSourceTerm(*row, term);
        if (term == "(Intercept)" || source == "(Intercept)" ||
            ModelSpecificationIncludesTerm(model, source)) return row;
    }
    return nullptr;
}

bool GeneralizedComparisonPresentationIsConsistent(
    const GeneralizedComparisonState &state,
    std::string *reason)
{
    auto fail = [&](const std::string &message) {
        if (reason) *reason = message;
        return false;
    };
    if (state.presentationFingerprint != GeneralizedComparisonFitSignature(state)) {
        return fail("presentation fingerprint does not match the current comparison specification");
    }
    for (const GeneralizedComparisonModel &model : state.models) {
        const auto fitState = GeneralizedComparisonResolvedFitState(state, model);
        if (!model.isStale && model.fit.ok && fitState != RegressionComparisonFitState::Valid) {
            return fail("fit statistics are not keyed to the current model specification revision");
        }
        for (const std::string &term : model.terms) {
            bool represented = false;
            for (const std::string &displayTerm : state.termRows) {
                const GeneralizedGLMRow *row = DisplayRowForGeneralizedComparisonTerm(state, displayTerm);
                const std::string source = GeneralizedRowSourceTerm(row, displayTerm);
                if (EquivalentModelTerm(source, term) || EquivalentModelTerm(displayTerm, term)) {
                    represented = true;
                    break;
                }
            }
            if (!represented) return fail("an included specification term has no semantic presentation row: " + term);
        }
        if (fitState == RegressionComparisonFitState::Valid) {
            std::string semanticReason;
            if (!GeneralizedComparisonFitSemanticsMatchSpecification(
                    state, model, model.fit.rows, &semanticReason)) {
                return fail(semanticReason);
            }
            std::string currentSource;
            for (const GeneralizedGLMRow &row : model.fit.rows) {
                if (row.rowType == "section_header") {
                    currentSource.clear();
                    continue;
                }
                std::string source = EffectiveGeneralizedRowSourceTerm(row, row.term);
                if ((row.rowType == "factor_level" || row.rowType == "reference") &&
                    source == row.term && !currentSource.empty()) source = currentSource;
                if (row.rowType == "factor_parent" || row.rowType == "term_parent")
                    currentSource = source.empty() ? row.term : source;
                if (row.term != "(Intercept)" && source != "(Intercept)" &&
                    !ModelSpecificationIncludesTerm(model, source)) {
                    return fail("a coefficient row belongs to a term outside the current specification: " + row.term);
                }
            }
        }
    }
    for (const std::string &displayTerm : state.termRows) {
        if (displayTerm == "(Intercept)") continue;
        const GeneralizedGLMRow *row = DisplayRowForGeneralizedComparisonTerm(state, displayTerm);
        const std::string source = GeneralizedRowSourceTerm(row, displayTerm);
        bool belongs = false;
        for (const GeneralizedComparisonModel &model : state.models) {
            if (ModelSpecificationIncludesTerm(model, source) ||
                ModelSpecificationIncludesTerm(model, displayTerm) ||
                TermListContainsEquivalentModelTerm(model.candidateTerms, source) ||
                TermListContainsEquivalentModelTerm(model.candidateTerms, displayTerm)) {
                belongs = true;
                break;
            }
        }
        if (!belongs) return fail(
            "a displayed term is neither included nor an explicit candidate in any model column: " +
            displayTerm);
    }
    if (reason) reason->clear();
    return true;
}

std::string GeneralizedComparisonConsistencyTrace(
    const GeneralizedGLMState &source,
    const GeneralizedComparisonState &state,
    int modelIndex)
{
    std::ostringstream out;
    auto appendTerms = [&](const ModelSpecification &specification) {
        if (specification.terms.empty()) out << " (none)";
        for (const std::string &term : specification.terms) {
            out << " " << term << ":" << ModelSpecificationTermType(specification, term);
            const std::string variable = BaseVariableForTermComponent(term);
            auto reference = specification.factorReferenceLevels.find(variable);
            if (reference != specification.factorReferenceLevels.end())
                out << "[reference=" << reference->second << "]";
            if (specification.centeredPredictors.count(variable)) out << "[centered]";
        }
    };
    out << "1 source single-model ModelSpecification revision=" << source.modelVersion
        << " fingerprint=" << GeneralizedGLMFitSignature(source) << " terms:";
    appendTerms(source);
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size()) {
        out << "\n2 comparison model unavailable";
        return out.str();
    }
    const GeneralizedComparisonModel &model = state.models[static_cast<std::size_t>(modelIndex)];
    const ModelSpecification canonical =
        EffectiveGeneralizedComparisonModelSpecification(state, model);
    const std::string currentFingerprint =
        GeneralizedComparisonModelSpecificationFingerprint(state, model);
    out << "\n2 specification copied into comparison revision=" << model.modelVersion
        << " fingerprint=" << currentFingerprint << " terms:";
    appendTerms(static_cast<const ModelSpecification &>(model));
    out << "\n3 comparison canonical terms:";
    appendTerms(canonical);
    out << "\n4 global/shared term registry: absent; per-column candidates:";
    for (const std::string &term : model.candidateTerms) out << " " << term;
    out << "; derived presentation snapshot fingerprint="
        << state.presentationFingerprint << " rows:";
    for (const std::string &displayTerm : state.termRows) {
        const GeneralizedGLMRow *row = DisplayRowForGeneralizedComparisonTerm(
            state, displayTerm);
        out << " " << (row ? row->term : displayTerm);
        if (row) out << "{row-type=" << row->rowType << "}";
    }
    out << "\n5 per-model inclusion map:";
    for (const std::string &term : ComparisonPresentationCandidates(
             model.candidateTerms, canonical.terms)) {
        const bool included = ModelSpecificationIncludesTerm(canonical, term);
        out << " " << term << "=" << (included ? "included" : "excluded-candidate")
            << "(" << ModelSpecificationTermType(canonical, term) << ")";
    }
    out << "\n6 semantic row model sent to AppKit:";
    for (const std::string &displayTerm : state.termRows) {
        const GeneralizedGLMRow *row = DisplayRowForGeneralizedComparisonTerm(state, displayTerm);
        out << " " << (row ? row->term : displayTerm) << "[row-type="
            << (row ? row->rowType : "unresolved") << ",source="
            << GeneralizedRowSourceTerm(row, displayTerm) << ",type="
            << (row && !row->termType.empty() ? row->termType :
                ModelSpecificationTermType(canonical,
                    GeneralizedRowSourceTerm(row, displayTerm))) << "]";
    }
    out << "\n7 fit request specification revision=" << model.requestedSpecificationRevision
        << " fingerprint=" << model.requestedSpecificationFingerprint << " terms:";
    appendTerms(canonical);
    out << "\n8 returned ModelFitResult specification revision="
        << model.fitSpecificationRevision << " fingerprint="
        << model.fitSpecificationFingerprint << " terms:";
    appendTerms(static_cast<const ModelSpecification &>(model.fit));
    out << "\n9 returned coefficient columns/semantic rows:";
    for (const std::string &term : canonical.terms) {
        out << "\n  " << term
            << " present=" << (ModelSpecificationIncludesTerm(canonical, term) ? "yes" : "no")
            << " included=yes type=" << ModelSpecificationTermType(canonical, term);
        const std::string variable = BaseVariableForTermComponent(term);
        auto reference = canonical.factorReferenceLevels.find(variable);
        if (reference != canonical.factorReferenceLevels.end()) {
            out << " reference=" << reference->second;
        }
        out << " coefficient-columns=";
        bool any = false;
        for (const GeneralizedGLMRow &row : model.fit.rows) {
            const std::string source = EffectiveGeneralizedRowSourceTerm(row, row.term);
            if (!EquivalentModelTerm(source, term) && !EquivalentModelTerm(row.term, term)) continue;
            if (row.rowType != "coefficient" && row.rowType != "factor_level") continue;
            out << (any ? "," : "") << row.term;
            any = true;
        }
        if (!any) out << "(none)";
    }
    out << "\n10 returned fit statistics: N=" << model.fit.n
        << " parameters=" << model.fit.parameterCount
        << " rank=" << model.fit.rank
        << " df-residual=" << model.fit.dfResidual
        << " residual-deviance=" << model.fit.residualDeviance;
    return out.str();
}

std::string GeneralizedComparisonDisplayLabel(const GeneralizedComparisonState &state,
                                              const std::string &term)
{
    return GeneralizedRowDisplayLabel(DisplayRowForGeneralizedComparisonTerm(state, term), term);
}

std::string GeneralizedComparisonSourceTerm(const GeneralizedComparisonState &state,
                                            const std::string &displayTerm)
{
    return GeneralizedRowSourceTerm(DisplayRowForGeneralizedComparisonTerm(state, displayTerm), displayTerm);
}

RegressionComparisonTermCellState GeneralizedComparisonTermCell(
    const GeneralizedComparisonState &state,
    int modelIndex,
    const std::string &term)
{
    RegressionComparisonTermCellState cell;
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size()) {
        return cell;
    }
    const GeneralizedComparisonModel &model = state.models[static_cast<std::size_t>(modelIndex)];
    const GeneralizedGLMRow *sharedRow = DisplayRowForGeneralizedComparisonTerm(state, term);
    const std::string source = GeneralizedRowSourceTerm(sharedRow, term);
    cell.factorLevel = sharedRow &&
        (sharedRow->rowType == "factor_level" || sharedRow->rowType == "reference");
    cell.factorParent = term != "(Intercept)" && !cell.factorLevel &&
        GeneralizedComparisonModelTermType(state, modelIndex, source) == "factor";
    cell.termIncluded = term == "(Intercept)" || GeneralizedModelIncludesTerm(model, source);
    cell.inclusionControlAvailable = term != "(Intercept)" && !cell.factorLevel && term == source;

    if (!cell.termIncluded ||
        GeneralizedComparisonResolvedFitState(state, model) != RegressionComparisonFitState::Valid ||
        cell.factorParent) {
        return cell;
    }

    const GeneralizedGLMRow *row = GeneralizedComparisonRowForPresentationTerm(
        model.fit.rows, term);
    if (!row && source != term) row = GeneralizedRowForTerm(model.fit.rows, source);
    if (!row) return cell;
    if (row->rowType == "reference") {
        cell.displayState = RegressionComparisonCellDisplayState::NotApplicable;
        cell.displayText = "\u2014";
        return cell;
    }
    if (row->rowType != "coefficient" && row->rowType != "factor_level") return cell;
    cell.coefficientValue = row->estimate;
    cell.displayText = GeneralizedCoefficientDisplay(model.fit.rows, model.fit.ok, model.terms, term);
    if (!cell.displayText.empty() && cell.displayText != "\u2014") {
        cell.displayState = RegressionComparisonCellDisplayState::Value;
    } else if (cell.displayText == "\u2014") {
        cell.displayState = RegressionComparisonCellDisplayState::NotApplicable;
    }
    return cell;
}

RegressionComparisonFitCellState GeneralizedComparisonFitCell(
    const GeneralizedComparisonState &state,
    int modelIndex,
    int fitRow)
{
    RegressionComparisonFitCellState cell;
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size()) return cell;
    const GeneralizedComparisonModel &model = state.models[static_cast<std::size_t>(modelIndex)];
    // Distribution and exposure are editable model-specification state rather
    // than fitted-result state.  Keep them visible while a model is pending or
    // stale (notably when Auto-refit is disabled).
    if (fitRow == 16) {
        cell.displayText = CountDistributionLabel(model.countDistribution);
        cell.displayState = RegressionComparisonCellDisplayState::Value;
        return cell;
    }
    if (fitRow == 17) {
        if (CountDistributionUsesTrials(model.countDistribution)) {
            cell.displayText = !model.trialsVariable.empty()
                ? model.trialsVariable
                : FormatDoubleOrDash(model.trialsConstant, 0);
        } else {
            cell.displayText = model.exposure.empty()
                ? "None" : model.exposure + " (log offset)";
        }
        cell.displayState = RegressionComparisonCellDisplayState::Value;
        return cell;
    }
    if (GeneralizedComparisonResolvedFitState(state, model) != RegressionComparisonFitState::Valid) {
        return cell;
    }
    const auto capabilities = GeneralizedComparisonModelCapabilities(state, model);
    if ((fitRow == 6 || fitRow == 7 || fitRow == 9 || fitRow == 14 || fitRow == 15) &&
        !capabilities.hasLikelihood) {
        cell.displayText = "\u2014";
    } else if (fitRow == 18) {
        const bool negativeBinomial = model.countDistribution == CountDistribution::NegativeBinomial;
        const double value = negativeBinomial ? model.fit.theta : model.fit.dispersion;
        if (std::isfinite(value)) {
            std::string label;
            switch (model.countDistribution) {
            case CountDistribution::NegativeBinomial: label = "Theta"; break;
            case CountDistribution::BetaBinomial: label = "Precision (phi)"; break;
            case CountDistribution::HurdleBetaBinomialCeiling: label = "Dispersion (sigma)"; break;
            case CountDistribution::QuasiPoisson: label = "Dispersion (phi)"; break;
            default: label = "Dispersion (fixed)"; break;
            }
            cell.displayText = label + ": " + FormatDoubleOrDash(value, 3);
        } else cell.displayText = "\u2014";
    } else if (fitRow == 14 || fitRow == 15) {
        const bool useAic = fitRow == 14;
        double minimum = INFINITY;
        for (const GeneralizedComparisonModel &candidate : state.models) {
            if (GeneralizedComparisonResolvedFitState(state, candidate) !=
                RegressionComparisonFitState::Valid) continue;
            if (!GeneralizedComparisonModelCapabilities(state, candidate).hasAIC) continue;
            if (state.countComparison &&
                (candidate.exposure != model.exposure ||
                 candidate.offsetVariable != model.offsetVariable ||
                 candidate.trialsVariable != model.trialsVariable ||
                 ((!std::isfinite(candidate.trialsConstant) != !std::isfinite(model.trialsConstant)) ||
                  (std::isfinite(candidate.trialsConstant) &&
                   candidate.trialsConstant != model.trialsConstant)) ||
                 !SameIntegerSet(candidate.fit.rowsUsed, model.fit.rowsUsed))) continue;
            const double candidateValue = useAic ? candidate.fit.aic : candidate.fit.bic;
            if (std::isfinite(candidateValue)) minimum = std::min(minimum, candidateValue);
        }
        const double current = useAic ? model.fit.aic : model.fit.bic;
        cell.displayText = std::isfinite(current) && std::isfinite(minimum)
            ? FormatDoubleOrDash(current - minimum, 2) : "\u2014";
    } else {
        GeneralizedComparisonFitDisplayData fit;
        fit.ok = model.fit.ok;
        fit.family = model.fit.family.empty() ? model.family : model.fit.family;
        fit.link = model.fit.link.empty() ? model.link : model.fit.link;
        fit.n = model.fit.n;
        fit.nullDeviance = model.fit.nullDeviance;
        fit.residualDeviance = model.fit.residualDeviance;
        fit.dfResidual = model.fit.dfResidual;
        fit.aic = model.fit.aic;
        fit.bic = model.fit.bic;
        fit.dispersion = model.fit.dispersion;
        fit.logLik = model.fit.logLik;
        cell.displayText = GeneralizedComparisonFitDisplay(
            fit, GeneralizedAdjacentModelTest(state, modelIndex), fitRow);
    }
    if (cell.displayText.empty()) return cell;
    cell.displayState = cell.displayText == "\u2014"
        ? RegressionComparisonCellDisplayState::NotApplicable
        : RegressionComparisonCellDisplayState::Value;
    return cell;
}

void RefreshRegressionTermRowsFromFits(RegressionComparisonState &state)
{
    std::vector<std::string> candidateRows{"(Intercept)"};
    // Preserve the user's existing presentation order.  Rebuilding solely by
    // walking model 1, model 2, ... makes rows jump whenever a term is added
    // to an earlier model.  Child rows are reduced to their source term here;
    // RegressionComparisonTermRowsFromFits expands them again immediately
    // below using the current fitted coefficient rows.
    for (const std::string &term : state.termRows) {
        if (term.empty() || term == "(Intercept)") continue;
        const GLMCoefficientRow *row = DisplayRowForComparisonTerm(state, term);
        std::string source = row
            ? RegressionRowSourceTerm(row, term)
            : ModelTermBaseForDisplayRow(term);
        if (source.empty()) source = term;
        bool knownSource = false;
        for (const RegressionComparisonModel &model : state.models) {
            const std::vector<std::string> knownTerms = ComparisonPresentationCandidates(
                model.candidateTerms, model.terms);
            if (TermListContainsEquivalentModelTerm(knownTerms, source)) {
                knownSource = true;
                break;
            }
        }
        // A stale R coefficient can have a technical name without an '='
        // separator (for example groupTreatment:countryGreece).  Once its fit
        // has been invalidated there is no safe way to infer a new reference
        // identity from that name.  Do not promote it to a base term; the
        // replacement fit will regenerate the appropriate child row.
        if (!knownSource) continue;
        if (!TermListContainsEquivalentModelTerm(candidateRows, source)) {
            candidateRows.push_back(source);
        }
    }
    std::vector<std::vector<std::string>> includedTermsByModel;
    std::vector<std::vector<GLMCoefficientRow>> coefficientRowsByModel;
    includedTermsByModel.reserve(state.models.size());
    coefficientRowsByModel.reserve(state.models.size());
    for (const RegressionComparisonModel &model : state.models) {
        for (const std::string &term : ComparisonPresentationCandidates(
                 model.candidateTerms, model.terms)) {
            if (!TermListContainsEquivalentModelTerm(candidateRows, term)) {
                candidateRows.push_back(term);
            }
        }
        includedTermsByModel.push_back(model.terms);
        // Do not rebuild factor/interaction child rows from a fit that belongs
        // to the previous specification. The base comparison term remains
        // available and the replacement fit will restore its current rows.
        coefficientRowsByModel.push_back(model.isStale
            ? std::vector<GLMCoefficientRow>() : model.fit.coefficients);
    }
    state.termRows = RegressionComparisonTermRowsFromFits(
        candidateRows, includedTermsByModel, coefficientRowsByModel);
}

void PruneRegressionComparisonRowsForCurrentTypes(RegressionComparisonState &state)
{
    std::vector<std::string> filtered;
    auto keep = [&](const std::string &term) {
        if (std::find(filtered.begin(), filtered.end(), term) == filtered.end()) filtered.push_back(term);
    };
    for (const std::string &term : state.termRows) {
        if (term == "(Intercept)") { keep(term); continue; }
        const GLMCoefficientRow *row = DisplayRowForComparisonTerm(state, term);
        std::string source = row ? RegressionRowSourceTerm(row, term) : ModelTermBaseForDisplayRow(term);
        if (source.empty()) source = term;
        const bool derived = source != term || term.find('=') != std::string::npos;
        bool sourceIncluded = false;
        bool sourceCanHaveDerivedRows = false;
        for (std::size_t modelIndex = 0; modelIndex < state.models.size(); ++modelIndex) {
            const RegressionComparisonModel &model = state.models[modelIndex];
            if (!ModelIncludesTerm(model, source)) continue;
            sourceIncluded = true;
            const std::string sourceType = RegressionComparisonModelTermType(
                state, static_cast<int>(modelIndex), source);
            sourceCanHaveDerivedRows = sourceCanHaveDerivedRows ||
                sourceType == "factor" || ModelTermTypeIsInteraction(sourceType);
        }
        if (derived && !sourceIncluded) continue;
        if (derived && !sourceCanHaveDerivedRows) continue;
        keep(term);
    }
    if (filtered.empty()) filtered.push_back("(Intercept)");
    state.termRows = filtered;
}

void InferRegressionComparisonTermTypesFromFitRows(RegressionComparisonState &state)
{
    for (RegressionComparisonModel &model : state.models) {
        for (const GLMCoefficientRow &row : model.fit.coefficients) {
            if (row.sourceTerm.empty() || row.sourceTerm == "NA" || row.sourceTerm == "(Intercept)") continue;
            const bool factorRow = row.termType == "factor" || row.rowType == "factor_parent" ||
                row.rowType == "reference" || row.rowType == "factor_level";
            if (factorRow) {
                const std::string defaultType = ModelTermDisplayType(
                    &state.seed, row.sourceTerm, state.termTypes);
                if (defaultType != "factor") {
                    model.termTypeOverrides[row.sourceTerm] = "factor";
                }
            }
        }
    }
}

void EnsureRegressionComparisonFactorLevelRows(RegressionComparisonState &state,
                                               const std::string &term)
{
    const NumericVariable *variable = FindNumericVariable(state.seed, term);
    if (!variable) return;
    std::vector<double> levels;
    for (double value : variable->values) {
        if (!std::isfinite(value)) continue;
        bool seen = false;
        for (double existing : levels) {
            if (std::fabs(existing - value) <= 1.0e-9) { seen = true; break; }
        }
        if (!seen) levels.push_back(value);
    }
    std::sort(levels.begin(), levels.end());
    if (levels.empty()) return;
    std::vector<std::string> rebuilt;
    for (const std::string &row : state.termRows) {
        if (row == term || ModelTermBaseForDisplayRow(row) != term || row.find('=') == std::string::npos) {
            if (std::find(rebuilt.begin(), rebuilt.end(), row) == rebuilt.end()) rebuilt.push_back(row);
        }
        if (row == term) {
            for (double level : levels) {
                const std::string levelText = FactorLevelTermValue(level);
                if (levelText.empty()) continue;
                const std::string levelTerm = term + "=" + levelText;
                if (std::find(rebuilt.begin(), rebuilt.end(), levelTerm) == rebuilt.end()) {
                    rebuilt.push_back(levelTerm);
                }
            }
        }
    }
    state.termRows = std::move(rebuilt);
}

bool ApplyRegressionComparisonTermType(RegressionComparisonState &state,
                                      int modelIndex,
                                      const std::string &term,
                                      const std::string &type,
                                      std::string *message)
{
    if (message) message->clear();
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size() ||
        term.empty() || IsInteractionTerm(term) ||
        (type != "numeric" && type != "factor")) return false;
    RegressionComparisonModel &model = state.models[static_cast<std::size_t>(modelIndex)];
    if (!ModelIncludesTerm(model, term)) return false;
    if (model.termTypes.empty()) model.termTypes = state.termTypes;
    const auto result = SetModelSpecificationTermType(
        model, term, type, RegressionComparisonPredictorMetadata(state, model, term));
    if (!result.ok) {
        if (message) {
            *message = result.message;
            if (message->find("too many distinct values") != std::string::npos) {
                const auto metadata = RegressionComparisonPredictorMetadata(state, model, term);
                *message = "Predictor `" + term + "` has " +
                    std::to_string(metadata.uniqueCount) +
                    " distinct numeric values, which is too many to treat safely as categorical.";
            }
        }
        return false;
    }
    if (!result.changed) return false;
    if (type == "factor") EnsureRegressionComparisonFactorLevelRows(state, term);
    SetRegressionComparisonActiveModel(state, modelIndex);
    InvalidateRegressionComparisonModel(state, model);
    PruneRegressionComparisonRowsForCurrentTypes(state);
    if (type == "factor") EnsureRegressionComparisonFactorLevelRows(state, term);
    return true;
}

bool ToggleRegressionComparisonPredictorCentering(RegressionComparisonState &state,
                                                  int modelIndex,
                                                  const std::string &term)
{
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size() ||
        term.empty() || IsInteractionTerm(term)) return false;
    RegressionComparisonModel &model = state.models[static_cast<std::size_t>(modelIndex)];
    if (!ModelIncludesTerm(model, term) ||
        RegressionComparisonModelTermType(state, modelIndex, term) == "factor") return false;
    if (model.termTypes.empty()) model.termTypes = state.termTypes;
    const bool centered = model.centeredPredictors.count(term) == 0;
    const auto result = SetModelSpecificationPredictorCentered(model, term, centered);
    if (!result.ok || !result.changed) return false;
    SetRegressionComparisonActiveModel(state, modelIndex);
    InvalidateRegressionComparisonModel(state, model);
    return true;
}

const GLMCoefficientRow *DisplayRowForComparisonTerm(const RegressionComparisonState &state,
                                                     const std::string &term)
{
    for (const RegressionComparisonModel &model : state.models) {
        const GLMCoefficientRow *row = CoefficientForTerm(model.fit.coefficients, term);
        if (row) return row;
    }
    return nullptr;
}

std::string RegressionComparisonDisplayLabel(const RegressionComparisonState &state,
                                             const std::string &term)
{
    return RegressionRowDisplayLabel(DisplayRowForComparisonTerm(state, term), term);
}

std::string RegressionComparisonSourceTerm(const RegressionComparisonState &state,
                                           const std::string &term)
{
    return RegressionRowSourceTerm(DisplayRowForComparisonTerm(state, term), term);
}

std::string RegressionComparisonTermDisplay(const RegressionComparisonModel &model,
                                            const std::string &term)
{
    bool factorParent = false;
    if (term != "(Intercept)" && term.find('=') == std::string::npos) {
        for (const GLMCoefficientRow &row : model.fit.coefficients) {
            if (row.sourceTerm == term && row.term != term) {
                factorParent = true;
                break;
            }
        }
    }
    if (factorParent) {
        return TermListContainsEquivalentModelTerm(model.terms, term)
            ? "" : "\u2014";
    }
    return RegressionCoefficientDisplay(model.fit, model.terms, term);
}

RegressionComparisonTermCellState RegressionComparisonTermCell(
    const RegressionComparisonState &state,
    int modelIndex,
    const std::string &term)
{
    RegressionComparisonTermCellState cell;
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size()) {
        return cell;
    }
    const RegressionComparisonModel &model = state.models[static_cast<std::size_t>(modelIndex)];
    const GLMCoefficientRow *sharedRow = DisplayRowForComparisonTerm(state, term);
    const std::string source = RegressionRowSourceTerm(sharedRow, term);
    cell.factorLevel = sharedRow &&
        (sharedRow->rowType == "factor_level" || sharedRow->rowType == "reference");
    cell.factorParent = term != "(Intercept)" && !cell.factorLevel &&
        RegressionComparisonModelTermType(state, modelIndex, source) == "factor";
    cell.termIncluded = term == "(Intercept)" || ModelIncludesTerm(model, source);
    cell.inclusionControlAvailable = term != "(Intercept)" && !cell.factorLevel &&
        term == source;

    if (!cell.termIncluded ||
        RegressionComparisonResolvedFitState(state, model) != RegressionComparisonFitState::Valid ||
        cell.factorParent) {
        return cell;
    }

    const GLMCoefficientRow *row = CoefficientForTerm(model.fit.coefficients, term);
    if (!row && source != term) row = CoefficientForTerm(model.fit.coefficients, source);
    if (!row) return cell;
    if (row->rowType == "reference") {
        cell.displayState = RegressionComparisonCellDisplayState::NotApplicable;
        cell.displayText = "\u2014";
        return cell;
    }
    if (row->rowType != "coefficient" && row->rowType != "factor_level") return cell;
    cell.coefficientValue = row->estimate;
    cell.displayText = RegressionCoefficientDisplay(model.fit, model.terms, term);
    if (!cell.displayText.empty() && cell.displayText != "\u2014") {
        cell.displayState = RegressionComparisonCellDisplayState::Value;
    } else if (cell.displayText == "\u2014") {
        cell.displayState = RegressionComparisonCellDisplayState::NotApplicable;
    }
    return cell;
}

RegressionComparisonFitCellState RegressionComparisonFitCell(
    const RegressionComparisonState &state,
    int modelIndex,
    int fitRow)
{
    RegressionComparisonFitCellState cell;
    if (modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= state.models.size()) return cell;
    const RegressionComparisonModel &model = state.models[static_cast<std::size_t>(modelIndex)];
    if (RegressionComparisonResolvedFitState(state, model) != RegressionComparisonFitState::Valid) {
        return cell;
    }

    if (fitRow >= 13 && fitRow <= 15) {
        const GLMFitSummary *previous = modelIndex > 0
            ? &state.models[static_cast<std::size_t>(modelIndex - 1)].fit : nullptr;
        cell.displayText = RegressionComparisonChangeDisplay(model.fit, previous, fitRow);
    } else if (fitRow == 16 || fitRow == 17) {
        const bool useAic = fitRow == 16;
        double minimum = INFINITY;
        for (const RegressionComparisonModel &candidate : state.models) {
            const double candidateValue = useAic ? candidate.fit.aic : candidate.fit.bic;
            if (candidate.fit.ok && std::isfinite(candidateValue)) minimum = std::min(minimum, candidateValue);
        }
        const double current = useAic ? model.fit.aic : model.fit.bic;
        cell.displayText = std::isfinite(current) && std::isfinite(minimum)
            ? FormatDoubleOrDash(current - minimum, 2) : "\u2014";
    } else {
        cell.displayText = RegressionComparisonFitDisplay(
            model.fit, RegressionAdjacentModelTest(state, modelIndex), fitRow);
    }
    if (cell.displayText.empty()) return cell;
    cell.displayState = cell.displayText == "\u2014"
        ? RegressionComparisonCellDisplayState::NotApplicable
        : RegressionComparisonCellDisplayState::Value;
    return cell;
}

ComparisonCopyTable BuildRegressionComparisonCopyTable(
    const RegressionComparisonState &state)
{
    ComparisonCopyTable table;
    table.title = state.title.empty() ? "Compare Linear Models" : state.title;
    table.response = state.response;
    for (const RegressionComparisonModel &model : state.models) {
        table.modelLabels.push_back(model.label);
        table.modelResponses.push_back(RegressionComparisonEffectiveResponse(state, model));
    }
    for (const std::string &term : state.termRows) {
        table.termLabels.push_back(RegressionComparisonDisplayLabel(state, term));
        std::vector<std::string> cells;
        cells.reserve(state.models.size());
        for (std::size_t modelIndex = 0; modelIndex < state.models.size(); ++modelIndex) {
            cells.push_back(RegressionComparisonTermCell(
                state, static_cast<int>(modelIndex), term).displayText);
        }
        table.termDisplaysByRow.push_back(std::move(cells));
    }
    for (int fitRow : RegressionComparisonVisibleFitRows(state.showInformationCriteria)) {
        table.fitLabels.push_back(RegressionComparisonFitLabel(
            fitRow, state.multipleImputation));
        std::vector<std::string> cells;
        cells.reserve(state.models.size());
        for (std::size_t modelIndex = 0; modelIndex < state.models.size(); ++modelIndex) {
            cells.push_back(RegressionComparisonFitCell(
                state, static_cast<int>(modelIndex), fitRow).displayText);
        }
        table.fitDisplaysByRow.push_back(std::move(cells));
    }
    table.footnote = RegressionComparisonFootnote();
    return table;
}

std::string DefaultGeneralizedFamilyForResponse(const PlotModel &model, const std::string &response)
{
    const NumericVariable *var = FindNumericVariable(model, response);
    if (!var) return "gaussian";
    return DefaultGeneralizedFamilyForValues(var->values);
}

std::set<int> GLMRowsUsedSet(const GLMFitSummary &fit)
{
    return std::set<int>(fit.rowsUsed.begin(), fit.rowsUsed.end());
}

} // namespace core
} // namespace rlispstat
