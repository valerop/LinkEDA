#include "glm_model.h"
#include "string_utils.h"

#include "command_model.h"
#include "dataset_model.h"
#include "format_model.h"
#include "model_terms.h"
#include "statistics_model.h"

#include <algorithm>
#include <cmath>
#include <cctype>
#include <cstdlib>
#include <iomanip>
#include <limits>
#include <set>
#include <sstream>
#include <utility>

namespace rlispstat {
namespace core {

namespace {

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

std::vector<std::string> GeneralizedLinksForFamily(const std::string &family)
{
    if (family == "gaussian") return {"identity", "log", "inverse"};
    if (family == "binomial") return {"logit", "probit", "cloglog", "cauchit", "log"};
    if (family == "poisson") return {"log", "identity", "sqrt"};
    if (family == "Gamma") return {"inverse", "identity", "log"};
    if (family == "inverse.gaussian") return {"1/mu^2", "inverse", "identity", "log"};
    if (family == "quasibinomial") return {"logit", "probit", "cloglog", "cauchit", "log"};
    if (family == "quasipoisson") return {"log", "identity", "sqrt"};
    return {"logit"};
}

std::string DefaultGeneralizedLink(const std::string &family)
{
    if (family == "gaussian") return "identity";
    if (family == "Gamma") return "inverse";
    if (family == "inverse.gaussian") return "1/mu^2";
    if (family == "poisson" || family == "quasipoisson") return "log";
    return "logit";
}

bool IsValidGeneralizedFamily(const std::string &family)
{
    static const std::set<std::string> families = {
        "gaussian", "binomial", "poisson", "Gamma", "inverse.gaussian",
        "quasibinomial", "quasipoisson"
    };
    return families.find(family) != families.end();
}

bool IsValidGeneralizedLink(const std::string &family, const std::string &link)
{
    std::vector<std::string> links = GeneralizedLinksForFamily(family);
    return std::find(links.begin(), links.end(), link) != links.end();
}

std::string BinaryLinkId(BinaryLink link)
{
    return link == BinaryLink::Probit ? "probit" : "logit";
}

std::string BinaryLinkLabel(BinaryLink link)
{
    return link == BinaryLink::Probit ? "Probit" : "Logit";
}

bool ParseBinaryLink(const std::string &value, BinaryLink &link)
{
    if (value == "logit" || value == "Logit") {
        link = BinaryLink::Logit;
        return true;
    }
    if (value == "probit" || value == "Probit") {
        link = BinaryLink::Probit;
        return true;
    }
    return false;
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
        const std::string &value = column.values[index];
        if (DataCellIsMissing(value)) continue;
        if (counts.emplace(value, 0).second) levels.push_back(value);
        ++counts[value];
    }
    if (levels.size() != 2) {
        result.status = levels.empty()
            ? "The response has no observed non-missing values in the selected scope."
            : "Binary Regression requires exactly two observed response values in the selected scope.";
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
                                  const std::string &scope)
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
    return out.str();
}

std::string GeneralizedGLMFitSignature(const GeneralizedGLMState &state)
{
    std::ostringstream out;
    out << state.group << "|" << state.response << "|" << state.family << "|" << state.link
        << "|" << state.scope;
    if (state.binaryRegression) {
        out << "|binary|event=" << state.responseCoding.eventValue
            << "|reference=" << state.responseCoding.referenceValue;
    }
    for (const std::string &term : state.terms) {
        out << "|" << term;
    }
    out << "|types";
    for (const auto &entry : state.termTypes) {
        out << "|" << entry.first << "=" << entry.second;
    }
    return out.str();
}

std::string RegressionComparisonFitSignature(const RegressionComparisonState &state)
{
    std::ostringstream out;
    out << state.id << "|" << state.group << "|" << state.response << "|" << state.scope;
    out << "|types";
    for (const auto &entry : state.termTypes) {
        out << "|" << entry.first << "=" << entry.second;
    }
    out << "|models";
    for (const RegressionComparisonModel &model : state.models) {
        out << "|" << model.id << ":" << model.label << ":"
            << (model.response.empty() ? state.response : model.response);
        for (const std::string &term : model.includedTerms) {
            out << "+" << term;
        }
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
	for (term in names(term_types)) {
	  if (identical(term_types[[term]], "factor") && term %in% names(dat)) {
	    dat[[term]] <- factor(dat[[term]])
	  }
	}
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
coef_matrix$term <- rownames(summary_fit$coefficients)
coef_matrix$partial_r2 <- NA_real_
coef_matrix <- coef_matrix[, c("term", "estimate", "std_error", "statistic", "p_value", "partial_r2")]
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
if (nrow(rows)) {
  for (i in seq_len(nrow(rows))) {
    cat("ROW", field(rows$row_type[[i]]), field(rows$term[[i]]), field(rows$display_label[[i]]),
        field(rows$source_term[[i]]), field(rows$level[[i]]), field(rows$reference_level[[i]]),
        field(rows$term_type[[i]]), safe_num(rows$estimate[[i]]), safe_num(rows$std_error[[i]]),
        field(rows$statistic_name[[i]]), safe_num(rows$statistic[[i]]), safe_num(rows$p_value[[i]]),
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
        if (cursor + 13 > lines.size()) return false;
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
        row.partialR2 = ParseOptionalDataCellDouble(lines[cursor++]);
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
    if (count == 0) return true;
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
    return true;
}

bool ReadGeneralizedRowsPayload(const std::vector<std::string> &lines,
                                std::size_t &cursor,
                                std::vector<GeneralizedGLMRow> &rows)
{
    if (cursor >= lines.size()) return false;
    long count = std::strtol(lines[cursor++].c_str(), nullptr, 10);
    if (count < 0) return false;
    rows.clear();
    rows.reserve(static_cast<std::size_t>(count));
    for (long i = 0; i < count; ++i) {
        if (cursor + 12 > lines.size()) return false;
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
    if (cursor < lines.size() && (lines[cursor] == "BINARY_V1" || lines[cursor] == "BINARY_V2")) {
        bool extendedBinaryMetadata = lines[cursor] == "BINARY_V2";
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
            state.termTests.push_back(row);
        }

        if (cursor >= lines.size()) return false;
        long diagnosticCount = std::strtol(lines[cursor++].c_str(), nullptr, 10);
        if (diagnosticCount < 0 || cursor + static_cast<std::size_t>(diagnosticCount) * 10 > lines.size()) return false;
        state.diagnostics.clear();
        for (long index = 0; index < diagnosticCount; ++index) {
            GeneralizedDiagnosticRow row;
            row.row = ParseOptionalDataCellInt(lines[cursor++]);
            row.observedLabel = lines[cursor++];
            row.observedBinary = ParseOptionalDataCellDouble(lines[cursor++]);
            row.observed = row.observedBinary;
            row.fitted = ParseOptionalDataCellDouble(lines[cursor++]);
            row.linearPredictor = ParseOptionalDataCellDouble(lines[cursor++]);
            row.devianceResidual = ParseOptionalDataCellDouble(lines[cursor++]);
            row.pearsonResidual = ParseOptionalDataCellDouble(lines[cursor++]);
            row.workingResidual = ParseOptionalDataCellDouble(lines[cursor++]);
            row.leverage = ParseOptionalDataCellDouble(lines[cursor++]);
            row.cooksDistance = ParseOptionalDataCellDouble(lines[cursor++]);
            state.diagnostics.push_back(row);
        }
    }
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
        ? std::vector<int>{0, 1, 2, 3, 9}
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

std::string GeneralizedComparisonFitLabel(int fitRow)
{
    static const char *labels[] = {
        "Family", "Link", "N", "Null dev.", "Residual dev.", "df residual",
        "AIC", "BIC", "Dispersion", "logLik", "\u0394df vs prev",
        "\u0394 deviance", "\u03C7\u00B2/F vs prev", "p vs prev"
    };
    if (fitRow >= 0 && fitRow < 14) return labels[fitRow];
    if (fitRow == 14) return "\u0394AIC";
    if (fitRow == 15) return "\u0394BIC";
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
        case 10: return test.ok ? std::to_string(test.df) : "\u2014";
        case 11: return test.ok ? FormatDoubleOrDash(test.delta, 3) : "\u2014";
        case 12: return test.ok ? FormatDoubleOrDash(test.statistic, 3) : "\u2014";
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
    return "Status: " + term + " is " + (type == "factor" ? "Factor." : "Numeric.");
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

std::string GLMRegressionCopyTableText(const GLMFitSummary &fit,
                                       const std::string &dependent,
                                       bool usesPooledGlobalWald)
{
    std::ostringstream out;
    out << "General Linear Model\n";
    out << "Response:\t" << dependent << "\n\n";
    out << "Variable\tb\tβ\tSE\tt\tp\tPartial R²\n";
    for (const GLMCoefficientRow &coef : fit.coefficients) {
        bool parentRow = coef.rowType == "factor_parent" || coef.rowType == "term_parent";
        bool referenceRow = coef.rowType == "reference";
        std::string structuralValue = parentRow ? "" : "—";
        out << (coef.displayLabel.empty() ? coef.term : coef.displayLabel) << "\t"
            << (parentRow || referenceRow ? structuralValue : FormatDoubleOrDash(coef.estimate, 4)) << "\t"
            << (parentRow || referenceRow ? structuralValue : FormatDoubleOrDash(coef.standardizedBeta, 3)) << "\t"
            << (parentRow || referenceRow ? structuralValue : FormatDoubleOrDash(coef.stdError, 4)) << "\t"
            << (parentRow || referenceRow ? structuralValue : FormatDoubleOrDash(coef.tValue, 3)) << "\t"
            << (parentRow || referenceRow ? structuralValue : FormatPValue(coef.pValue)) << "\t"
            << (parentRow || referenceRow ? structuralValue : FormatPercentOrDash(coef.partialR2, 1)) << "\n";
    }
    out << "\n";
    out << "N\t" << fit.n << "\n";
    out << "R²\t" << FormatPercentOrDash(fit.r2, 1) << "\n";
    out << "Adjusted R²\t" << FormatPercentOrDash(fit.adjR2, 1) << "\n";
    out << "s\t" << FormatDoubleOrDash(fit.sigma, 3) << "\n";
    out << "df residual\t" << fit.dfResidual << "\n";
    out << (usesPooledGlobalWald ? "D1/Wald" : "F") << "\t" << FormatDoubleOrDash(fit.globalF, 3) << "\n";
    out << "p\t" << FormatPValue(fit.globalP) << "\n";
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
    return out.str();
}

std::string GeneralizedGLMModelDetailsText(const GeneralizedGLMState &state)
{
    std::ostringstream out;
    out << "Technical fit details\n\n";
    out << "Family: " << state.family << "\n";
    out << "Link: " << state.link << "\n";
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
    out << "Dispersion: " << FormatDoubleOrDash(state.dispersion, 3);
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
    return "Status: interaction plot opened for " + term + ".";
}

std::string GeneralizedCoefficientDetailsStatus(const GeneralizedGLMRow &row,
                                                const std::string &defaultStatisticName)
{
    std::string source = row.sourceTerm.empty() || row.sourceTerm == "NA" ? row.term : row.sourceTerm;
    if (row.rowType == "factor_parent" || row.rowType == "term_parent") {
        std::string kind = row.rowType == "term_parent" ? "interaction term" : "factor term";
        return "Status: " + source + " is a " + kind + "; use its level rows for coefficients.";
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

std::string GeneralizedComparisonCoefficientDetailsStatus(const GeneralizedGLMRow &row)
{
    std::string source = row.sourceTerm.empty() ? row.term : row.sourceTerm;
    if (row.rowType == "factor_parent" || row.rowType == "term_parent") {
        return "Status: " + source + " is a factor term; use its level rows for coefficients.";
    }
    if (row.rowType == "reference") {
        return "Status: " + source + ": " + row.factorLevel +
            " is the reference category; b = \u2014; SE = \u2014; p = \u2014.";
    }
    return "Status: b = " + FormatDoubleOrDash(row.estimate, 4) +
        "; SE = " + FormatDoubleOrDash(row.stdError, 4) +
        "; " + row.statisticName + " = " + FormatDoubleOrDash(row.statistic, 3) +
        "; p " + FormatPValue(row.pValue);
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
        return "Status: " + row.sourceTerm + " is a factor term; use its level rows for coefficients.";
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
        "  Family = " + family + ", link = " + link;
}

std::string GeneralizedGLMFormattedOutputText(const GeneralizedGLMState &state,
                                              bool showModelDetails)
{
    std::ostringstream out;
    out << (state.binaryRegression ? "Binary Regression\n" : "Generalized Linear Model\n");
    out << "Response  " << (state.response.empty() ? "—" : state.response)
        << (state.binaryRegression ? "     Event  " + state.responseCoding.eventLabel +
             "     Reference  " + state.responseCoding.referenceLabel : "     Family  " + state.family)
        << "     Link  " << state.link
        << "     Residuals  " << state.residualType << "\n\n";
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
        out << std::left << std::setw(26) << "Variable / level"
            << std::right << std::setw(11) << "b"
            << std::setw(11) << "SE"
            << std::setw(9) << "z"
            << std::setw(11) << "Wald p"
            << std::setw(12) << "LR chi2"
            << std::setw(6) << "df"
            << std::setw(10) << "LR p"
            << std::setw(30) << (state.binaryLink == BinaryLink::Logit ? "OR / 95% CI for OR" : "95% CI") << "\n";
        out << std::string(126, '-') << "\n";
        for (const GeneralizedGLMRow &row : state.rows) {
            bool parent = row.rowType == "factor_parent" || row.rowType == "term_parent";
            bool reference = row.rowType == "reference";
            bool intercept = row.term == "(Intercept)" || row.sourceTerm == "(Intercept)";
            bool showTest = !intercept && (parent ||
                (!row.sourceTerm.empty() && row.term == row.sourceTerm));
            const BinaryTermTestRow *test = nullptr;
            const std::string source = row.sourceTerm.empty() ? row.term : row.sourceTerm;
            if (showTest) for (const BinaryTermTestRow &candidate : state.termTests) {
                if (candidate.term == source) { test = &candidate; break; }
            }
            const std::string dash = "—";
            std::string effect = dash;
            if (!parent && !reference) {
                effect = state.binaryLink == BinaryLink::Logit
                    ? FormatModelNumberOrDash(row.oddsRatio) + " [" + FormatModelNumberOrDash(row.oddsRatioLower) + ", " + FormatModelNumberOrDash(row.oddsRatioUpper) + "]"
                    : "[" + FormatModelNumberOrDash(row.ciLower) + ", " + FormatModelNumberOrDash(row.ciUpper) + "]";
            }
            std::string displayLabel = row.displayLabel.empty() ? row.term : row.displayLabel;
            if (reference && displayLabel.find("reference") == std::string::npos) displayLabel += " (reference)";
            out << std::left << std::setw(26) << displayLabel
                << std::right << std::setw(11) << (parent || reference ? dash : FormatModelNumberOrDash(row.estimate))
                << std::setw(11) << (parent || reference ? dash : FormatModelNumberOrDash(row.stdError))
                << std::setw(9) << (parent || reference ? dash : FormatModelNumberOrDash(row.statistic))
                << std::setw(11) << (parent || reference ? dash : FormatPValue(row.pValue))
                << std::setw(12) << (test ? FormatModelNumberOrDash(test->statistic) : dash)
                << std::setw(6) << (test ? std::to_string(test->df) : dash)
                << std::setw(10) << (test ? FormatPValue(test->pValue) : dash)
                << std::setw(30) << effect << "\n";
        }
        return out.str();
    } else {
        out << "N  " << state.n
            << "     Null deviance  " << FormatDoubleOrDash(state.nullDeviance, 3)
            << "     Residual deviance  " << FormatDoubleOrDash(state.residualDeviance, 3)
            << "     df residual  " << state.dfResidual << "\n";
        out << "\n";
    }
    out << std::left << std::setw(26) << "Variable / level"
        << std::setw(10) << "Type"
        << std::right << std::setw(12) << "b"
        << std::setw(12) << "SE"
        << std::setw(10) << state.statisticName
        << std::setw(10) << "p";
    if (state.binaryRegression) {
        out << std::setw(22) << "95% CI";
        if (state.binaryLink == BinaryLink::Logit) out << std::setw(12) << "OR" << std::setw(22) << "OR 95% CI";
    }
    out << "\n";
    out << std::string(80, '-') << "\n";
    for (const GeneralizedGLMRow &row : state.rows) {
        bool parentRow = row.rowType == "factor_parent" || row.rowType == "term_parent";
        bool referenceRow = row.rowType == "reference";
        bool interactionChildRow = ModelTermTypeIsInteraction(row.termType) &&
            row.rowType == "coefficient" &&
            !row.sourceTerm.empty() &&
            row.sourceTerm != row.term;
        std::string typeLabel = (row.rowType == "reference" || row.rowType == "factor_level" || interactionChildRow)
            ? ""
            : (row.termType.empty() ? "-" : row.termType);
        std::string structuralValue = parentRow ? "" : "—";
        out << std::left << std::setw(26) << (row.displayLabel.empty() ? row.term : row.displayLabel)
            << std::setw(10) << typeLabel
            << std::right << std::setw(12) << (parentRow || referenceRow ? structuralValue : FormatDoubleOrDash(row.estimate, 4))
            << std::setw(12) << (parentRow || referenceRow ? structuralValue : FormatDoubleOrDash(row.stdError, 4))
            << std::setw(10) << (parentRow || referenceRow ? structuralValue : FormatDoubleOrDash(row.statistic, 3))
            << std::setw(10) << (parentRow || referenceRow ? structuralValue : FormatPValue(row.pValue));
        if (state.binaryRegression) {
            out << std::setw(22) << (parentRow || referenceRow ? structuralValue :
                "[" + FormatDoubleOrDash(row.ciLower, 3) + ", " + FormatDoubleOrDash(row.ciUpper, 3) + "]");
            if (state.binaryLink == BinaryLink::Logit) {
                out << std::setw(12) << (parentRow || referenceRow ? structuralValue : FormatDoubleOrDash(row.oddsRatio, 3))
                    << std::setw(22) << (parentRow || referenceRow ? structuralValue :
                        "[" + FormatDoubleOrDash(row.oddsRatioLower, 3) + ", " + FormatDoubleOrDash(row.oddsRatioUpper, 3) + "]");
            }
        }
        out << "\n";
    }
    if (state.rows.empty()) {
        out << "Add at least one predictor to inspect coefficients.\n";
    }
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
        return FormatDouble(coef->estimate, 3) + SignificanceStars(coef->pValue);
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
    for (const GeneralizedGLMRow &row : rows) {
        if (row.term != term) continue;
        std::string source = GeneralizedRowSourceTerm(&row, term);
        if (row.rowType == "reference") {
            return "\u2014";
        }
        if (row.rowType == "factor_parent" || row.rowType == "term_parent") {
            return "";
        }
        if (!TermListContainsEquivalentModelTerm(includedTerms, source) &&
            source != "(Intercept)") {
            return "\u2014";
        }
        return FormatDoubleOrDash(row.estimate, 3) + SignificanceStars(row.pValue);
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
        std::string chosen = info->factorLevels.front();
        auto valueIt = input.factorValues.find(variable);
        if (valueIt != input.factorValues.end()) chosen = valueIt->second;
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
            coefficient.partialR2 = coefficient.tValue * coefficient.tValue /
                (coefficient.tValue * coefficient.tValue + result.dfResidual);
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
    const std::string prefix = "Untitled ";
    for (const std::string &label : labels) {
        if (label.rfind(prefix, 0) == 0) {
            next = std::max(next, std::atoi(label.substr(prefix.size()).c_str()) + 1);
        }
    }
    std::string candidate;
    do {
        candidate = "Untitled " + std::to_string(next++);
    } while (ComparisonLabelExists(labels, candidate));
    return candidate;
}

std::string UniqueRegressionComparisonCopyLabel(const std::vector<std::string> &labels,
                                                const std::string &label)
{
    if (label.empty() || label.rfind("Untitled ", 0) == 0 || label.rfind("Model ", 0) == 0) {
        return NextUntitledComparisonLabel(labels);
    }
    std::string candidate = label + " copy";
    if (!ComparisonLabelExists(labels, candidate)) return candidate;
    int copy = 2;
    do {
        candidate = label + " copy " + std::to_string(copy++);
    } while (ComparisonLabelExists(labels, candidate));
    return candidate;
}

std::string UniqueGeneralizedComparisonCopyLabel(const std::vector<std::string> &labels,
                                                 const std::string &label)
{
    std::string base = label.empty() ? "Untitled" : label;
    std::string candidate = base + " copy";
    int serial = 2;
    while (ComparisonLabelExists(labels, candidate)) {
        candidate = base + " copy " + std::to_string(serial++);
    }
    return candidate;
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
        for (const GeneralizedGLMRow &row : fitRows) {
            std::string source = EffectiveGeneralizedRowSourceTerm(row, row.term);
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
    for (const auto &row : displayRows) {
        bool sourceIsKnownBase = std::find(baseTerms.begin(), baseTerms.end(), row.second) != baseTerms.end();
        if (!sourceIsKnownBase) {
            add(row.first);
        }
    }
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
    return kind;
}

bool LinearDiagnosticKindIsImplemented(const std::string &kind)
{
    std::string normalized = NormalizeLinearDiagnosticKind(kind);
    return normalized == "residuals_fitted" || normalized == "observed_fitted";
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
    return kind;
}

double GeneralizedResidualForDiagnostic(const GeneralizedDiagnosticRow &row,
                                        const std::string &residualType)
{
    if (residualType == "pearson") return row.pearsonResidual;
    if (residualType == "working") return row.workingResidual;
    return row.devianceResidual;
}

DiagnosticPlotData BuildLinearDiagnosticPlotData(const std::string &kind,
                                                 const std::vector<LinearOlsDiagnosticRow> &diagnostics,
                                                 int fitVersion,
                                                 int diagnosticsVersion)
{
    DiagnosticPlotData data;
    data.kind = "scatter";
    data.diagnosticKind = NormalizeLinearDiagnosticKind(kind);
    data.title = data.diagnosticKind == "observed_fitted" ? "Observed vs fitted" : "Residuals vs fitted";
    data.xLabel = "fitted";
    data.yLabel = data.diagnosticKind == "observed_fitted" ? "observed" : "residual";
    data.fitVersion = fitVersion;
    data.diagnosticsVersion = diagnosticsVersion;
    data.points.reserve(diagnostics.size());
    for (const LinearOlsDiagnosticRow &row : diagnostics) {
        double y = data.diagnosticKind == "observed_fitted" ? row.observed : row.residual;
        if (std::isfinite(row.fitted) && std::isfinite(y)) {
            data.points.push_back(DiagnosticPlotPoint{row.fitted, y, row.row});
        }
    }
    data.ok = !data.points.empty();
    if (!data.ok) {
        data.message = "The requested diagnostic has no finite points for this fitted model.";
    }
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

    if (data.diagnosticKind == "observed_fitted") {
        data.title = "Observed vs fitted";
        data.yLabel = "observed";
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
    } else if (data.diagnosticKind == "residual_histogram") {
        data.kind = "histogram";
        data.title = "Residual histogram";
        data.xLabel = residualType + " residual";
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
        std::vector<std::pair<double, int>> residuals;
        for (const GeneralizedDiagnosticRow &row : diagnostics) {
            double residual = GeneralizedResidualForDiagnostic(row, residualType);
            if (std::isfinite(residual)) residuals.push_back(std::make_pair(residual, row.row));
        }
        if (residuals.size() < 2) {
            data.message = "Normal Q-Q is not available because fewer than two residuals are finite.";
            return data;
        }
        std::sort(residuals.begin(), residuals.end(),
                  [](const std::pair<double, int> &a, const std::pair<double, int> &b) {
                      return a.first < b.first;
                  });
        data.title = "Normal Q-Q of residuals";
        data.xLabel = "theoretical quantile";
        data.yLabel = residualType + " residual";
        for (std::size_t i = 0; i < residuals.size(); ++i) {
            double p = (static_cast<double>(i) + 0.5) / static_cast<double>(residuals.size());
            double q = NormalQuantileApprox(p);
            if (std::isfinite(q)) {
                data.points.push_back(DiagnosticPlotPoint{q, residuals[i].first, residuals[i].second});
            }
        }
    } else if (data.diagnosticKind == "scale_location") {
        data.title = "Scale-location";
        data.xLabel = "fitted";
        data.yLabel = "sqrt(|residual|)";
        for (const GeneralizedDiagnosticRow &row : diagnostics) {
            double residual = GeneralizedResidualForDiagnostic(row, residualType);
            if (std::isfinite(row.fitted) && std::isfinite(residual)) {
                data.points.push_back(DiagnosticPlotPoint{row.fitted, std::sqrt(std::fabs(residual)), row.row});
            }
        }
    } else if (data.diagnosticKind == "residuals_leverage") {
        data.title = "Residuals vs leverage";
        data.xLabel = "leverage";
        data.yLabel = residualType + " residual";
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
    return "Interaction plots currently support two-way interactions.";
}

std::string GLMInteractionNoFittedValuesStatus()
{
    return "The interaction plot could not be built because a component has no fitted values.";
}

std::string GLMInteractionFactorNoLevelsStatus()
{
    return "The interaction plot could not be built because a factor has no fitted levels.";
}

std::string GLMInteractionTypeNotAvailableStatus()
{
    return "This interaction plot type is not available.";
}

std::string GLMInteractionNoFiniteValuesStatus()
{
    return "The interaction plot has no finite fitted values.";
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
    return "Factor predictor `" + term + "` is not available.";
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
    return "The regression comparison is not available.";
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
    return "General Linear Model";
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

std::string GLMAddTermTitle()
{
    return "Add term";
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
    return "Family:";
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
    return "Interaction Plot";
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
    return "Family";
}

std::string GLMComparisonLinkMenuTitle()
{
    return "Link";
}

std::string GLMComparisonFamilyMenuItemTitle()
{
    return "Family...";
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

std::string GLMRegressionPartialRSquaredLabel()
{
    return "Partial R\u00B2";
}

std::string GLMInteractionsMenuTitle()
{
    return "Interactions";
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

bool HasModelTerm(const GroupModelState &state, const std::string &term)
{
    return TermListContainsEquivalentModelTerm(state.terms, term);
}

void MarkGroupModelChanged(GroupModelState &state)
{
    state.modelVersion += 1;
    state.isStale = true;
    state.precomputed = false;
    state.multipleImputation = false;
    state.imputationCount = 0;
    state.title.clear();
    state.note.clear();
    state.rFitPending = false;
    state.lastRFitSignature.clear();
}

GroupModelState &EnsureGroupModelState(std::map<std::string, GroupModelState> &states,
                                       const std::string &group,
                                       const PlotModel *seed)
{
    GroupModelState &state = states[group];
    if (state.group.empty()) {
        state.group = group;
        if (seed) {
            state.dependent = seed->yLabel;
            if (!seed->xLabel.empty() && seed->xLabel != state.dependent) {
                state.terms.push_back(seed->xLabel);
            }
        }
    }
    return state;
}

std::string EncodePooledRegressionComparisonModelSpec(const RegressionComparisonState &state)
{
    std::vector<CommandModelSpec> models;
    models.reserve(state.models.size());
    for (const RegressionComparisonModel &model : state.models) {
        std::string response = model.response.empty() ? state.response : model.response;
        models.push_back(CommandModelSpec{model.label, response, model.includedTerms});
    }
    return EncodeCommandModelSpecTable(models);
}

bool HasRegressionTerm(const RegressionComparisonState &state, const std::string &term)
{
    return TermListContainsEquivalentModelTerm(state.termRows, term);
}

bool ModelIncludesTerm(const RegressionComparisonModel &model, const std::string &term)
{
    return TermListContainsEquivalentModelTerm(model.includedTerms, term);
}

void AddRegressionTermRow(RegressionComparisonState &state, const std::string &term)
{
    if (term.empty() || HasRegressionTerm(state, term)) {
        return;
    }
    state.termRows.push_back(term);
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
    return TermListContainsEquivalentModelTerm(state.termRows, term);
}

bool GeneralizedModelIncludesTerm(const GeneralizedComparisonModel &model, const std::string &term)
{
    return TermListContainsEquivalentModelTerm(model.includedTerms, term);
}

void AddGeneralizedComparisonTermRow(GeneralizedComparisonState &state, const std::string &term)
{
    if (term.empty() || HasGeneralizedComparisonTerm(state, term)) {
        return;
    }
    state.termRows.push_back(term);
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

std::string GLMInteractionReportText(const GLMInteractionReport &report)
{
    std::ostringstream out;
    out << "Interaction term: " << report.term << "\n";
    if (!report.kind.empty()) out << "Type: " << report.kind << "\n";
    out << "Confidence level: 95%\n";
    out << "Multiple comparisons: Holm adjustment where applicable\n";
    out << "\n";
    if (!report.message.empty()) {
        out << report.message << "\n";
        return out.str();
    }
    for (const auto &section : report.sections) {
        out << section.first << "\n";
        out << "Effect\tEstimate\tSE\tz\tp\tp adjusted\tLower 95%\tUpper 95%\n";
        if (section.second.empty()) {
            out << "(none)\t\u2014\t\u2014\t\u2014\t\u2014\t\u2014\t\u2014\t\u2014\n";
        }
        for (const LinearFunctionEstimate &row : section.second) {
            out << row.label << "\t"
                << FormatDoubleOrDash(row.estimate, 4) << "\t"
                << FormatDoubleOrDash(row.stdError, 4) << "\t"
                << FormatDoubleOrDash(row.statistic, 3) << "\t"
                << FormatPValue(row.pValue) << "\t"
                << (std::isfinite(row.adjustedPValue) ? FormatPValue(row.adjustedPValue) : "\u2014") << "\t"
                << FormatDoubleOrDash(row.ciLower, 4) << "\t"
                << FormatDoubleOrDash(row.ciUpper, 4) << "\n";
        }
        out << "\n";
    }
    out << "Note: estimates are computed as linear functions of the fitted model coefficients (L beta) with standard errors from L V L'.\n";
    return out.str();
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
        if (const DataColumn *col = GLMDataColumn(dataframe, variable)) {
            raw = col->values;
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
        for (const std::string &value : raw) {
            if (DataCellIsMissing(value) || std::find(levels.begin(), levels.end(), value) != levels.end()) continue;
            levels.push_back(value);
        }
        SortFactorLevelsLikeR(levels);
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
    column.label = variable;
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
    total = std::min(total, column.values.size());
    columns.push_back(column);
    return true;
}

bool BuildInteractionDesignColumns(const PlotModel &model,
                                   const DataFrameModel *dataframe,
                                   const std::string &term,
                                   const std::map<std::string, std::string> &termTypes,
                                   std::vector<GLMDesignColumn> &columns,
                                   std::size_t &total,
                                   std::string &warning)
{
    const std::vector<std::string> parts = SplitInteractionTerm(term);
    if (parts.size() < 2) {
        return BuildMainEffectDesignColumns(model, dataframe, term, termTypes, columns, nullptr, total, warning);
    }
    std::vector<GLMDesignColumn> combined(1);
    combined.front().sourceTerm = term;
    combined.front().termType = ModelTermDisplayType(model, term, termTypes);
    combined.front().values.assign(total, 1.0);
    for (const std::string &part : parts) {
        std::vector<GLMDesignColumn> partColumns;
        GLMFactorInfo ignored;
        if (!BuildMainEffectDesignColumns(model, dataframe, part, termTypes, partColumns, &ignored, total, warning)) return false;
        if (partColumns.empty()) return true;
        std::vector<GLMDesignColumn> next;
        for (const GLMDesignColumn &left : combined) {
            for (const GLMDesignColumn &right : partColumns) {
                GLMDesignColumn column;
                column.label = left.label.empty() ? right.label : left.label + ":" + right.label;
                column.sourceTerm = term;
                column.termType = ModelTermDisplayType(model, term, termTypes);
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
                                     const std::string &scope)
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
            if (!BuildInteractionDesignColumns(model, dataframe, term, termTypes, design, total, fit.warning)) return fit;
        } else {
            GLMFactorInfo factorInfo;
            std::vector<GLMDesignColumn> termColumns;
            if (!BuildMainEffectDesignColumns(model, dataframe, term, termTypes, termColumns, &factorInfo, total, fit.warning)) return fit;
            if (!factorInfo.levels.empty()) factorInfos.push_back(std::move(factorInfo));
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
    LinearOlsResult ols = FitLinearOls(LinearOlsInput{x, y, rowIds});
    fit.n = ols.n; fit.dfModel = ols.dfModel; fit.dfResidual = ols.dfResidual;
    fit.r2 = ols.r2; fit.adjR2 = ols.adjR2; fit.globalF = ols.globalF; fit.globalP = ols.globalP;
    fit.ssRegression = ols.ssRegression; fit.ssResidual = ols.ssResidual; fit.msRegression = ols.msRegression;
    fit.msResidual = ols.msResidual; fit.rmse = ols.rmse; fit.sigma = ols.sigma; fit.aic = ols.aic; fit.bic = ols.bic;
    fit.beta = ols.beta; fit.covariance = ols.covariance; fit.warning = ols.warning;
    fit.designLabels.push_back("(Intercept)");
    for (const GLMDesignColumn &column : design) fit.designLabels.push_back(column.label);
    for (const LinearOlsDiagnosticRow &diag : ols.diagnostics) fit.diagnostics.push_back(diag);
    if (!ols.ok) return fit;
    std::vector<GLMCoefficientRow> raw;
    for (std::size_t j = 0; j < p; ++j) {
        GLMCoefficientRow row;
        if (j == 0) { row.term = "(Intercept)"; row.sourceTerm = "(Intercept)"; row.termType = "intercept"; row.displayLabel = "(Intercept)"; }
        else { const GLMDesignColumn &column = design[j - 1]; row.term = column.label; row.sourceTerm = column.sourceTerm; row.termType = column.termType; row.displayLabel = column.label; row.factorLevel = column.factorLevel; row.referenceLevel = column.referenceLevel; }
        if (j < ols.coefficients.size()) { const LinearOlsCoefficient &c = ols.coefficients[j]; row.estimate = c.estimate; row.standardizedBeta = c.standardizedBeta; row.stdError = c.stdError; row.tValue = c.tValue; row.pValue = c.pValue; row.partialR2 = c.partialR2; }
        raw.push_back(std::move(row));
    }
    auto findRaw = [&](const std::string &term) -> const GLMCoefficientRow * { for (const auto &row : raw) if (row.term == term) return &row; return nullptr; };
    auto rowsForSourceTerm = [&](const std::string &source) { std::vector<GLMCoefficientRow> rows; for (const auto &row : raw) if (row.sourceTerm == source) rows.push_back(row); return rows; };
    if (const GLMCoefficientRow *intercept = findRaw("(Intercept)")) fit.coefficients.push_back(*intercept);
    for (const std::string &term : terms) {
        auto factor = std::find_if(factorInfos.begin(), factorInfos.end(), [&](const GLMFactorInfo &info) { return info.term == term; });
        if (factor != factorInfos.end()) {
            GLMCoefficientRow parent; parent.term = term; parent.sourceTerm = term; parent.termType = "factor"; parent.rowType = "factor_parent"; parent.displayLabel = term; parent.referenceLevel = factor->levels.front(); fit.coefficients.push_back(parent);
            for (std::size_t i = 0; i < factor->levels.size(); ++i) {
                const std::string dummy = term + "=" + factor->levels[i];
                if (i == 0) { GLMCoefficientRow ref; ref.term = dummy; ref.sourceTerm = term; ref.termType = "factor"; ref.rowType = "reference"; ref.displayLabel = "  " + factor->levels[i]; ref.factorLevel = factor->levels[i]; ref.referenceLevel = factor->levels[i]; fit.coefficients.push_back(ref); }
                else if (const GLMCoefficientRow *level = findRaw(dummy)) { GLMCoefficientRow row = *level; row.rowType = "factor_level"; row.displayLabel = "  " + factor->levels[i]; fit.coefficients.push_back(std::move(row)); }
            }
        } else if (IsInteractionTerm(term)) {
            std::vector<GLMCoefficientRow> rows = rowsForSourceTerm(term);
            if (!rows.empty()) { GLMCoefficientRow parent; parent.term = term; parent.sourceTerm = term; parent.termType = ModelTermDisplayType(model, term, termTypes); parent.rowType = "term_parent"; parent.displayLabel = term; fit.coefficients.push_back(parent); for (GLMCoefficientRow &row : rows) { row.rowType = "coefficient"; row.termType = parent.termType; row.displayLabel = "  " + row.term; fit.coefficients.push_back(std::move(row)); } }
        } else if (const GLMCoefficientRow *row = findRaw(term)) {
            fit.coefficients.push_back(*row);
        }
    }
    fit.ok = true;
    return fit;
}

GLMInteractionReport BuildGLMInteractionReport(const PlotModel &model,
                                               const DataFrameModel *dataframe,
                                               const std::string &term,
                                               const std::vector<std::string> &terms,
                                               const std::map<std::string, std::string> &termTypes,
                                               const GLMFitSummary &fit,
                                               double confidenceLevel)
{
    GLMInteractionReport report;
    report.term = term;
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
    auto levels = [&](const std::string &variable) {
        std::vector<std::string> result; std::set<std::string> seen;
        if (const DataColumn *col = GLMDataColumn(dataframe, variable)) {
            for (std::size_t i = 0; i < col->values.size(); ++i) if (GLMRowIncluded(rows, (int)i + 1) && !DataCellIsMissing(col->values[i]) && seen.insert(col->values[i]).second) result.push_back(col->values[i]);
        } else if (const NumericVariable *var = GLMNumericVariable(model, variable)) {
            for (std::size_t i = 0; i < var->values.size(); ++i) if (GLMRowIncluded(rows, (int)i + 1) && std::isfinite(var->values[i]) && seen.insert(FactorLevelTermValue(var->values[i])).second) result.push_back(FactorLevelTermValue(var->values[i]));
        }
        return result;
    };
    auto design = [&](const std::map<std::string, double> &numeric, const std::map<std::string, std::string> &factor, std::vector<double> &out) {
        ScenarioDesignInput input; input.terms = terms; input.termTypes = termTypes; input.numericValues = numeric; input.factorValues = factor; input.expectedSize = fit.beta.size();
        for (const std::string &variable : BaseVariablesFromModelTerms(AvailableVariableNames(model, dataframe), "", terms)) {
            ScenarioVariableInfo info; info.type = GLMBaseVariableType(&model, variable, termTypes);
            if (info.type == "factor") info.factorLevels = levels(variable);
            else { NumericSummary values = summary(variable); info.numericDefault = values.ok ? values.mean : 0.0; }
            input.variables[variable] = std::move(info);
        }
        return BuildScenarioDesignVector(input, out);
    };
    auto estimate = [&](const std::vector<double> &contrast, const std::string &label) { return EstimateLinearFunction(fit.beta, fit.covariance, contrast, label, confidenceLevel); };
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
        report.sections.push_back({"Simple slopes", slopes});
        std::vector<LinearFunctionEstimate> comparisons;
        for (std::size_t i = 0; i < contrasts.size(); ++i) for (std::size_t j = i + 1; j < contrasts.size(); ++j) comparisons.push_back(estimate(SubtractVectors(contrasts[i], contrasts[j]), groupLevels[i] + " - " + groupLevels[j] + " slope difference"));
        ApplyHolmAdjustedPValues(comparisons); report.sections.push_back({"Pairwise slope comparisons (Holm)", comparisons});
        std::vector<std::pair<std::string, double>> values{{"M", xValues.mean}}; if (std::isfinite(xValues.sd) && xValues.sd > 0.0) values = {{"M - SD", xValues.mean - xValues.sd}, {"M", xValues.mean}, {"M + SD", xValues.mean + xValues.sd}};
        std::vector<LinearFunctionEstimate> predictions; for (const std::string &level : groupLevels) for (const auto &value : values) predictions.push_back(prediction({{x, value.second}}, {{g, level}}, g + " = " + level + ", " + x + " = " + value.first));
        report.sections.push_back({"Predicted means", predictions});
    } else if (aType == "factor" && bType == "factor") {
        report.kind = "categorical by categorical";
        const std::vector<std::string> aLevels = levels(a), bLevels = levels(b);
        if (aLevels.empty() || bLevels.empty()) { report.message = "The interaction could not be interpreted because one factor has no fitted levels."; return report; }
        std::vector<LinearFunctionEstimate> predictions; for (const std::string &av : aLevels) for (const std::string &bv : bLevels) predictions.push_back(prediction({}, {{a, av}, {b, bv}}, a + " = " + av + ", " + b + " = " + bv));
        report.sections.push_back({"Adjusted predictions", predictions});
        std::vector<LinearFunctionEstimate> effectsA, effectsB;
        for (const std::string &bv : bLevels) for (std::size_t i = 0; i < aLevels.size(); ++i) for (std::size_t j = i + 1; j < aLevels.size(); ++j) { std::vector<double> c; if (difference({}, {{a, aLevels[i]}, {b, bv}}, {}, {{a, aLevels[j]}, {b, bv}}, c)) effectsA.push_back(estimate(c, aLevels[i] + " - " + aLevels[j] + " within " + b + " = " + bv)); }
        for (const std::string &av : aLevels) for (std::size_t i = 0; i < bLevels.size(); ++i) for (std::size_t j = i + 1; j < bLevels.size(); ++j) { std::vector<double> c; if (difference({}, {{a, av}, {b, bLevels[i]}}, {}, {{a, av}, {b, bLevels[j]}}, c)) effectsB.push_back(estimate(c, bLevels[i] + " - " + bLevels[j] + " within " + a + " = " + av)); }
        ApplyHolmAdjustedPValues(effectsA); ApplyHolmAdjustedPValues(effectsB); report.sections.push_back({"Simple effects of " + a + " (Holm)", effectsA}); report.sections.push_back({"Simple effects of " + b + " (Holm)", effectsB});
    } else {
        report.kind = "continuous by continuous";
        NumericSummary av = summary(a), bv = summary(b);
        if (!av.ok || !bv.ok) { report.message = "The interaction could not be interpreted because one component has no fitted values."; return report; }
        auto spread = [](const NumericSummary &v) { return std::isfinite(v.sd) && v.sd > 0.0 ? std::vector<std::pair<std::string, double>>{{"M - SD", v.mean - v.sd}, {"M", v.mean}, {"M + SD", v.mean + v.sd}} : std::vector<std::pair<std::string, double>>{{"M", v.mean}}; };
        const auto as = spread(av), bs = spread(bv); std::vector<LinearFunctionEstimate> slopesA, slopesB, predictions;
        for (const auto &value : bs) { std::vector<double> c; if (difference({{a, av.mean + 1.0}, {b, value.second}}, {}, {{a, av.mean}, {b, value.second}}, {}, c)) slopesA.push_back(estimate(c, "Slope of " + a + " when " + b + " = " + value.first)); }
        for (const auto &value : as) { std::vector<double> c; if (difference({{a, value.second}, {b, bv.mean + 1.0}}, {}, {{a, value.second}, {b, bv.mean}}, {}, c)) slopesB.push_back(estimate(c, "Slope of " + b + " when " + a + " = " + value.first)); }
        for (const auto &avalue : as) for (const auto &bvalue : bs) predictions.push_back(prediction({{a, avalue.second}, {b, bvalue.second}}, {}, a + " = " + avalue.first + ", " + b + " = " + bvalue.first));
        report.sections.push_back({"Conditional slopes of " + a, slopesA}); report.sections.push_back({"Conditional slopes of " + b, slopesB}); report.sections.push_back({"Predicted means", predictions});
    }
    report.ok = report.message.empty();
    return report;
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
            if (GLMRowIncluded(rows, (int)i + 1) && !DataCellIsMissing(col->values[i]) &&
                seen.insert(col->values[i]).second) {
                levels.push_back(col->values[i]);
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
        eligibility.message = "The selected categorical term has fewer than two observed fitted levels.";
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

clean <- function(x) gsub("[\\t\\r\\n]", " ", as.character(x))
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
  if (nlevels(droplevels(model_data[[selected_term]])) < 2L) stop("The selected categorical term has fewer than two fitted levels.", call. = FALSE)
  emm <- emmeans::emmeans(fit, specs = selected_term)
  pairwise <- emmeans::contrast(emm, method = "pairwise", adjust = "tukey")
  tab <- as.data.frame(summary(pairwise, infer = c(TRUE, TRUE), level = confidence_level))
  if (!nrow(tab)) stop("No estimable pairwise comparisons could be formed for the selected term.", call. = FALSE)
  cat("OK\n")
  cat("METHOD\tR: nlme::gls with varIdent by selected factor; emmeans pairwise EMMs\tTukey (emmeans)\n")
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
if (length(args) < 9L) stop("Invalid Binary Regression pairwise request.", call. = FALSE)
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
scale <- if (identical(link, "logit")) "odds ratio" else "probability difference"
if (identical(link, "probit")) grid <- emmeans::regrid(grid, transform = "response")
tab <- as.data.frame(summary(emmeans::contrast(grid, method = "pairwise", adjust = "tukey"), infer = c(TRUE, TRUE), level = confidence))
if (!nrow(tab)) stop("No estimable pairwise comparisons were returned.", call. = FALSE)
clean <- function(x) gsub("[\t\r\n]", " ", as.character(x)); safe <- function(x) if (length(x) != 1L || !is.finite(x)) "NA" else sprintf("%.17g", x)
levels_for <- function(label) { lev <- levels(dat[[selected_term]]); for (first in lev) { prefix <- paste0(first, " - "); if (startsWith(label, prefix)) { second <- substr(label, nchar(prefix) + 1L, nchar(label)); if (second %in% lev) return(c(first, second)) } }; c("", "") }
cat("OK\n"); cat("METHOD", paste0("R: stats::glm binomial(", link, ") + emmeans; ", scale), "Tukey (emmeans)", sep = "\t"); cat("\n")
for (i in seq_len(nrow(tab))) {
  contrast <- as.character(tab$contrast[[i]]); lev <- levels_for(contrast)
  estimate <- tab$estimate[[i]]; lower <- tab$lower.CL[[i]]; upper <- tab$upper.CL[[i]]
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
    if (lines.empty() || lines.front() != "OK") {
        result.message = "R did not return pairwise-comparison results.";
        return false;
    }
    for (std::size_t i = 1; i < lines.size(); ++i) {
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
    out << "\nNote: for a one-factor model R uses Games-Howell. Otherwise, R fits a GLS model with a separate residual variance for each selected-factor level; emmeans obtains marginal means while holding numeric covariates at their fitted-sample means and averaging over other categorical terms.\n";
    return out.str();
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
        previous.fit, previous.response.empty() ? state.response : previous.response, previous.includedTerms);
    NestedLinearModelSummary right = BuildNestedLinearModelSummary(
        current.fit, current.response.empty() ? state.response : current.response, current.includedTerms);
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
        result.delta = current.comparisonDelta;
        result.statistic = current.comparisonStatistic;
        result.p = current.comparisonP;
        result.statisticName = "Chi-square";
        return result;
    }
    if (state.binaryComparison) return result;
    NestedGeneralizedModelSummary left = BuildNestedGeneralizedModelSummary(
        previous.fit.ok, previous.response.empty() ? state.response : previous.response,
        previous.fit.family, previous.fit.link, previous.includedTerms, previous.fit.rowsUsed,
        previous.fit.dfResidual, previous.fit.residualDeviance, previous.fit.dispersion);
    NestedGeneralizedModelSummary right = BuildNestedGeneralizedModelSummary(
        current.fit.ok, current.response.empty() ? state.response : current.response,
        current.fit.family, current.fit.link, current.includedTerms, current.fit.rowsUsed,
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
    if (!left.binaryRegression || !right.binaryRegression) return fail("Both models must be Binary Regression models.");
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

void RefreshGeneralizedTermRowsFromFits(GeneralizedComparisonState &state)
{
    std::vector<std::vector<std::string>> includedTermsByModel;
    std::vector<std::vector<GeneralizedGLMRow>> rowsByModel;
    includedTermsByModel.reserve(state.models.size());
    rowsByModel.reserve(state.models.size());
    for (const GeneralizedComparisonModel &model : state.models) {
        includedTermsByModel.push_back(model.includedTerms);
        rowsByModel.push_back(model.fit.rows);
    }
    state.termRows = GeneralizedComparisonTermRowsFromFits(includedTermsByModel, rowsByModel);
}

const GeneralizedGLMRow *DisplayRowForGeneralizedComparisonTerm(const GeneralizedComparisonState &state,
                                                                const std::string &term)
{
    for (const GeneralizedComparisonModel &model : state.models) {
        const GeneralizedGLMRow *row = GeneralizedRowForTerm(model.fit.rows, term);
        if (row) return row;
    }
    return nullptr;
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

void RefreshRegressionTermRowsFromFits(RegressionComparisonState &state)
{
    std::vector<std::vector<std::string>> includedTermsByModel;
    std::vector<std::vector<GLMCoefficientRow>> coefficientRowsByModel;
    includedTermsByModel.reserve(state.models.size());
    coefficientRowsByModel.reserve(state.models.size());
    for (const RegressionComparisonModel &model : state.models) {
        includedTermsByModel.push_back(model.includedTerms);
        coefficientRowsByModel.push_back(model.fit.coefficients);
    }
    state.termRows = RegressionComparisonTermRowsFromFits(
        state.termRows, includedTermsByModel, coefficientRowsByModel);
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
        std::string sourceType = ModelTermDisplayType(&state.seed, source, state.termTypes);
        bool sourceIncluded = false;
        for (const RegressionComparisonModel &model : state.models) {
            if (ModelIncludesTerm(model, source)) { sourceIncluded = true; break; }
        }
        if (derived && !sourceIncluded) continue;
        if (derived && sourceType != "factor" && !ModelTermTypeIsInteraction(sourceType)) continue;
        keep(term);
    }
    if (filtered.empty()) filtered.push_back("(Intercept)");
    state.termRows = filtered;
}

void InferRegressionComparisonTermTypesFromFitRows(RegressionComparisonState &state)
{
    for (const RegressionComparisonModel &model : state.models) {
        for (const GLMCoefficientRow &row : model.fit.coefficients) {
            if (row.sourceTerm.empty() || row.sourceTerm == "NA" || row.sourceTerm == "(Intercept)") continue;
            const bool factorRow = row.termType == "factor" || row.rowType == "factor_parent" ||
                row.rowType == "reference" || row.rowType == "factor_level";
            if (factorRow && state.termTypes.find(row.sourceTerm) == state.termTypes.end()) {
                state.termTypes[row.sourceTerm] = "factor";
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
                                      const std::string &term,
                                      const std::string &type)
{
    state.termTypes[term] = type;
    if (type == "factor") EnsureRegressionComparisonFactorLevelRows(state, term);
    bool affected = false;
    for (RegressionComparisonModel &model : state.models) {
        if (!ModelIncludesTerm(model, term)) continue;
        affected = true;
        model.modelVersion += 1;
        model.isStale = true;
    }
    PruneRegressionComparisonRowsForCurrentTypes(state);
    if (type == "factor") EnsureRegressionComparisonFactorLevelRows(state, term);
    return affected;
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

void PopulateMissingStandardizedBetas(GLMFitSummary &fit, const PlotModel &seed,
                                     const std::string &dependent)
{
    const NumericVariable *yvar = FindNumericVariable(seed, dependent);
    if (!yvar) return;
    for (GLMCoefficientRow &row : fit.coefficients) {
        if (std::isfinite(row.standardizedBeta)) continue;
        if (row.rowType != "coefficient") continue;
        if (row.term == "(Intercept)" || row.sourceTerm == "(Intercept)") continue;
        if (row.termType != "numeric") continue;
        std::string source = row.sourceTerm.empty() ? row.term : row.sourceTerm;
        if (source.find(':') != std::string::npos) continue;
        const NumericVariable *xvar = FindNumericVariable(seed, source);
        if (!xvar) continue;
        PairedSampleSDsResult sds =
            PairedFiniteSampleSDs(xvar->values, yvar->values, fit.rowsUsed);
        if (sds.ok) {
            row.standardizedBeta = row.estimate * sds.xSd / sds.ySd;
        }
    }
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
