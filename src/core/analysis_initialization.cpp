#include "analysis_initialization.h"

#include <algorithm>
#include <cctype>
#include <cmath>

namespace rlispstat {
namespace core {

namespace {

std::string NormalizeRole(std::string role)
{
    std::transform(role.begin(), role.end(), role.begin(), [](unsigned char value) {
        return static_cast<char>(std::tolower(value));
    });
    if (role == "y" || role == "response") return "dependent";
    if (role == "x" || role == "predictor") return "independent";
    if (role == "group" || role == "split") return "grouping";
    return role;
}

bool RoleMatches(const std::string &role,
                 const std::vector<std::string> &accepted)
{
    const std::string normalized = NormalizeRole(role);
    return std::any_of(accepted.begin(), accepted.end(), [&](const std::string &candidate) {
        return NormalizeRole(candidate) == normalized;
    });
}

const DataColumn *FindColumn(const DataFrameModel &dataframe,
                             const std::string &name)
{
    auto found = std::find_if(dataframe.columns.begin(), dataframe.columns.end(),
        [&](const DataColumn &column) { return column.name == name; });
    return found == dataframe.columns.end() ? nullptr : &*found;
}

bool SlotCardinalityIsValid(const AnalysisSlotDefinition &slot,
                            std::size_t count)
{
    return count >= slot.minimum && (slot.maximum == 0 || count <= slot.maximum);
}

AnalysisSlotDefinition Slot(std::string id,
                            std::vector<std::string> roles,
                            AnalysisVariableType type,
                            std::size_t minimum,
                            std::size_t maximum)
{
    AnalysisSlotDefinition result;
    result.id = std::move(id);
    result.acceptedRoles = std::move(roles);
    result.acceptedType = type;
    result.minimum = minimum;
    result.maximum = maximum;
    return result;
}

} // namespace

bool DefaultVariableRolesExperimentalFeatureEnabled()
{
    // The feature remains implemented and document-compatible, but is hidden
    // until role/type compatibility can be defined consistently for every
    // analysis editor.
    return false;
}

AnalysisVariableRoles DefaultVariableRolesForAnalysisInitialization(
    const AnalysisVariableRoles &roles)
{
    if (!DefaultVariableRolesExperimentalFeatureEnabled()) return {};
    return roles;
}

AnalysisVariableRoles FreshRegressionAnalysisVariableRoles(
    const AnalysisVariableRoles &roles)
{
    return DefaultVariableRolesForAnalysisInitialization(roles);
}

AnalysisDefinition SharedAnalysisDefinition(SharedAnalysisKind kind)
{
    AnalysisDefinition definition;
    switch (kind) {
    case SharedAnalysisKind::DescriptiveTable:
        definition.id = "descriptive_table";
        definition.slots = {
            Slot("variables", {"dependent", "independent"}, AnalysisVariableType::Any, 1, 0),
            Slot("group", {"grouping"}, AnalysisVariableType::Categorical, 0, 1)
        };
        definition.mutuallyDistinctSlotGroups = {{"variables", "group"}};
        break;
    case SharedAnalysisKind::ContingencyTable:
        definition.id = "contingency_table";
        // Use semantic categories, including ordered factors. File import
        // infers repeated text labels without changing their storage class.
        definition.slots = {
            Slot("row", {"row", "dependent"}, AnalysisVariableType::Categorical, 1, 1),
            Slot("column", {"column", "independent"}, AnalysisVariableType::Categorical, 1, 1)
        };
        definition.mutuallyDistinctSlotGroups = {{"row", "column"}};
        break;
    case SharedAnalysisKind::CorrelationMatrix:
        definition.id = "correlation_matrix";
        definition.slots = {
            Slot("variables", {"dependent", "independent"}, AnalysisVariableType::Numeric, 2, 0)
        };
        break;
    case SharedAnalysisKind::Dimensionality:
        definition.id = "dimensionality";
        definition.slots = {
            Slot("variables", {"dependent", "independent"}, AnalysisVariableType::NumericOrdinalOrBinary, 2, 0)
        };
        break;
    case SharedAnalysisKind::ScaleAnalysis:
        definition.id = "scale_analysis";
        definition.slots = {
            // Scale Analysis is intentionally not seeded from generic
            // dependent/independent roles. Only an explicit scale-item role
            // or a saved specification may populate a new session.
            Slot("items", {"scale_item", "scale item", "scale-item", "item"},
                 AnalysisVariableType::NumericOrOrdinal, 2, 0)
        };
        break;
    case SharedAnalysisKind::QuickCluster:
        definition.id = "quick_cluster";
        definition.slots = {
            Slot("variables", {"dependent", "independent"}, AnalysisVariableType::Numeric, 1, 0)
        };
        break;
    case SharedAnalysisKind::OneSampleComparison:
        definition.id = "one_sample_comparison";
        definition.slots = {
            Slot("responses", {"dependent"}, AnalysisVariableType::NumericOrdinalOrBinary, 1, 0)
        };
        break;
    case SharedAnalysisKind::IndependentSamplesComparison:
        definition.id = "independent_samples_comparison";
        definition.slots = {
            Slot("responses", {"dependent"}, AnalysisVariableType::NumericOrdinalOrBinary, 1, 0),
            Slot("group", {"grouping", "independent"}, AnalysisVariableType::TwoGroupGrouping, 1, 1)
        };
        definition.mutuallyDistinctSlotGroups = {{"responses", "group"}};
        break;
    case SharedAnalysisKind::PairedSamplesComparison:
        definition.id = "paired_samples_comparison";
        definition.slots = {
            Slot("first", {"dependent"}, AnalysisVariableType::NumericOrdinalOrBinary, 1, 1),
            Slot("second", {"independent"}, AnalysisVariableType::NumericOrdinalOrBinary, 1, 1)
        };
        definition.mutuallyDistinctSlotGroups = {{"first", "second"}};
        break;
    case SharedAnalysisKind::OneWayComparison:
        definition.id = "one_way_comparison";
        definition.slots = {
            Slot("responses", {"dependent"}, AnalysisVariableType::Numeric, 1, 0),
            Slot("group", {"grouping", "independent"}, AnalysisVariableType::Grouping, 1, 1)
        };
        definition.mutuallyDistinctSlotGroups = {{"responses", "group"}};
        break;
    case SharedAnalysisKind::LinearModel:
    case SharedAnalysisKind::RegressionComparison:
        definition.id = kind == SharedAnalysisKind::LinearModel
            ? "linear_model" : "regression_comparison";
        definition.slots = {
            Slot("response", {"dependent"}, AnalysisVariableType::Numeric, 1, 1),
            Slot("predictors", {"independent"}, AnalysisVariableType::NumericOrCategorical, 1, 0)
        };
        definition.mutuallyDistinctSlotGroups = {{"response", "predictors"}};
        break;
    case SharedAnalysisKind::GeneralizedLinearModel:
    case SharedAnalysisKind::GeneralizedComparison:
        definition.id = kind == SharedAnalysisKind::GeneralizedLinearModel
            ? "generalized_linear_model" : "generalized_comparison";
        definition.slots = {
            Slot("response", {"dependent"}, AnalysisVariableType::Any, 1, 1),
            Slot("predictors", {"independent"}, AnalysisVariableType::NumericOrCategorical, 1, 0)
        };
        definition.mutuallyDistinctSlotGroups = {{"response", "predictors"}};
        break;
    case SharedAnalysisKind::PositiveContinuousModel:
    case SharedAnalysisKind::ProportionModel:
        definition.id = kind == SharedAnalysisKind::PositiveContinuousModel
            ? "positive_continuous_model" : "proportion_model";
        definition.slots = {
            Slot("response", {"dependent"}, AnalysisVariableType::Numeric, 1, 1),
            Slot("predictors", {"independent"}, AnalysisVariableType::NumericOrCategorical, 1, 0)
        };
        definition.mutuallyDistinctSlotGroups = {{"response", "predictors"}};
        break;
    case SharedAnalysisKind::CountRegression:
        definition.id = "count_regression";
        definition.slots = {
            Slot("response", {"dependent"}, AnalysisVariableType::Numeric, 1, 1),
            Slot("predictors", {"independent"}, AnalysisVariableType::NumericOrCategorical, 1, 0)
        };
        definition.mutuallyDistinctSlotGroups = {{"response", "predictors"}};
        break;
    case SharedAnalysisKind::BinaryRegression:
        definition.id = "binary_regression";
        definition.slots = {
            Slot("response", {"dependent"}, AnalysisVariableType::Binary, 1, 1),
            Slot("predictors", {"independent"}, AnalysisVariableType::NumericOrCategorical, 1, 0)
        };
        definition.mutuallyDistinctSlotGroups = {{"response", "predictors"}};
        break;
    case SharedAnalysisKind::LinearMixedModel:
    case SharedAnalysisKind::GeneralizedMixedModel:
        definition.id = kind == SharedAnalysisKind::LinearMixedModel
            ? "linear_mixed_model" : "generalized_mixed_model";
        definition.slots = {
            Slot("response", {"dependent"},
                 kind == SharedAnalysisKind::LinearMixedModel
                    ? AnalysisVariableType::Numeric : AnalysisVariableType::Binary,
                 1, 1),
            Slot("fixed_effects", {"independent"},
                 AnalysisVariableType::NumericOrCategorical, 0, 0),
            Slot("group", {"grouping"}, AnalysisVariableType::Categorical, 1, 1)
        };
        definition.mutuallyDistinctSlotGroups = {
            {"response", "fixed_effects"}, {"response", "group"}
        };
        break;
    case SharedAnalysisKind::Scatterplot:
        definition.id = "scatterplot";
        definition.slots = {
            Slot("y", {"dependent"}, AnalysisVariableType::Numeric, 1, 1),
            Slot("x", {"independent"}, AnalysisVariableType::Numeric, 1, 1)
        };
        definition.mutuallyDistinctSlotGroups = {{"y", "x"}};
        break;
    case SharedAnalysisKind::ScatterplotMatrix:
    case SharedAnalysisKind::ParallelCoordinates:
        definition.id = kind == SharedAnalysisKind::ScatterplotMatrix
            ? "scatterplot_matrix" : "parallel_coordinates";
        definition.slots = {
            Slot("variables", {"dependent", "independent"},
                 AnalysisVariableType::Numeric, 2, 0)
        };
        break;
    case SharedAnalysisKind::Histogram:
        definition.id = "histogram";
        definition.slots = {
            Slot("x", {"dependent", "independent"}, AnalysisVariableType::Numeric, 1, 1),
            Slot("conditioning", {"conditioning", "grouping"}, AnalysisVariableType::Categorical, 0, 0)
        };
        break;
    case SharedAnalysisKind::Boxplot:
        definition.id = "boxplot";
        definition.slots = {
            Slot("y", {"dependent"}, AnalysisVariableType::Numeric, 1, 1),
            Slot("x", {"grouping", "independent"}, AnalysisVariableType::Categorical, 0, 1),
            // A grouping role initializes the boxplot's primary grouping
            // axis.  It must not also be duplicated into conditioning.
            Slot("conditioning", {"conditioning"}, AnalysisVariableType::Categorical, 0, 0)
        };
        break;
    case SharedAnalysisKind::BarChart:
        definition.id = "bar_chart";
        definition.slots = {
            Slot("x", {"dependent", "independent", "grouping"}, AnalysisVariableType::Categorical, 1, 1),
            Slot("conditioning", {"conditioning"}, AnalysisVariableType::Categorical, 0, 0)
        };
        break;
    case SharedAnalysisKind::TimeSeries:
        definition.id = "time_series";
        definition.slots = {
            Slot("y", {"dependent"}, AnalysisVariableType::Numeric, 1, 1),
            Slot("x", {"independent"}, AnalysisVariableType::DateTimeOrNumeric, 1, 1),
            Slot("conditioning", {"conditioning", "grouping"}, AnalysisVariableType::Categorical, 0, 0)
        };
        definition.mutuallyDistinctSlotGroups = {{"y", "x", "conditioning"}};
        break;
    case SharedAnalysisKind::TrellisPlot:
        definition.id = "trellis_plot";
        definition.slots = {
            Slot("y", {"dependent"}, AnalysisVariableType::Numeric, 1, 1),
            Slot("x", {"independent"}, AnalysisVariableType::Numeric, 1, 1),
            Slot("conditioning", {"conditioning", "grouping"}, AnalysisVariableType::Categorical, 0, 0)
        };
        definition.mutuallyDistinctSlotGroups = {{"y", "x", "conditioning"}};
        break;
    case SharedAnalysisKind::LinearModelTrellis:
        definition.id = "linear_model_trellis";
        definition.slots = {
            Slot("y", {"dependent"}, AnalysisVariableType::Numeric, 1, 1),
            Slot("conditioning", {"conditioning", "grouping"}, AnalysisVariableType::Categorical, 1, 0),
            Slot("predictors", {"independent"}, AnalysisVariableType::NumericOrCategorical, 0, 0)
        };
        definition.mutuallyDistinctSlotGroups = {
            {"y", "conditioning", "predictors"}
        };
        break;
    }
    return definition;
}

bool SharedAnalysisSupportsMultipleImputation(SharedAnalysisKind kind)
{
    switch (kind) {
    case SharedAnalysisKind::LinearModel:
    case SharedAnalysisKind::RegressionComparison:
    case SharedAnalysisKind::GeneralizedLinearModel:
    case SharedAnalysisKind::GeneralizedComparison:
    case SharedAnalysisKind::PositiveContinuousModel:
    case SharedAnalysisKind::ProportionModel:
    case SharedAnalysisKind::CountRegression:
    case SharedAnalysisKind::BinaryRegression:
    case SharedAnalysisKind::ScaleAnalysis:
        return true;
    default:
        return false;
    }
}

bool AnalysisVariableIsCompatible(const DataColumn &column,
                                  AnalysisVariableType acceptedType)
{
    const bool numeric = NormalizeVariableType(column.type) == "numeric" &&
        DataColumnAllowsNumeric(column);
    const bool categorical = VariableTypeIsFactorLike(column.type);
    const bool ordinal = NormalizeVariableType(column.type) == "ordered";
    const bool datetime = NormalizeVariableType(column.type) == "datetime";
    const std::size_t observedLevels = DataColumnObservedLevelCount(column);
    const bool binary = (categorical || numeric) && observedLevels == 2;
    const bool grouping = categorical
        ? observedLevels >= 2
        : numeric && observedLevels >= 2 &&
            observedLevels <= std::max<std::size_t>(
                12, static_cast<std::size_t>(std::ceil(
                    std::sqrt(static_cast<double>(column.values.size())))));
    switch (acceptedType) {
    case AnalysisVariableType::Any: return true;
    case AnalysisVariableType::Numeric: return numeric;
    case AnalysisVariableType::Categorical: return categorical;
    case AnalysisVariableType::Binary: return binary;
    case AnalysisVariableType::DateTime: return datetime;
    case AnalysisVariableType::DateTimeOrNumeric: return datetime || numeric;
    case AnalysisVariableType::NumericOrOrdinal: return numeric || ordinal;
    case AnalysisVariableType::NumericOrBinary: return numeric || binary;
    case AnalysisVariableType::NumericOrdinalOrBinary:
        return numeric || ordinal || binary;
    case AnalysisVariableType::Grouping: return grouping;
    case AnalysisVariableType::TwoGroupGrouping:
        // A two-sample procedure consumes the complete grouping variable; it
        // does not silently choose a pair from a multi-level factor.  Count
        // observed values rather than declared factor levels so unused levels
        // do not make an otherwise binary grouping variable ineligible.
        return binary;
    case AnalysisVariableType::NumericOrCategorical: return numeric || categorical;
    }
    return false;
}

InitialAnalysisSpecification ResolveInitialAnalysisSpecification(
    const DataFrameModel &dataframe,
    const AnalysisDefinition &definition,
    const AnalysisVariableRoles &roles,
    const AnalysisSpecification *existingSpecification)
{
    InitialAnalysisSpecification result;
    for (const AnalysisSlotDefinition &slot : definition.slots) {
        std::vector<std::string> selected;
        bool usedExisting = false;
        if (existingSpecification) {
            auto existing = existingSpecification->find(slot.id);
            if (existing != existingSpecification->end()) {
                usedExisting = true;
                for (const std::string &name : existing->second) {
                    const DataColumn *column = FindColumn(dataframe, name);
                    if (column && AnalysisVariableIsCompatible(*column, slot.acceptedType) &&
                        std::find(selected.begin(), selected.end(), name) == selected.end())
                        selected.push_back(name);
                }
                if (slot.maximum != 0 && selected.size() > slot.maximum)
                    selected.clear();
            }
        }

        if (!usedExisting) {
            std::vector<std::string> candidates;
            for (const auto &[name, role] : roles) {
                const DataColumn *column = FindColumn(dataframe, name);
                if (!column || !RoleMatches(role, slot.acceptedRoles) ||
                    !AnalysisVariableIsCompatible(*column, slot.acceptedType)) continue;
                candidates.push_back(name);
            }
            // A single-value slot is resolved only by exactly one explicit
            // role candidate.  Dataset order is never used as a tie-breaker.
            if (slot.maximum == 1) {
                if (candidates.size() == 1) selected = std::move(candidates);
            } else {
                selected = std::move(candidates);
            }
        }

        result.variables[slot.id] = selected;
        if (!SlotCardinalityIsValid(slot, selected.size())) {
            result.unresolvedSlots.insert(slot.id);
            result.sources[slot.id] = AnalysisInitializationSource::Unresolved;
        } else if (usedExisting) {
            result.sources[slot.id] = AnalysisInitializationSource::ExistingSpecification;
        } else if (!selected.empty()) {
            result.sources[slot.id] = AnalysisInitializationSource::DatasetRole;
        } else {
            result.sources[slot.id] = AnalysisInitializationSource::Unresolved;
        }
    }
    for (const auto &distinctSlots : definition.mutuallyDistinctSlotGroups) {
        std::map<std::string, std::vector<std::string>> owners;
        for (const std::string &slot : distinctSlots) {
            const auto found = result.variables.find(slot);
            if (found == result.variables.end()) continue;
            for (const std::string &variable : found->second)
                owners[variable].push_back(slot);
        }
        for (const auto &[variable, slots] : owners) {
            (void)variable;
            if (slots.size() < 2) continue;
            for (const std::string &slot : slots) {
                result.unresolvedSlots.insert(slot);
                result.sources[slot] = AnalysisInitializationSource::Unresolved;
            }
        }
    }
    result.minimumValid = result.unresolvedSlots.empty();
    return result;
}

std::vector<std::string> InitialAnalysisVariables(
    const InitialAnalysisSpecification &specification,
    const std::string &slot)
{
    auto found = specification.variables.find(slot);
    return found == specification.variables.end()
        ? std::vector<std::string>{} : found->second;
}

std::string InitialAnalysisVariable(
    const InitialAnalysisSpecification &specification,
    const std::string &slot)
{
    auto values = InitialAnalysisVariables(specification, slot);
    return values.size() == 1 ? values.front() : std::string{};
}

AnalysisVariableListLayout BuildAnalysisVariableListLayout(
    std::size_t variableCount,
    bool includeAddRow,
    std::size_t maximumVisibleRows,
    double rowHeight,
    double verticalPadding)
{
    AnalysisVariableListLayout layout;
    layout.variableRows = variableCount;
    layout.totalRows = variableCount + (includeAddRow ? 1U : 0U);
    const std::size_t rowLimit = std::max<std::size_t>(1, maximumVisibleRows);
    layout.visibleRows = std::min(layout.totalRows, rowLimit);
    layout.rowHeight = std::max(1.0, rowHeight);
    layout.contentHeight = verticalPadding +
        layout.rowHeight * static_cast<double>(layout.totalRows);
    layout.viewportHeight = verticalPadding +
        layout.rowHeight * static_cast<double>(layout.visibleRows);
    layout.scrolls = layout.totalRows > rowLimit;
    return layout;
}

} // namespace core
} // namespace rlispstat
