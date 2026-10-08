#ifndef RLISPSTAT_CORE_ANALYSIS_INITIALIZATION_H
#define RLISPSTAT_CORE_ANALYSIS_INITIALIZATION_H

#include "dataset_model.h"

#include <map>
#include <set>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

// Semantic, UI-independent input types used when an analysis declares the
// variables it can consume.  These deliberately describe data requirements,
// not AppKit or WinUI controls.
enum class AnalysisVariableType {
    Any,
    Numeric,
    Categorical,
    Binary,
    DateTime,
    DateTimeOrNumeric,
    NumericOrOrdinal,
    NumericOrBinary,
    NumericOrdinalOrBinary,
    Grouping,
    TwoGroupGrouping,
    NumericOrCategorical
};

struct AnalysisSlotDefinition {
    std::string id;
    std::vector<std::string> acceptedRoles;
    AnalysisVariableType acceptedType = AnalysisVariableType::Any;
    std::size_t minimum = 0;
    // Zero means that the slot accepts any number of variables.
    std::size_t maximum = 1;
};

struct AnalysisDefinition {
    std::string id;
    std::vector<AnalysisSlotDefinition> slots;
    // Each inner group names slots whose selected variables must not overlap.
    // This is semantic validation (for example X and Y), not a UI rule.
    std::vector<std::vector<std::string>> mutuallyDistinctSlotGroups;
};

using AnalysisVariableRoles = std::map<std::string, std::string>;
using AnalysisSpecification = std::map<std::string, std::vector<std::string>>;

// Default variable roles are retained in documents for backwards
// compatibility, but their editor and analysis-initialization behaviour are
// experimental and disabled by default. Keep the gate here so platform
// adapters cannot accidentally apply stored roles to only some analyses.
bool DefaultVariableRolesExperimentalFeatureEnabled();
AnalysisVariableRoles DefaultVariableRolesForAnalysisInitialization(
    const AnalysisVariableRoles &roles);

// Returns the (possibly disabled) dataset-level defaults used to seed a fresh
// regression analysis. The caller receives a value copy, so subsequent
// analysis edits cannot mutate the dataset defaults.
AnalysisVariableRoles FreshRegressionAnalysisVariableRoles(
    const AnalysisVariableRoles &roles);

enum class AnalysisInitializationSource {
    Unresolved,
    ExistingSpecification,
    DatasetRole
};

struct InitialAnalysisSpecification {
    AnalysisSpecification variables;
    std::map<std::string, AnalysisInitializationSource> sources;
    std::set<std::string> unresolvedSlots;
    bool minimumValid = false;
};

// Shared row-list sizing semantics for analysis variable tables. Platform
// adapters use viewportHeight for the window and contentHeight for scrolling.
struct AnalysisVariableListLayout {
    std::size_t variableRows = 0;
    std::size_t totalRows = 0;
    std::size_t visibleRows = 0;
    double rowHeight = 24.0;
    double contentHeight = 0.0;
    double viewportHeight = 0.0;
    bool scrolls = false;
};

// Shared analysis definitions.  Frontends may render the returned slots in
// different controls, but must not add their own variable-choice policy.
enum class SharedAnalysisKind {
    DescriptiveTable,
    ContingencyTable,
    CorrelationMatrix,
    Dimensionality,
    ScaleAnalysis,
    QuickCluster,
    OneSampleComparison,
    IndependentSamplesComparison,
    PairedSamplesComparison,
    OneWayComparison,
    LinearModel,
    GeneralizedLinearModel,
    PositiveContinuousModel,
    ProportionModel,
    CountRegression,
    BinaryRegression,
    RegressionComparison,
    GeneralizedComparison,
    LinearMixedModel,
    GeneralizedMixedModel,
    Scatterplot,
    ScatterplotMatrix,
    ParallelCoordinates,
    Histogram,
    Boxplot,
    BarChart,
    TimeSeries,
    TrellisPlot,
    LinearModelTrellis
};

AnalysisDefinition SharedAnalysisDefinition(SharedAnalysisKind kind);

// Backend capability shared by all platform adapters.  A frontend must not
// maintain a separate MI allow/deny list for analyses that use this layer.
bool SharedAnalysisSupportsMultipleImputation(SharedAnalysisKind kind);

bool AnalysisVariableIsCompatible(const DataColumn &column,
                                  AnalysisVariableType acceptedType);

InitialAnalysisSpecification ResolveInitialAnalysisSpecification(
    const DataFrameModel &dataframe,
    const AnalysisDefinition &definition,
    const AnalysisVariableRoles &roles,
    const AnalysisSpecification *existingSpecification = nullptr);

std::vector<std::string> InitialAnalysisVariables(
    const InitialAnalysisSpecification &specification,
    const std::string &slot);
std::string InitialAnalysisVariable(
    const InitialAnalysisSpecification &specification,
    const std::string &slot);

AnalysisVariableListLayout BuildAnalysisVariableListLayout(
    std::size_t variableCount,
    bool includeAddRow = true,
    std::size_t maximumVisibleRows = 10,
    double rowHeight = 24.0,
    double verticalPadding = 0.0);

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_ANALYSIS_INITIALIZATION_H
