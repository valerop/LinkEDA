#ifndef RLISPSTAT_CORE_MIXED_MODEL_H
#define RLISPSTAT_CORE_MIXED_MODEL_H

#include <string>
#include <utility>
#include <vector>

#include "analysis_scope.h"

namespace rlispstat {
namespace core {

struct DataFrameModel;

struct NativeMixedRandomSpec {
    std::string group;
    std::vector<std::string> terms;
    std::string covariance = "|";
};

struct NativeMixedModelState {
    std::string id;
    std::string group;
    AnalysisScope dataScope;
    bool dataScopeCaptured = false;
    std::string modelType;
    std::string response;
    std::vector<std::string> fixedEffects;
    std::vector<NativeMixedRandomSpec> randomEffects;
    std::string method = "REML";
    std::string family = "binomial";
    std::string link = "logit";
    std::string reportText;
};

struct MixedModelReportSection {
    std::string title;
    std::vector<std::vector<std::string>> rows;
};

struct MixedModelReportState {
    std::string id;
    std::string group;
    std::string modelType;
    std::string title;
    std::string dataset;
    std::string response;
    std::string scope;
    std::string estimation;
    std::string family;
    std::string link;
    std::string formula;
    std::vector<std::pair<std::string, std::string>> meta;
    std::vector<MixedModelReportSection> sections;
    std::string rawText;
};

struct MixedModelInitialStateResult {
    bool ok = false;
    NativeMixedModelState state;
    std::string message;
};

MixedModelInitialStateResult BuildInitialMixedModelState(const DataFrameModel &df,
                                                         const std::string &group,
                                                         bool generalized,
                                                         long long serial);
MixedModelReportState ParseMixedModelReport(const std::string &modelId,
                                             const std::string &group,
                                             const std::string &modelType,
                                             const std::string &text);
const MixedModelReportSection *FindMixedModelReportSection(const MixedModelReportState &state,
                                                           const std::string &title);
std::vector<std::pair<std::string, std::string>> MixedModelFitSummaryItems(
    const MixedModelReportState &state);
std::string MixedModelReportStatusLine(const MixedModelReportState &state);
std::string MixedModelCopyTableTitle();
std::string MixedModelNoNativeStateStatus();
std::string MixedModelRequiresRandomEffectStatus();
std::string MixedModelAddFixedEffectTitle();
std::string MixedModelAddRandomEffectTitle();
std::string MixedModelAddRandomSlopeTitle();
std::string MixedModelFixedEffectTitle();
std::string MixedModelRandomEffectTitle();
std::string MixedModelGeneralizedWindowTitle();
std::string MixedModelLinearWindowTitle();
std::string MixedModelWindowTitle();
std::string MixedModelResponseMenuTitle();
std::string MixedModelChangeFixedEffectTitle();
std::string MixedModelChangeFixedEffectMenuTitle();
std::string MixedModelNoAvailablePredictorsLabel();
std::string MixedModelNoAvailableResponseVariablesLabel();
std::string MixedModelNoAvailableFixedEffectsLabel();
std::string MixedModelNoAvailableReplacementsLabel();
std::string MixedModelChangeGroupingVariableTitle();
std::string MixedModelChangeGroupingVariableMenuTitle();
std::string MixedModelNoAvailableGroupingVariablesLabel();
std::string MixedModelNoAvailableRandomSlopesLabel();
std::string MixedModelRemoveRandomSlopeTitle();
std::string MixedModelRemoveRandomSlopeMenuTitle();
std::string MixedModelNoRandomSlopesToRemoveLabel();
std::string MixedModelAddFixedEffectDrawLabel();
std::string MixedModelAddRandomEffectDrawLabel();

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_MIXED_MODEL_H
