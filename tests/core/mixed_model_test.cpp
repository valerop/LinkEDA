#include "../../src/core/dataset_model.h"
#include "../../src/core/mixed_model.h"

#include <cassert>
#include <string>
#include <utility>
#include <vector>

using rlispstat::core::BuildInitialMixedModelState;
using rlispstat::core::DataColumn;
using rlispstat::core::DataFrameModel;
using rlispstat::core::MixedModelCopyTableTitle;
using rlispstat::core::MixedModelNoNativeStateStatus;
using rlispstat::core::MixedModelRequiresRandomEffectStatus;
using rlispstat::core::MixedModelAddFixedEffectTitle;
using rlispstat::core::MixedModelAddRandomEffectTitle;
using rlispstat::core::MixedModelAddRandomSlopeTitle;
using rlispstat::core::MixedModelFixedEffectTitle;
using rlispstat::core::MixedModelRandomEffectTitle;
using rlispstat::core::MixedModelGeneralizedWindowTitle;
using rlispstat::core::MixedModelLinearWindowTitle;
using rlispstat::core::MixedModelWindowTitle;
using rlispstat::core::MixedModelResponseMenuTitle;
using rlispstat::core::MixedModelChangeFixedEffectTitle;
using rlispstat::core::MixedModelChangeFixedEffectMenuTitle;
using rlispstat::core::MixedModelNoAvailablePredictorsLabel;
using rlispstat::core::MixedModelNoAvailableResponseVariablesLabel;
using rlispstat::core::MixedModelNoAvailableFixedEffectsLabel;
using rlispstat::core::MixedModelNoAvailableReplacementsLabel;
using rlispstat::core::MixedModelChangeGroupingVariableTitle;
using rlispstat::core::MixedModelChangeGroupingVariableMenuTitle;
using rlispstat::core::MixedModelNoAvailableGroupingVariablesLabel;
using rlispstat::core::MixedModelNoAvailableRandomSlopesLabel;
using rlispstat::core::MixedModelRemoveRandomSlopeTitle;
using rlispstat::core::MixedModelRemoveRandomSlopeMenuTitle;
using rlispstat::core::MixedModelNoRandomSlopesToRemoveLabel;
using rlispstat::core::MixedModelAddFixedEffectDrawLabel;
using rlispstat::core::MixedModelAddRandomEffectDrawLabel;
using rlispstat::core::FindMixedModelReportSection;
using rlispstat::core::MixedModelFitSummaryItems;
using rlispstat::core::MixedModelReportStatusLine;
using rlispstat::core::ParseMixedModelReport;

static DataColumn column(std::string name, std::string type, std::vector<std::string> values)
{
    DataColumn out;
    out.name = std::move(name);
    out.type = std::move(type);
    out.values = std::move(values);
    return out;
}

int main()
{
    DataFrameModel df;
    df.group = "cars";
    df.rows = 4;
    df.columns.push_back(column("mpg", "numeric", {"21", "22", "18", "19"}));
    df.columns.push_back(column("school", "factor", {"a", "a", "b", "b"}));
    df.columns.push_back(column("y01", "numeric", {"0", "1", "0", "1"}));
    df.columns.push_back(column("wt", "numeric", {"2.1", "2.4", "3.0", "3.2"}));
    df.columns.push_back(column("id", "numeric", {"1", "2", "3", "4"}));

    auto lmm = BuildInitialMixedModelState(df, "cars", false, 123);
    assert(lmm.ok);
    assert(lmm.state.id == "lmm_cars_123");
    assert(lmm.state.modelType == "linear_mixed_model");
    assert(lmm.state.response == "mpg");
    assert(lmm.state.method == "REML");
    assert(lmm.state.randomEffects.size() == 1);
    assert(lmm.state.randomEffects[0].group == "school");
    assert(lmm.state.randomEffects[0].terms == std::vector<std::string>{"1"});
    assert(lmm.state.fixedEffects == std::vector<std::string>{"y01"});

    auto glmm = BuildInitialMixedModelState(df, "cars", true, 456);
    assert(glmm.ok);
    assert(glmm.state.id == "glmm_cars_456");
    assert(glmm.state.modelType == "generalized_linear_mixed_model");
    assert(glmm.state.response == "y01");
    assert(glmm.state.method == "ML");
    assert(glmm.state.family == "binomial");
    assert(glmm.state.link == "logit");

    DataFrameModel noGroup = df;
    noGroup.columns.erase(noGroup.columns.begin() + 1);
    noGroup.columns.erase(noGroup.columns.begin() + 1);
    auto missingGroup = BuildInitialMixedModelState(noGroup, "cars", false, 789);
    assert(!missingGroup.ok);
    assert(missingGroup.message.find("No suitable grouping variable") != std::string::npos);

    std::string report =
        "Linear Mixed Model\n"
        "Dataset: cars\n"
        "Response: mpg\n"
        "Formula: `mpg` ~ `wt` + (1 | `cyl`)\n"
        "Scope: all\n"
        "Estimation: REML\n"
        "\n"
        "Fixed effects\n"
        "Variable\tb\tSE\tt\tdf\tp\n"
        "(Intercept)\t34.10\t2.10\t16.24\t28.0\t< .001\n"
        "wt\t-3.20\t0.70\t-4.57\t28.0\t< .001\n"
        "\n"
        "Random effects\n"
        "Group\tEffect\tVariance\tSD\tCorr\n"
        "cyl\tIntercept\t1.2500\t1.1180\t-\n"
        "\n"
        "Model fit\n"
        "Statistic\tValue\n"
        "N\t32\n"
        "Groups\tcyl: 3 levels\n"
        "Estimation\tREML\n"
        "AIC\t166.4\n"
        "Rows excluded\t0\n"
        "Convergence\tconverged\n"
        "\n"
        "Warnings\n"
        "Warning: singular fit.\n";
    auto parsed = ParseMixedModelReport("mix_1", "cars", "linear_mixed_model", report);
    assert(parsed.id == "mix_1");
    assert(parsed.group == "cars");
    assert(parsed.title == "Linear Mixed Model");
    assert(parsed.dataset == "cars");
    assert(parsed.response == "mpg");
    assert(parsed.scope == "all");
    assert(parsed.estimation == "REML");
    assert(parsed.formula == "`mpg` ~ `wt` + (1 | `cyl`)");
    assert(parsed.meta.size() == 5);
    assert(parsed.sections.size() == 4);
    assert(parsed.sections[0].title == "Fixed effects");
    assert(parsed.sections[0].rows.size() == 3);
    assert(parsed.sections[0].rows[1][0] == "(Intercept)");
    assert(parsed.sections[1].rows[1][0] == "cyl");
    assert(FindMixedModelReportSection(parsed, "Model fit") != nullptr);
    const auto fitItems = MixedModelFitSummaryItems(parsed);
    assert(fitItems.size() == 3);
    assert(fitItems[0] == std::make_pair(std::string("N"), std::string("32")));
    assert(fitItems[1] == std::make_pair(std::string("Groups"), std::string("cyl: 3 levels")));
    assert(fitItems[2] == std::make_pair(std::string("AIC"), std::string("166.4")));
    assert(MixedModelReportStatusLine(parsed) ==
           "Status: fitted on 32 rows, 0 excluded, scope = all, estimation = REML, convergence = converged");
    assert(parsed.sections[3].rows[0][0] == "Warning: singular fit.");

    const std::string legacyAlignedReport =
        "Linear Mixed Model\n"
        "Dataset: cars\n"
        "Response: mpg\n"
        "Scope: all\n"
        "Estimation: REML\n\n"
        "Fixed effects\n"
        "Variable     Type       b      SE       t    df      p\n"
        "(Intercept)  -     26.6636  0.9718  27.437  29.0  <.001\n"
        "  six        -          -       -       -     -      -\n\n"
        "Model fit\n"
        "Statistic       Value\n"
        "N                  32\n"
        "Rows excluded       0\n"
        "Convergence  converged\n";
    auto legacy = ParseMixedModelReport("legacy", "cars", "linear_mixed_model", legacyAlignedReport);
    const auto *legacyFixed = FindMixedModelReportSection(legacy, "Fixed effects");
    assert(legacyFixed != nullptr);
    assert(legacyFixed->rows[0].size() == 7);
    assert(legacyFixed->rows[2][0] == "  six");
    assert(MixedModelFitSummaryItems(legacy)[0] ==
           std::make_pair(std::string("N"), std::string("32")));
    assert(MixedModelReportStatusLine(legacy) ==
           "Status: fitted on 32 rows, 0 excluded, scope = all, estimation = REML, convergence = converged");
    assert(MixedModelCopyTableTitle() == "Copy Mixed Model Table");
    assert(MixedModelNoNativeStateStatus() == "This mixed-model report has no editable native state. Reopen or refit the model to edit it directly.");
    assert(MixedModelRequiresRandomEffectStatus() == "Mixed models require at least one random effect.");
    assert(MixedModelAddFixedEffectTitle() == "Add Fixed Effect");
    assert(MixedModelAddRandomEffectTitle() == "Add Random Effect");
    assert(MixedModelAddRandomSlopeTitle() == "Add Random Slope");
    assert(MixedModelFixedEffectTitle() == "Fixed Effect");
    assert(MixedModelRandomEffectTitle() == "Random Effect");
    assert(MixedModelGeneralizedWindowTitle() == "Generalized Linear Mixed Model");
    assert(MixedModelLinearWindowTitle() == "Linear Mixed Model");
    assert(MixedModelWindowTitle() == "Mixed Model");
    assert(MixedModelResponseMenuTitle() == "Response");
    assert(MixedModelChangeFixedEffectTitle() == "Change fixed effect");
    assert(MixedModelChangeFixedEffectMenuTitle() == "Change Fixed Effect");
    assert(MixedModelNoAvailablePredictorsLabel() == "No available predictors");
    assert(MixedModelNoAvailableResponseVariablesLabel() == "No available response variables");
    assert(MixedModelNoAvailableFixedEffectsLabel() == "No available fixed effects");
    assert(MixedModelNoAvailableReplacementsLabel() == "No available replacements");
    assert(MixedModelChangeGroupingVariableTitle() == "Change grouping variable");
    assert(MixedModelChangeGroupingVariableMenuTitle() == "Change Grouping Variable");
    assert(MixedModelNoAvailableGroupingVariablesLabel() == "No available grouping variables");
    assert(MixedModelNoAvailableRandomSlopesLabel() == "No available random slopes");
    assert(MixedModelRemoveRandomSlopeTitle() == "Remove random slope");
    assert(MixedModelRemoveRandomSlopeMenuTitle() == "Remove Random Slope");
    assert(MixedModelNoRandomSlopesToRemoveLabel() == "No random slopes to remove");
    assert(MixedModelAddFixedEffectDrawLabel() == "+ Add fixed effect");
    assert(MixedModelAddRandomEffectDrawLabel() == "+ Add random effect");
    return 0;
}
