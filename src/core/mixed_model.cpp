#include "mixed_model.h"

#include "dataset_model.h"
#include "string_utils.h"

#include <sstream>

namespace rlispstat {
namespace core {

namespace {

bool IsMixedModelSectionTitle(const std::string &line)
{
    return line == "Fixed effects" || line == "Random effects" ||
        line == "Groups" || line == "Model fit" || line == "Warnings";
}

std::vector<std::string> SplitReportColumns(const std::string &line)
{
    std::vector<std::string> out;
    auto append = [&](const std::string &value) {
        std::string cleaned = TrimCopy(value);
        if (cleaned.empty()) return;
        if (out.empty()) {
            const std::size_t first = value.find_first_not_of(" \t");
            if (first != std::string::npos && first > 0) cleaned = "  " + cleaned;
        }
        out.push_back(cleaned);
    };
    std::size_t start = 0;
    if (line.find('\t') != std::string::npos) {
        for (std::size_t i = 0; i < line.size(); ++i) {
            if (line[i] == '\t') {
                append(line.substr(start, i - start));
                start = i + 1;
            }
        }
    } else {
        std::size_t i = 0;
        while (i < line.size()) {
            if (line[i] != ' ') {
                ++i;
                continue;
            }
            std::size_t finish = i;
            while (finish < line.size() && line[finish] == ' ') ++finish;
            if (finish - i >= 2 && i > start && !TrimCopy(line.substr(start, i - start)).empty()) {
                append(line.substr(start, i - start));
                start = finish;
            }
            i = finish;
        }
    }
    append(line.substr(start));
    if (out.empty()) out.push_back(TrimCopy(line));
    return out;
}

std::string ReportFitValue(const MixedModelReportState &state, const std::string &key)
{
    const MixedModelReportSection *fit = nullptr;
    for (const MixedModelReportSection &section : state.sections) {
        if (section.title == "Model fit") {
            fit = &section;
            break;
        }
    }
    if (!fit) return std::string();
    for (std::size_t row = 1; row < fit->rows.size(); ++row) {
        if (fit->rows[row].size() >= 2 && fit->rows[row][0] == key) return fit->rows[row][1];
    }
    return std::string();
}

} // namespace

MixedModelInitialStateResult BuildInitialMixedModelState(const DataFrameModel &df,
                                                         const std::string &group,
                                                         bool generalized,
                                                         long long serial)
{
    MixedModelInitialStateResult result;

    std::string response;
    for (const DataColumn &col : df.columns) {
        if (generalized) {
            if (DataColumnLooksBinaryNumeric(col)) {
                response = col.name;
                break;
            }
        } else if (col.type == "numeric" && !DataColumnAllMissing(col)) {
            response = col.name;
            break;
        }
    }
    if (response.empty()) {
        result.message = generalized
            ? "Choose a binary 0/1 response before fitting a generalized mixed model."
            : "The active dataset has no numeric response variables.";
        return result;
    }

    std::string randomGroup;
    for (const DataColumn &col : df.columns) {
        if (col.name == response) continue;
        if (DataColumnLooksGroupingCandidate(col, df.rows)) {
            randomGroup = col.name;
            break;
        }
    }
    if (randomGroup.empty()) {
        result.message = "No suitable grouping variable was found. Choose a categorical variable with at least two levels.";
        return result;
    }

    std::vector<std::string> fixed;
    for (const DataColumn &col : df.columns) {
        if (col.name == response || col.name == randomGroup || DataColumnLooksLikeId(col)) continue;
        if (DataColumnObservedLevelCount(col) < 2) continue;
        fixed.push_back(col.name);
        break;
    }

    NativeMixedModelState state;
    state.id = std::string(generalized ? "glmm_" : "lmm_") + group + "_" + std::to_string(serial);
    state.group = group;
    state.modelType = generalized ? "generalized_linear_mixed_model" : "linear_mixed_model";
    state.response = response;
    state.fixedEffects = fixed;
    state.method = generalized ? "ML" : "REML";
    state.family = "binomial";
    state.link = "logit";
    NativeMixedRandomSpec randomSpec;
    randomSpec.group = randomGroup;
    randomSpec.terms.push_back("1");
    randomSpec.covariance = "|";
    state.randomEffects.push_back(randomSpec);

    result.ok = true;
    result.state = state;
    return result;
}

MixedModelReportState ParseMixedModelReport(const std::string &modelId,
                                            const std::string &group,
                                            const std::string &modelType,
                                            const std::string &text)
{
    MixedModelReportState state;
    state.id = modelId;
    state.group = group;
    state.modelType = modelType;
    state.title = modelType == "generalized_linear_mixed_model"
        ? "Generalized Linear Mixed Model"
        : "Linear Mixed Model";
    state.rawText = text;
    std::vector<std::string> lines = SplitLines(text);
    bool sawSection = false;
    MixedModelReportSection *current = nullptr;
    for (std::size_t i = 0; i < lines.size(); ++i) {
        const std::string rawLine = lines[i];
        std::string line = TrimCopy(rawLine);
        if (line.empty()) continue;
        if (i == 0 && (line == "Linear Mixed Model" || line == "Generalized Linear Mixed Model")) {
            state.title = line;
            continue;
        }
        if (IsMixedModelSectionTitle(line)) {
            sawSection = true;
            state.sections.push_back(MixedModelReportSection());
            state.sections.back().title = line;
            current = &state.sections.back();
            continue;
        }
        if (!sawSection) {
            std::size_t sep = line.find(':');
            if (sep != std::string::npos) {
                std::string key = TrimCopy(line.substr(0, sep));
                std::string value = TrimCopy(line.substr(sep + 1));
                if (key == "Formula") state.formula = value;
                else if (key == "Dataset") state.dataset = value;
                else if (key == "Response") state.response = value;
                else if (key == "Scope") state.scope = value;
                else if (key == "Estimation") state.estimation = value;
                else if (key == "Family") state.family = value;
                else if (key == "Link") state.link = value;
                state.meta.push_back(std::make_pair(key, value));
            }
        } else if (current) {
            current->rows.push_back(current->title == "Warnings"
                ? std::vector<std::string>{line}
                : SplitReportColumns(rawLine));
        }
    }
    return state;
}

const MixedModelReportSection *FindMixedModelReportSection(const MixedModelReportState &state,
                                                           const std::string &title)
{
    for (const MixedModelReportSection &section : state.sections) {
        if (section.title == title) return &section;
    }
    return nullptr;
}

std::vector<std::pair<std::string, std::string>> MixedModelFitSummaryItems(
    const MixedModelReportState &state)
{
    std::vector<std::pair<std::string, std::string>> items;
    const MixedModelReportSection *fit = FindMixedModelReportSection(state, "Model fit");
    if (!fit) return items;
    for (std::size_t row = 1; row < fit->rows.size(); ++row) {
        if (fit->rows[row].size() < 2) continue;
        const std::string &label = fit->rows[row][0];
        if (label == "Rows excluded" || label == "Convergence" ||
            label == "Estimation" || label == "Family" || label == "Link") {
            continue;
        }
        items.push_back(std::make_pair(label, fit->rows[row][1]));
    }
    return items;
}

std::string MixedModelReportStatusLine(const MixedModelReportState &state)
{
    const std::string n = ReportFitValue(state, "N");
    const std::string excluded = ReportFitValue(state, "Rows excluded");
    const std::string convergence = ReportFitValue(state, "Convergence");
    std::ostringstream out;
    out << "Status: fitted";
    if (!n.empty()) out << " on " << n << " rows";
    if (!excluded.empty()) out << ", " << excluded << " excluded";
    if (!state.scope.empty()) out << ", scope = " << state.scope;
    if (!state.estimation.empty()) {
        out << ", estimation = " << state.estimation;
    } else if (!state.family.empty()) {
        out << ", family = " << state.family;
        if (!state.link.empty()) out << "/" << state.link;
    }
    if (!convergence.empty()) out << ", convergence = " << convergence;
    return out.str();
}

std::string MixedModelCopyTableTitle()
{
    return "Copy Mixed Model Table";
}

std::string MixedModelNoNativeStateStatus()
{
    return "This mixed-model report has no editable native state. Reopen or refit the model to edit it directly.";
}

std::string MixedModelRequiresRandomEffectStatus()
{
    return "Mixed models require at least one random effect.";
}

std::string MixedModelAddFixedEffectTitle()
{
    return "Add Fixed Effect";
}

std::string MixedModelAddRandomEffectTitle()
{
    return "Add Random Effect";
}

std::string MixedModelAddRandomSlopeTitle()
{
    return "Add Random Slope";
}

std::string MixedModelFixedEffectTitle()
{
    return "Fixed Effect";
}

std::string MixedModelRandomEffectTitle()
{
    return "Random Effect";
}

std::string MixedModelGeneralizedWindowTitle()
{
    return "Generalized Linear Mixed Model";
}

std::string MixedModelLinearWindowTitle()
{
    return "Linear Mixed Model";
}

std::string MixedModelWindowTitle()
{
    return "Mixed Model";
}

std::string MixedModelResponseMenuTitle()
{
    return "Response";
}

std::string MixedModelChangeFixedEffectTitle()
{
    return "Change fixed effect";
}

std::string MixedModelChangeFixedEffectMenuTitle()
{
    return "Change Fixed Effect";
}

std::string MixedModelNoAvailablePredictorsLabel()
{
    return "No available predictors";
}

std::string MixedModelNoAvailableResponseVariablesLabel()
{
    return "No available response variables";
}

std::string MixedModelNoAvailableFixedEffectsLabel()
{
    return "No available fixed effects";
}

std::string MixedModelNoAvailableReplacementsLabel()
{
    return "No available replacements";
}

std::string MixedModelChangeGroupingVariableTitle()
{
    return "Change grouping variable";
}

std::string MixedModelChangeGroupingVariableMenuTitle()
{
    return "Change Grouping Variable";
}

std::string MixedModelNoAvailableGroupingVariablesLabel()
{
    return "No available grouping variables";
}

std::string MixedModelNoAvailableRandomSlopesLabel()
{
    return "No available random slopes";
}

std::string MixedModelRemoveRandomSlopeTitle()
{
    return "Remove random slope";
}

std::string MixedModelRemoveRandomSlopeMenuTitle()
{
    return "Remove Random Slope";
}

std::string MixedModelNoRandomSlopesToRemoveLabel()
{
    return "No random slopes to remove";
}

std::string MixedModelAddFixedEffectDrawLabel()
{
    return "+ Add fixed effect";
}

std::string MixedModelAddRandomEffectDrawLabel()
{
    return "+ Add random effect";
}

} // namespace core
} // namespace rlispstat
