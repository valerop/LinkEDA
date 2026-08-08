#include "command_dispatcher.h"
#include "boxplot_model.h"
#include "barplot_model.h"
#include "dataset_protocol.h"
#include "histogram_model.h"
#include "model_terms.h"
#include "scatterplot_model.h"
#include "string_utils.h"
#include "trellis_scatterplot_model.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdlib>
#include <sstream>
#include <utility>

namespace rlispstat {
namespace core {

CommandDispatcher::CommandDispatcher(CommandDispatcherServices services)
    : services_(std::move(services)),
      session_(services_.session),
      plotCoordinator_(applicationState_)
{
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

std::string CommandDispatcher::dispatch(const std::vector<std::string> &lines)
{
    CommandRequest request = ParseCommandRequest(lines);
    if (request.name.empty()) {
        return "ERR empty command";
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
        out << "OK\t" << state.group << "|" << state.dependent << "|" << state.scope;
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
        applicationState_.correlationMatrices()[state.id] = state;
        return services_.selection.refitCorrelation(state.id)
            ? "OK\t" + state.id : "ERR no registered dataset/group for correlation matrix";
    }
    case CommandAction::CorrelationOpenStructured: {
        if (request.args.size() < 13) return "ERR malformed CORR_OPEN_STRUCTURED command";
        if (!services_.queries.groupSeed || !services_.ui.showCorrelationMatrix) {
            return std::nullopt;
        }
        CorrelationMatrixState state;
        std::size_t cursor = 0;
        state.id = request.args[cursor++];
        state.group = request.args[cursor++];
        state.title = request.args[cursor++];
        state.method = request.args[cursor++];
        state.missingMode = request.args[cursor++];
        state.showP = request.args[cursor++] == "TRUE";
        state.showPValue = request.args[cursor++] == "TRUE";
        state.showN = request.args[cursor++] == "TRUE";
        state.imputationCount = std::atoi(request.args[cursor++].c_str());
        state.poolingMethod = request.args[cursor++];
        state.note = request.args[cursor++];
        state.precomputed = true;
        state.multipleImputation = true;
        if (state.method != "pearson") return "ERR only Pearson correlations are supported";
        if (state.missingMode != "pairwise" && state.missingMode != "listwise") {
            return "ERR invalid missing-data mode";
        }
        if (!services_.queries.groupSeed(state.group, state.seed)) {
            return "ERR no registered dataset/group for correlation matrix";
        }
        state.hasSeed = true;
        if (cursor >= request.args.size()) return "ERR malformed structured correlation variable payload";
        const long variableCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
        if (variableCount < 0 || cursor + static_cast<std::size_t>(variableCount) > request.args.size()) {
            return "ERR invalid structured correlation variable count";
        }
        for (long index = 0; index < variableCount; ++index) {
            const std::string &variable = request.args[cursor++];
            if (!FindNumericVariable(state.seed, variable)) {
                return "ERR correlation variables must be numeric";
            }
            if (std::find(state.variables.begin(), state.variables.end(), variable) == state.variables.end()) {
                state.variables.push_back(variable);
            }
        }
        if (cursor >= request.args.size()) return "ERR malformed structured correlation cell payload";
        const long cellCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
        if (cellCount < 0) return "ERR invalid structured correlation cell count";
        for (long index = 0; index < cellCount; ++index) {
            if (cursor + 8 > request.args.size()) return "ERR malformed structured correlation cell";
            CorrelationCellResult cell;
            cell.xVariable = request.args[cursor++];
            cell.yVariable = request.args[cursor++];
            cell.r = ParseOptionalDataCellDouble(request.args[cursor++]);
            cell.p = ParseOptionalDataCellDouble(request.args[cursor++]);
            cell.n = std::atoi(request.args[cursor++].c_str());
            cell.status = request.args[cursor++];
            cell.detail = request.args[cursor++];
            const long rowCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (rowCount < 0 || cursor + static_cast<std::size_t>(rowCount) > request.args.size()) {
                return "ERR invalid structured correlation row count";
            }
            for (long row = 0; row < rowCount; ++row) cell.rowsUsed.push_back(std::atoi(request.args[cursor++].c_str()));
            state.cells.push_back(std::move(cell));
        }
        state.modelVersion += 1;
        applicationState_.correlationMatrices()[state.id] = state;
        services_.ui.showCorrelationMatrix(state.id);
        return "OK\t" + state.id;
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
        if (cursor >= request.args.size()) return "ERR invalid dimensionality variable count";
        const long count = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
        if (count < 0 || request.args.size() < cursor + static_cast<std::size_t>(count)) {
            return "ERR invalid dimensionality variable count";
        }
        for (long index = 0; index < count; ++index) {
            const std::string &variable = request.args[cursor + static_cast<std::size_t>(index)];
            if (!FindNumericVariable(state.seed, variable)) {
                return "ERR dimensionality variables must be numeric";
            }
            if (std::find(state.variables.begin(), state.variables.end(), variable) == state.variables.end()) {
                state.variables.push_back(variable);
            }
        }
        if (state.variables.size() < 2) {
            return "ERR principal components/factor analysis requires at least two numeric variables";
        }
        applicationState_.dimensionalityModels()[state.id] = state;
        return services_.selection.refitDimensionality(state.id)
            ? "OK\t" + state.id
            : "ERR no registered dataset/group for principal components/factor analysis";
    }
    case CommandAction::DimensionalityUpdate: {
        if (request.args.size() < 11) return "ERR malformed PCAFA_UPDATE command";
        if (!services_.ui.showDimensionality || !services_.ui.refreshDimensionalityPlots) {
            return std::nullopt;
        }
        std::size_t cursor = 0;
        const std::string id = request.args[cursor++];
        auto it = applicationState_.dimensionalityModels().find(id);
        if (it == applicationState_.dimensionalityModels().end()) {
            return "ERR unknown principal components/factor analysis model";
        }
        DimensionalityState updated = it->second;
        updated.group = request.args[cursor++];
        updated.method = request.args[cursor++];
        updated.missingMode = request.args[cursor++];
        updated.rotation = request.args[cursor++];
        updated.scope = request.args[cursor++];
        updated.scale = request.args[cursor++] != "FALSE";
        updated.componentCount = std::max(1, std::atoi(request.args[cursor++].c_str()));
        updated.status = request.args[cursor++];
        const long variableCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
        if (variableCount < 0 || cursor + static_cast<std::size_t>(variableCount) > request.args.size()) {
            return "ERR invalid PCAFA_UPDATE variable payload";
        }
        updated.variables.assign(request.args.begin() + static_cast<std::ptrdiff_t>(cursor),
                                 request.args.begin() + static_cast<std::ptrdiff_t>(cursor + variableCount));
        cursor += static_cast<std::size_t>(variableCount);
        auto readInts = [&](std::vector<int> &values) {
            if (cursor >= request.args.size()) return false;
            const long count = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (count < 0 || cursor + static_cast<std::size_t>(count) > request.args.size()) return false;
            values.clear();
            for (long index = 0; index < count; ++index) values.push_back(std::atoi(request.args[cursor++].c_str()));
            return true;
        };
        if (!readInts(updated.rowsUsed) || !readInts(updated.rowsExcluded)) {
            return "ERR malformed PCAFA_UPDATE row payload";
        }
        if (cursor >= request.args.size()) return "ERR missing PCAFA_UPDATE component count";
        const long componentCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
        if (componentCount < 0 || cursor + static_cast<std::size_t>(componentCount) * 5 > request.args.size()) {
            return "ERR malformed PCAFA_UPDATE component payload";
        }
        updated.components.clear();
        for (long index = 0; index < componentCount; ++index) {
            DimensionalityFitComponent component;
            component.index = std::atoi(request.args[cursor++].c_str());
            component.eigenvalue = ParseOptionalDataCellDouble(request.args[cursor++]);
            component.parallelEigenvalue = ParseOptionalDataCellDouble(request.args[cursor++]);
            component.variance = ParseOptionalDataCellDouble(request.args[cursor++]);
            component.cumulative = ParseOptionalDataCellDouble(request.args[cursor++]);
            updated.components.push_back(component);
        }
        if (cursor >= request.args.size()) return "ERR missing PCAFA_UPDATE loading count";
        const long loadingCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
        if (loadingCount < 0) return "ERR invalid PCAFA_UPDATE loading count";
        updated.loadings.clear();
        for (long index = 0; index < loadingCount; ++index) {
            if (cursor + 4 > request.args.size()) return "ERR malformed PCAFA_UPDATE loading payload";
            DimensionalityFitLoading loading;
            loading.variable = request.args[cursor++];
            loading.communality = ParseOptionalDataCellDouble(request.args[cursor++]);
            loading.uniqueness = ParseOptionalDataCellDouble(request.args[cursor++]);
            const long valueCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (valueCount < 0 || cursor + static_cast<std::size_t>(valueCount) > request.args.size()) {
                return "ERR invalid PCAFA_UPDATE loading values";
            }
            for (long value = 0; value < valueCount; ++value) {
                loading.values.push_back(ParseOptionalDataCellDouble(request.args[cursor++]));
            }
            updated.loadings.push_back(std::move(loading));
        }
        if (cursor >= request.args.size()) return "ERR missing PCAFA_UPDATE score count";
        const long scoreCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
        if (scoreCount < 0) return "ERR invalid PCAFA_UPDATE score count";
        updated.scores.clear();
        for (long index = 0; index < scoreCount; ++index) {
            if (cursor + 2 > request.args.size()) return "ERR malformed PCAFA_UPDATE score payload";
            DimensionalityFitScore score;
            score.row = std::atoi(request.args[cursor++].c_str());
            const long valueCount = std::strtol(request.args[cursor++].c_str(), nullptr, 10);
            if (valueCount < 0 || cursor + static_cast<std::size_t>(valueCount) > request.args.size()) {
                return "ERR invalid PCAFA_UPDATE score values";
            }
            for (long value = 0; value < valueCount; ++value) {
                score.values.push_back(ParseOptionalDataCellDouble(request.args[cursor++]));
            }
            if (!score.values.empty()) score.x = score.values[0];
            score.y = score.values.size() >= 2 ? score.values[1] : 0.0;
            updated.scores.push_back(std::move(score));
        }
        updated.rFitPending = false;
        updated.lastRFitSignature = DimensionalityFitSignature(
            updated.method, updated.missingMode, updated.rotation, updated.scope,
            updated.scale, updated.componentCount, updated.variables);
        updated.modelVersion += 1;
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
        std::vector<std::string> variables;
        for (long index = 0; index < count; ++index) {
            const std::string &variable = request.args[static_cast<std::size_t>(2 + index)];
            if (!FindNumericVariable(state.seed, variable)) {
                return "ERR dimensionality variables must be numeric";
            }
            if (std::find(variables.begin(), variables.end(), variable) == variables.end()) {
                variables.push_back(variable);
            }
        }
        if (variables.size() < 2) {
            return "ERR principal components/factor analysis requires at least two numeric variables";
        }
        state.variables = variables;
        state.componentCount = std::min(state.componentCount, static_cast<int>(state.variables.size()));
        if (!services_.selection.refitDimensionality) return std::nullopt;
        return services_.selection.refitDimensionality(state.id)
            ? "OK" : "ERR unknown principal components/factor analysis model";
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
        if (variables.empty()) return "ERR quick cluster requires at least one numeric variable";
        state.variables = variables;
        if (!services_.selection.refitDendrogram) return std::nullopt;
        return services_.selection.refitDendrogram(state.id) ? "OK" : "ERR unknown dendrogram";
    }
    case CommandAction::DendrogramOpen: {
        if (request.args.size() < 7) return "ERR malformed DENDRO_OPEN command";
        if (!services_.queries.groupSeed || !services_.selection.refitDendrogram) {
            return std::nullopt;
        }
        DendrogramState state;
        state.id = request.args[0];
        state.group = request.args[1];
        state.distance = request.args[2] == "correlation" ? "euclidean" : request.args[2];
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
        if (state.variables.empty()) return "ERR quick cluster requires at least one numeric variable";
        applicationState_.dendrograms()[state.id] = state;
        return services_.selection.refitDendrogram(state.id)
            ? "OK\t" + state.id : "ERR no registered dataset/group for quick cluster";
    }
    case CommandAction::ModelSetY:
    case CommandAction::ModelAddTerm:
    case CommandAction::ModelRemoveTerm:
    case CommandAction::ModelClearRole: {
        if (request.args.empty()) return "ERR missing group name";
        if (!services_.queries.groupPlot) return std::nullopt;
        PlotModel seed;
        if (!services_.queries.groupPlot(request.args[0], seed)) return "ERR no active plot/group";
        GroupModelState &state = EnsureGroupModelState(
            applicationState_.groupModels(), request.args[0], &seed);
        const std::string variable = request.args.size() >= 2 ? request.args[1] : "";
        bool changed = false;
        if (request.action == CommandAction::ModelSetY) {
            if (variable.empty() || !FindNumericVariable(seed, variable)) {
                return "ERR dependent variable must be an available numeric variable";
            }
            state.dependent = variable;
            state.terms.erase(std::remove_if(state.terms.begin(), state.terms.end(),
                [&](const std::string &candidate) { return ModelTermShouldBeRemoved(candidate, variable); }),
                state.terms.end());
            changed = true;
        } else if (request.action == CommandAction::ModelAddTerm) {
            const DataFrameModel *dataframe = applicationState_.datasets().find(request.args[0]);
            if (variable.empty() || !ModelTermExistsForVariables(
                AvailableVariableNames(seed, dataframe), variable, state.dependent)) {
                return "ERR predictor must be an available variable";
            }
            if (state.dependent != variable) {
                changed = AddHierarchicalTermsToVector(AvailableVariableNames(seed, dataframe),
                                                       state.dependent, state.terms, variable);
            }
        } else if (request.action == CommandAction::ModelRemoveTerm) {
            if (variable.empty()) return "ERR missing model term";
            const std::size_t before = state.terms.size();
            state.terms.erase(std::remove_if(state.terms.begin(), state.terms.end(),
                [&](const std::string &candidate) { return ModelTermShouldBeRemoved(candidate, variable); }),
                state.terms.end());
            changed = state.terms.size() != before;
        } else {
            if (variable.empty()) return "ERR missing variable";
            if (state.dependent == variable) {
                state.dependent.clear();
                changed = true;
            }
            const std::size_t before = state.terms.size();
            state.terms.erase(std::remove_if(state.terms.begin(), state.terms.end(),
                [&](const std::string &candidate) { return ModelTermShouldBeRemoved(candidate, variable); }),
                state.terms.end());
            changed = changed || state.terms.size() != before;
        }
        if (changed) {
            MarkGroupModelChanged(state);
            if (services_.ui.refreshModelGroup) services_.ui.refreshModelGroup(request.args[0]);
        }
        return "OK";
    }
    case CommandAction::ModelScope: {
        if (request.args.empty()) return "ERR missing group name";
        if (request.args.size() < 2 || (request.args[1] != "all" && request.args[1] != "selected" &&
            request.args[1] != "unselected" && request.args[1] != "compare_selected_all")) {
            return "ERR model scope must be all, selected, unselected, or compare_selected_all";
        }
        if (!services_.queries.groupPlot) return std::nullopt;
        PlotModel seed;
        if (!services_.queries.groupPlot(request.args[0], seed)) return "ERR no active plot/group";
        GroupModelState &state = EnsureGroupModelState(
            applicationState_.groupModels(), request.args[0], &seed);
        state.scope = request.args[1];
        MarkGroupModelChanged(state);
        if (services_.ui.refreshModelGroup) services_.ui.refreshModelGroup(request.args[0]);
        return "OK";
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
    case CommandAction::SetAnalysisScopeAll: {
        if (request.args.empty()) return "ERR missing dataset ID";
        AnalysisScopeChangeEvent event;
        std::string error;
        if (!applicationState_.resetActiveAnalysisScopeToAllObservations(
                request.args[0], &event, &error)) {
            return "ERR " + error;
        }
        return "OK " + AnalysisScopeSummary(event.currentScope,
            event.currentScope.totalDatasetRows);
    }
    case CommandAction::SetAnalysisScopeFromSelection: {
        if (request.args.empty()) return "ERR missing dataset ID";
        const std::string description = request.args.size() > 1 && !request.args[1].empty()
            ? request.args[1] : "Current selection snapshot";
        const std::optional<std::string> sourceView = request.args.size() > 2 && !request.args[2].empty()
            ? std::optional<std::string>(request.args[2]) : std::nullopt;
        AnalysisScopeChangeEvent event;
        std::string error;
        if (!applicationState_.setActiveAnalysisScopeFromSelection(
                request.args[0], AnalysisScopeSourceKind::CurrentSelection,
                description, sourceView, &event, &error)) {
            return "ERR " + error;
        }
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
        return "OK " + AnalysisScopeSummary(event.currentScope,
            event.currentScope.totalDatasetRows);
    }
    case CommandAction::UseSavedAnalysisScope: {
        if (request.args.size() < 2) return "ERR missing saved selection name";
        AnalysisScopeChangeEvent event;
        std::string error;
        if (!applicationState_.activateSavedSelection(
                request.args[0], request.args[1], &event, &error)) {
            return "ERR " + error;
        }
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
        for (long i = 0; i < count; ++i) {
            const int row = std::atoi(request.args[static_cast<std::size_t>(2 + i)].c_str());
            if (row > 0) rows.insert(row);
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
        if (!services_.selection.visibleRows) return std::nullopt;
        std::set<int> rows;
        if (!services_.selection.visibleRows(request.args[0], rows)) {
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
        if (request.name != "INVERT") return std::nullopt;
        if (request.args.empty()) return "ERR missing group name";
        if (!services_.selection.visibleRows) return std::nullopt;
        std::set<int> visible;
        if (!services_.selection.visibleRows(request.args[0], visible)) {
            return "ERR no active plot/group";
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
        return "OK";
    }
    case CommandAction::ClearOverlays: {
        if (request.args.empty()) return "ERR missing plot id";
        if (!services_.selection.clearOverlays) return std::nullopt;
        return services_.selection.clearOverlays(request.args[0])
            ? "OK" : "ERR no active plot";
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
        const int curveCount = std::atoi(request.args[2].c_str());
        if (curveCount < 0) return "ERR invalid curve count";
        std::vector<SmoothCurveData> curves;
        curves.reserve(static_cast<std::size_t>(curveCount));
        std::size_t cursor = 3;
        for (int i = 0; i < curveCount; ++i) {
            if (cursor + 4 > request.args.size()) return "ERR malformed curve data";
            SmoothCurveData curve;
            curve.scope = scope;
            curve.groupId = request.args[cursor++];
            curve.ok = request.args[cursor++] == "1";
            const int pointCount = std::atoi(request.args[cursor++].c_str());
            if (pointCount < 0) return "ERR invalid point count in curve";
            std::istringstream xValues(request.args[cursor++]);
            std::istringstream yValues(request.args[cursor++]);
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
            curves.push_back(std::move(curve));
        }
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
        const int curveCount = std::atoi(request.args[3].c_str());
        if (curveCount < 0) return "ERR invalid curve count";
        std::vector<SmoothCurveData> curves;
        curves.reserve(static_cast<std::size_t>(curveCount));
        std::size_t cursor = 4;
        for (int i = 0; i < curveCount; ++i) {
            if (cursor + 4 > request.args.size()) return "ERR malformed curve data";
            SmoothCurveData curve;
            curve.scope = scope;
            curve.groupId = request.args[cursor++];
            curve.ok = request.args[cursor++] == "1";
            const int pointCount = std::atoi(request.args[cursor++].c_str());
            if (pointCount < 0) return "ERR invalid point count in curve";
            std::istringstream xValues(request.args[cursor++]);
            std::istringstream yValues(request.args[cursor++]);
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
            curves.push_back(std::move(curve));
        }
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
        PlotModel seed;
        if (!services_.queries.groupSeed(request.args[0], seed)) {
            return "ERR no registered dataset/group for linear model";
        }
        GroupModelState &state = EnsureGroupModelState(applicationState_.groupModels(), request.args[0], &seed);
        state.rFitPending = false;
        state.isStale = false;
        GLMFitSummary fit;
        fit.ok = false;
        fit.warning = request.args[1];
        applicationState_.linearModelFits()[request.args[0]] = fit;
        services_.ui.refreshModelGroup(request.args[0]);
        return "OK\tglm:" + request.args[0];
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
        PlotModel seed;
        if (!services_.queries.groupSeed(group, seed)) return "ERR no registered dataset/group for linear model";
        PopulateMissingStandardizedBetas(fit, seed, dependent);
        GroupModelState &state = EnsureGroupModelState(applicationState_.groupModels(), group, &seed);
        const std::string signature = LinearGLMFitSignature(dependent, terms, state.termTypes, scope);
        if (state.rFitPending && !state.lastRFitSignature.empty() && signature != state.lastRFitSignature) {
            return "OK\tglm:" + group;
        }
        state.dependent = dependent;
        state.terms = terms;
        state.scope = scope;
        state.precomputed = false;
        state.multipleImputation = false;
        state.imputationCount = 0;
        state.title.clear();
        state.note.clear();
        state.isStale = false;
        state.rFitPending = false;
        state.lastRFitSignature = signature;
        ++state.fitVersion;
        ++state.diagnosticsVersion;
        state.diagnostics = fit.diagnostics;
        state.rowsUsed = fit.rowsUsed;
        state.rowsExcluded = fit.rowsExcluded;
        applicationState_.linearModelFits()[group] = fit;
        services_.ui.modelUpdated(group, fit.rowsUsed);
        return "OK\tglm:" + group;
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
        auto it = applicationState_.regressionComparisons().find(request.args[0]);
        if (it != applicationState_.regressionComparisons().end()) {
            it->second.rFitPending = false;
            for (RegressionComparisonModel &model : it->second.models) {
                model.isStale = false;
                model.fit.ok = false;
                model.fit.warning = request.args[1];
            }
            if (services_.ui.refreshRegressionComparison) {
                services_.ui.refreshRegressionComparison(it->second.group);
            }
        }
        return "OK\t" + request.args[0];
    }
    case CommandAction::RegressionComparisonUpdate: {
        if (request.args.size() < 8) return "ERR malformed REGCMP_UPDATE command";
        if (!services_.queries.groupSeed || !services_.ui.regressionComparisonUpdated) return std::nullopt;
        std::size_t cursor = 0;
        RegressionComparisonState state;
        state.id = request.args[cursor++];
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
        for (long i = 0; i < typeCount; ++i) state.termTypes[request.args[cursor++]] = request.args[cursor++];
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
            for (long j = 0; j < terms; ++j) model.includedTerms.push_back(request.args[cursor++]);
            if (!ReadLinearFitPayload(request.args, cursor, model.fit)) return "ERR malformed regression comparison fit payload";
            if (!ReadLinearDiagnosticsPayload(request.args, cursor, model.fit.diagnostics)) return "ERR malformed regression comparison diagnostic payload";
            if (!ReadLinearDesignPayload(request.args, cursor, model.fit)) return "ERR malformed regression comparison design payload";
            PopulateMissingStandardizedBetas(model.fit, state.seed, model.response.empty() ? state.response : model.response);
            if (cursor + 6 > request.args.size()) return "ERR malformed regression comparison test payload";
            model.comparisonOk = request.args[cursor++] == "TRUE";
            model.comparisonDf = ParseOptionalDataCellInt(request.args[cursor++]);
            model.comparisonDf2 = ParseOptionalDataCellDouble(request.args[cursor++]);
            model.comparisonDelta = ParseOptionalDataCellDouble(request.args[cursor++]);
            model.comparisonStatistic = ParseOptionalDataCellDouble(request.args[cursor++]);
            model.comparisonP = ParseOptionalDataCellDouble(request.args[cursor++]);
            model.isStale = false;
            model.fitVersion = 1;
            state.models.push_back(std::move(model));
        }
        auto &comparisons = applicationState_.regressionComparisons();
        auto old = comparisons.find(state.id);
        if (old != comparisons.end()) {
            state.activeModel = old->second.activeModel;
            state.title = old->second.title;
            state.note = old->second.note;
            if (state.termTypes.empty()) state.termTypes = old->second.termTypes;
            const std::string signature = RegressionComparisonFitSignature(state);
            if (old->second.rFitPending && !old->second.lastRFitSignature.empty() &&
                signature != old->second.lastRFitSignature) return "OK\t" + state.id;
        }
        InferRegressionComparisonTermTypesFromFitRows(state);
        RefreshRegressionTermRowsFromFits(state);
        PruneRegressionComparisonRowsForCurrentTypes(state);
        state.rFitPending = false;
        state.lastRFitSignature = RegressionComparisonFitSignature(state);
        comparisons[state.id] = state;
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
        state.autoRefit = false; state.precomputed = true; state.multipleImputation = true;
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
            for (long j = 0; j < terms; ++j) model.includedTerms.push_back(request.args[cursor++]);
            if (!ReadLinearFitPayload(request.args, cursor, model.fit)) return "ERR malformed pooled comparison fit payload";
            PopulateMissingStandardizedBetas(model.fit, state.seed, model.response.empty() ? state.response : model.response);
            if (cursor + 6 > request.args.size()) return "ERR malformed pooled comparison test payload";
            model.comparisonOk = request.args[cursor++] == "TRUE"; model.comparisonDf = ParseOptionalDataCellInt(request.args[cursor++]);
            model.comparisonDf2 = ParseOptionalDataCellDouble(request.args[cursor++]); model.comparisonDelta = ParseOptionalDataCellDouble(request.args[cursor++]);
            model.comparisonStatistic = ParseOptionalDataCellDouble(request.args[cursor++]); model.comparisonP = ParseOptionalDataCellDouble(request.args[cursor++]);
            model.isStale = false; model.fitVersion = 1; state.models.push_back(std::move(model));
        }
        if (state.termRows.empty()) RefreshRegressionTermRowsFromFits(state);
        applicationState_.regressionComparisons()[state.id] = state;
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
                    if (!ModelIncludesTerm(model, candidate)) model.includedTerms.push_back(candidate);
                }
            }
            state.models.push_back(std::move(model));
        }
        if (state.models.empty()) {
            RegressionComparisonModel model;
            model.id = state.id + ":model:1";
            model.label = "Untitled 1";
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
                << "|" << model.includedTerms.size();
            for (const std::string &term : model.includedTerms) out << "|" << term;
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
        return "OK\t" + RegressionCoefficientDisplay(model.fit, model.includedTerms, request.args[2]);
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
    case CommandAction::RegressionComparisonSetResponse:
    case CommandAction::RegressionComparisonSetScope: {
        if (request.args.size() < 2) return "ERR malformed comparison update command";
        if (!services_.ui.regressionComparisonUpdated) return std::nullopt;
        auto it = applicationState_.regressionComparisons().find(request.args[0]);
        if (it == applicationState_.regressionComparisons().end()) return "ERR unknown comparison";
        RegressionComparisonState &state = it->second;
        if (request.action == CommandAction::RegressionComparisonSetResponse) {
            if (!FindNumericVariable(state.seed, request.args[1])) {
                return "ERR response variable is not available";
            }
            state.response = request.args[1];
            state.termRows.erase(std::remove(state.termRows.begin(), state.termRows.end(), state.response),
                                 state.termRows.end());
            for (RegressionComparisonModel &model : state.models) {
                model.includedTerms.erase(
                    std::remove(model.includedTerms.begin(), model.includedTerms.end(), state.response),
                    model.includedTerms.end());
            }
        } else {
            if (request.args[1] != "all" && request.args[1] != "selected" &&
                request.args[1] != "unselected") return "ERR invalid comparison scope";
            state.scope = request.args[1];
        }
        for (RegressionComparisonModel &model : state.models) {
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
                model.includedTerms.push_back(candidate);
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
                if (!ModelIncludesTerm(model, candidate)) model.includedTerms.push_back(candidate);
            }
        } else {
            RemoveModelTermCascade(model.includedTerms, term);
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
                if (!ModelIncludesTerm(model, candidate)) model.includedTerms.push_back(candidate);
            }
        }
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
        const std::string term = ModelTermBaseForDisplayRow(request.args[1]);
        if (term.empty() || IsInteractionTerm(term)) return "ERR invalid comparison term type";
        const std::string type = request.args[2] == "factor" ? "factor" : "numeric";
        const std::vector<std::string> available = AvailableVariableNames(state.seed);
        bool exists = false;
        for (const RegressionComparisonModel &model : state.models) {
            const std::string response = model.response.empty() ? state.response : model.response;
            if (ModelTermExistsForVariables(available, term, response)) { exists = true; break; }
        }
        if (!exists) return "ERR invalid comparison term";
        const bool affected = ApplyRegressionComparisonTermType(state, term, type);
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
        PlotModel seed;
        if (!services_.queries.groupSeed(group, seed)) {
            return "ERR no registered dataset/group for pooled linear model";
        }
        PopulateMissingStandardizedBetas(fit, seed, dependent);
        GroupModelState &state = EnsureGroupModelState(applicationState_.groupModels(), group, &seed);
        state.dependent = dependent;
        state.terms = std::move(terms);
        state.scope = scope;
        state.precomputed = true;
        state.multipleImputation = true;
        state.imputationCount = imputationCount;
        state.title = title.empty() ? "General Linear Model - Multiple Imputation" : title;
        state.note = note;
        state.isStale = false;
        state.fitVersion += 1;
        state.rowsUsed = fit.rowsUsed;
        state.rowsExcluded = fit.rowsExcluded;
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
        services_.ui.showLinearModel(plot.group);
        return "OK";
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
                if (legendPosition == "top_left" || legendPosition == "top_right" ||
                    legendPosition == "bottom_left" || legendPosition == "bottom_right") {
                    model->timeSeriesLegendPosition = legendPosition;
                }
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
        if (parsed) applicationState_.registerDataset(dataframe);
        ComputeRanges(*model);
        applicationState_.plots()[id] = model;
        applicationState_.ensureSelectionGroup(group);
        applicationState_.datasets().rememberActiveDatasetGroup(group);
        EnsureGroupModelState(applicationState_.groupModels(), group, model);
        if (services_.ui.addPlot) services_.ui.addPlot(model);
        if (services_.ui.openDataSheet) services_.ui.openDataSheet(group);
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
        if (parsed) applicationState_.registerDataset(dataframe);
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
        services_.ui.showMeanComparison(state);
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
            (state.subtitle.empty() ? "" : "    ") + std::string("Factor: ") + specValue("Factor:");
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
        return id.empty() ? "ERR could not open interaction plot" : "OK\t" + id;
    }
    case CommandAction::ModelInteractionReport: {
        if (request.args.size() < 2) return "ERR malformed MODEL_INTERACTION_REPORT command";
        if (!services_.queries.groupSeed) return std::nullopt;
        const std::string group = request.args[0];
        const std::string term = DecodeCommandField(request.args[1]);
        PlotModel seed;
        if (!services_.queries.groupSeed(group, seed)) return "ERR no registered dataset/group for model";
        GroupModelState &state = EnsureGroupModelState(applicationState_.groupModels(), group, &seed);
        const std::string dependent = state.dependent.empty() ? seed.yLabel : state.dependent;
        const std::string scope = state.scope == "compare_selected_all" ? "all" : state.scope;
        const DataFrameModel *dataframe = applicationState_.datasets().find(group);
        std::set<int> selectedRows;
        applicationState_.selectedRows(group, selectedRows);
        const GLMFitSummary fit = FitMultipleLinearModel(
            seed, dataframe, selectedRows, dependent,
            state.terms, state.termTypes, scope);
        const GLMInteractionReport report = BuildGLMInteractionReport(
            seed, dataframe, term, state.terms, state.termTypes, fit);
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
        if (!services_.queries.groupSeed(state.group, state.seed)) {
            return "ERR no registered dataset/group for generalized GLM";
        }
        state.hasSeed = true;
        if (!ReadGeneralizedStatePayload(request.args, cursor, state)) {
            return "ERR malformed generalized GLM fit payload";
        }
        state.modelVersion = 1;
        state.fitVersion = 1;
        state.diagnosticsVersion = 1;
        state.rFitPending = false;
        state.lastRFitSignature = GeneralizedGLMFitSignature(state);
        auto &models = applicationState_.generalizedGLMs();
        auto old = models.find(state.id);
        if (old != models.end()) {
            const std::string signature = GeneralizedGLMFitSignature(state);
            const bool acceptsInitialBinaryCoding = old->second.binaryRegression &&
                !old->second.responseCodingExplicit && state.binaryRegression;
            if (old->second.rFitPending && !old->second.lastRFitSignature.empty() &&
                signature != old->second.lastRFitSignature && !acceptsInitialBinaryCoding) {
                return "OK\t" + state.id;
            }
            state.autoRefit = old->second.autoRefit;
            state.residualType = old->second.residualType;
            state.modelVersion = std::max(1, old->second.modelVersion);
            state.fitVersion = state.modelVersion;
            state.diagnosticsVersion = state.modelVersion;
            state.lastRFitSignature = signature;
            if (!state.dataScopeCaptured && old->second.dataScopeCaptured) {
                state.dataScope = old->second.dataScope;
                state.dataScopeCaptured = true;
            }
        }
        if (!state.dataScopeCaptured) {
            state.dataScope = applicationState_.activeAnalysisScope(state.group);
            state.dataScopeCaptured = true;
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
        if (!services_.queries.groupSeed(state.group, state.seed)) {
            return "ERR no registered dataset/group for pooled generalized GLM";
        }
        state.hasSeed = true;
        if (!ReadGeneralizedStatePayload(request.args, cursor, state)) {
            return "ERR malformed pooled generalized GLM fit payload";
        }
        state.fitVersion = 1;
        state.diagnosticsVersion = 1;
        applicationState_.generalizedGLMs()[state.id] = state;
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
        if (refreshTitle && services_.ui.refreshPlotTitle) services_.ui.refreshPlotTitle(request.args[0]);
        if (services_.ui.redrawPlot) services_.ui.redrawPlot(request.args[0]);
        return "OK";
    }
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
    case CommandAction::SetActiveDataset: {
        if (request.args.empty()) return "ERR missing dataset name";
        if (!applicationState_.datasets().setActiveDataset(request.args[0])) {
            return "ERR dataset is not registered";
        }
        if (services_.ui.activeDatasetChanged) {
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
        if (request.name != "VARIABLES_WINDOW") return std::nullopt;
        if (request.args.empty()) return "ERR missing group name";
        if (!services_.queries.groupInfo || !services_.ui.showVariablesWindow) return std::nullopt;
        std::size_t selected = 0;
        std::vector<std::string> plotIds;
        if (!services_.queries.groupInfo(request.args[0], selected, plotIds)) {
            return "ERR no active plot/group";
        }
        services_.ui.showVariablesWindow(request.args[0]);
        return "OK";
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
        VariableTypeChangeEffects effects;
        std::string message;
        if (!applicationState_.setVariableType(request.args[0], request.args[1], request.args[2],
                                               effects, &message)) {
            return "ERR " + message;
        }
        if (services_.ui.datasetVariableTypeChanged) {
            services_.ui.datasetVariableTypeChanged(request.args[0], request.args[1],
                                                    NormalizeVariableType(request.args[2]), effects);
        }
        return "OK\t" + message;
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
        if (parsed) applicationState_.registerDataset(dataframe);
        if (services_.ui.datasetRegistered) {
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
        if (services_.ui.refreshPlotTheme) services_.ui.refreshPlotTheme();
        return "OK\t" + plotTheme_;
    }
    default:
        return std::nullopt;
    }
}

} // namespace core
} // namespace rlispstat
