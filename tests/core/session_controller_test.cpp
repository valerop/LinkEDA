#include "../../src/core/command_dispatcher.h"
#include "../../src/core/barplot_model.h"
#include "../../src/core/histogram_model.h"
#include "../../src/core/session_controller.h"

#include <cassert>
#include <fstream>
#include <set>
#include <string>
#include <vector>

namespace {

rlispstat::core::SessionReadLine Reader(const std::vector<std::string> &lines)
{
    std::size_t cursor = 0;
    return [lines, cursor](std::string &line) mutable {
        if (cursor == lines.size()) {
            return false;
        }
        line = lines[cursor++];
        return true;
    };
}

std::vector<std::string> VersionedRegressionComparisonUpdate(
    const std::string &id, int generation, const std::string &modelId)
{
    // Minimal but complete REGCMP_UPDATE for y ~ planet.  R sends the
    // effective (expanded) term types, while the native specification keeps
    // only model-local overrides.  A matching pending generation must be
    // sufficient to associate the result with the current specification.
    return {
        "REGCMP_UPDATE", id, std::to_string(generation), "cars", "distance", "all", "TRUE",
        "2", "(Intercept)", "planet",
        "1", "planet", "factor",
        "1", modelId, "Untitled 1", "distance",
        "1", "planet",
        "1", "planet", "factor",
        "0",
        // GLM fit summary.
        "TRUE", "60", "0", "2", "57",
        "0.25", "0.22", "9.5", "0.003",
        "100", "300", "50", "5.2631579",
        "2.294", "2.294", "NA", "NA", "",
        // Rows used, rows excluded, coefficient rows.
        "0", "0", "0",
        // Diagnostics and design payloads.
        "0", "0", "FACTOR_CODINGS", "0", "PREDICTOR_CENTERS", "0",
        // Sequential comparison with the previous model.
        "FALSE", "NA", "NA", "NA", "NA", "NA"
    };
}

std::vector<std::string> VersionedBinaryMIComparisonUpdate(
    const std::string &fingerprint)
{
    std::vector<std::string> result = {
        "GENERALIZED_COMPARISON_OPEN_STRUCTURED",
        "binary_mi_type_roundtrip", "mi_type_roundtrip", "y_bin", "1", "0",
        "TRUE", "1",
        "binary_mi_type_roundtrip:model:1", "Model 1", "binomial|logit",
        "400", "NA", "NA", "NA", "NA", "x + z + g",
        "FALSE", "0", "NA", "NA", "Baseline model",
        "GENERALIZED_COMPARISON_MODELS_V5", "73", "FALSE", "TRUE", "1",
        "binary_mi_type_roundtrip:model:1", "y_bin", "binomial", "logit", "all",
        "FALSE", "poisson", "", "FALSE", "FALSE", "FALSE", "", "NA",
        "29", fingerprint,
        "3", "x", "z", "g",
        "3", "x", "numeric", "z", "numeric", "g", "factor",
        "0",
        "1", "g", "A",
        // Minimal pooled fit summary.
        "TRUE", "400", "0", "553.8", "451.5", "395", "NA", "NA",
        "1", "NA", "z", "Pooled with mice::pool.",
        "0", "0", "7"
    };
    const auto appendRow = [&](const std::string &term,
                               const std::string &source,
                               const std::string &type,
                               const std::string &rowType,
                               const std::string &label,
                               const std::string &level,
                               const std::string &reference,
                               const std::string &estimate) {
        result.insert(result.end(), {
            term, source, type, rowType, label, level, reference,
            estimate, estimate == "NA" ? "NA" : "0.1", "z",
            estimate == "NA" ? "NA" : "1", estimate == "NA" ? "NA" : ".3"
        });
    };
    appendRow("(Intercept)", "(Intercept)", "intercept", "coefficient",
              "(Intercept)", "", "", ".1");
    appendRow("x", "x", "numeric", "coefficient", "x", "", "", ".2");
    appendRow("z", "z", "numeric", "coefficient", "z", "", "", "-.3");
    appendRow("g", "g", "factor", "factor_parent", "g", "", "A", "NA");
    appendRow("g=A", "g", "factor", "reference", "  A", "A", "A", "NA");
    appendRow("g=B", "g", "factor", "factor_level", "  B", "B", "A", ".4");
    appendRow("g=C", "g", "factor", "factor_level", "  C", "C", "A", "-.5");
    result.insert(result.end(), {
        "GENERALIZED_META_V1", "TRUE", "5", "FALSE", "5", "5", "FALSE", "0"
    });
    return result;
}

} // namespace

int main(int argc, char **argv)
{
    std::vector<std::string> calls;
    rlispstat::core::SessionController controller({
        [&](const rlispstat::core::SessionPlot &plot) {
            calls.push_back("add:" + plot.id);
            assert(plot.group == "cars");
            assert(plot.points.size() == 2);
            return std::string();
        },
        [&](const std::string &group, std::set<int> &rows) {
            calls.push_back("selected:" + group);
            rows = {3, 8};
            return true;
        },
        [&](const std::string &group) {
            calls.push_back("clear:" + group);
            return std::string();
        },
        [&]() { calls.push_back("close"); }
    });

    assert(controller.handle(Reader({"ADD_PLOT", "plot_1", "cars", "speed", "distance", "Distance vs speed", "2",
                                     "1.5 2.5 3", "4.5 5.5 8"})) == "OK");
    assert(controller.hasSession("cars"));
    assert(controller.handle(Reader({"SELECTED", "cars"})) == "OK 3 8");
    assert(controller.handle(Reader({"CLEAR", "cars"})) == "OK");
    assert(controller.handle(Reader({"NOT_A_COMMAND"})) == "ERR unknown command");
    assert(controller.handle(Reader({"ADD_PLOT", "plot_2", "cars", "x", "y", "Title", "-1"})) ==
           "ERR invalid point count");
    assert(controller.handle(Reader({"CLOSE_ALL"})) == "OK");
    assert(controller.isClosed());
    assert((calls == std::vector<std::string>{"add:plot_1", "selected:cars", "clear:cars", "close"}));

    rlispstat::core::SessionController failed({
        [](const rlispstat::core::SessionPlot &) { return std::string("ERR UI could not open plot"); },
        {}, {}, {}
    });
    assert(failed.handle(Reader({"ADD_PLOT", "plot_3", "other", "x", "y", "Title", "0"})) ==
           "ERR UI could not open plot");
    assert(!failed.hasSession("other"));

    std::vector<std::string> dispatchCalls;
    rlispstat::core::CommandDispatcher dispatcher({
        {},
        {},
        {},
        [&](const rlispstat::core::CommandRequest &request, const std::vector<std::string> &lines) {
            dispatchCalls.push_back(request.name);
            assert(request.args == std::vector<std::string>{"plot_1"});
            assert(lines.size() == 2);
            return std::string("OK handled");
        }
    });
    assert(dispatcher.dispatch({"PLOT_INFO", "plot_1"}) == "OK handled");
    assert(dispatcher.dispatch({}) == "ERR empty command");
    assert((dispatchCalls == std::vector<std::string>{"PLOT_INFO"}));

    rlispstat::core::CommandDispatcher unavailable({{}, {}, {}, {}});
    assert(unavailable.dispatch({"UNKNOWN"}) == "ERR unknown command");
    assert(unavailable.dispatch({"EXPORT_PLOT", "plot_5", "/tmp/plot.png", "png"}) ==
           "ERR plot view is not available for export");

    std::vector<std::string> sessionDispatchCalls;
    rlispstat::core::CommandDispatcher sessionDispatcher({
        {
            [&](const rlispstat::core::SessionPlot &plot) {
                sessionDispatchCalls.push_back("add:" + plot.id);
                return std::string();
            },
            {}, {},
            [&]() { sessionDispatchCalls.push_back("close"); }
        },
        {},
        {},
        {}
    });
    assert(sessionDispatcher.dispatchSession(Reader({"PING"})) == "OK");
    assert(sessionDispatcher.dispatchSession(Reader({"ADD_PLOT", "plot_4", "iris", "x", "y", "Title", "0"})) ==
           "OK");
    assert(sessionDispatcher.dispatchSession(Reader({"CLOSE_ALL"})) == "OK");
    assert(sessionDispatcher.sessionClosed());
    assert(sessionDispatcher.dispatch({"COPY_PLOT", "plot_4", "png"}) ==
           "ERR plot view is not available for copying");
    assert((sessionDispatchCalls == std::vector<std::string>{"add:plot_4", "close"}));

    rlispstat::core::PlotModel plot;
    plot.id = "plot_5";
    plot.group = "cars";
    plot.xLabel = "speed";
    plot.yLabel = "distance";
    plot.points.push_back({1.0, 2.0, 1});
    plot.variables.push_back({"speed", {1.0}});
    rlispstat::core::CommandDispatcher queryDispatcher({
        {},
        {
            [=]() { return std::vector<rlispstat::core::PlotModel>{plot}; },
            [=](const std::string &id, rlispstat::core::PlotModel &out, std::size_t &selected) {
                if (id != plot.id) return false;
                out = plot;
                selected = 1;
                return true;
            },
            [=](const std::string &id, rlispstat::core::PlotModel &out) {
                if (id != plot.id) return false;
                out = plot;
                return true;
            },
            []() { return std::vector<rlispstat::core::GroupSelectionSummary>{{"cars", 1, 1}}; },
            [](const std::string &group, std::size_t &selected, std::vector<std::string> &ids) {
                if (group != "cars") return false;
                selected = 1;
                ids = {"plot_5"};
                return true;
            },
            [](const std::string &id, std::vector<std::pair<int, std::string>> &colors) {
                if (id != "plot_5") return false;
                colors = {{1, "blue"}};
                return true;
            }
        },
        {},
        {}
    });
    assert(queryDispatcher.dispatch({"PING"}) == "OK");
    assert(queryDispatcher.dispatch({"PLOT_INFO"}) == "ERR missing plot id");
    assert(queryDispatcher.dispatch({"PLOT_INFO", "missing"}) == "ERR no active plot");
    assert(queryDispatcher.dispatch({"GET_XVAR", "plot_5"}) == "OK speed");
    assert(queryDispatcher.dispatch({"GROUP_INFO", "missing"}) == "ERR no active plot/group");
    assert(queryDispatcher.dispatch({"POINT_COLORS", "plot_5"}) == "OK\t1|blue");
    assert(queryDispatcher.dispatch({"LIST_PLOTS"}).rfind("OK\tplot_5", 0) == 0);

    std::vector<std::string> uiCalls;
    int themeRefreshes = 0;
    rlispstat::core::CommandDispatcher uiDispatcher({
        {}, {},
        {
            [&](const std::string &id, const std::string &path, const std::string &format) {
                uiCalls.push_back("export:" + id + ":" + path + ":" + format);
                return true;
            },
            [&](const std::string &id, const std::string &format) {
                uiCalls.push_back("copy:" + id + ":" + format);
                return false;
            },
            [&]() { ++themeRefreshes; }
        },
        {}
    });
    assert(uiDispatcher.dispatch({"EXPORT_PLOT", "plot_5", "/tmp/plot.png", "png"}) == "OK");
    assert(uiDispatcher.dispatch({"COPY_PLOT", "plot_5", "pdf"}) ==
           "ERR plot view is not available for copying");
    assert(uiDispatcher.dispatch({"EXPORT_PLOT", "plot_5", "/tmp/plot.bmp", "bmp"}) ==
           "ERR unsupported plot export format");
    assert(uiDispatcher.dispatch({"PLOT_THEME"}) == "OK\tpublication");
    uiDispatcher.applicationState().ensureSelectionGroup("cars");
    assert(uiDispatcher.applicationState().setSelectedRows("cars", {2, 4}));
    rlispstat::core::PlotModel retainedPlot;
    retainedPlot.id = "theme_state";
    retainedPlot.xmin = -3.0;
    retainedPlot.xmax = 8.0;
    uiDispatcher.applicationState().plots()[retainedPlot.id] = &retainedPlot;
    assert(uiDispatcher.dispatch({"PLOT_THEME", "manet"}) == "OK\tmanet");
    assert(uiDispatcher.dispatch({"PLOT_THEME", "vista"}) == "OK\tvista");
    assert(uiDispatcher.dispatch({"PLOT_THEME", "beige"}) == "OK\tbeige");
    assert(uiDispatcher.dispatch({"PLOT_THEME", "datadesk"}) == "OK\tdatadesk");
    assert(uiDispatcher.dispatch({"PLOT_THEME", "garish"}) == "OK\tgarish");
    std::set<int> retainedSelection;
    assert(uiDispatcher.applicationState().selectedRows("cars", retainedSelection));
    assert(retainedSelection == std::set<int>({2, 4}));
    assert(retainedPlot.xmin == -3.0 && retainedPlot.xmax == 8.0);
    assert(retainedPlot.rExportTheme == "garish");
    assert(uiDispatcher.dispatch({"PLOT_THEME", "unknown"}) == "ERR unsupported plot theme");
    assert(themeRefreshes == 5);
    assert((uiCalls == std::vector<std::string>{
        "export:plot_5:/tmp/plot.png:PNG", "copy:plot_5:PDF"}));

    rlispstat::core::CommandDispatcher firstState({{}, {}, {}, {}});
    rlispstat::core::CommandDispatcher secondState({{}, {}, {}, {}});
    rlispstat::core::DataFrameModel dataset;
    dataset.group = "cars";
    dataset.rows = 2;
    firstState.applicationState().datasets().registerDataset(dataset);
    assert(firstState.applicationState().datasets().contains("cars"));
    assert(!secondState.applicationState().datasets().contains("cars"));
    assert(firstState.applicationState().datasets().setActiveDataset("cars"));
    assert(firstState.applicationState().datasets().activeDatasetGroup() == "cars");
    assert(firstState.applicationState().eraseDataset("cars"));
    assert(firstState.applicationState().datasets().empty());
    dataset.columns.push_back({"model", "text"});
    firstState.applicationState().datasets().registerDataset(dataset);
    firstState.applicationState().ensureSelectionGroup("cars");
    int selectionVersion = 0;
    assert(firstState.applicationState().setSelectedRows("cars", {1, 3}, &selectionVersion));
    assert(selectionVersion == 1);
    std::set<int> independentSelection;
    assert(firstState.applicationState().selectedRows("cars", independentSelection));
    assert(independentSelection == std::set<int>({1, 3}));
    assert(!secondState.applicationState().hasSelectionGroup("cars"));
    assert(firstState.applicationState().setSelectedColor("cars", "blue"));
    assert(firstState.applicationState().setPointColor("cars", 1, "blue"));
    assert(firstState.applicationState().selectedColor("cars") == "blue");
    const std::vector<std::pair<int, std::string>> expectedPointColors{{1, "blue"}};
    assert(firstState.applicationState().pointColors("cars") == expectedPointColors);
    assert(firstState.applicationState().setLabelColumn("cars", "model"));
    assert(firstState.applicationState().labelColumn("cars") == "model");
    assert(!firstState.applicationState().setLabelColumn("cars", "missing"));
    assert(secondState.applicationState().pointColors("cars").empty());
    assert(secondState.applicationState().labelColumn("cars").empty());
    firstState.applicationState().activePlotId() = "plot_5";
    assert(firstState.applicationState().activePlotId() == "plot_5");
    assert(secondState.applicationState().activePlotId().empty());
    rlispstat::core::PlotModel portablePlot;
    portablePlot.id = "plot_5";
    firstState.applicationState().plots()[portablePlot.id] = &portablePlot;
    firstState.applicationState().groupModels()["cars"] = {};
    assert(firstState.applicationState().plots().at("plot_5") == &portablePlot);
    assert(firstState.applicationState().groupModels().count("cars") == 1);
    assert(secondState.applicationState().plots().empty());
    assert(secondState.applicationState().groupModels().empty());
    firstState.applicationState().correlationMatrices()["corr"] = {"corr", "cars"};
    firstState.applicationState().correlationMatrices()["corr"].variables = {"model"};
    firstState.applicationState().correlationMatrices()["corr"].selectedRow = 2;
    firstState.applicationState().correlationMatrices()["corr"].selectedCol = 3;
    firstState.applicationState().dimensionalityModels()["pca"] = {"pca", "cars"};
    firstState.applicationState().dimensionalityModels()["pca"].variables = {"model"};
    firstState.applicationState().dendrograms()["tree"] = {"tree", "cars"};
    firstState.applicationState().dendrograms()["tree"].variables = {"model"};
    firstState.applicationState().generalizedComparisons()["gglmcmp"] = {"gglmcmp", "cars"};
    rlispstat::core::GeneralizedComparisonModel generalizedComparisonModel;
    generalizedComparisonModel.id = "m1";
    generalizedComparisonModel.label = "Model 1";
    generalizedComparisonModel.response = "model";
    generalizedComparisonModel.terms = {"model"};
    firstState.applicationState().generalizedComparisons()["gglmcmp"].models = {
        generalizedComparisonModel};
    rlispstat::core::GeneralizedGLMState generalizedModel;
    generalizedModel.id = "gglm";
    generalizedModel.group = "cars";
    generalizedModel.response = "model";
    generalizedModel.terms = {"model"};
    firstState.applicationState().generalizedGLMs()["gglm"] = std::move(generalizedModel);
    firstState.applicationState().regressionComparisons()["regcmp"] = {"regcmp", "cars", "model"};
    rlispstat::core::RegressionComparisonModel regressionComparisonModel;
    regressionComparisonModel.id = "m1";
    regressionComparisonModel.label = "Model 1";
    regressionComparisonModel.response = "model";
    regressionComparisonModel.terms = {"model"};
    firstState.applicationState().regressionComparisons()["regcmp"].models = {
        regressionComparisonModel};
    firstState.applicationState().modelTrellises()["trellis"].id = "trellis";
    firstState.applicationState().modelTrellises()["trellis"].specification.baseModel.group = "cars";
    firstState.applicationState().modelTrellises()["trellis"].specification.baseModel.response = "model";
    const rlispstat::core::VariableTypeChangeEffects typeEffects =
        firstState.applicationState().applyVariableTypeChange("cars", "model", "factor");
    assert(typeEffects.correlationIds == std::vector<std::string>({"corr"}));
    assert(typeEffects.dendrogramIds == std::vector<std::string>({"tree"}));
    assert(typeEffects.dimensionalityIds.empty());
    assert(typeEffects.generalizedGlmIdsToRefit == std::vector<std::string>({"gglm"}));
    assert(typeEffects.regressionComparisonIds == std::vector<std::string>({"regcmp"}));
    assert(typeEffects.generalizedComparisonIds == std::vector<std::string>({"gglmcmp"}));
    assert(typeEffects.modelTrellisIds == std::vector<std::string>({"trellis"}));
    assert(firstState.applicationState().correlationMatrices().at("corr").variables.empty());
    assert(firstState.applicationState().correlationMatrices().at("corr").selectedRow == -1);
    assert(firstState.applicationState().dendrograms().at("tree").variables.empty());
    assert(firstState.applicationState().dimensionalityModels().at("pca").status ==
           "At least two numeric, ordinal, or binary variables are required.");
    // Dataset metadata must not manufacture a model term merely because the
    // variable's storage type changed.
    assert(!firstState.applicationState().groupModels().at("cars").termTypes.count("model"));
    assert(firstState.applicationState().generalizedComparisons().at("gglmcmp").models.front().isStale);
    assert(firstState.applicationState().regressionComparisons().at("regcmp").termTypes.at("model") == "factor");
    assert(firstState.applicationState().generalizedGLMs().at("gglm").termTypes.at("model") == "factor");
    assert(firstState.applicationState().generalizedComparisons().at("gglmcmp").termTypes.at("model") == "factor");
    assert(firstState.applicationState().modelTrellises().at("trellis").specification.baseModel.termTypes.at("model") == "factor");
    std::string renameError;
    assert(rlispstat::core::RenameDataFrameColumn(
        *firstState.applicationState().datasets().find("cars"), "model", "renamed", &renameError));
    firstState.applicationState().applyVariableRename("cars", "model", "renamed");
    assert(firstState.applicationState().labelColumn("cars") == "renamed");
    assert(!firstState.applicationState().groupModels().at("cars").termTypes.count("renamed"));
    assert(firstState.applicationState().generalizedGLMs().at("gglm").response == "renamed");
    assert(firstState.applicationState().regressionComparisons().at("regcmp").response == "renamed");
    const auto &renamedRows =
        firstState.applicationState().generalizedComparisons().at("gglmcmp").termRows;
    assert(std::find(renamedRows.begin(), renamedRows.end(), "renamed") != renamedRows.end());
    assert(firstState.applicationState().correlationMatrices().count("corr") == 1);
    assert(firstState.applicationState().dimensionalityModels().count("pca") == 1);
    assert(firstState.applicationState().dendrograms().count("tree") == 1);
    assert(secondState.applicationState().correlationMatrices().empty());
    assert(secondState.applicationState().dimensionalityModels().empty());
    assert(secondState.applicationState().dendrograms().empty());
    assert(secondState.applicationState().generalizedComparisons().empty());
    const rlispstat::core::RemovedGroupDomainState removedState =
        firstState.applicationState().removeGroupDomainState("cars");
    assert(removedState.correlationIds == std::vector<std::string>({"corr"}));
    assert(removedState.dimensionalityIds == std::vector<std::string>({"pca"}));
    assert(removedState.dendrogramIds == std::vector<std::string>({"tree"}));
    assert(removedState.generalizedComparisonIds == std::vector<std::string>({"gglmcmp"}));
    assert(removedState.modelTrellisIds == std::vector<std::string>({"trellis"}));
    assert(!firstState.applicationState().hasSelectionGroup("cars"));
    assert(firstState.applicationState().pointColors("cars").empty());
    assert(firstState.applicationState().groupModels().empty());
    assert(firstState.applicationState().correlationMatrices().empty());
    assert(firstState.applicationState().generalizedComparisons().empty());
    firstState.applicationState().plots().clear();

    std::vector<std::string> selectionEvents;
    rlispstat::core::CommandDispatcher selectionDispatcher({
        {}, {}, {},
        [](const rlispstat::core::CommandRequest &, const std::vector<std::string> &) {
            return std::string("fallback");
        },
        {
            [](const std::string &group, std::set<int> &rows) {
                if (group != "cars") return false;
                rows = {1, 2, 3};
                return true;
            },
            [&](const std::string &group, const std::set<int> &rows,
                int version, bool redraw) {
                selectionEvents.push_back(group + ":" +
                    rlispstat::core::CaseSetText(rows) + ":" +
                    std::to_string(version) + ":" + (redraw ? "1" : "0"));
            }
        }
    });
    selectionDispatcher.applicationState().groupSelections()["cars"] = {};
    assert(selectionDispatcher.dispatch({"SELECTED", "cars"}) == "OK");
    assert(selectionDispatcher.dispatch({"SET_SELECTED", "cars", "2", "1", "3"}) == "OK");
    assert(selectionDispatcher.dispatch({"SELECTED", "cars"}) == "OK 1 3");
    assert(selectionDispatcher.dispatch({"INVERT", "cars"}) == "OK");
    assert(selectionDispatcher.dispatch({"SELECTED", "cars"}) == "OK 2");
    assert(selectionDispatcher.dispatch({"CLEAR", "cars"}) == "OK");
    assert(selectionDispatcher.dispatch({"SELECT_ALL", "cars"}) == "OK");
    assert(selectionDispatcher.dispatch({"SET_SELECTED", "missing", "0"}) ==
           "ERR no active plot/group");
    assert(selectionDispatcher.dispatch({"CLEAR_SELECTION", "cars"}) == "fallback");
    assert((selectionEvents == std::vector<std::string>{
        "cars:1 3:1:0", "cars:2:2:0", "cars::3:1", "cars:1 2 3:4:0"}));

    std::vector<std::string> colorEvents;
    rlispstat::core::CommandDispatcher colorDispatcher({
        {}, {}, {}, {},
        {
            {}, {},
            [&](const std::string &group, const std::vector<int> &rows,
                bool updatePanel, bool recompute) {
                std::set<int> cases(rows.begin(), rows.end());
                colorEvents.push_back(group + ":" + rlispstat::core::CaseSetText(cases) +
                    ":" + (updatePanel ? "1" : "0") + ":" + (recompute ? "1" : "0"));
            }
        }
    });
    colorDispatcher.applicationState().groupSelections()["cars"] = {1, 3};
    assert(colorDispatcher.dispatch({"SET_SELECTED_COLOR", "cars", "blue"}) == "OK");
    assert(colorDispatcher.dispatch({"GET_SELECTED_COLOR", "cars"}) == "OK blue");
    assert(colorDispatcher.dispatch({"RESET_SELECTED_COLOR", "cars"}) == "OK");
    assert(colorDispatcher.dispatch({"GET_SELECTED_COLOR", "cars"}) == "OK black");
    assert(colorDispatcher.dispatch({"RESET_SELECTED_COLOR", "missing"}) ==
           "ERR no active plot/group");
    assert(colorDispatcher.dispatch({"SET_POINT_COLOR", "cars", "green", "2", "2", "3"}) == "OK");
    assert(colorDispatcher.dispatch({"CLEAR_ROW_COLORS", "cars", "1", "1"}) == "OK");
    assert(colorDispatcher.dispatch({"SET_SELECTED_COLOR", "missing", "blue"}) ==
           "ERR no active plot/group");
    assert(colorDispatcher.dispatch({"SET_POINT_COLOR", "cars", "unknown", "1", "1"}) ==
           "ERR unknown color");
    assert((colorEvents == std::vector<std::string>{
        "cars:1 3:1:1", "cars:1 3:1:1", "cars:2 3:0:1", "cars::0:1"}));

    std::string interactionMode = "none";
    std::string selectionMode = "replace";
    rlispstat::core::CommandDispatcher modeDispatcher({
        {}, {}, {}, {},
        {
            {}, {}, {},
            [&](const std::string &plotId, std::string &interaction, std::string &selection) {
                if (plotId != "plot_5") return false;
                interaction = interactionMode;
                selection = selectionMode;
                return true;
            },
            [&](const std::string &plotId, const std::string &mode) {
                if (plotId != "plot_5") return false;
                interactionMode = mode;
                return true;
            },
            [&](const std::string &plotId, const std::string &mode) {
                if (plotId != "plot_5") return false;
                selectionMode = mode;
                return true;
            }
        }
    });
    assert(modeDispatcher.dispatch({"MODE", "plot_5"}) == "OK none");
    assert(modeDispatcher.dispatch({"MODE", "plot_5", "brush"}) == "OK");
    assert(modeDispatcher.dispatch({"MODE", "plot_5"}) == "OK brush");
    assert(modeDispatcher.dispatch({"MODE", "plot_5", "invalid"}) == "ERR invalid mode");
    assert(modeDispatcher.dispatch({"SELECTION_MODE", "plot_5", "add"}) == "OK");
    assert(modeDispatcher.dispatch({"SELECTION_OPERATION", "plot_5"}) == "OK add");
    assert(modeDispatcher.dispatch({"SELECTION_OPERATION", "plot_5", "toggle"}) == "OK");
    assert(modeDispatcher.dispatch({"SELECTION_MODE", "plot_5", "invalid"}) ==
           "ERR invalid selection mode");
    assert(modeDispatcher.dispatch({"MODE", "missing"}) == "ERR no active plot");

    std::vector<std::string> overlays;
    rlispstat::core::CommandDispatcher overlayDispatcher({
        {}, {}, {}, {},
        {
            {}, {}, {}, {}, {}, {},
            [&](const std::string &plotId, const std::string &source) {
                if (plotId != "plot_5") return false;
                overlays.push_back(source);
                return true;
            },
            [&](const std::string &plotId) {
                if (plotId != "plot_5") return false;
                overlays.clear();
                return true;
            }
        }
    });
    assert(overlayDispatcher.dispatch({"ADD_LM", "plot_5", "both"}) == "OK");
    assert((overlays == std::vector<std::string>{"both"}));
    assert(overlayDispatcher.dispatch({"CLEAR_OVERLAYS", "plot_5"}) == "OK");
    assert(overlays.empty());
    assert(overlayDispatcher.dispatch({"ADD_LM", "plot_5", "invalid"}) ==
           "ERR invalid overlay data source");
    assert(overlayDispatcher.dispatch({"CLEAR_OVERLAYS", "missing"}) == "ERR no active plot");

    rlispstat::core::PlotModel smoothPlot;
    smoothPlot.id = "plot_5";
    std::vector<rlispstat::core::SmoothCurveData> replacementCurves;
    int redraws = 0;
    rlispstat::core::CommandDispatcher smoothDispatcher({
        {},
        {
            {}, {},
            [&](const std::string &plotId, rlispstat::core::PlotModel &plot) {
                if (plotId != "plot_5") return false;
                plot = smoothPlot;
                return true;
            }
        },
        {
            {}, {}, {},
            [&](const std::string &plotId) {
                assert(plotId == "plot_5");
                ++redraws;
            }
        },
        {},
        {
            {}, {}, {}, {}, {}, {}, {}, {},
            [&](const std::string &plotId, rlispstat::core::SmoothCurveScope scope,
                const std::vector<rlispstat::core::SmoothCurveData> &curves) {
                if (plotId != "plot_5" || scope != rlispstat::core::SmoothCurveScope::Selection) {
                    return false;
                }
                replacementCurves = curves;
                return true;
            }
        }
    });
    assert(smoothDispatcher.dispatch(
        {"ADD_SMOOTH", "plot_5", "selected", "1", "subset", "1", "2", "1 2", "3 4"}) ==
        "OK");
    assert(replacementCurves.size() == 1);
    assert(replacementCurves[0].x == std::vector<double>({1.0, 2.0}));
    assert(replacementCurves[0].y == std::vector<double>({3.0, 4.0}));
    assert(redraws == 1);
    assert(smoothDispatcher.dispatch(
        {"ADD_SMOOTH", "plot_5", "selected", "FIT_CURVE_V2", "lm", "1",
         "subset", "1", "2", "1 2", "3 4", "2 3", "4 5"}) == "OK");
    assert(replacementCurves.size() == 1);
    assert(replacementCurves[0].fitMethod == "lm");
    assert(replacementCurves[0].confidenceLower ==
           std::vector<double>({2.0, 3.0}));
    assert(replacementCurves[0].confidenceUpper ==
           std::vector<double>({4.0, 5.0}));
    assert(smoothDispatcher.dispatch({"ADD_SMOOTH", "plot_5", "selected", "0"}) == "OK");
    assert(replacementCurves.size() == 1);
    assert(replacementCurves[0].scope == rlispstat::core::SmoothCurveScope::Selection);
    assert(!replacementCurves[0].ok);
    assert(replacementCurves[0].message.empty());
    assert(smoothDispatcher.dispatch({"ADD_SMOOTH", "plot_5", "overall", "-1", "unused"}) ==
           "ERR invalid curve count");
    assert(smoothDispatcher.dispatch(
        {"ADD_SMOOTH", "plot_5", "overall", "1", "all", "1", "2", "1", "3 4"}) ==
        "ERR curve point count mismatch");

    rlispstat::core::PlotModel axisPlot;
    axisPlot.id = "plot_5";
    axisPlot.variables = {{"x", {1.0}}, {"y", {2.0}}};
    std::vector<std::string> axisChanges;
    rlispstat::core::CommandDispatcher axisDispatcher({
        {},
        {
            {}, {},
            [&](const std::string &plotId, rlispstat::core::PlotModel &plot) {
                if (plotId != "plot_5") return false;
                plot = axisPlot;
                return true;
            }
        },
        {}, {},
        {
            {}, {}, {}, {}, {}, {}, {}, {}, {},
            [&](const std::string &plotId, bool changeX, const std::string &variable) {
                if (plotId != "plot_5") return false;
                axisChanges.push_back(std::string(changeX ? "x:" : "y:") + variable);
                return true;
            }
        }
    });
    assert(axisDispatcher.dispatch({"SET_XVAR", "plot_5", "x"}) == "OK");
    assert(axisDispatcher.dispatch({"SET_YVAR", "plot_5", "y"}) == "OK");
    assert(axisDispatcher.dispatch({"SET_XVAR", "plot_5", "missing"}) ==
           "ERR variable is not available for this plot");
    assert((axisChanges == std::vector<std::string>{"x:x", "y:y"}));

    std::vector<std::string> closedPlots;
    rlispstat::core::CommandDispatcher closeDispatcher({
        {}, {},
        {
            {}, {}, {}, {},
            [&](const std::string &plotId) {
                if (plotId != "plot_5") return false;
                closedPlots.push_back(plotId);
                return true;
            }
        },
        {}, {}
    });
    assert(closeDispatcher.dispatch({"CLOSE_PLOT"}) == "ERR missing plot id");
    assert(closeDispatcher.dispatch({"CLOSE_PLOT", "missing"}) == "ERR no active plot");
    assert(closeDispatcher.dispatch({"CLOSE_PLOT", "plot_5"}) == "OK");
    assert((closedPlots == std::vector<std::string>{"plot_5"}));

    int redrawCount = 0;
    rlispstat::core::CommandDispatcher redrawDispatcher({
        {},
        {
            {}, {},
            [](const std::string &plotId, rlispstat::core::PlotModel &plot) {
                if (plotId != "plot_5") return false;
                plot.id = plotId;
                return true;
            }
        },
        {
            {}, {}, {},
            [&](const std::string &plotId) {
                assert(plotId == "plot_5");
                ++redrawCount;
            }
        },
        {}, {}
    });
    assert(redrawDispatcher.dispatch({"REDRAW"}) == "ERR missing plot id");
    assert(redrawDispatcher.dispatch({"REDRAW", "missing"}) == "ERR no active plot");
    assert(redrawDispatcher.dispatch({"REDRAW", "plot_5"}) == "OK");
    assert(redrawCount == 1);

    std::vector<std::string> visibilityChanges;
    rlispstat::core::CommandDispatcher visibilityDispatcher({
        {}, {},
        {
            {}, {}, {}, {}, {},
            [&](bool show) { visibilityChanges.push_back(show ? "panel:show" : "panel:hide"); },
            [&](bool show) { visibilityChanges.push_back(show ? "palette:show" : "palette:hide"); }
        },
        {}, {}
    });
    assert(visibilityDispatcher.dispatch({"PANEL"}) == "ERR missing panel state");
    assert(visibilityDispatcher.dispatch({"PANEL", "invalid"}) == "ERR invalid panel state");
    assert(visibilityDispatcher.dispatch({"PANEL", "show"}) == "OK");
    assert(visibilityDispatcher.dispatch({"PANEL_HIDE"}) == "OK");
    assert(visibilityDispatcher.dispatch({"PANEL_SHOW"}) == "OK");
    assert(visibilityDispatcher.dispatch({"PALETTE", "hide"}) == "OK");
    assert((visibilityChanges == std::vector<std::string>{
        "panel:show", "panel:hide", "panel:show", "palette:hide"}));

    std::vector<std::string> activeDatasetChanges;
    rlispstat::core::CommandDispatcher datasetDispatcher({
        {}, {},
        {
            {}, {}, {}, {}, {}, {}, {},
            [&](const std::string &group) { activeDatasetChanges.push_back(group); }
        },
        {}, {}
    });
    rlispstat::core::DataFrameModel activeDataset;
    activeDataset.group = "cars";
    datasetDispatcher.applicationState().datasets().registerDataset(activeDataset);
    assert(datasetDispatcher.dispatch({"SET_ACTIVE_DATASET"}) == "ERR missing dataset name");
    assert(datasetDispatcher.dispatch({"SET_ACTIVE_DATASET", "missing"}) ==
           "ERR dataset is not registered");
    assert(datasetDispatcher.dispatch({"SET_ACTIVE_DATASET", "cars"}) == "OK");
    assert(datasetDispatcher.dispatch({"DATA_SET_ACTIVE_DATASET", "cars"}) == "OK");
    assert(datasetDispatcher.applicationState().datasets().activeDatasetGroup() == "cars");
    assert(activeDatasetChanges.empty());
    rlispstat::core::DataFrameModel secondActiveDataset;
    secondActiveDataset.group = "trucks";
    datasetDispatcher.applicationState().datasets().registerDataset(secondActiveDataset);
    assert(datasetDispatcher.dispatch({"SET_ACTIVE_DATASET", "trucks"}) == "OK");
    assert((activeDatasetChanges == std::vector<std::string>{"trucks"}));

    rlispstat::core::CommandDispatcher savedScopeDispatcher({});
    rlispstat::core::DataFrameModel savedScopeData;
    savedScopeData.group = "scope cars";
    savedScopeData.rows = 5;
    assert(savedScopeDispatcher.applicationState().registerDataset(savedScopeData));
    assert(savedScopeDispatcher.applicationState().setSelectedRows(
        "scope cars", std::set<int>{1, 3}));
    assert(savedScopeDispatcher.dispatch({
        "SAVE_ANALYSIS_SCOPE_FROM_SELECTION", "scope cars",
        "High mileage cars", "R"}).rfind("OK saved and applied ", 0) == 0);
    assert(savedScopeDispatcher.applicationState().activeAnalysisScope("scope cars").kind ==
           rlispstat::core::AnalysisScopeKind::ExplicitRowIds);
    assert(rlispstat::core::AnalysisScopeSelectionName(
        savedScopeDispatcher.applicationState().activeAnalysisScope("scope cars")) ==
        std::optional<std::string>("High mileage cars"));
    assert(savedScopeDispatcher.dispatch({
        "SET_ANALYSIS_SCOPE_FROM_SELECTION", "scope cars",
        "Current selection", "R"}).rfind("OK ", 0) == 0);
    assert(savedScopeDispatcher.applicationState().savedSelections("scope cars").size() == 1);
    // Legacy named activation remains accepted, but the normal UI/API now uses
    // the two explicit commands above.
    assert(savedScopeDispatcher.dispatch({
        "SET_ANALYSIS_SCOPE_FROM_SELECTION", "scope cars",
        "Selection: High mileage cars", "R"}).rfind("OK ", 0) == 0);
    const std::string savedReply = savedScopeDispatcher.dispatch({
        "GET_SAVED_ANALYSIS_SCOPES", "scope cars"});
    assert(savedReply == "OK\t1|High mileage cars|2|1|3");
    assert(savedScopeDispatcher.applicationState().setSelectedRows(
        "scope cars", std::set<int>{2, 4}));
    assert(savedScopeDispatcher.dispatch({
        "SET_ANALYSIS_SCOPE_FROM_SELECTION", "scope cars",
        "Selection: Even cars", "R"}).rfind("OK ", 0) == 0);
    assert(savedScopeDispatcher.dispatch({
        "USE_SAVED_ANALYSIS_SCOPE", "scope cars", "High mileage cars"}).rfind("OK ", 0) == 0);
    assert(savedScopeDispatcher.dispatch({
        "ADD_SAVED_ANALYSIS_SCOPE", "scope cars", "Even cars", "High or even"}).rfind("OK ", 0) == 0);
    assert((savedScopeDispatcher.applicationState().resolveActiveAnalysisRowIds("scope cars") ==
            std::vector<int>{1, 3, 2, 4}));

    std::vector<std::string> openedSheets;
    rlispstat::core::CommandDispatcher dataSheetDispatcher({
        {}, {},
        {
            {}, {}, {}, {}, {}, {}, {}, {},
            [&](const std::string &group) { openedSheets.push_back(group); }
        },
        {}, {}
    });
    rlispstat::core::DataFrameModel sheetDataset;
    sheetDataset.group = "cars";
    dataSheetDispatcher.applicationState().datasets().registerDataset(sheetDataset);
    assert(dataSheetDispatcher.dispatch({"DATA_OPEN_DATA_SHEET"}) == "OK");
    assert(dataSheetDispatcher.dispatch({"DATA_OPEN_DATA_SHEET", "other"}) == "OK");
    assert((openedSheets == std::vector<std::string>{"cars", "other"}));

    std::vector<std::string> variableWindows;
    rlispstat::core::CommandDispatcher variablesDispatcher({
        {},
        {},
        {
            {}, {}, {}, {}, {}, {}, {}, {}, {},
            [&](const std::string &group) { variableWindows.push_back(group); }
        },
        {}, {}
    });
    assert(variablesDispatcher.dispatch({"VARIABLES_WINDOW"}) == "ERR missing group name");
    rlispstat::core::DataFrameModel variableViewDataset;
    variableViewDataset.group = "cars";
    variableViewDataset.rows = 1;
    variableViewDataset.columns.push_back({"speed", "numeric", {}, {}, -1, {"42"}});
    assert(variablesDispatcher.applicationState().registerDataset(variableViewDataset));
    assert(variablesDispatcher.dispatch({"VARIABLES_WINDOW", "missing"}) ==
           "ERR dataset is not registered");
    assert(variablesDispatcher.dispatch({"VARIABLES_WINDOW", "cars"}) == "OK");
    assert(variablesDispatcher.dispatch({"DATA_VARIABLE_VIEW"}) == "OK");
    assert((variableWindows == std::vector<std::string>{"cars", "cars"}));

    rlispstat::core::CommandDispatcher variableInfoDispatcher({
        {},
        {
            {}, {}, {}, {}, {}, {},
            [](const std::string &group,
               std::vector<rlispstat::core::CommandVariableInfo> &variables) {
                if (group != "cars") return false;
                variables = {{"speed", "numeric", "predictor"},
                             {"distance", "numeric", "response"}};
                return true;
            }
        },
        {}, {}, {}
    });
    assert(variableInfoDispatcher.dispatch({"VARIABLE_INFO"}) == "ERR missing group name");
    assert(variableInfoDispatcher.dispatch({"VARIABLE_INFO", "missing"}) ==
           "ERR no active plot/group");
    assert(variableInfoDispatcher.dispatch({"VARIABLE_INFO", "cars"}) ==
           "OK\tspeed|numeric|predictor\tdistance|numeric|response");

    int layoutResets = 0;
    rlispstat::core::CommandDispatcher layoutDispatcher({
        {}, {},
        {
            {}, {}, {}, {}, {}, {}, {}, {}, {}, {},
            [&]() { ++layoutResets; }
        },
        {}, {}
    });
    assert(layoutDispatcher.dispatch({"RESET_WINDOW_LAYOUT"}) == "OK");
    assert(layoutResets == 1);

    int activeDatasetViews = 0;
    rlispstat::core::CommandDispatcher activeDatasetViewDispatcher({
        {}, {},
        {
            {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {},
            [&]() { ++activeDatasetViews; }
        },
        {}, {}
    });
    assert(activeDatasetViewDispatcher.dispatch({"DATA_SHOW_ACTIVE_DATASET"}) == "OK");
    assert(activeDatasetViews == 1);

    rlispstat::core::CommandDispatcher importDispatcher({
        {}, {},
        {
            {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {},
            [](const std::string &path, std::string &message) {
                if (path == "/tmp/ok.csv") {
                    message = "cars";
                    return true;
                }
                message = "unsupported file";
                return false;
            }
        },
        {}, {}
    });
    assert(importDispatcher.dispatch({"NATIVE_IMPORT_FILE"}) == "ERR missing import path");
    assert(importDispatcher.dispatch({"NATIVE_IMPORT_FILE", "/tmp/ok.csv"}) == "OK\tcars");
    assert(importDispatcher.dispatch({"NATIVE_IMPORT_FILE", "/tmp/bad.txt"}) ==
           "ERR unsupported file");

    std::vector<std::string> variableTypeNotifications;
    std::vector<rlispstat::core::DatasetMutationEvent> datasetMutations;
    rlispstat::core::CommandDispatcherServices variableTypeServices;
    variableTypeServices.ui.datasetVariableTypeChanged =
        [&](const std::string &group, const std::string &variable, const std::string &type,
            const rlispstat::core::VariableTypeChangeEffects &) {
            variableTypeNotifications.push_back(group + ":" + variable + ":" + type);
        };
    variableTypeServices.ui.datasetMutated =
        [&](const rlispstat::core::DatasetMutationEvent &event) {
            datasetMutations.push_back(event);
        };
    rlispstat::core::CommandDispatcher variableTypeDispatcher(variableTypeServices);
    rlispstat::core::DataFrameModel variableTypeDataset;
    variableTypeDataset.group = "cars";
    variableTypeDataset.rows = 2;
    rlispstat::core::DataColumn speedColumn;
    speedColumn.name = "speed";
    speedColumn.type = "numeric";
    speedColumn.values = {"1", "2"};
    variableTypeDataset.columns.push_back(speedColumn);
    rlispstat::core::DataColumn colorColumn;
    colorColumn.name = "color";
    colorColumn.type = "factor";
    colorColumn.values = {"Red", "Blue"};
    colorColumn.definedLevels = {"Red", "Green", "Blue"};
    variableTypeDataset.columns.push_back(colorColumn);
    assert(variableTypeDispatcher.applicationState().registerDataset(variableTypeDataset));
    rlispstat::core::PlotModel variableTypePlot;
    variableTypePlot.id = "type_plot";
    variableTypePlot.group = "cars";
    variableTypePlot.xLabel = "speed";
    variableTypePlot.variables = {{"speed", {1.0, 2.0}}};
    variableTypePlot.variableMeta = {{"speed", "numeric"}};
    variableTypeDispatcher.applicationState().plots()[variableTypePlot.id] = &variableTypePlot;
    assert(variableTypeDispatcher.dispatch({"SET_VARIABLE_TYPE", "cars", "speed"}) ==
           "ERR malformed SET_VARIABLE_TYPE command");
    assert(variableTypeDispatcher.dispatch({"SET_VARIABLE_TYPE", "cars", "speed", "factor"}) ==
           "OK\tspeed is now treated as Categorical.");
    assert(variableTypeDispatcher.applicationState().groupModels().empty());
    assert(variableTypeDispatcher.dispatch(
        {"SET_DEFAULT_VARIABLE_ROLE", "cars", "speed", "dependent"}) ==
        "OK\tSet the default role for `speed` to Dependent.");
    assert(variableTypeDispatcher.applicationState().variableRoles("cars").at("speed") ==
           "dependent");
    assert(variableTypePlot.variableMeta.front().type == "factor");
    assert(variableTypePlot.variables.empty());
    assert(variableTypeDispatcher.dispatch({"SET_VARIABLE_TYPE", "cars", "missing", "factor"}) ==
           "ERR Variable `missing` was not found in dataset `cars`.");
    assert(variableTypeDispatcher.dispatch({"SET_VARIABLE_TYPE", "cars", "speed", "unsupported"}) ==
           "ERR Variable type must be Numeric, Categorical, Ordinal, or Text.");
    assert(variableTypeDispatcher.dispatch({
        "SET_VARIABLE_TYPE", "cars", "color", "numeric",
        "category_order", "3", "Red", "Green", "Blue",
        "mapping", "3", "Red", "10", "Green", "20", "Blue", "30"
    }) == "OK\tcolor is now treated as Numeric.");
    const auto *mappedDataset = variableTypeDispatcher.applicationState().datasets().find("cars");
    assert(mappedDataset != nullptr);
    assert(mappedDataset->columns[1].values == std::vector<std::string>({"10", "30"}));
    assert(mappedDataset->columns[1].numericMapping.at("Green") == "20");
    assert(variableTypeDispatcher.dispatch(
        {"SET_VARIABLE_DESCRIPTION", "cars", "speed", "Vehicle speed"}) ==
        "OK\tUpdated description for `speed`.");
    assert(variableTypeDispatcher.dispatch(
        {"RENAME_VARIABLE", "cars", "speed", "pace"}) ==
        "OK\tRenamed `speed` to `pace`.");
    assert(variableTypeDispatcher.applicationState().variableRoles("cars").at("pace") ==
           "dependent");
    assert(variableTypeDispatcher.dispatch(
        {"SET_VARIABLE_TYPE", "cars", "pace", "numeric"}) ==
        "OK\tpace is now treated as Numeric.");
    assert(variableTypePlot.variableMeta.front().name == "pace");
    assert(variableTypePlot.variableMeta.front().type == "numeric");
    assert(variableTypePlot.variables.size() == 2);
    assert(rlispstat::core::FindNumericVariable(variableTypePlot, "pace") != nullptr);
    assert(rlispstat::core::FindNumericVariable(variableTypePlot, "color") != nullptr);
    assert((variableTypeNotifications == std::vector<std::string>{
        "cars:speed:factor", "cars:color:numeric", "cars:pace:numeric"}));
    assert(variableTypeDispatcher.dispatch(
        {"SET_VARIABLE_DECIMALS", "cars", "pace", "2"}) ==
        "OK\tDecimals for `pace` set to 2.");
    assert(variableTypeDispatcher.dispatch(
        {"SET_DATA_CELL", "cars", "pace", "2", "3.5"}) ==
        "OK\tUpdated row 2, variable `pace`.");
    assert(variableTypeDispatcher.dispatch(
        {"SET_LABEL_COLUMN", "cars", "pace"}) == "OK");
    const auto *mutatedDataset =
        variableTypeDispatcher.applicationState().datasets().find("cars");
    assert(mutatedDataset != nullptr);
    assert(mutatedDataset->columns[0].name == "pace");
    assert(mutatedDataset->columns[0].description == "Vehicle speed");
    assert(mutatedDataset->columns[0].decimals == 2);
    assert(mutatedDataset->columns[0].values[1] == "3.5");
    assert(variableTypeDispatcher.applicationState().labelColumn("cars") == "pace");
    assert(datasetMutations.size() == 8);
    assert(datasetMutations[0].kind ==
           rlispstat::core::DatasetMutationKind::VariableMetadata);
    assert(datasetMutations[2].kind ==
           rlispstat::core::DatasetMutationKind::VariableMetadata);
    assert(datasetMutations[3].kind ==
           rlispstat::core::DatasetMutationKind::VariableRename);
    assert(datasetMutations[6].kind ==
           rlispstat::core::DatasetMutationKind::CellValue);
    assert(datasetMutations[7].kind ==
           rlispstat::core::DatasetMutationKind::LabelColumn);

    // A cell mutation invalidates only analysis states whose specification
    // actually uses the edited variable.
    auto &editedModel = variableTypeDispatcher.applicationState().groupModels()["cars"];
    editedModel.group = "cars";
    editedModel.response = "pace";
    editedModel.terms = {"other"};
    editedModel.isStale = false;
    assert(variableTypeDispatcher.dispatch(
        {"SET_DATA_CELL", "cars", "pace", "1", "9"}) ==
        "OK\tUpdated row 1, variable `pace`.");
    assert(variableTypeDispatcher.applicationState().groupModels()["cars"].isStale);
    assert(datasetMutations.back().valueChangeEffects.groupModel);

    std::vector<std::string> registeredDatasets;
    rlispstat::core::CommandDispatcher registerDatasetDispatcher({
        {}, {},
        {
            {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {},
            [&](const std::string &group, bool showDataSheet) {
                registeredDatasets.push_back(group + (showDataSheet ? ":show" : ":silent"));
            }
        },
        {}, {}
    });
    assert(registerDatasetDispatcher.dispatch(
        {"REGISTER_DATASET", "cars", "DATAFRAME", "1", "1", "speed", "numeric", "42"}) == "OK");
    const rlispstat::core::DataFrameModel *registered =
        registerDatasetDispatcher.applicationState().datasets().find("cars");
    assert(registered != nullptr && registered->rows == 1 && registered->columns.size() == 1);
    assert(registerDatasetDispatcher.dispatch({"DATASET_SYNC_STATUS", "missing"}) == "OK\tmissing");
    assert(registerDatasetDispatcher.dispatch({"DATASET_SYNC_STATUS", "cars"}) ==
           "OK\tpresent\t1\t1\t1\tdata_frame\t\t0\t1\tversion\t\t\tEND");
    assert((registeredDatasets == std::vector<std::string>{"cars:show"}));
    assert(registerDatasetDispatcher.dispatch(
        {"REGISTER_DATASET_SILENT", "cars", "DATAFRAME", "1", "1", "speed", "numeric", "42"}) == "OK");
    assert((registeredDatasets == std::vector<std::string>{"cars:show"}));
    assert(registerDatasetDispatcher.dispatch(
        {"REGISTER_DATASET_SILENT", "cars", "DATAFRAME", "1", "1", "speed", "numeric", "43"}) == "OK");
    registered = registerDatasetDispatcher.applicationState().datasets().find("cars");
    assert(registered != nullptr && registered->columns[0].values[0] == "43");
    assert((registeredDatasets == std::vector<std::string>{"cars:show", "cars:silent"}));
    assert(registerDatasetDispatcher.dispatch(
        {"REGISTER_DATASET_SILENT", "cars", "DATAFRAME", "1", "1", "speed",
         "numeric", "43", "DATA_SYNC_SESSION_V1", "same-session"}) == "OK");
    auto &pendingModel = registerDatasetDispatcher.applicationState().generalizedGLMs()["busy"];
    pendingModel.group = "cars";
    pendingModel.rFitPending = true;
    assert(registerDatasetDispatcher.dispatch(
        {"SET_DATA_CELL", "cars", "speed", "1", "44"}).rfind(
             "ERR Wait for the current R calculation", 0) == 0);
    pendingModel.rFitPending = false;
    assert(registerDatasetDispatcher.dispatch(
        {"SET_DATA_CELL", "cars", "speed", "1", "44"}).rfind("OK", 0) == 0);
    assert(registerDatasetDispatcher.dispatch(
        {"REGISTER_DATASET_SILENT", "cars", "DATAFRAME", "1", "1", "speed",
         "numeric", "43", "DATA_SYNC_SESSION_V1", "same-session"}).rfind(
             "ERR dataset changed in LinkEDA", 0) == 0);
    assert(registerDatasetDispatcher.applicationState().datasets().find("cars")
               ->columns[0].values[0] == "44");
    assert(registerDatasetDispatcher.dispatch({"REGISTER_DATASET", "cars"}) ==
           "ERR malformed REGISTER_DATASET command");

    rlispstat::core::PlotModel modelSeed;
    modelSeed.id = "model_seed";
    modelSeed.group = "cars";
    modelSeed.xLabel = "speed";
    modelSeed.yLabel = "distance";
    modelSeed.variables = {{"speed", {1.0}}, {"distance", {2.0}}, {"weight", {3.0}}};
    modelSeed.variableMeta = {{"speed", "numeric"}, {"distance", "numeric"},
                              {"weight", "numeric"}};
    rlispstat::core::CommandDispatcher modelInfoDispatcher({
        {},
        {
            {}, {}, {}, {}, {}, {}, {},
            [&](const std::string &group, rlispstat::core::PlotModel &plot) {
                if (group != "cars") return false;
                plot = modelSeed;
                return true;
            }
        },
        {}, {}, {}
    });
    assert(modelInfoDispatcher.dispatch({"MODEL_INFO"}) == "ERR missing group name");
    assert(modelInfoDispatcher.dispatch({"MODEL_INFO", "missing"}) == "ERR no active plot/group");
    assert(modelInfoDispatcher.dispatch({"MODEL_INFO", "cars"}) ==
           "OK\tcars||all");
    assert(modelInfoDispatcher.dispatch({"MODEL_SET_Y", "cars", "missing"}) ==
           "ERR dependent variable must be an available numeric variable");
    assert(modelInfoDispatcher.dispatch({"MODEL_SET_Y", "cars", "distance"}) == "OK");
    assert(modelInfoDispatcher.dispatch({"MODEL_ADD_TERM", "cars", "speed"}) == "OK");
    assert(modelInfoDispatcher.dispatch(
        {"MODEL_REPLACE_TERM", "cars", "speed", "weight"}) == "OK");
    assert((modelInfoDispatcher.applicationState().groupModels()["cars"].terms ==
            std::vector<std::string>{"weight"}));
    assert(modelInfoDispatcher.dispatch({"MODEL_ADD_TERM", "cars", "speed"}) == "OK");
    assert(modelInfoDispatcher.dispatch({"MODEL_REMOVE_TERM", "cars", "speed"}) == "OK");
    assert(modelInfoDispatcher.dispatch({"MODEL_CLEAR_ROLE", "cars", "weight"}) == "OK");
    assert(modelInfoDispatcher.dispatch({"MODEL_ADD_TERM", "cars", "missing"}) ==
           "ERR predictor must be an available variable");
    assert(modelInfoDispatcher.dispatch({"MODEL_SCOPE", "cars", "invalid"}) ==
           "ERR Analysis scope is global. Use the central Analysis Scope menu.");
    assert(modelInfoDispatcher.dispatch({"MODEL_SCOPE", "cars", "selected"}) ==
           "ERR Analysis scope is global. Use the central Analysis Scope menu.");
    assert(modelInfoDispatcher.dispatch({"MODEL_INFO", "cars"}) ==
           "OK\tcars|distance|all");


    rlispstat::core::CommandDispatcher correlationInfoDispatcher({{}, {}, {}, {}, {}});
    rlispstat::core::CorrelationMatrixState correlation;
    correlation.id = "corr_1";
    correlation.group = "cars";
    correlation.variables = {"speed", "distance"};
    correlation.cells.resize(3);
    correlationInfoDispatcher.applicationState().correlationMatrices()[correlation.id] = correlation;
    assert(correlationInfoDispatcher.dispatch({"CORR_INFO"}) == "ERR missing correlation id");
    assert(correlationInfoDispatcher.dispatch({"CORR_INFO", "missing"}) ==
           "ERR unknown correlation matrix");
    assert(correlationInfoDispatcher.dispatch({"CORR_INFO", "corr_1"}) ==
           "OK\tCORRELATION|corr_1|cars|pearson|pairwise|2|3|speed|distance");

    int openedCorrelationRefits = 0;
    rlispstat::core::CommandDispatcherServices correlationOpenServices;
    correlationOpenServices.queries.groupSeed =
        [&](const std::string &group, rlispstat::core::PlotModel &seed) {
            if (group != "mi_cars") return false;
            seed = modelSeed; seed.group = group; return true;
        };
    correlationOpenServices.queries.groupIsMultipleImputation =
        [](const std::string &group) { return group == "mi_cars"; };
    correlationOpenServices.selection.refitCorrelation = [&](const std::string &id) {
        if (id != "corr_open") return false;
        ++openedCorrelationRefits; return true;
    };
    rlispstat::core::CommandDispatcher correlationOpenDispatcher(correlationOpenServices);
    assert(correlationOpenDispatcher.dispatch(
        {"CORR_OPEN", "corr_open", "mi_cars", "pearson", "pairwise", "TRUE", "FALSE",
         "TRUE", "3", "speed", "distance", "speed"}) == "OK\tcorr_open");
    const auto &openedCorrelation =
        correlationOpenDispatcher.applicationState().correlationMatrices().at("corr_open");
    assert(openedCorrelation.multipleImputation);
    assert(openedCorrelation.title == "Pearson Correlation Matrix - Multiple Imputation");
    assert((openedCorrelation.variables == std::vector<std::string>{"speed", "distance"}));
    assert(openedCorrelation.hasSeed);
    assert(openedCorrelationRefits == 1);
    assert(correlationOpenDispatcher.dispatch(
        {"CORR_OPEN", "corr_bad", "mi_cars", "spearman", "pairwise", "TRUE", "FALSE",
         "TRUE", "1", "speed"}) == "ERR only Pearson correlations are supported");
    assert(correlationOpenDispatcher.dispatch(
        {"CORR_OPEN", "corr_bad", "missing", "pearson", "pairwise", "TRUE", "FALSE",
         "TRUE", "1", "speed"}) == "ERR no registered dataset/group for correlation matrix");

    std::vector<std::string> shownStructuredCorrelations;
    rlispstat::core::CommandDispatcherServices structuredCorrelationServices;
    structuredCorrelationServices.queries.groupSeed =
        [&](const std::string &group, rlispstat::core::PlotModel &seed) {
            if (group != "cars") return false;
            seed = modelSeed;
            return true;
        };
    structuredCorrelationServices.ui.showCorrelationMatrix =
        [&](const std::string &id) { shownStructuredCorrelations.push_back(id); };
    rlispstat::core::CommandDispatcher structuredCorrelationDispatcher(structuredCorrelationServices);
    assert(structuredCorrelationDispatcher.dispatch(
        {"CORR_OPEN_STRUCTURED", "corr_struct", "cars", "Pooled correlations", "pearson", "listwise",
         "TRUE", "FALSE", "TRUE", "5", "Rubin", "Pooled from imputations", "2", "speed", "distance",
         "1", "speed", "distance", "0.5", "0.1", "10", "valid", "pooled", "2", "1", "2"}) ==
           "OK\tcorr_struct");
    const auto &structuredCorrelation =
        structuredCorrelationDispatcher.applicationState().correlationMatrices().at("corr_struct");
    assert(structuredCorrelation.precomputed && structuredCorrelation.multipleImputation);
    assert(structuredCorrelation.imputationCount == 5);
    assert(structuredCorrelation.cells.size() == 1);
    assert(structuredCorrelation.cells.front().rowsUsed == std::vector<int>({1, 2}));
    assert((shownStructuredCorrelations == std::vector<std::string>{"corr_struct"}));
    assert(structuredCorrelationDispatcher.dispatch(
        {"CORR_OPEN_STRUCTURED", "corr_struct_bad", "cars", "Bad", "pearson", "listwise",
         "TRUE", "FALSE", "TRUE", "1", "", "", "1", "missing", "0"}) ==
           "ERR correlation variables must be numeric");

    int openedDimensionalityRefits = 0;
    rlispstat::core::CommandDispatcherServices dimensionalityOpenServices;
    dimensionalityOpenServices.queries.groupSeed =
        [&](const std::string &group, rlispstat::core::PlotModel &seed) {
            if (group != "cars") return false;
            seed = modelSeed; return true;
        };
    dimensionalityOpenServices.selection.refitDimensionality = [&](const std::string &id) {
        if (id != "pca_open" && id != "pca_partial" && id != "pca_blank") return false;
        ++openedDimensionalityRefits; return true;
    };
    rlispstat::core::CommandDispatcher dimensionalityOpenDispatcher(dimensionalityOpenServices);
    assert(dimensionalityOpenDispatcher.dispatch(
        {"PCAFA_OPEN", "pca_open", "cars", "pca", "listwise", "TRUE", "4", "varimax",
         "selected", "3", "speed", "distance", "speed"}) == "OK\tpca_open");
    const auto &openedDimensionality =
        dimensionalityOpenDispatcher.applicationState().dimensionalityModels().at("pca_open");
    assert(openedDimensionality.rotation == "varimax");
    assert(openedDimensionality.scope == "selected");
    assert(openedDimensionality.componentCount == 4);
    assert((openedDimensionality.variables == std::vector<std::string>{"speed", "distance"}));
    assert(openedDimensionality.hasSeed);
    assert(openedDimensionalityRefits == 1);
    assert(dimensionalityOpenDispatcher.dispatch(
        {"PCAFA_OPEN", "pca_partial", "cars", "pca", "listwise", "TRUE", "2",
         "none", "all", "1", "speed"}) == "OK\tpca_partial");
    assert((dimensionalityOpenDispatcher.applicationState().dimensionalityModels()
        .at("pca_partial").variables == std::vector<std::string>{"speed"}));
    assert(dimensionalityOpenDispatcher.dispatch(
        {"PCAFA_OPEN", "pca_blank", "cars", "pca", "listwise", "TRUE", "2",
         "none", "all", "0"}) == "OK\tpca_blank");
    assert(dimensionalityOpenDispatcher.applicationState().dimensionalityModels()
        .at("pca_blank").variables.empty());
    assert(openedDimensionalityRefits == 3);

    int openedDendrogramRefits = 0;
    rlispstat::core::CommandDispatcherServices dendrogramOpenServices;
    dendrogramOpenServices.queries.groupSeed =
        [&](const std::string &group, rlispstat::core::PlotModel &seed) {
            if (group != "cars") return false;
            seed = modelSeed; return true;
        };
    dendrogramOpenServices.selection.refitDendrogram = [&](const std::string &id) {
        if (id != "tree_open") return false;
        ++openedDendrogramRefits; return true;
    };
    rlispstat::core::CommandDispatcher dendrogramOpenDispatcher(dendrogramOpenServices);
    assert(dendrogramOpenDispatcher.dispatch(
        {"DENDRO_OPEN", "tree_open", "cars", "euclidean", "complete", "pairwise", "3",
         "speed", "distance", "speed"}) == "OK\ttree_open");
    const auto &openedDendrogram =
        dendrogramOpenDispatcher.applicationState().dendrograms().at("tree_open");
    assert(openedDendrogram.distance == "euclidean");
    assert(openedDendrogram.linkage == "complete");
    assert((openedDendrogram.variables == std::vector<std::string>{"speed", "distance"}));
    assert(openedDendrogram.hasSeed);
    assert(openedDendrogramRefits == 1);
    assert(dendrogramOpenDispatcher.dispatch(
        {"DENDRO_OPEN", "tree_bad", "cars", "bad", "complete", "pairwise", "1", "speed"}) ==
           "ERR unsupported dendrogram distance");

    std::vector<std::string> dimensionalityUpdates;
    rlispstat::core::CommandDispatcherServices dimensionalityUpdateServices;
    dimensionalityUpdateServices.ui.showDimensionality =
        [&](const std::string &id) { dimensionalityUpdates.push_back("show:" + id); };
    dimensionalityUpdateServices.ui.refreshDimensionalityPlots =
        [&](const std::string &id) { dimensionalityUpdates.push_back("refresh:" + id); };
    rlispstat::core::CommandDispatcher dimensionalityUpdateDispatcher(dimensionalityUpdateServices);
    rlispstat::core::DimensionalityState pendingDimensionality;
    pendingDimensionality.id = "pca_update";
    pendingDimensionality.group = "cars";
    pendingDimensionality.variables = {"speed", "distance"};
    dimensionalityUpdateDispatcher.applicationState().dimensionalityModels()["pca_update"] =
        pendingDimensionality;
    assert(dimensionalityUpdateDispatcher.dispatch(
        {"PCAFA_UPDATE", "pca_update", "cars", "pca", "listwise", "none", "all", "TRUE", "2",
         "fitted", "2", "speed", "distance", "2", "1", "2", "1", "3", "1", "1", "2.0", "NA",
         "0.5", "0.5", "1", "speed", "0.8", "0.2", "2", "0.1", "0.2", "1", "1", "2", "3", "4"}) ==
           "OK\tpca_update");
    const auto &updatedDimensionality =
        dimensionalityUpdateDispatcher.applicationState().dimensionalityModels().at("pca_update");
    assert(updatedDimensionality.rowsUsed == std::vector<int>({1, 2}));
    assert(updatedDimensionality.rowsExcluded == std::vector<int>({3}));
    assert(updatedDimensionality.components.size() == 1);
    assert(updatedDimensionality.loadings.front().values == std::vector<double>({0.1, 0.2}));
    assert(updatedDimensionality.scores.front().x == 3.0 && updatedDimensionality.scores.front().y == 4.0);
    assert((dimensionalityUpdates == std::vector<std::string>{"show:pca_update", "refresh:pca_update"}));

    dimensionalityUpdates.clear();
    auto &changedDimensionality = dimensionalityUpdateDispatcher.applicationState()
        .dimensionalityModels().at("pca_update");
    changedDimensionality.variables = {"speed", "acceleration"};
    assert(dimensionalityUpdateDispatcher.dispatch(
        {"PCAFA_UPDATE", "pca_update", "cars", "pca", "listwise", "none", "all", "TRUE", "2",
         "stale", "2", "speed", "distance", "0", "0", "0", "0", "0"}) ==
           "OK\tpca_update");
    assert((changedDimensionality.variables == std::vector<std::string>{"speed", "acceleration"}));
    assert(dimensionalityUpdates.empty());

    changedDimensionality.variables = {"speed", "distance"};
    changedDimensionality.autoFit = false;
    assert(dimensionalityUpdateDispatcher.dispatch(
        {"PCAFA_UPDATE", "pca_update", "cars", "pca", "listwise", "none", "all", "TRUE", "2",
         "ignored while auto-fit is off", "2", "speed", "distance", "0", "0", "0", "0", "0"}) ==
           "OK\tpca_update");
    assert(!changedDimensionality.autoFit);
    assert(dimensionalityUpdates.empty());

    rlispstat::core::CommandDispatcher dimensionalityInfoDispatcher({{}, {}, {}, {}, {}});
    rlispstat::core::DimensionalityState dimensionality;
    dimensionality.id = "pca_1";
    dimensionality.group = "cars";
    dimensionality.variables = {"speed", "distance"};
    dimensionality.rowsUsed = {1, 2};
    dimensionality.loadings.resize(2);
    dimensionalityInfoDispatcher.applicationState().dimensionalityModels()[dimensionality.id] = dimensionality;
    assert(dimensionalityInfoDispatcher.dispatch({"PCAFA_INFO"}) ==
           "ERR missing dimensionality model id");
    assert(dimensionalityInfoDispatcher.dispatch({"PCAFA_INFO", "missing"}) ==
           "ERR unknown principal components/factor analysis model");
    assert(dimensionalityInfoDispatcher.dispatch({"PCAFA_INFO", "pca_1"}) ==
           "OK\tDIMENSIONALITY|pca_1|cars|pca|listwise|TRUE|2|none|all|2|2|2|speed|distance");

    int correlationRefits = 0;
    rlispstat::core::CommandDispatcherServices correlationVariableServices;
    correlationVariableServices.selection.refitCorrelation = [&](const std::string &id) {
        if (id != "corr_2") return false;
        ++correlationRefits; return true;
    };
    rlispstat::core::CommandDispatcher correlationVariablesDispatcher(correlationVariableServices);
    rlispstat::core::CorrelationMatrixState correlationVariables;
    correlationVariables.id = "corr_2";
    correlationVariables.seed.variables = {{"x", {1.0}}, {"y", {2.0}}};
    correlationVariablesDispatcher.applicationState().correlationMatrices()["corr_2"] = correlationVariables;
    assert(correlationVariablesDispatcher.dispatch({"CORR_SET_VARIABLES", "corr_2", "3", "x", "y", "x"}) == "OK");
    assert((correlationVariablesDispatcher.applicationState().correlationMatrices().at("corr_2").variables ==
            std::vector<std::string>{"x", "y"}));
    assert(correlationVariablesDispatcher.dispatch({"CORR_SET_VARIABLES", "corr_2", "1", "missing"}) ==
           "ERR correlation variables must be numeric");
    assert(correlationRefits == 1);

    int dimensionalityRefits = 0;
    rlispstat::core::CommandDispatcherServices dimensionalityVariableServices;
    dimensionalityVariableServices.selection.refitDimensionality = [&](const std::string &id) {
        if (id != "pca_2") return false;
        ++dimensionalityRefits; return true;
    };
    rlispstat::core::CommandDispatcher dimensionalityVariablesDispatcher(dimensionalityVariableServices);
    rlispstat::core::DimensionalityState dimensionalityVariables;
    dimensionalityVariables.id = "pca_2";
    dimensionalityVariables.seed.variables = {{"x", {1.0}}, {"y", {2.0}}, {"z", {3.0}}};
    dimensionalityVariables.eligibleVariables = {"x", "y", "z"};
    dimensionalityVariablesDispatcher.applicationState().dimensionalityModels()["pca_2"] = dimensionalityVariables;
    assert(dimensionalityVariablesDispatcher.dispatch(
        {"PCAFA_SET_VARIABLES", "pca_2", "3", "x", "y", "x"}) == "OK");
    assert((dimensionalityVariablesDispatcher.applicationState().dimensionalityModels().at("pca_2").variables ==
            std::vector<std::string>{"x", "y"}));
    assert(dimensionalityVariablesDispatcher.dispatch(
        {"PCAFA_SET_VARIABLES", "pca_2", "1", "x"}) == "OK");
    assert((dimensionalityVariablesDispatcher.applicationState().dimensionalityModels()
        .at("pca_2").variables == std::vector<std::string>{"x"}));
    assert(dimensionalityVariablesDispatcher.dispatch(
        {"PCAFA_SET_VARIABLES", "pca_2", "0"}) == "OK");
    assert(dimensionalityVariablesDispatcher.applicationState().dimensionalityModels()
        .at("pca_2").variables.empty());
    assert(dimensionalityVariablesDispatcher.dispatch(
        {"PCAFA_SET_VARIABLES", "pca_2", "2", "y", "z"}) == "OK");
    assert((dimensionalityVariablesDispatcher.applicationState().dimensionalityModels()
        .at("pca_2").variables == std::vector<std::string>{"y", "z"}));
    assert(dimensionalityRefits == 4);

    std::vector<std::string> shownScaleAnalyses;
    int scaleRefits = 0;
    rlispstat::core::CommandDispatcherServices scaleServices;
    scaleServices.ui.showScaleAnalysis = [&](const std::string &id) {
        shownScaleAnalyses.push_back(id);
    };
    scaleServices.selection.refitScaleAnalysis = [&](const std::string &id) {
        ++scaleRefits;
        return id == "scale_1";
    };
    rlispstat::core::CommandDispatcher scaleDispatcher(scaleServices);
    rlispstat::core::DataFrameModel scaleData;
    scaleData.group = "scale_data";
    scaleData.rows = 4;
    rlispstat::core::DataColumn scaleQ1;
    scaleQ1.name = "q1"; scaleQ1.type = "numeric"; scaleQ1.values = {"1", "2", "3", "4"};
    rlispstat::core::DataColumn scaleQ2;
    scaleQ2.name = "q2"; scaleQ2.type = "numeric"; scaleQ2.values = {"4", "3", "2", "1"};
    rlispstat::core::DataColumn scaleGroup;
    scaleGroup.name = "group"; scaleGroup.type = "factor"; scaleGroup.values = {"a", "b", "a", "b"};
    scaleData.columns = {scaleQ1, scaleQ2, scaleGroup};
    assert(scaleDispatcher.applicationState().registerDataset(scaleData));
    assert(scaleDispatcher.dispatch({
        "SCALE_ANALYSIS_OPEN", "scale_1", "scale_data", "ordinary", "1", "1", "fp-1",
        "alpha = 0.8", "2",
        "q1", "numeric", "forward", "FALSE", "", "", "2.5", "1.3", "0", "0.6", "0.7",
        "q2", "numeric", "forward", "FALSE", "", "", "2.5", "1.3", "0", "0.6", "0.7",
        "psych::alpha"
    }) == "OK\tscale_1");
    const auto &openedScale = scaleDispatcher.applicationState().scaleAnalyses().at("scale_1");
    assert(openedScale.specification.items.size() == 2);
    assert(openedScale.result.items.size() == 2);
    assert(openedScale.result.fingerprint == "fp-1");
    assert((shownScaleAnalyses == std::vector<std::string>{"scale_1"}));
    const auto *scaleCode =
        scaleDispatcher.applicationState().outputCodeReference("scale_1");
    assert(scaleCode != nullptr);
    assert(scaleCode->outputBlockId == "table");
    const std::string scaleVerification =
        rlispstat::core::BuildAnalysisVerificationRCode(
            scaleCode->provenance, scaleCode->outputBlockId);
    assert(scaleVerification.find("psych::alpha") != std::string::npos);
    assert(scaleVerification.find("verification_data_path") != std::string::npos);
    assert(scaleVerification.find("LinkEDA:::") == std::string::npos);
    assert(scaleCode->publication.table.has_value());
    assert(scaleCode->publication.table->rows.size() == 2);
    assert(scaleDispatcher.dispatch({
        "SCALE_ANALYSIS_UPDATE", "scale_1", "scale_data", "multiple_imputation", "5", "1", "fp-1",
        "m = 5; alpha by imputation", "2",
        "q1", "numeric", "forward", "FALSE", "", "", "2.5", "1.3", "0", "0.6", "0.7",
        "q2", "numeric", "forward", "FALSE", "", "", "2.5", "1.3", "0", "0.6", "0.7",
        "psych::alpha per imputation; not Rubin-pooled",
        "SCALE_DETAILS_V1",
        "2", "4", "0", "2.5", "1.3", "0.5", "0.8", "0.81", "", "descriptive_by_imputation_not_Rubin_pooled",
        "2", "1", "0.79", "0.80", "", "2", "0.81", "0.82", "",
        "pearson", "formal_fisher_z_plus_mice_pool_scalar", "2", "q1", "q2", "1", "0.6", "0.6", "1",
        "SCALE_CORRELATION_DETAILS_V1", "", "0.04", "0.04", "", "4", "4", "4", "4",
        "value", "1", "1", "2", "1", "MR1",
        "q1", "0.7", "0.49", "0.51", "q2", "0.8", "0.64", "0.36",
        "mean", "descriptive_across_imputations", "20", "2.5", "0.7", "1", "4",
        "SCALE_PLOTS_V1", "2",
        "score_distribution", "Imputation 1", "2", "1", "0.4", "2", "0.6",
        "scree_observed", "Observed mean", "2", "1", "1.2", "2", "0.8"
    }) == "OK\tscale_1");
    const auto &detailedScale = scaleDispatcher.applicationState().scaleAnalyses().at("scale_1");
    assert(detailedScale.result.imputationCount == 5);
    assert(detailedScale.result.scaleSummary.alpha == "0.8");
    assert(detailedScale.result.reliabilityByImputation.size() == 2);
    assert(detailedScale.result.correlations.variables.size() == 2);
    assert(detailedScale.result.correlations.values.size() == 4);
    assert(detailedScale.result.correlations.pValues.size() == 4);
    assert(detailedScale.result.correlations.pValues[1] == "0.04");
    assert(detailedScale.result.correlations.sampleSizes.size() == 4);
    assert(detailedScale.result.correlations.sampleSizes[1] == "4");
    assert(detailedScale.result.dimensionality.loadings.size() == 2);
    assert(detailedScale.result.dimensionality.factorNames == std::vector<std::string>{"MR1"});
    assert(detailedScale.result.scores.method == "mean");
    assert(detailedScale.result.scores.validN == "20");
    assert(detailedScale.result.plotSeries.size() == 2);
    assert(detailedScale.result.plotSeries[0].kind == "score_distribution");
    assert(detailedScale.result.plotSeries[0].labels ==
        std::vector<std::string>({"1", "2"}));
    assert(detailedScale.result.plotSeries[1].values ==
        std::vector<std::string>({"1.2", "0.8"}));
    scaleCode = scaleDispatcher.applicationState().outputCodeReference("scale_1");
    assert(scaleCode != nullptr);
    const std::string scaleMiVerification =
        rlispstat::core::BuildAnalysisVerificationRCode(
            scaleCode->provenance, scaleCode->outputBlockId);
    assert(scaleMiVerification.find("completed_sets <- split") != std::string::npos);
    assert(scaleMiVerification.find("not Rubin-pooled") != std::string::npos);
    assert(scaleDispatcher.dispatch({
        "SCALE_ANALYSIS_UPDATE", "scale_1", "scale_data", "ordinary", "1", "0", "stale",
        "stale", "0", "psych::alpha"
    }).find("ERR stale Scale Analysis result rejected") == 0);
    assert(scaleDispatcher.dispatch({
        "SCALE_ANALYSIS_SET_ITEMS", "scale_1", "2",
        "q1", "numeric", "FALSE", "FALSE", "", "",
        "q2", "numeric", "TRUE", "TRUE", "1", "4"
    }) == "OK");
    const auto &editedScale = scaleDispatcher.applicationState().scaleAnalyses().at("scale_1");
    assert(editedScale.specification.revision == 2);
    assert(editedScale.specification.items[1].reversed);
    assert(editedScale.specification.items[1].hasScoringRange);
    assert(scaleRefits == 1);
    assert(scaleDispatcher.dispatch({"SCALE_ANALYSIS_INFO", "scale_1"}).find(
        "OK\tSCALE_ANALYSIS|scale_1|scale_data|2|") == 0);
    assert(scaleDispatcher.dispatch({
        "SCALE_ANALYSIS_SET_ITEMS", "scale_1", "1",
        "group", "numeric", "FALSE", "FALSE", "", ""
    }) == "OK");
    const auto &dichotomousScale =
        scaleDispatcher.applicationState().scaleAnalyses().at("scale_1");
    assert(dichotomousScale.specification.items.size() == 1);
    assert(dichotomousScale.specification.items.front().type ==
           rlispstat::core::ScaleItemType::Numeric);

    int dendrogramRefits = 0;
    rlispstat::core::CommandDispatcherServices dendrogramVariableServices;
    dendrogramVariableServices.selection.refitDendrogram = [&](const std::string &id) {
        if (id != "tree_2") return false;
        ++dendrogramRefits; return true;
    };
    rlispstat::core::CommandDispatcher dendrogramVariablesDispatcher(dendrogramVariableServices);
    rlispstat::core::DendrogramState dendrogramVariables;
    dendrogramVariables.id = "tree_2";
    dendrogramVariables.seed.variables = {{"x", {1.0}}, {"y", {2.0}}};
    dendrogramVariablesDispatcher.applicationState().dendrograms()["tree_2"] = dendrogramVariables;
    assert(dendrogramVariablesDispatcher.dispatch(
        {"DENDRO_SET_VARIABLES", "tree_2", "3", "x", "y", "x"}) == "OK");
    assert((dendrogramVariablesDispatcher.applicationState().dendrograms().at("tree_2").variables ==
            std::vector<std::string>{"x", "y"}));
    assert(dendrogramVariablesDispatcher.dispatch({"DENDRO_SET_VARIABLES", "tree_2", "0"}) ==
           "OK");
    assert(dendrogramVariablesDispatcher.applicationState().dendrograms()
        .at("tree_2").variables.empty());
    assert(dendrogramRefits == 2);

    rlispstat::core::PlotModel histogramPlot;
    histogramPlot.id = "hist_1";
    histogramPlot.kind = "histogram";
    histogramPlot.histogramDensityMode = "none";
    histogramPlot.histogramPoints = {{1.0, 1, 0}, {2.0, 2, 0}, {3.0, 3, 0}};
    rlispstat::core::RebinHistogram(histogramPlot, 2);
    int histogramRedraws = 0;
    rlispstat::core::CommandDispatcher histogramDispatcher({
        {},
        {
            {}, {},
            [&](const std::string &plotId, rlispstat::core::PlotModel &plot) {
                if (plotId != "hist_1") return false;
                plot = histogramPlot;
                return true;
            }
        },
        {
            {}, {}, {},
            [&](const std::string &plotId) {
                assert(plotId == "hist_1");
                ++histogramRedraws;
            }
        },
        {},
        {
            {}, {}, {}, {}, {}, {}, {}, {}, {}, {},
            [&](const std::string &plotId,
                const std::function<void(rlispstat::core::PlotModel &)> &mutation) {
                if (plotId != "hist_1") return false;
                mutation(histogramPlot);
                return true;
            }
        }
    });
    assert(histogramDispatcher.dispatch({"HIST_BREAKS", "hist_1"}).rfind("OK\t", 0) == 0);
    assert(histogramDispatcher.dispatch({"HIST_DENSITY_INFO", "hist_1"}) ==
           "OK\tFALSE|none|0|1");
    assert(histogramDispatcher.dispatch({"HIST_SHOW_DENSITY", "hist_1", "TRUE"}) == "OK");
    assert(histogramPlot.histogramShowDensity);
    assert(histogramPlot.histogramDensityMode == "all");
    assert(histogramDispatcher.dispatch({"HIST_SET_DENSITY_MODE", "hist_1", "selected"}) == "OK");
    assert(histogramPlot.histogramDensityMode == "selected");
    assert(histogramDispatcher.dispatch({"HIST_SET_DENSITY_BW", "hist_1", "0.25"}) == "OK");
    assert(histogramPlot.histogramDensityBw == 0.25);
    assert(histogramDispatcher.dispatch({"HIST_SET_DENSITY_ADJUST", "hist_1", "2"}) == "OK");
    assert(histogramPlot.histogramDensityAdjust == 2.0);
    assert(histogramDispatcher.dispatch({"HIST_SET_BINS", "hist_1", "3"}) == "OK");
    assert(histogramPlot.histogramBins.size() == 3);
    assert(histogramDispatcher.dispatch({"HIST_SET_BINNING_RULE", "hist_1", "sqrt"}) == "OK");
    assert(histogramDispatcher.dispatch({"HIST_SET_BINS", "hist_1", "0"}) ==
           "ERR histogram bin count must be positive");
    assert(histogramDispatcher.dispatch({"HIST_SET_DENSITY_MODE", "hist_1", "bad"}) ==
           "ERR invalid histogram density mode");
    assert(histogramDispatcher.dispatch({"HIST_SET_DENSITY_BW", "hist_1", "-1"}) == "OK");
    assert(histogramPlot.histogramDensityBw == 0.0);
    assert(histogramDispatcher.dispatch({"HIST_SET_DENSITY_BW", "hist_1", "bad"}) ==
           "ERR density bandwidth must be non-negative");
    assert(histogramDispatcher.dispatch({"HIST_BREAKS", "missing"}) == "ERR no active plot");
    assert(histogramRedraws == 7);

    rlispstat::core::DataFrameModel barData;
    barData.group = "bar_data";
    barData.rows = 4;
    barData.columns = {
        {"x", "factor", "", "", -1, {"A", "A", "B", "B"}},
        {"split", "factor", "", "", -1, {"No", "Yes", "No", "Yes"}},
        {"other", "factor", "", "", -1, {"L", "M", "L", "M"}}
    };
    rlispstat::core::PlotModel barPlot;
    barPlot.id = "bar_1";
    barPlot.group = barData.group;
    barPlot.kind = "barplot";
    barPlot.xLabel = "x";
    barPlot.barplotXVariables = {"x"};
    std::string barBuildError;
    assert(rlispstat::core::RebuildBarplotFromDataFrame(barPlot, barData, &barBuildError));
    int barRedraws = 0;
    rlispstat::core::CommandDispatcherServices barServices;
    barServices.queries.plot = [&](const std::string &id, rlispstat::core::PlotModel &plot) {
        if (id != barPlot.id) return false;
        plot = barPlot;
        return true;
    };
    barServices.ui.redrawPlot = [&](const std::string &id) {
        assert(id == barPlot.id);
        ++barRedraws;
    };
    barServices.selection.mutateBarplot = [&](const std::string &id,
            const std::function<void(rlispstat::core::PlotModel &)> &mutation) {
        if (id != barPlot.id) return false;
        mutation(barPlot);
        return true;
    };
    rlispstat::core::CommandDispatcher barDispatcher(barServices);
    barDispatcher.applicationState().datasets().registerDataset(barData);
    assert(barDispatcher.dispatch({"BARPLOT_SPLIT_BY", "bar_1", "split"}) == "OK");
    assert(barPlot.barplotSplitVariable == "split");
    assert(barPlot.barplotShowConditionalPercent);
    assert(barDispatcher.dispatch({"BARPLOT_MODE", "bar_1", "overall_percent"}) == "OK");
    assert(barPlot.barplotMode == "overall_percent");
    assert(barDispatcher.dispatch({"BARPLOT_ROW_COLORS", "bar_1", "tooltip"}) == "OK");
    assert(barPlot.barplotRowColorDisplay == "tooltip");
    assert(barDispatcher.dispatch({"BARPLOT_SELECTION_DISPLAY", "bar_1", "outline"}) == "OK");
    assert(barPlot.barplotSelectionDisplay == "outline");
    assert(barDispatcher.dispatch({"BARPLOT_SPLIT_STROKE_WIDTH", "bar_1", "6"}) == "OK");
    assert(barPlot.barplotSplitStrokeWidth == 6.0);
    assert(barDispatcher.dispatch({"BARPLOT_LEVEL_SET_COLOR", "bar_1", "Yes", "blue"}) == "OK");
    assert(barPlot.barplotYLevelColors["Yes"] == "blue");
    assert(barDispatcher.dispatch({"BARPLOT_SET_X", "bar_1", "other"}) == "OK");
    assert((barPlot.barplotXVariables == std::vector<std::string>{"other"}));
    assert(barDispatcher.dispatch({"BARPLOT_CLEAR_SPLIT", "bar_1"}) == "OK");
    assert(barPlot.barplotSplitVariable.empty());
    assert(!barPlot.barplotShowConditionalPercent);
    assert(barRedraws == 8);

    rlispstat::core::PlotModel requestedSmoothPlot;
    requestedSmoothPlot.id = "smooth_1";
    rlispstat::core::SmoothCurveData requestedSmoothCurve;
    requestedSmoothCurve.scope = rlispstat::core::SmoothCurveScope::Selection;
    requestedSmoothCurve.groupId = "blue";
    requestedSmoothCurve.x = {1.0, 2.0};
    requestedSmoothCurve.y = {3.0, 4.0};
    requestedSmoothCurve.ok = true;
    requestedSmoothPlot.smoothCurves.push_back(requestedSmoothCurve);
    int smoothRedraws = 0;
    std::vector<rlispstat::core::SmoothCurveScope> toggledSmoothScopes;
    rlispstat::core::CommandDispatcherServices smoothServices;
    smoothServices.queries.plot = [&](const std::string &id, rlispstat::core::PlotModel &plot) {
        if (id != requestedSmoothPlot.id) return false;
        plot = requestedSmoothPlot;
        return true;
    };
    smoothServices.ui.redrawPlot = [&](const std::string &id) {
        assert(id == requestedSmoothPlot.id);
        ++smoothRedraws;
    };
    smoothServices.selection.toggleSmoothCurves =
        [&](const std::string &id, rlispstat::core::SmoothCurveScope scope) {
            if (id != requestedSmoothPlot.id) return false;
            toggledSmoothScopes.push_back(scope);
            return true;
        };
    rlispstat::core::CommandDispatcher requestedSmoothDispatcher(smoothServices);
    assert(requestedSmoothDispatcher.dispatch({"SMOOTH_INFO", "smooth_1"}) == "OK\tselected|1|blue|2");
    assert(requestedSmoothDispatcher.dispatch({"REQUEST_SMOOTH", "smooth_1", "color"}) == "OK");
    assert(toggledSmoothScopes == std::vector<rlispstat::core::SmoothCurveScope>(
        {rlispstat::core::SmoothCurveScope::ColorGroup}));
    assert(smoothRedraws == 1);
    assert(requestedSmoothDispatcher.dispatch({"REQUEST_SMOOTH", "smooth_1", "bad"}) ==
           "ERR invalid smooth scope");

    std::vector<rlispstat::core::MainRCompareMeansTask> compareMeansTasks;
    rlispstat::core::CommandDispatcherServices compareMeansServices;
    compareMeansServices.ui.queueCompareMeansTask =
        [&](const rlispstat::core::MainRCompareMeansTask &task) { compareMeansTasks.push_back(task); };
    rlispstat::core::CommandDispatcher compareMeansDispatcher(compareMeansServices);
    rlispstat::core::DataFrameModel compareMeansDataset;
    compareMeansDataset.group = "cars";
    compareMeansDispatcher.applicationState().datasets().registerDataset(compareMeansDataset);
    assert(compareMeansDispatcher.dispatch(
        {"REQUEST_COMPARE_MEANS", "cars", "t_test", "speed", "distance", "gear"}) == "OK");
    assert(compareMeansTasks.size() == 1);
    assert(compareMeansTasks.front().var2 == "distance" && compareMeansTasks.front().groupVar == "gear");
    assert(compareMeansDispatcher.dispatch({"REQUEST_COMPARE_MEANS", "missing", "t_test", "speed"}) ==
           "ERR dataset is not registered");
    assert(compareMeansDispatcher.dispatch({"MAIN_R_TASKS"}) == "OK");

    int modelErrorRefreshes = 0;
    rlispstat::core::CommandDispatcherServices modelErrorServices;
    modelErrorServices.queries.groupSeed = [&](const std::string &group, rlispstat::core::PlotModel &seed) {
        if (group != "cars") return false;
        seed = modelSeed;
        return true;
    };
    modelErrorServices.ui.refreshModelGroup = [&](const std::string &group) {
        assert(group == "cars");
        ++modelErrorRefreshes;
    };
    rlispstat::core::CommandDispatcher modelErrorDispatcher(modelErrorServices);
    rlispstat::core::GroupModelState pendingLinearError;
    pendingLinearError.group = "cars";
    pendingLinearError.rFitPending = true;
    pendingLinearError.isStale = true;
    pendingLinearError.lastRFitSignature = "current-selected-row-snapshot";
    modelErrorDispatcher.applicationState().groupModels()["cars"] = pendingLinearError;
    assert(modelErrorDispatcher.dispatch({
        "MODEL_UPDATE_ERROR", "cars", "obsolete failure", "LINEAR_RESULT_V1",
        "older-selected-row-snapshot"}) == "OK\tglm:cars");
    assert(modelErrorDispatcher.applicationState().groupModels().at("cars").rFitPending);
    assert(modelErrorRefreshes == 0);
    assert(modelErrorDispatcher.dispatch({
        "MODEL_UPDATE_ERROR", "cars", "current failure", "LINEAR_RESULT_V1",
        "current-selected-row-snapshot"}) == "OK\tglm:cars");
    assert(!modelErrorDispatcher.applicationState().groupModels().at("cars").rFitPending);
    assert(modelErrorDispatcher.applicationState().linearModelFits().at("cars").warning ==
           "current failure");
    assert(modelErrorRefreshes == 1);
    assert(modelErrorDispatcher.dispatch({"MODEL_UPDATE_ERROR", "cars", "R failed"}) == "OK\tglm:cars");
    assert(!modelErrorDispatcher.applicationState().linearModelFits().at("cars").ok);
    assert(modelErrorDispatcher.applicationState().linearModelFits().at("cars").warning == "R failed");
    assert(modelErrorRefreshes == 2);

    // A result with the exact current identity but incompatible term
    // semantics is a completed failed request, not a stale reply.  It must
    // clear the pending flag and refresh with a visible error; otherwise the
    // native table remains permanently stuck on the preceding predictor.
    int semanticMismatchUpdates = 0;
    rlispstat::core::CommandDispatcherServices semanticMismatchServices;
    semanticMismatchServices.queries.groupSeed =
        [&](const std::string &group, rlispstat::core::PlotModel &seed) {
            if (group != "cars") return false;
            seed = modelSeed;
            return true;
        };
    semanticMismatchServices.ui.modelUpdated =
        [&](const std::string &group, const std::vector<int> &rows) {
            assert(group == "cars");
            assert(rows.empty());
            ++semanticMismatchUpdates;
        };
    rlispstat::core::CommandDispatcher semanticMismatchDispatcher(
        semanticMismatchServices);
    rlispstat::core::GroupModelState pendingCoastal;
    pendingCoastal.group = "cars";
    pendingCoastal.response = "distance";
    pendingCoastal.scope = "all";
    pendingCoastal.terms = {"coastal"};
    pendingCoastal.termTypes = {{"coastal", "factor"}};
    pendingCoastal.rFitPending = true;
    pendingCoastal.isStale = true;
    pendingCoastal.lastRFitSignature = rlispstat::core::LinearGLMFitSignature(
        pendingCoastal.response, pendingCoastal.terms,
        pendingCoastal.termTypes, pendingCoastal.scope,
        pendingCoastal.centeredPredictors,
        pendingCoastal.factorReferenceLevels);
    semanticMismatchDispatcher.applicationState().groupModels()["cars"] =
        pendingCoastal;
    std::vector<std::string> mismatchedCurrentUpdate = {
        "MODEL_UPDATE", "cars", "distance", "all", "1", "coastal",
        // Minimal complete linear-fit payload.
        "TRUE", "1", "0", "1", "0", "1", "1", "1", "0.1",
        "1", "0", "1", "0", "0", "0", "NA", "NA", "",
        // Used rows, excluded rows, then one incorrectly numeric row.
        "0", "0", "1",
        "coastal", "coastal", "numeric", "coefficient", "coastal", "", "",
        "1", "NA", "1", "1", "0.1", "NA", "NA",
        // Diagnostics and design payloads, followed by exact identity.
        "0", "0", "LINEAR_RESULT_V1", pendingCoastal.lastRFitSignature
    };
    assert(semanticMismatchDispatcher.dispatch(mismatchedCurrentUpdate) ==
           "OK\tglm:cars");
    const auto &rejectedCurrent =
        semanticMismatchDispatcher.applicationState().groupModels().at("cars");
    assert(!rejectedCurrent.rFitPending);
    assert(!rejectedCurrent.isStale);
    assert(semanticMismatchUpdates == 1);
    const auto &semanticError =
        semanticMismatchDispatcher.applicationState().linearModelFits().at("cars");
    assert(!semanticError.ok);
    assert(semanticError.warning.find("exact current model specification") !=
           std::string::npos);

    int comparisonErrorRefreshes = 0;
    rlispstat::core::CommandDispatcherServices comparisonErrorServices;
    comparisonErrorServices.ui.refreshRegressionComparison =
        [&](const std::string &group) { assert(group == "cars"); ++comparisonErrorRefreshes; };
    rlispstat::core::CommandDispatcher comparisonErrorDispatcher(comparisonErrorServices);
    rlispstat::core::RegressionComparisonState comparisonErrorState;
    comparisonErrorState.id = "cmp_1";
    comparisonErrorState.group = "cars";
    comparisonErrorState.rFitPending = true;
    comparisonErrorState.rFitGeneration = 4;
    comparisonErrorState.models.push_back({});
    comparisonErrorDispatcher.applicationState().regressionComparisons()["cmp_1"] = comparisonErrorState;
    assert(comparisonErrorDispatcher.dispatch(
               {"REGCMP_UPDATE_ERROR", "cmp_1", "3", "obsolete failure"}) == "OK\tcmp_1");
    assert(comparisonErrorDispatcher.applicationState().regressionComparisons().at("cmp_1").rFitPending);
    assert(comparisonErrorDispatcher.dispatch({"REGCMP_UPDATE_ERROR", "cmp_1", "fit failed"}) == "OK\tcmp_1");
    assert(!comparisonErrorDispatcher.applicationState().regressionComparisons().at("cmp_1").rFitPending);
    assert(comparisonErrorDispatcher.applicationState().regressionComparisons().at("cmp_1").models.front().fit.warning ==
           "fit failed");
    assert(comparisonErrorRefreshes == 1);

    rlispstat::core::CommandDispatcher regressionComparisonUpdateDispatcher({{}, {}, {}, {}, {}});
    assert(regressionComparisonUpdateDispatcher.dispatch({"REGCMP_UPDATE"}) ==
           "ERR malformed REGCMP_UPDATE command");

    int versionedComparisonUpdates = 0;
    rlispstat::core::CommandDispatcherServices versionedComparisonServices;
    versionedComparisonServices.queries.groupSeed =
        [&](const std::string &group, rlispstat::core::PlotModel &seed) {
            if (group != "cars") return false;
            seed = modelSeed;
            return true;
        };
    versionedComparisonServices.ui.regressionComparisonUpdated =
        [&](const rlispstat::core::RegressionComparisonState &) {
            ++versionedComparisonUpdates;
        };
    rlispstat::core::CommandDispatcher versionedComparisonDispatcher(versionedComparisonServices);
    rlispstat::core::RegressionComparisonState versionedComparison;
    versionedComparison.id = "cmp_versioned";
    versionedComparison.group = "cars";
    versionedComparison.response = "distance";
    versionedComparison.scope = "all";
    versionedComparison.autoRefit = true;
    versionedComparison.termRows = {"(Intercept)", "planet"};
    versionedComparison.rFitPending = true;
    versionedComparison.rFitGeneration = 8;
    rlispstat::core::RegressionComparisonModel versionedModel;
    versionedModel.id = "cmp_versioned:model:1";
    versionedModel.label = "Untitled 1";
    versionedModel.response = "distance";
    versionedModel.terms = {"planet"};
    versionedModel.factorReferenceLevels = {{"planet", "B"}};
    versionedModel.modelVersion = 3;
    versionedModel.isStale = true;
    versionedModel.fitState = rlispstat::core::RegressionComparisonFitState::Pending;
    versionedComparison.models.push_back(versionedModel);
    versionedComparisonDispatcher.applicationState().regressionComparisons()[versionedComparison.id] =
        versionedComparison;

    // The current generation is authoritative even when R returns the
    // effective factor type rather than the native sparse override map.
    assert(versionedComparisonDispatcher.dispatch(
        VersionedRegressionComparisonUpdate(
            versionedComparison.id, 8, versionedModel.id)) == "OK\tcmp_versioned");
    const auto &acceptedVersioned = versionedComparisonDispatcher.applicationState()
        .regressionComparisons().at(versionedComparison.id);
    assert(!acceptedVersioned.rFitPending);
    assert(acceptedVersioned.models.front().fit.ok);
    assert(acceptedVersioned.models.front().fit.n == 60);
    assert(acceptedVersioned.models.front().fitVersion == 3);
    assert(acceptedVersioned.termTypes.at("planet") == "factor");
    assert(acceptedVersioned.models.front().termTypeOverrides.empty());
    assert(acceptedVersioned.models.front().factorReferenceLevels.at("planet") == "B");
    assert(versionedComparisonUpdates == 1);

    // A response with the current number is still late when an edit made
    // while Auto-refit was disabled has cleared the pending request.
    auto editedVersioned = acceptedVersioned;
    editedVersioned.rFitGeneration = 9;
    editedVersioned.rFitPending = false;
    editedVersioned.models.front().fit.n = 17;
    versionedComparisonDispatcher.applicationState().regressionComparisons()[editedVersioned.id] =
        editedVersioned;
    assert(versionedComparisonDispatcher.dispatch(
        VersionedRegressionComparisonUpdate(
            editedVersioned.id, 9, versionedModel.id)) == "OK\tcmp_versioned");
    assert(versionedComparisonDispatcher.applicationState().regressionComparisons()
        .at(editedVersioned.id).models.front().fit.n == 17);
    assert(versionedComparisonUpdates == 1);

    // An older generation cannot replace the currently pending fit either.
    auto newerVersioned = editedVersioned;
    newerVersioned.rFitGeneration = 10;
    newerVersioned.rFitPending = true;
    versionedComparisonDispatcher.applicationState().regressionComparisons()[newerVersioned.id] =
        newerVersioned;
    assert(versionedComparisonDispatcher.dispatch(
        VersionedRegressionComparisonUpdate(
            newerVersioned.id, 9, versionedModel.id)) == "OK\tcmp_versioned");
    assert(versionedComparisonDispatcher.applicationState().regressionComparisons()
        .at(newerVersioned.id).rFitPending);
    assert(versionedComparisonUpdates == 1);

    // The precomputed/MI Linear Model Comparison reader uses the same
    // key/value protocol.  Exercise several predictors together so duplicate
    // values such as two `numeric` entries cannot overwrite each other.
    rlispstat::core::RegressionComparisonState pooledLinearRoundTrip;
    rlispstat::core::CommandDispatcherServices pooledLinearServices;
    pooledLinearServices.queries.groupSeed =
        [&](const std::string &group, rlispstat::core::PlotModel &seed) {
            if (group != "cars") return false;
            seed = modelSeed;
            return true;
        };
    pooledLinearServices.ui.regressionComparisonUpdated =
        [&](const rlispstat::core::RegressionComparisonState &state) {
            pooledLinearRoundTrip = state;
        };
    rlispstat::core::CommandDispatcher pooledLinearDispatcher(pooledLinearServices);
    const std::vector<std::string> pooledLinearCommand = {
        "REGCMP_OPEN_POOLED", "linear_mi_type_roundtrip", "cars", "distance",
        "all", "5", "Linear Model Comparison - Multiple Imputation", "Pooled",
        "4", "(Intercept)", "speed", "weight", "planet",
        "1", "linear_mi_type_roundtrip:model:1", "Model 1", "distance",
        "3", "speed", "weight", "planet",
        "3", "speed", "numeric", "weight", "numeric", "planet", "factor",
        "0",
        "TRUE", "400", "0", "4", "395", ".4", ".39", "10", ".001",
        "100", "200", "25", ".5", ".7", ".7", "NA", "NA", "",
        "0", "0", "0",
        "FALSE", "NA", "NA", "NA", "NA", "NA"
    };
    assert(pooledLinearDispatcher.dispatch(pooledLinearCommand) ==
           "OK\tlinear_mi_type_roundtrip");
    assert(pooledLinearRoundTrip.multipleImputation);
    assert(pooledLinearRoundTrip.imputationCount == 5);
    assert(pooledLinearRoundTrip.autoRefit);
    assert(pooledLinearRoundTrip.models.size() == 1);
    assert((pooledLinearRoundTrip.models.front().termTypeOverrides ==
           std::map<std::string, std::string>({
               {"speed", "numeric"}, {"weight", "numeric"}, {"planet", "factor"}
           })));
    // A pooled R reply carries only fitted terms. It must not erase a
    // per-column excluded candidate or reset Auto-refit while replacing fit
    // values, otherwise the visible plus disappears after every asynchronous
    // MI fit.
    auto &storedPooled = pooledLinearDispatcher.applicationState()
        .regressionComparisons().at("linear_mi_type_roundtrip");
    storedPooled.models.front().candidateTerms.push_back("held_out_candidate");
    storedPooled.autoRefit = false;
    rlispstat::core::RefreshRegressionTermRowsFromFits(storedPooled);
    assert(std::find(storedPooled.termRows.begin(), storedPooled.termRows.end(),
                     "held_out_candidate") != storedPooled.termRows.end());
    assert(pooledLinearDispatcher.dispatch(pooledLinearCommand) ==
           "OK\tlinear_mi_type_roundtrip");
    assert(!pooledLinearRoundTrip.autoRefit);
    assert(std::find(pooledLinearRoundTrip.termRows.begin(),
                     pooledLinearRoundTrip.termRows.end(),
                     "held_out_candidate") != pooledLinearRoundTrip.termRows.end());
    const auto pooledCandidateCell = rlispstat::core::RegressionComparisonTermCell(
        pooledLinearRoundTrip, 0, "held_out_candidate");
    assert(!pooledCandidateCell.termIncluded);
    assert(pooledCandidateCell.inclusionControlAvailable);

    std::vector<std::string> requestedRegressionComparisons;
    std::vector<std::string> shownRegressionComparisons;
    std::vector<std::string> refreshedRegressionComparisons;
    std::vector<std::string> shownLinearModels;
    std::vector<std::string> refreshedLinearModelGroups;
    std::vector<std::string> recordingCommands;
    rlispstat::core::CommandDispatcherServices regressionComparisonOpenServices;
    regressionComparisonOpenServices.queries.groupSeed =
        [&](const std::string &group, rlispstat::core::PlotModel &seed) {
            if (group != "cars") return false;
            seed = modelSeed;
            return true;
        };
    regressionComparisonOpenServices.queries.groupPlot =
        [&](const std::string &group, rlispstat::core::PlotModel &plot) {
            if (group != "cars") return false;
            plot = modelSeed;
            return true;
        };
    regressionComparisonOpenServices.queries.activePlot =
        [&](rlispstat::core::PlotModel &plot) {
            plot = modelSeed;
            return true;
        };
    regressionComparisonOpenServices.ui.requestRegressionComparisonFit =
        [&](rlispstat::core::RegressionComparisonState &state) {
            requestedRegressionComparisons.push_back(state.id);
            state.rFitPending = true;
        };
    regressionComparisonOpenServices.ui.showRegressionComparison =
        [&](const std::string &id) { shownRegressionComparisons.push_back(id); };
    regressionComparisonOpenServices.ui.regressionComparisonUpdated =
        [&](const rlispstat::core::RegressionComparisonState &state) {
            refreshedRegressionComparisons.push_back(state.id);
        };
    regressionComparisonOpenServices.ui.regressionComparisonVisible =
        [](const std::string &id, bool &visible) {
            visible = id == "cmp_open";
            return id == "cmp_open";
        };
    regressionComparisonOpenServices.ui.openRegressionComparisonDiagnostic =
        [](const std::string &id, int index, const std::string &type) {
            return id == "cmp_open" && index == 1 && type == "residuals" ? "cmp_diag" : "";
        };
    regressionComparisonOpenServices.ui.showLinearModel =
        [&](const std::string &group) { shownLinearModels.push_back(group); };
    regressionComparisonOpenServices.ui.refreshModelGroup =
        [&](const std::string &group) { refreshedLinearModelGroups.push_back(group); };
    regressionComparisonOpenServices.ui.openLinearModelDiagnostic =
        [](const std::string &group, const std::string &type) {
            return group == "cars" && type == "residuals_fitted" ? "linear_diag" : "";
        };
    regressionComparisonOpenServices.ui.openLinearInteractionPlot =
        [](const std::string &group, const std::string &term) {
            return group == "cars" && term == "speed:weight" ? "interaction_plot" : "";
        };
    regressionComparisonOpenServices.ui.dispatchRecordingCommand =
        [&](const std::string &command) { recordingCommands.push_back(command); };
    rlispstat::core::CommandDispatcher regressionComparisonOpenDispatcher(
        regressionComparisonOpenServices);
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"REGCMP_OPEN", "cmp_open", "cars", "distance", "all", "TRUE", "2",
         "Base", "1", "speed", "Full", "1", "speed:weight"}) == "OK\tcmp_open");
    const auto &openedComparison =
        regressionComparisonOpenDispatcher.applicationState().regressionComparisons().at("cmp_open");
    assert(openedComparison.rFitPending && openedComparison.models.size() == 2);
    assert((openedComparison.models[1].terms ==
            std::vector<std::string>{"speed", "weight", "speed:weight"}));
    assert((requestedRegressionComparisons == std::vector<std::string>{"cmp_open"}));
    assert((shownRegressionComparisons == std::vector<std::string>{"cmp_open"}));
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"REGCMP_OPEN2", "cmp_open2", "cars", "distance", "selected", "FALSE", "1",
         "Alternative", "speed", "1", "weight"}) == "OK\tcmp_open2");
    const auto &openedComparison2 =
        regressionComparisonOpenDispatcher.applicationState().regressionComparisons().at("cmp_open2");
    assert(openedComparison2.models.front().response == "speed");
    assert((openedComparison2.models.front().terms == std::vector<std::string>{"weight"}));
    assert(!openedComparison2.autoRefit);
    const std::size_t requestsBeforePendingTypeEdit = requestedRegressionComparisons.size();
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"REGCMP_SET_TYPE", "cmp_open2", "0", "weight", "factor"}) == "OK");
    const auto &pendingTypeComparison =
        regressionComparisonOpenDispatcher.applicationState().regressionComparisons().at("cmp_open2");
    assert(pendingTypeComparison.models.front().termTypeOverrides.at("weight") == "factor");
    assert(pendingTypeComparison.models.front().isStale);
    assert(requestedRegressionComparisons.size() == requestsBeforePendingTypeEdit);
    assert(regressionComparisonOpenDispatcher.dispatch({"REGCMP_INFO", "cmp_open"}).rfind(
               "OK\tCOMPARISON|cmp_open|cars|distance|all|2|4\tTERMS", 0) == 0);
    assert(regressionComparisonOpenDispatcher.dispatch({"REGCMP_CELL", "cmp_open", "1", "speed"}).rfind(
               "OK\t", 0) == 0);
    assert(regressionComparisonOpenDispatcher.dispatch({"REGCMP_FIT_CELL", "cmp_open", "2", "1"}).rfind(
               "OK\t", 0) == 0);
    assert(regressionComparisonOpenDispatcher.dispatch({"REGCMP_VISIBLE", "cmp_open"}) ==
           "OK\tREGISTERED|TRUE");
    assert(regressionComparisonOpenDispatcher.dispatch({"REGCMP_VISIBLE", "missing"}) ==
           "OK\tMISSING|FALSE");
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"REGCMP_OPEN_DIAGNOSTIC", "cmp_open", "2", "residuals"}) == "OK\tcmp_diag");
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"REGCMP_SET_SCOPE", "cmp_open", "unselected"}) ==
        "ERR Analysis scope is global. Use the central Analysis Scope menu.");
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"REGCMP_SET_RESPONSE", "cmp_open", "speed"}) == "OK");
    const auto &updatedComparison =
        regressionComparisonOpenDispatcher.applicationState().regressionComparisons().at("cmp_open");
    assert(updatedComparison.scope == "all" && updatedComparison.response == "speed");
    assert(updatedComparison.models.front().terms.empty());
    assert((requestedRegressionComparisons ==
            std::vector<std::string>{"cmp_open", "cmp_open2", "cmp_open"}));
    assert((refreshedRegressionComparisons ==
            std::vector<std::string>{"cmp_open2", "cmp_open"}));
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"REGCMP_ADD_TERM", "cmp_open", "weight"}) == "OK");
    assert((regressionComparisonOpenDispatcher.applicationState().regressionComparisons().at("cmp_open")
                .models.front().terms == std::vector<std::string>{"weight"}));
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"REGCMP_ADD_MODEL", "cmp_open", "Alternative", "speed", "1", "weight"}) ==
           "OK\tcmp_open:model:3");
    const auto &comparisonWithAddedModel =
        regressionComparisonOpenDispatcher.applicationState().regressionComparisons().at("cmp_open");
    assert(comparisonWithAddedModel.activeModel == 2 &&
           comparisonWithAddedModel.models.back().response == "speed");
    assert((comparisonWithAddedModel.models.back().terms == std::vector<std::string>{"weight"}));
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"REGCMP_SET_TYPE", "cmp_open", "2", "weight", "factor"}) == "OK");
    const auto &comparisonWithFactor =
        regressionComparisonOpenDispatcher.applicationState().regressionComparisons().at("cmp_open");
    assert(comparisonWithFactor.models[2].termTypeOverrides.at("weight") == "factor");
    assert(comparisonWithFactor.models[0].termTypeOverrides.count("weight") == 0);
    assert(rlispstat::core::RegressionComparisonModelTermType(
               comparisonWithFactor, 2, "weight") == "factor");
    assert(std::find(comparisonWithFactor.termRows.begin(), comparisonWithFactor.termRows.end(), "weight=3") !=
           comparisonWithFactor.termRows.end());
    const std::string independentLinearReply =
        regressionComparisonOpenDispatcher.dispatch({"GLM", "active"});
    assert(independentLinearReply.rfind(
        "OK\tglm:linear_model:cars:", 0) == 0);
    assert(regressionComparisonOpenDispatcher.dispatch({"RECORD_START"}) == "OK");
    assert((recordingCommands == std::vector<std::string>{"RECORD_START"}));
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"MODEL_OPEN_INTERACTION_PLOT", "cars", "speed:weight"}) == "OK\tinteraction_plot");
    assert(regressionComparisonOpenDispatcher.dispatch({"MODEL_OPEN", "cars"}) == "OK");
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"MODEL_OPEN_DIAGNOSTIC", "cars", "residuals_fitted"}) == "OK\tlinear_diag");
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"MODEL_OPEN_POOLED", "cars", "Pooled", "distance", "selected", "3", "pooled fit", "1", "speed",
         "TRUE", "10", "0", "1", "8", "0.5", "0.4", "8", "0.01", "20", "10", "20", "1.25",
         "1.1", "100", "101", "", "", "1", "1", "0", "0",
         "0", "0", "FACTOR_CODINGS", "0", "PREDICTOR_CENTERS", "0",
         "LINEAR_SCOPE_ROWS_V1", "2", "2", "5"}) == "OK\tglm:cars");
    assert(regressionComparisonOpenDispatcher.applicationState().linearModelFits().at("cars").n == 10);
    const auto &pooledLinearState =
        regressionComparisonOpenDispatcher.applicationState().groupModels().at("cars");
    assert(pooledLinearState.multipleImputation);
    assert(pooledLinearState.scope == "selected");
    const std::string pooledLinearBaseSignature = rlispstat::core::LinearGLMFitSignature(
        pooledLinearState.response, pooledLinearState.terms,
        rlispstat::core::EffectiveModelSpecificationTermTypes(pooledLinearState),
        pooledLinearState.scope, pooledLinearState.centeredPredictors,
        pooledLinearState.factorReferenceLevels);
    assert(pooledLinearState.lastRFitSignature ==
           rlispstat::core::LinearGLMFitIdentityWithSelection(
               pooledLinearBaseSignature, "selected", {2, 5}));
    assert(shownLinearModels.size() == 3);
    assert(shownLinearModels.front().rfind("linear_model:cars:", 0) == 0);
    assert(shownLinearModels[1] == "cars" && shownLinearModels[2] == "cars");
    assert((refreshedLinearModelGroups == std::vector<std::string>{"cars", "cars"}));
    assert(requestedRegressionComparisons.back() == "cmp_open");
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"REGCMP_SET_TERM", "cmp_open", "1", "distance", "TRUE"}) == "OK");
    assert((regressionComparisonOpenDispatcher.applicationState().regressionComparisons().at("cmp_open")
                .models.front().terms == std::vector<std::string>{"weight", "distance"}));
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"REGCMP_SET_TERM", "cmp_open", "1", "distance", "FALSE"}) == "OK");
    assert((regressionComparisonOpenDispatcher.applicationState().regressionComparisons().at("cmp_open")
                .models.front().terms == std::vector<std::string>{"weight"}));
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"REGCMP_OPEN", "cmp_bad", "cars", "distance", "bad", "TRUE", "0"}) ==
           "ERR invalid comparison scope");

    rlispstat::core::PlotModel *addedPlot = nullptr;
    std::string addedPlotDataSheet;
    std::string redrawnPlot;
    rlispstat::core::CommandDispatcherServices addPlotServices;
    addPlotServices.ui.addPlot = [&](rlispstat::core::PlotModel *plot) { addedPlot = plot; };
    addPlotServices.ui.openDataSheet = [&](const std::string &group) { addedPlotDataSheet = group; };
    addPlotServices.ui.redrawPlot = [&](const std::string &plotId) { redrawnPlot = plotId; };
    rlispstat::core::CommandDispatcher addPlotDispatcher(addPlotServices);
    assert(addPlotDispatcher.dispatch(
        {"ADD_PLOT", "plot_new", "new_group", "x", "y", "2", "1 2 1", "3 4 2",
         "VARS", "2", "x", "2", "1", "3", "y", "2", "2", "4", "VARMETA", "2",
         "x", "numeric", "y", "numeric"}) == "OK");
    assert(addedPlot && addedPlot->points.size() == 2 && addedPlotDataSheet == "new_group");
    const auto *scatterCode =
        addPlotDispatcher.applicationState().outputCodeReference("plot_new");
    assert(scatterCode);
    assert(scatterCode->provenance.verificationRCode.at("plot").find(
               "ggplot2::geom_point") != std::string::npos);
    assert(scatterCode->provenance.verificationRCode.at("plot").find(
               "linkeda_plot_theme <- ggplot2::theme_classic(base_size = 11)") !=
           std::string::npos);
    assert(scatterCode->publication.plot->theme == "publication");
    assert(scatterCode->publication.plot.has_value());
    assert(scatterCode->publication.plot->kind == "scatter");
    assert(scatterCode->publication.availableBackends ==
           std::vector<rlispstat::core::PublicationBackend>{
               rlispstat::core::PublicationBackend::Ggplot2});
    assert(rlispstat::core::BuildPublicationRCode(
               *scatterCode, rlispstat::core::PublicationBackend::Ggplot2)
               .find("ggplot2::geom_point") != std::string::npos);
    assert((scatterCode->provenance.verificationVariables ==
            std::vector<std::string>{"x", "y"}));
    addedPlot->overlays.push_back({1, "lm", "all", true});
    addedPlot->smoothCurves.push_back(
        rlispstat::core::PendingSmoothCurve(
            rlispstat::core::SmoothCurveScope::Overall, "lm"));
    addedPlot->scatterFitConfidenceIntervalsVisible = true;
    rlispstat::core::RefreshBasicPlotCodeReference(
        addPlotDispatcher.applicationState(), *addedPlot);
    scatterCode = addPlotDispatcher.applicationState().outputCodeReference("plot_new");
    assert(scatterCode->provenance.verificationRCode.at("plot").find(
               "method = \"lm\", formula = y ~ x, se = TRUE") !=
           std::string::npos);
    assert(scatterCode->provenance.verificationRCode.at("plot").find(
               "level = 0.94999999999999996") != std::string::npos);
    rlispstat::core::SmoothCurveData exportedLoess =
        rlispstat::core::EnabledEmptySmoothCurve(
            rlispstat::core::SmoothCurveScope::Overall, "loess");
    exportedLoess.ok = true;
    addedPlot->smoothCurves.push_back(exportedLoess);
    rlispstat::core::RefreshBasicPlotCodeReference(
        addPlotDispatcher.applicationState(), *addedPlot);
    scatterCode = addPlotDispatcher.applicationState().outputCodeReference("plot_new");
    assert(scatterCode->provenance.verificationRCode.at("plot").find(
               "method = \"loess\", formula = y ~ x, se = FALSE") !=
           std::string::npos);
    addedPlot->scatterSmoothConfidenceIntervalsVisible = true;
    rlispstat::core::RefreshBasicPlotCodeReference(
        addPlotDispatcher.applicationState(), *addedPlot);
    scatterCode = addPlotDispatcher.applicationState().outputCodeReference("plot_new");
    assert(scatterCode->provenance.verificationRCode.at("plot").find(
               "method = \"loess\", formula = y ~ x, se = TRUE") !=
           std::string::npos);
    assert(addPlotDispatcher.dispatch({"PLOT_THEME", "minimal"}) ==
           "OK\tminimal");
    const auto *retintedScatterCode =
        addPlotDispatcher.applicationState().outputCodeReference("plot_new");
    assert(retintedScatterCode && retintedScatterCode->publication.plot);
    assert(retintedScatterCode->publication.plot->theme == "minimal");
    assert(retintedScatterCode->provenance.verificationRCode.at("plot").find(
               "linkeda_plot_theme <- ggplot2::theme_minimal()") !=
           std::string::npos);
    rlispstat::core::DataFrameModel trellisVerificationData =
        *addPlotDispatcher.applicationState().datasets().find("new_group");
    trellisVerificationData.group = "trellis_group";
    trellisVerificationData.columns.push_back(
        {"panel", "factor", "", "", -1, {"A", "B"}});
    assert(addPlotDispatcher.applicationState().registerDataset(trellisVerificationData));
    rlispstat::core::PlotModel trellisVerificationPlot;
    trellisVerificationPlot.id = "trellis_new";
    trellisVerificationPlot.group = "trellis_group";
    trellisVerificationPlot.kind = "trellis_scatterplot";
    trellisVerificationPlot.title = "Trellis";
    trellisVerificationPlot.xLabel = "x";
    trellisVerificationPlot.yLabel = "y";
    trellisVerificationPlot.trellisSpecificationInitialized = true;
    trellisVerificationPlot.trellisSpecification.plotType =
        rlispstat::core::TrellisPlotType::Scatter;
    trellisVerificationPlot.trellisSpecification.xVariableId = "x";
    trellisVerificationPlot.trellisSpecification.yVariableId = "y";
    trellisVerificationPlot.trellisSpecification.conditioningVariables.push_back(
        {"panel", "panel", rlispstat::core::TrellisConditioningVariableKind::Categorical,
         rlispstat::core::TrellisDimension::Nested, std::nullopt, false});
    trellisVerificationPlot.overlays.push_back({1, "lm", "all", true});
    trellisVerificationPlot.smoothCurves.push_back(
        rlispstat::core::PendingSmoothCurve(
            rlispstat::core::SmoothCurveScope::Overall, "lm"));
    trellisVerificationPlot.scatterFitConfidenceIntervalsVisible = true;
    rlispstat::core::RefreshBasicPlotCodeReference(
        addPlotDispatcher.applicationState(), trellisVerificationPlot);
    const auto *trellisCode =
        addPlotDispatcher.applicationState().outputCodeReference("trellis_new");
    assert(trellisCode);
    assert(trellisCode->provenance.verificationRCode.at("plot").find(
        "ggplot2::facet_wrap") != std::string::npos);
    assert(trellisCode->provenance.verificationRCode.at("plot").find(
        "ggplot2::geom_point") != std::string::npos);
    assert(trellisCode->provenance.verificationRCode.at("plot").find(
        "method = \"lm\", formula = y ~ x, se = TRUE") != std::string::npos);
    assert((trellisCode->provenance.verificationVariables ==
            std::vector<std::string>{"panel", "x", "y"}));
    rlispstat::core::PlotModel matrixVerificationPlot;
    matrixVerificationPlot.id = "matrix_new";
    matrixVerificationPlot.group = "new_group";
    matrixVerificationPlot.kind = "scatter_matrix";
    matrixVerificationPlot.title = "Scatterplot matrix";
    matrixVerificationPlot.variables = {{"x", {1.0, 2.0}}, {"y", {2.0, 4.0}}};
    matrixVerificationPlot.scatterMatrixVariables = {"x", "y"};
    matrixVerificationPlot.overlays.push_back({1, "lm", "all", true});
    rlispstat::core::RefreshBasicPlotCodeReference(
        addPlotDispatcher.applicationState(), matrixVerificationPlot);
    const auto *matrixCode =
        addPlotDispatcher.applicationState().outputCodeReference("matrix_new");
    assert(matrixCode);
    assert((matrixCode->provenance.verificationVariables ==
            std::vector<std::string>{"x", "y"}));
    assert(matrixCode->provenance.verificationRCode.at("plot").find(
        "ggplot2::facet_grid") != std::string::npos);
    assert(matrixCode->provenance.verificationRCode.at("plot").find(
        "method = \"lm\", formula = y ~ x") != std::string::npos);
    rlispstat::core::PlotModel *firstAddedPlot = addedPlot;
    rlispstat::core::DataFrameModel imputedPlotData =
        *addPlotDispatcher.applicationState().datasets().find("new_group");
    imputedPlotData.group = "mi_plot_group";
    imputedPlotData.datasetType = "multiple_imputation";
    imputedPlotData.imputationCount = 5;
    assert(addPlotDispatcher.applicationState().registerDataset(imputedPlotData));
    assert(addPlotDispatcher.dispatch(
        {"ADD_PLOT", "mi_plot", "mi_plot_group", "x", "y", "2",
         "1 2 1", "3 4 2", "VARS", "2", "x", "2", "1", "3",
         "y", "2", "2", "4"}) == "OK");
    const auto *miPlotCode =
        addPlotDispatcher.applicationState().outputCodeReference("mi_plot");
    assert(miPlotCode && miPlotCode->provenance.verificationRCode.at("plot").find(
        "mi_long$.imp == displayed_imputation") != std::string::npos);
    assert(miPlotCode->provenance.verificationRCode.at("plot").find(
        "displayed_imputation <- 1L") != std::string::npos);
    assert(!miPlotCode->provenance.verificationWarnings.empty());
    assert(addPlotDispatcher.dispatch(
        {"ADD_PLOT", "mi_diagnostic_plot", "mi_diagnostic_plot:unlinked",
         "Iteration", "Value", "Chain means", "2", "1 2 1 0", "2 3 2 0",
         "TIME_SERIES", "numeric", "Imputation", "1", "1",
         "IMPUTATION_PROCESS_DIAGNOSTIC", "chain_mean"}) == "OK");
    assert(addedPlot && rlispstat::core::PlotIsImputationDiagnostic(*addedPlot));
    assert(addPlotDispatcher.dispatch(
        {"SET_DIAGNOSTIC_PLOT_PROVENANCE", "mi_diagnostic_plot",
         "ANALYSIS_PROVENANCE_V2", "mi_diagnostics:new_group", "Chain means",
         "new_group", "1", "data.frame", "0", "recorded", "78", "0",
         "0", "0", "0"}) == "OK");
    const auto *diagnosticPlot =
        addPlotDispatcher.applicationState().plots().at("mi_diagnostic_plot");
    assert(diagnosticPlot->group == "mi_diagnostic_plot:unlinked");
    assert(diagnosticPlot->dataScopeCaptured);
    assert(diagnosticPlot->dataScope.datasetId == "new_group");
    assert(diagnosticPlot->dataScope.totalDatasetRows == 2);
    assert(diagnosticPlot->codeReference.provenance.scope.sourceN == 2);
    assert(redrawnPlot == "mi_diagnostic_plot");
    assert(addPlotDispatcher.dispatch(
        {"ADD_PLOT", "time_new", "new_group", "year", "sales", "Sales over year", "4",
         "2022 4 1 0", "2021 2 2 0", "2022 8 3 1", "2021 6 4 1",
         "TIME_SERIES", "numeric", "store", "2", "North", "South"}) == "OK");
    assert(addedPlot && addedPlot->kind == "time_series");
    assert(addedPlot->interactionPlotLines.size() == 2);
    assert(addedPlot->interactionPlotLines[0].label == "North");
    assert(addedPlot->interactionPlotLines[0].points[0].row == 2);
    assert(addedPlot->timeSeriesGroupVariable == "store");
    assert(addPlotDispatcher.applicationState().plots().at("plot_new") == firstAddedPlot);
    assert(addPlotDispatcher.applicationState().groupModels().at("new_group").group == "new_group");
    assert(addPlotDispatcher.dispatch(
        {"ADD_PLOT", "plot_frame", "frame_group", "x", "y", "0", "DATAFRAME", "1", "1",
         "value", "numeric", "12"}) == "OK");
    assert(addPlotDispatcher.applicationState().datasets().find("frame_group")->rows == 1);
    assert(addPlotDispatcher.dispatch(
        {"ADD_PLOT", "bad_plot", "new_group", "x", "y", "0", "VARS", "-1"}) ==
           "ERR invalid variable count");
    rlispstat::core::DataFrameModel boxVerificationData;
    boxVerificationData.group = "box_group";
    boxVerificationData.rows = 2;
    boxVerificationData.columns = {
        {"x", "factor", "", "", -1, {"A", "B"}},
        {"y", "numeric", "", "", -1, {"2", "4"}}
    };
    assert(addPlotDispatcher.applicationState().registerDataset(boxVerificationData));
    assert(addPlotDispatcher.dispatch(
        {"ADD_BOXPLOT", "box_new", "box_group", "x", "y", "Box", "TRUE", "TRUE", "TRUE", "2",
         "2\tA\t1", "4\tB\t2"}) == "OK");
    assert(addedPlot && addedPlot->kind == "boxplot" && addedPlot->boxplotPoints.size() == 2);
    assert((addedPlot->boxplotCategories == std::vector<std::string>{"A", "B"}));
    const auto *boxCode =
        addPlotDispatcher.applicationState().outputCodeReference("box_new");
    assert(boxCode && boxCode->provenance.verificationRCode.at("plot").find(
        "grDevices::boxplot.stats") != std::string::npos);
    assert(boxCode->publication.plot && boxCode->publication.plot->kind == "boxplot");
    assert(rlispstat::core::BuildPublicationRCode(
               *boxCode, rlispstat::core::PublicationBackend::Ggplot2)
               .find("ggplot2::geom_boxplot") != std::string::npos);
    rlispstat::core::DataFrameModel histogramVerificationData;
    histogramVerificationData.group = "hist_group";
    histogramVerificationData.rows = 2;
    histogramVerificationData.columns = {
        {"x", "numeric", "", "", -1, {"1", "3"}}
    };
    assert(addPlotDispatcher.applicationState().registerDataset(histogramVerificationData));
    assert(addPlotDispatcher.dispatch(
        {"ADD_HISTOGRAM", "hist_new", "hist_group", "x", "Histogram", "TRUE", "FALSE", "2", "2",
         "0\t2", "2\t4", "1\t1\t1", "3\t2\t2"}) == "OK");
    assert(addedPlot && addedPlot->kind == "histogram" && addedPlot->histogramBins.size() == 2);
    assert((addedPlot->histogramBins[0].rows == std::vector<int>{1}));
    assert(addedPlot->histogramShowTickMarks);
    assert(addedPlot->histogramShowTickLabels);
    addedPlot->histogramShowDensity = true;
    addedPlot->histogramDensityMode = "all";
    addedPlot->histogramShowRug = true;
    addedPlot->histogramShowTickMarks = false;
    addedPlot->histogramShowTickLabels = false;
    rlispstat::core::RefreshBasicPlotCodeReference(
        addPlotDispatcher.applicationState(), *addedPlot);
    const auto *histogramCode =
        addPlotDispatcher.applicationState().outputCodeReference("hist_new");
    assert(histogramCode && histogramCode->provenance.verificationRCode.at("plot").find(
        "cut(x, breaks = histogram_breaks") != std::string::npos);
    assert(histogramCode->provenance.verificationRCode.at("plot").find(
        "geom_histogram") == std::string::npos);
    assert(histogramCode->provenance.verificationRCode.at("plot").find(
        "geom_density") != std::string::npos);
    assert(histogramCode->provenance.verificationRCode.at("plot").find(
        "geom_rug") != std::string::npos);
    assert(histogramCode->provenance.verificationRCode.at("plot").find(
        "axis.ticks = ggplot2::element_blank()") != std::string::npos);
    assert(histogramCode->provenance.verificationRCode.at("plot").find(
        "axis.text = ggplot2::element_blank()") != std::string::npos);
    assert(histogramCode->publication.plot &&
           histogramCode->publication.plot->kind == "histogram_density");
    assert(!histogramCode->publication.plot->showAxisTickMarks);
    assert(!histogramCode->publication.plot->showAxisTickLabels);
    assert(rlispstat::core::BuildPublicationRCode(
               *histogramCode, rlispstat::core::PublicationBackend::Ggplot2)
               .find("ggplot2::geom_rect") != std::string::npos);
    rlispstat::core::DataFrameModel barVerificationData;
    barVerificationData.group = "bar_group";
    barVerificationData.rows = 2;
    barVerificationData.columns = {
        {"cat", "factor", "", "", -1, {"A", "A"}}
    };
    assert(addPlotDispatcher.applicationState().registerDataset(barVerificationData));
    assert(addPlotDispatcher.dispatch(
        {"ADD_BARPLOT", "bar_new", "bar_group", "cat", "", "Bar", "overall_percent", "equal", "", "2", "0",
         "1", "A\t2\t100\t1\t1,2", "0"}) == "OK");
    assert(addedPlot && addedPlot->kind == "barplot" && addedPlot->barplotBins.size() == 1);
    assert(addedPlot->barplotBins.front().segments.front().level == "All");
    const auto *barCode =
        addPlotDispatcher.applicationState().outputCodeReference("bar_new");
    assert(barCode && barCode->provenance.verificationRCode.at("plot").find(
        "as.data.frame(table(") != std::string::npos);
    assert(barCode->provenance.verificationRCode.at("plot").find(
        "100 * reference_counts$count / sum(reference_counts$count)") !=
        std::string::npos);
    assert(barCode->publication.plot && barCode->publication.plot->kind == "barplot");
    assert(rlispstat::core::BuildPublicationRCode(
               *barCode, rlispstat::core::PublicationBackend::Ggplot2)
               .find("ggplot2::geom_col") != std::string::npos);

    rlispstat::core::Table1DisplayState compareMeansTable;
    rlispstat::core::CommandDispatcherServices compareMeansReportServices;
    compareMeansReportServices.ui.showCompareMeansTable =
        [&](const rlispstat::core::Table1DisplayState &state) { compareMeansTable = state; };
    rlispstat::core::CommandDispatcher compareMeansReportDispatcher(compareMeansReportServices);
    assert(compareMeansReportDispatcher.dispatch(
        {"COMPARE_MEANS_OPEN", "cm_result", "cars", "one_sample_t_test", "One-Sample t Test", "",
         "2.0", "18", "", "0.05", "1.2", "0.4", "One-sample t-test", "",
         "COMPARE_MEANS_V2", "0.4", "2.0",
         "2", "Response:", "mpg",
         "8", "N", "32", "Mean", "20.090625", "SD", "6.026948", "SE", "1.065198",
         "1", "Cohen's d", "0.5", "0",
         "2", "1", "2", "1", "3", "", "1"}) == "OK\tcm_result");
    assert(compareMeansTable.tableType == "compare_means");
    assert(compareMeansTable.title == "One-Sample t Test");
    assert(compareMeansTable.subtitle == "Response: mpg");
    assert(compareMeansTable.columns.size() == 12);
    assert(compareMeansTable.rows.size() == 2);
    assert(compareMeansTable.rows[0].values[1] == "20.091");
    assert(compareMeansTable.rows[0].rawValues[1] == "20.090625");
    assert(compareMeansTable.rows[1].values[4] == "t = 2");
    assert(compareMeansTable.rows[1].values[6] == ".050");
    assert(compareMeansTable.rows[1].values[9] == "0.4");
    assert(compareMeansTable.rows[1].values[10] == "2");
    assert(compareMeansTable.statusText == "Calculated in R on 2 rows; 1 excluded.");

    rlispstat::core::MeanComparisonState meanComparison;
    rlispstat::core::CommandDispatcherServices meanComparisonServices;
    meanComparisonServices.ui.showMeanComparison =
        [&](const rlispstat::core::MeanComparisonState &state) { meanComparison = state; };
    rlispstat::core::CommandDispatcher meanComparisonDispatcher(meanComparisonServices);
    assert(meanComparisonDispatcher.dispatch({
        "COMPARE_MEANS_BATCH_OPEN", "COMPARE_MEANS_BATCH_V1", "cm_batch", "cars",
        "one_sample_t_test", "One-Sample t Test", "", "One-sample t-test", "two.sided",
        "0.95", "0", "", "", "1", "mpg", "", "One-sample t-test", "Cohen's d",
        "32", "20.090625", "6.026948", "1.065424", "NA", "NA", "NA", "NA",
        "20.090625", "NA", "1.065424", "17.917679", "22.263571", "18.85693", "31", "NA",
        "1.526e-18", "3.333", "0", "2", "1", "2", "1", "3", "0"
    }) == "OK\tcm_batch");
    assert(meanComparison.id == "cm_batch");
    assert(meanComparison.tables.size() == 2);
    assert(meanComparison.tables[1].tableId == "group_descriptives");
    assert(meanComparison.tables[0].rows.size() == 1);
    assert(meanComparison.tables[0].rows[0].cells[6].numericUpperValue.has_value());
    assert(meanComparison.tables[0].rows[0].originalRowIndices == std::vector<int>({1, 2}));

    std::vector<std::string> refreshedGeneralizedGLMs;
    rlispstat::core::CommandDispatcherServices generalizedServices;
    generalizedServices.queries.groupSeed = [&](const std::string &group, rlispstat::core::PlotModel &seed) {
        if (group != "cars") return false;
        seed = modelSeed;
        return true;
    };
    generalizedServices.ui.refreshGeneralizedGLM =
        [&](const std::string &id) { refreshedGeneralizedGLMs.push_back(id); };
    rlispstat::core::CommandDispatcher generalizedDispatcher(generalizedServices);
    assert(generalizedDispatcher.dispatch(
        {"GENERALIZED_GLM_OPEN_STRUCTURED", "glm_generalized", "cars", "Logistic", "outcome",
         "binomial", "logit", "all", "fit complete", "0", "0", "TRUE", "10", "0", "1", "2",
         "8", "12", "13", "1", "4", "z", "fitted", "1", "1", "0", "0"}) ==
           "OK\tglm_generalized");
    const auto &generalized =
        generalizedDispatcher.applicationState().generalizedGLMs().at("glm_generalized");
    assert(generalized.hasSeed && generalized.ok && generalized.n == 10);
    assert(generalized.rowsUsed == std::vector<int>({1}));
    assert((refreshedGeneralizedGLMs == std::vector<std::string>{"glm_generalized"}));

    // A generalized-comparison backend failure belongs to one exact request
    // generation. Stale failures are ignored, while the current failure must
    // terminate pending and expose a visible per-model error instead of
    // leaving the whole comparison column indefinitely blank.
    std::vector<std::string> shownGeneralizedComparisons;
    rlispstat::core::CommandDispatcherServices generalizedComparisonErrorServices;
    generalizedComparisonErrorServices.ui.showGeneralizedComparison =
        [&](const std::string &id) { shownGeneralizedComparisons.push_back(id); };
    rlispstat::core::CommandDispatcher generalizedComparisonErrorDispatcher(
        generalizedComparisonErrorServices);
    rlispstat::core::GeneralizedComparisonState pendingGeneralizedComparison;
    pendingGeneralizedComparison.id = "binary_mi_comparison_error";
    pendingGeneralizedComparison.group = "mi_test_mids";
    pendingGeneralizedComparison.multipleImputation = true;
    pendingGeneralizedComparison.imputationCount = 5;
    pendingGeneralizedComparison.rFitGeneration = 12;
    pendingGeneralizedComparison.rFitPending = true;
    pendingGeneralizedComparison.autoRefit = true;
    rlispstat::core::GeneralizedComparisonModel pendingGeneralizedModel;
    pendingGeneralizedModel.id = "binary_mi_comparison_error:model:1";
    pendingGeneralizedModel.label = "Model 1";
    pendingGeneralizedModel.isStale = true;
    pendingGeneralizedModel.fitState =
        rlispstat::core::RegressionComparisonFitState::Pending;
    pendingGeneralizedComparison.models.push_back(pendingGeneralizedModel);
    generalizedComparisonErrorDispatcher.applicationState().generalizedComparisons()[
        pendingGeneralizedComparison.id] = pendingGeneralizedComparison;
    assert(generalizedComparisonErrorDispatcher.dispatch({
        "GENERALIZED_COMPARISON_UPDATE_ERROR", pendingGeneralizedComparison.id,
        "11", "obsolete failure"
    }) == "OK\tbinary_mi_comparison_error");
    assert(generalizedComparisonErrorDispatcher.applicationState()
        .generalizedComparisons().at(pendingGeneralizedComparison.id).rFitPending);
    assert(shownGeneralizedComparisons.empty());
    assert(generalizedComparisonErrorDispatcher.dispatch({
        "GENERALIZED_COMPARISON_UPDATE_ERROR", pendingGeneralizedComparison.id,
        "12", "deliberate MI comparison failure"
    }) == "OK\tbinary_mi_comparison_error");
    const auto &failedGeneralizedComparison = generalizedComparisonErrorDispatcher
        .applicationState().generalizedComparisons().at(pendingGeneralizedComparison.id);
    assert(!failedGeneralizedComparison.rFitPending);
    assert(failedGeneralizedComparison.autoRefit);
    assert(failedGeneralizedComparison.models.front().isStale);
    assert(!failedGeneralizedComparison.models.front().fit.ok);
    assert(failedGeneralizedComparison.models.front().fit.status ==
           "deliberate MI comparison failure");
    assert(failedGeneralizedComparison.models.front().fitState ==
           rlispstat::core::RegressionComparisonFitState::Error);
    assert(shownGeneralizedComparisons ==
           std::vector<std::string>({"binary_mi_comparison_error"}));

    // Multiple key/value pairs on one wire message must be decoded in their
    // transmitted order.  Expressions such as map[args[cursor++]] =
    // args[cursor++] are not safe here: assignment evaluates the right side
    // first and used to turn x=numeric,z=numeric,g=factor into the corrupt
    // map numeric=z,factor=g.  The resulting fit was correct in R but native
    // rejected its immutable fingerprint and left the comparison blank.
    int acceptedBinaryMIComparisons = 0;
    rlispstat::core::CommandDispatcherServices binaryMITypeServices;
    binaryMITypeServices.ui.showGeneralizedComparison =
        [&](const std::string &id) {
            assert(id == "binary_mi_type_roundtrip");
            ++acceptedBinaryMIComparisons;
        };
    rlispstat::core::CommandDispatcher binaryMITypeDispatcher(binaryMITypeServices);
    rlispstat::core::DataFrameModel binaryMIData;
    binaryMIData.group = "mi_type_roundtrip";
    binaryMIData.rows = 400;
    binaryMIData.datasetType = "multiple_imputation";
    binaryMIData.imputationId = "mi-type-roundtrip";
    binaryMIData.sourceDatasetId = "mi-type-source";
    binaryMIData.imputationCount = 5;
    binaryMIData.columns = {
        {"y_bin", "numeric"}, {"x", "numeric"}, {"z", "numeric"},
        {"g", "factor", "", "", -1, {}, {}, {"A", "B", "C"}}
    };
    binaryMITypeDispatcher.applicationState().registerDataset(binaryMIData);
    rlispstat::core::GeneralizedComparisonState binaryMIComparison;
    binaryMIComparison.id = "binary_mi_type_roundtrip";
    binaryMIComparison.group = binaryMIData.group;
    binaryMIComparison.response = "y_bin";
    binaryMIComparison.family = "binomial";
    binaryMIComparison.link = "logit";
    binaryMIComparison.scope = "all";
    binaryMIComparison.binaryComparison = true;
    binaryMIComparison.multipleImputation = true;
    binaryMIComparison.imputationCount = 5;
    binaryMIComparison.datasetType = binaryMIData.datasetType;
    binaryMIComparison.imputationSetId = binaryMIData.imputationId;
    binaryMIComparison.sourceDatasetId = binaryMIData.sourceDatasetId;
    binaryMIComparison.rFitGeneration = 73;
    binaryMIComparison.rFitPending = true;
    binaryMIComparison.autoRefit = true;
    binaryMIComparison.responseCoding.ok = true;
    binaryMIComparison.responseCoding.eventValue = "1";
    binaryMIComparison.responseCoding.eventLabel = "1";
    binaryMIComparison.responseCoding.referenceValue = "0";
    binaryMIComparison.responseCoding.referenceLabel = "0";
    binaryMIComparison.termTypes = {
        {"x", "numeric"}, {"z", "numeric"}, {"g", "factor"}
    };
    rlispstat::core::GeneralizedComparisonModel binaryMIModel;
    binaryMIModel.id = "binary_mi_type_roundtrip:model:1";
    binaryMIModel.label = "Model 1";
    binaryMIModel.response = "y_bin";
    binaryMIModel.family = "binomial";
    binaryMIModel.link = "logit";
    binaryMIModel.scope = "all";
    binaryMIModel.familyKind = rlispstat::core::ModelFamilyKind::GeneralizedLinear;
    binaryMIModel.terms = {"x", "z", "g"};
    binaryMIModel.candidateTerms = {"x", "z", "g", "held_out_candidate"};
    binaryMIModel.termTypes = binaryMIComparison.termTypes;
    binaryMIModel.termTypeOverrides = binaryMIComparison.termTypes;
    binaryMIModel.factorReferenceLevels = {{"g", "A"}};
    binaryMIModel.modelVersion = 29;
    binaryMIModel.requestedSpecificationRevision = 29;
    binaryMIModel.fitState = rlispstat::core::RegressionComparisonFitState::Pending;
    binaryMIComparison.models.push_back(binaryMIModel);
    const std::string binaryMIFingerprint =
        rlispstat::core::GeneralizedComparisonModelSpecificationFingerprint(
            binaryMIComparison, binaryMIComparison.models.front());
    binaryMIComparison.models.front().requestedSpecificationFingerprint =
        binaryMIFingerprint;
    binaryMITypeDispatcher.applicationState().generalizedComparisons()[
        binaryMIComparison.id] = binaryMIComparison;
    assert(binaryMITypeDispatcher.dispatch(
        VersionedBinaryMIComparisonUpdate(binaryMIFingerprint)) ==
        "OK\tbinary_mi_type_roundtrip");
    const auto &acceptedBinaryMI = binaryMITypeDispatcher.applicationState()
        .generalizedComparisons().at(binaryMIComparison.id);
    assert(acceptedBinaryMIComparisons == 1);
    assert(!acceptedBinaryMI.rFitPending && acceptedBinaryMI.autoRefit);
    assert(acceptedBinaryMI.models.front().fitState ==
           rlispstat::core::RegressionComparisonFitState::Valid);
    assert(acceptedBinaryMI.models.front().fit.parameterCount == 5);
    assert(acceptedBinaryMI.models.front().fit.dfResidual == 395);
    assert(acceptedBinaryMI.models.front().termTypeOverrides ==
           binaryMIComparison.termTypes);
    assert(acceptedBinaryMI.models.front().factorReferenceLevels.at("g") == "A");
    assert(acceptedBinaryMI.models.front().fit.rows.size() == 7);
    assert(std::find(acceptedBinaryMI.termRows.begin(), acceptedBinaryMI.termRows.end(),
                     "held_out_candidate") != acceptedBinaryMI.termRows.end());
    const auto acceptedBinaryCandidate =
        rlispstat::core::GeneralizedComparisonTermCell(
            acceptedBinaryMI, 0, "held_out_candidate");
    assert(!acceptedBinaryCandidate.termIncluded);
    assert(acceptedBinaryCandidate.inclusionControlAvailable);

    // Initial response coding may be accepted from R, but that exception must
    // never let an in-flight result for an older term list replace a newer
    // Binary Regression specification.
    rlispstat::core::GeneralizedGLMState binaryWithNewTerm = generalized;
    binaryWithNewTerm.id = "glm_binary_async";
    binaryWithNewTerm.binaryRegression = true;
    binaryWithNewTerm.binaryLink = rlispstat::core::BinaryLink::Logit;
    binaryWithNewTerm.responseCoding.ok = true;
    binaryWithNewTerm.responseCoding.eventValue = "1";
    binaryWithNewTerm.responseCoding.eventLabel = "1";
    binaryWithNewTerm.responseCoding.referenceValue = "0";
    binaryWithNewTerm.responseCoding.referenceLabel = "0";
    binaryWithNewTerm.terms = {"speed", "distance"};
    binaryWithNewTerm.termTypes = {{"speed", "numeric"}, {"distance", "numeric"}};
    binaryWithNewTerm.termTypeOverrides = binaryWithNewTerm.termTypes;
    binaryWithNewTerm.responseCodingExplicit = false;
    binaryWithNewTerm.rFitPending = true;
    binaryWithNewTerm.lastRFitSignature =
        rlispstat::core::GeneralizedGLMFitSignature(binaryWithNewTerm);
    generalizedDispatcher.applicationState().generalizedGLMs()[binaryWithNewTerm.id] =
        binaryWithNewTerm;
    const auto refreshCountBeforeOlderBinaryResult = refreshedGeneralizedGLMs.size();
    assert(generalizedDispatcher.dispatch(
        {"GENERALIZED_GLM_OPEN_STRUCTURED", "glm_binary_async", "cars", "Logistic", "outcome",
         "binomial", "logit", "all", "older binary fit", "1", "speed", "1", "speed", "numeric",
         "TRUE", "10", "0", "1", "2", "8", "12", "13", "1", "4", "z", "fitted",
         "1", "1", "0", "0",
         "BINARY_V1", "1", "0", "5", "5", "1", "2", ".1", ".2", ".3", ".4",
         ".8", "0", "1", "TRUE", "0", "0", "0", "0"}) == "OK\tglm_binary_async");
    const auto &binaryAfterOlderResult =
        generalizedDispatcher.applicationState().generalizedGLMs().at("glm_binary_async");
    assert((binaryAfterOlderResult.terms == std::vector<std::string>{"speed", "distance"}));
    assert(refreshedGeneralizedGLMs.size() == refreshCountBeforeOlderBinaryResult);

    // A fit result for an older specification must not repopulate a model
    // after the user has removed its terms.  Emptying the specification also
    // clears the pending-fit marker, so signature validation cannot depend on
    // rFitPending alone.
    auto &currentGeneralized =
        generalizedDispatcher.applicationState().generalizedGLMs().at("glm_generalized");
    currentGeneralized.terms.clear();
    currentGeneralized.termTypes.clear();
    currentGeneralized.termTypeOverrides.clear();
    currentGeneralized.rFitPending = false;
    currentGeneralized.lastRFitSignature.clear();
    const auto refreshCountBeforeStaleResult = refreshedGeneralizedGLMs.size();
    assert(generalizedDispatcher.dispatch(
        {"GENERALIZED_GLM_OPEN_STRUCTURED", "glm_generalized", "cars", "Logistic", "outcome",
         "binomial", "logit", "all", "stale fit", "1", "speed", "1", "speed", "numeric",
         "TRUE", "10", "0", "1", "2", "8", "12", "13", "1", "4", "z", "fitted",
         "1", "1", "0", "0"}) == "OK\tglm_generalized");
    const auto &generalizedAfterStaleResult =
        generalizedDispatcher.applicationState().generalizedGLMs().at("glm_generalized");
    assert(generalizedAfterStaleResult.terms.empty());
    assert(refreshedGeneralizedGLMs.size() == refreshCountBeforeStaleResult);

    // Generation is the primary identity: even a result whose semantic
    // specification is indistinguishable from the current model must be
    // rejected when it belongs to an older request (the A-B-A case).
    rlispstat::core::GeneralizedGLMState versionedGeneralized = generalized;
    versionedGeneralized.id = "glm_generalized_generation";
    versionedGeneralized.terms = {"speed"};
    versionedGeneralized.termTypes = {{"speed", "numeric"}};
    versionedGeneralized.termTypeOverrides = versionedGeneralized.termTypes;
    versionedGeneralized.rFitPending = true;
    versionedGeneralized.rFitGeneration = 2;
    versionedGeneralized.lastRFitSignature =
        rlispstat::core::GeneralizedGLMFitSignature(versionedGeneralized);
    generalizedDispatcher.applicationState().generalizedGLMs()[versionedGeneralized.id] =
        versionedGeneralized;
    const auto refreshCountBeforeOldGeneration = refreshedGeneralizedGLMs.size();
    std::vector<std::string> versionedGeneralizedResult = {
        "GENERALIZED_GLM_OPEN_STRUCTURED", "glm_generalized_generation", "cars", "Logistic",
        "outcome", "binomial", "logit", "all", "fit complete", "1", "speed", "1",
        "speed", "numeric", "TRUE", "10", "0", "1", "2", "8", "12", "13", "1",
        "4", "z", "fitted", "1", "1", "0", "0", "GGLM_RESULT_V1", "1"
    };
    assert(generalizedDispatcher.dispatch(versionedGeneralizedResult) ==
           "OK\tglm_generalized_generation");
    const auto &afterOldGeneration = generalizedDispatcher.applicationState()
        .generalizedGLMs().at("glm_generalized_generation");
    assert(afterOldGeneration.rFitPending && afterOldGeneration.rFitGeneration == 2);
    assert(refreshedGeneralizedGLMs.size() == refreshCountBeforeOldGeneration);
    versionedGeneralizedResult.back() = "2";
    assert(generalizedDispatcher.dispatch(versionedGeneralizedResult) ==
           "OK\tglm_generalized_generation");
    const auto &afterCurrentGeneration = generalizedDispatcher.applicationState()
        .generalizedGLMs().at("glm_generalized_generation");
    assert(!afterCurrentGeneration.rFitPending && afterCurrentGeneration.rFitGeneration == 2);
    assert(refreshedGeneralizedGLMs.size() == refreshCountBeforeOldGeneration + 1);
    assert(generalizedDispatcher.dispatch(versionedGeneralizedResult) ==
           "OK\tglm_generalized_generation");
    assert(refreshedGeneralizedGLMs.size() == refreshCountBeforeOldGeneration + 1);

    assert(generalizedDispatcher.dispatch(
        {"GENERALIZED_GLM_OPEN_STRUCTURED", "glm_panel", "cars", "Generalized Linear Model", "distance",
         "gaussian", "identity", "selected", "fit complete", "1", "speed", "1", "speed", "numeric",
         "ANALYSIS_SCOPE_V1", "explicit", "trellis_panel", "Trellis panel — cyl = 4", "10", "3", "1", "4", "7",
         "TRUE", "3", "7", "1", "2", "8", "12", "13", "1", "4", "t", "fitted", "3", "1", "4", "7",
         "0", "0"}) == "OK\tglm_panel");
    const auto &panelGeneralized =
        generalizedDispatcher.applicationState().generalizedGLMs().at("glm_panel");
    assert(panelGeneralized.dataScopeCaptured);
    assert(panelGeneralized.dataScope.kind == rlispstat::core::AnalysisScopeKind::ExplicitRowIds);
    assert(panelGeneralized.dataScope.sourceKind == rlispstat::core::AnalysisScopeSourceKind::TrellisPanel);
    assert(panelGeneralized.dataScope.sourceDescription == "Trellis panel — cyl = 4");
    assert(panelGeneralized.dataScope.originalRowIds == std::vector<int>({1, 4, 7}));

    std::vector<std::string> openedGeneralizedGLMs;
    rlispstat::core::CommandDispatcherServices generalizedOpenServices;
    generalizedOpenServices.queries.groupSeed = [&](const std::string &group, rlispstat::core::PlotModel &seed) {
        if (group != "cars") return false;
        seed = modelSeed;
        return true;
    };
    generalizedOpenServices.queries.createGeneralizedGLMId =
        [](const std::string &) { return std::string("gglm_cars_1"); };
    generalizedOpenServices.ui.showGeneralizedGLM =
        [&](const std::string &id) { openedGeneralizedGLMs.push_back("show:" + id); };
    generalizedOpenServices.ui.requestGeneralizedGLMFit =
        [&](const std::string &id) { openedGeneralizedGLMs.push_back("fit:" + id); return true; };
    rlispstat::core::CommandDispatcher generalizedOpenDispatcher(generalizedOpenServices);
    assert(generalizedOpenDispatcher.dispatch(
        {"GENERALIZED_GLM_OPEN", "cars", "distance", "binomial", "logit", "1", "speed"}) ==
           "OK\tgglm_cars_1");
    const auto &openedGeneralized =
        generalizedOpenDispatcher.applicationState().generalizedGLMs().at("gglm_cars_1");
    assert(openedGeneralized.terms == std::vector<std::string>({"speed"}));
    assert((openedGeneralizedGLMs == std::vector<std::string>{"show:gglm_cars_1", "fit:gglm_cars_1"}));
    assert(generalizedOpenDispatcher.dispatch(
        {"GENERALIZED_GLM_OPEN", "cars", "distance", "bad", "logit", "0", ""}) ==
           "ERR invalid generalized linear model family");

    std::vector<std::string> shownPooledGeneralizedGLMs;
    rlispstat::core::CommandDispatcherServices pooledGeneralizedServices;
    pooledGeneralizedServices.queries.groupSeed = generalizedOpenServices.queries.groupSeed;
    pooledGeneralizedServices.ui.showGeneralizedGLM =
        [&](const std::string &id) { shownPooledGeneralizedGLMs.push_back(id); };
    rlispstat::core::CommandDispatcher pooledGeneralizedDispatcher(pooledGeneralizedServices);
    assert(pooledGeneralizedDispatcher.dispatch(
        {"GENERALIZED_GLM_OPEN_POOLED", "gglm_pooled", "cars", "Pooled", "outcome", "binomial",
         "logit", "all", "5", "pooled", "0", "TRUE", "10", "0", "1", "2", "8", "12", "13",
         "1", "4", "z", "fitted", "1", "1", "0", "0"}) == "OK\tgglm_pooled");
    const auto &pooledGeneralized =
        pooledGeneralizedDispatcher.applicationState().generalizedGLMs().at("gglm_pooled");
    assert(pooledGeneralized.precomputed && pooledGeneralized.multipleImputation);
    assert(pooledGeneralized.imputationCount == 5 && !pooledGeneralized.autoRefit);
    assert((shownPooledGeneralizedGLMs == std::vector<std::string>{"gglm_pooled"}));

    // A pooled result is accepted only for the exact pending generation and
    // immutable specification.  A malformed current-generation result must
    // terminate pending instead of provoking an Auto-refit loop.
    std::vector<std::string> refreshedPooledGeneralizedGLMs;
    pooledGeneralizedServices.ui.refreshGeneralizedGLM =
        [&](const std::string &id) { refreshedPooledGeneralizedGLMs.push_back(id); };
    rlispstat::core::CommandDispatcher versionedPooledDispatcher(pooledGeneralizedServices);
    rlispstat::core::GeneralizedGLMState pendingPooled;
    pendingPooled.id = "gglm_pooled_versioned";
    pendingPooled.group = "cars";
    pendingPooled.response = "outcome";
    pendingPooled.family = "binomial";
    pendingPooled.link = "logit";
    pendingPooled.scope = "all";
    pendingPooled.terms = {"speed"};
    pendingPooled.termTypes = {{"speed", "numeric"}};
    pendingPooled.termTypeOverrides = pendingPooled.termTypes;
    pendingPooled.multipleImputation = true;
    pendingPooled.autoRefit = true;
    pendingPooled.modelVersion = 3;
    pendingPooled.rFitGeneration = 5;
    pendingPooled.rFitPending = true;
    pendingPooled.dataScope = rlispstat::core::AllObservationsAnalysisScope("cars", 10);
    pendingPooled.dataScopeCaptured = true;
    pendingPooled.lastRFitSignature =
        rlispstat::core::GeneralizedGLMFitSignature(pendingPooled);
    versionedPooledDispatcher.applicationState().generalizedGLMs()[pendingPooled.id] =
        pendingPooled;
    auto pooledResult = [](const std::string &term, const std::string &generation,
                           const std::string &fitOk = "TRUE",
                           const std::string &status = "fitted") {
        return std::vector<std::string>{
            "GENERALIZED_GLM_OPEN_POOLED", "gglm_pooled_versioned", "cars", "Pooled",
            "outcome", "binomial", "logit", "all", "5", "pooled", "1", term,
            "MODEL_SPEC_V1", "1", term, "numeric", "0", "0",
            "ANALYSIS_SCOPE_V1", "all", "all_data", "All observations", "10", "0",
            fitOk, "10", "0", "1", "2", "8", "NA", "NA", "1", "NA", "z",
            status, "1", "1", "0", "0", "GGLM_RESULT_V1", generation
        };
    };
    assert(versionedPooledDispatcher.dispatch(pooledResult("distance", "5")) ==
           "OK\tgglm_pooled_versioned");
    const auto &afterMismatchedPooled = versionedPooledDispatcher.applicationState()
        .generalizedGLMs().at("gglm_pooled_versioned");
    assert(!afterMismatchedPooled.rFitPending);
    assert(afterMismatchedPooled.autoRefit);
    assert(afterMismatchedPooled.terms == std::vector<std::string>({"speed"}));
    assert(afterMismatchedPooled.status.find("Rejected pooled") != std::string::npos);
    assert(refreshedPooledGeneralizedGLMs ==
           std::vector<std::string>({"gglm_pooled_versioned"}));

    auto &retryPooled = versionedPooledDispatcher.applicationState()
        .generalizedGLMs().at("gglm_pooled_versioned");
    retryPooled.rFitPending = true;
    retryPooled.rFitGeneration = 6;
    retryPooled.autoRefit = true;
    assert(versionedPooledDispatcher.dispatch(pooledResult("speed", "6")) ==
           "OK\tgglm_pooled_versioned");
    const auto &afterAcceptedPooled = versionedPooledDispatcher.applicationState()
        .generalizedGLMs().at("gglm_pooled_versioned");
    assert(!afterAcceptedPooled.rFitPending);
    assert(afterAcceptedPooled.autoRefit);
    assert(afterAcceptedPooled.fitVersion == 3);

    // A terminal backend error for the exact pending specification also ends
    // the request and disables Auto-refit, so refreshing the native window
    // cannot immediately submit the same failing revision again.
    auto &failingPooled = versionedPooledDispatcher.applicationState()
        .generalizedGLMs().at("gglm_pooled_versioned");
    failingPooled.rFitPending = true;
    failingPooled.rFitGeneration = 7;
    failingPooled.autoRefit = true;
    assert(versionedPooledDispatcher.dispatch(
        pooledResult("speed", "7", "FALSE", "deliberate fit error")) ==
           "OK\tgglm_pooled_versioned");
    const auto &afterFailedPooled = versionedPooledDispatcher.applicationState()
        .generalizedGLMs().at("gglm_pooled_versioned");
    assert(!afterFailedPooled.rFitPending);
    assert(afterFailedPooled.autoRefit);
    assert(afterFailedPooled.status == "deliberate fit error");

    rlispstat::core::NativeMixedModelState shownMixedModel;
    rlispstat::core::CommandDispatcherServices mixedServices;
    mixedServices.ui.showMixedModel =
        [&](const rlispstat::core::NativeMixedModelState &state) { shownMixedModel = state; };
    rlispstat::core::CommandDispatcher mixedDispatcher(mixedServices);
    assert(mixedDispatcher.dispatch(
        {"MIXED_MODEL_OPEN_STRUCTURED", "mixed_1", "cars", "linear_mixed_model", "distance", "", "",
         "", "1", "speed", "1", "gear", "|", "0", "2", "line one", "line two"}) == "OK\tmixed_1");
    assert(shownMixedModel.method == "REML" && shownMixedModel.randomEffects.front().terms ==
           std::vector<std::string>({"1"}));
    assert(mixedDispatcher.applicationState().nativeMixedModels().at("mixed_1").reportText ==
           "line one\nline two");

    std::string mixedTextShown;
    rlispstat::core::CommandDispatcherServices mixedTextServices;
    mixedTextServices.ui.showMixedModelText =
        [&](const std::string &, const std::string &, const std::string &, const std::string &text) {
            mixedTextShown = text;
        };
    rlispstat::core::CommandDispatcher mixedTextDispatcher(mixedTextServices);
    assert(mixedTextDispatcher.dispatch(
        {"MIXED_MODEL_OPEN_TEXT", "mixed_text", "cars", "linear", "2", "first", "second"}) ==
           "OK\tmixed_text");
    assert(mixedTextShown == "first\nsecond");
    assert(mixedTextDispatcher.dispatch({"MIXED_MODEL_OPEN_TEXT", "x", "cars", "linear", "-1"}) ==
           "ERR invalid mixed model payload");

    rlispstat::core::CommandDispatcherServices generalizedDiagnosticServices;
    generalizedDiagnosticServices.ui.openGeneralizedGLMDiagnostic =
        [](const std::string &model, const std::string &type) {
            return model == "gglm_1" && type == "residuals" ? "diag_1" : "";
        };
    rlispstat::core::CommandDispatcher generalizedDiagnosticDispatcher(generalizedDiagnosticServices);
    assert(generalizedDiagnosticDispatcher.dispatch(
        {"GENERALIZED_GLM_OPEN_DIAGNOSTIC", "gglm_1", "residuals"}) == "OK\tdiag_1");
    assert(generalizedDiagnosticDispatcher.dispatch(
        {"GENERALIZED_GLM_OPEN_DIAGNOSTIC", "missing", "residuals"}) ==
           "ERR generalized linear model diagnostic could not be opened");

    rlispstat::core::PlotModel boxplot;
    boxplot.id = "box_1";
    boxplot.kind = "boxplot";
    boxplot.group = "box_data";
    boxplot.yLabel = "x";
    boxplot.variables = {{"x", {1.0, 2.0, 3.0, 4.0}}, {"y", {3.0, 4.0, 5.0, 6.0}}};
    boxplot.boxplotVariables = {"x"};
    int boxplotRedraws = 0;
    rlispstat::core::CommandDispatcherServices boxplotServices;
    boxplotServices.queries.plot = [&](const std::string &plotId,
                                      rlispstat::core::PlotModel &plot) {
        if (plotId != "box_1") return false;
        plot = boxplot; return true;
    };
    boxplotServices.ui.redrawPlot = [&](const std::string &plotId) {
        assert(plotId == "box_1"); ++boxplotRedraws;
    };
    boxplotServices.selection.mutateBoxplot =
        [&](const std::string &plotId,
            const rlispstat::core::CommandBoxplotMutation &mutation,
            std::string &message) {
            if (plotId != "box_1") return false;
            return mutation(boxplot, message);
        };
    rlispstat::core::CommandDispatcher boxplotDispatcher(boxplotServices);
    rlispstat::core::DataFrameModel boxData;
    boxData.group = "box_data"; boxData.rows = 4;
    rlispstat::core::DataColumn boxY; boxY.name = "x"; boxY.type = "numeric";
    boxY.values = {"1", "2", "3", "4"};
    rlispstat::core::DataColumn boxGroup; boxGroup.name = "g"; boxGroup.type = "factor";
    boxGroup.values = {"A", "A", "B", "B"}; boxGroup.definedLevels = {"A", "B"};
    boxData.columns = {boxY, boxGroup};
    boxplotDispatcher.applicationState().registerDataset(boxData);
    assert(boxplotDispatcher.dispatch({"BOXPLOT_ADD_VARIABLE", "box_1", "y"}) == "OK");
    assert((boxplot.boxplotVariables == std::vector<std::string>{"x", "y"}));
    assert(boxplotDispatcher.dispatch({"BOXPLOT_REMOVE_VARIABLE", "box_1", "y"}) == "OK");
    assert((boxplot.boxplotVariables == std::vector<std::string>{"x"}));
    assert(boxplotDispatcher.dispatch(
        {"BOXPLOT_ADD_GROUPING_VARIABLE", "box_1", "g"}) == "OK");
    assert((boxplot.boxplotGroupingVariables == std::vector<std::string>{"g"}));
    assert((boxplot.boxplotCategories == std::vector<std::string>{"A", "B"}));
    assert(boxplotDispatcher.dispatch(
        {"BOXPLOT_REMOVE_GROUPING_VARIABLE", "box_1", "g"}) == "OK");
    assert(boxplot.boxplotGroupingVariables.empty() && boxplot.xLabel.empty());
    assert(boxplotDispatcher.dispatch({"BOXPLOT_ADD_VARIABLE", "box_1", "missing"}) ==
           "ERR numeric variable not found: missing");
    assert(boxplotDispatcher.dispatch({"BOXPLOT_REMOVE_VARIABLE", "box_1", "x"}) ==
           "ERR a boxplot must keep at least one variable");
    assert(boxplotDispatcher.dispatch({"BOXPLOT_ADD_VARIABLE", "missing", "y"}) ==
           "ERR no active plot");
    assert(boxplotDispatcher.dispatch({"BOXPLOT_ADD_VARIABLE", "box_1"}) ==
           "ERR malformed boxplot variable command");
    assert(boxplotDispatcher.dispatch({"BOXPLOT_OPTIONS", "box_1"}).rfind("OK\t", 0) == 0);
    assert(boxplotDispatcher.dispatch({"BOXPLOT_OPTION", "box_1", "points", "FALSE"}) == "OK");
    assert(!boxplot.boxplotShowPoints);
    assert(boxplotDispatcher.dispatch({"BOXPLOT_OPTION", "box_1", "standardize", "TRUE"}) == "OK");
    assert(boxplot.boxplotStandardizeVariables);
    assert(boxplotDispatcher.dispatch(
        {"BOXPLOT_SPLIT_VIOLIN", "box_1", "TRUE", "two.sided", "2", "-2"}) == "OK");
    assert(boxplot.boxplotSplitViolin && boxplot.boxplotShowViolin);
    assert(boxplot.boxplotSplitLower == -2.0 && boxplot.boxplotSplitUpper == 2.0);
    boxplot.boxplotShowH0Simulation = true;
    assert(boxplotDispatcher.dispatch(
        {"BOXPLOT_H0_SIMULATION", "box_1", "FALSE", "0", "two.sided", "100"}) == "OK");
    assert(!boxplot.boxplotShowH0Simulation);
    assert(boxplotDispatcher.dispatch(
        {"BOXPLOT_H0_SIMULATION", "box_1", "TRUE", "nan", "two.sided", "100"}) ==
           "ERR invalid H0 value");
    assert(boxplotDispatcher.dispatch({"BOXPLOT_OPTION", "box_1", "unknown", "TRUE"}) ==
           "ERR unknown boxplot option");
    assert(boxplotRedraws == 8);

    rlispstat::core::CommandDispatcher linkedViewsDispatcher({});
    auto &linkedState = linkedViewsDispatcher.applicationState();
    rlispstat::core::PlotModel linkedFirst;
    linkedFirst.id = "linked-1";
    linkedFirst.group = "shared-data";
    rlispstat::core::PlotModel linkedSecond;
    linkedSecond.id = "linked-2";
    linkedSecond.group = "shared-data";
    linkedState.ensureSelectionGroup("shared-data");
    linkedState.plots()[linkedFirst.id] = &linkedFirst;
    linkedState.plots()[linkedSecond.id] = &linkedSecond;
    int linkedSelectionVersion = 0;
    assert(linkedState.setSelectedRows(
        "shared-data", {2, 5}, &linkedSelectionVersion));
    assert(linkedSelectionVersion == 1);
    std::set<int> linkedRows;
    assert(linkedState.selectedRows(linkedFirst.group, linkedRows));
    assert(linkedRows == std::set<int>({2, 5}));
    assert(linkedState.selectedRows(linkedSecond.group, linkedRows));
    assert(linkedRows == std::set<int>({2, 5}));
    linkedState.plots().erase(linkedFirst.id);
    assert(linkedState.plots().count(linkedSecond.id) == 1);
    assert(linkedState.selectedRows("shared-data", linkedRows));
    assert(linkedRows == std::set<int>({2, 5}));
    assert(linkedState.clearSelectedRows("shared-data", &linkedSelectionVersion));
    assert(linkedSelectionVersion == 2);
    assert(linkedState.selectedRows(linkedSecond.group, linkedRows));
    assert(linkedRows.empty());

    int closeAllCalls = 0;
    rlispstat::core::CommandDispatcher closeAllDispatcher({
        {}, {},
        {
            {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {},
            [&]() { ++closeAllCalls; }
        },
        {}, {}
    });
    rlispstat::core::DataFrameModel retainedDataset;
    retainedDataset.group = "cars";
    closeAllDispatcher.applicationState().datasets().registerDataset(retainedDataset);
    closeAllDispatcher.applicationState().groupSelections()["cars"] = {1};
    closeAllDispatcher.applicationState().groupPointColors()["cars"][1] = "blue";
    closeAllDispatcher.applicationState().groupModels()["cars"] = {};
    closeAllDispatcher.applicationState().correlationMatrices()["corr"] = {"corr", "cars"};
    closeAllDispatcher.applicationState().dimensionalityModels()["pca"] = {"pca", "cars"};
    closeAllDispatcher.applicationState().dendrograms()["tree"] = {"tree", "cars"};
    closeAllDispatcher.applicationState().generalizedComparisons()["gglmcmp"] = {"gglmcmp", "cars"};
    assert(closeAllDispatcher.dispatch({"CLOSE_ALL"}) == "OK");
    assert(closeAllCalls == 1);
    assert(closeAllDispatcher.applicationState().datasets().contains("cars"));
    assert(closeAllDispatcher.applicationState().groupSelections().empty());
    assert(closeAllDispatcher.applicationState().groupPointColors().empty());
    assert(closeAllDispatcher.applicationState().groupModels().empty());
    assert(closeAllDispatcher.applicationState().correlationMatrices().empty());
    assert(closeAllDispatcher.applicationState().dimensionalityModels().empty());
    assert(closeAllDispatcher.applicationState().dendrograms().empty());
    assert(closeAllDispatcher.applicationState().generalizedComparisons().empty());

    rlispstat::core::PlotModel interactionFitModel;
    interactionFitModel.group = "interaction_data";
    interactionFitModel.yLabel = "outcome";
    interactionFitModel.variables = {
        {"outcome", {2.0, 4.1, 4.9, 10.2, 5.8, 13.9}},
        {"dose", {0.0, 1.0, 2.0, 0.0, 1.0, 2.0}}
    };
    interactionFitModel.variableMeta = {{"outcome", "numeric"}, {"dose", "numeric"}, {"condition", "factor"}};
    rlispstat::core::DataFrameModel interactionFitData;
    interactionFitData.group = "interaction_data";
    interactionFitData.rows = 6;
    interactionFitData.columns = {{"condition", "factor", "", "", -1, {"control", "control", "control", "treatment", "treatment", "treatment"}}};
    rlispstat::core::GLMFitSummary interactionFit = rlispstat::core::FitMultipleLinearModel(
        interactionFitModel, &interactionFitData, {}, "outcome",
        {"dose", "condition", "dose:condition"}, {}, "all");
    assert(interactionFit.ok);
    assert(interactionFit.n == 6);
    assert(interactionFit.designLabels.size() == 4);
    assert(interactionFit.coefficients.size() >= 6);
    rlispstat::core::GLMInteractionReport interactionReport = rlispstat::core::BuildGLMInteractionReport(
        interactionFitModel, &interactionFitData, "dose:condition",
        {"dose", "condition", "dose:condition"}, {}, interactionFit);
    assert(interactionReport.ok);
    assert(interactionReport.kind == "continuous by categorical");
    assert(rlispstat::core::GLMInteractionReportText(interactionReport).find("Simple slopes") != std::string::npos);

    rlispstat::core::CommandDispatcherServices interactionReportServices;
    interactionReportServices.queries.groupSeed = [&](const std::string &group, rlispstat::core::PlotModel &seed) {
        if (group != "interaction_data") return false;
        seed = interactionFitModel;
        return true;
    };
    rlispstat::core::CommandDispatcher interactionReportDispatcher(interactionReportServices);
    interactionReportDispatcher.applicationState().datasets().registerDataset(interactionFitData);
    rlispstat::core::GroupModelState &interactionState = interactionReportDispatcher.applicationState().groupModels()["interaction_data"];
    interactionState.response = "outcome";
    interactionState.terms = {"dose", "condition", "dose:condition"};
    assert(interactionReportDispatcher.dispatch(
        {"MODEL_INTERACTION_REPORT", "interaction_data", "dose:condition"}) ==
        "ERR linear interaction report requires a completed R fit; refit the model and retry");
    interactionReportDispatcher.applicationState().linearModelFits()["interaction_data"] =
        interactionFit;
    const std::string interactionReply = interactionReportDispatcher.dispatch(
        {"MODEL_INTERACTION_REPORT", "interaction_data", "dose:condition"});
    assert(interactionReply.rfind("OK\t", 0) == 0);
    assert(rlispstat::core::DecodeCommandField(interactionReply.substr(3)).find("Simple slopes") != std::string::npos);
    assert(interactionReportDispatcher.dispatch({"MODEL_INTERACTION_REPORT", "interaction_data"}) ==
           "ERR malformed MODEL_INTERACTION_REPORT command");
    assert(interactionReportDispatcher.dispatch({"MODEL_INTERACTION_REPORT", "missing", "dose:condition"}) ==
           "ERR no registered dataset/group for model");
    rlispstat::core::GLMFitSummary selectedInteractionFit = rlispstat::core::FitMultipleLinearModel(
        interactionFitModel, &interactionFitData, {1, 2, 3}, "outcome",
        {"dose", "condition", "dose:condition"}, {}, "selected");
    assert(selectedInteractionFit.n == 3);
    assert(selectedInteractionFit.rowsUsed == std::vector<int>({1, 2, 3}));
    rlispstat::core::Table1DisplayState receivedTable1;
    bool receivedTable1State = false;
    rlispstat::core::CommandDispatcherServices table1Services;
    table1Services.ui.showTable1 = [&](const rlispstat::core::Table1DisplayState &state) {
        receivedTable1 = state;
        receivedTable1State = true;
    };
    rlispstat::core::CommandDispatcher table1Dispatcher(table1Services);
    const std::string table1Reply = table1Dispatcher.dispatch({
        "TABLE1_OPEN_STRUCTURED", "table1_1", "cars", "Table 1. Descriptive statistics", "",
        "2", "mpg", "wt", "2", "mpg", "numeric", "wt", "categorical",
        "1", "Overall", "5",
        "1", "numeric_mean_sd", "mpg", "", "mpg", "", "", "mpg mean/SD", "1", "20.09 (6.03)",
        "2", "categorical_parent", "wt", "", "wt", "", "", "", "1", "",
        "3", "categorical_level", "wt", "Aurelia", "Aurelia", "", "", "wt = Aurelia n (%)", "1", "10 (31.2%)",
        "4", "categorical_level", "wt", "Borealis", "Borealis", "", "", "wt = Borealis n (%)", "1", "11 (34.4%)",
        "5", "categorical_level", "wt", "Cygnus", "Cygnus", "", "", "wt = Cygnus n (%)", "1", "11 (34.4%)",
        "1", "Numeric variables are shown as mean (SD).",
        "all", "All observations", "32", "0", "TRUE", "TRUE",
        "TABLE1_DISPLAY_V1", "5",
        "1", "1", "7", "mean", "20.09", "sd", "6.03", "se", "1.07",
        "ci95", "[17.91, 22.27]", "median", "19.20", "q1", "15.43", "q3", "22.80",
        "2", "1", "0",
        "3", "1", "0",
        "4", "1", "0",
        "5", "1", "0"
    });
    assert(table1Reply == "OK\ttable1_1");
    assert(receivedTable1State);
    assert(receivedTable1.datasetId == "cars");
    assert(receivedTable1.variables.size() == 2);
    assert(receivedTable1.rows.size() == 5);
    assert(receivedTable1.rows[0].values[0] == "20.09 (6.03)");
    assert(receivedTable1.rows[1].rowType == "categorical_parent");
    assert(receivedTable1.rows[2].level == "Aurelia");
    assert(receivedTable1.rows[3].level == "Borealis");
    assert(receivedTable1.rows[4].level == "Cygnus");
    assert(receivedTable1.dataScopeCaptured);
    assert(receivedTable1.showP);
    assert(receivedTable1.showTest);
    assert(receivedTable1.rows[0].statisticValues.size() == 1);
    assert(receivedTable1.rows[0].statisticValues[0].at("mean") == "20.09");
    assert(receivedTable1.rows[0].statisticValues[0].at("ci95") == "[17.91, 22.27]");

    const auto miContingencyReply = table1Dispatcher.dispatch({
        "TABLE1_OPEN_STRUCTURED", "mi_cross", "cars", "Contingency Table — Multiple Imputation", "group",
        "1", "category", "0", "2", "A", "Total", "1",
        "1", "mi_contingency_level", "", "First", "First", "", "", "", "2", "1.5 (75.0%)", "2.0 (100.0%)",
        "1", "Descriptive averages across imputations.",
        "explicit", "Selected rows", "32", "2", "2", "4", "FALSE", "FALSE",
        "TABLE_KIND_V1", "mi_contingency"
    });
    assert(miContingencyReply == "OK\tmi_cross");
    assert(receivedTable1.tableType == "mi_contingency");
    assert(receivedTable1.stubHeaders == std::vector<std::string>{"category"});
    assert(receivedTable1.rows.front().stubValues == std::vector<std::string>{"First"});
    assert(!receivedTable1.showP && !receivedTable1.showTest);
    assert(!receivedTable1.linkEnabled);
    assert(receivedTable1.dataScope.kind == rlispstat::core::AnalysisScopeKind::ExplicitRowIds);
    const auto preserved = rlispstat::core::Table1ApplyDisplayPreferences(
        receivedTable1, rlispstat::core::Table1DisplayPreferences{});
    assert(preserved.rows.front().values.front() == "1.5 (75.0%)");

    if (argc > 1) {
        std::ofstream scripts(argv[1]);
        for (const std::string &id : {"plot_new", "time_new", "box_new", "hist_new", "bar_new"}) {
            const auto *reference = addPlotDispatcher.applicationState().outputCodeReference(id);
            assert(reference);
            scripts << "local({\n"
                    << reference->provenance.verificationRCode.at("plot")
                    << "})\n";
        }
    }

    return 0;
}
