#include "../../src/core/command_dispatcher.h"
#include "../../src/core/histogram_model.h"
#include "../../src/core/session_controller.h"

#include <cassert>
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

} // namespace

int main()
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
    assert(uiDispatcher.dispatch({"PLOT_THEME"}) == "OK\tclassic");
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
    firstState.applicationState().generalizedComparisons()["gglmcmp"].termRows = {"model"};
    firstState.applicationState().generalizedComparisons()["gglmcmp"].models = {{"m1", "Model 1", "model"}};
    firstState.applicationState().generalizedGLMs()["gglm"] = {"gglm", "cars", "model", {"model"}};
    firstState.applicationState().regressionComparisons()["regcmp"] = {"regcmp", "cars", "model"};
    firstState.applicationState().regressionComparisons()["regcmp"].models = {{"m1", "Model 1", "model", {"model"}}};
    const rlispstat::core::VariableTypeChangeEffects typeEffects =
        firstState.applicationState().applyVariableTypeChange("cars", "model", "factor");
    assert(typeEffects.correlationIds == std::vector<std::string>({"corr"}));
    assert(typeEffects.dendrogramIds == std::vector<std::string>({"tree"}));
    assert(typeEffects.dimensionalityIds.empty());
    assert(typeEffects.generalizedGlmIdsToRefit == std::vector<std::string>({"gglm"}));
    assert(firstState.applicationState().correlationMatrices().at("corr").variables.empty());
    assert(firstState.applicationState().correlationMatrices().at("corr").selectedRow == -1);
    assert(firstState.applicationState().dendrograms().at("tree").variables.empty());
    assert(firstState.applicationState().dimensionalityModels().at("pca").status ==
           "At least two numeric variables are required.");
    assert(firstState.applicationState().groupModels().at("cars").termTypes.at("model") == "factor");
    assert(firstState.applicationState().generalizedComparisons().at("gglmcmp").models.front().isStale);
    std::string renameError;
    assert(rlispstat::core::RenameDataFrameColumn(
        *firstState.applicationState().datasets().find("cars"), "model", "renamed", &renameError));
    firstState.applicationState().applyVariableRename("cars", "model", "renamed");
    assert(firstState.applicationState().labelColumn("cars") == "renamed");
    assert(firstState.applicationState().groupModels().at("cars").termTypes.at("renamed") == "factor");
    assert(firstState.applicationState().generalizedGLMs().at("gglm").response == "renamed");
    assert(firstState.applicationState().regressionComparisons().at("regcmp").response == "renamed");
    assert(firstState.applicationState().generalizedComparisons().at("gglmcmp").termRows.front() == "renamed");
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
    assert(colorDispatcher.dispatch({"SET_POINT_COLOR", "cars", "green", "2", "2", "3"}) == "OK");
    assert(colorDispatcher.dispatch({"CLEAR_ROW_COLORS", "cars", "1", "1"}) == "OK");
    assert(colorDispatcher.dispatch({"SET_SELECTED_COLOR", "missing", "blue"}) ==
           "ERR no active plot/group");
    assert(colorDispatcher.dispatch({"SET_POINT_COLOR", "cars", "unknown", "1", "1"}) ==
           "ERR unknown color");
    assert((colorEvents == std::vector<std::string>{
        "cars:1 3:1:1", "cars:2 3:0:1", "cars:1:0:1"}));

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
    assert(datasetDispatcher.applicationState().datasets().activeDatasetGroup() == "cars");
    assert((activeDatasetChanges == std::vector<std::string>{"cars"}));

    rlispstat::core::CommandDispatcher savedScopeDispatcher({});
    rlispstat::core::DataFrameModel savedScopeData;
    savedScopeData.group = "scope cars";
    savedScopeData.rows = 5;
    assert(savedScopeDispatcher.applicationState().registerDataset(savedScopeData));
    assert(savedScopeDispatcher.applicationState().setSelectedRows(
        "scope cars", std::set<int>{1, 3}));
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
        {
            {}, {}, {}, {},
            [](const std::string &group, std::size_t &selected, std::vector<std::string> &plotIds) {
                if (group != "cars") return false;
                selected = 0;
                plotIds = {"plot_5"};
                return true;
            }
        },
        {
            {}, {}, {}, {}, {}, {}, {}, {}, {},
            [&](const std::string &group) { variableWindows.push_back(group); }
        },
        {}, {}
    });
    assert(variablesDispatcher.dispatch({"VARIABLES_WINDOW"}) == "ERR missing group name");
    assert(variablesDispatcher.dispatch({"VARIABLES_WINDOW", "missing"}) ==
           "ERR no active plot/group");
    assert(variablesDispatcher.dispatch({"VARIABLES_WINDOW", "cars"}) == "OK");
    assert((variableWindows == std::vector<std::string>{"cars"}));

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

    int variableTypeNotifications = 0;
    rlispstat::core::CommandDispatcherServices variableTypeServices;
    variableTypeServices.ui.datasetVariableTypeChanged =
        [&](const std::string &group, const std::string &variable, const std::string &type,
            const rlispstat::core::VariableTypeChangeEffects &) {
            assert(group == "cars" && variable == "speed" && type == "factor");
            ++variableTypeNotifications;
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
    assert(variableTypeDispatcher.applicationState().registerDataset(variableTypeDataset));
    assert(variableTypeDispatcher.dispatch({"SET_VARIABLE_TYPE", "cars", "speed"}) ==
           "ERR malformed SET_VARIABLE_TYPE command");
    assert(variableTypeDispatcher.dispatch({"SET_VARIABLE_TYPE", "cars", "speed", "factor"}) ==
           "OK\tspeed is now treated as Factor.");
    assert(variableTypeDispatcher.dispatch({"SET_VARIABLE_TYPE", "cars", "missing", "factor"}) ==
           "ERR Variable `missing` was not found in dataset `cars`.");
    assert(variableTypeDispatcher.dispatch({"SET_VARIABLE_TYPE", "cars", "speed", "unsupported"}) ==
           "ERR Variable type must be numeric, factor, ordered factor, text, or logical.");
    assert(variableTypeNotifications == 1);

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
    assert((registeredDatasets == std::vector<std::string>{"cars:show"}));
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
           "OK\tcars|distance|all|speed");
    assert(modelInfoDispatcher.dispatch({"MODEL_SET_Y", "cars", "missing"}) ==
           "ERR dependent variable must be an available numeric variable");
    assert(modelInfoDispatcher.dispatch({"MODEL_SET_Y", "cars", "distance"}) == "OK");
    assert(modelInfoDispatcher.dispatch({"MODEL_ADD_TERM", "cars", "weight"}) == "OK");
    assert(modelInfoDispatcher.dispatch({"MODEL_REMOVE_TERM", "cars", "speed"}) == "OK");
    assert(modelInfoDispatcher.dispatch({"MODEL_CLEAR_ROLE", "cars", "weight"}) == "OK");
    assert(modelInfoDispatcher.dispatch({"MODEL_ADD_TERM", "cars", "missing"}) ==
           "ERR predictor must be an available variable");
    assert(modelInfoDispatcher.dispatch({"MODEL_SCOPE", "cars", "invalid"}) ==
           "ERR model scope must be all, selected, unselected, or compare_selected_all");
    assert(modelInfoDispatcher.dispatch({"MODEL_SCOPE", "cars", "selected"}) == "OK");
    assert(modelInfoDispatcher.dispatch({"MODEL_INFO", "cars"}) ==
           "OK\tcars|distance|selected");

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
    rlispstat::core::CommandDispatcher correlationOpenDispatcher({
        {},
        {
            {}, {}, {}, {}, {}, {}, {},
            {},
            [&](const std::string &group, rlispstat::core::PlotModel &seed) {
                if (group != "mi_cars") return false;
                seed = modelSeed;
                seed.group = group;
                return true;
            },
            [](const std::string &group) { return group == "mi_cars"; }
        },
        {}, {},
        {
            {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {},
            [&](const std::string &id) {
                if (id != "corr_open") return false;
                ++openedCorrelationRefits;
                return true;
            }
        }
    });
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
    rlispstat::core::CommandDispatcher dimensionalityOpenDispatcher({
        {},
        {
            {}, {}, {}, {}, {}, {}, {},
            {},
            [&](const std::string &group, rlispstat::core::PlotModel &seed) {
                if (group != "cars") return false;
                seed = modelSeed;
                return true;
            }
        },
        {}, {},
        {
            {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {},
            [&](const std::string &id) {
                if (id != "pca_open") return false;
                ++openedDimensionalityRefits;
                return true;
            }
        }
    });
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
        {"PCAFA_OPEN", "pca_bad", "cars", "pca", "listwise", "TRUE", "2", "1", "speed"}) ==
           "ERR principal components/factor analysis requires at least two numeric variables");

    int openedDendrogramRefits = 0;
    rlispstat::core::CommandDispatcher dendrogramOpenDispatcher({
        {},
        {
            {}, {}, {}, {}, {}, {}, {},
            {},
            [&](const std::string &group, rlispstat::core::PlotModel &seed) {
                if (group != "cars") return false;
                seed = modelSeed;
                return true;
            }
        },
        {}, {},
        {
            {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {},
            [&](const std::string &id) {
                if (id != "tree_open") return false;
                ++openedDendrogramRefits;
                return true;
            }
        }
    });
    assert(dendrogramOpenDispatcher.dispatch(
        {"DENDRO_OPEN", "tree_open", "cars", "correlation", "complete", "pairwise", "3",
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
    dimensionalityUpdateDispatcher.applicationState().dimensionalityModels()["pca_update"] = {"pca_update", "old"};
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
    rlispstat::core::CommandDispatcher correlationVariablesDispatcher({
        {}, {}, {}, {},
        {
            {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {},
            [&](const std::string &id) {
                if (id != "corr_2") return false;
                ++correlationRefits;
                return true;
            }
        }
    });
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
    rlispstat::core::CommandDispatcher dimensionalityVariablesDispatcher({
        {}, {}, {}, {},
        {
            {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {},
            [&](const std::string &id) {
                if (id != "pca_2") return false;
                ++dimensionalityRefits;
                return true;
            }
        }
    });
    rlispstat::core::DimensionalityState dimensionalityVariables;
    dimensionalityVariables.id = "pca_2";
    dimensionalityVariables.seed.variables = {{"x", {1.0}}, {"y", {2.0}}, {"z", {3.0}}};
    dimensionalityVariablesDispatcher.applicationState().dimensionalityModels()["pca_2"] = dimensionalityVariables;
    assert(dimensionalityVariablesDispatcher.dispatch(
        {"PCAFA_SET_VARIABLES", "pca_2", "3", "x", "y", "x"}) == "OK");
    assert((dimensionalityVariablesDispatcher.applicationState().dimensionalityModels().at("pca_2").variables ==
            std::vector<std::string>{"x", "y"}));
    assert(dimensionalityVariablesDispatcher.dispatch(
        {"PCAFA_SET_VARIABLES", "pca_2", "1", "x"}) ==
           "ERR principal components/factor analysis requires at least two numeric variables");
    assert(dimensionalityRefits == 1);

    int dendrogramRefits = 0;
    rlispstat::core::CommandDispatcher dendrogramVariablesDispatcher({
        {}, {}, {}, {},
        {
            {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {},
            [&](const std::string &id) {
                if (id != "tree_2") return false;
                ++dendrogramRefits;
                return true;
            }
        }
    });
    rlispstat::core::DendrogramState dendrogramVariables;
    dendrogramVariables.id = "tree_2";
    dendrogramVariables.seed.variables = {{"x", {1.0}}, {"y", {2.0}}};
    dendrogramVariablesDispatcher.applicationState().dendrograms()["tree_2"] = dendrogramVariables;
    assert(dendrogramVariablesDispatcher.dispatch(
        {"DENDRO_SET_VARIABLES", "tree_2", "3", "x", "y", "x"}) == "OK");
    assert((dendrogramVariablesDispatcher.applicationState().dendrograms().at("tree_2").variables ==
            std::vector<std::string>{"x", "y"}));
    assert(dendrogramVariablesDispatcher.dispatch({"DENDRO_SET_VARIABLES", "tree_2", "0"}) ==
           "ERR quick cluster requires at least one numeric variable");
    assert(dendrogramRefits == 1);

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
    assert(modelErrorDispatcher.dispatch({"MODEL_UPDATE_ERROR", "cars", "R failed"}) == "OK\tglm:cars");
    assert(!modelErrorDispatcher.applicationState().linearModelFits().at("cars").ok);
    assert(modelErrorDispatcher.applicationState().linearModelFits().at("cars").warning == "R failed");
    assert(modelErrorRefreshes == 1);

    int comparisonErrorRefreshes = 0;
    rlispstat::core::CommandDispatcherServices comparisonErrorServices;
    comparisonErrorServices.ui.refreshRegressionComparison =
        [&](const std::string &group) { assert(group == "cars"); ++comparisonErrorRefreshes; };
    rlispstat::core::CommandDispatcher comparisonErrorDispatcher(comparisonErrorServices);
    rlispstat::core::RegressionComparisonState comparisonErrorState;
    comparisonErrorState.id = "cmp_1";
    comparisonErrorState.group = "cars";
    comparisonErrorState.rFitPending = true;
    comparisonErrorState.models.push_back({});
    comparisonErrorDispatcher.applicationState().regressionComparisons()["cmp_1"] = comparisonErrorState;
    assert(comparisonErrorDispatcher.dispatch({"REGCMP_UPDATE_ERROR", "cmp_1", "fit failed"}) == "OK\tcmp_1");
    assert(!comparisonErrorDispatcher.applicationState().regressionComparisons().at("cmp_1").rFitPending);
    assert(comparisonErrorDispatcher.applicationState().regressionComparisons().at("cmp_1").models.front().fit.warning ==
           "fit failed");
    assert(comparisonErrorRefreshes == 1);

    rlispstat::core::CommandDispatcher regressionComparisonUpdateDispatcher({{}, {}, {}, {}, {}});
    assert(regressionComparisonUpdateDispatcher.dispatch({"REGCMP_UPDATE"}) ==
           "ERR malformed REGCMP_UPDATE command");

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
    assert((openedComparison.models[1].includedTerms ==
            std::vector<std::string>{"speed", "weight", "speed:weight"}));
    assert((requestedRegressionComparisons == std::vector<std::string>{"cmp_open"}));
    assert((shownRegressionComparisons == std::vector<std::string>{"cmp_open"}));
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"REGCMP_OPEN2", "cmp_open2", "cars", "distance", "selected", "FALSE", "1",
         "Alternative", "speed", "1", "weight"}) == "OK\tcmp_open2");
    const auto &openedComparison2 =
        regressionComparisonOpenDispatcher.applicationState().regressionComparisons().at("cmp_open2");
    assert(openedComparison2.models.front().response == "speed");
    assert((openedComparison2.models.front().includedTerms == std::vector<std::string>{"weight"}));
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
        {"REGCMP_SET_SCOPE", "cmp_open", "unselected"}) == "OK");
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"REGCMP_SET_RESPONSE", "cmp_open", "speed"}) == "OK");
    const auto &updatedComparison =
        regressionComparisonOpenDispatcher.applicationState().regressionComparisons().at("cmp_open");
    assert(updatedComparison.scope == "unselected" && updatedComparison.response == "speed");
    assert(updatedComparison.models.front().includedTerms.empty());
    assert((requestedRegressionComparisons ==
            std::vector<std::string>{"cmp_open", "cmp_open2", "cmp_open", "cmp_open"}));
    assert((refreshedRegressionComparisons == std::vector<std::string>{"cmp_open", "cmp_open"}));
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"REGCMP_ADD_TERM", "cmp_open", "weight"}) == "OK");
    assert((regressionComparisonOpenDispatcher.applicationState().regressionComparisons().at("cmp_open")
                .models.front().includedTerms == std::vector<std::string>{"weight"}));
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"REGCMP_ADD_MODEL", "cmp_open", "Alternative", "speed", "1", "weight"}) ==
           "OK\tcmp_open:model:3");
    const auto &comparisonWithAddedModel =
        regressionComparisonOpenDispatcher.applicationState().regressionComparisons().at("cmp_open");
    assert(comparisonWithAddedModel.activeModel == 2 &&
           comparisonWithAddedModel.models.back().response == "speed");
    assert((comparisonWithAddedModel.models.back().includedTerms == std::vector<std::string>{"weight"}));
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"REGCMP_SET_TYPE", "cmp_open", "weight", "factor"}) == "OK");
    const auto &comparisonWithFactor =
        regressionComparisonOpenDispatcher.applicationState().regressionComparisons().at("cmp_open");
    assert(comparisonWithFactor.termTypes.at("weight") == "factor");
    assert(std::find(comparisonWithFactor.termRows.begin(), comparisonWithFactor.termRows.end(), "weight=3") !=
           comparisonWithFactor.termRows.end());
    assert(regressionComparisonOpenDispatcher.dispatch({"GLM", "active"}) == "OK");
    assert(regressionComparisonOpenDispatcher.dispatch({"RECORD_START"}) == "OK");
    assert((recordingCommands == std::vector<std::string>{"RECORD_START"}));
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"MODEL_OPEN_INTERACTION_PLOT", "cars", "speed:weight"}) == "OK\tinteraction_plot");
    assert(regressionComparisonOpenDispatcher.dispatch({"MODEL_OPEN", "cars"}) == "OK");
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"MODEL_OPEN_DIAGNOSTIC", "cars", "residuals_fitted"}) == "OK\tlinear_diag");
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"MODEL_OPEN_POOLED", "cars", "Pooled", "distance", "all", "3", "pooled fit", "1", "speed",
         "TRUE", "10", "0", "1", "8", "0.5", "0.4", "8", "0.01", "20", "10", "20", "1.25",
         "1.1", "100", "101", "", "", "1", "1", "0", "0"}) == "OK\tglm:cars");
    assert(regressionComparisonOpenDispatcher.applicationState().linearModelFits().at("cars").n == 10);
    assert(regressionComparisonOpenDispatcher.applicationState().groupModels().at("cars").multipleImputation);
    assert((shownLinearModels == std::vector<std::string>{"cars", "cars", "cars"}));
    assert((refreshedLinearModelGroups == std::vector<std::string>{"cars", "cars"}));
    assert(requestedRegressionComparisons.back() == "cmp_open");
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"REGCMP_SET_TERM", "cmp_open", "1", "speed", "TRUE"}) == "OK");
    assert((regressionComparisonOpenDispatcher.applicationState().regressionComparisons().at("cmp_open")
                .models.front().includedTerms == std::vector<std::string>{"weight", "speed"}));
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"REGCMP_SET_TERM", "cmp_open", "1", "speed", "FALSE"}) == "OK");
    assert((regressionComparisonOpenDispatcher.applicationState().regressionComparisons().at("cmp_open")
                .models.front().includedTerms == std::vector<std::string>{"weight"}));
    assert(regressionComparisonOpenDispatcher.dispatch(
        {"REGCMP_OPEN", "cmp_bad", "cars", "distance", "bad", "TRUE", "0"}) ==
           "ERR invalid comparison scope");

    rlispstat::core::PlotModel *addedPlot = nullptr;
    std::string addedPlotDataSheet;
    rlispstat::core::CommandDispatcherServices addPlotServices;
    addPlotServices.ui.addPlot = [&](rlispstat::core::PlotModel *plot) { addedPlot = plot; };
    addPlotServices.ui.openDataSheet = [&](const std::string &group) { addedPlotDataSheet = group; };
    rlispstat::core::CommandDispatcher addPlotDispatcher(addPlotServices);
    assert(addPlotDispatcher.dispatch(
        {"ADD_PLOT", "plot_new", "new_group", "x", "y", "2", "1 2 1", "3 4 2",
         "VARS", "2", "x", "2", "1", "3", "y", "2", "2", "4", "VARMETA", "2",
         "x", "numeric", "y", "numeric"}) == "OK");
    assert(addedPlot && addedPlot->points.size() == 2 && addedPlotDataSheet == "new_group");
    rlispstat::core::PlotModel *firstAddedPlot = addedPlot;
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
    assert(addPlotDispatcher.dispatch(
        {"ADD_BOXPLOT", "box_new", "box_group", "x", "y", "Box", "TRUE", "TRUE", "TRUE", "2",
         "2\tA\t1", "4\tB\t2"}) == "OK");
    assert(addedPlot && addedPlot->kind == "boxplot" && addedPlot->boxplotPoints.size() == 2);
    assert((addedPlot->boxplotCategories == std::vector<std::string>{"A", "B"}));
    assert(addPlotDispatcher.dispatch(
        {"ADD_HISTOGRAM", "hist_new", "hist_group", "x", "Histogram", "TRUE", "FALSE", "2", "2",
         "0\t2", "2\t4", "1\t1\t1", "3\t2\t2"}) == "OK");
    assert(addedPlot && addedPlot->kind == "histogram" && addedPlot->histogramBins.size() == 2);
    assert((addedPlot->histogramBins[0].rows == std::vector<int>{1}));
    assert(addPlotDispatcher.dispatch(
        {"ADD_BARPLOT", "bar_new", "bar_group", "cat", "", "Bar", "count", "equal", "", "2", "0",
         "1", "A\t2\t100\t1\t1,2", "0"}) == "OK");
    assert(addedPlot && addedPlot->kind == "barplot" && addedPlot->barplotBins.size() == 1);
    assert(addedPlot->barplotBins.front().segments.front().level == "All");

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
    boxplot.variables = {{"x", {1.0, 2.0}}, {"y", {3.0, 4.0}}};
    boxplot.boxplotVariables = {"x"};
    int boxplotRedraws = 0;
    rlispstat::core::CommandDispatcher boxplotDispatcher({
        {},
        {
            {}, {},
            [&](const std::string &plotId, rlispstat::core::PlotModel &plot) {
                if (plotId != "box_1") return false;
                plot = boxplot;
                return true;
            }
        },
        {
            {}, {}, {},
            [&](const std::string &plotId) {
                assert(plotId == "box_1");
                ++boxplotRedraws;
            }
        },
        {},
        {
            {}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {},
            [&](const std::string &plotId,
                const rlispstat::core::CommandBoxplotMutation &mutation,
                std::string &message) {
                if (plotId != "box_1") return false;
                return mutation(boxplot, message);
            }
        }
    });
    assert(boxplotDispatcher.dispatch({"BOXPLOT_ADD_VARIABLE", "box_1", "y"}) == "OK");
    assert((boxplot.boxplotVariables == std::vector<std::string>{"x", "y"}));
    assert(boxplotDispatcher.dispatch({"BOXPLOT_REMOVE_VARIABLE", "box_1", "y"}) == "OK");
    assert((boxplot.boxplotVariables == std::vector<std::string>{"x"}));
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
    assert(boxplotRedraws == 6);

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
    interactionState.dependent = "outcome";
    interactionState.terms = {"dose", "condition", "dose:condition"};
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
    return 0;
}
