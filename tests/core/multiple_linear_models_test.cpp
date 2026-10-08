#include "../../src/core/command_dispatcher.h"
#include "../../src/core/main_r_task_model.h"

#include <cassert>
#include <string>
#include <vector>

using namespace rlispstat::core;

int main()
{
    PlotModel seed;
    seed.id = "cars_seed";
    seed.group = "cars";
    seed.variables = {{"speed", {1.0}}, {"distance", {2.0}}, {"weight", {3.0}}};
    seed.variableMeta = {{"speed", "numeric"}, {"distance", "numeric"},
                         {"weight", "numeric"}};
    CommandDispatcherServices services;
    services.queries.groupPlot = [seed](const std::string &group, PlotModel &out) {
        if (group != "cars") return false;
        out = seed;
        return true;
    };
    services.queries.groupSeed = services.queries.groupPlot;
    std::string refreshedModel;
    services.ui.refreshModelGroup = [&](const std::string &id) {
        refreshedModel = id;
    };
    CommandDispatcher dispatcher(services);
    auto &models = dispatcher.applicationState().groupModels();
    const std::string first = CreateLinearModelInstance(models, "cars");
    const std::string second = CreateLinearModelInstance(models, "cars");
    assert(first != second);
    assert(models.at(first).group == "cars" && models.at(second).group == "cars");
    assert(dispatcher.dispatch({"MODEL_SET_Y", first, "distance"}) == "OK");
    assert(dispatcher.dispatch({"MODEL_ADD_TERM", first, "speed"}) == "OK");
    assert(dispatcher.dispatch({"MODEL_SET_Y", second, "speed"}) == "OK");
    assert(dispatcher.dispatch({"MODEL_ADD_TERM", second, "weight"}) == "OK");
    assert(models.at(first).response == "distance");
    assert((models.at(first).terms == std::vector<std::string>{"speed"}));
    assert(models.at(second).response == "speed");
    assert((models.at(second).terms == std::vector<std::string>{"weight"}));

    MainRLinearGLMTask task;
    task.group = "cars";
    task.modelId = second;
    task.dependent = "speed";
    task.terms = {"weight"};
    task.requestIdentity = "second-fit";
    const std::string wire = EncodeMainRTask(task);
    assert(wire.find("FIT_ID_V1\tsecond-fit\tLINEAR_MODEL_ID_V1\t" + second) !=
           std::string::npos);
    models.at(second).rFitPending = true;
    models.at(second).lastRFitSignature = "second-fit";
    assert(dispatcher.dispatch({"MODEL_UPDATE_ERROR", "cars", "second fit failed",
                                "LINEAR_RESULT_V1", "second-fit",
                                "LINEAR_MODEL_ID_V1", second}) == "OK\tglm:" + second);
    assert(refreshedModel == second);
    assert(dispatcher.applicationState().linearModelFits().at(second).warning ==
           "second fit failed");
    assert(dispatcher.applicationState().linearModelFits().count(first) == 0);
    return 0;
}
