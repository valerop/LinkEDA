#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>

int main()
{
    @autoreleasepool {
        DataFrameModel alien;
        alien.group = "Alien-type-smoke";
        alien.rows = 3;
        DataColumn planet;
        planet.name = "planet";
        planet.type = "factor";
        planet.values = {"Aurelia", "Borealis", "Cygnus"};
        DataColumn happy;
        happy.name = "happy";
        happy.type = "factor";
        happy.values = {"no", "yes", "no"};
        alien.columns = {planet, happy};
        assert(MacCommandDispatcher().applicationState().registerDataset(alien));

        const std::string modelId =
            rlispstat::core::CreateLinearModelInstance(g_groupModels, alien.group);
        GroupModelState &model = g_groupModels.at(modelId);
        model.terms = {"planet", "happy"};
        model.termTypes = {{"planet", "numeric"}, {"happy", "numeric"}};

        GroupModelState &synchronized = EnsureGroupModelUnlocked(modelId);
        const auto types = rlispstat::core::EffectiveModelSpecificationTermTypes(synchronized);
        assert(types.at("planet") == "factor");
        assert(types.at("happy") == "factor");
        assert(!synchronized.multipleImputation);

        // Report refreshes must resolve a plot and selection through the
        // dataset ID too; the model window's ID has no dataset or seed plot.
        PlotModel seed;
        seed.id = "alien-type-seed";
        seed.group = alien.group;
        g_plots[seed.id] = &seed;
        synchronized.response = "planet";
        synchronized.terms = {"happy", "planet:happy"};
        synchronized.isStale = false;
        GLMFitSummary fit;
        fit.ok = true;
        g_precomputedLinearModelFits[modelId] = fit;
        RegressionInteractionDerivedLink link;
        link.sourceModelKind = "linear_single";
        link.sourceModelId = modelId;
        link.group = alien.group;
        link.term = "planet:happy";
        InteractionDerivedRefreshSnapshot snapshot;
        std::string message;
        assert(BuildInteractionDerivedRefreshSnapshot(link, snapshot, message));
        assert(snapshot.seed.group == alien.group);
        assert(snapshot.rBacked);
        g_precomputedLinearModelFits.erase(modelId);
        g_plots.erase(seed.id);
    }
}
