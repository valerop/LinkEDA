#include "../../src/core/application_state.h"
#include "../../src/core/command_dispatcher.h"

#include <cassert>
#include <string>
#include <vector>

int main()
{
    rlispstat::core::ApplicationState state;
    rlispstat::core::DataFrameModel data;
    data.group = "matrix_data";
    data.rows = 2;
    data.columns.push_back({"x", "numeric", "", "", -1, {"1", "2"}});
    data.columns.push_back({"y", "numeric", "", "", -1, {"3", "5"}});
    assert(state.registerDataset(data));

    rlispstat::core::PlotModel plot;
    plot.id = "matrix_plot";
    plot.group = data.group;
    plot.kind = "scatter_matrix";
    plot.title = "Scatterplot matrix";
    plot.variables = {{"x", {1.0, 2.0}}, {"y", {3.0, 5.0}}};
    plot.scatterMatrixVariables = {"x", "y"};
    plot.overlays.push_back({1, "lm", "all", true});
    rlispstat::core::RefreshBasicPlotCodeReference(state, plot);
    const auto *reference = state.outputCodeReference(plot.id);
    assert(reference);
    assert((reference->provenance.verificationVariables ==
            std::vector<std::string>{"x", "y"}));
    const std::string &code = reference->provenance.verificationRCode.at("plot");
    assert(code.find("ggplot2::facet_grid") != std::string::npos);
    assert(code.find("method = \"lm\", formula = y ~ x") != std::string::npos);
    assert(code.find("analysis_data[[x_name]]") != std::string::npos);
    return 0;
}
