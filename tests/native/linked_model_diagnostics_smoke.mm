#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>

int main()
{
    @autoreleasepool {
        [NSApplication sharedApplication];
        using namespace rlispstat::core;
        GeneralizedGLMState source;
        source.id = "linked-diagnostic-source";
        source.group = "linked-diagnostic-data";
        source.family = "poisson";
        source.link = "log";
        source.ok = true;
        source.modelVersion = 1;
        source.fitVersion = 1;
        source.diagnosticsVersion = 1;
        for (int row = 1; row <= 12; ++row) {
            GeneralizedDiagnosticRow diagnostic;
            diagnostic.row = row;
            diagnostic.observed = row;
            diagnostic.fitted = row + 0.5;
            diagnostic.rawResidual = row - 6.0;
            // Theoretical quantiles are fixture values standing in for the
            // row-wise quantiles supplied by R, not a native calculation.
            diagnostic.qqTheoreticalQuantiles["raw"] = row / 10.0;
            source.diagnostics.push_back(diagnostic);
        }
        g_generalizedGLMs[source.id] = source;

        for (const std::string kind : {
                 "residual_histogram", "residuals_fitted", "normal_qq"}) {
            auto *plot = new PlotModel();
            plot->id = "linked-diagnostic-" + kind;
            plot->group = source.group;
            plot->glmModelId = source.id;
            plot->glmDiagnosticKind = kind;
            plot->isGLMDiagnostic = true;
            plot->kind = kind == "residual_histogram" ? "histogram" : "scatter";
            plot->displayedResidualType = "raw";
            g_plots[plot->id] = plot;
        }

        RefreshModelDiagnosticsForModelNow(source.id);
        auto *histogram = g_plots.at("linked-diagnostic-residual_histogram");
        auto *scatter = g_plots.at("linked-diagnostic-residuals_fitted");
        auto *qq = g_plots.at("linked-diagnostic-normal_qq");
        assert(histogram->histogramPoints.size() == 12);
        assert(histogram->histogramPoints.front().x == -5.0);
        assert(scatter->points.size() == 12);
        assert(qq->points.size() == 12);

        g_generalizedGLMs[source.id].rFitPending = true;
        g_generalizedGLMs[source.id].modelVersion = 2;
        RefreshModelDiagnosticsForModelNow(source.id);
        assert(histogram->histogramPoints.empty());
        assert(histogram->histogramBins.empty());
        assert(scatter->points.empty());
        assert(qq->points.empty());

        auto &updated = g_generalizedGLMs[source.id];
        updated.rFitPending = false;
        updated.fitVersion = 2;
        updated.diagnosticsVersion = 2;
        for (auto &row : updated.diagnostics) row.rawResidual += 10.0;
        RefreshModelDiagnosticsForModelNow(source.id);
        assert(histogram->histogramPoints.size() == 12);
        assert(histogram->histogramPoints.front().x == 5.0);
        assert(histogram->displayedFitVersion == 2);
        assert(scatter->points.size() == 12);
        assert(scatter->points.front().y == 5.0);
        assert(qq->points.size() == 12);

        for (const std::string kind : {
                 "residual_histogram", "residuals_fitted", "normal_qq"}) {
            const std::string id = "linked-diagnostic-" + kind;
            delete g_plots.at(id);
            g_plots.erase(id);
        }
        g_generalizedGLMs.erase(source.id);
    }
}
