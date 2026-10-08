// Compile this translation unit in place of linkeda_macos_app.mm and the
// native entry point, linking the shared core and other platform support files.
#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>
#include <objc/runtime.h>
static NSMenu *capturedMenu;
static void CaptureMenu(id, SEL, NSMenu *menu, NSEvent *, NSView *) { capturedMenu = menu; }
static NSMenuItem *FindCommand(NSMenu *menu, NSString *command) {
    for (NSMenuItem *item in [menu itemArray]) {
        if ([[item representedObject] isKindOfClass:[NSString class]] &&
            [(NSString *)[item representedObject] isEqualToString:command]) return item;
        if (NSMenuItem *found = FindCommand([item submenu], command)) return found;
    }
    return nil;
}
static NSMenuItem *FindTitle(NSMenu *menu, NSString *title) {
    for (NSMenuItem *item in [menu itemArray]) {
        if ([[item title] isEqualToString:title]) return item;
        if (NSMenuItem *found = FindTitle([item submenu], title)) return found;
    }
    return nil;
}
int main() {
    @autoreleasepool {
        [NSApplication sharedApplication];
        PlotModel plot; plot.id = "histogram-menu-test"; plot.group = "hist-data";
        plot.kind = "histogram"; plot.xLabel = "Value";
        NumericVariable variable; variable.name = "Value"; plot.variables.push_back(variable);
        variable.name = "Other"; plot.variables.push_back(variable);
        std::vector<double> values;
        for (int i = 1; i <= 64; ++i) {
            const double value = i == 64 ? 100 : .1 * i;
            values.push_back(value);
            plot.histogramPoints.push_back({value, i, 0});
        }
        rlispstat::core::RebinHistogram(plot, 10);
        MacCommandDispatcher().applicationState().plots()[plot.id] = &plot;
        HistogramView *view = [[HistogramView alloc] initWithModel:&plot];
        g_histogramViews.push_back(view);
        NSMenu *axis = [view menuForEvent:nil];
        NSMenu *context = [view menuForEvent:nil];
        for (const auto &rule : rlispstat::core::HistogramBinningRuleMenuOptions()) {
            assert(FindCommand(axis, ToNSString(rule.command)));
            assert(FindCommand(context, ToNSString(rule.command)));
            DispatchCommand(&plot, rule.command);
            assert(plot.histogramBins.size() == static_cast<size_t>(HistogramBinCountForRule(values, rule.value)));
            size_t cases = 0; for (const auto &bin : plot.histogramBins) cases += bin.rows.size();
            assert(cases == 64);
        }
        DispatchCommand(&plot, "HIST_SET_BINS|15");
        assert(plot.histogramBins.size() == 15);
        assert(FindCommand([view menuForEvent:nil], @"HIST_SET_BINS|15").state == NSControlStateValueOn);
        assert(FindCommand(axis, @"HIST_SET_BINS"));
        const NSRect pr = [view plotRect];
        assert(NSPointInRect(NSMakePoint(NSMidX(pr), NSMaxY(pr) + 10), [view histogramAxisRect]));
        assert(!NSPointInRect(NSMakePoint(NSMidX(pr), NSMidY(pr)), [view histogramAxisRect]));
        Method popup = class_getClassMethod([NSMenu class], @selector(popUpContextMenu:withEvent:forView:));
        IMP originalPopup = method_setImplementation(popup, (IMP)CaptureMenu);
        NSPoint axisPoint = [view convertPoint:NSMakePoint(NSMidX(pr), NSMaxY(pr) + 10) toView:nil];
        NSEvent *primary = [NSEvent mouseEventWithType:NSEventTypeLeftMouseDown location:axisPoint
            modifierFlags:0 timestamp:0 windowNumber:0 context:nil eventNumber:0 clickCount:1 pressure:1];
        [view mouseDown:primary];
        assert(FindCommand(capturedMenu, @"CHANGE_X_VARIABLE|Value").state == NSControlStateValueOn);
        assert(FindCommand(capturedMenu, @"CHANGE_X_VARIABLE|Other"));
        assert(!FindCommand(capturedMenu, @"HIST_SET_BINS"));
        NSEvent *secondary = [NSEvent mouseEventWithType:NSEventTypeRightMouseDown location:axisPoint
            modifierFlags:0 timestamp:0 windowNumber:0 context:nil eventNumber:1 clickCount:1 pressure:1];
        [view rightMouseDown:secondary];
        assert(FindCommand(capturedMenu, @"HIST_SET_BINS"));
        assert(!FindCommand(capturedMenu, @"CHANGE_X_VARIABLE|Other"));
        assert(!view.dragging);
        method_setImplementation(popup, originalPopup);
        plot.isGLMDiagnostic = true; plot.glmDiagnosticKind = "residual_histogram";
        NSMenu *residualAxis = [view menuForEvent:nil];
        assert([[view variableMenu] numberOfItems] > 0);
        assert(![residualAxis itemWithTitle:@"Change variable"]);
        assert(FindCommand(residualAxis, @"HIST_SET_BINNING_RULE|fd"));
        PlotModel trellis; trellis.kind = "trellis_scatterplot";
        trellis.trellisSpecification.histogramBinCount = 10;
        NSMenu *trellisMenu = [[NSMenu alloc] init]; AddHistogramBinningMenuItems(trellisMenu, &trellis);
        for (const auto &rule : rlispstat::core::HistogramBinningRuleMenuOptions())
            assert(FindCommand(trellisMenu, ToNSString("TRELLIS_SCATTERPLOT_SET_HISTOGRAM_BINS|" + rule.value)));
        DataFrameModel data; data.group = "trellis-hist-data"; data.rows = 64;
        DataColumn column; column.name = "Value"; column.type = "numeric";
        for (double value : values) column.values.push_back(std::to_string(value));
        data.columns.push_back(column);
        column.name = "Other";
        data.columns.push_back(column);
        MacCommandDispatcher().applicationState().datasets().registerDataset(data);
        trellis.id = "trellis-hist-test"; trellis.group = data.group;
        trellis.trellisSpecificationInitialized = true;
        trellis.trellisSpecification.plotType = TrellisPlotType::Histogram;
        trellis.trellisSpecification.xVariableId = "Value";
        std::string error;
        assert(rlispstat::core::RebuildTrellisPlotFromDataFrame(trellis, data, &error));
        MacCommandDispatcher().applicationState().plots()[trellis.id] = &trellis;
        for (const auto &rule : rlispstat::core::HistogramBinningRuleMenuOptions()) {
            DispatchCommand(&trellis, "TRELLIS_SCATTERPLOT_SET_HISTOGRAM_BINS|" + rule.value);
            assert(trellis.trellisSpecification.histogramBinCount ==
                static_cast<size_t>(HistogramBinCountForRule(values, rule.value)));
            assert(trellis.points.size() == 64);
        }
        PlotModel scatterTrellis;
        scatterTrellis.id = "trellis-scatter-smoothing-test";
        scatterTrellis.group = data.group;
        scatterTrellis.kind = "trellis_scatterplot";
        scatterTrellis.trellisSpecificationInitialized = true;
        scatterTrellis.trellisSpecification.plotType = TrellisPlotType::Scatter;
        scatterTrellis.trellisSpecification.xVariableId = "Value";
        scatterTrellis.trellisSpecification.yVariableId = "Other";
        assert(rlispstat::core::RebuildTrellisPlotFromDataFrame(scatterTrellis, data, &error));
        MacCommandDispatcher().applicationState().plots()[scatterTrellis.id] = &scatterTrellis;
        ScatterView *scatterView = [[ScatterView alloc] initWithModel:&scatterTrellis];
        assert([scatterView.smoothSpanControl isHidden]);
        NSMenuItem *showSlider = FindTitle([scatterView menuForEvent:nil], @"Show Smoothing Slider");
        assert(showSlider && [showSlider target] == scatterView);
        assert([showSlider action] == @selector(toggleSmoothSpanControl:));
        assert([NSApp sendAction:[showSlider action] to:[showSlider target] from:showSlider]);
        assert(scatterTrellis.smoothShowSpanSlider && ![scatterView.smoothSpanControl isHidden]);
        [scatterView.smoothSpanSlider setDoubleValue:1.10];
        [scatterView smoothSpanChanged:scatterView.smoothSpanSlider];
        assert(std::abs(scatterTrellis.smoothSpan - 1.10) < 0.001);
        NSMenuItem *hideSlider = FindTitle([scatterView menuForEvent:nil], @"Hide Smoothing Slider");
        assert(hideSlider && [NSApp sendAction:[hideSlider action] to:[hideSlider target] from:hideSlider]);
        assert(!scatterTrellis.smoothShowSpanSlider && [scatterView.smoothSpanControl isHidden]);
        MacCommandDispatcher().applicationState().plots().erase(scatterTrellis.id);
        [scatterView release];
        MacCommandDispatcher().applicationState().plots().erase(trellis.id);
        g_histogramViews.clear();
        MacCommandDispatcher().applicationState().plots().erase(plot.id);
        [view release];
    }
}
