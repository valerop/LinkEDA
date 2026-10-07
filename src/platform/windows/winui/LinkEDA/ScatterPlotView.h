#pragma once

#include "ApplicationMenu.h"
#include "../../../../core/barplot_model.h"
#include "../../../../core/boxplot_model.h"
#include "../../../../core/command_model.h"
#include "../../../../core/format_model.h"
#include "../../../../core/dimensionality_model.h"
#include "../../../../core/histogram_model.h"
#include "../../../../core/plot_geometry.h"
#include "../../../../core/scatter_matrix_model.h"
#include "../../../../core/scatterplot_model.h"
#include "../../../../core/session_controller.h"
#include "../../../../core/selection_model.h"
#include "../../../../core/trellis_scatterplot_model.h"
#include "../../../../core/window_note_model.h"

#include <winrt/Microsoft.UI.Dispatching.h>
#include <winrt/Microsoft.UI.Xaml.h>
#include <winrt/Microsoft.UI.Xaml.Controls.h>
#include <winrt/Microsoft.UI.Xaml.Shapes.h>

#include <functional>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace winrt::LinkEDA::implementation
{
    class ScatterPlotView final
        : public std::enable_shared_from_this<ScatterPlotView>
    {
    public:
        using SelectionCallback = std::function<void(
            std::string const&, std::set<int> const&, ::rlispstat::core::SelectionMode)>;
        using ClosedCallback = std::function<void()>;
        using CommandCallback = std::function<void(std::vector<std::string> const&)>;

        static std::shared_ptr<ScatterPlotView> Create();
        static std::shared_ptr<ScatterPlotView> CreateSnapshotHost(
            ::rlispstat::core::PlotModel const& plot,
            std::string themeName);

        void SetSelectionCallback(SelectionCallback callback);
        void SetClosedCallback(ClosedCallback callback);
        void SetCommandCallback(CommandCallback callback);
        void SetApplicationCommands(std::shared_ptr<ApplicationCommandRegistry> registry,
                                    std::string token);
        void SetAnalysisScope(::rlispstat::core::AnalysisScope scope);
        void SetExcludedRows(std::set<int> rows);
        void SetDataFrame(::rlispstat::core::DataFrameModel const* dataframe);
        void RefreshVariableMetadata(::rlispstat::core::DataFrameModel const* dataframe);
        void Show(::rlispstat::core::SessionPlot const& plot);
        void Show(::rlispstat::core::PlotModel& plot);
        void SetVisualStyle(std::string themeName,
                            std::vector<std::pair<int, std::string>> const& pointColors);
        void ApplyGlobalTheme(std::string themeName);
        void SetLabelState(std::string labelColumn,
                           std::map<int, std::string> rowLabels);
        void RefreshSelection(std::set<int> const& selectedRows);
        void Reset();
        void Activate();
        void Close();
        void ShowInformation(std::string const& title, std::string const& message);
        ::rlispstat::core::PlotModel ExportModelSnapshot() const;
        winrt::Microsoft::UI::Xaml::FrameworkElement ContentRoot() const;
        void ResizeSnapshotHost(double width, double height);

        std::string const& PlotId() const;
        std::string const& Group() const;
        winrt::Microsoft::UI::Dispatching::DispatcherQueue DispatcherQueue() const;

    private:
        explicit ScatterPlotView(bool embedded = false);
        void Initialize();
        void AttachClosedHandler();
        void BeginSelection(double x, double y, ::rlispstat::core::SelectionMode mode);
        bool RowSelectionGestureEnabled() const;
        bool ViewportNavigationGestureEnabled() const;
        void UpdateSelection(double x, double y);
        std::set<int> RowsForFixedBrush(double x, double y) const;
        void PublishBrushSelection(double x, double y);
        void CompleteSelection(double x, double y,
                               ::rlispstat::core::SelectionMode mode);
        void ApplyVisibleSelection(::rlispstat::core::SelectionMode mode);
        void ClearSelection();
        void ConfigureContextMenu();
        void DispatchPlotCommand(std::vector<std::string> command);
        void DispatchEncodedPlotCommand(std::string const& encodedCommand);
        winrt::Microsoft::UI::Xaml::Controls::MenuFlyoutItemBase
            CreateBarplotMenuItem(
                ::rlispstat::core::BarplotMenuOption const& option,
                bool showCheck = false);
        void SelectBarplotRows(
            std::vector<int> const& rows,
            ::rlispstat::core::SelectionMode mode =
                ::rlispstat::core::SelectionMode::Replace);
        void RefreshContextSelectionState();
        void RefreshAnalysisScopeMenuState();
        void AttachAxisVariableMenu(
            winrt::Microsoft::UI::Xaml::Controls::TextBlock const& label,
            bool xAxis);
        void AttachBoxplotGroupingVariableMenu(
            winrt::Microsoft::UI::Xaml::Controls::TextBlock const& label,
            std::string const& variable,
            bool trellis);
        void AttachBoxplotSelection(
            winrt::Microsoft::UI::Xaml::Controls::TextBlock const& label,
            std::vector<int> rows);
        winrt::Microsoft::UI::Xaml::Controls::MenuFlyoutSubItem
            CreateVariableTypeMenu(std::string const& variable);
        void AttachVariableTypeMenu(
            winrt::Microsoft::UI::Xaml::Controls::TextBlock const& label,
            std::string const& variable);
        void AttachNumericVariableReplacementMenu(
            winrt::Microsoft::UI::Xaml::Controls::TextBlock const& label,
            std::string const& currentVariable,
            std::string const& replacementToken,
            std::vector<std::string> const& currentVariables,
            std::string const& command);
        void SetLocalTheme(std::string themeName);
        void AttachTrellisConditionMenu(
            winrt::Microsoft::UI::Xaml::FrameworkElement const& target,
            std::vector<std::size_t> conditionIndices);
        void SetNotesVisible(bool visible);
        bool SupportsPointSizeControl() const;
        void AddStickyNote();
        void RenderStickyNotes();
        void ActivateStickyNote(int index);
        bool SupportsIncrementalPointUpdates() const;
        ::rlispstat::core::ScatterplotRenderPlan BuildCurrentScatterRenderPlan() const;
        std::vector<winrt::Microsoft::UI::Xaml::UIElement> CreatePointVisuals(
            ::rlispstat::core::ScatterplotPointDrawItem const& item,
            ::rlispstat::core::PlotThemeStyleSpec const& theme);
        std::vector<winrt::Microsoft::UI::Xaml::UIElement> CreateSelectionRingVisuals(
            ::rlispstat::core::Point const& point,
            double pointRadius,
            ::rlispstat::core::PlotThemeStyleSpec const& theme,
            ::rlispstat::core::PlotRGBA color) const;
        void RefreshPointVisuals(std::set<int> const& rows);
        void RefreshScatterOverlayVisuals();
        void RefreshInteractionSeriesVisuals();
        std::string ExportSvgDocument() const;
        winrt::Windows::Foundation::IAsyncOperation<
            winrt::Windows::Storage::Streams::InMemoryRandomAccessStream>
            RenderSvgToPngAsync(std::string svg, double scale);
        winrt::Windows::Foundation::IAsyncOperation<bool>
            RenderSvgToPdfAsync(std::string svg, std::wstring path);
        winrt::fire_and_forget CopyPlotAsSvgAsync();
        winrt::fire_and_forget CopyPlotAsPngAsync();
        winrt::fire_and_forget CopyPlotRichAsync(bool preferVector);
        winrt::fire_and_forget SavePlotAsync(std::string format);
        void RenderPlot(double width, double height);
        void RenderBoxplot(::rlispstat::core::Rect const& plotRect,
                           ::rlispstat::core::PlotThemeStyleSpec const& theme);
        void RefreshBoxplotSelectionVisuals(std::set<int> const& rows);
        void RenderHistogram(::rlispstat::core::Rect const& plotRect,
                             ::rlispstat::core::PlotThemeStyleSpec const& theme);
        void RenderBarplot(double width, double height,
                           ::rlispstat::core::PlotThemeStyleSpec const& theme);
        void RefreshBarplotSelectionVisuals(
            ::rlispstat::core::PlotThemeStyleSpec const& theme,
            std::set<int> const* changedRows = nullptr);
        ::rlispstat::core::DataViewport diagnosticSeriesViewport_{};
        ::rlispstat::core::Rect diagnosticSeriesRect_{};
        winrt::Microsoft::UI::Xaml::Controls::Canvas timeSeriesSelectionCanvas_{ nullptr };
        std::vector<winrt::Microsoft::UI::Xaml::Shapes::Polyline>
            timeSeriesLineVisuals_;
        std::map<int, std::vector<winrt::Microsoft::UI::Xaml::Shapes::Ellipse>>
            timeSeriesPointVisuals_;
        struct TimeSeriesLegendVisual
        {
            std::size_t seriesIndex = 0;
            winrt::Microsoft::UI::Xaml::Shapes::Line sample{ nullptr };
            winrt::Microsoft::UI::Xaml::Controls::TextBlock label{ nullptr };
        };
        std::vector<TimeSeriesLegendVisual> timeSeriesLegendVisuals_;
        void RenderTimeSeries(::rlispstat::core::Rect const& plotRect,
                              ::rlispstat::core::PlotThemeStyleSpec const& theme);
        void RefreshTimeSeriesSelectionVisuals(std::set<int> const& rows);
        void RenderScatterMatrix(double width, double height,
                                 ::rlispstat::core::PlotThemeStyleSpec const& theme);
        void RefreshScatterMatrixSelectionVisuals(std::set<int> const& rows);
        void RefreshScatterMatrixRings(
            std::vector<::rlispstat::core::ScatterMatrixPointDrawItem> const& points,
            ::rlispstat::core::PlotThemeStyleSpec const& theme);
        void RenderTrellis(double width, double height,
                           ::rlispstat::core::PlotThemeStyleSpec const& theme);
        void RefreshTrellisScatterSelectionVisuals(std::set<int> const& rows);
        void RefreshTrellisBoxplotSelectionVisuals(std::set<int> const& rows);
        void RefreshTrellisScatterRings(
            std::vector<::rlispstat::core::TrellisPanelRenderPlan> const& plans,
            ::rlispstat::core::PlotThemeStyleSpec const& theme);
        void RenderColorLegend(double width, double height,
                               ::rlispstat::core::PlotThemeStyleSpec const& theme);

        winrt::Microsoft::UI::Xaml::Window window_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::Grid root_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::Canvas pointsCanvas_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::Canvas plotCanvas_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::Canvas scatterOverlayCanvas_{ nullptr };
        struct InteractionSeriesVisual
        {
            std::vector<winrt::Microsoft::UI::Xaml::Shapes::Line> intervalLines;
            std::vector<winrt::Microsoft::UI::Xaml::Shapes::Polygon> confidenceBands;
            winrt::Microsoft::UI::Xaml::Shapes::Polyline connectedLine{ nullptr };
            std::vector<winrt::Microsoft::UI::Xaml::Shapes::Ellipse> markers;
            std::vector<::rlispstat::core::Point> markerCenters;
            winrt::Microsoft::UI::Xaml::Shapes::Line legendSample{ nullptr };
            winrt::Microsoft::UI::Xaml::Shapes::Ellipse legendMarker{ nullptr };
            ::rlispstat::core::Point legendMarkerCenter;
            winrt::Microsoft::UI::Xaml::Controls::TextBlock legendLabel{ nullptr };
        };
        std::vector<InteractionSeriesVisual> interactionSeriesVisuals_;
        winrt::Microsoft::UI::Xaml::Controls::Canvas barplotSelectionCanvas_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::Canvas annotationCanvas_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::Border smoothSpanControl_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::TextBlock smoothSpanLabel_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::Slider smoothSpanSlider_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::Border pointSizeControl_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::TextBlock pointSizeLabel_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::Slider pointSizeSlider_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::WebView2 exportWebView_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::Border notesPanel_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::TextBox notesText_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::CheckBox includeNotes_{ nullptr };
        winrt::Microsoft::UI::Xaml::Shapes::Rectangle selectionRectangle_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::TextBlock plotTitle_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::TextBlock emptyLabel_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::TextBlock xAxisLabel_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::TextBlock yAxisLabel_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::MenuFlyoutSubItem selectionColorsMenu_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::MenuFlyoutItem copySelectedRowsItem_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::MenuFlyoutItem excludeSelectedPlotItem_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::MenuFlyoutItem restorePlotScopeItem_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::ToggleMenuFlyoutItem selectedFutureScopeItem_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::ToggleMenuFlyoutItem visibleFutureScopeItem_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::ToggleMenuFlyoutItem allFutureScopeItem_{ nullptr };
        winrt::Microsoft::UI::Dispatching::DispatcherQueueTimer resizeTimer_{ nullptr };
        std::shared_ptr<ApplicationCommandRegistry> applicationCommands_;
        std::string applicationWindowToken_;
        struct RenderedPoint {
            int row = 0;
            double x = 0.0;
            double y = 0.0;
        };
        std::vector<RenderedPoint> renderedPoints_;
        std::vector<::rlispstat::core::ScatterplotCaseGeometry> scatterGeometry_;
        std::map<int, std::vector<winrt::Microsoft::UI::Xaml::UIElement>> pointVisuals_;
        uint32_t pointVisualStartIndex_ = 0;
        ::rlispstat::core::DataViewport renderedViewport_;
        ::rlispstat::core::Rect renderedPlotRect_;
        double pendingPlotWidth_ = 360.0;
        double pendingPlotHeight_ = 280.0;
        ::rlispstat::core::SessionPlot currentPlot_;
        ::rlispstat::core::PlotModel* currentModel_ = nullptr;
        std::unique_ptr<::rlispstat::core::PlotModel> ownedSnapshotModel_;
        ::rlispstat::core::DataFrameModel dataFrame_;
        bool hasDataFrame_ = false;
        std::vector<::rlispstat::core::BoxplotCaseGeometry> boxplotGeometry_;
        std::vector<std::string> boxplotCategoryOrder_;
        winrt::Microsoft::UI::Xaml::Controls::Canvas boxplotSelectionCanvas_{ nullptr };
        std::map<int, std::vector<winrt::Microsoft::UI::Xaml::Shapes::Ellipse>>
            boxplotPointVisuals_;
        ::rlispstat::core::HistogramLayout histogramLayout_;
        std::vector<std::vector<::rlispstat::core::CaseId>> histogramBinRows_;
        ::rlispstat::core::BarplotLayout barplotLayout_;
        std::vector<::rlispstat::core::BarplotBinSummary> barplotBins_;
        struct BarplotSelectionLayer
        {
            winrt::Microsoft::UI::Xaml::Controls::Canvas canvas{ nullptr };
            ::rlispstat::core::Rect localRect;
            std::vector<int> rows;
            std::string colorOverride;
            winrt::Microsoft::UI::Xaml::Controls::TextBlock valueLabel{ nullptr };
        };
        std::vector<BarplotSelectionLayer> barplotSelectionLayers_;
        std::map<int, std::vector<std::size_t>> barplotSelectionLayersByRow_;
        ::rlispstat::core::TrellisScatterplotLayout trellisLayout_;
        std::vector<::rlispstat::core::BoxplotCaseGeometry> trellisBoxplotGeometry_;
        struct TrellisPointVisual
        {
            winrt::Microsoft::UI::Xaml::Shapes::Ellipse dot{ nullptr };
            winrt::Microsoft::UI::Xaml::Controls::TextBlock label{ nullptr };
        };
        std::map<int, std::vector<TrellisPointVisual>> trellisPointVisuals_;
        std::map<std::string, winrt::Microsoft::UI::Xaml::Shapes::Path>
            trellisRingPaths_;
        winrt::Microsoft::UI::Xaml::Controls::Canvas trellisSelectionCanvas_{ nullptr };
        std::vector<std::vector<winrt::Microsoft::UI::Xaml::Shapes::Polyline>>
            trellisTimeSeriesLineVisuals_;
        ::rlispstat::core::ScatterMatrixLayout scatterMatrixLayout_;
        std::vector<::rlispstat::core::ScatterMatrixCaseGeometry> scatterMatrixGeometry_;
        struct ScatterMatrixPointVisual
        {
            winrt::Microsoft::UI::Xaml::Shapes::Ellipse dot{ nullptr };
            winrt::Microsoft::UI::Xaml::Controls::TextBlock label{ nullptr };
            bool selected = false;
        };
        std::map<int, std::vector<ScatterMatrixPointVisual>> scatterMatrixPointVisuals_;
        std::map<std::string, winrt::Microsoft::UI::Xaml::Shapes::Path>
            scatterMatrixRingPaths_;
        std::set<int> selectedRows_;
        std::set<int> excludedRows_;
        std::vector<int> visibleFutureScopeRows_;
        ::rlispstat::core::AnalysisScope analysisScope_;
        std::map<int, std::string> pointColors_;
        std::map<int, std::string> rowLabels_;
        std::string labelColumn_;
        std::string renderedLabelDisplayMode_ = "none";
        bool labelRefreshQueued_ = false;
        ::rlispstat::core::WindowNote note_;
        std::vector<::rlispstat::core::WindowStickyNote> stickyNotes_;
        std::vector<winrt::Microsoft::UI::Xaml::Controls::Border> stickyVisuals_;
        bool updatingNotes_ = false;
        bool updatingSmoothSpan_ = false;
        bool updatingPointSize_ = false;
        bool colorLegendDragging_ = false;
        bool colorLegendMoved_ = false;
        double colorLegendDragStartX_ = 0.0;
        double colorLegendDragStartY_ = 0.0;
        double colorLegendOriginalX_ = 0.0;
        double colorLegendOriginalY_ = 0.0;
        bool interactionLegendDragging_ = false;
        bool interactionLegendMoved_ = false;
        double interactionLegendDragStartX_ = 0.0;
        double interactionLegendDragStartY_ = 0.0;
        double interactionLegendAppliedDx_ = 0.0;
        double interactionLegendAppliedDy_ = 0.0;
        std::set<int> interactionLegendPressedRows_;
        ::rlispstat::core::SelectionMode interactionLegendSelectionMode_ =
            ::rlispstat::core::SelectionMode::Replace;
        double pointSizeScale_ = 1.0;
        bool pointSizeControlVisible_ = false;
        std::string themeName_ = "publication";
        std::string globalThemeName_ = "publication";
        std::optional<std::string> localThemeOverride_;
        SelectionCallback selectionCallback_;
        ClosedCallback closedCallback_;
        CommandCallback commandCallback_;
        std::string plotId_;
        std::string group_;
        double selectionStartX_ = 0.0;
        double selectionStartY_ = 0.0;
        ::rlispstat::core::SelectionMode selectionMode_ =
            ::rlispstat::core::SelectionMode::Replace;
        bool selecting_ = false;
        std::set<int> brushSelectionBase_;
        std::set<int> brushPublishedSelection_;
        bool brushSelectionPublished_ = false;
        int stickyDragIndex_ = -1;
        int stickyAnchorDragIndex_ = -1;
        int activeStickyIndex_ = -1;
        double stickyDragStartX_ = 0.0;
        double stickyDragStartY_ = 0.0;
        double stickyOriginalX_ = 0.0;
        double stickyOriginalY_ = 0.0;
        bool closed_ = false;
        bool embedded_ = false;
    };
}
