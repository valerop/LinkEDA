#pragma once

#include "../../../../core/dataset_model.h"
#include "../../../../core/correlation_model.h"
#include "../../../../core/mean_comparison_model.h"
#include "../../../../core/glm_model.h"
#include "../../../../core/dimensionality_model.h"
#include "../../../../core/scale_analysis_model.h"
#include "../../../../core/dendrogram_model.h"
#include "../../../../core/mixed_model.h"
#include "../../../../core/model_trellis_model.h"
#include "../../../../core/table1_model.h"
#include "../../../../core/command_model.h"
#include "../../../../core/window_note_model.h"
#include "../../../../core/analysis_initialization.h"

#include <functional>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

namespace winrt::rlispstatWinUI::implementation
{
    class TableExportService;
    struct ScatterplotSpecification
    {
        std::string group;
        std::string xVariable;
        std::string yVariable;
        std::string title;
        ::rlispstat::core::AnalysisScope dataScope;
        bool dataScopeCaptured = false;
    };

    struct DescriptiveStatisticsSpecification
    {
        std::string requestId;
        std::string group;
        std::vector<std::string> variables;
        std::string groupVariable;
        bool includeMissing = true;
        bool showP = true;
        bool showTest = true;
        bool showN = true;
        std::string ordinalAs = "ordinal";
        bool useSelection = false;
        std::vector<int> selectedRows;
        std::string scopeDescription = "All observations";
    };

    struct ContingencyTableSpecification
    {
        std::string group;
        std::string rowVariable;
        std::string columnVariable;
    };

    struct MissingDataImputationSpecification
    {
        std::string group;
        std::vector<std::string> imputeVariables;
        std::vector<std::string> predictorVariables;
        std::map<std::string, std::string> methods;
        int imputations = 5;
        int iterations = 5;
        std::string seed;
        bool openDataSheet = true;
    };

    struct RDataAssignmentSpecification
    {
        std::string group;
        std::string objectName;
        bool selectedRowsOnly = false;
        bool replaceExisting = false;
    };

    enum class PlotWorkflowKind
    {
        Trellis,
        TimeSeries,
        ScatterMatrix,
        ParallelCoordinates,
        Boxplot,
        Histogram,
        BarChart
    };

    struct PlotWorkflowSpecification
    {
        PlotWorkflowKind kind = PlotWorkflowKind::Histogram;
        std::string group;
        std::string xVariable;
        std::string yVariable;
        std::string secondaryVariable;
        std::string groupVariable;
        std::string conditionVariable;
        std::string trellisType = "scatter";
        std::vector<std::string> variables;
        std::string title;
        int bins = 0;
        bool standardize = true;
        bool connect = true;
        ::rlispstat::core::AnalysisScope dataScope;
        bool dataScopeCaptured = false;
    };

    enum class AnalysisWorkflowKind
    {
        Table1,
        CorrelationMatrix,
        Dimensionality,
        FactorAnalysis,
        ScaleAnalysis,
        QuickCluster,
        OneSampleT,
        IndependentT,
        PairedT,
        OneWayAnova,
        LinearModel,
        LinearModelTrellis,
        RegressionComparison,
        BinaryRegression,
        BinaryRegressionComparison,
        GeneralizedLinearModel,
        CountRegression,
        CountRegressionComparison,
        PositiveContinuousModel,
        PositiveContinuousComparison,
        ProportionModel,
        ProportionComparison,
        GeneralizedComparison,
        LinearMixedModel,
        GeneralizedMixedModel
    };

    struct AnalysisWorkflowSpecification
    {
        AnalysisWorkflowKind kind = AnalysisWorkflowKind::CorrelationMatrix;
        std::string group;
        std::string response;
        std::string secondary;
        std::string groupVariable;
        std::vector<std::string> variables;
        std::string family = "gaussian";
        std::string link = "identity";
        std::string method;
        std::string rowCondition;
        std::string columnCondition;
        std::string scope = "all";
    };

    class VariablesWindow final
        : public std::enable_shared_from_this<VariablesWindow>
    {
    public:
        using Closed = std::function<void()>;
        using Command = std::function<std::string(std::vector<std::string> const&)>;

        static std::shared_ptr<VariablesWindow> Create();
        void SetClosedCallback(Closed callback);
        void SetCommandCallback(Command callback);
        void Show(::rlispstat::core::DataFrameModel const& dataframe,
                  std::map<std::string, std::string> const& roles,
                  bool activate = true);
        void Activate();
        void Close();

    private:
        VariablesWindow();
        void Initialize();
        void AttachLifetime();
        void RebuildRows();
        std::string Dispatch(std::vector<std::string> const& command);

        Microsoft::UI::Xaml::Window window_{ nullptr };
        Microsoft::UI::Xaml::Controls::Grid root_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock title_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock hint_{ nullptr };
        Microsoft::UI::Xaml::Controls::Grid table_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock status_{ nullptr };
        ::rlispstat::core::DataFrameModel dataframe_;
        std::map<std::string, std::string> roles_;
        Closed closedCallback_;
        Command commandCallback_;
        bool updating_ = false;
        bool closed_ = false;
    };

    class AnalysisWorkflowDialog final
        : public std::enable_shared_from_this<AnalysisWorkflowDialog>
    {
    public:
        using Submit = std::function<void(AnalysisWorkflowSpecification const&)>;
        using Closed = std::function<void()>;
        static std::shared_ptr<AnalysisWorkflowDialog> Create(
            Microsoft::UI::Xaml::Window const& owner,
            ::rlispstat::core::DataFrameModel const& dataframe,
            AnalysisWorkflowKind kind, Submit submit, Closed closed,
            ::rlispstat::core::InitialAnalysisSpecification initial = {},
            std::vector<::rlispstat::core::AnalysisScopeChoice> scopeChoices = {});
        void Show();
        void Close();

    private:
        AnalysisWorkflowDialog(Microsoft::UI::Xaml::Window const& owner,
                               ::rlispstat::core::DataFrameModel const& dataframe,
                               AnalysisWorkflowKind kind, Submit submit, Closed closed,
                               ::rlispstat::core::InitialAnalysisSpecification initial,
                               std::vector<::rlispstat::core::AnalysisScopeChoice> scopeChoices);
        void Initialize();
        void AttachLifetime();
        void Validate();
        void SubmitNow();
        void RebuildDistributionLinks();

        Microsoft::UI::Xaml::Window owner_{ nullptr };
        Microsoft::UI::Xaml::Controls::ContentDialog dialog_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock responseLabel_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox response_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock secondaryLabel_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox secondary_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock groupLabel_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox groupVariable_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock rowLabel_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox rowCondition_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock columnLabel_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox columnCondition_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox scope_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock variablesLabel_{ nullptr };
        Microsoft::UI::Xaml::Controls::ListView variables_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock familyLabel_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox family_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock linkLabel_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox link_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock methodLabel_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox method_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock validation_{ nullptr };
        ::rlispstat::core::DataFrameModel dataframe_;
        AnalysisWorkflowKind kind_;
        Submit submit_;
        Closed closedCallback_;
        ::rlispstat::core::InitialAnalysisSpecification initial_;
        std::vector<::rlispstat::core::AnalysisScopeChoice> scopeChoices_;
        std::vector<std::string> distributionIds_;
        std::vector<std::string> linkIds_;
        bool closed_ = false;
    };

    class PlotWorkflowDialog final
        : public std::enable_shared_from_this<PlotWorkflowDialog>
    {
    public:
        using Submit = std::function<void(PlotWorkflowSpecification const&)>;
        using Closed = std::function<void()>;
        static std::shared_ptr<PlotWorkflowDialog> Create(
            Microsoft::UI::Xaml::Window const& owner,
            ::rlispstat::core::DataFrameModel const& dataframe,
            PlotWorkflowKind kind, Submit submit, Closed closed,
            std::string initialTrellisType = "scatter",
            ::rlispstat::core::InitialAnalysisSpecification initial = {});
        void Show();
        void Close();

    private:
        PlotWorkflowDialog(Microsoft::UI::Xaml::Window const& owner,
                           ::rlispstat::core::DataFrameModel const& dataframe,
                           PlotWorkflowKind kind, Submit submit, Closed closed,
                           std::string initialTrellisType,
                           ::rlispstat::core::InitialAnalysisSpecification initial);
        void Initialize();
        void AttachLifetime();
        void Validate();
        void SubmitNow();

        Microsoft::UI::Xaml::Window owner_{ nullptr };
        Microsoft::UI::Xaml::Controls::ContentDialog dialog_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock validation_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox xVariable_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox yVariable_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox secondaryVariable_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox groupVariable_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox conditionVariable_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox trellisType_{ nullptr };
        Microsoft::UI::Xaml::Controls::ListView variables_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBox bins_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBox title_{ nullptr };
        Microsoft::UI::Xaml::Controls::CheckBox standardize_{ nullptr };
        Microsoft::UI::Xaml::Controls::CheckBox connect_{ nullptr };
        ::rlispstat::core::DataFrameModel dataframe_;
        PlotWorkflowKind kind_;
        Submit submit_;
        Closed closedCallback_;
        std::string initialTrellisType_ = "scatter";
        ::rlispstat::core::InitialAnalysisSpecification initial_;
        bool closed_ = false;
    };

    class ScatterplotDialog final : public std::enable_shared_from_this<ScatterplotDialog>
    {
    public:
        using Submit = std::function<void(ScatterplotSpecification const&)>;
        using Closed = std::function<void()>;
        static std::shared_ptr<ScatterplotDialog> Create(
            Microsoft::UI::Xaml::Window const& owner,
            ::rlispstat::core::DataFrameModel const& dataframe,
            Submit submit, Closed closed,
            ::rlispstat::core::InitialAnalysisSpecification initial = {});
        void Show();
        void Close();

    private:
        ScatterplotDialog(Microsoft::UI::Xaml::Window const& owner,
                          ::rlispstat::core::DataFrameModel const& dataframe,
                          Submit submit, Closed closed,
                          ::rlispstat::core::InitialAnalysisSpecification initial);
        void Initialize();
        void Validate();
        void SubmitNow();
        void AttachLifetime();

        Microsoft::UI::Xaml::Window owner_{ nullptr };
        Microsoft::UI::Xaml::Controls::ContentDialog dialog_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox xVariable_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox yVariable_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBox title_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock validation_{ nullptr };
        ::rlispstat::core::DataFrameModel dataframe_;
        ::rlispstat::core::InitialAnalysisSpecification initial_;
        Submit submit_;
        Closed closedCallback_;
        bool closed_ = false;
    };

    class DescriptiveStatisticsDialog final
        : public std::enable_shared_from_this<DescriptiveStatisticsDialog>
    {
    public:
        using Submit = std::function<void(DescriptiveStatisticsSpecification const&)>;
        using Closed = std::function<void()>;
        static std::shared_ptr<DescriptiveStatisticsDialog> Create(
            Microsoft::UI::Xaml::Window const& owner,
            ::rlispstat::core::DataFrameModel const& dataframe,
            std::set<int> const& selectedRows,
            std::vector<::rlispstat::core::SavedSelection> savedSelections,
            ::rlispstat::core::InitialAnalysisSpecification initial,
            Submit submit, Closed closed);
        void SetAnalysisScope(::rlispstat::core::AnalysisScope const& scope);
        void Show();
        void Close();
        void ReportError(std::string const& message);

    private:
        DescriptiveStatisticsDialog(Microsoft::UI::Xaml::Window const& owner,
                                    ::rlispstat::core::DataFrameModel const& dataframe,
                                    std::set<int> const& selectedRows,
                                    std::vector<::rlispstat::core::SavedSelection> savedSelections,
                                    ::rlispstat::core::InitialAnalysisSpecification initial,
                                    Submit submit, Closed closed);
        void Initialize();
        void AttachLifetime();
        void AddVariables();
        void RemoveVariables();
        void Validate();
        void SubmitNow();

        Microsoft::UI::Xaml::Window owner_{ nullptr };
        Microsoft::UI::Xaml::Controls::ContentDialog dialog_{ nullptr };
        Microsoft::UI::Xaml::Controls::ListView available_{ nullptr };
        Microsoft::UI::Xaml::Controls::ListView selected_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox groupVariable_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox scope_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox ordinalAs_{ nullptr };
        Microsoft::UI::Xaml::Controls::CheckBox includeMissing_{ nullptr };
        Microsoft::UI::Xaml::Controls::CheckBox showP_{ nullptr };
        Microsoft::UI::Xaml::Controls::CheckBox showTest_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock validation_{ nullptr };
        ::rlispstat::core::DataFrameModel dataframe_;
        std::set<int> initialSelection_;
        std::vector<::rlispstat::core::SavedSelection> savedSelections_;
        ::rlispstat::core::InitialAnalysisSpecification initial_;
        Submit submit_;
        Closed closedCallback_;
        bool busy_ = false;
        bool closed_ = false;
    };

    class ContingencyTableDialog final
        : public std::enable_shared_from_this<ContingencyTableDialog>
    {
    public:
        using Submit = std::function<void(ContingencyTableSpecification const&)>;
        using Closed = std::function<void()>;
        static std::shared_ptr<ContingencyTableDialog> Create(
            Microsoft::UI::Xaml::Window const& owner,
            ::rlispstat::core::DataFrameModel const& dataframe,
            ::rlispstat::core::InitialAnalysisSpecification initial,
            Submit submit, Closed closed);
        void Show();
        void Close();

    private:
        ContingencyTableDialog(Microsoft::UI::Xaml::Window const& owner,
                               ::rlispstat::core::DataFrameModel const& dataframe,
                               ::rlispstat::core::InitialAnalysisSpecification initial,
                               Submit submit, Closed closed);
        void Initialize();
        void AttachLifetime();
        void Validate();
        void SubmitNow();

        Microsoft::UI::Xaml::Window owner_{ nullptr };
        Microsoft::UI::Xaml::Controls::ContentDialog dialog_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox rowVariable_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox columnVariable_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock validation_{ nullptr };
        ::rlispstat::core::DataFrameModel dataframe_;
        ::rlispstat::core::InitialAnalysisSpecification initial_;
        Submit submit_;
        Closed closedCallback_;
        bool closed_ = false;
    };

    class MissingDataImputationDialog final
        : public std::enable_shared_from_this<MissingDataImputationDialog>
    {
    public:
        using Submit = std::function<void(MissingDataImputationSpecification const&)>;
        using Closed = std::function<void()>;
        using Failed = std::function<void(std::wstring const&)>;
        static std::shared_ptr<MissingDataImputationDialog> Create(
            Microsoft::UI::Xaml::Window const& owner,
            ::rlispstat::core::DataFrameModel const& dataframe,
            Submit submit, Closed closed, Failed failed);
        void Show();
        void Close();
        bool IsClosed() const noexcept { return closed_; }
        bool IsVisible() const noexcept { return visible_; }

    private:
        struct VariableRow
        {
            std::string name;
            Microsoft::UI::Xaml::Controls::CheckBox impute{ nullptr };
            Microsoft::UI::Xaml::Controls::ComboBox method{ nullptr };
            Microsoft::UI::Xaml::Controls::CheckBox predictor{ nullptr };
        };

        MissingDataImputationDialog(
            Microsoft::UI::Xaml::Window const& owner,
            ::rlispstat::core::DataFrameModel const& dataframe,
            Submit submit, Closed closed, Failed failed);
        void Initialize();
        void AttachLifetime();
        winrt::fire_and_forget ShowAsyncOperation();
        void ReportOpenFailure(std::wstring const& message);
        void Validate();
        void SubmitNow();
        void DeselectAll();
        void DeselectPredictors();

        Microsoft::UI::Xaml::Window owner_{ nullptr };
        Microsoft::UI::Xaml::Controls::ContentDialog dialog_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBox imputations_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBox iterations_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBox seed_{ nullptr };
        Microsoft::UI::Xaml::Controls::CheckBox openDataSheet_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock summary_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock validation_{ nullptr };
        std::vector<VariableRow> variableRows_;
        ::rlispstat::core::DataFrameModel dataframe_;
        Submit submit_;
        Closed closedCallback_;
        Failed failedCallback_;
        Microsoft::UI::Dispatching::DispatcherQueueTimer openWatchdog_{ nullptr };
        bool closed_ = false;
        bool visible_ = false;
        bool showPending_ = false;
    };

    class DatasetVariableDialog final
        : public std::enable_shared_from_this<DatasetVariableDialog>
    {
    public:
        using Submit = std::function<void(std::vector<std::string> const&)>;
        using Closed = std::function<void()>;
        static std::shared_ptr<DatasetVariableDialog> Create(
            Microsoft::UI::Xaml::Window const& owner,
            ::rlispstat::core::DataFrameModel const& dataframe,
            std::wstring title, std::wstring information,
            std::wstring primaryButton, bool multiple,
            std::string currentValue, Submit submit, Closed closed);
        void Show();
        void Close();

    private:
        DatasetVariableDialog(
            Microsoft::UI::Xaml::Window const& owner,
            ::rlispstat::core::DataFrameModel const& dataframe,
            std::wstring title, std::wstring information,
            std::wstring primaryButton, bool multiple,
            std::string currentValue, Submit submit, Closed closed);
        void Initialize();
        void AttachLifetime();
        void RebuildSingleVariableList();

        Microsoft::UI::Xaml::Window owner_{ nullptr };
        Microsoft::UI::Xaml::Controls::ContentDialog dialog_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBox variableSearch_{ nullptr };
        Microsoft::UI::Xaml::Controls::ListView variableList_{ nullptr };
        ::rlispstat::core::DataFrameModel dataframe_;
        std::wstring title_;
        std::wstring information_;
        std::wstring primaryButton_;
        bool multiple_ = false;
        std::string currentValue_;
        Submit submit_;
        Closed closedCallback_;
        bool closed_ = false;
    };

    class RDataAssignmentDialog final
        : public std::enable_shared_from_this<RDataAssignmentDialog>
    {
    public:
        using Submit = std::function<void(RDataAssignmentSpecification const&)>;
        using Closed = std::function<void()>;
        static std::shared_ptr<RDataAssignmentDialog> Create(
            Microsoft::UI::Xaml::Window const& owner,
            ::rlispstat::core::DataFrameModel const& dataframe,
            bool selectedRowsOnly, std::size_t selectedRowCount,
            Submit submit, Closed closed);
        void Show();
        void Close();

    private:
        RDataAssignmentDialog(
            Microsoft::UI::Xaml::Window const& owner,
            ::rlispstat::core::DataFrameModel const& dataframe,
            bool selectedRowsOnly, std::size_t selectedRowCount,
            Submit submit, Closed closed);
        void Initialize();
        void AttachLifetime();
        void Validate();
        void SubmitNow();

        Microsoft::UI::Xaml::Window owner_{ nullptr };
        Microsoft::UI::Xaml::Controls::ContentDialog dialog_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBox objectName_{ nullptr };
        Microsoft::UI::Xaml::Controls::CheckBox replaceExisting_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock validation_{ nullptr };
        ::rlispstat::core::DataFrameModel dataframe_;
        bool selectedRowsOnly_ = false;
        std::size_t selectedRowCount_ = 0;
        Submit submit_;
        Closed closedCallback_;
        bool closed_ = false;
    };

    class AnalysisOutputView final : public std::enable_shared_from_this<AnalysisOutputView>
    {
    public:
        using Closed = std::function<void()>;
        using Command = std::function<void(std::vector<std::string> const&)>;
        static std::shared_ptr<AnalysisOutputView> Create();
        void SetClosedCallback(Closed callback);
        void SetCommandCallback(Command callback,
                                std::vector<std::string> availableVariables);
        void Show(::rlispstat::core::Table1DisplayState const& state);
        void EmbedDescriptives(std::shared_ptr<AnalysisOutputView> const& child);
        void Activate();
        void Close();

    private:
        AnalysisOutputView();
        void Initialize();
        void AttachLifetime();
        void BuildTable(::rlispstat::core::Table1DisplayState const& state);
        void CopyTable();
        void ConfigureContextMenu();
        Microsoft::UI::Xaml::Controls::MenuFlyout CreateMissingnessMenu(std::string pattern = {}, std::string variable = {});
        void SelectMissingnessPattern(std::string const& pattern);
        Microsoft::UI::Xaml::Controls::MenuFlyout CreateTable1VariableMenu(
            std::string const& variable);
        Microsoft::UI::Xaml::Controls::MenuFlyout CreateTable1ReplacementMenu(
            std::string const& variable);
        Microsoft::UI::Xaml::Controls::MenuFlyout CreateTable1AddVariableFlyout();
        Microsoft::UI::Xaml::Controls::MenuFlyout CreateNestedRowVariableMenu(
            std::string const& variable);
        Microsoft::UI::Xaml::Controls::MenuFlyout CreateNestedSplitVariableMenu();
        Microsoft::UI::Xaml::Controls::MenuFlyoutSubItem CreateGroupingMenu();
        Microsoft::UI::Xaml::Controls::MenuFlyoutSubItem CreateAddVariableMenu();
        std::string VariableAnalysisType(std::string const& variable) const;
        void Dispatch(std::string command, std::string argument = {});

        Microsoft::UI::Xaml::Window window_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock title_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock badge_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock subtitle_{ nullptr };
        Microsoft::UI::Xaml::Controls::StackPanel report_{ nullptr };
        std::shared_ptr<AnalysisOutputView> embeddedDescriptives_;
        Microsoft::UI::Xaml::Controls::Grid table_{ nullptr };
        Microsoft::UI::Xaml::Controls::StackPanel footnotes_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock hint_{ nullptr };
        ::rlispstat::core::Table1DisplayState state_;
        ::rlispstat::core::Table1DisplayState sourceState_;
        Microsoft::UI::Xaml::Controls::Button addModelVariable_{ nullptr };
        std::shared_ptr<TableExportService> exporter_;
        std::vector<std::string> availableVariables_;
        Closed closedCallback_;
        Command commandCallback_;
        bool presented_ = false;
        bool closed_ = false;
    };

    // A GLM interaction interpretation is a report made of several related
    // tables, not one flat table.  Keeping a dedicated native view preserves
    // the fitted-model hierarchy (metadata, adjusted predictions and each
    // family of simple effects) on Windows just as it does on macOS.
    class GLMInteractionReportView final
        : public std::enable_shared_from_this<GLMInteractionReportView>
    {
    public:
        using Closed = std::function<void()>;
        using Command = std::function<void(std::vector<std::string> const&)>;
        static std::shared_ptr<GLMInteractionReportView> Create();
        void SetClosedCallback(Closed callback);
        void SetCommandCallback(Command callback);
        void SetPendingMessage(std::string const& message);
        void Show(std::string const& id,
                  std::string const& datasetId,
                  std::string const& title,
                  std::string const& subtitle,
                  ::rlispstat::core::GLMInteractionReport const& report);
        void Activate();
        void Close();

    private:
        GLMInteractionReportView();
        void Initialize();
        void AttachLifetime();
        void Render();
        void CopyReport();

        Microsoft::UI::Xaml::Window window_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock title_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock badge_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock subtitle_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox grouping_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox comparison_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock comparisonLabel_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock stratumLabel_{ nullptr };
        Microsoft::UI::Xaml::Controls::StackPanel groupingControls_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox emmReference_{ nullptr };
        Microsoft::UI::Xaml::Controls::StackPanel referenceControls_{ nullptr };
        Microsoft::UI::Xaml::Controls::StackPanel binaryControls_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox effectQuantity_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox effectAdjustment_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox effectPresentation_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox effectConfidence_{ nullptr };
        Microsoft::UI::Xaml::Controls::StackPanel content_{ nullptr };
        std::shared_ptr<TableExportService> exporter_;
        std::string id_;
        std::string datasetId_;
        std::string titleText_;
        std::string subtitleText_;
        ::rlispstat::core::GLMInteractionReport report_;
        Closed closedCallback_;
        Command commandCallback_;
        bool updatingReference_ = false;
        bool updatingBinaryOptions_ = false;
        bool presented_ = false;
        bool closed_ = false;
    };

    class CorrelationMatrixView final
        : public std::enable_shared_from_this<CorrelationMatrixView>
    {
    public:
        using Closed = std::function<void()>;
        using Command = std::function<void(std::vector<std::string> const&)>;
        static std::shared_ptr<CorrelationMatrixView> Create();
        void SetClosedCallback(Closed callback);
        void SetCommandCallback(Command callback);
        void SetScopeChoices(std::vector<::rlispstat::core::AnalysisScopeChoice> choices);
        void Show(::rlispstat::core::CorrelationMatrixState const& state,
                  std::size_t selectedRowCount);
        void Activate();
        void Close();

    private:
        CorrelationMatrixView();
        void Initialize();
        void AttachLifetime();
        void Render();
        void RebuildControls();
        void ConfigureContextMenu();
        void AppendDisplayOptions(winrt::Microsoft::UI::Xaml::Controls::MenuFlyout const& menu);
        Microsoft::UI::Xaml::Controls::MenuFlyout CreateVariableMenu(
            std::size_t variableIndex);
        void SetVariables(std::vector<std::string> variables);

        Microsoft::UI::Xaml::Window window_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock title_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock badge_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox missingMode_{ nullptr };
        Microsoft::UI::Xaml::Controls::CheckBox showStars_{ nullptr };
        Microsoft::UI::Xaml::Controls::CheckBox showPValues_{ nullptr };
        Microsoft::UI::Xaml::Controls::CheckBox showSampleSizes_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox scope_{ nullptr };
        Microsoft::UI::Xaml::Controls::Button variablesButton_{ nullptr };
        Microsoft::UI::Xaml::Controls::Canvas matrix_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock status_{ nullptr };
        std::shared_ptr<TableExportService> exporter_;
        ::rlispstat::core::CorrelationMatrixState state_;
        Closed closedCallback_;
        Command commandCallback_;
        std::vector<::rlispstat::core::AnalysisScopeChoice> scopeChoices_;
        bool updatingControls_ = false;
        bool presented_ = false;
        bool closed_ = false;
    };

    class MeanComparisonView final
        : public std::enable_shared_from_this<MeanComparisonView>
    {
    public:
        using Closed = std::function<void()>;
        using Command = std::function<void(std::vector<std::string> const&)>;
        static std::shared_ptr<MeanComparisonView> Create(bool descriptivesOnly = false);
        void SetClosedCallback(Closed callback);
        void SetCommandCallback(Command callback,
                                std::vector<std::string> responseVariables,
                                std::vector<std::string> groupingVariables,
                                std::map<std::string, std::string> variableTypes,
                                std::map<std::string, std::vector<std::string>> groupingLevels,
                                std::size_t selectedRowCount,
                                std::size_t totalRowCount);
        void SetScopeChoices(std::vector<::rlispstat::core::AnalysisScopeChoice> choices);
        void Show(::rlispstat::core::MeanComparisonState const& state);
        void ShowPending(::rlispstat::core::MeanComparisonState const& state);
        void Activate();
        void Close();

    private:
        explicit MeanComparisonView(bool descriptivesOnly);
        void Initialize();
        void AttachLifetime();
        void Render();
        void RebuildControls();
        void ConfigureContextMenu();
        void ResizeForVisibleContent();
        void OpenVariableChooser(
            std::vector<std::string> candidates,
            std::wstring title,
            std::wstring information,
            std::string currentValue,
            std::function<void(std::string const&)> submit,
            std::function<void()> afterClosed = {});
        void OpenPairChooser(std::vector<std::string> candidates);
        void AppendVariableChoices(
            Microsoft::UI::Xaml::Controls::MenuFlyoutSubItem const& parent,
            std::vector<std::string> const& candidates,
            std::wstring chooserTitle,
            std::wstring chooserInformation,
            std::string currentValue,
            std::function<void(std::string const&)> action);
        Microsoft::UI::Xaml::Controls::MenuFlyoutSubItem CreatePlotsMenu(
            std::string const& variable,
            std::string const& secondVariable,
            std::wstring const& title);
        Microsoft::UI::Xaml::Controls::MenuFlyout CreateVariableMenu(
            ::rlispstat::core::MeanComparisonRow const& row);

        Microsoft::UI::Xaml::Window window_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock title_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock badge_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock subtitle_{ nullptr };
        Microsoft::UI::Xaml::Controls::StackPanel controls_{ nullptr };
        Microsoft::UI::Xaml::Controls::StackPanel content_{ nullptr };
        ::rlispstat::core::MeanComparisonState state_;
        std::vector<std::string> responseVariables_;
        std::vector<std::string> groupingVariables_;
        std::map<std::string, std::string> variableTypes_;
        std::map<std::string, std::vector<std::string>> groupingLevels_;
        std::size_t selectedRowCount_ = 0;
        std::size_t totalRowCount_ = 0;
        std::vector<::rlispstat::core::AnalysisScopeChoice> scopeChoices_;
        std::shared_ptr<DatasetVariableDialog> variableDialog_;
        std::shared_ptr<TableExportService> exporter_;
        Closed closedCallback_;
        Command commandCallback_;
        bool updatingControls_ = false;
        bool presented_ = false;
        bool showDescriptives_ = false;
        bool descriptivesOnly_ = false;
        bool closed_ = false;
    };

    class LinearModelView final : public std::enable_shared_from_this<LinearModelView>
    {
    public:
        using Closed = std::function<void()>;
        using Diagnostic = std::function<void(std::string const&)>;
        using Command = std::function<void(std::vector<std::string> const&)>;
        static std::shared_ptr<LinearModelView> Create();
        void SetClosedCallback(Closed callback);
        void SetDiagnosticCallback(Diagnostic callback);
        void SetCommandCallback(Command callback,
                                std::vector<std::string> numericVariables,
                                std::vector<std::string> availableVariables);
        void SetScopeChoices(std::vector<::rlispstat::core::AnalysisScopeChoice> choices);
        void Show(::rlispstat::core::GroupModelState const& model,
                  ::rlispstat::core::GLMFitSummary const& fit);
        void Activate();
        void Close();

    private:
        LinearModelView();
        void Initialize();
        void AttachLifetime();
        void Render(::rlispstat::core::GroupModelState const& model,
                    ::rlispstat::core::GLMFitSummary const& fit);
        void InitializeReport();
        void RebuildTermRows(::rlispstat::core::GLMFitSummary const& fit);
        void RebuildControls();
        void ConfigureContextMenu();
        Microsoft::UI::Xaml::Window window_{ nullptr };
        Microsoft::UI::Xaml::Controls::Grid root_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock title_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock badge_{ nullptr };
        Microsoft::UI::Xaml::Controls::Button modelActionsButton_{ nullptr };
        Microsoft::UI::Xaml::Controls::StackPanel controls_{ nullptr };
        Microsoft::UI::Xaml::Controls::StackPanel content_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock fitSummary_{ nullptr };
        Microsoft::UI::Xaml::Controls::Grid anovaGrid_{ nullptr };
        Microsoft::UI::Xaml::Controls::Grid termsGrid_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock footer_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock note_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock warning_{ nullptr };
        std::vector<Microsoft::UI::Xaml::Controls::TextBlock> anovaCells_;
        std::vector<Microsoft::UI::Xaml::Controls::TextBlock> termCells_;
        std::vector<std::string> termRowStructure_;
        Closed closedCallback_;
        Diagnostic diagnosticCallback_;
        Command commandCallback_;
        ::rlispstat::core::GroupModelState model_;
        ::rlispstat::core::GLMFitSummary fit_;
        std::shared_ptr<TableExportService> exporter_;
        std::vector<std::string> numericVariables_;
        std::vector<std::string> availableVariables_;
        std::vector<::rlispstat::core::AnalysisScopeChoice> scopeChoices_;
        std::string controlsSignature_;
        bool commandChoicesChanged_ = true;
        bool reportInitialized_ = false;
        bool termRowsInitialized_ = false;
        int renderedFitVersion_ = -1;
        bool renderedFitOk_ = false;
        std::string renderedWarning_;
        std::size_t windowCoefficientRows_ = static_cast<std::size_t>(-1);
        bool presented_ = false;
        bool closed_ = false;
    };

    class DimensionalityView final : public std::enable_shared_from_this<DimensionalityView>
    {
    public:
        using Closed = std::function<void()>;
        using Changed = std::function<void(std::string const&, std::string const&,
            std::string const&, std::string const&, bool, int, bool)>;
        using Action = std::function<void(std::string const&)>;
        using Command = std::function<void(std::vector<std::string> const&)>;
        static std::shared_ptr<DimensionalityView> Create();
        void SetClosedCallback(Closed callback);
        void SetChangedCallback(Changed callback);
        void SetActionCallback(Action callback);
        void SetCommandCallback(Command callback);
        void SetScopeChoices(std::vector<::rlispstat::core::AnalysisScopeChoice> choices);
        void Show(::rlispstat::core::DimensionalityState const& state);
        void Activate();
        void Close();

    private:
        DimensionalityView();
        void Initialize();
        void AttachLifetime();
        void Render(::rlispstat::core::DimensionalityState const& state);
        void NotifyChanged();
        void ConfigureContextMenu(::rlispstat::core::DimensionalityState const& state);
        Microsoft::UI::Xaml::Controls::MenuFlyout CreateVariableMenu(
            std::size_t variableIndex);

        Microsoft::UI::Xaml::Window window_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock title_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock badge_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox method_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox missing_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox components_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox rotation_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox imputation_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox scope_{ nullptr };
        Microsoft::UI::Xaml::Controls::CheckBox scale_{ nullptr };
        Microsoft::UI::Xaml::Controls::CheckBox autoFit_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock selectedRows_{ nullptr };
        Microsoft::UI::Xaml::Controls::Canvas report_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock status_{ nullptr };
        std::shared_ptr<TableExportService> exporter_;
        ::rlispstat::core::DimensionalityState state_;
        Closed closedCallback_;
        Changed changedCallback_;
        Action actionCallback_;
        Command commandCallback_;
        std::vector<::rlispstat::core::AnalysisScopeChoice> scopeChoices_;
        bool updating_ = false;
        bool presented_ = false;
        bool closed_ = false;
    };

    class ScaleAnalysisView final : public std::enable_shared_from_this<ScaleAnalysisView>
    {
    public:
        using Closed = std::function<void()>;
        using Changed = std::function<void(::rlispstat::core::ScaleAnalysisSpecification const&)>;
        using OpenScatterplot = std::function<void(std::string const&, std::string const&)>;
        using Command = std::function<void(std::vector<std::string> const&)>;

        static std::shared_ptr<ScaleAnalysisView> Create();
        void SetClosedCallback(Closed callback);
        void SetChangedCallback(Changed callback);
        void SetCommandCallback(Command callback);
        void SetOpenScatterplotCallback(OpenScatterplot callback);
        void SetAvailableItems(std::vector<::rlispstat::core::ScaleItemSpecification> items);
        void Show(::rlispstat::core::ScaleAnalysisState const& state);
        void Activate();
        void Close();

    private:
        ScaleAnalysisView();
        void Initialize();
        void AttachLifetime();
        void Render();
        void RenderDerived(::rlispstat::core::ScaleAnalysisChildKind kind);
        void ConfigureAddMenu();
        void RefreshAddItemSelector(int preferredIndex = 0);
        void MoveAddItemSelection(int delta);
        void AddSelectedQuickItem();
        void AddQuickItem(::rlispstat::core::ScaleItemSpecification const& item);
        void ConfigureContextMenu();
        Microsoft::UI::Xaml::Controls::MenuFlyoutSubItem CreateAddItemSubmenu();
        void OpenDerived(::rlispstat::core::ScaleAnalysisChildKind kind);
        void ShowAnalysisInformation();
        Microsoft::UI::Xaml::Controls::MenuFlyout CreateItemMenu(std::size_t index);
        void Submit(::rlispstat::core::ScaleAnalysisSpecification candidate);
        winrt::fire_and_forget PromptRange(std::size_t index);

        Microsoft::UI::Xaml::Window window_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock title_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock badge_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox correlation_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox score_{ nullptr };
        Microsoft::UI::Xaml::Controls::Button addItem_{ nullptr };
        Microsoft::UI::Xaml::Controls::Flyout addItemFlyout_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBox addItemSearch_{ nullptr };
        Microsoft::UI::Xaml::Controls::ListView addItemList_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock addItemEmpty_{ nullptr };
        Microsoft::UI::Xaml::Controls::StackPanel report_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock status_{ nullptr };
        std::shared_ptr<TableExportService> exporter_;
        struct DerivedWindow
        {
            Microsoft::UI::Xaml::Window window{ nullptr };
            Microsoft::UI::Xaml::Controls::StackPanel report{ nullptr };
            Microsoft::UI::Xaml::Controls::TextBlock status{ nullptr };
            std::shared_ptr<TableExportService> exporter;
            ::rlispstat::core::ScaleAnalysisDerivedIdentity identity;
            int selectedCorrelationRow = -1;
            int selectedCorrelationColumn = -1;
            int focusedDimensionComponent = 0;
            std::string focusedDimensionVariable;
            int selectedDimensionImputation = 1;
            bool showCorrelationStars = false;
            bool showCorrelationPValues = false;
            bool showCorrelationN = false;
            Microsoft::UI::Xaml::Controls::ToggleMenuFlyoutItem correlationStarsItem{ nullptr };
            Microsoft::UI::Xaml::Controls::ToggleMenuFlyoutItem correlationPValuesItem{ nullptr };
            Microsoft::UI::Xaml::Controls::ToggleMenuFlyoutItem correlationNItem{ nullptr };
            bool presented = false;
        };
        std::map<int, std::shared_ptr<DerivedWindow>> derivedWindows_;
        ::rlispstat::core::ScaleAnalysisState state_;
        std::vector<::rlispstat::core::ScaleItemSpecification> availableItems_;
        std::vector<::rlispstat::core::ScaleItemSpecification> filteredAddItems_;
        Closed closedCallback_;
        Changed changedCallback_;
        OpenScatterplot openScatterplotCallback_;
        Command commandCallback_;
        bool updating_ = false;
        bool presented_ = false;
        bool closed_ = false;
    };

    class DendrogramView final : public std::enable_shared_from_this<DendrogramView>
    {
    public:
        using Closed = std::function<void()>;
        using Changed = std::function<void(std::string const&, std::string const&)>;
        using SelectRow = std::function<void(int)>;
        using SelectRows = std::function<void(std::vector<int> const&, std::string const&)>;
        using Command = std::function<void(std::vector<std::string> const&)>;
        static std::shared_ptr<DendrogramView> Create();
        void SetClosedCallback(Closed callback);
        void SetChangedCallback(Changed callback);
        void SetSelectRowCallback(SelectRow callback);
        void SetSelectRowsCallback(SelectRows callback);
        void SetCommandCallback(Command callback);
        void Show(::rlispstat::core::DendrogramState const& state,
                  std::set<int> const& selectedRows,
                  std::vector<std::pair<int, std::string>> const& rowColors = {},
                  std::vector<std::string> const& availableVariables = {});
        void RefreshSelection(std::set<int> const& selectedRows);
        void RefreshRowColors(std::vector<std::pair<int, std::string>> const& rowColors);
        void SetVisualTheme(std::string const& themeName);
        void Activate();
        void Close();

    private:
        DendrogramView();
        void Initialize();
        void AttachLifetime();
        void Render();
        void FitCanvasToViewport(double viewportWidth = 0.0,
                                 double viewportHeight = 0.0);
        void CompletePointerGesture(Microsoft::UI::Xaml::Input::PointerRoutedEventArgs const& event);
        void ConfigureContextMenu();
        void SetNotesVisible(bool visible);
        std::string ExportSvgDocument() const;
        winrt::fire_and_forget CopyPlotAsync(bool includeSvg);
        winrt::fire_and_forget SavePlotAsync(std::string format);
        Windows::Foundation::IAsyncOperation<
            Windows::Storage::Streams::InMemoryRandomAccessStream>
            RenderSvgToPngAsync(std::string svg, double scale);
        Windows::Foundation::IAsyncOperation<bool>
            RenderSvgToPdfAsync(std::string svg, std::wstring path);
        Microsoft::UI::Xaml::Window window_{ nullptr };
        Microsoft::UI::Xaml::Controls::Grid root_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock title_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock badge_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox linkage_{ nullptr };
        Microsoft::UI::Xaml::Controls::ComboBox missing_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock distance_{ nullptr };
        Microsoft::UI::Xaml::Controls::Button addVariable_{ nullptr };
        Microsoft::UI::Xaml::Controls::Canvas canvas_{ nullptr };
        Microsoft::UI::Xaml::Controls::ScrollViewer scroll_{ nullptr };
        Microsoft::UI::Xaml::Controls::Border notesPanel_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBox notesText_{ nullptr };
        Microsoft::UI::Xaml::Controls::CheckBox includeNotes_{ nullptr };
        Microsoft::UI::Xaml::Controls::WebView2 exportWebView_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock status_{ nullptr };
        ::rlispstat::core::DendrogramState state_;
        bool fitTreeToWindow_ = true;
        ::rlispstat::core::DendrogramPlotGeometry geometry_;
        ::rlispstat::core::WindowNote note_;
        std::string themeName_ = "publication";
        ::rlispstat::core::Rect legendRect_;
        std::set<int> selectedRows_;
        std::map<int, std::string> rowColors_;
        std::vector<std::string> availableVariables_;
        ::rlispstat::core::Point dragStart_;
        ::rlispstat::core::Point dragCurrent_;
        double colorLegendDragStartX_ = 0.0;
        double colorLegendDragStartY_ = 0.0;
        double colorLegendOriginalX_ = 0.0;
        double colorLegendOriginalY_ = 0.0;
        Closed closedCallback_;
        Changed changedCallback_;
        SelectRow selectRowCallback_;
        SelectRows selectRowsCallback_;
        Command commandCallback_;
        bool updating_ = false;
        bool dragging_ = false;
        bool colorLegendDragging_ = false;
        bool colorLegendMoved_ = false;
        bool updatingNotes_ = false;
        bool presented_ = false;
        bool closed_ = false;
    };

    class GeneralizedModelView final : public std::enable_shared_from_this<GeneralizedModelView>
    {
    public:
        using Closed=std::function<void()>;
        using Diagnostic=std::function<void(std::string const&)>;
        using Command=std::function<void(std::vector<std::string> const&)>;
        static std::shared_ptr<GeneralizedModelView> Create();
        void SetClosedCallback(Closed callback);
        void SetDiagnosticCallback(Diagnostic callback);
        void SetCommandCallback(Command callback,
                                std::vector<std::string> responseCandidates,
                                std::vector<std::string> predictorCandidates,
                                std::vector<std::string> exposureCandidates = {});
        void SetScopeChoices(std::vector<::rlispstat::core::AnalysisScopeChoice> choices);
        void Show(::rlispstat::core::GeneralizedGLMState const& state);
        void Activate(); void Close();
    private:
        GeneralizedModelView(); void Initialize(); void AttachLifetime();
        void Render(::rlispstat::core::GeneralizedGLMState const& state);
        void InitializeReport();
        void RebuildCoefficientRows(::rlispstat::core::GeneralizedGLMState const& state);
        void RebuildTermTests(::rlispstat::core::GeneralizedGLMState const& state);
        void RebuildControls(); void ConfigureContextMenu();
        Microsoft::UI::Xaml::Window window_{nullptr};
        Microsoft::UI::Xaml::Controls::Grid root_{nullptr};
        Microsoft::UI::Xaml::Controls::TextBlock title_{nullptr},badge_{nullptr},subtitle_{nullptr};
        Microsoft::UI::Xaml::Controls::StackPanel controls_{nullptr};
        Microsoft::UI::Xaml::Controls::StackPanel content_{nullptr};
        Microsoft::UI::Xaml::Controls::TextBlock fitSummary_{nullptr};
        Microsoft::UI::Xaml::Controls::TextBlock fitDetails_{nullptr};
        Microsoft::UI::Xaml::Controls::Grid countFitGrid_{nullptr};
        Microsoft::UI::Xaml::Controls::Grid coefficientGrid_{nullptr};
        Microsoft::UI::Xaml::Controls::TextBlock termTestsTitle_{nullptr};
        Microsoft::UI::Xaml::Controls::Grid termTestsGrid_{nullptr};
        Microsoft::UI::Xaml::Controls::TextBlock status_{nullptr};
        Microsoft::UI::Xaml::Controls::StackPanel warnings_{nullptr};
        Closed closedCallback_; Diagnostic diagnosticCallback_; Command commandCallback_;
        std::vector<std::string> responseCandidates_, predictorCandidates_, exposureCandidates_;
        std::vector<::rlispstat::core::AnalysisScopeChoice> scopeChoices_;
        std::shared_ptr<TableExportService> exporter_;
        std::vector<Microsoft::UI::Xaml::Controls::TextBlock> coefficientCells_;
        std::vector<std::string> coefficientStructure_;
        std::string controlsSignature_;
        bool commandChoicesChanged_=true;
        bool reportInitialized_=false;
        bool coefficientRowsInitialized_=false;
        std::size_t windowCoefficientRows_=static_cast<std::size_t>(-1);
        int renderedFitVersion_=-1;
        bool renderedOk_=false;
        std::string renderedStatus_;
        ::rlispstat::core::GeneralizedGLMState state_; bool presented_=false; bool closed_=false;
    };

    class MixedModelView final : public std::enable_shared_from_this<MixedModelView>
    {
    public:
        using Closed=std::function<void()>;
        using Command=std::function<void(std::vector<std::string> const&)>;
        static std::shared_ptr<MixedModelView> Create();
        void SetClosedCallback(Closed callback);
        void SetCommandCallback(Command callback,
                                ::rlispstat::core::NativeMixedModelState const& nativeState,
                                std::vector<std::string> responseCandidates,
                                std::vector<std::string> fixedEffectCandidates,
                                std::vector<std::string> groupingCandidates,
                                std::vector<std::string> numericCandidates);
        void Show(::rlispstat::core::MixedModelReportState const& state);
        void Activate(); void Close();
    private:
        MixedModelView(); void Initialize(); void AttachLifetime();
        void Render(::rlispstat::core::MixedModelReportState const& state);
        void ConfigureContextMenu();
        void CopyReport();
        Microsoft::UI::Xaml::Window window_{nullptr};
        Microsoft::UI::Xaml::Controls::TextBlock title_{nullptr},badge_{nullptr},subtitle_{nullptr};
        Microsoft::UI::Xaml::Controls::StackPanel content_{nullptr};
        Closed closedCallback_; Command commandCallback_;
        ::rlispstat::core::MixedModelReportState state_;
        ::rlispstat::core::NativeMixedModelState nativeState_;
        std::vector<std::string> responseCandidates_;
        std::vector<std::string> fixedEffectCandidates_;
        std::vector<std::string> groupingCandidates_;
        std::vector<std::string> polynomialCandidates_;
        bool hasNativeState_=false; bool presented_=false; bool closed_=false;
    };

    class RegressionComparisonView final : public std::enable_shared_from_this<RegressionComparisonView>
    {
    public:
        using Closed=std::function<void()>;
        using Command=std::function<void(std::vector<std::string> const&)>;
        using Diagnostic=std::function<void(int, std::string const&)>;
        static std::shared_ptr<RegressionComparisonView> Create();
        void SetClosedCallback(Closed callback);
        void SetCommandCallback(Command callback, Diagnostic diagnostic,
                                std::vector<std::string> numericVariables,
                                std::vector<std::string> availableVariables);
        void SetScopeChoices(std::vector<::rlispstat::core::AnalysisScopeChoice> choices);
        void Show(::rlispstat::core::RegressionComparisonState const& state);
        void ShowMessage(std::string const& message);
        void Activate(); void Close();
    private:
        RegressionComparisonView(); void Initialize(); void AttachLifetime();
        void Render(::rlispstat::core::RegressionComparisonState const& state);
        void RebuildControls();
        void ConfigureContextMenu();
        Microsoft::UI::Xaml::Window window_{nullptr};Microsoft::UI::Xaml::Controls::TextBlock title_{nullptr},badge_{nullptr},subtitle_{nullptr};Microsoft::UI::Xaml::Controls::StackPanel controls_{nullptr},content_{nullptr};
        Closed closedCallback_; Command commandCallback_; Diagnostic diagnosticCallback_;
        ::rlispstat::core::RegressionComparisonState state_;
        std::vector<std::string> numericVariables_,availableVariables_;
        std::vector<::rlispstat::core::AnalysisScopeChoice> scopeChoices_;
        std::shared_ptr<TableExportService> exporter_;
        std::string controlsSignature_;
        bool commandChoicesChanged_=true;
        bool reportInitialized_=false;
        bool presented_=false;bool closed_=false;
    };

    class ModelTrellisView final : public std::enable_shared_from_this<ModelTrellisView>
    {
    public:
        using Closed=std::function<void()>;
        using Command=std::function<void(std::vector<std::string> const&)>;
        static std::shared_ptr<ModelTrellisView> Create();
        void SetClosedCallback(Closed callback);
        void SetCommandCallback(Command callback, std::vector<std::string> predictors,
                                std::vector<std::string> conditioningVariables);
        void Show(::rlispstat::core::ModelTrellisState const& state);
        void Activate(); void Close();
    private:
        ModelTrellisView(); void Initialize(); void AttachLifetime();
        void Render(::rlispstat::core::ModelTrellisState const& state);
        void ConfigureContextMenu();
        Microsoft::UI::Xaml::Window window_{nullptr};
        Microsoft::UI::Xaml::Controls::TextBlock title_{nullptr},badge_{nullptr},subtitle_{nullptr},status_{nullptr};
        Microsoft::UI::Xaml::Controls::Grid panels_{nullptr};
        Closed closedCallback_; Command commandCallback_;
        ::rlispstat::core::ModelTrellisState state_;
        std::shared_ptr<TableExportService> exporter_;
        std::vector<std::string> predictors_,conditioningVariables_; bool presented_=false; bool closed_=false;
    };

    class GeneralizedComparisonView final : public std::enable_shared_from_this<GeneralizedComparisonView>
    {
    public:
        using Closed=std::function<void()>;
        using Command=std::function<void(std::vector<std::string> const&)>;
        using Diagnostic=std::function<void(int, std::string const&)>;
        static std::shared_ptr<GeneralizedComparisonView> Create();
        void SetClosedCallback(Closed callback);
        void SetCommandCallback(Command callback, Diagnostic diagnostic,
                                std::vector<std::string> responseVariables,
                                std::vector<std::string> availableVariables,
                                std::vector<std::string> exposureVariables = {});
        void SetScopeChoices(std::vector<::rlispstat::core::AnalysisScopeChoice> choices);
        void Show(::rlispstat::core::GeneralizedComparisonState const& state);
        void ShowMessage(std::string const& message);
        void Activate(); void Close();
    private:
        GeneralizedComparisonView(); void Initialize(); void AttachLifetime();
        void Render(::rlispstat::core::GeneralizedComparisonState const& state);
        void RebuildControls();
        void ConfigureContextMenu();
        Microsoft::UI::Xaml::Window window_{nullptr};
        Microsoft::UI::Xaml::Controls::TextBlock title_{nullptr},badge_{nullptr},subtitle_{nullptr};
        Microsoft::UI::Xaml::Controls::StackPanel controls_{nullptr},content_{nullptr};
        Closed closedCallback_; Command commandCallback_; Diagnostic diagnosticCallback_;
        ::rlispstat::core::GeneralizedComparisonState state_;
        std::vector<std::string> responseVariables_,availableVariables_,exposureVariables_;
        std::vector<::rlispstat::core::AnalysisScopeChoice> scopeChoices_;
        std::shared_ptr<TableExportService> exporter_;
        std::string controlsSignature_;
        bool commandChoicesChanged_=true;
        bool reportInitialized_=false;
        bool presented_=false;
        bool closed_=false;
    };
}
