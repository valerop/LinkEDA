#pragma once

#include "ApplicationMenu.h"
#include "../../../../core/analysis_scope.h"
#include "../../../../core/dataset_model.h"
#include "../../../../core/selection_model.h"
#include "../../../../core/window_note_model.h"

#include <functional>
#include <chrono>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

namespace winrt::LinkEDA::implementation
{
    class DataSheetView final : public std::enable_shared_from_this<DataSheetView>
    {
    public:
        using SelectionCallback = std::function<void(
            std::string const&, std::set<int> const&, ::rlispstat::core::SelectionMode)>;
        using ClosedCallback = std::function<void()>;
        using ImputationDisplayCallback = std::function<void(
            std::string const&, std::string const&)>;
        using CommandCallback = std::function<std::string(
            std::vector<std::string> const&)>;
        using NoteChangedCallback = std::function<void(
            std::string const&, std::optional<::rlispstat::core::WindowNote> const&)>;

        static std::shared_ptr<DataSheetView> Create();

        void SetSelectionCallback(SelectionCallback callback);
        void SetClosedCallback(ClosedCallback callback);
        void SetImputationDisplayCallback(ImputationDisplayCallback callback);
        void SetCommandCallback(CommandCallback callback);
        void SetNoteChangedCallback(NoteChangedCallback callback)
        { noteChangedCallback_ = std::move(callback); }
        void SetApplicationMenu(std::shared_ptr<ApplicationCommandRegistry> registry,
                                std::string token);
        void Show(::rlispstat::core::DataFrameModel const& dataframe,
                  bool activate = true);
        void RefreshRowColors(std::vector<std::pair<int, std::string>> const& pointColors);
        void RefreshSelection(std::set<int> const& selectedRows);
        void RefreshStructure(::rlispstat::core::DataFrameModel const& dataframe,
                              std::size_t preferredColumn);
        void RefreshColumnMetadata(::rlispstat::core::DataFrameModel const& dataframe,
                                   std::size_t column);
        void RefreshCell(::rlispstat::core::DataFrameModel const& dataframe,
                         int row);
        void SetAnalysisScope(::rlispstat::core::AnalysisScope scope,
                              ::rlispstat::core::AnalysisScope baseScope);
        void SetExcludedRows(std::set<int> rows);
        void SetLabelColumn(std::string labelColumn);
        void SetOperationStatus(std::string const& message);
        ::rlispstat::core::WindowNote const& DocumentNote() const { return note_; }
        void SetDocumentNote(std::optional<::rlispstat::core::WindowNote> const& note)
        {
            note_ = note.value_or(::rlispstat::core::MakeWindowNote("data-sheet-" + group_));
        }
        void Activate();
        void Close();
        std::string const& Group() const;
        winrt::Microsoft::UI::Xaml::Window NativeWindow() const;

    private:
        DataSheetView();
        void Initialize();
        void AttachClosedHandler();
        void RebuildRows(::rlispstat::core::DataFrameModel const& dataframe);
        void PrepareRowContainer(
            winrt::Microsoft::UI::Xaml::Controls::ListViewItem const& item,
            int row);
        void SelectionChanged();
        void SynchronizeListSelection();
        void ApplyRowStyles();
        void ApplyRowStyles(std::set<int> const& rows);
        void ApplyRowStyle(int row);
        void UpdateStatus();
        void QueueActiveDataset();
        winrt::Microsoft::UI::Xaml::Controls::MenuFlyout BuildRowsContextMenu(
            ::rlispstat::core::DataFrameModel const& dataframe, int clickedRow = 0);
        winrt::Microsoft::UI::Xaml::Controls::MenuFlyout BuildColumnContextMenu(
            ::rlispstat::core::DataColumn const& column,
            std::size_t columnIndex,
            bool includeRowActions = false, int clickedRow = 0);
        void AppendRowsContextItems(
            winrt::Microsoft::UI::Xaml::Controls::MenuFlyout const& menu,
            ::rlispstat::core::DataFrameModel const& dataframe,
            bool includeVariableView, int clickedRow = 0);
        void AppendImputationDisplayItems(
            winrt::Microsoft::UI::Xaml::Controls::MenuFlyout const& menu,
            ::rlispstat::core::DataFrameModel const& dataframe);
        void AppendRCodeExportItems(
            winrt::Microsoft::UI::Xaml::Controls::MenuFlyoutSubItem const& exportMenu,
            std::string const& objectKind,
            std::string const& objectId,
            std::string const& column = {});
        void ShowTextEditor(std::wstring const& title,
                            std::wstring const& information,
                            std::wstring const& initialValue,
                            std::vector<std::string> commandPrefix,
                            bool multiline = false);
        void ShowDecimalsEditor(::rlispstat::core::DataColumn const& column);
        void ShowAnnotationEditor();
        void SelectColumn(std::size_t columnIndex);
        void BeginCellEdit(
            winrt::Microsoft::UI::Xaml::Controls::ListViewItem const& item,
            winrt::Microsoft::UI::Xaml::Controls::Border const& border,
            int row, std::size_t column);
        bool CommitCellEdit();
        void CommitCellEditAndMove(bool reverse);
        void QueueCellEdit(int row, std::size_t column);
        void TryBeginPendingCellEdit();
        void CancelCellEdit();
        std::string Dispatch(std::vector<std::string> const& command);

        winrt::Microsoft::UI::Xaml::Window window_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::TextBlock status_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::ListView rowsView_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::ContentDialog editorDialog_{ nullptr };
        winrt::Microsoft::UI::Dispatching::DispatcherQueueTimer diagnosticsTimer_{ nullptr };
        std::shared_ptr<DataSheetMenuHost> applicationMenu_;
        std::shared_ptr<ApplicationCommandRegistry> applicationCommands_;
        ::rlispstat::core::DataFrameModel const* dataframe_ = nullptr;
        std::map<int, winrt::Microsoft::UI::Xaml::Controls::ListViewItem> realizedRows_;
        SelectionCallback selectionCallback_;
        ClosedCallback closedCallback_;
        ImputationDisplayCallback imputationDisplayCallback_;
        CommandCallback commandCallback_;
        NoteChangedCallback noteChangedCallback_;
        std::string group_;
        std::string labelColumn_;
        ::rlispstat::core::WindowNote note_;
        ::rlispstat::core::AnalysisScope analysisScope_;
        ::rlispstat::core::AnalysisScope baseAnalysisScope_;
        int rowCount_ = 0;
        std::size_t columnCount_ = 0;
        std::vector<double> columnWidths_;
        double rowContentWidth_ = 42.0;
        std::size_t selectedCount_ = 0;
        std::set<int> selectedRows_;
        std::set<int> excludedRows_;
        std::set<int> baseScopeRows_;
        std::map<int, std::string> pointColors_;
        int selectedColumn_ = -1;
        int editingRow_ = 0;
        std::size_t editingColumn_ = 0;
        std::wstring editingOriginalValue_;
        winrt::Microsoft::UI::Xaml::Controls::Border editingBorder_{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::TextBox editingTextBox_{ nullptr };
        bool endingCellEdit_ = false;
        bool pendingCellEdit_ = false;
        int pendingEditRow_ = 0;
        std::size_t pendingEditColumn_ = 0;
        bool suppressSelection_ = false;
        bool windowActive_ = false;
        bool listSelectionDirty_ = false;
        bool applicationMenuAttached_ = false;
        bool activeDatasetQueued_ = false;
        bool closed_ = false;
        uint64_t diagnosticMoves_ = 0;
        uint64_t diagnosticResizes_ = 0;
        uint64_t diagnosticLayouts_ = 0;
        uint64_t diagnosticPreparedRows_ = 0;
        uint64_t diagnosticRecycledRows_ = 0;
        double diagnosticPrepareMilliseconds_ = 0.0;
        double diagnosticMaxPrepareMilliseconds_ = 0.0;
        double diagnosticMaxResizeLayoutMilliseconds_ = 0.0;
        bool diagnosticResizePending_ = false;
        std::chrono::steady_clock::time_point diagnosticResizeStarted_{};
    };
}
