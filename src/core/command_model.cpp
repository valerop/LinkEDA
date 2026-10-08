#include "command_model.h"

#include "export_model.h"
#include "glm_model.h"

#include <algorithm>
#include <cctype>
#include <cstdio>
#include <iomanip>
#include <sstream>

namespace rlispstat {
namespace core {

std::string EncodeCommandField(const std::string &text)
{
    std::ostringstream out;
    out << std::uppercase << std::hex;
    for (unsigned char ch : text) {
        if (ch == '|' || ch == '%' || ch == '\n' || ch == '\r' || ch == '\t') {
            out << '%' << std::setw(2) << std::setfill('0') << static_cast<int>(ch);
        } else {
            out << static_cast<char>(ch);
        }
    }
    return out.str();
}

std::string DecodeCommandField(const std::string &text)
{
    std::string out;
    out.reserve(text.size());
    for (std::size_t i = 0; i < text.size(); ++i) {
        if (text[i] == '%' && i + 2 < text.size() &&
            std::isxdigit(static_cast<unsigned char>(text[i + 1])) &&
            std::isxdigit(static_cast<unsigned char>(text[i + 2]))) {
            unsigned int value = 0;
            if (std::sscanf(text.substr(i + 1, 2).c_str(), "%02x", &value) == 1) {
                out.push_back(static_cast<char>(value));
                i += 2;
                continue;
            }
        }
        out.push_back(text[i]);
    }
    return out;
}

std::string EncodeCommandModelSpecTable(const std::vector<CommandModelSpec> &models)
{
    std::ostringstream out;
    for (std::size_t i = 0; i < models.size(); ++i) {
        if (i) out << "\n";
        const CommandModelSpec &model = models[i];
        out << EncodeCommandField(model.label) << "|" << EncodeCommandField(model.response) << "|";
        for (std::size_t j = 0; j < model.terms.size(); ++j) {
            if (j) out << "\t";
            out << EncodeCommandField(model.terms[j]);
        }
    }
    return out.str();
}

std::string ProtocolSafeValue(std::string value)
{
    for (char &ch : value) {
        if (ch == '\r' || ch == '\n' || ch == '\t' || ch == '|') {
            ch = ' ';
        }
    }
    return value;
}

ApplicationMenuTitles DefaultApplicationMenuTitles()
{
    return {
        "LinkEDA",
        "Quit LinkEDA",
        "q",
        "File",
        "Edit",
        "Data",
        "Active Dataset",
        "Record",
        "Plot",
        "Analyze",
        "Variable Type",
        "Color Selected Points"
    };
}

PlotContextMenuTitles DefaultPlotContextMenuTitles()
{
    return {
        "LinkEDA",
        "Variables",
        "Change X variable",
        "Change Y variable",
        "Mouse mode",
        "Selection",
        "Color selected points",
        "Color Selected Rows",
        "Open color palette",
        "Add Column to Data",
        "Labels",
        "View",
        "Theme",
        "Brush",
        "Imputation uncertainty",
        "Regression lines",
        "Analyze This Plot",
        "Analyze This Bar Chart",
        "Analyze This Histogram",
        "Analyze This Boxplot",
        "Plot",
        "Export"
    };
}

DataSheetContextMenuTitles DefaultDataSheetContextMenuTitles()
{
    return {
        "Rows",
        "Color Selected Rows",
        "Clear Row Color",
        "Clear All Row Colors",
        "Variable View",
        "Set variable type to Numeric",
        "Set variable type to Categorical",
        "Set variable type to Text",
        "Show Variable Information",
        "Imputed Data Display",
        "All imputed values separated by |",
        "Original incomplete data"
    };
}

std::string DataSheetImputationVersionTitle(int version)
{
    return "Imputation " + std::to_string(std::max(1, version));
}

VariableViewContextMenuTitles DefaultVariableViewContextMenuTitles()
{
    return {
        "Variable",
        "Default role (Experimental)",
        "Type",
        "Edit Description...",
        "Show Variable Information"
    };
}

ModelContextMenuTitles DefaultModelContextMenuTitles()
{
    return {
        "Model",
        "Statistic",
        "Predictor",
        "Coefficient",
        "Model Fit",
        "Model Comparison",
        "Add term",
        "Add term...",
        "Add model",
        "Add model from active model",
        "Refit Model",
        "Refit active model",
        "Turn Auto-refit On",
        "Turn Auto-refit Off",
        "Open Diagnostics",
        "Copy value",
        "Show statistic details",
        "Show coefficient details",
        "Change variable...",
        "Change type...",
        "Remove predictor",
        "Show variable information",
        "Response Variable",
        "Change Response...",
        "Clear Response",
        "Predictor Terms",
        "Add Term...",
        "Change Selected Term...",
        "Remove Selected Term",
        "Treat predictor as continuous",
        "Treat predictor as categorical",
        "Treat selected predictor as numeric",
        "Treat selected predictor as categorical",
        "Show Selected Term Information",
        "Add/Remove term in this model",
        "Add term to all models",
        "Remove term from comparison",
        "Copy Regression Table",
        "Explain statistic",
        "Open diagnostic plot",
        "Open relevant diagnostic plot",
        "Compare with null model",
        "Interpret interaction...",
        "Effect plot",
        "Partial regression plot",
        "Drop coefficient",
        "No available interactions",
        "Add polynomial term...",
        "Copy Model Table",
        "Copy Model Comparison Table",
        "Export Model Table as PDF...",
        "Show Rows Used/Excluded",
        "Open this model as single GLM window",
        "Open as single GLM window",
        "Open this model as single generalized GLM window",
        "Open as single generalized GLM window",
        "Compare models..."
    };
}

std::vector<ApplicationMenuCommandOption> FileCommandOptions()
{
    return {
        {"Import Data...", "import_data", "FILE_IMPORT_DATA", "o", true, false},
        {"Open Data from R...", "open_data_from_r", "FILE_OPEN_DATA_FROM_R", "", false, false},
        {"Export Data...", "export_data", "FILE_EXPORT_DATA", "", false, false},
        {"Close Data File and Analyses", "close_data_file", "FILE_CLOSE_DATASET", "W", true, true},
        {"Return Data to R...", "return_data_to_r", "FILE_RETURN_DATA_TO_R", "", false, false},
        {"Return Selected Rows to R...", "return_selected_rows_to_r", "FILE_RETURN_SELECTED_ROWS_TO_R", "", false, false},
        {"Cancel without Returning", "cancel_data_return", "FILE_CANCEL_DATA_RETURN", "", false, false}
    };
}

std::vector<std::vector<ApplicationMenuCommandOption>> FileCommandOptionGroups()
{
    std::vector<ApplicationMenuCommandOption> options = FileCommandOptions();
    return {
        {options[0], options[1], options[2], options[3]},
        {options[4], options[5], options[6]}
    };
}

std::vector<ApplicationMenuCommandOption> EditCommandOptions()
{
    return {
        {"Copy", "copy", "EDIT_COPY", "c", true, false},
        {"Copy Selected Row Indices", "copy_selection", "EDIT_COPY_SELECTION", "C", true, true},
        {"Clear Selection", "clear_selection", "EDIT_CLEAR_SELECTION", "", false, false}
    };
}

std::vector<std::vector<ApplicationMenuCommandOption>> RecordCommandOptionGroups()
{
    return {
        {
            {"Start Recording", "start", "RECORD_START", "", false, false},
            {"Stop Recording", "stop", "RECORD_STOP", "", false, false}
        },
        {
            {"Show Recording", "show", "RECORD_SHOW", "", false, false},
            {"Replay Recording", "replay", "RECORD_REPLAY", "", false, false}
        },
        {
            {"Copy R Script", "copy_r", "RECORD_COPY_R", "", false, false},
            {"Save R Script...", "save_r", "RECORD_SAVE_R", "", false, false},
            {"Copy Internal Commands", "copy_internal", "RECORD_COPY_INTERNAL", "", false, false}
        },
        {
            {"Clear Recording", "clear", "RECORD_CLEAR", "", false, false}
        }
    };
}

std::vector<ApplicationMenuCommandOption> ModelingCommandOptions()
{
    struct Actions {
        StatisticalModelType type;
        const char *fit;
        const char *compare;
        const char *fitValue;
        const char *compareValue;
    };
    const std::vector<Actions> actions = {
        {StatisticalModelType::Linear, "ANALYZE_GLM",
         "ANALYZE_REGRESSION_COMPARISON", "linear_model", "compare_linear_models"},
        {StatisticalModelType::Binary, "ANALYZE_BINARY_REGRESSION",
         "ANALYZE_BINARY_REGRESSION_COMPARISON", "binary_model", "compare_binary_models"},
        {StatisticalModelType::Count, "ANALYZE_COUNT_REGRESSION",
         "ANALYZE_COUNT_REGRESSION_COMPARISON", "count_model", "compare_count_models"},
        {StatisticalModelType::PositiveContinuous, "ANALYZE_POSITIVE_CONTINUOUS_MODEL",
         "ANALYZE_POSITIVE_CONTINUOUS_COMPARISON", "positive_continuous_model",
         "compare_positive_continuous_models"},
        {StatisticalModelType::Proportion, "ANALYZE_PROPORTION_MODEL",
         "ANALYZE_PROPORTION_COMPARISON", "proportion_model", "compare_proportion_models"}
    };
    std::vector<ApplicationMenuCommandOption> options;
    for (const auto &action : actions) {
        std::string subgroup = StatisticalModelTypeLabel(action.type);
        constexpr const char suffix[] = " Model";
        if (subgroup.size() >= sizeof(suffix) - 1 &&
            subgroup.compare(subgroup.size() - (sizeof(suffix) - 1),
                             sizeof(suffix) - 1, suffix) == 0) {
            subgroup.erase(subgroup.size() - (sizeof(suffix) - 1));
        }
        const bool linear = action.type == StatisticalModelType::Linear;
        options.push_back({"Fit Model...", action.fitValue, action.fit,
                           linear ? "g" : "", linear, false, false, subgroup});
        options.push_back({"Compare Models...", action.compareValue, action.compare,
                           linear ? "G" : "", linear, linear, false, subgroup});
        if (linear) {
            options.push_back({"Model Trellis...", "linear_model_trellis",
                               "ANALYZE_LINEAR_MODEL_TRELLIS", "", false, false,
                               true, subgroup});
        }
    }
    return options;
}

std::vector<ApplicationMenuCommandGroup> AnalyzeCommandMenuGroups()
{
    return {
        {
            "Descriptives",
            {
                {"Table 1...", "table1", "ANALYZE_TABLE1", "", false, false},
                {"Contingency Table\u2026", "contingency_table", "ANALYZE_CONTINGENCY_TABLE", "", false, false},
                {"Correlation Matrix...", "correlation_matrix", "ANALYZE_CORRELATION_MATRIX", "", false, false},
                {"Principal Components / Factor Analysis...", "dimensionality", "ANALYZE_DIMENSIONALITY", "", false, false},
                {"Quick Cluster...", "quick_cluster", "ANALYZE_QUICK_CLUSTER", "", false, false}
            }
        },
        {
            "Test",
            {
                {"One-Sample Tests...", "one_sample_t", "ANALYZE_ONE_SAMPLE_T", "", false, false},
                {"Two-Sample Tests...", "independent_t", "ANALYZE_INDEPENDENT_T", "", false, false},
                {"Paired-Samples Tests...", "paired_t", "ANALYZE_PAIRED_T", "", false, false},
                {"One-Way ANOVA...", "oneway_anova", "ANALYZE_ONEWAY_ANOVA", "", false, false}
            }
        },
        {
            "",
            {
                {"Scale Analysis...", "scale_analysis", "ANALYZE_SCALE_ANALYSIS", "", false, false}
            }
        },
        {
            "Modeling",
            ModelingCommandOptions()
        },
        {
            // Preserve the implementation for possible future recovery, but
            // keep both entry points outside the normal Analyze menu.
            "Mixed Models",
            {
                {"Linear Mixed Model...", "linear_mixed_model", "ANALYZE_LINEAR_MIXED_MODEL", "", false, false, true},
                {"Generalized Linear Mixed Model...", "generalized_linear_mixed_model", "ANALYZE_GENERALIZED_MIXED_MODEL", "", false, false, true}
            }
        }
    };
}

std::vector<ApplicationMenuCommandGroup> VisibleAnalyzeCommandMenuGroups(
    bool includeExperimental)
{
    std::vector<ApplicationMenuCommandGroup> visible;
    for (const auto &group : AnalyzeCommandMenuGroups()) {
        ApplicationMenuCommandGroup visibleGroup;
        visibleGroup.title = group.title;
        for (const auto &option : group.options) {
            if (includeExperimental || !option.experimental) {
                visibleGroup.options.push_back(option);
            }
        }
        if (!visibleGroup.options.empty()) {
            visible.push_back(std::move(visibleGroup));
        }
    }
    return visible;
}

std::string GlobalAnalysisScopeMenuTitle()
{
    return std::string("Analysis Scope") + "\xE2\x80\xA6";
}

std::string PlotAnalysisScopeMenuTitle()
{
    return std::string("Global Analysis Scope") + "\xE2\x80\xA6";
}

std::vector<ApplicationMenuCommandOption> AnalysisScopeCommandOptions()
{
    return {
        {"Use Current Selection", "selection", "SET_ANALYSIS_SCOPE_FROM_SELECTION", "", false, false},
        {"Use Unselected Observations", "unselected", "SET_ANALYSIS_SCOPE_UNSELECTED", "", false, false},
        {"Save Current Selection as Scope...", "save-selection", "SAVE_ANALYSIS_SCOPE_FROM_SELECTION", "", false, false},
        {"Use All Observations", "all", "SET_ANALYSIS_SCOPE_ALL", "", false, false}
    };
}

std::vector<ApplicationMenuCommandOption> PlotCommandOptions()
{
    std::vector<ApplicationMenuCommandOption> options;
    for (const LinkedPlotCommandOption &plot : LinkedPlotCommandOptions()) {
        const bool scatterplot = plot.command == "PLOT_NEW_LINKED_SCATTERPLOT";
        options.push_back({
            plot.title,
            plot.value,
            plot.command,
            scatterplot ? "n" : "",
            scatterplot,
            false
        });
    }
    return options;
}

std::vector<ApplicationMenuCommandOption> TrellisPlotStartCommandOptions()
{
    return {
        {"Scatterplot...", "scatter", "PLOT_NEW_TRELLIS_SCATTERPLOT|scatter", "", false, false},
        {"Time Series...", "time_series", "PLOT_NEW_TRELLIS_SCATTERPLOT|time_series", "", false, false},
        {"Boxplot...", "boxplot", "PLOT_NEW_TRELLIS_SCATTERPLOT|boxplot", "", false, false},
        {"Bar Chart...", "bar", "PLOT_NEW_TRELLIS_SCATTERPLOT|bar", "", false, false},
        {"Histogram...", "histogram", "PLOT_NEW_TRELLIS_SCATTERPLOT|histogram", "", false, false}
    };
}

std::vector<LinkedPlotCommandOption> LinkedPlotCommandOptions()
{
    return {
        {"Scatterplot...", "scatterplot", "PLOT_NEW_LINKED_SCATTERPLOT"},
        {"Trellis Plots", "trellis_scatterplot", "PLOT_NEW_TRELLIS_SCATTERPLOT"},
        {"Time Series...", "time_series", "PLOT_NEW_TIME_SERIES"},
        {"Scatterplot Matrix...", "scatter_matrix", "PLOT_NEW_LINKED_SCATTER_MATRIX"},
        {"Parallel Coordinates...", "parallel_coordinates", "PLOT_NEW_PARALLEL_COORDINATES"},
        {"Boxplot...", "boxplot", "PLOT_NEW_LINKED_BOXPLOT"},
        {"Histogram...", "histogram", "PLOT_NEW_LINKED_HISTOGRAM"},
        {"Bar Chart...", "bar_chart", "PLOT_NEW_LINKED_BAR_CHART"}
    };
}

std::vector<ApplicationMenuCommandOption> ActiveDatasetCommandOptions(const std::vector<std::string> &groups)
{
    std::vector<ApplicationMenuCommandOption> options;
    options.reserve(groups.size());
    for (const std::string &group : groups) {
        options.push_back({
            group,
            group,
            "DATA_SET_ACTIVE_DATASET|" + group,
            "",
            false,
            false
        });
    }
    return options;
}

std::string NoRegisteredDatasetsTitle()
{
    return "No Registered Datasets";
}

std::vector<LinkedPlotCommandOption> SelectionCommandOptions(bool includeSelectAllVisible)
{
    std::vector<LinkedPlotCommandOption> options = {
        {"Clear Selection", "clear", "CLEAR_SELECTION"}
    };
    if (includeSelectAllVisible) {
        options.push_back({"Select All Visible", "select_all_visible", "SELECT_ALL_VISIBLE"});
    }
    options.push_back({"Invert Selection", "invert", "INVERT_SELECTION"});
    return options;
}

std::vector<std::vector<LinkedPlotCommandOption>> PlotExportCommandOptionGroups(bool includeSelectedRows)
{
    std::vector<std::vector<LinkedPlotCommandOption>> groups;
    std::vector<LinkedPlotCommandOption> current;
    for (const ExportMenuAction &action : BuildExportMenuActions(
             StandardVisualExportCapabilities(ExportPlatform::MacOS), ExportPlatform::MacOS)) {
        if (action.separatorBefore && !current.empty()) {
            groups.push_back(current);
            current.clear();
        }
        current.push_back({action.title, action.identifier, action.command});
    }
    if (!current.empty()) groups.push_back(current);
    if (includeSelectedRows) {
        groups.push_back({
            {"Copy selected row indices", "copy_selected", "COPY_SELECTED"}
        });
    }
    return groups;
}

std::vector<LinkedPlotCommandOption> DataWindowCommandOptions()
{
    return {
        {"Show Active Dataset", "show_active_dataset", "DATA_SHOW_ACTIVE_DATASET"},
        {"Open Data Sheet", "open_data_sheet", "DATA_OPEN_DATA_SHEET"},
        {"Variable View", "variable_view", "DATA_VARIABLE_VIEW"},
        {"Choose Label Column...", "choose_label_column", "DATA_CHOOSE_LABEL_COLUMN"}
    };
}

std::vector<LinkedPlotCommandOption> DataColumnCommandOptions(const std::string &group)
{
    const std::string suffix = group.empty() ? "" : "|" + group;
    return {
        {"Selection: selected / not selected", "selection", "DATA_ADD_SELECTION_COLUMN" + suffix},
        {"Point colors", "point_colors", "DATA_ADD_POINT_COLOR_COLUMN" + suffix}
    };
}

std::vector<LinkedPlotCommandOption> DataVariableTypeCommandOptions()
{
    return {
        {"Set active X variable type to Numeric", "numeric", "DATA_VARIABLE_TYPE_NUMERIC"},
        {"Set active X variable type to Categorical", "factor", "DATA_VARIABLE_TYPE_FACTOR"}
    };
}

std::vector<LinkedPlotCommandOption> VariableViewRoleOptions()
{
    return {
        {"Dependent", "dependent", ""},
        {"Independent", "independent", ""},
        {"None", "none", ""}
    };
}

std::vector<LinkedPlotCommandOption> VariableViewTypeOptions()
{
    return {
        {"Numeric", "numeric", ""},
        {"Categorical", "factor", ""},
        {"Ordinal", "ordered", ""},
        {"Text", "character", ""}
    };
}

std::vector<LinkedPlotCommandOption> ModelTermTypeOptions()
{
    return {
        {"Numeric", "numeric", ""},
        {"Categorical", "factor", ""}
    };
}

std::vector<LinkedPlotCommandOption> ModelDiagnosticPlotOptions(bool includeExtended)
{
    // All regression surfaces share the same diagnostic contract.  The
    // argument remains for source compatibility with older platform code,
    // but a caller can no longer accidentally request a reduced menu.
    (void)includeExtended;
    return {
        {"Observed vs fitted", "observed_fitted", ""},
        {"Residuals vs fitted", "residuals_fitted", ""},
        {"Residual histogram", "residual_histogram", ""},
        {"Normal Q-Q of residuals", "normal_qq", ""},
        {"Scale-location", "scale_location", ""},
        {"Residuals vs leverage", "residuals_leverage", ""},
        {"Cook's distance", "cooks_distance", ""}
    };
}

std::string PredictorTypeMenuTitle()
{
    return "Predictor type";
}

std::string TermTypeMenuTitle()
{
    return "Term type";
}

std::string AutoRefitMenuTitle(bool autoRefit)
{
    ModelContextMenuTitles titles = DefaultModelContextMenuTitles();
    return autoRefit ? titles.autoRefitOff : titles.autoRefitOn;
}

ApplicationMenuCommandOption DataPointsColorPaletteCommandOption()
{
    return {"Open Data Points Color Palette...", "open_color_palette", "DATA_POINTS_COLOR", "", false, false};
}

ApplicationMenuCommandOption DataVariableInformationCommandOption()
{
    return {"Show Variable Information", "variable_information", "DATA_SHOW_VARIABLE_INFORMATION", "", false, false};
}

ApplicationMenuCommandOption DataRefreshCommandOption()
{
    return {"Refresh R Data Frames", "refresh_r_dataframes", "DATA_REFRESH_R_DATAFRAMES", "", false, false};
}

bool IsLinkedPlotCommandName(const std::string &commandName)
{
    if (commandName == "PLOT_NEW_SCATTERPLOT" ||
        commandName == "PLOT_NEW_LINKED_BARCHART") {
        return true;
    }
    std::vector<LinkedPlotCommandOption> options = LinkedPlotCommandOptions();
    return std::any_of(options.begin(), options.end(),
                       [&](const LinkedPlotCommandOption &option) {
                           return option.command == commandName;
                       });
}

bool IsDataColumnCommandName(const std::string &commandName)
{
    std::vector<LinkedPlotCommandOption> options = DataColumnCommandOptions();
    return std::any_of(options.begin(), options.end(),
                       [&](const LinkedPlotCommandOption &option) {
                           return option.command == commandName;
                       });
}

bool IsNativeUiDispatchCommandName(const std::string &commandName)
{
    if (commandName == "DATA_MISSING_DATA_OVERVIEW" || commandName == "DATA_MISSINGNESS_MODELS" || commandName == "DATA_IMPUTATION_DIAGNOSTICS" ||
        commandName == "DATA_CLOSE_DATA_SHEET" ||
        commandName == "FILE_IMPORT_DATA" ||
        commandName == "FILE_EXPORT_DATA" ||
        commandName == "FILE_OPEN_DATA_FROM_R" ||
        commandName == "FILE_RETURN_DATA_TO_R" ||
        commandName == "FILE_RETURN_SELECTED_ROWS_TO_R" ||
        commandName == "FILE_CANCEL_DATA_RETURN" ||
        commandName == "DATA_REFRESH_R_DATAFRAMES" ||
        commandName == "DATA_SHOW_ACTIVE_DATASET" ||
        commandName == "DATA_SET_ACTIVE_DATASET" ||
        commandName == "DATA_CHOOSE_LABEL_COLUMN" ||
        commandName == "DATA_VARIABLE_VIEW" ||
        commandName == "DATA_POINTS_COLOR" ||
        // The generic generalized-model entries are intentionally absent from
        // the user-facing Models menu.  Keep their legacy dispatch names
        // registered so older .linkeda documents and integrations continue to
        // resolve to the shared generalized-model infrastructure.
        commandName == "ANALYZE_GENERALIZED_GLM" ||
        commandName == "ANALYZE_GENERALIZED_COMPARISON" ||
        commandName == "OPEN_DENDROGRAM") {
        return true;
    }
    if (IsDataColumnCommandName(commandName) || IsLinkedPlotCommandName(commandName)) {
        return true;
    }
    std::vector<ApplicationMenuCommandGroup> analyzeGroups = AnalyzeCommandMenuGroups();
    for (const ApplicationMenuCommandGroup &group : analyzeGroups) {
        for (const ApplicationMenuCommandOption &option : group.options) {
            if (option.command == commandName) {
                return true;
            }
        }
    }
    return false;
}

namespace {

bool StartsWith(const std::string &value, const std::string &prefix)
{
    return value.rfind(prefix, 0) == 0;
}

std::string CommandHead(const std::string &command)
{
    std::size_t pipe = command.find('|');
    return pipe == std::string::npos ? command : command.substr(0, pipe);
}

std::vector<std::string> SplitMenuArgs(const std::string &command)
{
    std::vector<std::string> args;
    std::size_t start = command.find('|');
    if (start == std::string::npos) {
        return args;
    }
    ++start;
    while (start <= command.size()) {
        std::size_t next = command.find('|', start);
        if (next == std::string::npos) {
            args.push_back(command.substr(start));
            break;
        }
        args.push_back(command.substr(start, next - start));
        start = next + 1;
    }
    return args;
}

CommandCategory Categorize(const std::string &name)
{
    if (name == "PING" || name == "LIST_PLOTS" || name == "PLOT_INFO" ||
        name == "GROUPS" || name == "GROUP_INFO" || name == "DATASET_SYNC_STATUS" || name == "VARIABLES" ||
        name == "GET_XVAR" || name == "GET_YVAR" || name == "DIAGNOSTIC_INFO" ||
        name == "GET_ANALYSIS_SCOPE" || name == "GET_SAVED_ANALYSIS_SCOPES") {
        return CommandCategory::Protocol;
    }
    if (StartsWith(name, "RECORD_")) {
        return CommandCategory::Recording;
    }
    if (name == "PALETTE" || StartsWith(name, "PALETTE_") ||
        name == "OPEN_COLOR_PALETTE" || name == "DATA_POINTS_COLOR") {
        return CommandCategory::Palette;
    }
    if (name == "PANEL" || StartsWith(name, "PANEL_") ||
        name == "RESET_WINDOW_LAYOUT" || name == "TILE_WINDOWS" ||
        name == "FILE_CLOSE_WORKBENCH" || name == "CLOSE_ALL") {
        return CommandCategory::Window;
    }
    if (name == "REGISTER_DATASET" || name == "REGISTER_DATASET_SILENT" ||
        name == "SET_ACTIVE_DATASET" || name == "NATIVE_IMPORT_FILE" ||
        name == "OPEN_VARIABLES_WINDOW" || name == "SET_VARIABLE_TYPE" ||
        name == "SET_DEFAULT_VARIABLE_ROLE" ||
        name == "SET_DATA_CELL" || name == "RENAME_VARIABLE" ||
        name == "SET_VARIABLE_DESCRIPTION" || name == "SET_VARIABLE_DECIMALS" ||
        name == "SET_LABEL_COLUMN" ||
        StartsWith(name, "DATA_") || StartsWith(name, "VARIABLE_")) {
        return CommandCategory::Dataset;
    }
    if (name == "SELECTED" || name == "SET_SELECTED" || name == "SELECT_ALL" ||
        name == "INVERT" || name == "INVERT_SELECTION" || name == "CLEAR" ||
        name == "CLEAR_SELECTION" ||
        name == "SELECT_ALL_VISIBLE" || name == "SET_SELECTED_COLOR" ||
        name == "RESET_SELECTED_COLOR" || name == "GET_SELECTED_COLOR" ||
        name == "SET_POINT_COLOR" || name == "CLEAR_ROW_COLORS" ||
        StartsWith(name, "SELECTION_") || StartsWith(name, "SET_ANALYSIS_SCOPE_") ||
        name == "SAVE_ANALYSIS_SCOPE_FROM_SELECTION" ||
        name == "USE_SAVED_ANALYSIS_SCOPE" || name == "ADD_SAVED_ANALYSIS_SCOPE" ||
        name == "TOGGLE_CASE_INCLUDED") {
        return CommandCategory::Selection;
    }
    if (StartsWith(name, "MODEL_") || StartsWith(name, "REGCMP_") ||
        StartsWith(name, "GENERALIZED_GLM_") || StartsWith(name, "MIXED_MODEL_") ||
        name == "GLM" || name == "MODEL_INFO") {
        return CommandCategory::Model;
    }
    if (StartsWith(name, "ANALYZE_") || StartsWith(name, "OPEN_") ||
        StartsWith(name, "CORR_") || StartsWith(name, "PCAFA_") ||
        StartsWith(name, "DENDRO_") || StartsWith(name, "TABLE1_") ||
        StartsWith(name, "COMPARE_MEANS_")) {
        return CommandCategory::Analysis;
    }
    if (StartsWith(name, "PLOT_") || StartsWith(name, "SET_XVAR") ||
        StartsWith(name, "SET_YVAR") || StartsWith(name, "CHANGE_X_VARIABLE") ||
        StartsWith(name, "CHANGE_Y_VARIABLE") || StartsWith(name, "ADD_PLOT") ||
        StartsWith(name, "ADD_BOXPLOT") || StartsWith(name, "ADD_HISTOGRAM") ||
        StartsWith(name, "ADD_BARPLOT") || StartsWith(name, "BOXPLOT_") ||
        StartsWith(name, "HIST_") || StartsWith(name, "BARPLOT_") ||
        StartsWith(name, "PCA_BIPLOT_") || StartsWith(name, "SET_IMPUTATION_DISPLAY") ||
        StartsWith(name, "SET_IMPUTATION_UNCERTAINTY") ||
        StartsWith(name, "SCATTER_MATRIX_") || StartsWith(name, "TRELLIS_SCATTERPLOT_") || StartsWith(name, "SET_MODE_") ||
        StartsWith(name, "SET_SELECTION_") || name == "MODE" ||
        name == "SELECTION_OPERATION" || name == "RESET_ZOOM" ||
        name == "RESCALE" || name == "REDRAW" || name == "CLOSE_PLOT" ||
        name == "SAVE_SVG" || name == "SAVE_PNG" || name == "SAVE_PDF" ||
        name == "COPY_SVG" || name == "COPY_EMF" ||
        name == "COPY_PNG" || name == "COPY_PDF" ||
        name == "COPY_SELECTED" || StartsWith(name, "BRUSH_") ||
        StartsWith(name, "TOGGLE_LM_") || StartsWith(name, "ADD_LM_") ||
        name == "ADD_LM" || name == "OVERLAYS" || name == "CLEAR_OVERLAYS" ||
        StartsWith(name, "SET_LABEL_DISPLAY") || StartsWith(name, "TIME_SERIES_")) {
        return CommandCategory::Plot;
    }
    if (StartsWith(name, "CONTEXT_")) {
        return CommandCategory::Analysis;
    }
    return CommandCategory::Unknown;
}

CommandAction ResolveAction(const std::string &name)
{
    if (StartsWith(name, "RECORD_")) return CommandAction::RecordingCommand;
    if (name == "PING") return CommandAction::Ping;
    if (name == "NATIVE_IMPORT_FILE") return CommandAction::NativeImportFile;
    if (name == "LIST_PLOTS") return CommandAction::ListPlots;
    if (name == "PLOT_INFO") return CommandAction::PlotInfo;
    if (name == "EXPORT_PLOT") return CommandAction::ExportPlot;
    if (name == "COPY_PLOT") return CommandAction::CopyPlot;
    if (name == "PLOT_THEME") return CommandAction::PlotTheme;
    if (name == "DIAGNOSTIC_INFO") return CommandAction::DiagnosticInfo;
    if (name == "GROUPS") return CommandAction::Groups;
    if (name == "GROUP_INFO") return CommandAction::GroupInfo;
    if (name == "DATASET_SYNC_STATUS") return CommandAction::DatasetSyncStatus;
    if (name == "VARIABLES") return CommandAction::Variables;
    if (name == "GET_XVAR") return CommandAction::GetXVariable;
    if (name == "GET_YVAR") return CommandAction::GetYVariable;
    if (name == "GET_ANALYSIS_SCOPE") return CommandAction::GetAnalysisScope;
    if (name == "GET_SAVED_ANALYSIS_SCOPES") return CommandAction::GetSavedAnalysisScopes;
    if (name == "GET_EXCLUDED_ROWS") return CommandAction::GetExcludedRows;
    if (name == "EXCLUDE_SELECTED_CASES") return CommandAction::ExcludeSelectedCases;
    if (name == "INCLUDE_SELECTED_CASES") return CommandAction::IncludeSelectedCases;
    if (name == "INCLUDE_ALL_CASES") return CommandAction::IncludeAllCases;
    if (name == "TOGGLE_CASE_INCLUDED") return CommandAction::ToggleCaseIncluded;
    if (name == "SET_ANALYSIS_SCOPE_ALL") return CommandAction::SetAnalysisScopeAll;
    if (name == "SET_ANALYSIS_SCOPE_FROM_SELECTION" || name == "SET_ANALYSIS_SCOPE_UNSELECTED") return CommandAction::SetAnalysisScopeFromSelection;
    if (name == "SET_ANALYSIS_SCOPE_FROM_ROWS") return CommandAction::SetAnalysisScopeFromRows;
    if (name == "SAVE_ANALYSIS_SCOPE_FROM_SELECTION") return CommandAction::SaveAnalysisScopeFromSelection;
    if (name == "USE_SAVED_ANALYSIS_SCOPE") return CommandAction::UseSavedAnalysisScope;
    if (name == "ADD_SAVED_ANALYSIS_SCOPE") return CommandAction::AddSavedAnalysisScope;
    if (name == "PLOT_TOGGLE_SCOPE_FREEZE") return CommandAction::TogglePlotScopeFreeze;
    if (name == "OPEN_EQUIVALENT_WITH_ACTIVE_SCOPE") return CommandAction::OpenEquivalentWithActiveScope;
    if (name == "OPEN_EQUIVALENT_WITH_ALL_ROWS") return CommandAction::OpenEquivalentWithAllRows;
    if (name == "FILE_IMPORT_DATA") return CommandAction::FileImportData;
    if (name == "FILE_EXPORT_DATA") return CommandAction::FileExportData;
    if (name == "FILE_OPEN_DATA_FROM_R") return CommandAction::FileOpenDataFromR;
    if (name == "FILE_RETURN_DATA_TO_R") return CommandAction::FileReturnDataToR;
    if (name == "FILE_RETURN_SELECTED_ROWS_TO_R") return CommandAction::FileReturnSelectedRowsToR;
    if (name == "FILE_CANCEL_DATA_RETURN") return CommandAction::FileCancelDataReturn;
    if (name == "FILE_OPEN_DATA_SHEET" || name == "DATA_OPEN_DATA_SHEET") {
        return CommandAction::OpenDataSheet;
    }
    if (name == "REGISTER_DATASET") return CommandAction::RegisterDataset;
    if (name == "REGISTER_DATASET_SILENT") return CommandAction::RegisterDatasetSilent;
    if (name == "SET_ACTIVE_DATASET") return CommandAction::SetActiveDataset;
    if (name == "DATA_SET_ACTIVE_DATASET") return CommandAction::DataSetActiveDataset;
    if (name == "DATA_SHOW_ACTIVE_DATASET") return CommandAction::ShowActiveDataset;
    if (name == "DATA_CHOOSE_LABEL_COLUMN" || name == "CHOOSE_LABEL_COLUMN") {
        return CommandAction::ChooseLabelColumn;
    }
    if (name == "DATA_VARIABLE_VIEW" || name == "OPEN_VARIABLES_WINDOW" ||
        name == "VARIABLES_WINDOW") {
        return CommandAction::VariableView;
    }
    if (name == "DATA_ADD_SELECTION_COLUMN") return CommandAction::AddSelectionColumn;
    if (name == "DATA_ADD_POINT_COLOR_COLUMN") return CommandAction::AddPointColorColumn;
    if (name == "DATA_VARIABLE_TYPE_NUMERIC") return CommandAction::SetVariableTypeNumeric;
    if (name == "DATA_VARIABLE_TYPE_FACTOR") return CommandAction::SetVariableTypeFactor;
    if (name == "SET_VARIABLE_TYPE") return CommandAction::SetVariableType;
    if (name == "SET_DEFAULT_VARIABLE_ROLE") return CommandAction::SetDefaultVariableRole;
    if (name == "SET_DATA_CELL") return CommandAction::SetDataCell;
    if (name == "RENAME_VARIABLE") return CommandAction::RenameVariable;
    if (name == "SET_VARIABLE_DESCRIPTION") return CommandAction::SetVariableDescription;
    if (name == "SET_VARIABLE_DECIMALS") return CommandAction::SetVariableDecimals;
    if (name == "SET_LABEL_COLUMN") return CommandAction::SetLabelColumn;
    if (name == "DATA_SHOW_VARIABLE_INFORMATION" || name == "VARIABLE_INFO") {
        return CommandAction::ShowVariableInformation;
    }
    if (name == "SHOW_R_CODE") return CommandAction::ShowRCode;
    if (name == "SHOW_R_PUBLICATION_CODE") return CommandAction::ShowRPublicationCode;
    if (name == "DATA_MAKE_SUBSET_FROM_SELECTION") return CommandAction::MakeSubsetFromSelection;
    if (name == "DATA_MAKE_SUBSET_FROM_SAVED_SELECTION") return CommandAction::MakeSubsetFromSavedSelection;
    if (name == "DATA_MAKE_SUBSET_FROM_VARIABLES") return CommandAction::MakeSubsetFromVariables;
    if (name == "MODEL_INFO") return CommandAction::ModelInfo;
    if (name == "MODEL_SET_Y") return CommandAction::ModelSetY;
    if (name == "MODEL_ADD_TERM") return CommandAction::ModelAddTerm;
    if (name == "MODEL_REPLACE_TERM") return CommandAction::ModelReplaceTerm;
    if (name == "MODEL_REMOVE_TERM") return CommandAction::ModelRemoveTerm;
    if (name == "MODEL_CLEAR_ROLE") return CommandAction::ModelClearRole;
    if (name == "MODEL_SCOPE") return CommandAction::ModelScope;
    if (name == "MODEL_OPEN") return CommandAction::ModelOpen;
    if (name == "MODEL_OPEN_RESIDUALS_FITTED") return CommandAction::ModelOpenResidualsFitted;
    if (name == "MODEL_OPEN_OBSERVED_FITTED") return CommandAction::ModelOpenObservedFitted;
    if (name == "MODEL_OPEN_DIAGNOSTICS") return CommandAction::ModelOpenDiagnostics;
    if (name == "MODEL_OPEN_DIAGNOSTIC") return CommandAction::ModelOpenDiagnostic;
    if (name == "MODEL_INTERACTION_REPORT") return CommandAction::ModelInteractionReport;
    if (name == "MODEL_OPEN_INTERACTION_PLOT") return CommandAction::ModelOpenInteractionPlot;
    if (name == "DATA_REFRESH_R_DATAFRAMES") return CommandAction::RefreshRDataFrames;
    if (name == "PANEL") return CommandAction::Panel;
    if (name == "PANEL_SHOW") return CommandAction::PanelShow;
    if (name == "PANEL_HIDE") return CommandAction::PanelHide;
    if (name == "RESET_WINDOW_LAYOUT" || name == "TILE_WINDOWS") {
        return CommandAction::ResetWindowLayout;
    }
    if (name == "OPEN_COLOR_PALETTE" || name == "PALETTE_SHOW" ||
        name == "DATA_POINTS_COLOR") {
        return CommandAction::OpenColorPalette;
    }
    if (name == "PALETTE_HIDE") return CommandAction::PaletteHide;
    if (name == "PALETTE") return CommandAction::PaletteState;
    if (name == "PLOT_NEW_SCATTERPLOT" || name == "PLOT_NEW_LINKED_SCATTERPLOT") {
        return CommandAction::NewScatterplot;
    }
    if (name == "PLOT_NEW_TRELLIS_SCATTERPLOT") {
        return CommandAction::NewTrellisScatterplot;
    }
    if (name == "TRELLIS_SCATTERPLOT_SET_X") {
        return CommandAction::SetTrellisScatterplotX;
    }
    if (name == "TRELLIS_SCATTERPLOT_SET_Y") {
        return CommandAction::SetTrellisScatterplotY;
    }
    if (name == "TRELLIS_SCATTERPLOT_SET_CONDITION") {
        return CommandAction::SetTrellisScatterplotCondition;
    }
    if (name == "TRELLIS_SCATTERPLOT_SET_LAYOUT") {
        return CommandAction::SetTrellisScatterplotLayout;
    }
    if (name == "TRELLIS_SCATTERPLOT_SET_ORDER") {
        return CommandAction::SetTrellisScatterplotOrder;
    }
    if (name == "TRELLIS_SCATTERPLOT_SET_TYPE") return CommandAction::SetTrellisPlotType;
    if (name == "TRELLIS_SCATTERPLOT_SET_TYPE_WITH_X") return CommandAction::SetTrellisPlotTypeWithX;
    if (name == "TRELLIS_SCATTERPLOT_SET_BOXPLOT_GROUPS") return CommandAction::SetTrellisBoxplotGroupingVariables;
    if (name == "TRELLIS_SCATTERPLOT_BOXPLOT_ADD_GROUP") return CommandAction::AddTrellisBoxplotGroupingVariable;
    if (name == "TRELLIS_SCATTERPLOT_BOXPLOT_REPLACE_GROUP") return CommandAction::ReplaceTrellisBoxplotGroupingVariable;
    if (name == "TRELLIS_SCATTERPLOT_BOXPLOT_REMOVE_GROUP") return CommandAction::RemoveTrellisBoxplotGroupingVariable;
    if (name == "TRELLIS_SCATTERPLOT_BOXPLOT_MOVE_GROUP_EARLIER") return CommandAction::MoveTrellisBoxplotGroupingVariableEarlier;
    if (name == "TRELLIS_SCATTERPLOT_BOXPLOT_MOVE_GROUP_LATER") return CommandAction::MoveTrellisBoxplotGroupingVariableLater;
    if (name == "TRELLIS_SCATTERPLOT_ADD_CONDITION_CATEGORICAL") return CommandAction::AddTrellisConditionCategorical;
    if (name == "TRELLIS_SCATTERPLOT_ADD_CONDITION_ORDERED") return CommandAction::AddTrellisConditionOrdered;
    if (name == "TRELLIS_SCATTERPLOT_ADD_CONDITION_EQUAL_WIDTH") return CommandAction::AddTrellisConditionEqualWidth;
    if (name == "TRELLIS_SCATTERPLOT_ADD_CONDITION_EQUAL_COUNT") return CommandAction::AddTrellisConditionEqualCount;
    if (name == "TRELLIS_SCATTERPLOT_SET_CONDITION_FACTOR") return CommandAction::SetTrellisConditionFactor;
    if (name == "TRELLIS_SCATTERPLOT_SET_CONDITION_ORDERED") return CommandAction::SetTrellisConditionOrdered;
    if (name == "TRELLIS_SCATTERPLOT_SET_CONDITION_EQUAL_WIDTH") return CommandAction::SetTrellisConditionEqualWidth;
    if (name == "TRELLIS_SCATTERPLOT_SET_CONDITION_EQUAL_COUNT") return CommandAction::SetTrellisConditionEqualCount;
    if (name == "TRELLIS_SCATTERPLOT_REMOVE_CONDITION") return CommandAction::RemoveTrellisCondition;
    if (name == "TRELLIS_SCATTERPLOT_MOVE_CONDITION_EARLIER") return CommandAction::MoveTrellisConditionEarlier;
    if (name == "TRELLIS_SCATTERPLOT_MOVE_CONDITION_LATER") return CommandAction::MoveTrellisConditionLater;
    if (name == "TRELLIS_SCATTERPLOT_CONDITION_EQUAL_WIDTH") return CommandAction::ConfigureTrellisConditionEqualWidth;
    if (name == "TRELLIS_SCATTERPLOT_CONDITION_EQUAL_COUNT") return CommandAction::ConfigureTrellisConditionEqualCount;
    if (name == "TRELLIS_SCATTERPLOT_CONDITION_BINS") return CommandAction::ConfigureTrellisConditionBins;
    if (name == "TRELLIS_SCATTERPLOT_DIMENSION_ROWS") return CommandAction::SetTrellisConditionDimensionRows;
    if (name == "TRELLIS_SCATTERPLOT_DIMENSION_COLUMNS") return CommandAction::SetTrellisConditionDimensionColumns;
    if (name == "TRELLIS_SCATTERPLOT_DIMENSION_NESTED") return CommandAction::SetTrellisConditionDimensionNested;
    if (name == "TRELLIS_SCATTERPLOT_SWAP_DIMENSIONS") return CommandAction::SwapTrellisRowsAndColumns;
    if (name == "TRELLIS_SCATTERPLOT_TOGGLE_Y_AXIS_SIDE") return CommandAction::ToggleTrellisYAxisSide;
    if (name == "TRELLIS_SCATTERPLOT_SET_SPLIT") return CommandAction::SetTrellisSplitVariable;
    if (name == "TRELLIS_SCATTERPLOT_SET_SCALE") return CommandAction::SetTrellisScaleMode;
    if (name == "TRELLIS_SCATTERPLOT_SET_BAR_MEASURE") return CommandAction::SetTrellisBarMeasure;
    if (name == "TRELLIS_SCATTERPLOT_SET_HISTOGRAM_MEASURE") return CommandAction::SetTrellisHistogramMeasure;
    if (name == "TRELLIS_SCATTERPLOT_SET_HISTOGRAM_BINS") return CommandAction::SetTrellisHistogramBins;
    if (name == "TRELLIS_SCATTERPLOT_TOGGLE_CONNECT") return CommandAction::ToggleTrellisConnectObservations;
    if (name == "PLOT_NEW_TIME_SERIES") {
        return CommandAction::NewTimeSeries;
    }
    if (name == "TIME_SERIES_SET_GROUP") {
        return CommandAction::SetTimeSeriesGroup;
    }
    if (name == "TIME_SERIES_SET_IDENTIFICATION") {
        return CommandAction::SetTimeSeriesIdentification;
    }
    if (name == "TIME_SERIES_SET_LEGEND_POSITION") {
        return CommandAction::SetTimeSeriesLegendPosition;
    }
    if (name == "PLOT_INTERACTION_SET_LEGEND_POSITION") {
        return CommandAction::SetInteractionLegendPosition;
    }
    if (name == "PLOT_REGRESSION_TOGGLE_CONNECTING_LINE") {
        return CommandAction::ToggleRegressionConnectingLine;
    }
    if (name == "PLOT_REGRESSION_TOGGLE_CONFIDENCE_INTERVALS") {
        return CommandAction::ToggleRegressionConfidenceIntervals;
    }
    if (name == "PLOT_SMOOTH_TOGGLE_CONFIDENCE_INTERVALS") {
        return CommandAction::ToggleSmoothConfidenceIntervals;
    }
    if (name == "PLOT_REGRESSION_TOGGLE_CONFIDENCE_LEVEL") {
        return CommandAction::ToggleRegressionConfidenceLevel;
    }
    if (name == "PLOT_REGRESSION_SET_EFFECT_X") {
        return CommandAction::SetRegressionEffectXAxis;
    }
    if (name == "PLOT_REGRESSION_SET_EFFECT_QUANTITY") {
        return CommandAction::SetRegressionEffectQuantity;
    }
    if (name == "PLOT_REGRESSION_SET_EFFECT_ADJUSTMENT") {
        return CommandAction::SetRegressionEffectAdjustment;
    }
    if (name == "PLOT_REGRESSION_SET_EFFECT_PRESENTATION") {
        return CommandAction::SetRegressionEffectPresentation;
    }
    if (name == "PLOT_REGRESSION_SET_EFFECT_CONFIDENCE") {
        return CommandAction::SetRegressionEffectConfidence;
    }
    if (name == "PLOT_NEW_LINKED_SCATTER_MATRIX") {
        return CommandAction::NewScatterMatrix;
    }
    if (name == "PLOT_NEW_PARALLEL_COORDINATES") {
        return CommandAction::NewParallelCoordinates;
    }
    if (name == "SCATTER_MATRIX_ADD_VARIABLE") {
        return CommandAction::ScatterMatrixAddVariable;
    }
    if (name == "SCATTER_MATRIX_REMOVE_VARIABLE") {
        return CommandAction::ScatterMatrixRemoveVariable;
    }
    if (name == "SCATTER_MATRIX_REPLACE_VARIABLE") {
        return CommandAction::ScatterMatrixReplaceVariable;
    }
    if (name == "PLOT_NEW_LINKED_BOXPLOT") return CommandAction::NewBoxplot;
    if (name == "PLOT_NEW_LINKED_HISTOGRAM") return CommandAction::NewHistogram;
    if (name == "PLOT_NEW_LINKED_BAR_CHART" || name == "PLOT_NEW_LINKED_BARCHART") {
        return CommandAction::NewBarChart;
    }
    if (name == "ADD_PLOT") return CommandAction::AddPlot;
    if (name == "ADD_BOXPLOT") return CommandAction::AddBoxplot;
    if (name == "ADD_HISTOGRAM") return CommandAction::AddHistogram;
    if (name == "ADD_BARPLOT") return CommandAction::AddBarplot;
    if (name == "BARPLOT_SET_X") return CommandAction::BarplotSetX;
    if (name == "BARPLOT_ADD_X") return CommandAction::BarplotAddX;
    if (name == "BARPLOT_REPLACE_X") return CommandAction::BarplotReplaceX;
    if (name == "BARPLOT_REMOVE_X") return CommandAction::BarplotRemoveX;
    if (name == "BARPLOT_SPLIT_BY") return CommandAction::BarplotSplitBy;
    if (name == "BARPLOT_CLEAR_SPLIT") return CommandAction::BarplotClearSplit;
    if (name == "BARPLOT_TOGGLE_CONDITIONAL_PERCENT") {
        return CommandAction::BarplotToggleConditionalPercent;
    }
    if (name == "BARPLOT_ROW_COLORS") return CommandAction::BarplotRowColors;
    if (name == "BARPLOT_SET_SPLIT_STROKE_WIDTH_PROMPT") {
        return CommandAction::BarplotSetSplitStrokeWidthPrompt;
    }
    if (name == "BARPLOT_SPLIT_STROKE_WIDTH") return CommandAction::BarplotSplitStrokeWidth;
    if (name == "BARPLOT_SEGMENT_ENCODING") return CommandAction::BarplotSegmentEncoding;
    if (name == "BARPLOT_SHOW_PATTERNS") return CommandAction::BarplotShowPatterns;
    if (name == "BARPLOT_SELECTION_DISPLAY") return CommandAction::BarplotSelectionDisplay;
    if (name == "BARPLOT_MODE") return CommandAction::BarplotMode;
    if (name == "BARPLOT_WIDTH") return CommandAction::BarplotWidth;
    if (name == "BARPLOT_SEGMENT_SELECT") return CommandAction::BarplotSegmentSelect;
    if (name == "BARPLOT_SEGMENT_APPLY_COLOR_TO_ROWS") {
        return CommandAction::BarplotSegmentApplyColorToRows;
    }
    if (name == "BARPLOT_SEGMENT_SET_COLOR") return CommandAction::BarplotSegmentSetColor;
    if (name == "BARPLOT_LEVEL_SET_COLOR") return CommandAction::BarplotLevelSetColor;
    if (name == "BARPLOT_SEGMENT_SET_ALPHA") return CommandAction::BarplotSegmentSetAlpha;
    if (name == "BARPLOT_LEVEL_SET_PATTERN") return CommandAction::BarplotLevelSetPattern;
    if (name == "BARPLOT_SEGMENT_SET_PATTERN") return CommandAction::BarplotSegmentSetPattern;
    if (name == "BARPLOT_SEGMENT_RESET_COLOR") return CommandAction::BarplotSegmentResetColor;
    if (name == "BARPLOT_LEVEL_RESET_COLOR") return CommandAction::BarplotLevelResetColor;
    if (name == "BARPLOT_SEGMENT_DETAILS") return CommandAction::BarplotSegmentDetails;
    if (name == "CONTEXT_CORRELATION_XY") return CommandAction::ContextCorrelationXY;
    if (name == "CONTEXT_DESCRIPTIVES_XY") return CommandAction::ContextDescriptivesXY;
    if (name == "CONTEXT_LINEAR_MODEL_XY") return CommandAction::ContextLinearModelXY;
    if (name == "PLOT_ANALYZE_MODEL") return CommandAction::PlotAnalyzeModel;
    if (name == "PLOT_ANALYZE_CORRELATIONS") return CommandAction::PlotAnalyzeCorrelations;
    if (name == "PLOT_ANALYZE_CONTINGENCY") return CommandAction::PlotAnalyzeContingency;
    if (name == "PLOT_ANALYZE_DESCRIPTIVES") return CommandAction::PlotAnalyzeDescriptives;
    if (name == "CONTEXT_HISTOGRAM_FREQUENCY") return CommandAction::ContextHistogramFrequency;
    if (name == "CONTEXT_HISTOGRAM_DESCRIPTIVES") return CommandAction::ContextHistogramDescriptives;
    if (name == "CONTEXT_BARCHART_TABLE") return CommandAction::ContextBarchartTable;
    if (name == "CONTEXT_BARCHART_NESTED_TABLE") return CommandAction::ContextBarchartNestedTable;
    if (name == "CONTEXT_BOXPLOT_DESCRIPTIVES") return CommandAction::ContextBoxplotDescriptives;
    if (name == "ANALYZE_CONTINGENCY_TABLE") return CommandAction::AnalyzeContingencyTable;
    if (name == "CORR_SET_PART") return CommandAction::CorrelationSetPart;
    if (name == "CORR_TOGGLE_DISPLAY") return CommandAction::CorrelationToggleDisplay;
    if (name == "CORR_SET_MISSING") return CommandAction::CorrelationSetMissing;
    if (name == "CORR_UPDATE") return CommandAction::CorrelationUpdate;
    if (name == "CORR_OPEN_STRUCTURED") return CommandAction::CorrelationOpenStructured;
    if (name == "CORR_OPEN") return CommandAction::CorrelationOpen;
    if (name == "CORR_SET_VARIABLES") return CommandAction::CorrelationSetVariables;
    if (name == "CORR_INFO") return CommandAction::CorrelationInfo;
    if (name == "OPEN_CORRELATION_MATRIX" || name == "ANALYZE_CORRELATION_MATRIX") {
        return CommandAction::OpenCorrelationMatrix;
    }
    if (name == "PCAFA_OPEN") return CommandAction::DimensionalityOpen;
    if (name == "PCAFA_UPDATE") return CommandAction::DimensionalityUpdate;
    if (name == "PCAFA_SET_VARIABLES") return CommandAction::DimensionalitySetVariables;
    if (name == "PCAFA_SET_IMPUTATION") return CommandAction::DimensionalitySetImputation;
    if (name == "PCAFA_INFO") return CommandAction::DimensionalityInfo;
    if (name == "SCALE_ANALYSIS_OPEN") return CommandAction::ScaleAnalysisOpen;
    if (name == "SCALE_ANALYSIS_UPDATE") return CommandAction::ScaleAnalysisUpdate;
    if (name == "SCALE_ANALYSIS_SET_ITEMS") return CommandAction::ScaleAnalysisSetItems;
    if (name == "SCALE_ANALYSIS_INFO") return CommandAction::ScaleAnalysisInfo;
    if (name == "DENDRO_UPDATE") return CommandAction::DendrogramUpdate;
    if (name == "DENDRO_OPEN") return CommandAction::DendrogramOpen;
    if (name == "DENDRO_SET_VARIABLES") return CommandAction::DendrogramSetVariables;
    if (name == "DENDRO_SET_DISTANCE") return CommandAction::DendrogramSetDistance;
    if (name == "DENDRO_INFO") return CommandAction::DendrogramInfo;
    if (name == "OPEN_DIMENSIONALITY" || name == "ANALYZE_DIMENSIONALITY" ||
        name == "ANALYZE_FACTOR_ANALYSIS") {
        return CommandAction::OpenDimensionality;
    }
    if (name == "OPEN_SCALE_ANALYSIS" || name == "ANALYZE_SCALE_ANALYSIS") {
        return CommandAction::OpenScaleAnalysis;
    }
    if (name == "OPEN_DENDROGRAM" || name == "ANALYZE_QUICK_CLUSTER") {
        return CommandAction::OpenDendrogram;
    }
    if (name == "TABLE1_OPEN_STRUCTURED") return CommandAction::Table1OpenStructured;
    if (name == "TABLE1_OPEN") return CommandAction::Table1OpenText;
    if (name == "COMPARE_MEANS_OPEN" || name == "COMPARE_MEANS_OPEN_POOLED") {
        return CommandAction::CompareMeansOpen;
    }
    if (name == "COMPARE_MEANS_BATCH_OPEN") return CommandAction::CompareMeansBatchOpen;
    if (name == "COMPARE_MEANS_BATCH_ERROR") return CommandAction::CompareMeansBatchError;
    if (name == "OPEN_TABLE1" || name == "ANALYZE_TABLE1") return CommandAction::OpenTable1;
    if (name == "ANALYZE_ONE_SAMPLE_T") return CommandAction::AnalyzeOneSampleT;
    if (name == "ANALYZE_INDEPENDENT_T") return CommandAction::AnalyzeIndependentT;
    if (name == "ANALYZE_PAIRED_T") return CommandAction::AnalyzePairedT;
    if (name == "ANALYZE_ONEWAY_ANOVA") return CommandAction::AnalyzeOneWayAnova;
    if (name == "DATA_IMPUTATION_DIAGNOSTICS") return CommandAction::OpenImputationDiagnostics;
    if (name == "SHOW_MISSING_INFORMATION") return CommandAction::ShowMissingInformation;
    if (name == "MI_ADD_VARIABLE") return CommandAction::MissingInformationAddVariable;
    if (name == "SET_DIAGNOSTIC_PLOT_PROVENANCE") return CommandAction::SetDiagnosticPlotProvenance;
    if (name == "ANALYZE_MISSING_DATA_IMPUTATION") {
        return CommandAction::AnalyzeMissingDataImputation;
    }
    if (name == "DATA_MISSING_DATA_PATTERNS" || name == "DATA_MISSING_DATA_OVERVIEW" || name == "DATA_MISSINGNESS_MODELS") {
        return CommandAction::DataMissingDataPatterns;
    }
    if (name == "GLM" || name == "OPEN_GLM" || name == "ANALYZE_GLM") return CommandAction::OpenGLM;
    if (name == "OPEN_LINEAR_MODEL_TRELLIS" || name == "ANALYZE_LINEAR_MODEL_TRELLIS") {
        return CommandAction::OpenLinearModelTrellis;
    }
    if (name == "OPEN_REGRESSION_COMPARISON" || name == "ANALYZE_REGRESSION_COMPARISON") {
        return CommandAction::OpenRegressionComparison;
    }
    if (name == "GENERALIZED_GLM_OPEN") return CommandAction::GeneralizedGLMOpen;
    if (name == "GENERALIZED_GLM_OPEN_POOLED") return CommandAction::GeneralizedGLMOpenPooled;
    if (name == "GENERALIZED_GLM_OPEN_DIAGNOSTIC") return CommandAction::GeneralizedGLMOpenDiagnostic;
    if (name == "OPEN_GENERALIZED_GLM" || name == "ANALYZE_GENERALIZED_GLM") {
        return CommandAction::OpenGeneralizedGLM;
    }
    if (name == "OPEN_POSITIVE_CONTINUOUS_MODEL" ||
        name == "ANALYZE_POSITIVE_CONTINUOUS_MODEL") {
        return CommandAction::OpenPositiveContinuousModel;
    }
    if (name == "OPEN_PROPORTION_MODEL" || name == "ANALYZE_PROPORTION_MODEL") {
        return CommandAction::OpenProportionModel;
    }
    if (name == "OPEN_COUNT_REGRESSION" || name == "ANALYZE_COUNT_REGRESSION") {
        return CommandAction::OpenCountRegression;
    }
    if (name == "OPEN_COUNT_REGRESSION_COMPARISON" ||
        name == "ANALYZE_COUNT_REGRESSION_COMPARISON") {
        return CommandAction::OpenCountRegressionComparison;
    }
    if (name == "OPEN_BINARY_REGRESSION" || name == "ANALYZE_BINARY_REGRESSION") {
        return CommandAction::OpenBinaryRegression;
    }
    if (name == "OPEN_GENERALIZED_COMPARISON" || name == "ANALYZE_GENERALIZED_COMPARISON") {
        return CommandAction::OpenGeneralizedComparison;
    }
    if (name == "OPEN_POSITIVE_CONTINUOUS_COMPARISON" ||
        name == "ANALYZE_POSITIVE_CONTINUOUS_COMPARISON") {
        return CommandAction::OpenPositiveContinuousComparison;
    }
    if (name == "OPEN_PROPORTION_COMPARISON" ||
        name == "ANALYZE_PROPORTION_COMPARISON") {
        return CommandAction::OpenProportionComparison;
    }
    if (name == "OPEN_BINARY_REGRESSION_COMPARISON" || name == "ANALYZE_BINARY_REGRESSION_COMPARISON") {
        return CommandAction::OpenBinaryRegressionComparison;
    }
    if (name == "GENERALIZED_COMPARISON_UPDATE_ERROR") {
        return CommandAction::GeneralizedComparisonUpdateError;
    }
    if (name == "GENERALIZED_COMPARISON_OPEN_STRUCTURED") {
        return CommandAction::GeneralizedComparisonOpenStructured;
    }
    if (name == "ANALYZE_LINEAR_MIXED_MODEL") return CommandAction::AnalyzeLinearMixedModel;
    if (name == "ANALYZE_GENERALIZED_MIXED_MODEL") return CommandAction::AnalyzeGeneralizedMixedModel;
    if (name == "MIXED_MODEL_OPEN_STRUCTURED") return CommandAction::MixedModelOpenStructured;
    if (name == "MIXED_MODEL_OPEN_TEXT") return CommandAction::MixedModelOpenText;
    if (name == "SET_LABEL_DISPLAY") return CommandAction::SetLabelDisplay;
    if (name == "SET_SELECTED_COLOR") return CommandAction::SetSelectedColor;
    if (name == "RESET_SELECTED_COLOR") return CommandAction::ResetSelectedColor;
    if (name == "GET_SELECTED_COLOR") return CommandAction::GetSelectedColor;
    if (name == "SET_POINT_COLOR") return CommandAction::SetPointColor;
    if (name == "CLEAR_ROW_COLORS") return CommandAction::ClearRowColors;
    if (name == "SELECTED") return CommandAction::SelectedRows;
    if (name == "SET_SELECTED") return CommandAction::SetSelectedRows;
    if (name == "SELECT_ALL") return CommandAction::SelectAllRows;
    if (name == "MODE") return CommandAction::InteractionMode;
    if (name == "SELECTION_MODE") return CommandAction::SelectionMode;
    if (name == "SELECTION_OPERATION") return CommandAction::SelectionOperation;
    if (name == "BOXPLOT_TOGGLE_POINTS") return CommandAction::BoxplotTogglePoints;
    if (name == "BOXPLOT_TOGGLE_BOX") return CommandAction::BoxplotToggleBox;
    if (name == "BOXPLOT_TOGGLE_WHISKERS") return CommandAction::BoxplotToggleWhiskers;
    if (name == "BOXPLOT_TOGGLE_VIOLIN") return CommandAction::BoxplotToggleViolin;
    if (name == "BOXPLOT_CLEAR_SPLIT_VIOLIN") return CommandAction::BoxplotClearSplitViolin;
    if (name == "BOXPLOT_CONFIGURE_SPLIT_VIOLIN") return CommandAction::BoxplotConfigureSplitViolin;
    if (name == "BOXPLOT_CONFIGURE_H0") return CommandAction::BoxplotConfigureH0;
    if (name == "BOXPLOT_CLEAR_H0") return CommandAction::BoxplotClearH0;
    if (name == "BOXPLOT_TOGGLE_CONNECT_ROWS") return CommandAction::BoxplotToggleConnectRows;
    if (name == "BOXPLOT_TOGGLE_STANDARDIZE") return CommandAction::BoxplotToggleStandardize;
    if (name == "BOXPLOT_GROUP_ORDER") return CommandAction::BoxplotGroupOrder;
    if (name == "BOXPLOT_ADD_VARIABLE") return CommandAction::BoxplotAddVariable;
    if (name == "BOXPLOT_REMOVE_VARIABLE") return CommandAction::BoxplotRemoveVariable;
    if (name == "BOXPLOT_REPLACE_VARIABLE") return CommandAction::BoxplotReplaceVariable;
    if (name == "BOXPLOT_ADD_GROUPING_VARIABLE") return CommandAction::BoxplotAddGroupingVariable;
    if (name == "BOXPLOT_REPLACE_GROUPING_VARIABLE") return CommandAction::BoxplotReplaceGroupingVariable;
    if (name == "BOXPLOT_REMOVE_GROUPING_VARIABLE") return CommandAction::BoxplotRemoveGroupingVariable;
    if (name == "BOXPLOT_MOVE_GROUPING_VARIABLE_EARLIER") return CommandAction::BoxplotMoveGroupingVariableEarlier;
    if (name == "BOXPLOT_MOVE_GROUPING_VARIABLE_LATER") return CommandAction::BoxplotMoveGroupingVariableLater;
    if (name == "BOXPLOT_SPLIT_VIOLIN") return CommandAction::BoxplotSplitViolin;
    if (name == "BOXPLOT_H0_SIMULATION") return CommandAction::BoxplotH0Simulation;
    if (name == "BOXPLOT_OPTION") return CommandAction::BoxplotOption;
    if (name == "BOXPLOT_OPTIONS") return CommandAction::BoxplotOptions;
    if (name == "BOXPLOT_SHOW_POINTS") return CommandAction::BoxplotShowPoints;
    if (name == "BOXPLOT_HIDE_POINTS") return CommandAction::BoxplotHidePoints;
    if (name == "BOXPLOT_SHOW_BOX") return CommandAction::BoxplotShowBox;
    if (name == "BOXPLOT_HIDE_BOX") return CommandAction::BoxplotHideBox;
    if (name == "BOXPLOT_SHOW_WHISKERS") return CommandAction::BoxplotShowWhiskers;
    if (name == "BOXPLOT_HIDE_WHISKERS") return CommandAction::BoxplotHideWhiskers;
    if (name == "SCATTER_TOGGLE_OVERLAP_SIZE") return CommandAction::ScatterToggleOverlapSize;
    if (name == "SCATTER_TOGGLE_OVERLAP_SHADING") return CommandAction::ScatterToggleOverlapShading;
    if (name == "HIST_TOGGLE_COUNTS") return CommandAction::HistogramToggleCounts;
    if (name == "HIST_TOGGLE_TICK_MARKS") return CommandAction::HistogramToggleTickMarks;
    if (name == "HIST_TOGGLE_TICK_LABELS") return CommandAction::HistogramToggleTickLabels;
    if (name == "HIST_TOGGLE_RUG") return CommandAction::HistogramToggleRug;
    if (name == "HIST_TOGGLE_DENSITY") return CommandAction::HistogramToggleDensity;
    if (name == "HIST_BREAKS") return CommandAction::HistogramBreaks;
    if (name == "HIST_SET_BINS") return CommandAction::HistogramSetBins;
    if (name == "HIST_SET_BINNING_RULE") return CommandAction::HistogramSetBinningRule;
    if (name == "HIST_DENSITY_INFO") return CommandAction::HistogramDensityInfo;
    if (name == "HIST_SHOW_DENSITY") return CommandAction::HistogramShowDensity;
    if (name == "HIST_SET_DENSITY_MODE") return CommandAction::HistogramSetDensityMode;
    if (name == "HIST_SET_DENSITY_BW") return CommandAction::HistogramSetDensityBandwidth;
    if (name == "HIST_SET_DENSITY_ADJUST") return CommandAction::HistogramSetDensityAdjust;
    if (name == "PCA_BIPLOT_SET_X") return CommandAction::BiplotSetX;
    if (name == "PCA_BIPLOT_SET_Y") return CommandAction::BiplotSetY;
    if (name == "SET_IMPUTATION_DISPLAY") return CommandAction::SetImputationDisplay;
    if (name == "SET_IMPUTATION_UNCERTAINTY") return CommandAction::SetImputationUncertainty;
    if (name == "CHANGE_X_VARIABLE" || name == "SET_XVAR") return CommandAction::ChangeXVariable;
    if (name == "CHANGE_Y_VARIABLE" || name == "SET_YVAR") return CommandAction::ChangeYVariable;
    if (name == "PLOT_COLOR_BY") return CommandAction::SetPlotColorBy;
    if (name == "SAVE_COLOR_SCHEME") return CommandAction::SaveColorScheme;
    if (name == "APPLY_COLOR_SCHEME") return CommandAction::ApplyColorScheme;
    if (name == "PLOT_COLOR_LEGEND_VISIBLE") return CommandAction::SetPlotColorLegendVisible;
    if (name == "PLOT_COLOR_LEGEND_POSITION") return CommandAction::SetPlotColorLegendPosition;
    if (name == "PLOT_CONDITION_CATEGORICAL") return CommandAction::ConditionPlotCategorical;
    if (name == "PLOT_CONDITION_ORDERED") return CommandAction::ConditionPlotOrdered;
    if (name == "PLOT_CONDITION_EQUAL_WIDTH") return CommandAction::ConditionPlotEqualWidth;
    if (name == "PLOT_CONDITION_EQUAL_COUNT") return CommandAction::ConditionPlotEqualCount;
    if (name == "SET_MODE_NONE") return CommandAction::SetModeNone;
    if (name == "SET_MODE_SELECT") return CommandAction::SetModeSelect;
    if (name == "SET_MODE_BRUSH") return CommandAction::SetModeBrush;
    if (name == "SET_MODE_IDENTIFY") return CommandAction::SetModeIdentify;
    if (name == "SET_MODE_LABEL") return CommandAction::SetModeLabel;
    if (name == "SET_MODE_PAN") return CommandAction::SetModePan;
    if (name == "SET_MODE_ZOOM") return CommandAction::SetModeZoom;
    if (name == "SET_SELECTION_REPLACE") return CommandAction::SetSelectionReplace;
    if (name == "SET_SELECTION_ADD") return CommandAction::SetSelectionAdd;
    if (name == "SET_SELECTION_SUBTRACT") return CommandAction::SetSelectionSubtract;
    if (name == "SET_SELECTION_TOGGLE") return CommandAction::SetSelectionToggle;
    if (name == "CLEAR" || name == "CLEAR_SELECTION") return CommandAction::ClearSelection;
    if (name == "INVERT" || name == "INVERT_SELECTION") return CommandAction::InvertSelection;
    if (name == "SELECT_ALL_VISIBLE") return CommandAction::SelectAllVisible;
    if (name == "REDRAW") return CommandAction::RedrawPlot;
    if (name == "RESET_ZOOM" || name == "RESCALE") return CommandAction::ResetZoom;
    if (name == "BRUSH_LARGER") return CommandAction::BrushLarger;
    if (name == "BRUSH_SMALLER") return CommandAction::BrushSmaller;
    if (name == "COPY_SELECTED") return CommandAction::CopySelected;
    if (name == "ADD_LM") return CommandAction::AddLm;
    if (name == "TOGGLE_LM_ALL") return CommandAction::ToggleLmAll;
    if (name == "TOGGLE_LM_SELECTED") return CommandAction::ToggleLmSelected;
    if (name == "TOGGLE_LM_COLOR") return CommandAction::ToggleLmColor;
    if (name == "TOGGLE_LM_BOTH") return CommandAction::ToggleLmBoth;
    if (name == "ADD_LM_ALL") return CommandAction::AddLmAll;
    if (name == "ADD_LM_SELECTED") return CommandAction::AddLmSelected;
    if (name == "ADD_LM_BOTH") return CommandAction::AddLmBoth;
    if (name == "ADD_LM_COLOR") return CommandAction::AddLmColor;
    if (name == "ADD_SMOOTH") return CommandAction::AddSmooth;
    if (name == "ADD_TRELLIS_SMOOTH") return CommandAction::AddTrellisSmooth;
    if (name == "REQUEST_SMOOTH") return CommandAction::RequestSmooth;
    if (name == "SMOOTH_INFO") return CommandAction::SmoothInfo;
    if (name == "REQUEST_COMPARE_MEANS") return CommandAction::RequestCompareMeans;
    if (name == "MAIN_R_TASKS") return CommandAction::MainRTasks;
    if (name == "MODEL_UPDATE_ERROR") return CommandAction::ModelUpdateError;
    if (name == "MODEL_UPDATE") return CommandAction::ModelUpdate;
    if (name == "MODEL_TRELLIS_UPDATE_ERROR") return CommandAction::ModelTrellisUpdateError;
    if (name == "MODEL_TRELLIS_UPDATE") return CommandAction::ModelTrellisUpdate;
    if (name == "REGCMP_UPDATE_ERROR") return CommandAction::RegressionComparisonUpdateError;
    if (name == "REGCMP_UPDATE") return CommandAction::RegressionComparisonUpdate;
    if (name == "REGCMP_OPEN_POOLED") return CommandAction::RegressionComparisonOpenPooled;
    if (name == "REGCMP_OPEN") return CommandAction::RegressionComparisonOpen;
    if (name == "REGCMP_OPEN2") return CommandAction::RegressionComparisonOpen2;
    if (name == "REGCMP_INFO") return CommandAction::RegressionComparisonInfo;
    if (name == "REGCMP_CELL") return CommandAction::RegressionComparisonCell;
    if (name == "REGCMP_FIT_CELL") return CommandAction::RegressionComparisonFitCell;
    if (name == "REGCMP_VISIBLE") return CommandAction::RegressionComparisonVisible;
    if (name == "REGCMP_OPEN_DIAGNOSTIC") return CommandAction::RegressionComparisonOpenDiagnostic;
    if (name == "REGCMP_SET_RESPONSE") return CommandAction::RegressionComparisonSetResponse;
    if (name == "REGCMP_SET_SCOPE") return CommandAction::RegressionComparisonSetScope;
    if (name == "REGCMP_ADD_TERM") return CommandAction::RegressionComparisonAddTerm;
    if (name == "REGCMP_SET_TERM") return CommandAction::RegressionComparisonSetTerm;
    if (name == "REGCMP_ADD_MODEL") return CommandAction::RegressionComparisonAddModel;
    if (name == "REGCMP_SET_TYPE") return CommandAction::RegressionComparisonSetType;
    if (name == "MODEL_OPEN_POOLED") return CommandAction::ModelOpenPooled;
    if (name == "GENERALIZED_GLM_OPEN_STRUCTURED") return CommandAction::GeneralizedGLMOpenStructured;
    if (name == "POINT_COLORS") return CommandAction::PointColors;
    if (name == "TOGGLE_SMOOTH_OVERALL") return CommandAction::ToggleSmoothOverall;
    if (name == "TOGGLE_SMOOTH_SELECTED") return CommandAction::ToggleSmoothSelected;
    if (name == "TOGGLE_SMOOTH_COLOR") return CommandAction::ToggleSmoothColor;
    if (name == "OVERLAYS") return CommandAction::Overlays;
    if (name == "CLEAR_OVERLAYS") return CommandAction::ClearOverlays;
    if (name == "SAVE_SVG") return CommandAction::SaveSvg;
    if (name == "SAVE_PNG") return CommandAction::SavePng;
    if (name == "SAVE_PDF") return CommandAction::SavePdf;
    if (name == "COPY_SVG") return CommandAction::CopySvg;
    if (name == "COPY_EMF") return CommandAction::CopyEmf;
    if (name == "COPY_PNG") return CommandAction::CopyPng;
    if (name == "COPY_PDF") return CommandAction::CopyPdf;
    if (name == "CLOSE_PLOT") return CommandAction::ClosePlot;
    if (name == "CLOSE_ALL") return CommandAction::CloseAll;
    return CommandAction::Unknown;
}

} // namespace

CommandRequest ParseCommandRequest(const std::vector<std::string> &lines)
{
    CommandRequest request;
    if (lines.empty()) {
        return request;
    }
    request.name = lines.front();
    request.args.assign(lines.begin() + 1, lines.end());
    request.category = Categorize(request.name);
    request.action = ResolveAction(request.name);
    return request;
}

CommandRequest ParseMenuCommand(const std::string &command)
{
    CommandRequest request;
    request.name = CommandHead(command);
    request.args = SplitMenuArgs(command);
    request.category = Categorize(request.name);
    request.action = ResolveAction(request.name);
    return request;
}

std::string CommandCategoryName(CommandCategory category)
{
    switch (category) {
    case CommandCategory::Protocol: return "protocol";
    case CommandCategory::Dataset: return "dataset";
    case CommandCategory::Selection: return "selection";
    case CommandCategory::Plot: return "plot";
    case CommandCategory::Model: return "model";
    case CommandCategory::Analysis: return "analysis";
    case CommandCategory::Data: return "data";
    case CommandCategory::Recording: return "recording";
    case CommandCategory::Palette: return "palette";
    case CommandCategory::Window: return "window";
    case CommandCategory::Unknown:
    default:
        return "unknown";
    }
}

std::string CommandActionName(CommandAction action)
{
    switch (action) {
    case CommandAction::Ping: return "ping";
    case CommandAction::NativeImportFile: return "native_import_file";
    case CommandAction::ListPlots: return "list_plots";
    case CommandAction::PlotInfo: return "plot_info";
    case CommandAction::ExportPlot: return "export_plot";
    case CommandAction::CopyPlot: return "copy_plot";
    case CommandAction::PlotTheme: return "plot_theme";
    case CommandAction::DiagnosticInfo: return "diagnostic_info";
    case CommandAction::Groups: return "groups";
    case CommandAction::GroupInfo: return "group_info";
    case CommandAction::DatasetSyncStatus: return "dataset_sync_status";
    case CommandAction::Variables: return "variables";
    case CommandAction::GetXVariable: return "get_x_variable";
    case CommandAction::GetYVariable: return "get_y_variable";
    case CommandAction::GetAnalysisScope: return "get_analysis_scope";
    case CommandAction::GetSavedAnalysisScopes: return "get_saved_analysis_scopes";
    case CommandAction::GetExcludedRows: return "get_excluded_rows";
    case CommandAction::ExcludeSelectedCases: return "exclude_selected_cases";
    case CommandAction::IncludeSelectedCases: return "include_selected_cases";
    case CommandAction::IncludeAllCases: return "include_all_cases";
    case CommandAction::ToggleCaseIncluded: return "toggle_case_included";
    case CommandAction::SetAnalysisScopeAll: return "set_analysis_scope_all";
    case CommandAction::SetAnalysisScopeFromSelection: return "set_analysis_scope_from_selection";
    case CommandAction::SetAnalysisScopeFromRows: return "set_analysis_scope_from_rows";
    case CommandAction::SaveAnalysisScopeFromSelection: return "save_analysis_scope_from_selection";
    case CommandAction::UseSavedAnalysisScope: return "use_saved_analysis_scope";
    case CommandAction::AddSavedAnalysisScope: return "add_saved_analysis_scope";
    case CommandAction::TogglePlotScopeFreeze: return "toggle_plot_scope_freeze";
    case CommandAction::OpenEquivalentWithActiveScope: return "open_equivalent_with_active_scope";
    case CommandAction::OpenEquivalentWithAllRows: return "open_equivalent_with_all_rows";
    case CommandAction::FileImportData: return "file_import_data";
    case CommandAction::FileExportData: return "file_export_data";
    case CommandAction::FileOpenDataFromR: return "file_open_data_from_r";
    case CommandAction::OpenDataSheet: return "open_data_sheet";
    case CommandAction::RegisterDataset: return "register_dataset";
    case CommandAction::RegisterDatasetSilent: return "register_dataset_silent";
    case CommandAction::SetActiveDataset: return "set_active_dataset";
    case CommandAction::DataSetActiveDataset: return "data_set_active_dataset";
    case CommandAction::ShowActiveDataset: return "show_active_dataset";
    case CommandAction::ChooseLabelColumn: return "choose_label_column";
    case CommandAction::VariableView: return "variable_view";
    case CommandAction::AddSelectionColumn: return "add_selection_column";
    case CommandAction::AddPointColorColumn: return "add_point_color_column";
    case CommandAction::SetVariableTypeNumeric: return "set_variable_type_numeric";
    case CommandAction::SetVariableTypeFactor: return "set_variable_type_factor";
    case CommandAction::SetVariableType: return "set_variable_type";
    case CommandAction::SetDefaultVariableRole: return "set_default_variable_role";
    case CommandAction::SetDataCell: return "set_data_cell";
    case CommandAction::RenameVariable: return "rename_variable";
    case CommandAction::SetVariableDescription: return "set_variable_description";
    case CommandAction::SetVariableDecimals: return "set_variable_decimals";
    case CommandAction::SetLabelColumn: return "set_label_column";
    case CommandAction::ShowVariableInformation: return "show_variable_information";
    case CommandAction::ShowRCode: return "show_r_code";
    case CommandAction::ShowRPublicationCode: return "show_r_publication_code";
    case CommandAction::MakeSubsetFromSelection: return "make_subset_from_selection";
    case CommandAction::MakeSubsetFromSavedSelection: return "make_subset_from_saved_selection";
    case CommandAction::MakeSubsetFromVariables: return "make_subset_from_variables";
    case CommandAction::ModelInfo: return "model_info";
    case CommandAction::ModelSetY: return "model_set_y";
    case CommandAction::ModelAddTerm: return "model_add_term";
    case CommandAction::ModelReplaceTerm: return "model_replace_term";
    case CommandAction::ModelRemoveTerm: return "model_remove_term";
    case CommandAction::ModelClearRole: return "model_clear_role";
    case CommandAction::ModelScope: return "model_scope";
    case CommandAction::ModelOpen: return "model_open";
    case CommandAction::ModelOpenResidualsFitted: return "model_open_residuals_fitted";
    case CommandAction::ModelOpenObservedFitted: return "model_open_observed_fitted";
    case CommandAction::ModelOpenDiagnostics: return "model_open_diagnostics";
    case CommandAction::ModelOpenDiagnostic: return "model_open_diagnostic";
    case CommandAction::ModelInteractionReport: return "model_interaction_report";
    case CommandAction::ModelOpenInteractionPlot: return "model_open_interaction_plot";
    case CommandAction::RefreshRDataFrames: return "refresh_r_dataframes";
    case CommandAction::Panel: return "panel";
    case CommandAction::PanelShow: return "panel_show";
    case CommandAction::PanelHide: return "panel_hide";
    case CommandAction::ResetWindowLayout: return "reset_window_layout";
    case CommandAction::RecordingCommand: return "recording_command";
    case CommandAction::OpenColorPalette: return "open_color_palette";
    case CommandAction::PaletteHide: return "palette_hide";
    case CommandAction::PaletteState: return "palette_state";
    case CommandAction::NewScatterplot: return "new_scatterplot";
    case CommandAction::NewTrellisScatterplot: return "new_trellis_scatterplot";
    case CommandAction::NewTimeSeries: return "new_time_series";
    case CommandAction::NewScatterMatrix: return "new_scatter_matrix";
    case CommandAction::NewParallelCoordinates: return "new_parallel_coordinates";
    case CommandAction::ScatterMatrixAddVariable: return "scatter_matrix_add_variable";
    case CommandAction::ScatterMatrixRemoveVariable: return "scatter_matrix_remove_variable";
    case CommandAction::ScatterMatrixReplaceVariable: return "scatter_matrix_replace_variable";
    case CommandAction::NewBoxplot: return "new_boxplot";
    case CommandAction::NewHistogram: return "new_histogram";
    case CommandAction::NewBarChart: return "new_bar_chart";
    case CommandAction::AddPlot: return "add_plot";
    case CommandAction::AddBoxplot: return "add_boxplot";
    case CommandAction::AddHistogram: return "add_histogram";
    case CommandAction::AddBarplot: return "add_barplot";
    case CommandAction::BarplotSetX: return "barplot_set_x";
    case CommandAction::BarplotAddX: return "barplot_add_x";
    case CommandAction::BarplotReplaceX: return "barplot_replace_x";
    case CommandAction::BarplotRemoveX: return "barplot_remove_x";
    case CommandAction::BarplotSplitBy: return "barplot_split_by";
    case CommandAction::BarplotClearSplit: return "barplot_clear_split";
    case CommandAction::BarplotToggleConditionalPercent: return "barplot_toggle_conditional_percent";
    case CommandAction::BarplotRowColors: return "barplot_row_colors";
    case CommandAction::BarplotSetSplitStrokeWidthPrompt: return "barplot_set_split_stroke_width_prompt";
    case CommandAction::BarplotSplitStrokeWidth: return "barplot_split_stroke_width";
    case CommandAction::BarplotSegmentEncoding: return "barplot_segment_encoding";
    case CommandAction::BarplotShowPatterns: return "barplot_show_patterns";
    case CommandAction::BarplotSelectionDisplay: return "barplot_selection_display";
    case CommandAction::BarplotMode: return "barplot_mode";
    case CommandAction::BarplotWidth: return "barplot_width";
    case CommandAction::BarplotSegmentSelect: return "barplot_segment_select";
    case CommandAction::BarplotSegmentApplyColorToRows: return "barplot_segment_apply_color_to_rows";
    case CommandAction::BarplotSegmentSetColor: return "barplot_segment_set_color";
    case CommandAction::BarplotLevelSetColor: return "barplot_level_set_color";
    case CommandAction::BarplotSegmentSetAlpha: return "barplot_segment_set_alpha";
    case CommandAction::BarplotLevelSetPattern: return "barplot_level_set_pattern";
    case CommandAction::BarplotSegmentSetPattern: return "barplot_segment_set_pattern";
    case CommandAction::BarplotSegmentResetColor: return "barplot_segment_reset_color";
    case CommandAction::BarplotLevelResetColor: return "barplot_level_reset_color";
    case CommandAction::BarplotSegmentDetails: return "barplot_segment_details";
    case CommandAction::ContextCorrelationXY: return "context_correlation_xy";
    case CommandAction::CorrelationSetPart: return "correlation_set_part";
    case CommandAction::CorrelationToggleDisplay: return "correlation_toggle_display";
    case CommandAction::CorrelationSetMissing: return "correlation_set_missing";
    case CommandAction::CorrelationUpdate: return "correlation_update";
    case CommandAction::CorrelationOpenStructured: return "correlation_open_structured";
    case CommandAction::CorrelationOpen: return "correlation_open";
    case CommandAction::CorrelationSetVariables: return "correlation_set_variables";
    case CommandAction::CorrelationInfo: return "correlation_info";
    case CommandAction::ContextDescriptivesXY: return "context_descriptives_xy";
    case CommandAction::ContextLinearModelXY: return "context_linear_model_xy";
    case CommandAction::PlotAnalyzeModel: return "plot_analyze_model";
    case CommandAction::PlotAnalyzeCorrelations: return "plot_analyze_correlations";
    case CommandAction::PlotAnalyzeContingency: return "plot_analyze_contingency";
    case CommandAction::PlotAnalyzeDescriptives: return "plot_analyze_descriptives";
    case CommandAction::ContextHistogramFrequency: return "context_histogram_frequency";
    case CommandAction::ContextHistogramDescriptives: return "context_histogram_descriptives";
    case CommandAction::ContextBarchartTable: return "context_barchart_table";
    case CommandAction::ContextBarchartNestedTable: return "context_barchart_nested_table";
    case CommandAction::ContextBoxplotDescriptives: return "context_boxplot_descriptives";
    case CommandAction::AnalyzeContingencyTable: return "analyze_contingency_table";
    case CommandAction::OpenCorrelationMatrix: return "open_correlation_matrix";
    case CommandAction::DimensionalityOpen: return "dimensionality_open";
    case CommandAction::DimensionalityUpdate: return "dimensionality_update";
    case CommandAction::DimensionalitySetVariables: return "dimensionality_set_variables";
    case CommandAction::DimensionalitySetImputation: return "dimensionality_set_imputation";
    case CommandAction::DimensionalityInfo: return "dimensionality_info";
    case CommandAction::ScaleAnalysisOpen: return "scale_analysis_open";
    case CommandAction::ScaleAnalysisUpdate: return "scale_analysis_update";
    case CommandAction::ScaleAnalysisSetItems: return "scale_analysis_set_items";
    case CommandAction::ScaleAnalysisInfo: return "scale_analysis_info";
    case CommandAction::DendrogramOpen: return "dendrogram_open";
    case CommandAction::DendrogramSetVariables: return "dendrogram_set_variables";
    case CommandAction::DendrogramSetDistance: return "dendrogram_set_distance";
    case CommandAction::DendrogramInfo: return "dendrogram_info";
    case CommandAction::OpenDimensionality: return "open_dimensionality";
    case CommandAction::OpenScaleAnalysis: return "open_scale_analysis";
    case CommandAction::OpenDendrogram: return "open_dendrogram";
    case CommandAction::Table1OpenStructured: return "table1_open_structured";
    case CommandAction::Table1OpenText: return "table1_open_text";
    case CommandAction::CompareMeansOpen: return "compare_means_open";
    case CommandAction::CompareMeansBatchOpen: return "compare_means_batch_open";
    case CommandAction::CompareMeansBatchError: return "compare_means_batch_error";
    case CommandAction::OpenTable1: return "open_table1";
    case CommandAction::AnalyzeOneSampleT: return "analyze_one_sample_t";
    case CommandAction::AnalyzeIndependentT: return "analyze_independent_t";
    case CommandAction::AnalyzePairedT: return "analyze_paired_t";
    case CommandAction::AnalyzeOneWayAnova: return "analyze_oneway_anova";
    case CommandAction::AnalyzeMissingDataImputation: return "analyze_missing_data_imputation";
    case CommandAction::OpenGLM: return "open_glm";
    case CommandAction::OpenLinearModelTrellis: return "open_linear_model_trellis";
    case CommandAction::OpenRegressionComparison: return "open_regression_comparison";
    case CommandAction::OpenGeneralizedGLM: return "open_generalized_glm";
    case CommandAction::OpenPositiveContinuousModel: return "open_positive_continuous_model";
    case CommandAction::OpenProportionModel: return "open_proportion_model";
    case CommandAction::OpenCountRegression: return "open_count_regression";
    case CommandAction::OpenCountRegressionComparison: return "open_count_regression_comparison";
    case CommandAction::OpenBinaryRegression: return "open_binary_regression";
    case CommandAction::GeneralizedGLMOpen: return "generalized_glm_open";
    case CommandAction::GeneralizedGLMOpenPooled: return "generalized_glm_open_pooled";
    case CommandAction::GeneralizedGLMOpenDiagnostic: return "generalized_glm_open_diagnostic";
    case CommandAction::OpenGeneralizedComparison: return "open_generalized_comparison";
    case CommandAction::OpenPositiveContinuousComparison: return "open_positive_continuous_comparison";
    case CommandAction::OpenProportionComparison: return "open_proportion_comparison";
    case CommandAction::OpenBinaryRegressionComparison: return "open_binary_regression_comparison";
    case CommandAction::GeneralizedComparisonUpdateError: return "generalized_comparison_update_error";
    case CommandAction::GeneralizedComparisonOpenStructured: return "generalized_comparison_open_structured";
    case CommandAction::AnalyzeLinearMixedModel: return "analyze_linear_mixed_model";
    case CommandAction::AnalyzeGeneralizedMixedModel: return "analyze_generalized_mixed_model";
    case CommandAction::MixedModelOpenStructured: return "mixed_model_open_structured";
    case CommandAction::MixedModelOpenText: return "mixed_model_open_text";
    case CommandAction::SetLabelDisplay: return "set_label_display";
    case CommandAction::SetSelectedColor: return "set_selected_color";
    case CommandAction::ResetSelectedColor: return "reset_selected_color";
    case CommandAction::GetSelectedColor: return "get_selected_color";
    case CommandAction::SetPointColor: return "set_point_color";
    case CommandAction::ClearRowColors: return "clear_row_colors";
    case CommandAction::SelectedRows: return "selected_rows";
    case CommandAction::SetSelectedRows: return "set_selected_rows";
    case CommandAction::SelectAllRows: return "select_all_rows";
    case CommandAction::InteractionMode: return "interaction_mode";
    case CommandAction::SelectionMode: return "selection_mode";
    case CommandAction::SelectionOperation: return "selection_operation";
    case CommandAction::BoxplotTogglePoints: return "boxplot_toggle_points";
    case CommandAction::BoxplotToggleBox: return "boxplot_toggle_box";
    case CommandAction::BoxplotToggleWhiskers: return "boxplot_toggle_whiskers";
    case CommandAction::BoxplotToggleViolin: return "boxplot_toggle_violin";
    case CommandAction::BoxplotClearSplitViolin: return "boxplot_clear_split_violin";
    case CommandAction::BoxplotConfigureSplitViolin: return "boxplot_configure_split_violin";
    case CommandAction::BoxplotConfigureH0: return "boxplot_configure_h0";
    case CommandAction::BoxplotClearH0: return "boxplot_clear_h0";
    case CommandAction::BoxplotToggleConnectRows: return "boxplot_toggle_connect_rows";
    case CommandAction::BoxplotToggleStandardize: return "boxplot_toggle_standardize";
    case CommandAction::BoxplotGroupOrder: return "boxplot_group_order";
    case CommandAction::BoxplotAddVariable: return "boxplot_add_variable";
    case CommandAction::BoxplotRemoveVariable: return "boxplot_remove_variable";
    case CommandAction::BoxplotReplaceVariable: return "boxplot_replace_variable";
    case CommandAction::BoxplotAddGroupingVariable: return "boxplot_add_grouping_variable";
    case CommandAction::BoxplotReplaceGroupingVariable: return "boxplot_replace_grouping_variable";
    case CommandAction::BoxplotRemoveGroupingVariable: return "boxplot_remove_grouping_variable";
    case CommandAction::BoxplotMoveGroupingVariableEarlier: return "boxplot_move_grouping_variable_earlier";
    case CommandAction::BoxplotMoveGroupingVariableLater: return "boxplot_move_grouping_variable_later";
    case CommandAction::BoxplotSplitViolin: return "boxplot_split_violin";
    case CommandAction::BoxplotH0Simulation: return "boxplot_h0_simulation";
    case CommandAction::BoxplotOption: return "boxplot_option";
    case CommandAction::BoxplotOptions: return "boxplot_options";
    case CommandAction::BoxplotShowPoints: return "boxplot_show_points";
    case CommandAction::BoxplotHidePoints: return "boxplot_hide_points";
    case CommandAction::BoxplotShowBox: return "boxplot_show_box";
    case CommandAction::BoxplotHideBox: return "boxplot_hide_box";
    case CommandAction::BoxplotShowWhiskers: return "boxplot_show_whiskers";
    case CommandAction::BoxplotHideWhiskers: return "boxplot_hide_whiskers";
    case CommandAction::ScatterToggleOverlapSize: return "scatter_toggle_overlap_size";
    case CommandAction::ScatterToggleOverlapShading: return "scatter_toggle_overlap_shading";
    case CommandAction::HistogramToggleCounts: return "histogram_toggle_counts";
    case CommandAction::HistogramToggleTickMarks: return "histogram_toggle_tick_marks";
    case CommandAction::HistogramToggleTickLabels: return "histogram_toggle_tick_labels";
    case CommandAction::HistogramToggleRug: return "histogram_toggle_rug";
    case CommandAction::HistogramToggleDensity: return "histogram_toggle_density";
    case CommandAction::HistogramBreaks: return "histogram_breaks";
    case CommandAction::HistogramSetBins: return "histogram_set_bins";
    case CommandAction::HistogramSetBinningRule: return "histogram_set_binning_rule";
    case CommandAction::HistogramDensityInfo: return "histogram_density_info";
    case CommandAction::HistogramShowDensity: return "histogram_show_density";
    case CommandAction::HistogramSetDensityMode: return "histogram_set_density_mode";
    case CommandAction::HistogramSetDensityBandwidth: return "histogram_set_density_bandwidth";
    case CommandAction::HistogramSetDensityAdjust: return "histogram_set_density_adjust";
    case CommandAction::BiplotSetX: return "biplot_set_x";
    case CommandAction::BiplotSetY: return "biplot_set_y";
    case CommandAction::SetImputationDisplay: return "set_imputation_display";
    case CommandAction::SetImputationUncertainty: return "set_imputation_uncertainty";
    case CommandAction::ChangeXVariable: return "change_x_variable";
    case CommandAction::ChangeYVariable: return "change_y_variable";
    case CommandAction::SetPlotColorBy: return "set_plot_color_by";
    case CommandAction::SaveColorScheme: return "save_color_scheme";
    case CommandAction::ApplyColorScheme: return "apply_color_scheme";
    case CommandAction::SetPlotColorLegendVisible: return "set_plot_color_legend_visible";
    case CommandAction::SetPlotColorLegendPosition: return "set_plot_color_legend_position";
    case CommandAction::ConditionPlotCategorical: return "condition_plot_categorical";
    case CommandAction::ConditionPlotOrdered: return "condition_plot_ordered";
    case CommandAction::ConditionPlotEqualWidth: return "condition_plot_equal_width";
    case CommandAction::ConditionPlotEqualCount: return "condition_plot_equal_count";
    case CommandAction::SetTrellisScatterplotX: return "set_trellis_scatterplot_x";
    case CommandAction::SetTrellisScatterplotY: return "set_trellis_scatterplot_y";
    case CommandAction::SetTrellisScatterplotCondition: return "set_trellis_scatterplot_condition";
    case CommandAction::SetTrellisScatterplotLayout: return "set_trellis_scatterplot_layout";
    case CommandAction::SetTrellisScatterplotOrder: return "set_trellis_scatterplot_order";
    case CommandAction::SetTrellisPlotType: return "set_trellis_plot_type";
    case CommandAction::SetTrellisPlotTypeWithX: return "set_trellis_plot_type_with_x";
    case CommandAction::SetTrellisBoxplotGroupingVariables: return "set_trellis_boxplot_grouping_variables";
    case CommandAction::AddTrellisBoxplotGroupingVariable: return "add_trellis_boxplot_grouping_variable";
    case CommandAction::ReplaceTrellisBoxplotGroupingVariable: return "replace_trellis_boxplot_grouping_variable";
    case CommandAction::RemoveTrellisBoxplotGroupingVariable: return "remove_trellis_boxplot_grouping_variable";
    case CommandAction::MoveTrellisBoxplotGroupingVariableEarlier: return "move_trellis_boxplot_grouping_variable_earlier";
    case CommandAction::MoveTrellisBoxplotGroupingVariableLater: return "move_trellis_boxplot_grouping_variable_later";
    case CommandAction::AddTrellisConditionCategorical: return "add_trellis_condition_categorical";
    case CommandAction::AddTrellisConditionOrdered: return "add_trellis_condition_ordered";
    case CommandAction::AddTrellisConditionEqualWidth: return "add_trellis_condition_equal_width";
    case CommandAction::AddTrellisConditionEqualCount: return "add_trellis_condition_equal_count";
    case CommandAction::SetTrellisConditionFactor: return "set_trellis_condition_factor";
    case CommandAction::SetTrellisConditionOrdered: return "set_trellis_condition_ordered";
    case CommandAction::SetTrellisConditionEqualWidth: return "set_trellis_condition_equal_width";
    case CommandAction::SetTrellisConditionEqualCount: return "set_trellis_condition_equal_count";
    case CommandAction::RemoveTrellisCondition: return "remove_trellis_condition";
    case CommandAction::MoveTrellisConditionEarlier: return "move_trellis_condition_earlier";
    case CommandAction::MoveTrellisConditionLater: return "move_trellis_condition_later";
    case CommandAction::ConfigureTrellisConditionEqualWidth: return "configure_trellis_condition_equal_width";
    case CommandAction::ConfigureTrellisConditionEqualCount: return "configure_trellis_condition_equal_count";
    case CommandAction::ConfigureTrellisConditionBins: return "configure_trellis_condition_bins";
    case CommandAction::SetTrellisConditionDimensionRows: return "set_trellis_condition_dimension_rows";
    case CommandAction::SetTrellisConditionDimensionColumns: return "set_trellis_condition_dimension_columns";
    case CommandAction::SetTrellisConditionDimensionNested: return "set_trellis_condition_dimension_nested";
    case CommandAction::SwapTrellisRowsAndColumns: return "swap_trellis_rows_and_columns";
    case CommandAction::ToggleTrellisYAxisSide: return "toggle_trellis_y_axis_side";
    case CommandAction::SetTrellisSplitVariable: return "set_trellis_split_variable";
    case CommandAction::SetTrellisScaleMode: return "set_trellis_scale_mode";
    case CommandAction::SetTrellisBarMeasure: return "set_trellis_bar_measure";
    case CommandAction::SetTrellisHistogramMeasure: return "set_trellis_histogram_measure";
    case CommandAction::SetTrellisHistogramBins: return "set_trellis_histogram_bins";
    case CommandAction::ToggleTrellisConnectObservations: return "toggle_trellis_connect_observations";
    case CommandAction::SetTimeSeriesGroup: return "set_time_series_group";
    case CommandAction::SetTimeSeriesIdentification: return "set_time_series_identification";
    case CommandAction::SetTimeSeriesLegendPosition: return "set_time_series_legend_position";
    case CommandAction::SetInteractionLegendPosition: return "set_interaction_legend_position";
    case CommandAction::ToggleRegressionConnectingLine: return "toggle_regression_connecting_line";
    case CommandAction::ToggleRegressionConfidenceIntervals: return "toggle_regression_confidence_intervals";
    case CommandAction::ToggleSmoothConfidenceIntervals: return "toggle_smooth_confidence_intervals";
    case CommandAction::ToggleRegressionConfidenceLevel: return "toggle_regression_confidence_level";
    case CommandAction::SetRegressionEffectXAxis: return "set_regression_effect_x_axis";
    case CommandAction::SetModeNone: return "set_mode_none";
    case CommandAction::SetModeSelect: return "set_mode_select";
    case CommandAction::SetModeBrush: return "set_mode_brush";
    case CommandAction::SetModeIdentify: return "set_mode_identify";
    case CommandAction::SetModeLabel: return "set_mode_label";
    case CommandAction::SetModePan: return "set_mode_pan";
    case CommandAction::SetModeZoom: return "set_mode_zoom";
    case CommandAction::SetSelectionReplace: return "set_selection_replace";
    case CommandAction::SetSelectionAdd: return "set_selection_add";
    case CommandAction::SetSelectionSubtract: return "set_selection_subtract";
    case CommandAction::SetSelectionToggle: return "set_selection_toggle";
    case CommandAction::ClearSelection: return "clear_selection";
    case CommandAction::InvertSelection: return "invert_selection";
    case CommandAction::SelectAllVisible: return "select_all_visible";
    case CommandAction::RedrawPlot: return "redraw_plot";
    case CommandAction::ResetZoom: return "reset_zoom";
    case CommandAction::BrushLarger: return "brush_larger";
    case CommandAction::BrushSmaller: return "brush_smaller";
    case CommandAction::CopySelected: return "copy_selected";
    case CommandAction::AddLm: return "add_lm";
    case CommandAction::ToggleLmAll: return "toggle_lm_all";
    case CommandAction::ToggleLmSelected: return "toggle_lm_selected";
    case CommandAction::ToggleLmColor: return "toggle_lm_color";
    case CommandAction::ToggleLmBoth: return "toggle_lm_both";
    case CommandAction::AddLmAll: return "add_lm_all";
    case CommandAction::AddLmSelected: return "add_lm_selected";
    case CommandAction::AddLmBoth: return "add_lm_both";
    case CommandAction::AddLmColor: return "add_lm_color";
    case CommandAction::AddSmooth: return "add_smooth";
    case CommandAction::AddTrellisSmooth: return "add_trellis_smooth";
    case CommandAction::RequestSmooth: return "request_smooth";
    case CommandAction::SmoothInfo: return "smooth_info";
    case CommandAction::RequestCompareMeans: return "request_compare_means";
    case CommandAction::MainRTasks: return "main_r_tasks";
    case CommandAction::ModelUpdateError: return "model_update_error";
    case CommandAction::ModelUpdate: return "model_update";
    case CommandAction::ModelTrellisUpdateError: return "model_trellis_update_error";
    case CommandAction::ModelTrellisUpdate: return "model_trellis_update";
    case CommandAction::RegressionComparisonUpdateError: return "regression_comparison_update_error";
    case CommandAction::RegressionComparisonUpdate: return "regression_comparison_update";
    case CommandAction::RegressionComparisonOpenPooled: return "regression_comparison_open_pooled";
    case CommandAction::RegressionComparisonOpen: return "regression_comparison_open";
    case CommandAction::RegressionComparisonOpen2: return "regression_comparison_open2";
    case CommandAction::RegressionComparisonInfo: return "regression_comparison_info";
    case CommandAction::RegressionComparisonCell: return "regression_comparison_cell";
    case CommandAction::RegressionComparisonFitCell: return "regression_comparison_fit_cell";
    case CommandAction::RegressionComparisonVisible: return "regression_comparison_visible";
    case CommandAction::RegressionComparisonOpenDiagnostic: return "regression_comparison_open_diagnostic";
    case CommandAction::RegressionComparisonSetResponse: return "regression_comparison_set_response";
    case CommandAction::RegressionComparisonSetScope: return "regression_comparison_set_scope";
    case CommandAction::RegressionComparisonAddTerm: return "regression_comparison_add_term";
    case CommandAction::RegressionComparisonSetTerm: return "regression_comparison_set_term";
    case CommandAction::RegressionComparisonAddModel: return "regression_comparison_add_model";
    case CommandAction::RegressionComparisonSetType: return "regression_comparison_set_type";
    case CommandAction::ModelOpenPooled: return "model_open_pooled";
    case CommandAction::GeneralizedGLMOpenStructured: return "generalized_glm_open_structured";
    case CommandAction::PointColors: return "point_colors";
    case CommandAction::ToggleSmoothOverall: return "toggle_smooth_overall";
    case CommandAction::ToggleSmoothSelected: return "toggle_smooth_selected";
    case CommandAction::ToggleSmoothColor: return "toggle_smooth_color";
    case CommandAction::Overlays: return "overlays";
    case CommandAction::ClearOverlays: return "clear_overlays";
    case CommandAction::SaveSvg: return "save_svg";
    case CommandAction::SavePng: return "save_png";
    case CommandAction::SavePdf: return "save_pdf";
    case CommandAction::CopySvg: return "copy_svg";
    case CommandAction::CopyEmf: return "copy_emf";
    case CommandAction::CopyPng: return "copy_png";
    case CommandAction::CopyPdf: return "copy_pdf";
    case CommandAction::ClosePlot: return "close_plot";
    case CommandAction::CloseAll: return "close_all";
    case CommandAction::Unknown:
    default:
        return "unknown";
    }
}

bool CommandRequiresPlot(CommandCategory category)
{
    return category == CommandCategory::Plot ||
        category == CommandCategory::Model ||
        category == CommandCategory::Selection;
}

bool CommandIsDatasetCommand(const CommandRequest &request)
{
    return request.category == CommandCategory::Dataset ||
        request.name == "GROUPS" ||
        request.name == "GROUP_INFO" ||
        request.name == "DATASET_SYNC_STATUS" ||
        request.name == "VARIABLES";
}

bool CommandHasName(const CommandRequest &request, const std::string &name)
{
    return request.name == name;
}

} // namespace core
} // namespace rlispstat
