#include "../../../../core/missing_data_model.h"
#include "pch.h"
#include "App.xaml.h"
#include "PerformanceTrace.h"
#include "WindowBranding.h"
#include "WelcomeWindow.h"
#include <winrt/Windows.Networking.h>
#include "../../../../core/plot_geometry.h"
#include "../../../../core/glm_model.h"
#include "../../../../core/model_terms.h"
#include "../../../../core/scatterplot_model.h"
#include "../../../../core/scatter_matrix_model.h"
#include "../../../../core/boxplot_model.h"
#include "../../../../core/histogram_model.h"
#include "../../../../core/barplot_model.h"
#include "../../../../core/trellis_scatterplot_model.h"
#include "../../../../core/statistics_model.h"
#include "../../../../core/table1_model.h"
#include "../../../../core/format_model.h"
#include "../../../../core/dataset_protocol.h"
#include "../../../../core/export_model.h"
#include "../../../../core/svg_writer.h"
#include "../../../../core/linkeda_document.h"
#include "../../../../core/analysis_initialization.h"
#include "../../../../core/main_r_task_model.h"
#include <cctype>
#include <cwctype>
#include <cmath>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <memory>
#include <sstream>
#include <system_error>
#include <vector>
#include <shobjidl.h>
#include <winrt/Microsoft.UI.Xaml.Media.Imaging.h>
#include <winrt/Windows.Graphics.Imaging.h>

using namespace winrt;
using namespace Microsoft::UI::Xaml;

namespace
{
    std::vector<::rlispstat::core::AnalysisScopeChoice> AnalysisScopeChoicesFor(
        ::rlispstat::core::ApplicationState const& applicationState,
        std::string const& datasetId,
        bool includeUnselected)
    {
        (void)includeUnselected;
        const auto scope = applicationState.activeAnalysisScope(datasetId);
        return {{::rlispstat::core::AnalysisScopeChoiceValue("all", scope, true),
                 ::rlispstat::core::AnalysisScopeSummary(scope, scope.totalDatasetRows, false)}};
    }

    std::string InteractionPostEstimationOptions(
        std::vector<std::string> const& command)
    {
        std::vector<std::string> options{"emm_reference=none"};
        for (auto const& value : command)
        {
            const bool supported = value.rfind("emm_reference=", 0) == 0 ||
                value.rfind("plot_focal=", 0) == 0 ||
                value.rfind("report_focal=", 0) == 0 ||
                value.rfind("report_group=", 0) == 0 ||
                value.rfind("effect_quantity=", 0) == 0 ||
                value.rfind("effect_adjustment=", 0) == 0 ||
                value.rfind("effect_presentation=", 0) == 0 ||
                value.rfind("effect_confidence=", 0) == 0;
            if (!supported) continue;
            const auto separator = value.find('=');
            const std::string prefix = value.substr(0, separator + 1);
            options.erase(std::remove_if(options.begin(), options.end(),
                [&](const std::string &existing) {
                    return existing.rfind(prefix, 0) == 0;
                }), options.end());
            options.push_back(value);
        }
        std::ostringstream joined;
        for (std::size_t index = 0; index < options.size(); ++index) {
            if (index) joined << '\t';
            joined << options[index];
        }
        return joined.str();
    }

    constexpr wchar_t kMessageEnd[] = L"__LINKEDA_TCP_MESSAGE_END__";

    struct NativeImportResult
    {
        bool ok = false;
        std::vector<std::string> payload;
        std::string message;
    };

    struct NativeImputationResult
    {
        bool ok = false;
        std::vector<std::string> payload;
        std::string message;
    };

    bool ParseNativeImportDataFrame(std::vector<std::string> const& payload,
                                    ::rlispstat::core::DataFrameModel& dataframe,
                                    std::string& error)
    {
        if (payload.size() < 3 || payload.front() != "REGISTER_DATASET")
        {
            error = "The native importer did not return a dataset payload.";
            return false;
        }
        std::vector<std::string> args(payload.begin() + 1, payload.end());
        const std::string group = args.front();
        std::size_t cursor = 1;
        if (cursor < args.size() && args[cursor] == "VARS")
        {
            if (++cursor >= args.size()) { error = "Malformed variable payload."; return false; }
            const long count = std::strtol(args[cursor++].c_str(), nullptr, 10);
            for (long index = 0; index < count; ++index)
            {
                if (cursor + 1 >= args.size()) { error = "Malformed variable payload."; return false; }
                ++cursor;
                const long values = std::strtol(args[cursor++].c_str(), nullptr, 10);
                if (values < 0 || cursor + static_cast<std::size_t>(values) > args.size())
                { error = "Malformed variable payload."; return false; }
                cursor += static_cast<std::size_t>(values);
            }
        }
        if (cursor < args.size() && args[cursor] == "VARMETA")
        {
            if (++cursor >= args.size()) { error = "Malformed variable metadata."; return false; }
            const long count = std::strtol(args[cursor++].c_str(), nullptr, 10);
            if (count < 0 || cursor + static_cast<std::size_t>(count) * 2 > args.size())
            { error = "Malformed variable metadata."; return false; }
            cursor += static_cast<std::size_t>(count) * 2;
        }
        bool parsed = false;
        if (!::rlispstat::core::ParseDataFramePayload(
                args, cursor, group, dataframe, &parsed, error)) return false;
        if (!parsed) error = "The native importer returned no data frame.";
        return parsed;
    }

    std::filesystem::path RegisteredRscriptPath()
    {
        const auto query = [](HKEY root) -> std::filesystem::path
        {
            wchar_t value[32768]{};
            DWORD bytes = sizeof(value);
            if (RegGetValueW(root, L"SOFTWARE\\R-core\\R", L"InstallPath",
                             RRF_RT_REG_SZ, nullptr, value, &bytes) != ERROR_SUCCESS)
                return {};
            auto path = std::filesystem::path(value) / L"bin" / L"x64" / L"Rscript.exe";
            if (std::filesystem::exists(path)) return path;
            path = std::filesystem::path(value) / L"bin" / L"Rscript.exe";
            return std::filesystem::exists(path) ? path : std::filesystem::path{};
        };
        auto path = query(HKEY_CURRENT_USER);
        return path.empty() ? query(HKEY_LOCAL_MACHINE) : path;
    }

    std::filesystem::path FindRscriptPath()
    {
        wchar_t rHome[32768]{};
        const DWORD length = GetEnvironmentVariableW(L"R_HOME", rHome,
                                                       static_cast<DWORD>(std::size(rHome)));
        if (length > 0 && length < std::size(rHome))
        {
            auto path = std::filesystem::path(rHome) / L"bin" / L"x64" / L"Rscript.exe";
            if (std::filesystem::exists(path)) return path;
            path = std::filesystem::path(rHome) / L"bin" / L"Rscript.exe";
            if (std::filesystem::exists(path)) return path;
        }
        auto registered = RegisteredRscriptPath();
        if (!registered.empty()) return registered;
        std::vector<std::filesystem::path> candidates;
        std::error_code error;
        const std::filesystem::path root = L"C:\\Program Files\\R";
        if (std::filesystem::is_directory(root, error))
            for (auto const& entry : std::filesystem::directory_iterator(root, error))
            {
                auto path = entry.path() / L"bin" / L"x64" / L"Rscript.exe";
                if (std::filesystem::exists(path, error)) candidates.push_back(path);
            }
        if (!candidates.empty())
        {
            std::sort(candidates.begin(), candidates.end());
            return candidates.back();
        }
        return L"Rscript.exe";
    }

    std::wstring QuoteProcessArgument(std::wstring value)
    {
        std::wstring quoted = L"\"";
        for (wchar_t ch : value)
        {
            if (ch == L'\"') quoted += L"\\\"";
            else quoted += ch;
        }
        quoted += L"\"";
        return quoted;
    }

    NativeImportResult RunNativeImport(std::string const& path,
                                       std::string const& group)
    {
        NativeImportResult result;
        const auto unique = std::to_wstring(GetCurrentProcessId()) + L"-" +
            std::to_wstring(GetTickCount64());
        const auto directory = std::filesystem::temp_directory_path();
        const auto scriptPath = directory / (L"linkeda-import-" + unique + L".R");
        const auto payloadPath = directory / (L"linkeda-import-" + unique + L".payload");
        const auto logPath = directory / (L"linkeda-import-" + unique + L".log");
        const auto cleanup = [&]()
        {
            std::error_code ignored;
            std::filesystem::remove(scriptPath, ignored);
            std::filesystem::remove(payloadPath, ignored);
            std::filesystem::remove(logPath, ignored);
        };
        {
            std::ofstream script(scriptPath, std::ios::binary);
            if (!script)
            {
                result.message = ::rlispstat::core::NativeImportTemporaryScriptFailedStatus();
                return result;
            }
            script << ::rlispstat::core::NativeImportRScript();
        }

        SECURITY_ATTRIBUTES security{ sizeof(SECURITY_ATTRIBUTES), nullptr, TRUE };
        HANDLE log = CreateFileW(logPath.c_str(), GENERIC_WRITE,
            FILE_SHARE_READ | FILE_SHARE_WRITE, &security, CREATE_ALWAYS,
            FILE_ATTRIBUTE_TEMPORARY, nullptr);
        if (log == INVALID_HANDLE_VALUE)
        {
            cleanup();
            result.message = ::rlispstat::core::NativeImportRscriptLaunchFailedStatus();
            return result;
        }
        STARTUPINFOW startup{}; startup.cb = sizeof(startup);
        startup.dwFlags = STARTF_USESTDHANDLES;
        startup.hStdOutput = log; startup.hStdError = log;
        startup.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
        PROCESS_INFORMATION process{};
        const auto rscript = FindRscriptPath();
        std::wstring command = QuoteProcessArgument(rscript.wstring()) + L" --vanilla " +
            QuoteProcessArgument(scriptPath.wstring()) + L" " +
            QuoteProcessArgument(to_hstring(path).c_str()) + L" " +
            QuoteProcessArgument(to_hstring(group).c_str()) + L" " +
            QuoteProcessArgument(payloadPath.wstring());
        std::vector<wchar_t> mutableCommand(command.begin(), command.end());
        mutableCommand.push_back(L'\0');
        const BOOL started = CreateProcessW(
            rscript.is_absolute() ? rscript.c_str() : nullptr,
            mutableCommand.data(), nullptr, nullptr, TRUE, CREATE_NO_WINDOW,
            nullptr, nullptr, &startup, &process);
        CloseHandle(log);
        if (!started)
        {
            cleanup();
            result.message = ::rlispstat::core::NativeImportRscriptLaunchFailedStatus();
            return result;
        }
        WaitForSingleObject(process.hProcess, INFINITE);
        DWORD exitCode = 1;
        GetExitCodeProcess(process.hProcess, &exitCode);
        CloseHandle(process.hThread); CloseHandle(process.hProcess);

        if (exitCode != 0)
        {
            std::ifstream logInput(logPath, std::ios::binary);
            std::ostringstream text; text << logInput.rdbuf();
            result.message = text.str();
            if (result.message.empty())
                result.message = ::rlispstat::core::NativeImportRscriptFailedStatus();
            cleanup();
            return result;
        }
        std::ifstream payload(payloadPath, std::ios::binary);
        for (std::string line; std::getline(payload, line);)
        {
            if (!line.empty() && line.back() == '\r') line.pop_back();
            result.payload.push_back(std::move(line));
        }
        if (result.payload.empty())
            result.message = ::rlispstat::core::NativeImportPayloadMissingStatus();
        else result.ok = true;
        cleanup();
        return result;
    }

    struct NativeDocumentResult
    {
        bool ok = false;
        std::vector<unsigned char> payload;
        std::string message;
    };

    bool RunRScriptText(std::string const& scriptText,
                        std::vector<std::wstring> const& arguments,
                        std::string& message,
                        std::vector<std::string>* outputLines = nullptr)
    {
        const auto unique = std::to_wstring(GetCurrentProcessId()) + L"-" +
            std::to_wstring(GetTickCount64());
        const auto directory = std::filesystem::temp_directory_path();
        const auto scriptPath = directory / (L"linkeda-document-" + unique + L".R");
        const auto logPath = directory / (L"linkeda-document-" + unique + L".log");
        const auto cleanup = [&]()
        {
            std::error_code ignored;
            std::filesystem::remove(scriptPath, ignored);
            std::filesystem::remove(logPath, ignored);
        };
        {
            std::ofstream script(scriptPath, std::ios::binary);
            if (!script) { message = "Could not create a temporary LinkEDA document script."; return false; }
            script << scriptText;
        }
        SECURITY_ATTRIBUTES security{ sizeof(SECURITY_ATTRIBUTES), nullptr, TRUE };
        HANDLE log = CreateFileW(logPath.c_str(), GENERIC_WRITE,
            FILE_SHARE_READ | FILE_SHARE_WRITE, &security, CREATE_ALWAYS,
            FILE_ATTRIBUTE_TEMPORARY, nullptr);
        if (log == INVALID_HANDLE_VALUE) { cleanup(); message = "Could not start Rscript."; return false; }
        STARTUPINFOW startup{}; startup.cb = sizeof(startup);
        startup.dwFlags = STARTF_USESTDHANDLES;
        startup.hStdOutput = log; startup.hStdError = log;
        startup.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
        PROCESS_INFORMATION process{};
        const auto rscript = FindRscriptPath();
        std::wstring command = QuoteProcessArgument(rscript.wstring()) + L" --vanilla " +
            QuoteProcessArgument(scriptPath.wstring());
        for (const auto& argument : arguments) command += L" " + QuoteProcessArgument(argument);
        std::vector<wchar_t> mutableCommand(command.begin(), command.end());
        mutableCommand.push_back(L'\0');
        const BOOL started = CreateProcessW(
            rscript.is_absolute() ? rscript.c_str() : nullptr, mutableCommand.data(),
            nullptr, nullptr, TRUE, CREATE_NO_WINDOW, nullptr, nullptr, &startup, &process);
        CloseHandle(log);
        if (!started) { cleanup(); message = "Could not start Rscript."; return false; }
        WaitForSingleObject(process.hProcess, INFINITE);
        DWORD exitCode = 1; GetExitCodeProcess(process.hProcess, &exitCode);
        CloseHandle(process.hThread); CloseHandle(process.hProcess);
        std::ifstream input(logPath, std::ios::binary);
        std::ostringstream captured;
        captured << input.rdbuf();
        const std::string output = captured.str();
        if (outputLines)
        {
            outputLines->clear();
            std::istringstream lines(output);
            for (std::string line; std::getline(lines, line);)
            {
                if (!line.empty() && line.back() == '\r') line.pop_back();
                outputLines->push_back(std::move(line));
            }
        }
        if (exitCode != 0)
        {
            message = output;
            if (message.empty()) message = "R could not read or write the LinkEDA document.";
            cleanup(); return false;
        }
        cleanup(); return true;
    }

    struct NativeGLMPairwiseRun
    {
        bool ok = false;
        ::rlispstat::core::GLMPairwiseComparisonResult result;
        std::string message;
    };

    NativeGLMPairwiseRun RunNativeGLMPairwiseComparisons(
        ::rlispstat::core::DataFrameModel const& dataframe,
        ::rlispstat::core::PlotModel const& seed,
        std::string const& dependent,
        std::vector<std::string> const& terms,
        std::map<std::string, std::string> const& termTypes,
        ::rlispstat::core::GLMFitSummary const& fit,
        ::rlispstat::core::GLMPairwiseComparisonRequest const& request)
    {
        NativeGLMPairwiseRun run;
        run.result.term = request.term;
        run.result.adjustment = request.adjustment;
        const auto unique = std::to_wstring(GetCurrentProcessId()) + L"-" +
            std::to_wstring(GetTickCount64());
        const auto csvPath = std::filesystem::temp_directory_path() /
            (L"linkeda-glm-pairwise-" + unique + L".csv");
        const auto cleanup = [&]()
        {
            std::error_code ignored;
            std::filesystem::remove(csvPath, ignored);
        };
        {
            std::ofstream csv(csvPath, std::ios::binary);
            if (!csv || !::rlispstat::core::WriteDataFrameCSV(csv, dataframe, fit.rowsUsed))
            {
                run.message = "Could not write the fitted observations for pairwise comparisons.";
                cleanup();
                return run;
            }
        }

        std::vector<std::string> available;
        available.reserve(dataframe.columns.size());
        for (auto const& column : dataframe.columns) available.push_back(column.name);
        const auto variables = ::rlispstat::core::BaseVariablesFromModelTerms(
            available, dependent, terms);
        auto wide = [](std::string const& value)
        {
            return std::wstring(to_hstring(value).c_str());
        };
        std::vector<std::wstring> arguments{
            csvPath.wstring(), wide(dependent), wide(request.term),
            wide(std::to_string(request.confidenceLevel)),
            std::to_wstring(static_cast<unsigned long long>(terms.size()))
        };
        for (auto const& term : terms) arguments.push_back(wide(term));
        arguments.push_back(std::to_wstring(
            static_cast<unsigned long long>(variables.size())));
        for (auto const& variable : variables) arguments.push_back(wide(variable));
        for (auto const& variable : variables)
            arguments.push_back(wide(::rlispstat::core::ModelTermDisplayType(
                &seed, variable, termTypes)));
        arguments.push_back(wide(request.adjustment.empty() ? "auto" : request.adjustment));

        std::vector<std::string> lines;
        if (!RunRScriptText(::rlispstat::core::GLMPairwiseComparisonsRScript(),
                            arguments, run.message, &lines))
        {
            cleanup();
            return run;
        }
        cleanup();
        if (!::rlispstat::core::ParseGLMPairwiseComparisonsRResult(
                lines, run.result))
        {
            run.message = run.result.message.empty()
                ? "R could not calculate pairwise comparisons."
                : run.result.message;
            return run;
        }
        run.ok = true;
        return run;
    }

    struct NativeRegressionPostEstimationRun
    {
        bool ok = false;
        std::vector<std::string> lines;
        std::string message;
    };

    NativeRegressionPostEstimationRun RunRRegressionPostEstimation(
        ::rlispstat::core::DataFrameModel const& dataframe,
        std::string const& mode,
        std::string const& analysisId,
        std::string const& term,
        std::string const& response,
        std::vector<std::string> const& terms,
        std::string const& scope,
        std::string const& modelSpec,
        std::string const& residualType = {})
    {
        NativeRegressionPostEstimationRun run;
        const auto unique = std::to_wstring(GetCurrentProcessId()) + L"-" +
            std::to_wstring(GetTickCount64());
        const auto directory = std::filesystem::temp_directory_path();
        const auto inputPath = directory / (L"linkeda-postest-" + unique + L".input");
        const auto outputPath = directory / (L"linkeda-postest-" + unique + L".output");
        const auto cleanup = [&]()
        {
            std::error_code ignored;
            std::filesystem::remove(inputPath, ignored);
            std::filesystem::remove(outputPath, ignored);
        };
        {
            std::ofstream input(inputPath, std::ios::binary);
            if (!input)
            {
                run.message = "Could not write the regression post-estimation dataset.";
                return run;
            }
            ::rlispstat::core::WriteDataFramePayloadForR(input, dataframe, true);
            input.close();
            if (!input)
            {
                cleanup();
                run.message = "Could not write the regression post-estimation dataset.";
                return run;
            }
        }
        auto join = [](std::vector<std::string> const& values)
        {
            std::ostringstream text;
            for (std::size_t i = 0; i < values.size(); ++i)
            {
                if (i) text << '\t';
                text << values[i];
            }
            return text.str();
        };
        auto wide = [](std::string const& value)
        {
            return std::wstring(to_hstring(value).c_str());
        };
        std::vector<std::string> ignored;
        if (!RunRScriptText(
                ::rlispstat::core::NativePooledAnalysisRScript(),
                {inputPath.wstring(), outputPath.wstring(), wide(mode),
                 wide(analysisId), wide(residualType), wide(term), wide(response),
                 wide(join(terms)), wide(scope.empty() ? "all" : scope),
                 wide(modelSpec)}, run.message, &ignored))
        {
            cleanup();
            return run;
        }
        std::ifstream output(outputPath, std::ios::binary);
        for (std::string line; std::getline(output, line);)
        {
            if (!line.empty() && line.back() == '\r') line.pop_back();
            run.lines.push_back(std::move(line));
        }
        cleanup();
        if (run.lines.empty())
        {
            run.message = "R returned no regression post-estimation result.";
            return run;
        }
        run.ok = true;
        return run;
    }

    ::rlispstat::core::Table1DisplayState GLMPairwiseDisplayState(
        std::string const& group,
        ::rlispstat::core::GLMPairwiseComparisonResult const& result,
        std::string const& id,
        bool fromAnova = false)
    {
        using namespace ::rlispstat::core;
        Table1DisplayState state;
        state.id = id;
        state.datasetId = group;
        state.title = "Pairwise comparisons — " + result.term;
        state.codeReference=PairwiseDiagnosticsCodeReference(id,result);
        state.subtitle = "Estimated marginal means · " +
            (result.adjustment.empty() ? std::string("Tukey") : result.adjustment);
        state.tableType = "glm_pairwise";
        state.nativeGenerated = true;
        state.stubHeaders = { "Comparison" };
        state.columns = { "Estimate", "SE", "df", "t", "p", "95% CI" };
        if(result.scaleReport.ok) {
            state.stubHeaders={result.scaleHeaders.front()};
            state.columns.assign(result.scaleHeaders.begin()+1,result.scaleHeaders.end());
            state.subtitle="Estimated marginal means: response scale; statistical contrasts: link scale (identity: response)";
            for(const auto &values:result.scaleRows) {
                Table1DisplayRow row;row.rowIndex=static_cast<int>(state.rows.size());row.rowType="comparison";
                row.variable=result.term;row.label=values.front();row.stubValues={values.front()};
                row.values.assign(values.begin()+1,values.end());state.rows.push_back(std::move(row));
            }
            state.footnotes=result.scaleReport.notes;
            if(fromAnova) state.footnotes.push_back(
                "Comparisons use the observations fitted in the source ANOVA.");
            return state;
        }
        int index = 0;
        for (auto const& comparison : result.comparisons)
        {
            Table1DisplayRow row;
            row.rowIndex = index++;
            row.rowType = "comparison";
            row.variable = result.term;
            row.label = comparison.label;
            row.stubValues = { comparison.label };
            row.values = {
                FormatDoubleOrDash(comparison.estimate, 4),
                FormatDoubleOrDash(comparison.stdError, 4),
                FormatDoubleOrDash(comparison.degreesOfFreedom, 1),
                FormatDoubleOrDash(comparison.statistic, 3),
                std::isfinite(comparison.adjustedPValue)
                    ? FormatPValue(comparison.adjustedPValue) : std::string("—"),
                "[" + FormatDoubleOrDash(comparison.ciLower, 4) + ", " +
                    FormatDoubleOrDash(comparison.ciUpper, 4) + "]"
            };
            state.rows.push_back(std::move(row));
        }
        if (!result.method.empty()) state.footnotes.push_back("Method: " + result.method);
        state.footnotes.push_back(fromAnova
            ? "Comparisons use the observations fitted in the source ANOVA."
            : "Numeric covariates are held at their fitted-sample means; other categorical predictors are averaged over their fitted categories.");
        state.statusText = std::to_string(result.comparisons.size()) +
            " pairwise comparisons for " + result.term;
        return state;
    }

    ::rlispstat::core::Table1DisplayState GLMPairwiseFailureState(
        std::string const& group, std::string const& term,
        std::string const& id, std::string const& message)
    {
        ::rlispstat::core::Table1DisplayState state;
        state.id = id;
        state.datasetId = group;
        state.title = "Pairwise comparisons — " + term;
        state.subtitle = message.empty() ? "Pairwise comparisons are not available." : message;
        state.tableType = "glm_pairwise";
        state.nativeGenerated = true;
        state.warnings.push_back(state.subtitle);
        state.statusText = state.subtitle;
        return state;
    }

    std::vector<int> NativePostEstimationRows(
        ::rlispstat::core::AnalysisScope const& scope, bool captured)
    {
        return captured && scope.kind ==
            ::rlispstat::core::AnalysisScopeKind::ExplicitRowIds
            ? scope.originalRowIds : std::vector<int>{};
    }

    ::rlispstat::core::MainRGeneralizedGLMTask NativePostEstimationTask(
        ::rlispstat::core::GeneralizedGLMState const& state)
    {
        ::rlispstat::core::MainRGeneralizedGLMTask task;
        task.id = state.id;
        task.generation = state.rFitGeneration;
        task.group = state.group;
        task.response = state.response;
        task.family = state.family;
        task.link = state.link;
        task.modelType = ::rlispstat::core::StatisticalModelTypeId(state.modelType);
        task.responseBoundsConfigured = state.responseBoundsConfigured;
        task.responseLower = state.responseLower;
        task.responseUpper = state.responseUpper;
        task.scope = state.scope;
        task.terms = state.terms;
        task.termTypes = ::rlispstat::core::EffectiveModelSpecificationTermTypes(state);
        task.centeredPredictors.assign(state.centeredPredictors.begin(),
                                       state.centeredPredictors.end());
        task.factorReferenceLevels = state.factorReferenceLevels;
        task.binaryRegression = state.binaryRegression;
        task.countRegression = state.countRegression;
        task.countDistribution =
            ::rlispstat::core::CountDistributionId(state.countDistribution);
    task.exposure = state.exposure;
    task.offsetVariable = state.offsetVariable;
        task.trialsVariable = state.trialsVariable;
        task.trialsConstant = state.trialsConstant;
        task.eventValue = state.responseCodingExplicit
            ? state.responseCoding.eventValue : std::string{};
        task.referenceValue = state.responseCodingExplicit
            ? state.responseCoding.referenceValue : std::string{};
        ::rlispstat::core::SetGeneralizedTaskAnalysisScope(task, state.dataScope,
            state.dataScopeCaptured, {});
        return task;
    }

    ::rlispstat::core::MainRGeneralizedGLMTask NativePostEstimationTask(
        ::rlispstat::core::GeneralizedComparisonState const& state,
        ::rlispstat::core::GeneralizedComparisonModel const& model)
    {
        const auto specification =
            ::rlispstat::core::EffectiveGeneralizedComparisonModelSpecification(
                state, model);
        ::rlispstat::core::MainRGeneralizedGLMTask task;
        task.id = model.id;
        task.generation = static_cast<std::uint64_t>(
            std::max(0, state.rFitGeneration));
        task.group = state.group;
        task.response = specification.response.empty()
            ? state.response : specification.response;
        task.family = model.family.empty() ? state.family : model.family;
        task.link = model.link.empty() ? state.link : model.link;
        task.modelType = ::rlispstat::core::StatisticalModelTypeId(state.modelType);
        task.responseBoundsConfigured = model.responseBoundsConfigured;
        task.responseLower = model.responseLower;
        task.responseUpper = model.responseUpper;
        task.scope = specification.scope.empty()
            ? state.scope : specification.scope;
        task.terms = specification.terms;
        task.termTypes =
            ::rlispstat::core::GeneralizedComparisonModelTermTypes(state, model);
        task.centeredPredictors.assign(specification.centeredPredictors.begin(),
                                       specification.centeredPredictors.end());
        task.factorReferenceLevels = specification.factorReferenceLevels;
        task.binaryRegression = state.binaryComparison;
        task.countRegression = state.countComparison;
        task.countDistribution =
            ::rlispstat::core::CountDistributionId(model.countDistribution);
        task.exposure = model.exposure;
        task.offsetVariable = model.offsetVariable;
        task.trialsVariable = model.trialsVariable;
        task.trialsConstant = model.trialsConstant;
        task.eventValue = state.responseCoding.eventValue.empty()
            ? state.responseCoding.eventLabel : state.responseCoding.eventValue;
        task.referenceValue = state.responseCoding.referenceValue.empty()
            ? state.responseCoding.referenceLabel : state.responseCoding.referenceValue;
        task.rows = NativePostEstimationRows(state.dataScope,
                                              state.dataScopeCaptured);
        return task;
    }

    std::string JoinTaskFields(std::vector<std::string> const& values)
    {
        std::ostringstream out;
        for (std::size_t index = 0; index < values.size(); ++index)
        {
            if (index) out << '\t';
            out << values[index];
        }
        return out.str();
    }

    NativeImputationResult RunNativeImputation(
        ::rlispstat::core::DataFrameModel const& dataframe,
        winrt::rlispstatWinUI::implementation::MissingDataImputationSpecification const& specification,
        std::string const& outputGroup)
    {
        NativeImputationResult result;
        const auto unique = std::to_wstring(GetCurrentProcessId()) + L"-" +
            std::to_wstring(GetTickCount64());
        const auto directory = std::filesystem::temp_directory_path();
        const auto inputPath = directory / (L"linkeda-mice-" + unique + L".input");
        const auto payloadPath = directory / (L"linkeda-mice-" + unique + L".payload");
        const auto cleanup = [&]()
        {
            std::error_code ignored;
            std::filesystem::remove(inputPath, ignored);
            std::filesystem::remove(payloadPath, ignored);
        };

        {
            std::ofstream input(inputPath, std::ios::binary);
            if (!input)
            {
                result.message = ::rlispstat::core::NativeRDataPayloadTemporaryFileFailedStatus();
                return result;
            }
            ::rlispstat::core::WriteDataFramePayloadForR(input, dataframe, false);
            input.close();
            if (!input)
            {
                cleanup();
                result.message = ::rlispstat::core::NativeRDataPayloadTemporaryFileFailedStatus();
                return result;
            }
        }

        std::vector<std::string> methodFields;
        methodFields.reserve(specification.methods.size());
        for (auto const& [variable, method] : specification.methods)
            methodFields.push_back(variable + "=" + method);

        std::string runMessage;
        const bool ran = RunRScriptText(
            ::rlispstat::core::NativeMiceImputationRScript(),
            {
                inputPath.wstring(), payloadPath.wstring(),
                to_hstring(outputGroup).c_str(),
                to_hstring(JoinTaskFields(specification.imputeVariables)).c_str(),
                to_hstring(JoinTaskFields(specification.predictorVariables)).c_str(),
                to_hstring(JoinTaskFields(methodFields)).c_str(),
                std::to_wstring(std::max(1, specification.imputations)),
                std::to_wstring(std::max(0, specification.iterations)),
                to_hstring(specification.seed).c_str()
            }, runMessage);
        if (!ran)
        {
            cleanup();
            result.message = runMessage.empty()
                ? ::rlispstat::core::NativeMiceRscriptFailedStatus()
                : runMessage;
            return result;
        }

        std::ifstream payload(payloadPath, std::ios::binary);
        for (std::string line; std::getline(payload, line);)
        {
            if (!line.empty() && line.back() == '\r') line.pop_back();
            result.payload.push_back(std::move(line));
        }
        cleanup();
        if (result.payload.empty())
        {
            result.message = ::rlispstat::core::NativeMicePayloadMissingStatus();
            return result;
        }
        result.ok = true;
        result.message = ::rlispstat::core::NativeMiceCreatedDatasetStatus(
            outputGroup, specification.imputations);
        return result;
    }

    NativeDocumentResult WriteNativeDocument(
        std::string const& path, std::vector<unsigned char> const& payload)
    {
        NativeDocumentResult result;
        const auto unique = std::to_wstring(GetCurrentProcessId()) + L"-" +
            std::to_wstring(GetTickCount64());
        const auto temp = std::filesystem::temp_directory_path() /
            (L"linkeda-payload-" + unique + L".bin");
        std::filesystem::path destination(to_hstring(path).c_str());
        auto staged = destination; staged += L".tmp-" + unique;
        const auto cleanup = [&]()
        {
            std::error_code ignored; std::filesystem::remove(temp, ignored);
            std::filesystem::remove(staged, ignored);
        };
        {
            std::ofstream output(temp, std::ios::binary);
            if (!output) { result.message = "Could not create the temporary document payload."; return result; }
            output.write(reinterpret_cast<const char*>(payload.data()),
                         static_cast<std::streamsize>(payload.size()));
        }
        if (!RunRScriptText(::rlispstat::core::NativeLinkEDAWriteRScript(),
                            {temp.wstring(), staged.wstring()}, result.message))
        { cleanup(); return result; }
        if (!MoveFileExW(staged.c_str(), destination.c_str(),
                         MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH))
        {
            result.message = "Windows could not atomically replace the LinkEDA document.";
            cleanup(); return result;
        }
        cleanup(); result.ok = true; return result;
    }

    NativeDocumentResult ReadNativeDocument(std::string const& path)
    {
        NativeDocumentResult result;
        const auto unique = std::to_wstring(GetCurrentProcessId()) + L"-" +
            std::to_wstring(GetTickCount64());
        const auto payloadPath = std::filesystem::temp_directory_path() /
            (L"linkeda-payload-" + unique + L".bin");
        const auto cleanup = [&]()
        {
            std::error_code ignored; std::filesystem::remove(payloadPath, ignored);
        };
        if (!RunRScriptText(::rlispstat::core::NativeLinkEDAReadRScript(),
                            {std::filesystem::path(to_hstring(path).c_str()).wstring(),
                             payloadPath.wstring()}, result.message))
        { cleanup(); return result; }
        std::ifstream input(payloadPath, std::ios::binary);
        result.payload.assign(std::istreambuf_iterator<char>(input), {});
        cleanup();
        if (result.payload.empty()) { result.message = "The LinkEDA document contains no data."; return result; }
        result.ok = true; return result;
    }

    std::vector<std::string> VariableNames(::rlispstat::core::DataFrameModel const& dataframe)
    {
        std::vector<std::string> names;
        names.reserve(dataframe.columns.size());
        for (auto const& column : dataframe.columns) names.push_back(column.name);
        return names;
    }

    std::vector<std::string> MeanComparisonNumericVariables(
        ::rlispstat::core::DataFrameModel const& dataframe)
    {
        std::vector<std::string> names;
        for (auto const& column : dataframe.columns)
        {
            const auto type = ::rlispstat::core::NormalizeVariableType(column.type);
            if ((type == "numeric" || type == "ordered") &&
                !::rlispstat::core::DataColumnLooksLikeId(column)) names.push_back(column.name);
        }
        return names;
    }

    std::vector<std::string> MeanComparisonLevels(
        ::rlispstat::core::DataColumn const& column);

    bool MeanComparisonCategoricalType(std::string const& rawType)
    {
        return ::rlispstat::core::VariableTypeIsCategorical(rawType);
    }

    bool MeanComparisonBinaryCategorical(::rlispstat::core::DataColumn const& column)
    {
        return ::rlispstat::core::MeanComparisonBinaryResponse(column);
    }

    std::vector<std::string> MeanComparisonResponseVariables(
        ::rlispstat::core::DataFrameModel const& dataframe, bool allowBinaryCategorical)
    {
        std::vector<std::string> names;
        const bool pooledMultipleImputation =
            dataframe.datasetType == "multiple_imputation";
        for (auto const& column : dataframe.columns)
        {
            if (::rlispstat::core::DataColumnLooksLikeId(column)) continue;
            const auto type = ::rlispstat::core::NormalizeVariableType(column.type);
            if (::rlispstat::core::MeanComparisonResponseTypeAllowed(
                    allowBinaryCategorical ? "comparison" : "one_way_anova", type,
                    allowBinaryCategorical && MeanComparisonBinaryCategorical(column), pooledMultipleImputation))
                names.push_back(column.name);
        }
        return names;
    }

    std::map<std::string, std::string> MeanComparisonVariableTypes(
        ::rlispstat::core::DataFrameModel const& dataframe)
    {
        std::map<std::string, std::string> result;
        for (auto const& column : dataframe.columns)
            result[column.name] = ::rlispstat::core::NormalizeVariableType(column.type);
        return result;
    }

    void ConfigureOneSampleTask(
        ::rlispstat::core::MainRCompareMeansTask& task,
        ::rlispstat::core::MeanComparisonState const& state,
        ::rlispstat::core::DataFrameModel const& dataframe)
    {
        task.testType = "one_sample_mixed";
        for (auto const& response : state.specification.dependentVariableIds)
        {
            auto column = std::find_if(dataframe.columns.begin(), dataframe.columns.end(),
                [&](auto const& candidate) { return candidate.name == response; });
            if (column == dataframe.columns.end()) continue;
            const auto type = ::rlispstat::core::NormalizeVariableType(column->type);
            const std::string method = type == "ordered" ? "wilcoxon" :
                MeanComparisonBinaryCategorical(*column) ? "binomial" : "student";
            const auto value = state.specification.testValues.find(response);
            const double testValue = value != state.specification.testValues.end()
                ? value->second : method == "binomial" ? 0.5 : state.specification.testValue;
            task.oneSampleTests.push_back({response, testValue, method});
        }
    }

    void ConfigureIndependentSamplesTask(
        ::rlispstat::core::MainRCompareMeansTask& task,
        ::rlispstat::core::MeanComparisonState const& state,
        ::rlispstat::core::DataFrameModel const& dataframe)
    {
        task.testType = "independent_mixed";
        for (auto const& response : state.specification.dependentVariableIds)
        {
            auto column = std::find_if(dataframe.columns.begin(), dataframe.columns.end(),
                [&](auto const& candidate) { return candidate.name == response; });
            if (column == dataframe.columns.end()) continue;
            const auto type = ::rlispstat::core::NormalizeVariableType(column->type);
            const std::string method = MeanComparisonBinaryCategorical(*column) ? "proportion" :
                type == "ordered" ? "mann_whitney" :
                task.method == "student" ? "student" :
                task.method == "mann_whitney" ? "mann_whitney" : "welch";
            task.oneSampleTests.push_back({response, 0.0, method});
        }
    }

    void ConfigurePairedSamplesTask(
        ::rlispstat::core::MainRCompareMeansTask& task,
        ::rlispstat::core::MeanComparisonState const& state,
        ::rlispstat::core::DataFrameModel const& dataframe)
    {
        task.testType = "paired_mixed";
        for (auto const& pair : state.specification.pairs)
        {
            auto first = std::find_if(dataframe.columns.begin(), dataframe.columns.end(),
                [&](auto const& candidate) { return candidate.name == pair.firstVariableId; });
            auto second = std::find_if(dataframe.columns.begin(), dataframe.columns.end(),
                [&](auto const& candidate) { return candidate.name == pair.secondVariableId; });
            if (first == dataframe.columns.end() || second == dataframe.columns.end()) continue;
            const auto firstType = ::rlispstat::core::NormalizeVariableType(first->type);
            const auto secondType = ::rlispstat::core::NormalizeVariableType(second->type);
            const bool binaryPair = MeanComparisonBinaryCategorical(*first) &&
                MeanComparisonBinaryCategorical(*second);
            const std::string method = binaryPair ? "mcnemar" :
                (firstType == "ordered" || secondType == "ordered" || task.method == "wilcoxon")
                    ? "wilcoxon" : "student";
            task.oneSampleTests.push_back({
                pair.firstVariableId + '\x1f' + pair.secondVariableId, 0.0, method});
        }
    }

    std::map<std::string, std::string> Table1CanonicalVariableTypes(
        ::rlispstat::core::DataFrameModel const& dataframe,
        std::vector<std::string> const& variables,
        std::string const& groupVariable)
    {
        std::set<std::string> requested(variables.begin(), variables.end());
        if (!groupVariable.empty()) requested.insert(groupVariable);
        std::map<std::string, std::string> types;
        for (auto const& column : dataframe.columns)
        {
            if (!requested.count(column.name)) continue;
            const auto type = ::rlispstat::core::NormalizeVariableType(column.type);
            if (type == "numeric") types[column.name] = "numeric";
            else if (type == "ordered") types[column.name] = "ordinal";
            else if (::rlispstat::core::VariableTypeIsCategorical(type))
                types[column.name] = "categorical";
        }
        return types;
    }

    std::vector<std::string> MeanComparisonLevels(::rlispstat::core::DataColumn const& column)
    {
        return ::rlispstat::core::DataColumnObservedLevels(column);
    }

    bool MeanComparisonSameLevelSet(
        std::vector<std::string> const& left,
        std::vector<std::string> const& right)
    {
        if (left.size() != right.size()) return false;
        std::set<std::string> leftSet(left.begin(), left.end());
        std::set<std::string> rightSet(right.begin(), right.end());
        return leftSet.size() == left.size() && rightSet.size() == right.size() &&
            leftSet == rightSet;
    }

    bool MeanComparisonPairedVariablesCompatible(
        ::rlispstat::core::DataFrameModel const& dataframe,
        std::string const& firstName,
        std::string const& secondName)
    {
        if (firstName.empty() || firstName == secondName) return false;
        const auto first = std::find_if(dataframe.columns.begin(), dataframe.columns.end(),
            [&](auto const& column) { return column.name == firstName; });
        const auto second = std::find_if(dataframe.columns.begin(), dataframe.columns.end(),
            [&](auto const& column) { return column.name == secondName; });
        if (first == dataframe.columns.end() || second == dataframe.columns.end()) return false;
        const bool firstBinary = MeanComparisonBinaryCategorical(*first);
        const bool secondBinary = MeanComparisonBinaryCategorical(*second);
        if (firstBinary || secondBinary)
            return ::rlispstat::core::MeanComparisonBinaryPairCompatible(*first, *second);
        const auto firstType = ::rlispstat::core::NormalizeVariableType(first->type);
        const auto secondType = ::rlispstat::core::NormalizeVariableType(second->type);
        const auto numericOrOrdinal = [](std::string const& type)
        { return type == "numeric" || type == "ordered"; };
        return numericOrOrdinal(firstType) && numericOrOrdinal(secondType);
    }

    bool MeanComparisonGroupingCandidate(::rlispstat::core::DataColumn const& column,
                                         bool independent)
    {
        if (::rlispstat::core::DataColumnLooksLikeId(column)) return false;
        if (independent)
            return ::rlispstat::core::AnalysisVariableIsCompatible(
                column, ::rlispstat::core::AnalysisVariableType::TwoGroupGrouping);
        const auto levels = MeanComparisonLevels(column);
        const bool categorical = ::rlispstat::core::VariableTypeIsCategorical(column.type);
        const bool numeric = ::rlispstat::core::VariableTypeIsNumeric(column.type);
        if (levels.size() < 2) return false;
        if (categorical) return true;
        if (!numeric) return false;
        return levels.size() <= std::max<std::size_t>(
            12, static_cast<std::size_t>(std::ceil(std::sqrt(
                static_cast<double>(column.values.size())))));
    }

    bool MeanComparisonUsesGroupingVariable(
        ::rlispstat::core::MeanComparisonState const& state)
    {
        return state.analysisType == "independent_samples_t_test" ||
            state.analysisType == "one_way_anova";
    }

    bool MeanComparisonReadyForRefit(
        ::rlispstat::core::MeanComparisonState const& state)
    {
        if (state.analysisType == "paired_samples_t_test")
            return !state.specification.pairs.empty();
        if (state.specification.dependentVariableIds.empty()) return false;
        if (!MeanComparisonUsesGroupingVariable(state)) return true;
        if (!state.specification.groupingVariableId.has_value()) return false;
        if (std::find(state.specification.dependentVariableIds.begin(),
            state.specification.dependentVariableIds.end(),
            *state.specification.groupingVariableId) !=
            state.specification.dependentVariableIds.end())
            return false;
        return state.analysisType != "independent_samples_t_test" ||
            (state.specification.groupOrderIds.size() == 2 &&
             state.specification.groupOrderIds[0] != state.specification.groupOrderIds[1]);
    }

    std::string MeanComparisonSelectionPrompt(
        ::rlispstat::core::MeanComparisonState const& state)
    {
        if (state.analysisType == "paired_samples_t_test")
            return state.specification.pairs.empty()
                ? "Choose a variable pair from the context menu."
                : std::string{};

        const bool missingDependent =
            state.specification.dependentVariableIds.empty();
        if (!MeanComparisonUsesGroupingVariable(state))
            return missingDependent
                ? "Choose a dependent variable from the context menu."
                : std::string{};

        const bool missingGroup =
            !state.specification.groupingVariableId.has_value();
        const bool sameVariable = !missingDependent && !missingGroup &&
            std::find(state.specification.dependentVariableIds.begin(),
                state.specification.dependentVariableIds.end(),
                *state.specification.groupingVariableId) !=
                state.specification.dependentVariableIds.end();
        if (sameVariable)
            return "The dependent and grouping variables must be different.";
        if (missingDependent && missingGroup)
            return "Choose a dependent variable and a grouping variable.";
        if (missingDependent)
            return "Choose a dependent variable from the context menu.";
        if (missingGroup)
            return "Choose a grouping variable.";
        return std::string{};
    }

    // Variable type is dataset metadata.  Keep an already-open mean comparison
    // attached to the same variables whenever their roles remain admissible;
    // otherwise remove only the now-invalid role and let the native controls
    // make the missing choice explicit instead of fitting a stale model.
    bool ReconcileMeanComparisonVariableRoles(
        ::rlispstat::core::MeanComparisonState& state,
        ::rlispstat::core::DataFrameModel const& dataframe)
    {
        const bool mixedResponses = state.analysisType == "one_sample_t_test" ||
            state.analysisType == "independent_samples_t_test" ||
            state.analysisType == "paired_samples_t_test";
        const auto responses = MeanComparisonResponseVariables(dataframe, mixedResponses);
        const auto isAdmissible = [&](std::string const& variable)
        {
            return std::find(responses.begin(), responses.end(), variable) != responses.end();
        };

        auto& dependents = state.specification.dependentVariableIds;
        dependents.erase(std::remove_if(dependents.begin(), dependents.end(),
            [&](std::string const& variable) { return !isAdmissible(variable); }),
            dependents.end());
        for (auto iterator = state.specification.testValues.begin();
             iterator != state.specification.testValues.end();)
            if (std::find(dependents.begin(), dependents.end(), iterator->first) == dependents.end())
                iterator = state.specification.testValues.erase(iterator);
            else ++iterator;

        auto& pairs = state.specification.pairs;
        pairs.erase(std::remove_if(pairs.begin(), pairs.end(),
            [&](auto const& pair)
            {
                return !isAdmissible(pair.firstVariableId) ||
                    !isAdmissible(pair.secondVariableId);
            }), pairs.end());

        if (MeanComparisonUsesGroupingVariable(state))
        {
            const bool independent =
                state.analysisType == "independent_samples_t_test";
            const auto current = state.specification.groupingVariableId;
            const auto column = current ? std::find_if(dataframe.columns.begin(),
                dataframe.columns.end(), [&](auto const& candidate)
                { return candidate.name == *current; }) : dataframe.columns.end();
            if (!current || column == dataframe.columns.end() ||
                !MeanComparisonGroupingCandidate(*column, independent) ||
                std::find(dependents.begin(), dependents.end(), *current) != dependents.end())
            {
                state.specification.groupingVariableId.reset();
                state.specification.groupOrderIds.clear();
                state.groupVariable.clear();
            }
            else
            {
                const auto levels = MeanComparisonLevels(*column);
                if (independent &&
                    !::rlispstat::core::MeanComparisonIndependentGroupOrderIsValid(
                        state.specification.groupOrderIds, levels))
                    state.specification.groupOrderIds =
                        ::rlispstat::core::MeanComparisonDefaultIndependentGroupOrder(levels);
                else if (!independent && !MeanComparisonSameLevelSet(
                    state.specification.groupOrderIds, levels))
                    state.specification.groupOrderIds = levels;
                state.groupVariable = *current;
            }
        }
        else
        {
            state.specification.groupingVariableId.reset();
            state.specification.groupOrderIds.clear();
            state.groupVariable.clear();
        }
        return MeanComparisonReadyForRefit(state);
    }

    std::string SnapshotFileStem(std::string value, std::size_t fallbackIndex)
    {
        for (char& ch : value)
            if (!(std::isalnum(static_cast<unsigned char>(ch)) || ch == '-' || ch == '_')) ch = '_';
        while (!value.empty() && value.back() == '_') value.pop_back();
        if (value.empty()) value = "snapshot_" + std::to_string(fallbackIndex);
        return value;
    }

    std::string SnapshotCell(std::string value)
    {
        for (char& ch : value)
            if (ch == '\t' || ch == '\r' || ch == '\n') ch = ' ';
        return value;
    }

    std::string DataFrameSnapshotText(::rlispstat::core::DataFrameModel const& dataframe)
    {
        std::ostringstream output;
        output << '#';
        for (auto const& column : dataframe.columns)
            output << '\t' << SnapshotCell(column.displayName.empty()
                ? column.name : column.displayName);
        output << '\n';
        for (int row = 0; row < dataframe.rows; ++row)
        {
            output << row + 1;
            for (auto const& column : dataframe.columns)
            {
                auto const& values = column.displayValues.size() == column.values.size()
                    ? column.displayValues : column.values;
                output << '\t' << (static_cast<std::size_t>(row) < values.size()
                    ? SnapshotCell(values[static_cast<std::size_t>(row)]) : std::string{});
            }
            output << '\n';
        }
        return output.str();
    }

    void LogStartup(std::string const& message);

    ::rlispstat::core::FrozenTableContent Table1SnapshotContent(
        ::rlispstat::core::Table1DisplayState const& state)
    {
        ::rlispstat::core::FrozenTableContent table;
        if (!state.stubHeaders.empty())
            table.column_headers = state.stubHeaders;
        else
            table.column_headers.push_back("Variable");
        table.column_headers.insert(table.column_headers.end(),
            state.columns.begin(), state.columns.end());
        if (state.showP) table.column_headers.push_back("p");
        if (state.showTest) table.column_headers.push_back("Test");

        table.rows.reserve(state.rows.size() + state.footnotes.size());
        for (auto const& source : state.rows)
        {
            std::vector<std::string> row;
            if (!state.stubHeaders.empty())
            {
                row = source.stubValues;
                row.resize(state.stubHeaders.size());
            }
            else
                row.push_back(source.label);
            row.insert(row.end(), source.values.begin(), source.values.end());
            row.resize((state.stubHeaders.empty() ? 1 : state.stubHeaders.size()) +
                state.columns.size());
            if (state.showP) row.push_back(source.p);
            if (state.showTest) row.push_back(source.test);
            table.rows.push_back(std::move(row));
        }
        for (auto const& note : state.footnotes)
        {
            std::vector<std::string> row(table.column_headers.size());
            if (!row.empty()) row.front() = note;
            table.rows.push_back(std::move(row));
        }
        table.source_row_count = state.rows.size();
        table.total_source_row_count = state.rows.size();
        return table;
    }

    ::rlispstat::core::FrozenTableContent CorrelationSnapshotContent(
        ::rlispstat::core::CorrelationMatrixState const& state)
    {
        using namespace ::rlispstat::core;
        FrozenTableContent table;
        table.column_headers.reserve(state.variables.size() + 1);
        table.column_headers.push_back("Variable");
        table.column_headers.insert(table.column_headers.end(),
            state.variables.begin(), state.variables.end());
        table.column_widths.assign(table.column_headers.size(), 120.0);
        if (!table.column_widths.empty()) table.column_widths.front() = 150.0;

        table.rows.reserve(state.variables.size());
        for (std::size_t rowIndex = 0; rowIndex < state.variables.size(); ++rowIndex)
        {
            std::vector<std::string> row;
            row.reserve(state.variables.size() + 1);
            row.push_back(state.variables[rowIndex]);
            for (std::size_t columnIndex = 0;
                 columnIndex < state.variables.size(); ++columnIndex)
            {
                std::string value=CorrelationDisplayedCellText(state,rowIndex,columnIndex);
                row.push_back(std::move(value));
            }
            table.rows.push_back(std::move(row));
        }
        table.source_row_count = table.rows.size();
        table.total_source_row_count = table.rows.size();
        table.tab_delimited_text = FrozenTableToTabDelimited(table);
        return table;
    }

    fire_and_forget LoadSnapshotSvgPreviewAsync(
        Microsoft::UI::Xaml::Controls::WebView2 preview,
        std::string svg,
        bool preserveNaturalSize = false)
    {
        try
        {
            co_await preview.EnsureCoreWebView2Async();
            if (auto core = preview.CoreWebView2())
            {
                core.Settings().AreDefaultContextMenusEnabled(false);
                core.Settings().AreDevToolsEnabled(false);
                core.NavigationCompleted([](auto const&, auto const& args)
                {
                    if (!args.IsSuccess())
                        LogStartup("Snapshot preview navigation failed: " +
                            std::to_string(static_cast<int>(args.WebErrorStatus())));
                });
                const std::string svgLayout = preserveNaturalSize
                    ? "body{display:flex;align-items:center;justify-content:center;}"
                      "svg{display:block;max-width:100%;max-height:100%;width:auto;height:auto;}"
                    : "svg{display:block;width:100%;height:100%;}";
                const std::string html =
                    "<!doctype html><html><head><meta charset=\"utf-8\"><style>"
                    "html,body{margin:0;width:100%;height:100%;overflow:hidden;background:white;}"
                    + svgLayout +
                    "</style></head><body>" + svg + "</body></html>";
                preview.NavigateToString(to_hstring(html));
            }
        }
        catch (hresult_error const& error)
        {
            LogStartup("Snapshot preview WebView failed: " + to_string(error.message()));
        }
        catch (...)
        {
            LogStartup("Snapshot preview WebView failed: unknown error");
        }
    }

    Microsoft::UI::Xaml::FrameworkElement SnapshotTablePreview(
        ::rlispstat::core::FrozenTableContent const& frozen,
        bool fillAvailable = false)
    {
        using namespace Microsoft::UI::Xaml;
        using namespace Microsoft::UI::Xaml::Controls;
        auto layout = ::rlispstat::core::BuildVectorTableLayout("",
            ::rlispstat::core::FrozenTableToTabDelimited(frozen), 1400.0);
        auto table = Grid();
        const std::size_t columns = layout.columnWidths.size();
        const std::size_t visibleRows = std::min<std::size_t>(layout.rows.size(), 40);
        for (std::size_t column = 0; column < columns; ++column)
        {
            auto definition = ColumnDefinition();
            definition.Width(GridLengthHelper::FromPixels(std::clamp(
                layout.columnWidths[column] * 1.22, 90.0, 260.0)));
            table.ColumnDefinitions().Append(definition);
        }
        for (std::size_t row = 0; row < visibleRows; ++row)
        {
            auto definition = RowDefinition();
            definition.Height(GridLengthHelper::Auto());
            table.RowDefinitions().Append(definition);
            for (std::size_t column = 0; column < columns; ++column)
            {
                auto cell = Border();
                cell.Padding(Thickness{8, 5, 8, 5});
                cell.BorderBrush(Media::SolidColorBrush(
                    Windows::UI::ColorHelper::FromArgb(255, 210, 213, 218)));
                cell.BorderThickness(Thickness{0, 0, 1, 1});
                if (row == 0)
                    cell.Background(Media::SolidColorBrush(
                        Windows::UI::ColorHelper::FromArgb(255, 242, 244, 247)));
                auto value = TextBlock();
                value.Text(to_hstring(column < layout.rows[row].size()
                    ? layout.rows[row][column] : std::string{}));
                value.TextWrapping(TextWrapping::NoWrap);
                value.TextTrimming(TextTrimming::CharacterEllipsis);
                if (row == 0)
                    value.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
                cell.Child(value);
                Grid::SetRow(cell, static_cast<int>(row));
                Grid::SetColumn(cell, static_cast<int>(column));
                table.Children().Append(cell);
            }
        }
        auto scroll = ScrollViewer();
        scroll.HorizontalScrollBarVisibility(ScrollBarVisibility::Auto);
        scroll.VerticalScrollBarVisibility(ScrollBarVisibility::Auto);
        if (!fillAvailable) scroll.MaxHeight(260);
        scroll.HorizontalAlignment(HorizontalAlignment::Stretch);
        scroll.VerticalAlignment(VerticalAlignment::Stretch);
        scroll.Content(table);
        return scroll;
    }

    std::string SnapshotBase64(std::vector<unsigned char> const& data)
    {
        static constexpr char alphabet[] =
            "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
        std::string encoded;
        encoded.reserve((data.size() + 2) / 3 * 4);
        for (std::size_t index = 0; index < data.size(); index += 3)
        {
            const unsigned int a = data[index];
            const unsigned int b = index + 1 < data.size() ? data[index + 1] : 0;
            const unsigned int c = index + 2 < data.size() ? data[index + 2] : 0;
            const unsigned int value = (a << 16) | (b << 8) | c;
            encoded.push_back(alphabet[(value >> 18) & 63]);
            encoded.push_back(alphabet[(value >> 12) & 63]);
            encoded.push_back(index + 1 < data.size() ? alphabet[(value >> 6) & 63] : '=');
            encoded.push_back(index + 2 < data.size() ? alphabet[value & 63] : '=');
        }
        return encoded;
    }

    std::string SnapshotTextTable(std::string text)
    {
        for (char& ch : text) if (ch == '\t') ch = ' ';
        return std::string("Output\n") + text;
    }

    std::string SnapshotSvgDocument(::rlispstat::core::SnapshotItem const& snapshot,
                                    bool albumEditor = false)
    {
        std::string svg;
        if (auto const* plot = std::get_if<::rlispstat::core::FrozenPlotContent>(
                &snapshot.renderable_content))
            svg = ::rlispstat::core::BuildSvgPlotSnapshotDocument(
                plot->model, snapshot.metadata.theme_name, snapshot.metadata.title,
                plot->preferred_width, plot->preferred_height).svg;
        else if (auto const* table = std::get_if<::rlispstat::core::FrozenTableContent>(
                     &snapshot.renderable_content))
            svg = ::rlispstat::core::BuildSvgTableDocument(snapshot.metadata.title,
                ::rlispstat::core::FrozenTableToTabDelimited(*table)).svg;
        else if (auto const* vector = std::get_if<::rlispstat::core::FrozenSvgContent>(
                     &snapshot.renderable_content))
            svg = vector->svg;
        else if (auto const* text = std::get_if<::rlispstat::core::FrozenTextContent>(
                     &snapshot.renderable_content))
            svg = ::rlispstat::core::BuildSvgTableDocument(snapshot.metadata.title,
                SnapshotTextTable(text->plain_text)).svg;
        else if (auto const* image = std::get_if<::rlispstat::core::FrozenImageContent>(
                     &snapshot.renderable_content))
        {
            std::ostringstream output;
            output << "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\""
                << image->preferred_width << "\" height=\"" << image->preferred_height
                << "\" viewBox=\"0 0 " << image->preferred_width << ' '
                << image->preferred_height << "\"><rect width=\"100%\" height=\"100%\" "
                   "fill=\"white\"/><image width=\"100%\" height=\"100%\" "
                   "href=\"data:image/png;base64," << SnapshotBase64(image->png_data)
                << "\"/></svg>";
            svg = output.str();
        }
        if (!svg.empty() && !albumEditor && snapshot.metadata.include_notes)
        {
            svg = ::rlispstat::core::ComposeSvgDocumentWithWindowStickyNotes(
                svg, snapshot.stickers);
            if (snapshot.note && snapshot.note->has_content)
                svg = ::rlispstat::core::ComposeSvgDocumentWithWindowNote(svg,
                    {"Notes", snapshot.note->plain_text});
        }
        return svg;
    }

    std::string SnapshotNativePngSvgDocument(
        ::rlispstat::core::SnapshotItem const& snapshot,
        std::vector<uint8_t> const& png,
        double width,
        double height)
    {
        width = std::max(320.0, width);
        height = std::max(240.0, height);
        std::ostringstream output;
        output << "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\""
            << width << "\" height=\"" << height << "\" viewBox=\"0 0 "
            << width << ' ' << height << "\"><rect width=\"100%\" height=\"100%\" "
               "fill=\"white\"/><image width=\"100%\" height=\"100%\" "
               "preserveAspectRatio=\"xMidYMid meet\" href=\"data:image/png;base64,"
            << SnapshotBase64(png) << "\"/></svg>";
        std::string svg = output.str();
        if (snapshot.metadata.include_notes)
        {
            svg = ::rlispstat::core::ComposeSvgDocumentWithWindowStickyNotes(
                svg, snapshot.stickers);
            if (snapshot.note && snapshot.note->has_content)
                svg = ::rlispstat::core::ComposeSvgDocumentWithWindowNote(svg,
                    {"Notes", snapshot.note->plain_text});
        }
        return svg;
    }

    Microsoft::UI::Xaml::FrameworkElement SnapshotPreviewElement(
        ::rlispstat::core::SnapshotItem const& snapshot,
        bool fillAvailable = false)
    {
        using namespace Microsoft::UI::Xaml;
        using namespace Microsoft::UI::Xaml::Controls;
        if (auto const* plot = std::get_if<::rlispstat::core::FrozenPlotContent>(
                &snapshot.renderable_content))
        {
            std::string svg = SnapshotSvgDocument(snapshot, fillAvailable);
            auto preview = WebView2();
            if (!fillAvailable)
                preview.Height(std::clamp(plot->preferred_height * 0.58, 230.0, 360.0));
            else
            {
                preview.MinWidth(640);
                preview.MinHeight(480);
            }
            preview.HorizontalAlignment(HorizontalAlignment::Stretch);
            preview.VerticalAlignment(VerticalAlignment::Stretch);
            preview.IsHitTestVisible(false);
            preview.Loaded([preview, svg = std::move(svg)](auto const&, auto const&)
            {
                LoadSnapshotSvgPreviewAsync(preview, svg);
            });
            return preview;
        }
        if (auto const* image = std::get_if<::rlispstat::core::FrozenImageContent>(
                &snapshot.renderable_content))
        {
            std::string svg = SnapshotSvgDocument(snapshot, fillAvailable);
            auto preview = WebView2();
            if (!fillAvailable)
                preview.Height(std::clamp(image->preferred_height * 0.58, 230.0, 360.0));
            else
            {
                preview.MinWidth(640);
                preview.MinHeight(480);
            }
            preview.HorizontalAlignment(HorizontalAlignment::Stretch);
            preview.VerticalAlignment(VerticalAlignment::Stretch);
            preview.IsHitTestVisible(false);
            preview.Loaded([preview, svg = std::move(svg)](auto const&, auto const&)
            {
                LoadSnapshotSvgPreviewAsync(preview, svg);
            });
            return preview;
        }
        if (auto const* vector = std::get_if<::rlispstat::core::FrozenSvgContent>(
                &snapshot.renderable_content))
        {
            std::string svg = SnapshotSvgDocument(snapshot, fillAvailable);
            auto preview = WebView2();
            if (!fillAvailable)
                preview.Height(std::clamp(
                    vector->preferred_height * 0.58, 230.0, 360.0));
            else
            {
                preview.MinWidth(640);
                preview.MinHeight(480);
            }
            preview.HorizontalAlignment(HorizontalAlignment::Stretch);
            preview.VerticalAlignment(VerticalAlignment::Stretch);
            preview.IsHitTestVisible(false);
            preview.Loaded([preview, svg = std::move(svg)](auto const&, auto const&)
            {
                LoadSnapshotSvgPreviewAsync(preview, svg, true);
            });
            return preview;
        }
        if (auto const* table = std::get_if<::rlispstat::core::FrozenTableContent>(
                &snapshot.renderable_content))
            return SnapshotTablePreview(*table, fillAvailable);
        if (auto const* text = std::get_if<::rlispstat::core::FrozenTextContent>(
                &snapshot.renderable_content))
        {
            auto preview = TextBox();
            preview.Text(to_hstring(text->plain_text));
            preview.IsReadOnly(true);
            preview.AcceptsReturn(true);
            preview.TextWrapping(TextWrapping::NoWrap);
            preview.FontFamily(Media::FontFamily(L"Consolas"));
            if (!fillAvailable) preview.MaxHeight(240);
            preview.HorizontalAlignment(HorizontalAlignment::Stretch);
            preview.VerticalAlignment(VerticalAlignment::Stretch);
            return preview;
        }
        auto unavailable = TextBlock();
        unavailable.Text(L"Snapshot preview unavailable");
        unavailable.FontStyle(Windows::UI::Text::FontStyle::Italic);
        return unavailable;
    }

    std::string ScopeName(::rlispstat::core::AnalysisScope const& scope)
    {
        return scope.kind == ::rlispstat::core::AnalysisScopeKind::ExplicitRowIds
            ? "selected" : "all";
    }

    std::string StartupId(char const* prefix, std::string const& group)
    {
        return std::string(prefix) + group + "_" +
            std::to_string(static_cast<unsigned long long>(GetTickCount64()));
    }

    hstring ListenerPort()
    {
        wchar_t value[16]{};
        const DWORD length = GetEnvironmentVariableW(L"LINKEDA_WINUI_PORT", value, 16);
        return length > 0 && length < 16 ? hstring(value) : hstring(L"39072");
    }

    void LogStartup(std::string const& message)
    {
        char tempPath[MAX_PATH]{};
        if (GetTempPathA(MAX_PATH, tempPath) == 0) return;
        std::ofstream output(std::string(tempPath) + "rlispstat_winui_startup.log", std::ios::app);
        output << message << '\n';
    }

    std::vector<std::wstring> SplitLines(std::wstring const& request)
    {
        std::wistringstream input(request);
        std::vector<std::wstring> lines;
        for (std::wstring line; std::getline(input, line);)
        {
            if (!line.empty() && line.back() == L'\r') line.pop_back();
            lines.push_back(std::move(line));
        }
        return lines;
    }

    std::vector<std::string> DecodeRequestLines(std::wstring const& request)
    {
        const auto wideLines = SplitLines(request);
        std::vector<std::string> lines;
        lines.reserve(wideLines.size());
        for (auto const& line : wideLines) lines.push_back(to_string(hstring(line)));
        if (!lines.empty() && lines.back() == to_string(hstring(kMessageEnd)))
            lines.pop_back();
        return lines;
    }

    bool IsCompleteRequest(std::wstring const& request)
    {
        const auto lines = SplitLines(request);
        if (lines.empty()) return false;
        // R waits for the response before closing its socket, so EOF cannot
        // delimit a request.  The transport-only marker lets all commands,
        // including variable-length data and analysis payloads, use the same
        // framing without changing their public command protocol.
        return lines.back() == kMessageEnd;
    }

    bool ApplyDiagnosticPlotData(::rlispstat::core::PlotModel& model,
                                 ::rlispstat::core::DiagnosticPlotData const& data)
    {
        if (!data.ok) return false;
        const int previousBinCount = static_cast<int>(model.histogramBins.size());
        model.kind = data.kind;
        model.glmDiagnosticKind = data.diagnosticKind;
        model.title = data.title;
        if (model.diagnosticImputationCount > 0 &&
            !model.diagnosticShowImputationUncertainty)
            model.title += " - Showing imputation " +
                std::to_string(model.diagnosticImputationIndex) + " of " +
                std::to_string(model.diagnosticImputationCount) + " (not pooled)";
        model.xLabel = data.xLabel;
        model.yLabel = data.yLabel;
        model.diagnosticShowIdentityLine = data.showIdentityLine;
        model.points.clear();
        model.histogramPoints.clear();
        model.histogramBins.clear();
        model.rocStepPoints.clear();
        model.rocThresholds.clear();
        model.interactionPlotLines = data.lines;
        model.diagnosticImputationValues = data.imputationValues;
        if (data.kind == "histogram")
        {
            for (auto const& point : data.points)
                model.histogramPoints.push_back({ point.x, point.row, 0 });
            if (previousBinCount > 0) ::rlispstat::core::RebinHistogram(model, previousBinCount);
            else ::rlispstat::core::RebinHistogramByRule(model, "sturges");
        }
        else
        {
            for (auto const& point : data.points)
                model.points.push_back({ point.x, point.y, point.row });
            for (auto const& point : data.visualPoints)
                model.rocStepPoints.push_back({ point.x, point.y, 0 });
            for (auto const& threshold : data.rocThresholds)
            {
                ::rlispstat::core::ROCThresholdLink link;
                link.point = { threshold.falsePositiveRate, threshold.truePositiveRate, 0 };
                link.threshold = threshold.threshold;
                link.truePositive = threshold.truePositive;
                link.falsePositive = threshold.falsePositive;
                link.trueNegative = threshold.trueNegative;
                link.falseNegative = threshold.falseNegative;
                link.crossingRows = threshold.crossingRows;
                link.falsePositiveRows = threshold.falsePositiveRows;
                link.falseNegativeRows = threshold.falseNegativeRows;
                model.rocThresholds.push_back(std::move(link));
            }
            ::rlispstat::core::ComputeRanges(model);
            if (!model.diagnosticImputationValues.empty())
            {
                std::vector<::rlispstat::core::ScatterplotPointValue> points;
                points.reserve(model.points.size());
                for (auto const& point : model.points)
                    points.push_back({ point.row, point.x, point.y });
                if (auto viewport =
                    ::rlispstat::core::ScatterplotViewportIncludingPointImputations(
                        points, model.diagnosticImputationValues,
                        model.scatterImputationUncertainty))
                {
                    model.xmin = viewport->xmin; model.xmax = viewport->xmax;
                    model.ymin = viewport->ymin; model.ymax = viewport->ymax;
                }
            }
            ::rlispstat::core::ApplyDiagnosticIdentityRange(model);
        }
        model.displayedFitVersion = data.fitVersion;
        model.displayedDiagnosticsVersion = data.diagnosticsVersion;
        model.displayedDiagnosticOptionsVersion = data.diagnosticOptionsVersion;
        model.displayedResidualType = data.residualType;
        return true;
    }

    void SetDiagnosticImputedModelInputRows(
        ::rlispstat::core::PlotModel& model,
        ::rlispstat::core::DataFrameModel const* dataframe,
        ::rlispstat::core::ModelSpecification const& specification,
        std::vector<std::string> const& additionalVariables = {})
    {
        model.diagnosticRowsWithImputedModelInputs = dataframe
            ? ::rlispstat::core::RowsWithImputedModelInputs(
                *dataframe, specification, additionalVariables)
            : std::set<int>{};
    }

    void InitializeDiagnosticImputationPresentation(
        ::rlispstat::core::PlotModel& model,
        ::rlispstat::core::DataFrameModel const* dataframe,
        std::string const& diagnosticKind)
    {
        model.diagnosticShowImputationUncertainty = dataframe &&
            ::rlispstat::core::DiagnosticImputationUncertaintyShouldBeDefault(
                *dataframe, diagnosticKind);
        model.diagnosticImputationUncertaintyScope = "all";
    }

    std::vector<::rlispstat::core::GLMDiagnosticRow> const&
    LinearDiagnosticsForPlot(::rlispstat::core::PlotModel& model,
                             ::rlispstat::core::GLMFitSummary const& fit)
    {
        model.diagnosticImputationCount =
            static_cast<int>(fit.diagnosticsByImputation.size());
        if (fit.diagnosticsByImputation.empty())
        {
            model.diagnosticImputationIndex = 1;
            return fit.diagnostics;
        }
        model.diagnosticImputationIndex = std::clamp(
            model.diagnosticImputationIndex, 1,
            static_cast<int>(fit.diagnosticsByImputation.size()));
        return fit.diagnosticsByImputation[
            static_cast<std::size_t>(model.diagnosticImputationIndex - 1)];
    }

    std::vector<::rlispstat::core::GeneralizedGLMDiagnosticRow> const&
    GeneralizedDiagnosticsForPlot(::rlispstat::core::PlotModel& model,
                                  ::rlispstat::core::GeneralizedGLMState const& state)
    {
        model.diagnosticImputationCount =
            static_cast<int>(state.diagnosticsByImputation.size());
        if (state.diagnosticsByImputation.empty())
        {
            model.diagnosticImputationIndex = 1;
            return state.diagnostics;
        }
        model.diagnosticImputationIndex = std::clamp(
            model.diagnosticImputationIndex, 1,
            static_cast<int>(state.diagnosticsByImputation.size()));
        return state.diagnosticsByImputation[
            static_cast<std::size_t>(model.diagnosticImputationIndex - 1)];
    }

    ::rlispstat::core::DiagnosticPlotData LinearDiagnosticDataForPlot(
        ::rlispstat::core::PlotModel& model,
        ::rlispstat::core::GLMFitSummary const& fit,
        std::string const& kind, int fitVersion, int diagnosticsVersion)
    {
        if (model.diagnosticShowImputationUncertainty &&
            fit.diagnosticsByImputation.size() > 1)
        {
            model.diagnosticImputationCount =
                static_cast<int>(fit.diagnosticsByImputation.size());
            std::vector<::rlispstat::core::DiagnosticPlotData> perImputation;
            perImputation.reserve(fit.diagnosticsByImputation.size());
            for (auto const& rows : fit.diagnosticsByImputation)
                perImputation.push_back(
                    ::rlispstat::core::BuildLinearDiagnosticPlotData(
                        kind,
                        model.displayedResidualType.empty() ? "raw" : model.displayedResidualType,
                        rows, fitVersion, diagnosticsVersion));
            const auto allImputations =
                ::rlispstat::core::BuildDiagnosticPlotDataAcrossImputations(
                    perImputation);
            model.diagnosticAllImputationValues = allImputations.imputationValues;
            return model.diagnosticImputationUncertaintyScope == "all"
                ? allImputations
                : ::rlispstat::core::BuildDiagnosticPlotDataAcrossImputations(
                    perImputation, model.diagnosticRowsWithImputedModelInputs);
        }
        // Do not erase the user's MI presentation while a replacement fit is
        // pending or while only one diagnostic slice is temporarily present.
        model.diagnosticAllImputationValues.clear();
        return ::rlispstat::core::BuildLinearDiagnosticPlotData(
            kind,
            model.displayedResidualType.empty() ? "raw" : model.displayedResidualType,
            LinearDiagnosticsForPlot(model, fit),
            fitVersion, diagnosticsVersion);
    }

    ::rlispstat::core::DiagnosticPlotData GeneralizedDiagnosticDataForPlot(
        ::rlispstat::core::PlotModel& model,
        ::rlispstat::core::GeneralizedGLMState const& state,
        std::string const& kind,
        ::rlispstat::core::DiagnosticOptions const& options,
        int fitVersion, int diagnosticsVersion)
    {
        const auto noteIndex=static_cast<std::size_t>(std::max(1,model.diagnosticImputationIndex)-1);
        model.diagnosticSummary=noteIndex<state.diagnosticNotesByImputation.size()
            ?state.diagnosticNotesByImputation[noteIndex]:std::string{};
        const auto diagnosticKind =
            ::rlispstat::core::NormalizeGeneralizedDiagnosticKind(kind);
        if (diagnosticKind == "observed_predicted_score_distribution" ||
            diagnosticKind == "boundary_zero_fit")
        {
            model.diagnosticImputationCount = static_cast<int>(
                state.boundedCountDistributionsByImputation.size());
            auto const* distribution = &state.boundedCountDistribution;
            if (!state.boundedCountDistributionsByImputation.empty())
            {
                model.diagnosticImputationIndex = std::clamp(
                    model.diagnosticImputationIndex, 1,
                    static_cast<int>(state.boundedCountDistributionsByImputation.size()));
                distribution = &state.boundedCountDistributionsByImputation[
                    static_cast<std::size_t>(model.diagnosticImputationIndex - 1)];
            }
            const bool includeCeiling = state.countRegression &&
                ::rlispstat::core::CountDistributionUsesTrials(state.countDistribution) &&
                state.countDistribution != ::rlispstat::core::CountDistribution::PerfectScore;
            return diagnosticKind == "boundary_zero_fit"
                ? ::rlispstat::core::BuildDiscreteBoundaryDiagnosticPlotData(
                    *distribution, includeCeiling, fitVersion, diagnosticsVersion)
                : ::rlispstat::core::BuildBoundedCountDistributionDiagnosticPlotData(
                    *distribution, fitVersion, diagnosticsVersion);
        }
        auto displayOptions = options;
        if (!model.displayedResidualType.empty() &&
            ::rlispstat::core::IsGeneralizedResidualTypeAvailable(
                state, model.displayedResidualType))
        {
            displayOptions.residualType = model.displayedResidualType;
        }
        if (model.diagnosticShowImputationUncertainty &&
            state.diagnosticsByImputation.size() > 1)
        {
            model.diagnosticImputationCount =
                static_cast<int>(state.diagnosticsByImputation.size());
            std::vector<::rlispstat::core::DiagnosticPlotData> perImputation;
            perImputation.reserve(state.diagnosticsByImputation.size());
            for (auto const& rows : state.diagnosticsByImputation)
                perImputation.push_back(
                    ::rlispstat::core::BuildGeneralizedDiagnosticPlotData(
                        kind, displayOptions, rows, fitVersion, diagnosticsVersion));
            const auto allImputations =
                ::rlispstat::core::BuildDiagnosticPlotDataAcrossImputations(
                    perImputation);
            model.diagnosticAllImputationValues = allImputations.imputationValues;
            return model.diagnosticImputationUncertaintyScope == "all"
                ? allImputations
                : ::rlispstat::core::BuildDiagnosticPlotDataAcrossImputations(
                    perImputation, model.diagnosticRowsWithImputedModelInputs);
        }
        // Keep this plot-level choice stable across model revisions.
        model.diagnosticAllImputationValues.clear();
        return ::rlispstat::core::BuildGeneralizedDiagnosticPlotData(
            kind, displayOptions, GeneralizedDiagnosticsForPlot(model, state),
            fitVersion, diagnosticsVersion);
    }

    fire_and_forget ReplyToClient(
        winrt::Windows::Networking::Sockets::StreamSocket socket,
        winrt::Microsoft::UI::Dispatching::DispatcherQueue dispatcher,
        winrt::rlispstatWinUI::implementation::App* app)
    {
        try
        {
            winrt::Windows::Storage::Streams::DataReader reader(socket.InputStream());
            reader.UnicodeEncoding(winrt::Windows::Storage::Streams::UnicodeEncoding::Utf8);
            reader.InputStreamOptions(winrt::Windows::Storage::Streams::InputStreamOptions::Partial);
            winrt::Windows::Storage::Streams::DataWriter writer(socket.OutputStream());
            writer.UnicodeEncoding(winrt::Windows::Storage::Streams::UnicodeEncoding::Utf8);

            for (;;)
            {
                ::rlispstat::windows::performance::Scope requestTiming("TCP.Request");
                std::wstring requestText;
                bool peerClosed = false;
                while (requestText.size() < 8 * 1024 * 1024)
                {
                    const auto length = co_await reader.LoadAsync(64 * 1024);
                    if (length == 0)
                    {
                        peerClosed = true;
                        break;
                    }
                    const auto chunk = reader.ReadString(length);
                    requestText.append(chunk.c_str(), chunk.size());
                    if (IsCompleteRequest(requestText)) break;
                }
                if (peerClosed && requestText.empty()) co_return;

                bool keepAlivePoll = false;
                std::string reply = "ERR incomplete WinUI command";
                if (IsCompleteRequest(requestText))
                {
                    // The R task poll is the only high-frequency command.  It
                    // may reuse one socket for the lifetime of the R session;
                    // normal commands retain the established one-request-per-
                    // connection protocol.
                    auto commandLines = DecodeRequestLines(requestText);
                    keepAlivePoll = commandLines.size() == 1 &&
                        commandLines.front() == "MAIN_R_TASKS_KEEPALIVE";
                    if (keepAlivePoll) commandLines.front() = "MAIN_R_TASKS";
                    const bool backgroundPoll = commandLines.size() == 1 &&
                        commandLines.front() == "MAIN_R_TASKS";
                    if (backgroundPoll && !app->HasPendingMainRTasks())
                    {
                        reply = "OK";
                    }
                    else
                    {
                        // Decode and split potentially large dataset payloads
                        // before entering the XAML dispatcher. Only state
                        // mutation and view work run on the UI thread.
                        co_await wil::resume_foreground(dispatcher,
                            backgroundPoll
                                ? winrt::Microsoft::UI::Dispatching::DispatcherQueuePriority::Low
                                : winrt::Microsoft::UI::Dispatching::DispatcherQueuePriority::Normal);
                        reply = app->DispatchRequest(commandLines);
                        // Socket I/O and timing do not belong on the XAML
                        // dispatcher after the UI portion has completed.
                        co_await winrt::resume_background();
                    }
                }

                writer.WriteString(to_hstring(reply + (keepAlivePoll
                    ? "\n__LINKEDA_TCP_RESPONSE_END__\n"
                    : "\n")));
                co_await writer.StoreAsync();
                co_await writer.FlushAsync();
                if (!keepAlivePoll) co_return;
            }
        }
        catch (hresult_error const& error)
        {
            LogStartup("TCP client error: "
                + std::to_string(static_cast<uint32_t>(error.code().value))
                + " " + to_string(error.message()));
        }
        catch (...)
        {
            LogStartup("TCP client error: unknown exception");
        }
    }

    void ComputeScatterRangesIncludingAllImputations(
        ::rlispstat::core::PlotModel& plot,
        ::rlispstat::core::DataFrameModel const& dataframe)
    {
        if (!::rlispstat::core::DataFrameShowsAllImputations(dataframe))
        {
            ::rlispstat::core::ComputeRanges(plot);
            return;
        }
        auto const* xColumn = ::rlispstat::core::FindDataColumnInDataFrame(
            dataframe, plot.xLabel);
        auto const* yColumn = ::rlispstat::core::FindDataColumnInDataFrame(
            dataframe, plot.yLabel);
        if (!xColumn || !yColumn)
        {
            ::rlispstat::core::ComputeRanges(plot);
            return;
        }
        std::vector<::rlispstat::core::ScatterplotPointValue> points;
        points.reserve(plot.points.size());
        for (auto const& point : plot.points)
            points.push_back({ point.row, point.x, point.y });
        auto viewport = ::rlispstat::core::ScatterplotViewportIncludingImputations(
            dataframe, *xColumn, *yColumn, points,
            plot.scatterImputationUncertainty);
        if (!viewport)
        {
            ::rlispstat::core::ComputeRanges(plot);
            return;
        }
        plot.dataXmin = plot.xmin = viewport->xmin;
        plot.dataXmax = plot.xmax = viewport->xmax;
        plot.dataYmin = plot.ymin = viewport->ymin;
        plot.dataYmax = plot.ymax = viewport->ymax;
    }

}

// To learn more about WinUI, the WinUI project structure,
// and more about our project templates, see: http://aka.ms/winui-project-info.

namespace winrt::rlispstatWinUI::implementation
{
    static std::optional<::rlispstat::core::SharedAnalysisKind>
    SharedInitializationKind(AnalysisWorkflowKind kind)
    {
        using Shared = ::rlispstat::core::SharedAnalysisKind;
        switch (kind)
        {
        case AnalysisWorkflowKind::Table1: return Shared::DescriptiveTable;
        case AnalysisWorkflowKind::CorrelationMatrix: return Shared::CorrelationMatrix;
        case AnalysisWorkflowKind::Dimensionality:
        case AnalysisWorkflowKind::FactorAnalysis: return Shared::Dimensionality;
        case AnalysisWorkflowKind::ScaleAnalysis: return Shared::ScaleAnalysis;
        case AnalysisWorkflowKind::QuickCluster: return Shared::QuickCluster;
        case AnalysisWorkflowKind::OneSampleT: return Shared::OneSampleComparison;
        case AnalysisWorkflowKind::IndependentT: return Shared::IndependentSamplesComparison;
        case AnalysisWorkflowKind::PairedT: return Shared::PairedSamplesComparison;
        case AnalysisWorkflowKind::OneWayAnova: return Shared::OneWayComparison;
        case AnalysisWorkflowKind::LinearModel: return Shared::LinearModel;
        case AnalysisWorkflowKind::LinearModelTrellis: return Shared::LinearModelTrellis;
        case AnalysisWorkflowKind::RegressionComparison: return Shared::RegressionComparison;
        case AnalysisWorkflowKind::BinaryRegression:
        case AnalysisWorkflowKind::BinaryRegressionComparison: return Shared::BinaryRegression;
        case AnalysisWorkflowKind::GeneralizedLinearModel:
            return Shared::GeneralizedLinearModel;
        case AnalysisWorkflowKind::CountRegression:
        case AnalysisWorkflowKind::CountRegressionComparison:
            return Shared::CountRegression;
        case AnalysisWorkflowKind::PositiveContinuousModel:
        case AnalysisWorkflowKind::PositiveContinuousComparison:
            return Shared::PositiveContinuousModel;
        case AnalysisWorkflowKind::ProportionModel:
        case AnalysisWorkflowKind::ProportionComparison:
            return Shared::ProportionModel;
        case AnalysisWorkflowKind::GeneralizedComparison:
            return Shared::GeneralizedComparison;
        case AnalysisWorkflowKind::LinearMixedModel:
            return Shared::LinearMixedModel;
        case AnalysisWorkflowKind::GeneralizedMixedModel:
            return Shared::GeneralizedMixedModel;
        default: return std::nullopt;
        }
    }

    static ::rlispstat::core::SharedAnalysisKind SharedPlotInitializationKind(
        PlotWorkflowKind kind, std::string const& trellisType = "scatter")
    {
        using Shared = ::rlispstat::core::SharedAnalysisKind;
        if (kind == PlotWorkflowKind::Trellis)
        {
            if (trellisType == "time_series") return Shared::TimeSeries;
            if (trellisType == "boxplot") return Shared::Boxplot;
            if (trellisType == "bar") return Shared::BarChart;
            if (trellisType == "histogram") return Shared::Histogram;
            return Shared::TrellisPlot;
        }
        switch (kind)
        {
        case PlotWorkflowKind::TimeSeries: return Shared::TimeSeries;
        case PlotWorkflowKind::ScatterMatrix: return Shared::ScatterplotMatrix;
        case PlotWorkflowKind::ParallelCoordinates: return Shared::ParallelCoordinates;
        case PlotWorkflowKind::Boxplot: return Shared::Boxplot;
        case PlotWorkflowKind::Histogram: return Shared::Histogram;
        case PlotWorkflowKind::BarChart: return Shared::BarChart;
        default: return Shared::Scatterplot;
        }
    }

    /// <summary>
    /// Initializes the singleton application object.  This is the first line of authored code
    /// executed, and as such is the logical equivalent of main() or WinMain().
    /// </summary>
    App::App()
    {
        LogStartup("App constructor entered");
        // Xaml objects should not call InitializeComponent during construction.
        // See https://github.com/microsoft/cppwinrt/tree/master/nuget#initializecomponent

#if defined _DEBUG && !defined DISABLE_XAML_GENERATED_BREAK_ON_UNHANDLED_EXCEPTION
        UnhandledException([](IInspectable const&, UnhandledExceptionEventArgs const& e)
        {
            LogStartup("XAML unhandled exception: " + to_string(e.Message()));
            if (IsDebuggerPresent())
            {
                auto errorMessage = e.Message();
                __debugbreak();
            }
        });
#endif
    }

    /// <summary>
    /// Invoked when the application is launched.
    /// </summary>
    /// <param name="e">Details about the launch request and process.</param>
    void App::OnLaunched([[maybe_unused]] LaunchActivatedEventArgs const& e)
    {
        LogStartup("OnLaunched entered");
        try
        {
            LoadInterfaceFontPreference();
            LoadInterfaceSizePreference();
            // WinUI needs one initialized window to keep the dispatcher alive
            // between R commands.  This technical owner is excluded from the
            // switcher and hidden immediately; it has no user-facing content.
            technicalOwner_ = Window();
            technicalOwner_.AppWindow().IsShownInSwitchers(false);
            dispatcherQueue_ = technicalOwner_.DispatcherQueue();
            technicalOwner_.Activate();
            technicalOwner_.AppWindow().Hide();
            InitializeDispatcher();
            applicationCommands_ = std::make_shared<ApplicationCommandRegistry>(
                [this](std::vector<std::string> const& command)
                {
                    return DispatchApplicationCommand(command);
                },
                [this](std::string const& group)
                {
                    return commandDispatcher_ &&
                        commandDispatcher_->applicationState().datasets().find(group) != nullptr;
                },
                [this](std::string const& group)
                {
                    if (!commandDispatcher_) return false;
                    std::set<int> rows;
                    return commandDispatcher_->applicationState().selectedRows(group, rows) &&
                        !rows.empty();
                },
                [this]()
                {
                    return std::wstring(to_hstring(interfaceFont_).c_str());
                },
                [this]()
                {
                    return std::wstring(to_hstring(interfaceSize_).c_str());
                },
                [this]()
                {
                    return commandDispatcher_ ? commandDispatcher_->plotTheme() : "publication";
                },
                [this](std::string const& group)
                {
                    return commandDispatcher_
                        ? commandDispatcher_->applicationState().activeAnalysisScope(group)
                        : ::rlispstat::core::AnalysisScope{};
                },
                [this](std::string const& group)
                {
                    return commandDispatcher_
                        ? commandDispatcher_->applicationState().savedSelections(group)
                        : std::vector<::rlispstat::core::SavedSelection>{};
                },
                [this](std::string const& group)
                {
                    return commandDispatcher_
                        ? commandDispatcher_->applicationState().selectedColor(group)
                        : std::string{};
                });
            applicationCommands_->SetMissingDataCapabilities([this](std::string const& group) {
                const auto* data=commandDispatcher_?commandDispatcher_->applicationState().datasets().find(group):nullptr;
                return data?::rlispstat::core::MissingDataCapabilitiesFor(*data) : ::rlispstat::core::MissingDataCapabilities{};
            });
            SetDataSheetRecoveryCallback([this](std::string const& group)
            {
                ShowDataSheet(commandDispatcher_ ? commandDispatcher_->applicationState()
                    .dataSheetGroupForPlotGroup(group) : group);
            });
            SetSnapshotAlbumCallbacks(
                [this](std::string const& kind, std::string const& sourceId,
                       std::string const& group)
                {
                    AddActiveSnapshot(kind, sourceId, group);
                },
                [this]() { ShowSnapshotAlbum(); });
            LogStartup("Hidden technical owner created");
            StartTcpListener();
        }
        catch (hresult_error const& error)
        {
            LogStartup("OnLaunched hresult: " + std::to_string(static_cast<uint32_t>(error.code().value)) + " " + to_string(error.message()));
            MessageBoxW(nullptr, error.message().c_str(), L"LinkEDA startup error", MB_OK | MB_ICONERROR);
        }
    }

    fire_and_forget App::StartTcpListener()
    {
        try
        {
            listener_ = winrt::Windows::Networking::Sockets::StreamSocketListener();
            const auto dispatcher = dispatcherQueue_;
            listener_.ConnectionReceived([dispatcher, this](auto const&, auto const& args)
            {
                ReplyToClient(args.Socket(), dispatcher, this);
            });
            const auto port = ListenerPort();
            const winrt::Windows::Networking::HostName loopback(L"127.0.0.1");
            co_await listener_.BindEndpointAsync(loopback, port);
            LogStartup("TCP listener bound to 127.0.0.1:" + to_string(port));
        }
        catch (hresult_error const& error)
        {
            LogStartup("TCP listener error: "
                + std::to_string(static_cast<uint32_t>(error.code().value))
                + " " + to_string(error.message()));
        }
    }

    void App::LoadInterfaceFontPreference()
    {
        std::string fontName = "Inter";
        try
        {
            auto values = Windows::Storage::ApplicationData::Current()
                .LocalSettings().Values();
            if (auto stored = values.TryLookup(L"LinkEDA.InterfaceFont"))
            {
                const auto value = unbox_value_or<hstring>(stored, L"Inter");
                fontName = to_string(value);
            }
        }
        catch (hresult_error const& error)
        {
            LogStartup("Could not read interface font preference: " +
                to_string(error.message()));
        }
        if (!ApplyInterfaceFont(fontName, false))
            (void)ApplyInterfaceFont("Inter", false);
    }

    bool App::ApplyInterfaceFont(std::string const& fontName, bool persist)
    {
        static const std::map<std::string, std::string> supported{
            {"Inter", "ms-appx:///Assets/Fonts/Inter-Variable.ttf#Inter"},
            {"Source Sans 3", "ms-appx:///Assets/Fonts/SourceSans3-Variable.ttf#Source Sans 3"},
            {"IBM Plex Sans", "ms-appx:///Assets/Fonts/IBMPlexSans-Variable.ttf#IBM Plex Sans"},
            {"Aptos", "Aptos"},
            {"Calibri", "Calibri"},
            {"Segoe UI", "Segoe UI"},
            {"Tahoma", "Tahoma"}
        };
        auto supportedFont = supported.find(fontName);
        if (supportedFont == supported.end()) return false;

        try
        {
            auto family = Media::FontFamily(to_hstring(supportedFont->second));
            auto resources = Application::Current().Resources();
            resources.Insert(box_value(L"LinkEDAInterfaceFontFamily"), family);
            resources.Insert(box_value(L"ContentControlThemeFontFamily"), family);
            interfaceFont_ = fontName;
            if (persist)
            {
                Windows::Storage::ApplicationData::Current().LocalSettings()
                    .Values().Insert(L"LinkEDA.InterfaceFont",
                                     box_value(to_hstring(fontName)));
            }
            RefreshLinkEDAInterfaceFont(family);
            if (applicationCommands_) applicationCommands_->RefreshCommandState();
            LogStartup("Interface font set to " + fontName);
            return true;
        }
        catch (hresult_error const& error)
        {
            LogStartup("Could not apply interface font: " +
                to_string(error.message()));
            return false;
        }
    }

    void App::LoadInterfaceSizePreference()
    {
        std::string sizeName = "compact";
        try
        {
            auto values = Windows::Storage::ApplicationData::Current()
                .LocalSettings().Values();
            if (auto stored = values.TryLookup(L"LinkEDA.InterfaceSize"))
                sizeName = to_string(unbox_value_or<hstring>(stored, L"compact"));
        }
        catch (hresult_error const& error)
        {
            LogStartup("Could not read interface size preference: " +
                to_string(error.message()));
        }
        if (!ApplyInterfaceSize(sizeName, false))
            (void)ApplyInterfaceSize("compact", false);
    }

    bool App::ApplyInterfaceSize(std::string const& sizeName, bool persist)
    {
        const std::map<std::string, double> sizes{
            {"compact", 12.5}, {"standard", 14.0}, {"large", 16.0}
        };
        auto found = sizes.find(sizeName);
        if (found == sizes.end()) return false;

        try
        {
            auto resources = Application::Current().Resources();
            resources.Insert(box_value(L"LinkEDAInterfaceFontSize"),
                             box_value(found->second));
            resources.Insert(box_value(L"ControlContentThemeFontSize"),
                             box_value(found->second));
            interfaceSize_ = sizeName;
            if (persist)
            {
                Windows::Storage::ApplicationData::Current().LocalSettings()
                    .Values().Insert(L"LinkEDA.InterfaceSize",
                                     box_value(to_hstring(sizeName)));
            }
            if (applicationCommands_) applicationCommands_->RefreshCommandState();
            LogStartup("Interface size set to " + sizeName);
            return true;
        }
        catch (hresult_error const& error)
        {
            LogStartup("Could not apply interface size: " +
                to_string(error.message()));
            return false;
        }
    }

    void App::ShowWelcome(std::string const& version)
    {
        welcomeVersion_ = version.empty() ? "unknown" : version;
        if (!welcomeWindow_ || welcomeWindow_->Closed())
        {
            welcomeWindow_ = WelcomeWindow::Create();
            welcomeWindow_->SetClosedCallback([this]()
            {
                welcomeWindow_.reset();
            });
            welcomeWindow_->SetChooseRDataFrameCallback([this]()
            {
                if (!welcomeWindow_) return;
                welcomeWindow_->SetStatus("Reading data frames from R...");
                pendingMainRTasks_.dataBrowseTasks.push_back({
                    StartupId("welcome-r-", "data")
                });
                MarkMainRTaskPending();
            });
            welcomeWindow_->SetOpenFileCallback([this]()
            {
                ChooseWelcomeFile();
            });
            welcomeWindow_->SetOpenDocumentCallback([this]()
            {
                ChooseNativeDocumentFile();
            });
            welcomeWindow_->SetRecentFileCallback([this](std::string const& path)
            {
                if (!welcomeWindow_ || path.empty()) return;
                const std::filesystem::path file(to_hstring(path).c_str());
                if (file.extension() == L".linkeda") OpenNativeDocument(path);
                else ImportDataFileNative(path, path);
            });
            welcomeWindow_->SetExampleCallback([this](std::string const& example)
            {
                QueueWelcomeExample(example);
            });
            welcomeWindow_->SetDocumentationCallback([this]()
            {
                const auto& strings = ::rlispstat::core::DefaultWelcomeStrings();
                MessageBoxW(welcomeWindow_ ? welcomeWindow_->NativeHandle() : nullptr,
                    to_hstring(strings.documentationWorkInProgress).c_str(),
                    to_hstring(strings.documentation).c_str(), MB_OK | MB_ICONINFORMATION);
            });
            welcomeWindow_->SetAboutCallback([this]()
            {
                ShowAbout();
            });
        }
        std::vector<::rlispstat::core::WelcomeRecentItem> recent;
        recent.reserve(welcomeRecentPaths_.size());
        for (auto const& path : welcomeRecentPaths_)
        {
            const std::filesystem::path file(to_hstring(path).c_str());
            recent.push_back({ path, to_string(hstring(file.filename().wstring())), path,
                to_string(hstring(file.extension().wstring())), std::filesystem::exists(file) });
        }
        welcomeWindow_->Show(::rlispstat::core::DefaultWelcomeWindowModel(
            welcomeVersion_, true, std::move(recent)));
    }

    void App::HideWelcome()
    {
        if (welcomeWindow_) welcomeWindow_->Hide();
    }

    void App::ChooseWelcomeFile()
    {
        if (!welcomeWindow_) return;
        com_ptr<IFileOpenDialog> dialog;
        const HRESULT created = CoCreateInstance(CLSID_FileOpenDialog, nullptr,
            CLSCTX_INPROC_SERVER, IID_PPV_ARGS(dialog.put()));
        if (FAILED(created))
        {
            welcomeWindow_->SetStatus("Windows could not open the file chooser.", true);
            return;
        }
        const COMDLG_FILTERSPEC filters[] = {
            { L"All supported data files", L"*.linkeda;*.csv;*.tsv;*.txt;*.sav;*.zsav;*.dta;*.sas7bdat;*.xpt;*.xlsx;*.xls;*.rds;*.rda;*.RData" },
            { L"mice multiple-imputation files", L"*.rds;*.rda;*.RData" },
            { L"All files", L"*.*" }
        };
        dialog->SetFileTypes(static_cast<UINT>(std::size(filters)), filters);
        dialog->SetTitle(L"Open a data file");
        dialog->SetOkButtonLabel(L"Open");
        const HRESULT shown = dialog->Show(welcomeWindow_->NativeHandle());
        if (shown == HRESULT_FROM_WIN32(ERROR_CANCELLED)) return;
        if (FAILED(shown))
        {
            welcomeWindow_->SetStatus("The selected file could not be opened.", true);
            return;
        }
        com_ptr<IShellItem> item;
        if (FAILED(dialog->GetResult(item.put()))) return;
        PWSTR rawPath = nullptr;
        if (FAILED(item->GetDisplayName(SIGDN_FILESYSPATH, &rawPath)) || !rawPath) return;
        const std::string path = to_string(hstring(rawPath));
        CoTaskMemFree(rawPath);
        const std::filesystem::path selected(to_hstring(path).c_str());
        if (selected.extension() == L".linkeda") OpenNativeDocument(path);
        else ImportDataFileNative(path, path);
    }

    void App::ChooseNativeDocumentFile()
    {
        com_ptr<IFileOpenDialog> dialog;
        if (FAILED(CoCreateInstance(CLSID_FileOpenDialog, nullptr, CLSCTX_INPROC_SERVER,
                                    IID_PPV_ARGS(dialog.put())))) return;
        const COMDLG_FILTERSPEC filters[] = {
            { L"LinkEDA data documents", L"*.linkeda" }, { L"All files", L"*.*" }
        };
        dialog->SetFileTypes(static_cast<UINT>(std::size(filters)), filters);
        dialog->SetDefaultExtension(L"linkeda");
        dialog->SetTitle(L"Open Data"); dialog->SetOkButtonLabel(L"Open");
        HWND owner = welcomeWindow_ ? welcomeWindow_->NativeHandle() : GetActiveWindow();
        const HRESULT shown = dialog->Show(owner);
        if (shown == HRESULT_FROM_WIN32(ERROR_CANCELLED)) return;
        if (FAILED(shown)) { MessageBoxW(owner, L"Windows could not open the file chooser.",
                                        L"Open Data", MB_OK | MB_ICONERROR); return; }
        com_ptr<IShellItem> item; if (FAILED(dialog->GetResult(item.put()))) return;
        PWSTR rawPath = nullptr;
        if (FAILED(item->GetDisplayName(SIGDN_FILESYSPATH, &rawPath)) || !rawPath) return;
        const std::string path = to_string(hstring(rawPath)); CoTaskMemFree(rawPath);
        OpenNativeDocument(path);
    }

    void App::ChooseSaveDocumentFile(std::string const& group)
    {
        auto path = ChooseSaveDocumentPath(group);
        if (path) SaveNativeDocument(group, *path);
    }

    std::optional<std::string> App::ChooseSaveDocumentPath(std::string const& group)
    {
        if (group.empty()) return std::nullopt;
        com_ptr<IFileSaveDialog> dialog;
        if (FAILED(CoCreateInstance(CLSID_FileSaveDialog, nullptr, CLSCTX_INPROC_SERVER,
                                    IID_PPV_ARGS(dialog.put())))) return std::nullopt;
        const COMDLG_FILTERSPEC filters[] = {{ L"LinkEDA data documents", L"*.linkeda" }};
        dialog->SetFileTypes(1, filters); dialog->SetDefaultExtension(L"linkeda");
        dialog->SetFileName(to_hstring(group + ".linkeda").c_str());
        dialog->SetTitle(L"Save Data As"); dialog->SetOkButtonLabel(L"Save");
        const HRESULT shown = dialog->Show(GetActiveWindow());
        if (shown == HRESULT_FROM_WIN32(ERROR_CANCELLED)) return std::nullopt;
        if (FAILED(shown)) { MessageBoxW(GetActiveWindow(), L"Windows could not open the save chooser.",
                                        L"Save Data As", MB_OK | MB_ICONERROR); return std::nullopt; }
        com_ptr<IShellItem> item; if (FAILED(dialog->GetResult(item.put()))) return std::nullopt;
        PWSTR rawPath = nullptr;
        if (FAILED(item->GetDisplayName(SIGDN_FILESYSPATH, &rawPath)) || !rawPath)
            return std::nullopt;
        std::filesystem::path selected(rawPath); CoTaskMemFree(rawPath);
        if (selected.extension() != L".linkeda") selected += L".linkeda";
        return to_string(hstring(selected.wstring()));
    }

    void App::ChooseExportDataFile(std::string const& group)
    {
        if (!commandDispatcher_ || group.empty()) return;
        auto const* registered = commandDispatcher_->applicationState().datasets().find(group);
        if (!registered)
        {
            MessageBoxW(GetActiveWindow(), L"No active dataset is available to export.",
                        L"Export Data", MB_OK | MB_ICONINFORMATION);
            return;
        }
        const ::rlispstat::core::DataFrameModel dataframe = *registered;

        com_ptr<IFileSaveDialog> dialog;
        if (FAILED(CoCreateInstance(CLSID_FileSaveDialog, nullptr, CLSCTX_INPROC_SERVER,
                                    IID_PPV_ARGS(dialog.put())))) return;
        const auto exportFilters = ::rlispstat::core::NativeDataExportFileFilters();
        std::vector<std::wstring> filterTitles;
        std::vector<std::wstring> filterPatterns;
        filterTitles.reserve(exportFilters.size());
        filterPatterns.reserve(exportFilters.size());
        for (const auto& filter : exportFilters)
        {
            filterTitles.push_back(to_hstring(filter.title).c_str());
            filterPatterns.push_back(L"*." + std::wstring(to_hstring(filter.extension).c_str()));
        }
        std::vector<COMDLG_FILTERSPEC> dialogFilters;
        dialogFilters.reserve(exportFilters.size());
        for (std::size_t index = 0; index < exportFilters.size(); ++index)
            dialogFilters.push_back({ filterTitles[index].c_str(), filterPatterns[index].c_str() });
        dialog->SetFileTypes(static_cast<UINT>(dialogFilters.size()), dialogFilters.data());
        const std::string baseName = ::rlispstat::core::SafeExportBaseName(
            group, "LinkEDA-data");
        dialog->SetFileName(to_hstring(baseName).c_str());
        dialog->SetTitle(L"Export Data");
        dialog->SetOkButtonLabel(L"Export");
        const HRESULT shown = dialog->Show(GetActiveWindow());
        if (shown == HRESULT_FROM_WIN32(ERROR_CANCELLED)) return;
        if (FAILED(shown))
        {
            MessageBoxW(GetActiveWindow(), L"Windows could not open the export chooser.",
                        L"Export Data", MB_OK | MB_ICONERROR);
            return;
        }
        com_ptr<IShellItem> item;
        if (FAILED(dialog->GetResult(item.put()))) return;
        PWSTR rawPath = nullptr;
        if (FAILED(item->GetDisplayName(SIGDN_FILESYSPATH, &rawPath)) || !rawPath) return;
        std::filesystem::path selected(rawPath);
        CoTaskMemFree(rawPath);
        UINT selectedFilter = 1;
        (void)dialog->GetFileTypeIndex(&selectedFilter);
        const std::size_t formatIndex = selectedFilter > 0 &&
            selectedFilter <= exportFilters.size()
            ? static_cast<std::size_t>(selectedFilter - 1) : 0;
        const std::string format = exportFilters[formatIndex].identifier;
        const std::wstring expectedExtension = L"." +
            std::wstring(to_hstring(exportFilters[formatIndex].extension).c_str());
        std::wstring actualExtension = selected.extension().wstring();
        std::transform(actualExtension.begin(), actualExtension.end(), actualExtension.begin(),
            [](wchar_t value) { return static_cast<wchar_t>(std::towlower(value)); });
        std::wstring normalizedExpected = expectedExtension;
        std::transform(normalizedExpected.begin(), normalizedExpected.end(), normalizedExpected.begin(),
            [](wchar_t value) { return static_cast<wchar_t>(std::towlower(value)); });
        if (actualExtension != normalizedExpected) selected.replace_extension(expectedExtension);

        bool exported = false;
        std::string failure;
        if (format == "csv")
        {
            std::ofstream out(selected, std::ios::out | std::ios::trunc | std::ios::binary);
            exported = out && ::rlispstat::core::WriteDataExportCSV(out, dataframe);
            out.flush();
            exported = exported && static_cast<bool>(out);
            out.close();
            exported = exported && static_cast<bool>(out);
            if (!exported) failure = "The CSV file could not be generated or written.";
        }
        else
        {
            const auto unique = std::to_wstring(GetCurrentProcessId()) + L"-" +
                std::to_wstring(GetTickCount64());
            const auto inputPath = std::filesystem::temp_directory_path() /
                (L"linkeda-export-" + unique + L".csv");
            const auto metadataPath = std::filesystem::temp_directory_path() /
                (L"linkeda-export-" + unique + L"-metadata.csv");
            {
                std::ofstream input(inputPath, std::ios::out | std::ios::trunc | std::ios::binary);
                exported = input && ::rlispstat::core::WriteDataExportCSV(input, dataframe);
                input.flush();
                exported = exported && static_cast<bool>(input);
            }
            if (exported)
            {
                std::ofstream metadata(metadataPath,
                    std::ios::out | std::ios::trunc | std::ios::binary);
                exported = metadata &&
                    ::rlispstat::core::WriteDataExportMetadataCSV(metadata, dataframe);
                metadata.flush();
                exported = exported && static_cast<bool>(metadata);
            }
            if (!exported)
            {
                failure = ::rlispstat::core::NativeDataExportTemporaryFileFailedStatus();
            }
            else
            {
                exported = RunRScriptText(
                    ::rlispstat::core::NativeDataExportRScript(),
                    { inputPath.wstring(), metadataPath.wstring(), selected.wstring(),
                      std::wstring(to_hstring(format).c_str()) }, failure);
            }
            std::error_code ignored;
            std::filesystem::remove(inputPath, ignored);
            std::filesystem::remove(metadataPath, ignored);
        }
        if (!exported)
        {
            if (failure.empty()) failure = ::rlispstat::core::NativeDataExportRscriptFailedStatus();
            MessageBoxW(GetActiveWindow(), to_hstring(failure).c_str(),
                        L"Export Data Failed", MB_OK | MB_ICONERROR);
            return;
        }
        const bool multipleImputation = dataframe.datasetType == "multiple_imputation" &&
            dataframe.imputationCount > 0;
        MessageBoxW(GetActiveWindow(),
            to_hstring(::rlispstat::core::NativeDataExportSuccessStatus(
                format, multipleImputation)).c_str(),
            L"Data Exported", MB_OK | MB_ICONINFORMATION);
    }

    bool App::BuildNativeDocumentPayload(
        std::string const& group, std::vector<unsigned char>& payload,
        std::string& error) const
    {
        if (!commandDispatcher_ || group.empty())
        {
            error = "The LinkEDA data service is not available.";
            return false;
        }
        ::rlispstat::core::LinkEDADataDocument document;
        if (!::rlispstat::core::CreateLinkEDADataDocument(
                commandDispatcher_->applicationState(), group, document, &error))
            return false;
        payload = ::rlispstat::core::EncodeLinkEDADataDocument(document, &error);
        return !payload.empty();
    }

    bool App::SaveNativeDocumentBlocking(
        std::string const& group, std::string const& path)
    {
        std::vector<unsigned char> payload;
        std::string error;
        if (!BuildNativeDocumentPayload(group, payload, error))
        {
            MessageBoxW(GetActiveWindow(), to_hstring(error).c_str(),
                        L"Save Data", MB_OK | MB_ICONERROR);
            return false;
        }
        auto result = WriteNativeDocument(path, payload);
        if (!result.ok)
        {
            MessageBoxW(GetActiveWindow(), to_hstring(result.message).c_str(),
                        L"Save Data", MB_OK | MB_ICONERROR);
            return false;
        }
        documentPaths_[group] = path;
        documentBaselines_[group] = std::move(payload);
        NoteWelcomeRecentPath(path);
        return true;
    }

    fire_and_forget App::ImportDataFileNative(std::string path,
                                               std::string sourcePath)
    {
        auto lifetime = get_strong();
        try
        {
            if (welcomeWindow_) welcomeWindow_->SetStatus("Importing data...");
            // Match the macOS import architecture whenever LinkEDA was launched
            // from R.  The canonical R importer applies janitor name cleaning and
            // preserves mice::mids objects (including every completed imputation
            // and the metadata required for pooled analyses).  The standalone
            // native importer remains available when no main R session exists.
            if (mainRSessionAvailable_.load(std::memory_order_acquire))
            {
                pendingMainRTasks_.importDataTasks.push_back({
                    std::move(path), std::move(sourcePath), false
                });
                MarkMainRTaskPending();
                co_return;
            }
            if (!commandDispatcher_)
            {
                if (welcomeWindow_)
                    welcomeWindow_->SetStatus("The LinkEDA data service is not available.", true);
                co_return;
            }

            const auto group = commandDispatcher_->applicationState().datasets()
                .uniqueDatasetName(::rlispstat::core::SafeDatasetNameForPath(sourcePath));
            const auto dispatcher = dispatcherQueue_;
            co_await winrt::resume_background();
            auto result = RunNativeImport(path, group);
            co_await wil::resume_foreground(dispatcher,
                winrt::Microsoft::UI::Dispatching::DispatcherQueuePriority::Normal);

            if (!result.ok)
            {
                if (welcomeWindow_)
                    welcomeWindow_->SetStatus(result.message.empty()
                        ? "The data file could not be imported." : result.message, true);
                co_return;
            }

            ::rlispstat::core::DataFrameModel dataframe;
            std::string parseError;
            if (!ParseNativeImportDataFrame(result.payload, dataframe, parseError))
            {
                if (welcomeWindow_) welcomeWindow_->SetStatus(parseError, true);
                co_return;
            }
            ShowNativeImportColumnChooser(std::move(dataframe), std::move(sourcePath));
        }
        catch (std::exception const& error)
        {
            if (welcomeWindow_) welcomeWindow_->SetStatus(error.what(), true);
        }
        catch (...)
        {
            if (welcomeWindow_)
                welcomeWindow_->SetStatus("The data file could not be imported.", true);
        }
    }

    void App::ShowImportColumnChooser(std::string stagedDataset,
                                      std::string information,
                                      std::vector<std::string> variables)
    {
        if (stagedDataset.empty() || variables.empty()) return;
        if (importColumnsDialog_) importColumnsDialog_->Close();

        Window owner{ nullptr };
        if (welcomeWindow_ && !welcomeWindow_->Closed())
            owner = welcomeWindow_->NativeWindow();
        if (!owner && commandDispatcher_)
        {
            const auto active = commandDispatcher_->applicationState().datasets()
                .activeDatasetGroup();
            auto found = dataSheets_.find(active);
            if (found != dataSheets_.end() && found->second)
                owner = found->second->NativeWindow();
        }
        if (!owner)
            for (auto const& [group, sheet] : dataSheets_)
                if (sheet) { owner = sheet->NativeWindow(); break; }
        if (!owner)
        {
            ShowWelcome(welcomeVersion_);
            if (welcomeWindow_) owner = welcomeWindow_->NativeWindow();
        }
        if (!owner) return;

        ::rlispstat::core::DataFrameModel choices;
        choices.group = stagedDataset;
        choices.columns.reserve(variables.size());
        for (auto const& variable : variables)
        {
            ::rlispstat::core::DataColumn column;
            column.name = variable;
            choices.columns.push_back(std::move(column));
        }
        auto submitted = std::make_shared<bool>(false);
        importColumnsDialog_ = DatasetVariableDialog::Create(
            owner, choices, L"Choose variables to import",
            to_hstring(information + " All variables are selected initially; use Select none to start with a reduced set.").c_str(),
            L"Import selected", true, {},
            [this, stagedDataset, submitted](std::vector<std::string> const& selected)
            {
                if (selected.empty()) return;
                *submitted = true;
                ::rlispstat::core::MainRImportDataTask task;
                task.stagedDataset = stagedDataset;
                task.selectedVariables = selected;
                pendingMainRTasks_.importDataTasks.push_back(std::move(task));
                if (welcomeWindow_) welcomeWindow_->SetStatus("Importing selected variables...");
                MarkMainRTaskPending();
            },
            [this, stagedDataset, submitted]()
            {
                importColumnsDialog_.reset();
                if (*submitted) return;
                ::rlispstat::core::MainRImportDataTask task;
                task.stagedDataset = stagedDataset;
                task.cancelStagedImport = true;
                pendingMainRTasks_.importDataTasks.push_back(std::move(task));
                MarkMainRTaskPending();
            });
        importColumnsDialog_->Show();
    }

    void App::ShowNativeImportColumnChooser(::rlispstat::core::DataFrameModel dataframe,
                                            std::string sourcePath)
    {
        if (dataframe.columns.empty() || !commandDispatcher_) return;
        if (importColumnsDialog_) importColumnsDialog_->Close();
        Window owner{ nullptr };
        if (welcomeWindow_ && !welcomeWindow_->Closed())
            owner = welcomeWindow_->NativeWindow();
        if (!owner)
        {
            const auto active = commandDispatcher_->applicationState().datasets()
                .activeDatasetGroup();
            auto found = dataSheets_.find(active);
            if (found != dataSheets_.end() && found->second)
                owner = found->second->NativeWindow();
        }
        if (!owner)
            for (auto const& [group, sheet] : dataSheets_)
                if (sheet) { owner = sheet->NativeWindow(); break; }
        if (!owner)
        {
            ShowWelcome(welcomeVersion_);
            if (welcomeWindow_) owner = welcomeWindow_->NativeWindow();
        }
        if (!owner) return;

        ::rlispstat::core::DataFrameModel choices;
        choices.group = dataframe.group;
        choices.columns.reserve(dataframe.columns.size());
        for (auto const& source : dataframe.columns)
        {
            ::rlispstat::core::DataColumn column; column.name = source.name;
            choices.columns.push_back(std::move(column));
        }
        auto submitted = std::make_shared<bool>(false);
        const auto information = "Choose the variables to import from `" + sourcePath +
            "`. All variables are selected initially; use Select none to start with a reduced set.";
        importColumnsDialog_ = DatasetVariableDialog::Create(
            owner, choices, L"Choose variables to import", to_hstring(information).c_str(),
            L"Import selected", true, {},
            [this, dataframe = std::move(dataframe), sourcePath,
             submitted](std::vector<std::string> const& selected) mutable
            {
                if (selected.empty() || !commandDispatcher_) return;
                *submitted = true;
                const std::set<std::string> included(selected.begin(), selected.end());
                dataframe.columns.erase(std::remove_if(dataframe.columns.begin(),
                    dataframe.columns.end(), [&](auto const& column)
                    { return included.find(column.name) == included.end(); }),
                    dataframe.columns.end());
                if (!commandDispatcher_->applicationState().registerDataset(dataframe))
                {
                    if (welcomeWindow_)
                        welcomeWindow_->SetStatus("The selected variables could not be imported.", true);
                    return;
                }
                commandDispatcher_->applicationState().datasets().setActiveDataset(dataframe.group);
                NoteWelcomeRecentPath(sourcePath);
                std::string importMessage =
                    ::rlispstat::core::NativeImportDatasetLoadedStatus(
                        dataframe.group, dataframe.rows, dataframe.columns.size());
                const std::string review =
                    ::rlispstat::core::ImportedVariableTypeReviewWarning(dataframe);
                if (!review.empty()) importMessage += "\n\n" + review;
                if (welcomeWindow_) welcomeWindow_->SetStatus(importMessage, !review.empty());
                HideWelcome();
                ShowDataSheet(dataframe.group);
                if (!review.empty())
                    MessageBoxW(nullptr, to_hstring(importMessage).c_str(), L"Import Data",
                        MB_OK | MB_ICONWARNING);
            },
            [this, submitted]()
            {
                importColumnsDialog_.reset();
                if (!*submitted && welcomeWindow_)
                    welcomeWindow_->SetStatus("Import cancelled.");
            });
        importColumnsDialog_->Show();
    }

    fire_and_forget App::OpenNativeDocument(std::string path)
    {
        auto lifetime = get_strong();
        if (welcomeWindow_) welcomeWindow_->SetStatus("Opening LinkEDA data document...");
        const auto dispatcher = dispatcherQueue_;
        co_await winrt::resume_background();
        auto result = ReadNativeDocument(path);
        ::rlispstat::core::LinkEDADataDocument document;
        std::string error;
        if (result.ok)
            result.ok = ::rlispstat::core::DecodeLinkEDADataDocument(result.payload, document, &error);
        if (!result.ok && result.message.empty()) result.message = error;
        co_await wil::resume_foreground(dispatcher,
            winrt::Microsoft::UI::Dispatching::DispatcherQueuePriority::Normal);
        if (!result.ok)
        {
            const std::string message = result.message.empty()
                ? "The LinkEDA data document could not be opened." : result.message;
            if (welcomeWindow_) welcomeWindow_->SetStatus(message, true);
            else MessageBoxW(GetActiveWindow(), to_hstring(message).c_str(),
                             L"Open Data", MB_OK | MB_ICONERROR);
            co_return;
        }
        auto& state = commandDispatcher_->applicationState();
        if (state.datasets().contains(document.dataset.group))
        {
            const std::string group = state.datasets().uniqueDatasetName(document.dataset.group);
            document.dataset.group = group; document.analysis_scope.datasetId = group;
            for (auto& selection : document.saved_selections) selection.datasetId = group;
        }
        if (!::rlispstat::core::ApplyLinkEDADataDocument(document, state, &error))
        {
            if (welcomeWindow_) welcomeWindow_->SetStatus(error, true);
            else MessageBoxW(GetActiveWindow(), to_hstring(error).c_str(),
                             L"Open Data", MB_OK | MB_ICONERROR);
            co_return;
        }
        const std::string group = document.dataset.group;
        documentPaths_[group] = path;
        std::vector<unsigned char> baseline;
        if (BuildNativeDocumentPayload(group, baseline, error))
            documentBaselines_[group] = std::move(baseline);
        NoteWelcomeRecentPath(path);
        ShowDataSheet(group); HideWelcome();
    }

    fire_and_forget App::SaveNativeDocument(std::string group, std::string path)
    {
        auto lifetime = get_strong();
        if (!commandDispatcher_ || group.empty()) co_return;
        std::vector<unsigned char> payload;
        std::string error;
        if (!BuildNativeDocumentPayload(group, payload, error))
        {
            MessageBoxW(GetActiveWindow(), to_hstring(error).c_str(),
                        L"Save Data", MB_OK | MB_ICONERROR); co_return;
        }
        const auto dispatcher = dispatcherQueue_;
        co_await winrt::resume_background();
        auto result = WriteNativeDocument(path, payload);
        co_await wil::resume_foreground(dispatcher,
            winrt::Microsoft::UI::Dispatching::DispatcherQueuePriority::Normal);
        if (!result.ok)
        {
            MessageBoxW(GetActiveWindow(), to_hstring(result.message).c_str(),
                        L"Save Data", MB_OK | MB_ICONERROR); co_return;
        }
        documentPaths_[group] = path;
        documentBaselines_[group] = std::move(payload);
        NoteWelcomeRecentPath(path);
    }

    void App::ShowAbout()
    {
        const auto& credits = ::rlispstat::core::DefaultApplicationCredits();
        const auto lines = ::rlispstat::core::AboutCreditLines(credits);
        auto text = lines[0] + "\n" + lines[1] + "\n" + lines[2] +
            "\n\nLinkEDA version " + welcomeVersion_;
        try
        {
            const auto version = Windows::ApplicationModel::Package::Current().Id().Version();
            text += "\nWindows package " + std::to_string(version.Major) + "." +
                std::to_string(version.Minor) + "." +
                std::to_string(version.Build) + "." +
                std::to_string(version.Revision);
        }
        catch (...)
        {
            // Unpackaged developer launches do not have package identity.
        }
        MessageBoxW(GetActiveWindow(), to_hstring(text).c_str(), L"About LinkEDA",
                    MB_OK | MB_ICONINFORMATION);
    }

    void App::QuitApplication()
    {
        if (quitting_ || !commandDispatcher_) return;
        quitting_ = true;

        for (auto const& [group, dataframe] :
             commandDispatcher_->applicationState().datasets().datasets())
        {
            std::vector<unsigned char> current;
            std::string error;
            if (!BuildNativeDocumentPayload(group, current, error))
            {
                MessageBoxW(GetActiveWindow(), to_hstring(error).c_str(),
                            L"Quit LinkEDA", MB_OK | MB_ICONERROR);
                quitting_ = false;
                return;
            }
            const auto path = documentPaths_.find(group);
            const auto baseline = documentBaselines_.find(group);
            const bool neverSaved = path == documentPaths_.end();
            const bool changed = baseline == documentBaselines_.end() ||
                baseline->second != current;
            if (!neverSaved && !changed) continue;

            const std::string detail = neverSaved
                ? "The dataset `" + group + "` has not been saved to disk."
                : "The dataset `" + group + "` has changes that have not been saved.";
            const auto message = detail + "\n\nSave it before quitting LinkEDA?";
            const int choice = MessageBoxW(GetActiveWindow(),
                to_hstring(message).c_str(), L"Quit LinkEDA",
                MB_YESNOCANCEL | MB_ICONWARNING);
            if (choice == IDCANCEL || choice == 0)
            {
                quitting_ = false;
                return;
            }
            if (choice == IDYES)
            {
                std::optional<std::string> savePath;
                if (path != documentPaths_.end()) savePath = path->second;
                else savePath = ChooseSaveDocumentPath(group);
                if (!savePath || !SaveNativeDocumentBlocking(group, *savePath))
                {
                    quitting_ = false;
                    return;
                }
            }
        }

        try { if (listener_) listener_.Close(); } catch (...) {}
        listener_ = nullptr;
        ResetAllViews();
        if (paletteWindow_) { auto window = paletteWindow_; paletteWindow_ = nullptr; window.Close(); }
        if (snapshotAlbumWindow_)
        {
            auto window = snapshotAlbumWindow_; snapshotAlbumWindow_ = nullptr; window.Close();
        }
        if (welcomeWindow_ && !welcomeWindow_->Closed()) welcomeWindow_->NativeWindow().Close();
        welcomeWindow_.reset();
        if (technicalOwner_)
        {
            auto window = technicalOwner_; technicalOwner_ = nullptr; window.Close();
        }
        PostQuitMessage(0);
    }

    void App::NoteWelcomeRecentPath(std::string const& path)
    {
        if (path.empty()) return;
        welcomeRecentPaths_.erase(std::remove(welcomeRecentPaths_.begin(),
            welcomeRecentPaths_.end(), path), welcomeRecentPaths_.end());
        welcomeRecentPaths_.insert(welcomeRecentPaths_.begin(), path);
        if (welcomeRecentPaths_.size() > 8) welcomeRecentPaths_.resize(8);
    }

    void App::QueueWelcomeExample(std::string const& example)
    {
        if (!welcomeWindow_) return;
        if (example.empty())
        {
            std::vector<std::pair<std::string, std::string>> items;
            for (auto const& item : ::rlispstat::core::DefaultWelcomeExampleItems())
                items.push_back({ item.id, item.displayName + "  —  " + item.category +
                    "\n" + item.description });
            exampleChooser_ = RDataFrameChooserWindow::Create(
                welcomeWindow_->NativeWindow(), "welcome-examples", std::move(items),
                "Statistical examples", "Choose a statistical example",
                "Each public dataset is prepared to exercise specific LinkEDA analyses and plots.",
                false);
            exampleChooser_->SetCompletedCallback(
                [this](std::string const&, std::string const& selected, bool cancelled)
                {
                    if (!cancelled && !selected.empty()) QueueWelcomeExample(selected);
                });
            exampleChooser_->Show();
            return;
        }
        if (!::rlispstat::core::FindWelcomeExample(example)) return;
        welcomeWindow_->SetStatus("Opening " + example + "...");
        pendingMainRTasks_.welcomeActionTasks.push_back({
            StartupId("welcome-example-", example), "example", example
        });
        MarkMainRTaskPending();
    }

    std::string App::ShowRDataFrameChooser(std::vector<std::string> const& command)
    {
        if (command.size() < 4) return "ERR malformed R data chooser request";
        const auto count = static_cast<std::size_t>(std::max(0, std::atoi(command[3].c_str())));
        const auto available = std::min(count, (command.size() - 4) / 4);
        std::vector<std::pair<std::string, std::string>> items;
        items.reserve(available);
        for (std::size_t index = 0; index < available; ++index)
        {
            const auto cursor = 4 + index * 4;
            items.push_back({ command[cursor], command[cursor + 1] +
                " rows x " + command[cursor + 2] + " columns" });
        }
        if (items.empty()) return "ERR no R data frames are available";
        Window owner = technicalOwner_;
        if (welcomeWindow_ && !welcomeWindow_->Closed())
            owner = welcomeWindow_->NativeWindow();
        else if (!dataSheets_.empty() && dataSheets_.begin()->second)
            owner = dataSheets_.begin()->second->NativeWindow();
        rDataChooser_ = RDataFrameChooserWindow::Create(
            owner, command[1], std::move(items));
        rDataChooser_->SetCompletedCallback([this](std::string const& requestId,
                                                   std::string const& objectName,
                                                   bool cancelled)
        {
            pendingMainRTasks_.dataChoiceTasks.push_back({
                requestId, objectName, cancelled
            });
            MarkMainRTaskPending();
            if (welcomeWindow_)
                welcomeWindow_->SetStatus(cancelled ? "Connected to R" : "Opening data...");
        });
        rDataChooser_->Show();
        return "OK";
    }

    void App::InitializeDispatcher()
    {
        ::rlispstat::core::CommandDispatcherServices services;
        services.queries.listPlots = [this]()
        {
            std::vector<::rlispstat::core::PlotModel> result;
            if (!commandDispatcher_) return result;
            for (auto const& entry : commandDispatcher_->applicationState().plots())
            {
                if (entry.second) result.push_back(*entry.second);
            }
            return result;
        };
        services.queries.plot = [this](std::string const& plotId,
                                       ::rlispstat::core::PlotModel& plot)
        {
            if (!commandDispatcher_) return false;
            auto const& plots = commandDispatcher_->applicationState().plots();
            auto found = plots.find(plotId);
            if (found == plots.end() || !found->second) return false;
            plot = *found->second;
            return true;
        };
        services.queries.plotWithSelection = [this](std::string const& plotId,
                                                     ::rlispstat::core::PlotModel& plot,
                                                     std::size_t& selectedCount)
        {
            if (!commandDispatcher_) return false;
            auto const& state = commandDispatcher_->applicationState();
            auto found = state.plots().find(plotId);
            if (found == state.plots().end() || !found->second) return false;
            plot = *found->second;
            std::set<int> selected;
            state.selectedRows(plot.group, selected);
            selectedCount = selected.size();
            return true;
        };
        services.queries.groups = [this]()
        {
            std::map<std::string, ::rlispstat::core::GroupSelectionSummary> summaries;
            if (!commandDispatcher_) {
                return std::vector<::rlispstat::core::GroupSelectionSummary>{};
            }
            auto const& state = commandDispatcher_->applicationState();
            for (auto const& entry : state.plots())
            {
                if (!entry.second) continue;
                auto& summary = summaries[entry.second->group];
                summary.group = entry.second->group;
                ++summary.plotCount;
            }
            for (auto& entry : summaries)
            {
                std::set<int> selected;
                state.selectedRows(entry.first, selected);
                entry.second.selectedCount = selected.size();
            }
            std::vector<::rlispstat::core::GroupSelectionSummary> result;
            result.reserve(summaries.size());
            for (auto const& entry : summaries) result.push_back(entry.second);
            return result;
        };
        services.queries.groupInfo = [this](std::string const& group,
                                             std::size_t& selectedCount,
                                             std::vector<std::string>& plotIds)
        {
            if (!commandDispatcher_) return false;
            plotIds = commandDispatcher_->plotCoordinator().plotIdsForGroup(group);
            std::set<int> selected;
            if (!commandDispatcher_->applicationState().selectedRows(group, selected)) {
                return false;
            }
            selectedCount = selected.size();
            return !plotIds.empty();
        };
        services.queries.pointColors = [this](std::string const& group,
                                               std::vector<std::pair<int, std::string>>& colors)
        {
            if (!commandDispatcher_ ||
                !commandDispatcher_->applicationState().hasSelectionGroup(group)) {
                return false;
            }
            colors = commandDispatcher_->applicationState().displayPointColors(group);
            return true;
        };
        services.queries.activePlot = [this](::rlispstat::core::PlotModel& plot)
        {
            if (!commandDispatcher_) return false;
            auto const& state = commandDispatcher_->applicationState();
            auto found = state.plots().find(state.activePlotId());
            if (found == state.plots().end() || !found->second) return false;
            plot = *found->second;
            return true;
        };
        auto groupSeed = [this](std::string const& group,
                                ::rlispstat::core::PlotModel& plot)
        {
            if (!commandDispatcher_) return false;
            auto const& state = commandDispatcher_->applicationState();
            // Analysis models need every registered variable, not merely the
            // x/y payload carried by whichever plot happened to open first.
            if (auto const* dataframe = state.datasets().find(group))
            {
                ::rlispstat::core::PopulateDatasetSeedPlot(plot, *dataframe, group);
                return true;
            }
            for (auto const& entry : state.plots())
            {
                if (!entry.second || entry.second->group != group) continue;
                plot = *entry.second;
                return true;
            }
            return false;
        };
        services.queries.groupPlot = groupSeed;
        services.queries.groupSeed = std::move(groupSeed);
        services.queries.createGeneralizedGLMId = [](std::string const& group)
        {
            return "generalized_windows_" + group + "_" +
                std::to_string(static_cast<unsigned long long>(GetTickCount64()));
        };
        services.ui.openLinearInteractionPlot = [this](std::string const& group,
                                                        std::string const& term)
        {
            if (!commandDispatcher_ || group.empty() || term.empty()) return std::string{};
            auto& state = commandDispatcher_->applicationState();
            auto modelState = state.groupModels().find(group);
            auto const* dataframe = state.datasets().find(group);
            if (modelState == state.groupModels().end() || !dataframe) return std::string{};

            auto const& specification = modelState->second;
            const std::string analysisId = "glm_interaction:" + group + ":" + term;
            const std::string modelSpec =
                ::rlispstat::core::EncodeStandaloneLinearMISpec(
                    ::rlispstat::core::EffectiveModelSpecificationTermTypes(specification),
                    specification.centeredPredictors,
                    specification.factorReferenceLevels,
                    NativePostEstimationRows(specification.dataScope,
                                              specification.dataScopeCaptured));
            auto post = RunRRegressionPostEstimation(
                *dataframe, "glm_interaction_plot", analysisId, term,
                specification.response, specification.terms,
                specification.scope.empty() ? "all" : specification.scope,
                modelSpec);
            if (!post.ok) return std::string{};
            std::string message;
            if (!OpenPooledRegressionInteractionPlot(
                    group, analysisId, specification.response, term,
                    post.lines, message)) return std::string{};
            for (auto const& [plotId, plot] : plots_)
                if (plot && plot->glmModelId == analysisId) return plotId;
            return std::string{};
        };
        services.selection.visibleRows = [this](std::string const& group,
                                                 std::set<int>& rows)
        {
            if (!commandDispatcher_) return false;
            auto const& state = commandDispatcher_->applicationState();
            if (auto const* dataset = state.datasets().find(group)) {
                for (int row = 1; row <= dataset->rows; ++row) rows.insert(row);
                return true;
            }
            bool foundGroup = false;
            for (auto const& entry : state.plots())
            {
                if (!entry.second || entry.second->group != group) continue;
                foundGroup = true;
                for (auto const& point : entry.second->points)
                {
                    if (point.row > 0) rows.insert(point.row);
                }
            }
            if (!foundGroup)
            {
                auto const* dataset = state.datasets().find(group);
                if (!dataset) return false;
                foundGroup = true;
                for (int row = 1; row <= dataset->rows; ++row) rows.insert(row);
            }
            return foundGroup;
        };
        services.selection.selectionChanged = [this](std::string const& group,
                                                       std::set<int> const&,
                                                       int,
                                                       bool)
        {
            if (!commandDispatcher_) return;
            const auto snapshot = commandDispatcher_->plotCoordinator().selectionSnapshot(group);
            if (snapshot.accepted) RefreshSelectionVisuals(snapshot.event);
            for (auto const& [plotId, model] : plots_)
            {
                if (!model || model->group != group) continue;
                if (model->kind == "scatter_matrix" &&
                    ::rlispstat::core::HasOverlaySource(*model, "selected"))
                {
                    ::rlispstat::core::InvalidateScatterMatrixFits(*model);
                    QueueSmoothRecompute(*model,
                        ::rlispstat::core::SmoothCurveScope::Selection, "lm");
                    continue;
                }
                if (::rlispstat::core::MarkSmoothCurveScopePendingIfPresent(
                        model->smoothCurves,
                        ::rlispstat::core::SmoothCurveScope::Selection))
                    QueueSmoothRecompute(*model,
                        ::rlispstat::core::SmoothCurveScope::Selection);
            }
        };
        services.selection.rowColorsChanged = [this](std::string const& group,
                                                      std::vector<int> const& changedRows,
                                                      bool,
                                                      bool)
        {
            if (!commandDispatcher_) return;
            ::rlispstat::windows::performance::Scope timing(
                "App.RefreshRowColors",
                "group=" + group + ",changed=" + std::to_string(changedRows.size()));
            const auto colors = commandDispatcher_->applicationState().displayPointColors(group);
            for (auto const& [plotId, model] : plots_)
            {
                if (!model || model->group != group) continue;
                auto view = views_.find(plotId);
                if (view != views_.end() && view->second)
                    view->second->SetVisualStyle(commandDispatcher_->plotTheme(), colors);
                if (model->kind == "scatter_matrix" &&
                    ::rlispstat::core::HasOverlaySource(*model, "color"))
                {
                    ::rlispstat::core::InvalidateScatterMatrixFits(*model);
                    QueueSmoothRecompute(*model,
                        ::rlispstat::core::SmoothCurveScope::ColorGroup, "lm");
                    continue;
                }
                if (::rlispstat::core::MarkSmoothCurveScopePendingIfPresent(
                        model->smoothCurves,
                        ::rlispstat::core::SmoothCurveScope::ColorGroup))
                    QueueSmoothRecompute(*model,
                        ::rlispstat::core::SmoothCurveScope::ColorGroup);
            }
            auto sheet = dataSheets_.find(group);
            if (sheet != dataSheets_.end() && sheet->second)
                sheet->second->RefreshRowColors(colors);
            for (auto const& [id, view] : dendrogramViews_)
            {
                auto state = commandDispatcher_->applicationState().dendrograms().find(id);
                if (view && state != commandDispatcher_->applicationState().dendrograms().end() &&
                    state->second.group == group) view->RefreshRowColors(colors);
            }
            QueueCommandStateRefresh();
        };
        services.selection.plotModes = [this](std::string const& plotId,
                                               std::string& interactionMode,
                                               std::string& selectionMode)
        {
            auto found = plots_.find(plotId);
            if (found == plots_.end() || !found->second) return false;
            interactionMode = found->second->interactionMode;
            selectionMode = found->second->selectionMode;
            return true;
        };
        services.selection.setInteractionMode = [this](std::string const& plotId,
                                                        std::string const& mode)
        {
            auto found = plots_.find(plotId);
            if (found == plots_.end() || !found->second) return false;
            found->second->interactionMode = mode;
            return true;
        };
        services.selection.setSelectionMode = [this](std::string const& plotId,
                                                      std::string const& mode)
        {
            auto found = plots_.find(plotId);
            if (found == plots_.end() || !found->second) return false;
            found->second->selectionMode = mode;
            return true;
        };
        services.selection.addLinearModelOverlay = [this](std::string const& plotId,
                                                           std::string const& source)
        {
            auto found = plots_.find(plotId);
            if (found == plots_.end() || !found->second) return false;
            if (source == "both")
            {
                ::rlispstat::core::AddOverlaySource(*found->second, "all");
                ::rlispstat::core::AddOverlaySource(*found->second, "selected");
            }
            else ::rlispstat::core::AddOverlaySource(*found->second, source);
            QueueSmoothRecompute(*found->second,
                source == "selected" ? ::rlispstat::core::SmoothCurveScope::Selection :
                source == "color" ? ::rlispstat::core::SmoothCurveScope::ColorGroup :
                ::rlispstat::core::SmoothCurveScope::Overall);
            if (source == "both")
                QueueSmoothRecompute(*found->second,
                    ::rlispstat::core::SmoothCurveScope::Selection);
            return true;
        };
        services.selection.clearOverlays = [this](std::string const& plotId)
        {
            auto found = plots_.find(plotId);
            if (found == plots_.end() || !found->second) return false;
            found->second->overlays.clear();
            ::rlispstat::core::InvalidateScatterMatrixFits(*found->second);
            found->second->smoothCurves.erase(
                std::remove_if(found->second->smoothCurves.begin(),
                    found->second->smoothCurves.end(), [](auto const& curve)
                    { return curve.fitMethod == "lm"; }),
                found->second->smoothCurves.end());
            for (auto& panel : found->second->trellisPanelSmoothCurves)
                panel.second.erase(std::remove_if(panel.second.begin(), panel.second.end(),
                    [](auto const& curve) { return curve.fitMethod == "lm"; }),
                    panel.second.end());
            return true;
        };
        services.selection.removeLinearModelOverlay = [this](
            std::string const& plotId, std::string const& source)
        {
            auto found = plots_.find(plotId);
            if (found == plots_.end() || !found->second) return false;
            ::rlispstat::core::RemoveOverlaySource(*found->second, source);
            if (found->second->kind == "scatter_matrix" &&
                found->second->scatterMatrixFitsPending)
                QueueSmoothRecompute(*found->second,
                    ::rlispstat::core::SmoothCurveScope::Overall, "lm");
            return true;
        };
        services.selection.replaceSmoothCurves = [this](std::string const& plotId,
            ::rlispstat::core::SmoothCurveScope scope,
            std::vector<::rlispstat::core::SmoothCurveData> const& curves)
        {
            auto found = plots_.find(plotId);
            if (found == plots_.end() || !found->second) return false;
            auto& target = found->second->smoothCurves;
            const std::string fitMethod = curves.empty() ? "loess" : curves.front().fitMethod;
            target.erase(std::remove_if(target.begin(), target.end(), [scope, fitMethod](auto const& curve)
            { return curve.scope == scope && curve.fitMethod == fitMethod; }), target.end());
            target.insert(target.end(), curves.begin(), curves.end());
            return true;
        };
        services.selection.replaceTrellisSmoothCurves = [this](std::string const& plotId,
            std::string const& panelId, ::rlispstat::core::SmoothCurveScope scope,
            std::vector<::rlispstat::core::SmoothCurveData> const& curves)
        {
            auto found = plots_.find(plotId);
            if (found == plots_.end() || !found->second) return false;
            if (found->second->kind == "scatter_matrix")
            {
                const std::string prefix = "matrix:" +
                    std::to_string(found->second->scatterMatrixFitGeneration) + ":";
                const std::string source = scope == ::rlispstat::core::SmoothCurveScope::Selection
                    ? "selected" : scope == ::rlispstat::core::SmoothCurveScope::ColorGroup
                    ? "color" : "all";
                if (panelId.rfind(prefix, 0) != 0 ||
                    !::rlispstat::core::HasOverlaySource(*found->second, source)) return true;
            }
            auto& target = found->second->trellisPanelSmoothCurves[panelId];
            const std::string fitMethod = curves.empty() ? "loess" : curves.front().fitMethod;
            target.erase(std::remove_if(target.begin(), target.end(), [scope, fitMethod](auto const& curve)
            { return curve.scope == scope && curve.fitMethod == fitMethod; }), target.end());
            target.insert(target.end(), curves.begin(), curves.end());
            return true;
        };
        services.selection.setAxisVariable = [this](std::string const& plotId, bool changeX,
                                                     std::string const& variable)
        {
            auto found = plots_.find(plotId);
            if (found == plots_.end() || !found->second) return false;
            auto& model = *found->second;
            const auto* numericVariable =
                ::rlispstat::core::FindNumericVariable(model, variable);
            if (!numericVariable) return false;
            if ((changeX ? model.xLabel : model.yLabel) == variable) return true;
            if (model.kind == "histogram" && changeX)
            {
                if (std::none_of(numericVariable->values.begin(),
                                 numericVariable->values.end(),
                                 [](double value) { return std::isfinite(value); }))
                    return false;
                const std::string previousDefaultTitle =
                    ::rlispstat::core::HistogramDefaultTitle(model.xLabel);
                model.xLabel = variable;
                ::rlispstat::core::RebuildHistogramPointsFromCurrentVariable(model);
                if (model.title.empty() || model.title == previousDefaultTitle)
                    model.title = ::rlispstat::core::HistogramDefaultTitle(variable);
                auto view = views_.find(plotId);
                if (view != views_.end() && view->second) view->second->Show(model);
                return true;
            }
            if (changeX) model.xLabel = variable; else model.yLabel = variable;
            ::rlispstat::core::RebuildPointsForCurrentVariables(model);
            ::rlispstat::core::ComputeRanges(model);
            // Linear overlays are specifications, not cached geometry, and
            // therefore refit locally from the rebuilt points. Smooth curves
            // are coordinates returned by R: retain their enabled scopes but
            // discard the obsolete coordinates and queue fresh fits.
            if (::rlispstat::core::InvalidateSmoothCurvesForCoordinateChange(
                    model.smoothCurves))
                QueueExistingSmoothRecompute(model);
            auto view = views_.find(plotId);
            if (view != views_.end() && view->second) view->second->Show(model);
            return true;
        };
        services.selection.toggleSmoothCurves = [this](std::string const& plotId,
                                                        ::rlispstat::core::SmoothCurveScope scope)
        {
            auto found = plots_.find(plotId);
            if (found == plots_.end() || !found->second) return false;
            auto& model = *found->second;
            const bool wasPresent = ::rlispstat::core::SmoothCurveScopeIsPresent(
                model.smoothCurves, scope);
            if (!::rlispstat::core::ToggleSmoothCurveScopePending(model.smoothCurves, scope))
                return false;
            if (!wasPresent)
                // Keep every smooth request on the same path. In particular,
                // QueueSmoothRecompute only supplies Trellis split values when
                // a split variable is actually configured. The former inline
                // path supplied one empty string per point for an unsplit
                // Trellis plot; R consequently treated it as grouped data and
                // produced no curves.
                QueueSmoothRecompute(model, scope);
            return true;
        };
        services.selection.mutateHistogram = [this](std::string const& plotId,
            std::function<void(::rlispstat::core::PlotModel&)> const& mutation)
        {
            auto found = plots_.find(plotId);
            if (found == plots_.end() || !found->second ||
                found->second->kind != "histogram") return false;
            mutation(*found->second);
            return true;
        };
        services.selection.mutateBarplot = [this](std::string const& plotId,
            std::function<void(::rlispstat::core::PlotModel&)> const& mutation)
        {
            auto found = plots_.find(plotId);
            if (found == plots_.end() || !found->second ||
                found->second->kind != "barplot") return false;
            mutation(*found->second);
            return true;
        };
        services.selection.mutateScatterMatrix = [this](std::string const& plotId,
            std::function<bool(::rlispstat::core::PlotModel&)> const& mutation)
        {
            auto found = plots_.find(plotId);
            if (found == plots_.end() || !found->second ||
                found->second->kind != "scatter_matrix") return false;
            return mutation(*found->second);
        };
        services.selection.mutateBoxplot = [this](std::string const& plotId,
            ::rlispstat::core::CommandBoxplotMutation const& mutation,
            std::string& error)
        {
            auto found = plots_.find(plotId);
            if (found == plots_.end() || !found->second ||
                found->second->kind != "boxplot") return false;
            return mutation(*found->second, error);
        };
        services.selection.refitCorrelation = [this](std::string const& id)
        {
            if (!commandDispatcher_) return false;
            auto& states = commandDispatcher_->applicationState().correlationMatrices();
            auto found = states.find(id);
            if (found == states.end()) return false;
            auto& state = found->second;
            commandDispatcher_->applicationState().captureAnalysisScope(state.group,state.dataScope,state.dataScopeCaptured);
            auto const* dataframe = commandDispatcher_->applicationState().datasets().find(state.group);
            if (!dataframe) return false;
            QueueCorrelationFit(state);
            ++state.modelVersion;
            ShowCorrelationMatrix(id);
            return true;
        };
        services.selection.refitDimensionality = [this](std::string const& id)
        {
            return RefitDimensionality(id);
        };
        services.selection.refitScaleAnalysis = [this](std::string const& id)
        {
            return RefitScaleAnalysis(id);
        };
        services.selection.refitDendrogram = [this](std::string const& id)
        {
            return RefitDendrogram(id);
        };
        services.ui.redrawPlot = [this](std::string const& plotId)
        {
            if (!commandDispatcher_) return;
            auto found = views_.find(plotId);
            auto model = plots_.find(plotId);
            if (found == views_.end() || !found->second ||
                model == plots_.end() || !model->second) return;
            // Overlay commands mutate the model without changing selection.
            // A selection-only refresh was therefore a no-op and left the new
            // fit/smooth invisible until a later pointer event.
            found->second->Show(*model->second);
            found->second->SetVisualStyle(commandDispatcher_->plotTheme(),
                commandDispatcher_->applicationState().displayPointColors(
                    model->second->group));
            std::set<int> selected;
            commandDispatcher_->applicationState().selectedRows(
                model->second->group, selected);
            found->second->RefreshSelection(selected);
        };
        services.ui.refreshPlotTheme = [this]()
        {
            if (!commandDispatcher_) return;
            for (auto const& [plotId, view] : views_)
            {
                auto model = plots_.find(plotId);
                if (!view || model == plots_.end() || !model->second) continue;
                view->ApplyGlobalTheme(commandDispatcher_->plotTheme());
            }
            QueueCommandStateRefresh();
        };
        services.ui.closePlot = [this](std::string const& plotId)
        {
            auto found = views_.find(plotId);
            if (found == views_.end() || !found->second) return false;
            found->second->Close();
            return true;
        };
        services.ui.closeAll = [this]()
        {
            ResetAllViews();
        };
        services.ui.datasetRegistered = [this](std::string const& group, bool showDataSheet)
        {
            if (!commandDispatcher_) return;
            auto const* dataframe = commandDispatcher_->applicationState().datasets().find(group);
            if (!dataframe) return;
            if (showDataSheet) ShowDataSheet(group);
            else HandleDatasetMutation({
                ::rlispstat::core::DatasetMutationKind::VariableMetadata,
                group, {}, {}, 0 });
            for (auto &[id,state]:commandDispatcher_->applicationState().correlationMatrices()) {
                if (state.group!=group || state.precomputed) continue;
                ::rlispstat::core::PopulateDatasetSeedPlot(state.seed,*dataframe,group);state.hasSeed=true;
                state.variables.erase(std::remove_if(state.variables.begin(),state.variables.end(),
                    [&](const auto &name) { return !::rlispstat::core::FindNumericVariable(state.seed,name); }),state.variables.end());
                QueueCorrelationFit(state);
                if(correlationViews_.count(id)) ShowCorrelationMatrix(id);
            }
            QueueCommandStateRefresh();
        };
        services.ui.activeDatasetChanged = [this](std::string const&)
        {
            QueueCommandStateRefresh();
        };
        services.ui.openDataSheet = [this](std::string const& group)
        {
            ShowDataSheet(group);
        };
        services.ui.showVariablesWindow = [this](std::string const& group)
        {
            ShowVariablesWindow(group);
        };
        services.ui.datasetVariableTypeChanged = [this](
            std::string const& group, std::string const& variable,
            std::string const& type,
            ::rlispstat::core::VariableTypeChangeEffects const& effects)
        {
            HandleVariableTypeChanged(group, variable, type, effects);
        };
        services.ui.datasetMutated = [this](
            ::rlispstat::core::DatasetMutationEvent const& event)
        {
            HandleDatasetMutation(event);
        };
        services.ui.analysisScopeChanged = [this](
            ::rlispstat::core::AnalysisScopeChangeEvent const& event)
        {
            auto& application = commandDispatcher_->applicationState();
            RefreshOpenPlotsForScope(event.datasetId, false);
            RefitOpenAnalysesForScope(event.datasetId);
            auto sheet = dataSheets_.find(event.datasetId);
            if (sheet != dataSheets_.end() && sheet->second)
                sheet->second->SetAnalysisScope(event.currentScope);
            for (auto const& [id, model] : plots_)
            {
                if (!model || model->group != event.datasetId) continue;
                auto view = views_.find(id);
                if (view != views_.end() && view->second)
                    view->second->SetAnalysisScope(event.currentScope);
            }
            RefreshAnalysisScopeIndicators(event.datasetId);
            RefreshAnalysisScopePanel();
            QueueCommandStateRefresh();
        };
        services.ui.showActiveDataset = [this]()
        {
            if (!commandDispatcher_) return;
            ShowDataSheet(commandDispatcher_->applicationState().datasets().activeDatasetGroup());
        };
        services.ui.showTable1 = [this](::rlispstat::core::Table1DisplayState const& state)
        {
            ShowTable1(state);
        };
        services.ui.showCorrelationMatrix = [this](std::string const& id)
        {
            ShowCorrelationMatrix(id);
        };
        services.ui.showMeanComparison = [this](::rlispstat::core::MeanComparisonState const& state)
        {
            ShowMeanComparison(state);
        };
        services.ui.showCompareMeansTable = [this](::rlispstat::core::Table1DisplayState const& state)
        {
            ShowTable1(state);
        };
        services.ui.showLinearModel = [this](std::string const& group)
        {
            ShowLinearModel(group);
            if (!commandDispatcher_) return;
            auto const model = commandDispatcher_->applicationState().groupModels().find(group);
            if (model != commandDispatcher_->applicationState().groupModels().end() &&
                model->second.isStale && !model->second.precomputed)
                QueueLinearModelFit(group);
        };
        services.ui.openLinearModelDiagnostic = [this](std::string const& group,
                                                        std::string const& kind)
        {
            return OpenLinearModelDiagnostic(group, kind);
        };
        services.ui.showDendrogram = [this](std::string const& id) { ShowDendrogram(id); };
        services.ui.showDimensionality = [this](std::string const& id)
        {
            ShowDimensionality(id);
        };
        services.ui.refreshDimensionalityPlots = [this](std::string const& id)
        {
            RefreshDimensionalityPlots(id);
        };
        services.ui.showScaleAnalysis = [this](std::string const& id)
        {
            ShowScaleAnalysis(id);
        };
        services.ui.refreshScaleAnalysisPlots = [](std::string const&)
        {
            // Scale-derived plots are opened from the owning Scale Analysis
            // session.  The result window itself is refreshed by
            // showScaleAnalysis after each accepted immutable revision.
        };
        services.ui.showRCode = [this](std::string const& title, std::string const& code,
            std::optional<::rlispstat::core::DataFrameModel> const& verificationData)
        {
            const std::string displayedCode = verificationData
                ? ::rlispstat::core::BuildQuartoVerificationDocument(title, code) : code;
            auto currentCode = std::make_shared<std::string>(displayedCode);
            auto dialog = Controls::ContentDialog();
            dialog.Title(box_value(to_hstring(title)));
            if (verificationData) dialog.SecondaryButtonText(L"Save Quarto + Data...");
            dialog.CloseButtonText(L"Close");
            auto box = Controls::TextBox();
            box.Text(to_hstring(displayedCode)); box.IsReadOnly(true); box.AcceptsReturn(true);
            box.TextWrapping(TextWrapping::NoWrap);
            box.FontFamily(Media::FontFamily(L"Consolas"));
            box.MinHeight(480); box.HorizontalAlignment(HorizontalAlignment::Stretch);
            box.VerticalScrollBarVisibility(ScrollBarVisibility::Auto);
            box.HorizontalScrollBarVisibility(ScrollBarVisibility::Auto);
            dialog.Content(box);
            auto owner = technicalOwner_.Content().try_as<FrameworkElement>();
            if (owner) dialog.XamlRoot(owner.XamlRoot());
            if (verificationData)
            {
                auto data = *verificationData;
                dialog.SecondaryButtonClick([currentCode, data, box](auto const&,
                    Controls::ContentDialogButtonClickEventArgs const& args)
                {
                    args.Cancel(true);
                    com_ptr<IFileSaveDialog> chooser;
                    if (FAILED(CoCreateInstance(CLSID_FileSaveDialog, nullptr,
                            CLSCTX_INPROC_SERVER, IID_PPV_ARGS(chooser.put())))) return;
                    COMDLG_FILTERSPEC filter{ L"Quarto document (*.qmd)", L"*.qmd" };
                    chooser->SetFileTypes(1, &filter);
                    chooser->SetDefaultExtension(L"qmd");
                    chooser->SetFileName(L"comprobar_resultado.qmd");
                    if (FAILED(chooser->Show(GetActiveWindow()))) return;
                    com_ptr<IShellItem> item;
                    if (FAILED(chooser->GetResult(item.put()))) return;
                    PWSTR rawPath = nullptr;
                    if (FAILED(item->GetDisplayName(SIGDN_FILESYSPATH, &rawPath)) || !rawPath)
                        return;
                    std::filesystem::path selectedPath(rawPath);
                    CoTaskMemFree(rawPath);
                    if (selectedPath.extension() != L".qmd" &&
                        selectedPath.extension() != L".QMD")
                        selectedPath.replace_extension(L".qmd");
                    const std::wstring baseName = selectedPath.stem().wstring().empty()
                        ? L"comprobar_resultado" : selectedPath.stem().wstring();
                    const std::filesystem::path bundleDirectory =
                        selectedPath.parent_path() / baseName;
                    const std::filesystem::path quartoPath =
                        bundleDirectory / (baseName + L".qmd");
                    const std::filesystem::path dataPath =
                        bundleDirectory / (baseName + L"_data.rds");
                    bool ok = true;
                    std::string failure;
                    std::error_code directoryError;
                    if (std::filesystem::exists(bundleDirectory, directoryError))
                    {
                        MessageBoxW(GetActiveWindow(),
                            L"A folder with that name already exists. Choose another name so LinkEDA does not overwrite an existing verification bundle.",
                            L"R Verification Export Failed", MB_OK | MB_ICONERROR);
                        return;
                    }
                    ok = std::filesystem::create_directory(bundleDirectory, directoryError);
                    if (!ok)
                    {
                        failure = directoryError
                            ? "The verification folder could not be created: " +
                                directoryError.message()
                            : "The verification folder could not be created.";
                        MessageBoxW(GetActiveWindow(), to_hstring(failure).c_str(),
                            L"R Verification Export Failed", MB_OK | MB_ICONERROR);
                        return;
                    }
                    const auto unique = std::to_wstring(GetCurrentProcessId()) + L"-" +
                        std::to_wstring(GetTickCount64());
                    const auto inputPath = std::filesystem::temp_directory_path() /
                        (L"linkeda-verification-" + unique + L".csv");
                    const auto metadataPath = std::filesystem::temp_directory_path() /
                        (L"linkeda-verification-" + unique + L"-metadata.csv");
                    if (ok)
                    {
                        std::ofstream input(inputPath, std::ios::out | std::ios::trunc |
                                                       std::ios::binary);
                        ok = input && ::rlispstat::core::WriteDataExportCSV(input, data);
                    }
                    if (ok)
                    {
                        std::ofstream metadata(metadataPath, std::ios::out | std::ios::trunc |
                                                             std::ios::binary);
                        ok = metadata &&
                            ::rlispstat::core::WriteDataExportMetadataCSV(metadata, data);
                    }
                    if (!data.verificationPreparedRds.empty()) {
                        std::error_code copyError;
                        ok = std::filesystem::copy_file(std::filesystem::path(to_hstring(data.verificationPreparedRds).c_str()), dataPath, copyError);
                        if (!ok) failure = "The frozen R input could not be copied: " + copyError.message();
                    } else if (ok) ok = RunRScriptText(
                        ::rlispstat::core::NativeDataExportRScript(),
                        { inputPath.wstring(), metadataPath.wstring(), dataPath.wstring(),
                          L"rds" }, failure);
                    std::error_code ignored;
                    std::filesystem::remove(inputPath, ignored);
                    std::filesystem::remove(metadataPath, ignored);
                    if (!ok)
                    {
                        std::filesystem::remove_all(bundleDirectory, ignored);
                        if (failure.empty())
                            failure = "The R verification data could not be written.";
                        MessageBoxW(GetActiveWindow(), to_hstring(failure).c_str(),
                            L"R Verification Export Failed", MB_OK | MB_ICONERROR);
                        return;
                    }

                    std::string updatedCode;
                    if (!::rlispstat::core::BindAnalysisVerificationDataPath(
                            *currentCode, to_string(hstring(dataPath.wstring())),
                            updatedCode))
                    {
                        std::filesystem::remove_all(bundleDirectory, ignored);
                        MessageBoxW(GetActiveWindow(),
                            L"The data were saved, but the R code path could not be updated.",
                            L"R Verification Export Failed", MB_OK | MB_ICONERROR);
                        return;
                    }
                    {
                        std::ofstream script(quartoPath, std::ios::out | std::ios::trunc |
                                                         std::ios::binary);
                        script.write(updatedCode.data(),
                                     static_cast<std::streamsize>(updatedCode.size()));
                        ok = static_cast<bool>(script);
                    }
                    if (ok)
                    {
                        *currentCode = std::move(updatedCode);
                        box.Text(to_hstring(*currentCode));
                    }
                    else
                    {
                        failure = "The Quarto verification document could not be written.";
                        std::filesystem::remove_all(bundleDirectory, ignored);
                    }
                    MessageBoxW(GetActiveWindow(),
                        to_hstring(ok
                            ? "The Quarto document and its data were saved successfully in " +
                                to_string(hstring(bundleDirectory.wstring()))
                            : failure).c_str(),
                        ok ? L"R Verification Package Saved" : L"R Verification Export Failed",
                        MB_OK | (ok ? MB_ICONINFORMATION : MB_ICONERROR));
                });
            }
            dialog.ShowAsync();
        };
        services.ui.showGeneralizedGLM = [this](std::string const& id)
        {
            ShowGeneralizedModel(id);
        };
        services.ui.refreshGeneralizedGLM = [this](std::string const& id)
        {
            ShowGeneralizedModel(id);
        };
        services.ui.requestGeneralizedGLMFit = [this](std::string const& id)
        {
            return RequestGeneralizedModelFit(id);
        };
        services.ui.openGeneralizedGLMDiagnostic = [this](std::string const& id,
                                                           std::string const& kind)
        {
            return OpenGeneralizedModelDiagnostic(id, kind);
        };
        services.ui.showMixedModel = [this](::rlispstat::core::NativeMixedModelState const& state)
        {
            ShowMixedModel(state);
        };
        services.ui.showMixedModelText = [this](std::string const& id, std::string const& group,
            std::string const& modelType, std::string const& text)
        {
            ShowMixedModelText(id, group, modelType, text);
        };
        services.ui.requestRegressionComparisonFit = [this](::rlispstat::core::RegressionComparisonState& state)
        {
            QueueRegressionComparison(state);
        };
        services.ui.regressionComparisonUpdated = [this](::rlispstat::core::RegressionComparisonState const& state)
        {
            if (!commandDispatcher_) return;
            commandDispatcher_->applicationState().regressionComparisons()[state.id] = state;
            ShowRegressionComparison(state.id);
        };
        services.ui.showRegressionComparison = [this](std::string const& id)
        {
            ShowRegressionComparison(id);
        };
        services.ui.refreshRegressionComparison = [this](std::string const& group)
        {
            if (!commandDispatcher_) return;
            for (auto const& [id, state] : commandDispatcher_->applicationState().regressionComparisons())
                if (state.group == group) ShowRegressionComparison(id);
        };
        services.ui.refreshModelTrellis = [this](std::string const& id)
        {
            ShowModelTrellis(id);
        };
        services.ui.showGeneralizedComparison = [this](std::string const& id)
        {
            ShowGeneralizedComparison(id);
        };
        services.ui.regressionComparisonVisible = [this](std::string const& id, bool& visible)
        {
            visible = regressionComparisonViews_.count(id) && regressionComparisonViews_[id];
            return true;
        };
        services.ui.openRegressionComparisonDiagnostic = [this](std::string const& id,
                                                                  int modelIndex,
                                                                  std::string const& kind)
        {
            return OpenRegressionComparisonDiagnostic(id, modelIndex, kind);
        };
        services.ui.refreshModelGroup = [this](std::string const& group)
        {
            // Model roles are shared dataset metadata. Changing one in Variable
            // View must keep any already-open editors synchronized, but it must
            // not manufacture a General Linear Model output window. A model is
            // fitted here only when the user already has that output open.
            if (variableViews_.count(group) && variableViews_[group])
                ShowVariablesWindow(group, false);
            if (linearModelViews_.count(group) && linearModelViews_[group]) {
                auto model = commandDispatcher_->applicationState().groupModels().find(group);
                if (model != commandDispatcher_->applicationState().groupModels().end() &&
                    model->second.isStale)
                    QueueLinearModelFit(group);
                else
                    ShowLinearModel(group);
            } else if (commandDispatcher_ &&
                       commandDispatcher_->applicationState().datasets().contains(group)) {
                for (auto const& [modelId, view] : linearModelViews_) {
                    auto model = commandDispatcher_->applicationState().groupModels().find(modelId);
                    if (view && model != commandDispatcher_->applicationState().groupModels().end() &&
                        model->second.group == group)
                        QueueLinearModelFit(modelId);
                }
            }
        };
        services.ui.modelUpdated = [this](std::string const& group, std::vector<int> const&)
        {
            // An asynchronous R reply may arrive after its output was closed.
            // Keep the result in ApplicationState without reopening the window.
            if (linearModelViews_.count(group) && linearModelViews_[group])
                ShowLinearModel(group);
        };
        services.ui.addPlot = [this](::rlispstat::core::PlotModel* plot)
        {
            if (!plot || !commandDispatcher_) return;
            const std::string plotId = plot->id;
            plots_[plotId] = std::unique_ptr<::rlispstat::core::PlotModel>(plot);
            auto& coordinator = commandDispatcher_->plotCoordinator();
            const auto attached = AttachPlotModel(*plot);
            if (!attached.accepted)
            {
                plots_.erase(plotId);
                return;
            }
            coordinator.activatePlot(plotId);
            ShowNativePlot(*plot);
        };
        services.session.addPlot = [this](::rlispstat::core::SessionPlot const& plot)
        {
            auto model = std::make_unique<::rlispstat::core::PlotModel>();
            model->id = plot.id;
            model->group = plot.group;
            model->xLabel = plot.xLabel;
            model->yLabel = plot.yLabel;
            model->title = plot.title;
            for (auto const& point : plot.points)
            {
                model->points.push_back({ point.x, point.y, point.row });
            }

            if (!commandDispatcher_) return std::string("ERR coordinator unavailable");
            auto* modelPointer = model.get();
            plots_[plot.id] = std::move(model);
            auto& coordinator = commandDispatcher_->plotCoordinator();
            const auto coordination = AttachPlotModel(*modelPointer);
            if (!coordination.accepted)
            {
                plots_.erase(plot.id);
                return std::string("ERR invalid plot identity");
            }
            coordinator.activatePlot(plot.id);
            ShowScatterPlot(plot);
            return std::string();
        };
        services.session.selectedRows = [this](std::string const& group, std::set<int>& rows)
        {
            return commandDispatcher_->applicationState().selectedRows(group, rows);
        };
        services.session.clearSelection = [this](std::string const& group)
        {
            if (!commandDispatcher_)
            {
                return std::string("ERR coordinator unavailable");
            }
            const auto coordination =
                commandDispatcher_->plotCoordinator().clearSelection(group);
            if (!coordination.accepted) return std::string("ERR no active plot/group");
            RefreshSelectionVisuals(coordination.event);
            return std::string();
        };
        services.session.closeAll = [this]()
        {
            ResetAllViews();
        };
        commandDispatcher_ =
            std::make_unique<::rlispstat::core::CommandDispatcher>(std::move(services));
        commandDispatcher_->setMissingInformationRefresh([this](const ::rlispstat::core::Table1DisplayState& state) {
            dispatcherQueue_.TryEnqueue([this,state]() {
                const auto found=outputViews_.find(state.id);
                if(found==outputViews_.end() || !found->second) return;
                outputStates_[state.id]=state;
                found->second->Show(state);
            });
        });
    }

    bool App::PreparePersistentColorsForManualEdit(std::string const& group)
    {
        if (!commandDispatcher_ || group.empty()) return false;
        auto& application = commandDispatcher_->applicationState();
        const auto activeOverride = application.colorOverride(group);
        if (!activeOverride) return true;

        const std::wstring message =
            L"Colors are currently assigned by \u201c" +
            std::wstring(to_hstring(activeOverride->variable).c_str()) +
            L"\u201d.\n\nYes: edit the visible colors and make them the current case colors."
            L"\nNo: restore the previous case colors before making this change."
            L"\nCancel: leave all colors unchanged.";
        const int choice = MessageBoxW(GetActiveWindow(), message.c_str(),
            L"Color override is active", MB_YESNOCANCEL | MB_ICONQUESTION);
        if (choice == IDCANCEL || choice == 0) return false;
        if (choice == IDYES)
            application.promoteColorOverrideToPersistent(group);
        else
            application.cancelColorOverride(group);
        return true;
    }

    std::string App::DispatchApplicationCommand(std::vector<std::string> const& command)
    {
        if (!commandDispatcher_ || command.empty()) return "ERR dispatcher unavailable";
        LogStartup(std::string("Application command: ") + command[0]);
        if (command[0] == "WELCOME_ABOUT")
        {
            ShowAbout(); return "OK";
        }
        if (command[0] == "FILE_QUIT_APPLICATION")
        {
            QuitApplication(); return "OK";
        }
        if (command[0] == "UI_SET_INTERFACE_FONT")
        {
            if (command.size() < 2 || !ApplyInterfaceFont(command[1], true))
                return "ERR unsupported interface font";
            return "OK";
        }
        if (command[0] == "UI_SET_INTERFACE_SIZE")
        {
            if (command.size() < 2 || !ApplyInterfaceSize(command[1], true))
                return "ERR unsupported interface size";
            return "OK";
        }
        const std::string group = command.size() > 1 && !command[1].empty()
            ? command[1]
            : commandDispatcher_->applicationState().datasets().activeDatasetGroup();
        const bool selectedColorCommand = command[0] == "SET_SELECTED_COLOR" ||
            command[0] == "RESET_SELECTED_COLOR";
        bool hasManualColorTarget = true;
        if (selectedColorCommand) {
            std::set<int> selected;
            hasManualColorTarget = commandDispatcher_->applicationState()
                .selectedRows(group, selected) && !selected.empty();
            if (!hasManualColorTarget && command[0] == "RESET_SELECTED_COLOR")
                return "OK no selected rows";
        } else if (command[0] == "SET_POINT_COLOR") {
            hasManualColorTarget = command.size() > 3 && std::atoi(command[3].c_str()) > 0;
        } else if (command[0] == "CLEAR_ROW_COLORS" && command.size() > 2) {
            hasManualColorTarget = std::atoi(command[2].c_str()) > 0;
            if (!hasManualColorTarget) return "OK no selected rows";
        }
        if ((selectedColorCommand || command[0] == "SET_POINT_COLOR" ||
             command[0] == "CLEAR_ROW_COLORS") && hasManualColorTarget &&
            !PreparePersistentColorsForManualEdit(group))
            return "OK cancelled";
        const auto openPlotDialog = [this, &group](PlotWorkflowKind kind)
        {
            if (group.empty()) return false;
            if (dataSheets_.find(group) == dataSheets_.end()) ShowDataSheet(group);
            OpenPlotWorkflowDialog(group, kind);
            return true;
        };
        if (command[0] == "WELCOME_SHOW")
        {
            ShowWelcome(welcomeVersion_); return "OK";
        }
        if (command[0] == "WELCOME_DOCUMENTATION")
        {
            const auto& strings = ::rlispstat::core::DefaultWelcomeStrings();
            MessageBoxW(nullptr, to_hstring(strings.documentationWorkInProgress).c_str(),
                to_hstring(strings.documentation).c_str(), MB_OK | MB_ICONINFORMATION);
            return "OK";
        }
        if (command[0] == "FILE_IMPORT_DATA")
        {
            if (!welcomeWindow_ || welcomeWindow_->Closed()) ShowWelcome(welcomeVersion_);
            ChooseWelcomeFile(); return "OK";
        }
        if (command[0] == "FILE_OPEN_DATA")
        {
            ChooseNativeDocumentFile(); return "OK";
        }
        if (command[0] == "FILE_SAVE_DATA" || command[0] == "FILE_SAVE_DATA_AS")
        {
            if (group.empty()) return "ERR no active dataset";
            auto path = documentPaths_.find(group);
            if (command[0] == "FILE_SAVE_DATA_AS" || path == documentPaths_.end())
                ChooseSaveDocumentFile(group);
            else SaveNativeDocument(group, path->second);
            return "OK";
        }
        if (command[0] == "FILE_EXPORT_DATA")
        {
            if (group.empty()) return "ERR no active dataset";
            ChooseExportDataFile(group);
            return "OK";
        }
        if (command[0] == "FILE_CLOSE_DATASET")
        {
            if (group.empty() || !commandDispatcher_->applicationState().datasets().contains(group))
                return "ERR no active dataset";
            std::vector<std::string> plotIds;
            for (auto const& [id, plot] : plots_)
                if (plot && plot->group == group) plotIds.push_back(id);
            if (applicationCommands_) applicationCommands_->CloseDatasetWindows(group);
            for (auto const& id : plotIds)
            {
                if (auto view = views_.find(id); view != views_.end() && view->second)
                    view->second->Close();
                views_.erase(id);
                plots_.erase(id);
            }
            dataSheets_.erase(group);
            variableViews_.erase(group);
            documentPaths_.erase(group);
            documentBaselines_.erase(group);
            commandDispatcher_->applicationState().closeDatasetAndAnalyses(group);
            RefreshAnalysisScopePanel();
            return "OK";
        }
        if (command[0] == "FILE_OPEN_DATA_FROM_R")
        {
            pendingMainRTasks_.dataBrowseTasks.push_back({ StartupId("menu-r-", "data") });
            MarkMainRTaskPending();
            return "OK";
        }
        if (command[0] == "FILE_RETURN_DATA_TO_R" ||
            command[0] == "FILE_RETURN_SELECTED_ROWS_TO_R")
        {
            if (group.empty()) return "ERR no active dataset";
            if (dataSheets_.find(group) == dataSheets_.end()) ShowDataSheet(group);
            OpenRDataAssignmentDialog(group,
                command[0] == "FILE_RETURN_SELECTED_ROWS_TO_R");
            return "OK";
        }
        if (command[0] == "DATA_POINTS_COLOR")
        {
            if (group.empty()) return "ERR no active dataset";
            ShowColorPalette(group); return "OK";
        }
        if (command[0] == "SHOW_ANALYSIS_SCOPE_PANEL")
        {
            if (group.empty()) return "ERR no active dataset";
            ShowAnalysisScopePanel(group); return "OK";
        }
        if (command[0] == "SAVE_ANALYSIS_SCOPE_FROM_SELECTION")
        {
            if (group.empty()) return "ERR no active dataset";
            if (command.size() > 2 && !command[2].empty())
                return commandDispatcher_->dispatch(command);
            if (dataSheets_.find(group) == dataSheets_.end()) ShowDataSheet(group);
            auto sheet = dataSheets_.find(group);
            if (sheet == dataSheets_.end() || !sheet->second)
                return "ERR data sheet is unavailable";
            auto owner = sheet->second->NativeWindow().Content()
                .try_as<Microsoft::UI::Xaml::FrameworkElement>();
            SaveCurrentSelectionAsScopeAsync(group, owner);
            return "OK";
        }
        if (command[0] == "DATA_CHOOSE_LABEL_COLUMN")
        {
            if (group.empty()) return "ERR no active dataset";
            if (dataSheets_.find(group) == dataSheets_.end()) ShowDataSheet(group);
            OpenChooseLabelColumnDialog(group); return "OK";
        }
        if (command[0] == "DATA_MISSING_DATA_PATTERNS" || command[0] == "DATA_MISSING_DATA_OVERVIEW" || command[0] == "DATA_MISSINGNESS_MODELS")
        {
            if (group.empty()) return "ERR no active dataset";
            if (dataSheets_.find(group) == dataSheets_.end()) ShowDataSheet(group);
            OpenMissingDataPatternsDialog(group,command[0]=="DATA_MISSINGNESS_MODELS"); return "OK";
        }
        if (command[0] == "DATA_IMPUTATION_DIAGNOSTICS") {
            pendingMainRTasks_.miDiagnosticsTasks.push_back({command.size()>1&&!command[1].empty()?command[1]:group,
                command.size()>2?command[2]:"summary",command.size()>3?command[3]:"",
                command.size()>4?std::atoi(command[4].c_str()):1});
            MarkMainRTaskPending();return "OK";
        }
        if (command[0] == "ANALYZE_MISSING_DATA_IMPUTATION")
        {
            if (group.empty()) return "ERR no active dataset";
            if (dataSheets_.find(group) == dataSheets_.end())
            {
                ShowDataSheet(group);
                dispatcherQueue_.TryEnqueue(
                    Microsoft::UI::Dispatching::DispatcherQueuePriority::Normal,
                    [this, group]() { OpenMissingDataImputationDialog(group); });
            }
            else OpenMissingDataImputationDialog(group);
            return "OK";
        }
        if (command[0] == "SNAPSHOT_ALBUM_SHOW")
        {
            ShowSnapshotAlbum(); return "OK";
        }
        if (command[0] == "SNAPSHOT_ALBUM_ADD_ACTIVE")
        {
            if (command.size() < 4) return "ERR no active snapshot source";
            AddActiveSnapshot(command[1], command[2], command[3]); return "OK";
        }
        if (command[0] == "SNAPSHOT_ALBUM_EXPORT_PDF")
        {
            if (command.size() < 2 || command[1].empty()) return "ERR no export path";
            if (snapshotAlbum_.exportItems().empty()) return "ERR no snapshots";
            ShowSnapshotAlbum();
            ExportSnapshotAlbumPdfAsync(to_hstring(command[1]).c_str());
            return "OK";
        }
        if (command[0] == "PLOT_NEW_LINKED_SCATTERPLOT")
        {
            if (group.empty()) return "ERR no active dataset";
            if (dataSheets_.find(group) == dataSheets_.end()) ShowDataSheet(group);
            OpenScatterplotDialog(group); return "OK";
        }
        if (command[0] == "PLOT_NEW_TRELLIS_SCATTERPLOT")
        {
            if (group.empty()) return "ERR no active dataset";
            OpenTrellisPlotDialog(group, command.size() > 2 ? command[2] : "scatter");
            return "OK";
        }
        if (command[0] == "PLOT_NEW_TIME_SERIES")
            return openPlotDialog(PlotWorkflowKind::TimeSeries) ? "OK" : "ERR no active dataset";
        if (command[0] == "PLOT_NEW_LINKED_SCATTER_MATRIX")
            return openPlotDialog(PlotWorkflowKind::ScatterMatrix) ? "OK" : "ERR no active dataset";
        if (command[0] == "PLOT_NEW_PARALLEL_COORDINATES")
            return openPlotDialog(PlotWorkflowKind::ParallelCoordinates) ? "OK" : "ERR no active dataset";
        if (command[0] == "PLOT_NEW_LINKED_BOXPLOT")
            return openPlotDialog(PlotWorkflowKind::Boxplot) ? "OK" : "ERR no active dataset";
        if (command[0] == "PLOT_NEW_LINKED_HISTOGRAM")
            return openPlotDialog(PlotWorkflowKind::Histogram) ? "OK" : "ERR no active dataset";
        if (command[0] == "PLOT_NEW_LINKED_BAR_CHART")
            return openPlotDialog(PlotWorkflowKind::BarChart) ? "OK" : "ERR no active dataset";
        if (command[0] == "ANALYZE_TABLE1")
        {
            if (group.empty()) return "ERR no active dataset";
            return StartAnalysis(group, AnalysisWorkflowKind::Table1)
                ? "OK" : "ERR analysis could not be started";
        }
        if (command[0] == "ANALYZE_CONTINGENCY_TABLE")
        {
            if (group.empty()) return "ERR no active dataset";
            if (dataSheets_.find(group) == dataSheets_.end()) ShowDataSheet(group);
            OpenContingencyTableDialog(group); return "OK";
        }
        const std::map<std::string, AnalysisWorkflowKind> analysisWorkflows{
            {"ANALYZE_CORRELATION_MATRIX", AnalysisWorkflowKind::CorrelationMatrix},
            {"ANALYZE_DIMENSIONALITY", AnalysisWorkflowKind::Dimensionality},
            {"ANALYZE_FACTOR_ANALYSIS", AnalysisWorkflowKind::FactorAnalysis},
            {"ANALYZE_SCALE_ANALYSIS", AnalysisWorkflowKind::ScaleAnalysis},
            {"ANALYZE_QUICK_CLUSTER", AnalysisWorkflowKind::QuickCluster},
            {"ANALYZE_ONE_SAMPLE_T", AnalysisWorkflowKind::OneSampleT},
            {"ANALYZE_INDEPENDENT_T", AnalysisWorkflowKind::IndependentT},
            {"ANALYZE_PAIRED_T", AnalysisWorkflowKind::PairedT},
            {"ANALYZE_ONEWAY_ANOVA", AnalysisWorkflowKind::OneWayAnova},
            {"ANALYZE_GLM", AnalysisWorkflowKind::LinearModel},
            {"ANALYZE_LINEAR_MODEL_TRELLIS", AnalysisWorkflowKind::LinearModelTrellis},
            {"ANALYZE_REGRESSION_COMPARISON", AnalysisWorkflowKind::RegressionComparison},
            {"ANALYZE_BINARY_REGRESSION", AnalysisWorkflowKind::BinaryRegression},
            {"ANALYZE_BINARY_REGRESSION_COMPARISON", AnalysisWorkflowKind::BinaryRegressionComparison},
            {"ANALYZE_GENERALIZED_GLM", AnalysisWorkflowKind::GeneralizedLinearModel},
            {"ANALYZE_COUNT_REGRESSION", AnalysisWorkflowKind::CountRegression},
            {"ANALYZE_COUNT_REGRESSION_COMPARISON", AnalysisWorkflowKind::CountRegressionComparison},
            {"ANALYZE_POSITIVE_CONTINUOUS_MODEL", AnalysisWorkflowKind::PositiveContinuousModel},
            {"ANALYZE_POSITIVE_CONTINUOUS_COMPARISON", AnalysisWorkflowKind::PositiveContinuousComparison},
            {"ANALYZE_PROPORTION_MODEL", AnalysisWorkflowKind::ProportionModel},
            {"ANALYZE_PROPORTION_COMPARISON", AnalysisWorkflowKind::ProportionComparison},
            {"ANALYZE_GENERALIZED_COMPARISON", AnalysisWorkflowKind::GeneralizedComparison},
            {"ANALYZE_LINEAR_MIXED_MODEL", AnalysisWorkflowKind::LinearMixedModel},
            {"ANALYZE_GENERALIZED_MIXED_MODEL", AnalysisWorkflowKind::GeneralizedMixedModel}
        };
        if (auto workflow = analysisWorkflows.find(command[0]); workflow != analysisWorkflows.end())
        {
            if (group.empty()) return "ERR no active dataset";
            if (workflow->second == AnalysisWorkflowKind::LinearModelTrellis)
            {
                OpenAnalysisWorkflowDialog(group, workflow->second);
                return "OK";
            }
            return StartAnalysis(group, workflow->second)
                ? "OK" : "ERR analysis could not be started";
        }
        return commandDispatcher_->dispatch(command);
    }

    std::string App::DispatchRequest(std::vector<std::string> const& lines)
    {
        const bool routineTaskPoll = lines.size() == 1 &&
            lines.front() == "MAIN_R_TASKS";
        ::rlispstat::windows::performance::Scope timing(
            "TCP.DispatchRequest", "lines=" + std::to_string(lines.size()),
            !routineTaskPoll);
        if (!commandDispatcher_) return "ERR dispatcher unavailable";
        if (lines.empty()) return "ERR empty command";

        // All R commands, including ADD_PLOT and selection queries, go through
        // the shared dispatcher.  The former lightweight SessionController
        // path discarded time-series and trellis metadata and maintained a
        // second, divergent selection state.
        const auto& command = lines.front();
        auto retainScaleAnalysisLeasesAfterDelivery = [this]() {
            auto leasedScaleAnalysisTasks =
                std::move(pendingMainRTasks_.scaleAnalysisTasks);
            pendingMainRTasks_ = {};
            pendingMainRTasks_.scaleAnalysisTasks =
                std::move(leasedScaleAnalysisTasks);
            pendingMainRTasksAvailable_.store(
                !pendingMainRTasks_.scaleAnalysisTasks.empty(),
                std::memory_order_release);
        };
        if (command == "MAIN_R_TASKS")
        {
            auto reply = ::rlispstat::core::AppendMainRTaskMessages(
                "OK", pendingMainRTasks_);
            retainScaleAnalysisLeasesAfterDelivery();
            return reply;
        }
        if (command == "WELCOME_LAUNCH")
        {
            if (lines.size() < 4) return "ERR malformed Welcome launch request";
            ::rlispstat::core::LaunchRequest launch;
            const bool fromExistingR = lines[1] != "standalone";
            mainRSessionAvailable_.store(fromExistingR, std::memory_order_release);
            launch.environment = !fromExistingR
                ? ::rlispstat::core::LaunchEnvironment::StandaloneApplication
                : ::rlispstat::core::LaunchEnvironment::FromExistingRSession;
            if (lines[3] != "none" && !lines[3].empty()) launch.initialFile = lines[3];
            const auto decision = ::rlispstat::core::DecideLaunch(launch);
            if (decision.shouldShowWelcome) ShowWelcome(lines[2]);
            return "OK";
        }
        if (command == "WELCOME_RECENT_NOTE")
        {
            if (lines.size() >= 2) NoteWelcomeRecentPath(lines[1]);
            return "OK";
        }
        if (command == "WELCOME_INITIAL_OPEN_ERROR")
        {
            ShowWelcome(welcomeVersion_);
            if (welcomeWindow_ && lines.size() >= 3)
                welcomeWindow_->SetStatus(lines.back(), true);
            return "OK";
        }
        if (command == "WELCOME_ACTION_RESULT")
        {
            if (lines.size() >= 3 && lines[2] == "ok") HideWelcome();
            else if (welcomeWindow_)
                welcomeWindow_->SetStatus(lines.size() >= 5 ? lines[4]
                    : "The requested action could not be completed.", true);
            return "OK";
        }
        if (command == "IMPORT_DATA_PREVIEW")
        {
            if (lines.size() < 6) return "ERR malformed import preview";
            char* end = nullptr;
            const long count = std::strtol(lines[4].c_str(), &end, 10);
            if (!end || *end != '\0' || count < 1 ||
                lines.size() < 5 + static_cast<std::size_t>(count))
                return "ERR malformed import preview variables";
            std::vector<std::string> variables(lines.begin() + 5,
                lines.begin() + 5 + count);
            ShowImportColumnChooser(lines[1], lines[2], std::move(variables));
            return "OK";
        }
        if (command == "SCALE_SAVE_TOTAL") {
            if (lines.size() != 3 || (lines[2] != "sum" && lines[2] != "mean")) return "ERR invalid score request";
            auto found = commandDispatcher_->applicationState().scaleAnalyses().find(lines[1]);
            if (found == commandDispatcher_->applicationState().scaleAnalyses().end()) return "ERR unknown scale";
            const auto &state = found->second;
            if (state.rFitPending || !::rlispstat::core::ScaleAnalysisResultMatchesSpecification(state.specification, state.result)) return "ERR scale is updating";
            pendingMainRTasks_.analysisWorkflowTasks.push_back(::rlispstat::core::ScaleTotalScoreSaveTask(
                state.id, state.group, lines[2], state.specification.revision, state.specification.fingerprint));
            MarkMainRTaskPending(); return "OK";
        }
        if(command=="MISSING_PATTERN_ACTION") {
            if(lines.size()!=5)return "ERR malformed missingness action";
            pendingMainRTasks_.analysisWorkflowTasks.push_back(::rlispstat::core::MissingPatternActionTask(lines[1],lines[2],lines[3],lines[4]));
            MarkMainRTaskPending();return "OK";
        }
        if(command=="MISSING_DATA_REPORT") {
            if(lines.size()!=3)return "ERR missing report table ids";
            auto overview=outputViews_.find(lines[1]),descriptives=outputViews_.find(lines[2]);
            if(overview==outputViews_.end() || descriptives==outputViews_.end())return "ERR report tables unavailable";
            overview->second->EmbedDescriptives(descriptives->second);return "OK";
        }
        if (command == "MI_DIAGNOSTICS_ERROR" || command == "MISSING_DATA_ERROR") {
            MessageBoxW(nullptr, to_hstring(lines.size()>1 ? lines[1] : "Diagnostics unavailable.").c_str(),
                command=="MISSING_DATA_ERROR"?L"Missing Data":L"Imputation Diagnostics", MB_OK | MB_ICONWARNING);
            return "OK";
        }
        if (command == "IMPORT_DATA_RESULT")
        {
            if (lines.size() >= 2 && lines[1] == "ok")
            {
                if (lines.size() >= 4) NoteWelcomeRecentPath(lines[3]);
                HideWelcome();
                if (lines.size() >= 3 &&
                    lines[2].find("Review imported variable types:") != std::string::npos)
                    MessageBoxW(nullptr, to_hstring(lines[2]).c_str(), L"Import Data",
                        MB_OK | MB_ICONWARNING);
            }
            else if (lines.size() >= 2 && lines[1] == "cancelled")
            {
                if (welcomeWindow_) welcomeWindow_->SetStatus("Import cancelled.");
            }
            else if (welcomeWindow_)
                welcomeWindow_->SetStatus(lines.size() >= 3 ? lines[2]
                    : "The data file could not be imported.", true);
            return "OK";
        }
        if (command == "OPEN_R_DATA_CHOOSER_V2")
            return ShowRDataFrameChooser(lines);
        if (command == "R_DATA_ASSIGN_RESULT")
        {
            const bool ok = lines.size() >= 3 && lines[2] == "ok";
            const auto message = lines.size() >= 5 ? lines[4]
                : (ok ? "The data were returned to R."
                      : "The data could not be returned to R.");
            MessageBoxW(nullptr, to_hstring(message).c_str(), L"Return Data to R",
                MB_OK | (ok ? MB_ICONINFORMATION : MB_ICONERROR));
            return "OK";
        }
        if (command == "R_DATASET_SYNC_RESULT")
        {
            const bool ok = lines.size() >= 3 && lines[2] == "ok";
            if (!ok)
            {
                if (lines.size() >= 4) datasetRecoveryPending_.erase(lines[3]);
                const auto message = lines.size() >= 5 ? lines[4]
                    : "The edited dataset could not be synchronized with R.";
                LogStartup("Dataset synchronization failed: " + message);
            }
            return "OK";
        }
        if (command == "MULTIPLE_IMPUTATION_RESULT")
        {
            const bool ok = lines.size() >= 2 && lines[1] == "ok";
            const auto group = lines.size() >= 3 ? lines[2] : "";
            const auto message = lines.size() >= 4 ? lines[3]
                : (ok ? "The imputed dataset was created."
                      : "The imputation could not be completed.");
            if (ok && !group.empty()) ShowDataSheet(group);
            MessageBoxW(nullptr, to_hstring(message).c_str(),
                ok ? L"Multiple Imputation" : L"Imputation Failed",
                MB_OK | (ok ? MB_ICONINFORMATION : MB_ICONERROR));
            return "OK";
        }
        if (command == "WORKBENCH_MESSAGE")
        {
            const auto title = lines.size() >= 2 ? lines[1] : "LinkEDA";
            const auto message = lines.size() >= 3 ? lines[2] : "";
            MessageBoxW(welcomeWindow_ ? welcomeWindow_->NativeHandle() : nullptr,
                to_hstring(message).c_str(), to_hstring(title).c_str(),
                MB_OK | MB_ICONINFORMATION);
            return "OK";
        }
        if (command == "TABLE1_OPEN_ERROR" && lines.size() >= 4 && lines[3] != "REQUEST_V1")
        {
            auto dialog = descriptiveDialogs_.find(lines[2]);
            if (dialog != descriptiveDialogs_.end() && dialog->second)
                dialog->second->ReportError(lines[3]);
            return "OK";
        }
        if (command.rfind("ANALYZE_", 0) == 0 && lines.size() <= 2 &&
            command != "ANALYZE_CONTINGENCY_TABLE")
            return DispatchApplicationCommand(lines);
        if (command.rfind("PLOT_NEW_", 0) == 0 && lines.size() <= 3)
            return DispatchApplicationCommand(lines);
        if (command.rfind("SNAPSHOT_ALBUM_", 0) == 0)
            return DispatchApplicationCommand(lines);
        if (command == "PLOT_NEW_LINKED_SCATTER_MATRIX" ||
            command == "PLOT_NEW_PARALLEL_COORDINATES")
        {
            const std::string group = lines.size() > 1 && !lines[1].empty()
                ? lines[1]
                : commandDispatcher_->applicationState().datasets().activeDatasetGroup();
            if (group.empty()) return "ERR no active dataset";
            std::vector<std::string> variables;
            std::size_t variableStart = 2;
            bool standardize = true, connect = true;
            if (command == "PLOT_NEW_PARALLEL_COORDINATES" && lines.size() >= 4)
            {
                standardize = lines[2] != "FALSE";
                connect = lines[3] != "FALSE";
                variableStart = 4;
            }
            for (std::size_t index = variableStart; index < lines.size(); ++index)
                if (!lines[index].empty()) variables.push_back(lines[index]);
            const auto id = command == "PLOT_NEW_LINKED_SCATTER_MATRIX"
                ? CreateScatterMatrix(group, variables)
                : CreateParallelCoordinates(group, variables, standardize, connect);
            return id.empty() ? "ERR at least two numeric variables are required" : "OK\t" + id;
        }
        if (command == "ANALYZE_CONTINGENCY_TABLE")
        {
            const std::string group = lines.size() > 1 && !lines[1].empty()
                ? lines[1]
                : commandDispatcher_->applicationState().datasets().activeDatasetGroup();
            if (group.empty()) return "ERR no active dataset";
            if (lines.size() >= 4 && !lines[2].empty() && !lines[3].empty())
                CreateContingencyTable({ group, lines[2], lines[3] });
            else
            {
                ::rlispstat::core::AnalysisSpecification existing{
                    { "row", {} }, { "column", {} }
                };
                if (lines.size() >= 3 && !lines[2].empty())
                    existing["row"] = { lines[2] };
                if (lines.size() >= 4 && !lines[3].empty())
                    existing["column"] = { lines[3] };
                OpenContingencyTableDialog(group,
                    lines.size() >= 3 ? &existing : nullptr);
            }
            return "OK";
        }
        if (command == "OPEN_LINEAR_MODEL_TRELLIS" ||
            command == "ANALYZE_LINEAR_MODEL_TRELLIS")
        {
            const std::string group = lines.size() > 1 && !lines[1].empty()
                ? lines[1]
                : commandDispatcher_->applicationState().datasets().activeDatasetGroup();
            if (group.empty()) return "ERR no active dataset";
            std::string response, rowCondition, columnCondition, adjustment = "holm";
            std::vector<std::string> terms;
            if (lines.size() < 7)
            {
                OpenAnalysisWorkflowDialog(group, AnalysisWorkflowKind::LinearModelTrellis);
                return "OK";
            }
            if (lines.size() >= 7)
            {
                response = lines[2]; rowCondition = lines[3]; columnCondition = lines[4];
                adjustment = lines[5];
                const auto count = static_cast<std::size_t>(std::max(0, std::atoi(lines[6].c_str())));
                for (std::size_t index = 0; index < count && 7 + index < lines.size(); ++index)
                    terms.push_back(lines[7 + index]);
            }
            const auto id = CreateModelTrellis(group, response, terms, rowCondition,
                                               columnCondition, adjustment);
            if (id.empty()) return "ERR linear model trellis specification is not valid";
            auto reply = std::string("OK\t") + id;
            reply = ::rlispstat::core::AppendMainRTaskMessages(reply, pendingMainRTasks_);
            retainScaleAnalysisLeasesAfterDelivery();
            return reply;
        }
        if (command == "MODEL_UPDATE_ERROR" && lines.size() >= 3)
        {
            const std::string& group = lines[1];
            const bool datasetMissing =
                lines[2].find("is not registered") != std::string::npos;
            const bool recoveryAlreadyAttempted =
                datasetRecoveryPending_.find(group) != datasetRecoveryPending_.end();

            // First let the shared model state consume the failed result so
            // rFitPending is cleared.  If R restarted while the native windows
            // survived, repopulate its registry from ApplicationState and
            // retry exactly once.  The task encoder emits dataset sync tasks
            // before model tasks, so the retry cannot overtake recovery.
            auto reply = commandDispatcher_->dispatch(lines);
            if (datasetMissing && !recoveryAlreadyAttempted)
            {
                auto const* dataframe =
                    commandDispatcher_->applicationState().datasets().find(group);
                if (dataframe && QueueDatasetSyncToR(*dataframe))
                {
                    datasetRecoveryPending_.insert(group);
                    auto model = commandDispatcher_->applicationState()
                        .groupModels().find(group);
                    if (model != commandDispatcher_->applicationState()
                        .groupModels().end())
                    {
                        ::rlispstat::core::MarkGroupModelChanged(model->second);
                        QueueLinearModelFit(group);
                    }
                }
            }
            else if (recoveryAlreadyAttempted)
            {
                // Do not recurse forever if recovery itself failed.  A later
                // explicit model request may make one fresh recovery attempt.
                datasetRecoveryPending_.erase(group);
            }
            return reply;
        }
        auto reply = commandDispatcher_->dispatch(lines);
        if (command == "MODEL_UPDATE" && lines.size() >= 2)
            datasetRecoveryPending_.erase(lines[1]);
        if (command == "SCALE_ANALYSIS_UPDATE" &&
            reply.rfind("OK", 0) == 0 && lines.size() >= 7)
        {
            ::rlispstat::core::AcknowledgeMainRScaleAnalysisTask(
                pendingMainRTasks_.scaleAnalysisTasks,
                lines[1], std::atoi(lines[5].c_str()), lines[6]);
        }
        if (::rlispstat::core::MainRReplyAcceptsTaskAppend(reply))
        {
            reply = ::rlispstat::core::AppendMainRTaskMessages(reply, pendingMainRTasks_);
            retainScaleAnalysisLeasesAfterDelivery();
        }
        if (reply.rfind("ERR ", 0) == 0)
            LogStartup("CommandDispatcher reply: " + reply);
        return reply;
    }

    bool App::HasPendingMainRTasks() const noexcept
    {
        return pendingMainRTasksAvailable_.load(std::memory_order_acquire);
    }

    void App::MarkMainRTaskPending() noexcept
    {
        pendingMainRTasksAvailable_.store(true, std::memory_order_release);
    }

    void App::ShowScatterPlot(::rlispstat::core::SessionPlot const& plot)
    {
        std::shared_ptr<ScatterPlotView> view;
        auto existing = views_.find(plot.id);
        if (existing != views_.end())
        {
            view = existing->second;
        }
        else
        {
            view = ScatterPlotView::Create();
        }

        view->SetSelectionCallback([this](std::string const& group,
                                          std::set<int> const& rows,
                                          ::rlispstat::core::SelectionMode mode)
        {
            ApplySelectedRows(group, rows, mode);
        });
        view->SetCommandCallback([this](std::vector<std::string> const& command)
        {
            HandleAnalysisViewCommand(command);
        });
        const auto plotId = plot.id;
        view->SetClosedCallback([this, plotId]()
        {
            HandleViewClosed(plotId);
        });
        const std::string windowToken = "plot:" + plot.id;
        view->SetApplicationCommands(applicationCommands_, windowToken);
        std::weak_ptr<ScatterPlotView> weakView = view;
        if (applicationCommands_)
        {
            applicationCommands_->RegisterWindow({
                windowToken,
                to_hstring("Scatterplot: " + plot.title).c_str(),
                plot.group,
                "plot",
                plot.id,
                [weakView]() { if (auto current = weakView.lock()) current->Activate(); },
                [weakView]() { if (auto current = weakView.lock()) current->Close(); },
                {},
                {},
                {},
                {}
            });
        }
        views_[plot.id] = view;
        auto const* dataframe = commandDispatcher_
            ? commandDispatcher_->applicationState().datasets().find(plot.group)
            : nullptr;
        view->SetDataFrame(dataframe);
        auto nativePlot = plots_.find(plot.id);
        if (nativePlot != plots_.end() && nativePlot->second)
            view->Show(*nativePlot->second);
        else
            view->Show(plot);
        if (commandDispatcher_)
        {
            view->SetAnalysisScope(commandDispatcher_->applicationState()
                .activeAnalysisScope(plot.group));
            view->SetVisualStyle(commandDispatcher_->plotTheme(),
                commandDispatcher_->applicationState().displayPointColors(plot.group));
            const std::string labelColumn = commandDispatcher_->applicationState().labelColumn(plot.group);
            std::map<int, std::string> labels;
            if (dataframe)
            {
                auto found = std::find_if(dataframe->columns.begin(), dataframe->columns.end(),
                    [&](auto const& column) { return column.name == labelColumn; });
                if (found != dataframe->columns.end())
                    labels = ::rlispstat::core::RowLabelMapForColumn(*found);
            }
            view->SetLabelState(labelColumn, std::move(labels));
        }

        const auto selection = commandDispatcher_
            ? commandDispatcher_->plotCoordinator().selectionSnapshot(plot.group)
            : ::rlispstat::core::PlotCoordinationResult{};
        view->RefreshSelection(selection.event.selectedRows);
        view->Activate();

        LogStartup("Rendered " + std::to_string(plot.points.size())
            + " scatter points in view " + plot.id
            + " through CommandDispatcher/ApplicationState");
    }

    void App::ShowNativePlot(::rlispstat::core::PlotModel& plot)
    {
        std::shared_ptr<ScatterPlotView> view;
        auto existing = views_.find(plot.id);
        view = existing != views_.end() && existing->second
            ? existing->second : ScatterPlotView::Create();
        view->SetSelectionCallback([this](std::string const& group,
                                          std::set<int> const& rows,
                                          ::rlispstat::core::SelectionMode mode)
        {
            ApplySelectedRows(group, rows, mode);
        });
        view->SetCommandCallback([this](std::vector<std::string> const& command)
        {
            HandleAnalysisViewCommand(command);
        });
        const std::string plotId = plot.id;
        view->SetClosedCallback([this, plotId]() { HandleViewClosed(plotId); });
        const std::string token = "plot:" + plot.id;
        view->SetApplicationCommands(applicationCommands_, token);
        std::weak_ptr<ScatterPlotView> weakView = view;
        if (applicationCommands_)
        {
            applicationCommands_->RegisterWindow({
                token,
                to_hstring(::rlispstat::core::TitleForPlot(plot)).c_str(),
                plot.group,
                "plot",
                plot.id,
                [weakView]() { if (auto current = weakView.lock()) current->Activate(); },
                [weakView]() { if (auto current = weakView.lock()) current->Close(); },
                {},
                {},
                {},
                {}
            });
        }
        views_[plot.id] = view;
        auto const* dataframe = commandDispatcher_
            ? commandDispatcher_->applicationState().datasets().find(plot.group)
            : nullptr;
        view->SetDataFrame(dataframe);
        view->Show(plot);
        if (commandDispatcher_)
        {
            view->SetAnalysisScope(commandDispatcher_->applicationState()
                .activeAnalysisScope(plot.group));
            view->SetVisualStyle(commandDispatcher_->plotTheme(),
                commandDispatcher_->applicationState().displayPointColors(plot.group));
            const std::string labelColumn = commandDispatcher_->applicationState().labelColumn(plot.group);
            std::map<int, std::string> labels;
            if (dataframe)
            {
                auto found = std::find_if(dataframe->columns.begin(), dataframe->columns.end(),
                    [&](auto const& column) { return column.name == labelColumn; });
                if (found != dataframe->columns.end())
                    labels = ::rlispstat::core::RowLabelMapForColumn(*found);
            }
            view->SetLabelState(labelColumn, std::move(labels));
            const auto selection = commandDispatcher_->plotCoordinator().selectionSnapshot(plot.group);
            if (selection.accepted) view->RefreshSelection(selection.event.selectedRows);
        }
        view->Activate();
        LogStartup("Rendered native " + plot.kind + " view " + plot.id +
            " through shared core render plans");
    }

    bool App::OpenPooledRegressionInteractionPlot(
        std::string const& group,
        std::string const& analysisId,
        std::string const& response,
        std::string const& term,
        std::vector<std::string> const& payload,
        std::string& message)
    {
        const bool simpleEffect = term.find(':') == std::string::npos;
        if (!commandDispatcher_)
        {
            message = "The application state is unavailable.";
            return false;
        }
        auto const* dataframe =
            commandDispatcher_->applicationState().datasets().find(group);
        if (!dataframe)
        {
            message = "The source dataset is no longer available.";
            return false;
        }
        ::rlispstat::core::RegressionInteractionPlotResult pooled;
        if (!::rlispstat::core::ParseRegressionInteractionPlotRResult(
                payload, pooled))
        {
            message = pooled.message;
            return false;
        }
        ::rlispstat::core::PopulateRegressionInteractionLegendRows(
            pooled, *dataframe);
        ::rlispstat::core::PlotModel* existing = nullptr;
        for (auto& [plotId, candidate] : plots_)
            if (candidate && candidate->isRegressionDerivedPlot &&
                candidate->glmModelId == analysisId)
            {
                existing = candidate.get();
                break;
            }
        auto plot = existing
            ? std::unique_ptr<::rlispstat::core::PlotModel>{}
            : std::make_unique<::rlispstat::core::PlotModel>();
        auto* pointer = existing ? existing : plot.get();
        const bool preservedIntervalsVisible = existing
            ? existing->regressionConfidenceIntervalsVisible : false;
        const bool preservedLevelVisible = existing
            ? existing->regressionConfidenceLevelVisible : true;
        const bool preservedConnectEstimates = existing
            ? existing->regressionConnectEstimates : true;
        const std::string preservedLegendPosition = existing
            ? existing->interactionLegendPosition : "right";
        const bool preservedCustomLegendPosition = existing
            ? existing->interactionLegendUsesCustomPosition : false;
        const double preservedLegendX = existing
            ? existing->interactionLegendX : 0.72;
        const double preservedLegendY = existing
            ? existing->interactionLegendY : 0.10;
        const std::string preservedLegendTitleOverride = existing
            ? existing->interactionLegendTitleOverride : std::string();
        const bool preservedLegendTitleVisible = existing
            ? existing->interactionLegendTitleVisible : true;
        const std::string preservedId = existing ? existing->id :
            (simpleEffect ? "pooled_effect_" : "pooled_interaction_") + group + "_" +
            std::to_string(static_cast<unsigned long long>(GetTickCount64()));
        ::rlispstat::core::PopulateDatasetSeedPlot(*pointer, *dataframe, group);
        pointer->regressionConfidenceIntervalsVisible = preservedIntervalsVisible;
        pointer->regressionConfidenceLevelVisible = preservedLevelVisible;
        pointer->regressionConnectEstimates = preservedConnectEstimates;
        pointer->interactionLegendPosition = preservedLegendPosition;
        pointer->interactionLegendUsesCustomPosition = preservedCustomLegendPosition;
        pointer->interactionLegendX = preservedLegendX;
        pointer->interactionLegendY = preservedLegendY;
        pointer->interactionLegendTitleOverride = preservedLegendTitleOverride;
        pointer->interactionLegendTitleVisible = preservedLegendTitleVisible;
        pointer->id = preservedId;
        pointer->kind = "glm_interaction";
        pointer->isDatasetSeed = false;
        pointer->isGLMDiagnostic = false;
        pointer->isGLMInteractionPlot = true;
        pointer->isRegressionDerivedPlot = true;
        pointer->glmModelId = analysisId;
        pointer->glmInteractionTerm = term;
        pointer->group = group;
        pointer->points.clear();
        pointer->interactionPlotLines = pooled.lines;
        pointer->interactionXTicks = pooled.xTicks;
        pointer->interactionXTickRows = pooled.xTickCaseIds;
        ::rlispstat::core::ApplyRegressionInteractionPresentation(
            *pointer, pooled, response);
        ::rlispstat::core::ApplyRegressionInteractionVariableLabels(
            *pointer, pooled, *dataframe, response);
        pointer->overlays.clear();
        for (auto const& line : pointer->interactionPlotLines)
            pointer->points.insert(pointer->points.end(), line.points.begin(), line.points.end());
        if (pointer->points.empty())
        {
            message = "R returned no finite pooled fitted values for the effect plot.";
            return false;
        }
        if (auto dependency = regressionDerivedOutputs_.find(analysisId + "|plot");
            dependency != regressionDerivedOutputs_.end())
        {
            pointer->regressionDerivedKind = dependency->second.outputKind;
            pointer->regressionDerivedSourceKind = dependency->second.sourceKind;
            pointer->regressionDerivedSourceModelId = dependency->second.sourceModelId;
            pointer->regressionDerivedTerm = term;
            pointer->regressionDerivedSourceRevision = dependency->second.sourceRevision;
            pointer->regressionDerivedSourceFitVersion = dependency->second.sourceFitVersion;
        }
        auto const& applicationState = commandDispatcher_->applicationState();
        pointer->rExportTheme = commandDispatcher_->plotTheme();
        auto const* source = pointer->regressionDerivedSourceModelId.empty()
            ? applicationState.outputCodeReference(analysisId)
            : applicationState.regressionSourceOutputCodeReference(
                pointer->regressionDerivedSourceKind,
                pointer->regressionDerivedSourceModelId);
        if (source)
        {
            pointer->codeReference =
                ::rlispstat::core::RegressionInteractionPlotCodeReference(
                    *pointer, pooled, *source);
            commandDispatcher_->applicationState().registerOutputCodeReference(
                pointer->codeReference);
        }
        ::rlispstat::core::ComputeRanges(*pointer);
        ::rlispstat::core::ApplyRegressionInteractionAxisRange(*pointer);
        if (existing)
        {
            auto view = views_.find(existing->id);
            if (view != views_.end() && view->second) view->second->Show(*existing);
            return true;
        }
        const std::string plotId = pointer->id;
        plots_[plotId] = std::move(plot);
        const auto attached = AttachPlotModel(*pointer);
        if (!attached.accepted)
        {
            plots_.erase(plotId);
            message = "Could not register the pooled effect plot.";
            return false;
        }
        commandDispatcher_->plotCoordinator().activatePlot(plotId);
        ShowNativePlot(*pointer);
        return true;
    }

    bool App::OpenPooledRegressionPartialPlot(
        std::string const& group,
        std::string const& analysisId,
        std::string const& term,
        std::vector<std::string> const& payload,
        std::string& message)
    {
        if (!commandDispatcher_)
        {
            message = "The application state is unavailable.";
            return false;
        }
        auto const* dataframe =
            commandDispatcher_->applicationState().datasets().find(group);
        if (!dataframe)
        {
            message = "The source dataset is no longer available.";
            return false;
        }
        ::rlispstat::core::RegressionPartialPlotResult result;
        if (!::rlispstat::core::ParseRegressionPartialPlotRResult(
                payload, result))
        {
            message = result.message;
            return false;
        }
        ::rlispstat::core::PlotModel* existing = nullptr;
        for (auto& [plotId, candidate] : plots_)
            if (candidate && candidate->isRegressionDerivedPlot &&
                candidate->regressionDerivedKind == "partial_regression_plot" &&
                candidate->glmModelId == analysisId)
            {
                existing = candidate.get();
                break;
            }
        // A predictor change replaces the requesting window only after R succeeds.
        for (auto& [plotId, candidate] : plots_)
            if (candidate && candidate->regressionPartialPendingAnalysisId == analysisId) {
                existing = candidate.get();
                if (existing->glmModelId != analysisId)
                    regressionDerivedOutputs_.erase(existing->glmModelId + "|plot");
                break;
            }
        auto plot = existing
            ? std::unique_ptr<::rlispstat::core::PlotModel>{}
            : std::make_unique<::rlispstat::core::PlotModel>();
        auto* pointer = existing ? existing : plot.get();
        const std::string preservedId = existing ? existing->id :
            "pooled_partial_" + group + "_" +
            std::to_string(static_cast<unsigned long long>(GetTickCount64()));
        const int preservedImputation = existing
            ? existing->diagnosticImputationIndex : 1;
        const bool preservedUncertainty = existing
            ? existing->diagnosticShowImputationUncertainty :
              ::rlispstat::core::DiagnosticImputationUncertaintyShouldBeDefault(
                  *dataframe, "partial_regression");
        ::rlispstat::core::PopulateDatasetSeedPlot(*pointer, *dataframe, group);
        pointer->id = preservedId;
        pointer->kind = "scatter";
        pointer->isDatasetSeed = false;
        pointer->isGLMDiagnostic = true;
        pointer->isGLMInteractionPlot = false;
        pointer->isRegressionDerivedPlot = true;
        pointer->regressionDerivedKind = "partial_regression_plot";
        pointer->glmModelId = analysisId;
        pointer->glmDiagnosticKind = "partial_regression";
        pointer->regressionDerivedTerm = term;
        pointer->group = group;
        pointer->diagnosticImputationIndex = std::clamp(
            preservedImputation, 1, std::max(1, result.imputationCount));
        pointer->diagnosticImputationCount = std::max(1, result.imputationCount);
        pointer->diagnosticShowImputationUncertainty = preservedUncertainty;
        pointer->regressionPartialPointsByImputation = result.pointsByImputation;
        pointer->regressionPartialResidualType = result.residualType;
        pointer->regressionPartialContributionScale = result.contributionScale;
        pointer->regressionPartialAvailableTerms = result.availableTerms;
        pointer->diagnosticAvailableResidualTypes = result.availableResidualTypes;
        pointer->regressionPartialPendingAnalysisId.clear();
        pointer->displayedResidualType = result.residualType;
        if (auto dependency = regressionDerivedOutputs_.find(analysisId + "|plot");
            dependency != regressionDerivedOutputs_.end())
        {
            pointer->regressionDerivedSourceKind = dependency->second.sourceKind;
            pointer->regressionDerivedSourceModelId = dependency->second.sourceModelId;
            pointer->regressionDerivedSourceRevision = dependency->second.sourceRevision;
            pointer->regressionDerivedSourceFitVersion = dependency->second.sourceFitVersion;
            pointer->displayedFitVersion = dependency->second.sourceFitVersion;
            pointer->displayedDiagnosticsVersion = dependency->second.sourceFitVersion;
            pointer->generalizedDiagnosticResiduals =
                dependency->second.sourceKind.rfind("generalized", 0) == 0;
        }
        if (!ApplyDiagnosticPlotData(
                *pointer, ::rlispstat::core::BuildRegressionPartialPlotData(
                    result, pointer->diagnosticImputationIndex,
                    pointer->diagnosticShowImputationUncertainty)))
        {
            message = "R returned no finite partial-regression points.";
            return false;
        }
        auto& application = commandDispatcher_->applicationState();
        if (const auto* source = application.regressionSourceOutputCodeReference(
                pointer->regressionDerivedSourceKind, pointer->regressionDerivedSourceModelId)) {
            pointer->codeReference = ::rlispstat::core::RegressionPartialPlotCodeReference(
                *pointer, result, *source);
            application.registerOutputCodeReference(pointer->codeReference);
        }
        if (existing)
        {
            auto view = views_.find(existing->id);
            if (view != views_.end() && view->second) view->second->Show(*existing);
            return true;
        }
        const std::string plotId = pointer->id;
        plots_[plotId] = std::move(plot);
        const auto attached = AttachPlotModel(*pointer);
        if (!attached.accepted)
        {
            plots_.erase(plotId);
            message = "Could not register the partial regression plot.";
            return false;
        }
        commandDispatcher_->plotCoordinator().activatePlot(plotId);
        ShowNativePlot(*pointer);
        return true;
    }

    void App::ShowDataSheet(std::string const& group)
    {
        if (!commandDispatcher_ || group.empty()) return;
        auto const* dataframe = commandDispatcher_->applicationState().datasets().find(group);
        if (!dataframe) return;
        HideWelcome();

        std::shared_ptr<DataSheetView> view;
        auto existing = dataSheets_.find(group);
        if (existing != dataSheets_.end() && existing->second)
        {
            view = existing->second;
        }
        else
        {
            view = DataSheetView::Create();
            view->SetSelectionCallback([this](std::string const& selectedGroup,
                                              std::set<int> const& rows,
                                              ::rlispstat::core::SelectionMode mode)
            {
                ApplySelectedRows(selectedGroup, rows, mode);
            });
            view->SetClosedCallback([this, group]()
            {
                HandleDataSheetClosed(group);
            });
            view->SetImputationDisplayCallback(
                [this](std::string const& selectedGroup, std::string const& mode)
                {
                    SetImputationDisplayMode(selectedGroup, mode);
                });
            view->SetCommandCallback([this](std::vector<std::string> const& command)
            {
                return DispatchApplicationCommand(command);
            });
            view->SetNoteChangedCallback(
                [this](std::string const& selectedGroup,
                       std::optional<::rlispstat::core::WindowNote> const& note)
                {
                    if (commandDispatcher_)
                        commandDispatcher_->applicationState().setDocumentNote(selectedGroup, note);
                });
            dataSheets_[group] = view;
        }

        const std::string windowToken = "data:" + group;
        view->SetApplicationMenu(applicationCommands_, windowToken);
        std::weak_ptr<DataSheetView> weakView = view;
        if (applicationCommands_)
        {
            applicationCommands_->RegisterWindow({
                windowToken,
                to_hstring("Data: " + group).c_str(),
                group,
                "data",
                {},
                [weakView]() { if (auto current = weakView.lock()) current->Activate(); },
                [weakView]() { if (auto current = weakView.lock()) current->Close(); },
                [this, group]() { OpenScatterplotDialog(group); },
                [this, group]() {
                    OpenPlotWorkflowDialog(group, PlotWorkflowKind::ScatterMatrix);
                },
                [this, group]() {
                    OpenPlotWorkflowDialog(group, PlotWorkflowKind::ParallelCoordinates);
                },
                [this, group]() { OpenDescriptiveStatisticsDialog(group); },
                [this, group]() { CreateModelTrellis(group); },
                [this, group]() { OpenContingencyTableDialog(group); }
            });
        }

        view->SetLabelColumn(commandDispatcher_->applicationState().labelColumn(group));
        view->Show(*dataframe);
        view->SetAnalysisScope(
            commandDispatcher_->applicationState().activeAnalysisScope(group));
        view->SetDocumentNote(commandDispatcher_->applicationState().documentNote(group));
        view->RefreshRowColors(commandDispatcher_->applicationState().displayPointColors(group));
        std::set<int> selected;
        commandDispatcher_->applicationState().selectedRows(group, selected);
        view->RefreshSelection(selected);
        view->Activate();
        LogStartup("Opened data sheet for " + group + ": "
            + std::to_string(dataframe->rows) + " rows, "
            + std::to_string(dataframe->columns.size()) + " columns");
    }

    void App::ShowVariablesWindow(std::string const& group, bool activate)
    {
        if (!commandDispatcher_ || group.empty()) return;
        auto const* dataframe = commandDispatcher_->applicationState().datasets().find(group);
        if (!dataframe) return;
        HideWelcome();

        std::shared_ptr<VariablesWindow> view;
        auto existing = variableViews_.find(group);
        if (existing != variableViews_.end() && existing->second)
        {
            view = existing->second;
        }
        else
        {
            view = VariablesWindow::Create();
            view->SetClosedCallback([this, group]()
            {
                HandleVariablesWindowClosed(group);
            });
            view->SetCommandCallback([this](std::vector<std::string> const& command)
            {
                if (!commandDispatcher_) return std::string("ERR command dispatcher is unavailable");
                return commandDispatcher_->dispatch(command);
            });
            variableViews_[group] = view;
        }

        std::map<std::string, std::string> roles;
        auto const& models = commandDispatcher_->applicationState().groupModels();
        auto model = models.find(group);
        for (auto const& column : dataframe->columns)
        {
            std::string role = "none";
            if (model != models.end())
            {
                if (model->second.response == column.name) role = "dependent";
                else if (::rlispstat::core::HasModelTerm(model->second, column.name)) role = "independent";
            }
            roles[column.name] = role;
        }

        const std::string token = "variables:" + group;
        std::weak_ptr<VariablesWindow> weakView = view;
        if (applicationCommands_)
        {
            applicationCommands_->RegisterWindow({
                token,
                to_hstring(::rlispstat::core::VariableViewWindowTitle(group)).c_str(),
                group,
                "data",
                {},
                [weakView]() { if (auto current = weakView.lock()) current->Activate(); },
                [weakView]() { if (auto current = weakView.lock()) current->Close(); },
                {}, {}, {}, {}, {}, {}
            });
        }
        view->Show(*dataframe, roles, activate);
    }

    void App::HandleVariablesWindowClosed(std::string const& group)
    {
        variableViews_.erase(group);
        if (applicationCommands_) applicationCommands_->UnregisterWindow("variables:" + group);
    }

    void App::HandleVariableTypeChanged(
        std::string const& group, std::string const& variable,
        std::string const& type,
        ::rlispstat::core::VariableTypeChangeEffects const& effects)
    {
        (void)type;
        if (!commandDispatcher_) return;
        auto const* dataframe = commandDispatcher_->applicationState().datasets().find(group);
        if (!dataframe) return;

        // A variable type is dataset metadata, not a model-local override.
        // Keep R's registered dataset in step with the canonical Windows
        // ApplicationState before any affected analysis is refitted. Without
        // this, R fitted the requested factor coding but then silently
        // registered its older numeric copy of the dataset, leaving Variable
        // View and the analysis report visibly contradictory.
        // Dataset-sync tasks are serialized ahead of model-fit tasks in the
        // same MAIN_R_TASKS batch.
        if (!QueueDatasetSyncToR(*dataframe))
            LogStartup("Could not queue variable-type synchronization for " + group);

        // The registered data frame remains the source of truth. Refresh the variable
        // catalog embedded in plot models without round-tripping through R.
        for (auto& [id, plot] : plots_)
            if (plot && plot->group == group)
                ::rlispstat::core::SyncPlotVariablesFromDataFrame(*plot, *dataframe);

        // Plot menus are native, cached flyouts.  Refresh their variable
        // catalog immediately; rebuilding the portable metadata alone leaves
        // an already-open menu classifying the variable by its former type.
        // A Trellis that actively conditions on this variable was rebuilt in
        // ApplicationState and also needs one visual redraw so its panels
        // switch between categorical levels and numeric bins at once.
        const std::set<std::string> rebuiltTrellisPlots(
            effects.trellisPlotIds.begin(), effects.trellisPlotIds.end());
        for (auto& [id, plot] : plots_)
        {
            if (!plot || plot->group != group) continue;
            auto view = views_.find(id);
            if (view == views_.end() || !view->second) continue;
            view->second->RefreshVariableMetadata(dataframe);
            if (rebuiltTrellisPlots.count(id))
            {
                view->second->Show(*plot);
                view->second->SetVisualStyle(commandDispatcher_->plotTheme(),
                    commandDispatcher_->applicationState().displayPointColors(group));
            }
        }

        auto variables = variableViews_.find(group);
        if (variables != variableViews_.end() && variables->second)
            ShowVariablesWindow(group);

        // Existing analyses retain their shared model state. Refit only analyses whose
        // admissible-variable set or coding changed, matching the macOS propagation path.
        for (auto const& id : effects.correlationIds) {
            auto found=commandDispatcher_->applicationState().correlationMatrices().find(id);
            if(found!=commandDispatcher_->applicationState().correlationMatrices().end()) QueueCorrelationFit(found->second);
            if(correlationViews_.count(id)) ShowCorrelationMatrix(id);
        }
        for (auto const& id : effects.dimensionalityIds)
            if (dimensionalityViews_.count(id)) RefitDimensionality(id);
        for (auto const& id : effects.scaleAnalysisIds)
        {
            auto current = commandDispatcher_->applicationState().scaleAnalyses().find(id);
            if (current == commandDispatcher_->applicationState().scaleAnalyses().end()) continue;
            if (current->second.autoFit && current->second.specification.items.size() >= 2)
                RefitScaleAnalysis(id);
            else if (scaleAnalysisViews_.count(id)) ShowScaleAnalysis(id);
        }
        for (auto const& id : effects.dendrogramIds)
            if (dendrogramViews_.count(id)) RefitDendrogram(id);
        for (auto const& id : effects.generalizedGlmIdsToRefit)
            RequestGeneralizedModelFit(id);
        for (auto const& id : effects.generalizedGlmIds)
            if (generalizedModelViews_.count(id)) ShowGeneralizedModel(id);
        for (auto const& id : effects.regressionComparisonIds)
        {
            auto found = commandDispatcher_->applicationState().regressionComparisons().find(id);
            if (found == commandDispatcher_->applicationState().regressionComparisons().end()) continue;
            if (found->second.autoRefit) QueueRegressionComparison(found->second);
            if (regressionComparisonViews_.count(id)) ShowRegressionComparison(id);
        }
        for (auto const& id : effects.generalizedComparisonIds)
        {
            auto found = commandDispatcher_->applicationState().generalizedComparisons().find(id);
            if (found == commandDispatcher_->applicationState().generalizedComparisons().end()) continue;
            if (found->second.autoRefit) QueueGeneralizedComparison(found->second);
            if (generalizedComparisonViews_.count(id)) ShowGeneralizedComparison(id);
        }
        for (auto const& id : effects.modelTrellisIds)
        {
            auto found = commandDispatcher_->applicationState().modelTrellises().find(id);
            if (found == commandDispatcher_->applicationState().modelTrellises().end()) continue;
            QueueModelTrellisFit(found->second);
            if (modelTrellisViews_.count(id)) ShowModelTrellis(id);
        }
        for (auto const& id : effects.mixedModelIds)
        {
            auto found = commandDispatcher_->applicationState().nativeMixedModels().find(id);
            if (found == commandDispatcher_->applicationState().nativeMixedModels().end() ||
                !mixedModelViews_.count(id)) continue;
            auto& tasks = pendingMainRTasks_.mixedModelTasks;
            tasks.erase(std::remove_if(tasks.begin(), tasks.end(),
                [&](auto const& task) { return task.state.id == id; }), tasks.end());
            tasks.push_back({found->second});
            MarkMainRTaskPending();
        }
        for (auto const& [modelId, view] : linearModelViews_) {
            auto model = commandDispatcher_->applicationState().groupModels().find(modelId);
            if (view && model != commandDispatcher_->applicationState().groupModels().end() &&
                model->second.group == group && model->second.isStale) {
                if (model->second.autoRefit) QueueLinearModelFit(modelId);
                else ShowLinearModel(modelId);
            }
        }

        // Descriptive tables and contingency tables also interpret the
        // canonical variable type.  Cell edits already refreshed these
        // windows through HandleDatasetMutation; metadata-only edits must do
        // the same or an open report keeps the result produced for the old
        // type until it is closed and recreated.
        std::vector<std::string> affectedOutputIds;
        for (auto const& [id, state] : outputStates_)
        {
            if (state.datasetId != group) continue;
            if (state.groupVariable == variable ||
                std::find(state.variables.begin(), state.variables.end(), variable) !=
                    state.variables.end())
                affectedOutputIds.push_back(id);
        }
        for (auto const& id : affectedOutputIds)
        {
            auto found = outputStates_.find(id);
            if (found == outputStates_.end()) continue;
            auto state = found->second;
            commandDispatcher_->applicationState().captureAnalysisScope(state.datasetId, state.dataScope, state.dataScopeCaptured);
            if (state.tableType == "nested_contingency")
            {
                auto rebuilt = ::rlispstat::core::NestedContingencyTableStateForDataFrame(
                    *dataframe, state.id, state.variables, state.groupVariable,
                    state.nestedDisplayMode,
                    state.dataScopeCaptured ? &state.dataScope : nullptr);
                ShowTable1(rebuilt);
            }
            else if (state.tableType == "table1")
            {
                ::rlispstat::core::MainRTable1Task task;
                task.id = state.id; task.group = state.datasetId;
                task.variables = state.variables; task.groupVariable = state.groupVariable;
                task.variableTypes = Table1CanonicalVariableTypes(
                    *dataframe, task.variables, task.groupVariable);
                task.includeMissing = true; task.showP = state.showP;
                task.showTest = state.showTest; task.showN = true;
                task.ordinalAs = "ordinal";
                task.scope = state.dataScopeCaptured ? ScopeName(state.dataScope) : "all";
                task.scopeDescription = state.dataScopeCaptured
                    ? state.dataScope.sourceDescription : "All observations";
                if (state.dataScopeCaptured &&
                    state.dataScope.kind == ::rlispstat::core::AnalysisScopeKind::ExplicitRowIds)
                    task.rows = state.dataScope.originalRowIds;
                commandDispatcher_->prepareTable1Task(task);
                pendingMainRTasks_.table1Tasks.push_back(std::move(task));
                MarkMainRTaskPending();
            }
        }

        // Refresh the admissible-variable catalog in every open t test/ANOVA
        // window.  Refit only a test that actually used the changed variable;
        // if that role is no longer legal, clear the stale result and leave
        // the corresponding native selector empty for the user to resolve.
        std::vector<std::string> meanIds;
        for (auto const& [id, state] : meanComparisonStates_)
            if (state.datasetId == group) meanIds.push_back(id);
        for (auto const& id : meanIds)
        {
            auto found = meanComparisonStates_.find(id);
            if (found == meanComparisonStates_.end()) continue;
            auto state = found->second;
            const bool affected = state.specification.groupingVariableId == variable ||
                std::find(state.specification.dependentVariableIds.begin(),
                    state.specification.dependentVariableIds.end(), variable) !=
                    state.specification.dependentVariableIds.end() ||
                std::any_of(state.specification.pairs.begin(), state.specification.pairs.end(),
                    [&](auto const& pair)
                    {
                        return pair.firstVariableId == variable ||
                            pair.secondVariableId == variable;
                    });
            const bool ready = ReconcileMeanComparisonVariableRoles(state, *dataframe);
            if (affected && !ready)
            {
                state.tables.clear();
                state.warnings = {
                    state.analysisType == "paired_samples_t_test"
                        ? "Choose a valid pair of numeric variables."
                        : MeanComparisonUsesGroupingVariable(state) &&
                              !state.specification.groupingVariableId
                        ? "Choose a valid grouping variable."
                        : "Choose a valid numeric dependent variable."
                };
            }
            ShowMeanComparison(state);
            if (affected && ready) RefitMeanComparison(id);
        }
        RefreshDiagnosticPlots();
        QueueCommandStateRefresh();
    }

    void App::HandleDatasetMutation(
        ::rlispstat::core::DatasetMutationEvent const& event)
    {
        if (!commandDispatcher_ || event.group.empty()) return;
        auto const* dataframe = commandDispatcher_->applicationState().datasets().find(event.group);
        if (!dataframe) return;

        const bool structuralChange =
            event.kind == ::rlispstat::core::DatasetMutationKind::ColumnStructure;
        const bool rebuildPlots = event.kind == ::rlispstat::core::DatasetMutationKind::CellValue ||
            event.kind == ::rlispstat::core::DatasetMutationKind::VariableRename;
        const bool valueChanged = event.kind == ::rlispstat::core::DatasetMutationKind::CellValue;
        const bool renamed = event.kind == ::rlispstat::core::DatasetMutationKind::VariableRename;
        const bool rSyncQueued = !(valueChanged || renamed || structuralChange) ||
            QueueDatasetSyncToR(*dataframe);
        if ((valueChanged || renamed || structuralChange) && !rSyncQueued)
            LogStartup("Could not queue edited dataset synchronization for " + event.group);
        if (structuralChange)
        {
            for (auto& [id, owned] : plots_)
                if (owned && owned->group == event.group)
                    ::rlispstat::core::SyncPlotVariablesFromDataFrame(*owned, *dataframe);
        }
        if (rebuildPlots)
        {
            for (auto& [id, owned] : plots_)
            {
                if (!owned || owned->group != event.group) continue;
                auto& plot = *owned;
                if (plot.isGLMDiagnostic || plot.kind == "pca_biplot" ||
                    plot.kind == "pca_scree" || plot.kind == "interaction_plot")
                    continue;
                ::rlispstat::core::SyncPlotVariablesFromDataFrame(plot, *dataframe);
                std::string ignored;
                if (plot.isDatasetSeed)
                    ::rlispstat::core::RefreshDatasetSeedPlotAfterVariableSync(plot);
                else if (plot.kind == "scatter")
                {
                    ::rlispstat::core::RebuildPointsForCurrentVariables(plot);
                    ::rlispstat::core::ComputeRanges(plot);
                }
                else if (plot.kind == "time_series")
                    ::rlispstat::core::RebuildTimeSeriesFromDataFrame(plot, *dataframe, &ignored);
                else if (plot.kind == "trellis_scatterplot")
                    ::rlispstat::core::RebuildTrellisPlotFromDataFrame(plot, *dataframe, &ignored);
                else if (plot.kind == "scatter_matrix")
                {
                    ::rlispstat::core::RebuildScatterMatrixPoints(plot);
                    ::rlispstat::core::ComputeRanges(plot);
                    if (plot.scatterMatrixFitsPending)
                        QueueSmoothRecompute(plot,
                            ::rlispstat::core::SmoothCurveScope::Overall, "lm");
                }
                else if (plot.kind == "histogram")
                {
                    const int bins = std::max(1, static_cast<int>(plot.histogramBins.size()));
                    ::rlispstat::core::RebuildHistogramPointsFromCurrentVariable(plot);
                    ::rlispstat::core::RebinHistogram(plot, bins);
                }
                else if (plot.kind == "barplot")
                    ::rlispstat::core::RebuildBarplotFromDataFrame(plot, *dataframe, &ignored);
                else if (plot.kind == "boxplot")
                {
                    if (::rlispstat::core::BoxplotUsesVariableAxes(plot))
                        ::rlispstat::core::RebuildParallelBoxplotPoints(plot, &ignored);
                    else
                        ::rlispstat::core::RebuildGroupedBoxplotPointsFromDataFrame(
                            plot, *dataframe);
                }
                auto view = views_.find(id);
                if (view != views_.end() && view->second)
                {
                    view->second->SetDataFrame(dataframe);
                    view->second->Show(plot);
                    view->second->SetVisualStyle(commandDispatcher_->plotTheme(),
                        commandDispatcher_->applicationState().displayPointColors(event.group));
                }
            }
        }

        if (event.kind == ::rlispstat::core::DatasetMutationKind::LabelColumn || rebuildPlots)
        {
            const std::string labelColumn =
                commandDispatcher_->applicationState().labelColumn(event.group);
            std::map<int, std::string> labels;
            auto found = std::find_if(dataframe->columns.begin(), dataframe->columns.end(),
                [&](auto const& column) { return column.name == labelColumn; });
            if (found != dataframe->columns.end())
                labels = ::rlispstat::core::RowLabelMapForColumn(*found);
            for (auto const& [id, plot] : plots_)
            {
                if (!plot || plot->group != event.group) continue;
                auto view = views_.find(id);
                if (view != views_.end() && view->second)
                    view->second->SetLabelState(labelColumn, labels);
            }
        }

        auto sheet = dataSheets_.find(event.group);
        if (sheet != dataSheets_.end() && sheet->second)
        {
            if (structuralChange)
            {
                const auto column = std::find_if(dataframe->columns.begin(),
                    dataframe->columns.end(), [&](auto const& value) {
                        return value.name == event.variable;
                    });
                const std::size_t index = column == dataframe->columns.end()
                    ? dataframe->columns.size()
                    : static_cast<std::size_t>(std::distance(dataframe->columns.begin(), column));
                sheet->second->RefreshStructure(*dataframe, index);
            }
            else if (valueChanged)
                sheet->second->RefreshCell(*dataframe, event.row);
            else if (event.kind == ::rlispstat::core::DatasetMutationKind::VariableMetadata)
            {
                const auto column = std::find_if(dataframe->columns.begin(),
                    dataframe->columns.end(), [&](auto const& value) {
                        return value.name == event.variable;
                    });
                if (column != dataframe->columns.end())
                    sheet->second->RefreshColumnMetadata(*dataframe,
                        static_cast<std::size_t>(std::distance(
                            dataframe->columns.begin(), column)));
            }
            else sheet->second->Show(*dataframe, false);
            sheet->second->SetAnalysisScope(
                commandDispatcher_->applicationState().activeAnalysisScope(event.group));
            sheet->second->RefreshRowColors(
                commandDispatcher_->applicationState().displayPointColors(event.group));
            std::set<int> selected;
            commandDispatcher_->applicationState().selectedRows(event.group, selected);
            sheet->second->RefreshSelection(selected);
        }

        auto variables = variableViews_.find(event.group);
        if (variables != variableViews_.end() && variables->second)
        {
            std::map<std::string, std::string> roles;
            auto const& models = commandDispatcher_->applicationState().groupModels();
            auto model = models.find(event.group);
            for (auto const& column : dataframe->columns)
            {
                std::string role = "none";
                if (model != models.end())
                {
                    if (model->second.response == column.name) role = "dependent";
                    else if (::rlispstat::core::HasModelTerm(model->second, column.name))
                        role = "independent";
                }
                roles[column.name] = role;
            }
            variables->second->Show(*dataframe, roles, false);
        }
        // A refit queued after a failed data sync would use the previous R
        // dataset while the sheet already shows the edited value.
        if ((valueChanged || renamed) && rSyncQueued)
        {
            auto const& effects = event.valueChangeEffects;
            if (effects.groupModel) {
                for (auto const& [modelId, view] : linearModelViews_) {
                    auto model = commandDispatcher_->applicationState().groupModels().find(modelId);
                    if (view && model != commandDispatcher_->applicationState().groupModels().end() &&
                        model->second.group == event.group) {
                        if (model->second.autoRefit) QueueLinearModelFit(modelId);
                        else ShowLinearModel(modelId);
                    }
                }
            }
            for (auto const& id : effects.correlationIds)
            {
                auto found = commandDispatcher_->applicationState().correlationMatrices().find(id);
                if (found == commandDispatcher_->applicationState().correlationMatrices().end()) continue;
                auto& state = found->second;
                QueueCorrelationFit(state);
                ++state.modelVersion;
                ShowCorrelationMatrix(id);
            }
            for (auto const& id : effects.dimensionalityIds) RefitDimensionality(id);
            for (auto const& id : effects.scaleAnalysisIds)
            {
                auto found = commandDispatcher_->applicationState().scaleAnalyses().find(id);
                if (found == commandDispatcher_->applicationState().scaleAnalyses().end()) continue;
                if (found->second.autoFit) RefitScaleAnalysis(id);
                else if (scaleAnalysisViews_.count(id)) ShowScaleAnalysis(id);
            }
            for (auto const& id : effects.dendrogramIds) RefitDendrogram(id);
            for (auto const& id : effects.regressionComparisonIds)
            {
                auto found = commandDispatcher_->applicationState().regressionComparisons().find(id);
                if (found == commandDispatcher_->applicationState().regressionComparisons().end()) continue;
                if (found->second.autoRefit) QueueRegressionComparison(found->second);
                if (regressionComparisonViews_.count(id)) ShowRegressionComparison(id);
            }
            for (auto const& id : effects.generalizedGlmIdsToRefit)
                RequestGeneralizedModelFit(id);
            for (auto const& id : effects.generalizedGlmIds)
                if (generalizedModelViews_.count(id)) ShowGeneralizedModel(id);
            for (auto const& id : effects.generalizedComparisonIds)
            {
                auto found = commandDispatcher_->applicationState().generalizedComparisons().find(id);
                if (found == commandDispatcher_->applicationState().generalizedComparisons().end()) continue;
                if (found->second.autoRefit) QueueGeneralizedComparison(found->second);
                if (generalizedComparisonViews_.count(id)) ShowGeneralizedComparison(id);
            }
            for (auto const& id : effects.modelTrellisIds)
            {
                auto found = commandDispatcher_->applicationState().modelTrellises().find(id);
                if (found == commandDispatcher_->applicationState().modelTrellises().end()) continue;
                auto& state = found->second;
                std::set<int> rows;
                if (state.dataScopeCaptured &&
                    state.dataScope.kind == ::rlispstat::core::AnalysisScopeKind::ExplicitRowIds)
                    rows.insert(state.dataScope.originalRowIds.begin(), state.dataScope.originalRowIds.end());
                state.panels = ::rlispstat::core::BuildModelTrellisPanels(
                    state.specification, *dataframe, rows);
                QueueModelTrellisFit(state);
                if (modelTrellisViews_.count(id)) ShowModelTrellis(id);
            }
            for (auto const& id : effects.mixedModelIds)
            {
                auto found = commandDispatcher_->applicationState().nativeMixedModels().find(id);
                if (found == commandDispatcher_->applicationState().nativeMixedModels().end() ||
                    !mixedModelViews_.count(id)) continue;
                commandDispatcher_->applicationState().captureAnalysisScope(found->second.group, found->second.dataScope, found->second.dataScopeCaptured);
                auto& tasks = pendingMainRTasks_.mixedModelTasks;
                tasks.erase(std::remove_if(tasks.begin(), tasks.end(),
                    [&](auto const& task) { return task.state.id == id; }), tasks.end());
                pendingMainRTasks_.mixedModelTasks.push_back({found->second});
                MarkMainRTaskPending();
                ShowMixedModel(found->second);
            }
            for (auto const& [id, state] : outputStates_)
            {
                if (state.datasetId != event.group) continue;
                const bool affected = state.groupVariable == event.variable ||
                    std::find(state.variables.begin(), state.variables.end(), event.variable) != state.variables.end();
                if (!affected) continue;
                if (state.tableType == "nested_contingency")
                {
                    auto rebuilt = ::rlispstat::core::NestedContingencyTableStateForDataFrame(
                        *dataframe, state.id, state.variables, state.groupVariable,
                        state.nestedDisplayMode, state.dataScopeCaptured ? &state.dataScope : nullptr);
                    ShowTable1(rebuilt);
                }
                else if (state.tableType == "table1")
                {
                    ::rlispstat::core::MainRTable1Task task;
                    task.id = state.id; task.group = state.datasetId;
                    task.variables = state.variables; task.groupVariable = state.groupVariable;
                    task.variableTypes = Table1CanonicalVariableTypes(
                        *dataframe, task.variables, task.groupVariable);
                    task.includeMissing = true; task.showP = state.showP;
                    task.showTest = state.showTest; task.showN = true;
                    task.ordinalAs = "ordinal";
                    task.scope = state.dataScopeCaptured ? ScopeName(state.dataScope) : "all";
                    task.scopeDescription = state.dataScopeCaptured
                        ? state.dataScope.sourceDescription : "All observations";
                    if (state.dataScopeCaptured &&
                        state.dataScope.kind == ::rlispstat::core::AnalysisScopeKind::ExplicitRowIds)
                        task.rows = state.dataScope.originalRowIds;
                    commandDispatcher_->prepareTable1Task(task);
                    pendingMainRTasks_.table1Tasks.push_back(std::move(task));
                    MarkMainRTaskPending();
                }
            }
            for (auto const& [id, state] : meanComparisonStates_)
            {
                if (state.datasetId != event.group) continue;
                const bool affected = state.specification.groupingVariableId == event.variable ||
                    std::find(state.specification.dependentVariableIds.begin(),
                              state.specification.dependentVariableIds.end(), event.variable) !=
                        state.specification.dependentVariableIds.end() ||
                    std::any_of(state.specification.pairs.begin(), state.specification.pairs.end(),
                        [&](auto const& pair) { return pair.firstVariableId == event.variable ||
                                                       pair.secondVariableId == event.variable; });
                if (affected) RefitMeanComparison(id);
            }
        }
        RefreshDiagnosticPlots();
        QueueCommandStateRefresh();
    }

    bool App::QueueDatasetSyncToR(::rlispstat::core::DataFrameModel const& dataframe)
    {
        std::error_code error;
        auto folder = std::filesystem::temp_directory_path(error) /
            ("rlispstat-sync-" + std::to_string(
                static_cast<unsigned long long>(GetTickCount64())));
        if (error || !std::filesystem::create_directories(folder, error) || error) return false;
        auto payload = folder / "dataset.txt";
        std::ofstream output(payload, std::ios::binary | std::ios::trunc);
        if (!output)
        {
            std::filesystem::remove_all(folder, error);
            return false;
        }
        ::rlispstat::core::WriteDataFramePayloadForR(output, dataframe, true);
        output.close();
        if (!output)
        {
            std::filesystem::remove_all(folder, error);
            return false;
        }
        pendingMainRTasks_.datasetSyncTasks.push_back({
            StartupId("sync-r-", dataframe.group), dataframe.group, payload.string() });
        MarkMainRTaskPending();
        return true;
    }

    void App::SetImputationDisplayMode(std::string const& group,
                                       std::string const& mode)
    {
        if (!commandDispatcher_ || group.empty()) return;
        auto* dataframe = commandDispatcher_->applicationState().datasets().find(group);
        if (!dataframe || dataframe->datasetType != "multiple_imputation" ||
            dataframe->imputationCount <= 0) return;

        if (mode == "all" || mode == "original")
        {
            dataframe->imputationDisplayMode = mode;
        }
        else if (mode.rfind("version:", 0) == 0)
        {
            const int version = std::atoi(mode.substr(8).c_str());
            if (version < 1 || version > dataframe->imputationCount) return;
            dataframe->activeImputationVersion = version;
            dataframe->imputationDisplayMode = "version";
        }
        else return;

        ::rlispstat::core::ApplyImputationDisplayModeToStoredValues(*dataframe);

        for (auto& [id, owned] : plots_)
        {
            if (!owned || owned->group != group) continue;
            auto& plot = *owned;
            if (plot.isGLMDiagnostic || plot.kind == "pca_biplot" ||
                plot.kind == "pca_scree" || plot.kind == "interaction_plot")
                continue;

            ::rlispstat::core::SyncPlotVariablesFromDataFrame(plot, *dataframe);
            std::string ignored;
            if (plot.isDatasetSeed)
            {
                ::rlispstat::core::RefreshDatasetSeedPlotAfterVariableSync(plot);
            }
            else if (plot.kind == "scatter")
            {
                ::rlispstat::core::RebuildPointsForCurrentVariables(plot);
                ComputeScatterRangesIncludingAllImputations(plot, *dataframe);
                if (::rlispstat::core::SmoothCurveScopeIsPresent(
                        plot.smoothCurves, ::rlispstat::core::SmoothCurveScope::Overall) ||
                    ::rlispstat::core::SmoothCurveScopeIsPresent(
                        plot.smoothCurves, ::rlispstat::core::SmoothCurveScope::Selection) ||
                    ::rlispstat::core::SmoothCurveScopeIsPresent(
                        plot.smoothCurves, ::rlispstat::core::SmoothCurveScope::ColorGroup))
                    QueueExistingSmoothRecompute(plot);
            }
            else if (plot.kind == "time_series")
                ::rlispstat::core::RebuildTimeSeriesFromDataFrame(plot, *dataframe, &ignored);
            else if (plot.kind == "trellis_scatterplot")
                ::rlispstat::core::RebuildTrellisPlotFromDataFrame(plot, *dataframe, &ignored);
            else if (plot.kind == "scatter_matrix")
            {
                ::rlispstat::core::RebuildScatterMatrixPoints(plot);
                ::rlispstat::core::ComputeRanges(plot);
                if (plot.scatterMatrixFitsPending)
                    QueueSmoothRecompute(plot,
                        ::rlispstat::core::SmoothCurveScope::Overall, "lm");
            }
            else if (plot.kind == "histogram")
            {
                const int bins = std::max(1, static_cast<int>(plot.histogramBins.size()));
                ::rlispstat::core::RebuildHistogramPointsFromCurrentVariable(plot);
                ::rlispstat::core::RebinHistogram(plot, bins);
            }
            else if (plot.kind == "barplot")
                ::rlispstat::core::RebuildBarplotFromDataFrame(plot, *dataframe, &ignored);
            else if (plot.kind == "boxplot")
            {
                if (::rlispstat::core::BoxplotUsesVariableAxes(plot))
                    ::rlispstat::core::RebuildParallelBoxplotPoints(plot, &ignored);
                else
                    ::rlispstat::core::RebuildGroupedBoxplotPointsFromDataFrame(plot, *dataframe);
            }

            auto view = views_.find(id);
            if (view != views_.end() && view->second)
            {
                view->second->SetDataFrame(dataframe);
                view->second->Show(plot);
                view->second->SetVisualStyle(commandDispatcher_->plotTheme(),
                    commandDispatcher_->applicationState().displayPointColors(group));
            }
        }

        auto sheet = dataSheets_.find(group);
        if (sheet != dataSheets_.end() && sheet->second)
        {
            sheet->second->Show(*dataframe);
            sheet->second->RefreshRowColors(
                commandDispatcher_->applicationState().displayPointColors(group));
            std::set<int> selected;
            commandDispatcher_->applicationState().selectedRows(group, selected);
            sheet->second->RefreshSelection(selected);
        }
    }

    void App::ShowColorPalette(std::string const& group)
    {
        if (!commandDispatcher_ || group.empty()) return;
        paletteGroup_ = group;
        if (paletteWindow_)
        {
            paletteWindow_.Title(to_hstring("LinkEDA palette — " + group));
            paletteWindow_.Activate();
            return;
        }
        paletteWindow_ = CreateLinkEDAWindow();
        paletteWindow_.Title(to_hstring("LinkEDA palette — " + group));
        // A colour palette is a persistent tool, not a document window.  Keep
        // it visible while the user compares plots and keep it out of Alt-Tab.
        auto palettePresenter = Microsoft::UI::Windowing::OverlappedPresenter::CreateForToolWindow();
        palettePresenter.IsAlwaysOnTop(true);
        palettePresenter.IsResizable(false);
        palettePresenter.IsMaximizable(false);
        palettePresenter.IsMinimizable(false);
        paletteWindow_.AppWindow().SetPresenter(palettePresenter);
        paletteWindow_.AppWindow().IsShownInSwitchers(false);
        auto root = Controls::StackPanel(); root.Padding(Thickness{12}); root.Spacing(10);
        root.Background(Media::SolidColorBrush(
            Windows::UI::ColorHelper::FromArgb(255, 250, 250, 250)));
        auto title = Controls::TextBlock(); title.Text(L"Color selected observations");
        title.FontSize(13); title.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
        root.Children().Append(title);
        auto swatches = Controls::Grid(); swatches.ColumnSpacing(8); swatches.RowSpacing(8);
        for (int column = 0; column < 4; ++column) swatches.ColumnDefinitions().Append(Controls::ColumnDefinition());
        for (int row = 0; row < 2; ++row) swatches.RowDefinitions().Append(Controls::RowDefinition());
        int index = 0;
        for (auto const& color : ::rlispstat::core::PaletteColors())
        {
            auto button = Controls::Button(); button.Width(40); button.Height(34);
            button.Padding(Thickness{0});
            auto swatchBrush = Media::SolidColorBrush(Windows::UI::ColorHelper::FromArgb(255,
                static_cast<uint8_t>(std::lround(color.baseR * 255.0)),
                static_cast<uint8_t>(std::lround(color.baseG * 255.0)),
                static_cast<uint8_t>(std::lround(color.baseB * 255.0))));
            auto transparentBrush = Media::SolidColorBrush(
                Windows::UI::ColorHelper::FromArgb(0, 0, 0, 0));
            button.Background(transparentBrush);
            button.BorderThickness(Thickness{ 0 });
            // Keep the swatch separate from the Button background.  WinUI is then
            // free to render its hover state without replacing the represented colour.
            auto swatch = Controls::Border();
            swatch.Width(40); swatch.Height(34);
            swatch.Background(swatchBrush);
            swatch.BorderBrush(Media::SolidColorBrush(
                Windows::UI::ColorHelper::FromArgb(255, 100, 100, 100)));
            swatch.BorderThickness(Thickness{ 1 });
            button.Content(swatch);
            Controls::ToolTipService::SetToolTip(button, box_value(to_hstring(color.name)));
            button.Click([this, name = std::string(color.name)](auto const&, auto const&)
            {
                if (commandDispatcher_ && !paletteGroup_.empty())
                    DispatchApplicationCommand({ "SET_SELECTED_COLOR", paletteGroup_, name });
            });
            Controls::Grid::SetRow(button, index / 4); Controls::Grid::SetColumn(button, index % 4);
            swatches.Children().Append(button); ++index;
        }
        root.Children().Append(swatches);
        auto reset = Controls::Button(); reset.Content(box_value(L"Default / Reset color"));
        reset.HorizontalAlignment(HorizontalAlignment::Stretch);
        reset.Click([this](auto const&, auto const&)
        {
            if (!commandDispatcher_ || paletteGroup_.empty()) return;
            std::set<int> selected;
            commandDispatcher_->applicationState().selectedRows(paletteGroup_, selected);
            std::vector<std::string> command{ "CLEAR_ROW_COLORS", paletteGroup_, std::to_string(selected.size()) };
            for (int row : selected) command.push_back(std::to_string(row));
            DispatchApplicationCommand(command);
        });
        root.Children().Append(reset);
        paletteWindow_.Content(root);
        ResizeLinkEDAWindowClient(paletteWindow_, 230, 185);
        paletteWindow_.Closed([this](auto const&, auto const&) { paletteWindow_ = nullptr; });
        paletteWindow_.Activate();
    }

    void App::ShowAnalysisScopePanel(std::string const& group)
    {
        if (!commandDispatcher_ || group.empty()) return;
        analysisScopeGroup_ = group;
        if (!analysisScopeWindow_)
        {
            analysisScopeWindow_ = CreateLinkEDAWindow();
            analysisScopeWindow_.Title(L"Analysis Scope");
            auto presenter = Microsoft::UI::Windowing::OverlappedPresenter::CreateForToolWindow();
            presenter.IsAlwaysOnTop(true);
            presenter.IsResizable(false);
            presenter.IsMaximizable(false);
            presenter.IsMinimizable(false);
            analysisScopeWindow_.AppWindow().SetPresenter(presenter);
            analysisScopeWindow_.AppWindow().IsShownInSwitchers(false);

            auto root = Controls::StackPanel();
            root.Padding(Thickness{10, 8, 10, 7});
            root.Spacing(3);
            root.Background(Media::SolidColorBrush(
                Windows::UI::ColorHelper::FromArgb(255, 250, 250, 250)));
            analysisScopeSummary_ = Controls::TextBlock();
            analysisScopeSummary_.FontSize(11.5);
            analysisScopeSummary_.FontWeight(
                Windows::UI::Text::FontWeights::SemiBold());
            analysisScopeSummary_.TextTrimming(TextTrimming::CharacterEllipsis);
            root.Children().Append(analysisScopeSummary_);
            analysisScopeOptions_ = Controls::StackPanel();
            analysisScopeOptions_.Spacing(1);
            analysisScopeScroll_ = Controls::ScrollViewer();
            analysisScopeScroll_.Content(analysisScopeOptions_);
            analysisScopeScroll_.VerticalScrollBarVisibility(
                Controls::ScrollBarVisibility::Auto);
            analysisScopeScroll_.HorizontalScrollBarVisibility(
                Controls::ScrollBarVisibility::Disabled);
            analysisScopeScroll_.MaxHeight(147);
            root.Children().Append(analysisScopeScroll_);
            analysisScopeSave_ = Controls::Button();
            analysisScopeSave_.Content(box_value(L"+ Save selection..."));
            analysisScopeSave_.FontSize(11);
            analysisScopeSave_.Padding(Thickness{5, 1, 5, 1});
            analysisScopeSave_.HorizontalAlignment(HorizontalAlignment::Left);
            analysisScopeSave_.Click([this](auto const&, auto const&)
            {
                if (analysisScopeGroup_.empty()) return;
                const std::string reply = DispatchApplicationCommand(
                    {"SAVE_ANALYSIS_SCOPE_FROM_SELECTION", analysisScopeGroup_});
                if (reply.rfind("ERR ", 0) == 0)
                    MessageBoxW(nullptr, to_hstring(reply.substr(4)).c_str(),
                        L"Save Analysis Scope", MB_OK | MB_ICONWARNING);
            });
            root.Children().Append(analysisScopeSave_);
            analysisScopeWindow_.Content(root);
            ResizeLinkEDAWindowClient(analysisScopeWindow_, 250, 128);
            analysisScopeWindow_.Closed([this](auto const&, auto const&)
            {
                analysisScopeWindow_ = nullptr;
                analysisScopeSummary_ = nullptr;
                analysisScopeScroll_ = nullptr;
                analysisScopeOptions_ = nullptr;
                analysisScopeSave_ = nullptr;
            });
        }
        RefreshAnalysisScopePanel();
        analysisScopeWindow_.Activate();
    }

    void App::RefreshAnalysisScopePanel()
    {
        if (!analysisScopeWindow_ || !analysisScopeSummary_ ||
            !analysisScopeScroll_ || !analysisScopeOptions_ ||
            !analysisScopeSave_ || !commandDispatcher_) return;
        const auto* dataset = commandDispatcher_->applicationState().datasets().find(
            analysisScopeGroup_);
        analysisScopeOptions_.Children().Clear();
        if (!dataset)
        {
            analysisScopeSummary_.Text(L"No active dataset");
            analysisScopeScroll_.IsEnabled(false);
            analysisScopeSave_.IsEnabled(false);
            return;
        }
        std::set<int> selected;
        commandDispatcher_->applicationState().selectedRows(
            analysisScopeGroup_, selected);
        const auto active = commandDispatcher_->applicationState()
            .activeAnalysisScope(analysisScopeGroup_);
        const auto saved = commandDispatcher_->applicationState()
            .savedSelections(analysisScopeGroup_);
        analysisScopeSummary_.Text(to_hstring(
            ::rlispstat::core::AnalysisScopeSummary(
                active, static_cast<std::size_t>(std::max(0, dataset->rows)), false)));
        const std::size_t total = static_cast<std::size_t>(
            std::max(0, dataset->rows));
        struct ScopeRow {
            std::string label;
            std::string value;
            std::size_t count;
            bool enabled;
            bool checked;
        };
        std::vector<ScopeRow> rows{
            {"All observations", "all", total, true,
                active.kind == ::rlispstat::core::AnalysisScopeKind::AllObservations},
            {"Current selection", "selected", selected.size(), !selected.empty(),
                ::rlispstat::core::AnalysisScopeTracksCurrentSelection(active)},
            {"Unselected", "unselected",
                total >= selected.size() ? total - selected.size() : 0, true,
                active.sourceKind ==
                    ::rlispstat::core::AnalysisScopeSourceKind::CurrentUnselection}
        };
        const auto activeName = ::rlispstat::core::AnalysisScopeSelectionName(active);
        for (auto const& selection : saved)
            rows.push_back({selection.name,
                ::rlispstat::core::SavedAnalysisScopeChoiceValue(selection.name),
                selection.originalRowIds.size(), true,
                activeName && *activeName == selection.name});
        for (auto const& row : rows)
        {
            auto line = Controls::Grid();
            auto labelColumn = Controls::ColumnDefinition();
            labelColumn.Width(GridLength{1.0, GridUnitType::Star});
            line.ColumnDefinitions().Append(labelColumn);
            auto countColumn = Controls::ColumnDefinition();
            countColumn.Width(GridLength{38.0, GridUnitType::Pixel});
            line.ColumnDefinitions().Append(countColumn);
            auto option = Controls::RadioButton();
            option.Content(box_value(to_hstring(row.label)));
            option.GroupName(L"linkeda-global-analysis-scope");
            option.FontSize(11);
            option.IsEnabled(row.enabled);
            option.IsChecked(row.checked);
            option.Click([this, value = row.value](auto const&, auto const&)
            {
                if (!commandDispatcher_ || analysisScopeGroup_.empty()) return;
                std::vector<std::string> command;
                if (value == "all")
                    command = {"SET_ANALYSIS_SCOPE_ALL", analysisScopeGroup_};
                else if (value == "selected")
                    command = {"SET_ANALYSIS_SCOPE_FROM_SELECTION", analysisScopeGroup_};
                else if (value == "unselected")
                    command = {"SET_ANALYSIS_SCOPE_UNSELECTED", analysisScopeGroup_};
                else if (auto name =
                    ::rlispstat::core::SavedAnalysisScopeNameFromChoiceValue(value))
                    command = {"USE_SAVED_ANALYSIS_SCOPE", analysisScopeGroup_, *name};
                else return;
                const std::string reply = DispatchApplicationCommand(command);
                if (reply.rfind("ERR ", 0) == 0)
                    MessageBoxW(nullptr, to_hstring(reply.substr(4)).c_str(),
                        L"Analysis Scope", MB_OK | MB_ICONWARNING);
                RefreshAnalysisScopePanel();
            });
            Controls::Grid::SetColumn(option, 0);
            line.Children().Append(option);
            auto count = Controls::TextBlock();
            count.Text(to_hstring(row.count));
            count.FontSize(11);
            count.Opacity(0.65);
            count.TextAlignment(TextAlignment::Right);
            count.VerticalAlignment(VerticalAlignment::Center);
            Controls::Grid::SetColumn(count, 1);
            line.Children().Append(count);
            analysisScopeOptions_.Children().Append(line);
        }
        analysisScopeScroll_.IsEnabled(true);
        analysisScopeSave_.IsEnabled(!selected.empty());
        const double listHeight = rows.empty() ? 0.0 : std::min(147.0,
            static_cast<double>(rows.size()) * 21.0);
        analysisScopeScroll_.Height(listHeight);
        ResizeLinkEDAWindowClient(analysisScopeWindow_, 250,
            static_cast<int>(65.0 + listHeight));
    }

    void App::ShowSnapshotAlbum()
    {
        if (!snapshotAlbumWindow_)
        {
            snapshotAlbumWindow_ = CreateLinkEDAWindow();
            snapshotAlbumWindow_.Title(L"LinkEDA Snapshot Album");
            auto root = Controls::Grid();
            root.Background(Media::SolidColorBrush(
                Windows::UI::ColorHelper::FromArgb(255, 250, 250, 250)));
            snapshotAlbumTabs_ = Controls::TabView();
            snapshotAlbumTabs_.IsAddTabButtonVisible(false);
            snapshotAlbumTabs_.HorizontalContentAlignment(HorizontalAlignment::Stretch);
            snapshotAlbumTabs_.VerticalContentAlignment(VerticalAlignment::Stretch);
            // Size album pages from the window-owned root, never from
            // TabView itself. TabView's desired size depends on its selected
            // page; deriving the page height from it creates a feedback loop
            // that repeatedly shrinks the plot to the 280 px fallback.
            root.SizeChanged([this](auto const&, SizeChangedEventArgs const& args)
            {
                if (!snapshotAlbumTabs_) return;
                const double pageHeight = std::max(280.0, args.NewSize().Height - 48.0);
                for (uint32_t index = 0;
                     index < snapshotAlbumTabs_.TabItems().Size(); ++index)
                {
                    auto tab = snapshotAlbumTabs_.TabItems().GetAt(index)
                        .try_as<Controls::TabViewItem>();
                    if (!tab) continue;
                    auto page = tab.Content().try_as<Controls::Grid>();
                    if (page)
                    {
                        page.Height(pageHeight);
                        if (page.Children().Size() > 0)
                            if (auto primary = page.Children().GetAt(0)
                                    .try_as<Controls::Grid>())
                                primary.Height(pageHeight);
                    }
                }
            });
            snapshotAlbumTabs_.CanDragTabs(true);
            snapshotAlbumTabs_.CanReorderTabs(true);
            snapshotAlbumTabs_.CloseButtonOverlayMode(
                Controls::TabViewCloseButtonOverlayMode::Auto);
            snapshotAlbumTabs_.TabCloseRequested(
                [this](auto const&, Controls::TabViewTabCloseRequestedEventArgs const& args)
            {
                auto tab = args.Tab();
                const std::string identifier = to_string(
                    unbox_value_or<hstring>(tab.Tag(), L""));
                if (identifier.empty()) return;
                snapshotAlbum_.remove(identifier);
                snapshotAlbumSelectionId_.clear();
                RefreshSnapshotAlbum();
            });
            snapshotAlbumTabs_.SelectionChanged([this](auto const&, auto const&)
            {
                if (!snapshotAlbumTabs_) return;
                auto tab = snapshotAlbumTabs_.SelectedItem().try_as<Controls::TabViewItem>();
                if (tab)
                    snapshotAlbumSelectionId_ = to_string(
                        unbox_value_or<hstring>(tab.Tag(), L""));
                UpdateSnapshotAlbumNotesPanelWidth();
            });
            snapshotAlbumTabs_.TabDragCompleted([this](auto const&, auto const&)
            {
                if (!snapshotAlbumTabs_) return;
                for (uint32_t position = 0;
                     position < snapshotAlbumTabs_.TabItems().Size(); ++position)
                {
                    auto tab = snapshotAlbumTabs_.TabItems().GetAt(position)
                        .try_as<Controls::TabViewItem>();
                    if (!tab) continue;
                    const std::string identifier = to_string(
                        unbox_value_or<hstring>(tab.Tag(), L""));
                    if (!identifier.empty()) snapshotAlbum_.move(identifier, position);
                }
            });
            snapshotAlbumEmpty_ = Controls::TextBlock();
            snapshotAlbumEmpty_.Text(
                L"Snapshot Album\n\nAdd the active graph or analysis from "
                L"Window → Snapshot Album.");
            snapshotAlbumEmpty_.TextWrapping(TextWrapping::Wrap);
            snapshotAlbumEmpty_.TextAlignment(TextAlignment::Center);
            snapshotAlbumEmpty_.FontSize(15);
            snapshotAlbumEmpty_.HorizontalAlignment(HorizontalAlignment::Center);
            snapshotAlbumEmpty_.VerticalAlignment(VerticalAlignment::Center);
            snapshotAlbumEmpty_.Foreground(Media::SolidColorBrush(
                Windows::UI::ColorHelper::FromArgb(255, 92, 96, 104)));
            root.Children().Append(snapshotAlbumTabs_);
            root.Children().Append(snapshotAlbumEmpty_);
            snapshotAlbumExportWebView_ = Controls::WebView2();
            snapshotAlbumExportWebView_.Visibility(Visibility::Collapsed);
            snapshotAlbumExportWebView_.Opacity(0.001);
            snapshotAlbumExportWebView_.IsHitTestVisible(false);
            root.Children().Append(snapshotAlbumExportWebView_);
            snapshotAlbumWindow_.Content(root);
            ResizeLinkEDAWindowClient(snapshotAlbumWindow_, 1080, 700);
            snapshotAlbumWindow_.Closed([this](auto const&, auto const&)
            {
                snapshotAlbumWindow_ = nullptr;
                snapshotAlbumTabs_ = nullptr;
                snapshotAlbumEmpty_ = nullptr;
                snapshotAlbumExportWebView_ = nullptr;
                snapshotAlbumPlotViews_.clear();
                snapshotAlbumNotesExpanded_ = false;
            });
        }
        RefreshSnapshotAlbum();
        snapshotAlbumWindow_.Activate();
    }

    void App::RefreshSnapshotAlbum()
    {
        if (!snapshotAlbumTabs_) return;
        std::string selected = snapshotAlbumSelectionId_;
        if (selected.empty()) if (auto previous = snapshotAlbumTabs_.SelectedItem()
                .try_as<Controls::TabViewItem>())
            selected = to_string(unbox_value_or<hstring>(previous.Tag(), L""));
        snapshotAlbumTabs_.TabItems().Clear();
        snapshotAlbumPlotViews_.clear();
        snapshotAlbumTabs_.Visibility(snapshotAlbum_.empty()
            ? Visibility::Collapsed : Visibility::Visible);
        if (snapshotAlbumEmpty_)
            snapshotAlbumEmpty_.Visibility(snapshotAlbum_.empty()
                ? Visibility::Visible : Visibility::Collapsed);
        for (auto const& snapshot : snapshotAlbum_.items())
        {
            const auto snapshotId = snapshot.metadata.snapshot_id;
            auto page = Controls::Grid();
            page.HorizontalAlignment(HorizontalAlignment::Stretch);
            page.VerticalAlignment(VerticalAlignment::Stretch);
            page.Height(std::max(600.0,
                snapshotAlbumTabs_.ActualHeight() - 48.0));
            page.Background(Media::SolidColorBrush(
                Windows::UI::ColorHelper::FromArgb(255, 255, 255, 255)));
            page.ColumnDefinitions().Append(Controls::ColumnDefinition());
            auto notesColumn = Controls::ColumnDefinition();
            const bool notesVisible = snapshot.note && snapshot.note->visible;
            notesColumn.Width(notesVisible
                ? GridLengthHelper::FromPixels(270)
                : GridLengthHelper::FromPixels(0));
            page.ColumnDefinitions().Append(notesColumn);

            auto primary = Controls::Grid();
            primary.Padding(Thickness{12});
            primary.HorizontalAlignment(HorizontalAlignment::Stretch);
            primary.VerticalAlignment(VerticalAlignment::Stretch);
            // TabView's content presenter measures an unconstrained child at
            // its desired height even when the page itself has an explicit
            // height. Give the actual plot host that height too; otherwise the
            // first 280 px render becomes self-perpetuating.
            primary.Height(page.Height());
            if (auto const* frozenPlot =
                    std::get_if<::rlispstat::core::FrozenPlotContent>(
                        &snapshot.renderable_content))
            {
                auto plotView = ScatterPlotView::CreateSnapshotHost(
                    frozenPlot->model, snapshot.metadata.theme_name);
                // Establish a non-minimal desired size before the host enters
                // TabView's visual tree. Subsequent SizeChanged notifications
                // make it responsive, but they must not inherit the initial
                // 280 px fallback measurement.
                plotView->ResizeSnapshotHost(
                    std::max(640.0, frozenPlot->preferred_width),
                    std::max(480.0, frozenPlot->preferred_height));
                auto content = plotView->ContentRoot();
                content.HorizontalAlignment(HorizontalAlignment::Stretch);
                content.VerticalAlignment(VerticalAlignment::Stretch);
                primary.Children().Append(content);
                std::weak_ptr<ScatterPlotView> weakPlotView = plotView;
                primary.SizeChanged([weakPlotView](auto const&,
                                                   SizeChangedEventArgs const& args)
                {
                    if (auto view = weakPlotView.lock())
                    {
                        LogStartup("Snapshot plot host arranged: " +
                            std::to_string(static_cast<int>(args.NewSize().Width)) + "x" +
                            std::to_string(static_cast<int>(args.NewSize().Height)));
                        view->ResizeSnapshotHost(
                            std::max(360.0, args.NewSize().Width - 24.0),
                            std::max(280.0, args.NewSize().Height - 24.0));
                    }
                });
                primary.Loaded([weakPlotView](auto const& sender, auto const&)
                {
                    if (auto view = weakPlotView.lock())
                    {
                        auto host = sender.template try_as<Controls::Grid>();
                        if (host)
                            view->ResizeSnapshotHost(
                                std::max(360.0, host.ActualWidth() - 24.0),
                                std::max(280.0, host.ActualHeight() - 24.0));
                    }
                });
                snapshotAlbumPlotViews_.emplace(snapshotId, std::move(plotView));
            }
            else
            {
                primary.Children().Append(SnapshotPreviewElement(snapshot, true));
            }
            AttachStickerOverlay(primary, snapshot.stickers,
                [this, snapshotId](auto const& stickers)
                {
                    if (auto item = snapshotAlbum_.find(snapshotId))
                    {
                        item->stickers = stickers;
                        if (!stickers.empty()) item->metadata.include_notes = true;
                    }
                });
            page.Children().Append(primary);
            if (notesVisible)
            {
                auto notesPanel = Controls::Grid();
                notesPanel.Padding(Thickness{12});
                notesPanel.RowDefinitions().Append(Controls::RowDefinition());
                notesPanel.RowDefinitions().GetAt(0).Height(GridLengthHelper::Auto());
                notesPanel.RowDefinitions().Append(Controls::RowDefinition());
                notesPanel.Background(Media::SolidColorBrush(
                    Windows::UI::ColorHelper::FromArgb(255, 246, 246, 246)));
                auto heading = Controls::TextBlock(); heading.Text(L"Notes");
                heading.FontSize(13);
                heading.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
                notesPanel.Children().Append(heading);
                auto notes = Controls::TextBox();
                notes.Text(snapshot.note ? to_hstring(snapshot.note->plain_text) : hstring{});
                notes.PlaceholderText(L"Snapshot notes");
                notes.AcceptsReturn(true); notes.TextWrapping(TextWrapping::Wrap);
                notes.VerticalAlignment(VerticalAlignment::Stretch);
                Controls::Grid::SetRow(notes, 1); notes.Margin(Thickness{0, 8, 0, 0});
                notes.TextChanged([this, snapshotId, notes](auto const&, auto const&)
                {
                    snapshotAlbum_.setNoteText(snapshotId, to_string(notes.Text()));
                });
                notesPanel.Children().Append(notes);
                Controls::Grid::SetColumn(notesPanel, 1);
                page.Children().Append(notesPanel);
            }

            auto menu = Controls::MenuFlyout();
            auto rename = Controls::MenuFlyoutItem(); rename.Text(L"Rename Snapshot...");
            rename.Click([this, snapshotId, page](auto const&, auto const&)
            { RenameSnapshotAsync(snapshotId, page); });
            menu.Items().Append(rename);
            auto annotate = Controls::MenuFlyoutSubItem();
            annotate.Text((snapshot.note && snapshot.note->has_content) ||
                !snapshot.stickers.empty() ? L"Annotate •" : L"Annotate");
            auto toggleNotes = Controls::MenuFlyoutItem();
            toggleNotes.Text(notesVisible ? L"Hide Notes" :
                (snapshot.note && snapshot.note->has_content ? L"Show Notes •" : L"Show Notes"));
            toggleNotes.Click([this, snapshotId, notesVisible](auto const&, auto const&)
            {
                snapshotAlbum_.setNoteVisible(snapshotId, !notesVisible);
                snapshotAlbumSelectionId_ = snapshotId;
                RefreshSnapshotAlbum();
            });
            annotate.Items().Append(toggleNotes);
            auto newSticker = Controls::MenuFlyoutItem();
            newSticker.Text(L"New Sticker");
            newSticker.Click([primary](auto const&, auto const&)
            {
                AddStickerToOverlay(primary);
            });
            annotate.Items().Append(newSticker);
            menu.Items().Append(annotate);
            menu.Items().Append(Controls::MenuFlyoutSeparator());
            auto include = Controls::ToggleMenuFlyoutItem();
            include.Text(L"Include in Album Export");
            include.IsChecked(snapshot.metadata.include_in_export);
            include.Click([this, snapshotId, include](auto const&, auto const&)
            { snapshotAlbum_.setIncludedInExport(snapshotId, include.IsChecked()); });
            menu.Items().Append(include);
            auto includeNotes = Controls::ToggleMenuFlyoutItem();
            includeNotes.Text(L"Include Notes in Export");
            includeNotes.IsChecked(snapshot.metadata.include_notes);
            includeNotes.Click([this, snapshotId, includeNotes](auto const&, auto const&)
            { snapshotAlbum_.setNotesIncluded(snapshotId, includeNotes.IsChecked()); });
            menu.Items().Append(includeNotes);
            menu.Items().Append(Controls::MenuFlyoutSeparator());
            auto exportAlbum = Controls::MenuFlyoutItem();
            exportAlbum.Text(L"Export Snapshot Album...");
            exportAlbum.Click([this](auto const&, auto const&) { ExportSnapshotAlbum(); });
            menu.Items().Append(exportAlbum);
            menu.Items().Append(Controls::MenuFlyoutSeparator());
            auto remove = Controls::MenuFlyoutItem(); remove.Text(L"Remove Snapshot");
            remove.Click([this, snapshotId](auto const&, auto const&)
            {
                snapshotAlbum_.remove(snapshotId);
                snapshotAlbumSelectionId_.clear();
                RefreshSnapshotAlbum();
            });
            menu.Items().Append(remove);
            page.ContextFlyout(menu);

            auto tab = Controls::TabViewItem();
            const auto title = snapshot.metadata.title.empty()
                ? snapshot.metadata.default_title : snapshot.metadata.title;
            tab.Header(box_value(to_hstring(title)));
            tab.Tag(box_value(to_hstring(snapshotId)));
            tab.IsClosable(snapshotAlbum_.size() > 1);
            tab.HorizontalContentAlignment(HorizontalAlignment::Stretch);
            tab.VerticalContentAlignment(VerticalAlignment::Stretch);
            tab.Content(page);
            snapshotAlbumTabs_.TabItems().Append(tab);
            if (snapshotId == selected) snapshotAlbumTabs_.SelectedItem(tab);
        }
        if (!snapshotAlbum_.empty() && !snapshotAlbumTabs_.SelectedItem())
        {
            snapshotAlbumTabs_.SelectedIndex(
                static_cast<int32_t>(snapshotAlbumTabs_.TabItems().Size() - 1));
            auto tab = snapshotAlbumTabs_.SelectedItem().try_as<Controls::TabViewItem>();
            if (tab) snapshotAlbumSelectionId_ = to_string(
                unbox_value_or<hstring>(tab.Tag(), L""));
        }
        UpdateSnapshotAlbumNotesPanelWidth();
    }

    fire_and_forget App::RenameSnapshotAsync(
        std::string snapshotId, FrameworkElement owner)
    {
        auto const* snapshot = snapshotAlbum_.find(snapshotId);
        if (!snapshot || !owner) co_return;
        auto field = Controls::TextBox();
        field.Text(to_hstring(snapshot->metadata.title));
        field.SelectAll();
        auto dialog = Controls::ContentDialog();
        dialog.Title(box_value(L"Rename Snapshot"));
        dialog.Content(field);
        dialog.PrimaryButtonText(L"Rename");
        dialog.CloseButtonText(L"Cancel");
        dialog.DefaultButton(Controls::ContentDialogButton::Primary);
        dialog.XamlRoot(owner.XamlRoot());
        if (co_await dialog.ShowAsync() == Controls::ContentDialogResult::Primary)
        {
            snapshotAlbum_.rename(snapshotId, to_string(field.Text()));
            snapshotAlbumSelectionId_ = snapshotId;
            RefreshSnapshotAlbum();
        }
    }

    fire_and_forget App::SaveCurrentSelectionAsScopeAsync(
        std::string group, FrameworkElement owner)
    {
        if (!commandDispatcher_ || group.empty() || !owner) co_return;
        std::set<int> selected;
        if (!commandDispatcher_->applicationState().selectedRows(group, selected) ||
            selected.empty())
        {
            MessageBoxW(nullptr, L"Select one or more observations first.",
                L"Save Analysis Scope", MB_OK | MB_ICONINFORMATION);
            co_return;
        }
        auto panel = Controls::StackPanel();
        panel.Spacing(8.0);
        auto explanation = Controls::TextBlock();
        explanation.Text(L"Save the current selection for later use and make "
            L"the new named scope active.");
        explanation.TextWrapping(TextWrapping::Wrap);
        panel.Children().Append(explanation);
        auto field = Controls::TextBox();
        field.PlaceholderText(L"For example: High mileage cars");
        panel.Children().Append(field);
        auto dialog = Controls::ContentDialog();
        dialog.Title(box_value(L"Save Current Selection as Scope"));
        dialog.Content(panel);
        dialog.PrimaryButtonText(L"Save");
        dialog.CloseButtonText(L"Cancel");
        dialog.DefaultButton(Controls::ContentDialogButton::Primary);
        dialog.XamlRoot(owner.XamlRoot());
        if (co_await dialog.ShowAsync() != Controls::ContentDialogResult::Primary)
            co_return;
        const std::string name = to_string(field.Text());
        std::string validationError;
        if (!::rlispstat::core::ValidateAnalysisScopeSelectionName(name, &validationError))
        {
            MessageBoxW(nullptr, to_hstring(validationError).c_str(),
                L"Save Analysis Scope", MB_OK | MB_ICONWARNING);
            co_return;
        }
        const std::string reply = commandDispatcher_->dispatch(
            {"SAVE_ANALYSIS_SCOPE_FROM_SELECTION", group, name, "data-sheet"});
        if (reply.rfind("ERR ", 0) == 0)
        {
            MessageBoxW(nullptr, to_hstring(reply.substr(4)).c_str(),
                L"Save Analysis Scope", MB_OK | MB_ICONWARNING);
            co_return;
        }
        if (auto sheet = dataSheets_.find(group);
            sheet != dataSheets_.end() && sheet->second)
        {
            sheet->second->SetOperationStatus(
                "Saved and applied scope '" + name + "' (" +
                std::to_string(selected.size()) + " observations).");
        }
        QueueCommandStateRefresh();
    }

    void App::UpdateSnapshotAlbumNotesPanelWidth()
    {
        if (!snapshotAlbumWindow_ || !snapshotAlbumTabs_) return;
        bool shouldExpand = false;
        auto tab = snapshotAlbumTabs_.SelectedItem().try_as<Controls::TabViewItem>();
        if (tab)
        {
            const std::string identifier = to_string(
                unbox_value_or<hstring>(tab.Tag(), L""));
            auto const* snapshot = snapshotAlbum_.find(identifier);
            shouldExpand = snapshot && snapshot->note && snapshot->note->visible;
        }
        if (shouldExpand == snapshotAlbumNotesExpanded_) return;
        snapshotAlbumNotesExpanded_ = shouldExpand;
        ResizeLinkEDAWindowClient(snapshotAlbumWindow_, shouldExpand ? 1350 : 1080, 700);
    }

    void App::ExportSnapshotAlbum()
    {
        if (snapshotAlbum_.exportItems().empty())
        {
            MessageBoxW(nullptr, L"No snapshots are included in export.",
                L"Export Snapshot Album", MB_OK | MB_ICONINFORMATION);
            return;
        }
        com_ptr<IFileSaveDialog> dialog;
        if (FAILED(CoCreateInstance(CLSID_FileSaveDialog, nullptr, CLSCTX_INPROC_SERVER,
            IID_PPV_ARGS(dialog.put())))) return;
        const COMDLG_FILTERSPEC filters[]{
            {L"PDF document", L"*.pdf"},
            {L"SVG files", L"*.svg"},
            {L"High-resolution PNG files", L"*.png"}
        };
        dialog->SetFileTypes(static_cast<UINT>(std::size(filters)), filters);
        dialog->SetFileTypeIndex(1);
        dialog->SetDefaultExtension(L"pdf");
        dialog->SetFileName(L"LinkEDA-Snapshot-Album.pdf");
        dialog->SetTitle(L"Export Snapshot Album");
        FILEOPENDIALOGOPTIONS options{}; dialog->GetOptions(&options);
        dialog->SetOptions(options | FOS_FORCEFILESYSTEM | FOS_OVERWRITEPROMPT |
            FOS_STRICTFILETYPES);
        if (dialog->Show(nullptr) != S_OK) return;
        UINT choice = 1; dialog->GetFileTypeIndex(&choice);
        com_ptr<IShellItem> selected; if (FAILED(dialog->GetResult(selected.put()))) return;
        PWSTR rawPath = nullptr;
        if (FAILED(selected->GetDisplayName(SIGDN_FILESYSPATH, &rawPath)) || !rawPath) return;
        std::filesystem::path selectedPath(rawPath); CoTaskMemFree(rawPath);
        if (choice == 1)
        {
            ExportSnapshotAlbumPdfAsync(selectedPath.wstring());
            return;
        }
        // Multi-snapshot raster/vector formats are written to a named folder
        // beside the chosen placeholder file so ordering and manifest remain
        // unambiguous and no unrelated files are overwritten.
        selectedPath.replace_extension();
        const std::filesystem::path folder = selectedPath;
        std::error_code error; std::filesystem::create_directories(folder, error);
        if (error) return;
        if (choice == 3)
        {
            ExportSnapshotAlbumPngAsync(folder);
            return;
        }
        if (choice != 2) return;
        ExportSnapshotAlbumSvgAsync(folder);
    }

    Windows::Foundation::IAsyncOperation<Windows::Storage::Streams::InMemoryRandomAccessStream>
        App::RenderSnapshotNativePlotToPngAsync(std::string snapshotId,
                                                double width,
                                                double height,
                                                double scale)
    {
        using Microsoft::UI::Xaml::Media::Imaging::RenderTargetBitmap;
        using Windows::Graphics::Imaging::BitmapAlphaMode;
        using Windows::Graphics::Imaging::BitmapEncoder;
        using Windows::Graphics::Imaging::BitmapPixelFormat;
        using Windows::Storage::Streams::DataReader;
        using Windows::Storage::Streams::InMemoryRandomAccessStream;

        auto found = snapshotAlbumPlotViews_.find(snapshotId);
        if (found == snapshotAlbumPlotViews_.end() || !found->second || !snapshotAlbumTabs_)
            co_return InMemoryRandomAccessStream{nullptr};

        auto previous = snapshotAlbumTabs_.SelectedItem();
        for (uint32_t index = 0; index < snapshotAlbumTabs_.TabItems().Size(); ++index)
        {
            auto tab = snapshotAlbumTabs_.TabItems().GetAt(index)
                .try_as<Controls::TabViewItem>();
            if (!tab) continue;
            if (to_string(unbox_value_or<hstring>(tab.Tag(), L"")) == snapshotId)
            {
                snapshotAlbumTabs_.SelectedItem(tab);
                break;
            }
        }

        width = std::max(320.0, width);
        height = std::max(240.0, height);
        scale = std::clamp(scale, 1.0, 4.0);
        auto view = found->second;
        auto root = view->ContentRoot();
        const double previousWidth = root.Width();
        const double previousHeight = root.Height();
        view->ResizeSnapshotHost(width, height);
        const auto restoreAlbumView = [&]()
        {
            view->ResizeSnapshotHost(
                std::isfinite(previousWidth) ? previousWidth : width,
                std::isfinite(previousHeight) ? previousHeight : height);
            if (previous) snapshotAlbumTabs_.SelectedItem(previous);
        };

        try
        {
            // TabView only connects the selected page to the visual tree. Wait for
            // that page and its Canvas children to complete one native layout pass
            // before asking RenderTargetBitmap for pixels.
            co_await winrt::resume_after(std::chrono::milliseconds(40));
            co_await wil::resume_foreground(dispatcherQueue_);

            const int pixelWidth = static_cast<int>(std::lround(width * scale));
            const int pixelHeight = static_cast<int>(std::lround(height * scale));
            RenderTargetBitmap bitmap;
            co_await bitmap.RenderAsync(root, pixelWidth, pixelHeight);
            auto pixels = co_await bitmap.GetPixelsAsync();
            DataReader reader = DataReader::FromBuffer(pixels);
            std::vector<uint8_t> bytes(pixels.Length());
            reader.ReadBytes(bytes);

            InMemoryRandomAccessStream stream;
            auto encoder = co_await BitmapEncoder::CreateAsync(
                BitmapEncoder::PngEncoderId(), stream);
            encoder.SetPixelData(BitmapPixelFormat::Bgra8,
                BitmapAlphaMode::Premultiplied,
                static_cast<uint32_t>(bitmap.PixelWidth()),
                static_cast<uint32_t>(bitmap.PixelHeight()),
                96.0 * scale, 96.0 * scale, bytes);
            co_await encoder.FlushAsync();
            stream.Seek(0);
            restoreAlbumView();
            co_return stream;
        }
        catch (...)
        {
            restoreAlbumView();
            throw;
        }
    }

    fire_and_forget App::ExportSnapshotAlbumSvgAsync(std::filesystem::path folder)
    {
        auto items = snapshotAlbum_.items();
        std::ofstream manifest(folder / L"manifest.csv", std::ios::binary);
        manifest << ::rlispstat::core::SnapshotAlbumManifestCSV(snapshotAlbum_);
        bool ok = true;
        std::size_t order = 0;
        try
        {
            for (auto const& snapshot : items)
            {
                if (!snapshot.metadata.include_in_export) continue;
                ++order;
                std::string svg;
                if (auto const* plot = std::get_if<::rlispstat::core::FrozenPlotContent>(
                        &snapshot.renderable_content))
                {
                    auto stream = co_await RenderSnapshotNativePlotToPngAsync(
                        snapshot.metadata.snapshot_id,
                        plot->preferred_width, plot->preferred_height);
                    if (!stream) { ok = false; break; }
                    const uint32_t length = static_cast<uint32_t>(stream.Size());
                    Windows::Storage::Streams::DataReader reader(stream.GetInputStreamAt(0));
                    co_await reader.LoadAsync(length);
                    std::vector<uint8_t> bytes(length); reader.ReadBytes(bytes);
                    svg = SnapshotNativePngSvgDocument(snapshot, bytes,
                        plot->preferred_width, plot->preferred_height);
                }
                else svg = SnapshotSvgDocument(snapshot);

                std::ostringstream prefix;
                prefix << std::setw(3) << std::setfill('0') << order;
                const std::string title = snapshot.metadata.title.empty()
                    ? snapshot.metadata.default_title : snapshot.metadata.title;
                std::ofstream file(folder / to_hstring(prefix.str() + "-" +
                    SnapshotFileStem(title, order) + ".svg").c_str(), std::ios::binary);
                file << svg;
                ok = file.good();
                if (!ok) break;
            }
        }
        catch (...) { ok = false; }
        MessageBoxW(nullptr, ok ? L"The SVG snapshots and manifest.csv were exported."
            : L"LinkEDA could not export every SVG snapshot.",
            L"Export Snapshot Album", MB_OK | (ok ? MB_ICONINFORMATION : MB_ICONERROR));
    }

    Windows::Foundation::IAsyncOperation<Windows::Storage::Streams::InMemoryRandomAccessStream>
        App::RenderSnapshotSvgToPngAsync(std::string svg, double width, double height)
    {
        if (!snapshotAlbumExportWebView_)
            co_return Windows::Storage::Streams::InMemoryRandomAccessStream{nullptr};
        const double scale = 300.0 / 96.0;
        snapshotAlbumExportWebView_.Width(std::clamp(width * scale, 1000.0, 5000.0));
        snapshotAlbumExportWebView_.Height(std::clamp(height * scale, 750.0, 5000.0));
        snapshotAlbumExportWebView_.Visibility(Visibility::Visible);
        co_await snapshotAlbumExportWebView_.EnsureCoreWebView2Async();
        const std::string html =
            "<!doctype html><html><head><meta charset=\"utf-8\"><style>"
            "html,body{margin:0;width:100%;height:100%;overflow:hidden;background:white;}"
            "svg{display:block;width:100%;height:100%;}"
            "</style></head><body>" + svg + "</body></html>";
        winrt::handle completed(CreateEventW(nullptr, TRUE, FALSE, nullptr));
        event_token token{};
        token = snapshotAlbumExportWebView_.NavigationCompleted(
            [completed = completed.get()](auto const&, auto const&) { SetEvent(completed); });
        snapshotAlbumExportWebView_.NavigateToString(to_hstring(html));
        co_await winrt::resume_on_signal(completed.get());
        co_await wil::resume_foreground(dispatcherQueue_);
        snapshotAlbumExportWebView_.NavigationCompleted(token);
        Windows::Storage::Streams::InMemoryRandomAccessStream stream;
        co_await snapshotAlbumExportWebView_.CoreWebView2().CapturePreviewAsync(
            Microsoft::Web::WebView2::Core::CoreWebView2CapturePreviewImageFormat::Png,
            stream);
        snapshotAlbumExportWebView_.Visibility(Visibility::Collapsed);
        stream.Seek(0);
        co_return stream;
    }

    fire_and_forget App::ExportSnapshotAlbumPngAsync(std::filesystem::path folder)
    {
        auto items = snapshotAlbum_.items();
        std::ofstream manifest(folder / L"manifest.csv", std::ios::binary);
        manifest << ::rlispstat::core::SnapshotAlbumManifestCSV(snapshotAlbum_);
        bool ok = true; std::size_t order = 0;
        try
        {
            for (auto const& snapshot : items)
            {
                if (!snapshot.metadata.include_in_export) continue;
                ++order;
                std::string svg;
                std::vector<uint8_t> nativePlotPng;
                auto layout = ::rlispstat::core::BuildVectorTableLayout("", "x", 1200.0);
                double width = 1200.0, height = 850.0;
                if (auto const* plot = std::get_if<::rlispstat::core::FrozenPlotContent>(
                        &snapshot.renderable_content))
                {
                    width = plot->preferred_width; height = plot->preferred_height;
                    auto native = co_await RenderSnapshotNativePlotToPngAsync(
                        snapshot.metadata.snapshot_id, width, height);
                    if (!native) { ok = false; break; }
                    const uint32_t length = static_cast<uint32_t>(native.Size());
                    Windows::Storage::Streams::DataReader reader(native.GetInputStreamAt(0));
                    co_await reader.LoadAsync(length);
                    nativePlotPng.resize(length); reader.ReadBytes(nativePlotPng);
                    svg = SnapshotNativePngSvgDocument(snapshot, nativePlotPng, width, height);
                }
                else if (auto const* table = std::get_if<::rlispstat::core::FrozenTableContent>(
                             &snapshot.renderable_content))
                {
                    layout = ::rlispstat::core::BuildVectorTableLayout(snapshot.metadata.title,
                        ::rlispstat::core::FrozenTableToTabDelimited(*table), 1200.0);
                    width = layout.dimensions.widthPoints;
                    height = layout.dimensions.heightPoints;
                    svg = SnapshotSvgDocument(snapshot);
                }
                else if (auto const* image = std::get_if<
                             ::rlispstat::core::FrozenImageContent>(
                                 &snapshot.renderable_content))
                {
                    width = image->preferred_width;
                    height = image->preferred_height;
                    svg = SnapshotSvgDocument(snapshot);
                }
                else if (auto const* vector = std::get_if<
                             ::rlispstat::core::FrozenSvgContent>(
                                 &snapshot.renderable_content))
                {
                    width = vector->preferred_width;
                    height = vector->preferred_height;
                    svg = SnapshotSvgDocument(snapshot);
                }
                else svg = SnapshotSvgDocument(snapshot);
                auto stream = co_await RenderSnapshotSvgToPngAsync(svg, width, height);
                if (!stream) { ok = false; break; }
                const uint32_t length = static_cast<uint32_t>(stream.Size());
                Windows::Storage::Streams::DataReader reader(stream.GetInputStreamAt(0));
                co_await reader.LoadAsync(length);
                std::vector<uint8_t> bytes(length); reader.ReadBytes(bytes);
                std::ostringstream prefix; prefix << std::setw(3) << std::setfill('0') << order;
                const std::string title = snapshot.metadata.title.empty()
                    ? snapshot.metadata.default_title : snapshot.metadata.title;
                std::ofstream file(folder / to_hstring(prefix.str() + "-" +
                    SnapshotFileStem(title, order) + ".png").c_str(), std::ios::binary);
                file.write(reinterpret_cast<char const*>(bytes.data()),
                    static_cast<std::streamsize>(bytes.size()));
                ok = file.good(); if (!ok) break;
            }
        }
        catch (...) { ok = false; }
        if (snapshotAlbumExportWebView_)
            snapshotAlbumExportWebView_.Visibility(Visibility::Collapsed);
        MessageBoxW(nullptr, ok ? L"The high-resolution PNG snapshots and manifest.csv were exported."
            : L"LinkEDA could not export every PNG snapshot.",
            L"Export Snapshot Album", MB_OK | (ok ? MB_ICONINFORMATION : MB_ICONERROR));
    }

    fire_and_forget App::ExportSnapshotAlbumPdfAsync(std::wstring path)
    {
        if (!snapshotAlbumExportWebView_) co_return;
        bool ok = false;
        try
        {
            std::string html =
                "<!doctype html><html><head><meta charset=\"utf-8\">"
                "<title>LinkEDA Snapshot Album</title><style>"
                "@page{size:A4 landscape;margin:0;}html,body{margin:0;background:white;}"
                ".page{box-sizing:border-box;width:100vw;height:100vh;padding:28px;"
                "display:flex;align-items:center;justify-content:center;page-break-after:always;}"
                ".page:last-child{page-break-after:auto}.page svg{max-width:100%;max-height:100%;}"
                "</style></head><body>";
            auto items = snapshotAlbum_.items();
            for (auto const& snapshot : items)
            {
                if (!snapshot.metadata.include_in_export) continue;
                std::string svg;
                if (auto const* plot = std::get_if<::rlispstat::core::FrozenPlotContent>(
                        &snapshot.renderable_content))
                {
                    auto native = co_await RenderSnapshotNativePlotToPngAsync(
                        snapshot.metadata.snapshot_id,
                        plot->preferred_width, plot->preferred_height);
                    if (!native) throw hresult_error(E_FAIL, L"Native snapshot capture failed");
                    const uint32_t length = static_cast<uint32_t>(native.Size());
                    Windows::Storage::Streams::DataReader reader(native.GetInputStreamAt(0));
                    co_await reader.LoadAsync(length);
                    std::vector<uint8_t> bytes(length); reader.ReadBytes(bytes);
                    svg = SnapshotNativePngSvgDocument(snapshot, bytes,
                        plot->preferred_width, plot->preferred_height);
                }
                else svg = SnapshotSvgDocument(snapshot);
                html += "<section class=\"page\">" + svg + "</section>";
            }
            html += "</body></html>";
            snapshotAlbumExportWebView_.Width(1123); snapshotAlbumExportWebView_.Height(794);
            snapshotAlbumExportWebView_.Visibility(Visibility::Visible);
            co_await snapshotAlbumExportWebView_.EnsureCoreWebView2Async();
            winrt::handle completed(CreateEventW(nullptr, TRUE, FALSE, nullptr));
            event_token token{};
            token = snapshotAlbumExportWebView_.NavigationCompleted(
                [completed = completed.get()](auto const&, auto const&) { SetEvent(completed); });
            snapshotAlbumExportWebView_.NavigateToString(to_hstring(html));
            co_await winrt::resume_on_signal(completed.get());
            co_await wil::resume_foreground(dispatcherQueue_);
            snapshotAlbumExportWebView_.NavigationCompleted(token);
            auto settings = snapshotAlbumExportWebView_.CoreWebView2().Environment().CreatePrintSettings();
            settings.ShouldPrintBackgrounds(true); settings.ShouldPrintHeaderAndFooter(false);
            settings.MarginTop(0); settings.MarginRight(0); settings.MarginBottom(0); settings.MarginLeft(0);
            settings.PageWidth(11.69); settings.PageHeight(8.27);
            ok = co_await snapshotAlbumExportWebView_.CoreWebView2().PrintToPdfAsync(path, settings);
        }
        catch (...) { ok = false; }
        if (snapshotAlbumExportWebView_)
            snapshotAlbumExportWebView_.Visibility(Visibility::Collapsed);
        MessageBoxW(nullptr, ok ? L"The multipage PDF was exported."
            : L"LinkEDA could not export the Snapshot Album PDF.",
            L"Export Snapshot Album", MB_OK | (ok ? MB_ICONINFORMATION : MB_ICONERROR));
    }

    fire_and_forget App::AddActiveSnapshot(
        std::string kind, std::string sourceId, std::string group)
    {
        auto lifetime = get_strong();
        LogStartup("Snapshot add requested: kind=" + kind + ",source=" +
            sourceId + ",group=" + group);
        if (!commandDispatcher_ || sourceId.empty())
        {
            LogStartup("Snapshot add rejected: dispatcher or source id unavailable");
            co_return;
        }
        ::rlispstat::core::SnapshotItem item;
        item.metadata.source_view_id = sourceId;
        item.metadata.dataset_id = group;
        item.metadata.created_at = std::chrono::system_clock::now();
        item.metadata.theme_name = commandDispatcher_->plotTheme();
        if (!group.empty())
            item.metadata.source_scope = commandDispatcher_->applicationState().activeAnalysisScope(group);

        if (kind == "plot")
        {
            std::optional<::rlispstat::core::PlotModel> frozen;
            // The command means "this window", so the rendered view is the
            // primary source. Its exported model also contains the exact
            // point size, local colours and labels visible at click time.
            if (auto view = views_.find(sourceId);
                view != views_.end() && view->second)
                frozen = view->second->ExportModelSnapshot();
            else if (auto model = plots_.find(sourceId);
                     model != plots_.end() && model->second)
                frozen = *model->second;
            else
            {
                // A rebuilt context menu can briefly retain the preceding
                // source id. Resolve that safely only when the dataset owns a
                // single live plot; never guess between multiple windows.
                std::shared_ptr<ScatterPlotView> soleGroupView;
                for (auto const& [id, candidate] : views_)
                {
                    if (!candidate || candidate->Group() != group) continue;
                    if (soleGroupView) { soleGroupView.reset(); break; }
                    soleGroupView = candidate;
                }
                if (soleGroupView) frozen = soleGroupView->ExportModelSnapshot();
            }
            if (!frozen)
            {
                LogStartup("Snapshot add rejected: plot view/model not found for " + sourceId);
                MessageBoxW(nullptr,
                    L"The active plot could not be frozen. Please activate the plot and try again.",
                    L"Add to Snapshot Album", MB_OK | MB_ICONWARNING);
                co_return;
            }
            item.metadata.source_view_id = frozen->id.empty() ? sourceId : frozen->id;
            item.content_kind = frozen->kind == "trellis_scatterplot"
                ? ::rlispstat::core::SnapshotContentKind::Trellis
                : ::rlispstat::core::SnapshotContentKind::Plot;
            item.metadata.title = frozen->title;
            item.metadata.default_title = frozen->title.empty()
                ? ::rlispstat::core::SnapshotContentKindName(item.content_kind)
                : frozen->title;
            item.metadata.source_description = frozen->kind;
            item.renderable_content = ::rlispstat::core::FrozenPlotContent{
                std::move(*frozen), 720, 520 };
        }
        else if (kind == "data")
        {
            auto const* dataframe = commandDispatcher_->applicationState().datasets().find(group);
            if (!dataframe) co_return;
            item.content_kind = ::rlispstat::core::SnapshotContentKind::DataTable;
            item.metadata.title = "Data: " + group;
            item.metadata.default_title = item.metadata.title;
            item.metadata.source_description = "Data Sheet";
            auto table = ::rlispstat::core::FrozenTableFromTabDelimited(
                DataFrameSnapshotText(*dataframe));
            table.source_row_count = static_cast<std::size_t>(std::max(0, dataframe->rows));
            table.total_source_row_count = table.source_row_count;
            item.renderable_content = std::move(table);
        }
        else if (kind == "output")
        {
            item.content_kind =
                ::rlispstat::core::SnapshotContentKind::StatisticalTable;
            item.metadata.source_description = "Analysis Table";
            item.metadata.title = WindowSnapshotTitle(kind, sourceId, group);
            auto correlation = commandDispatcher_->applicationState()
                .correlationMatrices().find(sourceId);
            if (correlation != commandDispatcher_->applicationState()
                    .correlationMatrices().end())
            {
                item.metadata.title = correlation->second.title;
                item.metadata.source_description = "Correlation Matrix";
            }
            else if (auto found = outputStates_.find(sourceId);
                     found != outputStates_.end())
            {
                item.metadata.title = found->second.title;
                item.metadata.source_description = "Table 1";
            }
            if (item.metadata.title.empty()) item.metadata.title = "Analysis Result";
            item.metadata.default_title = item.metadata.title;

            auto root = WindowSnapshotVisualRoot(kind, sourceId, group);
            if (!root)
            {
                LogStartup("Snapshot add rejected: output visual not found for " + sourceId);
                MessageBoxW(nullptr,
                    L"The table window could not be captured. Please activate it and try again.",
                    L"Add to Snapshot Album", MB_OK | MB_ICONWARNING);
                co_return;
            }
            root.UpdateLayout();
            const double width = std::max(1.0, root.ActualWidth());
            const double height = std::max(1.0, root.ActualHeight());
            try
            {
                std::string svg = WindowSnapshotVectorSvg(root);
                if (svg.empty()) throw hresult_error(E_FAIL);
                item.renderable_content = ::rlispstat::core::FrozenSvgContent{
                    std::move(svg), width, height };
            }
            catch (...)
            {
                LogStartup("Snapshot add rejected: output vector capture failed for " +
                    sourceId);
                MessageBoxW(nullptr,
                    L"LinkEDA could not freeze the table as vector artwork.",
                    L"Add to Snapshot Album", MB_OK | MB_ICONWARNING);
                co_return;
            }
        }
        else co_return;

        if (auto note = WindowSnapshotNote(kind, sourceId, group);
            note && note->has_content)
        {
            item.note = *note;
            item.metadata.include_notes = true;
        }
        item.stickers = WindowSnapshotStickers(kind, sourceId, group);
        if (!item.stickers.empty()) item.metadata.include_notes = true;

        auto& added = snapshotAlbum_.add(std::move(item));
        LogStartup("Snapshot added: id=" + added.metadata.snapshot_id +
            ",source=" + added.metadata.source_view_id +
            ",type=" + added.metadata.source_description);
        snapshotAlbumSelectionId_ = added.metadata.snapshot_id;
        ShowSnapshotAlbum();
        co_return;
    }

    bool App::ApplySelectedRows(std::string const& group, std::set<int> const& rows,
                                ::rlispstat::core::SelectionMode mode)
    {
        if (!commandDispatcher_) return false;
        const auto coordination = commandDispatcher_->plotCoordinator().applySelection(
            group, rows, mode);
        if (!coordination.accepted) return false;
        RefreshSelectionVisuals(coordination.event);
        for (auto const& [plotId, model] : plots_)
        {
            if (!model || model->group != group) continue;
            if (model->kind == "scatter_matrix" &&
                ::rlispstat::core::HasOverlaySource(*model, "selected"))
            {
                ::rlispstat::core::InvalidateScatterMatrixFits(*model);
                QueueSmoothRecompute(*model,
                    ::rlispstat::core::SmoothCurveScope::Selection, "lm");
                continue;
            }
            if (::rlispstat::core::MarkSmoothCurveScopePendingIfPresent(
                    model->smoothCurves,
                    ::rlispstat::core::SmoothCurveScope::Selection))
                QueueSmoothRecompute(*model,
                    ::rlispstat::core::SmoothCurveScope::Selection);
        }
        LogStartup("Selection changed for " + group + ": "
            + ::rlispstat::core::CaseSetText(coordination.event.selectedRows));
        return true;
    }

    void App::QueueSmoothRecompute(::rlispstat::core::PlotModel& model,
                                   ::rlispstat::core::SmoothCurveScope scope,
                                   std::string const& fitMethod)
    {
        if (model.kind == "scatter_matrix" && fitMethod.empty()) {
            QueueSmoothRecompute(model, scope, "lm");
            return;
        }
        if (fitMethod.empty()) {
            std::vector<std::string> methods;
            for (auto const& curve : model.smoothCurves)
                if (curve.scope == scope && !curve.ok && !curve.message.empty() &&
                    std::find(methods.begin(), methods.end(), curve.fitMethod) == methods.end())
                    methods.push_back(curve.fitMethod);
            for (auto const& method : methods) QueueSmoothRecompute(model, scope, method);
            return;
        }
        std::set<int> selected;
        commandDispatcher_->applicationState().selectedRows(model.group, selected);
        const auto colors = commandDispatcher_->applicationState().displayPointColors(model.group);
        if (model.kind == "scatter_matrix")
        {
            if (!model.scatterMatrixFitsPending) return;
            pendingMainRTasks_.trellisSmoothTasks.erase(
                std::remove_if(pendingMainRTasks_.trellisSmoothTasks.begin(),
                    pendingMainRTasks_.trellisSmoothTasks.end(),
                    [&](auto const& task) {
                        return task.plotId == model.id &&
                            task.panelId.rfind("matrix:", 0) == 0;
                    }), pendingMainRTasks_.trellisSmoothTasks.end());
            const auto variables = ::rlispstat::core::ScatterMatrixVariablesForModel(model);
            const auto rows = ::rlispstat::core::ScatterMatrixVisibleCaseIdsForModel(model);
            const auto series =
                ::rlispstat::core::ScatterMatrixVariableSeriesForModel(model, variables);
            if (series.size() < 2) {
                model.scatterMatrixFitsPending = false;
                return;
            }
            const std::vector<int> selectedRows(selected.begin(), selected.end());
            for (std::size_t row = 0; row < series.size(); ++row)
            {
                for (std::size_t column = 0; column < series.size(); ++column)
                {
                    if (row == column) continue;
                    std::vector<::rlispstat::core::MainRSmoothPoint> points;
                    std::vector<int> panelRows;
                    points.reserve(rows.size());
                    panelRows.reserve(rows.size());
                    for (const auto caseId : rows)
                    {
                        if (caseId <= 0) continue;
                        const auto index = static_cast<std::size_t>(caseId - 1);
                        if (index >= series[column].values.size() ||
                            index >= series[row].values.size()) continue;
                        const double x = series[column].values[index];
                        const double y = series[row].values[index];
                        if (!std::isfinite(x) || !std::isfinite(y)) continue;
                        points.push_back({caseId, x, y});
                        panelRows.push_back(caseId);
                    }
                    for (auto const& overlay : model.overlays)
                    {
                        if (overlay.type != "lm" || !overlay.visible) continue;
                        ::rlispstat::core::SmoothCurveScope overlayScope =
                            ::rlispstat::core::SmoothCurveScope::Overall;
                        if (overlay.source == "selected")
                            overlayScope = ::rlispstat::core::SmoothCurveScope::Selection;
                        else if (overlay.source == "color")
                            overlayScope = ::rlispstat::core::SmoothCurveScope::ColorGroup;
                        else if (overlay.source != "all") continue;
                        ::rlispstat::core::MainRTrellisSmoothTask task{
                            model.id, model.group,
                            ::rlispstat::core::ScatterMatrixFitPanelId(model, row, column),
                            overlayScope, series[column].name, series[row].name,
                            model.smoothSpan, panelRows, {}, selectedRows, colors,
                            "lm", model.scatterFitConfidenceLevel
                        };
                        task.useExplicitPoints = true;
                        task.explicitPoints = points;
                        pendingMainRTasks_.trellisSmoothTasks.push_back(std::move(task));
                    }
                }
            }
            model.scatterMatrixFitsPending = false;
            MarkMainRTaskPending();
            return;
        }
        if (model.kind == "trellis_scatterplot" && model.trellisSpecificationInitialized &&
            model.trellisSpecification.plotType == ::rlispstat::core::TrellisPlotType::Scatter)
        {
            const std::vector<int> selectedRows(selected.begin(), selected.end());
            std::map<std::string, std::string> splitColorByLevel;
            const auto splitLevels = ::rlispstat::core::OrderedUniqueBarplotLevels(
                model.trellisPointSplitValues);
            const auto palette = ::rlispstat::core::BarplotPaletteOrder();
            for (std::size_t index = 0; index < splitLevels.size(); ++index)
                if (!palette.empty())
                    splitColorByLevel[splitLevels[index]] =
                        palette[index % palette.size()];
            for (auto const& panelId : model.trellisPanelLevels)
            {
                std::vector<int> rows;
                std::vector<std::string> splitColors;
                for (std::size_t index = 0; index < model.points.size(); ++index)
                {
                    if (index >= model.trellisPointPanels.size() ||
                        model.trellisPointPanels[index] != panelId) continue;
                    rows.push_back(model.points[index].row);
                    if (!model.trellisSpecification.splitVariableId.empty() &&
                        index < model.trellisPointSplitValues.size())
                        splitColors.push_back(
                            splitColorByLevel[model.trellisPointSplitValues[index]]);
                }
                if (rows.empty()) continue;
                pendingMainRTasks_.trellisSmoothTasks.push_back({
                    model.id, model.group, panelId, scope,
                    model.trellisSpecification.xVariableId,
                    model.trellisSpecification.yVariableId,
                    model.smoothSpan, std::move(rows), std::move(splitColors),
                    selectedRows, colors, fitMethod,
                    model.scatterFitConfidenceLevel });
            }
            MarkMainRTaskPending();
            return;
        }
        ::rlispstat::core::MainRSmoothTask task{
            model.id, model.group, scope, model.xLabel, model.yLabel, model.smoothSpan,
            std::vector<int>(selected.begin(), selected.end()), colors };
        task.fitMethod = fitMethod;
        task.confidenceLevel = model.scatterFitConfidenceLevel;
        std::vector<::rlispstat::core::ScatterplotImputationPointSet> pointSets;
        if (model.isGLMDiagnostic)
        {
            pointSets =
                ::rlispstat::core::BuildRegressionDiagnosticSmoothPointSets(model);
        }
        else if (model.kind == "scatter")
        {
            ::rlispstat::core::ScatterplotRenderInput imputationInput;
            imputationInput.points.reserve(model.points.size());
            for (auto const& point : model.points)
                imputationInput.points.push_back({ point.row, point.x, point.y });
            auto const* dataframe =
                commandDispatcher_->applicationState().datasets().find(model.group);
            if (dataframe && ::rlispstat::core::DataFrameShowsAllImputations(*dataframe))
            {
                imputationInput.imputationDataFrame = dataframe;
                imputationInput.xColumn = ::rlispstat::core::FindDataColumnInDataFrame(
                    *dataframe, model.xLabel);
                imputationInput.yColumn = ::rlispstat::core::FindDataColumnInDataFrame(
                    *dataframe, model.yLabel);
            }
            pointSets =
                ::rlispstat::core::BuildScatterplotImputationPointSets(imputationInput);
        }
        ::rlispstat::core::PopulateMainRSmoothTaskImputationPointSets(
            task, pointSets);
        pendingMainRTasks_.smoothTasks.push_back(std::move(task));
        MarkMainRTaskPending();
    }

    void App::QueueExistingSmoothRecompute(::rlispstat::core::PlotModel& model)
    {
        if (model.kind == "scatter_matrix")
        {
            ::rlispstat::core::InvalidateScatterMatrixFits(model);
            if (model.scatterMatrixFitsPending)
                QueueSmoothRecompute(model,
                    ::rlispstat::core::SmoothCurveScope::Overall, "lm");
            return;
        }
        const auto hasScope = [&](::rlispstat::core::SmoothCurveScope scope)
        {
            return std::any_of(model.smoothCurves.begin(), model.smoothCurves.end(),
                [scope](auto const& curve) { return curve.scope == scope; });
        };
        const bool overall = hasScope(::rlispstat::core::SmoothCurveScope::Overall);
        const bool selected = hasScope(::rlispstat::core::SmoothCurveScope::Selection);
        const bool color = hasScope(::rlispstat::core::SmoothCurveScope::ColorGroup);
        if (!::rlispstat::core::MarkExistingSmoothCurvesPending(model.smoothCurves)) return;
        if (overall) QueueSmoothRecompute(model, ::rlispstat::core::SmoothCurveScope::Overall);
        if (selected) QueueSmoothRecompute(model, ::rlispstat::core::SmoothCurveScope::Selection);
        if (color) QueueSmoothRecompute(model, ::rlispstat::core::SmoothCurveScope::ColorGroup);
    }

    void App::HandleViewClosed(std::string const& plotId)
    {
        if (resettingViews_ || plotId.empty()) return;
        if (auto plot = plots_.find(plotId); plot != plots_.end() && plot->second &&
            plot->second->isRegressionDerivedPlot)
            ForgetRegressionDerivedOutput(plot->second->glmModelId, "plot");
        if (applicationCommands_) applicationCommands_->UnregisterWindow("plot:" + plotId);
        const auto coordination = commandDispatcher_
            ? commandDispatcher_->plotCoordinator().detachPlot(plotId)
            : ::rlispstat::core::PlotCoordinationResult{};
        views_.erase(plotId);
        plots_.erase(plotId);
        LogStartup("Closed scatter view " + plotId
            + "; remaining views: " + std::to_string(views_.size())
            + "; group still active: "
            + (coordination.event.groupHasPlots ? "yes" : "no"));
    }

    void App::HandleDataSheetClosed(std::string const& group)
    {
        if (resettingViews_) return;
        if (auto dialog = scatterDialogs_.find(group); dialog != scatterDialogs_.end() && dialog->second)
            dialog->second->Close();
        if (auto dialog = descriptiveDialogs_.find(group); dialog != descriptiveDialogs_.end() && dialog->second)
            dialog->second->Close();
        if (auto dialog = contingencyDialogs_.find(group); dialog != contingencyDialogs_.end() && dialog->second)
            dialog->second->Close();
        if (auto dialog = imputationDialogs_.find(group); dialog != imputationDialogs_.end() && dialog->second)
            dialog->second->Close();
        std::vector<std::shared_ptr<RDataAssignmentDialog>> returnDialogs;
        for (auto const& [key, dialog] : rDataAssignmentDialogs_)
            if (key.rfind(group + ":", 0) == 0 && dialog) returnDialogs.push_back(dialog);
        for (auto const& dialog : returnDialogs) dialog->Close();
        std::vector<std::shared_ptr<PlotWorkflowDialog>> plotDialogs;
        for (auto const& [key, dialog] : plotWorkflowDialogs_)
            if (key.rfind(group + ":", 0) == 0 && dialog) plotDialogs.push_back(dialog);
        for (auto const& dialog : plotDialogs) dialog->Close();
        std::vector<std::shared_ptr<AnalysisWorkflowDialog>> analysisDialogs;
        for (auto const& [key, dialog] : analysisWorkflowDialogs_)
            if (key.rfind(group + ":", 0) == 0 && dialog) analysisDialogs.push_back(dialog);
        for (auto const& dialog : analysisDialogs) dialog->Close();
        if (applicationCommands_) applicationCommands_->UnregisterWindow("data:" + group);
        dataSheets_.erase(group);
        LogStartup("Closed data sheet for " + group);
    }

    void App::OpenScatterplotDialog(std::string const& group)
    {
        if (!commandDispatcher_) return;
        auto sheet = dataSheets_.find(group);
        auto const* dataframe = commandDispatcher_->applicationState().datasets().find(group);
        if (sheet == dataSheets_.end() || !sheet->second || !dataframe) return;
        if (auto existing = scatterDialogs_.find(group);
            existing != scatterDialogs_.end() && existing->second)
        {
            return;
        }
        auto initial = ::rlispstat::core::ResolveInitialAnalysisSpecification(
            *dataframe,
            ::rlispstat::core::SharedAnalysisDefinition(
                ::rlispstat::core::SharedAnalysisKind::Scatterplot),
            ::rlispstat::core::DefaultVariableRolesForAnalysisInitialization(
                commandDispatcher_->applicationState().variableRoles(group)));
        auto dialog = ScatterplotDialog::Create(sheet->second->NativeWindow(), *dataframe,
            [this](ScatterplotSpecification const& specification)
            {
                CreateScatterplot(specification);
            },
            [this, group]() { scatterDialogs_.erase(group); },
            std::move(initial));
        scatterDialogs_[group] = dialog;
        dialog->Show();
    }

    void App::OpenPlotWorkflowDialog(std::string const& group, PlotWorkflowKind kind)
    {
        if (!commandDispatcher_) return;
        auto sheet = dataSheets_.find(group);
        auto const* dataframe = commandDispatcher_->applicationState().datasets().find(group);
        if (sheet == dataSheets_.end() || !sheet->second || !dataframe) return;
        const auto key = group + ":" + std::to_string(static_cast<int>(kind));
        if (auto existing = plotWorkflowDialogs_.find(key);
            existing != plotWorkflowDialogs_.end() && existing->second) return;
        auto initial = ::rlispstat::core::ResolveInitialAnalysisSpecification(
            *dataframe,
            ::rlispstat::core::SharedAnalysisDefinition(
                SharedPlotInitializationKind(kind)),
            ::rlispstat::core::DefaultVariableRolesForAnalysisInitialization(
                commandDispatcher_->applicationState().variableRoles(group)));
        auto dialog = PlotWorkflowDialog::Create(
            sheet->second->NativeWindow(), *dataframe, kind,
            [this](PlotWorkflowSpecification const& specification)
            { CreatePlotWorkflow(specification); },
            [this, key]() { plotWorkflowDialogs_.erase(key); }, "scatter",
            std::move(initial));
        plotWorkflowDialogs_[key] = dialog; dialog->Show();
    }

    void App::OpenTrellisPlotDialog(std::string const& group, std::string const& plotType)
    {
        if (!commandDispatcher_) return;
        if (dataSheets_.find(group) == dataSheets_.end()) ShowDataSheet(group);
        auto sheet = dataSheets_.find(group);
        auto const* dataframe = commandDispatcher_->applicationState().datasets().find(group);
        if (sheet == dataSheets_.end() || !sheet->second || !dataframe) return;
        const std::string key = group + ":trellis:" + plotType;
        if (auto existing = plotWorkflowDialogs_.find(key);
            existing != plotWorkflowDialogs_.end() && existing->second) return;
        auto initial = ::rlispstat::core::ResolveInitialAnalysisSpecification(
            *dataframe,
            ::rlispstat::core::SharedAnalysisDefinition(
                SharedPlotInitializationKind(PlotWorkflowKind::Trellis, plotType)),
            ::rlispstat::core::DefaultVariableRolesForAnalysisInitialization(
                commandDispatcher_->applicationState().variableRoles(group)));
        auto dialog = PlotWorkflowDialog::Create(
            sheet->second->NativeWindow(), *dataframe, PlotWorkflowKind::Trellis,
            [this](PlotWorkflowSpecification const& specification)
            { CreatePlotWorkflow(specification); },
            [this, key]() { plotWorkflowDialogs_.erase(key); }, plotType,
            std::move(initial));
        plotWorkflowDialogs_[key] = dialog;
        dialog->Show();
    }

    void App::CreatePlotWorkflow(PlotWorkflowSpecification const& specification)
    {
        if (!commandDispatcher_) return;
        auto const* dataframe = commandDispatcher_->applicationState().datasets().find(specification.group);
        if (!dataframe) return;
        if (specification.kind == PlotWorkflowKind::ScatterMatrix)
        { (void)CreateScatterMatrix(specification.group, specification.variables); return; }
        if (specification.kind == PlotWorkflowKind::ParallelCoordinates)
        {
            (void)CreateParallelCoordinates(specification.group, specification.variables,
                                            specification.standardize, specification.connect);
            return;
        }
        auto model = std::make_unique<::rlispstat::core::PlotModel>();
        ::rlispstat::core::PopulateDatasetSeedPlot(*model, *dataframe, specification.group);
        model->isDatasetSeed = false; model->interactionMode = "select";
        model->selectionMode = "replace"; model->overlays.clear(); model->smoothCurves.clear();
        model->id = "plot_windows_" + std::to_string(
            static_cast<unsigned long long>(GetTickCount64()));
        const auto* xColumn = [&]() -> ::rlispstat::core::DataColumn const*
        {
            for (auto const& column : dataframe->columns)
                if (column.name == specification.xVariable) return &column;
            return nullptr;
        }();
        const auto findColumn = [&](std::string const& name) -> ::rlispstat::core::DataColumn const*
        {
            for (auto const& column : dataframe->columns) if (column.name == name) return &column;
            return nullptr;
        };
        std::string error;
        if (specification.kind == PlotWorkflowKind::TimeSeries)
        {
            model->kind = "time_series"; model->xLabel = specification.xVariable;
            model->yLabel = specification.yVariable;
            model->timeSeriesGroupVariable = specification.groupVariable;
            model->timeSeriesTimeType = xColumn
                ? ::rlispstat::core::InferTimeSeriesTimeType(*xColumn) : "numeric";
            model->title = specification.title.empty()
                ? specification.yVariable + " over " + specification.xVariable
                : specification.title;
            if (!::rlispstat::core::RebuildTimeSeriesFromDataFrame(*model, *dataframe, &error)) return;
        }
        else if (specification.kind == PlotWorkflowKind::Trellis)
        {
            const auto type = ::rlispstat::core::ParseTrellisPlotType(specification.trellisType);
            if (!type || specification.conditionVariable.empty()) return;
            model->kind = "trellis_scatterplot"; model->xLabel = specification.xVariable;
            model->yLabel = specification.yVariable;
            model->trellisConditionVariable = specification.conditionVariable;
            model->trellisSpecification = {};
            model->trellisSpecification.plotType = *type;
            model->trellisSpecification.xVariableId = specification.xVariable;
            if (*type == ::rlispstat::core::TrellisPlotType::Boxplot)
                model->trellisSpecification.boxplotGroupingVariableIds = {specification.xVariable};
            model->trellisSpecification.yVariableId = specification.yVariable;
            ::rlispstat::core::TrellisConditioningVariable condition;
            condition.variableId = specification.conditionVariable;
            condition.variableLabel = specification.conditionVariable;
            const auto* conditionColumn = findColumn(specification.conditionVariable);
            if (!conditionColumn) return;
            condition.kind = ::rlispstat::core::TrellisConditioningKindForColumn(*conditionColumn);
            if (condition.kind ==
                ::rlispstat::core::TrellisConditioningVariableKind::ContinuousBinned)
            {
                ::rlispstat::core::TrellisContinuousBinningSpecification binning;
                binning.method = ::rlispstat::core::TrellisContinuousBinningMethod::EqualWidth;
                binning.binCount = 4;
                condition.binning = std::move(binning);
            }
            condition.dimension = ::rlispstat::core::TrellisDimension::Columns;
            model->trellisSpecification.conditioningVariables.push_back(condition);
            model->trellisSpecificationInitialized = true;
            if (!specification.title.empty())
            {
                model->trellisSpecification.hasCustomTitle = true;
                model->trellisSpecification.customTitle = specification.title;
            }
            if (!::rlispstat::core::RebuildTrellisPlotFromDataFrame(*model, *dataframe, &error)) return;
        }
        else if (specification.kind == PlotWorkflowKind::Boxplot)
        {
            const auto* yColumn = findColumn(specification.yVariable);
            const auto* groupColumn = findColumn(specification.groupVariable);
            if (!yColumn) return;
            model->kind = "boxplot"; model->xLabel = specification.groupVariable;
            model->yLabel = specification.yVariable;
            model->title = specification.title.empty()
                ? ::rlispstat::core::BoxplotDefaultTitle(model->yLabel, model->xLabel)
                : specification.title;
            model->boxplotPoints.clear(); model->boxplotVariables.clear();
            if (groupColumn) model->boxplotGroupingVariables = {specification.groupVariable};
            if (!groupColumn) model->boxplotVariables.push_back(model->yLabel);
            for (std::size_t index = 0; index < yColumn->values.size(); ++index)
            {
                char* end = nullptr; const double y = std::strtod(yColumn->values[index].c_str(), &end);
                if (end == yColumn->values[index].c_str() || !std::isfinite(y)) continue;
                const std::string category = groupColumn && index < groupColumn->values.size()
                    ? (groupColumn->values[index].empty() ? "NA" : groupColumn->values[index])
                    : model->yLabel;
                model->boxplotPoints.push_back({ y, category, static_cast<int>(index + 1) });
            }
            ::rlispstat::core::EnsureParallelBoxplotState(*model);
            model->boxplotCategories = ::rlispstat::core::CategoriesForPlot(*model);
            model->boxplotDefinedCategories = model->boxplotCategories;
        }
        else if (specification.kind == PlotWorkflowKind::Histogram)
        {
            const auto* variable = ::rlispstat::core::FindNumericVariable(*model, specification.xVariable);
            if (!variable) return;
            model->kind = "histogram"; model->xLabel = specification.xVariable; model->yLabel.clear();
            model->title = specification.title.empty()
                ? ::rlispstat::core::HistogramDefaultTitle(model->xLabel) : specification.title;
            model->histogramPoints.clear();
            for (std::size_t index = 0; index < variable->values.size(); ++index)
                if (std::isfinite(variable->values[index]))
                    model->histogramPoints.push_back({ variable->values[index], static_cast<int>(index + 1), 0 });
            if (model->histogramPoints.empty()) return;
            if (specification.bins > 0) ::rlispstat::core::RebinHistogram(*model, specification.bins);
            else ::rlispstat::core::RebinHistogramByRule(*model, "sturges");
        }
        else if (specification.kind == PlotWorkflowKind::BarChart)
        {
            std::vector<std::string> xNames{ specification.secondaryVariable };
            if (!specification.groupVariable.empty() && specification.groupVariable != xNames.front())
                xNames.push_back(specification.groupVariable);
            std::vector<::rlispstat::core::DataColumn const*> xColumns;
            for (auto const& name : xNames) if (auto column = findColumn(name)) xColumns.push_back(column);
            const auto* split = findColumn(specification.conditionVariable);
            if (xColumns.size() != xNames.size()) return;
            model->kind = "barplot"; model->barplotXVariables = xNames;
            model->barplotSplitVariable = split ? specification.conditionVariable : "";
            model->xLabel.clear();
            for (std::size_t index = 0; index < xNames.size(); ++index)
                model->xLabel += (index ? " + " : "") + xNames[index];
            model->title = specification.title.empty()
                ? ::rlispstat::core::BarplotDefaultTitle(model->xLabel, model->barplotSplitVariable)
                : specification.title;
            model->barplotRowColorDisplay = "hide";
            model->barplotShowConditionalPercent = split != nullptr;
            ::rlispstat::core::RebuildBarplotBinsForColumns(*model, xColumns, split);
            ::rlispstat::core::NormalizeBarplotVisualState(*model);
            if (model->barplotBins.empty()) return;
        }
        else return;

        if (specification.dataScopeCaptured)
        {
            model->dataScope = specification.dataScope;
            model->dataScopeCaptured = true;
        }

        auto* pointer = model.get(); plots_[model->id] = std::move(model);
        const auto attached = AttachPlotModel(*pointer);
        if (!attached.accepted) { plots_.erase(pointer->id); return; }
        commandDispatcher_->plotCoordinator().activatePlot(pointer->id); ShowNativePlot(*pointer);
    }

    void App::OpenAnalysisWorkflowDialog(std::string const& group, AnalysisWorkflowKind kind)
    {
        if (!commandDispatcher_) return;
        const auto* dataframe = commandDispatcher_->applicationState().datasets().find(group);
        if (!dataframe) return;
        if (dataSheets_.find(group) == dataSheets_.end()) ShowDataSheet(group);
        auto owner = dataSheets_.find(group);
        if (owner == dataSheets_.end() || !owner->second) return;
        const std::string key = group + ":" + std::to_string(static_cast<int>(kind));
        if (auto existing = analysisWorkflowDialogs_.find(key);
            existing != analysisWorkflowDialogs_.end() && existing->second) return;
        ::rlispstat::core::InitialAnalysisSpecification initial;
        if (auto sharedKind = SharedInitializationKind(kind))
            initial = ::rlispstat::core::ResolveInitialAnalysisSpecification(
                *dataframe, ::rlispstat::core::SharedAnalysisDefinition(*sharedKind),
                ::rlispstat::core::DefaultVariableRolesForAnalysisInitialization(
                    commandDispatcher_->applicationState().variableRoles(group)));
        auto dialog = AnalysisWorkflowDialog::Create(
            owner->second->NativeWindow(), *dataframe, kind,
            [this](AnalysisWorkflowSpecification const& specification)
            {
                if (specification.kind == AnalysisWorkflowKind::LinearModelTrellis)
                    (void)CreateModelTrellis(specification.group, specification.response, specification.variables,
                        specification.rowCondition, specification.columnCondition, "holm",
                        specification.scope);
                else
                    QueueAnalysisWorkflow(specification);
            },
            [this, key]() { analysisWorkflowDialogs_.erase(key); },
            std::move(initial), AnalysisScopeChoicesFor(
                commandDispatcher_->applicationState(), group, true));
        analysisWorkflowDialogs_[key] = dialog;
        dialog->Show();
    }

    void App::QueueAnalysisWorkflow(AnalysisWorkflowSpecification const& specification)
    {
        if (!commandDispatcher_) return;
        ::rlispstat::core::MainRAnalysisWorkflowTask task;
        switch (specification.kind)
        {
        case AnalysisWorkflowKind::Table1: task.kind = "table1"; break;
        case AnalysisWorkflowKind::CorrelationMatrix: task.kind = "correlation_matrix"; break;
        case AnalysisWorkflowKind::Dimensionality: task.kind = "dimensionality"; break;
        case AnalysisWorkflowKind::FactorAnalysis: task.kind = "dimensionality"; break;
        case AnalysisWorkflowKind::ScaleAnalysis: return;
        case AnalysisWorkflowKind::QuickCluster: task.kind = "quick_cluster"; break;
        case AnalysisWorkflowKind::OneSampleT: task.kind = "one_sample_t"; break;
        case AnalysisWorkflowKind::IndependentT: task.kind = "independent_t"; break;
        case AnalysisWorkflowKind::PairedT: task.kind = "paired_t"; break;
        case AnalysisWorkflowKind::OneWayAnova: task.kind = "oneway_anova"; break;
        case AnalysisWorkflowKind::LinearModel: task.kind = "linear_model"; break;
        case AnalysisWorkflowKind::LinearModelTrellis: task.kind = "linear_model_trellis"; break;
        case AnalysisWorkflowKind::RegressionComparison: task.kind = "regression_comparison"; break;
        case AnalysisWorkflowKind::BinaryRegression: task.kind = "binary_regression"; break;
        case AnalysisWorkflowKind::BinaryRegressionComparison: task.kind = "binary_regression_comparison"; break;
        case AnalysisWorkflowKind::GeneralizedLinearModel: task.kind = "generalized_linear_model"; break;
        case AnalysisWorkflowKind::CountRegression: task.kind = "count_regression"; break;
        case AnalysisWorkflowKind::CountRegressionComparison: task.kind = "generalized_comparison"; break;
        case AnalysisWorkflowKind::PositiveContinuousModel: task.kind = "generalized_linear_model"; break;
        case AnalysisWorkflowKind::PositiveContinuousComparison: task.kind = "generalized_comparison"; break;
        case AnalysisWorkflowKind::ProportionModel: task.kind = "generalized_linear_model"; break;
        case AnalysisWorkflowKind::ProportionComparison: task.kind = "generalized_comparison"; break;
        case AnalysisWorkflowKind::GeneralizedComparison: task.kind = "generalized_comparison"; break;
        case AnalysisWorkflowKind::LinearMixedModel: task.kind = "linear_mixed_model"; break;
        case AnalysisWorkflowKind::GeneralizedMixedModel: task.kind = "generalized_mixed_model"; break;
        }
        task.group = specification.group;
        task.response = specification.response;
        task.secondary = specification.secondary;
        task.groupVariable = specification.groupVariable;
        task.variables = specification.variables;
        task.family = specification.family;
        task.link = specification.link;
        task.method = specification.method;
        task.rowCondition = specification.rowCondition;
        task.columnCondition = specification.columnCondition;
        const auto scope = commandDispatcher_->applicationState().activeAnalysisScope(specification.group);
        if (scope.kind == ::rlispstat::core::AnalysisScopeKind::ExplicitRowIds)
            task.rows = ::rlispstat::core::ResolveAnalysisScopeRowIds(scope, scope.totalDatasetRows);
        if (scope.kind == ::rlispstat::core::AnalysisScopeKind::ExplicitRowIds && task.rows.empty()) {
            MessageBoxW(nullptr,L"The global analysis scope contains no observations.",L"Analysis Scope",MB_OK | MB_ICONINFORMATION);
            return;
        }
        pendingMainRTasks_.analysisWorkflowTasks.push_back(std::move(task));
        MarkMainRTaskPending();
    }

    void App::QueueLinearModelFit(std::string const& group)
    {
        if (!commandDispatcher_) return;
        auto& applicationState = commandDispatcher_->applicationState();
        auto found = applicationState.groupModels().find(group);
        if (found == applicationState.groupModels().end()) return;
        auto& state = found->second;
        state.frozenScopeNotice.clear();
        const std::string datasetGroup = state.group;
        commandDispatcher_->applicationState().captureAnalysisScope(state.group, state.dataScope, state.dataScopeCaptured, &state.scope);
        if (auto const* dataframe = applicationState.datasets().find(datasetGroup))
        {
            if (::rlispstat::core::SynchronizeGroupModelTermTypes(state, *dataframe))
                ::rlispstat::core::MarkGroupModelChanged(state);
        }
        std::string scope = ::rlispstat::core::ModelSpecificationFitScope(state);
        if (scope == "compare_selected_all") scope = "all";
        std::set<int> currentSelectedSet;
        applicationState.selectedRows(datasetGroup, currentSelectedSet);
        const std::vector<int> currentSelectedRows(
            currentSelectedSet.begin(), currentSelectedSet.end());
        const std::vector<int> fitSelectionRows =
            ::rlispstat::core::ModelSpecificationSelectionRows(
                state, currentSelectedRows);
        const auto effectiveTermTypes =
            ::rlispstat::core::EffectiveModelSpecificationTermTypes(state);
        std::string signature = ::rlispstat::core::LinearGLMFitSignature(
            state.response, state.terms, effectiveTermTypes, scope,
            state.centeredPredictors, state.factorReferenceLevels);
        signature = ::rlispstat::core::LinearGLMFitIdentityWithSelection(
            signature, scope, fitSelectionRows);

        // A completed error is a result to display, not a request to submit
        // the same failing fit again.  The old refresh callback retried it
        // recursively through MAIN_R_TASKS and could monopolise the R session.
        if (!state.isStale && !state.rFitPending)
        {
            ShowLinearModel(group);
            return;
        }
        if (state.rFitPending && state.lastRFitSignature == signature)
        {
            ShowLinearModel(group);
            return;
        }

        auto& tasks = pendingMainRTasks_.linearGLMTasks;
        tasks.erase(std::remove_if(tasks.begin(), tasks.end(),
            [&](::rlispstat::core::MainRLinearGLMTask const& task)
            { return (task.modelId.empty() ? task.group : task.modelId) == group; }), tasks.end());

        if (state.response.empty() || state.terms.empty())
        {
            state.rFitPending = false;
            state.isStale = false;
            state.lastRFitSignature = signature;
            ::rlispstat::core::GLMFitSummary empty;
            empty.warning = state.response.empty()
                ? "Choose a response variable before fitting."
                : "Add at least one independent variable before fitting.";
            applicationState.linearModelFits()[group] = std::move(empty);
            ShowLinearModel(group);
            return;
        }

        state.rFitPending = true;
        state.lastRFitSignature = signature;
        ::rlispstat::core::MainRLinearGLMTask task;
        task.group = datasetGroup; task.modelId = group;
        task.dependent = state.response; task.scope = scope;
        task.terms = state.terms; task.termTypes = effectiveTermTypes;
        task.centeredPredictors.assign(state.centeredPredictors.begin(),
                                       state.centeredPredictors.end());
        task.factorReferenceLevels = state.factorReferenceLevels;
        task.rows = fitSelectionRows;
        task.requestIdentity = signature;
        tasks.push_back(std::move(task));
        MarkMainRTaskPending();
        ShowLinearModel(group);
    }

    void App::RefitLinearModel(std::string const& group)
    {
        if (!commandDispatcher_) return;
        auto found = commandDispatcher_->applicationState().groupModels().find(group);
        if (found == commandDispatcher_->applicationState().groupModels().end()) return;
        ::rlispstat::core::MarkGroupModelChanged(found->second);
        QueueLinearModelFit(group);
    }

    void App::RefitMeanComparison(std::string const& id)
    {
        auto found = meanComparisonStates_.find(id);
        if (found == meanComparisonStates_.end()) return;
        auto& state = found->second;
        commandDispatcher_->applicationState().captureAnalysisScope(state.datasetId, state.dataScope, state.dataScopeCaptured);
        // Context-menu edits may arrive while a menu is closing. Do not queue
        // an incomplete analysis: a pairless task cannot produce a report.
        if (!MeanComparisonReadyForRefit(state))
        {
            state.tables.clear();
            state.subtitle = MeanComparisonSelectionPrompt(state);
            ShowMeanComparison(state);
            return;
        }
        if (MeanComparisonUsesGroupingVariable(state) && commandDispatcher_ &&
            state.specification.groupingVariableId)
        {
            if (auto const* dataframe =
                    commandDispatcher_->applicationState().datasets().find(state.datasetId))
            {
                auto const column = std::find_if(dataframe->columns.begin(), dataframe->columns.end(),
                    [&](auto const& candidate)
                    { return candidate.name == *state.specification.groupingVariableId; });
                if (column != dataframe->columns.end())
                {
                    auto const levels = MeanComparisonLevels(*column);
                    if (state.analysisType == "independent_samples_t_test")
                    {
                        if (!::rlispstat::core::MeanComparisonIndependentGroupOrderIsValid(
                                state.specification.groupOrderIds, levels))
                            state.specification.groupOrderIds =
                                ::rlispstat::core::MeanComparisonDefaultIndependentGroupOrder(levels);
                    }
                    else if (!MeanComparisonSameLevelSet(
                                 state.specification.groupOrderIds, levels))
                        state.specification.groupOrderIds = levels;
                }
            }
        }
        if (state.dataScopeCaptured &&
            state.dataScope.kind == ::rlispstat::core::AnalysisScopeKind::ExplicitRowIds &&
            state.dataScope.originalRowIds.empty())
        {
            state.tables.clear();
            state.subtitle = "No rows are selected. Select rows or choose All data.";
            ShowMeanComparison(state);
            return;
        }
        ::rlispstat::core::MainRCompareMeansTask task;
        task.id = state.id; task.group = state.datasetId;
        task.testType = state.analysisType == "one_sample_t_test" ? "one_sample_t" :
            state.analysisType == "independent_samples_t_test" ? "independent_t" :
            state.analysisType == "paired_samples_t_test" ? "paired_t" : "oneway_anova";
        task.responses = state.specification.dependentVariableIds;
        for (auto const& pair : state.specification.pairs)
            task.pairs.push_back({ pair.firstVariableId, pair.secondVariableId });
        task.groupVar = state.specification.groupingVariableId.value_or("");
        task.testValue = state.specification.testValue;
        task.alternative = state.specification.alternative == ::rlispstat::core::AlternativeHypothesis::Greater
            ? "greater" : state.specification.alternative == ::rlispstat::core::AlternativeHypothesis::Less
            ? "less" : "two.sided";
        task.confidenceLevel = state.specification.confidenceLevel;
        using Method = ::rlispstat::core::MeanComparisonMethod;
        switch (state.specification.method)
        {
        case Method::OneSampleT: case Method::StudentT: task.method = "student"; break;
        case Method::WilcoxonSignedRank: task.method = "wilcoxon"; break;
        case Method::MannWhitney: task.method = "mann_whitney"; break;
        case Method::ClassicalAnova: task.method = "classical"; break;
        case Method::KruskalWallis: task.method = "kruskal_wallis"; break;
        default: task.method = "welch"; break;
        }
        task.pAdjustment = state.specification.pAdjustment == ::rlispstat::core::MultipleTestingAdjustment::Bonferroni
            ? "bonferroni" : state.specification.pAdjustment == ::rlispstat::core::MultipleTestingAdjustment::None
            ? "none" : "holm";
        task.groupOrder = state.specification.groupOrderIds;
        task.scope = state.dataScopeCaptured ? ScopeName(state.dataScope) : "all";
        if (state.dataScopeCaptured && state.dataScope.kind == ::rlispstat::core::AnalysisScopeKind::ExplicitRowIds)
            task.rows = state.dataScope.originalRowIds;
        if (state.analysisType == "one_sample_t_test")
        {
            if (auto const* dataframe = commandDispatcher_->applicationState().datasets().find(state.datasetId))
                ConfigureOneSampleTask(task, state, *dataframe);
        }
        else if (state.analysisType == "independent_samples_t_test")
        {
            if (auto const* dataframe = commandDispatcher_->applicationState().datasets().find(state.datasetId))
                ConfigureIndependentSamplesTask(task, state, *dataframe);
        }
        else if (state.analysisType == "paired_samples_t_test")
        {
            if (auto const* dataframe = commandDispatcher_->applicationState().datasets().find(state.datasetId))
                ConfigurePairedSamplesTask(task, state, *dataframe);
        }
        if (state.analysisType == "one_way_anova")
        {
            for (auto const& table : state.tables)
            {
                if (table.tableId != "omnibus") continue;
                for (auto const& row : table.rows)
                {
                    const std::string outputId = "mean_pairwise:" + state.id + ":" + row.variable;
                    auto previous = outputStates_.find(outputId);
                    if (previous == outputStates_.end()) continue;
                    auto pending = previous->second;
                    pending.subtitle = "Updating for the current scope; previous comparison results remain visible.";
                    pending.statusText = pending.subtitle;
                    ShowTable1(pending);
                }
            }
        }
        // Preserve the completed report while R computes the replacement, but
        // make the accepted menu click visible immediately. Rapid changes to
        // the same analysis supersede its older queued request.
        if (auto view = meanComparisonViews_.find(state.id);
            view != meanComparisonViews_.end() && view->second)
            view->second->ShowPending(state);
        auto& tasks = pendingMainRTasks_.compareMeansTasks;
        tasks.erase(std::remove_if(tasks.begin(), tasks.end(),
            [&](auto const& pending) { return pending.id == state.id; }), tasks.end());
        tasks.push_back(std::move(task));
        MarkMainRTaskPending();
    }

    void App::HandleAnalysisViewCommand(std::vector<std::string> const& command)
    {
        if (!command.empty() && (command[0]=="MODEL_SCOPE" || command[0]=="GGLM_SET_SCOPE" ||
            command[0]=="REGCMP_SET_SCOPE" || command[0]=="GCOMP_SET_SCOPE" || command[0]=="GCOMP_SET_MODEL_SCOPE" ||
            command[0]=="MEAN_SET_SCOPE" || command[0]=="CORR_SET_SCOPE" || command[0]=="DIM_SET_SCOPE")) return;

        if (!commandDispatcher_ || command.empty()) return;
        const std::string& name = command[0];
        if (name == "DENDRO_SHOW_DISTANCE_MATRIX" && command.size() >= 2)
        {
            auto& states = commandDispatcher_->applicationState().dendrograms();
            auto found = states.find(command[1]);
            if (found == states.end() || found->second.rFitPending ||
                found->second.caseRows.size() < 2) return;
            ::rlispstat::core::MainRDendrogramTask task;
            task.id = found->second.id; task.group = found->second.group;
            task.revision = found->second.requestRevision;
            task.dataVersion = found->second.sourceDataVersion;
            task.distanceMatrix = true;
            pendingMainRTasks_.dendrogramTasks.push_back(std::move(task));
            MarkMainRTaskPending();
            return;
        }
        if (name == "SET_SELECTED_COLOR" || name == "RESET_SELECTED_COLOR" ||
            name == "SET_POINT_COLOR" || name == "CLEAR_ROW_COLORS")
        {
            const std::string reply = DispatchApplicationCommand(command);
            if (reply.rfind("ERR ", 0) == 0)
                LogStartup("Color command failed: " + reply);
            return;
        }
        if (name == "PLOT_TOGGLE_SCOPE_FREEZE" && command.size() >= 2)
        {
            auto found = plots_.find(command[1]);
            if (found == plots_.end() || !found->second ||
                found->second->isDatasetSeed ||
                ::rlispstat::core::PlotIsDerivedAnalysisView(*found->second)) return;
            auto& plot = *found->second;
            plot.analysisScopeFrozen = !plot.analysisScopeFrozen;
            const auto global = commandDispatcher_->applicationState()
                .activeAnalysisScope(plot.group);
            if (plot.analysisScopeFrozen)
            {
                const std::size_t total = plot.dataScope.totalDatasetRows;
                const auto rows = ::rlispstat::core::ResolveAnalysisScopeRowIds(
                    plot.dataScope, total);
                plot.dataScope = ::rlispstat::core::ExplicitAnalysisScope(
                    plot.group, rows,
                    ::rlispstat::core::AnalysisScopeSourceKind::OtherExplicitSubset,
                    plot.dataScope.sourceDescription,
                    total,
                    plot.dataScope.sourceViewId,
                    plot.dataScope.sourceElementId);
                plot.dataScopeCaptured = true;
                plot.frozenScopeNotice = plot.dataScopeCaptured
                    ? ::rlispstat::core::FrozenAnalysisScopeNotice(
                        plot.dataScope, global)
                    : std::string{};
                auto view = views_.find(plot.id);
                if (view != views_.end() && view->second) view->second->Show(plot);
            }
            else RefreshOpenPlotsForScope(plot.group, false);
            QueueCommandStateRefresh();
            return;
        }
        if (name == "REGRESSION_INTERACTION_SET_FACTOR_ORDER" && command.size() >= 4)
        {
            auto found = regressionDerivedOutputs_.find(command[1] + "|table");
            if (found == regressionDerivedOutputs_.end()) return;
            auto refresh = found->second.refreshCommand;
            refresh.erase(std::remove_if(refresh.begin(), refresh.end(),
                [](std::string const& field) {
                    return field.rfind("report_focal=", 0) == 0 ||
                        field.rfind("report_group=", 0) == 0;
                }), refresh.end());
            refresh.push_back("report_focal=" + command[2]);
            refresh.push_back("report_group=" + command[3]);
            HandleAnalysisViewCommand(refresh);
            return;
        }
        if (name == "REGRESSION_INTERACTION_SET_FOCAL" && command.size() >= 3)
        {
            auto found = regressionDerivedOutputs_.find(command[1] + "|table");
            if (found == regressionDerivedOutputs_.end()) return;
            auto refresh = found->second.refreshCommand;
            refresh.erase(std::remove_if(refresh.begin(), refresh.end(),
                [](std::string const& field) { return field.rfind("report_focal=", 0) == 0; }), refresh.end());
            refresh.push_back("report_focal=" + command[2]);
            HandleAnalysisViewCommand(refresh);
            return;
        }
        if (name == "REGRESSION_INTERACTION_SET_REFERENCE" && command.size() >= 3)
        {
            auto found = regressionDerivedOutputs_.find(command[1] + "|table");
            if (found == regressionDerivedOutputs_.end()) return;
            auto refresh = found->second.refreshCommand;
            refresh.erase(std::remove_if(refresh.begin(), refresh.end(),
                [](std::string const& field) {
                    return field.rfind("emm_reference=", 0) == 0;
                }), refresh.end());
            refresh.push_back("emm_reference=" + command[2]);
            HandleAnalysisViewCommand(refresh);
            return;
        }
        if (name == "REGRESSION_INTERACTION_SET_BINARY_OPTION" && command.size() >= 4)
        {
            auto found = regressionDerivedOutputs_.find(command[1] + "|table");
            if (found == regressionDerivedOutputs_.end()) return;
            std::string prefix;
            if (command[2] == "quantity") prefix = "effect_quantity=";
            else if (command[2] == "adjustment") prefix = "effect_adjustment=";
            else if (command[2] == "presentation") prefix = "effect_presentation=";
            else if (command[2] == "confidence") prefix = "effect_confidence=";
            else return;
            auto refresh = found->second.refreshCommand;
            refresh.erase(std::remove_if(refresh.begin(), refresh.end(),
                [&](std::string const& field) {
                    return field.rfind(prefix, 0) == 0;
                }), refresh.end());
            refresh.push_back(prefix + command[3]);
            HandleAnalysisViewCommand(refresh);
            return;
        }
        if (name == "SAVE_ANALYSIS_SCOPE_FROM_SELECTION")
        {
            const std::string reply = DispatchApplicationCommand(command);
            if (reply.rfind("ERR ", 0) == 0)
                LogStartup("Save analysis scope command failed: " + reply);
            return;
        }
        if (name == "SET_ANALYSIS_SCOPE_FROM_SELECTION" ||
            name == "SET_ANALYSIS_SCOPE_FROM_ROWS" ||
            name == "SET_ANALYSIS_SCOPE_ALL" || name == "SET_ANALYSIS_SCOPE_UNSELECTED" ||
            name == "USE_SAVED_ANALYSIS_SCOPE")
        {
            const auto reply = commandDispatcher_->dispatch(command);
            if (reply.rfind("ERR ", 0) == 0)
                LogStartup("Analysis scope command failed: " + reply);
            return;
        }
        if (name == "OPEN_COLOR_PALETTE" && command.size() >= 2)
        {
            auto plot = plots_.find(command[1]);
            if (plot != plots_.end() && plot->second) ShowColorPalette(plot->second->group);
            return;
        }
        if (name == "OPEN_DENDRO_COLOR_PALETTE" && command.size() >= 2)
        {
            ShowColorPalette(command[1]);
            return;
        }
        if ((name == "PCA_BIPLOT_SET_X" || name == "PCA_BIPLOT_SET_Y") &&
            command.size() >= 3)
        {
            auto plot = plots_.find(command[1]);
            if (plot == plots_.end() || !plot->second ||
                plot->second->kind != "pca_biplot") return;
            auto& model = *plot->second;
            char* end = nullptr;
            const long parsed = std::strtol(command[2].c_str(), &end, 10);
            if (end == command[2].c_str() || *end != '\0') return;
            auto state = commandDispatcher_->applicationState().dimensionalityModels().find(
                model.dimensionalityModelId);
            if (state == commandDispatcher_->applicationState().dimensionalityModels().end()) return;
            const bool exists = std::any_of(state->second.components.begin(),
                state->second.components.end(), [parsed](auto const& component)
                { return component.index == parsed; });
            if (!exists) return;
            if (name == "PCA_BIPLOT_SET_X")
            {
                if (parsed == model.biplotYComponent) return;
                model.biplotXComponent = static_cast<int>(parsed);
            }
            else
            {
                if (parsed == model.biplotXComponent) return;
                model.biplotYComponent = static_cast<int>(parsed);
            }
            RefreshDimensionalityPlots(model.dimensionalityModelId);
            return;
        }
        if ((name == "DENDRO_SET_LABEL" || name == "DENDRO_SET_COLOR_BY" ||
             name == "DENDRO_ROTATE_270" || name == "DENDRO_ROTATE_LABELS_90" || name == "DENDRO_TOGGLE_COLOR_LEGEND" ||
             name == "DENDRO_SET_LEGEND_POSITION" || name == "DENDRO_SET_LEGEND_COORDS") &&
            command.size() >= 2)
        {
            auto found = commandDispatcher_->applicationState().dendrograms().find(command[1]);
            if (found == commandDispatcher_->applicationState().dendrograms().end()) return;
            auto& state = found->second;
            auto const* dataframe = commandDispatcher_->applicationState().datasets().find(state.group);
            if (!dataframe) return;
            std::string error;
            std::vector<int> colorByChangedRows;
            if (name == "DENDRO_SET_LABEL")
            {
                if (command.size() < 3 || !::rlispstat::core::SetDendrogramLabelVariable(
                        state, *dataframe, command[2], &error)) return;
                state.labelVariableConfigured = true;
            }
            else if (name == "DENDRO_SET_COLOR_BY")
            {
                if (command.size() < 3) return;
                auto& application = commandDispatcher_->applicationState();
                std::map<int, std::string> before;
                for (auto const& [row, color] : application.displayPointColors(state.group))
                    before[row] = color;
                if (!application.setColorOverride(state.group, command[2], &error)) return;
                std::map<int, std::string> after;
                for (auto const& [row, color] : application.displayPointColors(state.group))
                    after[row] = color;
                for (auto const& [row, color] : before)
                    if (after.find(row) == after.end() || after[row] != color)
                        colorByChangedRows.push_back(row);
                for (auto const& [row, color] : after)
                    if (before.find(row) == before.end()) colorByChangedRows.push_back(row);
            }
            else if (name == "DENDRO_ROTATE_270") {state.rotate270 = !state.rotate270;state.rotateCaseLabels90=false;state.verticalFlip=false;}
            else if (name == "DENDRO_ROTATE_LABELS_90") state.rotateCaseLabels90 = !state.rotateCaseLabels90;
            else if (name == "DENDRO_TOGGLE_COLOR_LEGEND") state.colorByLegendVisible = !state.colorByLegendVisible;
            else if (name == "DENDRO_SET_LEGEND_POSITION" && command.size() >= 3)
            {
                if(command[2]=="top_left"){state.colorByLegendX=.08;state.colorByLegendY=.10;}
                else if(command[2]=="top_right"){state.colorByLegendX=.72;state.colorByLegendY=.10;}
                else if(command[2]=="bottom_left"){state.colorByLegendX=.08;state.colorByLegendY=.68;}
                else if(command[2]=="bottom_right"){state.colorByLegendX=.72;state.colorByLegendY=.68;}
            }
            else if (name == "DENDRO_SET_LEGEND_COORDS" && command.size() >= 4)
            {state.colorByLegendX=std::clamp(std::stod(command[2]),0.0,1.0);state.colorByLegendY=std::clamp(std::stod(command[3]),0.0,1.0);}
            ShowDendrogram(state.id);
            if (!colorByChangedRows.empty())
            {
                const auto colors =
                    commandDispatcher_->applicationState().displayPointColors(state.group);
                for (auto const& [plotId, model] : plots_)
                {
                    if (!model || model->group != state.group) continue;
                    auto view = views_.find(plotId);
                    if (view != views_.end() && view->second)
                        view->second->SetVisualStyle(
                            commandDispatcher_->plotTheme(), colors);
                    if (model->kind == "scatter_matrix" &&
                        ::rlispstat::core::HasOverlaySource(*model, "color"))
                    {
                        ::rlispstat::core::InvalidateScatterMatrixFits(*model);
                        QueueSmoothRecompute(*model,
                            ::rlispstat::core::SmoothCurveScope::ColorGroup, "lm");
                        continue;
                    }
                    if (::rlispstat::core::MarkSmoothCurveScopePendingIfPresent(
                            model->smoothCurves,
                            ::rlispstat::core::SmoothCurveScope::ColorGroup))
                        QueueSmoothRecompute(*model,
                            ::rlispstat::core::SmoothCurveScope::ColorGroup);
                }
                auto sheet = dataSheets_.find(state.group);
                if (sheet != dataSheets_.end() && sheet->second)
                    sheet->second->RefreshRowColors(colors);
                for (auto const& [id, view] : dendrogramViews_)
                {
                    auto dendrogram = commandDispatcher_->applicationState().dendrograms().find(id);
                    if (view && dendrogram != commandDispatcher_->applicationState().dendrograms().end() &&
                        dendrogram->second.group == state.group)
                        view->RefreshRowColors(colors);
                }
                QueueCommandStateRefresh();
            }
            return;
        }
        if (name == "SET_SMOOTH_SPAN" && command.size() >= 3)
        {
            auto found = plots_.find(command[1]);
            if (found == plots_.end() || !found->second) return;
            char* end = nullptr;
            const double value = std::strtod(command[2].c_str(), &end);
            if (end == command[2].c_str() || *end != '\0') return;
            found->second->smoothSpan = ::rlispstat::core::ClampSmoothSpan(value);
            QueueExistingSmoothRecompute(*found->second);
            auto view = views_.find(command[1]);
            if (view != views_.end() && view->second) view->second->Show(*found->second);
            return;
        }
        if (name.rfind("PLOT_ANALYZE_", 0) == 0 && command.size() >= 2)
        {
            auto plot = plots_.find(command[1]);
            if (plot == plots_.end() || !plot->second) return;
            auto& model = *plot->second;
            auto& applicationState = commandDispatcher_->applicationState();
            auto const* dataframe = applicationState.datasets().find(model.group);
            if (!dataframe) return;
            const auto context =
                ::rlispstat::core::BuildPlotAnalysisContext(model, *dataframe);
            const auto scope = applicationState.activeAnalysisScope(model.group);

            if (name == "PLOT_ANALYZE_MODEL" && context.offersModel)
            {
                ::rlispstat::core::PlotModel seed;
                ::rlispstat::core::PopulateDatasetSeedPlot(seed, *dataframe, model.group);
                seed.yLabel = context.dependentVariable;
                if (context.modelKind == ::rlispstat::core::PlotAnalysisModelKind::Linear)
                {
                    const std::string modelId = ::rlispstat::core::CreateLinearModelInstance(
                        applicationState.groupModels(), model.group);
                    auto& state = applicationState.groupModels().at(modelId);
                    state.response = context.dependentVariable;
                    state.terms = context.predictors;
                    state.termTypeOverrides =
                        ::rlispstat::core::PlotAnalysisTermTypes(context);
                    state.termTypes = state.termTypeOverrides;
                    state.dataScope = scope;
                    state.dataScopeCaptured = true;
                    state.scope = ScopeName(scope);
                    ::rlispstat::core::MarkGroupModelChanged(state);
                    QueueLinearModelFit(modelId);
                    return;
                }

                auto responseColumn = std::find_if(dataframe->columns.begin(), dataframe->columns.end(),
                    [&](auto const& column) { return column.name == context.dependentVariable; });
                if (responseColumn == dataframe->columns.end()) return;
                auto coding = ::rlispstat::core::InspectBinaryResponse(*responseColumn);
                if (!coding.ok) return;
                ::rlispstat::core::GeneralizedGLMState state;
                state.id = StartupId("binary_plot_", model.group);
                state.group = model.group;
                state.seed = std::move(seed);
                state.hasSeed = true;
                state.response = context.dependentVariable;
                state.responseCoding = coding;
                state.responseCodingExplicit = true;
                state.terms = context.predictors;
                state.termTypes = ::rlispstat::core::PlotAnalysisTermTypes(context);
                state.title = "Binary Model";
                state.modelType = ::rlispstat::core::StatisticalModelType::Binary;
                state.binaryRegression = true;
                state.family = "binomial";
                state.binaryLink = ::rlispstat::core::BinaryLink::Logit;
                state.link = ::rlispstat::core::BinaryLinkId(state.binaryLink);
                state.dataScope = scope;
                state.dataScopeCaptured = true;
                state.scope = ScopeName(scope);
                state.modelVersion = 1;
                const auto id = state.id;
                applicationState.generalizedGLMs()[id] = std::move(state);
                ShowGeneralizedModel(id);
                RequestGeneralizedModelFit(id);
                return;
            }
            if (name == "PLOT_ANALYZE_CORRELATIONS" && context.offersCorrelations)
            {
                ::rlispstat::core::PlotModel seed;
                ::rlispstat::core::PopulateDatasetSeedPlot(seed, *dataframe, model.group);
                ::rlispstat::core::CorrelationMatrixState state;
                state.id = StartupId("corr_plot_", model.group);
                state.group = model.group;
                state.title = "Correlations from plot";
                state.method = "pearson";
                state.missingMode = "pairwise";
                state.showP = true;
                state.variables = context.numericVariables;
                state.seed = std::move(seed);
                state.hasSeed = true;
                state.dataScope = scope;
                state.dataScopeCaptured = true;
                QueueCorrelationFit(state);
                ++state.modelVersion;
                const auto id = state.id;
                applicationState.correlationMatrices()[id] = std::move(state);
                ShowCorrelationMatrix(id);
                return;
            }
            if (name == "PLOT_ANALYZE_FACTOR" && context.numericVariables.size() >= 2)
            {
                ::rlispstat::core::PlotModel seed;
                ::rlispstat::core::PopulateDatasetSeedPlot(seed, *dataframe, model.group);
                ::rlispstat::core::DimensionalityState state;
                state.id = StartupId("factor_plot_", model.group);
                state.group = model.group;
                state.method = "factor";
                state.missingMode = "listwise";
                state.scale = true;
                state.variables = context.numericVariables;
                state.componentCount = std::min<int>(2,
                    static_cast<int>(state.variables.size()));
                state.seed = std::move(seed);
                state.hasSeed = true;
                state.dataScope = scope;
                state.dataScopeCaptured = true;
                state.scope = ScopeName(scope);
                const auto id = state.id;
                applicationState.dimensionalityModels()[id] = std::move(state);
                (void)RefitDimensionality(id);
                return;
            }
            if (name == "PLOT_ANALYZE_CONTINGENCY" && context.offersContingencyTables)
            {
                ::rlispstat::core::AnalysisSpecification existing{
                    { "row", {} }, { "column", {} }
                };
                if (!context.dependentVariable.empty() &&
                    std::find(context.categoricalVariables.begin(),
                              context.categoricalVariables.end(),
                              context.dependentVariable) != context.categoricalVariables.end())
                    existing["row"] = { context.dependentVariable };
                if (context.categoricalVariables.size() == 2)
                {
                    if (existing["row"].empty())
                        existing["row"] = { context.categoricalVariables.front() };
                    const auto column = std::find_if(context.categoricalVariables.begin(),
                        context.categoricalVariables.end(), [&](auto const& variable)
                        { return variable != existing["row"].front(); });
                    if (column != context.categoricalVariables.end())
                        existing["column"] = { *column };
                }
                const auto initial = ::rlispstat::core::ResolveInitialAnalysisSpecification(
                    *dataframe,
                    ::rlispstat::core::SharedAnalysisDefinition(
                        ::rlispstat::core::SharedAnalysisKind::ContingencyTable),
                    {}, &existing);
                if (initial.minimumValid)
                    CreateContingencyTable({ model.group,
                        ::rlispstat::core::InitialAnalysisVariable(initial, "row"),
                        ::rlispstat::core::InitialAnalysisVariable(initial, "column") });
                else
                    OpenContingencyTableDialog(model.group, &existing);
                return;
            }
            if (name == "PLOT_ANALYZE_DESCRIPTIVES" && context.offersDescriptives)
            {
                std::vector<std::string> variables = context.numericVariables.empty()
                    ? context.categoricalVariables : context.numericVariables;
                std::vector<std::string> eligibleGrouping;
                for (auto const& candidate : context.groupingVariables)
                {
                    if (candidate != context.dependentVariable &&
                        std::find(variables.begin(), variables.end(), candidate) == variables.end())
                        eligibleGrouping.push_back(candidate);
                }
                const std::string grouping = eligibleGrouping.size() == 1
                    ? eligibleGrouping.front() : std::string{};
                const auto id = StartupId("table1_plot_", model.group);
                auto state = ::rlispstat::core::Table1PendingStateForDataFrame(
                    *dataframe, id, variables, grouping, {}, &scope);
                state.title = "Descriptive statistics from plot";
                ShowTable1(state);
                return;
            }
            return;
        }
        if ((name == "CONTEXT_CORRELATION_XY" ||
             name == "CONTEXT_DESCRIPTIVES_XY" ||
             name == "CONTEXT_LINEAR_MODEL_XY") && command.size() >= 2)
        {
            auto plot = plots_.find(command[1]);
            if (plot == plots_.end() || !plot->second) return;
            auto& model = *plot->second;
            auto& applicationState = commandDispatcher_->applicationState();
            auto const* dataframe = applicationState.datasets().find(model.group);
            if (!dataframe || model.xLabel.empty() || model.yLabel.empty() ||
                model.xLabel == model.yLabel) return;
            const auto scope = applicationState.activeAnalysisScope(model.group);

            if (name == "CONTEXT_DESCRIPTIVES_XY")
            {
                const auto id = StartupId("table1_plot_", model.group);
                auto state = ::rlispstat::core::Table1PendingStateForDataFrame(
                    *dataframe, id, { model.xLabel, model.yLabel }, "", {}, &scope);
                state.title = "Descriptive statistics: " + model.xLabel + " and " + model.yLabel;
                ShowTable1(state);
                return;
            }
            if (name == "CONTEXT_CORRELATION_XY")
            {
                ::rlispstat::core::PlotModel seed;
                ::rlispstat::core::PopulateDatasetSeedPlot(seed, *dataframe, model.group);
                ::rlispstat::core::CorrelationMatrixState state;
                state.id = StartupId("corr_plot_", model.group);
                state.group = model.group;
                state.title = "Correlation: " + model.yLabel + " with " + model.xLabel;
                state.method = "pearson";
                state.missingMode = "pairwise";
                state.showP = true;
                state.showPValue = false;
                state.showN = false;
                state.variables = { model.xLabel, model.yLabel };
                state.seed = std::move(seed);
                state.hasSeed = true;
                state.dataScope = scope;
                state.dataScopeCaptured = true;
                QueueCorrelationFit(state);
                ++state.modelVersion;
                const auto id = state.id;
                applicationState.correlationMatrices()[id] = std::move(state);
                ShowCorrelationMatrix(id);
                return;
            }

            ::rlispstat::core::PlotModel seed;
            ::rlispstat::core::PopulateDatasetSeedPlot(seed, *dataframe, model.group);
            const std::string modelId = ::rlispstat::core::CreateLinearModelInstance(
                applicationState.groupModels(), model.group);
            auto& state = applicationState.groupModels().at(modelId);
            state.response = model.yLabel;
            state.terms = { model.xLabel };
            state.dataScope = scope;
            state.dataScopeCaptured = true;
            state.scope = ScopeName(scope);
            ::rlispstat::core::SynchronizeGroupModelTermTypes(state, *dataframe);
            ::rlispstat::core::MarkGroupModelChanged(state);
            QueueLinearModelFit(modelId);
            return;
        }
        if ((name == "SET_LABEL_COLUMN" || name == "SET_LABEL_DISPLAY") &&
            command.size() >= 3)
        {
            auto plot = plots_.find(command[1]);
            if (plot == plots_.end() || !plot->second) return;
            const std::string group = plot->second->group;
            if (name == "SET_LABEL_DISPLAY")
            {
                const std::string mode = command[2];
                if (mode != "none" && mode != "selected" && mode != "all") return;
                plot->second->labelDisplayMode = mode;
            }
            else
            {
                const std::string column = command[2] == "." ? std::string{} : command[2];
                if (!commandDispatcher_->applicationState().setLabelColumn(group, column)) return;
            }
            const std::string column = commandDispatcher_->applicationState().labelColumn(group);
            std::map<int, std::string> labels;
            if (auto const* dataframe = commandDispatcher_->applicationState().datasets().find(group))
            {
                auto foundColumn = std::find_if(dataframe->columns.begin(), dataframe->columns.end(),
                    [&](auto const& candidate) { return candidate.name == column; });
                if (foundColumn != dataframe->columns.end())
                    labels = ::rlispstat::core::RowLabelMapForColumn(*foundColumn);
            }
            for (auto const& [id, model] : plots_)
            {
                if (!model || model->group != group) continue;
                auto view = views_.find(id);
                if (view != views_.end() && view->second)
                    view->second->SetLabelState(column, labels);
            }
            return;
        }
        if (name == "SET_IMPUTATION_UNCERTAINTY" && command.size() >= 3)
        {
            auto found = plots_.find(command[1]);
            if (found == plots_.end() || !found->second ||
                found->second->kind != "scatter" ||
                !::rlispstat::core::ScatterImputationUncertaintyModeIsValid(command[2]))
                return;
            found->second->scatterImputationUncertainty = command[2];
            auto const* dataframe = commandDispatcher_->applicationState()
                .datasets().find(found->second->group);
            if (found->second->isGLMDiagnostic &&
                !found->second->diagnosticImputationValues.empty())
            {
                std::vector<::rlispstat::core::ScatterplotPointValue> points;
                for (auto const& point : found->second->points)
                    points.push_back({ point.row, point.x, point.y });
                if (auto viewport =
                    ::rlispstat::core::ScatterplotViewportIncludingPointImputations(
                        points, found->second->diagnosticImputationValues, command[2]))
                {
                    found->second->xmin = viewport->xmin;
                    found->second->xmax = viewport->xmax;
                    found->second->ymin = viewport->ymin;
                    found->second->ymax = viewport->ymax;
                }
            }
            else if (dataframe)
                ComputeScatterRangesIncludingAllImputations(*found->second, *dataframe);
            auto view = views_.find(command[1]);
            if (view != views_.end() && view->second)
            {
                view->second->SetDataFrame(dataframe);
                view->second->Show(*found->second);
                view->second->SetVisualStyle(commandDispatcher_->plotTheme(),
                    commandDispatcher_->applicationState().displayPointColors(
                        found->second->group));
            }
            return;
        }
        if (name == "SET_DIAGNOSTIC_IMPUTATION_DISPLAY" && command.size() >= 3)
        {
            auto found = plots_.find(command[1]);
            if (found == plots_.end() || !found->second ||
                !found->second->isGLMDiagnostic ||
                found->second->diagnosticImputationCount <= 1) return;
            auto& model = *found->second;
            if (command[2] == "all")
            {
                if (model.kind != "scatter" || model.glmDiagnosticKind == "roc_curve" ||
                    model.glmDiagnosticKind == "normal_qq") return;
                model.diagnosticShowImputationUncertainty = true;
            }
            else if (command[2].rfind("version:", 0) == 0)
            {
                const int version = std::atoi(command[2].substr(8).c_str());
                if (version < 1 || version > model.diagnosticImputationCount) return;
                model.diagnosticShowImputationUncertainty = false;
                model.diagnosticImputationIndex = version;
            }
            else return;
            // Presentation changed even though the source fit revision did
            // not; force the diagnostic projection to be rebuilt.
            model.displayedDiagnosticsVersion = -1;
            RefreshDiagnosticPlots();
            QueueExistingSmoothRecompute(model);
            return;
        }
        if (name == "SET_DIAGNOSTIC_IMPUTATION_SCOPE" && command.size() >= 3)
        {
            auto found = plots_.find(command[1]);
            if (found == plots_.end() || !found->second ||
                !found->second->isGLMDiagnostic ||
                !found->second->diagnosticShowImputationUncertainty ||
                (command[2] != "direct" && command[2] != "all")) return;
            found->second->diagnosticImputationUncertaintyScope = command[2];
            // Presentation changed while the fitted model remained current.
            found->second->displayedDiagnosticsVersion = -1;
            RefreshDiagnosticPlots();
            QueueExistingSmoothRecompute(*found->second);
            return;
        }
        if (name == "SET_PARTIAL_PLOT_TERM" && command.size() >= 3)
        {
            auto found = plots_.find(command[1]);
            if (found == plots_.end() || !found->second) return;
            auto& model = *found->second;
            if (model.regressionDerivedKind != "partial_regression_plot" ||
                std::find(model.regressionPartialAvailableTerms.begin(),
                    model.regressionPartialAvailableTerms.end(), command[2]) ==
                    model.regressionPartialAvailableTerms.end()) return;
            if (!model.regressionPartialPendingAnalysisId.empty()) {
                regressionDerivedOutputs_.erase(model.regressionPartialPendingAnalysisId + "|plot");
                model.regressionPartialPendingAnalysisId.clear();
            }
            if (model.regressionDerivedTerm == command[2]) return;
            auto dependency = regressionDerivedOutputs_.find(model.glmModelId + "|plot");
            if (dependency == regressionDerivedOutputs_.end()) return;
            auto refresh = dependency->second.refreshCommand;
            if (refresh.size() < 3) return;
            const auto separator = refresh[2].find('\x1f');
            refresh[2] = separator == std::string::npos ? command[2] :
                refresh[2].substr(0, separator + 1) + command[2];
            const auto suffix = ":" + model.regressionDerivedTerm;
            if (model.glmModelId.size() < suffix.size() ||
                model.glmModelId.compare(model.glmModelId.size() - suffix.size(), suffix.size(), suffix) != 0) return;
            model.regressionPartialPendingAnalysisId = model.glmModelId.substr(
                0, model.glmModelId.size() - model.regressionDerivedTerm.size()) + command[2];
            HandleAnalysisViewCommand(refresh);
            return;
        }
        if (name == "SET_DIAGNOSTIC_RESIDUAL_TYPE" && command.size() >= 3)
        {
            auto found = plots_.find(command[1]);
            if (found == plots_.end() || !found->second ||
                !found->second->isGLMDiagnostic) return;
            auto& model = *found->second;
            if (model.regressionDerivedKind == "partial_regression_plot")
            {
                const auto choices = ::rlispstat::core::DiagnosticPlotResidualChoices(model);
                const bool validPartial = std::find(choices.begin(), choices.end(), command[2]) != choices.end();
                if (!validPartial || model.displayedResidualType == command[2]) return;
                model.displayedResidualType = command[2];
                model.regressionPartialResidualType = command[2];
                auto dependency = regressionDerivedOutputs_.find(model.glmModelId + "|plot");
                if (dependency == regressionDerivedOutputs_.end()) return;
                const auto refreshCommand = dependency->second.refreshCommand;
                HandleAnalysisViewCommand(refreshCommand);
                return;
            }
            const bool generalized = model.generalizedDiagnosticResiduals;
            const bool valid = generalized
                ? std::find(model.diagnosticAvailableResidualTypes.begin(),
                            model.diagnosticAvailableResidualTypes.end(),
                            command[2]) != model.diagnosticAvailableResidualTypes.end()
                : (command[2] == "raw" || command[2] == "standardized" ||
                   command[2] == "studentized");
            if (!valid || model.displayedResidualType == command[2]) return;
            model.displayedResidualType = command[2];
            // Residual representation is plot presentation state.  Rebuild
            // the diagnostic projection without refitting the model.
            model.displayedDiagnosticsVersion = -1;
            RefreshDiagnosticPlots();
            QueueExistingSmoothRecompute(model);
            return;
        }
        if (name == "SET_IMPUTATION_DISPLAY" && command.size() >= 3)
        {
            auto found = plots_.find(command[1]);
            if (found == plots_.end() || !found->second) return;
            SetImputationDisplayMode(found->second->group, command[2]);
            return;
        }
        if (name == "PLOT_COLOR_BY" && command.size() >= 3)
        {
            auto found = plots_.find(command[1]);
            if (found == plots_.end() || !found->second) return;
            auto& model = *found->second;
            auto& application = commandDispatcher_->applicationState();
            std::map<int, std::string> before;
            for (auto const& [row, color] : application.displayPointColors(model.group))
                before[row] = color;
            std::string error;
            if (!application.setColorOverride(model.group, command[2], &error)) return;
            const auto activeOverride = application.colorOverride(model.group);
            for (auto const& [plotId, linkedModel] : plots_)
                if (linkedModel && linkedModel->group == model.group && linkedModel->kind == "barplot")
                    linkedModel->barplotRowColorDisplay = activeOverride
                        ? (linkedModel->barplotSplitVariable.empty()
                            ? "bar_fill" : "composition_strip") : "hide";
            std::map<int, std::string> after;
            for (auto const& [row, color] : application.displayPointColors(model.group))
                after[row] = color;
            std::vector<int> changedRows;
            for (auto const& [row, color] : before)
                if (after.find(row) == after.end() || after[row] != color)
                    changedRows.push_back(row);
            for (auto const& [row, color] : after)
                if (before.find(row) == before.end()) changedRows.push_back(row);

            const auto colors =
                application.displayPointColors(model.group);
            for (auto const& [plotId, linkedModel] : plots_)
            {
                if (!linkedModel || linkedModel->group != model.group) continue;
                auto view = views_.find(plotId);
                if (view != views_.end() && view->second) {
                    view->second->Show(*linkedModel);
                    view->second->SetVisualStyle(commandDispatcher_->plotTheme(), colors);
                }
                if (!changedRows.empty() && linkedModel->kind == "scatter_matrix" &&
                    ::rlispstat::core::HasOverlaySource(*linkedModel, "color"))
                {
                    ::rlispstat::core::InvalidateScatterMatrixFits(*linkedModel);
                    QueueSmoothRecompute(*linkedModel,
                        ::rlispstat::core::SmoothCurveScope::ColorGroup, "lm");
                    continue;
                }
                if (!changedRows.empty() &&
                    ::rlispstat::core::MarkSmoothCurveScopePendingIfPresent(
                        linkedModel->smoothCurves,
                        ::rlispstat::core::SmoothCurveScope::ColorGroup))
                    QueueSmoothRecompute(*linkedModel,
                        ::rlispstat::core::SmoothCurveScope::ColorGroup);
            }
            auto sheet = dataSheets_.find(model.group);
            if (sheet != dataSheets_.end() && sheet->second)
                sheet->second->RefreshRowColors(colors);
            for (auto const& [id, view] : dendrogramViews_)
            {
                auto state = commandDispatcher_->applicationState().dendrograms().find(id);
                if (view && state != commandDispatcher_->applicationState().dendrograms().end() &&
                    state->second.group == model.group) view->RefreshRowColors(colors);
            }
            QueueCommandStateRefresh();
            return;
        }
        if (name == "PLOT_COLOR_LEGEND_VISIBLE" && command.size() >= 3)
        {
            auto found = plots_.find(command[1]);
            if (found == plots_.end() || !found->second) return;
            found->second->colorByLegendVisible = command[2] != "0";
            auto view = views_.find(command[1]);
            if (view != views_.end() && view->second) view->second->Show(*found->second);
            return;
        }
        if (name == "PLOT_COLOR_LEGEND_POSITION" && command.size() >= 3)
        {
            auto found = plots_.find(command[1]);
            if (found == plots_.end() || !found->second ||
                !::rlispstat::core::SetPlotColorLegendPosition(*found->second, command[2])) return;
            auto view = views_.find(command[1]);
            if (view != views_.end() && view->second) view->second->Show(*found->second);
            return;
        }
        if (name == "PLOT_INTERACTION_SET_LEGEND_POSITION" && command.size() >= 3)
        {
            auto found = plots_.find(command[1]);
            if (found == plots_.end() || !found->second ||
                found->second->kind != "glm_interaction" ||
                !::rlispstat::core::SetInteractionLegendPosition(
                    *found->second, command[2])) return;
            auto view = views_.find(command[1]);
            if (view != views_.end() && view->second)
            {
                view->second->Show(*found->second);
                view->second->SetVisualStyle(commandDispatcher_->plotTheme(),
                    commandDispatcher_->applicationState().displayPointColors(
                        found->second->group));
            }
            return;
        }
        if (name == "PLOT_REGRESSION_SET_EFFECT_X" && command.size() >= 3)
        {
            auto found = plots_.find(command[1]);
            if (found == plots_.end() || !found->second ||
                found->second->kind != "glm_interaction" ||
                found->second->xLabel == command[2]) return;
            auto variables = ::rlispstat::core::SplitInteractionTerm(
                found->second->glmInteractionTerm);
            if (std::find(variables.begin(), variables.end(), command[2]) ==
                variables.end()) return;
            auto dependency = regressionDerivedOutputs_.find(
                found->second->glmModelId + "|plot");
            if (dependency == regressionDerivedOutputs_.end())
            {
                LogStartup("Effect plot cannot change axes because its source model is no longer registered.");
                return;
            }
            auto refresh = dependency->second.refreshCommand;
            refresh.erase(std::remove_if(refresh.begin(), refresh.end(),
                [](std::string const& value)
                {
                    return value.rfind("plot_focal=", 0) == 0;
                }), refresh.end());
            refresh.push_back("plot_focal=" + command[2]);
            HandleAnalysisViewCommand(refresh);
            return;
        }
        if ((name == "PLOT_REGRESSION_SET_EFFECT_QUANTITY" ||
             name == "PLOT_REGRESSION_SET_EFFECT_ADJUSTMENT" ||
             name == "PLOT_REGRESSION_SET_EFFECT_PRESENTATION" ||
             name == "PLOT_REGRESSION_SET_EFFECT_CONFIDENCE") &&
            command.size() >= 3)
        {
            auto found = plots_.find(command[1]);
            if (found == plots_.end() || !found->second ||
                found->second->kind != "glm_interaction" ||
                (!found->second->regressionBinaryProbability &&
                 !found->second->regressionBoundedCount)) return;
            auto dependency = regressionDerivedOutputs_.find(
                found->second->glmModelId + "|plot");
            if (dependency == regressionDerivedOutputs_.end())
            {
                LogStartup("Binary effect plot cannot be recalculated because its source model is no longer registered.");
                return;
            }
            std::string prefix;
            if (name == "PLOT_REGRESSION_SET_EFFECT_QUANTITY")
                prefix = "effect_quantity=";
            else if (name == "PLOT_REGRESSION_SET_EFFECT_ADJUSTMENT")
                prefix = "effect_adjustment=";
            else if (name == "PLOT_REGRESSION_SET_EFFECT_PRESENTATION")
                prefix = "effect_presentation=";
            else
                prefix = "effect_confidence=";
            auto refresh = dependency->second.refreshCommand;
            refresh.erase(std::remove_if(refresh.begin(), refresh.end(),
                [&](std::string const& value)
                {
                    return value.rfind(prefix, 0) == 0;
                }), refresh.end());
            refresh.push_back(prefix + command[2]);
            HandleAnalysisViewCommand(refresh);
            return;
        }
        if (name == "PLOT_SMOOTH_TOGGLE_CONFIDENCE_INTERVALS" && command.size() >= 2)
        {
            auto found = plots_.find(command[1]);
            if (found == plots_.end() || !found->second ||
                (found->second->kind != "scatter" &&
                 found->second->kind != "trellis_scatterplot")) return;
            found->second->scatterSmoothConfidenceIntervalsVisible =
                !found->second->scatterSmoothConfidenceIntervalsVisible;
            ::rlispstat::core::RefreshBasicPlotCodeReference(
                commandDispatcher_->applicationState(), *found->second);
            commandDispatcher_->applicationState().registerOutputCodeReference(
                found->second->codeReference);
            auto view = views_.find(command[1]);
            if (view != views_.end() && view->second)
                view->second->Show(*found->second);
            return;
        }
        if ((name == "PLOT_REGRESSION_TOGGLE_CONNECTING_LINE" ||
             name == "PLOT_REGRESSION_TOGGLE_CONFIDENCE_INTERVALS" ||
             name == "PLOT_REGRESSION_TOGGLE_CONFIDENCE_LEVEL") && command.size() >= 2)
        {
            auto found = plots_.find(command[1]);
            if (found == plots_.end() || !found->second) return;
            if (name == "PLOT_REGRESSION_TOGGLE_CONFIDENCE_INTERVALS" &&
                (found->second->kind == "scatter" ||
                 found->second->kind == "trellis_scatterplot"))
            {
                found->second->scatterFitConfidenceIntervalsVisible =
                    !found->second->scatterFitConfidenceIntervalsVisible;
                ::rlispstat::core::RefreshBasicPlotCodeReference(
                    commandDispatcher_->applicationState(), *found->second);
                commandDispatcher_->applicationState().registerOutputCodeReference(
                    found->second->codeReference);
                auto view = views_.find(command[1]);
                if (view != views_.end() && view->second)
                    view->second->Show(*found->second);
                return;
            }
            if (found->second->kind != "glm_interaction") return;
            if (name == "PLOT_REGRESSION_TOGGLE_CONNECTING_LINE")
                found->second->regressionConnectEstimates =
                    !found->second->regressionConnectEstimates;
            else if (name == "PLOT_REGRESSION_TOGGLE_CONFIDENCE_INTERVALS")
            {
                found->second->regressionConfidenceIntervalsVisible =
                    !found->second->regressionConfidenceIntervalsVisible;
                ::rlispstat::core::ComputeRanges(*found->second);
                ::rlispstat::core::ApplyRegressionInteractionAxisRange(*found->second);
            }
            else
                found->second->regressionConfidenceLevelVisible =
                    !found->second->regressionConfidenceLevelVisible;
            ::rlispstat::core::SynchronizeRegressionPlotCodeReferenceDisplay(
                *found->second);
            commandDispatcher_->applicationState().registerOutputCodeReference(
                found->second->codeReference);
            auto view = views_.find(command[1]);
            if (view != views_.end() && view->second) view->second->Show(*found->second);
            return;
        }
        if (name.rfind("PLOT_CONDITION_", 0) == 0 && command.size() >= 3)
        {
            auto found = plots_.find(command[1]);
            if (found == plots_.end() || !found->second) return;
            auto& model = *found->second;
            auto const* dataframe = commandDispatcher_->applicationState().datasets().find(model.group);
            if (!dataframe) return;
            const bool categorical = name == "PLOT_CONDITION_CATEGORICAL" ||
                name == "PLOT_CONDITION_ORDERED";
            const auto kind = categorical
                ? ::rlispstat::core::TrellisConditioningVariableKind::Categorical
                : ::rlispstat::core::TrellisConditioningVariableKind::ContinuousBinned;
            const auto method = name == "PLOT_CONDITION_EQUAL_COUNT"
                ? ::rlispstat::core::TrellisContinuousBinningMethod::EqualCount
                : ::rlispstat::core::TrellisContinuousBinningMethod::EqualWidth;
            std::string error;
            if (!::rlispstat::core::ConvertPlotToTrellisWithCondition(
                    model, *dataframe, command[2], kind, method,
                    name == "PLOT_CONDITION_ORDERED", &error)) return;
            ::rlispstat::core::RefreshBasicPlotCodeReference(
                commandDispatcher_->applicationState(), model);
            auto view = views_.find(model.id);
            if (view != views_.end() && view->second)
            {
                view->second->Show(model);
                view->second->SetVisualStyle(commandDispatcher_->plotTheme(),
                    commandDispatcher_->applicationState().displayPointColors(model.group));
            }
            return;
        }
        if ((name.rfind("TRELLIS_SCATTERPLOT_", 0) == 0 ||
             name.rfind("TIME_SERIES_", 0) == 0) && command.size() >= 2)
        {
            auto found = plots_.find(command[1]);
            if (found == plots_.end() || !found->second) return;
            auto& model = *found->second;
            auto const* dataframe = commandDispatcher_->applicationState().datasets().find(model.group);
            const bool presentationOnly = name == "TIME_SERIES_SET_IDENTIFICATION" || name == "TIME_SERIES_SET_LEGEND_POSITION";
            if (!presentationOnly && (!dataframe || ::rlispstat::core::PlotIsImputationDiagnostic(model))) return;
            const std::string value = command.size() >= 3 ? command[2] : std::string{};
            bool ok = true; std::string error;
            if (model.kind == "time_series")
            {
                if (name == "TIME_SERIES_SET_GROUP") model.timeSeriesGroupVariable = value == "." ? "" : value;
                else if (name == "TIME_SERIES_SET_IDENTIFICATION")
                {
                    if (value != "legend" && value != "start_labels" && value != "none") return;
                    model.timeSeriesIdentification = value;
                }
                else if (name == "TIME_SERIES_SET_LEGEND_POSITION") {
                    if (!::rlispstat::core::SetTimeSeriesLegendPosition(model,value)) return;
                }
                else if (name == "TIME_SERIES_SET_X" || name == "TIME_SERIES_SET_Y")
                {
                    if (name == "TIME_SERIES_SET_X")
                    {
                        model.xLabel = value;
                        if (auto const* column = ::rlispstat::core::FindDataColumnInDataFrame(*dataframe, value))
                            model.timeSeriesTimeType = ::rlispstat::core::InferTimeSeriesTimeType(*column);
                    }
                    else model.yLabel = value;
                }
                else return;
                if (name != "TIME_SERIES_SET_IDENTIFICATION" && name != "TIME_SERIES_SET_LEGEND_POSITION")
                    ok = ::rlispstat::core::RebuildTimeSeriesFromDataFrame(model, *dataframe, &error);
            }
            else if (model.kind == "trellis_scatterplot")
            {
                ::rlispstat::core::InitializeTrellisSpecificationFromLegacy(model);
                auto candidate = model;
                auto& specification = candidate.trellisSpecification;
                const bool coordinatesChanged =
                    name == "TRELLIS_SCATTERPLOT_SET_X" ||
                    name == "TRELLIS_SCATTERPLOT_SET_Y" ||
                    name == "TRELLIS_SCATTERPLOT_SET_TYPE_WITH_X" ||
                    name.rfind("TRELLIS_SCATTERPLOT_BOXPLOT_", 0) == 0 ||
                    name == "TRELLIS_SCATTERPLOT_SET_BOXPLOT_GROUPS";
                const bool panelDataChanged =
                    name == "TRELLIS_SCATTERPLOT_SET_CONDITION" ||
                    name.rfind("TRELLIS_SCATTERPLOT_ADD_CONDITION_", 0) == 0 ||
                    name.rfind("TRELLIS_SCATTERPLOT_SET_CONDITION_", 0) == 0 ||
                    name == "TRELLIS_SCATTERPLOT_CONDITION_BINS" ||
                    name == "TRELLIS_SCATTERPLOT_REMOVE_CONDITION";
                if (name == "TRELLIS_SCATTERPLOT_SET_X")
                {
                    specification.xVariableId = value;
                    if (specification.plotType == ::rlispstat::core::TrellisPlotType::Boxplot)
                    {
                        if (specification.boxplotGroupingVariableIds.empty())
                            specification.boxplotGroupingVariableIds.push_back(value);
                        else specification.boxplotGroupingVariableIds.front() = value;
                    }
                }
                else if (name == "TRELLIS_SCATTERPLOT_SET_Y")
                    specification.yVariableId = value;
                else if (name == "TRELLIS_SCATTERPLOT_SET_TYPE")
                {
                    const auto type = ::rlispstat::core::ParseTrellisPlotType(value); if (!type) return;
                    specification.plotType = *type;
                    if (*type == ::rlispstat::core::TrellisPlotType::Boxplot &&
                        specification.boxplotGroupingVariableIds.empty())
                        specification.boxplotGroupingVariableIds = {specification.xVariableId};
                }
                else if (name == "TRELLIS_SCATTERPLOT_SET_TYPE_WITH_X")
                {
                    if (command.size() < 4) return;
                    const auto type = ::rlispstat::core::ParseTrellisPlotType(value); if (!type) return;
                    specification.plotType = *type;
                    specification.xVariableId = command[3];
                    if (*type == ::rlispstat::core::TrellisPlotType::Boxplot)
                        specification.boxplotGroupingVariableIds = {command[3]};
                }
                else if (name == "TRELLIS_SCATTERPLOT_SET_BOXPLOT_GROUPS")
                {
                    if (command.size() < 4) return;
                    const long count = std::strtol(command[2].c_str(), nullptr, 10);
                    if (count < 1 || command.size() != 3 + static_cast<std::size_t>(count)) return;
                    specification.boxplotGroupingVariableIds.assign(command.begin() + 3, command.end());
                    specification.xVariableId = specification.boxplotGroupingVariableIds.front();
                }
                else if (name == "TRELLIS_SCATTERPLOT_BOXPLOT_ADD_GROUP" ||
                         name == "TRELLIS_SCATTERPLOT_BOXPLOT_REPLACE_GROUP" ||
                         name == "TRELLIS_SCATTERPLOT_BOXPLOT_REMOVE_GROUP" ||
                         name == "TRELLIS_SCATTERPLOT_BOXPLOT_MOVE_GROUP_EARLIER" ||
                         name == "TRELLIS_SCATTERPLOT_BOXPLOT_MOVE_GROUP_LATER")
                {
                    auto& groups = specification.boxplotGroupingVariableIds;
                    if (groups.empty()) groups.push_back(specification.xVariableId);
                    auto group = std::find(groups.begin(), groups.end(), value);
                    if (name == "TRELLIS_SCATTERPLOT_BOXPLOT_ADD_GROUP")
                    {
                        auto const* column = ::rlispstat::core::FindDataColumnInDataFrame(*dataframe, value);
                        if (!column || value == specification.yVariableId || group != groups.end() ||
                            !::rlispstat::core::DataColumnLooksGroupingCandidate(*column, dataframe->rows)) return;
                        groups.push_back(value);
                    }
                    else
                    {
                        if (group == groups.end()) return;
                        const std::size_t index = static_cast<std::size_t>(group - groups.begin());
                        if (name == "TRELLIS_SCATTERPLOT_BOXPLOT_REPLACE_GROUP")
                        {
                            if (command.size() < 4) return;
                            const std::string replacement = command[3];
                            auto const* column = ::rlispstat::core::FindDataColumnInDataFrame(
                                *dataframe, replacement);
                            if (!column || replacement == specification.yVariableId ||
                                std::find(groups.begin(), groups.end(), replacement) != groups.end() ||
                                !::rlispstat::core::DataColumnLooksGroupingCandidate(
                                    *column, dataframe->rows)) return;
                            groups[index] = replacement;
                        }
                        else if (name == "TRELLIS_SCATTERPLOT_BOXPLOT_REMOVE_GROUP")
                        {
                            if (groups.size() <= 1) return;
                            groups.erase(group);
                        }
                        else
                        {
                            const bool earlier = name == "TRELLIS_SCATTERPLOT_BOXPLOT_MOVE_GROUP_EARLIER";
                            if ((earlier && index == 0) || (!earlier && index + 1 >= groups.size())) return;
                            std::swap(groups[index], groups[earlier ? index - 1 : index + 1]);
                        }
                    }
                    specification.xVariableId = groups.front();
                }
                else if (name == "TRELLIS_SCATTERPLOT_SET_LAYOUT")
                { specification.layoutMode = value; candidate.trellisLayoutMode = value; }
                else if (name == "TRELLIS_SCATTERPLOT_SET_ORDER")
                { specification.panelOrder = value; candidate.trellisPanelOrder = value; }
                else if (name == "TRELLIS_SCATTERPLOT_SET_SCALE") specification.scaleMode = value;
                else if (name == "TRELLIS_SCATTERPLOT_TOGGLE_Y_AXIS_SIDE")
                    candidate.trellisRowStripsOnLeft = !candidate.trellisRowStripsOnLeft;
                else if (name == "TRELLIS_SCATTERPLOT_SET_BAR_MEASURE")
                {
                    if (value != "count" && value != "percent" &&
                        value != "conditional_percent") return;
                    specification.barMeasure = value;
                }
                else if (name == "TRELLIS_SCATTERPLOT_SET_HISTOGRAM_MEASURE")
                {
                    if (value != "count" && value != "percent" && value != "density") return;
                    specification.histogramMeasure = value;
                }
                else if (name == "TRELLIS_SCATTERPLOT_SET_HISTOGRAM_BINS")
                {
                    const auto count = ::rlispstat::core::HistogramBinCountForChoice(candidate, value);
                    if (!count) return;
                    specification.histogramBinCount = static_cast<std::size_t>(*count);
                }
                else if (name == "TRELLIS_SCATTERPLOT_SET_SPLIT")
                {
                    if (specification.plotType == ::rlispstat::core::TrellisPlotType::TimeSeries)
                        specification.groupingVariableId = value == "." ? "" : value;
                    else specification.splitVariableId = value == "." ? "" : value;
                }
                else if (name == "TRELLIS_SCATTERPLOT_TOGGLE_CONNECT")
                    specification.connectObservations = !specification.connectObservations;
                else if (name == "TRELLIS_SCATTERPLOT_ADD_CONDITION_CATEGORICAL" ||
                         name == "TRELLIS_SCATTERPLOT_ADD_CONDITION_ORDERED")
                    ok = ::rlispstat::core::AddTrellisConditioningVariable(candidate, *dataframe, value,
                        ::rlispstat::core::TrellisConditioningVariableKind::Categorical,
                        ::rlispstat::core::TrellisContinuousBinningMethod::EqualWidth,
                        &error, name == "TRELLIS_SCATTERPLOT_ADD_CONDITION_ORDERED");
                else if (name == "TRELLIS_SCATTERPLOT_ADD_CONDITION_EQUAL_WIDTH" ||
                         name == "TRELLIS_SCATTERPLOT_ADD_CONDITION_EQUAL_COUNT")
                    ok = ::rlispstat::core::AddTrellisConditioningVariable(candidate, *dataframe, value,
                        ::rlispstat::core::TrellisConditioningVariableKind::ContinuousBinned,
                        name == "TRELLIS_SCATTERPLOT_ADD_CONDITION_EQUAL_COUNT"
                            ? ::rlispstat::core::TrellisContinuousBinningMethod::EqualCount
                            : ::rlispstat::core::TrellisContinuousBinningMethod::EqualWidth, &error);
                else if (name == "TRELLIS_SCATTERPLOT_SET_CONDITION")
                {
                    if (command.size() < 4) return;
                    ok = ::rlispstat::core::ReplaceTrellisConditioningVariable(
                        candidate, *dataframe, value, command[3], &error);
                }
                else if (name == "TRELLIS_SCATTERPLOT_SET_CONDITION_FACTOR" ||
                         name == "TRELLIS_SCATTERPLOT_SET_CONDITION_ORDERED" ||
                         name == "TRELLIS_SCATTERPLOT_SET_CONDITION_EQUAL_WIDTH" ||
                         name == "TRELLIS_SCATTERPLOT_SET_CONDITION_EQUAL_COUNT")
                {
                    const bool categorical = name == "TRELLIS_SCATTERPLOT_SET_CONDITION_FACTOR" ||
                        name == "TRELLIS_SCATTERPLOT_SET_CONDITION_ORDERED";
                    ok = ::rlispstat::core::SetTrellisConditioningInterpretation(
                        candidate, *dataframe, value,
                        categorical
                            ? ::rlispstat::core::TrellisConditioningVariableKind::Categorical
                            : ::rlispstat::core::TrellisConditioningVariableKind::ContinuousBinned,
                        name == "TRELLIS_SCATTERPLOT_SET_CONDITION_EQUAL_COUNT"
                            ? ::rlispstat::core::TrellisContinuousBinningMethod::EqualCount
                            : ::rlispstat::core::TrellisContinuousBinningMethod::EqualWidth,
                        name == "TRELLIS_SCATTERPLOT_SET_CONDITION_ORDERED", &error);
                }
                else if (name == "TRELLIS_SCATTERPLOT_CONDITION_BINS")
                {
                    if (command.size() < 4) return;
                    const long count = std::strtol(command[3].c_str(), nullptr, 10);
                    if (count < 2 || count > 6) return;
                    const auto foundCondition = std::find_if(
                        specification.conditioningVariables.begin(),
                        specification.conditioningVariables.end(),
                        [&](auto const& condition) { return condition.variableId == value; });
                    const auto method = foundCondition != specification.conditioningVariables.end() &&
                        foundCondition->binning
                        ? foundCondition->binning->method
                        : ::rlispstat::core::TrellisContinuousBinningMethod::EqualWidth;
                    ok = ::rlispstat::core::ConfigureTrellisContinuousCondition(
                        candidate, *dataframe, value, method,
                        static_cast<std::size_t>(count), &error);
                }
                else if (name == "TRELLIS_SCATTERPLOT_MOVE_CONDITION_EARLIER" ||
                         name == "TRELLIS_SCATTERPLOT_MOVE_CONDITION_LATER")
                    ok = ::rlispstat::core::MoveTrellisConditioningVariable(
                        candidate, *dataframe, value,
                        name == "TRELLIS_SCATTERPLOT_MOVE_CONDITION_EARLIER" ? -1 : 1,
                        &error);
                else if (name == "TRELLIS_SCATTERPLOT_DIMENSION_ROWS" ||
                         name == "TRELLIS_SCATTERPLOT_DIMENSION_COLUMNS" ||
                         name == "TRELLIS_SCATTERPLOT_DIMENSION_NESTED")
                {
                    const auto dimension =
                        name == "TRELLIS_SCATTERPLOT_DIMENSION_ROWS"
                            ? ::rlispstat::core::TrellisDimension::Rows
                            : name == "TRELLIS_SCATTERPLOT_DIMENSION_COLUMNS"
                                ? ::rlispstat::core::TrellisDimension::Columns
                                : ::rlispstat::core::TrellisDimension::Nested;
                    ok = ::rlispstat::core::SetTrellisConditioningDimension(
                        candidate, *dataframe, value, dimension, &error);
                }
                else if (name == "TRELLIS_SCATTERPLOT_SWAP_DIMENSIONS")
                    ok = ::rlispstat::core::SwapTrellisRowsAndColumns(
                        candidate, *dataframe, &error);
                else if (name == "TRELLIS_SCATTERPLOT_REMOVE_CONDITION")
                    ok = ::rlispstat::core::RemoveTrellisConditioningVariable(candidate, *dataframe, value, &error);
                else if (name == "TIME_SERIES_SET_IDENTIFICATION")
                    candidate.timeSeriesIdentification = value;
                else if (name == "TIME_SERIES_SET_LEGEND_POSITION")
                    candidate.timeSeriesLegendPosition = value;
                else return;
                if (ok && name != "TRELLIS_SCATTERPLOT_SET_LAYOUT" &&
                    name != "TRELLIS_SCATTERPLOT_SET_ORDER" &&
                    name != "TRELLIS_SCATTERPLOT_SET_SCALE" &&
                    name != "TRELLIS_SCATTERPLOT_TOGGLE_Y_AXIS_SIDE" &&
                    name != "TRELLIS_SCATTERPLOT_TOGGLE_CONNECT" &&
                    name != "TIME_SERIES_SET_IDENTIFICATION" &&
                    name != "TIME_SERIES_SET_LEGEND_POSITION" &&
                    name != "TRELLIS_SCATTERPLOT_ADD_CONDITION_CATEGORICAL" &&
                    name != "TRELLIS_SCATTERPLOT_ADD_CONDITION_ORDERED" &&
                    name != "TRELLIS_SCATTERPLOT_ADD_CONDITION_EQUAL_WIDTH" &&
                    name != "TRELLIS_SCATTERPLOT_ADD_CONDITION_EQUAL_COUNT" &&
                    name != "TRELLIS_SCATTERPLOT_SET_CONDITION" &&
                    name != "TRELLIS_SCATTERPLOT_SET_CONDITION_FACTOR" &&
                    name != "TRELLIS_SCATTERPLOT_SET_CONDITION_ORDERED" &&
                    name != "TRELLIS_SCATTERPLOT_SET_CONDITION_EQUAL_WIDTH" &&
                    name != "TRELLIS_SCATTERPLOT_SET_CONDITION_EQUAL_COUNT" &&
                    name != "TRELLIS_SCATTERPLOT_CONDITION_BINS" &&
                    name != "TRELLIS_SCATTERPLOT_MOVE_CONDITION_EARLIER" &&
                    name != "TRELLIS_SCATTERPLOT_MOVE_CONDITION_LATER" &&
                    name != "TRELLIS_SCATTERPLOT_DIMENSION_ROWS" &&
                    name != "TRELLIS_SCATTERPLOT_DIMENSION_COLUMNS" &&
                    name != "TRELLIS_SCATTERPLOT_DIMENSION_NESTED" &&
                    name != "TRELLIS_SCATTERPLOT_SWAP_DIMENSIONS" &&
                    name != "TRELLIS_SCATTERPLOT_REMOVE_CONDITION")
                    ok = ::rlispstat::core::RebuildTrellisPlotFromDataFrame(candidate, *dataframe, &error);
                if (ok)
                {
                    if (name == "TRELLIS_SCATTERPLOT_SET_SPLIT" &&
                        (specification.plotType == ::rlispstat::core::TrellisPlotType::Scatter ||
                         specification.plotType == ::rlispstat::core::TrellisPlotType::Histogram))
                        ok = ::rlispstat::core::SetPlotColorByVariable(
                            candidate, *dataframe, value, &error);
                }
                if (ok)
                {
                    model = std::move(candidate);
                    if (name == "TRELLIS_SCATTERPLOT_SET_SPLIT")
                    {
                        auto& application = commandDispatcher_->applicationState();
                        application.setColorOverride(model.group, value, nullptr);
                        std::vector<int> changedRows;
                        for (auto const& [row, color] : application.displayPointColors(model.group))
                            changedRows.push_back(row);
                        const auto colors = application.displayPointColors(model.group);
                        for (auto const& [plotId, linkedModel] : plots_)
                        {
                            if (!linkedModel || linkedModel->group != model.group)
                                continue;
                            auto linkedView = views_.find(plotId);
                            if (linkedView != views_.end() && linkedView->second)
                                linkedView->second->SetVisualStyle(
                                    commandDispatcher_->plotTheme(), colors);
                            if (!changedRows.empty() &&
                                linkedModel->kind == "scatter_matrix" &&
                                ::rlispstat::core::HasOverlaySource(*linkedModel, "color"))
                            {
                                ::rlispstat::core::InvalidateScatterMatrixFits(*linkedModel);
                                QueueSmoothRecompute(*linkedModel,
                                    ::rlispstat::core::SmoothCurveScope::ColorGroup, "lm");
                                continue;
                            }
                            if (!changedRows.empty() &&
                                ::rlispstat::core::MarkSmoothCurveScopePendingIfPresent(
                                    linkedModel->smoothCurves,
                                    ::rlispstat::core::SmoothCurveScope::ColorGroup))
                                QueueSmoothRecompute(*linkedModel,
                                    ::rlispstat::core::SmoothCurveScope::ColorGroup);
                        }
                        auto sheet = dataSheets_.find(model.group);
                        if (sheet != dataSheets_.end() && sheet->second)
                            sheet->second->RefreshRowColors(colors);
                        for (auto const& [id, dendrogramView] : dendrogramViews_)
                        {
                            auto state = commandDispatcher_->applicationState()
                                             .dendrograms().find(id);
                            if (dendrogramView &&
                                state != commandDispatcher_->applicationState()
                                             .dendrograms().end() &&
                                state->second.group == model.group)
                                dendrogramView->RefreshRowColors(colors);
                        }
                    }
                    if ((coordinatesChanged || panelDataChanged) &&
                        ::rlispstat::core::InvalidateTrellisSmoothCurvesForDataChange(model))
                        QueueExistingSmoothRecompute(model);
                }
            }
            else return;
            if (!ok) return;
            ::rlispstat::core::RefreshBasicPlotCodeReference(
                commandDispatcher_->applicationState(), model);
            auto view = views_.find(model.id);
            if (view != views_.end() && view->second)
            {
                view->second->Show(model);
                view->second->SetVisualStyle(commandDispatcher_->plotTheme(),
                    commandDispatcher_->applicationState().displayPointColors(model.group));
            }
            return;
        }
        if (name.rfind("MIXED_", 0) == 0 && command.size() >= 2)
        {
            auto found = commandDispatcher_->applicationState().nativeMixedModels().find(command[1]);
            if (found == commandDispatcher_->applicationState().nativeMixedModels().end()) return;
            auto& state = found->second;
            const std::string value = command.size() >= 3 ? command[2] : std::string{};
            const auto split = [](std::string const& encoded)
            {
                const auto separator = encoded.find('\x1f');
                return std::pair<std::string, std::string>{
                    separator == std::string::npos ? encoded : encoded.substr(0, separator),
                    separator == std::string::npos ? std::string{} : encoded.substr(separator + 1) };
            };
            if (name == "MIXED_SET_RESPONSE")
            {
                if (value.empty()) return; state.response = value;
                state.fixedEffects.erase(std::remove(state.fixedEffects.begin(), state.fixedEffects.end(), value), state.fixedEffects.end());
                for (auto& random : state.randomEffects)
                    random.terms.erase(std::remove(random.terms.begin(), random.terms.end(), value), random.terms.end());
            }
            else if (name == "MIXED_ADD_FIXED")
            {
                auto const* data = commandDispatcher_->applicationState().datasets().find(state.group);
                if (!data) return;
                std::vector<std::string> variables;
                for (auto const& col : data->columns) variables.push_back(col.name);
                ::rlispstat::core::AddHierarchicalTermsToVector(variables, state.response, state.fixedEffects, value);
            }
            else if (name == "MIXED_REMOVE_FIXED")
                state.fixedEffects.erase(std::remove(state.fixedEffects.begin(), state.fixedEffects.end(), value), state.fixedEffects.end());
            else if (name == "MIXED_ADD_RANDOM")
            {
                if (value.empty() || value == state.response ||
                    std::any_of(state.randomEffects.begin(), state.randomEffects.end(),
                        [&](auto const& spec) { return spec.group == value; })) return;
                ::rlispstat::core::NativeMixedRandomSpec spec; spec.group = value; spec.terms = { "1" };
                state.randomEffects.push_back(std::move(spec));
            }
            else if (name == "MIXED_REMOVE_RANDOM")
            {
                if (state.randomEffects.size() <= 1) return;
                state.randomEffects.erase(std::remove_if(state.randomEffects.begin(), state.randomEffects.end(),
                    [&](auto const& spec) { return spec.group == value; }), state.randomEffects.end());
            }
            else if (name == "MIXED_ADD_SLOPE" || name == "MIXED_REMOVE_SLOPE")
            {
                auto [group, term] = split(value); if (group.empty() || term.empty() || term == "1") return;
                auto random = std::find_if(state.randomEffects.begin(), state.randomEffects.end(),
                    [&](auto const& spec) { return spec.group == group; });
                if (random == state.randomEffects.end()) return;
                if (name == "MIXED_ADD_SLOPE")
                {
                    if (std::find(random->terms.begin(), random->terms.end(), term) == random->terms.end())
                        random->terms.push_back(term);
                }
                else random->terms.erase(std::remove(random->terms.begin(), random->terms.end(), term), random->terms.end());
            }
            else if (name == "MIXED_SET_METHOD")
            {
                if (state.modelType != "linear_mixed_model" || (value != "REML" && value != "ML")) return;
                state.method = value;
            }
            else if (name == "MIXED_SET_FAMILY")
            {
                if (state.modelType != "generalized_linear_mixed_model" ||
                    (value != "binomial" && value != "poisson")) return;
                state.family = value;
                if (!::rlispstat::core::IsValidGeneralizedLink(state.family, state.link))
                    state.link = ::rlispstat::core::DefaultGeneralizedLink(state.family);
            }
            else if (name == "MIXED_SET_LINK")
            {
                if (!::rlispstat::core::IsValidGeneralizedLink(state.family, value)) return;
                state.link = value;
            }
            else if (name != "MIXED_REFIT") return;
            if (state.response.empty() || state.randomEffects.empty())
            {
                state.reportText = state.modelType == "generalized_linear_mixed_model"
                    ? "Generalized Linear Mixed Model\n" : "Linear Mixed Model\n";
                state.reportText += "Dataset: " + state.group + "\n";
                state.reportText += "Response: " + state.response + "\n";
                state.reportText += "Scope: " + ScopeName(state.dataScope) + "\nWarnings\n";
                state.reportText += state.response.empty()
                    ? "Choose a response variable from the context menu.\n"
                    : "Choose a grouping variable for the random effect from the context menu.\n";
                ShowMixedModel(state);
                return;
            }
            commandDispatcher_->applicationState().captureAnalysisScope(state.group, state.dataScope, state.dataScopeCaptured);
            pendingMainRTasks_.mixedModelTasks.push_back({ state });
            MarkMainRTaskPending();
            return;
        }
        if (name.rfind("REGCMP_", 0) == 0 && command.size() >= 2)
        {
            auto& applicationState=commandDispatcher_->applicationState();
            auto found=applicationState.regressionComparisons().find(command[1]);
            if(found==applicationState.regressionComparisons().end())return;
            auto& state=found->second;const std::string value=command.size()>=3?command[2]:std::string{};
            auto const* comparisonDataframe=applicationState.datasets().find(state.group);
            const auto comparisonVariables=::rlispstat::core::AvailableVariableNames(state.seed,comparisonDataframe);
            auto split=[](std::string const& encoded){const auto separator=encoded.find('\x1f');return std::pair<std::string,std::string>{separator==std::string::npos?encoded:encoded.substr(0,separator),separator==std::string::npos?std::string{}:encoded.substr(separator+1)};};
            auto modelAndTerm=[&](int& index,std::string& term)
            {
                auto [modelText,decodedTerm]=split(value);index=std::atoi(modelText.c_str());term=decodedTerm;
                return index>=0&&(size_t)index<state.models.size()&&!term.empty();
            };
            auto addTermToModel=[&](int index,std::string const& term)
            {
                return ::rlispstat::core::IncludeRegressionComparisonTerm(
                    state,index,term,comparisonVariables);
            };
            if(name=="REGCMP_OPEN_SINGLE")
            {
                const int index=std::atoi(value.c_str());if(index<0||(size_t)index>=state.models.size())return;
                auto const& comparisonModel=state.models[(size_t)index];
                ::rlispstat::core::GroupModelState single;
                static_cast<::rlispstat::core::ModelSpecification&>(single)=
                    ::rlispstat::core::EffectiveRegressionComparisonModelSpecification(state,comparisonModel);
                single.group=state.group;
                single.precomputed=state.precomputed;single.multipleImputation=state.multipleImputation;
                single.imputationCount=state.imputationCount;single.title=comparisonModel.label;
                single.note=state.note;single.modelVersion=comparisonModel.modelVersion;
                single.fitVersion=comparisonModel.fitVersion;single.isStale=comparisonModel.isStale;
                single.rFitPending=state.rFitPending;single.rowsUsed=comparisonModel.fit.rowsUsed;
                single.rowsExcluded=comparisonModel.fit.rowsExcluded;
                const std::string modelId = ::rlispstat::core::CreateLinearModelInstance(
                    applicationState.groupModels(), state.group);
                single.modelId = modelId;
                applicationState.groupModels()[modelId]=std::move(single);
                applicationState.linearModelFits()[modelId]=comparisonModel.fit;
                ShowLinearModel(modelId);return;
            }
            if(name=="REGCMP_SHOW_DETAILS")
            {
                int index=-1;std::string term;if(!modelAndTerm(index,term))return;
                auto const& model=state.models[(size_t)index];
                ::rlispstat::core::Table1DisplayState table;table.id="regcmp_details:"+state.id+":"+std::to_string(index)+":"+term;
                table.datasetId=state.group;table.title="Coefficient details — "+term;table.subtitle=model.label;
                table.tableType="glm_coefficient_details";table.nativeGenerated=true;table.stubHeaders={"Coefficient"};
                table.columns={"b","beta","SE","t","p","Partial r","Delta R2"};
                int rowIndex=0;
                for(auto const& coefficient:model.fit.coefficients)
                {
                    const std::string source=coefficient.sourceTerm.empty()?coefficient.term:coefficient.sourceTerm;
                    if(source!=term&&coefficient.term!=term)continue;
                    ::rlispstat::core::Table1DisplayRow row;row.rowIndex=rowIndex++;row.rowType="coefficient";row.variable=term;
                    row.label=coefficient.displayLabel.empty()?coefficient.term:coefficient.displayLabel;row.stubValues={row.label};
                    row.values={::rlispstat::core::FormatDoubleOrDash(coefficient.estimate,4),
                        ::rlispstat::core::FormatDoubleOrDash(coefficient.standardizedBeta,4),
                        ::rlispstat::core::FormatDoubleOrDash(coefficient.stdError,4),
                        ::rlispstat::core::FormatDoubleOrDash(coefficient.tValue,3),
                        ::rlispstat::core::FormatPValue(coefficient.pValue),
                        ::rlispstat::core::FormatDoubleOrDash(coefficient.partialR,3),
                        ::rlispstat::core::FormatDoubleOrDash(coefficient.deltaR2,3)};
                    table.rows.push_back(std::move(row));
                }
                if(table.rows.empty())table.warnings.push_back("No fitted coefficient is available for this term in "+model.label+".");
                table.statusText=table.rows.empty()?table.warnings.front():std::to_string(table.rows.size())+" coefficient rows";
                ShowTable1(table);return;
            }
            if(name=="REGCMP_PAIRWISE")
            {
                int index=-1;std::string term;if(!modelAndTerm(index,term))return;
                auto const* dataframe=applicationState.datasets().find(state.group);
                const std::string id="regcmp_pairwise:"+state.id+":"+std::to_string(index)+":"+term;
                auto const& model=state.models[(size_t)index];
                const auto dependency=BeginRegressionDerivedOutputRequest(
                    id,"table","linear_comparison",model.id,command);
                if(!dataframe||!model.fit.ok)
                {ShowTable1(GLMPairwiseFailureState(state.group,term,id,"Fit this model before requesting pairwise comparisons."));return;}
                ::rlispstat::core::PlotModel seed;::rlispstat::core::PopulateDatasetSeedPlot(seed,*dataframe,state.group);
                ::rlispstat::core::GLMPairwiseComparisonRequest request;request.term=term;
                const auto modelTypes=::rlispstat::core::RegressionComparisonModelTermTypes(state,model);
                const auto eligibility=::rlispstat::core::EvaluateGLMPairwiseComparisonEligibility(seed,dataframe,
                    model.response.empty()?state.response:model.response,model.terms,modelTypes,model.fit,request);
                if(!eligibility.eligible)
                {ShowTable1(GLMPairwiseFailureState(state.group,term,id,eligibility.message));return;}
                auto dataframeCopy=*dataframe;auto modelCopy=model;if(modelCopy.response.empty())modelCopy.response=state.response;auto types=modelTypes;const auto dispatcher=dispatcherQueue_;
                const auto specification=::rlispstat::core::EffectiveRegressionComparisonModelSpecification(state,model);
                const std::vector<int> selectedRows=state.dataScopeCaptured&&state.dataScope.kind==::rlispstat::core::AnalysisScopeKind::ExplicitRowIds?state.dataScope.originalRowIds:std::vector<int>{};
                const std::string modelSpec=::rlispstat::core::EncodeStandaloneLinearMISpec(
                    specification.termTypes,specification.centeredPredictors,
                    specification.factorReferenceLevels,selectedRows);
                const std::string scope=specification.scope.empty()?state.scope:specification.scope;
                std::thread([this,dispatcher,dataframeCopy=std::move(dataframeCopy),modelCopy=std::move(modelCopy),types=std::move(types),request,id,group=state.group,modelSpec,scope,dependency]()mutable
                {
                    NativeGLMPairwiseRun run;run.result.term=request.term;
                    auto post=RunRRegressionPostEstimation(dataframeCopy,"glm_pairwise",id,
                        request.term,modelCopy.response,modelCopy.terms,scope,modelSpec);
                    run.message=post.message;
                    run.ok=post.ok&&::rlispstat::core::ParseGLMPairwiseComparisonsRResult(post.lines,run.result);
                    if(!run.ok&&run.message.empty())run.message=run.result.message;
                    dispatcher.TryEnqueue([this,run=std::move(run),group,term=request.term,id,dependency]()mutable
                    {if(!RegressionDerivedOutputRequestIsCurrent(dependency))return;if(run.ok)ShowTable1(GLMPairwiseDisplayState(group,run.result,id));else{LogStartup("GLM comparison pairwise comparison failed: "+run.message);ShowTable1(GLMPairwiseFailureState(group,term,id,run.message));}});
                }).detach();return;
            }
            if(name=="REGCMP_PARTIAL_PLOT")
            {
                int index=-1;std::string term;if(!modelAndTerm(index,term))return;
                auto const* dataframe=applicationState.datasets().find(state.group);auto const& model=state.models[(size_t)index];
                if(!dataframe)return;
                const auto specification=::rlispstat::core::EffectiveRegressionComparisonModelSpecification(state,model);
                if(std::find(specification.terms.begin(),specification.terms.end(),term)==specification.terms.end())return;
                const std::string resultId="glm_partial_plot:"+state.id+":"+std::to_string(index)+":"+term;
                std::string residualType="raw";
                for(auto const& [plotId,plot]:plots_)
                    if(plot&&(plot->glmModelId==resultId||plot->regressionPartialPendingAnalysisId==resultId)&&plot->regressionDerivedKind=="partial_regression_plot"&&!plot->displayedResidualType.empty())
                        residualType=plot->displayedResidualType;
                const auto dependency=BeginRegressionDerivedOutputRequest(
                    resultId,"plot","linear_comparison",model.id,command);
                const std::string modelSpec=::rlispstat::core::EncodeStandaloneLinearMISpec(
                    specification.termTypes,specification.centeredPredictors,
                    specification.factorReferenceLevels,
                    NativePostEstimationRows(state.dataScope,state.dataScopeCaptured));
                const std::string response=specification.response.empty()?state.response:specification.response;
                const std::string scope=specification.scope.empty()?state.scope:specification.scope;
                auto dataframeCopy=*dataframe;const auto dispatcher=dispatcherQueue_;const auto terms=specification.terms;
                std::thread([this,dispatcher,dataframeCopy=std::move(dataframeCopy),resultId,
                             term,response,terms,scope,modelSpec,residualType,group=state.group,dependency]()mutable
                {
                    auto post=RunRRegressionPostEstimation(dataframeCopy,"glm_partial_plot",resultId,
                        term,response,terms,scope,modelSpec,residualType);
                    dispatcher.TryEnqueue([this,post=std::move(post),resultId,term,group,dependency]()mutable
                    {
                        if(!RegressionDerivedOutputRequestIsCurrent(dependency))return;
                        if(!post.ok){LogStartup("Regression comparison partial plot failed: "+post.message);return;}
                        std::string message;if(!OpenPooledRegressionPartialPlot(group,resultId,term,post.lines,message))
                            LogStartup("Regression comparison partial plot failed: "+message);
                    });
                }).detach();return;
            }
            if(name=="REGCMP_INTERACTION_REPORT"||name=="REGCMP_INTERACTION_PLOT")
            {
                int index=-1;std::string term;if(!modelAndTerm(index,term))return;
                auto const* dataframe=applicationState.datasets().find(state.group);auto const& model=state.models[(size_t)index];
                if(!dataframe)return;
                const auto specification=::rlispstat::core::EffectiveRegressionComparisonModelSpecification(state,model);
                if(!::rlispstat::core::ModelIncludesTerm(model,term))
                {
                    const std::string message=::rlispstat::core::ComparisonInteractionNotIncludedMessage(term,model.label);
                    auto view=regressionComparisonViews_.find(state.id);
                    if(view!=regressionComparisonViews_.end()&&view->second)view->second->ShowMessage(message);
                    else LogStartup(message);
                    return;
                }
                const std::string reportId="regcmp_interaction:"+state.id+":"+std::to_string(index)+":"+term;
                const std::string modelSpec=::rlispstat::core::EncodeStandaloneLinearMISpec(
                    specification.termTypes,specification.centeredPredictors,
                    specification.factorReferenceLevels,
                    NativePostEstimationRows(state.dataScope,state.dataScopeCaptured));
                const std::string response=specification.response.empty()?state.response:specification.response;
                const std::string scope=specification.scope.empty()?state.scope:specification.scope;
                const std::string mode=name=="REGCMP_INTERACTION_PLOT"?"glm_interaction_plot":"glm_interaction";
                const std::string postOptions =
                    InteractionPostEstimationOptions(command);
                const auto dependency=BeginRegressionDerivedOutputRequest(
                    reportId,mode=="glm_interaction_plot"?"plot":"table",
                    "linear_comparison",model.id,command);
                auto dataframeCopy=*dataframe;const auto dispatcher=dispatcherQueue_;const auto terms=specification.terms;
                const std::string group=state.group;const std::string label=model.label;
                std::thread([this,dispatcher,dataframeCopy=std::move(dataframeCopy),mode,reportId,
                             term,response,terms,scope,modelSpec,group,label,dependency,
                             postOptions]()mutable
                {
                    auto post=RunRRegressionPostEstimation(dataframeCopy,mode,reportId,
                        term,response,terms,scope,modelSpec,postOptions);
                    dispatcher.TryEnqueue([this,post=std::move(post),mode,reportId,term,response,group,label,dependency]()mutable
                    {
                        if(!RegressionDerivedOutputRequestIsCurrent(dependency))return;
                        if(!post.ok){LogStartup("Regression comparison interaction failed: "+post.message);return;}
                        if(mode=="glm_interaction_plot")
                        {
                            std::string message;if(!OpenPooledRegressionInteractionPlot(group,reportId,response,term,post.lines,message))
                                LogStartup("Regression comparison effect plot failed: "+message);
                        }
                        else
                        {
                            ::rlispstat::core::GLMInteractionReport report;
                            if (::rlispstat::core::ParseGLMInteractionReportText(post.lines, report))
                                ShowGLMInteractionReport(reportId, group,
                                    "Effect — "+term+(label.empty()?std::string{}:" · "+label),
                                    "Estimated marginal means and contrasts from emmeans", report);
                            else LogStartup("Regression comparison interaction report could not be parsed: "+report.message);
                        }
                    });
                }).detach();return;
            }
            // Pooled comparison results remain fully inspectable. Mutations
            // are disabled until their specification is sent through the
            // pooled-Rubin refit protocol as well.
            if(state.precomputed)return;
            bool fitChanged=true;bool forceRefit=false;
            if(name=="REGCMP_SET_RESPONSE")
            {
                if(value.empty())return;state.response=value;
                state.termRows.erase(std::remove_if(state.termRows.begin(),state.termRows.end(),[&](auto const& term){return ::rlispstat::core::ModelTermShouldBeRemoved(term,value);}),state.termRows.end());
                for(auto& model:state.models)
                {
                    const auto edit=::rlispstat::core::SetModelSpecificationResponse(model,value);
                    model.candidateTerms.erase(std::remove_if(
                        model.candidateTerms.begin(),model.candidateTerms.end(),
                        [&](auto const& term){return ::rlispstat::core::ModelTermShouldBeRemoved(term,value);}),
                        model.candidateTerms.end());
                    if(edit.changed)::rlispstat::core::InvalidateRegressionComparisonModel(state,model);
                }
            }
            else if(name=="REGCMP_ADD_TERM")
            {
                if(state.models.empty())return;const int active=std::clamp(state.activeModel,0,(int)state.models.size()-1);if(!addTermToModel(active,value))return;
            }
            else if(name=="REGCMP_ADD_TERM_MODEL")
            {
                auto [modelText,term]=split(value);const int index=std::atoi(modelText.c_str());if(!addTermToModel(index,term))return;
            }
            else if(name=="REGCMP_REMOVE_TERM")
            {
                if(!::rlispstat::core::RemoveRegressionComparisonTermCompletely(state,value))return;
            }
            else if(name=="REGCMP_REMOVE_TERM_ALL")
            {
                if(!::rlispstat::core::RemoveRegressionComparisonTermCompletely(state,value))return;
            }
            else if(name=="REGCMP_ADD_TERM_ALL")
            {
                if(!::rlispstat::core::IncludeRegressionComparisonTermInAllModels(
                    state,value,comparisonVariables))return;
            }
            else if(name=="REGCMP_SET_TYPE")
            {
                auto [modelText,remainder]=split(value);auto [term,type]=split(remainder);
                const int index=std::atoi(modelText.c_str());
                std::string message;
                if(term.empty()||(type!="numeric"&&type!="factor")||
                    !::rlispstat::core::ApplyRegressionComparisonTermType(
                        state,index,term,type,&message))
                {
                    if(!message.empty())LogStartup(message);
                    return;
                }
            }
            else if(name=="REGCMP_TOGGLE_TERM")
            {
                auto [modelText,term]=split(value);const int index=std::atoi(modelText.c_str());
                if(!::rlispstat::core::ToggleRegressionComparisonTerm(
                    state,index,term,comparisonVariables))return;
            }
            else if(name=="REGCMP_REPLACE_TERM")
            {
                auto [modelText,remainder]=split(value);auto [term,replacement]=split(remainder);const int index=std::atoi(modelText.c_str());
                if(!::rlispstat::core::ReplaceRegressionComparisonTerm(
                    state,index,term,replacement,comparisonVariables))return;
            }
            else if(name=="REGCMP_ADD_INTERACTION")
            {
                auto [modelText,term]=split(value);const int index=std::atoi(modelText.c_str());
                if(!::rlispstat::core::IncludeRegressionComparisonTerm(
                    state,index,term,comparisonVariables))return;
            }
            else if(name=="REGCMP_TOGGLE_CENTER")
            {
                auto [modelText,term]=split(value);const int index=std::atoi(modelText.c_str());
                if(!::rlispstat::core::ToggleRegressionComparisonPredictorCentering(state,index,term))return;
            }
            else if(name=="REGCMP_ADD_MODEL")
            {
                ::rlispstat::core::AddRegressionComparisonModel(state,state.activeModel,false);
            }
            else if(name=="REGCMP_ADD_EMPTY_MODEL")
            {
                ::rlispstat::core::AddRegressionComparisonModel(state,state.activeModel,true);
            }
            else if(name=="REGCMP_DUPLICATE_MODEL")
            {
                const int source=std::atoi(value.c_str());if(::rlispstat::core::AddRegressionComparisonModel(state,source,false)<0)return;
            }
            else if(name=="REGCMP_DELETE_MODEL")
            {
                const int index=std::atoi(value.c_str());if(!::rlispstat::core::DeleteRegressionComparisonModel(state,index))return;
            }
            else if(name=="REGCMP_RENAME_MODEL")
            {
                auto [modelText,label]=split(value);const int index=std::atoi(modelText.c_str());if(index<0||(size_t)index>=state.models.size()||label.empty())return;state.models[(size_t)index].label=label;fitChanged=false;
            }
            else if(name=="REGCMP_SET_ACTIVE")
            {
                const int index=std::atoi(value.c_str());if(!::rlispstat::core::SetRegressionComparisonActiveModel(state,index)&&state.activeModel!=index)return;fitChanged=false;
            }
            else if(name=="REGCMP_SET_SCOPE")
            {
                ::rlispstat::core::AnalysisScope savedScope;
                if (::rlispstat::core::SavedAnalysisScopeNameFromChoiceValue(value))
                {
                    std::string error;
                    if(!applicationState.resolveSavedAnalysisScopeChoice(
                            state.group,value,savedScope,&error))return;
                    state.scope="selected";state.dataScope=std::move(savedScope);
                    state.dataScopeCaptured=true;
                }
                else
                {
                    if(value!="all"&&value!="selected"&&value!="unselected")return;
                    state.scope=value;state.dataScope={};state.dataScopeCaptured=false;
                }
                for(auto& model:state.models)::rlispstat::core::InvalidateRegressionComparisonModel(state,model);
            }
            else if(name=="REGCMP_TOGGLE_AUTO")
            {
                state.autoRefit=!state.autoRefit;fitChanged=false;
                if(state.autoRefit)
                {
                    for(auto& model:state.models)
                        ::rlispstat::core::InvalidateRegressionComparisonModel(state,model);
                    forceRefit=true;
                }
            }
            else if(name=="REGCMP_TOGGLE_INFO"){state.showInformationCriteria=!state.showInformationCriteria;fitChanged=false;}
            else if(name=="REGCMP_REFIT"){forceRefit=true;for(auto& model:state.models)::rlispstat::core::InvalidateRegressionComparisonModel(state,model);}
            else if(name=="REGCMP_REFIT_MODEL")
            {const int index=std::atoi(value.c_str());if(index<0||(size_t)index>=state.models.size())return;state.activeModel=index;::rlispstat::core::InvalidateRegressionComparisonModel(state,state.models[(size_t)index]);forceRefit=true;}
            else return;
            ::rlispstat::core::RefreshRegressionTermRowsFromFits(state);
            state.rFitPending=false;state.lastRFitSignature.clear();
            if(forceRefit||(fitChanged&&state.autoRefit))QueueRegressionComparison(state);
            ShowRegressionComparison(state.id);return;
        }
        if (name.rfind("GCOMP_", 0) == 0 && command.size() >= 2)
        {
            auto& applicationState=commandDispatcher_->applicationState();
            auto found=applicationState.generalizedComparisons().find(command[1]);
            if(found==applicationState.generalizedComparisons().end())return;
            auto& state=found->second;const std::string value=command.size()>=3?command[2]:std::string{};
            auto const* dataframe=applicationState.datasets().find(state.group);
            const auto comparisonVariables=::rlispstat::core::AvailableVariableNames(state.seed,dataframe);
            auto split=[](std::string const& encoded){const auto separator=encoded.find('\x1f');return std::pair<std::string,std::string>{separator==std::string::npos?encoded:encoded.substr(0,separator),separator==std::string::npos?std::string{}:encoded.substr(separator+1)};};
            auto addTermToModel=[&](int index,std::string const& term)
            {return ::rlispstat::core::IncludeGeneralizedComparisonTerm(state,index,term,comparisonVariables);};
            if(name=="GCOMP_PARTIAL_PLOT")
            {
                auto [modelText,term]=split(value);const int index=std::atoi(modelText.c_str());
                if(!dataframe||index<0||(size_t)index>=state.models.size()||term.empty())return;
                const auto task=NativePostEstimationTask(state,state.models[(size_t)index]);
                if(std::find(task.terms.begin(),task.terms.end(),term)==task.terms.end())return;
                const std::string resultId="gglm_partial_plot:"+state.id+":"+std::to_string(index)+":"+term;
                std::string residualType=state.models[(size_t)index].diagnosticOptions.residualType.empty()
                    ? "working":state.models[(size_t)index].diagnosticOptions.residualType;
                for(auto const& [plotId,plot]:plots_)
                    if(plot&&(plot->glmModelId==resultId||plot->regressionPartialPendingAnalysisId==resultId)&&plot->regressionDerivedKind=="partial_regression_plot"&&!plot->displayedResidualType.empty())
                        residualType=plot->displayedResidualType;
                const auto dependency=BeginRegressionDerivedOutputRequest(
                    resultId,"plot","generalized_comparison",state.models[(size_t)index].id,command);
                auto dataframeCopy=*dataframe;const auto dispatcher=dispatcherQueue_;
                std::thread([this,dispatcher,dataframeCopy=std::move(dataframeCopy),task,
                             resultId,term,residualType,dependency]()mutable
                {
                    auto post=RunRRegressionPostEstimation(dataframeCopy,"gglm_partial_plot",resultId,
                        term,task.response,task.terms,task.scope,
                        ::rlispstat::core::EncodeStandaloneGeneralizedMISpec(task),residualType);
                    dispatcher.TryEnqueue([this,post=std::move(post),task,resultId,term,dependency]()mutable
                    {
                        if(!RegressionDerivedOutputRequestIsCurrent(dependency))return;
                        if(!post.ok){LogStartup("Generalized comparison partial plot failed: "+post.message);return;}
                        std::string message;if(!OpenPooledRegressionPartialPlot(task.group,resultId,term,post.lines,message))
                            LogStartup("Generalized comparison partial plot failed: "+message);
                    });
                }).detach();return;
            }
            if(name=="GCOMP_PAIRWISE"||name=="GCOMP_INTERACTION_REPORT"||
               name=="GCOMP_INTERACTION_PLOT")
            {
                auto [modelText,term]=split(value);const int index=std::atoi(modelText.c_str());
                if(!dataframe||index<0||(size_t)index>=state.models.size()||term.empty())return;
                const auto task=NativePostEstimationTask(state,state.models[(size_t)index]);
                if(!::rlispstat::core::GeneralizedModelIncludesTerm(state.models[(size_t)index],term))
                {
                    if(name=="GCOMP_INTERACTION_REPORT"||name=="GCOMP_INTERACTION_PLOT")
                    {
                        const std::string message=::rlispstat::core::ComparisonInteractionNotIncludedMessage(
                            term,state.models[(size_t)index].label);
                        auto view=generalizedComparisonViews_.find(state.id);
                        if(view!=generalizedComparisonViews_.end()&&view->second)view->second->ShowMessage(message);
                        else LogStartup(message);
                    }
                    return;
                }
                if(name=="GCOMP_PAIRWISE")
                {
                    const auto type=task.termTypes.find(term);
                    if(type==task.termTypes.end()||(type->second!="factor"&&type->second!="ordered"))return;
                }
                else if(term.find(':')==std::string::npos &&
                        name!="GCOMP_INTERACTION_PLOT")return;
                const std::string mode=name=="GCOMP_PAIRWISE"?"gglm_pairwise":
                    name=="GCOMP_INTERACTION_PLOT"?"gglm_interaction_plot":"gglm_interaction";
                const std::string resultId=mode+":"+state.id+":"+std::to_string(index)+":"+term;
                const std::string postOptions =
                    InteractionPostEstimationOptions(command);
                const std::string label=state.models[(size_t)index].label;
                const auto dependency=BeginRegressionDerivedOutputRequest(
                    resultId,mode=="gglm_interaction_plot"?"plot":"table",
                    "generalized_comparison",state.models[(size_t)index].id,command);
                auto dataframeCopy=*dataframe;const auto dispatcher=dispatcherQueue_;
                std::thread([this,dispatcher,dataframeCopy=std::move(dataframeCopy),task,
                             mode,resultId,term,label,dependency,postOptions]()mutable
                {
                    auto post=RunRRegressionPostEstimation(dataframeCopy,mode,resultId,
                        term,task.response,task.terms,task.scope,
                        ::rlispstat::core::EncodeStandaloneGeneralizedMISpec(task),
                        postOptions);
                    dispatcher.TryEnqueue([this,post=std::move(post),task,mode,resultId,term,label,dependency]()mutable
                    {
                        if(!RegressionDerivedOutputRequestIsCurrent(dependency))return;
                        if(!post.ok){LogStartup("Generalized comparison post-estimation failed: "+post.message);return;}
                        if(mode=="gglm_pairwise")
                        {
                            ::rlispstat::core::GLMPairwiseComparisonResult result;result.term=term;
                            if(::rlispstat::core::ParseGLMPairwiseComparisonsRResult(post.lines,result))
                                ShowTable1(GLMPairwiseDisplayState(task.group,result,resultId));
                            else LogStartup("Generalized comparison pairwise comparison failed: "+result.message);
                        }
                        else if(mode=="gglm_interaction_plot")
                        {
                            std::string message;if(!OpenPooledRegressionInteractionPlot(
                                task.group,resultId,task.response,term,post.lines,message))
                                LogStartup("Generalized comparison effect plot failed: "+message);
                        }
                        else
                        {
                            ::rlispstat::core::GLMInteractionReport report;
                            if (::rlispstat::core::ParseGLMInteractionReportText(post.lines, report))
                                ShowGLMInteractionReport(resultId, task.group,
                                    "Effect — "+term+(label.empty()?std::string{}:" · "+label),
                                    task.countDistribution == "hurdle_beta_binomial_ceiling"
                                        ? "Response-scale effects derived in R from both ZABB components"
                                        : "Estimated marginal means and contrasts from emmeans",
                                    report);
                            else LogStartup("Generalized comparison interaction report could not be parsed: "+report.message);
                        }
                    });
                }).detach();return;
            }
            if(name=="GCOMP_OPEN_SINGLE")
            {
                const int index=std::atoi(value.c_str());if(index<0||(size_t)index>=state.models.size())return;
                auto const& comparisonModel=state.models[(size_t)index];
                auto single=::rlispstat::core::CreateSingleModelFromGeneralizedComparison(
                    state,index,"gcomp_single:"+state.id+":"+comparisonModel.id);
                const bool needsFit=!single.ok;
                const auto singleId=single.id;applicationState.generalizedGLMs()[singleId]=std::move(single);
                if(needsFit){RequestGeneralizedModelFit(singleId);return;}
                ShowGeneralizedModel(singleId);return;
            }
            bool fitChanged=true;bool forceRefit=false;
            if(name=="GCOMP_SET_RESPONSE")
            {
                if(value.empty()||!dataframe)return;auto column=std::find_if(dataframe->columns.begin(),dataframe->columns.end(),[&](auto const& candidate){return candidate.name==value;});if(column==dataframe->columns.end())return;
                if(state.binaryComparison){auto coding=::rlispstat::core::InspectBinaryResponse(*column);if(!coding.ok)return;state.responseCoding=coding;}
                else if(state.countComparison&&!::rlispstat::core::InspectCountResponse(*column).ok)return;
                else if(state.modelType==::rlispstat::core::StatisticalModelType::PositiveContinuous&&!::rlispstat::core::InspectResponseDomain(*column,::rlispstat::core::ResponseDomain::StrictlyPositiveContinuous).ok)return;
                else if(state.modelType==::rlispstat::core::StatisticalModelType::Proportion){auto distribution=::rlispstat::core::FindDistributionSpecification(state.family);if(!distribution||!::rlispstat::core::InspectResponseDomain(*column,distribution->responseDomain).ok)return;}
                state.response=value;for(auto& model:state.models){const auto edit=::rlispstat::core::SetModelSpecificationResponse(model,value);if(model.exposure==value)model.exposure.clear();if(model.offsetVariable==value)model.offsetVariable.clear();model.candidateTerms.erase(std::remove_if(model.candidateTerms.begin(),model.candidateTerms.end(),[&](auto const& term){return ::rlispstat::core::ModelTermShouldBeRemoved(term,value);}),model.candidateTerms.end());model.interactionImpliedTerms.clear();if(edit.changed)::rlispstat::core::InvalidateGeneralizedComparisonModel(state,model);}::rlispstat::core::RefreshGeneralizedTermRowsFromFits(state);
            }
            else if(name=="GCOMP_SET_SCOPE")
            {
                ::rlispstat::core::AnalysisScope savedScope;
                if(::rlispstat::core::SavedAnalysisScopeNameFromChoiceValue(value))
                {
                    std::string error;
                    if(!applicationState.resolveSavedAnalysisScopeChoice(
                            state.group,value,savedScope,&error))return;
                    state.scope="selected";state.dataScope=std::move(savedScope);
                    state.dataScopeCaptured=true;
                }
                else
                {
                    if(value!="all"&&value!="selected"&&value!="unselected")return;
                    state.scope=value;state.dataScope={};state.dataScopeCaptured=false;
                }
                for(auto& model:state.models){model.scope=state.scope;::rlispstat::core::InvalidateGeneralizedComparisonModel(state,model);}
            }
            else if(name=="GCOMP_SET_LINK")
            {
                if(!::rlispstat::core::SetGeneralizedComparisonLink(state,value))return;
            }
            else if(name=="GCOMP_SET_FAMILY")
            {
                if(!::rlispstat::core::SetGeneralizedComparisonFamily(state,value))return;
            }
            else if(name=="GCOMP_SET_EVENT")
            {if(!state.binaryComparison||value.empty())return;if(value==state.responseCoding.referenceLabel||value==state.responseCoding.referenceValue){std::swap(state.responseCoding.eventLabel,state.responseCoding.referenceLabel);std::swap(state.responseCoding.eventValue,state.responseCoding.referenceValue);std::swap(state.responseCoding.eventCount,state.responseCoding.referenceCount);}else if(value!=state.responseCoding.eventLabel&&value!=state.responseCoding.eventValue)return;for(auto& model:state.models)::rlispstat::core::InvalidateGeneralizedComparisonModel(state,model);}
            else if(name=="GCOMP_ADD_TERM")
            {if(value.empty()||state.models.empty())return;const int active=std::clamp(state.activeModel,0,(int)state.models.size()-1);if(!addTermToModel(active,value))return;}
            else if(name=="GCOMP_ADD_TERM_MODEL")
            {auto [modelText,term]=split(value);const int index=std::atoi(modelText.c_str());if(!addTermToModel(index,term))return;}
            else if(name=="GCOMP_REMOVE_TERM")
            {if(!::rlispstat::core::RemoveGeneralizedComparisonTermCompletely(state,value))return;}
            else if(name=="GCOMP_REMOVE_TERM_ALL")
            {if(!::rlispstat::core::RemoveGeneralizedComparisonTermCompletely(state,value))return;}
            else if(name=="GCOMP_ADD_TERM_ALL")
            {if(!::rlispstat::core::IncludeGeneralizedComparisonTermInAllModels(state,value,comparisonVariables))return;}
            else if(name=="GCOMP_TOGGLE_TERM")
            {auto [modelText,term]=split(value);const int index=std::atoi(modelText.c_str());if(!::rlispstat::core::ToggleGeneralizedComparisonTerm(state,index,term,comparisonVariables))return;}
            else if(name=="GCOMP_REPLACE_TERM")
            {
                auto [modelText,remainder]=split(value);auto [term,replacement]=split(remainder);const int index=std::atoi(modelText.c_str());
                if(!::rlispstat::core::ReplaceGeneralizedComparisonTerm(
                    state,index,term,replacement,comparisonVariables))return;
            }
            else if(name=="GCOMP_SET_TYPE")
            {
                auto [modelText,remainder]=split(value);auto [term,type]=split(remainder);const int index=std::atoi(modelText.c_str());
                const auto metadata=::rlispstat::core::PredictorMetadataForModelSpecification(state.seed,dataframe,term);
                if(type!="numeric"&&type!="factor")return;
                if(!::rlispstat::core::ApplyGeneralizedComparisonTermType(state,index,term,type,metadata))return;
            }
            else if(name=="GCOMP_SET_REFERENCE")
            {
                auto [modelText,remainder]=split(value);auto [term,reference]=split(remainder);const int index=std::atoi(modelText.c_str());
                const auto metadata=::rlispstat::core::PredictorMetadataForModelSpecification(state.seed,dataframe,term);
                if(!::rlispstat::core::ApplyGeneralizedComparisonReferenceLevel(state,index,term,reference,metadata.levels))return;
            }
            else if(name=="GCOMP_SET_COUNT_DISTRIBUTION")
            {
                auto [modelText,distributionText]=split(value);const int index=std::atoi(modelText.c_str());
                ::rlispstat::core::CountDistribution distribution;
                if(!::rlispstat::core::ParseCountDistribution(distributionText,distribution)||
                    !::rlispstat::core::SetGeneralizedComparisonCountDistribution(state,index,distribution))return;
            }
            else if(name=="GCOMP_SET_EXPOSURE")
            {
                auto [modelText,exposure]=split(value);const int index=std::atoi(modelText.c_str());
                if(!exposure.empty())
                {
                    if(!dataframe)return;auto column=std::find_if(dataframe->columns.begin(),dataframe->columns.end(),[&](auto const& candidate){return candidate.name==exposure;});
                    if(column==dataframe->columns.end()||!::rlispstat::core::InspectCountExposure(*column).ok)return;
                }
                if(!::rlispstat::core::SetGeneralizedComparisonExposure(state,index,exposure))return;
            }
            else if(name=="GCOMP_SET_OFFSET")
            {
                auto [modelText,offset]=split(value);const int index=std::atoi(modelText.c_str());
                if(!::rlispstat::core::SetGeneralizedComparisonOffset(state,index,offset))return;
            }
            else if(name=="GCOMP_SET_TRIALS")
            {
                auto [modelText,trialsText]=split(value);const int index=std::atoi(modelText.c_str());
                char *end=nullptr;const double trials=std::strtod(trialsText.c_str(),&end);
                if(!end||end==trialsText.c_str()||*end!='\0'||!std::isfinite(trials)||trials<=0.0||std::floor(trials)!=trials)return;
                if(!::rlispstat::core::SetGeneralizedComparisonTrials(state,index,"",trials))return;
            }
            else if(name=="GCOMP_SET_MODEL_SCOPE")
            {
                auto [modelText,scope]=split(value);const int index=std::atoi(modelText.c_str());
                if(!::rlispstat::core::SetGeneralizedComparisonModelScope(state,index,scope))return;
            }
            else if(name=="GCOMP_TOGGLE_CENTER")
            {auto [modelText,term]=split(value);const int index=std::atoi(modelText.c_str());if(!::rlispstat::core::ToggleGeneralizedComparisonPredictorCentering(state,index,term))return;}
            else if(name=="GCOMP_ADD_INTERACTION")
            {auto [modelText,term]=split(value);const int index=std::atoi(modelText.c_str());if(!addTermToModel(index,term))return;}
            else if(name=="GCOMP_ADD_MODEL")
            {::rlispstat::core::AddGeneralizedComparisonModel(state,state.activeModel,false);}
            else if(name=="GCOMP_ADD_EMPTY_MODEL")
            {::rlispstat::core::AddGeneralizedComparisonModel(state,state.activeModel,true);}
            else if(name=="GCOMP_DUPLICATE_MODEL")
            {const int source=std::atoi(value.c_str());if(::rlispstat::core::AddGeneralizedComparisonModel(state,source,false)<0)return;}
            else if(name=="GCOMP_DELETE_MODEL")
            {const int index=std::atoi(value.c_str());if(!::rlispstat::core::DeleteGeneralizedComparisonModel(state,index))return;}
            else if(name=="GCOMP_RENAME_MODEL")
            {auto [modelText,label]=split(value);const int index=std::atoi(modelText.c_str());if(index<0||(size_t)index>=state.models.size()||label.empty())return;state.models[(size_t)index].label=label;fitChanged=false;}
            else if(name=="GCOMP_SET_RESIDUAL")
            {
                auto [modelId,residualType]=split(value);
                auto* model=::rlispstat::core::GeneralizedComparisonModelById(state,modelId);
                if(!model||!::rlispstat::core::IsGeneralizedResidualTypeAvailable(model->fit,residualType))return;
                if(model->diagnosticOptions.residualType!=residualType)
                {
                    model->diagnosticOptions.residualType=residualType;
                    ++model->diagnosticOptions.version;
                    model->fit.diagnosticOptions=model->diagnosticOptions;
                }
                ShowGeneralizedComparison(state.id);
                RefreshDiagnosticPlots();
                return;
            }
            else if(name=="GCOMP_SET_ACTIVE")
            {const int index=std::atoi(value.c_str());if(!::rlispstat::core::SetGeneralizedComparisonActiveModel(state,index)&&state.activeModel!=index)return;fitChanged=false;}
            else if(name=="GCOMP_TOGGLE_AUTO")
            {
                state.autoRefit=!state.autoRefit;fitChanged=false;
                if(state.autoRefit)
                {
                    for(auto& model:state.models)
                        ::rlispstat::core::InvalidateGeneralizedComparisonModel(state,model);
                    forceRefit=true;
                }
            }
            else if(name=="GCOMP_TOGGLE_INFO"){state.showInformationCriteria=!state.showInformationCriteria;fitChanged=false;}
            else if(name=="GCOMP_REFIT"){forceRefit=true;for(auto& model:state.models)::rlispstat::core::InvalidateGeneralizedComparisonModel(state,model);}
            else if(name=="GCOMP_REFIT_MODEL")
            {const int index=std::atoi(value.c_str());if(index<0||(size_t)index>=state.models.size())return;state.activeModel=index;::rlispstat::core::InvalidateGeneralizedComparisonModel(state,state.models[(size_t)index]);forceRefit=true;}
            else return;
            ::rlispstat::core::RefreshGeneralizedTermRowsFromFits(state);
            state.rFitPending=false;state.lastRFitSignature.clear();
            if(forceRefit||(fitChanged&&state.autoRefit))QueueGeneralizedComparison(state);
            ShowGeneralizedComparison(state.id);return;
        }
        if(name.rfind("MODEL_TRELLIS_",0)==0&&command.size()>=2)
        {
            auto found=commandDispatcher_->applicationState().modelTrellises().find(command[1]);if(found==commandDispatcher_->applicationState().modelTrellises().end())return;auto& state=found->second;const std::string value=command.size()>=3?command[2]:std::string{};auto const* dataframe=commandDispatcher_->applicationState().datasets().find(state.specification.baseModel.group);if(!dataframe)return;bool refit=false;bool rebuild=false;std::string error;
            if(name=="MODEL_TRELLIS_ADD_TERM"){if(!::rlispstat::core::AddModelTrellisIndependentVariable(state.specification,value,*dataframe,&error))return;rebuild=refit=true;}
            else if(name=="MODEL_TRELLIS_REMOVE_TERM"){if(!::rlispstat::core::RemoveModelTrellisTerm(state.specification,value))return;rebuild=refit=true;}
            else if(name=="MODEL_TRELLIS_SET_CONDITION")
            {const auto separator=value.find('|');if(separator==std::string::npos)return;::rlispstat::core::ModelTrellisDimension dimension;if(value.substr(0,separator)=="rows")dimension=::rlispstat::core::ModelTrellisDimension::Rows;else dimension=::rlispstat::core::ModelTrellisDimension::Columns;const auto variable=value.substr(separator+1);const std::optional<std::string> selected=variable.empty()?std::nullopt:std::optional<std::string>(variable);if(!::rlispstat::core::SetModelTrellisConditioningVariable(state.specification,dimension,selected,*dataframe,&error))return;rebuild=refit=true;}
            else if(name=="MODEL_TRELLIS_SWAP"){::rlispstat::core::SwapModelTrellisDimensions(state.specification);for(auto& panel:state.panels){std::swap(panel.key.rowLevelId,panel.key.columnLevelId);std::swap(panel.rowLevelLabel,panel.columnLevelLabel);}}
            else if(name=="MODEL_TRELLIS_SET_CONTENT")
            {using Content=::rlispstat::core::ModelTrellisPanelContent;if(value=="compact")state.specification.panelContent=Content::CompactSummary;else if(value=="selected")state.specification.panelContent=Content::SelectedResult;else if(value=="coefficients")state.specification.panelContent=Content::Coefficients;else if(value=="term_tests")state.specification.panelContent=Content::TermTests;else if(value=="model_fit")state.specification.panelContent=Content::ModelFit;else return;}
            else if(name=="MODEL_TRELLIS_SET_RESULT")
            {if(value.rfind("term|",0)==0){state.specification.displayedTermId=value.substr(5);state.specification.displayedCoefficientId.reset();}else if(value.rfind("coef|",0)==0){state.specification.displayedCoefficientId=value.substr(5);state.specification.displayedTermId.reset();}else{state.specification.displayedTermId.reset();state.specification.displayedCoefficientId.reset();}state.specification.panelContent=::rlispstat::core::ModelTrellisPanelContent::SelectedResult;}
            else if(name=="MODEL_TRELLIS_SET_ADJUSTMENT")
            {using Adjustment=::rlispstat::core::ModelTrellisPAdjustment;if(value=="none")state.specification.pAdjustment=Adjustment::None;else if(value=="holm")state.specification.pAdjustment=Adjustment::Holm;else if(value=="bonferroni")state.specification.pAdjustment=Adjustment::Bonferroni;else return;::rlispstat::core::ApplyModelTrellisPAdjustment(state);}
            else if(name=="MODEL_TRELLIS_SET_ARRANGEMENT")
            {using Arrangement=::rlispstat::core::ModelTrellisPanelArrangement;if(value=="automatic")state.specification.arrangement=Arrangement::Automatic;else if(value=="one_row")state.specification.arrangement=Arrangement::OneRow;else if(value=="one_column")state.specification.arrangement=Arrangement::OneColumn;else return;}
            else if(name=="MODEL_TRELLIS_REFIT")refit=true;else return;
            if(rebuild){std::set<int> rows;if(state.dataScopeCaptured&&state.dataScope.kind==::rlispstat::core::AnalysisScopeKind::ExplicitRowIds)rows.insert(state.dataScope.originalRowIds.begin(),state.dataScope.originalRowIds.end());else commandDispatcher_->applicationState().selectedRows(state.specification.baseModel.group,rows);state.panels=::rlispstat::core::BuildModelTrellisPanels(state.specification,*dataframe,rows);}
            if(refit)QueueModelTrellisFit(state);ShowModelTrellis(state.id);return;
        }
        if (name.rfind("TABLE1_", 0) == 0 && command.size() >= 2)
        {
            auto found = outputStates_.find(command[1]);
            if (found == outputStates_.end() || found->second.tableType != "table1") return;
            auto& state = found->second;
            const std::string value = command.size() >= 3 ? command[2] : std::string{};
            if (name == "TABLE1_OPEN_HISTOGRAM" || name == "TABLE1_OPEN_BARPLOT" ||
                name == "TABLE1_OPEN_BOXPLOT")
            {
                if (value.empty()) return;
                PlotWorkflowSpecification specification;
                specification.group = state.datasetId;
                if (name == "TABLE1_OPEN_HISTOGRAM")
                {
                    specification.kind = PlotWorkflowKind::Histogram;
                    specification.xVariable = value;
                }
                else if (name == "TABLE1_OPEN_BARPLOT")
                {
                    specification.kind = PlotWorkflowKind::BarChart;
                    specification.secondaryVariable = value;
                }
                else
                {
                    specification.kind = PlotWorkflowKind::Boxplot;
                    specification.yVariable = value;
                    specification.groupVariable = state.groupVariable;
                }
                CreatePlotWorkflow(specification);
                return;
            }
            if (name == "TABLE1_ADD_VARIABLE")
            {
                if (!value.empty() && value != state.groupVariable &&
                    std::find(state.variables.begin(), state.variables.end(), value) == state.variables.end())
                    state.variables.push_back(value);
            }
            else if (name == "TABLE1_REMOVE_VARIABLE")
            {
                if (state.variables.size() <= 1) return;
                state.variables.erase(std::remove(state.variables.begin(), state.variables.end(), value), state.variables.end());
                state.variableTypes.erase(value);
                // Values already pooled for the remaining variables remain
                // valid while R refreshes multiplicity adjustments and
                // footnotes. Remove the requested block immediately so the
                // context-menu command never appears to have been ignored.
                state.rows.erase(std::remove_if(state.rows.begin(), state.rows.end(),
                    [&value](auto const& row) { return row.variable == value; }),
                    state.rows.end());
                if (auto view = outputViews_.find(state.id);
                    view != outputViews_.end() && view->second)
                    view->second->Show(state);
            }
            else if (name == "TABLE1_REPLACE_VARIABLE")
            {
                const auto separator = value.find('\x1f');
                if (separator == std::string::npos) return;
                const std::string oldVariable = value.substr(0, separator);
                const std::string newVariable = value.substr(separator + 1);
                if (newVariable.empty() || newVariable == state.groupVariable ||
                    std::find(state.variables.begin(), state.variables.end(), newVariable) != state.variables.end()) return;
                auto selected = std::find(state.variables.begin(), state.variables.end(), oldVariable);
                if (selected == state.variables.end()) return;
                *selected = newVariable;
            }
            else if (name == "TABLE1_SET_TYPE")
            {
                const auto separator = value.find('\x1f');
                if (separator == std::string::npos) return;
                const std::string variable = value.substr(0, separator);
                const std::string type = value.substr(separator + 1);
                if (variable.empty() || (type != "numeric" && type != "categorical" && type != "ordinal")) return;
                const std::string backendType = type == "categorical" ? "factor" :
                    type == "ordinal" ? "ordered" : "numeric";
                const std::string reply = commandDispatcher_->dispatch({
                    "SET_VARIABLE_TYPE", state.datasetId, variable, backendType});
                if (reply.rfind("ERR ", 0) == 0) {
                    LogStartup("Variable type command failed: " + reply);
                    return;
                }
                state.variableTypes[variable] = type;
            }
            else if (name == "TABLE1_SET_GROUP")
            {
                const bool wasUngrouped = state.groupVariable.empty();
                state.groupVariable = value;
                state.variables.erase(std::remove(state.variables.begin(), state.variables.end(), value), state.variables.end());
                if (state.groupVariable.empty()) { state.showP = false; state.showTest = false; }
                else if (wasUngrouped) { state.showP = true; state.showTest = true; }
            }
            else if (name == "TABLE1_TOGGLE_P") state.showP = !state.showP;
            else if (name == "TABLE1_TOGGLE_TEST") state.showTest = !state.showTest;
            else return;
            ::rlispstat::core::MainRTable1Task task;
            task.id = state.id; task.group = state.datasetId; task.variables = state.variables;
            task.groupVariable = state.groupVariable; task.includeMissing = true;
            if (auto const* dataframe = commandDispatcher_->applicationState().datasets().find(
                    state.datasetId))
                task.variableTypes = Table1CanonicalVariableTypes(
                    *dataframe, task.variables, task.groupVariable);
            for (auto const& [variable, type] : state.variableTypes)
                if (std::find(task.variables.begin(), task.variables.end(), variable) != task.variables.end())
                    task.variableTypes[variable] = type;
            task.showP = state.showP; task.showTest = state.showTest; task.showN = true;
            task.ordinalAs = "ordinal";
            task.scope = state.dataScopeCaptured ? ScopeName(state.dataScope) : "all";
            task.scopeDescription = state.dataScopeCaptured ? state.dataScope.sourceDescription : "All observations";
            if (state.dataScopeCaptured && state.dataScope.kind == ::rlispstat::core::AnalysisScopeKind::ExplicitRowIds)
                task.rows = state.dataScope.originalRowIds;
            commandDispatcher_->prepareTable1Task(task);
            pendingMainRTasks_.table1Tasks.push_back(std::move(task));
            MarkMainRTaskPending();
            return;
        }
        if (name == "TABLE_ADD_ROW" || name == "TABLE_REPLACE_ROW" ||
            name == "TABLE_REMOVE_ROW" ||
            name == "TABLE_SET_SPLIT" || name == "TABLE_SET_DISPLAY")
        {
            if (command.size() < 3) return;
            auto found = outputStates_.find(command[1]);
            if (found == outputStates_.end() || (found->second.tableType != "nested_contingency" &&
                found->second.tableType != "mi_contingency")) return;
            auto state = found->second;
            if (name == "TABLE_ADD_ROW")
            {
                if (!command[2].empty() && command[2] != state.groupVariable &&
                    std::find(state.variables.begin(), state.variables.end(), command[2]) == state.variables.end())
                    state.variables.push_back(command[2]);
            }
            else if (name == "TABLE_REPLACE_ROW")
            {
                const auto separator = command[2].find('\x1f');
                if (separator == std::string::npos) return;
                const auto oldVariable = command[2].substr(0, separator);
                const auto newVariable = command[2].substr(separator + 1);
                if (newVariable.empty() || newVariable == state.groupVariable ||
                    std::find(state.variables.begin(), state.variables.end(), newVariable) !=
                        state.variables.end()) return;
                auto current = std::find(state.variables.begin(), state.variables.end(), oldVariable);
                if (current == state.variables.end()) return;
                *current = newVariable;
            }
            else if (name == "TABLE_REMOVE_ROW")
            {
                if (state.variables.size() <= 1) return;
                state.variables.erase(std::remove(state.variables.begin(), state.variables.end(), command[2]), state.variables.end());
            }
            else if (name == "TABLE_SET_SPLIT")
            {
                state.groupVariable = command[2];
                state.variables.erase(std::remove(state.variables.begin(), state.variables.end(), state.groupVariable), state.variables.end());
            }
            else state.nestedDisplayMode = command[2];
            commandDispatcher_->applicationState().captureAnalysisScope(state.datasetId,state.dataScope,state.dataScopeCaptured);
            auto const* dataframe = commandDispatcher_->applicationState().datasets().find(state.datasetId);
            if (!dataframe || state.variables.empty()) return;
            if (dataframe->datasetType == "multiple_imputation")
            {
                ::rlispstat::core::MainRAnalysisWorkflowTask task;
                task.id=state.id;task.kind="contingency";task.group=state.datasetId;
                task.variables=state.variables;task.groupVariable=state.groupVariable;
                task.method=state.nestedDisplayMode;
                if(state.dataScopeCaptured && state.dataScope.kind==::rlispstat::core::AnalysisScopeKind::ExplicitRowIds) {
                    task.rows=state.dataScope.originalRowIds;
                    if(task.rows.empty()) return;
                }
                outputStates_[state.id]=state;
                pendingMainRTasks_.analysisWorkflowTasks.push_back(std::move(task));
                MarkMainRTaskPending();
                return;
            }
            const auto* scope = state.dataScopeCaptured ? &state.dataScope : nullptr;
            auto rebuilt = ::rlispstat::core::NestedContingencyTableStateForDataFrame(
                *dataframe, state.id, state.variables, state.groupVariable,
                state.nestedDisplayMode, scope);
            rebuilt.sourcePlotId = state.sourcePlotId;
            rebuilt.linkEnabled = state.linkEnabled; rebuilt.nativeGenerated = true;
            ShowTable1(rebuilt);
            return;
        }
        if (name == "CORR_SET_SCOPE" && command.size() >= 3)
        {
            auto& applicationState = commandDispatcher_->applicationState();
            auto found = applicationState.correlationMatrices().find(command[1]);
            if (found == applicationState.correlationMatrices().end() ||
                found->second.precomputed) return;
            auto const* dataframe = applicationState.datasets().find(found->second.group);
            if (!dataframe) return;
            if (::rlispstat::core::SavedAnalysisScopeNameFromChoiceValue(command[2]))
            {
                std::string error;
                if (!applicationState.resolveSavedAnalysisScopeChoice(
                        found->second.group, command[2], found->second.dataScope,
                        &error)) return;
            }
            else if (command[2] == "selected")
            {
                std::set<int> selected;
                applicationState.selectedRows(found->second.group, selected);
                found->second.dataScope = ::rlispstat::core::ExplicitAnalysisScope(
                    found->second.group,
                    std::vector<int>(selected.begin(), selected.end()),
                    ::rlispstat::core::AnalysisScopeSourceKind::CurrentSelection,
                    "Selected rows", static_cast<std::size_t>(std::max(0, dataframe->rows)));
            }
            else if (command[2] == "all")
            {
                found->second.dataScope = ::rlispstat::core::AllObservationsAnalysisScope(
                    found->second.group,
                    static_cast<std::size_t>(std::max(0, dataframe->rows)));
            }
            else return;
            found->second.dataScopeCaptured = true;
            found->second.selectedRow = -1;
            found->second.selectedCol = -1;
            std::vector<std::string> refit{ "CORR_SET_VARIABLES", command[1],
                std::to_string(found->second.variables.size()) };
            refit.insert(refit.end(), found->second.variables.begin(),
                found->second.variables.end());
            commandDispatcher_->dispatch(refit);
            return;
        }
        if (name=="CORR_SET_MISSING" || name=="CORR_TOGGLE_DISPLAY" || name=="CORR_SET_PART") {
            commandDispatcher_->dispatch(command);return;
        }
        if (name == "CORR_SET_TYPE" && command.size() >= 3)
        {
            auto found = commandDispatcher_->applicationState().correlationMatrices().find(command[1]);
            if (found == commandDispatcher_->applicationState().correlationMatrices().end() ||
                found->second.precomputed) return;
            const auto separator = command[2].find('\x1f');
            if (separator == std::string::npos) return;
            const auto variable = command[2].substr(0, separator);
            const auto type = command[2].substr(separator + 1);
            if (type != "numeric" && type != "factor") return;
            if (type != "numeric" && found->second.variables.size() <= 2) return;
            if (std::find(found->second.variables.begin(), found->second.variables.end(), variable) ==
                found->second.variables.end()) return;
            const auto group = found->second.group;
            auto variables = found->second.variables;
            const auto reply = commandDispatcher_->dispatch(
                { "SET_VARIABLE_TYPE", group, variable, type });
            if (reply.rfind("ERR ", 0) == 0) return;
            if (type != "numeric")
            {
                variables.erase(std::remove(variables.begin(), variables.end(), variable),
                    variables.end());
                std::vector<std::string> refit{ "CORR_SET_VARIABLES", command[1],
                    std::to_string(variables.size()) };
                refit.insert(refit.end(), variables.begin(), variables.end());
                commandDispatcher_->dispatch(refit);
            }
            return;
        }
        if (name == "MODEL_COMPARE_MODELS" && command.size() >= 2)
        {
            auto& applicationState = commandDispatcher_->applicationState();
            auto source = applicationState.groupModels().find(command[1]);
            if (source == applicationState.groupModels().end() ||
                source->second.response.empty()) return;
            auto fit = applicationState.linearModelFits().find(command[1]);
            auto const* dataframe = applicationState.datasets().find(source->second.group);
            ::rlispstat::core::PlotModel seed;
            if (dataframe)
                ::rlispstat::core::PopulateDatasetSeedPlot(
                    seed, *dataframe, source->second.group);
            const std::string id = StartupId("regcmp_", source->second.group);
            auto comparison = ::rlispstat::core::CreateRegressionComparisonFromSingleModel(
                id, source->second,
                fit == applicationState.linearModelFits().end() ? nullptr : &fit->second,
                dataframe ? &seed : nullptr);
            const bool needsFit = comparison.models.empty() ||
                ::rlispstat::core::RegressionComparisonResolvedFitState(
                    comparison, comparison.models.front()) !=
                    ::rlispstat::core::RegressionComparisonFitState::Valid;
            applicationState.regressionComparisons()[id] = std::move(comparison);
            ShowRegressionComparison(id);
            if (needsFit) QueueRegressionComparison(
                applicationState.regressionComparisons().at(id));
            return;
        }
        if (name == "MODEL_PAIRWISE" && command.size() >= 3)
        {
            auto& applicationState = commandDispatcher_->applicationState();
            auto model = applicationState.groupModels().find(command[1]);
            auto fit = applicationState.linearModelFits().find(command[1]);
            auto const* dataframe = model == applicationState.groupModels().end()
                ? nullptr : applicationState.datasets().find(model->second.group);
            const std::string id = "glm_pairwise:" + command[1] + ":" + command[2];
            const auto dependency = BeginRegressionDerivedOutputRequest(
                id, "table", "linear_single", command[1], command);
            if (model == applicationState.groupModels().end() ||
                fit == applicationState.linearModelFits().end() || !dataframe)
            {
                ShowTable1(GLMPairwiseFailureState(
                    command[1], command[2], id,
                    "Fit the model before requesting pairwise comparisons."));
                return;
            }
            ::rlispstat::core::PlotModel seed;
            ::rlispstat::core::PopulateDatasetSeedPlot(seed, *dataframe, command[1]);
            ::rlispstat::core::GLMPairwiseComparisonRequest request;
            request.term = command[2];
            const auto eligibility =
                ::rlispstat::core::EvaluateGLMPairwiseComparisonEligibility(
                    seed, dataframe, model->second.response,
                    model->second.terms, model->second.termTypes,
                    fit->second, request);
            if (!eligibility.eligible)
            {
                ShowTable1(GLMPairwiseFailureState(
                    command[1], command[2], id, eligibility.message));
                return;
            }

            auto dataframeCopy = *dataframe;
            auto modelCopy = model->second;
            const auto dispatcher = dispatcherQueue_;
            std::thread([this, dispatcher, dataframeCopy = std::move(dataframeCopy),
                         seed = std::move(seed), modelCopy = std::move(modelCopy),
                         request, id, dependency]() mutable
            {
                NativeGLMPairwiseRun run;
                run.result.term = request.term;
                const std::string spec = ::rlispstat::core::EncodeStandaloneLinearMISpec(
                    modelCopy.termTypes, modelCopy.centeredPredictors,
                    modelCopy.factorReferenceLevels, {});
                auto post = RunRRegressionPostEstimation(
                    dataframeCopy, "glm_pairwise", id, request.term,
                    modelCopy.response, modelCopy.terms,
                    modelCopy.scope.empty() ? "all" : modelCopy.scope, spec);
                run.message = post.message;
                run.ok = post.ok && ::rlispstat::core::ParseGLMPairwiseComparisonsRResult(
                    post.lines, run.result);
                if (!run.ok && run.message.empty()) run.message = run.result.message;
                dispatcher.TryEnqueue([this, group = modelCopy.group,
                                       term = request.term, id,
                                       run = std::move(run), dependency]() mutable
                {
                    if (!RegressionDerivedOutputRequestIsCurrent(dependency)) return;
                    if (run.ok)
                    {
                        ShowTable1(GLMPairwiseDisplayState(group, run.result, id));
                    }
                    else
                    {
                        LogStartup("GLM pairwise comparison failed: " + run.message);
                        ShowTable1(GLMPairwiseFailureState(
                            group, term, id, run.message));
                    }
                });
            }).detach();
            return;
        }
        if (name == "MODEL_SET_TYPE" && command.size() >= 3)
        {
            const auto separator = command[2].find('\x1f');
            if (separator == std::string::npos) return;
            const auto term = command[2].substr(0, separator);
            const auto type = command[2].substr(separator + 1);
            if (type != "numeric" && type != "factor") return;
            auto& applicationState = commandDispatcher_->applicationState();
            auto found = applicationState.groupModels().find(command[1]);
            if (found == applicationState.groupModels().end()) return;
            auto const* dataframe = applicationState.datasets().find(found->second.group);
            ::rlispstat::core::PlotModel seed;
            if (dataframe)
                ::rlispstat::core::PopulateDatasetSeedPlot(seed, *dataframe, found->second.group);
            else
                return;
            const auto edit = ::rlispstat::core::SetModelSpecificationTermType(
                found->second, term, type,
                ::rlispstat::core::PredictorMetadataForLinearModel(
                    seed, dataframe, term));
            if (!edit.ok)
                LogStartup("Predictor type command failed: " + edit.message);
            else if (edit.changed)
            {
                ::rlispstat::core::MarkGroupModelChanged(found->second);
                QueueLinearModelFit(command[1]);
            }
            return;
        }
        if (name == "MODEL_SET_REFERENCE" && command.size() >= 3)
        {
            const auto separator = command[2].find('\x1f');
            if (separator == std::string::npos) return;
            const std::string term = command[2].substr(0, separator);
            const std::string reference = command[2].substr(separator + 1);
            auto& applicationState = commandDispatcher_->applicationState();
            auto model = applicationState.groupModels().find(command[1]);
            auto fit = applicationState.linearModelFits().find(command[1]);
            if (model == applicationState.groupModels().end() ||
                fit == applicationState.linearModelFits().end()) return;
            const std::string variable = ::rlispstat::core::BaseVariableForTermComponent(term);
            const auto coding = std::find_if(fit->second.factorCodings.begin(),
                fit->second.factorCodings.end(), [&](auto const& value)
                { return value.variable == variable; });
            if (coding == fit->second.factorCodings.end() ||
                std::find(coding->levels.begin(), coding->levels.end(), reference) ==
                    coding->levels.end()) return;
            const auto edit = ::rlispstat::core::SetModelSpecificationReferenceLevel(
                model->second, variable, reference, coding->levels);
            if (!edit.ok || !edit.changed) return;
            ::rlispstat::core::MarkGroupModelChanged(model->second);
            QueueLinearModelFit(command[1]);
            return;
        }
        if (name == "MODEL_TOGGLE_CENTER" && command.size() >= 3)
        {
            auto found = commandDispatcher_->applicationState().groupModels().find(command[1]);
            if (found == commandDispatcher_->applicationState().groupModels().end()) return;
            auto& state = found->second;
            const std::string value = command[2];
            bool changed = false;
            if (value == "__none__")
            {
                for (auto const& term : state.terms)
                    changed = ::rlispstat::core::SetModelSpecificationPredictorCentered(
                        state, term, false).changed || changed;
            }
            else if (value == "__all__")
            {
                for (auto const& term : state.terms)
                {
                    const auto edit = ::rlispstat::core::SetModelSpecificationPredictorCentered(
                        state, term, true);
                    changed = edit.changed || changed;
                }
            }
            else
            {
                const auto edit = ::rlispstat::core::SetModelSpecificationPredictorCentered(
                    state, value, state.centeredPredictors.count(value) == 0);
                if (!edit.ok) return;
                changed = edit.changed;
            }
            if (!changed) return;
            ::rlispstat::core::MarkGroupModelChanged(state);
            QueueLinearModelFit(command[1]);
            return;
        }
        if (name == "MODEL_REFIT" && command.size() >= 2)
        {
            RefitLinearModel(command[1]); return;
        }
        if (name.rfind("MEAN_", 0) == 0 && command.size() >= 2)
        {
            auto found = meanComparisonStates_.find(command[1]);
            if (found == meanComparisonStates_.end()) return;
            auto& state = found->second;
            const std::string value = command.size() >= 3 ? command[2] : std::string{};
            auto const* dataframe = commandDispatcher_->applicationState().datasets().find(state.datasetId);
            if (!dataframe) return;
            const auto numeric = MeanComparisonNumericVariables(*dataframe);
            const bool oneSample = state.analysisType == "one_sample_t_test";
            const bool mixedResponses = oneSample || state.analysisType == "independent_samples_t_test" ||
                state.analysisType == "paired_samples_t_test";
            const auto dependentsAvailable = MeanComparisonResponseVariables(*dataframe, mixedResponses);
            if (name == "MEAN_OPEN_DESCRIPTIVES")
            {
                const auto id = state.id;
                auto view = meanComparisonDescriptiveViews_.count(id) &&
                    meanComparisonDescriptiveViews_[id]
                    ? meanComparisonDescriptiveViews_[id] : MeanComparisonView::Create(true);
                view->SetClosedCallback([this, id]()
                {
                    if (!resettingViews_) meanComparisonDescriptiveViews_.erase(id);
                });
                meanComparisonDescriptiveViews_[id] = view;
                view->Show(state);
                return;
            }
            if (name == "MEAN_PAIRWISE")
            {
                if (state.analysisType != "one_way_anova" ||
                    state.specification.method ==
                        ::rlispstat::core::MeanComparisonMethod::KruskalWallis ||
                    value.empty() ||
                    state.groupVariable.empty()) return;
                const ::rlispstat::core::MeanComparisonRow* omnibus = nullptr;
                for (auto const& table : state.tables)
                {
                    if (table.tableId != "omnibus") continue;
                    for (auto const& row : table.rows)
                        if (row.variable == value) { omnibus = &row; break; }
                }
                if (!omnibus || omnibus->originalRowIndices.empty()) return;
                const std::string id = "mean_pairwise:" + state.id + ":" + value;
                const int generation = ++meanPairwiseGenerations_[id];
                auto dataframeCopy = *dataframe;
                auto stateCopy = state;
                const std::vector<int> rowsUsed = omnibus->originalRowIndices;
                const auto dispatcher = dispatcherQueue_;
                std::thread([this, dispatcher, dataframeCopy = std::move(dataframeCopy),
                             stateCopy = std::move(stateCopy), rowsUsed, response = value,
                             id, generation]() mutable
                {
                    ::rlispstat::core::GLMPairwiseComparisonRequest request;
                    request.term = stateCopy.groupVariable;
                    request.confidenceLevel = stateCopy.confidenceLevel;
                    request.adjustment = stateCopy.method.find("Classical") != std::string::npos
                        ? "classical-tukey" : "games-howell";
                    NativeGLMPairwiseRun run;
                    if (stateCopy.multipleImputation)
                    {
                        const std::map<std::string, std::string> types{
                            {stateCopy.groupVariable, "factor"}};
                        const std::string spec = ::rlispstat::core::EncodeStandaloneLinearMISpec(
                            types, {}, {}, rowsUsed);
                        const auto post = RunRRegressionPostEstimation(
                            dataframeCopy, "glm_pairwise", id, request.term, response,
                            {request.term}, stateCopy.dataScopeCaptured
                                ? ScopeName(stateCopy.dataScope) : "all", spec);
                        run.message = post.message;
                        run.ok = post.ok && ::rlispstat::core::ParseGLMPairwiseComparisonsRResult(
                            post.lines, run.result);
                    }
                    else
                    {
                        ::rlispstat::core::PlotModel seed;
                        ::rlispstat::core::PopulateDatasetSeedPlot(
                            seed, dataframeCopy, stateCopy.datasetId);
                        ::rlispstat::core::GLMFitSummary fit;
                        fit.ok = true;
                        fit.beta = {0.0};
                        fit.covariance = {{1.0}};
                        fit.rowsUsed = rowsUsed;
                        const std::map<std::string, std::string> types{
                            {stateCopy.groupVariable, "factor"}};
                        run = RunNativeGLMPairwiseComparisons(
                            dataframeCopy, seed, response, {request.term}, types, fit, request);
                    }
                    dispatcher.TryEnqueue([this, id, generation,
                                           sourceId = stateCopy.id,
                                           group = stateCopy.datasetId,
                                           term = request.term,
                                           run = std::move(run)]() mutable
                    {
                        auto current = meanPairwiseGenerations_.find(id);
                        if (current == meanPairwiseGenerations_.end() ||
                            current->second != generation) return;
                        if (run.ok)
                        {
                            auto output = GLMPairwiseDisplayState(group, run.result, id, true);
                            auto source = meanComparisonStates_.find(sourceId);
                            if (source != meanComparisonStates_.end())
                            {
                                output.codeReference.provenance.dataVersion =
                                    source->second.provenance.dataVersion;
                                output.codeReference.provenance.scope =
                                    source->second.provenance.scope;
                            }
                            ShowTable1(output);
                        }
                        else ShowTable1(GLMPairwiseFailureState(group, term, id,
                            run.message.empty() ? "Pairwise comparisons could not be calculated."
                                                : run.message));
                    });
                }).detach();
                return;
            }
            if (name == "MEAN_OPEN_PLOT")
            {
                const auto firstSeparator = value.find('\x1f');
                const auto secondSeparator = firstSeparator == std::string::npos
                    ? std::string::npos : value.find('\x1f', firstSeparator + 1);
                if (firstSeparator == std::string::npos || secondSeparator == std::string::npos) return;
                const auto kind = value.substr(0, firstSeparator);
                const auto variable = value.substr(firstSeparator + 1,
                    secondSeparator - firstSeparator - 1);
                const auto secondVariable = value.substr(secondSeparator + 1);
                if (variable.empty()) return;

                if (kind == "paired_scatterplot")
                {
                    if (secondVariable.empty() || secondVariable == variable) return;
                    ScatterplotSpecification specification;
                    specification.group = state.datasetId;
                    specification.xVariable = variable;
                    specification.yVariable = secondVariable;
                    specification.title = variable + " vs " + secondVariable;
                    specification.dataScope = state.dataScope;
                    specification.dataScopeCaptured = state.dataScopeCaptured;
                    CreateScatterplot(specification);
                    return;
                }
                if (kind == "paired_difference_histogram")
                {
                    if (secondVariable.empty() || secondVariable == variable) return;
                    auto findColumn = [&](std::string const& columnName)
                        -> ::rlispstat::core::DataColumn const*
                    {
                        auto column = std::find_if(dataframe->columns.begin(), dataframe->columns.end(),
                            [&](auto const& candidate) { return candidate.name == columnName; });
                        return column == dataframe->columns.end() ? nullptr : &*column;
                    };
                    const auto* firstColumn = findColumn(variable);
                    const auto* secondColumn = findColumn(secondVariable);
                    if (!firstColumn || !secondColumn) return;
                    auto model = std::make_unique<::rlispstat::core::PlotModel>();
                    ::rlispstat::core::PopulateDatasetSeedPlot(*model, *dataframe, state.datasetId);
                    model->isDatasetSeed = false;
                    model->id = "paired_difference_histogram_windows_" + std::to_string(
                        static_cast<unsigned long long>(GetTickCount64()));
                    model->kind = "histogram";
                    model->xLabel = variable + " \u2212 " + secondVariable;
                    model->title = "Histogram of paired differences: " + model->xLabel;
                    model->interactionMode = "select";
                    model->selectionMode = "replace";
                    model->histogramPoints.clear();
                    const auto count = std::min(firstColumn->values.size(), secondColumn->values.size());
                    for (std::size_t index = 0; index < count; ++index)
                    {
                        char* firstEnd = nullptr; char* secondEnd = nullptr;
                        const double firstValue = std::strtod(firstColumn->values[index].c_str(), &firstEnd);
                        const double secondValue = std::strtod(secondColumn->values[index].c_str(), &secondEnd);
                        if (firstEnd == firstColumn->values[index].c_str() ||
                            secondEnd == secondColumn->values[index].c_str() ||
                            !std::isfinite(firstValue) || !std::isfinite(secondValue)) continue;
                        model->histogramPoints.push_back({
                            firstValue - secondValue, static_cast<int>(index + 1), 0});
                    }
                    if (model->histogramPoints.empty()) return;
                    ::rlispstat::core::RebinHistogramByRule(*model, "sturges");
                    if (state.dataScopeCaptured)
                    {
                        model->dataScope = state.dataScope;
                        model->dataScopeCaptured = true;
                    }
                    auto* pointer = model.get(); plots_[model->id] = std::move(model);
                    const auto attached = AttachPlotModel(*pointer);
                    if (!attached.accepted) { plots_.erase(pointer->id); return; }
                    commandDispatcher_->plotCoordinator().activatePlot(pointer->id);
                    ShowNativePlot(*pointer);
                    return;
                }

                PlotWorkflowSpecification specification;
                specification.group = state.datasetId;
                specification.dataScope = state.dataScope;
                specification.dataScopeCaptured = state.dataScopeCaptured;
                if (kind == "histogram")
                {
                    specification.kind = PlotWorkflowKind::Histogram;
                    specification.xVariable = variable;
                }
                else if (kind == "bar_chart")
                {
                    specification.kind = PlotWorkflowKind::BarChart;
                    specification.secondaryVariable = variable;
                    if (state.analysisType == "independent_samples_t_test")
                        specification.groupVariable = state.specification.groupingVariableId.value_or("");
                }
                else if (kind == "boxplot" || kind == "grouped_boxplot")
                {
                    specification.kind = PlotWorkflowKind::Boxplot;
                    specification.yVariable = variable;
                    if (kind == "grouped_boxplot")
                        specification.groupVariable = state.specification.groupingVariableId.value_or("");
                }
                else return;
                CreatePlotWorkflow(specification);
                return;
            }
            if (name == "MEAN_SET_DEPENDENT")
            {
                if (std::find(dependentsAvailable.begin(), dependentsAvailable.end(), value) == dependentsAvailable.end()) return;
                auto& dependents = state.specification.dependentVariableIds;
                dependents.erase(std::remove(dependents.begin(), dependents.end(), value), dependents.end());
                if (dependents.empty()) dependents.push_back(value);
                else dependents.front() = value;
                if (oneSample && !state.specification.testValues.count(value))
                {
                    auto column = std::find_if(dataframe->columns.begin(), dataframe->columns.end(),
                        [&](auto const& candidate) { return candidate.name == value; });
                    const auto type = column == dataframe->columns.end() ? std::string{} :
                        ::rlispstat::core::NormalizeVariableType(column->type);
                    state.specification.testValues[value] =
                        MeanComparisonCategoricalType(type) ? 0.5 : state.specification.testValue;
                }
                if (state.specification.groupingVariableId &&
                    *state.specification.groupingVariableId == value)
                    state.specification.groupingVariableId.reset();
            }
            else if (name == "MEAN_ADD_DEPENDENT")
            {
                if (std::find(dependentsAvailable.begin(), dependentsAvailable.end(), value) == dependentsAvailable.end()) return;
                if (std::find(state.specification.dependentVariableIds.begin(),
                    state.specification.dependentVariableIds.end(), value) == state.specification.dependentVariableIds.end())
                {
                    state.specification.dependentVariableIds.push_back(value);
                    if (oneSample)
                    {
                        auto column = std::find_if(dataframe->columns.begin(), dataframe->columns.end(),
                            [&](auto const& candidate) { return candidate.name == value; });
                        const auto type = column == dataframe->columns.end() ? std::string{} :
                            ::rlispstat::core::NormalizeVariableType(column->type);
                        state.specification.testValues[value] =
                            MeanComparisonCategoricalType(type) ? 0.5 : state.specification.testValue;
                    }
                }
            }
            else if (name == "MEAN_REPLACE_DEPENDENT")
            {
                const auto separator = value.find('\x1f');
                if (separator == std::string::npos) return;
                const auto oldVariable = value.substr(0, separator);
                const auto newVariable = value.substr(separator + 1);
                if (std::find(dependentsAvailable.begin(), dependentsAvailable.end(), newVariable) == dependentsAvailable.end() ||
                    std::find(state.specification.dependentVariableIds.begin(),
                        state.specification.dependentVariableIds.end(), newVariable) !=
                        state.specification.dependentVariableIds.end() ||
                    (state.specification.groupingVariableId &&
                     *state.specification.groupingVariableId == newVariable)) return;
                auto current = std::find(state.specification.dependentVariableIds.begin(),
                    state.specification.dependentVariableIds.end(), oldVariable);
                if (current == state.specification.dependentVariableIds.end()) return;
                *current = newVariable;
                if (oneSample)
                {
                    state.specification.testValues.erase(oldVariable);
                    auto column = std::find_if(dataframe->columns.begin(), dataframe->columns.end(),
                        [&](auto const& candidate) { return candidate.name == newVariable; });
                    const auto type = column == dataframe->columns.end() ? std::string{} :
                        ::rlispstat::core::NormalizeVariableType(column->type);
                    state.specification.testValues[newVariable] =
                        MeanComparisonCategoricalType(type) ? 0.5 : state.specification.testValue;
                }
            }
            else if (name == "MEAN_REMOVE_DEPENDENT")
            {
                if (state.specification.dependentVariableIds.size() <= 1) return;
                state.specification.dependentVariableIds.erase(std::remove(
                    state.specification.dependentVariableIds.begin(),
                    state.specification.dependentVariableIds.end(), value),
                    state.specification.dependentVariableIds.end());
                state.specification.testValues.erase(value);
            }
            else if (name == "MEAN_SET_GROUP")
            {
                auto column = std::find_if(dataframe->columns.begin(), dataframe->columns.end(),
                    [&](auto const& candidate) { return candidate.name == value; });
                const bool independent = state.analysisType == "independent_samples_t_test";
                if (column == dataframe->columns.end() || !MeanComparisonGroupingCandidate(*column, independent)) return;
                state.specification.groupingVariableId = value;
                const auto levels = MeanComparisonLevels(*column);
                state.specification.groupOrderIds = independent
                    ? ::rlispstat::core::MeanComparisonDefaultIndependentGroupOrder(levels)
                    : levels;
                state.groupVariable = value;
            }
            else if (name == "MEAN_SET_REFERENCE_GROUP")
            {
                if (state.analysisType != "independent_samples_t_test" ||
                    !state.specification.groupingVariableId) return;
                const auto levelMap = std::find_if(dataframe->columns.begin(),
                    dataframe->columns.end(), [&](auto const& candidate)
                    { return candidate.name == *state.specification.groupingVariableId; });
                if (levelMap == dataframe->columns.end()) return;
                const auto levels = MeanComparisonLevels(*levelMap);
                const auto order =
                    ::rlispstat::core::MeanComparisonIndependentGroupOrderForReference(
                        levels, value);
                if (order.empty() || order == state.specification.groupOrderIds) return;
                state.specification.groupOrderIds = order;
                state.subtitle = ::rlispstat::core::MeanComparisonGroupingVariableLine(
                    *state.specification.groupingVariableId) + " \xc2\xb7 " +
                    state.method + " \xc2\xb7 Difference: " + order[0] +
                    " \xe2\x88\x92 " + order[1];
            }
            else if (name == "MEAN_SET_SCOPE")
            {
                const std::size_t totalRows = static_cast<std::size_t>(
                    std::max(0, dataframe->rows));
                if (::rlispstat::core::SavedAnalysisScopeNameFromChoiceValue(value))
                {
                    std::string error;
                    if (!commandDispatcher_->applicationState().resolveSavedAnalysisScopeChoice(
                            state.datasetId, value, state.dataScope, &error)) return;
                }
                else if (value == "all")
                    state.dataScope = ::rlispstat::core::AllObservationsAnalysisScope(
                        state.datasetId, totalRows);
                else if (value == "selected")
                {
                    std::set<int> selected;
                    commandDispatcher_->applicationState().selectedRows(
                        state.datasetId, selected);
                    state.dataScope = ::rlispstat::core::ExplicitAnalysisScope(
                        state.datasetId,
                        std::vector<int>(selected.begin(), selected.end()),
                        ::rlispstat::core::AnalysisScopeSourceKind::CurrentSelection,
                        "Selected rows", totalRows);
                }
                else return;
                state.dataScopeCaptured = true;
            }
            else if (name == "MEAN_ADD_PAIR")
            {
                const auto separator = value.find('\x1f'); if (separator == std::string::npos) return;
                const auto first = value.substr(0, separator), second = value.substr(separator + 1);
                if (first == second ||
                    std::find(dependentsAvailable.begin(), dependentsAvailable.end(), first) == dependentsAvailable.end() ||
                    std::find(dependentsAvailable.begin(), dependentsAvailable.end(), second) == dependentsAvailable.end() ||
                    !MeanComparisonPairedVariablesCompatible(*dataframe, first, second)) return;
                const bool duplicate = std::any_of(state.specification.pairs.begin(), state.specification.pairs.end(),
                    [&](auto const& pair) { return pair.firstVariableId == first && pair.secondVariableId == second; });
                if (!duplicate) state.specification.pairs.push_back({ "pair:" + first + "\x1f" + second, first, second });
            }
            else if (name == "MEAN_REPLACE_PAIR")
            {
                const auto firstSeparator = value.find('\x1e');
                const auto secondSeparator = firstSeparator == std::string::npos
                    ? std::string::npos : value.find('\x1e', firstSeparator + 1);
                if (firstSeparator == std::string::npos || secondSeparator == std::string::npos) return;
                const auto pairId = value.substr(0, firstSeparator);
                const auto side = value.substr(firstSeparator + 1,
                    secondSeparator - firstSeparator - 1);
                const auto replacement = value.substr(secondSeparator + 1);
                if ((side != "first" && side != "second") ||
                    std::find(dependentsAvailable.begin(), dependentsAvailable.end(), replacement) == dependentsAvailable.end()) return;
                auto pair = std::find_if(state.specification.pairs.begin(),
                    state.specification.pairs.end(),
                    [&](auto const& candidate) { return candidate.pairId == pairId; });
                if (pair == state.specification.pairs.end()) return;
                const auto nextFirst = side == "first" ? replacement : pair->firstVariableId;
                const auto nextSecond = side == "second" ? replacement : pair->secondVariableId;
                if (nextFirst == nextSecond ||
                    !MeanComparisonPairedVariablesCompatible(*dataframe, nextFirst, nextSecond) ||
                    std::any_of(state.specification.pairs.begin(),
                    state.specification.pairs.end(), [&](auto const& candidate)
                    {
                        return candidate.pairId != pairId && candidate.firstVariableId == nextFirst &&
                            candidate.secondVariableId == nextSecond;
                    })) return;
                pair->firstVariableId = nextFirst; pair->secondVariableId = nextSecond;
                pair->pairId = "pair:" + nextFirst + "\x1f" + nextSecond;
            }
            else if (name == "MEAN_REMOVE_PAIR")
            {
                if (state.specification.pairs.size() <= 1) return;
                state.specification.pairs.erase(std::remove_if(state.specification.pairs.begin(),
                    state.specification.pairs.end(), [&](auto const& pair) { return pair.pairId == value; }),
                    state.specification.pairs.end());
            }
            else if (name == "MEAN_REVERSE_PAIR")
            {
                auto pair = std::find_if(state.specification.pairs.begin(), state.specification.pairs.end(),
                    [&](auto const& candidate) { return candidate.pairId == value; });
                if (pair == state.specification.pairs.end()) return;
                std::swap(pair->firstVariableId, pair->secondVariableId);
                pair->pairId = "pair:" + pair->firstVariableId + "\x1f" + pair->secondVariableId;
            }
            else if (name == "MEAN_SET_METHOD")
            {
                using Method = ::rlispstat::core::MeanComparisonMethod;
                if (value == "student") state.specification.method =
                    state.analysisType == "one_sample_t_test" || state.analysisType == "paired_samples_t_test"
                    ? Method::OneSampleT : Method::StudentT;
                else if (value == "wilcoxon") state.specification.method = Method::WilcoxonSignedRank;
                else if (value == "mann_whitney") state.specification.method = Method::MannWhitney;
                else if (value == "classical") state.specification.method = Method::ClassicalAnova;
                else if (value == "kruskal_wallis") state.specification.method = Method::KruskalWallis;
                else if (value == "welch") state.specification.method =
                    state.analysisType == "one_way_anova" ? Method::WelchAnova : Method::WelchT;
                else return;
            }
            else if (name == "MEAN_SET_ALTERNATIVE")
                state.specification.alternative = value == "greater" ? ::rlispstat::core::AlternativeHypothesis::Greater :
                    value == "less" ? ::rlispstat::core::AlternativeHypothesis::Less :
                    ::rlispstat::core::AlternativeHypothesis::TwoSided;
            else if (name == "MEAN_SET_CONFIDENCE")
            {
                const double level = std::strtod(value.c_str(), nullptr);
                if (level <= 0.0 || level >= 1.0) return;
                state.specification.confidenceLevel = level; state.confidenceLevel = level;
            }
            else if (name == "MEAN_SET_ADJUSTMENT")
                state.specification.pAdjustment = ::rlispstat::core::MeanComparisonAdjustmentFromName(value);
            else if (name == "MEAN_SET_TEST_VALUE" && oneSample)
            {
                const auto separator = value.find('\x1f');
                if (separator == std::string::npos) return;
                const auto variable = value.substr(0, separator);
                char* end = nullptr;
                const double testValue = std::strtod(value.c_str() + separator + 1, &end);
                if (!end || *end != '\0' || !std::isfinite(testValue) ||
                    std::find(state.specification.dependentVariableIds.begin(),
                        state.specification.dependentVariableIds.end(), variable) ==
                        state.specification.dependentVariableIds.end()) return;
                auto column = std::find_if(dataframe->columns.begin(), dataframe->columns.end(),
                    [&](auto const& candidate) { return candidate.name == variable; });
                const auto type = column == dataframe->columns.end() ? std::string{} :
                    ::rlispstat::core::NormalizeVariableType(column->type);
                if (MeanComparisonCategoricalType(type) && (testValue < 0.0 || testValue > 1.0)) return;
                const auto current = state.specification.testValues.find(variable);
                if (current != state.specification.testValues.end() &&
                    std::fabs(current->second - testValue) < 1e-12) return;
                state.specification.testValues[variable] = testValue;
            }
            else if (name == "MEAN_SET_ALL_TEST_VALUES" && oneSample)
            {
                char* end = nullptr; const double testValue = std::strtod(value.c_str(), &end);
                if (!end || *end != '\0' || !std::isfinite(testValue)) return;
                for (auto const& variable : state.specification.dependentVariableIds)
                {
                    auto column = std::find_if(dataframe->columns.begin(), dataframe->columns.end(),
                        [&](auto const& candidate) { return candidate.name == variable; });
                    const auto type = column == dataframe->columns.end() ? std::string{} :
                        ::rlispstat::core::NormalizeVariableType(column->type);
                    if (MeanComparisonCategoricalType(type) && (testValue < 0.0 || testValue > 1.0)) return;
                }
                bool changed = std::fabs(state.specification.testValue - testValue) >= 1e-12;
                state.specification.testValue = testValue; state.testValue = testValue;
                for (auto const& variable : state.specification.dependentVariableIds)
                {
                    const auto current = state.specification.testValues.find(variable);
                    changed = changed || current == state.specification.testValues.end() ||
                        std::fabs(current->second - testValue) >= 1e-12;
                    state.specification.testValues[variable] = testValue;
                }
                if (!changed) return;
            }
            else if (name == "MEAN_SET_VARIABLE_TYPE")
            {
                const auto separator = value.find('\x1f'); if (separator == std::string::npos) return;
                const auto variable = value.substr(0, separator);
                const auto type = value.substr(separator + 1);
                if (type != "numeric" && type != "ordered" && type != "factor") return;
                const auto reply = commandDispatcher_->dispatch(
                    {"SET_VARIABLE_TYPE", state.datasetId, variable, type});
                if (reply.rfind("ERR ", 0) == 0)
                    LogStartup("Variable type command failed: " + reply);
                return;
            }
            else return;
            RefitMeanComparison(state.id); return;
        }
        if (name == "GGLM_COMPARE_MODELS" && command.size() >= 2)
        {
            auto& applicationState = commandDispatcher_->applicationState();
            auto source = applicationState.generalizedGLMs().find(command[1]);
            if (source == applicationState.generalizedGLMs().end() ||
                source->second.response.empty()) return;
            const std::string id = StartupId(
                source->second.binaryRegression ? "binarycmp_" : "gglmcmp_",
                source->second.group);
            auto comparison = ::rlispstat::core::CreateGeneralizedComparisonFromSingleModel(
                id, source->second);
            const bool needsFit = comparison.models.empty() ||
                ::rlispstat::core::GeneralizedComparisonResolvedFitState(
                    comparison, comparison.models.front()) !=
                    ::rlispstat::core::RegressionComparisonFitState::Valid;
            applicationState.generalizedComparisons()[id] = std::move(comparison);
            ShowGeneralizedComparison(id);
            if (needsFit) QueueGeneralizedComparison(
                applicationState.generalizedComparisons().at(id));
            return;
        }
        if (name.rfind("GGLM_", 0) == 0 && command.size() >= 2)
        {
            auto found = commandDispatcher_->applicationState().generalizedGLMs().find(command[1]);
            if (found == commandDispatcher_->applicationState().generalizedGLMs().end()) return;
            auto& state = found->second;
            const std::string value = command.size() >= 3 ? command[2] : std::string{};
            auto const* dataframe = commandDispatcher_->applicationState().datasets().find(state.group);
            if (name == "GGLM_PARTIAL_PLOT")
            {
                if (!dataframe || value.empty()) return;
                const auto task = NativePostEstimationTask(state);
                if (std::find(task.terms.begin(), task.terms.end(), value) == task.terms.end()) return;
                const std::string resultId = "gglm_partial_plot:" + state.id + ":" + value;
                std::string residualType = state.diagnosticOptions.residualType.empty()
                    ? "working" : state.diagnosticOptions.residualType;
                for (auto const& [plotId, plot] : plots_)
                    if (plot && (plot->glmModelId == resultId || plot->regressionPartialPendingAnalysisId == resultId) &&
                        plot->regressionDerivedKind == "partial_regression_plot" &&
                        !plot->displayedResidualType.empty())
                        residualType = plot->displayedResidualType;
                const auto dependency = BeginRegressionDerivedOutputRequest(
                    resultId, "plot", "generalized_single", state.id, command);
                auto dataframeCopy = *dataframe;
                const auto dispatcher = dispatcherQueue_;
                std::thread([this, dispatcher, dataframeCopy = std::move(dataframeCopy),
                             task, resultId, term = value, residualType, dependency]() mutable
                {
                    auto post = RunRRegressionPostEstimation(
                        dataframeCopy, "gglm_partial_plot", resultId, term,
                        task.response, task.terms, task.scope,
                        ::rlispstat::core::EncodeStandaloneGeneralizedMISpec(task),
                        residualType);
                    dispatcher.TryEnqueue([this, post = std::move(post), task,
                                           resultId, term, dependency]() mutable
                    {
                        if (!RegressionDerivedOutputRequestIsCurrent(dependency)) return;
                        if (!post.ok)
                        {
                            LogStartup("Generalized-model partial plot failed: " + post.message);
                            return;
                        }
                        std::string message;
                        if (!OpenPooledRegressionPartialPlot(
                                task.group, resultId, term, post.lines, message))
                            LogStartup("Generalized-model partial plot failed: " + message);
                    });
                }).detach();
                return;
            }
            if (name == "GGLM_PAIRWISE" || name == "GGLM_INTERACTION_REPORT" ||
                name == "GGLM_INTERACTION_PLOT")
            {
                if (!dataframe || value.empty()) return;
                const auto task = NativePostEstimationTask(state);
                if (std::find(task.terms.begin(), task.terms.end(), value) == task.terms.end()) return;
                if (name == "GGLM_PAIRWISE")
                {
                    const auto type = task.termTypes.find(value);
                    if (type == task.termTypes.end() ||
                        (type->second != "factor" && type->second != "ordered")) return;
                }
                else if (value.find(':') == std::string::npos &&
                         name != "GGLM_INTERACTION_PLOT") return;
                const std::string mode = name == "GGLM_PAIRWISE" ? "gglm_pairwise" :
                    name == "GGLM_INTERACTION_PLOT" ? "gglm_interaction_plot" :
                    "gglm_interaction";
                const std::string resultId = mode + ":" + state.id + ":" + value;
                const std::string postOptions =
                    InteractionPostEstimationOptions(command);
                const auto dependency = BeginRegressionDerivedOutputRequest(
                    resultId, mode == "gglm_interaction_plot" ? "plot" : "table",
                    "generalized_single", state.id, command);
                auto dataframeCopy = *dataframe;
                const auto dispatcher = dispatcherQueue_;
                std::thread([this, dispatcher, dataframeCopy = std::move(dataframeCopy),
                             task, mode, resultId, term = value, dependency,
                             postOptions]() mutable
                {
                    auto post = RunRRegressionPostEstimation(
                        dataframeCopy, mode, resultId, term, task.response,
                        task.terms, task.scope,
                        ::rlispstat::core::EncodeStandaloneGeneralizedMISpec(task),
                        postOptions);
                    dispatcher.TryEnqueue([this, post = std::move(post), task,
                                           mode, resultId, term, dependency]() mutable
                    {
                        if (!RegressionDerivedOutputRequestIsCurrent(dependency)) return;
                        if (!post.ok)
                        {
                            LogStartup("Generalized-model post-estimation failed: " + post.message);
                            return;
                        }
                        if (mode == "gglm_pairwise")
                        {
                            ::rlispstat::core::GLMPairwiseComparisonResult result;
                            result.term = term;
                            if (::rlispstat::core::ParseGLMPairwiseComparisonsRResult(
                                    post.lines, result))
                                ShowTable1(GLMPairwiseDisplayState(task.group, result, resultId));
                            else LogStartup("Generalized-model pairwise comparison failed: " + result.message);
                        }
                        else if (mode == "gglm_interaction_plot")
                        {
                            std::string message;
                            if (!OpenPooledRegressionInteractionPlot(
                                    task.group, resultId, task.response, term,
                                    post.lines, message))
                                LogStartup("Generalized-model effect plot failed: " + message);
                        }
                        else
                        {
                            ::rlispstat::core::GLMInteractionReport report;
                            if (::rlispstat::core::ParseGLMInteractionReportText(post.lines, report))
                                ShowGLMInteractionReport(resultId, task.group,
                                    "Effect — " + term,
                                    task.countDistribution == "hurdle_beta_binomial_ceiling"
                                        ? "Response-scale effects derived in R from both ZABB components"
                                        : "Estimated marginal means and contrasts from emmeans",
                                    report);
                            else LogStartup("Generalized-model interaction report could not be parsed: " + report.message);
                        }
                    });
                }).detach();
                return;
            }
            if (state.precomputed && !state.multipleImputation) return;
            std::vector<std::string> available;
            if (dataframe)
                for (auto const& column : dataframe->columns)
                    if (!DataColumnAllMissing(column)) available.push_back(column.name);
            for (auto const& variable : state.seed.variables)
                if (std::find(available.begin(), available.end(), variable.name) == available.end())
                    available.push_back(variable.name);
            for (auto const& variable : state.seed.variableMeta)
                if (std::find(available.begin(), available.end(), variable.name) == available.end())
                    available.push_back(variable.name);
            bool forceRefit = false; bool fitChanged = false;
            if (name == "GGLM_SET_RESPONSE" &&
                std::find(available.begin(), available.end(), value) != available.end())
            {
                if (state.binaryRegression)
                {
                    if (!dataframe) return;
                    auto column = std::find_if(dataframe->columns.begin(), dataframe->columns.end(),
                        [&](auto const& candidate) { return candidate.name == value; });
                    if (column == dataframe->columns.end()) return;
                    auto coding = ::rlispstat::core::InspectBinaryResponse(*column);
                    if (!coding.ok) return;
                    state.responseCoding = std::move(coding); state.responseCodingExplicit = false;
                }
                else if (state.countRegression)
                {
                    if (!dataframe) return;
                    auto column = std::find_if(dataframe->columns.begin(), dataframe->columns.end(),
                        [&](auto const& candidate) { return candidate.name == value; });
                    if (column == dataframe->columns.end() ||
                        !::rlispstat::core::InspectCountResponse(*column).ok) return;
                    if (state.exposure == value) state.exposure.clear();
                    if (state.offsetVariable == value) state.offsetVariable.clear();
                }
                const auto edit = ::rlispstat::core::SetModelSpecificationResponse(
                    state, value);
                if (!edit.ok) { LogStartup("Generalized GLM response command failed: " + edit.message); return; }
                fitChanged = edit.changed;
            }
            else if (name == "GGLM_ADD_TERM")
            {
                const auto edit = ::rlispstat::core::AddModelSpecificationTerm(
                    state, available, value);
                if (!edit.ok) { LogStartup("Generalized GLM add-term command failed: " + edit.message); return; }
                fitChanged = edit.changed;
            }
            else if (name == "GGLM_REPLACE_TERM")
            {
                const auto separator = value.find('\x1f');
                if (separator == std::string::npos) return;
                const auto oldTerm = value.substr(0, separator);
                const auto replacement = value.substr(separator + 1);
                const auto edit = ::rlispstat::core::ReplaceModelSpecificationTerm(
                    state, available, oldTerm, replacement);
                if (!edit.ok) { LogStartup("Generalized GLM replace-term command failed: " + edit.message); return; }
                fitChanged = edit.changed;
            }
            else if (name == "GGLM_REMOVE_TERM")
            {
                const auto edit = ::rlispstat::core::RemoveModelSpecificationTerm(
                    state, value);
                if (!edit.ok) { LogStartup("Generalized GLM remove-term command failed: " + edit.message); return; }
                fitChanged = edit.changed;
            }
            else if (name == "GGLM_SET_TYPE")
            {
                const auto separator = value.find('\x1f'); if (separator == std::string::npos) return;
                const auto term = value.substr(0, separator);
                const auto type = value.substr(separator + 1);
                const auto metadata = ::rlispstat::core::PredictorMetadataForModelSpecification(
                    state.seed, dataframe, term);
                const auto edit = ::rlispstat::core::SetModelSpecificationTermType(
                    state, term, type, metadata);
                if (!edit.ok) { LogStartup("Generalized GLM predictor-type command failed: " + edit.message); return; }
                fitChanged = edit.changed;
            }
            else if (name == "GGLM_SET_REFERENCE")
            {
                const auto separator = value.find('\x1f'); if (separator == std::string::npos) return;
                const auto term = value.substr(0, separator);
                const auto reference = value.substr(separator + 1);
                const auto metadata = ::rlispstat::core::PredictorMetadataForModelSpecification(
                    state.seed, dataframe, term);
                const auto edit = ::rlispstat::core::SetModelSpecificationReferenceLevel(
                    state, term, reference, metadata.levels);
                if (!edit.ok) { LogStartup("Generalized GLM reference-level command failed: " + edit.message); return; }
                fitChanged = edit.changed;
            }
            else if (name == "GGLM_TOGGLE_CENTER")
            {
                bool changed = false;
                if (value == "__none__" || value == "__all__")
                {
                    const bool centered = value == "__all__";
                    for (auto const& term : state.terms)
                    {
                        if (::rlispstat::core::IsInteractionTerm(term)) continue;
                        const auto edit = ::rlispstat::core::SetModelSpecificationPredictorCentered(
                            state, term, centered);
                        if (edit.ok) changed = edit.changed || changed;
                    }
                }
                else
                {
                    const auto edit = ::rlispstat::core::SetModelSpecificationPredictorCentered(
                        state, value, state.centeredPredictors.count(value) == 0);
                    if (!edit.ok) { LogStartup("Generalized GLM centering command failed: " + edit.message); return; }
                    changed = edit.changed;
                }
                fitChanged = changed;
            }
            else if (name == "GGLM_SET_FAMILY")
            {
                if (state.binaryRegression || state.countRegression ||
                    !::rlispstat::core::IsValidGeneralizedFamily(value)) return;
                if (state.family != value)
                {
                    state.family = value;
                    state.link = ::rlispstat::core::DefaultGeneralizedLink(value);
                    fitChanged = true;
                }
            }
            else if (name == "GGLM_SET_LINK")
            {
                if (state.countRegression) return;
                if (!::rlispstat::core::IsValidGeneralizedLink(state.family, value)) return;
                if (state.binaryRegression)
                {
                    ::rlispstat::core::BinaryLink parsed;
                    if (!::rlispstat::core::ParseBinaryLink(value, parsed)) return;
                    fitChanged = state.binaryLink != parsed;
                    state.binaryLink = parsed;
                    state.link = ::rlispstat::core::BinaryLinkId(parsed);
                }
                else if (state.link != value) { state.link = value; fitChanged = true; }
            }
            else if (name == "GGLM_SET_RESPONSE_BOUNDS")
            {
                if (!::rlispstat::core::GeneralizedFamilyRequiresResponseBounds(state.family)) return;
                const auto separator = value.find(';');
                if (separator == std::string::npos) return;
                const std::string lowerText = value.substr(0, separator);
                const std::string upperText = value.substr(separator + 1);
                char *lowerEnd = nullptr;
                char *upperEnd = nullptr;
                const double lower = std::strtod(lowerText.c_str(), &lowerEnd);
                const double upper = std::strtod(upperText.c_str(), &upperEnd);
                if (!lowerEnd || !upperEnd || lowerEnd == lowerText.c_str() ||
                    upperEnd == upperText.c_str() || *lowerEnd != '\0' || *upperEnd != '\0' ||
                    !std::isfinite(lower) || !std::isfinite(upper) || lower >= upper)
                {
                    state.responseBoundsConfigured = false;
                    state.ok = false;
                    state.status = "Enter finite response bounds with lower < upper.";
                    ShowGeneralizedModel(id);
                    return;
                }
                fitChanged = !state.responseBoundsConfigured || state.responseLower != lower ||
                    state.responseUpper != upper;
                state.responseBoundsConfigured = true;
                state.responseLower = lower;
                state.responseUpper = upper;
            }
            else if (name == "GGLM_SET_COUNT_DISTRIBUTION")
            {
                if (!state.countRegression) return;
                ::rlispstat::core::CountDistribution distribution;
                if (!::rlispstat::core::ParseCountDistribution(value, distribution)) return;
                if (state.countDistribution != distribution)
                {
                    state.countDistribution = distribution;
                    const bool bounded =
                        ::rlispstat::core::CountDistributionUsesTrials(distribution);
                    state.family = distribution == ::rlispstat::core::CountDistribution::QuasiPoisson
                        ? "quasipoisson" : bounded ? "binomial" : "poisson";
                    state.link = bounded ? "logit" : "log";
                    if (bounded) state.exposure.clear();
                    if (distribution == ::rlispstat::core::CountDistribution::HurdleBetaBinomialCeiling ||
                        distribution == ::rlispstat::core::CountDistribution::PerfectScore)
                        state.offsetVariable.clear();
                    fitChanged = true;
                }
            }
            else if (name == "GGLM_SET_TRIALS")
            {
                if (!state.countRegression ||
                    !::rlispstat::core::CountDistributionUsesTrials(state.countDistribution)) return;
                char *end = nullptr;
                const double trials = std::strtod(value.c_str(), &end);
                if (!end || end == value.c_str() || *end != '\0' || !std::isfinite(trials) ||
                    trials <= 0.0 || std::floor(trials) != trials)
                {
                    state.trialsVariable.clear();
                    state.trialsConstant = NAN;
                    state.ok = false;
                    state.status = "Trials must be a fixed positive integer.";
                    ShowGeneralizedModel(id);
                    return;
                }
                if (!state.trialsVariable.empty() || state.trialsConstant != trials)
                {
                    state.trialsVariable.clear();
                    state.trialsConstant = trials;
                    fitChanged = true;
                }
            }
            else if (name == "GGLM_SET_EXPOSURE")
            {
                if (!state.countRegression || !dataframe) return;
                const std::string exposure = value == "__none__" ? std::string{} : value;
                if (!exposure.empty())
                {
                    if (exposure == state.response || exposure == state.offsetVariable) return;
                    auto column = std::find_if(dataframe->columns.begin(), dataframe->columns.end(),
                        [&](auto const& candidate) { return candidate.name == exposure; });
                    if (column == dataframe->columns.end() ||
                        !::rlispstat::core::InspectCountExposure(*column).ok) return;
                }
                if (state.exposure != exposure)
                {
                    state.exposure = exposure;
                    fitChanged = true;
                }
            }
            else if (name == "GGLM_SET_OFFSET")
            {
                if (!dataframe) return;
                const std::string offset = value == "__none__" ? std::string{} : value;
                if (!offset.empty())
                {
                    const auto numeric = ::rlispstat::core::NumericVariableNames(&state.seed);
                    if (offset == state.response || offset == state.exposure ||
                        std::find(numeric.begin(), numeric.end(), offset) == numeric.end()) return;
                }
                if (state.offsetVariable != offset)
                {
                    state.offsetVariable = offset;
                    fitChanged = true;
                }
            }
            else if (name == "GGLM_SET_EVENT")
            {
                if (!state.binaryRegression || !state.responseCoding.ok) return;
                if (value == state.responseCoding.referenceValue)
                {
                    std::swap(state.responseCoding.eventValue, state.responseCoding.referenceValue);
                    std::swap(state.responseCoding.eventLabel, state.responseCoding.referenceLabel);
                    std::swap(state.responseCoding.eventCount, state.responseCoding.referenceCount);
                    fitChanged = true;
                }
                else if (value != state.responseCoding.eventValue) return;
                state.responseCodingExplicit = true;
            }
            else if (name == "GGLM_SET_RESIDUAL")
            {
                if (!::rlispstat::core::IsGeneralizedResidualTypeAvailable(state, value)) return;
                if (state.diagnosticOptions.residualType != value)
                {
                    state.diagnosticOptions.residualType = value;
                    ++state.diagnosticOptions.version;
                }
                fitChanged = false;
            }
            else if (name == "GGLM_TOGGLE_AUTO")
            {
                state.autoRefit = !state.autoRefit;
                // Match macOS: switching auto-refit back on immediately brings
                // an out-of-date model up to date.
                forceRefit = state.autoRefit &&
                    (!state.ok || state.fitVersion < state.modelVersion);
                fitChanged = false;
            }
            else if (name == "GGLM_SET_SCOPE")
            {
                if (::rlispstat::core::SavedAnalysisScopeNameFromChoiceValue(value))
                {
                    ::rlispstat::core::AnalysisScope savedScope;
                    std::string error;
                    if (!commandDispatcher_->applicationState().resolveSavedAnalysisScopeChoice(
                            state.group, value, savedScope, &error)) return;
                    state.scope = "selected";
                    state.dataScope = std::move(savedScope);
                    state.dataScopeCaptured = true;
                    fitChanged = true;
                }
                else
                {
                    if (value != "all" && value != "selected" && value != "unselected") return;
                    if (state.scope != value || state.dataScopeCaptured)
                    {
                        state.scope = value; state.dataScope = {};
                        state.dataScopeCaptured = false; fitChanged = true;
                    }
                }
            }
            else if (name == "GGLM_REFIT") forceRefit = true;
            else return;
            if (!fitChanged && !forceRefit)
            {
                ShowGeneralizedModel(state.id);
                return;
            }
            if (fitChanged || forceRefit) ++state.modelVersion;
            state.rFitPending = false;
            if (forceRefit || (fitChanged && state.autoRefit)) RequestGeneralizedModelFit(state.id);
            else {
                if (fitChanged && !state.autoRefit)
                    state.status = ::rlispstat::core::GeneralizedGLMPendingManualFitStatus();
                ShowGeneralizedModel(state.id);
            }
            return;
        }
        if (name == "MODEL_OPEN_PARTIAL_PLOT" && command.size() >= 3)
        {
            auto& applicationState = commandDispatcher_->applicationState();
            auto model = applicationState.groupModels().find(command[1]);
            if (model == applicationState.groupModels().end()) return;
            auto const* dataframe = applicationState.datasets().find(model->second.group);
            const std::string& term = command[2];
            if (!dataframe || term.empty() ||
                std::find(model->second.terms.begin(), model->second.terms.end(), term) ==
                    model->second.terms.end()) return;
            const std::string resultId =
                "glm_partial_plot:" + command[1] + ":" + term;
            std::string residualType = "raw";
            for (auto const& [plotId, plot] : plots_)
                if (plot && (plot->glmModelId == resultId || plot->regressionPartialPendingAnalysisId == resultId) &&
                    plot->regressionDerivedKind == "partial_regression_plot" &&
                    !plot->displayedResidualType.empty())
                    residualType = plot->displayedResidualType;
            const auto dependency = BeginRegressionDerivedOutputRequest(
                resultId, "plot", "linear_single", command[1], command);
            auto dataframeCopy = *dataframe;
            auto modelCopy = model->second;
            const auto dispatcher = dispatcherQueue_;
            std::thread([this, dispatcher, dataframeCopy = std::move(dataframeCopy),
                         modelCopy = std::move(modelCopy), resultId, term,
                         residualType, dependency]() mutable
            {
                auto post = RunRRegressionPostEstimation(
                    dataframeCopy, "glm_partial_plot", resultId, term,
                    modelCopy.response, modelCopy.terms,
                    modelCopy.scope.empty() ? "all" : modelCopy.scope,
                    ::rlispstat::core::EncodeStandaloneLinearMISpec(
                        ::rlispstat::core::EffectiveModelSpecificationTermTypes(modelCopy),
                        modelCopy.centeredPredictors, modelCopy.factorReferenceLevels,
                        NativePostEstimationRows(modelCopy.dataScope,
                                                  modelCopy.dataScopeCaptured)),
                    residualType);
                dispatcher.TryEnqueue([this, post = std::move(post), resultId,
                                       term, group = modelCopy.group, dependency]() mutable
                {
                    if (!RegressionDerivedOutputRequestIsCurrent(dependency)) return;
                    if (!post.ok)
                    {
                        LogStartup("Linear-model partial plot failed: " + post.message);
                        return;
                    }
                    std::string message;
                    if (!OpenPooledRegressionPartialPlot(
                            group, resultId, term, post.lines, message))
                        LogStartup("Linear-model partial plot failed: " + message);
                });
            }).detach();
            return;
        }
        if ((name == "MODEL_INTERACTION_REPORT" ||
             name == "MODEL_OPEN_INTERACTION_PLOT") && command.size() >= 3)
        {
            auto& applicationState = commandDispatcher_->applicationState();
            auto model = applicationState.groupModels().find(command[1]);
            if (model == applicationState.groupModels().end()) return;
            auto const* dataframe = applicationState.datasets().find(model->second.group);
            if (!dataframe) return;
            const std::string& term = command[2];
            const std::string reportId =
                "glm_interaction_report:" + command[1] + ":" + term;
            const std::string mode = name == "MODEL_OPEN_INTERACTION_PLOT"
                ? "glm_interaction_plot" : "glm_interaction";
            const std::string postOptions =
                InteractionPostEstimationOptions(command);
            const auto dependency = BeginRegressionDerivedOutputRequest(
                reportId, mode == "glm_interaction_plot" ? "plot" : "table",
                "linear_single", command[1], command);
            auto dataframeCopy = *dataframe;
            auto modelCopy = model->second;
            const auto dispatcher = dispatcherQueue_;
            std::thread([this, dispatcher, dataframeCopy = std::move(dataframeCopy),
                         modelCopy = std::move(modelCopy), mode, reportId, term,
                         dependency, postOptions]() mutable
            {
                auto post = RunRRegressionPostEstimation(
                    dataframeCopy, mode, reportId, term, modelCopy.response,
                    modelCopy.terms, modelCopy.scope.empty() ? "all" : modelCopy.scope,
                    ::rlispstat::core::EncodeStandaloneLinearMISpec(
                        ::rlispstat::core::EffectiveModelSpecificationTermTypes(modelCopy),
                        modelCopy.centeredPredictors, modelCopy.factorReferenceLevels,
                        NativePostEstimationRows(modelCopy.dataScope,
                                                  modelCopy.dataScopeCaptured)),
                    postOptions);
                dispatcher.TryEnqueue([this, post = std::move(post), mode, reportId,
                                       term, group = modelCopy.group,
                                       response = modelCopy.response, dependency]() mutable
                {
                    if (!RegressionDerivedOutputRequestIsCurrent(dependency)) return;
                    if (!post.ok)
                    {
                        LogStartup("Linear-model interaction failed: " + post.message);
                        return;
                    }
                    if (mode == "glm_interaction_plot")
                    {
                        std::string message;
                        if (!OpenPooledRegressionInteractionPlot(
                                group, reportId, response, term, post.lines, message))
                            LogStartup("Linear-model effect plot failed: " + message);
                    }
                    else
                    {
                        ::rlispstat::core::GLMInteractionReport report;
                        if (::rlispstat::core::ParseGLMInteractionReportText(post.lines, report))
                            ShowGLMInteractionReport(reportId, group,
                                "Effect — " + term,
                                "Estimated marginal means and contrasts from emmeans", report);
                        else LogStartup("Linear-model interaction report could not be parsed: " + report.message);
                    }
                });
            }).detach();
            return;
        }
        const auto reply = commandDispatcher_->dispatch(command);
        if (command[0] == "BARPLOT_SEGMENT_DETAILS" && reply.rfind("OK\t", 0) == 0 &&
            command.size() > 1)
        {
            auto found = views_.find(command[1]);
            if (found != views_.end() && found->second)
                found->second->ShowInformation("Bar segment", reply.substr(3));
        }
        if (reply.rfind("ERR ", 0) == 0)
        {
            if (command[0] == "MI_ADD_VARIABLE" || command[0] == "SHOW_MISSING_INFORMATION" || command[0] == "SHOW_R_CODE" ||
                command[0] == "SHOW_R_PUBLICATION_CODE")
            {
                ShowWorkflowMessage(
                    technicalOwner_,
                    command[0] == "SHOW_R_PUBLICATION_CODE"
                        ? L"R Publication Code" : (command[0] == "SHOW_MISSING_INFORMATION"
                            ? L"Missing-information diagnostics" : (command[0] == "MI_ADD_VARIABLE"
                                ? L"Add variable" : L"R Code")),
                    reply.substr(4));
            }
            else LogStartup("Analysis command failed: " + reply);
        }
    }

    bool App::StartAnalysis(std::string const& group, AnalysisWorkflowKind kind)
    {
        if (!commandDispatcher_) return false;
        auto& applicationState = commandDispatcher_->applicationState();
        auto const* dataframe = applicationState.datasets().find(group);
        if (!dataframe) return false;

        ::rlispstat::core::PlotModel seed;
        ::rlispstat::core::PopulateDatasetSeedPlot(seed, *dataframe, group);
        const auto numeric = ::rlispstat::core::NumericVariableNames(&seed);
        const auto names = VariableNames(*dataframe);
        const auto scope = applicationState.activeAnalysisScope(group);

        // Dataset default roles remain persisted for compatibility, but the
        // experimental feature gate prevents them from affecting fresh
        // analyses while role/type compatibility is being redesigned.
        const auto variableRoles =
            ::rlispstat::core::DefaultVariableRolesForAnalysisInitialization(
                applicationState.variableRoles(group));
        const auto freshRegressionRoles =
            ::rlispstat::core::FreshRegressionAnalysisVariableRoles(variableRoles);
        const auto initialFor = [&](::rlispstat::core::SharedAnalysisKind sharedKind)
        {
            return ::rlispstat::core::ResolveInitialAnalysisSpecification(
                *dataframe, ::rlispstat::core::SharedAnalysisDefinition(sharedKind),
                variableRoles);
        };

        if (kind == AnalysisWorkflowKind::Table1)
        {
            const auto initial = initialFor(
                ::rlispstat::core::SharedAnalysisKind::DescriptiveTable);
            const auto id = StartupId("table1_", group);
            ShowTable1(::rlispstat::core::Table1PendingStateForDataFrame(
                *dataframe, id,
                ::rlispstat::core::InitialAnalysisVariables(initial, "variables"),
                ::rlispstat::core::InitialAnalysisVariable(initial, "group"), {}, &scope));
            return true;
        }
        if (kind == AnalysisWorkflowKind::CorrelationMatrix)
        {
            const auto initial = initialFor(
                ::rlispstat::core::SharedAnalysisKind::CorrelationMatrix);
            ::rlispstat::core::CorrelationMatrixState state;
            state.id = StartupId("corr_", group); state.group = group;
            state.title = "Pearson Correlation Matrix"; state.method = "pearson";
            state.missingMode = "pairwise"; state.showP = true;
            state.showPValue = false; state.showN = false;
            state.variables = ::rlispstat::core::InitialAnalysisVariables(
                initial, "variables");
            state.seed = seed; state.hasSeed = true;
            state.dataScope = scope; state.dataScopeCaptured = true;
            if (initial.minimumValid) QueueCorrelationFit(state);
            ++state.modelVersion;
            const auto id = state.id;
            applicationState.correlationMatrices()[id] = std::move(state);
            ShowCorrelationMatrix(id);
            return true;
        }
        if (kind == AnalysisWorkflowKind::ScaleAnalysis)
        {
            ::rlispstat::core::ScaleAnalysisState state;
            state.id = StartupId("scale_", group);
            state.group = group;
            state.specification.datasetId = group;
            state.specification.items = ::rlispstat::core::InitialScaleItems(
                *dataframe, variableRoles);
            state.specification.revision = 1;
            std::string validationError;
            if (!::rlispstat::core::ValidateScaleAnalysisSpecification(
                    *dataframe, state.specification, validationError))
                return false;
            state.seed = seed;
            state.hasSeed = true;
            state.dataScope = scope;
            state.dataScopeCaptured = true;
            state.result.backend = dataframe->datasetType == "multiple_imputation"
                ? "multiple_imputation" : "ordinary";
            state.result.imputationCount = state.result.backend == "multiple_imputation"
                ? std::max(1, dataframe->imputationCount) : 1;
            state.status = state.specification.items.size() < 2
                ? "Choose at least two numeric or ordinal items."
                : "Fitting Scale Analysis in R...";
            const auto id = state.id;
            applicationState.scaleAnalyses()[id] = std::move(state);
            ShowScaleAnalysis(id);
            if (applicationState.scaleAnalyses()[id].specification.items.size() >= 2)
                return RefitScaleAnalysis(id);
            return true;
        }
        if (kind == AnalysisWorkflowKind::Dimensionality ||
            kind == AnalysisWorkflowKind::FactorAnalysis)
        {
            const auto initial = initialFor(
                ::rlispstat::core::SharedAnalysisKind::Dimensionality);
            ::rlispstat::core::DimensionalityState state;
            state.id = StartupId("dim_", group); state.group = group;
            state.method = kind == AnalysisWorkflowKind::FactorAnalysis
                ? "factor" : "pca";
            state.missingMode = "listwise"; state.scale = true;
            state.variables = ::rlispstat::core::InitialAnalysisVariables(
                initial, "variables");
            state.componentCount = std::min<int>(2, static_cast<int>(state.variables.size()));
            state.seed = seed; state.hasSeed = true; state.dataScope = scope;
            state.dataScopeCaptured = true; state.scope = ScopeName(scope);
            state.eligibleVariables = ::rlispstat::core::EligibleDimensionalityVariables(*dataframe);
            state.multipleImputation = dataframe->datasetType == "multiple_imputation" ||
                dataframe->imputationCount > 1;
            state.imputationCount = state.multipleImputation
                ? std::max(1, dataframe->imputationCount) : 1;
            state.displayedImputation = state.multipleImputation
                ? std::max(1, std::min(state.imputationCount,
                                      dataframe->activeImputationVersion)) : 1;
            const auto id = state.id;
            applicationState.dimensionalityModels()[id] = std::move(state);
            if (initial.minimumValid) return RefitDimensionality(id);
            ShowDimensionality(id);
            return true;
        }
        if (kind == AnalysisWorkflowKind::QuickCluster)
        {
            const auto initial = initialFor(
                ::rlispstat::core::SharedAnalysisKind::QuickCluster);
            ::rlispstat::core::DendrogramState state;
            state.id = StartupId("dendro_", group); state.group = group;
            state.variables = ::rlispstat::core::InitialAnalysisVariables(
                initial, "variables");
            state.distance = "euclidean"; state.linkage = "average";
            state.missingMode = "pairwise"; state.seed = seed; state.hasSeed = true;
            state.dataScope = scope; state.dataScopeCaptured = true;
            const auto id = state.id;
            applicationState.dendrograms()[id] = std::move(state);
            if (initial.minimumValid) return RefitDendrogram(id);
            ShowDendrogram(id);
            return true;
        }
        if (kind == AnalysisWorkflowKind::OneSampleT ||
            kind == AnalysisWorkflowKind::IndependentT ||
            kind == AnalysisWorkflowKind::PairedT ||
            kind == AnalysisWorkflowKind::OneWayAnova)
        {
            const bool independent = kind == AnalysisWorkflowKind::IndependentT;
            const bool paired = kind == AnalysisWorkflowKind::PairedT;
            const auto sharedKind = kind == AnalysisWorkflowKind::OneSampleT
                ? ::rlispstat::core::SharedAnalysisKind::OneSampleComparison
                : independent
                    ? ::rlispstat::core::SharedAnalysisKind::IndependentSamplesComparison
                    : paired
                        ? ::rlispstat::core::SharedAnalysisKind::PairedSamplesComparison
                        : ::rlispstat::core::SharedAnalysisKind::OneWayComparison;
            const auto initial = initialFor(sharedKind);
            const auto initialResponses = ::rlispstat::core::InitialAnalysisVariables(
                initial, "responses");
            const auto initialFirst = ::rlispstat::core::InitialAnalysisVariable(
                initial, "first");
            const auto initialSecond = ::rlispstat::core::InitialAnalysisVariable(
                initial, "second");
            const auto initialGroup = ::rlispstat::core::InitialAnalysisVariable(
                initial, "group");
            const std::string testType = kind == AnalysisWorkflowKind::OneSampleT ? "one_sample_t" :
                independent ? "independent_t" : paired ? "paired_t" : "oneway_anova";

            ::rlispstat::core::MeanComparisonState state;
            state.id = StartupId("cm_batch_", group); state.datasetId = group;
            state.title = ::rlispstat::core::CompareMeansTitle(testType);
            state.confidenceLevel = 0.95; state.alternative = "two.sided";
            state.dataScope = scope; state.dataScopeCaptured = true;
            state.specification.confidenceLevel = 0.95;
            state.specification.pAdjustment = ::rlispstat::core::MultipleTestingAdjustment::Holm;
            if (kind == AnalysisWorkflowKind::OneSampleT)
            {
                state.analysisType = "one_sample_t_test";
                state.specification.kind = ::rlispstat::core::MeanComparisonKind::OneSample;
                state.specification.method = ::rlispstat::core::MeanComparisonMethod::OneSampleT;
                state.specification.dependentVariableIds = initialResponses;
                const auto firstResponse = initialResponses.empty()
                    ? std::string{} : initialResponses.front();
                auto firstColumn = firstResponse.empty() ? dataframe->columns.end() :
                    std::find_if(dataframe->columns.begin(), dataframe->columns.end(),
                        [&](auto const& candidate) { return candidate.name == firstResponse; });
                const double initialTestValue = firstColumn != dataframe->columns.end() &&
                    MeanComparisonBinaryCategorical(*firstColumn) ? 0.5 : 0.0;
                state.specification.testValue = initialTestValue;
                state.testValue = initialTestValue;
                for (auto const& response : initialResponses)
                    state.specification.testValues[response] = initialTestValue;
            }
            else if (paired)
            {
                state.analysisType = "paired_samples_t_test";
                state.specification.kind = ::rlispstat::core::MeanComparisonKind::PairedSamples;
                state.specification.method = ::rlispstat::core::MeanComparisonMethod::OneSampleT;
                if (!initialFirst.empty() && !initialSecond.empty() &&
                    MeanComparisonPairedVariablesCompatible(
                        *dataframe, initialFirst, initialSecond))
                    state.specification.pairs.push_back({
                        "pair:" + initialFirst + "\x1f" + initialSecond,
                        initialFirst, initialSecond});
                state.title = "Paired-Samples Tests";
            }
            else
            {
                state.analysisType = independent ? "independent_samples_t_test" : "one_way_anova";
                state.specification.kind = independent
                    ? ::rlispstat::core::MeanComparisonKind::IndependentSamples
                    : ::rlispstat::core::MeanComparisonKind::OneWayAnova;
                state.specification.method = independent
                    ? ::rlispstat::core::MeanComparisonMethod::WelchT
                    : ::rlispstat::core::MeanComparisonMethod::WelchAnova;
                state.specification.dependentVariableIds = initialResponses;
                if (!initialGroup.empty())
                {
                    auto column = std::find_if(dataframe->columns.begin(),
                        dataframe->columns.end(), [&](auto const& candidate)
                        { return candidate.name == initialGroup; });
                    if (column != dataframe->columns.end() &&
                        MeanComparisonGroupingCandidate(*column, independent))
                    {
                        state.specification.groupingVariableId = initialGroup;
                        const auto levels = MeanComparisonLevels(*column);
                        state.specification.groupOrderIds = independent
                            ? ::rlispstat::core::MeanComparisonDefaultIndependentGroupOrder(levels)
                            : levels;
                        state.groupVariable = initialGroup;
                    }
                }
                state.method = independent ? "Welch" : "Welch one-way ANOVA";
                if (independent) state.title = "Two-Sample Tests";
            }
            state.subtitle = MeanComparisonSelectionPrompt(state);
            ShowMeanComparison(state);
            if (MeanComparisonReadyForRefit(state)) RefitMeanComparison(state.id);
            return true;
        }
        if (kind == AnalysisWorkflowKind::LinearModel)
        {
            if (numeric.empty()) return false;
            auto& state = ::rlispstat::core::EnsureGroupModelState(
                applicationState.groupModels(), group, nullptr);
            if (state.response.empty() && state.terms.empty())
            {
                const auto initial = ::rlispstat::core::ResolveInitialAnalysisSpecification(
                    *dataframe,
                    ::rlispstat::core::SharedAnalysisDefinition(
                        ::rlispstat::core::SharedAnalysisKind::LinearModel),
                    variableRoles);
                state.response = ::rlispstat::core::InitialAnalysisVariable(
                    initial, "response");
                state.terms = ::rlispstat::core::InitialAnalysisVariables(
                    initial, "predictors");
            }
            state.dataScope = scope; state.dataScopeCaptured = true;
            state.scope = ScopeName(scope);
            ::rlispstat::core::SynchronizeGroupModelTermTypes(state, *dataframe);
            ::rlispstat::core::MarkGroupModelChanged(state);
            QueueLinearModelFit(group);
            return true;
        }
        if (kind == AnalysisWorkflowKind::GeneralizedLinearModel ||
            kind == AnalysisWorkflowKind::CountRegression ||
            kind == AnalysisWorkflowKind::BinaryRegression ||
            kind == AnalysisWorkflowKind::PositiveContinuousModel ||
            kind == AnalysisWorkflowKind::ProportionModel)
        {
            if (names.empty()) return false;
            const bool binary = kind == AnalysisWorkflowKind::BinaryRegression;
            const bool count = kind == AnalysisWorkflowKind::CountRegression;
            const bool positive = kind == AnalysisWorkflowKind::PositiveContinuousModel;
            const bool proportion = kind == AnalysisWorkflowKind::ProportionModel;
            const auto sharedKind = binary
                ? ::rlispstat::core::SharedAnalysisKind::BinaryRegression
                : count ? ::rlispstat::core::SharedAnalysisKind::CountRegression
                : positive ? ::rlispstat::core::SharedAnalysisKind::PositiveContinuousModel
                : proportion ? ::rlispstat::core::SharedAnalysisKind::ProportionModel
                        : ::rlispstat::core::SharedAnalysisKind::GeneralizedLinearModel;
            auto state = ::rlispstat::core::CreateIndependentGeneralizedGLMSession(
                StartupId(binary ? "binary_" : count ? "count_" :
                    positive ? "positive_" : proportion ? "proportion_" : "gglm_", group),
                group, seed, *dataframe, sharedKind, freshRegressionRoles,
                scope, ScopeName(scope));
            const auto id = state.id;
            applicationState.generalizedGLMs()[id] = std::move(state);
            ShowGeneralizedModel(id);
            if (!applicationState.generalizedGLMs()[id].response.empty() &&
                (binary || !applicationState.generalizedGLMs()[id].terms.empty()))
                RequestGeneralizedModelFit(id);
            return true;
        }
        if (kind == AnalysisWorkflowKind::RegressionComparison)
        {
            const auto initial = ::rlispstat::core::ResolveInitialAnalysisSpecification(
                *dataframe,
                ::rlispstat::core::SharedAnalysisDefinition(
                    ::rlispstat::core::SharedAnalysisKind::RegressionComparison),
                freshRegressionRoles);
            const auto initialResponse = ::rlispstat::core::InitialAnalysisVariable(
                initial, "response");
            const auto initialPredictors = ::rlispstat::core::InitialAnalysisVariables(
                initial, "predictors");
            ::rlispstat::core::RegressionComparisonState state;
            state.id = StartupId("regcmp_", group); state.group = group;
            state.response = initialResponse;
            state.scope = ScopeName(scope); state.autoRefit = true;
            state.seed = seed; state.hasSeed = true; state.dataScope = scope;
            state.dataScopeCaptured = true; state.termRows.push_back("(Intercept)");
            state.multipleImputation = dataframe->datasetType == "multiple_imputation";
            state.imputationCount = state.multipleImputation ? dataframe->imputationCount : 0;
            if (state.multipleImputation)
            {
                state.title = "Compare Linear Models \xE2\x80\x94 Multiple Imputation";
                state.note = "Coefficients and model tests are pooled across imputations using Rubin-compatible inference.";
            }
            ::rlispstat::core::RegressionComparisonModel model;
            model.id = state.id + ":model:1"; model.label = "Model 1";
            model.response = state.response;
            for (auto const& term : initialPredictors)
            {
                if (!state.response.empty() &&
                    ::rlispstat::core::ModelTermShouldBeRemoved(term, state.response)) continue;
                model.terms.push_back(term);
                ::rlispstat::core::AddRegressionTermRow(state, term);
            }
            state.models.push_back(std::move(model));
            ::rlispstat::core::SynchronizeRegressionComparisonTermTypes(state, *dataframe);
            const auto id = state.id;
            applicationState.regressionComparisons()[id] = std::move(state);
            if (!applicationState.regressionComparisons()[id].response.empty())
                QueueRegressionComparison(applicationState.regressionComparisons()[id]);
            ShowRegressionComparison(id);
            return true;
        }
        if (kind == AnalysisWorkflowKind::LinearMixedModel ||
            kind == AnalysisWorkflowKind::GeneralizedMixedModel)
        {
            const bool generalized = kind == AnalysisWorkflowKind::GeneralizedMixedModel;
            const auto initial = initialFor(generalized
                ? ::rlispstat::core::SharedAnalysisKind::GeneralizedMixedModel
                : ::rlispstat::core::SharedAnalysisKind::LinearMixedModel);
            ::rlispstat::core::NativeMixedModelState state;
            state.id = StartupId(generalized ? "glmm_" : "lmm_", group);
            state.group = group;
            state.modelType = generalized
                ? "generalized_linear_mixed_model" : "linear_mixed_model";
            state.method = generalized ? "ML" : "REML";
            state.family = "binomial"; state.link = "logit";
            state.response = ::rlispstat::core::InitialAnalysisVariable(initial, "response");
            state.fixedEffects = ::rlispstat::core::InitialAnalysisVariables(initial, "fixed_effects");
            const auto randomGroup = ::rlispstat::core::InitialAnalysisVariable(initial, "group");
            if (!randomGroup.empty())
            {
                ::rlispstat::core::NativeMixedRandomSpec random;
                random.group = randomGroup; random.terms = { "1" };
                state.randomEffects.push_back(std::move(random));
            }
            state.dataScope = scope;
            state.dataScopeCaptured = true;
            const auto id = state.id;
            applicationState.nativeMixedModels()[id] = state;
            if (!state.response.empty() && !state.randomEffects.empty())
            {
                pendingMainRTasks_.mixedModelTasks.push_back({state});
                MarkMainRTaskPending();
            }
            else
            {
                auto& stored = applicationState.nativeMixedModels()[id];
                stored.reportText = generalized
                    ? "Generalized Linear Mixed Model\n" : "Linear Mixed Model\n";
                stored.reportText += "Dataset: " + group + "\n";
                stored.reportText += "Response: " + stored.response + "\n";
                stored.reportText += "Scope: " + ScopeName(scope) + "\nWarnings\n";
                stored.reportText += stored.response.empty()
                    ? "Choose a response variable from the context menu.\n"
                    : "Choose a grouping variable for the random effect from the context menu.\n";
                ShowMixedModel(stored);
            }
            return true;
        }
        if (kind == AnalysisWorkflowKind::GeneralizedComparison ||
            kind == AnalysisWorkflowKind::CountRegressionComparison ||
            kind == AnalysisWorkflowKind::BinaryRegressionComparison ||
            kind == AnalysisWorkflowKind::PositiveContinuousComparison ||
            kind == AnalysisWorkflowKind::ProportionComparison)
        {
            const bool binary = kind == AnalysisWorkflowKind::BinaryRegressionComparison;
            const bool count = kind == AnalysisWorkflowKind::CountRegressionComparison;
            const bool positive =
                kind == AnalysisWorkflowKind::PositiveContinuousComparison;
            const bool proportion = kind == AnalysisWorkflowKind::ProportionComparison;
            const auto modelType = binary ? ::rlispstat::core::StatisticalModelType::Binary
                : count ? ::rlispstat::core::StatisticalModelType::Count
                : positive ? ::rlispstat::core::StatisticalModelType::PositiveContinuous
                : proportion ? ::rlispstat::core::StatisticalModelType::Proportion
                             : ::rlispstat::core::StatisticalModelType::LegacyGeneralized;
            const auto regressionKind = binary
                ? ::rlispstat::core::SharedAnalysisKind::BinaryRegression
                : count ? ::rlispstat::core::SharedAnalysisKind::CountRegression
                : positive ? ::rlispstat::core::SharedAnalysisKind::PositiveContinuousModel
                : proportion ? ::rlispstat::core::SharedAnalysisKind::ProportionModel
                        : ::rlispstat::core::SharedAnalysisKind::GeneralizedComparison;
            const auto initial = ::rlispstat::core::ResolveInitialAnalysisSpecification(
                *dataframe, ::rlispstat::core::SharedAnalysisDefinition(regressionKind),
                freshRegressionRoles);
            const auto initialResponse = ::rlispstat::core::InitialAnalysisVariable(
                initial, "response");
            const auto initialPredictors = ::rlispstat::core::InitialAnalysisVariables(
                initial, "predictors");
            ::rlispstat::core::GeneralizedComparisonState state;
            state.id = StartupId(binary ? "binarycmp_" : count ? "countcmp_"
                : positive ? "positivecmp_" : proportion ? "proportioncmp_"
                                                       : "gglmcmp_", group);
            state.group = group; state.seed = seed; state.hasSeed = true;
            state.modelType = modelType;
            state.dataScope = scope; state.dataScopeCaptured = true; state.scope = ScopeName(scope);
            state.binaryComparison = binary; state.countComparison = count; state.autoRefit = true;
            ::rlispstat::core::SynchronizeGeneralizedComparisonDatasetIdentity(state, *dataframe);
            if (binary)
            {
                state.family = "binomial"; state.binaryLink = ::rlispstat::core::BinaryLink::Logit;
                state.link = ::rlispstat::core::BinaryLinkId(state.binaryLink);
                auto column = std::find_if(dataframe->columns.begin(), dataframe->columns.end(),
                    [&](auto const& candidate) { return candidate.name == initialResponse; });
                if (column != dataframe->columns.end())
                {
                    auto coding = ::rlispstat::core::InspectBinaryResponse(*column);
                    if (coding.ok) { state.response = column->name; state.responseCoding = std::move(coding); }
                }
            }
            else if (count)
            {
                state.family = "poisson";
                state.link = "log";
                auto column = std::find_if(dataframe->columns.begin(), dataframe->columns.end(),
                    [&](auto const& candidate) { return candidate.name == initialResponse; });
                if (column != dataframe->columns.end() &&
                    ::rlispstat::core::InspectCountResponse(*column).ok)
                    state.response = column->name;
            }
            else
            {
                state.response = initialResponse;
                if (modelType == ::rlispstat::core::StatisticalModelType::LegacyGeneralized)
                {
                    state.family = ::rlispstat::core::DefaultGeneralizedFamilyForResponse(
                        seed, state.response);
                    state.link = ::rlispstat::core::DefaultGeneralizedLink(state.family);
                }
                else
                {
                    const auto distributions =
                        ::rlispstat::core::DistributionSpecificationsForModel(modelType);
                    state.family = distributions.empty() ? std::string{} : distributions.front().id;
                    state.link = ::rlispstat::core::DefaultLinkForModelDistribution(
                        modelType, state.family);
                    auto column = std::find_if(dataframe->columns.begin(), dataframe->columns.end(),
                        [&](auto const& candidate) { return candidate.name == state.response; });
                    const auto *distribution =
                        ::rlispstat::core::FindDistributionSpecification(state.family);
                    if (column == dataframe->columns.end() || !distribution ||
                        !::rlispstat::core::InspectResponseDomain(
                            *column, distribution->responseDomain).ok)
                        state.response.clear();
                }
            }
            ::rlispstat::core::GeneralizedComparisonModel first;
            first.id = state.id + ":model:1"; first.label = "Model 1";
            first.modelVersion = 1;
            first.response = state.response; first.family = state.family; first.link = state.link;
            first.scope = state.scope; first.dataScope = scope; first.dataScopeCaptured = true;
            if (proportion) { first.responseBoundsConfigured = true; first.responseLower = 0.0; first.responseUpper = 1.0; }
            if (count) first.countDistribution = ::rlispstat::core::CountDistribution::Poisson;
            for (auto const& term : initialPredictors)
            {
                if (!state.response.empty() &&
                    ::rlispstat::core::ModelTermShouldBeRemoved(term, state.response)) continue;
                first.terms.push_back(term);
                ::rlispstat::core::AddGeneralizedComparisonTermRow(state, term);
            }
            state.models.push_back(std::move(first)); state.activeModel = 0;
            ::rlispstat::core::RefreshGeneralizedTermRowsFromFits(state);
            const auto id = state.id;
            applicationState.generalizedComparisons()[id] = std::move(state);
            ShowGeneralizedComparison(id);
            auto& queuedState = applicationState.generalizedComparisons()[id];
            if (!queuedState.response.empty())
                QueueGeneralizedComparison(queuedState);
            return true;
        }
        return false;
    }

    void App::OpenDescriptiveStatisticsDialog(std::string const& group)
    {
        if (!commandDispatcher_) return;
        auto sheet = dataSheets_.find(group);
        auto const* dataframe = commandDispatcher_->applicationState().datasets().find(group);
        if (sheet == dataSheets_.end() || !sheet->second || !dataframe) return;
        if (auto existing = descriptiveDialogs_.find(group);
            existing != descriptiveDialogs_.end() && existing->second)
        {
            return;
        }
        std::set<int> selection;
        commandDispatcher_->applicationState().selectedRows(group, selection);
        auto initial = ::rlispstat::core::ResolveInitialAnalysisSpecification(
            *dataframe,
            ::rlispstat::core::SharedAnalysisDefinition(
                ::rlispstat::core::SharedAnalysisKind::DescriptiveTable),
            ::rlispstat::core::DefaultVariableRolesForAnalysisInitialization(
                commandDispatcher_->applicationState().variableRoles(group)));
        auto dialog = DescriptiveStatisticsDialog::Create(
            sheet->second->NativeWindow(), *dataframe, selection,
            commandDispatcher_->applicationState().savedSelections(group),
            std::move(initial),
            [this](DescriptiveStatisticsSpecification const& specification)
            {
                QueueDescriptiveStatistics(specification);
            },
            [this, group]() { descriptiveDialogs_.erase(group); });
        dialog->SetAnalysisScope(commandDispatcher_->applicationState().activeAnalysisScope(group));
        descriptiveDialogs_[group] = dialog;
        dialog->Show();
    }

    void App::OpenContingencyTableDialog(
        std::string const& group,
        ::rlispstat::core::AnalysisSpecification const* existingSpecification)
    {
        if (!commandDispatcher_) return;
        auto sheet = dataSheets_.find(group);
        auto const* dataframe = commandDispatcher_->applicationState().datasets().find(group);
        if (sheet == dataSheets_.end() || !sheet->second || !dataframe) return;
        if (auto existing = contingencyDialogs_.find(group);
            existing != contingencyDialogs_.end() && existing->second) return;
        auto initial = ::rlispstat::core::ResolveInitialAnalysisSpecification(
            *dataframe,
            ::rlispstat::core::SharedAnalysisDefinition(
                ::rlispstat::core::SharedAnalysisKind::ContingencyTable),
            ::rlispstat::core::DefaultVariableRolesForAnalysisInitialization(
                commandDispatcher_->applicationState().variableRoles(group)),
            existingSpecification);
        auto dialog = ContingencyTableDialog::Create(
            sheet->second->NativeWindow(), *dataframe,
            std::move(initial),
            [this](ContingencyTableSpecification const& specification)
            {
                CreateContingencyTable(specification);
            },
            [this, group]() { contingencyDialogs_.erase(group); });
        contingencyDialogs_[group] = dialog;
        dialog->Show();
    }

    void App::OpenMissingDataImputationDialog(std::string const& group)
    {
        LogStartup(std::string("Opening Multiple Imputation dialog for dataset: ") + group);
        const auto fail = [](std::wstring const& message)
        {
            LogStartup(std::string("Multiple Imputation dialog failed: ") +
                to_string(hstring(message)));
            MessageBoxW(GetActiveWindow(), message.c_str(), L"Multiple Imputation",
                        MB_OK | MB_ICONERROR);
        };
        if (!commandDispatcher_)
        {
            fail(L"The LinkEDA data service is not available.");
            return;
        }
        auto sheet = dataSheets_.find(group);
        auto const* dataframe = commandDispatcher_->applicationState().datasets().find(group);
        if (sheet == dataSheets_.end() || !sheet->second || !dataframe)
        {
            fail(L"The active dataset is not available to the imputation dialog.");
            return;
        }
        if (auto existing = imputationDialogs_.find(group);
            existing != imputationDialogs_.end() && existing->second)
        {
            if (existing->second->IsClosed()) imputationDialogs_.erase(existing);
            else
            {
                // Show() also activates the XamlRoot owner when the modal is
                // already open, recovering a dialog hidden behind another
                // independent LinkEDA window.
                try { existing->second->Show(); }
                catch (hresult_error const& error) { fail(error.message().c_str()); }
                return;
            }
        }
        try
        {
            auto dialog = MissingDataImputationDialog::Create(
                sheet->second->NativeWindow(), *dataframe,
                [this](MissingDataImputationSpecification const& specification)
                {
                    QueueMultipleImputation(specification);
                },
                [this, group]() { imputationDialogs_.erase(group); },
                fail);
            imputationDialogs_[group] = dialog;
            dialog->Show();
        }
        catch (hresult_error const& error)
        {
            imputationDialogs_.erase(group);
            fail(error.message().c_str());
        }
        catch (std::exception const& error)
        {
            imputationDialogs_.erase(group);
            fail(to_hstring(error.what()).c_str());
        }
    }

    void App::OpenChooseLabelColumnDialog(std::string const& group)
    {
        if (!commandDispatcher_) return;
        auto sheet = dataSheets_.find(group);
        auto const* dataframe = commandDispatcher_->applicationState().datasets().find(group);
        if (sheet == dataSheets_.end() || !sheet->second || !dataframe) return;
        const std::string key = group + ":label";
        if (datasetVariableDialogs_.count(key) && datasetVariableDialogs_[key]) return;
        const auto labels = ::rlispstat::core::BuildChooseLabelColumnDialogState();
        auto dialog = DatasetVariableDialog::Create(
            sheet->second->NativeWindow(), *dataframe,
            to_hstring(labels.title).c_str(), to_hstring(labels.informativeText).c_str(),
            to_hstring(labels.chooseButtonTitle).c_str(), false,
            commandDispatcher_->applicationState().labelColumn(group),
            [this, group](std::vector<std::string> const& variables)
            {
                const std::string selected = variables.empty() ? std::string{} : variables.front();
                (void)commandDispatcher_->dispatch({"SET_LABEL_COLUMN", group, selected});
            },
            [this, key]() { datasetVariableDialogs_.erase(key); });
        datasetVariableDialogs_[key] = dialog;
        dialog->Show();
    }

    fire_and_forget App::OpenMissingDataPatternsDialog(std::string group, bool models)
    {
        auto lifetime=get_strong();
        if(!commandDispatcher_)co_return;
        auto sheet=dataSheets_.find(group);
        auto const* current=commandDispatcher_->applicationState().datasets().find(group);
        if(sheet==dataSheets_.end() || !sheet->second || !current)co_return;
        const auto data=*current;
        const auto scope=commandDispatcher_->applicationState().activeAnalysisScope(group);
        const auto capability=::rlispstat::core::MissingDataCapabilitiesFor(data);
        if(models && !capability.models) {
            ShowWorkflowMessage(technicalOwner_,L"Missingness Models",capability.modelsUnavailableReason);
            co_return;
        }
        auto panel=Controls::StackPanel(); panel.Spacing(8);
        auto label=[&](std::wstring const& text){auto t=Controls::TextBlock();t.Text(text);t.TextWrapping(TextWrapping::Wrap);panel.Children().Append(t);};
        label(models?L"Model associations between missingness and observed predictors. This does not establish MAR. Missing predictors are excluded.":
            L"Choose variables for the missingness table. Add or remove variables, open descriptives, request Little’s test and save derived columns from the results window.");
        label(L"Data source");
        auto source=Controls::ComboBox();
        if(capability.original)source.Items().Append(box_value(L"Original data before imputation"));
        source.Items().Append(box_value(L"Current/active data")); source.SelectedIndex(0);panel.Children().Append(source);
        auto target=Controls::ComboBox();
        if(models){label(L"Target missingness");for(auto const& v:capability.targets)target.Items().Append(box_value(to_hstring(v)));target.SelectedIndex(0);panel.Children().Append(target);}
        label(to_hstring("Scope: "+scope.sourceDescription).c_str());
        std::vector<Controls::CheckBox> first, second, save;
        auto lists=Controls::StackPanel();lists.Orientation(Controls::Orientation::Horizontal);lists.Spacing(12);
        auto list=[&](bool descriptions,std::vector<Controls::CheckBox>& buttons){
            auto contents=Controls::StackPanel();
            auto header=Controls::TextBlock();header.Text(models?L"Predictors":L"Variables");contents.Children().Append(header);
            auto items=Controls::StackPanel();
            for(auto const& col:data.columns){auto b=Controls::CheckBox();b.Content(box_value(to_hstring(col.name+" ("+col.type+")")));
                const bool supported=col.type!="character" && col.type!="text";
                b.IsEnabled(!descriptions || supported);b.IsChecked(!models && (!descriptions || supported));
                items.Children().Append(b);buttons.push_back(b);}
            auto scroll=Controls::ScrollViewer();scroll.Height(220);scroll.Width(500);scroll.Content(items);contents.Children().Append(scroll);lists.Children().Append(contents);
        };
        list(models,first);panel.Children().Append(lists);
        if(models)label(L"Save to data (optional)");
        const std::vector<std::pair<std::string,std::wstring>> choices=models?
            std::vector<std::pair<std::string,std::wstring>>{{"indicator",L"Missingness indicator"},{"probability",L"Predicted probability"}}:
            std::vector<std::pair<std::string,std::wstring>>{{"pattern",L"Missingness pattern"},{"n_missing",L"Number missing"},{"pct_missing",L"Percent missing"},{"indicators",L"Variable indicators"}};
        if(models)for(auto const& choice:choices){auto b=Controls::CheckBox();b.Content(box_value(to_hstring(choice.second)));panel.Children().Append(b);save.push_back(b);}
        auto body=Controls::ScrollViewer();body.Content(panel);body.MaxHeight(650);
        auto dialog=Controls::ContentDialog();dialog.Title(box_value(models?L"Missingness Models":L"Missing Data Overview"));
        dialog.Content(body);dialog.PrimaryButtonText(models?L"Fit model":L"Analyze");dialog.CloseButtonText(L"Cancel");
        auto owner=sheet->second->NativeWindow().Content().as<FrameworkElement>();dialog.XamlRoot(owner.XamlRoot());
        if(co_await dialog.ShowAsync()!=Controls::ContentDialogResult::Primary)co_return;
        auto checked=[](Controls::CheckBox const& b){return b.IsChecked() && b.IsChecked().Value();};
        const std::string response=models && target.SelectedIndex()>=0?capability.targets[target.SelectedIndex()]:"";
        std::vector<std::string> variables,descriptions,saved;
        for(size_t i=0;i<first.size();++i)if(checked(first[i]) && data.columns[i].name!=response)variables.push_back(data.columns[i].name);
        for(size_t i=0;i<second.size();++i)if(checked(second[i]))descriptions.push_back(data.columns[i].name);
        for(size_t i=0;i<save.size();++i)if(checked(save[i]))saved.push_back(choices[i].first);
        pendingMainRTasks_.analysisWorkflowTasks.push_back(::rlispstat::core::MissingDataWorkflowTask(data,models,
            capability.original && source.SelectedIndex()==0?"original":"active",response,variables,descriptions,saved,scope));
        MarkMainRTaskPending();
    }

    fire_and_forget App::QueueMultipleImputation(
        MissingDataImputationSpecification specification)
    {
        auto lifetime = get_strong();
        const auto labels = ::rlispstat::core::BuildMissingDataImputationDialogState();
        if (!commandDispatcher_) co_return;
        auto const* registered = commandDispatcher_->applicationState().datasets().find(
            specification.group);
        if (!registered)
        {
            MessageBoxW(nullptr, to_hstring(labels.datasetUnavailableStatus).c_str(),
                to_hstring(labels.failedTitle).c_str(), MB_OK | MB_ICONERROR);
            co_return;
        }
        const auto scope = commandDispatcher_->applicationState().activeAnalysisScope(specification.group);
        const auto rows = ::rlispstat::core::ResolveAnalysisScopeRowIds(scope, registered->rows);
        if (rows.empty()) {
            MessageBoxW(nullptr, L"The global analysis scope contains no observations.", L"Multiple Imputation", MB_OK | MB_ICONINFORMATION);
            co_return;
        }
        const auto dataframe = ::rlispstat::core::SubsetDataFrame(*registered, rows, registered->group);
        const std::string outputGroup = commandDispatcher_->applicationState().datasets()
            .uniqueDatasetName(specification.group + " imputed");
        const auto dispatcher = dispatcherQueue_;
        if (auto sheet = dataSheets_.find(specification.group);
            sheet != dataSheets_.end() && sheet->second)
            sheet->second->SetOperationStatus("Running multiple imputation...");

        co_await winrt::resume_background();
        auto result = RunNativeImputation(dataframe, specification, outputGroup);
        co_await wil::resume_foreground(dispatcher,
            winrt::Microsoft::UI::Dispatching::DispatcherQueuePriority::Normal);

        if (result.ok)
        {
            if (!specification.openDataSheet && !result.payload.empty() &&
                result.payload.front() == "REGISTER_DATASET")
                result.payload.front() = "REGISTER_DATASET_SILENT";
            const std::string reply = DispatchRequest(result.payload);
            if (reply.rfind("ERR ", 0) == 0)
            {
                result.ok = false;
                result.message = reply.substr(4);
            }
        }
        if (auto sheet = dataSheets_.find(specification.group);
            sheet != dataSheets_.end() && sheet->second)
        {
            sheet->second->SetOperationStatus(result.ok
                ? result.message
                : "Multiple imputation failed: " + result.message);
        }
        MessageBoxW(nullptr, to_hstring(result.message).c_str(),
            to_hstring(result.ok ? labels.title : labels.failedTitle).c_str(),
            MB_OK | (result.ok ? MB_ICONINFORMATION : MB_ICONERROR));
    }

    void App::OpenRDataAssignmentDialog(std::string const& group,
                                        bool selectedRowsOnly)
    {
        if (!commandDispatcher_) return;
        auto sheet = dataSheets_.find(group);
        auto const* dataframe = commandDispatcher_->applicationState().datasets().find(group);
        if (sheet == dataSheets_.end() || !sheet->second || !dataframe) return;
        std::set<int> selectedRows;
        commandDispatcher_->applicationState().selectedRows(group, selectedRows);
        const auto key = group + (selectedRowsOnly ? ":selected" : ":all");
        if (auto existing = rDataAssignmentDialogs_.find(key);
            existing != rDataAssignmentDialogs_.end() && existing->second) return;
        auto dialog = RDataAssignmentDialog::Create(
            sheet->second->NativeWindow(), *dataframe, selectedRowsOnly,
            selectedRows.size(),
            [this](RDataAssignmentSpecification const& specification)
            {
                QueueRDataAssignment(specification);
            },
            [this, key]() { rDataAssignmentDialogs_.erase(key); });
        rDataAssignmentDialogs_[key] = dialog;
        dialog->Show();
    }

    void App::QueueRDataAssignment(RDataAssignmentSpecification const& specification)
    {
        if (!commandDispatcher_) return;
        auto const* dataframe = commandDispatcher_->applicationState().datasets().find(
            specification.group);
        if (!dataframe) return;

        std::set<int> selectedRows;
        if (specification.selectedRowsOnly)
        {
            commandDispatcher_->applicationState().selectedRows(
                specification.group, selectedRows);
            if (selectedRows.empty())
            {
                MessageBoxW(nullptr, L"Select at least one observation first.",
                    L"Return Selected Rows to R", MB_OK | MB_ICONINFORMATION);
                return;
            }
        }

        std::error_code error;
        auto folder = std::filesystem::temp_directory_path(error) /
            ("rlispstat-return-" + std::to_string(
                static_cast<unsigned long long>(GetTickCount64())));
        if (error || !std::filesystem::create_directories(folder, error) || error)
        {
            MessageBoxW(nullptr, L"LinkEDA could not create the temporary R exchange folder.",
                L"Return Data to R", MB_OK | MB_ICONERROR);
            return;
        }
        auto payload = folder / "dataset.txt";
        std::ofstream output(payload, std::ios::binary | std::ios::trunc);
        if (!output)
        {
            std::filesystem::remove_all(folder, error);
            MessageBoxW(nullptr, L"LinkEDA could not write the temporary R data payload.",
                L"Return Data to R", MB_OK | MB_ICONERROR);
            return;
        }
        ::rlispstat::core::WriteDataFramePayloadForR(output, *dataframe, true);
        output.close();
        if (!output)
        {
            std::filesystem::remove_all(folder, error);
            MessageBoxW(nullptr, L"LinkEDA could not finish writing the R data payload.",
                L"Return Data to R", MB_OK | MB_ICONERROR);
            return;
        }

        ::rlispstat::core::MainRDataAssignTask task;
        task.requestId = StartupId("return-r-", specification.group);
        task.group = specification.group;
        task.payloadPath = payload.string();
        task.objectName = specification.objectName;
        task.mode = specification.selectedRowsOnly ? "selected" : "all";
        task.replaceExisting = specification.replaceExisting;
        task.rows.assign(selectedRows.begin(), selectedRows.end());
        pendingMainRTasks_.dataAssignTasks.push_back(std::move(task));
        MarkMainRTaskPending();
    }

    void App::CreateContingencyTable(ContingencyTableSpecification const& specification)
    {
        if (!commandDispatcher_) return;
        auto const* dataframe = commandDispatcher_->applicationState().datasets().find(specification.group);
        if (!dataframe) return;
        const auto id = "contingency_windows_" + std::to_string(
            static_cast<unsigned long long>(GetTickCount64()));
        const auto scope = commandDispatcher_->applicationState().activeAnalysisScope(specification.group);
        if (dataframe->datasetType == "multiple_imputation")
        {
            ::rlispstat::core::MainRAnalysisWorkflowTask task;
            task.kind = "contingency";
            task.group = specification.group;
            task.variables = { specification.rowVariable };
            task.groupVariable = specification.columnVariable;
            if (scope.kind == ::rlispstat::core::AnalysisScopeKind::ExplicitRowIds)
            {
                task.rows = ::rlispstat::core::ResolveAnalysisScopeRowIds(scope, scope.totalDatasetRows);
                if (task.rows.empty()) return;
            }
            pendingMainRTasks_.analysisWorkflowTasks.push_back(std::move(task));
            MarkMainRTaskPending();
            return;
        }
        auto state = ::rlispstat::core::NestedContingencyTableStateForDataFrame(
            *dataframe, id, { specification.rowVariable }, specification.columnVariable,
            "count_percent", &scope);
        state.datasetId = specification.group;
        state.title = "Contingency Table \xE2\x80\x94 " + specification.rowVariable +
            " by " + specification.columnVariable;
        ShowTable1(state);
    }

    ::rlispstat::core::PlotCoordinationResult App::AttachPlotModel(
        ::rlispstat::core::PlotModel& model)
    {
        if (!commandDispatcher_) return {};
        auto& state = commandDispatcher_->applicationState();
        auto const* dataset = state.datasets().find(model.group);
        const std::size_t totalRows = dataset
            ? static_cast<std::size_t>(std::max(0, dataset->rows)) : 0;
        if (model.isDatasetSeed)
        {
            model.dataScope = ::rlispstat::core::AllObservationsAnalysisScope(
                model.group, totalRows);
            model.dataScopeCaptured = false;
        }
        else
        {
            if (!model.dataScopeCaptured)
            {
                model.dataScope = state.activeAnalysisScope(model.group);
                model.dataScopeCaptured = true;
            }
            ::rlispstat::core::ApplyAnalysisScopeToPlot(
                model, model.dataScope, totalRows);
        }
        auto result = commandDispatcher_->plotCoordinator().attachPlot(model);
        if (result.accepted && !model.isDatasetSeed)
        {
            model.rExportTheme = commandDispatcher_->plotTheme();
            ::rlispstat::core::RefreshBasicPlotCodeReference(state, model);
        }
        return result;
    }

    void App::CreateScatterplot(ScatterplotSpecification const& specification)
    {
        if (!commandDispatcher_) return;
        auto const* dataframe = commandDispatcher_->applicationState().datasets().find(specification.group);
        if (!dataframe) return;
        auto model = std::make_unique<::rlispstat::core::PlotModel>();
        ::rlispstat::core::PopulateDatasetSeedPlot(*model, *dataframe, specification.group);
        model->id = "scatter_windows_" + std::to_string(
            static_cast<unsigned long long>(GetTickCount64()));
        model->kind = "scatter";
        model->isDatasetSeed = false;
        model->xLabel = specification.xVariable;
        model->yLabel = specification.yVariable;
        model->title = specification.title.empty()
            ? ::rlispstat::core::ScatterplotDefaultTitle(model->xLabel, model->yLabel)
            : specification.title;
        std::string preparationError;
        if (!::rlispstat::core::PrepareScatterplotVariablesFromDataFrame(
                *model, *dataframe, specification.xVariable,
                specification.yVariable, &preparationError))
        {
            LogStartup("Could not open scatterplot: " + preparationError);
            return;
        }
        model->interactionMode = "select";
        model->selectionMode = "replace";
        model->overlays.clear();
        if (specification.dataScopeCaptured)
        {
            model->dataScope = specification.dataScope;
            model->dataScopeCaptured = true;
        }
        auto* modelPointer = model.get();
        plots_[model->id] = std::move(model);
        const auto attached = AttachPlotModel(*modelPointer);
        if (!attached.accepted)
        {
            plots_.erase(modelPointer->id);
            return;
        }
        commandDispatcher_->plotCoordinator().activatePlot(modelPointer->id);
        ::rlispstat::core::SessionPlot plot;
        plot.id = modelPointer->id;
        plot.group = modelPointer->group;
        plot.xLabel = modelPointer->xLabel;
        plot.yLabel = modelPointer->yLabel;
        plot.title = modelPointer->title;
        for (auto const& point : modelPointer->points)
            plot.points.push_back({ point.x, point.y, point.row });
        ShowScatterPlot(plot);
    }

    std::string App::CreateScatterMatrix(std::string const& group,
                                         std::vector<std::string> const& variables)
    {
        if (!commandDispatcher_) return {};
        auto const* dataframe = commandDispatcher_->applicationState().datasets().find(group);
        if (!dataframe) return {};
        auto model = std::make_unique<::rlispstat::core::PlotModel>();
        ::rlispstat::core::PopulateDatasetSeedPlot(*model, *dataframe, group);
        if (model->variables.size() < 2) return {};
        model->id = "scatter_matrix_windows_" + std::to_string(
            static_cast<unsigned long long>(GetTickCount64()));
        model->kind = "scatter_matrix";
        model->isDatasetSeed = false;
        model->title = "Scatterplot matrix";
        for (auto const& requested : variables)
            if (::rlispstat::core::FindNumericVariable(*model, requested))
                model->scatterMatrixVariables.push_back(requested);
        if (model->scatterMatrixVariables.size() < 2) return {};
        model->interactionMode = "select";
        model->selectionMode = "replace";
        ::rlispstat::core::RebuildScatterMatrixPoints(*model);
        ::rlispstat::core::ComputeRanges(*model);
        auto* pointer = model.get();
        plots_[model->id] = std::move(model);
        const auto attached = AttachPlotModel(*pointer);
        if (!attached.accepted) { plots_.erase(pointer->id); return {}; }
        commandDispatcher_->plotCoordinator().activatePlot(pointer->id);
        ShowNativePlot(*pointer);
        return pointer->id;
    }

    std::string App::CreateParallelCoordinates(std::string const& group,
                                               std::vector<std::string> const& variables,
                                               bool standardize, bool connect)
    {
        if (!commandDispatcher_) return {};
        auto const* dataframe = commandDispatcher_->applicationState().datasets().find(group);
        if (!dataframe) return {};
        auto model = std::make_unique<::rlispstat::core::PlotModel>();
        ::rlispstat::core::PopulateDatasetSeedPlot(*model, *dataframe, group);
        if (model->variables.size() < 2) return {};
        model->id = "parallel_windows_" + std::to_string(
            static_cast<unsigned long long>(GetTickCount64()));
        model->kind = "boxplot";
        model->isDatasetSeed = false;
        model->title = "Parallel Coordinates";
        model->xLabel.clear();
        for (auto const& requested : variables)
            if (::rlispstat::core::FindNumericVariable(*model, requested))
                model->boxplotVariables.push_back(requested);
        if (model->boxplotVariables.size() < 2) return {};
        model->boxplotStandardizeVariables = standardize;
        model->boxplotConnectRows = connect;
        model->boxplotShowPoints = true;
        model->boxplotShowBox = false;
        model->boxplotShowWhiskers = false;
        model->boxplotShowViolin = false;
        model->boxplotConnectionLineWidth = 1.25;
        model->interactionMode = "select";
        model->selectionMode = "replace";
        ::rlispstat::core::EnsureParallelBoxplotState(*model);
        auto* pointer = model.get();
        plots_[model->id] = std::move(model);
        const auto attached = AttachPlotModel(*pointer);
        if (!attached.accepted) { plots_.erase(pointer->id); return {}; }
        commandDispatcher_->plotCoordinator().activatePlot(pointer->id);
        ShowNativePlot(*pointer);
        return pointer->id;
    }

    void App::RefreshOpenPlotsForScope(std::string const& group,
                                       bool selectionChanged)
    {
        if (!commandDispatcher_) return;
        auto& application = commandDispatcher_->applicationState();
        const auto global = application.activeAnalysisScope(group);
        if (selectionChanged &&
            !::rlispstat::core::AnalysisScopeTracksCurrentSelection(global) &&
            global.sourceKind !=
                ::rlispstat::core::AnalysisScopeSourceKind::CurrentUnselection)
            return;
        auto const* dataframe = application.datasets().find(group);
        if (!dataframe) return;

        for (auto& [id, owned] : plots_)
        {
            if (!owned || owned->group != group || owned->isDatasetSeed ||
                ::rlispstat::core::PlotIsDerivedAnalysisView(*owned)) continue;
            auto& plot = *owned;
            if (plot.analysisScopeFrozen)
            {
                plot.frozenScopeNotice = plot.dataScopeCaptured
                    ? ::rlispstat::core::FrozenAnalysisScopeNotice(
                        plot.dataScope, global)
                    : std::string{};
            }
            else
            {
                ::rlispstat::core::SyncPlotVariablesFromDataFrame(plot, *dataframe);
                std::string ignored;
                if (plot.kind == "scatter")
                {
                    ::rlispstat::core::RebuildPointsForCurrentVariables(plot);
                    ComputeScatterRangesIncludingAllImputations(plot, *dataframe);
                }
                else if (plot.kind == "time_series")
                    ::rlispstat::core::RebuildTimeSeriesFromDataFrame(
                        plot, *dataframe, &ignored);
                else if (plot.kind == "trellis_scatterplot")
                    ::rlispstat::core::RebuildTrellisPlotFromDataFrame(
                        plot, *dataframe, &ignored);
                else if (plot.kind == "scatter_matrix")
                {
                    ::rlispstat::core::RebuildScatterMatrixPoints(plot);
                    ::rlispstat::core::ComputeRanges(plot);
                }
                else if (plot.kind == "histogram")
                    ::rlispstat::core::RebuildHistogramPointsFromCurrentVariable(plot);
                else if (plot.kind == "barplot")
                    ::rlispstat::core::RebuildBarplotFromDataFrame(
                        plot, *dataframe, &ignored);
                else if (plot.kind == "boxplot")
                {
                    if (::rlispstat::core::BoxplotUsesVariableAxes(plot))
                        ::rlispstat::core::RebuildParallelBoxplotPoints(
                            plot, &ignored);
                    else
                        ::rlispstat::core::RebuildGroupedBoxplotPointsFromDataFrame(
                            plot, *dataframe);
                }
                else continue;

                ::rlispstat::core::ApplyAnalysisScopeToPlot(
                    plot, global,
                    static_cast<std::size_t>(std::max(0, dataframe->rows)));
                plot.frozenScopeNotice.clear();
                plot.rExportTheme = commandDispatcher_->plotTheme();
                ::rlispstat::core::RefreshBasicPlotCodeReference(application, plot);
                application.registerOutputCodeReference(plot.codeReference);
                if (plot.kind == "scatter_matrix")
                {
                    ::rlispstat::core::InvalidateScatterMatrixFits(plot);
                    if (plot.scatterMatrixFitsPending)
                        QueueSmoothRecompute(plot,
                            ::rlispstat::core::SmoothCurveScope::Overall, "lm");
                }
                else QueueExistingSmoothRecompute(plot);
            }
            auto view = views_.find(id);
            if (view != views_.end() && view->second)
            {
                view->second->SetDataFrame(dataframe);
                view->second->SetAnalysisScope(global);
                view->second->Show(plot);
                view->second->SetVisualStyle(commandDispatcher_->plotTheme(),
                    application.displayPointColors(group));
            }
        }
    }

    void App::RefitOpenAnalysesForScope(std::string const& group)
    {
        auto& app = commandDispatcher_->applicationState();
        const auto globalScope = app.activeAnalysisScope(group);
        for (auto& [id, state] : app.groupModels()) {
            if (state.group != group || (state.precomputed && !state.multipleImputation) ||
                !linearModelViews_.count(id) || !linearModelViews_[id]) continue;
            if (!state.autoRefit) {
                state.frozenScopeNotice = state.dataScopeCaptured
                    ? ::rlispstat::core::FrozenAnalysisScopeNotice(state.dataScope, globalScope)
                    : std::string{};
                ShowLinearModel(id);
                continue;
            }
            if (!app.captureAnalysisScope(group, state.dataScope, state.dataScopeCaptured, &state.scope)) continue;
            ::rlispstat::core::MarkGroupModelChanged(state);
            QueueLinearModelFit(id);
        }
        for (auto& [id, state] : app.correlationMatrices()) {
            if (state.group != group || state.precomputed || !correlationViews_.count(id) || !correlationViews_[id] ||
                !app.captureAnalysisScope(group, state.dataScope, state.dataScopeCaptured)) continue;
            QueueCorrelationFit(state);
            ShowCorrelationMatrix(id);
        }
        for (auto& [id, state] : app.dimensionalityModels()) {
            if (state.group == group && dimensionalityViews_.count(id) && dimensionalityViews_[id]) {
                if (!state.autoFit) {
                    if (state.dataScopeCaptured)
                        state.status = ::rlispstat::core::FrozenAnalysisScopeNotice(state.dataScope, globalScope);
                    ShowDimensionality(id);
                    continue;
                }
                if (app.captureAnalysisScope(group, state.dataScope, state.dataScopeCaptured, &state.scope))
                    RefitDimensionality(id);
            }
        }
        for (auto& [id, state] : app.scaleAnalyses()) {
            if (state.group == group && scaleAnalysisViews_.count(id) && scaleAnalysisViews_[id]) {
                if (!state.autoFit) {
                    if (state.dataScopeCaptured)
                        state.status = ::rlispstat::core::FrozenAnalysisScopeNotice(state.dataScope, globalScope);
                    ShowScaleAnalysis(id);
                    continue;
                }
                if (!app.captureAnalysisScope(group, state.dataScope, state.dataScopeCaptured)) continue;
                // Scope is part of the fit identity even when its items do
                // not change. Reject any older in-flight R result by revision.
                state.modelVersion = ++state.specification.revision;
                RefitScaleAnalysis(id);
            }
        }
        for (auto& [id, state] : app.dendrograms())
            if (state.group == group && dendrogramViews_.count(id) && dendrogramViews_[id] &&
                app.captureAnalysisScope(group, state.dataScope, state.dataScopeCaptured)) RefitDendrogram(id);
        for (auto& [id, state] : app.generalizedGLMs()) {
            if (state.group != group || (state.precomputed && !state.multipleImputation) ||
                !generalizedModelViews_.count(id) || !generalizedModelViews_[id]) continue;
            if (!state.autoRefit) {
                state.frozenScopeNotice = state.dataScopeCaptured
                    ? ::rlispstat::core::FrozenAnalysisScopeNotice(state.dataScope, globalScope)
                    : std::string{};
                ShowGeneralizedModel(id);
                continue;
            }
            if (app.captureAnalysisScope(group, state.dataScope, state.dataScopeCaptured, &state.scope))
                RequestGeneralizedModelFit(id);
        }
        for (auto& [id, state] : app.regressionComparisons()) {
            if (state.group != group || state.precomputed ||
                !regressionComparisonViews_.count(id) || !regressionComparisonViews_[id]) continue;
            if (!state.autoRefit) {
                state.frozenScopeNotice = state.dataScopeCaptured
                    ? ::rlispstat::core::FrozenAnalysisScopeNotice(state.dataScope, globalScope)
                    : std::string{};
                ShowRegressionComparison(id);
                continue;
            }
            if (!app.captureAnalysisScope(group, state.dataScope, state.dataScopeCaptured, &state.scope)) continue;
            QueueRegressionComparison(state);
            ShowRegressionComparison(id);
        }
        for (auto& [id, state] : app.generalizedComparisons()) {
            if (state.group != group || !generalizedComparisonViews_.count(id) || !generalizedComparisonViews_[id]) continue;
            if (!state.autoRefit) {
                state.frozenScopeNotice = state.dataScopeCaptured
                    ? ::rlispstat::core::FrozenAnalysisScopeNotice(state.dataScope, globalScope)
                    : std::string{};
                ShowGeneralizedComparison(id);
                continue;
            }
            if (!app.captureAnalysisScope(group, state.dataScope, state.dataScopeCaptured, &state.scope)) continue;
            QueueGeneralizedComparison(state);
            ShowGeneralizedComparison(id);
        }
        for (auto& [id, state] : app.modelTrellises())
            if (state.specification.baseModel.group == group && modelTrellisViews_.count(id) && modelTrellisViews_[id] &&
                app.captureAnalysisScope(group, state.dataScope, state.dataScopeCaptured)) {
                QueueModelTrellisFit(state);
                ShowModelTrellis(id);
            }
        for (auto const& [id, state] : meanComparisonStates_)
            if (state.datasetId == group && meanComparisonViews_.count(id) && meanComparisonViews_[id])
                RefitMeanComparison(id);

        std::vector<::rlispstat::core::Table1DisplayState> tablesToRefit;
        for (auto const& [id, state] : outputStates_)
        {
            if (state.datasetId != group || !state.missingnessSource.empty() ||
                !state.linkedModelOutputId.empty() ||
                (state.tableType != "table1" && state.tableType != "frequency" &&
                 state.tableType != "nested_contingency")) continue;
            auto requested = state;
            requested.dataScope = globalScope;
            requested.dataScopeCaptured = true;
            requested.needsRFit = true;
            requested.statusText =
                "Global analysis scope changed; recalculating in R...";
            tablesToRefit.push_back(std::move(requested));
        }
        for (auto const& state : tablesToRefit) ShowTable1(state);
    }

    void App::RefreshAnalysisScopeIndicators(std::string const& group)
    {
        auto& app=commandDispatcher_->applicationState();
            auto dialog = descriptiveDialogs_.find(group);
            if(dialog != descriptiveDialogs_.end() && dialog->second) dialog->second->SetAnalysisScope(app.activeAnalysisScope(group));
            const auto choices = AnalysisScopeChoicesFor(commandDispatcher_->applicationState(),group,true);
            for(auto const& [id,view] : correlationViews_) if(view && app.correlationMatrices().at(id).group==group) { view->SetScopeChoices(choices); }
            for(auto const& [id,view] : meanComparisonViews_) if(view && meanComparisonStates_.at(id).datasetId==group) { view->SetScopeChoices(choices); }
            for(auto const& [id,view] : dimensionalityViews_) if(view && app.dimensionalityModels().at(id).group==group) { view->SetScopeChoices(choices); }
            for(auto const& [modelId,view] : linearModelViews_) {
                auto model = app.groupModels().find(modelId);
                if(view && model != app.groupModels().end() && model->second.group == group)
                    view->SetScopeChoices(choices);
            }
            for(auto const& [id,view] : generalizedModelViews_) if(view && app.generalizedGLMs().at(id).group==group) view->SetScopeChoices(choices);
            for(auto const& [id,view] : regressionComparisonViews_) if(view && app.regressionComparisons().at(id).group==group) view->SetScopeChoices(choices);
            for(auto const& [id,view] : generalizedComparisonViews_) if(view && app.generalizedComparisons().at(id).group==group) view->SetScopeChoices(choices);
    }

    void App::QueueDescriptiveStatistics(
        DescriptiveStatisticsSpecification const& specification)
    {
        ::rlispstat::core::MainRTable1Task task;
        task.id = specification.requestId;
        task.group = specification.group;
        task.variables = specification.variables;
        task.groupVariable = specification.groupVariable;
        if (commandDispatcher_)
            if (auto const* dataframe = commandDispatcher_->applicationState().datasets().find(
                    specification.group))
                task.variableTypes = Table1CanonicalVariableTypes(
                    *dataframe, task.variables, task.groupVariable);
        task.includeMissing = specification.includeMissing;
        task.showP = specification.showP;
        task.showTest = specification.showTest;
        task.showN = specification.showN;
        task.ordinalAs = specification.ordinalAs;
        const auto scope = commandDispatcher_->applicationState().activeAnalysisScope(specification.group);
        task.scope = scope.kind == ::rlispstat::core::AnalysisScopeKind::AllObservations ? "all" : "selected";
        task.scopeDescription = scope.sourceDescription;
        task.rows = commandDispatcher_->applicationState().resolveActiveAnalysisRowIds(specification.group);
        commandDispatcher_->prepareTable1Task(task);
        pendingMainRTasks_.table1Tasks.push_back(std::move(task));
        MarkMainRTaskPending();
        LogStartup("Queued descriptive statistics for " + specification.group);
    }

    void App::ShowTable1(::rlispstat::core::Table1DisplayState const& state)
    {
        ::rlispstat::core::Table1DisplayState stateCopy = state;
        if (stateCopy.needsRFit && commandDispatcher_) {
            ::rlispstat::core::MainRTable1Task task;
            task.id = state.id; task.group = state.datasetId; task.title = state.title;
            task.analysisKind = state.tableType == "nested_contingency" ? "nested_contingency" : state.tableType == "frequency" ? "frequency" : "table1";
            task.contingencyDisplayMode = state.nestedDisplayMode;
            task.sourcePlotId = state.sourcePlotId; task.linkEnabled = state.linkEnabled;
            task.variables = state.variables; task.groupVariable = state.groupVariable;
            task.variableTypes = state.variableTypes; task.showP = state.showP; task.showTest = state.showTest;
            task.scope = state.dataScopeCaptured ? ScopeName(state.dataScope) : "all";
            task.scopeDescription = state.dataScopeCaptured ? state.dataScope.sourceDescription : "All observations";
            if (task.scope == "selected") task.rows = state.dataScope.originalRowIds;
            commandDispatcher_->prepareTable1Task(task);
            pendingMainRTasks_.table1Tasks.push_back(std::move(task));
            MarkMainRTaskPending();
            stateCopy.needsRFit = false;
        }
        if (commandDispatcher_)
            ::rlispstat::core::RefreshNativeTableCodeReference(
                commandDispatcher_->applicationState(), stateCopy);
        if (auto dialog = descriptiveDialogs_.find(state.datasetId);
            dialog != descriptiveDialogs_.end() && dialog->second)
            dialog->second->Close();
        std::shared_ptr<AnalysisOutputView> view;
        auto existing = outputViews_.find(state.id);
        if (existing != outputViews_.end() && existing->second) view = existing->second;
        else view = AnalysisOutputView::Create();
        const std::string resultId = state.id;
        view->SetClosedCallback([this, resultId]() { HandleOutputClosed(resultId); });
        std::vector<std::string> availableVariables;
        if (commandDispatcher_)
            if (auto const* dataframe = commandDispatcher_->applicationState().datasets().find(state.datasetId))
                for (auto const& column : dataframe->columns)
                    if (state.tableType == "nested_contingency" || state.tableType == "mi_contingency") {
                        if (::rlispstat::core::AnalysisVariableIsCompatible(column,
                                ::rlispstat::core::AnalysisVariableType::Categorical))
                            availableVariables.push_back(column.name);
                    } else if (!::rlispstat::core::DataColumnLooksLikeId(column))
                        availableVariables.push_back(column.name);
        view->SetCommandCallback([this](std::vector<std::string> const& command)
            { HandleAnalysisViewCommand(command); }, std::move(availableVariables));
        outputViews_[resultId] = view;
        outputStates_[resultId] = stateCopy;
        if (applicationCommands_)
        {
            std::weak_ptr<AnalysisOutputView> weakView = view;
            applicationCommands_->RegisterWindow({
                "output:" + resultId,
                to_hstring(state.title).c_str(),
                state.datasetId,
                "output",
                resultId,
                [weakView]() { if (auto current = weakView.lock()) current->Activate(); },
                [weakView]() { if (auto current = weakView.lock()) current->Close(); },
                {},
                {},
                {},
                {}
            });
        }
        view->Show(stateCopy);
    }

    void App::HandleOutputClosed(std::string const& resultId)
    {
        if (resettingViews_) return;
        if (commandDispatcher_) commandDispatcher_->cancelTable1Task(resultId);
        ForgetRegressionDerivedOutput(resultId, "table");
        if (applicationCommands_) applicationCommands_->UnregisterWindow("output:" + resultId);
        outputViews_.erase(resultId);
        outputStates_.erase(resultId);
    }

    void App::ShowGLMInteractionReport(
        std::string const& id,
        std::string const& datasetId,
        std::string const& title,
        std::string const& subtitle,
        ::rlispstat::core::GLMInteractionReport const& report)
    {
        std::shared_ptr<GLMInteractionReportView> view;
        auto existing = interactionReportViews_.find(id);
        if (existing != interactionReportViews_.end() && existing->second)
            view = existing->second;
        else
            view = GLMInteractionReportView::Create();
        view->SetClosedCallback([this, id]() { HandleGLMInteractionReportClosed(id); });
        view->SetCommandCallback([this](std::vector<std::string> const& command) {
            HandleAnalysisViewCommand(command);
        });
        interactionReportViews_[id] = view;
        if (applicationCommands_)
        {
            std::weak_ptr<GLMInteractionReportView> weakView = view;
            applicationCommands_->RegisterWindow({
                "output:" + id,
                to_hstring(title).c_str(),
                datasetId,
                "output",
                id,
                [weakView]() { if (auto current = weakView.lock()) current->Activate(); },
                [weakView]() { if (auto current = weakView.lock()) current->Close(); },
                {}, {}, {}, {}, {}
            });
        }
        if (auto dependency = regressionDerivedOutputs_.find(id + "|table");
            dependency != regressionDerivedOutputs_.end()) {
            auto &application = commandDispatcher_->applicationState();
            if (const auto *source = application.regressionSourceOutputCodeReference(
                    dependency->second.sourceKind, dependency->second.sourceModelId)) {
                auto reference = ::rlispstat::core::RegressionInteractionReportCodeReference(id, report, *source);
                if (!reference.outputId.empty()) application.registerOutputCodeReference(reference);
            }
        }
        view->Show(id, datasetId, title, subtitle, report);
    }

    void App::HandleGLMInteractionReportClosed(std::string const& id)
    {
        if (resettingViews_) return;
        ForgetRegressionDerivedOutput(id, "table");
        if (applicationCommands_) applicationCommands_->UnregisterWindow("output:" + id);
        interactionReportViews_.erase(id);
    }

    void App::ShowCorrelationMatrix(std::string const& id)
    {
        if (!commandDispatcher_) return;
        auto found = commandDispatcher_->applicationState().correlationMatrices().find(id);
        if (found == commandDispatcher_->applicationState().correlationMatrices().end()) return;
        ::rlispstat::core::RefreshCorrelationCodeReference(
            commandDispatcher_->applicationState(), found->second);
        auto view = correlationViews_.count(id) && correlationViews_[id]
            ? correlationViews_[id] : CorrelationMatrixView::Create();
        view->SetClosedCallback([this, id]() { HandleCorrelationClosed(id); });
        view->SetCommandCallback([this](std::vector<std::string> const& command)
            { HandleAnalysisViewCommand(command); });
        correlationViews_[id] = view;
        std::weak_ptr<CorrelationMatrixView> weakView = view;
        if (applicationCommands_)
        {
            applicationCommands_->RegisterWindow({
                "correlation:" + id,
                to_hstring(found->second.title).c_str(),
                found->second.group,
                "output",
                id,
                [weakView]() { if (auto current = weakView.lock()) current->Activate(); },
                [weakView]() { if (auto current = weakView.lock()) current->Close(); },
                {}, {}, {}, {}
            });
        }
        std::set<int> selectedRows;
        commandDispatcher_->applicationState().selectedRows(
            found->second.group, selectedRows);
        view->SetScopeChoices(AnalysisScopeChoicesFor(
            commandDispatcher_->applicationState(), found->second.group, false));
        view->Show(found->second, selectedRows.size());
    }

    void App::HandleCorrelationClosed(std::string const& id)
    {
        if (resettingViews_) return;
        if (applicationCommands_) applicationCommands_->UnregisterWindow("correlation:" + id);
        correlationViews_.erase(id);
    }

    void App::ShowMeanComparison(::rlispstat::core::MeanComparisonState const& state)
    {
        auto effective = state;
        std::set<std::string> previousResponses;
        if (auto existing = meanComparisonStates_.find(state.id);
            existing != meanComparisonStates_.end())
        {
            if (existing->second.dataScopeCaptured && !effective.dataScopeCaptured)
            {
                effective.dataScope = existing->second.dataScope;
                effective.dataScopeCaptured = true;
            }
            for (auto const& table : existing->second.tables)
                if (table.tableId == "omnibus")
                    for (auto const& row : table.rows)
                        previousResponses.insert(row.variable);
        }
        if (effective.dataScopeCaptured)
        {
            const auto scope = ::rlispstat::core::AnalysisScopeWindowSummary(
                effective.dataScope, effective.dataScope.totalDatasetRows);
            if (!scope.empty() && effective.subtitle.find(scope) == std::string::npos)
                effective.subtitle += effective.subtitle.empty() ? scope : " \u00b7 " + scope;
        }
        meanComparisonStates_[effective.id] = effective;
        auto view = meanComparisonViews_.count(effective.id) && meanComparisonViews_[effective.id]
            ? meanComparisonViews_[effective.id] : MeanComparisonView::Create();
        view->SetClosedCallback([this, id = effective.id]() { HandleMeanComparisonClosed(id); });
        std::vector<std::string> numericVariables;
        std::vector<std::string> groupingVariables;
        std::map<std::string, std::string> variableTypes;
        std::map<std::string, std::vector<std::string>> groupingLevels;
        std::size_t selectedRowCount = 0;
        std::size_t totalRowCount = 0;
        if (commandDispatcher_)
        {
            if (auto const* dataframe = commandDispatcher_->applicationState().datasets().find(effective.datasetId))
            {
                totalRowCount = static_cast<std::size_t>(std::max(0, dataframe->rows));
                std::set<int> selected;
                if (commandDispatcher_->applicationState().selectedRows(
                        effective.datasetId, selected))
                    selectedRowCount = selected.size();
                numericVariables = MeanComparisonResponseVariables(
                    *dataframe, effective.analysisType == "one_sample_t_test" ||
                        effective.analysisType == "independent_samples_t_test" ||
                        effective.analysisType == "paired_samples_t_test");
                variableTypes = MeanComparisonVariableTypes(*dataframe);
                const bool independent = effective.analysisType == "independent_samples_t_test";
                for (auto const& column : dataframe->columns)
                    if (MeanComparisonGroupingCandidate(column, independent))
                    {
                        groupingVariables.push_back(column.name);
                        groupingLevels[column.name] = MeanComparisonLevels(column);
                    }
            }
        }
        if (commandDispatcher_)
            view->SetScopeChoices(AnalysisScopeChoicesFor(
                commandDispatcher_->applicationState(), effective.datasetId, false));
        view->SetCommandCallback([this](std::vector<std::string> const& command)
            { HandleAnalysisViewCommand(command); }, std::move(numericVariables),
            std::move(groupingVariables), std::move(variableTypes),
            std::move(groupingLevels), selectedRowCount, totalRowCount);
        meanComparisonViews_[effective.id] = view;
        std::weak_ptr<MeanComparisonView> weakView = view;
        if (applicationCommands_)
        {
            applicationCommands_->RegisterWindow({
                "mean:" + effective.id, to_hstring(effective.title).c_str(), effective.datasetId,
                "output", effective.id,
                [weakView]() { if (auto current = weakView.lock()) current->Activate(); },
                [weakView]() { if (auto current = weakView.lock()) current->Close(); },
                {}, {}, {}, {}
            });
        }
        view->Show(effective);
        if (effective.analysisType == "one_way_anova")
        {
            std::set<std::string> currentResponses;
            for (auto const& table : effective.tables)
            {
                if (table.tableId != "omnibus") continue;
                for (auto const& row : table.rows)
                {
                    currentResponses.insert(row.variable);
                    const std::string outputId = "mean_pairwise:" + effective.id + ":" + row.variable;
                    if (!row.variable.empty() && outputStates_.count(outputId))
                    {
                        if (effective.specification.method ==
                            ::rlispstat::core::MeanComparisonMethod::KruskalWallis)
                        {
                            ++meanPairwiseGenerations_[outputId];
                            ShowTable1(GLMPairwiseFailureState(effective.datasetId,
                                effective.groupVariable, outputId,
                                "Kruskal-Wallis requires rank-based post-hoc comparisons."));
                        }
                        else HandleAnalysisViewCommand({"MEAN_PAIRWISE", effective.id, row.variable});
                    }
                }
            }
            for (auto const& response : previousResponses)
            {
                if (currentResponses.count(response)) continue;
                const std::string outputId = "mean_pairwise:" + effective.id + ":" + response;
                if (!outputStates_.count(outputId)) continue;
                ++meanPairwiseGenerations_[outputId];
                ShowTable1(GLMPairwiseFailureState(effective.datasetId,
                    effective.groupVariable, outputId,
                    "The source ANOVA no longer contains this response."));
            }
        }
    }

    void App::HandleMeanComparisonClosed(std::string const& id)
    {
        if (resettingViews_) return;
        if (applicationCommands_) applicationCommands_->UnregisterWindow("mean:" + id);
        if (auto descriptive = meanComparisonDescriptiveViews_.find(id);
            descriptive != meanComparisonDescriptiveViews_.end())
        {
            auto view = descriptive->second;
            meanComparisonDescriptiveViews_.erase(descriptive);
            if (view) view->Close();
        }
        meanComparisonViews_.erase(id);
        meanComparisonStates_.erase(id);
    }

    void App::ShowLinearModel(std::string const& group)
    {
        if (!commandDispatcher_) return;
        auto model = commandDispatcher_->applicationState().groupModels().find(group);
        if (model == commandDispatcher_->applicationState().groupModels().end()) return;
        const std::string datasetGroup = model->second.group;
        auto fit = commandDispatcher_->applicationState().linearModelFits().find(group);
        ::rlispstat::core::GLMFitSummary empty;
        auto const& summary = fit == commandDispatcher_->applicationState().linearModelFits().end()
            ? empty : fit->second;
        auto view = linearModelViews_.count(group) && linearModelViews_[group]
            ? linearModelViews_[group] : LinearModelView::Create();
        view->SetClosedCallback([this, group]() { HandleLinearModelClosed(group); });
        view->SetDiagnosticCallback([this, group](std::string const& kind)
        { OpenLinearModelDiagnostic(group, kind); });
        std::vector<std::string> numericVariables;
        std::vector<std::string> availableVariables;
        if (auto const* dataframe = commandDispatcher_->applicationState().datasets().find(datasetGroup))
        {
            availableVariables = VariableNames(*dataframe);
            ::rlispstat::core::PlotModel seed;
            ::rlispstat::core::PopulateDatasetSeedPlot(seed, *dataframe, datasetGroup);
            numericVariables = ::rlispstat::core::NumericVariableNames(&seed);
        }
        view->SetScopeChoices(AnalysisScopeChoicesFor(
            commandDispatcher_->applicationState(), datasetGroup, true));
        view->SetCommandCallback([this](std::vector<std::string> const& command)
            { HandleAnalysisViewCommand(command); }, std::move(numericVariables),
            std::move(availableVariables));
        linearModelViews_[group] = view;
        std::weak_ptr<LinearModelView> weakView = view;
        if (applicationCommands_)
        {
            applicationCommands_->RegisterWindow({
                "glm:" + group, L"Linear Model", datasetGroup, "output", group,
                [weakView]() { if (auto current = weakView.lock()) current->Activate(); },
                [weakView]() { if (auto current = weakView.lock()) current->Close(); },
                {}, {}, {}, {}
            });
        }
        view->Show(model->second, summary);
        RefreshRegressionDerivedOutputs(group);
        // Diagnostic plots should move in lockstep with the authoritative fit.
        // Refreshing them while the R fit is pending adds a redundant redraw
        // and can briefly expose mismatched results.
        RefreshDiagnosticPlots();
    }

    void App::HandleLinearModelClosed(std::string const& group)
    {
        if (resettingViews_) return;
        if (applicationCommands_) applicationCommands_->UnregisterWindow("glm:" + group);
        linearModelViews_.erase(group);
        if (commandDispatcher_ && group.rfind("linear_model:", 0) == 0) {
            auto& application = commandDispatcher_->applicationState();
            application.groupModels().erase(group);
            application.linearModelFits().erase(group);
            application.eraseOutputCodeReference("glm:" + group);
        }
    }

    void App::ShowDimensionality(std::string const& id)
    {
        if (!commandDispatcher_) return;
        auto found = commandDispatcher_->applicationState().dimensionalityModels().find(id);
        if (found == commandDispatcher_->applicationState().dimensionalityModels().end()) return;
        auto view = dimensionalityViews_.count(id) && dimensionalityViews_[id]
            ? dimensionalityViews_[id] : DimensionalityView::Create();
        view->SetClosedCallback([this, id]() { HandleDimensionalityClosed(id); });
        view->SetChangedCallback([this, id](std::string const& method, std::string const& missing,
            std::string const& rotation, std::string const& scope, bool scale, int components,
            bool autoFit)
        {
            if (!commandDispatcher_) return;
            auto current = commandDispatcher_->applicationState().dimensionalityModels().find(id);
            if (current == commandDispatcher_->applicationState().dimensionalityModels().end()) return;
            auto& state = current->second;
            const bool enablingAutoFit = !state.autoFit && autoFit;
            state.method = method; state.missingMode = missing;
            state.rotation = rotation; state.scale = scale;
            if (::rlispstat::core::SavedAnalysisScopeNameFromChoiceValue(scope))
            {
                ::rlispstat::core::AnalysisScope savedScope;
                std::string error;
                if (!commandDispatcher_->applicationState().resolveSavedAnalysisScopeChoice(
                        state.group, scope, savedScope, &error)) return;
                state.scope = "selected";
                state.dataScope = std::move(savedScope);
                state.dataScopeCaptured = true;
            }
            else
            {
                if (scope != "all" && scope != "selected" && scope != "unselected") return;
                state.scope = scope;
                state.dataScope = {};
                state.dataScopeCaptured = false;
            }
            if (enablingAutoFit) state.lastRFitSignature.clear();
            state.componentCount = components; state.autoFit = autoFit;
            if (enablingAutoFit && !state.components.empty())
                state.status = "Current results are up to date.";
            RefitDimensionality(id);
        });
        view->SetActionCallback([this, id](std::string const& kind)
        {
            OpenDimensionalityPlot(id, kind);
        });
        view->SetCommandCallback([this](std::vector<std::string> const& command)
            { HandleAnalysisViewCommand(command); });
        view->SetScopeChoices(AnalysisScopeChoicesFor(
            commandDispatcher_->applicationState(), found->second.group, true));
        dimensionalityViews_[id] = view;
        std::weak_ptr<DimensionalityView> weakView = view;
        if (applicationCommands_)
        {
            applicationCommands_->RegisterWindow({
                "dimensionality:" + id, to_hstring(::rlispstat::core::DimensionalityWindowTitle()).c_str(),
                found->second.group, "output", id,
                [weakView]() { if (auto current = weakView.lock()) current->Activate(); },
                [weakView]() { if (auto current = weakView.lock()) current->Close(); },
                {}, {}, {}, {}
            });
        }
        view->Show(found->second);
    }

    void App::HandleDimensionalityClosed(std::string const& id)
    {
        if (resettingViews_) return;
        if (applicationCommands_) applicationCommands_->UnregisterWindow("dimensionality:" + id);
        dimensionalityViews_.erase(id);
    }

    void App::ShowScaleAnalysis(std::string const& id)
    {
        if (!commandDispatcher_) return;
        auto& applicationState = commandDispatcher_->applicationState();
        auto found = applicationState.scaleAnalyses().find(id);
        if (found == applicationState.scaleAnalyses().end()) return;
        auto const* dataframe = applicationState.datasets().find(found->second.group);
        if (!dataframe) return;

        auto view = scaleAnalysisViews_.count(id) && scaleAnalysisViews_[id]
            ? scaleAnalysisViews_[id] : ScaleAnalysisView::Create();
        view->SetClosedCallback([this, id]() { HandleScaleAnalysisClosed(id); });
        view->SetCommandCallback([this](std::vector<std::string> const& command)
            { HandleAnalysisViewCommand(command); });
        std::vector<::rlispstat::core::ScaleItemSpecification> available;
        for (auto const& column : dataframe->columns)
        {
            if (!::rlispstat::core::ScaleColumnIsEligible(column)) continue;
            ::rlispstat::core::ScaleItemSpecification item;
            item.variable = column.name;
            item.type = ::rlispstat::core::DefaultScaleItemType(column);
            available.push_back(std::move(item));
        }
        view->SetAvailableItems(std::move(available));
        view->SetOpenScatterplotCallback([this, group = found->second.group](
            std::string const& xVariable, std::string const& yVariable)
        {
            ScatterplotSpecification specification;
            specification.group = group;
            specification.xVariable = xVariable;
            specification.yVariable = yVariable;
            specification.title = ::rlispstat::core::ScatterplotDefaultTitle(
                xVariable, yVariable);
            CreateScatterplot(specification);
        });
        view->SetChangedCallback([this, id](
            ::rlispstat::core::ScaleAnalysisSpecification const& requested)
        {
            if (!commandDispatcher_) return;
            auto& stateMap = commandDispatcher_->applicationState().scaleAnalyses();
            auto current = stateMap.find(id);
            if (current == stateMap.end()) return;
            auto const* currentData = commandDispatcher_->applicationState().datasets().find(
                current->second.group);
            if (!currentData) return;
            auto candidate = requested;
            candidate.datasetId = current->second.group;
            candidate.revision = current->second.specification.revision + 1;
            const auto previousFingerprint = current->second.specification.fingerprint;
            std::string error;
            if (!::rlispstat::core::ValidateScaleAnalysisSpecification(
                    *currentData, candidate, error))
            {
                current->second.status = error;
                ShowScaleAnalysis(id);
                return;
            }
            if (!previousFingerprint.empty() && candidate.fingerprint == previousFingerprint)
            {
                candidate.revision = current->second.specification.revision;
                current->second.specification = std::move(candidate);
                ShowScaleAnalysis(id);
                return;
            }
            current->second.specification = std::move(candidate);
            current->second.modelVersion = current->second.specification.revision;
            if (current->second.specification.items.size() < 2)
            {
                const auto backend = current->second.result.backend;
                const auto imputations = current->second.result.imputationCount;
                current->second.result = {};
                current->second.result.backend = backend;
                current->second.result.imputationCount = std::max(1, imputations);
                current->second.rFitPending = false;
                current->second.pendingRevision = -1;
                current->second.pendingFingerprint.clear();
                current->second.status = "Choose at least two numeric or ordinal items.";
                ShowScaleAnalysis(id);
                return;
            }
            if (!current->second.autoFit)
            {
                current->second.rFitPending = false;
                current->second.pendingRevision = -1;
                current->second.pendingFingerprint.clear();
                current->second.status =
                    ::rlispstat::core::ScaleAnalysisHasCompletedResult(current->second.result)
                    ? "Auto-fit is off. Displaying the last completed result for the previous scale specification."
                    : "Auto-fit is off. Turn it on to fit the current scale.";
                ShowScaleAnalysis(id);
                return;
            }
            RefitScaleAnalysis(id);
        });
        scaleAnalysisViews_[id] = view;
        std::weak_ptr<ScaleAnalysisView> weakView = view;
        if (applicationCommands_)
        {
            applicationCommands_->RegisterWindow({
                "scale-analysis:" + id,
                to_hstring(::rlispstat::core::ScaleAnalysisWindowTitle(
                    found->second.result.imputationCount > 1)).c_str(),
                found->second.group, "output", id,
                [weakView]() { if (auto current = weakView.lock()) current->Activate(); },
                [weakView]() { if (auto current = weakView.lock()) current->Close(); },
                {}, {}, {}, {}
            });
        }
        view->Show(found->second);
    }

    void App::HandleScaleAnalysisClosed(std::string const& id)
    {
        if (resettingViews_) return;
        if (applicationCommands_) applicationCommands_->UnregisterWindow("scale-analysis:" + id);
        scaleAnalysisViews_.erase(id);
    }

    bool App::RefitScaleAnalysis(std::string const& id)
    {
        if (!commandDispatcher_) return false;
        auto& applicationState = commandDispatcher_->applicationState();
        auto found = applicationState.scaleAnalyses().find(id);
        if (found == applicationState.scaleAnalyses().end()) return false;
        auto& state = found->second;
        if (!state.autoFit)
        {
            state.rFitPending = false;
            state.pendingRevision = -1;
            state.pendingFingerprint.clear();
            state.status = ::rlispstat::core::ScaleAnalysisHasCompletedResult(state.result)
                ? "Auto-fit is off. Displaying the last completed result for the previous scale specification."
                : "Auto-fit is off. Turn it on to fit the current scale.";
            ShowScaleAnalysis(id);
            return true;
        }
        commandDispatcher_->applicationState().captureAnalysisScope(state.group, state.dataScope, state.dataScopeCaptured);
        auto const* dataframe = applicationState.datasets().find(state.group);
        if (!dataframe) return false;
        std::string error;
        if (!::rlispstat::core::ValidateScaleAnalysisSpecification(
                *dataframe, state.specification, error))
        {
            state.rFitPending = false;
            state.pendingRevision = -1;
            state.pendingFingerprint.clear();
            state.status = error;
            ShowScaleAnalysis(id);
            return false;
        }
        if (state.specification.items.size() < 2)
        {
            state.rFitPending = false;
            state.pendingRevision = -1;
            state.pendingFingerprint.clear();
            state.status = "Choose at least two numeric or ordinal items.";
            ShowScaleAnalysis(id);
            return true;
        }
        if (state.rFitPending &&
            state.pendingRevision == state.specification.revision &&
            state.pendingFingerprint == state.specification.fingerprint)
            return true;

        ::rlispstat::core::MainRScaleAnalysisTask task;
        task.id = state.id;
        task.group = state.group;
        task.specification = state.specification;
        task.scope = state.dataScopeCaptured &&
            state.dataScope.kind == ::rlispstat::core::AnalysisScopeKind::ExplicitRowIds
            ? "selected" : "all";
        if (state.dataScopeCaptured &&
            state.dataScope.kind == ::rlispstat::core::AnalysisScopeKind::ExplicitRowIds)
            task.rows = ::rlispstat::core::ResolveAnalysisScopeRowIds(
                state.dataScope, state.dataScope.totalDatasetRows);
        state.rFitPending = true;
        state.pendingRevision = state.specification.revision;
        state.pendingFingerprint = state.specification.fingerprint;
        state.status = "Fitting Scale Analysis in the main R session...";
        ::rlispstat::core::QueueLatestMainRScaleAnalysisTask(
            pendingMainRTasks_.scaleAnalysisTasks, std::move(task));
        MarkMainRTaskPending();
        ShowScaleAnalysis(id);
        return true;
    }

    bool App::RefitDimensionality(std::string const& id)
    {
        if (!commandDispatcher_) return false;
        auto& states = commandDispatcher_->applicationState().dimensionalityModels();
        auto found = states.find(id);
        if (found == states.end()) return false;
        auto& state = found->second;
        if (auto task = ::rlispstat::core::PrepareDimensionalityRTask(
                commandDispatcher_->applicationState(), state)) {
            auto &queue = pendingMainRTasks_.dimensionalityTasks;
            queue.erase(std::remove_if(queue.begin(), queue.end(),
                [&](const auto &pending) { return pending.id == state.id; }), queue.end());
            queue.push_back(std::move(*task));
            MarkMainRTaskPending();
        }
        ShowDimensionality(id);
        RefreshDimensionalityPlots(id);
        return true;
    }

    void App::OpenDimensionalityPlot(std::string const& id, std::string const& kind)
    {
        if (!commandDispatcher_) return;
        auto found = commandDispatcher_->applicationState().dimensionalityModels().find(id);
        if (found == commandDispatcher_->applicationState().dimensionalityModels().end()) return;
        auto& state = found->second;
        auto model = std::make_unique<::rlispstat::core::PlotModel>(state.seed);
        model->id = (kind == "biplot" ? "biplot_windows_" : "scree_windows_") +
            std::to_string(static_cast<unsigned long long>(GetTickCount64()));
        model->kind = kind == "biplot" ? "pca_biplot" : "pca_scree";
        model->isDatasetSeed = false; model->dimensionalityModelId = id;
        model->interactionMode = kind == "biplot" ? "select" : "none";
        model->selectionMode = "replace";
        auto* pointer = model.get(); plots_[model->id] = std::move(model);
        RefreshDimensionalityPlots(id);
        const auto attached = AttachPlotModel(*pointer);
        if (!attached.accepted) { plots_.erase(pointer->id); return; }
        commandDispatcher_->plotCoordinator().activatePlot(pointer->id);
        ShowNativePlot(*pointer);
    }

    void App::RefreshDimensionalityPlots(std::string const& id)
    {
        if (!commandDispatcher_) return;
        auto found = commandDispatcher_->applicationState().dimensionalityModels().find(id);
        if (found == commandDispatcher_->applicationState().dimensionalityModels().end()) return;
        auto const& state = found->second;
        for (auto& [plotId, model] : plots_)
        {
            if (!model || model->dimensionalityModelId != id) continue;
            model->points.clear(); model->screeParallelPoints.clear();
            model->biplotLoadings.clear(); model->biplotComponentOptions.clear();
            if (state.components.empty()) {
                model->title=state.status;
                auto view=views_.find(plotId);
                if (view!=views_.end() && view->second) view->second->Show(*model);
                continue;
            }
            if (model->kind == "pca_scree")
            {
                auto scree = ::rlispstat::core::BuildDimensionalityScreePlotState(
                    state.components, state.method, state.componentCount, state.rotation);
                if (!scree.ok) continue;
                model->title = scree.title; model->xLabel = scree.xLabel; model->yLabel = scree.yLabel;
                model->points.clear(); model->screeParallelPoints.clear();
                for (auto const& point : scree.observed) model->points.push_back({point.x, point.y, point.rowId});
                for (auto const& point : scree.parallel) model->screeParallelPoints.push_back({point.x, point.y, point.rowId});
                model->xmin = model->dataXmin = scree.xmin; model->xmax = model->dataXmax = scree.xmax;
                model->ymin = model->dataYmin = scree.ymin; model->ymax = model->dataYmax = scree.ymax;
            }
            else if (model->kind == "pca_biplot")
            {
                auto biplot = ::rlispstat::core::BuildDimensionalityBiplotPlotState(
                    state.components, state.loadings, state.scores, state.method,
                    model->biplotXComponent, model->biplotYComponent);
                if (!biplot.ok) continue;
                model->biplotXComponent = biplot.xComponent; model->biplotYComponent = biplot.yComponent;
                model->biplotComponentOptions.clear();
                for (auto const& component : state.components)
                    model->biplotComponentOptions.push_back({component.index,
                        ::rlispstat::core::DimensionalityComponentMenuLabel(
                            state.method, component.index, component.variance)});
                model->title = biplot.title; model->xLabel = biplot.xLabel; model->yLabel = biplot.yLabel;
                model->points.clear(); model->biplotLoadings.clear();
                for (auto const& point : biplot.scores) model->points.push_back({point.x, point.y, point.rowId});
                for (auto const& loading : biplot.loadings)
                    model->biplotLoadings.push_back({loading.variable, loading.x, loading.y});
                model->dataXmin = biplot.dataXmin; model->dataXmax = biplot.dataXmax;
                model->dataYmin = biplot.dataYmin; model->dataYmax = biplot.dataYmax;
                model->xmin = biplot.xmin; model->xmax = biplot.xmax;
                model->ymin = biplot.ymin; model->ymax = biplot.ymax;
            }
            auto view = views_.find(plotId);
            if (view != views_.end() && view->second) view->second->Show(*model);
        }
    }

    void App::QueueCorrelationFit(::rlispstat::core::CorrelationMatrixState& state)
    {
        auto task=::rlispstat::core::PrepareCorrelationRTask(commandDispatcher_->applicationState(),state);
        auto &queue=pendingMainRTasks_.correlationTasks;
        queue.erase(std::remove_if(queue.begin(),queue.end(),
            [&](const auto &pending) { return pending.id==state.id; }),queue.end());
        if (state.rFitPending) { queue.push_back(std::move(task)); MarkMainRTaskPending(); }
    }

    bool App::RefitDendrogram(std::string const& id)
    {
        if (!commandDispatcher_) return false;
        auto& states=commandDispatcher_->applicationState().dendrograms();auto found=states.find(id);if(found==states.end())return false;
        auto& state=found->second;
        auto task = ::rlispstat::core::PrepareDendrogramRTask(commandDispatcher_->applicationState(), state);
        auto &queue = pendingMainRTasks_.dendrogramTasks;
        queue.erase(std::remove_if(queue.begin(), queue.end(),
            [&](const auto &pending) { return pending.id == id; }), queue.end());
        if (state.rFitPending) { queue.push_back(std::move(task)); MarkMainRTaskPending(); }
        ShowDendrogram(id);
        return true;
    }

    void App::ShowDendrogram(std::string const& id)
    {
        if(!commandDispatcher_)return;auto found=commandDispatcher_->applicationState().dendrograms().find(id);if(found==commandDispatcher_->applicationState().dendrograms().end())return;
        ::rlispstat::core::RefreshDendrogramCodeReference(
            commandDispatcher_->applicationState(), found->second);
        auto& dendrogramState=found->second;auto const* dataframe=commandDispatcher_->applicationState().datasets().find(dendrogramState.group);if(dataframe&&!dendrogramState.labelVariableConfigured){const auto label=commandDispatcher_->applicationState().labelColumn(dendrogramState.group);if(!label.empty())::rlispstat::core::SetDendrogramLabelVariable(dendrogramState,*dataframe,label);}
        auto view=dendrogramViews_.count(id)&&dendrogramViews_[id]?dendrogramViews_[id]:DendrogramView::Create();dendrogramViews_[id]=view;
        view->SetClosedCallback([this,id](){HandleDendrogramClosed(id);});
        view->SetChangedCallback([this,id](std::string const& linkage,std::string const& missing){if(!commandDispatcher_)return;auto found=commandDispatcher_->applicationState().dendrograms().find(id);if(found==commandDispatcher_->applicationState().dendrograms().end())return;found->second.linkage=linkage;found->second.missingMode=missing;RefitDendrogram(id);});
        view->SetSelectRowCallback([this,group=found->second.group](int row){ApplySelectedRows(group,{row},::rlispstat::core::SelectionMode::Toggle);});
        view->SetSelectRowsCallback([this,group=found->second.group](std::vector<int> const& rows,std::string const&){ApplySelectedRows(group,std::set<int>(rows.begin(),rows.end()),::rlispstat::core::SelectionMode::Replace);});
        view->SetCommandCallback([this](std::vector<std::string> const& command){HandleAnalysisViewCommand(command);});
        view->SetVisualTheme(commandDispatcher_->plotTheme());std::set<int> selected;commandDispatcher_->applicationState().selectedRows(found->second.group,selected);std::vector<std::string> available;if(dataframe)for(auto const& column:dataframe->columns)available.push_back(column.name);view->Show(found->second,selected,commandDispatcher_->applicationState().displayPointColors(found->second.group),available);
        std::weak_ptr<DendrogramView> weakView=view;if(applicationCommands_)applicationCommands_->RegisterWindow({"dendrogram:"+id,to_hstring(::rlispstat::core::DendrogramWindowTitle()).c_str(),found->second.group,"output",id,[weakView](){if(auto current=weakView.lock())current->Activate();},[weakView](){if(auto current=weakView.lock())current->Close();},{},{},{},{}});
    }

    void App::HandleDendrogramClosed(std::string const& id)
    {
      if (resettingViews_)
        return;
      if (applicationCommands_)
        applicationCommands_->UnregisterWindow("dendrogram:" + id);
      dendrogramViews_.erase(id);
    }

    bool App::RequestGeneralizedModelFit(std::string const &id) {
      if (!commandDispatcher_)
        return false;
      auto found =
          commandDispatcher_->applicationState().generalizedGLMs().find(id);
      if (found ==
          commandDispatcher_->applicationState().generalizedGLMs().end())
        return false;
      auto &state = found->second;
      state.frozenScopeNotice.clear();
        commandDispatcher_->applicationState().captureAnalysisScope(state.group, state.dataScope, state.dataScopeCaptured, &state.scope);
      if (state.countRegression &&
          state.countDistribution == ::rlispstat::core::CountDistribution::HurdleBetaBinomialCeiling) {
        ::rlispstat::core::ClearGeneralizedGLMFitResults(state);
        RefreshRegressionDerivedOutputs(id);
      }
      if (auto const *dataframe =
              commandDispatcher_->applicationState().datasets().find(state.group)) {
        ::rlispstat::core::SynchronizeModelSpecificationTermTypes(state, *dataframe);
      }
      if (state.response.empty() ||
          (!state.binaryRegression && !state.countRegression && state.terms.empty())) {
        auto &tasks = pendingMainRTasks_.generalizedGLMTasks;
        tasks.erase(std::remove_if(tasks.begin(), tasks.end(),
          [&](::rlispstat::core::MainRGeneralizedGLMTask const& task) {
            return task.id == state.id;
          }), tasks.end());
        state.rFitPending = false;
        state.lastRFitSignature.clear();
        state.ok = false;
        state.rows.clear(); state.termTests.clear(); state.pairwise.clear();
        state.diagnostics.clear(); state.warnings.clear();
        state.n = 0; state.excluded = 0;
        state.status = state.response.empty()
          ? "Choose a response variable before fitting."
          : "Add at least one independent variable before fitting.";
        ShowGeneralizedModel(id);
        return true;
      }
      if (::rlispstat::core::GeneralizedFamilyRequiresResponseBounds(state.family) &&
          (!state.responseBoundsConfigured || !std::isfinite(state.responseLower) ||
           !std::isfinite(state.responseUpper) || state.responseLower >= state.responseUpper)) {
        state.rFitPending = false;
        state.lastRFitSignature.clear();
        state.ok = false;
        state.status = "Enter finite response bounds with lower < upper before fitting.";
        ShowGeneralizedModel(id);
        return true;
      }
      if (state.countRegression &&
          ::rlispstat::core::CountDistributionUsesTrials(state.countDistribution) &&
          state.trialsVariable.empty() &&
          (!std::isfinite(state.trialsConstant) || state.trialsConstant <= 0.0 ||
           std::floor(state.trialsConstant) != state.trialsConstant)) {
        state.rFitPending = false;
        state.lastRFitSignature.clear();
        state.ok = false;
        state.status = "Enter Trials as a fixed positive integer before fitting this bounded-count model.";
        ShowGeneralizedModel(id);
        return true;
      }
      const std::string effectiveDistribution = state.countRegression
        ? ::rlispstat::core::CountDistributionId(state.countDistribution)
        : state.family;
      if (state.modelType != ::rlispstat::core::StatisticalModelType::LegacyGeneralized &&
          !::rlispstat::core::DistributionIsAvailableForModel(
              state.modelType, effectiveDistribution)) {
        state.rFitPending = false;
        state.lastRFitSignature.clear();
        state.ok = false;
        state.status = "The selected distribution is not available for " +
          ::rlispstat::core::StatisticalModelTypeLabel(state.modelType) + ".";
        ShowGeneralizedModel(id);
        return true;
      }
      if (state.dataScopeCaptured &&
          state.dataScope.kind == ::rlispstat::core::AnalysisScopeKind::ExplicitRowIds &&
          state.dataScope.originalRowIds.empty()) {
        state.rFitPending = false;
        state.lastRFitSignature.clear();
        state.ok = false;
        state.status = "The analysis scope contains no rows.";
        ShowGeneralizedModel(id);
        return true;
      }
      if (auto const *dataframe =
              commandDispatcher_->applicationState().datasets().find(state.group)) {
        auto const *responseColumn = ::rlispstat::core::FindDataColumnInDataFrame(
            *dataframe, state.response);
        std::set<int> validationRows;
        if (state.dataScopeCaptured &&
            state.dataScope.kind == ::rlispstat::core::AnalysisScopeKind::ExplicitRowIds) {
          validationRows.insert(state.dataScope.originalRowIds.begin(),
                                state.dataScope.originalRowIds.end());
        }
        ::rlispstat::core::ResponseDomainValidation validation;
        if (!responseColumn) {
          validation.status = "The response variable was not found in the analysis dataset.";
        } else if (state.modelType == ::rlispstat::core::StatisticalModelType::Count) {
          auto const count = ::rlispstat::core::InspectCountResponse(
              *responseColumn, validationRows);
          validation.ok = count.ok;
          validation.status = count.status;
        } else if (state.modelType == ::rlispstat::core::StatisticalModelType::Binary) {
          validation = ::rlispstat::core::InspectResponseDomain(
              *responseColumn, ::rlispstat::core::ResponseDomain::Binary,
              validationRows);
        } else if (state.modelType ==
                   ::rlispstat::core::StatisticalModelType::PositiveContinuous) {
          validation = ::rlispstat::core::InspectResponseDomain(
              *responseColumn,
              ::rlispstat::core::ResponseDomain::StrictlyPositiveContinuous,
              validationRows);
        } else if (state.modelType ==
                   ::rlispstat::core::StatisticalModelType::Proportion) {
          validation = ::rlispstat::core::InspectResponseDomain(
              *responseColumn,
              state.family == "beta"
                ? ::rlispstat::core::ResponseDomain::OpenUnitInterval
                : ::rlispstat::core::ResponseDomain::OpenClosedUnitInterval,
              validationRows);
        } else {
          validation.ok = true;
        }
        if (!validation.ok) {
          state.rFitPending = false;
          state.lastRFitSignature.clear();
          state.ok = false;
          state.status = validation.status.empty()
            ? "The response is not compatible with the selected model."
            : validation.status;
          ShowGeneralizedModel(id);
          return true;
        }
        if (state.countRegression && !state.exposure.empty()) {
          auto const *exposureColumn = ::rlispstat::core::FindDataColumnInDataFrame(
              *dataframe, state.exposure);
          auto const exposureValidation = exposureColumn
            ? ::rlispstat::core::InspectCountExposure(*exposureColumn, validationRows)
            : ::rlispstat::core::CountExposureValidation{};
          if (!exposureColumn || !exposureValidation.ok) {
            state.rFitPending = false;
            state.lastRFitSignature.clear();
            state.ok = false;
            state.status = exposureColumn && !exposureValidation.status.empty()
              ? exposureValidation.status
              : "The exposure variable was not found in the analysis dataset.";
            ShowGeneralizedModel(id);
            return true;
          }
        }
        if (!state.offsetVariable.empty()) {
          if (state.countRegression && state.offsetVariable == state.exposure) {
            state.rFitPending = false;
            state.lastRFitSignature.clear();
            state.ok = false;
            state.status =
              "The same variable cannot be both Exposure and Link-scale offset. "
              "Exposure is transformed with log(); Link-scale offset is used as stored.";
            ShowGeneralizedModel(id);
            return true;
          }
          auto const *offsetColumn = ::rlispstat::core::FindDataColumnInDataFrame(
              *dataframe, state.offsetVariable);
          auto const offsetValidation = offsetColumn
            ? ::rlispstat::core::InspectLinkOffset(*offsetColumn, validationRows)
            : ::rlispstat::core::LinkOffsetValidation{};
          if (!offsetColumn || !offsetValidation.ok) {
            state.rFitPending = false;
            state.lastRFitSignature.clear();
            state.ok = false;
            state.status = offsetColumn && !offsetValidation.status.empty()
              ? offsetValidation.status
              : "The link-scale offset variable was not found in the analysis dataset.";
            ShowGeneralizedModel(id);
            return true;
          }
        }
      }
      const std::string signature =
          ::rlispstat::core::GeneralizedGLMFitSignature(state);
      if (state.rFitPending && state.lastRFitSignature == signature)
        return true;
      state.rFitPending = true;
      state.lastRFitSignature = signature;
      ::rlispstat::core::MainRGeneralizedGLMTask task;
      task.id = state.id;
      task.generation = ++state.rFitGeneration;
      task.group = state.group;
      task.response = state.response;
      task.family = state.family;
      task.link = state.link;
      task.modelType = ::rlispstat::core::StatisticalModelTypeId(state.modelType);
      task.responseBoundsConfigured = state.responseBoundsConfigured;
      task.responseLower = state.responseLower;
      task.responseUpper = state.responseUpper;
      task.scope = state.scope;
      task.terms = state.terms;
      task.termTypes = ::rlispstat::core::EffectiveModelSpecificationTermTypes(state);
      task.centeredPredictors.assign(state.centeredPredictors.begin(),
                                     state.centeredPredictors.end());
      task.factorReferenceLevels = state.factorReferenceLevels;
      task.binaryRegression = state.binaryRegression;
      task.countRegression = state.countRegression;
      task.countDistribution = ::rlispstat::core::CountDistributionId(state.countDistribution);
      task.exposure = state.exposure;
      task.offsetVariable = state.offsetVariable;
      task.trialsVariable = state.trialsVariable;
      task.trialsConstant = state.trialsConstant;
      task.eventValue = state.responseCoding.eventValue;
      task.referenceValue = state.responseCoding.referenceValue;
      std::set<int> selection;
      commandDispatcher_->applicationState().selectedRows(state.group, selection);
      ::rlispstat::core::SetGeneralizedTaskAnalysisScope(task, state.dataScope,
          state.dataScopeCaptured, selection);
      pendingMainRTasks_.generalizedGLMTasks.push_back(std::move(task));
      MarkMainRTaskPending();
      ShowGeneralizedModel(id);
      return true;
    }

    void App::ShowGeneralizedModel(std::string const &id) {
      if (!commandDispatcher_)
        return;
      auto found =
          commandDispatcher_->applicationState().generalizedGLMs().find(id);
      if (found ==
          commandDispatcher_->applicationState().generalizedGLMs().end())
        return;
      auto view = generalizedModelViews_.count(id) && generalizedModelViews_[id]
                      ? generalizedModelViews_[id]
                      : GeneralizedModelView::Create();
      generalizedModelViews_[id] = view;
      view->SetClosedCallback(
          [this, id]() { HandleGeneralizedModelClosed(id); });
      view->SetDiagnosticCallback([this, id](std::string const &kind) {
        OpenGeneralizedModelDiagnostic(id, kind);
      });
      std::vector<std::string> responses;
      std::vector<std::string> predictors;
      std::vector<std::string> exposures;
      if (auto const *dataframe =
              commandDispatcher_->applicationState().datasets().find(
                  found->second.group)) {
        for (auto const &column : dataframe->columns) {
          if (::rlispstat::core::DataColumnAllMissing(column))
            continue;
          predictors.push_back(column.name);
          if (found->second.countRegression &&
              column.name != found->second.response &&
              ::rlispstat::core::InspectCountExposure(column).ok)
            exposures.push_back(column.name);
          if (found->second.binaryRegression) {
            if (::rlispstat::core::InspectBinaryResponse(column).ok)
              responses.push_back(column.name);
          } else if (found->second.countRegression) {
            if (::rlispstat::core::InspectCountResponse(column).ok)
              responses.push_back(column.name);
          } else if (column.type == "numeric") {
            responses.push_back(column.name);
          }
        }
      }
      view->SetScopeChoices(AnalysisScopeChoicesFor(
          commandDispatcher_->applicationState(), found->second.group, true));
      view->SetCommandCallback(
          [this](std::vector<std::string> const &command) {
            HandleAnalysisViewCommand(command);
          },
          std::move(responses), std::move(predictors), std::move(exposures));
      std::weak_ptr<GeneralizedModelView> weakView = view;
      if (applicationCommands_)
        applicationCommands_->RegisterWindow(
            {"generalized:" + id,
             to_hstring(found->second.title.empty() ? "Generalized Linear Model"
                                                    : found->second.title)
                 .c_str(),
             found->second.group,
             "output",
             {},
             [weakView]() {
               if (auto current = weakView.lock())
                 current->Activate();
             },
             [weakView]() {
               if (auto current = weakView.lock())
                 current->Close();
             },
             {},
             {},
             {},
             {}});
      // Registering a new window rebuilds the Window menus.  Do that while
      // the invoking data-sheet menu still owns the interaction, then present
      // the output last.  Activating the output first allowed the menu teardown
      // to return the data sheet to the foreground immediately afterwards.
      view->Show(found->second);
      RefreshRegressionDerivedOutputs(id);
      RefreshDiagnosticPlots();
    }
    void App::HandleGeneralizedModelClosed(std::string const& id){if(resettingViews_)return;if(applicationCommands_)applicationCommands_->UnregisterWindow("generalized:"+id);generalizedModelViews_.erase(id);}

    void App::ShowMixedModel(::rlispstat::core::NativeMixedModelState const& state)
    {
        ShowMixedModelText(state.id,state.group,state.modelType,state.reportText);
    }
    void App::ShowMixedModelText(std::string const& id,std::string const& group,std::string const& modelType,std::string const& text)
    {
        auto report = ::rlispstat::core::ParseMixedModelReport(id, group, modelType, text);
        auto view = mixedModelViews_.count(id) && mixedModelViews_[id]
            ? mixedModelViews_[id] : MixedModelView::Create();
        mixedModelViews_[id] = view;
        view->SetClosedCallback([this,id](){HandleMixedModelClosed(id);});
        if (commandDispatcher_)
        {
            auto native = commandDispatcher_->applicationState().nativeMixedModels().find(id);
            auto const* dataframe = commandDispatcher_->applicationState().datasets().find(group);
            if (native != commandDispatcher_->applicationState().nativeMixedModels().end() && dataframe)
            {
                std::vector<std::string> responses, fixed, grouping, polynomial;
                auto isRandomGroup = [&](std::string const& name)
                {
                    return std::any_of(native->second.randomEffects.begin(), native->second.randomEffects.end(),
                        [&](auto const& spec) { return spec.group == name; });
                };
                const bool generalized = native->second.modelType == "generalized_linear_mixed_model";
                for (auto const& column : dataframe->columns)
                {
                    if (::rlispstat::core::DataColumnLooksLikeId(column) ||
                        ::rlispstat::core::DataColumnAllMissing(column)) continue;
                    if (!isRandomGroup(column.name) &&
                        ((!generalized && column.type == "numeric") ||
                         (generalized && (column.type == "numeric" ||
                             ::rlispstat::core::DataColumnLooksBinaryNumeric(column)))))
                        responses.push_back(column.name);
                    if (column.type == "numeric" && ::rlispstat::core::DataColumnAllowsNumeric(column) && !isRandomGroup(column.name))
                        polynomial.push_back(column.name);
                    if (column.name != native->second.response && !isRandomGroup(column.name) &&
                        ::rlispstat::core::DataColumnObservedLevelCount(column) >= 2)
                        fixed.push_back(column.name);
                    if (column.name != native->second.response &&
                        ::rlispstat::core::DataColumnLooksGroupingCandidate(column, dataframe->rows))
                        grouping.push_back(column.name);
                }
                view->SetCommandCallback([this](std::vector<std::string> const& command)
                    { HandleAnalysisViewCommand(command); }, native->second,
                    std::move(responses), std::move(fixed), std::move(grouping), std::move(polynomial));
            }
        }
        view->Show(report);
        std::weak_ptr<MixedModelView> weakView=view;if(applicationCommands_)applicationCommands_->RegisterWindow({"mixed:"+id,to_hstring(report.title).c_str(),group,"output",id,[weakView](){if(auto current=weakView.lock())current->Activate();},[weakView](){if(auto current=weakView.lock())current->Close();},{},{},{},{}});
    }
    void App::HandleMixedModelClosed(std::string const& id){if(resettingViews_)return;if(applicationCommands_)applicationCommands_->UnregisterWindow("mixed:"+id);mixedModelViews_.erase(id);}

    void App::QueueRegressionComparison(::rlispstat::core::RegressionComparisonState& state)
    {
        state.frozenScopeNotice.clear();
        if (commandDispatcher_) commandDispatcher_->applicationState().captureAnalysisScope(state.group, state.dataScope, state.dataScopeCaptured, &state.scope);
        if(state.response.empty()||state.models.empty()){state.rFitPending=false;state.lastRFitSignature.clear();for(auto& model:state.models){model.fit={};model.isStale=true;model.fitState=::rlispstat::core::RegressionComparisonFitState::NotFitted;}return;}
        if(commandDispatcher_){if(auto const* dataframe=commandDispatcher_->applicationState().datasets().find(state.group)){const bool datasetChanged=::rlispstat::core::SynchronizeRegressionComparisonDatasetIdentity(state,*dataframe);const bool specificationChanged=::rlispstat::core::SynchronizeRegressionComparisonTermTypes(state,*dataframe);if(datasetChanged||specificationChanged)for(auto& model:state.models)::rlispstat::core::InvalidateRegressionComparisonModel(state,model);}}
        const std::string signature=::rlispstat::core::RegressionComparisonFitSignature(state);if(state.rFitPending&&state.lastRFitSignature==signature)return;state.rFitPending=true;state.lastRFitSignature=signature;
        ::rlispstat::core::MainRRegressionComparisonTask task;task.id=state.id;task.generation=++state.rFitGeneration;task.group=state.group;task.response=state.response;task.scope=state.scope;task.autoRefit=state.autoRefit;task.datasetType=state.datasetType;task.imputationSetId=state.imputationSetId;task.sourceDatasetId=state.sourceDatasetId;task.imputationCount=state.imputationCount;task.termRows=state.termRows;task.termTypes=state.termTypes;
        for(auto& model:state.models)
        {
            model.fitState=::rlispstat::core::RegressionComparisonFitState::Pending;model.isStale=true;
            const auto specification=::rlispstat::core::EffectiveRegressionComparisonModelSpecification(state,model);
            ::rlispstat::core::MainRRegressionComparisonModelTask item;item.id=model.id;item.label=model.label;
            item.response=specification.response;item.terms=specification.terms;item.termTypes=specification.termTypes;
            item.centeredPredictors.assign(specification.centeredPredictors.begin(),specification.centeredPredictors.end());
            item.factorReferenceLevels=specification.factorReferenceLevels;
            task.models.push_back(std::move(item));
        }
        if(state.dataScopeCaptured&&state.dataScope.kind==::rlispstat::core::AnalysisScopeKind::ExplicitRowIds)task.rows=state.dataScope.originalRowIds;
        auto& pending=pendingMainRTasks_.regressionComparisonTasks;
        pending.erase(std::remove_if(pending.begin(),pending.end(),[&](auto const& queued){return queued.id==state.id;}),pending.end());
        pending.push_back(std::move(task));MarkMainRTaskPending();
    }

    void App::ShowRegressionComparison(std::string const& id)
    {
        if(!commandDispatcher_)return;auto found=commandDispatcher_->applicationState().regressionComparisons().find(id);if(found==commandDispatcher_->applicationState().regressionComparisons().end())return;auto view=regressionComparisonViews_.count(id)&&regressionComparisonViews_[id]?regressionComparisonViews_[id]:RegressionComparisonView::Create();regressionComparisonViews_[id]=view;view->SetClosedCallback([this,id](){HandleRegressionComparisonClosed(id);});
        auto const* dataframe=commandDispatcher_->applicationState().datasets().find(found->second.group);auto available=::rlispstat::core::AvailableVariableNames(found->second.seed,dataframe);auto numeric=::rlispstat::core::NumericVariableNames(&found->second.seed);
        view->SetScopeChoices(AnalysisScopeChoicesFor(commandDispatcher_->applicationState(),found->second.group,true));
        view->SetCommandCallback([this](std::vector<std::string> const& command){HandleAnalysisViewCommand(command);},[this,id](int modelIndex,std::string const& kind){OpenRegressionComparisonDiagnostic(id,modelIndex,kind);},std::move(numeric),std::move(available));
        auto displayState=found->second;
        displayState.note=::rlispstat::core::RegressionComparisonMultipleImputationNote(
            found->second,dataframe);
        view->Show(displayState);std::weak_ptr<RegressionComparisonView> weakView=view;if(applicationCommands_)applicationCommands_->RegisterWindow({"regression-comparison:"+id,to_hstring(displayState.title.empty()?"Compare Linear Models":displayState.title).c_str(),found->second.group,"output",id,[weakView](){if(auto current=weakView.lock())current->Activate();},[weakView](){if(auto current=weakView.lock())current->Close();},{},{},{},{}});for(auto const& sourceModel:found->second.models)RefreshRegressionDerivedOutputs(sourceModel.id);RefreshDiagnosticPlots();
    }
    void App::HandleRegressionComparisonClosed(std::string const& id){if(resettingViews_)return;if(applicationCommands_)applicationCommands_->UnregisterWindow("regression-comparison:"+id);regressionComparisonViews_.erase(id);}

    std::string App::CreateModelTrellis(std::string const& group,
                                        std::string const& response,
                                        std::vector<std::string> const& terms,
                                        std::string const& rowCondition,
                                        std::string const& columnCondition,
                                        std::string const& pAdjustment,
                                        std::string const& requestedScope)
    {
        if(!commandDispatcher_)return {};auto const* data=commandDispatcher_->applicationState().datasets().find(group);if(!data)return {};
        ::rlispstat::core::PlotModel seed;::rlispstat::core::PopulateDatasetSeedPlot(seed,*data,group);
        std::vector<std::string> numeric;for(auto const& variable:seed.variables)numeric.push_back(variable.name);if(numeric.empty()||std::find(numeric.begin(),numeric.end(),response)==numeric.end())return {};
        ::rlispstat::core::ModelTrellisState state;state.id="model_trellis_windows_"+std::to_string(static_cast<unsigned long long>(GetTickCount64()));state.sourceModelGroup=group;state.specification.baseModel.group=group;state.specification.baseModel.response=response;state.specification.panelContent=::rlispstat::core::ModelTrellisPanelContent::CompactSummary;
        std::set<int> analysisRows;
        if(::rlispstat::core::SavedAnalysisScopeNameFromChoiceValue(requestedScope))
        {
            std::string scopeError;
            if(!commandDispatcher_->applicationState().resolveSavedAnalysisScopeChoice(
                    group,requestedScope,state.dataScope,&scopeError))return {};
            state.dataScopeCaptured=true;
            state.specification.baseModel.scope="selected";
            analysisRows.insert(state.dataScope.originalRowIds.begin(),state.dataScope.originalRowIds.end());
        }
        else
        {
            state.specification.baseModel.scope=requestedScope=="selected"||requestedScope=="unselected"
                ?requestedScope:"all";
            if(state.specification.baseModel.scope!="all")
                commandDispatcher_->applicationState().selectedRows(group,analysisRows);
        }
        std::string error;for(auto const& term:terms)if(!::rlispstat::core::AddModelTrellisIndependentVariable(state.specification,term,*data,&error))return {};
        const auto categorical=::rlispstat::core::ModelTrellisCategoricalVariables(*data,state.specification);if(categorical.empty()||(rowCondition.empty()&&columnCondition.empty()))return {};
        const std::string selectedColumn=columnCondition;
        if(!rowCondition.empty()&&!::rlispstat::core::SetModelTrellisConditioningVariable(state.specification,::rlispstat::core::ModelTrellisDimension::Rows,rowCondition,*data,&error))return {};
        if(!selectedColumn.empty()&&!::rlispstat::core::SetModelTrellisConditioningVariable(state.specification,::rlispstat::core::ModelTrellisDimension::Columns,selectedColumn,*data,&error))return {};
        if(pAdjustment=="none")state.specification.pAdjustment=::rlispstat::core::ModelTrellisPAdjustment::None;else if(pAdjustment=="bonferroni")state.specification.pAdjustment=::rlispstat::core::ModelTrellisPAdjustment::Bonferroni;else state.specification.pAdjustment=::rlispstat::core::ModelTrellisPAdjustment::Holm;
        state.panels=::rlispstat::core::BuildModelTrellisPanels(state.specification,*data,analysisRows);state.status="Waiting for R panel fits...";const std::string id=state.id;commandDispatcher_->applicationState().modelTrellises()[id]=std::move(state);ShowModelTrellis(id);QueueModelTrellisFit(commandDispatcher_->applicationState().modelTrellises()[id]);return id;
    }

    void App::QueueModelTrellisFit(::rlispstat::core::ModelTrellisState& state)
    {
        if (!commandDispatcher_) return;
        auto& app = commandDispatcher_->applicationState();
        app.captureAnalysisScope(state.specification.baseModel.group,state.dataScope,state.dataScopeCaptured,&state.specification.baseModel.scope);
        const auto* data = app.datasets().find(state.specification.baseModel.group);
        if (!data) return;
        const auto rows = app.resolveActiveAnalysisRowIds(data->group);
        state.panels = ::rlispstat::core::BuildModelTrellisPanels(state.specification,*data,std::set<int>(rows.begin(),rows.end()));

        std::vector<std::string> omitted;auto effective=::rlispstat::core::EffectiveModelTrellisTerms(state.specification,&omitted);::rlispstat::core::MainRModelTrellisTask task;task.id=state.id;task.generation=++state.specificationGeneration;task.group=state.specification.baseModel.group;task.response=state.specification.baseModel.response;task.scope=state.specification.baseModel.scope;task.confidenceLevel=state.specification.baseModel.confidenceLevel;task.pAdjustment=::rlispstat::core::ModelTrellisPAdjustmentId(state.specification.pAdjustment);task.baseTerms=state.specification.baseModel.terms;task.effectiveTerms=std::move(effective);task.omittedTerms=std::move(omitted);task.termTypes=state.specification.baseModel.termTypes;
        for(auto const& panel:state.panels){::rlispstat::core::MainRModelTrellisPanelTask item;item.panelId=panel.panelId;item.rowLevelId=panel.key.rowLevelId.value_or("");item.rowLevelLabel=panel.rowLevelLabel;item.columnLevelId=panel.key.columnLevelId.value_or("");item.columnLevelLabel=panel.columnLevelLabel;item.rows=panel.candidateOriginalRows;task.panels.push_back(std::move(item));}
        state.fitPending=true;state.status="Waiting for R panel fits...";pendingMainRTasks_.modelTrellisTasks.push_back(std::move(task));MarkMainRTaskPending();
    }

    void App::ShowModelTrellis(std::string const& id)
    {
        if(!commandDispatcher_)return;auto found=commandDispatcher_->applicationState().modelTrellises().find(id);if(found==commandDispatcher_->applicationState().modelTrellises().end())return;auto view=modelTrellisViews_.count(id)&&modelTrellisViews_[id]?modelTrellisViews_[id]:ModelTrellisView::Create();modelTrellisViews_[id]=view;view->SetClosedCallback([this,id](){HandleModelTrellisClosed(id);});auto const* dataframe=commandDispatcher_->applicationState().datasets().find(found->second.specification.baseModel.group);if(dataframe)view->SetCommandCallback([this](std::vector<std::string> const& command){HandleAnalysisViewCommand(command);},::rlispstat::core::ModelTrellisIndependentVariables(*dataframe,found->second.specification),::rlispstat::core::ModelTrellisCategoricalVariables(*dataframe,found->second.specification));view->Show(found->second);std::weak_ptr<ModelTrellisView> weakView=view;if(applicationCommands_)applicationCommands_->RegisterWindow({"model-trellis:"+id,L"Linear Model Trellis",found->second.specification.baseModel.group,"output",id,[weakView](){if(auto current=weakView.lock())current->Activate();},[weakView](){if(auto current=weakView.lock())current->Close();},{},{},{},{},{}});
    }
    void App::HandleModelTrellisClosed(std::string const& id){if(resettingViews_)return;if(applicationCommands_)applicationCommands_->UnregisterWindow("model-trellis:"+id);modelTrellisViews_.erase(id);}
    void App::QueueGeneralizedComparison(::rlispstat::core::GeneralizedComparisonState& state)
    {
        state.frozenScopeNotice.clear();
        if (commandDispatcher_) commandDispatcher_->applicationState().captureAnalysisScope(state.group, state.dataScope, state.dataScopeCaptured, &state.scope);
        if(state.response.empty()||state.models.empty())
        {for(auto& model:state.models){model.fit={};model.isStale=false;model.fitState=::rlispstat::core::RegressionComparisonFitState::NotFitted;}return;}
        if(auto const* dataframe=commandDispatcher_->applicationState().datasets().find(state.group))
        {
            const bool datasetChanged=::rlispstat::core::SynchronizeGeneralizedComparisonDatasetIdentity(state,*dataframe);
            for(auto& model:state.models)
            {
                const bool specificationChanged=::rlispstat::core::SynchronizeModelSpecificationTermTypes(model,*dataframe);
                if(datasetChanged||specificationChanged)
                    ::rlispstat::core::InvalidateGeneralizedComparisonModel(state,model);
            }
        }
        ::rlispstat::core::MainRGeneralizedComparisonTask task;
        task.id=state.id;task.group=state.group;task.response=state.response;task.family=state.family;
        task.modelType=::rlispstat::core::StatisticalModelTypeId(state.modelType);
        task.link=state.link;task.scope=state.scope;task.binaryComparison=state.binaryComparison;
        task.countComparison=state.countComparison;
        task.generation=++state.rFitGeneration;task.autoRefit=state.autoRefit;
        task.eventValue=state.responseCoding.eventValue.empty()?state.responseCoding.eventLabel:state.responseCoding.eventValue;
        task.referenceValue=state.responseCoding.referenceValue.empty()?state.responseCoding.referenceLabel:state.responseCoding.referenceValue;
        task.datasetType=state.datasetType;task.imputationSetId=state.imputationSetId;
        task.sourceDatasetId=state.sourceDatasetId;task.imputationCount=state.imputationCount;
        task.termTypes=state.termTypes;
        for(auto& model:state.models){const auto specification=::rlispstat::core::EffectiveGeneralizedComparisonModelSpecification(state,model);::rlispstat::core::MainRGeneralizedComparisonModelTask item;item.id=model.id;item.label=model.label;item.response=specification.response;item.family=model.family.empty()?state.family:model.family;item.link=model.link.empty()?state.link:model.link;item.scope=specification.scope.empty()?state.scope:specification.scope;item.responseBoundsConfigured=model.responseBoundsConfigured;item.responseLower=model.responseLower;item.responseUpper=model.responseUpper;item.countRegression=state.countComparison;item.countDistribution=::rlispstat::core::CountDistributionId(model.countDistribution);item.exposure=model.exposure;item.offsetVariable=model.offsetVariable;item.trialsVariable=model.trialsVariable;item.trialsConstant=model.trialsConstant;item.terms=specification.terms;item.termTypes=::rlispstat::core::GeneralizedComparisonModelTermTypes(state,model);item.centeredPredictors.assign(specification.centeredPredictors.begin(),specification.centeredPredictors.end());item.factorReferenceLevels=specification.factorReferenceLevels;item.specificationRevision=model.modelVersion;item.specificationFingerprint=::rlispstat::core::GeneralizedComparisonModelSpecificationFingerprint(state,model);model.requestedSpecificationRevision=item.specificationRevision;model.requestedSpecificationFingerprint=item.specificationFingerprint;task.models.push_back(std::move(item));model.isStale=true;model.fitState=::rlispstat::core::RegressionComparisonFitState::Pending;}
        if(state.dataScopeCaptured&&state.dataScope.kind==::rlispstat::core::AnalysisScopeKind::ExplicitRowIds)
            task.rows=state.dataScope.originalRowIds;
        else
        {
            bool needsSelectionRows=false;
            for(auto const& model:state.models)
            {
                const auto specification=::rlispstat::core::EffectiveGeneralizedComparisonModelSpecification(state,model);
                const auto scope=specification.scope.empty()?state.scope:specification.scope;
                if(scope=="selected"||scope=="unselected")
                {
                    needsSelectionRows=true;
                    break;
                }
            }
            if(needsSelectionRows)
            {
                std::set<int> selectedRows;
                if(commandDispatcher_->applicationState().selectedRows(state.group,selectedRows))
                    task.rows.assign(selectedRows.begin(),selectedRows.end());
            }
        }
        state.lastRFitSignature=::rlispstat::core::GeneralizedComparisonFitSignature(state);::rlispstat::core::RefreshGeneralizedTermRowsFromFits(state);state.rFitPending=true;pendingMainRTasks_.generalizedComparisonTasks.push_back(std::move(task));
        MarkMainRTaskPending();
    }

    void App::ShowGeneralizedComparison(std::string const& id)
    {
        if(!commandDispatcher_)return;auto found=commandDispatcher_->applicationState().generalizedComparisons().find(id);if(found==commandDispatcher_->applicationState().generalizedComparisons().end())return;
        auto& state=found->second;auto const* dataframe=commandDispatcher_->applicationState().datasets().find(state.group);if(dataframe&&!state.hasSeed){::rlispstat::core::PopulateDatasetSeedPlot(state.seed,*dataframe,state.group);state.hasSeed=true;}
        std::vector<std::string> responses,available,exposures;if(dataframe){for(auto const& column:dataframe->columns){if(::rlispstat::core::DataColumnAllMissing(column))continue;available.push_back(column.name);if(state.binaryComparison){if(::rlispstat::core::InspectBinaryResponse(column).ok)responses.push_back(column.name);}else if(state.countComparison){if(::rlispstat::core::InspectCountResponse(column).ok)responses.push_back(column.name);if(column.name!=state.response&&::rlispstat::core::InspectCountExposure(column).ok)exposures.push_back(column.name);}else if(state.modelType==::rlispstat::core::StatisticalModelType::PositiveContinuous){if(::rlispstat::core::InspectResponseDomain(column,::rlispstat::core::ResponseDomain::StrictlyPositiveContinuous).ok)responses.push_back(column.name);}else if(state.modelType==::rlispstat::core::StatisticalModelType::Proportion){auto distribution=::rlispstat::core::FindDistributionSpecification(state.family);if(distribution&&::rlispstat::core::InspectResponseDomain(column,distribution->responseDomain).ok)responses.push_back(column.name);}else if(column.type=="numeric"||column.type=="ordinal")responses.push_back(column.name);}}
        auto view=generalizedComparisonViews_.count(id)&&generalizedComparisonViews_[id]?generalizedComparisonViews_[id]:GeneralizedComparisonView::Create();generalizedComparisonViews_[id]=view;view->SetClosedCallback([this,id](){HandleGeneralizedComparisonClosed(id);});
        view->SetScopeChoices(AnalysisScopeChoicesFor(commandDispatcher_->applicationState(),state.group,true));
        view->SetCommandCallback(
            [this](std::vector<std::string> const& command){HandleAnalysisViewCommand(command);},
            [this,id](int modelIndex,std::string const& kind){OpenGeneralizedComparisonDiagnostic(id,modelIndex,kind);},
            std::move(responses),std::move(available),std::move(exposures));view->Show(state);
        std::weak_ptr<GeneralizedComparisonView> weakView=view;if(applicationCommands_)applicationCommands_->RegisterWindow({"generalized-comparison:"+id,to_hstring(::rlispstat::core::StatisticalModelComparisonTitle(state.modelType,state.multipleImputation)).c_str(),state.group,"output",id,[weakView](){if(auto current=weakView.lock())current->Activate();},[weakView](){if(auto current=weakView.lock())current->Close();},{},{},{},{},{}});
        for(auto const& sourceModel:state.models)RefreshRegressionDerivedOutputs(sourceModel.id);
        RefreshDiagnosticPlots();
    }
    void App::HandleGeneralizedComparisonClosed(std::string const& id){if(resettingViews_)return;if(applicationCommands_)applicationCommands_->UnregisterWindow("generalized-comparison:"+id);generalizedComparisonViews_.erase(id);}

    std::string App::AddDiagnosticPlot(std::unique_ptr<::rlispstat::core::PlotModel> model)
    {
        if (!commandDispatcher_ || !model) return {};
        model->isDatasetSeed = false;
        model->isGLMDiagnostic = true;
        model->interactionMode = "select";
        model->selectionMode = "replace";
        auto* pointer = model.get();
        const std::string id = pointer->id;
        plots_[id] = std::move(model);
        const auto attached = AttachPlotModel(*pointer);
        if (!attached.accepted) { plots_.erase(id); return {}; }
        commandDispatcher_->plotCoordinator().activatePlot(id);
        ShowNativePlot(*pointer);
        return id;
    }

    std::string App::OpenLinearModelDiagnostic(std::string const& group,
                                                std::string const& kind)
    {
        if (!commandDispatcher_) return {};
        auto fit = commandDispatcher_->applicationState().linearModelFits().find(group);
        auto modelState = commandDispatcher_->applicationState().groupModels().find(group);
        auto const* dataframe = modelState == commandDispatcher_->applicationState().groupModels().end()
            ? nullptr : commandDispatcher_->applicationState().datasets().find(modelState->second.group);
        if (fit == commandDispatcher_->applicationState().linearModelFits().end() ||
            modelState == commandDispatcher_->applicationState().groupModels().end() ||
            !fit->second.ok || !dataframe) return {};
        auto model = std::make_unique<::rlispstat::core::PlotModel>();
        ::rlispstat::core::PopulateDatasetSeedPlot(*model, *dataframe, modelState->second.group);
        SetDiagnosticImputedModelInputRows(
            *model, dataframe, modelState->second);
        InitializeDiagnosticImputationPresentation(*model, dataframe, kind);
        auto data = LinearDiagnosticDataForPlot(
            *model, fit->second, kind,
            modelState->second.fitVersion, modelState->second.diagnosticsVersion);
        model->id = "diagnostic_linear_windows_" +
            std::to_string(static_cast<unsigned long long>(GetTickCount64()));
        model->glmModelId = "linear:" + group;
        if (!ApplyDiagnosticPlotData(*model, data)) return {};
        return AddDiagnosticPlot(std::move(model));
    }

    std::string App::OpenGeneralizedModelDiagnostic(std::string const& id,
                                                     std::string const& kind)
    {
        if (!commandDispatcher_) return {};
        auto found = commandDispatcher_->applicationState().generalizedGLMs().find(id);
        if (found == commandDispatcher_->applicationState().generalizedGLMs().end() ||
            !found->second.ok) return {};
        auto model = std::make_unique<::rlispstat::core::PlotModel>();
        auto const* dataframe =
            commandDispatcher_->applicationState().datasets().find(found->second.group);
        if (found->second.hasSeed) *model = found->second.seed;
        else if (dataframe)
            ::rlispstat::core::PopulateDatasetSeedPlot(*model, *dataframe, found->second.group);
        else return {};
        SetDiagnosticImputedModelInputRows(
            *model, dataframe, found->second,
            found->second.exposure.empty()
                ? std::vector<std::string>{}
                : std::vector<std::string>{found->second.exposure});
        InitializeDiagnosticImputationPresentation(*model, dataframe, kind);
        auto data = GeneralizedDiagnosticDataForPlot(
            *model, found->second, kind, found->second.diagnosticOptions,
            found->second.fitVersion, found->second.diagnosticsVersion);
        model->id = "diagnostic_generalized_windows_" +
            std::to_string(static_cast<unsigned long long>(GetTickCount64()));
        model->glmModelId = "generalized:" + id;
        model->generalizedDiagnosticResiduals = true;
        model->diagnosticAvailableResidualTypes =
            ::rlispstat::core::AvailableGeneralizedResidualTypes(found->second);
        if (!ApplyDiagnosticPlotData(*model, data)) return {};
        const std::string plotId = AddDiagnosticPlot(std::move(model));
        auto inserted = plots_.find(plotId);
        if (inserted != plots_.end() && inserted->second)
        {
            inserted->second->rExportTheme = commandDispatcher_->plotTheme();
            inserted->second->codeReference =
                ::rlispstat::core::GeneralizedDiagnosticPlotCodeReference(
                    *inserted->second, found->second);
            if (!inserted->second->codeReference.provenance.verificationRCode.empty())
                commandDispatcher_->applicationState().registerOutputCodeReference(
                    inserted->second->codeReference);
        }
        return plotId;
    }

    std::string App::OpenRegressionComparisonDiagnostic(std::string const& id,
                                                         int modelIndex,
                                                         std::string const& kind)
    {
        if (!commandDispatcher_) return {};
        auto found = commandDispatcher_->applicationState().regressionComparisons().find(id);
        if (found == commandDispatcher_->applicationState().regressionComparisons().end() ||
            modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= found->second.models.size()) return {};
        auto const& comparisonModel = found->second.models[static_cast<std::size_t>(modelIndex)];
        if (!comparisonModel.fit.ok) return {};
        auto model = std::make_unique<::rlispstat::core::PlotModel>();
        auto const* dataframe =
            commandDispatcher_->applicationState().datasets().find(found->second.group);
        if (found->second.hasSeed) *model = found->second.seed;
        else if (dataframe)
            ::rlispstat::core::PopulateDatasetSeedPlot(*model, *dataframe, found->second.group);
        else return {};
        SetDiagnosticImputedModelInputRows(
            *model, dataframe, comparisonModel);
        InitializeDiagnosticImputationPresentation(*model, dataframe, kind);
        auto data = LinearDiagnosticDataForPlot(
            *model, comparisonModel.fit, kind, comparisonModel.fitVersion,
            comparisonModel.fitVersion);
        model->id = "diagnostic_comparison_windows_" +
            std::to_string(static_cast<unsigned long long>(GetTickCount64()));
        // Bind the diagnostic to the semantic model, not its current column.
        // Columns can be inserted, removed, or reordered while model.id remains
        // stable for the lifetime of that model specification.
        model->glmModelId = comparisonModel.id;
        if (!ApplyDiagnosticPlotData(*model, data)) return {};
        return AddDiagnosticPlot(std::move(model));
    }

    std::string App::OpenGeneralizedComparisonDiagnostic(std::string const& id,
                                                          int modelIndex,
                                                          std::string const& kind)
    {
        if (!commandDispatcher_) return {};
        auto found = commandDispatcher_->applicationState().generalizedComparisons().find(id);
        if (found == commandDispatcher_->applicationState().generalizedComparisons().end() ||
            modelIndex < 0 || static_cast<std::size_t>(modelIndex) >= found->second.models.size()) return {};
        auto const& source = found->second.models[static_cast<std::size_t>(modelIndex)];
        if (!source.fit.ok) return {};
        auto model = std::make_unique<::rlispstat::core::PlotModel>();
        auto const* dataframe =
            commandDispatcher_->applicationState().datasets().find(found->second.group);
        if (found->second.hasSeed) *model = found->second.seed;
        else if (dataframe)
            ::rlispstat::core::PopulateDatasetSeedPlot(*model, *dataframe, found->second.group);
        else return {};
        SetDiagnosticImputedModelInputRows(
            *model, dataframe, source,
            source.exposure.empty()
                ? std::vector<std::string>{}
                : std::vector<std::string>{source.exposure});
        InitializeDiagnosticImputationPresentation(*model, dataframe, kind);
        auto data = GeneralizedDiagnosticDataForPlot(
            *model, source.fit, kind, source.diagnosticOptions,
            source.fitVersion, source.fit.diagnosticsVersion);
        model->id = "diagnostic_generalized_comparison_windows_" +
            std::to_string(static_cast<unsigned long long>(GetTickCount64()));
        model->glmModelId = source.id;
        model->generalizedDiagnosticResiduals = true;
        model->diagnosticAvailableResidualTypes =
            ::rlispstat::core::AvailableGeneralizedResidualTypes(source.fit);
        if (!ApplyDiagnosticPlotData(*model, data)) return {};
        const std::string plotId = AddDiagnosticPlot(std::move(model));
        auto inserted = plots_.find(plotId);
        if (inserted != plots_.end() && inserted->second)
        {
            inserted->second->rExportTheme = commandDispatcher_->plotTheme();
            inserted->second->codeReference =
                ::rlispstat::core::GeneralizedDiagnosticPlotCodeReference(
                    *inserted->second, source.fit);
            if (!inserted->second->codeReference.provenance.verificationRCode.empty())
                commandDispatcher_->applicationState().registerOutputCodeReference(
                    inserted->second->codeReference);
        }
        return plotId;
    }

    bool App::RegressionDerivedSourceIdentity(
        RegressionDerivedOutputDependency const& dependency,
        int& revision, int& fitVersion, bool& accepted) const
    {
        revision = 0;
        fitVersion = 0;
        accepted = false;
        if (!commandDispatcher_) return false;
        auto const& state = commandDispatcher_->applicationState();
        if (dependency.sourceKind == "linear_single")
        {
            auto model = state.groupModels().find(dependency.sourceModelId);
            if (model == state.groupModels().end()) return false;
            revision = model->second.modelVersion;
            fitVersion = model->second.fitVersion;
            auto fit = state.linearModelFits().find(dependency.sourceModelId);
            accepted = fit != state.linearModelFits().end() && fit->second.ok &&
                !model->second.isStale && !model->second.rFitPending &&
                model->second.fitVersion >= model->second.modelVersion;
            return true;
        }
        if (dependency.sourceKind == "generalized_single")
        {
            auto model = state.generalizedGLMs().find(dependency.sourceModelId);
            if (model == state.generalizedGLMs().end()) return false;
            revision = model->second.modelVersion;
            fitVersion = model->second.fitVersion;
            accepted = model->second.ok && !model->second.rFitPending &&
                model->second.fitVersion >= model->second.modelVersion;
            return true;
        }
        if (dependency.sourceKind == "linear_comparison")
        {
            for (auto const& [id, comparison] : state.regressionComparisons())
            {
                auto const* model = ::rlispstat::core::RegressionComparisonModelById(
                    comparison, dependency.sourceModelId);
                if (!model) continue;
                revision = model->modelVersion;
                fitVersion = model->fitVersion;
                accepted = model->fit.ok && !model->isStale &&
                    model->fitVersion >= model->modelVersion &&
                    ::rlispstat::core::RegressionComparisonResolvedFitState(
                        comparison, *model) ==
                    ::rlispstat::core::RegressionComparisonFitState::Valid;
                return true;
            }
            return false;
        }
        if (dependency.sourceKind == "generalized_comparison")
        {
            for (auto const& [id, comparison] : state.generalizedComparisons())
            {
                auto const* model = ::rlispstat::core::GeneralizedComparisonModelById(
                    comparison, dependency.sourceModelId);
                if (!model) continue;
                revision = model->modelVersion;
                fitVersion = model->fitVersion;
                accepted = model->fit.ok && !model->isStale &&
                    ::rlispstat::core::GeneralizedComparisonResolvedFitState(
                        comparison, *model) ==
                    ::rlispstat::core::RegressionComparisonFitState::Valid;
                return true;
            }
        }
        return false;
    }

    App::RegressionDerivedOutputDependency App::BeginRegressionDerivedOutputRequest(
        std::string outputId, std::string outputKind,
        std::string sourceKind, std::string sourceModelId,
        std::vector<std::string> refreshCommand)
    {
        RegressionDerivedOutputDependency dependency;
        dependency.outputId = std::move(outputId);
        dependency.outputKind = std::move(outputKind);
        dependency.sourceKind = std::move(sourceKind);
        dependency.sourceModelId = std::move(sourceModelId);
        dependency.refreshCommand = std::move(refreshCommand);
        dependency.dependencyId = dependency.outputId + "|" + dependency.outputKind;
        bool accepted = false;
        RegressionDerivedSourceIdentity(dependency, dependency.sourceRevision,
                                        dependency.sourceFitVersion, accepted);
        dependency.requestGeneration = ++nextRegressionDerivedRequestGeneration_;
        regressionDerivedOutputs_[dependency.dependencyId] = dependency;
        return dependency;
    }

    bool App::RegressionDerivedOutputRequestIsCurrent(
        RegressionDerivedOutputDependency const& dependency) const
    {
        auto registered = regressionDerivedOutputs_.find(dependency.dependencyId);
        if (registered == regressionDerivedOutputs_.end() ||
            registered->second.requestGeneration != dependency.requestGeneration)
            return false;
        int revision = 0;
        int fitVersion = 0;
        bool accepted = false;
        return RegressionDerivedSourceIdentity(
                   dependency, revision, fitVersion, accepted) && accepted &&
            revision == dependency.sourceRevision &&
            fitVersion == dependency.sourceFitVersion;
    }

    bool App::RegressionDerivedOutputHasConsumer(
        RegressionDerivedOutputDependency const& dependency) const
    {
        if (dependency.outputKind == "plot")
        {
            for (auto const& [plotId, plot] : plots_)
                if (plot && plot->isRegressionDerivedPlot &&
                    plot->glmModelId == dependency.outputId)
                    return true;
            return false;
        }
        auto output = outputViews_.find(dependency.outputId);
        if (output != outputViews_.end() && output->second) return true;
        auto interaction = interactionReportViews_.find(dependency.outputId);
        return interaction != interactionReportViews_.end() && interaction->second;
    }

    void App::ClearRegressionDerivedOutput(
        RegressionDerivedOutputDependency const& dependency,
        std::string const& message)
    {
        if (dependency.outputKind == "plot")
        {
            for (auto& [plotId, plot] : plots_)
            {
                if (!plot || !plot->isRegressionDerivedPlot ||
                    plot->glmModelId != dependency.outputId) continue;
                plot->points.clear();
                plot->interactionPlotLines.clear();
                plot->interactionXTicks.clear();
                plot->interactionXTickRows.clear();
                plot->overlays.clear();
                auto view = views_.find(plotId);
                if (view != views_.end() && view->second) view->second->Show(*plot);
            }
            return;
        }
        auto interaction = interactionReportViews_.find(dependency.outputId);
        if (interaction != interactionReportViews_.end() && interaction->second)
        {
            interaction->second->SetPendingMessage(message);
            return;
        }
        auto state = outputStates_.find(dependency.outputId);
        auto view = outputViews_.find(dependency.outputId);
        if (state == outputStates_.end() || view == outputViews_.end() || !view->second)
            return;
        auto cleared = state->second;
        cleared.rows.clear();
        cleared.warnings = { message };
        cleared.statusText = message;
        outputStates_[dependency.outputId] = cleared;
        view->second->Show(cleared);
    }

    void App::RefreshRegressionDerivedOutputs(std::string const& sourceModelId)
    {
        std::vector<RegressionDerivedOutputDependency> dependencies;
        for (auto const& [id, dependency] : regressionDerivedOutputs_)
            if (dependency.sourceModelId == sourceModelId)
                dependencies.push_back(dependency);
        for (auto const& dependency : dependencies)
        {
            if (!RegressionDerivedOutputHasConsumer(dependency)) continue;
            int revision = 0;
            int fitVersion = 0;
            bool accepted = false;
            const bool found = RegressionDerivedSourceIdentity(
                dependency, revision, fitVersion, accepted);
            if (!found || !accepted)
            {
                ClearRegressionDerivedOutput(
                    dependency, "Waiting for the updated source model.");
                continue;
            }
            if (revision == dependency.sourceRevision &&
                fitVersion == dependency.sourceFitVersion)
                continue;
            ClearRegressionDerivedOutput(
                dependency, "Updating from the current source model...");
            HandleAnalysisViewCommand(dependency.refreshCommand);
        }
    }

    void App::ForgetRegressionDerivedOutput(
        std::string const& outputId, std::string const& outputKind)
    {
        regressionDerivedOutputs_.erase(outputId + "|" + outputKind);
    }

    void App::RefreshDiagnosticPlots()
    {
        if (!commandDispatcher_) return;
        for (auto& [plotId, model] : plots_)
        {
            if (!model) continue;
            if (model->isGLMDiagnostic && model->isRegressionDerivedPlot &&
                model->regressionDerivedKind == "partial_regression_plot")
            {
                if (model->regressionPartialPointsByImputation.empty()) continue;
                ::rlispstat::core::RegressionPartialPlotResult result;
                result.ok = true;
                result.term = model->regressionDerivedTerm;
                result.residualType = model->regressionPartialResidualType;
                result.contributionScale = model->regressionPartialContributionScale;
                result.imputationCount = static_cast<int>(
                    model->regressionPartialPointsByImputation.size());
                result.pointsByImputation =
                    model->regressionPartialPointsByImputation;
                auto data = ::rlispstat::core::BuildRegressionPartialPlotData(
                    result, model->diagnosticImputationIndex,
                    model->diagnosticShowImputationUncertainty);
                if (data.ok && ApplyDiagnosticPlotData(*model, data))
                {
                    auto view = views_.find(plotId);
                    if (view != views_.end() && view->second)
                        view->second->Show(*model);
                }
                continue;
            }
            if (!model->isGLMDiagnostic) continue;
            auto clearStale = [&]()
            {
                model->points.clear();
                model->histogramPoints.clear();
                model->histogramBins.clear();
                model->rocStepPoints.clear();
                model->rocThresholds.clear();
                model->interactionPlotLines.clear();
                model->diagnosticImputationValues.clear();
                model->diagnosticAllImputationValues.clear();
                model->displayedFitVersion = 0;
                model->displayedDiagnosticsVersion = 0;
                model->displayedDiagnosticOptionsVersion = 0;
                model->diagnosticSummary =
                    "No current model fit is available for this diagnostic.";
                const std::string suffix = " — waiting for current model fit";
                if (model->title.size() < suffix.size() ||
                    model->title.compare(model->title.size() - suffix.size(),
                                         suffix.size(), suffix) != 0)
                    model->title += suffix;
                auto view = views_.find(plotId);
                if (view != views_.end() && view->second)
                    view->second->Show(*model);
            };
            auto retainFrozen = [&]()
            {
                model->analysisScopeFrozen = true;
                model->frozenScopeNotice =
                    "This diagnostic still belongs to the frozen source-model fit.";
                auto view = views_.find(plotId);
                if (view != views_.end() && view->second)
                    view->second->Show(*model);
            };
            ::rlispstat::core::DiagnosticPlotData data;
            ::rlispstat::core::GeneralizedGLMState const* generalizedCodeSource = nullptr;
            bool sourceFrozen = false;
            if (model->glmModelId.rfind("linear:", 0) == 0)
            {
                const std::string modelId = model->glmModelId.substr(7);
                auto fit = commandDispatcher_->applicationState().linearModelFits().find(modelId);
                auto state = commandDispatcher_->applicationState().groupModels().find(modelId);
                if (fit == commandDispatcher_->applicationState().linearModelFits().end() ||
                    state == commandDispatcher_->applicationState().groupModels().end())
                {
                    clearStale();
                    continue;
                }
                sourceFrozen = !state->second.autoRefit;
                if (sourceFrozen) retainFrozen();
                else {
                    model->analysisScopeFrozen = false;
                    model->frozenScopeNotice.clear();
                }
                if (!fit->second.ok || (!sourceFrozen &&
                    (state->second.isStale || state->second.rFitPending))) {
                    clearStale();
                    continue;
                }
                auto const* dataframe =
                    commandDispatcher_->applicationState().datasets().find(state->second.group);
                SetDiagnosticImputedModelInputRows(
                    *model, dataframe,
                    state->second);
                data = LinearDiagnosticDataForPlot(
                    *model, fit->second, model->glmDiagnosticKind,
                    state->second.fitVersion, state->second.diagnosticsVersion);
            }
            else if (model->glmModelId.rfind("generalized:", 0) == 0)
            {
                const std::string id = model->glmModelId.substr(12);
                auto found = commandDispatcher_->applicationState().generalizedGLMs().find(id);
                if (found == commandDispatcher_->applicationState().generalizedGLMs().end())
                {
                    clearStale();
                    continue;
                }
                sourceFrozen = !found->second.autoRefit;
                if (sourceFrozen) retainFrozen();
                else {
                    model->analysisScopeFrozen = false;
                    model->frozenScopeNotice.clear();
                }
                if (!found->second.ok || (!sourceFrozen &&
                    (found->second.rFitPending ||
                     found->second.fitVersion < found->second.modelVersion))) {
                    clearStale();
                    continue;
                }
                auto const* dataframe =
                    commandDispatcher_->applicationState().datasets().find(found->second.group);
                SetDiagnosticImputedModelInputRows(
                    *model, dataframe,
                    found->second,
                    found->second.exposure.empty()
                        ? std::vector<std::string>{}
                        : std::vector<std::string>{found->second.exposure});
                data = GeneralizedDiagnosticDataForPlot(
                    *model, found->second, model->glmDiagnosticKind,
                    found->second.diagnosticOptions, found->second.fitVersion,
                    found->second.diagnosticsVersion);
                generalizedCodeSource = &found->second;
            }
            else
            {
                bool resolved = false;
                for (auto const& entry : commandDispatcher_->applicationState().regressionComparisons())
                {
                    auto const* source = ::rlispstat::core::RegressionComparisonModelById(
                        entry.second, model->glmModelId);
                    if (!source) continue;
                    sourceFrozen = !entry.second.autoRefit;
                    if (sourceFrozen) retainFrozen();
                    if ((!sourceFrozen &&
                         (entry.second.rFitPending || source->isStale)) ||
                        !source->fit.ok)
                    {
                        resolved = true;
                        break;
                    }
                    SetDiagnosticImputedModelInputRows(
                        *model,
                        commandDispatcher_->applicationState().datasets().find(entry.second.group),
                        *source);
                    data = LinearDiagnosticDataForPlot(
                        *model, source->fit, model->glmDiagnosticKind,
                        source->fitVersion, source->fitVersion);
                    resolved = true;
                    break;
                }
                if (!resolved)
                {
                    for (auto const& entry : commandDispatcher_->applicationState().generalizedComparisons())
                    {
                        auto const* source = ::rlispstat::core::GeneralizedComparisonModelById(
                            entry.second, model->glmModelId);
                        if (!source) continue;
                        sourceFrozen = !entry.second.autoRefit;
                        if (sourceFrozen) retainFrozen();
                        if ((!sourceFrozen &&
                             (entry.second.rFitPending || source->isStale)) ||
                            !source->fit.ok)
                        {
                            resolved = true;
                            break;
                        }
                        SetDiagnosticImputedModelInputRows(
                            *model,
                            commandDispatcher_->applicationState().datasets().find(entry.second.group),
                            *source,
                            source->exposure.empty()
                                ? std::vector<std::string>{}
                                : std::vector<std::string>{source->exposure});
                        data = GeneralizedDiagnosticDataForPlot(
                            *model, source->fit, model->glmDiagnosticKind,
                            source->diagnosticOptions, source->fitVersion,
                            source->fit.diagnosticsVersion);
                        generalizedCodeSource = &source->fit;
                        resolved = true;
                        break;
                    }
                }
                if (!resolved)
                {
                    clearStale();
                    continue;
                }
                if (!sourceFrozen) {
                    model->analysisScopeFrozen = false;
                    model->frozenScopeNotice.clear();
                }
            }
            if (!data.ok)
            {
                clearStale();
                continue;
            }
            if (data.fitVersion == model->displayedFitVersion &&
                data.diagnosticsVersion == model->displayedDiagnosticsVersion &&
                data.diagnosticOptionsVersion == model->displayedDiagnosticOptionsVersion &&
                data.residualType == model->displayedResidualType) continue;
            if (!generalizedCodeSource) model->diagnosticSummary.clear();
            if (!ApplyDiagnosticPlotData(*model, data)) continue;
            if (generalizedCodeSource)
            {
                model->rExportTheme = commandDispatcher_->plotTheme();
                model->diagnosticAvailableResidualTypes =
                    ::rlispstat::core::AvailableGeneralizedResidualTypes(
                        *generalizedCodeSource);
                model->codeReference =
                    ::rlispstat::core::GeneralizedDiagnosticPlotCodeReference(
                        *model, *generalizedCodeSource);
                if (!model->codeReference.provenance.verificationRCode.empty())
                    commandDispatcher_->applicationState().registerOutputCodeReference(
                        model->codeReference);
            }
            auto view = views_.find(plotId);
            if (view != views_.end() && view->second) view->second->Show(*model);
        }
    }

    void App::RefreshSelectionVisuals(
        ::rlispstat::core::PlotCoordinationEvent const& event)
    {
        ::rlispstat::windows::performance::Scope timing(
            "App.RefreshSelectionVisuals",
            "group=" + event.group + ",plots=" +
                std::to_string(event.affectedPlotIds.size()) + ",selected=" +
                std::to_string(event.selectedRows.size()));
        std::size_t refreshed = 0;
        for (auto const& plotId : event.affectedPlotIds)
        {
            auto found = views_.find(plotId);
            if (found != views_.end() && found->second)
            {
                found->second->RefreshSelection(event.selectedRows);
                ++refreshed;
            }
        }
        auto sheet = dataSheets_.find(event.group);
        if (sheet != dataSheets_.end() && sheet->second)
        {
            sheet->second->RefreshSelection(event.selectedRows);
            ++refreshed;
        }
        for(auto const& [id,view]:dendrogramViews_){auto state=commandDispatcher_->applicationState().dendrograms().find(id);if(view&&state!=commandDispatcher_->applicationState().dendrograms().end()&&state->second.group==event.group){view->RefreshSelection(event.selectedRows);++refreshed;}}
        if(sheet != dataSheets_.end() && sheet->second)
            sheet->second->SetAnalysisScope(commandDispatcher_->applicationState().activeAnalysisScope(event.group));
        RefreshAnalysisScopeIndicators(event.group);
        const auto global = commandDispatcher_->applicationState().activeAnalysisScope(event.group);
        if (::rlispstat::core::AnalysisScopeTracksCurrentSelection(global) ||
            global.sourceKind == ::rlispstat::core::AnalysisScopeSourceKind::CurrentUnselection) {
            RefreshOpenPlotsForScope(event.group, true);
            RefitOpenAnalysesForScope(event.group);
        }
        QueueCommandStateRefresh();
        LogStartup("Refreshed selection for " + event.group + " in "
            + std::to_string(refreshed) + " views: "
            + ::rlispstat::core::CaseSetText(event.selectedRows));
    }

    void App::ResetAllViews()
    {
        resettingViews_ = true;
        regressionDerivedOutputs_.clear();
        for (auto const& [plotId, view] : views_)
        {
            if (view) view->Close();
        }
        std::vector<std::shared_ptr<ScatterplotDialog>> scatterDialogs;
        for (auto const& [group, dialog] : scatterDialogs_) if (dialog) scatterDialogs.push_back(dialog);
        for (auto const& dialog : scatterDialogs) dialog->Close();
        std::vector<std::shared_ptr<PlotWorkflowDialog>> workflowDialogs;
        for (auto const& [key, dialog] : plotWorkflowDialogs_) if (dialog) workflowDialogs.push_back(dialog);
        for (auto const& dialog : workflowDialogs) dialog->Close();
        std::vector<std::shared_ptr<AnalysisWorkflowDialog>> analysisDialogs;
        for (auto const& [key, dialog] : analysisWorkflowDialogs_) if (dialog) analysisDialogs.push_back(dialog);
        for (auto const& dialog : analysisDialogs) dialog->Close();
        std::vector<std::shared_ptr<DescriptiveStatisticsDialog>> descriptiveDialogs;
        for (auto const& [group, dialog] : descriptiveDialogs_) if (dialog) descriptiveDialogs.push_back(dialog);
        for (auto const& dialog : descriptiveDialogs) dialog->Close();
        std::vector<std::shared_ptr<ContingencyTableDialog>> contingencyDialogs;
        for (auto const& [group, dialog] : contingencyDialogs_) if (dialog) contingencyDialogs.push_back(dialog);
        for (auto const& dialog : contingencyDialogs) dialog->Close();
        std::vector<std::shared_ptr<MissingDataImputationDialog>> imputationDialogs;
        for (auto const& [group, dialog] : imputationDialogs_) if (dialog) imputationDialogs.push_back(dialog);
        for (auto const& dialog : imputationDialogs) dialog->Close();
        std::vector<std::shared_ptr<DatasetVariableDialog>> datasetVariableDialogs;
        for (auto const& [key, dialog] : datasetVariableDialogs_)
            if (dialog) datasetVariableDialogs.push_back(dialog);
        for (auto const& dialog : datasetVariableDialogs) dialog->Close();
        std::vector<std::shared_ptr<RDataAssignmentDialog>> returnDialogs;
        for (auto const& [key, dialog] : rDataAssignmentDialogs_) if (dialog) returnDialogs.push_back(dialog);
        for (auto const& dialog : returnDialogs) dialog->Close();
        for (auto const& [group, sheet] : dataSheets_)
        {
            if (sheet) sheet->Close();
        }
        for (auto const& [group, variables] : variableViews_)
        {
            if (variables) variables->Close();
        }
        for (auto const& [id, output] : outputViews_) if (output) output->Close();
        for (auto const& [id, output] : interactionReportViews_) if (output) output->Close();
        for (auto const& [id, output] : correlationViews_) if (output) output->Close();
        for (auto const& [id, output] : meanComparisonViews_) if (output) output->Close();
        for (auto const& [id, output] : meanComparisonDescriptiveViews_) if (output) output->Close();
        for (auto const& [id, output] : linearModelViews_) if (output) output->Close();
        for (auto const& [id, output] : dimensionalityViews_) if (output) output->Close();
        for (auto const& [id, output] : scaleAnalysisViews_) if (output) output->Close();
        for (auto const& [id, output] : dendrogramViews_) if (output) output->Close();
        for (auto const& [id, output] : generalizedModelViews_) if (output) output->Close();
        for (auto const& [id, output] : mixedModelViews_) if (output) output->Close();
        for (auto const& [id, output] : regressionComparisonViews_) if (output) output->Close();
        for (auto const& [id, output] : modelTrellisViews_) if (output) output->Close();
        for (auto const& [id, output] : generalizedComparisonViews_) if (output) output->Close();
        scatterDialogs_.clear();
        plotWorkflowDialogs_.clear();
        analysisWorkflowDialogs_.clear();
        descriptiveDialogs_.clear();
        contingencyDialogs_.clear();
        imputationDialogs_.clear();
        datasetVariableDialogs_.clear();
        rDataAssignmentDialogs_.clear();
        outputViews_.clear();
        outputStates_.clear();
        interactionReportViews_.clear();
        correlationViews_.clear();
        meanComparisonViews_.clear();
        meanPairwiseGenerations_.clear();
        meanComparisonDescriptiveViews_.clear();
        meanComparisonStates_.clear();
        linearModelViews_.clear();
        dimensionalityViews_.clear();
        scaleAnalysisViews_.clear();
        dendrogramViews_.clear();
        generalizedModelViews_.clear();
        mixedModelViews_.clear();
        regressionComparisonViews_.clear();
        modelTrellisViews_.clear();
        generalizedComparisonViews_.clear();
        dataSheets_.clear();
        variableViews_.clear();
        views_.clear();
        if (applicationCommands_) applicationCommands_->ClearWindows();
        if (commandDispatcher_) commandDispatcher_->plotCoordinator().resetPlots();
        plots_.clear();
        RefreshAnalysisScopePanel();
        resettingViews_ = false;
        LogStartup("Reset all auxiliary views");
    }

    void App::QueueCommandStateRefresh()
    {
        if (!applicationCommands_ || commandStateRefreshQueued_) return;
        commandStateRefreshQueued_ = true;
        dispatcherQueue_.TryEnqueue(
            Microsoft::UI::Dispatching::DispatcherQueuePriority::Low,
            [this]()
            {
                commandStateRefreshQueued_ = false;
                if (applicationCommands_) applicationCommands_->RefreshCommandState();
                RefreshAnalysisScopePanel();
            });
    }
}
