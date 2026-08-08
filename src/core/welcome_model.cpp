#include "welcome_model.h"

#include <algorithm>
#include <set>

namespace rlispstat {
namespace core {

LaunchDecision DecideLaunch(const LaunchRequest &request, bool initialDataValid)
{
    const bool hasInitialData = request.hasInitialRObject ||
        request.initialFile.has_value() || request.initialRObjectRequestId.has_value() ||
        request.hasConcreteOpenRequest;
    if (!hasInitialData) return {false, true, false};
    if (initialDataValid) return {true, false, false};
    return {false, false, true};
}

const WelcomeStrings &DefaultWelcomeStrings()
{
    static const WelcomeStrings strings{
        "Welcome to LinkEDA",
        "LinkEDA",
        "Interactive exploratory data analysis",
        "Start with data",
        "Choose a source to begin exploring.",
        "Choose a data frame from R\u2026",
        "Open a data file\u2026",
        "Recent",
        "No recent data files",
        "Example datasets",
        "Connected to R",
        "Connection to R unavailable",
        "Documentation",
        "Documentation is a work in progress.",
        "About LinkEDA",
        "Version",
        "Opening data\u2026"
    };
    return strings;
}

const ApplicationCredits &DefaultApplicationCredits()
{
    static const ApplicationCredits credits{
        "Pedro Valero-Mora",
        "University of Valencia",
        "Department of Methodology of the Behavioural Sciences, University of Valencia"
    };
    return credits;
}

WelcomeWindowModel DefaultWelcomeWindowModel(const std::string &version,
                                             bool connectedToR,
                                             std::vector<WelcomeRecentItem> recentItems)
{
    WelcomeWindowModel model;
    model.strings = DefaultWelcomeStrings();
    model.credits = DefaultApplicationCredits();
    model.version = version;
    model.capabilities.connectedToExistingRSession = connectedToR;
    model.capabilities.canChooseRDataFrame = connectedToR;
    model.capabilities.canOpenFiles = true;
    model.capabilities.canOpenRecentFiles = true;
    model.capabilities.canOpenExamples = connectedToR;
    model.recentItems = NormalizeWelcomeRecentItems(recentItems);
    model.exampleItems = {
        {"mtcars", "mtcars", "Motor Trend car road tests"},
        {"Alien", "Alien", "Alien data for generalized linear models"}
    };
    return model;
}

std::string WelcomeAuthorLine(const ApplicationCredits &credits)
{
    return credits.developerName + " \xC2\xB7 " + credits.institutionShort;
}

std::string WelcomeAuthorAccessibilityText(const ApplicationCredits &credits)
{
    return "Developed by " + credits.developerName + ", " + credits.institutionShort;
}

std::vector<std::string> AboutCreditLines(const ApplicationCredits &credits)
{
    const std::size_t separator = credits.institutionFull.find(", ");
    const std::string department = separator == std::string::npos
        ? credits.institutionFull : credits.institutionFull.substr(0, separator);
    const std::string institution = separator == std::string::npos
        ? credits.institutionShort : credits.institutionFull.substr(separator + 2);
    return {
        "Developed by " + credits.developerName,
        department,
        institution
    };
}

WelcomeAuthorPresentation DefaultWelcomeAuthorPresentation(
    const ApplicationCredits &credits)
{
    return {WelcomeTextRole::SecondaryText, true, false,
            WelcomeAuthorAccessibilityText(credits)};
}

std::vector<std::string> WelcomeTabOrderCommandIds()
{
    return {"choose_r_dataframe", "open_data_file", "recent_items",
            "example_datasets", "documentation", "about"};
}

std::vector<WelcomeRecentItem> NormalizeWelcomeRecentItems(
    const std::vector<WelcomeRecentItem> &items, std::size_t maximumItems)
{
    std::vector<WelcomeRecentItem> normalized;
    std::set<std::string> paths;
    for (const WelcomeRecentItem &item : items) {
        if (item.path.empty() || !paths.insert(item.path).second) continue;
        normalized.push_back(item);
        if (normalized.size() >= maximumItems) break;
    }
    return normalized;
}

} // namespace core
} // namespace rlispstat
