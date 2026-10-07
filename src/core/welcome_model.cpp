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
        "Import a data file\u2026",
        "Recent",
        "No recent data files",
        "Example datasets",
        "Connected to R",
        "Connection to R unavailable",
        "Documentation",
        "Documentation is a work in progress.",
        "About LinkEDA",
        "Version",
        "Opening data\u2026",
        "Open Data\u2026",
        "Import Data\u2026"
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

const std::vector<WelcomeExampleItem> &DefaultWelcomeExampleItems()
{
    static const std::vector<WelcomeExampleItem> items = {
        {"mtcars", "Motor Trend cars", "General graphics, correlations, tests, and linear or binary models.", "mtcars.csv", "General"},
        {"Alien", "Alien", "Linked selections, categorical groups, tables, and generalized linear models.", "Alien.csv", "General"},
        {"anscombe", "Anscombe's quartet", "Regression diagnostics and grouped or Trellis scatterplots.", "anscombe.csv", "Linear models"},
        {"airquality", "New York air quality", "Missing-data workflows, time series, scatterplots, and histograms.", "airquality.csv", "Missing data"},
        {"chickweight", "Chick growth", "Longitudinal, grouped, and Trellis plots.", "chickweight.csv", "Longitudinal"},
        {"iris", "Iris flowers", "PCA, factor analysis, clustering, scatterplot matrices, and parallel coordinates.", "iris.csv", "Multivariate"},
        {"sleep_paired", "Paired sleep data", "Paired-samples tests and distribution plots.", "sleep_paired.csv", "Mean comparisons"},
        {"toothgrowth", "Guinea-pig tooth growth", "Two-sample tests, ANOVA, interactions, boxplots, and bars.", "toothgrowth.csv", "Mean comparisons"},
        {"usarrests", "US arrests", "Correlation, PCA, factor analysis, clustering, and multivariate plots.", "usarrests.csv", "Multivariate"},
        {"warpbreaks", "Warp breaks", "Poisson or negative-binomial models, interactions, and Trellis plots.", "warpbreaks.csv", "Count models"},
        {"titanic", "Titanic passengers", "Contingency tables, binary models, interactions, and bar charts.", "titanic.csv", "Categorical data"},
        {"school_absence", "School absence", "Overdispersed count models and factorial interactions.", "school_absence.csv", "Count models"},
        {"insurance_claims", "Car insurance claims", "Poisson models with a log-exposure offset and model comparisons.", "insurance_claims.csv", "Count models"},
        {"gasoline_yield", "Gasoline yield", "Beta proportion models and regression diagnostics.", "gasoline_yield.csv", "Proportion models"},
        {"theoph", "Theophylline concentration", "Positive-continuous models, time series, and grouped plots.", "theoph.csv", "Positive continuous models"},
        {"bfi", "Big Five personality items", "Scale analysis, correlations, and factor analysis.", "bfi.csv", "Psychometrics"},
        {"nhanes_missing", "NHANES with missing values", "Missing-data overview, imputation, and descriptives.", "nhanes_missing.csv", "Missing data"},
        {"nhanes_mice", "NHANES multiple imputation", "Pooled analyses, diagnostics, tables, and plots from a mice mids object.", "nhanes_mice.rds", "Multiple imputation"}
    };
    return items;
}

const WelcomeExampleItem *FindWelcomeExample(const std::string &id)
{
    const auto &items = DefaultWelcomeExampleItems();
    const auto found = std::find_if(items.begin(), items.end(),
        [&id](const WelcomeExampleItem &item) { return item.id == id; });
    return found == items.end() ? nullptr : &*found;
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
    model.exampleItems = DefaultWelcomeExampleItems();
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
    return {"choose_r_dataframe", "open_linkeda_document", "import_data_file", "recent_items",
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
