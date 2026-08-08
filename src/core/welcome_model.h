#ifndef RLISPSTAT_CORE_WELCOME_MODEL_H
#define RLISPSTAT_CORE_WELCOME_MODEL_H

#include <cstddef>
#include <optional>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

enum class LaunchEnvironment {
    FromExistingRSession,
    StandaloneApplication
};

struct LaunchRequest {
    LaunchEnvironment environment = LaunchEnvironment::FromExistingRSession;
    std::optional<std::string> initialFile;
    bool hasInitialRObject = false;
    std::optional<std::string> initialRObjectRequestId;
    bool hasConcreteOpenRequest = false;
};

struct LaunchDecision {
    bool shouldOpenInitialData = false;
    bool shouldShowWelcome = false;
    bool shouldReportInitialDataError = false;
};

LaunchDecision DecideLaunch(const LaunchRequest &request,
                            bool initialDataValid = true);

struct ApplicationCredits {
    std::string developerName;
    std::string institutionShort;
    std::string institutionFull;
};

enum class WelcomeTextRole {
    PrimaryTitle,
    SecondaryText,
    StatusText
};

struct WelcomeAuthorPresentation {
    WelcomeTextRole role = WelcomeTextRole::SecondaryText;
    bool usesSemanticSystemColor = true;
    bool interactive = false;
    std::string accessibilityLabel;
};

struct WelcomeStrings {
    std::string windowTitle;
    std::string applicationName;
    std::string tagline;
    std::string startHeading;
    std::string startDescription;
    std::string chooseRDataFrame;
    std::string openDataFile;
    std::string recentHeading;
    std::string noRecentItems;
    std::string examplesHeading;
    std::string connectedToR;
    std::string connectionUnavailable;
    std::string documentation;
    std::string documentationWorkInProgress;
    std::string about;
    std::string versionPrefix;
    std::string openingData;
};

struct WelcomeRecentItem {
    std::string id;
    std::string displayName;
    std::string path;
    std::string typeLabel;
    bool exists = true;
};

struct WelcomeExampleItem {
    std::string id;
    std::string displayName;
    std::string description;
};

struct WelcomeCapabilities {
    bool connectedToExistingRSession = false;
    bool canChooseRDataFrame = false;
    bool canOpenFiles = true;
    bool canOpenRecentFiles = true;
    bool canOpenExamples = false;
};

struct WelcomeWindowModel {
    WelcomeStrings strings;
    ApplicationCredits credits;
    std::string version;
    WelcomeCapabilities capabilities;
    std::vector<WelcomeRecentItem> recentItems;
    std::vector<WelcomeExampleItem> exampleItems;
};

const WelcomeStrings &DefaultWelcomeStrings();
const ApplicationCredits &DefaultApplicationCredits();
WelcomeWindowModel DefaultWelcomeWindowModel(const std::string &version,
                                             bool connectedToR,
                                             std::vector<WelcomeRecentItem> recentItems = {});

std::string WelcomeAuthorLine(const ApplicationCredits &credits);
std::string WelcomeAuthorAccessibilityText(const ApplicationCredits &credits);
std::vector<std::string> AboutCreditLines(const ApplicationCredits &credits);
WelcomeAuthorPresentation DefaultWelcomeAuthorPresentation(
    const ApplicationCredits &credits);
std::vector<std::string> WelcomeTabOrderCommandIds();

std::vector<WelcomeRecentItem> NormalizeWelcomeRecentItems(
    const std::vector<WelcomeRecentItem> &items,
    std::size_t maximumItems = 8);

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_WELCOME_MODEL_H
