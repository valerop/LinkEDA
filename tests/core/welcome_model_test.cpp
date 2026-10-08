#include "core/welcome_model.h"

#include <cassert>
#include <string>

using namespace rlispstat::core;

int main()
{
    LaunchRequest empty;
    auto emptyDecision = DecideLaunch(empty);
    assert(emptyDecision.shouldShowWelcome);
    assert(!emptyDecision.shouldOpenInitialData);

    LaunchRequest fromR;
    fromR.hasInitialRObject = true;
    auto dataDecision = DecideLaunch(fromR);
    assert(dataDecision.shouldOpenInitialData);
    assert(!dataDecision.shouldShowWelcome);

    LaunchRequest fromFile;
    fromFile.initialFile = "/tmp/data.sav";
    assert(DecideLaunch(fromFile).shouldOpenInitialData);
    auto failed = DecideLaunch(fromFile, false);
    assert(failed.shouldReportInitialDataError);
    assert(!failed.shouldShowWelcome);

    WelcomeWindowModel model = DefaultWelcomeWindowModel("1.2.3", true);
    assert(model.strings.applicationName == "LinkEDA");
    assert(model.strings.tagline == "Interactive exploratory data analysis");
    assert(model.strings.startHeading == "Start with data");
    assert(model.strings.documentationWorkInProgress ==
           "Documentation is a work in progress.");
    assert(model.version == "1.2.3");
    assert(model.capabilities.canChooseRDataFrame);
    assert(model.exampleItems.size() == 18);
    assert(model.exampleItems[0].id == "mtcars");
    assert(model.exampleItems[1].id == "Alien");
    assert(model.exampleItems[2].id == "anscombe");
    assert(model.exampleItems.back().id == "nhanes_mice");
    assert(FindWelcomeExample("Alien") != nullptr);
    assert(FindWelcomeExample("Alien")->fileName == "Alien.csv");
    assert(FindWelcomeExample("insurance_claims") != nullptr);
    assert(FindWelcomeExample("insurance_claims")->fileName == "insurance_claims.csv");
    assert(FindWelcomeExample("does-not-exist") == nullptr);
    assert(WelcomeAuthorLine(model.credits) ==
           "Pedro Valero-Mora \xC2\xB7 University of Valencia");
    assert(WelcomeAuthorAccessibilityText(model.credits) ==
           "Developed by Pedro Valero-Mora, University of Valencia");
    const auto about = AboutCreditLines(model.credits);
    assert(about.size() == 3);
    assert(about[0] == "Developed by Pedro Valero-Mora");
    assert(about[1] == "Department of Methodology of the Behavioural Sciences");
    assert(about[2] == "University of Valencia");
    assert(WelcomeAuthorLine(model.credits).find("Professor") == std::string::npos);
    const auto authorPresentation = DefaultWelcomeAuthorPresentation(model.credits);
    assert(authorPresentation.role == WelcomeTextRole::SecondaryText);
    assert(authorPresentation.usesSemanticSystemColor);
    assert(!authorPresentation.interactive);
    assert(authorPresentation.accessibilityLabel ==
           "Developed by Pedro Valero-Mora, University of Valencia");

    auto recent = NormalizeWelcomeRecentItems({
        {"a", "a.sav", "/tmp/a.sav", "SAV", true},
        {"duplicate", "a.sav", "/tmp/a.sav", "SAV", true},
        {"b", "b.csv", "/tmp/b.csv", "CSV", false}
    });
    assert(recent.size() == 2);
    assert(!recent[1].exists);
    assert(WelcomeTabOrderCommandIds().front() == "choose_r_dataframe");
    assert(WelcomeTabOrderCommandIds().back() == "about");
    return 0;
}
