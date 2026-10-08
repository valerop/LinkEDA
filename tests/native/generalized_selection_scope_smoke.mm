#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>
#include <fstream>
int main(){@autoreleasepool{
    [NSApplication sharedApplication];
    using namespace rlispstat::core;
    GeneralizedGLMState state;
    state.id="selection-model";state.group="selected-count-mi";
    state.response="y";state.terms={"x"};state.termTypes={{"x","numeric"}};
    state.multipleImputation=true;state.countRegression=true;
    state.trialsConstant=24;state.family="binomial";state.link="logit";
    state.countDistribution=CountDistribution::BetaBinomial;
    for(int row=216;row<=835;++row)g_groupSelections[state.group].insert(row);
    std::ofstream requests("/tmp/linkeda-selected-count-requests.txt");
    for(const std::string scope:{"selected","unselected","all"}) {
        state.scope=scope;state.dataScopeCaptured=false;
        auto task=GeneralizedGLMTaskForState(state,1);
        assert(task.scope==scope);
        assert(task.rows.size()==(scope=="all"?0:620));
        requests<<EncodeMainRTask(task)<<"\n";
    }
    state.scope="unselected";state.dataScopeCaptured=true;
    state.dataScope=ExplicitAnalysisScope(state.group,{2,8,12},
        AnalysisScopeSourceKind::OtherExplicitSubset,"Saved subset",835);
    auto saved=GeneralizedGLMTaskForState(state,2);
    assert(saved.scope=="selected" && saved.rows==std::vector<int>({2,8,12}));
    // A later selection edit must not mutate the queued request.
    g_groupSelections[state.group].clear();
    assert(saved.rows==std::vector<int>({2,8,12}));
    for(auto distribution:{CountDistribution::Poisson,CountDistribution::QuasiPoisson,
        CountDistribution::NegativeBinomial,CountDistribution::BinomialTrials,
        CountDistribution::BetaBinomial,CountDistribution::HurdleBetaBinomialCeiling,
        CountDistribution::PerfectScore}) {
        state.countDistribution=distribution;state.dataScopeCaptured=false;state.scope="selected";
        g_groupSelections[state.group]={1,3,7};
        assert(GeneralizedGLMTaskForState(state,3).rows==std::vector<int>({1,3,7}));
    }
}}
