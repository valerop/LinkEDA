#include "../../src/core/main_r_task_model.h"
#include "../../src/core/plot_geometry.h"

#include <algorithm>
#include <cassert>
#include <string>
#include <vector>

using rlispstat::core::AppendMainRTaskMessages;
using rlispstat::core::EncodeMainRTask;
using rlispstat::core::EncodeStandaloneGeneralizedMISpec;
using rlispstat::core::MainRCompareMeansTask;
using rlispstat::core::MainRDataChoiceTask;
using rlispstat::core::MainRDataBrowseTask;
using rlispstat::core::MainRWelcomeActionTask;
using rlispstat::core::MainRDataAssignTask;
using rlispstat::core::MainRDatasetSyncTask;
using rlispstat::core::MainRDataReturnTask;
using rlispstat::core::MainRDimensionalityTask;
using rlispstat::core::MainRDimensionalityScoreSaveTask;
using rlispstat::core::MainRScaleAnalysisTask;
using rlispstat::core::MainRGeneralizedGLMTask;
using rlispstat::core::MainRGeneralizedComparisonModelTask;
using rlispstat::core::MainRGeneralizedComparisonTask;
using rlispstat::core::MainRImportDataTask;
using rlispstat::core::MainRPlotExportTask;
using rlispstat::core::MainRLinearGLMTask;
using rlispstat::core::MainRMixedModelTask;
using rlispstat::core::MainRRegressionComparisonModelTask;
using rlispstat::core::MainRRegressionComparisonTask;
using rlispstat::core::MainRReplyAcceptsTaskAppend;
using rlispstat::core::MainRSmoothScopeToken;
using rlispstat::core::MainRSmoothTask;
using rlispstat::core::MainRTrellisSmoothTask;
using rlispstat::core::MainRTrellisPanelAnalysisTask;
using rlispstat::core::MainRTaskBatch;
using rlispstat::core::MainRTable1Task;
using rlispstat::core::MainRAnalysisWorkflowTask;
using rlispstat::core::MainRMultipleImputationTask;
using rlispstat::core::NativeMixedModelState;
using rlispstat::core::NativeMixedRandomSpec;
using rlispstat::core::SmoothCurveScope;
using rlispstat::core::BinaryLink;
using rlispstat::core::BinaryLinkId;
using rlispstat::core::BinaryLinkLabel;
using rlispstat::core::SupportedBinaryLinks;
using rlispstat::core::ScaleItemSpecification;
using rlispstat::core::ScaleItemType;

static bool contains(const std::string &text, const std::string &needle)
{
    return text.find(needle) != std::string::npos;
}

int main()
{
    const std::vector<BinaryLink> binaryLinks = SupportedBinaryLinks();
    assert(binaryLinks.size() == 4);
    assert(BinaryLinkId(binaryLinks[0]) == "logit");
    assert(BinaryLinkLabel(binaryLinks[1]) == "Log");
    assert(BinaryLinkId(binaryLinks[2]) == "probit");
    assert(BinaryLinkId(binaryLinks[3]) == "cloglog");

    assert(MainRReplyAcceptsTaskAppend("OK"));
    assert(MainRReplyAcceptsTaskAppend("OK\tpayload"));
    assert(!MainRReplyAcceptsTaskAppend("ERR nope"));
    assert(!MainRReplyAcceptsTaskAppend("O"));

    assert(MainRSmoothScopeToken(SmoothCurveScope::Overall) == "overall");
    assert(MainRSmoothScopeToken(SmoothCurveScope::Selection) == "selected");
    assert(MainRSmoothScopeToken(SmoothCurveScope::ColorGroup) == "color");

    MainRSmoothTask smooth{"plot1", "cars", SmoothCurveScope::ColorGroup, "wt", "mpg", 0.6};
    assert(EncodeMainRTask(smooth) == "SMOOTH_NEEDED|plot1|cars|color|wt|mpg|0.59999999999999998|0|0|FIT_CURVE_V2|loess|0.94999999999999996");
    smooth.imputationPointSets = {
        {1, {{1, 2.5, 3.5}, {2, 4.5, 5.5}}},
        {2, {{1, 2.75, 3.25}}}
    };
    assert(EncodeMainRTask(smooth) ==
           "SMOOTH_NEEDED|plot1|cars|color|wt|mpg|0.59999999999999998|0|0|"
           "MI_POINT_SETS_V1|2|1|2|1|2.5|3.5|2|4.5|5.5|2|1|1|2.75|3.25|"
           "FIT_CURVE_V2|loess|0.94999999999999996");
    smooth.imputationPointSets.clear();
    smooth.useVisibleRows = true;
    smooth.visibleRows = {1, 4};
    assert(EncodeMainRTask(smooth) ==
           "SMOOTH_NEEDED|plot1|cars|color|wt|mpg|0.59999999999999998|0|0|"
           "VISIBLE_ROWS_V1|2|1|4|FIT_CURVE_V2|loess|0.94999999999999996");
    smooth.visibleRows.clear();
    assert(EncodeMainRTask(smooth).find("VISIBLE_ROWS_V1|0|") != std::string::npos);

    MainRTrellisSmoothTask trellisSmooth;
    trellisSmooth.plotId = "trellis1";
    trellisSmooth.group = "cars";
    trellisSmooth.panelId = "4\x1f" "0";
    trellisSmooth.scope = SmoothCurveScope::Overall;
    trellisSmooth.xVariable = "wt";
    trellisSmooth.yVariable = "mpg";
    trellisSmooth.span = 0.75;
    trellisSmooth.rows = {1, 4, 7};
    trellisSmooth.colorKeys = {"blue", "blue", "red"};
    assert(EncodeMainRTask(trellisSmooth) ==
           "TRELLIS_SMOOTH_NEEDED\ttrellis1\tcars\t4\x1f" "0\toverall\twt\tmpg\t0.75\t3\t1\t4\t7\t3\tblue\tblue\tred\t0\t0\tFIT_CURVE_V2\tloess\t0.94999999999999996");

    // An unsplit Trellis panel must encode zero split-colour keys. Sending
    // one empty key per observation makes R enter its grouped-fit branch and
    // yields no curves because empty split names are discarded.
    trellisSmooth.colorKeys.clear();
    assert(EncodeMainRTask(trellisSmooth) ==
           "TRELLIS_SMOOTH_NEEDED\ttrellis1\tcars\t4\x1f" "0\toverall\twt\tmpg\t0.75\t3\t1\t4\t7\t0\t0\t0\tFIT_CURVE_V2\tloess\t0.94999999999999996");
    trellisSmooth.useExplicitPoints = true;
    trellisSmooth.explicitPoints = {{1, 1.0, 3.0}, {4, 2.0, 5.0}};
    assert(EncodeMainRTask(trellisSmooth) ==
           "TRELLIS_SMOOTH_NEEDED\ttrellis1\tcars\t4\x1f" "0\toverall\twt\tmpg\t0.75\t3\t1\t4\t7\t0\t0\t0\tFIT_CURVE_V2\tloess\t0.94999999999999996\tEXPLICIT_POINTS_V1\t2\t1\t1\t3\t4\t2\t5");

    MainRCompareMeansTask compare{"cars", "t", "mpg", "wt", "am"};
    assert(EncodeMainRTask(compare) == "COMPARE_MEANS_NEEDED|cars|t|mpg|wt|am");

    MainRImportDataTask import{"/tmp/example data.sav"};
    assert(EncodeMainRTask(import) == "IMPORT_DATA_NEEDED\t/tmp/example data.sav");
    MainRImportDataTask stagedImport{"/tmp/staged/example.sav", "/cloud/example.sav", true};
    assert(EncodeMainRTask(stagedImport) ==
           "IMPORT_DATA_NEEDED\t/tmp/staged/example.sav\t/cloud/example.sav\t1");
    MainRImportDataTask commitImport;
    commitImport.stagedDataset = "example";
    commitImport.selectedVariables = {"score", "group"};
    assert(EncodeMainRTask(commitImport) ==
           "IMPORT_DATA_COMMIT_NEEDED\texample\t0\t2\tscore\tgroup");
    MainRImportDataTask cancelImport;
    cancelImport.stagedDataset = "example";
    cancelImport.cancelStagedImport = true;
    assert(EncodeMainRTask(cancelImport) ==
           "IMPORT_DATA_COMMIT_NEEDED\texample\t1\t0");

    MainRDataReturnTask dataReturn{"edit-1", "cars", "/tmp/data.payload", "selected", {2, 5}};
    assert(EncodeMainRTask(dataReturn) ==
           "R_DATA_RETURN_NEEDED\tedit-1\tcars\t/tmp/data.payload\tselected\t2\t2\t5");
    MainRDataChoiceTask dataChoice{"choose-1", "mtcars", false};
    assert(EncodeMainRTask(dataChoice) ==
           "R_DATA_CHOICE_NEEDED\tchoose-1\tchoose\tmtcars");

    MainRWelcomeActionTask welcomeExample{"welcome-1", "example", "iris"};
    assert(EncodeMainRTask(welcomeExample) ==
           "WELCOME_ACTION_NEEDED\twelcome-1\texample\tiris");
    MainRDataBrowseTask dataBrowse{"browse-1"};
    assert(EncodeMainRTask(dataBrowse) == "R_DATA_BROWSE_NEEDED\tbrowse-1");
    MainRDataAssignTask dataAssign{
        "assign-1", "cars", "/tmp/data.payload", "cars_edited", "selected", true, {2, 5}
    };
    assert(EncodeMainRTask(dataAssign) ==
           "R_DATA_ASSIGN_NEEDED\tassign-1\tcars\t/tmp/data.payload\tcars_edited\tselected\t1\t2\t2\t5");
    MainRDatasetSyncTask datasetSync{"sync-1", "cars", "/tmp/sync.payload"};
    assert(EncodeMainRTask(datasetSync) ==
           "R_DATASET_SYNC_NEEDED\tsync-1\tcars\t/tmp/sync.payload");

    MainRPlotExportTask exportTask;
    exportTask.requestId = "exp1";
    exportTask.plotId = "plot1";
    exportTask.path = "/tmp/plot.svg";
    exportTask.format = "svg";
    exportTask.operation = "copy";
    exportTask.group = "cars";
    exportTask.plotKind = "scatter";
    exportTask.xVariable = "wt";
    exportTask.yVariable = "mpg";
    exportTask.title = "MPG & weight";
    exportTask.options = {{"theme", "classic"}};
    assert(EncodeMainRTask(exportTask) ==
           "PLOT_EXPORT_NEEDED\texp1\tplot1\t/tmp/plot.svg\tsvg\tcopy\t7\t5\tcars\tscatter\twt\tmpg\tMPG & weight\t1\ttheme\tclassic");

    MainRTrellisPanelAnalysisTask panelAnalysis;
    panelAnalysis.requestId = "panel1";
    panelAnalysis.plotId = "trellis1";
    panelAnalysis.group = "cars";
    panelAnalysis.panelId = "4";
    panelAnalysis.panelLabel = "cyl = 4";
    panelAnalysis.plotType = "boxplot";
    panelAnalysis.xVariable = "am";
    panelAnalysis.yVariable = "mpg";
    panelAnalysis.rows = {1, 2, 3};
    assert(EncodeMainRTask(panelAnalysis) ==
        "TRELLIS_PANEL_ANALYSIS_NEEDED\tpanel1\ttrellis1\tcars\t4\tcyl = 4\tboxplot\tam\tmpg\t\t3\t1\t2\t3");

    MainRCompareMeansTask compareBatch;
    compareBatch.id = "cm_1";
    compareBatch.group = "cars";
    compareBatch.testType = "paired_t";
    compareBatch.responses = {"mpg", "wt"};
    compareBatch.pairs = {{"mpg", "cyl"}, {"wt", "hp"}};
    compareBatch.alternative = "greater";
    compareBatch.confidenceLevel = 0.9;
    compareBatch.method = "welch";
    compareBatch.pAdjustment = "bonferroni";
    compareBatch.groupOrder = {"0", "1"};
    const std::string encodedBatch = EncodeMainRTask(compareBatch);
    assert(encodedBatch.find("COMPARE_MEANS_BATCH_NEEDED\tcm_1\tcars\tpaired_t\tgreater\t0.90000000000000002\twelch\tbonferroni") == 0);
    assert(encodedBatch.find("\t2\tmpg\twt\t2\tmpg\tcyl\twt\thp\t2\t0\t1\tall\t0\t0") != std::string::npos);

    MainRCompareMeansTask mixedOneSample;
    mixedOneSample.id = "cm_mixed";
    mixedOneSample.group = "Alien";
    mixedOneSample.testType = "one_sample_mixed";
    mixedOneSample.responses = {"size", "rank", "happy"};
    mixedOneSample.oneSampleTests = {
        {"size", 60.0, "student"},
        {"rank", 3.0, "wilcoxon"},
        {"happy", 0.5, "binomial"}
    };
    const std::string encodedMixed = EncodeMainRTask(mixedOneSample);
    assert(contains(encodedMixed,
        "\t3\tsize\trank\thappy\t0\t0\tall\t0\t3\tsize\t60\tstudent"
        "\trank\t3\twilcoxon\thappy\t0.5\tbinomial"));

    MainRCompareMeansTask mixedIndependent;
    mixedIndependent.id = "cm_independent_mixed";
    mixedIndependent.group = "Alien";
    mixedIndependent.testType = "independent_mixed";
    mixedIndependent.responses = {"score", "rating", "improved"};
    mixedIndependent.groupVar = "group";
    mixedIndependent.method = "welch";
    mixedIndependent.groupOrder = {"control", "treatment"};
    mixedIndependent.oneSampleTests = {
        {"score", 0.0, "welch"},
        {"rating", 0.0, "mann_whitney"},
        {"improved", 0.0, "proportion"}
    };
    const std::string encodedIndependentMixed = EncodeMainRTask(mixedIndependent);
    assert(contains(encodedIndependentMixed,
        "COMPARE_MEANS_BATCH_NEEDED\tcm_independent_mixed\tAlien\tindependent_mixed"));
    assert(contains(encodedIndependentMixed,
        "\t3\tscore\trating\timproved\t0\t2\tcontrol\ttreatment\tall\t0\t3"
        "\tscore\t0\twelch\trating\t0\tmann_whitney\timproved\t0\tproportion"));

    MainRCompareMeansTask mixedPaired;
    mixedPaired.id = "cm_paired_mixed";
    mixedPaired.group = "Alien";
    mixedPaired.testType = "paired_mixed";
    mixedPaired.pairs = {{"before", "after"}, {"rank_1", "rank_2"}, {"yes_1", "yes_2"}};
    mixedPaired.oneSampleTests = {
        {std::string("before") + '\x1f' + "after", 0.0, "student"},
        {std::string("rank_1") + '\x1f' + "rank_2", 0.0, "wilcoxon"},
        {std::string("yes_1") + '\x1f' + "yes_2", 0.0, "mcnemar"}
    };
    const std::string encodedPairedMixed = EncodeMainRTask(mixedPaired);
    assert(contains(encodedPairedMixed,
        "COMPARE_MEANS_BATCH_NEEDED\tcm_paired_mixed\tAlien\tpaired_mixed"));
    assert(contains(encodedPairedMixed,
        "\t0\t3\tbefore\tafter\trank_1\trank_2\tyes_1\tyes_2\t0\tall\t0\t3"));
    assert(contains(encodedPairedMixed, std::string("before") + '\x1f' + "after\t0\tstudent"));
    assert(contains(encodedPairedMixed, std::string("yes_1") + '\x1f' + "yes_2\t0\tmcnemar"));

    MainRCompareMeansTask sparseMixedPaired;
    sparseMixedPaired.group = "Alien";
    sparseMixedPaired.testType = "paired_mixed";
    sparseMixedPaired.var1 = "before";
    sparseMixedPaired.var2 = "after";
    const std::string encodedSparseMixedPaired = EncodeMainRTask(sparseMixedPaired);
    assert(contains(encodedSparseMixedPaired,
        "COMPARE_MEANS_BATCH_NEEDED\t\tAlien\tpaired_mixed"));
    assert(!contains(encodedSparseMixedPaired, "COMPARE_MEANS_NEEDED|"));

    MainRLinearGLMTask glm;
    glm.group = "cars";
    glm.dependent = "mpg";
    glm.scope = "all";
    glm.terms = {"wt", "am"};
    glm.termTypes = {{"am", "factor"}};
    glm.centeredPredictors = {"wt"};
    glm.factorReferenceLevels = {{"am", "manual"}};
    glm.requestIdentity = "mpg|all|wt|am|selection|2|7";
    assert(EncodeMainRTask(glm) == "GLM_NEEDED\tcars\tmpg\tall\t2\twt\tam\t1\tam\tfactor\t1\twt\t1\tam\tmanual\t0\tFIT_ID_V1\tmpg|all|wt|am|selection|2|7");

    MainRRegressionComparisonTask reg;
    reg.id = "cmp1";
    reg.generation = 7;
    reg.group = "cars";
    reg.response = "mpg";
    reg.scope = "all";
    reg.autoRefit = false;
    reg.datasetType = "multiple_imputation";
    reg.imputationSetId = "mi-cars";
    reg.sourceDatasetId = "cars-source";
    reg.imputationCount = 5;
    reg.termRows = {"(Intercept)", "wt", "am"};
    reg.termTypes = {{"am", "factor"}};
    MainRRegressionComparisonModelTask regBase;
    regBase.id = "m1";
    regBase.label = "Base";
    regBase.response = "mpg";
    regBase.terms = {"wt"};
    regBase.termTypes = {{"wt", "numeric"}};
    regBase.centeredPredictors = {"wt"};
    regBase.factorReferenceLevels = {{"am", "manual"}};
    reg.models.push_back(regBase);
    MainRRegressionComparisonModelTask regFull;
    regFull.id = "m2";
    regFull.label = "Full";
    regFull.response = "mpg";
    regFull.terms = {"wt", "am"};
    regFull.termTypes = {{"am", "factor"}, {"wt", "numeric"}};
    reg.models.push_back(regFull);
    std::string regLine = EncodeMainRTask(reg);
    assert(regLine ==
           "REGCMP_NEEDED\tcmp1\t7\tcars\tmpg\tall\tFALSE"
           "\t3\t(Intercept)\twt\tam\t1\tam\tfactor\t2"
           "\tm1\tBase\tmpg\t1\twt\t1\twt\tnumeric\t1\twt\t1\tam\tmanual"
           "\tm2\tFull\tmpg\t2\twt\tam\t2\tam\tfactor\twt\tnumeric\t0\t0\t0"
           "\tREGCMP_DATASET_V1\tmultiple_imputation\tmi-cars\tcars-source\t5");

    MainRGeneralizedGLMTask gglm;
    gglm.id = "g1";
    gglm.group = "cars";
    gglm.response = "cyl";
    gglm.family = "poisson";
    gglm.link = "log";
    gglm.scope = "selected";
    gglm.generation = 17;
    gglm.terms = {"mpg"};
    gglm.termTypes = {{"mpg", "numeric"}};
    assert(EncodeMainRTask(gglm) ==
           "GGLM_NEEDED\tg1\tcars\tcyl\tpoisson\tlog\tselected\t1\tmpg\t1\tmpg\tnumeric"
           "\tGENERALIZED\t\t\t0\tMODEL_SPEC_V1\t0\t0\tOFFSET_SPEC_V1\t\tMODEL_TYPE_V1\tlegacy_generalized\tGGLM_REQUEST_V1\t17");
    gglm.centeredPredictors = {"mpg"};
    gglm.factorReferenceLevels = {{"gear", "4"}};
    assert(EncodeMainRTask(gglm) ==
           "GGLM_NEEDED\tg1\tcars\tcyl\tpoisson\tlog\tselected\t1\tmpg\t1\tmpg\tnumeric"
           "\tGENERALIZED\t\t\t0\tMODEL_SPEC_V1\t1\tmpg\t1\tgear\t4"
           "\tOFFSET_SPEC_V1\t\tMODEL_TYPE_V1\tlegacy_generalized\tGGLM_REQUEST_V1\t17");
    gglm.offsetVariable = "known_log_offset";
    assert(contains(EncodeMainRTask(gglm),
                    "\tOFFSET_SPEC_V1\tknown_log_offset\tMODEL_TYPE_V1"));
    gglm.offsetVariable.clear();
    gglm.family = "beta";
    gglm.link = "logit";
    gglm.responseBoundsConfigured = true;
    gglm.responseLower = 0.0;
    gglm.responseUpper = 100.0;
    assert(contains(EncodeMainRTask(gglm),
                    "\tOFFSET_SPEC_V1\t\tBOUNDED_RESPONSE_V1\t0\t100\tMODEL_TYPE_V1\tlegacy_generalized\tGGLM_REQUEST_V1\t17"));
    gglm.responseBoundsConfigured = false;
    gglm.binaryRegression = true;
    gglm.family = "binomial";
    gglm.link = "logit";
    gglm.eventValue = "yes";
    gglm.referenceValue = "no";
    assert(contains(EncodeMainRTask(gglm), "\tBINARY\tyes\tno"));

    MainRGeneralizedGLMTask binaryFactors;
    binaryFactors.id = "binary_mtcars";
    binaryFactors.group = "mtcars";
    binaryFactors.response = "vs";
    binaryFactors.family = "binomial";
    binaryFactors.link = "logit";
    binaryFactors.scope = "all";
    binaryFactors.terms = {"mpg", "hp", "am", "cyl"};
    binaryFactors.termTypes = {
        {"mpg", "numeric"}, {"hp", "numeric"},
        {"am", "factor"}, {"cyl", "factor"}
    };
    binaryFactors.factorReferenceLevels = {{"am", "1"}, {"cyl", "6"}};
    binaryFactors.binaryRegression = true;
    binaryFactors.eventValue = "1";
    binaryFactors.referenceValue = "0";
    const std::string encodedBinaryFactors = EncodeMainRTask(binaryFactors);
    assert(contains(encodedBinaryFactors,
        "\t4\tmpg\thp\tam\tcyl\t4\tam\tfactor\tcyl\tfactor\thp\tnumeric\tmpg\tnumeric"));
    assert(contains(encodedBinaryFactors,
        "\tBINARY\t1\t0\t0\tMODEL_SPEC_V1\t0\t2\tam\t1\tcyl\t6"));
    assert(contains(encodedBinaryFactors, "\tGGLM_REQUEST_V1\t0"));

    MainRGeneralizedGLMTask countRegression;
    countRegression.id = "count1";
    countRegression.group = "counts";
    countRegression.response = "events";
    countRegression.family = "poisson";
    countRegression.link = "log";
    countRegression.scope = "all";
    countRegression.terms = {"treatment", "time"};
    countRegression.termTypes = {{"treatment", "factor"}, {"time", "numeric"}};
    countRegression.centeredPredictors = {"time"};
    countRegression.factorReferenceLevels = {{"treatment", "control"}};
    countRegression.countRegression = true;
    countRegression.countDistribution = "negative_binomial";
    countRegression.modelType = "count";
    countRegression.exposure = "person_time";
    countRegression.generation = 23;
    countRegression.rows = {2, 5, 8};
    {
        using namespace rlispstat::core;
        std::set<int> selected;
        for (int row = 216; row <= 835; ++row) selected.insert(row);
        auto request = countRegression;
        request.scope = "selected";
        SetGeneralizedTaskAnalysisScope(request, {}, false, selected);
        assert(request.rows.size() == 620 && request.rows.front() == 216);
        request.scope = "unselected";
        SetGeneralizedTaskAnalysisScope(request, {}, false, selected);
        assert(request.scope == "unselected" && request.rows.size() == 620);
        auto saved = ExplicitAnalysisScope("cars", {2, 8, 12},
            AnalysisScopeSourceKind::OtherExplicitSubset, "Saved subset", 835);
        SetGeneralizedTaskAnalysisScope(request, saved, true, selected);
        assert(request.scope == "selected" && request.rows == std::vector<int>({2, 8, 12}));
        SetGeneralizedTaskAnalysisScope(request, {}, false, {});
        assert(request.scope == "selected" && request.rows.empty());
        request.scope = "all";
        SetGeneralizedTaskAnalysisScope(request, {}, false, selected);
        assert(request.rows.empty());
    }
    const std::string encodedCountRegression = EncodeMainRTask(countRegression);
    assert(contains(encodedCountRegression,
        "GGLM_NEEDED\tcount1\tcounts\tevents\tpoisson\tlog\tall"));
    assert(contains(encodedCountRegression,
        "\tCOUNT\t\t\t3\t2\t5\t8\tMODEL_SPEC_V1\t1\ttime\t1\ttreatment\tcontrol"));
    assert(contains(encodedCountRegression,
        "\tCOUNT_SPEC_V2\tnegative_binomial\tperson_time\t\tnan"));
    assert(contains(encodedCountRegression, "\tGGLM_REQUEST_V1\t23"));
    const std::string standaloneCountSpec =
        EncodeStandaloneGeneralizedMISpec(countRegression);
    assert(standaloneCountSpec ==
        "MI_GGLM_SPEC_V5|poisson|log|23|COUNT|negative_binomial|person_time|||nan|||FALSE|0|0|2|"
        "time|numeric|treatment|factor|1|time|1|treatment|control|3|2|5|8|"
        "MODEL_TYPE_V1|count");
    MainRGeneralizedGLMTask boundedRegression;
    boundedRegression.family = "beta";
    boundedRegression.link = "logit";
    boundedRegression.generation = 24;
    boundedRegression.responseBoundsConfigured = true;
    boundedRegression.responseLower = 0.0;
    boundedRegression.responseUpper = 100.0;
    boundedRegression.modelType = "proportion";
    boundedRegression.termTypes = {{"time", "numeric"}};
    assert(EncodeStandaloneGeneralizedMISpec(boundedRegression) ==
        "MI_GGLM_SPEC_V5|beta|logit|24|GENERALIZED|poisson||||nan|||TRUE|0|100|1|"
        "time|numeric|0|0|0|MODEL_TYPE_V1|proportion");

    MainRGeneralizedComparisonTask generalizedComparison;
    generalizedComparison.id = "gcmp1";
    generalizedComparison.group = "cars";
    generalizedComparison.response = "am";
    generalizedComparison.family = "binomial";
    generalizedComparison.link = "logit";
    generalizedComparison.scope = "all";
    generalizedComparison.binaryComparison = true;
    generalizedComparison.modelType = "binary";
    generalizedComparison.eventValue = "1";
    generalizedComparison.referenceValue = "0";
    generalizedComparison.termTypes = {{"cyl", "factor"}};
    MainRGeneralizedComparisonModelTask generalizedBase;
    generalizedBase.id = "gm1";
    generalizedBase.label = "Base";
    generalizedBase.response = "am";
    generalizedBase.family = "binomial";
    generalizedBase.link = "logit";
    generalizedBase.scope = "all";
    generalizedBase.terms = {"wt"};
    generalizedBase.termTypes = {{"wt", "numeric"}};
    generalizedBase.centeredPredictors = {"wt"};
    generalizedBase.specificationRevision = 7;
    generalizedBase.specificationFingerprint = "fingerprint-base";
    generalizedComparison.models.push_back(generalizedBase);

    MainRGeneralizedComparisonModelTask generalizedExtended;
    generalizedExtended.id = "gm2";
    generalizedExtended.label = "Extended";
    generalizedExtended.response = "am";
    generalizedExtended.family = "binomial";
    generalizedExtended.link = "logit";
    generalizedExtended.scope = "all";
    generalizedExtended.terms = {"wt", "cyl"};
    generalizedExtended.termTypes = {{"wt", "numeric"}, {"cyl", "factor"}};
    generalizedExtended.factorReferenceLevels = {{"cyl", "4"}};
    generalizedExtended.specificationRevision = 8;
    generalizedExtended.specificationFingerprint = "fingerprint-extended";
    generalizedComparison.models.push_back(generalizedExtended);
    generalizedComparison.rows = {1, 2};
    const std::string generalizedComparisonLine = EncodeMainRTask(generalizedComparison);
    assert(contains(generalizedComparisonLine,
        "GCOMP_NEEDED\tgcmp1\tcars\tam\tbinomial\tlogit\tall\tBINARY\t1\t0"));
    assert(contains(generalizedComparisonLine,
        "\t1\tcyl\tfactor\t2\tgm1\tBase\t1\twt\tgm2\tExtended\t2\twt\tcyl\t2\t1\t2"));
    assert(contains(generalizedComparisonLine,
        "\tGCOMP_SPEC_V8\t0\tTRUE\tFALSE\tbinary\tdata_frame\t\t\t0\t2\tgm1\tam\tbinomial\tlogit\tall"
        "\tFALSE\tpoisson\t\t\t\tnan\tFALSE\tnan\tnan\t7\tfingerprint-base\t1\twt"
        "\t1\twt\tnumeric\t1\twt\t0"));
    assert(contains(generalizedComparisonLine,
        "\tgm2\tam\tbinomial\tlogit\tall\tFALSE\tpoisson\t\t\t\tnan\tFALSE\tnan\tnan\t8\tfingerprint-extended"
        "\t2\twt\tcyl\t2\tcyl\tfactor\twt\tnumeric"
        "\t0\t1\tcyl\t4"));

    MainRGeneralizedComparisonTask countComparison;
    countComparison.id = "countcmp1";
    countComparison.group = "counts";
    countComparison.response = "events";
    countComparison.family = "poisson";
    countComparison.link = "log";
    countComparison.scope = "all";
    countComparison.countComparison = true;
    countComparison.modelType = "count";
    countComparison.datasetType = "multiple_imputation";
    countComparison.imputationSetId = "mi-counts";
    countComparison.sourceDatasetId = "counts-source";
    countComparison.imputationCount = 5;
    MainRGeneralizedComparisonModelTask poissonModel;
    poissonModel.id = "cm1";
    poissonModel.label = "Poisson";
    poissonModel.response = "events";
    poissonModel.family = "poisson";
    poissonModel.link = "log";
    poissonModel.scope = "all";
    poissonModel.countRegression = true;
    poissonModel.countDistribution = "poisson";
    poissonModel.exposure = "time";
    poissonModel.terms = {"x"};
    poissonModel.specificationRevision = 3;
    poissonModel.specificationFingerprint = "poisson-fingerprint";
    countComparison.models.push_back(poissonModel);
    MainRGeneralizedComparisonModelTask quasiModel = poissonModel;
    quasiModel.id = "cm2";
    quasiModel.label = "Quasi";
    quasiModel.countDistribution = "quasipoisson";
    quasiModel.specificationRevision = 4;
    quasiModel.specificationFingerprint = "quasi-fingerprint";
    countComparison.models.push_back(quasiModel);
    const std::string countComparisonLine = EncodeMainRTask(countComparison);
    assert(contains(countComparisonLine,
        "GCOMP_NEEDED\tcountcmp1\tcounts\tevents\tpoisson\tlog\tall\tCOUNT"));
    assert(contains(countComparisonLine,
        "\tGCOMP_SPEC_V8\t0\tTRUE\tTRUE\tcount\tmultiple_imputation\tmi-counts"
        "\tcounts-source\t5\t2\tcm1\tevents\tpoisson\tlog\tall"
        "\tTRUE\tpoisson\ttime\t\t\tnan\tFALSE\tnan\tnan\t3\tpoisson-fingerprint\t1\tx"));
    assert(contains(countComparisonLine,
        "\tcm2\tevents\tpoisson\tlog\tall\tTRUE\tquasipoisson\ttime\t\t\tnan\tFALSE\tnan\tnan\t4\tquasi-fingerprint\t1\tx"));

    NativeMixedModelState mixedState;
    mixedState.id = "mix1";
    mixedState.group = "cars";
    mixedState.modelType = "linear";
    mixedState.response = "mpg";
    mixedState.method = "REML";
    mixedState.family = "gaussian";
    mixedState.link = "identity";
    mixedState.fixedEffects = {"wt", "hp"};
    mixedState.randomEffects.push_back(NativeMixedRandomSpec{"subject", {"wt"}, "|"});
    MainRMixedModelTask mixed{mixedState};
    assert(EncodeMainRTask(mixed) ==
           "MIXED_MODEL_NEEDED\tmix1\tcars\tlinear\tmpg\tREML\tgaussian\tidentity\t2\twt\thp\t1\tsubject\t|\t1\twt\tall\t0");

    MainRDimensionalityTask dim;
    dim.id = "pc1";
    dim.group = "cars";
    dim.method = "pca";
    dim.missingMode = "listwise";
    dim.rotation = "varimax";
    dim.scope = "all";
    dim.scale = true;
    dim.componentCount = 3;
    dim.variables = {"mpg", "wt", "hp"};
    assert(EncodeMainRTask(dim) ==
           "PCAFA_FACTOR_NEEDED\tpc1\tcars\tpca\tlistwise\tvarimax\tall\tTRUE\t3\t3\tmpg\twt\thp\t0\tIMPUTATION_V1\t0\tEXTRACTION_V1\tminres");

    MainRDimensionalityScoreSaveTask scoreSave{
        "score-request-1", "dimensionality", "pc1", "cars", 2
    };
    assert(EncodeMainRTask(scoreSave) ==
           "DIMENSIONALITY_SAVE_SCORES_NEEDED\tscore-request-1\tdimensionality\tpc1\tcars\t2");

    MainRScaleAnalysisTask scale;
    scale.id = "scale1";
    scale.group = "items";
    scale.specification.datasetId = "items";
    scale.specification.revision = 3;
    scale.specification.fingerprint = "scale-fingerprint";

    {
        std::vector<MainRScaleAnalysisTask> leased;
        MainRScaleAnalysisTask first = scale;
        first.specification.revision = 2;
        first.specification.fingerprint = "scale-old";
        rlispstat::core::QueueLatestMainRScaleAnalysisTask(leased, first);

        MainRScaleAnalysisTask newest = scale;
        newest.specification.revision = 3;
        newest.specification.fingerprint = "scale-new";
        rlispstat::core::QueueLatestMainRScaleAnalysisTask(leased, newest);
        assert(leased.size() == 1);
        assert(leased.front().specification.revision == 3);
        assert(leased.front().specification.fingerprint == "scale-new");

        assert(!rlispstat::core::AcknowledgeMainRScaleAnalysisTask(
            leased, scale.id, 2, "scale-old"));
        assert(leased.size() == 1);
        assert(!rlispstat::core::AcknowledgeMainRScaleAnalysisTask(
            leased, scale.id, 3, "wrong-fingerprint"));
        assert(leased.size() == 1);
        assert(rlispstat::core::AcknowledgeMainRScaleAnalysisTask(
            leased, scale.id, 3, "scale-new"));
        assert(leased.empty());
    }
    scale.specification.computeOmega = true;
    ScaleItemSpecification q1;
    q1.variable = "q1";
    q1.type = ScaleItemType::Ordinal;
    ScaleItemSpecification q2;
    q2.variable = "q2";
    q2.type = ScaleItemType::Numeric;
    q2.reversed = true;
    q2.hasScoringRange = true;
    q2.scoringMinimum = 1;
    q2.scoringMaximum = 5;
    scale.specification.items = {q1, q2};
    scale.scope = "selected";
    scale.rows = {2, 4};
    assert(EncodeMainRTask(scale) ==
           "SCALE_ANALYSIS_NEEDED\tscale1\titems\t3\tscale-fingerprint\tauto\tmean\t0\tTRUE\tFALSE\tfactor\tpairwise\tTRUE\t0\tminres\toblimin\t20\tselected\t2\t2\t4\t2\tq1\tordinal\tFALSE\tFALSE\t0\t0\tq2\tnumeric\tTRUE\tTRUE\t1\t5");
    q2.hasScoringRange = false;
    scale.specification.items = {q1, q2};
    const std::string scaleWithoutRanges = EncodeMainRTask(scale);
    const std::string scaleWithoutRangesSuffix = "\tq2\tnumeric\tTRUE\tFALSE\t0\t0";
    assert(scaleWithoutRanges.size() >= scaleWithoutRangesSuffix.size());
    assert(scaleWithoutRanges.compare(
               scaleWithoutRanges.size() - scaleWithoutRangesSuffix.size(),
               scaleWithoutRangesSuffix.size(), scaleWithoutRangesSuffix) == 0);
    assert(std::count(scaleWithoutRanges.begin(), scaleWithoutRanges.end(), '\t') == 33);

    MainRTable1Task table1;
    table1.id = "table1_windows_1";
    table1.group = "cars";
    table1.variables = {"mpg", "wt"};
    table1.groupVariable = "am";
    table1.variableTypes = {{"am", "categorical"}, {"mpg", "numeric"}, {"wt", "numeric"}};
    table1.scope = "selected";
    table1.scopeDescription = "Current selection";
    table1.rows = {1, 4, 7};
    assert(EncodeMainRTask(table1) ==
           "TABLE1_NEEDED\ttable1_windows_1\tcars\tam\tTRUE\tTRUE\tTRUE\tTRUE\tordinal\tselected\tCurrent selection\t2\tmpg\twt\tTYPES\t3\tam\tcategorical\tmpg\tnumeric\twt\tnumeric\t3\t1\t4\t7");

    MainRAnalysisWorkflowTask workflow;
    workflow.kind = "generalized_mixed_model";
    workflow.group = "cars";
    workflow.response = "am";
    workflow.groupVariable = "cluster";
    workflow.variables = {"wt", "hp"};
    workflow.family = "binomial";
    workflow.link = "logit";
    workflow.rows = {1, 4, 7};
    assert(EncodeMainRTask(workflow) ==
           "ANALYSIS_WORKFLOW_NEEDED\tgeneralized_mixed_model\tcars\tam\t\tcluster\tbinomial\tlogit\t\t\t\t2\twt\thp\t3\t1\t4\t7");
    workflow.id = "comparison_windows_1";
    assert(EncodeMainRTask(workflow) ==
           "ANALYSIS_WORKFLOW_NEEDED\tgeneralized_mixed_model\tcars\tam\t\tcluster\tbinomial\tlogit\t\t\t\t2\twt\thp\t3\t1\t4\t7\tcomparison_windows_1");

    MainRMultipleImputationTask imputation;
    imputation.group = "cars";
    imputation.imputeVariables = {"mpg", "hp"};
    imputation.predictorVariables = {"mpg", "cyl", "hp"};
    imputation.methods = {{"hp", "pmm"}, {"mpg", "norm"}};
    imputation.imputations = 10;
    imputation.iterations = 7;
    imputation.seed = "42";
    assert(EncodeMainRTask(imputation) ==
           "MULTIPLE_IMPUTATION_NEEDED\tcars\t10\t7\t42\tTRUE\t2\tmpg\thp\t3\tmpg\tcyl\thp\t2\thp\tpmm\tmpg\tnorm");

    MainRTaskBatch batch;
    batch.smoothTasks.push_back(smooth);
    batch.trellisSmoothTasks.push_back(trellisSmooth);
    batch.importDataTasks.push_back(import);
    batch.dataReturnTasks.push_back(dataReturn);
    batch.dataChoiceTasks.push_back(dataChoice);
    batch.dataBrowseTasks.push_back(dataBrowse);
    batch.dataAssignTasks.push_back(dataAssign);
    batch.datasetSyncTasks.push_back(datasetSync);
    batch.plotExportTasks.push_back(exportTask);
    batch.trellisPanelAnalysisTasks.push_back(panelAnalysis);
    batch.compareMeansTasks.push_back(compare);
    batch.linearGLMTasks.push_back(glm);
    batch.regressionComparisonTasks.push_back(reg);
    batch.generalizedGLMTasks.push_back(gglm);
    batch.generalizedComparisonTasks.push_back(generalizedComparison);
    batch.mixedModelTasks.push_back(mixed);
    batch.dimensionalityTasks.push_back(dim);
    batch.dimensionalityScoreSaveTasks.push_back(scoreSave);
    batch.scaleAnalysisTasks.push_back(scale);
    batch.table1Tasks.push_back(table1);
    batch.analysisWorkflowTasks.push_back(workflow);
    batch.multipleImputationTasks.push_back(imputation);
    std::string appended = AppendMainRTaskMessages("OK\tbase", batch);
    assert(contains(appended, "\nSMOOTH_NEEDED|plot1|cars|color|wt|mpg|0.59999999999999998|0|0"));
    assert(contains(appended, "\nTRELLIS_SMOOTH_NEEDED\ttrellis1\tcars\t4\x1f" "0\toverall"));
    assert(contains(appended, "\nIMPORT_DATA_NEEDED\t/tmp/example data.sav"));
    assert(contains(appended, "\nR_DATA_RETURN_NEEDED\tedit-1\tcars"));
    assert(contains(appended, "\nR_DATA_CHOICE_NEEDED\tchoose-1\tchoose\tmtcars"));
    assert(contains(appended, "\nR_DATA_BROWSE_NEEDED\tbrowse-1"));
    assert(contains(appended, "\nR_DATA_ASSIGN_NEEDED\tassign-1\tcars"));
    assert(contains(appended, "\nR_DATASET_SYNC_NEEDED\tsync-1\tcars\t/tmp/sync.payload"));
    assert(contains(appended, "\nPLOT_EXPORT_NEEDED\texp1\tplot1\t/tmp/plot.svg\tsvg\tcopy\t7\t5\tcars\tscatter"));
    assert(contains(appended, "\nGLM_NEEDED\tcars\tmpg\tall\t2\twt\tam"));
    assert(contains(appended, "\nGCOMP_NEEDED\tgcmp1\tcars\tam"));
    assert(contains(appended, "\nPCAFA_FACTOR_NEEDED\tpc1\tcars\tpca"));
    assert(contains(appended, "\nDIMENSIONALITY_SAVE_SCORES_NEEDED\tscore-request-1\tdimensionality\tpc1\tcars\t2"));
    assert(contains(appended, "\nSCALE_ANALYSIS_NEEDED\tscale1\titems\t3\tscale-fingerprint"));
    assert(contains(appended, "\nTABLE1_NEEDED\ttable1_windows_1\tcars\tam"));
    assert(contains(appended, "\nANALYSIS_WORKFLOW_NEEDED\tgeneralized_mixed_model\tcars\tam"));
    assert(contains(appended, "\nMULTIPLE_IMPUTATION_NEEDED\tcars\t10\t7\t42\tTRUE"));
    assert(AppendMainRTaskMessages("ERR bad", batch) == "ERR bad");
    MainRTaskBatch diagnosticsBatch;
    diagnosticsBatch.miDiagnosticsTasks.push_back({"mi-data","chain_mean","bmi"});
    assert(contains(AppendMainRTaskMessages("OK",diagnosticsBatch),
        "\nMI_DIAGNOSTICS_NEEDED\tmi-data\tchain_mean\tbmi"));

    return 0;
}
