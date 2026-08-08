#include "../../src/core/main_r_task_model.h"
#include "../../src/core/plot_geometry.h"

#include <cassert>
#include <string>
#include <vector>

using rlispstat::core::AppendMainRTaskMessages;
using rlispstat::core::EncodeMainRTask;
using rlispstat::core::MainRCompareMeansTask;
using rlispstat::core::MainRDataChoiceTask;
using rlispstat::core::MainRDataBrowseTask;
using rlispstat::core::MainRWelcomeActionTask;
using rlispstat::core::MainRDataAssignTask;
using rlispstat::core::MainRDataReturnTask;
using rlispstat::core::MainRDimensionalityTask;
using rlispstat::core::MainRGeneralizedGLMTask;
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
using rlispstat::core::NativeMixedModelState;
using rlispstat::core::NativeMixedRandomSpec;
using rlispstat::core::SmoothCurveScope;

static bool contains(const std::string &text, const std::string &needle)
{
    return text.find(needle) != std::string::npos;
}

int main()
{
    assert(MainRReplyAcceptsTaskAppend("OK"));
    assert(MainRReplyAcceptsTaskAppend("OK\tpayload"));
    assert(!MainRReplyAcceptsTaskAppend("ERR nope"));
    assert(!MainRReplyAcceptsTaskAppend("O"));

    assert(MainRSmoothScopeToken(SmoothCurveScope::Overall) == "overall");
    assert(MainRSmoothScopeToken(SmoothCurveScope::Selection) == "selected");
    assert(MainRSmoothScopeToken(SmoothCurveScope::ColorGroup) == "color");

    MainRSmoothTask smooth{"plot1", "cars", SmoothCurveScope::ColorGroup, "wt", "mpg", 0.6};
    assert(EncodeMainRTask(smooth) == "SMOOTH_NEEDED|plot1|cars|color|wt|mpg|0.59999999999999998");

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
           "TRELLIS_SMOOTH_NEEDED\ttrellis1\tcars\t4\x1f" "0\toverall\twt\tmpg\t0.75\t3\t1\t4\t7\t3\tblue\tblue\tred");

    MainRCompareMeansTask compare{"cars", "t", "mpg", "wt", "am"};
    assert(EncodeMainRTask(compare) == "COMPARE_MEANS_NEEDED|cars|t|mpg|wt|am");

    MainRImportDataTask import{"/tmp/example data.sav"};
    assert(EncodeMainRTask(import) == "IMPORT_DATA_NEEDED\t/tmp/example data.sav");
    MainRImportDataTask stagedImport{"/tmp/staged/example.sav", "/cloud/example.sav", true};
    assert(EncodeMainRTask(stagedImport) ==
           "IMPORT_DATA_NEEDED\t/tmp/staged/example.sav\t/cloud/example.sav\t1");

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
    assert(encodedBatch.find("\t2\tmpg\twt\t2\tmpg\tcyl\twt\thp\t2\t0\t1\tall\t0") != std::string::npos);

    MainRLinearGLMTask glm;
    glm.group = "cars";
    glm.dependent = "mpg";
    glm.scope = "all";
    glm.terms = {"wt", "am"};
    glm.termTypes = {{"am", "factor"}};
    assert(EncodeMainRTask(glm) == "GLM_NEEDED\tcars\tmpg\tall\t2\twt\tam\t1\tam\tfactor\t0");

    MainRRegressionComparisonTask reg;
    reg.id = "cmp1";
    reg.group = "cars";
    reg.response = "mpg";
    reg.scope = "all";
    reg.autoRefit = false;
    reg.termRows = {"(Intercept)", "wt", "am"};
    reg.termTypes = {{"am", "factor"}};
    reg.models.push_back(MainRRegressionComparisonModelTask{"m1", "Base", "mpg", {"wt"}});
    reg.models.push_back(MainRRegressionComparisonModelTask{"m2", "Full", "mpg", {"wt", "am"}});
    std::string regLine = EncodeMainRTask(reg);
    assert(contains(regLine, "REGCMP_NEEDED\tcmp1\tcars\tmpg\tall\tFALSE"));
    assert(contains(regLine, "\t3\t(Intercept)\twt\tam\t1\tam\tfactor\t2\tm1\tBase\tmpg\t1\twt"));
    assert(contains(regLine, "\tm2\tFull\tmpg\t2\twt\tam"));

    MainRGeneralizedGLMTask gglm;
    gglm.id = "g1";
    gglm.group = "cars";
    gglm.response = "cyl";
    gglm.family = "poisson";
    gglm.link = "log";
    gglm.scope = "selected";
    gglm.terms = {"mpg"};
    gglm.termTypes = {{"mpg", "numeric"}};
    assert(EncodeMainRTask(gglm) ==
           "GGLM_NEEDED\tg1\tcars\tcyl\tpoisson\tlog\tselected\t1\tmpg\t1\tmpg\tnumeric\tGENERALIZED\t\t\t0");
    gglm.binaryRegression = true;
    gglm.family = "binomial";
    gglm.link = "logit";
    gglm.eventValue = "yes";
    gglm.referenceValue = "no";
    assert(contains(EncodeMainRTask(gglm), "\tBINARY\tyes\tno"));

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
           "PCAFA_FACTOR_NEEDED\tpc1\tcars\tpca\tlistwise\tvarimax\tall\tTRUE\t3\t3\tmpg\twt\thp\t0");

    MainRTaskBatch batch;
    batch.smoothTasks.push_back(smooth);
    batch.trellisSmoothTasks.push_back(trellisSmooth);
    batch.importDataTasks.push_back(import);
    batch.dataReturnTasks.push_back(dataReturn);
    batch.dataChoiceTasks.push_back(dataChoice);
    batch.dataBrowseTasks.push_back(dataBrowse);
    batch.dataAssignTasks.push_back(dataAssign);
    batch.plotExportTasks.push_back(exportTask);
    batch.trellisPanelAnalysisTasks.push_back(panelAnalysis);
    batch.compareMeansTasks.push_back(compare);
    batch.linearGLMTasks.push_back(glm);
    batch.regressionComparisonTasks.push_back(reg);
    batch.generalizedGLMTasks.push_back(gglm);
    batch.mixedModelTasks.push_back(mixed);
    batch.dimensionalityTasks.push_back(dim);
    std::string appended = AppendMainRTaskMessages("OK\tbase", batch);
    assert(contains(appended, "\nSMOOTH_NEEDED|plot1|cars|color|wt|mpg|0.59999999999999998"));
    assert(contains(appended, "\nTRELLIS_SMOOTH_NEEDED\ttrellis1\tcars\t4\x1f" "0\toverall"));
    assert(contains(appended, "\nIMPORT_DATA_NEEDED\t/tmp/example data.sav"));
    assert(contains(appended, "\nR_DATA_RETURN_NEEDED\tedit-1\tcars"));
    assert(contains(appended, "\nR_DATA_CHOICE_NEEDED\tchoose-1\tchoose\tmtcars"));
    assert(contains(appended, "\nR_DATA_BROWSE_NEEDED\tbrowse-1"));
    assert(contains(appended, "\nR_DATA_ASSIGN_NEEDED\tassign-1\tcars"));
    assert(contains(appended, "\nPLOT_EXPORT_NEEDED\texp1\tplot1\t/tmp/plot.svg\tsvg\tcopy\t7\t5\tcars\tscatter"));
    assert(contains(appended, "\nGLM_NEEDED\tcars\tmpg\tall\t2\twt\tam"));
    assert(contains(appended, "\nPCAFA_FACTOR_NEEDED\tpc1\tcars\tpca"));
    assert(AppendMainRTaskMessages("ERR bad", batch) == "ERR bad");

    return 0;
}
