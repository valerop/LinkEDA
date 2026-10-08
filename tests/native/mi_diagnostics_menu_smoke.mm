#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>
#import <PDFKit/PDFKit.h>
static NSMenuItem *FindMI(NSMenu *menu,NSString *title) {
    for(NSMenuItem *item in menu.itemArray) {
        if([item.title isEqualToString:title])return item;
        if(auto found=FindMI(item.submenu,title))return found;
    }
    return nil;
}
static void DrainMIDiagnosticRefresh() {
    dispatch_async(dispatch_get_main_queue(), ^{
        [NSApp stop:nil];
        [NSApp postEvent:[NSEvent otherEventWithType:NSEventTypeApplicationDefined location:NSZeroPoint
            modifierFlags:0 timestamp:0 windowNumber:0 context:nil subtype:0 data1:0 data2:0] atStart:NO];
    });
    [NSApp run];
}
int main(int argc,char **argv){@autoreleasepool{
    [NSApplication sharedApplication];
    NSMenu *menu=[[NSMenu alloc]initWithTitle:@"Model"];
    rlispstat::core::OutputCodeReference output;
    output.outputId="mi-test";output.analysisId="mi-test";output.outputBlockId="model";
    output.provenance.analysisId="mi-test";
    output.title="Linear Model";
    output.provenance.dataVersion.datasetId="ejemplo_datos_perdidos imputed";
    output.provenance.missingInformationColumns={"Variable","FMI","RIV","MCSE (estimate)","MCSE / SE (%)"};
    output.provenance.missingInformationRows={{"x","0.25","0.4","0.03","5"}};
    MacCommandDispatcher().applicationState().registerOutputCodeReference(output);
    AddMissingInformationMenu(menu,"mi-test");AddMissingInformationMenu(menu,"mi-test");
    assert(menu.numberOfItems==1);
    auto action=FindMI(menu,@"Show missing-information diagnostics");assert(action);
    assert([action.representedObject isEqualToString:@"SHOW_MISSING_INFORMATION|mi-test"]);
    // Exercise the native menu route, not just the shared command handler.
    [(CommandTarget *)action.target runCommand:action];
    assert(g_table1Controllers.count("mi-test:missing-information")==1);
    auto controller=g_table1Controllers.at("mi-test:missing-information");
    assert(rlispstat::core::Table1RowHasValues([controller state]->rows[0]));
    assert([[controller valueForKey:@"badgeField"] isHidden]);
    assert(rlispstat::core::Table1Subtitle(*[controller state],true)=="Dataset: ejemplo_datos_perdidos imputed");
    auto refit=output;refit.provenance.missingInformationRows[0][1]="0.61234";
    assert(MacCommandDispatcher().applicationState().outputCodeReferenceChanged);
    MacCommandDispatcher().applicationState().registerOutputCodeReference(refit);
    const auto* refreshedOutput=MacCommandDispatcher().applicationState().outputCodeReference("mi-test:missing-information");
    assert(refreshedOutput && refreshedOutput->provenance.missingInformationRows[0][1]=="0.61234");
    DrainMIDiagnosticRefresh();
    assert([controller state]->rows[0].values[0]=="0.612");
    MacCommandDispatcher().applicationState().registerOutputCodeReference(output);
    DrainMIDiagnosticRefresh();
    NSView *report=[controller valueForKey:@"reportView"];
    auto layout=rlispstat::core::BuildTable1ReportLayout(*[controller state], report.bounds.size.width);
    double cellX=layout.margin+layout.stubArea;
    for(size_t column=0;column<[controller state]->columns.size();++column) {
        for(double cellY:{layout.headerOffset-14.0,layout.headerOffset+6.0}) {
            auto cellMenu=[(Table1ReportView *)report diagnosticStatisticMenuAtPoint:NSMakePoint(cellX+5.0,cellY)];
            auto explain=FindMI(cellMenu,@"Explain statistic");
            assert(explain && !explain.submenu);
            assert([explain.representedObject isEqualToString:ToNSString([controller state]->columns[column])]);
            assert(explain.target==controller && explain.action==@selector(explainTableStatistic:));
        }
        cellX+=layout.valueColumnWidths[column];
    }
    assert(![(Table1ReportView *)report diagnosticStatisticMenuAtPoint:NSMakePoint(20,20)]);
    NSBitmapImageRep *bitmap=[report bitmapImageRepForCachingDisplayInRect:report.bounds];
    [report cacheDisplayInRect:report.bounds toBitmapImageRep:bitmap];
    assert([[bitmap representationUsingType:NSBitmapImageFileTypePNG properties:@{}]
        writeToFile:@"/tmp/linkeda-mi-diagnostics-render.png" atomically:YES]);
    // Correlation background, variable and cell menus use the same result actions.
    rlispstat::core::CorrelationMatrixState correlation;
    correlation.id="mi-test";correlation.title="Pearson Correlation Matrix – Multiple Imputation";
    correlation.group="ejemplo_datos_perdidos imputed";correlation.multipleImputation=true;
    correlation.precomputed=true;
    g_correlationMatrices[correlation.id]=correlation;
    auto correlationController=[[CorrelationMatrixWindowController alloc]initWithCorrelationId:correlation.id];
    auto backgroundMenu=[[NSMenu alloc]initWithTitle:@"Correlation Matrix"];
    [correlationController addResultActionsToMenu:backgroundMenu];
    auto correlationAction=FindMI(backgroundMenu,@"Show missing-information diagnostics");
    assert(correlationAction && FindMI(backgroundMenu,@"Multiple imputation"));
    assert([correlationAction.representedObject isEqualToString:@"SHOW_MISSING_INFORMATION|mi-test"]);
    [(CommandTarget *)correlationAction.target runCommand:correlationAction];
    DrainMIDiagnosticRefresh();
    assert(g_table1Controllers.at("mi-test:missing-information")==controller);
    assert(FindMI(backgroundMenu,@"Export"));
    g_correlationMatrices[correlation.id].id="ordinary-correlation";
    auto ordinaryMenu=[[NSMenu alloc]initWithTitle:@"Correlation Matrix"];
    [correlationController addResultActionsToMenu:ordinaryMenu];
    assert(!FindMI(ordinaryMenu,@"Multiple imputation"));
    g_correlationMatrices.erase(correlation.id);
    AddMissingInformationMenu(menu,"ordinary-test");assert(menu.numberOfItems==0);
    rlispstat::core::CommandDispatcherServices services;
    rlispstat::core::Table1DisplayState shown;
    services.ui.showTable1=[&](const rlispstat::core::Table1DisplayState& table){shown=table;};
    rlispstat::core::CommandDispatcher dispatcher(services);
    dispatcher.applicationState().registerOutputCodeReference(output);
    assert(dispatcher.dispatch({"SHOW_MISSING_INFORMATION","mi-test"})=="OK");
    assert(shown.tableType=="mi_missing_information" && shown.rows.size()==1);
    assert(shown.rows[0].values[0]=="0.25");
    assert(shown.rows[0].values[2]=="0.03" && shown.rows[0].values[3]=="5.0");
    assert(std::any_of(shown.footnotes.begin(),shown.footnotes.end(),[](const auto& note){return note.find("Explain statistic")!=std::string::npos;}));
    assert(shown.codeReference.publication.table);
    assert(shown.codeReference.publication.table->rows.size()==1);
    output.provenance.verificationRCode["missing_information"]="print('coefficient diagnostics')";
    dispatcher.applicationState().registerOutputCodeReference(output);
    assert(dispatcher.dispatch({"SHOW_MISSING_INFORMATION","mi-test"})=="OK");
    assert(shown.codeReference.outputBlockId=="missing_information");
    output.provenance.missingInformationColumns={"Variable","Estimate","p","FMI"};
    output.provenance.missingInformationRows={{"sexoMujer","0.3457123456789","0.000093561","0.084088"}};
    output.provenance.missingInformationDisplayRows={{-1,"factor_parent","sexo","sexo"},
        {-1,"reference","Hombre (reference)","sexo"},{0,"factor_level","Mujer","sexo"}};
    dispatcher.applicationState().registerOutputCodeReference(output);
    assert(dispatcher.dispatch({"SHOW_MISSING_INFORMATION","mi-test"})=="OK");
    assert(shown.rows.size()==3 && shown.rows[0].label=="sexo");
    assert(rlispstat::core::Table1RowIsIndented(shown.rows[1]));
    assert(rlispstat::core::Table1RowIsIndented(shown.rows[2]));
    assert(shown.rows[2].label=="Mujer" && shown.rows[2].values[0]=="0.346");
    assert(shown.rows[2].values[1]=="< .001" && shown.rows[2].values[2]=="0.084");
    assert(rlispstat::core::Table1CSVText(shown).find("0.3457123456789")!=std::string::npos);
    [controller updateState:shown];
    report=[controller valueForKey:@"reportView"];
    bitmap=[report bitmapImageRepForCachingDisplayInRect:report.bounds];
    [report cacheDisplayInRect:report.bounds toBitmapImageRep:bitmap];
    [[bitmap representationUsingType:NSBitmapImageFileTypePNG properties:@{}]
        writeToFile:@"/tmp/linkeda-mi-hierarchy-render.png" atomically:YES];
    auto compact=GeneralizedGLMReportLayout::Compute(7,940,6,0,0,false,false,true);
    auto expanded=GeneralizedGLMReportLayout::Compute(7,1400,6,0,0,false,false,true);
    assert(compact.coefX[1]==expanded.coefX[1] && compact.coefX[5]==expanded.coefX[5]);
    assert(compact.coefW[0]<=260 && compact.coefW[5]<=200);
    int refreshCount=0;
    dispatcher.setMissingInformationRefresh([&](const auto& table){shown=table;++refreshCount;});
    auto otherModel=output;otherModel.outputId="unrelated-model";
    dispatcher.applicationState().registerOutputCodeReference(otherModel);
    assert(refreshCount==0);
    output.provenance.missingInformationRows[0][1]="-0.987654321";
    output.provenance.dataVersion.version=7;
    output.provenance.scope.kind="explicit";
    output.provenance.scope.description="Selected original rows";
    dispatcher.applicationState().registerOutputCodeReference(output);
    assert(refreshCount==1 && shown.rows[2].values[0]=="-0.988");
    assert(shown.codeReference.provenance.dataVersion.version==7);
    assert(shown.codeReference.publication.table->subtitle=="Selected original rows");
    output.provenance.missingInformationRows.clear();
    output.provenance.missingInformationDisplayRows.clear();
    dispatcher.applicationState().registerOutputCodeReference(output);
    assert(refreshCount==2 && shown.rows.size()==1);
    assert(shown.rows[0].values[0].find("Unavailable")!=std::string::npos);
    // Diagnostics stay linked but do not expose model-editing controls.
    DataFrameModel editData; editData.group="mi-edit"; editData.rows=5;
    for(const std::string name:{"y","x","z","w"}) {
        DataColumn column;column.name=name;column.type="numeric";column.values.assign(5,"1");
        editData.columns.push_back(column);
    }
    rlispstat::core::Table1DisplayState editTable;
    std::string refreshedModel, requestedFit;
    rlispstat::core::CommandDispatcherServices editServices;
    editServices.ui.showTable1=[&](const auto& table){editTable=table;};
    editServices.queries.groupPlot=[](const std::string& group,PlotModel&){return group=="mi-edit";};
    editServices.ui.refreshModelGroup=[&](const auto& group){refreshedModel=group;};
    editServices.ui.refreshGeneralizedGLM=[&](const auto& id){refreshedModel=id;};
    editServices.ui.requestGeneralizedGLMFit=[&](const auto& id){requestedFit=id;return true;};
    rlispstat::core::CommandDispatcher editor(editServices);
    assert(editor.applicationState().registerDataset(editData));
    auto& linear=editor.applicationState().groupModels()["mi-edit"];
    linear.group="mi-edit";linear.response="y";linear.terms={"x"};linear.multipleImputation=true;
    auto editOutput=refit;editOutput.outputId="glm:mi-edit";
    editor.applicationState().registerOutputCodeReference(editOutput);
    assert(editor.dispatch({"SHOW_MISSING_INFORMATION",editOutput.outputId})=="OK");
    assert(editTable.linkedModelOutputId.empty());
    assert(editTable.addVariableOptions.empty());
    [controller updateState:editTable];
    auto addButton=(NSButton *)[controller valueForKey:@"addVariableButton"];
    assert(addButton.hidden);
    assert(!FindMI([controller linkedModelVariableMenu],@"z"));
    assert(editor.dispatch({"MI_ADD_VARIABLE",editOutput.outputId,"z"}).rfind("ERR",0)==0);
    assert((linear.terms==std::vector<std::string>{"x"}));
    auto& binary=editor.applicationState().generalizedGLMs()["binary:mi-edit"];
    binary.id="binary:mi-edit";binary.group="mi-edit";binary.response="y";binary.terms={"x"};
    binary.multipleImputation=true;binary.precomputed=true;
    editOutput.outputId=binary.id;editor.applicationState().registerOutputCodeReference(editOutput);
    assert(editor.dispatch({"SHOW_MISSING_INFORMATION",binary.id})=="OK");
    [controller updateState:editTable];
    assert(addButton.hidden && editTable.addVariableOptions.empty());
    assert(editor.dispatch({"MI_ADD_VARIABLE",binary.id,"z"}).rfind("ERR",0)==0);
    assert((binary.terms==std::vector<std::string>{"x"}));
    [controller updateState:shown];assert(addButton.hidden);
    auto descriptive=editOutput;descriptive.outputId="descriptive-table";
    descriptive.provenance.missingInformationColumns={"Variable","Estimate","p","FMI"};
    descriptive.provenance.missingInformationRows={{"y — Control","12.5","—","0.083"},
        {"y — Treatment","13.1","—","0.132"},{"y — Difference","0.6","0.351","0.15"}};
    descriptive.provenance.missingInformationDisplayRows={{-1,"term_parent","y","y"},
        {0,"summary_mean","Control","y"},{1,"summary_mean","Treatment","y"},
        {2,"group_comparison","Difference (Treatment − Control)","y"}};
    editor.applicationState().registerOutputCodeReference(descriptive);
    assert(editor.dispatch({"SHOW_MISSING_INFORMATION",descriptive.outputId})=="OK");
    assert(rlispstat::core::Table1RowIsParent(editTable.rows[0]));
    for(size_t i=1;i<editTable.rows.size();++i) {
        assert(rlispstat::core::Table1RowIsIndented(editTable.rows[i]));
        assert(!rlispstat::core::Table1RowIsParent(editTable.rows[i]));
    }
    assert(editTable.rows[1].label=="Control" && editTable.rows[1].values[1]=="—");
    assert(editTable.rows[3].values[1]==".351");
    assert(editTable.footnotes.front().find("unadjusted contrast p-value")!=std::string::npos);
    assert(editTable.codeReference.publication.table->rows[1][0].text.find("Control")!=std::string::npos);
    editTable.footnotes.push_back("This MCSE does not measure the stability of standard errors, confidence limits or p-values. High relative efficiency alone does not guarantee their stability.");
    [controller updateState:editTable];
    report=[controller valueForKey:@"reportView"];
    bitmap=[report bitmapImageRepForCachingDisplayInRect:report.bounds];
    [report cacheDisplayInRect:report.bounds toBitmapImageRep:bitmap];
    [[bitmap representationUsingType:NSBitmapImageFileTypePNG properties:@{}]
        writeToFile:@"/tmp/linkeda-mi-table1-hierarchy-render.png" atomically:YES];
    const CGFloat narrowHeight=[(Table1ReportView *)report preferredHeightForWidth:500];
    const CGFloat wideHeight=[(Table1ReportView *)report preferredHeightForWidth:1200];
    assert(narrowHeight>wideHeight);
    [report setFrame:NSMakeRect(0,0,720,[(Table1ReportView *)report preferredHeightForWidth:720])];
    PDFDocument *notesPDF=[[PDFDocument alloc] initWithData:[report dataWithPDFInsideRect:report.bounds]];
    [notesPDF.string writeToFile:@"/tmp/linkeda-notes-export.txt" atomically:YES encoding:NSUTF8StringEncoding error:nil];
    NSString *notesText=[notesPDF.string stringByReplacingOccurrencesOfString:@"\n" withString:@" "];
    assert([notesText containsString:@"High relative efficiency alone does not guarantee their stability."]);
    if(argc>2) {
        auto readPayload=[](const std::string& name) {
            std::ifstream input("/tmp/linkeda-"+name+".payload");std::vector<std::string> result;
            for(std::string line;std::getline(input,line);)result.push_back(line);
            assert(!result.empty());return result;
        };
        DataFrameModel data;data.group="mi-linked-fixture";data.rows=8;data.datasetType="multiple_imputation";
        for(const std::string name:{"y","z","g","country"}) {
            DataColumn column;column.name=name;column.type=name=="y" || name=="z" ? "numeric" : "factor";
            column.values.assign(8,"1");data.columns.push_back(column);
        }
        auto& application=MacCommandDispatcher().applicationState();
        assert(application.registerDataset(data));
        for(const std::string kind:{"table1","means"}) {
            const std::string id="linked-"+kind;
            assert(HandleCommandLines(readPayload(id+"-1")).rfind("OK",0)==0);
            DrainMIDiagnosticRefresh();
            assert(MacCommandDispatcher().dispatch({"SHOW_MISSING_INFORMATION",id})=="OK");
            DrainMIDiagnosticRefresh();
            auto linked=g_table1Controllers.at(id+":missing-information");
            const auto oldRows=[linked state]->rows;
            assert(HandleCommandLines(readPayload(id+"-2")).rfind("OK",0)==0);
            DrainMIDiagnosticRefresh();
            assert(g_table1Controllers.at(id+":missing-information")==linked);
            if(kind=="table1") assert([linked state]->rows.size()>oldRows.size());
            else {
                assert([linked state]->rows[0].label=="y");
                assert([linked state]->rows[0].values[0]!=oldRows[0].values[0]);
            }
        }
        assert(HandleCommandLines(readPayload("linked-cross")).rfind("OK",0)==0);
        DrainMIDiagnosticRefresh();
        auto cross=g_table1Controllers.at("linked-cross");
        assert(([cross state]->stubHeaders==std::vector<std::string>{"g","country"}));
        assert([cross state]->nestedDisplayMode=="percent");
        auto menu=[cross contextMenuForRow:0];
        for(NSString *label in @[@"Add Row Variable",@"Remove Row Variable",@"Change Column Variable",@"Counts Only",@"Percentages Only"])
            assert(FindMI(menu,label));
        assert(FindMI([cross contextMenuForNestedRowVariable:"g"],@"Replace with"));
        assert(!FindMI(menu,@"Refresh from Bar Chart"));
    }
    if(const char *fixture=std::getenv("LINKEDA_COMPARISON_PRESENTATION")) {
        std::ifstream input(fixture);std::vector<std::string> payload;
        for(std::string line;std::getline(input,line);)payload.push_back(line);
        assert(!payload.empty());
        DataFrameModel dataset;dataset.group="mi-comparison-presentation";dataset.rows=24;
        dataset.datasetType="multiple_imputation";dataset.imputationCount=3;
        for(const std::string name:{"perceived_risk_before","perceived_risk_after","genero"}) {
            DataColumn column;column.name=name;column.type=name=="genero"?"factor":"numeric";
            column.values.assign(24,"1");dataset.columns.push_back(column);
        }
        assert(MacCommandDispatcher().applicationState().registerDataset(dataset));
        assert(HandleCommandLines(payload).rfind("OK",0)==0);DrainMIDiagnosticRefresh();
        assert(HandleCommandLines({"SHOW_MISSING_INFORMATION","mi-comparison-presentation"})=="OK");
        DrainMIDiagnosticRefresh();
        auto diagnostic=g_table1Controllers.at("mi-comparison-presentation:missing-information");
        assert([diagnostic state]->rows.size()==8);
        assert([diagnostic state]->rows[0].label=="perceived_risk_before");
        assert([diagnostic state]->rows[4].label=="perceived_risk_after");
        assert([diagnostic state]->rows[1].label=="Female");
        assert([diagnostic state]->rows[2].label=="Male");
        assert([diagnostic state]->rows[3].label=="Difference (Female − Male)");
        assert([diagnostic state]->rows[1].values[3]=="—");
        assert([diagnostic state]->footnotes.size()==3);
        auto scroll=(NSScrollView *)[diagnostic valueForKey:@"scrollView"];
        assert(scroll.scrollerStyle==NSScrollerStyleLegacy);
        auto window=(NSWindow *)[diagnostic valueForKey:@"window"];
        assert(NSWidth(window.contentView.bounds)>=1180);
        [HighResolutionPNGDataForView(window.contentView) writeToFile:@"/tmp/linkeda-mi-comparison-presentation.png" atomically:YES];
        assert([[(NSTextField *)[diagnostic valueForKey:@"statusField"] stringValue] length]==0);
        auto comparison=g_meanComparisonControllers.at("mi-comparison-presentation");
        auto state=[comparison state];
        auto descriptive=std::find_if(state->tables.begin(),state->tables.end(),[](const auto &table){return table.tableId=="group_descriptives";});
        assert(descriptive!=state->tables.end() && descriptive->rows.size()==6);
        assert(descriptive->columns[0].key=="n");
        assert(descriptive->rows[0].kind==rlispstat::core::MeanComparisonRowKind::GroupHeader);
        assert(descriptive->rows[1].label=="Female" && descriptive->rows[2].label=="Male");
        assert(descriptive->rows[1].variable=="perceived_risk_before");
        if(![comparison descriptivesVisible])[comparison toggleDescriptives:nil];
        auto comparisonWindow=(NSWindow *)[comparison valueForKey:@"window"];
        [HighResolutionPNGDataForView(comparisonWindow.contentView) writeToFile:@"/tmp/linkeda-group-descriptives-presentation.png" atomically:YES];
        auto exportMenu=[comparison contextMenuForTable:0 row:0];
        for(NSString *title in @[@"Copy",@"Save",@"PDF image (vector)",@"Copy as PNG",@"Copy as SVG",@"Save as PDF...",@"Save as PNG...",@"Save as SVG...",@"R Code",@"PDF (APA 7 table)…"])
            assert(FindMI(exportMenu,title));
        auto target=(MacVisualTableExportTarget *)[FindMI(exportMenu,@"Copy as PNG") target];
        NSData *png=[target pngData];assert(png.length>1000);
        [png writeToFile:@"/tmp/linkeda-compare-export.png" atomically:YES];
        NSData *svg=[target svgData];assert(svg.length>1000);
        [svg writeToFile:@"/tmp/linkeda-compare-export.svg" atomically:YES];
        std::vector<rlispstat::core::PublicationTableSpec> publication;
        for(const auto &table:state->tables)publication.push_back(rlispstat::core::MeanComparisonPublicationTable(table));
        rlispstat::core::LatexPublicationOptions apa;apa.apa7=true;apa.tableNumber="2";
        apa.title="Two-Sample Comparisons and Group Descriptives";
        std::ofstream recipe("/tmp/linkeda-comparison-apa.R");
        recipe<<rlispstat::core::BuildLatexPublicationRCode(publication,true,apa);

    }
    if(const char *fixture=std::getenv("LINKEDA_PAIRED_PRESENTATION")) {
        std::ifstream input(fixture); std::vector<std::string> payload;
        for(std::string line;std::getline(input,line);)payload.push_back(line);
        DataFrameModel dataset;dataset.group="mi-paired-presentation";dataset.rows=24;
        dataset.datasetType="multiple_imputation";dataset.imputationCount=3;
        for(const std::string name:{"age_before","age_after","risk_before","risk_after"}) {
            DataColumn column;column.name=name;column.type="numeric";column.values.assign(24,"1");
            dataset.columns.push_back(column);
        }
        assert(MacCommandDispatcher().applicationState().registerDataset(dataset));
        assert(HandleCommandLines(payload).rfind("OK",0)==0);DrainMIDiagnosticRefresh();
        assert(HandleCommandLines({"SHOW_MISSING_INFORMATION","mi-paired-presentation"})=="OK");
        DrainMIDiagnosticRefresh();
        auto diagnostic=g_table1Controllers.at("mi-paired-presentation:missing-information");
        const auto &rows=[diagnostic state]->rows;
        assert(rows.size()==8);
        assert(rows[0].label=="age_before − age_after");
        assert(rows[1].label=="age_before" && rows[2].label=="age_after");
        assert(rows[3].label=="Difference (age_before − age_after)");
        assert(rows[4].label=="risk_before − risk_after");
        assert(rows[1].values[3]=="—");
        assert([[(NSTextField *)[diagnostic valueForKey:@"statusField"] stringValue] length]==0);
        auto scroll=(NSScrollView *)[diagnostic valueForKey:@"scrollView"];
        assert(scroll.hasHorizontalScroller && scroll.scrollerStyle==NSScrollerStyleLegacy);
        auto window=(NSWindow *)[diagnostic valueForKey:@"window"];
        [HighResolutionPNGDataForView(window.contentView) writeToFile:@"/tmp/linkeda-paired-mi-presentation.png" atomically:YES];
    }
    if(argc>1) {
        std::ifstream input(argv[1]);std::vector<std::string> command;
        for(std::string line;std::getline(input,line);)command.push_back(line);
        assert(!command.empty());
        DataFrameModel dataset;dataset.group=command[2];dataset.rows=25;
        for(const std::string name:{"age","bmi","hyp","chl"}) {
            DataColumn column;column.name=name;column.type="numeric";column.values.assign(25,"1");
            dataset.columns.push_back(column);
        }
        assert(dispatcher.applicationState().registerDataset(dataset));
        auto reply=dispatcher.dispatch(command);assert(reply.rfind("OK",0)==0);
        assert(shown.tableType=="mi_diagnostics" && shown.rows.size()==4);
        assert(shown.codeReference.publication.table);
        assert(shown.codeReference.publication.table->rows.size()==4);
        assert(!shown.codeReference.provenance.preparedDataPath.empty());
        assert(shown.columns.size()==5);
    }
    {
        DataFrameModel dataset; dataset.group="anova-selection";dataset.rows=24;
        dataset.datasetType="multiple_imputation";dataset.imputationCount=3;
        for(const std::string name:{"age_before","age_group_before","country"}) {
            DataColumn column;column.name=name;column.type=name=="age_before"?"numeric":"factor";
            for(int i=0;i<24;++i)column.values.push_back(name=="age_before"?"20":(i%2?"A":"B"));
            dataset.columns.push_back(column);
        }
        assert(MacCommandDispatcher().applicationState().registerDataset(dataset));
        MeanComparisonState state;state.id="anova-selection-output";state.datasetId=dataset.group;
        state.analysisType="one_way_anova";state.title="One-Way ANOVA";
        state.specification.groupingVariableId="country";state.groupVariable="country";
        auto controller=[[MeanComparisonWindowController alloc] initWithState:state];
        auto menu=[controller addVariableMenu];
        assert(FindMI(menu,@"age_before"));
        assert(!FindMI(menu,@"age_group_before") && !FindMI(menu,@"country"));
        auto scope=(NSPopUpButton *)[controller valueForKey:@"scopePopup"];
        auto window=(NSWindow *)[controller valueForKey:@"window"];
        assert(NSMaxX(scope.frame)<=NSWidth(window.contentView.bounds)-10);
    }
    if(const char *fixture=std::getenv("LINKEDA_ANOVA_DIAGNOSTICS")) {
        std::ifstream input(fixture);std::vector<std::string> payload;
        for(std::string line;std::getline(input,line);)payload.push_back(line);
        DataFrameModel dataset;dataset.group="mi-anova-diagnostics";dataset.rows=36;
        dataset.datasetType="multiple_imputation";dataset.imputationCount=3;
        for(const std::string name:{"age_after","risk_after","country"}) {
            DataColumn column;column.name=name;column.type=name=="country"?"factor":"numeric";
            column.values.assign(36,"1");dataset.columns.push_back(column);
        }
        assert(MacCommandDispatcher().applicationState().registerDataset(dataset));
        assert(HandleCommandLines(payload).rfind("OK",0)==0);DrainMIDiagnosticRefresh();
        assert(HandleCommandLines({"SHOW_MISSING_INFORMATION","mi-anova-diagnostics"})=="OK");
        DrainMIDiagnosticRefresh();
        auto diagnostic=g_table1Controllers.at("mi-anova-diagnostics:missing-information");
        auto state=[diagnostic state];assert(state->rows.size()==2);
        assert(state->rows[0].label=="age_after" && state->rows[1].label=="risk_after");
        assert((state->columns==std::vector<std::string>{"Group","Test","F","df1","df2","p","RIV","m"}));
        assert(state->rows[0].values[0]=="country");
        assert(state->rows[0].values[3]=="2");
        assert(state->footnotes.size()==2 && state->footnotes[0].find("joint test")!=std::string::npos);
        auto window=(NSWindow *)[diagnostic valueForKey:@"window"];
        [HighResolutionPNGDataForView(window.contentView) writeToFile:@"/tmp/linkeda-anova-diagnostics.png" atomically:YES];
    }
    auto root=ImputationDiagnosticsMenuItem("test-data");
    for(NSString *title in @[@"Missingness summary",@"Missingness by variable",@"Missing-data patterns",
        @"Show all patterns",@"Imputation model",@"Logged events",@"Convergence summary",@"Chain means",@"Chain variances",@"Observed vs imputed"])
        assert(FindMI(root.submenu,title));
    auto state=rlispstat::core::BuildMissingDataImputationDialogState();
    assert(state.automaticModelNote.find("choose methods and predictors")!=std::string::npos);
    assert(state.automaticModelNote.find("convergence")!=std::string::npos);
    assert(state.automaticModelNote.find("computing resources")!=std::string::npos);
}}
