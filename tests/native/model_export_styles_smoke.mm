#include "../../src/platform/macos/linkeda_macos_app.mm"
#import <PDFKit/PDFKit.h>
#import <objc/runtime.h>
#include <cassert>
#include <iostream>
static NSString *savePath;
static NSInteger Details(id object,SEL){
 NSAlert *alert=(NSAlert*)object;
 assert([alert.messageText isEqualToString:@"APA 7 table details"]);
 auto views=[(NSStackView*)alert.accessoryView arrangedSubviews];
 [(NSTextField*)views[1] setStringValue:@"3"];[(NSTextField*)views[3] setStringValue:@"Adjusted effects"];
 return NSAlertFirstButtonReturn;
}
static NSInteger Save(id,SEL){return NSModalResponseOK;}
static NSURL *URL(id,SEL){return [NSURL fileURLWithPath:savePath];}
static NSString *PDFText(NSData *data){assert(data);PDFDocument *d=[[[PDFDocument alloc]initWithData:data]autorelease];assert(d.pageCount==1);return d.string;}
int main(int argc,char**argv){assert(argc==2);@autoreleasepool{
 [NSApplication sharedApplication];using namespace rlispstat::core;
 NSString *folder=ToNSString(argv[1]);
 GroupModelState state;state.response="outcome_after";state.multipleImputation=true;
 GLMFitSummary fit;fit.ok=true;fit.n=835;fit.dfResidual=831;fit.r2=.156;fit.adjR2=.153;fit.sigma=3.498;fit.globalF=31.23;fit.globalP=.0001;
 auto add=[&](const char*label,const char*type,double b,double se,double t,double p){GLMCoefficientRow r;r.term=r.displayLabel=label;r.rowType=type;r.estimate=b;r.stdError=se;r.tValue=t;r.pValue=p;r.termType=std::string(type)=="factor_parent"?"factor":"numeric";fit.coefficients.push_back(r);};
 add("(Intercept)","coefficient",4.0584,.4097,9.906,.0001);
 add("outcome_before","coefficient",.3529,.0391,9.021,.0001);
 add("group","factor_parent",NAN,NAN,3.363,.070);
 add("Control","reference",NAN,NAN,NAN,NAN);add("Treatment","factor_level",-.5502,.3001,-1.834,.069);
 auto publication=*LinearModelCodeReference(state,fit).publication.table;
 auto reports=LinearModelReportTables(state,fit);
 auto details=class_getInstanceMethod([NSAlert class],@selector(runModal));auto save=class_getInstanceMethod([NSSavePanel class],@selector(runModal));auto url=class_getInstanceMethod([NSSavePanel class],@selector(URL));
 auto oldD=method_setImplementation(details,(IMP)Details),oldS=method_setImplementation(save,(IMP)Save),oldU=method_setImplementation(url,(IMP)URL);
 for(int kind=0;kind<2;++kind){
  if(kind){GeneralizedGLMState s;s.response="peer_score";s.modelType=StatisticalModelType::Count;s.countRegression=true;s.countDistribution=CountDistribution::BetaBinomial;s.family="betabinomial";s.link="logit";s.n=1121;s.dfResidual=1117;s.multipleImputation=true;s.statisticName="t";s.betaBinomialDispersion=28.378;
   for(const auto&r:fit.coefficients){GeneralizedGLMRow row;row.term=row.displayLabel=r.displayLabel;row.rowType=r.rowType;row.termType=r.termType;row.estimate=r.estimate;row.stdError=r.stdError;row.statistic=r.tValue;row.pValue=r.pValue;row.exponentiatedEstimate=std::isfinite(r.estimate)?std::exp(r.estimate):NAN;row.exponentiatedLower=row.exponentiatedEstimate*.8;row.exponentiatedUpper=row.exponentiatedEstimate*1.2;s.rows.push_back(row);}
   publication=GeneralizedModelPublicationTable(s);reports=GeneralizedModelReportTables(s);
  }
  NSString*stem=kind?@"generalized":@"linear";
  MacVisualTableExportTarget *target=[[MacVisualTableExportTarget alloc]initWithView:nil title:@"Model" baseName:stem tabDelimited:@""];
  [target setPublicationTable:publication];[target setReportTables:reports];
  NSData *normal=[target pdfData];NSString *normalText=PDFText(normal);assert([normalText containsString:@"Type"] && [normalText containsString:@"Terms"]);
  assert([normal writeToFile:[folder stringByAppendingPathComponent:[stem stringByAppendingString:@"-normal.pdf"]] atomically:YES]);
  NSData *png=[target pngData];assert(png);assert([png writeToFile:[folder stringByAppendingPathComponent:[stem stringByAppendingString:@"-normal.png"]] atomically:YES]);
  savePath=[folder stringByAppendingPathComponent:[stem stringByAppendingString:@"-apa.pdf"]];
  [target saveAPAPDF:nil];NSString *apaText=PDFText([NSData dataWithContentsOfFile:savePath]);
  assert([apaText containsString:@"Table 3"] && [apaText containsString:@"Adjusted effects"]);
  assert(![apaText containsString:@"Type"] && ![apaText containsString:@"Sources"]);
  assert([apaText containsString:@"Control (reference)"] && [apaText containsString:@"Note."]);
  assert([apaText containsString:@"-0.5502"]); // never wrap a minus sign onto a separate line

  [target release];
 }
 method_setImplementation(details,oldD);method_setImplementation(save,oldS);method_setImplementation(url,oldU);
 std::cout<<"Normal clipboard/PNG and actual APA save path passed for linear and generalized models.\n";
}}
