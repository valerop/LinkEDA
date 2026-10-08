#include "../../src/platform/macos/linkeda_macos_app.mm"
#import <PDFKit/PDFKit.h>
#include <cassert>
#include <iostream>
static std::vector<std::string> ReadLines(const std::string &path) {
 std::ifstream file(path);assert(file);std::vector<std::string> lines;
 for(std::string line;std::getline(file,line);)lines.push_back(line);return lines;
}
static void SavePDF(NSData *data,NSString *path) {
 assert(data);auto doc=[[[PDFDocument alloc]initWithData:data]autorelease];assert(doc.pageCount>=1);
 assert([data writeToFile:path atomically:YES]);
 NSString *flat=[[doc.string componentsSeparatedByCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]]componentsJoinedByString:@""];
 assert([flat containsString:@"Logratio"] && [flat containsString:@"Response"]);
 assert([flat containsString:@"Statisticalcontrasts:"]);
}
int main(int argc,char **argv){assert(argc==2);@autoreleasepool{
 [NSApplication sharedApplication];using namespace rlispstat::core;
 const std::string folder=argv[1];NSString *dir=ToNSString(folder);
 GLMPairwiseComparisonResult result;assert(ParseGLMPairwiseComparisonsRResult(ReadLines(folder+"/pairwise.txt"),result));
 assert(result.scaleReport.ok && result.scaleRows.size()==3 && result.scaleHeaders.size()==14);
 assert(result.scaleReport.sections[0].estimateHeader=="Log ratio");
 assert(result.scaleRows[0][0]=="(Control - A) - Third");
 assert(GLMPairwiseComparisonsText(result).find("Response difference")!=std::string::npos);
 auto malformed=ReadLines(folder+"/pairwise.txt");auto marker=std::find(malformed.begin(),malformed.end(),"PAIRWISE_SCALES_V1");assert(marker!=malformed.end());*(marker+1)="999999";
 GLMPairwiseComparisonResult invalid;assert(!ParseGLMPairwiseComparisonsRResult(malformed,invalid));
 GLMPairwiseComparisonPlotLink link;link.factor="g";
 auto controller=[[[GLMPairwiseResultsWindowController alloc]initWithTerm:"g" result:result plotLink:link]autorelease];
 [[controller window]display];NSView *view=[[controller window]contentView];
 auto pairwiseTable=(NSTableView *)[controller valueForKey:@"_table"];
 assert(pairwiseTable && pairwiseTable.tableColumns.count==result.scaleHeaders.size());
 assert([[pairwiseTable.tableColumns[0] dataCell] alignment]==NSTextAlignmentLeft);
 for(NSUInteger col=1;col<pairwiseTable.tableColumns.count;++col)
   assert([[pairwiseTable.tableColumns[col] dataCell] alignment]==NSTextAlignmentRight);
 auto pairwiseScroll=[pairwiseTable enclosingScrollView];
 assert(pairwiseScroll && pairwiseScroll.hasHorizontalScroller);
 assert(NSMinX(pairwiseScroll.contentView.bounds)==0.0);
 NSBitmapImageRep *bitmap=[view bitmapImageRepForCachingDisplayInRect:view.bounds];[view cacheDisplayInRect:view.bounds toBitmapImageRep:bitmap];
 [[bitmap representationUsingType:NSBitmapImageFileTypePNG properties:@{}]writeToFile:[dir stringByAppendingPathComponent:@"pairwise-window.png"] atomically:YES];
 auto table=PairwiseScalePublicationTable(result);LatexPublicationOptions opts;opts.reportHeadings=true;
 SavePDF(PublicationTablesPDFData({table},opts),[dir stringByAppendingPathComponent:@"pairwise-normal.pdf"]);
 SavePDF(PublicationTablePDFData(table),[dir stringByAppendingPathComponent:@"pairwise-apa.pdf"]);
 GLMInteractionReport report;assert(ParseGLMInteractionReportText(ReadLines(folder+"/interaction.txt"),report));
 auto tables=InteractionScalePublicationTables(report);assert(tables.size()>=3);
 SavePDF(PublicationTablesPDFData(tables,opts),[dir stringByAppendingPathComponent:@"effect-normal.pdf"]);
 auto body=BuildGLMInteractionReportDocumentView(ToNSString(GLMInteractionReportText(report)));
 for(NSView *v in body.subviews)assert(NSMaxX(v.frame)<=NSWidth(body.frame)+1);
 [[body dataWithPDFInsideRect:body.bounds]writeToFile:[dir stringByAppendingPathComponent:@"effect-window.pdf"] atomically:YES];
 std::cout<<"Pairwise transport, malformed input, native window and semantic PDF exports passed.\n";
}}
