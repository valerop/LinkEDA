#import <Cocoa/Cocoa.h>
#import <UniformTypeIdentifiers/UniformTypeIdentifiers.h>

#include "binary_regression_pdf_macos.h"
#include "../../core/binary_regression_export.h"

#include <cmath>
#include <cstdlib>
#include <iomanip>
#include <sstream>
#include <vector>

namespace rlispstat {
namespace platform {
namespace macos {
namespace {

NSString *Text(const std::string &value)
{
    NSString *result = [[NSString alloc] initWithBytes:value.data() length:value.size() encoding:NSUTF8StringEncoding];
    return result ?: @"";
}

void DrawText(NSString *text, NSRect rect, NSDictionary *attributes)
{
    [text drawInRect:rect withAttributes:attributes];
}

NSString *APANumber(double value, int digits = 2)
{
    if (std::isnan(value)) return @"\u2014";
    if (std::isinf(value)) return value > 0 ? @"\u221e" : @"-\u221e";
    int usedDigits = digits;
    const double magnitude = std::fabs(value);
    if (magnitude >= 1000000.0 || (magnitude > 0.0 && magnitude < 0.0001)) {
        return [NSString stringWithFormat:@"%.2e", value];
    }
    if (magnitude > 0.0 && magnitude < 0.01) usedDigits = std::max(usedDigits, 3);
    if (magnitude > 0.0 && magnitude < 0.001) usedDigits = std::max(usedDigits, 4);
    return [NSString stringWithFormat:@"%.*f", usedDigits, value];
}

NSString *APAPValue(double value)
{
    if (!std::isfinite(value)) return @"\u2014";
    if (value < 0.001) return @"< .001";
    NSString *formatted = [NSString stringWithFormat:@"%.3f", value];
    return [formatted hasPrefix:@"0"] ? [formatted substringFromIndex:1] : formatted;
}

NSString *APAInterval(double lower, double upper)
{
    if (std::isnan(lower) || std::isnan(upper)) return @"\u2014";
    return [NSString stringWithFormat:@"[%@, %@]", APANumber(lower), APANumber(upper)];
}

void DrawRule(CGFloat left, CGFloat right, CGFloat y, CGFloat width = 0.75)
{
    NSBezierPath *rule = [NSBezierPath bezierPath];
    [rule moveToPoint:NSMakePoint(left, y)];
    [rule lineToPoint:NSMakePoint(right, y)];
    [rule setLineWidth:width];
    [[NSColor blackColor] setStroke];
    [rule stroke];
}

NSDictionary *AlignedAttributes(NSDictionary *base, NSTextAlignment alignment)
{
    NSMutableParagraphStyle *style = [[NSMutableParagraphStyle alloc] init];
    [style setAlignment:alignment];
    NSMutableDictionary *attributes = [base mutableCopy];
    attributes[NSParagraphStyleAttributeName] = style;
    return attributes;
}

std::string SecondaryTableNumber(const std::string &first, int offset)
{
    char *end = nullptr;
    const long number = std::strtol(first.c_str(), &end, 10);
    if (end && *end == '\0' && end != first.c_str()) return std::to_string(number + offset);
    return first + "." + std::to_string(offset + 1);
}

NSString *MainNote(const core::GeneralizedGLMState &state)
{
    NSString *scale = state.binaryLink == core::BinaryLink::Logit
        ? @"OR = odds ratio. Confidence intervals for odds ratios are 95% Wald intervals."
        : @"Coefficients and confidence intervals are reported on the probit scale. Confidence intervals are 95% Wald intervals.";
    NSString *pValue = APAPValue(state.globalP);
    NSString *pClause = [pValue hasPrefix:@"<"]
        ? [NSString stringWithFormat:@"p %@", pValue]
        : [NSString stringWithFormat:@"p = %@", pValue];
    return [NSString stringWithFormat:
        @"Note. N = %d. The event modelled was %@; %@ was the reference outcome. The model used a binomial distribution with a %@ link. The overall model test was LR \u03c7\u00b2(%d) = %@, %@, and Nagelkerke pseudo-R\u00b2 = %@. %@",
        state.n, Text(state.responseCoding.eventLabel), Text(state.responseCoding.referenceLabel), Text(state.link),
        state.dfModel, APANumber(state.globalLR), pClause, APANumber(state.nagelkerkeR2, 3), scale];
}

struct PDFSession {
    CGContextRef context = nullptr;
    CGFloat pageWidth = 595.0;
    CGFloat pageHeight = 842.0;
    CGFloat margin = 44.0;
    int page = 0;
    CGFloat y = 44.0;
    NSDictionary *tableNumberAttributes = nil;
    NSDictionary *titleAttributes = nil;
    NSDictionary *headerAttributes = nil;
    NSDictionary *bodyAttributes = nil;
    NSDictionary *noteAttributes = nil;

    void endPage()
    {
        if (page == 0) return;
        [NSGraphicsContext setCurrentContext:nil];
        CGContextRestoreGState(context);
        CGContextEndPage(context);
    }

    void beginPage(NSString *tableNumber, NSString *title, bool continued)
    {
        if (page > 0) endPage();
        ++page;
        CGContextBeginPage(context, nullptr);
        CGContextSaveGState(context);
        CGContextTranslateCTM(context, 0, pageHeight);
        CGContextScaleCTM(context, 1, -1);
        [NSGraphicsContext setCurrentContext:[NSGraphicsContext graphicsContextWithCGContext:context flipped:YES]];
        y = margin;
        NSString *numberText = continued ? [NSString stringWithFormat:@"Table %@ (continued)", tableNumber]
                                         : [NSString stringWithFormat:@"Table %@", tableNumber];
        DrawText(numberText, NSMakeRect(margin, y, pageWidth - 2 * margin, 17), tableNumberAttributes);
        y += 20.0;
        NSString *titleText = continued ? [title stringByAppendingString:@" (continued)"] : title;
        const CGFloat titleHeight = std::max((CGFloat)18.0,
            [titleText boundingRectWithSize:NSMakeSize(pageWidth - 2 * margin, 60.0)
                options:NSStringDrawingUsesLineFragmentOrigin attributes:titleAttributes].size.height + 2.0);
        DrawText(titleText, NSMakeRect(margin, y, pageWidth - 2 * margin, titleHeight), titleAttributes);
        y += titleHeight + 8.0;
    }
};

void DrawCoefficientHeader(PDFSession &pdf, const std::vector<CGFloat> &widths, bool logit)
{
    NSArray<NSString *> *headers = logit
        ? @[@"Predictor", @"B", @"SE", @"z", @"p", @"OR", @"95% CI for OR"]
        : @[@"Predictor", @"B", @"SE", @"z", @"p", @"95% CI"];
    DrawRule(pdf.margin, pdf.pageWidth - pdf.margin, pdf.y, 0.9); pdf.y += 5.0;
    CGFloat x = pdf.margin;
    for (NSUInteger index = 0; index < headers.count; ++index) {
        NSDictionary *attributes = AlignedAttributes(pdf.headerAttributes, index == 0 ? NSTextAlignmentLeft : NSTextAlignmentRight);
        DrawText(headers[index], NSMakeRect(x, pdf.y, widths[index] - 4.0, 17.0), attributes);
        x += widths[index];
    }
    pdf.y += 19.0;
    DrawRule(pdf.margin, pdf.pageWidth - pdf.margin, pdf.y); pdf.y += 5.0;
}

void DrawCoefficientTable(PDFSession &pdf,
                          const core::GeneralizedGLMState &state,
                          const core::BinaryAPAReportModel &model,
                          NSString *tableNumber,
                          NSString *title)
{
    const CGFloat available = pdf.pageWidth - 2 * pdf.margin;
    std::vector<CGFloat> widths;
    if (model.logit) {
        widths = {available * 0.29, available * 0.085, available * 0.09, available * 0.075,
                  available * 0.085, available * 0.10, available * 0.275};
    } else {
        widths = {available * 0.34, available * 0.11, available * 0.11,
                  available * 0.09, available * 0.10, available * 0.25};
    }
    pdf.beginPage(tableNumber, title, false);
    DrawCoefficientHeader(pdf, widths, model.logit);
    for (const core::BinaryAPACoefficientRow &row : model.coefficients) {
        NSString *predictor = Text(row.predictor);
        const CGFloat labelHeight = [predictor boundingRectWithSize:NSMakeSize(widths[0] - 4.0, 80.0)
            options:NSStringDrawingUsesLineFragmentOrigin attributes:pdf.bodyAttributes].size.height + 3.0;
        const CGFloat rowHeight = std::max((CGFloat)19.0, labelHeight);
        if (pdf.y + rowHeight > pdf.pageHeight - pdf.margin - 105.0) {
            pdf.beginPage(tableNumber, title, true);
            DrawCoefficientHeader(pdf, widths, model.logit);
        }
        NSMutableArray<NSString *> *cells = [NSMutableArray arrayWithObjects:
            predictor, APANumber(row.estimate), APANumber(row.stdError), APANumber(row.statistic), APAPValue(row.pValue), nil];
        if (model.logit) [cells addObject:APANumber(row.oddsRatio)];
        [cells addObject:model.logit ? APAInterval(row.oddsRatioLower, row.oddsRatioUpper)
                                        : APAInterval(row.ciLower, row.ciUpper)];
        CGFloat x = pdf.margin;
        for (NSUInteger index = 0; index < cells.count; ++index) {
            NSDictionary *attributes = AlignedAttributes(pdf.bodyAttributes, index == 0 ? NSTextAlignmentLeft : NSTextAlignmentRight);
            DrawText(cells[index], NSMakeRect(x, pdf.y, widths[index] - 4.0, rowHeight), attributes);
            x += widths[index];
        }
        pdf.y += rowHeight;
    }
    DrawRule(pdf.margin, pdf.pageWidth - pdf.margin, pdf.y, 0.9); pdf.y += 8.0;
    NSString *note = MainNote(state);
    CGFloat noteHeight = [note boundingRectWithSize:NSMakeSize(available, 160.0)
        options:NSStringDrawingUsesLineFragmentOrigin attributes:pdf.noteAttributes].size.height + 3.0;
    CGFloat warningHeight = 0.0;
    for (const std::string &warning : model.warnings) {
        NSString *text = [@"Caution. " stringByAppendingString:Text(warning)];
        warningHeight += [text boundingRectWithSize:NSMakeSize(available, 100.0)
            options:NSStringDrawingUsesLineFragmentOrigin attributes:pdf.noteAttributes].size.height + 5.0;
    }
    if (pdf.y + noteHeight + warningHeight > pdf.pageHeight - pdf.margin) {
        pdf.beginPage(tableNumber, title, true);
        DrawRule(pdf.margin, pdf.pageWidth - pdf.margin, pdf.y, 0.9); pdf.y += 8.0;
    }
    DrawText(note, NSMakeRect(pdf.margin, pdf.y, available, noteHeight), pdf.noteAttributes); pdf.y += noteHeight + 4.0;
    for (const std::string &warning : model.warnings) {
        NSString *text = [@"Caution. " stringByAppendingString:Text(warning)];
        CGFloat height = [text boundingRectWithSize:NSMakeSize(available, 100.0)
            options:NSStringDrawingUsesLineFragmentOrigin attributes:pdf.noteAttributes].size.height + 3.0;
        DrawText(text, NSMakeRect(pdf.margin, pdf.y, available, height), pdf.noteAttributes); pdf.y += height + 2.0;
    }
}

void DrawTermTests(PDFSession &pdf,
                   const core::BinaryAPAReportModel &model,
                   NSString *tableNumber)
{
    NSString *title = @"Likelihood-Ratio Tests of Model Terms";
    pdf.beginPage(tableNumber, title, false);
    const CGFloat available = pdf.pageWidth - 2 * pdf.margin;
    const std::vector<CGFloat> widths = {available * 0.52, available * 0.19, available * 0.11, available * 0.18};
    NSArray<NSString *> *headers = @[@"Predictor", @"LR \u03c7\u00b2", @"df", @"p"];
    DrawRule(pdf.margin, pdf.pageWidth - pdf.margin, pdf.y, 0.9); pdf.y += 5.0;
    CGFloat x = pdf.margin;
    for (NSUInteger index = 0; index < headers.count; ++index) {
        DrawText(headers[index], NSMakeRect(x, pdf.y, widths[index] - 4.0, 17.0),
                 AlignedAttributes(pdf.headerAttributes, index == 0 ? NSTextAlignmentLeft : NSTextAlignmentRight));
        x += widths[index];
    }
    pdf.y += 19.0; DrawRule(pdf.margin, pdf.pageWidth - pdf.margin, pdf.y); pdf.y += 5.0;
    for (const core::BinaryTermTestRow &row : model.termTests) {
        NSArray<NSString *> *cells = @[Text(row.term), APANumber(row.statistic),
            [NSString stringWithFormat:@"%d", row.df], APAPValue(row.pValue)];
        x = pdf.margin;
        for (NSUInteger index = 0; index < cells.count; ++index) {
            DrawText(cells[index], NSMakeRect(x, pdf.y, widths[index] - 4.0, 18.0),
                     AlignedAttributes(pdf.bodyAttributes, index == 0 ? NSTextAlignmentLeft : NSTextAlignmentRight));
            x += widths[index];
        }
        pdf.y += 19.0;
    }
    DrawRule(pdf.margin, pdf.pageWidth - pdf.margin, pdf.y, 0.9);
}

void DrawDetailedFit(PDFSession &pdf,
                     const core::GeneralizedGLMState &state,
                     NSString *tableNumber)
{
    NSString *title = @"Detailed Binary Regression Model Fit";
    pdf.beginPage(tableNumber, title, false);
    const CGFloat available = pdf.pageWidth - 2 * pdf.margin;
    const std::vector<CGFloat> widths = {available * 0.62, available * 0.38};
    NSArray<NSString *> *headers = @[@"Measure", @"Value"];
    DrawRule(pdf.margin, pdf.pageWidth - pdf.margin, pdf.y, 0.9); pdf.y += 5.0;
    CGFloat x = pdf.margin;
    for (NSUInteger index = 0; index < headers.count; ++index) {
        DrawText(headers[index], NSMakeRect(x, pdf.y, widths[index] - 4.0, 17.0),
                 AlignedAttributes(pdf.headerAttributes, index == 0 ? NSTextAlignmentLeft : NSTextAlignmentRight));
        x += widths[index];
    }
    pdf.y += 19.0; DrawRule(pdf.margin, pdf.pageWidth - pdf.margin, pdf.y); pdf.y += 5.0;
    NSArray<NSArray<NSString *> *> *rows = @[
        @[@"N", [NSString stringWithFormat:@"%d", state.n]],
        @[@"Events", [NSString stringWithFormat:@"%d", state.responseCoding.eventCount]],
        @[@"Log likelihood", APANumber(state.logLik)],
        @[@"Deviance", APANumber(state.residualDeviance)],
        @[@"LR \u03c7\u00b2", APANumber(state.globalLR)],
        @[@"df", [NSString stringWithFormat:@"%d", state.dfModel]],
        @[@"p", APAPValue(state.globalP)],
        @[@"AIC", APANumber(state.aic)],
        @[@"BIC", APANumber(state.bic)],
        @[@"Nagelkerke pseudo-R\u00b2", APANumber(state.nagelkerkeR2, 3)],
        @[@"Apparent AUC", APANumber(state.auc, 3)]
    ];
    for (NSArray<NSString *> *row in rows) {
        x = pdf.margin;
        for (NSUInteger index = 0; index < row.count; ++index) {
            DrawText(row[index], NSMakeRect(x, pdf.y, widths[index] - 4.0, 18.0),
                     AlignedAttributes(pdf.bodyAttributes, index == 0 ? NSTextAlignmentLeft : NSTextAlignmentRight));
            x += widths[index];
        }
        pdf.y += 19.0;
    }
    DrawRule(pdf.margin, pdf.pageWidth - pdf.margin, pdf.y, 0.9); pdf.y += 8.0;
    NSString *note = @"Note. AUC was calculated on the same observations used to fit the model and does not represent external validation.";
    DrawText(note, NSMakeRect(pdf.margin, pdf.y, available, 35.0), pdf.noteAttributes);
}

} // namespace

bool RenderBinaryRegressionAPAPDF(const core::GeneralizedGLMState &state,
                                  const std::string &path,
                                  const BinaryAPAExportOptions &options,
                                  std::string *message)
{
    const core::BinaryAPAReportModel model = core::BuildBinaryAPAReportModel(state);
    bool landscape = options.orientation == BinaryAPAPageOrientation::Landscape;
    if (options.orientation == BinaryAPAPageOrientation::Automatic) {
        for (const core::BinaryAPACoefficientRow &row : model.coefficients) {
            if (row.predictor.size() > 42) { landscape = true; break; }
        }
    }
    PDFSession pdf;
    pdf.pageWidth = landscape ? 842.0 : 595.0;
    pdf.pageHeight = landscape ? 595.0 : 842.0;
    CGRect mediaBox = CGRectMake(0, 0, pdf.pageWidth, pdf.pageHeight);
    NSURL *url = [NSURL fileURLWithPath:Text(path)];
    pdf.context = CGPDFContextCreateWithURL((__bridge CFURLRef)url, &mediaBox, nullptr);
    if (!pdf.context) {
        if (message) *message = "The APA-style PDF could not be created.";
        return false;
    }
    pdf.tableNumberAttributes = @{NSFontAttributeName:[NSFont systemFontOfSize:10.5 weight:NSFontWeightBold], NSForegroundColorAttributeName:[NSColor blackColor]};
    pdf.titleAttributes = @{NSFontAttributeName:[[NSFontManager sharedFontManager] convertFont:[NSFont systemFontOfSize:11.5] toHaveTrait:NSItalicFontMask], NSForegroundColorAttributeName:[NSColor blackColor]};
    pdf.headerAttributes = @{NSFontAttributeName:[NSFont systemFontOfSize:9.3 weight:NSFontWeightSemibold], NSForegroundColorAttributeName:[NSColor blackColor]};
    pdf.bodyAttributes = @{NSFontAttributeName:[NSFont systemFontOfSize:9.3], NSForegroundColorAttributeName:[NSColor blackColor]};
    pdf.noteAttributes = @{NSFontAttributeName:[NSFont systemFontOfSize:8.8], NSForegroundColorAttributeName:[NSColor blackColor]};
    NSString *tableNumber = Text(options.tableNumber.empty() ? "1" : options.tableNumber);
    NSString *title = Text(options.title.empty() ? model.defaultTitle : options.title);
    DrawCoefficientTable(pdf, state, model, tableNumber, title);
    int secondary = 1;
    if (options.includeTermTests) {
        DrawTermTests(pdf, model, Text(SecondaryTableNumber(options.tableNumber.empty() ? "1" : options.tableNumber, secondary++)));
    }
    if (options.includeDetailedFit) {
        DrawDetailedFit(pdf, state, Text(SecondaryTableNumber(options.tableNumber.empty() ? "1" : options.tableNumber, secondary++)));
    }
    pdf.endPage();
    CGPDFContextClose(pdf.context);
    CGContextRelease(pdf.context);
    if (message) message->clear();
    return true;
}

void ShowBinaryRegressionAPAExportPanel(const core::GeneralizedGLMState &state)
{
    const core::BinaryAPAReportModel model = core::BuildBinaryAPAReportModel(state);
    NSAlert *alert = [[NSAlert alloc] init];
    [alert setMessageText:@"APA 7 style export"];
    [alert setInformativeText:@"Choose the compact academic table content."];
    NSTextField *numberField = [NSTextField textFieldWithString:@"1"];
    NSTextField *titleField = [NSTextField textFieldWithString:Text(model.defaultTitle)];
    NSPopUpButton *orientation = [[NSPopUpButton alloc] initWithFrame:NSMakeRect(0, 0, 420, 26)];
    [orientation addItemsWithTitles:@[@"Automatic", @"Portrait", @"Landscape"]];
    NSButton *termTests = [NSButton checkboxWithTitle:@"Include likelihood-ratio tests of terms" target:nil action:nil];
    NSButton *detailedFit = [NSButton checkboxWithTitle:@"Include detailed model-fit table" target:nil action:nil];
    [termTests setState:NSControlStateValueOff]; [detailedFit setState:NSControlStateValueOff];
    NSStackView *accessory = [[NSStackView alloc] initWithFrame:NSMakeRect(0, 0, 420, 216)];
    [accessory setOrientation:NSUserInterfaceLayoutOrientationVertical];
    [accessory setAlignment:NSLayoutAttributeLeading]; [accessory setSpacing:5.0];
    for (NSView *view in @[[NSTextField labelWithString:@"Table number"], numberField,
                           [NSTextField labelWithString:@"Table title"], titleField,
                           [NSTextField labelWithString:@"Orientation"], orientation,
                           termTests, detailedFit]) [accessory addArrangedSubview:view];
    [[numberField widthAnchor] constraintEqualToConstant:420].active = YES;
    [[titleField widthAnchor] constraintEqualToConstant:420].active = YES;
    [alert setAccessoryView:accessory]; [alert addButtonWithTitle:@"Export"]; [alert addButtonWithTitle:@"Cancel"];
    if ([alert runModal] != NSAlertFirstButtonReturn) return;
    NSSavePanel *panel = [NSSavePanel savePanel];
    [panel setAllowedContentTypes:@[UTTypePDF]];
    [panel setNameFieldStringValue:[NSString stringWithFormat:@"binary_regression_%@_APA7.pdf", Text(state.response)]];
    if ([panel runModal] != NSModalResponseOK) return;
    BinaryAPAExportOptions options;
    options.tableNumber = [[numberField stringValue] UTF8String];
    options.title = [[titleField stringValue] UTF8String];
    options.orientation = [orientation indexOfSelectedItem] == 2 ? BinaryAPAPageOrientation::Landscape
        : ([orientation indexOfSelectedItem] == 1 ? BinaryAPAPageOrientation::Portrait : BinaryAPAPageOrientation::Automatic);
    options.includeTermTests = [termTests state] == NSControlStateValueOn;
    options.includeDetailedFit = [detailedFit state] == NSControlStateValueOn;
    std::string message;
    if (!RenderBinaryRegressionAPAPDF(state, [[[panel URL] path] UTF8String], options, &message)) {
        NSAlert *error = [[NSAlert alloc] init]; [error setMessageText:@"Export table"];
        [error setInformativeText:Text(message)]; [error runModal];
    }
}

} // namespace macos
} // namespace platform
} // namespace rlispstat
