// Link with the macOS backend (without backend_macos.mm's main).
#import <Cocoa/Cocoa.h>
#include <cassert>
#include "../../src/core/provenance_model.h"
@interface MacVisualTableExportTarget : NSObject
- (instancetype)initWithView:(NSView *)view title:(NSString *)title baseName:(NSString *)baseName tabDelimited:(NSString *)tabDelimited;
- (NSData *)svgData;
- (NSData *)pdfData;
- (NSData *)pngData;
- (void)setPublicationTable:(const rlispstat::core::PublicationTableSpec &)table;
@end
@interface MacTabularExportView : NSView
- (instancetype)initWithTitle:(NSString *)title tabDelimited:(NSString *)text;
@end
int main(int argc, char **) {
    NSAutoreleasePool *outer = [[NSAutoreleasePool alloc] init];
    [NSApplication sharedApplication];
    MacVisualTableExportTarget *target;
    {
        NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
        NSView *view = [[MacTabularExportView alloc] initWithTitle:@"Linear Model" tabDelimited:@"Variable\tb\nTreatment\t-0.611\n"];
        target = [[MacVisualTableExportTarget alloc] initWithView:view
            title:[NSString stringWithFormat:@"Linear %@", @"Model"]
            baseName:[NSString stringWithFormat:@"general-%@-model", @"linear"]
            tabDelimited:@"Variable\tb\nTreatment\t-0.611\n"];
        [view release];
        [pool drain];
    }
    // Save panels run a nested event loop after the menu's autorelease pool
    // drains. All of the target's inputs must still be alive at that point.
    NSString *name = [[target valueForKey:@"exportBaseName"] stringByAppendingPathExtension:@"svg"];
    assert([name isEqualToString:@"general-linear-model.svg"]);
    NSData *svg = [target svgData];
    assert([svg length] > 0);
    NSString *text = [[[NSString alloc] initWithData:svg encoding:NSUTF8StringEncoding] autorelease];
    assert([text containsString:@"Treatment"]);
    assert([svg writeToFile:@"/tmp/linkeda-linear-export-lifetime.svg" atomically:YES]);
    NSData *png = [target pngData];
    assert([png length] > 0);
    assert([png writeToFile:@"/tmp/linkeda-linear-export-lifetime.png" atomically:YES]);
    NSBitmapImageRep *bitmap = [NSBitmapImageRep imageRepWithData:png];
    assert(bitmap && [bitmap pixelsWide] >= 1000);
    size_t ink = 0;
    for (NSInteger y = 0; y < [bitmap pixelsHigh]; y += 2)
        for (NSInteger x = 0; x < [bitmap pixelsWide]; x += 2) {
            NSColor *pixel = [[bitmap colorAtX:x y:y] colorUsingColorSpace:[NSColorSpace genericRGBColorSpace]];
            if ([pixel alphaComponent] > .5 && [pixel redComponent] < .5) ++ink;
        }
    assert(ink > 100); // A valid PNG header or an all-white image is not enough.
    // Optional integration check: requires R, tinytable and a TeX engine.
    if (argc > 1) {
        using namespace rlispstat::core;
        PublicationTableSpec table;
        table.title = "Linear model export";
        table.columns = {{"term", "Variable", "", -1, false}, {"b", "b", "", 4, false}};
        table.rows = {{{PublicationValueKind::Text, 0, "Treatment", false},
                       {PublicationValueKind::Number, -.611, {}, false}}};
        [target setPublicationTable:table];
        NSData *pdf = [target pdfData];
        assert([pdf length] > 5);
        assert(std::string(static_cast<const char *>([pdf bytes]), 5) == "%PDF-");
    }
    [target release];
    [outer drain];
}
