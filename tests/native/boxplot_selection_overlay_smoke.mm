#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>

@interface CountingBoxplotView : BoxplotView
@property(nonatomic) NSInteger fullRedrawRequests;
@end

@implementation CountingBoxplotView
- (void)setNeedsDisplay:(BOOL)needed
{
    if (needed) self.fullRedrawRequests += 1;
    [super setNeedsDisplay:needed];
}
@end

int main()
{
    @autoreleasepool {
        [NSApplication sharedApplication];
        rlispstat::core::PlotModel model;
        model.id = "boxplot-selection-overlay-smoke";
        model.group = "boxplot-selection-overlay-group";
        model.kind = "boxplot";
        CountingBoxplotView *view =
            [[CountingBoxplotView alloc] initWithModel:&model];
        NSWindow *window = [[NSWindow alloc]
            initWithContentRect:NSMakeRect(0, 0, 720, 520)
                      styleMask:NSWindowStyleMaskTitled
                        backing:NSBackingStoreBuffered defer:NO];
        [window setContentView:view];
        view.dragging = YES;
        view.dragStart = NSMakePoint(100, 100);
        view.dragCurrent = view.dragStart;
        const NSInteger before = view.fullRedrawRequests;
        NSEvent *drag = [NSEvent mouseEventWithType:NSEventTypeLeftMouseDragged
            location:NSMakePoint(230, 180) modifierFlags:0 timestamp:0
            windowNumber:[window windowNumber] context:nil eventNumber:1
            clickCount:1 pressure:0];
        [view mouseDragged:drag];
        assert(view.fullRedrawRequests == before);
        assert(![view.gestureOverlay isHidden]);
        assert(NSWidth(view.gestureOverlay.gestureRect) > 0);
        assert(NSHeight(view.gestureOverlay.gestureRect) > 0);
        [view release];
        [window release];
    }
}
