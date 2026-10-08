#include "../../src/platform/macos/linkeda_macos_app.mm"
#include <cassert>

int main(){@autoreleasepool{
    [NSApplication sharedApplication];
    DendrogramState state;state.id="viewport-test";state.group="286 cases";
    for(int i=0;i<286;++i){state.caseRows.push_back(i+1);state.leafOrder.push_back(i);}
    for(int i=0;i<285;++i){
        rlispstat::core::DendrogramMergeModel merge;merge.left=i==0?0:286+i-1;
        merge.right=i+1;merge.height=(i+1)/285.0;state.merges.push_back(merge);
    }
    g_dendrograms[state.id]=state;
    auto controller=[[DendrogramWindowController alloc]initWithDendrogramId:state.id];
    [controller refresh];
    NSWindow *window=[controller valueForKey:@"window"];
    NSScrollView *scroll=[controller valueForKey:@"scrollView"];
    DendrogramView *view=[controller valueForKey:@"dendrogramView"];
    NSButton *add=[controller valueForKey:@"variablesButton"];
    assert([add.title isEqualToString:@"+ Add variable"] && !add.bordered);
    for(NSValue *sizeValue in @[[NSValue valueWithSize:NSMakeSize(700,420)],
                              [NSValue valueWithSize:NSMakeSize(1150,650)]]){
        NSSize size=sizeValue.sizeValue;
        [window setContentSize:size];[controller refresh];
        assert(NSEqualSizes(window.contentView.frame.size,size));
        assert(NSEqualSizes(view.frame.size,scroll.contentSize));
        assert(NSMaxX(add.frame)<=size.width && NSMinX(add.frame)==16);
        auto geometry=rlispstat::core::BuildDendrogramPlotGeometry(state.caseRows,state.merges,
            state.leafOrder,view.frame.size.width,view.frame.size.height);
        for(auto const& branch:geometry.branches){
            assert(branch.start.x>=0 && branch.end.x<scroll.contentSize.width);
            assert(branch.start.y>=0 && branch.end.y<scroll.contentSize.height);
        }
    }
    [controller toggleFitTreeToWindow:nil];
    assert(view.frame.size.width>scroll.contentSize.width);
    assert(scroll.hasHorizontalScroller);
    [controller toggleFitTreeToWindow:nil];
    assert(NSEqualSizes(view.frame.size,scroll.contentSize));
    // Rotation must still fit the complete tree without resizing the window.
    g_dendrograms[state.id].rotate270=true;[controller refresh];
    assert(NSEqualSizes(view.frame.size,scroll.contentSize));
    g_dendrograms[state.id].rotate270=false;[controller refresh];
    NSData *pdf=[window.contentView dataWithPDFInsideRect:window.contentView.bounds];
    assert(pdf.length>1000);[pdf writeToFile:@"/tmp/linkeda-dendrogram-viewport.pdf" atomically:YES];
    [window setDelegate:nil];[window close];
}}
