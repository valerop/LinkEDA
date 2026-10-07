#ifndef RLISPSTAT_PLATFORM_MACOS_CONVERSIONS_H
#define RLISPSTAT_PLATFORM_MACOS_CONVERSIONS_H

#import <Cocoa/Cocoa.h>

#include "../../core/plot_geometry.h"

namespace rlispstat {
namespace platform {
namespace macos {

inline core::Point ToCorePoint(NSPoint point)
{
    return {point.x, point.y};
}

inline NSPoint ToNSPoint(const core::Point &point)
{
    return NSMakePoint(point.x, point.y);
}

inline core::Rect ToCoreRect(NSRect rect)
{
    return {rect.origin.x, rect.origin.y, rect.size.width, rect.size.height};
}

inline NSRect ToNSRect(const core::Rect &rect)
{
    return NSMakeRect(rect.x, rect.y, rect.width, rect.height);
}

} // namespace macos
} // namespace platform
} // namespace rlispstat

#endif // RLISPSTAT_PLATFORM_MACOS_CONVERSIONS_H
