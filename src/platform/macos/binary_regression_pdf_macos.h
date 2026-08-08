#ifndef RLISPSTAT_PLATFORM_MACOS_BINARY_REGRESSION_PDF_H
#define RLISPSTAT_PLATFORM_MACOS_BINARY_REGRESSION_PDF_H

#include "../../core/glm_model.h"

#include <string>

namespace rlispstat {
namespace platform {
namespace macos {

enum class BinaryAPAPageOrientation { Automatic, Portrait, Landscape };

struct BinaryAPAExportOptions {
    std::string tableNumber = "1";
    std::string title;
    BinaryAPAPageOrientation orientation = BinaryAPAPageOrientation::Automatic;
    bool includeTermTests = false;
    bool includeDetailedFit = false;
};

bool RenderBinaryRegressionAPAPDF(const core::GeneralizedGLMState &state,
                                  const std::string &path,
                                  const BinaryAPAExportOptions &options,
                                  std::string *message = nullptr);

void ShowBinaryRegressionAPAExportPanel(const core::GeneralizedGLMState &state);

} // namespace macos
} // namespace platform
} // namespace rlispstat

#endif
