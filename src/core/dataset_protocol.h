#ifndef RLISPSTAT_CORE_DATASET_PROTOCOL_H
#define RLISPSTAT_CORE_DATASET_PROTOCOL_H

#include "dataset_model.h"

#include <cstddef>
#include <iosfwd>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

bool ParseDataFramePayload(const std::vector<std::string> &lines,
                           std::size_t &cursor,
                           const std::string &group,
                           DataFrameModel &out,
                           bool *parsed,
                           std::string &error);
void WriteDataFramePayloadForR(std::ostream &out,
                               const DataFrameModel &df,
                               bool includeImputation = false);

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_DATASET_PROTOCOL_H
