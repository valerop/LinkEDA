#ifndef RLISPSTAT_CORE_STRING_UTILS_H
#define RLISPSTAT_CORE_STRING_UTILS_H

#include <map>
#include <string>
#include <vector>

namespace rlispstat {
namespace core {

std::string TrimCopy(const std::string &text);
std::string LowerCopy(const std::string &text);
std::string JoinStrings(const std::vector<std::string> &values, const std::string &separator);
bool AppendUniqueString(std::vector<std::string> &values, const std::string &value);
std::vector<std::string> UniqueStrings(const std::vector<std::string> &values);
std::vector<std::string> SplitTabs(const std::string &line);
std::vector<std::string> SplitLines(const std::string &text);
std::string ShellQuoteSingle(const std::string &value);

std::string TempPath(const std::string &prefix, const std::string &suffix);

template <typename T>
inline void RenameMapKey(std::map<std::string, T> &values, const std::string &oldName, const std::string &newName)
{
    auto it = values.find(oldName);
    if (it != values.end()) {
        T value = it->second;
        values.erase(it);
        values[newName] = value;
    }
}

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_STRING_UTILS_H
