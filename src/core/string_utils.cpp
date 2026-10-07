#include "string_utils.h"

#include <algorithm>
#include <cctype>
#include <chrono>
#include <cstdlib>
#include <filesystem>
#include <sstream>
#include <thread>

namespace rlispstat {
namespace core {

std::string TrimCopy(const std::string &text)
{
    std::size_t start = 0;
    while (start < text.size() && std::isspace(static_cast<unsigned char>(text[start]))) {
        ++start;
    }
    std::size_t end = text.size();
    while (end > start && std::isspace(static_cast<unsigned char>(text[end - 1]))) {
        --end;
    }
    return text.substr(start, end - start);
}

std::string LowerCopy(const std::string &text)
{
    std::string out = text;
    std::transform(out.begin(), out.end(), out.begin(),
                   [](unsigned char ch) { return static_cast<char>(std::tolower(ch)); });
    return out;
}

std::string JoinStrings(const std::vector<std::string> &values, const std::string &separator)
{
    std::ostringstream out;
    for (std::size_t i = 0; i < values.size(); ++i) {
        if (i) {
            out << separator;
        }
        out << values[i];
    }
    return out.str();
}

bool AppendUniqueString(std::vector<std::string> &values, const std::string &value)
{
    if (std::find(values.begin(), values.end(), value) != values.end()) {
        return false;
    }
    values.push_back(value);
    return true;
}

std::vector<std::string> UniqueStrings(const std::vector<std::string> &values)
{
    std::vector<std::string> out;
    for (const std::string &value : values) {
        AppendUniqueString(out, value);
    }
    return out;
}

std::vector<std::string> SplitTabs(const std::string &line)
{
    std::vector<std::string> parts;
    std::size_t start = 0;
    while (true) {
        std::size_t pos = line.find('\t', start);
        if (pos == std::string::npos) {
            parts.push_back(line.substr(start));
            break;
        }
        parts.push_back(line.substr(start, pos - start));
        start = pos + 1;
    }
    return parts;
}

std::vector<std::string> SplitLines(const std::string &text)
{
    std::vector<std::string> lines;
    std::string line;
    std::istringstream input(text);
    while (std::getline(input, line)) {
        if (!line.empty() && line.back() == '\r') {
            line.pop_back();
        }
        lines.push_back(line);
    }
    return lines;
}

std::string ShellQuoteSingle(const std::string &value)
{
    std::string out = "'";
    for (char ch : value) {
        if (ch == '\'') {
            out += "'\\''";
        } else {
            out += ch;
        }
    }
    out += "'";
    return out;
}

std::string TempPath(const std::string &prefix, const std::string &suffix)
{
    std::filesystem::path directory;
    try {
        directory = std::filesystem::temp_directory_path();
    } catch (const std::filesystem::filesystem_error &) {
        directory = ".";
    }
    std::ostringstream out;
    out << prefix << "_"
        << std::chrono::duration_cast<std::chrono::microseconds>(
               std::chrono::system_clock::now().time_since_epoch()).count()
        << "_" << std::hash<std::thread::id>{}(std::this_thread::get_id())
        << "_" << std::rand() << suffix;
    return (directory / out.str()).string();
}

} // namespace core
} // namespace rlispstat
