#include "../../src/core/string_utils.h"

#include <cassert>
#include <string>
#include <vector>

using rlispstat::core::AppendUniqueString;
using rlispstat::core::JoinStrings;
using rlispstat::core::LowerCopy;
using rlispstat::core::ShellQuoteSingle;
using rlispstat::core::SplitLines;
using rlispstat::core::SplitTabs;
using rlispstat::core::TrimCopy;
using rlispstat::core::UniqueStrings;

int main()
{
    assert(TrimCopy("  alpha beta \t") == "alpha beta");
    assert(TrimCopy("\n\r") == "");
    assert(LowerCopy("AbC-123") == "abc-123");
    assert(JoinStrings({"a", "b", "c"}, ", ") == "a, b, c");
    assert(JoinStrings({}, ",").empty());

    std::vector<std::string> uniqueValues = {"a"};
    assert(!AppendUniqueString(uniqueValues, "a"));
    assert(AppendUniqueString(uniqueValues, "b"));
    assert(uniqueValues.size() == 2);
    std::vector<std::string> unique = UniqueStrings({"a", "", "b", "a", ""});
    assert(unique.size() == 3);
    assert(unique[0] == "a");
    assert(unique[1].empty());
    assert(unique[2] == "b");

    std::vector<std::string> tabs = SplitTabs("a\t\tc");
    assert(tabs.size() == 3);
    assert(tabs[0] == "a");
    assert(tabs[1].empty());
    assert(tabs[2] == "c");

    std::vector<std::string> lines = SplitLines("one\r\ntwo\n");
    assert(lines.size() == 2);
    assert(lines[0] == "one");
    assert(lines[1] == "two");

    assert(ShellQuoteSingle("plain") == "'plain'");
    assert(ShellQuoteSingle("a'b") == "'a'\\''b'");
    return 0;
}
