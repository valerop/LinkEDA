#ifndef RLISPSTAT_CORE_SELECTION_MODEL_H
#define RLISPSTAT_CORE_SELECTION_MODEL_H

#include <map>
#include <set>
#include <string>
#include <vector>

#include "plot_geometry.h"

namespace rlispstat {
namespace core {

using CaseId = int;

enum class SelectionMode {
    Replace,
    Add,
    Subtract,
    Toggle
};

SelectionMode SelectionModeFromString(const std::string &mode,
                                      SelectionMode fallback = SelectionMode::Replace);
std::string EffectiveSelectionModeName(bool optionDown,
                                       bool commandDown,
                                       bool controlDown,
                                       bool shiftDown,
                                       const std::string &fallbackMode);

std::set<CaseId> NormalizeCaseSet(const std::set<CaseId> &cases);
std::set<CaseId> CaseSetFromVector(const std::vector<CaseId> &cases);
std::set<CaseId> CaseSetFromGroupedVectors(const std::vector<std::vector<CaseId>> &groups);
std::string CaseSetText(const std::set<CaseId> &cases,
                        const std::string &separator = " ");

struct GroupSelectionSummary {
    std::string group;
    std::size_t plotCount = 0;
    std::size_t selectedCount = 0;
};

std::string GroupSelectionsResponseText(const std::vector<GroupSelectionSummary> &groups);
std::string GroupInfoResponseText(const std::string &group,
                                  std::size_t selectedCount,
                                  const std::vector<std::string> &plotIds);

std::set<CaseId> ApplySelectionOperation(const std::set<CaseId> &current,
                                         const std::set<CaseId> &incoming,
                                         SelectionMode mode);

std::set<CaseId> InvertSelectionWithin(const std::set<CaseId> &current,
                                       const std::set<CaseId> &universe);

class SelectionModel {
public:
    const std::set<CaseId> &selectedCases() const;
    std::vector<CaseId> selectedCasesVector() const;
    int version() const;
    bool empty() const;
    std::size_t size() const;
    bool contains(CaseId caseId) const;

    bool setSelectedCases(const std::set<CaseId> &cases);
    bool mergeSelection(const std::set<CaseId> &cases, SelectionMode mode);
    bool clear();
    bool invertWithin(const std::set<CaseId> &universe);

private:
    bool assignNormalized(std::set<CaseId> next);

    std::set<CaseId> selected_;
    int version_ = 0;
};

std::set<int> VisibleRowsForModel(PlotModel *model);
size_t SelectionCount(const std::string &group,
                      const std::map<std::string, std::set<int>> &groupSelections);
std::vector<int> RowsForScope(int rowCount,
                              const std::set<int> &selectedRows,
                              const std::string &scope);

} // namespace core
} // namespace rlispstat

#endif // RLISPSTAT_CORE_SELECTION_MODEL_H
