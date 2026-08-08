#include "selection_model.h"

#include <algorithm>
#include <sstream>

namespace rlispstat {
namespace core {

SelectionMode SelectionModeFromString(const std::string &mode, SelectionMode fallback)
{
    if (mode == "replace") return SelectionMode::Replace;
    if (mode == "add") return SelectionMode::Add;
    if (mode == "subtract") return SelectionMode::Subtract;
    if (mode == "toggle") return SelectionMode::Toggle;
    return fallback;
}

std::string EffectiveSelectionModeName(bool optionDown,
                                       bool commandDown,
                                       bool controlDown,
                                       bool shiftDown,
                                       const std::string &fallbackMode)
{
    if (optionDown) {
        return "subtract";
    }
    if (commandDown || controlDown) {
        return "toggle";
    }
    if (shiftDown) {
        return "add";
    }
    return fallbackMode;
}

std::set<CaseId> NormalizeCaseSet(const std::set<CaseId> &cases)
{
    std::set<CaseId> normalized;
    for (CaseId caseId : cases) {
        if (caseId > 0) {
            normalized.insert(caseId);
        }
    }
    return normalized;
}

std::set<CaseId> CaseSetFromVector(const std::vector<CaseId> &cases)
{
    std::set<CaseId> rows;
    for (CaseId caseId : cases) {
        if (caseId > 0) {
            rows.insert(caseId);
        }
    }
    return rows;
}

std::set<CaseId> CaseSetFromGroupedVectors(const std::vector<std::vector<CaseId>> &groups)
{
    std::set<CaseId> rows;
    for (const std::vector<CaseId> &group : groups) {
        for (CaseId caseId : group) {
            if (caseId > 0) {
                rows.insert(caseId);
            }
        }
    }
    return rows;
}

std::string CaseSetText(const std::set<CaseId> &cases, const std::string &separator)
{
    std::ostringstream out;
    bool first = true;
    for (CaseId caseId : NormalizeCaseSet(cases)) {
        if (!first) {
            out << separator;
        }
        out << caseId;
        first = false;
    }
    return out.str();
}

std::string GroupSelectionsResponseText(const std::vector<GroupSelectionSummary> &groups)
{
    std::ostringstream out;
    out << "OK";
    for (const GroupSelectionSummary &group : groups) {
        out << "\t" << group.group << "|" << group.plotCount << "|" << group.selectedCount;
    }
    return out.str();
}

std::string GroupInfoResponseText(const std::string &group,
                                  std::size_t selectedCount,
                                  const std::vector<std::string> &plotIds)
{
    std::ostringstream out;
    out << "OK\t" << group << "|" << selectedCount;
    for (const std::string &plotId : plotIds) {
        out << "|" << plotId;
    }
    return out.str();
}

std::set<CaseId> ApplySelectionOperation(const std::set<CaseId> &current,
                                         const std::set<CaseId> &incoming,
                                         SelectionMode mode)
{
    std::set<CaseId> next = NormalizeCaseSet(current);
    std::set<CaseId> rows = NormalizeCaseSet(incoming);
    if (mode == SelectionMode::Add) {
        next.insert(rows.begin(), rows.end());
    } else if (mode == SelectionMode::Subtract) {
        for (CaseId row : rows) {
            next.erase(row);
        }
    } else if (mode == SelectionMode::Toggle) {
        for (CaseId row : rows) {
            auto it = next.find(row);
            if (it == next.end()) {
                next.insert(row);
            } else {
                next.erase(it);
            }
        }
    } else {
        next = rows;
    }
    return next;
}

std::set<CaseId> InvertSelectionWithin(const std::set<CaseId> &current,
                                       const std::set<CaseId> &universe)
{
    std::set<CaseId> normalizedCurrent = NormalizeCaseSet(current);
    std::set<CaseId> normalizedUniverse = NormalizeCaseSet(universe);
    std::set<CaseId> inverted;
    for (CaseId caseId : normalizedUniverse) {
        if (normalizedCurrent.find(caseId) == normalizedCurrent.end()) {
            inverted.insert(caseId);
        }
    }
    return inverted;
}

const std::set<CaseId> &SelectionModel::selectedCases() const
{
    return selected_;
}

std::vector<CaseId> SelectionModel::selectedCasesVector() const
{
    return std::vector<CaseId>(selected_.begin(), selected_.end());
}

int SelectionModel::version() const
{
    return version_;
}

bool SelectionModel::empty() const
{
    return selected_.empty();
}

std::size_t SelectionModel::size() const
{
    return selected_.size();
}

bool SelectionModel::contains(CaseId caseId) const
{
    return selected_.find(caseId) != selected_.end();
}

bool SelectionModel::setSelectedCases(const std::set<CaseId> &cases)
{
    return assignNormalized(NormalizeCaseSet(cases));
}

bool SelectionModel::mergeSelection(const std::set<CaseId> &cases, SelectionMode mode)
{
    return assignNormalized(ApplySelectionOperation(selected_, cases, mode));
}

bool SelectionModel::clear()
{
    return assignNormalized(std::set<CaseId>());
}

bool SelectionModel::invertWithin(const std::set<CaseId> &universe)
{
    return assignNormalized(InvertSelectionWithin(selected_, universe));
}

bool SelectionModel::assignNormalized(std::set<CaseId> next)
{
    next = NormalizeCaseSet(next);
    if (next == selected_) {
        return false;
    }
    selected_ = std::move(next);
    ++version_;
    return true;
}

std::set<int> VisibleRowsForModel(PlotModel *model)
{
    if (!model || !PlotLinksToDataRows(*model)) {
        return std::set<int>();
    }
    if (model->kind == "boxplot") {
        std::vector<CaseId> rows;
        rows.reserve(model->boxplotPoints.size());
        for (const BoxplotPoint &p : model->boxplotPoints) {
            rows.push_back(p.row);
        }
        return CaseSetFromVector(rows);
    } else if (model->kind == "barplot") {
        std::vector<std::vector<CaseId>> groups;
        groups.reserve(model->barplotBins.size());
        for (const BarplotBin &bin : model->barplotBins) {
            groups.push_back(std::vector<CaseId>(bin.rows.begin(), bin.rows.end()));
        }
        return CaseSetFromGroupedVectors(groups);
    } else {
        std::vector<CaseId> rows;
        rows.reserve(model->points.size());
        for (const DataPoint &p : model->points) {
            rows.push_back(p.row);
        }
        return CaseSetFromVector(rows);
    }
}

size_t SelectionCount(const std::string &group,
                      const std::map<std::string, std::set<int>> &groupSelections)
{
    auto it = groupSelections.find(group);
    return it == groupSelections.end() ? 0 : it->second.size();
}

std::vector<int> RowsForScope(int rowCount,
                              const std::set<int> &selectedRows,
                              const std::string &scope)
{
    std::vector<int> rows;
    if (scope == "all" || rowCount <= 0) {
        return rows;
    }
    if (scope != "selected" && scope != "unselected") {
        return rows;
    }
    std::set<int> selected = NormalizeCaseSet(selectedRows);
    for (int row = 1; row <= rowCount; ++row) {
        bool isSelected = selected.find(row) != selected.end();
        if ((scope == "selected" && isSelected) ||
            (scope == "unselected" && !isSelected)) {
            rows.push_back(row);
        }
    }
    return rows;
}

} // namespace core
} // namespace rlispstat
