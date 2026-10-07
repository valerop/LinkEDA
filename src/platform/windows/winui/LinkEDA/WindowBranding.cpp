#include "pch.h"
#include "WindowBranding.h"

#include <microsoft.ui.xaml.window.h>
#include <winrt/Windows.ApplicationModel.h>
#include <winrt/Windows.Storage.h>

#include <algorithm>
#include <array>
#include <cmath>
#include <filesystem>
#include <string>
#include <functional>
#include <fstream>
#include <iomanip>
#include <iterator>
#include <locale>
#include <memory>
#include <optional>
#include <set>
#include <sstream>
#include <vector>

namespace
{
    std::function<void(std::string const&)> g_showDataSheet;
    std::function<void(std::string const&, std::string const&, std::string const&)>
        g_addSnapshot;
    std::function<void()> g_showSnapshotAlbum;

    struct SnapshotWindowSource
    {
        winrt::weak_ref<winrt::Microsoft::UI::Xaml::FrameworkElement> root;
        winrt::weak_ref<winrt::Microsoft::UI::Xaml::Window> window;
        std::string kind;
        std::string sourceId;
        std::string group;
        ::rlispstat::core::WindowNote note;
        std::vector<::rlispstat::core::WindowStickyNote> stickers;
        int activeSticker = -1;
        int stickerDrag = -1;
        int anchorDrag = -1;
        double dragStartX = 0.0;
        double dragStartY = 0.0;
        double originalX = 0.0;
        double originalY = 0.0;
        std::function<void(
            std::vector<::rlispstat::core::WindowStickyNote> const&)> stickersChanged;
        winrt::Microsoft::UI::Xaml::Controls::ColumnDefinition notesColumn{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::Border notesPanel{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::TextBox notesEditor{ nullptr };
        winrt::Microsoft::UI::Xaml::Controls::CheckBox notesInclude{ nullptr };
        bool updatingNote = false;
    };

    std::vector<SnapshotWindowSource> g_snapshotSources;

    struct ContextMenuBinding
    {
        winrt::weak_ref<winrt::Microsoft::UI::Xaml::FrameworkElement> root;
        winrt::Microsoft::UI::Xaml::Controls::MenuFlyout menu{ nullptr };
        std::vector<winrt::weak_ref<winrt::Microsoft::UI::Xaml::UIElement>> hooked;
    };

    std::vector<std::shared_ptr<ContextMenuBinding>> g_contextMenus;
    std::vector<winrt::weak_ref<winrt::Microsoft::UI::Xaml::Window>> g_windows;

    void ApplyFontToVisualTree(
        winrt::Microsoft::UI::Xaml::DependencyObject const& node,
        winrt::Microsoft::UI::Xaml::Media::FontFamily const& family)
    {
        using namespace winrt::Microsoft::UI::Xaml;
        using namespace winrt::Microsoft::UI::Xaml::Controls;
        if (!node) return;

        if (auto control = node.try_as<Control>()) control.FontFamily(family);
        else if (auto text = node.try_as<TextBlock>()) text.FontFamily(family);

        const int count = Media::VisualTreeHelper::GetChildrenCount(node);
        for (int index = 0; index < count; ++index)
            ApplyFontToVisualTree(
                Media::VisualTreeHelper::GetChild(node, index), family);
    }

    void CollectSnapshotText(
        winrt::Microsoft::UI::Xaml::DependencyObject const& node,
        std::vector<std::string>& lines)
    {
        using namespace winrt;
        using namespace Microsoft::UI::Xaml;
        using namespace Microsoft::UI::Xaml::Controls;
        if (!node) return;
        if (auto element = node.try_as<FrameworkElement>(); element &&
            element.Name() == L"LinkEDA.TableNotesPanel") return;
        if (auto text = node.try_as<TextBlock>())
        {
            const std::string value = to_string(text.Text());
            if (!value.empty()) lines.push_back(value);
        }
        else if (auto textBox = node.try_as<TextBox>())
        {
            const std::string value = to_string(textBox.Text());
            if (!value.empty()) lines.push_back(value);
        }
        else if (auto panel = node.try_as<Panel>())
        {
            for (auto const& child : panel.Children())
                CollectSnapshotText(child, lines);
        }
        else if (auto border = node.try_as<Border>())
            CollectSnapshotText(border.Child(), lines);
        else if (auto scroll = node.try_as<ScrollViewer>())
            CollectSnapshotText(scroll.Content().try_as<DependencyObject>(), lines);
        else if (auto content = node.try_as<ContentControl>())
            CollectSnapshotText(content.Content().try_as<DependencyObject>(), lines);
    }

    std::string SnapshotElementText(
        winrt::Microsoft::UI::Xaml::DependencyObject const& node)
    {
        std::vector<std::string> lines;
        CollectSnapshotText(node, lines);
        std::string result;
        for (auto const& line : lines)
        {
            if (!result.empty()) result += " ";
            result += line;
        }
        return result;
    }

    std::vector<std::string> SnapshotGridRow(
        winrt::Microsoft::UI::Xaml::Controls::Grid const& grid)
    {
        using namespace winrt::Microsoft::UI::Xaml;
        using namespace winrt::Microsoft::UI::Xaml::Controls;
        std::size_t columns = grid.ColumnDefinitions().Size();
        for (auto const& child : grid.Children())
            if (auto element = child.try_as<FrameworkElement>())
                columns = std::max(columns,
                    static_cast<std::size_t>(Grid::GetColumn(element)) + 1);
        if (columns < 2) return {};
        std::vector<std::string> row(columns);
        for (auto const& child : grid.Children())
        {
            auto element = child.try_as<FrameworkElement>();
            if (!element) continue;
            const auto column = static_cast<std::size_t>(Grid::GetColumn(element));
            if (column >= row.size()) continue;
            const auto value = SnapshotElementText(child);
            if (!value.empty())
            {
                if (!row[column].empty()) row[column] += " ";
                row[column] += value;
            }
        }
        return row;
    }

    void ConsiderSnapshotTable(
        std::vector<std::vector<std::string>> rows,
        std::vector<std::vector<std::string>>& best,
        std::size_t& bestScore)
    {
        if (rows.size() < 2) return;
        std::size_t columns = 0, populated = 0;
        for (auto const& row : rows)
        {
            columns = std::max(columns, row.size());
            populated += static_cast<std::size_t>(std::count_if(
                row.begin(), row.end(), [](auto const& value) { return !value.empty(); }));
        }
        if (columns < 2 || populated < 4) return;
        const std::size_t score = populated + rows.size() * columns;
        if (score > bestScore)
        {
            bestScore = score;
            best = std::move(rows);
        }
    }

    void CollectSnapshotTableCandidates(
        winrt::Microsoft::UI::Xaml::DependencyObject const& node,
        std::vector<std::vector<std::string>>& best,
        std::size_t& bestScore)
    {
        using namespace winrt::Microsoft::UI::Xaml;
        using namespace winrt::Microsoft::UI::Xaml::Controls;
        if (!node) return;
        if (auto element = node.try_as<FrameworkElement>(); element &&
            element.Name() == L"LinkEDA.TableNotesPanel") return;
        if (auto grid = node.try_as<Grid>())
        {
            std::size_t rowCount = grid.RowDefinitions().Size();
            std::size_t columnCount = grid.ColumnDefinitions().Size();
            for (auto const& child : grid.Children())
            {
                auto element = child.try_as<FrameworkElement>();
                if (!element) continue;
                rowCount = std::max(rowCount,
                    static_cast<std::size_t>(Grid::GetRow(element)) + 1);
                columnCount = std::max(columnCount,
                    static_cast<std::size_t>(Grid::GetColumn(element)) + 1);
            }
            if (rowCount >= 2 && columnCount >= 2)
            {
                std::vector<std::vector<std::string>> rows(
                    rowCount, std::vector<std::string>(columnCount));
                for (auto const& child : grid.Children())
                {
                    auto element = child.try_as<FrameworkElement>();
                    if (!element) continue;
                    const auto row = static_cast<std::size_t>(Grid::GetRow(element));
                    const auto column = static_cast<std::size_t>(Grid::GetColumn(element));
                    if (row >= rows.size() || column >= rows[row].size()) continue;
                    const auto value = SnapshotElementText(child);
                    if (!value.empty())
                    {
                        if (!rows[row][column].empty()) rows[row][column] += " ";
                        rows[row][column] += value;
                    }
                }
                ConsiderSnapshotTable(std::move(rows), best, bestScore);
            }
        }
        if (auto panel = node.try_as<Panel>())
        {
            std::vector<std::vector<std::string>> gridRows;
            for (auto const& child : panel.Children())
            {
                if (auto rowGrid = child.try_as<Grid>())
                {
                    auto row = SnapshotGridRow(rowGrid);
                    if (!row.empty()) gridRows.push_back(std::move(row));
                }
                CollectSnapshotTableCandidates(child, best, bestScore);
            }
            ConsiderSnapshotTable(std::move(gridRows), best, bestScore);
        }
        else if (auto border = node.try_as<Border>())
            CollectSnapshotTableCandidates(border.Child(), best, bestScore);
        else if (auto scroll = node.try_as<ScrollViewer>())
            CollectSnapshotTableCandidates(
                scroll.Content().try_as<DependencyObject>(), best, bestScore);
        else if (auto content = node.try_as<ContentControl>())
            CollectSnapshotTableCandidates(
                content.Content().try_as<DependencyObject>(), best, bestScore);
    }

    SnapshotWindowSource* SnapshotSourceForRoot(
        winrt::Microsoft::UI::Xaml::FrameworkElement const& root)
    {
        for (auto iterator = g_snapshotSources.begin(); iterator != g_snapshotSources.end();)
        {
            if (auto current = iterator->root.get())
            {
                if (winrt::get_abi(current) == winrt::get_abi(root)) return &*iterator;
                ++iterator;
            }
            else iterator = g_snapshotSources.erase(iterator);
        }
        return nullptr;
    }

    bool SameElement(
        winrt::Microsoft::UI::Xaml::UIElement const& left,
        winrt::Microsoft::UI::Xaml::UIElement const& right)
    {
        return left && right && winrt::get_abi(left) == winrt::get_abi(right);
    }

    winrt::Microsoft::UI::Xaml::FrameworkElement FindSnapshotTableVisual(
        winrt::Microsoft::UI::Xaml::DependencyObject const& node)
    {
        using namespace winrt::Microsoft::UI::Xaml;
        if (!node) return nullptr;
        if (auto element = node.try_as<FrameworkElement>(); element &&
            element.Name() == L"LinkEDA.SnapshotTable")
            return element;
        const int count = Media::VisualTreeHelper::GetChildrenCount(node);
        for (int index = 0; index < count; ++index)
            if (auto found = FindSnapshotTableVisual(
                    Media::VisualTreeHelper::GetChild(node, index)))
                return found;
        return nullptr;
    }

    bool ContainsVisualElement(
        winrt::Microsoft::UI::Xaml::DependencyObject const& root,
        winrt::Microsoft::UI::Xaml::UIElement const& sought)
    {
        using namespace winrt::Microsoft::UI::Xaml;
        if (!root || !sought) return false;
        if (auto element = root.try_as<UIElement>(); SameElement(element, sought))
            return true;
        const int count = Media::VisualTreeHelper::GetChildrenCount(root);
        for (int index = 0; index < count; ++index)
            if (ContainsVisualElement(
                    Media::VisualTreeHelper::GetChild(root, index), sought))
                return true;
        return false;
    }

    std::string SnapshotSvgEscape(std::string const& value)
    {
        std::string escaped;
        escaped.reserve(value.size() + value.size() / 8);
        for (char character : value)
        {
            switch (character)
            {
            case '&': escaped += "&amp;"; break;
            case '<': escaped += "&lt;"; break;
            case '>': escaped += "&gt;"; break;
            case '\"': escaped += "&quot;"; break;
            case '\'': escaped += "&apos;"; break;
            default: escaped.push_back(character); break;
            }
        }
        return escaped;
    }

    std::string SnapshotSvgNumber(double value)
    {
        if (!std::isfinite(value)) value = 0.0;
        std::ostringstream output;
        output.imbue(std::locale::classic());
        output << std::fixed << std::setprecision(3) << value;
        std::string result = output.str();
        while (result.size() > 1 && result.back() == '0') result.pop_back();
        if (!result.empty() && result.back() == '.') result.pop_back();
        return result;
    }

    std::string SnapshotBase64(std::vector<unsigned char> const& data)
    {
        static constexpr char alphabet[] =
            "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
        std::string encoded;
        encoded.reserve((data.size() + 2) / 3 * 4);
        for (std::size_t index = 0; index < data.size(); index += 3)
        {
            const unsigned int a = data[index];
            const unsigned int b = index + 1 < data.size() ? data[index + 1] : 0;
            const unsigned int c = index + 2 < data.size() ? data[index + 2] : 0;
            const unsigned int value = (a << 16) | (b << 8) | c;
            encoded.push_back(alphabet[(value >> 18) & 63]);
            encoded.push_back(alphabet[(value >> 12) & 63]);
            encoded.push_back(index + 1 < data.size()
                ? alphabet[(value >> 6) & 63] : '=');
            encoded.push_back(index + 2 < data.size()
                ? alphabet[value & 63] : '=');
        }
        return encoded;
    }

    struct SnapshotFontSource
    {
        std::string family;
        std::string assetPath;
    };

    std::optional<SnapshotFontSource> SnapshotFontFromFamily(
        winrt::Microsoft::UI::Xaml::Media::FontFamily const& font)
    {
        if (!font || font.Source().empty()) return std::nullopt;
        const std::string source = winrt::to_string(font.Source());
        const auto separator = source.rfind('#');
        if (separator == std::string::npos || separator + 1 >= source.size())
            return std::nullopt;
        static constexpr char prefix[] = "ms-appx:///";
        if (source.rfind(prefix, 0) != 0) return std::nullopt;
        return SnapshotFontSource{
            source.substr(separator + 1),
            source.substr(sizeof(prefix) - 1, separator - (sizeof(prefix) - 1)) };
    }

    void CollectSnapshotFonts(
        winrt::Microsoft::UI::Xaml::DependencyObject const& node,
        std::vector<SnapshotFontSource>& fonts,
        std::set<std::string>& seen)
    {
        using namespace winrt::Microsoft::UI::Xaml;
        using namespace winrt::Microsoft::UI::Xaml::Controls;
        if (!node) return;
        if (auto element = node.try_as<FrameworkElement>();
            !element || element.Visibility() != Visibility::Visible)
            return;
        if (node.try_as<Button>() || node.try_as<ComboBox>() ||
            node.try_as<CheckBox>() || node.try_as<TextBox>())
            return;
        if (auto text = node.try_as<TextBlock>())
        {
            try
            {
                if (auto source = SnapshotFontFromFamily(text.FontFamily());
                    source && seen.insert(source->family).second)
                    fonts.push_back(std::move(*source));
            }
            catch (...) {}
        }
        const int count = Media::VisualTreeHelper::GetChildrenCount(node);
        for (int index = 0; index < count; ++index)
            CollectSnapshotFonts(Media::VisualTreeHelper::GetChild(node, index),
                fonts, seen);
    }

    void AppendSnapshotEmbeddedFonts(
        winrt::Microsoft::UI::Xaml::FrameworkElement const& root,
        std::ostringstream& output)
    {
        std::vector<SnapshotFontSource> fonts;
        std::set<std::string> seen;
        CollectSnapshotFonts(root, fonts, seen);
        if (fonts.empty()) return;
        try
        {
            const std::filesystem::path installed =
                winrt::Windows::ApplicationModel::Package::Current()
                    .InstalledLocation().Path().c_str();
            std::ostringstream css;
            bool any = false;
            for (auto const& font : fonts)
            {
                std::filesystem::path asset = installed;
                std::string relative = font.assetPath;
                std::replace(relative.begin(), relative.end(), '/', '\\');
                asset /= winrt::to_hstring(relative).c_str();
                std::ifstream input(asset, std::ios::binary);
                if (!input) continue;
                std::vector<unsigned char> bytes(
                    std::istreambuf_iterator<char>(input), {});
                if (bytes.empty()) continue;
                any = true;
                css << "@font-face{font-family:'" << font.family
                    << "';src:url(data:font/ttf;base64,"
                    << SnapshotBase64(bytes)
                    << ") format('truetype');font-style:normal;"
                       "font-weight:100 900;font-display:block;}";
            }
            if (any) output << "<defs><style><![CDATA[" << css.str()
                << "]]></style></defs>";
        }
        catch (...) {}
    }

    struct SnapshotSvgPaint
    {
        std::string color = "none";
        double opacity = 0.0;
    };

    SnapshotSvgPaint SnapshotBrushPaint(
        winrt::Microsoft::UI::Xaml::Media::Brush const& brush,
        double inheritedOpacity)
    {
        using namespace winrt::Microsoft::UI::Xaml::Media;
        auto solid = brush.try_as<SolidColorBrush>();
        if (!solid) return {};
        const auto color = solid.Color();
        std::ostringstream hex;
        hex << '#' << std::hex << std::setfill('0')
            << std::setw(2) << static_cast<int>(color.R)
            << std::setw(2) << static_cast<int>(color.G)
            << std::setw(2) << static_cast<int>(color.B);
        return { hex.str(), std::clamp(inheritedOpacity * brush.Opacity() *
            static_cast<double>(color.A) / 255.0, 0.0, 1.0) };
    }

    void AppendSnapshotPaint(std::ostringstream& output,
                             char const* attribute,
                             SnapshotSvgPaint const& paint)
    {
        output << ' ' << attribute << "=\"" << paint.color << "\"";
        if (paint.color != "none" && paint.opacity < 0.999)
            output << ' ' << attribute << "-opacity=\""
                << SnapshotSvgNumber(paint.opacity) << "\"";
    }

    winrt::Windows::Foundation::Point SnapshotPointInRoot(
        winrt::Microsoft::UI::Xaml::UIElement const& element,
        winrt::Microsoft::UI::Xaml::FrameworkElement const& root,
        double x,
        double y)
    {
        try
        {
            return element.TransformToVisual(root).TransformPoint(
                winrt::Windows::Foundation::Point{
                    static_cast<float>(x), static_cast<float>(y) });
        }
        catch (...)
        {
            return { static_cast<float>(x), static_cast<float>(y) };
        }
    }

    void AppendSnapshotVectorNode(
        winrt::Microsoft::UI::Xaml::DependencyObject const& node,
        winrt::Microsoft::UI::Xaml::FrameworkElement const& root,
        std::ostringstream& output,
        double inheritedOpacity)
    {
        using namespace winrt;
        using namespace Microsoft::UI::Xaml;
        using namespace Microsoft::UI::Xaml::Controls;
        using namespace Microsoft::UI::Xaml::Shapes;
        if (!node) return;
        auto element = node.try_as<FrameworkElement>();
        if (!element || element.Visibility() != Visibility::Visible) return;
        const double opacity = std::clamp(
            inheritedOpacity * element.Opacity(), 0.0, 1.0);
        if (opacity <= 0.001) return;

        // Interactive affordances are deliberately not part of a table
        // snapshot, even when a transparent button is laid over the table.
        if (node.try_as<Button>() || node.try_as<ComboBox>() ||
            node.try_as<CheckBox>() || node.try_as<TextBox>())
            return;

        const double width = std::max(0.0, element.ActualWidth());
        const double height = std::max(0.0, element.ActualHeight());
        const auto origin = SnapshotPointInRoot(element, root, 0.0, 0.0);

        if (auto border = node.try_as<Border>())
        {
            const auto fill = SnapshotBrushPaint(border.Background(), opacity);
            if (fill.color != "none" && width > 0.0 && height > 0.0)
            {
                output << "<rect x=\"" << SnapshotSvgNumber(origin.X)
                    << "\" y=\"" << SnapshotSvgNumber(origin.Y)
                    << "\" width=\"" << SnapshotSvgNumber(width)
                    << "\" height=\"" << SnapshotSvgNumber(height) << "\"";
                AppendSnapshotPaint(output, "fill", fill);
                const auto radius = border.CornerRadius();
                if (radius.TopLeft > 0.0)
                    output << " rx=\"" << SnapshotSvgNumber(radius.TopLeft) << "\"";
                output << "/>";
            }
            const auto stroke = SnapshotBrushPaint(border.BorderBrush(), opacity);
            const auto thickness = border.BorderThickness();
            auto line = [&](double x1, double y1, double x2, double y2, double size)
            {
                if (stroke.color == "none" || size <= 0.0) return;
                output << "<line x1=\"" << SnapshotSvgNumber(x1)
                    << "\" y1=\"" << SnapshotSvgNumber(y1)
                    << "\" x2=\"" << SnapshotSvgNumber(x2)
                    << "\" y2=\"" << SnapshotSvgNumber(y2) << "\"";
                AppendSnapshotPaint(output, "stroke", stroke);
                output << " stroke-width=\"" << SnapshotSvgNumber(size) << "\"/>";
            };
            line(origin.X, origin.Y, origin.X + width, origin.Y, thickness.Top);
            line(origin.X + width, origin.Y, origin.X + width,
                origin.Y + height, thickness.Right);
            line(origin.X, origin.Y + height, origin.X + width,
                origin.Y + height, thickness.Bottom);
            line(origin.X, origin.Y, origin.X, origin.Y + height, thickness.Left);
        }
        else if (auto panel = node.try_as<Panel>())
        {
            const auto fill = SnapshotBrushPaint(panel.Background(), opacity);
            if (fill.color != "none" && width > 0.0 && height > 0.0)
            {
                output << "<rect x=\"" << SnapshotSvgNumber(origin.X)
                    << "\" y=\"" << SnapshotSvgNumber(origin.Y)
                    << "\" width=\"" << SnapshotSvgNumber(width)
                    << "\" height=\"" << SnapshotSvgNumber(height) << "\"";
                AppendSnapshotPaint(output, "fill", fill);
                output << "/>";
            }
        }

        if (auto rectangle = node.try_as<
                Microsoft::UI::Xaml::Shapes::Rectangle>())
        {
            const auto fill = SnapshotBrushPaint(rectangle.Fill(), opacity);
            const auto stroke = SnapshotBrushPaint(rectangle.Stroke(), opacity);
            output << "<rect x=\"" << SnapshotSvgNumber(origin.X)
                << "\" y=\"" << SnapshotSvgNumber(origin.Y)
                << "\" width=\"" << SnapshotSvgNumber(width)
                << "\" height=\"" << SnapshotSvgNumber(height) << "\"";
            AppendSnapshotPaint(output, "fill", fill);
            AppendSnapshotPaint(output, "stroke", stroke);
            if (stroke.color != "none")
                output << " stroke-width=\"" << SnapshotSvgNumber(
                    rectangle.StrokeThickness()) << "\"";
            if (rectangle.RadiusX() > 0.0)
                output << " rx=\"" << SnapshotSvgNumber(rectangle.RadiusX()) << "\"";
            output << "/>";
        }
        else if (auto ellipse = node.try_as<
                     Microsoft::UI::Xaml::Shapes::Ellipse>())
        {
            const auto fill = SnapshotBrushPaint(ellipse.Fill(), opacity);
            const auto stroke = SnapshotBrushPaint(ellipse.Stroke(), opacity);
            output << "<ellipse cx=\"" << SnapshotSvgNumber(origin.X + width / 2.0)
                << "\" cy=\"" << SnapshotSvgNumber(origin.Y + height / 2.0)
                << "\" rx=\"" << SnapshotSvgNumber(width / 2.0)
                << "\" ry=\"" << SnapshotSvgNumber(height / 2.0) << "\"";
            AppendSnapshotPaint(output, "fill", fill);
            AppendSnapshotPaint(output, "stroke", stroke);
            if (stroke.color != "none")
                output << " stroke-width=\"" << SnapshotSvgNumber(
                    ellipse.StrokeThickness()) << "\"";
            output << "/>";
        }
        else if (auto sourceLine = node.try_as<
                     Microsoft::UI::Xaml::Shapes::Line>())
        {
            const auto start = SnapshotPointInRoot(sourceLine, root,
                sourceLine.X1(), sourceLine.Y1());
            const auto end = SnapshotPointInRoot(sourceLine, root,
                sourceLine.X2(), sourceLine.Y2());
            const auto stroke = SnapshotBrushPaint(sourceLine.Stroke(), opacity);
            output << "<line x1=\"" << SnapshotSvgNumber(start.X)
                << "\" y1=\"" << SnapshotSvgNumber(start.Y)
                << "\" x2=\"" << SnapshotSvgNumber(end.X)
                << "\" y2=\"" << SnapshotSvgNumber(end.Y) << "\"";
            AppendSnapshotPaint(output, "stroke", stroke);
            output << " stroke-width=\"" << SnapshotSvgNumber(
                sourceLine.StrokeThickness()) << "\"/>";
        }
        else if (auto text = node.try_as<TextBlock>())
        {
            const std::string value = to_string(text.Text());
            if (!value.empty())
            {
                const double fontSize = std::max(1.0, text.FontSize());
                double x = origin.X;
                std::string anchor = "start";
                if (text.TextAlignment() == TextAlignment::Center)
                {
                    x += width / 2.0; anchor = "middle";
                }
                else if (text.TextAlignment() == TextAlignment::Right)
                {
                    x += width; anchor = "end";
                }
                std::string family = "Segoe UI";
                try
                {
                    if (auto font = text.FontFamily(); font && !font.Source().empty())
                        family = to_string(font.Source());
                }
                catch (...) {}
                const auto familySeparator = family.rfind('#');
                if (familySeparator != std::string::npos &&
                    familySeparator + 1 < family.size())
                    family = family.substr(familySeparator + 1);
                family = "'" + family + "', 'Segoe UI', sans-serif";
                const auto foreground = SnapshotBrushPaint(text.Foreground(), opacity);
                std::vector<std::string> lines;
                std::size_t start = 0;
                while (start <= value.size())
                {
                    const auto end = value.find('\n', start);
                    lines.push_back(value.substr(start,
                        end == std::string::npos ? std::string::npos : end - start));
                    if (end == std::string::npos) break;
                    start = end + 1;
                }
                const double lineHeight = fontSize * 1.22;
                double baseline = origin.Y + text.BaselineOffset();
                if (!std::isfinite(baseline) || baseline <= origin.Y)
                    baseline = origin.Y + fontSize * 0.94;
                output << "<text x=\"" << SnapshotSvgNumber(x)
                    << "\" y=\"" << SnapshotSvgNumber(baseline)
                    << "\" text-anchor=\"" << anchor
                    << "\" font-family=\"" << SnapshotSvgEscape(family)
                    << "\" font-size=\"" << SnapshotSvgNumber(fontSize) << "\"";
                if (text.FontWeight().Weight >= 600)
                    output << " font-weight=\"600\"";
                if (text.FontStyle() != Windows::UI::Text::FontStyle::Normal)
                    output << " font-style=\"italic\"";
                AppendSnapshotPaint(output, "fill", foreground);
                output << '>';
                for (std::size_t index = 0; index < lines.size(); ++index)
                {
                    if (index == 0) output << SnapshotSvgEscape(lines[index]);
                    else output << "<tspan x=\"" << SnapshotSvgNumber(x)
                        << "\" dy=\"" << SnapshotSvgNumber(lineHeight)
                        << "\">" << SnapshotSvgEscape(lines[index]) << "</tspan>";
                }
                output << "</text>";
            }
        }

        const int count = Media::VisualTreeHelper::GetChildrenCount(node);
        for (int index = 0; index < count; ++index)
            AppendSnapshotVectorNode(Media::VisualTreeHelper::GetChild(node, index),
                root, output, opacity);
    }

    winrt::Microsoft::UI::Xaml::Controls::MenuFlyout NearestLocalContextMenu(
        winrt::Microsoft::UI::Xaml::DependencyObject const& source,
        winrt::Microsoft::UI::Xaml::FrameworkElement const& root)
    {
        using namespace winrt::Microsoft::UI::Xaml;
        using namespace winrt::Microsoft::UI::Xaml::Controls;
        DependencyObject current = source;
        while (current)
        {
            if (auto element = current.try_as<FrameworkElement>())
            {
                if (auto menu = element.ContextFlyout().try_as<MenuFlyout>()) return menu;
            }
            if (winrt::get_abi(current) == winrt::get_abi(root)) break;
            current = Media::VisualTreeHelper::GetParent(current);
        }
        return nullptr;
    }

    void HookContextTree(
        winrt::Microsoft::UI::Xaml::DependencyObject const& node,
        std::shared_ptr<ContextMenuBinding> const& binding)
    {
        using namespace winrt;
        using namespace Microsoft::UI::Xaml;
        using namespace Microsoft::UI::Xaml::Controls;
        using namespace Microsoft::UI::Xaml::Input;
        if (!node) return;
        if (auto element = node.try_as<UIElement>())
        {
            bool hooked = false;
            for (auto iterator = binding->hooked.begin();
                 iterator != binding->hooked.end();)
            {
                if (auto current = iterator->get())
                {
                    if (SameElement(current, element)) hooked = true;
                    ++iterator;
                }
                else iterator = binding->hooked.erase(iterator);
            }
            if (!hooked)
            {
                binding->hooked.push_back(make_weak(element));
                std::weak_ptr<ContextMenuBinding> weakBinding = binding;
                element.ContextRequested(
                    [weakBinding](UIElement const& sender,
                                  ContextRequestedEventArgs const& args)
                    {
                        if (args.Handled()) return;
                        auto current = weakBinding.lock();
                        if (!current || !current->menu) return;
                        auto root = current->root.get();
                        if (!root) return;
                        const auto xamlRoot = root.XamlRoot();
                        if (xamlRoot && !SameElement(
                                xamlRoot.Content().try_as<UIElement>(), root))
                            return;
                        // A control with ContextFlyout has its own automatic
                        // opener. Showing it here as well races that opener
                        // and can consume the first click on its items.
                        auto source = args.OriginalSource().try_as<DependencyObject>();
                        if (source && NearestLocalContextMenu(source, root)) return;
                        auto menu = current->menu;
                        if (!menu) return;
                        menu.AreOpenCloseAnimationsEnabled(false);
                        Controls::Primitives::FlyoutShowOptions options;
                        auto target = sender.try_as<FrameworkElement>();
                        if (!target) target = root;
                        Windows::Foundation::Point point{};
                        if (args.TryGetPosition(target, point))
                        {
                            options.Position(point);
                            options.ShowMode(
                                Controls::Primitives::FlyoutShowMode::Standard);
                            menu.ShowAt(target, options);
                        }
                        else menu.ShowAt(target);
                        args.Handled(true);
                    });
            }
        }

        // ContextRequested bubbles to the root. Hooking descendants opens the
        // window menu before a cell's own handler can select its local menu.
    }

    winrt::Microsoft::UI::Xaml::Controls::MenuFlyout MenuForRoot(
        winrt::Microsoft::UI::Xaml::FrameworkElement const& root)
    {
        for (auto const& binding : g_contextMenus)
            if (auto current = binding->root.get(); SameElement(current, root))
                return binding->menu;
        return nullptr;
    }

    winrt::Microsoft::UI::Xaml::Controls::Canvas TableAnnotationCanvas(
        winrt::Microsoft::UI::Xaml::FrameworkElement const& root,
        bool create)
    {
        using namespace winrt::Microsoft::UI::Xaml;
        using namespace winrt::Microsoft::UI::Xaml::Controls;
        auto overlay = root.try_as<Grid>();
        if (!overlay) return nullptr;
        for (auto const& child : overlay.Children())
            if (auto canvas = child.try_as<Canvas>(); canvas &&
                canvas.Name() == L"LinkEDA.TableAnnotations") return canvas;
        if (!create) return nullptr;
        auto canvas = Canvas();
        canvas.Name(L"LinkEDA.TableAnnotations");
        canvas.Background(nullptr);
        canvas.HorizontalAlignment(HorizontalAlignment::Stretch);
        canvas.VerticalAlignment(VerticalAlignment::Stretch);
        Canvas::SetZIndex(canvas, 9000);
        overlay.Children().Append(canvas);
        return canvas;
    }

    winrt::Microsoft::UI::Xaml::Media::SolidColorBrush StickerBrush(
        ::rlispstat::core::StickyNoteColor color)
    {
        using winrt::Windows::UI::ColorHelper;
        using winrt::Microsoft::UI::Xaml::Media::SolidColorBrush;
        switch (color)
        {
        case ::rlispstat::core::StickyNoteColor::Pink:
            return SolidColorBrush(ColorHelper::FromArgb(255, 255, 184, 214));
        case ::rlispstat::core::StickyNoteColor::Blue:
            return SolidColorBrush(ColorHelper::FromArgb(255, 184, 222, 250));
        case ::rlispstat::core::StickyNoteColor::Green:
            return SolidColorBrush(ColorHelper::FromArgb(255, 199, 240, 179));
        case ::rlispstat::core::StickyNoteColor::Orange:
            return SolidColorBrush(ColorHelper::FromArgb(255, 255, 207, 153));
        default:
            return SolidColorBrush(ColorHelper::FromArgb(255, 255, 240, 143));
        }
    }

    void UpdateTableAnnotateLabel(
        winrt::Microsoft::UI::Xaml::FrameworkElement const& root)
    {
        auto source = SnapshotSourceForRoot(root);
        auto menu = MenuForRoot(root);
        if (!source || !menu) return;
        const bool annotated = source->note.has_content ||
            std::any_of(source->stickers.begin(), source->stickers.end(),
                [](auto const& sticker) { return sticker.has_content; });
        for (auto const& base : menu.Items())
            if (auto item = base.try_as<winrt::Microsoft::UI::Xaml::Controls::MenuFlyoutSubItem>();
                item && winrt::unbox_value_or<winrt::hstring>(item.Tag(), L"") ==
                    L"LinkEDA.Annotation")
            {
                item.Text(annotated ? L"Annotate •" : L"Annotate");
                for (auto const& child : item.Items())
                    if (auto notes = child.try_as<winrt::Microsoft::UI::Xaml::Controls::ToggleMenuFlyoutItem>();
                        notes && winrt::unbox_value_or<winrt::hstring>(notes.Tag(), L"") ==
                            L"LinkEDA.Annotation.Notes")
                    {
                        notes.IsChecked(source->note.visible);
                        notes.Text(source->note.visible ? L"Hide Notes" : L"Show Notes");
                    }
            }
    }

    void NotifyStickerChange(SnapshotWindowSource const& source)
    {
        if (source.stickersChanged) source.stickersChanged(source.stickers);
    }

    void RenderTableStickers(
        winrt::Microsoft::UI::Xaml::FrameworkElement const& root)
    {
        using namespace winrt;
        using namespace Microsoft::UI::Xaml;
        using namespace Microsoft::UI::Xaml::Controls;
        auto source = SnapshotSourceForRoot(root);
        auto canvas = TableAnnotationCanvas(root, true);
        if (!source || !canvas) return;
        canvas.Children().Clear();
        if (source->activeSticker >= static_cast<int>(source->stickers.size()))
            source->activeSticker = -1;
        auto weakRoot = make_weak(root);
        for (std::size_t index = 0; index < source->stickers.size(); ++index)
        {
            auto const& note = source->stickers[index];
            const double displayedWidth = note.collapsed ? 32.0 : note.width;
            const double displayedHeight = note.collapsed ? 28.0 : note.height;
            auto leader = Microsoft::UI::Xaml::Shapes::Line();
            leader.X1(note.x + displayedWidth / 2.0);
            leader.Y1(note.y + displayedHeight / 2.0);
            leader.X2(note.anchor_x); leader.Y2(note.anchor_y);
            leader.Stroke(Media::SolidColorBrush(
                Windows::UI::ColorHelper::FromArgb(255, 56, 56, 56)));
            leader.Opacity(0.72); leader.StrokeThickness(1.5);
            canvas.Children().Append(leader);

            auto anchor = Microsoft::UI::Xaml::Shapes::Ellipse();
            anchor.Width(10); anchor.Height(10);
            anchor.Fill(Media::SolidColorBrush(
                Windows::UI::ColorHelper::FromArgb(255, 51, 51, 51)));
            Canvas::SetLeft(anchor, note.anchor_x - 5);
            Canvas::SetTop(anchor, note.anchor_y - 5);
            canvas.Children().Append(anchor);

            auto sticker = Border();
            sticker.Width(displayedWidth); sticker.Height(displayedHeight);
            sticker.Background(StickerBrush(note.color));
            sticker.BorderBrush(Media::SolidColorBrush(
                Windows::UI::ColorHelper::FromArgb(255, 85, 85, 85)));
            sticker.BorderThickness(Thickness{
                source->activeSticker == static_cast<int>(index) ? 2.0 : 1.0});
            sticker.CornerRadius(CornerRadius{5});
            Canvas::SetLeft(sticker, note.x); Canvas::SetTop(sticker, note.y);

            if (note.collapsed)
            {
                auto expand = Button(); expand.Content(box_value(L"…"));
                expand.Padding(Thickness{0});
                expand.HorizontalAlignment(HorizontalAlignment::Stretch);
                expand.VerticalAlignment(VerticalAlignment::Stretch);
                ToolTipService::SetToolTip(expand, box_value(L"Expand this note"));
                expand.Click([weakRoot, index](auto const&, auto const&)
                {
                    auto currentRoot = weakRoot.get();
                    auto current = currentRoot ? SnapshotSourceForRoot(currentRoot) : nullptr;
                    if (!current || index >= current->stickers.size()) return;
                    ::rlispstat::core::SetWindowStickyNoteCollapsed(
                        current->stickers[index], false);
                    current->activeSticker = static_cast<int>(index);
                    NotifyStickerChange(*current);
                    RenderTableStickers(currentRoot);
                });
                sticker.Child(expand); canvas.Children().Append(sticker);
                continue;
            }

            auto layout = Grid();
            auto headerRow = RowDefinition(); headerRow.Height(GridLength{24});
            auto bodyRow = RowDefinition();
            bodyRow.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Star));
            layout.RowDefinitions().Append(headerRow);
            layout.RowDefinitions().Append(bodyRow);
            auto header = Grid(); header.Background(StickerBrush(note.color));
            auto titleColumn = ColumnDefinition();
            titleColumn.Width(GridLengthHelper::FromValueAndType(1, GridUnitType::Star));
            auto collapseColumn = ColumnDefinition(); collapseColumn.Width(GridLength{28});
            auto colorColumn = ColumnDefinition(); colorColumn.Width(GridLength{28});
            auto closeColumn = ColumnDefinition(); closeColumn.Width(GridLength{28});
            header.ColumnDefinitions().Append(titleColumn);
            header.ColumnDefinitions().Append(collapseColumn);
            header.ColumnDefinitions().Append(colorColumn);
            header.ColumnDefinitions().Append(closeColumn);
            auto collapseButton = Button(); collapseButton.Content(box_value(L"−"));
            collapseButton.Padding(Thickness{2}); Grid::SetColumn(collapseButton, 1);
            ToolTipService::SetToolTip(collapseButton, box_value(L"Collapse to marker"));
            header.Children().Append(collapseButton);
            auto colorButton = Button(); colorButton.Content(box_value(L"●"));
            colorButton.Padding(Thickness{2}); Grid::SetColumn(colorButton, 2);
            ToolTipService::SetToolTip(colorButton, box_value(L"Sticker color"));
            header.Children().Append(colorButton);
            auto closeButton = Button(); closeButton.Content(box_value(L"×"));
            closeButton.Padding(Thickness{2}); Grid::SetColumn(closeButton, 3);
            ToolTipService::SetToolTip(closeButton, box_value(L"Delete Sticker"));
            header.Children().Append(closeButton); layout.Children().Append(header);

            auto editor = TextBox(); editor.AcceptsReturn(true);
            editor.TextWrapping(TextWrapping::Wrap);
            editor.Text(to_hstring(note.plain_text));
            editor.HorizontalAlignment(HorizontalAlignment::Stretch);
            editor.VerticalAlignment(VerticalAlignment::Stretch);
            editor.Padding(Thickness{8}); editor.Background(StickerBrush(note.color));
            editor.BorderThickness(Thickness{0}); editor.PlaceholderText(L"Write a note…");
            Grid::SetRow(editor, 1); layout.Children().Append(editor);
            auto grip = Primitives::Thumb(); grip.Width(16); grip.Height(16);
            grip.HorizontalAlignment(HorizontalAlignment::Right);
            grip.VerticalAlignment(VerticalAlignment::Bottom);
            Grid::SetRow(grip, 1); layout.Children().Append(grip);
            sticker.Child(layout); canvas.Children().Append(sticker);

            editor.TextChanged([weakRoot, index, editor](auto const&, auto const&)
            {
                auto currentRoot = weakRoot.get();
                auto current = currentRoot ? SnapshotSourceForRoot(currentRoot) : nullptr;
                if (!current || index >= current->stickers.size()) return;
                ::rlispstat::core::SetWindowStickyNoteText(
                    current->stickers[index], to_string(editor.Text()));
                NotifyStickerChange(*current);
                UpdateTableAnnotateLabel(currentRoot);
            });
            closeButton.Click([weakRoot, index](auto const&, auto const&)
            {
                auto currentRoot = weakRoot.get();
                auto current = currentRoot ? SnapshotSourceForRoot(currentRoot) : nullptr;
                if (!current || index >= current->stickers.size()) return;
                current->stickers.erase(current->stickers.begin() +
                    static_cast<std::ptrdiff_t>(index));
                current->activeSticker = -1;
                NotifyStickerChange(*current);
                RenderTableStickers(currentRoot);
                UpdateTableAnnotateLabel(currentRoot);
            });
            collapseButton.Click([weakRoot, index](auto const&, auto const&)
            {
                auto currentRoot = weakRoot.get();
                auto current = currentRoot ? SnapshotSourceForRoot(currentRoot) : nullptr;
                if (!current || index >= current->stickers.size()) return;
                ::rlispstat::core::SetWindowStickyNoteCollapsed(
                    current->stickers[index], true);
                current->activeSticker = -1;
                NotifyStickerChange(*current);
                RenderTableStickers(currentRoot);
            });
            auto colorMenu = MenuFlyout(); colorMenu.AreOpenCloseAnimationsEnabled(false);
            const std::array<std::pair<::rlispstat::core::StickyNoteColor,
                wchar_t const*>, 5> colors{{
                {::rlispstat::core::StickyNoteColor::Yellow, L"Yellow"},
                {::rlispstat::core::StickyNoteColor::Pink, L"Pink"},
                {::rlispstat::core::StickyNoteColor::Blue, L"Blue"},
                {::rlispstat::core::StickyNoteColor::Green, L"Green"},
                {::rlispstat::core::StickyNoteColor::Orange, L"Orange"}
            }};
            for (auto const& [color, name] : colors)
            {
                auto item = ToggleMenuFlyoutItem(); item.Text(name);
                item.IsChecked(note.color == color);
                item.Click([weakRoot, index, color](auto const&, auto const&)
                {
                    auto currentRoot = weakRoot.get();
                    auto current = currentRoot ? SnapshotSourceForRoot(currentRoot) : nullptr;
                    if (!current || index >= current->stickers.size()) return;
                    ::rlispstat::core::SetWindowStickyNoteColor(
                        current->stickers[index], color);
                    current->activeSticker = static_cast<int>(index);
                    NotifyStickerChange(*current);
                    RenderTableStickers(currentRoot);
                });
                colorMenu.Items().Append(item);
            }
            colorButton.Flyout(colorMenu);
            grip.DragDelta([weakRoot, index, sticker, leader](auto const&,
                Primitives::DragDeltaEventArgs const& event)
            {
                auto currentRoot = weakRoot.get();
                auto current = currentRoot ? SnapshotSourceForRoot(currentRoot) : nullptr;
                if (!current || index >= current->stickers.size()) return;
                auto& value = current->stickers[index];
                ::rlispstat::core::ResizeWindowStickyNote(value,
                    value.width + event.HorizontalChange(),
                    value.height + event.VerticalChange());
                sticker.Width(value.width); sticker.Height(value.height);
                leader.X1(value.x + value.width / 2.0);
                leader.Y1(value.y + value.height / 2.0);
                NotifyStickerChange(*current);
            });
            header.PointerPressed([weakRoot, index, header](auto const&, auto const& event)
            {
                auto currentRoot = weakRoot.get();
                auto current = currentRoot ? SnapshotSourceForRoot(currentRoot) : nullptr;
                auto currentCanvas = currentRoot ? TableAnnotationCanvas(currentRoot, false) : nullptr;
                if (!current || !currentCanvas || index >= current->stickers.size()) return;
                current->activeSticker = static_cast<int>(index);
                current->stickerDrag = static_cast<int>(index);
                const auto point = event.GetCurrentPoint(currentCanvas).Position();
                current->dragStartX = point.X; current->dragStartY = point.Y;
                current->originalX = current->stickers[index].x;
                current->originalY = current->stickers[index].y;
                header.CapturePointer(event.Pointer()); event.Handled(true);
            });
            header.PointerMoved([weakRoot, sticker, leader](auto const&, auto const& event)
            {
                auto currentRoot = weakRoot.get();
                auto current = currentRoot ? SnapshotSourceForRoot(currentRoot) : nullptr;
                auto currentCanvas = currentRoot ? TableAnnotationCanvas(currentRoot, false) : nullptr;
                if (!current || !currentCanvas || current->stickerDrag < 0 ||
                    static_cast<std::size_t>(current->stickerDrag) >= current->stickers.size())
                    return;
                const auto point = event.GetCurrentPoint(currentCanvas).Position();
                auto& value = current->stickers[current->stickerDrag];
                ::rlispstat::core::MoveWindowStickyNote(value,
                    current->originalX + point.X - current->dragStartX,
                    current->originalY + point.Y - current->dragStartY);
                Canvas::SetLeft(sticker, value.x); Canvas::SetTop(sticker, value.y);
                leader.X1(value.x + value.width / 2.0);
                leader.Y1(value.y + value.height / 2.0);
                NotifyStickerChange(*current); event.Handled(true);
            });
            header.PointerReleased([weakRoot](auto const&, auto const& event)
            {
                auto currentRoot = weakRoot.get();
                auto current = currentRoot ? SnapshotSourceForRoot(currentRoot) : nullptr;
                if (!current || current->stickerDrag < 0) return;
                current->stickerDrag = -1; event.Handled(true);
            });
            anchor.PointerPressed([weakRoot, index, anchor](auto const&, auto const& event)
            {
                auto currentRoot = weakRoot.get();
                auto current = currentRoot ? SnapshotSourceForRoot(currentRoot) : nullptr;
                if (!current || index >= current->stickers.size()) return;
                current->activeSticker = static_cast<int>(index);
                current->anchorDrag = static_cast<int>(index);
                anchor.CapturePointer(event.Pointer()); event.Handled(true);
            });
            anchor.PointerMoved([weakRoot, anchor, leader](auto const&, auto const& event)
            {
                auto currentRoot = weakRoot.get();
                auto current = currentRoot ? SnapshotSourceForRoot(currentRoot) : nullptr;
                auto currentCanvas = currentRoot ? TableAnnotationCanvas(currentRoot, false) : nullptr;
                if (!current || !currentCanvas || current->anchorDrag < 0 ||
                    static_cast<std::size_t>(current->anchorDrag) >= current->stickers.size())
                    return;
                const auto point = event.GetCurrentPoint(currentCanvas).Position();
                ::rlispstat::core::MoveWindowStickyNoteAnchor(
                    current->stickers[current->anchorDrag], point.X, point.Y);
                Canvas::SetLeft(anchor, point.X - 5); Canvas::SetTop(anchor, point.Y - 5);
                leader.X2(point.X); leader.Y2(point.Y);
                NotifyStickerChange(*current); event.Handled(true);
            });
            anchor.PointerReleased([weakRoot](auto const&, auto const& event)
            {
                auto currentRoot = weakRoot.get();
                auto current = currentRoot ? SnapshotSourceForRoot(currentRoot) : nullptr;
                if (!current || current->anchorDrag < 0) return;
                current->anchorDrag = -1; event.Handled(true);
            });
            editor.GotFocus([weakRoot, index](auto const&, auto const&)
            {
                auto currentRoot = weakRoot.get();
                auto current = currentRoot ? SnapshotSourceForRoot(currentRoot) : nullptr;
                if (current) current->activeSticker = static_cast<int>(index);
            });
        }
    }

    void AddTableSticker(
        winrt::Microsoft::UI::Xaml::FrameworkElement const& root)
    {
        auto source = SnapshotSourceForRoot(root);
        if (!source) return;
        auto note = ::rlispstat::core::MakeWindowStickyNote(
            ::rlispstat::core::GenerateWindowStickyNoteId());
        const double offset = 18.0 * static_cast<double>(source->stickers.size() % 8);
        ::rlispstat::core::MoveWindowStickyNote(note, 24.0 + offset, 24.0 + offset);
        source->stickers.push_back(std::move(note));
        source->activeSticker = static_cast<int>(source->stickers.size() - 1);
        NotifyStickerChange(*source);
        RenderTableStickers(root);
        UpdateTableAnnotateLabel(root);
    }

    constexpr double kTableNotesPanelWidth = 286.0;

    void ResizeForTableNotes(SnapshotWindowSource const& source, double delta)
    {
        auto window = source.window.get();
        if (!window || std::abs(delta) < 0.5) return;
        try
        {
            HWND handle{};
            winrt::check_hresult(window.as<::IWindowNative>()->get_WindowHandle(&handle));
            RECT client{};
            if (!GetClientRect(handle, &client)) return;
            const UINT dpi = std::max<UINT>(96, GetDpiForWindow(handle));
            const double scale = static_cast<double>(dpi) / 96.0;
            const double width = static_cast<double>(client.right - client.left) / scale;
            const double height = static_cast<double>(client.bottom - client.top) / scale;
            winrt::LinkEDA::implementation::ResizeLinkEDAWindowClient(
                window, std::max(520.0, width + delta), height);
        }
        catch (...) {}
    }

    bool EnsureTableNotesPanel(
        winrt::Microsoft::UI::Xaml::FrameworkElement const& root)
    {
        using namespace winrt;
        using namespace Microsoft::UI::Xaml;
        using namespace Microsoft::UI::Xaml::Controls;
        auto source = SnapshotSourceForRoot(root);
        auto grid = root.try_as<Grid>();
        if (!source || !grid) return false;
        if (source->notesPanel) return true;

        if (grid.ColumnDefinitions().Size() == 0)
        {
            auto main = ColumnDefinition();
            main.Width(GridLengthHelper::FromValueAndType(1, GridUnitType::Star));
            grid.ColumnDefinitions().Append(main);
        }
        source->notesColumn = ColumnDefinition();
        source->notesColumn.Width(GridLength{0});
        grid.ColumnDefinitions().Append(source->notesColumn);

        auto panel = Border();
        panel.Name(L"LinkEDA.TableNotesPanel");
        panel.Visibility(Visibility::Collapsed);
        panel.Margin(Thickness{14, 0, 0, 0});
        panel.Padding(Thickness{14, 12, 10, 10});
        panel.BorderThickness(Thickness{1, 0, 0, 0});
        panel.BorderBrush(Media::SolidColorBrush(
            Windows::UI::ColorHelper::FromArgb(255, 218, 221, 225)));
        panel.Background(Media::SolidColorBrush(
            Windows::UI::ColorHelper::FromArgb(255, 249, 250, 251)));
        Grid::SetColumn(panel, static_cast<int>(grid.ColumnDefinitions().Size() - 1));
        Grid::SetRow(panel, 0);
        Grid::SetRowSpan(panel, std::max(1, static_cast<int>(grid.RowDefinitions().Size())));

        auto layout = Grid();
        auto titleRow = RowDefinition();
        titleRow.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Auto));
        auto editorRow = RowDefinition();
        editorRow.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Star));
        auto exportRow = RowDefinition();
        exportRow.Height(GridLengthHelper::FromValueAndType(1, GridUnitType::Auto));
        layout.RowDefinitions().Append(titleRow);
        layout.RowDefinitions().Append(editorRow);
        layout.RowDefinitions().Append(exportRow);

        auto title = TextBlock();
        title.Text(L"Notes"); title.FontSize(15);
        title.FontWeight(Windows::UI::Text::FontWeights::SemiBold());
        title.Margin(Thickness{0, 0, 0, 9});
        layout.Children().Append(title);

        auto editor = TextBox();
        editor.AcceptsReturn(true); editor.TextWrapping(TextWrapping::Wrap);
        editor.VerticalContentAlignment(VerticalAlignment::Top);
        editor.HorizontalAlignment(HorizontalAlignment::Stretch);
        editor.VerticalAlignment(VerticalAlignment::Stretch);
        editor.PlaceholderText(L"Notes for this table");
        editor.Text(to_hstring(source->note.plain_text));
        Grid::SetRow(editor, 1); layout.Children().Append(editor);

        auto include = CheckBox();
        include.Content(box_value(L"Include notes in export"));
        include.IsChecked(source->note.include_in_export);
        include.Margin(Thickness{0, 9, 0, 0});
        Grid::SetRow(include, 2); layout.Children().Append(include);
        panel.Child(layout); grid.Children().Append(panel);

        source->notesPanel = panel; source->notesEditor = editor;
        source->notesInclude = include;
        const auto weakRoot = make_weak(root);
        editor.TextChanged([weakRoot, editor](auto const&, auto const&)
        {
            auto currentRoot = weakRoot.get();
            auto current = currentRoot ? SnapshotSourceForRoot(currentRoot) : nullptr;
            if (!current || current->updatingNote) return;
            ::rlispstat::core::SetWindowNoteText(
                current->note, to_string(editor.Text()));
            if (current->note.has_content && !current->note.include_in_export)
            {
                ::rlispstat::core::SetWindowNoteIncludedInExport(current->note, true);
                current->updatingNote = true;
                if (current->notesInclude) current->notesInclude.IsChecked(true);
                current->updatingNote = false;
            }
            UpdateTableAnnotateLabel(currentRoot);
        });
        include.Checked([weakRoot](auto const&, auto const&)
        {
            auto currentRoot = weakRoot.get();
            auto current = currentRoot ? SnapshotSourceForRoot(currentRoot) : nullptr;
            if (current && !current->updatingNote)
                ::rlispstat::core::SetWindowNoteIncludedInExport(current->note, true);
        });
        include.Unchecked([weakRoot](auto const&, auto const&)
        {
            auto currentRoot = weakRoot.get();
            auto current = currentRoot ? SnapshotSourceForRoot(currentRoot) : nullptr;
            if (current && !current->updatingNote)
                ::rlispstat::core::SetWindowNoteIncludedInExport(current->note, false);
        });
        return true;
    }

    void SetTableNotesVisible(
        winrt::Microsoft::UI::Xaml::FrameworkElement const& root, bool visible)
    {
        using namespace winrt::Microsoft::UI::Xaml;
        auto source = SnapshotSourceForRoot(root);
        if (!source || (visible && !EnsureTableNotesPanel(root))) return;
        source = SnapshotSourceForRoot(root);
        if (!source) return;
        const bool wasVisible = source->note.visible && source->notesPanel &&
            source->notesPanel.Visibility() == Visibility::Visible;
        ::rlispstat::core::SetWindowNoteVisible(source->note, visible);
        if (source->notesPanel)
            source->notesPanel.Visibility(visible ? Visibility::Visible : Visibility::Collapsed);
        if (source->notesColumn)
            source->notesColumn.Width(GridLength{visible ? kTableNotesPanelWidth : 0.0});
        if (visible != wasVisible)
            ResizeForTableNotes(*source, visible ? kTableNotesPanelWidth : -kTableNotesPanelWidth);
        if (visible && source->notesEditor) source->notesEditor.Focus(FocusState::Programmatic);
        UpdateTableAnnotateLabel(root);
    }

    void AppendTableAnnotationContextItems(
        winrt::Microsoft::UI::Xaml::Window const& window,
        winrt::Microsoft::UI::Xaml::Controls::MenuFlyout const& menu)
    {
        using namespace winrt;
        using namespace Microsoft::UI::Xaml;
        using namespace Microsoft::UI::Xaml::Controls;
        auto root = window.Content().try_as<FrameworkElement>();
        auto source = root ? SnapshotSourceForRoot(root) : nullptr;
        if (!root || !source || source->kind != "output") return;
        for (auto const& base : menu.Items())
            if (auto item = base.try_as<MenuFlyoutSubItem>(); item &&
                (unbox_value_or<hstring>(item.Tag(), L"") == L"LinkEDA.Annotation" ||
                 to_string(item.Text()).rfind("Annotate", 0) == 0)) return;

        uint32_t exportIndex = menu.Items().Size();
        for (uint32_t index = 0; index < menu.Items().Size(); ++index)
        {
            auto item = menu.Items().GetAt(index).try_as<MenuFlyoutSubItem>();
            if (item && item.Text() == L"Export") exportIndex = index;
        }
        auto annotate = MenuFlyoutSubItem();
        annotate.Text(L"Annotate");
        annotate.Tag(box_value(L"LinkEDA.Annotation"));
        auto notes = ToggleMenuFlyoutItem();
        notes.Text(L"Show Notes");
        notes.Tag(box_value(L"LinkEDA.Annotation.Notes"));
        notes.IsChecked(source->note.visible);
        notes.Click([weakRoot = make_weak(root)](auto const& sender, auto const&)
        {
            auto currentRoot = weakRoot.get();
            auto item = sender.try_as<ToggleMenuFlyoutItem>();
            if (currentRoot && item) SetTableNotesVisible(currentRoot, item.IsChecked());
        });
        annotate.Items().Append(notes);
        auto sticker = MenuFlyoutItem();
        sticker.Text(L"New Sticker");
        sticker.Click([weakRoot = make_weak(root)](auto const&, auto const&)
        {
            if (auto currentRoot = weakRoot.get()) AddTableSticker(currentRoot);
        });
        annotate.Items().Append(sticker);
        annotate.Items().Append(MenuFlyoutSeparator());
        auto clear = MenuFlyoutItem();
        clear.Text(L"Clear Notes");
        clear.Click([weakRoot = make_weak(root)](auto const&, auto const&)
        {
            auto currentRoot = weakRoot.get();
            auto current = currentRoot ? SnapshotSourceForRoot(currentRoot) : nullptr;
            if (current)
            {
                ::rlispstat::core::ClearWindowNote(current->note);
                current->updatingNote = true;
                if (current->notesEditor) current->notesEditor.Text(L"");
                if (current->notesInclude) current->notesInclude.IsChecked(false);
                current->updatingNote = false;
                UpdateTableAnnotateLabel(currentRoot);
            }
        });
        annotate.Items().Append(clear);
        winrt::LinkEDA::implementation::AppendSnapshotAlbumContextItems(
            window, annotate);
        if (exportIndex < menu.Items().Size())
        {
            // Annotation belongs immediately before the final export action.
            // This keeps Export at the bottom, consistently with the macOS
            // result menus and with the other LinkEDA contextual menus.
            menu.Items().InsertAt(exportIndex, annotate);
            menu.Items().InsertAt(exportIndex + 1, MenuFlyoutSeparator());
        }
        else
        {
            menu.Items().Append(MenuFlyoutSeparator());
            menu.Items().Append(annotate);
        }
    }

    winrt::hstring InstalledIconPath()
    {
        try
        {
            const auto root = winrt::Windows::ApplicationModel::Package::Current()
                .InstalledLocation().Path();
            return root + L"\\Assets\\LinkEDA.ico";
        }
        catch (...)
        {
            wchar_t executable[MAX_PATH]{};
            const DWORD length = GetModuleFileNameW(nullptr, executable, MAX_PATH);
            if (length == 0 || length >= MAX_PATH) return {};
            std::wstring path(executable, length);
            const auto separator = path.find_last_of(L"\\/");
            if (separator != std::wstring::npos) path.resize(separator);
            path += L"\\Assets\\LinkEDA.ico";
            return winrt::hstring(path);
        }
    }

}

namespace winrt::LinkEDA::implementation
{
    void AppendTableAnnotationContextItems(
        Microsoft::UI::Xaml::Window const& window,
        Microsoft::UI::Xaml::Controls::MenuFlyout const& menu)
    {
        ::AppendTableAnnotationContextItems(window, menu);
    }

    void ApplyLinkEDAWindowIcon(Microsoft::UI::Xaml::Window const& window)
    {
        try
        {
            const auto path = InstalledIconPath();
            if (!path.empty()) window.AppWindow().SetIcon(path);
        }
        catch (...)
        {
            // The AppX visual assets still provide the shell icon if a host
            // does not permit changing an individual AppWindow icon.
        }
    }

    Microsoft::UI::Xaml::Window CreateLinkEDAWindow()
    {
        auto window = Microsoft::UI::Xaml::Window();
        ApplyLinkEDAWindowIcon(window);
        g_windows.push_back(make_weak(window));
        return window;
    }

    void RefreshLinkEDAInterfaceFont(
        Microsoft::UI::Xaml::Media::FontFamily const& family)
    {
        for (auto iterator = g_windows.begin(); iterator != g_windows.end();)
        {
            if (auto window = iterator->get())
            {
                auto root = window.Content().try_as<Microsoft::UI::Xaml::FrameworkElement>();
                if (root)
                {
                    ApplyFontToVisualTree(root, family);
                    root.InvalidateMeasure();
                    root.InvalidateArrange();
                }
                ++iterator;
            }
            else iterator = g_windows.erase(iterator);
        }
    }

    void ResizeLinkEDAWindowClient(
        Microsoft::UI::Xaml::Window const& window,
        double width,
        double height)
    {
        HWND handle{};
        check_hresult(window.as<::IWindowNative>()->get_WindowHandle(&handle));
        const UINT dpi = std::max<UINT>(96, GetDpiForWindow(handle));
        const double scale = static_cast<double>(dpi) / 96.0;
        RECT bounds{
            0,
            0,
            static_cast<LONG>(std::lround(width * scale)),
            static_cast<LONG>(std::lround(height * scale))
        };
        const auto style = static_cast<DWORD>(GetWindowLongPtrW(handle, GWL_STYLE));
        const auto extendedStyle = static_cast<DWORD>(
            GetWindowLongPtrW(handle, GWL_EXSTYLE));
        if (!AdjustWindowRectExForDpi(
                &bounds, style, GetMenu(handle) != nullptr, extendedStyle, dpi))
            check_win32(GetLastError());
        auto outerWidth = static_cast<int32_t>(bounds.right - bounds.left);
        auto outerHeight = static_cast<int32_t>(bounds.bottom - bounds.top);
        MONITORINFO monitorInfo{ sizeof(MONITORINFO) };
        if (GetMonitorInfoW(MonitorFromWindow(handle, MONITOR_DEFAULTTONEAREST),
                            &monitorInfo))
        {
            const auto workWidth = static_cast<int32_t>(
                monitorInfo.rcWork.right - monitorInfo.rcWork.left);
            const auto workHeight = static_cast<int32_t>(
                monitorInfo.rcWork.bottom - monitorInfo.rcWork.top);
            outerWidth = std::min(outerWidth, std::max<int32_t>(520, workWidth - 16));
            outerHeight = std::min(outerHeight, std::max<int32_t>(360, workHeight - 24));
        }
        window.AppWindow().Resize({
            outerWidth, outerHeight
        });
    }

    void AlignLinkEDAWindowToWorkAreaLeft(
        Microsoft::UI::Xaml::Window const& window)
    {
        HWND handle{};
        check_hresult(window.as<::IWindowNative>()->get_WindowHandle(&handle));

        RECT current{};
        if (!GetWindowRect(handle, &current)) return;

        MONITORINFO monitorInfo{ sizeof(MONITORINFO) };
        if (!GetMonitorInfoW(MonitorFromWindow(handle, MONITOR_DEFAULTTONEAREST),
                             &monitorInfo)) return;

        const int32_t height = current.bottom - current.top;
        const int32_t latestTop = std::clamp<int32_t>(
            current.top,
            monitorInfo.rcWork.top,
            std::max(monitorInfo.rcWork.top,
                     monitorInfo.rcWork.bottom - height));
        SetWindowPos(handle, nullptr,
                     monitorInfo.rcWork.left, latestTop,
                     0, 0,
                     SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE);
    }

    void AttachWindowContextFlyout(
        Microsoft::UI::Xaml::Window const& window,
        Microsoft::UI::Xaml::Controls::MenuFlyout const& menu,
        bool appendSnapshotAlbum)
    {
        using namespace Microsoft::UI::Xaml;
        using namespace Microsoft::UI::Xaml::Controls;
        using namespace Microsoft::UI::Xaml::Input;
        menu.AreOpenCloseAnimationsEnabled(false);
        menu.ShowMode(Controls::Primitives::FlyoutShowMode::Standard);
        if (appendSnapshotAlbum)
        {
            auto root = window.Content().try_as<FrameworkElement>();
            auto source = root ? SnapshotSourceForRoot(root) : nullptr;
            if (source && source->kind == "output")
                AppendTableAnnotationContextItems(window, menu);
            else
                AppendSnapshotAlbumContextItems(window, menu);
        }
        auto root = window.Content().try_as<FrameworkElement>();
        if (!root) return;
        std::shared_ptr<ContextMenuBinding> binding;
        for (auto iterator = g_contextMenus.begin(); iterator != g_contextMenus.end();)
        {
            if (auto current = (*iterator)->root.get())
            {
                if (SameElement(current, root)) binding = *iterator;
                ++iterator;
            }
            else iterator = g_contextMenus.erase(iterator);
        }
        if (!binding)
        {
            binding = std::make_shared<ContextMenuBinding>();
            binding->root = make_weak(root);
            g_contextMenus.push_back(binding);
        }
        binding->menu = menu;
        // Handle ContextRequested on the content root so local cell handlers
        // receive the event first. This also covers touch and Shift+F10.
        HookContextTree(root, binding);
    }

    void AppendSnapshotAlbumContextItems(
        Microsoft::UI::Xaml::Window const& window,
        Microsoft::UI::Xaml::Controls::MenuFlyout const& menu)
    {
        using namespace Microsoft::UI::Xaml::Controls;
        auto root = window.Content().try_as<Microsoft::UI::Xaml::FrameworkElement>();
        if (!root || !SnapshotSourceForRoot(root)) return;
        for (auto const& base : menu.Items())
            if (auto item = base.try_as<MenuFlyoutItem>(); item &&
                unbox_value_or<hstring>(item.Tag(), L"") == L"LinkEDA.SnapshotAlbum.Add")
                return;
        menu.Items().Append(MenuFlyoutSeparator());
        auto add = MenuFlyoutItem();
        add.Text(L"Add Snapshot to Snapshot Album");
        add.Tag(box_value(L"LinkEDA.SnapshotAlbum.Add"));
        const auto source = *SnapshotSourceForRoot(root);
        add.Click([kind = source.kind, sourceId = source.sourceId, group = source.group]
            (auto const&, auto const&)
        {
            if (g_addSnapshot) g_addSnapshot(kind, sourceId, group);
        });
        menu.Items().Append(add);
        auto show = MenuFlyoutItem();
        show.Text(L"Show Snapshot Album");
        show.Tag(box_value(L"LinkEDA.SnapshotAlbum.Show"));
        show.Click([](auto const&, auto const&)
        {
            if (g_showSnapshotAlbum) g_showSnapshotAlbum();
        });
        menu.Items().Append(show);
    }

    void AppendSnapshotAlbumContextItems(
        Microsoft::UI::Xaml::Window const& window,
        Microsoft::UI::Xaml::Controls::MenuFlyoutSubItem const& menu)
    {
        using namespace Microsoft::UI::Xaml::Controls;
        auto root = window.Content().try_as<Microsoft::UI::Xaml::FrameworkElement>();
        if (!root || !SnapshotSourceForRoot(root)) return;
        for (auto const& base : menu.Items())
            if (auto item = base.try_as<MenuFlyoutItem>(); item &&
                unbox_value_or<hstring>(item.Tag(), L"") == L"LinkEDA.SnapshotAlbum.Add")
                return;
        menu.Items().Append(MenuFlyoutSeparator());
        auto add = MenuFlyoutItem();
        add.Text(L"Add to Snapshot Album");
        add.Tag(box_value(L"LinkEDA.SnapshotAlbum.Add"));
        const auto source = *SnapshotSourceForRoot(root);
        add.Click([kind = source.kind, sourceId = source.sourceId, group = source.group]
            (auto const&, auto const&)
        {
            if (g_addSnapshot) g_addSnapshot(kind, sourceId, group);
        });
        menu.Items().Append(add);
        auto show = MenuFlyoutItem();
        show.Text(L"Show Snapshot Album");
        show.Tag(box_value(L"LinkEDA.SnapshotAlbum.Show"));
        show.Click([](auto const&, auto const&)
        {
            if (g_showSnapshotAlbum) g_showSnapshotAlbum();
        });
        menu.Items().Append(show);
    }

    void SetSnapshotAlbumCallbacks(
        std::function<void(std::string const&, std::string const&, std::string const&)> add,
        std::function<void()> show)
    {
        g_addSnapshot = std::move(add);
        g_showSnapshotAlbum = std::move(show);
    }

    void SetWindowSnapshotSource(Microsoft::UI::Xaml::Window const& window,
                                 std::string const& kind,
                                 std::string const& sourceId,
                                 std::string const& group)
    {
        auto root = window.Content().try_as<Microsoft::UI::Xaml::FrameworkElement>();
        if (!root || sourceId.empty()) return;
        if (auto existing = SnapshotSourceForRoot(root))
        {
            existing->window = make_weak(window);
            if (existing->sourceId != sourceId || existing->kind != kind)
            {
                existing->note = ::rlispstat::core::MakeWindowNote(sourceId);
                existing->stickers.clear();
                existing->activeSticker = -1;
                existing->updatingNote = true;
                if (existing->notesEditor) existing->notesEditor.Text(L"");
                if (existing->notesInclude) existing->notesInclude.IsChecked(false);
                existing->updatingNote = false;
                RenderTableStickers(root);
            }
            existing->kind = kind; existing->sourceId = sourceId; existing->group = group;
            return;
        }
        SnapshotWindowSource source;
        source.root = make_weak(root); source.window = make_weak(window);
        source.kind = kind; source.sourceId = sourceId; source.group = group;
        source.note = ::rlispstat::core::MakeWindowNote(sourceId);
        g_snapshotSources.push_back(std::move(source));
    }

    std::string WindowSnapshotPlainText(std::string const& kind,
                                        std::string const& sourceId,
                                        std::string const& group)
    {
        for (auto iterator = g_snapshotSources.begin(); iterator != g_snapshotSources.end();)
        {
            if (auto root = iterator->root.get())
            {
                if (iterator->kind == kind && iterator->sourceId == sourceId &&
                    iterator->group == group)
                {
                    std::vector<std::string> lines;
                    CollectSnapshotText(root, lines);
                    std::string text;
                    for (auto const& line : lines)
                    {
                        if (!text.empty()) text.push_back('\n');
                        text += line;
                    }
                    return text;
                }
                ++iterator;
            }
            else iterator = g_snapshotSources.erase(iterator);
        }
        return {};
    }

    std::optional<::rlispstat::core::FrozenTableContent>
        WindowSnapshotStructuredTable(std::string const& kind,
                                      std::string const& sourceId,
                                      std::string const& group)
    {
        for (auto iterator = g_snapshotSources.begin();
             iterator != g_snapshotSources.end();)
        {
            if (auto root = iterator->root.get())
            {
                if (iterator->kind == kind && iterator->sourceId == sourceId &&
                    iterator->group == group)
                {
                    std::vector<std::vector<std::string>> rows;
                    std::size_t score = 0;
                    CollectSnapshotTableCandidates(root, rows, score);
                    if (rows.size() < 2) return std::nullopt;
                    ::rlispstat::core::FrozenTableContent table;
                    table.column_headers = std::move(rows.front());
                    rows.erase(rows.begin());
                    table.rows = std::move(rows);
                    table.source_row_count = table.rows.size();
                    table.total_source_row_count = table.rows.size();
                    table.tab_delimited_text =
                        ::rlispstat::core::FrozenTableToTabDelimited(table);
                    return table;
                }
                ++iterator;
            }
            else iterator = g_snapshotSources.erase(iterator);
        }
        return std::nullopt;
    }

    std::optional<::rlispstat::core::WindowNote>
        WindowSnapshotNote(std::string const& kind,
                           std::string const& sourceId,
                           std::string const& group)
    {
        for (auto iterator = g_snapshotSources.begin();
             iterator != g_snapshotSources.end();)
        {
            if (auto root = iterator->root.get())
            {
                if (iterator->kind == kind && iterator->sourceId == sourceId &&
                    iterator->group == group)
                    return iterator->note;
                ++iterator;
            }
            else iterator = g_snapshotSources.erase(iterator);
        }
        return std::nullopt;
    }

    std::vector<::rlispstat::core::WindowStickyNote>
        WindowSnapshotStickers(std::string const& kind,
                               std::string const& sourceId,
                               std::string const& group)
    {
        for (auto iterator = g_snapshotSources.begin();
             iterator != g_snapshotSources.end();)
        {
            if (auto root = iterator->root.get())
            {
                if (iterator->kind == kind && iterator->sourceId == sourceId &&
                    iterator->group == group) {
                    auto stickers = iterator->stickers;
                    const double width = std::max(1.0, root.ActualWidth());
                    const double height = std::max(1.0, root.ActualHeight());
                    for (auto &sticker : stickers)
                        ::rlispstat::core::SetWindowStickyNoteReferenceSize(
                            sticker, width, height);
                    return stickers;
                }
                ++iterator;
            }
            else iterator = g_snapshotSources.erase(iterator);
        }
        return {};
    }

    Microsoft::UI::Xaml::FrameworkElement WindowSnapshotVisualRoot(
        std::string const& kind,
        std::string const& sourceId,
        std::string const& group)
    {
        using namespace Microsoft::UI::Xaml;
        using namespace Microsoft::UI::Xaml::Controls;
        for (auto iterator = g_snapshotSources.begin();
             iterator != g_snapshotSources.end();)
        {
            if (auto root = iterator->root.get())
            {
                if (iterator->kind == kind && iterator->sourceId == sourceId &&
                    iterator->group == group)
                {
                    if (auto overlay = root.try_as<Grid>(); overlay &&
                        overlay.Name() == L"LinkEDA.WindowOverlay" &&
                        overlay.Children().Size() > 0)
                        if (auto content = overlay.Children().GetAt(0)
                                .try_as<FrameworkElement>())
                        {
                            if (kind == "output")
                                if (auto table = FindSnapshotTableVisual(content))
                                    return table;
                            return content;
                        }
                    if (kind == "output")
                        if (auto table = FindSnapshotTableVisual(root)) return table;
                    return root;
                }
                ++iterator;
            }
            else iterator = g_snapshotSources.erase(iterator);
        }
        return nullptr;
    }

    std::string WindowSnapshotVectorSvg(
        Microsoft::UI::Xaml::FrameworkElement const& root)
    {
        if (!root) return {};
        root.UpdateLayout();
        const double width = std::max(1.0, root.ActualWidth());
        const double height = std::max(1.0, root.ActualHeight());
        std::ostringstream output;
        output.imbue(std::locale::classic());
        output << "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\""
            << SnapshotSvgNumber(width) << "\" height=\""
            << SnapshotSvgNumber(height) << "\" viewBox=\"0 0 "
            << SnapshotSvgNumber(width) << ' ' << SnapshotSvgNumber(height)
            << "\" shape-rendering=\"geometricPrecision\" "
               "text-rendering=\"geometricPrecision\">";
        AppendSnapshotEmbeddedFonts(root, output);
        output << "<rect width=\"100%\" height=\"100%\" fill=\"white\"/>";
        AppendSnapshotVectorNode(root, root, output, 1.0);
        output << "</svg>";
        return output.str();
    }

    std::string WindowSnapshotTitle(std::string const& kind,
                                    std::string const& sourceId,
                                    std::string const& group)
    {
        auto visual = WindowSnapshotVisualRoot(kind, sourceId, group);
        if (!visual) return {};
        for (auto iterator = g_windows.begin(); iterator != g_windows.end();)
        {
            if (auto window = iterator->get())
            {
                auto root = window.Content().try_as<Microsoft::UI::Xaml::FrameworkElement>();
                const bool matches = ContainsVisualElement(root, visual);
                if (matches) return to_string(window.Title());
                ++iterator;
            }
            else iterator = g_windows.erase(iterator);
        }
        return {};
    }

    void AttachStickerOverlay(
        Microsoft::UI::Xaml::FrameworkElement const& root,
        std::vector<::rlispstat::core::WindowStickyNote> stickers,
        std::function<void(std::vector<::rlispstat::core::WindowStickyNote> const&)>
            changed)
    {
        if (!root) return;
        const double width = std::max(1.0, root.ActualWidth());
        const double height = std::max(1.0, root.ActualHeight());
        for (auto &sticker : stickers)
            sticker = ::rlispstat::core::WindowStickyNoteForCanvas(
                sticker, width, height);
        if (auto existing = SnapshotSourceForRoot(root))
        {
            existing->stickers = std::move(stickers);
            existing->stickersChanged = std::move(changed);
            existing->activeSticker = -1;
        }
        else
        {
            SnapshotWindowSource source;
            source.root = make_weak(root);
            source.kind = "snapshot_album";
            source.sourceId = ::rlispstat::core::GenerateWindowNoteViewId();
            source.note = ::rlispstat::core::MakeWindowNote(source.sourceId);
            source.stickers = std::move(stickers);
            source.stickersChanged = std::move(changed);
            g_snapshotSources.push_back(std::move(source));
        }
        RenderTableStickers(root);
    }

    void AddStickerToOverlay(
        Microsoft::UI::Xaml::FrameworkElement const& root)
    {
        if (root) AddTableSticker(root);
    }

    void SetDataSheetRecoveryCallback(
        std::function<void(std::string const&)> callback)
    {
        g_showDataSheet = std::move(callback);
    }

    void SetWindowDataSheetGroup(Microsoft::UI::Xaml::Window const& window,
                                 std::string const& group)
    {
        using namespace Microsoft::UI::Xaml;
        using namespace Microsoft::UI::Xaml::Controls;
        if (group.empty()) return;
        auto current = window.Content();
        if (!current) return;
        Grid overlay{ nullptr };
        Button button{ nullptr };
        if (auto existing = current.try_as<Grid>(); existing &&
            existing.Name() == L"LinkEDA.WindowOverlay")
        {
            overlay = existing;
            for (auto const& child : overlay.Children())
                if (auto candidate = child.try_as<Button>(); candidate &&
                    candidate.Name() == L"LinkEDA.ShowDataSheet") { button = candidate; break; }
        }
        else
        {
            auto currentRoot = current.try_as<FrameworkElement>();
            auto inheritedMenu = currentRoot ? MenuForRoot(currentRoot)
                : MenuFlyout{ nullptr };
            overlay = Grid(); overlay.Name(L"LinkEDA.WindowOverlay");
            overlay.Children().Append(current.try_as<UIElement>());
            button = Button(); button.Name(L"LinkEDA.ShowDataSheet");
            button.Width(32); button.Height(30); button.Padding(Thickness{ 0 });
            button.HorizontalAlignment(HorizontalAlignment::Right);
            button.VerticalAlignment(VerticalAlignment::Bottom);
            button.Margin(Thickness{ 0, 0, 12, 12 });
            button.Opacity(0.86); button.Content(SymbolIcon(Symbol::ViewAll));
            ToolTipService::SetToolTip(button, box_value(L"Show data sheet"));
            button.Click([](auto const& sender, auto const&)
            {
                if (!g_showDataSheet) return;
                const auto value = unbox_value_or<hstring>(sender.as<Button>().Tag(), L"");
                if (!value.empty()) g_showDataSheet(to_string(value));
            });
            Controls::Canvas::SetZIndex(button, 10000);
            overlay.Children().Append(button);
            window.Content(overlay);
            if (inheritedMenu) AttachWindowContextFlyout(window, inheritedMenu);
        }
        if (button) button.Tag(box_value(to_hstring(group)));
        TableAnnotationCanvas(overlay, true);
    }
}
