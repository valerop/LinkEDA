#pragma once

#include "pch.h"
#include "../../../../core/snapshot_album_model.h"

#include <functional>
#include <optional>
#include <string>

namespace winrt::rlispstatWinUI::implementation
{
    void ApplyLinkEDAWindowIcon(Microsoft::UI::Xaml::Window const& window);
    Microsoft::UI::Xaml::Window CreateLinkEDAWindow();
    void RefreshLinkEDAInterfaceFont(
        Microsoft::UI::Xaml::Media::FontFamily const& family);
    void ResizeLinkEDAWindowClient(
        Microsoft::UI::Xaml::Window const& window,
        double width,
        double height);
    void AlignLinkEDAWindowToWorkAreaLeft(
        Microsoft::UI::Xaml::Window const& window);
    void AttachWindowContextFlyout(
        Microsoft::UI::Xaml::Window const& window,
        Microsoft::UI::Xaml::Controls::MenuFlyout const& menu,
        bool appendSnapshotAlbum = true);
    void AppendSnapshotAlbumContextItems(
        Microsoft::UI::Xaml::Window const& window,
        Microsoft::UI::Xaml::Controls::MenuFlyout const& menu);
    void AppendSnapshotAlbumContextItems(
        Microsoft::UI::Xaml::Window const& window,
        Microsoft::UI::Xaml::Controls::MenuFlyoutSubItem const& menu);
    void AppendTableAnnotationContextItems(
        Microsoft::UI::Xaml::Window const& window,
        Microsoft::UI::Xaml::Controls::MenuFlyout const& menu);
    void SetSnapshotAlbumCallbacks(
        std::function<void(std::string const&, std::string const&, std::string const&)> add,
        std::function<void()> show);
    void SetWindowSnapshotSource(Microsoft::UI::Xaml::Window const& window,
                                 std::string const& kind,
                                 std::string const& sourceId,
                                 std::string const& group);
    std::string WindowSnapshotPlainText(std::string const& kind,
                                        std::string const& sourceId,
                                        std::string const& group);
    std::optional<::rlispstat::core::FrozenTableContent>
        WindowSnapshotStructuredTable(std::string const& kind,
                                      std::string const& sourceId,
                                      std::string const& group);
    std::optional<::rlispstat::core::WindowNote>
        WindowSnapshotNote(std::string const& kind,
                           std::string const& sourceId,
                           std::string const& group);
    std::vector<::rlispstat::core::WindowStickyNote>
        WindowSnapshotStickers(std::string const& kind,
                               std::string const& sourceId,
                               std::string const& group);
    Microsoft::UI::Xaml::FrameworkElement WindowSnapshotVisualRoot(
        std::string const& kind,
        std::string const& sourceId,
        std::string const& group);
    std::string WindowSnapshotVectorSvg(
        Microsoft::UI::Xaml::FrameworkElement const& root);
    std::string WindowSnapshotTitle(std::string const& kind,
                                    std::string const& sourceId,
                                    std::string const& group);
    void AttachStickerOverlay(
        Microsoft::UI::Xaml::FrameworkElement const& root,
        std::vector<::rlispstat::core::WindowStickyNote> stickers,
        std::function<void(std::vector<::rlispstat::core::WindowStickyNote> const&)>
            changed);
    void AddStickerToOverlay(
        Microsoft::UI::Xaml::FrameworkElement const& root);
    void SetDataSheetRecoveryCallback(
        std::function<void(std::string const&)> callback);
    void SetWindowDataSheetGroup(Microsoft::UI::Xaml::Window const& window,
                                 std::string const& group);
}
