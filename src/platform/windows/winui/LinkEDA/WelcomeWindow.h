#pragma once

#include "pch.h"

#include "../../../../core/welcome_model.h"

#include <functional>
#include <memory>
#include <string>
#include <utility>
#include <vector>

namespace winrt::LinkEDA::implementation
{
    class WelcomeWindow final : public std::enable_shared_from_this<WelcomeWindow>
    {
    public:
        using Action = std::function<void()>;
        using ValueAction = std::function<void(std::string const&)>;

        static std::shared_ptr<WelcomeWindow> Create();

        void SetClosedCallback(Action callback);
        void SetChooseRDataFrameCallback(Action callback);
        void SetOpenFileCallback(Action callback);
        void SetOpenDocumentCallback(Action callback);
        void SetRecentFileCallback(ValueAction callback);
        void SetExampleCallback(ValueAction callback);
        void SetDocumentationCallback(Action callback);
        void SetAboutCallback(Action callback);
        void Show(::rlispstat::core::WelcomeWindowModel const& model);
        void SetStatus(std::string const& text, bool error = false);
        void Hide();
        void Activate();
        bool Closed() const { return closed_; }
        HWND NativeHandle() const;
        Microsoft::UI::Xaml::Window NativeWindow() const { return window_; }

    private:
        WelcomeWindow();
        void Initialize();
        void AttachLifetime();

        Microsoft::UI::Xaml::Window window_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock connection_{ nullptr };
        Microsoft::UI::Xaml::Controls::TextBlock version_{ nullptr };
        Microsoft::UI::Xaml::Controls::Button chooseR_{ nullptr };
        Microsoft::UI::Xaml::Controls::Button openFile_{ nullptr };
        Microsoft::UI::Xaml::Controls::Button openDocument_{ nullptr };
        Microsoft::UI::Xaml::Controls::Button examplesButton_{ nullptr };
        Microsoft::UI::Xaml::Controls::StackPanel recentPanel_{ nullptr };
        Action closedCallback_;
        Action chooseRCallback_;
        Action openFileCallback_;
        Action openDocumentCallback_;
        ValueAction recentFileCallback_;
        ValueAction exampleCallback_;
        Action documentationCallback_;
        Action aboutCallback_;
        bool closed_ = false;
    };

    class RDataFrameChooserWindow final
        : public std::enable_shared_from_this<RDataFrameChooserWindow>
    {
    public:
        using Completed = std::function<void(std::string const& requestId,
                                             std::string const& objectName,
                                             bool cancelled)>;
        static std::shared_ptr<RDataFrameChooserWindow> Create(
            Microsoft::UI::Xaml::Window const& owner,
            std::string requestId,
            std::vector<std::pair<std::string, std::string>> items,
            std::string windowTitle = "Open data from R",
            std::string heading = "Choose a data frame from R",
            std::string instruction = "Choose a data frame from the connected R session",
            bool showItemId = true);
        void SetCompletedCallback(Completed callback);
        void Show();

    private:
        RDataFrameChooserWindow(Microsoft::UI::Xaml::Window const& owner,
                                std::string requestId,
                                std::vector<std::pair<std::string, std::string>> items,
                                std::string windowTitle,
                                std::string heading,
                                std::string instruction,
                                bool showItemId);
        void Initialize();
        void AttachLifetime();
        void Complete(bool cancelled);

        Microsoft::UI::Xaml::Window owner_{ nullptr };
        Microsoft::UI::Xaml::Window window_{ nullptr };
        Microsoft::UI::Xaml::Controls::ListView list_{ nullptr };
        std::string requestId_;
        std::vector<std::pair<std::string, std::string>> items_;
        std::string windowTitle_;
        std::string heading_;
        std::string instruction_;
        bool showItemId_ = true;
        Completed completedCallback_;
        bool completed_ = false;
    };
}
