#pragma once

#include <chrono>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <mutex>
#include <sstream>
#include <string>

namespace rlispstat::windows::performance
{
    inline bool Enabled()
    {
        static const bool enabled = []
        {
#if defined(_DEBUG)
            return true;
#else
            wchar_t value[8]{};
            if (GetEnvironmentVariableW(L"LINKEDA_PERF_LOG", value,
                                        static_cast<DWORD>(std::size(value))) > 0)
                return true;
            wchar_t temporary[MAX_PATH]{};
            const DWORD length = GetTempPathW(MAX_PATH, temporary);
            if (length == 0 || length >= MAX_PATH) return false;
            const auto flag = std::filesystem::path(temporary) /
                L"LinkEDA-enable-performance.flag";
            return GetFileAttributesW(flag.c_str()) != INVALID_FILE_ATTRIBUTES;
#endif
        }();
        return enabled;
    }

    inline void Write(std::string const& operation, double milliseconds,
                      std::string const& detail = {})
    {
        if (!Enabled()) return;
        static std::mutex mutex;
        std::lock_guard lock(mutex);
        // Keep the diagnostic stream open. Opening and closing it for every
        // measurement made the instrumentation itself visible as UI stutter.
        static std::ofstream stream = []
        {
            wchar_t temporary[MAX_PATH]{};
            const DWORD length = GetTempPathW(MAX_PATH, temporary);
            if (length == 0 || length >= MAX_PATH) return std::ofstream{};
            const auto path = std::filesystem::path(temporary) /
                L"LinkEDA-performance.log";
            return std::ofstream(path, std::ios::app);
        }();
        if (!stream) return;
        stream << GetTickCount64() << '\t' << GetCurrentThreadId() << '\t'
               << operation << '\t' << milliseconds;
        if (!detail.empty()) stream << '\t' << detail;
        stream << '\n';
    }

    class Scope final
    {
    public:
        explicit Scope(std::string operation, std::string detail = {},
                       bool record = true)
            : operation_(std::move(operation)), detail_(std::move(detail)),
              started_(Clock::now()), enabled_(record && Enabled()) {}

        ~Scope()
        {
            if (!enabled_) return;
            const auto elapsed = std::chrono::duration<double, std::milli>(
                Clock::now() - started_).count();
            Write(operation_, elapsed, detail_);
        }

        Scope(Scope const&) = delete;
        Scope& operator=(Scope const&) = delete;

    private:
        using Clock = std::chrono::steady_clock;
        std::string operation_;
        std::string detail_;
        Clock::time_point started_;
        bool enabled_ = false;
    };
}
