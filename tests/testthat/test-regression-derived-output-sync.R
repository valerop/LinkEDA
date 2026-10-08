test_that("regression-derived outputs retain an immutable source identity", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- linkeda_source_test_root()
  geometry <- paste(readLines(file.path(root, "src", "core", "plot_geometry.h"),
                              warn = FALSE), collapse = "\n")
  mac <- paste(readLines(file.path(root, "src", "platform", "macos",
                                   "linkeda_macos_app.mm"),
                         warn = FALSE), collapse = "\n")
  windows <- paste(readLines(file.path(
    root, "src", "platform", "windows", "winui", "LinkEDA",
    "App.xaml.cpp"
  ), warn = FALSE), collapse = "\n")

  for (field in c("isRegressionDerivedPlot", "regressionDerivedSourceKind",
                  "regressionDerivedSourceModelId",
                  "regressionDerivedSourceRevision",
                  "regressionDerivedSourceFitVersion")) {
    expect_match(geometry, field, fixed = TRUE)
  }

  # macOS pairwise tables and plots use the exact source revision and reject a
  # reply when either the model revision or accepted fit version has changed.
  expect_match(mac, "PairwiseDerivedSnapshotIsCurrent", fixed = TRUE)
  expect_match(mac, "RefreshPairwiseDerivedOutputsForModelNow", fixed = TRUE)
  expect_match(mac, "sourceModelRevision", fixed = TRUE)
  expect_match(mac, "sourceFitVersion", fixed = TRUE)

  # Interaction reports and R-backed effect plots use the same live
  # source identity.  Reports are rebuilt in their existing window, plots are
  # updated in place, and stale asynchronous calculations are discarded.
  expect_match(mac, "InteractionDerivedSnapshotIsCurrent", fixed = TRUE)
  expect_match(mac, "RefreshInteractionDerivedOutputsForModelNow", fixed = TRUE)
  expect_match(mac, "ReplaceGLMInteractionReportBody", fixed = TRUE)
  expect_match(mac, "regressionDerivedKind = plot->isRegressionDerivedPlot", fixed = TRUE)
  expect_match(
    mac,
    paste0(
      "BuildInteractionDerivedRefreshSnapshot\\(\\s*",
      "oldLink, snapshot, message, false\\)"
    ),
    perl = TRUE,
    info = paste(
      "Changing a display option must refresh both simple effects and",
      "interactions; simple effects do not contain a colon."
    )
  )
  effect_plot_guards <- lengths(regmatches(
      mac,
      gregexpr("IsPooledRegressionEffectPlot\\(\\*plot\\)", mac, perl = TRUE)
    ))
  expect_true(
    effect_plot_guards >= 3L,
    info = paste(
      "Clearing, applying and source-refitting must all recognize both",
      "effect_plot and interaction_plot."
    )
  )
  for (kind in c("linear_single", "generalized_single",
                 "linear_comparison", "generalized_comparison")) {
    expect_match(mac, paste0("sourceModelKind = \"", kind, "\""), fixed = TRUE)
  }
  expect_match(
    mac,
    paste0(
      "openGeneralizedInteractionReport:[\\s\\S]*?",
      "g_pendingInteractionReportJobs.insert\\(jobKey\\)[\\s\\S]*?",
      "std::thread worker"
    ),
    perl = TRUE,
    info = "Generalized-model effect interpretation must not block AppKit while R refits."
  )
  expect_match(
    mac,
    paste0(
      "openGeneralizedInteractionPlot:[\\s\\S]*?",
      "g_pendingInteractionPlotJobs.insert\\(jobKey\\)[\\s\\S]*?",
      "std::thread worker"
    ),
    perl = TRUE,
    info = "Generalized-model effect plots must not block AppKit while R refits."
  )
  expect_match(mac, "model changed while the effect plot was being calculated",
               fixed = TRUE)

  # Every Windows R post-estimation route (single/comparison, linear/
  # generalized) registers a live dependency, and every asynchronous reply is
  # guarded before it can replace the visible result.
  expect_gte(lengths(regmatches(
    windows, gregexpr("BeginRegressionDerivedOutputRequest\\(", windows)
  )), 7L) # six call sites plus the definition
  expect_gte(lengths(regmatches(
    windows,
    gregexpr("RegressionDerivedOutputRequestIsCurrent\\(dependency\\)", windows)
  )), 6L)
  for (kind in c("linear_single", "generalized_single",
                 "linear_comparison", "generalized_comparison")) {
    expect_match(windows, kind, fixed = TRUE)
  }
  expect_match(windows, "RefreshRegressionDerivedOutputs(group)", fixed = TRUE)
  expect_match(windows, "RefreshRegressionDerivedOutputs(id)", fixed = TRUE)
  expect_match(windows, "Updating from the current source model", fixed = TRUE)
  expect_match(windows, "Waiting for the updated source model", fixed = TRUE)

  # Model revisions count specification changes while fit versions count
  # accepted fits.  Several edits may be coalesced into one R fit, so the two
  # counters must only be compared with their captured snapshot, never ordered
  # against each other.  Ordering them made Windows silently discard valid
  # interaction reports and effect plots.
  expect_false(
    grepl("fitVersion\\s*>=\\s*modelVersion", windows, perl = TRUE),
    info = paste(
      "A current accepted fit may legitimately have a lower fit counter than",
      "its model revision after coalesced specification edits."
    )
  )
  expect_match(
    windows,
    paste0(
      "RegressionDerivedOutputRequestIsCurrent[\\s\\S]*?accepted[\\s\\S]*?",
      "revision == dependency\\.sourceRevision[\\s\\S]*?",
      "fitVersion == dependency\\.sourceFitVersion"
    ),
    perl = TRUE
  )

  # A source refit refreshes report and plot concurrently.  Every native R
  # worker must have a process-wide unique temporary-file suffix; millisecond
  # ticks alone let the two jobs overwrite each other's script/output files.
  expect_gte(lengths(regmatches(
    windows, gregexpr("UniqueNativeRunSuffix\\(\\)", windows, perl = TRUE)
  )), 10L)
  expect_equal(
    lengths(regmatches(
      windows,
      gregexpr("std::to_wstring\\(GetCurrentProcessId\\(\\)\\)",
               windows, perl = TRUE)
    )),
    1L,
    info = paste(
      "The process/tick expression belongs only in UniqueNativeRunSuffix;",
      "native R workers must call the helper instead of rebuilding it."
    )
  )
})
