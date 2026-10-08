interaction_menu_source_root <- function() {
  linkeda_source_test_root()
}

test_that("interaction discovery uses one shared hierarchical menu model", {
  skip_if_not(file.exists(file.path(linkeda_source_test_root(), "src", "platform", "windows", "winui", "LinkEDA")),
              "Windows UI sources are absent from the macOS checkout")
  root <- interaction_menu_source_root()
  core <- paste(readLines(file.path(root, "src", "core", "model_terms.cpp"),
                          encoding = "UTF-8",
                          warn = FALSE), collapse = "\n")
  mac <- paste(readLines(file.path(root, "src", "platform", "macos",
                                  "linkeda_macos_app.mm"), warn = FALSE),
               collapse = "\n")
  windows <- paste(readLines(file.path(root, "src", "platform", "windows",
                                      "winui", "LinkEDA",
                                      "WorkflowWindows.cpp"), warn = FALSE),
                   collapse = "\n")

  expect_match(core, "BuildInteractionMenuGroups", fixed = TRUE)
  expect_match(core, "displayParts.push_back(BaseVariableForTermComponent(*anchor))",
               fixed = TRUE)
  expect_match(core, "JoinStrings(displayParts, \" × \")",
               fixed = TRUE)
  expect_match(core, "anchored && parts.size() == requestedVariables.size() + 1",
               fixed = TRUE)
  expect_match(mac, "BuildInteractionMenuGroups(candidates, baseTerm)",
               fixed = TRUE)
  expect_match(mac, "if (!baseTerm.empty())", fixed = TRUE)
  expect_match(mac, "if (order.order != directOrder) continue;", fixed = TRUE)
  expect_match(mac, "if (order.order <= directOrder) continue;", fixed = TRUE)
  expect_match(mac, "ModelTermTypeDisplayName(type)", fixed = TRUE)
  expect_false(grepl('name + " (" + type + ")"', mac, fixed = TRUE))
  expect_match(windows, "BuildInteractionMenuSubItem", fixed = TRUE)
  expect_match(windows, "BuildInteractionMenuGroups(candidates, baseTerm)",
               fixed = TRUE)
  expect_match(windows, "if (!baseTerm.empty())", fixed = TRUE)
  expect_match(windows, "if (orderGroup.order != directOrder) continue;", fixed = TRUE)
  expect_match(windows, "if (orderGroup.order <= directOrder) continue;", fixed = TRUE)
})
