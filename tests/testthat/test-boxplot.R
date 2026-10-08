test_that("boxplot preparation preserves row ids and drops missing y", {
  d <- data.frame(g = c("a", "a", "b", NA), y = c(1, NA, 3, 4))
  out <- LinkEDA:::.rls_prepare_boxplot_data(d, "y", "g", "g1")
  expect_equal(out$row, c(1L, 3L, 4L))
  expect_equal(out$category, c("a", "b", "NA"))
  expect_equal(out$group, "g1")
})

test_that("boxplot summaries use Tukey whiskers by group", {
  d <- data.frame(g = c(rep("a", 5), rep("b", 4)), y = c(1, 2, 3, 4, 100, 10, 11, 12, 13))
  s <- LinkEDA:::.rls_boxplot_summaries(d, "y", "g")
  a <- s[s$category == "a", ]
  expect_equal(a$median, 3)
  expect_equal(a$lower, 1)
  expect_equal(a$upper, 4)
  expect_true("b" %in% s$category)
})

test_that("boxplot preparation supports ordered nested grouping variables", {
  data <- data.frame(
    outer = c("A", "A", "B", "B"),
    inner = c("one", "two", "one", "two"),
    y = c(1, 2, 3, 4)
  )
  prepared <- LinkEDA:::.rls_prepare_boxplot_data(
    data, "y", c("outer", "inner"), "nested"
  )
  expect_identical(prepared$x_names, c("outer", "inner"))
  expect_identical(prepared$x_name, "outer + inner")
  expect_identical(prepared$category, c(
    "outer=A · inner=one", "outer=A · inner=two",
    "outer=B · inner=one", "outer=B · inner=two"
  ))
  expect_error(
    LinkEDA:::.rls_prepare_boxplot_data(data, "y", c("outer", "outer")),
    "unique"
  )
})

test_that("new boxplot uses active dataset validation", {
  ls_register_dataset("box_active", data.frame(g = c("a", "b"), y = c(1, 2)))
  on.exit(ls_unregister_dataset("box_active"), add = TRUE)
  old_launch <- getOption("LinkEDA.launch")
  options(LinkEDA.launch = FALSE)
  on.exit(options(LinkEDA.launch = old_launch), add = TRUE)
  expect_error(ls_new_boxplot(y = "y", x = "g"), "launching is disabled")
  expect_error(LinkEDA:::.rls_prepare_boxplot_data(data.frame(y = letters[1:2]), "y"), "numeric")
})

test_that("boxplot parallel options validate compatible variables", {
  d <- data.frame(g = c("a", "b"), y = c(1, 2), z = c(3, 4), txt = c("x", "y"))
  expect_error(ls_boxplot(d, y = "y", x = "g", connect_rows = TRUE), "ungrouped")
  expect_error(ls_boxplot(d, y = "y", x = "g", standardize = TRUE), "ungrouped")

  assign("box_fake", list(
    id = "box_fake", group = "box_fake_group", x = "", y = "y", n = 2L,
    rows = 1:2, data = d, type = "boxplot", title = "y",
    boxplot_variables = "y"
  ), envir = LinkEDA:::.rls_state$plots)
  on.exit(rm(list = "box_fake", envir = LinkEDA:::.rls_state$plots), add = TRUE)

  expect_equal(LinkEDA:::.rls_boxplot_variable_record("box_fake", "z")$variable, "z")
  expect_error(LinkEDA:::.rls_boxplot_variable_record("box_fake", "txt"), "numeric")

  record <- get("box_fake", envir = LinkEDA:::.rls_state$plots)
  record$x <- "g"
  assign("box_fake", record, envir = LinkEDA:::.rls_state$plots)
  expect_error(LinkEDA:::.rls_boxplot_variable_record("box_fake", "z"), "ungrouped")
})

test_that("boxplot options include standardization flag", {
  expect_true(exists("ls_boxplot_standardize_variables"))
  expect_true(exists("ls_boxplot_show_violin"))
  expect_true(exists("ls_boxplot_split_violin"))
  expect_true(exists("ls_boxplot_h0_simulation"))
  expect_true(exists("ls_boxplot_clear_h0_simulation"))
})

test_that("boxplot split violin display values respect standardization", {
  record <- list(
    type = "boxplot",
    x = "",
    y = "y",
    data = data.frame(y = c(1, 2, 3), z = c(10, 20, 30)),
    boxplot_variables = c("y", "z"),
    standardize = FALSE
  )
  expect_equal(LinkEDA:::.rls_boxplot_display_values(record), c(1, 2, 3, 10, 20, 30))
  record$standardize <- TRUE
  expect_equal(round(LinkEDA:::.rls_boxplot_display_values(record), 6), c(-1, 0, 1, -1, 0, 1))
})

test_that("boxplot H0 simulation validates scalar numeric inputs", {
  assign("box_h0_fake", list(
    id = "box_h0_fake", group = "box_h0_group", x = "", y = "y", n = 3L,
    rows = 1:3, data = data.frame(y = c(1, 2, 3)), type = "boxplot",
    title = "y", boxplot_variables = "y"
  ), envir = LinkEDA:::.rls_state$plots)
  on.exit(rm(list = "box_h0_fake", envir = LinkEDA:::.rls_state$plots), add = TRUE)

  expect_error(ls_boxplot_h0_simulation("box_h0_fake", h0 = NA_real_), "finite")
  expect_error(ls_boxplot_h0_simulation("box_h0_fake", h0 = 0, draws = NA_real_), "finite")
})
