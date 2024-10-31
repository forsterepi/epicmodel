test_that("constructor gives class", {
  expect_s3_class(new_scc(), "epicmodel_scc")
  expect_s3_class(empty_scc(), "epicmodel_scc")
})

test_that("empty_scc and validate_scc fit together", {
  expect_no_error(empty_scc() %>% validate_scc())
})

test_that("validate_scc finds errors", {
  expect_error(new_scc() %>% validate_scc())
  expect_error(empty_steplist() %>% validate_scc())

  scc_party <- readRDS(test_path("fixtures", "scc_party.rds"))
  expect_no_error(scc_party %>% validate_scc())
})

test_that("plot, print, and summary of epicmodel_scc work", {
  expect_no_error(empty_scc() %>% plot())
  expect_no_error(empty_scc() %>% plot(unknown = F))
  expect_error(empty_scc() %>% plot(unknown = "F"), class = "input_unknown")
  expect_error(empty_scc() %>% plot(names = "F"), class = "input_names")
  expect_no_error(empty_scc() %>% print())
  expect_no_error(empty_scc() %>% summary())

  expect_error(empty_scc() %>% plot(text_color = c("white","blue")), class = "input_text_color")
  expect_error(empty_scc() %>% plot(text_color = "bblack"), class = "invalid_text_color")
  expect_error(empty_scc() %>% plot(pie_color = c("white","blue")), class = "input_pie_color")
  expect_error(empty_scc() %>% plot(pie_color = c("white","blue","bblack")), class = "invalid_pie_color")
  expect_error(empty_scc() %>% plot(border_color = c("white","blue")), class = "input_border_color")
  expect_error(empty_scc() %>% plot(border_color = "bblack"), class = "invalid_border_color")
  expect_warning(empty_scc() %>% plot(border_color = "black"), class = "unknown_plus_border_color")

  scc_party <- readRDS(test_path("fixtures", "scc_party.rds"))
  expect_no_error(scc_party %>% print())
  expect_no_error(plot(scc_party))
  expect_no_error(plot(scc_party, remove_sc = 3))
  expect_error(plot(scc_party, remove_sc = 4), class = "input_remove_sc")
  expect_no_error(plot(scc_party, sc_label = "A"))
  expect_no_error(plot(scc_party, remove_sc = c(2,3), sc_label = c("A","B","C")))
  expect_error(plot(scc_party, remove_sc = c(2,3), sc_label = c("A","A")), class = "input_sc_label")
})

test_that("sufficient_order works", {
  order1 <- data.frame(order = c("a5d1->a7d3e3","a7d3e3->a5d1"), suff = c(T,F))
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  stplist <- steplist_party_test %>% remove_na() %>% remove_segment("d4") %>% check_steplist()

  expect_equal(sufficient_order(order = NA, steplist = stplist), NA_character_)
  expect_equal(sufficient_order(order = order1, steplist = stplist, sufficient = TRUE), "Ana is invited -> birthday party takes place on a weekday")
  expect_equal(sufficient_order(order = order1, steplist = stplist, sufficient = FALSE), "birthday party takes place on a weekday -> Ana is invited")

  order2 <- order1
  order2$suff <- TRUE
  expect_equal(sufficient_order(order = order2, steplist = stplist, sufficient = TRUE),
               c("Ana is invited -> birthday party takes place on a weekday","birthday party takes place on a weekday -> Ana is invited"))
  expect_equal(sufficient_order(order = order2, steplist = stplist, sufficient = FALSE), NA_character_)

  order3 <- order1
  order3$suff <- FALSE
  expect_equal(sufficient_order(order = order3, steplist = stplist), NA_character_)
})

test_that("module_prev works", {
  scc_mini_and <- readRDS(test_path("fixtures", "scc_mini_and.rds"))
  expect_error(module_prev(scc_mini_and), class = "no_modules")

  scc_party <- readRDS(test_path("fixtures", "scc_party.rds"))
  out <- list(cc90 = c("guests: 40% (4/10)","orga: 40% (4/10)","food: 20% (2/10)"),
              cc103 = c("guests: 60% (6/10)","orga: 30% (3/10)","food: 10% (1/10)"),
              cc125 = c("guests: 46% (6/13)","orga: 38% (5/13)","food: 15% (2/13)"))
  expect_equal(module_prev(scc_party), out)
})
