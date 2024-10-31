test_that("uncheck_steplist works", {
  expect_error(uncheck_steplist(NA), class = "input_steplist")
  expect_error(uncheck_steplist(NULL), class = "input_steplist")
  expect_error(uncheck_steplist(""), class = "input_steplist")

  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  x1 <- uncheck_steplist(steplist_party_test)
  expect_equal(class(x1), "epicmodel_steplist")

  x2 <- steplist_party_test %>% remove_na() %>% remove_segment("d4") %>% check_steplist()
  expect_equal(class(x2), "epicmodel_steplist_checked")
  x2 %<>% uncheck_steplist()
  expect_equal(class(x2), "epicmodel_steplist")
})

test_that("remove_all_modules works", {
  expect_error(remove_all_modules(NA), class = "no_epicmodel_steplist")

  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_false(steplist_party_test %>% remove_all_modules() %>% are_modules_used())
  expect_no_error(steplist_party_test %>% remove_all_modules() %>% check_modules_in_steps())
  expect_no_warning(steplist_party_test %>% remove_all_modules() %>% check_modules_in_steps())

  x <- remove_all_modules(steplist_party_test)
  non_empty_elements <- x$step$module_step %>% magrittr::is_in("") %>% magrittr::not() %>% sum()
  expect_equal(x$module %>% nrow(), 0)
  expect_equal(non_empty_elements, 0)

  x <- steplist_party_test %>% remove_na() %>% remove_segment("d4") %>% check_steplist()
  expect_equal(class(x), "epicmodel_steplist_checked")
  x %<>% remove_all_modules()
  expect_equal(class(x), "epicmodel_steplist")
})

test_that("remove_na works", {
  expect_error(remove_na(NA), class = "no_epicmodel_steplist")

  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  x <- steplist_party_test
  x$icc %<>% dplyr::filter(id1 != "NA")
  expect_equal(remove_na(steplist_party_test),x)

  x <- remove_na(empty_steplist())
  expect_equal(nrow(x$outc),0)

  expect_no_error(steplist_party_test %>% remove_na() %>% check_steps_in_icc())
  expect_no_error(steplist_party_test %>% remove_na() %>% check_outc())

  x <- steplist_party_test %>% remove_na() %>% remove_segment("d4") %>% check_steplist()
  expect_equal(class(x), "epicmodel_steplist_checked")
  x %<>% remove_na()
  expect_equal(class(x), "epicmodel_steplist")
})

test_that("remove_segment works", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_error(remove_segment(steplist_party_test,NA), class = "no_character")
  expect_error(remove_segment(steplist_party_test,NULL), class = "no_character")
  expect_error(remove_segment(steplist_party_test,d4), class = "no_character")
  expect_error(remove_segment(steplist_party_test,c("d4","d3")), class = "no_single_character")

  expect_error(remove_segment(NA,"d4"), class = "no_epicmodel_steplist")
  expect_error(remove_segment(NULL,"d4"), class = "no_epicmodel_steplist")
  expect_error(remove_segment("","d4"), class = "no_epicmodel_steplist")

  expect_error(remove_segment(steplist_party_test,"a2d3e4"), class = "wrong_pattern")
  expect_error(remove_segment(steplist_party_test,"IFa2e3THENa3"), class = "wrong_pattern")

  x <- remove_segment(steplist_party_test,"d4")
  expect_error(remove_segment(x,"d4"), class = "id_not_found")

  tab <- steplist_party_test
  tab$what %<>% dplyr::filter(id_what != "a1")
  expect_equal(remove_segment(steplist_party_test,"a1"),tab)

  tab <- steplist_party_test
  tab$does %<>% dplyr::filter(id_does != "d4")
  expect_equal(remove_segment(steplist_party_test,"d4"),tab)

  tab <- steplist_party_test
  tab$where %<>% dplyr::filter(id_where != "e1")
  expect_equal(remove_segment(steplist_party_test,"e1"),tab)

  tab <- steplist_party_test
  tab$module %<>% dplyr::filter(id_module != "m1")
  expect_equal(remove_segment(steplist_party_test,"m1"),tab)

  tab <- steplist_party_test
  tab$icc %<>% dplyr::filter(id_icc != "i1")
  expect_equal(remove_segment(steplist_party_test,"i1"),tab)

  x <- steplist_party_test %>% remove_na() %>% remove_segment("d4") %>% check_steplist()
  expect_equal(class(x), "epicmodel_steplist_checked")
  x %<>% remove_segment("a1")
  expect_equal(class(x), "epicmodel_steplist")
})
