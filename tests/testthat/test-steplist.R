test_that("constructor gives class", {
  expect_s3_class(new_steplist(), "epicmodel_steplist")
  expect_s3_class(empty_steplist(), "epicmodel_steplist")
})

test_that("empty_steplist and validate_steplist fit together", {
  expect_no_error(empty_steplist() %>% validate_steplist())
})

test_that("validate_steplist finds errors plus generics", {
  expect_error(new_steplist() %>% validate_steplist())
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_no_error(steplist_party_test %>% validate_steplist())

  steplist_rain_test <- readRDS(test_path("fixtures", "steplist_rain_test.rds"))
  steplist_rain_test_checked <- steplist_rain_test %>% check_steplist()
  expect_no_error(validate_steplist(steplist_rain_test_checked))

  expect_no_error(print(steplist_rain_test))
  expect_no_error(print(steplist_rain_test_checked))
  expect_no_error(empty_steplist() %>% print())

  expect_no_error(summary(steplist_rain_test))
  expect_no_error(summary(steplist_rain_test_checked))
  expect_no_error(empty_steplist() %>% summary())

  expect_no_error(plot(steplist_rain_test))
  expect_no_error(plot(steplist_rain_test_checked))
  expect_no_error(empty_steplist() %>% plot())
})
