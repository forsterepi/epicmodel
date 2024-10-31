test_that("all_true works", {
  expect_true(c(T,T,T) %>% all_true())
  expect_false(c(T,T,F) %>% all_true())
  expect_false(c(F,F,F) %>% all_true())
  expect_true(TRUE %>% all_true())
  expect_false(FALSE %>% all_true())

  expect_error(NA %>% all_true(), class = "input_all_true")
  expect_error(c(T,T,T,NA) %>% all_true(), class = "input_all_true")
  expect_error(NULL %>% all_true(), class = "input_all_true")
  expect_error(c("TRUE","TRUE") %>% all_true(), class = "input_all_true")
})

test_that("all_false works", {
  expect_false(c(T,T,T) %>% all_false())
  expect_false(c(T,T,F) %>% all_false())
  expect_true(c(F,F,F) %>% all_false())
  expect_false(TRUE %>% all_false())
  expect_true(FALSE %>% all_false())

  expect_error(NA %>% all_false(), class = "input_all_false")
  expect_error(c(T,T,T,NA) %>% all_false(), class = "input_all_false")
  expect_error(NULL %>% all_false(), class = "input_all_false")
  expect_error(c("TRUE","TRUE") %>% all_false(), class = "input_all_false")
})
