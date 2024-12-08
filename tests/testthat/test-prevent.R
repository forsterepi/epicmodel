test_that("get_causes_prevent works", {
  scc_rain_test <- scc_rain
  split_rain_test <- scc_rain_test$steplist %>% process_steplist() %>% split_prc()

  expect_error(get_causes_prevent(scc_rain_test, c("THENa5","abc"), split_rain_test), class = "invalid_causes")
  expect_error(get_causes_prevent(scc_rain_test, "abc", split_rain_test), class = "invalid_causes")

  expect_no_error(get_causes_prevent(scc_rain_test, NULL, split_rain_test))
  expect_equal(get_causes_prevent(scc_rain_test, "THENa5", split_rain_test), NULL)

  expect_no_error(get_causes_prevent(scc_rain_test, c("THENd2a3","THENa1"), split_rain_test))
  causes1 <- data.frame(THENa1 = c(T,F,F), THENd2a3 = c(F,T,F)) %>% magrittr::set_rownames(c("prev1","prev2","prev3"))
  expect_equal(get_causes_prevent(scc_rain_test, c("THENd2a3","THENa1"), split_rain_test), causes1)

  steplist_mini_intv <- readRDS(test_path("fixtures", "steplist_mini_intv.rds")) %>% check_steplist()
  scc_mini_intv <- create_scc(steplist_mini_intv)
  split_mini_intv <- scc_mini_intv$steplist %>% process_steplist() %>% split_prc()

  expect_no_error(get_causes_prevent(scc_mini_intv, "THENa1", split_mini_intv))
  expect_no_error(get_causes_prevent(scc_mini_intv, causes = NULL, split_mini_intv))
  expect_equal(get_causes_prevent(scc_mini_intv, "THENa1", split_mini_intv),
               data.frame(THENa1 = F) %>% magrittr::set_rownames("prev1"))

  steplist_mini_intv_to_depends <- readRDS(test_path("fixtures", "steplist_mini_intv_to_depends.rds")) %>% check_steplist()
  scc_mini_intv_to_depends <- create_scc(steplist_mini_intv_to_depends)
  split_mini_intv_to_depends <- scc_mini_intv_to_depends$steplist %>% process_steplist() %>% split_prc()
  expect_no_error(get_causes_prevent(scc_mini_intv_to_depends, "IFNOTa7THENa3", split_mini_intv_to_depends))
  expect_no_error(get_causes_prevent(scc_mini_intv_to_depends, c("THENa2","IFNOTa7THENa3","IFNOTa6THENa1"), split_mini_intv_to_depends))
})

test_that("minimize_prev works", {
  scc_rain_test <- scc_rain
  split_rain_test <- scc_rain_test$steplist %>% process_steplist() %>% split_prc()

  pc <- get_causes_prevent(scc_rain_test, c("IFNOTd6a6THENd5a6","THENa5","THENa1","THENd2a3"), split_rain_test)
  pc %<>% magrittr::not() %>% as.data.frame()

  pc1 <- pc
  pc1_compare <- pc1[c(1:4),]
  expect_equal(minimize_prev(pc1), pc1_compare)

  pc2 <- pc[15,]
  expect_equal(minimize_prev(pc2), pc2)

  pc3 <- pc[c(8:12),]
  pc3_compare <- pc3[c(1:3),]
  expect_equal(minimize_prev(pc3), pc3_compare)

  steplist_mini_intv <- readRDS(test_path("fixtures", "steplist_mini_intv.rds")) %>% check_steplist()
  scc_mini_intv <- create_scc(steplist_mini_intv)
  split_mini_intv <- scc_mini_intv$steplist %>% process_steplist() %>% split_prc()

  pc_mini_int <- get_causes_prevent(scc_mini_intv, "THENa1", split_mini_intv)
  pc_mini_int %<>% magrittr::not() %>% as.data.frame()

  expect_equal(minimize_prev(pc_mini_int), pc_mini_int)
})

test_that("prevent works as expected", {
  scc_rain_test <- scc_rain

  expect_no_error(scc_rain_test %>% prevent())
  expect_no_error(scc_rain_test %>% prevent(output = "table"))
  expect_no_error(scc_rain_test %>% prevent(causes = "THENa1"))
  expect_no_error(scc_rain_test %>% prevent(causes = "THENa1", output = "table"))
  expect_no_error(scc_rain_test %>% prevent(causes = c("THENa1","THENd2a3")))
  expect_no_error(scc_rain_test %>% prevent(causes = c("THENa1","THENd2a3"), output = "table"))

  expect_equal(scc_rain_test %>% prevent(causes = c("THENa1","THENd2a3"), output = "table"),
               data.frame(THENa1 = c(F,T), THENd2a3 = c(T,F)) %>%
                 magrittr::set_rownames(c("prev1","prev2")))
  expect_equal(scc_rain_test %>% prevent(causes = c("IFNOTd6a6THENd5a6","THENa5","THENa1","THENd2a3"), output = "table"),
               data.frame(IFNOTd6a6THENd5a6 = c(F,F,T), THENa5 = c(F,T,F), THENa1 = c(T,F,F), THENd2a3 = c(F,T,T)) %>%
                 magrittr::set_rownames(c("prev2","prev6","prev7")))

  steplist_mini_intv <- readRDS(test_path("fixtures", "steplist_mini_intv.rds")) %>% check_steplist()
  scc_mini_intv <- create_scc(steplist_mini_intv)

  expect_no_error(scc_mini_intv %>% prevent())
  expect_no_error(scc_mini_intv %>% prevent("THENa1"))
  expect_equal(scc_mini_intv %>% prevent("THENa1", output = "table"),
               data.frame(THENa1 = c(T)) %>% magrittr::set_rownames("prev1"))

  testthat::skip("Check scc_party example, also here and in intervene() check for ICC")
  scc_party <- readRDS(test_path("fixtures", "scc_party.rds"))
  expect_error(scc_party %>% prevent(c("THENa5d1","THENa4d1","THENa6d1","THENa7d3e3","THENa3","THENa7d3e4","THENa7d3e5","THENa7d3e6")), class = "icc_causes")
  expect_no_error(scc_party %>% prevent(c("THENa5d1","THENa4d1","THENa6d1","THENa7d3e3","THENa3","THENa7d3e4")))
})
