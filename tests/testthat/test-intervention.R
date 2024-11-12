test_that("get_intv works", {
  steplist_rain_test <- readRDS(test_path("fixtures", "steplist_rain_test.rds")) %>% check_steplist()
  scc_rain_test <- create_scc(steplist_rain_test)
  split_rain_test <- scc_rain_test$steplist %>% process_steplist() %>% split_prc()

  expect_error(get_intv(scc_rain_test, c("THENd6a6", "all"), split_rain_test), class = "invalid_interventions_all")
  expect_error(get_intv(scc_rain_test, c("THENd6a6","abc"), split_rain_test), class = "invalid_interventions")
  expect_error(get_intv(scc_rain_test, "abc", split_rain_test), class = "invalid_interventions")

  expect_no_error(get_intv(scc_rain_test, NULL, split_rain_test))

  expect_no_error(get_intv(scc_rain_test, "THENd6a6", split_rain_test))
  intv1 <- data.frame(THENd6a6 = TRUE, THENd6a7 = FALSE, THENd4e1 = FALSE) %>% magrittr::set_rownames("intv1")
  expect_equal(get_intv(scc_rain_test, "THENd6a6", split_rain_test), intv1)

  expect_no_error(get_intv(scc_rain_test, c("THENd6a6","THENd6a7"), split_rain_test))
  intv2 <- data.frame(THENd6a6 = c(T,F,T), THENd6a7 = c(T,T,F), THENd4e1 = c(F,F,F)) %>% magrittr::set_rownames(c("intv1","intv2","intv3"))
  expect_equal(get_intv(scc_rain_test, c("THENd6a6","THENd6a7"), split_rain_test) %>% magrittr::set_attr("out.attrs", NULL), intv2)

  expect_no_error(get_intv(scc_rain_test, c("all"), split_rain_test))
  intv3 <- data.frame(THENd6a6 = c(T,F,T,F,T,F,T), THENd6a7 = c(T,T,F,F,T,T,F), THENd4e1 = c(T,T,T,T,F,F,F)) %>%
    magrittr::set_rownames(paste0("intv",c(1:7)))
  expect_equal(get_intv(scc_rain_test, "all", split_rain_test) %>% magrittr::set_attr("out.attrs", NULL), intv3)

  steplist_mini_and <- readRDS(test_path("fixtures", "steplist_mini_and.rds")) %>% check_steplist()
  scc_mini_and <- create_scc(steplist_mini_and)
  split_mini_and <- scc_mini_and$steplist %>% process_steplist() %>% split_prc()
  expect_equal(get_intv(scc_mini_and, NULL, split_mini_and), NULL)

  steplist_mini_intv <- readRDS(test_path("fixtures", "steplist_mini_intv.rds")) %>% check_steplist()
  scc_mini_intv <- create_scc(steplist_mini_intv)
  split_mini_intv <- scc_mini_intv$steplist %>% process_steplist() %>% split_prc()
  expect_no_error(get_intv(scc_mini_intv, "all", split_mini_intv))
  expect_no_error(get_intv(scc_mini_intv, "THENa2", split_mini_intv))

  steplist_mini_intv_to_depends <- readRDS(test_path("fixtures", "steplist_mini_intv_to_depends.rds")) %>% check_steplist()
  scc_mini_intv_to_depends <- create_scc(steplist_mini_intv_to_depends)
  split_mini_intv_to_depends <- scc_mini_intv_to_depends$steplist %>% process_steplist() %>% split_prc()
  expect_no_error(get_intv(scc_mini_intv_to_depends, "all", split_mini_intv_to_depends))
  expect_no_error(get_intv(scc_mini_intv_to_depends, "THENa6", split_mini_intv_to_depends))
  expect_no_error(get_intv(scc_mini_intv_to_depends, c("THENa6","THENa7"), split_mini_intv_to_depends))
})

test_that("get_causes works", {
  steplist_rain_test <- readRDS(test_path("fixtures", "steplist_rain_test.rds")) %>% check_steplist()
  scc_rain_test <- create_scc(steplist_rain_test)
  split_rain_test <- scc_rain_test$steplist %>% process_steplist() %>% split_prc()

  expect_error(get_causes(scc_rain_test, c("THENa5", "all"), split_rain_test), class = "invalid_causes_all")
  expect_error(get_causes(scc_rain_test, c("THENa5","abc"), split_rain_test), class = "invalid_causes")
  expect_error(get_causes(scc_rain_test, "abc", split_rain_test), class = "invalid_causes")

  expect_no_error(get_causes(scc_rain_test, NULL, split_rain_test))
  expect_equal(get_causes(scc_rain_test, "THENa5", split_rain_test), NULL)

  expect_no_error(get_causes(scc_rain_test, c("THENd2a3","THENa1"), split_rain_test))
  causes1 <- data.frame(IFNOTd6a6THENd5a6 = F, THENa5 = F, THENa1 = T, THENd2a3 = T) %>% magrittr::set_rownames("cc5")
  expect_equal(get_causes(scc_rain_test, c("THENd2a3","THENa1"), split_rain_test) %>% magrittr::set_attr("out.attrs", NULL), causes1)

  expect_no_error(get_causes(scc_rain_test, c("all"), split_rain_test))
  expect_equal(get_causes(scc_rain_test, "all", split_rain_test), scc_rain_test$sc_cc)

  steplist_mini_intv <- readRDS(test_path("fixtures", "steplist_mini_intv.rds")) %>% check_steplist()
  scc_mini_intv <- create_scc(steplist_mini_intv)
  split_mini_intv <- scc_mini_intv$steplist %>% process_steplist() %>% split_prc()

  expect_no_error(get_causes(scc_mini_intv, "all", split_mini_intv))
  expect_no_error(get_causes(scc_mini_intv, "THENa1", split_mini_intv))

  steplist_mini_intv_to_depends <- readRDS(test_path("fixtures", "steplist_mini_intv_to_depends.rds")) %>% check_steplist()
  scc_mini_intv_to_depends <- create_scc(steplist_mini_intv_to_depends)
  split_mini_intv_to_depends <- scc_mini_intv_to_depends$steplist %>% process_steplist() %>% split_prc()
  expect_no_error(get_causes(scc_mini_intv_to_depends, "all", split_mini_intv_to_depends))
  expect_no_error(get_causes(scc_mini_intv_to_depends, "IFNOTa7THENa3", split_mini_intv_to_depends))
  expect_no_error(get_causes(scc_mini_intv_to_depends, c("THENa2","IFNOTa7THENa3","IFNOTa6THENa1"), split_mini_intv_to_depends))
})

test_that("get_prevented_* works", {
  x <- readRDS(test_path("fixtures", "steplist_rain_test.rds")) %>% process_steplist() %>% split_prc()

  expect_setequal(get_prevented_causes(x$causes, "THENd6a6"), "IFNOTd6a6THENd5a6")
  expect_setequal(get_prevented_causes(x$causes, "THENd6a7"), character(0))
  expect_setequal(get_prevented_causes(x$causes, "THENd4e1"), character(0))
  expect_setequal(get_prevented_causes(x$causes, c("THENd6a6","THENd6a7")), "IFNOTd6a6THENd5a6")
  expect_setequal(get_prevented_causes(x$causes, c("THENd6a7","THENd4e1")), character(0))

  expect_setequal(get_prevented_non_start_steps(x$non_start_steps, "THENd6a6"), character(0))
  expect_setequal(get_prevented_non_start_steps(x$non_start_steps, "THENd6a7"), "IFd1e2+a1IFNOTd6a7THENa8d2a2")
  expect_setequal(get_prevented_non_start_steps(x$non_start_steps, "THENd4e1"), "IFd5a6+a5IFNOTd4e1THENd3e3")
  expect_setequal(get_prevented_non_start_steps(x$non_start_steps, c("THENd6a6","THENd6a7")), "IFd1e2+a1IFNOTd6a7THENa8d2a2")
  expect_setequal(get_prevented_non_start_steps(x$non_start_steps, c("THENd6a7","THENd4e1")),
                  c("IFd1e2+a1IFNOTd6a7THENa8d2a2", "IFd5a6+a5IFNOTd4e1THENd3e3"))
  expect_setequal(get_prevented_non_start_steps(x$non_start_steps, c("THENd6a6","THENd4e1")), "IFd5a6+a5IFNOTd4e1THENd3e3")
  expect_setequal(get_prevented_non_start_steps(x$non_start_steps, c("THENd6a6","THENd6a7","THENd4e1")),
                  c("IFd1e2+a1IFNOTd6a7THENa8d2a2", "IFd5a6+a5IFNOTd4e1THENd3e3"))

  x2 <- readRDS(test_path("fixtures", "steplist_mini_intv.rds")) %>% process_steplist() %>% split_prc()
  expect_setequal(get_prevented_causes(x2$causes, "THENa2"), character(0))
  expect_setequal(get_prevented_non_start_steps(x2$non_start_steps, "THENa2"), "IFa1IFNOTa2THENa3")

  x3 <- readRDS(test_path("fixtures", "steplist_mini_intv_to_depends.rds")) %>% process_steplist() %>% split_prc()
  expect_setequal(get_prevented_causes(x3$causes, "THENa2"), character(0))
  expect_setequal(get_prevented_causes(x3$causes, "THENa6"), "IFNOTa6THENa1")
  expect_setequal(get_prevented_causes(x3$causes, c("THENa6","THENa7")), c("IFNOTa7THENa3","IFNOTa6THENa1"))
  expect_setequal(get_prevented_non_start_steps(x3$non_start_steps, "THENa2"), "IFa1IFNOTa2THENa4")
  expect_setequal(get_prevented_non_start_steps(x3$non_start_steps, "THENa6"), character(0))
  expect_setequal(get_prevented_non_start_steps(x3$non_start_steps, c("THENa6","THENa7")), character(0))
})

test_that("check_causes_x_intv works", {
  steplist_rain_test <- readRDS(test_path("fixtures", "steplist_rain_test.rds")) %>% check_steplist()
  scc_rain_test <- create_scc(steplist_rain_test)
  prc_rain_test <- scc_rain_test$steplist %>% process_steplist()
  split_rain_test <- prc_rain_test %>% split_prc()
  outc_list_rain_test <- scc_rain_test$steplist %>% transform_outc()
  cause_set_rain_test <- get_causes(scc_rain_test, "all", split_rain_test)

  expect_no_error(check_causes_x_intv(scc_rain_test, cause_set_rain_test %>% dplyr::slice(1), intv = c("THENd6a6","THENd6a7"),
                                      prc_rain_test, split_rain_test, outc_list_rain_test))
  expect_error(check_causes_x_intv(scc_rain_test, cause_set_rain_test %>% dplyr::slice(1), intv_rain_test,
                                   prc_rain_test, split_rain_test, outc_list_rain_test), class = "input_intv")
  expect_error(check_causes_x_intv(scc_rain_test, cause_set_rain_test, intv = c("THENd6a6","THENd6a7"),
                                   prc_rain_test, split_rain_test, outc_list_rain_test), class = "input_cause_set")
  expect_equal(check_causes_x_intv(scc_rain_test, cause_set_rain_test %>% dplyr::slice(1), intv = c("THENd6a6","THENd6a7"),
                                   prc_rain_test, split_rain_test, outc_list_rain_test) %>% magrittr::extract2(1), "never")
  expect_equal(check_causes_x_intv(scc_rain_test, cause_set_rain_test %>% dplyr::slice(1), intv = c("THENd6a6","THENd6a7"),
                                   prc_rain_test, split_rain_test, outc_list_rain_test) %>% magrittr::extract2(2), NA)

  steplist_mini_intv <- readRDS(test_path("fixtures", "steplist_mini_intv.rds")) %>% check_steplist()
  scc_mini_intv <- create_scc(steplist_mini_intv)
  prc_mini_intv <- scc_mini_intv$steplist %>% process_steplist()
  split_mini_intv <- prc_mini_intv %>% split_prc()
  outc_list_mini_intv <- scc_mini_intv$steplist %>% transform_outc()
  cause_set_mini_intv <- get_causes(scc_mini_intv, "all", split_mini_intv)

  expect_no_error(check_causes_x_intv(scc_mini_intv, cause_set_mini_intv %>% dplyr::slice(1), intv = "THENa2",
                                      prc_mini_intv, split_mini_intv, outc_list_mini_intv))
  expect_equal(check_causes_x_intv(scc_mini_intv, cause_set_mini_intv %>% dplyr::slice(1), intv = "THENa2",
                                   prc_mini_intv, split_mini_intv, outc_list_mini_intv) %>% magrittr::extract2(1), "never")
  expect_equal(check_causes_x_intv(scc_mini_intv, cause_set_mini_intv %>% dplyr::slice(1), intv = "THENa2",
                                   prc_mini_intv, split_mini_intv, outc_list_mini_intv) %>% magrittr::extract2(2), NA)

  steplist_mini_intv_to_depends <- readRDS(test_path("fixtures", "steplist_mini_intv_to_depends.rds")) %>% check_steplist()
  scc_mini_intv_to_depends <- create_scc(steplist_mini_intv_to_depends)
  prc_mini_intv_to_depends <- scc_mini_intv_to_depends$steplist %>% process_steplist()
  split_mini_intv_to_depends <- prc_mini_intv_to_depends %>% split_prc()
  outc_list_mini_intv_to_depends <- scc_mini_intv_to_depends$steplist %>% transform_outc()
  cause_set_mini_intv_to_depends <- get_causes(scc_mini_intv_to_depends, c("THENa2","IFNOTa7THENa3","IFNOTa6THENa1"), split_mini_intv_to_depends)

  expect_no_error(check_causes_x_intv(scc_mini_intv_to_depends, cause_set_mini_intv_to_depends %>% dplyr::slice(1), intv = "THENa7",
                                      prc_mini_intv_to_depends, split_mini_intv_to_depends, outc_list_mini_intv_to_depends))
  expect_equal(check_causes_x_intv(scc_mini_intv_to_depends, cause_set_mini_intv_to_depends %>% dplyr::slice(1), intv = "THENa7",
                                   prc_mini_intv_to_depends, split_mini_intv_to_depends, outc_list_mini_intv_to_depends) %>% magrittr::extract2(1),
               "depends")
  order_mini_intv_to_depends <- data.frame(order = c("a1->a2","a2->a1"), suff = c(T,F))
  expect_equal(check_causes_x_intv(scc_mini_intv_to_depends, cause_set_mini_intv_to_depends %>% dplyr::slice(1), intv = "THENa7",
                                   prc_mini_intv_to_depends, split_mini_intv_to_depends, outc_list_mini_intv_to_depends) %>% magrittr::extract2(2),
               order_mini_intv_to_depends)
})

test_that("minimize_intv works", {
  intv1 <- data.frame(THENa2 = TRUE) %>% magrittr::set_rownames("intv1")
  out_status1 <- data.frame(intv0 = "always", intv1 = "never") %>% magrittr::set_rownames("cc1")
  mini1 <- data.frame(intv1 = TRUE) %>% magrittr::set_rownames("cc1")

  expect_equal(minimize_intv(intv = intv1, out_status = out_status1), mini1)

  steplist_rain_test <- readRDS(test_path("fixtures", "steplist_rain_test.rds")) %>% check_steplist()
  scc_rain_test <- create_scc(steplist_rain_test)
  intv_rain_test <- scc_rain_test %>% intervene(causes = "all", intervention = "all", output = "table")
  mini_rain_test <- data.frame(intv1 = c(F,F), intv2 = c(F,F), intv3 = c(F,F), intv4 = c(F,T), intv5 = c(F,F), intv6 = c(T,T), intv7 = c(F,T)) %>%
    magrittr::set_rownames(c("cc5","cc14"))

  expect_equal(intv_rain_test$mini, mini_rain_test)
})

test_that("intervene works as expected", {
  steplist_rain_test <- readRDS(test_path("fixtures", "steplist_rain_test.rds")) %>% check_steplist()
  scc_rain_test <- create_scc(steplist_rain_test)

  expect_no_error(scc_rain_test %>% intervene(causes = "all", intervention = "all"))
  expect_no_error(scc_rain_test %>% intervene(causes = "all"))
  expect_no_error(scc_rain_test %>% intervene(intervention = "all"))
  expect_no_error(scc_rain_test %>% intervene(causes = "THENa5", intervention = "all"))
  expect_no_error(scc_rain_test %>% intervene(causes = "THENa5", intervention = "THENd4e1"))
  expect_no_error(scc_rain_test %>% intervene(causes = "all", intervention = c("THENd4e1","THENd6a7")))

  steplist_mini_intv <- readRDS(test_path("fixtures", "steplist_mini_intv.rds")) %>% check_steplist()
  scc_mini_intv <- create_scc(steplist_mini_intv)

  expect_no_error(scc_mini_intv %>% intervene(causes = "all", intervention = "all"))
  status_mini_intv <- data.frame(intv0 = "always", intv1 = "never") %>% magrittr::set_rownames("cc1")
  expect_equal(intervene(scc_mini_intv, causes = "all", intervention = "all", output = "table")[["status"]], status_mini_intv)

  steplist_mini_intv_to_depends <- readRDS(test_path("fixtures", "steplist_mini_intv_to_depends.rds")) %>% check_steplist()
  scc_mini_intv_to_depends <- create_scc(steplist_mini_intv_to_depends)

  expect_no_error(scc_mini_intv_to_depends %>% intervene(causes = "all", intervention = "all"))
  expect_no_error(scc_mini_intv_to_depends %>% intervene(causes = c("THENa2","IFNOTa7THENa3","IFNOTa6THENa1"), intervention = "THENa7",
                                                         output = "table"))
  status_mini_intv <- data.frame(intv0 = "always", intv1 = "depends") %>% magrittr::set_rownames("cc7")
  expect_equal(intervene(scc_mini_intv_to_depends, causes = c("THENa2","IFNOTa7THENa3","IFNOTa6THENa1"), intervention = "THENa7",
                                         output = "table")[["status"]], status_mini_intv)
  order_mini_intv <- data.frame(order = c("a1->a2","a2->a1"), suff = c(T,F))
  expect_equal(intervene(scc_mini_intv_to_depends, causes = c("THENa2","IFNOTa7THENa3","IFNOTa6THENa1"), intervention = "THENa7",
                         output = "table")[["order"]][["intv1"]][["cc7"]], order_mini_intv)

  scc_party <- readRDS(test_path("fixtures", "scc_party.rds"))
  expect_no_error(scc_party %>% intervene(causes = "all"))
  expect_no_error(scc_party %>% intervene(causes = "all", intervention = "all"))
  expect_no_error(scc_party %>% intervene(causes = "THENa5d1", intervention = "all"))
})
