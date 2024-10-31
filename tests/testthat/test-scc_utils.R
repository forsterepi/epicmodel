test_that("split_prc works", {
  expect_error(split_prc(NULL), class = "input_prc")
  expect_error(split_prc(NA), class = "input_prc")

  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_error(split_prc(steplist_party_test$step), class = "input_prc")

  steplist_mini_and_prc <- readRDS(test_path("fixtures", "steplist_mini_and.rds")) %>% process_steplist()
  expect_equal(split_prc(steplist_mini_and_prc) %>% magrittr::extract2("non_start_steps"),
               steplist_mini_and_prc %>% dplyr::filter(id_step == "IFa1+a2THENa3"))
  expect_equal(split_prc(steplist_mini_and_prc) %>% magrittr::extract2("causes"),
               steplist_mini_and_prc %>% dplyr::filter(id_step %in% c("THENa1","THENa2")))
  expect_equal(split_prc(steplist_mini_and_prc) %>% magrittr::extract2("interventions"),
               steplist_mini_and_prc %>% dplyr::filter(id_step == "aaa"))
  expect_equal(split_prc(steplist_mini_and_prc) %>% magrittr::extract2("ifnot_steps"), character(0))
  expect_equal(split_prc(steplist_mini_and_prc) %>% magrittr::extract2("end_steps"), c("IFa1+a2THENa3"))

  steplist_mini_depends_prc <- readRDS(test_path("fixtures", "steplist_mini_depends.rds")) %>% process_steplist()
  expect_equal(split_prc(steplist_mini_depends_prc) %>% magrittr::extract2("non_start_steps"),
               steplist_mini_depends_prc %>% dplyr::filter(id_step %in% c("IFa1IFNOTa2THENa3","IFa2+a3THENa4")))
  expect_equal(split_prc(steplist_mini_depends_prc) %>% magrittr::extract2("causes"),
               steplist_mini_depends_prc %>% dplyr::filter(id_step %in% c("THENa1","THENa2")))
  expect_equal(split_prc(steplist_mini_depends_prc) %>% magrittr::extract2("interventions"),
               steplist_mini_depends_prc %>% dplyr::filter(id_step == "aaa"))
  expect_equal(split_prc(steplist_mini_depends_prc) %>% magrittr::extract2("ifnot_steps"), c("IFa1IFNOTa2THENa3"))
  expect_equal(split_prc(steplist_mini_depends_prc) %>% magrittr::extract2("end_steps"), c("IFa2+a3THENa4"))

  steplist_mini_implau_prc <- readRDS(test_path("fixtures", "steplist_mini_implau.rds")) %>% process_steplist()
  expect_equal(split_prc(steplist_mini_implau_prc) %>% magrittr::extract2("non_start_steps"),
               steplist_mini_implau_prc %>% dplyr::filter(id_step %in% c("IFa2IFNOTa3THENa5","IFa1IFNOTa5THENa4","IFa4+a5+a3THENa6")))
  expect_equal(split_prc(steplist_mini_implau_prc) %>% magrittr::extract2("causes"),
               steplist_mini_implau_prc %>% dplyr::filter(id_step %in% c("THENa1","THENa2","THENa3")))
  expect_equal(split_prc(steplist_mini_implau_prc) %>% magrittr::extract2("interventions"),
               steplist_mini_implau_prc %>% dplyr::filter(id_step == "aaa"))
  expect_equal(split_prc(steplist_mini_implau_prc) %>% magrittr::extract2("ifnot_steps"), c("IFa2IFNOTa3THENa5","IFa1IFNOTa5THENa4"))
  expect_equal(split_prc(steplist_mini_implau_prc) %>% magrittr::extract2("end_steps"), c("IFa4+a5+a3THENa6"))

  steplist_mini_intv_to_depends_prc <- readRDS(test_path("fixtures", "steplist_mini_intv_to_depends.rds")) %>% process_steplist()
  expect_equal(split_prc(steplist_mini_intv_to_depends_prc) %>% magrittr::extract2("non_start_steps"),
               steplist_mini_intv_to_depends_prc %>% dplyr::filter(id_step %in% c("IFa1IFNOTa2THENa4","IF(a4+a2)or(a3)THENa5")))
  expect_equal(split_prc(steplist_mini_intv_to_depends_prc) %>% magrittr::extract2("causes"),
               steplist_mini_intv_to_depends_prc %>% dplyr::filter(id_step %in% c("THENa2","IFNOTa7THENa3","IFNOTa6THENa1")))
  expect_equal(split_prc(steplist_mini_intv_to_depends_prc) %>% magrittr::extract2("interventions"),
               steplist_mini_intv_to_depends_prc %>% dplyr::filter(id_step %in% c("THENa6","THENa7")))
  expect_equal(split_prc(steplist_mini_intv_to_depends_prc) %>% magrittr::extract2("ifnot_steps"),
               c("IFNOTa7THENa3","IFNOTa6THENa1","IFa1IFNOTa2THENa4"))
  expect_equal(split_prc(steplist_mini_intv_to_depends_prc) %>% magrittr::extract2("end_steps"), c("IF(a4+a2)or(a3)THENa5"))

  steplist_mini_intv_prc <- readRDS(test_path("fixtures", "steplist_mini_intv.rds")) %>% process_steplist()
  expect_equal(split_prc(steplist_mini_intv_prc) %>% magrittr::extract2("non_start_steps"),
               steplist_mini_intv_prc %>% dplyr::filter(id_step %in% c("IFa1IFNOTa2THENa3")))
  expect_equal(split_prc(steplist_mini_intv_prc) %>% magrittr::extract2("causes"),
               steplist_mini_intv_prc %>% dplyr::filter(id_step %in% c("THENa1")))
  expect_equal(split_prc(steplist_mini_intv_prc) %>% magrittr::extract2("interventions"),
               steplist_mini_intv_prc %>% dplyr::filter(id_step %in% c("THENa2")))
  expect_equal(split_prc(steplist_mini_intv_prc) %>% magrittr::extract2("ifnot_steps"), c("IFa1IFNOTa2THENa3"))
  expect_equal(split_prc(steplist_mini_intv_prc) %>% magrittr::extract2("end_steps"), c("IFa1IFNOTa2THENa3"))
})

test_that("get_cause_combinations works", {
  steplist_mini_and <- readRDS(test_path("fixtures", "steplist_mini_and.rds")) %>% check_steplist()
  causes_mini_and <- steplist_mini_and %>% process_steplist() %>% split_prc() %>% magrittr::extract2("causes")

  cc_mini_and <- data.frame(THENa1 = c(F,T,T), THENa2 = c(T,F,T)) %>% magrittr::set_rownames(paste0("cc",1:3))
  expect_equal(get_cause_combinations(causes_mini_and, steplist_mini_and) %>% magrittr::set_attr("out.attrs", NULL), cc_mini_and)
})

test_that("transform_outc & transform_outc_list work", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds")) %>% remove_na() %>% remove_segment("d4") %>% check_steplist()
  outc_party_test <- data.frame(sce = c("1","1","1","1"), id = c("a4d5","a9d6","a6d5","a2d6"))
  expect_equal(transform_outc(steplist_party_test), outc_party_test)

  outc_party_test <- transform_outc(steplist_party_test)
  outc_compare <- "c('a4d5','a9d6','a6d5','a2d6') %>% magrittr::is_in(final_list_then) %>% magrittr::not() %>% sum() %>% magrittr::equals(0)"
  expect_equal(transform_outc_list(outc_party_test), outc_compare)
})

test_that("transform_if_list works", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  steplist_party_test_prc <- steplist_party_test %>% process_steplist()

  expect_error(transform_if_list(steplist_party_test_prc$if_list[1][[1]]), class = "input_if_list")
  str1 <- "c('a5d1') %>% magrittr::is_in(current_list_then) %>% magrittr::not() %>% sum() %>% magrittr::equals(0)"
  expect_equal(transform_if_list(steplist_party_test_prc$if_list[5][[1]]), str1)
  str2 <- paste0("c('a7d3e2') %>% magrittr::is_in(current_list_then) %>% magrittr::not() %>% sum() %>% magrittr::equals(0) | c('a7d3e1','a3')",
                 " %>% magrittr::is_in(current_list_then) %>% magrittr::not() %>% sum() %>% magrittr::equals(0)")
  expect_equal(transform_if_list(steplist_party_test_prc$if_list[9][[1]]), str2)
  str3 <- paste0("c('a7d3e5') %>% magrittr::is_in(current_list_then) %>% magrittr::not() %>% sum() %>% magrittr::equals(0) | c('a7d3e4') %>% ",
                 "magrittr::is_in(current_list_then) %>% magrittr::not() %>% sum() %>% magrittr::equals(0)")
  expect_equal(transform_if_list(steplist_party_test_prc$if_list[10][[1]]), str3)
  str4 <- paste0("c('a4d1','a5d5') %>% magrittr::is_in(current_list_then) %>% magrittr::not() %>% sum() %>% magrittr::equals(0)",
                 " | c('a4d1','a7d3e5') %>% magrittr::is_in(current_list_then) %>% magrittr::not() %>% sum() %>% magrittr::equals(0)")
  expect_equal(transform_if_list(steplist_party_test_prc$if_list[14][[1]]), str4)
})

test_that("is_fulfilled works", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  steplist_party_test_prc <- steplist_party_test %>% process_steplist()

  if_list1 <- steplist_party_test_prc$if_list[14][[1]]
  current_list1 <- c("a4d1","a5d5")
  current_list2 <- c("a5d5","a7d3e5")
  current_list3 <- c("a7d3e5","a4d1")
  expect_true(is_fulfilled(if_list1, current_list1))
  expect_false(is_fulfilled(if_list1, current_list2))
  expect_true(is_fulfilled(if_list1, current_list3))
})

test_that("is_fulfilled_outc works", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds")) %>% remove_na() %>% remove_segment("d4") %>% check_steplist()
  outc_party_test <- transform_outc(steplist_party_test)

  final_list_then1 <- c("a4d5","a9d6","a6d5","a2d6","abc")
  final_list_then2 <- c("a4d5","a9d6","a6d5")
  final_list_then3 <- c("a5d5","a7d3e5")
  expect_true(is_fulfilled_outc(outc_party_test, final_list_then1))
  expect_false(is_fulfilled_outc(outc_party_test, final_list_then2))
  expect_false(is_fulfilled_outc(outc_party_test, final_list_then3))
})

test_that("next_round_of_steps works", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  steplist_party_test_prc <- steplist_party_test %>% process_steplist()

  current_list_then1 <- c("a5d1","a4d1")
  steps_left1 <- steplist_party_test_prc %>% dplyr::filter(!(.data$then_step %in% current_list_then1)) %>% dplyr::filter(!is.na(if_step))
  expect_equal(next_round_of_steps(steps_left1, current_list_then1), c("IFa5d1IFNOTa7d3e3THENa5d5"))

  current_list_then2 <- c("a5d1","a4d1","a5d5")
  steps_left2 <- steplist_party_test_prc %>% dplyr::filter(!(.data$then_step %in% current_list_then2)) %>% dplyr::filter(!is.na(if_step))
  expect_equal(next_round_of_steps(steps_left2, current_list_then2), c("IF(a4d1+a5d5)or(a4d1+a7d3e5)THENa4d5"))

  current_list_then3 <- c("a5d1","a4d1","a7d3e5")
  steps_left3 <- steplist_party_test_prc %>% dplyr::filter(!(.data$then_step %in% current_list_then3)) %>% dplyr::filter(!is.na(if_step))
  expect_equal(next_round_of_steps(steps_left3, current_list_then3),
               c("IFa5d1IFNOTa7d3e3THENa5d5","IF(a7d3e5)or(a7d3e4)THENa7d3e2","IF(a4d1+a5d5)or(a4d1+a7d3e5)THENa4d5"))
})

test_that("is_sufficient works", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds")) %>% remove_na() %>% remove_segment("d4") %>% check_steplist()
  outc_party_test <- transform_outc(steplist_party_test)
  steplist_party_test_split <- steplist_party_test %>% process_steplist() %>% split_prc()
  cc <- get_cause_combinations(steplist_party_test_split$causes, steplist_party_test)

  #c("THENa5d1","THENa4d1","THENa6d1","THENa3","THENa7d3e4")
  suff1 <- is_sufficient(cc, row = 122, non_start_steps = steplist_party_test_split$non_start_steps, outc_party_test)
  expect_true(suff1$is_suff)
  expect_equal(suff1$final_list, c("THENa5d1","THENa4d1","THENa6d1","THENa3","THENa7d3e4","IFa5d1IFNOTa7d3e3THENa5d5",
                                   "IF(a7d3e5)or(a7d3e4)THENa7d3e2","IFa6d1THENa6d5","IF(a7d3e4)or(a8d2a1)THENa9d6",
                                   "IF(a7d3e2)or(a7d3e1+a3)THENa2d6","IF(a4d1+a5d5)or(a4d1+a7d3e5)THENa4d5"))

  #c("THENa4d1","THENa6d1","THENa7d3e5")
  suff2 <- is_sufficient(cc, row = 51, non_start_steps = steplist_party_test_split$non_start_steps, outc_party_test)
  expect_false(suff2$is_suff)
  expect_equal(suff2$final_list, c("THENa4d1","THENa6d1","THENa7d3e5","IF(a7d3e5)or(a7d3e4)THENa7d3e2","IFa6d1THENa6d5",
                                   "IF(a4d1+a5d5)or(a4d1+a7d3e5)THENa4d5","IF(a7d3e2)or(a7d3e1+a3)THENa2d6"))
})

test_that("minimize_sc works", {
  steplist_rain_test <- readRDS(test_path("fixtures", "steplist_rain_test.rds")) %>% check_steplist()
  causes_rain_test <- steplist_rain_test %>% process_steplist() %>% split_prc() %>% magrittr::extract2("causes")

  cc <- get_cause_combinations(causes_rain_test, steplist_rain_test)

  cc_compare1 <- cc[1:4,]
  expect_equal(minimize_sc(cc), cc_compare1)

  cc2 <- cc[3:15,]
  cc_compare2 <- cc[c(3:5),]
  expect_equal(minimize_sc(cc2), cc_compare2)
})

test_that("get_sc_to_check_for_ifnot & check_ifnot work", {
  sc_test <- data.frame(THENa5d1 = c(F,T,F,T,T,T,T,T,T),
                   THENa4d1 = c(T,T,T,T,T,T,T,T,T),
                   THENa6d1 = c(T,T,T,T,T,T,T,T,T),
                   THENa7d3e3 = c(T,F,T,T,F,T,T,T,T),
                   THENa3 = c(F,F,T,F,T,F,T,T,T),
                   THENa7d3e4 = c(F,T,F,F,T,T,F,F,T),
                   THENa7d3e5 = c(T,F,T,T,F,F,F,T,F),
                   THENa7d3e6 = c(F,F,F,F,F,F,T,F,F)) %>%
    magrittr::set_rownames(c("cc90", "cc103", "cc114", "cc118", "cc122", "cc123", "cc125", "cc126", "cc127"))

  sc_final_steps_test <- list(cc90 = c("THENa4d1", "THENa6d1", "THENa7d3e3", "THENa7d3e5", "IF(a7d3e5)or(a7d3e4)THENa7d3e2", "IFa6d1THENa6d5",
                                       "IF(a4d1+a5d5)or(a4d1+a7d3e5)THENa4d5", "IFa7d3e3THENa8d2a1", "IF(a7d3e2)or(a7d3e1+a3)THENa2d6",
                                       "IF(a7d3e4)or(a8d2a1)THENa9d6"),
                              cc103 = c("THENa5d1", "THENa4d1", "THENa6d1", "THENa7d3e4", "IFa5d1IFNOTa7d3e3THENa5d5",
                                        "IF(a7d3e5)or(a7d3e4)THENa7d3e2", "IFa6d1THENa6d5", "IF(a7d3e4)or(a8d2a1)THENa9d6",
                                        "IF(a7d3e2)or(a7d3e1+a3)THENa2d6", "IF(a4d1+a5d5)or(a4d1+a7d3e5)THENa4d5"),
                              cc114 = c("THENa4d1", "THENa6d1", "THENa7d3e3", "THENa3", "THENa7d3e5", "IF(a7d3e5)or(a7d3e4)THENa7d3e2",
                                        "IFa6d1THENa6d5", "IF(a4d1+a5d5)or(a4d1+a7d3e5)THENa4d5", "IFa7d3e3THENa8d2a1",
                                        "IF(a7d3e2)or(a7d3e1+a3)THENa2d6", "IF(a7d3e4)or(a8d2a1)THENa9d6"),
                              cc118 = c("THENa5d1", "THENa4d1", "THENa6d1", "THENa7d3e3", "THENa7d3e5", "IFa5d1IFNOTa7d3e3THENa5d5",
                                        "IF(a7d3e5)or(a7d3e4)THENa7d3e2", "IFa6d1THENa6d5", "IF(a4d1+a5d5)or(a4d1+a7d3e5)THENa4d5",
                                        "IFa7d3e3THENa8d2a1", "IF(a7d3e2)or(a7d3e1+a3)THENa2d6", "IF(a7d3e4)or(a8d2a1)THENa9d6"),
                              cc122 = c("THENa5d1", "THENa4d1", "THENa6d1", "THENa3", "THENa7d3e4", "IFa5d1IFNOTa7d3e3THENa5d5",
                                        "IF(a7d3e5)or(a7d3e4)THENa7d3e2", "IFa6d1THENa6d5", "IF(a7d3e4)or(a8d2a1)THENa9d6",
                                        "IF(a7d3e2)or(a7d3e1+a3)THENa2d6", "IF(a4d1+a5d5)or(a4d1+a7d3e5)THENa4d5"),
                              cc123 = c("THENa5d1", "THENa4d1", "THENa6d1", "THENa7d3e3", "THENa7d3e4", "IFa5d1IFNOTa7d3e3THENa5d5",
                                        "IF(a7d3e5)or(a7d3e4)THENa7d3e2", "IFa6d1THENa6d5", "IFa7d3e3THENa8d2a1", "IF(a7d3e4)or(a8d2a1)THENa9d6",
                                        "IF(a7d3e2)or(a7d3e1+a3)THENa2d6", "IF(a4d1+a5d5)or(a4d1+a7d3e5)THENa4d5"),
                              cc125 = c("THENa5d1", "THENa4d1", "THENa6d1", "THENa7d3e3", "THENa3", "THENa7d3e6", "IFa5d1IFNOTa7d3e3THENa5d5",
                                        "IFa7d3e6THENa7d3e1", "IFa6d1THENa6d5", "IFa7d3e3THENa8d2a1", "IF(a7d3e2)or(a7d3e1+a3)THENa2d6",
                                        "IF(a4d1+a5d5)or(a4d1+a7d3e5)THENa4d5", "IF(a7d3e4)or(a8d2a1)THENa9d6"),
                              cc126 = c("THENa5d1", "THENa4d1", "THENa6d1", "THENa7d3e3", "THENa3", "THENa7d3e5", "IFa5d1IFNOTa7d3e3THENa5d5",
                                        "IF(a7d3e5)or(a7d3e4)THENa7d3e2", "IFa6d1THENa6d5", "IF(a4d1+a5d5)or(a4d1+a7d3e5)THENa4d5",
                                        "IFa7d3e3THENa8d2a1", "IF(a7d3e2)or(a7d3e1+a3)THENa2d6", "IF(a7d3e4)or(a8d2a1)THENa9d6"),
                              cc127 = c("THENa5d1", "THENa4d1", "THENa6d1", "THENa7d3e3", "THENa3", "THENa7d3e4", "IFa5d1IFNOTa7d3e3THENa5d5",
                                        "IF(a7d3e5)or(a7d3e4)THENa7d3e2", "IFa6d1THENa6d5", "IFa7d3e3THENa8d2a1", "IF(a7d3e4)or(a8d2a1)THENa9d6",
                                        "IF(a7d3e2)or(a7d3e1+a3)THENa2d6", "IF(a4d1+a5d5)or(a4d1+a7d3e5)THENa4d5"))

  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds")) %>% remove_na() %>% remove_segment("d4") %>% check_steplist()
  steplist_party_test_prc <- steplist_party_test %>% process_steplist()
  steplist_party_test_split <- steplist_party_test %>% process_steplist() %>% split_prc()
  outc_party_test <- transform_outc(steplist_party_test)

  compare <- data.frame(THENa5d1 = c(T,T,T,T,T,T,T),
                        THENa4d1 = c(T,T,T,T,T,T,T),
                        THENa6d1 = c(T,T,T,T,T,T,T),
                        THENa7d3e3 = c(F,T,F,T,T,T,T),
                        THENa3 = c(F,F,T,F,T,T,T),
                        THENa7d3e4 = c(T,F,T,T,F,F,T),
                        THENa7d3e5 = c(F,T,F,F,F,T,F),
                        THENa7d3e6 = c(F,F,F,F,T,F,F)) %>%
    magrittr::set_rownames(c("cc103", "cc118", "cc122", "cc123", "cc125", "cc126", "cc127"))

  expect_equal(get_sc_to_check_for_ifnot(sc_test, sc_final_steps_test, steplist_party_test_split$ifnot_steps), compare)

  check1 <- check_ifnot(re_sc = compare, row = 1, sc_final_steps = sc_final_steps_test, prc = steplist_party_test_prc,
                        prc_split = steplist_party_test_split, outc_list = outc_party_test)
  expect_equal(check1$sc_status, "always")
  expect_equal(check1$order, NA)
  expect_false(check1$incon)
  expect_equal(check1$incon_then, NA)

  check2 <- check_ifnot(re_sc = compare, row = 5, sc_final_steps = sc_final_steps_test, prc = steplist_party_test_prc,
                        prc_split = steplist_party_test_split, outc_list = outc_party_test)
  expect_equal(check2$sc_status, "depends")
  order2 <- data.frame(order = c("a5d1->a7d3e3","a7d3e3->a5d1"), suff = c(T,F))
  expect_equal(check2$order, order2)
  expect_false(check2$incon)
  expect_equal(check2$incon_then, NA)
})

test_that("unknown_sc works", {
  sc_in <- data.frame(THENa1 = c(T,F), THENa2 = c(T,T), THENa3 = c(F,T)) %>% magrittr::set_rownames(c("cc2","cc7"))
  sc_out <- data.frame(THENa1 = c(T,F,F), THENa2 = c(T,T,F), THENa3 = c(F,T,F), U1 = c(T,F,F), U2 = c(F,T,F), USC = c(F,F,T)) %>%
    magrittr::set_rownames(c("cc2","cc7","cc0"))

  expect_equal(unknown_sc(sc_in), sc_out)
})
