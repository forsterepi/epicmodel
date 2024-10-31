test_that("mini_and", {
  steplist_mini_and <- readRDS(test_path("fixtures", "steplist_mini_and.rds")) %>% check_steplist()
  expect_no_error(steplist_mini_and %>% create_scc())

  scc_mini_and <- steplist_mini_and %>% create_scc()
  expect_length(scc_mini_and$sc_status, 1)
  expect_equal(scc_mini_and$sc_status %>% unname(), "always")

  expect_no_error(scc_mini_and %>% effect_size())

  expect_equal(sc_contain_steps(scc_mini_and, steps = c("THENa1","THENa2"), output = "table")[[1]] %>% unname(), c(TRUE, TRUE))
  expect_no_error(sc_contain_steps(scc_mini_and))

  expect_no_error(scc_cause_sets(scc_mini_and, output = "all"))
  expect_no_error(scc_cause_sets(scc_mini_and, output = "desc_no_start", unknown = T, depends = F))

  expect_equal(are_sufficient(scc_mini_and, causes = "THENa1"), "never")
  expect_equal(are_sufficient(scc_mini_and, causes = c("THENa1", "THENa2"), type = "status"), "always")
  expect_false(are_sufficient(scc_mini_and, causes = "THENa1", type = "binary"))
  expect_true(are_sufficient(scc_mini_and, causes = c("THENa1", "THENa2"), type = "binary"))

  expect_no_error(show_steps(scc_mini_and))
  df_steps <- data.frame(id_step = c("THENa1","THENa2","IFa1+a2THENa3"), desc_step = c("Start: a1","Start: a2","End: IF a1 and a2 THEN a3"))
  expect_equal(show_steps(scc_mini_and, output = "table"), df_steps)
})

test_that("mini_depends", {
  steplist_mini_depends <- readRDS(test_path("fixtures", "steplist_mini_depends.rds")) %>% check_steplist()
  expect_no_error(steplist_mini_depends %>% create_scc())

  scc_mini_depends <- steplist_mini_depends %>% create_scc()
  expect_length(scc_mini_depends$sc_status, 1)
  expect_equal(scc_mini_depends$sc_status %>% unname(), "depends")

  order_depends <- data.frame(order = c("a1->a2", "a2->a1"), suff = c(TRUE, FALSE))
  expect_equal(scc_mini_depends$sc_order[[1]], order_depends)
  expect_false(scc_mini_depends$sc_implausibilities[[1]])
  expect_equal(scc_mini_depends$sc_implausibilities_detail[[1]], NA)

  expect_no_error(scc_mini_depends %>% effect_size())
  expect_error(scc_mini_depends %>% effect_size(depends = F), class = "input_depends_false_no_always")

  expect_equal(sc_contain_steps(scc_mini_depends, steps = c("THENa1","THENa2"), output = "table")[[1]] %>% unname(), c(TRUE, TRUE))
  expect_no_error(sc_contain_steps(scc_mini_depends, output = "table"))

  expect_no_error(scc_cause_sets(scc_mini_depends, output = "all"))
  expect_error(scc_cause_sets(scc_mini_depends, output = "all", depends = F), class = "input_depends_false_no_always")
  expect_no_error(scc_cause_sets(scc_mini_depends, output = "desc_no_start", unknown = T))

  expect_equal(are_sufficient(scc_mini_depends, causes = "THENa1"), "never")
  expect_equal(are_sufficient(scc_mini_depends, causes = c("THENa1", "THENa2"), type = "status"), "depends")
  expect_false(are_sufficient(scc_mini_depends, causes = "THENa1", type = "binary"))
  expect_true(are_sufficient(scc_mini_depends, causes = c("THENa1", "THENa2"), type = "binary"))

  expect_no_error(show_steps(scc_mini_depends))
})

test_that("mini_doomed", {
  steplist_mini_doomed <- readRDS(test_path("fixtures", "steplist_mini_doomed.rds")) %>% check_steplist()
  expect_no_error(steplist_mini_doomed %>% create_scc())
  expect_equal(steplist_mini_doomed %>% create_scc(), NULL)
})

test_that("mini_implau", {
  steplist_mini_implau <- readRDS(test_path("fixtures", "steplist_mini_implau.rds")) %>% check_steplist()
  expect_no_error(steplist_mini_implau %>% create_scc())

  scc_mini_implau <- steplist_mini_implau %>% create_scc()
  expect_length(scc_mini_implau$sc_status, 1)
  expect_equal(scc_mini_implau$sc_status %>% unname(), "depends (potential order implausibilities)")

  expect_true(scc_mini_implau$sc_implausibilities[[1]])
  expect_equal(scc_mini_implau$sc_implausibilities_detail[[1]], "a5")

  expect_no_error(scc_mini_implau %>% effect_size())
  expect_error(scc_mini_implau %>% effect_size(depends = F), class = "input_depends_false_no_always")

  expect_equal(sc_contain_steps(scc_mini_implau, steps = c("THENa1","THENa2","THENa3"), output = "table")[[1]] %>% unname(), c(TRUE, TRUE, TRUE))
  expect_no_error(sc_contain_steps(scc_mini_implau, output = "table"))

  expect_no_error(scc_cause_sets(scc_mini_implau, output = "all"))
  expect_error(scc_cause_sets(scc_mini_implau, output = "all", depends = F), class = "input_depends_false_no_always")
  expect_no_error(scc_cause_sets(scc_mini_implau, output = "desc_no_start", unknown = T))

  expect_equal(are_sufficient(scc_mini_implau, causes = "THENa1"), "never")
  expect_equal(are_sufficient(scc_mini_implau, causes = c("THENa1", "THENa2", "THENa3"), type = "status"), "depends")
  expect_false(are_sufficient(scc_mini_implau, causes = "THENa1", type = "binary"))
  expect_true(are_sufficient(scc_mini_implau, causes = c("THENa1", "THENa2","THENa3"), type = "binary"))

  expect_no_error(show_steps(scc_mini_implau))
})

test_that("mini_intv", {
  steplist_mini_intv <- readRDS(test_path("fixtures", "steplist_mini_intv.rds")) %>% check_steplist()
  expect_no_error(steplist_mini_intv %>% create_scc())

  scc_mini_intv <- steplist_mini_intv %>% create_scc()
  expect_length(scc_mini_intv$sc_status, 1)
  expect_equal(scc_mini_intv$sc_status %>% unname(), "always")

  expect_no_error(scc_mini_intv %>% effect_size())

  expect_equal(sc_contain_steps(scc_mini_intv, steps = c("THENa1","THENa2"), output = "table")[[1]] %>% unname(), c(TRUE, FALSE))
  expect_no_error(sc_contain_steps(scc_mini_intv))

  expect_no_error(scc_cause_sets(scc_mini_intv, output = "all"))
  expect_no_error(scc_cause_sets(scc_mini_intv, output = "desc_no_start", unknown = T, depends = F))

  expect_equal(are_sufficient(scc_mini_intv, causes = "THENa1"), "always")
  expect_error(are_sufficient(scc_mini_intv, causes = "THENa2"), class = "invalid_causes")
  expect_true(are_sufficient(scc_mini_intv, causes = "THENa1", type = "binary"))

  expect_no_error(show_steps(scc_mini_intv))
})

test_that("mini_or", {
  steplist_mini_or <- readRDS(test_path("fixtures", "steplist_mini_or.rds")) %>% check_steplist()
  expect_no_error(steplist_mini_or %>% create_scc())

  scc_mini_or <- steplist_mini_or %>% create_scc()
  expect_length(scc_mini_or$sc_status, 2)
  expect_equal(scc_mini_or$sc_status %>% unname(), c("always","always"))

  expect_no_error(scc_mini_or %>% effect_size())

  expect_equal(sc_contain_steps(scc_mini_or, steps = "THENa1", output = "table") %>% unlist() %>% sum(), 1)
  expect_equal(sc_contain_steps(scc_mini_or, steps = "THENa2", output = "table") %>% unlist() %>% sum(), 1)
  expect_equal(sc_contain_steps(scc_mini_or, steps = c("THENa1","THENa2"), output = "table") %>% unlist() %>% sum(), 2)
  expect_no_error(sc_contain_steps(scc_mini_or))

  expect_no_error(scc_cause_sets(scc_mini_or, output = "all"))
  expect_no_error(scc_cause_sets(scc_mini_or, output = "desc_no_start", unknown = T, depends = F))

  expect_equal(are_sufficient(scc_mini_or, causes = "THENa1"), "always")
  expect_equal(are_sufficient(scc_mini_or, causes = "THENa2"), "always")
  expect_equal(are_sufficient(scc_mini_or, causes = c("THENa1", "THENa2"), type = "status"), "always")
  expect_true(are_sufficient(scc_mini_or, causes = "THENa1", type = "binary"))
  expect_true(are_sufficient(scc_mini_or, causes = "THENa2", type = "binary"))
  expect_true(are_sufficient(scc_mini_or, causes = c("THENa1", "THENa2"), type = "binary"))

  expect_no_error(show_steps(scc_mini_or))
})

test_that("party_test", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds")) %>% remove_na() %>% remove_segment("d4") %>% check_steplist()
  expect_no_error(steplist_party_test %>% create_scc())

  scc_party_test <- steplist_party_test %>% create_scc()
  expect_length(scc_party_test$sc_status, 3)
  expect_equal(scc_party_test$sc_status %>% unname(), c("always","always","depends"))

  expect_no_error(scc_party_test %>% effect_size())

  expect_equal(sc_contain_steps(scc_party_test, steps = "IFa5d1IFNOTa7d3e3THENa5d5", output = "table") %>% unlist() %>% unname(), c(F,T,T))
  expect_no_error(sc_contain_steps(scc_party_test))

  expect_no_error(scc_cause_sets(scc_party_test, output = "all"))
  expect_no_error(scc_cause_sets(scc_party_test, output = "all", unknown = T, depends = F))
  expect_no_error(scc_cause_sets(scc_party_test, output = "all", unknown = F, depends = F))
  expect_no_error(scc_cause_sets(scc_party_test, output = "all", unknown = T, depends = T))
  expect_no_error(scc_cause_sets(scc_party_test, output = "all", unknown = F, depends = T))
  expect_no_error(scc_cause_sets(scc_party_test, output = "desc_no_start", unknown = T, depends = F))
  expect_no_error(scc_cause_sets(scc_party_test, output = "desc_no_start", unknown = F, depends = F))
  expect_no_error(scc_cause_sets(scc_party_test, output = "desc_no_start", unknown = F, depends = T))
  expect_no_error(scc_cause_sets(scc_party_test, output = "desc_no_start", unknown = T, depends = T))

  expect_equal(are_sufficient(scc_party_test, causes = "THENa5d1"), "never")
  expect_equal(are_sufficient(scc_party_test, causes = c("THENa4d1","THENa6d1","THENa7d3e3","THENa7d3e5")), "always")
  expect_equal(are_sufficient(scc_party_test, causes = c("THENa5d1","THENa4d1","THENa6d1","THENa7d3e3","THENa3","THENa7d3e6"),
                              type = "status"), "depends")
  expect_equal(are_sufficient(scc_party_test, causes = c("THENa5d1","THENa4d1","THENa6d1","THENa7d3e3","THENa3","THENa7d3e6","THENa7d3e5"),
                              type = "status"), "always")

  expect_false(are_sufficient(scc_party_test, causes = "THENa5d1", type = "binary"))
  expect_true(are_sufficient(scc_party_test, causes = c("THENa4d1","THENa6d1","THENa7d3e3","THENa7d3e5"), type = "binary"))
  expect_true(are_sufficient(scc_party_test, causes = c("THENa5d1","THENa4d1","THENa6d1","THENa7d3e3","THENa3","THENa7d3e6"), type = "binary"))
  expect_true(are_sufficient(scc_party_test, causes = c("THENa5d1","THENa4d1","THENa6d1","THENa7d3e3","THENa3","THENa7d3e6","THENa7d3e5"),
                              type = "binary"))

  expect_no_error(show_steps(scc_party_test))
})

test_that("rain_test", {
  steplist_rain_test <- readRDS(test_path("fixtures", "steplist_rain_test.rds")) %>% check_steplist()
  expect_no_error(steplist_rain_test %>% create_scc())

  scc_rain_test <- steplist_rain_test %>% create_scc()
  expect_length(scc_rain_test$sc_status, 2)
  expect_equal(scc_rain_test$sc_status %>% unname(), c("always","always"))

  expect_no_error(scc_rain_test %>% effect_size())

  expect_equal(sc_contain_steps(scc_rain_test, steps = "IFd5a6+a5IFNOTd4e1THENd3e3", output = "table") %>% unlist() %>% unname(), c(F,T))
  expect_no_error(sc_contain_steps(scc_rain_test))

  expect_no_error(scc_cause_sets(scc_rain_test, output = "all"))
  expect_no_error(scc_cause_sets(scc_rain_test, output = "all", unknown = T, depends = F))
  expect_no_error(scc_cause_sets(scc_rain_test, output = "all", unknown = F, depends = F))
  expect_no_error(scc_cause_sets(scc_rain_test, output = "all", unknown = T, depends = T))
  expect_no_error(scc_cause_sets(scc_rain_test, output = "all", unknown = F, depends = T))
  expect_no_error(scc_cause_sets(scc_rain_test, output = "desc_no_start", unknown = T, depends = F))
  expect_no_error(scc_cause_sets(scc_rain_test, output = "desc_no_start", unknown = F, depends = F))
  expect_no_error(scc_cause_sets(scc_rain_test, output = "desc_no_start", unknown = F, depends = T))
  expect_no_error(scc_cause_sets(scc_rain_test, output = "desc_no_start", unknown = T, depends = T))

  expect_equal(are_sufficient(scc_rain_test, causes = "THENa1"), "never")
  expect_equal(are_sufficient(scc_rain_test, causes = c("THENa1","THENd2a3")), "always")
  expect_false(are_sufficient(scc_rain_test, causes = "THENa1", type = "binary"))
  expect_true(are_sufficient(scc_rain_test, causes = c("THENa1","THENd2a3"), type = "binary"))

  expect_no_error(show_steps(scc_rain_test))
})

test_that("all options of scc_cause_sets work", {
  steplist_mini_and <- readRDS(test_path("fixtures", "steplist_mini_and.rds")) %>% check_steplist()
  scc_mini_and <- steplist_mini_and %>% create_scc()

  expect_equal(scc_cause_sets(scc_mini_and, output = "id")[[1]], c("THENa1","THENa2"))
  expect_equal(scc_cause_sets(scc_mini_and, output = "id", unknown = T, depends = F)[[1]], c("THENa1","THENa2","U1"))
  expect_equal(scc_cause_sets(scc_mini_and, output = "id", unknown = T, depends = F)[[2]], c("USC"))
  expect_equal(scc_cause_sets(scc_mini_and, output = "id", unknown = T, depends = T)[[1]], c("THENa1","THENa2","U1"))
  expect_equal(scc_cause_sets(scc_mini_and, output = "id", unknown = T, depends = T)[[2]], c("USC"))

  expect_equal(scc_cause_sets(scc_mini_and, output = "desc")[[1]], c("Start: a1","Start: a2"))
  expect_equal(scc_cause_sets(scc_mini_and, output = "desc", unknown = T, depends = F)[[1]], c("Start: a1","Start: a2","U1"))
  expect_equal(scc_cause_sets(scc_mini_and, output = "desc", unknown = T, depends = F)[[2]], c("USC"))
  expect_equal(scc_cause_sets(scc_mini_and, output = "desc", unknown = T, depends = T)[[1]], c("Start: a1","Start: a2","U1"))
  expect_equal(scc_cause_sets(scc_mini_and, output = "desc", unknown = T, depends = T)[[2]], c("USC"))

  expect_equal(scc_cause_sets(scc_mini_and, output = "desc_no_start")[[1]], c("a1","a2"))
  expect_equal(scc_cause_sets(scc_mini_and, output = "desc_no_start", unknown = T, depends = F)[[1]], c("a1","a2","U1"))
  expect_equal(scc_cause_sets(scc_mini_and, output = "desc_no_start", unknown = T, depends = F)[[2]], c("USC"))
  expect_equal(scc_cause_sets(scc_mini_and, output = "desc_no_start", unknown = T, depends = T)[[1]], c("a1","a2","U1"))
  expect_equal(scc_cause_sets(scc_mini_and, output = "desc_no_start", unknown = T, depends = T)[[2]], c("USC"))

  expect_equal(scc_cause_sets(scc_mini_and, output = "all")[["id"]][[1]], c("THENa1","THENa2"))
  expect_equal(scc_cause_sets(scc_mini_and, output = "all", unknown = T, depends = F)[["id"]][[1]], c("THENa1","THENa2","U1"))
  expect_equal(scc_cause_sets(scc_mini_and, output = "all", unknown = T, depends = F)[["id"]][[2]], c("USC"))
  expect_equal(scc_cause_sets(scc_mini_and, output = "all", unknown = T, depends = T)[["id"]][[1]], c("THENa1","THENa2","U1"))
  expect_equal(scc_cause_sets(scc_mini_and, output = "all", unknown = T, depends = T)[["id"]][[2]], c("USC"))

  expect_equal(scc_cause_sets(scc_mini_and, output = "all")[["desc"]][[1]], c("Start: a1","Start: a2"))
  expect_equal(scc_cause_sets(scc_mini_and, output = "all", unknown = T, depends = F)[["desc"]][[1]], c("Start: a1","Start: a2","U1"))
  expect_equal(scc_cause_sets(scc_mini_and, output = "all", unknown = T, depends = F)[["desc"]][[2]], c("USC"))
  expect_equal(scc_cause_sets(scc_mini_and, output = "all", unknown = T, depends = T)[["desc"]][[1]], c("Start: a1","Start: a2","U1"))
  expect_equal(scc_cause_sets(scc_mini_and, output = "all", unknown = T, depends = T)[["desc"]][[2]], c("USC"))

  expect_equal(scc_cause_sets(scc_mini_and, output = "all")[["desc_no_start"]][[1]], c("a1","a2"))
  expect_equal(scc_cause_sets(scc_mini_and, output = "all", unknown = T, depends = F)[["desc_no_start"]][[1]], c("a1","a2","U1"))
  expect_equal(scc_cause_sets(scc_mini_and, output = "all", unknown = T, depends = F)[["desc_no_start"]][[2]], c("USC"))
  expect_equal(scc_cause_sets(scc_mini_and, output = "all", unknown = T, depends = T)[["desc_no_start"]][[1]], c("a1","a2","U1"))
  expect_equal(scc_cause_sets(scc_mini_and, output = "all", unknown = T, depends = T)[["desc_no_start"]][[2]], c("USC"))
})

test_that("scc_cause_sets works", {
  expect_error(scc_cause_sets(NA), class = "no_scc")
  expect_error(scc_cause_sets(NULL), class = "no_scc")
})

test_that("scc_cause_sets performs correctly for the party example", {
  scc_party <-  readRDS(test_path("fixtures", "scc_party.rds"))
  id_expect <- list(cc90 = c("THENa4d1","THENa6d1","THENa7d3e3","THENa7d3e5"),
                    cc103 = c("THENa5d1","THENa4d1","THENa6d1","THENa7d3e4"),
                    cc125 = c("THENa5d1","THENa4d1","THENa6d1","THENa7d3e3","THENa3","THENa7d3e6"))
  desc_expect <- list(cc90 = c("Start: Emma is invited","Start: Laura is invited",
                               "Start: Birthday party takes place on a weekday","Start: Birthday party takes place at a karaoke bar"),
                      cc103 = c("Start: Ana is invited","Start: Emma is invited","Start: Laura is invited",
                                "Start: Birthday party takes place at a restaurant"),
                      cc125 = c("Start: Ana is invited","Start: Emma is invited","Start: Laura is invited",
                                "Start: Birthday party takes place on a weekday","Start: No rain","Start: Birthday party takes place at the beach"))
  desc_no_start_expect <- list(cc90 = c("Emma is invited","Laura is invited",
                                        "Birthday party takes place on a weekday","Birthday party takes place at a karaoke bar"),
                               cc103 = c("Ana is invited","Emma is invited","Laura is invited",
                                         "Birthday party takes place at a restaurant"),
                               cc125 = c("Ana is invited","Emma is invited","Laura is invited",
                                         "Birthday party takes place on a weekday","No rain","Birthday party takes place at the beach"))
  all_expect <- list(id = id_expect, desc = desc_expect, desc_no_start = desc_no_start_expect)

  expect_equal(scc_cause_sets(scc_party,"id"), id_expect)
  expect_equal(scc_cause_sets(scc_party,"desc"), desc_expect)
  expect_equal(scc_cause_sets(scc_party,"desc_no_start"), desc_no_start_expect)
  expect_equal(scc_cause_sets(scc_party,"all"), all_expect)
})
