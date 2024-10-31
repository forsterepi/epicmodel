test_that("check_what_ids throws correct errors", {
  expect_error(check_what_ids(new_steplist()), class = "no_epicmodel_steplist")

  steplist_zero <- readRDS(test_path("fixtures", "steplist_zero.rds"))
  expect_no_error(check_what_ids(steplist_zero))

  steplist_na <- readRDS(test_path("fixtures", "steplist_na.rds"))
  expect_no_error(check_what_ids(steplist_na))

  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  x <- steplist_party_test
  x$what$id_what[1] <- "e1"
  expect_error(check_what_ids(x), class = "wrong_id_format")

  x <- steplist_party_test
  x$what$id_what[2] <- "a1"
  expect_error(check_what_ids(x), class = "duplicated_ids")
})

test_that("check_does_ids throws correct errors", {
  expect_error(check_does_ids(new_steplist()), class = "no_epicmodel_steplist")

  steplist_zero <- readRDS(test_path("fixtures", "steplist_zero.rds"))
  expect_no_error(check_does_ids(steplist_zero))

  steplist_na <- readRDS(test_path("fixtures", "steplist_na.rds"))
  expect_no_error(check_does_ids(steplist_na))

  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  x <- steplist_party_test
  x$does$id_does[1] <- "e1"
  expect_error(check_does_ids(x), class = "wrong_id_format")

  x <- steplist_party_test
  x$does$id_does[2] <- "d1"
  expect_error(check_does_ids(x), class = "duplicated_ids")
})

test_that("check_where_ids throws correct errors", {
  expect_error(check_where_ids(new_steplist()), class = "no_epicmodel_steplist")

  steplist_zero <- readRDS(test_path("fixtures", "steplist_zero.rds"))
  expect_no_error(check_where_ids(steplist_zero))

  steplist_na <- readRDS(test_path("fixtures", "steplist_na.rds"))
  expect_no_error(check_where_ids(steplist_na))

  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  x <- steplist_party_test
  x$where$id_where[1] <- "a1"
  expect_error(check_where_ids(x), class = "wrong_id_format")

  x <- steplist_party_test
  x$where$id_where[2] <- "e1"
  expect_error(check_where_ids(x), class = "duplicated_ids")
})

test_that("check_module_ids throws correct errors", {
  expect_error(check_module_ids(new_steplist()), class = "no_epicmodel_steplist")

  steplist_zero <- readRDS(test_path("fixtures", "steplist_zero.rds"))
  expect_no_error(check_module_ids(steplist_zero))

  steplist_na <- readRDS(test_path("fixtures", "steplist_na.rds"))
  expect_no_error(check_module_ids(steplist_na))

  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  x <- steplist_party_test
  x$module$id_module[1] <- "e1"
  expect_error(check_module_ids(x), class = "wrong_id_format")

  x <- steplist_party_test
  x$module$id_module[2] <- "m1"
  expect_error(check_module_ids(x), class = "duplicated_ids")
})

test_that("check_icc_ids throws correct errors", {
  expect_error(check_icc_ids(new_steplist()), class = "no_epicmodel_steplist")

  steplist_zero <- readRDS(test_path("fixtures", "steplist_zero.rds"))
  expect_no_error(check_icc_ids(steplist_zero))

  steplist_na <- readRDS(test_path("fixtures", "steplist_na.rds"))
  expect_no_error(check_icc_ids(steplist_na))

  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  x <- steplist_party_test
  x$icc$id_icc[1] <- "e1"
  expect_error(check_icc_ids(x), class = "wrong_id_format")

  x <- steplist_party_test
  x$icc$id_icc[3] <- "i1"
  expect_error(check_icc_ids(x), class = "duplicated_ids")
})

test_that("check_*_ids functions throw no error for steplist_party_test", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_no_error(steplist_party_test %>% check_what_ids())
  expect_no_error(steplist_party_test %>% check_does_ids())
  expect_no_error(steplist_party_test %>% check_where_ids())
  expect_no_error(steplist_party_test %>% check_module_ids())
  expect_no_error(steplist_party_test %>% check_icc_ids())
})

test_that("check_what_keys throws correct warnings", {
  expect_error(check_what_keys(new_steplist()), class = "no_epicmodel_steplist")

  steplist_zero <- readRDS(test_path("fixtures", "steplist_zero.rds"))
  expect_no_error(check_what_keys(steplist_zero))

  steplist_na <- readRDS(test_path("fixtures", "steplist_na.rds"))
  expect_no_error(check_what_keys(steplist_na))

  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  x <- steplist_party_test
  x$what$key_what[2] <- "cake"
  expect_warning(check_what_keys(x), class = "duplicated_keys")
})

test_that("check_does_keys throws correct warnings", {
  expect_error(check_does_keys(new_steplist()), class = "no_epicmodel_steplist")

  steplist_zero <- readRDS(test_path("fixtures", "steplist_zero.rds"))
  expect_no_error(check_does_keys(steplist_zero))

  steplist_na <- readRDS(test_path("fixtures", "steplist_na.rds"))
  expect_no_error(check_does_keys(steplist_na))

  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  x <- steplist_party_test
  x$does$key_does[2] <- "invite"
  expect_warning(check_does_keys(x), class = "duplicated_keys")
})

test_that("check_where_keys throws correct warnings", {
  expect_error(check_where_keys(new_steplist()), class = "no_epicmodel_steplist")

  steplist_zero <- readRDS(test_path("fixtures", "steplist_zero.rds"))
  expect_no_error(check_where_keys(steplist_zero))

  steplist_na <- readRDS(test_path("fixtures", "steplist_na.rds"))
  expect_no_error(check_where_keys(steplist_na))

  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  x <- steplist_party_test
  x$where$key_where[2] <- "outside"
  expect_warning(check_where_keys(x), class = "duplicated_keys")
})

test_that("check_module_keys throws correct warnings", {
  expect_error(check_module_keys(new_steplist()), class = "no_epicmodel_steplist")

  steplist_zero <- readRDS(test_path("fixtures", "steplist_zero.rds"))
  expect_no_error(check_module_keys(steplist_zero))

  steplist_na <- readRDS(test_path("fixtures", "steplist_na.rds"))
  expect_no_error(check_module_keys(steplist_na))

  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  x <- steplist_party_test
  x$module$key_module[2] <- "guests"
  expect_warning(check_module_keys(x), class = "duplicated_keys")
})

test_that("check_*_keys functions throw no error for steplist_party_test", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_no_error(steplist_party_test %>% check_what_keys())
  expect_no_error(steplist_party_test %>% check_does_keys())
  expect_no_error(steplist_party_test %>% check_where_keys())
  expect_no_error(steplist_party_test %>% check_module_keys())
})

test_that("are_modules_used works correctly", {
  expect_error(are_modules_used(new_steplist()), class = "no_epicmodel_steplist")

  steplist_zero <- readRDS(test_path("fixtures", "steplist_zero.rds"))
  expect_no_error(are_modules_used(steplist_zero))
  expect_false(are_modules_used(steplist_zero))

  steplist_na <- readRDS(test_path("fixtures", "steplist_na.rds"))
  expect_no_error(are_modules_used(steplist_na))
  expect_false(are_modules_used(steplist_na))

  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_true(are_modules_used(steplist_party_test))
  expect_false(are_modules_used(empty_steplist()))

  x <- empty_steplist()
  x$module %<>% dplyr::filter(id_module != "m0")
  expect_false(are_modules_used(x))
})

test_that("check_modules_in_steps works", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_error(check_modules_in_steps(new_steplist()), class = "no_epicmodel_steplist")
  expect_no_error(check_modules_in_steps(steplist_party_test))
  expect_no_error(check_modules_in_steps(empty_steplist()))

  steplist_zero <- readRDS(test_path("fixtures", "steplist_zero.rds"))
  expect_no_error(check_modules_in_steps(steplist_zero))

  steplist_na <- readRDS(test_path("fixtures", "steplist_na.rds"))
  expect_no_error(check_modules_in_steps(steplist_na))

  x <- steplist_party_test
  x$module %<>% rbind(.,c("m4","test","test"))
  expect_warning(check_modules_in_steps(x), class = "unused_modules")

  x <- steplist_party_test
  x$step %<>% rbind(.,c("test","test","0","m5","",""))
  expect_error(check_modules_in_steps(x), class = "unspecified_modules")

  x <- steplist_party_test
  x$step$module_step[1:3] <- ""
  expect_error(check_modules_in_steps(x), class = "empty_modules_in_step")
})

test_that("check_steps_in_icc works", {
  expect_error(check_steps_in_icc(new_steplist()), class = "no_epicmodel_steplist")

  steplist_zero <- readRDS(test_path("fixtures", "steplist_zero.rds"))
  expect_no_error(check_steps_in_icc(steplist_zero))

  steplist_na <- readRDS(test_path("fixtures", "steplist_na.rds"))
  expect_no_error(check_steps_in_icc(steplist_na))

  x <- empty_steplist()
  x$icc %<>% dplyr::filter(id_icc != "i0")
  expect_no_error(check_steps_in_icc(x))

  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_error(check_steps_in_icc(steplist_party_test), class = "unspecified_steps_in_icc")

  x <- steplist_party_test
  x$icc %<>% dplyr::filter(id1 != "NA")
  expect_no_error(check_steps_in_icc(x))
})

test_that("check_what_in_steps works", {
  expect_error(check_what_in_steps(new_steplist()), class = "no_epicmodel_steplist")

  steplist_zero <- readRDS(test_path("fixtures", "steplist_zero.rds"))
  expect_no_error(check_what_in_steps(steplist_zero))

  steplist_na <- readRDS(test_path("fixtures", "steplist_na.rds"))
  expect_no_error(check_what_in_steps(steplist_na))

  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_no_error(check_what_in_steps(steplist_party_test))

  x <- steplist_party_test
  x$what %<>% dplyr::filter(id_what != "a1")
  expect_error(check_what_in_steps(x), class = "unspecified_segments")

  x <- steplist_party_test
  x$step %<>% dplyr::filter(id_step != "IF(a7d3e2)or(a7d3e1+a3)THENa2d6")
  expect_warning(check_what_in_steps(x), class = "unused_segments")
})

test_that("check_does_in_steps works", {
  expect_error(check_does_in_steps(new_steplist()), class = "no_epicmodel_steplist")

  steplist_zero <- readRDS(test_path("fixtures", "steplist_zero.rds"))
  expect_no_error(check_does_in_steps(steplist_zero))

  steplist_na <- readRDS(test_path("fixtures", "steplist_na.rds"))
  expect_no_error(check_does_in_steps(steplist_na))

  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_warning(check_does_in_steps(steplist_party_test), class = "unused_segments")

  x <- steplist_party_test
  x$does %<>% dplyr::filter(id_does != "d4")
  expect_no_error(check_does_in_steps(x))

  x <- steplist_party_test
  x$does %<>% dplyr::filter(id_does != "d1")
  expect_error(check_does_in_steps(x), class = "unspecified_segments")
})

test_that("check_where_in_steps works", {
  expect_error(check_where_in_steps(new_steplist()), class = "no_epicmodel_steplist")

  steplist_zero <- readRDS(test_path("fixtures", "steplist_zero.rds"))
  expect_no_error(check_where_in_steps(steplist_zero))

  steplist_na <- readRDS(test_path("fixtures", "steplist_na.rds"))
  expect_no_error(check_where_in_steps(steplist_na))

  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_no_error(check_where_in_steps(steplist_party_test))

  x <- steplist_party_test
  x$step %<>% dplyr::filter(id_step != "IFa7d3e6THENa7d3e1")
  expect_warning(check_where_in_steps(x), class = "unused_segments")

  x <- steplist_party_test
  x$where %<>% dplyr::filter(id_where != "e1")
  expect_error(check_where_in_steps(x), class = "unspecified_segments")
})

test_that("check_refs works", {
  expect_error(check_refs(new_steplist()), class = "no_epicmodel_steplist")

  steplist_zero <- readRDS(test_path("fixtures", "steplist_zero.rds"))
  expect_no_error(check_refs(steplist_zero))

  steplist_na <- readRDS(test_path("fixtures", "steplist_na.rds"))
  expect_warning(check_refs(steplist_na), class = "no_refs")

  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_warning(check_refs(steplist_party_test), class = "no_refs")

  x <- steplist_party_test
  x$step$ref_step <- paste0("ref",c(1:16))
  expect_no_error(check_refs(x))
})

test_that("check_start_end_steps works", {
  expect_error(check_start_end_steps(new_steplist()), class = "no_epicmodel_steplist")

  steplist_zero <- readRDS(test_path("fixtures", "steplist_zero.rds"))
  expect_error(check_start_end_steps(steplist_zero), class = "no_cc")

  steplist_na <- readRDS(test_path("fixtures", "steplist_na.rds"))
  expect_no_error(check_start_end_steps(steplist_na))

  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_no_error(check_start_end_steps(steplist_party_test))

  x <- steplist_party_test
  x$step$end_step[c(1,2)] <- "1"
  expect_error(check_start_end_steps(x), class = "start_end_error")

  x <- steplist_party_test
  x$step$end_step[c(1)] <- "1"
  expect_error(check_start_end_steps(x), class = "start_end_error")

  expect_error(empty_steplist() %>% check_start_end_steps(), class = "no_cc")
})

test_that("check_then_use works", {
  expect_error(check_then_use(new_steplist()), class = "no_epicmodel_steplist")

  steplist_zero <- readRDS(test_path("fixtures", "steplist_zero.rds"))
  expect_no_error(check_then_use(steplist_zero))

  steplist_na <- readRDS(test_path("fixtures", "steplist_na.rds"))
  expect_no_error(check_then_use(steplist_na))

  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_no_error(check_then_use(steplist_party_test))

  x <- steplist_party_test
  x$step$id_step[1] <- "THENa4d1"
  expect_error(check_then_use(x), class = "unavailable_if_ifnot")

  x <- steplist_party_test
  x$then$id_then[1] <- "test"
  expect_error(check_then_use(x), class = "unavailable_then")

  x <- steplist_party_test
  x$step$id_step[1] <- "THENa4d1"
  x$step %<>% dplyr::filter(id_step != "IFa5d1IFNOTa7d3e3THENa5d5")
  x$step %<>% dplyr::filter(id_step != "IF(a4d1+a5d5)or(a4d1+a7d3e5)THENa4d5")
  expect_warning(check_then_use(x), class = "then_duplicates")

  x$step$end_step[1] <- "1"
  expect_error(check_then_use(x), class = "dupli_then_with_varying_end_step")
})

test_that("check_then_if_ifnot works", {
  steplist_mini_intv <- readRDS(test_path("fixtures", "steplist_mini_intv.rds"))

  stp_1 <- steplist_mini_intv
  stp_1$step$id_step[3] <- "IFa1IFNOTa1THENa3"
  expect_error(check_then_if_ifnot(stp_1), class = "if_ifnot_equality")

  stp_2 <- steplist_mini_intv
  stp_2$step$id_step[3] <- "IFa1IFNOT(a2)or(a1+a3)THENa3"
  expect_error(check_then_if_ifnot(stp_2), class = "then_in_ifnot")

  stp_3 <- steplist_mini_intv
  stp_3$step$id_step[3] <- "IF(a1+a3)or(a2)IFNOTa2THENa3"
  expect_error(check_then_if_ifnot(stp_3), class = "then_in_if")
})

test_that("check_outc works", {
  expect_error(check_outc(new_steplist()), class = "no_epicmodel_steplist")
  expect_error(empty_steplist() %>% check_outc(), class = "outc_empty")

  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  x <- steplist_party_test
  x$step %<>% dplyr::filter(id_step != "IF(a7d3e2)or(a7d3e1+a3)THENa2d6")
  expect_error(check_outc(x), class = "unspecified_steps_in_outc")

  x <- steplist_party_test
  x$outc$id_outc[1] <- "a4d5+a9d6+a6d5"
  expect_warning(check_outc(x), class = "unused_steps_in_outc")

  steplist_zero <- readRDS(test_path("fixtures", "steplist_zero.rds"))
  expect_error(check_outc(steplist_zero), class = "outc_empty")

  steplist_na <- readRDS(test_path("fixtures", "steplist_na.rds"))
  expect_error(check_outc(steplist_na), class = "outc_empty")

  expect_no_warning(check_outc(steplist_party_test), class = "outc_subsets")

  x <- steplist_party
  x$outc %<>% rbind(.,c("a4d5+a2d6","test"))
  expect_warning(check_outc(x), class = "outc_subsets")

  x <- steplist_party
  x$outc$id_outc[1] <- "a4d5+a9d6+a6d5"
  x$outc %<>% rbind(.,c("a4d5+a2d6","test"))
  expect_no_warning(check_outc(x), class = "outc_subsets")
})

test_that("check_steplist works", {
  steplist_mini_and <- readRDS(test_path("fixtures", "steplist_mini_and.rds"))
  steplist_mini_and_checked <- steplist_mini_and %>% unclass(.)
  steplist_mini_and_checked %<>% structure(., class = "epicmodel_steplist_checked")
  expect_no_error(check_steplist(steplist_mini_and))
  expect_equal(check_steplist(steplist_mini_and), steplist_mini_and_checked)

  steplist_mini_depends <- readRDS(test_path("fixtures", "steplist_mini_depends.rds"))
  steplist_mini_depends_checked <- steplist_mini_depends %>% unclass(.)
  steplist_mini_depends_checked %<>% structure(., class = "epicmodel_steplist_checked")
  expect_no_error(check_steplist(steplist_mini_depends))
  expect_equal(check_steplist(steplist_mini_depends), steplist_mini_depends_checked)

  steplist_mini_doomed <- readRDS(test_path("fixtures", "steplist_mini_doomed.rds"))
  steplist_mini_doomed_checked <- steplist_mini_doomed %>% unclass(.)
  steplist_mini_doomed_checked %<>% structure(., class = "epicmodel_steplist_checked")
  expect_no_error(check_steplist(steplist_mini_doomed))
  expect_equal(check_steplist(steplist_mini_doomed), steplist_mini_doomed_checked)

  steplist_mini_implau <- readRDS(test_path("fixtures", "steplist_mini_implau.rds"))
  steplist_mini_implau_checked <- steplist_mini_implau %>% unclass(.)
  steplist_mini_implau_checked %<>% structure(., class = "epicmodel_steplist_checked")
  expect_no_error(check_steplist(steplist_mini_implau))
  expect_equal(check_steplist(steplist_mini_implau), steplist_mini_implau_checked)

  steplist_mini_intv <- readRDS(test_path("fixtures", "steplist_mini_intv.rds"))
  steplist_mini_intv_checked <- steplist_mini_intv %>% unclass(.)
  steplist_mini_intv_checked %<>% structure(., class = "epicmodel_steplist_checked")
  expect_no_error(check_steplist(steplist_mini_intv))
  expect_equal(check_steplist(steplist_mini_intv), steplist_mini_intv_checked)

  steplist_mini_or <- readRDS(test_path("fixtures", "steplist_mini_or.rds"))
  steplist_mini_or_checked <- steplist_mini_or %>% unclass(.)
  steplist_mini_or_checked %<>% structure(., class = "epicmodel_steplist_checked")
  expect_no_error(check_steplist(steplist_mini_or))
  expect_equal(check_steplist(steplist_mini_or), steplist_mini_or_checked)

  expect_error(empty_steplist() %>% check_steplist())
})
