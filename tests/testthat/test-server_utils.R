source(system.file("shiny","steplist_creator","R","server_utils.R", package = "epicmodel"), local = TRUE)

test_that("fun_get_id works", {
  expect_equal(fun_get_id(c("a1","a2"),"what"), "a3")
  expect_equal(fun_get_id(c("i1","i2"),"what"), "i3")
  expect_equal(fun_get_id(c("e1","e2"),"what"), "e3")
  expect_equal(fun_get_id(c("d1","d2"),"what"), "d3")
  expect_equal(fun_get_id(c("m1","m2"),"what"), "m3")
})

test_that("fun_get_id works for empty data.frames", {
  # create character(0) for x$what$id_what
  x <- empty_steplist()
  x$what %<>% dplyr::filter(.data$id_what != "a0")

  expect_equal(fun_get_id(x$what$id_what, "what"), "a1")
  expect_equal(fun_get_id(x$what$id_what, "does"), "d1")
  expect_equal(fun_get_id(x$what$id_what, "where"), "e1")
  expect_equal(fun_get_id(x$what$id_what, "module"), "m1")
  expect_equal(fun_get_id(x$what$id_what, "icc"), "i1")
})

test_that("fun_get_id works for NULL, NA, non-character inputs, and ''", {
  expect_equal(fun_get_id(NULL,"what"),"a1")
  expect_equal(fun_get_id(NA,"what"),"a1")
  expect_equal(fun_get_id(1,"what"),"a1")
  expect_equal(fun_get_id(TRUE,"what"),"a1")
  expect_equal(fun_get_id("","what"),"a1")
})


test_that("THEN IDs normal behavior works", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_equal(fun_create_then_step_id(subject_key = "Laura",
                                       does_key = "bring",
                                       object_key = "cake",
                                       where_key = "beach",
                                       check_object = "",
                                       what_data = steplist_party_test$what,
                                       does_data = steplist_party_test$does,
                                       where_data = steplist_party_test$where,
                                       then_data = steplist_party_test$then),"a6d2a1e6")
  expect_equal(fun_create_then_step_id(subject_key = "Laura",
                                       does_key = "happy",
                                       object_key = "food is fine",
                                       where_key = "beach",
                                       check_object = "1",
                                       what_data = steplist_party_test$what,
                                       does_data = steplist_party_test$does,
                                       where_data = steplist_party_test$where,
                                       then_data = steplist_party_test$then),"a6d4(a9d6)e6")
})

test_that("THEN IDs creation can handle NA and NULL", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_equal(fun_create_then_step_id(subject_key = NULL,
                                       does_key = NULL,
                                       object_key = NULL,
                                       where_key = NULL,
                                       check_object = "",
                                       what_data = steplist_party_test$what,
                                       does_data = steplist_party_test$does,
                                       where_data = steplist_party_test$where,
                                       then_data = steplist_party_test$then),"")
  expect_equal(fun_create_then_step_id(subject_key = "NA",
                                       does_key = "NA",
                                       object_key = "NA",
                                       where_key = "NA",
                                       check_object = "",
                                       what_data = steplist_party_test$what,
                                       does_data = steplist_party_test$does,
                                       where_data = steplist_party_test$where,
                                       then_data = steplist_party_test$then),"")
  expect_equal(fun_create_then_step_id(subject_key = NA,
                                       does_key = NA,
                                       object_key = NA,
                                       where_key = NA,
                                       check_object = "",
                                       what_data = steplist_party_test$what,
                                       does_data = steplist_party_test$does,
                                       where_data = steplist_party_test$where,
                                       then_data = steplist_party_test$then),"")
})

test_that("THEN descriptions normal behavior works", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_equal(fun_create_then_step_desc(subject_key = "Laura",
                                       does_key = "bring",
                                       object_key = "cake",
                                       where_key = "beach",
                                       check_object = "",
                                       what_data = steplist_party_test$what,
                                       does_data = steplist_party_test$does,
                                       where_data = steplist_party_test$where,
                                       then_data = steplist_party_test$then),"Laura brings birthday cake at the beach")
  expect_equal(fun_create_then_step_desc(subject_key = "Laura",
                                       does_key = "happy",
                                       object_key = "food is fine",
                                       where_key = "beach",
                                       check_object = "1",
                                       what_data = steplist_party_test$what,
                                       does_data = steplist_party_test$does,
                                       where_data = steplist_party_test$where,
                                       then_data = steplist_party_test$then),"Laura is happy that food is fine at the beach")
})

test_that("THEN descriptions creation can handle NA and NULL", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_equal(fun_create_then_step_desc(subject_key = NULL,
                                       does_key = NULL,
                                       object_key = NULL,
                                       where_key = NULL,
                                       check_object = "",
                                       what_data = steplist_party_test$what,
                                       does_data = steplist_party_test$does,
                                       where_data = steplist_party_test$where,
                                       then_data = steplist_party_test$then),"")
  expect_equal(fun_create_then_step_desc(subject_key = "NA",
                                       does_key = "NA",
                                       object_key = "NA",
                                       where_key = "NA",
                                       check_object = "",
                                       what_data = steplist_party_test$what,
                                       does_data = steplist_party_test$does,
                                       where_data = steplist_party_test$where,
                                       then_data = steplist_party_test$then),"")
  expect_equal(fun_create_then_step_desc(subject_key = NA,
                                       does_key = NA,
                                       object_key = NA,
                                       where_key = NA,
                                       check_object = "",
                                       what_data = steplist_party_test$what,
                                       does_data = steplist_party_test$does,
                                       where_data = steplist_party_test$where,
                                       then_data = steplist_party_test$then),"")
})

test_that("normal IFNOT IDs work", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_equal(fun_create_step_ifnot_id(input_select = list("birthday party takes place inside","birthday party takes place outside","no rain"),
                                        input_numeric = list("1","2","2"),
                                        then_data = steplist_party_test$then),list("(a7d3e2)or(a7d3e1+a3)",FALSE,FALSE))
})

test_that("error indicators in IFNOT ID work", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_true(fun_create_step_ifnot_id(input_select = list("Ana is invited","Ana is invited","Emma is invited"),
                           input_numeric = list("1","1","1"),
                           then_data = steplist_party_test$then)[[2]])
  expect_true(fun_create_step_ifnot_id(input_select = list("Ana is invited","Ana is invited","Emma is invited"),
                                       input_numeric = list("1","2","3"),
                                       then_data = steplist_party_test$then)[[3]])
})

test_that("normal IFNOT descriptions work", {
  expect_equal(fun_create_step_ifnot_desc(input_select = list("birthday party takes place inside","birthday party takes place outside","no rain"),
                                        input_numeric = list("1","2","2")),
               "birthday party takes place inside or (birthday party takes place outside and no rain)")
})



test_that("normal IF IDs work", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_equal(fun_create_step_if_id(input_select = list("birthday party takes place inside","birthday party takes place outside","no rain"),
                                        input_numeric = list("1","2","2"),
                                        then_data = steplist_party_test$then),list("(a7d3e2)or(a7d3e1+a3)",FALSE,FALSE))
})

test_that("error indicators in IF ID work", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_true(fun_create_step_if_id(input_select = list("Ana is invited","Ana is invited","Emma is invited"),
                                       input_numeric = list("1","1","1"),
                                       then_data = steplist_party_test$then)[[2]])
  expect_true(fun_create_step_if_id(input_select = list("Ana is invited","Ana is invited","Emma is invited"),
                                       input_numeric = list("1","2","3"),
                                       then_data = steplist_party_test$then)[[3]])
})

test_that("normal IF descriptions work", {
  expect_equal(fun_create_step_if_desc(input_select = list("birthday party takes place inside","birthday party takes place outside","no rain"),
                                          input_numeric = list("1","2","2")),"birthday party takes place inside or (birthday party takes place outside and no rain)")
})

test_that("Step IDs can be created", {
  expect_equal(fun_create_step_id(input_if = "a5d1",
                                  input_ifnot = "a7d3e3",
                                  input_then = "a5d5"),"IFa5d1IFNOTa7d3e3THENa5d5")
  expect_equal(fun_create_step_id(input_if = "",
                                  input_ifnot = "",
                                  input_then = "a5d5"),"THENa5d5")
})

test_that("Step descriptions can be created", {
  expect_equal(fun_create_step_desc(input_if = "birthday party takes place inside or (birthday party takes place outside and no rain)",
                                    input_ifnot = "",
                                    input_then = "weather is fine",
                                    input_end_step = "1"),
               "End: IF birthday party takes place inside or (birthday party takes place outside and no rain) THEN weather is fine")
  expect_equal(fun_create_step_desc(input_if = "",
                                    input_ifnot = "",
                                    input_then = "birthday party takes place on a weekday",
                                    input_end_step = "0"),
               "Start: birthday party takes place on a weekday")
})

test_that("gun_get_module_id works", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_equal(fun_get_module_id(steplist_module = steplist_party_test$module,
                                 input = "food"),"m2")
})

test_that("gun_get_module_id works for NULL and ''", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_equal(fun_get_module_id(steplist_module = steplist_party_test$module,
                                 input = NULL),"")
  expect_equal(fun_get_module_id(steplist_module = steplist_party_test$module,
                                 input = ""),"")
})

test_that("no end steps lead to empty selection in outc", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  x <- steplist_party_test$step %>% dplyr::filter(.data$end_step == "0")
  test <- steplist_party_test
  test$step <- x
  expect_equal(get_options_outc(test),"")
})

test_that("selection in outc works", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_equal(get_options_outc(steplist_party_test),
               c("Emma is coming","food is fine","Laura is coming","weather is fine"))
})

test_that("creating outc IDs works", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_equal(get_id_outc("food is fine",steplist_party_test$step),"a9d6")

})

test_that("creating outc IDs works for NULL, '', non-used values", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_equal(get_id_outc(NULL,steplist_party_test$step),"")
  expect_equal(get_id_outc("",steplist_party_test$step),"")
  expect_equal(get_id_outc("test",steplist_party_test$step),"NA")
})

test_that("create_outc_desc returns correct values", {
  expect_equal(create_outc_desc(list("","",""), coll = T),"")
  expect_equal(create_outc_desc(list("","",""), coll = F),"")
  expect_equal(create_outc_desc(list("","","test"), coll = T),"test")
  expect_equal(create_outc_desc(list("","","test"), coll = F),"test")
  expect_equal(create_outc_desc(list("","test1","test 2"), coll = T),"test1 and test 2")
  expect_equal(create_outc_desc(list("","test1","test 2"), coll = F),c("test1","test 2"))
  expect_equal(create_outc_desc(NULL, coll = T),"")
  expect_equal(create_outc_desc(NULL, coll = F),"")
  expect_equal(create_outc_desc(c(NULL,""), coll = F),"")
  expect_equal(create_outc_desc(c(NULL,"test","test 2"), coll = F),c("test","test 2"))
  expect_equal(create_outc_desc(c(NULL,"test","test 2"), coll = T),"test and test 2")
})

test_that("check_outc_duplicates works", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_true(check_outc_duplicates("a4d5+a9d6+a6d5+a2d6",steplist_party_test$outc))
  expect_true(check_outc_duplicates("a4d5+a9d6+a2d6+a6d5",steplist_party_test$outc))
  expect_false(check_outc_duplicates("a4d5+a9d6+a2d6+a6d5+a12d54e4",steplist_party_test$outc))
  expect_false(check_outc_duplicates("a4d5+a9d6+a6d5",steplist_party_test$outc))
  expect_false(check_outc_duplicates("",steplist_party_test$outc))
  expect_false(check_outc_duplicates(NULL,steplist_party_test$outc))
})
