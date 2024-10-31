test_that("for sep_then, separating different patterns with WHAT object works", {
  expect_equal(sep_then(c("a14","a34")),
               list(c("a14","a34"),
                    as.character(c(NA,NA)),
                    as.character(c(NA,NA)),
                    as.character(c(NA,NA))) %>% magrittr::set_names(c("subject","does","object","where")))
  expect_equal(sep_then(c("d32","d45")),
               list(as.character(c(NA,NA)),
                    c("d32","d45"),
                    as.character(c(NA,NA)),
                    as.character(c(NA,NA))) %>% magrittr::set_names(c("subject","does","object","where")))
  expect_equal(sep_then(c("a2456d98","a34d9")),
               list(c("a2456","a34"),
                    c("d98","d9"),
                    as.character(c(NA,NA)),
                    as.character(c(NA,NA))) %>% magrittr::set_names(c("subject","does","object","where")))
  expect_equal(sep_then(c("a32a90","a4a909")),
               list(c("a32","a4"),
                    as.character(c(NA,NA)),
                    c("a90","a909"),
                    as.character(c(NA,NA))) %>% magrittr::set_names(c("subject","does","object","where")))
  expect_equal(sep_then(c("d4a65","d6a78")),
               list(as.character(c(NA,NA)),
                    c("d4","d6"),
                    c("a65","a78"),
                    as.character(c(NA,NA))) %>% magrittr::set_names(c("subject","does","object","where")))
  expect_equal(sep_then(c("a1d2a333","a88d88a99")),
               list(c("a1","a88"),
                    c("d2","d88"),
                    c("a333","a99"),
                    as.character(c(NA,NA))) %>% magrittr::set_names(c("subject","does","object","where")))
  expect_equal(sep_then(c("e35","e45")),
               list(as.character(c(NA,NA)),
                    as.character(c(NA,NA)),
                    as.character(c(NA,NA)),
                    c("e35","e45")) %>% magrittr::set_names(c("subject","does","object","where")))
  expect_equal(sep_then(c("a4e87","a87e49")),
               list(c("a4","a87"),
                    as.character(c(NA,NA)),
                    as.character(c(NA,NA)),
                    c("e87","e49")) %>% magrittr::set_names(c("subject","does","object","where")))
  expect_equal(sep_then(c("d54e1","d0,e4")),
               list(as.character(c(NA,NA)),
                    c("d54","d0"),
                    as.character(c(NA,NA)),
                    c("e1","e4")) %>% magrittr::set_names(c("subject","does","object","where")))
  expect_equal(sep_then(c("a43d5e12","a23d34e4")),
               list(c("a43","a23"),
                    c("d5","d34"),
                    as.character(c(NA,NA)),
                    c("e12","e4")) %>% magrittr::set_names(c("subject","does","object","where")))
  expect_equal(sep_then(c("a12a3e43","a4a18e3")),
               list(c("a12","a4"),
                    as.character(c(NA,NA)),
                    c("a3","a18"),
                    c("e43","e3")) %>% magrittr::set_names(c("subject","does","object","where")))
  expect_equal(sep_then(c("d23a5e990","d34a1e90")),
               list(as.character(c(NA,NA)),
                    c("d23","d34"),
                    c("a5","a1"),
                    c("e990","e90")) %>% magrittr::set_names(c("subject","does","object","where")))
  expect_equal(sep_then(c("a9d8a6e7","a11d22a33e44")),
               list(c("a9","a11"),
                    c("d8","d22"),
                    c("a6","a33"),
                    c("e7","e44")) %>% magrittr::set_names(c("subject","does","object","where")))
})

test_that("for sep_then, separating different patterns with THEN object works", {
  expect_equal(sep_then(c("(a32a90)","(a37a92)")),
               list(as.character(c(NA,NA)),
                    as.character(c(NA,NA)),
                    c("a32a90","a37a92"),
                    as.character(c(NA,NA))) %>% magrittr::set_names(c("subject","does","object","where")))
  expect_equal(sep_then(c("a32(a1d2a3e4)","a2(a14d22a4e98)")),
               list(c("a32","a2"),
                    as.character(c(NA,NA)),
                    c("a1d2a3e4","a14d22a4e98"),
                    as.character(c(NA,NA))) %>% magrittr::set_names(c("subject","does","object","where")))
  expect_equal(sep_then(c("d4(a1d2a3e4)","d34(a0d14a33e45)")),
               list(as.character(c(NA,NA)),
                    c("d4","d34"),
                    c("a1d2a3e4","a0d14a33e45"),
                    as.character(c(NA,NA))) %>% magrittr::set_names(c("subject","does","object","where")))
  expect_equal(sep_then(c("a1d2(a1d2a3e4)","a12d24(a11d21a32e34)")),
               list(c("a1","a12"),
                    c("d2","d24"),
                    c("a1d2a3e4","a11d21a32e34"),
                    as.character(c(NA,NA))) %>% magrittr::set_names(c("subject","does","object","where")))
  expect_equal(sep_then(c("a12(a1d2a3e4)e43","a1232567552334(a12d22a343e44)e4")),
               list(c("a12","a1232567552334"),
                    as.character(c(NA,NA)),
                    c("a1d2a3e4","a12d22a343e44"),
                    c("e43","e4")) %>% magrittr::set_names(c("subject","does","object","where")))
  expect_equal(sep_then(c("d23(a1d2a3e4)e990","d23423(a7d3a32e14)e90")),
               list(as.character(c(NA,NA)),
                    c("d23","d23423"),
                    c("a1d2a3e4","a7d3a32e14"),
                    c("e990","e90")) %>% magrittr::set_names(c("subject","does","object","where")))
  expect_equal(sep_then(c("a9d8(a1d2a3e4)e7","a1d1(a1d2a5e4)e1")),
               list(c("a9","a1"),
                    c("d8","d1"),
                    c("a1d2a3e4","a1d2a5e4"),
                    c("e7","e1")) %>% magrittr::set_names(c("subject","does","object","where")))
  expect_equal(sep_then(c("(a1d2a3e4)e43","(a111d211a13e41)e413")),
               list(as.character(c(NA,NA)),
                    as.character(c(NA,NA)),
                    c("a1d2a3e4","a111d211a13e41"),
                    c("e43","e413")) %>% magrittr::set_names(c("subject","does","object","where")))
})

test_that("for sep_then, NA inputs work", {
  expect_equal(sep_then(NA),
               list(as.character(NA),
                    as.character(NA),
                    as.character(NA),
                    as.character(NA)) %>% magrittr::set_names(c("subject","does","object","where")))
  expect_equal(sep_then(c(NA,NA)),
               list(as.character(c(NA,NA)),
                    as.character(c(NA,NA)),
                    as.character(c(NA,NA)),
                    as.character(c(NA,NA))) %>% magrittr::set_names(c("subject","does","object","where")))
  expect_equal(sep_then(c("a2d3e4",NA)),
               list(as.character(c("a2",NA)),
                    as.character(c("d3",NA)),
                    as.character(c(NA,NA)),
                    as.character(c("e4",NA))) %>% magrittr::set_names(c("subject","does","object","where")))
})

test_that("for sep_then, keywords throw an error", {
  expect_error(sep_then(c("THENa12","a12")), class = "sep_then_keyword")
  expect_error(sep_then(c("IFa12","a12")), class = "sep_then_keyword")
  expect_error(sep_then(c("IFNOTa12","a12")), class = "sep_then_keyword")
})

test_that("for sep_step, separating steps works", {
  expect_equal(sep_step(c("IFa5d1IFNOTa7d3e3THENa5d5","IFa51d11IFNOTa17d31e31THENa51d51")),
               list(c("a5d1","a51d11"),
                    c("a7d3e3","a17d31e31"),
                    c("a5d5","a51d51")) %>% magrittr::set_names(c("if","ifnot","then")))
  expect_equal(sep_step(c("IF(a7d3e5)or(a7d3e4)THENa7d3e2","IF(a71d32e35)or(a37d33e4)THENa17d32e2")),
               list(c("(a7d3e5)or(a7d3e4)","(a71d32e35)or(a37d33e4)"),
                    as.character(c(NA,NA)),
                    c("a7d3e2","a17d32e2")) %>% magrittr::set_names(c("if","ifnot","then")))
  expect_equal(sep_step(c("IFNOT(a4d1+a5d5)or(a4d1+a7d3e5)THENa4d5","IFNOT(a4d1+a445d5)or(a4d1+a7d3e5)THENa234d5")),
               list(as.character(c(NA,NA)),
                    c("(a4d1+a5d5)or(a4d1+a7d3e5)","(a4d1+a445d5)or(a4d1+a7d3e5)"),
                    c("a4d5","a234d5")) %>% magrittr::set_names(c("if","ifnot","then")))
  expect_equal(sep_step(c("THENa7d3e6","THENa712d3e6")),
               list(as.character(c(NA,NA)),
                    as.character(c(NA,NA)),
                    c("a7d3e6","a712d3e6")) %>% magrittr::set_names(c("if","ifnot","then")))
})

test_that("for sep_step, NA inputs work", {
  expect_equal(sep_step(NA), list(as.character(NA),
                                  as.character(NA),
                                  as.character(NA)) %>% magrittr::set_names(c("if","ifnot","then")))
  expect_equal(sep_step(c(NA,NA)), list(as.character(c(NA,NA)),
                                  as.character(c(NA,NA)),
                                  as.character(c(NA,NA))) %>% magrittr::set_names(c("if","ifnot","then")))
})

test_that("sep_if_ifnot works", {
  tab1 <- matrix(c("1","a5d5","1","a7d3e5","2","a6d5","3","a4d5"), nrow = 4, ncol = 2, byrow = T) %>%
    as.data.frame() %>% magrittr::set_colnames(c("sce","id"))
  tab2 <- matrix(c("1","a5d5"), nrow = 1, ncol = 2, byrow = T) %>%
    as.data.frame() %>% magrittr::set_colnames(c("sce","id"))
  tab3 <- matrix(c("1","a5d5","1","a5d1"), nrow = 2, ncol = 2, byrow = T) %>%
    as.data.frame() %>% magrittr::set_colnames(c("sce","id"))
  tab_na <- matrix(c(NA,NA), nrow = 1, ncol = 2, byrow = T) %>%
    as.data.frame() %>% magrittr::set_colnames(c("sce","id"))

  expect_equal(sep_if_ifnot(c("(a5d5+a7d3e5)or(a6d5)or(a4d5)",
                              "a5d5",
                              NA,
                              "a5d5+a5d1")),
               list(tab1,tab2,tab_na,tab3))
  expect_equal(sep_if_ifnot(NA),list(tab_na))
})

test_that("sep_if_ifnot works for edge cases", {
  zero <- readRDS(test_path("fixtures", "steplist_zero.rds"))
  zero_sep <- sep_step(zero$step$id_step)
  expect_equal(sep_if_ifnot(zero_sep[["if"]])[[1]] %>% nrow(),0)
  expect_equal(sep_if_ifnot(NA)[[1]] %>% nrow(),1)
})

test_that("processing steplists works", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  prc_steplist_party <- readRDS(test_path("fixtures", "prc_steplist_party.rds"))
  expect_equal(process_steplist(steplist_party_test),prc_steplist_party)

  steplist_zero <- readRDS(test_path("fixtures", "steplist_zero.rds"))
  expect_equal(process_steplist(steplist_zero) %>% nrow(),0)

  steplist_na <- readRDS(test_path("fixtures", "steplist_na.rds"))
  expect_equal(process_steplist(steplist_na) %>% nrow(),1)
})
