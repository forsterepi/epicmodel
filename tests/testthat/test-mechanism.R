test_that("constructor gives class", {
  expect_s3_class(new_mechanism(), "epicmodel_mechanism")
})

test_that("validate_mechanism, plot, print work", {
  expect_error(new_mechanism() %>% validate_mechanism())
  expect_error(empty_steplist() %>% validate_mechanism())

  scc_party <- readRDS(test_path("fixtures", "scc_party.rds"))
  expect_no_error(mechanism_party <- mechanism(scc_party, modules = F))
  mechanism_party <- mechanism(scc_party)
  expect_no_error(mechanism_party %>% validate_mechanism())

  expect_no_error(mechanism_party %>% plot())
  expect_no_error(mechanism_party %>% plot(reverse = F))
  expect_no_error(mechanism_party %>% print())
})

test_that("mechanism works as expected", {
  steplist_rain_test <- readRDS(test_path("fixtures", "steplist_rain_test.rds")) %>% check_steplist()
  scc_rain_test <- create_scc(steplist_rain_test)
  expect_no_error(scc_rain_test %>% mechanism())
  expect_no_error(scc_rain_test %>% mechanism(modules = F))

  steplist_mini_intv <- readRDS(test_path("fixtures", "steplist_mini_intv.rds")) %>% check_steplist()
  scc_mini_intv <- create_scc(steplist_mini_intv)
  expect_no_error(scc_mini_intv %>% mechanism())
  expect_no_warning(scc_mini_intv %>% mechanism())
  expect_warning(scc_mini_intv %>% mechanism(modules = T), regexp = "The SCC model does not use modules!")

  steplist_mini_intv_to_depends <- readRDS(test_path("fixtures", "steplist_mini_intv_to_depends.rds")) %>% check_steplist()
  scc_mini_intv_to_depends <- create_scc(steplist_mini_intv_to_depends)
  expect_no_error(scc_mini_intv_to_depends %>% mechanism())
})

test_that("export_mechanism_handle_input works", {
  expect_equal(export_mechanism_handle_input(sc = NULL, n_sc = 2, file_name = NULL, file_type = "png", title = NULL)[["title_used"]],
               NULL)
  expect_equal(export_mechanism_handle_input(sc = NULL, n_sc = 2, file_name = NULL, file_type = "png", title = "test")[["title_used"]],
               c("test1","test2"))
  expect_equal(export_mechanism_handle_input(sc = NULL, n_sc = 2, file_name = NULL, file_type = "png", title = NULL)[["file_name_used"]],
               c("graph_sc1.png","graph_sc2.png"))
  expect_equal(export_mechanism_handle_input(sc = NULL, n_sc = 2, file_name = NULL, file_type = "PNG", title = NULL)[["file_name_used"]],
               c("graph_sc1.png","graph_sc2.png"))
  expect_equal(export_mechanism_handle_input(sc = NULL, n_sc = 2, file_name = "graph_sc.PNG", file_type = "PNG", title = NULL)[["file_name_used"]],
               c("graph_sc1.png","graph_sc2.png"))
  expect_equal(export_mechanism_handle_input(sc = NULL, n_sc = 2, file_name = "graph_sc.pNg", file_type = "PNG", title = NULL)[["file_name_used"]],
               c("graph_sc1.png","graph_sc2.png"))
  expect_equal(export_mechanism_handle_input(sc = NULL, n_sc = 2, file_name = "graph_sc.PNG", file_type = "pdf", title = NULL)[["file_name_used"]],
               c("graph_sc.png1.pdf","graph_sc.png2.pdf"))
  expect_equal(export_mechanism_handle_input(sc = NULL, n_sc = 2, file_name = "graph_sc.PNG", file_type = "PDF", title = NULL)[["file_name_used"]],
               c("graph_sc.png1.pdf","graph_sc.png2.pdf"))
  expect_equal(export_mechanism_handle_input(sc = 2, n_sc = 2, file_name = NULL, file_type = "png", title = NULL)[["title_used"]],
               NULL)
  expect_equal(export_mechanism_handle_input(sc = 2L, n_sc = 2, file_name = NULL, file_type = "png", title = NULL)[["title_used"]],
               NULL)
  expect_equal(export_mechanism_handle_input(sc = 2, n_sc = 2, file_name = NULL, file_type = "png", title = "test")[["title_used"]],
               "test")
  expect_equal(export_mechanism_handle_input(sc = 2, n_sc = 2, file_name = NULL, file_type = "png", title = NULL)[["file_name_used"]],
               "graph_sc2.png")
  expect_equal(export_mechanism_handle_input(sc = 2, n_sc = 2, file_name = NULL, file_type = "PNG", title = NULL)[["file_name_used"]],
               "graph_sc2.png")
  expect_equal(export_mechanism_handle_input(sc = 2, n_sc = 2, file_name = "graph_sc.PNG", file_type = "PNG", title = NULL)[["file_name_used"]],
               "graph_sc.png")
  expect_equal(export_mechanism_handle_input(sc = 2, n_sc = 2, file_name = "graph_sc.pNg", file_type = "PNG", title = NULL)[["file_name_used"]],
               "graph_sc.png")
  expect_equal(export_mechanism_handle_input(sc = 2, n_sc = 2, file_name = "graph_sc.PNG", file_type = "pdf", title = NULL)[["file_name_used"]],
               "graph_sc.png.pdf")
  expect_equal(export_mechanism_handle_input(sc = 2, n_sc = 2, file_name = "graph_sc.PNG", file_type = "PDF", title = NULL)[["file_name_used"]],
               "graph_sc.png.pdf")
})

test_that("are_colors throws errors for invalid inputs", {
  expect_error(are_colors(NA), class = "input_x")
  expect_error(are_colors(NULL), class = "input_x")
  expect_error(are_colors(""), class = "input_x")
})

test_that("are_colors knows all types of color specification", {
  expect_true(are_colors(c("black","#123123","#ACBdef")))
  expect_false(are_colors("123456"))
  expect_false(are_colors("blackk"))
  expect_false(are_colors("#12312"))
})
