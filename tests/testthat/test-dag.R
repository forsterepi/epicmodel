test_that("scc_to_dag works", {
  steplist_party_test <- readRDS(test_path("fixtures", "steplist_party_test.rds"))
  expect_error(scc_to_dag(steplist_party_test), class = "no_scc")
  expect_error(scc_to_dag(scc = NULL), class = "no_scc")
  expect_error(scc_to_dag(scc = NA), class = "no_scc")
})

test_that("scc_to_dag mini_and", {
  steplist_mini_and <- readRDS(test_path("fixtures", "steplist_mini_and.rds")) %>% check_steplist()
  scc_mini_and <- steplist_mini_and %>% create_scc()

  expect_no_error(scc_mini_and %>% scc_to_dag(unknown = T))
  expect_no_error(scc_mini_and %>% scc_to_dag(unknown = F))

  dag <- dagitty::dagitty('dag {CC1 [pos="1.000,0.250"]
                                CC2 [pos="1.000,0.750"]
                                O [outcome,pos="4.000,0.500"]
                                SC1 [pos="3.000,0.000"]
                                USC [pos="3.000,1.000"]
                                U_SC1 [pos="2.000,0.000"]
                                U_USC [pos="2.000,1.000"]
                                CC1 -> SC1
                                CC2 -> SC1
                                SC1 -> O
                                USC -> O
                                U_SC1 -> SC1
                                U_USC -> USC
                              }')
  legend <- data.frame(desc = c("a1","a2"), label = c("CC1","CC2"))
  expect_equal(scc_mini_and %>% scc_to_dag(unknown = T) %>% magrittr::extract2("dag"), dag)
  expect_equal(scc_mini_and %>% scc_to_dag(unknown = T) %>% magrittr::extract2("legend"), legend)

  expect_no_error(scc_mini_and %>% scc_to_dag(unknown = T) %>% magrittr::extract2("dag") %>% plot_dag())
  expect_error(scc_mini_and %>% scc_to_dag(unknown = T) %>% plot_dag(), class = "scc_to_dag_complete_list")
})

test_that("scc_to_dag mini_depends", {
  steplist_mini_depends <- readRDS(test_path("fixtures", "steplist_mini_depends.rds")) %>% check_steplist()
  scc_mini_depends <- steplist_mini_depends %>% create_scc()

  expect_no_error(scc_mini_depends %>% scc_to_dag(unknown = T))
  expect_no_error(scc_mini_depends %>% scc_to_dag(unknown = F))

  dag <- dagitty::dagitty('dag {CC1 [pos="1.000,0.250"]
                                CC2 [pos="1.000,0.750"]
                                O [outcome,pos="4.000,0.500"]
                                SC1 [pos="3.000,0.000"]
                                USC [pos="3.000,1.000"]
                                U_SC1 [pos="2.000,0.000"]
                                U_USC [pos="2.000,1.000"]
                                CC1 -> SC1
                                CC2 -> SC1
                                SC1 -> O
                                USC -> O
                                U_SC1 -> SC1
                                U_USC -> USC
                              }')
  legend <- data.frame(desc = c("a1","a2"), label = c("CC1","CC2"))
  expect_equal(scc_mini_depends %>% scc_to_dag(unknown = T) %>% magrittr::extract2("dag"), dag)
  expect_equal(scc_mini_depends %>% scc_to_dag(unknown = T) %>% magrittr::extract2("legend"), legend)

  expect_no_error(scc_mini_depends %>% scc_to_dag(unknown = T) %>% magrittr::extract2("dag") %>% plot_dag())
})

test_that("scc_to_dag mini_implau", {
  steplist_mini_implau <- readRDS(test_path("fixtures", "steplist_mini_implau.rds")) %>% check_steplist()
  scc_mini_implau <- steplist_mini_implau %>% create_scc()

  expect_no_error(scc_mini_implau %>% scc_to_dag(unknown = T))
  expect_no_error(scc_mini_implau %>% scc_to_dag(unknown = F))

  dag <- dagitty::dagitty('dag {CC1 [pos="1.000,0.250"]
                                CC2 [pos="1.000,0.750"]
                                CC3 [pos="1.000,1.250"]
                                O [outcome,pos="4.000,0.750"]
                                SC1 [pos="3.000,0.000"]
                                USC [pos="3.000,1.500"]
                                U_SC1 [pos="2.000,0.000"]
                                U_USC [pos="2.000,1.500"]
                                CC1 -> SC1
                                CC2 -> SC1
                                CC3 -> SC1
                                SC1 -> O
                                USC -> O
                                U_SC1 -> SC1
                                U_USC -> USC
                              }')
  legend <- data.frame(desc = c("a1","a2","a3"), label = c("CC1","CC2","CC3"))
  expect_equal(scc_mini_implau %>% scc_to_dag(unknown = T) %>% magrittr::extract2("dag"), dag)
  expect_equal(scc_mini_implau %>% scc_to_dag(unknown = T) %>% magrittr::extract2("legend"), legend)

  expect_no_error(scc_mini_implau %>% scc_to_dag(unknown = T) %>% magrittr::extract2("dag") %>% plot_dag())
})

test_that("scc_to_dag mini_intv", {
  steplist_mini_intv <- readRDS(test_path("fixtures", "steplist_mini_intv.rds")) %>% check_steplist()
  scc_mini_intv <- steplist_mini_intv %>% create_scc()

  expect_no_error(scc_mini_intv %>% scc_to_dag(unknown = T))
  expect_no_error(scc_mini_intv %>% scc_to_dag(unknown = F))

  dag <- dagitty::dagitty('dag {CC1 [pos="1.000,0.250"]
                                O [outcome,pos="3.000,0.250"]
                                SC1 [pos="2.000,0.000"]
                                CC1 -> SC1
                                SC1 -> O
                                }')
  legend <- data.frame(desc = c("a1"), label = c("CC1"))
  expect_equal(scc_mini_intv %>% scc_to_dag(unknown = F) %>% magrittr::extract2("dag"), dag)
  expect_equal(scc_mini_intv %>% scc_to_dag(unknown = F) %>% magrittr::extract2("legend"), legend)

  expect_no_error(scc_mini_intv %>% scc_to_dag(unknown = T) %>% magrittr::extract2("dag") %>% plot_dag())
})

test_that("scc_to_dag mini_or", {
  steplist_mini_or <- readRDS(test_path("fixtures", "steplist_mini_or.rds")) %>% check_steplist()
  scc_mini_or <- steplist_mini_or %>% create_scc()

  expect_no_error(scc_mini_or %>% scc_to_dag(unknown = T))
  expect_no_error(scc_mini_or %>% scc_to_dag(unknown = F))

  dag <- dagitty::dagitty('dag {CC1 [pos="1.000,0.250"]
                                CC2 [pos="1.000,0.750"]
                                O [outcome,pos="4.000,0.500"]
                                SC1 [pos="3.000,0.000"]
                                SC2 [pos="3.000,0.500"]
                                USC [pos="3.000,1.000"]
                                U_SC1 [pos="2.000,0.000"]
                                U_SC2 [pos="2.000,0.500"]
                                U_USC [pos="2.000,1.000"]
                                CC1 -> SC1
                                CC2 -> SC2
                                SC1 -> O
                                SC2 -> O
                                USC -> O
                                U_SC1 -> SC1
                                U_SC2 -> SC2
                                U_USC -> USC
                              }')
  legend <- data.frame(desc = c("a2","a1"), label = c("CC1","CC2"))
  expect_equal(scc_mini_or %>% scc_to_dag(unknown = T) %>% magrittr::extract2("dag"), dag)
  expect_equal(scc_mini_or %>% scc_to_dag(unknown = T) %>% magrittr::extract2("legend"), legend)

  expect_no_error(scc_mini_or %>% scc_to_dag(unknown = T) %>% magrittr::extract2("dag") %>% plot_dag())
})

test_that("scc_to_dag rain_test", {
  steplist_rain_test <- readRDS(test_path("fixtures", "steplist_rain_test.rds")) %>% check_steplist()
  scc_rain_test <- steplist_rain_test %>% create_scc()

  expect_no_error(scc_rain_test %>% scc_to_dag(unknown = T))
  expect_no_error(scc_rain_test %>% scc_to_dag(unknown = F))

  dag <- dagitty::dagitty('dag {CC1 [pos="1.000,0.250"]
                                CC2 [pos="1.000,0.750"]
                                CC3 [pos="1.000,1.250"]
                                CC4 [pos="1.000,1.750"]
                                O [outcome,pos="4.000,1.000"]
                                SC1 [pos="3.000,0.000"]
                                SC2 [pos="3.000,1.000"]
                                USC [pos="3.000,2.000"]
                                U_SC1 [pos="2.000,0.000"]
                                U_SC2 [pos="2.000,1.000"]
                                U_USC [pos="2.000,2.000"]
                                CC1 -> SC1
                                CC1 -> SC2
                                CC2 -> SC1
                                CC3 -> SC2
                                CC4 -> SC2
                                SC1 -> O
                                SC2 -> O
                                USC -> O
                                U_SC1 -> SC1
                                U_SC2 -> SC2
                                U_USC -> USC
                                }')
  legend <- data.frame(desc = c("rain","get groceries","no vacation","weekday"), label = c("CC1","CC2","CC3","CC4"))
  expect_equal(scc_rain_test %>% scc_to_dag(unknown = T) %>% magrittr::extract2("dag"), dag)
  expect_equal(scc_rain_test %>% scc_to_dag(unknown = T) %>% magrittr::extract2("legend"), legend)

  expect_no_error(scc_rain_test %>% scc_to_dag(unknown = T) %>% magrittr::extract2("dag") %>% plot_dag())
})

test_that("plot_dag dag inputs work", {
  expect_error(plot_dag(dag = ""), class = "no_dag")
  expect_error(plot_dag(dag = NA), class = "no_dag")
  expect_error(plot_dag(dag = NULL), class = "no_dag")
  expect_error(plot_dag(dag = 'dag {E -> O}'), class = "no_dag")
})

test_that("plot_dag node & path input types are checked correctly", {
  dag1 <- dagitty::dagitty('dag {E -> O}')
  expect_error(plot_dag(dag1, node_outc = ""), class = "input_node_outc")
  expect_error(plot_dag(dag1, node_outc = NA), class = "input_node_outc")
  expect_error(plot_dag(dag1, node_outc = c("E","O")), class = "input_node_outc")
  expect_error(plot_dag(dag1, node_expo = ""), class = "input_node_expo")
  expect_error(plot_dag(dag1, node_expo = NA), class = "input_node_expo")
  expect_error(plot_dag(dag1, node_expo = c("E","O")), class = "input_node_expo")
  expect_error(plot_dag(dag1, node_adj = NA), class = "input_node_adj")
  expect_error(plot_dag(dag1, node_adj = ""), class = "input_node_adj")
  expect_error(plot_dag(dag1, node_adj = c("E","")), class = "input_node_adj")
  expect_error(plot_dag(dag1, node_latent = NA), class = "input_node_latent")
  expect_error(plot_dag(dag1, node_latent = ""), class = "input_node_latent")
  expect_error(plot_dag(dag1, node_latent = c("E","")), class = "input_node_latent")
  expect_error(plot_dag(dag1, path_causal = NA), class = "input_path_causal")
  expect_error(plot_dag(dag1, path_causal = ""), class = "input_path_causal")
  expect_error(plot_dag(dag1, path_causal = c("E->O","")), class = "input_path_causal")
  expect_error(plot_dag(dag1, path_biased = NA), class = "input_path_biased")
  expect_error(plot_dag(dag1, path_biased = ""), class = "input_path_biased")
  expect_error(plot_dag(dag1, path_biased = c("E->O","")), class = "input_path_biased")
})

test_that("plot_dag recognizes node overlaps", {
  dag1 <- dagitty::dagitty('dag {O [outcome] V1 -> O}')
  expect_error(plot_dag(dag1, node_outc = "V1"), class = "outc_exists")
  expect_error(plot_dag(dag1, node_expo = "X"), class = "input_node_expo_choices")
  expect_error(plot_dag(dag1, node_adj = "O"), class = "input_node_adj_choices")
  expect_error(plot_dag(dag1, node_adj = c("V1","V2")), class = "input_node_adj_choices")
  expect_error(plot_dag(dag1, node_latent = "O"), class = "input_node_latent_choices")
  expect_error(plot_dag(dag1, node_latent = c("V1","V2")), class = "input_node_latent_choices")
  dag2 <- dagitty::dagitty('dag {E [exposure] E -> V2}')
  expect_error(plot_dag(dag2, node_expo = "V2"), class = "expo_exists")
  expect_error(plot_dag(dag2, node_outc = "X"), class = "input_node_outc_choices")
  dag3 <- dagitty::dagitty('dag {E -> O}')
  expect_error(plot_dag(dag3, node_outc = "E", node_expo = "E"), class = "outc_equals_expo")
  expect_error(plot_dag(dag3, node_outc = "O", node_adj = "O"), class = "outc_in_adj")
  expect_error(plot_dag(dag3, node_adj = "E", node_expo = "E"), class = "expo_in_adj")
  expect_error(plot_dag(dag3, node_outc = "O", node_latent = "O"), class = "outc_in_latent")
  expect_error(plot_dag(dag3, node_latent = "E", node_expo = "E"), class = "expo_in_latent")
  expect_error(plot_dag(dag3, node_adj = "E", node_latent = c("E","O")), class = "node_adj_latent_overlap")
})

test_that("plot_dag recognizes path overlaps", {
  dag1 <- dagitty::dagitty('dag {V1 -> V2 ; V2 -> V3}')
  expect_error(plot_dag(dag1, path_causal = "V3->V4"), class = "input_path_causal_choices")
  expect_error(plot_dag(dag1, path_biased = "V3->V4"), class = "input_path_biased_choices")
  expect_error(plot_dag(dag1, path_causal = "V1->V2", path_biased = "V1->V2"), class = "path_causal_biased_overlap")
  expect_no_error(plot_dag(dag1, path_causal = "V1 -> V2"))
  expect_no_error(plot_dag(dag1, path_biased = "V1 -> V2"))
})

test_that("plot_dag labels work", {
  dag1 <- dagitty::dagitty('dag {V1 -> V2 ; V2 -> V3}')
  expect_error(plot_dag(dag1, label = c("a","b","c")), class = "input_label")
  expect_error(plot_dag(dag1, label = NA), class = "input_label")
  expect_error(plot_dag(dag1, label = c(V1 = "", V2 = "b")), class = "input_label")
  expect_error(plot_dag(dag1, label = c(V1 = NA, V2 = "b")), class = "input_label")
  expect_error(plot_dag(dag1, label = c(V1 = "a", V4 = "b")), class = "label_names")
  expect_error(plot_dag(dag1, label = c(V1 = "a", all = "b")), class = "label_names")
  expect_error(plot_dag(dag1, label = c(V1 = "a", V1 = "b")), class = "label_names_dupli")
  expect_error(plot_dag(dag1, label_shift = list(c(1,2),c(0,1))), class = "input_label_shift")
  expect_error(plot_dag(dag1, label_shift = NA), class = "input_label_shift")
  expect_error(plot_dag(dag1, label_shift = list(V1 = c(1,1), V2 = 2)), class = "input_label_shift_elements")
  expect_error(plot_dag(dag1, label_shift = list(V1 = c(1,1), V4 = c(1,1))), class = "label_shift_names")
  expect_error(plot_dag(dag1, label_shift = list(V1 = c(1,1), V1 = c(1,1))), class = "label_shift_names_dupli")
  expect_no_error(plot_dag(dag1, label_shift = list(V1 = c(1,1), outcome = c(1,1), exposure = c(1,1), adjusted = c(1,1), latent = c(1,1),
                                                    other = c(1,1), all = c(1,1))))
  expect_error(plot_dag(dag1, label_size = NA), class = "input_label_size")
  expect_error(plot_dag(dag1, label_size = NULL), class = "input_label_size")
  expect_error(plot_dag(dag1, label_size = ""), class = "input_label_size")
  expect_error(plot_dag(dag1, label_size = c(1,2)), class = "input_label_size")
  expect_no_error(plot_dag(dag1, label_size = 1.2))
  expect_no_error(plot_dag(dag1, label_size = 12L))
})

test_that("plot_dag plot and scc parameters work", {
  dag1 <- dagitty::dagitty('dag {V1 -> V2 ; V2 -> V3}')
  expect_error(plot_dag(dag1, node_size = NA), class = "input_node_size")
  expect_error(plot_dag(dag1, node_size = NULL), class = "input_node_size")
  expect_error(plot_dag(dag1, node_size = ""), class = "input_node_size")
  expect_error(plot_dag(dag1, node_size = c(1,2)), class = "input_node_size")
  expect_no_error(plot_dag(dag1, node_size = 1.2))
  expect_no_error(plot_dag(dag1, node_size = 12L))

  expect_error(plot_dag(dag1, node_stroke = NA), class = "input_node_stroke")
  expect_error(plot_dag(dag1, node_stroke = NULL), class = "input_node_stroke")
  expect_error(plot_dag(dag1, node_stroke = ""), class = "input_node_stroke")
  expect_error(plot_dag(dag1, node_stroke = c(1,2)), class = "input_node_stroke")
  expect_no_error(plot_dag(dag1, node_stroke = 1.2))
  expect_no_error(plot_dag(dag1, node_stroke = 12L))

  expect_error(plot_dag(dag1, e_w = NA), class = "input_e_w")
  expect_error(plot_dag(dag1, e_w = NULL), class = "input_e_w")
  expect_error(plot_dag(dag1, e_w = ""), class = "input_e_w")
  expect_error(plot_dag(dag1, e_w = c(1,2)), class = "input_e_w")
  expect_no_error(plot_dag(dag1, e_w = 1.2))
  expect_no_error(plot_dag(dag1, e_w = 12L))

  expect_error(plot_dag(dag1, cap_mm = NA), class = "input_cap_mm")
  expect_error(plot_dag(dag1, cap_mm = NULL), class = "input_cap_mm")
  expect_error(plot_dag(dag1, cap_mm = ""), class = "input_cap_mm")
  expect_error(plot_dag(dag1, cap_mm = c(1,2)), class = "input_cap_mm")
  expect_no_error(plot_dag(dag1, cap_mm = 1.2))
  expect_no_error(plot_dag(dag1, cap_mm = 12L))

  expect_error(plot_dag(dag1, scc = NA), class = "input_scc")
  expect_error(plot_dag(dag1, scc = NULL), class = "input_scc")
  expect_error(plot_dag(dag1, scc = ""), class = "input_scc")
  expect_error(plot_dag(dag1, scc = c(TRUE,FALSE)), class = "input_scc")
  expect_no_error(plot_dag(dag1, scc = T))
  expect_no_error(plot_dag(dag1, scc = FALSE))

  expect_error(plot_dag(dag1, scc_size = c(1,NA)), class = "input_scc_size")
  expect_error(plot_dag(dag1, scc_size = NULL), class = "input_scc_size")
  expect_error(plot_dag(dag1, scc_size = c("2","1")), class = "input_scc_size")
  expect_error(plot_dag(dag1, scc_size = 1), class = "input_scc_size")
  expect_error(plot_dag(dag1, scc_size = c(1,2,3)), class = "input_scc_size")
  expect_no_error(plot_dag(dag1, scc_size = c(1.2,12L)))

  expect_error(plot_dag(dag1, scc_shift = c(1,NA)), class = "input_scc_shift")
  expect_error(plot_dag(dag1, scc_shift = NULL), class = "input_scc_shift")
  expect_error(plot_dag(dag1, scc_shift = c("2","1")), class = "input_scc_shift")
  expect_error(plot_dag(dag1, scc_shift = 1), class = "input_scc_shift")
  expect_error(plot_dag(dag1, scc_shift = c(1,2,3)), class = "input_scc_shift")
  expect_no_error(plot_dag(dag1, scc_shift = c(1.2,12L)))

  expect_error(plot_dag(dag1, scc_angle = NA), class = "input_scc_angle")
  expect_error(plot_dag(dag1, scc_angle = NULL), class = "input_scc_angle")
  expect_error(plot_dag(dag1, scc_angle = ""), class = "input_scc_angle")
  expect_error(plot_dag(dag1, scc_angle = c(1,2)), class = "input_scc_angle")
  expect_no_error(plot_dag(dag1, scc_angle = 1.2))
  expect_no_error(plot_dag(dag1, scc_angle = 12L))
})

test_that("plot_dag allows NULL inputs for the correct set of parameters", {
  dag1 <- dagitty::dagitty('dag {V1 -> V2 ; V2 -> V3}')
  expect_no_error(plot_dag(dag1, node_outc = NULL))
  expect_no_error(plot_dag(dag1, node_expo = NULL))
  expect_no_error(plot_dag(dag1, node_adj = NULL))
  expect_no_error(plot_dag(dag1, node_latent = NULL))
  expect_no_error(plot_dag(dag1, path_causal = NULL))
  expect_no_error(plot_dag(dag1, path_biased = NULL))
  expect_no_error(plot_dag(dag1, label = NULL))
  expect_no_error(plot_dag(dag1, label_shift = NULL))
})

test_that("plot_dag calls that are expect to work do actually work", {
  dag_x <- dagitty::dagitty('dag {bb="0,0,1,1"
    C1 [pos="0.312,0.129"]
    C2 [pos="0.313,0.399"]
    O [outcome,pos="0.394,0.254"]
    V1 [pos="0.223,0.256"]
    C1 -> O ; C1 -> V1 ; C2 -> O ; C2 -> V1 ; V1 -> O}')
  expect_no_error(plot_dag(dag_x, node_expo = "V1", node_adj = c("C1","C2"), path_causal = "V1 -> O",
                           label = c(V1 = "Expo"), label_shift = list(outcome = c(0.01,0), exposure = c(0,0.015)),
                           cap_mm = 10, e_w = 1, node_stroke = 2, label_size = 3, node_size = 8))
  expect_no_error(plot_dag(dag_x, node_expo = "V1", label_shift = list(all = c(0,0.04)), node_latent = "C2", path_biased = c("C2->V1","C2->O"),
                           e_w = 1, scc = T, scc_size = c(0.11,0.035), scc_shift = c(-0.003,-0.055), scc_angle = 5))
})
