#' Transform SCC to DAG
#'
#' Creates an object of class `dagitty` (`dagitty` package) from a SCC model, following VanderWeele and Robins (2007).
#'
#' @param scc An object of class `epicmodel_scc`.
#' @param unknown TRUE (default) or FALSE. If TRUE, unknown causes are added to the SCC model: every sufficient cause gets an additional
#' individual unknown component cause representing additional unknown components; an unknown sufficient cause is added to the model consisting
#' of a single unknown component cause and representing all unknown sufficient causes.
#'
#' @return A list of length 2 containing an object of class `dagitty` (named `dag`) and a data.frame containing the information, which label in the
#' DAG corresponds to which component cause (named `legend`).
#'
#' @seealso
#' * [dagitty::dagitty()]
#' * \code{\link[=new_scc]{SCC models}} for more information on unknown causes and SCC models in general
#' * [plot_dag()] to create a `ggplot` object from `dagitty` model code
#'
#' @references VanderWeele TJ, Robins JM (2007): Directed acyclic graphs, sufficient causes, and the properties of conditioning
#' on a common effect. American Journal of Epidemiology 166 (9): 1096–1104.
#'
#' @export
#'
#' @examples
#' # Create some SCC model
#' steplist_checked <- check_steplist(steplist_rain)
#' scc_model <- create_scc(steplist_checked)
#'
#' # Transform it into a DAG
#' scc_to_dag(scc_model)
scc_to_dag <- function(scc, unknown = TRUE) {
  # Check input
  if (inherits(scc, "epicmodel_scc") %>% magrittr::not()) {
    cli::cli_abort("{.var scc} must be a {.emph epicmodel_scc} class object!",
                   class = "no_scc")
  }

  rlang::try_fetch(checkmate::assert_logical(unknown, any.missing = F, len = 1, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort("{.var unknown} must be TRUE or FALSE!",
                                    parent = cnd, class = "input_unknown")
                   })
  #=============================================================================
  # Get cause sets
  sc <- scc_cause_sets(scc, output = "desc_no_start", depends = TRUE, unknown = FALSE)
  ## Create labels
  cc_unique <- sc %>% unlist() %>% unique()
  cc_unique %<>% cbind(.,paste0("CC",c(1:length(.)))) %>% as.data.frame() %>% magrittr::set_colnames(c("desc","label"))

  # Edges
  edges <- vector(mode = "list", length = length(sc))
  for (i in 1:length(sc)) {
    edges_temp <- rep(NA_character_, length(sc[[i]]))
    for (j in 1:length(sc[[i]])) {
      label_temp <- cc_unique$label[cc_unique$desc == sc[[i]][j]]
      edges_temp[j] <- paste0(label_temp," -> SC",i)
    }
    if (unknown) {edges_temp %<>% c(.,paste0("U_SC",i," -> SC",i))}
    edges[[i]] <- edges_temp
  }
  edges %<>% unlist()
  if (unknown) {edges %<>% c(.,"U_USC -> USC")}

  edges_sc <- paste0("SC",c(1:length(sc))," -> O")
  if (unknown) {edges_sc %<>% c(.,"USC -> O")}
  edges_final <- c(edges, edges_sc)

  # Coordinates
  ## x
  x_coord_cc <- rep(1, nrow(cc_unique)) %>% magrittr::set_names(cc_unique$label)
  if (unknown) {x_coord_cc %<>% c(.,rep(2, length(sc) + 1) %>% magrittr::set_names(c(paste0("U_SC",c(1:length(sc))),"U_USC")))}
  x_coord_sc <- rep((2 + unknown), length(sc)) %>% magrittr::set_names(paste0("SC",c(1:length(sc))))
  if (unknown) {x_coord_sc %<>% c(.,USC = 3)}
  x_coord_sc %<>% c(.,O = (3 + unknown))
  x_coord <- c(x_coord_cc, x_coord_sc)
  ## y
  y_max <- nrow(cc_unique)
  y_coord_cc <- seq(0.25, (y_max / 2) - 0.25, length.out = y_max) %>% magrittr::set_names(cc_unique$label)
  if (unknown) {
    y_coord_cc %<>% c(.,seq(0, y_max / 2, length.out = length(sc) + 1) %>% magrittr::set_names(c(paste0("U_SC",c(1:length(sc))),"U_USC")))
    y_coord_sc <- seq(0, y_max / 2, length.out = length(sc) + 1) %>% magrittr::set_names(c(paste0("SC",c(1:length(sc))), "USC"))
    y_coord_sc %<>% c(.,O = (y_max / 4))
  } else {
    y_coord_sc <- seq(0, y_max / 2, length.out = length(sc)) %>% magrittr::set_names(paste0("SC",c(1:length(sc))))
    y_coord_sc %<>% c(.,O = (y_max / 4))
  }
  y_coord <- c(y_coord_cc, y_coord_sc)

  ## Finalize
  dag_string <- paste0("dag{", edges_final %>% stringr::str_c(collapse = " ; ")," O [outcome]}")
  dag <- dagitty::dagitty(dag_string)
  dagitty::coordinates(dag) <- list(x = x_coord, y = y_coord)
  out <- list(dag = dag, legend = cc_unique)
  #=============================================================================
  # Check output
  rlang::try_fetch({
      checkmate::assert_list(out, any.missing = F, null.ok = F, len = 2, names = "unique")
      checkmate::assert_set_equal(names(out), c("dag","legend"), ordered = T)
    }, error = function(cnd) {cli::cli_abort(c("Output validation error: {.var out}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output_out")
  })

  rlang::try_fetch({
      checkmate::assert_class(out$dag, "dagitty", null.ok = F)
    }, error = function(cnd) {cli::cli_abort(c("Output validation error: {.var out$dag}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output_dag")
  })

  rlang::try_fetch({
      checkmate::assert_data_frame(out$legend, types = "character", any.missing = F, null.ok = F, ncols = 2, min.rows = 1)
    }, error = function(cnd) {cli::cli_abort(c("Output validation error: {.var out$legend}",
                                               "i" = "The cause is probably a bug in the {.pkg epicmodel} package. Please report it on github!"),
                                             parent = cnd, class = "output_legend")
  })
  #=============================================================================
  return(out)
}

#' Plot DAG
#'
#' Creates a ggplot from a `dagitty` object, using packages `dagitty` and `ggdag`. Mimics format and colors used on the `dagitty` homepage
#' www.dagitty.net. Please note the recommendation in argument `label_shift` below: Getting the values for `label_shift` right can be an iterative
#' and slightly tedious procedure. It is highly recommended to evaluate the result of the current values already in the saved plot using,
#' e.g., `ggsave` and not in the RStudio Viewer.
#'
#' @param dag An object of class `dagitty`. Can be created by using `dagitty::dagitty('[model_code]')` or [epicmodel::scc_to_dag()]. If your DAG has
#' been created by [epicmodel::scc_to_dag()], make sure to pass only the first element (named `dag`) to `plot_dag`.
#' @param node_outc A single element of type character or NULL (default). If the outcome has not yet been specified in `dag`, it can be done here by
#'   specifiying the name of the corresponding node.
#' @param node_expo A single element of type character or NULL (default). If the exposure has not yet been specified in `dag`, it can be done here
#' by specifiying the name of the corresponding node.
#' @param node_adj A character vector or NULL (default). Specify the names of nodes that should be defined as "adjusted".
#' @param node_latent A character vector or NULL (default). Specify the names of nodes that should be defined as "latent".
#' @param path_causal A character vector or NULL (default). Specify the names of the paths in format "V1->V2" that should be defined as "causal".
#' @param path_biased A character vector or NULL (default). Specify the names of the paths in format "V1->V2" that should be defined as "biased".
#' @param label A named character vector or NULL (default). Change the name of nodes in the graph, i.e., labels. The vector elements correspond to
#' the new names, the vector names correspond to the old node names, i.e., `label = c(old_name = "new_name").`
#' @param label_shift A named list (with all elements being numerical vectors of length 2) or NULL (default). Numerical values are used to move the
#' labels of the corresponding nodes in x and y direction, respectively. The list names correspond to the nodes to which the values apply. Possible
#' list names are the node names (initial names prior to changing them via `label`), node types, i.e., `outcome`, `exposure`, `adjusted`, `latent`,
#' and `other`, as well as `all`, which applies to all nodes. If a node is addressed by several entries, e.g., its name and `all`, all entries are
#' summed up. See the example below. Getting the values for `label_shift` right can be an iterative and slightly tedious procedure. It is highly
#' recommended to evaluate the result of the current values already in the saved plot using, e.g., `ggsave` and not in the RStudio Viewer.
#' @param label_size A single numeric value, which controls the font size of the label. Default is 2.5.
#' @param node_size A single numeric value, which controls the size of the circle that represents the node. Default is 7.
#' @param node_stroke A single numeric value, which controls the size of the black border around the node circles. Default is 1.
#' @param e_w A single numeric value, which controls edge width. Default is 0.4.
#' @param cap_mm A single numeric value, which controls the distance, i.e., white space, between when the node ends and the edge begins/ the edge
#' ends and the node begins. Higher values correspond to shorter edges/arrows. Default is 4.
#' @param scc TRUE or FALSE (default). Only applies to DAGs that are based on sufficient-component cause (SCC) models. If TRUE, an ellipse is
#' added to the DAG, which should surround all sufficient cause variables, if they are a determinative set of sufficient causes, as suggested by
#' VanderWeele and Robins (2007). If the DAG is not based on a SCC, leave `scc` at FALSE.
#' @param scc_size A numeric vector of length 2, which controls the size of the ellipse. Default is c(0.1, 0.35).
#' @param scc_shift A numeric vector of length 2, which controls the shift of the complete ellipse in x and y direction. Default is c(0, 0).
#' @param scc_angle A single numeric value, which controls rotiation of the ellipse in degree units. Default is 0.
#'
#' @returns A `ggplot` object.
#'
#' @seealso
#' * [dagitty::dagitty()]
#' * [epicmodel::scc_to_dag()] for creating DAGs from SCC models
#' * \code{\link[=new_scc]{SCC models}} for more information on SCC models
#'
#' @references VanderWeele TJ, Robins JM (2007): Directed acyclic graphs, sufficient causes, and the properties of conditioning on
#' a common effect. American Journal of Epidemiology 166 (9): 1096–1104.
#'
#' @export
#'
#' @examples
#' # Create some SCC model
#' steplist_checked <- check_steplist(steplist_rain)
#' scc_model <- create_scc(steplist_checked)
#'
#' # Transform it into a DAG
#' dag <- scc_to_dag(scc_model)[["dag"]]
#'
#' # Plot DAG
#' plot_dag(dag, label_shift = list(all = c(0,0.15), outcome = c(0.05,0)))
#'
#' # plot_dag() works also with dagitty objects created in other ways
#' dag_to_plot <- dagitty::dagitty('dag {
#' bb="-2.628,-2.412,2.659,2.378"
#' V1 [pos="-2.128,-1.912"]
#' V2 [pos="-0.031,0.035"]
#' V3 [pos="2.159,1.878"]
#' V1 -> V2
#' V2 -> V3
#' }')
#' plot_dag(dag_to_plot, node_outc = "V3", node_expo = "V1", label = c(V3 = "outcome"), label_shift = list(all = c(0,0.15)))
plot_dag <- function(dag, node_outc = NULL, node_expo = NULL, node_adj = NULL, node_latent = NULL,
                     path_causal = NULL, path_biased = NULL,
                     label = NULL, label_shift = NULL, label_size = 2.5,
                     node_size = 7, node_stroke = 1, e_w = 0.4, cap_mm = 4,
                     scc = FALSE, scc_size = c(0.1,0.35), scc_shift = c(0,0), scc_angle = 0){
  # Check package availability (ggdag, ggforce, ggraph are only used here)
  rlang::check_installed(c("ggdag", "ggforce", "ggraph"))

  # Check input
  if (inherits(dag, "dagitty") %>% magrittr::not()) {
    if (is.list(dag)) {
      if (names(dag) %>% magrittr::equals(c("dag","legend")) %>% all_true()) {
        cli::cli_abort(c("{.var dag} must be a {.emph dagitty} class object!",
                         "i" = "If you used {.fn epicmodel::scc_to_dag} to create the DAG, please only input the element {.var dag}.",
                         "i" = "{.code test_dag <- scc_to_dag(scc)}",
                         "i" = "{.code plot_dag(dag = test_dag$dag)}"),
                       class = "scc_to_dag_complete_list")
      }
    }
    cli::cli_abort(c("{.var dag} must be a {.emph dagitty} class object!",
                   "i" = "Use {.fn dagitty::dagitty} or {.fn epicmodel::scc_to_dag} to create {.emph dagitty} class objects."),
                   class = "no_dag")
  }

  rlang::try_fetch(checkmate::assert_character(node_outc, any.missing = F, len = 1, min.chars = 1, null.ok = T),
                   error = function(cnd) {
                     cli::cli_abort("{.var node_outc} must be a single element of type character!", parent = cnd, class = "input_node_outc")
                   })

  rlang::try_fetch(checkmate::assert_character(node_expo, any.missing = F, len = 1, min.chars = 1, null.ok = T),
                   error = function(cnd) {
                     cli::cli_abort("{.var node_expo} must be a single element of type character!", parent = cnd, class = "input_node_expo")
                   })

  rlang::try_fetch(checkmate::assert_character(node_adj, any.missing = F, min.len = 1, min.chars = 1, null.ok = T),
                   error = function(cnd) {
                     cli::cli_abort("{.var node_adj} must be a character vector!", parent = cnd, class = "input_node_adj")
                   })

  rlang::try_fetch(checkmate::assert_character(node_latent, any.missing = F, min.len = 1, min.chars = 1, null.ok = T),
                   error = function(cnd) {
                     cli::cli_abort("{.var node_latent} must be a character vector!", parent = cnd, class = "input_node_latent")
                   })

  rlang::try_fetch(checkmate::assert_character(path_causal, any.missing = F, min.len = 1, min.chars = 1, null.ok = T),
                   error = function(cnd) {
                     cli::cli_abort("{.var path_causal} must be a character vector!", parent = cnd, class = "input_path_causal")
                   })

  if (!is.null(path_causal)) {path_causal %<>% stringr::str_replace_all(" ","")}

  rlang::try_fetch(checkmate::assert_character(path_biased, any.missing = F, min.len = 1, min.chars = 1, null.ok = T),
                   error = function(cnd) {
                     cli::cli_abort("{.var path_biased} must be a character vector!", parent = cnd, class = "input_path_biased")
                   })

  if (!is.null(path_biased)) {path_biased %<>% stringr::str_replace_all(" ","")}

  rlang::try_fetch(checkmate::assert_character(label, any.missing = F, min.len = 1, min.chars = 1, null.ok = T, names = "named"),
                   error = function(cnd) {
                     cli::cli_abort("{.var label} must be a named character vector!", parent = cnd, class = "input_label")
                   })

  rlang::try_fetch(checkmate::assert_list(label_shift, any.missing = F, null.ok = T, names = "named", types = "numeric"),
                   error = function(cnd) {
                     cli::cli_abort("{.var label_shift} must be a named list containing numeric vectors of length 2!",
                                    parent = cnd, class = "input_label_shift")
                   })

  if (!is.null(label_shift)) {
    rlang::try_fetch({
      for (i in 1:length(label_shift)) {
        checkmate::assert_numeric(label_shift[[i]], any.missing = F, len = 2, null.ok = F)
      }
    },error = function(cnd) {
      cli::cli_abort("{.var label_shift} must be a named list containing numeric vectors of length 2!",
                     parent = cnd, class = "input_label_shift_elements")
    })
  }

  rlang::try_fetch(checkmate::assert_number(label_size, na.ok = F, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort("{.var label_size} must be a single numeric value!", parent = cnd, class = "input_label_size")
                   })

  rlang::try_fetch(checkmate::assert_number(node_size, na.ok = F, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort("{.var node_size} must be a single numeric value!", parent = cnd, class = "input_node_size")
                   })

  rlang::try_fetch(checkmate::assert_number(node_stroke, na.ok = F, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort("{.var node_stroke} must be a single numeric value!", parent = cnd, class = "input_node_stroke")
                   })

  rlang::try_fetch(checkmate::assert_number(e_w, na.ok = F, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort("{.var e_w} must be a single numeric value!", parent = cnd, class = "input_e_w")
                   })

  rlang::try_fetch(checkmate::assert_number(cap_mm, na.ok = F, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort("{.var cap_mm} must be a single numeric value!", parent = cnd, class = "input_cap_mm")
                   })

  rlang::try_fetch(checkmate::assert_logical(scc, any.missing = F, len = 1, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort("{.var scc} must be TRUE or FALSE!", parent = cnd, class = "input_scc")
                   })

  rlang::try_fetch(checkmate::assert_numeric(scc_size, any.missing = F, len = 2, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort("{.var scc_size} must be a numeric vector of length 2!", parent = cnd, class = "input_scc_size")
                   })

  rlang::try_fetch(checkmate::assert_numeric(scc_shift, any.missing = F, len = 2, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort("{.var scc_shift} must be a numeric vector of length 2!", parent = cnd, class = "input_scc_shift")
                   })

  rlang::try_fetch(checkmate::assert_number(scc_angle, na.ok = F, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort("{.var scc_angle} must be a single numeric value!", parent = cnd, class = "input_scc_angle")
                   })

  # Create tidy_dag
  tidy_dag <- ggdag::tidy_dagitty(dag)
  tidy_dag %<>% ggdag::node_status()
  tidy_dag[[1]]$status %<>% as.character()
  tidy_dag[[1]]$path_names <- paste0(tidy_dag[[1]]$name,tidy_dag[[1]]$direction,tidy_dag[[1]]$to)
  tidy_dag[[1]]$path_names[tidy_dag[[1]][,c("name","direction","to")] %>% is.na() %>% rowSums() %>% magrittr::is_greater_than(0)] <- NA

  nodes_adj_latent <- tidy_dag[[1]]$name[is.na(tidy_dag[[1]]$status)] %>% unique()
  paths_causal_biased <- tidy_dag[[1]]$path_names[!is.na(tidy_dag[[1]]$path_names)] %>% unique()

  # Continue checking input
  if (!is.null(node_outc)) {
    if ("outcome" %>% magrittr::is_in(tidy_dag[[1]]$status)) {
      cli::cli_abort(c("The outcome has already been specified in the DAG!",
                       "i" = "Change {.var node_outc} to NULL or change the {.var dag} accordingly!"), class = "outc_exists")
    }
    if (node_outc %>% magrittr::is_in(nodes_adj_latent) %>% magrittr::not()) {
      cli::cli_abort(c("{.var node_outc} must be an existing node from the DAG!",
                       "i" = "Here is a list of all valid nodes: {nodes_adj_latent %>% stringr::str_c(collapse = ', ')}"),
                     class = "input_node_outc_choices")
    }
  }

  if (!is.null(node_expo)) {
    if ("exposure" %>% magrittr::is_in(tidy_dag[[1]]$status)) {
      cli::cli_abort(c("The exposure has already been specified in the DAG!",
                       "i" = "Change {.var node_expo} to NULL or change the {.var dag} accordingly!"), class = "expo_exists")
    }
    if (node_expo %>% magrittr::is_in(nodes_adj_latent) %>% magrittr::not()) {
      cli::cli_abort(c("{.var node_expo} must be an existing node from the DAG!",
                       "i" = "Here is a list of all valid nodes: {nodes_adj_latent %>% stringr::str_c(collapse = ', ')}"),
                     class = "input_node_expo_choices")
    }
  }

  if (!is.null(node_outc) & !is.null(node_expo)) {
    if (node_outc == node_expo) {
      cli::cli_abort("Outcome and exposure must not be the same node!", class = "outc_equals_expo")
    }
  }

  if (!is.null(node_adj) & !is.null(node_outc)) {
    if (node_outc %>% magrittr::is_in(node_adj)) {
      cli::cli_abort("{.var node_outc} must not be part of {.var node_adj}!", class = "outc_in_adj")
    }
  }

  if (!is.null(node_adj) & !is.null(node_expo)) {
    if (node_expo %>% magrittr::is_in(node_adj)) {
      cli::cli_abort("{.var node_expo} must not be part of {.var node_adj}!", class = "expo_in_adj")
    }
  }

  if (!is.null(node_adj)) {
    if (node_adj %>% magrittr::is_in(nodes_adj_latent) %>% all_true() %>% magrittr::not()) {
      invalid_node_adj <- node_adj[!(node_adj %in% nodes_adj_latent)] %>% stringr::str_c(collapse = ", ")
      cli::cli_abort(c("{.var node_adj} must only contain existing nodes from the DAG without assigned status!",
                       "i" = "The following elements from {.var node_adj} are not in the DAG or already have a status assigned: {invalid_node_adj}",
                       "i" = "Here is a list of all valid nodes: {nodes_adj_latent %>% stringr::str_c(collapse = ', ')}"),
                     class = "input_node_adj_choices")
    }
  }

  if (!is.null(node_latent) & !is.null(node_outc)) {
    if (node_outc %>% magrittr::is_in(node_latent)) {
      cli::cli_abort("{.var node_outc} must not be part of {.var node_latent}!", class = "outc_in_latent")
    }
  }

  if (!is.null(node_latent) & !is.null(node_expo)) {
    if (node_expo %>% magrittr::is_in(node_latent)) {
      cli::cli_abort("{.var node_expo} must not be part of {.var node_latent}!", class = "expo_in_latent")
    }
  }

  if (!is.null(node_latent)) {
    if (node_latent %>% magrittr::is_in(nodes_adj_latent) %>% all_true() %>% magrittr::not()) {
      invalid_node_latent <- node_latent[!(node_latent %in% nodes_adj_latent)] %>% stringr::str_c(collapse = ", ")
      cli::cli_abort(c("{.var node_latent} must only contain existing nodes from the DAG without assigned status!",
                       "i" = "The following elements from {.var node_latent} are not in the DAG or already have a status assigned:
                       {invalid_node_latent}",
                       "i" = "Here is a list of all valid nodes: {nodes_adj_latent %>% stringr::str_c(collapse = ', ')}"),
                     class = "input_node_latent_choices")
    }
  }

  if (!is.null(node_adj) & !is.null(node_latent)) {
    if (node_adj %>% magrittr::is_in(node_latent) %>% all_false() %>% magrittr::not()) {
      node_overlap <- node_adj[node_adj %>% magrittr::is_in(node_latent)] %>% stringr::str_c(collapse = ", ")
      cli::cli_abort(c("Nodes must not be part of both {.var node_adj} and {.var node_latent}!",
                       "i" = "The following nodes are part of both {.var node_adj} and {.var node_latent}: {node_overlap}"),
                     class = "node_adj_latent_overlap")
    }
  }

  if (!is.null(path_causal)) {
     if (path_causal %>% magrittr::is_in(paths_causal_biased) %>% all_true() %>% magrittr::not()) {
      invalid_path_causal <- path_causal[!(path_causal %in% paths_causal_biased)] %>% stringr::str_c(collapse = ", ")
      cli::cli_abort(c("{.var path_causal} must only contain existing paths from the DAG!",
                       "i" = "The following elements from {.var path_causal} are not in the DAG: {invalid_path_causal}",
                       "i" = "Here is a list of all valid paths: {paths_causal_biased %>% stringr::str_c(collapse = ', ')}"),
                        class = "input_path_causal_choices")
                     }
  }

  if (!is.null(path_biased)) {
    if (path_biased %>% magrittr::is_in(paths_causal_biased) %>% all_true() %>% magrittr::not()) {
      invalid_path_biased <- path_biased[!(path_biased %in% paths_causal_biased)] %>% stringr::str_c(collapse = ", ")
      cli::cli_abort(c("{.var path_biased} must only contain existing paths from the DAG!",
                       "i" = "The following elements from {.var path_biased} are not in the DAG: {invalid_path_biased}",
                       "i" = "Here is a list of all valid paths: {paths_causal_biased %>% stringr::str_c(collapse = ', ')}"),
                     class = "input_path_biased_choices")
    }
  }

  if (!is.null(path_causal) & !is.null(path_biased)) {
    if (path_causal %>% magrittr::is_in(path_biased) %>% all_false() %>% magrittr::not()) {
      path_overlap <- path_causal[path_causal %>% magrittr::is_in(path_biased)] %>% stringr::str_c(collapse = ", ")
      cli::cli_abort(c("Paths must not be part of both {.var path_causal} and {.var path_biased}!",
                       "i" = "The following paths are part of both {.var path_causal} and {.var path_biased}: {path_overlap}"),
                     class = "path_causal_biased_overlap")
    }
  }

  if (!is.null(label)) {
    rlang::try_fetch(checkmate::assert_subset(names(label), tidy_dag[[1]]$name, empty.ok = F),
                     error = function(cnd) {
                       cli::cli_abort("The names of {.var label} must be node names!", parent = cnd, class = "label_names")
                     })
    if (names(label) %>% duplicated() %>% all_false() %>% magrittr::not()) {
      label_names_dupli <- names(label)[names(label) %>% duplicated()] %>% unique()
      cli::cli_abort(c("You try to change the {.var label} for the same node multiple times!",
                       "i" = "The following nodes appear more than once: {label_names_dupli}"
                     ), class = "label_names_dupli")
    }
  }

  if (!is.null(label_shift)) {
    rlang::try_fetch(checkmate::assert_subset(names(label_shift),
                                              c("exposure","outcome","adjusted","latent","other","all",tidy_dag[[1]]$name), empty.ok = F),
                     error = function(cnd) {
                       cli::cli_abort(c("The names of {.var label_shift} must be node names, node types, or 'all'!",
                                        "i" = "Valid node types are 'exposure', 'outcome', 'adjusted', 'latent', 'other'.",
                                        "i" = "Values specified for 'all', the node type, and the individual node are all added up."),
                                      parent = cnd, class = "label_shift_names")
                     })
    if (names(label_shift) %>% duplicated() %>% all_false() %>% magrittr::not()) {
      label_shift_names_dupli <- names(label_shift)[names(label_shift) %>% duplicated()] %>% unique()
      cli::cli_abort(c("You try to use the same name in {.var label_shift} multiple times!",
                       "i" = "The following names appear more than once: {label_shift_names_dupli}"
      ), class = "label_shift_names_dupli")
    }
  }

  # Continue preparing tidy_dag
  tidy_dag[[1]]$status %<>% stringr::str_to_title()
  tidy_dag[[1]]$status %<>% factor(levels = c("Outcome","Exposure","Adjusted","Latent","Other"))
  if (!is.null(node_outc)) {tidy_dag[[1]]$status[tidy_dag[[1]]$name %in% node_outc] <- "Outcome"}
  if (!is.null(node_expo)) {tidy_dag[[1]]$status[tidy_dag[[1]]$name %in% node_expo] <- "Exposure"}
  if (!is.null(node_adj)) {tidy_dag[[1]]$status[tidy_dag[[1]]$name %in% node_adj] <- "Adjusted"}
  if (!is.null(node_latent)) {tidy_dag[[1]]$status[tidy_dag[[1]]$name %in% node_latent] <- "Latent"}
  tidy_dag[[1]]$status[is.na(tidy_dag[[1]]$status)] <- "Other"

  # Add path status
  tidy_dag[[1]]$paths <- "Other"
  if (!is.null(path_causal)) {tidy_dag[[1]]$paths[tidy_dag[[1]]$path_names %in% path_causal] <- "Causal"}
  if (!is.null(path_biased)) {tidy_dag[[1]]$paths[tidy_dag[[1]]$path_names %in% path_biased] <- "Biasing"}
  tidy_dag[[1]]$paths %<>% factor(levels = c("Causal","Biasing","Other"))

  # Dummy value for path legend and path coordinate defaults
  dum <- mean(mean(tidy_dag[[1]]$x),mean(tidy_dag[[1]]$y))

  # Subset x, xend, y, yend
  ## cp: causal path
  tidy_dag[[1]]$x_cp <- dum
  tidy_dag[[1]]$x_cp[tidy_dag[[1]]$paths == "Causal"] <- tidy_dag[[1]]$x[tidy_dag[[1]]$paths == "Causal"]
  tidy_dag[[1]]$y_cp <- dum
  tidy_dag[[1]]$y_cp[tidy_dag[[1]]$paths == "Causal"] <- tidy_dag[[1]]$y[tidy_dag[[1]]$paths == "Causal"]
  tidy_dag[[1]]$xend_cp <- dum
  tidy_dag[[1]]$xend_cp[tidy_dag[[1]]$paths == "Causal"] <- tidy_dag[[1]]$xend[tidy_dag[[1]]$paths == "Causal"]
  tidy_dag[[1]]$yend_cp <- dum
  tidy_dag[[1]]$yend_cp[tidy_dag[[1]]$paths == "Causal"] <- tidy_dag[[1]]$yend[tidy_dag[[1]]$paths == "Causal"]
  ## bp: biasing path
  tidy_dag[[1]]$x_bp <- dum
  tidy_dag[[1]]$x_bp[tidy_dag[[1]]$paths == "Biasing"] <- tidy_dag[[1]]$x[tidy_dag[[1]]$paths == "Biasing"]
  tidy_dag[[1]]$y_bp <- dum
  tidy_dag[[1]]$y_bp[tidy_dag[[1]]$paths == "Biasing"] <- tidy_dag[[1]]$y[tidy_dag[[1]]$paths == "Biasing"]
  tidy_dag[[1]]$xend_bp <- dum
  tidy_dag[[1]]$xend_bp[tidy_dag[[1]]$paths == "Biasing"] <- tidy_dag[[1]]$xend[tidy_dag[[1]]$paths == "Biasing"]
  tidy_dag[[1]]$yend_bp <- dum
  tidy_dag[[1]]$yend_bp[tidy_dag[[1]]$paths == "Biasing"] <- tidy_dag[[1]]$yend[tidy_dag[[1]]$paths == "Biasing"]
  ## op: other path
  tidy_dag[[1]]$x_op <- dum
  tidy_dag[[1]]$x_op[tidy_dag[[1]]$paths == "Other"] <- tidy_dag[[1]]$x[tidy_dag[[1]]$paths == "Other"]
  tidy_dag[[1]]$y_op <- dum
  tidy_dag[[1]]$y_op[tidy_dag[[1]]$paths == "Other"] <- tidy_dag[[1]]$y[tidy_dag[[1]]$paths == "Other"]
  tidy_dag[[1]]$xend_op <- dum
  tidy_dag[[1]]$xend_op[tidy_dag[[1]]$paths == "Other"] <- tidy_dag[[1]]$xend[tidy_dag[[1]]$paths == "Other"]
  tidy_dag[[1]]$yend_op <- dum
  tidy_dag[[1]]$yend_op[tidy_dag[[1]]$paths == "Other"] <- tidy_dag[[1]]$yend[tidy_dag[[1]]$paths == "Other"]

  # Define colors
  col_nodes <- c("Exposure" = "#CCDC49", "Outcome" = "#47B9E4", "Adjusted" = "#FFFFFF", "Latent" = "#777777", "Other" = "#CECECE")
  col_paths <- c("Causal" = "#48A820", "Biasing" = "#DA3E9E", "Other" = "#000000")

  # Create labels
  lab_nodes <- tidy_dag[[1]][,c("name","x","y","status")] %>% unique()
  ## Change posisiton based on input
  if (!is.null(label_shift)) {
    for (i in 1:length(label_shift)) {
      if (names(label_shift)[i] == "all") {
        lab_nodes$x %<>% magrittr::add(label_shift[[names(label_shift)[i]]][1])
        lab_nodes$y %<>% magrittr::add(label_shift[[names(label_shift)[i]]][2])
      }
      if (names(label_shift)[i] %in% c("exposure","outcome","adjusted","latent","other")) {
        lab_nodes$x[lab_nodes$status == names(label_shift)[i] %>% stringr::str_to_title()] %<>%
          magrittr::add(label_shift[[names(label_shift)[i]]][1])
        lab_nodes$y[lab_nodes$status == names(label_shift)[i] %>% stringr::str_to_title()] %<>%
          magrittr::add(label_shift[[names(label_shift)[i]]][2])
      }
      if (names(label_shift)[i] %in% lab_nodes$name) {
        lab_nodes$x[lab_nodes$name == names(label_shift)[i]] %<>% magrittr::add(label_shift[[names(label_shift)[i]]][1])
        lab_nodes$y[lab_nodes$name == names(label_shift)[i]] %<>% magrittr::add(label_shift[[names(label_shift)[i]]][2])
      }
    }
  }
  ## Change labels based on input
  if (!is.null(label)) {
    for (i in 1:nrow(lab_nodes)) {
      if (lab_nodes$name[i] %in% names(label)) {
        lab_nodes$name[i] <- label[lab_nodes$name[i]]
      }
    }
  }

  # Plot
  plot <- tidy_dag %>% ggplot2::ggplot(ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend)) +
    ggdag::geom_dag_point(ggplot2::aes(fill = .data$status), size = node_size, shape = "circle filled", stroke = node_stroke) +
    ggdag::geom_dag_edges(ggplot2::aes(x = .data$x_op, xend = .data$xend_op, y = .data$y_op, yend = .data$yend_op), edge_width = e_w,
                          edge_color = col_paths["Other"], start_cap = ggraph::circle(cap_mm,'mm'),end_cap = ggraph::circle(cap_mm,'mm')) +
    ggdag::geom_dag_edges(ggplot2::aes(x = .data$x_bp, xend = .data$xend_bp, y = .data$y_bp, yend = .data$yend_bp), edge_width = e_w,
                          edge_color = col_paths["Biasing"], start_cap = ggraph::circle(cap_mm,'mm'), end_cap = ggraph::circle(cap_mm,'mm')) +
    ggdag::geom_dag_edges(ggplot2::aes(x = .data$x_cp, xend = .data$xend_cp, y = .data$y_cp, yend = .data$yend_cp), edge_width = e_w,
                          edge_color = col_paths["Causal"], start_cap = ggraph::circle(cap_mm,'mm'), end_cap = ggraph::circle(cap_mm,'mm')) +
    ggplot2::scale_fill_manual(name = "Variables", values = col_nodes) +
    ggplot2::geom_line(ggplot2::aes(x = dum, y = dum, colour = .data$paths), linewidth = 2) + #add line of length 0 to enable legend for paths
    ggplot2::scale_colour_manual(name = "Paths", values = col_paths) + #color line of length 0 to enable legend for paths
    ggdag::theme_dag(legend.text = ggplot2::element_text(size = 8), legend.title = ggplot2::element_text(size = 10)) +
    ggplot2::scale_y_reverse()

  # Annotate labels
  for (i in 1:nrow(lab_nodes)) {
    plot <- plot + ggplot2::annotate(geom = "label", x = lab_nodes$x[i], y = lab_nodes$y[i], label = lab_nodes$name[i], size = label_size,
                                     label.r = grid::unit(0.4,"lines"), label.size = NA)
  }

  # Annotate SCC ellipse
  if (scc == T) {
    plot <- plot + ggforce::geom_ellipse(ggplot2::aes(x0 = (dum + scc_shift[1]), y0 = (dum + scc_shift[2]), a = scc_size[1], b = scc_size[2],
                                                      angle = scc_angle * (pi/180)), linewidth = e_w, color = "#CECECE")
  }

  # Return plot
  return(plot)
}
