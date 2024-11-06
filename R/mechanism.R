#' Investigate mechanisms
#'
#' Creates graphs that visiualize the mechanisms behind each sufficient cause using the `DiagrammeR` package.
#'
#' @details
#' ## The graphs
#' One graph per sufficient cause is created. The graphs display steps as nodes and IF/IFNOT relations as edges. Nodes will not be
#' labeled with their IDs or descriptions due to limited space, but with newly created labels. These labels are based on the type of node and are
#' listed together with the step description in the` legend` (accessed by `print()`). Step descriptions are also accessible via tooltips in the
#' graph. Just put your cursor on the node labels.
#'
#' There are 4 different types of nodes:
#'   * `Component causes`: Labeled `"CC"`, squares, gray border
#'   * `Interventions`: Labeled `"I"`, triangles, gray border
#'   * `End steps`: Labeled `"E"`, circles, black border
#'   * `Other steps`: Labeled `"S"`, circles, gray border
#'
#' There are 2 types of edges:
#'   * `IF conditions`: gray arrows
#'   * `IFNOT conditions`: red and T-shaped
#'
#' ## `epicmodel_mechanism` objects
#' `epicmodel_mechanism` objects are created by `mechanism()`. They are lists containing 2 elements:
#' \describe{
#'  \item{`legend`}{A data.frame with up to 3 variables:
#'   * `Label`: Contains the labels used in the graphs.
#'   * `Module`: Contains the name of the module to which this step belongs. Only available if `modules = TRUE` in `mechanism()` and if the SCC
#'      model actually uses modules (specified in element `sc_use_modules` of `epicmodel_scc` objects).
#'   * `Step`: A description of the corresponding step.}
#'
#'  \item{`graph`}{A list of length equal to the number of sufficient causes. Each element contains another list with 2 elements:
#'   * `ndf`: A data.frame containing information about nodes in the graph (see [DiagrammeR::node_aes()]).
#'   * `edf`: A data.frame containing information about edges in the graph (see [DiagrammeR::edge_aes()]).}
#'
#'  \item{`ndf`}{Data.frames containing the following variables:
#'   * `id`: Node ID used internally by `DiagrammeR` to define edges (`from` and `to` in `edf` data.frames).
#'   * `type`: Type of node as defined by `epicmodel`. Possible options are: `cc` (component cause), `int` (intervention), `end` (step that is part
#'      of an outcome definition), `other` (all other steps).
#'   * `label`: The label displayed in the graph and listed in variable `Label` of `legend`.
#'   * `tooltip`: The text displayed when putting the cursor on top of the node label. Corresponds to the step descriptions in variable `Step` of
#'     `legend`.
#'   * `shape`: The shape of the node. `square` for type `cc`, `triangle` for type `int`, and `circle` for types `end` and `other`.
#'   * `color`: Color of the node border. Gray for types `cc`, `int`, and `other`, and black for type `end`.
#'   * `fillcolor`: Color of the background, which is similar for all steps in the same module. If modules are not considered, `fillcolor` is white
#'     for all nodes.
#'   * `fontcolor`: Color of the node label. Always black.}
#'
#'  \item{`edf`}{Data.frames containing the following variables:
#'   * `id`: Edge ID used internally by `DiagrammeR`.
#'   * `from`: Node ID of the node from which the edge starts.
#'   * `to`: Node ID of the node at which the edge ends.
#'   * `rel`: Type of edge as defined by `epicmodel`. Possible options are: `if` (`from` node is in IF condition of `to` node), `ifnot` (`from` node
#'     is in IFNOT condition of `to` node).
#'   * `arrowhead`: Type of arrow. `normal` for rel `if` and `tee` for rel `ifnot`.
#'   * `arrowsize`: Size of arrow. 1 for rel `if` and 1.2 for rel `ifnot`.
#'   * `color`: Color of arrow. Gray for rel `if` and `#A65141` for rel `ifnot`.}
#'  }
#'
#' @param scc For `mechanism()`, an object of class `epicmodel_scc`.
#' @param modules For `mechanism()`, TRUE (default) or FALSE, indicating if nodes in the same module should be colored equally (TRUE) or if all
#' nodes have white background (FALSE). Colors are only applied, if modules have actually been specified in the `epicmodel_steplist`. If modules
#' are considered by `mechanism()`, the module keywords are added to the legend (accessable via `print()`).
#' @param module_colors For `mechanism()`, if nodes are colored by module, colors can be provided via this argument. Colors must be provided as a
#' character vector. Both named colors and hexadecimal color codes are allowed. The function has 8 colors stored internally.
#' If `module_colors` = NULL (default), these colors are used. If the model has more than 8 modules, `module_colors` must be specified. If more
#' colors than necessary are specified, the function takes as many as necessary from the start of the vector.
#' @param x `x` is used in several functions:
#' * `new_mechanism()`: A list to be converted to class `epicmodel_mechanism`.
#' * `validate_mechanism()`: An object of class `epicmodel_mechanism` to be validated.
#' * `plot.epicmodel_mechanism()`: An object of class `epicmodel_mechanism`.
#' * `print.epicmodel_mechanism()`: An object of class `epicmodel_mechanism`.
#' @param ... Additional arguments for generics `print()` and `plot()`.
#'
#' @returns
#' * `mechanism()`: An object of class `epicmodel_mechanism`. Use `plot()` to plot the graphs in the RStudio Viewer. Use `print()` to print the
#'   legend in the console. Use [export_mechanism()] to save the graphs as PNG, PDF, SVG, or PostScript.
#' * `new_mechanism()`: An object of class `epicmodel_mechanism`.
#' * `validate_mechanism()`: An object of class `epicmodel_mechanism` that has been checked to have the correct structure.
#' * `plot.epicmodel_mechanism()`: Renders the graphs in the RStudio Viewer.
#' * `print.epicmodel_mechanism()`: Prints the legend of the `epicmodel_mechanism` object in the console.
#'
#' @seealso
#' * [export_mechanism()] for saving the plots
#' * [DiagrammeR::node_aes()] for a list of node-related variables in `DiagrammeR`
#' * [DiagrammeR::edge_aes()] for a list of edge-related variables in `DiagrammeR`
#'
#' @export
#'
#' @examples
#' # Create some SCC model
#' steplist_checked <- check_steplist(steplist_rain)
#' scc_model <- create_scc(steplist_checked)
#'
#' # Derive mechanisms
#' mech <- mechanism(scc_model)
#'
#' # new_mechanism() and validate_mechanism() are used inside mechanism()
#' # nonetheless, you can check its structure using validate_mechanism()
#' validate_mechanism(mech)
#'
#' # Plot the mechanisms
#' plot(mech)
#'
#' # Print the legend
#' print(mech)
#' mech
#'
mechanism <- function(scc, modules = TRUE, module_colors = NULL) {
  # Check inputs
  if (inherits(scc, "epicmodel_scc") %>% magrittr::not()) {
    cli::cli_abort("{.var scc} must be a {.emph epicmodel_scc} class object!",
                   class = "no_scc")
  }

  rlang::try_fetch(checkmate::assert_logical(modules, any.missing = F, len = 1, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort("{.var modules} must be TRUE or FALSE!",
                                    parent = cnd, class = "input_modules")
                   })

  rlang::try_fetch(checkmate::assert_character(module_colors, any.missing = F, min.len = 1, min.chars = 1, null.ok = T),
                   error = function(cnd) {
                     cli::cli_abort("{.var module_colors} must be a character vector holding valid color descriptions!",
                                    parent = cnd, class = "input_module_colors")
                   })

  if (!is.null(module_colors)) {
    if (!are_colors(module_colors)) {
      cli::cli_abort("{.var module_colors} contains invalid color descriptions!", class = "invalid_module_colors")
    }
  }
  #=============================================================================
  # Process steplist
  prc <- scc$steplist %>% process_steplist()
  prc_split <- prc %>% split_prc()
  ## Add THEN description
  prc %<>% dplyr::full_join(scc$steplist$then, by = c("then_step" = "id_then"))

  # Add node label
  prc$label <- NA_character_
  ## Split into parts by step type
  prc_cc <- prc %>% dplyr::filter(.data$id_step %in% prc_split$causes$id_step)
  prc_int <- prc %>% dplyr::filter(.data$id_step %in% prc_split$interventions$id_step)
  prc_end <- prc %>% dplyr::filter(.data$id_step %in% prc_split$end_steps)
  cc_int_end <- c(prc_cc$id_step, prc_int$id_step, prc_end$id_step)
  prc_other <- prc %>% dplyr::filter(!(.data$id_step %in% cc_int_end))
  ## Add labels by step type
  if (nrow(prc_cc) > 0) {prc_cc$label <- paste0("CC",c(1:nrow(prc_cc)))}
  if (nrow(prc_int) > 0) {prc_int$label <- paste0("I",c(1:nrow(prc_int)))}
  if (nrow(prc_end) > 0) {prc_end$label <- paste0("E",c(1:nrow(prc_end)))}
  if (nrow(prc_other) > 0) {prc_other$label <- paste0("S",c(1:nrow(prc_other)))}
  ## Bind back together
  prc <- rbind(prc_cc, prc_int, prc_other, prc_end)

  # Add module colors
  if (scc$sc_use_modules & modules) {
    ## Get number of modules
    n_modules <- nrow(scc$steplist$module)
    ## No module_colors specified
    if (is.null(module_colors) & n_modules > 8) {
      cli::cli_abort(c("Your SCC model contains more than 8 modules. {.fn mechanism} can only color up to 8 modules without specification of
                       {.var module_colors}.",
                       "i" = "Set {.var modules} to FALSE or provide a character vector with at least {n_modules} colors for {.var module_colors}."),
                     class = "no_colors_when_needed")
    }
    if (is.null(module_colors) & n_modules <= 8) {
      colors_used <- c("#fbb4ae","#b3cde3","#ccebc5","#ffffcc","#fddaec","#fed9a6","#cccccc","#e5d8bd")[1:n_modules]
    }
    ## module_colors specified
    if (!is.null(module_colors)) {
      if (length(module_colors) < n_modules) {
        cli::cli_abort("Your SCC model contains {n_modules} modules, but you only specified {length(module_colors)} colors in {.var module_colors}.",
                       class = "not_enough_colors")
      } else {
        colors_used <- module_colors[1:n_modules]
      }
    }
    ## Merge color to prc
    color_df <- scc$steplist$module %>% cbind(.,colors_used)
    prc %<>% dplyr::left_join(color_df, by = c("module_step" = "id_module"))

  } else {
    ## Only warn if "modules = T" has been included in the function call
    if (methods::hasArg(modules) & modules) {warning("The SCC model does not use modules!")}
    prc$colors_used <- "white"
  }

  out_graph <- vector(mode = "list", length = length(scc$sc_steps))

  # Loop over every sufficient cause
  for (i in 1:length(scc$sc_steps)) {
    prc_temp <- prc %>% dplyr::filter(.data$id_step %in% scc$sc_steps[[i]])

    cc_temp <- prc_temp %>% dplyr::filter(.data$id_step %in% prc_split$causes$id_step)
    int_temp <- prc_temp %>% dplyr::filter(.data$id_step %in% prc_split$interventions$id_step)
    end_temp <- prc_temp %>% dplyr::filter(.data$id_step %in% prc_split$end_steps)
    cc_int_end_temp <- c(cc_temp$id_step, int_temp$id_step, end_temp$id_step)
    other_temp <- prc_temp %>% dplyr::filter(!(.data$id_step %in% cc_int_end_temp))

    # Nodes
    ndf_cc <- DiagrammeR::create_node_df(n = nrow(cc_temp),
                                         type = "cc",
                                         label = cc_temp$label,
                                         tooltip = cc_temp$desc_then,
                                         shape = "square",
                                         color = "gray",
                                         fillcolor = cc_temp$colors_used,
                                         fontcolor = "black")
    ndf_int <- DiagrammeR::create_node_df(n = nrow(int_temp),
                                          type = "int",
                                          label = int_temp$label,
                                          tooltip = int_temp$desc_then,
                                          shape = "triangle",
                                          color = "gray",
                                          fillcolor = int_temp$colors_used,
                                          fontcolor = "black")
    ndf_end <- DiagrammeR::create_node_df(n = nrow(end_temp),
                                          type = "end",
                                          label = end_temp$label,
                                          tooltip = end_temp$desc_then,
                                          shape = "circle",
                                          color = "black",
                                          fillcolor = end_temp$colors_used,
                                          fontcolor = "black")
    ndf_other <- DiagrammeR::create_node_df(n = nrow(other_temp),
                                            type = "other",
                                            label = other_temp$label,
                                            tooltip = other_temp$desc_then,
                                            shape = "circle",
                                            color = "gray",
                                            fillcolor = other_temp$colors_used,
                                            fontcolor = "black")
    ndf <- DiagrammeR::combine_ndfs(ndf_cc, ndf_int, ndf_end, ndf_other)

    # Edges
    ## Merge node ids to prc_temp
    prc_temp %<>% dplyr::left_join(ndf[,c("label","id")], by = "label")

    ## If
    ### Get dataset of steps in the current sufficient cause with IF conditions
    if_temp <- prc_temp %>% dplyr::filter(!is.na(.data$if_step))
    if (nrow(if_temp) > 0) {
      ### Create empty container
      if_edges <- vector(mode = "list", length = nrow(if_temp))
      ### Loop over all steps with IF condition
      for (j in 1:length(if_edges)) {
        #### Get if_list from processed steplist
        if_list_temp <- if_temp$if_list[[j]]
        #### Get number of entries in if_list
        n_if_list_temp <- nrow(if_list_temp)
        #### Create output container for current step (from is set to this steps node id)
        out_temp <- data.frame(from = rep(NA_integer_, n_if_list_temp),
                               to = rep(if_temp$id[j], n_if_list_temp),
                               sce = rep(NA_character_, n_if_list_temp))
        #### Loop over every line in if_list
        for (k in 1:n_if_list_temp) {
          ##### Check if the current part of the IF condition has been used for the current sufficient cause
          if ((prc_temp$then_step == if_list_temp$id[k]) %>% all_false()) {
            ###### If not, set "from" from 0 instead of the node id
            out_temp$from[k] <- 0
          } else {
            ###### If yes, set "from" from the corresponding node id
            out_temp$from[k] <- prc_temp$id[prc_temp$then_step == if_list_temp$id[k]]
          }
          ##### Record scenario as well
          out_temp$sce[k] <- if_list_temp$sce[k]
        }
        #### If there are scenarios with parts set from 0, all other parts are set from 0 as well,
        #### thereby only completed IF conditions appear in the graph
        if ((out_temp$from == 0) %>% all_false() %>% magrittr::not()) {
          to_zero <- out_temp$sce[out_temp$from == 0]
          out_temp$from[out_temp$sce %in% to_zero] <- 0
        }
        if_edges[[j]] <- out_temp
      }

      if_edges %<>% purrr::map_dfr(as.data.frame) %>% dplyr::filter(.data$from != 0)

      edf_if <- DiagrammeR::create_edge_df(from = if_edges$from,
                                           to = if_edges$to,
                                           rel = "if",
                                           arrowhead = "normal",
                                           arrowsize = 1,
                                           color = "gray")
    }

    ## Ifnot
    ### Get dataset of steps in the current sufficient cause with IFNOT conditions
    ifnot_temp <- prc_temp %>% dplyr::filter(!is.na(.data$ifnot_step))
    if (nrow(ifnot_temp) > 0) {
      ### Create empty container
      ifnot_edges <- vector(mode = "list", length = nrow(ifnot_temp))
      ### Loop over all steps with IF condition
      for (j in 1:length(ifnot_edges)) {
        #### Get ifnot_list from processed steplist
        ifnot_list_temp <- ifnot_temp$ifnot_list[[j]]
        #### Get number of entries in ifnot_list
        n_ifnot_list_temp <- nrow(ifnot_list_temp)
        #### Create output container for current step (from is set to this steps node id)
        out_temp <- data.frame(from = rep(NA_integer_, n_ifnot_list_temp),
                               to = rep(ifnot_temp$id[j], n_ifnot_list_temp),
                               sce = rep(NA_character_, n_ifnot_list_temp))
        #### Loop over every line in ifnot_list
        for (k in 1:n_ifnot_list_temp) {
          ##### Check if the current part of the IF condition has been used for the current sufficient cause
          if ((prc_temp$then_step == ifnot_list_temp$id[k]) %>% all_false()) {
            ###### If not, set "from" from 0 instead of the node id
            out_temp$from[k] <- 0
          } else {
            ###### If yes, set "from" from the corresponding node id
            out_temp$from[k] <- prc_temp$id[prc_temp$then_step == ifnot_list_temp$id[k]]
          }
          ##### Record scenario as well
          out_temp$sce[k] <- ifnot_list_temp$sce[k]
        }
        #### If there are scenarios with parts set from 0, all other parts are set from 0 as well,
        #### thereby only completed IF conditions appear in the graph
        if ((out_temp$from == 0) %>% all_false() %>% magrittr::not()) {
          to_zero <- out_temp$sce[out_temp$from == 0]
          out_temp$from[out_temp$sce %in% to_zero] <- 0
        }
        ifnot_edges[[j]] <- out_temp
      }

      ifnot_edges %<>% purrr::map_dfr(as.data.frame) %>% dplyr::filter(.data$from != 0)

      edf_ifnot <- DiagrammeR::create_edge_df(from = ifnot_edges$from,
                                              to = ifnot_edges$to,
                                              rel = "ifnot",
                                              arrowhead = "tee",
                                              arrowsize = 1.2,
                                              color = "#A65141")
    }

    if (nrow(if_temp) == 0 & nrow(ifnot_temp) > 0) {
      edf <- edf_ifnot
    }
    if (nrow(if_temp) > 0 & nrow(ifnot_temp) == 0) {
      edf <- edf_if
    }
    if (nrow(if_temp) > 0 & nrow(ifnot_temp) > 0) {
      edf <- DiagrammeR::combine_edfs(edf_if, edf_ifnot)
    }

    out_graph[[i]] <- list(ndf = ndf, edf = edf)
  }

  # Create legend
  if (scc$sc_use_modules & modules) {
    legend <- prc %>% dplyr::select(dplyr::all_of(c("label","key_module","desc_then"))) %>% as.data.frame()
    colnames(legend)[which(colnames(legend) == "label")] <- "Label"
    colnames(legend)[which(colnames(legend) == "key_module")] <- "Module"
    colnames(legend)[which(colnames(legend) == "desc_then")] <- "Step"
  } else {
    legend <- prc %>% dplyr::select(dplyr::all_of(c("label","desc_then"))) %>% as.data.frame()
    colnames(legend)[which(colnames(legend) == "label")] <- "Label"
    colnames(legend)[which(colnames(legend) == "desc_then")] <- "Step"
  }


  # Combine and transform to class "epicmodel_mechanism"
  out <- list(legend = legend, graph = out_graph) %>% new_mechanism()
  out %<>% validate_mechanism()
  return(out)
}

#' @description `new_mechanism()` and `validate_mechanism()` define the `epicmodel_mechanism` S3 class, which is created by `mechanism()`.
#'
#' @rdname mechanism
#'
#' @export
#'
new_mechanism <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = "epicmodel_mechanism")
}

#' @rdname mechanism
#'
#' @export
#'
validate_mechanism <- function(x) {
  # S3 class
  checkmate::assert_class(x, "epicmodel_mechanism", null.ok = F)

  # Set up checkmate collection
  coll <- checkmate::makeAssertCollection()

  # Element names
  checkmate::assert_set_equal(names(x), c("legend","graph"), add = coll)

  # legend
  checkmate::assert_data_frame(x$legend, types = "character", any.missing = F, null.ok = F, min.rows = 1, min.cols = 2, max.cols = 3,
                               col.names = "unique", add = coll)
  if (ncol(x$legend) == 2) {
    checkmate::assert_set_equal(colnames(x$legend), c("Label","Step"), add = coll)
  }
  if (ncol(x$legend) == 3) {
    checkmate::assert_set_equal(colnames(x$legend), c("Label","Module","Step"), add = coll)
  }

  # graph
  checkmate::assert_list(x$graph, types = "list", any.missing = F, null.ok = F, min.len = 1, add = coll)
  for (i in 1:length(x$graph)) {
    checkmate::assert_list(x$graph[[i]], types = "data.frame", any.missing = F, null.ok = F, len = 2, names = "unique", add = coll)
    checkmate::assert_set_equal(names(x$graph[[i]]), c("ndf","edf"), add = coll)
    checkmate::assert_data_frame(x$graph[[i]][["ndf"]], type = c("integer", "character"), any.missing = F, null.ok = F, ncols = 8, add = coll)
    checkmate::assert_set_equal(colnames(x$graph[[i]][["ndf"]]),
                                c("id","type","label","tooltip","shape","color","fillcolor","fontcolor"), add = coll)
    checkmate::assert_data_frame(x$graph[[i]][["edf"]], type = c("integer", "character", "numeric"), any.missing = F, null.ok = F, ncols = 7,
                                 add = coll)
    checkmate::assert_set_equal(colnames(x$graph[[i]][["edf"]]),
                                c("id","from","to","rel","arrowhead","arrowsize","color"), add = coll)
  }

  # Report collection
  checkmate::reportAssertions(coll)

  return(x)
}

#' @description `plot()` renders the graphs in the RStudio Viewer.
#'
#' @method plot epicmodel_mechanism
#'
#' @param reverse For `plot.epicmodel_mechanism()`, TRUE or FALSE indicating if the output should be displayed in reverse order. Since graphs
#' rendered later show up first in the viewer pane, reverse = T leads to SC1 being the last rendered and the one displayed on top.
#'
#' @rdname mechanism
#'
#' @export
#'
plot.epicmodel_mechanism <- function(x, reverse = TRUE, ...) {
  # Check input
  rlang::try_fetch(checkmate::assert_logical(reverse, any.missing = F, len = 1, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort("{.var reverse} must be TRUE or FALSE!",
                                    parent = cnd, class = "input_reverse")
                   })
  #=============================================================================
  # Set up and fill output container
  out <- vector(mode = "list", length = length(x$graph)) %>% magrittr::set_names(paste0("SC",c(1:length(x$graph))))
  for (i in 1:length(x$graph)) {
    gr <- DiagrammeR::create_graph(nodes_df = x$graph[[i]]$ndf, edges_df = x$graph[[i]]$edf,
                                      graph_name = paste0("SC",i), attr_theme = "lr") %>%
            DiagrammeR::render_graph(title = paste0("SC",i))
    out[[i]] <- gr
  }

  # Reverse order
  if (reverse) {out %<>% rev()}

  return(out)
}

#' @description `print()` prints the legend for node labels in the console.
#'
#' @method print epicmodel_mechanism
#'
#' @rdname mechanism
#'
#' @export
#'
print.epicmodel_mechanism <- function(x, ...) {
  print(x$legend, right = F, row.names = F)
}

#' Export mechanisms
#'
#' Exports one or all sufficient cause mechanisms as PNG, PDF, SVG, or PostScript using `DiagrammeR::export_graph()`.
#'
#' @param mechanism An object of class `epicmodel_mechanism`.
#' @param sc A single integer value (can be specified as numeric, e.g., 2 instead of 2L). If provided, a graph is only exported for the specified
#'   sufficient cause, e.g., for SC2 if sc = 2. If sc = NULL (default), graphs for all sufficient causes are exported.
#' @inheritParams DiagrammeR::export_graph
#' @inheritDotParams DiagrammeR::export_graph width height
#'
#' @seealso
#' * [DiagrammeR::export_graph()]
#' * [mechanism()] for information on sufficient cause mechanisms
#'
#' @returns Saves the mechanisms as PNG, PDF, SVG, or PostScript.
#'
#' @export
#'
#' @examples
#' # Derive mechanisms
#' mech <- mechanism(scc_rain)
#'
#' # Export mechanism plot of sufficient cause (sc) 1
#' if(interactive()){
#' tmp <- tempfile(fileext = ".png")
#' export_mechanism(mech, sc = 1, file_name = tmp, title = "Sufficient Cause 1")
#' unlink(tmp) # delete saved file
#' }
export_mechanism <- function(mechanism, sc = NULL, file_name = NULL, file_type = "png", title = NULL, ...) {
  # Check input
  if (inherits(mechanism, "epicmodel_mechanism") %>% magrittr::not()) {
    cli::cli_abort("{.var mechanism} must be a {.emph epicmodel_mechanism} class object!",
                   class = "no_mechanism")
  }

  rlang::try_fetch({checkmate::assert_numeric(sc, upper = length(mechanism$graph), any.missing = F, len = 1, null.ok = T)
                    if (!is.null(sc)) {sc %<>% as.integer()}
                    checkmate::assert_integer(sc, upper = length(mechanism$graph), any.missing = F, len = 1, null.ok = T)
                    }, error = function(cnd) {
                     cli::cli_abort("{.var sc} must be a single integer smaller or equal the number of sufficient causes!",
                                    parent = cnd, class = "input_sc")
                   })

  rlang::try_fetch(checkmate::assert_character(file_name, any.missing = F, len = 1, min.chars = 1, null.ok = T),
                   error = function(cnd) {
    cli::cli_abort(c("{.var file_name} must be a single character!",
                   "i" = "If it does not end with the file extension specified in file_type, it will be added."),
                   parent = cnd, class = "input_file_name")
  })

  rlang::try_fetch(checkmate::assert_character(title, any.missing = F, len = 1, min.chars = 1, null.ok = T),
                   error = function(cnd) {
                     cli::cli_abort("{.var title} must be a single character!",
                                    parent = cnd, class = "input_title")
                   })

  rlang::try_fetch({checkmate::assert_character(file_type, any.missing = F, len = 1, min.chars = 1, null.ok = F)
                   file_type %<>% stringr::str_to_lower()
                   checkmate::assert_choice(file_type, choices = c("png", "pdf", "svg", "ps"))},
                   error = function(cnd) {
                     cli::cli_abort("{.var file_type} must be one of: png, pdf, svg, ps!",
                                    parent = cnd, class = "input_file_type")
                   })
  #=============================================================================
  # Handle inputs
  title_used <- export_mechanism_handle_input(sc = sc, n_sc = length(mechanism$graph), file_name = file_name,
                                              file_type = file_type, title = title)[["title_used"]]
  file_name_used <- export_mechanism_handle_input(sc = sc, n_sc = length(mechanism$graph), file_name = file_name,
                                                  file_type = file_type, title = title)[["file_name_used"]]

  # Export
  if (is.null(sc)) {
    for (i in 1:length(mechanism$graph)) {
      DiagrammeR::create_graph(nodes_df = mechanism$graph[[i]]$ndf, edges_df = mechanism$graph[[i]]$edf,
                               graph_name = paste0("SC",i), attr_theme = "lr") %>%
        DiagrammeR::export_graph(file_name = file_name_used[i], file_type = file_type, title = title_used[i], ...)
    }
  } else {
    DiagrammeR::create_graph(nodes_df = mechanism$graph[[sc]]$ndf, edges_df = mechanism$graph[[sc]]$edf,
                             graph_name = paste0("SC",sc), attr_theme = "lr") %>%
      DiagrammeR::export_graph(file_name = file_name_used, file_type = file_type, title = title_used, ...)
  }
}

#' Handle inputs for function `export_mechanism()`
#'
#' Prepares `file_name` and `title` inputs for `DiagrammeR::export_graph()`.
#'
#' @param sc Input `sc` from function `export_mechanism()`.
#' @param n_sc Number of sufficient causes, usually as `length(mechanism$graph)`.
#' @param file_name Input `file_name` from function `export_mechanism()`.
#' @param file_type Input `file_type` from function `export_mechanism()`.
#' @param title Input `title` from function `export_mechanism()`.
#'
#' @returns A list of length 2 containing elements `title_used` and `file_name_used`. If no `title` has been specified in `export_mechanism`,
#' element `title_used` is NULL. If a `title` has been specified, numbers from 1 to `n_sc` are added at the end, apart form when `sc` is specified,
#' in which case the specified `title` is used without making any changes. In any way, `title_used` is a character vector of length `n_sc` or of
#' length 1 if `sc` has been specified. `file_name_used` has the same lenght as `title_used` (if `title_used` is not NULL) containing the paths
#' under which each graph is saved. If `sc` is NULL, a number from 1 to `n_sc` is added to the file name.
#'
#' @noRd
export_mechanism_handle_input <- function(sc, n_sc, file_name, file_type, title) {
  if (is.null(sc)) {
    if (!is.null(file_name)) {
      name <- file_name %>% stringr::str_to_lower() %>% stringr::str_replace(paste0("\\.", stringr::str_to_lower(file_type)),"")
      file_name_used <- paste0(name,c(1:n_sc),".",stringr::str_to_lower(file_type))
    } else {
      file_name_used <- paste0("graph_sc",c(1:n_sc),".",stringr::str_to_lower(file_type))
    }
    if (!is.null(title)) {
      title_used <- paste0(title,c(1:n_sc))
    } else {
      title_used <- NULL
    }
  } else {
    if (!is.null(file_name)) {
      name <- file_name %>% stringr::str_to_lower() %>% stringr::str_replace(paste0("\\.", stringr::str_to_lower(file_type)),"")
      file_name_used <- paste0(name,".",stringr::str_to_lower(file_type))
    } else {
      file_name_used <- paste0("graph_sc",sc,".",stringr::str_to_lower(file_type))
    }
    if (!is.null(title)) {
      title_used <- title
    } else {
      title_used <- NULL
    }
  }
  #=============================================================================
  # Check output
  rlang::try_fetch(checkmate::assert_character(title_used, any.missing = F, null.ok = T, min.len = 1, min.chars = 1),
                   error = function(cnd) {
                     cli::cli_abort("Output validation error: {.var title_used}",
                                    parent = cnd, class = "output_title_used")
                   })

  rlang::try_fetch(checkmate::assert_character(file_name_used, any.missing = F, null.ok = F, min.len = 1, min.chars = 1),
                   error = function(cnd) {
                     cli::cli_abort("Output validation error: {.var file_name_used}",
                                    parent = cnd, class = "output_file_name_used")
                   })
  #=============================================================================
  out <- list(title_used = title_used, file_name_used = file_name_used)
  return(out)
}

#' Check color vector
#'
#' Checks if all elements of a character vector describe valid colors.
#'
#' @param x Character vector containing potential color descriptions.
#'
#' @returns TRUE if all elements are valid color descriptions. FALSE if at least one element is an invalid color description.
#'
#' @noRd
are_colors <- function(x) {
  # Check input
  rlang::try_fetch(checkmate::assert_character(x, min.len = 1, min.chars = 1, any.missing = F, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort("Input validation error: {.var x}", parent = cnd, class = "input_x")
                   })
  #=============================================================================
  # Get colors by name
  color_names <- grDevices::colors()

  # Define long hexadecimal form string pattern
  pattern <- "^#[[:digit:][abcdef]]{6}$"

  # Input to lower case
  x %<>% stringr::str_to_lower()

  # Prepare empty output container
  check <- vector(mode = "logical", length = length(x))

  # Fill container
  for (i in 1:length(x)) {
    check[i] <- (x[i] %>% stringr::str_detect(pattern) | x[i] %in% color_names)
  }

  # Check if all of them are colors
  out <- check %>% all_true()
  #=============================================================================
  # Check output
  rlang::try_fetch(checkmate::assert_logical(out, any.missing = F, len = 1, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort("Output validation error", parent = cnd, class = "output")
                   })
  #=============================================================================
  return(out)
}
