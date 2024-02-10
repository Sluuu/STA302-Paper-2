#' Create the igraph object
#' @description Create a graph from node covariates and edgelists
#' @param graph_data a list containing the edgelist and node attriutes
#' @param features a list of features to include in the graph
#'
#' @return an igraph network
#' @export
#' @importFrom magrittr %>% %<>%
#'
make_base_igraph_network <- function(graph_data, features) {
  g <- igraph::graph_from_edgelist(as.matrix(graph_data$edges[, 1:2]), directed = FALSE)

  # Extract features
  df_feature <-
    tibble::tibble(id = igraph::V(g)$name) %>%
    dplyr::left_join(graph_data$nodes, by = "id")

  # Check if the order of id in df_feature is the same with that of g
  stopifnot(all(igraph::V(g)$name == df_feature[["id"]]))

  # Add nodal features to g
  for (feature in features) {
    g %<>%
      igraph::set.vertex.attribute(feature, value = df_feature[[feature]])
  }

  return(g)
}

#' Keep only Tokyo nodes
#'
#' @param g An igraph network object
#'
#' @return the filtered network that includes only nodes located in Tokyo
#' @export
#'
keep_tokyo <- function(g) {
  is_tokyo <- igraph::get.vertex.attribute(g, "prefecture") == "東京都"
  tokyo_vert_ids <- igraph::V(g)[is_tokyo]
  return(igraph::induced_subgraph(g, tokyo_vert_ids))
}

#' Keep k-core
#'
#' @param g an igraph network object
#' @param k the minimum degree
#'
#' @return an igraph network object containing nodes that belong to the k-core
#' @export
#'
keep_k_core <- function(g, k) {
  k_core <- igraph::coreness(g)
  vert_ids <- igraph::V(g)[k_core >= k]
  return(igraph::induced_subgraph(g, vert_ids))
}

#' Extract the Giant Component
#'
#' @param an igraph network object
#'
#' @return an igraph network representing the largest connected component
#' @export
#'
extract_giant_component <- function(g) {
  components <- igraph::clusters(g)
  biggest_cluster_id <- which.max(components$csize)
  vert_ids <- igraph::V(g)[components$membership == biggest_cluster_id]
  return(igraph::induced_subgraph(g, vert_ids))
}

#' Build the Graph
#'
#' @param g an igraph network
#' @param k the minimum degree
#'
#' @return the filtered network
#' @export
#'
build_graph <- function(g, k) {
  g <- keep_tokyo(g)
  g <- keep_k_core(g, k)
  g <- extract_giant_component(g)

  df <-
    tibble::tibble(
      k_core = k
    )

  g$filter_condition <- df

  return(g)
}

#' Descriptive Statistics for the Graph
#'
#' @param g an igraph network
#'
#' @return a list containing summary statistics of the network, the degree distribution and its summary
#' @export
#'
get_graph_stats <- function(g) {
  n_nodes <- igraph::gorder(g)
  edges <- igraph::gsize(g)
  triangles <- sum(igraph::count_triangles(g)) / 3
  density <- igraph::edge_density(g)
  degrees <- igraph::degree(g)

  network_stats <-
    g$filter_condition %>%
    dplyr::mutate(
      n_nodes = n_nodes,
      edges = edges,
      triangles = triangles,
      density = density
    )

  output <- list(
    network_stats = network_stats,
    degrees = degrees,
    degree_stats = summary(degrees)
  )

  return(output)
}
