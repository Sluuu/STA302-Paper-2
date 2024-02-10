library(tibble)
library(magrittr)

get_test_data <- function(){
  # Create an empty graph
  g <- igraph::make_empty_graph(10, directed=FALSE)
  igraph::V(g)$name <- LETTERS[1:10]
  # Create a fully connected cluster
  full_g <- igraph::make_full_graph(5, directed=FALSE)
  igraph::V(full_g)$name <- LETTERS[1:5]
  # Add both edgelists and include a few more edges
  g <- g + full_g
  g <- igraph::add_edges(g, c("F","B", "F","E", "G","C", "H","I", "J","E"))

  edgelist <- as.data.frame(igraph::as_edgelist(g))
  colnames(edgelist) <- c("src", "dst")
  # Assign all nodes to Tokyo
  nodes <- tibble(id=igraph::V(g)$name, x=1:10, y=1:10, z=1:10, prefecture="東京都")
  # Except for the "E" node, which is located in Chiba
  nodes[5, "prefecture"] <- "千葉県"

  graph_data <- list(edges=edgelist, nodes=nodes)
  return(graph_data)
}

test_that("creates the igraph object", {
  test_data <- get_test_data()
  test_features <- c("x", "y")
  g <- aea2023::make_base_igraph_network(test_data, test_features)
  # All the nodes in the nodes dataframe should be included in the network
  expect_equal(sort(igraph::V(g)$name), sort(test_data$nodes$id))
  # The number of edges should be the same
  expect_equal(nrow(igraph::as_edgelist(g)), nrow(test_data$edges))
  # The desired features should be attached to the igraph object
  expect_equal(test_features, tail(igraph::vertex_attr_names(g), 2))
})


test_that("keeps only tokyo nodes", {
  test_data <- get_test_data()
  test_features <- c("x", "y", "prefecture")
  g <- aea2023::make_base_igraph_network(test_data, test_features)
  filtered_g <- aea2023::keep_tokyo(g)
  # The prefecture of all the nodes should be Tokyo
  expect_equal(unique(igraph::V(filtered_g)$prefecture), "東京都")
  # The nodes that remain in the filtered network should be the ones located in Tokyo in the original data
  expect_equal(sort(igraph::V(filtered_g)$name), sort(igraph::V(g)$name[igraph::V(g)$prefecture == "東京都"]))
})

test_that("extracts the k-core", {
  test_data <- get_test_data()
  test_features <- c("x", "y")
  g <- aea2023::make_base_igraph_network(test_data, test_features)
  kcore <- aea2023::keep_k_core(g, 3)
  # The degree in the filtered graph should not be larger than k
  expect_gte(max(igraph::degree(kcore)), 3)
})

test_that("extracts the giant component", {
  # Create a fully connected graph
  g_large_comp <- igraph::make_full_graph(6)
  igraph::V(g_large_comp)$name <- LETTERS[1:6]
  # Now create a smaller fully connected graph
  g_small_comp <- igraph::make_full_graph(3)
  igraph::V(g_small_comp)$name <- LETTERS[7:9]
  # Put them into a single network. Node names are different, so both components should be disconnected.
  g <- g_large_comp + g_small_comp
  filtered_g <- aea2023::extract_giant_component(g)
  # The filtered graph should contain the same nodes as the larger component created above
  expect_equal(sort(igraph::V(filtered_g)$name), sort(igraph::V(g_large_comp)$name))
  # The edges should be the same too
  expect_equal(nrow(igraph::as_edgelist(filtered_g)), nrow(igraph::as_edgelist(g_large_comp)))
})
