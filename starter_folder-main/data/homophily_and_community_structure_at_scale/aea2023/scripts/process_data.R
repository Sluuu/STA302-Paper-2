library(targets)

# This file contains the pipeline that reads the raw data into an R list.
# The list is used in posterior pipelines to create the network for the analysis.

tar_option_set(packages = c("dplyr", "aea2023", "readr", "magrittr"))

params <- config::get()


list(
  # Read each file as dataframes and attach them to a list
  tar_target(
    path_nodes,
    params[["path_raw_node"]],
    format = "file"
  ),
  tar_target(
    path_edges,
    params[["path_raw_edge"]],
    format = "file"
  ),
  tar_target(
    df_nodes,
    read_csv(path_nodes, col_types = c(id = "c"))
  ),
  tar_target(
    df_edges,
    read_csv(path_edges, col_types = c(src = "c", dst = "c"))
  ),
  tar_target(
    list_data,
    list("nodes" = df_nodes, "edges" = df_edges)
  ),
  # Set the names of the node attributes
  tar_target(
    nodal_features,
    c("title_rank", "occupation_code", "main_category",
      "large_category", "prefecture", "city", "h3_5", "h3_6",
      "h3_7", "h3_8", "h3_9", "h3_10", "num_month")
  ),
  # Create the base graph containing all the information. This will be used for further filtering.
  tar_target(
    base_igraph_network,
    make_base_igraph_network(list_data, nodal_features)
  ),
  # Keep only the nodes for which we have information on the target variables
  tar_target(
    base_nonmissing_igraph_network,
    base_igraph_network %>%
      igraph::delete.vertices(igraph::V(base_igraph_network)[
        is.na(large_category) | is.na(prefecture) | is.na(title_rank) | is.na(occupation_code) | is.na(num_month)
        ])
  ),
  # From the remaining graph, extract the largest connected component of the 10-core.
  tar_target(
    graph,
    build_graph(base_nonmissing_igraph_network, 10)
  ),
  # Obtain descriptive statistics for the graph
  tar_target(
    graph_stats,
    get_graph_stats(graph)
  ),
  # Cast the graph into a statnet::network object, which is the format required by the lighthergm library
  tar_target(
    g_tokyo_10core,
    intergraph::asNetwork(graph)
  )
)
