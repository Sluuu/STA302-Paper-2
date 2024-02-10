library(targets)
# This pipeline performs the analysis employing HERGM on the graph created from the `create_network` pipeline.

tar_option_set(packages = c("dplyr", "aea2023", "stargazer", "ggplot2"))

params <- config::get()


list(
  # Set the path to the network created in the `create_network` pipeline.
  tar_target(
    path_list_graph,
    params[["path_list_graph"]],
    format = "file"
  ),
  # Read the graph
  tar_target(
    g_tokyo_10core,
    readRDS(path_list_graph)
  ),
  # Obtain a random seed.
  tar_target(
    random_seed,
    get_random_seed()
  ),
  # Apply the block recovery step to the network
  tar_target(
    first_clustering_run,
    lighthergm::hergm(
      g_tokyo_10core ~ edges + triangle + nodematch("h3_7") + nodematch("large_category") + nodematch("occupation_code"),
      n_clusters = params[["n_cluster"]],
      n_em_step_max = params[["n_iteration_without_covariate"]],
      estimate_parameters = FALSE,
      use_infomap_python = TRUE,
      clustering_with_features = FALSE,
      verbose = TRUE,
      infomap_seed = random_seed,
      seeds = c(random_seed)
    )
  ),
  # Apply a few additional iterations of the EM algorithm departing from the checkpoint created by the previous step
  tar_target(
    second_clustering_run,
    lighthergm::hergm(
      first_clustering_run,
      n_em_step_max = params[["n_iteration_without_covariate"]],
      clustering_with_features = FALSE
    )
  ),
  # Apply a final round of EM iterations departing from the partition obtained in the previous step.
  # This time, employ information on node features.
  tar_target(
    third_clustering_run,
    lighthergm::hergm(
      g_tokyo_10core ~ edges + triangle + nodematch("h3_7") + nodematch("large_category") + nodematch("occupation_code"),
      n_clusters = params[["n_iteration_with_covariate"]],
      n_em_step_max = params[["n_cluster"]],
      estimate_parameters = FALSE,
      clustering_with_features = TRUE,
      initialized_cluster_data = second_clustering_run$partition,
      seeds = c(random_seed),
      verbose = TRUE
    )
  ),
  # Estimate the model coefficients
  tar_target(
    coefficient_results,
    lighthergm::hergm(
      third_clustering_run,
      block_membership = third_clustering_run$partition,
      estimate_coefficients = TRUE,
      method_second_step = "MPLE"
    )
  ),
  tar_target(
    data_path,
    params[["path_list_processd_data"]],
    format = "file"
  ),
  # Read the nodes features data and the edgelist
  tar_target(
    list_data,
    readRDS(data_path)
  ),
  # Attach the block affiliations to the nodes
  tar_target(
    nodes_data,
    get_nodes_with_block(list_data$nodes, coefficient_results$partition, g_tokyo_10core)
  ),
  # Obtain descriptive statistics from the nodes data
  tar_target(
    descriptive_stats,
    get_descriptive_stats(
      nodes_data
    )
  ),
  # Plot the evolution of the lower bound
  tar_target(
    lower_bound_plot,
    plot_lower_bound(third_clustering_run$EM_lower_bound)
  ),
  tar_target(
    save_lower_bound_plot,
    save_plot(lower_bound_plot, "outputs/lower_bound.pdf"),
    format = "file"
  ),
  # Plot the distribution of block sizes
  tar_target(
    block_size_plot,
    plot_block_size(third_clustering_run$partition)
  ),
  tar_target(
    save_block_size_plot,
    save_plot(block_size_plot, "outputs/block_size_distribution.pdf"),
    format = "file"
  ),
  # Gather the estimate results
  tar_target(
    coefficients_table,
    get_coef_table(coefficient_results)
  )
)
