#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.

renv::restore(prompt = FALSE)
devtools::install(upgrade = FALSE, dependencies = FALSE)

# To run the pipeline with dummy data, set the R_CONFIG_ACTIVE environment variable to "dummy".
# Otherwise, leave R_CONFIG_ACTIVE unspecified or set it to "default".
projects <- config::get("tar_projects")

# An auxiliary function to run a pipeline
run_pipeline <- function(tar_project, envvar_config = Sys.getenv("R_CONFIG_ACTIVE")) {
  withr::with_envvar(
    new = c("TAR_PROJECT" = tar_project),
    code = targets::tar_make(callr_arguments = list(env = c(R_CONFIG_ACTIVE = envvar_config)))
  )
}

# Execute data processing and estimation pipelines
message(paste0("Pipeline mode: ", attr(config::get(), "config")))
purrr::walk(projects, run_pipeline)
