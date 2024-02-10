library(magrittr)

generate_dummy_data <- function(n_nodes, n_cluster) {
  title_rank <- 1:5
  occupation_code <- c(1:27, 101:106, 201, 202)
  large_category <- c(
    "マスコミ業界の会社",
    "広告業界の会社",
    "医療・福祉業界の会社",
    "食品業界の会社",
    "運輸・物流業界の会社",
    "エンタメ業界の会社",
    "コンサルティング業界の会社",
    "商社業界の会社",
    "アパレル・美容業界の会社",
    "外食業界の会社",
    "不動産業界の会社",
    "生活用品業界の会社",
    "人材業界の会社",
    "金融業界の会社",
    "IT業界の会社",
    "その他サービス業界の会社",
    "小売業界の会社",
    "自動車・乗り物業界の会社",
    "ゲーム業界の会社",
    "教育業界",
    "専門サービス業界の会社",
    "通信業界の会社",
    "その他業界",
    "電気製品業界の会社",
    "建設・工事業界の会社",
    "機械業界の会社",
    "化学業界の会社",
    "エネルギー業界の会社",
    "通信機器業界の会社",
    "製造業界の会社",
    "機械関連サービス業界の会社"
  )
  prefecture <- c("東京都")

  sampler <- purrr::partial(sample, replace = TRUE, size = n_nodes)

  df_feature <- list(
    "title_rank" = title_rank,
    "occupation_code" = occupation_code,
    "large_category" = large_category,
    "prefecture" = prefecture
  ) %>%
    purrr::imap_dfc(~tibble::tibble(!!(.y) := sampler(.x)))

  df_random_feature <-
    c("main_category", "city", "h3_5", "h3_6", "h3_7", "h3_8", "h3_9", "h3_10", "num_month") %>%
    purrr::set_names() %>%
    purrr::map_dfc(~sampler(as.character(1:20)))

  df_node_dummy <- tibble::tibble(
    id = as.character(1:n_nodes),
    cluster = rep(1:n_cluster, each = n_nodes/n_cluster)
  ) %>%
    dplyr::bind_cols(df_feature, df_random_feature)

  g_dummy <- lighthergm::simulate_hergm(
    formula_for_simulation = g ~ edges + triangle + nodematch("h3_7") + nodematch("large_category") + nodematch("occupation_code"),
    data_for_simulation = df_node_dummy,
    colname_vertex_id = "id",
    colname_block_membership = "cluster",
    coef_within_block = c(-1, 0.001, 0.5, 0.5, 0.5),
    coef_between_block = c(-3, 0.2, 0.2, 0.2),
    ergm_control = ergm::control.simulate.formula(
      MCMC.burnin = 10000000
    )
  )

  list_net <- intergraph::asDF(g_dummy)

  df_edge_dummy <- list_net$edges %>%
    dplyr::rename(src = V1, dst = V2) %>%
    dplyr::mutate(dplyr::across(c(src, dst), as.character))

  return(list(edges = df_edge_dummy, nodes = df_node_dummy))
}

# Generate a dummy network and save it as CSV
dummy_net <- withr::with_seed(
  seed = 334,
  code = generate_dummy_data(n_nodes = 3000, n_cluster = 100)
)

readr::write_csv(dummy_net$edges, "data/edges_dummy.csv")
readr::write_csv(dummy_net$nodes, "data/nodes_dummy.csv")
