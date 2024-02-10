

#' Get Random Seed
#' Obtains a random seed from the environment or from the System time
#' @return an integer seed
#' @export
get_random_seed <- function(){
  # Obtain a random seed from the system time
  current_time <-  Sys.time()
  current_time <- as.integer(current_time)
  random_seed <- current_time %% 100000

  # Check if there is a seed set in the environment variable. If set, use that one,
  # else, use the random seed.
  seed_from_env <- as.integer(Sys.getenv("PROJECT_SEED"))

  # If the seed cannot be parsed as an integer, use the random seed
  seed <- ifelse(is.na(seed_from_env), random_seed, seed_from_env)
  message(paste("Using seed:", seed))
  return(seed)
}

#' Save Plot
#'
#' @param plot a ggplot2 plot to save
#' @param file_path the path where to store the plot
#'
#' @importFrom ggplot2 ggsave
#' @export
#'
save_plot <- function(plot, file_path){
  ggsave(
    file_path,
    plot = plot,
    device = 'pdf',
    units = 'cm',
    width = 10.0,
    height = 10.0
  )
}

get_category_translations <- function(){
  return (tibble::tribble(
    ~large_category, ~large_category_en,
    "マスコミ業界の会社", "MULTIMEDIA",
    "広告業界の会社", "ADVERTISEMENT",
    "医療・福祉業界の会社", "MEDICAL CARE AND WELFARE",
    "食品業界の会社", "FOOD",
    "運輸・物流業界の会社", "TRANSPORTATION AND LOGISTICS",
    "エンタメ業界の会社", "ENTERTAINMENT",
    "コンサルティング業界の会社", "CONSULTING",
    "商社業界の会社", "TRADING",
    "アパレル・美容業界の会社", "APPAREL",
    "外食業界の会社", "RESTAURANTS",
    "不動産業界の会社", "REAL STATE",
    "生活用品業界の会社", "DAILY SUPPLIES",
    "人材業界の会社", "HUMAN RESOURCES",
    "金融業界の会社", "FINANCE",
    "IT業界の会社", "INFORMATION TECHNOLOGY",
    "その他サービス業界の会社", "OTHER SERVICES",
    "小売業界の会社", "RETAIL",
    "自動車・乗り物業界の会社", "VEHICLES",
    "ゲーム業界の会社", "GAMING",
    "教育業界", "EDUCATION",
    "専門サービス業界の会社", "SPECIALIZED SERVICES",
    "通信業界の会社", "TELECOMMUNICATIONS",
    "その他業界", "OTHERS",
    "電気製品業界の会社", "ELECTRONICS",
    "建設・工事業界の会社", "CONSTRUCTION",
    "機械業界の会社", "MACHINERY",
    "化学業界の会社", "CHEMICALS",
    "エネルギー業界の会社", "ENERGY",
    "通信機器業界の会社", "TELECOMMUNICATION EQUIPMENT",
    "製造業界の会社", "MANUFACTURING",
    "機械関連サービス業界の会社", "MACHINE-RELATED SERVICES"
  ) )
}

occupation_codes <- tibble::tribble(
  ~occupation_code, ~category,
  1,	"営業（フロント）",
  2,	"営業企画",
  3,	"マーケティング",
  4,	"商品企画",
  5,	"販売サービス系",
  6,	"コンサル系専門部門",
  7,	"経営企画・事業企画",
  8,	"経理・財務・管理会計",
  9,	"IR",
  10,	"総務",
  11,	"法務",
  12,	"購買",
  13,	"物流・ロジスティックス",
  14,	"広報",
  15,	"人事",
  16,	"お客様相談",
  17,	"秘書",
  18,	"内部統制・内部監査",
  19,	"金融系専門職",
  20,	"IT系専門職（情報システム部門、システム開発）",
  21,	"製造系専門職",
  22,	"建設系専門職",
  23,	"品質管理・品質保証",
  24,	"設計",
  25,	"開発",
  26,	"研究",
  27,	"クリエイティブ系専門職",
  101,	"支店・支所",
  102,	"海外支店",
  103,	"その他事業",
  104,	"その他管理",
  105,	"取締役・役員",
  106,	"その他技術系",
  201,	"分類不能",
  202,	"分類不能"
)


#' Plot the Lower Bound
#'
#' @param lower_bound a lower bound vector returned by `lighthergm_res$EM_lower_bound`
#'
#' @return a ggplot2 plot of the evolution of the lower bound with respect to the iteration number
#' @export
#' @import ggplot2
#'
plot_lower_bound <- function(lower_bound){
  lower_bound_df <- data.frame(
    lower_bound = lower_bound,
    iteration = 1:length(lower_bound)
  )
  ggplot(data = lower_bound_df) +
    geom_line(aes(x = iteration, y = lower_bound/1000000)) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(labels = scales::comma) +
    xlab("Iteration Number") +
    ylab("Lower Bound Value (millions)") +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank()
    )
}

#' Plot the Block Size Distribution
#'
#' @param partition a factor containing the ID of the block assigned to each node
#'
#' @return a ggplot2 plot of the distribution of block sizes
#' @export
#' @import ggplot2
#' @importFrom dplyr %>%
#'
plot_block_size <- function(partition) {
  blocks <- as.data.frame(
    table(partition) %>%
      sort()
  ) %>%
    dplyr::rename(block = partition, size = Freq)

  ggplot(data=blocks) +
    geom_bar(aes(x = block, y = size), stat="identity", width = 1) +
    xlab("Blocks") +
    ylab("Number of affiliated nodes") +
    theme_bw() +
    theme(
      axis.ticks.x = element_blank(),
      axis.text.x=element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank()
    )
}

#' Attach Block Affiliations
#'
#' @param nodes a dataframe containing node covariates
#' @param blocks a factor containing the block IDs for each node (in the same order as the one used in the network object)
#' @param g a statnet network object
#'
#' @return a tibble containing the node covariates and the block affiliations
#' @export
#' @importFrom  tibble tibble
#' @importFrom magrittr %>%
#'
get_nodes_with_block <- function(nodes, blocks, g){
  return (
    tibble(
      id = network::network.vertex.names(g),
      block = blocks
    ) %>% dplyr::left_join(
      nodes,
      by = "id"
    )
  )
}


#' Get Data Descriptive Statistics
#'
#' @param nodes a dataframe containing node covariates
#'
#' @return a list containing industry, occupation and city counts
#' @importFrom magrittr %>%
#' @import dplyr
#' @export
#'
get_descriptive_stats <- function(nodes){
  industry_counts <- nodes %>%
    group_by(large_category) %>%
    summarise(industry_count = n()) %>%
    arrange(desc(industry_count)) %>%
    mutate(industry_share = industry_count/sum(industry_count)) %>%
    left_join(get_category_translations())

  occupation_counts <- nodes %>%
    group_by(occupation_code) %>%
    summarise(occupation_count = n()) %>%
    arrange(desc(occupation_count)) %>%
    mutate(occupation_share = occupation_count/sum(occupation_count)) %>%
    left_join(occupation_codes)

  city_counts <- nodes %>%
    group_by(city) %>%
    summarise(city_count = n()) %>%
    arrange(desc(city_count))

  return(
    list(
      industry_counts = industry_counts,
      city_counts = city_counts,
      occupation_counts = occupation_counts
    )
  )
}


#' Get a Table of Coefficient Estimates
#'
#' @param hergm_results An estimation result returned by `lighthergm::hergm()`
#' with `estimate_coefficients = TRUE`.
#' @param format_output filename or object type (character string).
#' See the help page of `modelsummary::modelsummary()`.
#' @return a `modelsummary_list` table containing the coefficient estimates
#' @export
get_coef_table <- function(hergm_results, format_output = "default") {
  res_table <-
    c("markdown", format_output) %>%
    purrr::map(create_modelsummary, hergm_results = hergm_results)

  print(res_table[[1]])
  return(res_table[[2]])
}


create_modelsummary <- function(format_output, hergm_results) {
  modelsummary::modelsummary(
    list(
      "(1)" = hergm_results$est_between,
      "(2)" = hergm_results$est_within
    ),
    output = format_output,
    stars = TRUE,
    coef_rename =
      c(
        "edges" = "Intercept ($\\alpha$)",
        "triangle" = "Shared Contacts ($\\gamma$)",
        "nodematch.h3_7" = "Same Location (H3 Tile) ($\\beta_{1}$)",
        "nodematch.large_category" = "Same Industry ($\\beta_{2}$)",
        "nodematch.occupation_code" = "Same Occupation ($\\beta_{3}$)"
      ),
    gof_omit = "^(?!.*BIC)"
  ) %>%
    kableExtra::add_header_above((c(" " = 1, "Between" = 1, "Within" = 1)))
}
