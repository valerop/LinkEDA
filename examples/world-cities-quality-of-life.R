# Synthetic world-city housing and quality-of-life data
#
# The city names are real labels, but every numerical value in this example is
# fictitious. Do not use these data to compare real cities or make decisions.
# The small number of missing values is intentional so the data frame can be
# used to demonstrate missing-data exploration and multiple imputation.

world_cities_qol <- data.frame(
  city = c(
    "Valencia", "Lisbon", "Barcelona", "Paris", "Berlin", "Copenhagen",
    "London", "New York", "Toronto", "Mexico City", "Sao Paulo",
    "Buenos Aires", "Cape Town", "Nairobi", "Cairo", "Istanbul", "Tokyo",
    "Seoul", "Singapore", "Bangkok", "Mumbai", "Sydney", "Auckland", "Dubai"
  ),
  region = factor(c(
    rep("Europe", 7), rep("North America", 2), rep("Latin America", 3),
    rep("Africa", 3), "Middle East", rep("Asia", 5), rep("Oceania", 2),
    "Middle East"
  )),
  city_size = ordered(
    c(
      "medium", "large", "large", "megacity", "large", "medium",
      "megacity", "megacity", "large", "megacity", "megacity", "large",
      "large", "large", "megacity", "megacity", "megacity", "megacity",
      "large", "megacity", "megacity", "large", "medium", "large"
    ),
    levels = c("medium", "large", "megacity")
  ),
  coastal = factor(
    c(
      "yes", "yes", "yes", "no", "no", "yes", "no", "yes", "yes",
      "no", "no", "yes", "yes", "no", "no", "yes", "yes", "yes",
      "yes", "no", "yes", "yes", "yes", "yes"
    ),
    levels = c("no", "yes")
  ),
  apartment_price_eur_m2 = c(
    3100, 4700, 5200, 9100, 5900, 7200, 10500, 11800, 6800, 2400, 2100,
    NA, 2300, 1400, 1200, 3300, 9800, 8600, 11200, 2600, 1900, 9000, NA, 6100
  ),
  monthly_rent_eur = c(
    1050, 1450, 1600, 2350, 1550, 1950, 2700, 3200, 1950, 850, 760, 700,
    820, 540, 430, 920, 2150, 1800, 2550, NA, 620, 2250, 1750, 1900
  ),
  median_net_salary_eur = c(
    2100, 2050, 2450, 3400, 3300, 4200, 3900, 5100, 3900, 1250, 1050, 900,
    1350, 720, 520, 1250, 4100, 3600, 4300, 1150, NA, 4200, 3500, 3800
  ),
  safety_index_0_100 = c(
    78, 76, 71, 65, 73, 84, 66, 58, 79, 55, 50, 62, 53, 57, 60, 64, 88,
    82, 91, 68, 59, 83, 86, 89
  ),
  healthcare_index_0_100 = c(
    82, 79, 84, 88, 86, 91, 85, 80, 87, 69, 66, 72, 68, NA, 61, 73, 92,
    90, 89, 71, 63, 88, 86, 81
  ),
  public_transport_index_0_100 = c(
    76, 74, 85, 94, 91, 88, 93, 89, 82, 78, 75, 72, 67, 61, 70, 84, 96,
    95, 90, 86, 80, 79, 73, 77
  ),
  green_space_pct = c(
    18, 16, 14, 17, 24, 26, 20, 13, 27, 15, 12, 19, 21, 14, 10, 13, 18,
    16, 22, 17, 11, 29, 31, 15
  ),
  air_quality_index_0_100 = c(
    78, 74, 70, 68, 76, 88, 71, 62, 80, 55, 51, 67, 73, 49, 42, 58, 75,
    66, 86, 54, NA, 82, 90, 60
  ),
  commute_minutes = c(
    29, 31, 34, 43, 38, 32, 47, 51, 40, 48, 52, 45, 39, 46, 50, 44, 42,
    49, 36, 47, 56, 37, NA, 41
  ),
  life_satisfaction_0_10 = c(
    7.8, 7.5, 7.3, 7.0, 7.4, 8.2, 7.1, 6.8, 7.9, 6.9, 6.5, 7.2, 7.0,
    6.6, 6.3, 6.8, 7.7, 7.5, 8.1, 7.0, 6.4, 8.0, 8.3, NA
  ),
  stringsAsFactors = FALSE
)

# Exactly eight intentionally missing cells, all in numerical analysis fields.
stopifnot(
  nrow(world_cities_qol) == 24L,
  ncol(world_cities_qol) == 14L,
  sum(is.na(world_cities_qol)) == 8L,
  is.ordered(world_cities_qol$city_size)
)

# Optional helper for LinkEDA's multiple-imputation workflow. It deliberately
# delegates imputation to mice and prevents the city label from being imputed
# or used as a predictor.
make_world_cities_qol_mids <- function(m = 5L, seed = 20260829L) {
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("Install the 'mice' package to create the multiple-imputation object.")
  }

  variable_names <- names(world_cities_qol)
  method <- stats::setNames(rep("", length(variable_names)), variable_names)
  predictor_matrix <- matrix(
    0,
    nrow = length(variable_names),
    ncol = length(variable_names),
    dimnames = list(variable_names, variable_names)
  )
  incomplete <- names(which(vapply(world_cities_qol, anyNA, logical(1))))
  method[incomplete] <- "pmm"

  # A deliberately compact predictor set avoids overfitting this 24-row demo.
  predictor_matrix["apartment_price_eur_m2", c(
    "monthly_rent_eur", "median_net_salary_eur", "city_size"
  )] <- 1
  predictor_matrix["monthly_rent_eur", c(
    "apartment_price_eur_m2", "median_net_salary_eur", "city_size"
  )] <- 1
  predictor_matrix["median_net_salary_eur", c(
    "apartment_price_eur_m2", "monthly_rent_eur", "healthcare_index_0_100"
  )] <- 1
  predictor_matrix["healthcare_index_0_100", c(
    "safety_index_0_100", "life_satisfaction_0_10", "median_net_salary_eur"
  )] <- 1
  predictor_matrix["air_quality_index_0_100", c(
    "green_space_pct", "safety_index_0_100", "coastal"
  )] <- 1
  predictor_matrix["commute_minutes", c(
    "public_transport_index_0_100", "city_size", "apartment_price_eur_m2"
  )] <- 1
  predictor_matrix["life_satisfaction_0_10", c(
    "safety_index_0_100", "healthcare_index_0_100", "green_space_pct"
  )] <- 1

  mice::mice(
    world_cities_qol,
    m = as.integer(m),
    method = method,
    predictorMatrix = predictor_matrix,
    seed = as.integer(seed),
    printFlag = FALSE
  )
}

# Examples:
# LinkEDA::LinkEDA(world_cities_qol, name = "Synthetic world cities")
# cities_mids <- make_world_cities_qol_mids()
# LinkEDA::ls_import_mice(cities_mids, name = "Synthetic world cities MI")
