# =========================
# 0. Start
# =========================
library(dplyr)
library(tidyr)
library(ggplot2)
library(Matrix)
library(igraph)
if (requireNamespace("vscDebugger", quietly = TRUE)) {
  library(vscDebugger)
}
has_modelsummary <- requireNamespace("modelsummary", quietly = TRUE)

set.seed(123)

# Save outputs to the project folder whenever it exists. This avoids a common
# RStudio problem: getwd() can point somewhere different each time, so the cache
# appears to be "missing" and the simulation runs again.
project_output_dir <- "D:/master/M2/Applied Labor Economics/final/our code"
script_path <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
output_dir <- if (dir.exists(project_output_dir)) {
  project_output_dir
} else if (!is.na(script_path)) {
  dirname(script_path)
} else {
  getwd()
}
simulation_cache <- file.path(output_dir, "simulated_data_cache.rds")
simulation_cache_version <- 2L
force_resimulate <- FALSE

cache_is_current <- FALSE
if (!force_resimulate && file.exists(simulation_cache)) {
  cached <- readRDS(simulation_cache)
  cache_is_current <- is.list(cached) &&
    isTRUE(cached$version == simulation_cache_version) &&
    all(c("df", "df_simulated") %in% names(cached))
}

if (cache_is_current) {
  df <- cached$df
  df_simulated <- cached$df_simulated
  print(paste0("Loaded cached simulated data from: ", simulation_cache))
  print(paste0("Cached worker-period rows: ", nrow(df)))
  print(paste0("Cached firm-period rows: ", nrow(df_simulated)))
}

# this file:
# 1. the key is to simulate the number of workers move from firm k to firm j;
# 2. for each firm, there should be firm wage premia & worker's value of the firm (latent)

# our idea:
# women sorted to firms with lower wage premium even in pre period
# women are more likely to receive low premium even within the same firm compared to men
# Covid imposed same firm specific shock on non-wage factorsfor men and women
# (as Covid makes WFH more available, bringing in more flexibility)
# but female further sort out to firms with lower wage premium
# because they value non wage factors than men ? or even more than themselves before Covid?
# (as women are more likely to be the main caregivers at home & caregiving is more critical in the post-Covid period)

# our assumptions:
# 1. Men and women operate in different labor markets:
#  different wage premium (normaly lower for women) and value of non-wage factors
# 2. Covid shock is likely to have impacted these two markets at the same time but to different extents:
# 2.1. worker fixed effect pre/post unchanged: alpha_i
# 2.2. firm wage premium pre/post changed: psi
# 2.3. firm non-wage factor pre/post changed: amenity
# 2.4. female post-COVID non-wage shock is stronger: rho
# 2.5. female further sort out to firms with lower wage premium

# =========================
# 1. Basic parameters
# =========================
years <- c(2018, 2019, 2021, 2022)
pre_years <- c(2018, 2019)
post_years <- c(2021, 2022)
year_pairs <- data.frame(
  year1 = c(2018, 2021),
  year2 = c(2019, 2022),
  period = c("pre", "post")
)

n_firms <- 1500
n_workers <- 300000
worker_id <- 1:n_workers

offer_shock_sd <- 0.15
full_rank_damping <- 0.85

if (!cache_is_current) {
  female <- rbinom(n_workers, 1, 0.5)
  birth_year <- sample(1965:1995, n_workers, replace = TRUE) # or 1993-1967??

  # earntype_coarse1 <- sample(c(1, 2), n_workers, replace = TRUE, prob = c(0.3, 0.7))
  # earntype_coarse2 <- sample(c(1, 2), n_workers, replace = TRUE, prob = c(0.3, 0.7))

  # worker fixed effects (same for each year)
  alpha_i <- rnorm(n_workers, mean = 0, sd = 0.4)

  # Firm wage premium: (should be estimated with real data from AKM)
  # 1. common firm wage premium base (sd = 0.3)
  psi_base <- scale(rnorm(n_firms))[, 1] * 0.3
  # 2. COVID shock on wage premium (sd = 0.08)
  psi_covid_shock <- scale(rnorm(n_firms))[, 1] * 0.08
  # 3. women more likely to receive lower wage premium within same firm
  female_penalty <- rnorm(n_firms, mean = -0.12, sd = 0.04)
  print(paste0("mean of female_penalty:", mean(female_penalty)))
  print(paste0("proportion of women receiving higher wage premium:", mean(female_penalty > 0)))

  # Non-wage factors:
  # 1. firm non-wage amenity base: higher = better non-wage value
  amenity_base <- scale(rnorm(n_firms))[, 1]
  # 2. Covid shock on non-wage factors (mostly improve as WFH is more common)
  amenity_covid_shock <- rnorm(n_firms, mean = 0.25, sd = 0.25)
  print(paste0("mean of amenity_covid_shock:", mean(amenity_covid_shock)))
  print(paste0("proportion of improved amenity:", mean(amenity_covid_shock > 0)))
}

# =========================
# 2. Simulation
# =========================

if (cache_is_current) {
  # The simulation is large. Reuse it unless you delete simulated_data_cache.rds
  # or change the file name above.
} else {
  print(paste0("Cache path checked: ", simulation_cache))
  print("No current cached simulated data found. Running the simulation now.")

  all_data <- list()
  all_simulated <- list()

  for (p in seq_len(nrow(year_pairs))) {
    year1 <- year_pairs$year1[p]
    year2 <- year_pairs$year2[p]
    period <- year_pairs$period[p]

    post <- ifelse(period == "post", 1, 0)

    age1 <- year1 - birth_year
    age2 <- year2 - birth_year

    exp1 <- age1 - 22 # perfect linearity
    exp2 <- age2 - 22

    #--------------------------
    # 2.1. OUR DGP!
    #--------------------------

    # correlation between wage premium and value: after Covid female value non-wage even more
    rho_female <- ifelse(post == 0, 0.6, 0.3)
    rho_male <- ifelse(post == 0, 0.8, 0.7)
    rho <- ifelse(female == 1, rho_female, rho_male)

    # firm-level wage premium
    psi_common <- psi_base + post * psi_covid_shock
    psi_common <- scale(psi_common)[, 1] * 0.3

    # women receive lower premium within same firm
    psi_male <- psi_common
    psi_female <- psi_common + female_penalty

    # common firm-specific non-wage shock for men and women
    amenity <- amenity_base + post * amenity_covid_shock
    amenity <- scale(amenity)[, 1]

    # gender-specific latent firm value
    V_male_true <- rho_male * scale(psi_male)[, 1] +
      sqrt(1 - rho_male^2) * amenity

    V_female_true <- rho_female * scale(psi_female)[, 1] +
      sqrt(1 - rho_female^2) * amenity

    #--------------------------
    # 2.2. firm-level objects
    #--------------------------

    lambda1 <- 0.20 # probability an employed worker receives an outside offer (find_lambda.m from Sorkin)
    lambda_ene_base <- 0.08 # probability of an indirect employer change through nonemployment
    sigma_eps <- 1 # scale of idiosyncratic utility shock

    # firm size in the initial assignment: higher value = higher attractiveness = higher firm size
    V_avg_true <- 0.5 * V_male_true + 0.5 * V_female_true
    firm_size <- exp(0.3 * V_avg_true)
    firm_size_prob <- firm_size / sum(firm_size)

    # offer intensity f_j:
    # Large random offer shocks make recovered ranks reflect recruiting intensity,
    # not only worker-valued firm quality.
    offer_shock <- rnorm(n_firms, 0, offer_shock_sd)
    offer <- exp(0.5 * V_avg_true + offer_shock)
    offer_prob <- offer / sum(offer)

    #--------------------------
    # 2.3. initial firms
    #--------------------------

    current_firm <- sample(1:n_firms, size = n_workers, replace = TRUE, prob = firm_size_prob)

    psi_current <- ifelse(
      female == 1,
      psi_female[current_firm],
      psi_male[current_firm]
    )

    # baseline wage in year 1: 10 + worker fixed effect + firm fixed effect + age and its polinomials (without exp1) + non-wage factor
    logy1 <- 10 + alpha_i + psi_current +
      0.03 * age1 - 0.0002 * age1^2 + 0.00001 * age1^3 +
      rnorm(n_workers, 0, 0.2)

    #--------------------------
    # 2.4. outside offers and job-to-job decisions
    #--------------------------

    gets_offer <- rbinom(n_workers, size = 1, prob = lambda1)

    # offered firm
    offer_firm <- sample(1:n_firms, size = n_workers, replace = TRUE, prob = offer_prob)

    # prevent same-firm "offers" by redrawing when necessary
    same_firm <- which(offer_firm == current_firm)
    while (length(same_firm) > 0) {
      offer_firm[same_firm] <- sample(1:n_firms, size = length(same_firm), replace = TRUE, prob = offer_prob)
      same_firm <- which(offer_firm == current_firm)
    }

    # worker compares utility current vs utility offer: U = common firm value + iid shock
    V_current <- ifelse(
      female == 1,
      V_female_true[current_firm],
      V_male_true[current_firm]
    )

    V_offer <- ifelse(
      female == 1,
      V_female_true[offer_firm],
      V_male_true[offer_firm]
    )

    eps_current <- rnorm(n_workers, 0, sigma_eps)
    eps_offer <- rnorm(n_workers, 0, sigma_eps)

    U_current <- V_current + eps_current
    U_offer <- V_offer + eps_offer

    # revealed preferences: direct employer-to-employer move
    move <- ifelse(gets_offer == 1 & U_offer > U_current, 1, 0)

    # Indirect employer change through nonemployment.
    # Lower-valued firms have higher separation risk, and separated workers draw
    # their next employer from the same offer-arrival distribution.
    ene_prob <- plogis(qlogis(lambda_ene_base) - 0.35 * V_current)
    ene <- as.integer(move == 0 & rbinom(n_workers, size = 1, prob = ene_prob) == 1)

    ene_firm <- sample(1:n_firms, size = n_workers, replace = TRUE, prob = offer_prob)
    same_firm_ene <- which(ene == 1 & ene_firm == current_firm)
    while (length(same_firm_ene) > 0) {
      ene_firm[same_firm_ene] <- sample(
        1:n_firms,
        size = length(same_firm_ene),
        replace = TRUE,
        prob = offer_prob
      )
      same_firm_ene <- which(ene == 1 & ene_firm == current_firm)
    }

    next_firm <- ifelse(move == 1, offer_firm, ifelse(ene == 1, ene_firm, current_firm))

    psi_next <- ifelse(
      female == 1,
      psi_female[next_firm],
      psi_male[next_firm]
    )

    # year 2 wages
    logy2 <- 10 + alpha_i + psi_next +
      0.03 * age2 - 0.0002 * age2^2 + 0.00001 * age2^3 +
      rnorm(n_workers, 0, 0.2)

    # EE indicator: 1 if changed employer directly
    ee <- as.integer(move == 1)

    # ENE indicator: 1 if changed employer indirectly via nonemployment
    ene <- as.integer(ene == 1)

    #--------------------------
    # 2.5. simulated dataset
    #--------------------------

    matrix_sim <- data.frame(
      worker_id = worker_id,
      female = female,
      year1 = year1,
      year2 = year2,
      period = period,
      age1 = age1,
      age2 = age2,
      index1 = current_firm,
      index2 = next_firm,
      logy1 = logy1,
      logy2 = logy2,
      ee = ee,
      ene = ene
    )

    simulated <- data.frame(
      firm_id = 1:n_firms,
      period = period,
      psi_male = psi_male,
      psi_female = psi_female,
      amenity = amenity,
      female_penalty = female_penalty,
      rho_male = rho_male,
      rho_female = rho_female,
      V_male_true = V_male_true,
      V_female_true = V_female_true
    )

    all_data[[p]] <- matrix_sim
    all_simulated[[p]] <- simulated
  }

  df <- bind_rows(all_data) #
  df_simulated <- bind_rows(all_simulated)

  saveRDS(
    list(
      version = simulation_cache_version,
      df = df,
      df_simulated = df_simulated
    ),
    simulation_cache
  )
  print(paste0("Saved simulated data cache to: ", simulation_cache))
}

stopifnot(all(df$ee + df$ene <= 1))

sorkin_empty_rank <- function(period_keep, gender_keep, method_keep) {
  data.frame(
    firm_id = integer(),
    period = character(),
    female = integer(),
    method = character(),
    V_hat = numeric()
  )
}

require_rows <- function(data, min_rows, label) {
  if (nrow(data) < min_rows) {
    stop(paste0(
      label,
      " has only ",
      nrow(data),
      " usable rows. Check mobility rates or estimator connectivity before plotting."
    ))
  }
  invisible(data)
}

build_count_matrix <- function(origin, dest, n_nodes) {
  if (length(origin) == 0) {
    return(matrix(0, nrow = n_nodes, ncol = n_nodes))
  }

  # M[dest, origin] counts moves from origin to destination.
  # sparseMatrix automatically sums duplicate origin-destination pairs.
  as.matrix(sparseMatrix(
    i = dest,
    j = origin,
    x = 1,
    dims = c(n_nodes, n_nodes)
  ))
}

standardize_or_center <- function(x) {
  sx <- sd(x, na.rm = TRUE)
  if (is.na(sx) || sx == 0) {
    return(as.numeric(x - mean(x, na.rm = TRUE)))
  }
  as.numeric((x - mean(x, na.rm = TRUE)) / sx)
}

largest_component_nodes <- function(M, strong = TRUE) {
  g <- graph_from_adjacency_matrix(
    t(M),
    mode = "directed",
    weighted = TRUE,
    diag = FALSE
  )

  scc <- components(g, mode = ifelse(strong, "strong", "weak"))
  which(scc$membership == which.max(scc$csize))
}

recover_sorkin_value <- function(M, node_ids, min_nodes = 10) {
  diag(M) <- 0

  # First try the strict Sorkin choice: largest strongly connected component.
  # In small simulated subsamples this can occasionally be too small, especially
  # when many transitions pass through nonemployment. In that case we fall back
  # to the largest weakly connected active component so the validation plots do
  # not become silently empty.
  keep <- largest_component_nodes(M, strong = TRUE)
  active <- which(rowSums(M) > 0 & colSums(M) > 0)
  used_weak_fallback <- FALSE

  if (length(intersect(keep, active)) < min_nodes) {
    keep <- intersect(largest_component_nodes(M, strong = FALSE), active)
    used_weak_fallback <- TRUE
    message(
      "Using weak-component fallback because the strict strongly connected ",
      "set had fewer than ", min_nodes, " active nodes."
    )
  }

  if (length(keep) < 2) {
    return(data.frame(node_id = integer(), V_hat = numeric()))
  }

  M_sub <- M[keep, keep, drop = FALSE]
  node_ids_sub <- node_ids[keep]

  if (used_weak_fallback) {
    # The weak component is a backup for simulated subsamples with thin flows.
    # A tiny off-diagonal pseudo-flow prevents a reducible matrix from putting
    # all eigenvector mass on one terminal subset. It is deliberately tiny
    # relative to actual counts and is not used when the strict SCC works.
    pseudo_flow <- 1e-6
    M_sub <- M_sub + pseudo_flow
    diag(M_sub) <- 0
  }

  # This mirrors tilde_exptV.m in the reference code:
  # M_sub[dest, origin] stores moves from origin to destination.
  # The normalization follows tilde_exptV.m: S = diag(sum(M_sub));
  # A = solve(S) %*% M_sub.
  outflow <- colSums(M_sub)
  keep_positive <- which(outflow > 0)

  if (length(keep_positive) < 2) {
    return(data.frame(node_id = integer(), V_hat = numeric()))
  }

  M_sub <- M_sub[keep_positive, keep_positive, drop = FALSE]
  node_ids_sub <- node_ids_sub[keep_positive]
  S_sub <- diag(colSums(M_sub))
  A <- solve(S_sub) %*% M_sub

  eig <- eigen(A)
  idx <- which.max(Re(eig$values))

  q <- abs(Re(eig$vectors[, idx]))
  q <- pmax(q, 1e-10)

  data.frame(
    node_id = node_ids_sub,
    V_hat = standardize_or_center(log(q))
  )
}

recover_augmented_value <- function(M, node_ids, damping = full_rank_damping) {
  diag(M) <- 0
  active <- which(rowSums(M) > 0 | colSums(M) > 0)

  if (length(active) < 2) {
    return(data.frame(node_id = integer(), V_hat = numeric()))
  }

  M_sub <- M[active, active, drop = FALSE]
  node_ids_sub <- node_ids[active]
  outflow <- colSums(M_sub)
  n <- nrow(M_sub)

  # Full EE+ENE matrices can be weakly connected because nonemployment is a hub.
  # To keep the estimator usable in simulation, we use a damped transition matrix:
  # columns with no outflow send equal mass everywhere, and every column receives
  # a small teleportation probability. This is a stable approximation to the
  # full mobility ranking, while the EE-only option above keeps the stricter
  # Sorkin eigenvector.
  A <- matrix(1 / n, nrow = n, ncol = n)
  positive_outflow <- which(outflow > 0)
  A[, positive_outflow] <- sweep(
    M_sub[, positive_outflow, drop = FALSE],
    2,
    outflow[positive_outflow],
    "/"
  )

  A <- damping * A + (1 - damping) * matrix(1 / n, nrow = n, ncol = n)

  eig <- eigen(A)
  idx <- which.max(Re(eig$values))
  q <- abs(Re(eig$vectors[, idx]))
  q <- pmax(q / sum(q), 1e-10)

  data.frame(
    node_id = node_ids_sub,
    V_hat = standardize_or_center(log(q))
  )
}

recover_full_flow_score <- function(data, period_keep, gender_keep) {
  d <- data %>%
    filter(period == period_keep)

  if (!is.null(gender_keep)) {
    d <- d %>% filter(female == gender_keep)
  }

  # Full-flow validation score:
  # - EE and ENE destinations are evidence that a firm is attractive.
  # - EE and ENE origins are evidence that workers leave that firm.
  # - The +1 smoothing keeps every firm in the graph and prevents log(0).
  # This is intentionally simpler and more stable than the structural Sorkin
  # loop. It is the right object for checking whether the simulated mobility
  # patterns point toward the true latent firm value.
  transition_rows <- d %>%
    filter((ee == 1 | ene == 1), index1 != index2)

  inflow <- transition_rows %>%
    count(index2, name = "inflow") %>%
    rename(firm_id = index2)

  outflow <- transition_rows %>%
    count(index1, name = "outflow") %>%
    rename(firm_id = index1)

  data.frame(firm_id = 1:n_firms) %>%
    left_join(inflow, by = "firm_id") %>%
    left_join(outflow, by = "firm_id") %>%
    mutate(
      inflow = replace_na(inflow, 0),
      outflow = replace_na(outflow, 0),
      V_hat = standardize_or_center(log((inflow + 1) / (outflow + 1))),
      period = period_keep,
      female = gender_keep,
      method = "full"
    ) %>%
    select(firm_id, period, female, method, V_hat)
}

# =========================
# 3. revealed-preference ranking function
# =========================

estimate_rank <- function(data, period_keep, gender_keep = NULL, method_keep = "ee") {
  d <- data %>%
    filter(period == period_keep)

  if (!is.null(gender_keep)) {
    d <- d %>% filter(female == gender_keep)
  }

  if (!method_keep %in% c("ee", "full")) {
    stop("method_keep must be either 'ee' or 'full'")
  }

  #--------------------------
  # 3.1 construct mobility matrix M[j, k] = moves from k to j
  #--------------------------

  ee_rows <- d %>% filter(ee == 1, index1 != index2)
  ene_rows <- d %>% filter(ene == 1, index1 != index2)

  print(paste0(
    "Period: ", period_keep,
    ", gender: ", gender_keep,
    ", method: ", method_keep,
    ", Number of EE moves: ", nrow(ee_rows),
    ", Number of ENE moves: ", nrow(ene_rows)
  ))

  if (method_keep == "ee" && nrow(ee_rows) == 0) {
    return(sorkin_empty_rank(period_keep, gender_keep, method_keep))
  }

  if (method_keep == "full" && (nrow(ee_rows) + nrow(ene_rows)) == 0) {
    return(sorkin_empty_rank(period_keep, gender_keep, method_keep))
  }

  if (method_keep == "ee") {
    node_ids <- 1:n_firms
    M <- build_count_matrix(
      origin = ee_rows$index1,
      dest = ee_rows$index2,
      n_nodes = n_firms
    )
    recovered <- recover_sorkin_value(M, node_ids)
    return(recovered %>%
      transmute(
        firm_id = node_id,
        period = period_keep,
        female = gender_keep,
        method = method_keep,
        V_hat = V_hat
      ))
  }

  recover_full_flow_score(data, period_keep, gender_keep)
}

# =========================
# 5. Estimation
# =========================

#--------------------------
# 5.0 data preparation & summary
#--------------------------

wage_long <- bind_rows(
  df %>%
    transmute(
      worker_id,
      female,
      period,
      year = year1,
      age = age1,
      firm_id = index1,
      logy = logy1
    ),
  df %>%
    transmute(
      worker_id,
      female,
      period,
      year = year2,
      age = age2,
      firm_id = index2,
      logy = logy2
    )
) %>%
  mutate(
    female = as.integer(female),
    gender = ifelse(female == 1, "Female", "Male"),
    post = as.integer(period == "post")
  )


summary_table <- wage_long %>%
  group_by(gender, period) %>%
  summarise(
    n_obs = n(),
    n_workers = n_distinct(worker_id),
    n_firms = n_distinct(firm_id),
    mean_log_wage = mean(logy, na.rm = TRUE),
    sd_log_wage = sd(logy, na.rm = TRUE),
    p10_log_wage = quantile(logy, 0.10, na.rm = TRUE),
    p50_log_wage = quantile(logy, 0.50, na.rm = TRUE),
    p90_log_wage = quantile(logy, 0.90, na.rm = TRUE),
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    .groups = "drop"
  )

if (has_modelsummary) {
  modelsummary::datasummary_df(
    summary_table,
    output = file.path(output_dir, "summary_statistics.tex"),
    title = "Summary Statistics by Gender and Period"
  )
} else {
  write.csv(
    summary_table,
    file.path(output_dir, "summary_statistics.csv"),
    row.names = FALSE
  )
  print("Package 'modelsummary' not installed; saved summary_statistics.csv instead.")
}

firm_gap <- df_simulated %>%
  mutate(
    post = as.integer(period == "post"),
    psi_gap = psi_female - psi_male,
    V_gap_true = V_female_true - V_male_true
  )

mobility_summary <- df %>%
  group_by(period, female) %>%
  summarise(
    ee_moves = sum(ee),
    ene_moves = sum(ene),
    stayers = sum(ee == 0 & ene == 0),
    ee_rate = mean(ee),
    ene_rate = mean(ene),
    .groups = "drop"
  )

print(mobility_summary)

#--------------------------
# 5.1 gender pay gap (simulated data) and the the impact of Covid
#--------------------------

pay_gap_summary <- wage_long %>%
  group_by(period, female) %>%
  summarise(
    mean_logy = mean(logy),
    sd_logy = sd(logy),
    n = n(),
    .groups = "drop"
  )

pay_gap_did <- lm(logy ~ female * post + age + I(age^2) + I(age^3), data = wage_long)

print(pay_gap_summary)
print(summary(pay_gap_did))

#--------------------------
# 5.2 gender gap in firm premium (simulated data)  and the the impact of Covid
#--------------------------

firm_premium_gap_summary <- firm_gap %>%
  group_by(period) %>%
  summarise(
    mean_psi_male = mean(psi_male),
    mean_psi_female = mean(psi_female),
    mean_psi_gap = mean(psi_gap),
    sd_psi_gap = sd(psi_gap),
    n_firms = n(),
    .groups = "drop"
  )

firm_premium_gap_did <- lm(psi_gap ~ post, data = firm_gap)

print(firm_premium_gap_summary)
print(summary(firm_premium_gap_did))

#--------------------------
# 5.3 V_hat and the the impact of Covid
#--------------------------

# get full-model V_hat for pre and post period & male and female separately
rank_method <- "full"

rank_male_pre <- estimate_rank(df, period_keep = "pre", gender_keep = 0, method_keep = rank_method) %>%
  transmute(firm_id, V_male_pre = V_hat)

rank_male_post <- estimate_rank(df, period_keep = "post", gender_keep = 0, method_keep = rank_method) %>%
  transmute(firm_id, V_male_post = V_hat)

rank_female_pre <- estimate_rank(df, period_keep = "pre", gender_keep = 1, method_keep = rank_method) %>%
  transmute(firm_id, V_female_pre = V_hat)

rank_female_post <- estimate_rank(df, period_keep = "post", gender_keep = 1, method_keep = rank_method) %>%
  transmute(firm_id, V_female_post = V_hat)

rank_all <- rank_male_pre %>%
  full_join(rank_male_post, by = "firm_id") %>%
  full_join(rank_female_pre, by = "firm_id") %>%
  full_join(rank_female_post, by = "firm_id")

require_rows(rank_all, min_rows = 2, label = "rank_all")

rank_coverage <- data.frame(
  measure = c("male_pre", "male_post", "female_pre", "female_post"),
  recovered_firms = c(
    sum(!is.na(rank_all$V_male_pre)),
    sum(!is.na(rank_all$V_male_post)),
    sum(!is.na(rank_all$V_female_pre)),
    sum(!is.na(rank_all$V_female_post))
  )
)

print(rank_coverage)

rank_all <- rank_all %>%
  mutate(
    delta_male = V_male_post - V_male_pre,
    delta_female = V_female_post - V_female_pre,
    gender_gap_pre = V_female_pre - V_male_pre,
    gender_gap_post = V_female_post - V_male_post,
    did_gender_sorting = gender_gap_post - gender_gap_pre
  )

print(summary(rank_all$did_gender_sorting))
print(summary(lm(did_gender_sorting ~ 1, data = rank_all))) # check if the mean of did_gender_sorting is 0

V_hat_gap_summary <- rank_all %>%
  summarise(
    mean_male_pre = mean(V_male_pre, na.rm = TRUE),
    mean_male_post = mean(V_male_post, na.rm = TRUE),
    mean_female_pre = mean(V_female_pre, na.rm = TRUE),
    mean_female_post = mean(V_female_post, na.rm = TRUE),
    mean_gender_gap_pre = mean(gender_gap_pre, na.rm = TRUE),
    mean_gender_gap_post = mean(gender_gap_post, na.rm = TRUE),
    mean_did_gender_sorting = mean(did_gender_sorting, na.rm = TRUE),
    n_firms = n()
  )

print(V_hat_gap_summary)

#--------------------------
# 5.4 V_true (simulated data) and the the impact of Covid
#--------------------------

V_true_gap_summary <- firm_gap %>%
  group_by(period) %>%
  summarise(
    mean_V_male_true = mean(V_male_true),
    mean_V_female_true = mean(V_female_true),
    mean_V_gap_true = mean(V_gap_true),
    sd_V_gap_true = sd(V_gap_true),
    n_firms = n(),
    .groups = "drop"
  )

V_true_gap_did <- lm(V_gap_true ~ post, data = firm_gap)

print(V_true_gap_summary)
print(summary(V_true_gap_did))

#--------------------------
# 5.5 check the correlation of V_hat between male and female, pre and post sepearately
#--------------------------

V_hat_correlations <- data.frame(
  period = c("pre", "post"),
  corr_male_female = c(
    cor(rank_all$V_male_pre, rank_all$V_female_pre, use = "complete.obs"),
    cor(rank_all$V_male_post, rank_all$V_female_post, use = "complete.obs")
  ),
  n_complete = c(
    sum(complete.cases(rank_all$V_male_pre, rank_all$V_female_pre)),
    sum(complete.cases(rank_all$V_male_post, rank_all$V_female_post))
  )
)

print(V_hat_correlations)

#--------------------------
# 5.6 check the correlation of V_true between male and female, pre and post sepearately
#--------------------------

V_true_correlations <- firm_gap %>%
  group_by(period) %>%
  summarise(
    corr_male_female = cor(V_male_true, V_female_true),
    n_firms = n(),
    .groups = "drop"
  )

print(V_true_correlations)

# =========================
# 6. Visualization (for both true simulated data and estimated data)
# =========================

#--------------------------
# 6.0 data preparation
#--------------------------

# rank_all to long format
rank_all_long <- rank_all %>%
  select(
    firm_id,
    V_male_pre,
    V_male_post,
    V_female_pre,
    V_female_post
  ) %>%
  tidyr::pivot_longer(
    cols = -firm_id,
    names_to = c("gender", "period"),
    names_pattern = "V_(male|female)_(pre|post)",
    values_to = "V_hat"
  )

# df_simulated to long format
df_simulated_male <- df_simulated %>%
  select(
    firm_id, period,
    V_male_true,
    psi_male
  ) %>%
  mutate(gender = "male") %>%
  rename(
    V_true = V_male_true,
    psi = psi_male
  )

df_simulated_female <- df_simulated %>%
  select(
    firm_id, period,
    V_female_true,
    psi_female
  ) %>%
  mutate(gender = "female") %>%
  rename(
    V_true = V_female_true,
    psi = psi_female
  )

df_simulated_long <- bind_rows(df_simulated_male, df_simulated_female)

# merge estimated ranks with true simulated firm values
comparison <- rank_all_long %>%
  left_join(
    df_simulated_long,
    by = c("firm_id", "period", "gender")
  )

require_rows(
  comparison %>% filter(!is.na(V_hat), !is.na(V_true)),
  min_rows = 2,
  label = "comparison data for validity plots"
)

# sanity check
# how many firms in each gender-period? (results: 1 with no mobility)
print(table(comparison$gender, comparison$period))
# missing value only in V_hat
print(colSums(is.na(comparison)))

# firm premia to long format: keep wage premia (psi) separate from values.
firm_premia_long <- df_simulated %>%
  select(
    firm_id,
    period,
    psi_male,
    psi_female
  ) %>%
  pivot_longer(
    cols = c(psi_male, psi_female),
    names_to = "gender",
    names_pattern = "psi_(male|female)",
    values_to = "psi"
  ) %>%
  mutate(
    gender = ifelse(gender == "female", "Female", "Male")
  )

# V_true and V_hat distribution data. These are deliberately in one figure,
# but separated by facet rows so true and recovered values are easy to compare.
value_dist_long <- comparison %>%
  filter(!is.na(V_hat), !is.na(V_true)) %>%
  transmute(
    firm_id,
    gender = ifelse(gender == "female", "Female", "Male"),
    period,
    `Recovered value: V_hat` = V_hat,
    `True value: V_true` = V_true
  ) %>%
  pivot_longer(
    cols = c(`Recovered value: V_hat`, `True value: V_true`),
    names_to = "value_type",
    values_to = "value"
  )
#--------------------------
# 6.1 Check for validity of our measure based on our simulation
#--------------------------

# total sample
validity <- corr <- cor(comparison$V_hat, comparison$V_true, use = "complete.obs") # remove missing values

print(paste0(
  "the correlation between full-model V_hat and V_true for total sample: ",
  validity
))

comparison_plot <- comparison %>%
  filter(!is.na(V_hat), !is.na(V_true)) %>%
  transmute(
    firm_id,
    # Standardizing makes the 45-degree line interpretable in the figure.
    # It does not affect the printed correlation.
    hat = standardize_or_center(V_hat),
    true = standardize_or_center(V_true)
  )

require_rows(comparison_plot, min_rows = 2, label = "total-sample validity plot data")

line_range <- range(c(comparison_plot$hat, comparison_plot$true), finite = TRUE)
line_45 <- data.frame(
  x = line_range[1],
  xend = line_range[2],
  y = line_range[1],
  yend = line_range[2],
  line_type = "45-degree line"
)

p1 <- ggplot(comparison_plot, aes(x = hat, y = true)) +
  geom_segment(
    data = line_45,
    aes(x = x, y = y, xend = xend, yend = yend, linetype = line_type),
    color = "grey40",
    linewidth = 0.8,
    inherit.aes = FALSE,
    show.legend = TRUE
  ) +
  geom_point(color = "grey60", size = 1.1, alpha = 0.55, show.legend = FALSE) +
  geom_smooth(
    aes(linetype = "Linear fit"),
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 1.1,
    show.legend = TRUE
  ) +
  scale_linetype_manual(
    name = "Line",
    breaks = c("45-degree line", "Linear fit"),
    labels = c("45 degree", "Fitted line"),
    values = c(
      "45-degree line" = "dashed",
      "Linear fit" = "solid"
    )
  ) +
  labs(
    x = "Recovered firm value (standardized V_hat)",
    y = "True latent firm value (standardized V_true)",
    title = "Validity check of revealed-preference ranking (total sample)"
  ) +
  coord_equal() +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

validity_check_path <- file.path(output_dir, "validity_check.png")
ggsave(validity_check_path, plot = p1, width = 8, height = 6, dpi = 300)
print(paste0("Saved: ", normalizePath(validity_check_path, mustWork = FALSE)))

# gender_perid subsample
validity_sub <- comparison %>%
  group_by(gender, period) %>%
  summarise(
    corr = cor(V_hat, V_true, use = "complete.obs"),
    n = sum(complete.cases(V_hat, V_true)),
    .groups = "drop"
  )
print("the correlation between V_hat and V_true for gender_period subsamples: ")
print(validity_sub)

comparison_plot_sub <- comparison %>%
  filter(!is.na(V_hat), !is.na(V_true)) %>%
  group_by(gender, period) %>%
  mutate(
    # Standardize within each panel, since the plot is faceted by gender-period.
    hat = standardize_or_center(V_hat),
    true = standardize_or_center(V_true)
  ) %>%
  ungroup() %>%
  select(firm_id, gender, period, hat, true)

require_rows(comparison_plot_sub, min_rows = 2, label = "subsample validity plot data")

line_45_sub <- comparison_plot_sub %>%
  group_by(gender, period) %>%
  summarise(
    x = min(c(hat, true), na.rm = TRUE),
    xend = max(c(hat, true), na.rm = TRUE),
    y = x,
    yend = xend,
    line_type = "45-degree line",
    .groups = "drop"
  )

p1_sub <- ggplot(comparison_plot_sub, aes(x = hat, y = true)) +
  geom_segment(
    data = line_45_sub,
    aes(x = x, y = y, xend = xend, yend = yend, linetype = line_type),
    color = "grey40",
    linewidth = 0.8,
    inherit.aes = FALSE,
    show.legend = TRUE
  ) +
  geom_point(color = "grey60", size = 1.1, alpha = 0.55, show.legend = FALSE) +
  geom_smooth(
    aes(linetype = "Linear fit"),
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 1.1,
    show.legend = TRUE
  ) +
  facet_grid(gender ~ period) +
  scale_linetype_manual(
    name = "Line",
    breaks = c("45-degree line", "Linear fit"),
    labels = c("45 degree", "Fitted line"),
    values = c(
      "45-degree line" = "dashed",
      "Linear fit" = "solid"
    )
  ) +
  labs(
    x = "Recovered firm value (standardized V_hat)",
    y = "True latent firm value (standardized V_true)",
    title = "Validity check of revealed-preference ranking (gender_period subsamples)"
  ) +
  coord_equal() +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

validity_check_sub_path <- file.path(output_dir, "validity_check_sub.png")
ggsave(validity_check_sub_path, plot = p1_sub, width = 8, height = 6, dpi = 300)
print(paste0("Saved: ", normalizePath(validity_check_sub_path, mustWork = FALSE)))

#--------------------------
# 6.2 wage distribution by gender and period
#--------------------------
p_wage_dist <- ggplot(wage_long, aes(x = logy, color = gender, fill = gender)) +
  geom_density(alpha = 0.25, linewidth = 0.8) +
  facet_wrap(~period) +
  labs(
    title = "Distribution of Log Wages by Gender and Period",
    x = "Log wage",
    y = "Density",
    color = "Gender",
    fill = "Gender"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(output_dir, "distribution_log_wage_gender_period.png"),
  plot = p_wage_dist,
  width = 8,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# 6.3. Firm wage-premia distribution by gender and period
# ------------------------------------------------------------

p_premia_dist <- ggplot(firm_premia_long, aes(x = psi, color = gender, fill = gender)) +
  geom_density(alpha = 0.25, linewidth = 0.8) +
  facet_wrap(~period) +
  labs(
    title = "Distribution of Firm Wage Premia by Gender and Period",
    x = "Firm wage premium (psi)",
    y = "Density",
    color = "Gender",
    fill = "Gender"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(output_dir, "distribution_firm_premia_gender_period.png"),
  plot = p_premia_dist,
  width = 8,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 6.4. V_true and V_hat distribution by gender and period
# ------------------------------------------------------------

p_value_dist <- ggplot(value_dist_long, aes(x = value, color = gender, fill = gender)) +
  geom_density(alpha = 0.25, linewidth = 0.8) +
  facet_grid(value_type ~ period, scales = "free") +
  labs(
    title = "Distribution of True and Recovered Firm Values by Period",
    x = "Firm value",
    y = "Density",
    color = "Gender",
    fill = "Gender"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(output_dir, "distribution_values_true_hat_gender_period.png"),
  plot = p_value_dist,
  width = 9,
  height = 6,
  dpi = 300
)

premia_summary_table <- firm_premia_long %>%
  group_by(gender, period) %>%
  summarise(
    n_firms = n(),
    mean = mean(psi, na.rm = TRUE),
    sd = sd(psi, na.rm = TRUE),
    p10 = quantile(psi, 0.10, na.rm = TRUE),
    p50 = quantile(psi, 0.50, na.rm = TRUE),
    p90 = quantile(psi, 0.90, na.rm = TRUE),
    .groups = "drop"
  )

print(premia_summary_table)

if (has_modelsummary) {
  modelsummary::datasummary_df(
    premia_summary_table,
    output = file.path(output_dir, "premia_summary_statistics.tex"),
    title = "Firm Premia Summary Statistics by Gender and Period"
  )
} else {
  write.csv(
    premia_summary_table,
    file.path(output_dir, "premia_summary_statistics.csv"),
    row.names = FALSE
  )
  print("Package 'modelsummary' not installed; saved premia_summary_statistics.csv instead.")
}

# Check exported files
list.files(output_dir)
#--------------------------
# 6.5 Quick check for compensating differentials
#--------------------------
comparison_cd <- comparison %>%
  transmute(
    firm_id,
    gender,
    period,
    hat = V_hat,
    psi = psi
  ) %>%
  filter(!is.na(hat), !is.na(psi))

require_rows(comparison_cd, min_rows = 2, label = "compensating-differentials plot data")

print(summary(lm(psi ~ hat, data = comparison_cd)))

p2 <- ggplot(comparison_cd, aes(x = hat, y = psi)) +
  geom_point(color = "grey60", size = 1.1, alpha = 0.55) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1.1) +
  facet_grid(gender ~ period) +
  labs(
    x = "Recovered firm value (V_hat)",
    y = "Firm wage premium (psi)",
    title = "Compensating differentials check"
  ) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

compensating_differentials_path <- file.path(output_dir, "compensating_differentials.png")
ggsave(compensating_differentials_path, plot = p2, width = 8, height = 6, dpi = 300)
print(paste0("Saved: ", normalizePath(compensating_differentials_path, mustWork = FALSE)))
