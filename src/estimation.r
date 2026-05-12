# =========================
# Estimation on real-format data
# =========================
# This script replaces the simulation exercise. It uses:
#   1. yearly worker-firm data for AKM firm premia and wage-gap regressions;
#   2. monthly worker-pair transitions for revealed-preference firm values,
#      lambda recovery, opportunities, and preference/opportunity decompositions.

library(dplyr)
library(tidyr)
library(ggplot2)
library(Matrix)
library(igraph)
library(arrow)
has_modelsummary <- requireNamespace("modelsummary", quietly = TRUE)

set.seed(123)

# =========================
# 0. Paths and helpers
# =========================

script_args <- commandArgs(trailingOnly = FALSE)
script_file_arg <- "--file="
script_path <- sub(script_file_arg, "", script_args[startsWith(script_args, script_file_arg)][1])
project_dir <- if (!is.na(script_path) && nzchar(script_path)) {
    normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
} else {
    normalizePath(getwd(), mustWork = TRUE)
}

data_dir <- file.path(project_dir, "simulated_data")
output_dir <- file.path(project_dir, "results/estimation")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

period_levels <- c("pre", "post")
pre_years <- c(2018, 2019)
post_years <- c(2021, 2022)

read_parquet_data <- function(name) {
    path <- file.path(data_dir, paste0(name, ".parquet"))
    if (!file.exists(path)) {
        stop("Missing data file: ", path)
    }
    as.data.frame(arrow::read_parquet(path))
}

first_existing <- function(data, candidates, required = TRUE, label = "column") {
    hit <- candidates[candidates %in% names(data)]
    if (length(hit) > 0) {
        return(hit[1])
    }
    if (required) {
        stop(
            "Could not find ", label, ". Tried: ",
            paste(candidates, collapse = ", "),
            ". Available columns are: ",
            paste(names(data), collapse = ", ")
        )
    }
    NA_character_
}

standardize_or_center <- function(x) {
    sx <- sd(x, na.rm = TRUE)
    if (is.na(sx) || sx == 0) {
        return(as.numeric(x - mean(x, na.rm = TRUE)))
    }
    as.numeric((x - mean(x, na.rm = TRUE)) / sx)
}

rank_to_normal_score <- function(x) {
    ok <- is.finite(x) & !is.na(x)
    out <- rep(NA_real_, length(x))
    n_ok <- sum(ok)
    if (n_ok < 2) {
        out[ok] <- 0
        return(out)
    }
    r <- rank(x[ok], ties.method = "average")
    p <- (r - 0.5) / n_ok
    out[ok] <- qnorm(p)
    standardize_or_center(out)
}

period_from_year <- function(year) {
    out <- ifelse(year %in% pre_years, "pre", ifelse(year %in% post_years, "post", NA_character_))
    factor(out, levels = period_levels)
}

normalize_female <- function(x) {
    x_chr <- tolower(trimws(as.character(x)))
    ifelse(
        x_chr %in% c("1", "female", "f", "woman", "women"),
        1L,
        ifelse(x_chr %in% c("0", "male", "m", "man", "men"), 0L, as.integer(as.numeric(x_chr) == 1))
    )
}

normalize_sx <- function(x) {
    x_chr <- tolower(trimws(as.character(x)))
    ifelse(
        x_chr %in% c("2", "female", "f", "woman", "women"),
        1L,
        ifelse(x_chr %in% c("1", "male", "m", "man", "men"), 0L, NA_integer_)
    )
}

month_to_year <- function(x, origin_year = 2018L) {
    if (inherits(x, "Date") || inherits(x, "POSIXt")) {
        return(as.integer(format(x, "%Y")))
    }

    x_chr <- trimws(as.character(x))
    parsed_year <- suppressWarnings(as.integer(substr(x_chr, 1, 4)))
    looks_like_ym <- grepl("^\\d{6}$", x_chr)
    out <- ifelse(looks_like_ym, parsed_year, NA_integer_)

    x_num <- suppressWarnings(as.integer(as.numeric(x_chr)))
    sequential <- is.na(out) & !is.na(x_num)
    out[sequential] <- origin_year + floor((x_num[sequential] - 1L) / 12L)
    out
}

month_to_month <- function(x) {
    if (inherits(x, "Date") || inherits(x, "POSIXt")) {
        return(as.integer(format(x, "%m")))
    }

    x_chr <- trimws(as.character(x))
    looks_like_ym <- grepl("^\\d{6}$", x_chr)
    parsed_month <- suppressWarnings(as.integer(substr(x_chr, 5, 6)))
    out <- ifelse(looks_like_ym, parsed_month, NA_integer_)

    x_num <- suppressWarnings(as.integer(as.numeric(x_chr)))
    sequential <- is.na(out) & !is.na(x_num)
    out[sequential] <- ((x_num[sequential] - 1L) %% 12L) + 1L
    out
}

require_rows <- function(data, min_rows, label) {
    if (nrow(data) < min_rows) {
        stop(label, " has only ", nrow(data), " usable rows.")
    }
    invisible(data)
}

write_simple_latex_table <- function(tab, path, caption = NULL, digits = 3) {
    escape_latex <- function(x) gsub("_", "\\\\_", as.character(x), fixed = TRUE)
    tab_out <- tab
    for (nm in names(tab_out)) {
        if (is.numeric(tab_out[[nm]])) {
            tab_out[[nm]] <- ifelse(is.na(tab_out[[nm]]), "", formatC(tab_out[[nm]], digits = digits, format = "f"))
        } else {
            tab_out[[nm]] <- escape_latex(tab_out[[nm]])
        }
    }
    lines <- c(
        "\\begin{table}[!htbp]\\centering",
        if (!is.null(caption)) paste0("\\caption{", escape_latex(caption), "}") else NULL,
        paste0("\\begin{tabular}{", paste(rep("l", ncol(tab_out)), collapse = ""), "}"),
        "\\hline",
        paste(escape_latex(names(tab_out)), collapse = " & "),
        "\\\\ \\hline",
        paste0(apply(tab_out, 1, paste, collapse = " & "), " \\\\"),
        "\\hline",
        "\\end{tabular}",
        "\\end{table}"
    )
    writeLines(lines, path)
}

# =========================
# 1. Load and standardize data
# =========================

spell_year_raw <- read_parquet_data("spell_year")
worker_year_raw <- read_parquet_data("worker_year")
spell_month_raw <- read_parquet_data("spell_month")
worker_month_raw <- read_parquet_data("worker_month")
workers_pairs_raw <- read_parquet_data("workers_pairs")

standardize_worker_year <- function(data) {
    worker <- first_existing(data, c("worker_id", "id_worker", "wid", "person_id", "nninouv", "i"), label = "worker id")
    firm <- first_existing(data, c("firm_id", "id_firm", "fid", "siret", "sir", "j"), label = "firm id")
    year <- first_existing(data, c("year", "annee"), label = "year")
    female <- first_existing(data, c("female", "woman", "sex_female", "gender_female", "sx"), label = "female indicator")
    age <- first_existing(data, c("age", "age_years"), required = FALSE)
    wage <- first_existing(data, c("log_hourly_wage", "log_wage", "logy", "ln_wage", "hourly_wage", "wage", "earnings", "netnet", "s_brut"), label = "wage")
    female_value <- if (female == "sx") normalize_sx(data[[female]]) else normalize_female(data[[female]])
    age_value <- if (!is.na(age)) as.numeric(data[[age]]) else rep(NA_real_, nrow(data))

    out <- data %>%
        transmute(
            worker_id = .data[[worker]],
            firm_id = .data[[firm]],
            year = as.integer(.data[[year]]),
            female = female_value,
            age = age_value,
            logy = as.numeric(.data[[wage]])
        )

    if (!grepl("^log|^ln", wage)) {
        out <- out %>% mutate(logy = log(pmax(logy, 1e-8)))
    }

    out %>%
        filter(year %in% c(pre_years, post_years), !is.na(worker_id), !is.na(firm_id), !is.na(logy)) %>%
        mutate(
            period = period_from_year(year),
            post = as.integer(period == "post"),
            gender = ifelse(female == 1, "Female", "Male")
        )
}

standardize_worker_pairs <- function(data, worker_gender_lookup = NULL) {
    worker <- first_existing(data, c("worker_id", "id_worker", "wid", "person_id", "nninouv", "i"), label = "worker id")
    female <- first_existing(data, c("female", "woman", "sex_female", "gender_female", "sx"), required = FALSE)
    year1 <- first_existing(data, c("year1", "year_origin", "year_t", "year", "annee1"), required = FALSE)
    month1 <- first_existing(data, c("month1", "month_origin", "month_t", "month", "mois1"), label = "origin month")
    year2 <- first_existing(data, c("year2", "year_dest", "year_t1", "next_year", "annee2"), required = FALSE)
    month2 <- first_existing(data, c("month2", "month_dest", "month_t1", "next_month", "mois2"), required = FALSE)
    firm1 <- first_existing(data, c("firm1", "firm_id1", "firm_origin", "firm_t", "firm_id_t", "index1", "firm_id", "sir1", "sir_t", "sir_origin", "sir"), required = FALSE)
    firm2 <- first_existing(data, c("firm2", "firm_id2", "firm_dest", "firm_t1", "firm_id_t1", "next_firm_id", "index2", "sir2", "sir_t1", "sir_dest", "next_sir"), required = FALSE)
    emp1 <- first_existing(data, c("emp1", "employed1", "employed_t", "emp_t", "is_employed1"), required = FALSE)
    emp2 <- first_existing(data, c("emp2", "employed2", "employed_t1", "emp_t1", "is_employed2"), required = FALSE)
    ee_col <- first_existing(data, c("ee", "EE", "is_ee"), required = FALSE)
    en_col <- first_existing(data, c("en", "EN", "emp_to_nonemp"), required = FALSE)
    ne_col <- first_existing(data, c("ne", "NE", "nonemp_to_emp"), required = FALSE)
    ene_col <- first_existing(data, c("ene", "ENE", "is_ene"), required = FALSE)
    female_value <- if (!is.na(female)) {
        if (female == "sx") normalize_sx(data[[female]]) else normalize_female(data[[female]])
    } else {
        rep(NA_integer_, nrow(data))
    }
    month1_source <- data[[month1]]
    month2_source <- if (!is.na(month2)) data[[month2]] else month1_source
    year1_value <- if (!is.na(year1)) as.integer(data[[year1]]) else month_to_year(month1_source)
    month1_value <- month_to_month(month1_source)
    year2_value <- if (!is.na(year2)) as.integer(data[[year2]]) else month_to_year(month2_source)
    month2_value <- month_to_month(month2_source)
    index1_value <- if (!is.na(firm1)) data[[firm1]] else rep(NA, nrow(data))
    index2_value <- if (!is.na(firm2)) data[[firm2]] else rep(NA, nrow(data))
    employed1_value <- if (!is.na(emp1)) as.integer(data[[emp1]]) else as.integer(!is.na(index1_value))
    employed2_value <- if (!is.na(emp2)) as.integer(data[[emp2]]) else as.integer(!is.na(index2_value))
    ee_value <- if (!is.na(ee_col)) as.integer(data[[ee_col]]) else rep(NA_integer_, nrow(data))
    en_value <- if (!is.na(en_col)) as.integer(data[[en_col]]) else rep(0L, nrow(data))
    ne_value <- if (!is.na(ne_col)) as.integer(data[[ne_col]]) else rep(0L, nrow(data))
    ene_value <- if (!is.na(ene_col)) as.integer(data[[ene_col]]) else as.integer(en_value == 1L | ne_value == 1L)

    out <- data %>%
        transmute(
            worker_id = .data[[worker]],
            female = female_value,
            year1 = year1_value,
            month1 = month1_value,
            year2 = year2_value,
            month2 = month2_value,
            index1 = index1_value,
            index2 = index2_value,
            employed1 = employed1_value,
            employed2 = employed2_value,
            ee = ee_value,
            en = en_value,
            ne = ne_value,
            ene = ene_value
        ) %>%
        mutate(
            ee = ifelse(is.na(ee), as.integer(employed1 == 1 & employed2 == 1 & !is.na(index1) & !is.na(index2) & index1 != index2), ee),
            ene = ifelse(is.na(ene), 0L, ene),
            period = period_from_year(year1),
            post = as.integer(period == "post"),
            gender = ifelse(female == 1, "Female", "Male")
        ) %>%
        filter(!is.na(period))

    if (all(is.na(out$female))) {
        if (is.null(worker_gender_lookup)) {
            stop("workers_pairs has no gender column, and no worker_gender_lookup was supplied.")
        }
        out <- out %>%
            select(-female, -gender) %>%
            left_join(worker_gender_lookup, by = "worker_id") %>%
            mutate(gender = ifelse(female == 1, "Female", "Male"))
    }

    out
}

wage_long <- standardize_worker_year(worker_year_raw)
worker_gender_lookup <- wage_long %>%
    distinct(worker_id, female)
monthly_pairs <- standardize_worker_pairs(workers_pairs_raw, worker_gender_lookup)

firm_levels <- sort(unique(c(wage_long$firm_id, monthly_pairs$index1, monthly_pairs$index2)))
firm_levels <- firm_levels[!is.na(firm_levels)]
firm_map <- data.frame(firm_id = firm_levels, firm_node = seq_along(firm_levels))
n_firms <- nrow(firm_map)

wage_long <- wage_long %>% left_join(firm_map, by = "firm_id")
monthly_pairs <- monthly_pairs %>%
    left_join(firm_map, by = c("index1" = "firm_id")) %>%
    rename(origin_node = firm_node) %>%
    left_join(firm_map, by = c("index2" = "firm_id")) %>%
    rename(dest_node = firm_node)

# =========================
# 2. AKM firm premia on yearly data
# =========================

estimate_akm_firm_premia <- function(data_subset, max_iter = 200, tol = 1e-7) {
    require_rows(data_subset, min_rows = 10, label = "AKM subset")
    if (all(is.na(data_subset$age))) {
        base_resid <- resid(lm(logy ~ factor(year), data = data_subset))
    } else {
        base_resid <- resid(lm(logy ~ age + I(age^2) + I(age^3) + factor(year), data = data_subset))
    }

    worker_levels <- sort(unique(data_subset$worker_id))
    firm_levels_local <- sort(unique(data_subset$firm_node))
    worker_index <- match(data_subset$worker_id, worker_levels)
    firm_index <- match(data_subset$firm_node, firm_levels_local)

    alpha <- numeric(length(worker_levels))
    psi <- numeric(length(firm_levels_local))

    for (iter in seq_len(max_iter)) {
        old_psi <- psi
        alpha <- rowsum(base_resid - psi[firm_index], worker_index, reorder = FALSE)[, 1] /
            as.vector(tabulate(worker_index, nbins = length(worker_levels)))
        psi <- rowsum(base_resid - alpha[worker_index], firm_index, reorder = FALSE)[, 1] /
            as.vector(tabulate(firm_index, nbins = length(firm_levels_local)))
        firm_weights <- as.vector(tabulate(firm_index, nbins = length(firm_levels_local)))
        psi <- psi - weighted.mean(psi, firm_weights)
        if (max(abs(psi - old_psi), na.rm = TRUE) < tol) break
    }

    data.frame(
        firm_node = firm_levels_local,
        psi_hat = as.numeric(psi),
        n_obs = as.vector(tabulate(firm_index, nbins = length(firm_levels_local)))
    )
}

akm_firm_premia_long <- wage_long %>%
    group_by(period, female) %>%
    group_modify(~ estimate_akm_firm_premia(.x)) %>%
    ungroup() %>%
    left_join(firm_map, by = "firm_node") %>%
    mutate(gender = ifelse(female == 1, "Female", "Male"))

write.csv(akm_firm_premia_long, file.path(output_dir, "akm_firm_premia_gender_period.csv"), row.names = FALSE)

# =========================
# 3. Revealed-preference recovery on monthly data
# =========================

build_count_matrix <- function(origin, dest, n_nodes) {
    if (length(origin) == 0) return(matrix(0, nrow = n_nodes, ncol = n_nodes))
    as.matrix(sparseMatrix(i = dest, j = origin, x = 1, dims = c(n_nodes, n_nodes)))
}

recover_exptv_damped <- function(M, damping = 0.85) {
    diag(M) <- 0
    n <- nrow(M)
    outflow <- colSums(M)
    A <- matrix(1 / n, nrow = n, ncol = n)
    positive_outflow <- which(outflow > 0)
    A[, positive_outflow] <- sweep(M[, positive_outflow, drop = FALSE], 2, outflow[positive_outflow], "/")
    A <- damping * A + (1 - damping) * matrix(1 / n, nrow = n, ncol = n)
    eig <- eigen(A)
    q <- abs(Re(eig$vectors[, which.max(Re(eig$values))]))
    pmax(q / sum(q), 1e-10)
}

build_sorkin_inputs <- function(data, period_keep, gender_keep) {
    d <- data %>%
        filter(period == period_keep, female == gender_keep)

    ee_rows <- d %>% filter(ee == 1, !is.na(origin_node), !is.na(dest_node), origin_node != dest_node)
    ene_rows <- d %>% filter(ene == 1, !is.na(origin_node), !is.na(dest_node), origin_node != dest_node)

    active_firms <- sort(unique(c(
        ee_rows$origin_node,
        ee_rows$dest_node,
        ene_rows$origin_node,
        ene_rows$dest_node
    )))
    active_firms <- active_firms[!is.na(active_firms)]

    if (length(active_firms) == 0) {
        return(list(
            data = d,
            ee_rows = ee_rows,
            ene_rows = ene_rows,
            M = NULL,
            g_level = NULL,
            fo_level = NULL,
            firm_nodes = integer(),
            ee_count = 0L,
            ene_count = 0L
        ))
    }

    local_map <- data.frame(
        firm_node = active_firms,
        local_node = seq_along(active_firms)
    )

    ee_rows <- ee_rows %>%
        left_join(local_map, by = c("origin_node" = "firm_node")) %>%
        rename(origin_local = local_node) %>%
        left_join(local_map, by = c("dest_node" = "firm_node")) %>%
        rename(dest_local = local_node)

    ene_rows <- ene_rows %>%
        left_join(local_map, by = c("origin_node" = "firm_node")) %>%
        rename(origin_local = local_node) %>%
        left_join(local_map, by = c("dest_node" = "firm_node")) %>%
        rename(dest_local = local_node)

    d_local <- d %>%
        left_join(local_map, by = c("origin_node" = "firm_node")) %>%
        rename(origin_local = local_node)

    nonemployment_node <- 1L
    n_firms_local <- length(active_firms)
    n_nodes <- n_firms_local + 1L
    origin <- c(ee_rows$origin_local + 1L, ene_rows$origin_local + 1L, rep(nonemployment_node, nrow(ene_rows)))
    dest <- c(ee_rows$dest_local + 1L, rep(nonemployment_node, nrow(ene_rows)), ene_rows$dest_local + 1L)
    M <- build_count_matrix(origin, dest, n_nodes)

    g_level <- numeric(n_nodes)
    g_level <- g_level + tabulate(d_local$origin_local[!is.na(d_local$origin_local)] + 1L, nbins = n_nodes)

    fo_level <- numeric(n_nodes)
    fo_level <- fo_level + tabulate(ene_rows$dest_local + 1L, nbins = n_nodes)
    fo_level[nonemployment_node] <- max(1, sum(fo_level))

    list(
        data = d_local,
        ee_rows = ee_rows,
        ene_rows = ene_rows,
        M = M,
        g_level = g_level,
        fo_level = fo_level,
        firm_nodes = active_firms,
        ee_count = nrow(ee_rows),
        ene_count = nrow(ene_rows)
    )
}

sorkin_given_lambda <- function(M, exptV, g_level, fo_level, lambda1, n_groups = 200) {
    eps <- 1e-8
    fo <- (pmax(fo_level, 0) + eps) / sum(pmax(fo_level, 0) + eps)
    g <- (pmax(g_level, 0) + eps) / sum(pmax(g_level, 0) + eps)
    hires_ne <- sum(fo_level)
    W <- sum(g_level)
    if (hires_ne <= 0 || W <= 0) return(NULL)

    b <- (exptV * g) / fo
    a <- (exptV[1] * hires_ne) / W
    CexpV <- b - a / (1 - lambda1)
    positive <- CexpV > 0 & is.finite(CexpV)
    if (!any(positive)) return(NULL)
    CexpV[!positive] <- min(CexpV[positive])

    fC <- fo * (b / CexpV)
    C1 <- 1 / sum(fC[-1])
    f <- fC[-1] * C1
    expVe <- CexpV[-1] / C1
    expVn_init <- (a / (1 - lambda1)) / C1
    Ve_raw <- log(pmax(expVe / expVn_init, eps))

    firm_g <- g[-1] / sum(g[-1])
    f <- pmax(f, eps)
    f <- f / sum(f)

    ord <- order(Ve_raw)
    grouped <- data.frame(
        group = pmin(n_groups, floor(cumsum(firm_g[ord]) * n_groups) + 1L),
        Ve = Ve_raw[ord],
        g = firm_g[ord],
        f = f[ord]
    ) %>%
        group_by(group) %>%
        summarise(Ve = weighted.mean(Ve, g), g = sum(g), f = sum(f), .groups = "drop")

    P <- outer(exp(grouped$Ve), exp(grouped$Ve), FUN = function(i, j) i / (i + j))
    diag(P) <- 0
    prob_accept_model <- mean(as.numeric(t(grouped$f) %*% P))
    ee_prob_model <- lambda1 * prob_accept_model
    ee_prob_data <- sum(M[-1, -1]) / sum(g_level[-1])
    gap <- abs(ee_prob_model - ee_prob_data) / max(ee_prob_data, eps)

    list(
        gap = gap,
        Ve = rank_to_normal_score(Ve_raw),
        f = f,
        ee_prob_model = ee_prob_model,
        ee_prob_data = ee_prob_data
    )
}

estimate_lambda_and_ve <- function(M, g_level, fo_level, grid = seq(0.02, 0.50, by = 0.01)) {
    exptV <- recover_exptv_damped(M)
    fits <- lapply(grid, function(lambda1) {
        fit <- sorkin_given_lambda(M, exptV, g_level, fo_level, lambda1)
        if (is.null(fit)) return(data.frame(lambda = lambda1, gap = Inf))
        data.frame(lambda = lambda1, gap = fit$gap, ee_prob_model = fit$ee_prob_model, ee_prob_data = fit$ee_prob_data)
    }) %>% bind_rows()

    best_lambda <- fits$lambda[which.min(fits$gap)]
    if (!is.finite(min(fits$gap, na.rm = TRUE))) {
        return(NULL)
    }
    best_fit <- sorkin_given_lambda(M, exptV, g_level, fo_level, best_lambda)
    if (is.null(best_fit)) {
        return(NULL)
    }
    list(lambda = best_lambda, grid = fits, Ve = best_fit$Ve, f = best_fit$f,
         ee_prob_data = best_fit$ee_prob_data, ee_prob_model = best_fit$ee_prob_model)
}

recover_stable_monthly_rank <- function(inputs) {
    n_firms_local <- length(inputs$firm_nodes)
    transition_rows <- bind_rows(
        inputs$ee_rows %>% transmute(origin_node = origin_local, dest_node = dest_local),
        inputs$ene_rows %>% transmute(origin_node = origin_local, dest_node = dest_local)
    )
    if (nrow(transition_rows) == 0) {
        return(list(V_hat = rep(NA_real_, n_firms_local), offer_hat = rep(1 / n_firms_local, n_firms_local)))
    }
    inflow <- tabulate(transition_rows$dest_node, nbins = n_firms_local)
    outflow <- tabulate(transition_rows$origin_node, nbins = n_firms_local)
    offer_hat <- pmax(inflow, 1)
    offer_hat <- offer_hat / sum(offer_hat)
    list(
        V_hat = rank_to_normal_score(log((inflow + 1) / (outflow + 1))),
        offer_hat = offer_hat
    )
}

recover_sorkin_full_model <- function(data, period_keep, gender_keep) {
    inputs <- build_sorkin_inputs(data, period_keep, gender_keep)
    if ((inputs$ee_count + inputs$ene_count) == 0 || length(inputs$firm_nodes) == 0) {
        return(data.frame(firm_node = integer(), period = character(), female = integer(),
                          lambda_hat = numeric(), offer_hat = numeric(), V_hat = numeric()))
    }
    fit <- estimate_lambda_and_ve(inputs$M, inputs$g_level, inputs$fo_level)
    if (is.null(fit)) {
        fallback <- recover_stable_monthly_rank(inputs)
        return(data.frame(
            firm_node = inputs$firm_nodes,
            period = factor(period_keep, levels = period_levels),
            female = gender_keep,
            lambda_hat = NA_real_,
            offer_hat = fallback$offer_hat,
            V_hat = fallback$V_hat
        ))
    }
    data.frame(
        firm_node = inputs$firm_nodes,
        period = factor(period_keep, levels = period_levels),
        female = gender_keep,
        lambda_hat = fit$lambda,
        offer_hat = fit$f,
        V_hat = fit$Ve
    )
}

rank_all_long <- bind_rows(
    recover_sorkin_full_model(monthly_pairs, "pre", 0),
    recover_sorkin_full_model(monthly_pairs, "post", 0),
    recover_sorkin_full_model(monthly_pairs, "pre", 1),
    recover_sorkin_full_model(monthly_pairs, "post", 1)
) %>%
    left_join(firm_map, by = "firm_node") %>%
    mutate(gender = ifelse(female == 1, "Female", "Male"))

lambda_summary <- rank_all_long %>%
    distinct(period, female, gender, lambda_hat)
write.csv(lambda_summary, file.path(output_dir, "sorkin_lambda_estimates.csv"), row.names = FALSE)

# =========================
# 4. Summary tables and regressions
# =========================

mobility_by_year_gender <- monthly_pairs %>%
    group_by(year1, gender) %>%
    summarise(num_ee = sum(ee, na.rm = TRUE), num_ene = sum(ene, na.rm = TRUE),
              ee_rate = mean(ee, na.rm = TRUE), ene_rate = mean(ene, na.rm = TRUE), .groups = "drop") %>%
    rename(year = year1)

data_summary_by_year_gender <- wage_long %>%
    left_join(akm_firm_premia_long %>% select(period, female, firm_node, psi_hat),
              by = c("period", "female", "firm_node")) %>%
    group_by(year, gender) %>%
    summarise(workers = n_distinct(worker_id), firms = n_distinct(firm_id),
              `Mean log wage` = mean(logy, na.rm = TRUE),
              `Mean AKM psi` = mean(psi_hat, na.rm = TRUE), .groups = "drop") %>%
    left_join(mobility_by_year_gender, by = c("year", "gender")) %>%
    mutate(across(c(num_ee, num_ene, ee_rate, ene_rate), ~replace_na(.x, 0))) %>%
    arrange(year, gender)

write.csv(data_summary_by_year_gender, file.path(output_dir, "data_summary_by_year_gender.csv"), row.names = FALSE)
write_simple_latex_table(data_summary_by_year_gender, file.path(output_dir, "data_summary_by_year_gender.tex"),
                         caption = "Summary Statistics by Year and Gender")

pay_gap_raw_did <- lm(logy ~ female * post, data = wage_long)
pay_gap_did <- if (all(is.na(wage_long$age))) {
    lm(logy ~ female * post + factor(year), data = wage_long)
} else {
    lm(logy ~ female * post + age + I(age^2) + I(age^3) + factor(year), data = wage_long)
}

did_worker_regression_data <- wage_long %>%
    left_join(akm_firm_premia_long %>% select(period, female, firm_node, psi_hat),
              by = c("period", "female", "firm_node"))

firm_premium_worker_did <- lm(psi_hat ~ female * post, data = did_worker_regression_data)
firm_premium_worker_year_did <- lm(psi_hat ~ female * post + factor(year), data = did_worker_regression_data)

did_regression_models <- list(
    "log(hourly wage)" = pay_gap_raw_did,
    "log(hourly wage)" = pay_gap_did,
    "firm premia" = firm_premium_worker_did,
    "firm premia" = firm_premium_worker_year_did
)

did_regression_extra_rows <- data.frame(
    term = c("Std.Errors", "FE: year"),
    `log(hourly wage)` = c("by: worker_id", ""),
    `log(hourly wage)` = c("by: worker_id", "x"),
    `firm premia` = c("by: worker_id", ""),
    `firm premia` = c("by: worker_id", "x"),
    check.names = FALSE
)

if (has_modelsummary) {
    modelsummary::modelsummary(
        did_regression_models,
        output = file.path(output_dir, "did_results_table.tex"),
        title = "Regression Results",
        coef_map = c("female" = "female", "post" = "post", "female:post" = "female-post",
                     "age" = "age", "I(age^2)" = "age2", "I(age^3)" = "age3"),
        vcov = list(~worker_id, ~worker_id, ~worker_id, ~worker_id),
        stars = c("+" = 0.1, "*" = 0.05, "**" = 0.01, "***" = 0.001),
        gof_map = c("nobs", "r.squared", "adj.r.squared", "aic", "bic", "rmse"),
        add_rows = did_regression_extra_rows,
        notes = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
    )
} else {
    did_regression_summary <- bind_rows(lapply(names(did_regression_models), function(model_name) {
        coefs <- summary(did_regression_models[[model_name]])$coefficients
        data.frame(
            model = model_name,
            term = rownames(coefs),
            estimate = coefs[, "Estimate"],
            std_error = coefs[, "Std. Error"],
            statistic = coefs[, "t value"],
            p_value = coefs[, "Pr(>|t|)"],
            row.names = NULL
        )
    }))
    write.csv(did_regression_summary, file.path(output_dir, "did_results_table.csv"), row.names = FALSE)
    message("Package 'modelsummary' not installed; saved did_results_table.csv instead.")
}

# =========================
# 5. Sorkin sorting, opportunities, and preferences
# =========================

employment_shares <- wage_long %>%
    count(period, female, firm_node, name = "n") %>%
    group_by(period, female) %>%
    mutate(share = n / sum(n)) %>%
    ungroup() %>%
    select(period, female, firm_node, share) %>%
    pivot_wider(names_from = female, values_from = share, names_prefix = "female_", values_fill = 0) %>%
    rename(share_male = female_0, share_female = female_1)

akm_premia_wide <- akm_firm_premia_long %>%
    select(firm_node, period, female, psi_hat) %>%
    pivot_wider(names_from = female, values_from = psi_hat, names_prefix = "female_") %>%
    rename(psi_hat_male = female_0, psi_hat_female = female_1)

rank_offer_wide <- rank_all_long %>%
    select(firm_node, period, female, V_hat, offer_hat) %>%
    mutate(gender_key = ifelse(female == 1, "female", "male")) %>%
    select(-female) %>%
    pivot_wider(names_from = gender_key, values_from = c(V_hat, offer_hat), names_sep = "_")

make_model_share <- function(offer, value) {
    eps <- 1e-8
    w <- pmax(ifelse(is.finite(offer), offer, eps), eps) * exp(standardize_or_center(ifelse(is.finite(value), value, 0)))
    w / sum(w, na.rm = TRUE)
}

sorkin_decomp_data <- rank_offer_wide %>%
    left_join(employment_shares, by = c("period", "firm_node")) %>%
    left_join(akm_premia_wide, by = c("period", "firm_node")) %>%
    group_by(period) %>%
    group_modify(~{
        d <- .x
        d$share_model_mm <- make_model_share(d$offer_hat_male, d$V_hat_male)
        d$share_model_mf <- make_model_share(d$offer_hat_male, d$V_hat_female)
        d$share_model_fm <- make_model_share(d$offer_hat_female, d$V_hat_male)
        d$share_model_ff <- make_model_share(d$offer_hat_female, d$V_hat_female)
        d
    }) %>%
    ungroup()

compute_decomp <- function(data, price_var, price_label) {
    data %>%
        group_by(period) %>%
        summarise(
            price_system = price_label,
            observed_sorting_gap = sum((share_female - share_male) * .data[[price_var]], na.rm = TRUE),
            model_sorting_gap = sum((share_model_ff - share_model_mm) * .data[[price_var]], na.rm = TRUE),
            preference_component = sum((share_model_mf - share_model_mm) * .data[[price_var]], na.rm = TRUE),
            opportunity_component = sum((share_model_fm - share_model_mm) * .data[[price_var]], na.rm = TRUE),
            interaction_component = model_sorting_gap - preference_component - opportunity_component,
            .groups = "drop"
        )
}

sorkin_preference_opportunity <- bind_rows(
    compute_decomp(sorkin_decomp_data, "psi_hat_male", "Male price"),
    compute_decomp(sorkin_decomp_data, "psi_hat_female", "Female price")
)

sorkin_preference_opportunity_change <- sorkin_preference_opportunity %>%
    pivot_longer(cols = c(observed_sorting_gap, model_sorting_gap, preference_component,
                          opportunity_component, interaction_component),
                 names_to = "component", values_to = "value") %>%
    pivot_wider(names_from = period, values_from = value) %>%
    mutate(change_post_minus_pre = post - pre)

write.csv(sorkin_preference_opportunity, file.path(output_dir, "sorkin_preference_opportunity_by_period.csv"), row.names = FALSE)
write.csv(sorkin_preference_opportunity_change, file.path(output_dir, "sorkin_preference_opportunity_change.csv"), row.names = FALSE)

write_sorkin_decomposition_latex_table <- function(decomp_change, path,
                                                   caption = "Preference and Opportunity Decomposition",
                                                   digits = 3) {
    component_labels <- c(
        "observed_sorting_gap" = "Observed sorting gap",
        "model_sorting_gap" = "Model sorting gap",
        "preference_component" = "Preferences",
        "opportunity_component" = "Opportunities",
        "interaction_component" = "Interaction"
    )

    fmt <- function(x) ifelse(is.na(x), "", formatC(x, digits = digits, format = "f"))

    table_wide <- decomp_change %>%
        mutate(component = component_labels[component]) %>%
        pivot_wider(
            id_cols = component,
            names_from = price_system,
            values_from = c(pre, post, change_post_minus_pre),
            names_glue = "{price_system}: {.value}"
        )

    desired_cols <- c(
        "component",
        "Male price: pre",
        "Male price: post",
        "Male price: change_post_minus_pre",
        "Female price: pre",
        "Female price: post",
        "Female price: change_post_minus_pre"
    )
    table_wide <- table_wide[, desired_cols]
    names(table_wide) <- c("", "Pre", "Post", "Post-Pre", "Pre", "Post", "Post-Pre")

    for (j in seq_along(table_wide)) {
        if (is.numeric(table_wide[[j]])) {
            table_wide[[j]] <- fmt(table_wide[[j]])
        }
    }

    escape_latex <- function(x) gsub("_", "\\\\_", as.character(x), fixed = TRUE)
    body <- apply(table_wide, 1, function(row) paste(escape_latex(row), collapse = " & "))
    lines <- c(
        "\\begin{table}[!htbp]\\centering",
        paste0("\\caption{", escape_latex(caption), "}"),
        "\\begin{tabular}{lcccccc}",
        "\\hline",
        " & \\multicolumn{3}{c}{Male price} & \\multicolumn{3}{c}{Female price} \\\\",
        "\\cline{2-4} \\cline{5-7}",
        paste(escape_latex(names(table_wide)), collapse = " & "),
        "\\\\ \\hline",
        paste0(body, " \\\\"),
        "\\hline",
        "\\multicolumn{7}{l}{\\footnotesize{Preferences use recovered firm values; opportunities use recovered offer probabilities.}} \\\\",
        "\\multicolumn{7}{l}{\\footnotesize{Post-Pre reports the change after Covid relative to before Covid.}} \\\\",
        "\\end{tabular}",
        "\\end{table}"
    )
    writeLines(lines, path)
}

write_sorkin_decomposition_latex_table(
    sorkin_preference_opportunity_change,
    file.path(output_dir, "sorkin_preference_opportunity_table.tex"),
    caption = "Sorkin-Style Preference and Opportunity Decomposition"
)

# =========================
# 6. Figures
# =========================

p_wage_dist <- ggplot(wage_long, aes(x = logy, color = gender, fill = gender)) +
    geom_density(alpha = 0.25, linewidth = 0.8) +
    facet_wrap(~period) +
    labs(title = "Distribution of Log Wages by Gender and Period",
         x = "Log hourly wage", y = "Density", color = "Gender", fill = "Gender") +
    theme_minimal()
ggsave(file.path(output_dir, "distribution_log_wage_gender_period.png"), p_wage_dist, width = 8, height = 5, dpi = 300)

firm_premia_long <- akm_firm_premia_long %>%
    transmute(firm_node, firm_id, period, gender, psi = psi_hat)
p_premia_dist <- ggplot(firm_premia_long, aes(x = psi, color = gender, fill = gender)) +
    geom_density(alpha = 0.25, linewidth = 0.8) +
    facet_wrap(~period) +
    labs(title = "Distribution of AKM Firm Premia by Gender and Period",
         x = "AKM firm premium", y = "Density", color = "Gender", fill = "Gender") +
    theme_minimal()
ggsave(file.path(output_dir, "distribution_firm_premia_gender_period.png"), p_premia_dist, width = 8, height = 5, dpi = 300)

value_dist_long <- rank_all_long %>%
    transmute(firm_node, firm_id, period, gender, value = V_hat)
p_value_dist <- ggplot(value_dist_long, aes(x = value, color = gender, fill = gender)) +
    geom_density(alpha = 0.25, linewidth = 0.8) +
    facet_wrap(~period) +
    labs(title = "Distribution of Recovered Firm Values by Gender and Period",
         x = "Recovered firm value", y = "Density", color = "Gender", fill = "Gender") +
    theme_minimal()
ggsave(file.path(output_dir, "distribution_values_hat_gender_period.png"), p_value_dist, width = 8, height = 5, dpi = 300)

compensating_data <- rank_all_long %>%
    left_join(akm_firm_premia_long %>% select(firm_node, period, female, psi_hat),
              by = c("firm_node", "period", "female"))
p_comp <- ggplot(compensating_data, aes(x = V_hat, y = psi_hat, color = gender)) +
    geom_point(alpha = 0.35, size = 0.8) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
    facet_grid(gender ~ period) +
    labs(title = "Compensating Differentials: Firm Value and Firm Premia",
         x = "Recovered firm value", y = "AKM firm premium", color = "Gender") +
    theme_classic(base_size = 14)
ggsave(file.path(output_dir, "compensating_differentials.png"), p_comp, width = 8, height = 6, dpi = 300)

print("Finish estimation!")
