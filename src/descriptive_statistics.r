library(data.table)
library(arrow)

# =========================================================
# Paths
# =========================================================
get_project_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- sub(file_arg, "", args[startsWith(args, file_arg)][1])

  if (!is.na(script_path) && nzchar(script_path)) {
    return(normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE))
  }

  normalizePath(getwd(), mustWork = TRUE)
}

project_dir <- get_project_dir()
data_dir <- file.path(project_dir, "simulated_data")
output_dir <- file.path(project_dir, "results/tables/descriptive_statistics")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

spell_year_path <- file.path(data_dir, "spell_year.parquet")
if (!file.exists(spell_year_path)) {
  stop("Missing required parquet file: ", spell_year_path, call. = FALSE)
}

# =========================================================
# Helpers
# =========================================================
T_months <- 60L

day_to_month <- function(day) ((day - 1L) %/% 30L) + 1L
month_to_year <- function(t) 2018L + (t - 1L) %/% 12L
period_t_min <- function(years_keep) (min(years_keep) - 2018L) * 12L + 1L
period_t_max <- function(years_keep) (max(years_keep) - 2018L + 1L) * 12L

period_defs <- list(
  full = 2018:2022,
  pre = 2018:2019,
  post = 2021:2022
)

format_number <- function(x, digits = 1L) {
  if (length(x) == 0L || is.na(x) || !is.finite(x)) {
    return("")
  }

  if (abs(x) < 0.5 * 10^(-digits)) {
    x <- 0
  }

  formatC(x, format = "f", digits = digits, big.mark = ",")
}

format_cell <- function(x, type = "number", digits = 1L, did = FALSE) {
  if (length(x) == 0L || is.na(x) || !is.finite(x)) {
    return("")
  }

  if (type == "percent") {
    suffix <- if (did) " pp" else "\\%"
    return(paste0(format_number(100 * x, digits), suffix))
  }

  if (type == "count") {
    return(formatC(round(x), format = "d", big.mark = ","))
  }

  format_number(x, digits)
}

mean_or_na <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  mean(x)
}

coerce_numeric_column <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }

  suppressWarnings(as.numeric(gsub(",", ".", trimws(as.character(x)), fixed = TRUE)))
}

write_latex_table <- function(rows, caption, label, first_col, notes, out_path) {
  header <- c(
    first_col,
    "\\shortstack{Full\\\\2018--22\\\\Women}",
    "\\shortstack{Full\\\\2018--22\\\\Men}",
    "\\shortstack{Pre-COVID\\\\2018--19\\\\Women}",
    "\\shortstack{Pre-COVID\\\\2018--19\\\\Men}",
    "\\shortstack{Post-COVID\\\\2021--22\\\\Women}",
    "\\shortstack{Post-COVID\\\\2021--22\\\\Men}",
    "\\shortstack{Diff-in-Diff\\\\(Women -- Men)}"
  )

  body <- apply(rows, 1L, function(x) paste(x, collapse = " & "))

  lines <- c(
    "% Requires: \\usepackage{booktabs,graphicx,caption}",
    "\\begin{table}[!htbp]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    "\\scriptsize",
    "\\setlength{\\tabcolsep}{2.2pt}",
    "\\resizebox{\\textwidth}{!}{%",
    "\\begin{tabular}{@{}l*{7}{r}@{}}",
    "\\toprule",
    paste(header, collapse = " & "),
    "\\\\",
    "\\midrule",
    paste0(body, "\\\\"),
    "\\bottomrule",
    "\\end{tabular}%",
    "}",
    "\\caption*{\\footnotesize",
    paste0("\\textit{Notes :} ", notes, "\\\\[0.5em]"),
    "\\textit{Source :} Simulated matched employer--employee data.",
    "}",
    "\\end{table}"
  )

  writeLines(lines, out_path, useBytes = TRUE)
}

period_gender_stats <- function(dt, years_keep, value_fun) {
  d <- dt[year %in% years_keep]

  d[
    ,
    .(
      women = value_fun(.SD[female == 1L]),
      men = value_fun(.SD[female == 0L])
    )
  ]
}

make_summary_row <- function(label, values, type = "number", digits = 1L, diff = "did") {
  last_col <- NA_real_

  if (diff == "did") {
    last_col <- (values$post_women - values$post_men) -
      (values$pre_women - values$pre_men)
  } else if (diff == "full_gap") {
    last_col <- values$full_women - values$full_men
  }

  c(
    label,
    format_cell(values$full_women, type, digits),
    format_cell(values$full_men, type, digits),
    format_cell(values$pre_women, type, digits),
    format_cell(values$pre_men, type, digits),
    format_cell(values$post_women, type, digits),
    format_cell(values$post_men, type, digits),
    if (diff == "blank") "" else format_cell(last_col, type, digits, did = type == "percent")
  )
}

worker_counts_from_spell_year <- function(dt, years_keep) {
  d <- dt[year %in% years_keep]

  d[
    ,
    .(
      women = uniqueN(nninouv[female == 1L]),
      men = uniqueN(nninouv[female == 0L])
    )
  ]
}

# =========================================================
# Load spell_year
# =========================================================
spell_year <- as.data.table(read_parquet(spell_year_path))

if (!"female" %in% names(spell_year)) {
  spell_year[, female := as.integer(sx == 0L)]
}

spell_year[, female := as.integer(female)]

if ("log_hourly_wage" %in% names(spell_year)) {
  setnames(spell_year, "log_hourly_wage", "log_hourly_wage_input")
}

numeric_columns <- c(
  "netnet", "s_brut", "nbheur", "nbheur_nouv", "nb_heur",
  "spell_days", "debremu", "finremu", "hourly_wage",
  "log_hourly_wage_input", "age", "xp", "ancsir", "last_t"
)

for (col in intersect(numeric_columns, names(spell_year))) {
  spell_year[, (col) := coerce_numeric_column(get(col))]
}

spell_year[, annual_hours_for_stats := NA_real_]
for (col in intersect(c("nbheur_nouv", "nbheur", "nb_heur"), names(spell_year))) {
  spell_year[
    (is.na(annual_hours_for_stats) |
      !is.finite(annual_hours_for_stats) |
      annual_hours_for_stats <= 0) &
      !is.na(get(col)) &
      is.finite(get(col)) &
      get(col) > 0,
    annual_hours_for_stats := get(col)
  ]
}

if (!"hourly_wage" %in% names(spell_year)) {
  spell_year[, hourly_wage := NA_real_]
}

spell_year[
  ,
  hourly_wage_from_earnings := fifelse(
    !is.na(netnet) &
      is.finite(netnet) &
      netnet > 0 &
      !is.na(annual_hours_for_stats) &
      is.finite(annual_hours_for_stats) &
      annual_hours_for_stats > 0,
    netnet / annual_hours_for_stats,
    NA_real_
  )
]

spell_year[
  is.na(hourly_wage) | !is.finite(hourly_wage) | hourly_wage <= 0,
  hourly_wage := hourly_wage_from_earnings
]

if (!"last_t" %in% names(spell_year)) {
  spell_year[, last_t := T_months]
}

spell_year[
  ,
  months_covered := fifelse(
    !is.na(spell_days) & spell_days > 0,
    pmax(spell_days / 30, 1),
    pmax((finremu - debremu + 1) / 30, 1)
  )
]

spell_year[, monthly_hours := annual_hours_for_stats / months_covered]
spell_year[
  ,
  log_hourly_wage := fifelse(
    !is.na(hourly_wage) & is.finite(hourly_wage) & hourly_wage > 0,
    log(hourly_wage),
    NA_real_
  )
]

if ("log_hourly_wage_input" %in% names(spell_year)) {
  spell_year[
    (is.na(log_hourly_wage) | !is.finite(log_hourly_wage)) &
      !is.na(log_hourly_wage_input) &
      is.finite(log_hourly_wage_input),
    log_hourly_wage := log_hourly_wage_input
  ]
}

drop_columns <- intersect(
  c("annual_hours_for_stats", "hourly_wage_from_earnings", "log_hourly_wage_input"),
  names(spell_year)
)
spell_year[, (drop_columns) := NULL]

spell_year[
  ,
  contract_type := fifelse(
    !is.na(ce_nouv) & nzchar(ce_nouv),
    ce_nouv,
    ce
  )
]
spell_year[, part_time := as.integer(contract_type == "P")]

# =========================================================
# Table 1: labor market outcomes from spell_year
# =========================================================
labor_metrics <- list(
  list(
    label = "Average hourly wage",
    type = "number",
    digits = 2L,
    fun = function(d) mean_or_na(d$hourly_wage)
  ),
  list(
    label = "Average log hourly wage",
    type = "number",
    digits = 3L,
    fun = function(d) mean_or_na(d$log_hourly_wage)
  ),
  list(
    label = "Average monthly hours",
    type = "number",
    digits = 1L,
    fun = function(d) mean_or_na(d$monthly_hours)
  ),
  list(
    label = "Share part-time",
    type = "percent",
    digits = 1L,
    fun = function(d) mean_or_na(d$part_time)
  ),
  list(
    label = "Average age",
    type = "number",
    digits = 1L,
    fun = function(d) mean_or_na(d$age)
  ),
  list(
    label = "Average experience",
    type = "number",
    digits = 1L,
    fun = function(d) mean_or_na(d$xp)
  ),
  list(
    label = "Average firm tenure",
    type = "number",
    digits = 1L,
    fun = function(d) mean_or_na(d$ancsir)
  )
)

labor_rows <- lapply(
  labor_metrics,
  function(metric) {
    full <- period_gender_stats(spell_year, period_defs$full, metric$fun)
    pre <- period_gender_stats(spell_year, period_defs$pre, metric$fun)
    post <- period_gender_stats(spell_year, period_defs$post, metric$fun)

    values <- list(
      full_women = full$women,
      full_men = full$men,
      pre_women = pre$women,
      pre_men = pre$men,
      post_women = post$women,
      post_men = post$men
    )

    make_summary_row(metric$label, values, metric$type, metric$digits)
  }
)

full_workers_t1 <- worker_counts_from_spell_year(spell_year, period_defs$full)
pre_workers_t1 <- worker_counts_from_spell_year(spell_year, period_defs$pre)
post_workers_t1 <- worker_counts_from_spell_year(spell_year, period_defs$post)

labor_rows <- c(
  labor_rows,
  list(make_summary_row(
    "N workers",
    list(
      full_women = full_workers_t1$women,
      full_men = full_workers_t1$men,
      pre_women = pre_workers_t1$women,
      pre_men = pre_workers_t1$men,
      post_women = post_workers_t1$women,
      post_men = post_workers_t1$men
    ),
    type = "count",
    diff = "blank"
  ))
)

labor_rows <- do.call(rbind, labor_rows)

table1_path <- file.path(output_dir, "table1_labor_market_outcomes.tex")
write_latex_table(
  rows = labor_rows,
  caption = "Labor Market Outcomes and Workforce Composition",
  label = "tab:labor_market_outcomes",
  first_col = "Outcome",
  notes = paste(
    "The unit of observation is a worker-firm-year spell from spell\\_year.",
    "Hourly wages, hours, age, experience, tenure, and contract status are",
    "computed directly at the spell level. Part-time spells are identified",
    "from contract type P, using ce\\_nouv when available and ce otherwise.",
    "The last column reports the post-COVID minus pre-COVID change in the",
    "women-men gap. The N row counts workers with at least one spell in the",
    "corresponding period."
  ),
  out_path = table1_path
)

# =========================================================
# Build monthly panel from spell_year for mobility statistics
# =========================================================
spell_year[, spell_row_id := .I]

spell_month_index <- spell_year[
  ,
  .(month = seq.int(day_to_month(debremu), day_to_month(finremu))),
  by = spell_row_id
]

spell_month <- spell_year[
  spell_month_index,
  on = "spell_row_id",
  allow.cartesian = TRUE
]

spell_month[, t := (year - 2018L) * 12L + month]
spell_month <- spell_month[t >= 1L & t <= T_months]

setorder(spell_month, nninouv, t, -netnet, -nbheur, spell_row_id)
spell_month <- spell_month[, .SD[1L], by = .(nninouv, t)]

worker_frame <- spell_year[
  ,
  .(
    female = first(female),
    last_t = max(last_t, na.rm = TRUE)
  ),
  by = .(worker_id = nninouv)
]

worker_frame[!is.finite(last_t) | is.na(last_t), last_t := T_months]
worker_frame[, last_t := pmin(as.integer(last_t), T_months)]

worker_month <- worker_frame[
  ,
  .(t = seq_len(T_months)),
  by = .(worker_id, female, last_t)
]

worker_month[
  ,
  `:=`(
    year = month_to_year(t),
    employment_status = fifelse(t <= last_t, 0L, NA_integer_),
    firm_id = NA_character_
  )
]

worker_month[
  spell_month[, .(worker_id = nninouv, t, firm_id = sir)],
  on = .(worker_id, t),
  `:=`(
    employment_status = 1L,
    firm_id = i.firm_id
  )
]

setorder(worker_month, worker_id, t)

m1 <- worker_month[
  t < T_months,
  .(
    worker_id,
    female,
    last_t,
    month1 = t,
    month2 = t + 1L,
    emp1 = employment_status,
    firm1 = firm_id
  )
]

m2 <- worker_month[
  ,
  .(
    worker_id,
    month2 = t,
    emp2 = employment_status,
    firm2 = firm_id
  )
]

workers_pairs <- m2[m1, on = .(worker_id, month2)]

workers_pairs[
  ,
  `:=`(
    year1 = month_to_year(month1),
    year2 = month_to_year(month2),
    observed_pair = month2 <= last_t & !is.na(emp1) & !is.na(emp2),
    ee = emp1 == 1L & emp2 == 1L & firm1 != firm2,
    en = emp1 == 1L & emp2 == 0L,
    ne = emp1 == 0L & emp2 == 1L
  )
]

period_workers <- function(years_keep) {
  worker_frame[last_t >= period_t_min(years_keep)]
}

period_pairs <- function(dt, years_keep) {
  dt[
    observed_pair == TRUE &
      month1 >= period_t_min(years_keep) &
      month2 <= period_t_max(years_keep)
  ]
}

event_incidence <- function(event_by_worker, workers) {
  d <- copy(workers[, .(worker_id, female)])
  d[, event := FALSE]

  if (nrow(event_by_worker) > 0L) {
    d[event_by_worker[event == TRUE, .(worker_id)], on = "worker_id", event := TRUE]
  }

  d[
    ,
    .(
      women = mean_or_na(as.numeric(event[female == 1L])),
      men = mean_or_na(as.numeric(event[female == 0L]))
    )
  ]
}

transition_incidence <- function(years_keep, event_col) {
  workers <- period_workers(years_keep)
  pairs <- period_pairs(workers_pairs, years_keep)

  events <- pairs[
    ,
    .(event = any(get(event_col) == TRUE, na.rm = TRUE)),
    by = worker_id
  ]

  event_incidence(events, workers)
}

stay_same_firm_incidence <- function(years_keep) {
  workers <- period_workers(years_keep)

  employed <- worker_month[
    employment_status == 1L &
      t >= period_t_min(years_keep) &
      t <= period_t_max(years_keep),
    .(worker_id, t, firm_id)
  ]

  firm_changes <- employed[
    ,
    .(
      has_employment = .N > 0L,
      has_move = any(firm_id != shift(firm_id), na.rm = TRUE)
    ),
    by = worker_id
  ]

  d <- workers[, .(worker_id, female)]
  d[, `:=`(has_employment = FALSE, has_move = FALSE)]
  d[
    firm_changes,
    on = "worker_id",
    `:=`(
      has_employment = i.has_employment,
      has_move = i.has_move
    )
  ]
  d[, event := has_employment & !has_move]

  d[
    ,
    .(
      women = mean_or_na(as.numeric(event[female == 1L])),
      men = mean_or_na(as.numeric(event[female == 0L]))
    )
  ]
}

stay_ne_3_months_incidence <- function(years_keep) {
  workers <- period_workers(years_keep)

  ne_months <- worker_month[
    employment_status == 0L &
      t >= period_t_min(years_keep) &
      t <= period_t_max(years_keep),
    .(worker_id, t)
  ]

  runs <- ne_months[
    ,
    {
      spell_group <- cumsum(is.na(shift(t)) | t != shift(t) + 1L)
      .(max_ne_run = max(tabulate(spell_group), 0L))
    },
    by = worker_id
  ]

  events <- runs[, .(worker_id, event = max_ne_run >= 3L)]
  event_incidence(events, workers)
}

mean_employed_months <- function(years_keep) {
  workers <- period_workers(years_keep)

  employed_months <- worker_month[
    t >= period_t_min(years_keep) &
      t <= period_t_max(years_keep) &
      employment_status == 1L,
    .(months = .N),
    by = worker_id
  ]

  d <- workers[, .(worker_id, female)]
  d[, months := 0L]
  d[employed_months, on = "worker_id", months := i.months]

  d[
    ,
    .(
      women = mean_or_na(months[female == 1L]),
      men = mean_or_na(months[female == 0L])
    )
  ]
}

average_ne_duration <- function(years_keep) {
  workers <- period_workers(years_keep)

  ne_months <- worker_month[
    t >= period_t_min(years_keep) &
      t <= period_t_max(years_keep) &
      employment_status == 0L,
    .(months = .N),
    by = worker_id
  ]

  d <- workers[, .(worker_id, female)]
  d[, months := 0L]
  d[ne_months, on = "worker_id", months := i.months]
  d <- d[months > 0L]

  d[
    ,
    .(
      women = mean_or_na(months[female == 1L]),
      men = mean_or_na(months[female == 0L])
    )
  ]
}

period_worker_counts <- function(years_keep) {
  workers <- period_workers(years_keep)

  workers[
    ,
    .(
      women = sum(female == 1L),
      men = sum(female == 0L)
    )
  ]
}

mobility_values <- function(metric_fun) {
  full <- metric_fun(period_defs$full)
  pre <- metric_fun(period_defs$pre)
  post <- metric_fun(period_defs$post)

  list(
    full_women = full$women,
    full_men = full$men,
    pre_women = pre$women,
    pre_men = pre$men,
    post_women = post$women,
    post_men = post$men
  )
}

transition_rows <- list(
  make_summary_row(
    "Stay same firm",
    mobility_values(stay_same_firm_incidence),
    type = "percent",
    digits = 1L
  ),
  make_summary_row(
    "EE transition",
    mobility_values(function(years_keep) transition_incidence(years_keep, "ee")),
    type = "percent",
    digits = 1L
  ),
  make_summary_row(
    "E $\\to$ NE",
    mobility_values(function(years_keep) transition_incidence(years_keep, "en")),
    type = "percent",
    digits = 1L
  ),
  make_summary_row(
    "NE $\\to$ E",
    mobility_values(function(years_keep) transition_incidence(years_keep, "ne")),
    type = "percent",
    digits = 1L
  ),
  make_summary_row(
    "Stay NE",
    mobility_values(stay_ne_3_months_incidence),
    type = "percent",
    digits = 1L
  ),
  make_summary_row(
    "Mean job spell months",
    mobility_values(mean_employed_months),
    type = "number",
    digits = 1L
  ),
  make_summary_row(
    "Average NE duration",
    mobility_values(average_ne_duration),
    type = "number",
    digits = 1L
  )
)

full_workers_t2 <- period_worker_counts(period_defs$full)
pre_workers_t2 <- period_worker_counts(period_defs$pre)
post_workers_t2 <- period_worker_counts(period_defs$post)

transition_rows <- c(
  transition_rows,
  list(make_summary_row(
    "N workers",
    list(
      full_women = full_workers_t2$women,
      full_men = full_workers_t2$men,
      pre_women = pre_workers_t2$women,
      pre_men = pre_workers_t2$men,
      post_women = post_workers_t2$women,
      post_men = post_workers_t2$men
    ),
    type = "count",
    diff = "blank"
  ))
)

transition_rows <- do.call(rbind, transition_rows)

table2_path <- file.path(output_dir, "table2_employment_transitions.tex")
write_latex_table(
  rows = transition_rows,
  caption = "Employment Transitions and Mobility",
  label = "tab:employment_transitions",
  first_col = "Transition",
  notes = paste(
    "Mobility statistics are reconstructed from spell\\_year by expanding each",
    "spell into the months covered by debremu and finremu. Transition rows",
    "report worker-level incidence rates: the denominator is the number of",
    "workers followed at least one month in the period, and a worker is",
    "counted once if the transition occurs at least once. Stay same firm is",
    "the share of workers with employment in the period and no firm move.",
    "Stay NE is the share with at least three consecutive non-employment",
    "months. Mean job spell months is the average number of employed months",
    "per worker. Average NE duration is computed among workers with at least",
    "one non-employment month in the period. The last column reports the",
    "post-COVID minus pre-COVID change in the women-men gap."
  ),
  out_path = table2_path
)

combined_path <- file.path(output_dir, "descriptive_statistics_tables.tex")
writeLines(
  c(readLines(table1_path), "", readLines(table2_path)),
  combined_path,
  useBytes = TRUE
)

message("Exported LaTeX tables:")
message(" - ", table1_path)
message(" - ", table2_path)
message(" - ", combined_path)
