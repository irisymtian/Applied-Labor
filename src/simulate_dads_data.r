library(data.table)
library(arrow)

set.seed(123)

# =========================================================
# Paths
# =========================================================
script_args <- commandArgs(trailingOnly = FALSE)
script_file_arg <- "--file="
script_path <- sub(script_file_arg, "", script_args[startsWith(script_args, script_file_arg)][1])
project_dir <- if (!is.na(script_path) && nzchar(script_path)) {
  normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
} else {
  normalizePath(getwd(), mustWork = TRUE)
}
data_dir <- file.path(project_dir, "simulated_data")

# =========================================================
# Parameters
# =========================================================
n_workers <- 20000L
n_firms <- 1000L
years <- 2018:2022
T_months <- 60L

firm_ids <- sprintf("%09d", seq_len(n_firms))
worker_ids <- paste0("N", sprintf("%011d", seq_len(n_workers)))

month_to_year <- function(t) 2018L + (t - 1L) %/% 12L
month_in_year <- function(t) (t - 1L) %% 12L + 1L
month_start_day <- function(month) (month - 1L) * 30L + 1L
month_end_day <- function(month) month * 30L
day_to_month <- function(day) ((day - 1L) %/% 30L) + 1L

# =========================================================
# 1. Worker frame and simulated monthly employment histories
#    This monthly history is only the simulation engine.
#    Final worker_month is rebuilt later from spell_month.
# =========================================================
workers <- data.table(
  worker_id = worker_ids,
  male = rbinom(n_workers, 1L, 0.5)
)

workers[, female := 1L - male]
workers[, base_age_2018 := sample(25:55, n_workers, replace = TRUE)]

# Around 15% exit before December 2022.
exit_workers <- sample(workers$worker_id, round(0.15 * n_workers))
workers[, last_t := T_months]
workers[
  worker_id %chin% exit_workers,
  last_t := sample(24:59, .N, replace = TRUE)
]

# Ensure every firm is represented in the initial allocation.
workers[, initial_firm := sample(rep(firm_ids, length.out = .N))]

month_seed <- workers[
  ,
  .(t = seq_len(T_months)),
  by = .(worker_id, male, female, base_age_2018, last_t, initial_firm)
]

month_seed[
  ,
  `:=`(
    year = month_to_year(t),
    month = month_in_year(t),
    age = base_age_2018 + month_to_year(t) - 2018L,
    employment_status = NA_integer_
  )
]

month_seed[t <= last_t, employment_status := rbinom(.N, 1L, 0.92)]
month_seed[t == 1L, employment_status := 1L]

# Around 30% of workers change firm at least once.
mover_ids <- sample(workers$worker_id, round(0.30 * n_workers))

move_plan <- workers[
  worker_id %chin% mover_ids,
  {
    n_moves <- sample(1:3, 1L, prob = c(0.65, 0.25, 0.10))
    possible_months <- 2:last_t
    .(t = sort(sample(possible_months, min(n_moves, length(possible_months)))))
  },
  by = worker_id
]

# A direct employer-to-employer move requires employment in month t - 1 and t.
force_employed <- unique(rbind(
  move_plan[, .(worker_id, t)],
  move_plan[, .(worker_id, t = t - 1L)]
))

month_seed[force_employed, on = .(worker_id, t), employment_status := 1L]
month_seed[t > last_t, employment_status := NA_integer_]

month_seed[, is_move_month := FALSE]
month_seed[move_plan, on = .(worker_id, t), is_move_month := TRUE]

setorder(month_seed, worker_id, t)

month_seed[
  ,
  firm_id := {
    current_firm <- initial_firm[1L]
    out <- rep(NA_character_, .N)

    for (i in seq_len(.N)) {
      if (is.na(employment_status[i]) || employment_status[i] == 0L) {
        out[i] <- NA_character_
      } else {
        if (is_move_month[i]) {
          current_firm <- sample(firm_ids[firm_ids != current_firm], 1L)
        }
        out[i] <- current_firm
      }
    }

    out
  },
  by = worker_id
]

stopifnot(month_seed[t == 1L, all(employment_status == 1L)])

# =========================================================
# 2. spell_year
#    Stacked DADS-Panel-like base.
#    One row = one continuous worker-firm-year job spell.
# =========================================================
employed_months <- month_seed[
  employment_status == 1L,
  .(
    worker_id,
    male,
    female,
    base_age_2018,
    last_t,
    t,
    year,
    month,
    age,
    firm_id
  )
]

setorder(employed_months, worker_id, t)

employed_months[
  ,
  new_spell := is.na(shift(t)) |
    t != shift(t) + 1L |
    year != shift(year) |
    firm_id != shift(firm_id),
  by = worker_id
]

employed_months[, worker_spell_seq := cumsum(new_spell), by = worker_id]
employed_months[, spell_id := .GRP, by = .(worker_id, worker_spell_seq)]

spell_year <- employed_months[
  ,
  .(
    start_t = min(t),
    end_t = max(t),
    start_month = min(month),
    end_month = max(month),
    nb_months = .N,
    sx = first(male),
    male = first(male),
    female = first(female),
    base_age_2018 = first(base_age_2018),
    last_t = first(last_t),
    age = first(age)
  ),
  by = .(spell_id, nninouv = worker_id, year, sir = firm_id)
]

spell_year[
  ,
  `:=`(
    nic4 = sprintf("%04d", sample(1:9999, .N, replace = TRUE)),
    regt = "11",
    a38 = sample(c("BZ", "CZ", "FZ", "GZ", "HZ", "JZ", "KZ", "MN", "OQ", "RU"), .N, replace = TRUE),
    pcs4 = sample(c("461b", "523a", "542a", "545c", "621a", "632b"), .N, replace = TRUE),
    hourly_wage = round(rlnorm(.N, log(20), 0.35), 6),
    debremu = month_start_day(start_month),
    finremu = month_end_day(end_month),
    nbheur = round(nb_months * 151.67),
    nbheur_nouv = round(nb_months * 151.67),
    dp = nb_months * 30L,
    dp_nouv = nb_months * 30L,
    spell_days = nb_months * 30L,
    xp = pmax(age - sample(18:25, .N, replace = TRUE), 0),
    ancsir = sample(0:20, .N, replace = TRUE),
    ce = "C",
    ce_nouv = "C",
    apet2 = sample(c("47", "56", "62", "70", "84", "86"), .N, replace = TRUE),
    apen2 = sample(c("47", "56", "62", "70", "84", "86"), .N, replace = TRUE)
  )
]

spell_year[
  ,
  `:=`(
    netnet = round(hourly_wage * nbheur * runif(.N, 0.90, 1.10), 0),
    s_brut = round(hourly_wage * nbheur * runif(.N, 1.15, 1.35), 0),
    nb_heur = nbheur,
    cs1 = substr(pcs4, 1L, 1L),
    establishment_id = paste0(sir, nic4)
  )
]

setcolorder(
  spell_year,
  c(
    "spell_id", "nninouv", "sir", "nic4", "establishment_id", "year",
    "start_t", "end_t", "start_month", "end_month", "debremu", "finremu",
    "sx", "male", "female", "regt", "age", "base_age_2018", "last_t",
    "xp", "ancsir", "ce", "ce_nouv", "a38", "pcs4", "netnet", "s_brut", "nbheur",
    "nbheur_nouv", "apet2", "apen2", "cs1", "dp", "dp_nouv",
    "spell_days", "nb_heur", "nb_months", "hourly_wage"
  )
)

setorder(spell_year, nninouv, year, start_month, spell_id)

# =========================================================
# 3. worker_year
#    Derived from spell_year.
#    One row = one worker-year, keeping the dominant employer.
# =========================================================
worker_year_candidates <- spell_year[
  ,
  .(
    netnet = sum(netnet),
    s_brut = sum(s_brut),
    nbheur = sum(nbheur),
    nbheur_nouv = sum(nbheur_nouv),
    spell_days = sum(spell_days),
    nb_months = sum(nb_months),
    dominant_spell_id = spell_id[which.max(netnet)],
    sx = first(sx),
    male = first(male),
    female = first(female),
    regt = first(regt),
    age = first(age),
    base_age_2018 = first(base_age_2018),
    last_t = first(last_t),
    a38 = a38[which.max(netnet)],
    pcs4 = pcs4[which.max(netnet)],
    hourly_wage = weighted.mean(hourly_wage, nbheur),
    first_month = min(start_month),
    last_month = max(end_month)
  ),
  by = .(nninouv, year, sir)
]

worker_year_totals <- spell_year[
  ,
  .(
    total_netnet = sum(netnet),
    total_s_brut = sum(s_brut),
    total_nbheur = sum(nbheur),
    n_spells = .N,
    n_firms_year = uniqueN(sir)
  ),
  by = .(nninouv, year)
]

setorder(worker_year_candidates, nninouv, year, -netnet, -nbheur, sir)

worker_year <- worker_year_candidates[
  ,
  .SD[1L],
  by = .(nninouv, year)
]

worker_year <- worker_year_totals[
  worker_year,
  on = .(nninouv, year)
]

setcolorder(
  worker_year,
  c(
    "nninouv", "year", "sir", "dominant_spell_id", "sx", "male", "female",
    "regt", "age", "base_age_2018", "last_t", "a38", "pcs4", "netnet", "s_brut",
    "nbheur", "nbheur_nouv", "spell_days", "nb_months", "hourly_wage",
    "first_month", "last_month", "total_netnet", "total_s_brut",
    "total_nbheur", "n_spells", "n_firms_year"
  )
)

setorder(worker_year, nninouv, year)

# =========================================================
# 4. spell_month
#    Derived from spell_year by expanding debremu-finremu months.
# =========================================================
spell_month_index <- spell_year[
  ,
  .(month = seq.int(day_to_month(debremu), day_to_month(finremu))),
  by = spell_id
]

spell_month <- spell_year[
  spell_month_index,
  on = "spell_id",
  allow.cartesian = TRUE
]

spell_month[, t := (year - 2018L) * 12L + month]

# Required overlap rule: keep highest netnet, then highest nbheur.
setorder(spell_month, nninouv, t, -netnet, -nbheur, spell_id)
spell_month <- spell_month[
  ,
  .SD[1L],
  by = .(nninouv, t)
]

spell_month <- spell_month[
  ,
  .(
    spell_id,
    worker_id = nninouv,
    t,
    year,
    month,
    firm_id = sir,
    establishment_id,
    region = regt,
    sector = a38,
    occupation = pcs4,
    male,
    female,
    base_age_2018,
    last_t,
    age,
    employment_status = 1L,
    hourly_wage,
    netnet,
    s_brut,
    nbheur,
    nb_months,
    debremu,
    finremu
  )
]

setorder(spell_month, worker_id, t)

# =========================================================
# 5. worker_month
#    Derived from spell_month.
# =========================================================
worker_frame <- unique(spell_month[, .(worker_id, male, female, base_age_2018, last_t)])

worker_month <- worker_frame[
  ,
  .(t = seq_len(T_months)),
  by = .(worker_id, male, female, base_age_2018, last_t)
]

worker_month[
  ,
  `:=`(
    year = month_to_year(t),
    month = month_in_year(t),
    age = base_age_2018 + month_to_year(t) - 2018L,
    employment_status = fifelse(t <= last_t, 0L, NA_integer_),
    firm_id = NA_character_,
    establishment_id = NA_character_,
    region = NA_character_,
    sector = NA_character_,
    occupation = NA_character_,
    hourly_wage = NA_real_,
    netnet = NA_real_,
    s_brut = NA_real_,
    nbheur = NA_real_,
    spell_id = NA_integer_
  )
]

worker_month[
  spell_month,
  on = .(worker_id, t),
  `:=`(
    employment_status = 1L,
    firm_id = i.firm_id,
    establishment_id = i.establishment_id,
    region = i.region,
    sector = i.sector,
    occupation = i.occupation,
    hourly_wage = i.hourly_wage,
    netnet = i.netnet,
    s_brut = i.s_brut,
    nbheur = i.nbheur,
    spell_id = i.spell_id
  )
]

setcolorder(
  worker_month,
  c(
    "worker_id", "t", "year", "month", "employment_status", "firm_id",
    "establishment_id", "region", "sector", "occupation", "hourly_wage",
    "netnet", "s_brut", "nbheur", "spell_id", "last_t", "male", "female",
    "base_age_2018", "age"
  )
)

setorder(worker_month, worker_id, t)

# =========================================================
# 6. workers_pairs
#    Derived from worker_month.
# =========================================================
m1 <- worker_month[
  t < T_months,
  .(
    worker_id,
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
    ee = fifelse(
      month2 > last_t | is.na(emp1) | is.na(emp2),
      NA_integer_,
      fifelse(emp1 == 1L & emp2 == 1L & firm1 != firm2, 1L, 0L)
    ),
    en = fifelse(
      month2 > last_t | is.na(emp1) | is.na(emp2),
      NA_integer_,
      fifelse(emp1 == 1L & emp2 == 0L, 1L, 0L)
    ),
    ne = fifelse(
      month2 > last_t | is.na(emp1) | is.na(emp2),
      NA_integer_,
      fifelse(emp1 == 0L & emp2 == 1L, 1L, 0L)
    ),
    last_t_less_60 = last_t < T_months
  )
]

workers_pairs <- workers_pairs[
  ,
  .(
    worker_id,
    last_t,
    month1,
    month2,
    emp1,
    firm1,
    emp2,
    firm2,
    ee,
    en,
    ne,
    last_t_less_60
  )
]

setorder(workers_pairs, worker_id, month1)

# =========================================================
# 7. Internal consistency checks
# =========================================================
stopifnot(nrow(workers) == n_workers)
stopifnot(uniqueN(spell_year$sir) == n_firms)
stopifnot(worker_month[t == 1L, all(employment_status == 1L)])
stopifnot(worker_month[t > last_t, all(is.na(employment_status))])
stopifnot(worker_month[t <= last_t & is.na(spell_id), all(employment_status == 0L)])
stopifnot(nrow(worker_month) == n_workers * T_months)
stopifnot(nrow(workers_pairs) == n_workers * (T_months - 1L))
stopifnot(workers_pairs[, all(month2 == month1 + 1L)])
stopifnot(workers_pairs[, all(month1 %between% c(1L, 59L))])

worker_year_keys <- worker_year[, .(nninouv, year, sir)]
spell_year_keys <- unique(spell_year[, .(nninouv, year, sir)])
stopifnot(nrow(worker_year_keys[!spell_year_keys, on = .(nninouv, year, sir)]) == 0L)

stopifnot(nrow(spell_month[!spell_year[, .(spell_id)], on = "spell_id"]) == 0L)
stopifnot(!anyDuplicated(spell_month[, .(worker_id, t)]))

spell_month_check <- spell_month[, .(worker_id, t, firm_id)]
worker_month_check <- worker_month[
  employment_status == 1L,
  .(worker_id, t, firm_id)
]
stopifnot(nrow(worker_month_check[!spell_month_check, on = .(worker_id, t, firm_id)]) == 0L)
stopifnot(nrow(spell_month_check[!worker_month_check, on = .(worker_id, t, firm_id)]) == 0L)

pair_firms <- unique(c(
  workers_pairs[!is.na(firm1), firm1],
  workers_pairs[!is.na(firm2), firm2]
))
stopifnot(all(pair_firms %chin% spell_month$firm_id))

stopifnot(workers_pairs[month2 > last_t, all(is.na(ee) & is.na(en) & is.na(ne))])

changed_workers <- employed_months[, .(n_firms_seen = uniqueN(firm_id)), by = worker_id][
  n_firms_seen > 1L,
  .N
]

multi_employer_worker_years <- spell_year[
  ,
  .(n_firms_year = uniqueN(sir)),
  by = .(nninouv, year)
][n_firms_year > 1L, .N]

exit_rate <- mean(workers$last_t < T_months)
move_rate <- changed_workers / n_workers

message("Simulation checks passed.")
message("Workers: ", n_workers)
message("Firms observed in spell_year: ", uniqueN(spell_year$sir))
message("Exit rate before month 60: ", round(exit_rate, 3))
message("Workers changing firm at least once: ", round(move_rate, 3))
message("Worker-years with multiple employers: ", multi_employer_worker_years)
message("spell_year rows: ", nrow(spell_year))
message("worker_year rows: ", nrow(worker_year))
message("spell_month rows: ", nrow(spell_month))
message("worker_month rows: ", nrow(worker_month))
message("workers_pairs rows: ", nrow(workers_pairs))

# =========================================================
# 8. Export parquet files
# =========================================================
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

write_parquet(spell_year, file.path(data_dir, "spell_year.parquet"))
write_parquet(worker_year, file.path(data_dir, "worker_year.parquet"))
write_parquet(spell_month, file.path(data_dir, "spell_month.parquet"))
write_parquet(worker_month, file.path(data_dir, "worker_month.parquet"))
write_parquet(workers_pairs, file.path(data_dir, "workers_pairs.parquet"))

message("Exported parquet files to ", data_dir)
