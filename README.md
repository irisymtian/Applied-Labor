# Applied Labor Project

This repository contains the code used to simulate, describe, and estimate a
matched employer-employee analysis of gender wage gaps before and after Covid.

The empirical logic is:

1. build or load DADS-like worker-firm data;
2. construct yearly and monthly worker panels;
3. produce descriptive statistics by gender and period;
4. estimate AKM-style firm premia and Sorkin-style firm values;
5. study how wages, firm premia, sorting, preferences, and opportunities differ
   between women and men before and after Covid.

The main periods are:

- full period: 2018-2022;
- pre-Covid: 2018-2019;
- post-Covid: 2021-2022;
- 2020 is shown in some descriptive figures but excluded from the main pre/post
  comparison.

## Repository Structure

```text
Applied-Labor/
  README.md
  simulation_final.Rmd
  simulation_final.tex
  src/
    simulate_dads_data.r
    descriptive_statistics.r
    model_estimation.r
    simulation_final.R
    simulation_full_yearly.r
  simulated_data/
    spell_year.parquet
    worker_year.parquet
    spell_month.parquet
    worker_month.parquet
    workers_pairs.parquet
    simulated_data_cache.rds
  results/
    tables/
    figures/
    estimation/
  estimation_results/
```

## Main Data Files

The project uses five DADS-like parquet files.

### `spell_year.parquet`

Stacked yearly job-spell data. One row is one worker-firm-year spell. A worker
can have several rows in the same year if they changed employer during the year.
This is the base used for spell-level descriptive statistics.

### `worker_year.parquet`

Worker-year dominant-employer data. One row is one worker-year. The dominant
employer is the firm associated with the highest annual net earnings within the
worker-year.

### `spell_month.parquet`

Monthly spell data derived from `spell_year`. One row is one employed
worker-month. Yearly spells are expanded using `debremu` and `finremu`.

### `worker_month.parquet`

Monthly worker panel derived from `spell_month`. One row is one worker-month.
It includes both employment and non-employment months up to the worker's last
observed month.

### `workers_pairs.parquet`

Monthly transition data derived from `worker_month`. One row is one worker and
one pair of consecutive months. It identifies EE, E-to-NE, and NE-to-E
transitions.

## Main Scripts

### `src/simulate_dads_data.r`

Generates a synthetic DADS-like dataset for 20,000 workers and 1,000 firms over
2018-2022. It creates the five main parquet files in `simulated_data/`.

Run with:

```r
source("src/simulate_dads_data.r")
```

or:

```bash
Rscript src/simulate_dads_data.r
```

### `src/descriptive_statistics.r`

Builds the descriptive tables and the appendix figure on gender wage trends.

It uses `simulated_data/spell_year.parquet` as input and reconstructs the
monthly panel needed for mobility statistics. The main outputs are:

- `results/tables/descriptive_statistics/table1_labor_market_outcomes.tex`;
- `results/tables/descriptive_statistics/table2_employment_transitions.tex`;
- `results/tables/descriptive_statistics/descriptive_statistics_tables.tex`;
- `results/figures/descriptive_statistics/parallel_trends_log_hourly_wage.png`;
- `results/figures/descriptive_statistics/parallel_trends_log_hourly_wage.pdf`;
- `results/figures/descriptive_statistics/parallel_trends_log_hourly_wage.csv`.

Important definitions:

- Table 1 outcomes are computed at the `spell_year` level.
- Table 2 mobility statistics are computed from the reconstructed monthly panel.
- Transition rates are worker-level incidence rates: a worker is counted once if
  the transition occurs at least once in the period.
- The `N workers` row counts workers followed at the beginning of the
  corresponding period, whether or not they have an employment spell in that
  period.

Run with:

```bash
Rscript src/descriptive_statistics.r
```

### `src/model_estimation.r`

Runs the main estimation code. It estimates:

- AKM-style firm premia by gender and period;
- Difference-in-Differences regressions for log hourly wages and firm premia;
- Sorkin-style firm values and offer probabilities from mobility transitions;
- preference and opportunity decompositions;
- correlations between male and female firm values, firm premia, and
  compensating differentials;
- role-of-firms decomposition tables.

This script currently contains real-data paths near the top:

```r
data_dir <- "C:/Users/Public/Documents/Yumiao_TIAN/Data/DADS_Panel tous salaries_2022/Processed"
output_dir <- "C:/Users/Public/Documents/Yumiao_TIAN/Result_Elisee"
```

Before running it on another machine, update these paths so that `data_dir`
points to the folder containing the parquet files and `output_dir` points to the
desired export folder.

Typical outputs include:

- `akm_firm_premia_full_period.csv`;
- `akm_firm_premia_gender_period.csv`;
- `did_results_table.tex`;
- `data_summary_by_year_gender.csv` and `.tex`;
- `gender_period_correlations.csv` and `.tex`;
- `sorkin_lambda_estimates.csv`;
- `sorkin_preference_opportunity_by_period.csv`;
- `sorkin_preference_opportunity_change.csv`;
- `sorkin_preference_opportunity_table.tex`;
- `role_of_firms_gender_gap.tex`;
- wage, firm-premia, firm-value, and compensating-differential figures.

Run with:

```bash
Rscript src/model_estimation.r
```

### `src/simulation_final.R` and `src/simulation_full_yearly.r`

Older simulation and validation scripts. They were used to test the recovery of
firm values and PageRank-style ranking procedures before adapting the workflow
to DADS-like files.

### `simulation_final.Rmd`

Readable R Markdown version of the simulation exercise. The rendered output is
`simulation_final.tex`.

## Estimation Details

### Difference-in-Differences

The DiD is used mainly as a descriptive accounting device. The coefficient on
`Female x Post` summarizes the post-Covid change in the women-men gap relative
to the pre-Covid period. It should not be interpreted as a strict causal effect
unless a parallel-trends assumption is accepted.

Adjusted specifications include worker and year fixed effects, plus sector
fixed effects at the A38 level and occupation fixed effects at the PCS4 level
when these variables are available. Controls can include age polynomials,
experience, firm tenure, and part-time status.

### AKM Firm Premia

Firm premia are estimated separately by gender and period. The code normalizes
firm premia to have a weighted mean of zero within each gender-period cell:

```text
sum_j s_jgt psi_jgt = 0
```

No particular firm is fixed as the zero-premium reference firm. The
normalization only fixes the level of the effects and does not affect
differences across firms or changes in gaps across periods.

### Sorkin-Style Firm Values

The Sorkin-style part uses monthly mobility transitions to recover firm values
and offer probabilities. To avoid memory problems with very large firm sets, the
estimation is designed to focus on firms that are connected through observed
mobility transitions.

## Main Outputs to Keep

For replication or for rebuilding report tables on another machine, the most
important exports are:

- `role_of_firms_gender_gap_components.csv`;
- `role_of_firms_gender_gap_correlations.csv`;
- `role_table_period_gender_wage.csv`;
- `akm_firm_premia_full_period.csv`;
- `akm_firm_premia_gender_period.csv`;
- `did_results_table.tex`;
- `gender_period_correlations.csv`;
- `sorkin_lambda_estimates.csv`;
- `sorkin_lambda_estimates_full_period.csv`;
- `sorkin_preference_opportunity_by_period.csv`;
- `sorkin_preference_opportunity_change.csv`;
- descriptive tables and figures from `results/tables/` and `results/figures/`.

## R Packages

The scripts use the following main packages:

- `data.table`;
- `arrow`;
- `dplyr`;
- `tidyr`;
- `ggplot2`;
- `Matrix`;
- `igraph`;
- `fixest`;
- `modelsummary` optional, for LaTeX regression tables.

Install missing packages with:

```r
install.packages(c(
  "data.table", "arrow", "dplyr", "tidyr", "ggplot2",
  "Matrix", "igraph", "fixest", "modelsummary"
))
```

## Recommended Workflow

For simulated data:

```bash
Rscript src/simulate_dads_data.r
Rscript src/descriptive_statistics.r
```

For real DADS-like data:

1. make sure the five parquet files exist in the processed data folder;
2. update `data_dir` and `output_dir` in `src/model_estimation.r`;
3. run:

```bash
Rscript src/model_estimation.r
```

## Notes for the Report

- Table 1 is spell-level: wages, hours, part-time, age, experience, and tenure
  are computed from `spell_year`.
- Table 2 is worker-level for mobility: transition rates are the share of
  workers who experience at least one transition of the relevant type.
- `Average NE duration` is the average total number of non-employment months
  among workers who experience at least one non-employment month in the period.
- The parallel-trends figure is descriptive and should be presented as an
  appendix check, not as a formal validation of a causal DiD design.
