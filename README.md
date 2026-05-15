# Applied Labor Project

This repository contains the code and exported outputs for a matched
employer-employee analysis of gender wage gaps before and after Covid.

Important distinction:

- `simulated_data/` contains synthetic DADS-like data created for development,
  testing, and code validation.
- `results/` contains the actual outputs obtained from the real DADS-Panel-like
  data used in the report. These are the results to use in the paper.

The real raw data are not stored in this repository. The scripts that run on the
real data point to an external processed-data folder.

## Research Goal

The project studies whether changes after Covid are associated with changes in:

- the gender gap in hourly wages;
- the gender gap in firm premia;
- the sorting of women and men across firms;
- recovered firm values, opportunities, and preferences.

The analysis is descriptive. The Difference-in-Differences specifications are
used as accounting tools to summarize changes in women-men gaps before and after
Covid, not as strict causal estimates.

Main periods:

- full period: 2018-2022;
- pre-Covid: 2018-2019;
- post-Covid: 2021-2022;
- 2020 is excluded from the main pre/post comparison.

## Repository Structure

```text
Applied-Labor/
  README.md
  src/
    simulate_dads_data.r
    descriptive_statistics.r
    model_estimation.r
    estimation_update2.r
    simulation_final.R
    simulation_full_yearly.r
    simulation_final.Rmd
    simulation_final.tex
  simulated_data/
    spell_year.parquet
    worker_year.parquet
    spell_month.parquet
    worker_month.parquet
    workers_pairs.parquet
    simulated_data_cache.rds
  results/
    tables/
      table1_labor_market_outcomes.tex
      table2_employment_transitions.tex
    figures/
      distribution_log_wage_gender_period.png
      distribution2_firm_premia_gender_period.png
      distribution_values_hat_gender_period.png
      compensating_differentials.png
    estimation/
      did_results_table.tex
      role_of_firms_gender_gap.tex
      gender_period_correlations.tex
      data_summary_by_year_gender.tex
      role_table_period_gender_wage.csv
      sorkin_lambda_estimates.csv
      sorkin_lambda_estimates_full_period.csv
      sorkin_preference_opportunity_by_period.csv
      sorkin_preference_opportunity_change.csv
      sorkin_preference_opportunity_table.tex
```

## Data Structure

The project uses five DADS-like datasets.

### `spell_year`

Stacked yearly job-spell data. One row is one worker-firm-year spell. A worker
can have several rows in the same year if they changed employer during the year.
This is not the dominant-employer annual base.

### `worker_year`

Worker-year dominant-employer data. One row is one worker-year. The dominant
employer is the firm associated with the highest annual net earnings in that
worker-year.

### `spell_month`

Monthly spell data derived from `spell_year`. One row is one employed
worker-month. Yearly spells are expanded using `debremu` and `finremu`.

### `worker_month`

Monthly worker panel derived from `spell_month`. One row is one worker-month.
It includes employment and non-employment months until the worker's last
observed month.

### `workers_pairs`

Monthly transition data derived from `worker_month`. One row is one worker and
one pair of consecutive months. It identifies:

- EE: employment-to-employment transition with firm change;
- E to NE: employment-to-non-employment transition;
- NE to E: non-employment-to-employment transition.

## Scripts

### `src/simulate_dads_data.r`

Creates synthetic DADS-like data for 20,000 workers and 1,000 firms over
2018-2022. The script exports the five parquet files in `simulated_data/`.

This script is useful for testing the full workflow without access to the real
confidential data.

Run:

```bash
Rscript src/simulate_dads_data.r
```

### `src/descriptive_statistics.r`

Creates descriptive tables and a descriptive wage-trends figure.

When run locally, the script uses the synthetic files in `simulated_data/`.
However, the current tables stored in `results/tables/` are the real-data
versions used in the report.

Main outputs:

- `results/tables/table1_labor_market_outcomes.tex`;
- `results/tables/table2_employment_transitions.tex`;
- a parallel-trends-style wage figure when the script is run in the local
  workflow.

Definitions:

- Table 1 is computed from `spell_year`; outcomes are spell-level statistics.
- Table 2 is computed from the monthly panel reconstructed from `spell_year`.
- Transition rates are worker-level incidence rates: a worker is counted once
  if the transition occurs at least once in the period.
- The `N workers` row counts workers followed at the beginning of the
  corresponding period, whether or not they have an employment spell in that
  period.
- `Average NE duration` is the average total number of non-employment months
  among workers who have at least one non-employment month in the period.

Run:

```bash
Rscript src/descriptive_statistics.r
```

### `src/model_estimation.r`

Main estimation script for the real data. It points to an external processed
DADS folder and writes outputs to an external results folder.

Near the top of the script:

```r
data_dir <- file.path("path/to/processed/parquet/files")
output_dir <- file.path("path/to/export/folder")
```

Change these paths when running the code on another machine.

The script estimates:

- AKM-style firm premia by gender and period;
- descriptive Difference-in-Differences regressions for log hourly wages and
  firm premia;
- Sorkin-style firm values and offer probabilities from mobility transitions;
- preference and opportunity decompositions;
- correlations between male and female firm values, firm premia, and
  compensating differentials;
- role-of-firms decomposition tables.

Run:

```bash
Rscript src/model_estimation.r
```

### `src/estimation_update2.r`

Local version of the estimation script that uses `simulated_data/` and writes to
a local output folder. It is useful for testing code changes before applying
them to the real-data script.

### `src/simulation_final.R` and `src/simulation_full_yearly.r`

Older simulation and validation scripts used to develop and test the recovery
of firm values and ranking methods before adapting the workflow to DADS-like
files.

### `src/simulation_final.Rmd`

Readable R Markdown version of the simulation exercise. The rendered output is
`src/simulation_final.tex`.

## Results Folder

The `results/` folder is the folder to use for the report. It contains outputs
from the real data, not merely simulated examples.

### `results/tables/`

Contains the descriptive LaTeX tables:

- `table1_labor_market_outcomes.tex`: labor market outcomes and workforce
  composition by gender and period;
- `table2_employment_transitions.tex`: employment transitions and mobility by
  gender and period.

### `results/figures/`

Contains report figures:

- `distribution_log_wage_gender_period.png`: distribution of log hourly wages by
  gender and period;
- `distribution2_firm_premia_gender_period.png`: distribution of AKM firm
  premia by gender and period;
- `distribution_values_hat_gender_period.png`: distribution of recovered firm
  values by gender and period;
- `compensating_differentials.png`: relationship between recovered firm values
  and AKM firm premia.

### `results/estimation/`

Contains estimation tables and compact exports:

- `did_results_table.tex`: descriptive DiD regressions for wages and firm
  premia;
- `role_of_firms_gender_gap.tex`: main role-of-firms decomposition table;
- `gender_period_correlations.tex`: correlations between male and female firm
  values, firm premia, and compensating differentials;
- `data_summary_by_year_gender.tex`: annual summary statistics by gender;
- `role_table_period_gender_wage.csv`: mean wages by gender and period used in
  the role-of-firms decomposition;
- `sorkin_lambda_estimates.csv`: lambda estimates by gender for pre/post
  periods;
- `sorkin_lambda_estimates_full_period.csv`: lambda estimates by gender for the
  full period;
- `sorkin_preference_opportunity_by_period.csv`: preference/opportunity
  decomposition by gender and period;
- `sorkin_preference_opportunity_change.csv`: post-minus-pre change in that
  decomposition;
- `sorkin_preference_opportunity_table.tex`: LaTeX table for the
  preference/opportunity decomposition.

## Methodological Notes

### Descriptive Difference-in-Differences

The DiD specifications are used descriptively. The coefficient on
`Female x Post` measures the post-Covid change in the women-men gap relative to
the pre-Covid period, conditional on the included controls and fixed effects.

Adjusted specifications include worker and year fixed effects, sector fixed
effects at the A38 level, occupation fixed effects at the PCS4 level, and
time-varying controls when available:

- age, age squared, and age cubed;
- potential experience and potential experience squared;
- firm tenure and firm tenure squared;
- part-time status.

Because worker fixed effects absorb time-invariant worker characteristics, the
main coefficient of interest is the interaction `Female x Post`.

### AKM Firm Premia

AKM firm premia are estimated separately by gender and period. The firm premia
are normalized so that their weighted mean is zero within each gender-period
cell:

```text
sum_j s_jgt psi_jgt = 0
```

No specific firm is chosen as the zero-premium reference firm. This
normalization fixes only the level of the firm effects and does not affect
differences across firms or changes in gender gaps across periods.

The AKM residualization includes worker/job controls and year effects. Sector
and occupation fixed effects are excluded from the AKM firm-premium estimation
because they are closely related to firm affiliation and may absorb part of the
firm component.

### Sorkin-Style Firm Values

The Sorkin-style part uses worker mobility transitions to recover firm values
and offer probabilities. The algorithm focuses on firms connected through
observed mobility transitions, because firms without mobility links do not
provide identifying variation for recovered firm values.

## R Packages

Main packages:

- `data.table`;
- `arrow`;
- `dplyr`;
- `tidyr`;
- `ggplot2`;
- `Matrix`;
- `igraph`;
- `fixest`;
- `modelsummary` optional, used for LaTeX regression tables.

Install missing packages with:

```r
install.packages(c(
  "data.table", "arrow", "dplyr", "tidyr", "ggplot2",
  "Matrix", "igraph", "fixest", "modelsummary"
))
```

## Recommended Workflow

For local testing with simulated data:

```bash
Rscript src/simulate_dads_data.r
Rscript src/descriptive_statistics.r
Rscript src/estimation_update2.r
```

For the real data:

1. make sure the five parquet files exist in the external processed data folder;
2. update `data_dir` and `output_dir` in `src/model_estimation.r` if needed;
3. run:

```bash
Rscript src/model_estimation.r
```

Then copy or keep the relevant exported tables and figures in `results/`.

## Report Notes

- Use `results/` for the report outputs.
- Treat `simulated_data/` as a development and testing input, not as the source
  of the final reported estimates.
- The parallel-trends figure is best presented as a descriptive appendix check,
  not as a formal validation of a causal DiD design.
- The DiD estimates should be described as conditional changes in gender gaps,
  not as causal effects of Covid.
