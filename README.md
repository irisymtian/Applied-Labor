# Estimation
`estimation.r` performs an empirical labor economics analysis using worker–firm matched data before and after Covid. It estimates AKM-style firm wage premia from yearly wage data and applies a revealed-preference method to monthly worker mobility transitions in order to recover firm values, job opportunities, and worker preferences across firms. The script then compares outcomes by gender and period through summary statistics, difference-in-differences wage regressions, decomposition analyses, and visualizations such as wage distributions, firm premia distributions, and compensating differential plots. Finally, it exports regression tables, summary tables, figures, and estimated parameters.

# Simulated Data Description
1. `spell_year`:
stacked worker–firm–year spell dataset  
Each row corresponds to one employment spell between a worker and a firm within a given year.  
A worker may therefore appear multiple times in the same year if they changed employer during the year.
2. `worker_year`:
the dominant-employer worker–year dataset  
Each row corresponds to one worker-year observation.  
For each worker and year, the dominant employer is defined as the firm associated with the highest annual earnings.
3. `spell_month`:
the monthly employment spell dataset  
Each row corresponds to one employed worker-month.  
It is obtained by expanding yearly spells from `spell_year` into monthly observations.
4. `worker_month`:
the balanced worker-month panel  
Each row corresponds to one worker-month between January 2018 and the worker’s last observed month.  
The dataset includes both employment and non-employment months.
5. `workers_pairs`:
the monthly transition datasetS  
Each row corresponds to a pair of consecutive months for a given worker.  
The dataset is used to identify employment-to-employment, employment-to-nonemployment, and nonemployment-to-employment transitions.
