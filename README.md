# Estimation
`estimation.r` performs an empirical labor economics analysis using worker–firm matched data before and after Covid. It estimates AKM-style firm wage premia from yearly wage data and applies a revealed-preference method to monthly worker mobility transitions in order to recover firm values, job opportunities, and worker preferences across firms. The script then compares outcomes by gender and period through summary statistics, difference-in-differences wage regressions, decomposition analyses, and visualizations such as wage distributions, firm premia distributions, and compensating differential plots. Finally, it exports regression tables, summary tables, figures, and estimated parameters.

# Simulation
`simulated_final.r` mainly consists of three parts:
- Data generation
- PageRank function
- Validity: the key is the correlation between the recovered value and simulated value.  
We also include some code for the descriptive analyses and regressions we plan to run with DADS data.  
`simulated_final.Rmd` is the markdowm version of the same code for readability and the results are saved as  `simulation_final.tex`.

# Simulated Data Description
1. `simulated_data_cache.rds`:  
the simulated data generated with `simulated_final.r` with pre-set parameters to test for the validity of the PageRank we constructed. 
(note: the dataset below are generated randomly just to adjust our validated `simulated_final.r` to `estimation.r` with the same data structure and variables name as the our prepossesed DADS data)
2. `spell_year`: stacked worker–firm–year spell dataset  
Each row corresponds to one employment spell between a worker and a firm within a given year.  
A worker may therefore appear multiple times in the same year if they changed employer during the year.
3. `worker_year`: the dominant-employer worker–year dataset  
Each row corresponds to one worker-year observation.  
For each worker and year, the dominant employer is defined as the firm associated with the highest annual earnings.
4. `spell_month`: the monthly employment spell dataset  
Each row corresponds to one employed worker-month.  
It is obtained by expanding yearly spells from `spell_year` into monthly observations.
5. `worker_month`: the balanced worker-month panel  
Each row corresponds to one worker-month between January 2018 and the worker’s last observed month.  
The dataset includes both employment and non-employment months.
6. `workers_pairs`: the monthly transition dataset  
Each row corresponds to a pair of consecutive months for a given worker.  
The dataset is used to identify employment-to-employment, employment-to-nonemployment, and nonemployment-to-employment transitions.
