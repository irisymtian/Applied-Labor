# Simulated Data Decription
1. `spell_year`
stacked worker–firm–year spell dataset.  
Each row corresponds to one employment spell between a worker and a firm within a given year.  
A worker may therefore appear multiple times in the same year if they changed employer during the year.
2. `worker_year`
the dominant-employer worker–year dataset.  
Each row corresponds to one worker-year observation.  
For each worker and year, the dominant employer is defined as the firm associated with the highest annual earnings.
3. `spell_month`
the monthly employment spell dataset.  
Each row corresponds to one employed worker-month.  
It is obtained by expanding yearly spells from `spell_year` into monthly observations.
4. `worker_month`
the balanced worker-month panel.  
Each row corresponds to one worker-month between January 2018 and the worker’s last observed month.  
The dataset includes both employment and non-employment months.
5. `workers_pairs`
the monthly transition dataset.  
Each row corresponds to a pair of consecutive months for a given worker.  
The dataset is used to identify employment-to-employment, employment-to-nonemployment, and nonemployment-to-employment transitions.
