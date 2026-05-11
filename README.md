## `spell_year`

`spell_year` is the stacked worker–firm–year spell dataset.  
Each row corresponds to one employment spell between a worker and a firm within a given year.  
A worker may therefore appear multiple times in the same year if they changed employer during the year.

---

## `worker_year`

`worker_year` is the dominant-employer worker–year dataset.  
Each row corresponds to one worker-year observation.  
For each worker and year, the dominant employer is defined as the firm associated with the highest annual earnings.

---

## `spell_month`

`spell_month` is the monthly employment spell dataset.  
Each row corresponds to one employed worker-month.  
It is obtained by expanding yearly spells from `spell_year` into monthly observations.

---

## `worker_month`

`worker_month` is the balanced worker-month panel.  
Each row corresponds to one worker-month between January 2018 and the worker’s last observed month.  
The dataset includes both employment and non-employment months.

---

## `workers_pairs`

`workers_pairs` is the monthly transition dataset.  
Each row corresponds to a pair of consecutive months for a given worker.  
The dataset is used to identify employment-to-employment, employment-to-nonemployment, and nonemployment-to-employment transitions.