# DEOptim Notes

## No Benefit for cr_gradient

```R
heuristic_penalty <- function(summary) {
  0 +
    (1 - summary$AUC) * 5 +
    (1 - summary$per_admin_in) * 2 +
    (1 - summary$per_admin_pos) * 3 +
    tanh_penalty(summary$ch_hr_lower, 9, 1, 1) +
    tanh_penalty(summary$ch_hr_upper, 9, 1, 1) +
    tanh_penalty(summary$aki_hr_upper, 15, 15, -1) +
    tanh_penalty(summary$aki_hr_upper, 60, 10, 1) +
    grepl("APACHE_II", summary$glm_model) * 5 +
    grepl("APACHE_III", summary$glm_model) * 5 +
    grepl("Baseline_Cr", summary$glm_model) +
    grepl("\\bcr\\b", summary$glm_model) * 3
}

Iteration: 1 bestvalit: 5.197201 bestmemit:    5.500000    1.100000   11.500000   67.500000
Iteration: 2 bestvalit: 4.029217 bestmemit:    4.600000    0.900000    9.600000   16.000000
Iteration: 3 bestvalit: 4.029217 bestmemit:    4.600000    0.900000    9.600000   16.000000
Iteration: 4 bestvalit: 4.029217 bestmemit:    4.600000    0.900000    9.600000   16.000000
Iteration: 5 bestvalit: 4.029217 bestmemit:    4.600000    0.900000    9.600000   16.000000
Iteration: 6 bestvalit: 4.029217 bestmemit:    4.600000    0.900000    9.600000   16.000000
Iteration: 7 bestvalit: 4.029217 bestmemit:    4.600000    0.900000    9.600000   16.000000
Iteration: 8 bestvalit: 4.029217 bestmemit:    4.600000    0.900000    9.600000   16.000000
Iteration: 9 bestvalit: 4.029217 bestmemit:    4.600000    0.900000    9.600000   16.000000
Iteration: 10 bestvalit: 4.029217 bestmemit:    4.600000    0.900000    9.600000   16.000000
Iteration: 11 bestvalit: 4.009710 bestmemit:    5.300000    1.400000    9.200000   24.500000
Iteration: 12 bestvalit: 4.009710 bestmemit:    5.300000    1.400000    9.200000   24.500000
Iteration: 13 bestvalit: 4.009710 bestmemit:    5.300000    1.400000    9.200000   24.500000
Iteration: 14 bestvalit: 3.933016 bestmemit:    5.300000    1.600000    8.600000   15.800000
Iteration: 15 bestvalit: 3.829303 bestmemit:    5.100000    1.000000    7.100000   29.900000
Iteration: 16 bestvalit: 3.829303 bestmemit:    5.100000    1.000000    7.100000   29.900000
Iteration: 17 bestvalit: 3.829303 bestmemit:    5.100000    1.000000    7.100000   29.900000
Iteration: 18 bestvalit: 3.829303 bestmemit:    5.100000    1.000000    7.100000   29.900000
Iteration: 19 bestvalit: 3.829303 bestmemit:    5.100000    1.000000    7.100000   29.900000
Iteration: 20 bestvalit: 3.829303 bestmemit:    5.100000    1.000000    7.100000   29.900000

> publish(multi_bestmem$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95 p-value
1            PCs_cardio            4.13  [2.09;8.17]  <0.001
2           Vasopressor            3.27  [1.69;6.32]  <0.001
3 Chronic_liver_disease           23.49 [9.54;57.83]  <0.001

> multi_bestmem$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos
1 0.8555132   0.7924528   0.8492908         0.130366    0.4909561     0.3188406          190               22
  n_admissions_neg n_UR   n n_event_pos n_event_neg                                                   glm_model
1              168  184 617          53         564 AKI_2or3 ~ PCs_cardio + Vasopressor + Chronic_liver_disease
    AUC_all ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1 0.8768232         4.6         5.6          7.1           37
```

## Weight for cr_gradient

```R
heuristic_penalty <- function(summary) {
  0 +
    (1 - summary$AUC) * 5 +
    (1 - summary$per_admin_in) * 2 +
    (1 - summary$per_admin_pos) * 3 +
    tanh_penalty(summary$ch_hr_lower, 9, 1, 1) +
    tanh_penalty(summary$ch_hr_upper, 9, 1, 1) +
    tanh_penalty(summary$aki_hr_upper, 15, 15, -1) +
    tanh_penalty(summary$aki_hr_upper, 60, 10, 1) +
    grepl("APACHE_II", summary$glm_model) * 5 +
    grepl("APACHE_III", summary$glm_model) * 5 +
    grepl("Baseline_Cr", summary$glm_model) +
    grepl("\\bcr\\b", summary$glm_model) * 3 +
    (1 - grepl("cr_gradient", summary$glm_model)) * 5
  # other
}

Iteration: 1 bestvalit: 4.734422 bestmemit:    5.000000    1.300000    8.900000   70.100000
Iteration: 2 bestvalit: 3.829685 bestmemit:    5.000000    1.300000    8.900000   38.300000

> publish(multi_bestmem$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95   p-value
1            PCs_cardio            3.25  [1.56;6.76]   0.00160
2           Vasopressor            2.54  [1.27;5.07]   0.00809
3                    HT            0.38  [0.17;0.86]   0.02046
4 Chronic_liver_disease           17.23 [7.22;41.11]   < 0.001
5           cr_gradient            3.19  [1.61;6.32]   < 0.001
> multi_bestmem$summary
       AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos
1 0.879927   0.7446809   0.8753351        0.1085311    0.5271318     0.2608696          204               18
  n_admissions_neg n_UR   n n_event_pos n_event_neg
1              186  198 793          47         746
                                                                       glm_model   AUC_all ch_hr_lower ch_hr_upper
1 AKI_2or3 ~ PCs_cardio + Vasopressor + HT + Chronic_liver_disease + cr_gradient 0.8729108        4.35        5.65
  aki_hr_lower aki_hr_upper
1          8.9         47.2

Iteration: 1 bestvalit: 4.734422 bestmemit:    5.000000    1.300000    8.900000   70.100000
Iteration: 2 bestvalit: 3.829685 bestmemit:    5.000000    1.300000    8.900000   38.300000
Iteration: 3 bestvalit: 3.829685 bestmemit:    5.000000    1.300000    8.900000   38.300000
Iteration: 4 bestvalit: 3.829685 bestmemit:    5.000000    1.300000    8.900000   38.300000
Iteration: 5 bestvalit: 3.829685 bestmemit:    5.000000    1.300000    8.900000   38.300000
Iteration: 6 bestvalit: 3.829685 bestmemit:    5.000000    1.300000    8.900000   38.300000
Iteration: 7 bestvalit: 3.829685 bestmemit:    5.000000    1.300000    8.900000   38.300000
Iteration: 8 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 9 bestvalit: 3.784056 bestmemit:    5.000000    1.600000    8.700000   17.300000
Iteration: 10 bestvalit: 3.784056 bestmemit:    5.000000    1.600000    8.700000   17.300000
Iteration: 11 bestvalit: 3.779393 bestmemit:    4.900000    1.600000    8.700000   17.300000
Iteration: 12 bestvalit: 3.779393 bestmemit:    4.900000    1.600000    8.700000   17.300000
Iteration: 13 bestvalit: 3.779393 bestmemit:    4.900000    1.600000    8.700000   17.300000
Iteration: 14 bestvalit: 3.779393 bestmemit:    4.900000    1.600000    8.700000   17.300000
Iteration: 15 bestvalit: 3.779393 bestmemit:    4.900000    1.600000    8.700000   17.300000
Iteration: 16 bestvalit: 3.779393 bestmemit:    4.900000    1.600000    8.700000   17.300000
Iteration: 17 bestvalit: 3.779393 bestmemit:    4.900000    1.600000    8.700000   17.300000
Iteration: 18 bestvalit: 3.779393 bestmemit:    4.900000    1.600000    8.700000   17.300000
Iteration: 19 bestvalit: 3.779393 bestmemit:    4.900000    1.600000    8.700000   17.300000
Iteration: 20 bestvalit: 3.779393 bestmemit:    4.900000    1.600000    8.700000   17.300000

> publish(multi_bestmem$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95  p-value
1             Mecvenadm            0.30  [0.13;0.72]   0.0064
2            PCs_cardio            7.49 [3.43;16.36]   <0.001
3           Vasopressor            3.80  [1.93;7.44]   <0.001
4                    HT            0.28  [0.14;0.59]   <0.001
5 Chronic_liver_disease           23.67 [9.30;60.24]   <0.001
6           cr_gradient            2.89  [1.54;5.42]   <0.001

> multi_bestmem$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos
1 0.8728321    0.754386   0.8648339        0.1329848    0.5555556     0.3043478          215               21
  n_admissions_neg n_UR   n n_event_pos n_event_neg
1              194  209 930          57         873
                                                                                   glm_model   AUC_all ch_hr_lower
1 AKI_2or3 ~ Mecvenadm + PCs_cardio + Vasopressor + HT + Chronic_liver_disease + cr_gradient 0.8921445         4.1
  ch_hr_upper aki_hr_lower aki_hr_upper
1         5.7          8.7           26
```

## Increased weighting for cr_gradient

```R
heuristic_penalty <- function(summary) {
  0 +
    (1 - summary$AUC) * 5 +
    (1 - summary$per_admin_in) * 2 +
    (1 - summary$per_admin_pos) * 3 +
    tanh_penalty(summary$ch_hr_lower, 9, 1, 1) +
    tanh_penalty(summary$ch_hr_upper, 9, 1, 1) +
    tanh_penalty(summary$aki_hr_upper, 15, 15, -1) +
    tanh_penalty(summary$aki_hr_upper, 60, 10, 1) +
    grepl("APACHE_II", summary$glm_model) * 5 +
    grepl("APACHE_III", summary$glm_model) * 5 +
    grepl("Baseline_Cr", summary$glm_model) +
    grepl("\\bcr\\b", summary$glm_model) * 3 +
    (1 - grepl("cr_gradient", summary$glm_model)) * 6
  # other
}

Iteration: 1 bestvalit: 4.734422 bestmemit:    5.000000    1.300000    8.900000   70.100000
Iteration: 2 bestvalit: 3.829685 bestmemit:    5.000000    1.300000    8.900000   38.300000
Iteration: 3 bestvalit: 3.829685 bestmemit:    5.000000    1.300000    8.900000   38.300000
Iteration: 4 bestvalit: 3.829685 bestmemit:    5.000000    1.300000    8.900000   38.300000
Iteration: 5 bestvalit: 3.829685 bestmemit:    5.000000    1.300000    8.900000   38.300000
Iteration: 6 bestvalit: 3.829685 bestmemit:    5.000000    1.300000    8.900000   38.300000
Iteration: 7 bestvalit: 3.829685 bestmemit:    5.000000    1.300000    8.900000   38.300000
Iteration: 8 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 9 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 10 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 11 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 12 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 13 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 14 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 15 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 16 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 17 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 18 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 19 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 20 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000


> publish(multi_bestmem$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95   p-value
1            PCs_cardio            3.30  [1.75;6.22]   < 0.001
2           Vasopressor            2.50  [1.37;4.56]   0.00279
3                    HT            0.40  [0.20;0.80]   0.00934
4 Chronic_liver_disease           11.81 [5.57;25.07]   < 0.001
5           cr_gradient            2.86  [1.58;5.17]   < 0.001
> multi_bestmem$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos
1 0.8694461   0.7704918    0.864486        0.1214668    0.5426357     0.2608696          210               18
  n_admissions_neg n_UR   n n_event_pos n_event_neg
1              192  204 917          61         856
                                                                       glm_model   AUC_all ch_hr_lower ch_hr_upper
1 AKI_2or3 ~ PCs_cardio + Vasopressor + HT + Chronic_liver_disease + cr_gradient 0.8736977         4.5         6.1
  aki_hr_lower aki_hr_upper
1          9.3           40
```

## Re-run with NP = 160

Consider increasing NP later, or running multiple times

```R
Iteration: 1 bestvalit: 3.452473 bestmemit:    4.100000    5.600000    3.700000   41.200000
Iteration: 2 bestvalit: 3.450481 bestmemit:    4.100000    6.000000    3.900000   32.300000
Iteration: 3 bestvalit: 3.398991 bestmemit:    4.900000    5.700000    3.100000   38.800000
Iteration: 4 bestvalit: 3.398991 bestmemit:    4.900000    5.700000    3.100000   38.800000
Iteration: 5 bestvalit: 3.263448 bestmemit:    4.100000    5.900000    3.500000   33.400000
Iteration: 6 bestvalit: 3.263448 bestmemit:    4.100000    5.900000    3.500000   33.400000
Iteration: 7 bestvalit: 3.160532 bestmemit:    4.000000    5.700000    3.100000   38.800000
Iteration: 8 bestvalit: 3.147642 bestmemit:    4.000000    5.700000    3.000000   38.800000
Iteration: 9 bestvalit: 3.147642 bestmemit:    4.000000    5.700000    3.000000   38.800000
Iteration: 10 bestvalit: 3.147642 bestmemit:    4.000000    5.700000    3.000000   38.800000
Iteration: 11 bestvalit: 3.147642 bestmemit:    4.000000    5.700000    3.000000   38.800000
Iteration: 12 bestvalit: 3.147642 bestmemit:    4.000000    5.700000    3.000000   38.800000
Iteration: 13 bestvalit: 3.147642 bestmemit:    4.000000    5.700000    3.000000   38.800000
Iteration: 14 bestvalit: 3.147642 bestmemit:    4.000000    5.700000    3.000000   38.800000
Iteration: 15 bestvalit: 3.141014 bestmemit:    4.100000    6.000000    3.000000   34.400000
Iteration: 16 bestvalit: 3.141014 bestmemit:    4.100000    6.000000    3.000000   34.400000
Iteration: 17 bestvalit: 3.141014 bestmemit:    4.100000    6.000000    3.000000   34.400000
Iteration: 18 bestvalit: 3.141014 bestmemit:    4.100000    6.000000    3.000000   34.400000
Iteration: 19 bestvalit: 3.141014 bestmemit:    4.100000    6.000000    3.000000   34.400000
Iteration: 20 bestvalit: 3.141014 bestmemit:    4.100000    6.000000    3.000000   34.400000

> publish(cr_ch_bestmem$model, print = FALSE, digits = c(2, 3))$regressionTable
     Variable Units OddsRatio       CI.95 p-value
1 cr_gradient            2.50 [2.09;2.99]  <0.001

> cr_ch_bestmem$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR    n
1 0.6034536    0.464837   0.7420701        0.1654717    0.8630491     0.7246377          334               50              284  325 4177
  n_event_pos n_event_neg              glm_model   AUC_all ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1         583        3594 AKI_2or3 ~ cr_gradient 0.6034536         1.1         7.1            3         37.4
```

```R
Iteration: 1 bestvalit: 4.734422 bestmemit:    5.000000    1.300000    8.900000   70.100000
Iteration: 2 bestvalit: 3.829685 bestmemit:    5.000000    1.300000    8.900000   38.300000
Iteration: 3 bestvalit: 3.829685 bestmemit:    5.000000    1.300000    8.900000   38.300000
Iteration: 4 bestvalit: 3.829685 bestmemit:    5.000000    1.300000    8.900000   38.300000
Iteration: 5 bestvalit: 3.829685 bestmemit:    5.000000    1.300000    8.900000   38.300000
Iteration: 6 bestvalit: 3.829685 bestmemit:    5.000000    1.300000    8.900000   38.300000
Iteration: 7 bestvalit: 3.829685 bestmemit:    5.000000    1.300000    8.900000   38.300000
Iteration: 8 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 9 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 10 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 11 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 12 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 13 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 14 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 15 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 16 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 17 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 18 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 19 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000
Iteration: 20 bestvalit: 3.823878 bestmemit:    5.300000    1.600000    9.300000   30.700000

> publish(multi_bestmem$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95   p-value
1            PCs_cardio            3.30  [1.75;6.22]   < 0.001
2           Vasopressor            2.50  [1.37;4.56]   0.00279
3                    HT            0.40  [0.20;0.80]   0.00934
4 Chronic_liver_disease           11.81 [5.57;25.07]   < 0.001
5           cr_gradient            2.86  [1.58;5.17]   < 0.001
> multi_bestmem$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR   n n_event_pos
1 0.8694461   0.7704918    0.864486        0.1214668    0.5426357     0.2608696          210               18              192  204 917          61
  n_event_neg                                                                      glm_model   AUC_all ch_hr_lower ch_hr_upper aki_hr_lower
1         856 AKI_2or3 ~ PCs_cardio + Vasopressor + HT + Chronic_liver_disease + cr_gradient 0.8736977         4.5         6.1          9.3
  aki_hr_upper
1           40
```

```R
> publish(multi_bestmem_cr$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio         CI.95  p-value
1            PCs_cardio            1.10   [0.47;2.57]   0.8184
2           Vasopressor            2.85   [1.25;6.47]   0.0126
3                    HT            0.52   [0.19;1.41]   0.1975
4 Chronic_liver_disease           25.11 [10.90;57.82]   <0.001
5           cr_gradient            2.57   [1.18;5.59]   0.0173
> multi_bestmem_cr$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR   n n_event_pos
1 0.8848741   0.7027027   0.9443182       0.07899507            1             1          210               12              198  204 917          37
  n_event_neg                                                                                 glm_model   AUC_all ch_hr_lower ch_hr_upper
1         880 Cr_defined_AKI_2or3 ~ PCs_cardio + Vasopressor + HT + Chronic_liver_disease + cr_gradient 0.8848741         4.5         6.1
  aki_hr_lower aki_hr_upper
1          9.3           40
```

```R
> publish(multi_bestmem_olig$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95   p-value
1            PCs_cardio            4.22  [1.94;9.18]   < 0.001
2           Vasopressor            3.00  [1.43;6.29]   0.00358
3                    HT            0.54  [0.24;1.20]   0.12810
4 Chronic_liver_disease            5.04 [1.93;13.17]   < 0.001
5           cr_gradient            1.62  [0.77;3.43]   0.20278
> multi_bestmem_olig$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR   n n_event_pos
1 0.8395289   0.9166667   0.7150965       0.03789761            1             1          210               11              199  204 917          36
  n_event_neg                                                                                   glm_model   AUC_all ch_hr_lower ch_hr_upper
1         881 Olig_defined_AKI_2or3 ~ PCs_cardio + Vasopressor + HT + Chronic_liver_disease + cr_gradient 0.8395289         4.5         6.1
  aki_hr_lower aki_hr_upper
1          9.3           40
```
