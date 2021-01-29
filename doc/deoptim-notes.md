# DEOptim Notes

## Summary

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
    grepl("\\bAPACHE_II\\b", summary$glm_model) * 5 +
    grepl("\\bAPACHE_III\\b", summary$glm_model) * 5 +
    grepl("\\bBaseline_Cr\\b", summary$glm_model) +
    grepl("\\bcr\\b", summary$glm_model) * 3 +
    (1 - grepl("cr_gradient", summary$glm_model)) * 6 +
}
```

The following is for ABG only!!

Baseline model of all patients (n_admissions = 387). All explanatory variables included

```R
> publish(baseline_all$model, print = FALSE, digits = c(2, 3))$regressionTable
                Variable Units OddsRatio       CI.95  p-value
1                    Age            1.01 [0.98;1.03]   0.6594
2                   Male            0.84 [0.45;1.55]   0.5706
3              Mecvenadm            1.32 [0.56;3.11]   0.5196
4              APACHE_II            1.12 [1.02;1.23]   0.0197
5             APACHE_III            0.99 [0.97;1.02]   0.5154
6            Baseline_Cr            1.00 [0.99;1.01]   0.4860
7             PCs_cardio            2.31 [1.10;4.83]   0.0265
8            Vasopressor            1.72 [0.96;3.08]   0.0709
9               Diabetes            1.56 [0.82;2.98]   0.1783
10                    AF            1.69 [0.73;3.95]   0.2221
11                   IHD            1.55 [0.78;3.10]   0.2140
12                    HF            1.04 [0.30;3.69]   0.9460
13                    HT            1.01 [0.55;1.86]   0.9664
14                   PVD            1.04 [0.34;3.17]   0.9422
15 Chronic_liver_disease            1.87 [0.73;4.78]   0.1898
> baseline_all$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos
1 0.7230154   0.6029412   0.7799353         0.235965            1             1          377               68
  n_admissions_neg n_UR   n n_event_pos n_event_neg
1              309  367 377          68         309
                                                                                                                                                          glm_model
1 AKI_2or3 ~ Age + Male + Mecvenadm + APACHE_II + APACHE_III + Baseline_Cr + PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease
    AUC_all ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1 0.7230154        -Inf         Inf         -Inf          Inf
```

Baseline model with backwards stepwise regressions for AKI stage 2 or 3. Only impactful variable remain

```R
> publish(baseline_sig$model, print = FALSE, digits = c(2, 3))$regressionTable
     Variable Units OddsRatio       CI.95   p-value
1   APACHE_II            1.10 [1.05;1.16]   < 0.001
2  PCs_cardio            2.68 [1.46;4.92]   0.00153
3 Vasopressor            1.72 [0.98;3.01]   0.05944
> baseline_sig$summary
      AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos
1 0.68913   0.4558824   0.8511327        0.2587635            1             1          377               68
  n_admissions_neg n_UR   n n_event_pos n_event_neg                                       glm_model   AUC_all ch_hr_lower
1              309  367 377          68         309 AKI_2or3 ~ APACHE_II + PCs_cardio + Vasopressor 0.7230154        -Inf
  ch_hr_upper aki_hr_lower aki_hr_upper
1         Inf         -Inf          Inf
```

Baseline model with backwards stepwise regressions for AKI (any stage). Only impactful variables remain

```R
> publish(baseline_sig_AKI_ICU$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio       CI.95   p-value
1             Mecvenadm            1.65 [0.94;2.91]   0.08367
2            APACHE_III            1.01 [1.00;1.03]   0.06418
3           Baseline_Cr            1.01 [1.00;1.02]   0.16412
4            PCs_cardio            1.48 [0.87;2.52]   0.15141
5           Vasopressor            2.81 [1.70;4.66]   < 0.001
6              Diabetes            1.63 [0.92;2.89]   0.09480
7                    HT            1.60 [1.01;2.56]   0.04735
8 Chronic_liver_disease            3.51 [1.53;8.09]   0.00315
> baseline_sig_AKI_ICU$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos
1 0.7349842   0.7395349   0.6358025        0.5355923            1             1          377              215
  n_admissions_neg n_UR   n n_event_pos n_event_neg
1              162  367 377         215         162
                                                                                                          glm_model
1 AKI_ICU ~ Mecvenadm + APACHE_III + Baseline_Cr + PCs_cardio + Vasopressor + Diabetes + HT + Chronic_liver_disease
    AUC_all ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1 0.7384439        -Inf         Inf         -Inf          Inf
```

Only cr: From differential evolution optimisation, this is the best model for creatinine change of >1mg/L/hr (cr_gradient) for AKI stage 2 or 3

```R
> publish(cr_ch_bestmem$model, print = FALSE, digits = c(2, 3))$regressionTable
     Variable Units OddsRatio       CI.95 p-value
1 cr_gradient            2.50 [2.09;2.99]  <0.001

> cr_ch_bestmem$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR    n
1 0.6034536    0.464837   0.7420701        0.1654717    0.8630491     0.7246377          334               50              284  325 4177
  n_event_pos n_event_neg              glm_model   AUC_all ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1         583        3594 AKI_2or3 ~ cr_gradient 0.6034536         1.1         7.1            3         37.4
```

From differential evolution optimisation, this is the best multivariable model for AKI stage 2 or 3 chosen with backward selection.

```R
> publish(multi_bestmem$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95   p-value
1            PCs_cardio            3.30  [1.75;6.22]   < 0.001
2           Vasopressor            2.50  [1.37;4.56]   0.00279
3                    HT            0.40  [0.20;0.80]   0.00934
4 Chronic_liver_disease           11.81 [5.57;25.07]   < 0.001
5           cr_gradient            2.86  [1.58;5.17]   < 0.001
> multi_bestmem$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions
1 0.8694461   0.7704918    0.864486        0.1214668    0.5426357     0.2608696          210
  n_admissions_pos n_admissions_neg n_UR   n n_event_pos n_event_neg
1               18              192  204 917          61         856
                                                                       glm_model   AUC_all
1 AKI_2or3 ~ PCs_cardio + Vasopressor + HT + Chronic_liver_disease + cr_gradient 0.8736977
  ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1         4.5         6.1          9.3           40
```

This is the previous model, but before backwards stepwise regression

```R
> publish(multi_bestmem_all$model, print = FALSE, digits = c(2, 3))$regressionTable
                Variable Units OddsRatio        CI.95   p-value
1                    Age            0.99  [0.96;1.02]   0.58271
2                   Male            0.89  [0.41;1.93]   0.75992
3              Mecvenadm            0.55  [0.21;1.44]   0.22445
4              APACHE_II            0.89  [0.78;1.02]   0.08759
5             APACHE_III            1.04  [1.00;1.09]   0.03127
6            Baseline_Cr            0.96  [0.93;0.99]   0.01715
7             PCs_cardio            3.86  [1.65;9.06]   0.00191
8            Vasopressor            2.65  [1.33;5.28]   0.00541
9               Diabetes            2.41  [1.16;5.03]   0.01899
10                    AF            0.32  [0.08;1.33]   0.11701
11                   IHD            2.26  [1.01;5.05]   0.04764
12                    HF            0.00   [0.00;Inf]   0.98241
13                    HT            0.33  [0.15;0.74]   0.00676
14                   PVD            2.71 [0.60;12.34]   0.19688
15 Chronic_liver_disease           14.35 [6.32;32.56]   < 0.001
16                    cr            1.04  [1.01;1.07]   0.01172
17           cr_gradient            1.83  [0.95;3.54]   0.07267
> multi_bestmem_all$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions
1 0.8736977   0.8688525   0.7757009       0.05751193    0.5426357     0.2608696          210
  n_admissions_pos n_admissions_neg n_UR   n n_event_pos n_event_neg
1               18              192  204 917          61         856
                                                                                                                                                                             glm_model
1 AKI_2or3 ~ Age + Male + Mecvenadm + APACHE_II + APACHE_III + Baseline_Cr + PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease + cr + cr_gradient
    AUC_all ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1 0.8736977         4.5         6.1          9.3           40
```

Using the multivariable model, now try to predict AKI_ICU (any stage)

```R
> publish(multi_bestmem_aki$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95 p-value
1            PCs_cardio            1.84  [1.31;2.60]  <0.001
2           Vasopressor            2.41  [1.73;3.34]  <0.001
3                    HT            1.05  [0.75;1.48]   0.768
4 Chronic_liver_disease            9.83 [5.28;18.30]  <0.001
5           cr_gradient            2.11  [1.45;3.07]  <0.001
> multi_bestmem_aki$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions
1 0.7187298   0.7217391   0.5938865        0.2102887            1             1          210
  n_admissions_pos n_admissions_neg n_UR   n n_event_pos n_event_neg
1               79              131  204 917         230         687
                                                                      glm_model   AUC_all ch_hr_lower
1 AKI_ICU ~ PCs_cardio + Vasopressor + HT + Chronic_liver_disease + cr_gradient 0.7187298         4.5
  ch_hr_upper aki_hr_lower aki_hr_upper
1         6.1          9.3           40
```

Using the multivariable model, now try to predict creatinine defined AKI stage 2 or 3

```R
> publish(multi_bestmem_cr$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio         CI.95  p-value
1            PCs_cardio            1.10   [0.47;2.57]   0.8184
2           Vasopressor            2.85   [1.25;6.47]   0.0126
3                    HT            0.52   [0.19;1.41]   0.1975
4 Chronic_liver_disease           25.11 [10.90;57.82]   <0.001
5           cr_gradient            2.57   [1.18;5.59]   0.0173
> multi_bestmem_cr$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions
1 0.8848741   0.7027027   0.9443182       0.07899507            1             1          210
  n_admissions_pos n_admissions_neg n_UR   n n_event_pos n_event_neg
1               12              198  204 917          37         880
                                                                                  glm_model   AUC_all
1 Cr_defined_AKI_2or3 ~ PCs_cardio + Vasopressor + HT + Chronic_liver_disease + cr_gradient 0.8848741
  ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1         4.5         6.1          9.3           40
```

Using the multivariable model, predict creatinine defined AKI (any stage)

```R
> publish(multi_bestmem_cr_all$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95  p-value
1            PCs_cardio            0.61  [0.39;0.94]   0.0254
2           Vasopressor            2.06  [1.39;3.05]   <0.001
3                    HT            1.39  [0.92;2.11]   0.1176
4 Chronic_liver_disease            7.32 [4.03;13.29]   <0.001
5           cr_gradient            2.58  [1.67;3.97]   <0.001
> multi_bestmem_cr_all$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos
1 0.7184331   0.7246377   0.6508344        0.1398687            1             1          210               47
  n_admissions_neg n_UR   n n_event_pos n_event_neg
1              163  204 917         138         779
                                                                             glm_model   AUC_all ch_hr_lower
1 Cr_defined_AKI ~ PCs_cardio + Vasopressor + HT + Chronic_liver_disease + cr_gradient 0.7184331         4.5
  ch_hr_upper aki_hr_lower aki_hr_upper
1         6.1          9.3           40
```

Using the multivariable model, now try to predict oliguria defined AKI stage 2 or 3

```R
> publish(multi_bestmem_olig$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95   p-value
1            PCs_cardio            4.22  [1.94;9.18]   < 0.001
2           Vasopressor            3.00  [1.43;6.29]   0.00358
3                    HT            0.54  [0.24;1.20]   0.12810
4 Chronic_liver_disease            5.04 [1.93;13.17]   < 0.001
5           cr_gradient            1.62  [0.77;3.43]   0.20278
> multi_bestmem_olig$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions
1 0.8395289   0.9166667   0.7150965       0.03789761            1             1          210
  n_admissions_pos n_admissions_neg n_UR   n n_event_pos n_event_neg
1               11              199  204 917          36         881
                                                                                    glm_model   AUC_all
1 Olig_defined_AKI_2or3 ~ PCs_cardio + Vasopressor + HT + Chronic_liver_disease + cr_gradient 0.8395289
  ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1         4.5         6.1          9.3           40
```

Using the multivariable model, predict oliguria defined AKI (any stage)

```R
> publish(multi_bestmem_olig_all$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio       CI.95  p-value
1            PCs_cardio            3.08 [2.02;4.68]   <0.001
2           Vasopressor            2.83 [1.89;4.24]   <0.001
3                    HT            0.69 [0.45;1.05]   0.0858
4 Chronic_liver_disease            4.27 [2.18;8.36]   <0.001
5           cr_gradient            1.33 [0.84;2.09]   0.2202
> multi_bestmem_olig_all$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos
1 0.7233296   0.6171875   0.7401774        0.1463169            1             1          210               44
  n_admissions_neg n_UR   n n_event_pos n_event_neg
1              166  204 917         128         789
                                                                               glm_model   AUC_all ch_hr_lower
1 Olig_defined_AKI ~ PCs_cardio + Vasopressor + HT + Chronic_liver_disease + cr_gradient 0.7233296         4.5
  ch_hr_upper aki_hr_lower aki_hr_upper
1         6.1          9.3           40
```

## Penalise HT

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
    grepl("\\bAPACHE_II\\b", summary$glm_model) * 5 +
    grepl("\\bAPACHE_III\\b", summary$glm_model) * 5 +
    grepl("\\bBaseline_Cr\\b", summary$glm_model) +
    grepl("\\bcr\\b", summary$glm_model) * 3 +
    (1 - grepl("cr_gradient", summary$glm_model)) * 6 +
    grepl("\\bHT\\b", summary$glm_model)
}
```

```R
> multi_optim <- deoptim_wrapper(
+   lower = c(4, 0.5, 3, 1),
+   upper = c(6, 6, 12, 72),
+   itermax = 20,
+   outcome_var = "AKI_2or3",
+   baseli .... [TRUNCATED]
Iteration: 1 bestvalit: 5.035099 bestmemit:    5.400000    0.900000    8.700000   59.500000
Iteration: 2 bestvalit: 4.730220 bestmemit:    5.400000    0.900000    8.700000   51.800000
Iteration: 3 bestvalit: 4.730220 bestmemit:    5.400000    0.900000    8.700000   51.800000
Iteration: 4 bestvalit: 4.021813 bestmemit:    5.200000    1.300000   10.300000   24.900000
Iteration: 5 bestvalit: 4.021813 bestmemit:    5.200000    1.300000   10.300000   24.900000
Iteration: 6 bestvalit: 4.021813 bestmemit:    5.200000    1.300000   10.300000   24.900000
Iteration: 7 bestvalit: 4.021813 bestmemit:    5.200000    1.300000   10.300000   24.900000
Iteration: 8 bestvalit: 4.021813 bestmemit:    5.200000    1.300000   10.300000   24.900000
Iteration: 9 bestvalit: 4.020938 bestmemit:    5.200000    1.300000   10.300000   25.100000
Iteration: 10 bestvalit: 4.020938 bestmemit:    5.200000    1.300000   10.300000   25.100000
Iteration: 11 bestvalit: 4.020938 bestmemit:    5.200000    1.300000   10.300000   25.100000
Iteration: 12 bestvalit: 4.020938 bestmemit:    5.200000    1.300000   10.300000   25.100000
Iteration: 13 bestvalit: 4.020938 bestmemit:    5.200000    1.300000   10.300000   25.100000
Iteration: 14 bestvalit: 4.020938 bestmemit:    5.200000    1.300000   10.300000   25.100000
Iteration: 15 bestvalit: 4.020938 bestmemit:    5.200000    1.300000   10.300000   25.100000
Iteration: 16 bestvalit: 4.020938 bestmemit:    5.200000    1.300000   10.300000   25.100000
Iteration: 17 bestvalit: 4.020938 bestmemit:    5.200000    1.300000   10.300000   25.100000
Iteration: 18 bestvalit: 4.020938 bestmemit:    5.200000    1.300000   10.300000   25.100000
Iteration: 19 bestvalit: 4.020938 bestmemit:    5.200000    1.300000   10.300000   25.100000
Iteration: 20 bestvalit: 4.020938 bestmemit:    5.200000    1.300000   10.300000   25.100000

> if (FALSE) multi_optim <- list(result = list(optim = list(bestmem = c(5.3, 1.6, 9.3, 30.7))))

> multi_bestmem <- heuristic_wrapper(multi_optim$result$optim$bestmem,
+   outcome_var = "AKI_2or3",
+   baseline_predictors = c(
+     "Age + Male +  ..." ... [TRUNCATED]

> publish(multi_bestmem$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95   p-value
1            PCs_cardio            3.63  [1.70;7.74]   < 0.001
2 Chronic_liver_disease           16.06 [6.75;38.21]   < 0.001
3           cr_gradient            3.13  [1.55;6.31]   0.00149

> multi_bestmem$summary
       AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR   n n_event_pos
1 0.851733         0.7   0.8731563       0.09706783    0.5119363          0.25          193               17              176  188 718          40
  n_event_neg                                                   glm_model   AUC_all ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1         678 AKI_2or3 ~ PCs_cardio + Chronic_liver_disease + cr_gradient 0.8835914        4.55        5.85         10.3         35.4

> multi_bestmem_all <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "AKI_2or3",
+   baseline_predictors = c(
+     "Age  ..." ... [TRUNCATED]

> publish(multi_bestmem_all$model, print = FALSE, digits = c(2, 3))$regressionTable
                Variable Units OddsRatio        CI.95   p-value
1                    Age            1.00  [0.97;1.04]   0.82840
2                   Male            0.59  [0.23;1.53]   0.28218
3              Mecvenadm            0.46  [0.14;1.55]   0.20860
4              APACHE_II            0.92  [0.77;1.10]   0.37130
5             APACHE_III            1.04  [0.98;1.09]   0.19360
6            Baseline_Cr            0.96  [0.92;1.00]   0.03161
7             PCs_cardio            5.05 [1.70;15.04]   0.00359
8            Vasopressor            1.82  [0.78;4.24]   0.16467
9               Diabetes            2.84  [1.15;7.04]   0.02410
10                    AF            0.13  [0.01;1.31]   0.08363
11                   IHD            2.90  [1.10;7.60]   0.03092
12                    HF            0.00   [0.00;Inf]   0.98914
13                    HT            0.28  [0.10;0.75]   0.01146
14                   PVD            0.74  [0.06;8.51]   0.80637
15 Chronic_liver_disease           19.00 [6.88;52.47]   < 0.001
16                    cr            1.05  [1.01;1.08]   0.01026
17           cr_gradient            1.99  [0.89;4.43]   0.09267

> multi_bestmem_all$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR   n n_event_pos
1 0.8835914       0.875     0.79941       0.04409193    0.5119363          0.25          193               17              176  188 718          40
  n_event_neg
1         678
                                                                                                                                                                             glm_model
1 AKI_2or3 ~ Age + Male + Mecvenadm + APACHE_II + APACHE_III + Baseline_Cr + PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease + cr + cr_gradient
    AUC_all ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1 0.8835914        4.55        5.85         10.3         35.4

> # FIXME: n_admission for these below are based on multi_bestmem$data, not the real analysis_data...
>
> multi_bestmem_aki <- heuristic_wrapper(
+   .... [TRUNCATED]

> publish(multi_bestmem_aki$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95  p-value
1            PCs_cardio            1.59  [1.07;2.36]   0.0226
2 Chronic_liver_disease            8.66 [4.30;17.45]   <0.001
3           cr_gradient            2.59  [1.68;3.98]   <0.001

> multi_bestmem_aki$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR   n n_event_pos
1 0.6788869   0.4671053   0.8286219        0.2372456            1             1          193               70              123  188 718         152
  n_event_neg                                                  glm_model   AUC_all ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1         566 AKI_ICU ~ PCs_cardio + Chronic_liver_disease + cr_gradient 0.6788869        4.55        5.85         10.3         35.4

> multi_bestmem_cr <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "Cr_defined_AKI_2or3",
+   baseline_predictors = gsub .... [TRUNCATED]

> publish(multi_bestmem_cr$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio         CI.95  p-value
1            PCs_cardio            1.40   [0.53;3.71]   0.4950
2 Chronic_liver_disease           29.92 [11.66;76.78]   <0.001
3           cr_gradient            3.19   [1.25;8.13]   0.0149

> multi_bestmem_cr$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR   n n_event_pos
1 0.8741595   0.8333333   0.8645533       0.03709903            1             1          193               11              182  188 718          24
  n_event_neg                                                              glm_model   AUC_all ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1         694 Cr_defined_AKI_2or3 ~ PCs_cardio + Chronic_liver_disease + cr_gradient 0.8741595        4.55        5.85         10.3         35.4

> multi_bestmem_cr_all <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "Cr_defined_AKI",
+   baseline_predictors = gsub( .... [TRUNCATED]

> publish(multi_bestmem_cr_all$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95  p-value
1            PCs_cardio            0.55  [0.33;0.94]   0.0273
2 Chronic_liver_disease            5.31 [2.63;10.72]   <0.001
3           cr_gradient            3.49  [2.10;5.81]   <0.001

> multi_bestmem_cr_all$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR   n n_event_pos
1 0.6946387   0.5164835   0.8070175         0.134318            1             1          193               43              150  188 718          91
  n_event_neg                                                         glm_model   AUC_all ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1         627 Cr_defined_AKI ~ PCs_cardio + Chronic_liver_disease + cr_gradient 0.6946387        4.55        5.85         10.3         35.4

> multi_bestmem_olig <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "Olig_defined_AKI_2or3",
+   baseline_predictors =  .... [TRUNCATED]

> publish(multi_bestmem_olig$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95 p-value
1            PCs_cardio            5.42 [2.06;14.22]  <0.001
2 Chronic_liver_disease            9.69 [3.30;28.44]  <0.001
3           cr_gradient            1.47  [0.59;3.69]    0.41

> multi_bestmem_olig$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR   n n_event_pos
1 0.8356272   0.9565217    0.628777       0.03013223            1             1          193               10              183  188 718          23
  n_event_neg                                                                glm_model   AUC_all ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1         695 Olig_defined_AKI_2or3 ~ PCs_cardio + Chronic_liver_disease + cr_gradient 0.8356272        4.55        5.85         10.3         35.4

> multi_bestmem_olig_all <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "Olig_defined_AKI",
+   baseline_predictors = g .... [TRUNCATED]

> publish(multi_bestmem_olig_all$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95 p-value
1            PCs_cardio            2.58  [1.59;4.21]  <0.001
2 Chronic_liver_disease            6.21 [3.03;12.72]  <0.001
3           cr_gradient            1.31  [0.76;2.25]   0.327

> multi_bestmem_olig_all$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR   n n_event_pos
1 0.6641948   0.7176471   0.5687204       0.07555805            1             1          193               38              155  188 718          85
  n_event_neg                                                           glm_model   AUC_all ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1         633 Olig_defined_AKI ~ PCs_cardio + Chronic_liver_disease + cr_gradient 0.6641948        4.55        5.85         10.3         35.4

> multi_bestmem_baseline <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "AKI_2or3",
+   baseline_predictors = gsub(".*~ ..." ... [TRUNCATED]

> publish(multi_bestmem_baseline$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95 p-value
1            PCs_cardio            4.37  [2.09;9.10]  <0.001
2 Chronic_liver_disease           17.05 [7.29;39.90]  <0.001

> multi_bestmem_baseline$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR   n n_event_pos
1 0.8209624       0.925   0.6415929       0.04842253            1             1          193               17              176  188 718          40
  n_event_neg                                     glm_model   AUC_all ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1         678 AKI_2or3 ~ PCs_cardio + Chronic_liver_disease 0.8209624        4.55        5.85         10.3         35.4
```


## No HT Penalty, increase NP = 320 and itermax = 200

```R
> # ---- multi ----
> set.seed(8)

> multi_optim <- deoptim_wrapper(
+   lower = c(4, 0.5, 3, 1),
+   upper = c(6, 6, 12, 72),
+   itermax = 200,
+   outcome_var = "AKI_2or3",
+   basel .... [TRUNCATED]
Iteration: 1 bestvalit: 4.166777 bestmemit:    5.200000    0.900000    8.700000   45.600000
Iteration: 2 bestvalit: 3.898837 bestmemit:    5.100000    1.100000    8.600000   43.200000
Iteration: 3 bestvalit: 3.898837 bestmemit:    5.100000    1.100000    8.600000   43.200000
Iteration: 4 bestvalit: 3.898837 bestmemit:    5.100000    1.100000    8.600000   43.200000
Iteration: 5 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 6 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 7 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 8 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 9 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 10 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 11 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 12 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 13 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 14 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 15 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 16 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 17 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 18 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 19 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 20 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 21 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 22 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 23 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 24 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 25 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 26 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 27 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 28 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 29 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 30 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 31 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 32 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 33 bestvalit: 3.740370 bestmemit:    4.900000    1.500000    8.800000   17.800000
Iteration: 34 bestvalit: 3.724045 bestmemit:    4.900000    1.500000    8.600000   17.400000
Iteration: 35 bestvalit: 3.724045 bestmemit:    4.900000    1.500000    8.600000   17.400000
Iteration: 36 bestvalit: 3.724045 bestmemit:    4.900000    1.500000    8.600000   17.400000
Iteration: 37 bestvalit: 3.724045 bestmemit:    4.900000    1.500000    8.600000   17.400000
Iteration: 38 bestvalit: 3.724045 bestmemit:    4.900000    1.500000    8.600000   17.400000
Iteration: 39 bestvalit: 3.724045 bestmemit:    4.900000    1.500000    8.600000   17.400000
Iteration: 40 bestvalit: 3.724045 bestmemit:    4.900000    1.500000    8.600000   17.400000
Iteration: 41 bestvalit: 3.724045 bestmemit:    4.900000    1.500000    8.600000   17.400000
Iteration: 42 bestvalit: 3.724045 bestmemit:    4.900000    1.500000    8.600000   17.400000
Iteration: 43 bestvalit: 3.724045 bestmemit:    4.900000    1.500000    8.600000   17.400000
Iteration: 44 bestvalit: 3.724045 bestmemit:    4.900000    1.500000    8.600000   17.400000
Iteration: 45 bestvalit: 3.724045 bestmemit:    4.900000    1.500000    8.600000   17.400000
Iteration: 46 bestvalit: 3.714389 bestmemit:    5.300000    1.600000    8.700000   36.700000
Iteration: 47 bestvalit: 3.714389 bestmemit:    5.300000    1.600000    8.700000   36.700000
Iteration: 48 bestvalit: 3.714389 bestmemit:    5.300000    1.600000    8.700000   36.700000
Iteration: 49 bestvalit: 3.714389 bestmemit:    5.300000    1.600000    8.700000   36.700000
Iteration: 50 bestvalit: 3.714389 bestmemit:    5.300000    1.600000    8.700000   36.700000
Iteration: 51 bestvalit: 3.679485 bestmemit:    4.900000    1.500000    8.600000   18.100000
Iteration: 52 bestvalit: 3.679485 bestmemit:    4.900000    1.500000    8.600000   18.100000
Iteration: 53 bestvalit: 3.679485 bestmemit:    4.900000    1.500000    8.600000   18.100000
Iteration: 54 bestvalit: 3.679485 bestmemit:    4.900000    1.500000    8.600000   18.100000
Iteration: 55 bestvalit: 3.679485 bestmemit:    4.900000    1.500000    8.600000   18.100000
Iteration: 56 bestvalit: 3.679485 bestmemit:    4.900000    1.500000    8.600000   18.100000
Iteration: 57 bestvalit: 3.679485 bestmemit:    4.900000    1.500000    8.600000   18.100000
Iteration: 58 bestvalit: 3.679485 bestmemit:    4.900000    1.500000    8.600000   18.100000
Iteration: 59 bestvalit: 3.679326 bestmemit:    4.900000    1.500000    8.700000   18.900000
Iteration: 60 bestvalit: 3.666623 bestmemit:    4.900000    1.500000    8.500000   18.900000
Iteration: 61 bestvalit: 3.666623 bestmemit:    4.900000    1.500000    8.500000   18.900000
Iteration: 62 bestvalit: 3.666623 bestmemit:    4.900000    1.500000    8.500000   18.900000
Iteration: 63 bestvalit: 3.666623 bestmemit:    4.900000    1.500000    8.500000   18.900000
Iteration: 64 bestvalit: 3.666623 bestmemit:    4.900000    1.500000    8.500000   18.900000
Iteration: 65 bestvalit: 3.666623 bestmemit:    4.900000    1.500000    8.500000   18.900000
Iteration: 66 bestvalit: 3.666623 bestmemit:    4.900000    1.500000    8.500000   18.900000
Iteration: 67 bestvalit: 3.666623 bestmemit:    4.900000    1.500000    8.500000   18.900000
Iteration: 68 bestvalit: 3.666623 bestmemit:    4.900000    1.500000    8.500000   18.900000
Iteration: 69 bestvalit: 3.666623 bestmemit:    4.900000    1.500000    8.500000   18.900000
Iteration: 70 bestvalit: 3.666623 bestmemit:    4.900000    1.500000    8.500000   18.900000
Iteration: 71 bestvalit: 3.666623 bestmemit:    4.900000    1.500000    8.500000   18.900000
Iteration: 72 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 73 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 74 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 75 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 76 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 77 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 78 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 79 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 80 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 81 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 82 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 83 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 84 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 85 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 86 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 87 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 88 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 89 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 90 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 91 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 92 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 93 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 94 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 95 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 96 bestvalit: 3.660926 bestmemit:    4.900000    1.500000    8.600000   19.100000
Iteration: 97 bestvalit: 3.613363 bestmemit:    5.100000    1.800000    8.800000   17.900000
Iteration: 98 bestvalit: 3.613363 bestmemit:    5.100000    1.800000    8.800000   17.900000
Iteration: 99 bestvalit: 3.613363 bestmemit:    5.100000    1.800000    8.800000   17.900000
Iteration: 100 bestvalit: 3.613363 bestmemit:    5.100000    1.800000    8.800000   17.900000
Iteration: 101 bestvalit: 3.613363 bestmemit:    5.100000    1.800000    8.800000   17.900000
Iteration: 102 bestvalit: 3.613363 bestmemit:    5.100000    1.800000    8.800000   17.900000
Iteration: 103 bestvalit: 3.613363 bestmemit:    5.100000    1.800000    8.800000   17.900000
Iteration: 104 bestvalit: 3.613363 bestmemit:    5.100000    1.800000    8.800000   17.900000
Iteration: 105 bestvalit: 3.613363 bestmemit:    5.100000    1.800000    8.800000   17.900000
Iteration: 106 bestvalit: 3.613363 bestmemit:    5.100000    1.800000    8.800000   17.900000
Iteration: 107 bestvalit: 3.613363 bestmemit:    5.100000    1.800000    8.800000   17.900000
Iteration: 108 bestvalit: 3.613363 bestmemit:    5.100000    1.800000    8.800000   17.900000
Iteration: 109 bestvalit: 3.613363 bestmemit:    5.100000    1.800000    8.800000   17.900000
Iteration: 110 bestvalit: 3.613363 bestmemit:    5.100000    1.800000    8.800000   17.900000
Iteration: 111 bestvalit: 3.613363 bestmemit:    5.100000    1.800000    8.800000   17.900000
Iteration: 112 bestvalit: 3.613363 bestmemit:    5.100000    1.800000    8.800000   17.900000
Iteration: 113 bestvalit: 3.613363 bestmemit:    5.100000    1.800000    8.800000   17.900000
Iteration: 114 bestvalit: 3.613363 bestmemit:    5.100000    1.800000    8.800000   17.900000
Iteration: 115 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 116 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 117 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 118 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 119 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 120 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 121 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 122 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 123 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 124 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 125 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 126 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 127 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 128 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 129 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 130 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 131 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 132 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 133 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 134 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 135 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 136 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 137 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 138 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 139 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 140 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 141 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 142 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 143 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 144 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 145 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 146 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 147 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 148 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 149 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 150 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 151 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 152 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 153 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 154 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 155 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 156 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 157 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 158 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 159 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 160 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 161 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 162 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 163 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 164 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 165 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 166 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 167 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 168 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 169 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 170 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 171 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 172 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 173 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 174 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 175 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 176 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 177 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 178 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 179 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 180 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 181 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 182 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 183 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 184 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 185 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 186 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 187 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 188 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 189 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 190 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 191 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 192 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 193 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 194 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 195 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 196 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 197 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 198 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 199 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000
Iteration: 200 bestvalit: 3.540947 bestmemit:    5.000000    2.000000    9.500000   17.400000

> if (FALSE) multi_optim <- list(result = list(optim = list(bestmem = c(5.3, 1.6, 9.3, 30.7))))

> multi_bestmem <- heuristic_wrapper(multi_optim$result$optim$bestmem,
+   outcome_var = "AKI_2or3",
+   baseline_predictors = c(
+     "Age + Male +  ..." ... [TRUNCATED]

> publish(multi_bestmem$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio         CI.95  p-value
1             Mecvenadm            0.36   [0.17;0.79]   0.0107
2            PCs_cardio            7.85  [3.88;15.88]   <0.001
3           Vasopressor            3.45   [1.91;6.25]   <0.001
4                    HT            0.23   [0.12;0.46]   <0.001
5 Chronic_liver_disease           29.48 [13.04;66.66]   <0.001
6           cr_gradient            2.78   [1.59;4.85]   <0.001

> multi_bestmem$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR    n
1 0.8891653   0.8026316   0.8618114        0.1274696     0.596817     0.3235294          225               22              203  219 1147
  n_event_pos n_event_neg                                                                                  glm_model   AUC_all ch_hr_lower
1          76        1071 AKI_2or3 ~ Mecvenadm + PCs_cardio + Vasopressor + HT + Chronic_liver_disease + cr_gradient 0.8997862           4
  ch_hr_upper aki_hr_lower aki_hr_upper
1           6          9.5         26.9

> multi_bestmem_all <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "AKI_2or3",
+   baseline_predictors = c(
+     "Age  ..." ... [TRUNCATED]

> publish(multi_bestmem_all$model, print = FALSE, digits = c(2, 3))$regressionTable
                Variable Units OddsRatio         CI.95   p-value
1                    Age            0.99   [0.97;1.02]   0.68040
2                   Male            0.70   [0.33;1.48]   0.35178
3              Mecvenadm            0.35   [0.14;0.87]   0.02466
4              APACHE_II            0.95   [0.84;1.07]   0.38098
5             APACHE_III            1.03   [0.99;1.07]   0.10664
6            Baseline_Cr            0.97   [0.95;1.00]   0.03703
7             PCs_cardio           10.38  [4.48;24.09]   < 0.001
8            Vasopressor            3.10   [1.61;5.96]   < 0.001
9               Diabetes            2.38   [1.19;4.76]   0.01397
10                    AF            0.69   [0.23;2.08]   0.51407
11                   IHD            1.53   [0.72;3.22]   0.26624
12                    HF            2.13   [0.53;8.56]   0.28566
13                    HT            0.21   [0.10;0.45]   < 0.001
14                   PVD            1.17   [0.23;5.96]   0.84961
15 Chronic_liver_disease           36.62 [15.17;88.37]   < 0.001
16                    cr            1.04   [1.01;1.06]   0.00555
17           cr_gradient            1.92   [1.04;3.56]   0.03789

> multi_bestmem_all$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR    n
1 0.8997862   0.8289474   0.8459384       0.08146556     0.596817     0.3235294          225               22              203  219 1147
  n_event_pos n_event_neg
1          76        1071
                                                                                                                                                                             glm_model
1 AKI_2or3 ~ Age + Male + Mecvenadm + APACHE_II + APACHE_III + Baseline_Cr + PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease + cr + cr_gradient
    AUC_all ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1 0.8997862           4           6          9.5         26.9

> # FIXME: n_admission for these below are based on multi_bestmem$data, not the real analysis_data...
>
> multi_bestmem_aki <- heuristic_wrapper(
+   .... [TRUNCATED]

> publish(multi_bestmem_aki$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95   p-value
1             Mecvenadm            0.48  [0.31;0.75]   0.00111
2            PCs_cardio            2.59  [1.83;3.67]   < 0.001
3           Vasopressor            2.92  [2.11;4.05]   < 0.001
4                    HT            0.96  [0.69;1.33]   0.81239
5 Chronic_liver_disease           13.99 [7.60;25.74]   < 0.001
6           cr_gradient            2.24  [1.57;3.18]   < 0.001

> multi_bestmem_aki$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR    n
1 0.7449008   0.6083333   0.7420066        0.1966128            1             1          225               86              139  219 1147
  n_event_pos n_event_neg                                                                                 glm_model   AUC_all ch_hr_lower
1         240         907 AKI_ICU ~ Mecvenadm + PCs_cardio + Vasopressor + HT + Chronic_liver_disease + cr_gradient 0.7449008           4
  ch_hr_upper aki_hr_lower aki_hr_upper
1           6          9.5         26.9

> multi_bestmem_cr <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "Cr_defined_AKI_2or3",
+   baseline_predictors = gsub .... [TRUNCATED]

> publish(multi_bestmem_cr$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio          CI.95   p-value
1             Mecvenadm            0.44    [0.18;1.09]   0.07669
2            PCs_cardio            3.61    [1.60;8.10]   0.00190
3           Vasopressor            3.61    [1.70;7.65]   < 0.001
4                    HT            0.31    [0.13;0.71]   0.00588
5 Chronic_liver_disease           42.82 [18.25;100.45]   < 0.001
6           cr_gradient            3.41    [1.74;6.69]   < 0.001

> multi_bestmem_cr$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR    n
1 0.9040657   0.8461538   0.8383562       0.06062809            1             1          225               15              210  219 1147
  n_event_pos n_event_neg                                                                                             glm_model   AUC_all
1          52        1095 Cr_defined_AKI_2or3 ~ Mecvenadm + PCs_cardio + Vasopressor + HT + Chronic_liver_disease + cr_gradient 0.9040657
  ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1           4           6          9.5         26.9

> multi_bestmem_cr_all <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "Cr_defined_AKI",
+   baseline_predictors = gsub( .... [TRUNCATED]

> publish(multi_bestmem_cr_all$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95 p-value
1             Mecvenadm            0.54  [0.33;0.90]   0.018
2            PCs_cardio            1.22  [0.79;1.88]   0.369
3           Vasopressor            2.43  [1.62;3.64]  <0.001
4                    HT            1.19  [0.79;1.78]   0.399
5 Chronic_liver_disease           15.10 [8.23;27.72]  <0.001
6           cr_gradient            2.89  [1.91;4.37]  <0.001

> multi_bestmem_cr_all$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR    n
1 0.7642193   0.7622378   0.6424303        0.1088277            1             1          225               52              173  219 1147
  n_event_pos n_event_neg                                                                                        glm_model   AUC_all ch_hr_lower
1         143        1004 Cr_defined_AKI ~ Mecvenadm + PCs_cardio + Vasopressor + HT + Chronic_liver_disease + cr_gradient 0.7642193           4
  ch_hr_upper aki_hr_lower aki_hr_upper
1           6          9.5         26.9

> multi_bestmem_olig <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "Olig_defined_AKI_2or3",
+   baseline_predictors =  .... [TRUNCATED]

> publish(multi_bestmem_olig$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95   p-value
1             Mecvenadm            0.77  [0.24;2.47]   0.65590
2            PCs_cardio            5.54 [2.43;12.65]   < 0.001
3           Vasopressor            3.21  [1.52;6.77]   0.00222
4                    HT            0.31  [0.13;0.73]   0.00798
5 Chronic_liver_disease           11.70 [4.55;30.08]   < 0.001
6           cr_gradient            1.06  [0.48;2.35]   0.87997

> multi_bestmem_olig$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR    n
1 0.8639244   0.9210526   0.7376014       0.03868438            1             1          225               13              212  219 1147
  n_event_pos n_event_neg                                                                                               glm_model   AUC_all
1          38        1109 Olig_defined_AKI_2or3 ~ Mecvenadm + PCs_cardio + Vasopressor + HT + Chronic_liver_disease + cr_gradient 0.8639244
  ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1           4           6          9.5         26.9

> multi_bestmem_olig_all <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "Olig_defined_AKI",
+   baseline_predictors = g .... [TRUNCATED]

> publish(multi_bestmem_olig_all$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio       CI.95  p-value
1             Mecvenadm            0.46 [0.27;0.78]   0.0040
2            PCs_cardio            3.11 [2.04;4.74]   <0.001
3           Vasopressor            3.16 [2.12;4.70]   <0.001
4                    HT            0.70 [0.47;1.04]   0.0803
5 Chronic_liver_disease            4.38 [2.23;8.58]   <0.001
6           cr_gradient            1.19 [0.77;1.85]   0.4408

> multi_bestmem_olig_all$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR    n
1 0.7170766    0.762963   0.5978261       0.09472103            1             1          225               47              178  219 1147
  n_event_pos n_event_neg                                                                                          glm_model   AUC_all ch_hr_lower
1         135        1012 Olig_defined_AKI ~ Mecvenadm + PCs_cardio + Vasopressor + HT + Chronic_liver_disease + cr_gradient 0.7170766           4
  ch_hr_upper aki_hr_lower aki_hr_upper
1           6          9.5         26.9

> multi_bestmem_baseline <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "AKI_2or3",
+   baseline_predictors = gsub(".*~ ..." ... [TRUNCATED]

> publish(multi_bestmem_baseline$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio         CI.95   p-value
1             Mecvenadm            0.34   [0.16;0.72]   0.00519
2            PCs_cardio            9.36  [4.67;18.76]   < 0.001
3           Vasopressor            3.51   [1.96;6.29]   < 0.001
4                    HT            0.26   [0.14;0.50]   < 0.001
5 Chronic_liver_disease           32.08 [14.26;72.13]   < 0.001

> multi_bestmem_baseline$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR    n
1 0.8746867   0.8947368   0.7394958       0.07113579            1             1          225               22              203  219 1147
  n_event_pos n_event_neg                                                                    glm_model   AUC_all ch_hr_lower ch_hr_upper
1          76        1071 AKI_2or3 ~ Mecvenadm + PCs_cardio + Vasopressor + HT + Chronic_liver_disease 0.8746867           4           6
  aki_hr_lower aki_hr_upper
1          9.5         26.9
```

## Remove HT all together

```R
multi_baseline_predictors <- c(
  # "Age + Male + Mecvenadm + APACHE_II + APACHE_III + Baseline_Cr",
  # "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + HT + PVD + Chronic_liver_disease"
  "Age + Male + APACHE_II + APACHE_III + Baseline_Cr",
  "PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + PVD + Chronic_liver_disease"
)
```

```R
Iteration: 1 bestvalit: 4.280794 bestmemit:    5.600000    2.000000   10.100000    6.900000
Iteration: 2 bestvalit: 4.212516 bestmemit:    5.100000    1.300000   10.800000   14.200000
Iteration: 3 bestvalit: 4.212516 bestmemit:    5.100000    1.300000   10.800000   14.200000
Iteration: 4 bestvalit: 4.052578 bestmemit:    5.200000    1.100000   10.100000   27.200000
Iteration: 5 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 6 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 7 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 8 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 9 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 10 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 11 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 12 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 13 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 14 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 15 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 16 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 17 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 18 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 19 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 20 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
>
> if (FALSE) multi_optim <- list(result = list(optim = list(bestmem = c(5.3, 1.6, 9.3, 30.7))))
>
> multi_bestmem <- heuristic_wrapper(multi_optim$result$optim$bestmem,
+   outcome_var = "AKI_2or3",
+   baseline_predictors = multi_baseline_predictors,
+   cr_predictors = "cr",
+   add_gradient_predictor = 1,
+   stepwise = TRUE,
+   k = "mBIC",
+   all_data = TRUE
+ )
> publish(multi_bestmem$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio         CI.95   p-value
1            PCs_cardio            3.21   [1.68;6.13]   < 0.001
2           Vasopressor            3.54   [1.90;6.62]   < 0.001
3 Chronic_liver_disease           25.97 [11.22;60.14]   < 0.001
4           cr_gradient            2.43   [1.34;4.38]   0.00328
> multi_bestmem$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR   n n_event_pos
1 0.8526658    0.796875   0.8388017        0.1195089    0.5490716     0.3382353          207               23              184  201 765          64
  n_event_neg                                                                 glm_model   AUC_all ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1         701 AKI_2or3 ~ PCs_cardio + Vasopressor + Chronic_liver_disease + cr_gradient 0.8700517         4.5         5.7          7.2         53.8
>
> multi_bestmem_all <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "AKI_2or3",
+   baseline_predictors = multi_baseline_predictors,
+   cr_predictors = "cr",
+   add_gradient_predictor = 1,
+
+   all_data = TRUE
+ )
> publish(multi_bestmem_all$model, print = FALSE, digits = c(2, 3))$regressionTable
                Variable Units OddsRatio         CI.95   p-value
1                    Age            1.01   [0.98;1.04]   0.39271
2                   Male            0.64   [0.32;1.29]   0.21438
3              APACHE_II            1.06   [0.94;1.20]   0.33796
4             APACHE_III            0.98   [0.95;1.02]   0.30217
5            Baseline_Cr            0.95   [0.92;0.98]   0.00259
6             PCs_cardio            2.99   [1.24;7.25]   0.01515
7            Vasopressor            3.13   [1.57;6.22]   0.00116
8               Diabetes            1.48   [0.71;3.08]   0.28993
9                     AF            0.80   [0.23;2.74]   0.71906
10                   IHD            1.82   [0.85;3.90]   0.12245
11                    HF            0.00    [0.00;Inf]   0.98201
12                   PVD            0.46   [0.07;3.13]   0.42865
13 Chronic_liver_disease           28.27 [11.87;67.37]   < 0.001
14                    cr            1.04   [1.01;1.07]   0.01368
15           cr_gradient            1.74   [0.91;3.35]   0.09440
> multi_bestmem_all$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR   n n_event_pos
1 0.8700517     0.84375   0.8345221        0.1045834    0.5490716     0.3382353          207               23              184  201 765          64
  n_event_neg
1         701
                                                                                                                                                            glm_model
1 AKI_2or3 ~ Age + Male + APACHE_II + APACHE_III + Baseline_Cr + PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + PVD + Chronic_liver_disease + cr + cr_gradient
    AUC_all ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1 0.8700517         4.5         5.7          7.2         53.8
>
> # FIXME: n_admission for these below are based on multi_bestmem$data, not the real analysis_data...
>
> multi_bestmem_aki <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "AKI_ICU",
+   baseline_predictors = gsub(".*~ ", "", multi_bestmem$summary$glm_model),
+   all_data = TRUE,
+   analysis_data = multi_bestmem$data
+ )
> publish(multi_bestmem_aki$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95   p-value
1            PCs_cardio            1.71  [1.21;2.41]   0.00235
2           Vasopressor            2.62  [1.87;3.66]   < 0.001
3 Chronic_liver_disease           10.18 [4.96;20.89]   < 0.001
4           cr_gradient            1.99  [1.34;2.94]   < 0.001
> multi_bestmem_aki$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR   n n_event_pos
1 0.7117344   0.7058824   0.6053131        0.2886075            1             1          207               84              123  201 765         238
  n_event_neg                                                                glm_model   AUC_all ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1         527 AKI_ICU ~ PCs_cardio + Vasopressor + Chronic_liver_disease + cr_gradient 0.7117344         4.5         5.7          7.2         53.8
>
> multi_bestmem_cr <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "Cr_defined_AKI_2or3",
+   baseline_predictors = gsub(".*~ ", "", multi_bestmem$summary$glm_model),
+   all_data = TRUE,
+   analysis_data = multi_bestmem$data
+ )
> publish(multi_bestmem_cr$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio         CI.95  p-value
1            PCs_cardio            2.04   [0.97;4.26]   0.0594
2           Vasopressor            4.24   [1.97;9.12]   <0.001
3 Chronic_liver_disease           37.52 [15.47;90.98]   <0.001
4           cr_gradient            2.25   [1.13;4.48]   0.0217
> multi_bestmem_cr$summary
       AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR   n n_event_pos
1 0.876046   0.8541667   0.7921897       0.06146473            1             1          207               17              190  201 765          48
  n_event_neg                                                                            glm_model  AUC_all ch_hr_lower ch_hr_upper aki_hr_lower
1         717 Cr_defined_AKI_2or3 ~ PCs_cardio + Vasopressor + Chronic_liver_disease + cr_gradient 0.876046         4.5         5.7          7.2
  aki_hr_upper
1         53.8
>
> multi_bestmem_cr_all <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "Cr_defined_AKI",
+   baseline_predictors = gsub(".*~ ", "", multi_bestmem$summary$glm_model),
+   all_data = TRUE,
+   analysis_data = multi_bestmem$data
+ )
> publish(multi_bestmem_cr_all$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95 p-value
1            PCs_cardio            0.78  [0.52;1.17]   0.224
2           Vasopressor            2.95  [2.01;4.32]  <0.001
3 Chronic_liver_disease            6.72 [3.50;12.89]  <0.001
4           cr_gradient            2.31  [1.51;3.55]  <0.001
> multi_bestmem_cr_all$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR   n n_event_pos
1 0.7154711   0.8113208   0.5445545        0.1440418            1             1          207               53              154  201 765         159
  n_event_neg                                                                       glm_model   AUC_all ch_hr_lower ch_hr_upper aki_hr_lower
1         606 Cr_defined_AKI ~ PCs_cardio + Vasopressor + Chronic_liver_disease + cr_gradient 0.7154711         4.5         5.7          7.2
  aki_hr_upper
1         53.8
>
> multi_bestmem_olig <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "Olig_defined_AKI_2or3",
+   baseline_predictors = gsub(".*~ ", "", multi_bestmem$summary$glm_model),
+   all_data = TRUE,
+   analysis_data = multi_bestmem$data
+ )
> publish(multi_bestmem_olig$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95  p-value
1            PCs_cardio            2.59  [1.11;6.03]   0.0280
2           Vasopressor            2.42  [1.07;5.49]   0.0346
3 Chronic_liver_disease           10.87 [4.10;28.77]   <0.001
4           cr_gradient            1.51  [0.65;3.50]   0.3405
> multi_bestmem_olig$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR   n n_event_pos
1 0.7894456   0.7142857   0.8046133       0.04893354            1             1          207               12              195  201 765          28
  n_event_neg                                                                              glm_model   AUC_all ch_hr_lower ch_hr_upper aki_hr_lower
1         737 Olig_defined_AKI_2or3 ~ PCs_cardio + Vasopressor + Chronic_liver_disease + cr_gradient 0.7894456         4.5         5.7          7.2
  aki_hr_upper
1         53.8
>
> multi_bestmem_olig_all <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "Olig_defined_AKI",
+   baseline_predictors = gsub(".*~ ", "", multi_bestmem$summary$glm_model),
+   all_data = TRUE,
+   analysis_data = multi_bestmem$data
+ )
> publish(multi_bestmem_olig_all$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio       CI.95   p-value
1            PCs_cardio            2.40 [1.56;3.69]   < 0.001
2           Vasopressor            1.98 [1.31;3.00]   0.00132
3 Chronic_liver_disease            4.84 [2.41;9.71]   < 0.001
4           cr_gradient            1.17 [0.72;1.88]   0.52795
> multi_bestmem_olig_all$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR   n n_event_pos
1 0.6689431   0.6695652   0.5861538        0.1388292            1             1          207               44              163  201 765         115
  n_event_neg                                                                         glm_model   AUC_all ch_hr_lower ch_hr_upper aki_hr_lower
1         650 Olig_defined_AKI ~ PCs_cardio + Vasopressor + Chronic_liver_disease + cr_gradient 0.6689431         4.5         5.7          7.2
  aki_hr_upper
1         53.8
>
> multi_bestmem_baseline <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "AKI_2or3",
+   baseline_predictors = gsub(".*~ | \\+ cr_gradient", "", multi_bestmem$summary$glm_model),
+   all_data = TRUE,
+   analysis_data = multi_bestmem$data
+ )
> publish(multi_bestmem_baseline$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio         CI.95 p-value
1            PCs_cardio            3.85   [2.05;7.24]  <0.001
2           Vasopressor            3.50   [1.88;6.50]  <0.001
3 Chronic_liver_disease           29.31 [12.68;67.75]  <0.001
> multi_bestmem_baseline$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR   n n_event_pos
1 0.8467034    0.796875   0.8388017        0.1197479            1             1          207               23              184  201 765          64
  n_event_neg                                                   glm_model   AUC_all ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1         701 AKI_2or3 ~ PCs_cardio + Vasopressor + Chronic_liver_disease 0.8467034         4.5         5.7          7.2         53.8
```

## Remove HT and Mecv from basline predictors

```R
Iteration: 1 bestvalit: 4.280794 bestmemit:    5.600000    2.000000   10.100000    6.900000
Iteration: 2 bestvalit: 4.212516 bestmemit:    5.100000    1.300000   10.800000   14.200000
Iteration: 3 bestvalit: 4.212516 bestmemit:    5.100000    1.300000   10.800000   14.200000
Iteration: 4 bestvalit: 4.052578 bestmemit:    5.200000    1.100000   10.100000   27.200000
Iteration: 5 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 6 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 7 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 8 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 9 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 10 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 11 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 12 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 13 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 14 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 15 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 16 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 17 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 18 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 19 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 20 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 21 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 22 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 23 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 24 bestvalit: 3.831842 bestmemit:    5.100000    1.200000    7.200000   46.600000
Iteration: 25 bestvalit: 3.774330 bestmemit:    5.100000    1.900000    8.900000   16.500000
Iteration: 26 bestvalit: 3.774330 bestmemit:    5.100000    1.900000    8.900000   16.500000
Iteration: 27 bestvalit: 3.774330 bestmemit:    5.100000    1.900000    8.900000   16.500000
Iteration: 28 bestvalit: 3.774330 bestmemit:    5.100000    1.900000    8.900000   16.500000
Iteration: 29 bestvalit: 3.694956 bestmemit:    4.500000    2.400000    9.700000   13.800000
Iteration: 30 bestvalit: 3.694956 bestmemit:    4.500000    2.400000    9.700000   13.800000
Iteration: 31 bestvalit: 3.694956 bestmemit:    4.500000    2.400000    9.700000   13.800000
Iteration: 32 bestvalit: 3.694956 bestmemit:    4.500000    2.400000    9.700000   13.800000
Iteration: 33 bestvalit: 3.694956 bestmemit:    4.500000    2.400000    9.700000   13.800000
Iteration: 34 bestvalit: 3.694956 bestmemit:    4.500000    2.400000    9.700000   13.800000
Iteration: 35 bestvalit: 3.694956 bestmemit:    4.500000    2.400000    9.700000   13.800000
Iteration: 36 bestvalit: 3.694956 bestmemit:    4.500000    2.400000    9.700000   13.800000
Iteration: 37 bestvalit: 3.694956 bestmemit:    4.500000    2.400000    9.700000   13.800000
Iteration: 38 bestvalit: 3.694956 bestmemit:    4.500000    2.400000    9.700000   13.800000
Iteration: 39 bestvalit: 3.694956 bestmemit:    4.500000    2.400000    9.700000   13.800000
Iteration: 40 bestvalit: 3.689047 bestmemit:    4.500000    2.500000    9.100000   15.900000
Iteration: 41 bestvalit: 3.689047 bestmemit:    4.500000    2.500000    9.100000   15.900000
Iteration: 42 bestvalit: 3.689047 bestmemit:    4.500000    2.500000    9.100000   15.900000
Iteration: 43 bestvalit: 3.689047 bestmemit:    4.500000    2.500000    9.100000   15.900000
Iteration: 44 bestvalit: 3.641510 bestmemit:    4.400000    2.500000   10.400000   14.200000
Iteration: 45 bestvalit: 3.604069 bestmemit:    4.400000    2.600000   10.200000   16.000000
Iteration: 46 bestvalit: 3.604069 bestmemit:    4.400000    2.600000   10.200000   16.000000
Iteration: 47 bestvalit: 3.604069 bestmemit:    4.400000    2.600000   10.200000   16.000000
Iteration: 48 bestvalit: 3.604069 bestmemit:    4.400000    2.600000   10.200000   16.000000
Iteration: 49 bestvalit: 3.604069 bestmemit:    4.400000    2.600000   10.200000   16.000000
Iteration: 50 bestvalit: 3.504697 bestmemit:    4.400000    2.600000    9.600000   16.200000
Iteration: 51 bestvalit: 3.504697 bestmemit:    4.400000    2.600000    9.600000   16.200000
Iteration: 52 bestvalit: 3.504697 bestmemit:    4.400000    2.600000    9.600000   16.200000
Iteration: 53 bestvalit: 3.504697 bestmemit:    4.400000    2.600000    9.600000   16.200000
Iteration: 54 bestvalit: 3.504697 bestmemit:    4.400000    2.600000    9.600000   16.200000
Iteration: 55 bestvalit: 3.503630 bestmemit:    4.400000    2.600000    9.500000   16.200000
Iteration: 56 bestvalit: 3.503630 bestmemit:    4.400000    2.600000    9.500000   16.200000
Iteration: 57 bestvalit: 3.503630 bestmemit:    4.400000    2.600000    9.500000   16.200000
Iteration: 58 bestvalit: 3.503630 bestmemit:    4.400000    2.600000    9.500000   16.200000
Iteration: 59 bestvalit: 3.503630 bestmemit:    4.400000    2.600000    9.500000   16.200000
Iteration: 60 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 61 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 62 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 63 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 64 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 65 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 66 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 67 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 68 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 69 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 70 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 71 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 72 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 73 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 74 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 75 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 76 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 77 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 78 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 79 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 80 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 81 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 82 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 83 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 84 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 85 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 86 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 87 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 88 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 89 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 90 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 91 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 92 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 93 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 94 bestvalit: 3.399761 bestmemit:    4.400000    2.600000    8.700000   16.200000
Iteration: 95 bestvalit: 3.397527 bestmemit:    4.400000    2.600000    8.700000   16.300000
Iteration: 96 bestvalit: 3.397527 bestmemit:    4.400000    2.600000    8.700000   16.300000
Iteration: 97 bestvalit: 3.397527 bestmemit:    4.400000    2.600000    8.700000   16.300000
Iteration: 98 bestvalit: 3.397527 bestmemit:    4.400000    2.600000    8.700000   16.300000
Iteration: 99 bestvalit: 3.392540 bestmemit:    4.400000    2.400000    8.700000   16.300000
Iteration: 100 bestvalit: 3.392540 bestmemit:    4.400000    2.400000    8.700000   16.300000
Iteration: 101 bestvalit: 3.392540 bestmemit:    4.400000    2.400000    8.700000   16.300000
Iteration: 102 bestvalit: 3.392540 bestmemit:    4.400000    2.400000    8.700000   16.300000
Iteration: 103 bestvalit: 3.392540 bestmemit:    4.400000    2.400000    8.700000   16.300000
Iteration: 104 bestvalit: 3.392540 bestmemit:    4.400000    2.400000    8.700000   16.300000
Iteration: 105 bestvalit: 3.392540 bestmemit:    4.400000    2.400000    8.700000   16.300000
Iteration: 106 bestvalit: 3.392540 bestmemit:    4.400000    2.400000    8.700000   16.300000
Iteration: 107 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 108 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 109 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 110 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 111 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 112 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 113 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 114 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 115 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 116 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 117 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 118 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 119 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 120 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 121 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 122 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 123 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 124 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 125 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 126 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 127 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 128 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 129 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 130 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 131 bestvalit: 3.388890 bestmemit:    4.400000    2.500000    8.700000   16.300000
Iteration: 132 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 133 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 134 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 135 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 136 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 137 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 138 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 139 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 140 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 141 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 142 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 143 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 144 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 145 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 146 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 147 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 148 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 149 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 150 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 151 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 152 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 153 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 154 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 155 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 156 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 157 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 158 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 159 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 160 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 161 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 162 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 163 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 164 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 165 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 166 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 167 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 168 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 169 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 170 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 171 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 172 bestvalit: 3.388231 bestmemit:    4.400000    2.600000    8.700000   16.500000
Iteration: 173 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 174 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 175 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 176 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 177 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 178 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 179 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 180 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 181 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 182 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 183 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 184 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 185 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 186 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 187 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 188 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 189 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 190 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 191 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 192 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 193 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 194 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 195 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 196 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 197 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 198 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 199 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
Iteration: 200 bestvalit: 3.371818 bestmemit:    4.400000    2.500000    8.700000   16.800000
>
> if (FALSE) multi_optim <- list(result = list(optim = list(bestmem = c(5.3, 1.6, 9.3, 30.7))))
>
> multi_bestmem <- heuristic_wrapper(multi_optim$result$optim$bestmem,
+   outcome_var = "AKI_2or3",
+   baseline_predictors = multi_baseline_predictors,
+   cr_predictors = "cr",
+   add_gradient_predictor = 1,
+   stepwise = TRUE,
+   k = "mBIC",
+   all_data = TRUE
+ )
> publish(multi_bestmem$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio         CI.95   p-value
1            PCs_cardio            3.88   [2.15;6.99]   < 0.001
2           Vasopressor            3.43   [2.06;5.70]   < 0.001
3              Diabetes            2.29   [1.37;3.82]   0.00151
4                   IHD            2.17   [1.25;3.77]   0.00596
5 Chronic_liver_disease           35.99 [17.87;72.49]   < 0.001
6           cr_gradient            2.72   [1.68;4.39]   < 0.001
> multi_bestmem$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR    n
1 0.8715354   0.7473684    0.862955        0.1156824    0.6286472     0.3970588          237               27              210  231 1496
  n_event_pos n_event_neg                                                                                  glm_model   AUC_all ch_hr_lower
1          95        1401 AKI_2or3 ~ PCs_cardio + Vasopressor + Diabetes + IHD + Chronic_liver_disease + cr_gradient 0.8655472        3.15
  ch_hr_upper aki_hr_lower aki_hr_upper
1        5.65          8.7         25.5
>
> multi_bestmem_all <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "AKI_2or3",
+   baseline_predictors = multi_baseline_predictors,
+   cr_predictors = "cr",
+   add_gradient_predictor = 1,
+
+   all_data = TRUE
+ )
> publish(multi_bestmem_all$model, print = FALSE, digits = c(2, 3))$regressionTable
                Variable Units OddsRatio         CI.95   p-value
1                    Age            0.98   [0.96;1.01]   0.13134
2                   Male            0.88   [0.49;1.58]   0.65962
3              APACHE_II            0.95   [0.86;1.05]   0.31036
4             APACHE_III            1.03   [1.00;1.06]   0.09025
5            Baseline_Cr            0.97   [0.95;0.99]   0.01354
6             PCs_cardio            5.45  [2.73;10.86]   < 0.001
7            Vasopressor            3.61   [2.10;6.22]   < 0.001
8               Diabetes            2.87   [1.66;4.96]   < 0.001
9                     AF            0.83   [0.33;2.09]   0.69928
10                   IHD            2.55   [1.42;4.60]   0.00178
11                    HF            1.87   [0.51;6.86]   0.34474
12                   PVD            1.35   [0.35;5.28]   0.66402
13 Chronic_liver_disease           39.01 [18.55;82.03]   < 0.001
14                    cr            1.02   [1.00;1.05]   0.04289
15           cr_gradient            2.08   [1.24;3.48]   0.00561
> multi_bestmem_all$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR    n
1 0.8655472   0.7157895   0.9122056        0.1306063    0.6286472     0.3970588          237               27              210  231 1496
  n_event_pos n_event_neg
1          95        1401
                                                                                                                                                            glm_model
1 AKI_2or3 ~ Age + Male + APACHE_II + APACHE_III + Baseline_Cr + PCs_cardio + Vasopressor + Diabetes + AF + IHD + HF + PVD + Chronic_liver_disease + cr + cr_gradient
    AUC_all ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1 0.8655472        3.15        5.65          8.7         25.5
>
> # FIXME: n_admission for these below are based on multi_bestmem$data, not the real analysis_data...
>
> multi_bestmem_aki <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "AKI_ICU",
+   baseline_predictors = gsub(".*~ ", "", multi_bestmem$summary$glm_model),
+   all_data = TRUE,
+   analysis_data = multi_bestmem$data
+ )
> publish(multi_bestmem_aki$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95  p-value
1            PCs_cardio            2.47  [1.82;3.37]   <0.001
2           Vasopressor            3.04  [2.29;4.02]   <0.001
3              Diabetes            1.44  [1.04;2.00]   0.0263
4                   IHD            0.77  [0.53;1.12]   0.1745
5 Chronic_liver_disease           13.05 [7.86;21.65]   <0.001
6           cr_gradient            2.14  [1.59;2.90]   <0.001
> multi_bestmem_aki$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR    n
1 0.7468598   0.7585139   0.6018755        0.1821211            1             1          237               97              140  231 1496
  n_event_pos n_event_neg                                                                                 glm_model   AUC_all ch_hr_lower
1         323        1173 AKI_ICU ~ PCs_cardio + Vasopressor + Diabetes + IHD + Chronic_liver_disease + cr_gradient 0.7468598        3.15
  ch_hr_upper aki_hr_lower aki_hr_upper
1        5.65          8.7         25.5
>
> multi_bestmem_cr <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "Cr_defined_AKI_2or3",
+   baseline_predictors = gsub(".*~ ", "", multi_bestmem$summary$glm_model),
+   all_data = TRUE,
+   analysis_data = multi_bestmem$data
+ )
> publish(multi_bestmem_cr$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio          CI.95   p-value
1            PCs_cardio            2.10    [1.07;4.12]   0.03130
2           Vasopressor            3.97    [2.10;7.52]   < 0.001
3              Diabetes            2.28    [1.26;4.15]   0.00666
4                   IHD            2.22    [1.10;4.48]   0.02514
5 Chronic_liver_disease           54.05 [25.32;115.36]   < 0.001
6           cr_gradient            3.69    [2.08;6.54]   < 0.001
> multi_bestmem_cr$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR    n
1 0.8813699   0.8059701   0.8908328       0.06355592            1             1          237               19              218  231 1496
  n_event_pos n_event_neg                                                                                             glm_model   AUC_all
1          67        1429 Cr_defined_AKI_2or3 ~ PCs_cardio + Vasopressor + Diabetes + IHD + Chronic_liver_disease + cr_gradient 0.8813699
  ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1        3.15        5.65          8.7         25.5
>
> multi_bestmem_cr_all <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "Cr_defined_AKI",
+   baseline_predictors = gsub(".*~ ", "", multi_bestmem$summary$glm_model),
+   all_data = TRUE,
+   analysis_data = multi_bestmem$data
+ )
> publish(multi_bestmem_cr_all$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95  p-value
1            PCs_cardio            0.92  [0.63;1.36]   0.6921
2           Vasopressor            2.89  [2.05;4.08]   <0.001
3              Diabetes            2.02  [1.40;2.91]   <0.001
4                   IHD            1.71  [1.10;2.67]   0.0173
5 Chronic_liver_disease           11.92 [7.17;19.82]   <0.001
6           cr_gradient            2.95  [2.07;4.21]   <0.001
> multi_bestmem_cr_all$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR    n
1 0.7596076   0.8376963   0.5624521       0.09952114            1             1          237               61              176  231 1496
  n_event_pos n_event_neg                                                                                        glm_model   AUC_all ch_hr_lower
1         191        1305 Cr_defined_AKI ~ PCs_cardio + Vasopressor + Diabetes + IHD + Chronic_liver_disease + cr_gradient 0.7596076        3.15
  ch_hr_upper aki_hr_lower aki_hr_upper
1        5.65          8.7         25.5
>
> multi_bestmem_olig <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "Olig_defined_AKI_2or3",
+   baseline_predictors = gsub(".*~ ", "", multi_bestmem$summary$glm_model),
+   all_data = TRUE,
+   analysis_data = multi_bestmem$data
+ )
> publish(multi_bestmem_olig$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio        CI.95 p-value
1            PCs_cardio            4.14  [1.93;8.89]  <0.001
2           Vasopressor            3.58  [1.80;7.10]  <0.001
3              Diabetes            1.80  [0.89;3.63]   0.103
4                   IHD            1.65  [0.78;3.48]   0.191
5 Chronic_liver_disease           14.22 [5.86;34.50]  <0.001
6           cr_gradient            1.08  [0.53;2.21]   0.824
> multi_bestmem_olig$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR    n
1 0.8379586   0.8409091   0.7665289        0.0273806            1             1          237               15              222  231 1496
  n_event_pos n_event_neg                                                                                               glm_model   AUC_all
1          44        1452 Olig_defined_AKI_2or3 ~ PCs_cardio + Vasopressor + Diabetes + IHD + Chronic_liver_disease + cr_gradient 0.8379586
  ch_hr_lower ch_hr_upper aki_hr_lower aki_hr_upper
1        3.15        5.65          8.7         25.5
>
> multi_bestmem_olig_all <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "Olig_defined_AKI",
+   baseline_predictors = gsub(".*~ ", "", multi_bestmem$summary$glm_model),
+   all_data = TRUE,
+   analysis_data = multi_bestmem$data
+ )
> publish(multi_bestmem_olig_all$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio       CI.95 p-value
1            PCs_cardio            3.42 [2.37;4.93]  <0.001
2           Vasopressor            2.72 [1.95;3.81]  <0.001
3              Diabetes            0.74 [0.47;1.14]   0.172
4                   IHD            0.41 [0.25;0.66]  <0.001
5 Chronic_liver_disease            5.03 [2.84;8.88]  <0.001
6           cr_gradient            1.13 [0.77;1.65]   0.523
> multi_bestmem_olig_all$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR    n
1 0.7083074   0.8044693   0.5307517        0.1010938            1             1          237               51              186  231 1496
  n_event_pos n_event_neg                                                                                          glm_model   AUC_all ch_hr_lower
1         179        1317 Olig_defined_AKI ~ PCs_cardio + Vasopressor + Diabetes + IHD + Chronic_liver_disease + cr_gradient 0.7083074        3.15
  ch_hr_upper aki_hr_lower aki_hr_upper
1        5.65          8.7         25.5
>
> multi_bestmem_baseline <- heuristic_wrapper(
+   multi_optim$result$optim$bestmem,
+   outcome_var = "AKI_2or3",
+   baseline_predictors = gsub(".*~ | \\+ cr_gradient", "", multi_bestmem$summary$glm_model),
+   all_data = TRUE,
+   analysis_data = multi_bestmem$data
+ )
> publish(multi_bestmem_baseline$model, print = FALSE, digits = c(2, 3))$regressionTable
               Variable Units OddsRatio         CI.95   p-value
1            PCs_cardio            4.63   [2.58;8.31]   < 0.001
2           Vasopressor            3.48   [2.10;5.75]   < 0.001
3              Diabetes            2.25   [1.36;3.72]   0.00158
4                   IHD            1.97   [1.13;3.41]   0.01602
5 Chronic_liver_disease           33.37 [16.65;66.88]   < 0.001
> multi_bestmem_baseline$summary
        AUC sensitivity specificity optimal_cutpoint per_admin_in per_admin_pos n_admissions n_admissions_pos n_admissions_neg n_UR    n
1 0.8670423   0.8105263   0.8087081       0.06652654            1             1          237               27              210  231 1496
  n_event_pos n_event_neg                                                                    glm_model   AUC_all ch_hr_lower ch_hr_upper
1          95        1401 AKI_2or3 ~ PCs_cardio + Vasopressor + Diabetes + IHD + Chronic_liver_disease 0.8670423        3.15        5.65
  aki_hr_lower aki_hr_upper
1          8.7         25.5
```
