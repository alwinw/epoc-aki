# DEOptim Notes

## Summary

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

Baseline model with backwards stepwise regressions. Only impactful variable remain

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
