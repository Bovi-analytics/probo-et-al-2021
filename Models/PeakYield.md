R Notebook
================

-   [Raw Data import](#raw-data-import)
-   [Data manipulation](#data-manipulation)
-   [Model M305](#model-m305)
    -   [Base model](#base-model)
    -   [Full model](#full-model)
    -   [Comparison of baseline and nested model](#comparison-of-baseline-and-nested-model)
    -   [Least square means](#least-square-means)

Raw Data import
===============

``` r
source('../DataImport.R')
```

Data manipulation
=================

``` r
#We inspect the quantile ranges

quantile(AllDataRaw$DaysPregnant)
```

    ##   0%  25%  50%  75% 100% 
    ##  150  275  278  283  297

``` r
AllData <- AllDataRaw %>% dplyr::filter(
                            LactationNumber == 1,
                            DaysPregnant <= 283, #We drop all above 75th percentile because no interest at this stage, missing inseminations?
                            M305 > 0 #No missing M305 calculations
                            ) %>% 
                          dplyr::mutate(
                            Date  = mdy_hms(Date), #reformat ordering date
                            Year = year(mdy_hms(CalvingDate)),
                            Month = month(mdy_hms(CalvingDate)),
                            DaysPregnantQuantile = case_when(
                              DaysPregnant < 267 ~ "0-5th Pct",
                              DaysPregnant < 275 ~ "5-25th Pct",
                              TRUE ~ "25-75 Pct"
                              )
                            ) %>%
                          dplyr::arrange(
                            HerdId,
                            AnimalId,
                            Date
                          ) %>%
                          dplyr::group_by(
                                          AnimalId,
                                          HerdId,
                                          DaysPregnantQuantile,
                                          Year,
                                          Month,
                                          CalvingDate
                                          ) %>% 
                          summarise(
                            Value = as.integer(last(PeakMilk))
                            )
```

Model M305
==========

Base model
----------

``` r
baseline <- lmer(
                  Value ~ 1 +  (1 | HerdId), 
                  data = AllData
                  )
qqnorm(residuals(baseline, type = 'pearson'))
```

![](PeakYield_files/figure-markdown_github/unnamed-chunk-4-1.png)

Full model
----------

``` r
GLM <- lmer(
                  Value ~ 
                    DaysPregnantQuantile + Year + Month
                     +  (1 | HerdId),
                  data = AllData
                  )
qqnorm(residuals(GLM))
```

![](PeakYield_files/figure-markdown_github/unnamed-chunk-5-1.png)

Comparison of baseline and nested model
---------------------------------------

``` r
anova(GLM,baseline, test="Chisq")
```

    ## refitting model(s) with ML (instead of REML)

    ## Data: AllData
    ## Models:
    ## baseline: Value ~ 1 + (1 | HerdId)
    ## GLM: Value ~ DaysPregnantQuantile + Year + Month + (1 | HerdId)
    ##          Df   AIC   BIC logLik deviance Chisq Chi Df Pr(>Chisq)    
    ## baseline  3 67583 67605 -33789    67577                            
    ## GLM       7 67510 67561 -33748    67496 80.97      4  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Least square means
------------------

``` r
lsmeans(GLM, pairwise~DaysPregnantQuantile, type = "response", adjust="tukey")
```

    ## Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
    ## To enable adjustments, set emm_options(pbkrtest.limit = 10616) or larger,
    ## but be warned that this may result in large computation time and memory use.

    ## Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
    ## To enable adjustments, set emm_options(lmerTest.limit = 10616) or larger,
    ## but be warned that this may result in large computation time and memory use.

    ## $lsmeans
    ##  DaysPregnantQuantile lsmean    SE  df asymp.LCL asymp.UCL
    ##  0-5th Pct              28.1 0.454 Inf      27.2      29.0
    ##  25-75 Pct              30.1 0.395 Inf      29.3      30.8
    ##  5-25th Pct             29.6 0.409 Inf      28.8      30.4
    ## 
    ## Degrees-of-freedom method: asymptotic 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast               estimate    SE  df z.ratio p.value
    ##  0-5th Pct - 25-75 Pct    -1.960 0.249 Inf -7.870  <.0001 
    ##  0-5th Pct - 5-25th Pct   -1.524 0.269 Inf -5.660  <.0001 
    ##  25-75 Pct - 5-25th Pct    0.435 0.143 Inf  3.051  0.0065 
    ## 
    ## P value adjustment: tukey method for comparing a family of 3 estimates
