R Notebook for Milkbot DECAY
================

- [Raw Data import](#raw-data-import)
- [Data manipulation](#data-manipulation)
- [Model](#model)
  - [Base model](#base-model)
  - [Full model](#full-model)
  - [Comparison of baseline and nested
    model](#comparison-of-baseline-and-nested-model)
  - [Least square means](#least-square-means)

# Raw Data import

``` r
source('../DataImport.R')
```

# Data manipulation

``` r
#We inspect the quantile ranges
scale_this <- function(x){
  (x - mean(x, na.rm=F)) / sd(x, na.rm=F)
}

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
                            Age = scale(as.numeric(difftime(mdy_hms(CalvingDate), mdy_hms(BirthDate), units = "days"))),
                            DaysPregnantQuantile = case_when(
                              DaysPregnant < 243 ~ "0-1th Pct",
                              DaysPregnant < 267 ~ "1-5th Pct",
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
                                          CalvingDate,
                                          BirthDate
                                          ) %>% 
                          summarise(
                            Value = as.numeric(last(Decay)),
                            Age = as.numeric(last(Age))
                            ) %>% 
                          tidyr::drop_na() 
```

    ## `summarise()` has grouped output by 'AnimalId', 'HerdId',
    ## 'DaysPregnantQuantile', 'Year', 'Month', 'CalvingDate'. You can override using
    ## the `.groups` argument.

``` r
AllData %>% ungroup %>% count(DaysPregnantQuantile)    
```

    ## # A tibble: 4 × 2
    ##   DaysPregnantQuantile     n
    ##   <chr>                <int>
    ## 1 0-1th Pct              129
    ## 2 1-5th Pct              501
    ## 3 25-75 Pct             8019
    ## 4 5-25th Pct            2231

# Model

## Base model

``` r
baseline <- lmer(
                  Value ~ 1 +  (1 | HerdId), 
                  data = AllData,
                  REML = FALSE
                  )
qqnorm(residuals(baseline, type = 'pearson'))
```

![](Decay_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Full model

``` r
GLM <- lmer(
                  Value ~ 
                    DaysPregnantQuantile + Year + Month + DaysPregnantQuantile + Age
                     +  (1 | HerdId),
                  data = AllData,
                  REML = FALSE
                  )
qqnorm(residuals(GLM))
```

![](Decay_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
summary(GLM)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
    ##   method [lmerModLmerTest]
    ## Formula: Value ~ DaysPregnantQuantile + Year + Month + DaysPregnantQuantile +  
    ##     Age + (1 | HerdId)
    ##    Data: AllData
    ## 
    ##       AIC       BIC    logLik  deviance  df.resid 
    ## -130888.1 -130822.5   65453.1 -130906.1     10871 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6339 -0.6296 -0.0579  0.5941  3.0437 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev. 
    ##  HerdId   (Intercept) 4.554e-08 0.0002134
    ##  Residual             3.417e-07 0.0005846
    ## Number of obs: 10880, groups:  HerdId, 89
    ## 
    ## Fixed effects:
    ##                                  Estimate Std. Error         df t value
    ## (Intercept)                     3.224e-02  1.039e-02  1.027e+04   3.103
    ## DaysPregnantQuantile1-5th Pct   6.229e-05  5.940e-05  1.083e+04   1.049
    ## DaysPregnantQuantile25-75 Pct   1.414e-04  5.421e-05  1.071e+04   2.608
    ## DaysPregnantQuantile5-25th Pct  1.370e-04  5.513e-05  1.073e+04   2.485
    ## Year                           -1.534e-05  5.152e-06  1.027e+04  -2.979
    ## Month                          -1.083e-05  1.669e-06  1.086e+04  -6.488
    ## Age                             3.103e-05  6.695e-06  1.087e+04   4.635
    ##                                Pr(>|t|)    
    ## (Intercept)                     0.00192 ** 
    ## DaysPregnantQuantile1-5th Pct   0.29434    
    ## DaysPregnantQuantile25-75 Pct   0.00911 ** 
    ## DaysPregnantQuantile5-25th Pct  0.01297 *  
    ## Year                            0.00290 ** 
    ## Month                          9.10e-11 ***
    ## Age                            3.61e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) DPQ1-P DPQ25P DPQ5-P Year   Month 
    ## DysPrgQ1-5P -0.010                                   
    ## DysPQ25-75P -0.013  0.888                            
    ## DysPrQ5-25P -0.010  0.872  0.966                     
    ## Year        -1.000  0.005  0.008  0.005              
    ## Month       -0.217 -0.008  0.005  0.002  0.216       
    ## Age         -0.060 -0.025 -0.018 -0.004  0.060  0.008

## Comparison of baseline and nested model

``` r
anova(GLM,baseline, test="Chisq")
```

    ## Data: AllData
    ## Models:
    ## baseline: Value ~ 1 + (1 | HerdId)
    ## GLM: Value ~ DaysPregnantQuantile + Year + Month + DaysPregnantQuantile + Age + (1 | HerdId)
    ##          npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## baseline    3 -130817 -130796  65412  -130823                         
    ## GLM         9 -130888 -130822  65453  -130906 82.729  6  9.746e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Least square means

``` r
emm_options(pbkrtest.limit = 10880)
LSMs<-emmeans::emmeans(GLM, pairwise~DaysPregnantQuantile, type = "response", adjust="sidak", glhargs=list())
multcomp::cld(LSMs$emmeans, alpha=0.05, Letters=letters, adjust="sidak")
```

    ##  DaysPregnantQuantile  emmean       SE     df lower.CL upper.CL .group
    ##  0-1th Pct            0.00124 5.82e-05 2156.9  0.00109  0.00138  ab   
    ##  1-5th Pct            0.00130 3.55e-05  388.2  0.00121  0.00139  a    
    ##  5-25th Pct           0.00137 2.70e-05  130.6  0.00130  0.00144  ab   
    ##  25-75 Pct            0.00138 2.48e-05   92.5  0.00131  0.00144   b   
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 4 estimates 
    ## P value adjustment: sidak method for 6 tests 
    ## significance level used: alpha = 0.05 
    ## NOTE: If two or more means share the same grouping symbol,
    ##       then we cannot show them to be different.
    ##       But we also did not show them to be the same.
