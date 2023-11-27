R Notebook for Milkbot SCALE
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
                            Value = as.numeric(last(Scale)),
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

![](Scale_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

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

![](Scale_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
summary(GLM)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
    ##   method [lmerModLmerTest]
    ## Formula: Value ~ DaysPregnantQuantile + Year + Month + DaysPregnantQuantile +  
    ##     Age + (1 | HerdId)
    ##    Data: AllData
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  72195.5  72261.2 -36088.8  72177.5    10871 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0141 -0.5830  0.0710  0.5998  4.1097 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  HerdId   (Intercept) 15.69    3.961   
    ##  Residual             43.34    6.583   
    ## Number of obs: 10880, groups:  HerdId, 89
    ## 
    ## Fixed effects:
    ##                                  Estimate Std. Error         df t value
    ## (Intercept)                    -2.661e+02  1.181e+02  1.085e+04  -2.253
    ## DaysPregnantQuantile1-5th Pct   1.500e+00  6.720e-01  1.088e+04   2.232
    ## DaysPregnantQuantile25-75 Pct   3.658e+00  6.144e-01  1.088e+04   5.954
    ## DaysPregnantQuantile5-25th Pct  3.255e+00  6.247e-01  1.088e+04   5.210
    ## Year                            1.477e-01  5.856e-02  1.085e+04   2.523
    ## Month                          -6.587e-02  1.883e-02  1.083e+04  -3.499
    ## Age                             6.605e-01  7.564e-02  1.086e+04   8.731
    ##                                Pr(>|t|)    
    ## (Intercept)                    0.024276 *  
    ## DaysPregnantQuantile1-5th Pct  0.025639 *  
    ## DaysPregnantQuantile25-75 Pct  2.71e-09 ***
    ## DaysPregnantQuantile5-25th Pct 1.92e-07 ***
    ## Year                           0.011664 *  
    ## Month                          0.000469 ***
    ## Age                             < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) DPQ1-P DPQ25P DPQ5-P Year   Month 
    ## DysPrgQ1-5P -0.009                                   
    ## DysPQ25-75P -0.012  0.889                            
    ## DysPrQ5-25P -0.008  0.873  0.966                     
    ## Year        -1.000  0.004  0.007  0.003              
    ## Month       -0.220 -0.008  0.004  0.001  0.219       
    ## Age         -0.057 -0.025 -0.018 -0.004  0.058  0.008

## Comparison of baseline and nested model

``` r
anova(GLM,baseline, test="Chisq")
```

    ## Data: AllData
    ## Models:
    ## baseline: Value ~ 1 + (1 | HerdId)
    ## GLM: Value ~ DaysPregnantQuantile + Year + Month + DaysPregnantQuantile + Age + (1 | HerdId)
    ##          npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## baseline    3 72363 72385 -36179    72357                         
    ## GLM         9 72196 72261 -36089    72178 179.57  6  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Least square means

``` r
emm_options(pbkrtest.limit = 10880)
LSMs<-emmeans::emmeans(GLM, pairwise~DaysPregnantQuantile, type = "response", adjust="sidak", glhargs=list())
multcomp::cld(LSMs$emmeans, alpha=0.05, Letters=letters, adjust="sidak")
```

    ##  DaysPregnantQuantile emmean    SE    df lower.CL upper.CL .group
    ##  0-1th Pct              31.2 0.740 709.2     29.4     33.1  a    
    ##  1-5th Pct              32.7 0.524 186.3     31.4     34.0  a    
    ##  5-25th Pct             34.5 0.455 105.9     33.3     35.6   b   
    ##  25-75 Pct              34.9 0.439  91.3     33.8     36.0   b   
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 4 estimates 
    ## P value adjustment: sidak method for 6 tests 
    ## significance level used: alpha = 0.05 
    ## NOTE: If two or more means share the same grouping symbol,
    ##       then we cannot show them to be different.
    ##       But we also did not show them to be the same.
