Lab 6 Comparing two means
================
Jonathan Kanyinda
2021-04-22

Researchers studying the number of electric fish species living in
various parts of the Amazon basin were interested in whether the
presence of tributaries affected the local number of electric fish
species in the main rivers (Fernandes et al. 2004).

They counted the number of electric fish species above and below the
entrance point of a major tributary at 12 different river locations.

The data is provided in your GitHub repository.

For each question below, write a sentence answering the question and
show the code you used to come up with the answer, if applicable.

## Question A

> What is the mean different in the number of species between areas
> upstream and downstream of a tributary? What is the 95% confidence
> interval of this mean difference. Show your code and write a sentence
> giving your
answer

ANSWER

``` r
# load packages -----------------------------------------------------------

library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.4     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
# read data ---------------------------------------------------------------

fish <- read_csv("chap12q19ElectricFish.csv")  # fish data
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   tributary = col_character(),
    ##   speciesUpstream = col_double(),
    ##   speciesDownstream = col_double()
    ## )

``` r
crab <- read_csv("chap15q27FiddlerCrabFans.csv")  # crab data
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   crabType = col_character(),
    ##   bodyTemperature = col_double()
    ## )

``` r
# put data in tidy format ------------------------------------------------

fish_long <- 
  pivot_longer(fish, speciesUpstream:speciesDownstream,
               names_to = "location",
               values_to = "species") %>% 
  mutate(location = str_remove(location, c("species"))) %>% 
  print()
```

    ## # A tibble: 24 x 3
    ##    tributary location   species
    ##    <chr>     <chr>        <dbl>
    ##  1 Içá       Upstream        14
    ##  2 Içá       Downstream      19
    ##  3 Jutaí     Upstream        11
    ##  4 Jutaí     Downstream      18
    ##  5 Japurá    Upstream         8
    ##  6 Japurá    Downstream       8
    ##  7 Coari     Upstream         5
    ##  8 Coari     Downstream       7
    ##  9 Purus     Upstream        10
    ## 10 Purus     Downstream      16
    ## # … with 14 more rows

``` r
t.test(formula = species ~ location, data = fish_long)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  species by location
    ## t = 0.59249, df = 21.81, p-value = 0.5596
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -4.587031  8.253697
    ## sample estimates:
    ## mean in group Downstream   mean in group Upstream 
    ##                 16.41667                 14.58333

Mean difference :

Mean\_difference = 16.41667 - 14.58333

Mean\_difference \# The mean difference is 1.83334

95% confidence intervall of Mean length difference :

Upper\_limit = -4.587031

Lower\_limit = 8.253697

## Question B

> Test the hypothesis that the tributaries have no effect on the number
> of species of electric fish.

ANSWER

``` r
t.test(formula = species ~ location, data = fish_long)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  species by location
    ## t = 0.59249, df = 21.81, p-value = 0.5596
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -4.587031  8.253697
    ## sample estimates:
    ## mean in group Downstream   mean in group Upstream 
    ##                 16.41667                 14.58333

The p value 0.5596 is greater than 0.05, therefore the null hypothesis
will not be rejected. This is because the null hypotheis is rejected
when the p value is smaller than 0.05, and since the obtained p value
was 0.5596 which was greater than 0.05, there was failure to reject the
null hypothesis stating that there was no difference in teh number of
species in the main rivers caused by the presence of tributaries. The
means from the different locations were different, the mean in the
downstream group was 16.41667 and the mean in the upstream group was
14.58333. The mean difference was 1.83334

## Question C

> State the assumptions that you had to make to complete parts (A) and
> (B). Create a graph to assess whether one of those assumptions was
> met.

ANSWER

``` r
fish_long %>% 
  ggplot(aes(x = species)) +
  geom_histogram(
    aes(fill = location), 
    bins = 8, 
    alpha = 0.5, 
    position = "identity"
  ) +
  scale_fill_manual(values = c("darkorange","cyan4")) +
  theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

For Question A, the assumption made was that the presence of tributaries
did not affect the local number of electric fish in the main rivers
(Also the Null hypothesis). For part B, the assumption was that the data
was normally distributed and that the peaks/umps of the two locations
would be close enough to generate an overall normal distribution. Based
on the graph,the distribution does not appear to be normal and therefore
does not meet teh assumption made about the normality.

## Question D

> Graph the distribution of body temperatures for each crab type:

``` r
crab %>% 
  filter(!is.na(crabType)) %>% 
  ggplot(aes(x = bodyTemperature)) +
  geom_histogram(
    aes(fill = crabType), 
    bins = 10, 
    alpha = 0.5, 
    position = "identity",
    na.rm = TRUE
  )  + 
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4", "yellow")) +
  theme_minimal() +
  facet_wrap(~crabType,, ncol=1)
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Question E

> Does body temperature varies among crab types? State the null and
> alternative hypothesis, conduct and ANOVA, and interpret the results.

``` r
# ANOVA test

aov_body_temperature_crabType <-
  aov(bodyTemperature ~ crabType, data = crab)
aov_body_temperature_crabType
```

    ## Call:
    ##    aov(formula = bodyTemperature ~ crabType, data = crab)
    ## 
    ## Terms:
    ##                 crabType Residuals
    ## Sum of Squares  2.641310  3.467619
    ## Deg. of Freedom        3        80
    ## 
    ## Residual standard error: 0.2081952
    ## Estimated effects may be unbalanced
    ## 1 observation deleted due to missingness

``` r
summary(aov_body_temperature_crabType) # summary of the ANOVA test
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)    
    ## crabType     3  2.641  0.8804   20.31  7e-10 ***
    ## Residuals   80  3.468  0.0433                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 1 observation deleted due to missingness

The null hypothesis is that the mean body temparature would be the same
in all four crab groups and, teh alternative hypothesis is that the mean
body temperature would be different in at least one of thefour groups.
Based on the graph, a difference in body temparature can be observed and
females appeared to have a higher body temperature than that of the
males in general.

Based on the result of the ANOVA test, the p-value obtained is 7e-10
which would be lower than any most likely alpha chosen, and thus will
result un the rejection of the null hypothesis and come to the
conclusion that the mean body temperature would be different in at least
one of the four crab species
