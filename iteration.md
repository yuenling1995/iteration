Iteration
================
Yuen
11/18/2021

## define the theme for all the ggplots!

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.3     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.1.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = 0.6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.color = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_color.discrete = scale_color_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec))/sd(x_vec)
```

    ##  [1] -1.836098983 -1.487232619  1.779540374  0.088417855  0.014619739
    ##  [6]  0.001810308  2.264740819 -1.254514415  0.371666163  0.149856243
    ## [11]  0.492856720  0.435224270 -0.647460886 -1.799039411 -0.823055731
    ## [16]  0.271343276 -0.423218323 -1.112478518 -0.583397698 -0.225579534
    ## [21]  0.698785342 -0.478076988  0.582811447  0.397384923  1.347697582
    ## [26]  0.680596195 -0.148403628 -0.144246902  1.749852746 -0.364400366

I want a function to compute z scores

``` r
z_scores = function(x){
  
  if (!is.numeric(x)){
    stop("Input must be numeric")
  }
  
  if (length(x)<3){
    stop("Input must have at least 3 numbers")
  }
  
  
  z = (x - mean(x)) / sd(x)
  return(z)
}

z_scores(x_vec)
```

    ##  [1] -1.836098983 -1.487232619  1.779540374  0.088417855  0.014619739
    ##  [6]  0.001810308  2.264740819 -1.254514415  0.371666163  0.149856243
    ## [11]  0.492856720  0.435224270 -0.647460886 -1.799039411 -0.823055731
    ## [16]  0.271343276 -0.423218323 -1.112478518 -0.583397698 -0.225579534
    ## [21]  0.698785342 -0.478076988  0.582811447  0.397384923  1.347697582
    ## [26]  0.680596195 -0.148403628 -0.144246902  1.749852746 -0.364400366

Try my function on some other things. These should give errors

``` r
z_scores(3)
```

    ## Error in z_scores(3): Input must have at least 3 numbers

``` r
z_scores("my name is Jeff")
```

    ## Error in z_scores("my name is Jeff"): Input must be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): Input must be numeric

``` r
z_scores(c(T,T,F,T))
```

    ## Error in z_scores(c(T, T, F, T)): Input must be numeric

## Multiple outputs

``` r
mean_and_sd = function(x){
  
  if (!is.numeric(x)){
    stop("Input must be numeric")
  }
  
  if (length(x)<3){
    stop("Input must have at least 3 numbers")
  }
  
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
}
```

Check that the function works.

``` r
x_vec = rnorm(1000, mean =3, sd = 4)
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.96  4.08

## Multiple inputs

I’d like to do this with a function

``` r
sim_data = 
  tibble(
    x = rnorm(n = 100, mean = 4, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.13  3.06

``` r
#true mean - mu, true sd - sigma
#and set up the default values if not defined
sim_mean_sd = function(samp_size, mu = 3, sigma = 4){
  sim_data = 
    tibble(
      x = rnorm(n = samp_size, mean = mu, sd = sigma)
    )
  
  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
}

sim_mean_sd(samp_size = 100, mu = 6, sigma = 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.07  2.87
