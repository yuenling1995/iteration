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

    ##  [1] -0.26901222 -1.46434274  1.08123939 -0.92951795  0.38973972  1.75065076
    ##  [7]  0.17364647 -0.50377426  1.25254495 -1.89892012 -0.40551188 -0.70555382
    ## [13]  1.97324197 -1.00023562 -0.07929402 -0.61609097 -0.18567604 -0.82063220
    ## [19] -0.79266822 -1.34644382  0.72457110  2.01438714  0.39329702  0.78828668
    ## [25] -0.11017307  0.35912141 -0.12304815  1.03833254 -0.62108384 -0.06708020

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

    ##  [1] -0.26901222 -1.46434274  1.08123939 -0.92951795  0.38973972  1.75065076
    ##  [7]  0.17364647 -0.50377426  1.25254495 -1.89892012 -0.40551188 -0.70555382
    ## [13]  1.97324197 -1.00023562 -0.07929402 -0.61609097 -0.18567604 -0.82063220
    ## [19] -0.79266822 -1.34644382  0.72457110  2.01438714  0.39329702  0.78828668
    ## [25] -0.11017307  0.35912141 -0.12304815  1.03833254 -0.62108384 -0.06708020

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
