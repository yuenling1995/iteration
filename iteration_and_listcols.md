Iteration and List Columns
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

## Lists

You can put anything in a list

``` r
l = list(
  vec_numeric = 5:8,
  vec_logical = c(T,T,F,T,F,F),
  mat = matrix(1:8, nrow = 2, ncol = 4),
  summary = summary(rnorm(100))
)
```

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
l[["vec_numeric"]]
```

    ## [1] 5 6 7 8

``` r
mean(l[["vec_numeric"]])
```

    ## [1] 6.5

``` r
l[["vec_numeric"]][1:3]
```

    ## [1] 5 6 7

## for loop

create a new list

``` r
list_norm = 
  list(
    a = rnorm(20, mean = 3, sd = 1),
    b = rnorm(30, mean = 0, sd = 5),
    c = rnorm(40, mean = 10, sd = 0.2),
    d = rnorm(20, mean = -3, sd = 1)
  )
```

``` r
list_norm
```

    ## $a
    ##  [1] 3.221186 1.680656 1.620622 1.918634 4.176876 4.563997 3.883008 2.665597
    ##  [9] 1.006802 2.630589 3.570259 2.452449 2.651444 3.298223 3.878314 2.171961
    ## [17] 2.304939 2.007589 3.429709 3.039292
    ## 
    ## $b
    ##  [1]  11.4457530   0.4793021  -3.3670034   1.8783501  -3.4246571 -17.1199433
    ##  [7]  -4.2796997  -0.5305944   4.4484385  -2.0002143   1.6174482  -3.6195891
    ## [13] -15.1431156   1.7270119   4.0775043   7.7959713  -2.1434050  -1.1194428
    ## [19]  -0.9259378   0.9215955  -4.4658702  -2.6770262   3.5760062  -7.3915917
    ## [25]   3.0641354   0.3890684  -1.8331326   2.7625968   4.9122752  -1.8130142
    ## 
    ## $c
    ##  [1] 10.138561 10.212762  9.910212  9.720448  9.719677  9.935623  9.989660
    ##  [8] 10.109467  9.860468 10.323527  9.422591  9.851049 10.019268 10.171599
    ## [15] 10.449466 10.150123  9.681592  9.909485 10.140200  9.893198  9.984349
    ## [22]  9.982373 10.169990 10.024587  9.847988 10.147450  9.775317 10.167166
    ## [29] 10.050603 10.087894 10.259213  9.854909  9.722610 10.034431 10.286104
    ## [36] 10.012906  9.789287 10.186090 10.244547 10.066044
    ## 
    ## $d
    ##  [1] -3.6612949 -3.6211844 -4.2629822 -4.3769802 -3.2912448 -1.3218603
    ##  [7] -3.8515762 -4.4077060 -2.6157957 -3.8990453 -2.5981506 -2.5994704
    ## [13] -4.4603140 -3.6444416 -3.8481176 -3.3248233 -1.5910968 -0.4635672
    ## [19] -3.7518927 -2.8166308

Pause and get my old function

``` r
mean_and_sd = function(x) {
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  if (length(x) <3) {
    stop("Input must have at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean= mean_x,
    sd = sd_x
  )
}
```

I can apply that function to each list element

``` r
mean_and_sd(list_norm[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.81 0.943

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.759  5.73

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.207

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.22  1.09

Let’s use a for loop:

``` r
output = vector("list",length = 4)
#output[[1]] = mean_and_sd(list_norm[1])

for (i in 1:4) {
  output[[i]] = mean_and_sd(list_norm[[i]])
}
```

## Let’s try map!

``` r
output = map(list_norm,mean_and_sd)
```

what if you want a different function?

``` r
output = map(list_norm, median)
output = map(list_norm, IQR)
```
