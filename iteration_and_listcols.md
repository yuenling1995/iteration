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
    ##  [1] 1.722105 3.984202 4.535752 2.722209 1.845303 3.831053 2.174619 3.748003
    ##  [9] 3.066328 3.356505 3.702862 2.274252 4.684260 2.986223 3.254275 3.315370
    ## [17] 3.865449 3.431948 3.238990 4.380048
    ## 
    ## $b
    ##  [1]  1.8052620  4.6671292  3.2229654  3.6883231 -6.3495356 -4.1790562
    ##  [7] -0.6565195 -6.9026876  0.6781867  2.7299416  2.7656070 -4.9472005
    ## [13] -8.7378534 -2.7606640  5.0208310  0.6701987 -2.2717812  4.3209479
    ## [19] -5.7878963  3.8688549 -2.5395143  3.2138174 -0.2776699  3.2005194
    ## [25]  8.9436720  1.9299447 -1.1850527  3.7130093  0.8307710  5.7076316
    ## 
    ## $c
    ##  [1] 10.319574 10.012894 10.177612 10.257899 10.199687 10.137033 10.243094
    ##  [8] 10.353704 10.081085  9.927480  9.684332  9.997615  9.925809  9.977130
    ## [15]  9.575592 10.129792  9.724810  9.779630  9.995468  9.742676  9.969401
    ## [22]  9.821122 10.057542 10.087552 10.165603 10.113507  9.683017 10.111082
    ## [29]  9.941939 10.026470 10.108167 10.091851  9.856693 10.017984  9.903757
    ## [36] 10.121614  9.930467 10.239291  9.869753 10.542617
    ## 
    ## $d
    ##  [1] -2.765742 -2.481470 -3.201910 -2.343710 -3.234905 -2.170020 -3.771274
    ##  [8] -3.498518 -3.049905 -4.122333 -3.641850 -2.835511 -3.163404 -4.461609
    ## [15] -2.337030 -1.531835 -3.429139 -4.208758 -2.319881 -2.652388

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
    ## 1  3.31 0.842

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.479  4.27

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.200

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.06 0.761

Let’s use a for loop:

``` r
output = vector("list",length = 4)
#output[[1]] = mean_and_sd(list_norm[1])

for (i in 1:4) {
  output[[i]] = mean_and_sd(list_norm[[i]])
}
```
