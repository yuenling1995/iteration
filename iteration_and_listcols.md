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
    ##  [1] 2.7201691 0.9135922 2.8028470 1.4197362 2.1414951 1.2592193 2.7213793
    ##  [8] 3.0054211 3.5576964 4.0904960 3.6508953 1.7403138 1.4195167 3.8524479
    ## [15] 1.8857983 3.5829887 3.7748588 2.6047621 3.0072092 3.0987127
    ## 
    ## $b
    ##  [1]  1.38130957 -6.25633562  6.84806870 -0.84540462  0.70887525 -6.49071015
    ##  [7]  6.97759550 -0.97344544  2.51982420 -6.42726078 -2.41378548 -0.07198562
    ## [13] -0.87730350  2.76452076 -2.80923944 -9.65719066  1.76608990  0.77602572
    ## [19] -5.88006677 -5.01862622 -5.11130771 -8.06195563  6.25579597 -5.85096232
    ## [25] -2.49447937  1.12830783  6.64074670  1.88950484 -8.01992190 -3.89431180
    ## 
    ## $c
    ##  [1]  9.824355  9.983218  9.802413 10.050125  9.761102  9.898065  9.793110
    ##  [8] 10.168044  9.771296  9.590112  9.924598 10.018651 10.173753  9.978300
    ## [15]  9.947749  9.837064  9.992294  9.708045  9.857651 10.231550 10.118654
    ## [22] 10.236202 10.278102 10.253924 10.027116 10.235709  9.702661 10.073276
    ## [29] 10.053119  9.925344  9.887183 10.301444  9.837610 10.180971 10.142437
    ## [36]  9.788897 10.360681 10.012472  9.593285 10.067221
    ## 
    ## $d
    ##  [1] -3.573441 -3.228991 -2.168548 -2.455203 -2.656434 -1.741407 -5.070488
    ##  [8] -3.507519 -3.809679 -2.180245 -1.583405 -1.124006 -3.707899 -3.046484
    ## [15] -2.937552 -4.005778 -1.173564 -3.057925 -3.054352 -1.849662

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
    ## 1  2.66 0.963

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.38  4.73

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.98 0.199

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.80  1.02

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

``` r
#double
output = map_dbl(list_norm, median)
```

``` r
output = map_df(list_norm, mean_and_sd, .id = "input")
```

## List columns

``` r
listcol_df =
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norm
  )
```

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(samp)
```

    ## $a
    ##  [1] 2.7201691 0.9135922 2.8028470 1.4197362 2.1414951 1.2592193 2.7213793
    ##  [8] 3.0054211 3.5576964 4.0904960 3.6508953 1.7403138 1.4195167 3.8524479
    ## [15] 1.8857983 3.5829887 3.7748588 2.6047621 3.0072092 3.0987127
    ## 
    ## $b
    ##  [1]  1.38130957 -6.25633562  6.84806870 -0.84540462  0.70887525 -6.49071015
    ##  [7]  6.97759550 -0.97344544  2.51982420 -6.42726078 -2.41378548 -0.07198562
    ## [13] -0.87730350  2.76452076 -2.80923944 -9.65719066  1.76608990  0.77602572
    ## [19] -5.88006677 -5.01862622 -5.11130771 -8.06195563  6.25579597 -5.85096232
    ## [25] -2.49447937  1.12830783  6.64074670  1.88950484 -8.01992190 -3.89431180
    ## 
    ## $c
    ##  [1]  9.824355  9.983218  9.802413 10.050125  9.761102  9.898065  9.793110
    ##  [8] 10.168044  9.771296  9.590112  9.924598 10.018651 10.173753  9.978300
    ## [15]  9.947749  9.837064  9.992294  9.708045  9.857651 10.231550 10.118654
    ## [22] 10.236202 10.278102 10.253924 10.027116 10.235709  9.702661 10.073276
    ## [29] 10.053119  9.925344  9.887183 10.301444  9.837610 10.180971 10.142437
    ## [36]  9.788897 10.360681 10.012472  9.593285 10.067221
    ## 
    ## $d
    ##  [1] -3.573441 -3.228991 -2.168548 -2.455203 -2.656434 -1.741407 -5.070488
    ##  [8] -3.507519 -3.809679 -2.180245 -1.583405 -1.124006 -3.707899 -3.046484
    ## [15] -2.937552 -4.005778 -1.173564 -3.057925 -3.054352 -1.849662

``` r
listcol_df %>% 
  filter(name == "a")
```

    ## # A tibble: 1 × 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>

Let’s try some operations

``` r
listcol_df$samp
```

    ## $a
    ##  [1] 2.7201691 0.9135922 2.8028470 1.4197362 2.1414951 1.2592193 2.7213793
    ##  [8] 3.0054211 3.5576964 4.0904960 3.6508953 1.7403138 1.4195167 3.8524479
    ## [15] 1.8857983 3.5829887 3.7748588 2.6047621 3.0072092 3.0987127
    ## 
    ## $b
    ##  [1]  1.38130957 -6.25633562  6.84806870 -0.84540462  0.70887525 -6.49071015
    ##  [7]  6.97759550 -0.97344544  2.51982420 -6.42726078 -2.41378548 -0.07198562
    ## [13] -0.87730350  2.76452076 -2.80923944 -9.65719066  1.76608990  0.77602572
    ## [19] -5.88006677 -5.01862622 -5.11130771 -8.06195563  6.25579597 -5.85096232
    ## [25] -2.49447937  1.12830783  6.64074670  1.88950484 -8.01992190 -3.89431180
    ## 
    ## $c
    ##  [1]  9.824355  9.983218  9.802413 10.050125  9.761102  9.898065  9.793110
    ##  [8] 10.168044  9.771296  9.590112  9.924598 10.018651 10.173753  9.978300
    ## [15]  9.947749  9.837064  9.992294  9.708045  9.857651 10.231550 10.118654
    ## [22] 10.236202 10.278102 10.253924 10.027116 10.235709  9.702661 10.073276
    ## [29] 10.053119  9.925344  9.887183 10.301444  9.837610 10.180971 10.142437
    ## [36]  9.788897 10.360681 10.012472  9.593285 10.067221
    ## 
    ## $d
    ##  [1] -3.573441 -3.228991 -2.168548 -2.455203 -2.656434 -1.741407 -5.070488
    ##  [8] -3.507519 -3.809679 -2.180245 -1.583405 -1.124006 -3.707899 -3.046484
    ## [15] -2.937552 -4.005778 -1.173564 -3.057925 -3.054352 -1.849662

``` r
listcol_df$samp[[1]]
```

    ##  [1] 2.7201691 0.9135922 2.8028470 1.4197362 2.1414951 1.2592193 2.7213793
    ##  [8] 3.0054211 3.5576964 4.0904960 3.6508953 1.7403138 1.4195167 3.8524479
    ## [15] 1.8857983 3.5829887 3.7748588 2.6047621 3.0072092 3.0987127

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.38  4.73

can I just … map?

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.66 0.963
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.38  4.73
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.98 0.199
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.80  1.02

So can I add a list column to the existing df?

``` r
listcol_df = 
  listcol_df %>% 
    mutate(summary = map(samp,mean_and_sd),
           medians = map_dbl(samp, median))
```

## Weather Data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"),
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>% 
  mutate(
    name = recode(
      id,
      USW00094728 = "Central_Park_NY",
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin/10,
    tmax = tmax/10) %>% 
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2021-10-26 15:01:14 (7.606)

    ## file min/max dates: 1869-01-01 / 2021-10-31

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2021-10-26 15:01:18 (1.697)

    ## file min/max dates: 1965-01-01 / 2020-02-29

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2021-10-26 15:01:19 (0.912)

    ## file min/max dates: 1999-09-01 / 2021-10-31

Get our list columns…

``` r
#create a df for each location
weather_nest = 
  weather_df %>% 
  nest(data = date:tmin)
```

``` r
weather_nest %>%pull(name)
```

    ## [1] "Central_Park_NY" "Waikiki_HA"      "Waterhole_WA"

``` r
weather_nest %>% pull(data)
```

    ## [[1]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows
    ## 
    ## [[2]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0  26.7  16.7
    ##  2 2017-01-02     0  27.2  16.7
    ##  3 2017-01-03     0  27.8  17.2
    ##  4 2017-01-04     0  27.2  16.7
    ##  5 2017-01-05     0  27.8  16.7
    ##  6 2017-01-06     0  27.2  16.7
    ##  7 2017-01-07     0  27.2  16.7
    ##  8 2017-01-08     0  25.6  15  
    ##  9 2017-01-09     0  27.2  15.6
    ## 10 2017-01-10     0  28.3  17.2
    ## # … with 355 more rows
    ## 
    ## [[3]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01   432  -6.8 -10.7
    ##  2 2017-01-02    25 -10.5 -12.4
    ##  3 2017-01-03     0  -8.9 -15.9
    ##  4 2017-01-04     0  -9.9 -15.5
    ##  5 2017-01-05     0  -5.9 -14.2
    ##  6 2017-01-06     0  -4.4 -11.3
    ##  7 2017-01-07    51   0.6 -11.5
    ##  8 2017-01-08    76   2.3  -1.2
    ##  9 2017-01-09    51  -1.2  -7  
    ## 10 2017-01-10     0  -5   -14.2
    ## # … with 355 more rows

``` r
#the first df - central park
weather_nest$data[[1]]
```

    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows

Suppose i want to regress tmax on tmin for each station

This works…

``` r
lm(tmax ~ tmin, data = weather_nest$data[[2]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[2]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509

Let’s write a function to do this regression

``` r
weather_lm = function(df){
  lm(tmax ~ tmin, data = df)
}

#regress each df
weather_lm(weather_nest$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
weather_lm(weather_nest$data[[2]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509

``` r
weather_lm(weather_nest$data[[3]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

``` r
#same thing - do it in map function:
map(weather_nest$data, weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

what about a map in a list column??

``` r
weather_nest = 
  weather_nest %>% 
    mutate(models = map(data,weather_lm))

#fetch the regression results for the second df
weather_nest$models[[2]]
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509
