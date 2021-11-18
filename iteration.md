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

    ##  [1]  0.6827700 -0.8151047 -1.0284172 -1.7399384  1.1101074  0.6977214
    ##  [7] -1.0927442  0.6899952  0.1687403 -0.3856973 -0.1254803 -0.7130969
    ## [13]  0.6792479 -0.2027386  0.7961188 -0.3160796  0.7603581  1.1298901
    ## [19]  1.3419656 -2.1794489  1.1247504  0.4915471 -1.4399097  0.8368490
    ## [25] -0.5405006  1.9074778 -1.2589012 -0.3547866 -0.3412783  0.1165835

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

    ##  [1]  0.6827700 -0.8151047 -1.0284172 -1.7399384  1.1101074  0.6977214
    ##  [7] -1.0927442  0.6899952  0.1687403 -0.3856973 -0.1254803 -0.7130969
    ## [13]  0.6792479 -0.2027386  0.7961188 -0.3160796  0.7603581  1.1298901
    ## [19]  1.3419656 -2.1794489  1.1247504  0.4915471 -1.4399097  0.8368490
    ## [25] -0.5405006  1.9074778 -1.2589012 -0.3547866 -0.3412783  0.1165835

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
    ## 1  2.84  4.04

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
    ## 1  3.59  2.89

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
    ## 1  6.35  2.46

## Let’s review Napoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

What about the next page of reviews?

Let’s turn the code into a function

``` r
read_page_reviews = function(url) {
  html = read_html(url)
  
  review_titles = 
    html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_text = 
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim()
  
  reviews = tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
  
  reviews
  
}
```

Let me try my function

``` r
dynamite_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"

read_page_reviews(dynamite_url)
```

    ## # A tibble: 10 × 3
    ##    title                                                 stars text             
    ##    <chr>                                                 <dbl> <chr>            
    ##  1 the cobweb in his hair during the bike ramp scene lol     5 5 stars for bein…
    ##  2 Best quirky movie ever                                    5 You all know the…
    ##  3 Classic Film                                              5 Had to order thi…
    ##  4 hehehehe                                                  5 goodjobboys      
    ##  5 Painful                                                   1 I think I sneeze…
    ##  6 GRAND                                                     5 GRAND            
    ##  7 Hello, 90s                                                5 So nostalgic mov…
    ##  8 Cult Classic                                              5 Watched it with …
    ##  9 Format was inaccurate                                     4 There was an opt…
    ## 10 Good funny                                                3 Would recommend

Let’s read a few pages of reviews.

``` r
dynamite_url_base= "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

dynamite_urls = str_c(dynamite_url_base, 1:5)

all_reviews =
  bind_rows(
    read_page_reviews(dynamite_urls[1]),
    read_page_reviews(dynamite_urls[2]),
    read_page_reviews(dynamite_urls[3]),
    read_page_reviews(dynamite_urls[4]),
    read_page_reviews(dynamite_urls[5])
  )
```

## Function as arguments

``` r
my_summary = function(x, summ_func) {
  
  summ_func(x)
}

x_vec = rnorm(100, 3, 7)

mean(x_vec)
```

    ## [1] 3.134394

``` r
median(x_vec)
```

    ## [1] 2.730327

``` r
my_summary(x_vec, IQR)
```

    ## [1] 8.128409
