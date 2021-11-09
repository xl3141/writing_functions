writing\_function
================
Xinyuan Liu
11/4/2021

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
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

## Z score

``` r
x_vec = rnorm(25, mean = 5, sd = 4 )

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.524817856  2.406390079 -0.721501328 -1.023848843 -0.630559388
    ##  [6] -0.446900721  0.918151181  0.086969609 -1.826788533 -0.491986513
    ## [11]  0.803046824 -1.657634287  0.413470894 -0.141330504  1.316131589
    ## [16] -0.090027941 -1.637637406  0.934173794  1.359127069  0.537842023
    ## [21] -0.375765073 -0.001381379 -0.252468818 -0.244113501  0.241823314

``` r
  z_scores = function(x){
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  }

z_scores(x = x_vec)
```

    ##  [1]  0.524817856  2.406390079 -0.721501328 -1.023848843 -0.630559388
    ##  [6] -0.446900721  0.918151181  0.086969609 -1.826788533 -0.491986513
    ## [11]  0.803046824 -1.657634287  0.413470894 -0.141330504  1.316131589
    ## [16] -0.090027941 -1.637637406  0.934173794  1.359127069  0.537842023
    ## [21] -0.375765073 -0.001381379 -0.252468818 -0.244113501  0.241823314

How great is this??

``` r
  z_scores = function(x){
  
  if(!is.numeric(x)){
    stop("x needs to be numeric")
  }
    
  if (length(x)< 3 ){
    stop("x should have at least 3 numbers")
  }
  z = (x - mean(x)) / sd(x)
  
  return(z)
  }
```

## Multiple outputs

``` r
  mean_sd = function(x){
  
  if(!is.numeric(x)){
    stop("x needs to be numeric")
  }
    
  if (length(x)< 3 ){
    stop("x should have at least 3 numbers")
  }
  mean_x = mean(x)
  sd_x = sd(x)
  
  output_df =
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  
  return(output_df)
  }

mean_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.22  3.32

## Different sample sizes, means, sds

``` r
sim_data =
  tibble(
    x = rnorm(30, mean = 2, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.04  2.70

Let’s write a function that simulates data, computes the mean and sd.

``` r
sim_mean_sd = function(n, mu, sigma){
 sim_data =
  tibble(
  x = rnorm(n, mean = mu, sd = sigma)
   )

 sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
}

sim_mean_sd(30, 4, 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.84  2.88

## revisit napolean

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

Write a function that gives reviews based on page url

``` r
get_page_reviews = function(page_url){

  

page_html = read_html(url)

review_titles = 
  page_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  page_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  page_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)

return(reviews)
}

base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

urls = str_c(base_url, 1:5)

bind_rows(
get_page_reviews(urls[1]),
get_page_reviews(urls[2]),
get_page_reviews(urls[3]),
get_page_reviews(urls[4]),
get_page_reviews(urls[5])
)
```

    ## # A tibble: 50 x 3
    ##    title                                                 stars text             
    ##    <chr>                                                 <dbl> <chr>            
    ##  1 Vintage                                                   5 Easy to order. I~
    ##  2 too many commercials                                      1 5 minutes into t~
    ##  3 this film is so good!                                     5 VOTE FOR PEDRO!  
    ##  4 Good movie                                                5 Weird story, goo~
    ##  5 I Just everyone to know this....                          5 VOTE FOR PEDRO !~
    ##  6 the cobweb in his hair during the bike ramp scene lol     5 5 stars for bein~
    ##  7 Best quirky movie ever                                    5 You all know the~
    ##  8 Classic Film                                              5 Had to order thi~
    ##  9 hehehehe                                                  5 goodjobboys      
    ## 10 Painful                                                   1 I think I sneeze~
    ## # ... with 40 more rows

``` r
get_page_reviews(url)
```

    ## # A tibble: 10 x 3
    ##    title                                                 stars text             
    ##    <chr>                                                 <dbl> <chr>            
    ##  1 Vintage                                                   5 Easy to order. I~
    ##  2 too many commercials                                      1 5 minutes into t~
    ##  3 this film is so good!                                     5 VOTE FOR PEDRO!  
    ##  4 Good movie                                                5 Weird story, goo~
    ##  5 I Just everyone to know this....                          5 VOTE FOR PEDRO !~
    ##  6 the cobweb in his hair during the bike ramp scene lol     5 5 stars for bein~
    ##  7 Best quirky movie ever                                    5 You all know the~
    ##  8 Classic Film                                              5 Had to order thi~
    ##  9 hehehehe                                                  5 goodjobboys      
    ## 10 Painful                                                   1 I think I sneeze~
