STT3030 Lab 1
================
2024

``` r
getwd()
```

    ## [1] "/Users/arthurcharpentier"

``` r
resultat = sin(sqrt(1:10))
resultat = vector(mode="numeric", length = 10L)
for (i in 1:length(resultat)) {
   resultat[i] = sin(sqrt(i))
}
```

``` r
library("exactRankTests")
```

    ##  Package 'exactRankTests' is no longer under development.
    ##  Please consider using package 'coin' instead.

``` r
?exactRankTests::perm.test
?perm.test
```

``` r
# Sample data
x <- c(1, 2, 3, 4, 5)
y <- c(6, 7, 8, 9, 10)

# Function to perform permutation test
permutation_test <- function(x, y, num_permutations = 10000) {
  # Combine the data
  combined <- c(x, y)
  n_x <- length(x)
  n_y <- length(y)
  
  # Calculate the observed difference in means
  observed_diff <- mean(x) - mean(y)
  
  # Initialize a vector to store the permutation differences
  perm_diffs <- numeric(num_permutations)
  
  # Perform the permutations
  for (i in 1:num_permutations) {
    # Permute the combined data
    permuted <- sample(combined)
    
    # Split the permuted data into two new samples
    perm_x <- permuted[1:n_x]
    perm_y <- permuted[(n_x + 1):(n_x + n_y)]
    
    # Calculate the difference in means for the permuted samples
    perm_diffs[i] <- mean(perm_x) - mean(perm_y)
  }
  
  # Calculate the p-value
  p_value <- mean(abs(perm_diffs) >= abs(observed_diff))
  
  # Return the observed difference and p-value
  list(observed_diff = observed_diff, p_value = p_value)
}

# Run the permutation test
result <- permutation_test(x, y)
print(result)
```

    ## $observed_diff
    ## [1] -5
    ## 
    ## $p_value
    ## [1] 0.0078

``` r
# install.packages("coin")
# Load the coin package
library(coin)
```

    ## Loading required package: survival

    ## 
    ## Attaching package: 'coin'

    ## The following objects are masked from 'package:exactRankTests':
    ## 
    ##     dperm, pperm, qperm, rperm

``` r
# Sample data
x <- c(1, 2, 3, 4, 5)
y <- c(6, 7, 8, 9, 10)

# Combine the data into a data frame
data <- data.frame(
  values = c(x, y),
  group = factor(rep(c("x", "y"), times = c(length(x), length(y))))
)

# Perform the permutation test
result <- oneway_test(values ~ group, data = data, distribution = "approximate")

# Print the result
print(result)
```

    ## 
    ##  Approximative Two-Sample Fisher-Pitman Permutation Test
    ## 
    ## data:  values by group (x, y)
    ## Z = -2.6112, p-value = 0.006
    ## alternative hypothesis: true mu is not equal to 0

``` r
x = rnorm(10)
y = rnorm(10)
data <- data.frame(
     values = c(x, y),
     group = factor(rep(c("x", "y"), times = c(length(x), length(y))))
 )
result <- oneway_test(values ~ group, data = data, distribution = "approximate")
result
```

    ## 
    ##  Approximative Two-Sample Fisher-Pitman Permutation Test
    ## 
    ## data:  values by group (x, y)
    ## Z = 0.016294, p-value = 0.9874
    ## alternative hypothesis: true mu is not equal to 0

``` r
str(result)
```

    ## Formal class 'ScalarIndependenceTest' [package "coin"] with 7 slots
    ##   ..@ parameter   : chr "mu"
    ##   ..@ nullvalue   : num 0
    ##   ..@ distribution:Formal class 'ApproxNullDistribution' [package "coin"] with 12 slots
    ##   .. .. ..@ seed          : int [1:626] 10403 490 2079088929 905161450 -13685813 -513608559 559729705 316819953 116463162 1399547243 ...
    ##   .. .. ..@ nresample     : int 10000
    ##   .. .. ..@ size          :function (alpha, type)  
    ##   .. .. ..@ pvalueinterval:function (q)  
    ##   .. .. ..@ midpvalue     :function (q)  
    ##   .. .. ..@ q             :function (p)  
    ##   .. .. ..@ d             :function (x)  
    ##   .. .. ..@ support       :function (raw = FALSE)  
    ##   .. .. ..@ parameters    : list()
    ##   .. .. ..@ pvalue        :function (q)  
    ##   .. .. ..@ p             :function (q)  
    ##   .. .. ..@ name          : chr "Monte Carlo Distribution"
    ##   ..@ statistic   :Formal class 'ScalarIndependenceTestStatistic' [package "coin"] with 15 slots
    ##   .. .. ..@ alternative                : chr "two.sided"
    ##   .. .. ..@ paired                     : logi FALSE
    ##   .. .. ..@ teststatistic              : num 0.0163
    ##   .. .. ..@ standardizedlinearstatistic: num 0.0163
    ##   .. .. ..@ linearstatistic            : num [1, 1] -0.0286
    ##   .. .. ..@ expectation                : num [1, 1] -0.0562
    ##   .. .. ..@ covariance                 : num [1, 1] 2.86
    ##   .. .. ..@ xtrans                     : num [1:20, 1] 1 1 1 1 1 1 1 1 1 1 ...
    ##   .. .. .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. .. .. ..$ : chr [1:20] "1" "2" "3" "4" ...
    ##   .. .. .. .. ..$ : chr "x"
    ##   .. .. .. ..- attr(*, "assign")= int 1
    ##   .. .. ..@ ytrans                     : num [1:20, 1] 0.0292 -0.5732 -0.2009 0.6917 0.4475 ...
    ##   .. .. .. ..- attr(*, "assign")= int 1
    ##   .. .. .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. .. .. ..$ : NULL
    ##   .. .. .. .. ..$ : chr ""
    ##   .. .. ..@ xtrafo                     :function (data, numeric_trafo = id_trafo, factor_trafo = f_trafo, ordered_trafo = of_trafo, 
    ##     surv_trafo = logrank_trafo, var_trafo = NULL, block = NULL)  
    ##   .. .. ..@ ytrafo                     :function (data, numeric_trafo = id_trafo, factor_trafo = f_trafo, ordered_trafo = of_trafo, 
    ##     surv_trafo = logrank_trafo, var_trafo = NULL, block = NULL)  
    ##   .. .. ..@ x                          :'data.frame':    20 obs. of  1 variable:
    ##   .. .. .. ..$ group: Factor w/ 2 levels "x","y": 1 1 1 1 1 1 1 1 1 1 ...
    ##   .. .. .. ..- attr(*, "terms")=Classes 'terms', 'formula'  language ~group
    ##   .. .. .. .. .. ..- attr(*, "variables")= language list(group)
    ##   .. .. .. .. .. ..- attr(*, "factors")= int [1, 1] 1
    ##   .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. .. .. .. .. .. ..$ : chr "group"
    ##   .. .. .. .. .. .. .. ..$ : chr "group"
    ##   .. .. .. .. .. ..- attr(*, "term.labels")= chr "group"
    ##   .. .. .. .. .. ..- attr(*, "order")= int 1
    ##   .. .. .. .. .. ..- attr(*, "intercept")= int 1
    ##   .. .. .. .. .. ..- attr(*, "response")= int 0
    ##   .. .. .. .. .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
    ##   .. .. .. .. .. ..- attr(*, "predvars")= language list(group)
    ##   .. .. .. .. .. ..- attr(*, "dataClasses")= Named chr "factor"
    ##   .. .. .. .. .. .. ..- attr(*, "names")= chr "group"
    ##   .. .. ..@ y                          :'data.frame':    20 obs. of  1 variable:
    ##   .. .. .. ..$ values: num [1:20] 0.0292 -0.5732 -0.2009 0.6917 0.4475 ...
    ##   .. .. .. ..- attr(*, "terms")=Classes 'terms', 'formula'  language ~values
    ##   .. .. .. .. .. ..- attr(*, "variables")= language list(values)
    ##   .. .. .. .. .. ..- attr(*, "factors")= int [1, 1] 1
    ##   .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. .. .. .. .. .. ..$ : chr "values"
    ##   .. .. .. .. .. .. .. ..$ : chr "values"
    ##   .. .. .. .. .. ..- attr(*, "term.labels")= chr "values"
    ##   .. .. .. .. .. ..- attr(*, "order")= int 1
    ##   .. .. .. .. .. ..- attr(*, "intercept")= int 1
    ##   .. .. .. .. .. ..- attr(*, "response")= int 0
    ##   .. .. .. .. .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
    ##   .. .. .. .. .. ..- attr(*, "predvars")= language list(values)
    ##   .. .. .. .. .. ..- attr(*, "dataClasses")= Named chr "numeric"
    ##   .. .. .. .. .. .. ..- attr(*, "names")= chr "values"
    ##   .. .. ..@ block                      : Factor w/ 1 level "0": 1 1 1 1 1 1 1 1 1 1 ...
    ##   .. .. ..@ weights                    : int [1:20] 1 1 1 1 1 1 1 1 1 1 ...
    ##   ..@ estimates   : list()
    ##   ..@ method      : chr "Two-Sample Fisher-Pitman Permutation Test"
    ##   ..@ call        : language independence_test.IndependenceProblem(object = new("IndependenceProblem",      x = list(group = c(1L, 1L, 1L, 1L,| __truncated__ ...

``` r
# Install and load necessary packages
if (!requireNamespace("osmdata", quietly = TRUE)) {
  install.packages("osmdata")
}
if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")
}
if (!requireNamespace("mapview", quietly = TRUE)) {
  install.packages("mapview")
}

library(osmdata)
```

    ## Data (c) OpenStreetMap contributors, ODbL 1.0. https://www.openstreetmap.org/copyright

``` r
library(sf)
```

    ## Linking to GEOS 3.11.0, GDAL 3.5.3, PROJ 9.1.0; sf_use_s2() is TRUE

``` r
library(mapview)

# Define the bounding box for Montréal, QC
montréal_bbox <- getbb("Montréal, Québec")

# Query OSM for all Jean Coutu locations in Montréal
jean_coutu_query <- opq(bbox = montréal_bbox) %>%
  add_osm_feature(key = "name", value = "Jean Coutu")

# Extract data
jean_coutu_data <- osmdata_sf(jean_coutu_query)

# Check the structure of the returned data
print(names(jean_coutu_data))
```

    ## [1] "bbox"              "overpass_call"     "meta"             
    ## [4] "osm_points"        "osm_lines"         "osm_polygons"     
    ## [7] "osm_multilines"    "osm_multipolygons"

``` r
# Extract the points data (store locations)
jean_coutu_locations <- jean_coutu_data$osm_points

# Visualize the locations on a map using mapview
# mapview(jean_coutu_locations)
```

``` r
regression = lm(dist ~ speed, data = cars)
str(regression)
```

    ## List of 12
    ##  $ coefficients : Named num [1:2] -17.58 3.93
    ##   ..- attr(*, "names")= chr [1:2] "(Intercept)" "speed"
    ##  $ residuals    : Named num [1:50] 3.85 11.85 -5.95 12.05 2.12 ...
    ##   ..- attr(*, "names")= chr [1:50] "1" "2" "3" "4" ...
    ##  $ effects      : Named num [1:50] -303.914 145.552 -8.115 9.885 0.194 ...
    ##   ..- attr(*, "names")= chr [1:50] "(Intercept)" "speed" "" "" ...
    ##  $ rank         : int 2
    ##  $ fitted.values: Named num [1:50] -1.85 -1.85 9.95 9.95 13.88 ...
    ##   ..- attr(*, "names")= chr [1:50] "1" "2" "3" "4" ...
    ##  $ assign       : int [1:2] 0 1
    ##  $ qr           :List of 5
    ##   ..$ qr   : num [1:50, 1:2] -7.071 0.141 0.141 0.141 0.141 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : chr [1:50] "1" "2" "3" "4" ...
    ##   .. .. ..$ : chr [1:2] "(Intercept)" "speed"
    ##   .. ..- attr(*, "assign")= int [1:2] 0 1
    ##   ..$ qraux: num [1:2] 1.14 1.27
    ##   ..$ pivot: int [1:2] 1 2
    ##   ..$ tol  : num 1e-07
    ##   ..$ rank : int 2
    ##   ..- attr(*, "class")= chr "qr"
    ##  $ df.residual  : int 48
    ##  $ xlevels      : Named list()
    ##  $ call         : language lm(formula = dist ~ speed, data = cars)
    ##  $ terms        :Classes 'terms', 'formula'  language dist ~ speed
    ##   .. ..- attr(*, "variables")= language list(dist, speed)
    ##   .. ..- attr(*, "factors")= int [1:2, 1] 0 1
    ##   .. .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. .. ..$ : chr [1:2] "dist" "speed"
    ##   .. .. .. ..$ : chr "speed"
    ##   .. ..- attr(*, "term.labels")= chr "speed"
    ##   .. ..- attr(*, "order")= int 1
    ##   .. ..- attr(*, "intercept")= int 1
    ##   .. ..- attr(*, "response")= int 1
    ##   .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
    ##   .. ..- attr(*, "predvars")= language list(dist, speed)
    ##   .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
    ##   .. .. ..- attr(*, "names")= chr [1:2] "dist" "speed"
    ##  $ model        :'data.frame':   50 obs. of  2 variables:
    ##   ..$ dist : num [1:50] 2 10 4 22 16 10 18 26 34 17 ...
    ##   ..$ speed: num [1:50] 4 4 7 7 8 9 10 10 10 11 ...
    ##   ..- attr(*, "terms")=Classes 'terms', 'formula'  language dist ~ speed
    ##   .. .. ..- attr(*, "variables")= language list(dist, speed)
    ##   .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
    ##   .. .. .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. .. .. ..$ : chr [1:2] "dist" "speed"
    ##   .. .. .. .. ..$ : chr "speed"
    ##   .. .. ..- attr(*, "term.labels")= chr "speed"
    ##   .. .. ..- attr(*, "order")= int 1
    ##   .. .. ..- attr(*, "intercept")= int 1
    ##   .. .. ..- attr(*, "response")= int 1
    ##   .. .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
    ##   .. .. ..- attr(*, "predvars")= language list(dist, speed)
    ##   .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
    ##   .. .. .. ..- attr(*, "names")= chr [1:2] "dist" "speed"
    ##  - attr(*, "class")= chr "lm"

``` r
summary(regression)$sigma
```

    ## [1] 15.37959

``` r
sqrt(sum(regression$residuals^2)/regression$df.residual)
```

    ## [1] 15.37959
