
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rescience - Setting up for Reproducible Science

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of rescience is to provide an easy framework to test different
preprocessing and machine learning methods while minimizing the risk of
overfitting.

## Installation

You can install the development version of rescience from
[GitHub](https://github.com/JohanLassen/rescience) with:

``` r
# install.packages("devtools")
# devtools::install_github("JohanLassen/rescience")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r

library(rescience)
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
#> ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
#> ✔ tibble  3.1.7     ✔ dplyr   1.0.9
#> ✔ tidyr   1.2.0     ✔ stringr 1.4.1
#> ✔ readr   2.1.2     ✔ forcats 0.5.1
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()

# dataset
head(pneumonia[,1:10])
#> # A tibble: 6 × 10
#>      id group     age gender weight height   BMI M363T419 M512T603 M364T419
#>   <dbl> <chr>   <dbl> <chr>   <dbl>  <dbl> <dbl>    <dbl>    <dbl>    <dbl>
#> 1     1 control    87 K          74    176  23.9   39264.   11245.    7083.
#> 2     2 control    48 K          52     NA  NA     17008.   22494.    6936.
#> 3     3 control    53 M          80    178  25.2    6923.   55520.    2573.
#> 4     4 control    27 M          67    171  22.9   23111.   71463.    5341.
#> 5     5 control    27 M          81    180  25     36683.   35476.   10148.
#> 6     6 control    49 M          NA    150  NA     24076.   34264.    5706.
```

To statistically analyze this data it must undergo (1) selection of
important variables including outcome, batch, and technical replicates,
(2) preprocessing to ensure that all features and compounds are within
the same range of values and to minimize batch effect, (3) perform a
machine learning model screening to evaluate which model fits the data
best.

Point 2 and 3 requires the analyst to be careful, read more here.

# The simple way to do it

The package is accompanied with a web app that ensures that a broad
range of users can confidently use the methods. The app exists as an
online version to show case the setup, but we recommend users to install
R, Rstudio, and this package to experience the best performance.

To run the app:

``` r
#run_app()
```

# Diving deeper

The true beauty of the package reveals itself when developing custom
scripts and implementing your own functions.

We strive to include as many different methods as possible to
accommodate methodological studies on batch effect, machine learning
algorithms, and model interpretation. Hence, it should be as easy as
possible to contribute to the package by writing functions that fits the
required data format.

For consistency we format data into a R list called ms. The ms list
contain the feature values
(ms$values) and sample meta data (ms$rowinfo).

# (1) Loading data

In the example of pneumonia we generate the ms the following way:

``` r

# First convert the pneumonia object to a tibble. 
pneumonia <- tibble(pneumonia)

# Generate list object
ms <- list()

# Assign feature values to ms$values
start_column <- 8 # The first column with feature values
end_column <- ncol(pneumonia) # The last column of the dataset
ms$values <- pneumonia[, start_column:end_column]

# Assign metadata to ms$rowinfo
ms$rowinfo <- pneumonia %>% select(rowid = id, group, age, gender, weight, height, BMI)
```

Now the ms object is made

| M363T419  | M512T603 | M364T419  | M365T392  | M186T177  |
|:---------:|:--------:|:---------:|:---------:|:---------:|
| 39263.557 | 11244.54 | 7082.998  | 54547.315 | 124593.65 |
| 17007.974 | 22493.90 | 6935.703  | 57963.620 | 76165.73  |
| 6923.392  | 55520.32 | 2572.867  | 5009.616  | 26871.19  |
| 23110.664 | 71463.46 | 5341.382  | 5785.511  | 64805.11  |
| 36682.675 | 35475.75 | 10147.962 | 10432.764 | 29821.71  |
| 24076.265 | 34263.70 | 5705.732  | 8523.413  | 12463.62  |

Feature values

| rowid |   group | age | gender | weight | height |   BMI |
|------:|--------:|----:|-------:|-------:|-------:|------:|
|     1 | control |  87 |      K |     74 |    176 | 23.89 |
|     2 | control |  48 |      K |     52 |     NA |    NA |
|     3 | control |  53 |      M |     80 |    178 | 25.25 |
|     4 | control |  27 |      M |     67 |    171 | 22.91 |
|     5 | control |  27 |      M |     81 |    180 | 25.00 |
|     6 | control |  49 |      M |     NA |    150 |    NA |

Meta Data

# Preprocessing

``` r

# fourth root transformation
ms <- transform_fourth_root(ms)

# Probabilistic quotient normalization
ms <- normalize_pqn(ms)

# Visualize distributions of first 10 compounds to see effect of preprocessing
ms$values[,1:5] %>% 
  pivot_longer(cols = everything(), names_to = "Feature") %>%
  mutate(Feature = as.factor(Feature)) %>% 
  ggplot(aes(x=value, fill = Feature)) +
  geom_histogram(position = "identity", alpha = 0.3)
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="man/figures/README-unnamed-chunk-6-1.png" alt="..." width="600px" height="300px" style="display: block; margin: auto;" />

And if we want to see the PCA plot:

``` r

# We plot by rowid as the data doesn't have batch info, but the rowid reflects the injection order
plot_pca(ms, color_label = "rowid") 
```

<img src="man/figures/README-unnamed-chunk-7-1.png" alt="..." width="1000px" height="300px" style="display: block; margin: auto;" />

# Machine learning

## Fitting multiple models

``` r

fits <- fit_models(x = ms$values, y=ms$rowinfo$group, methods = c("pls", "glmnet")) #PLS, Elastic net, Random Forest
```

## Obtaining the performance from the models

``` r

performance <- get_performance(fits)

knitr::kable(performance)
```

| method | rep  |  accuracy |       mcc |
|:-------|:-----|----------:|----------:|
| glmnet | Rep1 | 0.7530253 | 0.4953219 |
| glmnet | Rep2 | 0.7621012 | 0.5144059 |
| pls    | Rep1 | 0.6782178 | 0.3431979 |
| pls    | Rep2 | 0.6666667 | 0.3212393 |

## Plotting performance

``` r
plot_performance(fits)
```

<img src="man/figures/README-unnamed-chunk-10-1.png" alt="..." width="700px" height="300px" style="display: block; margin: auto;" />
