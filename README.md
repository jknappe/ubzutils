
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ubzutils

<!-- badges: start -->

<!-- badges: end -->

The goal of ubzutils is to provide useful function for data processing
in R for people working at the Center for Environmental and
Buitechnology (UBZ) at the Helmholtz Center for Environmental Research
(UFZ) in Leipzig, Germany.

## Installation

You can install the most current development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jknappe/ubzutils")
```

To load the package use:

``` r
library(ubzutils)
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: lubridate
#> 
#> Attaching package: 'lubridate'
#> The following objects are masked from 'package:base':
#> 
#>     date, intersect, setdiff, union
#> Loading required package: magrittr
#> Loading required package: plyr
#> ------------------------------------------------------------------------------
#> You have loaded plyr after dplyr - this is likely to cause problems.
#> If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
#> library(plyr); library(dplyr)
#> ------------------------------------------------------------------------------
#> 
#> Attaching package: 'plyr'
#> The following objects are masked from 'package:dplyr':
#> 
#>     arrange, count, desc, failwith, id, mutate, rename, summarise,
#>     summarize
#> Loading required package: RCurl
#> Loading required package: stringr
#> Loading required package: tibble
#> Loading required package: tidyr
#> 
#> Attaching package: 'tidyr'
#> The following object is masked from 'package:RCurl':
#> 
#>     complete
#> The following object is masked from 'package:magrittr':
#> 
#>     extract
```

## Function Families

This is an overview of the function families currently available within
the package:

### Carport Utility Functions

These functions are related to processing data from the carport green
roof. All functions in this family start with the `cp_` prefix and
include

| Function              | Short Description                                                                         |
| --------------------- | ----------------------------------------------------------------------------------------- |
| `cp_pull_loadcells()` | copies carport load cell data files as is from UFZ SFTP server to local directory.        |
| `cp_tidy_loadcells()` | cleans the data, converts them into tidy format and saves them as uncompressed .rds file. |
| `cp_load_loadcells()` | loads all available carport load cell data from the .rds file into your R environment.    |

Please refer to each individual function’s documentation for a more
detailed description of use cases, the arguments and outputs.

The functions are designed to work in a pipe, e.g. as:

``` r
loadcell_data <-
    cp_pull_loadcells(save_to = "/data/load-cells",
                      ssh_key = "/ssh-vps/carport-openssh-vps.ppk",
                      only_new = TRUE) %>%
    cp_tidy_loadcells() %>%
    cp_load_loadcells()
```

### Helper Functions

These functions are internal helper functions for the package and might
be exported (can be used in the R environment) if potentially useful for
other use cases outside of this package.

| Function      | Status   | Short Description                             |
| ------------- | -------- | --------------------------------------------- |
| `is_error()`  | exported | checks if a function returns an error or not. |
| `key_valid()` | hidden   | tests if the provided SSH key is valid.       |

Please refer to each individual function’s documentation for a more
detailed description of use cases, the arguments and outputs.
