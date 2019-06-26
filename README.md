Load packages (to install if needed)
====================================

    library(tidyverse)
    library(sf)
    library(raster)
    library(ConR)
    library(broom)
    library(rredlist)

Load functions specific to these analyses
=========================================

load dataset
============

    dataset <- read_csv("dataset_rainbio_used.csv")

    ## # A tibble: 590,241 x 12
    ##     idrb country  coly  colm kind_col tax_fam tax_sp_level tax_infra_sp_le~
    ##    <dbl> <chr>   <dbl> <dbl> <chr>    <chr>   <chr>        <chr>           
    ##  1  6881 Gabon    1992     8 herb     Arecac~ Eremospatha~ Eremospatha cus~
    ##  2  5528 Camero~  1995    10 herb     Arecac~ Eremospatha~ Eremospatha cus~
    ##  3  9178 Equato~  1998     3 herb     Arecac~ Eremospatha~ Eremospatha cus~
    ##  4  7007 Gabon    2003    11 Sili     Arecac~ Eremospatha~ Eremospatha cus~
    ##  5  6021 Gabon    1994    10 herb     Arecac~ Eremospatha~ Eremospatha cus~
    ##  6  7008 Gabon    2005     4 herb     Arecac~ Eremospatha~ Eremospatha cus~
    ##  7  9882 Zambia   1969     5 herb     Arecac~ Eremospatha~ Eremospatha cus~
    ##  8  5465 Angola   1937    NA herb     Arecac~ Eremospatha~ Eremospatha cus~
    ##  9  7522 Congo ~  1987     2 herb     Arecac~ Eremospatha~ Eremospatha cus~
    ## 10  8383 Sierra~  1952    NA herb     Arecac~ Eremospatha~ Eremospatha dra~
    ## # ... with 590,231 more rows, and 4 more variables: a_cultivated <lgl>,
    ## #   a_habit <chr>, ddlat <dbl>, ddlon <dbl>
