This document document R scripts and functions used for the analysis
presented in the manuscript entitled **A third of the tropical African
flora is potentially threatened with extinction** currently under
peer-review.

### Load packages (to be installed if needed)

    library(plyr)
    library(tidyverse)
    library(sf)
    library(raster)
    library(ConR)
    library(broom)
    library(rredlist)
    library(rgbif)
    library(doParallel)
    library(maps)

### load dataset

    dataset <- read_csv("data/dataset_rainbio_used.csv")

    dataset

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

### extracting data from gbif for all species

    species_list <-
      dataset %>%
      distinct(tax_sp_level)

    list_data <- as.list(pull(species_list))


    #### Searching for gbif occurrences for all species, takes around 5 hours in parallel on 4 cores.
    doParallel::registerDoParallel(4) ## define here the number of core on which you want to run the search

    system.time(results <-
      plyr::llply(list_data, .fun=function(x) {
        # source("functions_criteria_A.R")
        source("gbif_search_filtered_function.R")
      
        gbif_search_filtered(taxa = x)
      }
        , .progress = "text", .parallel=T, .paropts=list(.packages=c('rgbif','raster','tidyverse', 'ConR', 'sf'))))


    doParallel::stopImplicitCluster()

    gbif_data <- bind_rows(results)

    # write_csv(gbif_data, "gbif.searched.all.taxa.csv")

This table provide for each species the total number of occurence in
gbif, the total number of occupied cells in 10 km cell size grid and the
list of Continent where the species is recorded on gbif.

    ## # A tibble: 25,222 x 4
    ##    tax_sp_level               nbe_occ_gbif nbe_loc_gbif list_continents_gb~
    ##    <chr>                             <dbl>        <dbl> <chr>              
    ##  1 Eremospatha cuspidata                41           12 AFRICA             
    ##  2 Eremospatha dransfieldii              5            4 AFRICA             
    ##  3 Eremospatha haullevilleana           48           25 AFRICA             
    ##  4 Borassus aethiopum                   13            6 AFRICA             
    ##  5 Eremospatha hookeri                  11            6 AFRICA             
    ##  6 Eremospatha laurentii                30           11 AFRICA             
    ##  7 Eremospatha macrocarpa                5           NA <NA>               
    ##  8 Eremospatha quinquecostul~            7            4 AFRICA             
    ##  9 Eremospatha tessmanniana              0           NA <NA>               
    ## 10 Eremospatha wendlandiana             37           11 AFRICA             
    ## # ... with 25,212 more rows

### load SIG dataset

    ## protected areas network
    protected_areas_network <- sf::read_sf("data/NationalParks_filtered_corrected.shp")
    ## mineral deposits plus a buffer of 10 km around each point
    mineral_deposits <- sf::read_sf("data/poly_mineral_deposit.shp")
    raster_mayaux <- raster("data/sum.rasters.human.impacted.tif")

    ### getting a nice and updated world map
    world <- maps::map("world", fill=TRUE, plot=FALSE)
    IDs <- sapply(strsplit(world$names, ":"), function(x) x[1])
    world <- maptools::map2SpatialPolygons(world, IDs=IDs, proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
    suppressWarnings(world_map <- broom::tidy(world))
    world_id <- tibble(id_nbe=1:nrow(distinct(world_map, id)), id=pull(distinct(world_map, id)))

    world_map <- as(world, "sf")
    world_map <- world_map %>% 
      add_column(id_nbe=world_id$id_nbe, id=world_id$id)

### Preliminary Automated Conservation Assessment following Criterion B

    protected_areas_network_sp <- 
      as(protected_areas_network, "Spatial")

    criterion_b <-
      IUCN.eval(dplyr::select(dataset, ddlat, ddlon, tax_sp_level)[1:1000,], 
                protec.areas = protected_areas_network_sp, DrawMap = F, SubPop = F, file_name = "dataset_criterion_B", write_file_option = "csv", parallel = T, NbeCores = 4)

    ## # A tibble: 25,222 x 12
    ##    X1        EOO   AOO Nbe_unique_occ. Nbe_subPop Nbe_loc Nbe_loc_PA
    ##    <chr>   <dbl> <dbl>           <dbl> <lgl>        <dbl>      <dbl>
    ##  1 Aaro~ NA          4               1 NA               1          0
    ##  2 Abel~  4.19e4    20               5 NA               3          0
    ##  3 Abil~ NA          4               1 NA               1          0
    ##  4 Abil~  5.24e6    56              14 NA              13          1
    ##  5 Abru~  7.73e6   296              74 NA              70         10
    ##  6 Abru~  1.91e7   720             193 NA             171         22
    ##  7 Abru~ NA          8               2 NA               2          0
    ##  8 Abru~  1.40e7   536             137 NA             127         14
    ##  9 Abru~ NA          8               2 NA               2          0
    ## 10 Abut~  3.48e5    20               5 NA               5          0
    ## # ... with 25,212 more rows, and 5 more variables:
    ## #   Category_CriteriaB <chr>, Category_code <chr>,
    ## #   Ratio_occ_within_PA <dbl>, Category_AOO <chr>, Category_EOO <chr>

### Preliminary Automated Conservation Assessment following Criterion A

    source("criteria_A_function.R") 


    list_data <- split(dplyr::select(dataset, ddlat, ddlon, tax_sp_level), 
                       f =dataset$tax_sp_level)

    ### Take about 1 hour

    doParallel::registerDoParallel(4)

    system.time(Results <-
      plyr::llply(list_data, .fun=function(x) {

        source("criteria_A_function.R")
        sum.rasters.human.impacted <- raster("data/sum.rasters.human.impacted.tif")
        mineral_deposits <- sf::read_sf("data/poly_mineral_deposit.shp")
        protected_areas_network <- sf::read_sf("data/NationalParks_filtered_corrected.shp")
        
        IUCN_eval_CA(data = x, rasters = sum.rasters.human.impacted, mineral = mineral_deposits, protected_areas = protected_areas_network)
        
      }
        , .progress = "text", .parallel=T, .paropts=list(.packages=c('raster','tidyverse', 'sf'))))

    criterion_a<- bind_rows(Results)

    criterion_a <-
      criterion_a %>%
      mutate(AOO_without_impact=AOO_all-AOO_left) %>%
      mutate(AOO_decline=(AOO_all-AOO_without_impact)/AOO_all*100)

    criterion_a <- criterion_a %>%
      add_column(Category_code_CA=plyr::aaply(criterion_a$AOO_decline, 1, Cat_Crition_A))

    write_csv(criterion_a, "dataset_Criterion_A.csv")

    ## # A tibble: 25,222 x 11
    ##    tax_sp_level     N N_human_impacted N_mines N_impacted_total AOO_all
    ##    <chr>        <dbl>            <dbl>   <dbl>            <dbl>   <dbl>
    ##  1 Aaronsohnia~     1                0       0                0       4
    ##  2 Abelmoschus~     9                8       0                8      20
    ##  3 Abildgaardi~     1                1       0                1       4
    ##  4 Abildgaardi~    14                3       0                2      52
    ##  5 Abrus canes~    87               35       8               23     296
    ##  6 Abrus fruti~   198               56       6               53     716
    ##  7 Abrus gawen~     2                0       0                0       8
    ##  8 Abrus preca~   154               39       3               34     536
    ##  9 Abrus schim~     2                0       1                1       8
    ## 10 Abutilon an~     5                0       0                0      20
    ## # ... with 25,212 more rows, and 5 more variables: AOO_left <dbl>,
    ## #   nbe_occ_protected_area <dbl>, AOO_without_impact <dbl>,
    ## #   AOO_decline <dbl>, Category_code_CA <chr>
