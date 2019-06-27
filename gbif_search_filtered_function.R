gbif_search_filtered <- function(taxa) {
  
  nbe_loc <- continents <- NA
  
  Issues_codes_excluded <-
    c("ccm","conti","cdiv","cdout","cdrepf","cdreps","cucdmis","gass84","preneglat","preneglon","preswcd","zerocd")
  
  key <- rgbif::name_backbone(name=taxa, kingdom='plants')$speciesKey
  if(!is.null(key)) {
    found_occ <-
      occ_search(taxonKey=key, limit=2000, basisOfRecord ="PRESERVED_SPECIMEN")
    found_occ <- found_occ[[3]]
  }else{
    found_occ <- NULL
  }
  
  if(!is.null(found_occ)) {
    for (i in Issues_codes_excluded) found_occ <-
        found_occ %>%
        filter(!grepl(i, issues))
    
    if(any(colnames(found_occ)=="decimalLatitude") & any(colnames(found_occ)=="decimalLongitude")) {
      found_occ <-
        found_occ %>%
        filter(!is.na(decimalLatitude), !is.na(decimalLongitude))
      
      if(nrow(found_occ)>0) {
        sp.foc.sf<-
          st_as_sf(dplyr::select(found_occ, decimalLongitude, decimalLatitude), coords = c("decimalLongitude", "decimalLatitude"), 
                   crs = 4326)
        
        sp.foc.sf_proj <- sf::st_transform(sp.foc.sf, 4088)
        
        sp.foc.sf_proj_coord <- sf::st_coordinates(sp.foc.sf_proj) %>% 
          as_tibble()
        
        crs_proj <- crs("+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +a=6371007 +b=6371007 +units=m +no_defs")
        
        res <- 
          ConR::.cell.occupied(crs_proj = crs_proj, 
                         coord = sp.foc.sf_proj_coord, size = 10, export_shp = FALSE)
        
        nbe_loc <- res[[2]]
        
        continents <- paste0(unique(found_occ$continent)[!is.na(unique(found_occ$continent))], collapse = ", ")
      }
    }
    nbe_occ_gbif=nrow(found_occ)
  }else{
    nbe_occ_gbif <- NA
  }
  
  return(tibble(tax_sp_level=taxa, nbe_occ_gbif=nbe_occ_gbif, nbe_loc_gbif=nbe_loc, list_continents_gbif=continents))
}