

.AOO.estimation <- function(coordEAC, Cell_size_AOO=2, nbe.rep.rast.AOO=NULL) {
  
  
  coordEAC <- st_transform(coordEAC, crs = 3410)
  # Corners <- rbind(c(min(st_coordinates(coordEAC)[,1]), max(st_coordinates(coordEAC)[,1])), c(min(st_coordinates(coordEAC)[,2]), max(st_coordinates(coordEAC)[,2])))
  
  Corners <- st_bbox(coordEAC)
  
  ## if nbe.rep.rast.AOO is not provided, translations of 1/4 of resolution for varying the position of the raster
  if(is.null(nbe.rep.rast.AOO)) {
    Occupied_cells <- c()
    decal <- c(0,1,2,3)
    for (h in decal) {
      ext = extent(floor(Corners[1])-h*(Cell_size_AOO*1000/4)-2*Cell_size_AOO*1000, floor(Corners[3])+h*(Cell_size_AOO*1000/4)+2*Cell_size_AOO*1000, 
                   floor(Corners[2])-h*(Cell_size_AOO*1000/4)-2*Cell_size_AOO*1000, floor(Corners[4])+h*(Cell_size_AOO*1000/4)+2*Cell_size_AOO*1000)
      r = raster(ext, resolution=Cell_size_AOO*1000,crs="+proj=longlat +datum=WGS84 +no_defs")
      r2_AOO <- rasterize(st_coordinates(coordEAC), r)
      OCC <- length(which(!is.na(values(r2_AOO))))
      Occupied_cells <- c(Occupied_cells, OCC)
      
      ### If only one occupied cell, stop the production of raster
      if(OCC==1) break
    }
    h <- decal[which.min(Occupied_cells)]
    Occupied_cells <- min(Occupied_cells)
  }
  
  ## if nbe.rep.rast.AOO is provided, random starting position of the raster
  if(!is.null(nbe.rep.rast.AOO)) {
    Occupied_cells <- c()
    # rd.1.vec <- c()
    # rd.2.vec <- c()
    for (h in 1:nbe.rep.rast.AOO) {
      rd.1 <- runif(1)*Cell_size_AOO*1000
      rd.2 <- runif(1)*Cell_size_AOO*1000
      
      ext = extent(floor(Corners[1])-h*(Cell_size_AOO*1000/4)-2*Cell_size_AOO*1000, floor(Corners[3])+h*(Cell_size_AOO*1000/4)+2*Cell_size_AOO*1000, 
                   floor(Corners[2])-h*(Cell_size_AOO*1000/4)-2*Cell_size_AOO*1000, floor(Corners[4])+h*(Cell_size_AOO*1000/4)+2*Cell_size_AOO*1000)
      r = raster(ext, resolution=Cell_size_AOO*1000, crs="+proj=longlat +datum=WGS84 +no_defs")
      r
      r2_AOO <- rasterize(st_coordinates(coordEAC), r)
      OCC <- length(which(!is.na(values(r2_AOO))))
      Occupied_cells <- c(Occupied_cells, OCC)
      # rd.1.vec <- c(rd.1.vec, rd.1)
      # rd.2.vec <- c(rd.2.vec, rd.2)
      if(OCC==1) break
    }
  }
  h <- decal[which.min(Occupied_cells)]
  Occupied_cells <- min(Occupied_cells)
  
  AOO <- Occupied_cells*Cell_size_AOO*Cell_size_AOO  ### AOO
  return(c(AOO, Occupied_cells))
}


IUCN_eval_CA <- function(data, rasters, mineral, protected_areas) {
  data <-
    data %>%
    add_column(ID=1:nrow(data))
  
  extract.rasts.hum.impacted <-
    raster::extract(rasters, dplyr::select(data, ddlon, ddlat))
  
  sp.foc.sf<-
    st_as_sf(data, coords = c("ddlon", "ddlat"), 
             crs = 4326)
  
  suppressMessages(suppressWarnings(Intersect <- st_intersection(sp.foc.sf, mineral)))
  
  suppressMessages(suppressWarnings(Intersect_protected_areas <- st_intersection(sp.foc.sf, protected_areas)))
  
  ### ID of all points within protected areas
  ID.occ.protected.areas <- Intersect_protected_areas$ID
  
  ### ID of all points impacted
  ID.occ.impacted <- unique(c(data$ID[which(extract.rasts.hum.impacted>0.5)], Intersect$ID))
  
  
  ### AOO of all occurrences
  AOO.all <- .AOO.estimation(coordEAC=sp.foc.sf, Cell_size_AOO = 2, nbe.rep.rast.AOO = NULL)[1]
  
  if(length(ID.occ.impacted)>0) {
    
    sp.foc.sf.left <-
      st_as_sf(filter(data, ID %in% ID.occ.impacted), coords = c("ddlon", "ddlat"), 
               crs = 4326)
    
    AOO.left <- .AOO.estimation(sp.foc.sf.left, Cell_size_AOO = 2, nbe.rep.rast.AOO = NULL)[1]
  }else{
    AOO.left <- 0
  }
  
  
  return(tibble(tax_sp_level=data$tax_sp_level[1], 
                N=nrow(data), 
                N_human_impacted=sum(extract.rasts.hum.impacted>0.5), 
                N_mines=nrow(Intersect),
                N_impacted_total=length(ID.occ.impacted),
                AOO_all=AOO.all,
                AOO_left=AOO.left,
                nbe_occ_protected_area=length(ID.occ.protected.areas)))
  
}



Cat_Crition_A <- function(x,...) {
  cat <- NA
  if(x>=80) cat <- "CR"
  if(x<80 & x>=50) cat <- "EN"
  if(x<50 & x>=30) cat <- "VU"
  if(x<30) cat <- "LC or NT"
  return(cat)
}
