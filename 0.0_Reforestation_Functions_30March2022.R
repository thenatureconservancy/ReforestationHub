###
# Functions for TNC reforestation analysis

buffer.function <- function(raster_input){
  # Convert raster to a matrix
  # Can do this directly with matrix(raster_input), but R is really slow when converting rasters into a matrix
  tmp_mat <- matrix(getValues(raster_input),
                    nrow = raster_input@nrows, 
                    ncol = raster_input@ncols, 
                    byrow = FALSE)
  # Replace NAs with 0s to allow convolution
  tmp_mat[is.na(tmp_mat)] <- 0
  # Run the convolution
  tmp_mat <- convolution_fun(tmp_mat)
  #Converting 0s to NAs
  tmp_mat[tmp_mat%in%0]<-NA
  tmp_mat[!is.na(tmp_mat)]<-1
  # Put data in and put NAs back where they should be
  out_raster <- raster(tmp_mat, crs = crs(raster_input))
  extent(out_raster) <- extent(raster_input)
  
  
  return(m)
}


convolution_fun <- function(m){
  m_rows <- nrow(m)
  m_cols <- ncol(m)
  m_tmp <- rbind(matrix(0, nrow = 1, ncol = m_cols), m)[-(m_rows + 1),]
  m_tmp <- m_tmp + 
    rbind(matrix(0, nrow = 1, ncol = m_cols),
          cbind(m, matrix(0, nrow = m_rows, ncol = 1))[,-1])[-(m_rows + 1),]
  m_tmp <- m_tmp + 
    cbind(m, matrix(0, nrow = m_rows, ncol = 1))[,-1]
  m_tmp <- m_tmp + 
    rbind(cbind(m, matrix(0, nrow = m_rows, ncol = 1))[,-1],
          matrix(0, nrow = 1, ncol = m_cols))[-1,]
  m_tmp <- m_tmp + 
    rbind(m, matrix(0, nrow = 1, ncol = m_cols))[-1,]
  m_tmp <- m_tmp + 
    rbind(cbind(matrix(0, nrow = m_rows, ncol = 1), m)[,-(m_cols + 1)],
          matrix(0, nrow = 1, ncol = m_cols))[-1,]
  m_tmp <- m_tmp + cbind(matrix(0, nrow = m_rows, ncol = 1), m)[,-(m_cols + 1)]
  m_tmp <- m_tmp + 
    rbind(matrix(0, nrow = 1, ncol = m_cols),
          cbind(matrix(0, nrow = m_rows, ncol = 1), m)[,-(m_cols + 1)])[-(m_rows + 1),]
  return(m_tmp)
}


# Identifying current forests from nlcd data
# 41-43 = forest
excluded.regions <- 
  function(raster_input) {
    tmp.vector <- getValues(raster_input)
    tmp.vector[!(tmp.vector %in% c(11,12, # open water (11) or Perennial snow and ice (12)
                                   90,95, # Wetland
                                   22,23,24, # Urban lands that aren't open space
                                   31, # Barren
                                   82))] <- NA # Crop
    tmp.vector[!is.na(tmp.vector)] <- 1
    
    out.raster <- raster(matrix(tmp.vector, nrow = raster_input@nrows, byrow = TRUE), crs = crs(raster_input))
    extent(out.raster) <- extent(raster_input)
    return(out.raster)
  }


# Identifying potential forests
# 41-43 = forest
potential.forests <- 
  function(raster_input) {
    tmp.vector <- getValues(raster_input)
    tmp.vector[tmp.vector %in% c(11,12,31)] <- -1 # Identifying potential barren/lake/perennial ice regions
    tmp.vector[!(tmp.vector %in% bps.values$VALUE)] <- NA
    tmp.vector[!is.na(tmp.vector)] <- 1
    
    out.raster <- raster(matrix(tmp.vector, nrow = raster_input@nrows, byrow = TRUE), crs = crs(raster_input))
    extent(out.raster) <- extent(raster_input)
    return(out.raster)
  }


# Getting forest as per nafd
current.forests <-
  function(raster_input) {
    tmp.vector <- getValues(raster_input)
    tmp.vector[!(tmp.vector %in% 3)] <- NA # 3 indicates forest, changing everything that is not a 3 to NA
    tmp.vector[!is.na(tmp.vector)] <- 1 # Changing all positive values to 1
    
    out.raster <- raster(matrix(tmp.vector, nrow = raster_input@nrows, byrow = TRUE), crs = crs(raster_input))
    extent(out.raster) <- extent(raster_input)
    return(out.raster)
  }


# Getting current forest as per nlcd
current.forests.nlcd <-
  function(raster_input) {
    tmp.vector <- getValues(raster_input)
    tmp.vector[!(tmp.vector %in% c(41,42,43))] <- NA # 41-43 indicates forest, changing everything that is not a forest to NA
    # Where:
    # 41 = deciduous forest
    # 42 = evergreen forest
    # 43 = mixed forest

    tmp.vector[!is.na(tmp.vector)] <- 1 # Changing all positive values to 1

    out.raster <- raster(matrix(tmp.vector, nrow = raster_input@nrows, byrow = TRUE), crs = crs(raster_input))
    extent(out.raster) <- extent(raster_input)
    return(out.raster)
  }


# Getting opportunity in locations of conncetivity
connectivity.options <-
  function(raster_input) {
    tmp.vector <- getValues(raster_input)
    tmp.vector[!(tmp.vector %in% 3)] <- NA
    tmp.vector[!is.na(tmp.vector)] <- 1
    
    out.raster <- raster(matrix(tmp.vector, nrow = raster_input@nrows, byrow = TRUE), crs = crs(raster_input))
    extent(out.raster) <- extent(raster_input)
    return(out.raster)
  }


# function to get extent of flood tiles
get.flood.extents <- 
  function(flood.tiles) {
    out.df <- data.frame(tile_path = flood.tiles %>% .[!grepl('.aux',.)],
                         tile_extent = NA)
    for(i in 1:nrow(out.df)) {
      tmp.extent <- extent(raster(out.df$tile_path[i]))
      out.df$tile_extent[i] <- list(tmp.extent)
      out.df$xmin[i] <- tmp.extent[1]
      out.df$xmax[i] <- tmp.extent[2]
      out.df$ymin[i] <- tmp.extent[3]
      out.df$ymax[i] <- tmp.extent[4]
    }
    return(out.df)
  }


# And function for getting overlaps
get.flood.overlaps <-
  function(flood.extent) {
    flood.extent.new <- extent(flood.extent) + c(-1,1,-1,1)
    
    overlaps.out <-
      flood.extents %>% 
      filter(xmin >= flood.extent.new[1] &
               xmax <= flood.extent.new[2] &
               ymin >= flood.extent.new[3] &
               ymax <= flood.extent.new[4])
    
    return(overlaps.out)
  }

# And merging flood tiles
merge.flood.tiles <-
  function(raster.list) {
    raster.list.out <- list()
    for(i in 1:length(raster.list)) {
      raster.list.out[[i]] <- raster(raster.list[i])
    }
    # And merging
    merged.raster <- do.call(raster::merge,raster.list.out)#, fun = 'mean',na.rm=TRUE)
  }

# Alternative option
merge.flood.tiles.matrix <-
  function(raster.list) {
    # number of x tiles
    x.tiles <- sort(unique(flood.extent.overlaps$xmin))
    # number of y tiles
    y.tiles <- rev(sort(unique(flood.extent.overlaps$ymax)))
    
    # Mosaicing
    tmp.flood.extents <- 
      flood.extent.overlaps %>%
      arrange(xmin, ymax)
    
    # Getting number of rows and columns in out matrix
    n.rows <- raster(tmp.flood.extents$tile_path[1])@nrows
    n.cols <- raster(tmp.flood.extents$tile_path[1])@ncols
    
    # Creating empty matrix
    out.matrix <- matrix(NA, nrow = n.rows*length(y.tiles), ncol = n.cols*length(x.tiles))
    
    # And adding data in
    for(i in 1:nrow(tmp.flood.extents)) {
      row.min <- (1 + (which(y.tiles %in% flood.extents$ymax[i]) - 1) * n.cols)
      row.max <- which(y.tiles %in% tmp.flood.extents$ymax[i]) * n.cols
      col.min <- (1 + (which(x.tiles %in% tmp.flood.extents$xmin[i]) - 1) * n.cols)
      col.max <- which(x.tiles %in% tmp.flood.extents$xmin[i]) * n.cols
      
      out.matrix[row.min:row.max,
                 col.min:col.max] <-
        matrix(raster(tmp.flood.extents$tile_path[i]),byrow=TRUE,nrow = n.rows)
      # Plotting to check layout is correct
      # plot(raster(matrix(raster(tmp.flood.extents$tile_path[i]),nrow = n.rows, byrow = TRUE)))
    }
    
    # And mosaicing in a matrix
    out.raster <- raster(out.matrix, crs = crs(raster(tmp.flood.extents$tile_path[i])))
    extent(out.raster) <- c(min(tmp.flood.extents$xmin), max(tmp.flood.extents$xmax),
                            min(tmp.flood.extents$ymin), max(tmp.flood.extents$ymax))
    
    rm(out.matrix)
    return(out.raster)
    # For checking
    # plot(out.raster)
    
  }


###
# Function to identify connectivity classes
connectivity.function <-
  function(raster.input) {
    # Converting non connectivity opportunity classes to 0s
    raster.input[!(raster.input %in% as.numeric(connectivity.df$value[connectivity.df$keep %in% TRUE]))] <- 0
    # raster.input[is.na(raster.input)] <- 0
    # Converting connectivity opportunities to 1
    raster.input[(raster.input %in% as.numeric(connectivity.df$value[connectivity.df$keep %in% TRUE]))] <- 1
    # And returning the raster
    return(raster.input)
  }


###
# function to pair with tree cover type in the c afforestation assessments
# tree.match.function <- 
#   function(bps.values, afforestation.values) {
#     # Copying data set
#     out.dat <-
#       bps.values
#     
#     # Using search terms
#     for(i in k)
#     
#   }


### 
# Getting raster of wilderness areas
wilderness.function <- 
  function(raster.tile) {
    # Wilderness = land manager numbers of...
    #
    #
    #
    #
    #
    #
    #
  }






###
# Function to get year of last burn
recent.burn.function <- function(burn.list) {
  burn.list <- rev(sort(burn.list))
  
  for(i in burn.list) {
    # Importing raster and cropping it
    tmp.raster <- raster(i)
    tmp.raster <- crop(tmp.raster, projectExtent(bps.tile, crs(tmp.raster)))
    # Getting vector of values
    tmp.vector <- getValues(tmp.raster)
    # And only one to keep values of 1
    tmp.vector[!is.na(tmp.vector)] <- as.numeric(gsub(".*LBA_CU_","",i) %>% substr(start = 1, stop = 4))
    
    if(i %in% burn.list[1]) {
      # Vector to save
      tmp.vector.keep <- tmp.vector
    } else {
      tmp.vector.keep[!is.na(tmp.vector) & is.na(tmp.vector.keep)] <-
        tmp.vector[!is.na(tmp.vector) & is.na(tmp.vector.keep)]
    }
  }
  # Sticking it back into a map
  out.raster <- raster(matrix(tmp.vector.keep, nrow = tmp.raster@nrows, ncol = tmp.raster@ncols, byrow = TRUE), crs = crs(tmp.raster))
  extent(out.raster) <- extent(tmp.raster)
  # And reprojecting
  out.raster <- resample(out.raster, bps.tile)
  # Returning raster tile
  return(out.raster)
  
}



### 
# Function to get c sequestration
sequestration.rates <-
  function(raster_input) {
    
    #if(nrow(tmp.forest) > 1) {
    # Using counties to get region crosswalk
    # Getting county ids
    county.values <- getValues(county.tile)
    county.values.unique <- unique(county.values)
    # Vector to get out
    tmp.vector.out <- rep(NA, length(county.values))
    # Getting regions based off these
    regions <- unique(state.counties.db$USFS_region[state.counties.db$county_id %in% unique(county.values)])
    
    for(r in regions) {
      # Counties in the correct region
      tmp.county.values <- 
        state.counties.db$county_id[state.counties.db$USFS_region %in% r] %>%
        .[. %in% county.values.unique]
      # Converting to vector - this is faster in R
      tmp.vector <- getValues(raster_input)
      
      # Getting bps values that are forests
      forest.values <- sort(unique(tmp.vector[tmp.vector %in% bps.values$VALUE &
                                                county.values %in% tmp.county.values]))
      
      
      # Now looping through forest values
      # And looping through these to update
      for(i in forest.values) {
        # Forest type
        tmp.forest <- bps.values %>% filter(VALUE %in% i)
        seq.rate <- c.seq.rates$total_seq[c.seq.rates$Region_code %in% r & # Matching the region
                                            c.seq.rates$CLASS_NAME %in% unique(tmp.forest$CLASS_NAME) & # Matching forest type
                                            c.seq.rates$Age %in% afforestation.duration] # Matching age duration
        
        if(length(seq.rate) %in% 0) {
          seq.rate <- mean(tmp.forest$total_seq)
        }
        tmp.vector.out[tmp.vector %in% i & # In the bps type
                         county.values %in% state.counties.db$county_id[state.counties.db$USFS_region %in% r]]  <- # Is the county in the correct forest region
          seq.rate
      }  # End of bps type loop
      
    } # End of region raster
    
    # Converting back to a raster
    #
    out.raster <- raster(matrix(tmp.vector.out, nrow = raster_input@nrows, byrow = TRUE), crs = crs(raster_input))
    extent(out.raster) <- extent(raster_input)
    return(out.raster)
  } # End of function

# potential.forests <- 
#   function(raster_input) {
#     tmp.vector <- getValues(raster_input)
#     tmp.vector[!(tmp.vector %in% bps.values$VALUE)] <- NA
#     tmp.vector[!is.na(tmp.vector)] <- 1
#     
#     out.raster <- raster(matrix(tmp.vector, nrow = raster_input@nrows, byrow = TRUE), crs = crs(raster_input))
#     extent(out.raster) <- extent(raster_input)
#     return(out.raster)
#   }



###
# Getting age of burn
burn.function <-
  function(burn.tile) {
    burn.tile[burn.tile >= (2019-burn_age)] <- NA
    burn.tile[is.na(burn.tile)] <- 1
    return(burn.tile)
  }



# Summing by opportunity class
# And summing
opp.sum.function <-
  function(opp.class) {
    # Data frame to save
    out.df <- data.frame()
    
    # Lopping through counties
    county.loop <- unique(opp.df$county)
    
    for(county in county.loop) {
      # Limiting to the target county
      tmp.county <- which(opp.df$county %in% county)
      tmp.df.out <- opp.df[tmp.county,]
      # Summing by opportunity class
      tmp.df.out <- 
        rowsum(tmp.df.out[,c(opp.class,'seq_rates','count')], 
               group = tmp.df.out[,opp.class], na.rm = TRUE)
      # Getting back to correct value
      # rowsums is annoying like this
      tmp.df.out[,opp.class] <- tmp.df.out[,opp.class]/tmp.df.out$count
      # Adding county identifier back in
      tmp.df.out$county <- county
      # And adding to data frame that will be saved
      out.df <- rbind(out.df, tmp.df.out)
    } # End of county loop
    # Converting to data frame
    out.df <- as.data.frame(out.df)
    
    # Converting from 30x30m cells to hectares (e.g. multiplying by 900/10000)
    # And changing column names
    out.df <-
      out.df %>%
      mutate(seq_rates = seq_rates * (900 / 10000),
             count = count * (900 / 10000)) %>%
      dplyr::rename(tot_sequestration_tonnes_c = seq_rates,
                    tot_opp_area_hectares = count)
    
    # MErging county and state name
    out.df <-
      left_join(out.df, 
                states.counties.db %>% dplyr::select(county = county_id, State = NAME, County = COUNTY_NAME))
    
    # Doing translation for land manager and land covers
    if(opp.class %in% 'land_owner') {
      # Separating out by manager where:
      # 1 = Federal
      # 2 = State
      # 3 = Private
      # 4 = USFS
      # 5 = BLM
      # 6 = Other
      # 7 = not classified
      out.df <-
        out.df %>%
        mutate(land_owner = ifelse(land_owner %in% 1, 'federal',
                                   ifelse(land_owner %in% 2, 'state',
                                          ifelse(land_owner %in% 3, 'private',
                                                 ifelse(land_owner %in% 4, 'usfs',
                                                        ifelse(land_owner %in% 5, 'blm',
                                                               ifelse(land_owner %in% 6, 'other',
                                                                      ifelse(land_owner %in% 7, 'not_classified', NA))))))))
      
    }
    
    if(opp.class %in% 'current_land_cover') {
      # Separating by land cover
      # 1 = pasture
      # 2 = crop
      # 3 = forest
      # 4 = shrublands
      # 5 = grassland
      # 6 = urban open space
      
      out.df <-
        out.df %>%
        mutate(current_land_cover = ifelse(current_land_cover %in% 1, 'pasture',
                                           ifelse(current_land_cover %in% 2, 'challengingcrop',
                                                  ifelse(current_land_cover %in% 3, 'forest',
                                                         ifelse(current_land_cover %in% 4, 'shrub',
                                                                ifelse(current_land_cover %in% 5, 'grassland',
                                                                       ifelse(current_land_cover %in% 6, 'urbanopenspace', NA)))))))
    }
    
    # Saving data frame
    # return(out.df) # - Leaving here in case you want to see outputs
    write.csv(out.df,
              paste0(getwd(),'/CSV_Outputs/Reforestation_opp_by_',opp.class,'_tile_',x,'.csv'),
              row.names = FALSE)
  }


### Creating rasters by opportunity class
opp.raster.function <- 
  function(opp.list) {
    
    for(opp in opp.list) {
      tmp.raster <- opp.list[[opp]]
      tmp.raster[!is.na(forest.opps)] <- NA 
      writeRaster(tmp.raster,
                  paste0(getwd(),'/Raster_Outputs/',
                         names(opp.list[[opp]]),
                         '_tile_', 
                         x, 
                         '.tif'))
    }
  }



### Crosswalk between pad object id and manager
pad.manager.function <- 
  function(raster.input) {
    # List of ids
    id.list <- sort(unique(getValues(raster.input)))
    # Limiting pad manager crosswalk to objects only in this tile
    tmp.df <-
      pad.crosswalk %>%
      filter(OBJECTI %in% id.list) %>%
      dplyr::select(OBJECTI, Mng_Type_Use, Mng_fill) %>%
      unique()
    # List of managers
    man.list <- unique(tmp.df$Mng_Type_Use)
    # And looping through list of managers to update values in pad tile
    for(m in man.list) {
      # Updating tile with manager fill
      raster.input[raster.input %in% tmp.df$OBJECTI[tmp.df$Mng_Type_Use %in% m]] <- # If tile object id in 
        unique(tmp.df$Mng_fill[tmp.df$Mng_Type_Use %in% m])
    }
    # And getting rid of other values
    raster.input[raster.input>6]<- NA
    
    # And returning
    return(raster.input)
  }

### Crosswalk between pad object id and owner type
pad.ownertype.function <-
  function(raster.input) {
    # List of ids
    id.list <- sort(unique(getValues(raster.input)))
    # Limiting pad manager crosswalk to objects only in this tile
    tmp.df <-
      pad.crosswalk %>%
      filter(OBJECTI %in% id.list) %>%
      dplyr::select(OBJECTI, Own_Typ, Own_fill) %>%
      unique()
    # List of managers
    man.list <- unique(tmp.df$Own_Typ)
    # And looping through list of managers to update values in pad tile
    for(m in man.list) {
      # Updating tile with manager fill
      raster.input[raster.input %in% tmp.df$OBJECTI[tmp.df$Own_Typ %in% m]] <- # If tile object id in 
        unique(tmp.df$Own_fill[tmp.df$Own_Typ %in% m])
    }
    # And getting rid of other values
    raster.input[raster.input>9999]<- NA

    # And returning
    return(raster.input)
  }

### Crosswalk between pad object id and owner name
pad.ownername.function <-
  function(raster.input) {
    # List of ids
    id.list <- sort(unique(getValues(raster.input)))
    # Limiting pad manager crosswalk to objects only in this tile
    tmp.df <-
      pad.crosswalk %>%
      filter(OBJECTI %in% id.list) %>%
      dplyr::select(OBJECTI, Own_name_use, Own_name_fill) %>%
      unique()
    # List of managers
    man.list <- unique(tmp.df$Own_name_use)
    # And looping through list of managers to update values in pad tile
    for(m in man.list) {
      # Updating tile with manager fill
      raster.input[raster.input %in% tmp.df$OBJECTI[tmp.df$Own_name_use %in% m]] <- # If tile object id in 
        unique(tmp.df$Own_name_fill[tmp.df$Own_name_use %in% m])
    }
    # And getting rid of other values
    raster.input[raster.input>9999]<- NA

    # And returning
    return(raster.input)
  }


###
# Function to identifyburn opportunities
burn.age.bps <- 
  function(raster.input) {
    # List of bps values
    bps.values.burn <- sort(unique(getValues(bps.tile)))
    # Burn age on these
    tmp.bps.values <-
      bps.values %>%
      filter(VALUE %in% bps.values.burn) %>%
      filter(!(VALUE %in% c(11,31))) # Not barren or ice
    
    # List of unique burn values
    tmp.burn.ages <- sort(unique(tmp.bps.values$AgeMin))
    
    # If not potential forest, then don't need to worry about burn age
    raster.input[is.na(potential.forest)] <- NA
    
    # Looping through burn age
    # End of loop is a raster with minimum age until land cover is back in forest
    for(a in tmp.burn.ages) {
      raster.input[raster.input %in% unique(tmp.bps.values$VALUE[tmp.bps.values$AgeMin %in% a])] <- a
    }
    
    # And is not urban open space, crop, or pasture
    raster.input[nlcd.tile %in% c(21,81,82)] <- NA
    
    # Min year for recovery
    # This is the year since which a cell cannot be forested to be considered a post burn opportunity
    # Subtracting from 2010, because 2010 is baseline year for NAFD forest data
    raster.input <- 2010 - raster.input
    
    # And returning raster
    return(raster.input)
  }


### 
# Function to save rasters that aren't the land opp class or the land manager opp class
# Functions for those are later
save.raster.function <-
  function(df) {
    # Looping through data frame
    for(n in 1:nrow(df)) {
      # Map of forest opportunities
      tmp.raster <- forest.opps
      # Map of opportunity class
      tmp.exclude <- get(df$r_object[n])
      # Excluded cells that aren't in opportunity class
      tmp.raster[is.na(tmp.exclude) | tmp.exclude %in% 0] <- NA
      # Sequestration of all forest opportunitites
      tmp.raster.seq <- seq.tile
      # And excluding cells that aren't in opportunity class
      tmp.raster.seq[is.na(tmp.raster)] <- NA
      
      # Saving raster
      writeRaster(tmp.raster.seq,
                  paste0(getwd(),'/Raster_Outputs/',df$opp_class[n],'tile_',x,'.tif'),
                  overwrite = TRUE)
    }
  }


### Function to split NLCD data into multiple layers for opportunity classes.
save.land.cov.function <-
  function(raster.input) {
    # Looping through land cover classes
    ### Current land cover
    # Important classes
    # 11 = open water
    # 12 = pernnial ice/snow
    # 22 = urban, low intensity
    # 23 = urban, medium intensity
    # 24 = urban high intensity
    # 21 = urban, open space
    # 31 = barren
    # 41-43 = forest
    # 51-52 = shrublands
    # 71-74 = herbaceous; 71 = grassland; 72 = sedge; 73 = lichens; 74 = moss
    # 81-82 = planted; 81 = pasture/hay; 82 = cultivated crops
    # 90 = woody wetlands
    # 95 = emergant herbaceous wetlands
    
    land.cov.list <-
      list(c(81), # pasture
           c(82), # crop
           c(41:43), # Forest
           c(51:52), # Shrublands
           c(71:74), # Grassland
           c(21)) # Urban open space
    land.cov.names <- c('pasture','challengingcrop','forest','shrub','grassland','urbanopenspace')
    
    # Making raster brick
    for(i in 1:length(land.cov.list)) {
      out.raster <- raster.input
      out.raster[!(raster.input %in% land.cov.list[[i]])] <- NA # Exclude cells not in target land cover
      out.raster[is.na(forest.opps)] <- NA # Excluding non reforestation opportunities
      seq.raster <- seq.tile
      seq.raster[is.na(out.raster)] <- NA
      if(i %in% 1) {
        out.brick <- stack(seq.raster)
      } else {
        out.brick <- stack(out.brick,seq.raster)
      }
      writeRaster(out.raster,
                  paste0(getwd(),'/Raster_Outputs/',land.cov.names[i],'_tile_',x,'.tif'),
                  overwrite = TRUE)
    }
    # Updating names
    names(out.brick) <- land.cov.names
    
    # Returning raster brick
    return(out.brick)
  }


### Function to split NLCD data into multiple layers for opportunity classes.
save.land.man.function <-
  function(raster.input) {
    # Looping through land cover classes
    ### Current manager classes
    # 1 = Federal
    # 2 = State
    # 3 = Private
    # 4 = USFS
    # 5 = BLM
    # 6 = Other
    
    land.man.list <-
      list(c(1), # federal
           c(2), # state
           c(3), # private
           c(4), # usfs
           c(5), # blm
           c(6),# other
           7) # Unclassified
    land.man.names <- c('federal','state','private','usfs','blm','other', 'notclassified')
    
    # Making raster brick
    for(i in 1:length(land.man.list)) {
      out.raster <- raster.input
      out.raster[!(raster.input %in% land.man.list[[i]])] <- NA # Exclude cells not in target land cover
      out.raster[is.na(forest.opps)] <- NA# Excluding non reforestation opportunities
      seq.raster <- seq.tile
      seq.raster[is.na(out.raster)] <- NA
      if(i %in% 1) {
        out.brick <- stack(seq.raster)
      } else {
        out.brick <- stack(out.brick,seq.raster)
      }
      writeRaster(out.raster,
                  paste0(getwd(),'/Raster_Outputs/',land.man.names[i],'tile_',x,'.tif'),
                  overwrite = TRUE)
    }
    # Updating names
    names(out.brick) <- land.man.names
    
    # Returning raster brick
    return(out.brick)
  }






### Function to split NLCD data into multiple layers for opportunity classes.
land.cov.classes <-
  function(raster.input) {
    # Looping through land cover classes
    ### Current land cover
    # Important classes
    # 11 = open water
    # 12 = pernnial ice/snow
    # 22 = urban, low intensity
    # 23 = urban, medium intensity
    # 24 = urban high intensity
    # 21 = urban, open space
    # 31 = barren
    # 41-43 = forest
    # 51-52 = shrublands
    # 71-74 = herbaceous; 71 = grassland; 72 = sedge; 73 = lichens; 74 = moss
    # 81-82 = planted; 81 = pasture/hay; 82 = cultivated crops
    # 90 = woody wetlands
    # 95 = emergant herbaceous wetlands
    
    land.cov.list <-
      list(c(81), # pasture
           c(82), # crop
           c(41:43), # Forest
           c(51:52), # Shrublands
           c(71:74), # Grassland
           c(21)) # Urban open space
    # land.cov.names <- c('pasture','challengingcrop','forest','shrub','grassland','urbanopenspace')
    out.raster <- raster.input
    # out.raster <- NA
    # Making raster brick
    for(i in 1:length(land.cov.list)) {
      out.raster[raster.input %in% land.cov.list[[i]]] <- i # Exclude cells not in target land cover
    }
    # out.raster[is.na(forest.opps)] <- NA # Excluding non reforestation opportunities
    # Getting rid of other classes
    out.raster[out.raster > 6] <- NA
    out.raster[out.raster%in%0]<-NA
    # Returning raster brick
    return(out.raster)
  }





### Function to split NLCD data into multiple layers for opportunity classes.
land.cov.classes.deductions <-
  function(raster.input) {
    # Looping through land cover classes
    ### Current land cover
    # Important classes
    # 11 = open water
    # 12 = pernnial ice/snow
    # 22 = urban, low intensity
    # 23 = urban, medium intensity
    # 24 = urban high intensity
    # 21 = urban, open space
    # 31 = barren
    # 41-43 = forest; 41 = deciduous; 42 = evergreen; 43 = mixed
    # 51-52 = shrublands
    # 71-74 = herbaceous; 71 = grassland; 72 = sedge; 73 = lichens; 74 = moss
    # 81-82 = planted; 81 = pasture/hay; 82 = cultivated crops
    # 90 = woody wetlands
    # 95 = emergant herbaceous wetlands
    
    land.cov.df <-
      data.frame(
        matrix(c('pasture',81,.95,
                 'crop',82,.97,
                 'deciduous forest',41,.45,
                 'evergreen forest',42,.43,
                 'mixed forest',43,.23,
                 'shrub',list(51:52),.69,
                 'grassland',list(71:74),.79,
                 'urban open space',21,.85),
               ncol = 3, byrow = TRUE))
    
    names(land.cov.df) <- c('NLCD_cover','NLCD_value','deduction')
    
    out.raster <- raster.input
    # out.raster <- NA
    # Making raster brick
    for(i in 1:nrow(land.cov.df)) {
      out.raster[nlcd.tile %in% land.cov.df$NLCD_value[[i]]] <-  # Cells in land cover type
        raster.input[nlcd.tile %in% land.cov.df$NLCD_value[[i]]] * 
        land.cov.df$deduction[[i]] # Multiplied by non spatial deduction
    }
    
    # Returning total reforestation opportunities after incorporating non-spatial discounts
    return(out.raster)
  }



### Function to split NLCD data into multiple layers for opportunity classes.
nlcd.deductions <-
  function(raster.input) {
    # Looping through land cover classes
    ### Current land cover
    # Important classes
    # 11 = open water
    # 12 = pernnial ice/snow
    # 22 = urban, low intensity
    # 23 = urban, medium intensity
    # 24 = urban high intensity
    # 21 = urban, open space
    # 31 = barren
    # 41-43 = forest; 41 = deciduous; 42 = evergreen; 43 = mixed
    # 51-52 = shrublands
    # 71-74 = herbaceous; 71 = grassland; 72 = sedge; 73 = lichens; 74 = moss
    # 81-82 = planted; 81 = pasture/hay; 82 = cultivated crops
    # 90 = woody wetlands
    # 95 = emergant herbaceous wetlands
    
    land.cov.df <-
      data.frame(
        matrix(c('pasture',81,.95,
                 'crop',82,.97,
                 'deciduous forest',41,.45,
                 'evergreen forest',42,.43,
                 'mixed forest',43,.23,
                 'shrub',list(51:52),.69,
                 'grassland',list(71:74),.79,
                 'urban open space',21,.85),
               ncol = 3, byrow = TRUE))
    
    names(land.cov.df) <- c('NLCD_cover','NLCD_value','deduction')
    
    out.raster <- raster.input
    # out.raster <- NA
    # Making raster brick
    for(l in 1:nrow(land.cov.df)) {
      out.raster[raster.input %in% land.cov.df$NLCD_value[[l]]] <-  # Cells in land cover type
        land.cov.df$deduction[[l]]
    }
    
    out.raster[out.raster >= 1]<- NA
    
    # Returning total reforestation opportunities after incorporating non-spatial discounts
    return(out.raster)
  }



### 
# Function to save rasters that aren't the land opp class or the land manager opp class
# Functions for those are later
save.raster.function.deductions <-
  function(df) {
    # Looping through data frame
    for(n in 1:nrow(df)) {
      # Map of forest opportunities
      tmp.raster <- forest.opps
      # Map of opportunity class
      tmp.exclude <- get(df$r_object[n])
      # Excluded cells that aren't in opportunity class
      tmp.raster[is.na(tmp.exclude) | tmp.exclude %in% 0] <- NA
      # Sequestration of all forest opportunitites
      tmp.raster.seq <- seq.tile
      # And excluding cells that aren't in opportunity class
      tmp.raster.seq[is.na(tmp.raster)] <- NA
      
      # Saving raster
      writeRaster(tmp.raster.seq,
                  paste0(getwd(),'/Raster_Outputs/',df$opp_class[n],'tile_',x,'_deductions.tif'),
                  overwrite = TRUE)
    }
  }


### Function to split NLCD data into multiple layers for opportunity classes.
save.land.cov.function.deductions <-
  function(raster.input) {
    # Looping through land cover classes
    ### Current land cover
    # Important classes
    # 11 = open water
    # 12 = pernnial ice/snow
    # 22 = urban, low intensity
    # 23 = urban, medium intensity
    # 24 = urban high intensity
    # 21 = urban, open space
    # 31 = barren
    # 41-43 = forest
    # 51-52 = shrublands
    # 71-74 = herbaceous; 71 = grassland; 72 = sedge; 73 = lichens; 74 = moss
    # 81-82 = planted; 81 = pasture/hay; 82 = cultivated crops
    # 90 = woody wetlands
    # 95 = emergant herbaceous wetlands
    
    land.cov.list <-
      list(c(81), # pasture
           c(82), # crop
           c(41:43), # Forest
           c(51:52), # Shrublands
           c(71:74), # Grassland
           c(21)) # Urban open space
    land.cov.names <- c('pasture','challengingcrop','forest','shrub','grassland','urbanopenspace')
    
    # Making raster brick
    for(i in 1:length(land.cov.list)) {
      out.raster <- raster.input
      out.raster[!(raster.input %in% land.cov.list[[i]])] <- NA # Exclude cells not in target land cover
      out.raster[is.na(forest.opps)] <- NA # Excluding non reforestation opportunities
      seq.raster <- seq.tile
      seq.raster[is.na(out.raster)] <- NA
      if(i %in% 1) {
        out.brick <- stack(seq.raster)
      } else {
        out.brick <- stack(out.brick,seq.raster)
      }
      writeRaster(out.raster,
                  paste0(getwd(),'/Raster_Outputs/',land.cov.names[i],'_tile_',x,'_deductions.tif'),
                  overwrite = TRUE)
    }
    # Updating names
    names(out.brick) <- land.cov.names
    
    # Returning raster brick
    return(out.brick)
  }



# Summing by opportunity class
# And summing
# This is for saving tiles after deductions have been applied
opp.sum.function.deduction <-
  function(opp.class) {
    # Data frame to save
    out.df <- data.frame()
    
    # Lopping through counties
    county.loop <- unique(opp.df$county)
    
    for(county in county.loop) {
      # Limiting to the target county
      tmp.county <- which(opp.df$county %in% county)
      tmp.df.out <- opp.df[tmp.county,]
      # Summing by opportunity class
      tmp.df.out <- 
        rowsum(tmp.df.out[,c(opp.class,'seq_rates','count','area_cell')], 
               group = tmp.df.out[,opp.class], na.rm = TRUE)
      # Getting back to correct value
      # rowsums is annoying like this
      tmp.df.out[,opp.class] <- tmp.df.out[,opp.class]/tmp.df.out$count
      # Adding county identifier back in
      tmp.df.out$county <- county
      # And adding to data frame that will be saved
      out.df <- rbind(out.df, tmp.df.out)
    } # End of county loop
    # Converting to data frame
    out.df <- as.data.frame(out.df)
    
    # Converting from 30x30m cells to hectares (e.g. multiplying by 900/10000)
    # And changing column names
    out.df <-
      out.df %>%
      mutate(seq_rates = seq_rates * (900 / 10000),
             area_cell = area_cell * (900 / 10000)) %>%
      dplyr::rename(tot_sequestration_tonnes_c = seq_rates,
                    tot_opp_area_hectares = area_cell)
    
    # MErging county and state name
    out.df <-
      left_join(out.df, 
                states.counties.db %>% dplyr::select(county = county_id, State = NAME, County = COUNTY_NAME))
    
    # Doing translation for land manager and land covers
    if(opp.class %in% 'land_owner') {
      # Separating out by manager where:
      # 1 = Federal
      # 2 = State
      # 3 = Private
      # 4 = USFS
      # 5 = BLM
      # 6 = Other
      # 7 = not classified
      out.df <-
        out.df %>%
        mutate(land_owner = ifelse(land_owner %in% 1, 'federal',
                                   ifelse(land_owner %in% 2, 'state',
                                          ifelse(land_owner %in% 3, 'private',
                                                 ifelse(land_owner %in% 4, 'usfs',
                                                        ifelse(land_owner %in% 5, 'blm',
                                                               ifelse(land_owner %in% 6, 'other',
                                                                      ifelse(land_owner %in% 7, 'not_classified', NA))))))))
      
    }
    
    if(opp.class %in% 'current_land_cover') {
      # Separating by land cover
      # 1 = pasture
      # 2 = crop
      # 3 = forest
      # 4 = shrublands
      # 5 = grassland
      # 6 = urban open space
      
      out.df <-
        out.df %>%
        mutate(current_land_cover = ifelse(current_land_cover %in% 1, 'pasture',
                                           ifelse(current_land_cover %in% 2, 'challengingcrop',
                                                  ifelse(current_land_cover %in% 3, 'forest',
                                                         ifelse(current_land_cover %in% 4, 'shrub',
                                                                ifelse(current_land_cover %in% 5, 'grassland',
                                                                       ifelse(current_land_cover %in% 6, 'urbanopenspace', NA)))))))
    }
    
    # Saving data frame
    # return(out.df) # - Leaving here in case you want to see outputs
    write.csv(out.df,
              paste0(getwd(),'/CSV_Outputs/Reforestation_opp_by_',opp.class,'_tile_',x,'_deduction.csv'),
              row.names = FALSE)
  }


# And getting the faceted results for the website
facet.sum.function <-
	function(input.df) {

	#  If statement so that this works with deducted and non-deducted opportunity classes
	# Slight differences in names
	if('area_cell' %in% names(input.df)) {
		input.df <-
			input.df %>%
			mutate(count = area_cell) # Update column values so that it works
	} else {
		# Do nothing
	}

	# Summarising
	out.df <-
		input.df %>%
		mutate(county = FIPS_code) %>%
		dplyr::group_by(nlcd.cover,floodplain,corridors,streamside,postburn,land_owner_type,land_owner_name, county) %>%
		dplyr::summarise(acres = sum(count),
				 carbon = sum(seq_rates)) %>%
		dplyr::rename(nlcd = nlcd.cover,
			      fldpln = floodplain,
			      climcor = corridors,
			      buff30m = streamside,
			      brnubrn = postburn,
			      owntype = land_owner_type,
			      ownname = land_owner_name) %>%
		mutate(carbon = carbon * (900 / 10000), # Adjusting for area
		       acres = acres * (900 / 10000)) %>%
		mutate(acres = acres * 2.47105) # And adjusting for units - from hectares to acres

	# And saving
	# If statement to separate deducted from non deducted rasters
	 if('area_cell' %in% names(input.df)) {
        	#writing csv
		 write.csv(out.df,
			   paste0(getwd(),'/CSV_Outputs/Faceted_outputs_deducted_tile_',x,'.csv'),
			   row.names = FALSE)
	 } else {
        	write.csv(out.df,
                           paste0(getwd(),'/CSV_Outputs/Faceted_outputs_tile_',x,'.csv'),
                           row.names = FALSE)
	 }
	}


