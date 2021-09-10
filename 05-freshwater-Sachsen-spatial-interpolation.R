
library(tidyverse)
library(tmap)


# =================== spatial interpolation (Sachsen) -- preparation =================== 
# crs
sys <- "+proj=tmerc +lat_0=0 +lon_0=9 +k_0=1 +x_0=3500000 +y_0=0 +ellps=bessel +units=m"
# sys <- "+init=epsg:31467"

# Sachsen shp file
sachsen_sp <- as(germany_shp %>% filter(NAME_1=="Sachsen"), "Spatial")

# reproject Sachsen shape file
sachsen_sp <- spTransform(sachsen_sp, raster::crs(sys))
sachsen_sf <- sachsen_sp %>% sf::st_as_sf()


# check what variables were sampled each year
VarPerYear <- samples_fin_sub %>% filter(state=="Sachsen") %>% 
  ggplot() +
  geom_point(aes(x=year, y=name, color=name)) +
  ylab("variable") +
  theme(legend.position="none") +
  ggtitle("Variables sampled in each year (Sachsen)")
# ggsave(filename = "processed/freshwater_summary_stats/variables_sampled_each_year_Sachsen.png", VarPerYear)
rm(VarPerYea)




# =================== Thiessen polygons =================== 
library(rgdal)
library(spatstat)  # Used for the dirichlet tessellation function
library(spatstat.geom)
library(maptools)  # Used for conversion from SPDF to ppp
library(raster)    # Used to clip out thiessen polygons
library(sp)


thiessen_polygons_directory <- "processed/freshwater_trends_maps_by_site_Sachsen_min8years_interpolated/thiessen_polygons"


# map all the variables
waterquality_vars <- (trends8 %>% filter(state=="Sachsen"))$name %>% unique()

for (i in 1:length(waterquality_vars)) {
  print(paste0("Mapping: ", waterquality_vars[i]))
  
  P <- trends8 %>% filter(name==waterquality_vars[i], state=="Sachsen") %>% sf::st_as_sf(coords=c("easting", "northing"))
  P_sp <-  as(P, "Spatial")
  
  # create a tessellated surface
  P_sf <-  sf::st_geometry(P)
  th <- sf::st_as_sfc(dirichlet(spatstat.geom::as.ppp(P_sf))) # doesn't work with sp somehow
  th <- as(th, "Spatial")
  
  # the dirichlet function does not carry over projection information requiring that this information be added manually
  proj4string(P_sp) <- CRS(sys)
  proj4string(th) <- proj4string(P_sp)
  
  # join the point attributes to the tesselated surface via a spatial join
  # since the tessellated surface does not store attribute information
  th.z     <- over(th, P_sp, fn=mean)
  th.spdf  <-  SpatialPolygonsDataFrame(th, as.data.frame(th.z))
  
  # clip the tessellated surface to the Sachsen boundaries
  th.clp   <- raster::intersect(sachsen_sp, th.spdf)
  
  # convert SpatialPolygonsDataFrame to sf data frame
  th.clp.sf <- sf::st_as_sf(th.clp)
  
  # map the data
  trendsMap <- tm_shape(th.clp.sf) + 
    tm_polygons(lwd = 0.5, col="Estimate", palette = "-RdBu", n=7) +
    tm_shape(P_sp) + tm_dots() +
    tm_layout(legend.outside = TRUE, main.title = waterquality_vars[i]) 
  
  tmap_save(tm = trendsMap, 
            filename = paste0(thiessen_polygons_directory, "/", waterquality_vars[i], "_Trends.png"))
}

rm(waterquality_vars, P, P_sp, P_sf, th, th.z, th.spdf, th.clp, th.clp.sf, trendsMap, i)




# check outliers
# EC_25dC
P <- trends8 %>% filter(name=="EC_25dC", state=="Sachsen", Estimate > -150) %>% sf::st_as_sf(coords=c("easting", "northing"))
P_sp <-  as(P, "Spatial")

# create a tessellated surface
P_sf <-  sf::st_geometry(P)
th <- sf::st_as_sfc(dirichlet(spatstat.geom::as.ppp(P_sf))) # doesn't work with sp somehow
th <- as(th, "Spatial")

# the dirichlet function does not carry over projection information requiring that this information be added manually
proj4string(P_sp) <- CRS(sys)
proj4string(th) <- proj4string(P_sp)

# join the point attributes to the tesselated surface via a spatial join
# since the tessellated surface does not store attribute information
th.z     <- over(th, P_sp, fn=mean)
th.spdf  <-  SpatialPolygonsDataFrame(th, as.data.frame(th.z))

# clip the tessellated surface to the Sachsen boundaries
th.clp   <- raster::intersect(sachsen_sp, th.spdf)

# convert SpatialPolygonsDataFrame to sf data frame
th.clp.sf <- sf::st_as_sf(th.clp)

# map the data
trendsMap <- tm_shape(th.clp.sf) + 
  tm_polygons(lwd = 0.5, col="Estimate", palette = "-RdBu", n=7) +
  tm_shape(P_sp) + tm_dots() +
  tm_layout(legend.outside = TRUE, main.title = "EC_25dC") 
tmap_save(tm = trendsMap, 
          filename = paste0(thiessen_polygons_directory, "/", "EC_25dC", "_Trends_noOutliers.png"))


# HCO3
P <- trends8 %>% filter(name=="HCO3", state=="Sachsen", Estimate < 20) %>% sf::st_as_sf(coords=c("easting", "northing"))
P_sp <-  as(P, "Spatial")

# create a tessellated surface
P_sf <-  sf::st_geometry(P)
th <- sf::st_as_sfc(dirichlet(spatstat.geom::as.ppp(P_sf))) # doesn't work with sp somehow
th <- as(th, "Spatial")

# the dirichlet function does not carry over projection information requiring that this information be added manually
proj4string(P_sp) <- CRS(sys)
proj4string(th) <- proj4string(P_sp)

# join the point attributes to the tesselated surface via a spatial join
# since the tessellated surface does not store attribute information
th.z     <- over(th, P_sp, fn=mean)
th.spdf  <-  SpatialPolygonsDataFrame(th, as.data.frame(th.z))

# clip the tessellated surface to the Sachsen boundaries
th.clp   <- raster::intersect(sachsen_sp, th.spdf)

# convert SpatialPolygonsDataFrame to sf data frame
th.clp.sf <- sf::st_as_sf(th.clp)

# map the data
trendsMap <- tm_shape(th.clp.sf) + 
  tm_polygons(lwd = 0.5, col="Estimate", palette = "-RdBu", n=7) +
  tm_shape(P_sp) + tm_dots() +
  tm_layout(legend.outside = TRUE, main.title = "HCO3") 
tmap_save(tm = trendsMap, 
          filename = paste0(thiessen_polygons_directory, "/", "HCO3", "_Trends_noOutliers.png"))


rm(waterquality_vars, P, P_sp, P_sf, th, th.z, th.spdf, th.clp, th.clp.sf, trendsMap)
rm(thiessen_polygons_directory)




# =================== Inverse distance weighting (IDW) -- quick plot and check =================== 
# IDW
library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function
library(raster)


# plot interpolated maps for all variables (with idp=2.0) for quick check
years <- 1998:2013
idp_2.0_directory <- "processed/freshwater_trends_maps_by_site_Sachsen_min8years_interpolated/IDW_ipd_2.0"
waterquality_vars <- (samples_fin_sub_wSites %>% filter(state=="Sachsen", 
                                                 !(name %in% c("turbidity_phyChem.meas",
                                                               "tot.N.bound", "nonionised.NH3N"
                                                               ))))$name %>% unique()
# remove these variables since they don't start in 1998 and they're not important

for (j in 1:length(waterquality_vars)) {
  print(paste0("Mapping: ", waterquality_vars[j]))
  
  # create directory
  target_directory <- paste0(idp_2.0_directory, "/", waterquality_vars[j])
  dir.create(target_directory)
  
  variable <-  waterquality_vars[j]
  
  # interpolated maps of different years for each variable
  for (i in 1:length(years)) {
    print(paste0('Current year: ', years[i]))
    
    P <- samples_fin_sub_wSites %>% 
      filter(name==variable, year==years[i], state=="Sachsen") %>% 
      sf::st_as_sf(coords=c("easting", "northing"))
    
    # if P has values, to avoid error
    if (nrow(P) != 0) {
      P <- P %>% sf::st_set_crs(sys)
      
      P_sp <-  as(P, "Spatial")
      
      # Create an empty grid where n is the total number of cells
      grd              <- as.data.frame(spsample(P_sp, "regular", n=50000))
      names(grd)       <- c("X", "Y")
      coordinates(grd) <- c("X", "Y")
      gridded(grd)     <- TRUE  # Create SpatialPixel object
      fullgrid(grd)    <- TRUE  # Create SpatialGrid object
      
      # Add P's projection information to the empty grid
      proj4string(grd) <- proj4string(P_sp)
      
      # Interpolate the grid cells using a power value of 2 (idp=2.0)
      P.idw <- gstat::idw(value ~ 1, P_sp, newdata=grd, idp=2.0)
      
      # Convert to raster object then clip to Sachsen
      r       <- raster(P.idw)
      r.m     <- mask(r, sachsen_sp)
      
      # plot
      interpolatedMap <- tm_shape(r.m) + 
        tm_raster(n=10,palette = "-RdBu", style='cont') + 
        tm_shape(P) + tm_dots(size=0.1) +
        tm_layout(legend.outside = TRUE, main.title = variable) 
      
      tmap_save(tm = interpolatedMap, 
                filename = paste0(target_directory, '/', variable, '_', years[i], ".png"))
    }
  }
}

rm(idp_2.0_directory, target_directory, variable, P, P_sp, grd, P.idw, r, r.m, interpolatedMap, i, j)




# plot all the data points across years as well
data_points_directory <- "processed/freshwater_trends_maps_by_site_Sachsen_min8years_interpolated/data_points"

for (j in 1:length(waterquality_vars)) {
  print(paste0("Mapping: ", waterquality_vars[j]))
  
  # create directory
  target_directory <- paste0(data_points_directory, "/", waterquality_vars[j])
  dir.create(target_directory)
  
  variable <-  waterquality_vars[j]
  
  # maps of different years for each variable
  for (i in 1:length(years)) {
    print(paste0('Current year: ', years[i]))
    
    P <- samples_fin_sub_wSites %>% 
      filter(name==variable, year==years[i], state=="Sachsen") %>% 
      sf::st_as_sf(coords=c("easting", "northing"))
    
    # if P has values, to avoid error
    if (nrow(P) != 0) {
      P <- P %>% sf::st_set_crs(sys)
      
      # plot
      points_map <- tm_shape(sachsen_sf) +
        tm_polygons(lwd = 0.5) +
        tm_shape(P) + tm_dots(col='value', style='order', palette='viridis', alpha=0.7) +
        tm_layout(legend.outside = TRUE, main.title = variable)
      
      tmap_save(tm = points_map, 
                filename = paste0(target_directory, '/', variable, '_', years[i], ".png"))
    }
    
  }
}

rm(years, data_points_directory, target_directory, variable, P, points_map, i, j)




# =================== Inverse distance weighting (IDW) -- find the optimal idp values =================== 
# compute RMSE and R2 for different idp in IDW
idps <- c(2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0)
idps_directory <- "processed/freshwater_trends_maps_by_site_Sachsen_min8years_interpolated/IDW_different_idp"


for (k in 1:length(waterquality_vars)) {
  variable <- waterquality_vars[k]
  print(paste0("Processing: ", variable))
  
  # due to raster extent compatibility (majority of raster cropped since the points are too sparse)
  # all variables: from 1999 on
  if (variable == "chem.O2.demand") {
    years <- c(1999:2004, 2006)
  } else if (variable == "inorgBound.N") {
    years <- 2002:2013
  } else if (variable == "O2.consump.aft5days.wOutINH") {
    years <- 2007:2013
  } else if (variable == "O2.consump.aft7days.wINH") {
    years <- 1999:2006
  } else if (variable == "tot.N") {
    years <- 2000:2013
  } else if (variable == "tot.orgBound.N") {
    years <- 2000:2013
  } else {
    years <- 1999:2013
  }
  
  # df for storing rmse and r2 for different combinations of idp values and years
  idps_years_df <- expand.grid(idps, years) %>% 
    as_tibble() %>% 
    rename(idp=Var1, year=Var2) %>% 
    mutate(rmse = as.numeric(NA), 
           r2 = as.numeric(NA), 
           cor = as.numeric(NA))
  
  for (i in 1:length(years)) {
    
    for (j in 1:length(idps)) {
      P <- samples_fin_sub_wSites %>% filter(name==variable, year==years[i], state=="Sachsen") %>% 
        sf::st_as_sf(coords=c("easting", "northing"))
      
      P <- P %>% sf::st_set_crs(sys)
      
      P_sp <-  as(P, "Spatial")

      # create an empty grid where n is the total number of cells
      grd              <- as.data.frame(spsample(P_sp, "regular", n=50000))
      names(grd)       <- c("X", "Y")
      coordinates(grd) <- c("X", "Y")
      gridded(grd)     <- TRUE  # Create SpatialPixel object
      fullgrid(grd)    <- TRUE  # Create SpatialGrid object
      
      # add P's projection information to the empty grid
      proj4string(grd) <- proj4string(P_sp)
      
      # print info
      print(paste0('Current year: ', years[i]))
      print(paste0('Current idp value: ', idps[j]))
      
      # create directory
      dir.create(paste0(idps_directory, "/maps/", variable))
      target_directory <- paste0(idps_directory, "/maps/", variable, "/", variable, "_", "idp_", idps[j])
      dir.create(target_directory)
      
      # interpolate the grid cells using a power value of 2 (idp=2.0)
      P.idw <- gstat::idw(value ~ 1, P_sp, newdata=grd, idp=idps[j])
      
      # Ccnvert to raster object then clip to Sachsen
      r       <- raster(P.idw)
      r.m     <- mask(r, sachsen_sp)
      
      # plot
      interpolatedMap <- tm_shape(r.m) +
        tm_raster(n=10,palette = "-RdBu", style='cont') +
        tm_shape(P) + tm_dots(size=0.1) +
        tm_layout(legend.outside = TRUE, main.title = variable)
      
      tmap_save(tm = interpolatedMap,
                filename = paste0(target_directory, '/', variable, '_', years[i], ".png"))
      
      
      # model performance
      # extract raster based on the location of the observed values
      P_preds <- raster::extract(r.m, P, fun="mean", na.rm=T)
      obs_pred_df <- cbind(P$value, P_preds) %>% as.data.frame()
      names(obs_pred_df) <- c('observed', 'predicted')
      
      # compute RMSE
      rmse_tmp <- sqrt(sum((obs_pred_df$predicted - obs_pred_df$observed)^2, na.rm=T) / length(obs_pred_df$observed))
      
      # compute R^2
      r2_tmp <- (cor(obs_pred_df$observed, obs_pred_df$predicted, use="complete.obs", method="pearson"))^2
      
      # compute correlation
      cor_tmp <- cor(obs_pred_df$observed, obs_pred_df$predicted, use="complete.obs", method="pearson")
      
      # store them in a df
      idps_years_df[idps_years_df$idp==idps[j] & idps_years_df$year==years[i],]$rmse <- rmse_tmp
      idps_years_df[idps_years_df$idp==idps[j] & idps_years_df$year==years[i],]$r2 <- r2_tmp
      idps_years_df[idps_years_df$idp==idps[j] & idps_years_df$year==years[i],]$cor <- cor_tmp
      
    }
  }
  
  write.csv(idps_years_df, paste0(idps_directory, '/', 'csv/', variable, "_idp.csv"))
}

rm(idps, variable, years, P, P_sp, grd, target_directory, P.idw, r, r.m, interpolatedMap, P_preds, obs_pred_df, rmse_tmp, r2_tmp, cor_tmp, idps_years_df, i, j, k)



# pick the idp value for each year with the lowest rmse and highest r2
# # load all the csv files in and save in a list
# csv_directory <- "processed/freshwater_trends_maps_by_site_Sachsen_min8years_interpolated/IDW_different_idp/csv"
# csv_files <- list.files(csv_directory)
# 
# # initialize an empty list for storing the csv files
# csv_list <- vector(mode = "list", length = length(csv_files))
# 
# for (i in 1:length(csv_files)) {
#   current_file <- paste(csv_directory, csv_files[i], sep="/")
#   print(current_file)
# 
#   file_tmp <- readr::read_csv(current_file)
#   csv_list[[i]] <- file_tmp
#   names(csv_list)[i] <- csv_files[i]
# }
# 
# 
# # find the optimal value for each variable, for example
# variable <- csv_list['chem.O2.demand_idp.csv']$chem.O2.demand_idp
# 
# available_years <- variable$year %>% unique()
# 
# # show the lowest rmse and the highest r2 for each variable and each year
# for (i in 1:length(available_years)) {
#   print(paste(available_years[i], "Lowest rmse:"))
#   variable %>% filter(year==available_years[i]) %>% 
#     slice_min(rmse) %>% print()
#   
#   print(paste(available_years[i], "Highest r2:"))
#   variable %>% filter(year==available_years[i]) %>% 
#     slice_max(r2) %>% print()
# }
# 
# rm(csv_directory, csv_files, current_file, file_tmp, csv_list, variable, available_years)
# # manual recorded under "IDW_different_idp/optimal_idp"


# contains minimum 5 years with cor > 0.7
# EC_20dC_idp
# EC_25dC_idp
# HCO3_idp
# inorgBound.N_idp
# NO3_idp
# NO3N_idp
# O2.saturation_idp
# oPO4_idp
# oPO4P_idp
# pH.field_idp
# tot.hardness_idp
# tot.N_idp
# tot.orgBound.N_idp




# =================== Inverse distance weighting (IDW) -- generate rasters based on the optimal idp values =================== 
# load all the optimal idps txt files in and save in a list
idp_optimal_directory <- "processed/freshwater_trends_maps_by_site_Sachsen_min8years_interpolated/IDW_different_idp/optimal_idp"
idp_optimal_files <- list.files(idp_optimal_directory)

# initialize an empty list for storing the txt files
idp_optimal_list <- vector(mode = "list", length = length(idp_optimal_files))

for (i in 1:length(idp_optimal_files)) {
  current_file <- paste(idp_optimal_directory, idp_optimal_files[i], sep="/")
  print(current_file)
  
  file_tmp <- readr::read_delim(current_file, delim=" ")
  idp_optimal_list[[i]] <- file_tmp
  names(idp_optimal_list)[i] <- gsub(".txt", "", idp_optimal_files[i])
}


# function to produce raster output for each year with the corresponding idp value selected
produceRastersIDP <- function(variable.idps, variable) {
  for (i in 1:length(variable.idps$year)) {
    
    P <- samples_fin_sub_wSites %>% filter(name==variable, year==variable.idps$year[i], state=="Sachsen") %>% 
      sf::st_as_sf(coords=c("easting", "northing"))
    
    P <- P %>% sf::st_set_crs(sys)
    
    P_sp <-  as(P, "Spatial")
    
    # create an empty grid where n is the total number of cells
    grd              <- as.data.frame(spsample(P_sp, "regular", n=50000))
    names(grd)       <- c("X", "Y")
    coordinates(grd) <- c("X", "Y")
    gridded(grd)     <- TRUE  # Create SpatialPixel object
    fullgrid(grd)    <- TRUE  # Create SpatialGrid object
    
    # add P's projection information to the empty grid
    proj4string(grd) <- proj4string(P_sp)
    
    # print info
    print(paste0('Current year: ', variable.idps$year[i]))
    print(paste0('Current idp value: ', variable.idps$idp[i]))
    
    # create directory
    target_directory <- paste0(idps_directory, "/rasters_optimal_idp/", variable)
    dir.create(target_directory)
    
    # interpolate the grid cells using a power value of 2 (idp=2.0)
    P.idw <- gstat::idw(value ~ 1, P_sp, newdata=grd, idp=variable.idps$idp[i])
    
    # Ccnvert to raster object then clip to Sachsen
    r       <- raster(P.idw)
    r.m     <- mask(r, sachsen_sp)
    
    # save raster
    writeRaster(r.m, paste0(target_directory, '/', variable, '_', variable.idps$year[i], '_idp_', variable.idps$idp[i], '.tif'))
    
  }
}


# produce raster outputs for all the selected selected variables
selected_variables <- names(idp_optimal_list)

for (j in 1:length(selected_variables)) {
  produceRastersIDP(variable.idps=idp_optimal_list[[j]],
                    variable=selected_variables[j])
}

rm(idp_optimal_directory, idp_optimal_files, idp_optimal_list, current_file, file_tmp, produceRastersIDP, i, j)




# =================== Inverse distance weighting (IDW) -- load rasters in and convert to a df per variable =================== 
# function for loading rasters from a directory, creating a df, visualising them
rasterDirectory2df <- function(variable) {

  raster_directory <- paste(idps_directory, "rasters_optimal_idp", variable, sep="/")
  files <- list.files(raster_directory)
  
  # initialize an empty list
  raster_list <- vector(mode = "list", length = length(files))
  
  for (i in 1:length(files)){
    current_file <- paste(raster_directory, files[i], sep="/")
    print(files[i])
    variable_raster <- raster(current_file)
    raster_list[[i]] <- variable_raster
  }
  
  
  # change the dimensions, resolutions and extents
  # initialize an empty list
  raster_list_new <- vector(mode = "list", length = length(files))
  
  # use the Sachsen shp as a template to resample everything
  raster_template <- raster(extent(sachsen_sp), resolution = 800, crs = sys)
  
  for (i in 1:length(raster_list)){
    raster_i <- raster_list[[i]]
    raster_i_new <- projectRaster(from=raster_i, to=raster_template, method='bilinear')
    # ex <- extent(raster_template)
    # raster_i_new <- crop(raster_i_new, ex) 
    # raster_i_new <- mask(raster_i_new, raster_template)
    raster_list_new[[i]] <- raster_i_new
  }
  
  
  # # check raster extent compatibility (original)
  # for (i in 1:length(raster_list)){
  #   plot(raster_list[[i]])
  # }
  # 
  # # check raster extent compatibility (new)
  # for (i in 1:length(raster_list_new)){
  #   plot(raster_list_new[[i]])
  # }
  
  
  # transform raster to df and save to a list
  # initialize an empty list
  raster_df_list <- vector(mode = "list", length = length(files))
  
  for (i in 1:length(raster_list_new)) {
    r <- raster_list_new[[i]]
    print(names(r))
    # assign the names to the list
    names(raster_df_list)[i] <- names(r)
    # change the name to layer for simplicity
    names(r) <- "layer"
    df <- as.data.frame(r, xy=T)
    df$Year <- str_extract(names(raster_list_new[[i]]), "\\d{4}") %>% as.numeric()
    df <- subset(df,!is.na(layer))
    raster_df_list[[i]] <- df
  }
  
  # make sure all the pixels contain values that are present every year
  common_x <- Reduce(intersect, Map("[[", raster_df_list, "x"))
  common_y <- Reduce(intersect, Map("[[", raster_df_list, "y"))
  
  # row bind the dfs in the list into a single df
  raster_df <- raster_df_list[[1]] %>% filter(x %in% common_x, y %in% common_y)
  
  for (i in 2:length(raster_df_list)) {
    raster_df <- rbind(raster_df, raster_df_list[[i]] %>% 
                         filter(x %in% common_x, y %in% common_y))
  }
  
  # visualise all years
  rasters_ggplot <- ggplot() +
    geom_raster(aes(x=x, y=y, fill=layer), data=raster_df) +
    viridis::scale_fill_viridis() +
    facet_wrap(~Year) +
    ggtitle(variable)
  
  ggsave(filename = paste0(idps_directory, "/", "maps_optimal_idp_facet", "/", variable, ".png"), rasters_ggplot,
         height = 8, width = 12)
  
  return(raster_df)
}


# load rasters in in a list, create a df for each variable directory
# initialize an empty list for storing the dfs
raster_df_byVariable_list <- vector(mode = "list", length = length(selected_variables))

for (j in 1:length(raster_df_byVariable_list)) {
  raster_df_byVariable_list[[j]] <- rasterDirectory2df(variable=selected_variables[j])
  names(raster_df_byVariable_list)[j] <- selected_variables[j]
}

rm(rasterDirectory2df, j)




# =================== Inverse distance weighting (IDW) -- calculate trends =================== 
# function for calculating trends
calTrends <- function(df) {
  df_calculated <- df %>%
    mutate(x = as.character(x),
           y = as.character(y)) %>% 
    filter(!is.na(layer)) %>%
    group_by(x,y) %>%    # bugs when group_by numerical variables
    mutate(x = as.numeric(x),
           y = as.numeric(y)) %>% 
    do(model = broom::tidy(lm(layer ~ Year, data = .))) %>% 
    unnest(model) %>%
    filter(term=="Year")
  return(df_calculated)
}


# initialize an empty list for storing the trends dfs
trends_list <- vector(mode = "list", length = length(raster_df_byVariable_list))

for (j in 1:length(raster_df_byVariable_list)) {
  trends_list[[j]] <- calTrends(raster_df_byVariable_list[[j]])
  print(selected_variables[j])
  names(trends_list)[j] <- paste0("trends_", selected_variables[j])
}

# saveRDS(trends_list, paste(idps_directory, "trends_list.rds", sep="/"))
# trends_list <- readRDS(paste(idps_directory, "trends_list.rds", sep="/"))


# plotting the trends
for (j in 1:length(trends_list)) {
  # create spatial points data frame
  spg <- trends_list[[j]][,c("x","y","estimate")] %>% filter(!is.na(estimate))
  coordinates(spg) <- ~ x + y
  # coerce to SpatialPixelsDataFrame
  gridded(spg) <- TRUE
  # coerce to raster
  dfr <- raster(spg)
  projection(dfr) <- CRS(sys)
  
  trends_Map <- tm_shape(dfr) +
    tm_raster(n=10, palette = "-RdBu", style='order') +
    tm_layout(legend.outside = TRUE, main.title = names(trends_list)[j] )
  
  tmap_save(tm = trends_Map, 
            filename = paste0(idps_directory, '/', 'trends_maps/', names(trends_list)[j], ".png"))
}




# check outliers for trends_EC_20dC
# create spatial points data frame
spg <- trends_list[["trends_EC_20dC"]][,c("x","y","estimate")] %>% 
  filter(!is.na(estimate))
plot(spg$estimate)
abline(h=-160, col="red")
hist(spg$estimate)

# remove outliers
# spg <- spg %>% 
#   filter(estimate > -150 & estimate < 200)
# plot(spg$estimate)
# hist(spg$estimate)


# check outliers for trends_NO3
# create spatial points data frame
spg <- trends_list[["trends_NO3"]][,c("x","y","estimate")] %>% 
  filter(!is.na(estimate))
plot(spg$estimate)
hist(spg$estimate)

# remove outliers
# spg <- spg %>% 
#   filter(estimate > -3 & estimate < 8)
# plot(spg$estimate)
# hist(spg$estimate)


# check outliers for trends_NO3
# create spatial points data frame
spg <- trends_list[["trends_NO3"]][,c("x","y","estimate")] %>% 
  filter(!is.na(estimate))
plot(spg$estimate)
hist(spg$estimate)

# remove outliers
# spg <- spg %>% 
#   filter(estimate > -3 & estimate < 8)
# plot(spg$estimate)
# hist(spg$estimate)



# check outliers for trends_oPO4
# create spatial points data frame
spg <- trends_list[["trends_oPO4"]][,c("x","y","estimate")] %>% 
  filter(!is.na(estimate))
plot(spg$estimate)
hist(spg$estimate)

# remove outliers
spg <- spg %>%
  filter(estimate > -1.0 & estimate < 0.4)
plot(spg$estimate)
hist(spg$estimate)


coordinates(spg) <- ~ x + y
# coerce to SpatialPixelsDataFrame
gridded(spg) <- TRUE
# coerce to raster
dfr <- raster(spg)
projection(dfr) <- CRS(sys)

# trends_Map <- tm_shape(dfr) +
#   tm_raster(n=10, palette = "-RdBu", style='order') +
#   tm_layout(legend.outside = TRUE, main.title = "trends_oPO4" )
# 
# tmap_save(tm = trends_Map, 
#           filename = paste0(idps_directory, '/', 'trends_maps/', "trends_oPO4_noOutliers" , ".png"))
# 


# tm_shape(dfr) +
#   tm_raster(palette = "-RdBu", style='cont') +
#   tm_layout(legend.outside = TRUE, main.title = variable)
# 
# plot(dfr)
rm(calTrends, spg, dfr, trends_Map, j)



# =================== Inverse distance weighting (IDW) -- calculate means =================== 
# function for calculating means
calMeans <- function(df) {
  df_calculated <- df %>%
    mutate(x = as.character(x),
           y = as.character(y)) %>% 
    filter(!is.na(layer)) %>%
    group_by(x,y) %>%    # bugs when group_by numerical variables
    summarise(estimate=median(layer,na.rm=T)) %>% 
    mutate(x = as.numeric(x),
           y = as.numeric(y))
  return(df_calculated)
}


# initialize an empty list for storing the means dfs
means_list <- vector(mode = "list", length = length(raster_df_byVariable_list))

for (j in 1:length(raster_df_byVariable_list)) {
  means_list[[j]] <- calMeans(raster_df_byVariable_list[[j]])
  print(selected_variables[j])
  names(means_list)[j] <- paste0("means_", selected_variables[j])
}


# plotting the means
for (j in 1:length(means_list)) {
  # create spatial points data frame
  spg <- means_list[[j]][,c("x","y","estimate")] %>% filter(!is.na(estimate))
  coordinates(spg) <- ~ x + y
  # coerce to SpatialPixelsDataFrame
  gridded(spg) <- TRUE
  # coerce to raster
  dfr <- raster(spg)
  projection(dfr) <- CRS(sys)
  
  means_Map <- tm_shape(dfr) +
    tm_raster(n=10,palette = "-RdBu", style='order') +
    tm_layout(legend.outside = TRUE, main.title = names(means_list)[j] )
  
  tmap_save(tm = means_Map, 
            filename = paste0(idps_directory, '/', 'means_maps/', names(means_list)[j], ".png"))
}

rm(calMeans, spg, dfr, means_Map, j)




