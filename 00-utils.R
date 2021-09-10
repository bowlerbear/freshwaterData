
library(sp)
library(tidyverse)
library(ggmap)


# =================== convert easting and northing to lon and lat =================== 
# in freshwater set
# Gauß-Krüger
proj4string <- "+proj=tmerc +lat_0=0 +lon_0=9 +k_0=1 +x_0=3500000 +y_0=0 +ellps=bessel +units=m"

convertXY2LonLat <- function(x, y) {
  # source data
  xy <- data.frame(x, y)
  
  # transformed data
  pj <- proj4::project(xy, proj4string, inverse=TRUE)
  latlon <- data.frame(lat=pj$y, lon=pj$x)
  return(latlon)
}




# =================== convert lon and lat to easting and northing =================== 
wg84 <- "+proj=longlat +datum=WGS84 +no_defs"

convertLonLat2UTM <- function(x, y) {
	 xy <- data.frame(ID = 1:length(x), X = x, Y = y)
	 coordinates(xy) <- c("X", "Y")
	 proj4string(xy) <- CRS(wg84) 
	 df <- spTransform(xy, CRS(proj4string))
	 return(as.data.frame(df))
}




# =================== extract Germany's maps =================== 
# get centre of coordinates
# geocode("germany")
# lon   lat
# 10.5  51.2


# Germany's map 
# version 1
germany_map <- get_googlemap(center = c(10.5, 51.2), zoom = 6)
germany_map %>% ggmap()

# version 2 (without roads)
germany_map2 <- get_googlemap(center = c(10.5, 51.2), zoom = 6,
                              style = "feature:road|visibility:off")
germany_map2 %>% ggmap()

# version 3 (black & white)
germany_bw_map <- get_googlemap(center = c(10.5, 51.2), zoom = 6,
                                color = "bw",
                                style = "feature:road|visibility:off&style=element:labels|visibility:off")
germany_bw_map %>% ggmap()

# version 4 (black & white, without borders)
germany_bw_map2 <- get_googlemap(center = c(10.5, 51.2), zoom = 6,
                                 color = "bw",
                                 style = "feature:road|visibility:off")
germany_bw_map2 %>% ggmap()


# get Germany shp file
germany_shp <- raster::getData('GADM', country='DE', level=1)
# convert from sp to sf
germany_shp <- germany_shp %>% sf::st_as_sf()




# =================== check no. of missing values =================== 
checkNumNa <- function(df) {
  df_var <- vector()
  df_missing <- vector()
  
  for (i in 4:ncol(df)) {
    print(colnames(df[,i]))
    df_var[i] <- colnames(df[,i])
    
    print(df[,i] %>% is.na() %>% sum())
    df_missing[i] <- df[,i] %>% is.na() %>% sum()
    
    print((df[,i] %>% is.na() %>% sum())/nrow(df))
  }
  
  df_var <- df_var[4:length(df_var)]
  df_missing <- df_missing[4:length(df_missing)]
  df_missingPer <- df_missing/nrow(df)
  
  df_missing_df <- data.frame(matrix(ncol = 3, nrow = length(df_var)))
  colnames(df_missing_df) <- c("variable", "numMissingValues", "perMissingValues")
  df_missing_df$variable <- df_var
  df_missing_df$numMissingValues <- df_missing
  df_missing_df$perMissingValues <- df_missingPer
  df_missing_df <- df_missing_df %>% tibble()
  return(df_missing_df)
}



