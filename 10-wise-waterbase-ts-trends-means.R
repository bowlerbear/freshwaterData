
library(tidyverse)
library(ggmap)
library(tmap)


# =================== prepare data for time series =================== 
# change to long format
wb_dataCast_ts1 <- wb_dataCast %>% pivot_longer(cols=4:length(wb_dataCast))

# add year, month, date without year
wb_dataCast_ts1 <- wb_dataCast_ts1 %>% 
  rename(date = phenomenonTimeSamplingDate,
         site_id = monitoringSiteIdentifier) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         monthDate = strftime(date, "%m-%d"))


# mean means per year (removed entries on 1st Jan)
wb_summaryYear <- plyr::ddply((wb_dataCast_ts1 %>% filter(monthDate != "01-01")),c("name","year","site_id"),
                              summarise, meanVal = median(value,na.rm=T))
wb_summaryYear <- wb_summaryYear %>% tibble()

# check proportion of NA values
wb_summaryYear %>% is.na() %>% sum()
(wb_summaryYear %>% is.na() %>% sum())/nrow(wb_summaryYear) # 63.6% NA




# =================== time series =================== 
ts_directory <- "processed/wisewb/excluded_winter_months/wisewb_time_series"

# annual trends -- splines
annual_trend_geom_smooth <- wb_summaryYear %>% ggplot(aes(x=year,y=meanVal)) +
  geom_smooth() +
  facet_wrap(~name,scales="free") +
  ylab('mean') +
  ggtitle("Annual trend")
# ggsave(filename = paste0(ts_directory, "/", "annual_trend_geom_smooth.png"), annual_trend_geom_smooth)
rm(annual_trend_geom_smooth)

# annual trends -- splines and points
annual_trend_geom_smooth_point <- wb_summaryYear %>% ggplot(aes(x=year,y=meanVal)) +
  geom_point(colour="purple", size=0.2) +
  geom_smooth() +
  facet_wrap(~name,scales="free") +
  ylab('mean') +
  ggtitle("Annual trend")
# ggsave(filename = paste0(ts_directory, "/", "annual_trend_geom_smooth_point.png"), annual_trend_geom_smooth_point)
rm(annual_trend_geom_smooth_point)

# annual trends -- lines
annual_trend_geom_line <- wb_summaryYear %>% 
  group_by(name, year) %>% 
  summarise(meanVal=mean(meanVal, na.rm=T)) %>% 
  ggplot(aes(x=year,y=meanVal)) +
  geom_line() +
  facet_wrap(~name,scales="free") +
  ylab('mean') +
  ggtitle("Annual trend")
# ggsave(filename = paste0(ts_directory, "/", "annual_trend_geom_line.png"), annual_trend_geom_line)
rm(annual_trend_geom_line)



# monthly trends -- lines
monthly_trend_geom_line <- wb_dataCast_ts1 %>% 
  filter(year >= 1980, monthDate != "01-01") %>% 
  group_by(name, month) %>% 
  summarise(meanVal=mean(value, na.rm=T)) %>% 
  ggplot(aes(x=month, y=meanVal)) +
  geom_line() +
  facet_wrap(~name,scales="free") +
  scale_x_continuous(breaks=seq(1,12,1)) +
  ylab('mean') +
  ggtitle("Monthly trend")
# ggsave(filename = paste0(ts_directory, "/", "monthly_trend_geom_line.png"), monthly_trend_geom_line)
rm(monthly_trend_geom_line)

# monthly trends -- splines
monthly_trend_geom_smooth <- wb_dataCast_ts1 %>% 
  filter(year >= 1980, monthDate != "01-01") %>% 
  group_by(name, month) %>% 
  summarise(meanVal=mean(value, na.rm=T)) %>% 
  ggplot(aes(x=month, y=meanVal)) +
  geom_smooth() +
  facet_wrap(~name,scales="free") +
  scale_x_continuous(breaks=seq(1,12,1)) +
  ylab('mean') +
  ggtitle("Monthly trend")
# ggsave(filename = paste0(ts_directory, "/", "monthly_trend_geom_smooth.png"), monthly_trend_geom_smooth)
rm(monthly_trend_geom_smooth)

# monthly trends by year
monthly_trend_by_year <- wb_dataCast_ts1 %>% 
  filter(year >= 1980, monthDate != "01-01") %>% 
  group_by(name, year, month) %>% 
  summarise(meanVal=mean(value, na.rm=T)) %>% 
  ggplot(aes(x=month, y=meanVal)) +
  geom_line(aes(colour=factor(year))) + 
  facet_wrap(~name,scales="free") +
  scale_x_continuous(breaks=seq(1,12,1)) +
  ylab('mean') +
  ggtitle("Monthly trend by year")
# ggsave(filename = paste0(ts_directory, "/", "monthly_trend_by_year.png"), monthly_trend_by_year)
rm(monthly_trend_by_year)




# =================== time series (MTBQ) =================== 
# transform to UTM
# remove NAs in coordinates
WISE6_SpatialObject_DerivedData_UTM <- WISE6_SpatialObject_DerivedData %>% filter(!is.na(lon))

# transformation
XY_df <- convertLonLat2UTM(WISE6_SpatialObject_DerivedData_UTM$lon, WISE6_SpatialObject_DerivedData_UTM$lat)

WISE6_SpatialObject_DerivedData_UTM$easting <- XY_df$X
WISE6_SpatialObject_DerivedData_UTM$northing <- XY_df$Y

# germany's shape file
sys <- "+proj=tmerc +lat_0=0 +lon_0=9 +k_0=1 +x_0=3500000 +y_0=0 +ellps=bessel +units=m"
germany_sp <- as(germany_shp, "Spatial")
germany_sp <- sp::spTransform(germany_sp, raster::crs(sys))
germany_sf <- germany_sp %>% sf::st_as_sf()


# produce raster files and maps for each variable, in each year
wb_waterquality_vars <- wb_dataCast_ts1$name %>% unique()
directory <- "processed/wisewb/excluded_winter_months/wisewb_raster"

raster_directory <- paste0(directory, "/raster")
dir.create(raster_directory)

map_directory <- paste0(directory, "/map")
dir.create(map_directory)


for (i in 1:length(wb_waterquality_vars)) {
  print(paste0("Rasterising: ", wb_waterquality_vars[i]))
  
  years <- c((water_quality_icm_DE %>% filter(observedPropertyDeterminandLabel == wb_waterquality_vars[i]))$year %>% 
               unique() %>% sort())
  print(years)
  
  variable <- wb_waterquality_vars[i]
  
  # create directory
  # raster directory
  raster_subdirectory <-paste0(raster_directory, '/', variable)
  dir.create(raster_subdirectory)
  
  # plot directory
  map_subdirectory <- paste0(map_directory, '/', variable)
  dir.create(map_subdirectory)
  
  # create raster
  for (j in 1:length(years)) {
    print(paste0("Year: ", years[j]))
    
    r <- water_quality_icm_DE %>% rename(name = observedPropertyDeterminandLabel,
                                         site_id = monitoringSiteIdentifier,
                                         value = resultObservedValue) %>% 
      left_join((WISE6_SpatialObject_DerivedData_UTM %>% 
                   select(-monitoringSiteIdentifierScheme) %>% 
                   rename(site_id = monitoringSiteIdentifier)), 
                by="site_id") %>% 
      filter(name==variable, year==years[j], !is.na(easting)) %>% 
      sf::st_as_sf(coords=c("easting", "northing"))
    
    # if r has values
    if (nrow(r) != 0) {
      sf::st_crs(r) <- CRS("+proj=tmerc +lat_0=0 +lon_0=9 +k_0=1 +x_0=3500000 +y_0=0 +ellps=bessel +units=m")
      
      raster_template <- raster(extent(germany_sf), resolution = 10000,
                                crs = sf::st_crs(r)$proj4string)
      
      ch_raster <- rasterize(r, raster_template, field='value', fun=mean)
      
    }
    
    # if raster doesn't contain only NA values
    if ((nrow(r) != 0) & ((is.na(ch_raster) %>% unique()) == FALSE)) {
      # save raster
      writeRaster(ch_raster, paste0(raster_subdirectory, '/', variable, '_', years[j], '.tif'))
      
      # plot raster
      raster_map <- tm_shape(ch_raster) +
        tm_raster(style='cont', palette='viridis')
      
      tmap_save(tm = raster_map, 
                filename = paste0(map_subdirectory, '/', variable, '_', years[j], ".png"))
    }

  }
    
  }


rm(directory, raster_directory, map_directory)
rm(i, j, years, variable, raster_subdirectory, map_subdirectory, r, raster_template, ch_raster, raster_map)
# save(water_quality_icm_DE, file="water_quality_icm_DE.rds")



# extracted data for each MTBQ 
MTBQS <- rgdal::readOGR(dsn="MTBQ",layer="MTBQ_25833")
MTBQ_directory <- paste0(directory, "/MTBQ")
dir.create(MTBQ_directory)

# function for extracting MTBQ
extractMTBQ <- function(variable) {
  # get years of interest
  myyears <- c((water_quality_icm_DE %>% filter(observedPropertyDeterminandLabel == variable))$year %>% 
                 unique() %>% sort())
  myyearsC <- paste(myyears,collapse="|")
  print(myyears)
  
  # get mean climate value per MTBQ, and convert to a df
  fdirectory <- paste0('processed/wisewb/excluded_winter_months/wisewb_raster/raster/', variable)
  yearfiles <- list.files(fdirectory)[sapply(list.files(fdirectory),function(x)grepl(myyearsC,x))]
  
  # for each year, get mean climate value per MTBQ, and convert to a df
  df <- plyr::ldply(yearfiles,function(x){
    r <- raster(paste(fdirectory,x,sep="/"))
    mtbqs <- spTransform(MTBQS,CRS(projection(r)))
    mtbqs$meanVal <- raster::extract(r,mtbqs,fun=median,na.rm=T)
    mtbqs$Year <- str_extract(x, "\\d{4}") %>% as.numeric() # add year to the data frame
    return(mtbqs@data)
  })
  
  return(df)
}


# EC
EC_MTBQ <- extractMTBQ("EC")
# saveRDS(EC_MTBQ, file=paste0("processed/wisewb/excluded_winter_months/wisewb_raster/MTBQ/EC_MTBQ.rds"))

# herbicide
herbicide_MTBQ <- extractMTBQ("herbicide")


PO4_MTBQ <- readRDS("processed/wisewb/excluded_winter_months/wisewb_raster/MTBQ/PO4_MTBQ.rds")



# function from "sMon/sMon-Analyses/Odonata_Git/sMon-insects/R/sparta_wrapper_functions.R"
addMTBQ <- function(mtbqs){
  
  mtbqs$MTB_Q= ifelse(mtbqs$Quadrant == "NW",paste(mtbqs$Value, 1, sep = ""),
                      ifelse(mtbqs$Quadrant == "NO", paste(mtbqs$Value, 2, sep = ""),
                             ifelse(mtbqs$Quadrant == "SW", paste(mtbqs$Value, 3, sep = ""),
                                    paste(mtbqs$Value, 4, sep = ""))))
  return(mtbqs)
}

# ecoregions data
load("mtbqsDF.RData")
mtbqsDF %>% tibble()
mtbqsDF$MTB_CoarseNatur <- gsub("NaturrÃ¤ume Deutschland/","",mtbqsDF$MTB_CoarseNatur)





PO4_MTBQ %>% 
  group_by(Year) %>% 
  summarise(meanVal = mean(meanVal, na.rm = T)) %>% 
  ggplot() +
  geom_line(aes(x=Year, y=meanVal))


#add ecoregions
PO4_MTBQ <- PO4_MTBQ %>% addMTBQ()
PO4_MTBQ$Natur <- mtbqsDF$MTB_CoarseNatur[match(PO4_MTBQ$MTB_Q,mtbqsDF$MTB_Q)]

#remove any without
PO4_MTBQ <- subset(PO4_MTBQ, !is.na(Natur))

PO4_MTBQ_summary <- PO4_MTBQ %>%
  group_by(Natur,Year) %>%
  summarise(meanValue = mean(meanVal,na.rm=T))

ggplot(PO4_MTBQ_summary) +
  geom_line(aes(x=Year,y=meanValue,colour=Natur),size=1.5)+
  theme_bw()+
  ggtitle("PO4_MTBQ")+
  theme(legend.position = "top")







# =================== pick no. of years threshold for trends ===================
# function for calculating trends
wb_calTrends <- function(df, year_number) {
  # get trend at each site_id
  wb_trends <- plyr::ddply(na.omit(df),c("name","site_id"),function(x){
    if(length(unique(x$year))>year_number){
      lm1 <- lm(meanVal ~ year, data=x)
      temp <- data.frame(t(summary(lm1)$coef[2,]))
      temp$minYear <- min(x$year)
      temp$maxYear <- max(x$year)
      return(temp)
    }
  })
  # merge with site data
  wb_trends <- merge(wb_trends,(WISE6_SpatialObject_DerivedData %>% 
                                  select(-monitoringSiteIdentifierScheme) %>% 
                                  rename(site_id = monitoringSiteIdentifier)), by="site_id") %>% tibble()
  return(wb_trends)
}


# minimum 11 unique years: 934 rows
wb_trends11 <- wb_calTrends(df=wb_summaryYear, year_number=10)
wb_trends11$site_id %>% unique() %>% length() # 528 unique sites

# minimum 9 unique years: 1,950 rows
wb_trends9 <- wb_calTrends(df=wb_summaryYear, year_number=8)
wb_trends9$site_id %>% unique() %>% length() # 727 unique sites

# minimum 8 unique years: 2,694 rows
wb_trends8 <- wb_calTrends(df=wb_summaryYear, year_number=7)
wb_trends8$site_id %>% unique() %>% length() # 813 unique sites

# minimum 7 unique years: 3,441 rows
wb_trends7 <- wb_calTrends(df=wb_summaryYear, year_number=6)
wb_trends7$site_id %>% unique() %>% length() # 853 unique sites

# minimum 6 unique years: 4,265 rows
wb_trends6 <- wb_calTrends(df=wb_summaryYear, year_number=5)
wb_trends6$site_id %>% unique() %>% length() # 909 unique sites

# minimum 5 unique years: 5,280 rows
wb_trends5 <- wb_calTrends(df=wb_summaryYear, year_number=4)
wb_trends5$site_id %>% unique() %>% length() # 941 unique sites

# minimum 3 unique years: 7,159 rows
wb_trends3 <- wb_calTrends(df=wb_summaryYear, year_number=2)
wb_trends3$site_id %>% unique() %>% length() # 1017 unique sites

rm(wb_calTrends)




# =================== trends ===================
# quick check on variables (lon, lat)
wb_trends6 %>% filter(name=="NH4") %>% 
  ggplot(aes(x=lon, y=lat))+
  geom_point(aes(colour=Estimate), alpha=0.7)+
  viridis::scale_colour_viridis() +
  ggtitle("NH4")


germany_bw_map %>% ggmap() +
  geom_point(data = (wb_trends6 %>% filter(name=="NH4")),
             aes(x=lon, y=lat, colour=Estimate), alpha=0.7) +
  viridis::scale_colour_viridis() +
  ggtitle("NH4")

# check for the outlier
wb_trends6 %>% filter(name=="NH4", Estimate<0)


# plot trends maps for all water quality variables
wb_waterquality_vars <- wb_trends6$name %>% unique()


# ggmap version
for (i in 1:length(wb_waterquality_vars)) {
  print(paste0("Mapping: ", wb_waterquality_vars[i]))
  
  # black and white
  trendsMap2 <- germany_bw_map %>% ggmap() +
    geom_point(data = (wb_trends6 %>% filter(name==wb_waterquality_vars[i])),
               aes(x=lon, y=lat, colour=Estimate), alpha=0.7) +
    viridis::scale_colour_viridis() +
    ggtitle(wb_waterquality_vars[i])
  
  ggsave(filename = paste0("processed/wisewb/excluded_winter_months/wisewb_trends_maps_by_site_min6years_(ggmap_version)/", wb_waterquality_vars[i], "_Trends_bw.png"), 
         trendsMap2)
  
  # rm(latMax, latMin, lonMax, lonMin, trendsMap1, trendsMap2)
}
rm(i, trendsMap2)



# tmap version
trends_tmap_directory <- "processed/wisewb/excluded_winter_months/wisewb_trends_maps_by_site_min6years"

tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(wb_trends6 %>% filter(name=="herbicide", !is.na(lon)) %>% sf::st_as_sf(coords=c("lon", "lat"))) +
  tm_dots(col="Estimate", size=0.15, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "herbicide")


for (i in 1:length(wb_waterquality_vars)) {
  print(paste0("Mapping: ", wb_waterquality_vars[i]))
  
  # default map
  trendsMap <- tm_shape(germany_shp) +
    tm_polygons(lwd = 0.5) +
    tm_shape(wb_trends6 %>% filter(name==wb_waterquality_vars[i], !is.na(lon)) %>% sf::st_as_sf(coords=c("lon", "lat"))) +
    tm_dots(col="Estimate", size=0.15, alpha=0.7, palette = "viridis", midpoint = NA) +
    tm_layout(legend.outside = TRUE, main.title = wb_waterquality_vars[i])
  
  tmap_save(tm = trendsMap, 
            filename = paste0(trends_tmap_directory, "/", wb_waterquality_vars[i],
                              "_Trends.png"))
  
  rm(trendsMap)
}
rm(i)


# check outliers
# herbicide
trendsMap <- tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(wb_trends6 %>% filter(name=="herbicide", !is.na(lon), Estimate <= 0.15) %>% sf::st_as_sf(coords=c("lon", "lat"))) +
  tm_dots(col="Estimate", size=0.15, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "herbicide")
tmap_save(tm = trendsMap, 
          filename = paste0(trends_tmap_directory, "/", "herbicide", "_Trends_noOutliers.png"))

# NH4_Trends
trendsMap <- tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(wb_trends6 %>% filter(name=="NH4", !is.na(lon), Estimate >= -3) %>% sf::st_as_sf(coords=c("lon", "lat"))) +
  tm_dots(col="Estimate", size=0.15, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "NH4")
tmap_save(tm = trendsMap, 
          filename = paste0(trends_tmap_directory, "/", "NH4", "_Trends_noOutliers.png"))

# NO2_Trends
trendsMap <- tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(wb_trends6 %>% filter(name=="NO2", !is.na(lon), Estimate >= -3) %>% sf::st_as_sf(coords=c("lon", "lat"))) +
  tm_dots(col="Estimate", size=0.15, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "NO2")
tmap_save(tm = trendsMap, 
          filename = paste0(trends_tmap_directory, "/", "NO2", "_Trends_noOutliers.png"))

# NO3_Trends
trendsMap <- tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(wb_trends6 %>% filter(name=="NO3", !is.na(lon), Estimate < 10) %>% sf::st_as_sf(coords=c("lon", "lat"))) +
  tm_dots(col="Estimate", size=0.15, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "NO3")
tmap_save(tm = trendsMap, 
          filename = paste0(trends_tmap_directory, "/", "NO3", "_Trends_noOutliers.png"))

# PO4_Trends
trendsMap <- tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(wb_trends6 %>% filter(name=="PO4", !is.na(lon), Estimate > -0.3) %>% sf::st_as_sf(coords=c("lon", "lat"))) +
  tm_dots(col="Estimate", size=0.15, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "PO4")
tmap_save(tm = trendsMap, 
          filename = paste0(trends_tmap_directory, "/", "PO4", "_Trends_noOutliers.png"))

rm(trends_tmap_directory, trendsMap)




# =================== mean maps ===================
means_tmap_directory <- "processed/wisewb/excluded_winter_months/wisewb_means_maps_by_site_min8years"

# function for calculating means
wb_calMeans <- function(df, year_number) {
  # get mean at each site_id
  wb_means <- plyr::ddply(na.omit(df),c("name","site_id"),function(x){
    if(length(unique(x$year))>year_number){
      
      temp <- x %>% 
        group_by(site_id, name) %>% 
        summarise(meanVal = mean(meanVal, na.rm=T))
      temp$minYear <- min(x$year)
      temp$maxYear <- max(x$year)
      return(temp)
    }
  })
  # merge with site data
  wb_means <- merge(wb_means,(WISE6_SpatialObject_DerivedData %>% 
                                select(-monitoringSiteIdentifierScheme) %>% 
                                rename(site_id = monitoringSiteIdentifier)), by="site_id") %>% tibble()
  return(wb_means)
}

# minimum 8 years
wb_means8 <- wb_calMeans(df=wb_summaryYear, year_number=7)


# plot means maps for all water quality variables
wb_waterquality_vars <- wb_means8$name %>% unique()


tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(wb_means8 %>% filter(name=="NO3", !is.na(lon)) %>% sf::st_as_sf(coords=c("lon", "lat"))) +
  tm_dots(col="meanVal", size=0.15, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "NO3")


for (i in 1:length(wb_waterquality_vars)) {
  print(paste0("Mapping: ", wb_waterquality_vars[i]))
  
  # default map
  meansMap <- tm_shape(germany_shp) +
    tm_polygons(lwd = 0.5) +s
  tm_shape(wb_means8 %>% filter(name==wb_waterquality_vars[i], !is.na(lon)) %>% sf::st_as_sf(coords=c("lon", "lat"))) +
    tm_dots(col="meanVal", size=0.15, alpha=0.7, palette = "viridis", midpoint = NA) +
    tm_layout(legend.outside = TRUE, main.title = wb_waterquality_vars[i])
  
  tmap_save(tm = meansMap, 
            filename = paste0(means_tmap_directory, "/", wb_waterquality_vars[i],
                              "_Means.png"))
  
  rm(meansMap)
}
rm(i)


# check outliers
meansMap <- tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(wb_means8 %>% filter(name=="EC", !is.na(lon), meanVal < 300) %>% sf::st_as_sf(coords=c("lon", "lat"))) +
  tm_dots(col="meanVal", size=0.15, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "EC")
tmap_save(tm = meansMap, 
          filename = paste0(means_tmap_directory, "/", "EC", "_Means_noOutliers.png"))

meansMap <- tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(wb_means8 %>% filter(name=="herbicide", !is.na(lon), meanVal < 0.6) %>% sf::st_as_sf(coords=c("lon", "lat"))) +
  tm_dots(col="meanVal", size=0.15, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "herbicide")
tmap_save(tm = meansMap, 
          filename = paste0(means_tmap_directory, "/", "herbicide", "_Means_noOutliers.png"))

meansMap <- tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(wb_means8 %>% filter(name=="NH4", !is.na(lon), meanVal < 15) %>% sf::st_as_sf(coords=c("lon", "lat"))) +
  tm_dots(col="meanVal", size=0.15, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "NH4")
tmap_save(tm = meansMap, 
          filename = paste0(means_tmap_directory, "/", "NH4", "_Means_noOutliers.png"))

meansMap <- tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(wb_means8 %>% filter(name=="NO2", !is.na(lon), meanVal < 0.3) %>% sf::st_as_sf(coords=c("lon", "lat"))) +
  tm_dots(col="meanVal", size=0.15, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "NO2")
tmap_save(tm = meansMap, 
          filename = paste0(means_tmap_directory, "/", "NO2", "_Means_noOutliers.png"))

meansMap <- tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(wb_means8 %>% filter(name=="NO3", !is.na(lon), meanVal < 200) %>% sf::st_as_sf(coords=c("lon", "lat"))) +
  tm_dots(col="meanVal", size=0.15, alpha=0.7, palette = "viridis", midpoint = NA, n=7) +
  tm_layout(legend.outside = TRUE, main.title = "NO3")
tmap_save(tm = meansMap, 
          filename = paste0(means_tmap_directory, "/", "NO3", "_Means_noOutliers.png"))

rm(means_tmap_directory, wb_calMeans, meansMap)
rm(eda_directory, ts_directory)






