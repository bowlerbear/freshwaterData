
library(tidyverse)
library(ggmap)
library(tmap)
source("00-utils.R")


# =================== prepare data for time series =================== 
# change to long format
gs_dataCast_ts1 <- gs_dataCast %>% pivot_longer(cols=4:length(gs_dataCast))

# add year, month, date without year
gs_dataCast_ts1 <- gs_dataCast_ts1 %>% 
  rename(date = SampleDate,
         site_id = GEMSStationNumber) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         monthDate = strftime(date, "%m-%d"))


# mean means per year (removed entries on 1st Jan)
gs_summaryYear <- plyr::ddply((gs_dataCast_ts1 %>% filter(monthDate != "01-01")),c("name","year","site_id"),
                              summarise, meanVal = median(value,na.rm=T))
gs_summaryYear <- gs_summaryYear %>% tibble()

# check proportion of NA values
gs_summaryYear %>% is.na() %>% sum()
(gs_summaryYear %>% is.na() %>% sum())/nrow(gs_summaryYear) # 74.7% NA




# =================== time series =================== 




# =================== pick no. of years threshold for trends ===================
# function for calculating trends
gs_calTrends <- function(df, year_number) {
  # get trend at each site_id
  gs_trends <- plyr::ddply(na.omit(df),c("name","site_id"),function(x){
    if(length(unique(x$year))>year_number){
      lm1 <- lm(meanVal ~ year, data=x)
      temp <- data.frame(t(summary(lm1)$coef[2,]))
      temp$minYear <- min(x$year)
      temp$maxYear <- max(x$year)
      return(temp)
    }
  })
  # merge with site data
  gs_trends <- merge(gs_trends,(geomstat_meta_station %>% 
                                  select(c("GEMSStationNumber", "WaterType", "MainBasin",
                                           "Latitude", "Longitude")) %>% 
                                  rename(site_id = GEMSStationNumber)), by="site_id") %>% tibble()
  return(gs_trends)
}


# minimum 11 unique years: 1,056 rows
gs_trends11 <- gs_calTrends(df=gs_summaryYear, year_number=10)
gs_trends11$site_id %>% unique() %>% length() # 173 unique sites

# minimum 9 unique years: 1,492 rows
gs_trends9 <- gs_calTrends(df=gs_summaryYear, year_number=8)
gs_trends9$site_id %>% unique() %>% length() # 261 unique sites

# minimum 8 unique years: 1,663 rows
gs_trends8 <- gs_calTrends(df=gs_summaryYear, year_number=7)
gs_trends8$site_id %>% unique() %>% length() # 274 unique sites

# minimum 7 unique years: 1,832 rows
gs_trends7 <- gs_calTrends(df=gs_summaryYear, year_number=6)
gs_trends7$site_id %>% unique() %>% length() # 320 unique sites

# minimum 6 unique years: 2,124 rows
gs_trends6 <- gs_calTrends(df=gs_summaryYear, year_number=5)
gs_trends6$site_id %>% unique() %>% length() # 368 unique sites

# minimum 5 unique years: 2,460 rows
gs_trends5 <- gs_calTrends(df=gs_summaryYear, year_number=4)
gs_trends5$site_id %>% unique() %>% length() # 418 unique sites

# minimum 3 unique years: 3,305 rows
gs_trends3 <- gs_calTrends(df=gs_summaryYear, year_number=2)
gs_trends3$site_id %>% unique() %>% length() # 698 unique sites




# =================== trends ===================
# plot trends maps for all water quality variables
gs_waterquality_vars <- gs_trends6$name %>% unique()

# ggmap version
for (i in 1:length(gs_waterquality_vars)) {
  print(paste0("Mapping: ", gs_waterquality_vars[i]))
  
  # black and white
  trendsMap2 <- germany_bw_map %>% ggmap() +
    geom_point(data = (gs_trends6 %>% filter(name==gs_waterquality_vars[i])),
               aes(x=Longitude, y=Latitude, colour=Estimate), alpha=0.7) +
    viridis::scale_colour_viridis() +
    ggtitle(gs_waterquality_vars[i])
  
  ggsave(filename = paste0("processed/gemstat_trends_maps_by_site_min6years_(ggmap_version)/", gs_waterquality_vars[i], "_Trends_bw.png"), 
         trendsMap2)
  
  # rm(latMax, latMin, lonMax, lonMin, trendsMap1, trendsMap2)
}
rm(i)


# tmap version
for (i in 1:length(gs_waterquality_vars)) {
  print(paste0("Mapping: ", gs_waterquality_vars[i]))
  
  # default map
  trendsMap <- tm_shape(germany_shp) +
    tm_polygons(lwd = 0.5) +
    tm_shape(gs_trends6 %>% filter(name==gs_waterquality_vars[i], !is.na(Longitude)) %>% sf::st_as_sf(coords=c("Longitude", "Latitude"))) +
    tm_dots(col="Estimate", size=0.1, alpha=0.7, palette = "viridis", midpoint = NA) +
    tm_layout(legend.outside = TRUE, main.title = gs_waterquality_vars[i])
  
  tmap_save(tm = trendsMap, 
            filename = paste0("processed/gemstat_trends_maps_by_site_min6years/", gs_waterquality_vars[i],
                              "_Trends.png"))
  
  rm(trendsMap)
}
rm(i)



