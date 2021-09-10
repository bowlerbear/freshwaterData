
library(tidyverse)
library(ggmap)
library(tmap)


# =================== pick no. of years threshold for trends ===================
# using the summary Year df prepared for time series

# function for calculating trends
calTrends <- function(df, year_number) {
  # get trend at each site_id
  trends <- plyr::ddply(na.omit(df),c("name","site_id"),function(x){
    if(length(unique(x$year))>year_number){
      lm1 <- lm(meanVal ~ year, data=x)
      temp <- data.frame(t(summary(lm1)$coef[2,]))
      temp$minYear <- min(x$year)
      temp$maxYear <- max(x$year)
      return(temp)
    }
  })
  # merge with site data
  trends <- merge(trends, sites, by="site_id") %>% tibble()
}


# minimum 11 unique years
trends11 <- calTrends(df=summaryYear, year_number=10)

# minimum 9 unique years
trends9 <- calTrends(df=summaryYear, year_number=8)

# minimum 8 unique years
trends8 <- calTrends(df=summaryYear, year_number=7)

# minimum 6 unique years
trends6 <- calTrends(df=summaryYear, year_number=5)

# minimum 5 unique years
trends5 <- calTrends(df=summaryYear, year_number=4)

# minimum 3 unique years
trends3 <- calTrends(df=summaryYear, year_number=2)




# min 11 years: 6319 rows
trends11$site_id %>% unique() %>% length() # 438 unique sites
trends11$state %>% unique()
# [1] "Baden-Württemberg"   "Bayern"              "Niedersachsen"       "Nordrhein-Westfalen" "Rheinland-Pfalz"     "Sachsen"            
# [7] "Thüringen"      

# min 9 years: 8688 rows
trends9$site_id %>% unique() %>% length() # 614 unique sites
trends9$state %>% unique()
# [1] "Baden-Württemberg"   "Bayern"              "Hessen"              "Niedersachsen"       "Nordrhein-Westfalen" "Rheinland-Pfalz"    
# [7] "Sachsen"             "Thüringen"   

# min 8 years: 10805 rows
trends8$site_id %>% unique() %>% length() # 774 unique sites
trends8$state %>% unique()
# [1] "Baden-Württemberg"   "Bayern"              "Hessen"              "Niedersachsen"       "Nordrhein-Westfalen" "Rheinland-Pfalz"    
# [7] "Schleswig-Holstein"  "Sachsen"             "Sachsen-Anhalt"      "Thüringen"  

# min 6 years: 17414 rows
trends6$site_id %>% unique() %>% length() # 1248 unique sites
trends6$state %>% unique()
# [1] "Baden-Württemberg"   "Bayern"              "Hessen"              "Niedersachsen"       "Nordrhein-Westfalen" "Rheinland-Pfalz"    
# [7] "Schleswig-Holstein"  "Sachsen"             "Sachsen-Anhalt"      "Thüringen" 

# min 5 years: 22181 rows
trends5$site_id %>% unique() %>% length() # 1567 unique sites
trends5$state %>% unique()
# [1] "Baden-Württemberg"      "Bayern"                 "Hessen"                 "Mecklenburg-Vorpommern" "Niedersachsen"         
# [6] "Nordrhein-Westfalen"    "Rheinland-Pfalz"        "Schleswig-Holstein"     "Sachsen"                "Sachsen-Anhalt"        
# [11] "Thüringen"      

# min 3 years: 32695 rows
trends3$site_id %>% unique() %>% length() #  2422 unique sites
trends3$state %>% unique()
# [1] "Brandenburg"            "Baden-Württemberg"      "Bayern"                 "Hessen"                 "Mecklenburg-Vorpommern"
# [6] "Niedersachsen"          "Nordrhein-Westfalen"    "Rheinland-Pfalz"        "Schleswig-Holstein"     "Sachsen"               
# [11] "Sachsen-Anhalt"         "Thüringen"   




# =================== trend maps, by sites ===================
# chosen threshold
# trends5


# tmap version
trends_tmap_directory <- "processed/freshwater_trends_maps_by_site_min5years"

# example
tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(trends5 %>% filter(name=="oPO4") %>% sf::st_as_sf(coords=c("lon", "lat"), crs=sf::st_crs(germany_shp))) +
  tm_dots(col="Estimate", size=0.1, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "oPO4")


waterquality_vars <- trends5$name %>% unique()

for (i in 1:length(waterquality_vars)) {
  print(paste0("Mapping: ", waterquality_vars[i]))
  
  # default map
  trendsMap <- tm_shape(germany_shp) +
    tm_polygons(lwd = 0.5) +
    tm_shape(trends5 %>% filter(name==waterquality_vars[i]) %>% sf::st_as_sf(coords=c("lon", "lat"), crs=sf::st_crs(germany_shp))) +
    tm_dots(col="Estimate", size=0.1, alpha=0.7, palette = "viridis", midpoint = NA) +
    tm_layout(legend.outside = TRUE, main.title = waterquality_vars[i])
  
  tmap_save(tm = trendsMap, 
            filename = paste0(trends_tmap_directory, "/", waterquality_vars[i], "_Trends.png"))
}
rm(waterquality_vars, i, trendsMap)


# check outliers
trendsMap <- tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(trends5 %>% filter(name=="CO3", Estimate > -0.4) %>% sf::st_as_sf(coords=c("lon", "lat"), crs=sf::st_crs(germany_shp))) +
  tm_dots(col="Estimate", size=0.1, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "CO3")
tmap_save(tm = trendsMap, 
          filename = paste0(trends_tmap_directory, "/", "CO3", "_Trends_noOutliers.png"))

trendsMap <- tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(trends5 %>% filter(name=="filtarable.substances", Estimate < 10) %>% sf::st_as_sf(coords=c("lon", "lat"), crs=sf::st_crs(germany_shp))) +
  tm_dots(col="Estimate", size=0.1, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "filtarable.substances")
tmap_save(tm = trendsMap, 
          filename = paste0(trends_tmap_directory, "/", "filtarable.substances", "_Trends_noOutliers.png"))

trendsMap <- tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(trends5 %>% filter(name=="NO2N", Estimate < 0.1) %>% sf::st_as_sf(coords=c("lon", "lat"), crs=sf::st_crs(germany_shp))) +
  tm_dots(col="Estimate", size=0.1, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "NO2N")
tmap_save(tm = trendsMap, 
          filename = paste0(trends_tmap_directory, "/", "NO2N", "_Trends_noOutliers.png"))

trendsMap <- tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(trends5 %>% filter(name=="NO3", Estimate < 10) %>% sf::st_as_sf(coords=c("lon", "lat"), crs=sf::st_crs(germany_shp))) +
  tm_dots(col="Estimate", size=0.1, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "NO3")
tmap_save(tm = trendsMap, 
          filename = paste0(trends_tmap_directory, "/", "NO3", "_Trends_noOutliers.png"))

trendsMap <- tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(trends5 %>% filter(name=="nonionised.NH3N", Estimate > -0.015) %>% sf::st_as_sf(coords=c("lon", "lat"), crs=sf::st_crs(germany_shp))) +
  tm_dots(col="Estimate", size=0.1, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "nonionised.NH3N")
tmap_save(tm = trendsMap, 
          filename = paste0(trends_tmap_directory, "/", "nonionised.NH3N", "_Trends_noOutliers.png"))

# trendsMap <- tm_shape(germany_shp) +
#   tm_polygons(lwd = 0.5) +
#   tm_shape(trends5 %>% filter(name=="O2.saturation", Estimate > -10 & Estimate < 15) %>% sf::st_as_sf(coords=c("lon", "lat"), crs=sf::st_crs(germany_shp))) +
#   tm_dots(col="Estimate", size=0.1, alpha=0.7, palette = "viridis", midpoint = NA) +
#   tm_layout(legend.outside = TRUE, main.title = "O2.saturation")
# tmap_save(tm = trendsMap, 
#           filename = paste0(trends_tmap_directory, "/", "O2.saturation", "_Trends_noOutliers.png"))

trendsMap <- tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(trends5 %>% filter(name=="oPO4", Estimate < 0.6) %>% sf::st_as_sf(coords=c("lon", "lat"), crs=sf::st_crs(germany_shp))) +
  tm_dots(col="Estimate", size=0.1, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "oPO4")
tmap_save(tm = trendsMap, 
          filename = paste0(trends_tmap_directory, "/", "oPO4", "_Trends_noOutliers.png"))

trendsMap <- tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(trends5 %>% filter(name=="oPO4P", Estimate < 10) %>% sf::st_as_sf(coords=c("lon", "lat"), crs=sf::st_crs(germany_shp))) +
  tm_dots(col="Estimate", size=0.1, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "oPO4P")
tmap_save(tm = trendsMap, 
          filename = paste0(trends_tmap_directory, "/", "oPO4P", "_Trends_noOutliers.png"))

trendsMap <- tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(trends5 %>% filter(name=="pH.field", Estimate < 0.2) %>% sf::st_as_sf(coords=c("lon", "lat"), crs=sf::st_crs(germany_shp))) +
  tm_dots(col="Estimate", size=0.1, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "pH.field")
tmap_save(tm = trendsMap, 
          filename = paste0(trends_tmap_directory, "/", "pH.field", "_Trends_noOutliers.png"))

trendsMap <- tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(trends5 %>% filter(name=="tot.hardness", Estimate > -10) %>% sf::st_as_sf(coords=c("lon", "lat"), crs=sf::st_crs(germany_shp))) +
  tm_dots(col="Estimate", size=0.1, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "tot.hardness")
tmap_save(tm = trendsMap, 
          filename = paste0(trends_tmap_directory, "/", "tot.hardness", "_Trends_noOutliers.png"))

trendsMap <- tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(trends5 %>% filter(name=="tot.N", Estimate > -2) %>% sf::st_as_sf(coords=c("lon", "lat"), crs=sf::st_crs(germany_shp))) +
  tm_dots(col="Estimate", size=0.1, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "tot.N")
tmap_save(tm = trendsMap, 
          filename = paste0(trends_tmap_directory, "/", "tot.N", "_Trends_noOutliers.png"))

trendsMap <- tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(trends5 %>% filter(name=="tot.orgBound.N", Estimate > -0.6) %>% sf::st_as_sf(coords=c("lon", "lat"), crs=sf::st_crs(germany_shp))) +
  tm_dots(col="Estimate", size=0.1, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "tot.orgBound.N")
tmap_save(tm = trendsMap, 
          filename = paste0(trends_tmap_directory, "/", "tot.orgBound.N", "_Trends_noOutliers.png"))

trendsMap <- tm_shape(germany_shp) +
  tm_polygons(lwd = 0.5) +
  tm_shape(trends5 %>% filter(name=="tot.P", Estimate < 10) %>% sf::st_as_sf(coords=c("lon", "lat"), crs=sf::st_crs(germany_shp))) +
  tm_dots(col="Estimate", size=0.1, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "tot.P")
tmap_save(tm = trendsMap, 
          filename = paste0(trends_tmap_directory, "/", "tot.P", "_Trends_noOutliers.png"))

rm(trendsMap)
rm(trends_tmap_directory)



# =================== trend maps, by state ===================
# test
trends5 %>% filter(name=="tot.P") %>% 
  ggplot(aes(x=lon, y=lat))+
  geom_point(aes(colour=Estimate, shape=imputed_coor), alpha=0.7)+
  viridis::scale_colour_viridis() 

trends5 %>% filter(name=="H2O.temp") %>% 
  group_by(state) %>% 
  summarise(mean_estimate = mean(Estimate),
            nuSites = n_distinct(site_id))

germany_shp %>% 
  rename(state=NAME_1) %>% 
  left_join(
    (trends5 %>% filter(name=="H2O.temp") %>% 
       group_by(state) %>% 
       summarise(mean_estimate = mean(Estimate),
                 nuSites = n_distinct(site_id))),
    by="state") %>% 
  tm_shape() + 
  tm_polygons(col="mean_estimate", palette = "-RdBu", midpoint = NA) +
  tm_text(text="nuSites") +
  tm_layout(legend.outside = TRUE, main.title = "H2O.temp") +
  tm_credits(text="# = num. of sampled sites")




# plot trends maps for all water quality variables
waterquality_vars <- trends5$name %>% unique()

for (i in 1:length(waterquality_vars)) {
  print(paste0("Mapping: ", waterquality_vars[i]))
  
  # default map
  trendsMap_byState <- germany_shp %>% 
    rename(state=NAME_1) %>% 
    left_join(
      (trends5 %>% filter(name==waterquality_vars[i]) %>% 
         group_by(state) %>% 
         summarise(mean_estimate = mean(Estimate),
                   nuSites = n_distinct(site_id))),
      by="state") %>% 
    tm_shape() + 
    tm_polygons(col="mean_estimate", palette = "-RdBu", midpoint = NA) +
    tm_text(text="nuSites") +
    tm_layout(legend.outside = TRUE, main.title = waterquality_vars[i]) +
    tm_credits(text="# = num. of sampled sites")
  
  tmap_save(tm = trendsMap_byState, 
            filename = paste0("processed/freshwater_trends_maps_by_state_min5years/", waterquality_vars[i], "_Trends.png"))
}
rm(waterquality_vars, trendsMap_byState, i)





# =================== trend maps, by sites, Sachsen ===================
# tmap version
tm_shape(germany_shp %>% filter(NAME_1 == "Sachsen")) +
  tm_polygons(lwd = 0.5) +
  tm_shape(trends8 %>% filter(name=="oPO4", state=="Sachsen") %>% sf::st_as_sf(coords=c("lon", "lat"), crs=sf::st_crs(germany_shp))) +
  tm_dots(col="Estimate", size=0.1, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "oPO4")


waterquality_vars <- (trends8 %>% filter(state=="Sachsen"))$name %>% unique()

for (i in 1:length(waterquality_vars)) {
  print(paste0("Mapping: ", waterquality_vars[i]))
  
  # default map
  trendsMap <- tm_shape(germany_shp %>% filter(NAME_1 == "Sachsen")) +
    tm_polygons(lwd = 0.5) +
    tm_shape(trends8 %>% filter(name==waterquality_vars[i], state=="Sachsen") %>% sf::st_as_sf(coords=c("lon", "lat"), crs=sf::st_crs(germany_shp))) +
    tm_dots(col="Estimate", size=0.1, alpha=0.7, palette = "viridis", midpoint = NA) +
    tm_layout(legend.outside = TRUE, main.title = waterquality_vars[i])
  
  tmap_save(tm = trendsMap, 
            filename = paste0("processed/freshwater_trends_maps_by_site_Sachsen_min8years/", waterquality_vars[i], "_Trends.png"))
}
rm(waterquality_vars, i, trendsMap)


# check outliers
trendsMap <- tm_shape(germany_shp %>% filter(NAME_1 == "Sachsen")) +
  tm_polygons(lwd = 0.5) +
  tm_shape(trends8 %>% filter(name=="EC_25dC", state=="Sachsen", Estimate > -150) %>% sf::st_as_sf(coords=c("lon", "lat"), crs=sf::st_crs(germany_shp))) +
  tm_dots(col="Estimate", size=0.1, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "EC_25dC")
tmap_save(tm = trendsMap, 
          filename = paste0("processed/freshwater_trends_maps_by_site_Sachsen_min8years/", "EC_25dC", "_Trends_noOutliers.png"))

trendsMap <- tm_shape(germany_shp %>% filter(NAME_1 == "Sachsen")) +
  tm_polygons(lwd = 0.5) +
  tm_shape(trends8 %>% filter(name=="HCO3", state=="Sachsen", Estimate < 20) %>% sf::st_as_sf(coords=c("lon", "lat"), crs=sf::st_crs(germany_shp))) +
  tm_dots(col="Estimate", size=0.1, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "HCO3")
tmap_save(tm = trendsMap, 
          filename = paste0("processed/freshwater_trends_maps_by_site_Sachsen_min8years/", "HCO3", "_Trends_noOutliers.png"))


rm(trendsMap)
rm(calTrends, trends11, trends9, trends6, trends3)






