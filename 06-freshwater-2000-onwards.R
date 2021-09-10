
library(tidyverse)
library(ggmap)
library(tmap)


# from 2000 onwards
# using the summaryYear for time series in another script
summaryYear_2000 <- summaryYear %>% tibble() %>% filter(year >= 2000)
summaryYear_imp_2000 <- summaryYear_imp %>% tibble() %>% filter(year >= 2000)


# =================== trend maps, by state ===================
# get trend at each site_id (minimum 5 unique years)
trends5_2000 <- plyr::ddply(na.omit(summaryYear_2000),c("name","site_id"),function(x){
  if(length(unique(x$year))>4){
    lm1 <- lm(meanVal ~ year, data=x)
    temp <- data.frame(t(summary(lm1)$coef[2,]))
    temp$minYear <- min(x$year)
    temp$maxYear <- max(x$year)
    return(temp)
  }
})
# merge with site data
trends5_2000 <- merge(trends5_2000, sites, by="site_id")
trends5_2000 <- trends5_2000 %>% tibble()

# min 5 years: 18,008 rows
trends5_2000$site_id %>% unique() %>% length() # 1167 unique sites
trends5_2000$state %>% unique()
# [1] "Baden-Württemberg"      "Bayern"                 "Hessen"                 "Mecklenburg-Vorpommern"
# [5] "Niedersachsen"          "Nordrhein-Westfalen"    "Rheinland-Pfalz"        "Schleswig-Holstein"    
# [9] "Sachsen"                "Sachsen-Anhalt"         "Thüringen"       



# plot trends maps for all water quality variables
waterquality_vars <- trends5_2000$name %>% unique()

for (i in 1:length(waterquality_vars)) {
  print(paste0("Mapping: ", waterquality_vars[i]))
  
  # default map
  trendsMap_byState <- germany_shp %>% 
    rename(state=NAME_1) %>% 
    left_join(
      (trends5_2000 %>% filter(name==waterquality_vars[i]) %>% 
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
            filename = paste0("processed/freshwater_trends_maps_by_state_min5years_2000_onwards/", waterquality_vars[i],
                              "_Trends.png"))
}
rm(waterquality_vars, trendsMap_byState, i)




# =================== time series (before imputation) =================== 
ts_2000_directory <- "processed/freshwater_time_series_2000_onwards"

# annual trends -- splines
annual_trend_geom_smooth <- summaryYear_2000 %>% ggplot(aes(x=year,y=meanVal)) +
  geom_smooth() +
  facet_wrap(~name,scales="free") +
  ylab('mean') +
  ggtitle("Annual trend")
# ggsave(filename = paste0(ts_2000_directory, "/", "annual_trend_geom_smooth.png"), annual_trend_geom_smooth)
rm(annual_trend_geom_smooth)

# annual trends -- splines and points
annual_trend_geom_smooth_point <- summaryYear_2000 %>% ggplot(aes(x=year,y=meanVal)) +
  geom_point(colour="purple", size=0.2) +
  geom_smooth() +
  facet_wrap(~name,scales="free") +
  ylab('mean') +
  ggtitle("Annual trend")
# ggsave(filename = paste0(ts_2000_directory, "/", "annual_trend_geom_smooth_point.png"), annual_trend_geom_smooth_point)
rm(annual_trend_geom_smooth_point)

# annual trends -- lines
# summaryYear_2000 %>% ggplot(aes(x=year,y=meanVal)) +
#   geom_line() +
#   facet_wrap(~name,scales="free") +
#   ylab('annual median') +
#   ggtitle("Annual trend")

annual_trend_geom_line <- summaryYear_2000 %>% 
  group_by(name, year) %>% 
  summarise(meanVal=mean(meanVal, na.rm=T)) %>% 
  ggplot(aes(x=year,y=meanVal)) +
  geom_line() +
  facet_wrap(~name,scales="free") +
  ylab('mean') +
  ggtitle("Annual trend")
# ggsave(filename = paste0(ts_2000_directory, "/", "annual_trend_geom_line.png"), annual_trend_geom_line)
rm(annual_trend_geom_line)


# annual trends by site
# summaryYear_2000 %>% ggplot(aes(x=year,y=meanVal)) +
#   geom_line(aes(group=site_id)) +
#   facet_wrap(~name,scales="free") +
#   ggtitle("Annual trend by site")

# annual trends by state
# summaryYear_2000 %>% ggplot(aes(x=year,y=meanVal)) +
#   geom_line(aes(colour=state)) +
#   facet_wrap(~name,scales="free") +
#   ggtitle("Annual trend by state")

annual_trend_by_state <- summaryYear_2000 %>% 
  group_by(name, year, state) %>% 
  summarise(meanVal=mean(meanVal, na.rm=T)) %>% 
  ggplot(aes(x=year,y=meanVal)) +
  geom_line(aes(colour=state)) +
  facet_wrap(~name,scales="free") +
  ylab('mean') +
  ggtitle("Annual trend by state")
# ggsave(filename = paste0(ts_2000_directory, "/", "annual_trend_by_state.png"), annual_trend_by_state)
rm(annual_trend_by_state)


# trend using individual dates
trend_plot <- dataCast_ts1 %>% 
  filter(year >= 2000, monthDate != "01-01") %>% 
  group_by(name, date, state) %>% 
  summarise(meanVal=mean(value, na.rm=T)) %>% 
  ggplot(aes(x=date,y=meanVal)) +
  geom_line() +
  facet_wrap(~name,scales="free") +
  ylab('value (by day)') +
  ggtitle("Trend")
# ggsave(filename = paste0(ts_2000_directory, "/", "trend.png"), trend_plot)
rm(trend_plot)



# monthly trends -- lines
monthly_trend_geom_line <- dataCast_ts1 %>% 
  filter(year >= 2000, monthDate != "01-01") %>% 
  group_by(name, month) %>% 
  summarise(meanVal=mean(value, na.rm=T)) %>% 
  ggplot(aes(x=month, y=meanVal)) +
  geom_line() +
  facet_wrap(~name,scales="free") +
  scale_x_continuous(breaks=seq(1,12,1)) +
  ylab('mean') +
  ggtitle("Monthly trend")
# ggsave(filename = paste0(ts_2000_directory, "/", "monthly_trend_geom_line.png"), monthly_trend_geom_line)
rm(monthly_trend_geom_line)

# monthly trends -- splines
monthly_trend_geom_smooth <- dataCast_ts1 %>% 
  filter(year >= 2000, monthDate != "01-01") %>% 
  group_by(name, month) %>% 
  summarise(meanVal=mean(value, na.rm=T)) %>% 
  ggplot(aes(x=month, y=meanVal)) +
  geom_smooth() +
  facet_wrap(~name,scales="free") +
  scale_x_continuous(breaks=seq(1,12,1)) +
  ylab('mean') +
  ggtitle("Monthly trend")
# ggsave(filename = paste0(ts_2000_directory, "/", "monthly_trend_geom_smooth.png"), monthly_trend_geom_smooth)
rm(monthly_trend_geom_smooth)

# monthly trends by year
monthly_trend_by_year <- dataCast_ts1 %>% 
  filter(year >= 2000, monthDate != "01-01") %>% 
  group_by(name, year, month) %>% 
  summarise(meanVal=mean(value, na.rm=T)) %>% 
  ggplot(aes(x=month, y=meanVal)) +
  geom_line(aes(colour=factor(year))) + 
  facet_wrap(~name,scales="free") +
  scale_x_continuous(breaks=seq(1,12,1)) +
  ylab('mean') +
  ggtitle("Monthly trend by year")
# ggsave(filename = paste0(ts_2000_directory, "/", "monthly_trend_by_year.png"), monthly_trend_by_year)
rm(monthly_trend_by_year)


# monthly trends by state
monthly_trend_by_state <- dataCast_ts1 %>% 
  filter(year >= 2000, monthDate != "01-01") %>% 
  group_by(name, month, state) %>% 
  summarise(meanVal=mean(value, na.rm=T)) %>% 
  ggplot(aes(x=month, y=meanVal)) +
  geom_line(aes(colour=state)) + 
  facet_wrap(~name,scales="free") +
  scale_x_continuous(breaks=seq(1,12,1)) +
  ylab('mean') +
  ggtitle("Monthly trend by year")
# ggsave(filename = paste0(ts_2000_directory, "/", "monthly_trend_by_state.png"), monthly_trend_by_state)
rm(monthly_trend_by_state)



quantilesSummary <- plyr::ddply(samples_fin_sub,"name",summarise,
                                min=min(value,na.rm=T),
                                lowerQ=quantile(value,0.25,na.rm=T),
                                median=quantile(value,0.5,na.rm=T),
                                mean=mean(value,na.rm=T),
                                upperQ=quantile(value,0.75,na.rm=T),
                                max=max(value,na.rm=T))
quantilesSummary
rm(quantilesSummary)




# =================== time series (imputed) =================== 
ts_imputed_2000_directory <- "processed/freshwater_time_series_imputed_2000_onwards"

# annual trends -- splines
annual_trend_geom_smooth <- summaryYear_imp_2000 %>% ggplot(aes(x=year,y=meanVal)) +
  geom_smooth() +
  facet_wrap(~name,scales="free") +
  ylab('mean') +
  ggtitle("Annual trend")
# ggsave(filename = paste0(ts_imputed_2000_directory, "/", "annual_trend_geom_smooth.png"), annual_trend_geom_smooth)
rm(annual_trend_geom_smooth)

# annual trends --splines and points
annual_trend_geom_smooth_point <- summaryYear_imp_2000 %>% ggplot(aes(x=year,y=meanVal)) +
  geom_point(colour="purple", size=0.2) +
  geom_smooth() +
  facet_wrap(~name,scales="free") +
  ylab('mean') +
  ggtitle("Annual trend")
# ggsave(filename = paste0(ts_imputed_2000_directory, "/", "annual_trend_geom_smooth_point.png"), annual_trend_geom_smooth_point)
rm(annual_trend_geom_smooth_point)

# annual trends -- lines
# summaryYear_imp_2000 %>% ggplot(aes(x=year,y=meanVal)) +
#   geom_line() +
#   facet_wrap(~name,scales="free") +
#   ylab('annual median') +
#   ggtitle("Annual trend")

annual_trend_geom_line <- summaryYear_imp_2000 %>% 
  group_by(name, year) %>% 
  summarise(meanVal=mean(meanVal)) %>% 
  ggplot(aes(x=year,y=meanVal)) +
  geom_line() +
  facet_wrap(~name,scales="free") +
  ylab('mean') +
  ggtitle("Annual trend")
# ggsave(filename = paste0(ts_imputed_2000_directory, "/", "annual_trend_geom_line.png"), annual_trend_geom_line)
rm(annual_trend_geom_line)



# annual trends by site
# summaryYear_imp_2000 %>% ggplot(aes(x=year,y=meanVal)) +
#   geom_line(aes(group=site_id)) +
#   facet_wrap(~name,scales="free") +
#   ggtitle("Annual trend by site")

# annual trends by state
# summaryYear_imp_2000 %>% ggplot(aes(x=year,y=meanVal)) +
#   geom_line(aes(colour=state)) +
#   facet_wrap(~name,scales="free") +
#   ggtitle("Annual trend by state")

annual_trend_by_state <- summaryYear_imp_2000 %>% 
  group_by(name, year, state) %>% 
  summarise(meanVal=mean(meanVal)) %>% 
  ggplot(aes(x=year,y=meanVal)) +
  geom_line(aes(colour=state)) +
  facet_wrap(~name,scales="free") +
  ylab('mean') +
  ggtitle("Annual trend by state")
# ggsave(filename = paste0(ts_imputed_2000_directory, "/", "annual_trend_by_state.png"), annual_trend_by_state)
rm(annual_trend_by_state)


# trend using individual dates
trend_plot <- dataCast_new %>% 
  filter(year >= 2000, monthDate != "01-01") %>% 
  group_by(name, date, state) %>% 
  summarise(meanVal=mean(value)) %>% 
  ggplot(aes(x=date,y=meanVal)) +
  geom_line() +
  facet_wrap(~name,scales="free") +
  ylab('value (by day)') +
  ggtitle("Trend")
# ggsave(filename = paste0(ts_imputed_2000_directory, "/", "trend.png"), trend_plot)
rm(trend_plot)




# monthly trends -- lines
monthly_trend_geom_line <- dataCast_new %>% 
  filter(year >= 2000, monthDate != "01-01") %>% 
  group_by(name, month) %>% 
  summarise(meanVal=mean(value)) %>% 
  ggplot(aes(x=month, y=meanVal)) +
  geom_line() +
  facet_wrap(~name,scales="free") +
  scale_x_continuous(breaks=seq(1,12,1)) +
  ylab('mean') +
  ggtitle("Monthly trend")
# ggsave(filename = paste0(ts_imputed_2000_directory, "/", "monthly_trend_geom_line.png"), monthly_trend_geom_line)
rm(monthly_trend_geom_line)

# monthly trends -- splines
monthly_trend_geom_smooth <- dataCast_new %>% 
  filter(year >= 2000, monthDate != "01-01") %>% 
  group_by(name, month) %>% 
  summarise(meanVal=mean(value)) %>% 
  ggplot(aes(x=month, y=meanVal)) +
  geom_smooth() +
  facet_wrap(~name,scales="free") +
  scale_x_continuous(breaks=seq(1,12,1)) +
  ylab('mean') +
  ggtitle("Monthly trend")
# ggsave(filename = paste0(ts_imputed_2000_directory, "/", "monthly_trend_geom_smooth.png"), monthly_trend_geom_smooth)
rm(monthly_trend_geom_smooth)

# monthly trends by year
monthly_trend_by_year <- dataCast_new %>% 
  filter(year >= 2000, monthDate != "01-01") %>% 
  group_by(name, year, month) %>% 
  summarise(meanVal=mean(value)) %>% 
  ggplot(aes(x=month, y=meanVal)) +
  geom_line(aes(colour=factor(year))) + 
  facet_wrap(~name,scales="free") +
  scale_x_continuous(breaks=seq(1,12,1)) +
  ylab('mean') +
  ggtitle("Monthly trend by year")
# ggsave(filename = paste0(ts_imputed_2000_directory, "/", "monthly_trend_by_year.png"), monthly_trend_by_year)
rm(monthly_trend_by_year)


# monthly trends by state
monthly_trend_by_state <- dataCast_new %>% 
  filter(year >= 2000, monthDate != "01-01") %>% 
  group_by(name, month, state) %>% 
  summarise(meanVal=mean(value)) %>% 
  ggplot(aes(x=month, y=meanVal)) +
  geom_line(aes(colour=state)) + 
  facet_wrap(~name,scales="free") +
  scale_x_continuous(breaks=seq(1,12,1)) +
  ylab('mean') +
  ggtitle("Monthly trend by year")
# ggsave(filename = paste0(ts_imputed_2000_directory, "/", "monthly_trend_by_state.png"), monthly_trend_by_state)
rm(monthly_trend_by_state)


rm(summaryYear_2000, summaryYear_imp_2000, trends5_2000)
rm(ts_2000_directory)



