
library(tidyverse)


# =================== prepare data for time series (before imputation) =================== 
ts_directory <- "processed/freshwater_time_series"

# change to long format
dataCast_ts1 <- dataCast %>% pivot_longer(cols=4:length(dataCast))

# add year, month, date without year
dataCast_ts1 <- dataCast_ts1 %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         monthDate = strftime(date, "%m-%d")
         )

# add state abbr. and name
dataCast_ts1 <- dataCast_ts1 %>% 
  mutate(state_abbr = str_extract(site_id, "[:alpha:]{2}")) %>% 
  mutate(state = state_abbr) %>% 
  mutate(state = case_when(
    state == "BB" ~ "Brandenburg",
    state == "BW" ~ "Baden-Württemberg",
    state == "BY" ~ "Bayern",
    state == "HE" ~ "Hessen",
    state == "MV" ~ "Mecklenburg-Vorpommern",
    state == "NI" ~ "Niedersachsen",
    state == "NW" ~ "Nordrhein-Westfalen",
    state == "RP" ~ "Rheinland-Pfalz",
    state == "SH" ~ "Schleswig-Holstein",
    state == "SN" ~ "Sachsen",
    state == "ST" ~ "Sachsen-Anhalt",
    state == "TH" ~ "Thüringen"))


# mean means per year (removed entries on 1st Jan)
summaryYear <- plyr::ddply((dataCast_ts1 %>% filter(monthDate != "01-01")),c("name","year","site_id"),
                           summarise, meanVal = median(value,na.rm=T))
# from 1980 onwards
summaryYear <- summaryYear %>% tibble() %>% filter(year >= 1980)

# add state abbr. and name
summaryYear <- summaryYear %>% 
  mutate(state_abbr = str_extract(site_id, "[:alpha:]{2}")) %>% 
  mutate(state = state_abbr) %>% 
  mutate(state = case_when(
    state == "BB" ~ "Brandenburg",
    state == "BW" ~ "Baden-Württemberg",
    state == "BY" ~ "Bayern",
    state == "HE" ~ "Hessen",
    state == "MV" ~ "Mecklenburg-Vorpommern",
    state == "NI" ~ "Niedersachsen",
    state == "NW" ~ "Nordrhein-Westfalen",
    state == "RP" ~ "Rheinland-Pfalz",
    state == "SH" ~ "Schleswig-Holstein",
    state == "SN" ~ "Sachsen",
    state == "ST" ~ "Sachsen-Anhalt",
    state == "TH" ~ "Thüringen"))

# save(summaryYear, file="data/freshwater/cleaned/summaryYear.RData")



# =================== time series (before imputation) =================== 
# annual trends -- splines
annual_trend_geom_smooth <- summaryYear %>% ggplot(aes(x=year,y=meanVal)) +
  geom_smooth() +
  facet_wrap(~name,scales="free") +
  ylab('mean') +
  ggtitle("Annual trend")
# ggsave(filename = paste0(ts_directory, "/", "annual_trend_geom_smooth.png"), annual_trend_geom_smooth)
rm(annual_trend_geom_smooth)

# annual trends -- splines and points
annual_trend_geom_smooth_point <- summaryYear %>% ggplot(aes(x=year,y=meanVal)) +
  geom_point(colour="purple", size=0.2) +
  geom_smooth() +
  facet_wrap(~name,scales="free") +
  ylab('mean') +
  ggtitle("Annual trend")
# ggsave(filename = "paste0(ts_directory, "/", "annual_trend_geom_smooth_point.png"), annual_trend_geom_smooth_point)
rm(annual_trend_geom_smooth_point)

# annual trends -- lines
# summaryYear %>% ggplot(aes(x=year,y=meanVal)) +
#   geom_line() +
#   facet_wrap(~name,scales="free") +
#   ylab('annual median') +
#   ggtitle("Annual trend")

annual_trend_geom_line <- summaryYear %>% 
  group_by(name, year) %>% 
  summarise(meanVal=mean(meanVal, na.rm=T)) %>% 
  ggplot(aes(x=year,y=meanVal)) +
  geom_line() +
  facet_wrap(~name,scales="free") +
  ylab('mean') +
  ggtitle("Annual trend")
# ggsave(filename = paste0(ts_directory, "/", "annual_trend_geom_line.png"), annual_trend_geom_line)
rm(annual_trend_geom_line)


# annual trends by site
# summaryYear %>% ggplot(aes(x=year,y=meanVal)) +
#   geom_line(aes(group=site_id)) +
#   facet_wrap(~name,scales="free") +
#   ggtitle("Annual trend by site")

# annual trends by state
# summaryYear %>% ggplot(aes(x=year,y=meanVal)) +
#   geom_line(aes(colour=state)) +
#   facet_wrap(~name,scales="free") +
#   ggtitle("Annual trend by state")

annual_trend_by_state <- summaryYear %>% 
  group_by(name, year, state) %>% 
  summarise(meanVal=mean(meanVal, na.rm=T)) %>% 
  ggplot(aes(x=year,y=meanVal)) +
  geom_line(aes(colour=state)) +
  facet_wrap(~name,scales="free") +
  ylab('mean') +
  ggtitle("Annual trend by state")
# ggsave(filename = paste0(ts_directory, "/", "annual_trend_by_state.png"), annual_trend_by_state)
rm(annual_trend_by_state)


# trend using individual dates
trend_plot <- dataCast_ts1 %>% 
  filter(year >= 1980, monthDate != "01-01") %>% 
  group_by(name, date, state) %>% 
  summarise(meanVal=mean(value, na.rm=T)) %>% 
  ggplot(aes(x=date,y=meanVal)) +
  geom_line() +
  facet_wrap(~name,scales="free") +
  ylab('value (by day)') +
  ggtitle("Trend")
# ggsave(filename = paste0(ts_directory, "/", "freshwater_time_series/trend.png"), trend_plot)
rm(trend_plot)



# monthly trends -- lines
monthly_trend_geom_line <- dataCast_ts1 %>% 
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
monthly_trend_geom_smooth <- dataCast_ts1 %>% 
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
monthly_trend_by_year <- dataCast_ts1 %>% 
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


# monthly trends by state
monthly_trend_by_state <- dataCast_ts1 %>% 
  filter(year >= 1980, monthDate != "01-01") %>% 
  group_by(name, month, state) %>% 
  summarise(meanVal=mean(value, na.rm=T)) %>% 
  ggplot(aes(x=month, y=meanVal)) +
  geom_line(aes(colour=state)) + 
  facet_wrap(~name,scales="free") +
  scale_x_continuous(breaks=seq(1,12,1)) +
  ylab('mean') +
  ggtitle("Monthly trend by year")
# ggsave(filename = paste0(ts_directory, "/", "monthly_trend_by_state.png"), monthly_trend_by_state)
rm(monthly_trend_by_state)



quantilesSummary <- plyr::ddply(samples_fin_sub_wSites,"name",summarise,
                                min=min(value,na.rm=T),
                                lowerQ=quantile(value,0.25,na.rm=T),
                                median=quantile(value,0.5,na.rm=T),
                                mean=mean(value,na.rm=T),
                                upperQ=quantile(value,0.75,na.rm=T),
                                max=max(value,na.rm=T))
quantilesSummary
rm(quantilesSummary)




# =================== prepare data for time series (imputed) =================== 
dataCast_new <- dataCast_imp$ximp %>% tibble() 
# add sample_id to imputed variables
dataCast_new$sample_id <- dataCast$sample_id
dataCast_new <- dataCast_new %>% select(sample_id, everything())

# add site_id, date
dataCast_new <- dataCast %>% select(sample_id, site_id, date) %>% 
  left_join(dataCast_new, by="sample_id")

# change to long format
dataCast_new <- dataCast_new %>% pivot_longer(cols=4:length(dataCast_new))

# add year, month, date without year
dataCast_new <- dataCast_new %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         monthDate = strftime(date, "%m-%d")
         )


# check entry with 1st Jan
dataCast_new %>% filter(monthDate == "01-01")
dataCast_new %>% filter(monthDate == "01-02")

# add state abbr. and name
dataCast_new <- dataCast_new %>% 
  mutate(state_abbr = str_extract(site_id, "[:alpha:]{2}")) %>% 
  mutate(state = state_abbr) %>% 
  mutate(state = case_when(
    state == "BB" ~ "Brandenburg",
    state == "BW" ~ "Baden-Württemberg",
    state == "BY" ~ "Bayern",
    state == "HE" ~ "Hessen",
    state == "MV" ~ "Mecklenburg-Vorpommern",
    state == "NI" ~ "Niedersachsen",
    state == "NW" ~ "Nordrhein-Westfalen",
    state == "RP" ~ "Rheinland-Pfalz",
    state == "SH" ~ "Schleswig-Holstein",
    state == "SN" ~ "Sachsen",
    state == "ST" ~ "Sachsen-Anhalt",
    state == "TH" ~ "Thüringen"))


# mean means per year (removed entries on 1st Jan)
summaryYear_imp <- plyr::ddply((dataCast_new %>% filter(monthDate != "01-01")), c("name","year","site_id"),
                               summarise, meanVal = median(value,na.rm=T))

# from 1980 onwards
summaryYear_imp <- summaryYear_imp %>% tibble() %>% filter(year >= 1980)


# add state abbr. and name
summaryYear_imp <- summaryYear_imp %>% 
  mutate(state_abbr = str_extract(site_id, "[:alpha:]{2}")) %>% 
  mutate(state = state_abbr) %>% 
  mutate(state = case_when(
    state == "BB" ~ "Brandenburg",
    state == "BW" ~ "Baden-Württemberg",
    state == "BY" ~ "Bayern",
    state == "HE" ~ "Hessen",
    state == "MV" ~ "Mecklenburg-Vorpommern",
    state == "NI" ~ "Niedersachsen",
    state == "NW" ~ "Nordrhein-Westfalen",
    state == "RP" ~ "Rheinland-Pfalz",
    state == "SH" ~ "Schleswig-Holstein",
    state == "SN" ~ "Sachsen",
    state == "ST" ~ "Sachsen-Anhalt",
    state == "TH" ~ "Thüringen"))

# save(summaryYear_imp, file="data/freshwater/cleaned/summaryYear_imp.RData")




# =================== time series (imputed) =================== 
ts_imputed_directory <- "processed/freshwater_time_series_imputed"

# annual trends -- splines
annual_trend_geom_smooth <- summaryYear_imp %>% ggplot(aes(x=year,y=meanVal)) +
  geom_smooth() +
  facet_wrap(~name,scales="free") +
  ylab('mean') +
  ggtitle("Annual trend")
# ggsave(filename = paste0(ts_imputed_directory, "/", "annual_trend_geom_smooth.png"), annual_trend_geom_smooth)
rm(annual_trend_geom_smooth)

# annual trends --splines and points
annual_trend_geom_smooth_point <- summaryYear_imp %>% ggplot(aes(x=year,y=meanVal)) +
  geom_point(colour="purple", size=0.2) +
  geom_smooth() +
  facet_wrap(~name,scales="free") +
  ylab('mean') +
  ggtitle("Annual trend")
# ggsave(filename = paste0(ts_imputed_directory, "/", "annual_trend_geom_smooth_point.png"), annual_trend_geom_smooth_point)
rm(annual_trend_geom_smooth_point)

# annual trends -- lines
# summaryYear_imp %>% ggplot(aes(x=year,y=meanVal)) +
#   geom_line() +
#   facet_wrap(~name,scales="free") +
#   ylab('annual median') +
#   ggtitle("Annual trend")

annual_trend_geom_line <- summaryYear_imp %>% 
  group_by(name, year) %>% 
  summarise(meanVal=mean(meanVal)) %>% 
  ggplot(aes(x=year,y=meanVal)) +
  geom_line() +
  facet_wrap(~name,scales="free") +
  ylab('mean') +
  ggtitle("Annual trend")
# ggsave(filename = paste0(ts_imputed_directory, "/", "annual_trend_geom_line.png"), annual_trend_geom_line)
rm(annual_trend_geom_line)


# annual trends by site
# summaryYear_imp %>% ggplot(aes(x=year,y=meanVal)) +
#   geom_line(aes(group=site_id)) +
#   facet_wrap(~name,scales="free") +
#   ggtitle("Annual trend by site")

# annual trends by state
# summaryYear_imp %>% ggplot(aes(x=year,y=meanVal)) +
#   geom_line(aes(colour=state)) +
#   facet_wrap(~name,scales="free") +
#   ggtitle("Annual trend by state")

annual_trend_by_state <- summaryYear_imp %>% 
  group_by(name, year, state) %>% 
  summarise(meanVal=mean(meanVal)) %>% 
  ggplot(aes(x=year,y=meanVal)) +
  geom_line(aes(colour=state)) +
  facet_wrap(~name,scales="free") +
  ylab('mean') +
  ggtitle("Annual trend by state")
# ggsave(filename = paste0(ts_imputed_directory, "/", "annual_trend_by_state.png"), annual_trend_by_state)
rm(annual_trend_by_state)


# trend using individual dates
trend_plot <- dataCast_new %>% 
  filter(year >= 1980, monthDate != "01-01") %>% 
  group_by(name, date, state) %>% 
  summarise(meanVal=mean(value)) %>% 
  ggplot(aes(x=date,y=meanVal)) +
  geom_line() +
  facet_wrap(~name,scales="free") +
  ylab('value (by day)') +
  ggtitle("Trend")
# ggsave(filename = paste0(ts_imputed_directory, "/", "trend.png"), trend_plot)
rm(trend_plot)




# monthly trends -- lines
monthly_trend_geom_line <- dataCast_new %>% 
  filter(year >= 1980, monthDate != "01-01") %>% 
  group_by(name, month) %>% 
  summarise(meanVal=mean(value)) %>% 
  ggplot(aes(x=month, y=meanVal)) +
  geom_line() +
  facet_wrap(~name,scales="free") +
  scale_x_continuous(breaks=seq(1,12,1)) +
  ylab('mean') +
  ggtitle("Monthly trend")
# ggsave(filename = paste0(ts_imputed_directory, "/", "monthly_trend_geom_line.png"), monthly_trend_geom_line)
rm(monthly_trend_geom_line)

# monthly trends -- splines
monthly_trend_geom_smooth <- dataCast_new %>% 
  filter(year >= 1980, monthDate != "01-01") %>% 
  group_by(name, month) %>% 
  summarise(meanVal=mean(value)) %>% 
  ggplot(aes(x=month, y=meanVal)) +
  geom_smooth() +
  facet_wrap(~name,scales="free") +
  scale_x_continuous(breaks=seq(1,12,1)) +
  ylab('mean') +
  ggtitle("Monthly trend")
# ggsave(filename = paste0(ts_imputed_directory, "/", "monthly_trend_geom_smooth.png"), monthly_trend_geom_smooth)
rm(monthly_trend_geom_smooth)

# monthly trends by year
monthly_trend_by_year <- dataCast_new %>% 
  filter(year >= 1980, monthDate != "01-01") %>% 
  group_by(name, year, month) %>% 
  summarise(meanVal=mean(value)) %>% 
  ggplot(aes(x=month, y=meanVal)) +
  geom_line(aes(colour=factor(year))) + 
  facet_wrap(~name,scales="free") +
  scale_x_continuous(breaks=seq(1,12,1)) +
  ylab('mean') +
  ggtitle("Monthly trend by year")
# ggsave(filename = paste0(ts_imputed_directory, "/", "monthly_trend_by_year.png"), monthly_trend_by_year)
rm(monthly_trend_by_year)


# monthly trends by state
monthly_trend_by_state <- dataCast_new %>% 
  filter(year >= 1980, monthDate != "01-01") %>% 
  group_by(name, month, state) %>% 
  summarise(meanVal=mean(value)) %>% 
  ggplot(aes(x=month, y=meanVal)) +
  geom_line(aes(colour=state)) + 
  facet_wrap(~name,scales="free") +
  scale_x_continuous(breaks=seq(1,12,1)) +
  ylab('mean') +
  ggtitle("Monthly trend by year")
# ggsave(filename = paste0(ts_imputed_directory, "/", "monthly_trend_by_state.png"), monthly_trend_by_state)
rm(monthly_trend_by_state)


# monthly trends by year -- check which year is the one with constant values
dataCast_new %>% 
  filter(year >= 1980, monthDate != "01-01") %>% 
  group_by(name, year, month) %>% 
  summarise(meanVal=mean(value)) %>%  
  filter(name==c('NO3N', 'NO3', 'H2O.temp'), year == 2014)

# check what's going on in 2014
dataCast_new %>% filter(name=='NO3N', year == 2014)

# before imputation
dataCast %>% mutate(year = lubridate::year(date),
                    month = lubridate::month(date)) %>% 
  select(site_id, date, year, 'NO3N') %>% 
  filter(year==2014)

samples_fin_sub_wSites %>% filter(year == 2014)



# monthly trends by state -- check which state look different from the others
# EC_25dC
dataCast_new %>% 
  filter(year >= 1980, monthDate != "01-01", name=="EC_25dC", state=="Niedersachsen") %>% 
  group_by(name, month, state) %>% 
  summarise(meanVal=mean(value))

# NH4
dataCast_new %>% 
  filter(year >= 1980, monthDate != "01-01", name=="NH4", state=="Thüringen") %>% 
  group_by(name, month, state) %>% 
  summarise(meanVal=mean(value))

# NH4N
dataCast_new %>% 
  filter(year >= 1980, monthDate != "01-01", name=="NH4N", state=="Thüringen") %>% 
  group_by(name, month, state) %>% 
  summarise(meanVal=mean(value))


rm(ts_directory, ts_imputed_directory)
