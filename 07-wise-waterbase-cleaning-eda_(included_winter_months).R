
library(tidyverse)
library(ggmap)
library(tmap)
source("00-utils.R")


# =================== import data =================== 
# Waterbase - Water Quality ICM
# https://www.eea.europa.eu/data-and-maps/data/waterbase-water-quality-icm-1
water_quality_icm <- read_csv('data/wise-eea/Waterbase_v2020_1_T_WISE6_DisaggregatedData.csv') # 13Gb

WISE6_SpatialObject_DerivedData <- read_csv('data/wise-eea/Waterbase_v2020_1_WISE6_Part2_csv/Waterbase_v2020_1_S_WISE6_SpatialObject_DerivedData.csv') 
# only DE
WISE6_SpatialObject_DerivedData <- WISE6_SpatialObject_DerivedData %>% filter(countryCode == "DE")

WISE6_AggregatedData <- read_csv('data/wise-eea/Waterbase_v2020_1_WISE6_Part2_csv/Waterbase_v2020_1_T_WISE6_AggregatedData.csv') 
# only DE
WISE6_AggregatedData_monitoringSiteIdentifier <- WISE6_AggregatedData$monitoringSiteIdentifier %>% unique() %>% 
  str_extract("[:alpha:]+") %>% unique() 
WISE6_AggregatedData_monitoringSiteIdentifier[WISE6_AggregatedData_monitoringSiteIdentifier %>% str_detect("^DE")]
WISE6_AggregatedData <- WISE6_AggregatedData %>% 
  mutate(country_abbr = str_extract(monitoringSiteIdentifier, "[:alpha:]+")) %>% 
  filter(country_abbr %in% c(WISE6_AggregatedData_monitoringSiteIdentifier[WISE6_AggregatedData_monitoringSiteIdentifier %>% str_detect("^DE")]))
rm(WISE6_AggregatedData_monitoringSiteIdentifier)

# only DE
WISE6_AggregatedDataByWaterBody <- read_csv('data/wise-eea/Waterbase_v2020_1_WISE6_Part2_csv/Waterbase_v2020_1_T_WISE6_AggregatedDataByWaterBody.csv') 




# =================== clean data =================== 
water_quality_icm %>% head() %>% View()
water_quality_icm %>% str()


# filter rows which contain "DE" as unique international identifier
monitoringSiteIdentifier <- water_quality_icm$monitoringSiteIdentifier %>% unique() %>% 
  str_extract("[:alpha:]+") %>% unique() 

monitoringSiteIdentifier[monitoringSiteIdentifier %>% str_detect("^DE")]
# "DESM"
# "DE"
# "DEGM"
# "DELAA"
# "DEPLA"
# "DEMUE"
# "DESCH"
# "DEHH" 

water_quality_icm$monitoringSiteIdentifier %>% unique() %>% 
  str_extract("^(DE)[:alpha:]+") %>% unique() 

water_quality_icm <- water_quality_icm %>% 
  mutate(country_abbr = str_extract(monitoringSiteIdentifier, "[:alpha:]+")) %>% 
  filter(country_abbr %in% c("DESM", "DE", "DEGM", "DELAA", "DEPLA", "DEMUE", "DESCH", "DEHH"))


rm(water_quality_icm)
# new df
# write_csv(water_quality_icm, 'data/wise-eea/Waterbase_v2020_1_T_WISE6_DisaggregatedData_DE.csv')
water_quality_icm_DE <- read_csv('data/wise-eea/Waterbase_v2020_1_T_WISE6_DisaggregatedData_DE.csv')


# check df
water_quality_icm_DE %>% head()

# convert date format
water_quality_icm_DE <- water_quality_icm_DE %>% 
  mutate(phenomenonTimeSamplingDate = lubridate::ymd(phenomenonTimeSamplingDate))

# check dates
water_quality_icm_DE %>% arrange(phenomenonTimeSamplingDate) %>% head()       # min 1990-01-03
water_quality_icm_DE %>% arrange(desc(phenomenonTimeSamplingDate)) %>% head() # max 2019-12-16

# add year, month, date without year
water_quality_icm_DE <- water_quality_icm_DE %>% 
  mutate(year = lubridate::year(phenomenonTimeSamplingDate),
         month = lubridate::month(phenomenonTimeSamplingDate),
         monthDate = strftime(phenomenonTimeSamplingDate, "%m-%d")
         )




# =================== site data 1 =================== 
eda_directory_all <- "processed/wisewb/included_winter_months/wisewb_summary_stats/all_variables"

# check no. of missing coordinates
WISE6_SpatialObject_DerivedData %>% nrow() # 2074
WISE6_SpatialObject_DerivedData %>% filter(!is.na(lat)) %>% nrow() # 1645
# 429 missing coordinates

WISE6_SpatialObject_DerivedData$monitoringSiteIdentifier %>% unique()


# visualise available coordinates
germany_bw_map %>% ggmap() +
  geom_point(data=WISE6_SpatialObject_DerivedData, 
             aes(x=lon, y=lat), alpha=0.7)



# merge water_quality_icm_DE and WISE6_SpatialObject_DerivedData
water_quality_icm_DE_wSites <- water_quality_icm_DE %>% left_join((WISE6_SpatialObject_DerivedData %>% 
                                                                      select(-monitoringSiteIdentifierScheme)), 
                                                                   by="monitoringSiteIdentifier")

# no. of missing coordinates
water_quality_icm_DE_wSites %>% nrow() # 242484
water_quality_icm_DE_wSites %>% filter(!is.na(lat)) %>% nrow() # 192885
# 49599 missing coordinates

# visualise available coordinates
germany_bw_map %>% ggmap() +
  geom_point(data=water_quality_icm_DE_wSites, 
             aes(x=lon, y=lat), alpha=0.7)


# check how many variables for each site 
wb_sites_nuVar_wSites <- water_quality_icm_DE %>% 
  group_by(monitoringSiteIdentifier) %>% 
  summarise(nuVar = n_distinct(observedPropertyDeterminandLabel)) %>% 
  left_join((water_quality_icm_DE_wSites %>% 
               select(monitoringSiteIdentifier, lat, lon) %>% distinct()), 
            by="monitoringSiteIdentifier")

# visualise
wb_nuVarPerSite_map <- germany_bw_map %>% ggmap() +
  geom_point(data=wb_sites_nuVar_wSites, 
             aes(x=lon, y=lat, color=nuVar), alpha=0.7)+
  viridis::scale_colour_viridis() +
  ggtitle("Sites provided with coordinates, number of variabls sampled per site")
# ggsave(filename = paste0(eda_directory_all, "/", "nuVar_perSite_map.png"), wb_nuVarPerSite_map)
rm(wb_sites_nuVar_wSites, wb_nuVarPerSite_map)


# check how many years for each site 
wb_sites_nuYear_wSites <- water_quality_icm_DE %>% 
  group_by(monitoringSiteIdentifier) %>% 
  summarise(nuYear = n_distinct(year)) %>% 
  left_join((water_quality_icm_DE_wSites %>% 
               select(monitoringSiteIdentifier, lat, lon) %>% distinct()), 
            by="monitoringSiteIdentifier")

# visualise
wb_nuYearPerSite_map <- germany_bw_map %>% ggmap() +
  geom_point(data=wb_sites_nuYear_wSites, 
             aes(x=lon, y=lat, color=nuYear), alpha=0.7)+
  viridis::scale_colour_viridis() +
  ggtitle("Sites provided with coordinates, number of years sampled per site")
# ggsave(filename = paste0(eda_directory_all, "/", "nuYear_perSite_map.png"), wb_nuYearPerSite_map)
rm(wb_sites_nuYear_wSites, wb_nuYearPerSite_map)




# =================== summary statistics =================== 
# all samples variables
water_quality_icm_DE$observedPropertyDeterminandLabel %>% unique()

# which are the most commonly sampled variables
wb_varSummary <- plyr::ddply(water_quality_icm_DE,"observedPropertyDeterminandLabel",summarise,
                          nuDates = length(unique(phenomenonTimeSamplingDate[!is.na(resultObservedValue)])),
                          nuSites = length(unique(monitoringSiteIdentifier[!is.na(resultObservedValue)])),
                          nuYears = length(unique(year[!is.na(resultObservedValue)]))#,
                          #nuStates = length(unique(country_abbr[!is.na(resultObservedValue)]))
                          )
wb_varSummary %>% tibble()

# check number of variables per year
wb_nuVarPerYear <- water_quality_icm_DE %>% 
  group_by(year) %>% 
  summarise(n_distinct(observedPropertyDeterminandLabel)) %>% 
  ggplot() +
  geom_col(aes(x=year, y=`n_distinct(observedPropertyDeterminandLabel)`)) 
# ggsave(filename = paste0(eda_directory_all, "/", "nuVar_perYear.png"), wb_nuVarPerYear)
rm(wb_nuVarPerYear)


# check no. of years sampled for each variable
wb_varSummary$nuYears %>% sort()

# keep only variables which were at least sampled for 5 years
wb_var_keep <- wb_varSummary$observedPropertyDeterminandLabel[wb_varSummary$nuYears >= 5] # length: 80


# count the no. of data point in each combination of year and month

rm(eda_directory_all)




# =================== select variables =================== 
# herbicides
wb_herbicides <- c("Atrazine", "2,4-dichlorophenoxyacetic acid, 2-4 D", "Ametryn", "Bentazone", "Bromacil",
                   "Bromoxynil", "Chloridazon", "Cyanazine", "Dichlorprop (2,4-DP)", "Ethofumesate", "Hexazinone",
                   "Isoproturon", "Lenacil", "MCPA", "Mecoprop", "Metamitron", "Metazachlor", "Metolachlor", "Metribuzin",
                   "Pendimethalin", "Prometryn", "Propazine", "Simazine", "Terbuthylazine", "Terbutryn", "Trifluralin")

# insecticides
wb_insecticides <- c("Gamma-HCH (Lindane)", "Parathion", "Pirimicarb")

# fungicides
wb_fungicides <- c("Metalaxyl", "Propiconazole")

# mixed
wb_pesticides_general <- c("Diuron")


wb_var_selected <- c("Ammonium", "Nitrate", "Nitrite", "Phosphate", "Electrical conductivity",
                     "pH", "Water temperature", "Dissolved oxygen")

wb_var_selected <- c(wb_var_selected, wb_herbicides, wb_insecticides, wb_fungicides, wb_pesticides_general)


# change variable names in the df
water_quality_icm_DE <- water_quality_icm_DE %>% filter(observedPropertyDeterminandLabel %in% c(wb_var_selected))

water_quality_icm_DE <- water_quality_icm_DE %>% mutate(observedPropertyDeterminandLabel = case_when(
  observedPropertyDeterminandLabel == "Atrazine" ~ "herbicide",
  observedPropertyDeterminandLabel == "2,4-dichlorophenoxyacetic acid, 2-4 D" ~ "herbicide",
  observedPropertyDeterminandLabel == "Ametryn" ~ "herbicide",
  observedPropertyDeterminandLabel == "Bentazone" ~ "herbicide",
  observedPropertyDeterminandLabel == "Bromacil" ~ "herbicide",
  observedPropertyDeterminandLabel == "Bromoxynil" ~ "herbicide",
  observedPropertyDeterminandLabel == "Chloridazon" ~ "herbicide",
  observedPropertyDeterminandLabel == "Cyanazine" ~ "herbicide",
  observedPropertyDeterminandLabel == "Dichlorprop (2,4-DP)" ~ "herbicide",
  observedPropertyDeterminandLabel == "Ethofumesate" ~ "herbicide",
  observedPropertyDeterminandLabel == "Hexazinone" ~ "herbicide",
  observedPropertyDeterminandLabel == "Isoproturon" ~ "herbicide",
  observedPropertyDeterminandLabel == "Lenacil" ~ "herbicide",
  observedPropertyDeterminandLabel == "MCPA" ~ "herbicide",
  observedPropertyDeterminandLabel == "Mecoprop" ~ "herbicide",
  observedPropertyDeterminandLabel == "Metamitron" ~ "herbicide",
  observedPropertyDeterminandLabel == "Metazachlor" ~ "herbicide",
  observedPropertyDeterminandLabel == "Metolachlor" ~ "herbicide",
  observedPropertyDeterminandLabel == "Metribuzin" ~ "herbicide",
  observedPropertyDeterminandLabel == "Pendimethalin" ~ "herbicide",
  observedPropertyDeterminandLabel == "Prometryn" ~ "herbicide",
  observedPropertyDeterminandLabel == "Propazine" ~ "herbicide",
  observedPropertyDeterminandLabel == "Simazine" ~ "herbicide",
  observedPropertyDeterminandLabel == "Terbuthylazine" ~ "herbicide",
  observedPropertyDeterminandLabel == "Terbutryn" ~ "herbicide",
  observedPropertyDeterminandLabel == "Trifluralin" ~ "herbicide",
  observedPropertyDeterminandLabel == "Gamma-HCH (Lindane)" ~ "insecticide",
  observedPropertyDeterminandLabel == "Parathion" ~ "insecticide",
  observedPropertyDeterminandLabel == "Pirimicarb" ~ "insecticide",
  observedPropertyDeterminandLabel == "Metalaxyl" ~ "fungicide",
  observedPropertyDeterminandLabel == "Propiconazole" ~ "fungicide",
  observedPropertyDeterminandLabel == "Diuron" ~ "pesticide.general",
  observedPropertyDeterminandLabel == "Ammonium" ~ "NH4",
  observedPropertyDeterminandLabel == "Nitrate" ~ "NO3",
  observedPropertyDeterminandLabel == "Nitrite" ~ "NO2",
  observedPropertyDeterminandLabel == "Electrical conductivity" ~ "EC",
  observedPropertyDeterminandLabel == "pH" ~ "pH",
  observedPropertyDeterminandLabel == "Water temperature" ~ "H2O.temp",
  observedPropertyDeterminandLabel == "Dissolved oxygen" ~ "diss.O2",
  observedPropertyDeterminandLabel == "Phosphate" ~ "PO4",
))


water_quality_icm_DE$observedPropertyDeterminandLabel %>% unique()

# check unit
water_quality_icm_DE %>% group_by(observedPropertyDeterminandLabel, resultUom) %>% summarise(value = mean(resultObservedValue ))

rm(wb_varSummary, wb_var_keep)
rm(wb_herbicides, wb_insecticides, wb_fungicides, wb_pesticides_general)
rm(wb_var_selected)




# =================== site data 2 =================== 
eda_directory <- "processed/wisewb/included_winter_months/wisewb_summary_stats"

# merge water_quality_icm_DE and WISE6_SpatialObject_DerivedData
water_quality_icm_DE_wSites <- water_quality_icm_DE %>% left_join((WISE6_SpatialObject_DerivedData %>% 
                                                                     select(-monitoringSiteIdentifierScheme)), 
                                                                  by="monitoringSiteIdentifier")

# no. of missing coordinates
water_quality_icm_DE_wSites %>% nrow() # 127149
water_quality_icm_DE_wSites %>% filter(!is.na(lat)) %>% nrow() # 96907
# 30242 missing coordinates (23.8%)

# visualise available coordinates
germany_bw_map %>% ggmap() +
  geom_point(data=water_quality_icm_DE_wSites, 
             aes(x=lon, y=lat), alpha=0.7)


# check how many variables for each site 
wb_sites_nuVar_wSites <- water_quality_icm_DE %>% 
  group_by(monitoringSiteIdentifier) %>% 
  summarise(nuVar = n_distinct(observedPropertyDeterminandLabel)) %>% 
  left_join((water_quality_icm_DE_wSites %>% 
               select(monitoringSiteIdentifier, lat, lon) %>% distinct()), 
            by="monitoringSiteIdentifier")

# visualise
wb_nuVarPerSite_map <- germany_bw_map %>% ggmap() +
  geom_point(data=wb_sites_nuVar_wSites, 
             aes(x=lon, y=lat, color=nuVar), alpha=0.7)+
  viridis::scale_colour_viridis() +
  ggtitle("Sites provided with coordinates, number of variables sampled per site")
# ggsave(filename = paste0(eda_directory, "/", "nuVar_perSite_map.png"), wb_nuVarPerSite_map)
rm(wb_sites_nuVar_wSites, wb_nuVarPerSite_map)


# check how many years for each site 
wb_sites_nuYear_wSites <- water_quality_icm_DE %>% 
  group_by(monitoringSiteIdentifier) %>% 
  summarise(nuYear = n_distinct(year)) %>% 
  left_join((water_quality_icm_DE_wSites %>% 
               select(monitoringSiteIdentifier, lat, lon) %>% distinct()), 
            by="monitoringSiteIdentifier")

# visualise
wb_nuYearPerSite_map <- germany_bw_map %>% ggmap() +
  geom_point(data=wb_sites_nuYear_wSites, 
             aes(x=lon, y=lat, color=nuYear), alpha=0.7)+
  viridis::scale_colour_viridis() +
  ggtitle("Sites provided with coordinates, number of years sampled per site")
# ggsave(filename = paste0(eda_directory, "/", "nuYear_perSite_map.png"), wb_nuYearPerSite_map)
rm(wb_sites_nuYear_wSites, wb_nuYearPerSite_map)




# =================== summary statistics 2 =================== 
# plot histograms
wb_variables_distribution_plot <- ggplot(water_quality_icm_DE)+
  geom_histogram(aes(resultObservedValue)) +
  facet_wrap(~observedPropertyDeterminandLabel,scales="free",ncol=5)
# ggsave(filename = paste0(eda_directory, "/", "variables_distribution.png"), wb_variables_distribution_plot)
rm(wb_variables_distribution_plot)


# which are the most commonly sampled variables
wb_varSummary <- plyr::ddply(water_quality_icm_DE,"observedPropertyDeterminandLabel",summarise,
                             nuDates = length(unique(phenomenonTimeSamplingDate[!is.na(resultObservedValue)])),
                             nuSites = length(unique(monitoringSiteIdentifier[!is.na(resultObservedValue)])),
                             nuYears = length(unique(year[!is.na(resultObservedValue)]))#,
                             #nuStates = length(unique(country_abbr[!is.na(resultObservedValue)]))
                             )
wb_varSummary %>% tibble()

# visualise the most commonly sampled variables
wb_varSummary_hist <- wb_varSummary %>% 
  pivot_longer(cols=c("nuDates", "nuSites", "nuYears"), names_to="type", values_to="number") %>% 
  ggplot() +
  geom_col(aes(x=observedPropertyDeterminandLabel, y=number)) + 
  coord_flip() + 
  facet_wrap(~type,scales="free", ncol=2, nrow=2) +
  xlab("variable") + ylab("number") + 
  ggtitle("Sampled variables: number of dates, years, sites")
# ggsave(filename = paste0(eda_directory, "/", "nuDate_nuYear_nuSite_perVar.png"), wb_varSummary_hist)
rm(wb_varSummary, wb_varSummary_hist)


# check number of variables per year
wb_nuVarPerYear <- water_quality_icm_DE %>% 
  group_by(year) %>% 
  summarise(n_distinct(observedPropertyDeterminandLabel)) %>% 
  ggplot() +
  geom_col(aes(x=year, y=`n_distinct(observedPropertyDeterminandLabel)`)) +
  ylab("number of unique variables sampled")
# ggsave(filename = paste0(eda_directory, "/", "nuVar_perYear.png"), wb_nuVarPerYear)
rm(wb_nuVarPerYear)


# check what variables were sampled each year
wb_VarPerYear <- water_quality_icm_DE %>% ggplot() +
  geom_point(aes(x=year, y=observedPropertyDeterminandLabel, color=observedPropertyDeterminandLabel)) +
  ylab("variable") +
  theme(legend.position="none") +
  ggtitle("Variables sampled in each year")
# ggsave(filename = paste0(eda_directory, "/", "variables_sampled_each_year.png"), wb_VarPerYear)
rm(wb_VarPerYear)




# =================== reshape dataset =================== 
water_quality_icm_DE <- water_quality_icm_DE %>% mutate(sample_id = paste(monitoringSiteIdentifier,phenomenonTimeSamplingDate,sep="_"))

# reshape dataset
wb_dataCast <- reshape2::dcast(
  data = water_quality_icm_DE,
  sample_id+monitoringSiteIdentifier+phenomenonTimeSamplingDate~observedPropertyDeterminandLabel, 
  fun.aggregate=mean,
  value.var="resultObservedValue")
wb_dataCast <- wb_dataCast %>% tibble()


# check no. of missing values
wb_dataCast_missing_df <- wb_dataCast %>% checkNumNa()

wb_nuMissingValuesVariables_selected <- wb_dataCast_missing_df %>% 
  pivot_longer(cols=c("numMissingValues", "perMissingValues"), names_to="type", values_to="value") %>% 
  ggplot() +
  geom_col(aes(x=variable, y=value)) +
  coord_flip() + 
  facet_wrap(~type,scales="free", ncol=2) +
  ggtitle("Number/Percentage of missing values for each variable, across all combinations of sites and dates")
# ggsave(filename = paste0(eda_directory, "/", "nuNA_perVar_selected.png"), wb_nuMissingValuesVariables_selected)
rm(wb_dataCast_missing_df, wb_nuMissingValuesVariables_selected)




# =================== correlations among variables =================== 
# correlations among variables
wb_cors <- cor(x=(wb_dataCast %>% select(4:ncol(wb_dataCast))), use="pairwise.complete.obs")

wb_variables_corr_plot <- ggcorrplot::ggcorrplot(round(wb_cors,1), type = "lower", #p.mat = p.mat,
                                                        outline.col = "white", lab = TRUE,
                                                        ggtheme = ggplot2::theme_gray,
                                                        colors = c("#055a8c", "white", "#DD0000"))
# ggsave(filename = paste0(eda_directory, "/", "variables_important_correlation.png"), wb_variables_corr_plot)
rm(wb_cors, wb_variables_corr_plot)




# =================== PCA =================== 




rm(WISE6_AggregatedData, WISE6_AggregatedDataByWaterBody)





