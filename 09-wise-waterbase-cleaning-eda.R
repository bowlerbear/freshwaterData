
library(tidyverse)
# library(ggmap)
library(tmap)
# source("00-utils.R")


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

rm(WISE6_AggregatedData, WISE6_AggregatedDataByWaterBody)




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


# remove winter months from the dataset
water_quality_icm_DE <- water_quality_icm_DE %>% 
  filter(!(month %in% c(12, 1, 2))) # rmeove 17368 rows


# transform to UTM
# remove NAs in coordinates
WISE6_SpatialObject_DerivedData_UTM <- WISE6_SpatialObject_DerivedData %>% filter(!is.na(lon))

# transformation
XY_df <- convertLonLat2UTM(WISE6_SpatialObject_DerivedData_UTM$lon, WISE6_SpatialObject_DerivedData_UTM$lat)

WISE6_SpatialObject_DerivedData_UTM$easting <- XY_df$X
WISE6_SpatialObject_DerivedData_UTM$northing <- XY_df$Y
rm(XY_df)

# germany's shape file
sys <- "+proj=tmerc +lat_0=0 +lon_0=9 +k_0=1 +x_0=3500000 +y_0=0 +ellps=bessel +units=m"
germany_sp <- as(germany_shp, "Spatial")
germany_sp <- sp::spTransform(germany_sp, raster::crs(sys))
germany_sf <- germany_sp %>% sf::st_as_sf()




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
water_quality_icm_DE %>% group_by(observedPropertyDeterminandLabel, resultUom) %>% summarise(value = mean(resultObservedValue))

rm(wb_varSummary, wb_var_keep)
rm(wb_herbicides, wb_insecticides, wb_fungicides, wb_pesticides_general)
rm(wb_var_selected)




# =================== site data ===================
eda_directory <- "processed/wisewb/excluded_winter_months/wisewb_summary_stats"

# merge water_quality_icm_DE and WISE6_SpatialObject_DerivedData
water_quality_icm_DE_wSites <- water_quality_icm_DE %>% left_join((WISE6_SpatialObject_DerivedData_UTM %>% 
                                                                     select(-monitoringSiteIdentifierScheme)), 
                                                                  by="monitoringSiteIdentifier")


# no. of missing coordinates
water_quality_icm_DE_wSites %>% nrow() # 127149
water_quality_icm_DE_wSites %>% filter(!is.na(easting)) %>% nrow() # 96907
# 30242 missing coordinates


water_quality_icm_DE_wSites_sf <- water_quality_icm_DE_wSites %>% 
  filter(!is.na(easting)) %>% sf::st_as_sf(coords=c("easting", "northing"))
sf::st_crs(water_quality_icm_DE_wSites_sf) <- sys


# visualise available coordinates
tm_shape(germany_sf) +
  tm_polygons(lwd = 0.5) +
  tm_shape(water_quality_icm_DE_wSites_sf) +
  tm_dots() +
  tm_layout(legend.outside = TRUE)
rm(water_quality_icm_DE_wSites_sf)



# check how many variables for each site 
wb_sites_nuVar_wSites <- water_quality_icm_DE %>% 
  group_by(monitoringSiteIdentifier) %>% 
  summarise(nuVar = n_distinct(observedPropertyDeterminandLabel)) %>% 
  left_join((water_quality_icm_DE_wSites %>% 
               select(monitoringSiteIdentifier, easting, northing) %>% distinct()), 
            by="monitoringSiteIdentifier")

wb_sites_nuVar_wSites_sf <- wb_sites_nuVar_wSites %>% 
  filter(!is.na(easting)) %>% sf::st_as_sf(coords=c("easting", "northing"))
sf::st_crs(wb_sites_nuVar_wSites_sf) <- sys

# visualise
wb_nuVarPerSite_map <- tm_shape(germany_sf) +
  tm_polygons(lwd = 0.5) +
  tm_shape(wb_sites_nuVar_wSites_sf) +
  tm_dots(col="nuVar", size=0.15, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "Sites provided with coordinates, number of variabls sampled per site")

tmap_save(tm = wb_nuVarPerSite_map, filename = paste0(eda_directory, "/", "nuVar_perSite_map.png"))

rm(wb_sites_nuVar_wSites_sf, wb_nuVarPerSite_map)



# check how many years for each site 
wb_sites_nuYear_wSites <- water_quality_icm_DE %>% 
  group_by(monitoringSiteIdentifier) %>% 
  summarise(nuYear = n_distinct(year)) %>% 
  left_join((water_quality_icm_DE_wSites %>% 
               select(monitoringSiteIdentifier, easting, northing) %>% distinct()), 
            by="monitoringSiteIdentifier")

wb_sites_nuYear_wSites_sf <- wb_sites_nuYear_wSites %>% 
  filter(!is.na(easting)) %>% sf::st_as_sf(coords=c("easting", "northing"))
sf::st_crs(wb_sites_nuYear_wSites_sf) <- sys

# visualise
wb_nuYearPerSite_map <- tm_shape(germany_sf) +
  tm_polygons(lwd = 0.5) +
  tm_shape(wb_sites_nuYear_wSites_sf) +
  tm_dots(col="nuYear", size=0.15, alpha=0.7, palette = "viridis", midpoint = NA) +
  tm_layout(legend.outside = TRUE, main.title = "Sites provided with coordinates, number of variabls sampled per site")

tmap_save(tm = wb_nuYearPerSite_map, filename = paste0(eda_directory, "/", "nuYear_perSite_map.png"))

rm(wb_sites_nuYear_wSites_sf, wb_nuYearPerSite_map)




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
df_variables <- wb_dataCast %>% select(-c("sample_id", "monitoringSiteIdentifier", "phenomenonTimeSamplingDate"))

# impute missing values
# estimate number of components
nb <- missMDA::estim_ncpPCA(df_variables, ncp.min=0, ncp.max=5)
# $ncp
# [1] 0
# 
# $criterion
# 0            1            2            3            4            5 
# 1.720500e+05 2.445475e+05 8.326169e+05 2.554061e+09 2.381136e+05 1.938203e+04 

# actual impute
rr.impute <- missMDA::imputePCA(df_variables, ncp=0)
rr.impute$completeObs %>% data.frame() %>% tibble()


# run PCA
pca.fit <- prcomp(rr.impute$completeObs,scale=TRUE)
pca.fit %>% summary()


# visualise contribution of variables
pcaVariables_loadings <- factoextra::fviz_pca_var(pca.fit, col.var="contrib",
                                                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                                  repel = TRUE)
# ggsave(filename = paste0(eda_directory, "/", "pcaVariables_loadings.png"), pcaVariables_loadings)
rm(pcaVariables_loadings)


# visualize eigenvalues/variances
pcaVariables_screePlot <- factoextra::fviz_eig(pca.fit, addlabels = TRUE)
# ggsave(filename = paste0(eda_directory, "/", "pcaVariables_screePlot.png"), pcaVariables_screePlot)
rm(pcaVariables_screePlot)

# variable contributions to the principal axes:
# contributions of variables to PC1
pcaVariables_PC1 <- factoextra::fviz_contrib(pca.fit, choice = "var", axes = 1, top = 10)
# ggsave(filename = paste0(eda_directory, "/", "pcaVariables_PC1.png"), pcaVariables_PC1)
rm(pcaVariables_PC1)

# contributions of variables to PC2
pcaVariables_PC2 <- factoextra::fviz_contrib(pca.fit, choice = "var", axes = 2, top = 10)
# ggsave(filename = paste0(eda_directory, "/", "pcaVariables_PC2.png"), pcaVariables_PC2)
rm(pcaVariables_PC2)

rm(df_variables, nb, rr.impute, pca.fit)


rm(eda_directory)




