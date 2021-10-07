
library(tidyverse)
library(ggmap)
library(tmap)
source("00-utils.R")


# =================== import data =================== 
# https://gemstat.org
geomstat_samples <- read_delim('data/GEOMStat/samples_germany.csv', delim=";")

geomstat_meta_parameter <- read_csv('data/GEOMStat/metadata_parameter.csv')

geomstat_meta_station <- read_csv('data/GEOMStat/metadata_station.csv')




# =================== clean data =================== 
# rename columns
for (i in 1:ncol(geomstat_samples)) {
  names(geomstat_samples)[i] <- gsub(" ", "", names(geomstat_samples)[i])
}

for (i in 1:ncol(geomstat_meta_parameter)) {
  names(geomstat_meta_parameter)[i] <- gsub(" ", "", names(geomstat_meta_parameter)[i])
}


for (i in 1:ncol(geomstat_meta_station)) {
  names(geomstat_meta_station)[i] <- gsub(" ", "", names(geomstat_meta_station)[i])
}


# add year, month, date without year
geomstat_samples <- geomstat_samples %>% 
  mutate(year = lubridate::year(SampleDate),
         month = lubridate::month(SampleDate),
         monthDate = strftime(SampleDate, "%m-%d")
  )




# =================== site data =================== 
eda_directory <- "processed/gemstat_summary_stats"

# no. of missing coordinates
geomstat_meta_station %>% nrow() # 1048
geomstat_meta_station %>% filter(!is.na(Latitude)) %>% nrow() # 1048
# 0 missing coordinates

geomstat_samples$GEMSStationNumber %>% unique()


# visualise available coordinates
germany_bw_map %>% ggmap() +
  geom_point(data=geomstat_meta_station, 
             aes(x=Longitude, y=Latitude), alpha=0.7)


# merge geomstat_samples and geomstat_meta_station
geomstat_samples_wSites <- geomstat_samples %>% 
  left_join((geomstat_meta_station %>% 
               select(c("GEMSStationNumber", "WaterType", "MainBasin",
                        "Latitude", "Longitude"))), 
            by="GEMSStationNumber")

# no. of missing coordinates
geomstat_samples_wSites %>% nrow() # 676712
geomstat_samples_wSites %>% filter(!is.na(Latitude)) %>% nrow() # 676712
# 0 missing coordinates


# check how many variables for each site 
gs_sites_nuVar_wSites <- geomstat_samples %>% 
  group_by(GEMSStationNumber) %>% 
  summarise(nuVar = n_distinct(ParameterCode)) %>% 
  left_join((geomstat_samples_wSites %>% 
               select(GEMSStationNumber, Latitude, Longitude) %>% distinct()), 
            by="GEMSStationNumber")

# visualise
gs_nuVarPerSite_map <- germany_bw_map %>% ggmap() +
  geom_point(data=gs_sites_nuVar_wSites, 
             aes(x=Longitude, y=Latitude, color=nuVar), alpha=0.7)+
  viridis::scale_colour_viridis() +
  ggtitle("Sites provided with coordinates, number of variables sampled per site")
# ggsave(filename = paste0(eda_directory, "/", "nuVar_perSite_map.png"), gs_nuVarPerSite_map)
rm(gs_sites_nuVar_wSites, gs_nuVarPerSite_map)


# check how many years for each site 
gs_sites_nuYear_wSites <- geomstat_samples %>% 
  group_by(GEMSStationNumber) %>% 
  summarise(nuYear = n_distinct(year)) %>% 
  left_join((geomstat_samples_wSites %>% 
               select(GEMSStationNumber, Latitude, Longitude) %>% distinct()), 
            by="GEMSStationNumber")

# visualise
gs_nuYearPerSite_map <- germany_bw_map %>% ggmap() +
  geom_point(data=gs_sites_nuYear_wSites, 
             aes(x=Longitude, y=Latitude, color=nuYear), alpha=0.7)+
  viridis::scale_colour_viridis() +
  ggtitle("Sites provided with coordinates, number of years sampled per site")
# ggsave(filename = paste0(eda_directory, "/", "nuYear_perSite_map.png"), gs_nuYearPerSite_map)
rm(gs_sites_nuYear_wSites, gs_nuYearPerSite_map)




# =================== summary statistics =================== 
# all samples variables
geomstat_samples$ParameterCode %>% unique()
geomstat_meta_parameter$ParameterLongName %>% unique()

# which are the most commonly sampled variables
gs_varSummary <- plyr::ddply(geomstat_samples,"ParameterCode",summarise,
                             nuDates = length(unique(SampleDate[!is.na(Value)])),
                             nuSites = length(unique(GEMSStationNumber[!is.na(Value)])),
                             nuYears = length(unique(year[!is.na(Value)]))#,
                             #nuStates = length(unique(country_abbr[!is.na(resultObservedValue)]))
                             )
gs_varSummary %>% tibble()

# check number of variables per year
gs_nuVarPerYear <- geomstat_samples %>% 
  group_by(year) %>% 
  summarise(n_distinct(ParameterCode)) %>% 
  ggplot() +
  geom_col(aes(x=year, y=`n_distinct(ParameterCode)`)) 
# ggsave(filename = paste0(eda_directory, "/", "nuVar_perYear.png"), gs_nuVarPerYear)
rm(gs_nuVarPerYear)


# check no. of years sampled for each variable
gs_varSummary$nuYears %>% sort()

# keep only variables which were at least sampled for 5 years
gs_var_keep <- gs_varSummary$ParameterCode[gs_varSummary$nuYears >= 5] # length: 156

# get full names for the variables
gs_var_keep_df <- gs_var_keep %>% as.data.frame()
colnames(gs_var_keep_df) <- 'ParameterCode'
gs_var_keep_df <- gs_var_keep_df %>% 
  inner_join(geomstat_meta_parameter %>% select(ParameterCode, ParameterLongName),
             by='ParameterCode')




# =================== select variables =================== 
# herbicides
gs_herbicides <- c("2,4-Dichlorophenoxyacetic Acid", "Dichlorprop", "N-ethyl-6-(methylsulfanyl)-N'-(propan-2-yl)-1,3,5-triazine-2,4-diamine", 
                   "Atrazine", "Bentazone", "3,5-dibromo-4-hydroxybenzonitrile", "Cyanazine", "Hexazinone", "3-p-cumenyl-1,1-dimethylurea",
                   "(4-Chloro-2-methylphenoxy)acetic acid", "Mecoprop (mixed isomers)", "Metolachlor", "Metribuzin", "Pendimethalin",
                   "Prometryn", "Terbuthylazine",
                   "(2,4,5-Trichlorophenoxy)acetic acid", "2-(2,4,5-Trichlorophenoxy)propanoic acid", "4-(2,4-dichlorophenoxy)butanoic acid",
                   "3,6-Dichloropyridine-2-carboxylic acid", "Chloroxuron", "3,6-Dichloro-2-methoxybenzoic acid", "Flufenacet", "Linuron", 
                   "4-(4-Chloro-2-methylphenoxy)butanoic acid", "Metobromuron", "Napropamide", "Nicosulfuron", "Picloram")

# insecticides
gs_insecticides <- c("Gamma-Hexachlorocyclohexane (Lindane)", "Parathion ethyl", "Pirimicarb",
                     "4,4'-DDT", "Azinphos-ethyl", "Alpha-Hexachlorocyclohexane", "Chlorpyrifos-ethyl", "Total Dichloro-Diphenyl-Trichloroethanes",
                     "2,2-Dichlorovinyl dimethyl phosphate", "Dimethoate", "Diazinon", "Endosulfan", "Fenitrothion", "Azinphos-methyl",
                     "Imidacloprid", "Malathion", "Parathion methyl", "Thiacloprid")

# fungicides
gs_fungicides <- c("Metalaxyl",
                   "Azoxystrobin", "Hexachlorobenzene", "Tebuconazole")

# mixed
gs_pesticides_general <- c("Diuron",
                           "Pentachlorophenol", "3,5,6-Trichloro-2-pyridinyloxyacetic acid")


gs_var_selected <- c('Nitrite', 'Nitrate', 'Electrical Conductance',
                     'Percent Dissolved Oxygen Saturation', 'pH', 'Water Temperature',
                     'Nitrate and Nitrite','Total Ammonium Nitrogen', 'Free Ammonia Nitrogen', 
                     'Total Nitrogen', 'Dissolved Inorganic Nitrogen', 
                     'Total Phosphorus', 'Total Dissolved Phosphorus', 'Total Inorganic Phosphorus', 'Total Reactive Phosphorus', 'Dissolved Reactive Phosphorus',
                     'Dissolved Oxygen')

gs_var_selected <- c(gs_var_selected, gs_herbicides, gs_insecticides, gs_fungicides, gs_pesticides_general)


gs_var_selected_abbr <- (gs_var_keep_df %>% filter(ParameterLongName %in% gs_var_selected))$ParameterCode


# change variable names in the df
geomstat_samples <- geomstat_samples %>% filter(ParameterCode %in% c(gs_var_selected_abbr))

geomstat_samples <- geomstat_samples %>% mutate(ParameterCode = case_when(
  ParameterCode == "24D" ~ "herbicide",
  ParameterCode == "24DP" ~ "herbicide",
  ParameterCode == "AMETRYN" ~ "herbicide",
  ParameterCode == "ATRAZINE" ~ "herbicide",
  ParameterCode == "BENTAZON" ~ "herbicide",
  ParameterCode == "BROMOXYNIL" ~ "herbicide",
  ParameterCode == "CYANAZINE" ~ "herbicide",
  ParameterCode == "HEXAZINONE" ~ "herbicide",
  ParameterCode == "ISOPROTURON" ~ "herbicide",
  ParameterCode == "MCPA" ~ "herbicide",
  ParameterCode == "MCPPACID" ~ "herbicide",
  ParameterCode == "METOLACHOR" ~ "herbicide",
  ParameterCode == "METRIBUZIN" ~ "herbicide",
  ParameterCode == "PENDIMETHALIN" ~ "herbicide",
  ParameterCode == "PROMETRYN" ~ "herbicide",
  ParameterCode == "TERBUTHYLAZINE" ~ "herbicide",
  ParameterCode == "245T" ~ "herbicide",
  ParameterCode == "245TP" ~ "herbicide",
  ParameterCode == "24DB" ~ "herbicide",
  ParameterCode == "36DCP" ~ "herbicide",
  ParameterCode == "CHLOROXURON" ~ "herbicide",
  ParameterCode == "DICAMBA" ~ "herbicide",
  ParameterCode == "FLUFENACET" ~ "herbicide",
  ParameterCode == "LINURON" ~ "herbicide",
  ParameterCode == "MCPB" ~ "herbicide",
  ParameterCode == "METOBROMURON" ~ "herbicide",
  ParameterCode == "NAPROPAMIDE" ~ "herbicide",
  ParameterCode == "NICOSULFURON" ~ "herbicide",
  ParameterCode == "PICLORAM" ~ "herbicide",
  ParameterCode == "BHC-gamma" ~ "insecticide",
  ParameterCode == "PARATHION" ~ "insecticide",
  ParameterCode == "PIRIMICARB" ~ "insecticide",
  ParameterCode == "44DDT" ~ "insecticide",
  ParameterCode == "AZINPHOS-ETHYL" ~ "insecticide",
  ParameterCode == "BHC-alpha" ~ "insecticide",
  ParameterCode == "CHLORPYRIFOS" ~ "insecticide",
  ParameterCode == "DDT" ~ "insecticide",
  ParameterCode == "DICHLORVOS" ~ "insecticide",
  ParameterCode == "DIMETHOATE" ~ "insecticide",
  ParameterCode == "DIMPYLATE" ~ "insecticide",
  ParameterCode == "ENDOSULFAN" ~ "insecticide",
  ParameterCode == "FENITROTHION" ~ "insecticide",
  ParameterCode == "GUTHION" ~ "insecticide",
  ParameterCode == "IMIDACLOPRID" ~ "insecticide",
  ParameterCode == "MALATHION" ~ "insecticide",
  ParameterCode == "METAPHOS" ~ "insecticide",
  ParameterCode == "THIACLOPRID" ~ "insecticide",
  ParameterCode == "METALAXYL" ~ "fungicide",
  ParameterCode == "AZOXYSTROBIN" ~ "fungicide",
  ParameterCode == "HCB" ~ "fungicide",
  ParameterCode == "TEBUCONAZOLE" ~ "fungicide",
  ParameterCode == "DCMU" ~ "pesticide.general",
  ParameterCode == "PCP" ~ "pesticide.general",
  ParameterCode == "TRICLOPYR" ~ "pesticide.general",
  ParameterCode == "NO2N" ~ "NO2",
  ParameterCode == "NO3N" ~ "NO3",
  ParameterCode == "EC" ~ "EC",
  ParameterCode == "O2-Dis-Sat" ~ "per.diss.O2.saturation",
  ParameterCode == "pH" ~ "pH",
  ParameterCode == "TEMP" ~ "H2O.temp",
  ParameterCode == "NOxN" ~ "NO3.and.NO2",
  ParameterCode == "NH4N" ~ "tot.NH4N",
  ParameterCode == "NH3N" ~ "tot.NH3N",
  ParameterCode == "TN" ~ "tot.N",
  ParameterCode == "DIN" ~ "diss.inorg.N",
  ParameterCode == "TP" ~ "tot.P",
  ParameterCode == "TDP" ~ "tot.diss.P",
  ParameterCode == "TIP" ~ "tot.inorg.N",
  ParameterCode == "TRP" ~ "tot.reac.P",
  ParameterCode == "DRP" ~ "diss.reac.P",
  ParameterCode == "O2-Dis" ~ "diss.O2"
))

geomstat_samples$ParameterCode %>% unique()


# check unit
geomstat_samples$Unit %>% unique()
# "\xb5S/cm" "mg/l"  "\xb0C"    "---"   "\xb5g/l"  "%"   

geomstat_samples %>% group_by(ParameterCode, Unit) %>% summarise(value = mean(Value)) %>% as.data.frame()


geomstat_samples <- geomstat_samples %>% mutate(Unit = case_when(
  ParameterCode == "EC" ~ "uS/cm",
  ParameterCode == "fungicide" ~ "ug/L",
  ParameterCode == "herbicide" ~ "ug/L",
  ParameterCode == "insecticide" ~ "ug/L",
  ParameterCode == "pesticide.general" ~ "ug/L",
  ParameterCode == "H2O.temp" ~ "Cel",
  Unit == "---" ~ "[pH]",
  Unit == "mg/l" ~ "mg/L",
  Unit == "%" ~ "%"
))




# =================== site data 2 =================== 
# merge geomstat_samples and geomstat_meta_station
geomstat_samples_wSites <- geomstat_samples %>% 
  left_join((geomstat_meta_station %>% 
               select(c("GEMSStationNumber", "WaterType", "MainBasin",
                        "Latitude", "Longitude"))), 
            by="GEMSStationNumber" )

# no. of missing coordinates
geomstat_samples_wSites %>% nrow() # 393706
geomstat_samples_wSites %>% filter(!is.na(Latitude)) %>% nrow() # 393706
# 0 missing coordinates


# check how many variables for each site 
gs_sites_nuVar_wSites <- geomstat_samples %>% 
  group_by(GEMSStationNumber) %>% 
  summarise(nuVar = n_distinct(ParameterCode)) %>% 
  left_join((geomstat_samples_wSites %>% 
               select(GEMSStationNumber, Latitude, Longitude) %>% distinct()), 
            by="GEMSStationNumber")

# visualise
gs_nuVarPerSite_map <- germany_bw_map %>% ggmap() +
  geom_point(data=gs_sites_nuVar_wSites, 
             aes(x=Longitude, y=Latitude, color=nuVar), alpha=0.7)+
  viridis::scale_colour_viridis() +
  ggtitle("Sites provided with coordinates, number of variables sampled per site")
# ggsave(filename = paste0(eda_directory, "/", "nuVar_perSite_map.png"), gs_nuVarPerSite_map)
rm(gs_sites_nuVar_wSites, gs_nuVarPerSite_map)


# check how many years for each site 
gs_sites_nuYear_wSites <- geomstat_samples %>% 
  group_by(GEMSStationNumber) %>% 
  summarise(nuYear = n_distinct(year)) %>% 
  left_join((geomstat_samples_wSites %>% 
               select(GEMSStationNumber, Latitude, Longitude) %>% distinct()), 
            by="GEMSStationNumber")

# visualise
gs_nuYearPerSite_map <- germany_bw_map %>% ggmap() +
  geom_point(data=gs_sites_nuYear_wSites, 
             aes(x=Longitude, y=Latitude, color=nuYear), alpha=0.7)+
  viridis::scale_colour_viridis() +
  ggtitle("Sites provided with coordinates, number of years sampled per site")
# ggsave(filename = paste0(eda_directory, "/", "nuYear_perSite_map.png"), gs_nuYearPerSite_map)
rm(gs_sites_nuYear_wSites, gs_nuYearPerSite_map)




# =================== summary statistics 2 =================== 
# plot histograms
gs_variables_distribution_plot <- ggplot(geomstat_samples)+
  geom_histogram(aes(Value)) +
  facet_wrap(~ParameterCode,scales="free",ncol=5)
# ggsave(filename = paste0(eda_directory, "/", "variables_distribution.png"), gs_variables_distribution_plot)
rm(gs_variables_distribution_plot)


# which are the most commonly sampled variables
gs_varSummary <- plyr::ddply(geomstat_samples,"ParameterCode",summarise,
                             nuDates = length(unique(SampleDate[!is.na(Value)])),
                             nuSites = length(unique(GEMSStationNumber[!is.na(Value)])),
                             nuYears = length(unique(year[!is.na(Value)]))#,
                             #nuStates = length(unique(country_abbr[!is.na(resultObservedValue)]))
                             )
gs_varSummary %>% tibble()

# visualise the most commonly sampled variables
gs_varSummary_hist <- gs_varSummary %>% 
  pivot_longer(cols=c("nuDates", "nuSites", "nuYears"), names_to="type", values_to="number") %>% 
  ggplot() +
  geom_col(aes(x=ParameterCode, y=number)) + 
  coord_flip() + 
  facet_wrap(~type,scales="free", ncol=2, nrow=2) +
  xlab("variable") + ylab("number") + 
  ggtitle("Sampled variables: number of dates, years, sites")
# ggsave(filename = paste0(eda_directory, "/", "nuDate_nuYear_nuSite_perVar.png"), wb_varSummary_hist)
rm(gs_varSummary_hist)


# check number of variables per year
gs_nuVarPerYear <- geomstat_samples %>% 
  group_by(year) %>% 
  summarise(n_distinct(ParameterCode)) %>% 
  ggplot() +
  geom_col(aes(x=year, y=`n_distinct(ParameterCode)`)) +
  ylab("number of unique variables sampled")
# ggsave(filename = paste0(eda_directory, "/", "nuVar_perYear.png"), gs_nuVarPerYear)


# check what variables were sampled each year
gs_VarPerYear <- geomstat_samples %>% ggplot() +
  geom_point(aes(x=year, y=ParameterCode, color=ParameterCode)) +
  ylab("variable") +
  theme(legend.position="none") +
  ggtitle("Variables sampled in each year")
# ggsave(filename = paste0(eda_directory, "/", "variables_sampled_each_year.png"), gs_VarPerYear)
rm(gs_nuVarPerYear, gs_VarPerYear)




# =================== reshape dataset =================== 
geomstat_samples <- geomstat_samples %>% mutate(sample_id = paste(GEMSStationNumber,SampleDate,sep="_"))

# reshape dataset
gs_dataCast <- reshape2::dcast(
  data = geomstat_samples,
  sample_id+GEMSStationNumber+SampleDate~ParameterCode, 
  fun.aggregate=mean,
  value.var="Value")
gs_dataCast <- gs_dataCast %>% tibble()


# check no. of missing values
gs_dataCast_missing_df <- gs_dataCast %>% checkNumNa()

gs_nuMissingValuesVariables_selected <- gs_dataCast_missing_df %>% 
  pivot_longer(cols=c("numMissingValues", "perMissingValues"), names_to="type", values_to="value") %>% 
  ggplot() +
  geom_col(aes(x=variable, y=value)) +
  coord_flip() + 
  facet_wrap(~type,scales="free", ncol=2) +
  ggtitle("Number/Percentage of missing values for each variable, across all combinations of sites and dates")
# ggsave(filename = paste0(eda_directory, "/", "nuNA_perVar_selected.png"), gs_nuMissingValuesVariables_selected)
rm(gs_nuMissingValuesVariables_selected)




# =================== correlations among variables =================== 
# correlations among variables
gs_cors <- cor(x=(gs_dataCast %>% select(4:ncol(gs_dataCast))), use="pairwise.complete.obs")

gs_variables_corr_plot <- ggcorrplot::ggcorrplot(round(gs_cors,1), type = "lower", #p.mat = p.mat,
                                                 outline.col = "white", lab = TRUE,
                                                 ggtheme = ggplot2::theme_gray,
                                                 colors = c("#055a8c", "white", "#DD0000"))
# ggsave(filename = paste0(eda_directory, "/", "variables_important_correlation.png"), gs_variables_corr_plot)




# =================== PCA =================== 






