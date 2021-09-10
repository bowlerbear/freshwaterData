
library(tidyverse)
library(ggmap)
source("00-utils.R")


# =================== import data =================== 
# water quality patterns
tdir <- "data/freshwater"

# psm
psm_sites <- read.csv(paste(tdir,"psm_sites.csv",sep="/"),sep=";") %>% tibble()
psm_sites_info <- read.csv(paste(tdir,"psm_sites_info.csv",sep="/"),sep=";") %>% tibble()
psm_maxtu <- read.csv(paste(tdir,"psm_maxtu.csv",sep="/"),sep=";") %>% tibble()

head(psm_sites)
head(psm_sites_info)
head(psm_maxtu)

# samples extracted
load(paste(tdir,"Samples_extracted.RData",sep="/"))
samples_fin_sub <- samples_fin_sub %>% tibble()
str(samples_fin_sub)
head(samples_fin_sub)

# sampled_sites
sites <- read_csv(paste(tdir,"sites.csv", sep="/")) %>% tibble()

head(sites)



# psm ------------------ Pflanzenschutzmittel (phytopharmaceuticals)

## psm_sites
# site_id
# state
# site_nr
# site_name
# stream
# lake
# ezg ------------------ Einzugsgebiet (drainage basin)
# easting -------------- x
# northing ------------- y

## psm_sites_info
# site_id
# ezg_gis -------------- 
# agri_gis -------------
# use ------------------
# ezg_auth -------------
# agri_auth ------------
# ezg_fin --------------
# ezg_fin_source -------
# agri_fin -------------
# gri_fin_source -------

## psm_maxtu
# maxtu ---------------- max. toxic units
# log_maxtu ------------ log10(max. toxic units)
# sample_id
# site_id
# date
# variable_id

## samples_fin_sub
# pgroup ---------------
# fraction -------------
# qualifier ------------
# value ----------------
# value_fin ------------
# unit -----------------
# unit.1 ---------------
# loq ------------------ limit of quantitation
# lod ------------------ limit of detection
# source --------------- ude = ?; ld = ?

## sites
# site_id
# state
# easting
# northing




# =================== check duplicated variables, standardise state names =================== 
# psm_sites_info
# check duplicated variables
# unique(psm_sites_info$ezg_gis == psm_sites_info$ezg_fin)
# unique(psm_sites_info$agri_gis == psm_sites_info$agri_fin)


# sites
# standardise state names in sites
sites$state %>% unique()
sites$state <- ifelse(sites$state=="RP", "Rheinland-Pfalz", sites$state)
sites$state <- ifelse(sites$state=="BW", "Baden-Württemberg", sites$state)


# samples_fin_sub
# check var and var.1
unique(samples_fin_sub$variable_id == samples_fin_sub$variable_id.1) # TRUE
unique(samples_fin_sub$unit == samples_fin_sub$unit.1)
unique(samples_fin_sub$value == samples_fin_sub$value_fin)
# drop duplicated variables
samples_fin_sub <- samples_fin_sub %>% select(-variable_id.1)




# =================== rename variables in samples_fin_sub =================== 
# samples_fin_sub
# look at variable names
unique(samples_fin_sub$name)

# DE -- EN
# "Elektrische Leitfähigkeit (20°C)" --------------- electrical conductivity (20°C)
# "Elektrische Leitfähigkeit (25°C)" --------------- electrical conductivity (25°C)
# "Gesamt-Stickstoff" ------------------------------ total nitrogen
# "Hydrogenkarbonat" ------------------------------- hydrogen carbonate
# "Sauerstoffgehalt" ------------------------------- oxygen content
# "Sauerstoffsättigung" ---------------------------- oxygen saturation
# "Nitrat-Stickstoff" ------------------------------ nitrate nitrogen (NO3-N)
# "Nitrit-Stickstoff" ------------------------------ nitrite nitrogen (NO2-N)
# "Wassertemperatur" ------------------------------- water temperature
# "Abfiltrierbare Stoffe" -------------------------- filterable substances
# "Ammonium-Stickstoff" ---------------------------- ammonium nitrogen (NH4+-N)
# "anorganisch gebundener Stickstoff" -------------- inorganically bound nitrogen
# "Chemischer Sauerstoffbedarf" -------------------- chemical oxygen demand
# "gesamter organisch gebundener Stickstoff" ------- total organically bound nitrogen
# "nicht ionisiertes Ammonium (NH3-N)" ------------- non-ionised ammonium (NH3-N)
# "o-Phosphat-P" ----------------------------------- orthophosphate as phosphorus (PO4-P)  https://www.azocleantech.com/article.aspx?ArticleID=858
# "pH-Wert (Feld)" --------------------------------- pH value (field)
# "Trübung" ---------------------------------------- turbidity
# "Gesamtphosphor als P" --------------------------- total phosphorus as P
# "Gesamt-Phosphor" -------------------------------- total phosphorus
# "Gesamthärte" ------------------------------------ total hardness
# "Lufttemperatur" --------------------------------- air temperature
# "Ammonium" --------------------------------------- ammonium (NH4+)
# "Nitrat" ----------------------------------------- nitrate (NO3-)
# "Nitrit" ----------------------------------------- nitrite (NO2-)
# "Sauerstoffzehrung nach 5 Tagen ohne Hemmer" ----- oxygen consumption after 5 days without inhibitor
# "Sauerstoffzehrung nach 7 Tagen mit Hemmer" ------ oxygen consumption after 7 days with inhibitor
# "gesamter gebundener Stickstoff (TNb)" ----------- total nitrogen bound (TNb)
# "Karbonat" --------------------------------------- carbonate (CO3-2)
# "ortho-Phosphat" --------------------------------- orthophosphate (PO4-3)
# "Trübung (physiko-chem. Messung)" ---------------- turbidity (physico-chemical measurement)

# rename variables
samples_fin_sub <- samples_fin_sub %>% mutate(name = case_when(
  name == "Elektrische Leitfähigkeit (20°C)" ~ "EC_20dC",
  name == "Elektrische Leitfähigkeit (25°C)" ~ "EC_25dC",
  name == "Gesamt-Stickstoff" ~ "tot.N",
  name == "Hydrogenkarbonat" ~ "HCO3",
  name == "Sauerstoffgehalt" ~ "O2.content",
  name == "Sauerstoffsättigung" ~ "O2.saturation",
  name == "Nitrat-Stickstoff" ~ "NO3N",
  name == "Nitrit-Stickstoff" ~ "NO2N",
  name == "Wassertemperatur" ~ "H2O.temp",
  name == "Abfiltrierbare Stoffe" ~ "filtarable.substances",
  name == "Ammonium-Stickstoff" ~ "NH4N",
  name == "anorganisch gebundener Stickstoff" ~ "inorgBound.N",
  name == "Chemischer Sauerstoffbedarf" ~ "chem.O2.demand",
  name == "gesamter organisch gebundener Stickstoff" ~ "tot.orgBound.N",
  name == "nicht ionisiertes Ammonium (NH3-N)" ~ "nonionised.NH3N",
  name == "o-Phosphat-P" ~ "oPO4P",
  name == "pH-Wert (Feld)" ~ "pH.field",
  name == "Trübung" ~ "turbidity",
  name == "Gesamtphosphor als P" ~ "tot.P.as.P",
  name == "Gesamt-Phosphor" ~ "tot.P",
  name == "Gesamthärte" ~ "tot.hardness",
  name == "Lufttemperatur" ~ "air.temp",
  name == "Ammonium" ~ "NH4",
  name == "Nitrat" ~ "NO3",
  name == "Nitrit" ~ "NO2",
  name == "Sauerstoffzehrung nach 5 Tagen ohne Hemmer" ~ "O2.consump.aft5days.wOutINH",
  name == "Sauerstoffzehrung nach 7 Tagen mit Hemmer" ~ "O2.consump.aft7days.wINH",
  name == "gesamter gebundener Stickstoff (TNb)" ~ "tot.N.bound",
  name == "Karbonat" ~ "CO3",
  name == "ortho-Phosphat" ~ "oPO4",
  name == "Trübung (physiko-chem. Messung)" ~ "turbidity_phyChem.meas"
))

# look at new variable names
unique(samples_fin_sub$name)




# =================== add state and date info to samples_fin_sub =================== 
# add state
samples_fin_sub <- samples_fin_sub %>% 
  mutate(state_abbr = str_extract(site_id, "[:alpha:]{2}"))

# add state name
samples_fin_sub <- samples_fin_sub %>% 
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


# check dates
samples_fin_sub %>% arrange(date) %>% head()       # min 1969-01-01 
samples_fin_sub %>% arrange(desc(date)) %>% head() # max 2024-12-31 <- ?

# correct 2024 to 2004
# 2004 but not 2014 since for filtarable.substances:
# in 2014, all the site_ids start with TH
# in 2004, numerous site_ids start with HE
which(samples_fin_sub$date=="2024-12-31") # [1] 1986220 1987763
samples_fin_sub$date[1986220] <- "2004-12-31"
samples_fin_sub$date[1987763] <- "2004-12-31"

samples_fin_sub[1986220,]$sample_id <- "HE_219_2004-12-31"
samples_fin_sub[1987763,]$sample_id <- "HE_10768_2004-12-31"


# add year, month, date without year
samples_fin_sub <- samples_fin_sub %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         monthDate = strftime(date, "%m-%d")
  )

# check entry with 1st Jan
samples_fin_sub %>% filter(monthDate == "01-01")
samples_fin_sub %>% filter(monthDate == "01-02")




# =================== merging samples_fin_sub and sites df =================== 
# convert easting and northing (Gauß-Krüger) to lon and lat
latlon <- convertXY2LonLat(x=sites$easting, y=sites$northing)
latlon %>% head()

sites$lat <- latlon$lat
sites$lon <- latlon$lon

# merging samples_fin_sub and sites df 
samples_fin_sub_wSites <- samples_fin_sub %>% inner_join(sites %>% select(-state), by="site_id")

nrow(samples_fin_sub) - nrow(samples_fin_sub_wSites)
# all 1987960 data points contain site_id with coordinates
rm(samples_fin_sub)


rm(tdir, latlon)

