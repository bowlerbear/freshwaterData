
library(tidyverse)
library(ggmap)
library(tmap)




# =================== summary statistics =================== 
eda_directory <- "processed/freshwater_summary_stats"

# plot histograms
variables_distribution_plot <- ggplot(data=samples_fin_sub_wSites) +
  geom_histogram(aes(value))+
  facet_wrap(~name,scales="free",ncol=5)
# ggsave(filename = paste0(eda_directory, "/", "variables_distribution.png"), variables_distribution_plot)
rm(variables_distribution_plot)


# which are the most commonly sampled variables
varSummary <- plyr::ddply(samples_fin_sub_wSites,"name",summarise,
                          nuDates = length(unique(date[!is.na(value)])),
                          nuSites = length(unique(site_id[!is.na(value)])),
                          nuYears = length(unique(year[!is.na(value)])),
                          nuStates = length(unique(state_abbr[!is.na(value)])))
varSummary %>% tibble()

# visualise the most commonly sampled variables
varSummary_hist <- varSummary %>% 
  pivot_longer(cols=c("nuDates", "nuSites", "nuYears", "nuStates"), names_to="type", values_to="number") %>% 
  ggplot() +
  geom_col(aes(x=name, y=number)) + 
  coord_flip() + 
  facet_wrap(~type,scales="free", ncol=2, nrow=2) +
  xlab("variable") + ylab("number") + 
  ggtitle("Sampled variables: number of dates, years, sites, states")
# ggsave(filename = paste0(eda_directory, "/", "nuDate_nuYear_nuSite_nuState_perVar.png"), varSummary_hist)
rm(varSummary_hist, varSummary)


# check number of variables per year
nuVarPerYear <- samples_fin_sub_wSites %>% 
  group_by(year) %>% 
  summarise(n_distinct(name)) %>% 
  ggplot() +
  geom_col(aes(x=year, y=`n_distinct(name)`)) +
  ylab("number of unique variables sampled")
# ggsave(filename = paste0(eda_directory, "/", "nuVar_perYear.png"), nuVarPerYear)
rm(nuVarPerYear)


# check how many years for each site 
sites_nuYears <- samples_fin_sub_wSites %>% 
  group_by(site_id) %>% 
  summarise(nuYears = n_distinct(year)) 

sites_nuYear_wSites <- sites_nuYears %>% 
  left_join((samples_fin_sub_wSites %>% select(site_id, lat, lon) %>% distinct()), by="site_id")

# visualise no. of years for each site 
nuYearPerSite_map <- germany_bw_map %>% ggmap() +
  geom_point(data=sites_nuYear_wSites, 
             aes(x=lon, y=lat, color=nuYears), alpha=0.7)+
  viridis::scale_colour_viridis() +
  ggtitle("Sites provided with coordinates, number of years sampled per site")
# ggsave(filename = paste0(eda_directory, "/", "nuYear_perSite_map.png"), nuYearPerSite_map)
rm(nuYearPerSite_map)
rm(sites_nuYears, sites_nuYear_wSites)


# check number of unique variables per state
nuVarPerState_map <- germany_shp %>% 
  rename(state=NAME_1) %>% 
  left_join(
    (samples_fin_sub_wSites %>% 
       group_by(state) %>% 
       summarise(nuVar = n_distinct(name))),
    by="state") %>% 
  tm_shape() + tm_polygons(col="nuVar", palette = "viridis") +
  tm_text(text="nuVar") +
  tm_layout(legend.outside = TRUE, main.title = "Number of unique variables per state", main.title.size = 1)
# tmap_save(tm = nuVarPerState_map,
#           filename = paste0(eda_directory, "/", "nuVarPerState_map.png"))


# check number of unique sites per state
nuSitePerState_map <- germany_shp %>% 
  rename(state=NAME_1) %>% 
  left_join(
    (samples_fin_sub_wSites %>% 
       group_by(state) %>% 
       summarise(nuSite = n_distinct(site_id))),
    by="state") %>% 
  tm_shape() + tm_polygons(col="nuSite", palette = "viridis") +
  tm_text(text="nuSite") +
  tm_layout(legend.outside = TRUE, main.title = "Number of unique sites per state", main.title.size = 1)
# tmap_save(tm = nuSitePerState_map,
#           filename = paste0(eda_directory, "/", "nuSitePerState_map.png"))


# check how many years for each state
nuYearPerState_map <- germany_shp %>% 
  rename(state=NAME_1) %>% 
  left_join(
    (samples_fin_sub_wSites %>% 
       group_by(state) %>% 
       summarise(nuYear = n_distinct(year))),
    by="state") %>% 
  tm_shape() + tm_polygons(col="nuYear", palette = "viridis") +
  tm_text(text="nuYear") +
  tm_layout(legend.outside = TRUE, main.title = "Number of unique years per state", main.title.size = 1)
# tmap_save(tm = nuYearPerState_map,
#           filename = paste0(eda_directory, "/", "nuYearPerState_map.png"))


# tmap_save(tm = (tmap_arrange(nuVarPerState_map, nuSitePerState_map, nuYearPerState_map,
#                              ncol=2, nrow=2)),
#           filename = paste0(eda_directory, "/", "nuVar_nuSite_nuYear_PerState_map.png"))
rm(nuVarPerState_map, nuSitePerState_map, nuYearPerState_map)



# count the no. of data point in each combination of year and month
# combine year and month
samples_fin_sub_wSites <- samples_fin_sub_wSites %>% 
  mutate(yearMonth = paste(year, month, sep=".")) 

# count the occurrence of each unique combination
yearMonth_count_df <- samples_fin_sub_wSites %>% 
  group_by(yearMonth) %>% summarise(count=n())

# merge the occurrence df to the main df
samples_fin_sub_wSites <- samples_fin_sub_wSites %>% 
  left_join(yearMonth_count_df, by="yearMonth") %>% 
  rename(count_yearMonth = count)
rm(yearMonth_count_df)

# visualise frequency for each combination of year and month
nuCombination_yearMonth <- samples_fin_sub_wSites %>% ggplot(aes(x=year, y=month)) +
  geom_tile(aes(fill=count_yearMonth)) +
  #geom_text(aes(label=count_yearMonth)) +
  viridis::scale_fill_viridis() +
  scale_y_continuous(breaks=seq(1,12,1)) +
  ggtitle("Frequency for each combination of year and month")
# ggsave(filename = paste0(eda_directory, "/", "nuCombination_yearMonth.png"), nuCombination_yearMonth)
rm(nuCombination_yearMonth)


# change the bins
samples_fin_sub_wSites <- samples_fin_sub_wSites %>%
  # create a new variable from count
  mutate(count_yearMonth_factor=cut(count_yearMonth,breaks=c(0,10,50,100,500,1000,2000,5000,10000,max(count_yearMonth,na.rm=T)),
                                    labels=c("0-10","10-50","50-100","100-500","500-1000", "2000","2000-5000","5000-10000",">10000"))) %>%
  # change level order
  mutate(count_yearMonth_factor=factor(as.character(count_yearMonth_factor),levels=rev(levels(count_yearMonth_factor))))

# plot again with the new bins
nuCombination_yearMonth2 <- samples_fin_sub_wSites %>% ggplot(aes(x=year, y=month)) +
  geom_tile(aes(fill=count_yearMonth_factor),colour="white",size=0.25) +
  scale_fill_manual(values=rev(RColorBrewer::brewer.pal(9,"YlGnBu")),na.value="grey90") +
  scale_y_continuous(breaks=seq(1,12,1)) +
  ggtitle("Frequency for each combination of year and month")
# ggsave(filename = paste0(eda_directory, "/", "nuCombination_yearMonth2.png"), nuCombination_yearMonth2)
rm(nuCombination_yearMonth2)


# check what variables were sampled each year
VarPerYear <- samples_fin_sub_wSites %>% ggplot() +
  geom_point(aes(x=year, y=name, color=name)) +
  ylab("variable") +
  theme(legend.position="none") +
  ggtitle("Variables sampled in each year")
# ggsave(filename = paste0(eda_directory, "/", "variables_sampled_each_year.png"), VarPerYear)

# saveRDS(samples_fin_sub_wSites, "data/freshwater/cleaned/samples_fin_sub_wSites.rds")



# =================== reshape dataset =================== 
dataCast <- reshape2::dcast(samples_fin_sub_wSites, sample_id+site_id+date~name, value.var="value")
dataCast <- dataCast %>% tibble()

# check no. of missing values
dataCast_missing_df <- dataCast %>% checkNumNa()

nuMissingValuesVariables <- dataCast_missing_df %>% 
  pivot_longer(cols=c("numMissingValues", "perMissingValues"), names_to="type", values_to="value") %>% 
  ggplot() +
  geom_col(aes(x=variable, y=value)) +
  coord_flip() + 
  facet_wrap(~type,scales="free", ncol=2) +
  ggtitle("Number/Percentage of missing values for each variable, across all combinations of sites and dates")
# ggsave(filename = paste0(eda_directory, "/", "nuNA_perVar.png"), nuMissingValuesVariables)
rm(nuMissingValuesVariables)
rm(dataCast_missing_df)



# =================== correlations among variables =================== 
# correlations among variables
cors <- cor(x=(dataCast %>% select(-c("sample_id", "site_id", "date"))), use="pairwise.complete.obs")

# corrplot::corrplot(cors, method="color", type="upper", tl.col="black", tl.cex=0.65,
#                    col=RColorBrewer::brewer.pal(n=8, name="RdYlBu"))

# Compute a matrix of correlation p-values
# p.mat <- ggcorrplot::cor_pmat(x=(dataCast %>% select(-c("sample_id", "site_id", "date"))))
# head(p.mat[, 1:4])

variables_corr_plot <- ggcorrplot::ggcorrplot(round(cors,1), type = "lower", #p.mat = p.mat,
                                              outline.col = "white", lab = TRUE,
                                              ggtheme = ggplot2::theme_gray,
                                              colors = c("#055a8c", "white", "#DD0000"))
# ggsave(filename = paste0(eda_directory, "/", "variables_correlation.png"), variables_corr_plot)
rm(variables_corr_plot)

# variables_ggpairs <- GGally::ggpairs(data=(dataCast %>% select(-c("sample_id", "site_id", "date"))))
# ggsave(filename = paste0(eda_directory, "/", "variables_ggpairs.png"), variables_ggpairs)


# for correlations > 0.7 or < -0.7
# reshape to long format for subset
corsM <- cors %>% reshape2::melt()
corsM[corsM==1] <- NA
corsM <- subset(corsM, abs(value)>0.7)

# reshape back to wide matrix
corsM <- corsM %>% reshape2::dcast(Var1 ~ Var2, value.var = "value")
corsM.m <- corsM %>% select(-Var1) %>% as.matrix()
rownames(corsM.m) <- corsM$Var1 %>% as.character()
corsM.m

variables_corr_plot_0.7 <- ggcorrplot::ggcorrplot(round(corsM.m,2), type = "lower", #p.mat = p.mat,
                                                  outline.col = "white", lab = TRUE,
                                                  ggtheme = ggplot2::theme_gray,
                                                  colors = viridis::viridis(n=3))
# ggsave(filename = paste0(eda_directory, "/", "variables_correlation_0.7.png"), variables_corr_plot_0.7)
rm(variables_corr_plot_0.7)




# =================== imputate NA values in the selected variables =================== 
# impute missing values for certain variables (under NA threshold, correlated)
# get variables with < 50% missing values
var_underNAthreshold <- (dataCast_missing_df %>% 
                           filter(perMissingValues < 0.5))$variable

# get correlated variables (abs(correlation) > 0.7)
var_aboveCorThreshold <- corsM.m %>% colnames()

# get variables which satisfy both conditions
var_forImp <- var_underNAthreshold[var_underNAthreshold %in% var_aboveCorThreshold]

dataCast_miss <- dataCast %>% 
  select(c(var_forImp)) %>% 
  data.frame() 

#save(dataCast_miss, file="dataCast_miss.RData")

dataCast_imp <- missForest::missForest(xmis = dataCast_miss, verbose = TRUE)
load('data/freshwater/cleaned/dataCast_imp.RData')
dataCast_imp$ximp %>% tibble()


rm(cors, corsM, corsM.m, var_underNAthreshold, var_aboveCorThreshold, var_forImp, dataCast_miss)




# =================== PCA =================== 
df_variables <- dataCast %>% select(-c("sample_id", "site_id", "date"))
#pcaVariables <- prcomp(df_variables, na.action=na.exclude, scale = TRUE)  # doesn't work due to the amount of missing values


# impute missing values
# estimate number of components
nb <- missMDA::estim_ncpPCA(df_variables, ncp.min=0, ncp.max=5)
# $ncp
# [1] 2
# 
# $criterion
# 0        1        2        3        4        5 
# 269658.1 230735.4 161520.6 179114.0 201846.7 208477.3 

# actual impute
rr.impute <- missMDA::imputePCA(df_variables, ncp=2)
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




# =================== relationship between certain variables =================== 
important_var <- c("NH4", "NO3", "NO2", "oPO4", "EC_20dC", "EC_25dC", 
                   "pH.field", "O2.saturation", "H2O.temp"
                   , "NH4N", "NO2N", "NO3N", "nonionised.NH3N",
                   "tot.N", "tot.N.bound", "tot.orgBound.N", "inorgBound.N", 
                   "oPO4P", "tot.P", "tot.P.as.P",
                   "O2.content"
                   )

# dataCast %>% select(important_var) %>% GGally::ggpairs()


# correlations among important variables
cors_important <- cor(x=(dataCast %>% select(important_var)), use="pairwise.complete.obs")

variables_important_corr_plot <- ggcorrplot::ggcorrplot(round(cors_important,1), type = "lower", #p.mat = p.mat,
                                              outline.col = "white", lab = TRUE,
                                              ggtheme = ggplot2::theme_gray,
                                              colors = c("#055a8c", "white", "#DD0000"))
# ggsave(filename = paste0(eda_directory, "/", "variables_important_correlation.png"), variables_important_corr_plot)
rm(variables_important_corr_plot)


rm(important_var, cors_important)
rm(eda_directory)


