########################################################################################

#examine water quality patterns
tdir <- "data"
sites <- read.csv(paste(tdir,"psm_sites.csv",sep="/"),sep=";")
sites_info <- read.csv(paste(tdir,"psm_sites_info.csv",sep="/"),sep=";")
maxtu <- read.csv(paste(tdir,"psm_maxtu.csv",sep="/"),sep=";")

head(sites)
head(sites_info)
head(maxtu)

#samples extracted
load(paste(tdir,"Samples_extracted.RData",sep="/"))
str(samples_fin_sub)
head(samples_fin_sub)

#add year
library(lubridate)
samples_fin_sub$date <- as.Date(samples_fin_sub$date)
samples_fin_sub$Year <- year(samples_fin_sub$date)

#add site data


#look at variable names
unique(samples_fin_sub$name)

#plot histograms
library(ggplot2)
ggplot(data=subset(samples_fin_sub, name %in% unique(samples_fin_sub$name)[c(15:31)]))+
  geom_histogram(aes(value))+
  facet_wrap(~name,scales="free",ncol=5)
  
#which are the most commonly sampled variables
library(plyr)
varSummary <- ddply(samples_fin_sub,.(name),summarise,
                    nuDates = length(unique(date[!is.na(value)])),
                    nuSites = length(unique(site_id[!is.na(value)])),
                    nuYears = length(unique(Year[!is.na(value)])))
varSummary
  
  
#example correlations among variables
library(reshape2)
dataCast <- dcast(samples_fin_sub,sample_id+site_id+date~name,value.var="value")
cors <- cor(dataCast[,4:ncol(dataCast)],use="pairwise.complete.obs")
corsM <- melt(cors)
corsM[corsM==1] <- NA
library(corrplot)
corrplot(cors,method = "color",tl.col = "black",tl.cex=0.65)

subset(corsM,abs(value)>0.7)

ggplot(corsM)+
  geom_tile(aes(x=Var1,y=Var2,fill=value))+
  theme

#PCA
df <- dataCast[,4:ncol(dataCast)]
df = kNN(df)

names(df) <- gsub(" ","",names(df))
colnames(df) <- paste("var", 1:31, sep="")
princomp(~., data = df, 
         cor = TRUE, na.action=na.exclude)

summary(fit) 
loadings(fit) 
plot(fit,type="lines") 
fit$scores 
biplot(fit)

# impute missing values
library(missMDA)
# estimate number of components
nb <- estim_ncpPCA(df, ncp.min=0, ncp.max=5)
# actual impute
rr.impute <- imputePCA(df, ncp=2)

# Run pca
pca.fit <- prcomp(rr.impute$completeObs,scale=TRUE)

library(factoextra)
fviz_pca_var(pca.fit,repel = TRUE)

#time-series

#mean means per year
vars <- c("Wassertemperatur","Ammonium-Stickstoff","Nitrat","Sauerstoffgehalt")
summaryYear <- ddply(samples_fin_sub,.(name,Year,site_id),summarise,meanVal = median(value,na.rm=T))

ggplot(subset(summaryYear,name %in% vars),aes(x=Year,y=meanVal))+
        geom_line(aes(group=site_id))+
        facet_wrap(~name,scales="free")

ggplot(subset(summaryYear,name %in% vars),aes(x=Year,y=meanVal))+
  stat_smooth()+
  facet_wrap(~name,scales="free")


quantilesSummary <- ddply(samples_fin_sub,.(name),summarise,
                          min=min(value,na.rm=T),
                          lowerQ=quantile(value,0.25,na.rm=T),
                          median=quantile(value,0.5,na.rm=T),
                          mean=mean(value,na.rm=T),
                          upperQ=quantile(value,0.75,na.rm=T),
                          max=max(value,na.rm=T))


#get trend at each site_id
trends <- ddply(summaryYear,.(name,site_id),function(x){
  
  if(length(unique(x$Year))>10){
    lm1 <- lm(meanVal ~ Year, data=x)
    temp <- data.frame(t(summary(lm1)$coef[2,]))
    temp$minYear <- min(x$Year)
    temp$maxYear <- max(x$Year)
    return(temp)
  }
  
})

#merge with site data
trends <- merge(trends,sites,by="site_id")

ggplot(subset(trends,name==vars[4]),
       aes(x=easting, y=northing))+
  geom_point(aes(colour=Estimate))+
  scale_colour_gradient2(low="red",mid="white",high="blue",midpoint=0)+
  ggtitle(vars[4])

########################################################################################