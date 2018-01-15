library(rgdal)
library(sp)
library(rgeos)
library(raster)

data<-read.csv("invasives.csv")
data<- data[complete.cases(data[,16:17]),]
ecoregions<-shapefile("tnc_terr_ecoregions.shp")
p <- SpatialPointsDataFrame(data[,16:17], data, proj4string = crs(ecoregions))
ecojoin<-over(p,ecoregions)
ecojoin=cbind(ecojoin, data)
#####'ecojoin' contains the coordinates from our original data as spatial points. 
#####'These spatial points are used to join our original data entires to the appropriate 
#####'ecoregion, realm, MHT, etc.

#Parasite richness
# 25: ParasiteCorrectedName, 9:WWF_REALM2, 11: WWF_MHTNAM, 3:ECO_NAME
par_u<- unique(ecojoin[,c(25,9,11,3)])

#Host richness
# 20: HostCorrectedName, 9:WWF_REALM2, 11: WWF_MHTNAM, 3:ECO_NAME
host_u<- unique(ecojoin[,c(20,9,11,3)])

#parasites within specific WWF_REALM2 & MHTNAM combination organized by ECO_NAME
#PARASITES by ecoregion
summary1<-with(par_u, table(ECO_NAME))

#hosts within specific WWF_REALM2 & MHTNAM combination organized by ECO_NAME
#HOSTS by ecoregion
summary2<-with(host_u, table(ECO_NAME))

#ECO holds the name of ecoregion and the host and parasite richness in these areas
ECO<- merge(summary1, summary2, by="ECO_NAME")

colnames(ECO)<- c("ECO_NAME", "parasite_richness", "host_richness")

#3: Ecoregion-"ECO_NAME", 9: WWF_REALM2, 11: WWF_MHTNAM
region<- ecojoin[,c(3,9,11)]


#####'eco_final' has host richness, parasite richness, ecoregion, WWF_REALM2, and WWF_MHTNAM
eco_final<- merge(ECO, region, by="ECO_NAME")

##We included the number of hosts sampled
agdata <- aggregate(HostsSampled~HostCorrectedName+Citation+Longitude+Latitude, data=data, FUN=max)
agdatap <- SpatialPointsDataFrame(agdata[,3:4], agdata, proj4string = crs(ecoregions))

agover<- over(agdatap, ecoregions)

agover=cbind(agdatap@data,agover)
agoverag<- aggregate(HostsSampled~ECO_NAME, data=agover, FUN=sum)
eco_final<-merge(eco_final,agoverag)

eco_final=unique(eco_final)
#####This new version of 'eco_final' contains the number of hosts sampled as well as 
#####host richness, parasite richness, ecoregion, WWF_REALM2, and WWF_MHTNAM


##This summary is looking at the linear model of native AND invasive individuals:
test<-lm(log10(parasite_richness)~log10(host_richness)+log10(HostsSampled)+WWF_MHTNAM+WWF_REALM2,data=eco_final)
summary(test)


data<- read.csv("only_invasives.csv")
data<- data[complete.cases(data[,16:17]),]
ecoregions<-shapefile("tnc_terr_ecoregions.shp")
p <- SpatialPointsDataFrame(data[,16:17], data, proj4string = crs(ecoregions))
ecojoin<-over(p,ecoregions)
ecojoin<-cbind(ecojoin, data)
par_u<- unique(ecojoin[,c(25,9,11,3)])
host_u<- unique(ecojoin[,c(20,9,11,3)])
summary1<-with(par_u, table(ECO_NAME))
summary2<-with(host_u, table(ECO_NAME))
ECO<- merge(summary1, summary2, by="ECO_NAME")
colnames(ECO)<- c("ECO_NAME", "host_richness", "parasite_richness")
region<- ecojoin[,c(3,9,11)]
eco_final<- merge(ECO, region)

agdata <- aggregate(HostsSampled~HostCorrectedName+Citation+Longitude+Latitude, data=data, FUN=max)
agdatap <- SpatialPointsDataFrame(agdata[,3:4], agdata, proj4string = crs(ecoregions))
agover<- over(agdatap, ecoregions)
agover=cbind(agdatap@data,agover)
agoverag<- aggregate(HostsSampled~ECO_NAME, data=agover, FUN=sum)
eco_final<-merge(eco_final,agoverag)
eco_final=unique(eco_final)

##This summary is looking at the linear model of ONLY invasive individuals:
test<-lm(log10(parasite_richness)~log10(host_richness)+log10(HostsSampled)+WWF_MHTNAM+WWF_REALM2,data=eco_final)
summary(test)



data<-read.csv("invasives.csv")
data<- data[complete.cases(data[,16:17]),]
ecoregions<-shapefile("tnc_terr_ecoregions.shp")
p <- SpatialPointsDataFrame(data[,16:17], data, proj4string = crs(ecoregions))
ecojoin<-over(p,ecoregions)
#####'eco_final' has host richness, parasite richness, ecoregion, WWF_REALM2, and WWF_MHTNAM 
#####'(does not include the number of hosts sampled)
ecojoin=cbind(ecojoin, data)

#####20:HostCorrectedName, 25:ParasiteCorrectedName, 18:Group, 27:ParType, 9:WWF_REALM2, 
#####11:WWF_MHTNAM 43:Native Range #3: ECO_NAME
unique<- unique(ecojoin[,c(20, 25, 18, 27, 9, 11, 43)])
table1<- with(unique, table(Group, ParType,WWF_REALM2, WWF_MHTNAM, NativeRange))
table1<-as.data.frame(table1)
table2<-subset(table1,Freq>0)
table1_lm<- lm(log10(Freq)~NativeRange+Group+ParType+WWF_REALM2+WWF_MHTNAM, data=table2)
summary(table1_lm)
par(mar=c(1,12,1,1))
boxplot(log10(Freq)~NativeRange+Group+ParType,data=table2,horizontal=T,las=1,notch=F)

#####18:Group, 27:ParType, 9:WWF_REALM2, 11:WWF_MHTNAM 43:Native Range, 30:Citation #3:ECO_NAME
unique2<- unique(ecojoin[,c(18, 27, 9, 11, 43, 30, 3)])
table3<- with(unique2, table(Group, ParType, WWF_REALM2, WWF_MHTNAM, NativeRange, ECO_NAME))
table3<- as.data.frame(table3)
names(table3)[7]<- "Cit.ct"
table4<-subset(table3,Cit.ct>0)
final_table<- merge(table2, table4, by=c("Group","ParType","WWF_REALM2","WWF_MHTNAM","NativeRange","ECO_NAME"))
final_table_lm<- lm(log10(Freq)~NativeRange+Group+ParType+Cit.ct+WWF_REALM2+WWF_MHTNAM, data=final_table)
summary(final_table_lm)


###############################################################################################################
cent <- gCentroid(ecoregions, byid=TRUE)
cent <- as.data.frame(cent)
ecodat<-ecoregions@data
centroids<- cbind(cent, ecodat[,3])
names(centroids)[3]<- "ECO_NAME"


final_table2<- merge(final_table, centroids, by="ECO_NAME")

#(06/23/2017) Geoadditive model:

library(geoGAM)
library(coin)
#,"ParType","WWF_REALM2","WWF_MHTNAM","NativeRange", 
geogam2 <- geoGAM(response = "Freq",
                 covariates = c("Cit.ct","ParType","Group","NativeRange"),
                 #coords = c("x", "y"),
                 offset=T,
                 data = final_table2,
                 max.stop = 20)
summary(geogam2)
plot(geogam)

