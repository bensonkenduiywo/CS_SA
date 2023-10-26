#load packages
install.packages("readxl")
library(readxl)
library(sf)
library(terra)
library("viridis") 
library(tmap)
library(mapview)

root <- "D:/OneDrive - CGIAR/SA_Team/"
#FCS
FCS <- read_excel(paste0(root,'Brenda/WFP/data/FCS.xlsx'))
Final_df <- FCS["COUNTY"]
Final_df$Mean_poor <- rowMeans(FCS[,c("2019_Poor","2020_Poor", "2021_Poor")])
Final_df$Mean_borderline <- rowMeans(FCS[,c("2019_Borderline","2020_Borderline", "2021_Borderline")])
Final_df$Mean_acceptable <- rowMeans(FCS[,c("2019_Acceptable","2020_Acceptable", "2021_Acceptable")])
Final_df
head(Final_df)
Counties <- FCS$COUNTY
Counties
#rCSI
rcsi <- read_excel(paste0(root,'Brenda/WFP/data/FCS.xlsx'), sheet="rCSI")
colnames(rcsi)
rcsi_DF <- rcsi["COUNTY"]
rcsi_DF$Mean_None <- rowMeans(rcsi[,c("2019_None","2020_None", "2021_None")])
rcsi_DF$Mean_Stressed <- rowMeans(rcsi[,c("2019_Stressed","2020_Stressed", "2021_Stressed")])
rcsi_DF$Mean_Crisis <- rowMeans(rcsi[,c("2019_Crisis","2020_Crisis", "2020_Crisis")])
rcsi_DF

#livelihood coping
livelihood <- read_excel(paste0(root,'Brenda/WFP/data/FCS.xlsx'), sheet="Livelihood")
names(livelihood)
livelihood_df <- livelihood["COUNTY"]
livelihood_df$Mean_No_Coping <- rowMeans(livelihood[,c("2019_No_coping","2020_No_coping", "2021_No_coping")])
livelihood_df$Mean_Stress_Coping <- rowMeans(livelihood[,c("2019_Stress_coping","2020_Stress_coping", "2021_Stress_coping")])
livelihood_df$Mean_Crisis_Coping <- rowMeans(livelihood[,c("2019_Crisis_coping","2020_Crisis_coping", "2021_Crisis_coping")])
livelihood_df$Mean_Emergencies <- rowMeans(livelihood[,c("2019_Emergencies","2020_Emergencies", "2021_Emergencies")])
livelihood_df

#get counties shapefile
county_shp <- geodata::gadm(country='KEN', level=1, path=root)
county_shp <- sf::st_as_sf(county_shp)
county_shp <- sf::st_cast(county_shp)
county_shp$NAME_1
county_shp <- sf::st_cast(county_shp)
county_shp <- subset(county_shp, county_shp$NAME_1 %in% Counties)
county_shp$NAME_1

#remove unnecessary columns
county_shp <- subset(county_shp, select= -NL_NAME_1)
names(county_shp)[names(county_shp) == "NAME_1"] <- "COUNTY"
county_shp
merged <- merge(county_shp,Final_df, by="COUNTY")
names(merged)
county_rcsi <- merge(county_shp, rcsi_DF, by="COUNTY")
county_rcsi
county_livelihood <- merge(county_shp, livelihood_df, by="COUNTY")
names(county_livelihood)
#get conflict data
conflict <- sf::st_read(paste0(root,"Data/CSO/KEN/clim_conflict_ips_overlays (ACCLED-2017-2022).geojson"))
names(conflict)
plot(conflict)
#Redefine labels
reLabel <- function(conf){
  temp <- conf[conf$conflict_clust_label=="High conflict"  ,]
  temp <- st_intersection(temp, county_shp)
  i <- temp$intersect_conf_clim
  temp$intersect_conf_clim[i=="High conflict-[Low levels of drought stress/High precipitation]"] <-
    "High conflict + Low drought stress" 
  temp$intersect_conf_clim[i=="High conflict-[Moderate-Low levels of drought stress/High-Moderate precipitation]"] <-
    "High conflict + Moderate-Low drought stress"
  temp$intersect_conf_clim[i=="High conflict-[Moderate-High levels of drought stress/Moderate-Low precipitation]"] <-
    "High conflict + Moderate-High drought stress"
  temp$intersect_conf_clim[i=="High conflict-[High levels of drought stress/Low precipitation]"] <-
    "High conflict + High drought stress"
  i <- temp$intersect_conf_clim
  
  temp$clust[i=="High conflict + Low drought stress"] <- 4
  temp$clust[i=="High conflict + Moderate-Low drought stress"] <- 3
  temp$clust[i=="High conflict + Moderate-High drought stress"] <- 2
  temp$clust[i=="High conflict + High drought stress"] <- 1
  temp$clust <- as.factor(temp$clust)
  return(temp)
}
conflict_17_22 <- reLabel(conflict)
names(conflict_17_22)
#use all conflict clusters
all_cluster <- function(conf){
  county_conf <- st_intersection(conf, county_shp)
  i <- county_conf$intersect_conf_clim
  county_conf$intersect_conf_clim[i=="High conflict-[Low levels of drought stress/High precipitation]"] <-
    "High conflict + Low drought stress" 
  county_conf$intersect_conf_clim[i=="High conflict-[Moderate-Low levels of drought stress/High-Moderate precipitation]"] <-
    "High conflict + Moderate-Low drought stress"
  county_conf$intersect_conf_clim[i=="High conflict-[Moderate-High levels of drought stress/Moderate-Low precipitation]"] <-
    "High conflict + Moderate-High drought stress"
  county_conf$intersect_conf_clim[i=="High conflict-[High levels of drought stress/Low precipitation]"] <-
    "High conflict + High drought stress"
  
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Low levels of drought stress/High precipitation]"] <-
    "Moderate conflict + Low drought stress" 
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Moderate-Low levels of drought stress/High-Moderate precipitation]"] <-
    "Moderate conflict + Moderate-Low drought stress"
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Moderate-High levels of drought stress/Moderate-Low precipitation]"] <-
    "Moderate conflict + Moderate-High drought stress"
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High levels of drought stress/Low precipitation]"] <-
    "Moderate conflict + High drought stress"
  
  county_conf$intersect_conf_clim[i=="Limited conflict-[Low levels of drought stress/High precipitation]"] <-
    "Limited conflict + Low drought stress" 
  county_conf$intersect_conf_clim[i=="Limited conflict-[Moderate-Low levels of drought stress/High-Moderate precipitation]"] <-
    "Limited conflict + Moderate-Low drought stress"
  county_conf$intersect_conf_clim[i=="Limited conflict-[Moderate-High levels of drought stress/Moderate-Low precipitation]"] <-
    "Limited conflict + Moderate-High drought stress"
  county_conf$intersect_conf_clim[i=="Limited conflict-[High levels of drought stress/Low precipitation]"] <-
    "Limited conflict + High drought stress"
  i <- county_conf$intersect_conf_clim
  county_conf$bree[i=="Limited conflict + Low drought stress"] <- 12
  county_conf$bree[i=="Limited conflict + Moderate-Low drought stress"] <- 11
  county_conf$bree[i=="Limited conflict + Moderate-High drought stress"] <- 10
  county_conf$bree[i=="Limited conflict + High drought stress"] <- 9
 
  county_conf$bree[i=="Moderate conflict + Low drought stress"] <- 8
  county_conf$bree[i=="Moderate conflict + Moderate-Low drought stress"] <- 7
  county_conf$bree[i=="Moderate conflict + Moderate-High drought stress"] <- 6
  county_conf$bree[i=="Moderate conflict + High drought stress"] <- 5
 
  county_conf$bree[i=="High conflict + Low drought stress"] <- 4
  county_conf$bree[i=="High conflict + Moderate-Low drought stress"] <- 3
  county_conf$bree[i=="High conflict + Moderate-High drought stress"] <- 2
  county_conf$bree[i=="High conflict + High drought stress"] <- 1
  county_conf$bree <- as.factor(county_conf$bree)
  return(county_conf)
}

conflict_all <- all_cluster(conflict)

x <- conflict_all$bree
x
conf_label <- c("High conflict + High drought", "High conflict + Moderate-High drought",
                "High conflict + Moderate-Low drought", "High conflict + Low drought", 
                "Moderate conflict + High drought", "Moderate conflict + Moderate-High drought",
                "Moderate conflict + Moderate-Low drought", "Moderate conflict + Low drought",
                "Limited conflict + High drought", "Limited conflict + Moderate-High drought ",
                "Limited conflict + Moderate-Low drought", "Limited conflict + Low drought")

label <- c("High conflict + High drought stress", "High conflict + Moderate-High drought stress",
          "High conflict + Moderate-Low drought stress", "High conflict + Low drought stress")

#plotting
tmap_mode("plot")
map <- tm_shape(merged)+
      tm_fill(col="Mean_poor", title="FCS_Poor(%)",style = "cont", palette = viridis(100,direction	=-1),legend.show = T)+
      tm_shape(conflict_all) +
      tm_fill(col= "bree", palette="-YlOrRd", title="Conflict-Climate intersection",
          legend.show = T, labels=conf_label)+
    tm_compass(type = "8star", size=3,position = c("right", "bottom")) +
    tm_scale_bar(breaks = c(0, 50, 100), text.size = 1.2, 
               position = c("right", "bottom"))+
    tm_layout(legend.outside=F, 
            legend.text.size = 0.6,
            legend.text.color = "black",
            legend.title.size= 0.8,
            legend.title.color = "black",
            legend.frame=F,
            legend.position = c("left", "bottom"), 
            legend.width = 0.6,
            inner.margins = c(0.1,0.07,0,0)
  )

map






