library(haven)
library(geodata)
library(sf)
library(tmap)
library("viridis")
library(readxl)
library(tidyr)

path <- 'D:/OneDrive - CGIAR/SA_Team/Data/Mali/'
df1 <- read_sav(paste0(path,'ALL_2018_2023_Data_Fusion_PDM_Resilience.sav'))
df1
temp <- df1[df1$ADMIN0Name=='MALI', ]
temp
temp <- aggregate(FCS~YEAR+ADMIN2Name, data=temp,mean, na.rm=T)
write.csv(temp,paste0(path,'FCS_DataFusion.csv'))

#get mali shapefile
Mali_adm2 <- sf::st_as_sf(geodata::gadm(country = 'MALI',level = 2, path = tempdir() ))
Mali_adm2 <- Mali_adm2[c('NAME_1','NAME_2')]
Mali_adm2
View(Mali_adm2)
plot(Mali_adm2)

#rename admin 2 to remove apostrophe
Mali_adm2$NAME_2[Mali_adm2$NAME_2 == "Bafoulabé"] <- "Bafoulabe"
Mali_adm2$NAME_2[Mali_adm2$NAME_2 == "Kolondiéba"] <- "Kolondieba"
Mali_adm2$NAME_2[Mali_adm2$NAME_2 == "Ségou"] <- "Segou"
Mali_adm2$NAME_2[Mali_adm2$NAME_2 == "Diré"] <- "Dire"

#calculate mean for 2018-2023
df <-  read.csv(paste0(path,'FCS_DataFusion.csv'),header=T, sep=',')
View(df)
fcs_mean <- aggregate(FCS~ADMIN2Name, data=df, mean, na.rm=T)
names(fcs_mean)[names(fcs_mean) == "ADMIN2Name"] <- "NAME_2"
View(fcs_mean)
#merge df and shapefile
merged <- merge(Mali_adm2,fcs_mean, by="NAME_2")
merged

#read conflict data
conflict <- sf::st_read("D:/OneDrive - CGIAR/SA_Team/Data/CSO/MLI/MLI_megapixels.geojson")
names(conflict)
plot(conflict)
unique(conflict$intersect_conf_clim)
reLabel <- function(conf){
  temp <- conf[conf$conflict_clust_label=="High conflict"  ,]
  temp
  temp <- st_intersection(temp, merged)
  unique(temp$intersect_conf_clim)
  i <- temp$intersect_conf_clim
  temp$intersect_conf_clim[i=="High conflict-[High levels of precipitation/Low levels of drought stress]"] <-
    "High conflict + Low drought stress"
  temp$intersect_conf_clim[i=="High conflict-[Moderate-Low levels of precipitation/Moderate-High levels of drought stress]"] <-
    "High conflict + Moderate-High drought stress"
  i <- temp$intersect_conf_clim
  
  temp$clust[i=="High conflict + Low drought stress"] <- 2
  temp$clust[i=="High conflict + Moderate-High drought stress"] <- 1
  temp$clust <- as.factor(temp$clust)
  return(temp)
}
conflict_mali <- reLabel(conflict)
plot(conflict_mali)
unique(conflict_mali$intersect_conf_clim)
unique(conflict$conflict_clust_label)
all_cluster <- function(conf){
  county_conf <- st_intersection(conf, merged)
  i <- county_conf$intersect_conf_clim
  county_conf$intersect_conf_clim[i=="High conflict-[High levels of precipitation/Low levels of drought stress]"] <-
    "High conflict + Low drought stress" 
  county_conf$intersect_conf_clim[i=="High conflict-[Moderate-Low levels of precipitation/Moderate-High levels of drought stress]"] <-
    "High conflict + Moderate-High drought stress"
  
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High levels of precipitation/Low levels of drought stress]"] <-
    "Moderate conflict + Low drought stress" 
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High-Moderate levels of precipitation/Moderate-Low levels of drought stress]"] <-
    "Moderate conflict + Moderate-Low drought stress"
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Moderate-Low levels of precipitation/Moderate-High levels of drought stress]"] <-
    "Moderate conflict + Moderate-High drought stress"
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Low levels of precipitation/High levels of drought stress]"] <-
    "Moderate conflict + High drought stress"
  
  county_conf$intersect_conf_clim[i=="Limited conflict-[High levels of precipitation/Low levels of drought stress]"] <-
    "Limited conflict + Low drought stress" 
  county_conf$intersect_conf_clim[i=="Limited conflict-[High-Moderate levels of precipitation/Moderate-Low levels of drought stress]"] <-
    "Limited conflict + Moderate-Low drought stress"
  county_conf$intersect_conf_clim[i=="Limited conflict-[Moderate-Low levels of precipitation/Moderate-High levels of drought stress]"] <-
    "Limited conflict + Moderate-High drought stress"
  county_conf$intersect_conf_clim[i=="Limited conflict-[Low levels of precipitation/High levels of drought stress]"] <-
    "Limited conflict + High drought stress"
  i <- county_conf$intersect_conf_clim
  county_conf$clust[i=="Limited conflict + Low drought stress"] <- 10
  county_conf$clust[i=="Limited conflict + Moderate-Low drought stress"] <- 9
  county_conf$clust[i=="Limited conflict + Moderate-High drought stress"] <- 18
  county_conf$clust[i=="Limited conflict + High drought stress"] <- 7
  
  county_conf$clust[i=="Moderate conflict + Low drought stress"] <- 6
  county_conf$clust[i=="Moderate conflict + Moderate-Low drought stress"] <- 5
  county_conf$clust[i=="Moderate conflict + Moderate-High drought stress"] <- 4
  county_conf$clust[i=="Moderate conflict + High drought stress"] <- 3
  
  county_conf$clust[i=="High conflict + Low drought stress"] <- 2
  county_conf$clust[i=="High conflict + Moderate-High drought stress"] <- 1
  county_conf$clust <- as.factor(county_conf$clust)
  return(county_conf)
}

conflict_all <- all_cluster(conflict)
View(conflict_all)
county_conf <- conflict[conflict$conflict_clust_label==c("High conflict","Moderate conflict")  ,]
county_conf <- st_intersection(county_conf, merged)
View(county_conf)
unique(county_conf$conflict_clust_label)
conf_no_limited <- function(conf){
  county_conf <- conf[conf$conflict_clust_label != "Limited conflict"  ,]
  county_conf <- st_intersection(county_conf, merged)
  i <- county_conf$intersect_conf_clim
  county_conf$intersect_conf_clim[i=="High conflict-[High levels of precipitation/Low levels of drought stress]"] <-
    "High conflict + Low drought stress" 
  county_conf$intersect_conf_clim[i=="High conflict-[Moderate-Low levels of precipitation/Moderate-High levels of drought stress]"] <-
    "High conflict + Moderate-High drought stress"
  
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High levels of precipitation/Low levels of drought stress]"] <-
    "Moderate conflict + Low drought stress" 
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High-Moderate levels of precipitation/Moderate-Low levels of drought stress]"] <-
    "Moderate conflict + Moderate-Low drought stress"
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Moderate-Low levels of precipitation/Moderate-High levels of drought stress]"] <-
    "Moderate conflict + Moderate-High drought stress"
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Low levels of precipitation/High levels of drought stress]"] <-
    "Moderate conflict + High drought stress"
  
  i <- county_conf$intersect_conf_clim
  
  county_conf$clust[i=="Moderate conflict + Low drought stress"] <- 5
  county_conf$clust[i=="Moderate conflict + Moderate-Low drought stress"] <- 4
  county_conf$clust[i=="Moderate conflict + Moderate-High drought stress"] <- 3
  county_conf$clust[i=="Moderate conflict + High drought stress"] <- 2
  county_conf$clust[i=="High conflict + Moderate-High drought stress"] <- 1
  county_conf$clust <- as.factor(county_conf$clust)
  return(county_conf)
}

conflict__no_limited <- conf_no_limited(conflict)
unique(conflict__no_limited$intersect_conf_clim)
no_limited_label <- c("High conflict + Moderate-High drought", 
           "Moderate conflict + High drought", "Moderate conflict + Moderate-High drought",
           "Moderate conflict + Moderate-Low drought", "Moderate conflict + Low drought"
           )
unique(conflict_all$clust)
plot(conflict_all)
label <- c("High conflict + Moderate-High drought","High conflict + Low drought", 
            "Moderate conflict + High drought", "Moderate conflict + Moderate-High drought",
            "Moderate conflict + Moderate-Low drought", "Moderate conflict + Low drought",
            "Limited conflict + High drought", "Limited conflict + Moderate-High drought ",
            "Limited conflict + Moderate-Low drought", "Limited conflict + Low drought")
high_conf_label <- c("High conflict + Moderate-High drought stress","High conflict + Low drought stress")
#plotting FCS
tmap_mode("plot")
map <- tm_shape(merged)+
  tm_fill(col="FCS", title="FCS",style = "cont", palette = viridis(100,direction	=-1),legend.show = T)+
  tm_shape(conflict__no_limited) +
  tm_fill(col= "clust", palette="-YlOrRd", title="Conflict-Climate Intersection",
          legend.show = T, labels=no_limited_label)+
  #tm_shape(merged)+
 #tm_borders(col="grey",lwd=0.01)+
  #tm_text("NAME_2", size = 0.8, remove.overlap = TRUE, col ='black')+
  tm_compass(type = "8star", size=4,position = c("right", "bottom")) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 1.5, 
               position = c("right", "bottom"))+
  tm_layout(legend.outside=F, 
            legend.text.size = 1,
            legend.text.color = "black",
            legend.title.size= 1,
            legend.title.color = "black",
            legend.title.fontface = 2,
            legend.frame=F,
            asp=1.4,
            legend.position = c("right", "top"), 
            legend.width = 0.6,
            inner.margins = c(0,0,0,0)
  )

map
tmap_save(map,  dpi= 300,  height=8.3, width=11.7, units="in",
          filename="D:/OneDrive - CGIAR/SA_Team/Brenda/Mali/final_maps/FCS1.png")

#ICSMAG data
ICSMAG <- read_excel(paste0(path,"mli_ica+Nut_CollectingTable_20230720.xlsx"))
ICSMAG <- ICSMAG[c("adm2_name","adm1_name","ICAMAG","ICAMCG")]
ICSMAG <- ICSMAG %>% drop_na()
names(ICSMAG)[names(ICSMAG) == "adm2_name"] <- "NAME_2"
View(ICSMAG)

#merge icsmag and mali shapefile
merged_icsmag <- merge(Mali_adm2, ICSMAG, by="NAME_2")
unique(merged_icsmag$ICAMAG)
plot(merged_icsmag)

#get icsmag conflict data
high_conflict <- function(conf){
  temp <- conf[conf$conflict_clust_label=="High conflict"  ,]
  temp <- st_intersection(temp, merged_icsmag)
  i <- temp$intersect_conf_clim
  temp$intersect_conf_clim[i=="High conflict-[High levels of precipitation/Low levels of drought stress]"] <-
    "High conflict + Low drought stress"
  temp$intersect_conf_clim[i=="High conflict-[Moderate-Low levels of precipitation/Moderate-High levels of drought stress]"] <-
    "High conflict + Moderate-High drought stress"
  i <- temp$intersect_conf_clim
  
  temp$clust[i=="High conflict + Low drought stress"] <- 2
  temp$clust[i=="High conflict + Moderate-High drought stress"] <- 1
  temp$clust <- as.factor(temp$clust)
  return(temp)
}
high_conf <- high_conflict(conflict)
plot(high_conf)
all_conf <- function(conf){
  county_conf <- conf[conf$conflict_clust_label != "Limited conflict"  ,]
  county_conf <- st_intersection(county_conf, merged_icsmag)
  i <- county_conf$intersect_conf_clim
  county_conf$intersect_conf_clim[i=="High conflict-[High levels of precipitation/Low levels of drought stress]"] <-
    "High conflict + Low drought stress" 
  county_conf$intersect_conf_clim[i=="High conflict-[Moderate-Low levels of precipitation/Moderate-High levels of drought stress]"] <-
    "High conflict + Moderate-High drought stress"
  
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High levels of precipitation/Low levels of drought stress]"] <-
    "Moderate conflict + Low drought stress" 
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High-Moderate levels of precipitation/Moderate-Low levels of drought stress]"] <-
    "Moderate conflict + Moderate-Low drought stress"
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Moderate-Low levels of precipitation/Moderate-High levels of drought stress]"] <-
    "Moderate conflict + Moderate-High drought stress"
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Low levels of precipitation/High levels of drought stress]"] <-
    "Moderate conflict + High drought stress"
  
  i <- county_conf$intersect_conf_clim
  
  county_conf$clust[i=="Moderate conflict + Low drought stress"] <- 6
  county_conf$clust[i=="Moderate conflict + Moderate-Low drought stress"] <- 5
  county_conf$clust[i=="Moderate conflict + Moderate-High drought stress"] <- 4
  county_conf$clust[i=="Moderate conflict + High drought stress"] <- 3
  
  county_conf$clust[i=="High conflict + Low drought stress"] <- 2
  county_conf$clust[i=="High conflict + Moderate-High drought stress"] <- 1
  county_conf$clust <- as.factor(county_conf$clust)
  return(county_conf)
}

conflict_icsmag <- all_conf(conflict)
plot(conflict_icsmag)
unique(conflict_icsmag$clust)

#maps for icsmag
icsmag_map <- tm_shape(merged_icsmag)+
  tm_fill(col="ICAMAG", title="ICAMAG",style = "cat", palette = viridis(8,direction	=-1),legend.show = T)+
  tm_shape(conflict_icsmag) +
  tm_fill(col= "clust", palette="-YlOrRd", title="Conflict-Climate Intersection",
          legend.show = T, labels=label)+
  #tm_shape(merged)+
  #tm_borders(col="grey",lwd=0.01)+
  #tm_text("NAME_2", size = 0.8, remove.overlap = TRUE, col ='black')+
  tm_compass(type = "8star", size=4,position = c("right", "bottom")) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 1.5, 
               position = c("right", "bottom"))+
  tm_layout(legend.outside=F, 
            legend.text.size = 1,
            legend.text.color = "black",
            legend.title.size= 1.1,
            legend.title.color = "black",
            legend.title.fontface = 2,
            legend.frame=F,
            asp = 1.4,
            legend.position = c("left", "top"), 
            legend.width = 0.6,
            inner.margins = c(0,0,0,0)
  )

icsmag_map
tmap_save(icsmag_map,  dpi= 300,  height=8.3, width=11.7, units="in",
          filename="D:/OneDrive - CGIAR/SA_Team/Brenda/Mali/final_maps/ICAMAG.png")



