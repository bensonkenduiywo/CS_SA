library(readxl)
library(sf)
library(terra)
library("viridis") 
library(tmap)
library(mapview)
library(geodata)

root <- "D:/OneDrive - CGIAR/SA_Team/"
#fcs
read_fcs <- function(path,sheet_name){
  df <- read_excel(path, sheet=sheet_name)
  df_months <- subset(df, select=-c(County, FCS))
  #remove percentage
  df_months <- df_months * 100
  #calculate max values in each row
  df$max <- apply(df_months, 1, max, na.rm=TRUE)
  return(df)
}
fcs_2019 <- read_fcs(paste0(root,'Data/IPC/2019 Outcome Analysis.xlsx'),"NEW_FCS")
fcs_2020 <- read_fcs(paste0(root,'Data/IPC/2020 Outcome Analysis.xlsx'),"NEW_FCS")
fcs_2021 <- read_fcs(paste0(root,'Data/IPC/2021 Outcome Analysis.xlsx'),"NEW_FCS")
View(fcs_2020)
#get unique county names
counties <- unique(fcs_2019$County)

calculate_mean <- function(df, counties, name){
  final_df <- data.frame(county=character(), name=numeric())
  for (county in counties){
    county_df <- df[df$County == county, ]
    mean <- mean(county_df$max)
    final_df <- rbind(final_df, c(county, mean))
  }
  colnames(final_df)[1] <- "County"
  colnames(final_df)[2] <- name
  return(final_df)
}
final_fcs_2019 <- calculate_mean(fcs_2019, counties, "fcs_2019")
final_fcs_2020 <- calculate_mean(fcs_2020, counties, "fcs_2020")
final_fcs_2021 <- calculate_mean(fcs_2021, counties, "fcs_2021")
View(final_fcs_2019)
#2022
fcs_2022 <- read_excel(paste0(root,'Data/IPC/Outcome Indicator analysis_July_2022.xlsx'), sheet="FCS_2022")
fcs_months <- subset(fcs_2022,select=-1)
View(final_fcs_2020)
fcs_months <- fcs_months*100
fcs_2022$max_poor <- pmax(fcs_months$jan_poor,fcs_months$april_poor, fcs_months$may_poor, fcs_months$june_poor, na.rm=TRUE)
fcs_2022$max_borderline <- pmax(fcs_months$jan_borderline,fcs_months$april_borderline, fcs_months$may_borderline, fcs_months$june_borderline, na.rm=TRUE)
fcs_2022$fcs_2022 <- rowMeans(fcs_2022[,c("max_poor","max_borderline")])
final_fcs_2022 <- subset(fcs_2022, select=c("County", "fcs_2022"))
#average fcs over the years
fcs_19_20 <- merge(final_fcs_2019,final_fcs_2020, by = "County")
fcs_21_22 <- merge(final_fcs_2021,final_fcs_2022, by = "County")
View(fcs)
fcs <- merge(fcs_19_20,fcs_21_22, by = "County")
fcs$fcs_2019 <- as.numeric(fcs$fcs_2019)
fcs$fcs_2020 <- as.numeric(fcs$fcs_2020)
fcs$fcs_2021 <- as.numeric(fcs$fcs_2021)
fcs$fcs_2022 <- as.numeric(fcs$fcs_2022)
fcs$fcs <- round(rowMeans(fcs[,c("fcs_2019","fcs_2020","fcs_2021","fcs_2022")]),1)
fcs <- subset(fcs, select=c("County","fcs"))

#rCSI
read_rcsi <- function(path,sheet_name){
  df <- read_excel(path, sheet=sheet_name)
  df_months <- subset(df, select=-c(County, rCSI))
  #remove percentage
  df_months <- df_months * 100
  #calculate max values in each row
  df$max <- apply(df_months, 1, max, na.rm=TRUE)
  return(df)
}
rcsi_2019 <- read_rcsi(paste0(root,'Data/IPC/2019 Outcome Analysis.xlsx'),"NEW_rCSI")
rcsi_2020 <- read_rcsi(paste0(root,'Data/IPC/2020 Outcome Analysis.xlsx'),"NEW_rCSI")
rcsi_2021 <- read_rcsi(paste0(root,'Data/IPC/2021 Outcome Analysis.xlsx'),"NEW_rCSI")
View(fcs)
final_rcsi_2019 <- calculate_mean(rcsi_2019, counties,"rcsi_2019")
final_rcsi_2020 <- calculate_mean(rcsi_2020, counties,"rcsi_2020")
final_rcsi_2021 <- calculate_mean(rcsi_2021, counties,"rcsi_2021")
rcsi_2022 <- read_excel(paste0(root,'Data/IPC/Outcome Indicator analysis_July_2022.xlsx'), sheet="RCSI_2022")
rcsi_months <- subset(rcsi_2022,select=-1)
rcsi_months <- rcsi_months * 100
rcsi_2022$max_stressed <- pmax(rcsi_months$jan_stressed,rcsi_months$april_stressed,rcsi_months$may_stressed,rcsi_months$june_stressed,na.rm=TRUE)
rcsi_2022$max_crisis <- pmax(rcsi_months$jan_crisis,rcsi_months$april_crisis,rcsi_months$may_crisis,rcsi_months$june_crisis,na.rm=TRUE)
rcsi_2022$rcsi_2022 <- rowMeans(rcsi_2022[,c("max_stressed","max_crisis")])
final_rcsi_2022 <- subset(rcsi_2022, select=c("County", "rcsi_2022"))
rcsi_19_20 <- merge(final_rcsi_2019,final_rcsi_2020, by="County")
rcsi_21_22 <- merge(final_rcsi_2021,final_rcsi_2022, by="County")
rcsi <- merge(rcsi_19_20, rcsi_21_22, by="County")
rcsi$rcsi_2019 <- as.numeric(rcsi$rcsi_2019)
rcsi$rcsi_2020 <- as.numeric(rcsi$rcsi_2020)
rcsi$rcsi_2021 <- as.numeric(rcsi$rcsi_2021)
rcsi$rcsi_2022 <- as.numeric(rcsi$rcsi_2022)
rcsi$rcsi <- round(rowMeans(rcsi[,c("rcsi_2019","rcsi_2020","rcsi_2021","rcsi_2022")]),1)
rcsi <- subset(rcsi, select=c("County","rcsi"))

#livelihood
read_livelihood <- function(path,sheet_name){
  df <- read_excel(path, sheet=sheet_name)
  df_months <- subset(df, select=-c(County, Coping))
  #remove percentage
  df_months <- df_months * 100
  #calculate max values in each row
  df$max <- apply(df_months, 1, max, na.rm=TRUE)
  return(df)
}
lh_2019 <- read_livelihood(paste0(root,'Data/IPC/2019 Outcome Analysis.xlsx'),"NEW_Livelihood")
lh_2020 <- read_livelihood(paste0(root,'Data/IPC/2020 Outcome Analysis.xlsx'),"NEW_Livelihood")
lh_2021 <- read_livelihood(paste0(root,'Data/IPC/2021 Outcome Analysis.xlsx'),"NEW_Livelihood")
View(lh)
final_lh_2019 <- calculate_mean(lh_2019,counties,"lh_2019")
final_lh_2020 <- calculate_mean(lh_2020,counties,"lh_2020")
final_lh_2021 <- calculate_mean(lh_2021,counties,"lh_2021")
lh_2022 <- read_excel(paste0(root,'Data/IPC/Outcome Indicator analysis_July_2022.xlsx'), sheet="Livelihood_coping")
lh_months <- subset(lh_2022,select=-1)
lh_months <- lh_months * 100
lh_2022$max_crisis <- pmax(lh_months$april_crisis,lh_months$may_crisis,lh_months$june_crisis,lh_months$jan_crisis,na.rm=TRUE)
lh_2022$max_emerg <- pmax(lh_months$april_emergencies,lh_months$may_emergencies,lh_months$june_emergencies,lh_months$jan_emergencies,na.rm=TRUE)
lh_2022$lh_2022 <- rowMeans(lh_2022[,c("max_crisis","max_emerg")])
final_lh_2022 <- subset(lh_2022, select=c("County","lh_2022"))
lh_19_20 <- merge(final_lh_2019,final_lh_2020, by="County")
lh_21_22 <- merge(final_lh_2021,final_lh_2022, by="County")
lh <- merge(lh_19_20,lh_21_22, by="County")
lh$lh_2019 <- as.numeric(lh$lh_2019)
lh$lh_2020 <- as.numeric(lh$lh_2020)
lh$lh_2021 <- as.numeric(lh$lh_2021)
lh$lh_2022 <- as.numeric(lh$lh_2022)
lh$lh <- round(rowMeans(lh[,c("lh_2019","lh_2020","lh_2021","lh_2022")]))
lh <- subset(lh, select=c("County","lh"))
#IPC data
ipc <- read_excel(paste0(root,'Data/IPC/IPC.xlsx'))
View(ipc_months)
ipc_months <- subset(ipc, select=-1)
ipc_months <- ipc_months * 100
ipc$mean <- round(rowMeans(ipc_months[,1:ncol(ipc_months)],na.rm=TRUE),1)
ipc <- subset(ipc, select=c("County","mean"))
colnames(ipc)[2] <- "IPC"
View(ipc)
#rename counties
ipc$County[ipc$County=="Lamu county"] <- "Lamu"
ipc$County[ipc$County=="Tharaka"] <- "Tharaka-Nithi"
ipc$County[ipc$County=="TANA RIVER"] <- "Tana River"
ipc$County[ipc$County=="Taita"] <- "Taita Taveta"
ipc$County[ipc$County=="West pokot"] <- "West Pokot"

lh$County[lh$County=="Tharaka Nithi"] <- "Tharaka-Nithi"
lh$County[lh$County=="Tana river"] <- "Tana River"
lh$County[lh$County=="West pokot"] <- "West Pokot"

fcs$County[fcs$County=="Tharaka Nithi"] <- "Tharaka-Nithi"
fcs$County[fcs$County=="Tana river"] <- "Tana River"
fcs$County[fcs$County=="West pokot"] <- "West Pokot"

rcsi$County[rcsi$County=="Tharaka Nithi"] <- "Tharaka-Nithi"
rcsi$County[rcsi$County=="Tana river"] <- "Tana River"
rcsi$County[rcsi$County=="West pokot"] <- "West Pokot"
#merge all dataframes
keco <- merge(lh,rcsi, by="County")
keco2 <- merge(fcs,ipc, by="County")
keco <- merge(keco,keco2, by="County")
names(keco)[names(keco)=="County"] <- "COUNTY"
View(keco)
#get counties shapefile
county_shp <- geodata::gadm(country='KEN', level=1, path=tempdir())
county_shp <- sf::st_as_sf(county_shp)
county_shp <- sf::st_cast(county_shp)
county_shp$NAME_1
county_shp <- subset(county_shp, select=c("NAME_1"))
names(county_shp)[names(county_shp) == "NAME_1"] <- "COUNTY"
View(county_shp)
names(county_shp)
#official kenya shapefile
kenya <- sf::st_read(paste0(root,"Data/Admin/KEN/Kenya_county_dd.shp"))
kenya <- sf::st_as_sf(kenya)
kenya <- subset(kenya, select=c("county"))
names(kenya)[names(kenya) == "county"] <- "COUNTY"
kenya
#merge county and keco data
kenya_fs <- merge(county_shp,keco, by="COUNTY", all=TRUE)
View(kenya_fs)
plot(kenya_keco)
kenya_keco <- merge(kenya, keco, by="COUNTY", all=TRUE)
View(kenya_keco)
#Maps
#lakes shapefile
lakes <- sf::st_read(paste0(root,"Brenda/WFP/data/KEN_Lakes/KEN_Lakes.shp"))
lakes <- sf::st_as_sf(lakes)
lakes
st_is_valid(lakes)
plot(lakes)
indian_ocean <- sf::st_read(paste0(root,"Brenda/WFP/data/eez_iho/eez_iho.shp"))
indian_ocean <- sf::st_as_sf(indian_ocean)
ocean <- sf::st_read(paste0(root,"Brenda/WFP/data/iho/iho.shp"))
ocean <- sf::st_as_sf(ocean)
st_is_valid(ocean)
plot(ocean)
#neighbouring countries
cc <- country_codes()
View(cc)
neighbor <- geodata::gadm(country=c('ETH','SSD','SOM','TZA','UGA'), level=0, path=tempdir())
neighbors <- sf::st_as_sf(neighbor)
View(neighbors)
#conflict data
conflict <- sf::st_read(paste0(root,"Data/CSO/KEN/clim_conflict_ips_overlays (ACCLED-2017-2022).geojson"))
unique(conflict$clim_cluster_short_label)
names(conflict)
#all conflict clusters
all_conflicts <- function(conf){
  county_conf <- conf[conf$conflict_clust_label != "Limited conflict"  ,]
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
  
  i <- county_conf$intersect_conf_clim
  
  county_conf$clust[i=="Moderate conflict + Low drought stress"] <- 8
  county_conf$clust[i=="Moderate conflict + Moderate-Low drought stress"] <- 7
  county_conf$clust[i=="Moderate conflict + Moderate-High drought stress"] <- 6
  county_conf$clust[i=="Moderate conflict + High drought stress"] <- 5
  
  county_conf$clust[i=="High conflict + Low drought stress"] <- 4
  county_conf$clust[i=="High conflict + Moderate-Low drought stress"] <- 3
  county_conf$clust[i=="High conflict + Moderate-High drought stress"] <- 2
  county_conf$clust[i=="High conflict + High drought stress"] <- 1
  county_conf$clust <- as.factor(county_conf$clust)
  return(county_conf)
}
all_conflict <- all_conflicts(conflict)
all_label <- conf_label <- c("High conflict + High drought", "High conflict + Moderate-High drought",
                             "High conflict + Moderate-Low drought", "High conflict + Low drought", 
                             "Moderate conflict + High drought", "Moderate conflict + Moderate-High drought",
                             "Moderate conflict + Moderate-Low drought", "Moderate conflict + Low drought")

#high drought stress
high_drought <- function(conf){
  county_conf <- conf[conf$clim_cluster_short_label == "High levels of drought stress/Low precipitation"  ,]
  i <- county_conf$intersect_conf_clim
  county_conf$intersect_conf_clim[i=="High conflict-[High levels of drought stress/Low precipitation]"] <-
    "High drought stress + High conflict"
  
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High levels of drought stress/Low precipitation]"] <-
    "High drought stress + Moderate conflict"
  
  county_conf$intersect_conf_clim[i=="Limited conflict-[High levels of drought stress/Low precipitation]"] <-
    "High drought stress + Limited conflict"
  
  i <- county_conf$intersect_conf_clim
  
  county_conf$clust[i=="High drought stress + Limited conflict"] <- 3
  county_conf$clust[i=="High drought stress + Moderate conflict"] <- 2
  county_conf$clust[i=="High drought stress + High conflict"] <- 1
  
  county_conf$clust <- as.factor(county_conf$clust)
  return(county_conf)
}
high_drought_stress <- high_drought(conflict)
View(high_conf)
high_drought_label <- c("High drought stress + High conflict","High drought stress + Moderate conflict",
                        "High drought stress + Limited conflict")
#high conflict cluster
high_conflict <- function(conf){
  temp <- conf[conf$conflict_clust_label=="High conflict"  ,]
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
high_conf <- high_conflict(conflict)
high_label <- c("High conflict + High drought", "High conflict + Moderate-High drought",
                "High conflict + Moderate-Low drought", "High conflict + Low drought")
#tmap
tmap_mode("plot")
map_all <- tm_shape(kenya_keco)+
  tm_polygons(col="fcs",border.col = "black", title="FCS (%)",style = "cont", palette = viridis(100,direction	=-1),legend.show = F)+
  tm_shape(lakes) +
  tm_fill(col= "skyblue")+
  tm_shape(ocean)+
  tm_fill(col="skyblue")+
  tm_shape(neighbors)+
  tm_borders(col="black", lwd=1)+
  #tm_text("COUNTRY",size = 0.5, remove.overlap = TRUE, col ='black')+
  tm_shape(all_conflict) +
  tm_fill(col= "clust", palette="-YlOrRd", title="Conflict-Climate intersection",
          legend.show = T, labels=all_label)+
  tm_compass(type = "8star", size=9,position = c("right", "top")) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 1.5, 
               position = c("right", "bottom"))+
  tm_layout(legend.outside=F, 
            legend.text.size = 0.8,
            legend.text.color = "black",
            legend.title.size= 1.0,
            legend.title.color = "black",
            legend.title.fontface = 2,
            legend.frame=F,
            asp=0.7, 
            legend.width = 0.7,
            inner.margins = c(0,0.05,0,0.05)
  )
map_all
map_high_conf <- tm_shape(kenya_keco)+
  tm_polygons(col="fcs",border.col = "black", title="FCS (%)",style = "cont", palette = viridis(100,direction	=-1),legend.show = F)+
  tm_shape(lakes) +
  tm_fill(col= "skyblue")+
  tm_shape(ocean)+
  tm_fill(col="skyblue")+
  tm_shape(neighbors)+
  tm_borders(col="black", lwd=1)+
  #tm_text("COUNTRY",size = 0.5, remove.overlap = TRUE, col ='black')+
  tm_shape(high_conf) +
  tm_fill(col= "clust", palette="-YlOrRd", title="Conflict-Climate intersection",
          legend.show = T, labels=high_label)+
  tm_compass(type = "8star", size=9,position = c("right", "top")) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 1.5, 
               position = c("right", "bottom"))+
  tm_layout(legend.outside=F, 
            legend.text.size = 1.2,
            legend.text.color = "black",
            legend.title.size= 1.2,
            legend.title.color = "black",
            legend.title.fontface = 2,
            legend.frame=F,
            asp=0.7,
            legend.position = c("left", "bottom"), 
            legend.width = 0.7,
            inner.margins = c(0,0.05,0,0.05)
  )
map_high_conf
map_high_drought <- tm_shape(kenya_keco)+
  tm_polygons(col="fcs",border.col = "black", title="FCS (%)",style = "cont", palette = viridis(100,direction	=-1),legend.show = F)+
  tm_shape(lakes) +
  tm_fill(col= "skyblue")+
  tm_shape(ocean)+
  tm_fill(col="skyblue")+
  tm_shape(neighbors)+
  tm_borders(col="black", lwd=1)+
  #tm_text("COUNTRY",size = 0.5, remove.overlap = TRUE, col ='black')+
  tm_shape(high_drought_stress) +
  tm_fill(col= "clust", palette="-YlOrRd", title="Conflict-Climate intersection",
          legend.show = T, labels=high_drought_label)+
  tm_compass(type = "8star", size=9,position = c("right", "top")) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 1.5, 
               position = c("right", "bottom"))+
  tm_layout(legend.outside=F, 
            legend.text.size = 1.2,
            legend.text.color = "black",
            legend.title.size= 1.2,
            legend.title.color = "black",
            legend.title.fontface = 2,
            legend.frame=F,
            asp=0.7,
            legend.position = c("left", "bottom"), 
            legend.width = 0.7,
            inner.margins = c(0,0.05,0,0.05)
  )
map_high_drought
legend <- tm_shape(kenya_keco)+
  tm_polygons(col="fcs",border.col = "black", title="FCS (%)",style = "cont", palette = viridis(100,direction	=-1))+
  tm_layout(legend.only = TRUE, legend.text.size=1.5, legend.title.size = 1.6, legend.title.fontface = 2, asp=1.4)
legend
  
  
  
  
final <- tmap_arrange(map_all, map_high_conf, map_high_drought, legend, nrow=1,
                      ncol=4, widths=c(0.3,0.3,0.3,0.1))
final
tmap_save(final,  dpi= 300,  height=8.3, width=11.7, units="in",
          filename="D:/OneDrive - CGIAR/SA_Team/Brenda/WFP/KECO_MAPS/x8.png")

