#conf <- geojsonio::geojson_read('D:/OneDrive - CGIAR/Analysis/CSO/KEN/clim_conflict_ips_overlays  (ACCLED-1997-2022).geojson')

#root <- '//alliancedfs.alliance.cgiar.org/WS18_Afrca_K_N_ACO/1.Data/Palmira/CSO/data/' 
rm(list=ls(all=TRUE))
library(sf)
library(terra)
root <- 'D:/OneDrive - CGIAR/Analysis/CSO/'
sroot <- '//alliancedfs.alliance.cgiar.org/WS18_Afrca_K_N_ACO/1.Data/Palmira/CSO'
### KENYA
iso <- 'KEN'
counties <- c("Turkana", "Marsabit", "West Pokot", "Samburu", "Baringo", "Laikipia", "Meru",
              "Isiolo", "Garissa", "Wajir", "Mandera", "Tana River", "Tharaka-Nithi", "Kitui", "Makueni")

##get Conflict data'
c_12_22 <- sf::st_read(paste0(root,iso,'/clim_conflict_ips_overlays (ACCLED-2012-2022).geojson'))
c_17_22 <- sf::st_read(paste0(root,iso,'/clim_conflict_ips_overlays (ACCLED-2017-2022).geojson'))
bdy1 <- geodata::gadm(country=iso, level=1, path=root)
bdy <- sf::st_as_sf(bdy1)
bdy <- sf::st_cast(bdy) #Remove duplicates  
bdy <- subset(bdy, bdy$NAME_1 %in% counties)
##Get wasting  Proportion of children under five years old that fall two standard deviations below the expected weight for their height

# wstng <- list.files(path = paste0(sroot,'/data/_global/prevalence_wasting'), pattern = 'S1_MEAN_201.*', full.names = T)
# wstng<- wstng[length(wstng) - (4:0)]
# 
# wstng <- rast(wstng)
# names(wstng) <- paste0('yr',2015:2019)
# wstng <- mean(wstng)
# writeRaster(x = wstng, paste0(root, '2015_2019_Global_mean_wasting_under5.tif'), overwrite=TRUE)
# 
# wast <- crop(wstng, bdy)
# wast <- mask(wast, bdy)
# writeRaster(x = wstng, paste0(root, iso, '/2015_2019_KEN_mean_wasting_under5.tif'), overwrite=TRUE)

w10_19 <- rast(paste0(root,iso,'/2010_2019_KEN_mean_wasting_under5.tif'))
w15_19 <- rast(paste0(root,iso,'/2015_2019_KEN_mean_wasting_under5.tif'))
minMax <- function(x){
  n <- x - minmax(x)[1]
  d <- minmax(x)[2] - minmax(x)[1]
  return(round((n/d)*100,2))
}
w10_19 <- minMax(w10_19) 
w15_19 <- minMax(w15_19) 
names(w10_19) <- "2010-2019 Wasting (%)"
names(w15_19) <- "2015-2019 Wasting (%)"

#Redefine labels
reLabel <- function(conf){
  temp <- conf[conf$conflict_clust_label=="High conflict"  ,]
  temp <- st_intersection(temp, bdy)
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

c_12_22 <- reLabel(c_12_22)
c_17_22 <- reLabel(c_17_22)

labs <- c("High conflict + High drought stress", "High conflict + Moderate-High drought stress",
          "High conflict + Moderate-Low drought stress", "High conflict + Low drought stress")


library("viridis") 
library(tmap)
library(mapview)
visualize <- function(conf, wast, layerName, fileName){
  tmap_mode("plot")
  map <- tm_shape(wast)+
    tm_raster(layerName, palette=viridis(100,direction	
                                                       =-1), style="cont")+
    tm_shape(conf) +
    tm_fill(col= "clust", palette="-YlOrRd", title="Conflict-climate intersection",
            legend.show = T, labels=labs) +
    tm_shape(bdy)+
    tm_text("NAME_1", size = 1.2, remove.overlap = TRUE, col ='black')+ 
    tm_borders(col = 'black', lwd=1)+
    tm_compass(type = "8star", size=6,position = c("center", "top")) +
    tm_scale_bar(breaks = c(0, 50, 100), text.size = 1.2, 
                 position = c("right", "bottom"), width = 0.25)+
    tm_layout(legend.outside=F, 
              legend.text.size = 1.0,
              legend.title.size= 1.3,
              legend.frame=F,
              legend.position = c("left", "bottom"), 
              legend.width = 0.5,
    )
  
  #map 
  
  tmap_save(map,  dpi= 600,  height=8, width=10, units="in",
            filename=paste0(root,iso,"/",fileName))
  
  return(map)
}

fileName <- 'WFP_2010_2019_Wasting_climate_2012_2022_conflict_intersection.png'
visualize(c_12_22, w10_19, names(w10_19), fileName)

fileName <- 'WFP_2015_2019_Wasting_climate_2017_2022_conflict_intersection.png'
visualize(c_17_22, w15_19, names(w15_19), fileName)
