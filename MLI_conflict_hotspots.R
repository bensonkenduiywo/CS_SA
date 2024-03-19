#conf <- geojsonio::geojson_read('D:/OneDrive - CGIAR/Analysis/CSO/KEN/clim_conflict_ips_overlays  (ACCLED-1997-2022).geojson')

#root <- '//alliancedfs.alliance.cgiar.org/WS18_Afrca_K_N_ACO/1.Data/Palmira/CSO/data/' 
rm(list=ls(all=TRUE))
library(terra)
path <- 'D:/OneDrive - CGIAR/SA_Team/Data/Mali/'
#read conflict data
conf <- sf::st_read("D:/OneDrive - CGIAR/SA_Team/Data/CSO/MLI/clim_conflict_ips_overlays (ACCLED-2017-2022).geojson")
#get mali shapefile
bdy <- sf::st_as_sf(geodata::gadm(country = 'MALI',level = 2, path = tempdir() ))
bdy <- bdy[c('NAME_1','NAME_2')]

#rename admin 2 to remove accents
bdy$NAME_2 <- stringi::stri_trans_general(str = bdy$NAME_2, id = "Latin-ASCII")
#bdy$NAME_2 <- toupper(bdy$NAME_2)

#=====Create New Legend labels=========================
reLabel <- function(conf){
  county_conf <- conf
  i <- county_conf$intersect_conf_clim
  county_conf$intersect_conf_clim[i=="High conflict-[High levels of precipitation/Low levels of drought stress]"] <-
    "High conflict + Low drought stress" 
  county_conf$intersect_conf_clim[i=="High conflict-[High-Moderate levels of precipitation/Moderate-Low levels of drought stress]"] <-
    "High conflict + Moderate-Low drought stress" 
  county_conf$intersect_conf_clim[i=="High conflict-[Moderate-Low levels of precipitation/Moderate-High levels of drought stress]"] <-
    "High conflict + Moderate-High drought stress"
  county_conf$intersect_conf_clim[i=="High conflict-[Low levels of precipitation/High levels of drought stress]"] <-
    "High conflict + High drought stress" 
  
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High levels of precipitation/Low levels of drought stress]"] <-
    "Moderate conflict + Low drought stress" 
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High-Moderate levels of precipitation/Moderate-Low levels of drought stress]"] <-
    "Moderate conflict + Moderate-Low drought stress"
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Moderate-Low levels of precipitation/Moderate-High levels of drought stress]"] <-
    "Moderate conflict + Moderate-High drought stress"
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Low levels of precipitation/High levels of drought stress]"] <-
    "Moderate conflict + High drought stress"
  
  county_conf$intersect_conf_clim[i=="Limited conflict-[High levels of precipitation/Low levels of drought stress]"] <-
    "Limited conflict + Low of drought stress"
  county_conf$intersect_conf_clim[i=="Limited conflict-[High-Moderate levels of precipitation/Moderate-Low levels of drought stress]"] <-
    "Limited conflict + Moderate-Low of drought stress"
  county_conf$intersect_conf_clim[i=="Limited conflict-[Moderate-Low levels of precipitation/Moderate-High levels of drought stress]"] <-
    "Limited conflict + Moderate-High of drought stress"
  
  i <- county_conf$intersect_conf_clim
  county_conf$clust[i=="Limited conflict + Low of drought stress"] <- 11
  county_conf$clust[i=="Limited conflict + Moderate-Low of drought stress"] <- 10
  county_conf$clust[i=="Limited conflict + Moderate-High of drought stress"] <- 9
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

conf <-reLabel(conf)
labs <- unique(conf$intersect_conf_clim[order(conf$clust)]) 
#================================================Plot Map====================

library(tmap)
library(mapview)
tmap_mode("plot")
map <- tm_shape(conf) +
  #tm_fill(col= "clust", palette= c("red4", "red2", "orange2", "yellow", "grey85"), title="Conflict-climate intersection",
          #legend.show = T, labels=labs, popup.vars="NAME_3") + #"-YlOrRd" palette = viridis(100,direction	=-1)
  tm_fill(col= "clust", palette= viridis(11,direction	=-1, option = "H", alpha=0.7), title="Conflict-climate intersection",
          legend.show = T, labels=labs, popup.vars=c("NAME_2" ,"NAME_3", "FATALITIES", "climvar_medn_prec",  "livelihoods","median_female_edu", 
                       "female_population", "median_male_edu", "climvar_NDWS_median")
          ) + #"-YlOrRd" palette = viridis(100,direction	=-1)
  #tm_text("label", col='white', size = 1.1)+
  tm_shape(bdy)+
  tm_text("NAME_2", size = 1.0, col='black', remove.overlap = TRUE)+ 
  tm_borders(col = "black")+
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_scale_bar(breaks = c(0, 75, 150), text.size = 1, 
              position = c("right", "bottom"))+
  tm_mouse_coordinates()+
  tm_layout(legend.outside=F, 
            legend.text.size = 0.9,
            legend.text.color = "black",
            legend.title.size= 1.2,
            legend.title.fontface = 2,
            legend.frame=F,
            asp=1.3,
            legend.just = c("left", "top"), 
            legend.position  = c("left", "top"),
            legend.width = 0.75,
            inner.margins = c(0.02, 0.02, 0.05, 0.02)
  )
map 

fpath <- 'D:/OneDrive - CGIAR/SA_Team/Data/Mali/Results/'
tmap_save(map, dpi= 300,  width=11.7, height =8.3, units="in",
          filename=paste0(fpath, "Clim-Conflict-map.png"))

conf$NAME_3 <- stringi::stri_trans_general(str = conf$NAME_3, id = "Latin-ASCII")
conf$NAME_2 <- stringi::stri_trans_general(str = conf$NAME_2, id = "Latin-ASCII")
tmap_mode("view")
map <- tm_shape(conf) +
  tm_fill(col= "clust", palette= viridis(11,direction	=-1, option = "H", alpha=0.7), title="Conflict-climate intersection",
          legend.show = T, labels=labs, popup.vars=c("NAME_2" ,"NAME_3", 'intersect_conf_clim', "FATALITIES", "climvar_medn_prec",  "livelihoods","median_female_edu", 
                                                     "female_population", "male_population", "median_male_edu", "climvar_NDWS_median")
  ) +
  tm_shape(bdy)+
  tm_borders(col = "black")+
  tm_mouse_coordinates()+
  tm_view(view.legend.position =c("right", "top"))+
  tm_layout(legend.stack="vertical")+
  tm_format("World")

map 

tmap_save(map, dpi= 300,  width=11.7, height =8.3, units="in",
          filename=paste0(fpath, "Clim-Conflict-map.html"))
