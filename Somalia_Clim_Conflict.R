#load libraries
library(geojsonio)
library(geodata)
library(tmap)
library(mapview)

#root folder
root <- "D:/OneDrive - CGIAR/SA_Team/Brenda/Somalia/"
data <- paste0(root,"data/")


#somalia clim conflict data
clim_path <- paste0(root,"data/clim_conflict_ips_overlays.geojson")
clim_conflict <- geojsonio::geojson_sf(clim_path)
View(clim_conflict)

#somalia who layer
who_path <- paste0(root,"data/clim_conflict_ips_overlays_who_layers 1.geojson")
who <- geojsonio::geojson_sf(who_path)
who_high <- who[who$conflict_clust_label=="High conflict"  ,]
som_conflict$label[som_conflict$clust=='1' & som_conflict$FATALITIES=='38'] <- '1'
som_conflict$label[som_conflict$clust=='1' & som_conflict$FATALITIES=='515'] <- '2'
View(who_high)

#get somalia boundary
som_shp <- geodata::gadm(country='SOM', level=1, path=data)
som <- sf::st_as_sf(som_shp)
som <- sf::st_cast(som)
plot(som)

#conflict data
high_conflict <- clim_conflict[clim_conflict$conflict_clust_label=="High conflict"  ,]
View(high_conflict)
i <- high_conflict$intersect_conf_clim
high_conflict$intersect_conf_clim[i=="High conflict-[High levels of precipitation/Low levels of drought stress]"] <-
  "High conflict + Low drought stress" 
high_conflict$intersect_conf_clim[i=="High conflict-[Moderate levels of precipitation/Moderate levels of drought stress]"] <-
  "High conflict + Moderate drought stress"
high_conflict$intersect_conf_clim[i=="High conflict-[Low levels of precipitation/High levels of drought stress]"] <-
  "High conflict + High drought stress"
others <- clim_conflict[clim_conflict$conflict_clust_label!="High conflict"  ,]
others$intersect_conf_clim <- 'Low-Moderate Conflict-Climate Co-occurence'
View(others)
unique(others$intersect_conf_clim)
unique(high_conflict$intersect_conf_clim)
som_conflict <- rbind(high_conflict, others, deparse.level = 1)
View(som_conflict)
unique(som_conflict$intersect_conf_clim)
i <- som_conflict$intersect_conf_clim
som_conflict$clust[i=="Low-Moderate Conflict-Climate Co-occurence"] <- 4
som_conflict$clust[i=="High conflict + Low drought stress"] <- 3
som_conflict$clust[i=="High conflict + Moderate drought stress"] <- 2
som_conflict$clust[i=="High conflict + High drought stress"] <- 1
som_conflict$clust <- as.factor(som_conflict$clust)
labs <- c("High conflict + High drought stress", "High conflict + Moderate drought stress",
          "High conflict + Low drought stress", "Low-Moderate Conflict-Climate Co-occurence")
#add labels for high conf and high drought stress
som_conflict$label[som_conflict$clust=='1' & som_conflict$FATALITIES=='38'] <- '1'
som_conflict$label[som_conflict$clust=='1' & som_conflict$FATALITIES=='515'] <- '2'
View(som_conflict)
#maps
tmap_mode("plot")
map <- tm_shape(som_conflict)+
  tm_fill(col= "clust", palette= c("red4", "red2", "orange2", "grey85"), 
          title="Conflict-Climate Intersection",
          legend.show = T, labels=labs)+
  tm_text("label", col='white', size = 1.2)+
  tm_shape(som)+
  tm_text("NAME_1", size = 1.0, col='black', remove.overlap = TRUE)+
  tm_borders(col = "black")+
  tm_compass(type = "8star", size=6, position = c("left", "top")) +
  tm_scale_bar(breaks = c(0, 75, 150), text.size = 1, 
               position = c("right", "bottom"))+
  tm_layout(legend.outside=F, 
            legend.text.size = 1.3,
            legend.title.size= 1.5,
            legend.title.fontface = 2,
            legend.frame=F, 
            legend.position  = c("right", "bottom"),
            legend.width = 0.75,
            inner.margins = c(0.02, 0.02, 0.05, 0.02)
)
#map  
final <- paste0(root,"maps/somalia.png")
tmap_save(map,  dpi= 600,  height=11.7, width=8, units="in",
          filename=final)
  
  
  
  