#remove.packages("ggmap")

# devtools::install_github("dkahle/ggmap", ref = "tidyup", force = TRUE)

# library(ggmap)

library(rgdal)
library(tigris)
library(raster)
library(janitor)
library(readxl)
library(tidyverse)
library(sp)
library(tmap)
library(leaflet)


####Longitude and Latitude data for Elementary Schools ####
JeffcoSchoolAdresses <- read_csv("noUploadToShinyIO/enrollmentTab/mapping/JeffcoSchoolAdresses_longlat_link.csv") %>% 
  clean_names("lower_camel") %>% 
  mutate(schoolId = str_pad(schoolId, width = 4, side = c('left'), pad = '0' ))

#### Look at structure of file####
# str(JeffcoSchoolAdresses)

####Access Map of Jeffco Elementary School boundaries ###
JeffcoElem <- readOGR(dsn = "noUploadToShinyIO/enrollmentTab/mapping", layer = "Boundary___ES")
####Access Map of Jeffco Elementary School boundaries ###
JeffcoMiddle <- readOGR(dsn = "noUploadToShinyIO/enrollmentTab/mapping", layer = "Boundary___MS")
####Access Map of Jeffco Elementary School boundaries ###
JeffcoHigh <- readOGR(dsn = "noUploadToShinyIO/enrollmentTab/mapping", layer = "Boundary___HS")
 # summary(JeffcoElem)

####Jeffco <- county_subdivisions("08", '059', cb = FALSE, year = "2016") 

#### Look at structure of map####
# str(JeffcoElem@data)
# proj4string(JeffcoElem)

####API needed for ggmap####
#register_google(key="AIzaSyCg6rciuznch73MPIWxCta2of7475D7OZ8")

#### Determine if the data from boundary map matches  #### Look at results in console
# Elementary
all(JeffcoElem$ES_name %in% JeffcoSchoolAdresses$school)
all(JeffcoSchoolAdresses$school %in% JeffcoElem$ES_name)

# Middle
all(JeffcoElem$MS_name %in% JeffcoSchoolAdresses$school)
all(JeffcoSchoolAdresses$school %in% JeffcoElem$MS_name)

# High
all(JeffcoElem$HS_name %in% JeffcoSchoolAdresses$school)
all(JeffcoSchoolAdresses$school %in% JeffcoElem$HS_name)

##### Filter shape data to remove any NAs ######
JeffcoMapElem <- merge(JeffcoElem, by.x = "ES_CampCod", JeffcoSchoolAdresses, by.y = "schoolId") 
JeffcoMapElem <- JeffcoMapElem[!is.na(JeffcoMapElem@data$ES_CampCod) , ]
schoolsInElemShape <- unique(JeffcoMapElem@data$ES_CampCod)

JeffcoMapMiddle  <-  merge(JeffcoMiddle, by.x = "MS_CampCod", JeffcoSchoolAdresses, by.y = "schoolId")
JeffcoMapMiddle <- JeffcoMapMiddle[!is.na(JeffcoMapMiddle@data$MS_CampCod) , ]
schoolsInMiddleShape <- unique(JeffcoMapMiddle@data$MS_CampCod)

JeffcoMapHigh  <-  merge(JeffcoHigh, by.x = "HS_CampCod", JeffcoSchoolAdresses, by.y = "schoolId")
JeffcoMapHigh <- JeffcoMapHigh[!is.na(JeffcoMapHigh@data$HS_CampCod) , ]
schoolsInHighShape <- unique(JeffcoMapHigh@data$HS_CampCod)


##### Filter shape data to One School ######

if(SchoolNumber %in% schoolsInElemShape){
Boundaries <- JeffcoMapElem[JeffcoMapElem@data$ES_CampCod == SchoolNumber , ]


####  Maps ####
JeffcoLatLng <- 
  readRDS(file = "data/mapping/jeffcoLatLng.rds")
schoolDistrict <- 
  readRDS(file ='data/mapping/schoolDistrict.rds')
icon.fa <- makeAwesomeIcon(icon = "id-card", 
                           markerColor = "darkblue", 
                           library = "fa",
                           iconColor = "#FFFFFF", 
                           squareMarker = TRUE)

JeffcoLatLngB <-  JeffcoLatLng %>% 
  mutate(SchoolID = str_pad(SchoolID, width = 4, side = 'left', pad = '0')) %>% 
  filter(SchoolID == SchoolNumber) %>% 
  mutate(popupLabel = paste0('<b>', 'Link to Website: ', '</b>', 
                             '<a href =', 
                             websiteSchool, 
                             '
                                 # target=, 
                                 # `"`_blank`"``,
                                 >', 
                             School, '</a>',
                             '<br>', '<b>', 'Articulation Area: ', '</b>', 
                             SchoolArea, 
                             '<br>', streetAddress, 
                             '<br>', City, ' ', PhysicalZipcode))
#### use tmap package to map the combine dataset

####add interactive elements to the maps  

map <- tm_shape(Boundaries)+
  tm_fill(col="ES_CampCod",
          title = "",
          alpha = 0.2, 
          palette = '#673785')+
  tm_borders(col = "#971b72")+
  tm_layout("",
            legend.text.size = 1)



# # esMap %>% leaflet(options = leafletOptions(zoomControl = FALSE, 
#                                          attributionControl=FALSE)) %>%
Map <-  tmap_leaflet(map) %>% 
  setView(lng = Boundaries$longitude, 
          lat = Boundaries$latitude, 
          zoom = 13) %>% 
  addTiles(group = "OpenStreeMap.Default") %>% 
    # addProviderTiles(providers$Esri.WorldStreetMap) %>% 
  addAwesomeMarkers(lng = JeffcoLatLngB$longitude,
                    lat = JeffcoLatLngB$latitude,
                    icon = icon.fa, 
                    popup = JeffcoLatLngB$popupLabel) %>% 
  clearControls()

Map$x$options = append(Map$x$options, list("zoomControl" = FALSE, "attributionControl"=FALSE))
Map

} else if (SchoolNumber %in% schoolsInMiddleShape){
  Boundaries <- JeffcoMapMiddle[JeffcoMapMiddle@data$MS_CampCod == SchoolNumber , ]
  
  
  ####  Maps ####
  JeffcoLatLng <- 
    readRDS(file = "data/mapping/jeffcoLatLng.rds")
  schoolDistrict <- 
    readRDS(file ='data/mapping/schoolDistrict.rds')
  icon.fa <- makeAwesomeIcon(icon = "id-card", 
                             markerColor = "darkblue", 
                             library = "fa",
                             iconColor = "#FFFFFF", 
                             squareMarker = TRUE)
  
  JeffcoLatLngB <-  JeffcoLatLng %>% 
    mutate(SchoolID = str_pad(SchoolID, width = 4, side = 'left', pad = '0')) %>% 
    filter(SchoolID == SchoolNumber) %>% 
    mutate(popupLabel = paste0('<b>', 'Link to Website: ', '</b>', 
                               '<a href =', 
                               websiteSchool, 
                               '
                                 # target=, 
                                 # `"`_blank`"``,
                                 >', 
                               School, '</a>',
                               '<br>', '<b>', 'Articulation Area: ', '</b>', 
                               SchoolArea, 
                               '<br>', streetAddress, 
                               '<br>', City, ' ', PhysicalZipcode))
  #### use tmap package to map the combine dataset
  
  ####add interactive elements to the maps  
  
  map <- tm_shape(Boundaries)+
    tm_fill(col="MS_CampCod",
            title = "",
            alpha = 0.2, 
            palette = '#673785')+
    tm_borders(col = "#971b72")+
    tm_layout("",
              legend.text.size = 1)
  
  
  
  # # esMap %>% leaflet(options = leafletOptions(zoomControl = FALSE, 
  #                                          attributionControl=FALSE)) %>%
  Map <-  tmap_leaflet(map) %>% 
    setView(lng = Boundaries$longitude, 
            lat = Boundaries$latitude, 
            zoom = 13) %>% 
    addTiles(group = "OpenStreeMap.Default") %>% 
    # addProviderTiles(providers$Esri.WorldStreetMap) %>% 
    addAwesomeMarkers(lng = JeffcoLatLngB$longitude,
                      lat = JeffcoLatLngB$latitude,
                      icon = icon.fa, 
                      popup = JeffcoLatLngB$popupLabel) %>% 
    clearControls()
  
  Map$x$options = append(Map$x$options, list("zoomControl" = FALSE, "attributionControl"=FALSE))
  Map
  
} else if (SchoolNumber %in% schoolsInHighShape){
  Boundaries <- JeffcoMapHigh[JeffcoMapHigh@data$HS_CampCod == SchoolNumber , ]
  
  
  ####  Maps ####
  JeffcoLatLng <- 
    readRDS(file = "data/mapping/jeffcoLatLng.rds")
  schoolDistrict <- 
    readRDS(file ='data/mapping/schoolDistrict.rds')
  icon.fa <- makeAwesomeIcon(icon = "id-card", 
                             markerColor = "darkblue", 
                             library = "fa",
                             iconColor = "#FFFFFF", 
                             squareMarker = TRUE)
  
  JeffcoLatLngB <-  JeffcoLatLng %>% 
    mutate(SchoolID = str_pad(SchoolID, width = 4, side = 'left', pad = '0')) %>% 
    filter(SchoolID == SchoolNumber) %>% 
    mutate(popupLabel = paste0('<b>', 'Link to Website: ', '</b>', 
                               '<a href =', 
                               websiteSchool, 
                               '
                                 # target=, 
                                 # `"`_blank`"``,
                                 >', 
                               School, '</a>',
                               '<br>', '<b>', 'Articulation Area: ', '</b>', 
                               SchoolArea, 
                               '<br>', streetAddress, 
                               '<br>', City, ' ', PhysicalZipcode))
  #### use tmap package to map the combine dataset
  
  ####add interactive elements to the maps  
  
  map <- tm_shape(Boundaries)+
    tm_fill(col="HS_CampCod",
            title = "",
            alpha = 0.2, 
            palette = '#673785')+
    tm_borders(col = "#971b72")+
    tm_layout("",
              legend.text.size = 1)
  
  
  
  # # esMap %>% leaflet(options = leafletOptions(zoomControl = FALSE, 
  #                                          attributionControl=FALSE)) %>%
  Map <-  tmap_leaflet(map) %>% 
    setView(lng = Boundaries$longitude, 
            lat = Boundaries$latitude, 
            zoom = 13) %>% 
    addTiles(group = "OpenStreeMap.Default") %>% 
    # addProviderTiles(providers$Esri.WorldStreetMap) %>% 
    addAwesomeMarkers(lng = JeffcoLatLngB$longitude,
                      lat = JeffcoLatLngB$latitude,
                      icon = icon.fa, 
                      popup = JeffcoLatLngB$popupLabel) %>% 
    clearControls()
  
  Map$x$options = append(Map$x$options, list("zoomControl" = FALSE, "attributionControl"=FALSE))
  Map
} else {
  
  JeffcoLatLng <- 
    readRDS(file = "data/mapping/jeffcoLatLng.rds")
  schoolDistrict <- 
    readRDS(file ='data/mapping/schoolDistrict.rds')
  icon.fa <- makeAwesomeIcon(icon = "id-card", 
                             markerColor = "darkblue", 
                             library = "fa",
                             iconColor = "#FFFFFF", 
                             squareMarker = TRUE)
  
  JeffcoLatLngB <-  JeffcoLatLng %>% 
    mutate(SchoolID = str_pad(SchoolID, width = 4, side = 'left', pad = '0')) %>% 
    filter(SchoolID == SchoolNumber) %>% 
    mutate(popupLabel = paste0('<b>', 'Link to Website: ', '</b>', 
                               '<a href =', 
                               websiteSchool, 
                               '
                                 # target=, 
                                 # `"`_blank`"``,
                                 >', 
                               School, '</a>',
                               '<br>', '<b>', 'Articulation Area: ', '</b>', 
                               SchoolArea, 
                               '<br>', streetAddress, 
                               '<br>', City, ' ', PhysicalZipcode))
  Map <- qtm(shp=schoolDistrict,
             fill = NULL)
  
  Map %>% leaflet(options = leafletOptions(zoomControl = FALSE, 
                                           attributionControl=FALSE)) %>% 
    addTiles() %>% 
    setView(lng = JeffcoLatLngB$longitude, 
            lat = JeffcoLatLngB$latitude, 
            zoom = 13) %>% 
    addProviderTiles(providers$Esri.WorldStreetMap) %>% 
    addAwesomeMarkers(lng = JeffcoLatLngB$longitude,
                      lat = JeffcoLatLngB$latitude,
                      icon = icon.fa, 
                      popup = JeffcoLatLngB$popupLabel)
}
