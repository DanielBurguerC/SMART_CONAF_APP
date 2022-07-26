source("dependencias.R")
source("funciones.R")
inputs <- vroom("1-Data/inputs.csv")
names(inputs) <- c("REGION", "UNIDAD", "COD", "ANO")
limite_snaspe <- sf::st_read("1-Data/snaspe.shp")

mydata <- vroom("1-Data/Smart_connect.csv") %>%  janitor::clean_names()

var_null <-  mydata %>% dlookr::diagnose() %>%
  filter(missing_percent == 100) %>%
  pull(variables)
# 
mydata <- mydata %>%
  dplyr::select(-var_null)
# 
mydata <-  mydata %>%
  mutate(fecha = paste(flipTime::AsDate(mydata$waypoint_date),mydata$waypoint_time)) %>%
  mutate(fecha = as.POSIXct(fecha, formats = "%Y-%m-%d %H:%M:%S")-chron::hours(3)) %>%
  group_by(patrol_id) %>% mutate(inicio = min(fecha)) %>% mutate(final = max(fecha)) %>%
  mutate(duracion = (max(fecha)-min(fecha))/3600) %>%
  mutate(duracion = as.numeric(duracion)) %>%
  arrange(fecha) %>% ungroup() %>%
  mutate(id = paste0("id-", 1:dim(mydata)[1]),.before = conservation_area_id) %>%
  mutate(ano=format(fecha, format="%Y")) %>%
  mutate(popup = paste0(amenazas,especies))
# 
mydata$observation_category_0 <-  mydata$observation_category_0 %>% replace_na('Otros')
mydata$amenazas <- mydata$amenazas %>% tidyr::replace_na('')
mydata$especies <- mydata$especies %>% tidyr::replace_na('')
# 
# 
mydata <-  mydata %>% mutate(popup = paste0(amenazas,especies))
# 
coordenadas <-  cbind(x=mydata$x,y=mydata$y)
csv_spatial <- SpatialPointsDataFrame(coordenadas,data = mydata)
projection(csv_spatial)<- CRS("+proj=longlat +datum=WGS84")
# 
csv_spatial@data <- as.data.frame(csv_spatial@data)
# 
csv_spatial@data <- cbind(csv_spatial@data, ano = format(csv_spatial$inicio, format = "%Y"))
# 


for (i in 1:length(names(mydata))){
  if (names(mydata)[i]=="patrol_id") {
    names(mydata)[i] <- "Patrol_ID"
  } 
  
}

patrol_id <-  unique(mydata[c("Patrol_ID","conservation_area_id")])
# 
tracks <-  geojson_sf("1-Data/Smart_track.geojson")




# 
dates <- as.POSIXct(tracks$Patrol_Start_Date, format = "%Y-%m-%dT%H:%M:%S.000+0000")
# 
tracks <- cbind(tracks,ano=format(dates, format = "%Y"))
tracks_1 <- sf::st_transform(tracks, sf::st_crs("+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs"))



tracks_2 <- cbind(tracks_1, distances= st_length(tracks_1, varname = "distances"))
filtro_tracks_datos <- tracks_2[(tracks_2$Patrol_ID %in% patrol_id$Patrol_ID),]
tracks_data <- merge(filtro_tracks_datos, patrol_id, by = "Patrol_ID")
tracks_data <- st_transform(tracks_data, st_crs(tracks))

