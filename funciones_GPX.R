# Librerías y entorno

# rm(list=ls())

library(sf)         # Estadística espacial
library(leaflet)    # Mapas
library(tidyverse)  # tratamiento de datos y gráficos

# Carga de fichero GPX

st_read("./Data/bocaleones.gpx", layer = "track_points")-> bocaleones


##  Representación gráfica

# gráfico siple
ggplot(bocaleones) +
  geom_sf() +
  theme_bw()

# Representación en mapa
# Creamos la traza a partir de los puntos y lo representamos en el mapa.

bocaleones.trc <- bocaleones %>%
  st_combine() %>%
  st_cast(to = "LINESTRING") %>%
  st_sf()

leaflet()  %>% 
  addTiles() %>%
  setMaxBounds(as.double(st_bbox(bocaleones)[1]),as.double(st_bbox(bocaleones)[2]),as.double(st_bbox(bocaleones)[3]),as.double(st_bbox(bocaleones)[4])) %>%
  addPolylines(data = bocaleones.trc)


## Cálculo y representación de la velocidad

# Calculo de la distancia entre dos puntos en metros
st_distance(bocaleones$geometry[2], bocaleones$geometry[1])

# Calculo del tiempo entre dos puntos en segundos
# Bellavista$time
as.double(bocaleones$time[3]-bocaleones$time[1]) 

# Cálculo de la velocidad entre 2 puntos (Km/h)
st_distance(bocaleones$geometry[3], bocaleones$geometry[1])*3.6/as.double(bocaleones$time[3]-bocaleones$time[1])

# Para generalizarlo usar la función lag() que da el valor del registro anterior

bocaleones %>%
  select(track_seg_point_id,ele,time,geometry) %>%
  mutate(lagtime= lag(time)) %>%
  mutate (sigtime= as.double(time-lag(time)))%>%
  mutate(siggeom= lag(geometry)) %>%
  slice (-1)%>%
  mutate (distancia = st_distance(geometry,siggeom, by_element = TRUE)) %>%
  mutate (velocidad = distancia*3.6/sigtime)->paseo

plot(paseo$time,paseo$velocidad)
hist(paseo$velocidad)

# Filtrar y representar puntos de la traza quitando ruido (velocidad supera 4 km/h)

paseo %>%
  select(track_seg_point_id,ele,time,geometry, velocidad) %>%
  filter ( ! is.na(velocidad) & as.double(velocidad) < 4) -> paseo_filtrado

plot(paseo_filtrado$time,paseo_filtrado$velocidad)
hist(paseo_filtrado$velocidad)

paseo.trc <- paseo_filtrado %>%
  st_combine() %>%
  st_cast(to = "LINESTRING") %>%
  st_sf()

leaflet()  %>% 
  addTiles() %>%
  setMaxBounds(as.double(st_bbox(bocaleones)[1]),as.double(st_bbox(bocaleones)[2]),as.double(st_bbox(bocaleones)[3]),as.double(st_bbox(bocaleones)[4])) %>%
  addPolylines(data = paseo.trc)

# Se representa la ruta en función de su velocidad
pal <- colorNumeric(
  palette = c("red", "green", "blue"),
  domain = as.double(paseo_filtrado$velocidad))

leaflet()  %>% 
  addTiles() %>%
  setMaxBounds(as.double(st_bbox(paseo_filtrado)[1]),as.double(st_bbox(paseo_filtrado)[2]),as.double(st_bbox(paseo_filtrado)[3]),as.double(st_bbox(paseo_filtrado)[4])) %>%
  addCircleMarkers(data = paseo_filtrado, radius = 3, color = pal(as.double(paseo_filtrado$velocidad)), stroke=TRUE, fillOpacity = 1)

## Grabamos el paseo eliminando el ruido.

paseo %>%
  filter ( ! is.na(velocidad) & as.double(velocidad) < 4) %>%
  select(-lagtime,-sigtime,-siggeom,-distancia,-velocidad)-> paseo_grabar


st_write(paseo_grabar, 
         dsn="./Data/Bocaleones_filtrado.gpx", 
         layer="track_points", 
         driver = "GPX")