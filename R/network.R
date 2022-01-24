##### Ubicación de tiendas Oxxo en Colima ###
# Cargar librerías e inicializar ------------------------------------------
library(sf)
library(tmap)
library(osmdata)
library(tidyverse)
library(dodgr)

# Modo de mapeo
tmap_mode("plot")

# Inicializar variables
rm(list = ls())

# Descargar datos de caminos ----------------------------------------------
# Descarga de datos de OSM con límites en colima (si se descarga por nombre
# del lugar toma un área más amplia) 
q = opq(bbox = c(-103.78,19.2, -103.65, 19.31 )) %>% 
  add_osm_feature(key = "highway") 

# Convertir en objeto sf
osm_raw <- osmdata_sf(q = q)
osm_sc <- osmdata_sc(q = q)

# Extraer lineas
osm <- osm_raw$osm_lines

# Extraer enlaces relevantes
osm <- osm[osm$highway %in% c("living_street", "primary", "primary_link",
                              "residential", "secondary", "secondary_link",
                              "tertiary", "tertiary_link", "unclassified"),]


# Generar mapa de flujos --------------------------------------------------
# Mapa de flujos de autos
graph_motorcar <- weight_streetnet(osm, wt_profile = "motorcar")
nodes <- dodgr_vertices(graph_motorcar)
pts <- sample(nodes$id, size = 1000)
d <- dodgr_dists(graph_motorcar, from = pts, to = pts)


fmat <- matrix(runif(100*100), nrow = 100)
f <- dodgr_flows_aggregate(graph_motorcar, from = pts, to = pts, flows = fmat,
                           contract = TRUE)
# dodgr_flowmap(merge_directed_graph(f), linescale = 5)

directed_graph_motorcar <- dodgr_to_sf(merge_directed_graph(f))


st_write(st_transform(directed_graph_motorcar, 4486), "output/shp/motorcar.shp")

tm_shape(directed_graph_motorcar) +
  tm_lines(lwd = "flow", scale = 5, col = "flow")

# Mapa de flujos de peatones
graph_foot <- weight_streetnet(osm, wt_profile = "foot")
nodes <- dodgr_vertices(graph_foot)
pts <- sample(nodes$id, size = 1000)
d <- dodgr_dists(graph_foot, from = pts, to = pts)

fmat <- matrix(runif(100*100), nrow = 100)
f <- dodgr_flows_aggregate(graph_foot, from = pts, to = pts, flows = fmat,
                           contract = TRUE)
dodgr_flowmap(merge_directed_graph(f), linescale = 5)
directed_graph_foot <- dodgr_to_sf(merge_directed_graph(f))

tm_shape(directed_graph_foot) +
  tm_lines(lwd = "flow", scale = 5, col = "flow")

# Mapa de flujos de camones de reparto
graph_goods <- weight_streetnet(osm, wt_profile = "goods")
nodes <- dodgr_vertices(graph_goods)
pts <- sample(nodes$id, size = 1000)
d <- dodgr_dists(graph_goods, from = pts, to = pts)

fmat <- matrix(runif(100*100), nrow = 100)
f <- dodgr_flows_aggregate(graph_goods, from = pts, to = pts, flows = fmat,
                           contract = TRUE)
dodgr_flowmap(merge_directed_graph(f), linescale = 5)
directed_graph_goods <- dodgr_to_sf(merge_directed_graph(f))

tm_shape(directed_graph_goods) +
  tm_lines(lwd = "flow", scale = 5, col = "flow")

# Generar mapa de centralidad de enlaces ----------------------------------
graph_full <- weight_streetnet(osm, wt_profile = "motorcar")
graph <- dodgr_contract_graph (graph_full)
graph <- dodgr_centrality (graph)

graph_full <- dodgr_uncontract_graph (graph)

graph_sf <- merge_directed_graph (graph_full) %>%
  dodgr_to_sf()

graph_sf <- graph_sf %>% 
  mutate(central_norm = centrality/max(centrality))

tm_shape(graph_sf) +
  tm_lines(lwd = "central_norm")

st_buffer(kiosko_sf, 0.2)

# Datos de servicios actuales ---------------------------------------------

kiosko_sf <- st_read("data/shp/INEGI_DENUE_kiosko.shp", options="ENCODING=latin1")
oxxo_sf <- st_read("data/shp/INEGI_DENUE_oxxo.shp", options="ENCODING=latin1")

st_write(st_transform(kiosko_sf, 4486), "output/shp/kiosko.shp")

tm_shape(directed_graph_motorcar) +
  tm_lines(lwd = "flow", scale = 5, col = "flow") +
  tm_shape(kiosko_sf) +
  tm_dots(col = "blue") +
  tm_shape(oxxo_sf) +
  tm_dots(col = "red") +
  tm_shape(st_buffer(kiosko_sf, 1000)) +
  tm_polygons(col = "blue", alpha = 0.4) +
  tm_shape(st_buffer(oxxo_sf, 1000)) +
  tm_polygons(col = "red", alpha = 0.4)





# Isocrhones --------------------------------------------------------------

# Ampliamos la red

colima <- "colima colima mexico"
graph_complete <- opq(colima) %>% 
  add_osm_feature(key = "highway") %>% 
  osmdata_sc()

graph <- opq(colima) %>% 
  add_osm_feature(key = "highway") %>% 
  osmdata_sf()

graph <- graph$osm_lines
class(graph)

st_crs(graph_complete_weighted)
graph_complete_weighted <- weight_streetnet(graph_complete, wt_profile = "foot")

tlim <- c (2, 5, 10, 15) * 60 

polys_all <- data.frame()
for (i in 1:nrow(kiosko_sf)) {
  
  x <- dodgr_isochrones(graph_complete_weighted, from = sf::st_coordinates(kiosko_sf$geometry)[i,], tlim)
  iso <-  st_as_sf(x, coords=c("x","y"))
  try(polys <- iso %>% 
    dplyr::group_by(from, tlim) %>% 
    dplyr::summarise() %>%
    st_cast("POLYGON") %>% 
    st_convex_hull())
  
  polys_all <- rbind(polys_all, polys)
  
}

polys_tlim <- polys_all %>% 
  group_by(tlim) %>% 
  summarise() %>% 
  mutate(tlim = tlim/60) %>% 
  mutate(tlim = factor(tlim, levels = c("15", "10", "5", "2"))) 

tm_shape(graph, bbox = c(-103.78,19.2, -103.65, 19.31 )) +
  tm_lines(col = "grey") +
    tm_shape(directed_graph_foot) +
      tm_lines(scale = 0.5, col = "flow") +
    tm_shape(kiosko_sf) +
      tm_dots(col = "blue") +
    tm_shape(polys_tlim) +
      tm_polygons(col = "tlim", alpha = 0.3)





