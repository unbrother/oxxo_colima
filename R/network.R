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

# Extraer lineas
osm <- osm_raw$osm_lines

# Extraer enlaces relevantes
osm <- osm[osm$highway %in% c("living_street", "primary", "primary_link",
                              "residential", "secondary", "secondary_link",
                              "tertiary", "tertiary_link", "unclassified"),]

# Generar mapa de flujos --------------------------------------------------
raph <- weight_streetnet(osm, wt_profile = "motorcar")
nodes <- dodgr_vertices(graph)
pts <- sample(nodes$id, size = 1000)
d <- dodgr_dists(graph, from = pts, to = pts)

fmat <- matrix(runif(100*100), nrow = 100)
f <- dodgr_flows_aggregate(graph, from = pts, to = pts, flows = fmat,
                           contract = TRUE)
dodgr_flowmap(merge_directed_graph(f), linescale = 5)

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

