##### Ubicación de tiendas Oxxo en Colima ###
# Load libraries and initialize -------------------------------------------
library(sf)
library(tmap)
library(osmdata)
library(tidyverse)
library(dodgr)

# Map mode
tmap_mode("plot")

# Initialize variables
rm(list = ls())

# Download OSM Data -------------------------------------------------------
# Download data for Colima with bounding box (larger area if downloaded by name
q = opq(bbox = c(-103.78,19.2, -103.65, 19.31 )) %>% 
  add_osm_feature(key = "highway") 

# Convert to sf object
osm_raw <- osmdata_sf(q = q)
osm_sc <- osmdata_sc(q = q)

# Extract lines
osm <- osm_raw$osm_lines

# Extract relevant links
osm <- osm[osm$highway %in% c("living_street", "primary", "primary_link",
                              "residential", "secondary", "secondary_link",
                              "tertiary", "tertiary_link", "unclassified"),]


# Generate flowmap --------------------------------------------------------
# Motorcar flowmap
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

# Foot flowmap
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

# Goods flowmap (same as motorcar apparently)
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


# Generate centrality map -------------------------------------------------
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

# Existing services (shops) -----------------------------------------------

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











