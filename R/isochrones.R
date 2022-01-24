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

# Creación de isochrones para peatones
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

# Creación de isochrones para autos
graph_complete_weighted <- weight_streetnet(graph_complete, wt_profile = "motorcar")

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
  tm_shape(directed_graph_motorcar) +
  tm_lines(scale = 0.5, col = "flow") +
  tm_shape(kiosko_sf) +
  tm_dots(col = "blue") +
  tm_shape(polys_tlim) +
  tm_polygons(col = "tlim", alpha = 0.3)