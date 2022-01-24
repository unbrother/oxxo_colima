
library(sfnetworks)
library(tidygraph)

iso = directed_graph_motorcar %>%
  tidygraph::filter(tidygraph::node_distance_from(tidygraph::st_nearest_feature(kiosko_sf[1], directed_graph_motorcar), weights = time) <= 600)

iso_poly = iso %>%
  st_geometry() %>%
  st_combine() %>%
  st_convex_hull()

plot(net, col = "grey")
plot(iso_poly, col = NA, border = "black", lwd = 3, add = TRUE)
plot(iso, col = colors[2], add = TRUE)
plot(p, col = colors[1], pch = 8, cex = 2, lwd = 2, add = TRUE)



net = as_sfnetwork(osm_sc) %>%
  st_transform(4486) %>%
  activate("edges") %>%
  mutate(weight = edge_length())

net

types = net %>%
  activate("edges") %>%
  pull(highway) %>%
  unique()
types

time = net %>%
  activate("edges") %>%
  pull(time) %>%
  unique()
time


net = activate(net, "nodes")

p = net %>%
  st_geometry() %>%
  st_combine() %>%
  st_centroid()


st_crs(net)
kiosko_sf_tr <- st_transform(kiosko_sf, st_crs(net))
st_crs(kiosko_sf_tr)

iso_list <- list()
iso_poly_list <- list()
iso_df <- data.frame()

iso <- net %>%
  filter(node_distance_from(st_nearest_feature(kiosko_sf_tr[i,], net), weights = time) <= 120)

append(iso$iso, iso$iso)

iso[[length(iso)+10]] <- iso

iso <- net %>%
  filter(node_distance_from(st_nearest_feature(kiosko_sf_tr[1,], net), weights = time) <= 120)

for (i in 2:nrow(kiosko_sf_tr)) {
  
  iso_2 <- net %>%
    filter(node_distance_from(st_nearest_feature(kiosko_sf_tr[5,], net), weights = time) <= 120)
  
  
  iso[[5*10]] <- iso_2
  

  
#  iso_poly <- iso %>%
#    st_geometry() %>%
#    st_combine() %>%
#    st_convex_hull()
#  
#  iso_poly_list[[i]] <- iso_poly
#  
#  polys <- iso_poly_list[[1]][[1]] %>% 
#    st_geometry() %>% 
#    st_combine() %>% 
#    st_convex_hull()
#  
#  iso_df <- rbind(iso_df, polys)
  
}

x <- upgrade_graph(iso)

iso %>% 
  st_geometry() %>% 
  st_combine() %>% 
  st_convex_hull()

qtm(upgrade_graph(iso) %>% 
      st_geometry() %>% 
      st_combine() %>% 
      st_convex_hull())



iso_5 = net %>%
  filter(node_distance_from(st_nearest_feature(kiosko_sf_tr, net), weights = time) <= 300)

iso_2 = net %>%
  filter(node_distance_from(st_nearest_feature(kiosko_sf_tr, net), weights = time) <= 120)

iso_poly_5 = iso_5 %>%
  st_geometry() %>%
  st_combine() %>%
  st_convex_hull()

iso_poly_2 = iso_2 %>%
  st_geometry() %>%
  st_combine() %>%
  st_convex_hull()




iso <- list()
iso_poly <- list() 
iso_list <- list()
i <- 1

iso <- net %>%
  filter(node_distance_from(st_nearest_feature(kiosko_sf_tr[i,], net), weights = time) <= 120)

iso_2 <- net %>%
  filter(node_distance_from(st_nearest_feature(kiosko_sf_tr[2,], net), weights = time) <= 120)

iso2 <- rlist::list.append(iso, iso_2)

class(iso)
lista[[1]] <- iso

l[[i]] <- new_element
i <- i + 1

iso_list[[i]] <- iso

iso_poly[[i]] <- iso %>%
  st_geometry() %>%
  st_combine()

unlist(iso_poly)

x1 <- iso_poly[[1]] %>% 
  st_geometry() %>% 
  st_combine() %>% 
  st_convex_hull()

x2 <- iso_poly %>% 
  st_geometry() %>% 
  st_combine() %>% 
  st_convex_hull()



x <- iso_poly[[1]] <- iso_poly

tm_shape(iso_poly_120) +
  tm_polygons(col = "blue", alpha = 0.2) +
  tm_shape(kiosko_sf) +
  tm_dots()


library(osrm)
options(osrm.server = "http://0.0.0.0:5000/", osrm.profile = "driving")

fx_base <- function(i, pts) {
  message(paste("Running location", i))
  tryCatch({
    return(st_set_geometry(pts[i,],
                           osrm::osrmIsochrone(
                             loc = pts[i,],
                             breaks = c(30),
                             res = 60,
                             returnclass =  "sf") %>% 
                             filter(max == 30) %>% 
                             st_geometry()))
  }, error = function(e) {
    message(paste("Failure at location", i, ":", e))
    print(pts[i, ])
    return()
  })
}

library(tictoc)
tic()
suppressMessages({
  CA_isochrones_base <- map(1:1, ~fx_base(., kiosko_sf))
  CA_isochrones_base <- data.table::rbindlist(CA_isochrones_base) %>% st_as_sf
})

isochrones_base_time <- toc()

osrm::osrmIsochrone(
  loc = kiosko_sf[1],
  breaks = c(30),
  res = 60,
  returnclass =  "sf") %>% 
  filter(max == 30) %>% 
  st_geometry()






library(stplanr)
library(rgdal)

data_dir <- system.file("extdata", package = "stplanr")
unzip(file.path(data_dir, "smallsa1.zip"), exdir = tempdir())
unzip(file.path(data_dir, "testcycleway.zip"), exdir = tempdir())
unzip(file.path(data_dir, "sydroads.zip"), exdir = tempdir())
sa1income <- readOGR(tempdir(), "smallsa1")
testcycleway <- readOGR(tempdir(), "testcycleway")
sydroads <- readOGR(tempdir(), "roads")
class(sydroads)
qtm(sa1income)
sydnetwork <- SpatialLinesNetwork(sydroads)
tmap_options(check.and.fix = TRUE)
qtm(sydroads)
qtm(calc_network_catchment(
  sln = sydnetwork,
  polygonlayer = sa1income,
  targetlayer = testcycleway,
  calccols = c("Total"),
  maximpedance = 800,
  distance = 200,
  projection = "austalbers",
  dissolve = FALSE
))
