# load pkgs
require(sf)
require(tidyverse)

# load data
tree_df = sf::st_read("strassenbaeume_online_2017.gml")

# transform from utm to lat long
tree_df <-st_transform(x = tree_df, crs = 4326)
tree_df$long <-st_coordinates(tree_df$geometry)[,1]
tree_df$lat <-st_coordinates(tree_df$geometry)[,2]

# select cols and format
tree_df = select(tree_df, GATTUNG_DEUTSCH, PFLANZJAHR, STADTTEIL, BEZIRK, long, lat)
tree_df$geometry = NULL
tree_df = as.data.frame(tree_df)

# plot
ggplot() + 
  geom_point(data = tree_df, aes(x = long, y = lat), size = 0.01)

# next up: animation
https://www.r-graph-gallery.com/a-smooth-transition-between-chloropleth-and-cartogram/
