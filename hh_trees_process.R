# load pkgs
require(sf)
require(tidyverse)

# load data
setwd("Strassenbaumkataster_HH_2017-04-05_GML")
tree_df = sf::st_read("strassenbaeume_online_2017.gml")
g = as(tree_df, "Spatial")

# filter
tree_df = select(tree_df, GATTUNG_DEUTSCH, geometry)
tree_test = unlunclass(st_cast(tree_df$geometry, "POINT"))
tree_test1 = as.data.frame(unlist(tree_test))
