# load pkgs
require(sf)
require(tidyverse)
require(rgdal)
require(gganimate)
require(animation)
require(magick)

# load data
setwd("C:/Users/akruse/Documents/Strassenbaumkataster_HH_2017-04-05_GML")
tree_df = sf::st_read("strassenbaeume_online_2017.gml")

# get hh shape
setwd("C:/Users/akruse/Downloads/HH_ALKIS_Ortsteile")
hh_shape = readOGR(".","HH_ALKIS_Bezirke")
hh_shape <- fortify(hh_shape)

# get hh water shapes
setwd("C:/Users/akruse/Downloads/hamburg-latest-free.shp")
hh_water_shape = readOGR(".","gis.osm_water_a_free_1")
hh_water_shape@data$areasize =  rgeos::gArea(hh_water_shape, byid = T)
hh_water_shape <- hh_water_shape[hh_water_shape@data$areasize > mean(hh_water_shape@data$areasize),] 

# transform to lat long
tree_df <-st_transform(x = tree_df, crs = 4326)
tree_df$long <-st_coordinates(tree_df$geometry)[,1]
tree_df$lat <-st_coordinates(tree_df$geometry)[,2]

# select
tree_df = select(tree_df, OBJECTID, GATTUNG_DEUTSCH, PFLANZJAHR, STADTTEIL, BEZIRK, long, lat)
tree_df$geometry = NULL
tree_df = as.data.frame(tree_df)

# filter and sort
tree_df = filter(tree_df, PFLANZJAHR > 2006)

# replace rare factor values
lf <- names(which(table(tree_df$GATTUNG_DEUTSCH) < 10000))
levels(tree_df$GATTUNG_DEUTSCH)[levels(tree_df$GATTUNG_DEUTSCH) %in% lf] <- "Sonstige"

# create df with incremental states
datalist = list()
for(i in unique(tree_df$PFLANZJAHR)){
  
  mydata = filter(tree_df, PFLANZJAHR <= i)
  mydata$PFLANZJAHR = i
  datalist[[i]] = mydata
  
}
tree_df = do.call(rbind, datalist)
rm(mydata, datalist, i)

# create map plot
p = ggplot() + 
  geom_polygon(data = hh_shape, 
               aes(x = long, y = lat, group = group),
               color = 'white', fill = 'grey', size = .2) +
  geom_polygon(data = hh_water_shape, 
               aes(x = long, y = lat, group = group),
               color = NA, fill = 'blue', size = .2) +
  geom_point(data = tree_df, aes(x = long, y = lat, frame = PFLANZJAHR, color = GATTUNG_DEUTSCH), size = 0.01) +
  scale_x_continuous(limits = c(9.7,10.4)) + 
  scale_y_continuous(limits = c(53.35,53.75)) +
  theme_void() +
  ggtitle("Jahr: ") + 
  theme(legend.position="none")

# create bar plot
tree_df_grouped = tree_df %>% group_by(GATTUNG_DEUTSCH, PFLANZJAHR, BEZIRK) %>% summarise(value = n())

p=ggplot(tree_df_grouped, aes(x=GATTUNG_DEUTSCH, y=value, fill=BEZIRK, frame= PFLANZJAHR)) + 
  geom_bar(stat='identity', position = "identity") +
  ggtitle("Jahr: ")

# Make the animation
animation::ani.options(ani.width = 1000, ani.height = 800, ani.res = 300)
gganimate(p, interval = 1/9)

# links:
https://www.r-graph-gallery.com/a-smooth-transition-between-chloropleth-and-cartogram/
https://community.rstudio.com/t/tweenr-gganimate-with-line-plot/4027/5
