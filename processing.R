# load pkgs
require(sf)
require(tidyverse)
require(rgdal)
require(animation)


# load data
setwd("C:/Users/akruse/Documents/Strassenbaumkataster_HH_2017-04-05_GML")
tree_df = sf::st_read("strassenbaeume_online_2017.gml")

# transform to lat long
tree_df <-st_transform(x = tree_df, crs = 4326)
tree_df$long <-st_coordinates(tree_df$geometry)[,1]
tree_df$lat <-st_coordinates(tree_df$geometry)[,2]

# select
tree_df = dplyr::select(tree_df, OBJECTID, GATTUNG_DEUTSCH, PFLANZJAHR, STADTTEIL, BEZIRK, long, lat)
tree_df$geometry = NULL
tree_df = as.data.frame(tree_df)

# filter out NA
tree_df = tree_df[!is.na(tree_df$PFLANZJAHR),]
tree_df = filter(tree_df, PFLANZJAHR >= 1916)

# loop to create map for every year
g.progress= function(i=min(tree_df$PFLANZJAHR),maxi = max(tree_df$PFLANZJAHR)){

# grouping
trees = tree_df %>% 
  group_by(STADTTEIL, PFLANZJAHR) %>% 
  summarise(count = n()) %>%
  filter(PFLANZJAHR == i)

# read shapefile of Hamburg
setwd("C:/Users/akruse/Downloads/HH_ALKIS_Ortsteile/")
hh_shape = readOGR(".","HH_ALKIS_Stadtteile")

# add mising stadtteile
hood = as.data.frame(hh_shape@data$Stadtteil)
colnames(hood) = "STADTTEIL"
hood = merge(hood, trees, by = "STADTTEIL", all.x = T)
hood$count[is.na(hood$count)] = 0
hood$PFLANZJAHR[is.na(hood$PFLANZJAHR)] = i

# add data to polygon
hh_shape@data = data.frame(hh_shape@data, hood[match(hh_shape@data[,"Stadtteil"], hood[,"STADTTEIL"]),])

# format polygon to dataframe
hh_shape@data$id <- rownames(hh_shape@data)
hh_shape.df     <- ggplot2::fortify(hh_shape)
hh_shape.df     <- plyr::join(hh_shape.df,hh_shape@data, by="id")
hh_shape.df = as.data.frame(hh_shape.df)
hh_shape.df$`Planted Trees` = hh_shape.df$count

# remove islands outside from Hamburg from shapefile
hh_shape.df = filter(hh_shape.df, long > 9.7 & long < 10.4 & lat > 53.35 & lat < 53.75)
hh_shape.df = filter(hh_shape.df, !is.na(PFLANZJAHR))

print(max(hh_shape.df$count))

# plot sensors on map (one map for every month)
ggplot() +
  geom_polygon(data = hh_shape.df, aes(x = long, y = lat, group = group, fill = `Planted Trees`), color = "white", size = 0.2) +
  theme_void() +
  theme(legend.position = "bottom") +
  scale_fill_distiller(palette = "Greens", direction = 1, limits=c(0,600), guide = guide_colorbar(
    direction = "horizontal",
    barheight = unit(2, units = "mm"),
    barwidth = unit(50, units = "mm"),
    draw.ulim = F,
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = 0.5)) +
  theme(text=element_text(color =  "#4e4d47", family = "Tw Cen MT")) +
  #theme(plot.background=element_rect(fill="grey90")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) +
  labs(title = paste0("HAMBURG PLANTED STREET TREES ",hh_shape.df$PFLANZJAHR[1]), subtitle = "Baumkataster ~ Citizen Science ~ Transparenzportal") +
  theme(plot.subtitle = element_text(color = "#4e4d47", hjust = 0.5)) +
  labs(caption = "The data is available at transparenz.hamburg.de. The map is created on the 13.04.2018.") +
  theme(plot.caption = element_text(hjust=0.5, size = 9, color = "#4e4d47")) +
  theme(plot.margin=unit(c(.5,.8,.2,.8),"cm"))

}

# function for saving images
plot.save = function(i=1){
  file_path = paste0("C:/Users/akruse/Documents/gif/streettree", "/plot_",i ,".png")
  ggsave(filename=file_path, g.progress(i))
  
}

# save plot
map(1916:2016, plot.save)
  
