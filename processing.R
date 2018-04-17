# load pkgs
require(sf)
require(tidyverse)
require(rgdal)
require(animation)
require(viridis)
require(cowplot)
require(extrafont)

# load more fonts for plotting (via extrafont package)
loadfonts(device = "win")

# load data
setwd("C:/Users/akruse/Documents/Strassenbaumkataster_HH_2017-04-05_GML")
tree_df = sf::st_read("strassenbaeume_online_2017.gml")

# transform to lat long
tree_df =st_transform(x = tree_df, crs = 4326)
tree_df$long =st_coordinates(tree_df$geometry)[,1]
tree_df$lat =st_coordinates(tree_df$geometry)[,2]

# select coloumns
tree_df = dplyr::select(tree_df, OBJECTID, GATTUNG_DEUTSCH, PFLANZJAHR, STADTTEIL, BEZIRK, long, lat)
tree_df$geometry = NULL
tree_df = as.data.frame(tree_df)

# filter out NAs and subset data
tree_df = tree_df[!is.na(tree_df$PFLANZJAHR),]
tree_df = filter(tree_df, PFLANZJAHR >= 1920)

# bin years into decades
tree_df$decade = cut(tree_df$PFLANZJAHR, breaks=c(1920,1930,1940,1950,1960,1970,1980,1990,2000,2010,Inf), 
                     labels=c("1920-29","1930-39","1940-49","1950-59","1960-69","1970-79","1980-89","1990-99","2000-09","2010-16"), 
                     include.lowest=TRUE, right=FALSE)
tree_df$decade_num = as.numeric(tree_df$decade)

# loop to create map for every decade
g.progress= function(i=min(tree_df$decade_num),maxi = max(tree_df$decade_num)){

# group data by decade
trees = tree_df %>% 
  group_by(STADTTEIL, decade, decade_num) %>% 
  summarise(count = n()) %>%
  filter(decade_num == i)

# read shapefile of Hamburg
setwd("C:/Users/akruse/Downloads/HH_ALKIS_Ortsteile/")
hh_shape = readOGR(".","HH_ALKIS_Stadtteile")

# add mising neighbourhoodds to data
hood = as.data.frame(hh_shape@data$Stadtteil)
colnames(hood) = "STADTTEIL"
hood = merge(hood, trees, by = "STADTTEIL", all.x = T)
hood$count[is.na(hood$count)] = 0
hood$decade_num[is.na(hood$decade_num)] = i

# add data to polygon
hh_shape@data = data.frame(hh_shape@data, hood[match(hh_shape@data[,"Stadtteil"], hood[,"STADTTEIL"]),])

# format polygon to dataframe
hh_shape@data$id = rownames(hh_shape@data)
hh_shape.df = ggplot2::fortify(hh_shape)
hh_shape.df = plyr::join(hh_shape.df,hh_shape@data, by="id")
hh_shape.df = as.data.frame(hh_shape.df)

# remove islands outside from Hamburg from shapefile
hh_shape.df = filter(hh_shape.df, long > 9.7 & long < 10.4 & lat > 53.35 & lat < 53.75)

# filter out data without decade
hh_shape.df = filter(hh_shape.df, !is.na(decade_num))

# checkpoint for loop
print(max(hh_shape.df$count))

# plot map
ggplot() +
  geom_polygon(data = hh_shape.df, aes(x = long, y = lat, group = group, fill = count), color = NA, size = .1) +
  scale_fill_viridis(
    limits=c(0,2500),
    option = "magma",
    name = "Planted Trees by Neighbourhood",
    begin = .25, end = .75,
    direction = -1,
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 0.5)) +
  theme(text = element_text(family = "Tw Cen MT"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5, size=13, family = "Tw Cen MT", color = "#4e4d47"),
        legend.position = "top",
        legend.justification = "center",
        legend.text=element_text(size=8),
        legend.title=element_text(size=8),
        plot.subtitle = element_text(color = "#4e4d47", hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#f5f5f2", color = NA), 
        panel.background = element_rect(fill = "#f5f5f2", color = NA), 
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.border = element_blank()) +
  coord_fixed(ratio = 1.5/1) +
  labs(title = paste0("Ten Decades of Street Tree Planting in Hamburg: ",hh_shape.df$decade[1]), subtitle = "")
}

# group data for barplot
tree_df_gp = tree_df %>% group_by(decade) %>% summarise(count = n())

# add sequence for plot function
tree_df_gp$month.num = seq(1:nrow(tree_df_gp))

# loop for barplot
p.progress= function(i=1,maxi = max(tree_df_gp$month.num)){
  data = tree_df_gp
  data$count[data$month.num > i] = 0
  data$colori[data$month.num == i] = "zero"
  data$colori[data$month.num != i] = "nonzero"
  ggplot(data, aes(x = decade, y = count, fill = colori)) +
    geom_bar(stat="identity") +
    scale_y_continuous(limits = c(0, 50000)) +
    theme(text = element_text(family = "Tw Cen MT", colour = "#4e4d47"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          plot.caption = element_text(hjust=0.5, size = 8, color = "#939184"),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "#f5f5f2", color = NA), 
          panel.background = element_rect(fill = "#f5f5f2", color = NA), 
          legend.background = element_rect(fill = "#f5f5f2", color = NA),
          panel.border = element_blank(),
          axis.line.x = element_line(color="#4e4d47", size = .5),
          axis.line.y = element_line(color="#4e4d47", size = .5),
          axis.text.x = element_text(color = "#4e4d47", size = 8, angle = 90),
          axis.text.y = element_text(color = "#4e4d47", size = 8)) +
    xlab("") + 
    theme(plot.margin=unit(c(0,1.25,0,0.25),"cm")) +
    scale_fill_manual(values=c("#939184", "#4e4d47")) +
    labs(caption = "source: transparenz.hamburg.de") +
    theme(aspect.ratio=0.28)
}

# use plot_grid to combine map and barplot
plotf = function(i=1){plot_grid(g.progress(i),p.progress(i), rel_heights=c(6,2),ncol=1)}

# function for saving images
plot.save = function(i=1){
  file_path = paste0("C:/Users/akruse/Documents/gif/streettree", "/plot_",i ,".png")
  ggsave(filename=file_path, plotf(i), dpi = 600, width = 4.75, height = 7)
  
}

# save images
map(1:10, plot.save)
