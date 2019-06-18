
# Beatiful Map of Switzerland
# Code copied from Timo Grossenbacher (https://twitter.com/grssnbchr)

require(rgeos)
require(rgdal)
require(raster)
require(ggplot2)
require(viridis)
require(dplyr)
require(gtable)
require(grid)


theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}


data <- read.csv("avg_age_15.csv", stringsAsFactors = F)


### Read in geodata
gde_15 <- readOGR("gde-1-1-15.shp", layer = "gde-1-1-15")

# set crs to ch1903/lv03, just to make sure  (EPSG:21781)
crs(gde_15) <- "+proj=somerc +lat_0=46.95240555555556 
+lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 
+ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs"
# fortify, i.e., make ggplot2-compatible
map_data_fortified <- fortify(gde_15, region = "BFS_ID") %>% 
  mutate(id = as.numeric(id))
# now we join the thematic data
map_data <- map_data_fortified %>% left_join(data, by = c("id" = "bfs_id"))

# read in background relief
relief <- raster("02-relief-georef-clipped-resampled.tif")
relief_spdf <- as(relief, "SpatialPixelsDataFrame")
# relief is converted to a very simple data frame, 
# just as the fortified municipalities.
# for that we need to convert it to a 
# SpatialPixelsDataFrame first, and then extract its contents 
# using as.data.frame
relief <- as.data.frame(relief_spdf) %>% 
  rename(value = `X02.relief.georef.clipped.resampled`)
# remove unnecessary variables
rm(relief_spdf)
rm(gde_15)
rm(map_data_fortified)

# A very basic map
# What follows now is a very basic map with the municipalities rendered with `geom_polygon` and their outline with `geom_path`. 

p <- ggplot() +
  # municipality polygons
  geom_polygon(data = map_data, aes(fill = avg_age_15, 
                                    x = long, 
                                    y = lat, 
                                    group = group)) +
  # municipality outline
  geom_path(data = map_data, aes(x = long, 
                                 y = lat, 
                                 group = group), 
            color = "white", size = 0.1) +
  coord_equal() +
  # add the previously defined basic theme
  theme_map() +
  labs(x = NULL, 
       y = NULL, 
       title = "Switzerland's regional demographics", 
       subtitle = "Average age in Swiss municipalities, 2015", 
       caption = "Geometries: ThemaKart, BFS; Data: BFS, 2016")
p

### A better color scale
q <- p + scale_fill_viridis(option = "magma", direction = -1)
q

### Horizontal legend
q <- p +
  # this is the main part
  theme(legend.position = "bottom") +
  scale_fill_viridis(
    option = "magma", 
    direction = -1,
    name = "Average age",
    # here we use guide_colourbar because it is still a continuous scale
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      # some shifting around
      title.hjust = 0.5,
      label.hjust = 0.5
    ))
q

## Discrete classes with quantile scale
no_classes <- 6
labels <- c()

quantiles <- quantile(map_data$avg_age_15, 
                      probs = seq(0, 1, length.out = no_classes + 1))

# here I define custom labels (the default ones would be ugly)
labels <- c()
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2), 
                             " – ", 
                             round(quantiles[idx + 1], 2)))
}
# I need to remove the last label 
# because that would be something like "66.62 - NA"
labels <- labels[1:length(labels)-1]

# here I actually create a new 
# variable on the dataset with the quantiles
map_data$avg_age_15_quantiles <- cut(map_data$avg_age_15, 
                                     breaks = quantiles, 
                                     labels = labels, 
                                     include.lowest = T)

p <- ggplot() +
  # municipality polygons (watch how I 
  # use the new variable for the fill aesthetic)
  geom_polygon(data = map_data, aes(fill = avg_age_15_quantiles, 
                                    x = long, 
                                    y = lat, 
                                    group = group)) +
  # municipality outline
  geom_path(data = map_data, aes(x = long, 
                                 y = lat, 
                                 group = group), 
            color = "white", size = 0.1) +
  coord_equal() +
  theme_map() +
  labs(x = NULL, 
       y = NULL, 
       title = "Switzerland's regional demographics", 
       subtitle = "Average age in Swiss municipalities, 2015", 
       caption = "Geometries: ThemaKart, BFS; Data: BFS, 2016") +
  # now the discrete-option is used, 
  # and we use guide_legend instead of guide_colourbar
  scale_fill_viridis(
    option = "magma",
    name = "Average age",
    discrete = T,
    direction = -1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      reverse = T
    ))
p

### Discrete classes with pretty breaks

# here I define equally spaced pretty breaks - 
# they will be surrounded by the minimum value at 
# the beginning and the maximum value at the end. 
pretty_breaks <- c(39,40,41,42,43)
minVal <- min(map_data$avg_age_15, na.rm = T)
maxVal <- max(map_data$avg_age_15, na.rm = T)
# compute labels
labels <- c()
brks <- c(minVal, pretty_breaks, maxVal)
# round the labels (actually, only the extremes)
for(idx in 1:length(brks)){
labels <- c(labels,round(brks[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]
# define a new variable on the data set just as above
map_data$brks <- cut(map_data$avg_age_15, 
breaks = brks, 
include.lowest = TRUE, 
labels = labels)

brks_scale <- levels(map_data$brks)
labels_scale <- rev(brks_scale)

p <- ggplot() +
# municipality polygons
geom_polygon(data = map_data, aes(fill = brks, 
x = long, 
y = lat, 
group = group)) +
# municipality outline
geom_path(data = map_data, aes(x = long, 
y = lat, 
group = group), 
color = "white", size = 0.1) +
coord_equal() +
theme_map() +
theme(legend.position = "bottom") +
labs(x = NULL, 
y = NULL, 
title = "Switzerland's regional demographics", 
subtitle = "Average age in Swiss municipalities, 2015", 
caption = "Geometries: ThemaKart, BFS; Data: BFS, 2016")

q <- p +
# now we have to use a manual scale, 
# because only ever one number should be shown per label
scale_fill_manual(
# in manual scales, one has to define colors, well, manually
# I can directly access them using viridis' magma-function
values = rev(magma(6)),
breaks = rev(brks_scale),
name = "Average age",
drop = FALSE,
labels = labels_scale,
guide = guide_legend(
direction = "horizontal",
keyheight = unit(2, units = "mm"),
keywidth = unit(70 / length(labels), units = "mm"),
title.position = 'top',
# I shift the labels around, the should be placed 
# exactly at the right end of each legend key
title.hjust = 0.5,
label.hjust = 1,
nrow = 1,
byrow = T,
# also the guide needs to be reversed
reverse = T,
label.position = "bottom"
)
)

q

### More intuitive legend

extendLegendWithExtremes <- function(p){
p_grob <- ggplotGrob(p)
legend <- gtable_filter(p_grob, "guide-box")
legend_grobs <- legend$grobs[[1]]$grobs[[1]]
# grab the first key of legend
legend_first_key <- gtable_filter(legend_grobs, "key-3-1-1")
legend_first_key$widths <- unit(2, units = "cm")
# modify its width and x properties to make it longer
legend_first_key$grobs[[1]]$width <- unit(2, units = "cm")
legend_first_key$grobs[[1]]$x <- unit(0.15, units = "cm")

# last key of legend
legend_last_key <- gtable_filter(legend_grobs, "key-3-6-1")
legend_last_key$widths <- unit(2, units = "cm")
# analogous
legend_last_key$grobs[[1]]$width <- unit(2, units = "cm")
legend_last_key$grobs[[1]]$x <- unit(1.02, units = "cm")

# grab the last label so we can also shift its position
legend_last_label <- gtable_filter(legend_grobs, "label-5-6")
legend_last_label$grobs[[1]]$x <- unit(2, units = "cm")

# Insert new color legend back into the combined legend
legend_grobs$grobs[legend_grobs$layout$name == "key-3-1-1"][[1]] <- 
legend_first_key$grobs[[1]]
legend_grobs$grobs[legend_grobs$layout$name == "key-3-6-1"][[1]] <- 
legend_last_key$grobs[[1]]
legend_grobs$grobs[legend_grobs$layout$name == "label-5-6"][[1]] <- 
legend_last_label$grobs[[1]]

# finally, I need to create a new label for the minimum value 
new_first_label <- legend_last_label$grobs[[1]]
new_first_label$label <- round(min(map_data$avg_age_15, na.rm = T), 2)
new_first_label$x <- unit(-0.15, units = "cm")
new_first_label$hjust <- 1

legend_grobs <- gtable_add_grob(legend_grobs, 
new_first_label, 
t = 6, 
l = 2, 
name = "label-5-0", 
clip = "off")
legend$grobs[[1]]$grobs[1][[1]] <- legend_grobs
p_grob$grobs[p_grob$layout$name == "guide-box"][[1]] <- legend

# the plot is now drawn using this grid function
grid.newpage()
grid.draw(p_grob)
}
extendLegendWithExtremes(q)

### Better colors for classes

p <- p + scale_fill_manual(
# magma with 8 classes
values = rev(magma(8)[2:7]),
breaks = rev(brks_scale),
name = "Average age",
drop = FALSE,
labels = labels_scale,
guide = guide_legend(
direction = "horizontal",
keyheight = unit(2, units = "mm"),
keywidth = unit(70/length(labels), units = "mm"),
title.position = 'top',
title.hjust = 0.5,
label.hjust = 1,
nrow = 1,
byrow = T,
reverse = T,
label.position = "bottom"
)
)
# reapply the legend modification from above
extendLegendWithExtremes(p)

## Add Relief
# I add the relief with `geom_raster`. Now the problem is that I can't use the `fill` aesthetic because it (or its scale) is already in use by the `geom_polygon` layer. The workaround is using the `alpha` aesthetic which works fine here because the relief should be displayed with a greyscale anyway.

p <- ggplot() +
# raster comes as the first layer, municipalities on top
geom_raster(data = relief, aes(x = x, 
y = y, 
alpha = value)) +
# use the "alpha hack"
scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
# municipality polygons
geom_polygon(data = map_data, aes(fill = brks, 
x = long, 
y = lat, 
group = group)) +
# municipality outline
geom_path(data = map_data, aes(x = long, 
y = lat, 
group = group), 
color = "white", size = 0.1) +
# apart from that, nothing changes
coord_equal() +
theme_map() +
theme(legend.position = "bottom") +
labs(x = NULL, 
y = NULL, 
title = "Switzerland's regional demographics", 
subtitle = "Average age in Swiss municipalities, 2015", 
caption = "Geometries: ThemaKart, BFS; Data: BFS, 2016; Relief: swisstopo, 2016") + 
scale_fill_manual(
values = rev(magma(8)[2:7]),
breaks = rev(brks_scale),
name = "Average age",
drop = FALSE,
labels = labels_scale,
guide = guide_legend(
direction = "horizontal",
keyheight = unit(2, units = "mm"), 
keywidth = unit(70/length(labels), units = "mm"),
title.position = 'top',
title.hjust = 0.5,
label.hjust = 1,
nrow = 1,
byrow = T,
reverse = T,
label.position = "bottom"
)
)
extendLegendWithExtremes(p)

## Final map

p <- ggplot() +
# municipality polygons
geom_raster(data = relief, aes_string(x = "x", 
y = "y", 
alpha = "value")) +
scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
geom_polygon(data = map_data, aes(fill = brks, 
x = long, 
y = lat, 
group = group)) +
# municipality outline
geom_path(data = map_data, aes(x = long, 
y = lat, 
group = group), 
color = "white", size = 0.1) +
coord_equal() +
theme_map() +
theme(
legend.position = c(0.5, 0.03),
legend.text.align = 0,
legend.background = element_rect(fill = alpha('white', 0.0)),
legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", 
margin = margin(b = -0.1, 
t = -0.1, 
l = 2, 
unit = "cm"), 
debug = F),
legend.title = element_text(size = 8),
plot.margin = unit(c(.5,.5,.2,.5), "cm"),
panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
panel.border = element_blank(),
plot.caption = element_text(size = 6, 
hjust = 0.92, 
margin = margin(t = 0.2, 
b = 0, 
unit = "cm"), 
color = "#939184")
) +
labs(x = NULL, 
y = NULL, 
title = "Switzerland's regional demographics", 
subtitle = "Average age in Swiss municipalities, 2015", 
caption = "Map CC-BY-SA; Author: Timo Grossenbacher (@grssnbchr), Geometries: ThemaKart, BFS; Data: BFS, 2016; Relief: swisstopo, 2016") + 
scale_fill_manual(
values = rev(magma(8, alpha = 0.8)[2:7]),
breaks = rev(brks_scale),
name = "Average age",
drop = FALSE,
labels = labels_scale,
guide = guide_legend(
direction = "horizontal",
keyheight = unit(2, units = "mm"),
keywidth = unit(70/length(labels), units = "mm"),
title.position = 'top',
title.hjust = 0.5,
label.hjust = 1,
nrow = 1,
byrow = T,
reverse = T,
label.position = "bottom"
)
)
extendLegendWithExtremes(p)

# same code as above but different breaks
pretty_breaks <- c(40,42,44,46,48)
# find the extremes
minVal <- min(map_data$avg_age_15, na.rm = T)
maxVal <- max(map_data$avg_age_15, na.rm = T)
# compute labels
labels <- c()
brks <- c(minVal, pretty_breaks, maxVal)
# round the labels (actually, only the extremes)
for(idx in 1:length(brks)){
labels <- c(labels,round(brks[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]
# define a new variable on the data set just as above
map_data$brks <- cut(map_data$avg_age_15, 
breaks = brks, 
include.lowest = TRUE, 
labels = labels)

brks_scale <- levels(map_data$brks)
labels_scale <- rev(brks_scale)

p <- ggplot() +
# municipality polygons
geom_raster(data = relief, aes_string(x = "x", 
y = "y", 
alpha = "value")) +
scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
geom_polygon(data = map_data, aes(fill = brks, 
x = long, 
y = lat, 
group = group)) +
# municipality outline
geom_path(data = map_data, aes(x = long, 
y = lat, 
group = group), 
color = "white", size = 0.1) +
coord_equal() +
theme_map() +
theme(
legend.position = c(0.5, 0.03),
legend.text.align = 0,
legend.background = element_rect(fill = alpha('white', 0.0)),
legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", 
                             margin = margin(b = -0.1, 
                                             t = -0.1, 
                                             l = 2, 
                                             unit = "cm"), 
                             debug = F),
legend.title = element_text(size = 8),
plot.margin = unit(c(.5,.5,.2,.5), "cm"),
panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
panel.border = element_blank(),
plot.caption = element_text(size = 6, 
                            hjust = 0.92, 
                            margin = margin(t = 0.2, 
                                            b = 0, 
                                            unit = "cm"), 
                            color = "#939184")
) +
  labs(x = NULL, 
       y = NULL, 
       title = "Switzerland's regional demographics", 
       subtitle = "Average age in Swiss municipalities, 2015", 
       caption = "Map CC-BY-SA; Author: Timo Grossenbacher (@grssnbchr), Geometries: ThemaKart, BFS; Data: BFS, 2016; Relief: swisstopo, 2016") + 
  scale_fill_manual(
    values = rev(magma(8, alpha = 0.8)[2:7]),
    breaks = rev(brks_scale),
    name = "Average age",
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(70/length(labels), units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"
    )
  )
extendLegendWithExtremes(p)

#At this point, it might make sense to look at the histogram of the municipalities:

ggplot(data = data, aes(x = avg_age_15)) + 
geom_histogram(binwidth = 0.5) +
theme_minimal() +
xlab("Average age in Swiss municipality, 2015") +
ylab("Count")
