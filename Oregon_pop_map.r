library(sf)
library(tigris)
library(tidyverse)
library(stars)
library(rayshader)
library(MetBrewer)
library(colorspace)
library(dplyr)
library(rayrender)

# load geopackage data

data <- st_read("C:/Users/kanem/Documents/Pop_Mapping/data/kontur_population_US_20220630.gpkg")

# load states 

st <- states()

# filter for Oregon

oregon <- st %>% 
  filter(NAME == "Oregon") %>% 
  st_transform(crs = st_crs(data))

#plot test

oregon %>% 
  ggplot()+
  geom_sf()

#filter and limit data for points covered in OR

st_oregon <- st_intersection(data,oregon)

#create aspect ratio with help from bb

bb <- st_bbox(st_oregon)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) %>% 
  st_sfc(crs = st_crs(data))

bottom_right <- st_point(c(bb[["xmax"]],bb[["ymin"]])) %>% 
  st_sfc(crs = st_crs(data))
#Testing Plot
oregon %>% 
  ggplot()+
  geom_sf()+
  geom_sf(data = bottom_left)+
  geom_sf(data = bottom_right, color = "red" )

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) %>% 
  st_sfc(crs = st_crs(data))
height <- st_distance(bottom_left, top_left)

if (width > height) {
  w_ratio <- 1
  h_ratio <- height/width
} else {
  h_ratio <- 1
  w_ratio <-width/height
}
#convert to raster then matrix
size = 1000
oregon_rast <- st_rasterize(st_oregon,
                            nx = floor(size * w_ratio),
                            ny = floor(size * h_ratio)) 
mat <- matrix(oregon_rast$population,
              nrow = floor(size * w_ratio),
              ncol = floor(size * h_ratio))
# Color Palette

hg <- met.brewer("Hiroshige")
swatchplot(hg)

texture <- grDevices::colorRampPalette(hg, bias = 1)(256)
swatchplot(texture)
#Plot 3d render
rgl::rgl.close()

mat %>% 
  height_shade(texture = texture) %>% 
  plot_3d(heightmap = mat,
            zscale = 25,
            solid = FALSE,
            shadowdepth = 0)

render_camera(theta = -20, phi = 30, zoom = .8)

render_highquality(
    filename = "images/testplot.png"
)

