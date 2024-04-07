################################################################################
# Author: Josh Vertalka
# Date: 3-16-2024
# Purpose: Lab 10 of Geography 426
# This lab's focus is on 3D elevation Mapping.  
################################################################################

#Question
#1. Describe some of the functions that make a 3D map more visually appealing ie shadows, lighting, etc. 
# - 3D maps usually look good with lighting from the northwest (compass direction
# 315 degrees.) That will cast shadows on areas of the map for a better visual
# effect. It is also important to make sure that color is accurate, especially
# if using remotely sensed data. The stacking and rescaling seen in the code
# below are tools we can use to adjust color. 

options(repos = c(
  tylermorganwall = 'https://tylermorganwall.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'
))

install.packages('rayshader') #Take a break - This will take a minute to run. 


options(rgl.useNULL = FALSE)
library(ggplot2)
library(whitebox)
library(rayshader)
library(rayrender)
library(raster)
library(spatstat)
library(spatstat.utils)
library(suncalc)
library(sp)
library(lubridate)
library(rgdal)
library(magick)
################################################################################
################################################################################
################################################################################

#Put your code here
#_bZPzS4dqi@kuHXzYHlKVLhOQi!44kKlv@W9ujD40Gl5_T8rb1nRaUSSRfNnTxyI

gc_elevation = raster::raster("N36W113.hgt")
#gc = raster::raster("Grand_Canyon.TIF")

gc_r = raster::raster("LC09_L1TP_038035_20240327_20240328_02_T1_B4.TIF")
gc_g = raster::raster("LC09_L1TP_038035_20240327_20240328_02_T1_B3.TIF")
gc_b = raster::raster("LC09_L1TP_038035_20240327_20240328_02_T1_B2.TIF")

gc = raster::stack(gc_r, gc_g, gc_b)
raster::plotRGB(gc, scale=255^2)

gc = sqrt(raster::stack(gc_r, gc_g, gc_b))
raster::plotRGB(gc)

height_shade(raster_to_matrix(gc_elevation)) %>%
  plot_map()

gc_elevation_utm = raster::projectRaster(gc_elevation, crs = crs(gc), method = "bilinear")

bottom_left = c(y=-112.962270, x=36.124012)
top_right   = c(y=-112.348409, x=36.479277)

extent_latlong = sp::SpatialPoints(rbind(bottom_left, top_right), proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
extent_utm = sp::spTransform(extent_latlong, raster::crs(gc_elevation_utm))

e = raster::extent(extent_utm)
e

gc_cropped = raster::crop(gc, e)
elevation_cropped = raster::crop(gc_elevation_utm, e)

names(gc_cropped) = c("r","g","b")

gc_r_cropped = rayshader::raster_to_matrix(gc_cropped$r)
gc_g_cropped = rayshader::raster_to_matrix(gc_cropped$g)
gc_b_cropped = rayshader::raster_to_matrix(gc_cropped$b)

gcel_matrix = rayshader::raster_to_matrix(elevation_cropped)

gc_array = array(0,dim=c(nrow(gc_r_cropped),ncol(gc_r_cropped),3))

gc_array[,,1] = gc_r_cropped/255 #Red layer
gc_array[,,2] = gc_g_cropped/255 #Blue layer
gc_array[,,3] = gc_b_cropped/255 #Green layer

gc_array = aperm(gc_array, c(2,1,3))

plot_map(gc_array)

gc_contrast = scales::rescale(gc_array,to=c(0,1))
plot_map(gc_contrast)


plot_3d(gc_contrast, gcel_matrix, windowsize = c(1100,900), zscale = 15, shadowdepth = -50,
        zoom=0.5, phi=45,theta=-45,fov=70, background = "#F2E1D0", shadowcolor = "#523E2B")
text3d(x = 4011593, y = 348164, 
       text = "Supai")
render_snapshot(title_text = "Grand Canyon, Arizona | Imagery: Landsat 8 | DEM: 30m SRTM",
                title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)

