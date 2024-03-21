#Lab 7 Deliverables

library(sf)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(mapsf)
library(spatstat)
library(gstat)
library(raster)
library(ggplot2)
library(scales)
library(akima)

#Questions

#1.	Describe the differences between Inverse Distance Weighted and Kriging approaches to creating smooth continuous surface.
# - IDW is a relatively simple interpolation method that relies on distances
#between points. Kriging is much more complex and also takes into account
#spatial correlation structures. 

#2.	Describe how contour lines can enhance or retract the quality of a map. 
# - Contours can make it easy for the viewer to interpret elevation of an area,
#but if they are too close together, then the map can be cluttered and hard to
#read.

#Your turn to code

#1.	Please create a continuous smooth surface of one of the two following. Be sure to try different breakpoints, color, etc with you data. 
#a.	A continuous smooth surface of a variable about Vancouver, Canada. Data has been provided in the Lab material. Save an image of it and be sure to turn it in.  
#b.	An elevation map of an area of your choosing. Data has been provided in the code. Save an image of it and be sure to turn it in.

vancouver <- st_read("Vancouver_Cen2021.gpkg")

bbox <- st_bbox(vancouver) #This creates a bounding box around our study area
grd <- st_make_grid(st_as_sfc(bbox), cellsize = 0.0035) # This creates a grid of points. Adjust cellsize for different resolutions
my_sf_grid <- st_sf(geometry = grd)


#find centroids
centroids <- st_centroid(vancouver)



#Inverse Distance Weighted Interpolation
idw_result <- gstat::idw(formula = Population ~ 1, 
                         locations = centroids,        
                         newdata = my_sf_grid,         
                         idp = 2,                     
                         nmax = 30,                   
                         maxdist = 1000)              
idw_sf <- st_as_sf(idw_result, coords = c("x", "y"), crs = 26910)

#plot map 
ggplot(data = idw_sf) +
  geom_sf(aes(fill = var1.pred), color = NA) +
  scale_fill_viridis_c() +
  labs(title = "IDW Interpolation Results", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


