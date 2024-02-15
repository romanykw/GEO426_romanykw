#Packages from demo
library(sf)            # classes and functions for vector data
library(terra)         # classes and functions for raster data
library(spData)        # load geographic data
library(spDataLarge)   # load larger geographic data
library(sfheaders)     # helps create spatial data much faster - not necessary for this course but you might need it in the future
library(dplyr)         # several useful and intuitive functions
library(ggplot2)       # useful for visualizations
library(deldir)        # Delaunay triangulation and the Dirichlet or Voronoi tessellation
library(classInt)      # For creating class intervals for map legends
library(RColorBrewer)  # Provides color schemes for maps and graphics
library(cartography)   # Tools for creating thematic maps

data <- read.csv("MI_Mariage_rates_2022.csv")
#Questions 

#1. What is the difference between the sf library and the terra library 
# -sf is used for vector analysis and terra is used for raster analysis.

#2. There are some common spatial data manipulation functions (GIS shapefile manipulation functions) in the sf library. What are they and what do they do? 
# - sf can be used co create and manipulate dataframes. geom_sf is used to 
# set up points, lines, and polygons within ggplot.

#3. How would you create a new sf object representing a line or a polygon, given a set of coordinates? 
# - st_point, st_line, st_polygon and then the list of coordinates.

#4. How do you filter spatial data based on a specific attribute, such as area or population? 
# - Subset it with a dollar sign, e.g. data$area or data$population.  

#5. Suppose you have a dataset of weather stations and temperature readings. How would you visually represent areas of high and low temperatures on a map using R? 
# I would create a graduated symbols map using ggplot.

#Your turn to code 

#1. Create your own mixed geometry type with point, line, and polygon geometry and apply a buffer 
point_a <- st_point(c(5, 2))
multilinestring_a <- st_multilinestring(list(matrix(c(1,2, 2,3, 4,1), ncol = 2, byrow = TRUE))) 
polygon_a <- st_polygon(list(matrix(c(0,0, 3,0, 3,3, 0,3, 0,0), ncol = 2, byrow = TRUE)))

plot(point_a)
plot(multilinestring_a)
plot(polygon_a)
#2. Create two polygons and perform an union that combines both of them 
polygon_b <- st_polygon(list(matrix(c(0,0, 2,0, 2,2, 0,2, 0,0), ncol = 2, byrow = TRUE)))
polygon_c <- st_polygon(list(matrix(c(2,0, 4,0, 4,2, 2,2, 2,0), ncol = 2, byrow = TRUE)))

plot(polygon_b)
plot(polygon_c)
#3. Create a multi-layer raster object and produce a jpg output (jpg output is not in the R code – you’ll have to google/Chat GPT how to use it) 
# Load required package
install.packages("raster")
library(raster)

# Create raster layers
raster1 <- raster(matrix(1:100, nrow = 10, ncol = 10))
raster2 <- raster(matrix(101:200, nrow = 10, ncol = 10))

# Combine raster layers into a multi-layer raster object
multi_layer_raster <- stack(raster1, raster2)

# Plot the multi-layer raster
plot(multi_layer_raster)

# Export as JPG
jpeg("output.jpg", quality = 100)
plot(multi_layer_raster)
dev.off()

#4. The code provides a couple of ways to represent trees as a geographic phenomena. Please write code to represent trees in a different way. Be creative! 
trees_a <- data.frame(
  id = 1:5,  # Tree identifiers
  x = c(1, 2, 3, 4, 5),  # X-coordinates of trees
  y = c(1, 2, 1, 2, 1)  # Y-coordinates of trees
)
trees_sf_a <- st_as_sf(trees_a, coords = c("x", "y"), crs = 4326)  # Convert to sf object
plot(trees_sf_a, col = "green", pch = 19, cex = 2, axes = TRUE, box = FALSE, main = "Tree Locations", legend = FALSE)

#5. There are several examples of Choropleth maps that use different classification techniques. Please provide a Choropleth map of a variable in the Marriage Dataset that uses a sequential color scheme.  
# Read in shapefile containing Michigan county data
mi_counties <- st_read("C:/Users/wroma/Downloads/GEO426_romanykw/GEO426_romanykw_repo/mi_counties/Counties_(v17a).shp")
# Clean up county names by removing any periods
mi_counties$NAME <- gsub("\\.", "", mi_counties$NAME)
# Read in CSV file containing marriage rates in Michigan for 2022
marriages <- read.csv("C:/Users/wroma/Downloads/GEO426_romanykw/GEO426_romanykw_repo/MI_Mariage_rates_2022.csv")

# Display the first few rows of each dataset for inspection
head(mi_counties)
head(marriages)

# Join the marriage data with the spatial data of Michigan counties
# and convert certain columns to numeric for analysis
mi_counties <- mi_counties %>% 
  left_join(marriages, by = c('NAME' = 'County')) %>% 
  mutate_at(c("Marriage.Number", "Marriage.Rate", "Divorce.Number", 
              "Divorce.Rate", "Population"), as.numeric)

# Plot the spatial data of Michigan counties
plot(mi_counties)

# Visualize the data using ggplot
ggplot(mi_counties) +
  geom_sf(aes(fill = Population)) +
  scale_fill_gradient(name = "Population", low = "lightblue", high = "darkblue") +  # Sequential color scheme
  theme_minimal()

