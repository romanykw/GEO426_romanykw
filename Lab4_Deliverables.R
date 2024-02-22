#Lab 4 Deliverables

#1.	When would you want to change the scale of your data?
# - To show different aspects of a mapped area.

#2.	What are some pros and cons of changing your raster resolution to have less resolution?
# - Having a worse resolution makes an image less clear, but it can reduce 
#the amount of data used if using less data is a priority.

#3.	When would you want to generalize your polygon data (think countries, states, counties, etc)
# - I would want to generalize polygon data to reduce the amount of data being
#stored.

#4.	How is simplifying line data different from smoothing the line data?
# - Simplifying is reducing the number of vertices in a line while smoothing
# makes a line more visually appealing. 

#5.	When we are aggregating point to polygons in what situation do you want to use a concave approach? A convex approach?
# - Concave polygons are better for detailed approaches while convex polygons
# are better for simplified approaches.



#Your turn to code

#1.	Take the world map object and subset to Germany and produce a jpeg image. Submit your code and the Germany jpeg image. 
world <- st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))
world_valid <- st_make_valid(world)
#Continent Scale Map - Focusing on Europe
europe <- st_crop(world_valid, xmin = -10, xmax = 40, ymin = 35, ymax = 70)
ggplot(data = europe) +
  geom_sf() +
  labs(title = "European Map - Continent Scale") +
  theme_minimal()

#Country Scale Map - Focusing on Germany
germany <- st_crop(europe, xmin = 5, xmax = 15, ymin = 47, ymax = 55)
germany_plot <- ggplot(data = germany) +
  geom_sf() +
  labs(title = "Germany Map - Country Scale") +
  theme_minimal()
#Save Plot
ggsave(filename = "germany_plot.jpeg", plot = germany_plot, device = "jpeg")

#2.	Change the resolution of the volcano dataset to a factor of 30 and find the ‘max’ cell stat. 
# Load the volcano dataset
data("volcano")

# Original dimensions of the volcano dataset
original_dims <- dim(volcano)

# Remove columns and rows with all NA values
volcano_clean <- volcano[!apply(is.na(volcano), 1, all), !apply(is.na(volcano), 2, all)]

# New dimensions with factor of 30
new_dims <- original_dims * 30

# Create new grid using bilinear interpolation
new_volcano <- apply(volcano_clean, 1:2, function(x) {
  non_na_indices <- which(!is.na(x))
  if (length(non_na_indices) < 2) {
    interpolated_values <- rep(NA, length(x))
  } else {
    interp <- approxfun(non_na_indices, x[non_na_indices], method = "linear", rule = 2)
    interpolated_values <- interp(seq(1, length(x), length.out = new_dims[1]))
  }
  interpolated_values
})

# Find the max cell statistic
max_value <- max(new_volcano, na.rm = TRUE)

# Output the result
print(paste("Max cell statistic in the new grid:", max_value))



#3.	We did some smoothing operations. Please smooth the North Carolina dataset using the ‘ksmooth’ and ‘spline’ approaches and plot them to the screen on a 2x2 grid. 
# Load the dataset
#Original Vector Data
nc <- st_read(system.file("shape/nc.shp", package="sf"))
plot(st_geometry(nc), main="Original North Carolina Shapefile")

# Separate the data into x and y coordinates
x <- nc$NC$x
y <- nc$NC$y

# Plot original data
par(mfrow = c(2, 2))  # Set up 2x2 grid for plotting

# Plot original data
plot(x, y, main = "Original Data", pch = 20, col = "blue")

# Smoothing using ksmooth
smooth_ks <- ksmooth(x, y, "normal", bandwidth = 0.1)
lines(smooth_ks$x, smooth_ks$y, col = "red", lwd = 2)
title(main = "Smoothing with ksmooth")

# Smoothing using spline
smooth_spline <- smooth.spline(x, y)
lines(smooth_spline, col = "green", lwd = 2)
title(main = "Smoothing with spline")

# Add legend
legend("topleft", legend = c("Original", "ksmooth", "spline"), col = c("blue", "red", "green"), lwd = 2)

# Reset plot settings
par(mfrow = c(1, 1))

#4.	Plot your own convex and concave polygon using your own point dataset (you might have to browse the internet for a set of spatial points data).
# Generate some random points
set.seed(24)
n_points <- 40
points <- matrix(runif(10 * n_points, min = -4, max = 10), ncol = 2)
colnames(points) <- c("x", "y")
points_sf <- st_as_sf(data.frame(points), coords = c("x", "y"), crs = 4326)

# Create the convex hull as a polygon
convex_hull_polygon <- st_convex_hull(st_combine(points_sf))

# Create the concave hull as a polygon
library(concaveman)
concave_hull_polygon <- st_sf(geometry = concaveman(points_sf))

#Plots
plot(st_geometry(points_sf), col = 'blue', pch = 20, cex = 0.5)
plot(st_geometry(convex_hull_polygon), col = 'red', border = 'black', lwd = 2)
plot(st_geometry(concave_hull_polygon), col = 'red', border = 'black', lwd = 2)
