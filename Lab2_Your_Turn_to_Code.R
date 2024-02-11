#Lab 2 - Your Turn to Code!

data <- read.csv("Lab 2 Data.csv")

#1. Please provide three (3) measures of centrality for the poverty rate field in the data? 
p_mean <- mean(data$poverty, na.rm = TRUE)
p_median <- median(data$poverty, na.rm = TRUE)
p_sd <- sd(data$poverty, na.rm = TRUE)

print(paste("Mean Poverty:", p_mean))
print(paste("Median Poverty:", p_median))
print(paste("Standard Deviation of Poverty:", p_sd))

#2. Construct a boxplot of poverty rate with a title and the x-axis labeled. Provide a graphic of this boxplot in your answers. 
boxplot(data$poverty, main="Boxplot of Poverty Rate", ylab="Poverty Rate")

#3. Is the distribution of poverty rate skewed? If so, how? Be sure your code reflects your exploratory work and findings.  
hist(data$poverty, main="Histogram of Poverty Rate", xlab="Poverty Rate")
#Yes, the graph is skewed towards the lower end. 

#4. The code provides an example of different spatial dimensions (point, line, polygon). Using the code provided as a blueprint, please provide your own examples (in code!) of these three spatial dimensions. That is to say please alter the coordinates of the point, line, and polygons so that the plots are different.  

points1 <- st_as_sf(data.frame(id = 1:2, x = c(1,3), y = c(7, 5)), coords = c("x", "y"), crs = 4326)
points1
print(points1)


line_data1 <- data.frame(id = c(1, 1), x = c(2,5), y = c(5,2))
line_sf1 <- st_as_sf(line_data1, coords = c("x", "y"), crs = 4326)
line_sf_line1 <- line_sf1 %>% group_by(id) %>% summarise(geometry = st_combine(geometry)) %>% st_cast("LINESTRING")


coords1 <- matrix(c(2,2, 4,2, 4,4, 2,4, 2,2), byrow = TRUE, ncol = 2)
poly1 <- st_polygon(list(coords))
polygon_sf1 <- st_sf(geometry = st_sfc(poly), crs = 4326)


plot(st_geometry(points1), col = 'blue', pch = 19)
plot(st_geometry(line_sf_line1), col = 'red')
plot(polygon_sf1, col = 'green')

#5. A points data is being created in the code. Please add three additional points to the code and change the color of the associated plot from blue to black. 
points2 <- st_as_sf(data.frame(id = 1:5, x = c(1,3,2,4,5), y = c(7,5,4,6,9)), coords = c("x", "y"), crs = 4326)
points2
print(points2)

plot(st_geometry(points2), col = 'black', pch = 19)

#Questions
#1.	$26093.12
#2.	A histogram with more bins will show a more specific representation of data that makes it easier to see at small intervals. 
#3.	A histogram is good for looking at data in specific intervals, while a boxplot is better for looking at data in quartiles and determining outliers.
#4.	I could make a boxplot and get rid of outliers. 
#5.	Trees could be points or polygons depending on the scale of the map.

12345

