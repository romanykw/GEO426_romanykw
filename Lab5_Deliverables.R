#Lab 5 Deliverables

#Questions

#1.	Describe the difference between hue, saturation, and lightness (HSL) and the use cases for when to change the values of HSL for different cartography maps.
# - Hue is what shade of color you are using. Saturation is the amount of gray in 
# a given color. Lightness is how light or dark a color is. Hue is best for 
# mapping qualitative data while saturation and lightness are better for 
# quantitative data.

#2.	What are the RGB values of Red, Blue, and Green?
# Red- (255,0,0) Green-(0,255,0) Blue-(0,0,255)

#3.	What are some pros and cons of EPSG and ESRI based approaches to projections?
# - EPSG is comprehensive and widely accepted, but it's very complex. 
# ESRI is more user-friendly and compatible with ArcGIS, but it's not as 
# compatible with other softwares and it can be cost-prohibitive.

#4.	What is an EPSG value appropriate for the state of Michigan?
# 4326 (in WGS84)

# Your turn to code

#1.	I made a pretty bad choropleth map using a rose colored palette. Correct the coloring of my map using any of the approaches identified in the lab but keep the color as close as possible to the misty rose. Hint - enhance the coloring so that there is more or less.  (Your code should include code for this export)saturation. Export your map as a jpeg. 
#Create colors
my_color_names<-c("mistyrose1", "mistyrose2", "mistyrose3", "mistyrose4")
my_colors <- as.hexmode( c(256^(2:0) %*% col2rgb(my_color_names)) )
my_colors<-paste0("#", my_colors)
#Plot map with new colors

mf_map(mi_counties, 
       var = "Marriage.Number",
       type = "choro",
       pal = my_colors,
       nbreaks = length(my_colors))
#jpeg(Lab7_Q1.jpg)

#2.	I created a 5 class ordinal map based on the total of marriages by county. Please create an ordinal map that uses 4 classes and a different set of colors. Make sure you use the appropriate color scheme.You
#Create new ordinal data
breaks <- c(-Inf, 40, 450, 1500, Inf)
labels <- c("Very Low Marriages", "Low Marriages", 
             "High Marriages", "Very High Marriages")
mi_counties$OrdinalMarriages<- cut(mi_counties$Marriage.Number, 
                                   breaks = breaks, 
                                   labels = labels, 
                                   include.lowest = TRUE, 
                                   ordered_result = TRUE)
table(mi_counties$OrdinalMarriages)

ordinalBrewer2 <- brewer.pal(n = 4, name = "Set1")

mf_map(mi_counties, 
       var = "OrdinalMarriages",
       type = "typo",
       pal = ordinalBrewer2) 

#3.	Reproject the Michigan Shapefile data to be more appropriate projection for Michigan. 
mi_counties<-st_transform(mi_counties, 4326) #These four digits represent EPSG of 3857 which is a Web Mercator projection
plot(st_geometry(mi_counties))
dev.off()

#4.	Filter to Lithuania. Compare a WGS 1984 projection to an equal area projection that is appropriate for that area. Have a side-by-side comparison image in a jpeg and submit the jpeg. (Your code should include code for this export).
Lithuania<-world %>% filter(NAME == "Lithuania")
plot(st_geometry(Lithuania))

Lithuania2<-st_transform(Lithuania,32634) 

plot(st_geometry(Lithuania))
plot(st_geometry(Lithuania2))

#5.	Take the Michigan Shapefile and reproject it to be suited for Michigan (your choice which type equal area, equal distance, etc.). Create a choropleth map for relative risk. 
breaks <- div_breaks(mi_counties$relRisk, 7, 100)
reds <- mf_get_pal(n = 7, pal = "Reds 2", rev = TRUE) 
mf_map(mi_counties, 
       var = "relRisk",
       type = "choro",
       pal = reds,
       breaks = breaks)


