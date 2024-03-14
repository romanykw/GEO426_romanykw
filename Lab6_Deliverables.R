#Lab 6 Deliverables

#Questions

#1.	Describe when you would use a choropleth map versus a proportional symbol map versus a dot density map.
#- A choropleth map is best for a measuring a value that is consistent across
#an entire area, hence the solid color of the area. A proportional symbol map
#can use color fill and symbols to compare different areas that are mapped.

#2.	In your own words, describe what the ggplot2 library does in R and why it might be more useful than the ‘mf_map’ function in mapsf library. 
# - ggplot2 is a function used to plot data. It is useful for maps because
#the cartographer can control many paramaters within the function.

#Your turn to code

#1.	Please create a proportional symbol map that looks different than the one supplied in the code. Save an image of it and be sure to turn it in.  
ggplot() +
  geom_sf(data = abq, fill = "salmon", color = "black") +  
  geom_point(data = fb_shp_coords, aes(x = X, y = Y, size = checkins), 
             shape = 21, color = "white", fill = NA, stroke = 1.2) +  
  geom_point(data = fb_shp_coords, aes(x = X, y = Y, size = checkins), 
             shape = 21, fill = "green3", alpha = 0.4) +  
  scale_size_continuous(range = c(3, 10), 
                        breaks = c(2500, 5000, 7500),  
                        labels = c("2.5k", "5k", "7.5k")) +
  guides(size = guide_legend(override.aes = list(fill = "green3", color = "white", alpha = 0.4, stroke = 1.2))) +
  theme_minimal() +
  labs(size = "Check-ins", title = "Facebook Check-ins in Albuquerque") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

#2.	Please create a dot density  map that looks different than the one supplied in the code. Save an image of it and be sure to turn it in.
ggplot() +
  geom_sf(data = abq, fill = "lightblue", color = "black") +  # Base map
  geom_point(data = dots_data, aes(x = X, y = Y), 
             size = 0.5, color = "pink") +  # Solid blue dots
  theme_minimal() +
  labs(title = "Geographically Weighted Dot Density Map\nof Facebook Check-ins",
       caption = "1 dot = 500 check-ins") +  # Add caption to explain dot representation
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title
    plot.caption = element_text(hjust = 0.5))  # Center the caption

#3.	Please create a choropleth  map using the ‘mf_map’ function - try to make this map as visually appealing and appropriate as possible. Save an image of it and be sure to turn it in. 
abq_sf <- st_as_sf(abq, coords = c("longitude_column", "latitude_column"), crs = 4326)

# Plot using mf_map()
mf_map(abq_sf, fill = "estimate_DP05_0066", col = "white") +
  scale_fill_viridis_c() +  # Using Viridis color scale
  ggtitle("Choropleth Map") +
  labs(fill = "Value") +
  theme_minimal() +
  theme(legend.position = "bottom")

#4.	Please create a choropleth map using the ggplot2 library. Some code has been supplied to help start the process. It will be your job to finish/augment the code to produce the choropleth map. Save an image of it and be sure to turn it in.. 
ggplot(data = abq) +
  geom_sf(aes(fill = estimate_DP05_0066), color = "white") +  
  scale_fill_viridis_c() +  # Use a color scale that's perceptually uniform
  labs(title = "Choropleth Map", fill = "Value") +  # Add titles and labels
  theme_minimal() +  # Use a minimal theme
  theme(legend.position = "bottom")  # Position the legend at the bottom
