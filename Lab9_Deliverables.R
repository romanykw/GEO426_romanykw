#Lab 9 Deliverables

#Questions


#1.	Describe the difference between the UI and Server elements in an R Shiny App. 
# - Setting up a UI lets users control data variables in an easily navigable way.
# People should be able to figure out how to input using dropdown menus, 
# sliders, text boxes, etc. to create a desired output. The server processes the
# "hidden layer" puts the input through functions to create the output. 

#2.	Describe the advantages and challenges of using interactive mapping in cartography.
# - Interactive mapping allows for more complex maps that contain more variables
# such as time. It allows for multiple maps, datasets, or ideas to be communicated
# quickly and at will. However, interactive mapping requires substantially 
# more setup, e.g. using the shiny package and defining ui and server functions.

#Your turn to code

#1.	Please play with the app to find an appropriate map that shows covid rates (ensure number of classes, color are appropriately selected). Take a screenshot of that map. 


#2.	There’s a lot of data in the ‘world’ object. Add some more fields to your Shiny App. Select a field (other than the three provided) and apply cartographic principles to your newly added data field. Take a screenshot and submit it. 
library(shiny)
library(leaflet)
library(rnaturalearth)
library(sf)
library(dplyr)
library(RColorBrewer)

# Load world data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Sample data: Adding more variables
#set.seed(42)
#world$gdp_per_capita <- runif(nrow(world), 500, 100000)
#world$population <- runif(nrow(world), 100000, 10000000)
#world$life_expectancy <- runif(nrow(world), 50, 80)

covid<-read.csv("owid-covid-data.csv")

covid<-covid %>% 
  group_by(location) %>% 
  summarise(total_cases = sum(new_cases, na.rm = TRUE)) %>% 
  left_join(covid %>% 
              select(population,location) %>% 
              distinct(), by = c("location" = "location")) %>% 
  mutate(covid_cases_per_10000_ppl = total_cases/(population/10000),
         location = replace(location, location == 'United States', 'United States of America'),
         location = replace(location, location == 'Bahamas', 'The Bahamas')) %>% 
  data.frame() 

covid$location <- case_when(
  covid$location == "United States" ~ "United States of America",
  covid$location == "Czechia" ~ "Czech Republic",
  covid$location == "Congo" ~ "Republic of Congo",
  covid$location == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
  covid$location == "Bahamas" ~ "The Bahamas",
  covid$location == "North Macedonia" ~ "Macedonia",
  covid$location == "Cote d'Ivoire" ~ "Ivory Coast",
  covid$location == "Eswatini" ~ "Swaziland",
  covid$location == "Timor" ~ "East Timor",
  covid$location == "Tanzania" ~ "United Republic of Tanzania",
  covid$location == "Serbia" ~ "Republic of Serbia",
  covid$location == "Congo" ~ "Republic of Congo",
  # Handle special entities or aggregated regions if necessary
  covid$location %in% c("World", "Europe", "Asia", "North America", "South America", "Africa", "Oceania") ~ NA,
  # Default case
  TRUE ~ covid$location
)

world <-world %>% 
  left_join(covid, by = c("sovereignt" = "location"))


# Define the user interface (UI) of the Shiny app
ui <- fluidPage(
  titlePanel("Interactive Choropleth Map"),  # Title of the app
  sidebarLayout(
    sidebarPanel(  # Define the sidebar with input controls
      selectInput("variable", "Variable:",  # Dropdown to select the variable to display on the map
                  choices = c("Total Cases" = "total_cases",
                              "Population" = "population",
                              "COVID Cases per 10000 People" = "covid_cases_per_10000_ppl",
                              "GDP" = "gdp_md")),
      selectInput("palette", "Color Palette:",  # Dropdown to select the color palette for the map
                  choices = rownames(brewer.pal.info)),
      sliderInput("classes", "Number of Classes:", min = 3, max = 9, value = 7),  # Slider to select the number of color classes
      width = 3
    ),
    mainPanel(  # Main panel to display the map
      leafletOutput("map"),  # Output for the leaflet map
      width = 9
    )
  )
)

# Define the server logic of the Shiny app
server <- function(input, output) {
  output$map <- renderLeaflet({  # Render the leaflet map
    color_pal <- colorQuantile(input$palette, world[[input$variable]], n = input$classes, na.color = "#ffffff")  # Create a color palette
    
    leaflet(world) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%  # Add base map tiles
      addPolygons(fillColor = ~color_pal(world[[input$variable]]),  # Add polygons for countries
                  fillOpacity = 0.8,
                  color = "#BDBDC3",
                  weight = 1,
                  popup = ~paste(name, "<br>", input$variable, ":", round(world[[input$variable]], 2))) %>%
      addLegend(pal = color_pal, values = ~world[[input$variable]], opacity = 0.7, title = "GDP")  # Add a legend
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


#3.	The legend titles are not great - please change them. 



