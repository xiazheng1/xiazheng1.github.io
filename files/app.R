# URL:    https://xiazheng.shinyapps.io/homework-2-xiazheng1/

#__Question 3 (40%):__ Using what you created for question 2, convert it into a Shiny app.  Allow at least two elements to be 
# controlled in the UI. Then add the option to toggle streets on and off in your choropleth, using 
# the [Major Streets shapefile](https://data.cityofchicago.org/Transportation/Major-Streets/ueqs-5wr6).  
# Save this code as "app.R", and include the Major Streets shapefile in your repo.
library(sf)
library(tidyverse)
library(spData)
library(scales)
library(plotly)

# create ui
ui <- fluidPage(
  selectInput(inputId = "outcome",
              label = "Choose an educational outcome",
              choices = c("`Grade 3-8 On-Track Percentage 2013`", "`4-Year Graduation Rate Percentage 2013`")),
  
  selectInput(inputId = "zipcode",
              label = "Choose a Zip Code",
              choices = c("60628", "60639", "60609", "60652", "60644", "60634", "60616", "60625", "60607", "60623", "60620", "60612", "60608",
                          "60642", "60659", "60641", "60605", "60615", "60640", "60624", "60614", "60655", "60653", "60626", "60631", "60638",
                          "60622", "60621", "60636", "60643", "60629", "60618", "60649", "60660", "60617", "60619", "60613", "60632", "60647",
                          "60610", "60637", "60651")),
  radioButtons(inputId = "streets",
               label = "show major streets?",
               choices = c("Yes", "No")),
  plotlyOutput("ed")
)

# create server
server <- function(input, output) {
  df_es <- read_csv("df_es.csv")    # read in two datasets used above
  df_hs <- read_csv("df_hs.csv")
  
  chicago_zipcode <- st_read("geo_export_f6937836-efed-4725-8e38-25392c90f339.shp")  # read in the shapefiles
  chicago_zipcode$zip <- as.numeric(as.character(chicago_zipcode$zip))
  major_st <- st_read("Major_Streets.shp")
  
  df <- inner_join(df_es, df_hs, by = c("ZIP Code" = "ZIP Code")) %>%   # join the two datasets 
    group_by(`ZIP Code`) %>%                                           #  compute mean education outcomes by zipcodes
    mutate(`Grade 3-8 On-Track Percentage 2013` = mean(`Grade 3-8 On-Track Percentage 2013`, na.rm = TRUE)) %>% 
    mutate(`4-Year Graduation Rate Percentage 2013` = mean(`4-Year Graduation Rate Percentage 2013`, na.rm = TRUE))
  
  df <- inner_join(df, chicago_zipcode, by = c("ZIP Code" = "zip"))   # join the education data and the chicago zip codes shapefile
  
  df <- st_sf(df)  # extend the dataframe into a simple feature object
  
  major_st <- st_transform(major_st, 4326)    # make sure the crs of the shapefiles match
  st_crs(df) == st_crs(major_st) 
  
  data <- reactive({
    filter(df, `ZIP Code` == input$zipcode)
  })
  
  output$ed <- renderPlotly({                # make choropleths with the option to toggle streets on and off
    if (input$streets == "Yes") {
      plt <- ggplot(data = data()) +
        geom_sf(data = chicago_zipcode) +
        geom_sf(data = major_st) +
        geom_sf(aes(fill = input$outcome)) + 
        labs(title = "Chicago Public School Educational Outcome", 
             fill = "Percentage") +
        theme_void()
    }
    else if (input$streets == "No") {
      plt <- ggplot(data = data()) +
        geom_sf(data = chicago_zipcode) +
        geom_sf(aes(fill = input$outcome)) + 
        labs(title = "Chicago Public School Educational Outcome", 
             fill = "Percentage") +
        theme_void()
    }
    ggplotly(plt)
  }) 
  
}

shinyApp(ui = ui, server = server)
