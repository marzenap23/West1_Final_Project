library(ggplot2)
library(leaflet)
library(shiny)
library(dplyr)
library(lubridate)
library(sf)
library(DT)
library(tidyr)


# Load data and preprocess
# Call Data
call_data <- read.csv("311_Phone_Call_Log_Mod.csv", header = T)
# Abandoned Property
abandoned_properties <- st_read("Abandoned_Property_Parcels/Abandoned_Property_Parcels.shp")
abandoned_properties <- abandoned_properties %>%
  drop_na(Date_of_Ou)
# Street lights
street_lights <- read.csv("Street_Lights.csv", header = T)
table(street_lights$Bulb_Type)
# Council district lines
council_district_lines <- st_read("City_Council_Districts/City_Council_Districts.shp") 

  
# Add call date and day of week
call_data$Call_Date2 <- as.Date(call_data$Call_Date)
call_data$Day_of_Week <- wday(call_data$Call_Date2, label = TRUE, abbr = FALSE) #add day of week

# call volume by department
call_volume <- call_data %>%
  group_by(Call_Date2, Day_of_Week, Department) %>%
  summarize(Count = n())

# Convert Outcome_St to factor
abandoned_properties$Outcome_St <- as.factor(abandoned_properties$Outcome_St)

# Define a color palette for Outcome_St
outcome_palette <- colorFactor(
  palette = c("Deconstructed" = "blue", "Demolished" = "orange", "Repaired" = "yellow", "Repaired & Occupied" = "gray"),
  domain = abandoned_properties$Outcome_St
)

# Convert string to date
abandoned_properties$Date_of_Ou <- as.Date(abandoned_properties$Date_of_Ou)

# Street Lights Dashboard Data
street_light_calls <- call_data %>%
  filter(Called_About == 'Street Light Outage')

# Convert to spatial dataframe
street_lights.spatial <- street_lights %>%
  st_as_sf(coords = c('Lon', 'Lat')) %>%
  st_set_crs(value = 4326)

# Convert string to date
street_lights.spatial <- street_lights.spatial %>%
  mutate(Inspect_Date = as.Date(Inspect_Date))

# Define a color palette for Outcome_St
light_palette <- colorFactor(
  palette = c("Blue - Mercury Vapor" = "blue", na.value = "orange"),
  domain = street_lights.spatial$Bulb_Type
)

# Add new dist_rep concatenated column as.factor
council_district_lines <- council_district_lines %>%
  mutate(dist_rep = as.factor(paste(Num,"-",Council_Me, sep=" ")))
# Add coordinates for label
# council_district_lines <- council_district_lines %>%
#   mutate(dist_cent_lon = st_coordinates(st_centroid(geometry))[,'X'],
#          dist_cent_lat = st_coordinates(st_centroid(geometry))[,'Y'])

# Join council district data to abandoned property data
abandoned_properties_in_dists = st_join(abandoned_properties, council_district_lines, join = st_within)

# Join council district data to abandoned property data
street_lights_in_dists = st_join(street_lights.spatial, council_district_lines, join = st_within)


ui <- fluidPage(
  titlePanel("South Bend Civic Insight Dashboard"),
  tabsetPanel(
    tabPanel("Call Data Insights",
             sidebarLayout(
               sidebarPanel(
                 # Call Data Insights sidebarPanel
                 sliderInput("dateRange_callData",
                             "Select date range:",
                             min(call_data$Call_Date2),
                             max(call_data$Call_Date2),
                             range(call_data$Call_Date2),
                             timeFormat="%Y-%m-%d"),
                 actionButton("selectAll", "Select All Days"),
                 actionButton("unselectAll", "Unselect All Days"),
                 checkboxGroupInput("daysOfWeek",
                                    "Days of the week:",
                                    choices = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
                                    selected = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
                 selectInput("viewType",
                             "Select view type:",
                             choices = c("Total Calls by Department",
                                         "Total Calls by Work Order Type",
                                         "Average Duration by Department",
                                         "Average Duration by Work Order Type"))
               ),
               mainPanel(
                 plotOutput("callAnalysisPlot")
               )
             )
    ),
    tabPanel("Abandoned Properties",
             sidebarLayout(
               sidebarPanel(
                 # Abandoned Properties Map sidebarPanel content
                 dateRangeInput("dateRange_abandonedProperties",
                                "Select date range:",
                                start = min(abandoned_properties_in_dists$Date_of_Ou),
                                end = max(abandoned_properties_in_dists$Date_of_Ou),
                                min = min(abandoned_properties_in_dists$Date_of_Ou),
                                max = max(abandoned_properties_in_dists$Date_of_Ou),
                                separator = " - ",
                                format = "yyyy-mm-dd"),
                 checkboxGroupInput("districts",
                                    "Select districts:",
                                    choices = levels(abandoned_properties_in_dists$dist_rep),
                                    selected = levels(abandoned_properties_in_dists$dist_rep)),
                 checkboxGroupInput("outcomes",
                                    "Select outcomes:",
                                    choices = levels(abandoned_properties_in_dists$Outcome_St),
                                    selected = levels(abandoned_properties_in_dists$Outcome_St)),
                 selectInput("viewType_map",
                             "Select view type:",
                             choices = c("Map", "Data Table"),
                             selected = "Map")
               ),
               mainPanel(
                 uiOutput("mapOrTable"),
                 DTOutput("dataTable")
               )
             )
    ),
    tabPanel("Street Lights",
             sidebarLayout(
               sidebarPanel(
                 # Call Data Insights sidebarPanel
                 dateRangeInput("dateRange_streetLight_callData",
                                "Select date range:",
                                start = min(call_data$Call_Date2),
                                end = max(call_data$Call_Date2),
                                min = min(call_data$Call_Date2),
                                max = max(call_data$Call_Date2),
                                separator = " - ",
                                format = "yyyy-mm-dd"),
                 actionButton("streetLightSelectAll", "Select All Days"),
                 actionButton("streetLightUnselectAll", "Unselect All Days"),
                 checkboxGroupInput("streetLightCallsDaysOfWeek",
                                    "Days of the week:",
                                    choices = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
                                    selected = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
                 checkboxGroupInput("districts2",
                                    "Select district:",
                                    choices = levels(street_lights_in_dists$dist_rep),
                                    selected = levels(street_lights_in_dists$dist_rep)),
                 selectInput("streetLightViewType",
                             "Select chart:",
                             choices = c("Daily Street Light Outage Calls by Month", "Street Light Outage Calls by Day of Week"))
               ),
               mainPanel(
                 plotOutput("street_light_calls"),
                 leafletOutput('leaflet_map2'),
                 DTOutput("dataTable2")
               )
             ),

    )
  )
)



server <- function(input, output, session) {
  # Call Data Insights server code
  output$callAnalysisPlot <- renderPlot({
    filtered_call_data <- call_data %>%
      filter(Call_Date2 >= input$dateRange_callData[1] & Call_Date2 <= input$dateRange_callData[2] & Day_of_Week %in% input$daysOfWeek)

    if (input$viewType == "Total Calls by Department") {
      plot_data <- filtered_call_data %>%
        group_by(Department) %>%
        summarize(Count = n())

      ggplot(plot_data, aes(x = Count, y = reorder(Department, Count), fill = Department)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Total Calls by Department",
             x = "Number of Calls") +
        ylab(NULL) +
        theme_minimal() +
        theme(legend.position = "none")

    } else if (input$viewType == "Total Calls by Work Order Type") {
      plot_data <- filtered_call_data %>%
        filter(Work_Order_Type != '') %>%
        group_by(Work_Order_Type) %>%
        summarize(Count = n())

      ggplot(plot_data, aes(x = Count, y = reorder(Work_Order_Type, Count), fill = Work_Order_Type)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Total Calls by Work Order Type",
             x = "Number of Calls") +
        ylab(NULL) +
        theme_minimal() +
        theme(legend.position = "none")

    } else if (input$viewType == "Average Duration by Department") {
      plot_data <- filtered_call_data %>%
        group_by(Department) %>%
        summarize(AvgDuration = mean(duration_Seconds / 60, na.rm = TRUE))

      ggplot(plot_data, aes(x = AvgDuration, y = reorder(Department, AvgDuration), fill = Department)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Average Duration by Department",
             x = "Average Duration (Minutes)") +
        ylab(NULL) +
        theme_minimal() +
        theme(legend.position = "none")

    } else if (input$viewType == "Average Duration by Work Order Type") {
      plot_data <- filtered_call_data %>%
        filter(Work_Order_Type != '') %>%
        group_by(Work_Order_Type) %>%
        summarize(AvgDuration = mean(duration_Seconds / 60, na.rm = TRUE))

      ggplot(plot_data, aes(x = AvgDuration, y = reorder(Work_Order_Type, AvgDuration), fill = Work_Order_Type)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Average Duration by Work Order Type",
             x = "Average Duration (Minutes)") +
        ylab(NULL) +
        theme_minimal() +
        theme(legend.position = "none")
    }
  })
  observeEvent(input$selectAll, {
    updateCheckboxGroupInput(session, "daysOfWeek",
                             selected = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  })
  observeEvent(input$unselectAll, {
    updateCheckboxGroupInput(session, "daysOfWeek",
                             selected = character(0))
  })


  # Abandoned Properties Map server code
  # Filter data based on user input
  # Filtered abandoned properties
  filtered_abandoned_properties <- reactive({
    abandoned_properties_in_dists %>%
      filter(
        (dist_rep %in% input$districts) &
        Date_of_Ou >= input$dateRange_abandonedProperties[1] & 
        Date_of_Ou <= input$dateRange_abandonedProperties[2] & 
        (Outcome_St %in% input$outcomes) 
      )
  })
  # Filtered council district lines
  filtered_council_district_lines <- reactive({
    council_district_lines %>%
      filter(dist_rep %in% input$districts)
  })    

  # Render leaflet map
  output$leafletMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = filtered_council_district_lines(),
        color = "darkgreen",
        weight = 3,
        fillOpacity = 0.1,
        popup = ~paste("District and Rep: ", "<br>",
                       dist_rep)
      ) %>%
      addPolygons(
        data = filtered_abandoned_properties(),
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        fillColor = ~outcome_palette(Outcome_St),
        popup = ~paste("Property ID: ", Property_S, "<br>",
                       "Outcome: ", Outcome_St, "<br>",
                       "Zip Code: ", Zip_Code)
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("blue", "orange", "yellow", "gray"),
        labels = c("Deconstructed", "Demolished", "Repaired", "Repaired & Occupied"),
        opacity = 1,
        title = "Outcome"
      ) %>%
      setView(lng = -86.25, lat = 41.6767, zoom = 12)
  })
  # Render data table
  output$dataTable <- renderDT({
    # Summarize property count by district and outcome
    filtered_data <- filtered_abandoned_properties()
    contingency_table <- table(filtered_data$dist_rep, filtered_data$Outcome_St)
    contingency_df <- as.data.frame.matrix(contingency_table)
    datatable(contingency_df)
    
  })
  # Display either the map or data table based on user selection
  output$mapOrTable <- renderUI({
    if (input$viewType_map == "Map") {
      leafletOutput("leafletMap")
    } else {
      dataTableOutput("dataTable")
    }
  })
  
  
  # Street Lights

  # Update filters based upon Un/Select All toggles
  observeEvent(input$streetLightSelectAll, {
    updateCheckboxGroupInput(session, "streetLightCallsDaysOfWeek",
                             selected = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  })
  observeEvent(input$streetLightUnselectAll, {
    updateCheckboxGroupInput(session, "streetLightCallsDaysOfWeek",
                             selected = character(0))
  })
  
  # Filter street light and district line data based on user input
  filtered_street_lights <- reactive({
    street_lights_in_dists %>%
      filter(dist_rep %in% input$districts2)
  })
  
  filtered_council_district_lines2 <- reactive({
    council_district_lines %>%
      filter(dist_rep %in% input$districts2)
  })    
  
  # Render leaflet map
  output$leaflet_map2 <- renderLeaflet({
    leaflet() %>%
      # addTiles(group = 'basic') %>%
      addTiles() %>%
      # addProviderTiles(providers$Stadia.StamenToner, group = 'B&W') %>%
      addPolygons(
        data = filtered_council_district_lines2(),
        color = "darkgreen",
        weight = 3,
        fillOpacity = 0.1,
        popup = ~paste("District and Rep: ", "<br>",
                       dist_rep)
      ) %>%
      addCircles(
        data = filtered_street_lights(),
        color = "yellow",
        weight = .1,
        fillOpacity = 0.75,
        fillColor = ~light_palette(Bulb_Type),
        popup = ~paste("Owner: ", Ownership, "<br>",
                       "Bulb: ", Bulb_Type, " - ", Wattage, "<br>",
                       "Pole Number: ", Pole_Number)
      ) %>%
      # addLayersControl(
      #   baseGroups = c('Basic', 'B&W')
      # ) %>%
      setView(lng = -86.25, lat = 41.6767, zoom = 12)
  })
  
  
  # Render data table
  output$dataTable2 <- renderDT({
    # Summarize street lights by district and outcome
    filtered_data <- filtered_street_lights()
    datatable(
      as.data.frame(filtered_data) %>% 
        mutate(District = dist_rep) %>% 
        group_by(District) %>% 
        summarize(`Street Light Count` = n(), .groups = 'drop'),
      rownames = FALSE)
  })
  
  # Graph of street light calls
  output$street_light_calls <- renderPlot({
    filtered_street_light_call_data <- call_data %>%
      filter(Called_About == 'Street Light Outage') %>%
      filter(
        Call_Date2 >= input$dateRange_streetLight_callData[1] & 
        Call_Date2 <= input$dateRange_streetLight_callData[2] & 
        Day_of_Week %in% input$streetLightCallsDaysOfWeek
      )
    
    if (input$streetLightViewType == "Daily Street Light Outage Calls by Month") {
      plot_data <- filtered_street_light_call_data %>%
        mutate(
          Month = month(Call_Date2, label = TRUE),
          Day_of_Week = as.factor(Day_of_Week)
        ) %>%
        group_by(Month, Day_of_Week) %>%
        summarize(Count = n(), .groups = 'drop')
      
      ggplot(plot_data, aes(x = Month, y = Count, fill = Day_of_Week)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Daily Street Light Outage Calls by Month", y = "Number of Calls", x = 'Month', fill = 'Day of Week') +
        scale_fill_brewer(palette = "Set1") + 
        theme_minimal()
      
    } else if (input$streetLightViewType == "Street Light Outage Calls by Day of Week") {
      plot_data <- filtered_street_light_call_data %>%
        group_by(Call_Date2, Day_of_Week) %>%
        summarize(Count = n(), .groups = 'drop')
      
      ggplot(plot_data, aes(y = Count, x = Day_of_Week, fill = Day_of_Week)) +
        geom_boxplot() +
        scale_fill_brewer(palette = "Set1") + 
        labs(title = "Street Light Outage Calls by Day of Week", y = "Number of Calls", x = 'Day of Week', fill = 'Day of Week')
    }
  })
  
}
shinyApp(ui = ui, server = server)
