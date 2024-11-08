library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(RColorBrewer)  
library(readxl)

Accident <- read.csv("accident.csv")

FL_Accident <- Accident %>%
  filter(STATENAME == "Florida") %>%
  mutate(DAYNAME = case_when(
    DAY_WEEK == 1 ~ "Sunday",
    DAY_WEEK == 2 ~ "Monday",
    DAY_WEEK == 3 ~ "Tuesday",
    DAY_WEEK == 4 ~ "Wednesday",
    DAY_WEEK == 5 ~ "Thursday",
    DAY_WEEK == 6 ~ "Friday",
    DAY_WEEK == 7 ~ "Saturday",
    TRUE ~ as.character(DAY_WEEK)
  ))

ui <- fluidPage(
  titlePanel("Florida Car Accident Fatalities (2022)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("county1", "Select County 1:", 
                  choices = c("All Counties", unique(FL_Accident$COUNTYNAME)), 
                  selected = "All Counties"),
      radioButtons("dayType1", "Select Day Type for County 1:",
                   choices = c("All Days of the Week" = "All",
                               "Only Weekdays" = "Weekdays",
                               "Only Weekends" = "Weekends"),
                   selected = "All"),
      selectInput("county2", "Select County 2:", 
                  choices = c("All Counties", unique(FL_Accident$COUNTYNAME)), 
                  selected = unique(FL_Accident$COUNTYNAME)[2]),
      radioButtons("dayType2", "Select Day Type for County 2:",
                   choices = c("All Days of the Week" = "All",
                               "Only Weekdays" = "Weekdays",
                               "Only Weekends" = "Weekends"),
                   selected = "All"),
      sliderInput("hour", "Select Hour of the Day:", 
                  min = 0, max = 23, value = c(0, 23)),
      sliderInput("monthRange", "Select Month Range:", 
                  min = 1, max = 12, value = c(1, 12), 
                  step = 1, sep = "", 
                  animate = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Accident Map", leafletOutput("accidentMap", height = "600px")),
        tabPanel("Hourly Fatalities Heatmap", plotOutput("accidentHistogram", height = "400px")),
        tabPanel("Monthly Fatalities Bar Chart", plotOutput("monthlyAccidentBarChart", height = "400px"))
      )
    )
  )
)

server <- function(input, output) {
  
  filteredData1 <- reactive({
    data <- FL_Accident
    if (input$county1 != "All Counties") {
      data <- data %>% filter(COUNTYNAME == input$county1)
    }
    data %>%
      filter(
        (input$dayType1 == "All" | 
           (input$dayType1 == "Weekdays" & DAY_WEEK %in% 2:6) | 
           (input$dayType1 == "Weekends" & DAY_WEEK %in% c(1, 7))),
        HOUR >= input$hour[1],
        HOUR <= input$hour[2]
      )
  })
  
  filteredData2 <- reactive({
    data <- FL_Accident
    if (input$county2 != "All Counties") {
      data <- data %>% filter(COUNTYNAME == input$county2)
    }
    data %>%
      filter(
        (input$dayType2 == "All" | 
           (input$dayType2 == "Weekdays" & DAY_WEEK %in% 2:6) | 
           (input$dayType2 == "Weekends" & DAY_WEEK %in% c(1, 7))),
        HOUR >= input$hour[1],
        HOUR <= input$hour[2]
      )
  })
  
  monthlyData1 <- reactive({
    data <- FL_Accident
    if (input$county1 != "All Counties") {
      data <- data %>% filter(COUNTYNAME == input$county1)
    }
    data %>%
      filter(
        (input$dayType1 == "All" | 
           (input$dayType1 == "Weekdays" & DAY_WEEK %in% 2:6) | 
           (input$dayType1 == "Weekends" & DAY_WEEK %in% c(1, 7)))
      ) %>%
      mutate(MONTH = as.integer(MONTH)) %>%
      filter(MONTH >= input$monthRange[1], MONTH <= input$monthRange[2]) %>%
      group_by(MONTH) %>%
      summarise(
        fatalities = sum(FATALS),
        total_accidents = n(),  
        .groups = "drop"
      )
  })
  
  monthlyData2 <- reactive({
    data <- FL_Accident
    if (input$county2 != "All Counties") {
      data <- data %>% filter(COUNTYNAME == input$county2)
    }
    data %>%
      filter(
        (input$dayType2 == "All" | 
           (input$dayType2 == "Weekdays" & DAY_WEEK %in% 2:6) | 
           (input$dayType2 == "Weekends" & DAY_WEEK %in% c(1, 7)))
      ) %>%
      mutate(MONTH = as.integer(MONTH)) %>%
      filter(MONTH >= input$monthRange[1], MONTH <= input$monthRange[2]) %>%
      group_by(MONTH) %>%
      summarise(
        fatalities = sum(FATALS),
        total_accidents = n(),  
        .groups = "drop"
      )
  })
  
  output$accidentMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -81.5158, lat = 27.6648, zoom = 7)
  })
  
  observe({
    data1 <- filteredData1()
    data2 <- filteredData2()
    
    combined_data <- rbind(data1, data2)
    
    if (nrow(combined_data) > 0) {
      leafletProxy("accidentMap") %>%
        clearMarkers() %>%
        addCircleMarkers(
          data = combined_data,
          lng = ~LONGITUD,
          lat = ~LATITUDE,
          radius = 3,
          color = ifelse(combined_data$COUNTYNAME == input$county1, "blue", "red"),
          fillOpacity = 0.7,
          popup = ~paste("City:", CITYNAME, "<br>",
                         "County:", COUNTYNAME, "<br>",
                         "Date:", paste(MONTH, DAY, YEAR), "<br>",
                         "Time:", paste(HOUR, ":", MINUTE), "<br>",
                         "Fatalities:", FATALS)
        )
    } else {
      leafletProxy("accidentMap") %>% clearMarkers()
    }
  })
  
  output$accidentHistogram <- renderPlot({
    data1 <- filteredData1()
    data2 <- filteredData2()
    
    combined_data <- rbind(data1, data2)
    
    heatmap_data <- combined_data %>%
      group_by(HOUR, COUNTYNAME) %>%
      summarise(fatalities = n(), .groups = "drop")  
    
    ggplot(heatmap_data, aes(x = HOUR, y = COUNTYNAME, fill = fatalities)) +
      geom_tile(color = "white") + 
      scale_fill_gradient(low = "white", high = "red") +  
      labs(title = "Heatmap of Fatalities by Hour",
           x = "Hour of the Day",
           y = "County") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  
  })
  
  output$monthlyAccidentBarChart <- renderPlot({
    data1 <- monthlyData1()
    data2 <- monthlyData2()
    
    combined_data <- rbind(data1 %>% mutate(COUNTYNAME = input$county1),
                           data2 %>% mutate(COUNTYNAME = input$county2))
    
    ggplot(combined_data, aes(x = factor(MONTH, levels = 1:12, labels = month.abb), 
                              y = fatalities, fill = COUNTYNAME)) +  
      geom_bar(stat = "identity", position = "dodge", color = "black") +
      labs(title = "Monthly Fatalities by County",
           x = "Month",
           y = "Number of Fatalities") +  
      theme_minimal() +
      scale_fill_brewer(palette = "Set3") +  
      theme(legend.title = element_blank())
  })
}

shinyApp(ui = ui, server = server)
