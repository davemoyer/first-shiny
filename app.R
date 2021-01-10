## test a map-based shiny app using leaflet
## August-September 2019
## Dave

districts_info <- readRDS("data/districts_info.rds")
schools <- readRDS("data/schools.rds")
library(leaflet)
library(shiny)


## begin shiny app
ui <- fluidPage(theme = "bootstrap.min.css",
                
  titlePanel("Oregon School Demographics and Performance"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("district_data_select", 
                  label = "Choose district data",  
                  choices = list("Economically disadvantaged",
                                 "English Learners",
                                 "With disabilities"),
                  selected = "Economically Disadvantaged"),
      selectInput("school_data_select",
                  label = "Choose school data",
                  choices = list("Attendance",
                                 "ELA Proficiency",
                                 "Math Proficiency"),
                  selected = "Attendance")),
    
    mainPanel(
      leafletOutput("mymap")
    )
  )
)

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    
    district_data <- switch(input$district_data_select, 
                   "English Learners" = districts_info$el,
                   "With disabilities" = districts_info$sped,
                   "Economically disadvantaged" = districts_info$frl)
    
    district_color <- switch(input$district_data_select, 
                    "English Learners" = districts_info$el_pctile,
                    "With disabilities" = districts_info$sped_pctile,
                    "Economically disadvantaged" = districts_info$frl_pctile)
    
    school_data <- switch(input$school_data_select, 
                            "Attendance" = schools$reg_attend_pctile,
                            "ELA Proficiency" = schools$ela_pctile,
                            "Math Proficiency" = schools$math_pctile)
    
    school_color <- switch(input$school_data_select, 
                          "Attendance" = schools$reg_attend_pctile,
                          "ELA Proficiency" = schools$ela_pctile,
                          "Math Proficiency" = schools$math_pctile)
    
    school_labels <- switch(input$school_data_select,
                            "Attendance" = schools$reg_attend,
                            "ELA Proficiency" = schools$ela,
                            "Math Proficiency" = schools$math)
    
    district_labels <- sprintf(
      "<strong>%s</strong><br/>%s: %s%%",
      districts_info$district_name, input$district_data_select, district_data) %>% 
      lapply(htmltools::HTML)
    
    school_labels <- sprintf(
      "<strong>%s</strong><br/>%s: %s%%",
      schools$school_name, input$school_data_select, school_labels) %>%
      lapply(htmltools::HTML)
    
    bins <- c(0,0.25,0.5,0.75,1)
    district_pal <- colorBin("PuBu", domain = district_color, bins = bins)
    school_pal <- colorBin("RdBu", domain = school_color, bins = bins)
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = districts_info,
                  group = "Districts",
                  color = "#444444",
                  fillColor = ~district_pal(district_color),
                  label = district_labels,
                  weight = 0.25,
                  smoothFactor = 0.5,
                  opacity = 0.7,
                  fillOpacity = 0.5) %>%
      addCircleMarkers(data = schools,
                       group = "Schools",
                       lng = ~schools$lon,
                       lat = ~schools$lat,
                       label = school_labels,
                       fillColor = ~school_pal(school_color),
                       fillOpacity = 0.7,
                       radius = ~enroll_scaled,
                       opacity = 0.8,
                       stroke = FALSE) %>%
      addLayersControl(overlayGroups = c("Schools", "Districts"),
                       options = layersControlOptions(collapsed = FALSE))
      # addLegend(
      #   pal = school_pal, 
      #   values = bins,
      #   opacity = 0.7, 
      #   title = input$school_data_select,
      #   position = "bottomright") %>%
      # addLegend(
      #   pal = district_pal, 
      #   values = bins,
      #   opacity = 0.7, 
      #   title = input$district_data_select,
      #   position = "bottomright")
  })
}

shinyApp(ui, server)
