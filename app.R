library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(shinydashboard)
library(rvest)

ui <- fluidPage(
  
  dashboardPage( skin = "red",
                 dashboardHeader(title = "F1 pilots comparison"),
                 dashboardSidebar(sidebarMenu(menuItem(" General Comparison", icon = icon("medal"), tabName = "GeneralComparison"),
                                              menuItem(" Season standings", icon = icon("flag-checkered"), tabName = "Seasonstandings"),
                                              menuItem(" Races", icon = icon("rocket"), tabName = "Races"),
                                              menuItem(" Races Geograpy", icon = icon("location-dot"), tabName = "RacesGeograpy"),
                                              div(style = "width: 22rem",
                                                  selectInput("season",
                                                              "Season:",
                                                              choices = NULL),
                                                  selectInput("pilot1",
                                                              "First pilot:",
                                                              choices = NULL),
                                                  selectInput("pilot2",
                                                              "Second pilot:",
                                                              choices = NULL)
                                              ) 
                                              
                 )
                 ),
                 dashboardBody(
                   tabItems (
                     tabItem(tabName = "GeneralComparison",
                             h2("General Comparison of Pilots"),
                             h4("The graph shows the comparison of the chosen pilots in terms of their performance during the season."),
                             
                             div(style = "margin-top: 1rem;",
                                 fluidRow(
                                   
                                   
                                   box(
                                     title = textOutput("inputValue2"), status = "danger", solidHeader = TRUE,
                                     collapsible = TRUE,
                                     uiOutput("image2"),
                                     htmlOutput("inputValue2Name"),
                                     htmlOutput("inputValue2Code"),
                                     htmlOutput("inputValue2DOB"),
                                     htmlOutput("inputValue2Nationality")),
                                   
                                   box(
                                     title = textOutput("inputValue1"), status = "danger", solidHeader = TRUE,
                                     collapsible = TRUE,
                                     uiOutput("image1"),
                                     htmlOutput("inputValue1Name"),
                                     htmlOutput("inputValue1Code"),
                                     htmlOutput("inputValue1DOB"),
                                     htmlOutput("inputValue1Nationality")),
                                   
                                   box(
                                     title = "Performance Metrics for Pilot Comparison", status = "danger", solidHeader = TRUE,
                                     collapsible = TRUE,
                                     plotlyOutput("gencomp", height = "60rem"), width = 12)
                                 ))
                     ),
                     tabItem(tabName = "Races",
                             h2("Results of the Races"),
                             h4("The graph shows the comparison of the chosen metric between two pilots for a chosen race."),
                             div(style = "margin-top: 1rem;",
                                 
                                 fluidRow(
                                   
                                   box(
                                     title = "Grand Prix", background = "black", solidHeader = TRUE,
                                     selectInput("track",
                                                 "Select Gran Prix here",
                                                 choices = NULL)
                                   ),
                                   box(
                                     title = "Inputs", background = "black", solidHeader = TRUE,
                                     selectInput("var",
                                                 "Select Parameter here",
                                                 choices = c("Position", "Lap Time"),
                                                 selected = "Position")
                                   ),
                                   box(
                                     title = "Refrormance During the Race", status = "danger", solidHeader = TRUE,
                                     collapsible = TRUE,
                                     plotlyOutput("circuitChart"), width = 12
                                   ),
                                 )
                             ),
                     ),
                     tabItem(tabName = "Seasonstandings",
                             h2("Performance during the season"),
                             h4("The graph shows the comparison of the chosen metric between two pilots for a chosen season."),
                             div(
                               style = "margin-top: 1rem;",
                               fluidRow(
                                 
                                 box(
                                   title = "Season standings", background = "black", solidHeader = TRUE,
                                   selectInput("season_stats",
                                               "Select metric here",
                                               choices = c("Position", "Points"))),
                                 
                                 box(
                                   title = "Refrormance During the Race", status = "danger", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   plotlyOutput("seasonChart"), width = 12
                                 ),
                               ))
                     ),
                     tabItem(tabName = "RacesGeograpy",
                             h2("Geography of the races during the season"),
                             h4("This graph showcases the Formula 1 tracks where the selected pilots competed during the season"),
                             div(style = "margin-top: 1rem;",
                                 fluidRow(
                                   box(
                                     title = "Geography of the races within one season", status = "danger", solidHeader = TRUE,
                                     collapsible = TRUE,
                                     leafletOutput("geograpyChart", height = "60rem" ), width = 12
                                   ),
                                 ))
                     )
                   )
                 )
                 
  )
)

server <- function(input, output, session) {
  seasons_ds <- readr::read_csv("data/seasons.csv")
  pilots_ds <- readr::read_csv("data/drivers.csv")
  results_ds <- readr::read_csv("data/results.csv")
  races_ds <- readr::read_csv("data/races.csv")
  laps_ds <- readr::read_csv("data/lap_times.csv")
  circuit_ds <- readr::read_csv("data/circuits.csv")
  
  observe({
    seasons_distinct <- distinct(seasons_ds, year) %>%
      arrange(desc(year)) %>%
      slice(-1)
    
    updateSelectInput(session, "season", choices = seasons_distinct$year)
  })
  
  observeEvent(input$season, {
    
    season_races <- races_ds %>%
      filter(year == input$season) %>%
      arrange(round) %>%
      select(name) %>%
      distinct()
    
    updateSelectInput(session, "track", choices = season_races$name)
    
    pilots_distinct <- pilots_ds %>%
      inner_join(results_ds, by = "driverId") %>%
      inner_join(races_ds, by = "raceId") %>%
      filter(year == input$season) %>%
      mutate(fullname = paste(forename, surname, sep = " ")) %>%
      distinct() %>%
      arrange(fullname)
    
    shuffled_pilots <- sample(pilots_distinct$fullname)
    
    random_pilot1 <- shuffled_pilots[1]
    random_pilot2 <- shuffled_pilots[2]
    
    updateSelectInput(session, "pilot1", choices = pilots_distinct$fullname, selected = random_pilot1)
    updateSelectInput(session, "pilot2", choices = pilots_distinct$fullname, selected = random_pilot2)
  })
  
  output$gencomp <- renderPlotly({
    pilots_standings <- results_ds %>%
      inner_join(pilots_ds, by = "driverId") %>%
      inner_join(races_ds, by = "raceId") %>%
      filter(year == input$season) %>%
      mutate(fullname = paste(forename, surname, sep = " ")) %>%
      group_by(fullname) %>%
      mutate(wins = sum(position == 1)) %>%
      mutate(poles = sum(grid == 1)) %>%
      mutate(fastestLaps = sum(rank == 1)) %>%
      mutate(podiums = sum(position == 1 | position == 2 | position == 3)) %>%
      mutate(dnfs = sum(position == "\\N")) %>%
      mutate(pts = round(sum(points)/n_distinct(raceId), 1)) %>%
      filter(fullname == input$pilot1 | fullname == input$pilot2) %>%
      select(fullname, wins, poles, pts, fastestLaps, podiums, dnfs) %>%
      distinct
    
    trace1 <- list(
      name = pilots_standings[2,]$fullname,
      type = "bar",
      x = c(pilots_standings[2,]$wins, pilots_standings[2,]$poles,
            pilots_standings[2,]$fastestLaps, pilots_standings[2,]$pts,
            pilots_standings[2,]$podiums, pilots_standings[2,]$dnfs),
      y = c("Wins", "Poles", "Fastest Laps", "Avg. Points", "Podiums", "DNF"),
      marker = list(color = "lightblue"),
      orientation = "h"
    )
    trace2 <- list(
      name = pilots_standings[1,]$fullname,
      type = "bar",
      x = c(pilots_standings[1,]$wins, pilots_standings[1,]$poles,
            pilots_standings[1,]$fastestLaps, pilots_standings[1,]$pts,
            pilots_standings[1,]$podiums, pilots_standings[1,]$dnfs),
      y = c("Wins", "Poles", "Fastest Laps", "Avg. Points", "Podiums", "DNF"),
      marker = list(color = "red"),
      xaxis = "x2",
      yaxis = "y2",
      orientation = "h"
    )
    layout <- list(
      title = paste(input$pilot1, "vs", input$pilot2, input$season),
      xaxis = list(
        type = "linear",
        range = c(22, 0),
        domain = c(0, 0.5),
        showticklabels = FALSE
      ),
      yaxis = list(
        type = "category",
        autorange = TRUE
      ),
      xaxis2 = list(
        type = "linear",
        range = c(0, 22),
        anchor = "y2",
        domain = c(0.5, 1),
        showticklabels = FALSE
      ),
      yaxis2 = list(
        type = "category",
        anchor = "x2",
        domain = c(0, 1),
        autorange = TRUE,
        showticklabels = FALSE
      ),
      autosize = TRUE,
      showlegend = TRUE
    )
    p <- plot_ly()
    p <- add_trace(p, uid=trace1$uid, type=trace1$type, x=trace1$x, y=trace1$y,
                   orientation=trace1$orientation, marker = trace1$marker, name = trace1$name, text = trace1$x)
    p <- add_trace(p, uid=trace2$uid, type=trace2$type, x=trace2$x, y=trace2$y, xaxis=trace2$xaxis,
                   yaxis=trace2$yaxis, marker = trace2$marker, name = trace2$name, orientation=trace2$orientation, text = trace2$x )
    p <- layout(p, title=layout$title, width=layout$width, xaxis=layout$xaxis, yaxis=layout$yaxis,
                height=layout$height, xaxis2=layout$xaxis2, yaxis2=layout$yaxis2, autosize=layout$autosize,
                showlegend=layout$showlegend)
    
    
    
  })
  
  output$image1 <- renderUI({
    
    page_link <- pilots_info %>% filter(fullname == input$pilot1) %>% pull(url)
    page <- read_html(page_link)
    
    image_url <- page %>%
      html_nodes("div#mw-content-text img") %>%
      html_attr("src") %>%
      .[1]
    
    tags$img(src = image_url, width = "300px")
  })
  
  output$image2 <- renderUI({
    
    page_link <- pilots_info %>% filter(fullname == input$pilot2) %>% pull(url)
    page <- read_html(page_link)
    
    image_url <- page %>%
      html_nodes("div#mw-content-text img") %>%
      html_attr("src") %>%
      .[1]
    
    tags$img(src = image_url, width = "300px")
  })
  
  
  output$inputValue1 <- renderText({ paste("Pilot 1: ", input$pilot1) })
  output$inputValue2 <- renderText({ paste("Pilot 2: ", input$pilot2) })
  
  
  pilots_info <- pilots_ds %>%
    mutate(fullname = paste(forename, surname, sep = " ")) %>%
    mutate(dob = format(dob, "%Y-%m-%d")) %>%
    arrange(fullname)
  
  
  output$inputValue1Name <- renderText({ paste("<b>Full Name: </b>", input$pilot1) })
  output$inputValue1Code <- renderText({ paste("<b>Code: </b>", pilots_info %>% filter(fullname == input$pilot1) %>% select(code)) })
  output$inputValue1DOB <- renderText({ paste("<b>DOB: </b>", pilots_info %>% filter(fullname == input$pilot1) %>% select(dob)) })
  output$inputValue1Nationality <- renderText({ paste("<b>Nationality: </b>", pilots_info %>% filter(fullname == input$pilot1) %>% select(nationality)) })
  
  output$inputValue2Name <- renderText({ paste("<b>Full Name: </b>", input$pilot2) })
  output$inputValue2Code <- renderText({ paste("<b>Code: </b>", pilots_info %>% filter(fullname == input$pilot2) %>% select(code)) })
  output$inputValue2DOB <- renderText({ paste("<b>DOB: </b>", pilots_info %>% filter(fullname == input$pilot2) %>% select(dob)) })
  output$inputValue2Nationality <- renderText({ paste("<b>Nationality: </b>", pilots_info %>% filter(fullname == input$pilot2) %>% select(nationality)) })
  
  
  
  output$seasonChart <- renderPlotly({
    season_results <- results_ds %>%
      inner_join(races_ds, by = "raceId") %>%
      inner_join(pilots_ds, by = "driverId") %>%
      filter(year == input$season) %>%
      mutate(fullname = paste(forename, surname, sep = " ")) %>%
      arrange(date) %>%
      filter(fullname == input$pilot1 | fullname == input$pilot2)
    
    if(input$season_stats=="Position") {
      season_results <-  season_results %>% select(name, fullname, position) %>%
        mutate(position = ifelse(position == "\\N", "DNF", position)) %>%
        mutate(position = factor(position, levels = c(as.character(1:20), "DNF"))) %>%
        mutate(name = factor(name, levels = unique(name)))
      
      
      chart <- plot_ly(season_results, x = ~name, y = ~position, color = ~fullname, type = "scatter",
                       colors = "Set1", mode = "lines",
                       line = list(width = 1.5)) %>%
        layout(title = paste(input$season, "Pilot Positions"),
               xaxis = list(title = "Race"),
               yaxis = list(title = "Position", autorange = "reversed"),
               showlegend = TRUE)
    }
    else {
      season_results <-  season_results %>%  select(name, fullname, points) %>%
        mutate(name = factor(name, levels = unique(name))) %>%
        group_by(fullname) %>%
        mutate(points = cumsum(points))
      
      chart <- plot_ly(season_results, x = ~name, y = ~points, color = ~fullname, type = "scatter",
                       colors = "Set1", mode = "lines",
                       line = list(width = 1.5)) %>%
        layout(title = paste(input$season, "Pilot Standings"),
               xaxis = list(title = "Race"),
               yaxis = list(title = "Points"),
               showlegend = TRUE)
    }
    
    chart
  })
  
  output$circuitChart <- renderPlotly({
    
    race_results <- laps_ds %>%
      inner_join(pilots_ds, by = "driverId") %>%
      inner_join(races_ds, by = "raceId") %>%
      filter(year==input$season) %>%
      mutate(fullname = paste(forename, surname, sep = " ")) %>%
      select(gp=name, round, fullname, lap, position, milliseconds) %>%
      mutate(position = factor(position, levels = c(as.character(1:20)))) %>%
      mutate(pretty_format = sprintf("%d:%02d.%03d",
                                     floor(milliseconds / 60000),
                                     floor((milliseconds %% 60000) / 1000),
                                     milliseconds %% 1000)) %>%
      filter(gp==input$track) %>%
      filter(fullname == input$pilot1 | fullname == input$pilot2)
    
    if(input$var == "Position"){
      
      chart <- plot_ly(race_results, x = ~lap, y = ~position, color = ~fullname, colors = "Set1",
                       type = "scatter", mode = "lines",
                       line = list(width = 1.5)) %>%
        layout(title = paste(input$track, " ", input$season ),
               xaxis = list(title = "Laps"),
               yaxis = list(title = "Position", autorange = "reversed"),
               showlegend = TRUE)
      
      
      chart
    }
    else{
      chart <- plot_ly(race_results, x = ~lap, y = ~pretty_format, color = ~fullname, colors = "Set1",
                       type = "scatter", mode = "lines",
                       line = list(width = 1.5)) %>%
        layout(title = paste(input$track, input$season ),
               xaxis = list(title = "Laps"),
               yaxis = list(title = "Lap time", autorange = "reversed"),
               showlegend = TRUE)
      
      
      chart
    }
  })
  
  #Make map here
  output$geograpyChart <- renderLeaflet({
    
    race_locations_2 <- unique(laps_ds[, c("raceId", "driverId")]) %>%
      inner_join(pilots_ds, by = "driverId") %>%
      inner_join(races_ds, by = "raceId") %>%
      inner_join(circuit_ds, by = "circuitId") %>%
      filter(year==input$season) %>%
      mutate(fullname = paste(forename, surname, sep = " ")) %>%
      select(fullname, lat, lng, name.y, country, year) %>%
      filter(fullname == input$pilot1 | fullname == input$pilot2 )
    
    
    # Filter the data and select relevant columns
    filtered_data <- race_locations_2 %>%
      select(lat, lng, name.y)
    
    # Create a leaflet map
    map <- leaflet(filtered_data) %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2)  # Set initial view of the map
    
    # Add markers for each track
    map <- map %>%
      addMarkers(
        lng = ~lng,
        lat = ~lat,
        popup = ~name.y
      )
    
    map
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)
