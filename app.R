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
                                             
                                              #for team
                                              div(style = "width: 22rem",
                                                  selectInput("season",
                                                              "Season:",
                                                              choices = NULL),
                                                  selectInput("constructor1",
                                                              "First team:",
                                                              choices = NULL),
                                                  selectInput("constructor2",
                                                              "Second team:",
                                                              choices = NULL)
                                              ) 
                                              
                 )
                 ),
                 #forteam
                 dashboardBody(
                   tabItems (
                     tabItem(tabName = "GeneralComparison",
                             h2("General Comparison of Pilots"),
                             h4("The graph shows the comparison of the chosen teams in terms of their performance during the season."),
                             
                             div(style = "margin-top: 1rem;",
                                 fluidRow(
                                   
                                   
                                   box(
                                     title = textOutput("inputValue2"), status = "danger", solidHeader = TRUE,
                                     collapsible = TRUE,
                                     uiOutput("image2"),
                                     htmlOutput("inputValue2Name"),
                                     #htmlOutput("inputValue2Code"),
                                     #htmlOutput("inputValue2DOB"),
                                     htmlOutput("inputValue2Nationality")),
                                   
                                   box(
                                     title = textOutput("inputValue1"), status = "danger", solidHeader = TRUE,
                                     collapsible = TRUE,
                                     uiOutput("image1"),
                                     htmlOutput("inputValue1Name"),
                                     #htmlOutput("inputValue1Code"),
                                     #htmlOutput("inputValue1DOB"),
                                     htmlOutput("inputValue1Nationality")),
                                   
                                   box(
                                     title = "Performance Metrics for Constructor Comparison", status = "danger", solidHeader = TRUE,
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
                             h4("The graph shows the comparison of the chosen metric between two constructors for a chosen season."),
                             div(
                               style = "margin-top: 1rem;",
                               fluidRow(
                                 
                                 box(
                                   title = "Season standings", background = "black", solidHeader = TRUE,
                                   selectInput("season_stats",
                                               "Select metric here",
                                               choices = c("Position", "Points"))),
                                 
                                 box(
                                   title = "Performance During the Race", status = "danger", solidHeader = TRUE,
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
  #pilots_ds <- readr::read_csv("data/drivers.csv")
  results_ds <- readr::read_csv("data/results.csv")
  races_ds <- readr::read_csv("data/races.csv")
  #laps_ds <- readr::read_csv("data/lap_times.csv")
  #circuit_ds <- readr::read_csv("data/circuits.csv")
  
  constructors_ds <- readr::read_csv("data/constructors.csv")
  constructor_results_ds <- readr::read_csv("data/constructor_results.csv")
  constructor_standings_ds <- readr::read_csv("data/constructor_standings.csv")
  
  constructors_ds <- data.frame(constructors_ds) 
  #%>% rename("fullname" = name)
  constructor_results_ds<-data.frame(constructor_results_ds)
  constructor_standings_ds<-data.frame(constructor_standings_ds)
  
  observe({
    seasons_distinct <- distinct(seasons_ds, year) %>%
      arrange(desc(year)) %>%
      slice(-1)
    
    updateSelectInput(session, "season", choices = seasons_distinct$year)
  })
  
  #choose teams (constructors)
  observeEvent(input$season, {
    
    season_races <- races_ds %>%
      filter(year == input$season) %>%
      arrange(round) %>%
      select(name) %>%
      distinct()
    
    updateSelectInput(session, "track", choices = season_races$name)
    
    constructors_distinct <- constructors_ds %>%
      inner_join(constructor_standings_ds, by = "constructorId") %>%
      inner_join(races_ds, by = "raceId") %>%
      filter(year == input$season) %>%
      #mutate(fullname = paste(forename, surname, sep = " ")) %>%
      distinct() %>%
      arrange(fullname)
    
    shuffled_constructors <- sample(constructors_distinct$fullname)
    
    random_constructor1 <- shuffled_constructors[1]
    random_constructor2 <- shuffled_constructors[2]
    
    updateSelectInput(session, "constructor1", choices = constructors_distinct$fullname, selected = random_constructor1)
    updateSelectInput(session, "constructor2", choices = constructors_distinct$fullname, selected = random_constructor2)
  })
  
  
  output$gencomp <- renderPlotly({
    constructorStandings <- constructor_results_ds %>%
      inner_join(constructors_ds, by = "constructorId") %>%
      inner_join(constructor_standings_ds, by = c("constructorId","raceId","points")) %>%
      filter(year == input$season) %>%
      group_by(fullname) %>%
      mutate(wins = sum(position == 1)) %>%
      #mutate(poles = sum(grid == 1)) %>%
      #mutate(fastestLaps = sum(rank == 1)) %>%
      mutate(podiums = sum(position == 1 | position == 2 | position == 3)) %>%
      mutate(dnfs = sum(status == "D")) %>%
      mutate(pts = round(sum(points)/n_distinct(raceId), 1)) %>%
      filter(fullname == input$constructor1 | fullname == input$constructor2) %>%
      select(fullname, wins, pts, podiums, dnfs) %>%
      distinct
    
    trace1 <- list(
      name = constructorStandings[2,]$fullname,
      type = "bar",
      x = c(constructorStandings[2,]$wins, #constructors_standings[2,]$poles,
            constructorStandings[2,]$pts, #constructors_standings[2,]$fastestLaps, 
            constructorStandings[2,]$podiums, constructorStandings[2,]$dnfs),
      y = c("Wins", "Avg. Points", "Podiums", "DNF"),
      marker = list(color = "lightblue"),
      orientation = "h"
    )
    trace2 <- list(
      name = constructorStandings[1,]$fullname,
      type = "bar",
      x = c(constructorStandings[1,]$wins, #constructors_standings[1,]$poles,
            constructorStandings[1,]$pts, #constructors_standings[1,]$fastestLaps, 
            constructorStandings[1,]$podiums, constructorStandings[1,]$dnfs),
      y = c("Wins", "Avg. Points", "Podiums", "DNF"),
      marker = list(color = "red"),
      xaxis = "x2",
      yaxis = "y2",
      orientation = "h"
    )
    layout <- list(
      title = paste(input$constructor1, "vs", input$constructor2, input$season),
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
    
    page_link <- constructors_info %>% filter(fullname == input$constructor1) %>% pull(url)
    page <- read_html(page_link)
    
    image_url <- page %>%
      html_nodes("div#mw-content-text img") %>%
      html_attr("src") %>%
      .[1]
    
    tags$img(src = image_url, width = "300px")
  })
  
  output$image2 <- renderUI({
    
    page_link <- constructors_info %>% filter(fullname == input$constructor2) %>% pull(url)
    page <- read_html(page_link)
    
    image_url <- page %>%
      html_nodes("div#mw-content-text img") %>%
      html_attr("src") %>%
      .[1]
    
    tags$img(src = image_url, width = "300px")
  })
  
  
  output$inputValue1 <- renderText({ paste("Constructor 1: ", input$constructor1) })
  output$inputValue2 <- renderText({ paste("Constructor 2: ", input$constructor2) })
  
  
  constructors_info <- constructors_ds %>%
    #mutate(fullname = paste(forename, surname, sep = " ")) %>%
    #mutate(dob = format(dob, "%Y-%m-%d")) %>%
    arrange(fullname)
  
  
  output$inputValue1Name <- renderText({ paste("<b>Full Name: </b>", input$constructor1) })
  #output$inputValue1Code <- renderText({ paste("<b>Code: </b>", constructors_info %>% filter(name == input$constructor1) %>% select(constructorRef)) })
  #output$inputValue1DOB <- renderText({ paste("<b>DOB: </b>", constructors_info %>% filter(fullname == input$constructor1) %>% select(dob)) })
  output$inputValue1Nationality <- renderText({ paste("<b>Nationality: </b>", constructors_info %>% filter(fullname == input$constructor1) %>% select(nationality)) })
  
  output$inputValue2Name <- renderText({ paste("<b>Full Name: </b>", input$constructor2) })
  #output$inputValue2Code <- renderText({ paste("<b>Code: </b>", pilots_info %>% filter(fullname == input$pilot2) %>% select(code)) })
  #output$inputValue2DOB <- renderText({ paste("<b>DOB: </b>", pilots_info %>% filter(fullname == input$pilot2) %>% select(dob)) })
  output$inputValue2Nationality <- renderText({ paste("<b>Nationality: </b>", constructors_info %>% filter(fullname == input$constructor2) %>% select(nationality)) })
  
  
  
  output$seasonChart <- renderPlotly({
    season_constructor_results <- constructors_ds %>%
      inner_join(constructor_standings_ds, by = "constructorId") %>%
      inner_join(constructor_results_ds, by = c("constructorId","raceId","points")) %>%
      inner_join(races_ds, by = "raceId") %>%
      filter(year == input$season) %>%
      arrange(date) %>%
      filter(fullname == input$constructor1 | fullname == input$constructor2)
    
    if(input$season_stats=="Position") {
      season_constructor_results <-  season_constructor_results %>% select(name, fullname, positionText) %>%
        mutate(positionText = ifelse(positionText == "E", "DNF", positionText)) %>%
        #mutate(positionText = factor(position, levels = c(as.character(1:22), "DNF"))) %>%
        mutate(name = factor(name, levels = unique(name)))
      
      
      chart <- plot_ly(season_constructor_results, x = ~name, y = ~positionText, color = ~fullname, type = "scatter",
                       colors = "Set1", mode = "lines",
                       line = list(width = 1.5)) %>%
        layout(title = paste(input$season, "Constructor Positions"),
               xaxis = list(title = "Race"),
               yaxis = list(title = "Position", autorange = "reversed"),
               showlegend = TRUE)
    }
    else {
      season_constructor_results <-  season_constructor_results %>%  select(name, fullname, points) %>%
        mutate(name = factor(name, levels = unique(name))) %>%
        group_by(fullname) %>%
        mutate(points = cumsum(points))
      
      chart <- plot_ly(season_constructor_results, x = ~name, y = ~points, color = ~fullname, type = "scatter",
                       colors = "Set1", mode = "lines",
                       line = list(width = 1.5)) %>%
        layout(title = paste(input$season, "Constructor Standings"),
               xaxis = list(title = "Race"),
               yaxis = list(title = "Points"),
               showlegend = TRUE)
    }
    
    chart
  })
  
}


shinyApp(ui = ui, server = server)
