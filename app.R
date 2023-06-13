library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  
  titlePanel("F1 pilots comparison"),
  
  sidebarLayout(
    sidebarPanel(  
      selectInput("season",
                  "Season:", 
                  choices = NULL),
      selectInput("pilot1",
                  "First pilot:",
                  choices = NULL),
      selectInput("pilot2",
                  "Second pilot:",
                  choices = NULL)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("General Comparison",
                 div(style = "margin-top: 1rem;",
                     plotlyOutput("gencomp", height = "70rem"))
        ),
        tabPanel("Races",
                 div(style = "margin-top: 1rem;",
                     fluidRow(
                       column(width = 3,
                              selectInput("track",
                                          "Grand Prix",
                                          choices = NULL)),
                       column(width = 3,
                              selectInput("var",
                                          "Parameter",
                                          choices = c("Position", "Lap Time"),
                                          selected = "Position")),
                       column(width = 3,
                              selectInput("tab2ChartType",
                                          "Chart Type",
                                          choices = c("Line Chart", "Parallel Coordinates"),
                                          selected = "Line Chart")),
                       div(style = "margin-top: 2rem;",                       
                           column(width = 3,
                                  checkboxInput("tab2ShowAll",
                                                "Show Others",
                                                value = FALSE)
                           )
                       )
                     ),
                     plotlyOutput("circuitChart", height = "70rem")
                 )
        ),
        tabPanel("Season standings",
                 div(style = "margin-top: 1rem;",
                     fluidRow(
                       column(width = 3,
                              selectInput("season_stats",
                                          "Season standings:",
                                          choices = c("Position", "Points"))),

                       column(width = 3,
                              selectInput("tab3ChartType",
                                          "Chart Type",
                                          choices = c("Line Chart", "Parallel Coordinates"),
                                          selected = "Line Chart")),
                       div(style = "margin-top: 2rem;",                       
                           column(width = 3,
                                  checkboxInput("tab3ShowAll",
                                                "Show Others",
                                                value = FALSE)
                           )
                       )
                     )
                 ),
                 plotlyOutput("seasonChart", height = "70rem")
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
                   orientation=trace1$orientation, marker = trace1$marker, name = trace1$name)
    p <- add_trace(p, uid=trace2$uid, type=trace2$type, x=trace2$x, y=trace2$y, xaxis=trace2$xaxis,
                   yaxis=trace2$yaxis, marker = trace2$marker, name = trace2$name, orientation=trace2$orientation)
    p <- layout(p, title=layout$title, width=layout$width, xaxis=layout$xaxis, yaxis=layout$yaxis,
                height=layout$height, xaxis2=layout$xaxis2, yaxis2=layout$yaxis2, autosize=layout$autosize,
                showlegend=layout$showlegend)
    p
  })
  
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
      
      if(input$tab3ChartType == "Line Chart"){
      chart <- plot_ly(season_results, x = ~name, y = ~position, color = ~fullname, type = "scatter",
                       colors = "Set1", mode = "lines", 
                       line = list(width = 1.5)) %>%
        layout(title = paste(input$season, "Pilot Positions"),
               xaxis = list(title = ""),
               yaxis = list(title = "", autorange = "reversed"),
               showlegend = TRUE)
      }
      else{
      }
    }
    else {
      if(input$tab3ChartType == "Line Chart"){
      season_results <-  season_results %>%  select(name, fullname, points) %>%
        mutate(name = factor(name, levels = unique(name))) %>% 
        group_by(fullname) %>%
        mutate(points = cumsum(points)) 
      
      chart <- plot_ly(season_results, x = ~name, y = ~points, color = ~fullname, type = "scatter",
                       colors = "Set1", mode = "lines", 
                       line = list(width = 1.5)) %>%
        layout(title = paste(input$season, "Pilot Standings"),
               xaxis = list(title = ""),
               yaxis = list(title = ""),
               showlegend = TRUE)
      }
      else{
        
      }
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
               yaxis = list(title = "", autorange = "reversed"),
               showlegend = TRUE)
      
      
      chart
    }
    else{
      chart <- plot_ly(race_results, x = ~lap, y = ~pretty_format, color = ~fullname, colors = "Set1",
                       type = "scatter", mode = "lines", 
                       line = list(width = 1.5)) %>%
        layout(title = paste(input$track, input$season ),
               xaxis = list(title = "Laps"),
               yaxis = list(title = "", autorange = "reversed"),
               showlegend = TRUE)
      
      
      chart
    }
  })
  
  
}

shinyApp(ui = ui, server = server)
