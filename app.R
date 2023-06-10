library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

# Define UI for application that draws a histogram
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
                  choices = NULL),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("General Comparison",
                 plotlyOutput("gencomp")),
        tabPanel("Races",
                 div(style = "margin-top: 1rem;",
          
                     fluidRow(
                       column(width = 3,
                              selectInput("track",
                                          "Track:",
                                          choices = NULL)),
                       column(width = 6,
                              selectInput("var",
                                          "Parameter:",
                                          choices = NULL))
                     ))
        ),
        tabPanel("Season standings",
                 selectInput("season_stats",
                             "Season standings:",
                             choices = c("Position", "Points")),
                 div(
                   style = "margin-top: 1rem;",
                   plotlyOutput("lineChart", height = "70rem")
                 )
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
  
  seasons_distinct <- distinct(seasons_ds, year) %>%
    arrange(desc(year)) %>%
    slice(-1)
  
  observe({
    updateSelectInput(session, "season", choices = seasons_distinct$year)
  })
  
  observeEvent(input$season, {
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
    
  })
  
  output$lineChart <- renderPlotly({
    season_results <- results_ds %>% 
      inner_join(races_ds, by = "raceId") %>%
      inner_join(pilots_ds, by = "driverId") %>% 
      filter(year == input$season) %>%
      mutate(fullname = paste(forename, surname, sep = " ")) %>% 
      arrange(date) %>%
      filter(fullname == input$pilot1 | fullname == input$pilot2 )
    
      if(input$season_stats=="Position") {
        season_results <-  season_results %>% select(name, fullname, position) %>%
        mutate(position = ifelse(position == "\\N", "DNF", position)) %>%
        mutate(position = factor(position, levels = c(as.character(1:20), "DNF"))) %>% 
        mutate(name = factor(name, levels = unique(name)))
        
      
        chart <- plot_ly(season_results, x = ~name, y = ~position, color = ~fullname, type = "scatter", mode = "lines", 
                       line = list(width = 3)) %>%
        layout(title = paste(input$season, "Pilot Positions"),
               xaxis = list(title = ""),
               yaxis = list(title = "", autorange = "reversed"))
      }
      else {
        season_results <-  season_results %>%  select(name, fullname, points) %>%
          mutate(name = factor(name, levels = unique(name))) %>% 
          group_by(fullname) %>%
          mutate(points = cumsum(points)) 
        
        chart <- plot_ly(season_results, x = ~name, y = ~points, color = ~fullname, type = "scatter", mode = "lines", 
                         line = list(width = 3)) %>%
          layout(title = paste(input$season, "Pilot Standings"),
                 xaxis = list(title = ""),
                 yaxis = list(title = ""))
      }

    
    chart <- ggplotly(chart) %>% layout(theme = theme_minimal())
    chart
  })
  
}

shinyApp(ui = ui, server = server)
