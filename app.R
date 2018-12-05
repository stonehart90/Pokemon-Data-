

library(shiny)
library(tidyverse)
library(datasets)
pokemon_stats <- read_csv("pokemon data raw copy.csv")
#cleanData <- read_rds("Pokemon/y") 

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Pokemon Statistics"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "x", 
                  label = "Select a Stat:",
                  choices = c("speed", "attack", "defense", "sp_defense", "sp_attack", "hp")),
      
      selectInput(inputId = "generation",
                  label = "generation:",
                  choices = c("1",
                              "2",
                              "3",
                              "4",
                              "5",
                              "6",
                              "7")),
      textInput("name","Enter Pokemon Name (Coming Soon)",""),
      mainPanel()
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tab",
                  tabPanel("Pokemon by the Numbers",plotOutput("generationPlot")),
                  tabPanel("Pokemon Search", plotOutput("namePlot"))
      )
    )
  )
)

#Define server logic required to draw a histogram


server <- function(input, output) {
  
  output$generationPlot <- renderPlot({
    
    pokemon_stats %>% 
      filter(generation == input$generation) %>% 
      

      ggplot(aes_string(x = input$x)) +
      geom_histogram(binwidth = .5, na.rm = TRUE, stat = "count") + 
      labs(aes_string(x = input$x),
           title = "Pokemon Stat Spread Count by Generation",
           caption = "Pokemon Stat Spread Count by Generation") 
    
    # draw the histogram with the specified number of bins
    
  })
  
  output$namePlot <- renderPlot({
  
  what <- pokemon_stats 
    filter(name == input$name) %>% 
     select(speed, attack, defense, hp, sp_attack, sp_defense)
      ggplot(what,aes(x = pokemon_stats, y = count) + geom_bar())
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

