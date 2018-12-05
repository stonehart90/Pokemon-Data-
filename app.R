

library(shiny)
library(tidyverse)
library(datasets)
pokemon_stats <- read_csv("pokemon data raw copy.csv")
#cleanData <- read_rds("Pokemon/y") 

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Pokemon Statistics"),
  textAreaInput("caption", "Beta Application Background", "In this turn based game, Pokemon with the highest speed get to attack first, allowing for a swift advantage in battle. Pokemon with higher attack deal more damage with physical moves, while pokemon with higher special attack deal more damage with non-physical moves. Similar logic applies to defense and special defense, only they serve to help prevent a Pokemon's Hit Points (HP) from reaching zero and fainting. Do pay attention to outliers on the right. They often serve as an indication of a Legendary Pokemon, which are typically banned from competitve play. ", width = "1000px"),
  textAreaInput("caption", "Data Source", "These data come straight from publicly available Pokemon base stat totals."),
  textAreaInput("caption", "GitHub Repo", "https://github.com/stonehart90/Pokemon-Data-"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "x", 
                  label = "Select a Statistical Category:",
                  choices = c("speed", "attack", "defense", "sp_defense", "sp_attack", "hp")),
      
      selectInput(inputId = "generation",
                  label = "Select a Generation to Count its Number of Pokemon per Stat:",
                  choices = c("1",
                              "2",
                              "3",
                              "4",
                              "5",
                              "6",
                              "7")),
      textInput("name","Enter a Pokemon Name (Coming Soon)",""),
      mainPanel()
      
    ),
    #Above I devide the pokemon by generation so that players can better divide statistical distributions. 
    #I also use "choices" to allow the stats I want to analyze to be displayed in the drop down menu.
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
    #Here I am developing a way to search for a specific Pokemon. This is a bonus feature for the most serious players. 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

