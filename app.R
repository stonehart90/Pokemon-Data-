library(shiny)
library(tidyverse)
library(datasets)
pokemon_stats <- read_csv("pokemon data raw copy.csv")
#cleanData <- read_rds("Pokemon/y") 

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Pokemon Statistics"),
  textAreaInput("caption", "Application Background", "In this turn based game, Pokemon with the highest speed get to attack first, allowing for a swift advantage in battle. Pokemon with higher attack deal more damage with physical moves, while pokemon with higher special attack deal more damage with non-physical moves. Similar logic applies to defense and special defense, only they serve to help prevent a Pokemon's Hit Points (HP) from reaching zero and fainting. Do pay attention to outliers on the right. They often serve as an indication of a Legendary Pokemon, which are typically banned from competitve play. ", width = "1000px", height = "100px"),
  textAreaInput("caption", "Data Source", "These data come straight from publicly available Pokemon base stat totals."),
  textAreaInput("caption", "GitHub Repo", "https://github.com/stonehart90/Pokemon-Data-"),
  
  # Sidebar with a slider input for statistical category 
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
      textInput("name","Enter a Pokemon Name to See its Stat in Chosen Category (Case Sensitive):",""),
      mainPanel()
      
    ),
    #Above I divide the pokemon by generation so that players can better divide statistical distributions. 
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
      geom_histogram(color = "black", fill = "yellow", binwidth = .5, na.rm = TRUE, stat = "count") + 
      labs(aes_string(x = input$x),
           title = "Pokemon Stat Spread Count by Generation",
           caption = "Pokemon Stat Spread Count by Generation") 
    
    # draws the histogram to show count
    
  })
  
  output$namePlot <- renderPlot({
    #The following code allows a user to make a barplot for a Pokemon to see its specific stat. Notice the combintion of both aes and aes_string
    pokemon_stats %>% 
      filter(name == input$name) %>% 
      group_by(name) %>% 
     ggplot(aes(x = name, y = density(count))) + aes_string(y = input$x) + geom_col(color = "Black", fill = "Pink", binwidth =.05)
                        

  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

