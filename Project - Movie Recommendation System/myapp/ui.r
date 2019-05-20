
################
library(shiny)

genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film.Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci.Fi", "Thriller", "War", "Western")

shinyUI(fluidPage(
  titlePanel("Movie Recommendation System"),
  fluidRow(
    
    column(4, h3("Select Movie Genres You Prefer (order matters):"),
           wellPanel(
             selectInput("input_genre", "Genre #1",
                         genre_list),
             selectInput("input_genre2", "Genre #2",
                         genre_list),
             selectInput("input_genre3", "Genre #3",
                         genre_list)
             #submitButton("Update List of Movies")
           )),
    
    column(4, h3("Select Movies You Like of these Genres:"),
           wellPanel(
             # This outputs the dynamic UI component
             uiOutput("ui"),
             uiOutput("ui2"),
             uiOutput("ui3")
             #submitButton("Get Recommendations")
           )),
    
    column(4,
           h3("You Might Like The Following Movies Too!"),
           tableOutput("table")
           #verbatimTextOutput("dynamic_value")
    )
  ),
  
  fluidRow(
  
  )
))