#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(readr)
library(dplyr)

## set a seed for sampling sake
set.seed(05282025)
diamonds_data <- read.csv("diamonds.csv", stringsAsFactors=TRUE)
diamonds_data <- diamonds_data[, -c(1)]

# we will take a sample of 5000 for runtimes sake
diamonds <- sample_n(diamonds_data, size=5000, replace=FALSE)

##explore the data 
summary(diamonds)



# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Interactive Data Explorer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select Variable:",
                  choices = setNames(
                    names(diamonds), 
                    paste0(tools::toTitleCase(names(diamonds)), " (", sapply(diamonds, function(x) class(x)[1]), ")")
                  )),
      selectInput("plotType", "Select Plot Type:",
                  choices = c("Scatter Plot", "Histogram (Requires Numeric Data)", "Box Plot"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput("summary")
    )
  ),
  
  hr(),
  
  h3("Add New Diamond"),
  fluidRow(
    column(2, numericInput("carat", "Carat", value = 0.23, step = 0.01)),
    column(2, selectInput("cut", "Cut", choices = c("Ideal", "Premium", "Good", "Very Good", "Fair"))),
    column(2, selectInput("color", "Color", choices = c("E", "I", "J", "H", "G", "F", "D"))),
    column(2, selectInput("clarity", "Clarity", choices = c("SI2", "SI1", "VS1", "VS2", "VVS1", "VVS2", "I1"))),
    column(2, numericInput("depth", "Depth", value = 61.5, step = 0.1))
  ),
  fluidRow(
    column(2, numericInput("table", "Table", value = 55, step = 0.1)),
    column(2, numericInput("price", "Price", value = 326, step = 1)),
    column(2, numericInput("x", "X", value = 3.95, step = 0.01)),
    column(2, numericInput("y", "Y", value = 3.98, step = 0.01)),
    column(2, numericInput("z", "Z", value = 2.43, step = 0.01)),
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot <- renderPlot({
      selected_data <- diamonds
      
      if (input$plotType == "Scatter Plot") {
        plot(selected_data[[input$variable]], selected_data[[1]],
             xlab=input$variable, ylab=names(diamonds)[1],
             main=paste("Scatter Plot of", tools::toTitleCase(input$variable)))
      } else if (input$plotType == "Histogram (Requires Numeric Data)") {
        hist(selected_data[[input$variable]],
             xlab=input$variable, main=paste("Histogram of", tools::toTitleCase(input$variable)),
             col="lightblue")
      } else if (input$plotType == "Box Plot") {
        segments <- if (is.numeric(selected_data[[1]]) && length(unique(selected_data[[1]])) > 10) {
          cut(selected_data[[1]], breaks = 5, include.lowest = TRUE)
        } else {
          as.factor(selected_data[[1]])
        }
        boxplot(selected_data[[input$variable]] ~ segments,
                xlab = "Group", ylab = input$variable,
                main = paste("Box Plot of", tools::toTitleCase(input$variable)))
      }
    })
    
    output$summary <- renderPrint({
      summary(diamonds[[input$variable]]) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
