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
diamonds_data <- read.csv("diamonds.csv", stringsAsFactors=TRUE)
diamonds_data <- diamonds_data[, -c(1)]

# we will take a sample of 5000 for runtimes sake
diamonds <- sample_n(diamonds_data, size=5000, replace=FALSE)

# Define UI for application that draws a histogram
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
        boxplot(selected_data[[input$variable]] ~ selected_data[[1]],
                xlab="Group", ylab=input$variable,
                main=paste("Box Plot of", tools::toTitleCase(input$variable)))
      }
    })
    
    output$summary <- renderPrint({
      summary(diamonds[[input$variable]]) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
