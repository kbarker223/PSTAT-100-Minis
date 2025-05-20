#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Interactive Data Explorer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("variable", "Select Variable:",
                        choices=names(xxx)), ##update for our dataset
            selectInput("plotType", "Select Plot Type:",
                        choices = c("Scatter Plot", "Histogram", "Box Plot"))
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
      selected_data <- xxx ## change this for our dataset
      
      if (input$plotType == "Scatter Plot") {
        plot(selected_data[[input$variable]], selected_data[[1]],
             xlab=input$variable, ylab=names(xxx)[1],
             main=paste("Scatter Plot of", input$variable))
      } else if (input$plotType == "Histogram") {
        hist(selected_data[[input$variable]],
             xlab=input$variable, main=paste("Histogram of", input$variable),
             col="lightblue")
      } else if (input$plotType == "Box Plot") {
        boxplot(selected_data[[input$variable]] ~ selected_data[[1]],
                xlab="Group", ylab=input$variable,
                main=paste("Box Plot of", input$variable))
      }
    })
    
    output$summary <- renderPrint({
      summary(xxx[[input$variable]]) ## change for our dataset
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
