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

##add an is custom column for our custom diamonds (see below), also rename x y z
diamonds <- diamonds %>%
  rename("length_mm" = "x",
         "width_mm"  = "y",
         "depth_mm"  = "z",
         "depth_pct"   = "depth") %>%
  mutate(isCustom = FALSE)

#we dont want to display isCustom as a variable option
variable_names <- names(diamonds)
variable_names <- variable_names[variable_names != "isCustom"]

##estimate price for custom diamonds
##determined best model in PSTAT 126 Final project
price_model <- lm(log(price) ~ log(carat) + cut + color + clarity + depth_pct + table + length_mm + width_mm + depth_mm, data=diamonds)



# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Interactive Data Explorer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select Variable:",
                  choices = setNames(
                    # we dont need the isCustom variable to be visualized
                    variable_names, 
                    paste0(tools::toTitleCase(variable_names), " (", sapply(diamonds[variable_names], function(x) class(x)[1]), ")")
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
    column(2, numericInput("depth_pct", "Depth (%)", value = 61.5, step = 0.1))
  ),
  fluidRow(
    column(2, numericInput("table", "Table", value = 55, step = 0.1)),
    column(2, numericInput("price", "Price", value = 326, step = 1)),
    column(2, numericInput("length_mm", "Length (mm)", value = 3.95, step = 0.01)),
    column(2, numericInput("width_mm", "Width (mm)", value = 3.98, step = 0.01)),
    column(2, numericInput("depth_mm", "Depth (mm)", value = 2.43, step = 0.01)),
    column(2, actionButton("add_diamond", "Add Diamond", class = "btn-danger"))
  ),
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  rv <- reactiveValues(data = diamonds)
  
  # Add a new diamond when the button is pressed
  observeEvent(input$add_diamond, {
    
    #store inputs in new_df so we can use to estimate price
    rv$data <- rv$data[!rv$data$isCustom, ]
    
    new_df <- tibble(
        carat      = input$carat,
        cut        = factor(input$cut,   levels = levels(rv$data$cut)),
        color      = factor(input$color, levels = levels(rv$data$color)),
        clarity    = factor(input$clarity, levels = levels(rv$data$clarity)),
        depth_pct = input$depth_pct,
        table      = input$table,
        length_mm = input$length_mm,
        width_mm  = input$width_mm,
        depth_mm  = input$depth_mm,
        isCustom   = TRUE
      )
    
    # add the data with the estimated price to the dataset (we use exp since its log price in the model)
    new_row <- tibble(
      carat      = input$carat,
      cut        = factor(input$cut,   levels = levels(rv$data$cut)),
      color      = factor(input$color, levels = levels(rv$data$color)),
      clarity    = factor(input$clarity, levels = levels(rv$data$clarity)),
      depth_pct = input$depth_pct,
      table      = input$table,
      price      = exp(predict(price_model, new_df)),
      length_mm = input$length_mm,
      width_mm  = input$width_mm,
      depth_mm  = input$depth_mm,
      isCustom   = TRUE
    )
    
    rv$data <- bind_rows(rv$data, new_row)
  })
  
  output$plot <- renderPlot({
    selected_data <- rv$data
      
      if (input$plotType == "Scatter Plot") {
        plot(selected_data[[input$variable]], ifelse(selected_data[[input$variable]] == selected_data[[1]], selected_data[[4]], selected_data[[1]]),
             xlab= input$variable, 
             ylab=variable_names[ifelse(input$variable == "carat", 4, 1)],
             main=paste("Scatter Plot of", tools::toTitleCase(input$variable)), 
             
             #custom diamonds should look different
             col =ifelse(selected_data$isCustom, "red", "black"))
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
      summary(rv$data[[input$variable]])
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
