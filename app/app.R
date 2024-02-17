library(shiny)

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("SF AirBnb Prices"),
    # Dropdown selection bar for neighborhoods
    selectInput("neighborhood", 
                label = "Select a neighborhood:",
                choices = c("Choose a neighborhood", unique(sf_airbnb_clean$neighbourhood_cleansed)),
                selected = "Choose a neighborhood"),
    # Dropdown selection bar for number of bathrooms
    selectInput("bathrooms", 
                label = "Select number of bathrooms:",
                choices = c("Choose a neighborhood", unique(sf_airbnb_clean$bathrooms_number)),
                selected = "Bathrooms")
    
)


server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
