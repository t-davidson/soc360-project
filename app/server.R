library(shiny)
library(tidyverse)
library(ggplot2)

data <- read_csv("../data/2018_gss_sample.csv")

server <- function(input, output) {

    files <- reactiveValues()

    observe(files$data <- data)

    output$correlation_plot <- renderPlot({

        ggplot(data=data, aes(x = files$data[[input$var_x]], y = files$data[[input$var_y]], color = files$data[[input$var_z]])) + geom_point() +
            labs(y = input$var_x, x = input$var_y) + theme_bw()

    }
                                          )

    output$correlation <- renderText({
        paste("Correlation coefficient:", round(cor(data[[input$var_x]], data[[input$var_y]])[1],2), sep=" ")
            }
        )
    output$data_head <- renderTable({data %>% group_by(sex) %>% summarize_all(mean)}, spacing = "xs")
}
