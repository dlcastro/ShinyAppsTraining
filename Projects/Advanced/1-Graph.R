##########                      ####################
##########Daniel Lopes de Castro####################
##########Further Shiny-GGplot2####################
##########                     ####################

#in this chapter we're going to learn:
#A) Interactive graphic
#B) renderCachedPlot(), which speeds up your app by caching frequently used plots
#C) renderImage(), which allows you to display existing images

library(shiny)
library(ggplot2)



#A plot can respond to four different mouse20 events:
#click,
#dblclick (double click),
#hover (when the mouse stays in the same place for a little while),
#and brush (a rectangular selection tool).
#To turn these events into Shiny inputs,
#you supply a string to the corresponding plotOutput() argument,
#e.g. plotOutput("plot", click = "plot_click").
#This creates an input$plot_click that you can use to handle mouse clicks on the plot.

#Example with point click
ui <- basicPage(
    plotOutput("plot", click = "plot_click"),
    verbatimTextOutput("info")
)

server <- function(input, output) {
    output$plot <- renderPlot({
        plot(mtcars$wt, mtcars$mpg)
    }, res = 96)

    output$info <- renderPrint({
        req(input$plot_click)
        x <- round(input$plot_click$x, 2)
        y <- round(input$plot_click$y, 2)
        cat("[", x, ", ", y, "]", sep = "")
    })
}




#Example with nearPoint
ui <- fluidPage(
plotOutput("plot", click = "click"),
tableOutput("data")
)
server <- function(input, output, session) {
    output$plot <- renderPlot({
        plot(mtcars$wt, mtcars$mpg)
    }, res = 96)

    output$data <- renderTable({
        nearPoints(mtcars, input$click, xvar = "wt", yvar = "mpg")
    })
}



#more examples with nearPoint:
#If you use GGplot2 instead of R base plot,
#it won't be necessary to put values xvar and yvar
#since GGplot2 provides automatically
ui <- fluidPage(
    plotOutput("plot", click = "plot_click"),
    tableOutput("data")
)
server <- function(input, output, session) {
    output$plot <- renderPlot({
        ggplot(mtcars, aes(wt, mpg)) + geom_point()
    }, res = 96)

    output$data <- renderTable({
        nearPoints(mtcars, input$plot_click)
    })
}





shinyApp(ui, server)
