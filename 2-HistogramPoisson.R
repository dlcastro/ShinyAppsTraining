library(shiny)

ui <- fluidPage(
    fluidRow(
        column(3,
               numericInput("lambda1", label = "lambda1", value = 3),
               numericInput("lambda2", label = "lambda2", value = 3),
               numericInput("n", label = "n", value = 1e5, min = 0)
        ),
        column(9, plotOutput("hist"))
    )
)

server <- function(input, output, session) {
    timer <- reactiveTimer(1000)

    x1 <- reactive({
        timer()
        rpois(input$n, input$lambda1)
    })
    x2 <- reactive({
        timer()
        rpois(input$n, input$lambda2)
    })

    output$hist <- renderPlot({
        histogram(x1(), x2(), binwidth = 1, xlim = c(0, 40))
    }, res = 96)
}
shinyApp(ui,server)
