library(shiny)
library(ggplot2)



############################Functions#################


histogram <- function(x1, x2, binwidth = 0.1, xlim = c(-3, 3)) {
    df <- data.frame(
        x = c(x1, x2),
        g = c(rep("x1", length(x1)), rep("x2", length(x2)))
    )

    ggplot(df, aes(x, fill = g)) +
        geom_histogram(binwidth = binwidth) +
        coord_cartesian(xlim = xlim)
}

t_test <- function(x1, x2) {
    test <- t.test(x1, x2)

    sprintf(
        "p value: %0.3f\n[%0.2f, %0.2f]",
        test$p.value, test$conf.int[1], test$conf.int[2]
    )
}
##############################################




ui <- fluidPage(
    fluidRow(
        column(4,
               "Distribuição 1",
               numericInput("n1", label = "n (Tamanho da Amostra)", value = 1000, min = 1),
               numericInput("mean1", label = "µ (Média Amostral)", value = 0, step = 0.1),
               numericInput("sd1", label = "σ (Desvio Padrão)", value = 0.5, min = 0.1, step = 0.1)
        ),
        column(4,
               "Distribuição 2",
               numericInput("n2", label = "n (Tamanho da Amostra)", value = 1000, min = 1),
               numericInput("mean2", label = "µ (Média Amostral)", value = 0, step = 0.1),
               numericInput("sd2", label = "σ (Desvio Padrão)", value = 0.5, min = 0.1, step = 0.1)
        ),
        column(4,
               "Histogram",
               numericInput("binwidth", label = "Largura das caixas", value = 0.1, step = 0.1),
               sliderInput("range", label = "Alcance", value = c(-3, 3), min = -5, max = 5)
        )
    ),
    fluidRow(
        column(9, plotOutput("hist")),
        column(3, verbatimTextOutput("ttest"))
    )
)




######Server
server <- function(input, output, session) {
    x1 <- reactive(rnorm(input$n1, input$mean1, input$sd1))
    x2 <- reactive(rnorm(input$n2, input$mean2, input$sd2))

    output$hist <- renderPlot({
        histogram(x1(), x2(), binwidth = input$binwidth, xlim = input$range)
    }, res = 96)

    output$ttest <- renderText({
        t_test(x1(), x2())
    })
}

shinyApp(ui,server)

