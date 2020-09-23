library(shiny)
library(DT)




##Resume function:
count_top <- function(df, var, n = 5) {
    df %>%
        mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
        group_by({{ var }}) %>%
        summarise(n = as.integer(sum(weight)))
}


##UI

ui <- fluidPage(
    fluidRow(
        column(6,
               selectInput("code", "Produto", setNames(products$prod_code, products$title))
        )
    ),
    fluidRow(
        column(4, DT::dataTableOutput("diag")),
        column(4, DT::dataTableOutput("body_part")),
        column(4, DT::dataTableOutput("location")),
    ),
    fluidRow(
        column(12, plotOutput("age_sex"))
    )
)


##Server

server <- function(input, output, session) {
    selected <- reactive(injuries %>% filter(prod_code == input$code))

    output$diag <- DT::renderDataTable(
        count_top(selected(), diag), width = "100%"
    )
    output$body_part <- DT::renderDataTable(
        count_top(selected(), body_part), width = "100%"
    )
    output$location <- DT::renderDataTable(
        count_top(selected(), location), width = "100%"
    )

    summary <- reactive({
        selected() %>%
            count(age, sex, wt = weight) %>%
            left_join(population, by = c("age", "sex")) %>%
            mutate(rate = n / population * 1e4)
    })

    output$age_sex <- renderPlot({
        summary() %>%
            ggplot(aes(age, n, colour = sex)) +
            geom_line() +
            labs(y = "Estimated number of injuries")
    }, res = 96)
}

shinyApp(ui,server)



injuries %>%
    mutate(diag = fct_lump(fct_infreq(diag), n = 5)) %>%
    group_by(diag) %>%
    summarise(n = as.integer(sum(weight)))
