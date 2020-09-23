library(shiny)
library(DT)
library(vroom)
library(tidyverse)

#Data: National Electronic Injury Surveillance System (NEISS) - 2017

injuries <- vroom::vroom("injuries.tsv.gz")
products <- vroom::vroom("https://raw.githubusercontent.com/hadley/mastering-shiny/master/neiss/products.tsv")
population <- vroom::vroom("https://raw.githubusercontent.com/hadley/mastering-shiny/master/neiss/population.tsv")



##Resume function:
count_top <- function(df, var, n) {
    df %>%
        mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
        group_by({{ var }}) %>%
        summarise(n = as.integer(sum(weight)))
}


##UI

ui <- fluidPage(
    fluidRow(
        column(8,
               selectInput("code", "Produto", setNames(products$prod_code, products$title), width = "100%")
        ),
        column(2, selectInput("y","Y axis", c("rate", "count"))),
        column(2, numericInput("rows", "N Rows Table (+ Others)", value = 5))
    ),
    fluidRow(
        column(4, DT::dataTableOutput("diag")),
        column(4, DT::dataTableOutput("body_part")),
        column(4, DT::dataTableOutput("location")),
    ),
    fluidRow(
        column(12, plotOutput("age_sex"))
    ),
    fluidRow(
        column(2, actionButton("story", "Tell me a story")),
        column(10, textOutput("narrative"))
    )
)


##Server

server <- function(input, output, session) {
    selected <- reactive(injuries %>% filter(prod_code == input$code))
    rows <- reactive(input$rows)

    output$diag <- DT::renderDataTable(
        count_top(selected(), diag, input$rows), width = "100%"
    )
    output$body_part <- DT::renderDataTable(
        count_top(selected(), body_part, input$rows), width = "100%"
    )
    output$location <- DT::renderDataTable(
        count_top(selected(), location, input$rows), width = "100%"
    )

    summary <- reactive({
        selected() %>%
            count(age, sex, wt = weight) %>%
            left_join(population, by = c("age", "sex")) %>%
            mutate(rate = n / population * 1e4)
    })

    output$age_sex <- renderPlot({
        if (input$y == "count") {
            summary() %>%
                ggplot(aes(age, n, colour = sex)) +
                geom_line() +
                labs(y = "Estimated number of injuries")
        } else {
            summary() %>%
                ggplot(aes(age, rate, colour = sex)) +
                geom_line(na.rm = T) +
                labs(y = "Injuries per 10,000 people")
        }
    }, res = 96)

    output$narrative <- renderText({
        input$story
        selected() %>% pull(narrative) %>% sample(1)
    })
}

shinyApp(ui,server)


