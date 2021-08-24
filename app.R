library(shinyWidgets)
library(shiny)
library(highcharter)
source("functions.R")
library("shinythemes")

ui <- fluidPage(theme = shinytheme("united"),
    titlePanel("Compound Interest Calculator"),
    sidebarLayout(
        sidebarPanel(
            radioGroupButtons(
                inputId = "fxInput",
                label = h3("Currency:"), 
                choices = c("£", "$", "€", "₹", "¥"),
                status = "primary"
            ), 
            numericInput("startInput", h3("Initial Balance:"),
                         value=0, min=0, max=1000000, step=500
            ),
            numericInputIcon(
                inputId = "pctInput",
                label = h3("Interest Rate:"),
                value = 0,
                icon = icon("percent"),
                step=0.5
            ), 
            selectInput("timeInput", h3("Time Period:"),
                        choices=list("yearly" = 1, "monthly" = 2)),
            numericInputIcon(
                inputId = "yearInput",
                label = h3("Years:"),
                value = 0,
                step = 1
            ),
            numericInputIcon(
                inputId = "monthInput",
                label = h3("Months:"),
                value = 0,
                step = 1
            ),
            selectInput("compoundInput", h3("Compound Interval:"),
                        choices=list("Yearly (1/yr)"= 1, "Monthly (12/yr)" = 12),
                        selected=1),
            numericInput("depositInput", h3("Deposit Amount:"),
                         value=0, min=0, max=1000000, step=100
            ),
            selectInput("depositfreqInput", h3("Frequency:"),
                        choices=list("yearly" = 1, "monthly" = 2)),
            actionButton("run", "Run Calculation", icon("calculator")),
            
        ),
        mainPanel(column(12,
            tableOutput("balance"),
            highchartOutput("balanceplot"),)
        )
    ))

server <- function(input, output) {
    compounds <- reactive(get_number_of_compounds(input$compoundInput))
    deposits <- reactive(get_number_of_compounds(input$depositfreqInput))
    time <-  reactive(get_time_intervals(input$timeInput, input$yearInput, input$monthInput))
    result <- eventReactive(input$run, {get_interest_summary(input$startInput, input$pctInput / 100, compounds(), time(), input$depositInput)})
    output$balance <- renderTable(result()[['df']], colnames = TRUE)
    output$balanceplot <-renderHighchart({highchart() %>% 
            hc_chart(type = "column") %>% 
            hc_title(text="Compound Interest Calculator") %>%
            hc_xAxis(title = list(text = "Years")) %>% 
            hc_plotOptions(column = list(
                dataLabels = list(enabled = FALSE),
                stacking = "normal",
                enableMouseTracking = TRUE)
            ) %>% 
            hc_series( list(name="Total Interest",data=result()[['df']]$`Cum. Interest`, color="red"),
                       list(name="Total Deposits",data=result()[['df']]$`Cum. Deposits`, color="green"),
                       list(name="Initial Deposit",data=result()[['df']]$`Initial Deposit`, color="grey")
            )})
}    
shinyApp(ui = ui, server = server)
