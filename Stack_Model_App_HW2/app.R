library(tidyverse)         # for graphing and data cleaning
library(tidymodels)        # for modeling
library(stacks)            # for stacking models
library(naniar)            # for analyzing missing values
library(lubridate)         # for date manipulation
library(moderndive)        # for King County housing data
library(vip)               # for variable importance plots
library(DALEX)             # for model interpretation
library(DALEXtra)          # for extension of DALEX
library(patchwork)         # for combining plots nicely
library(shiny)             # for creating a shiny app
library(rsconnect)


lending_stack <- readRDS("lending_final_stack")
data("lending_club")

ui <- fluidPage(

    titlePanel("Lending Stack Model Predictions"),
    
    sidebarLayout(
        sidebarPanel(
                sliderInput(inputId = "funded_amnt",
                            label = "Funded Amount",
                            min = 1000,
                            max = 40000,
                            value = 1000),
                sliderInput(inputId = "int_rate",
                            label = "Interest Rate",
                            min = 5.32,
                            max = 28.99,
                            value = 5.32),
                sliderInput(inputId = "annual_inc",
                            label = "Annual Income",
                            min = 0,
                            max = 960000,
                            value = 0),
                sliderInput(inputId = "delinq_2yrs",
                            label = "Delinquent after 2 Years",
                            min = 0,
                            max = 22,
                            value = 0),
                sliderInput(inputId = "inq_last_6mths",
                            label = "Inquiries Last 6 Months",
                            min = 0,
                            max = 5,
                            value = 0),
                sliderInput(inputId = "revol_util",
                            label = "Revolving Utilization Rate",
                            min = 0,
                            max = 144.3,
                            value = 0),
                sliderInput(inputId = "acc_now_delinq",
                            label = "Number of Accounts now Delinquent",
                            min = 0,
                            max = 2,
                            value = 0),
                sliderInput(inputId = "open_il_6m",
                            label = "Open Installments last 6 Months",
                            min = 0,
                            max = 32,
                            value = 0),
                sliderInput(inputId = "open_il_12m",
                            label = "Open Installments last 12 Months",
                            min = 0,
                            max = 20,
                            value = 0),
                sliderInput(inputId = "open_il_24m",
                            label = "Open Installments last 24 Months",
                            min = 0,
                            max = 30,
                            value = 0),
                sliderInput(inputId = "total_bal_il",
                            label = "Total Balance of Installment Accounts",
                            min = 0,
                            max = 585583,
                            value = 0),
                sliderInput(inputId = "all_util",
                            label = "Balance to Credit Limit",
                            min = 0,
                            max = 198,
                            value = 0),
                sliderInput(inputId = "inq_fi",
                            label = "Personal Finance Inquiries",
                            min = 0,
                            max = 15,
                            value = 0),
                sliderInput(inputId = "inq_last_12m",
                            label = "Credit Inquiries last 12 Months",
                            min = 0,
                            max = 32,
                            value = 0),
                sliderInput(inputId = "delinq_amnt",
                            label = "Past Due Amount Owed Delinquent Accounts",
                            min = 0,
                            max = 42428,
                            value = 0),
                sliderInput(inputId = "num_il_tl",
                            label = "Number Installment Accounts",
                            min = 0,
                            max = 82,
                            value = 0),
                sliderInput(inputId = "total_il_high_credit_limit",
                            label = "Total Installment Credit Limit",
                            min = 0,
                            max = 554119,
                            value = 0),
                selectInput(inputId = "term",
                            label = "Term",
                            choices = lending_club$term),
                selectInput(inputId = "sub_grade",
                            label = "Loan Subgrade",
                            choices = lending_club$sub_grade),
                selectInput(inputId = "addr_state",
                            label = "State",
                            choices = lending_club$addr_state),
                selectInput(inputId = "verification_status",
                            label = "Verification Status",
                            choices = lending_club$verification_status),
                selectInput(inputId = "emp_length",
                            label = "Employment Length",
                            choices = lending_club$emp_length),
                submitButton(text = "Create Plot")
        ),
        mainPanel(
            plotOutput(outputId = "cpPlot")
        )
    )
)

server <- function(input, output) {

    output$cpPlot <- renderPlot({
        obs <- lending_club %>% 
            slice_sample()
        obs_many <- obs %>%
            sample_n(size = 50, replace = TRUE)
        
        obs_many %>% 
            select(input) %>% 
            bind_cols(
                predict(lending_stack,
                        new_data = obs_many,
                        type = "prob")
            ) %>% 
            ggplot(aes(x = input,
                       y = .pred_bad)) +
            geom_point() +
            labs(y = "Predicted Class")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
