##knitr::opts_chunk$set(echo = TRUE, error=TRUE, message=FALSE, warning=FALSE)


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

ui <- fluidPage(

    titlePanel("Lending Stack Model"),
    
    selectInput(
        inputId = "var",
        label = "Variable:",
        choices = list(funded_amt = "Funded Amount",
                       int_rate = "Interest Rate",
                       annual_inc = "Annual Income",
                       delinq_2yrs = "Delinquent after 2 Years",
                       inq_last_6mths = "Inquiries Last 6 Months",
                       revol_util = "Revolving Utilization Rate",
                       acc_now_delinq = "Number of Accounts now Delinquent",
                       open_il_6m = "Open Installments last 6 Months",
                       open_il_12m = "Open Installments last 12 Months",
                       open_il_24m = "Open Installments last 24 Months",
                       total_bal_il = "Total Balance of Installment Accounts",
                       all_util = "Balance to Credit Limit",
                       inq_fi = "Personal Finance Inquiries",
                       inq_last_12m = "Credit Inquiries last 12 Months",
                       delinq_amnt = "Past Due Amount Owed Delinquent Accounts",
                       num_il_tl = "Number of Installment Accounts",
                       total_il_high_credit_limit = "Total Installment Credit Limit"
                       ),
        plotOutput(outputId = "cpPlot")
    )
)

server <- function(input, output) {

    output$cpPlot <- renderPlot({
        obs <- lending_train %>% 
            slice_sample()
        obs_many <- obs %>%
            sample_n(size = 50, replace = TRUE)
        
        obs_many 
            
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
