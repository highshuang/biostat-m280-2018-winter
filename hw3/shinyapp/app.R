library(shiny)
library(tidyverse)
library(ggplot2)

# read in rds file
payroll <- readRDS("payroll.rds")

# remove all rows with NA and NaN values
payroll <- payroll[complete.cases(payroll), ]

# Question2 data: create the year-round pay data(long form)
pay <- payroll %>% select(year, basePay, overtimePay, otherPay) %>%
  group_by(year) %>%
  summarize(totalBase = sum(basePay, na.rm = TRUE),
         totalOvertime = sum(overtimePay, na.rm = TRUE),
         totalOther = sum(otherPay, na.rm = TRUE)) %>%
  gather(totalBase, totalOvertime, totalOther, key = "type", value = "value")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("LA City Employee Payroll"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        # Question3 Input: use slider to select the target year 
        sliderInput("year_Q3",
                    "Select a year :",
                    min = 2013,
                    max = 2017,
                    value = 2017, 
                    sep = ""
        ),
        
        # Question3 Input: number of rows to view individual
        numericInput(inputId = "obs_Q3",
                     label = "Number of observations to view info for 
                     individual:",
                     value = 10),
        
        # Question4 Input: number of rows to view depart
        numericInput(inputId = "obs_Q4",
                     label = "Number of observations to view info for 
                     department:",
                     value = 5),
        
        # Question4 Input: method(mean or median)
        radioButtons("method", "Choose either mean or median to visualize
                     the department earn most:",
                     c("median" = "median",
                       "mean" = "mean")),
        
        # Quesiton6 Input: text for job title
        textInput(inputId = "job_Q6",
                  label = "Type in the job you are interested 
                  about its annual salary:",
                  value = "Police Commander"),
        
        # Question6 Input: choose for adding density dist or not for hist in Q5
        radioButtons("density", "Visualize the histgram of annual salary 
                     with/without density curve",
                     c("density curve" = "with",
                       "no density curve" = "without"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         
         # Output: Tabset w plot, summary, and table ----
         tabsetPanel(type = "tabs",
                     # Question2 Output: barplot
                     tabPanel("Total Payroll Plot", plotOutput("barPlot_Q2")),
                     
                     # Question3 Output: table (person earn most)
                     tabPanel("Individual Earn Most", 
                              tableOutput("view_Q3")),
                     
                     # Question4 Output: table (depart earn most)
                     tabPanel("Department Earn Most", 
                              tableOutput("view_Q4")),
                     
                     # Question5 Output: table (depart cost most)
                     tabPanel("Department Cost Most", 
                              tableOutput("view_Q5")),
                     
                     # Question6 Output: hist for annual salary for given job
                     tabPanel("Dist for Annual Job Salary",
                              plotOutput("view_Q6"))
                     
         )
      )
   )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
   
   # Question2 output: stacked bar plot
   output$barPlot_Q2 <- renderPlot({
     ggplot(pay, aes(x = year, y = value / 1000000, fill = type)) +
       geom_col() +
       labs(
         x = "Year",
         y = "Pay (milloion$)",
         title = "LA City Employee Total Payroll")
   })
   
   output$view_Q3 <- renderTable({
     # create table for question 3
     data_Q3 <- payroll %>%
       filter(year == input$year_Q3) %>% 
       select(totalPay, basePay, overtimePay, otherPay, department, job) %>%
       arrange(desc(totalPay))
     head(data_Q3, n = input$obs_Q3)
   })
   
   output$view_Q4 <- renderTable({
     # create table for Question with method(median or mean)
     if(input$method == "mean") {
       data_Q4_mean <- payroll %>% filter(year == input$year_Q3) %>%
         group_by(department) %>%
         summarize(meanTotalPay = mean(totalPay, na.rm = TRUE),
                   meanBasePay = mean(basePay, na.rm = TRUE),
                   meanOvertimePay = mean(overtimePay, na.rm = TRUE),
                   meanOtherPay = mean(otherPay, na.rm = TRUE)) %>%
         arrange(desc(meanTotalPay)) %>%
         select(department, meanTotalPay, meanBasePay, meanOvertimePay, 
                meanOtherPay)
    
       head(data_Q4_mean, n = input$obs_Q4)
     }else {
       data_Q4_median <- payroll %>% filter(year == input$year_Q3) %>%
         group_by(department) %>%
         summarize(medianTotalPay = median(totalPay, na.rm = TRUE),
                   medianBasePay = median(basePay, na.rm = TRUE),
                   medianOvertimePay = median(overtimePay, na.rm = TRUE),
                   medianOtherPay = median(otherPay, na.rm = TRUE)) %>%
         arrange(desc(medianTotalPay)) %>%
         select(department, medianTotalPay, medianBasePay, medianOvertimePay, 
                medianOtherPay)
       
       head(data_Q4_median, n = input$obs_Q4) 
       
       
     }
   })
   
   output$view_Q5 <- renderTable({
     # create table for question5
     data_Q5 <- payroll %>%
       filter(year == input$year_Q3) %>% 
       group_by(department) %>%
       summarize(
         sumtotalCost = sum(totalCost),
         sumtotalPay = sum(totalPay),
         sumBasePay = sum(basePay),
         sumOvertimePay = sum(overtimePay),
         sumOtherPay = sum(otherPay)
       ) %>%
       arrange(desc(sumtotalCost)) %>%
       select(department, sumtotalCost, sumtotalPay, sumBasePay, sumOvertimePay,
              sumOtherPay)
     
     head(data_Q5, n = input$obs_Q4)
   })
   
   output$view_Q6 <- renderPlot({
     # create table with target job, elected year and totalPay 
     selectJob <- payroll %>% 
       filter(job == input$job_Q6, year == input$year_Q3) %>%
       select(year, job, totalPay)
     
     # add density curve if checkbox return TRUE
     if(input$density == "with") {
       ggplot(selectJob, aes(x = totalPay/1000)) +
         geom_histogram(aes(y = ..density..), fill = "white", color = "black") +
         geom_density(alpha = 0.2, fill = "#FF6666") +
         labs(
           title = "histgram for annual salary of interested job",
           y = "Probability",
           x = "Salary (thousand$)"
         )
       
     }else {
       ggplot(selectJob, aes(x = totalPay / 1000)) +
         geom_histogram(aes(y = ..density..), fill = "white", color = "black") +
         labs(
           title = "histgram for annual salary of interested job",
           y = "Probability",
           x = "Salary (thousand$)"
         )
     }
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

