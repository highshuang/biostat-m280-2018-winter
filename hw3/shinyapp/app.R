

library(shiny)
library(tidyverse)
library(magrittr)
library(ggplot2)

# import data 
payroll <- read_csv("/home/m280-data/la_payroll/LA_City_Employee_Payroll.csv")

# fix column names (replace space by underscore)
names(payroll) <- str_replace_all(names(payroll), " ", "_")

# change "Other_Pay_(Payroll_Explorer) to otherPay
names(payroll)[24] <- "otherPay"

# select target variables
payroll_small <- payroll %>%
  transmute(year = Year, department = Department_Title,
            job = Job_Class_Title,
            totalPay = abs(as.numeric(substr(Total_Payments, 2, 
                                             length(Total_Payments)))),
            basePay = abs(as.numeric(substr(Base_Pay, 2, length(Base_Pay)))),
            overtimePay = abs(as.numeric(substr(Overtime_Pay, 2, 
                                                length(Overtime_Pay)))),  # over time pay missing
            otherPay = abs(as.numeric(substr(otherPay, 2, 
                                             length(otherPay)))),
            totalCost = abs(as.numeric(substr(Average_Benefit_Cost, 2, 
                                              length(Average_Benefit_Cost)))),
            healthCost = abs(as.numeric(substr(Average_Health_Cost, 2, 
                                               length(Average_Health_Cost))))
  )


# convert payroll_small to rds file and read in as payroll data set
saveRDS(payroll_small, "payroll.rds")
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
                    value = 2017
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
                       "mean" = "mean"))
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
                     # Question6 Input: select job title 
                     selectInput(inputId = "job",
                                 label = "Select the job you interested:",
                                 choices = c("", "", ""))
                    
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
         summarize(meanTotal = mean(totalPay, na.rm = TRUE),
                   meanBase = mean(basePay, na.rm = TRUE),
                   meanOvertime = mean(overtimePay, na.rm = TRUE),
                   meanOther = mean(otherPay, na.rm = TRUE)) %>%
         arrange(desc(meanTotal)) %>%
         select(department, meanTotal, meanBase, meanOvertime, meanOther)
    
       head(data_Q4_mean, n = input$obs_Q4)
     }else {
       data_Q4_median <- payroll %>% filter(year == input$year_Q3) %>%
         group_by(department) %>%
         summarize(medianTotal = median(totalPay, na.rm = TRUE),
                   medianBase = median(basePay, na.rm = TRUE),
                   medianOvertime = median(overtimePay, na.rm = TRUE),
                   medianOther = median(otherPay, na.rm = TRUE)) %>%
         arrange(desc(medianTotal)) %>%
         select(department, medianTotal, medianBase, medianOvertime, medianOther)
       
       head(data_Q4_median, n = input$obs_Q4) 
       
     }
   })
   
   output$view_Q5 <- renderTable({
     data_Q5 <- payroll %>%
       filter(year == input$year_Q3) %>% 
       group_by(department) %>%
       arrange(desc(totalCost)) %>%
       select(department, totalCost, totalPay, basePay, overtimePay, otherPay)
     
     head(data_Q5, n = input$obs_Q4)
     
   })
}

# Run the application 

shinyApp(ui = ui, server = server)

