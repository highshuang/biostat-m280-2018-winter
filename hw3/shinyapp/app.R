

library(shiny)
library(tidyverse)
library(magrittr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("LA City Employee Payroll"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# import data 
payroll <- read_csv("/home/m280-data/la_payroll/LA_City_Employee_Payroll.csv")

#########################################
# view the data 
print(payroll, n = 2, width = Inf)
##########################################

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


#####################################
# view data
print(payroll_small, n = 20, width = Inf)

# check NA value 
payroll_small[is.na(payroll_small$healthCost),]

#####################################

# convert payroll_small to rds file and read in as payroll data set
saveRDS(payroll_small, "payroll.rds")
payroll <- readRDS("payroll.rds")

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 

shinyApp(ui = ui, server = server)

