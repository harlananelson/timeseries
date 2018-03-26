#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Time Series Simulation"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("alpha",
                     "alpha:",
                     min = -10,
                     max = 10,
                     step = 0.01,
                     value = 0.05),
         sliderInput("rho",
                     "rho:",
                     min = -2,
                     max = 2,
                     step = 0.01,
                     value = 0.5),
          sliderInput("n",
                     "length of series:",
                     min = 100,
                     max = 10000,
                     step = 100,
                     value = 100)
 
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
          plotOutput("timePlot") 
          #,dataTableOutput('table')
          ,textOutput('test')
   
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {  
  library(quadprog)
  library(tseries)
  library(lubridate)
  library(tidyverse)  
  
  xt <-function(xt,epsilon,alpha,rho){   
    alpha + rho*xt  + epsilon   
    } 
     
  my_data <-reactive({   
   d<-tibble(x=1:input$n, e=rnorm(input$n))   %>%   
      mutate(y=accumulate(e,`xt`,alpha=input$alpha,rho=input$rho))  
    }) 
  output$test <- renderText({ 
    my_adf<-adf.test(my_data()$y, alternative="stationary", k=12)
    strwrap(c(my_adf$method,":    ",sprintf("%0.4f",my_adf$p.value)))
  })
  output$table <- renderDataTable({
    my_data()
  })
  output$timePlot <- renderPlot({ 
    # generate bins based on input$bins from ui.R 
    ggplot(my_data()) + aes(x=x,y=y) + geom_point() + ggtitle("Time Series") 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

