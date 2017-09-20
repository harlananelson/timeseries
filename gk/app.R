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
   titlePanel("g&k distribution"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("A",
                     "A:",
                     min = 1,
                     max = 5,
                     step = 0.1,
                     value = 3),
         sliderInput("B",
                     "B:",
                     min=0,max=5,step=0.1,value=1),
         sliderInput("k",
                     "k:",
                     min=0,max=1,step=0.01,value=0.5)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("logPlot"),
         plotOutput("regPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   library(tidyverse)
   library('readxl')
   library(gk)
  my_data<-reactive({
      r<-rgk(5000,A=input$A,B=input$B,g=2,k=input$k)
  }) 
  output$logPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      ggplot() + aes(x=my_data()) + scale_x_log10() + geom_density() + ggtitle("Log Plot")
   })
    output$regPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      ggplot() + aes(x=my_data()) + geom_density()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

