#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(glue)
library(magrittr)
library(tidyquant)
library(shiny)

tq_get_options()

index_options     <- tq_index_options()
exchange_options  <- tq_exchange_options()
index_or_exchange <- list(index = "Index", exchange = "Exchange")
ie_options        <- list("Index" = index_options, "Exchange" = exchange_options)
ie_function        <- list("Index" = "tq_index", "Exchange" = "tq_exchange")

#ie_options[index_or_exchange[[1]]]
#ie_function["Index"]

#do.call(ie_function["Index"],list("RUSSELL1000"))

#do.call(as.character(ie_function["Index"]),list("RUSSELL1000"))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("tidyquant"),
   navbarPage(
     "Old Faithful", 
     tabPanel(
       "Tab 1",   
       # Sidebar with a slider input for number of bins   
       sidebarLayout(  
         sidebarPanel(   
           selectInput(
             "index_or_exchange", label= "Choose stock Index or Exchange: ", 
             choices = index_or_exchange, selected = index_or_exchange[[1]]), 
           uiOutput("ui_ei_selection") ,
           uiOutput("ui_stock_selection")
           ),   
         # Show a plot of the generated distribution  
         mainPanel(   
           textOutput("stock_choices")   
           )      
         )      
       ),  
     tabPanel(
       "Tab 2",  
       dataTableOutput("stock_list")  
       )  
     )      
   )

# Define server logic required to draw a histogram
server <- function(input, output) {   
  
  output$ui_ei_selection <- renderUI({ 
     selectInput("ie_selection", 
                 label   = "tq_index_options:",   
                 choices = ie_options[input$index_or_exchange], 
                 selected = ie_options[input$index_or_exchange]) 
   })
  
  stock_table <-  eventReactive(input$ie_selection,{
    do.call(as.character(ie_function[input$index_or_exchange]),list(input$ie_selection))
  })
  
   output$stock_list <- renderDataTable({
    stock_table()
  })
  
  stock_choices <- reactive({
    stock_table()  %>%  
      select(symbol,company) %$% 
      structure(as.character(symbol), names = as.character(company)) 
  })  
  
  output$stock_choices <- renderText({
    stock_choices()
  })

   output$ui_stock_selection <- renderUI({
         selectInput("stock_selection", label = "Select a Stock or Exchange", 
                    choices = stock_choices(), selected = stock_choices()[1])
     
   }) 
  }

#    structure(as.character(symbol), names = as.character(company)) 
#    select(symbol,company) 

# Run the application 
shinyApp(ui = ui, server = server)

