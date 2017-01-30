library(shiny)
library(rvest)

options(warn = -1)

if(!dir.exists('Data')){
  dir.create()
}

ui <- shinyUI(fluidPage(
   
    actionButton("run", "Run Code"),
    textOutput("text1")
    
))

server <- shinyServer(function(input, output) {
  
  observeEvent(input$run, {
    
    
    source('scrape-script.R')
    
    output$text1 <- renderText({ 
      paste('Finished',Sys.time())
    })
  })


})

shinyApp(ui = ui, server = server)

