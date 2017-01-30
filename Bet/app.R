library(shiny)
library(rvest)

options(warn = -1)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
    actionButton("run", "Run Code"),
    textOutput("text1")
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
    
    # ii <- 1
    # jj <- 1
    # while(ii < 1000){
    #   if(as.numeric(format(Sys.time(),'%M')) < 40){
    observeEvent(input$run, {
      
      
      
      output$text1 <- renderText({ 
        files <- list.files("Data")
        file <- read.csv(paste0('Data/',files[1]))
         
        as.character(file$Entry[3])
      })
      a = c(1,2,2)
      b = c(4,5,6)
      df <- data.frame(a,b)
      
      write.csv(df,paste0(format(Sys.time(),"%X"),'.csv'))
      
      # cat('Running Code')
      # url <- 'http://www.sportsbet.com.au/live-betting'
      # cat('Getting URL', file = stderr())
      # html <- read_html(url)
      # cat('Getting Nodes', file = stderr())
      # htmlNodes <- html_nodes(html,'td')
      # cat('Getting Text', file = stderr())
      # htmlText <- html_text(htmlNodes)
      # htmlText <- gsub('\n|\t|^\\s+','',htmlText[1:(grep('\r',htmlText)[1]-1)])
      # htmlText <- htmlText[-which(htmlText == "")]

      # df <- data.frame(htmlText[1:20],format(Sys.time(),'%X'))
      # colnames(df) <- c('Entry','Time')
      # cat('Writing CSV')
      # write.csv(df,paste0("Data/",format(Sys.time(),'%X'),'.csv'),row.names = F) 
      # cat("Finished")
    })
        
        # cat(jj)
        # jj <- jj + 1
    #   } else{
    #     next()
    #   }
    # }
    # 

})
# Run the application 
shinyApp(ui = ui, server = server)

