library(shiny)
library(rvest)
library(DT)

options(warn = -1)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
    actionButton("run", "Run Code")
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
    
    ii <- 1
    jj <- 1
    while(ii < 1000){
      if(as.numeric(format(Sys.time(),'%M')) < 40){
        url <- 'http://www.sportsbet.com.au/live-betting'
        
        html <- read_html(url)
        htmlNodes <- html_nodes(html,'td')
        htmlText <- html_text(htmlNodes)
        
        htmlText <- gsub('\n|\t|^\\s+','',htmlText[1:(grep('\r',htmlText)[1]-1)])
        htmlText <- htmlText[-which(htmlText == "")]
        
        df <- data.frame(htmlText[1:20],format(Sys.time(),'%X'))
        colnames(df) <- c('Entry','Time')
        
        write.csv(df,paste0("Data/",format(Sys.time(),'%X'),'.csv'),row.names = F) 
        print(jj)
        jj <- jj + 1
      } else{
        next()
      }
    }
    

})
# Run the application 
shinyApp(ui = ui, server = server)

