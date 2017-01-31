library(shiny)
library(rvest)

options(warn = -1)

if(!dir.exists('Data')){
  dir.create()
}

data.files <- list.files('Data/')

ui <- shinyUI(fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  
  # tags$h1('Over/Under Tracker',style = 'text-align:center'),
  tags$h4('Select Game',style = 'text-align:center;padding-top:20px'),
  HTML("<center>"),
  selectInput('game',label = NULL,choices = data.files),
  HTML("</center>"),
  HTML("<center style = padding-top:20px>"),
  actionButton("run", "Get Data & Update Graph",style= 'margin: 0 auto'),
  HTML("</center>") ,
  
  fluidRow(
    column(8,
           plotOutput("plot1")
           ),
    column(4,
           tags$h4(textOutput("text1"),style = "padding-top:70px;"),
           tags$h4(textOutput("text2")),
           tags$h4(textOutput("text3")),
           tags$h4(textOutput("text4")),
           tags$h4(textOutput("text5")),
           tags$h4(textOutput("text6")),
           tags$h4(textOutput("text7")),
           tags$h4(textOutput("text8"))
           )
  ),
  
  fluidRow(
    
    column(4,
          tableOutput('table')
    ),
    column(8,
            plotOutput('plot2')     
    )
  )
))

server <- shinyServer(function(input, output) {
  
  observeEvent(input$run, {
    
    last_up <-format(Sys.time(),"%X")
    
    gameid <- as.character(input$game)
    
    game_data <- read.csv(paste0('Data/',gameid))
    
    game_data$GameTime <- gsub("Half ","H",game_data$GameTime)
    
    if(game_data$Type[1] == 'NCAA'){
      college_times <- 1:20
      college_periods <- 1:2
      game_times <- c()
      for(jj in college_periods){
        for(ii in sort(college_times,decreasing = T)){
            game_times <- c(game_times,paste0('H',jj,' - ',college_times[ii],'m'))
        }
      }
      
      points_df <- data.frame(game_times,NA,NA,NA)
      colnames(points_df) <- c("GameTime","Points","PointsInMinute","Line")
      points_df$Points[1] <- 0
      points_df$Line[1] <- game_data$Line[1]
      for(ii in 2:nrow(points_df)){
        per <- strsplit(as.character(points_df$GameTime[ii])," - ")[[1]][1]
        min <-  as.character(as.numeric(gsub("m","",strsplit(as.character(points_df$GameTime)[ii]," - ")[[1]][2]))-1)
        
        if(length(intersect(grep(per,game_data$GameTime),grep(paste0(" ",min,":"),game_data$GameTime))) > 0){
          points_ii <- game_data$Points[intersect(grep(per,game_data$GameTime),grep(paste0(" ",min,":"),game_data$GameTime))]
          line_ii <- game_data$Line[intersect(grep(per,game_data$GameTime),grep(paste0(" ",min,":"),game_data$GameTime))]
        } else{
          points_ii <- NA
          line_ii <- NA
        }
        
        points_df$Points[ii] <- points_ii
        points_df$Line[ii] <- line_ii
        
      }
      
      points_in_minute <- 0
      for(pp in 2:nrow(points_df)){
        points_in_minute[pp] <- points_df$Points[pp] - points_df$Points[pp-1]
      }
      
      points_df$PointsInMinute <- points_in_minute
      
      points_df <<- points_df
      
      print('points')
      
    }
    if(game_data$Type[1] == "NBA"){
      game_times <- c()
      nba_times <- 1:12
      nba_periods <- 1:4
      for(jj in nba_periods){
        for(ii in sort(nba_times,decreasing = T)){
            game_times <- c(game_times,paste0('Q',jj,' - ',nba_times[ii],'m'))
        }
      }
      
      points_df <- data.frame(game_times,NA,NA,NA)
      colnames(points_df) <- c("GameTime","Points","PointsInMinute","Line")
      points_df$Points[1] <- 0
      points_df$Line[1] <- game_data$Line[1]
      for(ii in 2:nrow(points_df)){
        per <- strsplit(as.character(points_df$GameTime[ii])," - ")[[1]][1]
        min <-  as.character(as.numeric(gsub("m","",strsplit(as.character(points_df$GameTime)[ii]," - ")[[1]][2]))-1)
        
        if(length(intersect(grep(per,game_data$GameTime),grep(paste0(" ",min,":"),game_data$GameTime))) > 0){
          points_ii <- game_data$Points[intersect(grep(per,game_data$GameTime),grep(paste0(" ",min,":"),game_data$GameTime))]
          line_ii <- game_data$Line[intersect(grep(per,game_data$GameTime),grep(paste0(" ",min,":"),game_data$GameTime))]
        } else{
          points_ii <- NA
          line_ii <- NA
        }
        
        points_df$Points[ii] <- points_ii
        points_df$Line[ii] <- line_ii
        
      }
      
      points_in_minute <- 0
      for(pp in 2:nrow(points_df)){
        points_in_minute[pp] <- points_df$Points[pp] - points_df$Points[pp-1]
      }
      
      points_df$PointsInMinute <- points_in_minute
      
      print('points')
    }
    
    points_df_na <- points_df
    
    output$plot1 <- renderPlot({
      plot(points_df_na$Line,type = "l",xlab = "Time",ylab = "Line",col = 'red')
    })
    output$plot2 <- renderPlot({
      barplot(points_df_na$PointsInMinute,col = 'blue',ylab = "Points Per Minute",xlab = "Time")
    })
    
    output$table <- renderTable(points_df)
    
    if(any(is.na(game_data$Line))){
      n_na <- length(which(is.na(game_data$Line)))
      game_data <- game_data[-which(is.na(game_data$Line)),]
    }
    
    curr_line <- game_data$Line[nrow(game_data)]
    curr_points <- game_data$Points[nrow(game_data)]
    curr_gametime <- game_data$GameTime[nrow(game_data)]

    if(any(is.na(points_df$Line))){
      n_na <- length(which(is.na(points_df$Line)))
      points_df <- points_df[-which(is.na(points_df$Line)),]
      
      curr_ppm <- round(mean(points_df$PointsInMinute),digits = 2)
      
      total_at_curr <- round(curr_points + curr_ppm * n_na,digits = 0)
      req_rate <- round((curr_line-curr_points) / n_na,digits = 2)
    } else{
      curr_ppm <- round(mean(points_df$PointsInMinute),digits = 2)
      total_at_curr <- NA
      req_rate <- NA
    }
    
    
    
    # source('scrape-script.R')
    
    output$text6 <- renderText({ 
      paste0("Game: ",strsplit(gameid," - ")[[1]][1])
    })
    output$text1 <- renderText({ 
      paste0("Line: ", curr_line)
    })
    output$text2 <- renderText({ 
      paste0("Points: ", curr_points)
    })
    output$text3 <- renderText({ 
      paste0("Game Time: ", curr_gametime)
    })
    output$text4 <- renderText({ 
      paste0("Points Per Minute: ", curr_ppm)
    })
    output$text5 <- renderText({ 
      paste0("Last Updated: ",last_up)
    })
    output$text7 <- renderText({ 
      paste0("Proj Total: ",total_at_curr)
    })
    output$text8 <- renderText({ 
      paste0("Req. Points Per Minute: ",req_rate)
    })
  })


})

shinyApp(ui = ui, server = server)

