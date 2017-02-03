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
           tags$h4(textOutput("text8")),
           tags$h4(textOutput("text9"))
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
    
    url <- 'http://www.sportsbet.com.au/live-betting'
    
    html <- read_html(url)
    htmlNodes <- html_nodes(html,'#basketball-us a')
    
    HREFS <- html_attr(htmlNodes,'href')
    TITLES <- html_attr(htmlNodes,'title')
    
    to_remove <- c(which(is.na(TITLES)),grep('^Show all ',TITLES),grep(' more Markets$',TITLES))
    
    HREFS <- HREFS[-to_remove]
    TITLES <- TITLES[-to_remove]
    
    withProgress(message = 'Getting Data',value = 0, {
        source('scrape-script.R')
    })
    
    last_up <-format(Sys.time(),"%X")
    
    gameid <- as.character(input$game)
    
    game_data <- read.csv(paste0('Data/',gameid))
    
    game_data$GameTime <- gsub(" 1st"," Q1",game_data$GameTime)
    game_data$GameTime <- gsub("\\ 2nd"," Q2",game_data$GameTime)
    game_data$GameTime <- gsub(" 3rd", " Q3",game_data$GameTime)
    game_data$GameTime <- gsub(" 4th"," Q4",game_data$GameTime)
    game_data$GameTime <- gsub("1st ","H1 ",game_data$GameTime)
    game_data$GameTime <- gsub("2nd ","H2 ",game_data$GameTime)
    game_data$GameTime <- gsub('^:','0:',game_data$GameTime)
    game_data$GameTime <- gsub('\\.*','',game_data$GameTime)
    game_data$GameTime <- gsub('End','0:00 ',game_data$GameTime)
    game_data$GameTime <- gsub('Halftime','H1 0:00',game_data$GameTime)
    game_data$GameTime <- gsub('Half','0:00 Q2',game_data$GameTime)
    
    
    if(game_data$Type[1] == 'NCAA'){
      for(jj in 1:nrow(game_data)){
        g_ii <- strsplit(game_data$GameTime[jj]," ")[[1]]
        if(length(g_ii) > 1){
          game_data$GameTime[jj] <- paste(strsplit(game_data$GameTime[jj]," ")[[1]][2:1],collapse = " ")
        }
      }
    }
    
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

      for(ii in 1:nrow(points_df)){
        per <- strsplit(as.character(points_df$GameTime[ii])," - ")[[1]][1]
        min <-  as.character(as.numeric(gsub("m","",strsplit(as.character(points_df$GameTime)[ii]," - ")[[1]][2]))-1)
        
        if(length(intersect(grep(per,game_data$GameTime),grep(paste0("^",min,":"),game_data$GameTime))) > 0){
          points_ii <- game_data$Points[intersect(grep(per,game_data$GameTime),grep(paste0("^",min,":"),game_data$GameTime))]
          line_ii <- game_data$Line[intersect(grep(per,game_data$GameTime),grep(paste0("^",min,":"),game_data$GameTime))]
        } else{
          points_ii <- NA
          line_ii <- NA
        }
        
        points_df$Points[ii] <- points_ii
        points_df$Line[ii] <- line_ii
        
     }
      
     points_in_minute <- c()
     for(pp in 1:nrow(points_df)){
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
      
      for(ii in 1:nrow(points_df)){
        per <- strsplit(as.character(points_df$GameTime[ii])," - ")[[1]][1]
        min <-  as.character(as.numeric(gsub("m","",strsplit(as.character(points_df$GameTime)[ii]," - ")[[1]][2]))-1)
        
        if(length(intersect(grep(per,game_data$GameTime),grep(paste0("^",min,":"),game_data$GameTime))) > 0){
          points_ii <- game_data$Points[intersect(grep(per,game_data$GameTime),grep(paste0("^",min,":"),game_data$GameTime))]
          line_ii <- game_data$Line[intersect(grep(per,game_data$GameTime),grep(paste0("^",min,":"),game_data$GameTime))]
        } else{
          points_ii <- NA
          line_ii <- NA
        }
        
        points_df$Points[ii] <- points_ii
        points_df$Line[ii] <- line_ii
        
      }
      
      points_in_minute <- c()
      for(pp in 1:nrow(points_df)){
        points_in_minute[pp] <- points_df$Points[pp] - points_df$Points[pp-1]
      }
      
      points_df$PointsInMinute <- points_in_minute
      
      print('points')
    }
    
    points_df_na <- points_df
    
    output$plot1 <- renderPlot({
      if(length(which(!is.na(points_df_na$Line))) > 5){
        plot(points_df_na$Line,xlab = "Time",type = "o",ylab = "Line",col = 'red',axes = F)
        axis(1,at = 1:length(points_df_na$Line),labels = points_df_na$GameTime,las = T)
        axis(2,at = min(points_df_na$Line,na.rm = T):max(points_df_na$Line,na.rm = T),labels = min(points_df_na$Line,na.rm = T):max(points_df_na$Line,na.rm = T),las = T)
        
      } else{
        plot(1:10,type = "n",axes = F,xlab = "",ylab = "")
        text(5,7,paste("Not Enough Data Yet..",round(length(which(!is.na(points_df_na$Line))) / 5,digits = 2) * 100,"% Gathered."),cex = 1.2)
      }
    })
    output$plot2 <- renderPlot({
      if(length(which(!is.na(points_df_na$Line))) > 5){
        barplot(points_df_na$PointsInMinute,names.arg = gsub('m','',gsub(' - ','-',points_df_na$GameTime)),col = 'blue',ylab = "Points Per Minute",xlab = "Time")
      } else {
        plot(1:10,type = "n",axes = F,xlab = "",ylab = "")
        text(5,7,paste("Not Enough Data Yet..",round(length(which(!is.na(points_df_na$Line))) / 5,digits = 2) * 100,"% Gathered."),cex = 1.2)
      }
    })
    
    output$table <- renderTable(points_df)
    
    if(any(is.na(game_data$Line))){
      n_na <- length(which(is.na(game_data$Line)))
      game_data <- game_data[-which(is.na(game_data$Line)),]
    }
    
    curr_line <- game_data$Line[nrow(game_data)]
    curr_points <- game_data$Points[nrow(game_data)]
    curr_gametime <- game_data$GameTime[nrow(game_data)]
    
    if(length(curr_gametime) == 0 || length(curr_points) == 0 || length(curr_line) == 0){
      curr_line <- "NA"
      curr_points <- "NA"
      curr_gametime <- "NA"
      time.left <- "NA"
      mins.left <- "NA"
      max.points <- "NA"
      curr_ppm <- "NA"
      total_at_curr <- "NA"
      req_rate <- "NA"
    } else{
      if(game_data$Type[1] == 'NCAA'){
        mins.left <- strsplit(strsplit(curr_gametime," ")[[1]][1],':')[[1]][1]
        if(length(grep('H2',curr_gametime)) > 0){
          time.left <- as.numeric(mins.left)
        } else{
          time.left <- 20 + as.numeric(mins.left)
        }
      } else{
        mins.left <- strsplit(strsplit(curr_gametime," ")[[1]][1],':')[[1]][1]
        if(mins.left == "Half"){
          
        }
        if(length(grep('Q4',curr_gametime)) > 0){
          time.left <- as.numeric(mins.left)
        } else{
          if(length(grep('Q3',curr_gametime)) > 0){
            time.left <- 12 + as.numeric(mins.left)
          } else{
            if(length(grep('Q2',curr_gametime)) > 0){
              time.left <- 24 + as.numeric(mins.left)
            } else{
              if(length(grep('Q1',curr_gametime)) > 0){
                time.left <- 36 + as.numeric(mins.left)
              } 
            }
          }
        }
      }
      
      time.left <- as.numeric(time.left)
      
      max.points <- max(points_df$Points,na.rm = T)
      
      curr_ppm <-  round(max.points / max(which(points_df$Points == max.points)),digits = 2)
      total_at_curr <- round(curr_points + curr_ppm * time.left,digits = 0)
      req_rate <- round((curr_line-curr_points) / time.left,digits = 2)
    }

    output$text6 <- renderText({ 
      paste0("Game: ",strsplit(gameid," - ")[[1]][1])
    })
    output$text9 <- renderText({ 
      paste0("Minutes Left: ",time.left)
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

