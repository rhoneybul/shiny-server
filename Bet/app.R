library(shiny)
library(rvest)

options(warn = -1)

if(!dir.exists('Data')){
  dir.create()
}

data.files <- list.files('Data/')

data_ii <- data.files

for(ii in 1:length(data_ii)){
  data_ii[ii] <- as.Date(gsub(".csv","",strsplit(data_ii[ii],"  -  ")[[1]][2]),'%a %b %d')
}

data_ii_df <- data.frame(data.files,data_ii)

data_ii_df <- data_ii_df[order(data_ii_df$data_ii,decreasing = T),]

data.files <- data_ii_df$data.files

ui <- shinyUI(fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  fluidRow(
    column(8,
           tags$h4('Select Game',style = 'padding-top:20px;text-align:center'),
           HTML("<center>"),
           selectInput('game',label = NULL,choices = data.files),
           HTML("</center>"),
           HTML("<center style = padding-top:20px>"),
           actionButton("run", "Get Data & Update Graph",style= 'margin: 0 auto'),
           HTML("</center>")
           ),
    column(4,
           tags$h4("Current Games",style = 'padding-top:20px;padding-bottom:20px;'),
           htmlOutput("gamesText")
           )
  ),
  
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
    
    output$gamesText <- renderText({
      titleText <- c()
      for(TITLE in TITLES){
        titleText <- c(titleText,paste0("<h6>",strsplit(TITLE,"  -  ")[[1]][1],"</h6>"))
      }
      titleText
    })
    
    withProgress(message = 'Getting Data',value = 0, {
      library('rvest')
      
      ## FUNCTIONS
      
      get_nba= function(){
        espn <- 'https://sports.yahoo.com/nba/scoreboard/'
        
        espn <- read_html(espn)
        espn <- html_nodes(espn,'span')
        scoreHTMLnba <- html_text(espn)
        
        scoreHTMLnba <- scoreHTMLnba[5:length(scoreHTMLnba)]
        scoreHTMLnba <- scoreHTMLnba[-which(scoreHTMLnba == "")]
        return(scoreHTMLnba)
      }
      
      get_ncaa = function(){
        ncaa <- 'http://jsonline.sportsdirectinc.com/sports-scores/College-Basketball-Scores-Matchups.aspx'
        
        ncaa <- read_html(ncaa)
        ncaa <- html_nodes(ncaa,'.sdi-so-title , .sdi-datahead-sub')
        scoreHTMLncaa <- html_text(ncaa)
        return(scoreHTMLncaa)
      }
      
      title_comp <- strsplit(gsub('\\s+$','',gsub(' At | - .*',' ',paste(TITLES,collapse = " ")))," ")[[1]]
      
      scoreHTMLnba <- get_nba()
      
      while(length(scoreHTMLnba) <= 1){
        print('nba failed once')
        scoreHTMLnba <- get_nba()
      }
      
      scoreHTMLncaa <- get_ncaa()
      while(length(scoreHTMLncaa) == 0){
        print('NCAA failed once')
        scoreHTMLncaa <- get_ncaa()
      }
      
      if(length(TITLES) != 0 || length(htmlNodes) != 0){
        
        overs = c()
        win_1 <- c()
        win_2 <- c()
        hand <- c()
        points = c()
        period = c()
        game_type <- c()
        game_time <- c()
        if(length(TITLES) > 0){
          for(tt in 1:length(TITLES)){
            
            prog_amount <- tt / length(TITLES)
            
            incProgress(amount = prog_amount)
            
            ## GET TIME ##
            
            title_comp_ii <- strsplit(gsub('\\s+$','',gsub(' At | - .*',' ',TITLES[tt]))," ")[[1]]
            
            game_type[tt] <- ''
            if(length(grep(paste(paste0("^",title_comp_ii,"$"),collapse = "|"),scoreHTMLnba)) > 0){
              
              game_type[tt] <- 'NBA'
              
              game_ii <- min(grep(paste(title_comp_ii,collapse = "|"),scoreHTMLnba))-1
              
              if(game_ii == 0){
                game_time[tt] <- 'NA'
              } else{
                game_time[tt] <- scoreHTMLnba[game_ii]
                
                if(game_time[tt] %in% title_comp || game_time[tt] == "Final" ){
                  game_time[tt] <- "NA"
                  
                }
              }
              
              if(!(grepl('[^0-9]',game_time[tt]))){
                game_time[tt] <- "NA"
              }
              
            } else{
              
              game_type[tt] <- 'NCAA'
              
              greps <- c()
              for(tit in title_comp_ii){
                greps <- c(greps,grep(tit,scoreHTMLncaa))
              }
              
              gTab <- table(greps)
              
              if(max(gTab) == 1 || length(greps) == 0){
                game_time[tt] <- "NA"
              } else{
                grep_ii <- names(gTab)[which(as.numeric(gTab) == max(as.numeric(gTab)))]
                
                game_time[tt] <- scoreHTMLncaa[as.numeric(grep_ii) + 1]
                
              }
              
              if(!(grepl('[^0-9]',game_time[tt]))){
                game_time[tt] <- "NA"
              }
            }
            
            page <- HREFS[tt]
            html <- read_html(page)
            htmlNodes <- html_nodes(html,'.title.left,.date')
            htmlTitles <- html_text(htmlNodes)
            
            if(length(as.numeric(html_text(html_nodes(html,'#total-pts_TEAM_2'))) + as.numeric(html_text(html_nodes(html,'#total-pts_TEAM_1')))) != 0){
              points[tt] <- as.character(as.numeric(html_text(html_nodes(html,'#total-pts_TEAM_2'))) + as.numeric(html_text(html_nodes(html,'#total-pts_TEAM_1'))))
            } else{
              if(length(as.numeric(html_text(html_nodes(html,'.team-score')))) != 0){
                points[tt] <- as.character(sum(as.numeric(html_text(html_nodes(html,'.team-score')))))
              } else{
                points[tt] <- 'NA'
              }
            }
            
            if(length(html_text(html_nodes(html,'#period_text'))) > 0){
              period[tt] <- gsub('^Quarter ','Q',html_text(html_nodes(html,'#period_text')))
            } else{
              period[tt] <- 'NA'
            }  
            
            over <- ""
            if("Total Points Scored" %in% htmlTitles || "Total Points" %in% htmlTitles){
              td <- html_nodes(html,'td')
              td_text <- html_text(td)
              td_over <- gsub('^Over ','',td_text[grep('^Over ',td_text)[1]])
              over <- td_over
            } else{
              over <- 'NA' 
            }
            overs[tt] <- over
            
            win_1[tt] <- ''
            win_2[tt] <- ''
            if('Match' %in% htmlTitles){
              td_match <- html_nodes(html,'td')
              if(length(td_match) != 0){
                match_text <- html_text(td_match)
                match_ii <- grep(paste(title_comp_ii,collapse = "|"),match_text,value = T)
                match_ii <- grep("\n",match_ii,value = T)
                
                w1 <- strsplit(match_ii[1]," ")[[1]]
                w2 <- strsplit(match_ii[2]," ")[[1]]
                win_1[tt] <- strsplit(w1[length(w1)],"\r")[[1]][1]
                win_2[tt] <- strsplit(w2[length(w2)],"\r")[[1]][1]
              }
            }
            
            hand[tt] <- ''
            if('Handicap Betting' %in% htmlTitles || 'Handicap' %in% htmlTitles){
              td <- html_nodes(html,'td')
              hand_text <- html_text(td)
              hand_ii <- grep('\\+',hand_text,value = T)
              hand_ii <- grep('\n',hand_ii,invert = T,value = T)
              hand_te <- gsub('\\+','',gsub('..* +','',hand_ii))[1]
              
              hand[tt] <- hand_te
            }
            
            print(tt)
          }
          
          TITLES <- paste0(TITLES,"  -  ",format(as.POSIXlt(Sys.time(), "Australia/Perth"),"%a %b %d"),'.csv')
          
          for(tt in 1:length(TITLES)){
            df_title <- data.frame(format(as.POSIXlt(Sys.time(), "Australia/Perth"),"%X"),game_time[tt],period[tt],points[tt],overs[tt],win_1[tt],win_2[tt],hand[tt],game_type[tt])
            colnames(df_title) <- c('Time','GameTime','Period','Points','Line','MatchAway','MatchHome','HandicapLine','Type')
            data.dir <- list.files('Data')
            if(!(TITLES[tt] %in% data.dir)){
              file.name <- paste0("Data/",TITLES[tt])
              if(length(grep('-',file.name)) > 1){
                file.name <- paste(strsplit(file.name,'-')[[1]][1:2],collapse = "-")
              }
              write.csv(df_title,file.name,row.names = F)
            } else{
              file.name <- paste0("Data/",TITLES[tt])
              df_tt <- read.csv(file.name)
              df_title <- rbind(df_tt,df_title)
              if(length(grep('-',file.name)) > 1){
                file.name <- paste(strsplit(file.name,'-')[[1]][1:2],collapse = "-")
              }
              write.csv(df_title,file.name,row.names = F)
            }
          }
        }
        
      } else{
        print("No US Games")
      }
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
    
    if(nrow(game_data) != 0){
      if(game_data$Type[1] == 'NCAA'){
        for(jj in 1:nrow(game_data)){
          g_ii <- strsplit(game_data$GameTime[jj]," ")[[1]]
          if(length(g_ii) > 1){
            game_data$GameTime[jj] <- paste(strsplit(game_data$GameTime[jj]," ")[[1]][2:1],collapse = " ")
          }
        }
      }
    }
    
    if(nrow(game_data) != 0) {
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
          
          points_df$Points[ii] <- max(points_ii)
          points_df$Line[ii] <- max(line_ii)
          
        }
        
        points_in_minute <- c()
        for(pp in 1:nrow(points_df)){
          points_in_minute[pp] <- points_df$Points[pp] - points_df$Points[pp-1]
        }
        
        points_df$PointsInMinute <- points_in_minute
        
        points_df <<- points_df
        
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
          
          points_df$Points[ii] <- max(points_ii)
          points_df$Line[ii] <- max(line_ii)
          
        }
        
        points_in_minute <- c()
        for(pp in 1:nrow(points_df)){
          points_in_minute[pp] <- points_df$Points[pp] - points_df$Points[pp-1]
        }
        
        points_df$PointsInMinute <- points_in_minute
        
      }
    }
    
    if(nrow(game_data != 0)){
      points_df_na <- points_df
    } else{
      points_df <- game_data
      points_df_na <- game_data
    }
    
    output$plot1 <- renderPlot({
      if(length(which(!is.na(points_df_na$Line))) >= 5){
        plot(points_df_na$Line,xlab = "Time",type = "o",ylab = "Line",col = 'red',axes = F)
        axis(1,at = 1:length(points_df_na$Line),labels = points_df_na$GameTime,las = T)
        axis(2,at = min(points_df_na$Line,na.rm = T):max(points_df_na$Line,na.rm = T),labels = min(points_df_na$Line,na.rm = T):max(points_df_na$Line,na.rm = T),las = T)
        
      } else{
        plot(1:10,type = "n",axes = F,xlab = "",ylab = "")
        text(5,7,paste("Not Enough Data Yet..",round(length(which(!is.na(points_df_na$Line))) / 5,digits = 2) * 100,"% Gathered."),cex = 1.2)
      }
    })
    output$plot2 <- renderPlot({
      if(length(which(!is.na(points_df_na$Line))) >= 5){
        barplot(points_df_na$PointsInMinute,names.arg = gsub('m','',gsub(' - ','-',points_df_na$GameTime)),col = 'blue',ylab = "Points Per Minute",xlab = "Time")
      } else {
        plot(1:10,type = "n",axes = F,xlab = "",ylab = "")
        text(5,7,paste("Not Enough Data Yet..",round(length(which(!is.na(points_df_na$Line))) / 5,digits = 2) * 100,"% Gathered."),cex = 1.2)
      }
    })
    
    output$table <- renderTable(points_df)
    
    if(any(is.na(game_data$Points))){
      n_na <- length(which(is.na(game_data$Points)))
      game_data <- game_data[-which(is.na(game_data$Points)),]
    }
    
    curr_line <- game_data$Line[nrow(game_data)]
    curr_points <- game_data$Points[nrow(game_data)]
    curr_gametime <- game_data$GameTime[nrow(game_data)]
    
    if((length(curr_gametime) == 0 || is.na(curr_gametime)) || (length(curr_points) == 0 || is.na(curr_points)) || (length(curr_line) ==0 || is.na(curr_gametime))){
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
          time.left <- 20
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

