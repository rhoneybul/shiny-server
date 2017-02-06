library('rvest')

setwd("~/Desktop/RTH/Clients/Shiny-Server/Bet")

url <- 'http://www.sportsbet.com.au/live-betting'

html <- read_html(url)
htmlNodes <- html_nodes(html,'#basketball-us a')

HREFS <- html_attr(htmlNodes,'href')
TITLES <- html_attr(htmlNodes,'title')

to_remove <- c(which(is.na(TITLES)),grep('^Show all ',TITLES),grep(' more Markets$',TITLES))

HREFS <- HREFS[-to_remove]
TITLES <- TITLES[-to_remove]
  
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
  }
