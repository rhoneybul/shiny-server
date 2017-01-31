library('rvest')
library('RSelenium')

url <- 'http://www.sportsbet.com.au/live-betting'

html <- read_html(url)
htmlNodes <- html_nodes(html,'#basketball-us a')

if(length(htmlNodes) != 0){
  HREFS <- html_attr(htmlNodes,'href')
  TITLES <- html_attr(htmlNodes,'title')
  
  to_remove <- c(which(is.na(TITLES)),grep('^Show all ',TITLES),grep(' more Markets$',TITLES))
  
  HREFS <- HREFS[-to_remove]
  TITLES <- TITLES[-to_remove]
  
  title_comp <- strsplit(gsub('\\s+$','',gsub(' At | - .*',' ',paste(TITLES,collapse = " ")))," ")[[1]]
  
  espn <- 'https://sports.yahoo.com/nba/scoreboard/'
  
  espn <- read_html(espn)
  espn <- html_nodes(espn,'span')
  scoreHTMLnba <- html_text(espn)
  
  scoreHTMLnba <- scoreHTMLnba[5:length(scoreHTMLnba)]
  scoreHTMLnba <- scoreHTMLnba[-which(scoreHTMLnba == "")]
  
  ncaa <- 'http://jsonline.sportsdirectinc.com/sports-scores/College-Basketball-Scores-Matchups.aspx'
  
  ncaa <- read_html(ncaa)
  ncaa <- html_nodes(ncaa,'.sdi-so-title , .sdi-datahead-sub')
  scoreHTMLncaa <- html_text(ncaa)
  
  overs = c()
  points = c()
  period = c()
  game_times <- c()
  for(tt in 1:length(TITLES)){
    
    ## GET TIME ##
    
    title_comp_ii <- strsplit(gsub('\\s+$','',gsub(' At | - .*',' ',TITLES[tt]))," ")[[1]]
    
    if(length(grep(paste(title_comp_ii,collapse = "|"),scoreHTMLnba)) > 0){
      game_time[tt] <- scoreHTMLnba[min(grep(paste(title_comp_ii,collapse = "|"),scoreHTMLnba))-1]
      if(game_time[tt] %in% title_comp || game_time[tt] == "Final"){
        game_time[tt] <- "NA"
      }
    } else{
      
      greps <- c()
      for(tit in title_comp_ii){
        greps <- c(greps,grep(tit,scoreHTMLncaa))
      }
      
      if(max(gTab) == 1){
        game_time[tt] <- "NA"
      } else{
        grep_ii <- names(gTab)[which(as.numeric(gTab) == max(as.numeric(gTab)))]
        
        game_time[tt] <- scoreHTMLncaa[as.numeric(grep_ii) + 1]
        
      }
    }
    
    page <- HREFS[tt]
    html <- read_html(page)
    htmlNodes <- html_nodes(html,'.title.left')
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
      td_over <- gsub('Over ','',grep('^Over ',td_text,value = T)[1])
      over <- td_over
    } else{
      over <- 'NA' 
    }
    overs[tt] <- over
    
    print(tt)
  }
  
  TITLES <- paste(TITLES," - ",format(as.POSIXlt(Sys.time(), "America/New_York"),"%a %b %d"))
  
  for(tt in 1:length(TITLES)){
    df_title <- data.frame(format(as.POSIXlt(Sys.time(), "Australia/Perth"),"%X"),game_time[tt],period[tt],points[tt],overs[tt])
    colnames(df_title) <- c('Time','GameTime','Period','Points','Line')
    if(!(TITLES[tt] %in% gsub('.csv$','',list.files('Data')))){
      write.csv(df_title,paste0("Data/",TITLES[tt],".csv"),row.names = F)
    } else{
      df_tt <- read.csv(paste0('Data/',TITLES[tt],'.csv'))
      df_title <- rbind(df_tt,df_title)
      write.csv(df_title,paste0("Data/",TITLES[tt],".csv"),row.names = F)
    }
  }
} else{
  print("No US Games")
}

