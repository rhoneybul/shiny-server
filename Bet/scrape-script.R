library('rvest')

# url <- 'http://www.sportsbet.com.au/live-betting'
url <- "365/Index/index.html"

html <- read_html(url)
htmlNodes <- html_nodes(html,'#basketball-us a')

if(length(htmlNodes) == 0){
  stop('NO US GAMES')
}

HREFS <- html_attr(htmlNodes,'href')
TITLES <- html_attr(htmlNodes,'title')

to_remove <- c(which(is.na(TITLES)),grep('^Show all ',TITLES),grep(' more Markets$',TITLES))

HREFS <- HREFS[-to_remove]
TITLES <- TITLES[-to_remove]

overs = c()
points = c()
period = c()
for(tt in 1:length(TITLES)){
  
  page <- paste0('365/Games/',TITLES[tt],'.html')
  # page <- HREFS[tt]
  html <- read_html(page)
  htmlNodes <- html_nodes(html,'.title.left')
  htmlTitles <- html_text(htmlNodes)
  
  points[tt] <- as.numeric(html_text(html_nodes(html,'#total-pts_TEAM_2'))) + as.numeric(html_text(html_nodes(html,'#total-pts_TEAM_1')))
  period[tt] <- gsub('^Quarter ','Q',html_text(html_nodes(html,'#period_text')))
  
  over <- ""
  if("Total Points Scored" %in% htmlTitles){
    td <- html_nodes(html,'td')
    td_text <- html_text(td)
    td_over <- gsub('Over ','',grep('^Over ',td_text,value = T)[1])
    over <- td_over
  } else{
    over <- 'NA' 
  }
  overs[tt] <- over
}

TITLES <- paste(TITLES," - ",format(as.POSIXlt(Sys.time(), "America/New_York"),"%a %b %d"))

for(tt in 1:length(TITLES)){
  df_title <- data.frame(format(as.POSIXlt(Sys.time(), "Australia/Perth"),"%X"),period[tt],points[tt],overs[tt])
  colnames(df_title) <- c('Time','Period','Points','Line')
  if(!(TITLES[tt] %in% gsub('.csv$','',list.files('Data')))){
    write.csv(df_title,paste0("Data/",TITLES[tt],".csv"),row.names = F)
  } else{
    df_tt <- read.csv(paste0('Data/',TITLES[tt],'.csv'))
    df_title <- rbind(df_tt,df_title)
    write.csv(df_title,paste0("Data/",TITLES[tt],".csv"),row.names = F)
  }
}

