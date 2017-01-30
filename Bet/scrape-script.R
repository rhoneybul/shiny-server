library('rvest')

setwd("~/Desktop/RTH/Clients/Shiny-Server/Bet")

url <- 'http://www.sportsbet.com.au/live-betting'

html <- read_html(url)
htmlNodes <- html_nodes(html,'td')
htmlText <- html_text(htmlNodes)


htmlText <- gsub('\n|\t|^\\s+','',htmlText[1:(grep('\r',htmlText)[1]-1)])
htmlText <- htmlText[-which(htmlText == "")]

df <- data.frame(htmlText[1:20],format(Sys.time(),'%X'))
colnames(df) <- c('Entry','Time')

write.csv(df,paste0("Data/",format(Sys.time(),'%X'),'.csv'),row.names = F)
