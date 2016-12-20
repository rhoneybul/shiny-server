library(rmarkdown)

library(ggplot2)

con <- dbConnect(RSQLite::SQLite(),"Data/data.sqlite")

kwList <- dbGetQuery(con,paste0("select * from keywordList"))

keywords <- as.character(kwList$Keywords)

keywords <- strsplit(keywords,"\\,")

alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
                                 style="margin-left:auto;margin-right:auto;"
  )
}

for(ii in 1:length(keywords)){
  for(jj in 1:length(keywords[[ii]])){
    keywords[[ii]][jj] <- paste0(" ",keywords[[ii]][jj]," ")
  }
}

con <- dbConnect(RSQLite::SQLite(),"Data/data.sqlite")
commdb <- dbGetQuery(con,paste0('select * from FacebookConversation'))

comments_created <- commdb$COMMENT_CREATED

comments_created <- substr(comments_created,1,10)

commdb$COMMENT_CREATED <- comments_created

commdb <- commdb[order(commdb$COMMENT_CREATED),]

comments_created <- sort(unique(commdb$COMMENT_CREATED))

post_sources <- names(sort(table(commdb$POST_FROM),decreasing = TRUE))

db <- commdb

rmarkdown::render("report.Rmd",output_format = "pdf_document",output_file = "Data/Reports/Reports.pdf")

