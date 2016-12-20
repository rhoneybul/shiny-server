all.comments <- db.i$COMMENT

comments <- paste(" ",paste(all.comments,collapse = " ")," ")
comments <- gsub("[[:punct:]]|[0-9]","",comments)
comments <- tolower(comments)

comments_string <- paste0(" ",comments," ")
comments <- strsplit(comments," ")

## MAKE THE KEYWORD TABLE
kw.tab <- table(comments)
kw.tab <- as.data.frame(kw.tab)
kw.tab$comments <- gsub("\n","",kw.tab$comments)

## REMOVE STOPWORDS
stopwords <- gsub("[[:punct:]]","",stopwords("en"))
kw.remove <- c()
for(ii in 1:length(stopwords)){
  if(any(kw.tab$comments == stopwords[ii])){
    kw.remove <- c(kw.remove,which(kw.tab$comments == stopwords[ii]))
  }
}

kw.tab <- kw.tab[-kw.remove,]

## MATCH THE SPECIFIED WORDS AND PHRASES

count <- c()
for(ii in 1:length(keywords)){
  count[ii] <- 0
  for(aa in 1:length(keywords[[ii]])){
    count[ii] <- count[ii] + str_count(comments_string,as.character(keywords[[ii]][aa]))
  }
}

countdf <- data.frame(count,kwList$Placeholder)
colnames(countdf) <- c("kwCount","kw")

countdf <- countdf[order(-countdf$kwCount),]

if(any(countdf$kwCount == 0)){
  countdf <- countdf[-which(countdf$kwCount == 0),]
}
colnames(countdf) <- c("Frequency","Theme")

countdf <- countdf[1:20,]

## MAKE DF, ORDER AND WRITE
kw.tab <- kw.tab[order(-kw.tab$Freq),]

kw.tab$comments <- gsub("\\s+","",kw.tab$comments)
kw.tab <- kw.tab[-which(kw.tab$comments == ""),]

kw.tab <- kw.tab[1:20,]

colnames(kw.tab) <- c("Comment","Frequency")

substr(kw.tab$Comment,1,1) <- toupper(substr(kw.tab$Comment,1,1))