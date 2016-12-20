library(shiny)
library(RSQLite)
library(ggplot2)
library(plotly)
library(tm)
library(stringr)

options(warn = -1)

#setwd("Desktop/RTH/Clients/MoreThanIdeas/aboriginal-attitudes/app/aboriginal-attitudes")

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

# Define UI for application that draws a histogram
ui <- navbarPage("Aboriginal Attitudes",
    
    tabPanel("Time Series & Top Comment",
             fluidPage(
               
               tags$head(
                 tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
               ),
               
               fluidRow(
                 column(3,
                        img(src='morethanideas.jpeg'),
                        style = "height:70px"
                 )
               ),
               
               tags$hr(),
               
               tags$h1("View The Conversation On Aboriginal Australians",style = "text-align: center;padding-bottom:1%"),
               
               sidebarLayout(
                 sidebarPanel( 
                   tags$h3("Select Variables",style = "text-align: center"),
                   tags$hr(),
                   fluidRow(
                     column(6,
                            tags$h5("Period Starting",style = 'text-align:center'),
                            selectInput('startPeriod',label = NULL,choices = comments_created)
                     ),
                     
                     column(6,
                            tags$h5("Period Ending",style = 'text-align:center'),
                            selectInput('endPeriod',label = NULL,choices = rev(comments_created))
                     )
                   ),
                   fluidRow(
                     column(12,
                            tags$h5("Source",style = 'text-align:center'),
                            selectInput('source',label = NULL,choices = c("All Sources",post_sources))
                     )
                   ),
                   tags$br(),
                   fluidRow(
                     column(12,
                            tags$div(actionButton("viewButton",label = "View the Conversation!"),style = 'text-align:center')
                     )
                   )
                 ),
                 mainPanel(
                   tags$h2("Time Series of Discussion Frequency",style = "text-align:center"),
                   fluidRow(
                     column(12, align = 'center',
                            plotlyOutput("plot")
                     )
                   ),
                   
                   br(),
                   br(),
                   br(),
                   
                   tags$h2("Top Comments For This Period",style = "text-align:center"),
                   fluidRow(
                     column(12,
                            dataTableOutput('commentsTable')
                     )
                   )
                 )
               )
             )
    ),
    tabPanel("Event Analysis",
             
             tags$head(
               tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
             ),
             
             fluidRow(
               column(3,
                      img(src='morethanideas.jpeg'),
                      style = "height:70px"
               )
             ),
             
             tags$hr(),
             
             tags$div(id = "eventTitle", fluidRow(
               column(12,
                      h1("Choose A Particular Event To Analyse!",style = "text-align:center;color:#504f51")

               )
             )),
             
             tags$div(bootstrapPage(
               
               alignCenter(selectInput("eventSelect",label = NULL,choices = c("Pauline Hanson Comments","Linda Burney Elected","Close the Gap Report"))),
               tags$div(actionButton("eventAnalyse",label = "View the Analysis!"),style = 'text-align:center'),
               
               fluidRow(
                 column(4,
                        tags$h3("Week Before"),
                        plotlyOutput('event1')
                        ),
                 column(4,
                        tags$h3("On Day of Event"),
                        plotlyOutput('event2')
                        ),
                 column(4,
                        tags$h3("Week After"),
                        plotlyOutput('event3')
                        )
               ),
               
               fluidRow(
                 column(12,
                        plotlyOutput("eventTimeSeries")
                        )
               ),
               
               fluidRow(
                 column(6,
                        tags$h4("Top Posts",style = "text-align:center;font-weight:200"),
                        dataTableOutput('eventsTable1')
                        ),
                 column(6,
                        tags$h4("Top Comments",style = "text-align: center;font-weight:200"),
                        dataTableOutput('eventsTable2')
                        )
               )
               
             ))
             
    ),
    tabPanel("Keyword & Theme Analysis",
             
             fluidPage(
               
               tags$head(
                 tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
               ),
               
               fluidRow(
                 column(3,
                        img(src='morethanideas.jpeg'),
                        style = "height:70px"
                 )
               ),
               
               tags$hr(),
               
               tags$h1("View The Conversation On Aboriginal Australians",style = "text-align: center;padding-bottom:1%"),
               
             sidebarLayout(
               sidebarPanel( 
                 tags$h3("Select Variables",style = "text-align: center"),
                 tags$hr(),
                 fluidRow(
                   column(6,
                          tags$h5("Period Starting",style = 'text-align:center'),
                          selectInput('startPeriod2',label = NULL,choices = comments_created)
                   ),
                   
                   column(6,
                          tags$h5("Period Ending",style = 'text-align:center'),
                          selectInput('endPeriod2',label = NULL,choices = rev(comments_created))
                   )
                 ),
                 fluidRow(
                   column(12,
                          tags$h5("Source",style = 'text-align:center'),
                          selectInput('source2',label = NULL,choices = c("All Sources",post_sources))
                   )
                 ),
                 tags$br(),
                 fluidRow(
                   column(12,
                          tags$div(actionButton("viewButton2",label = "View the Conversation!"),style = 'text-align:center')
                   )
                 )
               ),
               mainPanel(
                 tags$h2("Most Common Keywords For This Period",style = "text-align:center"),
                 fluidRow(
                   column(12, align = 'center',
                          plotlyOutput("plot2")
                   )
                 ),
                 tags$h2("Most Common Themes For This Period",style = "text-align:center"),
                 fluidRow(
                   column(12, align = 'center',
                          plotlyOutput("plot3")
                   )
                 )
               )
             )
             )
             
    )
    # tabPanel("Download Reports",
    #          
    #          fluidPage(
    #            
    #            tags$head(
    #              tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    #            ),
    #            
    #            fluidRow(
    #              column(3,
    #                     img(src='morethanideas.jpeg'),
    #                     style = "height:70px"
    #              )
    #            ),
    #            
    #            tags$hr(),
    #            
    #            tags$h1("Download Analytics pdf Report For 2016",style = "text-align: center;padding-bottom:1%"),
    #            
    #            tags$div(actionButton("downloadButton",label = "Download Report!"),style = 'text-align:center')
    #            
    #          )
    # )
    
    
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output,session) {
  
  output$plot <- renderPlotly({
    df <- data.frame()
    ggplot(df) + geom_point() + theme_bw() + theme(panel.grid.major = element_blank()) + theme(axis.title = element_blank()) + theme(axis.text = element_blank()) + theme(axis.ticks = element_blank()) + theme(panel.border = element_blank()) + xlim(0, 10) + ylim(0, 100) + ggplot2::annotate("text",x = 5,y = 50,label = "Choose a time period to see the time series analysis!",size = 10)
  })
  output$plot2 <- renderPlotly({
    df <- data.frame()
    ggplot(df) + geom_point() + theme_bw() + theme(panel.grid.major = element_blank()) + theme(axis.title = element_blank()) + theme(axis.text = element_blank()) + theme(axis.ticks = element_blank()) + theme(panel.border = element_blank()) + xlim(0, 10) + ylim(0, 100) + ggplot2::annotate("text",x = 5,y = 50,label = "Choose a time period to see the most common keywords!",size = 10)
  })
  output$plot3 <- renderPlotly({
    df <- data.frame()
    ggplot(df) + geom_point() + theme_bw() + theme(panel.grid.major = element_blank()) + theme(axis.title = element_blank()) + theme(axis.text = element_blank()) + theme(axis.ticks = element_blank()) + theme(panel.border = element_blank()) + xlim(0, 10) + ylim(0, 100) + ggplot2::annotate("text",x = 5,y = 50,label = "Choose a time period to see the most common themes!",size = 10)
  })

  observeEvent(input$viewButton,{
    
    start_date <- input$startPeriod
    end_date <- input$endPeriod
    
    print(start_date)
    print(end_date)

    dbs.i <- min(which(start_date == commdb$COMMENT_CREATED))
    dbe.i <- max(which(end_date == commdb$COMMENT_CREATED))

    db <- commdb[dbs.i:dbe.i,]
    
    post_source = input$source
    
    if(post_source != "All Sources"){
      db <- db[which(db$POST_FROM == post_source),]
    }
    
    date_frequencies <- as.data.frame(table(db$COMMENT_CREATED))
    colnames(date_frequencies) <- c("Date","Frequency")

    output$plot <- renderPlotly({
      p <- ggplot(data=date_frequencies, aes(x = Date, y = Frequency, group=1)) +
        geom_line(colour="#42B874", size=0.5) +
        geom_point(colour = "#504f51",size = 0.3) +
        theme(axis.ticks.x = element_blank(),axis.text.x = element_blank())
      
      ggplotly(p)
    })
    
    ## GET TOP COMMENTS
    
    dbComments <- db[order(as.numeric(as.character(db$COMMENT_LIKES)),decreasing = TRUE),]
    dbComments <- dbComments[1:10,c("COMMENT","COMMENT_LIKES","COMMENT_CREATED")]
    colnames(dbComments) <- c("Comment","Number of Likes","Comment Date")
    
    output$commentsTable <- renderDataTable({dbComments},options = list(columnDefs = list(list(width = '700px', targets = 0))))
    
  })
  
  observeEvent(input$viewButton2,{

    start_date <- input$startPeriod2
    end_date <- input$endPeriod2
    
    print(start_date)
    print(end_date)
    
    dbs.i <- min(which(start_date == commdb$COMMENT_CREATED))
    dbe.i <- max(which(end_date == commdb$COMMENT_CREATED))

    db <- commdb[dbs.i:dbe.i,]
    
    print(nrow(db))

    post_source = input$source

    if(post_source != "All Sources"){ db <- db[which(db$POST_FROM == post_source),] }
    
    print(nrow(db))

    ## PROCESS THE COMMENTS
    
    all.comments <- db$COMMENT
    
    print(length(all.comments))
    
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

    ## MAKE DF, ORDER AND WRITE
    kw.tab <- kw.tab[order(-kw.tab$Freq),]

    kw.tab$comments <- gsub("\\s+","",kw.tab$comments)
    kw.tab <- kw.tab[-which(kw.tab$comments == ""),]

    kw.tab <- kw.tab[1:20,]

    colnames(kw.tab) <- c("Comment","Frequency")

    substr(kw.tab$Comment,1,1) <- toupper(substr(kw.tab$Comment,1,1))
    
    output$plot2 <- renderPlotly({
      p <- ggplot(data=kw.tab, aes(x = Comment, y = Frequency, group=1)) +
        geom_bar(stat = "identity",colour = "#42B874",fill = "#42B874") +
        theme(axis.ticks.x = element_blank()) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
        theme_bw()

      ggplotly(p)
    })
    
    output$plot3 <- renderPlotly({
      p <- ggplot(data=countdf, aes(x = Theme, y = Frequency, group=1)) +
        geom_bar(stat = "identity",colour = "#42B874",fill = "#42B874") +
        theme(axis.ticks.x = element_blank()) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      theme_bw()
      
      ggplotly(p)
    })
    
  })
  
  
  output$event2 <- renderPlotly({
    df <- data.frame()
    ggplot(df) + geom_point() + theme_bw() + theme(panel.grid.major = element_blank()) + theme(axis.title = element_blank()) + theme(axis.text = element_blank()) + theme(axis.ticks = element_blank()) + theme(panel.border = element_blank()) + xlim(0, 10) + ylim(0, 100) + ggplot2::annotate("text",x = 5,y = 50,label = "Choose an event to analyse!",size = 10)
  })
  
  observeEvent(input$eventAnalyse,{
    
    if(input$eventSelect == "Pauline Hanson Comments"){
      date <- "2016-11-28"
    }
    if(input$eventSelect == "Linda Burney Elected"){
      date <- "2016-08-30" 
    }
    if(input$eventSelect == "Closing the Gap Report"){
      date <- "2016-02-10"
    }
    
    db <- commdb
    
    db.before <- db[grep(paste0(seq(as.Date(date) - 8,as.Date(date) - 1,"days"),collapse = "|"),db$COMMENT_CREATED),]
    db.day <- db[which(db$COMMENT_CREATED == date),]
    db.after <- db[grep(paste0(seq(as.Date(date) + 1,as.Date(date) + 8,"days"),collapse = "|"),db$COMMENT_CREATED),]
    db.period <- db[grep(paste0(seq(as.Date(date) - 8,as.Date(date) + 8,"days"),collapse = "|"),db$COMMENT_CREATED),]
    
    period_frequencies <- as.data.frame(table(db.period$COMMENT_CREATED))
    colnames(period_frequencies) <- c("Date","Frequency")
    
    ### ===============
    
    ## BEFORE 
    
    ## ================
    
    before.comments <- db.before$COMMENT
    
    comments <- paste(" ",paste(before.comments,collapse = " ")," ")
    comments <- gsub("[[:punct:]]|[0-9]","",comments)
    comments <- tolower(comments)
    
    comments_string <- paste0(" ",comments," ")
    comments <- strsplit(comments," ")
    
    ## MAKE THE KEYWORD TABLE
    kw.tab.before <- table(comments)
    kw.tab.before <- as.data.frame(kw.tab.before)
    kw.tab.before$comments <- gsub("\n","",kw.tab.before$comments)
    
    ## REMOVE STOPWORDS
    stopwords <- gsub("[[:punct:]]","",stopwords("en"))
    kw.remove <- c()
    for(ii in 1:length(stopwords)){
      if(any(kw.tab.before$comments == stopwords[ii])){
        kw.remove <- c(kw.remove,which(kw.tab.before$comments == stopwords[ii]))
      }
    }
    
    kw.tab.before <- kw.tab.before[-kw.remove,]
    
    kw.tab.before <- kw.tab.before[order(-kw.tab.before$Freq),]
    
    kw.tab.before$comments <- gsub("\\s+","",kw.tab.before$comments)
    kw.tab.before <- kw.tab.before[-which(kw.tab.before$comments == ""),]
    
    kw.tab.before <- kw.tab.before[1:20,]
    
    colnames(kw.tab.before) <- c("Comment","Frequency")
    
    substr(kw.tab.before$Comment,1,1) <- toupper(substr(kw.tab.before$Comment,1,1))
    
    output$event1 <- renderPlotly({
      p <- ggplot(data=kw.tab.before, aes(x = Comment, y = Frequency, group=1)) +
        geom_bar(stat = "identity",colour = "#42B874",fill = "#42B874") +
        theme(axis.ticks.x = element_blank()) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      ggplotly(p)
    })
    
    ## =========
    
    ##   DAY
    
    ## =========
    
    day.comments <- db.day$COMMENT
    
    comments <- paste(" ",paste(day.comments,collapse = " ")," ")
    comments <- gsub("[[:punct:]]|[0-9]","",comments)
    comments <- tolower(comments)
    
    comments_string <- paste0(" ",comments," ")
    comments <- strsplit(comments," ")
    
    ## MAKE THE KEYWORD TABLE
    kw.tab.day <- table(comments)
    kw.tab.day <- as.data.frame(kw.tab.day)
    kw.tab.day$comments <- gsub("\n","",kw.tab.day$comments)
    
    ## REMOVE STOPWORDS
    stopwords <- gsub("[[:punct:]]","",stopwords("en"))
    kw.remove <- c()
    for(ii in 1:length(stopwords)){
      if(any(kw.tab.day$comments == stopwords[ii])){
        kw.remove <- c(kw.remove,which(kw.tab.day$comments == stopwords[ii]))
      }
    }
    
    kw.tab.day <- kw.tab.day[-kw.remove,]
    
    kw.tab.day <- kw.tab.day[order(-kw.tab.day$Freq),]
    
    kw.tab.day$comments <- gsub("\\s+","",kw.tab.day$comments)
    kw.tab.day <- kw.tab.day[-which(kw.tab.day$comments == ""),]
    
    kw.tab.day <- kw.tab.day[1:20,]
    
    colnames(kw.tab.day) <- c("Comment","Frequency")
    
    substr(kw.tab.day$Comment,1,1) <- toupper(substr(kw.tab.day$Comment,1,1))
    
    output$event2 <- renderPlotly({
      p <- ggplot(data=kw.tab.day, aes(x = Comment, y = Frequency, group=1)) +
        geom_bar(stat = "identity",colour = "#42B874",fill = "#42B874") +
        theme(axis.ticks.x = element_blank()) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    })
    
    ## ========================
    ##           AFTER
    ## ========================
    
    after.comments <- db.after$COMMENT
    
    comments <- paste(" ",paste(after.comments,collapse = " ")," ")
    comments <- gsub("[[:punct:]]|[0-9]","",comments)
    comments <- tolower(comments)
    
    comments_string <- paste0(" ",comments," ")
    comments <- strsplit(comments," ")
    
    ## MAKE THE KEYWORD TABLE
    kw.tab.after <- table(comments)
    kw.tab.after <- as.data.frame(kw.tab.after)
    kw.tab.after$comments <- gsub("\n","",kw.tab.after$comments)
    
    ## REMOVE STOPWORDS
    stopwords <- gsub("[[:punct:]]","",stopwords("en"))
    kw.remove <- c()
    for(ii in 1:length(stopwords)){
      if(any(kw.tab.after$comments == stopwords[ii])){
        kw.remove <- c(kw.remove,which(kw.tab.after$comments == stopwords[ii]))
      }
    }
    
    kw.tab.after <- kw.tab.after[-kw.remove,]
    
    kw.tab.after <- kw.tab.after[order(-kw.tab.after$Freq),]
    
    kw.tab.after$comments <- gsub("\\s+","",kw.tab.after$comments)
    kw.tab.after <- kw.tab.after[-which(kw.tab.after$comments == ""),]
    
    kw.tab.after <- kw.tab.after[1:20,]
    
    colnames(kw.tab.after) <- c("Comment","Frequency")
    
    substr(kw.tab.after$Comment,1,1) <- toupper(substr(kw.tab.after$Comment,1,1))
    
    output$event3 <- renderPlotly({
      p <- ggplot(data=kw.tab.after, aes(x = Comment, y = Frequency, group=1)) +
        geom_bar(stat = "identity",colour = "#42B874",fill = "#42B874") +
        theme(axis.ticks.x = element_blank()) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    })
    
    period_frequencies <- as.data.frame(table(db.period$COMMENT_CREATED))
    colnames(period_frequencies) <- c("Date","Frequency")
    
    output$eventTimeSeries <- renderPlotly({
      p <- ggplot(data=period_frequencies, aes(x = Date, y = Frequency, group=1)) +
        geom_line(colour="#42B874", size=0.5) +
        geom_point(colour = "#504f51",size = 0.3) +
        theme(axis.ticks.x = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    })
    
    dbCommentsPeriod <- db.period[order(as.numeric(as.character(db.period$COMMENT_LIKES)),decreasing = TRUE),]
    dbCommentsPeriod <- dbCommentsPeriod[1:10,c("COMMENT","COMMENT_LIKES","COMMENT_CREATED")]
    colnames(dbCommentsPeriod) <- c("Comment","Number of Likes","Comment Date")
    
    output$eventsTable2 <- renderDataTable({dbCommentsPeriod},options = list(columnDefs = list(list(width = '700px', targets = 0))))
    
    period.posts <- unique(db.period$POST)
    
    post.likes <- c()
    for(pp in 1:length(period.posts)){
      post.likes[pp] <- sum(as.numeric(db.period$COMMENT_LIKES[which(db.period$POST == period.posts[pp])]))
    }
    
    post.likes.df <- data.frame(period.posts,post.likes)
    post.likes.df <- post.likes.df[order(post.likes.df$post.likes,decreasing = TRUE),]
    colnames(post.likes.df) <- c("Post" ,"Likes")
    
    post.sources <- c()
    post.dates <- c()
    for(ii in 1:nrow(post.likes.df)){
      post.sources[ii] <- db.period$POST_FROM[which(db.period$POST == post.likes.df$Post[ii])[1]]
      post.dates[ii] <- substr(db.period$POST_CREATED[which(db.period$POST == post.likes.df$Post[ii])[1]],1,10)
    }
    
    post.likes.df <- data.frame(post.likes.df,post.sources,post.dates)
    
    post.likes.df <- post.likes.df[1:10,c(1,3,4)]
    
    colnames(post.likes.df) <- c("Post","Source","Date")
    
    output$eventsTable1 <- renderDataTable({post.likes.df},options = list(columnDefs = list(list(width = '700px', targets = c(0,1)))))
    
    
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)

