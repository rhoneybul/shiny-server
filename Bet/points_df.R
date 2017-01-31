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