files[2] <- 'Georgia Tech vs Texas State - Mon Jan 30.csv'
files[3] <- 'Detroit Pistons At Boston Celtics  -  Mon Jan 30.csv'
files[1] <- 'Brooklyn Nets At Miami Heat  -  Mon Jan 30.csv'

college_times <- 1:20
college_periods <- 1:2

nba_times <- 1:12
nba_periods <- 1:4
minute_intervals <- c(23,35,56)

college_minutes <- c()
for(jj in college_periods){
  for(ii in sort(college_times,decreasing = T)){
    for(kk in 1:length(minute_intervals)){
      college_minutes <- c(college_minutes,paste0('Half ',jj,' ',college_times[ii]-1,":",sort(minutes_intervals,decreasing = T)[kk]))
    }
  }
}

nba_minutes <- c()
for(jj in nba_periods){
  for(ii in sort(nba_times,decreasing = T)){
    for(kk in 1:length(minute_intervals)){
      nba_minutes <- c(nba_minutes,paste0('Q',jj,' ',nba_times[ii]-1,":",sort(minutes_intervals,decreasing = T)[kk]))
    }
  }
}

lines_1 <- sample(180:250,length(nba_minutes),replace = T) + 0.5

scores_1 <- sample(0:round(mean(lines_1) / (length(nba_minutes)*3),digits = 0)+2,length(nba_minutes),replace = T)
score_1 <- scores_1[1]
for(ss in 2:length(scores_1)){
  score_1[ss] <- score_1[ss-1] + scores_1[ss]
}

lines_3 <- sample(180:250,length(nba_minutes),replace = T) + 0.5

scores_3 <- sample(0:round(mean(lines_3) / (length(nba_minutes)*3),digits = 0)+2,length(nba_minutes),replace = T)
score_3 <- scores_3[1]
for(ss in 2:length(scores_3)){
  score_3[ss] <- score_3[ss-1] + scores_3[ss]
}

lines_2 <- sample(180:250,length(college_minutes),replace = T) + 0.5

scores_2 <- sample(0:round(mean(lines_2) / (length(college_minutes)*3),digits = 0)+2,length(college_minutes),replace = T)
score_2 <- scores_2[1]
for(ss in 2:length(scores_2)){
  score_2[ss] <- score_2[ss-1] + scores_2[ss]
}


df_1 <- data.frame(nba_minutes,lines_1,score_1,"NBA")
df_2 <- data.frame(college_minutes,lines_2,score_2,"NCAA")
df_3 <- data.frame(nba_minutes,lines_3,score_3,"NBA")

colnames(df_1) <- c('GameTime','Line','Points','Type')
colnames(df_2) <- c('GameTime','Line','Points','Type')
colnames(df_3) <- c('GameTime','Line','Points','Type')

write.csv(df_1,paste0('Data/',files[1]),row.names = F)
write.csv(df_2,paste0('Data/',files[2]),row.names = F)
write.csv(df_3,paste0('Data/',files[3]),row.names = F)










