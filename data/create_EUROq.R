

source("em_functions.R")


# FINAL COMPETITION TEAMS
teams2008 <- read.table("data/comp_overview2008.txt",header=T,sep="\t", stringsAsFactors = F)
teams2012 <- read.table("data/comp_overview2012.txt", header=T,sep="\t",stringsAsFactors = F)
teams2008 <- make_teams(teams2008$Team)
teams2012 <- make_teams(teams2012$Team)

# SHOTS

# 2008 qualifier shots on and off target 
on2008 <- read.table("data/q_ontarget2008.txt", header=T,stringsAsFactors = F, sep="\t")[,1:2]
off2008 <- read.table("data/q_offtarget2008.txt",header=T, stringsAsFactors = F,sep="\t")[,1:2]
colnames(on2008) <- c("team","q_avrg_ontarget")
colnames(off2008) <- c("team","q_avrg_offtarget")

shots2008 <- merge(on2008,off2008,by="team")
shots2008$year <- 2008

# 2012 qualifier on and off shots
on2012 <- read.table("data/q_ontarget2012.txt", header=T,stringsAsFactors = F, sep="\t")[,1:2]
off2012 <- read.table("data/q_offtarget2012.txt", header=T, stringsAsFactors = F,sep="\t")[,1:2]
colnames(on2012) <- c("team","q_avrg_ontarget")
colnames(off2012) <- c("team","q_avrg_offtarget")
shots2012 <- merge(on2012,off2012,by="team")
shots2012$year <- 2012

# handle weird teamnames
shots2008$team <- make_teams(shots2008$team)
shots2012$team <- make_teams(shots2012$team)

  # exclude teams that didn't qualify

# include only final competition teams
shots2008 <- shots2008[shots2008$team %in% teams2008,]
shots2012 <- shots2012[shots2012$team %in% teams2012,]

# combine 2008 and 2012 qualifier shots
shots <- rbind(shots2008,shots2012)

# add average shots column
shots$q_avrg_shots <- rowSums(shots[,2:3])

  # GOALS

# 2008 qualifier goals scored and conceded 
scored2008 <- read.table("data/q2008_scored.txt", header=T,stringsAsFactors = F, sep="\t")[,1:2]
conceded2008 <- read.table("data/q2008_conceded.txt", header=T, stringsAsFactors = F,sep="\t")[,1:2]
colnames(scored2008) <- c("team","q_avrg_scored")
colnames(conceded2008) <- c("team", "q_avrg_conceded")
goals2008 <- merge(scored2008,conceded2008,by="team")
goals2008$year <- 2008

# 2012 goal data
scored2012 <- read.table("data/q2012_scored.txt", header=T,stringsAsFactors = F, sep="\t")[,1:2]
conceded2012 <- read.table("data/q2012_conceded.txt", header=T, stringsAsFactors = F,sep="\t")[,1:2]
colnames(scored2012) <- c("team","q_avrg_scored")
colnames(conceded2012) <- c("team", "q_avrg_conceded")
goals2012 <- merge(scored2012,conceded2012,by="team")
goals2012$year <- 2012

# handle weird teamnames
goals2008$team <- make_teams(goals2008$team)
goals2012$team <- make_teams(goals2012$team)

# exclude teams that didn't qualify
goals2008 <- goals2008[goals2008$team %in% teams2008,]
goals2012 <- goals2012[goals2012$team %in% teams2012,]

# combine 2008 and 2008 qualifier goals
goals <- rbind(goals2008,goals2012)

# add goaldifference column
goals$q_avrg_goaldifference <- goals[,2]-goals[,3]

  # OVERVIEW

# qualifier overview data
ov2012 <- read.table("data/q_overview2012.txt", header=T,stringsAsFactors = F, sep="\t")
ov2008 <- read.table("data/q_overview2008.txt", header=T,stringsAsFactors = F, sep="\t")

ov2012$Team <- make_teams(ov2012$Team)
ov2008$Team <- make_teams(ov2008$Team)

# exclude teams that didn't qualify
ov2012 <- ov2012[ov2012$Team %in% teams2012,]
ov2008 <- ov2008[ov2008$Team %in% teams2008,]

colnames(ov2012) <- c("team","q_goals_scored","q_goals_against","q_yellow","q_red","q_on_target","q_offsides","q_corners","q_fouls")
colnames(ov2008) <- c("team","q_goals_scored","q_goals_against","q_yellow","q_red","q_on_target","q_offsides","q_corners","q_fouls")

ov2008$year <- 2008
ov2012$year <- 2012

# combine 2008 and 2012 overview data
ovdata <- rbind(ov2008,ov2012)
# use only offsides and corners (and team, year)
ovdata <- ovdata[,c(1,7,8,10)]

# merge with fifa rating and match data

em2012 <- read.csv2("data/em2012.csv", stringsAsFactors = F)
em2008 <- read.csv2("data/em2008.csv", stringsAsFactors = F)
em2012$year <- 2012
em2008$year <- 2008

em0812 <- rbind(em2012,em2008)

# change finish colnames to english and finnish teamnames to english
colnames(em0812) <- c("phase","hometeam","awayteam","goals_home","goals_away"
                      ,"fifa_home","fifa_away","goal_diff","fifa_diff","year")
Encoding(em0812$hometeam) <- "UTF-8"
Encoding(em0812$awayteam) <- "UTF-8"

# finnish to english
em0812$hometeam <- fi2en(em0812$hometeam)
em0812$awayteam <- fi2en(em0812$awayteam)


# merge the qualifier data into one data

shotgoal <- merge(shots,goals,by=c("team","year"))
qdata <- merge(shotgoal,ovdata,by=c("team","year"))

# spplit to home and awayteams to merge with match results
hometeamdata <- qdata
awayteamdata <- qdata

# rename columns except for "year"
colnames(hometeamdata)[-2] <- paste0("home",colnames(qdata)[-2])
colnames(awayteamdata)[-2] <- paste0("away",colnames(qdata)[-2])

em.1 <- merge(em0812, hometeamdata, by=c("hometeam","year"),all.x = T)
em.2 <- merge(em.1, awayteamdata,by=c("awayteam","year"),all.x = T)

# add match outcome
em.2$outcome <- 1
em.2$outcome[em.2$goal_diff > 0] <- 3
em.2$outcome[em.2$goal_diff < 0 ] <- 0
em.2$outcome <- factor(em.2$outcome,ordered = T, levels=c(0,1,3),labels=c("loss","draw","win"))

# save
save(em.2,file="data/EUROq.Rda")
