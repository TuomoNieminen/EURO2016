setwd("C:/Users/Tuomo/Dropbox/TYPY/EM2016")
source("em_functions.R")
library(gsheet)

url <- "https://docs.google.com/spreadsheets/d/1zgiYvDR4AdIFpZhBKrL8saMHW2ZRpRNqSoMRdjCPDn8/edit#gid=0"
em16fin <- gsheet2tbl(url)
em16fin$hometeam <- as.character(em16fin$hometeam)
em16fin$awayteam <- as.character(em16fin$awayteam)
pr <- get(load("data/predictors.Rda"))

url2 <- "https://docs.google.com/spreadsheets/d/1rmhZhSp8LhQRw0B6EnG3NEx8TzrB9WLOuSQNrsUfUv0"
em16 <- gsheet2tbl(url2)
em16$goal_diff <- em16$homegoals - em16$awaygoals

# add new uefa rating points
uefa_additions <-  sapply(pr$team, uefa_points, data = em16[1:36,])
pr$uefa <- (pr$uefa - 3000) + uefa_additions

# load 2008 and 2012 matches and qualifier stats
em1 <- get(load("data/EMq0812.Rda"))
em1 <- em1[complete.cases(em1),]
em1$shot_ratio <- em1$homeq_avrg_shots / em1$awayq_avrg_shots
em2 <- get(load("data/EMq2012.Rda"))

# fit models
library(MASS)
shotfit <- polr(outcome ~ shot_ratio, data = em1)
uefit <- polr(outcome ~ uefa_ratio, data = em2)

save(file="uefit.Rda",uefit)
save(file="shotfit.Rda",shotfit)

#### PLAYOFF PREDICTIONS ####

playoffs <- em16fin[1:8,]
playoff_preds <- mapply(playoffs$hometeam, playoffs$awayteam, FUN=get_winprobs,
                        MoreArgs = list(fit1=shotfit, fit2=uefit, df=pr))
em16 <- cbind(playoffs, t(playoff_preds))

team <- c(em16$hometeam, em16$awayteam)
id16 <- c(em16$id, em16$id); id8 <- c(em16$id8, em16$id8)
id4 <- c(em16$id4, em16$id4); id2 <- rep(51,16)
 
advance_top8 <- c(em16$win, em16$loss)
advance <- cbind.data.frame(id16, id8, id4, id2, team, advance_top8)

top4probs <- mapply(computeprobs, advance$team, advance$id16, advance$id8,
                    MoreArgs = list(cur = 8, advance = advance, fit1=shotfit, fit2=uefit))
advance_top4 <- advance$advance_top8 * top4probs
advance <- cbind(advance, advance_top4)

top2probs <- mapply(computeprobs, advance$team, advance$id8, advance$id4,
                    MoreArgs = list(cur=4, advance = advance, fit1=shotfit, fit2=uefit))
advance_top2 <- advance$advance_top4 * top2probs
advance <- cbind(advance, advance_top2)

winprobs <- mapply(computeprobs, advance$team, advance$id4, advance$id2,
                   MoreArgs = list(cur=2, advance=advance, fit1=shotfit, fit2=uefit))
win <- advance$advance_top2 * winprobs
advance <- cbind(advance, win)

final_predictions <- advance[order(-advance$win),][,5:ncol(advance)]
final_predictions[,2:5] <- 100*round(final_predictions[,2:5],4)

write.csv(file="final_predictions.csv",final_predictions)


############ QUARTER FINALS PREDICTIONS ###########

quarters <- em16fin[9:12,]
quarters$hometeam <- sapply(quarters$hometeam, function(id) {
  em16fin[em16fin$id==id,]$winner})
quarters$awayteam <- sapply(quarters$awayteam, function(id) {
  em16fin[em16fin$id==id,]$winner
})
quarter_preds <- mapply(quarters$hometeam, quarters$awayteam, FUN=get_winprobs,
                                               MoreArgs = list(fit1=shotfit, fit2=uefit, 
                                                               df=pr))

em16q <- cbind(quarters, t(quarter_preds))

team <- c(em16q$hometeam, em16q$awayteam)
id8 <- c(em16q$id8, em16q$id8)
id4 <- c(em16q$id4, em16q$id4); id2 <- rep(51,8)

advance_top4 <- c(em16q$win, em16q$loss)
advance2 <- cbind.data.frame(id8, id4, id2, team, advance_top4)

top2probs <- mapply(computeprobs, advance2$team, advance2$id8, advance2$id4,
                    MoreArgs = list(cur = 4, advance = advance2, 
                                    fit1=shotfit, fit2=uefit, 
                                    include_results = T))
advance_top2 <- advance2$advance_top4 * top2probs
advance2 <- cbind(advance2, advance_top2)

winprobs <- mapply(computeprobs, advance2$team, advance2$id4, advance2$id2,
                   MoreArgs = list(cur=2, advance=advance2, 
                                   fit1=shotfit, fit2=uefit,
                                   include_results=T))
win <- advance2$advance_top2 * winprobs
advance2 <- cbind(advance2, win)

final_predictions <- advance2[order(-advance2$win),][,4:ncol(advance2)]
prcnt <- function(x) paste0(100*round(x,4),"%")
final_predictions[,2:4] <- apply(final_predictions[,2:4],2,prcnt)

write.csv(file="quarters_predictions.csv",final_predictions)

#### probabilities for the matches that took place ####

library(gsheet)

url <- "https://docs.google.com/spreadsheets/d/1rmhZhSp8LhQRw0B6EnG3NEx8TzrB9WLOuSQNrsUfUv0"
em16 <- gsheet2tbl(url)
em16 <- em16[em16$group %in% c("top16", "top8", "top4", "top2"),]

final_matches_winprobs <- t(mapply(get_winprobs, em16$hometeam, em16$awayteam))
final_matches_winprobs
