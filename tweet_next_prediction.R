# This script is used to automatically post match predictions to twitter using the twitteR package
# Tuomo Nieminen 2016

library(twitteR)

# login to twitter
key <- ""
secret <- ""
token <- ""
token_secret <- ""
setup_twitter_oauth(key,secret,token,token_secret)

# prediction data
euro16 <- get(load("data/groupstage_predictions.Rda"))

# next match
match <- euro16[euro16$datetime > Sys.time(),][1,]
starttime <- format(match$datetime,"%H:%M")

# likely winner
fav <- ifelse(match$win>match$loss, match$home, match$away)
winprobs <- round(c(match$win, match$loss)*100, 0)

# build the tweet
tweet <- paste0("Seuraava #emfutis ",starttime," ",
              match$hometeam," vs ",match$awayteam,". ",
              "Suosikki on ",fav," (", max(winprobs),"% vs ",min(winprobs),"%, ",
              "tasapeli ",round(100*match$draw,0),"%).",
              " tyyppiarvo.com")

# update status on twitter
newstatus <- updateStatus(tweet)
