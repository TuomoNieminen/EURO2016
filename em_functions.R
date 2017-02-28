# some utility functions for the EURO analysis
# Tuomo Nieminen 2016

# remove unwanted characters from team names
make_teams <- function(teams) {
  teams <- strsplit(gsub("([A-Z]+)","~\\1",teams),"~")
  teams <- lapply(teams,
                  function(t) {
                    team <- t[-1]
                    team <- team[1:(length(team)/2)]
                    paste(team,collapse="")
                  })
  unlist(teams)
}


# finnish team names to english
fi2en <- function(fi_teams) {
  
  teamnames <- c("Puola"="Poland","Venaja"="Russia","Hollanti"="Netherlands",
                 "Saksa"="Germany","Espanja"="Spain","Irlanti"="Republic of Ireland",
                 "Ranska"="France","Ukraina"="Ukraine","Kreikka"="Greece",
                 "Tanska"="Denmark", "Italia"="Italy","Ruotsi"="Sweden",
                 "Tshekki"="Czech Republic","Portugali"="Portugal",
                 "Kroatia" ="Croatia","Englanti"="England","Sveitsi"="Switzerland",
                 "Itävalta"="Austria","Romania"="Romania","Turkki"="Turkey")
  Encoding(names(teamnames)) <- "UTF-8"
  teamnames[fi_teams]
  
}

# compute uefa points to update team uefa points inside the competition.
uefa_points <- function(team, data) {
  home <- data[data$hometeam==team,]
  away <- data[data$awayteam==team,]
  goals_scored <- sum(home$homegoals) + sum(away$awaygoals)
  goals_conceded <- sum(home$awaygoals) + sum(away$homegoals)
  
  wins <- sum(home$goal_diff > 0) + sum(away$goal_diff < 0)
  losses <- sum(home$goal_diff < 0) + sum(away$goal_diff > 0)
  draws <-  3 - wins - losses
  
  coefficient <- 3000 + 51*goals_scored - 50*goals_conceded + 3000*wins + 1000*draws
  coefficient
}


# average two models to compute win probabilities
computeprobs <- function(team, id, nextround_id, cur, advance, fit1, fit2, include_results=F) {
  prev <- cur*2
  match <- em16fin[em16fin$id==nextround_id,][,c("hometeam","awayteam")]
  team_i <- which(match==id)
  opp_i <- ifelse(team_i==1, 2, 1)
  opp_id <- match[match!=id]
  prevmatch <- em16fin[em16fin$id==opp_id,]
  if(include_results & !is.na(prevmatch$winner)) {
    opponents <- prevmatch$winner
    p_opponent_advanced <- 1
  } else {
    possible_opponents <- advance[advance[[paste0("id", prev)]]==opp_id,]
    p_opponent_advanced <- possible_opponents[[paste0("advance_top",cur)]]
    opponents <- possible_opponents$team
  }
  winprobs <- sapply(opponents, function(t) {
    team <- as.character(team)
    t <- as.character(t)
    pair <- c(team,t)
    get_winprobs(pair[team_i],pair[opp_i], df = pr, fit1=fit1, fit2=fit2)[team_i]
  })
  sum(winprobs*p_opponent_advanced)
}


# use two models to compute win probabilities
get_winprobs <- function(home, away, df = NULL, fit1 = NULL, fit2 = NULL) {
  require(MASS)
  if(is.null(df)) {
    df <- get(load("data/predictors.Rda"))
  }
  if(is.null(fit1)) {
    fit1 <- get(load("uefit.Rda"))
  }
  if(is.null(fit2)) {
    fit2 <- get(load("shotfit.Rda"))
  }
  
  homedf <- df[df$team==home,]
  awaydf <- df[df$team==away,]
  shot_ratio <- homedf$avrg_shots / awaydf$avrg_shots
  uefa_ratio <- homedf$uefa / awaydf$uefa
  
  # ignore possible hometeam advantage by averaging
  newdata1 <- data.frame(shot_ratio = shot_ratio, uefa_ratio = uefa_ratio)
  newdata2 <- data.frame(shot_ratio = 1/shot_ratio, uefa_ratio = 1/uefa_ratio)
  
  p1 <- predict(fit1, newdata1, type="p")
  p2 <- predict(fit2, newdata1, type="p")
  p3 <- predict(fit1, newdata2, type="p")[3:1]
  p4 <- predict(fit2, newdata2, type="p")[3:1]
  p <- (p1 + p2 + p3 + p4)/4
  p <- c(p["win"] + p["draw"]/2, p["loss"]+ p["draw"]/2)
  p
}
