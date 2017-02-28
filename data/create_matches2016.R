# add predictor data to the EURO 2016 matches
# Tuomo Nieminen 2016

source("em_functions.R")

matches <- read.csv2("data/matches2016.csv",header=T)
colnames(matches) <- c("date","time","hometeam","awayteam")
teams2016 <- unique(matches$hometeam)

# Add uefa data
uefa_2016 <- read.table("data/em2016_uefa.txt",header=T,sep="\t")[,1:2]
colnames(uefa_2016) <- c("team","uefa")

home_uefa <- uefa_2016
away_uefa <- uefa_2016
colnames(home_uefa) <- paste0("home",names(uefa_2016))
colnames(away_uefa) <- paste0("away",names(uefa_2016))

matches1 <- merge(home_uefa,matches, by=("hometeam"),all.x=T)
matches2 <- merge(away_uefa,matches1,by="awayteam",all.x=T)

# Add shots data

shots <- read.table("data/q_shots2016.txt", header=T, sep="\t")[,c(1,3)]
colnames(shots) <- c("team","avrg_shots")
shots$team <- make_teams(shots$team)

#exclude teams that didn't qualify
shots <- shots[shots$team %in% teams2016,]
# shots <- rbind(shots,data.frame(team="France",avrg_shots=NA))

# impute the number of shots for france (who didn't play qualifiers)
uef <- uefa_2016[order(uefa_2016$team),]
uef <- uef[uef$team != "France",]
shots <- shots[order(shots$team),]
france_uef <- c(1,uefa_2016[uefa_2016$team=="France",]$uefa)
shotmodel <- lm(shots$avrg_shots~uef$uefa)
france_shots <- (france_uef%*%shotmodel$coefficients)[1]
france <- data.frame(team="France",avrg_shots=france_shots)
shots <- rbind(shots,france)

# predictors <- merge(uefa_2016, shots, by="team")

homeshots <- shots
awayshots <- shots
colnames(homeshots) <- paste0("home",names(shots))
colnames(awayshots) <- paste0("away", names(shots))

matches3 <- merge(homeshots, matches2, by="hometeam",all.x=T)
matches4<- merge(awayshots, matches3, by="awayteam", all.x=T)

matches4$shot_ratio <- matches4$homeavrg_shots/matches4$awayavrg_shots
matches4$uefa_ratio <- matches4$homeuefa/matches4$awayuefa
names(matches4)

#add group info
groups <- data.frame("A"=c("France","Romania","Albania","Switzerland"),
                     "B"=c("England","Russia","Slovakia","Wales"),
                     "C"=c("Germany","Northern Ireland","Poland","Ukraine"),
                     "D"=c("Croatia","Czech Republic","Spain","Turkey"),
                     "E"=c("Belgium","Italy","Republic of Ireland","Sweden"),
                     "F"=c("Austria","Hungary","Iceland","Portugal"))

find_group <- function(country) {
  names(groups)[which(groups==country,arr.ind=T)[2]]
}

matches4$group <- sapply(matches4$hometeam,find_group)

save(file = "data/matches2016.Rda", matches4)
