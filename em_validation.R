# How well did the statistical model perform now that data is available for all match outcomes.
# Tuomo Nieminen 2016

#install.packages("gsheet")
library(gsheet)

# get data from google sheet
url <- "https://docs.google.com/spreadsheets/d/1rmhZhSp8LhQRw0B6EnG3NEx8TzrB9WLOuSQNrsUfUv0"
em16 <- gsheet2tbl(url)

# predictions made by the model
em16$typy_pred <- apply(em16[,5:7], 1, max)
pp <- em16$typy_pred

# expected number of correct predictions assuming that the probabilities were the true probabilities 
sum(pp)

# number of correct predictions
kk <- sum(em16$typy_oikein)

# p-value for null hypothesis that the probabilities were the true probabilities
library(poibin)
2*ppoibin(kk - 1 , pp)
