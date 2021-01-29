library(tidyverse)
library(lubridate)
sumry = read.csv('./NY DOE/Summary.csv')
scores = read.csv('./NY DOE/Scores.csv')
quality = read.csv('./NY DOE/Quality.csv')
# names_columns = s19[3,]
# colnames(s19) = names_columns
# s19 = s19[,-c(1:3)]
# s19 = s19[-c(1:4),]
# names(s19) = make.names(names(s19),unique = T)

nydoe = sumry %>% left_join(scores, by=c("Year", "DBN")) %>% left_join(quality, by=c("Year", "DBN"))



x = sfile %>% mutate(
  District = substring(DBN, 1, 2),
  Borough = substring(DBN, 3, 3),
  School.Number = substring(DBN, 4, 6)
) %>% mutate(
  Borough = case_when(
    Borough == "K" ~ "Brooklyn",
    Borough == "X" ~ "Bronx",
    Borough == "Q" ~ "Queens",
    Borough == "M" ~ "Manhattan",
    Borough == "R" ~ "Staten Island"
  )
)


for (i in 5:6) {
  x[, i] = parse_number(x[, i])
}

for (i in 5:6) {
  x[, i] = x[, i] / 100
}


