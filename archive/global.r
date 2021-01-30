#need global to run this whole thing....

library(tidyverse)
library(lubridate)
library(viridis)
library(hrbrthemes)
sumry = read.csv('../NY DOE/Summary.csv')
scores = read.csv('../NY DOE/Scores.csv')
quality = read.csv('../NY DOE/Quality.csv')
# names_columns = s19[3,]
# colnames(s19) = names_columns
# s19 = s19[,-c(1:3)]
# s19 = s19[-c(1:4),]
# names(s19) = make.names(names(s19),unique = T)

#joining tables and removing uneeded columns
nydoe = sumry %>%
  left_join(scores, by = c("Year", "DBN")) %>%
  left_join(quality, by =
              c("Year", "DBN")) %>% select(-c(6:25)) %>%
  mutate(
    District = substring(DBN, 1, 2),
    Borough = substring(DBN, 3, 3),
    School.Number = substring(DBN, 4, 6)
  ) %>%
  mutate(
    Borough = case_when(
      Borough == "K" ~ "Brooklyn",
      Borough == "X" ~ "Bronx",
      Borough == "Q" ~ "Queens",
      Borough == "M" ~ "Manhattan",
      Borough == "R" ~ "Staten Island"
    )
  )

#change char cols to numeric
for (i in 6:32) {
  nydoe[, i] = parse_number(nydoe[, i])
}
#had to do these manually
nydoe$X90Pct.Attend=parse_number(nydoe$X90Pct.Attend)
nydoe$ELA.Std=parse_number(nydoe$ELA.Std)
nydoe$Math.Std=parse_number(nydoe$Math.Std)
nydoe$Year=as.factor(nydoe$Year)

