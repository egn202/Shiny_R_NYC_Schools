library(tidyverse)
library(lubridate)
library(viridis)
library(hrbrthemes)
sumry = read.csv('./NY DOE/Summary.csv')
scores = read.csv('./NY DOE/Scores.csv')
quality = read.csv('./NY DOE/Quality.csv')
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

write.csv(nydoe, file = "nydoe.csv")

#1 should all the above be excluded from final code?

#3 challenges with YOY change

#2 HOW TO GRAPH THIS W/OUT CREATING VARIABLES TO STORE THE DFs - e.g. mix data from 2 diff pipe sequences
#graph of ps20 vs district
ps20 = nydoe %>% filter(DBN=="01M020") %>% select(Year,SA.Score)
district = nydoe %>% filter(District=="01") %>% select(Year,SA.Score) %>% group_by(Year) %>% summarise(SA.Score = mean(SA.Score, na.rm=T))

ggplot() + geom_line(data=ps20, aes(x=Year, y = SA.Score), color= "red") + 
  geom_line(data=district, aes(x=Year,y=SA.Score), color = "blue") +
  geom_point()
####


#hist of scores
nydoe %>% ggplot(aes(x=SA.Score))+
  geom_histogram(binwidth=.1, fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  theme_ipsum()


#hist scores by district
nydoe %>% group_by(Year, District) %>% summarise(mean=mean(SA.Score, na.rm=T)) %>% 
  ggplot(aes(fill=District, y=mean, x=Year)) + 
  geom_bar(position="dodge", stat="identity")+ facet_wrap(~Year)
  scale_fill_viridis(discrete = T) +
  theme_ipsum() +
  xlab("")

#density by year
nydoe %>% ggplot(aes(x=Math.Std, fill=Year)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()

#box plot scores by district
nydoe %>% filter(Year=="2019") %>% ggplot(aes(x=District, y=SA.Score))+geom_boxplot(aes(fill=Borough))+facet_wrap(~Year)

nydoe %>%  ggplot(aes(x=District, y=SA.Score))+geom_boxplot(aes(fill=Borough))+facet_wrap(~Year)

nydoe %>% ggplot() + geom_boxplot(aes(x =Year, y = Math.Std, fill=Year))+
geom_boxplot(aes(x =Year, y = ELA.Std))

#smooth line: math scores for district
nydoe %>% filter(District=="01") %>% ggplot() + geom_smooth(aes(x=Year, y=Math.Std))

#barplot - enrollment by race  #4 What if wanted to then drill in by a school on shiny? diff code needed?
nydoe %>% mutate(
  Asian = Asian * Enrollment,
  Black = Black * Enrollment,
  Hisp = Hisp * Enrollment,
  White = White * Enrollment
) %>% pivot_longer(c(Asian, Black, Hisp, White),
                   names_to = "Race",
                   values_to = "Enroll") %>% filter(District=="01") %>% 
  ggplot() + geom_col(aes(x = Year, y = Enroll, fill = Race),
                      stat = "identity",
                      position = "fill") + facet_wrap(~Name)




group_by(Year) %>% summarise(
  Asian = sum(Asian / 100 * Enrollment),
  Black = sum(Black / 100 * Enrollment),
  Hispanic = sum(Hisp / 100 * Enrollment),
  White = sum(White / 100 * Enrollment)
) 


  ggplot() + geom_col(aes(x = Year), stat="identity")





#scrap bin
nydoe %>% ggplot(aes(x=ELA.Score, y=Trust.Score))+geom_point(aes(color=District))
nydoe %>% ggplot(aes(x=SA.Score, y=Enrollment))+geom_point() 


+ coord_cartesian(ylim = c(85,100))



nydoe %>% ggplot(aes(x=SA.Score, y=White))+geom_point()




ggplot( aes(x=Student.A)) +
  geom_histogram( binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 3") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )





















#change percent to .01 format?
for (i in 5:6) {
  x[, i] = x[, i] / 100
}


