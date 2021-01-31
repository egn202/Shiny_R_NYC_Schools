nydoe = read.csv('nydoe.csv')
nydoe$District=as.factor(nydoe$District)
nydoe$School.Number=as.factor(nydoe$School.Number)

school = nydoe %>% filter(DBN=="01M020") %>% select(Year,SA.Score)
district = nydoe %>% filter(District=="1") %>% select(Year,SA.Score) %>% group_by(Year) %>% summarise(SA.Score = mean(SA.Score, na.rm=T))
city = city = nydoe %>% group_by(Year) %>% summarise(SA.Score = mean(SA.Score, na.rm=T))
graph=cbind(school, district$SA.Score,city$SA.Score)
colnames(graph)[2:4] = c("School","District","City")
graph = pivot_longer(graph, c(2:4),names_to = "level", values_to = "score")

ggplot(graph) + geom_line(aes(x=Year, y=score, color=level, lwd=1))+
  geom_point(aes(x=Year, y=score)) +
  labs(x="Year",
       y="Student Achievement Score")+scale_color_hue(c=40)

ggplot(graph, aes(x=Year)) + geom_line(aes(y=School), color= "School", lwd=2) + 
  geom_line(aes(y=District), color= "District", lwd=2) + 
  geom_line(aes(y=City), color= "City", lwd=2) +  
  labs(x="Year",
       y="Student Achievement Score")+
  scale_fill_hue(c = 40)


#selectinput prep

nydoe %>% filter(School.Number=="20")


###
school = nydoe %>% filter(DBN=="01M020", Year==2019) %>% pivot_longer(c(27:32), names_to = "Quality", values_to = "QScore" ) %>% select(Quality,QScore) %>% 
ggplot(aes(Quality,QScore))+geom_bar(aes(fill=Quality),stat='identity')+scale_fill_hue(c = 40)
  
g = ggplot(mpg,aes(cty,hwy))
g+geom_bar(aes(fill=cty), stat="identity")

  select(Year,RI.Score,CollabT.Score,SuprtEnv.Score,Leadrshp.Score,Comunty.Score) %>% 
  ggplot()+geom_bar(aes(x=Year, color="identity"), position="dodge")



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
nydoe %>% ggplot(aes(x=Math.Std, fill=as.factor(Year))) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()

#box plot scores by district
nydoe %>% filter(Year=="2019") %>% ggplot(aes(x=District, y=SA.Score))+geom_boxplot(aes(fill=Borough))+facet_wrap(~Year)

nydoe %>%  ggplot(aes(x=District, y=SA.Score))+geom_boxplot(aes(fill=Borough))+facet_wrap(~Year)

nydoe %>% ggplot() + geom_boxplot(aes(x =as.factor(Year), y = Math.Std, fill=as.factor(Year)))+
  geom_boxplot(aes(x =as.factor(Year), y = ELA.Std))

#smooth line: math scores for district
nydoe %>% filter(District=="1") %>% ggplot() + geom_smooth(aes(x= Year, y=Math.Std))

#barplot - enrollment by race  #4 What if wanted to then drill in by a school on shiny? diff code needed?
nydoe %>% mutate(
  Asian = Asian * Enrollment,
  Black = Black * Enrollment,
  Hisp = Hisp * Enrollment,
  White = White * Enrollment
) %>% pivot_longer(c(Asian, Black, Hisp, White),
                   names_to = "Race",
                   values_to = "Enroll") %>% filter(District=="1") %>% 
  ggplot() + geom_col(aes(x = Year, y = Enroll, fill = Race),
                      stat = "identity",
                      position = "fill") + facet_wrap(~Name)


nydoe %>% pivot_longer(c(Asian, Black, Hisp, White),
                       names_to = "Race",
                       values_to = "Enroll") %>% filter(District == "1") %>%
  ggplot() + geom_col(aes(x = Year, y = Enroll, fill = Race),
                      stat = "identity",
                      position = "fill") +
  scale_fill_brewer(palette = "Set1") + theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    panel.background = element_rect(fill = "white", )
  ) + labs(y = "", x = "") + facet_wrap(~ Name) + theme(strip.background = element_rect(color = "white"),
                                                        strip.text.x = element_text(size = 7))
#########


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
