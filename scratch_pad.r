nydoe = read.csv('nydoe.csv')
nydoe$District=as.factor(nydoe$District)
nydoe$School.Number=as.factor(nydoe$School.Number)



#infobox - 

summary(nydoe$EconNeed.) #upper quartile = 86.1
p = nydoe %>% filter(EconNeed.>=86.10) %>% summarise(avgMath=mean(Math.Std, na.rm=TRUE), avgELA=mean(mean(ELA.Std, na.rm=TRUE)))

#scatter: econ need : race
nydoe %>% select(EconNeed.,Asian,Black,Hispanic = Hisp,White) %>% pivot_longer(c(2:5), names_to = "Race", values_to = "Pct") %>% 
  filter(Race == "Black") %>% 
  ggplot(aes(x=EconNeed., y=Pct, color=Race))+geom_point(alpha=.5)+ 
  geom_smooth(col="#666666", se=FALSE, method="lm")+theme_light()


#scatter: econ need vs attendance####
nydoe %>% select(Attendance,EconNeed.)%>% 
  ggplot(aes(x=EconNeed., y=Attendance))+geom_point(col="#3399FF", alpha=.5)+ 
  geom_smooth(col="#666666", se=FALSE, method="lm")+theme_light()+ coord_cartesian(ylim = c(85,100))


cor(nydoe$EconNeed.,nydoe$Attendance, use="complete.obs") 

#scatter: econ need vs score####
nydoe %>% select(Math.Std,ELA.Std,EconNeed.)%>%  pivot_longer(c(1,2), names_to = "Test", values_to = "TScore") %>% 
ggplot(aes(x=EconNeed., y=TScore))+geom_point(col="#3399FF", alpha=.5)+ geom_smooth(col="#666666", se=FALSE, method="lm")+theme_light()
cor(t$EconNeed.,t$TScore, use="complete.obs")


#scatterplots - various
nydoe %>% ggplot(aes(x=ELA.Score, y=Trust.Score))+geom_point(aes(color=District))
nydoe %>% ggplot(aes(x=SA.Score, y=Enrollment))+geom_point() #+ coord_cartesian(ylim = c(85,100))
nydoe %>% ggplot(aes(x=SA.Score, y=White))+geom_point()


#scatter: econneed vs race % of school
nydoe %>% select(EconNeed., Asian, Hisp,White,Black) %>% pivot_longer(c(2:5), names_to = "Race", values_to = "Pct.School") %>% 
  filter(Race=="Black") %>% 
ggplot()+geom_point(aes(x=Pct.School, y=EconNeed.,color=Race),alpha=.5)+geom_smooth(aes(x=Pct.School, y=EconNeed.))


#scatter: quality vs score####
t = nydoe %>% select(Math.Std,ELA.Std,RI.Score,CollabT.Score, SuprtEnv.Score,Leadrshp.Score,Comunty.Score,Trust.Score)
colnames(t)[1:8] = c("Math","ELA","Rigorous Instruction","Collaborative Teachers","Supportive Enviornment","Leadership","Community","Trust")
t = t %>% pivot_longer(c(3:8), names_to = "Quality",values_to = "QScore" ) %>% pivot_longer(c(1,2), names_to = "Test", values_to = "TScore") 


t %>% filter(Quality == "Community", Test == "Math") %>% 
ggplot(aes(x=QScore, y=TScore, color=Quality))+geom_point(col="#3399FF", alpha=.5)+ geom_smooth(col="#666666", se=FALSE, method="lm")+theme_light()



#scatter: race vs. score (math or ela)
nydoe %>% select(Math.Std,ELA.Std, Asian, Hisp,White,Black) %>% pivot_longer(c(2:5), names_to = "Race", values_to = "Pct.School") %>% 
  


nydoe %>% ggplot(aes(x=Math.Std, y=EconNeed.))+geom_point(aes())




nydoe %>% filter 








#table of schools for test scores
y = nydoe %>% filter(Year=="2019") %>% select(District,School.Number,Name,ELA.Std,Math.Std,SA.Score)

#enrollment counts by year, dodged by race, total line in back -- nothing of interest

g = nydoe %>% mutate(Asian = Asian/100 * Enrollment,Black = Black/100 * Enrollment,Hisp = Hisp/100 * Enrollment,White = White/100 * Enrollment) %>% 
  pivot_longer(c(Asian, Black, Hisp, White),names_to = "Race",values_to = "Enroll") %>% select(Year,Race,Enroll) %>% 
  group_by(Year, Race) %>% summarise(Enroll=sum(Enroll))

h = nydoe %>% group_by(Year) %>% summarise(Enroll=sum(Enrollment))
  ggplot() + geom_bar(data=g,aes(x = Year, y = Enroll, fill = Race), stat = "identity",position = "dodge") + scale_fill_brewer(palette = "Dark2")
  ggplot(data=h)+geom_line(data=h,aes(x=Year, y=Enroll))

#hist of scores: all schools, all years
nydoe %>% ggplot(aes(x=ELA.Std))+
  geom_histogram(binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  theme_ipsum()


#hist scores by district
nydoe %>% group_by(Year, District) %>% summarise(mean=mean(SA.Score, na.rm=T)) %>% 
  ggplot(aes(fill=District, y=mean, x=Year)) + 
  geom_bar(position="dodge", stat="identity")+ facet_wrap(~Year)
scale_fill_viridis(discrete = T) +
  theme_ipsum() +
  xlab("")

#density by year
nydoe %>% ggplot(aes(x=ELA.Std, fill=as.factor(Year))) +
  geom_density( alpha=.25) +
  theme_ipsum()

#box plot scores by district
nydoe %>% filter(Year=="2019", District !="84") %>% ggplot(aes(x=District, y=ELA.Std))+geom_boxplot(aes(fill=Borough))+facet_wrap(~Year)

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

#line chart of school,district,city test scores 2015-19####
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

#???
group_by(Year) %>% summarise(
  Asian = sum(Asian / 100 * Enrollment),
  Black = sum(Black / 100 * Enrollment),
  Hispanic = sum(Hisp / 100 * Enrollment),
  White = sum(White / 100 * Enrollment)
) 
ggplot() + geom_col(aes(x = Year), stat="identity")

#???
school = nydoe %>% filter(DBN=="01M020", Year==2019) %>% pivot_longer(c(27:32), names_to = "Quality", values_to = "QScore" ) %>% select(Quality,QScore) %>% 
  ggplot(aes(Quality,QScore))+geom_bar(aes(fill=Quality),stat='identity')+scale_fill_hue(c = 40)

g = ggplot(mpg,aes(cty,hwy))
g+geom_bar(aes(fill=cty), stat="identity")

select(Year,RI.Score,CollabT.Score,SuprtEnv.Score,Leadrshp.Score,Comunty.Score) %>% 
  ggplot()+geom_bar(aes(x=Year, color="identity"), position="dodge")










