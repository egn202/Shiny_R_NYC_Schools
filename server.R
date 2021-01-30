shinyServer(function(input, output) {
  
  # nydoe = read.csv('nydoe.csv')
  # nydoe$District=as.factor(nydoe$District)
  # nydoe$School.Number=as.factor(nydoe$School.Number)
  
  output$plot1 <- renderPlot({
    ps20 = nydoe %>% filter(DBN=="01M020") %>% select(Year,SA.Score)
    district = nydoe %>% filter(District=="1") %>% select(Year,SA.Score) %>% group_by(Year) %>% summarise(SA.Score = mean(SA.Score, na.rm=T))
    city = nydoe %>% group_by(Year) %>% summarise(SA.Score = mean(SA.Score, na.rm=T))
    
    ggplot() + geom_line(data=ps20, aes(x=Year, y = SA.Score), color= "red") + 
      geom_line(data=district, aes(x=Year,y=SA.Score), color = "blue") +
      geom_line(data=city, aes(x=Year,y=SA.Score), color = "green") 
  })

  output$plot2 <- renderPlot({
    ps20 = nydoe %>% filter(DBN=="01M020") %>% select(Year,SA.Score)
    district = nydoe %>% filter(District=="1") %>% select(Year,SA.Score) %>% group_by(Year) %>% summarise(SA.Score = mean(SA.Score, na.rm=T))
    
    ggplot() + geom_line(data=ps20, aes(x=Year, y = SA.Score), color= "red") + 
      geom_line(data=district, aes(x=Year,y=SA.Score), color = "blue") +
      geom_point()
  })
  
  })