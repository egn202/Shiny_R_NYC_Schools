shinyServer(function(input, output, session) {

  observe({ #this updates the choice of schools based on the selected district
    Name <- unique(nydoe %>%
                     filter(nydoe$District == input$District) %>%
                     .$Name)
    updateSelectizeInput(
      session, "Name",
      choices = Name,
      selected = Name[1])
    
    Name2 <- unique(nydoe %>%
                     filter(nydoe$District == input$District2) %>%
                     .$Name)
    updateSelectizeInput(
      session, "Name2",
      choices = Name2,
      selected = Name2[1])
    })  
  
  #Student achievement chart
  output$plot1 <- renderPlot({
    school = nydoe %>% filter(Name == input$Name) %>% select(Year, SA.Score)
    district = nydoe %>% filter(District==input$District) %>% select(Year,SA.Score) %>% group_by(Year) %>% summarise(SA.Score = mean(SA.Score, na.rm=T))
    city = city = nydoe %>% group_by(Year) %>% summarise(SA.Score = mean(SA.Score, na.rm=T))
    graph=cbind(school, district$SA.Score,city$SA.Score)
    colnames(graph)[2:4] = c("School","District","City")
    graph = pivot_longer(graph, c(2:4),names_to = "level", values_to = "score")
    
    ggplot(graph) + geom_line(aes(x=Year, y=score, color=level),lwd=3)+
      geom_point(aes(x=Year, y=score),color="black",alpha=.5) + theme(legend.position = "right")+
      labs(x="Year",
           y="Student Achievement Score")+scale_color_brewer(palette="Dark2")+theme_classic()
  })
  #quality ratings chart
  output$plot2 <- renderPlot({
    quality = nydoe %>% filter(Name == input$Name, Year == 2019) %>% pivot_longer(c(27:32), names_to = "Quality", values_to = "QScore") %>% select(Quality, QScore)
    quality$Quality = c("Rigorous Instruction","Collaborative Teachers","Supportive Enviornment","Leadership","Family-Community Ties","Trust")  
    
    ggplot(data=quality, aes(Quality, QScore)) + geom_bar(aes(fill = Quality), stat='identity',) + coord_flip() + labs(y = "Rating",x="") + theme_classic() + theme(legend.position ="none") + scale_fill_brewer(palette="Set1")

  })
  #ELA standards chart
  output$plot3 <- renderPlot({
    school = nydoe %>% filter(Name == input$Name) %>% select(Year, ELA.Std)
    district = nydoe %>% filter(District==input$District) %>% select(Year,ELA.Std) %>% group_by(Year) %>% summarise(ELA.Std = mean(ELA.Std, na.rm=T))
    city = city = nydoe %>% group_by(Year) %>% summarise(ELA.Std = mean(ELA.Std, na.rm=T))
    graph=cbind(school, district$ELA.Std,city$ELA.Std)
    colnames(graph)[2:4] = c("School","District","City")
    graph = pivot_longer(graph, c(2:4),names_to = "level", values_to = "score")
    
    ggplot(graph) + geom_line(aes(x=Year, y=score, color=level),lwd=3)+
      geom_point(aes(x=Year, y=score),color="black",alpha=.5) + theme(legend.position = "right")+
      labs(x="Year",
           y="% Children at ELA Std")+scale_color_brewer(palette="Dark2")+theme_classic()
  })
  #Math Standards Chart
  output$plot4 <- renderPlot({
    school = nydoe %>% filter(Name == input$Name) %>% select(Year, Math.Std)
    district = nydoe %>% filter(District==input$District) %>% select(Year,Math.Std) %>% group_by(Year) %>% summarise(Math.Std = mean(Math.Std, na.rm=T))
    city = city = nydoe %>% group_by(Year) %>% summarise(Math.Std = mean(Math.Std, na.rm=T))
    graph=cbind(school, district$Math.Std,city$Math.Std)
    colnames(graph)[2:4] = c("School","District","City")
    graph = pivot_longer(graph, c(2:4),names_to = "level", values_to = "score")
    
    ggplot(graph) + geom_line(aes(x=Year, y=score, color=level),lwd=3)+
      geom_point(aes(x=Year, y=score),color="black",alpha=.5) + theme(legend.position = "right")+
      labs(x="Year",
           y="% Children at Math Std")+scale_color_brewer(palette="Dark2")+theme_classic()
  })
  
  #School demographics
  output$plot5 <- renderPlot({
    nydoe %>% filter(Name == input$Name) %>% select(Year, Asian, Black, Hisp, White) %>% pivot_longer(c(2:5), names_to = "Race", values_to = "Pct") %>%
    ggplot()+geom_bar(aes(x=Year, y=Pct, fill=Race),stat="identity") + scale_fill_brewer(palette="Set1")+theme_classic()      
  })
  
  # show school info DataTable
  output$table1 <- DT::renderDataTable({
    school_info = nydoe %>% filter(Name == input$Name) %>% select(Year, Enrollment,Principal,Teachers.3) %>%
      pivot_longer(c(2:4), names_to = "Info", values_to = "Pct") %>% pivot_wider(names_from =
                                                                                   Year, values_from = Pct)
    school_info$Info=c("Enrollment","Principal Tenure","Teacher with +3Y Exp %")
    datatable(school_info, rownames=FALSE) 
  })
  
  # graph addtl details
  output$plot6 <- renderPlot({
    g = nydoe %>% filter(Name == input$Name) %>% select(Year, Absent,ELL.,Diab.)
    colnames(g)[2:4] = c("Chronically Absent","English Lang.Learners","Special Needs")
    g = pivot_longer(g,c(2:4), names_to="Type", values_to = "Pct")
    
    ggplot(g)+geom_line(aes(x=Year, y=Pct, color=Type),lwd=3)+
      geom_point(aes(x=Year, y=Pct),color="black",alpha=.5) + theme(legend.position = "right")+
      labs(x="Year",
           y="%")+scale_color_brewer(palette="Dark2")+theme_classic()
  })
  
  # attendance graph
  output$plot7 <- renderPlot({
    school_info = nydoe %>% filter(Name == input$Name) %>% select(Year, Attendance,Teacher.Attendance)
    colnames(school_info)[2:3] = c("Attendance ","Teacher Attendance ")
    school_info = pivot_longer(school_info,c(2:3), names_to="Type", values_to = "Pct")
    
    ggplot(school_info)+geom_line(aes(x=Year, y=Pct, color=Type),lwd=3)+
      geom_point(aes(x=Year, y=Pct),color="black",alpha=.5) + theme(legend.position = "right")+
      labs(x="Year",
           y="%")+scale_color_brewer(palette="Dark2")+theme_classic()
  })
  
  # demograpic for district (wrapped)
  output$plot8 <- renderPlot({
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
                                                            strip.text.x = element_text(size = 9))  
    })
  
  #quality ratings charts (district level)
  output$plot9 <- renderPlot({
    quality = nydoe %>% filter(District == "1", Year == 2019) %>% pivot_longer(c(27:32), names_to = "Quality", values_to = "QScore") %>% select(Year, Name, Quality, QScore) %>% 
      mutate(Quality=gsub(Quality,pattern="RI.Score",replacement="Rigorous Instruction")) %>% 
      mutate(Quality=gsub(Quality,pattern="SuprtEnv.Score",replacement="Supportive Enviornment")) %>%  
      mutate(Quality=gsub(Quality,pattern="CollabT.Score",replacement="Collaborative Teachers")) %>%     
      mutate(Quality=gsub(Quality,pattern="Comunty.Score",replacement="Community")) %>% 
      mutate(Quality=gsub(Quality,pattern="Leadrshp.Score",replacement="Leadership")) %>% 
      mutate(Quality=gsub(Quality,pattern="Trust.Score",replacement="Trust"))
      
    ggplot(data=quality, aes(Quality, QScore)) + geom_bar(aes(fill = Quality), stat='identity',) + coord_flip() + labs(y="Score",x="") + theme_classic() + 
      theme(legend.position ="top", axis.text.y = element_blank()) + scale_fill_brewer(palette="Set1") + facet_wrap(~Name)+ theme(strip.text.x = element_text(size = 9))
    
  })
  
  
  })






