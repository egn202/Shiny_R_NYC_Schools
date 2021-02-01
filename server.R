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
           y="Student Achievement Score")+scale_color_brewer(palette="Set1")+theme_minimal()
  })
  #quality ratings bar chart
  output$plot2 <- renderPlot({
    quality = nydoe %>% filter(Name == input$Name, Year == 2019) %>% pivot_longer(c(27:32), names_to = "Quality", values_to = "QScore") %>% select(Quality, QScore)
    quality$Quality = c("Rigorous Instruction","Collaborative Teachers","Supportive Environment","Leadership","Family-Community Ties","Trust")  
    
    ggplot(data=quality, aes(Quality, QScore)) + geom_bar(aes(fill = Quality), stat='identity',) + coord_flip() + 
      labs(y = "Rating",x="") +
      geom_hline(yintercept = 1, linetype="dashed", color = "grey", size=.25) + 
      geom_hline(yintercept = 2, linetype="dashed", color = "grey", size=.5) +
      geom_hline(yintercept = 3, linetype="dashed", color = "grey", size=.5) +
      geom_hline(yintercept = 4, linetype="dashed", color = "grey", size=.5) +
      theme_classic() + theme(legend.position ="none") + scale_fill_brewer(palette="Set1")

  })
  #ELA standards chart
  output$plot3 <- renderPlot({
    school = nydoe %>% filter(Name == input$Name) %>% select(Year, ELA.Std) %>% arrange(Year)
    district = nydoe %>% filter(District==input$District) %>% select(Year,ELA.Std) %>% group_by(Year) %>% summarise(ELA.Std = mean(ELA.Std, na.rm=T))
    city = city = nydoe %>% group_by(Year) %>% summarise(ELA.Std = mean(ELA.Std, na.rm=T))
    graph=cbind(school, district$ELA.Std,city$ELA.Std)
    colnames(graph)[2:4] = c("School","District","City")
    graph = pivot_longer(graph, c(2:4),names_to = "level", values_to = "score")
    
    ggplot(graph) + geom_line(aes(x=Year, y=score, color=level),lwd=3)+
      geom_point(aes(x=Year, y=score),color="black",alpha=.5) + theme(legend.position = "right")+
      labs(x="Year",
           y="% Children at ELA Std")+scale_color_brewer(palette="Set1")+theme_minimal()
  })
  #Math Standards Chart
  output$plot4 <- renderPlot({
    school = nydoe %>% filter(Name == input$Name) %>% select(Year, Math.Std) %>% arrange(Year)
    district = nydoe %>% filter(District==input$District) %>% select(Year,Math.Std) %>% group_by(Year) %>% summarise(Math.Std = mean(Math.Std, na.rm=T))
    city = city = nydoe %>% group_by(Year) %>% summarise(Math.Std = mean(Math.Std, na.rm=T))
    graph=cbind(school, district$Math.Std,city$Math.Std)
    colnames(graph)[2:4] = c("School","District","City")
    graph = pivot_longer(graph, c(2:4),names_to = "level", values_to = "score")
    
    ggplot(graph) + geom_line(aes(x=Year, y=score, color=level),lwd=3)+
      geom_point(aes(x=Year, y=score),color="black",alpha=.5) + theme(legend.position = "right")+
      labs(x="Year",
           y="% Children at Math Std")+scale_color_brewer(palette="Set1")+theme_minimal()
  })
  
  #School demographics
  output$plot5 <- renderPlot({
    nydoe %>% filter(Name == input$Name) %>% select(Year, Asian, Black, Hisp, White) %>% pivot_longer(c(2:5), names_to = "Race", values_to = "Pct") %>%
    ggplot()+geom_bar(aes(x=Year, y=Pct, fill=Race),stat="identity") + scale_fill_brewer(palette="Dark2")+theme_classic()      
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
           y="%")+scale_color_brewer(palette="Dark2")+theme_minimal()
  })
  
  # attendance graph
  output$plot7 <- renderPlot({
    school_info = nydoe %>% filter(Name == input$Name) %>% select(Year, Attendance,Teacher.Attendance)
    colnames(school_info)[2:3] = c("Attendance ","Teacher Attendance ")
    school_info = pivot_longer(school_info,c(2:3), names_to="Type", values_to = "Pct")
    
    ggplot(school_info)+geom_line(aes(x=Year, y=Pct, color=Type),lwd=3)+
      geom_point(aes(x=Year, y=Pct),color="black",alpha=.5) + theme(legend.position = "right")+
      labs(x="Year",
           y="%")+scale_color_brewer(palette="Set2")+theme_minimal()
  })
  
  # demographic for district (wrapped)
  output$plot8 <- renderPlot({
    nydoe %>% pivot_longer(c(Asian, Black, Hisp, White),
                           names_to = "Race",
                           values_to = "Enroll") %>% filter(District == input$District2) %>%
      ggplot() + geom_col(aes(x = Year, y = Enroll, fill = Race),stat = "identity",position = "fill") +
      scale_fill_brewer(palette = "Dark2") + theme(legend.position ="top",axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),panel.background = element_rect(fill = "white", )) + labs(y = "", x = "") + 
      geom_hline(yintercept = .25, linetype="dashed", color = "grey", size=.1) +
      geom_hline(yintercept = .5, linetype="dashed", color = "grey", size=.1) +
      geom_hline(yintercept = .75, linetype="dashed", color = "grey", size=.1) +
      facet_wrap(~ Name) + theme(strip.background = element_rect(color = "white"),strip.text.x = element_text(size = 9))  
    })
  
  #quality ratings charts (district level)
  output$plot9 <- renderPlot({
    quality = nydoe %>% filter(District == input$District2, Year == 2019) %>% pivot_longer(c(27:32), names_to = "Quality", values_to = "QScore") %>% select(Year, Name, Quality, QScore) %>% 
      mutate(Quality=gsub(Quality,pattern="RI.Score",replacement="Rigorous Instruction")) %>% 
      mutate(Quality=gsub(Quality,pattern="SuprtEnv.Score",replacement="Supportive Enviornment")) %>%  
      mutate(Quality=gsub(Quality,pattern="CollabT.Score",replacement="Collaborative Teachers")) %>%     
      mutate(Quality=gsub(Quality,pattern="Comunty.Score",replacement="Community")) %>% 
      mutate(Quality=gsub(Quality,pattern="Leadrshp.Score",replacement="Leadership")) %>% 
      mutate(Quality=gsub(Quality,pattern="Trust.Score",replacement="Trust"))
      
    ggplot(data=quality, aes(Quality, QScore)) + geom_bar(aes(fill = Quality), stat='identity',) + coord_flip() + 
      labs(y="Score",x="") + theme_classic() + theme(legend.position ="top", axis.text.y = element_blank()) + 
      scale_fill_brewer(palette="Set1") + 
      geom_hline(yintercept = 1, linetype="dashed", color = "grey", size=.25) + 
      geom_hline(yintercept = 2, linetype="dashed", color = "grey", size=.5) +
      geom_hline(yintercept = 3, linetype="dashed", color = "grey", size=.5) +
      geom_hline(yintercept = 4, linetype="dashed", color = "grey", size=.5) +
      facet_wrap(~Name)+ theme(strip.text.x = element_text(size = 9))
    })
  
  #Testing for district
  output$plot10 <- renderPlot({
    nydoe %>% filter(District==input$District2) %>%select(Year,District,Name, School.Number ,ELA.Std,Math.Std) %>% 
      pivot_longer(c(5:6), names_to ="Type", values_to = "Pct.at.Std") %>%
      ggplot() + geom_line(aes(x=Year, y=Pct.at.Std, color=Type),lwd=1.5) + 
      geom_point(aes(x=Year, y=Pct.at.Std),color="black",alpha=.25) +
      facet_wrap(~Name)+
      labs(x="Year",
           y="% Children at Standard")+scale_color_brewer(palette="Set1")+theme_bw()+
      theme(strip.text.x = element_text(size = 9))+ theme(legend.position = "top")
  })
  
  #Citywide test scores
  output$plot11 <- renderPlot({
    nydoe %>% select(Year,ELA.Std,Math.Std) %>% 
      pivot_longer(c(2,3), names_to ="Type", values_to = "Pct.at.Std") %>%
      group_by(Year,Type) %>% summarise(Pct.at.Std = mean(Pct.at.Std, na.rm=T)) %>% 
      ggplot() + geom_line(aes(x=Year, y=Pct.at.Std, color=Type),lwd=1.5) + 
      geom_point(aes(x=Year, y=Pct.at.Std),color="black",alpha=.25) +
      labs(x="Year",
           y="% Children at Standard")+scale_color_brewer(palette="Set1")+theme_bw()+
      theme(strip.text.x = element_text(size = 9))+ theme(legend.position = "bottom")
  })
  #density plot ELA citywide
  output$plot12 <- renderPlot({
    nydoe %>% ggplot(aes(x=ELA.Std, fill=(as.factor(Year)))) +
      geom_density( alpha=.4) +
      theme_ipsum()+ labs(x="ELA Standard",fill="")
  })
  #density plot math citywide
  output$plot13 <- renderPlot({
    nydoe %>% ggplot(aes(x=Math.Std, fill=(as.factor(Year)))) +
      geom_density( alpha=.4) +
      theme_ipsum()+ labs(x="Math Standard",fill="")
  })
  #boxplot city ELA
  output$plot14 <- renderPlot({
    nydoe %>% filter(Year=="2019", District !="84") %>% 
      ggplot(aes(x=District, y=ELA.Std))+geom_boxplot(aes(fill=Borough)) + 
      scale_fill_brewer(palette="Set1")+labs(y="Children at ELA Standard")+ theme_bw() +ggtitle("Citywide ELA Testing 2019")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  #Boxplot city Math
  output$plot15 <- renderPlot({
    nydoe %>% filter(Year=="2019", District !="84") %>% 
      ggplot(aes(x=District, y=Math.Std))+geom_boxplot(aes(fill=Borough)) + 
      scale_fill_brewer(palette="Set1")+labs(y="Children at Math Standard")+ theme_bw() +ggtitle("Citywide Math Testing 2019")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # All test scores - 2019 schools for city tab DataTable
  output$table2 <- DT::renderDataTable({
    all_schools = nydoe %>% filter(Year=="2019") %>% select(Borough,District,School.Number,Name,ELA.Std,Math.Std,SA.Score)
    colnames(all_schools) = c("Borough","District","Number","Name","% at ELA Std","% at Math Std","Student Achievement Score")
    datatable(all_schools, rownames=FALSE, options = list(pageLength = 5,lengthMenu = c(5,10))) 
  })

  #Scatter: quality vs testing
  output$plot16 <- renderPlot({
    cor1 = scatter1 %>% filter(Quality == input$quality1, Test == input$test1) %>% summarise(Cor = cor(QScore,TScore, use="complete.obs"))

    scatter1 %>% filter(Quality == input$quality1, Test == input$test1) %>% 
      ggplot(aes(x=QScore, y=TScore, color=Quality))+geom_point(col="#CC33FF", alpha=.5)+ geom_smooth(col="#666666", se=FALSE)+theme_light()+
      xlab(input$quality1)+ ylab(input$test1)+ ggtitle(paste0("Cor:", round(cor1[[1]],digits=2)))
  })  
  
  #Scatter: quality vs testing
  output$plot17 <- renderPlot({
    cor2 = scatter1 %>% filter(Quality == input$quality2, Test == input$test2) %>% summarise(Cor = cor(QScore,TScore, use="complete.obs"))
    
    scatter1 %>% filter(Quality == input$quality2, Test == input$test2) %>% 
      ggplot(aes(x=QScore, y=TScore, color=Quality))+geom_point(col="#00CC66", alpha=.5)+ geom_smooth(col="#666666", se=FALSE)+theme_light()+
      xlab(input$quality2)+ ylab(input$test2) + ggtitle(paste0("Cor:", round(cor2[[1]],digits=2)))
    
  })  
  
  #Scatter: economic need vs testing
  output$plot18 <- renderPlot({
    nydoe %>% select(Math.Std,ELA.Std,EconNeed.)%>%  pivot_longer(c(1,2), names_to = "Test", values_to = "TScore") %>% 
    ggplot(aes(x=EconNeed., y=TScore))+geom_point(col="#CC3333", alpha=.5)+ geom_smooth(col="#666666", se=FALSE)+theme_light()+
      xlab("Economic Need")+ylab("Test Performance")+ggtitle("Testing Performance and Economic Need (Cor:-.65)")
  })    
  
  #scatter: econ need vs score
  output$plot19 <- renderPlot({
    nydoe %>% select(Attendance,EconNeed.)%>% 
      ggplot(aes(x=EconNeed., y=Attendance))+geom_point(col="#663300", alpha=.5)+ 
      geom_smooth(col="#666666", se=FALSE, method="lm")+theme_light()+ coord_cartesian(ylim = c(85,100))+
      ggtitle("Attendance and Economic Need")
    
  })  
  
  output$avgmathEco = renderInfoBox({
    avgmath_eco = nydoe %>% filter(EconNeed.>=86.10) %>% summarise(avgMth=mean(mean(Math.Std, na.rm=TRUE)))
    infoBox(h6(HTML("Math Performance<br/>(Upper Quartile of<br/>Economic Need)")),round(avgmath_eco,digits=2), 
    icon = icon("calculator"), width=5, col="orange") 
    
  })
  
  output$avgELAEco = renderInfoBox({
    avgELA_eco = nydoe %>% filter(EconNeed.>=86.10) %>% summarise(avgELA=mean(mean(ELA.Std, na.rm=TRUE)))
    infoBox(h6(HTML("ELA Performance<br/>(Upper Quartile of<br/>Economic Need)")),round(avgELA_eco,digits=2), 
            icon = icon("readme"), width=5, col="orange") 
    
  })
})




