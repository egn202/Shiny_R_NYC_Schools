## ui.R ##
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(viridis)
library(hrbrthemes)
library(DT)
library(htmltools)

shinyUI(dashboardPage(
  skin = "purple",
  dashboardHeader(title = "NYC Elementary & Middle Schools", titleWidth = 350),
  
  dashboardSidebar(sidebarMenu(
    img(src = "http://mannahattamamma.com/wp-content/uploads/2010/11/districtmapbig.gif", height = 240, width = 230),
    menuItem("School", tabName = "school", icon = icon("school")),
    menuItem("District", tabName = "district", icon = icon("layer-group")),
    menuSubItem(icon = NULL,
                selectizeInput(inputId = "District2",label = "District",choices = unique(nydoe$District))
    ),
    menuItem("City", tabName = "city", icon = icon("apple-alt")),
    menuItem("Insights", tabName = "insights", icon = icon("lightbulb")),
    menuItem("Source code",icon = icon("file-code-o"),href = "https://github.com/egn202/shinyProject.git")
  )),
  
  dashboardBody(tabItems(
    # First tab content
    tabItem(
      tabName = "school",
      h1("Individual School Dashboard"),
      h4("Please use this page to gather insights for a specific school. Answer questions about how it's performing
         versus its district or the city, the demographic make-up over time, and gain additional info about important 
         details such as attendance rates, enrollment, and principal tenure (info box at the bottom)."),
      h4("Note about the testing scores: Math and English Language Arts (ELA) are the key metrics used throughout this tool.
         The figures state what % of a given school is at standard. For example, a Math testing value of 35% means that 35% of
         students are meeting math standards. A little quirky, yes, welcome to NYC."),
      
      fluidRow(
        box(selectizeInput(inputId = "District",label = "Select District",choices = unique(nydoe$District))),
        box(selectizeInput(inputId = "Name",label = "Select School",choices = unique(nydoe$Name)))
      ),
      fluidRow(
        box(title = "Student Achievement",status = "primary",solidHeader = FALSE,collapsible = TRUE, 
            plotOutput("plot1", height = 250)),
        box(title = "Quality Ratings",status = "primary",solidHeader = FALSE,collapsible = TRUE,
          plotOutput("plot2", height = 250))
      ),
      fluidRow(
        box(title = "English Language Art (ELA)",status = "primary",solidHeader = FALSE,collapsible = TRUE,
          plotOutput("plot3", height = 250)),
        box(title = "Math",status = "primary",solidHeader = FALSE,collapsible = TRUE,
          plotOutput("plot4", height = 250))
      ),
      fluidRow(
        box(title = "Demographics",status = "primary",solidHeader = FALSE,collapsible = TRUE,width = 5,
          plotOutput("plot5", height = 250)),
        tabBox(title = "Additional Info",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset1",height = "250px",width = 7,
          tabPanel("General Info", DT::dataTableOutput("table1")),
          tabPanel("Attendance", plotOutput("plot7",height=250)),
          tabPanel("Student Info", plotOutput("plot6",height=250)))
      )
    ),
      
    # Second tab content
    tabItem(tabName = "district",
      h1("District Level Snapshot"),
      h4("Use this page to quickly spot the highest and lowest performing schools (qualitatively and testing-wise) in each district.
         In the demographics tab, see how the racial make-up of the schools and district have changed over a 5-year period."),
      h4("Please use the pulldown menu in the sidebar to change districts"),

      tabBox(title = tagList(shiny::icon("chalkboard-teacher"),""), # The id lets us use input$tabset1 on the server to find the current tab
       id = "tabset1",width = 13,
       tabPanel("Quality Survey", plotOutput("plot9",height=800)),
       tabPanel("Testing", plotOutput("plot10",height=800)),
       tabPanel("Demographics", plotOutput("plot8",height=800))
       )          
      ),
    
    tabItem(tabName = "city",
            h1("City Level Data and Analysis"),
            h4("Key Findings:"),
            h4("Highest performing districts are 2 and 26"),
            h4("Lowest performing districts are 23 and 32"),
            h4("Overall, testing is improving. The strong lower score skew in 2015 is gradually moving in the right direction"),
            h5("Use the table below to sort all schools by the type of test"),
      DT::dataTableOutput("table2"),

      fluidRow(
        box(title = "Citywide ELA Testing",status = "primary",solidHeader = FALSE,collapsible = TRUE,
            plotOutput("plot12", height = 250)),
        box(title = "Citywide Math Testing",status = "primary",solidHeader = FALSE,collapsible = TRUE,
            plotOutput("plot13", height = 250))
      ),
      plotOutput("plot14"),
      plotOutput("plot15")
      ),
    
    tabItem(tabName = "insights",
            
      tabBox(title = tagList(shiny::icon("chalkboard-teacher"),""), # The id lets us use input$tabset1 on the server to find the current tab
         id = "tabset2",width = 13,
         tabPanel("Quality:Testing",
            h1("Which school qualities are most important with respect to testing?"),
            h4("Finding: of the six Quality metrics reported for all NYC public schools, overall test performance (English Language Arts (ELA) and Math) 
            is most closely tied to a school's superior ability to provide a 'Supportive Enviornment' and 'Rigorous Instruction.'  
            Note also that 'Leadership' and 'Trust' appear to have the lowest impact on testing"),
            h4("Please use the tool below to explore how the a school's 'qualities' are correlated to test performance"),
            fluidRow(
              column(3,selectizeInput(inputId = "quality1",label = "Quality Rating",choices = unique(scatter1$Quality))),
              column(3,selectizeInput(inputId = "test1",label = "Test Type: Math or ELA",choices = unique(scatter1$Test))),
              column(3,selectizeInput(inputId = "quality2",label = "Quality Rating",choices = unique(scatter1$Quality))),
              column(3,selectizeInput(inputId = "test2",label = "Test Type: Math or ELA",choices = unique(scatter1$Test)))
            ),
            fluidRow(
              column(6,plotOutput("plot16", height = 350)),
              column(6,plotOutput("plot17", height = 350))
            ),
            ),
         tabPanel("Challenges",
            h1("Socioeconomic Challenges"),
            h4("The variable that had the strongest correlation was unfortunately a negative one where schools with the highest 
               Economic Need also had the lowest scores and lowest attendance rates. While this finding is not new or surprising, the data 
               further emphasizes the severity and complexity of the problem that probably cannot be solved through the school system alone."),
            infoBoxOutput("avgmathEco"),
            infoBoxOutput("avgELAEco"),
            infoBoxOutput("avgCity"),
            br(),
            plotOutput("plot18"),
            br(),br(),br(),br(),br(),
            plotOutput("plot19"),
            )
      ),
    )
  )
  )
))