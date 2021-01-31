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
    menuItem("Source code",icon = icon("file-code-o"),href = "https://github.com/rstudio/shinydashboard/") #creates link to another website
  )),
  
  dashboardBody(tabItems(
    # First tab content
    tabItem(
      tabName = "school",
      fluidRow(
        box(selectizeInput(inputId = "District",label = "Select District",choices = unique(nydoe$District))),
        box(selectizeInput(inputId = "Name",label = "Select School",choices = unique(nydoe$Name)))
      ),
      fluidRow(
        box(title = "Student Achievement",status = "primary",solidHeader = TRUE,collapsible = TRUE, 
            plotOutput("plot1", height = 250)),
        box(title = "Quality Ratings",status = "primary",solidHeader = TRUE,collapsible = TRUE,
          plotOutput("plot2", height = 250))
      ),
      fluidRow(
        box(title = "English Language Art (ELA)",status = "primary",solidHeader = TRUE,collapsible = TRUE,
          plotOutput("plot3", height = 250)),
        box(title = "Math",status = "primary",solidHeader = TRUE,collapsible = TRUE,
          plotOutput("plot4", height = 250))
      ),
      fluidRow(
        box(title = "Demographics",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 5,
          plotOutput("plot5", height = 250)),
        tabBox(title = "Additional Info",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset1",height = "250px",width = 7,
          tabPanel("General Info", DT::dataTableOutput("table1")),
          tabPanel("Attendance", plotOutput("plot7",height=250)),
          tabPanel("Student Info", plotOutput("plot6",height=250)))
      )
    ),
      # fluidRow(tabBox(
      #   # Title can include an icon
      #   title = tagList(shiny::icon("gear"), "tabBox status"),
      #   tabPanel(
      #     "Tab1",
      #     "Currently selected tab from first box:",
      #     verbatimTextOutput("tabset1Selected")
      #   ),
      #   tabPanel("Tab2", "Tab content 2")
      # ))

    # Second tab content
    tabItem(tabName = "district",
      # fluidRow(
      #   box(selectizeInput(inputId = "District2",label = "District",choices = unique(nydoe$District))),
      #   ),
      tabBox(title = tagList(shiny::icon("chalkboard-teacher"),""), # The id lets us use input$tabset1 on the server to find the current tab
       id = "tabset1",width = 13,
       tabPanel("Quality Survey", plotOutput("plot9",height=800)),
       tabPanel("Student Achievement", plotOutput("plot10",height=800)),
       tabPanel("Demographics", plotOutput("plot8",height=800))
       )          
      ),
    
    tabItem(tabName = "city",
      DT::dataTableOutput("table2"),
    # fluidRow(
      #   # box(title = "Citywide Student Testing",status = "primary",solidHeader = TRUE,collapsible = TRUE, 
      #   #     plotOutput("plot11", height = 250)),
      #   box(title = "Need a title pls",status = "primary",solidHeader = FALSE,collapsible = TRUE, height = 450, width=9,
      #       div(style="overflow-y: scroll", DT::dataTableOutput("table2")))
      # ),

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
            # fluidRow(
            #   box(selectizeInput(inputId = "District2",label = "District",choices = unique(nydoe$District))),
            #   ),
            tabBox(title = tagList(shiny::icon("chalkboard-teacher"),""), # The id lets us use input$tabset1 on the server to find the current tab
                   id = "tabset2",width = 13,
                   tabPanel("Scatter Plots",
                            fluidRow(
                              column(3,selectizeInput(inputId = "quality1",label = "Quality Rating",choices = unique(scatter1$Quality))),
                              column(3,selectizeInput(inputId = "test1",label = "Test Type: Math or ELA",choices = unique(scatter1$Test))),
                              column(3,selectizeInput(inputId = "quality2",label = "Quality Rating",choices = unique(scatter1$Quality))),
                              column(3,selectizeInput(inputId = "test2",label = "Test Type: Math or ELA",choices = unique(scatter1$Test)))
                            ),
                            fluidRow(
                              column(6,plotOutput("plot16", height = 250)),
                              column(6,plotOutput("plot17", height = 250))
                            ),
                            
                            fluidRow(
                              box(title = "Scatter Me!",status = "primary",solidHeader = FALSE,collapsible = TRUE,),

                              box(title = "Citywide Math Testing",status = "primary",solidHeader = FALSE,collapsible = TRUE
                                  ))
                            )),
                   #tabPanel("Student Achievement", plotOutput("plot10",height=800)),
                   tabPanel("Mega Pots!")
            )          
      )
    )
))