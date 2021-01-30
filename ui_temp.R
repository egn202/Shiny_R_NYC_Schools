## ui.R ##
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(viridis)
library(hrbrthemes)

shinyUI(dashboardPage(
  skin = "purple",
  dashboardHeader(title = "NYC Elementary & Middle Schools", titleWidth = 350),
  
  dashboardSidebar(sidebarMenu(
    img(src = "http://mannahattamamma.com/wp-content/uploads/2010/11/districtmapbig.gif", height = 240, width = 230),
    menuItem("School", tabName = "school", icon = icon("school"),selectizeInput(inputId = "District",
                                                                                label = "Select District",
                                                                                choices = unique(nydoe$District)),
    menuItem("District", tabName = "district", icon = icon("layer-group")),
    menuItem("City", tabName = "city", icon = icon("apple-alt")),
    menuItem("Insights", tabName = "insights", icon = icon("lightbulb")),
    menuItem("Source code",icon = icon("file-code-o"),href = "https://github.com/rstudio/shinydashboard/") #creates link to another website
  )),
  
  
  dashboardBody(tabItems(
    # First tab content
    tabItem(
      tabName = "school",
      fluidRow(
        box(
          selectizeInput(inputId = "Name",
                         label = "Select School",
                         choices = unique(nydoe$Name)),  
        )
      ),
      
      fluidRow(
        box(
          title = "Student Achievement",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("plot1", height = 250)
        ),
        
        box(
          title = "Quality Ratings",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("plot2", height = 250)
        )
      ),
      fluidRow(
        box(
          title = "English Language Art (ELA)",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("plot3", height = 250)
        ),
        
        box(
          title = "Math",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("plot4", height = 250)
        )
      ),
      
      fluidRow(
        box(
          title = "Demographics",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 5,
          plotOutput("plot5", height = 250)
        ),
        
        tabBox(
          title = "Additional Info",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset1",
          height = "250px",
          width = 7,
          tabPanel("General Info", DT::dataTableOutput("table1")),
          tabPanel("Attendance", plotOutput("plot7",height=250)),
          tabPanel("Student Info", plotOutput("plot6",height=250))
        )
      ),
      # 
      # fluidRow(
      #   tabBox(
      #     side = "right",
      #     height = "250px",
      #     selected = "Tab3",
      #     tabPanel("Tab1", "eugene"),
      #     tabPanel("Tab2", "Tab content 2"),
      #     tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
      #   )
      # ),
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
    ),
    
    
    # Second tab content
    tabItem(tabName = "district",
            h2("district tab content"))
  ))
)))