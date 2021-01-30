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
    menuItem("School", tabName = "school", icon = icon("dashboard")),
    menuItem("District", tabName = "district", icon = icon("th")),
    menuItem(
      "Source code",
      icon = icon("file-code-o"),
      href = "https://github.com/rstudio/shinydashboard/"
    ) #creates link to another website
  )),
  
  
  dashboardBody(tabItems(
    # First tab content
    tabItem(
      tabName = "school",
      fluidRow(
        box(
          title = "Student Achievement",
          status = "primary",
          solidHeader = TRUE,
          #status is the color of header
          collapsible = TRUE,
          plotOutput("plot1", height = 250)
        ),
        
        box(
          title = "Quality Ratings",
          status = "primary",
          solidHeader = TRUE,
          #status is the color of header
          collapsible = TRUE,
          plotOutput("plot2", height = 250)
        )
      ),
      fluidRow(
        box(
          title = "English Scores",
          status = "primary",
          solidHeader = TRUE,
          #status is the color of header
          collapsible = TRUE,
          plotOutput("plot3", height = 250)
        ),
        
        box(
          title = "Math Scores",
          status = "primary",
          solidHeader = TRUE,
          #status is the color of header
          collapsible = TRUE,
          plotOutput("plot4", height = 250)
        )
      ),
      
      fluidRow(
        tabBox(
          title = "First tabBox",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset1",
          height = "250px",
          tabPanel("Tab1", "First tab content"),
          tabPanel("Tab2", "Tab content 2")
        ),
        tabBox(
          side = "right",
          height = "250px",
          selected = "Tab3",
          tabPanel("Tab1", "Tab content 1"),
          tabPanel("Tab2", "Tab content 2"),
          tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
        )
      ),
      fluidRow(tabBox(
        # Title can include an icon
        title = tagList(shiny::icon("gear"), "tabBox status"),
        tabPanel(
          "Tab1",
          "Currently selected tab from first box:",
          verbatimTextOutput("tabset1Selected")
        ),
        tabPanel("Tab2", "Tab content 2")
      ))
    ),
    
    
    # Second tab content
    tabItem(tabName = "district",
            h2("district tab content"))
  ))
))