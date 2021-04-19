library(shiny)
library(tidyverse)
library(plotly)
library(tidyr)
library(zoo)
library(DT)
library(RColorBrewer)
library(shinydashboard)

######################    Maplet   #########################
#          Jinfeng Lu, Xiang Zhu, Yifan Wu                 #
#                                                          #
#                                                          #
############################################################


ui <- shinyUI(fluidPage(
  # Create a panel containing an application title.
  titlePanel("", windowTitle = "Maplet"),
  
  # load custom stylesheet
  includeCSS("www/style.css"),
  
  # load Google analytics script
  tags$head(includeScript("www/google-analytics-bioNPS.js")),
  
  # remove shiny "red" warning messages on GUI
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  # load page layout
  dashboardPage(
    # Change color
    skin = "red",
    dashboardHeader(title = span("Maplet", style = "font-size: 22px; font-weight: bold"),
                    titleWidth = 200),
    dashboardSidebar(width = 200,
                     sidebarMenu(
                       HTML(paste0(
                         "<br>",
                         "<a href='https://github.com/krumsieklab/maplet' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='maplet_hexagon.png' width = '150'></a>",
                         "<br>",
                         "<p style = 'text-align: center;'><small><a href='https://bio.nifdc.org.cn/pqf/search.do?formAction=pqfGsByJG&orgId=1' target='_blank'>Weill Cornell Medicine</br>BDS Capstone</a></small></p>",
                         "<br>"
                       )),
                       menuItem("Module 1", tabName = "mod1", icon = icon("home")),
                       menuItem("Module 2", tabName = "mod2", icon = icon("spinner")),
                       menuItem("Module 3", tabName = "mod3", icon = icon("calendar")),
                       menuItem("Module 4", tabName = "mod4", icon = icon("stats", lib = "glyphicon")),
                       menuItem("Module 5", tabName = "mod5", icon = icon("flask")),
                       menuItem("Module 6", tabName = "mod6", icon = icon("address-card")),
                       HTML(paste0(
                         "<br>", "<br>","<br>","<br>",
                         "<a href='https://weill.cornell.edu' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='WCM.png' width = '200'></a>"
                       ))
                     )
                     
    ), # end dashboardSidebar
    
    dashboardBody(
      
      tabItems(
        tabItem(tabName = "mod1",
                includeMarkdown("www/module1.md")
        ),
        tabItem(tabName = "mod2",
                includeMarkdown("www/module2.md")
        ),
        tabItem(tabName = "mod3",
                includeMarkdown("www/module3.md")
        ),
        tabItem(tabName = "mod4",
                includeMarkdown("www/module4.md")
        ),
        tabItem(tabName = "mod5",
                includeMarkdown("www/module5.md")
        ),
        tabItem(tabName = "mod6",
                includeMarkdown("www/module6.md")
        )
      )
      
    ) # end dashboardBody
    
  )# end dashboardPage
  
))


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  
})

shinyApp(ui = ui, server = server)
