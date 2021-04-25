library(shiny)
library(tidyverse)
library(plotly)
library(tidyr)
library(zoo)
library(DT)
library(RColorBrewer)
library(shinydashboard)
library(maplet)
library(plotly)

######################    Maplet   #########################
#          Jinfeng Lu, Xiang Zhu, Yifan Wu                 #
#                                                          #
#                                                          #
############################################################

D <-
  # load data
  mt_load_metabolon_v1(file = system.file("extdata", "example_data/sampledata.xlsx", package = "maplet"), sheet = "OrigScale") %>%
  # timing start
  mt_reporting_tic() %>%
  
  ###
  # heading
  mt_reporting_heading(heading = "Preprocessing") %>%
  mt_reporting_heading(heading = "Part 1", lvl=2) %>%
  # sample boxplot
  mt_plots_sample_boxplot() %>%
  # missingness plot
  mt_plots_missingness() %>%
  # filter metabolites with >20% missing values, then samples with >10% missing values
  mt_pre_filter_missingness(feat_max=0.2) %>%
  mt_pre_filter_missingness(samp_max=0.1) %>%
  # batch correction by variable BATCH_MOCK
  mt_pre_batch_median(batch_col = "BATCH_MOCK") %>%
  # heading
  mt_reporting_heading(heading = "Part 2", lvl=2) %>%
  # quotient normalization
  mt_pre_norm_quot() %>%
  # check if there is any correlation between normalization factors and outcomes (bad sign if so)
  mt_plots_dilution_factor(in_col="num1") %>%
  mt_plots_dilution_factor(in_col="Group") %>%
  # logging
  mt_pre_trans_log() %>%
  # KNN imputation
  mt_pre_impute_knn() %>%
  # outlier detection (multivariate) & visualization
  # mt_pre_outlier(method="mahalanobis", pval=0.01, reduce.dim = T) %>%
  mt_plots_pca(color='outlier_mahalanobis') %>%
  # final sample boxplot
  mt_plots_sample_boxplot(color=Group, title = 'final') %>%
  # PCA, colored by some rowData() fields... this function shows 2 PCs
  mt_plots_pca(color=Group, shape=BATCH_MOCK, size=NUM_MOCK) %>%
  # heatmap
  mt_plots_heatmap(scale_data = T) %>%
  
  ###
  mt_reporting_heading(heading = "Statistics") %>%
  # linear model, differential test on Group
  mt_stats_univ_lm(
    formula      = ~ Group,
    samp_filter = (Group %in% c("treatment1","treatment2")),
    stat_name         = "comp",
    n_cores     = 1
  ) %>%
  # add fold changes to result tables
  mt_post_fold_change(stat_name = "comp") %>%
  # add multiple testing correction
  mt_post_multtest(stat_name = "comp", method = "BH") %>%
  # p-value histogram
  mt_plots_pval_hist() %>%
  # Volcano plot as overview of results
  mt_plots_volcano(stat_name     = "comp",
                   feat_filter = p.adj < 0.1,
                   colour       = p.value < 0.05) %>%
  
  # final timing
  mt_reporting_toc() %>%
  
  {.}


# Get the next names by selected name list
# funs: a vector (eg: c("plots", "van"))
get_next_fun_names <- function(funs) {
  fun_names <- list()
  for (result in metadata(D)$results) {
    fun_names = append(names, list(result$fun))
  }
  
  next_name <- c()
  if (length(funs) == 1) {
    for (name in fun_names) {
      if (name[1] == funs) {
        next_name <- c(next_name, name[2])
      }
    }
  } else {
    for (name in fun_names) {
      if (exists(name[3])) {
        if ((name[1] == funs[1]) & (name[2] == funs[2])) {
          next_name <- c(next_name, name[3])
        }
      }
    }
  }
  next_name = sort(next_name)[!duplicated(sort(next_name))]
  return(next_name)
}


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
                includeMarkdown("www/module1.md"),
                br(),
                br(),
                fluidRow(
                  column(3,
                         selectInput(
                           "mod1.1",
                           label = "type1",
                           choices = c("load",
                                       "reporting",
                                       "plots",
                                       "pre",
                                       "stats",
                                       "post"),
                           selected = "plots"
                         )),
                  column(1, h2("|"), class = "text-center", 
                         style = "margin-top: -5px; "),
                  column(3, uiOutput("mod1.2")),
                  column(1, h2("|"), class = "text-center", 
                         style = "margin-top: -5px; "),
                  column(3, uiOutput("mod1.3"))),  
                br(),
                plotOutput("mod1.4"),
                #DT::dataTableOutput("mod1.5")
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

  output$mod1.2 = renderUI({
    selectInput(
      "mod1.2",
      label = "type2",
      choices = get_next_fun_names(input$"mod1.1"),
      selected = ""
    )
  })
  
  output$mod1.3 = renderUI({
    selectInput(
      "mod1.3",
      label = "type3",
      choices = get_next_fun_names(c(input$"mod1.1", input$"mod1.2")),
      selected = ""
    )
  })
  
  output$mod1.4 <- renderPlot({
    
    if (input$"mod1.1" == "plots") {
      if (input$"mod1.3" == "") {
        results <- D %>% mtm_res_get_entries(c(input$"mod1.1", input$"mod1.2"))
      } else {
        results <- D %>% mtm_res_get_entries(c(input$"mod1.1", input$"mod1.2", input$"mod1.3"))
      }
    }
    
    plots <- list()
    for (i in 1:length(results)) {
      plots[i] <- results[[i]]$output
    }
    grid.arrange(grobs = plots, ncol=1)
  })
  

})

shinyApp(ui = ui, server = server)
