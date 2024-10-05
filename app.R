require(shiny)
require(shinydashboard)
require(tidyverse)
require(plotly)  
require(wordcloud2)  
require(DT)  
require(rsconnect)

# Header
header <- dashboardHeader(title = "SpaceVisualizer")

# Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Upload Data", tabName = "upload", icon = icon("upload")), 
    menuItem("Summary", tabName = "summary", icon = icon("info-circle")),
    menuItem("Factors and Parameters", tabName = "factors", icon = icon("cogs")),
    menuItem("Comparison", tabName = "comparison", icon = icon("balance-scale"))
  )
)

# Body
body <- dashboardBody(
  tabItems(
    # First Tab - Upload Data
    tabItem(tabName = "upload", 
            fluidRow(
              box(
                title = "Experiment 1", status = "primary", solidHeader = TRUE,
                fileInput("exp1_cleaned_invest", "Cleaned Investigation File", accept = c(".tsv", ".txt")),
                fileInput("exp1_meta_exp", "Meta Exp File", accept = c(".tsv", ".txt"))
              ),
              box(
                title = "Experiment 2", status = "primary", solidHeader = TRUE,
                fileInput("exp2_cleaned_invest", "Cleaned Investigation File", accept = c(".tsv", ".txt")),
                fileInput("exp2_meta_exp", "Meta Exp File", accept = c(".tsv", ".txt"))
              )
            ),
            fluidRow(
              box(title = "Uploaded Files Summary", status = "primary", solidHeader = TRUE,
                  verbatimTextOutput("output_data"))
            )
    ),
    
    # Second Tab - Summary
    tabItem(tabName = "summary",
            fluidRow(
              box(
                title = "Select Experiment", status = "info", solidHeader = TRUE,
                selectInput("experiment_select", "Choose Experiment", 
                            choices = list("Experiment 1" = "exp1", "Experiment 2" = "exp2"))
              )
            ),
            fluidRow(
              box(
                title = "Experiment Summary and Word Cloud", status = "info", solidHeader = TRUE, width = 12,
                tabsetPanel(
                  tabPanel("Summary Output", verbatimTextOutput("summary_output")),
                  tabPanel("Word Cloud", wordcloud2Output("wordcloud_output", width = "100%", height = "400px"))
                )
              )
            )
    ),
    
    # Third Tab - Factors and Parameters
    tabItem(tabName = "factors",
            fluidRow(
              box(
                title = "Select Experiment", status = "warning", solidHeader = TRUE,
                selectInput("experiment_select_bar", "Choose Experiment", 
                            choices = list("Experiment 1" = "exp1", "Experiment 2" = "exp2"))
              )
            ),
            fluidRow(
              box(
                title = "Select Column for Bar Plot", status = "warning", solidHeader = TRUE,
                uiOutput("column_selector"),
                plotlyOutput("bar_plot", height = "400px")  
              )
            )
    ),
    
    # Fourth Tab - Comparison
    tabItem(tabName = "comparison",
            tabsetPanel(
              tabPanel("Experiment 1 Comparison",
                       fluidRow(
                         box(
                           title = "Experiment 1 Summary", status = "primary", solidHeader = TRUE, width = 12,  
                           DT::dataTableOutput("table_exp1")  
                         )
                       )
              ),
              tabPanel("Experiment 2 Comparison",
                       fluidRow(
                         box(
                           title = "Experiment 2 Summary", status = "primary", solidHeader = TRUE, width = 12,
                           DT::dataTableOutput("table_exp2")  
                         )
                       )
              )
            )
    )
  )
)

# UI setup
ui <- dashboardPage(skin="black",
                    header,
                    sidebar,
                    body
)

server <- function(input, output) {
  exp1_meta <- reactive({
    req(input$exp1_meta_exp)  
    read_tsv(input$exp1_meta_exp$datapath, col_names = TRUE) %>%
      rename_with(~ make.names(.))
  })
  
  exp1_inv <- reactive({
    req(input$exp1_cleaned_invest)  
    read_tsv(input$exp1_cleaned_invest$datapath, col_names = TRUE) %>%
      rename_with(~ make.names(.))
  })
  
  exp2_meta <- reactive({
    req(input$exp2_meta_exp)  
    read_tsv(input$exp2_meta_exp$datapath, col_names = TRUE) %>%
      rename_with(~ make.names(.))
  })
  
  exp2_inv <- reactive({
    req(input$exp2_cleaned_invest)  
    read_tsv(input$exp2_cleaned_invest$datapath, col_names = TRUE) %>%
      rename_with(~ make.names(.))
  })
  

  output$output_data <- renderPrint({
    
    if (is.null(input$exp1_cleaned_invest) || is.null(input$exp1_meta_exp)) {
      cat("Please upload both files for Experiment 1.\n")
    } else {
      cat("Experiment 1 Files:\n")
      cat("Cleaned Investigation File:", input$exp1_cleaned_invest$name, "\n")
      cat("Meta Exp File:", input$exp1_meta_exp$name, "\n")
    }
    
    if (is.null(input$exp2_cleaned_invest) || is.null(input$exp2_meta_exp)) {
      cat("\nPlease upload both files for Experiment 2.\n")
    } else {
      cat("\nExperiment 2 Files:\n")
      cat("Cleaned Investigation File:", input$exp2_cleaned_invest$name, "\n")
      cat("Meta Exp File:", input$exp2_meta_exp$name, "\n")
    }
  })
  
  output$summary_output <- renderPrint({
    req(input$experiment_select)  
    
    if (input$experiment_select == "exp1") {
      req(exp1_inv())  
      if (!is.null(exp1_inv())) {
        cat("Summary for Experiment 1:\n")
        cat("Study Identifier: ", exp1_inv()$`Study.Identifier`, "\n")
        cat("Study Title: ", exp1_inv()$`Study.Title`, "\n")
        cat("Study Description: ", exp1_inv()$`Study.Description`, "\n")
      } else {
        cat("No data available for Experiment 1.")
      }
    } else if (input$experiment_select == "exp2") {
      req(exp2_inv())  
      if (!is.null(exp2_inv())) {
        cat("Summary for Experiment 2:\n")
        cat("Study Identifier: ", exp2_inv()$`Study.Identifier`, "\n")
        cat("Study Title: ", exp2_inv()$`Study.Title`, "\n")
        cat("Study Description: ", exp2_inv()$`Study.Description`, "\n")
      } else {
        cat("No data available for Experiment 2.")
      }
    } else {
      cat("Please select an experiment.")
    }
  })
  
  
  output$wordcloud_output <- renderWordcloud2({
    req(input$experiment_select)  
    
    if (input$experiment_select == "exp1") {
      req(exp1_inv())  
      if ("Study.Description" %in% colnames(exp1_inv())) {
        text <- paste(exp1_inv()$`Study.Description`, collapse = " ")
        words <- unlist(strsplit(text, "\\W+"))
        words <- tolower(words)
        words <- words[nchar(words) > 0]
        
        word_freq <- as.data.frame(table(words))
        colnames(word_freq) <- c("word", "freq")
        
        if (nrow(word_freq) > 0) {
          wordcloud2(word_freq, size = 0.5)  
        } else {
          return(NULL)
        }
      } else {
        return(NULL)
      }
    } else if (input$experiment_select == "exp2") {
      req(exp2_inv())
      if ("Study.Description" %in% colnames(exp2_inv())) {
        text <- paste(exp2_inv()$`Study.Description`, collapse = " ")
        words <- unlist(strsplit(text, "\\W+"))
        words <- tolower(words)
        words <- words[nchar(words) > 0]
        
        word_freq <- as.data.frame(table(words))
        colnames(word_freq) <- c("word", "freq")
        
        if (nrow(word_freq) > 0) {
          wordcloud2(word_freq, size = 0.5)  
        } else {
          return(NULL)
        }
      } else {
        return(NULL)
      }
    }
  })
  
  output$column_selector <- renderUI({
    req(input$experiment_select_bar)  
    
    if (input$experiment_select_bar == "exp1") {
      req(exp1_meta())  
      selectInput("bar_column", "Choose Column for Bar Plot", choices = colnames(exp1_meta()), selected = colnames(exp1_meta())[1])
    } else if (input$experiment_select_bar == "exp2") {
      req(exp2_meta())  
      selectInput("bar_column", "Choose Column for Bar Plot", choices = colnames(exp2_meta()), selected = colnames(exp2_meta())[1])
    }
  })
  
  output$bar_plot <- renderPlotly({
    req(input$experiment_select_bar)  
    req(input$bar_column)  
    
    if (input$experiment_select_bar == "exp1") {
      req(exp1_meta())  
      data <- exp1_meta()
    } else if (input$experiment_select_bar == "exp2") {
      req(exp2_meta())  
      data <- exp2_meta()
    }
    
    plot_data <- data %>%
      count(!!sym(input$bar_column)) %>%
      arrange(desc(n))
    
    plot_ly(plot_data, x = ~get(input$bar_column), y = ~n, type = 'bar') %>%
      layout(title = paste("Bar Plot of", input$bar_column), 
             xaxis = list(title = input$bar_column),
             yaxis = list(title = "Count"))
  })
  
  
  output$table_exp1 <- DT::renderDataTable({
    req(exp1_meta())  
    datatable(exp1_meta(), options = list(pageLength = 5), rownames = FALSE)
  })
  
  
  output$table_exp2 <- DT::renderDataTable({
    req(exp2_meta())  
    datatable(exp2_meta(), options = list(pageLength = 5), rownames = FALSE)
  })
}

shinyApp(ui = ui, server = server)
