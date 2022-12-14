# Install and load required packages
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(MASS)) install.packages("MASS", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(reshape)) install.packages("reshape", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(readxl)
library(ggplot2)
library(plotly)
library(MASS)
library(reshape2)
library(reshape)
library(broom)

# Load and prepare dataset for page 1
res_yearly <- read_csv("resignation_rate_over_years.csv")
res_yearly_cols <- res_yearly %>% 
  dplyr::select(industry1, industry2)
str(res_yearly)
max(res_yearly$year)

# Load and prepare dataset for page 2 - model
regression_df <- read.csv("resignation_rate_by_factors.csv")
regression_df$Proportion_Annual_Leave <- as.factor(regression_df$Proportion_Annual_Leave)
model <- lm(Resignation~Median_monthly_income+Av_weekly_actual_hours_worked+Proportion_flexi_work_arrangement+Proportion_Annual_Leave, data = regression_df)
step <- stepAIC(model, direction ="both")
regression_plot_df <- step %>% augment()
regression_plot_df <- data.frame(regression_plot_df)

# Global variables
MIN_RES = min(res_yearly$resignation_rate)
MAX_RES = max(res_yearly$resignation_rate)
c(MIN_RES,MAX_RES)

MIN_INCOME = min(as.numeric(regression_df$Median_monthly_income), na.rm=TRUE)
MAX_INCOME = max(as.numeric(regression_df$Median_monthly_income), na.rm=TRUE)

MIN_HOURS = min(regression_df$Av_weekly_actual_hours_worked)
MAX_HOURS = max(regression_df$Av_weekly_actual_hours_worked)

MIN_FLEXI = min(regression_df$Proportion_flexi_work_arrangement)
MAX_FLEXI = max(regression_df$Proportion_flexi_work_arrangement)

####################################
# User interface                   #
####################################

ui <- fluidPage(theme = shinytheme("flatly"),
  
  navbarPage(
    
    "Resignation Rates in Singapore",
    
    # Yearly Trend tab
    tabPanel("Yearly Trends",
             
             # Page Intro
             h3("Resignation Rates over the Years"),
             fluidRow(column(
                        p("This page is for exploring the trend of resignation rates in Singapore by Industry and Occupation Group over the years.",br(),br(),
                        "Select", strong("Industry Level, "), strong("Industry Categories, "), "and", strong("Occupation Categories"), "from drop-down menu and change",
                        strong("Minimum Year"), "to update the plot. Hover over the plot to view details.",
                        style="text-align:justify;color:black;color:#045a8d;background-color:#ECF0F1;padding:15px;border-radius:10px"),
                        width=12)),
             br(),
             
             # Sidebar Panel - choosing plot variables
             sidebarPanel(
               varSelectInput("level_select", "Select Industry Level:",
                              data = res_yearly_cols,
                              multiple = FALSE),
               span(tags$i(h6("Industry1 is tied to industry categories at Singapore Standard Industrial Classification (SSIC) section level")), style="color:#045a8d"),
               span(tags$i(h6("Industry2 is tied to industry categories at SSIC division level")), style="color:#045a8d"),
               tags$br(),
               
               pickerInput("industry_select", "Industry Categories (Dropdown to select one or more): ",
                           choices = NULL,
                           multiple = TRUE),
               pickerInput("occupation_select", "Occupation Categories:",
                           choices = unique(res_yearly$occupation1),
                           multiple = FALSE),
               sliderInput("minimum_year",
                           "Minimum Year:",
                           min = as.integer(min(res_yearly$year)),
                           max = as.integer(max(res_yearly$year)-1),
                           value=as.integer(min(res_yearly$year)),
                           step = 1, sep="")
              ),
             
             # Main Panel - generate plot
             mainPanel(
               h4("Resignation Rate % over the years by selected Industry and Occupation"),
               plotlyOutput("plot", width = "700px", height = "520px")
               )
    ),
    
    # Workplace Factors tab
    tabPanel("Prediction by Workplace Factors",
             
             # Page Intro
             h3("Prediction of Resignation Rates by Workplace Factors"),
             fluidRow(column(
               p("This page is for exploring the relationship between workplace factors like Median Monthly Income, Average Weekly Hours Worked
                 and Proportion of Flexible Work Arrangement with Resignation Rate.",br(),br(),
                 "Select values of", strong("Median Monthly Income, "), strong("Average Weekly Hours Worked"), "and", strong("Proportion of Flexible Work Arrangement"),
                 "to see the predicted Resignation Rate.",
                 style="text-align:justify;color:black;color:#045a8d;background-color:#ECF0F1;padding:15px;border-radius:10px"),
               width=12)),
             br(),
             
             # Sidebar Panel - choosing prediction variables
             sidebarPanel(
               
               sliderInput("income",
                           "Median Monthly Income:",
                           min = MIN_INCOME,
                           max = MAX_INCOME,
                           value = 3500,
                           step = 250),
               sliderInput("weeklyhr",
                           "Average Weekly Hours Worked:",
                           min = MIN_HOURS,
                           max = MAX_HOURS,
                           value = 45,
                           step = 1),
               sliderInput("flexi",
                           "Proportion of Flexible Work Arrangement (%):",
                           min = MIN_FLEXI,
                           max = MAX_FLEXI,
                           value = 90,
                           step = 1)
             ),
             
             # Mainbar Panel - show outcome of prediction
             mainPanel(
               
               # Regression Plot
               h4("Resignation Rate % by the Independent Variables"),
               plotlyOutput("regression_plot", width = "700px", height = "350px"),
               
               # Prediction Variables and Results
               br(),
               "Based on your inputs, predicted Resignation Rate % is:",
               verbatimTextOutput("prediction")
               
             )
    ),
    
    # About this page tab
    tabPanel("About this page",
             
             div(
               h4("Last update"), 
               h6("31-Oct-2022"),

               br(),
               
               h4("Background"), 
               "The Great Resignation refers to a global workforce trend where a significant number of employees are voluntarily leaving their jobs during or towards the end of the pandemic.
               This study was initiated following the Great Resignation to take a deeper look into the resignation trends in Singapore.",
               br(),br(),
               "In our study, we have looked into the resignation rates in Singapore over the years, across industries and occupations, as well as in relation to other workplace factors.
               Through the analysis, we hope to better inform the Singapore Government and related service providers on the trends and underlying reasons that might be driving resignations.
               Ultimately, we aim to empower decision makers to tailor manpower policies to meet the needs of different industries, and in the long run, attain Singapore’s larger economic goals.",

               br(),br(),
               
               h4("Sources"),
               tags$b("Average Monthly Resignation Rates by Industry and Occupational Group (2012 to 2021): "), tags$a(href="https://data.gov.sg/dataset/average-monthly-recruitment-resignation-rates-by-industry-and-occupational-group-annual?resource_id=243896d0-1974-4fee-a4d8-613641b230ad", "Data.gov.sg"),
               
               br(),
               tags$b("Median Monthly Gross Wages within Each Major Occupational Groups by Industry (2021): "), tags$a(href="https://stats.mom.gov.sg/Pages/Occupational-Wages-Tables2021.aspx", "Stats.mom.gov.sg"),
               
               br(),
               tags$b("Average Actual Hours Worked per Week by Industry (2020): "), tags$a(href="https://stats.mom.gov.sg/Pages/Singapore-Yearbook-Of-Manpower-Statistics-2021-Employment-Hours-Worked-and-Conditions-Of-Employment.aspx", "Stats.mom.gov.sg"),
               
               br(),
               tags$b("Distribution of Full-Time Employees By Annual Leave Entitlement by Industry (2020): "), tags$a(href="https://data.gov.sg/dataset/distribution-of-full-time-employees-by-annual-leave-entitlement?resource_id=1b56422d-73e4-4853-9aab-88a8ddb753af", "Data.gov.sg"),
               
               br(),
               tags$b("Proportion of Establishments Offering Formal Flexible Work Arrangements by Industry (2021): "), tags$a(href="https://stats.mom.gov.sg/Pages/Employment-Conditions-Tables2021.aspx", "Stats.mom.gov.sg"),
               
               br(),br(),
               
               h4("Authors"),
               "Neo Yi Xin,  Loh Jiahui,  Zhu Yiting,  Sherinah Binte Rashid,  Fong Bao Xian",br(),
               "Singapore Management University",br(),
               "Masters of IT in Business graduate students",br(),
             )

    )

  )
)

####################################
# Server                           #
####################################

server <- function(input, output, session) {
  
  # Filter dataset based on level_select input
  selected_industry_level <- reactive({
    
    if (input$level_select=="industry1") {
      df <- dplyr::select(res_yearly, industry1) %>%
        dplyr::rename(industry=1) %>% 
        dplyr::arrange(industry)
    }
    
    if (input$level_select=="industry2") {
      df <- dplyr::select(res_yearly, industry2) %>%
        filter(industry2 != "total") %>%
        dplyr::rename(industry=1) %>% 
        dplyr::arrange(industry)
    }
    df
    
  })
  
  # Update industry categories in industry_select based on level_select input
  observeEvent(selected_industry_level(), {
    industry_names <- unique(selected_industry_level()$industry)
    updatePickerInput(session = session,
                      inputId = "industry_select",
                      choices = industry_names,
                      selected = industry_names[1:5])
  })
  
  # Update plot dataset based on selections in industry_select and occupation_select
  plot_data <- reactive({
    req(input$level_select, input$industry_select, input$occupation_select)
    
    if (input$level_select=="industry1") {
      df <- dplyr::select(res_yearly, industry1, industry2, year, occupation1, resignation_rate) %>%
        filter(industry2 == "total") %>%
        filter(industry1 %in% input$industry_select) %>%
        filter(occupation1 == input$occupation_select) %>%
        dplyr::rename(industry=1)%>%
        filter(year>=input$minimum_year)
    }

    if (input$level_select=="industry2") {
      df <- dplyr::select(res_yearly, industry2, year, occupation1, resignation_rate) %>%
        filter(industry2 %in% input$industry_select & industry2 != "total") %>%
        filter(occupation1 == input$occupation_select) %>%
        dplyr::rename(industry=1) %>%
        filter(year>input$minimum_year)
    }
    
    df
  })
  
  # Render plot for yearly trend
  output$plot <- renderPlotly({
    g <- ggplot(plot_data(), aes(x=year, y=resignation_rate, color=industry)) +
      geom_point(alpha=0.8) +
      geom_line() +
      scale_x_continuous("Year", breaks=unique(plot_data()$year)) +
      scale_y_continuous("Resignation Rate %", limits=c(MIN_RES,MAX_RES)) +
      theme(legend.title=element_blank(),
            plot.margin = margin(5,5,5,5))
    
    # legend at bottom
    ggplotly(g) %>%
      layout(legend=list(
        orientation = "h", x = 0, y = -0.2,
        title=list(text="Industry Legend")))
  })
  
  # Render plot for regression
  output$regression_plot <- renderPlotly({
    regression_plot_df %>% melt(measure.vars=c("Median_monthly_income","Av_weekly_actual_hours_worked","Proportion_flexi_work_arrangement"), variable_name=c("IV")) %>% 
      ggplot(.,aes(value, Resignation)) +
      geom_smooth(method = "lm") +
      facet_wrap(~IV, scales = "free_x") +
      xlab("Value of Independent Variables") +
      ylab("Resignation Rate %")
  })
  
  # Filter dataset based on level_select input
  prediction_df <- reactive({
    prediction <- step %>% augment(newdata = data.frame(Median_monthly_income=input$income,
                                                        Av_weekly_actual_hours_worked=input$weeklyhr,
                                                        Proportion_flexi_work_arrangement=input$flexi))
    prediction$.fitted
  })
  
  # output for employment conditions tab
  output$prediction <- renderPrint(prediction_df())  

}


####################################
# Shiny App                        #
####################################

shinyApp(ui = ui, server = server)