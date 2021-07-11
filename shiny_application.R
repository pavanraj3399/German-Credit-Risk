######## Setting working directory
setwd("~/Desktop/Shiny_application/application")


#####################################
######## Importing Libraries ########
######################################

library(dplyr)        # Data Manipulation
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(stringr)      # String Manipulation
library(tools)        # String Manipulation
library(data.table)
library(janitor)
library(reshape2)
library(highcharter)

###########################################
############ Importing Dataset ############
###########################################

# German credit data
german_credit_data <- readRDS("credit_data.rds")

# Training credit data
training_data <- readRDS("training_data.rds")

# Testing data
testing_data <- readRDS("testing_data.rds")

sample_model <- readRDS("sample_model.rds")


#########################################
############ Data Manipulation  #########
#########################################

# Create a Features Vector
feature_names <-
  c(
    "account_balance",
    "month_duration",
    "credit_history",
    "credit_purpose",
    "credit_amount",
    "savings_account_bonds",
    "employment_history",
    "installment_percent_disposable_income",
    "marital_status_sex",
    "debtors_guarantor_status",
    "residence_history_years",
    "property_type",
    "age_years",
    "other_installment_plans",
    "housing_type",
    "existing_credits_current_bank",
    "job_status",
    "people_liable_maintainance",
    "telephone_status",
    "foreign_worker",
    "credit_risk"
  )


# Subset Numerical Features
numerical_columns   <-
  c(
    "month_duration",
    "credit_amount",
    "installment_percent_disposable_income",
    "residence_history_years",
    "age_years",
    "existing_credits_current_bank",
    "people_liable_maintainance"
  )

# Subset Categorical Features
categorical_columns <-
  feature_names[!feature_names %in% numerical_columns]

# Radio button columns
radio_button_columns <- c("installment_percent_disposable_income","residence_history_years",
                          "existing_credits_current_bank","people_liable_maintainance")


###########################################
############ Shiny Application ############
###########################################




#
#' Title - Create picker widget for the columns for the user interface.
#'
#' @param features - All the features for the widget creation
#'
#' @return - return a list of picker widgets for the features passed.


picker_creator <- function(features) {
  picker_features <- features[!features %in% "credit_risk"]
  picker_list <- list()
  
  for (i in picker_features) {
    picker_widget <- div(class = "choosechannel",pickerInput(
      inputId = paste(i, "_id", sep = ""),
      label = toTitleCase(str_replace_all(i, "_", " ")),
      choices = levels(german_credit_data[, i]),
      options = list(
        title = paste("Select", toTitleCase(str_replace_all(i, "_", " "))),
        size = length(levels(german_credit_data[, i]))
      ),
      choicesOpt = list(style = rep(
        c(
          "color : grey ; background: white;font-weight: bold;"
        ),
        length(levels(german_credit_data[, i]))
      ))
    ))
    picker_list <- append(picker_list, list(picker_widget))
  }
  names(picker_list) <- picker_features
  return(picker_list)
}

picker_widget_list <- picker_creator(categorical_columns)



#' Title - Create Slider widget for the columns for the user interface.
#'
#' @param features - All the features for the widget creation
#'
#' @return - return a list of Slider widgets for the features passed.
#' 

slider_creator <- function(features) {
  slider_features <- features[!features %in% radio_button_columns]
  slider_list <- list()
  
  for (i in slider_features) {
    slider_widget <- div(class = "choosechannel",sliderTextInput(
      inputId = paste(i, "_id", sep = ""),
      label = toTitleCase(str_replace_all(i, "_", " ")),
      choices = unique(sort(german_credit_data[, i])),
      grid = FALSE,
      force_edges = TRUE
    ))
    slider_list <- append(slider_list, list(slider_widget))
  }
  names(slider_list) <- slider_features
  
  return(slider_list)
}

slider_widget_list <- slider_creator(numerical_columns)



#' Title - Create Radio button widget for the columns for the user interface.
#'
#' @param features - All the features for the widget creation
#'
#' @return - return a list of Radio button widgets for the features passed.


radio_creator <- function(features) {
  radio_features <- features
  
  radio_list <- list()
  
  for (i in radio_features) {
    radio_widget <- div(class = "choosechannel",awesomeRadio(
      inputId = paste(i, "_id", sep = ""),
      label = toTitleCase(str_replace_all(i, "_", " ")),
      choices = unique(sort(german_credit_data[, i])),
      inline = TRUE
    ))
    radio_list <- append(radio_list,list(radio_widget))
  }
  names(radio_list) <- radio_features
  return(radio_list)
}

radio_widget_list <- radio_creator(radio_button_columns)


# Server
server <- function(input, output, session) {
  
  data_set_input <- reactive({
    
    df <- data.frame(
      Name = c("account_balance",
               "credit_history",
               "credit_amount",
               "existing_credits_current_bank",
               "credit_purpose",
               "savings_account_bonds",
               "month_duration",
               "foreign_worker",
               "job_status",
               "employment_history",
               "other_installment_plans",
               "installment_percent_disposable_income",
               "marital_status_sex",
               "age_years",
               "property_type",
               "housing_type",
               "residence_history_years",
               "telephone_status",
               "debtors_guarantor_status",
               "people_liable_maintainance"
      ),
      Value =  c(input$account_balance_id,
                 input$credit_history_id,
                 input$credit_amount_id,
                 input$existing_credits_current_bank_id,
                 input$credit_purpose_id,
                 input$savings_account_bonds_id,
                 input$month_duration_id,
                 input$foreign_worker_id,
                 input$job_status_id,
                 input$employment_history_id,
                 input$other_installment_plans_id,
                 input$installment_percent_disposable_income_id,
                 input$marital_status_sex_id,
                 input$age_years_id,
                 input$property_type_id,
                 input$housing_type_id,
                 input$residence_history_years_id,
                 input$telephone_status_id,
                 input$debtors_guarantor_status_id,
                 input$people_liable_maintainance_id
      )
    )
    
    
    testing_input_dataframe <- transpose(df) %>% row_to_names(row_number = 1)
    
    testing_input_dataframe$credit_risk <- 0
    
    testing_input_dataframe[, numerical_columns] <-
      lapply(testing_input_dataframe[, numerical_columns], as.numeric)
    
    
    model_predictions <- predict(sample_model,testing_input_dataframe)
    
    model_probabilities_df <- suppressMessages(melt(predict(sample_model,testing_input_dataframe,type = "prob")))
    
    colnames(model_probabilities_df) <- c("label","value")
    
    model_probabilities_df$approval <- if_else(model_probabilities_df$label == "Good","Yes","No")
    
    model_probabilities_df$value    <- round(model_probabilities_df$value * 100,2)
    
    print(model_probabilities_df)
  })
  
  
  output$pie_plot <- renderHighchart({
    if (input$submit_button_id  > 1) {
      data_set_input() %>%
        hchart("pie",
               hcaes(x = approval, y = value,opacity =.6),
               name = "Credit Approval",
               innerSize = "80%") %>%
        hc_plotOptions(
          startAngle = 90,
          endAngle = 90,
          center = list('50%', '75%'),
          size = '100%'
        ) %>%
        hc_title(
          text = "Credit Approval Status",
          margin = 20,
          align = "center",
          style = list(color = "black")
        ) %>% hc_colors(c("green","red"))
    }
  })
  
  observeEvent(input$submit_button_id,
               if(!isTruthy(input$account_balance_id) | !isTruthy(input$credit_history_id) | 
                  !isTruthy(input$credit_purpose_id) | !isTruthy(input$savings_account_bonds_id) | 
                  !isTruthy(input$foreign_worker_id) | !isTruthy(input$job_status_id) |
                  !isTruthy(input$employment_history_id) | !isTruthy(input$other_installment_plans_id) | 
                  !isTruthy(input$marital_status_sex_id) | !isTruthy(input$property_type_id) | 
                  !isTruthy(input$housing_type_id) | !isTruthy(input$telephone_status_id) |
                  !isTruthy(input$debtors_guarantor_status_id)
               ) {
                 sendSweetAlert(
                   session = session,
                   title = "Warning...!!!",
                   text = "Fill in all the sections",
                   type = "warning",
                 )
               })
}


# Header
header <- dashboardHeader(
  title = shinyDashboardLogoDIY(
    boldText = "Dashboard",
    mainText = "credit risk",
    textSize = 16,
    badgeText = "V1",
    badgeTextColor = "white",
    badgeTextSize = 3,
    badgeBackColor = "orange",
    badgeBorderRadius = 3
  )
)


tag_widget_height <- tags$style(".choosechannel .btn   {height: 15.5px;padding: 0px;
                                font-size : 10px; font-weight: bold;}")
# Side Bar
sidebar <- dashboardSidebar(width = 430,
                            collapsed = FALSE,
                            fluidRow(column(width = 6,align = "center",tag_widget_height,picker_widget_list$account_balance),
                                     column(width = 6,align = "center",picker_widget_list$credit_history)),
                            fluidRow(column(width = 6,align = "center",tag_widget_height,slider_widget_list$credit_amount),
                                     column(width = 6,align = "center",radio_widget_list$existing_credits_current_bank)),
                            fluidRow(column(width = 6,align = "center",picker_widget_list$credit_purpose),
                                     column(width = 6,align = "center",picker_widget_list$savings_account_bonds)),
                            fluidRow(column(width = 6,align = "center",slider_widget_list$month_duration),
                                     column(width = 6,align = "center",picker_widget_list$foreign_worker)),
                            fluidRow(column(width = 6,align = "center",picker_widget_list$job_status),
                                     column(width = 6,align = "center",picker_widget_list$employment_history)),
                            fluidRow(column(width = 6,align = "center",picker_widget_list$other_installment_plans),
                                     column(width = 6,align = "center",radio_widget_list$installment_percent_disposable_income)),
                            fluidRow(column(width = 6,align = "center",picker_widget_list$marital_status_sex),
                                     column(width = 6,align = "center",slider_widget_list$age_years)),
                            fluidRow(column(width = 6,align = "center",picker_widget_list$property_type),
                                     column(width = 6,align = "center",picker_widget_list$housing_type)),
                            fluidRow(column(width = 6,align = "center",radio_widget_list$residence_history_years),
                                     column(width = 6,align = "center",picker_widget_list$telephone_status)),
                            fluidRow(column(width = 6,align = "center",picker_widget_list$debtors_guarantor_status),
                                     column(width = 6,align = "center",radio_widget_list$people_liable_maintainance)),
                            fluidRow(column(width = 12,actionBttn(inputId = "submit_button_id",
                                                                  label = "Submit",
                                                                  style = "pill",
                                                                  color = "primary",
                                                                  icon = icon("check-circle")
                            ), align = "center"))
)


# Body
body <-
  dashboardBody(#tags$style(HTML('body {font-family:"Georgia",Times,"Times New Roman",serif;font-size : 14px;background-color:lightblue}')),
    tabsetPanel(
      tabPanel(
        highchartOutput("pie_plot"),
        # tableOutput('tabledata'),
        title = "Credit Risk",
        icon = icon("credit-card", lib = "glyphicon")
      ),
      tabPanel(
        title = "Chatbot",
        icon = icon("envelope", lib = "glyphicon")
      ),
      tabPanel(
        title = " Documentaion",
        icon = icon("folder-open", lib = "glyphicon")
      )
    )
  )

# tags$head(tags$style(HTML(".skin-blue .main-sidebar {background-color:  azure;}"))))

# User Interface
ui <- dashboardPage(
  skin = "green",
  header = header,
  sidebar = sidebar,
  body = body
)


# Shiny application
shinyApp(ui = ui, server = server)


#######
# conver rad to num

