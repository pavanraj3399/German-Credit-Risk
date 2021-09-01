######## Setting working directory
setwd("~/Documents/GitHub/German-Credit-Risk")

#####################################
######## Importing Libraries ########
######################################

library(dplyr)           # Data Manipulation
library(shiny)           # Shiny application
library(shinydashboard)  # Shiny dashboard
library(shinyWidgets)    # Shiny Widgets
library(dashboardthemes) # Shiny dashboard themes
library(stringr)         # String Manipulation
library(tools)           # String Manipulation
library(data.table)      # Data Manipulation
library(janitor)         # Data Manipulation
library(reshape2)        # Data Manipulation
library(highcharter)     # Data Visualization
library(lime)            # Explaining Machine learning
library(caret)           # Machine learning
library(stats)           # Machine learning
library(shinycssloaders) # Loading widget
library(spsComps)        # Tooltip elements
library(cicerone)        # Guided tour of Shiny application


###########################################
############ Importing Dataset ############
###########################################

# German credit data
german_credit_data <- readRDS("credit_data.rds")

# Training credit data
training_data <- readRDS("training_data.rds")

# Testing data
testing_data <- readRDS("testing_data.rds")

# Cart Model
cart_model <- readRDS("cart_model.rds")

# Logistic regression baseleine
sample_model <- readRDS("sample_model.rds") 

# Logistic regression
logit_model  <- readRDS("logistic_regression_model.rds") 

# Cart Model with selected features 
shiny_cart_model <- readRDS("cart_shiny_model.rds")


#########################################
############ Data Manipulation  #########
#########################################

# Create a Features Vector
feature_names <-
  c(unique(shiny_cart_model$finalModel$frame$var)[!unique(shiny_cart_model$finalModel$frame$var) %in% "<leaf>"],
    "credit_risk"
  )

# Subset Numerical Features
numerical_columns   <-
  c(
    "month_duration",
    "credit_amount"
  )

# Subset Categorical Features
categorical_columns <-
  feature_names[!feature_names %in% numerical_columns]


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
  slider_features <- features[!features %in% "credit_risk"]
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


# Cicerone Guide
guide <- Cicerone$
  new()$ 
  step(
    el = "account_balance_id",
    title = "Feature Selection",
    description = "This is where you select relevent option for the features."
  )$ 
  step(
    el = "submit_button_id",
    title = "Submit",
    description = "This is where you submit your information for processing."
  )



# Server
server <- function(input, output, session) {
  
  guide$init()$start()
  session$onSessionEnded(stopApp)
  
  data_set_input <- reactive({
    
    df <- data.frame(
      Name = c("account_balance",
               "other_installment_plans",
               "credit_purpose",
               "employment_history",
               "month_duration",
               "credit_history",
               "debtors_guarantor_status",
               "credit_amount",
               "property_type",
               "savings_account_bonds"
      ),
      Value =  c(input$account_balance_id,
                 input$other_installment_plans_id,
                 input$credit_purpose_id,
                 input$employment_history_id,
                 input$month_duration_id,
                 input$credit_history_id,
                 input$debtors_guarantor_status_id,
                 input$credit_amount_id,
                 input$property_type_id,
                 input$savings_account_bonds_id
      )
    )
    
    testing_input_dataframe <- transpose(df) %>% row_to_names(row_number = 1)
    
    testing_input_dataframe$credit_risk <- 0
    
    testing_input_dataframe[, numerical_columns] <-
      lapply(testing_input_dataframe[, numerical_columns], as.numeric)
    
    return(testing_input_dataframe)
  })
  
  
  data_set_predictions <- reactive({
    
    model_predictions <- stats::predict(shiny_cart_model,data_set_input())
    
    return(model_predictions)
  })
  
  
  data_set_probabilities <- reactive({
    model_probabilities_df <- suppressMessages(reshape2::melt(stats::predict(shiny_cart_model,
                                                                             data_set_input(),type = "prob")))
    colnames(model_probabilities_df) <- c("label","value")
    model_probabilities_df$approval <- if_else(model_probabilities_df$label == "Good","Yes","No")
    model_probabilities_df$value    <- round(model_probabilities_df$value * 100,2)
    return(model_probabilities_df)
  })
  
  
  lime_explainer <- reactive({
    explainer <- lime(training_data %>% dplyr::select(all_of(feature_names)), shiny_cart_model)
    explanation <- lime::explain(data_set_input()[,names(training_data %>% dplyr::select(all_of(feature_names)))[!names(training_data %>% dplyr::select(all_of(feature_names))) %in% "credit_risk"]],
                                 explainer = explainer,
                                 n_labels = 2,
                                 n_features = 10)
    explanation <- explanation %>% as.data.frame()
    return(explanation)
  })
  
  explanation_plot_df <- reactive({
    
    explain_df <- lime_explainer()  %>% dplyr::select( feature_desc,feature_weight,
                                                       feature,feature_value,label) %>% 
      filter(label == "Good")
    
    # Features one 
    features_one <- c("account_balance",
                      "savings_account_bonds",
                      "property_type")
    
    features_one_df <- explain_df %>% filter(feature %in% features_one)
    
    features_one_df$statement <-
      paste(str_replace_all(
        features_one_df$feature_desc,
        c(" = " = " belongs to ",
          "_" = " ", "/" = "or")
      ),
      "Category")
    
    # Features two
    features_two <- c("credit_history")
    
    features_two_df <- explain_df %>% filter(feature %in% features_two)
    
    features_two_df$statement <- str_replace_all(features_two_df$feature_desc,
                                                 c(" = " = " suggests that you have ",
                                                   "_" = " ", "/" = "or"))
    # Features three
    features_three <- c("employment_history",
                        "housing_type","credit_purpose",
                        "other_installment_plans",
                        "debtors_guarantor_status")
    
    features_three_df <- explain_df %>% filter(feature %in% features_three)
    
    features_three_df$statement <- str_replace_all(
      features_three_df$feature_desc,
      c(
        " = " = " is ",
        "marital_status_sex" =  "Sex",
        ":" = " and marital status is",
        "_" = " ",
        "/" = "or"
      )
    )
    
    
    numerical_df <-  explain_df %>% filter(feature %in% c(numerical_columns))
    
    features_six <- numerical_df$feature_desc[numerical_df$feature_desc %like% " < " & 
                                                numerical_df$feature_desc %like% " <= "] 
    
    if (length(features_six) > 0 ) {
      
      features_six_df <- numerical_df %>% filter(feature_desc %in% features_six)
      
      features_six_df$statement <- str_replace_all(
        paste(
          features_six_df$feature,
          "is in between",
          str_split_fixed(features_six_df$feature_desc, " ", 5)[1:nrow(features_six_df), 1] ,
          "and",
          str_split_fixed(features_six_df$feature_desc, " ", 5)[1:nrow(features_six_df), 5]
        ),
        "_",
        " "
      )
      
    } else {
      features_six_df <- numerical_df %>% filter(feature == "not found")
    }
    
    
    ### less than or equal to df
    
    rows_seven <- numerical_df %>% filter(!feature %in% features_six_df$feature & feature_desc %like% " <= " )
    
    if (nrow(rows_seven) > 0) {
      features_seven_df <- numerical_df %>% filter(!feature %in% features_six_df$feature & feature_desc %like% " <= " )
      
      features_seven_df$statement <- str_replace_all(paste(
        features_seven_df$feature,
        "is below",
        str_split_fixed(features_seven_df$feature_desc, " ", 3)[1:nrow(features_seven_df), 3]
      ),
      "_",
      " ")
    } else {
      features_seven_df <- numerical_df %>% filter(feature == "not found")
    }
    
    ### more than df
    rows_eight <- numerical_df %>% filter(!feature %in% features_six_df$feature & feature_desc %like% " < " )
    
    if (nrow(rows_eight) > 0 ) {
      
      features_eight_df <- numerical_df %>% filter(!feature %in% features_six_df$feature & feature_desc %like% " < " )
      
      features_eight_df$statement <- str_replace_all(paste(
        features_eight_df$feature,
        "is more than",
        str_split_fixed(features_eight_df$feature_desc, " ", 3)[1:nrow(features_eight_df), 1]
      ),
      "_",
      " ")
    } else {
      features_eight_df <- numerical_df %>% filter(feature == "not found")
    }
    
    # Stacked dataframe
    stacked_data <- rbind(features_eight_df,
                          features_seven_df,
                          features_six_df,
                          features_three_df,
                          features_two_df,
                          features_one_df)
    
    # white spaces and tot title case
    stacked_data$statement <- toTitleCase(str_squish(stacked_data$statement))
    
    stacked_data$statement_type <- if_else(stacked_data$feature_weight > 0 , 
                                           "1 Positive","2 Negative")
    
    stacked_data <- stacked_data %>% arrange(desc(feature_weight))
    
    return(stacked_data)
  })
  
  
  # output$table <- renderDataTable({
  #   if (input$submit_button_id  > 1) {
  #   print(explanation_plot_df())
  #   }
  # })
  
  output$bar_plot <- renderHighchart({
    if (input$submit_button_id  > 1) {
      input$submit_button_id
      Sys.sleep(1)
      explanation_plot_df() %>%
        hchart(
          'bar', hcaes(x = toTitleCase(str_replace_all(feature,"_"," ")), y = feature_weight,
                       group = statement_type,
                       opacity =.6),
          borderColor = "black",
          tooltip = list(pointFormat = "{point.statement}",
                         headerFormat = "<br> <b> Reason : <b> <br> ")
        ) %>% 
        hc_title(
          text = "Factors influencing application",
          margin = 20,
          align = "center",
          style = list(color = "black")
        ) %>% 
        hc_colors(c("green","red")) %>% 
        hc_yAxis(title = list(text = "Estimated weight from the predictive model")) %>% 
        hc_xAxis(title = list(text = "Feature")) %>% hc_legend(
          layout =  'vertical',
          align =  'right',
          verticalAlign =  'middle')
    }
  })
  
  output$pie_plot <- renderHighchart({
    if (input$submit_button_id  > 1) {
      input$submit_button_id
      Sys.sleep(1)
      data_set_probabilities() %>%
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
        ) %>% 
        hc_colors(c("green","red")) %>% 
        hc_subtitle(
          text = paste("You have ",round(as.vector(data_set_probabilities()[1,] %>% dplyr::select(value))),
                       " % approval rate for your application."),
          align = "center",
          verticalAlign = 'bottom',
          style = list(color = "#2b908f", fontWeight = "bold",fontSize = "18px")
        )
    }
  })
  
  observeEvent(input$submit_button_id,
               if(!isTruthy(input$account_balance_id) | !isTruthy(input$credit_history_id) | 
                  !isTruthy(input$credit_purpose_id) | !isTruthy(input$savings_account_bonds_id) |
                  !isTruthy(input$employment_history_id) | !isTruthy(input$other_installment_plans_id) | 
                  !isTruthy(input$property_type_id) | !isTruthy(input$debtors_guarantor_status_id)
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
    boldText = "Application",
    mainText = "credit risk",
    textSize = 16,
    badgeText = "V1",
    badgeTextColor = "white",
    badgeTextSize = 3,
    badgeBackColor = "orange",
    badgeBorderRadius = 3
  )
)


# Widgets with Tool tips 

account_balance_tool <- picker_widget_list$account_balance %>%
  bsTooltip("Status of existing checking account in Euros(€)", "right")

credit_history_tool  <- picker_widget_list$credit_history %>%
  bsTooltip("Credit history", "right")

credit_amount_tool   <- slider_widget_list$credit_amount %>%
  bsTooltip("Amount required for Credit in Euros(€)", "right")

credit_purpose_tool  <- picker_widget_list$credit_purpose %>%
  bsTooltip("Purpose for the credit", "right")

savings_account_bonds_tool <- picker_widget_list$savings_account_bonds %>%
  bsTooltip("Savings / Bonds in your account in Euros(€)", "right")

month_duration_tool <- slider_widget_list$month_duration %>%
  bsTooltip("Duration for the credit in month", "right")

employment_history_tool <- picker_widget_list$employment_history %>%
  bsTooltip("Present employment since", "right")

other_installment_plans_tool <- picker_widget_list$other_installment_plans %>%
  bsTooltip("Other installment plans", "right")

property_type_tool <- picker_widget_list$property_type %>%
  bsTooltip("Category of the property you own", "right")

debtors_guarantor_status_tool <- picker_widget_list$debtors_guarantor_status %>%
  bsTooltip("Other debtors/guarantors for the credit", "right")

# UI customisation
tag_widget_height <- tags$style(".choosechannel .btn   {height: 15.5px; padding: 0px;
                                font-size : 10px;}")

tag_margin_outter <-  tags$style(".choosechannel {margin-bottom: -.35em;margin-top: .7em;
                                  font: normal normal 0.8em 'Arial'; 
                                 font-weight: normal; }")



# Side Bar
sidebar <- dashboardSidebar(width = 250,
                            collapsed = FALSE,
                            tag_margin_outter,tag_widget_height,account_balance_tool,
                            credit_history_tool,
                            credit_amount_tool,
                            credit_purpose_tool,
                            month_duration_tool,
                            employment_history_tool,
                            debtors_guarantor_status_tool,
                            property_type_tool,
                            savings_account_bonds_tool,
                            other_installment_plans_tool,
                            fluidRow(column(width = 12,actionBttn(inputId = "submit_button_id",
                                                                  label = "Submit",
                                                                  style = "pill",
                                                                  color = "primary",
                                                                  icon = icon("check-circle")
                            ), align = "center"))
)


# Body
body <-
  dashboardBody(
    tabsetPanel(
      tabPanel(
        fluidRow(width = 12,
                 align = "center", shinycssloaders::withSpinner(highchartOutput("pie_plot"))),
        fluidRow(width = 12,align = "center", shinycssloaders::withSpinner(highchartOutput("bar_plot"))),
        tags$div(
          HTML(
            "<script>
  window.watsonAssistantChatOptions = {
      integrationID: '1e678750-2f0c-4d20-b144-6c93d88447ed', // The ID of this integration.
      region: 'eu-gb', // The region your integration is hosted in.
      serviceInstanceID: '426256b5-a80f-4fbc-bbf6-fccc0f2ee3c3', // The ID of your service instance.
      onLoad: function(instance) { instance.render(); }
    };
  setTimeout(function(){
    const t=document.createElement('script');
    t.src='https://web-chat.global.assistant.watson.appdomain.cloud/loadWatsonAssistantChat.js';
    document.head.appendChild(t);
  });
</script>"
          )
        ),
        title = "Credit Risk",
        icon = icon("credit-card", lib = "glyphicon")
      ),
      tabPanel(
        tags$h3("Participant Information Sheet",style="color:black"),
        HTML("<p> For the Participant Information Sheet <a href='https://drive.google.com/file/d/1w6T6iVdJz7JEQSoh-IMzgaHVWa82GvOh/view?usp=sharing'> Click here </a>!</p>"),
        tags$h3("Online Survey",style="color:black"),
        HTML("<p> To take an online survey <a href='https://docs.google.com/forms/d/e/1FAIpQLSf5zfutZOkCqp4IHCmOtyXpfllMiuoh9_abdvQ8zysUXWFowA/viewform?usp=sf_link'> Click here </a>!</p>"),
        title = "Documentation",
        icon = icon("folder-open", lib = "glyphicon")
      )
    )
  )

# tags$head(tags$style(HTML(".skin-blue .main-sidebar {background-color:  azure;}"))))

# User Interface
ui <- dashboardPage(
  use_cicerone(),
  #skin = "green",
  header = header,
  sidebar = sidebar,
  body = body
)

ui


# Shiny application
shinyApp(ui = ui, server = server)




