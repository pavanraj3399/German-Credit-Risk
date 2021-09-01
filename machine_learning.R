######## Setting working directory
setwd("~/Documents/GitHub/German-Credit-Risk")

#####################################
######## Importing Libraries ########
######################################


library(ggplot2)
library(dplyr)        # Data Manipulation
library(plotly)       # Data Visualization
library(RColorBrewer) # Data Visualization
library(stringr)      # String Manipulation
library(tools)        # String Manipulation
library(caTools)      # Sampling
library(caret)        # Machine learning
library(pROC)         # ROC curves.
library(gdata)        #
library(rattle)       # Plot CART
library(e1071)
library(party)
library(RWeka)
library(lime)
library(arm)
library(C50)
library(randomForest)
library(CHAID)

###########################################
############ Importing Dataset ############
###########################################

credit_data <- read.table("german.data")

credit_data

################################################
############  Descriptive Statistics ############
################################################

#### First few observations
head(credit_data)

#### Structure of the dataset
str(credit_data)

#### Summary of the dataset
summary(credit_data)

#### Missing Values
colSums(is.na(credit_data))



#########################################
############ Data Manipulation  #########
#########################################

#########################################
############ Rename Feature names #######
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

# Rename Features with "feature_names" vector
colnames(credit_data) <- feature_names

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

###########################################
############ Mapping Observations #########
###########################################




# account_balance - Status of existing checking account (qualitative)

credit_data$account_balance <-
  ifelse(
    credit_data$account_balance == "A11",
    "Less than 0 Euros (€)",
    if_else(
      credit_data$account_balance == "A12",
      "Between 0 and  200 Euros (€)",
      if_else(
        credit_data$account_balance == "A13",
        "Greater than or equal to  200 Euros (€)",
        "No checking account"
      )
    )
  )


# credit_history - Credit history (qualitative)

credit_data$credit_history <-
  if_else(
    credit_data$credit_history == "A30",
    "No credits taken",
    if_else(
      credit_data$credit_history == "A31",
      "All credits paid back duly",
      if_else(
        credit_data$credit_history == "A32",
        "Existing credits paid back duly till now",
        if_else(
          credit_data$credit_history == "A33",
          "Delay paying off in the past",
          "Critical account / other credits existing (not at this bank)"
        )
      )
    )
  )

credit_data$credit_purpose <-
  if_else(
    credit_data$credit_purpose == "A40",
    "New car",
    if_else(
      credit_data$credit_purpose == "A41",
      "Used car",
      if_else(
        credit_data$credit_purpose == "A42",
        "Furniture / Equipment",
        if_else(
          credit_data$credit_purpose == "A43",
          "Radio / Telivision",
          if_else(
            credit_data$credit_purpose == "A44",
            "Domestic appliance",
            if_else(
              credit_data$credit_purpose == "A45",
              "Repairs",
              if_else(
                credit_data$credit_purpose == "A46",
                "Education",
                if_else(
                  credit_data$credit_purpose == "A47",
                  "Vacation / does not exist?",
                  if_else(
                    credit_data$credit_purpose == "A48",
                    "Retraining",
                    if_else(credit_data$credit_purpose == "A49", "Business", "Others")
                  )
                )
              )
            )
          )
        )
      )
    )
  )



# savings_account_bonds - Savings account/bonds (qualitative)

credit_data$savings_account_bonds <-
  if_else(
    credit_data$savings_account_bonds == "A61",
    "Less than 100 Euros (€)",
    if_else(
      credit_data$savings_account_bonds == "A62",
      "Between 100 and 500 Euros (€)",
      if_else(
        credit_data$savings_account_bonds == "A63",
        "Between 500 and 1000 Euros (€)",
        if_else(
          credit_data$savings_account_bonds == "A64",
          "Greater than or equal to 1000 Euros (€)",
          "Unknown / no known savings"
        )
      )
    )
  )


# employment_history - Present employment since (qualitative)

credit_data$employment_history <-
  if_else(
    credit_data$employment_history == "A71",
    "Unemployed",
    if_else(
      credit_data$employment_history == "A72",
      "Less than 1 year",
      if_else(
        credit_data$employment_history == "A73",
        "Between 1 and 4 years",
        if_else(
          credit_data$employment_history == "A74",
          "Between 4 and 7 years",
          "Greater than or equal to 7 years"
        )
      )
    )
  )


# marital_status_sex - Personal status and sex (qualitative)
credit_data$marital_status_sex <-
  ifelse(
    credit_data$marital_status_sex == "A91",
    "Male : divorced / seperated",
    if_else(
      credit_data$marital_status_sex == "A92",
      "Female : divorced /  seperated / married",
      if_else(
        credit_data$marital_status_sex == "A93",
        "Male : single",
        if_else(
          credit_data$marital_status_sex == "A94",
          "Male : married / widowed",
          "Female : single"
        )
      )
    )
  )



# debtors_guarantor_status -   Other debtors / guarantors (qualitative)

credit_data$debtors_guarantor_status <-
  if_else(
    credit_data$debtors_guarantor_status == "A101",
    "None",
    if_else(
      credit_data$debtors_guarantor_status == "A102",
      "Co-applicant",
      "Guarantor"
    )
  )


# property_type -   Property (qualitative)

credit_data$property_type <-
  if_else(
    credit_data$property_type == "A121",
    "Real estate",
    if_else(
      credit_data$property_type == "A122",
      "Building society savings agreement / Life insurance",
      if_else(
        credit_data$property_type == "A123",
        "Car or other",
        "Unknown / no property"
      )
    )
  )


# other_installment_plans -   Other installment plans  (qualitative)
credit_data$other_installment_plans <-
  if_else(
    credit_data$other_installment_plans == "A141",
    "Bank",
    if_else(
      credit_data$other_installment_plans == "A142",
      "Stores",
      "None"
    )
  )


#  housing_type -    Housing  (qualitative)
credit_data$housing_type <-
  if_else(
    credit_data$housing_type == "A151",
    "Rent",
    if_else(credit_data$housing_type == "A152",
            "Own", "For free")
  )



#  job_status -    Job  (qualitative)
credit_data$job_status <-
  if_else(
    credit_data$job_status == "A171",
    "Unemployed / unskilled - non resident",
    if_else(
      credit_data$job_status == "A172",
      "Unskilled - resident",
      if_else(
        credit_data$job_status == "A173",
        "Skilled employee / official",
        "Management / self employed / highly qualified employee / officer"
      )
    )
  )


#  telephone_status -    Telephone  (qualitative)
credit_data$telephone_status <-
  if_else(
    credit_data$telephone_status == "A191",
    "None",
    "Yes / registered under the customers name"
  )


#  foreign_worker -    foreign worker (qualitative)
credit_data$foreign_worker <-
  if_else(credit_data$foreign_worker == "A201", "Yes", "No")


#  credit_risk -    Credit risk (qualitative)
credit_data$credit_risk <-
  ifelse(credit_data$credit_risk == "1", "Good", "Bad")




################################################
############ Data type conversion ##############
################################################

# Covert to numeric data type
credit_data[, numerical_columns] <-
  lapply(credit_data[, numerical_columns], as.numeric)

# Covert to factor data type
credit_data[, categorical_columns] <-
  lapply(credit_data[, categorical_columns], as.factor)



################################################
############ Order Factor Levels ##############
################################################


# Set target factor reference to good to predict bad applications
credit_data$credit_risk <-
  relevel(credit_data$credit_risk, ref = "Good")


# Order levels in account balance
credit_data$account_balance <-
  factor(
    credit_data$account_balance,
    labels = c(
      "Less than 0 Euros (€)",
      "Between 0 and  200 Euros (€)",
      "Greater than or equal to  200 Euros (€)",
      "No checking account"
    )
  )


# Order levels in savings_account_bonds
credit_data$savings_account_bonds <-
  factor(
    credit_data$savings_account_bonds,
    labels = c(
      "Less than 100 Euros (€)",
      "Between 100 and 500 Euros (€)",
      "Between 500 and 1000 Euros (€)",
      "Greater than or equal to 1000 Euros (€)",
      "Unknown / no known savings"
    )
  )

# Order levels in employment_history
credit_data$employment_history <-
  factor(
    credit_data$employment_history,
    labels = c(
      "Less than 1 year",
      "Between 1 and 4 years",
      "Between 4 and 7 years",
      "Greater than or equal to 7 years",
      "Unemployed"
    )
  )

summary(credit_data)


################################################
############ Exploratory Data Analysis ##########
################################################


#' Title - Function to extract labels and counts for a qualitative feature
#'
#' @param feature_id  - Feature name in the dataset
#' @param label_or_value - string value ["values" or "labels"]
#'
#' @return if labels then return label names in the feature
#' if values then return value count of the labels in the feature


value_counts  <- function(feature_id, label_or_value) {
  select_feature  <- credit_data[[feature_id]]
  frequency_table <-
    table(select_feature) %>% as.data.frame() %>% arrange(Freq)
  feature_labels  <- as.vector(frequency_table$select_feature)
  values <- as.vector(frequency_table$Freq)
  if (label_or_value == "labels") {
    return(feature_labels)
  }
  else if (label_or_value == "values") {
    return(values)
  }
}



#' Title - Function to plot pie chart for a qualitative feature
#'
#' @param feature_id - Feature name in the dataset
#' @param title_name - Title for plot
#' @param hover_text - Hover text for plot
#'
#' @return - Pie chart for qualitative feature


plot_pie_plotly <- function(feature_id, title_name, hover_text) {
  cols <-
    colorRampPalette(brewer.pal(8, "Set2"))(length(unique(credit_data[[feature_id]])))
  
  figure_pie <- plot_ly(
    type = "pie",
    textposition = 'inside',
    labels = value_counts(feature_id, "labels"),
    values = value_counts(feature_id, "values"),
    textinfo = 'label+percent',
    opacity = .8,
    hoverinfo = "text",
    text = ~ paste(value_counts(feature_id, "values") , hover_text),
    marker = list(
      colors = cols,
      line = list(color = '#FFFFFF', width = 1)
    )
  )
  
  figure_pie <- figure_pie %>% layout(
    legend = list(
      title = list(text = toTitleCase(str_replace_all(
        feature_id, "_", " "
      ))),
      orientation = 'v'
    ),
    title = title_name,
    xaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE
    ),
    yaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE
    ),
    plot_bgcolor = 'rgb(243, 243, 243)'
  )
  figure_pie
}



#' Title - Function to plot bar chart for a qualitative feature
#'
#' @param feature_id - Feature name in the dataset
#' @param title_name - Title for plot
#' @param hover_text - Hover text for plot
#'
#' @return - Bar chart for qualitative feature


plot_bar_plotly <- function(feature_id, title_name, hover_text) {
  figure_bar <- plot_ly(
    type = "bar",
    y = value_counts(feature_id, "labels"),
    x = value_counts(feature_id, "values"),
    marker = list(
      color = 'rgb(49,130,189)',
      line = list(color = 'rgb(8,48,107)',
                  width = 1.5)
    ),
    text = ~ paste(value_counts(feature_id, "values") ,
                   hover_text),
    opacity = 0.7,
    hoverinfo = "text"
  )
  
  figure_bar <- figure_bar %>% layout(
    bargap = .6,
    title = title_name,
    xaxis = list(
      title = "Count",
      tickfont = list(size = 14,
                      color = 'rgb(107, 107, 107)'),
      showgrid = TRUE
    ),
    yaxis = list(
      title = toTitleCase(str_replace_all(feature_id, "_", " ")),
      categoryorder = "array",
      categoryarray =  value_counts(feature_id
                                    , "labels"),
      tickfont = list(size = 12,
                      color = 'rgb(107, 107, 107)')
    )
  )
  figure_bar
}



#' Title - Function to plot histogram density  chart for a quantitative feature
#'
#' @param feature_id - Feature name in the dataset
#' @param title_name - Title for plot
#'
#' @return - Histogram density  chart for quantitative feature

plot_hist_plotly <- function(feature_id, title_name) {
  density_feature <- density(credit_data[[feature_id]])
  
  
  figure_hist <- plot_ly(
    type = "histogram",
    x = credit_data[[feature_id]],
    opacity = .5,
    name = "Histogram"
  )
  
  figure_hist <- figure_hist %>% add_lines(
    x = density_feature$x,
    y = density_feature$y,
    fill = "tozeroy",
    yaxis = "y2",
    name = "Density",
    opacity = .001
  )
  
  figure_hist <- figure_hist %>% layout(
    bargap = .03,
    yaxis2 = list(
      overlaying = "y",
      side = "right",
      title = "Density",
      showgrid = FALSE
    ),
    title = title_name,
    xaxis = list(
      title = toTitleCase(str_replace_all(feature_id, "_", " ")),
      tickfont = list(size = 14,
                      color = 'rgb(107, 107, 107)')
    ),
    yaxis = list(
      showgrid = FALSE,
      title = "Count",
      tickfont = list(size = 12,
                      color = 'rgb(107, 107, 107)')
    ),
    paper_bgcolor = 'rgb(243, 243, 243)',
    plot_bgcolor = 'rgb(243, 243, 243)'
  )
  figure_hist
}



###############################################
############ Univariate Data Analysis #########
###############################################


plot_bar_plotly("account_balance", "title", "hover")
plot_bar_plotly("credit_purpose", "title", "hover")
plot_bar_plotly("employment_history", "title", "hover")
plot_bar_plotly("marital_status_sex", "title", "hover")
plot_bar_plotly("savings_account_bonds", "title", "hover")
plot_bar_plotly("residence_history_years", "title", "hover")
plot_bar_plotly("property_type", "title", "hover")
plot_bar_plotly("job_status", "title", "hover")
plot_bar_plotly("installment_percent_disposable_income", "title", "hover")
plot_bar_plotly("residence_history_years", "title", "hover")
plot_bar_plotly("existing_credits_current_bank", "title", "hover")
plot_bar_plotly("people_liable_maintainance", "title", "hover")



plot_pie_plotly("credit_history", "title", "hover")
plot_pie_plotly("debtors_guarantor_status", "title", "hover")
plot_pie_plotly("other_installment_plans", "title", "hover")
plot_pie_plotly("housing_type", "title", "hover")
plot_pie_plotly("job_status", "title", "hover")
plot_pie_plotly("telephone_status", "title", "hover")
plot_pie_plotly("foreign_worker", "title", "hover")
plot_pie_plotly("credit_risk", "title", "hover")


plot_hist_plotly("month_duration", "title")
plot_hist_plotly("credit_amount", "title")
plot_hist_plotly("age_years", "title")


###################################################
############  Bi Variate Data Analysis #########
###################################################


#' Title - Stacked bar chart by credit risk
#'
#' @param feature_id- Feature name in the dataset
#' @param title_name- Title for plot
#'
#' @return - Stacked bar chart with percentages as a whole for qualitative feature by credit risk

plot_bar_stacked_plotly <- function(feature_id, title_name) {
  data_frame_credit <-
    round(prop.table(table(credit_data[[feature_id]],
                           credit_data[["credit_risk"]]),
                     margin = 1) * 100, 2) %>% as.data.frame()
  
  colnames(data_frame_credit) <-
    c(feature_id, "credit_risk", "percentage")
  
  gd_val <-
    data_frame_credit[data_frame_credit["credit_risk"] == "Good", "percentage"]
  bd_val <-
    data_frame_credit[data_frame_credit["credit_risk"] == "Bad", "percentage"]
  labs   <- unique(data_frame_credit[[feature_id]])
  
  figure_bar_stacked <- plot_ly(
    type = "bar",
    x = gd_val,
    y = labs,
    marker = list(
      line = list(color = 'white',
                  width = 1.5),
      color = "green"
    ),
    name = "Good Applications",
    opacity = 0.5
  )
  
  figure_bar_stacked <-
    figure_bar_stacked %>% add_trace(
      type = "bar",
      x = bd_val,
      y = labs,
      marker = list(color = "red"),
      name = "Bad Applications",
      opacity = 0.5
    )
  
  figure_bar_stacked <-
    figure_bar_stacked %>% layout(
      bargap = .6,
      barmode = "stack",
      title = title_name,
      xaxis = list(
        title = "Percentage",
        automargin = T,
        ticksuffix = "%"
      ),
      yaxis = list(title = toTitleCase(str_replace_all(
        feature_id, "_", " "
      ))),
      legend = list(
        title = list(text = '<b> Credit Risk </b>'),
        orientation = "v"
      ),
      paper_bgcolor = 'rgb(243, 243, 243)',
      plot_bgcolor = 'rgb(243, 243, 243)'
    )
  figure_bar_stacked
}




#' Title - Histogram chart by credit risk
#'
#' @param feature_id - Feature name in the dataset
#' @param title_name - Title for plot
#'
#' @return - Histogram chart for quantitative feature by credit risk


plot_hist_risk_plotly <- function(feature_id, title_name) {
  gd_df <-
    credit_data %>% filter(credit_risk == "Good") %>% dplyr::select(all_of(feature_id))
  bd_df <-
    credit_data %>% filter(credit_risk == "Bad") %>% dplyr::select(all_of(feature_id))
  
  figure_hist <- plot_ly(
    type = "histogram",
    x = gd_df[[feature_id]],
    marker = list(color = "green"),
    name = "Good Applications",
    opacity = 0.4
  )
  
  figure_hist <- figure_hist %>% add_trace(
    type = "histogram",
    x = bd_df[[feature_id]],
    marker = list(color = "red"),
    name = "Bad Applications",
    opacity = 0.4
  )
  
  figure_hist <- figure_hist %>% layout(
    barmode = "overlay",
    yaxis2 = list(
      overlaying = "y",
      side = "right",
      title = "Density"
    ),
    title = title_name,
    xaxis = list(
      title = toTitleCase(str_replace_all(feature_id, "_", " ")),
      tickfont = list(size = 14,
                      color = 'rgb(107, 107, 107)')
    ),
    yaxis = list(
      title = "count",
      tickfont = list(size = 12,
                      color = 'rgb(107, 107, 107)')
    ),
    legend = list(
      title = list(text = '<b> Credit Risk </b>'),
      orientation = "v"
    ),
    paper_bgcolor = 'rgb(243, 243, 243)',
    plot_bgcolor = 'rgb(243, 243, 243)'
  )
  figure_hist
}


###################################################
###################################################
###################################################


plot_bar_stacked_plotly("account_balance", "title")
plot_bar_stacked_plotly("credit_history", "title")
plot_bar_stacked_plotly("credit_purpose", "title")
plot_bar_stacked_plotly("credit_history", "title")
plot_bar_stacked_plotly("savings_account_bonds", "title")
plot_bar_stacked_plotly("employment_history", "title")
plot_bar_stacked_plotly("marital_status_sex", "title")
plot_bar_stacked_plotly("debtors_guarantor_status", "title")
plot_bar_stacked_plotly("property_type", "title")
plot_bar_stacked_plotly("other_installment_plans", "title")
plot_bar_stacked_plotly("housing_type", "title")
plot_bar_stacked_plotly("job_status", "title")
plot_bar_stacked_plotly("telephone_status", "title")
plot_bar_stacked_plotly("foreign_worker", "title")


plot_hist_risk_plotly("credit_amount", "title")
plot_hist_risk_plotly("month_duration", "title")
plot_hist_risk_plotly("age_years", "title")

###################################################
############  Multi Variate Data Analysis #########
###################################################

#' Title - Box plot by credit risk
#'
#' @param categorical_column - Categoical feature for y-axis
#' @param numerical_column - Numericcal feature for x-axis
#' @param title_name - Title for plot
#'
#' @return - Box plot by credit risk

plot_box_plotly <- function(categorical_column,
                            numerical_column,
                            title_name) {
  bad_apps  <- credit_data %>% filter(credit_risk == "Bad")
  good_apps <- credit_data %>% filter(credit_risk == "Good")
  
  figure_box <- plot_ly(type = "box")
  
  figure_box <-
    figure_box %>% add_trace(y = bad_apps[[categorical_column]],
                             x = bad_apps[[numerical_column]],
                             name = "Bad applications")
  
  figure_box <-
    figure_box %>% add_trace(y = good_apps[[categorical_column]],
                             x = good_apps[[numerical_column]],
                             name = "Good applications")
  
  
  
  figure_box <- figure_box %>% layout(
    boxmode = "group",
    title = title_name,
    yaxis = list(
      title = toTitleCase(str_replace_all(categorical_column, "_", " ")),
      tickfont = list(size = 14,
                      color = 'rgb(107, 107, 107)')
    ),
    xaxis = list(
      showgrid = TRUE,
      title = toTitleCase(str_replace_all(numerical_column, "_", " ")),
      tickfont = list(size = 14,
                      color = 'rgb(107, 107, 107)')
    ),
    legend = list(
      title = list(text = '<b> Credit Risk </b>'),
      orientation = 'v'
    ),
    paper_bgcolor = 'rgb(243, 243, 243)',
    plot_bgcolor = 'rgb(243, 243, 243)'
  )
  
  suppressWarnings(print(figure_box))
}

###################################################


#' Title - Scatter plot by categorical feature
#'
#' @param xvar_numeric  - x axis numerical feature
#' @param yvar_numeric  - y axis numerical feature
#' @param size_numeric  - Marker size numerical feature
#' @param color_categorical - color legend - categorical feature
#' @param title_name - Title for plot
#'
#' @return -  Scatter plot by categorical feature

plot_bubble_plotly <- function(xvar_numeric,
                               yvar_numeric,
                               size_numeric,
                               color_categorical,
                               title_name) {
  credit_data_frame <- credit_data
  
  credit_data_frame[[color_categorical]] <-
    factor(credit_data_frame[[color_categorical]],
           levels = rev(value_counts(color_categorical, "labels")))
  
  credit_data_frame[["default"]]  <- 1
  
  cols <-
    colorRampPalette(brewer.pal(8, "Paired"))(length(unique(credit_data_frame[[color_categorical]])))
  
  figure_bubble <- plot_ly(type = "scatter",
                           mode = 'markers',
                           colors = cols)
  
  figure_bubble <-
    figure_bubble %>% add_trace(
      x = credit_data_frame[[xvar_numeric]],
      y = credit_data_frame[[yvar_numeric]],
      size = credit_data_frame[[size_numeric]],
      color = credit_data_frame[[color_categorical]],
      marker = list(
        symbol = 'circle',
        line = list(width = 1, color = '#FFFFFF')
      )
    )
  
  fit <-
    lm(credit_data_frame[[yvar_numeric]] ~ credit_data_frame[[xvar_numeric]])
  
  figure_bubble <-
    figure_bubble %>% add_trace(
      x = credit_data_frame[[xvar_numeric]],
      y = fitted(fit),
      type = 'scatter',
      mode = "lines",
      name = "Trend line",
      line = list(color = "black", width = 2)
    )
  
  figure_bubble <-
    figure_bubble %>% layout(
      legend = list(
        title = list(text = toTitleCase(
          str_replace_all(color_categorical, "_", " ")
        )),
        orientation = 'v'
      ),
      title = title_name,
      xaxis = list(
        title = toTitleCase(str_replace_all(xvar_numeric, "_", " ")),
        gridcolor = 'rgb(255, 255, 255)',
        zerolinewidth = 1,
        ticklen = 5,
        gridwidth = 2
      ),
      yaxis = list(
        title = toTitleCase(str_replace_all(yvar_numeric, "_", " ")),
        gridcolor = 'rgb(255, 255, 255)',
        zerolinewidth = 1,
        ticklen = 5,
        gridwith = 2
      ),
      plot_bgcolor = 'rgb(243, 243, 243)',
      paper_bgcolor = 'rgb(243, 243, 243)'
    )
  suppressWarnings(print(figure_bubble))
}


###################################################

#' Title - Correlation heatmap for Numerical features
#'
#' @param numerical_features - Numerical features
#' @param title_name - Title for plot
#'
#' @return - Correlation heatmap for Numerical features

plot_heatmap_plotly <- function(numerical_features, title_name) {
  correlation_matrix <-
    cor(credit_data %>% dplyr::select(all_of(numerical_features)))
  
  figure_heatmap <-
    plot_ly(
      x = toTitleCase(str_replace_all(numerical_features, "_", " ")),
      y = toTitleCase(str_replace_all(numerical_features, "_", " ")),
      z = correlation_matrix,
      type = "heatmap"
    )
  
  figure_heatmap <- figure_heatmap %>% layout(title = title_name,
                                              paper_bgcolor = 'rgb(243, 243, 243)',
                                              plot_bgcolor = 'rgb(243, 243, 243)')
  
  figure_heatmap
  
}

###################################################


#' Title - Parallel Coordinates Plot for Numerical features
#'
#' @param num_features - Numerical features
#'
#' @return -

plot_parllel_coord_plotly <- function(num_features) {
  credit_df <-
    credit_data %>% dplyr::select(all_of(c(num_features, "credit_risk"))) %>% arrange(desc(credit_risk))
  
  credit_df$credit_risk <-
    ifelse(credit_df$credit_risk == "Good", 1, 2)
  
  figure_parallel_coord <- plot_ly(type =  'parcoords')
  
  dim_list = list()
  
  for (i in num_features) {
    j <-
      list(
        range = c(min(credit_df[[i]]), max(credit_df[[i]])),
        label = toTitleCase(str_replace_all(i, "_", " ")),
        values = credit_df[[i]]
      )
    
    dim_list <- append(dim_list, list(j))
  }
  
  figure_parallel_coord <-
    figure_parallel_coord %>% add_trace(
      line = list(
        color = credit_df[["credit_risk"]],
        colorscale = list(c(0, 'red'), c(1, 'blue')),
        showscale = TRUE
      ),
      dimensions = dim_list
    )
  
  figure_parallel_coord <-
    figure_parallel_coord %>% layout(paper_bgcolor = 'rgb(243, 243, 243)',
                                     plot_bgcolor = 'rgb(243, 243, 243)',
                                     title = "Parallel Coordinates Plot")
  figure_parallel_coord
}

######################################################################################################


plot_box_plotly("credit_purpose", "credit_amount", "title")
plot_box_plotly("credit_purpose", "age_years", "title")
plot_box_plotly("credit_purpose", "month_duration", "title")

plot_box_plotly("employment_history", "credit_amount", "title")
plot_box_plotly("employment_history", "age_years", "title")
plot_box_plotly("employment_history", "month_duration", "title")

plot_box_plotly("job_status", "credit_amount", "title")
plot_box_plotly("job_status", "age_years", "title")
plot_box_plotly("job_status", "month_duration", "title")


plot_bubble_plotly("age_years",
                   "credit_amount",
                   "default",
                   "credit_risk",
                   "title")


plot_bubble_plotly("age_years",
                   "credit_amount",
                   "default",
                   "credit_purpose",
                   "title")

plot_bubble_plotly("age_years",
                   "credit_amount",
                   "default",
                   "property_type",
                   "title")

plot_bubble_plotly("age_years",
                   "credit_amount",
                   "default",
                   "foreign_worker",
                   "title")

plot_bubble_plotly("age_years",
                   "credit_amount",
                   "default",
                   "housing_type",
                   "title")

plot_bubble_plotly("month_duration",
                   "credit_amount",
                   "default",
                   "credit_risk",
                   "title")

plot_bubble_plotly("month_duration",
                   "age_years",
                   "default",
                   "credit_risk",
                   "title")

plot_heatmap_plotly(numerical_columns,
                    "title")


plot_parllel_coord_plotly(numerical_columns)



######################################################
################# Hypothesis Testing #################
######################################################

#################################################
################### T - test ###################
#################################################


#' Title- Test Means for Good and Bad Applications for numerical features
#'
#' @param numerical_features - Numerical features vector in the present in the credit data
#'
#' @return - A dataframe with Welch Two Sample t-test by comparing means between
#' Good and Bad Applications for the numerical features with columns
#' "numerical_features","p_values","test_statistic","critical_value"



ttest_pvalues_result <- function(numerical_features) {
  pvalue_list  <- list()
  test_statistic_list <- list()
  degree_of_freedom_list <- list()
  
  for (i in numerical_features) {
    ttest_result <- t.test(
      credit_data %>%
        filter(credit_risk == "Good") %>% dplyr::select(all_of(i)) ,
      credit_data %>%
        filter(credit_risk == "Bad") %>% dplyr::select(all_of(i))
    )
    
    pvalue_list <- unlist(append(pvalue_list, ttest_result$p.value))
    
    test_statistic_list <-
      abs(unlist(append(
        test_statistic_list, ttest_result$statistic
      )))
    
    degree_of_freedom_list <-
      unlist(append(degree_of_freedom_list, ttest_result$parameter))
    
    critical_value_list  <- qt(0.975, degree_of_freedom_list)
    
  }
  
  ttest_dataframe <-
    data.frame(
      numerical_features,
      pvalue_list,
      degree_of_freedom_list,
      test_statistic_list,
      critical_value_list
    )
  
  colnames(ttest_dataframe) <-
    c(
      "numerical_features",
      "p_values",
      "degree_of_freedom",
      "test_statistic_t-value",
      "critical_value"
    )
  
  return(ttest_dataframe)
}


# Null Hypothesis (H0) : There are no differences in means of between Good and Bad Applications
# Alternative Hypothesis (H1) : There are differences in mean of  between Good and Bad Applications
# Significance level : Test hypothesis at α = 0.05.
#  test statistic is larger than the critical value, we reject the null hypothesis.

ttest_df <- ttest_pvalues_result(numerical_columns)
ttest_df

write.csv(ttest_df,
          "~/Desktop/Shiny_application/files_generated/ttest_df.csv")

####################################################################
################### Anova - Analysis of Variance ###################
####################################################################


#' Title - Perform anova between every combination of categorical and a numerical feature in credit data
#'
#' @param categorical_features -  Numerical features vector in the present in the credit data
#' @param numerical_features - Categorical features vector in the present in the credit data
#'
#' @return -  A dataframe with anova between every combination of
#' categorical and a numerical feature in credit data with columns "numerical_feature",
#' "categorical_feature", "p_value", "test_statistic_f-value", "degree_of_freedom_one",
#' "degree_of_freedom_two","critical_value"



anova_test_results <-
  function(categorical_features,
           numerical_features) {
    num_features <- list()
    cat_features <- list()
    p_value_list <- list()
    test_statistic_list <- list()
    df_one_list <- list()
    df_two_list <- list()
    
    for (i in numerical_columns) {
      for (j  in categorical_columns) {
        num_features <- unlist(append(num_features, i))
        cat_features <- unlist(append(cat_features, j))
        anova_obj <- aov(credit_data[[i]] ~ credit_data[[j]])
        summary_anova_obj <- summary(anova_obj)
        p_value_list <-
          unlist(append(p_value_list, summary_anova_obj[[1]][[1, 5]]))
        test_statistic_list  <- unlist(append(test_statistic_list ,
                                              summary_anova_obj[[1]][[1, 4]]))
        df_one_list  <-
          unlist(append(df_one_list , summary_anova_obj[[1]]$Df[1]))
        df_two_list  <-
          unlist(append(df_two_list , summary_anova_obj[[1]]$Df[2]))
        critical_value_list <- qf(0.95, df_one_list, df_two_list)
      }
    }
    
    df_anova <- data.frame(
      num_features,
      cat_features,
      p_value_list,
      test_statistic_list,
      df_one_list,
      df_two_list,
      critical_value_list
    )
    
    colnames(df_anova) <- c(
      "numerical_feature",
      "categorical_feature",
      "p_value",
      "test_statistic_f-value",
      "degree_of_freedom_one",
      "degree_of_freedom_two",
      "critical_value"
    )
    
    return(df_anova)
  }

anova_df <-
  anova_test_results(categorical_columns, numerical_columns)

anova_df  %>% filter(p_value > 0.05)

write.csv(anova_df,
          "~/Desktop/Shiny_application/files_generated/anova_df.csv")

########################################################
################### Chi-Squared Test ###################
########################################################


#' Title -  Chi-Squared Test Means for Good and Bad Applications for categorical features
#'
#' @param categorical_features - Categorical features vector in the present in the credit data
#'
#' @return - A dataframe with Chi-Squared Test Means for Good and Bad Applications for categorical features
#' with columns "categorical_feature", "p_value", "test_statistic_X-squared", "degree_of_freedom", "critical_value"


chisq_test_results <- function(categorical_features) {
  categorical_features <-
    categorical_features[!categorical_features %in% "credit_risk"]
  
  cat_list <- list()
  p_value_list <- list()
  test_statistic_list <- list()
  degree_of_freedom_list <- list()
  
  for (i in categorical_features) {
    cat_list <- unlist(append(cat_list, i))
    frequency_table <-
      table(credit_data[["credit_risk"]], credit_data[[i]])
    
    chisquared_test_obj <-
      suppressWarnings(chisq.test(frequency_table))
    p_value_list <-
      unlist(append(p_value_list, chisquared_test_obj$p.value))
    test_statistic_list <-
      unlist(append(test_statistic_list, chisquared_test_obj$statistic))
    degree_of_freedom_list <-
      unlist(append(degree_of_freedom_list, chisquared_test_obj$parameter))
    critical_value_list <- qchisq(0.95, degree_of_freedom_list)
  }
  
  chisquared_df <-
    data.frame(
      cat_list,
      p_value_list,
      test_statistic_list,
      degree_of_freedom_list,
      critical_value_list
    )
  
  colnames(chisquared_df) <- c(
    "categorical_feature",
    "p_value",
    "test_statistic_X-squared",
    "degree_of_freedom",
    "critical_value"
  )
  
  
  return(chisquared_df)
}


chisquared_data <- chisq_test_results(categorical_columns)

chisquared_data %>% filter(p_value < 0.05)

chisquared_data %>% filter(`test_statistic_X-squared` < critical_value)

write.csv(
  chisquared_data,
  "~/Desktop/Shiny_application/files_generated/chisquared_data.csv"
)

###################################################################
###################### Machine learning ###########################
###################################################################

#########################################
############## Sampling #################
#########################################


# Split ratio
set.seed(123)
split <- sample.split(Y = credit_data$credit_risk, SplitRatio = .75)

# Training data
training_data <- subset(credit_data, split == TRUE)

# Testing data
testing_data  <- subset(credit_data, split == FALSE)


saveRDS(credit_data,
        "~/Desktop/Shiny_application/application/credit_data.rds")
saveRDS(training_data,
        "~/Desktop/Shiny_application/application/training_data.rds")
saveRDS(testing_data,
        "~/Desktop/Shiny_application/application/testing_data.rds")


# Target proportion in training data
table(training_data$credit_risk) / nrow(training_data) * 100

# Target proportion in testing data
table(testing_data$credit_risk) / nrow(testing_data) * 100





###########################################################
##################### Model Selection #####################
###########################################################

# Method list of caret models
method_list <- c(
  "LogitBoost",
  "LMT",
  "naive_bayes",
  "PART",
  'C5.0Rules',
  "C5.0Tree",
  "treebag",
  "bayesglm",
  "rpart",
  "rpart2",
  "ctree",
  "ctree2",
  "glm",
  "rf",
  "chaid"
)

# Model name list of caret models
model_list <- c(
  "Boosted Logistic Regression",
  "Logistic Model Trees",
  "Naive Bayes",
  "Rule-Based Classifier",
  "Single C5.0 Ruleset",
  "Single C5.0 Tree",
  "Bagged CART",
  "Bayesian Generalized Linear Model",
  "CART (complexity parameter)",
  "CART (maxdepth)",
  "Conditional Inference Tree (mincriterion)",
  "Conditional Inference Tree (maxdepth)",
  "Generalized Linear Model",
  "Random Forest",
  "CHi-squared Automated Interaction Detection"
)


# Models Dataframe
caret_models_df <- data.frame(model_list, method_list)
caret_models_df <- caret_models_df %>% arrange(method_list)
caret_models_df
colnames(caret_models_df) <- c("model", "method")
caret_models_df

###################################################################


#' Title - Function for model selection and comparison
#'
#' @param classification_models -  classification models from caret package

#' @param return_type - if return_type == "confusion matrix" then return confusion matrices from the models

#' - if return_type == "metrics dataframe" then return dataframe with model performance metrics with columns "Model", "Method","Accuracy","Kappa","Sensitivity","Specificity","Precision","Recall","F1-score","True_Positives","True_Negatives","False_Positives","False_Negatives","AUC"

#' - if return_type == "ROC curve" then return ROC curves for the models.


model_selection <-
  function(classification_models,
           return_type) {
    classification_models <- sort(classification_models)
    
    
    # Repeated 10 fold  cross-validation
    set.seed(123)
    fit_control <- trainControl(
      method = "repeatedcv",
      number = 10,
      repeats = 10,
      search = "random"
    )
    
    classification_report_list <- list()
    confusion_matrix_list      <- list()
    accuracy_list    <- list()
    kappa_list       <- list()
    sensitivity_list <- list()
    specificity_list <- list()
    precision_list   <- list()
    recall_list      <- list()
    f1score_list     <- list()
    true_positives_list   <- list()
    true_negatives_list   <- list()
    false_positives_list  <- list()
    false_negatives_list  <- list()
    rocr_curve_objs <- list()
    auc_list <- list()
    
    
    for (i in classification_models) {
      set.seed(123)
      
      if (i %in% c("C5.0Rules","C5.0Tree","LogitBoost")) {
        classification_model <- train(
          credit_risk~.,
          data = training_data,
          method = i,
          trControl = fit_control
        )
      }
      
      else if (i == "chaid"){
        classification_model <-  caret::train(
          training_data %>% dplyr::select(account_balance,credit_history,credit_history,
                                          savings_account_bonds,savings_account_bonds,
                                          employment_history,marital_status_sex,
                                          debtors_guarantor_status,property_type,
                                          other_installment_plans,housing_type,job_status,
                                          telephone_status,foreign_worker),
          training_data$credit_risk,
          method = i,
          trControl = fit_control
        )
      }
      
      else {
        classification_model <- train(
          training_data %>% dplyr::select(-credit_risk),
          training_data$credit_risk,
          method = i,
          trControl = fit_control
        )
      }
      
      
      model_predictions <- predict(classification_model,
                                   newdata = testing_data,
                                   type = "raw")
      
      model_probabilities <-
        predict(classification_model,
                newdata = testing_data,
                type = "prob")
      
      report <- confusionMatrix(model_predictions,
                                testing_data$credit_risk, positive = "Bad")
      
      classification_report_list <-
        append(classification_report_list, report)
      accuracy_list    <-
        unlist(append(accuracy_list, report$overall[1]))
      kappa_list       <-
        unlist(append(kappa_list, report$overall[2]))
      sensitivity_list <-
        unlist(append(sensitivity_list, report$byClass[1]))
      specificity_list <-
        unlist(append(specificity_list, report$byClass[2]))
      precision_list   <-
        unlist(append(precision_list, report$byClass[5]))
      recall_list      <-
        unlist(append(recall_list, report$byClass[6]))
      f1score_list     <-
        unlist(append(f1score_list, report$byClass[7]))
      true_positives_list   <-
        unlist(append(true_positives_list, report$table[4]))
      true_negatives_list   <-
        unlist(append(true_negatives_list, report$table[1]))
      false_positives_list  <-
        unlist(append(false_positives_list, report$table[3]))
      false_negatives_list  <-
        unlist(append(false_negatives_list, report$table[2]))
      rocr_curves <-
        roc(testing_data$credit_risk, model_probabilities$Bad)
      rocr_curve_objs <- append(rocr_curve_objs, list(rocr_curves))
      auc_list <-
        unlist(append(
          auc_list,
          auc(testing_data$credit_risk, model_probabilities$Bad)
        ))
    }
    
    names(rocr_curve_objs) <-
      caret_models_df[caret_models_df$method %in% classification_models, "model"]
    
    metrics_data_frame <-
      data.frame(
        caret_models_df[caret_models_df$method %in% classification_models, "model"],
        classification_models,
        accuracy_list,
        kappa_list,
        sensitivity_list,
        specificity_list,
        precision_list,
        recall_list,
        f1score_list,
        true_positives_list,
        true_negatives_list,
        false_positives_list,
        false_negatives_list,
        auc_list
      )
    
    colnames(metrics_data_frame) <-
      c(
        "Model",
        "Method",
        "Accuracy",
        "Kappa",
        "Sensitivity",
        "Specificity",
        "Precision",
        "Recall",
        "F1-score",
        "True_Positives",
        "True_Negatives",
        "False_Positives",
        "False_Negatives",
        "AUC"
      )
    
    if (return_type == "confusion matrix") {
      for (i in seq(2, length(classification_report_list), 6)) {
        confusion_matrix_list <-
          append(confusion_matrix_list, classification_report_list[i])
      }
      
      names(confusion_matrix_list) <-
        caret_models_df[caret_models_df$method %in% classification_models, "model"]
      
      return(confusion_matrix_list)
      
    } else if (return_type == "metrics dataframe") {
      return(metrics_data_frame)
      
    } else if (return_type == "ROC curve") {
      rocr_plot <-
        ggroc(
          rocr_curve_objs,
          legacy.axes = FALSE,
          linetype = 1,
          size = .3
        ) +
        labs(color = 'Model') + ggtitle("ROC Curve")  +
        geom_segment(aes(
          x = 1,
          xend = 0,
          y = 0,
          yend = 1
        ),
        color = "grey",
        linetype = "dashed")
      
      rocr_plot <- ggplotly(rocr_plot)
      
      return(rocr_plot)
    }
  }

#' Title - Tuning rpart model
#'
#' @param weight_list 
#' @param return_type 
#' @param algorithm 
#'
#' @return


tuning_rpart <- function(weight_list, return_type,algorithm) {
  classification_report_list <- list()
  confusion_matrix_list      <- list()
  accuracy_list    <- list()
  true_positives_list   <- list()
  true_negatives_list   <- list()
  false_positives_list  <- list()
  false_negatives_list  <- list()
  rocr_curve_objs <- list()
  auc_list <- list()
  wt_ratio <- list()
  
  for (i in weight_list) {
    set.seed(123)
    fit_control_wt <- trainControl(
      method = "repeatedcv",
      number = 10,
      repeats = 10,
      search = "random",
      verbose = FALSE
    )
    
    weights <- ifelse(training_data$credit_risk == "Good", 1, i)
    
    
    set.seed(123)
    rpart_model_wt <- caret::train(
      training_data %>% dplyr::select(-credit_risk),
      training_data$credit_risk,
      method = algorithm,
      trControl = fit_control_wt,
      weights = weights
    )
    
    model_predictions <- predict(rpart_model_wt,
                                 newdata = testing_data,
                                 type = "raw")
    
    model_probabilities <-
      predict(rpart_model_wt,
              newdata = testing_data,
              type = "prob")
    
    report <- confusionMatrix(model_predictions,
                              testing_data$credit_risk,
                              positive = "Bad")
    
    classification_report_list <-
      append(classification_report_list, report)
    
    accuracy_list  <-
      unlist(append(accuracy_list, report$overall[1]))
    
    true_positives_list   <-
      unlist(append(true_positives_list, report$table[4]))
    true_negatives_list   <-
      unlist(append(true_negatives_list, report$table[1]))
    false_positives_list  <-
      unlist(append(false_positives_list, report$table[3]))
    false_negatives_list  <-
      unlist(append(false_negatives_list, report$table[2]))
    
    rocr_curves <-
      roc(testing_data$credit_risk, model_probabilities$Bad)
    
    rocr_curve_objs <- append(rocr_curve_objs, list(rocr_curves))
    
    auc_list <-
      unlist(append(
        auc_list,
        auc(testing_data$credit_risk, model_probabilities$Bad)
      ))
    
    wt_ratio <- unlist(append(wt_ratio, i))
  }
  
  metrics_data_frame <-
    data.frame(
      wt_ratio,
      accuracy_list,
      true_positives_list,
      true_negatives_list,
      false_positives_list,
      false_negatives_list,
      auc_list
    )
  
  colnames(metrics_data_frame) <-
    c(
      "Weight",
      "Accuracy",
      "True_Positives",
      "True_Negatives",
      "False_Positives",
      "False_Negatives",
      "AUC"
    )
  
  metrics_data_frame$Weight <-
    paste("Good : Bad = 1 :", metrics_data_frame$Weight)
  
  if (return_type == "confusion matrix") {
    for (i in seq(2, length(classification_report_list), 6)) {
      confusion_matrix_list <-
        append(confusion_matrix_list, classification_report_list[i])
    }
    names(confusion_matrix_list) <- wt_ratio
    return(confusion_matrix_list)
  } else if (return_type == "metrics dataframe") {
    return(metrics_data_frame)
  } else if (return_type == "ROC curve") {
    names(rocr_curve_objs) <- metrics_data_frame$Weight
    rocr_plot <-
      ggroc(
        rocr_curve_objs,
        legacy.axes = FALSE,
        linetype = 1,
        size = .3
      ) +
      labs(color = 'Model') + ggtitle("ROC Curve")  +
      geom_segment(aes(
        x = 1,
        xend = 0,
        y = 0,
        yend = 1
      ),
      color = "grey",
      linetype = "dashed")
    
    rocr_plot <- ggplotly(rocr_plot)
    
    return(rocr_plot)
  }
}

rc <- model_selection(
  c("chaid","rpart","ctree"),
  "ROC curve")

rc


'''
#####

confusion_matrix_tree_based <-
  model_selection(
    c("chaid","rpart","C5.0Tree"),
    "confusion matrix"
  )

metrics_df_tree_based  <-
  model_selection(
    c("chaid","rpart","C5.0Tree"),
    "metrics dataframe"
  )


rocr_plot_plotly_tree_based <-
  model_selection(
    c("chaid","rpart","C5.0Tree"),
                  "ROC curve")

# ROC curves

saveRDS(rocr_plot_plotly_tree_based,"~/Desktop/Shiny_application/files_generated/roc_curves_tree_based.rds")


# confusion matrices


suppressWarnings(lapply(confusion_matrix_tree_based,
                        function(x) write.table( data.frame(x),
                                                 "~/Desktop/Shiny_application/files_generated/confusion_matrices_tree_based.txt",
                                                 append= T, sep= "," )))

saveRDS(confusion_matrix_tree_based,"~/Desktop/Shiny_application/files_generated/confusion_matrices_tree_based.rds")

# model metrics

write.csv(metrics_df_tree_based,"~/Desktop/Shiny_application/files_generated/model_metrics_tree_based.csv")
saveRDS(metrics_df_tree_based,"~/Desktop/Shiny_application/files_generated/model_metrics_tree_based.rds")

'''
#####


##########
##########
##########
'''

confusion_matrix_dat <-
  model_selection(
    caret_models_df$method,
    "confusion matrix"
  )


metrics_df  <-
  model_selection(
    caret_models_df$method,
    "metrics dataframe"
  )

rocr_plot_plotly <-
  model_selection(caret_models_df$method,
                  "ROC curve")


# ROC curves
rocr_plot_plotly
saveRDS(rocr_plot_plotly,"~/Desktop/Shiny_application/files_generated/roc_curves.rds")


# confusion matrices
confusion_matrix_dat

suppressWarnings(lapply(confusion_matrix_dat,
       function(x) write.table( data.frame(x),
                                "~/Desktop/Shiny_application/files_generated/confusion_matrices.txt",
                                append= T, sep= "," )))

saveRDS(confusion_matrix_dat,"~/Desktop/Shiny_application/files_generated/confusion_matrices.rds")

# model metrics
metrics_df
write.csv(metrics_df,"~/Desktop/Shiny_application/files_generated/model_metrics.csv")
saveRDS(metrics_df,"~/Desktop/Shiny_application/files_generated/model_metrics.rds")

##########
##########
##########
'''


# ROC curves
roc_curves_rds  <-
  readRDS("~/Desktop/Shiny_application/files_generated/roc_curves.rds")

# confusion matrices
confusion_matrices_rds <-
  readRDS("~/Desktop/Shiny_application/files_generated/confusion_matrices.rds")

# model metrics
model_metrics_rds <-
  readRDS("~/Desktop/Shiny_application/files_generated/model_metrics.rds")


imp_models <- c("bayesglm", "ctree", "glm", "LMT", "treebag")

####################################################
#################  Logistic model #################
###################################################


# Repeated 10 fold  cross-validation
set.seed(123)
fit_control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  search = "random"
)

# Baseline logistic model
logit_model <- caret::train(
  training_data %>% dplyr::select(-credit_risk),
  training_data$credit_risk,
  method = "glm",
  trControl = fit_control
)



# Model Summary
summary(logit_model)

# Predict Probabilities
logit_probabilities <- predict(logit_model,
                               type = "prob", newdata = testing_data)

# Predict labels
logit_predictions <- predict(logit_model,
                             type = "raw", newdata = testing_data)

# Classification report
confusionMatrix(logit_predictions, testing_data$credit_risk, positive = "Bad")

saveRDS(logit_model,
        "~/Desktop/Shiny_application/application/sample_model.rds")

##########################################################
#################  Logistic model Update #################
##########################################################



# Repeated 10 fold  cross-validation
set.seed(123)
fit_control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  search = "random"
)


# Logistic model Update
logit_model_update <- caret::train(
  credit_risk ~ account_balance + log(month_duration) +
    credit_purpose  +  savings_account_bonds * credit_history +
    employment_history + installment_percent_disposable_income +
    marital_status_sex * I(age_years ^ 2) +
    debtors_guarantor_status +
    residence_history_years * property_type  +
    I(credit_amount ^ 2)  +
    other_installment_plans + housing_type +
    existing_credits_current_bank + job_status * foreign_worker +
    people_liable_maintainance +
    telephone_status ,
  data = training_data,
  method = "glm",
  trControl = fit_control
)


# Model Summary
summary(logit_model_update)

formula(logit_model_update$finalModel)


# Predict Probabilities
logit_model_update_probabilities <- predict(logit_model_update,
                                            type = "prob", newdata = testing_data)


# Predict labels
logit_model_update_predictions <- predict(logit_model_update,
                                          type = "raw", newdata = testing_data)

# Classification report
confusionMatrix(logit_model_update_predictions,
                testing_data$credit_risk,
                positive = "Bad")



saveRDS(logit_model,
        "~/Desktop/Shiny_application/application/logistic_regression_model.rds")


##########################################################
#################  Logistic Model Trees #################
#########################################################


# Repeated 10 fold  cross-validation
set.seed(123)
fit_control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  search = "random"
)

# Logistic Model Trees
logit_model_trees <- caret::train(
  credit_risk ~ account_balance + log(month_duration) +
    credit_purpose  +  savings_account_bonds * credit_history +
    employment_history + installment_percent_disposable_income +
    marital_status_sex * I(age_years ^ 2) +
    debtors_guarantor_status +
    residence_history_years * property_type  +
    I(credit_amount ^ 2)  +
    other_installment_plans + housing_type +
    existing_credits_current_bank + job_status * foreign_worker +
    people_liable_maintainance +
    telephone_status ,
  data = training_data,
  method = "LMT",
  trControl = fit_control
)


logit_model_trees$finalModel
summary(logit_model_trees)

# Model Summary
summary(logit_model_trees)

logit_model_trees$finalModel

# Predict Probabilities
logit_model_trees_probabilities <- predict(logit_model_trees,
                                           type = "prob", newdata = testing_data)

# Predict labels
logit_model_trees_predictions <- predict(logit_model_trees,
                                         type = "raw", newdata = testing_data)

# Classification report
confusionMatrix(logit_model_trees_predictions,
                testing_data$credit_risk,
                positive = "Bad")

##############################################################################
#################  Conditional Inference Tree (mincriterion) #################
##############################################################################


# Repeated 10 fold  cross-validation
set.seed(123)
fit_control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  search = "random"
)


# Conditional Inference Tree (mincriterion)
set.seed(123)
ctree_model <- caret::train(
  training_data %>% dplyr::select(-credit_risk),
  training_data$credit_risk,
  method = "ctree",
  trControl = fit_control,
)



# Model Summary
summary(ctree_model$finalModel)


plot(ctree_model$finalModel)

ctree_model$finalModel

# Predict Probabilities
ctree_model_probabilities <- predict(ctree_model,
                                     type = "prob", newdata = testing_data)

# Predict labels
ctree_model_predictions <- predict(ctree_model,
                                   type = "raw", newdata = testing_data)

# Classification report
confusionMatrix(ctree_model_predictions, testing_data$credit_risk, positive = "Bad")

##############################################################################
#################  CSingle C5.0 Tree          ################################
##############################################################################

#######################################################################################################
#################  CHi-squared Automated Interaction Detection (alpha2, alpha3, alpha4)      ##########
#######################################################################################################

# Repeated 10 fold  cross-validation
set.seed(123)
fit_control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  search = "random"
)


# Conditional Inference Tree (mincriterion)
set.seed(123)
chaid_model <- caret::train(
  training_data %>% dplyr::select(account_balance,credit_history,credit_history,
                                  savings_account_bonds,savings_account_bonds,
                                  employment_history,marital_status_sex,
                                  debtors_guarantor_status,property_type,
                                  other_installment_plans,housing_type,job_status,
                                  telephone_status,foreign_worker
  ),
  training_data$credit_risk,
  method = "chaid",
  trControl = fit_control
)


# Model Summary
summary(chaid_model$finalModel)
chaid_model$finalModel

plot(chaid_model$finalModel)


fancyRpartPlot(chaid_model$finalModel)

# Predict Probabilities
chaid_model_probabilities <- predict(chaid_model,
                                     type = "prob", newdata = testing_data)

# Predict labels
chaid_model_predictions <- predict(chaid_model,
                                   type = "raw", newdata = testing_data)

# Classification report
confusionMatrix(chaid_model_predictions, testing_data$credit_risk, positive = "Bad")



#######################################################################################################
#################  CART - Classification and Regression Trees (rpart - complexity parameter) ##########
#######################################################################################################


# Repeated 10 fold  cross-validation
set.seed(123)
fit_control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  search = "random"
)


#  rpart model
set.seed(123)
rpart_model <- caret::train(
  training_data %>% dplyr::select(-credit_risk),
  training_data$credit_risk,
  method = "rpart",
  trControl = fit_control
)


# Model Summary
summary(rpart_model$finalModel)


# plot complexity parameter
plot(rpart_model, plotType = "line")



# Predict Probabilities
rpart_probabilities <- stats::predict(rpart_model,
                                      type = "prob",
                                      newdata = testing_data)

# Predict labels
rpart_predictions <- stats::predict(rpart_model,
                                    type = "raw", newdata = testing_data)

# Classification report
confusionMatrix(rpart_predictions, testing_data$credit_risk, positive = "Bad")

# Plot Tree
fancyRpartPlot(rpart_model$finalModel,
               main = "Classification Tree",tweak = 2)


#################################################################################################
#### Final model CART - Classification and Regression Trees (rpart - complexity parameter) ######
#################################################################################################



# Select optimal weight ratio
weight_df <- tuning_rpart(seq(1,2,0.1),return_type = "metrics dataframe","rpart")
weight_df
write.csv(weight_df,"~/Desktop/Shiny_application/files_generated/weight_df.csv")

ggplot(data = weight_df) + geom_point(aes(x = Weight,y= AUC,group = 1,color = "blue"))  +
  geom_point(aes(x = Weight,y= Accuracy,group = 1))  +
  theme(axis.text.x=element_text(angle=90, hjust=1))


plot(weight_df$AUC - weight_df$Accuracy)

# selct model
roc_rpart_select <- tuning_rpart(c(1,1.8,1.7,1.9),return_type = "ROC curve","rpart")
roc_rpart_select


# Repeated 10 fold  cross-validation
set.seed(123)
fit_control_tuned <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  search = "random"
)

# Set weight ratio
weight_ratio <- ifelse(training_data$credit_risk == "Good",1,1.8)

#  rpart model
set.seed(123)
rpart_model_tuned <- caret::train(
  training_data %>% dplyr::select(-credit_risk),
  training_data$credit_risk,
  method = "rpart",
  trControl = fit_control_tuned,
  weights = weight_ratio,
  tuneLength = 6
)

rpart_model_tuned$finalModel

# Model Summary
summary(rpart_model_tuned$finalModel)

rpart_model_tuned

# plot complexity parameter
plot(rpart_model_tuned, plotType = "line")

# Predict Probabilities
rpart_probabilities_tuned <- stats::predict(rpart_model_tuned,
                                            type = "prob",
                                            newdata = testing_data)

# Predict labels
rpart_predictions_tuned <- stats::predict(rpart_model_tuned,
                                          type = "raw", newdata = testing_data)

# Classification report
confusionMatrix(rpart_predictions_tuned, testing_data$credit_risk, positive = "Bad")


# Plot Tree
fancyRpartPlot(rpart_model_tuned$finalModel,tweak=2)

# Model on subset features

# Subset features 
shiny_features <- unique(rpart_model_tuned$finalModel$frame$var)[!unique(rpart_model_tuned$finalModel$frame$var) %in% "<leaf>"]

# Repeated 10 fold  cross-validation
set.seed(123)
fit_control_shiny_dt <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  search = "random"
)

# Set weight ratio
weight_ratio <- ifelse(training_data$credit_risk == "Good",1,1.8)



#  rpart model
set.seed(123)
rpart_model_shiny_dt <- caret::train(
  training_data %>% dplyr::select(all_of(shiny_features)),
  training_data$credit_risk,
  method = "rpart",
  trControl = fit_control_shiny_dt,
  weights = weight_ratio,
  tuneLength = 10
)

rpart_model_shiny_dt

fancyRpartPlot(rpart_model$finalModel)



# plot complexity parameter
plot(rpart_model_shiny_dt, plotType = "line")

# Predict Probabilities
rpart_probabilities_shiny_dt <- stats::predict(rpart_model_shiny_dt,
                                               type = "prob",
                                               newdata = testing_data %>% dplyr::select(all_of(shiny_features)))

rpart_probabilities_shiny_dt %>% head()


# Predict labels
rpart_predictions_shiny_dt <- stats::predict(rpart_model_shiny_dt,
                                             type = "raw", newdata = testing_data %>% dplyr::select(all_of(shiny_features)))

# Classification report
confusionMatrix(rpart_predictions_shiny_dt, testing_data$credit_risk, positive = "Bad")

# Plot Tree
fancyRpartPlot(rpart_model_shiny_dt$finalModel,
               tweak = 2)



####################################
saveRDS(rpart_model_tuned,
        "~/Desktop/Shiny_application/application/cart_model.rds")

saveRDS(rpart_model_shiny_dt,
        "~/Desktop/Shiny_application/application/cart_shiny_model.rds")
####################################


#######



shiny_features

# explainer
explainer <- lime(training_data %>% dplyr::select(all_of(shiny_features)), rpart_model_shiny_dt)

testing_data[1,shiny_features]

# explanation         
explanation <- explain(x = testing_data[1,shiny_features],
                       explainer = explainer,
                       n_labels = 1,n_features = ncol(testing_data[1,]) + 10)

explanation %>% as.data.frame() %>% head(n =1)

plot_features(explanation)

#######



unique(shiny_cart_model$finalModel$frame$var)

