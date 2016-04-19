#############################
# Thomas Cerbelaud | Shravan Kumar Chandrasekaran | Mathilde Sirbu
# SIEO 4150 
# Homework 3 , Group problem
# Due date: 09/15
#############################
# Set the path to our folder
getwd()
dir = "Historical prices/"
source("HMWK#1.R")
library(shiny)
library(ggplot2)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Customer Sentiment Analytics"),
  # Side panel design
  sidebarLayout(
    sidebarPanel(
      # Checkbox input: whether or not to superimpose the normal distribution
      radioButtons("main_radioselection","Select Mode",c("Comparison of Two Industries", "Analysis of an Industry", "Comparison of Two Brands", "Analysis of a Brand")),
       
       conditionalPanel(
          condition = "input.main_radioselection == 'Comparison of Two Industries'",
            # Sidebar with a slider for the level of confidence. If 95% is chosen for example, then the
            # confidence interval is the interval in which the value of the true mean has 95% probability
            # to be, if repeating the operation when renewing the experiment.
            # Slide bar
              selectInput("c1_inds_op1",
                          label = "Choose an Industry",
                          choices = Industry_choices,
                          selected = ""),
              selectInput("c1_inds_op2",
                          label = "Choose an Industry",
                          choices = Industry_choices,
                          selected = "")
                        ),
          conditionalPanel(
            condition = "input.main_radioselection == 'Analysis of an Industry'",
              # Sidebar with a slider for the level of confidence. If 95% is chosen for example, then the
              # confidence interval is the interval in which the value of the true mean has 95% probability
              # to be, if repeating the operation when renewing the experiment.
            selectInput("c2_inds_op1",
                        label = "Choose an Industry",
                        choices = Industry_choices,
                        selected = ""),
              radioButtons("c2_radioselection","Select Mode",c("Comparison", "Analysis")),
                  conditionalPanel(
                    condition = "input.c2_radioselection == 'Comparison'",
                   # Slide bar
                          radioButtons("c2_radioselection_s1","Select Mode",c("Comparison between two brands", "Comparison between two categories")),
                                  
                               conditionalPanel(
                                    condition = "input.c2_radioselection_s1 == 'Comparison between two brands'",
                                        selectInput("c2_s1_brand_op1",
                                                    label = "Choose a Brand",
                                                    choices = Brand_choices,
                                                    selected = ""),
                         
                                        selectInput("c2_s1_brand_op2",
                                                    label = "Choose a Brand",
                                                    choices = Brand_choices,
                                                    selected = "")
                                    
                                                ),
      
                                 conditionalPanel(
                                       condition = "input.c2_radioselection_s2 == 'Comparison between two categories'",
                                           selectInput("c2_s2_cat_op1",
                                                       label = "Choose a Category",
                                                       choices = Category_choices,
                                                       selected = ""),
                                           
                                           selectInput("c2_s2_cat_op2",
                                                       label = "Choose a Category",
                                                       choices = Category_choices,
                                                       selected = "")
                                   
                                                ),
  
                    conditionalPanel(
                      condition = "input.c2_radioselection == 'Analysis'",
                      # Slide bar
                                  print("Opt 4 selected"),
                                    checkboxInput(
                                    "c2_checkbox_cat",
                                    "Category"),
                                  checkboxInput(
                                    "c2_checkbox_brand",
                                    "Brand")
                                    )
  
    )
    
          ),
    
    
    ###### IT STARTS HERE
    
    conditionalPanel(
      condition = "input.main_radioselection == 'Comparison of Two Brands'",
      # Sidebar with a slider for the level of confidence. If 95% is chosen for example, then the
      # confidence interval is the interval in which the value of the true mean has 95% probability
      # to be, if repeating the operation when renewing the experiment.
      # Slide bar
      selectInput("c3_brand_op1",
                  label = "Choose a Brand",
                  choices = Brand_choices,
                  selected = ""),
      selectInput("c3_brand_op2",
                  label = "Choose a Brand",
                  choices = Brand_choices,
                  selected = "")
    ),
    conditionalPanel(
      condition = "input.main_radioselection == 'Analysis of a Brand'",
      # Sidebar with a slider for the level of confidence. If 95% is chosen for example, then the
      # confidence interval is the interval in which the value of the true mean has 95% probability
      # to be, if repeating the operation when renewing the experiment.
      selectInput("c4_brand",
                  label = "Choose a Brand",
                  choices = Brand_choices,
                  selected = ""),
      radioButtons("c4_radioselection","Select Mode",c("Comparison", "Analysis")),
      conditionalPanel(
        condition = "input.c4_radioselection == 'Comparison'",
          selectInput("c4_s1_cat_op1",
                      label = "Choose a Category",
                      choices = Category_choices,
                      selected = ""),
          selectInput("c4_s1_cat_op2",
                      label = "Choose a Category",
                      choices = Category_choices,
                      selected = "")
                      ),
        conditionalPanel(
          condition = "input.c4_radioselection == 'Analysis'",
          # Slide bar
          checkboxInput(
            "c4_s2_cat",
            "Category")
        )
      )
    ),
    # Show a plot of the generated distribution, Output, Main panel
    mainPanel(
      tabsetPanel(
        tabPanel("Time Series",  plotOutput("distPlot")
      )
    )
  )
)
))