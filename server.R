#############################
# Thomas Cerbelaud | Shravan Kumar Chandrasekaran | Mathilde Sirbu
# SIEO 4150 
# Homework 3, Group problem
# Due date: 09/15
#############################

# Set the path to our folder

source("HMWK#1.R")
library(shiny)

# Define server logic
shinyServer(function(input, output) {
  
  
##############################################################################  
  #BEGIN
  # Output for the main panel (and the side panel for the confidence interval)
  # Plot output
##############################################################################
  
  output$distPlot <- renderPlot({
  
    fromdate <- reactive(input$datefrom)
    todate <- reactive(input$dateto)
    
    c1_inds_op1 <- reactive(input$c1_inds_op1)
    c1_inds_op2 <- reactive(input$c2_inds_op1)
  
 
    #################    
    #CHOICE ONE
    #################           
        if(input$main_radioselection == 'Comparison of Two Industries')
    {
     data_c1_inds_op1_sub = reactive({
                         a <- subset(Data_Sentiments, Industry %in% input$c1_inds_op1)
                         a <- droplevels(a)
                         return(a)
                        })
     data_c1_inds_op1 <- data_c1_inds_op1_sub()
     

      data_c1_inds_op2_sub = reactive({
                         a <- subset(Data_Sentiments, Industry %in% input$c1_inds_op2)
                         a <- droplevels(a)
                         return(a)
                          })
     data_c1_inds_op2 <- data_c1_inds_op2_sub()

     
     
     ####GUIDO INDUSTRY COMPARISON FUNCTION CALL
     
     
     
        }
     #################    
     #CHOICE TWO
     #################    
     else if (input$main_radioselection == 'Analysis of an Industry') {
       
       
       
       fromdate <- reactive(input$datefrom)
       todate <- reactive(input$dateto)
       c2_inds_op1 <- reactive(input$c2_inds_op1)
       
       if(input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two brands'){}
       
       else if (input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two categories'){}
       
       else if (input$c2_radioselection == 'Analysis' & input.c2_checkbox2_cat == true) {}
       
       else if (input$c2_radioselection == 'Analysis' & input.c2_checkbox2_brand == true) {}
       
       else if (input$c2_radioselection == 'Analysis' & input.c2_checkbox2_cat == true & input.c2_checkbox2_brand == true) {}
       
       else {}
       
     }
     
     #################    
     #CHOICE THREE
     #################    
     else if (input$main_radioselection == 'Comparison of Two Brands') {
       
     }
     
     
     #################    
     #CHOICE FOUR
     #################    
     else if (input$main_radioselection == 'Analysis of a Brand') {
       
     }
     
     #################    
     #CATCH ERROR
     #################    
     else {}
     
    #END OF RENDER PLOT
})
  
  
  ##############################################################################  
  #END
  # Output for the main panel (and the side panel for the confidence interval)
  # Plot output
  ##############################################################################
  
  
  ##############################################################################  
  #BEGIN - DIMENSION ANALYSIS
  # Output for the main panel (and the side panel for the confidence interval)
  # Plot output
  ##############################################################################
  
  output$distPlot <- renderPlot({})
  
  ##############################################################################  
  #END - DIMENSION ANALYSIS
  # Output for the main panel (and the side panel for the confidence interval)
  # Plot output
  ##############################################################################
  
  ##############################################################################  
  #BEGIN - WORD CLOUD
  # Output for the main panel (and the side panel for the confidence interval)
  # Plot output
  ##############################################################################
  
  output$WordCloud <- renderPlot({ })
  
  ##############################################################################  
  #END - WORD CLOUD
  # Output for the main panel (and the side panel for the confidence interval)
  # Plot output
  ##############################################################################
    
    
})