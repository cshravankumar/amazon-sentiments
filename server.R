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
  
    #################    
    #CHOICE ONE
    #################           
    if(input$main_radioselection == 'Comparison of Two Industries'){}
    #################    
    #CHOICE TWO
    #################    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two brands'){}
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two categories'){}
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_cat == TRUE) {}
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_brand == TRUE) {}
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_cat == TRUE & input$c2_checkbox2_brand == TRUE) {}
    
    #################    
    #CHOICE THREE
    #################    
    else if (input$main_radioselection == 'Comparison of Two Brands') {}
    
    #################    
    #CHOICE FOUR
    #################    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Comparison') {}
    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Analysis' & input$c4_s2_cat == TRUE){}
    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Analysis' & input$c4_s2_cat == FALSE){}
    
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
  
  output$distPlot <- renderPlot({
    
    #################    
    #CHOICE ONE
    #################           
    if(input$main_radioselection == 'Comparison of Two Industries'){}
    #################    
    #CHOICE TWO
    #################    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two brands'){}
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two categories'){}
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_cat == TRUE){}
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_brand == TRUE) {}
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_cat == TRUE & input$c2_checkbox2_brand == TRUE) {}
    
    #################    
    #CHOICE THREE
    #################    
    else if (input$main_radioselection == 'Comparison of Two Brands') {}
    
    #################    
    #CHOICE FOUR
    #################    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Comparison') {}
    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Analysis' & input$c4_s2_cat == TRUE){}
    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Analysis' & input$c4_s2_cat == FALSE){}
    
    #################    
    #CATCH ERROR
    #################    
    else {}
    
    
  })
  
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
  
  output$WordCloud <- renderPlot({
    
    #################    
    #CHOICE ONE
    #################           
    if(input$main_radioselection == 'Comparison of Two Industries'){}
    #################    
    #CHOICE TWO
    #################    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two brands'){}
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two categories'){}
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_cat == TRUE) {}
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_brand == TRUE) {}
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_cat == TRUE & input$c2_checkbox2_brand == TRUE) {}
    
    #################    
    #CHOICE THREE
    #################    
    else if (input$main_radioselection == 'Comparison of Two Brands') {}
    
    #################    
    #CHOICE FOUR
    #################    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Comparison') {}
    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Analysis' & input$c4_s2_cat == TRUE){}
    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Analysis' & input$c4_s2_cat == FALSE){}
    
    #################    
    #CATCH ERROR
    #################    
    else {}
    
  })
  
  ##############################################################################  
  #END - WORD CLOUD
  # Output for the main panel (and the side panel for the confidence interval)
  # Plot output
  ##############################################################################
    
  ##############################################################################  
  #BEGIN - SALES RANK
  # Output for the main panel (and the side panel for the confidence interval)
  # Plot output
  ##############################################################################
  
  output$SalesRank <- renderPlot({
    
  })
  
  ##############################################################################  
  #END - SALES RANK
  # Output for the main panel (and the side panel for the confidence interval)
  # Plot output
  ##############################################################################
  
  
    
})