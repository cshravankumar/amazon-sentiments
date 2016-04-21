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
    if(input$main_radioselection == 'Comparison of Two Industries'){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
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
      
      
    }
    #################    
    #CHOICE TWO
    #################    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two brands'){
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s1_brand_op1_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s1_brand_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      
      data_c2_s1_brand_op2_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s1_brand_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s1_brand_op2 <- data_c2_s1_brand_op2_sub()
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two categories'){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s2_cat_op1_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s2_cat_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s2_cat_op1 <- data_c2_s2_cat_op1_sub()
      
      
      data_c2_s2_cat_op2_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s2_cat_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s2_cat_op2 <- data_c2_s2_cat_op2_sub()
      
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_cat == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s3_cat_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_cat <- data_c2_s3_cat_sub()
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_brand == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s3_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s3_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_brand <- data_c2_s3_brand_sub()
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_cat == TRUE & input$c2_checkbox2_brand == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      
      data_c2_s3_cat_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_cat <- data_c2_s3_cat_sub()
      
      
      
      data_c2_s3_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s3_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_brand <- data_c2_s3_brand_sub()
      
      
    }
    
    #################    
    #CHOICE THREE
    #################    
    else if (input$main_radioselection == 'Comparison of Two Brands') {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c3_brand_op1_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c3_brand_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c3_brand_op1 <- data_c3_brand_op1_sub()
      
      
      data_c3_brand_op2_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c3_brand_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c3_brand_op2 <- data_c3_brand_op2_sub()
      
      
      
      
    }
    
    #################    
    #CHOICE FOUR
    #################    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Comparison') {
      
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
      data_c4_s1_cat_op1_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c4_s1_cat_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s1_cat_op1 <- data_c4_s1_cat_op1_sub()
      
      
      data_c4_s1_cat_op2_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c4_s1_cat_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s1_cat_op2 <- data_c4_s1_cat_op2_sub()
      
    }
    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Analysis' & input$c4_s2_cat == TRUE){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
      data_c4_s3_cat_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c4_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s3_cat <- data_c4_s3_cat_sub()
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Analysis' & input$c4_s2_cat == FALSE){
      
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
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
  
  output$distPlot <- renderPlot({
    
    #################    
    #CHOICE ONE
    #################           
    if(input$main_radioselection == 'Comparison of Two Industries'){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
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
      
      
    }
    #################    
    #CHOICE TWO
    #################    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two brands'){
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s1_brand_op1_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s1_brand_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      
      data_c2_s1_brand_op2_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s1_brand_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s1_brand_op2 <- data_c2_s1_brand_op2_sub()
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two categories'){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s2_cat_op1_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s2_cat_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s2_cat_op1 <- data_c2_s2_cat_op1_sub()
      
      
      data_c2_s2_cat_op2_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s2_cat_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s2_cat_op2 <- data_c2_s2_cat_op2_sub()
      
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_cat == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s3_cat_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_cat <- data_c2_s3_cat_sub()
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_brand == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s3_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s3_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_brand <- data_c2_s3_brand_sub()
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_cat == TRUE & input$c2_checkbox2_brand == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      
      data_c2_s3_cat_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_cat <- data_c2_s3_cat_sub()
      
      
      
      data_c2_s3_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s3_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_brand <- data_c2_s3_brand_sub()
      
      
    }
    
    #################    
    #CHOICE THREE
    #################    
    else if (input$main_radioselection == 'Comparison of Two Brands') {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c3_brand_op1_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c3_brand_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c3_brand_op1 <- data_c3_brand_op1_sub()
      
      
      data_c3_brand_op2_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c3_brand_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c3_brand_op2 <- data_c3_brand_op2_sub()
      
      
      
      
    }
    
    #################    
    #CHOICE FOUR
    #################    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Comparison') {
      
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
      data_c4_s1_cat_op1_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c4_s1_cat_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s1_cat_op1 <- data_c4_s1_cat_op1_sub()
      
      
      data_c4_s1_cat_op2_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c4_s1_cat_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s1_cat_op2 <- data_c4_s1_cat_op2_sub()
      
    }
    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Analysis' & input$c4_s2_cat == TRUE){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
      data_c4_s3_cat_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c4_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s3_cat <- data_c4_s3_cat_sub()
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Analysis' & input$c4_s2_cat == FALSE){
      
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
    }
    
    #################    
    #CATCH ERROR
    #################    
    else {}
    
    
    #END OF RENDER PLOT
    
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
    if(input$main_radioselection == 'Comparison of Two Industries'){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
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
      
      
    }
    #################    
    #CHOICE TWO
    #################    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two brands'){
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s1_brand_op1_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s1_brand_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      
      data_c2_s1_brand_op2_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s1_brand_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s1_brand_op2 <- data_c2_s1_brand_op2_sub()
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two categories'){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s2_cat_op1_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s2_cat_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s2_cat_op1 <- data_c2_s2_cat_op1_sub()
      
      
      data_c2_s2_cat_op2_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s2_cat_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s2_cat_op2 <- data_c2_s2_cat_op2_sub()
      
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_cat == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s3_cat_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_cat <- data_c2_s3_cat_sub()
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_brand == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s3_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s3_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_brand <- data_c2_s3_brand_sub()
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_cat == TRUE & input$c2_checkbox2_brand == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      
      data_c2_s3_cat_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_cat <- data_c2_s3_cat_sub()
      
      
      
      data_c2_s3_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s3_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_brand <- data_c2_s3_brand_sub()
      
      
    }
    
    #################    
    #CHOICE THREE
    #################    
    else if (input$main_radioselection == 'Comparison of Two Brands') {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c3_brand_op1_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c3_brand_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c3_brand_op1 <- data_c3_brand_op1_sub()
      
      
      data_c3_brand_op2_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c3_brand_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c3_brand_op2 <- data_c3_brand_op2_sub()
      
      
      
      
    }
    
    #################    
    #CHOICE FOUR
    #################    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Comparison') {
      
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
      data_c4_s1_cat_op1_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c4_s1_cat_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s1_cat_op1 <- data_c4_s1_cat_op1_sub()
      
      
      data_c4_s1_cat_op2_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c4_s1_cat_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s1_cat_op2 <- data_c4_s1_cat_op2_sub()
      
    }
    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Analysis' & input$c4_s2_cat == TRUE){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
      data_c4_s3_cat_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c4_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s3_cat <- data_c4_s3_cat_sub()
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Analysis' & input$c4_s2_cat == FALSE){
      
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
    }
    
    #################    
    #CATCH ERROR
    #################    
    else {}
    
    
    #END OF RENDER PLOT
    
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
  
    #################    
    #CHOICE ONE
    #################           
    if(input$main_radioselection == 'Comparison of Two Industries'){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
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
      
      
    }
    #################    
    #CHOICE TWO
    #################    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two brands'){
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s1_brand_op1_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s1_brand_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      
      data_c2_s1_brand_op2_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s1_brand_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s1_brand_op2 <- data_c2_s1_brand_op2_sub()
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two categories'){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s2_cat_op1_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s2_cat_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s2_cat_op1 <- data_c2_s2_cat_op1_sub()
      
      
      data_c2_s2_cat_op2_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s2_cat_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s2_cat_op2 <- data_c2_s2_cat_op2_sub()
      
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_cat == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s3_cat_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_cat <- data_c2_s3_cat_sub()
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_brand == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s3_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s3_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_brand <- data_c2_s3_brand_sub()
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_cat == TRUE & input$c2_checkbox2_brand == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      
      data_c2_s3_cat_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_cat <- data_c2_s3_cat_sub()
      
      
      
      data_c2_s3_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s3_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_brand <- data_c2_s3_brand_sub()
      
      
    }
    
    #################    
    #CHOICE THREE
    #################    
    else if (input$main_radioselection == 'Comparison of Two Brands') {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c3_brand_op1_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c3_brand_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c3_brand_op1 <- data_c3_brand_op1_sub()
      
      
      data_c3_brand_op2_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c3_brand_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c3_brand_op2 <- data_c3_brand_op2_sub()
      
      
      
      
    }
    
    #################    
    #CHOICE FOUR
    #################    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Comparison') {
      
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
      data_c4_s1_cat_op1_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c4_s1_cat_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s1_cat_op1 <- data_c4_s1_cat_op1_sub()
      
      
      data_c4_s1_cat_op2_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c4_s1_cat_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s1_cat_op2 <- data_c4_s1_cat_op2_sub()
      
    }
    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Analysis' & input$c4_s2_cat == TRUE){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
      data_c4_s3_cat_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c4_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s3_cat <- data_c4_s3_cat_sub()
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Analysis' & input$c4_s2_cat == FALSE){
      
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
    }
    
    #################    
    #CATCH ERROR
    #################    
    else {}
    
    
    #END OF RENDER PLOT
    
      
  })
  
  ##############################################################################  
  #END - SALES RANK
  # Output for the main panel (and the side panel for the confidence interval)
  # Plot output
  ##############################################################################
  
  
    
})