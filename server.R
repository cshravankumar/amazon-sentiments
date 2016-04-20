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
  
  
  
  # Output for the main panel (and the side panel for the confidence interval)
  # Plot output

  
  output$distPlot <- renderPlot({
    #plot(Data_Sentiments$SentimentScore, type = 'l')
    #ggplot(aes(x = Data_Sentiments$Time, y = Data_Sentiments$SentimentScore), data = Data_Sentiments) + geom_line()
    # draw the histogram with the specified number of bins
    # Draw normal curve when the checkbox is checked
    #Subset_Data <- subset(Data_Sentiments, (Data_Sentiments$Industry == input$inds) & (Data_Sentiments$Category == input$cat) & (Data_Sentiments$Brand == input$brand))
  
    c1_inds_op1 <- reactive(input$c1_inds_op1)
    
    c1_inds_op2 <- reactive(input$c2_inds_op1)
    
    if(input$main_radioselection == 'Comparison of Two Industries')
    {
      #if(!is.null(input$c1_inds_op1) & !is.null(input$c1_inds_op2)){
   
      c1_Subset_Data <- subset(Data_Sentiments, (Industry == input$c1_inds_op1))
   
      c2_Subset_Data <- subset(Data_Sentiments, (Industry == input$c1_inds_op2))
      
     subset(Data_Sentiments, (Data_Sentiments$Industry == input$c1_inds_op2))
      
    
     data_sub = reactive({
       a <- subset(Data_Sentiments, Industry %in% input$c1_inds_op1)
       a <- droplevels(a)
       return(a)
     })
     
     dd <- data_sub()
     
     
     data_sub1 = reactive({
       a <- subset(Data_Sentiments, Industry %in% input$c1_inds_op2)
       a <- droplevels(a)
       return(a)
     })
     
     dd1 <- data_sub1()
     
     
     
     
      #plot(c1_Subset_Data$SentimentScore, type = 'l')
      #ggplot(aes(x = Data_Sentiments$Time, y = Data_Sentiments$SentimentScore), data = Data_Sentiments) + geom_line()
      ggplot(aes(x = dd$Time, y = dd$SentimentScore), data = dd) + geom_line(colour="#000099")  #
      #abline(v = c2_Subset_Data$SentimentScore, col = "palevioletred", lw = 2)
      
     # }
    }
    
    
    # Display interval of confidence when checked
      # Graphically represents the confidence interval
      # Display the numeric form of the confidence interval in the side panel, text output
    
    # Display interval of confidence when checked
      # Display the numeric form of the confidence interval in the side panel, text output
    
  })
  
  
})