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
    ggplot(aes(x = Data_Sentiments$Time, y = Data_Sentiments$SentimentScore), data = Data_Sentiments) + geom_line()
    # draw the histogram with the specified number of bins
    # Draw normal curve when the checkbox is checked
    Subset_Data <- subset(Data_Sentiments, (Data_Sentiments$Industry == input$inds) & (Data_Sentiments$Category == input$cat) & (Data_Sentiments$Brand == input$brand))
  
    # Display interval of confidence when checked
      # Graphically represents the confidence interval
      # Display the numeric form of the confidence interval in the side panel, text output
    
    # Display interval of confidence when checked
      # Display the numeric form of the confidence interval in the side panel, text output
    
  })
  

})