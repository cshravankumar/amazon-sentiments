#############################
# Thomas Cerbelaud | Shravan Kumar Chandrasekaran | Mathilde Sirbu
# SIEO 4150 
# Homework 1 , Group problem
# Due date: 09/15
#############################


dataset <- read.csv("/Users/Shravan/RShiny/amazon-sentiments/sentiments_new.csv")

SentiCols = c("Industry", "Category", "Brand","ReviewTime","SentimentScore", "Time", "SalesRank", "Overall", "Price", "Quality")

Data_Sentiments = data.frame(Industry = dataset["Industry"], Category = dataset["Category"], Brand = dataset["Brand"], ReviewTime = dataset["reviewTime"], SentimentScore = dataset["Sentiment.Score"], Time = as.Date(0,origin="1970-01-01"), SalesRank = dataset["SalesRank"], Overall = dataset["overall"], Price = dataset["Price"], Quality = dataset["Quality"])
colnames(Data_Sentiments) <- SentiCols

Industry_choices <- as.list(Data_Sentiments$Industry)
names(Industry_choices) <- Data_Sentiments$Industry

Category_choices <- as.list(Data_Sentiments$Category)
names(Category_choices) <- Data_Sentiments$Category

Brand_choices <- as.list(Data_Sentiments$Brand)
names(Brand_choices) <- Data_Sentiments$Brand

for (i in 1:length(Data_Sentiments$ReviewTime)) { 
  
  {
    Data_Sentiments$Time[i] <- as.Date(as.POSIXct(Data_Sentiments$ReviewTime[i], origin="1970-01-01"))
  }
}

