

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

flights <- read.table(unz("DelayedFlights.csv.zip", "DelayedFlights.csv"), nrows=1000, header=T, quote="\"", sep=",")
flights <- read.table(unz("DelayedFlights.csv.zip", "DelayedFlights.csv"), header=T, quote="\"", sep=",")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Partitions the supplied data into a test and train data set using the 
# supplied percentage to the training data set.
#_____________________________________________________________________
createTrainAndTestSets <- function(data, trainPercentage){
  
  test_index <- createDataPartition(y = data,
                                    times = 1,
                                    p = 1 - trainPercentage,
                                    list = FALSE)
  
  train_set <- data[-test_index,]
  
  test_set <- data[test_index,]
  
  list(train_set = train_set, test_set = test_set)
}

data <- createTrainAndTestSets(flights, 0.8)

knn_fit <- kmm3(y ~ ., data=data$train)


