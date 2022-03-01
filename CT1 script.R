# Critical Thinking 1
# 
# Course Code: DS-610
# Course Name: Advanced Applied Statistics
# Module: 03
#
# Student ID: G200007615
# Student Name: Abdulaziz Alqumayzi
# CRN: 15057



# import libraries
library(mlogit)

# read data 
songs <- read.csv("~/Master DS/Level-3/DS-610 Advanced Statistics/Critical Thinking/CT1/songs.csv")
View(songs)

# check duplicates 
length(unique(songs$songID)) == nrow(songs)

### Task-1 ###

# Specify the total number of observations
nrow(songs)

# the artist has the most number of songs in the dataset
sort(table(songs$artistname),decreasing=TRUE)[1:5]

# split data to training and testing sets 
train <- songs[songs$year < 2010,]
test <- songs[songs$year == 2010,]

# Specify the total number of observations in traning and testing sets
nrow(train)
nrow(test)

### Task-2 ###

# excluding the following variables from the model: "year", "songtitle", "artistname","songID", and "artistID").
drops <- c("year","songtitle","artistname","songID","artistID")
train <- train[ , !(names(train) %in% drops)]
test <- test[ , !(names(test) %in% drops)]

# change Top10 variable type to factor 
songs$Top10 <- as.factor(songs$Top10)

# build a logistic regression model to predict whether a song will be in the top 10
glm_model <- glm(Top10 ~ timesignature + timesignature_confidence + loudness + tempo +
                  tempo_confidence + key + key_confidence + energy + pitch + timbre_0_min +
                  timbre_0_max + timbre_1_min + timbre_1_max + timbre_2_min + timbre_2_max + 
                  timbre_3_min + timbre_5_max + timbre_6_min + timbre_6_max + timbre_7_min +
                  timbre_7_max + timbre_8_min + timbre_8_max + timbre_9_min + timbre_9_max + 
                  timbre_10_min + timbre_10_max + timbre_11_min + timbre_11_max, data =  train,
                 family = binomial())


# predicting our model on the test dataset
predicted = predict(glm_model, newdata = test, type = 'response')
predicted

### Task-3 ### 

# a table of the model summary
summary(glm_model)

### Task-4 ###

# investigating glm_model summary created

# a summary of loudness variable
summary(songs$loudness)

### Task-5 ###

# matrix model to predict each song to be in top 10 using 0.35 threshold
matrix_in_top10 <- table(Actual_value= test$Top10, Predicted_value= predicted >=0.35)
matrix_in_top10

# simple model to predict each song not to be in top 10 using 0 threshold
not_in_top10 <- table(Actual_value= test$Top10, Predicted_value= predicted >=0)
not_in_top10

# matrix to randomly predict a song to be in top 10 with probability 0.2
table(Actual_value= test$Top10, Predicted_value= predicted >=0.2)

# matrix to randomly predict a song to be in top 10 with probability 0.5
table(Actual_value= test$Top10, Predicted_value= predicted >=0.5)

### Task-6 ###

# accuracy score
((matrix_in_top10[[1,1]] + matrix_in_top10[[2,2]])/sum(matrix_in_top10))*100

# True Positive Rate
(matrix_in_top10[[2,2]]/(matrix_in_top10[[2,2]]+matrix_in_top10[[1,2]]))*100

# False positive Rate
(1-(matrix_in_top10[[2,2]]/(matrix_in_top10[[2,2]]+matrix_in_top10[[1,2]])))*100

### Task-7 ###

#matrix model to predict each song to be in top 10 using 0.45 threshold
matrix <- table(Actual_value= test$Top10, Predicted_value= predicted >=0.45)
matrix

# accuracy score
((matrix[[1,1]] + matrix[[2,2]])/sum(matrix))*100

# True Positive Rate
(matrix[[2,2]]/(matrix[[2,2]]+matrix[[1,2]]))*100

# False positive Rate
(1-(matrix[[2,2]]/(matrix[[2,2]]+matrix[[1,2]])))*100