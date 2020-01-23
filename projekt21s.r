library(neighbr)
library(readxl)
library(ISLR)
library(rpart)
library(rattle)
library(SDMTools)
library(dplyr)
library("e1071")
library(BBmisc)
require(tree)
require(dplyr)

source("usefull_functions.R")

path_to_wine <- "winequality-white.csv"
path_to_mushrooms <- "zmienioneDane.xlsx"

setClass(Class="DataObject",
         representation(
           train_data="data.frame", 
           test_data="data.frame",
           test_data_with_class="data.frame"
         )
)

wine_quality <- read.csv("winequality-white.csv", sep = ';')
data <- prepare_data(wine_quality)

# zmienioneDane <- read_excel(path_to_mushrooms)
# Wywalam kolumne z wartosciami NA
# zmienioneDane <- zmienioneDane[c(1:11,13:ncol(zmienioneDane))]
# data <- prepare_data(zmienioneDane)
# blad <- 0

global_svm_model <- svm(formula = quality ~ ., data = data@train_data, type = 'C', kernel = "linear")

global_tree_model <- rpart(quality~., method="class", data=data@train_data, control=rpart.control(minsplit=5))


knn_effectiveness <- 0

svm_effectiveness <- 0
global_svm_effectiveness <- 0

tree_effectiveness <- 0
global_tree_effectiveness <- 0


 for(siema in 1:20){

query <- random_rows(data@test_data, 50)
#query <- data@test_data[1:2,]
neighbours <- get_neighbours(data=data, 
                      queries = query, 
                      amount_of_neighbours = 20)

#bo kurwa tak xd
id_neighbours <- neighbours$test_set_scores[,2:(neighbours$k+1)]

true_quality <- 0
knn_predicted_quality <- 0

svm_predicted_quality <- 0
global_svm_quality <- 0

tree_predicted_quality <- 0
global_tree_quality <- 0 

for (i in 1:nrow(id_neighbours)){
  neighbours_data <- data@train_data[which(data@train_data$id == id_neighbours[i,1]),]
  true_quality[i] <- data@test_data_with_class[which(data@test_data_with_class$id == query[i,1]),]$quality
  
    for (n in 2:ncol(id_neighbours)){
      id <- id_neighbours[i,n]
      neighbours_data <- rbind(neighbours_data,data@train_data[which(data@train_data$id == id),])
    }
  knn_predicted_quality[i] <- as.numeric(tail(names(sort(table(neighbours_data$quality))), 1))

  #svm_model <- svm(formula = quality ~ ., data = neighbours_data, type = 'C', kernel = "linear")
  #summary(svm_model)
  #pred <- predict(svm_model,query[i,])
  svm_predicted_quality[i] <- svm_predict(neighbours_data,query[i,])
  
  #global svm
  pred <- predict(global_svm_model,query[i,])
  global_svm_quality[i] <- as.numeric(levels(pred))[pred]
  
  
  tree_predicted_quality[i] <- tree_predict(neighbours_data,query[i,])
  
  #global tree
  pred <- predict(global_tree_model, query[i,], type="prob")
  global_tree_quality[i] <- as.numeric(colnames(pred)[apply(pred,1,which.max)])
  
  #system.time(pred <- predict(svm_model,quality ~ .))
}

result <- cbind(true_quality,knn_predicted_quality, svm_predicted_quality, global_svm_quality, tree_predicted_quality, global_tree_quality) 
knn_effectiveness[siema] <- nrow(result[which(result[,1] == result[,2]),])

svm_effectiveness[siema] <- nrow(result[which(result[,1] == result[,3]),])
global_svm_effectiveness[siema] <- nrow(result[which(result[,1] == result[,4]),])

tree_effectiveness[siema] <- nrow(result[which(result[,1] == result[,5]),])
global_tree_effectiveness[siema] <- nrow(result[which(result[,1] == result[,6]),])
 }

sum(knn_effectiveness)

sum(svm_effectiveness)
sum(global_svm_effectiveness)

sum(tree_effectiveness)
sum(global_tree_effectiveness)
# for (i in 1:nrow(id_neighbours)){
#   data2 <- data@train_data[id_neighbours[i,1],]
# 
#   # Drzewko z rpart
#   # 
#   # 
#   for (n in 2:20){
#     id <- id_neighbours[i,n]
#     data2 <- rbind(data2,data@train_data[id,])
#   }
#   fit <- rpart(quality~.,
#                method="class", data=data2[,2:ncol(data2)], control=rpart.control(minsplit=5))
# 
#   result <- predict(fit, query[i,], type="prob")
#   # predicted_quality[i] <- as.numeric(colnames(result)[apply(result,1,which.max)])
#   predicted_quality[i] <- get_result_based_on_weights(as.numeric(colnames(result)), result)
# }
# 
# real_quality <- 0
# for (i in 1:nrow(query)){
#   real_quality[i] <- data@test_data_with_class[which(data@test_data_with_class$id == query[i,1]),]$quality
# }
# 
# # Błąd średniokwadratowy??
# blad[siema] <- sum((real_quality-predicted_quality)^2)
# }

# query_with_class <- cbind(query_with_class, neighbours_class)
# query_with_class <- cbind(query_with_class, predicted_quality)
# good_predicte_knn <- query_with_class[query_with_class$quality == query_with_class$neighbours_class,]
# good_predicte <- query_with_class[query_with_class$quality == query_with_class$predicted_quality,]