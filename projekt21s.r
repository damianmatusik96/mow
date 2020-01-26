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
require(gbm)

source("usefull_functionsS.R")

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

# Modele globalne na calym zbiorze trenujacym
global_svm_model <- svm(formula = quality ~ ., data = data@train_data, type = 'C', kernel = "linear")
global_tree_model <- rpart(quality~., method="class", data=data@train_data, control=rpart.control(minsplit=5))

knn_effectiveness <- 0
svm_effectiveness <- 0
global_svm_effectiveness <- 0
tree_effectiveness <- 0
global_tree_effectiveness <- 0
gbm_effectiveness <- 0


for(siema in 1:2) {
  query <- random_rows(data@test_data, 4)
  #query <- data@test_data[1:2,]
  neighbours <- get_neighbours(data=data, 
                        queries = query, 
                        amount_of_neighbours = 50)
  
  #bo kurwa tak xd
  id_neighbours <- neighbours$test_set_scores[,2:(neighbours$k+1)]
  
  true_quality <- 0
  knn_predicted_quality <- 0
  
  svm_predicted_quality <- 0
  global_svm_quality <- 0
  
  tree_predicted_quality <- 0
  global_tree_quality <- 0 
  
  gbm_result <- 0
  
  for (i in 1:nrow(id_neighbours)){
    neighbours_data <- data.frame()
    true_quality[i] <- data@test_data_with_class[which(data@test_data_with_class$id == query[i,1]),]$quality
    
      for (n in 1:ncol(id_neighbours)){
        id <- id_neighbours[i,n]
        neighbours_data <- rbind(neighbours_data,data@train_data[which(data@train_data$id == id),])
      }
    knn_predicted_quality[i] <- neighbours$test_set_scores[i,1]
  
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
    
    gbm_fit <- gbm(formula = quality ~ ., 
                   distribution = "gaussian", 
                   data = neighbours_data[,2:ncol(neighbours_data)], 
                   n.trees = 300, 
                   interaction.depth = 4, 
                   shrinkage = 0.01)
    
    gbm_result[i] <- round(predict(gbm_fit, query[i,], n.trees = 300))
  }
  
  result <- cbind(true_quality,knn_predicted_quality, svm_predicted_quality, global_svm_quality, tree_predicted_quality, global_tree_quality, gbm_result) 
  knn_effectiveness[siema] <- nrow(result[which(result[,1] == result[,2]),])
  
  svm_effectiveness[siema] <- nrow(result[which(result[,1] == result[,3]),])
  global_svm_effectiveness[siema] <- nrow(result[which(result[,1] == result[,4]),])
  
  tree_effectiveness[siema] <- nrow(result[which(result[,1] == result[,5]),])
  global_tree_effectiveness[siema] <- nrow(result[which(result[,1] == result[,6]),])
  
  gbm_effectiveness[siema] <- nrow(result[which(result[,1] == result[,7]),])
}
message("KNN")
message(sum(knn_effectiveness))

message("SVM")
message(sum(svm_effectiveness))
message("SVM global")
message(sum(global_svm_effectiveness))

message("Tree")
message(sum(tree_effectiveness))

message("Tree global")
message(sum(global_tree_effectiveness))