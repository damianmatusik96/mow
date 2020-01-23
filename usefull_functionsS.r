prepare_data <- function(x){
  #normalizuje
  xx <- normalize(x[,1:ncol(x)-1], method = 'range', range= c(0,1), on.constant = 'quiet')
  x <- cbind(xx,x[,ncol(x)])
  names(x)[ncol(x)]<-paste("quality")
  # Dodaje id
  id <- c(1:nrow(x))
  x <- cbind(id, x)
  
  # Dzielimy zbior na 75% - 25%
  smp_size <- floor(0.75 * nrow(x))
  train_index <- sample(seq_len(nrow(x)), size = smp_size)
  train_data <- x[train_index, ]
  test_data_with_class <- x[-train_index, ]
  
  # Usuwam klase
  test_data <- test_data_with_class[,1:(ncol(x) - 1)]
  

  return(new("DataObject",
             train_data=train_data, 
             test_data=test_data, 
             test_data_with_class=test_data_with_class))
}

random_rows <- function(df, n){
  return(df[sample(nrow(df), n),])
}

get_neighbours <- function(data,
                          queries,
                          amount_of_neighbours,
                          measure="euclidean") {
  
  query <- queries
  
  neighbours <- knn(data@train_data, 
                    query[,2:(ncol(query))], 
                    categorical_target = "quality", 
                    k = amount_of_neighbours, 
                    comparison_measure=measure,
                    return_ranked_neighbors = amount_of_neighbours, 
                    id = 'id')
  return(neighbours)
}

# Rysowanie drzewka

draw_tree <- function(fit) {
  fancyRpartPlot(fit, sub = NULL)
  printcp(fit) # display the results
  plotcp(fit) # visualize cross-validation results
  summary(fit) # detailed summary of splits
  # plot tree
  plot(fit, uniform=TRUE,
     main="Classification Tree for Kyphosis")
  text(fit, use.n=TRUE, all=TRUE, cex=.8)
}
#
# predict_quality <- function(train_data, 
#                             query, 
#                             amount_of_neighbours, 
#                             measure="euclidean")


get_result_based_on_weights <- function(classes, weights) {
  a <- sample(classes, size = 1, replace = TRUE, prob = weights)
  return(a)
}
# 
# 
# result1 <- get_result_based_on_weights(c(result)

svm_predict <- function(data, query) {
  svm_model <- svm(formula = quality ~ ., data = data, type = 'C', kernel = "linear")
  pred <- predict(svm_model,query)
  result <- as.numeric(levels(pred))[pred]
  return(result)
}

tree_predict <- function(data,query) {
    fit <- rpart(quality~.,
                 method="class", data=data, control=rpart.control(minsplit=5))

    result <- predict(fit, query, type="prob")
    result <- as.numeric(colnames(result)[apply(result,1,which.max)])
    return(result)
}