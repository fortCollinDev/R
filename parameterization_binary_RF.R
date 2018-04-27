###
#Dan Carver 7/27/17
#this function runs a seaies of paramaterization on a binary random forest model. 
#The output is a printed value for the ntree and mtry values that allowed for the 
#Two inputs are required
#1. dataset
#2. response_var
#The dataset should be a dataframe that contains the response variables and predictor variables
#The response variable is a string of the column name for the response variable within the dataset
#Below is an example code of how to prep you data to meet these specifications
###
#data <- read.csv('path_to_csv.csv')
#predictors <- c('X0405_blue',	'X0806_Tbrig',	'X0705_blue',	'X0405_mndwi',	'X1006_gemi',	'X0506_mndwi',	'X0705_ctvi')
#response_variable <- tam_pa
#df.2 <- data[, which(names(data) %in%predictors)]
#dataset <- cbind(response_var = data$tam_pa, df.2)
#response_var <- data$tam_pa
###
#This function will run randomForest a total of 95 times. It should only be used when a small set of
#predictor variables have been identified. 


RF_paramitization <- function(dataset, response_var) {
#defines a dataframe that will be populated with results from subsequent for loop
dfTrees = data.frame(matrix(vector(), 10, 5,
                              dimnames=list(c(), c("trees", "auc", "PCC", "TS", "sum"))),
                       stringsAsFactors=F)
#set counter
j=1
set.seed(123)

datasetP <- subset(dataset, select = -response_var)

#define the number of times the test should run
for (i in seq(1, 10, by=1)){
  l1start <- Sys.time()
  auc <- rep(NA, 7)
  PCC <- rep(NA, 7)
  TS <- rep(NA, 7)
  #run the rf model 
  trees <- c(500,1000,2000,3000,4000,5000,10000)
  n=1
  for (tree in trees){
    rf_modelBI = randomForest(x = datasetP, y = dataset$response_var, importance = TRUE, ntree = tree, mtry = 3)
    predicted <- predict(rf_modelBI)
    observed <- dataset$response_var
    aos <- accuracy(x=predicted, y=observed)
    auc[n] <- aos$auc
    PCC[n] <- aos$PCC
    TS[n] <- aos$true.skill
    n=n+1
  }
  var_trees <- data.frame(trees, auc, PCC,TS)
  # create a new column that produces a sum of the three assessment values 
  var_trees$sum <- rowSums(var_trees[2:4])
  #order the df based on ratio value 
  var_treeOrder <- var_trees[rev(order(var_trees$sum)),]
  #remove row names from the df
  rownames(var_treeOrder) <- NULL
  #add a row to the original df storing the highest ranked ratio value
  dfTrees[j,] <- var_treeOrder[1,]
  l1end <- Sys.time() - l1start
  print(paste("The", j ,"th iteration of the parameter testing for Ntrees was completed in ",l1end," seconds."))
  j = j+1
  }
  #Create a frequency chart of the number of times a tree was selected 
  freq <- count(dfTrees, 'trees')
  #rank the frequency chart in decreasing order
  freq_order <- freq[order(freq$freq, decreasing=TRUE), ]
  #if else statement ensuring that the more common or highest ranked value is selected. 
  if (freq_order$freq[1] > freq_order$freq[2]){
    tree_value <- freq_order$trees[1]
  } else{
    values <- freq_order$trees[freq_order$freq == freq_order$freq[1]]
    mat1 <- dfTrees[dfTrees$trees %in% values, ]
    mat1 <- as.data.frame(lapply(mat1, unlist))
    ratio_order <- mat1[order(mat1$sum, decreasing=TRUE), ]
    tree_value <- ratio_order$trees[1]
  }
print(dfTrees)
print(tree_value)



dfMtry = data.frame(matrix(vector(), 5, 5,
                            dimnames=list(c(), c("mtry", "auc", "PCC", "TS", "sum"))),
                     stringsAsFactors=F)
j =1 
for (i in seq(1, 5, by=1)){
  #create an empty vector 
  aucM <- rep(NA, 5)
  PCCM <- rep(NA, 5)
  TSM <- rep(NA, 5)
  
  #run the rf model 
  myt <- c(1,2,3,4,5)
  n=1
    for (mtry in myt){
      rf_modelBI = randomForest(x = datasetP, y = dataset$response_var, importance = TRUE, ntree = tree_value, mtry = mtry)
      predicted <- predict(rf_modelBI)
      observed <- data$tam_pa
      aos <- accuracy(x=predicted, y=observed)
      aucM[n] <- aos$auc
      PCCM[n] <- aos$PCC
      TSM[n] <- aos$true.skill
      n=n+1
      print(paste("The model for the ", mtry, "is complete"))
    }
  
  var_mtry <- data.frame(mtry, aucM, PCCM,TSM)
  # create a new column that produces a sum of the three assessment values 
  var_mtry$sum <- rowSums(var_mtry[2:4])
  #order the df based on ratio value 
  var_mtryOrder <- var_mtry[rev(order(var_mtry$sum)),]
  #remove row names from the df
  rownames(var_mtryOrder) <- NULL
  #add a row to the original df storing the highest ranked ratio value
  dfMtry[j,] <- var_mtryOrder[1,]
  l1end <- Sys.time() - l1start
  print(paste("The", j ,"th iteration of the parameter testing for Ntrees was completed in ",l1end," seconds."))
  j = j+1
  }
  #Create a frequency chart of the number of times a tree was selected 
  freq <- count(dfMtry, 'mtry')
  #rank the frequency chart in decreasing order
  freq_order <- freq[order(freq$freq, decreasing=TRUE), ]
  #if else statement ensuring that the more common or highest ranked value is selected. 
  if (nrow(freq_order) == 1) {
    mtry_value <- freq_order$mtry[1]
    } else if (freq_order$freq[1] > freq_order$freq[2]) {
    mtry_value <- freq_order$mtry[1] 
    }else {
    values <- freq_order$mtry[freq_order$freq == freq_order$freq[1]]
    mat1 <- dfMtry[dfMtry$mtry %in% values, ]
    mat1 <- as.data.frame(lapply(mat1, unlist))
    sum1 <- mat1[order(mat1$sum, decreasing=TRUE), ]
    tree_value <- sum1$mtry[1]
    }
    
print(dfMtry)
print(mtry_value)


rf_model_adjusted = randomForest(x = datasetP, y = dataset$response_var, importance = TRUE, ntree = tree_value, mtry = mtry_value)

predicted <- predict(rf_model_adjusted)
observed <- dataset$response_var
aos <- accuracy(x=predicted, y=observed)

return(aos)
}


