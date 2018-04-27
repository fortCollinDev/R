###
#Dan Carver 7/27/17
#this function runs a seaies of paramaterization on a regression based random forest model. 
#The output is a printed value for the ntree and mtry values that allowed for the 
#Two inputs are required
#1. dataset
#2. response_var
#The dataset should be a dataframe that contains the response variables and predictor variables
#The response variable is a string of the column name for the response variable within the dataset
#Below is an example code of how to prep you data to meet these specifications
###
# data <- read.csv("~/Dropbox/TamData_withabs_16all_ext.csv")
# predictors <- c('X0815_slavi','X0915_blue', 'X0415_gndvi',  'X0815_swir1',  'X1015_red')
# df.2 <- data[, which(names(data) %in%predictors)]
# dataset <- cbind(response_var = data$Percent_Cov, df.2)
###
#This function will run randomForest a total of 95 times. It should only be used when a small set of
#predictor variables have been identified. 

RF_paramitization <- function(dataset, response_var) {
  #defines a dataframe that will be populated with results from subsequent for loop
  dfTrees = data.frame(matrix(vector(), 10, 4,
                          dimnames=list(c(), c("trees", "VarE", "MSqR", "ratio"))),
                   stringsAsFactors=F)
  #set counter
  j=1
  #define the number of times the test should run
  for (i in c(1,2,3,4,5,6,7,8,9,10)){
    l1start <- Sys.time()
    #create an empty vectors to store results  
    VarE <- rep(NA, 7)
    MSqR <- rep(NA, 7)
    #defines the number of trees that the model will use 
    trees <- c(500,1000,2000,3000,4000,5000,10000)
    #set a second counter
    n=1
    for (tree in trees){
      #runs the randomforest model 
      rf_modelBI = randomForest(response_var ~ .,
                                data = dataset, importance = TRUE, ntree = tree)
      #calculates the percent variance explained
      VarE[n] <- rf_modelBI$rsq[tree]*100
      #define the mean root squared error
      MSqR[n] <- rf_modelBI$mse[tree]
      n=n+1
    }
    #creates a dataframe of for loop outputs 
    var_trees <- data.frame(trees, VarE, MSqR)
    # create a new column that produces a ratio of the two assessment values 
    var_trees$ratio <- (var_trees[2]/var_trees[3])
    #order the df based on ratio value 
    var_treeOrder <- var_trees[rev(order(var_trees$ratio)),]
    #remove row names from the df
    rownames(var_treeOrder) <- NULL
    #add a row to the original df storing the highest ranked ratio value
    dfTrees[j,] <- var_treeOrder[1,]
    l1end <- Sys.time() - l1start
    print(paste("The", j ,"th iteration of the parameter testing for Ntrees was completed in ",l1end," seconds."))
    j = j+1
  }
  print(dfTrees)
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
    ratio_order <- mat1[order(mat1$ratio, decreasing=TRUE), ]
    tree_value <- ratio_order$trees[1]
  }
  
  #create a new df for the mtry parameterization
  dfMtry = data.frame(matrix(vector(), 10, 4,
                             dimnames=list(c(), c("mtry", "VarE", "MSqR", "ratio"))),
                      stringsAsFactors=F)
  #set counter 
  j=1
  #define the number of iterations to perform 
  for (i in c(1,2,3,4,5,6,7,8,9,10)){
    l1start <- Sys.time()
    #create an empty vector 
    VarEM <- rep(NA, 5)
    MSqRM <- rep(NA, 5)
    #run the rf model 
    mtry <- c(1,2,3,4,5)
    n=1
    ntree = tree_value
    for (mtry1 in mtry){
      rf_modelmtry = randomForest(response_var ~ .,
                                  data = dataset, importance = TRUE, ntree = ntree, mtry = mtry1)
      VarEM[n] <- rf_modelmtry$rsq[ntree]*100
      MSqRM[n] <- rf_modelmtry$mse[ntree]
      n=n+1
    }
    var_mtry <- data.frame(mtry, VarEM, MSqRM)
    var_mtry$ratio <- (var_mtry[2]/var_mtry[3])
    var_mtry_order <- var_mtry[order(var_mtry$ratio, decreasing=TRUE), ]
    rownames(var_mtry_order) <- NULL
    dfMtry[j,] <- var_mtry_order[1,]
    l1end <-  Sys.time() - l1start 
    print(paste("The ", j ,"th iteration of the parameter test for Mtry was completed in ", l1end, " seconds."))
    j <- j+1
  }
  print(dfMtry)
  
  freq <- count(dfMtry, 'mtry')
  freq_order <- freq[order(freq$freq, decreasing=TRUE), ]
  mtry_value <- freq_order$mtry[1]
  
  
  freq <- count(dfMtry, 'mtry')
  freq_order <- freq[order(freq$freq, decreasing=TRUE), ]
  if (freq_order$freq[1] > freq_order$freq[2]){
    mtry_value <- freq_order$mtry[1]
  } else{
    values <- freq_order$mtry[freq_order$freq == freq_order$freq[1]]
    mt1 <- dfMtry[dfMtry$mtry %in% values, ]
    mt1 <- as.data.frame(lapply(mt1, unlist))
    ratio_order <- mt1[order(mt1$ratio, decreasing=TRUE), ]
    mtry_value <- ratio_order$mtry[1]
  }
  
  
  finalPrint <- print( paste("Parameter testing has shown that the ideal ntree value is ",tree_value, 
                             "and the ideal mtry value is ",mtry_value, " for this given set of predictor variables. " ))
  
  
  
  rf_modeladjusted = randomForest(response_var ~ . , data = dataset, 
                                  importance = TRUE, ntree = tree_value, mtry = mtry_value)
  print(dfTrees)
  print(dfMtry)
  print(finalPrint)
  return(rf_modeladjusted)
}




