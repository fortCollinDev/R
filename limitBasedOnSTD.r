###
#
# calculate some basic statistical values of from the presense and absence points
# determine a range of acceptable values for the projection of values from
# sampled points for alternate years
#
#
#
# 1st input csv: Sample
# the outputs of a variable selection process
#
# 2nd input CSV: predict
# outputs of an extract values from a time period that we do not have sampling points for
#
# dan Carver
# 3/19/2018
###

#stops R from using scientific notation
options(scipen = 999)

#define the function that will calculated basic statistical values
lowHigh <- function(x) {
  #this function takes in a dataframe. Calculates mean, std, mean-std, and mean+std.
  # Then returns a df with the calculated values
  mean <- sapply(x, mean)
  std <- sapply(x, sd)
  df = data.frame(cbind(mean, std))
  df['low'] <- as.numeric(df$mean-df$std)
  df['high'] <- as.numeric(df$mean+df$std)
  return(df)
}

lowHighLS5 <-function(x){
  #this function takes in a dataframe. Calculates mean, std, mean-std, and mean+std.
  # Then returns a df with the calculated values with an applied ratio to match the bit range of LS8 to LS5
  # (2^8 / 2^12)
  names <- colnames(x)
  bands <- matrix(ncol = 4, nrow = 0)
  colnames(bands) <- c("mean","std","lowLs5","highLs5")
  for (j in seq(length(names))){
    print(j)
    i=names[j]
    n <- readline(prompt = paste('Is the index', i , ' a single band? Please enter 1 for yes or 0 for no: '))
    if (n == 1){
      mean <- sapply(x[j], mean)*0.0625
      std <- sapply(x[j], sd)*0.0625
      df = data.frame(cbind(mean, std))
      df['lowLs5'] <- as.numeric(df$mean-df$std)
      df['highLs5'] <- as.numeric(df$mean+df$std)
    }else{
      mean <- sapply(x[j], mean)
      std <- sapply(x[j], sd)
      df = data.frame(cbind(mean, std))
      df['lowLs5'] <- as.numeric(df$mean-df$std)
      df['highLs5'] <- as.numeric(df$mean+df$std)
    }
    bands <- rbind(data.frame(bands),df)
    j=j+1
  }
  return(bands)
}

#input next round of data from a year that was sampled
dataSam <- read.csv("C:\\Users\\nreluser\\Downloads\\LS_allpredictors_2015_1.csv")
namesSam <- names(dataSam)
head(dataSam)
dataSam$system.index <-NULL
dataSam$.geo <- NULL
na.omit(dataSam)

#deal with the 'P' and 'A'
#this will change based on the input data set, use the head print out to test this
dataSam$PA <- ifelse(dataSam$PA15 == 'A', 0,1)
dataSam$PA15 <- NULL

# subset the data based on presence and absence
presenceSam <- subset(dataSam, PA== 1)
absenceSam <- subset(dataSam, PA== 0)

####
# that is all we do with the first dataset for the time being We will come back to it in a few
###

#import data from the year where the model is being projected
dataNew <- read.csv("C:\\Users\\nreluser\\Downloads\\All_2005_selectedpredictors_CC_0.75_noT1.csv")

namesNew <- names(dataNew)
namesNew
###
#Unique to this specific sample; Set values to match the 2015 colNames
colnames(dataNew) <-c("X","response_var","TCG_15_T2", "swir1_15_T3" )
#subset based on response variable
absence <- subset(dataNew, response_var==1)
presence <- subset(dataNew, response_var==0)
###


###Stand Method
#The removable of these varibles will be unique to the give dataset
# you need to get ride of these cols order for the basic stat functions to run
dataNew$system.index <-NULL
dataNew$.geo <- NULL
na.omit(dataNew)

#create a new column with 0 and 1 in liue of A and P
dataNew$PA <- ifelse(data$response_var == 'A', 0,1)
dataNew$PAdataNew
# remove the character column
dataNew$PA <- dataNew$response_var
dataNew$response_var <- NULL

#subset based on presence or absence
absence <- subset(dataNew, PA==0)
presence <- subset(dataNew, PA==1)
###


####
# These names many not match across sensors so we may have to do some more user inputs to make this subset
# most of the trickiness is making sure that these columns match
####

# Select the columns from your prediction dataset that match the column names from your sampled set
predictorsP <- presence[which(colnames(presence) %in% namesSam)]
predictorsA <- absence[which(colnames(absence) %in% namesSam)]

#check print to ensure the selection work the number of cols in predictorsP and predictorsA should be <= ncol(namesSam)
namesSam
head(predictorsP)
head(predictorsA)

#Check to make the order of the columns matchs
namesSam <- namesSam[which( namesSam %in% names(predictorsP))]

# subset the data based on presence and absence
presenceSam <- subset(dataSam, PA== 1)
presenceSam <- presenceSam[,namesSam]
absenceSam <- subset(dataSam, PA== 0)
absenceSam <- absenceSam[,namesSam]
# Alternative due to the

#apply the function to the data for Landsat 8 images
absenceRange <- lowHigh(absenceSam)
presenceRange <- lowHigh(presenceSam)

#apply the function for landsat 5 images
absenceRangeLS5 <- lowHighLS5(absenceSam)
presenceRangeLS5 <- lowHighLS5(presenceSam)


#find
totalNumP <- seq(1,ncol(predictorsP))
outputP <- matrix(nrow=nrow(predictorsP) ,ncol=ncol(predictorsP))
row.names(outputP) <- row.names(predictorsP)
for (pred in totalNumP){
  print(pred)
  low <- presenceRange[pred,3]
  high <- presenceRange[pred,4]
  outputP[,pred] <- predictorsP[,namesSam[pred]] > low & predictorsP[,namesSam[pred]] < high
}

presenceEval <- cbind(predictorsP, data.frame(outputP))
presenceEval$sum <- apply(presenceEval[,c("X1","X2")], 1, sum)
head(presenceEval)
table(presenceEval$sum)


#repeat the process for absence
totalNumA <- seq(1,ncol(predictorsA))
outputA <- matrix(nrow=nrow(predictorsA) ,ncol=ncol(predictorsA))
row.names(outputA) <- row.names(predictorsA)
for (pred in totalNumA){
  print(pred)
  low <- absenceRange[pred,3]
  high <- absenceRange[pred,4]
  outputA[,pred] <- predictorsA[,namesSam[pred]] > low & predictorsA[,namesSam[pred]] < high
}

absenceEval <- cbind(predictorsA, data.frame(outputA))
absenceEval$sum <- apply(absenceEval[,c("X1","X2")], 1, sum)
head(absenceEval)
table(absenceEval$sum)



#add the dataframes for presence and absence together
combinedDF <-rbind(absenceEval, presenceEval)
combinedDF <- combinedDF[order(as.numeric(row.names(combinedDF))),]

#import dataset for geographic data

dataGeo <- read.csv("C:\\Users\\nreluser\\Downloads\\all_predictors_2005.csv")
head(dataGeo)

valuesGeo <- cbind(combinedDF, dataGeo[,c('x_coord','y_coord')])
valuesGeo <- subset(valuesGeo, sum >= 1)
colnames(valuesGeo) <- c('TCG_05_T2', 'swir1_05_T3',    'X1',    'X2', 'sum',   'x_coord',  'y_coord')
head(valuesGeo)

#write out the csv
write.csv(valuesGeo, file = "C:\\Users\\nreluser\\Downloads\\backcast05SingleScene.csv")
