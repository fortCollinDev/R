# Random Forest Example with tamarisk continuous cover data
# Ancillary data from 2005/2006
# adapted from B Woodward
# MLV 06/28/2017


# install package for group installing and loading of following libraries (you
# only need to do this once ever on your computer)
install.packages("pacman")

# installs packages that are not installed yet and loads all packages needed
pacman::p_load(raster, shapefiles, sp, rgdal, randomForest, caret, e1071,
               MASS, ROCR, corrplot, rfUtilities, VSURF,dplyr)

# set seed = RF is an inherently random process (i.e. you will get a slightly
# different result each time you run the algorithm). setting a seed allows you
# to get the exact same results each time. the seed is just a random number that
# you choose-- it doesn't have a meaning.

set.seed(123)

# read in absence CSV file and save as an object
absent <- read.csv("C:\\Users\\nrel\\Downloads\\absence_pt_test1.csv")
# add column for absent
absent$ricePA <- as.factor(0)

#read in presence csv 
presence <- read.csv("C:\\Users\\nrel\\Downloads\\presence_pt_test.csv")
# add column for absent 
presence$ricePA <- as.factor(1)

#combind the two dataframes to one 
paData <- rbind(absent,presence)


#values and make this a new object with percent cover and all raster values
rastercolumns <- paData[,2:38]

#vsurf for
#vSRBCT.laptop <- VSURF.parallel(x = rastercolumns, y = data$tam_pa., nmin = 10)
vsurf1 <- VSURF(x=rastercolumns, y = paData$ricePA)
summary(vsurf1)
plot(vsurf1)

orderOfImprotance <- rastercolumns[,vsurf1$varselect.thres]
names(orderOfImprotance)



#produce a vector containing all the variable information
select_var <- rastercolumns[,vsurf1$varselect.pred]


# calculate correlation coefficient matrix
correlation <-cor(select_var, method="pearson")

# plot the correlation. the darker the number, the more correlated the two
# variables
corrplot(correlation,method="number")

x1 <- readline("Enter the numbers that you would like to remove seperated by commas. eg (4,25,3)")
x1 <- as.numeric(unlist(strsplit(x1, ",")))

select_var1 <- select_var[,-c(x1)]

correlation <-cor(select_var1, method="pearson")
corrplot(correlation,method="number")

x2 <- readline("Enter the numbers that you would like to remove seperated by commas. eg (4,25,3).")
x2 <- as.numeric(unlist(strsplit(x2, ",")))

select_var2 <- select_var1[,-c(x2)]

correlation <-cor(select_var2, method="pearson")
corrplot(correlation,method="number")

x3 <- readline("Enter the numbers that you would like to remove seperated by commas. eg (4,25,3).")
x3 <- as.numeric(unlist(strsplit(x3, ",")))

select_var3 <- select_var2[,-c(x3)]

correlation <-cor(select_var3, method="pearson")
corrplot(correlation,method="number")

#at this point there are 8 variables that are not correlated binded them to the binary indence and put into rf model
selected_binary <- cbind(rice_PA = paData$ricePA,  select_var1)


predictors <- c('X0405_blue',	'X0806_Tbrig',	'X0705_blue',	'X0405_mndwi',	'X1006_gemi',	'X0506_mndwi',	'X0705_ctvi')
df.2 <- data[, which(names(data) %in%predictors)]
dataset <- cbind(response_var = data$tam_pa, df.2)
response_var <- data$tam_pa




rf_modelBI = randomForest(x = select_var1, y = paData$ricePA, importance = TRUE, ntree = 2000, mtry = 3)

predicted <- predict(rf_modelBI)
observed <- paData$ricePA

aos <- accuracy(x=predicted, y=observed)



######Map results#####
# we can do this once we have our rasters ready
B1=raster("G:/MS_Research/Masters_Project/RQ1/Modelling/2011_Final_Model/Predictive_Stack/2011_Parsimony_Final_Updated.tif",1)
B2=raster("G:/MS_Research/Masters_Project/RQ1/Modelling/2011_Final_Model/Predictive_Stack/2011_Parsimony_Final_Updated.tif",2)
B3=raster("G:/MS_Research/Masters_Project/RQ1/Modelling/2011_Final_Model/Predictive_Stack/2011_Parsimony_Final_Updated.tif",3)
B6=raster("G:/MS_Research/Masters_Project/RQ1/Modelling/2011_Final_Model/Predictive_Stack/2011_Parsimony_Final_Updated.tif",4)
B7=raster("G:/MS_Research/Masters_Project/RQ1/Modelling/2011_Final_Model/Predictive_Stack/2011_Parsimony_Final_Updated.tif",5)
B9=raster("G:/MS_Research/Masters_Project/RQ1/Modelling/2011_Final_Model/Predictive_Stack/2011_Parsimony_Final_Updated.tif",6)



# Stack the objects into one dataframe
stack_6band=stack(B1,B2,B3,B6,B7,B9)

# Add header names - this might not be necessary since I add names later
names(stack_6band)=c('B1','B2', 'B3', 'B6', 'B7','B9')

#####################################################map it, BINARY FIRST, then CONTINUOUS###################################
predict(stack_6band, rf_model, filename="2011MortalityFinal_Thesis.tif",fun=predict,format="GTiff",datatype="FLT4S",overwrite=TRUE )
