# load libraries
#p<-c("factoextra","fpc","NbClust","dplyr","tidyverse","cluster","dendextend","mlbench","caret")
#install.packages("p")
library(fscaret)
library(MASS)
library(dplyr)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)
library(fpc)
library(NbClust)
library(dendextend) #for dendogram
library(mlbench)
library(caret)


set.seed(12345)

MyData <- read.csv(file="path of the file",
                   header=TRUE, sep=";", dec = ".", na.strings = c("-","??","","Unknown","NA"))
dim(MyData)

summary(MyData)

#one row was starting from one cell later I need to remove that 
MyData<-MyData[is.na(MyData$X),]
##because of that row numeric colums has been readed as factors
MyData$pop<-as.numeric(MyData$pop)
MyData$latitude<-as.numeric(MyData$latitude)
MyData$day<-as.numeric(MyData$day)
str(MyData)


####replace wrong values with NaN in day attribute
day<-function(x,na.rm=TRUE,...){
  if(x > 31 && !is.na(x)) x<-NaN else x
}
testdata<-MyData$day
testdata<-as.vector(testdata)
MyData$day<-sapply(testdata,day)
summary(MyData)

## Remove columns with more than 20% NA
cleanedData<-MyData[, -which(colMeans(is.na(MyData)) > 0.2)]
str(cleanedData)

##Remove name it's not important
cleanedData<-cleanedData[,-c(1,1)]
str(cleanedData)

#It will apply the is.factor() function to each component (column) of the data frame.
#is.factor() checks if the supplied vector is a factor as far as R is concerned.
f <- sapply(cleanedData, is.factor)


#---numeric attributes of the data-----
num<-which(!f,arr.ind = TRUE)
#remove the names from num to have only numeric values's index 
unname(num)

#---factor attributes of the data------
fac<-which(f,arr.ind = TRUE)
#remove the names from fac to have only values's index 
unname(fac)


#-Mean value inserted into remaining NAs on numeric attributes--------
for(i in 1:length(num)) {
  
  cleanedData[is.na(cleanedData[,num[i]]),num[i]]<- mean(cleanedData[,num[i]], na.rm = TRUE)
  
 
}
#---------------------------------------------------------------------

#------Mode value inserted into remaining NAs on factor attributes-----
#-function the find mode value of the data-----------
Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}
#----------------------------------------------------
for(i in 1:length(fac)) {
  
  cleanedData[is.na(cleanedData[,fac[i]]),fac[i]]<- Mode(cleanedData[,fac[i]], na.rm = TRUE )
  
}
#----------------------------------------------------------------------
summary(cleanedData) #NA values replaced with mean and mode values
#----------Function to remove outliers based on Interqartile Rage
remove_outliers<-function(x,na.rm=TRUE,...){
  #find position of 1st and 3rd quartile not including NA's
  qnt<-quantile(x,probs=c(.25,.75),na.rm = na.rm,...)
  H<- 1.5*IQR(x,na.rm = na.rm)
  
  temp<-x
  temp[x<(qnt[1]-H)]<-NA
  temp[x>(qnt[2]+H)]<-NA
  x<-temp
}
#----------------------------------------------------------------
#summary(cleanedData)
par(mfrow=c(3,2))
#--Apply function to remove outliers from numeric data-----------------
for(i in 1:length(num)) {
  testdata<-cleanedData[,num[i]]
  testdata<-as.vector(testdata)
  cleanedData[,num[i]]<-remove_outliers(testdata)
  hist(cleanedData[,num[i]], main=names(cleanedData)[num[i]])
}
#---Remove outliers from factor data-----------------------------------
cleanedData<-droplevels(cleanedData)
#----------------------------------------------------------------------
#-omit NAs from data---------------------------------------------------
cleanedData<-na.omit(cleanedData) #dropedlevels still showing if I don't do this
#----------------------------------------------------------------------
summary(cleanedData)

#--I saw that year attribute only have 2015 as value which means it's not important attribute for us 
cleanedData<-cleanedData[,-5]
str(cleanedData)
summary(cleanedData)
#-------------------------------------------------------------------------------------------------
#plots for factor datas--------------------------------------------------------------------------------------------
par(mfrow=c(2,2))
barplot(summary(cleanedData$cause), las=2,col=rainbow(length(levels(cleanedData$cause))), cex.names=.8)
barplot(summary(cleanedData$gender), las=2,col=rainbow(length(levels(cleanedData$gender))), cex.names=.8)
barplot(summary(cleanedData$month), las=2,col=rainbow(length(levels(cleanedData$month))), cex.names=.8)
barplot(summary(cleanedData$city), las=2,col=rainbow(length(levels(cleanedData$city))), cex.names=.8)
barplot(summary(cleanedData$raceethnicity), las=2,col=rainbow(length(levels(cleanedData$raceethnicity))), cex.names=.8)
barplot(summary(cleanedData$lawenforcementagency), las=2,col=rainbow(length(levels(cleanedData$lawenforcementagency))), cex.names=.8)
barplot(summary(cleanedData$armed), las=2,col=rainbow(length(levels(cleanedData$armed))), cex.names=.8)
barplot(summary(cleanedData$namelsad), las=2,col=rainbow(length(levels(cleanedData$namelsad))), cex.names=.8)
barplot(summary(cleanedData$state), las=2,col=rainbow(length(levels(cleanedData$state))), cex.names=.8)

#-------------------------------------------------------------------------------------------------------------------

#---Function to change factor datas to numeric-------------------
numerify <- function(x) if(is.factor(x)) as.numeric(x) else x
#---------------------------------------------------------------
#--------Min-Max normalization technique------------------------
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#---------------------------------------------------------------
#----------Normalization Data------------------------------------------------------------
#We are normalizing the data because when some values are very high, they dominate result.
ScaledData<-cleanedData
ScaledData[] <- lapply(ScaledData, numerify)
ScaledData<- as.data.frame(lapply(ScaledData, normalize))
#--omit NAs from data-------------
ScaledData<-na.omit(ScaledData)
#---------------------------------
str(ScaledData)
#----------------------------------------------------------------------------------------
##-FEATURE SELECTION-----------------------------------------------------------------------------
#---correlation plot----------------------------------
library(corrplot)
#install.packages("corrplot")
par(mfrow=c(1,1))
corrmatrix <- cor(ScaledData)
corrplot(corrmatrix, method = 'circle',bg = "black")
#-----------------------------------------------------
#--calculate correlation matrix----------------------------
correlationMatrix <- cor(ScaledData[,1:ncol(ScaledData)])
#--summarize the correlation matrix------------------------
print(correlationMatrix)
#--find attributes that are highly corrected (ideally >0.75)---------
#cutoff: A numeric value for the pair-wise absolute correlation cutoff
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes----------------------
print(highlyCorrelated)
#--------------------------------------------------------------------
#---highlyCorrelated attributes plot---------------------------------
corrmatrix <- cor(ScaledData[,c(highlyCorrelated)])
corrplot(corrmatrix, method = 'circle',bg = "black")
#--------------------------------------------------------------------




#--highlyCorrelated attributes not good for calculating clusters-----
#--because they are rising and falling together----------------------
#--so I removed all highlyCorrelated attributes--
names(ScaledData)
Data_to_cluster<-ScaledData[,-c(highlyCorrelated)]
str(Data_to_cluster)
#------------------------------------------------------------------------------


##Determine number of clusters###################################################
library(factoextra)
#--kmeans---------------------------------------
fviz_nbclust(Data_to_cluster,kmeans, method = c( "wss"))+
labs(title= "K-means") 
#--------------------------------------------------------------------------------

#--hierarchical clustering----------------------
fviz_nbclust(Data_to_cluster,hcut, method = "wss")+
labs(title= "hierarchical") 
#--------------------------------------------------------------------------------

#################################################################################

#-Compute clValid----------------------------------------------------------------
library(clValid)
clmethods <- c("kmeans","pam","hierarchical")
intern <- clValid(Data_to_cluster, nClust = 2:9, 
                  clMethods = clmethods, validation = "internal")
#-Summary-------
summary(intern)
#optimalScores(intern)
#--------------------------------------------------------------------------------
op <- par(no.readonly=TRUE)
par(mfrow=c(2,2),mar=c(4,4,3,1))
plot(intern, legend=FALSE)
plot(nClusters(intern),measures(intern,"Dunn")[,,1],type="n",axes=F,
         xlab="",ylab="")
legend("center", clusterMethods(intern), col=1:9, lty=1:9, pch=paste(1:9))
par(op)


#Compute and visualize k-means clustering
#********************************************************************************
#set.seed(123)
#km.res <- kmeans(Data_to_cluster, 2, nstart = 25)
##Visualize
#library("factoextra")
#fviz_cluster(km.res, data = Data_to_cluster,
#             ellipse.type = "convex",
#             palette = "jco",
#             ggtheme = theme_minimal())
#********************************************************************************


# Compute hierarchical clustering
#******************************************************************************************
res.hc <- Data_to_cluster %>%
  scale() %>%                    # Scale the data
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2")     # Compute hierachical clustering with ward method

# Visualize using factoextra
# Cut in 2 groups and color by groups
fviz_dend(res.hc, k = 2, # Cut in two groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
          )
cutree(res.hc,2)
#******************************************************************************************
###############################CLASSIFICATION##############################################

classData<-cleanedData
names(classData)

###take class name to last column
dput(names(classData))
classData<-classData[c("age","gender","raceethnicity","month",               
"city","state","latitude","longitude",           
"state_fp","county_fp","tract_ce","geo_id",              
"county_id","namelsad","lawenforcementagency","cause",               
"pop","share_white","share_black",         
"share_hispanic","p_income","h_income","county_income",       
"comp_income","county_bucket","nat_bucket","pov",                 
"urate","college" , "armed")]           

head(classData)



levels(classData$armed)<-c("Disputed","Firearm","Knife","No","Nonlethalfirearm","Other","Vehicle")  
for(i in 1:(ncol(classData)-1)){
  #we need armed datas as factors so using (ncol(classData)-1)
    classData[i] <- lapply(classData[i], numerify)
    classData[i] <- lapply(classData[i], normalize)
  
}
table(classData$armed)
#we have only one variable for class disputed if we don't eliminate it to not damage our model
classData<-classData[!(classData$armed == "Disputed"),]
classData<-droplevels(classData)
table(classData$armed)


#it needs training and test dataset so we need to split our dataset
# p is partition %70 of the data
set.seed(12345)
SplitIndex<-createDataPartition(classData$armed,p=.70,list=FALSE)
trainSet<-classData[SplitIndex, ]
testSet<-classData[-SplitIndex,]


# Loading library
library('randomForest')

# Using random forest for variable selection
rfModel <-randomForest(armed ~ ., data = trainSet)
# Getting the list of important variables
importance(rfModel)
#plot imortant variables
varImpPlot(rfModel, sort=TRUE, n.var=min(30, nrow(rfModel$importance)),
           type=NULL, class=NULL, scale=TRUE,
           main=deparse(substitute(rfModel)))
names(classData)

#I took only most important 5 features
new_Idx<-c(17,29,8,5,20,30)
classData<-classData[,new_Idx]

set.seed(17)
# Stratified sampling
TrainingDataIndex <- createDataPartition(classData$armed, p=0.75, list = FALSE)
# Create Training Data 
trainingData <- classData[TrainingDataIndex,]
testData <- classData[-TrainingDataIndex,]
#cross validation with group number 10
TrainingParameters <- trainControl(method = "repeatedcv", number = 10, repeats=10)


# train model with neural networks
NNModel <- train(trainingData[,-6], trainingData$armed,
                 method = "nnet",
                 trControl= TrainingParameters,
                 preProcess=c("scale","center"),
                 na.action = na.omit
)

NNPredictions <-predict(NNModel, testData)
# Create confusion matrix
cmNN <-confusionMatrix(NNPredictions, testData$armed)
print(cmNN)


#write clusters to csv
cleanedData$clusters <- cutree(res.hc,2)
View(cleanedData)
#write to csv file 
write.csv(cleanedData, "path of the file", row.names=F)









