library(raster)
library(rgdal)
library(sp)
library(maptools)
 
 
#example of a SVM classification using R
#In this case we use Polygons as input of the supervised training areas and we performed a simple "Grid search" for the SVM parameters - other more advanced approaches exist.
#Furthermore, using polygons for supervised classification can produce huge datasets for training, which implies that SVM.Tunning wastes a lot of time.
#So SVM tunning is performed using a sub-sample of the training area, and the SVM model using the whole dataset.
#A new package exists that can potentially make SVM run faster "ParallelSVM" but I have not tried it with raster data
 
#--------------- Data preparation ----------------------------
 
#Loading an input image
my.stack <- stack("PATH2FILE - Should be a multband raster")
names(my.stack.rgb) <- c("Band1","Band2","ETc") #you should rename the bands to a more meaningfull set - R loads them as RasterName.etc
 
#You should already have a shapefile with the training areas.
#Using your GIS software convert that shapefile to a rasterdataset (it's not the only way but it makes life a lot easier in R)
#I recommend converting to an integer type value to avoid conflicts down the line, computers handle numbers better
#This script assumes that the training raster has 1...N values, representing the classes.
 
my.training.raster <- raster("Path2convertedShafile2raster")
my.training.points <- rasterToPoints(my.tr.raster) #this converts the raster to a SpatialPointsDataFrame - That is the shapefile as R understands it
 
names(my.training.points)<-c("X","Y","Value")
 
my.training.points.df <- extract(x=my.stack,
                                 y=my.training.points[,c(1,2)],
                                 sp=TRUE) #this does not produce a real dataframe, it has to be converted after
 
my.training.points.df <- as.data.frame(my.training.points.df)
 
 
#-------------- SVM Tunning and Training --------------------------
#we are set to go into SVM training
#The grid approach works by creating 2 groups of parameters (in the case of radial type SVM, gamma and cost)
#and testing whci is the best combination iteratively.
#Once a set of 2 parameter is found, you can make the grid thinner by using a smaller interval - and repeating the process until satisfied
 
gammat.small <- seq(.1,.9, by = .1)
costt.small <- seq(1,106,by = 11)
length(gammat.small)
length(costt.small)
gammat.small
costt.small
 
#-------------- Optional step
#if your dataset is big (tr.training.points.df > a lot), then you can subsample for the initial tunning, not sure you should do it a lot
#if you are wondering, 10k points is quite fast, > 150k is quite slow for me
 
#a lazy example for subsampling can be the the following
 
my.training.points.df.sample <- my.training.points.df
 
Class01 <- my.training.points.df[my.training.points.df$Value == 1,]
Class02 <- my.training.points.df[my.training.points.df$Value == 2,]
Class03 <- my.training.points.df[my.training.points.df$Value == 3,]
#etc until you have you a group for all classes
#Value is the name selected in Line 27
 
#sampling N points from each group
#Attention - you should not have a sample bigger than the actual number of points in the sub-group
#You can check the minimum number of samples by running: table(my.training.points.df.sample$Value)
row.id <- sample(1:nrow(class01),size=N)
Class01.sample <- class01[row.id,]
 
row.id <- sample(1:nrow(class02),size=N)
Class02.sample <- class02[row.id,]
#etc
 
#once you are finished, you can recreate a dataframe with all - i will overwrite the previous DF
my.training.points.df.sample <- rbind(Class01.sample,
                                      Class02.sample,
                                      etc)
 
#we are ready for tunning our SVM
 
#----- SVM tunning ------------
 
#If you used the raw dataset
svm.tune.all <- tune.svm(my.training.points.df,
                         as.factor(my.training.points.df$Value),
                         gamma=gammat.small,
                         cost=costt.small)
 
#if you opted to make a smaller sample
svm.tune.sample <- tune.svm(my.training.points.df.sample,
                            as.factor(my.training.points.df.sample$Value),
                            gamma=gammat.small,
                            cost=costt.small)
 
#you can check the estimated best value for the gamma and cost parameters
svm.tune.all
svm.tune.sample
 
#You can repeat this process by choosing a new set of gammat.small and costt.small values - line 42 -
#or you can just script a loop, it should be easy to adapt.
 
#---- SVM Training ----
 
#it is very important that you mantain consistent names between all the data.
#For tuning, it is not as important to pay attention to this but for the actual SVM model it is very important
#if the names are wrong, R, won't be able to produce the final raster
 
#here you can probably use the new package "ParallelSVM", I didn't try it for rasters yet.
my.svm <- svm(formula=Value~., #again, value was the name I chose.
              data=my.training.points.df, #Notice i am using all the data for the SVM training -not the subsample
              type="C-classification", #Type of output, since you are using integers, clearly state to SVM that you want a classification
              kernel="radial", #Type of SVM model
              cost=selected.cost, #Selected parameters which resulted from your tunning procedure
              gamma=selected.gamma)
 
#In R, when you use the formula as Value~. it means: Value = FunctionOf(All covariates in the dataset)
#If, during tunning you removed covariates (in our case raster bands), then you can write Value= Selected band 1 + Selected band 2 + ... Selected band N
#Alternatively, you can remove the bands you are not interested in from your data.frame my.training.points and keep the formula as Value~.
#When the SVM is trainned using a subset of covariates, it is not necessary to remove them from your multiband raster
#since the model will only pick the covariates used in the training
 
 
#--- Predicting to your raster set
 
raster.prediction <- predict(model=my.svm,
                             object=my.stack,
                             filename="PATH2OUTPUT") #remove if you do not want to save your raster to the disc
#if you generally use tif, which I recommend to do so, you ca add: options=c("COMPRESS=LZW")
 
#Classification is done
 
#--------- Accuraccy assessment (using Caret package)
 
#load your point shapefile which has the validation
Validation.points <- readOGR("Path2shapefile") #it MUST be in the same projection as the rasters - R does not perform on-the-fly reprojections like GIS softwares
 
Validation.points.extract <- extract(x=raster.prediction,
                                     y=validation.points,
                                     sp=TRUE)
 
#it's always good go rename everything to make it easier to understand
#in my case, I'm using a shapefile which has:
#ID (object id from GIS)
#Object (class name or description)
#Reference (Integer number for the class)
#ModelSVM (name of the layer extracted from the raster.prediction)
names(Validation.points.extract) <- c("ID","Object","Reference","ModelSVM")
 
#creating a confusion matrix
Validation.points.extract.df <- as.data.frame(Validation.points.extract)
 
#Verify if all classes exist in both datasets or this will fail
unique(Validation.points.extract.df$Reference)
unique(Validation.points.extract.df$ModelSVM)
 
#confusion matrix:
confusionMatrix(data=Validation.points.extract.df$ModelSVM,
                reference=Validation.points.extract.df$Reference)
 
#Voilá!
 
#You can dump your results to a text file like this:
sink("PATH2FILE.Txt")
print(confusionMatrix(data=Validation.points.extract.df$ModelSVM,
                      reference=Validation.points.extract.df$Reference))
sink() #ALWAYS BE SURE TO CLOSE THE SINK IF THE SINK WAS STARTED
