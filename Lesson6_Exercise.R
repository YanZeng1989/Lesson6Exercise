###########Lesson 6 Exercise of Yan Zeng
library(rasta)
library(raster)
library(rgdal)
library(sp)
library(ggplot2)
library(rasterVis)
###load data

data(lulcGewata)
data(LUTGewata)
data(vcfGewata)
data(GewataB2)
data(GewataB3)
data(GewataB4)
#assign Na when value is higher than 100 this is the cloud
vcfGewata[vcfGewata > 100] <- NA
plot(vcfGewata)
cols <- c("orange", "light green", "brown", "light pink", "dark green", "light blue")
plot(lulcGewata, col=cols, legend=FALSE)
legend("topright", legend=LUTGewata$Class, fill=cols)
plot(lulcGewata, col=cols, legend=FALSE)
##brick GewataB2, GewataB3, GewataB4
gewata <- brick(GewataB2, GewataB3, GewataB4)
##calculate the ndvi with band3 and band4
ndvi <- overlay(GewataB4, GewataB3, fun=function(x,y){(x-y)/(x+y)})
##multiply all values by 10000
ndvi <- calc(ndvi, fun = function(x) floor(x*10000))
# change the data type
dataType(ndvi) <- "INT2U"
# name this layer to make plots interpretable
names(ndvi) <- "NDVI"
# make the covariate rasterBrick
covs <- addLayer(gewata, ndvi, vcfGewata)
plot(covs)

load("training.RData")
########drawing training polygons
##cropland
#cropland <- drawPoly(sp=TRUE)
###drwing more cropland and combine
#cropland <- gUnion(cropland, drawPoly(sp=TRUE))
##project the traing polygon from lulcGewata 
projection(cropland) <- projection(lulcGewata)
#####check the projection of cropland
projection(cropland)
plot(lulcGewata)
plot(cropland, add=TRUE)
###convert the training polygon of cropland to saptialpolygonsdataframe
cropland <- SpatialPolygonsDataFrame(cropland, data=data.frame(
  class="cropland"), match.ID=FALSE)



##forest
#draw traing polygons of forest
#forest <- drawPoly(sp=TRUE)
###repeat the below line to draw forest training polygon and combine with previous one
#forest <- gUnion(forest, drawPoly(sp=TRUE))
projection(forest) <- projection(lulcGewata)
projection(forest)
plot(lulcGewata)
plot(forest, add=TRUE)
###convert the traing polygon to saptialpolygonsdataframe and add class named forest
forest<- SpatialPolygonsDataFrame(forest, data=data.frame(
  class="forest"), match.ID=FALSE)

###baresoil

# and assign the result to an extent object e
##as the baresoil is not clear ,draw an exten to zoom in
#e <- drawExtent()
#class(e)
#print(e)

#plot(lulcGewata, ext=e)
##draw the training polygons of baresoil 
#baresoil <- drawPoly(sp=TRUE)
#baresoil <- gUnion(baresoil, drawPoly(sp=TRUE))
projection(baresoil) <- projection(lulcGewata)
projection(baresoil)
plot(lulcGewata)
plot(baresoil, add=TRUE)
baresoil<- SpatialPolygonsDataFrame(baresoil, data=data.frame(
  class="baresoil"), match.ID=FALSE)

##bamboo
#bamboo <- drawPoly(sp=TRUE)
#bamboo <- gUnion(bamboo, drawPoly(sp=TRUE))
projection(bamboo) <- projection(lulcGewata)
projection(bamboo)
plot(lulcGewata)
plot(bamboo, add=TRUE)
bamboo<- SpatialPolygonsDataFrame(bamboo, data=data.frame(
  class="bamboo"), match.ID=FALSE)

###wetland
#wetland <- drawPoly(sp=TRUE)
#wetland <- gUnion(wetland, drawPoly(sp=TRUE))
projection(wetland) <- projection(lulcGewata)
projection(wetland)
plot(lulcGewata)
plot(wetland, add=TRUE)
wetland<- SpatialPolygonsDataFrame(wetland, data=data.frame(
  class="wetland"), match.ID=FALSE)

##coffee plantation
#coffee <- drawPoly(sp=TRUE)
#coffee <- gUnion(coffee, drawPoly(sp=TRUE))
projection(coffee) <- projection(lulcGewata)
projection(coffee)
plot(lulcGewata)
plot(coffee, add=TRUE)
coffee<- SpatialPolygonsDataFrame(coffee, data=data.frame(
  class="coffee"), match.ID=FALSE)

cropland <- spChFIDs(cropland, "cropland")
forest <- spChFIDs(forest, "forest")
coffee <- spChFIDs(coffee, "coffee")
wetland <- spChFIDs(wetland, "wetland")
bamboo <- spChFIDs(bamboo, "bamboo")
baresoil <- spChFIDs(baresoil, "baresoil")

###combine the training polygon one by one 
trainingPoly <- spRbind(cropland, bamboo)
trainingPoly <- spRbind(trainingPoly, baresoil)
trainingPoly <- spRbind(trainingPoly, coffee)
trainingPoly <- spRbind(trainingPoly, forest)
trainingPoly <- spRbind(trainingPoly, wetland)

##save out the training polygon result
trainingPoly@data
##plot the lulcGewata and trainingpoly in one plot
plot(lulcGewata)
plot(trainingPoly, add=TRUE)

########################

# superimpose training polygons onto ndvi plot
plot(ndvi)
plot(trainingPoly, add = TRUE)

#check the value of class
trainingPoly@data$class
str(trainingPoly@data$class)

# define a reclassification function which substitutes
# the character label for the factor level (between 1 and 6)
reclass <- function(x){
  which(x==levels(trainingPoly@data$class))
}
# use sapply() to apply this function over each element of the 'Class' column
# and assign to a new column called 'Code'
trainingPoly@data$Code <- sapply(trainingPoly@data$class, FUN=reclass)

# assign 'Code' values to raster cells (where they overlap)
classes <- rasterize(trainingPoly, ndvi, field='Code')
# set the dataType of the raster to INT1U
dataType(classes) <- "INT1U"
# define a colour scale for the classes
cols <- c("orange", "light green", "brown", "light pink", "dark green", "light blue")
# plot without a legend
plot(classes, col=cols, legend=FALSE)
# add a customized legend
legend("topright", legend=c("cropland","bamboo","baresoil","coffee","forest","wetland"), fill=cols, bg="white")
#mask the covs with classes 
covmasked <- mask(covs, classes)
plot(covmasked)
# add the classes layer to this new brick
names(classes) <- "class"
trainingbrick <- addLayer(covmasked, classes)
plot(trainingbrick)
# extract all values into a matrix
valuetable <- getValues(trainingbrick)
# convert to a data.frame and inspect the first and last rows
valuetable <- as.data.frame(valuetable)
head(valuetable)
tail(valuetable)

#remove the na value
valuetable <- valuetable[!is.na(valuetable$class),]
head(valuetable)
tail(valuetable)
# convert values in the class column to factors
valuetable$class <- factor(valuetable$class, levels = c(1:6))

# add a label column to valuetable
valuetable$label <- with(valuetable, ifelse(class==1, "cropland",
                                            ifelse(class==2, "bamboo",
                                            ifelse(class==3,"baresoil",
                                            ifelse(class==4, "coffee",
                                            ifelse(class==5, "forest","wetland"))))))
                                                                                                                                                 

head(valuetable$label)

# NA values are not permitted in the covariates/predictor columns
# keep only the rows with containing no NA's
valuetable <- na.omit(valuetable)
# construct a random forest model
# covariates (x) are found in columns 1 to 5 of valuetable
names(valuetable)
#remove the na value in class
valuetable <- valuetable[!is.na(valuetable$class),]
########random froest
library(randomForest)
##get the formular of randomforest
save(valuetable,file="valuetable.RData")
load("valuetable.RData")
modelRF1 <- randomForest(x=valuetable[,c(1:5)], y=valuetable$class,
                         importance = TRUE,progress="text")
# inspect the structure and element names of the resulting model
modelRF1
class(modelRF1)
str(modelRF1)
names(modelRF1)
# inspect the confusion matrix of the OOB error assessment
modelRF1$confusion
# to make the confusion matrix more readable
colnames(modelRF1$confusion) <- c("cropland", "bamboo", "baresoil","coffee","forest","wetland","class.error")
rownames(modelRF1$confusion) <- c("cropland", "bamboo", "baresoil","coffee","forest","wetland")

#####################################################################
#####################Exercise Question Answer########################
modelRF1$confusion###confusion error of different error
##Answer 1:The forest has the highest accuracy and bamboo has the lowest accuracy
varImpPlot(modelRF1)
##Answer 2:the most important ranking bands is band 4
## the difference is the important ranking of band 2 and band 3 are 
##different compared with three class.In this case band 2 is more important than band 3

####################################################################
# check layer and column names
names(covs)
names(valuetable)
# predict land cover using the RF model
predLC <- predict(covs, model=modelRF1, na.rm=TRUE,progress="text")
# plot the results
cols <- c("orange", "dark green", "light blue","grey","red","pink")

plot(predLC, col=cols, legend=FALSE)
legend("bottomright", legend=c("cropland", "bamboo", "baresoil","coffee","forest","wetland"), fill=cols, bg="white")
########png


#a plot of the original lulcGewata raster with a meaningful legend (ie. classes as characters)
lulcGewata
cols <- c("orange", "dark green", "light blue","grey","red","pink")
plot(lulcGewata,col=cols)
legend("bottomright", legend=c("cropland", "banmboo", "bare soil","coffee plantation","forest","wetland"), fill=cols, bg="white")
plot(classes, legend=FALSE)
plot(lulcGewata)
#####################################################################
################Exercise Answer 3###################################

####check the histogram of each class in different bands
# 1. NDVI
p1 <- ggplot(data=valuetable, aes(x=NDVI)) +
  geom_histogram(binwidth=300) +
  facet_wrap(~ label) +
  theme_bw()
p1

##############There are alot of overlap between bamboo and forest in bands of ndvi
# 2. VCF
p2 <- ggplot(data=valuetable, aes(x=vcf2000Gewata)) +
  geom_histogram(binwidth=5) +
  labs(x="% Tree Cover") +
  facet_wrap(~ label) +
  theme_bw()
p2
#############bamboo and forest are in the same range between 60% and 85%
# 3. Bands 3 and 4
p3 <- ggplot(data=valuetable, aes(x=gewataB3, y=gewataB4)) +
  stat_bin2d() +
  facet_wrap(~ label) +
  theme_bw()
p3
##########wetland and forest have a lot overlap at band 4
# 4. Bands 2 and 3
p4 <- ggplot(data = valuetable, aes(x=gewataB2, y=gewataB3)) +
  stat_bin2d() +
  facet_wrap(~ label) +
  theme_bw()
p4
#########cropland and forest have a bit overlap at band 3
modelRF1$confusion
######From the confusion we know there are some overlap between belows class
#########wetland and bamboo
###########bamboo with forest
##########crop with wetland
##########crop with forest

#########The polt can tell something but not so accurate as some class has a overlap.Since 
###some class not occupy too much pixels in the image .The overlap is not so much actually.

