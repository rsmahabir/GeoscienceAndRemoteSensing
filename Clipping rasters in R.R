#required packages
library(sp) #not necessary for this set of particular operations but required upon loading raster package
library(raster)
 
#R uses GDAL for these operations on the rasters. It is an equivalent approach.
#more on this: https://geoscripting-wur.github.io/IntroToRaster/
 
 
#downloading worldclim data -------------------------
w.stack = getData('worldclim',
                  var='bio',
                  res=10) #Downloading a WCLIM tile set
 
#If data has already been downloaded:
w.stack = stack("path2file/filename.tif")
 
#both methods load all the bands - to make processing faster we select only one
w.raster <- raster(w.stack,layer=1)
 
#Clipping exercise
#Setting the clip extent
e <- extent(-133.2153004 * 1.05, #xmin
            83.10261     * 0.95, #xmax
            46.81686     * 0.95, #ymin
            58.7162728   * 1.05) #ymax
 
#Clipping command and saving to a file
w.raster.crop <- crop(w.raster, #raster variable
                      e,        #extent variable
                      overwrite=TRUE, #forces overwrite on the output files
                      filename="D:/wclim_b01.asc") #saves file to a given path - any common file extensions can be used here. Some other data types allow compression that saves disk space
 
#Clipping all the all the predictors in the original file
w.stack.crop <- crop(w.stack, #raster stack variable
                     e,        #extent variable
                     bylayer=TRUE, #creates one new raster per each layer in the stack
                     suffix="names", #keeps the original name and uses it for the name of each saved file
                     overwrite=TRUE, #forces overwrite on the output files
                     filename="D:/wclim_.asc") #saves file to a given path - any common file extensions can be used here. Some other data types allow compression that saves disk space
 
#optional:
#The names of each raster can be changed using the command names(w.rstack) which will in turn change the output filenames
