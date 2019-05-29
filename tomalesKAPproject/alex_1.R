getwd()
install.packages("googledrive")
#load google drive
library("googledrive")

#----- Creating a Output folder -----
#mainDir defines the path of working directory
mainDir<-"C:/Users/aalda/Desktop/T/tomalesKAPproject/All plots 2018")

#subDir defines the name of the output folder 
subDir<-"outputDirectory"

if(file.exists(subDir)){
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
}
setwd("C:/Users/aalda/Desktop/T/tomalesKAPproject/All plots 2018")

# Load Data ---------------------------------------------------------------
#Raster
files <- list.files()
dbf.files <- files[grep(".tif", files, fixed=T)]
for(i in dbf.files) { assign(unlist(strsplit(i, "[.]"))[1], raster(i)) }

BH1_RGB<-stack("BH-01 RGB_modified.tif") #because it is tif
BH1_IR<-raster('BH-01 IR_modified.tif')
BL2_RGB<-stack("BL-02 RGB.tif")
BL2_IR<-raster('BL-02 IR.tif')
#-----Creating files within Output folder ----