
#---------------------------------
setwd("C:/Users/aalda/Desktop/All plots 2018")

#install.packages("RStoolbox")
#install.packages("rasterVis")
#install.packages('raster')
#install.packages('gdalUtils')
#install.packages('tidyverse')
#install.packages('rgr')
#install.packages('uavRst')

library(gdalUtils)
library('RStoolbox')
library('rasterVis')
library('raster')
library('ggplot2')
library('rgr')
library('tidyverse')
#library('uavRst') #may not have been used.

# Load Data ---------------------------------------------------------------
#Raster

# list.files() producs a character vector of the names of files in the named directory "All plots 2018".
files <- list.files()
#
dbf.files <- files[grep(".tif", files, fixed=T)]
for(i in dbf.files) { assign(unlist(strsplit(i, "[.]"))[1], raster(i)) }

BH1_RGB<-stack("BH-01 RGB_modified.tif") #because it is tif
BH1_IR<-raster('BH-01 IR_modified.tif')
BL2_RGB<-stack("BL-02 RGB.tif")
BL2_IR<-raster('BL-02 IR.tif')


# BH-01 -------------------------------------------------------------------


#align extent

BH1_IR_proj<-projectRaster(BH1_IR, BH1_RGB)


#Shapefile

polygon<-shapefile("Polygons.shp")
plot(polygon, add=TRUE) #Alex_Error-line 49: plot.new has not been called yet.
BH1_shp_ug<-subset(polygon, PlotID=='BH-01 UG') #Ungrazed
BH1_shp_g<-subset(polygon, PlotID=='BH-01 G')#Grazed

# Mask and clip rasters to polygon --------------------------------------------------

#Ungrazed

BH1_RGB_mask<-mask(BH1_RGB, BH1_shp_ug)
BH1_IR_mask<-mask(BH1_IR_proj, BH1_shp_ug) 


BH1RGB_crop<- crop(BH1_RGB_mask, BH1_shp_ug) 
plot(BH1RGB_crop) #Alex_Plot of the four graphs.
ex<-extent(BH1RGB_crop)

BH1IR_crop<- crop(BH1_IR_mask, ex) 
plot(BH1IR_crop) #Alex_one IR graph.


# Stack and Brick IR and RGB --------------------------------------------------------

BH1_stack<-stack(BH1RGB_crop, BH1IR_crop)
nlayers(BH1_stack)
BH1_stack<-writeRaster(BH1_stack, filename="C:/Users/aalda/Desktop/All plots 2018/BH1_Stack.tif", format="GTiff", overwrite=TRUE)




# Calculate NDVI ----------------------------------------------------------

BH1_ndvi_ungrazed<-((BH1_stack[[5]]-BH1_stack[[1]])/(BH1_stack[[5]]+BH1_stack[[1]]))
plot(BH1_ndvi_ungrazed) #Alex_plot of NDVI of ungrazed area.
hist(BH1_ndvi_ungrazed) #Alex_NVDI historgram of ungrazed area.



# Do it all again for BH1 Grazed ------------------------------------------

# Mask and clip rasters to polygon --------------------------------------------------

#Grazed

BH1_RGB_mask<-mask(BH1_RGB, BH1_shp_g)
BH1_IR_mask<-mask(BH1_IR_proj, BH1_shp_g) 


BH1RGB_crop<- crop(BH1_RGB_mask, BH1_shp_g) 
plot(BH1RGB_crop) #Alex_plot of four graphs.
ex<-extent(BH1RGB_crop)

BH1IR_crop<- crop(BH1_IR_mask, ex) 
plot(BH1IR_crop)  #Alex_single plot of grazed area.


# Stack and Brick IR and RGB --------------------------------------------------------

BH1_stack<-stack(BH1RGB_crop, BH1IR_crop)
nlayers(BH1_stack)

# Calculate NDVI ----------------------------------------------------------

BH1_ndvi_grazed<-((BH1_stack[[5]]-BH1_stack[[1]])/(BH1_stack[[5]]+BH1_stack[[1]]))
plot(BH1_ndvi_grazed) #Alex: NDVI plot of grazed area.
BL2_IR_mask<-mask(BL2_IR_proj, BL2_shp_g) #Alex: BL2_IR_proj is found later (line 185) in code and BL2_shp_g (line 181) is earlier in code.



BL2RGB_crop<- crop(BL2_RGB_mask, BL2_shp_g) #Alex: run line 199 for BL2_RGB_mask first.
plot(BL2RGB_crop)     #Alex: (assistance) four plots but none of four images are displayed.  
ex<-extent(BL2RGB_crop)

BL2IR_crop<- crop(BL2_IR_mask, ex) 
plot(BL2IR_crop)    #Alex: (check) plot and image are displayed but the image is in shape of a parallelgram.


# Stack and Brick IR and RGB --------------------------------------------------------

BL2_stack<-stack(BL2RGB_crop, BL2IR_crop)
nlayers(BL2_stack)   #Alex: 5 layers are present. 

# Calculate NDVI ----------------------------------------------------------

BL2_ndvi_grazed<-((BL2_stack[[5]]-BL2_stack[[1]])/(BL2_stack[[5]]+BL2_stack[[1]]))
plot(BL2_ndvi_grazed) #end here.
hist(BL2_ndvi_grazed)

#compare to ungrazed

BL2_ndvi_ungrazed<-as.data.frame(BL2_ndvi_ungrazed)  #Alex: run line 218 first for BL2_ndvi_ungrazed.
BL2_ndvi_grazed<-as.data.frame(BL2_ndvi_grazed)

BH_01_g<-tibble(
  Value=BL2_ndvi_grazed$layer,
  Treatment="Grazed"
)

BH_01_ug<-tibble(
  Value=BL2_ndvi_ungrazed$layer,
  Treatment="Ungrazed"
)

BH_01<-rbind(BH_01_g, BH_01_ug)
ggplot(data=BH_01, aes(x=Treatment, y=Value))+
  geom_violin(scale='area')
hist(BH1_ndvi_grazed)      #Alex: historgram plot produced.

#compare to ungrazed

BH1_ndvi_ungrazed<-as.data.frame(BH1_ndvi_ungrazed)
BH1_ndvi_grazed<-as.data.frame(BH1_ndvi_grazed)

BH_01_g<-tibble(
  Value=BH1_ndvi_grazed$layer,
  Treatment="Grazed"
)

BH_01_ug<-tibble(
  Value=BH1_ndvi_ungrazed$layer,
  Treatment="Ungrazed"
)

BH_01<-rbind(BH_01_g, BH_01_ug)
ggplot(data=BH_01, aes(x=Treatment, y=Value))+
  geom_violin(scale='area')





# BL-02 -------------------------------------------------------------------

BL2_shp_ug<-subset(polygon, PlotID=='BL-02 UG') #Ungrazed
BL2_shp_g<-subset(polygon, PlotID=='BL-02 G')#Grazed

#align extent

BL2_IR_proj<-projectRaster(BL2_IR, BL2_RGB)

#Shapefile

polygon<-shapefile("Polygons.shp")
plot(polygon, add=TRUE)               #Alex: graph produced.
BL2_shp_ug<-subset(polygon, PlotID=='BH-01 UG') #Ungrazed
BL2_shp_g<-subset(polygon, PlotID=='BH-01 G')#Grazed

# Mask and clip rasters to polygon --------------------------------------------------

#Ungrazed


BL2_RGB_mask<-mask(BL2_RGB, BL2_shp_ug) #Alex: ran fine. 
BL2_IR_mask<-mask(BL2_IR_proj, BL2_shp_ug) 


BL2RGB_crop<- crop(BL2_RGB_mask, BL2_shp_ug) 
plot(BL2RGB_crop)
ex<-extent(BL2RGB_crop)

BL2IR_crop<- crop(BL2_IR_mask, ex) #Alex: no image displayed.
plot(BL2IR_crop)                   #Alex: no image displayed.


# Stack and Brick IR and RGB --------------------------------------------------------

BL2_stack<-stack(BL2RGB_crop, BL2IR_crop)   
nlayers(BL2_stack)                 #Alex: 5 layers. 

# Calculate NDVI ----------------------------------------------------------

BL2_ndvi_ungrazed<-((BL2_stack[[5]]-BL2_stack[[1]])/(BL2_stack[[5]]+BL2_stack[[1]]))
plot(BL2_ndvi_ungrazed)           #Alex: graph but no image dispalyed.
hist(BL2_ndvi_ungrazed)           #Alex: no histogram displayed. Error-invalid number of 'breaks'.



# Do it all again for BL2 Grazed ------------------------------------------

# Mask and clip rasters to polygon --------------------------------------------------

#Grazed

BL2_RGB_mask<-mask(BL2_RGB, BL2_shp_g)

