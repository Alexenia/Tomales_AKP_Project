
#---------------------------------
#Set working directory to ensure R can find the path to the files we wish to work with.
setwd("C:/Users/aalda/Desktop/All plots 2018")

#Installing and Loading all packages-------------

#install.packages("RStoolbox")
#install.packages("rasterVis")
#install.packages('raster')
#install.packages('gdalUtils')
#install.packages('tidyverse')
#install.packages('rgr')
#install.packages('uavRst')
#install.packages('rgdal') -Alex

#library('gdalUtils')      #Not needed.
#library('RStoolbox')      #Not needed.
#library('rasterVis')      #Not needed.
#library('ggplot2')        #Not needed.
#library('rgr')            #Not needed. 
#library('rgdal')          #Not needed.
#library('uavRst')         #Not needed.
library('raster')     #Functions to write, read, and manipulate raster data. 
library('tidyverse')  #Needed for the Tibble function.   


# Load Data ---------------------------------------------------------------

#Raster

# Creates a character vector list of raster files using the list.files() function in the folder "All plots 2018". This list will be used to generate a Rasterstack.
#files <- list.files()
#Creates a file that is a list of names only having .tif extensions. Grep function finds ".tiff" pattern in the created "files" and fixed=T means pattern is a text string.
#dbf.files <- files[grep(".tif", files, fixed=T)]        
#Use a For Loop to search for files in database, dbf.files, ... 
#for(i in dbf.files) { assign(unlist(strsplit(i, "[.]"))[1], raster(i)) }    

#Import RGB and IR images of each plot. RGB images are a multi-band rasters data. IR images are raster layers.  

BH1_RGB<-stack("BH-01 RGB_modified.tif")       # (1) Import multi-band raster data, using the stack() function. 
BH1_IR<-raster('BH-01 IR_modified.tif')        # (1) Import and create a Rasterlayer file using the raster function. 
BL2_RGB<-stack("BL-02 RGB.tif")  #(2)
BL2_IR<-raster('BL-02_IR.tif')   #(2)
#BH2_RGB<-stack("BH-02 RGB.tif")  #(3)        
#BH2_IR<-raster('BH-02 IR.tif')   #(3)
LGH1_RGB<-stack("LGH1-RGB_1-20_modified.tif")  #(4)
LGH1_IR<-raster('LGH1-IR_1-20_modified.tif')   #(4)
LGH2_RGB<-stack("LGH-2 RGB_modified.tif")      #(5)
LGH2_IR<-raster('LGH-2 IR_modified.tif')       #(5)
LGL1_RGB<-stack("LGL-1 RGB 1-20_modified.tif") #(6)
LGL1_IR<-raster('LG-L1 IR 1-20_modified.tif')  #(6)
LGL2_RGB<-stack("LGL-2-RGB_modified_2.tif")    #(7)
LGL2_IR<-raster('LGL-2 IR-modified.tif')       #(7)
OGH1_RGB<-stack("OGH-1 RGB2_modified.tif")     #(8)
OGH1_IR<-raster('OGH-1 IR_modified.tif')       #(8)
OGH2_RGB<-stack("OG-H2 RGB_modified.tif")      #(9)
OGH2_IR<-raster('OG-H2 IR_modified.tif')       #(9)



### (1) BH-01 -------------------------------------------------------------------



##Align Extent

#Project the data of a Raster object to a new RAster object with another projection (crs). projectRaster(from, to). 
BH1_IR_proj<-projectRaster(BH1_IR, BH1_RGB) 


##Shapefile


#Displays the specified Plot area. (Image #1: plot_BH1_IR_proj). One graph produced. 
plot(BH1_IR_proj)                    
#Reads the shapefile file that is storing geospatial data (polygons with attributes), the grazed and ungrazed enclosures in each plot.
polygon<-shapefile("Polygons.shp")   
#Draws the grazed and ungrazed enclosures over the specified plot. (Image #2: plot_Polygons).
plot(polygon, add=TRUE)               
#Separates the specified grazed and ungrazed enclosure by the PlotID from the rest of the enclosures listed in the Polygons.shp file.    
BH1_shp_ug<-subset(polygon, PlotID=='BH-01 UG')  #Ungrazed Enclosure  
BH1_shp_g<-subset(polygon, PlotID=='BH-01 G')    #Grazed Enclosure


## BH1-Ungrazed: Mask and clip rasters to polygon --------------------------------------------------


#Mask out the RGB and IR data outside the boundary of the grazed and ungrazed enclosures.
BH1_RGB_mask<-mask(BH1_RGB, BH1_shp_ug)        
BH1_IR_mask<-mask(BH1_IR_proj, BH1_shp_ug) 

#Crop the masked RGB and IR data outside the boundary of the ungrazed enclosure. 

#Start with RGB image. Crop the masked RGB data outside the boundary of the ungrazed enclosure. 
BH1RGB_crop<- crop(BH1_RGB_mask, BH1_shp_ug) 
#View the cropped RGB image of the ungrazed enclosure.          (Image #3: Plot_BH1RGB_crop). Four graphs produced.
plot(BH1RGB_crop)            
#View distribution of RGB image values of the cropped ungrazed enclosure. (Image #4: hist_BH1RGB_crop) 
hist(BH1RGB_crop)
#Find the extent of the cropped RGB ungrazed enclosure.
ex<-extent(BH1RGB_crop)


#Use the extent of the cropped RGB image to crop the IR image of the ungrazed enclosure.  
BH1IR_crop<- crop(BH1_IR_mask, ex) 
#View the cropped IR image of the ungrazed enclosure.          (Image #5: BH1IR_crop_ungrazed)
plot(BH1IR_crop)             


## BH1-Ungrazed: Stack IR and RGB RasterLayers --------------------------------------------------------


#Stack the cropped RGB and IR RasterLayers together to create a RasterStack.
BH1_stack<-stack(BH1RGB_crop, BH1IR_crop)
#Check the number of bands of the RasterStack. There should be 5 layers.
nlayers(BH1_stack)


## BH1-Ungrazed: Calculate NDVI of Ungrazed Enclosure ----------------------------------------------------------


#calculate NDVI:  NDVI = (NIR [band#5] - RED [band#1]) / (NIR [band#5] + RED [band#1])
BH1_ndvi_ungrazed<-((BH1_stack[[5]]-BH1_stack[[1]])/(BH1_stack[[5]]+BH1_stack[[1]]))
#View image of NDVI image of ungrazed enclosure. (Image) 
plot(BH1_ndvi_ungrazed)         #Alex: one image produced.        (Image#6:BH1_NDVI_ungrazed)
# View the distribution of NDVI values of the ungrazed enclosure. (Image#7: hist_BH1_ndvi_ungrazed)
hist(BH1_ndvi_ungrazed)         



# Do it all again for BH1 Grazed ------------------------------------------



# BH1-Grazed: Mask and Clip Rasters to Polygon -----------------------------


#Mask RGB and IR data outside of the grazed ploygon enclosure.
BH1_RGB_mask<-mask(BH1_RGB, BH1_shp_g)
BH1_IR_mask<-mask(BH1_IR_proj, BH1_shp_g) 

#Crop the masked RGB and IR data outside the boundary of the ungrazed enclosure. 

#Start with RGB image. Crop the masked RGB data outside the boundary of the grazed enclosure. 
BH1RGB_crop<- crop(BH1_RGB_mask, BH1_shp_g) 

#View the cropped RGB image of the grazed enclosure. (Image #8: BH1RGB_crop_grazed). Four graphs produced.
plot(BH1RGB_crop)          

#View the distribution of RGB image values of the cropped grazed enclosure.
hist(BH1RGB_crop)

#Find the extent of the cropped RGB grazed enclosure to later set the extent of the IR grazed enclosure.
ex<-extent(BH1RGB_crop)


#Set the extent of the masked IR image of the grazed enclosure using the extent of the cropped RGB grazed enclosure.  
BH1IR_crop<- crop(BH1_IR_mask, ex) 
#View the cropped IR image of the grazed enclosure. (Image #9: BH1IR_crop_grazed)
plot(BH1IR_crop)           


##BH1-Grazed: Stack IR and RGB RasterLayers --------------------------------------------------------

#Stack the cropped RGB and IR RasterLayers to create a RasterStack.
BH1_stack<-stack(BH1RGB_crop, BH1IR_crop)
#Check the number of bands of the RasterStack. There should be 5 layers. 
nlayers(BH1_stack)          


#BH1-Grazed: Calculate NDVI ----------------------------------------------------------


#Calculate NDVI of the grazed enclosure: NDVI = ((NIR [band#5] -  RED [band#1]) / (NIR [band#5] + RED [band#1]))
BH1_ndvi_grazed<-((BH1_stack[[5]]-BH1_stack[[1]])/(BH1_stack[[5]]+BH1_stack[[1]]))
#Graph NDVI image of grazed enclosure. (Image #10: BH1_ndvi_grazed). One graph produced.
plot(BH1_ndvi_grazed)           
#View the distribution of the NDVI values of the grazed enclosure. (Image #11: hist_BH1_ndvi_grazed)
hist(BH1_ndvi_grazed)           


##Compare NDVI Distribution of Grazed to Ungrazed Enclosures

#Turn NDVI grazed and ungrazed RasterLayers into data frames for comparison. 
BH1_ndvi_ungrazed<-as.data.frame(BH1_ndvi_ungrazed)
BH1_ndvi_grazed<-as.data.frame(BH1_ndvi_grazed)

#Turn NDVI Grazed Enclosure Rasterlayer into a data frame.
BH_01_g<-tibble(
  Value=BH1_ndvi_grazed$layer,
  Treatment="Grazed"
)

#Turn Ungrazed Enclosure Rasterlayers into a data frame.
BH_01_ug<-tibble(
  Value=BH1_ndvi_ungrazed$layer,
  Treatment="Ungrazed"
)

#Merge the grazed and ungrazed data frames, by combinding their rows.   
BH_01<-rbind(BH_01_g, BH_01_ug)
#Graph the NDVI distribution of the grazed and ungrazed enclosures in Vilon plot. (Image #12: ggplot_BH-01). One graph produced.
ggplot(data=BH_01, aes(x=Treatment, y=Value))+
  geom_violin(scale='area')                          




### (2) BL-02 -------------------------------------------------------------------


##align extent


BL2_IR_proj<-projectRaster(BL2_IR, BL2_RGB)    

##Shapefile


#(Image-1: BL2_IR_proj)
plot(BL2_IR_proj) 
polygon<-shapefile("Polygons.shp")
#(Image-2: BL2_plot_polygons)
plot(polygon, add=TRUE)
BL2_shp_ug<-subset(polygon, PlotID=='BL-02 UG') #Ungrazed
BL2_shp_g<-subset(polygon, PlotID=='BL-02 G') #Grazed


# Wrong: BL2_shp_ug<-subset(polygon, PlotID=='BH-01 UG') #Ungrazed     #Alex: warming messages.
#wrong: BL2_shp_g<-subset(polygon, PlotID=='BH-01 G')#Grazed

# Mask and clip rasters to polygon --------------------------------------------------

#Ungrazed

BL2_RGB_mask<-mask(BL2_RGB, BL2_shp_ug)
BL2_IR_mask<-mask(BL2_IR_proj, BL2_shp_ug) 


BL2RGB_crop<- crop(BL2_RGB_mask, BL2_shp_ug)    #Alex: first run line 191 for BL2_shp_ug. Problem: Error in .local(x, y, ...) : extents do not overlap
plot(BL2RGB_crop)                 #Alex: four images produced in shape of parallelgrams. (Image-3: BL2RGB_crop_ungrazed)
ex<-extent(BL2RGB_crop)
hist(BL2RGB_crop)                    #(Image-4: hist_BL2RGB_crop_ungrazed)
BL2IR_crop<- crop(BL2_IR_mask, ex) 
plot(BL2IR_crop)                    #Alex: (check) one image not produced. (Image-5: BL2IR_crop_ungrazed)


# Stack and Brick IR and RGB --------------------------------------------------------

BL2_stack<-stack(BL2RGB_crop, BL2IR_crop)
nlayers(BL2_stack)                           #Alex: 5 layers.

# Calculate NDVI ----------------------------------------------------------

BL2_ndvi_ungrazed<-((BL2_stack[[5]]-BL2_stack[[1]])/(BL2_stack[[5]]+BL2_stack[[1]]))
plot(BL2_ndvi_ungrazed)                 #Alex: (check) one image produced. (Image-6: BL2_NDVI_ungrazed)
hist(BL2_ndvi_ungrazed)                 #Alex: (check) histogram produced. (Image-7: hist_BL2_NDVI_ungrazed)



# Do it all again for BL2 Grazed ------------------------------------------

# Mask and clip rasters to polygon --------------------------------------------------

#Grazed

BL2_RGB_mask<-mask(BL2_RGB, BL2_shp_g)
BL2_IR_mask<-mask(BL2_IR_proj, BL2_shp_g)     #Ales wrote this line.


BL2RGB_crop<- crop(BL2_RGB_mask, BL2_shp_g)    
plot(BL2RGB_crop)                              #Alex: four images in the shape of parallelgrams.(Image-8: BL2_RGB_crop_grazed)
hist(BL2RGB_crop)                              #(Image-9: hist_BL2RGB_crop_grazed). Four graphs produced. 
ex<-extent(BL2RGB_crop)

BL2IR_crop<- crop(BL2_IR_mask, ex) 
plot(BL2IR_crop)                          #Alex: one image produce in the shape of parallelgram. (Image-10: BL2IR_crop_grazed)


# Stack and Brick IR and RGB --------------------------------------------------------

BL2_stack<-stack(BL2RGB_crop, BL2IR_crop)
nlayers(BL2_stack)

# Calculate NDVI ----------------------------------------------------------

BL2_ndvi_grazed<-((BL2_stack[[5]]-BL2_stack[[1]])/(BL2_stack[[5]]+BL2_stack[[1]]))
plot(BL2_ndvi_grazed)                           #Alex: one image produced in the shape of a parallelgram.(Image-11: BL2_NDVI_grazed)
hist(BL2_ndvi_grazed)                           #Alex: histogram produced. (Image-12: hist_BL2_ndvi_grazed)

#compare to ungrazed
BL2_ndvi_ungrazed<-((BL2_stack[[5]]-BL2_stack[[1]])/(BL2_stack[[5]]+BL2_stack[[1]]))  #alex:copied/pasted here.
BL2_ndvi_ungrazed<-as.data.frame(BL2_ndvi_ungrazed) #Alex: First run line 217 for BL2_ndvi_ungrazed which is ( BL2_ndvi_ungrazed<-((BL2_stack[[5]]-BL2_stack[[1]])/(BL2_stack[[5]]+BL2_stack[[1]])) .
BL2_ndvi_grazed<-as.data.frame(BL2_ndvi_grazed)

#alex wrote this
BL_02_g<-tibble(
  Value= BL2_ndvi_ungrazed$layer,
  Treatment="Grazed"
)

BL_02_ug<-tibble(
  Value=BL2_ndvi_ungrazed$layer,
  Treatment= "Ungrazed"
)

BL_02<-rbind(BL_02_g, BL_02_ug)
ggplot(data= BL_02, aes (x= Treatment, y=Value))+
  geom_violin(scale='area')       #(Image-13: ggplot_BL_02).



#(3) BH-02 ----------------------------------------------------------------------------------------

#BH2_RGB<-stack("BH-02 RGB.tif")          
#BH2_IR<-raster('BH-02 IR.tif')   


#Align Extent  -make the projection of image.name_IR and image.name_RGB the same.

projection(BH2_IR)     #Projection of BH2_IR
projection(BH2_RGB)    #Projection of BH2_RGB
crs(BH2_IR)<-'+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'   #Assign the projection of BH2_RGB to BH2_IR.
projection(BH2_IR)      #This shows that the crs of BH2_IR now has the same crs as BH2_RGB. 
BH2_IR_proj<-BH2_IR                  #Conformation of changed projection.


#Shapefile

plot(BH2_IR_proj)                                  #1-BH2_IR_proj image. ONe graph produced.
polygon<-shapefile("Polygons.shp")           
plot(polygon, add=TRUE)                            #Error: No boundaries displayed and no image with boundary produced. 
BH2_shp_ug<- subset(polygon, PlotID == 'BH-02 UG')  
BH2_shp_g<-subset(polygon, PlotID == 'BH-02_G')


#Mask and clip raster to polygon ---------------------------------

#BH2: UNGRAZED

#length(BH2_RGB)
#length(BH2_shp_ug)
BH2_RGB_mask<-mask(BH2_RGB, BH2_shp_ug)      
BH2_IR_mask<-mask(BH2_IR_proj, BH2_shp_ug)    

BH2RGB_crop<-crop(BH2_RGB_mask, BH2_shp_ug)
plot(BH2RGB_crop)
ex<-extent(BH2RGB_crop)

BH2IR_crop<-crop(BH2_IR_mask, ex)
plot(BH2IR_crop)

#BH2- Stack and Brick IR and RGB ----------------------------------

BH2_stack<-stack(BH2RGB_crop, BH2IR_crop)
nlayers(BH2_stack)

#BH2- Calculate NDVI for ungrazed ---------------------------------

BH2_ndvi_ungrazed<-((BH2_stack[[5]]-BH2_stack[[1]]) / (BH2_stack[[5]]+BH2_stack[[1]]))
Plot(BH2_ndvi_ungrazed)
hist(BH2_ndvi_ungrazed)

# Do it all again for Grazed ----------------------------------------------

#BH2: GRAZED -------------------------------------------------------

BH2_RGB_mask<-mask(BH2_RGB, BH2_shp_g)   
BH2_IR_mask<-mask(BH2_IR_proj, BH2_shp_g)  

BH2_RGB_crop<-crop(BH2_RGB_mask, BH2_shp_g)
plot(BH2RGB_crop)
ex<-extent(BH2RGB_crop)

BH2IR_crop<-crop(BH2_IR_mask, ex)
plot(BH2IR_crop)

#BH2- Stack and brick IR and RGB ----------------------------------

BH2_stack<-stack(BH2RGB_crop, BH2IR_crop)
nlayers(BH2_stack)

#BH2- Calculate NDVI for Grazed -----------------------------------

BH2_ndvi_grazed<-((BH2_stack[[5]]-BH2_stack[[1]])/(BH2_stack[[5]]+BH2_stack[[1]]))
plot(BH2_ndvi_grazed)

#BH2: compare grazed to ungrazed ----------------------------------

BH2_ndvi_ungrazed<-as.data.frame(BH2_ndvi_ungrazed)
BH2_ndvi_grazed<-as.data.frame(BH2_ndvi_grazed)

BH_02_g<-tibble(
  Value=BH2_ndvi_grazed$layer,
  Treatment="Grazed"
)

BH_02_ug<-tibble(
  Value=BH2_ndvi_ungrazed$layer,
  Treatment= "Ungrazed"
)

BH_02<-rbind(BH_02_g, BH_02_ug)
ggplot(data=BH_02, aes(x=Treatment, y=Value))+
  geom_violin(scale='area')




#(4) LGH-01 ----------------------------------------------------------------------------

#LGH1_RGB<-stack("LGH1-RGB_1-20_modified.tif")
#LGH1_IR<-raster('LGH1-IR_1-20_modified.tif')


#LGH1- Align Extent -------------------------------

LGH1_IR_proj<-projectRaster(LGH1_IR, LGH1_RGB)

#Shapefile ----------------------------------------

plot(LGH1_IR_proj)                                                           #image1-one graph produced.
polygon<-shapefile('C:/Users/aalda/Desktop/All plots 2018/Polygons.shp')
plot(polygon, add=TRUE)                                                      #image2-image with boundaries plotted.
#poly<-readOGR('C:/Users/aalda/Desktop/All plots 2018/Polygons.shp')
#poly@data


LGH1_shp_ug<-subset(polygon, PlotID =='LGH1 UG')  # Ungrazed
LGH1_shp_g<-subset(polygon, PlotID == 'LGH1 G')   #Grazed

#LGH1 - Mask and clip raster to polygon ----------------

#LGH1: Ungrazed 

LGH1_RGB_mask<-mask(LGH1_RGB, LGH1_shp_ug)  
LGH1_IR_mask<-mask(LGH1_IR_proj, LGH1_shp_ug)  

LGH1_RGB_crop<-crop(LGH1_RGB_mask, LGH1_shp_ug)
plot(LGH1_RGB_crop)                                           #(Image-3: LGH1_RGB_crop_ungrazed). Four graphs produced.                             
hist(LGH1_RGB_crop)                                           #(Image-4: hist_LGH1_RGB_crop_ungrazed). Four histogram graphs produced.
ex<-extent(LGH1_RGB_crop)


LGH1_IR_crop<-crop(LGH1_IR_mask, ex)
plot(LGH1_IR_crop)                                            #image5-four graphs produced.

#LGH1: Stack and brick IR and RGB --------------------------

LGH1_stack<-stack(LGH1_RGB_crop, LGH1_IR_crop)
nlayers(LGH1_stack)

#LGH1: Calculate NDVI--------------------------------------

LGH1_ndvi_ungrazed<-((LGH1_stack[[5]]-LGH1_stack[[1]])/(LGH1_stack[[5]]+LGH1_stack[[1]]))
plot(LGH1_ndvi_ungrazed)                          #6-UPdate: plot_LGH1_ndvi_ungrazed. one graph produced.
hist(LGH1_ndvi_ungrazed)                          #7-Update: hist_LGH1_ndvi_ungrazed. One graph produced.



# Do it all again for LGH1 Grazed --------------------

#LGH1: Grazed

LGH1_RGB_mask<-mask(LGH1_RGB, LGH1_shp_g)
LGH1_IR_mask<-mask(LGH1_IR_proj, LGH1_shp_g)

LGH1_RGB_crop_g<-crop(LGH1_RGB_mask, LGH1_shp_g)
plot(LGH1_RGB_crop_g)                               #image8-Four graphs produced. 
hist(LGH1_RGB_crop_g)                               #image9: hist_LGH1_RGB_crop_grazed. Four graphs produced. 
ex<-extent(LGH1_RGB_crop_g)

LGH1_IR_crop_g<-crop(LGH1_IR_mask, ex)
plot(LGH1_IR_crop_g)                                #image9- one graph produced.

#LGH1: Stack and brick IR and RGB -----------------------

LGH1_stack_g<-stack(LGH1_RGB_crop_g, LGH1_IR_crop_g)
nlayers(LGH1_stack_g)                                #5 layers


#LGH1: Calculate NDVI for Grazed --------------------

LGH1_ndvi_grazed<-((LGH1_stack_g[[5]]-LGH1_stack_g[[1]])/(LGH1_stack_g[[5]]+LGH1_stack_g[[1]]))
plot(LGH1_ndvi_grazed)                              #(Image-10: LGH1_NDVI_grazed). One graph produced.
hist(LGH1_ndvi_grazed)                              #(Image-11: hist_LGH1_NDVI_grazed). One histogram produced.

#LGH1: Compare Grazed to Ungrazed -------------------

LGH1_ndvi_ungrazed<-as.data.frame(LGH1_ndvi_ungrazed)
LGH1_ndvi_grazed<-as.data.frame(LGH1_ndvi_grazed)

LGH_01_g<-tibble(
  Value= LGH1_ndvi_grazed$layer,
  Treatment='Grazed'
)

LGH_01_ug<-tibble(
  Value=LGH1_ndvi_ungrazed$layer,
  Treatment='Ungrazed'
)

LGH_01<-rbind(LGH_01_g, LGH_01_ug)
 ggplot(data=LGH_01, aes(x=Treatment, y=Value))+
 geom_violin(scale='area')                        #(Image-12: ggplot_LGH-01). One graph produced. Update name on google drive.





#(5) LGH-02 ---------------------------------------------------------


#LGH2_RGB<-stack("LGH-2 RGB_modified.tif")      
#LGH2_IR<-raster('LGH-2 IR_modified.tif')       


# Align Extent--------------------------

LGH2_IR_proj<-projectRaster(LGH2_IR, LGH2_RGB)


## Shapefile ----------------------------

plot(LGH2_IR_proj)                                      #image1-LGH2_IR_proj. Single graph produced. 
polygon<-shapefile("Polygons.shp")
plot(polygon, add=TRUE)                                 #image2-plot(polygon). Single graph of plot and area produced.

LGH2_shp_ug<-subset(polygon, PlotID=='LGH-2 UG')  #Ungrazed
LGH2_shp_g<-subset(polygon, PlotID == 'LGH-2 G')  # Grazed


## LGH2: Mask and clip raster to polygon ------------

##LGH2: Ungrazed 

LGH2RGB_mask<-mask(LGH2_RGB, LGH2_shp_ug)                 
LGH2_IR_mask<-mask(LGH2_IR_proj, LGH2_shp_ug)


LGH2RGB_crop<-crop(LGH2RGB_mask, LGH2_shp_ug)
plot(LGH2RGB_crop)                                       #(Image-3: LGH2RGB_crop_ungrazed). Four graph produced.
hist(LGH2RGB_crop)                                       #(Image-4: hist_LGH2_RGB_crop_ungrazed). Four graphs produced.
ex<-extent(LGH2RGB_crop)

LGH2IR_crop<-crop(LGH2_IR_mask, ex)
plot(LGH2IR_crop)                                         #(Image-5: LGH2IR_crop). One graph produced.
                                   

#LGH2: Stack and brick IR and RGB -----------------------

LGH2_stack<-stack(LGH2RGB_crop, LGH2IR_crop)
nlayers(LGH2_stack)


#LGH2: Calculate NDVI of the Ungrazed ------------------

LGH2_ndvi_ungrazed<-((LGH2_stack[[5]]-LGH2_stack[[1]])/(LGH2_stack[[5]]+LGH2_stack[[1]]))
plot(LGH2_ndvi_ungrazed)                                  #6-LGH2_ndvi_ungrazed image.
hist(LGH2_ndvi_ungrazed)                                  #7-LGH2_ndvi_ungrazed histogram.


# Do it all again for LGH2 Grazed ------------------------

#LGH2: Grazed 

LGH2_RGB_mask<-mask(LGH2_RGB, LGH2_shp_g)
LGH2_IR_mask<-mask(LGH2_IR_proj, LGH2_shp_g)

LGH2RGB_crop<-crop(LGH2_RGB_mask, LGH2_shp_g)
plot(LGH2RGB_crop)                                        #8-LGH2RGB_crop_grazed image. Four images produced.
ex<-extent(LGH2RGB_crop)

LGH2IR_crop<-crop(LGH2_IR_mask, ex)
plot(LGH2IR_crop)                                         #9-LGH2IR_crop image. Graph produced.


#LGH2: Stack and brick IR and RGB ----------------------

LGH2_stack<-stack(LGH2RGB_crop, LGH2IR_crop)
nlayers(LGH2_stack)


#LGH2: Calculate NDVI ----------------------------------

LGH2_ndvi_grazed<-((LGH2_stack[[5]]-LGH2_stack[[1]])/ (LGH2_stack[[5]]+LGH2_stack[[1]]))
plot(LGH2_ndvi_grazed)                                      #10-LgH2_ndvi_grazed image. One graph produced.
hist(LGH2_ndvi_grazed)                                      #11-hist_LGH2_ndvi_grazed. One graph.

#LGH2: Compare Grazed to Ungrazed -------------------

LGH2_ndvi_ungrazed<-as.data.frame(LGH2_ndvi_ungrazed)
LGH2_ndvi_grazed<-as.data.frame(LGH2_ndvi_grazed)

LGH_02_g<-tibble(
  Value= LGH2_ndvi_grazed$layer,
  Treatment='Grazed'
)

LGH_02_ug<-tibble(
  Value=LGH2_ndvi_ungrazed$layer,
  Treatment='Ungrazed'
)

LGH_02<-rbind(LGH_02_g, LGH_02_ug)
ggplot(data=LGH_02, aes(x=Treatment, y=Value))+
  geom_violin(scale='area')                             #12-ggplot_LGH_02.




#(6) LGL-01 ----------------------------------------------------------

#LGL1_RGB<-stack("LGL-1 RGB 1-20_modified.tif") 
#LGL1_IR<-raster('LG-L1 IR 1-20_modified.tif')  


# Align Extent--------------------------

LGL1_IR_proj<-projectRaster(LGL1_IR, LGL1_RGB)


# Shapefile ----------------------------

plot(LGL1_IR_proj)                                  #1-LGL1_IR_proj. Single graph produced.
polygon<-shapefile("Polygons.shp")
plot(polygon, add=TRUE)                              #2-plot(polygon). Single graph produced.

LGL1_shp_ug<-subset(polygon, PlotID=='LGL1 UG')  #Ungrazed
LGL1_shp_g<-subset(polygon, PlotID == 'LGL-1 G')  # Grazed


#LGL1: Mask and clip raster to polygon ------------

#LGL1: Ungrazed 

LGL1_RGB_mask_ug<-mask(LGL1_RGB, LGL1_shp_ug)                    
LGL1_IR_mask<-mask(LGL1_IR_proj, LGL1_shp_ug)                 


LGL1_RGB_crop_ug<-crop(LGL1_RGB_mask_ug, LGL1_shp_ug)
plot(LGL1_RGB_crop_ug)                                    #(Image-: LGL1-RGB_crop_ug). Four graphs produced.
hist(LGL1_RGB_crop_ug)                                    #(Image-4:hist_LGL1_RGB_crop_ug). Four graphs produced.
ex<-extent(LGL1_RGB_crop_ug)

LGL1_IR_crop_ug<-crop(LGL1_IR_mask, ex)
plot(LGL1_IR_crop_ug)                           #5-LGL1-IR_crop_ug. Single graph produced. *Issue: only a portion of area is displayed. 
#plot(LGL1_IR_crop_ug, ylim=c(38.18600, 38.18750), xlim=c(-122.9809, -122.9608))   


#LGL1: Stack and brick IR and RGB -----------------------

LGL1_stack_ug<-stack(LGL1_RGB_crop_ug, LGL1_IR_crop_ug)
nlayers(LGL1_stack_ug)


#LGL1: Calculate NDVI of the Ungrazed ------------------

LGL1_ndvi_ungrazed<-((LGL1_stack_ug[[5]]-LGL1_stack_ug[[1]])/(LGL1_stack_ug[[5]]+LGL1_stack_ug[[1]]))
plot(LGL1_ndvi_ungrazed)             #6-LGL1_ndvi_ungrased. Single graph produced.  
hist(LGL1_ndvi_ungrazed)             #7-histogram: LGL1_ndvi_ungrazed.

# Do it all again for LGH2 Grazed ------------------------

#LGL1: Grazed 

LGL1RGB_mask_g<-mask(LGL1_RGB, LGL1_shp_g)
LGL1IR_mask_g<-mask(LGL1_IR_proj, LGL1_shp_g)

LGL1RGB_crop_g<-crop(LGL1RGB_mask_g, LGL1_shp_g)       
plot(LGL1RGB_crop_g)                                     #8-LGL1RGB_crop_g. Four graphs produced. 
hist(LGL1RGB_crop_g)                                      #9-hist_LGL1RGB_crop_grazed. Four graphs produced.
ex<-extent(LGL1RGB_crop_g)

LGL1IR_crop_g<-crop(LGL1IR_mask_g, ex)
plot(LGL1IR_crop_g)    #10-LGL1IR_crop_g. One graph produced.
#plot(LGL1IR_crop_g, ylim=c(38.18600, 38.18650), xlim=c(-122.9730, -122.9600))   
#extent(ex)

#LGL1: Stack and brick IR and RGB ----------------------

LGL1_stack<-stack(LGL1RGB_crop_g, LGL1IR_crop_g)
nlayers(LGL1_stack)


#LGL1: Calculate NDVI ----------------------------------

LGL1_ndvi_grazed<-((LGL1_stack[[5]]-LGL1_stack[[1]])/ (LGL1_stack[[5]]+LGL1_stack[[1]]))
plot(LGL1_ndvi_grazed)                       #11-LGL1_ndvi_grazed. One graph produced. 
hist(LGL1_ndvi_grazed)                       #12-hist_LGL1_ndvi_grazed. 

#LGL1: Compare Grazed to Ungrazed -------------------

LGL1_ndvi_ungrazed<-as.data.frame(LGL1_ndvi_ungrazed)
LGL1_ndvi_grazed<-as.data.frame(LGL1_ndvi_grazed)

LGL_01_g<-tibble(
  Value= LGL1_ndvi_grazed$layer,
  Treatment='Grazed'
)

LGL_01_ug<-tibble(
  Value=LGL1_ndvi_ungrazed$layer,
  Treatment='Ungrazed'
)

LGL_01<-rbind(LGL_01_g, LGL_01_ug)                
ggplot(data=LGL_01, aes(x=Treatment, y=Value))+
  geom_violin(scale='area')                       #13-update-ggplot_LGL_01. 






#(7) LGL-02 ----------------------------------------------------------

#LGL2_RGB<-stack("LGL-2-RGB_modified_2.tif")    
#LGL2_IR<-raster('LGL-2 IR-modified.tif')       


# Align Extent--------------------------

LGL2_IR_proj<-projectRaster(LGL2_IR, LGL2_RGB)


# Shapefile ----------------------------

plot(LGL2_IR_proj)                                     #`1-LGL2_IR_proj. Single graph produced. 
polygon<-shapefile("Polygons.shp")
plot(polygon, add=TRUE)                                #2-image of plot and boundary produced. 

LGL2_shp_ug<-subset(polygon, PlotID=='LGL2 UG')  #Ungrazed
LGL2_shp_g<-subset(polygon, PlotID == 'LGL-2 G')  # Grazed


# LGL2: Mask and clip raster to polygon ------------

#LGL2: Ungrazed 

LGL2_RGB_mask<-mask(LGL2_RGB, LGL2_shp_ug)          
LGL2_IR_mask<-mask(LGL2_IR_proj, LGL2_shp_ug)


LGL2RGB_crop<-crop(LGL2_RGB_mask, LGL2_shp_ug)
plot(LGL2RGB_crop)                                  #3-LGL2RGB_crop_ungrazed. Four graphs produced. 
hist(LGL2RGB_crop)                                  #4-hist_LGL2RGB_crop_ungrazed. Four graphs produced. 
ex<-extent(LGL2RGB_crop)

LGL2IR_crop<-crop(LGL2_IR_mask, ex)
plot(LGL2IR_crop)                                   #5-update: LGL2IR_crop.


####LGL2: Stack and brick IR and RGB -----------------------

LGL2_stack<-stack(LGL2RGB_crop, LGL2IR_crop)
nlayers(LGL2_stack)


#LGL2: Calculate NDVI of the Ungrazed ------------------

LGL2_ndvi_ungrazed<-((LGL2_stack[[5]]-LGL2_stack[[1]])/(LGL2_stack[[5]]+LGL2_stack[[1]]))
plot(LGL2_ndvi_ungrazed)             #6-LGL_ndvi_ungrazed.
hist(LGL2_ndvi_ungrazed)            #7-update: hist_LGL2_ndvi_ungrazed.


# Do it all again for LGH2 Grazed ------------------------

#LGL2: Grazed 

LGL2_RGB_mask<-mask(LGL2_RGB, LGL2_shp_g)
LGL2_IR_mask<-mask(LGL2_IR_proj, LGL2_shp_g)

LGL2RGB_crop<-crop(LGL2_RGB_mask, LGL2_shp_g)
plot(LGL2RGB_crop)                                #8-LGL2RGB_crop. Four graphs produced.
hist(LGL2RGB_crop)                                #9-hist_LGL2RGB_crop_grazed. Four graphs produced.
ex<-extent(LGL2RGB_crop)

LGL2IR_crop<-crop(LGL2_IR_mask, ex)
plot(LGL2IR_crop)                                 #10-LGL2IR_crop. One graph produced. 


#LGL2: Stack and brick IR and RGB ----------------------

LGL2_stack<-stack(LGL2RGB_crop, LGL2IR_crop)
nlayers(LGL2_stack)


#LGL2: Calculate NDVI ----------------------------------

LGL2_ndvi_grazed<-((LGL2_stack[[5]]-LGL2_stack[[1]])/ (LGL2_stack[[5]]+LGL2_stack[[1]]))
plot(LGL2_ndvi_grazed)                            #11-LGL2_ndvi_grazed image. One graph produced.
hist(LGL2_ndvi_grazed)                            #12-hist_LGL2_ndvi_grazed. One histogram produced.

#LGL2: Compare Grazed to Ungrazed -------------------

LGL2_ndvi_ungrazed<-as.data.frame(LGL2_ndvi_ungrazed)
LGL2_ndvi_grazed<-as.data.frame(LGL2_ndvi_grazed)

LGL_02_g<-tibble(
  Value= LGL2_ndvi_grazed$layer,
  Treatment='Grazed'
)

LGL_02_ug<-tibble(
  Value=LGL2_ndvi_ungrazed$layer,
  Treatment='Ungrazed'
)

LGL_02<-rbind(LGL_02_g, LGL_02_ug)
ggplot(data=LGL_02, aes(x=Treatment, y=Value))+
  geom_violin(scale='area')                #13-ggplot_LGL_02. One graph produced.





#(8) OGH-01 ----------------------------------------------------------


#OGH1_RGB<-stack("OGH-1 RGB2_modified.tif")     
#OGH1_IR<-raster('OG-H2 IR_modified.tif')       

# Align Extent--------------------------

OGH1_IR_proj<-projectRaster(OGH1_IR, OGH1_RGB)


# Shapefile ----------------------------

plot(OGH1_IR_proj)                                 #1-OGH1_IR_proj image. Graph produced
polygon<-shapefile("Polygons.shp")           
plot(polygon, add=TRUE)                         #2-OGH1 plot-polygon image produced.  

OGH1_shp_ug<-subset(polygon, PlotID=='OGH1 UG')  #Ungrazed
OGH1_shp_g<-subset(polygon, PlotID == 'OGH1 G')  # Grazed


# OGH1: Mask and clip raster to polygon ------------

#OGH1: Ungrazed 

OGH1_RGB_mask<-mask(OGH1_RGB, OGH1_shp_ug)            
OGH1_IR_mask<-mask(OGH1_IR_proj, OGH1_shp_ug)

OGH1RGB_crop<-crop(OGH1_RGB_mask, OGH1_shp_ug)
plot(OGH1RGB_crop)                                 #3-OGH1RGB_crop_grazed image. Four graph produced.
hist(OGH1RGB_crop)                                 #4-hist_OGH1RGB_crop_grazed image. Four graphs produced.
ex<-extent(OGH1RGB_crop)

OGH1IR_crop<-crop(OGH1_IR_mask, ex)
plot(OGH1IR_crop)                                 #5-update:plot_OGH1IR_crop_ungrazed. One graph produced.


#OGH1: Stack and brick IR and RGB -----------------------

OGH1_stack<-stack(OGH1RGB_crop, OGH1IR_crop)
nlayers(OGH1_stack)


#OGH1: Calculate NDVI of the Ungrazed ------------------

OGH1_ndvi_ungrazed<-((OGH1_stack[[5]]-OGH1_stack[[1]])/(OGH1_stack[[5]]+OGH1_stack[[1]]))
plot(OGH1_ndvi_ungrazed)                            #6-Update: OGH1_ndvi_ungrased. One graph produced. 
hist(OGH1_ndvi_ungrazed)                            #7-Update: hist_OGH1_ndvi_ungrazed. 


# Do it all again for OGH1 Grazed ------------------------

#OGH1: Grazed 

OGH1_RGB_mask<-mask(OGH1_RGB, OGH1_shp_g)
OGH1_IR_mask<-mask(OGH1_IR_proj, OGH1_shp_g)

OGH1RGB_crop<-crop(OGH1_RGB_mask, OGH1_shp_g)
plot(OGH1RGB_crop)                                #8-OGH1RGB_crop image. Four graphs produced. 
hist(OGH1RGB_crop)                                #9-hist_OGH1RGB_crop_grazed. Four graphs produced. 
ex<-extent(OGH1RGB_crop)

OGH1IR_crop<-crop(OGH1_IR_mask, ex)
plot(OGH1IR_crop)                                 #10-OGH1IR_crop image. One graph produced.


#OGH1: Stack and brick IR and RGB ----------------------

OGH1_stack<-stack(OGH1RGB_crop, OGH1IR_crop)
nlayers(OGH1_stack)


#OGH1: Calculate NDVI ----------------------------------

OGH1_ndvi_grazed<-((OGH1_stack[[5]]-OGH1_stack[[1]])/ (OGH1_stack[[5]]+OGH1_stack[[1]]))
plot(OGH1_ndvi_grazed)                        #11-OGH1_ndvi_grazed image. One graph produced. 
hist(OGH1_ndvi_grazed)                        #12-hist_OGH1_ndvi_grazed image. One histogram produced.

#OGH1: Compare Grazed to Ungrazed -------------------

OGH1_ndvi_ungrazed<-as.data.frame(OGH1_ndvi_ungrazed)
OGH1_ndvi_grazed<-as.data.frame(OGH1_ndvi_grazed)

OGH_01_g<-tibble(
  Value= OGH1_ndvi_grazed$layer,
  Treatment='Grazed'
)

OGH_01_ug<-tibble(
  Value=OGH1_ndvi_ungrazed$layer,
  Treatment='Ungrazed'
)

OGH_01<-rbind(OGH_01_g, OGH_01_ug)
ggplot(data=OGH_01, aes(x=Treatment, y=Value))+
  geom_violin(scale='area')                #13-ggplot_OGH-01 image.





#(9) OGH-02 ----------------------------------------------------------



#OGH2_RGB<-stack("OG-H2 RGB_modified.tif")      
#OGH2_IR<-raster('OG-H2 IR_modified.tif')       


# Align Extent--------------------------

OGH2_IR_proj<-projectRaster(OGH2_IR, OGH2_RGB)


# Shapefile ----------------------------

plot(OGH2_IR_proj)                                    #1-OGH2_IR_proj image. One graph produced.
polygon<-shapefile("Polygons.shp")
plot(polygon, add=TRUE)                               #2-OGH2-plot_polygons image.

OGH2_shp_ug<-subset(polygon, PlotID=='OGH2 UG')  #Ungrazed
OGH2_shp_g<-subset(polygon, PlotID == 'OGH2 G')  # Grazed


#OGH2: Mask and clip raster to polygon -------------

#OGH2: Ungrazed 

OGH2_RGB_mask<-mask(OGH2_RGB, OGH2_shp_ug)                
OGH2_IR_mask<-mask(OGH2_IR_proj, OGH2_shp_ug)

OGH2RGB_crop<-crop(OGH2_RGB_mask, OGH2_shp_ug)
plot(OGH2RGB_crop)                                       #3-OGH2RGB_crop image. Four graphs produced.
hist(OGH2RGB_crop)                                       #4-hist_OGH2RGb_crop_ungrazed. Four graphs produced.
ex<-extent(OGH2RGB_crop)

OGH2IR_crop<-crop(OGH2_IR_mask, ex)
plot(OGH2IR_crop)                                         #5-OGH2IR_crop-ungrazed. One graph produced. 


#OGH2: Stack and brick IR and RGB -----------------------

OGH2_stack<-stack(OGH2RGB_crop, OGH2IR_crop)
nlayers(OGH2_stack)


#OGH2: Calculate NDVI of the Ungrazed ------------------

OGH2_ndvi_ungrazed<-((OGH2_stack[[5]]-OGH2_stack[[1]])/(OGH2_stack[[5]]+OGH2_stack[[1]]))
plot(OGH2_ndvi_ungrazed)                                #6-OGH2_ndvi_ungrazed image not produced.**Issue: Graph not produced.                           
hist(OGH2_ndvi_ungrazed)                                #7-OGH2_ndvi_ungrazed histogram image. **Issue:Error in hist.default(v, main = main, plot = plot, ...):invalid number of 'breaks'


# Do it all again for LGH2 Grazed ------------------------

#OGH2: Grazed 

OGH2_RGB_mask<-mask(OGH2_RGB, OGH2_shp_g)
OGH2_IR_mask<-mask(OGH2_IR_proj, OGH2_shp_g)

OGH2RGB_crop<-crop(OGH2_RGB_mask, OGH2_shp_g)
plot(OGH2RGB_crop)                                    #8-OGH2RGB_crop image. Four graph produced.
hist(OGH2RGB_crop)                                    #9-hist_OGH2RGB_crop_grazed. Four graphs produced.
ex<-extent(OGH2RGB_crop)

OGH2IR_crop<-crop(OGH2_IR_mask, ex)
plot(OGH2IR_crop)                                     #10-OGH2IR-crop image. One graph produced.                                      #9-new: hist_OGH2IR_crop. upload on google drive.

#OGH2: Stack and brick IR and RGB ----------------------

OGH2_stack<-stack(OGH2RGB_crop, OGH2IR_crop)
nlayers(OGH2_stack)


#OGH2: Calculate NDVI ----------------------------------

OGH2_ndvi_grazed<-((OGH2_stack[[5]]-OGH2_stack[[1]])/ (OGH2_stack[[5]]+OGH2_stack[[1]]))
plot(OGH2_ndvi_grazed)                                #11OGH2_ndvi_grazed image. One graph produced.
hist(OGH2_ndvi_grazed)                                #12hist_OGH2_ndvi_grazed. 

#OGH2: Compare Grazed to Ungrazed -------------------

OGH2_ndvi_ungrazed<-as.data.frame(OGH2_ndvi_ungrazed)
OGH2_ndvi_grazed<-as.data.frame(OGH2_ndvi_grazed)

OGH_02_g<-tibble(
  Value= OGH2_ndvi_grazed$layer,
  Treatment='Grazed'
)

OGH_02_ug<-tibble(
  Value=OGH2_ndvi_ungrazed$layer,
  Treatment='Ungrazed'
)

OGH_02<-rbind(OGH_02_g, OGH_02_ug)
ggplot(data=OGH_02, aes(x=Treatment, y=Value))+
  geom_violin(scale='area')                      #13: ggplot_OGH-02 image. 



