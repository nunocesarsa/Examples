
#clips image given a number of pixels and saves the output to a file
#notice for now does not pad the data

#foreach
#adaptation from: https://www.gis-blog.com/increasing-the-speed-of-raster-processing-with-r-part-23-parallelisation/


library(raster)
library(rgeos)

library(stringr) # for padding strings

###### clipping function based on extent and resolution
res(bb)[1]


clipper <- function(rst.in,n_cells){
  
  #geographic extent
  e = as.vector(extent(rst.in))
  
  #loop terminators
  x_min = e[1]
  x_max = e[2]
  y_min = e[3]
  y_max = e[4]
  
  
  #steps in geographic distance
  #displacement = n_cells*resolution
  displacement = n_cells*res(rst.in)[1]
  
  #fetch total rows
  #total_row = nrow(rst.in)
  #total_col = ncol(rst.in)
  
  #The logic is to use the crop function from the raster package. For this, i need to provide an extent object that is used to crop the image
  #I will use N_cells as the indicator of the sides length and point to the center of a putative square
  #and thus the extent is easy calculate e.g.: xl = x_mid -  n_cells/2 
  
  #i will use while terminator (which i dread!) but i could use an if because the images have all the same sizes
  #starting top left, i will loop rightwards and downwards
  #Starting on top left
  
  #list object to store the pointers
  out.list = list()
  i=1
  
  y_n = y_max - displacement/2 #substract hafl distance to topside 
  
  while (y_n > y_min){
    #while my position is still bigger that the minimum possible on the y axis, do:
    
    #set yourself up on the left side of the image
    x_n = x_min + displacement/2 #add half distance to right side
    
    while (x_n < x_max){
      #while my position is still smaller that the maximum possible on the x axis, do:
      
      #these could straight be put on the extent object for clarity i explicitly give them a line
      #displacements in X
      x_l = x_n - displacement /2
      x_r = x_n + displacement /2
      #displacements in y
      y_d = y_n - displacement /2 
      y_u = y_n + displacement /2
      
      #store and step on the list
      out.list[[i]] = extent(x_l,x_r,y_d,y_u)
      i=i+1
      
      #move on X - notice that i am moving to the CENTER of the next square on the x axis, so, displacement is full
      x_n = x_n + displacement
      
    }
    
    #move on y - notice that i am moving to the CENTER of the next square on the y axis, so, displacement is full
    y_n = y_n - displacement
    
  }
  return(out.list)
}


#select file list
file.list = list.files('D:/ClippingExample/InputData/',full.names=T)
name.list = list.files('D:/ClippingExample/InputData/')

file.list
name.list

#testing clipper
bb = raster(file.list[1])
cc = clipper(bb,500)
length(cc)

#preparing for parallel processing
library(doParallel)
library(foreach)
library(parallel)
detectCores()

#setting up my available cores
UseCores <- 8 # i want to do other stuff at the same time...
cl       <- makeCluster(UseCores)
registerDoParallel(cl)

#creating an extent object and exploring the outputs
list_extents = clipper(bb,256)
list_extents[[1]]
length(list_extents)

#loading target raster
tgt.rst = stack(file.list)

#for each item in the dictionary
foreach(i=1:length(list_extents )) %dopar% {

  #packages might need to be loaded inside.. i reckon
  library(raster)
  library(rgeos)
  library(stringr) # for padding strings
  
  #loading ith extent
  tmp_extent = list_extents[[i]]
  
  #creting an outputname
  outname = paste('D:/ClippingExample/OutputData/',
                  str_pad(i,4,side=c("left"),pad = 0),
                  "_",
                  name.list,sep="")
  
  #cropping
  rst_crop = crop(tgt.rst,
                  tmp_extent)
  
  #this functions pads 0's to the output image to ensure the size fits
  extend(rst_crop,
         tmp_extent,
         outname,
         options=c("COMPRESS=LZW"),
         overwrite=T)

  
}


stopCluster(cl)
gc()

