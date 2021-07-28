library(jsonlite)
library(tidyverse)
library(purrr)
library(magrittr)
library(dplyr)

#Get Task list
tasks <- read_json("inst/tasks.json")
tasks_df <- tibble(
  id = purrr::map_int(tasks, "id"),
  img = purrr::map_chr(purrr::map(tasks, "data"), "image")
)

results <- read_json("inst/result.json")
# Get some basic data
completions <- tibble(
  id = purrr::map_int(results, "id"),
  img = purrr::map_chr(purrr::map(results, "data"), "image"),
  annotations = purrr::map(results, "completions")
)

#Start with the first image
library(imager)


# Extract the points and labels
get_labeled_points <- function(annotation) {
  # Pull out the annotation result object
  tmp <- annotation[[1]]$result
  
  # For each result, pull out label and points
  map_df(tmp, ~tibble(
    label =  pluck(., "value", "polygonlabels"),
    points = list(purrr::map_df(.$value$points, 
                                ~tibble(x= .[[1]], y = .[[2]])))
  ) %>%
    # label is a list by default, so unlist it
    unnest(label)
  )
}


create_mask <- function(points, imdim) {
  # Get dimensions so that we can be sure what lines up with what
  width <- imdim[1]
  height <- imdim[2]

  points <- points %>%
    # convert to relative coordinates [0,1] scale
    mutate(xpct = x, ypct = y,
           x = x / 100,
           y = y / 100)
  
  # This function takes points and returns a mask
  xROI::rasterizeROI(cbind(points$y, points$x), c(height, width)) %>%
    # convert to an image
    imager::as.cimg() %>%
    # mirror to deal with funky coordinate systems
    imager::mirror("x") %>%
    # Convert to 0-1 image (pixset)
    imager::as.pixset()
}

create_labeled_masks <- function(labeled_points, imdim = dim(image), image = NULL) {
  if (is.null(imdim) & is.null(image)) stop("Provide either dimensions or an image")
  
  labeled_points %>%
    # Create a mask for each label
    mutate(mask = purrr::map(points, create_mask, imdim = imdim))
}

mask_image <- function(mask, im) {
  # Censor the image using the masks
  y <- map(mask$mask, function(x) {
    xx <- im
    xx[x] <- 1
    return(xx)
  }) %>% as.imlist()
  
  # Add labels to the image pieces
  names(y) <- mask$label
  y
}

# Get the image
im <- load.image(completions$img[[1]])
#Plot original picture to see what we shuold be masking
plot(im)
# get all of the labeled masks
mask <- completions$annotations[[1]] %>%
  get_labeled_points() %>%
  create_labeled_masks(imdim = dim(im)) 

# get image pieces corresponding to the masks
labels <- mask_image(mask, im)

# Plot
plot(labels)


# To convert to useful data
# Define unlabeled = junk = 0
#        shoe-contact = useful = 1
#        shoe-non-contact = usefulish = 2
create_nn_data <- function(imdim, mask_df) {
  # Initialize assuming everything is junk
  res <- array(0, dim = imdim) %>% 
    as.cimg()
  # Go through the list of masks 
  for (i in 1:nrow(mask_df)) {
    value <- ifelse(mask_df$label[i] == "Shoe Sole (contact)", 1, 2)
    res[!mask_df$mask[[i]]] <- value
  }
  
  return(res)
}


nn_res <- create_nn_data(dim(im), mask)
plot(nn_res)
range(nn_res) 

BBOX_df <- function(df){
  RNN_df <- tibble(
    meta_id = purrr::map_int(df, "id"),
    image_https = purrr::map_chr(purrr::map(df, "data"), "image"),
    data = purrr::map(df, "completions")
  ) %>%
    mutate(poly_data = purrr::map(data, ~map(.[[1]]$result, "value"))) %>%
    unnest(poly_data) %>%
    mutate(polygon_points = purrr::map(poly_data, ~.$points %>% 
                                         map_dfr(~rbind(.) %>% 
                                                   set_names(c("x", "y")))),
           polygon_labels = purrr::map(poly_data, "polygonlabels")) %>%
    mutate(x = map(polygon_points, "x"),
           y = map(polygon_points, "y"),
           x_min = map_dbl(x,min),
           x_max = map_dbl(x, max),
           y_min = map_dbl(y,min),
           y_max = map_dbl(y, max)) %>% 
    select(-data, -poly_data)
  return(RNN_df)
}

head(finally, 10)
boundingbox <- finally$polygon_points
head(boundingbox)
plot(finally[1])










#My shot at the BBox plus padding/rotation/crop
library(raster)
library(shotGroups)
im2 <- nn_res
px <- im > .01
bbox <- crop.bbox(im2,px)
plot(bbox)
padded_bbox <- pad(bbox, 100, "xy") %>% plot()
rotate <- imrotate((bbox), 30) %>% plot()
autocrop(rotate) %>% plot()
#This is great and all but we need to be able to get the bbox for just the shoes features that make contact
#Using getMinBBox cuz mine I don't think will work
xy <- mask_df
bb <- getMinBBox(xy)

#This is the data augmentation part. (i.e. rotate and crop out the image from the bbox)
#Add pointless padding
padded <- pad(boats,30,"xy")
plot(padded)
#Remove padding
autocrop(padded) %>% plot()
#You can specify the colour if need be
autocrop(padded,"black") %>% plot
#autocrop has a zero-tolerance policy: if a pixel value is slightly different from the one you gave
#the pixel won't get cropped. A fix is to do a bucket fill first
padded <- isoblur(padded,10)
autocrop(padded) %>% plot()
padded2 <- bucketfill(padded,1,1,col=c(0,0,0),sigma=.1)
plot(padded2)
autocrop(padded2) %>% plot()


#Other examples
library(magick)
og_img <-image_read(im2)
plot(og_img)
image_trim(og_img)
frink <- image_read("https://jeroen.github.io/images/frink.png")
print(frink)
image_border(image_background(frink, "hotpink"), "#000080", "20x10")
image_trim(frink)
og_img <- image_read(im) %>% plot()
image_trim(og_img) %>% plot()
image_trim(im)
trim(bbox, padding=0)
extent(bbox)
imappend(list(im, bbox), "x") %>% plot
#Now let's pull part of the image that we want. Essentially a small box that
#contains the features of the shoe
imappend(list(boats,boats),"x") %>% plot
imappend(list(boats,boats),"y") %>% plot
purrr::map(1:3, ~imnoise(100,100)) %>% imappend("c") %>% plot
boats.gs <- grayscale(boats)
purrr::map(seq(1,5,l=3),function(v) isoblur(boats.gs,v)) %>% imappend("c") %>% plot
#imappend also works on pixsets
imsplit(boats > .5,"c") %>% imappend("x") %>% plot

##Could make a function giving us the minbbox, not sure how to do this tbh
# Create_bounding_box <- function() {
#   #convert image to an matrix where we can get the bbox
#   bb <- nn_res %>% as.matrix(nn_res)
# }


# coordinates given by a suitable data frame

# library(shotGroups)
# points <- points %>%
#   # convert to relative coordinates [0,1] scale
#   mutate(xpct = x, ypct = y,
#          x = x / 100,
#          y = y / 100)
# xy <- cbind(points$y, points$x) %>% as.matrix(xy)
# bb <- getMinBBox(nn_res)               # minimum bounding box
# 
# # plot points and minimum bounding box
# plot(point.y ~ point.x, data=mask, asp=1,
#      xlim=range(bb$points[ , 1]), ylim=range(bb$points[ , 2]), pch=16)
# drawBox2(bb, fg='blue', colCtr='blue', pch=4, cex=2)
# 
# bb$FoM                                   # figure of merit
# bb$angle                                 # box orientation
# 
# # coordinates given by a matrix
# ## Not run: 
# xy <- matrix(round(rnorm(16, 100, 15)), ncol=2)
# getMinBBox(xy)
# 
# ## End(Not run)
# 
