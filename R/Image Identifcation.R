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
im <- load.image(completions$img[[2]])
#Plot original picture to see what we shuold be masking
plot(im)
# get all of the labeled masks
mask <- completions$annotations[[2]] %>%
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
#Gives our bbox, will this suffice? 
library(raster)
im2 <- nn_res
px <- im2 > .01
bbox <- crop.bbox(im2,px)
plot(bbox)

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
