library(jsonlite)
library(tidyverse)
library(purrr)
library(nabor) # bucketfill function

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


# Get the image
im <- load.image(completions$img[[1]])

# get all of the labeled masks
mask <- completions$annotations[[1]] %>%
  get_labeled_points() %>%
  create_labeled_masks(imdim = dim(im)) 

# Censor the image using the masks
labels <- map(mask$mask, function(x) {
  xx <- im
  xx[x] <- 1
  return(xx)
}) %>% as.imlist()

# Add labels to the image pieces
names(labels) <- mask$label

# Plot
plot(labels)
