library(jsonlite)
library(tidyverse)
library(purrr)
library(nabor)
#Get Task list
tasks <- read_json("inst/tasks.json")
tasks_df <- tibble(
  id = purrr::map_int(tasks, "id"),
  img = purrr::map_chr(purrr::map(tasks, "data"), "image")
)

results <- read_json("inst/results.json")
# Get some basic data
completions <- tibble(
  id = purrr::map_int(results, "id"),
  img = purrr::map_chr(purrr::map(results, "data"), "image"),
  annotations = purrr::map(results, "completions")
)

#Start with the first image
library(imager)
im <- load.image(completions$img[[1]])


completions$annotations[[1]][[1]]$result[[1]]$value$points
# Need to get the rest of the stuff
#completions$annotations[[1]][[1]]$result[[1]]$value %>%
# discard("value")
#drop(, matches("value"))
#flatten(drop(completions$annotations[[1]][[1]]$result[[1]]$value))
labs <- tibble(
  label =  pluck(completions$annotations[[1]][[1]]$result[[1]]$value, "polygonlables"),
  points = list(purrr::map_df(completions$annotations[[1]][[1]]$result[[1]]$value$points, ~tibble(x= .[[1]], y = .[[2]])))
) %>%
  unnest(label)
plot(im)
points(labs$points[[1]] * c(dim(im)[1:2])/100, col="red")
#Fill in the color of shoe according to the points
shoe.new <- bucketfill(im, x=1000, y=500, color='green',sigma=.2)
plot(shoe.new)