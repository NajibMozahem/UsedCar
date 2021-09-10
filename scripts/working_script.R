library(tidyverse)

## get data set file names
file_names <- list.files("dataset")
file_names <- file_names[file_names != "unclean cclass.csv" & file_names != "unclean focus.csv"]

## read the csv files

get <- sapply(file_names, function(x){
  get1 <- read_csv(paste0("dataset/", x))
  car_type <- strsplit(x, "[.]")[[1]][1]
  get1 %>% mutate(car_type = car_type)
})

do.call(rbind, get)
