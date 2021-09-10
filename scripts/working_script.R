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

length <- length(get)
hold <- rep(FALSE, length)

for (i in 1:length) {
  if (dim(get[[i]])[2] == 10) {
    hold[i] = TRUE
  }
}

## get only the data that has 10 columns
get2 <- get[hold]

##combine
do.call(rbind, get2)
## we get an error that some columns have different names in different files.
## we print the column names of each dataset
for (i in get2) {
  print(colnames(i))
}
## notice that there is a column named tax in all cases except in one instance
## where it the name includes parantheses and currency. We need to rename the
## column. First get the names from the first data frame in the list, then apply
## the names to all data frames in the list
column_names <- colnames(get2[[1]])
for (i in 1:length(get2)) {
  colnames(get2[[i]]) <- column_names
}
## now combine again and save as data set
the_data <- do.call(rbind, get2)
