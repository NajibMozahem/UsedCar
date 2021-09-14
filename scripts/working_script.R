packages <- c("tidyverse", "caret", "LiblineaR")

lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }  
})

## get data set file names
file_names <- list.files("dataset")
file_names <- file_names[file_names != "unclean cclass.csv" & file_names != "unclean focus.csv"]

## read the csv files

get <- sapply(file_names, function(x){
  get1 <- read_csv(paste0("dataset/", x))
  car_type <- strsplit(x, "[.]")[[1]][1]
  get1 %>% mutate(car_type = car_type)  
  
})

## combine
do.call(rbind, get)
## we get an error message telling us that there are different
## number of columns in different data frames. If we look at the
## data frames we see that most have 10 columns while a few
## have eight columns. We eliminate the frames that do not have
## ten column so that we can have a homogeneous data set

length <- length(get)
hold <- rep(FALSE, length)
for (i in 1:length) {
  if (dim(get[[i]])[2] == 10) {
    hold[i] = TRUE
  }
}

## get only the data frames that have 10 columns
get2 <- get[hold]

##combine
do.call(rbind, get2)
## we get an error that some columns have different names in different files.
## we print the column names of each data set
for (i in get2) {
  print(colnames(i))
}
## notice that there is a column named tax in all cases except in one instance
## where it the name includes parentheses and currency. We need to rename the
## column. First get the names from the first data frame in the list, then apply
## the names to all data frames in the list
column_names <- colnames(get2[[1]])
for (i in 1:length(get2)) {
  colnames(get2[[i]]) <- column_names
}
## now combine again and save as data set
the_data <- do.call(rbind, get2)

## check for na values
colSums(is.na(the_data))
## no na values, which is good

## have a look at the structure
str(the_data)
## there are character variables that are better off as factors.
## we will convert them soon. First, lets us look at summary
## statistics for variables
summary(the_data)
## we see that the maximum year is 2060. Something is wrong.
## we should take a closer look
table(the_data$year)
## we see that there is one instance where the year is 2060.
## it is probably a typing error where the year is supposed
## to be 2006. To be sure, we remove this record
the_data <- the_data[the_data$year < 2060, ]

## now convert character variables to factors
the_data <- the_data %>% mutate(across(where(is.character), as.factor))

## look at the values of price
ggplot(the_data) + geom_histogram(aes(price), fill = "blue")
## we see that the variable is skewed. Perhaps it would be better
## to take the log scale
ggplot(the_data) + geom_histogram(aes(price), fill = "blue") + 
  scale_x_log10()

## look at the frequency of car types in the data set
the_data %>% group_by(car_type) %>% mutate(n = n()) %>% 
  ggplot() + geom_bar(aes(reorder(car_type, n)), fill = "blue") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

## let us look at the percentage of each car type in terms of
## year. This wil help us understand whether the data set
## contains some car types that are newer than other car types
ggplot(the_data) + geom_bar(aes(car_type, fill = as.factor(year)),
                            position = "fill") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
## it seems that the distribution is similar in all car types

## now do the same but for fuel type
ggplot(the_data) + geom_bar(aes(car_type, fill = fuelType),
                            position = "fill") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
## here we see differences, with Toyota having more hybrid cars
## we also see variation when comparing diesel and petrol.

## now the transmission
ggplot(the_data) + geom_bar(aes(car_type, fill = transmission),
                            position = "fill") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
## again we see variations. Audi for example does not have
## a ot of manual compared to ford. These differences are
## probably reflected in different prices for the models.
## this is why it is important to have an idea about these
## differences

## we now look at the differences in price when taking into 
## account the car type and the year. To do this, we calculate
## the average price for each car type in each year and we
## plot the results:

the_data %>% 
  group_by(car_type, year) %>% 
  summarise(price_avg = mean(price, na.rm = TRUE)) %>% 
  ggplot() + 
  geom_raster(aes(car_type, as.factor(year), 
                  fill = price_avg)) + 
  ylab("") + xlab("") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.text.y = element_text(size =6))
## the plot shows that the most expensive cars are audi, bmw,
## and mercedes (recent models). An interesting finding
## revealed by the graph is that there is a significant change
## in price as these three cars get older, while in the case of
## ford and hyundai for example, the change in the gradient
## as we move from year to year is much more subtle. This
## indicates that ford and hyundai cars do not depreciate in
## price as do bmw and audi cars.

## we next look at the change in price as a function of mileage:
ggplot(the_data) + geom_smooth(aes(mileage, price, 
                                   color = car_type), 
                               se = FALSE) + scale_y_log10()
## the graph shows that in all cases there is a sizeable drop in
## price as mileage increases (as expected), but the drop
## seems to be the smallest for Mercedes, while it is the
## steepest for VW and Toyota.

## Modeling

the_data <- the_data[sample(nrow(the_data), 5000), ]
test_index <- createDataPartition(the_data$price, p = 0.5, 
                                  list = FALSE)
train_set <- the_data[-test_index, ]
test_set <- the_data[test_index, ]

train_control <- trainControl(method = "repeatedcv", 
                              number = 10,
                              repeats = 5)

model_lm <- train(log(price) ~ ., data = train_set, 
                  method = "lm", trControl = train_control)

rmse_lm <- RMSE(predict(model_lm, test_set), 
                log(test_set$price))

model_rf <- train(log(price) ~ ., data = train_set,
                  method = "rf", 
                  trControl = train_control,
                  tuneGrid = expand.grid(mtry = c(1:50)))

ggplot(model_rf, highlight = TRUE)

rmse_rf <- RMSE(predict(model_rf, test_set), 
                log(test_set$price))

model_svm <- train(log(price) ~ ., data = train_set,
                   method = "svmLinear3",
                   trControl = train_control,
                   tuneGrid = data.frame(Loss = c(rep("L1", 6), 
                                                  rep("L2", 6)),
                                         cost = rep(seq(0.01, 
                                                        0.06, 
                                                        0.01), 
                                                    2)))
rmse_svm <- RMSE(predict(model_svm, test_set), 
     log(test_set$price))


rmse_results <- data.frame(Method = c("Linear", "Random forest", "SVM"),
                           RMSE = c(rmse_lm, rmse_rf, rmse_svm))

## create ensemble
ensemble <- data.frame(Linear = predict(model_lm, test_set),
                       Random_forest = predict(model_rf, test_set),
                       SVM = predict(model_svm, test_set))
ensemble_predict <- rowMeans(ensemble)
rmse_ensemble <- RMSE(ensemble_predict, log(test_set$price))
rmse_results <- rmse_results %>% add_row(Method = "Ensemble",
                                         RMSE = rmse_ensemble)

## the linear model is the best
## let us look at the accuracy of the predictions
predict_lm <- predict(model_lm, test_set)
check <- cbind(test_set, predict_lm)
## plot the predictions against the actual values and 
## compare using the y=x line
ggplot(check) + geom_point(aes(predict_lm, log(price)), alpha = 0.1) + 
  geom_line(aes(log(price), log(price))) + facet_wrap(~car_type)
## now plot the error distribution. 
ggplot(check) + 
  geom_histogram(aes(log(price) - predict_lm), fill = "blue")
## looks symmetric

## now plot errors against some variables
ggplot(check, aes(year, log(price) - predict_lm)) + geom_point() + 
  geom_smooth() + ylab("Errors")

ggplot(check, aes(car_type, log(price) - predict_lm)) + 
  geom_point() + ylab("Errors")

ggplot(check, aes(price, log(price) - predict_lm)) + geom_point() +
  geom_smooth() + ylab("Errors")

ggplot(check, aes(mileage, log(price) - predict_lm)) + geom_point() +
  geom_smooth() + ylab("Errors")

ggplot(check, aes(car_type, log(price) - predict_lm)) + 
  ylab("Errors") + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90,
                                                    vjust = 0.5))
