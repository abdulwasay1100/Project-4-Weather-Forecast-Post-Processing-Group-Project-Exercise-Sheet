library(zoo)
library(maps)
library(gbm)
library(caret)


fcs.test <- readRDS("sippel_data/ESSD_benchmark_test_data_forecasts.rds")
fcs.train <- readRDS("sippel_data/ESSD_benchmark_training_data_forecasts.rds")
obs.train <- readRDS("sippel_data/ESSD_benchmark_training_data_observations.rds")
obs.test <- readRDS("sippel_data/ESSD_benchmark_test_data_observations_4students.rds")

## inspect data structure
# str(fcs.test) ## you can do the same for the other variables
# str(obs.test)
str(fcs.train)
# str(obs.train)

str(obs.test)

prepare_data <- function(this_station, this_leadtime) {
  idx.lt <- which(obs.test$leadtime == this_leadtime)
  idx.sta <- which(obs.test$latlon$station.id == this_station)
  obs <- obs.test$data$t2m[idx.lt,,idx.sta]
  time <- obs.test$time + this_leadtime * 3600   # leadtime is added to real time
  obs <- cbind(obs.test$time, this_leadtime, this_station, obs) # now obs at leadtime lt are combined with true time
  obs <- zoo(obs, time)
  colnames(obs) <- c('init', 'lt', 'stat', 'obs')
  
  idx.lt <- which(fcs.test$leadtime == this_leadtime)
  idx.sta <- which(fcs.test$latlon$station.id == this_station)
  dmo <- fcs.test$data$t2m[idx.lt,,idx.sta]
  
  test <- cbind(dmo)  # hint: you can add more predictors here
  # the following are possible (+ any transformations/interactions): 
  # tp6 sshf6 slhf6 ssr6 str6 cp6 mx2t6 mn2t6 ssrd6 strd6 p10fg6
  
  time <- fcs.test$time + this_leadtime * 3600
  test <- zoo(test, time)
  test <- merge(obs, test)
  
  # Preparing training data
  trai.all <- NULL
  for(year in fcs.train$year){
    idx.lt <- which(obs.train$leadtime == this_leadtime)
    idx.year <- which(obs.train$year == year)
    idx.sta <- which(obs.train$latlon$station.id == this_station)
    
    obs <- obs.train$data$t2m[idx.lt,idx.year,,idx.sta]
    year.dummy <- as.POSIXct(paste0(2017 - 20 + year,'-01-02'))
    time <- year.dummy + obs.train$time * 3600 * 24 + this_leadtime * 3600
    fctime_obs_trai <- year.dummy + obs.train$time * 3600 * 24
    obs <- cbind(fctime_obs_trai, this_leadtime, this_station, obs)
    obs <- zoo(obs, time)
    colnames(obs) <- c('init', 'lt', 'stat', 'obs')
    
    idx.lt <- which(fcs.train$leadtime == this_leadtime)
    idx.year <- which(fcs.train$year == year)
    idx.sta <- which(fcs.train$latlon$station.id == this_station)
    dmo <- fcs.train$data$t2m[idx.lt,idx.year,,idx.sta]
    trai <- cbind(dmo)  # hint: you can add more predictors here
    time <- year.dummy + fcs.train$time * 3600 * 24 + this_leadtime * 3600
    
    trai <- zoo(trai, time)
    trai <- merge(obs, trai)
    
    trai.all <- rbind(trai.all, trai)
  }
  
  trai <- na.omit(trai.all)
  # Using only data for training before 2017-01-01
  trai <- trai[which(index(trai) < as.POSIXct('2017-01-01')),]
  
  # Convert K to °C
  trai$obs <- trai$obs - 273.15
  test$obs <- test$obs - 273.15
  trai$dmo <- trai$dmo - 273.15
  test$dmo <- test$dmo - 273.15
  
  # Adding sine and cosine for day of the year to capture seasonality
  yday <- as.POSIXlt(index(trai))$yday
  trai$sin.doy <- sin(2 * pi * yday / 365)
  trai$cos.doy <- cos(2 * pi * yday / 365)
  
  yday <- as.POSIXlt(index(test))$yday
  test$sin.doy <- sin(2 * pi * yday / 365)
  test$cos.doy <- cos(2 * pi * yday / 365)
  
  return(list(trai = trai, test = test))
}

my.pred = matrix(data = NA, nrow = 730, ncol = length(fcs.test$latlon$station.id))
mse_results <- data.frame(station_id = fcs.test$latlon$station.id, mse_dmo = NA, mse_mos = NA)

# Define hyperparameter grid
tuneGrid <- expand.grid(
  n.trees = c(500, 1000, 1500),
  interaction.depth = c(1, 3, 5),
  shrinkage = c(0.01, 0.1),
  n.minobsinnode = c(10, 20)
)
for(st in fcs.test$latlon$station.id){
  print(paste0('Forecast at Station: ', st, ' (', which(fcs.test$latlon$station.id == st), ' from ', length(fcs.test$latlon$station.id), ')'))
  
  this_station <- st  # station id
  this_leadtime <- 120  # hours
  
  # Prepare data
  data <- prepare_data(this_station, this_leadtime)
  trai <- data$trai
  test <- data$test
  
  # Train control for caret
  trainControl <- trainControl(method = "cv", number = 5, search = "random")
  
  # Train GBM model with random search for hyperparameter tuning
  fit.gbm <- train(
    obs ~ dmo + sin.doy + cos.doy,
    data = trai,
    method = "gbm",
    trControl = trainControl,
    tuneGrid = tuneGrid,
    metric = "RMSE",
    tuneLength = 10,  # Number of random combinations to try
    verbose = FALSE
  )
  
  # Get the best model
  best.model <- fit.gbm$finalModel
  
  # Predict using the best model
  pred <- predict(best.model, newdata = test, n.trees = fit.gbm$bestTune$n.trees)
  
  # Store predictions
  idx.sta <- which(fcs.test$latlon$station.id == this_station)
  my.pred[,idx.sta] <- pred
  
  # Evaluate performance
  mse.dmo <- mean((test$obs - test$dmo)^2, na.rm=TRUE)
  mse.mos <- mean((test$obs - my.pred[,idx.sta])^2, na.rm=TRUE)
  mse_results$mse_dmo[idx.sta] <- mse.dmo
  mse_results$mse_mos[idx.sta] <- mse.mos
  
  # Print best hyperparameter combination
  print(paste0('Best parameters for station ', this_station, ':'))
  print(fit.gbm$bestTune)
}

# Inspect the prediction structure
str(my.pred)

# Save prediction
save(my.pred, file = "my.GBM.MOS_group_X.rds")

# Evaluate the performance compared to baseline
mse_results$improvement <- 1 - mse_results$mse_mos / mse_results$mse_dmo
print(mse_results)

# Plot improvement for each station using base R
plot(mse_results$station_id, mse_results$improvement, type = "h",
     main = "Improvement in MSE for each Station",
     xlab = "Station ID", ylab = "Improvement", col = "blue",
     lwd = 2)
