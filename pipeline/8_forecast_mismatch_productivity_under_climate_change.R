####FORECAST MISMATCH DUE TO CLIMATE CHANGE####
#assuming equivalent reaction norm

load("XXX/temp_mismatch_cater_model.RData")

#generate dataframe for prediction
#requires a range of values to run
climate_change_blank<-as.data.frame(cbind(
  rep(c("BLUTI","GRETI","PIEFL"),each=576),
  rep(seq(10.8,15.5,0.1),times=36),
  rep(c("pirate"),times=1728),
  rep(levels(BLUTI_complete_data_with_temp$site),each=48,times=36),
  rep(c("pirate"),times=1728),
  rep(c(0,1),times=864)))

names(climate_change_blank)<-c("species","tasmean_AprMay","year","site","box","mismatch_cater")
climate_change_blank <- climate_change_blank %>%
  mutate_at(c("species","site","year","box"), as.factor) %>%
  mutate_at(c("tasmean_AprMay","mismatch_cater"), as.numeric)

climate_change_pred<-predict(temp_mismatch_cater_model,
                             newdat=climate_change_blank,
                             marginal= ~ site + year + box,
                             type="response",
                             interval="confidence")

climate_change_predictions<-cbind(climate_change_blank,climate_change_pred)

#filter data down to one 'site', all sites are the same because site is marginal
climate_change_predictions <- 
  climate_change_predictions[climate_change_predictions$site == "BRIDFORD", ]

#drop rows
climate_change_predictions$site <- NULL
climate_change_predictions$year <- NULL
climate_change_predictions$box <- NULL
climate_change_predictions$mismatch_cater <- NULL

#pull out the value/s we're interested in
climate_change_predictions[climate_change_predictions$tasmean_AprMay == "13.6", ]


####PREDICT DECLINE IN PRODUCTIVITY DUE TO FORECAST MISMATCH####
#pull out median frass values for peak
frass_data_FINAL <- frass_data_FINAL[!is.na(frass_data_FINAL$cater_model_max), ]
median(frass_data_FINAL$cater_model_max, na.rm = TRUE)

unique_peaks <- complete_data %>%
  distinct(height_cater_fit, .keep_all = TRUE)

complete_data[complete_data$tasmean_AprMay == "13.6", ]

#test with predicted mismatch from models, 27 for BT and 31 for GT
test_run <- GRETI_count_cater_predictions[GRETI_count_cater_predictions$mismatch_cater == "31" , ]

test_run <- test_run[test_run$upr > 4.3 ,]

test_run <- min(test_run$height_cater_log)   

minimum<-exp(test_run)

minimum

percent_exceed <- mean(frass_master$cater_model_max > 73.6, na.rm = TRUE) * 100

percent_exceed