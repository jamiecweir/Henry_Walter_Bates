####DATA PREP####
#required packages
library(dplyr)
library(readxl)
library(MCMCglmm)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(stringr)

frass_data_FINAL <-  read.csv(#insert file location#)
frass_master <- as.data.frame(frass_data_FINAL)
frass_master <- frass_master %>%
  mutate(trap_unique = paste(site, trap, sep = "_")) %>%
  mutate_at(vars(trap_unique, site, year), as.factor)

colnames(frass_master)[colnames(frass_master) == 'cater peak day'] <- 'cater_peak_day'
colnames(frass_master)[colnames(frass_master) == 'cater actual max'] <- 'cater_actual_max'
colnames(frass_master)[colnames(frass_master) == 'cater model max'] <- 'cater_model_max'

#filter out non-oak traps, for a focus on oak woodlands for this study
frass_master <- frass_master %>% filter(str_detect(hab, "OK"))

####CATERPILLAR PEAK AND HEIGHT MODELS####
#model the value of peak height and date for each site by year combination

cat_mass_peak_model <-
  MCMCglmm(
    cater_peak_day ~ year + site + year:site,
    random =  ~ trap_unique,
    data = frass_master,
    pr = TRUE,
    verbose = TRUE,
    nitt = 1500000,
    thin = 100,
    burnin = 500000,
    singular.ok = TRUE
  )

frass_master$log_cater_model_max <- log(frass_master$cater_model_max)

cat_mass_height_model <-
  MCMCglmm(
    log_cater_model_max ~ year + site + year:site,
    random =  ~ trap_unique,
    data = frass_master,
    pr = TRUE,
    verbose = TRUE,
    nitt = 1500000,
    thin = 100,
    burnin = 500000,
    singular.ok = TRUE,
    family="gaussian"
  )

####GENERATE FRASS PREDICTIONS####

#length(unique(frass_master$year))
#length(unique(frass_master$site))
blank_frass<-as.data.frame(cbind(
  rep(levels(frass_master$site),each=16),
  rep(levels(frass_master$year),each=1,times=16),
  rep(c(0,1),128),
  rep(c(0,1),128),
  rep(c(0,1),128),
  rep(c(0,1),128),
  rep(c(0,1),128),
  sample(frass_master$trap_unique,256,replace=TRUE)))

#sample(frass_master$trap_unique,238,replace=TRUE)

names(blank_frass)<-c("site","year","peak_day","log_model_max","duration","cater_peak_day",
                      "log_cater_model_max", "trap_unique")
blank_frass <- blank_frass %>%
  mutate_at(c("site","trap_unique","year"), as.factor) %>%
  mutate_at(c("peak_day","log_model_max","duration",
              "cater_peak_day", "log_cater_model_max"), as.numeric)  

distinct_frass_levels <- frass_master %>%
  distinct(site, year, .keep_all = FALSE)

#predictions
frass_pred<-predict(cat_mass_peak_model,
                    newdat=blank_frass,
                    marginal=~trap_unique,
                    type="response",
                    interval="confidence")

frass_predictions<-cbind(frass_predictions,frass_pred)

colnames(frass_predictions)[colnames(frass_predictions) == 'fit'] <- 'peak_cater_fit'
colnames(frass_predictions)[colnames(frass_predictions) == 'lwr'] <- 'peak_cater_lwr'
colnames(frass_predictions)[colnames(frass_predictions) == 'upr'] <- 'peak_cater_upr'

frass_pred<-predict(cat_mass_height_model,
                    newdat=blank_frass,
                    marginal=~trap_unique,
                    type="response",
                    interval="confidence")

frass_predictions<-cbind(frass_predictions,frass_pred)

colnames(frass_predictions)[colnames(frass_predictions) == 'fit'] <- 'height_cater_fit'
colnames(frass_predictions)[colnames(frass_predictions) == 'lwr'] <- 'height_cater_lwr'
colnames(frass_predictions)[colnames(frass_predictions) == 'upr'] <- 'height_cater_upr'

frass_predictions <- frass_predictions %>%
  inner_join(distinct_frass_levels, by = c("site","year"))

frass_predictions$height_cater_fit <- exp(frass_predictions$height_cater_fit)
frass_predictions$height_cater_lwr <- exp(frass_predictions$height_cater_lwr)
frass_predictions$height_cater_upr <- exp(frass_predictions$height_cater_upr)

#remove unnecessary columns
frass_predictions <- within(frass_predictions, rm("peak_day",
                                                  "log_model_max",
                                                  "duration",
                                                  "cater_peak_day",
                                                  "log_cater_model_max",
                                                  "trap_unique"))

#remove problematic rows, Treswell 2017 and 2022, Troughbarrow 2021, and Middlewood 2020 and 2021 
#sites have no data so weird estimates
frass_predictions <- frass_predictions %>% filter(!(site == "TRESWELL" & year == 2014 |
                                                      site == "TRESWELL" & year == 2017 |
                                                      site == "TRESWELL" & year == 2022 |
                                                      site == "TROUGHBARROW" & year == 2021 |
                                                      site == "MIDDLEWOOD" & year == 2020 |
                                                      site == "MIDDLEWOOD" & year == 2021 |
                                                      site == "FAITHWAITE" & year == 2020))

#save frass predictions
write.csv(frass_predictions,"frass_predictions.csv", row.names = TRUE)
