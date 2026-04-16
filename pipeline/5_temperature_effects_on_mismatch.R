####CREATE DATAFRAME WITH FRASS/BIRD AND TEMP DATA####

mean_monthly_temp <-
  read.csv("XXX/mean_monthly_temp.csv")

mean_monthly_temp <- mean_monthly_temp %>%
  mutate_at(c("site","year"), as.factor) 

#estimate April/May mean
mean_monthly_temp <- mean_monthly_temp %>%
  mutate(tasmean_AprMay = (tasmean_April+tasmean_May)/2) %>%
  mutate(mean_tasmin_AprMay = (mean_tasmin_April+mean_tasmin_May)/2) %>%
  mutate(mean_tasmax_AprMay = (mean_tasmax_April+mean_tasmax_May)/2)

complete_data <- complete_data %>%
  mutate_at(c("year"), as.factor) 
complete_data_with_temp <- complete_data %>%
  inner_join(mean_monthly_temp, by = c("site","year"))

#drop absent site data, brought in with temp
complete_data_with_temp$site <- droplevels(complete_data_with_temp$site)

write.csv(complete_data_with_temp,"XXX/complete_data_with_temp.csv", row.names = FALSE)

####TEMP EFFECT ON MISMATCH####

#rough models to determine best predictor
model_min<-lm(mismatch_cater~mean_tasmin_AprMay, data=complete_data_with_temp) #10%
model_mean<-lm(mismatch_cater~tasmean_AprMay, data=complete_data_with_temp) #11.8%
model_max<-lm(mismatch_cater~mean_tasmax_AprMay, data=complete_data_with_temp) #8%
model_mean_Apr<-lm(mismatch_cater~tasmean_April, data=complete_data_with_temp) #11%
model_mean_May<-lm(mismatch_cater~tasmean_May, data=complete_data_with_temp) #5.5%
model_mean_quad<-lm(mismatch_cater~tasmean_AprMay+I(tasmean_AprMay^2), data=complete_data_with_temp)#11.8% also

#comparing the R2, mean is the best predictor

complete_data_with_temp <- complete_data_with_temp %>%
  mutate_at(c("site","year","box","species","outcome"), as.factor)

temp_mismatch_cater_model <-
  MCMCglmm(
    mismatch_cater ~ tasmean_AprMay + species + tasmean_AprMay:species,
    random =  ~ year + site + box,
    data = complete_data_with_temp,
    pr = TRUE,
    verbose = TRUE,
    nitt = 1500000,
    thin = 100,
    burnin = 500000,
    singular.ok = TRUE,
    family="gaussian"
  )

save(temp_mismatch_cater_model, file = "XXX/temp_mismatch_cater_model.RData")

#by species now
BLUTI_complete_data_with_temp <-
  complete_data_with_temp %>%
  filter(species == c("BLUTI"))

GRETI_complete_data_with_temp <-
  complete_data_with_temp %>%
  filter(species == c("GRETI"))

PIEFL_complete_data_with_temp <-
  complete_data_with_temp %>%
  filter(species == c("PIEFL"))

BLUTI_complete_data_with_temp <- BLUTI_complete_data_with_temp %>%
  mutate_at(c("site","year","box"), as.factor)
BLUTI_complete_data_with_temp$species <- droplevels(BLUTI_complete_data_with_temp$species)

GRETI_complete_data_with_temp <- GRETI_complete_data_with_temp %>%
  mutate_at(c("site","year","box"), as.factor)
GRETI_complete_data_with_temp$species <- droplevels(GRETI_complete_data_with_temp$species)

PIEFL_complete_data_with_temp <- PIEFL_complete_data_with_temp %>%
  mutate_at(c("site","year","box"), as.factor)
PIEFL_complete_data_with_temp$site <- droplevels(PIEFL_complete_data_with_temp$site)
PIEFL_complete_data_with_temp$species <- droplevels(PIEFL_complete_data_with_temp$species)

a<-1000
prior_temp_mismatch<-list(R=list(V=1, nu=0.002),
                          G=list(G1=list(V=diag(2), nu=2, alpha.mu=rep(0,2), alpha.V=diag(2)*a),
                                 G2=list(V=diag(1), nu=1, alpha.mu=0, alpha.V=diag(1)*a),
                                 G3=list(V=diag(1), nu=1, alpha.mu=0, alpha.V=diag(1)*a)))

BLUTI_temp_mismatch_cater_model <-
  MCMCglmm(
    mismatch_cater ~ tasmean_AprMay,
    random =  ~ us(1+tasmean_AprMay):site + year + box,
    data = BLUTI_complete_data_with_temp,
    pr = TRUE,
    verbose = TRUE,
    nitt = 1500000,
    thin = 100,
    burnin = 500000,
    singular.ok = TRUE,
    family="gaussian",
    prior = prior_temp_mismatch
  )
save(BLUTI_temp_mismatch_cater_model, file = "XXX/BLUTI_temp_mismatch_cater_model.RData")

GRETI_temp_mismatch_cater_model <-
  MCMCglmm(
    mismatch_cater ~ tasmean_AprMay,
    random =  ~ us(1+tasmean_AprMay):site + year + box,
    data = GRETI_complete_data_with_temp,
    pr = TRUE,
    verbose = TRUE,
    nitt = 1500000,
    thin = 100,
    burnin = 500000,
    singular.ok = TRUE,
    family="gaussian",
    prior = prior_temp_mismatch
  )
save(GRETI_temp_mismatch_cater_model, file = "XXX/GRETI_temp_mismatch_cater_model.RData")


PIEFL_temp_mismatch_cater_model <-
  MCMCglmm(
    mismatch_cater ~ tasmean_AprMay,
    random =  ~ us(1+tasmean_AprMay):site + year + box,
    data = PIEFL_complete_data_with_temp,
    pr = TRUE,
    verbose = TRUE,
    nitt = 1500000,
    thin = 100,
    burnin = 500000,
    singular.ok = TRUE,
    family="gaussian",
    prior = prior_temp_mismatch
  )
save(PIEFL_temp_mismatch_cater_model, file = "XXX/PIEFL_temp_mismatch_cater_model.RData")
