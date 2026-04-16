####TEMPERATURE VARIATION####
model<-temperature_site_AprMay_overall

# Check fixed effects names and number
colnames(model$Sol)
# Should be: (Intercept), tasmean_AprMay

# Number of fixed effects (excluding intercept)... this part isn't really necessary here because we 
#have only 1 fixed effects... more complex models required it
fixed_effects <- 2

# Variance of tasmean_AprMay in your data
var_year_number <- var(model$X[, "year_number"])

# Posterior variance of fixed effect (year_number) multiplied by var of predictor
Vfixed_effects <- (model$Sol[,"year_number"])^2 * var_year_number
median(Vfixed_effects)
HPDinterval(as.mcmc(Vfixed_effects))

# Variance attributable to random slopes (site:tasmean_AprMay)
# Using formula: Var(slope) * Var(predictor)
var_year_number <- var(model$X[, "year_number"])
site_slope_variance <- model$VCV[,"year_number:year_number.site"] * var_year_number
median(site_slope_variance)
HPDinterval(as.mcmc(site_slope_variance))

# Variance attributable to random intercepts
# Site intercept variance
site_intercept_variance <- model$VCV[,"(Intercept):(Intercept).site"]
# Year intercept variance
year_variance <- model$VCV[,"year_number_factor"]
# Residual variance
residual_variance <- model$VCV[,"units"]

# Total random intercept variance
total_intercept_variance <- site_intercept_variance + year_variance + residual_variance

# Total model variance (fixed + random slope + random intercepts)
total_model_variance <- Vfixed_effects + site_slope_variance + total_intercept_variance

# Proportions of variance explained
# Fixed effects
fixed_prop <- Vfixed_effects / total_model_variance
median(fixed_prop)
HPDinterval(as.mcmc(fixed_prop))

# Random slopes
slope_prop <- site_slope_variance / total_model_variance
median(slope_prop)
HPDinterval(as.mcmc(slope_prop))

# Random intercepts
intercept_prop <- total_intercept_variance / total_model_variance
median(intercept_prop)
HPDinterval(as.mcmc(intercept_prop))

# Breakdown of intercept variance components
# Site intercept proportion
site_intercept_prop <- site_intercept_variance / total_model_variance #total_intercept_variance
median(site_intercept_prop)
HPDinterval(as.mcmc(site_intercept_prop))

# Year intercept proportion
year_prop <- year_variance / total_model_variance #total_intercept_variance
median(year_prop)
HPDinterval(as.mcmc(year_prop))

# Residual proportion
residual_prop <- residual_variance / total_model_variance #total_intercept_variance
median(residual_prop)
HPDinterval(as.mcmc(residual_prop))

# Combine variance proportion estimates and HPD intervals into a tidy dataframe
# Create a list of all proportion vectors and their labels
prop_list <- list(
  fixed_prop = fixed_prop,
  slope_prop = slope_prop,
  intercept_prop = intercept_prop,
  site_intercept_prop = site_intercept_prop,
  year_prop = year_prop,
  residual_prop = residual_prop
)

# Build a data frame with medians and HPD intervals ***SPECIES NAME***
prop_summary <- do.call(rbind, lapply(names(prop_list), function(name) {
  prop <- prop_list[[name]]
  hpd <- HPDinterval(as.mcmc(prop))
  data.frame(
    Component = name,
    Median = median(prop),
    Lower95 = hpd[1],
    Upper95 = hpd[2]
  )
}))

#save with species name
print(prop_summary)

write.csv(prop_summary,"XXX/temp_prop_var_summary.csv", row.names = TRUE)


####MISMATCH VS TEMPERATURE RELATIONSHIP####
# Load your fitted model. ***Switch the model to suit what you want***
model <- PIEFL_temp_mismatch_cater_model

# Check fixed effects names and number
colnames(model$Sol)
# Should be: (Intercept), tasmean_AprMay

# Number of fixed effects (excluding intercept)... this part isn't really necessary here because we 
#have only 1 fixed effects... more complex models required it
#fixed_effects <- 2

# Variance of tasmean_AprMay in your data
var_tasmean <- var(model$X[, "tasmean_AprMay"])

# Posterior variance of fixed effect (tasmean_AprMay) multiplied by var of predictor
Vfixed_effects <- (model$Sol[,"tasmean_AprMay"])^2 * var_tasmean
median(Vfixed_effects)
HPDinterval(as.mcmc(Vfixed_effects))

# Variance attributable to random slopes (site:tasmean_AprMay)
# Using formula: Var(slope) * Var(predictor)
var_tasmean <- var(model$X[, "tasmean_AprMay"])^2
site_slope_variance <- model$VCV[,"tasmean_AprMay:tasmean_AprMay.site"] * var_tasmean
median(site_slope_variance)
HPDinterval(as.mcmc(site_slope_variance))

# Variance attributable to random intercepts
# Site intercept variance
site_intercept_variance <- model$VCV[,"(Intercept):(Intercept).site"]
# Year intercept variance
year_variance <- model$VCV[,"year"]
# Box intercept variance
box_variance <- model$VCV[,"box"]
# Residual variance
residual_variance <- model$VCV[,"units"]

# Total random intercept variance
total_intercept_variance <- site_intercept_variance + year_variance + box_variance + residual_variance

# Total model variance (fixed + random slope + random intercepts)
total_model_variance <- Vfixed_effects + site_slope_variance + total_intercept_variance

# Proportions of variance explained
# Fixed effects
fixed_prop <- Vfixed_effects / total_model_variance
median(fixed_prop)
HPDinterval(as.mcmc(fixed_prop))

# Random slopes
slope_prop <- site_slope_variance / total_model_variance
median(slope_prop)
HPDinterval(as.mcmc(slope_prop))

# Random intercepts
intercept_prop <- total_intercept_variance / total_model_variance
median(intercept_prop)
HPDinterval(as.mcmc(intercept_prop))

# Breakdown of intercept variance components
# Site intercept proportion
site_intercept_prop <- site_intercept_variance / total_intercept_variance
median(site_intercept_prop)
HPDinterval(as.mcmc(site_intercept_prop))

# Year intercept proportion
year_prop <- year_variance / total_intercept_variance
median(year_prop)
HPDinterval(as.mcmc(year_prop))

# Box intercept proportion
box_prop <- box_variance / total_intercept_variance
median(box_prop)
HPDinterval(as.mcmc(box_prop))

# Residual proportion
residual_prop <- residual_variance / total_intercept_variance
median(residual_prop)
HPDinterval(as.mcmc(residual_prop))

# Combine variance proportion estimates and HPD intervals into a tidy dataframe
# Create a list of all proportion vectors and their labels
prop_list <- list(
  fixed_prop = fixed_prop,
  slope_prop = slope_prop,
  intercept_prop = intercept_prop,
  site_intercept_prop = site_intercept_prop,
  year_prop = year_prop,
  box_prop = box_prop,
  residual_prop = residual_prop
)

# Build a data frame with medians and HPD intervals ***SPECIES NAME***
prop_summary_PIEFL <- do.call(rbind, lapply(names(prop_list), function(name) {
  prop <- prop_list[[name]]
  hpd <- HPDinterval(as.mcmc(prop))
  data.frame(
    Component = name,
    Median = median(prop),
    Lower95 = hpd[1],
    Upper95 = hpd[2]
  )
}))

#save with species name
print(prop_summary_PIEFL)

#combine 
prop_summary_BLUTI$species <- "BLUTI"
prop_summary_GRETI$species <- "GRETI"
prop_summary_PIEFL$species <- "PIEFL"

# Combine them into one dataframe
prop_summary_all <- rbind(
  prop_summary_BLUTI,
  prop_summary_GRETI,
  prop_summary_PIEFL
)

prop_summary_all[, 2:4] <- prop_summary_all[, 2:4] * 100

write.csv(prop_summary_all,"XXX/variance_components_temp_and_mismatch.csv", row.names = FALSE)


####PERFORMANCE, MISMATCH AND PEAK HEIGHT####

#First set of poisson count models, which have residual variance#
model<-GRETI_count_cater_mismatch_model

#check number of fixed effects first
fixed_effects<-5

# Variance attributable to fixed effects. 

Vx<-cov(as.matrix(model $X[,2:fixed_effects]))

Vfixed_effects<-apply(model $Sol[,2: fixed_effects], 1, function(x){t(x)%*%Vx%*%x})

#variance attributable to fixed effects
variance_mismatch<- (model $Sol[,"mismatch_cater"]^2)*Vx["mismatch_cater","mismatch_cater"]
variance_height_log<- (model $Sol[,"height_cater_log"]^2)*Vx["height_cater_log","height_cater_log"]
variance_mismatch_quad<- (model $Sol[,"I(mismatch_cater^2)"]^2)*Vx["I(mismatch_cater^2)","I(mismatch_cater^2)"]
variance_mismatch_height<- (model $Sol[,"mismatch_cater:height_cater_log"]^2)*Vx["mismatch_cater:height_cater_log","mismatch_cater:height_cater_log"]

#variance due to random intercepts
variance_year<-model$VCV[,"year"]
variance_site<-model$VCV[,"site"]
variance_box<-model$VCV[,"box"]
variance_cltsize<-model$VCV[,"cltsize_final"]
variance_residual<-model$VCV[,"units"]

#total model variance
variance_TOTAL<-Vfixed_effects + variance_year + variance_site + variance_box +
  variance_cltsize + variance_residual
variance_TOTAL_ex_cltsize<-Vfixed_effects + variance_year + variance_site + variance_box +
  variance_residual
variance_TOTAL_fixed_sum<-variance_mismatch + variance_height_log + variance_mismatch_quad +
  variance_mismatch_height

median(variance_cltsize / variance_TOTAL)

median(variance_height_log / variance_TOTAL_fixed_sum)
median(variance_mismatch / variance_TOTAL_fixed_sum)
median(variance_mismatch_height / variance_TOTAL_fixed_sum)
median(variance_mismatch_quad / variance_TOTAL_fixed_sum)
median(variance_site / variance_TOTAL_ex_cltsize)
median(variance_box / variance_TOTAL_ex_cltsize)
median(variance_year / variance_TOTAL_ex_cltsize)
median(variance_residual / variance_TOTAL_ex_cltsize)

HPDinterval(variance_height_log / variance_TOTAL_ex_cltsize)
HPDinterval(variance_mismatch / variance_TOTAL_fixed_sum)
HPDinterval(variance_mismatch_height / variance_TOTAL_fixed_sum)
HPDinterval(variance_mismatch_quad / variance_TOTAL_fixed_sum)
HPDinterval(variance_site / variance_TOTAL_ex_cltsize)
HPDinterval(variance_box / variance_TOTAL_ex_cltsize)
HPDinterval(variance_year / variance_TOTAL_ex_cltsize)
HPDinterval(variance_residual / variance_TOTAL_ex_cltsize)

#compute values
values <- list(
  cltsize              = variance_cltsize / variance_TOTAL,
  height_log           = variance_height_log / variance_TOTAL_fixed_sum,
  mismatch             = variance_mismatch / variance_TOTAL_fixed_sum,
  mismatch_height      = variance_mismatch_height / variance_TOTAL_fixed_sum,
  mismatch_quad        = variance_mismatch_quad / variance_TOTAL_fixed_sum,
  all_fixed            = Vfixed_effects / variance_TOTAL_ex_cltsize,
  site                 = variance_site / variance_TOTAL_ex_cltsize,
  box                  = variance_box / variance_TOTAL_ex_cltsize,
  year                 = variance_year / variance_TOTAL_ex_cltsize,
  residual             = variance_residual / variance_TOTAL_ex_cltsize
)

#build dataframe
GRETI_count_df <- lapply(names(values), function(param) {
  dist <- values[[param]]
  hpd <- HPDinterval(dist)
  data.frame(
    species = "GRETI",
    model = "count",
    parameter = param,
    median = median(dist),
    lower = hpd[1],
    upper = hpd[2]
  )
}) %>% bind_rows()


#Second set with presabs model#
model<-GRETI_presabs_cater_mismatch_model

#check number of fixed effects first
fixed_effects<-5

# Variance attributable to fixed effects. 

Vx<-cov(as.matrix(model $X[,2:fixed_effects]))

Vfixed_effects<-apply(model $Sol[,2: fixed_effects], 1, function(x){t(x)%*%Vx%*%x})

#variance attributable to fixed effects
variance_mismatch<- (model $Sol[,"mismatch_cater"]^2)*Vx["mismatch_cater","mismatch_cater"]
variance_height_log<- (model $Sol[,"height_cater_log"]^2)*Vx["height_cater_log","height_cater_log"]
variance_mismatch_quad<- (model $Sol[,"I(mismatch_cater^2)"]^2)*Vx["I(mismatch_cater^2)","I(mismatch_cater^2)"]
variance_mismatch_height<- (model $Sol[,"mismatch_cater:height_cater_log"]^2)*Vx["mismatch_cater:height_cater_log","mismatch_cater:height_cater_log"]

#variance due to random intercepts
variance_year<-model$VCV[,"year"]
variance_site<-model$VCV[,"site"]
variance_box<-model$VCV[,"box"]
variance_cltsize<-model$VCV[,"cltsize_final"]
variance_residual<-model$VCV[,"units"]

#total model variance
variance_TOTAL<-Vfixed_effects + variance_year + variance_site + variance_box +
  variance_cltsize + variance_residual
variance_TOTAL_ex_cltsize<-Vfixed_effects + variance_year + variance_site + variance_box +
  variance_residual
variance_TOTAL_fixed_sum<-variance_mismatch + variance_height_log + variance_mismatch_quad +
  variance_mismatch_height

median(variance_cltsize / variance_TOTAL)

median(variance_height_log / variance_TOTAL_fixed_sum)
median(variance_mismatch / variance_TOTAL_fixed_sum)
median(variance_mismatch_height / variance_TOTAL_fixed_sum)
median(variance_mismatch_quad / variance_TOTAL_fixed_sum)
median(variance_site / variance_TOTAL_ex_cltsize)
median(variance_box / variance_TOTAL_ex_cltsize)
median(variance_year / variance_TOTAL_ex_cltsize)
median(variance_residual / variance_TOTAL_ex_cltsize)

HPDinterval(variance_height_log / variance_TOTAL_ex_cltsize)
HPDinterval(variance_mismatch / variance_TOTAL_fixed_sum)
HPDinterval(variance_mismatch_height / variance_TOTAL_fixed_sum)
HPDinterval(variance_mismatch_quad / variance_TOTAL_fixed_sum)
HPDinterval(variance_site / variance_TOTAL_ex_cltsize)
HPDinterval(variance_box / variance_TOTAL_ex_cltsize)
HPDinterval(variance_year / variance_TOTAL_ex_cltsize)
HPDinterval(variance_residual / variance_TOTAL_ex_cltsize)

#compute values
values <- list(
  cltsize              = variance_cltsize / variance_TOTAL,
  height_log           = variance_height_log / variance_TOTAL_fixed_sum,
  mismatch             = variance_mismatch / variance_TOTAL_fixed_sum,
  mismatch_height      = variance_mismatch_height / variance_TOTAL_fixed_sum,
  mismatch_quad        = variance_mismatch_quad / variance_TOTAL_fixed_sum,
  all_fixed            = Vfixed_effects / variance_TOTAL_ex_cltsize,
  site                 = variance_site / variance_TOTAL_ex_cltsize,
  box                  = variance_box / variance_TOTAL_ex_cltsize,
  year                 = variance_year / variance_TOTAL_ex_cltsize,
  residual             = variance_residual / variance_TOTAL_ex_cltsize
)

#build dataframe
GRETI_presabs_df <- lapply(names(values), function(param) {
  dist <- values[[param]]
  hpd <- HPDinterval(dist)
  data.frame(
    species = "GRETI",
    model = "presabs",
    parameter = param,
    median = median(dist),
    lower = hpd[1],
    upper = hpd[2]
  )
}) %>% bind_rows()


#combine into a single set
variance_comp<-rbind(BLUTI_count_df, BLUTI_presabs_df,
                     GRETI_count_df, GRETI_presabs_df,
                     PIEFL_count_df, PIEFL_presabs_df)

variance_comp <- variance_comp %>%
  mutate(median = median * 100, lower = lower*100, upper = upper*100) %>%
  mutate(across(c(median, lower, upper), round, 2))

write.csv(variance_comp,"XXX/variance_comp.csv", row.names=FALSE)
