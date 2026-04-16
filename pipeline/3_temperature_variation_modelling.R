#How does temperature change over time, across the complete timeframe at all sites?
#This includes 'gap' years for which no corresponding frass/bird data is available for the 
#year/site combination

mean_monthly_temp_all_year_site_combos<-read.csv("XXX/mean_monthly_temp_all_year_site_combos.csv")
mean_monthly_temp_all_year_site_combos$site<-as.factor(mean_monthly_temp_all_year_site_combos$site)
mean_monthly_temp_all_year_site_combos$year<-as.numeric(mean_monthly_temp_all_year_site_combos$year)

mean_monthly_temp_all_year_site_combos$site_within_year <- interaction(mean_monthly_temp_all_year_site_combos$year, mean_monthly_temp_all_year_site_combos$site, drop = TRUE)
mean_monthly_temp_all_year_site_combos$year_number <- mean_monthly_temp_all_year_site_combos$year - 2008
mean_monthly_temp_all_year_site_combos$year_factor <- as.factor(mean_monthly_temp_all_year_site_combos$year)
mean_monthly_temp_all_year_site_combos$year_number_factor <- as.factor(mean_monthly_temp_all_year_site_combos$year_number)

a<-1000
prior_temp<-list(R=list(V=1, nu=0.002),
                 G=list(G1=list(V=diag(2), nu=2, alpha.mu=rep(0,2), alpha.V=diag(2)*a),
                        G2=list(V=diag(1), nu=1, alpha.mu=0, alpha.V=diag(1)*a)))

temperature_site_AprMay_overall <-
  MCMCglmm(
    tasmean_AprMay ~ year_number,
    random =  ~ us(1+year_number):site + year_number_factor,
    data = mean_monthly_temp_all_year_site_combos,
    pr = TRUE,
    verbose = TRUE,
    nitt = 3000000,
    thin = 100,
    burnin = 1000000,
    singular.ok = TRUE,
    prior=prior_temp
  )

save(temperature_site_AprMay_overall, file = "XXX/temperature_site_AprMay_overall.RData")

