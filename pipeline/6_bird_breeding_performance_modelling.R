####MAIN MODELS OF MISMATCH AND CAT PEAK EFFECTS ON BIRD PERFORMANCE####

#model set-up and priors

a <- 1000

prior_count_mismatch<-list(R=list(V=1,nu=0.002),
                           G=list(G1=list(V=diag(1), nu=1, alpha.mu=0, alpha.V=diag(1)*a),
                                  G2=list(V=diag(1), nu=1, alpha.mu=0, alpha.V=diag(1)*a),
                                  G3=list(V=diag(1), nu=1, alpha.mu=0, alpha.V=diag(1)*a),
                                  G4=list(V=diag(1), nu=1, alpha.mu=0, alpha.V=diag(1)*a)))

prior_presabs_mismatch<-list(R=list(V=1, fix=1),
                             G=list(G1=list(V=diag(1), nu=1, alpha.mu=0, alpha.V=diag(1)*a),
                                    G2=list(V=diag(1), nu=1, alpha.mu=0, alpha.V=diag(1)*a),
                                    G3=list(V=diag(1), nu=1, alpha.mu=0, alpha.V=diag(1)*a),
                                    G4=list(V=diag(1), nu=1, alpha.mu=0, alpha.V=diag(1)*a)))

####BLUE TIT COUNT####

BLUTI_count_cater_mismatch_model <-
  MCMCglmm(
    fledged ~ mismatch_cater + height_cater_log + mismatch_cater:height_cater_log + I(mismatch_cater^2),
    random =  ~ year + site + box + cltsize_final,
    data = BLUTI_data_count,
    pr = TRUE,
    verbose = TRUE,
    nitt = 3000000,
    thin = 100,
    burnin = 1000000,
    singular.ok = TRUE,
    prior=prior_count_mismatch,
    family="poisson"
  )

save(BLUTI_count_cater_mismatch_model, file = "XXX/BLUTI_count_cater_mismatch_model.RData")

####BLUE TIT PRESENCE/ABSENCE####

BLUTI_presabs_cater_mismatch_model <-
  MCMCglmm(
    any_fledged ~ mismatch_cater + height_cater_log + mismatch_cater:height_cater_log + I(mismatch_cater^2),
    random =  ~ year + site + box + cltsize_final,
    data = BLUTI_data_presabs,
    pr = TRUE,
    verbose = TRUE,
    nitt = 3000000,
    thin = 100,
    burnin = 1000000,
    singular.ok = TRUE,
    prior=prior_presabs_mismatch,
    family="categorical"
  )

save(BLUTI_presabs_cater_mismatch_model, file = "XXX/BLUTI_presabs_cater_mismatch_model.RData")

####GREAT TIT COUNT####

GRETI_count_mismatch_model <-
  MCMCglmm(
    fledged ~ mismatch + height_log + mismatch:height_log + I(mismatch^2),
    random =  ~ year + site + box + cltsize_final,
    data = GRETI_data_count,
    pr = TRUE,
    verbose = TRUE,
    nitt = 1500000,
    thin = 100,
    burnin = 500000,
    singular.ok = TRUE,
    prior=prior_count_mismatch,
    family="poisson"
  )

save(GRETI_count_mismatch_model, file = "XXX/GRETI_count_mismatch_model.RData")

####GREAT TIT PRESENCE/ABSENCE####

GRETI_presabs_cater_mismatch_model <-
  MCMCglmm(
    any_fledged ~ mismatch_cater + height_cater_log + mismatch_cater:height_cater_log + I(mismatch_cater^2),
    random =  ~ year + site + box + cltsize_final,
    data = GRETI_data_presabs,
    pr = TRUE,
    verbose = TRUE,
    nitt = 3000000,
    thin = 100,
    burnin = 1000000,
    singular.ok = TRUE,
    prior=prior_presabs_mismatch,
    family="categorical"
  )

save(GRETI_presabs_cater_mismatch_model, file = "XXX/GRETI_presabs_cater_mismatch_model.RData")

####PIED FLY COUNT####

PIEFL_count_cater_mismatch_model <-
  MCMCglmm(
    fledged ~ mismatch_cater + height_cater_log + mismatch_cater:height_cater_log + I(mismatch_cater^2),
    random =  ~ year + site + box + cltsize_final,
    data = PIEFL_data_count,
    pr = TRUE,
    verbose = TRUE,
    nitt = 3000000,
    thin = 100,
    burnin = 1000000,
    singular.ok = TRUE,
    prior=prior_count_mismatch,
    family="gaussian"
  )

save(PIEFL_count_cater_mismatch_model, file = "XXX/PIEFL_count_cater_mismatch_model.RData")

####PIED FLY PRESENCE/ABSENCE####

PIEFL_presabs_cater_mismatch_model <-
  MCMCglmm(
    any_fledged ~ mismatch_cater + height_cater_log + mismatch_cater:height_cater_log + I(mismatch_cater^2),
    random =  ~ year + site + box + cltsize_final,
    data = PIEFL_data_presabs,
    pr = TRUE,
    verbose = TRUE,
    nitt = 3000000,
    thin = 100,
    burnin = 1000000,
    singular.ok = TRUE,
    prior=prior_presabs_mismatch,
    family="categorical"
  )

save(PIEFL_presabs_cater_mismatch_model, file = "XXX/PIEFL_presabs_cater_mismatch_model.RData")
