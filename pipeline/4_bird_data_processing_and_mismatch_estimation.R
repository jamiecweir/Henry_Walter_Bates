####LOAD AND PROCESS FILES####
Treswell_nest <-
  read.csv("XXX/Nest_TRESWELL.csv") %>%
  select(Year, Box, Popn, Species, CltSize, Fledged, DFE, Outcome) %>%
  mutate(Popn = "TRESWELL")
Herts_nest <-
  read.csv("XXX/Nest_HERTS.csv") %>%
  select(Year, Box, Popn, Species, CltSize, Fledged, DFE, Outcome) %>%
  mutate(Popn = "SPW")
EDartmoor_nest <-
  read.csv("XXX/Nest_EastDartmoor.csv") %>%
  select(Year, Box, Popn, Species, CltSize, Fledged, DFE, Outcome) %>%
  filter(Popn == c("YAR", "BVW", "NEA"))
EDartmoor_nest$Popn[EDartmoor_nest$Popn == "YAR"] <- "YARNER"
EDartmoor_nest$Popn[EDartmoor_nest$Popn == "BVW"] <- "BVW"
EDartmoor_nest$Popn[EDartmoor_nest$Popn == "NEA"] <- "NEADON"
Sussex_nest <-
  read.csv("XXX/Nest_SUSSEX.csv") %>%
  select(Year, Box, Popn, Species, CltSize, Fledged, DFE, Outcome) %>%
  filter(Popn == "SLINDON")
Lancs_nest <-
  read.csv("XXX/Nest_LANCS.csv") %>%
  select(Year, Popn, Species, CltSize, Fledged, DFE, Outcome) %>%
  mutate(Box = "NA") %>%
  filter(Popn == c("MIDDLE", "BLACK", "BELT", "FAITHWAITE", "TROWBARROW"))
Lancs_nest$Popn[Lancs_nest$Popn == "MIDDLE"] <- "MIDDLEWOOD"
Lancs_nest$Popn[Lancs_nest$Popn == "BLACK"] <- "BLACKWOOD"
Lancs_nest$Popn[Lancs_nest$Popn == "BELT"] <- "BELTWOOD"
Lancs_nest$Popn[Lancs_nest$Popn == "TROWBARROW"] <- "TROUGHBARROW"
Nags_nest <-
  read.csv("XXX/Nest_NAGS.csv") %>%
  select(Year, Box, Popn, Species, CltSize, Fledged, DFE, Outcome) %>%
  mutate(Popn = "NAGSHEAD")
#filter(Popn == c("NAG", "KEN", "FLO", "BRO", "CAN", "DEA", "LON",
#"LYD", "BOX", "QUA", "CAN")) %>%
Teign_nest <-
  read.csv("XXX/Nest_TEIGN.csv") %>%
  select(Year, Box, Popn, Species, CltSize, Fledged, DFE, Outcome) %>%
  filter(Popn == c("STEPS", "BRWD"))
Teign_nest$Popn[Teign_nest$Popn == "STEPS"] <- "DUNSFORD"
Teign_nest$Popn[Teign_nest$Popn == "BRWD"] <- "BRIDFORD"

#rbind together
nesting_data<-rbind(Treswell_nest,Herts_nest,EDartmoor_nest,
                    Sussex_nest, Lancs_nest, Nags_nest, Teign_nest)

#filter to relevant years
nesting_data<-nesting_data %>% 
  filter(Popn == "BELTWOOD" & between(Year, 2011, 2012) |
           Popn == "BLACKPOOL" & between(Year, 2013, 2013) |
           Popn == "BLACKWOOD" & between(Year, 2011, 2012) |
           Popn == "BRIDFORD" & between(Year, 2012, 2013) |
           Popn == "BVW" & between(Year, 2014, 2014) |
           Popn == "DUNSFORD" & between(Year, 2012, 2013) | 
           Popn == "FAITHWAITE" & between(Year, 2010, 2023) |
           Popn == "MIDDLEWOOD" & between(Year, 2014, 2023) |  
           Popn == "NAGSHEAD" & between(Year, 2014, 2023) |  
           Popn == "NEADON" & between(Year, 2012, 2023) |
           Popn == "SLINDON" & between(Year, 2018, 2023) |      
           Popn == "SPW" & between(Year, 2008, 2023) |
           Popn == "TRESWELL" & between(Year, 2011, 2023) |
           Popn == "TROUGHBARROW" & between(Year, 2011, 2023) |   
           Popn == "YARNER" & between(Year, 2011, 2023))   

#join with frass data
colnames(nesting_data)[colnames(nesting_data) == 'Popn'] <- 'site'
colnames(nesting_data)[colnames(nesting_data) == 'Year'] <- 'year'
colnames(nesting_data)[colnames(nesting_data) == 'Box'] <- 'box'
colnames(nesting_data)[colnames(nesting_data) == 'Species'] <- 'species'
colnames(nesting_data)[colnames(nesting_data) == 'CltSize'] <- 'cltsize'
colnames(nesting_data)[colnames(nesting_data) == 'Fledged'] <- 'fledged'
colnames(nesting_data)[colnames(nesting_data) == 'DFE'] <- 'dfe'
colnames(nesting_data)[colnames(nesting_data) == 'Outcome'] <- 'outcome'

nesting_data <- nesting_data %>%
  mutate_at(vars(year, box, site, species, outcome), as.factor) %>%
  mutate_at(vars(cltsize, fledged, dfe), as.numeric)

####COMBINE INTO SINGLE DATAFRAME WITH RELEVANT FRASS####

complete_data <- frass_predictions %>%
  inner_join(nesting_data, by = c("year", "site"))

#select relevant bird species

complete_data <- complete_data[which(complete_data$species=="GRETI" | 
                                       complete_data$species=="BLUTI" |
                                       complete_data$species=="PIEFL"),]
complete_data <- droplevels(complete_data)
levels(complete_data$site)

####CLUTCH SIZE ESTIMATION####

#where intial clutch sizes were not recorded for a nest, estimate these using site by year numerical averages and add to dataframe
complete_data_nocltsizenas <- complete_data %>% drop_na(cltsize)

clutch_predictions <- as.data.frame(complete_data_nocltsizenas %>% 
                                      group_by(species, year, site) %>% 
                                      summarize(
                                        avg_cltsize = mean(cltsize)
                                      ))

complete_data <- complete_data %>%
  inner_join(clutch_predictions, by = c("species", "year", "site"))

#add new column with either true or average value

complete_data$cltsize_final <- NA
complete_data$cltsize_final <- ifelse(is.na(complete_data$cltsize), complete_data$avg_cltsize, complete_data$cltsize)

####CALCULATING MISMATCH####

#10 for estimated time of maximum demand in chick development
#14 for average incubation period of all three species

complete_data <- 
  complete_data %>% mutate(mismatch_cater = (dfe + 10 + 14 + cltsize_final) - peak_cater_fit)

#filter out irrelevant outcomes, predation events which not linked to food availability
complete_data <- complete_data %>% 
  filter(!outcome %in% c("EP", "JP", "XD", "XO", "EE", 
                         "EF", "EI", "EM", "EU", "EW", 
                         "JE", "JF", "JI", "JM", "JU", 
                         "JW"))

#log transform heights
complete_data <- complete_data %>% mutate(height_cater_log = log(height_cater_fit))

#save complete data
write.csv(complete_data,"XXX/complete_data.csv", row.names = FALSE)