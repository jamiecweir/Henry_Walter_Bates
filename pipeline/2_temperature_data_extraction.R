####DATA SETUP AND PREP####
#For this workflow, you need the tasmax_hadukgrid_uk_5km_day and tasmin_ files downloaded from the MetOffice UK
#Data is required for the period April - June
#Place all min in folder named 'tasmin' and all max in folder 'tasmax'

#required packages
library(raster)
library(ncdf4) 
library(dplyr)

#data<-frass_predictions
frass_predictions<-read.csv("XXX")

frass_predictions <- frass_predictions %>%
  mutate_at(c("site", "year"), as.factor)

data<-frass_predictions
data <- data[,-(3:11)]

levels(data$site)[levels(data$site)=="WHITE WOOD"] <- "WHITEWOOD"
levels(data$site)[levels(data$site)=="NEADON CLEAVE"] <- "NEADON"

#alternative data with all site/year combinations
16*16
all_year_site_combos<-as.data.frame(cbind(
  rep(levels(data$year),each=16, times=1),
  rep(levels(data$site),each=1, times=16)))

names(all_year_site_combos)<-c("year","site")

all_year_site_combos <- all_year_site_combos %>%
  mutate_at(c("site", "year"), as.factor)

# Define a list of extents for the study sites (without the _ext suffix in the site names)
NAGSHEAD_ext <- extent(360000, 365000, 205000, 210000) #NAGSHEAD SO6008
YARNER = extent(275000, 280000, 075000, 080000) #YARNER SX7878
BVW = extent(275000, 280000, 075000, 080000) #BVW SX7780
NEADON = extent(275000, 280000, 075000, 080000) #NEADON SX7581
DUNSFORD_ext <- extent(279000, 279500, 088000, 088500) #DUNSFORD SX7988
BRIDFORD_ext <- extent(280000, 285000, 088000, 088500) #BRIDFORD SX8088
TRESWELL_ext <- extent(476000, 481000, 379000, 384000) #TRESWELL SK7679
SPW_ext <- extent(523000, 528000, 213000, 218000) #SPW TL2313
SLINDON_ext <- extent(495000, 500000, 110000, 115000) #SLINDON SU9510
MIDDLEWOOD_ext <- extent(360000, 365000, 466000, 471000) #MIDDLEWOOD SD6066
BLACKWOOD_ext <- extent(356000, 361000, 465000, 470000) #BLACKWOOD SD5665
BELTWOOD_ext <- extent(356000, 361000, 465000, 470000) #BELTWOOD SD5665 
FAITHWAITE_ext <- extent(357000, 362000, 466000, 471000) #FAITHWAITE SD5766
TROUGHBARROW_ext <- extent(348000, 353000, 475000, 480000) #TROUGHBARROW SD4875
WHITE_WOOD_ext <- extent(269000, 274000, 072000, 072500) #WHITEWOOD SX6972
BLACKPOOL_ext <- extent(268000, 273000, 072000, 072500) #BLACKPOOL SX6872

site_extents <- list(NAGSHEAD_ext = extent(360000, 365000, 205000, 210000), #NAGSHEAD SO6008
                     YARNER_ext = extent(275000, 280000, 075000, 080000), #YARNER SX7878
                     BVW_ext = extent(275000, 280000, 075000, 080000), #BVW SX7780
                     NEADON_ext = extent(275000, 280000, 075000, 080000), #NEADON SX7581
                     DUNSFORD_ext = extent(279000, 279500, 088000, 088500), #DUNSFORD SX7988
                     BRIDFORD_ext = extent(280000, 285000, 088000, 088500), #BRIDFORD SX8088
                     TRESWELL_ext = extent(476000, 481000, 379000, 384000), #TRESWELL SK7679
                     SPW_ext = extent(523000, 528000, 213000, 218000), #SPW TL2313
                     SLINDON_ext = extent(495000, 500000, 110000, 115000), #SLINDON SU9510
                     MIDDLEWOOD_ext = extent(360000, 365000, 466000, 471000), #MIDDLEWOOD SD6066
                     BLACKWOOD_ext = extent(356000, 361000, 465000, 470000), #BLACKWOOD SD5665
                     BELTWOOD_ext = extent(356000, 361000, 465000, 470000), #BELTWOOD SD5665 
                     FAITHWAITE_ext = extent(357000, 362000, 466000, 471000), #FAITHWAITE SD5766
                     TROUGHBARROW_ext = extent(348000, 353000, 475000, 480000), #TROUGHBARROW SD4875
                     WHITEWOOD_ext = extent(269000, 274000, 072000, 072500), #WHITEWOOD SX6972
                     BLACKPOOL_ext = extent(268000, 273000, 072000, 072500) #BLACKPOOL SX6872
                     
)

####ESTIMATE DAILY MEANS FOR APRIL TO JUNE OVER STUDY PERIOD####

#create the tas_files list for tasmin and tasmax in turn, running the loop each time to extract data
tas_files <- list.files("XXX/tasmax/", full.names = TRUE)

# Create separate columns for each day in April, May, and June (April_01, April_02, ..., June_30)
for (day in 1:30) {
  # Create columns for April (April_01, April_02, ..., April_30)
  data[[paste0("April_", sprintf("%02d", day))]] <- NA
}

for (day in 1:31) {
  # Create columns for May (May_01, May_02, ..., May_31)
  data[[paste0("May_", sprintf("%02d", day))]] <- NA
}

for (day in 1:30) {
  # Create columns for June (June_01, June_02, ..., June_30)
  data[[paste0("June_", sprintf("%02d", day))]] <- NA
}

# Function to extract the temperature for a specific day within a month
get_daily_temp_from_file <- function(site_ext, site_name, month, year) {
  # Generate the filename for the full month (e.g., "202304" for April 2023)
  month_str <- paste0(year, month)
  
  # Select files matching the month (April = 04, May = 05, June = 06)
  selected_files <- tas_files[grepl(month_str, tas_files)]  # Match files for the specific month
  
  # Check if the file exists
  if (length(selected_files) > 0) {
    # Assuming there is only one file per month, open the netCDF file
    month_file <- selected_files[1]
    r <- brick(month_file)  # Create a RasterBrick (each layer is a day)
    
    # Project and crop to the site extent
    r <- projectRaster(r, crs = CRS("+init=epsg:27700"))
    r <- crop(r, site_ext)
    
    return(r)  # Return the RasterBrick object for the entire month
  }
  
  return(NULL)  # Return NULL if no file is found
}

# Loop through each row in the data to process April, May, and June
for (i in 1:nrow(data)) {
  site_name <- as.character(data$site[i])  # Get site name
  site_ext_name <- paste0(site_name, "_ext")  # Append '_ext' to get the site extent name
  
  # Get the corresponding extent for the site
  site_ext <- site_extents[[site_ext_name]]
  
  # Process only if the extent is valid
  if (inherits(site_ext, "Extent")) {
    # Loop through April, May, and June (months 04, 05, and 06)
    for (month in c("04", "05", "06")) {
      # Get the number of days in the month (30 for April and June, 31 for May)
      days_in_month <- if (month == "05") 31 else 30
      
      # Get the raster data for the entire month
      monthly_raster <- get_daily_temp_from_file(site_ext, site_name, month, data$year[i])
      
      # If the raster data for the month is valid, extract the daily data
      if (!is.null(monthly_raster)) {
        # Loop through each day of the month
        for (day in 1:days_in_month) {
          # Extract the temperature data for the specific day (assuming each layer is a day)
          day_raster <- monthly_raster[[day]]
          
          # Calculate the mean for the specific day
          daily_temp_values <- values(day_raster)
          daily_mean <- mean(daily_temp_values, na.rm = TRUE)
          
          # Store the daily mean in the corresponding column
          if (!is.na(daily_mean)) {
            if (month == "04") {
              data[[paste0("April_", sprintf("%02d", day))]][i] <- daily_mean
            } else if (month == "05") {
              data[[paste0("May_", sprintf("%02d", day))]][i] <- daily_mean
            } else if (month == "06") {
              data[[paste0("June_", sprintf("%02d", day))]][i] <- daily_mean
            }
          } else {
            # If no valid data for the day, assign NA
            if (month == "04") {
              data[[paste0("April_", sprintf("%02d", day))]][i] <- NA
            } else if (month == "05") {
              data[[paste0("May_", sprintf("%02d", day))]][i] <- NA
            } else if (month == "06") {
              data[[paste0("June_", sprintf("%02d", day))]][i] <- NA
            }
          }
        }
      } else {
        # If no data for the month, assign NA to all days of the month
        for (day in 1:days_in_month) {
          if (month == "04") {
            data[[paste0("April_", sprintf("%02d", day))]][i] <- NA
          } else if (month == "05") {
            data[[paste0("May_", sprintf("%02d", day))]][i] <- NA
          } else if (month == "06") {
            data[[paste0("June_", sprintf("%02d", day))]][i] <- NA
          }
        }
      }
    }
  } else {
    # Invalid extent, assign NA to all day columns
    for (month in c("04", "05", "06")) {
      days_in_month <- if (month == "05") 31 else 30
      for (day in 1:days_in_month) {
        if (month == "04") {
          data[[paste0("April_", sprintf("%02d", day))]][i] <- NA
        } else if (month == "05") {
          data[[paste0("May_", sprintf("%02d", day))]][i] <- NA
        } else if (month == "06") {
          data[[paste0("June_", sprintf("%02d", day))]][i] <- NA
        }
      }
    }
  }
}

#label files as max/min
#add column with max or min
data_min<-data
data_max<-data

data_min$temp_type <- 'tasmin'
data_max$temp_type <- 'tasmax'

data_min<-as.data.frame(pivot_longer(data_min, 3:93, names_to = "day",
                                     values_to = "temp", values_drop_na = FALSE))

data_max<-as.data.frame(pivot_longer(data_max, 3:93, names_to = "day",
                                     values_to = "temp", values_drop_na = FALSE))

data_total <- data_min %>%
  inner_join(data_max, by = c("site","year","day"))

colnames(data_total)[colnames(data_total) == 'temp.x'] <- 'tasmin'
colnames(data_total)[colnames(data_total) == 'temp.y'] <- 'tasmax'

data_total <- data_total[,-c(3,6)]

data_total <- data_total %>% mutate(tasmean = (tasmin+tasmax)/2)

data_total_long <- data_total
data_total_wide <- data_total_long[,-(4:5)]
data_total_wide <- data_max<-as.data.frame(pivot_wider(data_total_wide, names_from = "day",
                                                       values_from = "tasmean"))

#write and save files
write.csv(data_total_long,file=XXX, row.names=FALSE)
write.csv(data_total_wide,file=XXX, row.names=FALSE)

####ESTIMATE MONTHLY MEANS for relevant site/year combinations####

tas_files <- list.files("XXX/tasmin/", full.names = TRUE)

# Modify the data dataframe to add columns for April, May, and June temperatures
data$mean_tasmin_April <- NA
data$mean_tasmin_May <- NA
data$mean_tasmin_June <- NA

# Function to extract the average temperature for a specific month (April, May, June)
get_monthly_avg_temp <- function(site_ext, site_name, year, month) {
  # Filter weather files for the specific month and year
  selected_files <- tas_files[grepl(paste0(year, month), tas_files)]
  
  # Initialize a list to hold rasters for the selected month
  monthly_rasters <- list()
  
  if (length(selected_files) > 0) {
    # Loop through selected files (only one file per year/month should match)
    for (month_file in selected_files) {
      # Open the netCDF file
      r <- brick(month_file)
      
      # Project and crop to the site extent
      r <- projectRaster(r, crs = CRS("+init=epsg:27700"))
      r <- crop(r, site_ext)
      
      # Append the raster for this month
      monthly_rasters[[month]] <- r
    }
  }
  
  # If a valid raster for the month exists, calculate the mean
  if (length(monthly_rasters) > 0) {
    monthly_stack <- stack(monthly_rasters[[month]])
    avg_temp <- calc(monthly_stack, mean)
    return(avg_temp)
  } else {
    return(NA)
  }
}

# Loop through each row in the data to process April, May, and June
for (i in 1:nrow(data)) {
  site_name <- as.character(data$site[i])  # Get site name
  site_ext_name <- paste0(site_name, "_ext")  # Append '_ext' to get the site extent name
  
  # Get the corresponding extent for the site
  site_ext <- site_extents[[site_ext_name]]
  
  # Process only if the extent is valid
  if (inherits(site_ext, "Extent")) {
    # Calculate the average temperature for April
    april_temp_raster <- get_monthly_avg_temp(site_ext, site_name, data$year[i], "04")
    
    # If a valid raster for April is returned, calculate the mean and store it
    if (inherits(april_temp_raster, "Raster") && nlayers(april_temp_raster) > 0) {
      april_temp_values <- values(april_temp_raster)
      data$mean_tasmin_April[i] <- ifelse(!all(is.na(april_temp_values)), mean(april_temp_values, na.rm = TRUE), NA)
    } else {
      # If no valid raster for April, assign NA
      data$mean_tasmin_April[i] <- NA
    }
    
    # Calculate the average temperature for May
    may_temp_raster <- get_monthly_avg_temp(site_ext, site_name, data$year[i], "05")
    
    # If a valid raster for May is returned, calculate the mean and store it
    if (inherits(may_temp_raster, "Raster") && nlayers(may_temp_raster) > 0) {
      may_temp_values <- values(may_temp_raster)
      data$mean_tasmin_May[i] <- ifelse(!all(is.na(may_temp_values)), mean(may_temp_values, na.rm = TRUE), NA)
    } else {
      # If no valid raster for May, assign NA
      data$mean_tasmin_May[i] <- NA
    }
    
    # Calculate the average temperature for June
    june_temp_raster <- get_monthly_avg_temp(site_ext, site_name, data$year[i], "06")
    
    # If a valid raster for June is returned, calculate the mean and store it
    if (inherits(june_temp_raster, "Raster") && nlayers(june_temp_raster) > 0) {
      june_temp_values <- values(june_temp_raster)
      data$mean_tasmin_June[i] <- ifelse(!all(is.na(june_temp_values)), mean(june_temp_values, na.rm = TRUE), NA)
    } else {
      # If no valid raster for June, assign NA
      data$mean_tasmin_June[i] <- NA
    }
    
  } else {
    # Invalid extent, assign NA for all months
    data$mean_tasmin_April[i] <- NA
    data$mean_tasmin_May[i] <- NA
    data$mean_tasmin_June[i] <- NA
  }
}

data_min<-data

####MAX TEMPS

tas_files <- list.files("XXX/tasmax/", full.names = TRUE)

# Modify the data dataframe to add columns for April, May, and June temperatures
data$mean_tasmax_April <- NA
data$mean_tasmax_May <- NA
data$mean_tasmax_June <- NA

# Function to extract the average temperature for a specific month (April, May, June)
get_monthly_avg_temp <- function(site_ext, site_name, year, month) {
  # Filter weather files for the specific month and year
  selected_files <- tas_files[grepl(paste0(year, month), tas_files)]
  
  # Initialize a list to hold rasters for the selected month
  monthly_rasters <- list()
  
  if (length(selected_files) > 0) {
    # Loop through selected files (only one file per year/month should match)
    for (month_file in selected_files) {
      # Open the netCDF file
      r <- brick(month_file)
      
      # Project and crop to the site extent
      r <- projectRaster(r, crs = CRS("+init=epsg:27700"))
      r <- crop(r, site_ext)
      
      # Append the raster for this month
      monthly_rasters[[month]] <- r
    }
  }
  
  # If a valid raster for the month exists, calculate the mean
  if (length(monthly_rasters) > 0) {
    monthly_stack <- stack(monthly_rasters[[month]])
    avg_temp <- calc(monthly_stack, mean)
    return(avg_temp)
  } else {
    return(NA)
  }
}

# Loop through each row in the data to process April, May, and June
for (i in 1:nrow(data)) {
  site_name <- as.character(data$site[i])  # Get site name
  site_ext_name <- paste0(site_name, "_ext")  # Append '_ext' to get the site extent name
  
  # Get the corresponding extent for the site
  site_ext <- site_extents[[site_ext_name]]
  
  # Process only if the extent is valid
  if (inherits(site_ext, "Extent")) {
    # Calculate the average temperature for April
    april_temp_raster <- get_monthly_avg_temp(site_ext, site_name, data$year[i], "04")
    
    # If a valid raster for April is returned, calculate the mean and store it
    if (inherits(april_temp_raster, "Raster") && nlayers(april_temp_raster) > 0) {
      april_temp_values <- values(april_temp_raster)
      data$mean_tasmax_April[i] <- ifelse(!all(is.na(april_temp_values)), mean(april_temp_values, na.rm = TRUE), NA)
    } else {
      # If no valid raster for April, assign NA
      data$mean_tasmax_April[i] <- NA
    }
    
    # Calculate the average temperature for May
    may_temp_raster <- get_monthly_avg_temp(site_ext, site_name, data$year[i], "05")
    
    # If a valid raster for May is returned, calculate the mean and store it
    if (inherits(may_temp_raster, "Raster") && nlayers(may_temp_raster) > 0) {
      may_temp_values <- values(may_temp_raster)
      data$mean_tasmax_May[i] <- ifelse(!all(is.na(may_temp_values)), mean(may_temp_values, na.rm = TRUE), NA)
    } else {
      # If no valid raster for May, assign NA
      data$mean_tasmax_May[i] <- NA
    }
    
    # Calculate the average temperature for June
    june_temp_raster <- get_monthly_avg_temp(site_ext, site_name, data$year[i], "06")
    
    # If a valid raster for June is returned, calculate the mean and store it
    if (inherits(june_temp_raster, "Raster") && nlayers(june_temp_raster) > 0) {
      june_temp_values <- values(june_temp_raster)
      data$mean_tasmax_June[i] <- ifelse(!all(is.na(june_temp_values)), mean(june_temp_values, na.rm = TRUE), NA)
    } else {
      # If no valid raster for June, assign NA
      data$mean_tasmax_June[i] <- NA
    }
    
  } else {
    # Invalid extent, assign NA for all months
    data$mean_tasmax_April[i] <- NA
    data$mean_tasmax_May[i] <- NA
    data$mean_tasmax_June[i] <- NA
  }
}

data_max<-data

####calculate average

data_total <- data_min %>%
  inner_join(data_max, by = c("site","year","day"))

data_total <- data %>% mutate(tasmean_April = (mean_tasmin_April+mean_tasmax_April)/2) %>%
  mutate(tasmean_May = (mean_tasmin_May+mean_tasmax_May)/2) %>%
  mutate(tasmean_June = (mean_tasmin_June+mean_tasmax_June)/2)

#save files
write.csv(data_total,file="XXX/mean_monthly_temp.csv", row.names=FALSE)


####ESTIMATE MONTHLY MEANS for all site/year combinations####
tas_files <- list.files("XXX/tasmin/", full.names = TRUE)

# Modify the data dataframe to add columns for April, May, and June temperatures
all_year_site_combos$mean_tasmin_April <- NA
all_year_site_combos$mean_tasmin_May <- NA
all_year_site_combos$mean_tasmin_June <- NA

# Function to extract the average temperature for a specific month (April, May, June)
get_monthly_avg_temp <- function(site_ext, site_name, year, month) {
  # Filter weather files for the specific month and year
  selected_files <- tas_files[grepl(paste0(year, month), tas_files)]
  
  # Initialize a list to hold rasters for the selected month
  monthly_rasters <- list()
  
  if (length(selected_files) > 0) {
    # Loop through selected files (only one file per year/month should match)
    for (month_file in selected_files) {
      # Open the netCDF file
      r <- brick(month_file)
      
      # Project and crop to the site extent
      r <- projectRaster(r, crs = CRS("+init=epsg:27700"))
      r <- crop(r, site_ext)
      
      # Append the raster for this month
      monthly_rasters[[month]] <- r
    }
  }
  
  # If a valid raster for the month exists, calculate the mean
  if (length(monthly_rasters) > 0) {
    monthly_stack <- stack(monthly_rasters[[month]])
    avg_temp <- calc(monthly_stack, mean)
    return(avg_temp)
  } else {
    return(NA)
  }
}

# Loop through each row in the data to process April, May, and June
for (i in 1:nrow(all_year_site_combos)) {
  site_name <- as.character(all_year_site_combos$site[i])  # Get site name
  site_ext_name <- paste0(site_name, "_ext")  # Append '_ext' to get the site extent name
  
  # Get the corresponding extent for the site
  site_ext <- site_extents[[site_ext_name]]
  
  # Process only if the extent is valid
  if (inherits(site_ext, "Extent")) {
    # Calculate the average temperature for April
    april_temp_raster <- get_monthly_avg_temp(site_ext, site_name, all_year_site_combos$year[i], "04")
    
    # If a valid raster for April is returned, calculate the mean and store it
    if (inherits(april_temp_raster, "Raster") && nlayers(april_temp_raster) > 0) {
      april_temp_values <- values(april_temp_raster)
      all_year_site_combos$mean_tasmin_April[i] <- ifelse(!all(is.na(april_temp_values)), mean(april_temp_values, na.rm = TRUE), NA)
    } else {
      # If no valid raster for April, assign NA
      all_year_site_combos$mean_tasmin_April[i] <- NA
    }
    
    # Calculate the average temperature for May
    may_temp_raster <- get_monthly_avg_temp(site_ext, site_name, all_year_site_combos$year[i], "05")
    
    # If a valid raster for May is returned, calculate the mean and store it
    if (inherits(may_temp_raster, "Raster") && nlayers(may_temp_raster) > 0) {
      may_temp_values <- values(may_temp_raster)
      all_year_site_combos$mean_tasmin_May[i] <- ifelse(!all(is.na(may_temp_values)), mean(may_temp_values, na.rm = TRUE), NA)
    } else {
      # If no valid raster for May, assign NA
      all_year_site_combos$mean_tasmin_May[i] <- NA
    }
    
    # Calculate the average temperature for June
    june_temp_raster <- get_monthly_avg_temp(site_ext, site_name, all_year_site_combos$year[i], "06")
    
    # If a valid raster for June is returned, calculate the mean and store it
    if (inherits(june_temp_raster, "Raster") && nlayers(june_temp_raster) > 0) {
      june_temp_values <- values(june_temp_raster)
      all_year_site_combos$mean_tasmin_June[i] <- ifelse(!all(is.na(june_temp_values)), mean(june_temp_values, na.rm = TRUE), NA)
    } else {
      # If no valid raster for June, assign NA
      all_year_site_combos$mean_tasmin_June[i] <- NA
    }
    
  } else {
    # Invalid extent, assign NA for all months
    all_year_site_combos$mean_tasmin_April[i] <- NA
    all_year_site_combos$mean_tasmin_May[i] <- NA
    all_year_site_combos$mean_tasmin_June[i] <- NA
  }
}

all_year_site_combos_min<-all_year_site_combos

####MAX TEMPS

tas_files <- list.files("XXX/tasmax/", full.names = TRUE)

# Modify the data dataframe to add columns for April, May, and June temperatures
all_year_site_combos$mean_tasmax_April <- NA
all_year_site_combos$mean_tasmax_May <- NA
all_year_site_combos$mean_tasmax_June <- NA

# Function to extract the average temperature for a specific month (April, May, June)
get_monthly_avg_temp <- function(site_ext, site_name, year, month) {
  # Filter weather files for the specific month and year
  selected_files <- tas_files[grepl(paste0(year, month), tas_files)]
  
  # Initialize a list to hold rasters for the selected month
  monthly_rasters <- list()
  
  if (length(selected_files) > 0) {
    # Loop through selected files (only one file per year/month should match)
    for (month_file in selected_files) {
      # Open the netCDF file
      r <- brick(month_file)
      
      # Project and crop to the site extent
      r <- projectRaster(r, crs = CRS("+init=epsg:27700"))
      r <- crop(r, site_ext)
      
      # Append the raster for this month
      monthly_rasters[[month]] <- r
    }
  }
  
  # If a valid raster for the month exists, calculate the mean
  if (length(monthly_rasters) > 0) {
    monthly_stack <- stack(monthly_rasters[[month]])
    avg_temp <- calc(monthly_stack, mean)
    return(avg_temp)
  } else {
    return(NA)
  }
}

# Loop through each row in the data to process April, May, and June
for (i in 1:nrow(all_year_site_combos)) {
  site_name <- as.character(all_year_site_combos$site[i])  # Get site name
  site_ext_name <- paste0(site_name, "_ext")  # Append '_ext' to get the site extent name
  
  # Get the corresponding extent for the site
  site_ext <- site_extents[[site_ext_name]]
  
  # Process only if the extent is valid
  if (inherits(site_ext, "Extent")) {
    # Calculate the average temperature for April
    april_temp_raster <- get_monthly_avg_temp(site_ext, site_name, all_year_site_combos$year[i], "04")
    
    # If a valid raster for April is returned, calculate the mean and store it
    if (inherits(april_temp_raster, "Raster") && nlayers(april_temp_raster) > 0) {
      april_temp_values <- values(april_temp_raster)
      all_year_site_combos$mean_tasmax_April[i] <- ifelse(!all(is.na(april_temp_values)), mean(april_temp_values, na.rm = TRUE), NA)
    } else {
      # If no valid raster for April, assign NA
      all_year_site_combos$mean_tasmax_April[i] <- NA
    }
    
    # Calculate the average temperature for May
    may_temp_raster <- get_monthly_avg_temp(site_ext, site_name, all_year_site_combos$year[i], "05")
    
    # If a valid raster for May is returned, calculate the mean and store it
    if (inherits(may_temp_raster, "Raster") && nlayers(may_temp_raster) > 0) {
      may_temp_values <- values(may_temp_raster)
      all_year_site_combos$mean_tasmax_May[i] <- ifelse(!all(is.na(may_temp_values)), mean(may_temp_values, na.rm = TRUE), NA)
    } else {
      # If no valid raster for May, assign NA
      all_year_site_combos$mean_tasmax_May[i] <- NA
    }
    
    # Calculate the average temperature for June
    june_temp_raster <- get_monthly_avg_temp(site_ext, site_name, all_year_site_combos$year[i], "06")
    
    # If a valid raster for June is returned, calculate the mean and store it
    if (inherits(june_temp_raster, "Raster") && nlayers(june_temp_raster) > 0) {
      june_temp_values <- values(june_temp_raster)
      all_year_site_combos$mean_tasmax_June[i] <- ifelse(!all(is.na(june_temp_values)), mean(june_temp_values, na.rm = TRUE), NA)
    } else {
      # If no valid raster for June, assign NA
      all_year_site_combos$mean_tasmax_June[i] <- NA
    }
    
  } else {
    # Invalid extent, assign NA for all months
    all_year_site_combos$mean_tasmax_April[i] <- NA
    all_year_site_combos$mean_tasmax_May[i] <- NA
    all_year_site_combos$mean_tasmax_June[i] <- NA
  }
}

all_year_site_combos_max<-all_year_site_combos

####calculate average

all_year_site_combos_total <- all_year_site_combos_min %>%
  inner_join(all_year_site_combos_max, by = c("site","year"))

all_year_site_combos_total <- all_year_site_combos_total %>% mutate(tasmean_April = (mean_tasmin_April+mean_tasmax_April)/2) %>%
  mutate(tasmean_May = (mean_tasmin_May+mean_tasmax_May)/2) %>%
  mutate(tasmean_June = (mean_tasmin_June+mean_tasmax_June)/2) %>%
  mutate(tasmean_AprMay = (tasmean_April+tasmean_May)/2)

#save files
write.csv(all_year_site_combos_total,file=XXX/mean_monthly_temp_all_year_site_combos.csv, row.names=FALSE)


