install.packages( 'https://raw.github.com/CDC-DNPAO/CDCAnthro/master/cdcanthro_0.1.2.tar.gz', type='source', repos=NULL )
install.packages("jsonlite")
install.packages("data.table")
install.packages("here")
install.packages("lubridate")
install.packages("devtools")
library(devtools)
library(lubridate)
library(jsonlite)
library(data.table)
library(here)
library(cdcanthro)
json_file_path <- here("sds_age_validation_2021.json")
json_data <- fromJSON(json_file_path)
growth_data <- data.table(json_data)
 
# Convert birth_date and observation_date to date format
growth_data[, birth_date := dmy(birth_date)]
growth_data[, observation_date := dmy(observation_date)]
 
# Calculate age in months
growth_data[, age_months := as.numeric(difftime(observation_date, birth_date, units = "days"))/30.4375]
growth_data[, decimal_age := as.numeric(difftime(observation_date, birth_date, units = "days"))/365.25]
 
# filter out rows with age_months < 24
growth_data <- growth_data[age_months >= 24]
 
# Define measurement methods to process
measurement_methods <- c("height","weight", "bmi")
 
# Loop through each measurement method
for (method in measurement_methods) {
  # Filter data for the current measurement method
  method_data <- growth_data[measurement_method == method]
 
  # Set the appropriate column based on the measurement method
  if (method == "weight") {
    method_data[, wt := as.numeric(observation_value)]
    method_data[, ht := NA]
    method_data[, bmi_opt := NA]
    return_column <- "waz"
  } else if (method == "height") {
    method_data[, ht := as.numeric(observation_value)]
    method_data[, wt := NA]
    method_data[, bmi_opt := NA]
    return_column <- "haz"
  } else if (method == "bmi") {
    method_data[, bmi_opt := as.numeric(observation_value)]
    method_data[, wt := NA]
    method_data[, ht := NA]
    return_column <- "bmiz"
  }
 
  # Apply the cdcanthro function
  anthro_data <- cdcanthro(
    data = method_data,
    age = age_months,
    wt = wt,
    ht = ht,
    bmi = bmi_opt,
    all = TRUE
  )

  if (method =="bmi"){
    print(head(head(anthro_data)))
  }
 
 
 output_file <- sprintf("anthro_output_%s.json", method)
 
 z_score<-anthro_data[[return_column]]

 anthro_data[, birth_date := format(birth_date, format = "%d/%m/%Y")]
 anthro_data[, observation_date := format(observation_date, format = "%d/%m/%Y")]
 
  # Write the output to a JSON file where the weight z-score is not missing, retaining only the specified columns
  anthro_data[!is.na(z_score),.(birth_date, observation_date, decimal_age, age_months, sex, measurement_method, observation_value, z_score)][, write_json(.SD, here(output_file))]
}
