library(ck37r)
library(testthat)

data(Ozone, package = "mlbench")

data = Ozone

# Add column names.
names(data) =
  tolower(c("Month", "Day_of_month", "Day_of_week", "ozone_reading",
            "pressure_height", "Wind_speed", "Humidity", "Temperature_Sandburg",
            "Temperature_ElMonte", "Inversion_base_height", "Pressure_gradient",
            "Inversion_temperature", "Visibility"))
str(data)

# Convert factors to numerics
categoricals = names(which(sapply(data, is.factor)))
categoricals

results = factors_to_indicators(data, verbose = T)
