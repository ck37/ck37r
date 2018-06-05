library(ck37r)

# Only run test if necessary suggested packages are installed.
pkg_suggests = c("testthat", "mlbench")
if (!all(suppressWarnings(sapply(pkg_suggests, require, quietly = TRUE,
                                 character.only = TRUE))))
  return()

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

# Add a factor variable with a single value; it should be removed.
data$single_level = as.factor(1)

results = factors_to_indicators(data, verbose = TRUE)
names(results)

# TODO: add test with missing values.
