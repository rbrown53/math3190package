## code to prepare `blood_pressure` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
blood_pressure <- read.table("data-raw/blood_pressure.txt", header = TRUE) |>
  as_tibble()
usethis::use_data(blood_pressure, overwrite = TRUE)
