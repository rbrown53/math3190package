## code to prepare `car_price_prediction` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
car_price_prediction <- read.csv("data-raw/car_price_prediction.csv") |>
  as_tibble()
usethis::use_data(car_price_prediction, overwrite = TRUE)
