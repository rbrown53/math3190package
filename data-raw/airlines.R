## code to prepare `airlines` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
airlines <- read.csv("data-raw/airlines.csv") |>
  as_tibble()
usethis::use_data(airlines, overwrite = TRUE)
