## code to prepare `CollegeDistance` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
CollegeDistance <- read.csv("data-raw/CollegeDistance.csv") |>
  as_tibble()
usethis::use_data(CollegeDistance, overwrite = TRUE)
