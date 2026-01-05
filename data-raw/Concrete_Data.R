## code to prepare `Concrete_Data` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
library(readxl) |> suppressPackageStartupMessages()
Concrete_Data <- read_excel("data-raw/Concrete_Data.xls") |>
  as_tibble()
usethis::use_data(Concrete_Data, overwrite = TRUE)
