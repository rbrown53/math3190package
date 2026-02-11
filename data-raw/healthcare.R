## code to prepare `healthcare` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
healthcare <- read.table("data-raw/healthcare.txt", header = TRUE) |>
  as_tibble()
usethis::use_data(healthcare, overwrite = TRUE)
