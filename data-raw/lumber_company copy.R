## code to prepare `lumber_company` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
lumber_company <- read.table("data-raw/lumber_company.txt", header = TRUE) |>
  as_tibble()
usethis::use_data(lumber_company, overwrite = TRUE)
