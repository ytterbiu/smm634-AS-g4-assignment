library(readr)
library(dplyr)
library(purrr)
library(stringr)

# ---- Paths (adjust if needed) ----
data_path <- "automobile/imports-85.data"
names_path <- "automobile/imports-85.names" # optional: to peek at the info file

auto <- read_csv(
  file = data_path,
  col_names = names_path,
  na = c("?", "NA")
)

summary(auto)
