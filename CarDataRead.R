df <- read.csv("car_price.csv") |>
  mutate(fsymboling = as.factor(symboling)) |>
  mutate(safety = case_match(
    symboling,
    c(-2, -1) ~ "<-1",
    0 ~ "0",
    1 ~ "1",
    2 ~ "2",
    3 ~ "3"
  )) |>
  mutate(safetyIncr = case_match(
    symboling,
    c(-2, -1) ~ "4",
    0 ~ "3",
    1 ~ "2",
    2 ~ "1",
    3 ~ "0"
  )) |>
  mutate(safetyIncr2 = case_match(
    symboling,
    0 ~ "Base",
    c(-2, -1) ~ "Safer",
    c(1, 2, 3) ~ "Riskier"
  )) |>
  mutate(cylinderNum = case_match(cylindernumber,
    c("two", "three") ~ "<three",
    .default = cylindernumber
  ))
