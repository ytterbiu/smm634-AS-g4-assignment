## Read in data
rm(list = ls())
source(file.path(".", "CarDataRead.R"))

## dependencies / external librarys
library(dplyr)
library(ggplot2)
library(car)
library(MASS)

## Clean data
df$carManufacturerClean <- sapply(strsplit(df$CarName, " +"), `[`, 1)


## Analysis 1
source(file.path(".", "CarAnalysisSafetyPrice.R"))
# clean this file so it doesn't have overlap
# summary of what this shows:...


## Analysis 2 (looking at mpg)
source(file.path(".", "CarAnalysisMPG.R"))
