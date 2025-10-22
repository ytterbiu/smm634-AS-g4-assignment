library(dplyr)
library(ggplot2)
library(car)

# df <- read.csv('C:\\Users\\ardih\\Study\\City_St_George\\SMM634_Analytics_Methods_for_Business\\Assignment1\\car_price.csv') |>
df <- read.csv('car_price.csv') |>
  mutate(fsymboling = as.factor(symboling)) |> 
  mutate(safety = case_match(symboling,
                             c(-2, -1) ~ '<-1',
                             0 ~ '0',
                             1 ~ '1',
                             2 ~ '2',
                             3 ~ '3' ))|>
  mutate(safetyIncr = case_match(symboling,
                                 c(-2, -1) ~ '4',
                                 0 ~ '3',
                                 1 ~ '2',
                                 2 ~ '1',
                                 3 ~ '0' ))|>
  mutate(safetyIncr2 = case_match(symboling,
                                 0 ~ 'Base',
                                 c(-2, -1) ~ 'Safer',
                                 c(1, 2, 3) ~ 'Riskier'))|>
  mutate(cylinderNum = case_match(cylindernumber,
                                  c("two", "three") ~ '<three',
                                  .default = cylindernumber))

# get car name
df$CarName
# carManufacturer <- strsplit(df$CarName, " +")[,1]
df$carManufacturer <- sapply(strsplit(df$CarName," +"), `[`, 1)

summary(df)
str(df)

colSums(is.na(df))

table(df$symboling)
table(df$safety)
table(df$safetyIncr)

table(df$cylindernumber)
table(df$cylinderNum)

table(df$carManufacturer)
# correct spelling errors
# Assumptions:
# > maxda is a typo of mazda
# > Nissan -> nissan
# > porcshce -> porsche
# > toyouta -> toyota
# > vokswagen -> volkswagen
# > vw -> volkswagen
df <-df %>% mutate(carManufacturer = case_match(carManufacturer,
                               'maxda' ~ 'mazda',
                               'Nissan' ~ 'nissan',
                               'porcshce' ~ 'porsche',
                               'toyouta' ~ 'toyota',
                               c('vokswagen', 'vw') ~ 'volkswagen',
                               .default = carManufacturer         
                               ))
table(df$carManufacturer)

lm1 <- lm(price ~ symboling + aspiration + carbody +
            enginelocation + carwidth + curbweight + enginetype +
            cylinderNum + enginesize + stroke + peakrpm + compressionratio
          , data=df)
# if we miss any variables then the estimate for the variables we included will 
# be biased (Lec 2?) - omitted variable bias
# Including all the variables in lm1 to avoid omitted variable bias
# so included all of the variables above in lm1

summary(lm1)
# what is the intercept here?
# A: Hard to interpret... if all other variables were 0 (but this includes 
# categorical variables)
# means that price is negative if everything is set to 0?
# doesn't have real meaning, but this is okay. Artifact of the model to make it 
# work

par(mfrow=c(2,2))
plot(lm1)
# RvsF - there is initial non-linearity that we may be able to fix
# QQ - slight non-normality
# Cook's distance is okay
# No outliers or high leverage
# BE note: include additional plot

vif(lm1)
# everything looks okay (i.e., <5)
# Enginesize is boardline, along with curb weight

lm2 <- lm(price ~ fsymboling + aspiration + carbody +
            enginelocation + carwidth + curbweight + enginetype +
            cylinderNum + enginesize + stroke + peakrpm + compressionratio
          , data=df)

summary(lm2)
# Note, for symboling the -2 hasn't been combined
# Baseline for symboling is -2

par(mfrow=c(2,2))
plot(lm2)

vif(lm2)
# engine size is a bit higher for GVIF

lm3 <- lm(price ~ safetyIncr + aspiration + carbody +
            enginelocation + carwidth + curbweight + enginetype +
            cylinderNum + enginesize + stroke + peakrpm + compressionratio +
            carManufacturer
          , data=df)

summary(lm3)
# Baseline is most risky (3 under old category)

par(mfrow=c(2,2))
plot(lm3)

vif(lm3)

## new model with safety increment 2

lm4 <- lm(price ~ safetyIncr + aspiration + carbody +
            enginelocation + carwidth + curbweight + enginetype +
            cylinderNum + enginesize + stroke + peakrpm + compressionratio +
            carManufacturer
          , data=df)

summary(lm4)
# Baseline is most risky (3 under old category)

par(mfrow=c(2,2))
plot(lm4)

vif(lm4)

# step AIC
library(MASS)
stepAIC(lm3, ~ safetyIncr + aspiration + carbody +
  enginelocation + carwidth + curbweight + enginetype +
  cylinderNum + enginesize + stroke + peakrpm + compressionratio +
  carManufacturer
, data=df, direction = c("backward"))

anova(lm1, lm4)

########



