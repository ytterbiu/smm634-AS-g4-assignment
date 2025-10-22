library(dplyr)
library(ggplot2)
library(car)

df <- read.csv('C:\\Users\\ardih\\Study\\City_St_George\\SMM634_Analytics_Methods_for_Business\\Assignment1\\car_price.csv') |>
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
  mutate(cylinderNum = case_match(cylindernumber,
                                  c("two", "three") ~ '<three',
                                  .default = cylindernumber))


summary(df)
str(df)

table(df$symboling)
table(df$safety)
table(df$safetyIncr)

table(df$cylindernumber)
table(df$cylinderNum)

lm1 <- lm(price ~ symboling + aspiration + carbody +
            enginelocation + carwidth + curbweight + enginetype +
            cylinderNum + enginesize + stroke + peakrpm + compressionratio
          , data=df)

summary(lm1)

par(mfrow=c(2,2))
plot(lm1)

vif(lm1)

lm2 <- lm(price ~ fsymboling + aspiration + carbody +
            enginelocation + carwidth + curbweight + enginetype +
            cylinderNum + enginesize + stroke + peakrpm + compressionratio
          , data=df)

summary(lm2)

par(mfrow=c(2,2))
plot(lm2)

vif(lm2)


lm3 <- lm(price ~ safetyIncr + aspiration + carbody +
            enginelocation + carwidth + curbweight + enginetype +
            cylinderNum + enginesize + stroke + peakrpm + compressionratio
          , data=df)

summary(lm3)

par(mfrow=c(2,2))
plot(lm3)

vif(lm3)



