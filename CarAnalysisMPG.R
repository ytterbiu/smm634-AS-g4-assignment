lm.mpg1 <- lm(
  price ~ safetyIncr + aspiration + carbody +
    enginelocation + carwidth + curbweight + enginetype +
    cylinderNum + enginesize + stroke + peakrpm + compressionratio +
    carManufacturerClean + citympg,
  data = df
)

summary(lm.mpg1)

par(mfrow = c(2, 2))
plot(lm.mpg1)

vif(lm.mpg1)

# step AIC

stepAIC(lm.mpg1, ~ safetyIncr + aspiration + carbody +
  enginelocation + carwidth + curbweight + enginetype +
  cylinderNum + enginesize + stroke + peakrpm + compressionratio +
  carManufacturerClean + citympg,
data = df, direction = c("backward")
)

# anova(lm1, lm4)

######## attempt 2
vars <- c(
  "price", "safetyIncr", "aspiration", "carbody", "enginelocation",
  "carwidth", "curbweight", "enginetype", "cylinderNum", "enginesize",
  "stroke", "peakrpm", "compressionratio", "carManufacturerClean", "citympg"
)

dfm <- df[, vars]
dfm <- dfm[complete.cases(dfm), ]

# Drop unused levels to avoid viv errors
dfm[] <- lapply(dfm, function(x) if (is.factor(x)) droplevels(x) else x)

lm.mpg2 <- lm(
  price ~ safetyIncr + aspiration + carbody +
    enginelocation + carwidth + curbweight + enginetype +
    cylinderNum + enginesize + stroke + peakrpm + compressionratio +
    carManufacturerClean + citympg,
  data = dfm
)

summary(lm.mpg2)
car::vif(lm.mpg2)
