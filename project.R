library(tidyverse)
library(sandwich)

# Import data
house_sales <- read_csv('kc_house_data.csv')
str(house_sales)

# Recode yr_renovated so that if the house was never renovated, the value is the year it was built
house_sales$yr_renovated_fixed <- ifelse(house_sales$yr_renovated == 0, house_sales$yr_built, house_sales$yr_renovated)
# Recode waterfront from numeric to categorical
house_sales$waterfront <- factor(house_sales$waterfront)

# Make univariate models to assess which variables to consider including in multivariate model
model_sqft_living <- lm(price ~ sqft_living, house_sales)
summary(model_sqft_living)

model_sqft_above <- lm(price ~ sqft_above, house_sales)
summary(model_sqft_above)

model_sqft_lot <- lm(price ~ sqft_lot, house_sales) 
summary(model_sqft_lot)

model_yr_built <- lm(price ~ yr_built, house_sales)
summary(model_yr_built)

model_yr_renovated <- lm(price ~ yr_renovated_fixed, house_sales)
summary(model_yr_renovated)

model_waterfront <- lm(price ~ waterfront, house_sales)
summary(model_waterfront)

model_grade <- lm(price ~ grade, house_sales)
summary(model_grade)

# Given R^2 values and significance of coefficients, candidate variables are
# grade, sqft_living, and waterfront

model_liv_grade <- lm(price ~ grade + sqft_living, house_sales)
summary(model_liv_grade)
plot(model_liv_grade, which=1)
plot(model_liv_grade, which=2)
plot(model_liv_grade, which=3)

model_liv_grade_interaction <- lm(price ~ grade * sqft_living, house_sales)
summary(model_liv_grade_interaction)
plot(model_liv_grade_interaction, which=1)
plot(model_liv_grade_interaction, which=2)
plot(model_liv_grade_interaction, which=3)

model_liv_grade_water <- lm(price ~ grade*sqft_living + waterfront, house_sales)
summary(model_liv_grade_water)
plot(model_liv_grade_water, which=1)
plot(model_liv_grade_water, which=2)
plot(model_liv_grade_water, which=3)

model_liv_grade_water_interaction1 <- lm(price ~ grade*sqft_living + waterfront*grade, house_sales)
summary(model_liv_grade_water_interaction1)
plot(model_liv_grade_water_interaction1, which=1)
plot(model_liv_grade_water_interaction1, which=2)
plot(model_liv_grade_water_interaction1, which=3)

model_liv_grade_water_interaction2 <- lm(price ~ grade*sqft_living + waterfront*sqft_living, house_sales)
summary(model_liv_grade_water_interaction2)
plot(model_liv_grade_water_interaction2, which=1)
plot(model_liv_grade_water_interaction2, which=2)
plot(model_liv_grade_water_interaction2, which=3)









