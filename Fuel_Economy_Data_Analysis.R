# 1. Add ggplot2::mpg dataset.
# 2. Make data ready for analysis doing preprocessing techniques.
# 3. Fit Generalized Linear Model using H2O in R.
# 4. Run GLM using following modelling structure. cty ~ year + cyl + displ.
# 5. Print coefficients table and give interpretation of results.
# 6. Name your final homework Script as “Fuel_Economy_Data_Analysis”.

library(tidyverse) 
library(data.table)
library(rstudioapi)
library(skimr)
library(inspectdf)
library(mice)
library(plotly)
library(highcharter)
library(recipes) 
library(caret) 
library(purrr) 
library(graphics) 
library(Hmisc) 
library(glue)
library(h2o)
library(dplyr)

df <- ggplot2::mpg
df %>% skim()
df %>% summary()

                                       #Data Preprocessing
# Fill NA's
df %>% inspect_na()

#Encoding
df$year <- df$year %>% as.factor()
df$cyl<- df$cyl %>% as.factor()
df$year <- df$year %>% as.numeric()
df$cyl<- df$cyl %>% as.numeric()
df %>% skim()

# Multicollinearity
target <- 'cty'
features <- df %>% select('year', 'cyl', 'displ') %>% names()

f <- as.formula(paste(target, paste(features, collapse = '+'), sep = '~'))
glm <- glm(f, data = df)
glm %>% summary()

df <- df %>% select(cty, features)

#Modelling

h2o.init()

h2o_data <- df %>% as.h2o()

# Splitting the data
h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]

# Fitting h2o model
model <- h2o.glm(
  x = features, y = target,
  training_frame = train,
  validation_frame = test,
  nfolds = 10, seed = 123,
  lambda = 0, compute_p_values = F)

# Predicting the Test set results
y_pred <- model %>% h2o.predict(newdata = test) %>% as.data.frame()
y_pred$predict

# Model evaluation 
test_set <- test %>% as.data.frame()
residuals = test_set$cty - y_pred$predict

# Calculate RMSE (Root Mean Square Error)
RMSE = sqrt(mean(residuals^2))

# Calculate Adjusted R2 (R Squared)
y_test_mean = mean(test_set$cty)

#total sum of squares
tss = sum((test_set$cty - y_test_mean)^2)

#residual sum of squares
rss = sum(residuals^2) 

R2 = 1 - (rss/tss); R2

#sample size
n <- test_set %>% nrow()

#number of independent variables
k <- features %>% length() 

Adjusted_R2 = 1-(1-R2)*((n-1)/(n-k-1))

tibble(RMSE = round(RMSE,1),
       R2, Adjusted_R2)

# Plotting actual & predicted 
my_data <- cbind(predicted = y_pred$predict,
                 observed = test_set$cty) %>% 
  as.data.frame()

g <- my_data %>% 
  ggplot(aes(predicted, observed)) + 
  geom_point(color = "darkblue") + 
  geom_smooth(method=lm) + 
  labs(x="Predecited Power Output", 
       y="Observed Power Output",
       title=glue('Test: Adjusted R2 = {round(enexpr(Adjusted_R2),2)}')) +
  theme(plot.title = element_text(color="red",size=16,hjust=0.5),
        axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14))

g %>% ggplotly()

# Check overfitting 
y_pred_train <- model %>% h2o.predict(newdata = train) %>% as.data.frame()

train_set <- train %>% as.data.frame()
residuals = train_set$cty - y_pred_train$predict

RMSE_train = sqrt(mean(residuals^2))
y_train_mean = mean(train_set$cty)

tss = sum((train_set$cty - y_train_mean)^2)
rss = sum(residuals^2)

R2_train = 1 - (rss/tss); R2_train

#sample size
n <- train_set %>% nrow()

#number of independent variables
k <- features %>% length()

Adjusted_R2_train = 1-(1-R2_train)*((n-1)/(n-k-1))

tibble(RMSE_train = round(RMSE_train, 1),
       R2_train, Adjusted_R2_train)

# Plotting actual & predicted
my_data_train <- cbind(predicted = y_pred_train$predict,
                       observed = train_set$cty) %>% as.data.frame()

g_train <- my_data_train %>% 
  ggplot(aes(predicted, observed)) + 
  geom_point(color = "darkred") + 
  geom_smooth(method=lm) + 
  labs(x="Predecited Power Output", 
       y="Observed Power Output",
       title=glue('Train: Adjusted R2 = {round(enexpr(Adjusted_R2_train),2)}')) +
  theme(plot.title = element_text(color="darkgreen",size=16,hjust=0.5),
        axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14))

g_train %>% ggplotly()

# Compare 
library(patchwork)
g_train + g

tibble(RMSE_train = round(RMSE_train,1),
       RMSE_test = round(RMSE,1),
       
       Adjusted_R2_train,
       Adjusted_R2_test = Adjusted_R2)