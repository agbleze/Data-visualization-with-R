## LOAD LIBRARIES
library(rsample) ## data splitting
library(dplyr) ## data transformation
library(caret) ## implementing with caret
library(h2o)
library(modeldata)

# convert some numeric variables to factors
data("attrition")
attrition <- attrition %>%
  mutate(
    JobLevel = factor(JobLevel),
    StockOptionLevel = factor(StockOptionLevel),
    TrainingTimesLastYear = factor(TrainingTimesLastYear)
  )

# create training (70%) amd test (30%) sets for the attribution data
# use set.seed for reproducibility
set.seed(123)
split <- initial_split(attrition, prop = .7, strata = "Attrition")
train <- training(split)
test <- testing(split)
#View(attrition)  
# distribution of Attrition rates across train and test set
table(train$Attrition)%>%
  prop.table()


# checking if predictors are conditionally independent of one another given response value
##  moderate to strong correlated variables indicates voilation of conditionally independent variables
train %>%
  filter(Attrition == "Yes") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

## check if continuous variables are normally distributed
train %>%
  select(Age, DailyRate, DistanceFromHome, HourlyRate, MonthlyIncome, MonthlyRate) %>%
  gather(metric, value) %>%
  ggplot(aes(value,fill = metric)) + geom_density(show.legend = F) + facet_wrap(~ metric, scales = "free")

  # using caret package for naive bayes
# create response and feature data
features <- setdiff(names(train), "Attrition")
x <- train[, features]
y <- train$Attrition

# set up 10-fold cross validation procedure
train_control <- trainControl(
  method = "cv",
  number = 10
)

# train model
nb.m1 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control
)

# results
confusionMatrix(nb.m1)

##### tuning parameters to improve model
# set up tuning grid
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)
)

# train model
nb.m2 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control,
  tuneGrid = search_grid,
  preProc = c("BoxCox", "center", "scale", "pca")
)

# top 5 model
nb.m2$results %>%
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

# plot search grid results
plot(nb.m2)

# results for best model  -- cross-validated (10 fold) confusin matrix
confusionMatrix(nb.m2)

######## testing model on test data
pred <- predict(nb.m2, newdata = test)
confusionMatrix(pred, test$Attrition)


################## Using h20 for Naive Bayes
h2o.no_progress()
h2o.init()

# create feature names
y <- "Attrition"
x <- setdiff(names(train), y)

# h2o cannot ingest ordered factors
train.h2o <- train %>%
  mutate_if(is.factor, factor, ordered = FALSE) %>%
  as.h2o()

# train model
nb.h2o <- h2o.naiveBayes(
  x = x,
  y = y,
  training_frame = train.h2o,
  nfolds = 10,
  laplace = 0
)

#assess results
h2o.confusionMatrix(nb.h2o)

#######  parameter tuning in h2o
## preprocess
preprocess <- preProcess(train, method = c("BoxCox", "center", "scale", "pca"))
train_pp <- predict(preprocess, train)
test_pp <- predict(preprocess, test)

# convert to h2o objects
train_pp.h2o <- train_pp %>%
  mutate_if(is.factor, factor, ordered = FALSE) %>%
  as.h2o()

test_pp.h2o <- test_pp %>%
  mutate_if(is.factor, factor, ordered = FALSE) %>%
  as.h2o()

# get new feature names -- PCA preprocessing reduced and changed some features
x <- setdiff(names(train_pp), "Attrition")

# create tuning grid
hyper_params <- list(
  laplace = seq(0, 5, by = 0.5)
)

## build grid search
grid <- h2o.grid(
  algorithm = "naivebayes",
  grid_id = "nb_grid",
  x = x,
  y = y,
  training_frame = train_pp.h2o,
  nfolds = 10,
  hyper_params = hyper_params
)

# sort grid models by mse
sorted_grid <- h2o.getGrid("nb_grid", sort_by = "accuracy", decreasing = TRUE)
sorted_grid

# grab top model id
best_h2o_model <- sorted_grid@model_ids[[1]]
best_model <- h2o.getModel(best_h2o_model)

# confusion matrix of best h2o
h2o.confusionMatrix(best_model)

# ROC curve
auc <- h2o.auc(best_model, xval = TRUE)
fpr <- h2o.performance(best_model, xval = TRUE) %>%
  h2o.fpr() %>%
  .[['fpr']]
tpr <- h2o.performance(best_model, xval = TRUE) %>%
  h2o.tpr() %>%
  .[['tpr']]

data.frame(fpr = fpr, tpr = tpr) %>%
  ggplot(aes(fpr, tpr)) +
  geom_line() + ggtitle(sprintf('AUC: %f', auc))


### evaluate on test set
h2o.performance(best_model, newdata = test_pp.h2o)

# predict new data
h2o.predict(nb.h2o, newdata = test_pp.h2o)

