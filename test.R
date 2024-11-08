library(caret)
library(ensembleR)
library(ggplot2)


names <- c(
    "Sex",
    "Length",
    "Diameter",
    "Height",
    "whole_weight_g",
    "shucked_weight_g",
    "viscera_weight_g",
    "shell_weight_g",
    "Rings"
)
abalone <- read.table("abalone/abalone.data", sep=",", header=FALSE, col.names = names)

train_idx <- sample(1:nrow(abalone), 0.8*nrow(abalone))
val_set_idx <- sample(train_idx, 0.3*length(train_idx))
train_set_idx <- setdiff(train_idx, val_set_idx)
test_set_idx <- setdiff(1:nrow(abalone), train_idx)

test_df <- abalone[test_set_idx, ]
val_df <- abalone[val_set_idx, ]
train_df <- abalone[train_set_idx, ]

# predict the shucked weight vs. sex, length, diameter, height, and rings
lm_fit <- lm(data=train_df, shucked_weight_g ~ Rings + Height + Diameter + Length + Sex)

# rf model
rf_fit <- ranger::ranger(data=train_df, shucked_weight_g ~ Rings + Height + Diameter + Length + Sex, num.trees = 256)

# xgboost model
dv <- caret::dummyVars(~ Sex, data=abalone, fullRank=TRUE)
dummy_vars <- predict(dv, abalone)

xgb_train_features <- as.matrix(cbind(train_df[, c("Rings", "Height", "Diameter", "Length")], dummy_vars[train_set_idx, ]))
xgb_fit <- xgboost::xgboost(data = xgb_train_features, label=train_df$shucked_weight_g, objective="reg:squarederror", nrounds=500)

# now fit the weights using the validation set
# predict the shucked weight for the test set
lm_pred <- predict(lm_fit, val_df)
rf_pred <- predict(rf_fit, val_df)$predictions
xgb_pred <- predict(xgb_fit, as.matrix(cbind(val_df[, c("Rings", "Height", "Diameter", "Length")], dummy_vars[val_set_idx, ])))

val_y_hats <- cbind('lm'=lm_pred, 'rf'=rf_pred, 'xgb'=xgb_pred)
val_y_true <- val_df$shucked_weight_g

rmse <- function(y, y_hat) {
    sqrt(mean((y - y_hat)^2))
}

val_rmse <- apply(val_y_hats, 2, function(y_hat) rmse(val_y_true, y_hat))
val_rmse


agg_fn <- fitAggregationFunction(val_y_hats, val_y_true, cores=4)

plot(agg_fn)

lm_pred_test <- predict(lm_fit, test_df)
rf_pred_test <- predict(rf_fit, test_df)$predictions
xgb_pred_test <- predict(xgb_fit, as.matrix(cbind(test_df[, c("Rings", "Height", "Diameter", "Length")], dummy_vars[test_set_idx, ])))

y_hats <- cbind('lm'=lm_pred_test, 'rf'=rf_pred_test, 'xgb'=xgb_pred_test)
y_true <- test_df$shucked_weight_g

test_predictions <- predict(agg_fn, y_hats, alpha=0.025)
test_predictions

weight_preds <- cbind.data.frame(test_predictions, y_true)
my_predictions <- weight_preds[sample(1:nrow(weight_preds), 100),]
my_predictions[, 'index'] = 1:nrow(my_predictions)
my_predictions$in_interval <- my_predictions$y_true > my_predictions$lower & my_predictions$y_true < my_predictions$upper


ggplot(data=my_predictions, aes(x=index, y=y_true, color=in_interval)) + geom_point() + geom_errorbar(aes(ymin=lower, ymax=upper, width=0)) + scale_color_manual(values=c("red", "black"))

mean(my_predictions$in_interval)

apply(y_hats, 2, function(y_hat) rmse(y_true, y_hat))
rmse(test_predictions[, 1], y_true)


