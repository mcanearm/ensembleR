library(caret)
library(ensembleR)
library(ggplot2)


data(abalone, package="ensembleR")
abalone$Age <- abalone$Rings + 1.5
abalone$Rings <- NULL


train_idx <- sample(1:nrow(abalone), 0.8*nrow(abalone))
val_set_idx <- sample(train_idx, 0.3*length(train_idx))
train_set_idx <- setdiff(train_idx, val_set_idx)
test_set_idx <- setdiff(1:nrow(abalone), train_idx)

test_df <- abalone[test_set_idx, ]
val_df <- abalone[val_set_idx, ]
train_df <- abalone[train_set_idx, ]

# predict the shucked weight vs. sex, length, diameter, height, and rings
lm_fit <- lm(data=train_df, Age ~ .)

# rf model
rf_fit <- ranger::ranger(data=train_df, Age ~ ., num.trees = 256)

# xgboost model
dv <- caret::dummyVars(~ Sex, data=abalone, fullRank=TRUE)
dummy_vars <- predict(dv, abalone)

keep_names <- setdiff(colnames(train_df), c("Age", "Sex"))
xgb_train_features <- as.matrix(cbind(train_df[, keep_names], dummy_vars[train_set_idx, ]))

xgb_train_features
xgb_fit <- xgboost::xgboost(data = xgb_train_features, label=train_df$Age, objective="reg:squarederror", nrounds=500)

# now fit the weights using the validation set
# predict the shucked weight for the test set
lm_pred <- predict(lm_fit, val_df)
rf_pred <- predict(rf_fit, val_df)$predictions
xgb_pred <- predict(xgb_fit, as.matrix(cbind(val_df[, keep_names], dummy_vars[val_set_idx, ])))

val_y_hats <- cbind('lm'=lm_pred, 'rf'=rf_pred, 'xgb'=xgb_pred)
val_y_true <- val_df$Age

rmse <- function(y, y_hat) {
    sqrt(mean((y - y_hat)^2))
}

val_rmse <- apply(val_y_hats, 2, function(y_hat) rmse(val_y_true, y_hat))
val_rmse

agg_fn <- fitAggregationFunction(val_y_hats, val_y_true, cores=4)

plot(agg_fn)

lm_pred_test <- predict(lm_fit, test_df)
rf_pred_test <- predict(rf_fit, test_df)$predictions
xgb_pred_test <- predict(xgb_fit, as.matrix(cbind(test_df[, keep_names], dummy_vars[test_set_idx, ])))

y_hats <- cbind('lm'=lm_pred_test, 'rf'=rf_pred_test, 'xgb'=xgb_pred_test)
y_true <- test_df$Age


alphas <- c(0.025, 0.05, 0.10)
conf_int_coverage <- lapply(alphas, function(alpha) {
    predictions <- cbind(predict(agg_fn, y_hats, alpha=alpha), y_true)
    replicate(100, {
        sample_preds <- predictions[sample(1:nrow(predictions), nrow(predictions), replace=TRUE), ]
        mean(sample_preds[, 4] > sample_preds[, 2] & sample_preds[, 4] < sample_preds[, 3])
    })
})

par(mfrow=c(1, 3))
for (i in 1:length(conf_int_coverage)) {
    target_alpha <- alphas[i]
    hist(conf_int_coverage[[i]], main=sprintf("CI=%i%%", 100*(1-target_alpha*2)), xlab="Coverage",
         xlim = c(
             min(1-2*target_alpha, min(conf_int_coverage[[i]])),
             max(conf_int_coverage[[i]])),
    cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
    abline(v=1-2*target_alpha, col="red")
}

# my_predictions[, 'index'] = 1:nrow(my_predictions)
my_predictions$in_interval <- my_predictions$y_true > my_predictions$lower & my_predictions$y_true < my_predictions$upper

mean(my_predictions$in_interval)


ggplot(data=my_predictions, aes(x=index, y=y_true, color=in_interval)) + geom_point() + geom_errorbar(aes(ymin=lower, ymax=upper, width=0)) + scale_color_manual(values=c("red", "black"))

apply(y_hats, 2, function(y_hat) rmse(y_true, y_hat))
rmse(test_predictions[, 1], y_true)


